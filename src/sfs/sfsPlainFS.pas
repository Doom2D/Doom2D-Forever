// Streaming R/O Virtual File System v0.2.0
// Copyright (C) XL A.S. Ketmar.  All rights reserved
// See the file aplicense.txt for conditions of use.
//
// simple grouping files w/o packing:
//   Quake I/II .PAK (PACK)
//   SiN .SIN (SPAK)
//
{$MODE OBJFPC}
{$R+}
unit sfsPlainFS;

interface

uses
  SysUtils, Classes, Contnrs, sfs;


type
  TSFSPlainVolumeType = (sfspvNone, sfspvPAK, sfspvSIN);

  TSFSPlainVolume = class (TSFSVolume)
  protected
    fType: TSFSPlainVolumeType;

    procedure ReadDirectory (); override;

  public
    function OpenFileByIndex (const index: Integer): TStream; override;
  end;

  TSFSPlainVolumeFactory = class (TSFSVolumeFactory)
  public
    function IsMyVolumePrefix (const prefix: AnsiString): Boolean; override;
    function Produce (const prefix, fileName: AnsiString; st: TStream): TSFSVolume; override;
    procedure Recycle (vol: TSFSVolume); override;
  end;


implementation

uses
  xstreams, utils;


{ TSFSPlainVolume }
procedure TSFSPlainVolume.ReadDirectory ();
var
  dsize, dofs, esz: LongWord;
  fi: TSFSFileInfo;
  name: packed array [0..120] of Char;
begin
  if (fType <> sfspvPAK) and (fType <> sfspvSIN) then raise ESFSError.Create('invalid archive');
  fFileStream.Seek(4, soCurrent); // skip signature
  fFileStream.ReadBuffer(dofs, 4);
  fFileStream.ReadBuffer(dsize, 4);
  fFileStream.Position := dofs;
  if fType = sfspvPAK then esz := 64 else esz := 128;
  while dsize >= esz do
  begin
    fi := TSFSFileInfo.Create(self);
    FillChar(name[0], length(name), 0);
    fFileStream.ReadBuffer(name[0], esz-8);
    fi.fName := PChar(@name[0]);
    fFileStream.ReadBuffer(fi.fOfs, 4);
    fFileStream.ReadBuffer(fi.fSize, 4);
    Dec(dsize, esz);
  end;
end;

function TSFSPlainVolume.OpenFileByIndex (const index: Integer): TStream;
begin
  result := nil;
  if fFiles = nil then exit;
  if (index < 0) or (index >= fFiles.Count) or (fFiles[index] = nil) then exit;
  result := TSFSPartialStream.Create(fFileStream, TSFSFileInfo(fFiles[index]).fOfs, TSFSFileInfo(fFiles[index]).fSize, false);
end;


{ TSFSPlainVolumeFactory }
function TSFSPlainVolumeFactory.IsMyVolumePrefix (const prefix: AnsiString): Boolean;
begin
  result :=
    StrEquCI1251(prefix, 'pak') or
    StrEquCI1251(prefix, 'sin');
end;

procedure TSFSPlainVolumeFactory.Recycle (vol: TSFSVolume);
begin
  vol.Free();
end;

function TSFSPlainVolumeFactory.Produce (const prefix, fileName: AnsiString; st: TStream): TSFSVolume;
var
  vt: TSFSPlainVolumeType;
  sign: packed array [0..3] of Char;
  dsize, dofs: Integer;
begin
  result := nil;
  vt := sfspvNone;

  st.ReadBuffer(sign[0], 4);
  st.ReadBuffer(dofs, 4);
  st.ReadBuffer(dsize, 4);
  st.Seek(-12, soCurrent);
  if sign = 'PACK' then
  begin
    if (dsize < 0) or (dofs < 0) or (dofs > st.Size) or (dofs+dsize > st.Size) or (dsize mod 64 <> 0) then exit;
    vt := sfspvPAK;
  end
  else if sign = 'SPAK' then
  begin
    if (dsize < 0) or (dofs < 0) or (dofs > st.Size) or (dofs+dsize > st.Size) or (dsize mod 64 <> 0) then exit;
    vt := sfspvSIN;
  end;

  result := TSFSPlainVolume.Create(fileName, st);
  TSFSPlainVolume(result).fType := vt;
  try
    result.DoDirectoryRead();
  except
    FreeAndNil(result);
    raise;
  end;
end;


var
  pakf: TSFSPlainVolumeFactory;
initialization
  pakf := TSFSPlainVolumeFactory.Create();
  SFSRegisterVolumeFactory(pakf);
//finalization
//  SFSUnregisterVolumeFactory(pakf);
end.
