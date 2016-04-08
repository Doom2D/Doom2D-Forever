// Streaming R/O Virtual File System v0.2.0
// Copyright (C) XL A.S. Ketmar.  All rights reserved
// See the file aplicense.txt for conditions of use.
//
// "memory group". reads the whole pack in memory (and decompress it if
// necessary). memory image has only one file named "<body>".
//
// now understands:
//   slh!: DOS Allegro "slh!"
//   mem : raw file (no processing, just read)
//     as a side effect this gives us an opportunity to read enclosed packs
//     from the packs which aren't supporting backseeking (such as zips).
//
{.$DEFINE SFS_MSMFS}
{$MODE DELPHI}
{.$R-}
unit sfsMemFS;

interface

{$IFDEF SFS_MSMFS}
uses
  SysUtils, Classes, Contnrs, sfs;


type
  TSFSMemVolumeType = (sfsmvNone, sfsmvRAW, sfsmvSLH);

  TSFSMemoryVolume = class(TSFSVolume)
  protected
    fType: TSFSMemVolumeType;
    fMemStream: TMemoryStream;

    procedure RAWRead ();
    procedure SLHRead ();

    procedure ReadDirectory (); override;

  public
    function OpenFileByIndex (const index: Integer): TStream; override;
  end;

  TSFSMemoryVolumeFactory = class (TSFSVolumeFactory)
  public
    function IsMyVolumePrefix (const prefix: TSFSString): Boolean; override;
    function Produce (const prefix, fileName: TSFSString; st: TStream): TSFSVolume; override;
    procedure Recycle (vol: TSFSVolume); override;
  end;
{$ENDIF}


implementation

{$IFDEF SFS_MSMFS}
uses
  xstreams;


function SLHCheckMagic (st: TStream): Boolean;
var
  sign: packed array [0..3] of Char;
begin
  result := false;
  st.ReadBuffer(sign[0], 4);
  st.Seek(-4, soCurrent);
  if sign <> 'slh!' then exit;
  result := true;
end;


{ TSFSMemoryVolume }
procedure TSFSMemoryVolume.RAWRead ();
var
  fi: TSFSFileInfo;
begin
  fMemStream.CopyFrom(fFileStream, 0); // voila %-)
  fi := TSFSFileInfo.Create(self);
  fi.fName := '<body>';
  fi.fSize := fMemStream.Size;
end;

procedure TSFSMemoryVolume.SLHRead ();
// unpack LZSS-packed file
var
  fi: TSFSFileInfo;
  bufi, bufo: packed array of Byte;
  iused, oused, rpos: Integer;
  dict: packed array of Byte;
  flags, dpos, pos, len: Word;
  c: Integer;

  function ReadCh (): Integer;
  begin
    if rpos >= iused then
    begin
      // int64!
      if fFileStream.Size-fFileStream.Position > Length(bufi) then iused := Length(bufi)
      else iused := fFileStream.Size-fFileStream.Position;
      rpos := 0;
      if iused > 0 then fFileStream.ReadBuffer(bufi[0], iused);
    end;

    if iused = 0 then result := -1
    else begin result := bufi[rpos]; Inc(rpos); end;
  end;

  procedure WriteCh (c: Byte);
  begin
    if oused >= Length(bufo) then
    begin
      fMemStream.WriteBuffer(bufo[0], oused);
      oused := 0;
    end;
    bufo[oused] := c; Inc(oused);
    dict[dpos] := c; dpos := (dpos+1) and $FFF;
  end;

begin
  fFileStream.Seek(4, soCurrent); // skip signature
  SetLength(bufi, 65536); SetLength(bufo, 65536); SetLength(dict, 4096);
  rpos := 0; iused := 0; oused := 0;
  flags := 0; dpos := 4096-18;
  repeat
    if (flags and $FF00) = 0 then
    begin
      c := ReadCh(); if c = -1 then break;
      flags := c or $FF00;
    end;

    if (flags and $01) <> 0 then
    begin
      // literal
      c := ReadCh(); if c = -1 then break;
      WriteCh(c);
    end
    else
    begin
      // "copy"
      c := ReadCh(); if c = -1 then break;
      pos := c;
      c := ReadCh(); if c = -1 then break;
      len := c;
      pos := (pos and $FF) or ((len and $F0) shl 4); len := (len and $0F)+3;
      while len > 0 do
      begin
        c := dict[pos]; pos := (pos+1) and $FFF; Dec(len);
        WriteCh(c);
      end;
    end;
    flags := flags shr 1;
  until false;
  if oused > 0 then fMemStream.WriteBuffer(bufo[0], oused);

  fi := TSFSFileInfo.Create(self);
  fi.fName := '<body>';
  fi.fSize := fMemStream.Size;
end;

procedure TSFSMemoryVolume.ReadDirectory ();
begin
  if fMemStream = nil then fMemStream := TMemoryStream.Create()
  else
  begin
    fMemStream.Position := 0; fMemStream.Size := 0;
  end;

  case fType of
    sfsmvSLH: SLHRead();
    sfsmvRAW: RAWRead();
    else raise ESFSError.Create('invalid memory SFS');
  end;

  fMemStream.Position := 0;
end;

function TSFSMemoryVolume.OpenFileByIndex (const index: Integer): TStream;
var
  fs: TStream;
begin
  result := nil; fs := nil;
  if fFiles = nil then exit;
  if (index < 0) or (index >= fFiles.Count) or (fFiles[index] = nil) then exit;

  try
    fs := TSFSMemoryStreamRO.Create(fMemStream.Memory, fMemStream.Size);
    if fFiles.Count = 1 then
    begin
      result := fs;
    end
    else
    begin
      try
        result := TSFSPartialStream.Create(fs,
          TSFSFileInfo(fFiles[index]).fOfs,
          TSFSFileInfo(fFiles[index]).fSize, true);
      except
        FreeAndNil(fs);
        raise;
      end;
    end;
  except
    result := nil;
  end;
end;


{ TSFSMemoryVolumeFactory }
function TSFSMemoryVolumeFactory.IsMyVolumePrefix (const prefix: TSFSString): Boolean;
begin
  result :=
    SFSStrEqu(prefix, 'mem') or
    SFSStrEqu(prefix, 'slh!');
end;

procedure TSFSMemoryVolumeFactory.Recycle (vol: TSFSVolume);
begin
  vol.Free();
end;

function TSFSMemoryVolumeFactory.Produce (const prefix, fileName: TSFSString; st: TStream): TSFSVolume;
var
  vt: TSFSMemVolumeType;
begin
  if (prefix <> 'mem') and SLHCheckMagic(st) then vt := sfsmvSLH
  else if prefix <> '' then vt := sfsmvRAW
  else vt := sfsmvNone;

  result := TSFSMemoryVolume.Create(fileName, st);
  TSFSMemoryVolume(result).fType := vt;
  try
    result.DoDirectoryRead();
  except
    FreeAndNil(result);
    raise;
  end;
end;


var
  memf: TSFSMemoryVolumeFactory;
initialization
  memf := TSFSMemoryVolumeFactory.Create();
  SFSRegisterVolumeFactory(memf);
finalization
  SFSUnregisterVolumeFactory(memf);
{$ENDIF}
end.
