// Streaming R/O Virtual File System v0.2.0
// Copyright (C) XL A.S. Ketmar.  All rights reserved
// See the file aplicense.txt for conditions of use.
//
// grouping files with packing:
//   zip, pk3: PKZIP-compatible archives (store, deflate)
//   dfwad   : D2D:F wad archives
//
{.$DEFINE SFS_DEBUG_ZIPFS}
{$MODE DELPHI}
{$R+}
unit sfsZipFS;

interface

uses
  SysUtils, Classes, Contnrs, sfs;


type
  TSFSZipVolumeType = (sfszvNone, sfszvZIP, sfszvDFWAD);

  TSFSZipVolume = class(TSFSVolume)
  protected
    fType: TSFSZipVolumeType;

    procedure ZIPReadDirectory ();
    procedure DFWADReadDirectory ();

    procedure ReadDirectory (); override;

  public
    function OpenFileByIndex (const index: Integer): TStream; override;
  end;

  TSFSZipVolumeFactory = class(TSFSVolumeFactory)
  public
    function IsMyVolumePrefix (const prefix: AnsiString): Boolean; override;
    function Produce (const prefix, fileName: AnsiString; st: TStream): TSFSVolume; override;
    procedure Recycle (vol: TSFSVolume); override;
  end;


implementation

uses
  xstreams, utils;


type
  TSFSZipFileInfo = class(TSFSFileInfo)
  public
    fMethod: Byte; // 0: store; 8: deflate; 255: other
    fPackSz: Int64; // can be -1
  end;

  TZLocalFileHeader = packed record
    version: Byte;
    hostOS: Byte;
    flags: Word;
    method: Word;
    time: LongWord;
    crc: LongWord;
    packSz: LongWord;
    unpackSz: LongWord;
    fnameSz: Word;
    localExtraSz: Word;
  end;

procedure readLFH (st: TStream; var hdr: TZLocalFileHeader);
{.$IFDEF ENDIAN_LITTLE}
begin
  hdr.version := readByte(st);
  hdr.hostOS := readByte(st);
  hdr.flags := readWord(st);
  hdr.method := readWord(st);
  hdr.time := readLongWord(st);
  hdr.crc := readLongWord(st);
  hdr.packSz := readLongWord(st);
  hdr.unpackSz := readLongWord(st);
  hdr.fnameSz := readWord(st);
  hdr.localExtraSz := readWord(st);
end;


function ZIPCheckMagic (st: TStream): Boolean;
var
  sign: packed array [0..3] of Char;
begin
  result := false;
  st.ReadBuffer(sign[0], 4);
  st.Seek(-4, soCurrent);
  if (sign <> 'PK'#3#4) and (sign <> 'PK'#5#6) then exit;
  result := true;
end;


function DFWADCheckMagic (st: TStream): Boolean;
var
  sign: packed array [0..5] of Char;
begin
  result := false;
  if st.Size < 10 then exit;
  st.ReadBuffer(sign[0], 6);
  {fcnt :=} readWord(st);
  st.Seek(-8, soCurrent);
  //writeln('trying DFWAD... [', sign, ']');
  if (sign[0] <> 'D') and (sign[1] <> 'F') and (sign[2] <> 'W') and
     (sign[3] <> 'A') and (sign[4] <> 'D') and (sign[5] <> #$01) then exit;
  //writeln('DFWAD FOUND, with ', fcnt, ' files');
  //if (fcnt < 0) then exit;
  result := true;
end;


{ TSFSZipVolume }
procedure TSFSZipVolume.ZIPReadDirectory ();
var
  fi: TSFSZipFileInfo;
  name: ShortString;
  sign: packed array [0..3] of Char;
  lhdr: TZLocalFileHeader;
  ignoreFile: Boolean;
  efid, efsz: Word;
  izver: Byte;
  izcrc: LongWord;
begin
  // read local directory
  repeat
    fFileStream.ReadBuffer(sign[0], Length(sign));

    // skip data descriptor
    if sign = 'PK'#7#8 then
    begin
      fFileStream.seek(3*4, soCurrent);
      continue;
    end;

    if sign <> 'PK'#3#4 then break;

    ignoreFile := false;

    readLFH(fFileStream, lhdr);

    fi := TSFSZipFileInfo.Create(self);
    fi.fPackSz := 0;
    fi.fMethod := 0;

    if lhdr.fnameSz > 255 then name[0] := #255 else name[0] := chr(lhdr.fnameSz);
    fFileStream.ReadBuffer(name[1], Length(name));
    fFileStream.Seek(lhdr.fnameSz-Length(name), soCurrent); // rest of the name (if any)
    fi.fName := utf8to1251(name);

    // here we should process extra field: it may contain utf8 filename
    while lhdr.localExtraSz >= 4 do
    begin
      efid := readWord(fFileStream);
      efsz := readWord(fFileStream);
      Dec(lhdr.localExtraSz, 4);
      if efsz > lhdr.localExtraSz then break;
      // Info-ZIP Unicode Path Extra Field?
      if (efid = $7075) and (efsz <= 255+5) and (efsz > 5) then
      begin
        fFileStream.ReadBuffer(izver, 1);
        if izver <> 1 then
        begin
          // skip it
          Dec(efsz, 1);
        end
        else
        begin
          Dec(lhdr.localExtraSz, efsz);
          fFileStream.ReadBuffer(izcrc, 4); // name crc, ignore it
          Dec(efsz, 5);
          name[0] := chr(efsz);
          fFileStream.ReadBuffer(name[1], Length(name));
          fi.fName := utf8to1251(name);
          break;
        end;
      end;
      // skip it
      if efsz > 0 then
      begin
        fFileStream.Seek(efsz, soCurrent);
        Dec(lhdr.localExtraSz, efsz);
      end;
    end;
    // skip the rest
    if lhdr.localExtraSz > 0 then fFileStream.Seek(lhdr.localExtraSz, soCurrent);

    if (lhdr.flags and 1) <> 0 then
    begin
      // encrypted file: skip it
      ignoreFile := true;
    end;

    if (lhdr.method <> 0) and (lhdr.method <> 8) then
    begin
      // not stored. not deflated. skip.
      ignoreFile := true;
    end;

    fi.fOfs := fFileStream.Position;
    fi.fSize := lhdr.unpackSz;
    fi.fPackSz := lhdr.packSz;
    fi.fMethod := lhdr.method;

    // skip packed data
    fFileStream.Seek(lhdr.packSz, soCurrent);
    if ignoreFile then fi.Free();
  until false;
  (*
  if (sign <> 'PK'#1#2) and (sign <> 'PK'#5#6) then
  begin
    {$IFDEF SFS_DEBUG_ZIPFS}
    WriteLn(ErrOutput, 'end: $', IntToHex(fFileStream.Position, 8));
    WriteLn(ErrOutput, 'sign: $', sign[0], sign[1], '#', ord(sign[2]), '#', ord(sign[3]));
    {$ENDIF}
    raise ESFSError.Create('invalid .ZIP archive (no central dir)');
  end;
  *)
end;


procedure TSFSZipVolume.DFWADReadDirectory ();
// idiotic format
var
  fcnt: Word;
  fi: TSFSZipFileInfo;
  f, c: Integer;
  fofs, fpksize: LongWord;
  curpath, fname: string;
  name: packed array [0..15] of Char;
begin
  curpath := '';
  fFileStream.Seek(6, soCurrent); // skip signature
  fcnt := readWord(fFileStream);
  if fcnt = 0 then exit;
  // read files
  for f := 0 to fcnt-1 do
  begin
    fFileStream.ReadBuffer(name[0], 16);
    fofs := readLongWord(fFileStream);
    fpksize := readLongWord(fFileStream);
    c := 0;
    fname := '';
    while (c < 16) and (name[c] <> #0) do
    begin
      if name[c] = '\' then name[c] := '/'
      else if name[c] = '/' then name[c] := '_';
      fname := fname+name[c];
      Inc(c);
    end;
    // new directory?
    if (fofs = 0) and (fpksize = 0) then
    begin
      if length(fname) <> 0 then fname := fname+'/';
      curpath := fname;
      continue;
    end;
    if length(fname) = 0 then continue; // just in case
    //writeln('DFWAD: [', curpath, '] [', fname, '] at ', fofs, ', size ', fpksize);
    // create file record
    fi := TSFSZipFileInfo.Create(self);
    fi.fOfs := fofs;
    fi.fSize := -1;
    fi.fPackSz := fpksize;
    fi.fName := fname;
    fi.fPath := curpath;
    fi.fMethod := 255;
  end;
end;

procedure TSFSZipVolume.ReadDirectory ();
begin
  case fType of
    sfszvZIP: ZIPReadDirectory();
    sfszvDFWAD: DFWADReadDirectory();
    else raise ESFSError.Create('invalid archive');
  end;
end;

function TSFSZipVolume.OpenFileByIndex (const index: Integer): TStream;
var
  rs: TStream;
begin
  result := nil;
  rs := nil;
  if fFiles = nil then exit;
  if (index < 0) or (index >= fFiles.Count) or (fFiles[index] = nil) then exit;
  try
    if TSFSZipFileInfo(fFiles[index]).fMethod = 0 then
    begin
      result := TSFSPartialStream.Create(fFileStream, TSFSZipFileInfo(fFiles[index]).fOfs, TSFSZipFileInfo(fFiles[index]).fSize, false);
    end
    else
    begin
      rs := TSFSPartialStream.Create(fFileStream, TSFSZipFileInfo(fFiles[index]).fOfs, TSFSZipFileInfo(fFiles[index]).fPackSz, false);
      result := TUnZStream.Create(rs, TSFSZipFileInfo(fFiles[index]).fSize, true, (TSFSZipFileInfo(fFiles[index]).fMethod <> 255));
    end;
  except
    FreeAndNil(rs);
    result := nil;
    exit;
  end;
end;


{ TSFSZipVolumeFactory }
function TSFSZipVolumeFactory.IsMyVolumePrefix (const prefix: AnsiString): Boolean;
begin
  result :=
    StrEquCI1251(prefix, 'zip') or
    StrEquCI1251(prefix, 'pk3') or
    StrEquCI1251(prefix, 'dfwad');
end;

procedure TSFSZipVolumeFactory.Recycle (vol: TSFSVolume);
begin
  vol.Free();
end;

function TSFSZipVolumeFactory.Produce (const prefix, fileName: AnsiString; st: TStream): TSFSVolume;
var
  vt: TSFSZipVolumeType;
begin
  vt := sfszvNone;
       if ZIPCheckMagic(st) then vt := sfszvZIP
  else if DFWADCheckMagic(st) then vt := sfszvDFWAD;

  if vt <> sfszvNone then
  begin
    result := TSFSZipVolume.Create(fileName, st);
    TSFSZipVolume(result).fType := vt;
    try
      result.DoDirectoryRead();
    except {$IFDEF SFS_DEBUG_ZIPFS} on e: Exception do begin
      WriteLn(errOutput, 'ZIP ERROR: [', e.ClassName, ']: ', e.Message);
      {$ENDIF}
      FreeAndNil(result);
      raise;
      {$IFDEF SFS_DEBUG_ZIPFS}end;{$ENDIF}
    end;
  end
  else
  begin
    result := nil;
  end;
end;


var
  zipf: TSFSZipVolumeFactory;
initialization
  zipf := TSFSZipVolumeFactory.Create();
  SFSRegisterVolumeFactory(zipf);
//finalization
//  SFSUnregisterVolumeFactory(zipf);
end.
