// Streaming R/O Virtual File System v0.2.0
// Copyright (C) XL A.S. Ketmar.  All rights reserved
// See the file aplicense.txt for conditions of use.
//
// grouping files with packing:
//   zip, pk3: PKZIP-compatible archives (store, deflate)
//   dfwad   : D2D:F wad archives
//
{.$DEFINE SFS_DEBUG_ZIPFS}
{$MODE OBJFPC}
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
    procedure removeCommonPath (); override;

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
  fcnt: Word;
begin
  result := false;
  if st.Size < 10 then exit;
  st.ReadBuffer(sign[0], 6);
  st.ReadBuffer(fcnt, 2);
  st.Seek(-8, soCurrent);
  //writeln('trying DFWAD... [', sign, ']');
  if (sign[0] <> 'D') and (sign[1] <> 'F') and (sign[2] <> 'W') and
     (sign[3] <> 'A') and (sign[4] <> 'D') and (sign[5] <> #$01) then exit;
  //writeln('DFWAD FOUND, with ', fcnt, ' files');
  //if (fcnt < 0) then exit;
  result := true;
end;


function maxPrefix (s0: string; s1: string): Integer;
var
  f: Integer;
begin
  for f := 1 to length(s0) do
  begin
    if f > length(s1) then begin result := f; exit; end;
    if UpCase1251(s0[f]) <> UpCase1251(s1[f]) then begin result := f; exit; end;
  end;
  result := length(s0);
end;


procedure TSFSZipVolume.removeCommonPath ();
var
  f, pl, maxsc, sc, c: integer;
  cp, s: string;
  fi: TSFSZipFileInfo;
begin
  if fType <> sfszvZIP then exit;
  maxsc := 0;
  if fFiles.Count = 0 then exit;
  cp := '';
  for f := 0 to fFiles.Count-1 do
  begin
    fi := TSFSZipFileInfo(fFiles[f]);
    s := fi.fPath;
    if length(s) > 0 then begin cp := s; break; end;
  end;
  if length(cp) = 0 then exit;
  for f := 0 to fFiles.Count-1 do
  begin
    fi := TSFSZipFileInfo(fFiles[f]);
    s := fi.fPath;
    if length(s) = 0 then continue;
    pl := maxPrefix(cp, s);
    //writeln('s=[', s, ']; cp=[', cp, ']; pl=', pl);
    if pl = 0 then exit; // no common prefix at all
    cp := Copy(cp, 1, pl);
    sc := 0;
    for c := 1 to length(s) do if s[c] = '/' then Inc(sc);
    if sc > maxsc then maxsc := sc;
  end;
  if maxsc < 2 then exit; // alas
  while (length(cp) > 0) and (cp[length(cp)] <> '/') do cp := Copy(cp, 1, length(cp)-1);
  if length(cp) < 2 then exit; // nothing to do
  for f := 0 to fFiles.Count-1 do
  begin
    fi := TSFSZipFileInfo(fFiles[f]);
    if length(fi.fPath) >= length(cp) then
    begin
      s := fi.fPath;
      fi.fPath := Copy(fi.fPath, length(cp)+1, length(fi.fPath));
      //writeln('FIXED [', s, '] -> [', fi.fPath, ']');
    end;
  end;
end;


{ TSFSZipVolume }
procedure TSFSZipVolume.ZIPReadDirectory ();
var
  fi: TSFSZipFileInfo;
  name: ShortString;
  sign, dSign: packed array [0..3] of Char;
  lhdr: TZLocalFileHeader;
  ignoreFile, skipped: Boolean;
  crc, psz, usz: LongWord;
  buf: packed array of Byte;
  bufPos, bufUsed: Integer;
  efid, efsz: Word;
  izver: Byte;
  izcrc: LongWord;
begin
  SetLength(buf, 0);
  // read local directory
  repeat
    fFileStream.ReadBuffer(sign[0], Length(sign));

    if sign <> 'PK'#3#4 then break;

    ignoreFile := false;
    skipped := false;

    fi := TSFSZipFileInfo.Create(self);
    fi.fPackSz := 0;
    fi.fMethod := 0;

    //fi.fOfs := fFileStream.Position;

    fFileStream.ReadBuffer(lhdr, SizeOf(lhdr));
    if lhdr.fnameSz > 255 then name[0] := #255 else name[0] := chr(lhdr.fnameSz);
    fFileStream.ReadBuffer(name[1], Length(name));
    fFileStream.Seek(lhdr.fnameSz-Length(name), soCurrent); // rest of the name (if any)
    fi.fName := utf8to1251(name);
    //writeln(Format('0x%08x : %s', [Integer(fi.fOfs), name]));

    // here we should process extra field: it may contain utf8 filename
    //fFileStream.Seek(lhdr.localExtraSz, soCurrent);
    while lhdr.localExtraSz >= 4 do
    begin
      efid := 0;
      efsz := 0;
      fFileStream.ReadBuffer(efid, 2);
      fFileStream.ReadBuffer(efsz, 2);
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

    if (lhdr.flags and (1 shl 3)) <> 0 then
    begin
      // it has a descriptor. stupid thing at all...
      {$IFDEF SFS_DEBUG_ZIPFS}
      WriteLn(ErrOutput, 'descr: $', IntToHex(fFileStream.Position, 8));
      WriteLn(ErrOutput, 'size: ', lhdr.unpackSz);
      WriteLn(ErrOutput, 'psize: ', lhdr.packSz);
      {$ENDIF}
      skipped := true;

      if lhdr.packSz <> 0 then
      begin
        // some kind of idiot already did our work (maybe paritally)
        // trust him (her? %-)
        fFileStream.Seek(lhdr.packSz, soCurrent);
      end;

      // scan for descriptor
      if Length(buf) = 0 then SetLength(buf, 65536);
      bufPos := 0; bufUsed := 0;
      fFileStream.ReadBuffer(dSign[0], 4);
      repeat
        if dSign <> 'PK'#7#8 then
        begin
          // skip one byte
          Move(dSign[1], dSign[0], 3);
          if bufPos >= bufUsed then
          begin
            bufPos := 0;
            // int64!
            if fFileStream.Size-fFileStream.Position > Length(buf) then bufUsed := Length(buf)
            else bufUsed := fFileStream.Size-fFileStream.Position;
            if bufUsed = 0 then raise ESFSError.Create('invalid ZIP file');
            fFileStream.ReadBuffer(buf[0], bufUsed);
          end;
          dSign[3] := chr(buf[bufPos]); Inc(bufPos);
          Inc(lhdr.packSz);
          continue;
        end;
        // signature found: check if it is a real one
        // ???: make stronger check (for the correct following signature)?
        // sign, crc, packsize, unpacksize
        fFileStream.Seek(-bufUsed+bufPos, soCurrent); bufPos := 0; bufUsed := 0;
        fFileStream.ReadBuffer(crc, 4); // crc
        fFileStream.ReadBuffer(psz, 4); // packed size
        // is size correct?
        if psz = lhdr.packSz then
        begin
          // this is a real description. fuck it off
          fFileStream.ReadBuffer(usz, 4); // unpacked size
          break;
        end;
        // this is just a sequence of bytes
        fFileStream.Seek(-8, soCurrent);
        fFileStream.ReadBuffer(dSign[0], 4);
        Inc(lhdr.packSz, 4);
      until false;
      // store correct values
      fi.fSize := usz;
      fi.fPackSz := psz;
    end;

    // skip packed data
    if not skipped then fFileStream.Seek(lhdr.packSz, soCurrent);
    if ignoreFile then fi.Free();
  until false;

  if (sign <> 'PK'#1#2) and (sign <> 'PK'#5#6) then
  begin
    {$IFDEF SFS_DEBUG_ZIPFS}
    WriteLn(ErrOutput, 'end: $', IntToHex(fFileStream.Position, 8));
    WriteLn(ErrOutput, 'sign: $', sign[0], sign[1], '#', ord(sign[2]), '#', ord(sign[3]));
    {$ENDIF}
    raise ESFSError.Create('invalid .ZIP archive (no central dir)');
  end;
end;


procedure TSFSZipVolume.DFWADReadDirectory ();
// idiotic format
var
  fcnt: Word;
  fi: TSFSZipFileInfo;
  f, c, fofs, fpksize: Integer;
  curpath, fname: string;
  name: packed array [0..15] of Char;
begin
  curpath := '';
  fFileStream.Seek(6, soCurrent); // skip signature
  fFileStream.ReadBuffer(fcnt, 2);
  if fcnt = 0 then exit;
  // read files
  for f := 0 to fcnt-1 do
  begin
    fFileStream.ReadBuffer(name[0], 16);
    fFileStream.ReadBuffer(fofs, 4);
    fFileStream.ReadBuffer(fpksize, 4);
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
