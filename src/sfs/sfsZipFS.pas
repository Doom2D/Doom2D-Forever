// Streaming R/O Virtual File System v0.2.0
// Copyright (C) XL A.S. Ketmar.  All rights reserved
// See the file aplicense.txt for conditions of use.
//
// grouping files with packing:
//   zip, jar: PKZIP-compatible archives (store, deflate)
//   fout2   : Fallout II .DAT
//   vtdb    : Asphyre's VTDb
//   dfwad   : D2D:F wad archives
//
{.$DEFINE SFS_DEBUG_ZIPFS}
{.$DEFINE SFS_ZIPFS_FULL}
{$MODE DELPHI}
{.$R-}
unit sfsZipFS;

interface

uses
  SysUtils, Classes, Contnrs, sfs;


type
  TSFSZipVolumeType = (
    sfszvNone,
    sfszvZIP,
    {$IFDEF SFS_ZIPFS_FULL}
    sfszvF2DAT,
    sfszvVTDB,
    {$ENDIF}
    sfszvDFWAD
  );

  TSFSZipVolume = class(TSFSVolume)
  protected
    fType: TSFSZipVolumeType;

    procedure ZIPReadDirectory ();
    procedure DFWADReadDirectory ();
    {$IFDEF SFS_ZIPFS_FULL}
    procedure F2DATReadDirectory ();
    procedure VTDBReadDirectory ();
    {$ENDIF}

    procedure ReadDirectory (); override;
    procedure removeCommonPath (); override;

  public
    function OpenFileByIndex (const index: Integer): TStream; override;
  end;

  TSFSZipVolumeFactory = class (TSFSVolumeFactory)
  public
    function IsMyVolumePrefix (const prefix: TSFSString): Boolean; override;
    function Produce (const prefix, fileName: TSFSString; st: TStream): TSFSVolume; override;
    procedure Recycle (vol: TSFSVolume); override;
  end;



implementation

uses
  zstream, xstreams;


type
  TZDecompressionStream = TDecompressionStream;

type
  TSFSZipFileInfo = class (TSFSFileInfo)
  public
    fMethod: Byte; // 0: store; 8: deflate; 255: other
    fPackSz: Int64;
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

{$IFDEF SFS_ZIPFS_FULL}
function F2DATCheckMagic (st: TStream): Boolean;
var
  dsize, fiSz: Integer;
begin
  result := false;
  st.Position := st.Size-8;
  st.ReadBuffer(dsize, 4); st.ReadBuffer(fiSz, 4);
  st.Position := 0;
  if (fiSz <> st.Size) or (dsize < 5+13) or (dsize > fiSz-4) then exit;
  result := true;
end;

function VTDBCheckMagic (st: TStream): Boolean;
var
  sign: packed array [0..3] of Char;
  fcnt, dofs: Integer;
begin
  result := false;
  if st.Size < 32 then exit;
  st.ReadBuffer(sign[0], 4);
  st.ReadBuffer(fcnt, 4); st.ReadBuffer(dofs, 4);
  st.Seek(-12, soCurrent);
  if sign <> 'vtdm' then exit;
  if (fcnt < 0) or (dofs < 32) or (dofs+fcnt*8 > st.Size) then exit;
  result := true;
end;
{$ENDIF}

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
    if SFSUpCase(s0[f]) <> SFSUpCase(s1[f]) then begin result := f; exit; end;
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

{$IFDEF SFS_ZIPFS_FULL}
procedure TSFSZipVolume.F2DATReadDirectory ();
var
  dsize: Integer;
  fi: TSFSZipFileInfo;
  name: ShortString;
  f: Integer;
  b: Byte;
begin
  fFileStream.Position := fFileStream.Size-8;
  fFileStream.ReadBuffer(dsize, 4);
  fFileStream.Seek(-dsize, soCurrent); Dec(dsize, 4);
  while dsize > 0 do
  begin
    fi := TSFSZipFileInfo.Create(self);
    fFileStream.ReadBuffer(f, 4);
    if (f < 1) or (f > 255) then raise ESFSError.Create('invalid Fallout II .DAT file');
    Dec(dsize, 4+f+13);
    if dsize < 0 then raise ESFSError.Create('invalid Fallout II .DAT file');
    name[0] := chr(f); if f > 0 then fFileStream.ReadBuffer(name[1], f);
    f := 1; while (f <= ord(name[0])) and (name[f] <> #0) do Inc(f); name[0] := chr(f-1);
    fi.fName := name;
    fFileStream.ReadBuffer(b, 1); // packed?
    if b = 0 then fi.fMethod := 0 else fi.fMethod := 255;
    fFileStream.ReadBuffer(fi.fSize, 4);
    fFileStream.ReadBuffer(fi.fPackSz, 4);
    fFileStream.ReadBuffer(fi.fOfs, 4);
  end;
end;

procedure TSFSZipVolume.VTDBReadDirectory ();
// idiotic format
var
  fcnt, dofs: Integer;
  keys: array of record name: string; ofs: Integer; end;
  fi: TSFSZipFileInfo;
  f, c: Integer;
  rtype: Word;
begin
  fFileStream.Seek(4, soCurrent); // skip signature
  fFileStream.ReadBuffer(fcnt, 4);
  fFileStream.ReadBuffer(dofs, 4);
  fFileStream.Seek(dofs, soBeginning);

  // read keys
  SetLength(keys, fcnt);
  for f := 0 to fcnt-1 do
  begin
    fFileStream.ReadBuffer(c, 4);
    if (c < 0) or (c > 1023) then raise ESFSError.Create('invalid VTDB file');
    SetLength(keys[f].name, c);
    if c > 0 then
    begin
      fFileStream.ReadBuffer(keys[f].name[1], c);
      keys[f].name := SFSReplacePathDelims(keys[f].name, '/');
      if keys[f].name[1] = '/' then Delete(keys[f].name, 1, 1);
    end;
    fFileStream.ReadBuffer(keys[f].ofs, 4);
  end;

  // read records (record type will be converted to directory name)
  for f := 0 to fcnt-1 do
  begin
    fFileStream.Position := keys[f].ofs;
    fi := TSFSZipFileInfo.Create(self);
    fFileStream.ReadBuffer(rtype, 2);
    fFileStream.ReadBuffer(fi.fSize, 4);
    fFileStream.ReadBuffer(fi.fPackSz, 4);
    fi.fOfs := fFileStream.Position+12;
    fi.fName := keys[f].name;
    fi.fPath := IntToHex(rtype, 4)+'/';
    fi.fMethod := 255;
  end;
end;
{$ENDIF}

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
    {$IFDEF SFS_ZIPFS_FULL}
    sfszvF2DAT: F2DATReadDirectory();
    sfszvVTDB: VTDBReadDirectory();
    {$ENDIF}
    sfszvDFWAD: DFWADReadDirectory();
    else raise ESFSError.Create('invalid zipped SFS');
  end;
end;

function TSFSZipVolume.OpenFileByIndex (const index: Integer): TStream;
var
  zs: TZDecompressionStream;
  fs, rs: TStream;
  gs: TSFSGuardStream;
  kill: Boolean;
  buf: packed array [0..1023] of Char;
  rd: LongInt;
begin
  result := nil;
  zs := nil;
  fs := nil;
  gs := nil;
  rs := nil;
  if fFiles = nil then exit;
  if (index < 0) or (index >= fFiles.Count) or (fFiles[index] = nil) then exit;
  kill := false;
  try
    {
    try
      fs := TFileStream.Create(fFileName, fmOpenRead or fmShareDenyWrite);
      kill := true;
    except
      fs := fFileStream;
    end;
    }
    fs := fFileStream;
    if TSFSZipFileInfo(fFiles[index]).fMethod = 0 then
    begin
      result := TSFSPartialStream.Create(fs,
        TSFSZipFileInfo(fFiles[index]).fOfs,
        TSFSZipFileInfo(fFiles[index]).fSize, kill);
    end
    else
    begin
      fs.Seek(TSFSZipFileInfo(fFiles[index]).fOfs, soBeginning);
      if TSFSZipFileInfo(fFiles[index]).fMethod = 255 then
      begin
        // sorry, pals, DFWAD is completely broken, so users of it should SUFFER
        if TSFSZipFileInfo(fFiles[index]).fSize = -1 then
        begin
          TSFSZipFileInfo(fFiles[index]).fSize := 0;
          //writeln('trying to determine file size for [', TSFSZipFileInfo(fFiles[index]).fPath, TSFSZipFileInfo(fFiles[index]).fName, ']');
          zs := TZDecompressionStream.Create(fs);
          try
            while true do
            begin
              rd := zs.read(buf, 1024);
              //writeln('  got ', rd, ' bytes');
              if rd > 0 then Inc(TSFSZipFileInfo(fFiles[index]).fSize, rd);
              if rd < 1024 then break;
            end;
            //writeln('  resulting size: ', TSFSZipFileInfo(fFiles[index]).fSize, ' bytes');
            // recreate stream
            FreeAndNil(zs);
            fs.Seek(TSFSZipFileInfo(fFiles[index]).fOfs, soBeginning);
          except
            //writeln('*** CAN''T determine file size for [', TSFSZipFileInfo(fFiles[index]).fPath, TSFSZipFileInfo(fFiles[index]).fName, ']');
            FreeAndNil(zs);
            if kill then FreeAndNil(fs);
            result := nil;
            exit;
          end;
        end;
        rs := TSFSPartialStream.Create(fs, TSFSZipFileInfo(fFiles[index]).fOfs, TSFSZipFileInfo(fFiles[index]).fPackSz, true);
        zs := TZDecompressionStream.Create(rs);
        rs := nil;
      end
      else
      begin
        rs := TSFSPartialStream.Create(fs, TSFSZipFileInfo(fFiles[index]).fOfs, TSFSZipFileInfo(fFiles[index]).fPackSz, true);
        zs := TZDecompressionStream.Create(rs, true {-15}{MAX_WBITS});
        rs := nil;
      end;
      gs := TSFSGuardStream.Create(zs, fs, true, kill, false);
      zs := nil;
      fs := nil;
      result := TSFSPartialStream.Create(gs, 0, TSFSZipFileInfo(fFiles[index]).fSize, true);
    end;
  except
    FreeAndNil(rs);
    FreeAndNil(gs);
    FreeAndNil(zs);
    if kill then FreeAndNil(fs);
    result := nil;
    exit;
  end;
end;


{ TSFSZipVolumeFactory }
function TSFSZipVolumeFactory.IsMyVolumePrefix (const prefix: TSFSString): Boolean;
begin
  result :=
    SFSStrEqu(prefix, 'zip') or
    SFSStrEqu(prefix, 'dfwad')
    {$IFDEF SFS_ZIPFS_FULL}
    or SFSStrEqu(prefix, 'jar') or
    SFSStrEqu(prefix, 'fout2') or
    SFSStrEqu(prefix, 'vtdb') or
    SFSStrEqu(prefix, 'wad')
    {$ENDIF}
    ;
end;

procedure TSFSZipVolumeFactory.Recycle (vol: TSFSVolume);
begin
  vol.Free();
end;

function TSFSZipVolumeFactory.Produce (const prefix, fileName: TSFSString; st: TStream): TSFSVolume;
var
  vt: TSFSZipVolumeType;
begin
  vt := sfszvNone;
       if ZIPCheckMagic(st) then vt := sfszvZIP
  else if DFWADCheckMagic(st) then vt := sfszvDFWAD
  {$IFDEF SFS_ZIPFS_FULL}
  else if F2DATCheckMagic(st) then vt := sfszvF2DAT
  else if VTDBCheckMagic(st) then vt := sfszvVTDB
  {$ENDIF}
  ;

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
