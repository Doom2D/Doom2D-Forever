(* Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
 * This file is part of Doom2D Forever.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation, version 3 of
 * the License ONLY.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 *)

// grouping files with packing:
//   zip, pk3: PKZIP-compatible archives (store, deflate)
//   dfwad   : D2D:F wad archives
//
{.$DEFINE SFS_DEBUG_ZIPFS}
{$INCLUDE ../shared/a_modes.inc}
{$SCOPEDENUMS OFF}
{.$R+}
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

  TSFSZipVolumeFactory = class (TSFSVolumeFactoryMethods)
  public
    class function IsMyVolumePrefix (const prefix: AnsiString): Boolean; override;
    class function Produce (const prefix, fileName: AnsiString; st: TStream): TSFSVolume; override;
    class procedure Recycle (vol: TSFSVolume); override;
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
  hdr.version := st.ReadByte();
  hdr.hostOS := st.ReadByte();
  hdr.flags := st.ReadWordLE();
  hdr.method := st.ReadWordLE();
  hdr.time := st.ReadDWordLE();
  hdr.crc := st.ReadDWordLE();
  hdr.packSz := st.ReadDWordLE();
  hdr.unpackSz := st.ReadDWordLE();
  hdr.fnameSz := st.ReadWordLE();
  hdr.localExtraSz := st.ReadWordLE();
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
  {fcnt :=} st.ReadWordLE();
  st.Seek(-8, soCurrent);
  if (sign[0] <> 'D') and (sign[1] <> 'F') and (sign[2] <> 'W') and
     (sign[3] <> 'A') and (sign[4] <> 'D') and (sign[5] <> #$01) then exit;
  result := true;
end;


{ TSFSZipVolume }
procedure TSFSZipVolume.ZIPReadDirectory ();
var
  fi: TSFSZipFileInfo;
  fname: AnsiString;
  sign: packed array [0..3] of Char;
  lhdr: TZLocalFileHeader;
  ignoreFile: Boolean;
  efid, efsz: Word;
  izver: Byte;
  izcrc: LongWord;
  buf: PByte;
  bufsz, f: Integer;
  cdofs, hdrofs: Int64;
  cdsize: LongWord;
  fileOffsets: array of Int64;
  nameLen, extraLen, commentLen: Word;
  fileIdx: Integer = -1;
begin
  // search for central dir pointer
  if fFileStream.size > 65636 then bufsz := 65636 else bufsz := fFileStream.size;
  fFileStream.position := fFileStream.size-bufsz;
  GetMem(buf, bufsz);
  cdofs := -1;
  cdsize := 0;
  try
    fFileStream.readBuffer(buf^, bufsz);
    for f := bufsz-16 downto 4 do
    begin
      if (buf[f-4] = ord('P')) and (buf[f-3] = ord('K')) and (buf[f-2] = 5) and (buf[f-1] = 6) then
      begin
        cdsize := LongWord(buf[f+8])+(LongWord(buf[f+9])<<8)+(LongWord(buf[f+10])<<16)+(LongWord(buf[f+11])<<24);
        cdofs := Int64(buf[f+12])+(Int64(buf[f+13])<<8)+(Int64(buf[f+14])<<16)+(Int64(buf[f+15])<<24);
        break;
      end;
    end;
  finally
    FreeMem(buf);
  end;

  if (cdofs >= 0) and (cdsize > 0) then
  begin
    // wow, we got central directory! process it
    fFileStream.position := cdofs;
    while cdsize >= 4 do
    begin
      Dec(cdsize, 4);
      fFileStream.readBuffer(sign, 4);
      if sign = 'PK'#1#2 then
      begin
        if cdsize < 42 then break;
        Dec(cdsize, 42);
        // skip uninteresting fields
        fFileStream.seek(2+2+2+2+2+2+4+4+4, soCurrent);
        nameLen := fFileStream.ReadWordLE();
        extraLen := fFileStream.ReadWordLE();
        commentLen := fFileStream.ReadWordLE();
        // skip uninteresting fields
        fFileStream.seek(2+2+4, soCurrent);
        hdrofs := fFileStream.ReadDWordLE();
        // now skip name, extra and comment
        if cdsize < nameLen+extraLen+commentLen then break;
        Dec(cdsize, nameLen+extraLen+commentLen);
        fFileStream.seek(nameLen+extraLen+commentLen, soCurrent);
        SetLength(fileOffsets, length(fileOffsets)+1);
        fileOffsets[high(fileOffsets)] := hdrofs;
        //writeln('file #', high(fileOffsets), ' found at ', hdrofs);
      end
      else if sign = 'PK'#7#8 then
      begin
        if cdsize < 3*4 then break;
        Dec(cdsize, 3*4);
        fFileStream.seek(3*4, soCurrent);
      end
      else
      begin
        break;
      end;
    end;
    if length(fileOffsets) = 0 then exit; // no files at all
    fileIdx := 0;
  end
  else
  begin
    fFileStream.position := 0;
  end;

  // read local directory
  repeat
    if fileIdx >= 0 then
    begin
      if fileIdx > High(fileOffsets) then break;
      //writeln('reading file #', fileIdx, ' at ', fileOffsets[fileIdx]);
      fFileStream.position := fileOffsets[fileIdx];
      Inc(fileIdx);
    end;

    while true do
    begin
      fFileStream.ReadBuffer(sign[0], Length(sign));
      // skip data descriptor
      if sign = 'PK'#7#8 then
      begin
        fFileStream.seek(3*4, soCurrent);
        continue;
      end;
      break;
    end;
    if sign <> 'PK'#3#4 then break;

    ignoreFile := false;

    readLFH(fFileStream, lhdr);

    fi := TSFSZipFileInfo.Create(self);
    fi.fPackSz := 0;
    fi.fMethod := 0;

    SetLength(fname, lhdr.fnameSz);
    if lhdr.fnameSz > 0 then
    begin
      fFileStream.ReadBuffer(fname[1], length(fname));
      fi.fName := utf8to1251(fname);
    end;

    // here we should process extra field: it may contain utf8 filename
    while lhdr.localExtraSz >= 4 do
    begin
      efid := fFileStream.ReadWordLE();
      efsz := fFileStream.ReadWordLE();
      Dec(lhdr.localExtraSz, 4);
      if efsz > lhdr.localExtraSz then break;
      // Info-ZIP Unicode Path Extra Field?
      if (efid = $7075) and (efsz > 5) then
      begin
        fFileStream.ReadBuffer(izver, 1);
        Dec(efsz, 1);
        Dec(lhdr.localExtraSz, 1);
        if izver = 1 then
        begin
          //writeln('!!!!!!!!!!!!');
          Dec(lhdr.localExtraSz, efsz);
          fFileStream.ReadBuffer(izcrc, 4); // name crc, ignore it for now
          Dec(efsz, 4);
          SetLength(fname, efsz);
          if length(fname) > 0 then fFileStream.readBuffer(fname[1], length(fname));
          fi.fName := utf8to1251(fname);
          //writeln('++++++ [', fi.fName, ']');
          efsz := 0;
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

    if (length(fi.fName) = 0) or (fname[length(fi.fName)] = '/') or (fname[length(fi.fName)] = '\') then
    begin
      ignoreFile := true;
    end
    else
    begin
      for f := 1 to length(fi.fName) do if fi.fName[f] = '\' then fi.fName[f] := '/';
    end;

    fi.fOfs := fFileStream.Position;
    fi.fSize := lhdr.unpackSz;
    fi.fPackSz := lhdr.packSz;
    fi.fMethod := lhdr.method;
    if fi.fMethod = 0 then fi.fPackSz := fi.fSize;

    // skip packed data
    if fileIdx < 0 then fFileStream.Seek(lhdr.packSz, soCurrent);
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
  fcnt := fFileStream.ReadWordLE();
  if fcnt = 0 then exit;
  // read files
  for f := 0 to fcnt-1 do
  begin
    fFileStream.ReadBuffer(name[0], 16);
    fofs := fFileStream.ReadDWordLE();
    fpksize := fFileStream.ReadDWordLE();
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
class function TSFSZipVolumeFactory.IsMyVolumePrefix (const prefix: AnsiString): Boolean;
begin
  result :=
    StrEquCI1251(prefix, 'zip') or
    StrEquCI1251(prefix, 'pk3') or
    StrEquCI1251(prefix, 'dfz') or
    StrEquCI1251(prefix, 'dfwad') or
    StrEquCI1251(prefix, 'dfzip');
end;

class procedure TSFSZipVolumeFactory.Recycle (vol: TSFSVolume);
begin
  vol.Free();
end;

class function TSFSZipVolumeFactory.Produce (const prefix, fileName: AnsiString; st: TStream): TSFSVolume;
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

initialization
  SFSRegisterVolumeFactory(TSFSZipVolumeFactory);
end.
