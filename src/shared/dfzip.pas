(* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../shared/a_modes.inc}
unit dfzip;

  (** Based on WadCvt tool **)

interface

  uses SysUtils, Classes;

  type
    TFileInfo = class
    public
      name: AnsiString;
      pkofs: Int64; // offset of file header
      size: Int64;
      pksize: Int64;
      crc: LongWord;
      method: Word;

      constructor Create ();
    end;

  function ZipOne (ds: TStream; fname: AnsiString; st: TStream; dopack: Boolean=true): TFileInfo;
  procedure writeCentralDir (ds: TStream; files: array of TFileInfo);

implementation

  uses utils, xstreams, crc, paszlib, e_log;

  const
    uni2wint: array [128..255] of Word = (
      $0402,$0403,$201A,$0453,$201E,$2026,$2020,$2021,$20AC,$2030,$0409,$2039,$040A,$040C,$040B,$040F,
      $0452,$2018,$2019,$201C,$201D,$2022,$2013,$2014,$003F,$2122,$0459,$203A,$045A,$045C,$045B,$045F,
      $00A0,$040E,$045E,$0408,$00A4,$0490,$00A6,$00A7,$0401,$00A9,$0404,$00AB,$00AC,$00AD,$00AE,$0407,
      $00B0,$00B1,$0406,$0456,$0491,$00B5,$00B6,$00B7,$0451,$2116,$0454,$00BB,$0458,$0405,$0455,$0457,
      $0410,$0411,$0412,$0413,$0414,$0415,$0416,$0417,$0418,$0419,$041A,$041B,$041C,$041D,$041E,$041F,
      $0420,$0421,$0422,$0423,$0424,$0425,$0426,$0427,$0428,$0429,$042A,$042B,$042C,$042D,$042E,$042F,
      $0430,$0431,$0432,$0433,$0434,$0435,$0436,$0437,$0438,$0439,$043A,$043B,$043C,$043D,$043E,$043F,
      $0440,$0441,$0442,$0443,$0444,$0445,$0446,$0447,$0448,$0449,$044A,$044B,$044C,$044D,$044E,$044F
    );

constructor TFileInfo.Create;
begin
  name := '';
  pkofs := 0;
  size := 0;
  pksize := 0;
  crc := crc32(0, nil, 0);
  method := 0;
end;

function toUtf8 (const s: AnsiString): AnsiString;
var
  uc: PUnicodeChar;
  xdc: PChar;
  pos, f: Integer;
begin
  GetMem(uc, length(s)*8);
  GetMem(xdc, length(s)*8);
  try
    FillChar(uc^, length(s)*8, 0);
    FillChar(xdc^, length(s)*8, 0);
    pos := 0;
    for f := 1 to length(s) do
    begin
      if ord(s[f]) < 128 then
        uc[pos] := UnicodeChar(ord(s[f]))
      else
        uc[pos] := UnicodeChar(uni2wint[ord(s[f])]);
      Inc(pos);
    end;
    FillChar(xdc^, length(s)*8, 0);
    f := UnicodeToUtf8(xdc, length(s)*8, uc, pos);
    while (f > 0) and (xdc[f-1] = #0) do Dec(f);
    SetLength(result, f);
    Move(xdc^, result[1], f);
  finally
    FreeMem(xdc);
    FreeMem(uc);
  end;
end;

// returs crc
function zpack (ds: TStream; ss: TStream; var aborted: Boolean): LongWord;
const
  IBSize = 65536;
  OBSize = 65536;
var
  zst: TZStream;
  ib, ob: PByte;
  err: Integer;
  rd: Integer;
  eof: Boolean;
  crc: LongWord;
  dstp, srcsize: Int64;
begin
  result := 0;
  //aborted := true; exit;
  aborted := false;
  crc := crc32(0, nil, 0);
  GetMem(ib, IBSize);
  GetMem(ob, OBSize);
  ss.position := 0;
  dstp := ds.position;
  srcsize := ss.size;
  try
    zst.next_out := ob;
    zst.avail_out := OBSize;
    zst.next_in := ib;
    zst.avail_in := 0;
    err := deflateInit2(zst, Z_BEST_COMPRESSION, Z_DEFLATED, -15, 9, 0);
    if err <> Z_OK then raise Exception.Create(zerror(err));
    try
      eof := false;
      repeat
        if zst.avail_in = 0 then
        begin
          // read input buffer part
          rd := ss.read(ib^, IBSize);
          if rd < 0 then raise Exception.Create('reading error');
          //writeln('  read ', rd, ' bytes');
          eof := (rd = 0);
          if rd <> 0 then begin crc := crc32(crc, Pointer(ib), rd); result := crc; end;
          zst.next_in := ib;
          zst.avail_in := rd;
        end;
        // now process the whole input
        while zst.avail_in > 0 do
        begin
          err := deflate(zst, Z_NO_FLUSH);
          if err <> Z_OK then raise Exception.Create(zerror(err));
          if zst.avail_out < OBSize then
          begin
            //writeln('  written ', OBSize-zst.avail_out, ' bytes');
            if ds.position+(OBSize-zst.avail_out)-dstp >= srcsize then
            begin
              // this will be overwritten anyway
              aborted := true;
              exit;
            end;
            ds.writeBuffer(ob^, OBSize-zst.avail_out);
            zst.next_out := ob;
            zst.avail_out := OBSize;
          end;
        end;
      until eof;
      // do leftovers
      while true do
      begin
        zst.avail_in := 0;
        err := deflate(zst, Z_FINISH);
        if (err <> Z_OK) and (err <> Z_STREAM_END) then raise Exception.Create(zerror(err));
        if zst.avail_out < OBSize then
        begin
          //writeln('  .written ', OBSize-zst.avail_out, ' bytes');
          if ds.position+(OBSize-zst.avail_out)-dstp >= srcsize then
          begin
            // this will be overwritten anyway
            aborted := true;
            exit;
          end;
          ds.writeBuffer(ob^, OBSize-zst.avail_out);
          zst.next_out := ob;
          zst.avail_out := OBSize;
        end;
        if err <> Z_OK then break;
      end;
      // succesfully flushed?
      if (err <> Z_STREAM_END) then raise Exception.Create(zerror(err));
    finally
      deflateEnd(zst);
    end;
  finally
    FreeMem(ob);
    FreeMem(ib);
  end;
end;

// this will write "extra field length" and extra field itself
{$IFDEF UTFEXTRA}
const UtfFlags = 0;

type
  TByteArray = array of Byte;

function buildUtfExtra (fname: AnsiString): TByteArray;
var
  crc: LongWord;
  fu: AnsiString;
  sz: Word;
begin
  fu := toUtf8(fname);
  if fu = fname then begin result := nil; exit; end; // no need to write anything
  crc := crc32(0, @fname[1], length(fname));
  sz := 2+2+1+4+length(fu);
  SetLength(result, sz);
  result[0] := ord('u');
  result[1] := ord('p');
  Dec(sz, 4);
  result[2] := sz and $ff;
  result[3] := (sz shr 8) and $ff;
  result[4] := 1;
  result[5] := crc and $ff;
  result[6] := (crc shr 8) and $ff;
  result[7] := (crc shr 16) and $ff;
  result[8] := (crc shr 24) and $ff;
  Move(fu[1], result[9], length(fu));
end;
{$ELSE}
const UtfFlags = (1 shl 10); // bit 11
{$ENDIF}

function ZipOne (ds: TStream; fname: AnsiString; st: TStream; dopack: Boolean=true): TFileInfo;
var
  oldofs, nfoofs, pkdpos, rd: Int64;
  sign: packed array [0..3] of Char;
  buf: PChar;
  bufsz: Integer;
  aborted: Boolean = false;
{$IFDEF UTFEXTRA}
  ef: TByteArray;
{$ENDIF}
begin
  result := TFileInfo.Create();
  result.pkofs := ds.position;
  result.size := st.size;
  if result.size > 0 then result.method := 8 else result.method := 0;
  if not dopack then
  begin
    result.method := 0;
    result.pksize := result.size;
  end;
{$IFDEF UTFEXTRA}
  result.name := fname;
  ef := buildUtfExtra(result.name);
{$ELSE}
  result.name := toUtf8(fname);
{$ENDIF}
  // write local header
  sign := 'PK'#3#4;
  ds.writeBuffer(sign, 4);
  writeInt(ds, Word($0A10)); // version to extract
  writeInt(ds, Word(UtfFlags)); // flags
  writeInt(ds, Word(result.method)); // compression method
  writeInt(ds, Word(0)); // file time
  writeInt(ds, Word(0)); // file date
  nfoofs := ds.position;
  writeInt(ds, LongWord(result.crc)); // crc32
  writeInt(ds, LongWord(result.pksize)); // packed size
  writeInt(ds, LongWord(result.size)); // unpacked size
  writeInt(ds, Word(length(fname))); // name length
{$IFDEF UTFEXTRA}
  writeInt(ds, Word(length(ef))); // extra field length
{$ELSE}
  writeInt(ds, Word(0)); // extra field length
{$ENDIF}
  ds.writeBuffer(fname[1], length(fname));
{$IFDEF UTFEXTRA}
  if length(ef) > 0 then ds.writeBuffer(ef[0], length(ef));
{$ENDIF}
  if dopack then
  begin
    // now write packed data
    if result.size > 0 then
    begin
      pkdpos := ds.position;
      st.position := 0;
      result.crc := zpack(ds, st, aborted);
      result.pksize := ds.position-pkdpos;
      if {result.pksize >= result.size} aborted then
      begin
        // there's no sence to pack this file, so just store it
        st.position := 0;
        ds.position := result.pkofs;
        result.Free();
        // store it
        result := ZipOne(ds, fname, st, false);
        exit;
      end
      else
      begin
        // fix header
        oldofs := ds.position;
        ds.position := nfoofs;
        writeInt(ds, LongWord(result.crc)); // crc32
        writeInt(ds, LongWord(result.pksize)); // crc32
        ds.position := oldofs;
      end;
    end;
  end
  else
  begin
    bufsz := 1024*1024;
    GetMem(buf, bufsz);
    try
      st.position := 0;
      result.crc := crc32(0, nil, 0);
      result.pksize := 0;
      while result.pksize < result.size do
      begin
        rd := result.size-result.pksize;
        if rd > bufsz then rd := bufsz;
        st.readBuffer(buf^, rd);
        ds.writeBuffer(buf^, rd);
        Inc(result.pksize, rd);
        result.crc := crc32(result.crc, buf, rd);
      end;
    finally
      FreeMem(buf);
    end;
    // fix header
    oldofs := ds.position;
    ds.position := nfoofs;
    writeInt(ds, LongWord(result.crc)); // crc32
    ds.position := oldofs;
    //write('(S) ');
  end;
end;


procedure writeCentralDir (ds: TStream; files: array of TFileInfo);
var
  cdofs, cdend: Int64;
  sign: packed array [0..3] of Char;
  f: Integer;
{$IFDEF UTFEXTRA}
  ef: TByteArray;
{$ENDIF}
begin
  cdofs := ds.position;
  for f := 0 to high(files) do
  begin
{$IFDEF UTFEXTRA}
    ef := buildUtfExtra(files[f].name);
{$ENDIF}
    sign := 'PK'#1#2;
    ds.writeBuffer(sign, 4);
    writeInt(ds, Word($0A10)); // version made by
    writeInt(ds, Word($0010)); // version to extract
    writeInt(ds, Word(UtfFlags)); // flags
    writeInt(ds, Word(files[f].method)); // compression method
    writeInt(ds, Word(0)); // file time
    writeInt(ds, Word(0)); // file date
    writeInt(ds, LongWord(files[f].crc));
    writeInt(ds, LongWord(files[f].pksize));
    writeInt(ds, LongWord(files[f].size));
    writeInt(ds, Word(length(files[f].name))); // name length
{$IFDEF UTFEXTRA}
    writeInt(ds, Word(length(ef))); // extra field length
{$ELSE}
    writeInt(ds, Word(0)); // extra field length
{$ENDIF}
    writeInt(ds, Word(0)); // comment length
    writeInt(ds, Word(0)); // disk start
    writeInt(ds, Word(0)); // internal attributes
    writeInt(ds, LongWord(0)); // external attributes
    writeInt(ds, LongWord(files[f].pkofs)); // header offset
    ds.writeBuffer(files[f].name[1], length(files[f].name));
{$IFDEF UTFEXTRA}
    if length(ef) > 0 then ds.writeBuffer(ef[0], length(ef));
{$ENDIF}
  end;
  cdend := ds.position;
  // write end of central dir
  sign := 'PK'#5#6;
  ds.writeBuffer(sign, 4);
  writeInt(ds, Word(0)); // disk number
  writeInt(ds, Word(0)); // disk with central dir
  writeInt(ds, Word(length(files))); // number of files on this dist
  writeInt(ds, Word(length(files))); // number of files total
  writeInt(ds, LongWord(cdend-cdofs)); // size of central directory
  writeInt(ds, LongWord(cdofs)); // central directory offset
  writeInt(ds, Word(0)); // archive comment length
end;

end.
