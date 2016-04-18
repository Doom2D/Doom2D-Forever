{$MODE OBJFPC}
{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}
program __wadcvt__;

uses
  SysUtils,
  Classes,
  utils in '../shared/utils.pas',
  xstreams in '../shared/xstreams.pas',
  crc,
  sfs,
  sfsPlainFS,
  sfsZipFS,
  paszlib;


procedure processed (count: Cardinal);
begin
  //writeln('  read ', count, ' bytes');
end;


// returs crc
function zpack (ds: TStream; ss: TStream): LongWord;
const
  IBSize = 65536;
  OBSize = 65536;
var
  zst: TZStream;
  ib, ob: PByte;
  ibpos: Cardinal;
  err: Integer;
  rd, f: Integer;
  eof: Boolean;
  crc: LongWord;
begin
  result := 0;
  crc := crc32(0, nil, 0);
  GetMem(ib, IBSize);
  GetMem(ob, OBSize);
  try
    zst.next_out := ob;
    zst.avail_out := OBSize;
    err := deflateInit2(zst, Z_BEST_COMPRESSION, Z_DEFLATED, -15, 9, 0);
    if err <> Z_OK then raise Exception.Create(zerror(err));
    try
      ibpos := 0;
      zst.next_out := ob;
      zst.avail_out := OBSize;
      eof := false;
      while true do
      begin
        while not eof and (ibpos < IBSize) do
        begin
          rd := ss.read((ib+ibpos)^, IBSize-ibpos);
          if rd < 0 then raise Exception.Create('reading error');
          eof := (rd <> IBSize-ibpos);
          if rd <> 0 then begin crc := crc32(crc, Pointer(ib+ibpos), rd); result := crc; end;
          Inc(ibpos, rd);
          if rd <> 0 then processed(rd);
        end;
        zst.next_in := ib;
        zst.avail_in := ibpos;
        if eof then break;
        err := deflate(zst, Z_NO_FLUSH);
        if (err <> Z_OK) and (err <> Z_STREAM_END) then raise Exception.Create(zerror(err));
        if zst.avail_out < OBSize then
        begin
          //writeln('  written ', OBSize-zst.avail_out, ' bytes');
          ds.writeBuffer(ob^, OBSize-zst.avail_out);
          zst.next_out := ob;
          zst.avail_out := OBSize;
        end;
        // shift input buffer
        if zst.avail_in < ibpos then
        begin
          rd := 0;
          for f := ibpos-zst.avail_in to ibpos-1 do
          begin
            ib[rd] := ib[f];
            Inc(rd);
          end;
          ibpos := rd;
          //writeln(' rd: ', zst.avail_in);
        end;
      end;
      // pack leftovers
      while zst.avail_in > 0 do
      begin
        err := deflate(zst, Z_NO_FLUSH);
        if (err <> Z_OK) and (err <> Z_STREAM_END) then raise Exception.Create(zerror(err));
        if zst.avail_out < OBSize then
        begin
          //writeln('  written ', OBSize-zst.avail_out, ' bytes');
          ds.writeBuffer(ob^, OBSize-zst.avail_out);
          zst.next_out := ob;
          zst.avail_out := OBSize;
        end;
      end;
      // stream compressed, flush zstream
      while true do
      begin
        zst.avail_in := 0;
        zst.next_out := ob;
        zst.avail_out := OBSize;
        err := deflate(zst, Z_FINISH);
        if zst.avail_out < OBSize then
        begin
          //writeln('  written ', OBSize-zst.avail_out, ' bytes');
          ds.writeBuffer(ob^, OBSize-zst.avail_out);
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


{
procedure TProg.putStr (const s: string; newline: Boolean=false);
begin
  write(#13, s);
  while lastlen > length(s) do
  begin
    write(' ');
    Dec(lastlen);
  end;
  if newline then
  begin
    writeln;
    lastlen := 0;
  end
  else
  begin
    lastlen := length(s);
  end;
end;

procedure TProg.onProgress (sender: TObject; const percent: double);
var
  prc: Integer;
begin
  prc := trunc(percent*100.0);
  putStr(Format('compressing %-33s  %3d%%', [lastname, prc]));
end;

procedure TProg.onFileStart (sender: TObject; const fileName: string);
begin
  lastname := fileName;
  putStr(Format('compressing %-33s  %3d%%', [lastname, 0]));
end;

procedure TProg.onFileEnd (sender: TObject; const ratio: double);
begin
  putStr(Format('compressed  %-33s  %f', [lastname, ratio]), true);
end;
}


// returns new file name
function detectExt (fpath, fname: string; fs: TStream): string;
var
  buf: PChar;
  buflen: Integer;
  f: Integer;
  st: string[24];
begin
  result := fname;
  if length(ExtractFileExt(fname)) <> 0 then exit;
  if fs.size < 16 then exit;
  buflen := Integer(fs.size);
  GetMem(buf, buflen);
  try
    fs.ReadBuffer(buf^, buflen);
    // xm
    Move(buf^, (PChar(@st[1]))^, 16);
    st[0] := #16;
    if (st = 'Extended Module:') then
    begin
      result := result+'.xm';
      exit;
    end;
    if (buf[0] = 'D') and (buf[1] = 'F') and (buf[2] = 'W') and
       (buf[3] = 'A') and (buf[4] = 'D') and (buf[5] = #$1) then
    begin
      result := result+'.wad';
      exit;
    end;
    if (buf[0] = 'M') and (buf[1] = 'A') and (buf[2] = 'P') and (buf[3] = #$1) then
    begin
      result := result+'.dfmap';
      exit;
    end;
    if (buf[0] = 'M') and (buf[1] = 'T') and (buf[2] = 'h') and (buf[3] = 'd') then
    begin
      result := result+'.mid';
      exit;
    end;
    if (buf[0] = 'R') and (buf[1] = 'I') and (buf[2] = 'F') and (buf[3] = 'F') and
       (buf[8] = 'W') and (buf[9] = 'A') and (buf[10] = 'V') and (buf[11] = 'E') then
    begin
      result := result+'.wav';
      exit;
    end;
    // mp3 (stupid hack)
    for f := 0 to 128-6 do
    begin
      if (buf[f+0] = #$4) and (buf[f+1] = 'L') and
         (buf[f+2] = 'A') and (buf[f+3] = 'M') and
         (buf[f+4] = 'E') and (buf[f+5] = '3') then
      begin
        result := result+'.mp3';
        exit;
      end;
    end;
    // more mp3 hacks
    if (buf[0] = 'I') and (buf[1] = 'D') and (buf[2] = '3') and (buf[3] <= #4) then
    begin
      result := result+'.mp3';
      exit;
    end;
    if buflen > 128 then
    begin
      if (buf[buflen-128] = 'T') and (buf[buflen-127] = 'A') and (buf[buflen-126] = 'G') then
      begin
        result := result+'.mp3';
        exit;
      end;
    end;
    // targa (stupid hack; this "signature" is not required by specs)
    if buflen >= 18 then
    begin
      Move((buf+buflen-18)^, (PChar(@st[1]))^, 16);
      st[0] := #16;
      if st = 'TRUEVISION-XFILE' then
      begin
        result := result+'.tga';
        exit;
      end;
    end;
  finally
    FreeMem(buf);
  end;
end;


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

constructor TFileInfo.Create ();
begin
  name := '';
  pkofs := 0;
  size := 0;
  pksize := 0;
  crc := crc32(0, nil, 0);
  method := 0;
end;


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


// this will write "extra field length" and extra field itself
type
  TByteArray = array of Byte;

function buildUtfExtra (fname: string): TByteArray;
var
  crc: LongWord;
  fu: string;
  sz: Word;
  uc: PUnicodeChar;
  xdc: PChar;
  pos, f: Integer;
begin
  GetMem(uc, length(fname)*8);
  GetMem(xdc, length(fname)*8);
  try
    FillChar(uc^, length(fname)*8, 0);
    FillChar(xdc^, length(fname)*8, 0);
    pos := 0;
    for f := 1 to length(fname) do
    begin
      if ord(fname[f]) < 128 then
        uc[pos] := UnicodeChar(ord(fname[f]))
      else
        uc[pos] := UnicodeChar(uni2wint[ord(fname[f])]);
      Inc(pos);
    end;
    FillChar(xdc^, length(fname)*8, 0);
    f := UnicodeToUtf8(xdc, length(fname)*8, uc, pos);
    while (f > 0) and (xdc[f-1] = #0) do Dec(f);
    SetLength(fu, f);
    Move(xdc^, fu[1], f);
    //writeln('[', fu, ']');
  finally
    FreeMem(xdc);
    FreeMem(uc);
  end;

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
  //result := nil;
end;


function ZipOne (ds: TStream; fname: string; st: TStream): TFileInfo;
var
  oldofs, nfoofs, pkdpos: Int64;
  sign: packed array [0..3] of Char;
  ef: TByteArray;
begin
  result := TFileInfo.Create();
  result.pkofs := ds.position;
  result.size := st.size;
  result.name := fname;
  if result.size > 0 then result.method := 8 else result.method := 0;
  ef := buildUtfExtra(result.name);
  // write local header
  sign := 'PK'#3#4;
  ds.writeBuffer(sign, 4);
  writeInt(ds, Word($0A10)); // version to extract
  //writeInt(ds, Word(1 shl 11)); // flags: utf-8 name
  writeInt(ds, Word(0)); // flags
  writeInt(ds, Word(result.method)); // compression method
  writeInt(ds, Word(0)); // file time
  writeInt(ds, Word(0)); // file date
  nfoofs := ds.position;
  writeInt(ds, LongWord(result.crc)); // crc32
  writeInt(ds, LongWord(result.pksize)); // packed size
  writeInt(ds, LongWord(result.size)); // unpacked size
  writeInt(ds, Word(length(fname))); // name length
  writeInt(ds, Word(length(ef))); // extra field length
  ds.writeBuffer(fname[1], length(fname));
  if length(ef) > 0 then ds.writeBuffer(ef[0], length(ef));
  // now write packed data
  if result.size > 0 then
  begin
    pkdpos := ds.position;
    st.position := 0;
    result.crc := zpack(ds, st);
    result.pksize := ds.position-pkdpos;
    // fix header
    oldofs := ds.position;
    ds.position := nfoofs;
    writeInt(ds, LongWord(result.crc)); // crc32
    writeInt(ds, LongWord(result.pksize)); // crc32
    ds.position := oldofs;
  end;
end;


procedure writeCentralDir (ds: TStream; files: array of TFileInfo);
var
  cdofs, cdend: Int64;
  sign: packed array [0..3] of Char;
  f: Integer;
  ef: TByteArray;
begin
  cdofs := ds.position;
  for f := 0 to high(files) do
  begin
    ef := buildUtfExtra(files[f].name);
    sign := 'PK'#1#2;
    ds.writeBuffer(sign, 4);
    writeInt(ds, Word($0A10)); // version made by
    writeInt(ds, Word($0010)); // version to extract
    //writeInt(ds, Word(1 shl 11)); // flags: utf-8 name
    writeInt(ds, Word(0)); // flags
    writeInt(ds, Word(files[f].method)); // compression method
    writeInt(ds, Word(0)); // file time
    writeInt(ds, Word(0)); // file date
    writeInt(ds, LongWord(files[f].crc));
    writeInt(ds, LongWord(files[f].pksize));
    writeInt(ds, LongWord(files[f].size));
    writeInt(ds, Word(length(files[f].name))); // name length
    writeInt(ds, Word(length(ef))); // extra field length
    writeInt(ds, Word(0)); // comment length
    writeInt(ds, Word(0)); // disk start
    writeInt(ds, Word(0)); // internal attributes
    writeInt(ds, LongWord(0)); // external attributes
    writeInt(ds, LongWord(files[f].pkofs)); // header offset
    ds.writeBuffer(files[f].name[1], length(files[f].name));
    if length(ef) > 0 then ds.writeBuffer(ef[0], length(ef));
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


var
  fs, fo: TStream;
  fl: TSFSFileList;
  f: Integer;
  infname: string;
  outfname: string;
  dvfn: string;
  newname: string;
  files: array of TFileInfo;
  nfo: TFileInfo;
begin
  if ParamCount() < 1 then
  begin
    WriteLn('usage: wadcvt file.wad');
    Halt(1);
  end;

  infname := ParamStr(1);
  if not StrEquCI1251(ExtractFileExt(infname), '.wad') and not StrEquCI1251(ExtractFileExt(infname), '.dfwad') then
  begin
    writeln('wtf?!');
    Halt(1);
  end;

  if ParamCount() > 1 then
  begin
    outfname := ParamStr(2);
  end
  else
  begin
    outfname := ChangeFileExt(infname, '.pk3');
  end;

  if not SFSAddDataFile(infname) then begin WriteLn('shit!'); Halt(1); end;
  dvfn := SFSGetLastVirtualName(infname);

  files := nil;

  fl := SFSFileList(dvfn);
  if fl = nil then
  begin
    writeln('wtf?!');
    Halt(1);
  end;

  fo := TFileStream.Create(outfname, fmCreate);
  try
    for f := 0 to fl.Count-1 do
    begin
      if length(fl[f].fName) = 0 then continue;
      fs := SFSFileOpen(dvfn+'::'+fl[f].fPath+fl[f].fName);
      newname := detectExt(fl[f].fPath, fl[f].fName, fs);
      fs.position := 0;
      writeln('[', f+1, '/', fl.Count, ']: ', fl[f].fPath, newname, '  ', fs.size);
      nfo := ZipOne(fo, fl[f].fPath+newname, fs);
      SetLength(files, length(files)+1);
      files[high(files)] := nfo;
    end;
    writeCentralDir(fo, files);
  except
    fo.Free();
    fo := nil;
    DeleteFile(outfname);
  end;
  if fo <> nil then fo.Free();
end.
