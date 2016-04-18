{$MODE DELPHI}
unit utils;

interface

// does filename have one of ".wad", ".pk3", ".zip" extensions?
function hasWadExtension (fn: AnsiString): Boolean;

// does filepath have ".XXX:\" in it?
function isWadPath (fn: AnsiString): Boolean;

// adds ".wad" extension if filename doesn't have one of ".wad", ".pk3", ".zip"
function addWadExtension (fn: AnsiString): AnsiString;

// convert number to strig with nice commas
function Int64ToStrComma (i: Int64): AnsiString;

function UpCase1251 (ch: Char): Char;

// `true` if strings are equal; ignoring case for cp1251
function StrEquCI1251 (const s0, s1: AnsiString): Boolean;

function utf8Valid (const s: AnsiString): Boolean;

function utf8to1251 (s: AnsiString): AnsiString;

// `pathname` will be modified if path is valid
// `lastIsDir` should be `true` if we are searching for directory
// nobody cares about shitdoze, so i'll use the same code path for it
function findFileCI (var pathname: AnsiString; lastIsDir: Boolean=false): Boolean;


implementation

uses
  SysUtils;


function hasWadExtension (fn: AnsiString): Boolean;
begin
  fn := ExtractFileExt(fn);
  result := StrEquCI1251(fn, '.wad') or StrEquCI1251(fn, '.pk3') or StrEquCI1251(fn, '.zip');
end;


function addWadExtension (fn: AnsiString): AnsiString;
begin
  result := fn;
  if not hasWadExtension(result) then result := result+'.wad';
end;


function isWadPath (fn: AnsiString): Boolean;
var
  p: Integer;
  s: AnsiString;
begin
  result := false;
  while true do
  begin
    p := Pos(':', fn);
    if (p = 0) or (length(fn)-p < 1) then break;
    if (p-4 > 1) and (fn[p-4] = '.') and ((fn[p+1] = '\') or (fn[p+1] = '/')) then
    begin
      s := Copy(fn, p-4, 4);
      if StrEquCI1251(s, '.wad') or StrEquCI1251(s, '.pk3') or StrEquCI1251(s, '.zip') then
      begin
        result := true;
        exit;
      end;
    end;
    Delete(fn, 1, p);
  end;
end;


function Int64ToStrComma (i: Int64): AnsiString;
var
  f: Integer;
begin
  Str(i, result);
  f := Length(result)+1;
  while f > 4 do
  begin
    Dec(f, 3); Insert(',', result, f);
  end;
end;


function UpCase1251 (ch: Char): Char;
begin
  if ch < #128 then
  begin
    if (ch >= 'a') and (ch <= 'z') then Dec(ch, 32);
  end
  else
  begin
    if (ch >= #224) and (ch <= #255) then
    begin
      Dec(ch, 32);
    end
    else
    begin
      case ch of
        #184, #186, #191: Dec(ch, 16);
        #162, #179: Dec(ch);
      end;
    end;
  end;
  result := ch;
end;


function StrEquCI1251 (const s0, s1: AnsiString): Boolean;
var
  i: Integer;
begin
  result := false;
  if length(s0) <> length(s1) then exit;
  for i := 1 to length(s0) do if UpCase1251(s0[i]) <> UpCase1251(s1[i]) then exit;
  result := true;
end;


// ////////////////////////////////////////////////////////////////////////// //
// utils
// `ch`: utf8 start
// -1: invalid utf8
function utf8CodeLen (ch: Word): Integer;
begin
  if ch < $80 then begin result := 1; exit; end;
  if (ch and $FE) = $FC then begin result := 6; exit; end;
  if (ch and $FC) = $F8 then begin result := 5; exit; end;
  if (ch and $F8) = $F0 then begin result := 4; exit; end;
  if (ch and $F0) = $E0 then begin result := 3; exit; end;
  if (ch and $E0) = $C0 then begin result := 2; exit; end;
  result := -1; // invalid
end;


function utf8Valid (const s: AnsiString): Boolean;
var
  pos, len: Integer;
begin
  result := false;
  pos := 1;
  while pos <= length(s) do
  begin
    len := utf8CodeLen(Byte(s[pos]));
    if len < 1 then exit; // invalid sequence start
    if pos+len-1 > length(s) then exit; // out of chars in string
    Dec(len);
    Inc(pos);
    // check other sequence bytes
    while len > 0 do
    begin
      if (Byte(s[pos]) and $C0) <> $80 then exit;
      Dec(len);
      Inc(pos);
    end;
  end;
  result := true;
end;


// ////////////////////////////////////////////////////////////////////////// //
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


function decodeUtf8Char (s: AnsiString; var pos: Integer): char;
var
  b, c: Integer;
begin
  (* The following encodings are valid, except for the 5 and 6 byte
   * combinations:
   *  0xxxxxxx
   *  110xxxxx 10xxxxxx
   *  1110xxxx 10xxxxxx 10xxxxxx
   *  11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
   *  111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
   *  1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
   *)
  result := '?';
  if pos > length(s) then exit;

  b := Byte(s[pos]);
  Inc(pos);
  if b < $80 then begin result := char(b); exit; end;

  // mask out unused bits
       if (b and $FE) = $FC then b := b and $01
  else if (b and $FC) = $F8 then b := b and $03
  else if (b and $F8) = $F0 then b := b and $07
  else if (b and $F0) = $E0 then b := b and $0F
  else if (b and $E0) = $C0 then b := b and $1F
  else exit; // invalid utf8

  // now continue
  while pos <= length(s) do
  begin
    c := Byte(s[pos]);
    if (c and $C0) <> $80 then break; // no more
    b := b shl 6;
    b := b or (c and $3F);
    Inc(pos);
  end;

  // done, try 1251
  for c := 128 to 255 do if uni2wint[c] = b then begin result := char(c and $FF); exit; end;
  // alas
end;


function utf8to1251 (s: AnsiString): AnsiString;
var
  pos: Integer;
begin
  if not utf8Valid(s) then begin result := s; exit; end;
  pos := 1;
  while pos <= length(s) do
  begin
    if Byte(s[pos]) >= $80 then break;
    Inc(pos);
  end;
  if pos > length(s) then begin result := s; exit; end; // nothing to do here
  result := '';
  pos := 1;
  while pos <= length(s) do result := result+decodeUtf8Char(s, pos);
end;


// ////////////////////////////////////////////////////////////////////////// //
// `pathname` will be modified if path is valid
// `lastIsDir` should be `true` if we are searching for directory
// nobody cares about shitdoze, so i'll use the same code path for it
function findFileCI (var pathname: AnsiString; lastIsDir: Boolean=false): Boolean;
var
  sr: TSearchRec;
  npt: AnsiString;
  newname: AnsiString = '';
  curname: AnsiString;
  wantdir: Boolean;
  attr: LongInt;
  foundher: Boolean;
begin
  npt := pathname;
  result := (length(npt) > 0);
  if (length(npt) > 0) and ((npt[1] = '/') or (npt[1] = '\')) then newname := '/';
  while length(npt) > 0 do
  begin
    // remove trailing slashes
    while (length(npt) > 0) and ((npt[1] = '/') or (npt[1] = '\')) do Delete(npt, 1, 1);
    if length(npt) = 0 then break;
    // extract name
    curname := '';
    while (length(npt) > 0) and (npt[1] <> '/') and (npt[1] <> '\') do
    begin
      curname := curname+npt[1];
      Delete(npt, 1, 1);
    end;
    // remove trailing slashes again
    while (length(npt) > 0) and ((npt[1] = '/') or (npt[1] = '\')) do Delete(npt, 1, 1);
    wantdir := lastIsDir or (length(npt) > 0); // do we want directory here?
    //writeln(Format('npt=[%s]; newname=[%s]; curname=[%s]; wantdir=%d', [npt, newname, curname, Integer(wantdir)]));
    // try the easiest case first
    attr := FileGetAttr(newname+curname);
    if attr <> -1 then
    begin
      if wantdir = ((attr and faDirectory) <> 0) then
      begin
        // i found her!
        newname := newname+curname;
        if wantdir then newname := newname+'/';
        continue;
      end;
    end;
    //writeln(Format('npt=[%s]; newname=[%s]; curname=[%s]; wantdir=%d', [npt, newname, curname, Integer(wantdir)]));
    // alas, either not found, or invalid attributes
    foundher := false;
    try
      if FindFirst(newname+'*', faAnyFile, sr) = 0 then
      repeat
        if (wantdir = ((sr.attr and faDirectory) <> 0)) and StrEquCI1251(sr.name, curname) then
        begin
          // i found her!
          newname := newname+sr.name;
          if wantdir then newname := newname+'/';
          foundher := true;
          break;
        end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
    if not foundher then begin newname := ''; result := false; break; end;
  end;
  if result then pathname := newname;
end;


end.
