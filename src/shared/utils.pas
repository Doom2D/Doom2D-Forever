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
{$INCLUDE a_modes.inc}
unit utils;

interface

uses
  SysUtils, Classes, md5;


// ////////////////////////////////////////////////////////////////////////// //
type
  SSArray = array of ShortString;


const wadExtensions: array [0..6] of AnsiString = (
  '.dfz',
  '.wad',
  '.dfwad',
  '.pk3',
  '.pak',
  '.zip',
  '.dfzip'
);

{$IF DEFINED(FREEBSD) OR DEFINED(DARWIN)}
const NilThreadId = nil;
{$ELSE}
const NilThreadId = 0;
{$ENDIF}


// ////////////////////////////////////////////////////////////////////////// //
type
  TUtf8DecoderFast = packed record
  public
    const Replacement = $FFFD; // replacement char for invalid unicode
    const Accept = 0;
    const Reject = 12;

  private
    state: LongWord;

  public
    codepoint: LongWord; // decoded codepoint (valid only when decoder is in "complete" state)

  public
    constructor Create (v: Boolean{fuck you, fpc});

    procedure reset (); inline;

    function complete (): Boolean; inline; // is current character complete? take `codepoint` then
    function invalid (): Boolean; inline;
    function completeOrInvalid (): Boolean; inline;

    // process one byte, return `true` if codepoint is ready
    function decode (b: Byte): Boolean; inline; overload;
    function decode (c: AnsiChar): Boolean; inline; overload;
  end;


// ////////////////////////////////////////////////////////////////////////// //
function getFilenameExt (const fn: AnsiString): AnsiString;
function setFilenameExt (const fn, ext: AnsiString): AnsiString;
function forceFilenameExt (const fn, ext: AnsiString): AnsiString;

// rewrites slashes to '/'
function fixSlashes (s: AnsiString): AnsiString;

// replaces all the shitty characters with '_'
// (everything except alphanumerics, '_', '.')
function sanitizeFilename (s: AnsiString): AnsiString;

function isAbsolutePath (const s: AnsiString): Boolean;
function isRootPath (const s: AnsiString): Boolean;

// strips out name from `fn`, leaving trailing slash
function getFilenamePath (const fn: AnsiString): AnsiString;

// ends with '/' or '\'?
function isFilenamePath (const fn: AnsiString): Boolean;

// strips extra trailing slashes in `path, and extra leading slashes in `fn`
// will add slash to `path`, even if `fn` is empty!
function filenameConcat (const path, fn: AnsiString): AnsiString;

// does filename have one of ".wad", ".pk3", ".zip" extensions?
function hasWadExtension (const fn: AnsiString): Boolean;

// does filepath have ".XXX:\" in it?
function isWadPath (const fn: AnsiString): Boolean;

// adds ".wad" extension if filename doesn't have one of ".wad", ".pk3", ".zip"
function addWadExtension (const fn: AnsiString): AnsiString;

// check wad signature
function isWadData (data: Pointer; len: LongWord): Boolean;

// convert number to strig with nice commas
function int64ToStrComma (i: Int64): AnsiString;

function upcase1251 (ch: AnsiChar): AnsiChar; inline;
function locase1251 (ch: AnsiChar): AnsiChar; inline;
function IsValid1251 (ch: Word): Boolean;
function IsPrintable1251 (ch: AnsiChar): Boolean;

function toLowerCase1251 (const s: AnsiString): AnsiString;

// `true` if strings are equal; ignoring case for cp1251
function strEquCI1251 (const s0, s1: AnsiString): Boolean;

function utf8Valid (const s: AnsiString): Boolean;

function utf8to1251 (s: AnsiString): AnsiString;

// findFileCI takes case-insensitive path, traverses it, and rewrites it to
// a case-sensetive one (using real on-disk names). return value means 'success'.
// if some dir or file wasn't found, pathname is undefined (destroyed, but not
// necessarily cleared).
// last name assumed to be a file, not directory (unless `lastIsDir` flag is set).
function findFileCI (var pathname: AnsiString; lastIsDir: Boolean=false): Boolean;

// findDiskWad tries to find the wad file using common wad extensions
// (see `wadExtensions` array).
// returns real on-disk filename, or empty string.
// original wad extension is used as a hint for the first try.
// also, this automatically performs `findFileCI()`.
function findDiskWad (fname: AnsiString): AnsiString;
// slashes must be normalized!
function isWadNamesEqu (wna, wnb: AnsiString): Boolean;

// they throws
function openDiskFileRO (pathname: AnsiString): TStream;
function createDiskFile (pathname: AnsiString): TStream;
// create file if necessary, but don't truncate the existing one
function openDiskFileRW (pathname: AnsiString): TStream;

// little endian
procedure writeSign (st: TStream; const sign: AnsiString);
function checkSign (st: TStream; const sign: AnsiString): Boolean;

procedure writeBool (st: TStream; b: Boolean);
function readBool (st: TStream): Boolean;

procedure writeStr (st: TStream; const str: AnsiString; maxlen: LongWord=65535);
function readStr (st: TStream; maxlen: LongWord=65535): AnsiString;

procedure writeInt (st: TStream; v: Byte); overload;
procedure writeInt (st: TStream; v: ShortInt); overload;
procedure writeInt (st: TStream; v: Word); overload;
procedure writeInt (st: TStream; v: SmallInt); overload;
procedure writeInt (st: TStream; v: LongWord); overload;
procedure writeInt (st: TStream; v: LongInt); overload;
procedure writeInt (st: TStream; v: Int64); overload;
procedure writeInt (st: TStream; v: UInt64); overload;

function readByte (st: TStream): Byte;
function readShortInt (st: TStream): ShortInt;
function readWord (st: TStream): Word;
function readSmallInt (st: TStream): SmallInt;
function readLongWord (st: TStream): LongWord;
function readLongInt (st: TStream): LongInt;
function readInt64 (st: TStream): Int64;
function readUInt64 (st: TStream): UInt64;

// big endian
procedure writeIntBE (st: TStream; v: Byte); overload;
procedure writeIntBE (st: TStream; v: ShortInt); overload;
procedure writeIntBE (st: TStream; v: Word); overload;
procedure writeIntBE (st: TStream; v: SmallInt); overload;
procedure writeIntBE (st: TStream; v: LongWord); overload;
procedure writeIntBE (st: TStream; v: LongInt); overload;
procedure writeIntBE (st: TStream; v: Int64); overload;
procedure writeIntBE (st: TStream; v: UInt64); overload;

function readByteBE (st: TStream): Byte;
function readShortIntBE (st: TStream): ShortInt;
function readWordBE (st: TStream): Word;
function readSmallIntBE (st: TStream): SmallInt;
function readLongWordBE (st: TStream): LongWord;
function readLongIntBE (st: TStream): LongInt;
function readInt64BE (st: TStream): Int64;
function readUInt64BE (st: TStream): UInt64;

function nlerp (a, b: Integer; t: Single): Integer; inline;

function nmin (a, b: Byte): Byte; inline; overload;
function nmin (a, b: ShortInt): ShortInt; inline; overload;
function nmin (a, b: Word): Word; inline; overload;
function nmin (a, b: SmallInt): SmallInt; inline; overload;
function nmin (a, b: LongWord): LongWord; inline; overload;
function nmin (a, b: LongInt): LongInt; inline; overload;
function nmin (a, b: Int64): Int64; inline; overload;
function nmin (a, b: UInt64): UInt64; inline; overload;
function nmin (a, b: Single): Single; inline; overload;
function nmin (a, b: Double): Double; inline; overload;
{$IFDEF FPC_HAS_TYPE_EXTENDED}
function nmin (a, b: Extended): Extended; inline; overload;
{$ENDIF}

function nmax (a, b: Byte): Byte; inline; overload;
function nmax (a, b: ShortInt): ShortInt; inline; overload;
function nmax (a, b: Word): Word; inline; overload;
function nmax (a, b: SmallInt): SmallInt; inline; overload;
function nmax (a, b: LongWord): LongWord; inline; overload;
function nmax (a, b: LongInt): LongInt; inline; overload;
function nmax (a, b: Int64): Int64; inline; overload;
function nmax (a, b: UInt64): UInt64; inline; overload;
function nmax (a, b: Single): Single; inline; overload;
function nmax (a, b: Double): Double; inline; overload;
{$IFDEF FPC_HAS_TYPE_EXTENDED}
function nmax (a, b: Extended): Extended; inline; overload;
{$ENDIF}
function nclamp (v, a, b: Byte): Byte; inline; overload;
function nclamp (v, a, b: ShortInt): ShortInt; inline; overload;
function nclamp (v, a, b: Word): Word; inline; overload;
function nclamp (v, a, b: SmallInt): SmallInt; inline; overload;
function nclamp (v, a, b: LongWord): LongWord; inline; overload;
function nclamp (v, a, b: LongInt): LongInt; inline; overload;
function nclamp (v, a, b: Int64): Int64; inline; overload;
function nclamp (v, a, b: UInt64): UInt64; inline; overload;
function nclamp (v, a, b: Single): Single; inline; overload;
function nclamp (v, a, b: Double): Double; inline; overload;
{$IFDEF FPC_HAS_TYPE_EXTENDED}
function nclamp (v, a, b: Extended): Extended; inline; overload;
{$ENDIF}

type
  TFormatStrFCallback = procedure (constref buf; len: SizeUInt);

// returns formatted string if `writerCB` is `nil`, empty string otherwise
function formatstrf (const fmt: AnsiString; const args: array of const; writerCB: TFormatStrFCallback=nil): AnsiString;

function wchar2win (wc: WideChar): AnsiChar; inline;
function utf2win (const s: AnsiString): AnsiString;
function win2utf (const s: AnsiString): AnsiString;
function digitInBase (ch: AnsiChar; base: Integer): Integer;

// returns string in single or double quotes
// single quotes supports only pascal-style '' for single quote char
// double quotes supports c-style escapes
// function will select quote mode automatically
function quoteStr (const s: AnsiString): AnsiString;
// separate single-quote and double-quote escape functions
function squoteStr (const s: AnsiString): AnsiString;
function dquoteStr (const s: AnsiString): AnsiString;


type
  generic TSimpleList<ItemT> = class
  private
    //type PItemT = ^ItemT;
    type TItemArr = array of ItemT;

  public
    type
      TEnumerator = record
      private
        mItems: TItemArr;
        mCount: Integer;
        mCurrent: Integer;
      public
        constructor Create (const aitems: TItemArr; acount: Integer);
        function MoveNext: Boolean;
        function getCurrent (): ItemT;
        property Current: ItemT read getCurrent;
      end;

  private
    mItems: TItemArr;
    mCount: Integer; // can be less than `mItems` size

  private
    function getAt (idx: Integer): ItemT; inline;
    procedure setAt (idx: Integer; const it: ItemT); inline;

    function getCapacity (): Integer; inline;
    procedure setCapacity (v: Integer); inline;

  public
    constructor Create (acapacity: Integer=-1);
    destructor Destroy (); override;

    //WARNING! don't change list contents in `for ... in`!
    function GetEnumerator (): TEnumerator;

    procedure reset (); inline; // won't resize `mItems`
    procedure clear (); inline;

    procedure append (constref it: ItemT); inline;
    procedure delete (idx: Integer); inline;
    function remove (idx: Integer): ItemT; inline;

  public
    property count: Integer read mCount;
    property capacity: Integer read getCapacity write setCapacity;
    property at[idx: Integer]: ItemT read getAt write setAt; default;
  end;


procedure FillMemory (Dest: Pointer; Len: LongWord; Ch: Byte); inline;
procedure CopyMemory (Dest: Pointer; Src: Pointer; Len: LongWord); inline;
procedure ZeroMemory (Dest: Pointer; Len: LongWord); inline;


type
  TDiskFileInfo = record
    diskName: AnsiString;
    size: LongInt;
    age: LongInt;
    // not changed by info getter; used in other parts of the code
    userName: AnsiString;
    tag: Integer;
    hash: TMD5Digest;
    udata: Pointer;
  end;

function GetDiskFileInfo (fname: AnsiString; var info: TDiskFileInfo): Boolean;


implementation

uses
  xstreams;

// ////////////////////////////////////////////////////////////////////////// //
procedure CopyMemory (Dest: Pointer; Src: Pointer; Len: LongWord); inline;
begin
  Move(Src^, Dest^, Len);
end;

procedure FillMemory (Dest: Pointer; Len: LongWord; Ch: Byte); inline;
begin
  FillChar(Dest^, Len, Ch);
end;

procedure ZeroMemory (Dest: Pointer; Len: LongWord); inline;
begin
  FillChar(Dest^, Len, 0);
end;


// ////////////////////////////////////////////////////////////////////////// //
// rewrites slashes to '/'
function fixSlashes (s: AnsiString): AnsiString;
{$IFDEF WINDOWS}
var
  f: Integer;
{$ENDIF}
begin
  result := s;
  {$IFDEF WINDOWS}
  for f := 1 to length(result) do if (result[f] = '\') then result[f] := '/';
  {$ENDIF}
end;

// replaces all the shitty characters with '_'
// (everything except alphanumerics, '_', '.')
function sanitizeFilename (s: AnsiString): AnsiString;
var
  i: Integer;
const
  leaveChars: set of Char = [ '0'..'9', 'A'..'Z', 'a'..'z', '_', '.', #192..#255 ];
  replaceWith: Char = '_';
begin
  result := s;
  for i := 1 to length(result) do
    if not (result[i] in leaveChars) then
      result[i] := replaceWith;
end;

function isAbsolutePath (const s: AnsiString): Boolean;
begin
  result := false;
  if (length(s) = 0) then exit;
  {$IFDEF WINDOWS}
  if (s[1] = '/') or (s[1] = '\') then begin result := true; exit; end;
  if (length(s) > 2) and (s[2] = ':') and ((s[3] = '/') or (s[3] = '\')) then begin result := true; exit; end;
  {$ELSE}
  result := (s[1] = '/');
  {$ENDIF}
end;


function isRootPath (const s: AnsiString): Boolean;
begin
  result := false;
  if (length(s) = 0) then exit;
  {$IFDEF WINDOWS}
  if (s = '/') or (s = '\') then begin result := true; exit; end;
  if (length(s) = 3) and (s[2] = ':') and ((s[3] = '/') or (s[3] = '\')) then begin result := true; exit; end;
  {$ELSE}
  result := (s = '/');
  {$ENDIF}
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TSimpleList.TEnumerator.Create (const aitems: TItemArr; acount: Integer);
begin
  mItems := aitems;
  mCurrent := -1;
  mCount := acount;
end;

function TSimpleList.TEnumerator.MoveNext: Boolean;
begin
  Inc(mCurrent);
  result := (mCurrent < mCount);
end;

function TSimpleList.TEnumerator.getCurrent (): ItemT;
begin
  result := mItems[mCurrent];
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TSimpleList.Create (acapacity: Integer=-1);
begin
  mItems := nil;
  if (acapacity > 0) then SetLength(mItems, acapacity);
  mCount := 0;
end;


destructor TSimpleList.Destroy ();
begin
  mItems := nil;
  inherited;
end;


function TSimpleList.getCapacity (): Integer; inline;
begin
  result := Length(mItems);
end;


procedure TSimpleList.setCapacity (v: Integer); inline;
begin
  if (v < mCount) then v := mCount;
  if (v <> Length(mItems)) then SetLength(mItems, v);
end;


function TSimpleList.GetEnumerator (): TEnumerator;
begin
  if (Length(mItems) > 0) then result := TEnumerator.Create(mItems, mCount)
  else result := TEnumerator.Create(nil, -1);
end;


procedure TSimpleList.reset (); inline;
begin
  mCount := 0;
end;


procedure TSimpleList.clear (); inline;
begin
  mItems := nil;
  mCount := 0;
end;


function TSimpleList.getAt (idx: Integer): ItemT; inline;
begin
  if (idx >= 0) and (idx < mCount) then result := mItems[idx] else result := Default(ItemT);
end;


procedure TSimpleList.setAt (idx: Integer; const it: ItemT); inline;
begin
  if (idx >= 0) and (idx < mCount) then mItems[idx] := it;
end;


procedure TSimpleList.append (constref it: ItemT); inline;
var
  newsz: Integer;
begin
  if (mCount >= Length(mItems)) then
  begin
    newsz := mCount+(mCount div 3)+128;
    SetLength(mItems, newsz);
  end;
  mItems[mCount] := it;
  Inc(mCount);
end;


procedure TSimpleList.delete (idx: Integer); inline;
var
  f: Integer;
begin
  if (idx >= 0) and (idx < mCount) then
  begin
    for f := idx+1 to mCount-1 do mItems[f-1] := mItems[f];
  end;
end;


function TSimpleList.remove (idx: Integer): ItemT; inline;
var
  f: Integer;
begin
  if (idx >= 0) and (idx < mCount) then
  begin
    result := mItems[idx];
    for f := idx+1 to mCount-1 do mItems[f-1] := mItems[f];
  end
  else
  begin
    result := Default(ItemT);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
var
  wc2shitmap: array[0..65535] of AnsiChar;
  wc2shitmapInited: Boolean = false;


// ////////////////////////////////////////////////////////////////////////// //
const
  cp1251: array[0..127] of Word = (
    $0402,$0403,$201A,$0453,$201E,$2026,$2020,$2021,$20AC,$2030,$0409,$2039,$040A,$040C,$040B,$040F,
    $0452,$2018,$2019,$201C,$201D,$2022,$2013,$2014,$FFFD,$2122,$0459,$203A,$045A,$045C,$045B,$045F,
    $00A0,$040E,$045E,$0408,$00A4,$0490,$00A6,$00A7,$0401,$00A9,$0404,$00AB,$00AC,$00AD,$00AE,$0407,
    $00B0,$00B1,$0406,$0456,$0491,$00B5,$00B6,$00B7,$0451,$2116,$0454,$00BB,$0458,$0405,$0455,$0457,
    $0410,$0411,$0412,$0413,$0414,$0415,$0416,$0417,$0418,$0419,$041A,$041B,$041C,$041D,$041E,$041F,
    $0420,$0421,$0422,$0423,$0424,$0425,$0426,$0427,$0428,$0429,$042A,$042B,$042C,$042D,$042E,$042F,
    $0430,$0431,$0432,$0433,$0434,$0435,$0436,$0437,$0438,$0439,$043A,$043B,$043C,$043D,$043E,$043F,
    $0440,$0441,$0442,$0443,$0444,$0445,$0446,$0447,$0448,$0449,$044A,$044B,$044C,$044D,$044E,$044F
  );


procedure initShitMap ();
var
  f: Integer;
begin
  for f := 0 to High(wc2shitmap) do wc2shitmap[f] := '?';
  for f := 0 to 127 do wc2shitmap[f] := AnsiChar(f);
  for f := 0 to 127 do wc2shitmap[cp1251[f]] := AnsiChar(f+128);
  wc2shitmapInited := true;
end;


// ////////////////////////////////////////////////////////////////////////// //
// fast state-machine based UTF-8 decoder; using 8 bytes of memory
// code points from invalid range will never be valid, this is the property of the state machine
const
  // see http://bjoern.hoehrmann.de/utf-8/decoder/dfa/
  utf8dfa: array[0..$16c-1] of Byte = (
    // maps bytes to character classes
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00, // 00-0f
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00, // 10-1f
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00, // 20-2f
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00, // 30-3f
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00, // 40-4f
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00, // 50-5f
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00, // 60-6f
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00, // 70-7f
    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01, // 80-8f
    $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09, // 90-9f
    $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07, // a0-af
    $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07, // b0-bf
    $08,$08,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02, // c0-cf
    $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02, // d0-df
    $0a,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$04,$03,$03, // e0-ef
    $0b,$06,$06,$06,$05,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08, // f0-ff
    // maps a combination of a state of the automaton and a character class to a state
    $00,$0c,$18,$24,$3c,$60,$54,$0c,$0c,$0c,$30,$48,$0c,$0c,$0c,$0c, // 100-10f
    $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$00,$0c,$0c,$0c,$0c,$0c,$00, // 110-11f
    $0c,$00,$0c,$0c,$0c,$18,$0c,$0c,$0c,$0c,$0c,$18,$0c,$18,$0c,$0c, // 120-12f
    $0c,$0c,$0c,$0c,$0c,$0c,$0c,$18,$0c,$0c,$0c,$0c,$0c,$18,$0c,$0c, // 130-13f
    $0c,$0c,$0c,$0c,$0c,$18,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$24, // 140-14f
    $0c,$24,$0c,$0c,$0c,$24,$0c,$0c,$0c,$0c,$0c,$24,$0c,$24,$0c,$0c, // 150-15f
    $0c,$24,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c);


// ////////////////////////////////////////////////////////////////////////// //
constructor TUtf8DecoderFast.Create (v: Boolean{fuck you, fpc}); begin state := Accept; codepoint := 0; end;

procedure TUtf8DecoderFast.reset (); inline; begin state := Accept; codepoint := 0; end;

function TUtf8DecoderFast.complete (): Boolean; inline; begin result := (state = Accept); end;
function TUtf8DecoderFast.invalid (): Boolean; inline; begin result := (state = Reject); end;
function TUtf8DecoderFast.completeOrInvalid (): Boolean; inline; begin result := (state = Accept) or (state = Reject); end;

function TUtf8DecoderFast.decode (c: AnsiChar): Boolean; inline; overload; begin result := decode(Byte(c)); end;

function TUtf8DecoderFast.decode (b: Byte): Boolean; inline; overload;
var
  tp: LongWord;
begin
  if (state = Reject) then begin state := Accept; codepoint := 0; end;
  tp := utf8dfa[b];
  if (state <> Accept) then codepoint := (b and $3f) or (codepoint shl 6) else codepoint := ($ff shr tp) and b;
  state := utf8dfa[256+state+tp];
  if (state = Reject) then begin codepoint := Replacement; state := Accept; end;
  result := (state = Accept);
end;


// ////////////////////////////////////////////////////////////////////////// //
function wchar2win (wc: WideChar): AnsiChar; inline;
begin
  if not wc2shitmapInited then initShitMap();
  if (LongWord(wc) > 65535) then result := '?' else result := wc2shitmap[LongWord(wc)];
end;


// ////////////////////////////////////////////////////////////////////////// //
function utf2win (const s: AnsiString): AnsiString;
var
  f, c: Integer;
  ud: TUtf8DecoderFast;
begin
  for f := 1 to Length(s) do
  begin
    if (Byte(s[f]) > 127) then
    begin
      ud := TUtf8DecoderFast.Create(true);
      result := '';
      for c := 1 to Length(s) do
      begin
        if ud.decode(s[c]) then result += wchar2win(WideChar(ud.codepoint));
      end;
      exit;
    end;
  end;
  result := s;
end;


function win2utf (const s: AnsiString): AnsiString;
var
  f, c: Integer;

  function utf8Encode (code: Integer): AnsiString;
  begin
    if (code < 0) or (code > $10FFFF) then begin result := '?'; exit; end;
    if (code <= $7f) then
    begin
      result := AnsiChar(code and $ff);
    end
    else if (code <= $7FF) then
    begin
      result := AnsiChar($C0 or (code shr 6));
      result += AnsiChar($80 or (code and $3F));
    end
    else if (code <= $FFFF) then
    begin
      result := AnsiChar($E0 or (code shr 12));
      result += AnsiChar($80 or ((code shr 6) and $3F));
      result += AnsiChar($80 or (code and $3F));
    end
    else if (code <= $10FFFF) then
    begin
      result := AnsiChar($F0 or (code shr 18));
      result += AnsiChar($80 or ((code shr 12) and $3F));
      result += AnsiChar($80 or ((code shr 6) and $3F));
      result += AnsiChar($80 or (code and $3F));
    end
    else
    begin
      result := '?';
    end;
  end;

begin
  for f := 1 to Length(s) do
  begin
    if (Byte(s[f]) > 127) then
    begin
      result := '';
      for c := 1 to Length(s) do
      begin
        if (Byte(s[c]) < 128) then
        begin
          result += s[c];
        end
        else
        begin
          result += utf8Encode(cp1251[Byte(s[c])-128])
        end;
      end;
      exit;
    end;
  end;
  result := s;
end;


// ////////////////////////////////////////////////////////////////////////// //
function digitInBase (ch: AnsiChar; base: Integer): Integer;
begin
  result := -1;
  if (base < 1) or (base > 36) then exit;
  if (ch < '0') then exit;
  if (base <= 10) then
  begin
    if (Integer(ch) >= 48+base) then exit;
    result := Integer(ch)-48;
  end
  else
  begin
    if (ch >= '0') and (ch <= '9') then begin result := Integer(ch)-48; exit; end;
    if (ch >= 'a') and (ch <= 'z') then Dec(ch, 32); // poor man's tolower()
    if (ch < 'A') or (Integer(ch) >= 65+(base-10)) then exit;
    result := Integer(ch)-65+10;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function squoteStr (const s: AnsiString): AnsiString;
var
  f: Integer;
begin
  result := '''';
  for f := 1 to Length(s) do
  begin
    if (s[f] = '''') then result += '''';
    result += s[f];
  end;
  result += '''';
end;

function dquoteStr (const s: AnsiString): AnsiString;
var
  f: Integer;
  ch: AnsiChar;
begin
  result := '"';
  for f := 1 to Length(s) do
  begin
    ch := s[f];
         if (ch = #0) then result += '\z'
    else if (ch = #9) then result += '\t'
    else if (ch = #10) then result += '\n'
    else if (ch = #13) then result += '\r'
    else if (ch = #27) then result += '\e'
    else if (ch < ' ') or (ch = #127) then
    begin
      result += '\x';
      result += LowerCase(IntToHex(Integer(ch), 2));
    end
    else if (ch = '"') or (ch = '\') then
    begin
      result += '\';
      result += ch;
    end
    else
    begin
      result += ch;
    end;
  end;
  result += '"';
end;

function quoteStr (const s: AnsiString): AnsiString;
var
  needSingle: Boolean = false;
  f: Integer;
begin
  for f := 1 to Length(s) do
  begin
    if (s[f] = '''') then begin needSingle := true; continue; end;
    if (s[f] < ' ') or (s[f] = #127) then begin result := dquoteStr(s); exit; end;
  end;
  if needSingle then result := squoteStr(s) else result := ''''+s+'''';
end;


// ////////////////////////////////////////////////////////////////////////// //
function getFilenameExt (const fn: AnsiString): AnsiString;
var
  pos: Integer;
  ch: AnsiChar;
begin
  pos := Length(fn);
  while (pos > 0) do
  begin
    ch := fn[pos];
    if (ch = '.') then
    begin
      if (pos = Length(fn)) then result := '' else result := Copy(fn, pos, Length(fn)-pos+1);
      exit;
    end;
    if (ch = '/') or (ch = '\') then break;
    Dec(pos);
  end;
  result := ''; // no extension
end;


function setFilenameExt (const fn, ext: AnsiString): AnsiString;
var
  pos: Integer;
  ch: AnsiChar;
begin
  result := fn;
  if (Length(ext) = 0) or (ext = '.') then exit;
  pos := Length(fn);
  while (pos > 0) do
  begin
    ch := fn[pos];
    if (ch = '.') then exit;
    if (ch = '/') or (ch = '\') then break;
    Dec(pos);
  end;
  if (ext[1] <> '.') then result += '.';
  result += ext;
end;


function forceFilenameExt (const fn, ext: AnsiString): AnsiString;
var
  pos: Integer;
  ch: AnsiChar;
begin
  result := fn;
  pos := Length(fn);
  while (pos > 0) do
  begin
    ch := fn[pos];
    if (ch = '.') then
    begin
      if (Length(ext) = 0) or (ext = '.') then
      begin
        result := Copy(fn, 1, pos-1);
      end
      else
      begin
        if (ext[1] = '.') then result := Copy(fn, 1, pos-1) else result := Copy(fn, 1, pos);
        result += ext;
        exit;
      end;
    end;
    if (ch = '/') or (ch = '\') then break;
    Dec(pos);
  end;
  if (Length(ext) > 0) then
  begin
    if (ext[1] <> '.') then result += '.';
    result += ext;
  end;
end;


// strips out name from `fn`, leaving trailing slash
function getFilenamePath (const fn: AnsiString): AnsiString;
var
  pos: Integer;
  ch: AnsiChar;
begin
  if (Length(fn) = 0) then begin result := './'; exit; end;
  if (fn[Length(fn)] = '/') or (fn[Length(fn)] = '\') then begin result := fn; exit; end;
  pos := Length(fn);
  while (pos > 0) do
  begin
    ch := fn[pos];
    if (ch = '/') or (ch = '\') then begin result := Copy(fn, 1, pos); exit; end;
    Dec(pos);
  end;
  result := './'; // no path -> current dir
end;


// ends with '/' or '\'?
function isFilenamePath (const fn: AnsiString): Boolean;
begin
  if (Length(fn) = 0) then
  begin
    result := false;
  end
  else
  begin
    result := (fn[Length(fn)] = '/') or (fn[Length(fn)] = '\');
  end;
end;


// strips extra trailing slashes in `path, and extra leading slashes in `fn`
// will add slash to `path`, even if `fn` is empty!
function filenameConcat (const path, fn: AnsiString): AnsiString;
var
  pos: Integer;
begin
  pos := 1;
  while (pos <= Length(fn)) and ((fn[pos] = '/') or (fn[pos] = '\')) do Inc(pos);
  result := path;
  if (Length(result) > 0) and ((result[Length(result)] <> '/') and (result[Length(result)] <> '\')) then result += '/';
  if (pos <= Length(fn)) then
  begin
    result += Copy(fn, pos, Length(fn)-pos+1);
    //FIXME: make this faster!
    while (Length(result) > 0) and ((result[Length(result)] = '/') or (result[Length(result)] = '\')) do
    begin
      Delete(result, Length(result), 1);
    end;
    if (fn[Length(fn)] = '/') or (fn[Length(fn)] = '\') then result += '/';
  end;
end;


function hasWadExtension (const fn: AnsiString): Boolean;
var
  ext, newExt: AnsiString;
begin
  ext := getFilenameExt(fn);
  result := true;
  for newExt in wadExtensions do if (StrEquCI1251(ext, newExt)) then exit;
  result := false;
  //result := StrEquCI1251(ext, '.wad') or StrEquCI1251(ext, '.pk3') or StrEquCI1251(ext, '.zip') or StrEquCI1251(ext, '.dfz');
end;


function addWadExtension (const fn: AnsiString): AnsiString;
begin
  result := fn;
  if not hasWadExtension(result) then result := result+'.wad';
end;


function isWadData (data: Pointer; len: LongWord): Boolean;
  var p: PChar;
begin
  p := PChar(data);
  Result :=
    (* ZIP *)
    ((len > 3) and (p[0] = 'P') and (p[1] = 'K') and (p[2] = #03) and (p[3] = #04)) or
    ((len > 3) and (p[0] = 'P') and (p[1] = 'K') and (p[2] = #05) and (p[3] = #06)) or
    (* PACK *)
    ((len > 3) and (p[0] = 'P') and (p[1] = 'A') and (p[2] = 'C') and (p[3] = 'K')) or
    ((len > 3) and (p[0] = 'S') and (p[1] = 'P') and (p[2] = 'A') and (p[3] = 'K')) or
    (* DFWAD *)
    ((len > 5) and (p[0] = 'D') and (p[1] = 'F') and (p[2] = 'W') and (p[3] = 'A') and (p[4] = 'D') and (p[5] = #01))
end;


function isWadPath (const fn: AnsiString): Boolean;
var
  pos: Integer;
  s, wext: AnsiString;
begin
  result := false;
  pos := 1;
  while (pos <= Length(fn)) do
  begin
    if (fn[pos] = ':') then
    begin
      if (Length(fn)-pos < 1) then break;
      if (pos-4 > 1) and (fn[pos-4] = '.') and ((fn[pos+1] = '\') or (fn[pos+1] = '/')) then
      begin
        s := Copy(fn, pos-4, 4);
        for wext in wadExtensions do
        begin
          if strEquCI1251(s, wext) then
          begin
            result := true;
            exit;
          end;
        end;
      end;
    end;
    Inc(pos);
  end;
end;


function int64ToStrComma (i: Int64): AnsiString;
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


function upcase1251 (ch: AnsiChar): AnsiChar; inline;
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


function locase1251 (ch: AnsiChar): AnsiChar; inline;
begin
  if ch < #128 then
  begin
    if (ch >= 'A') and (ch <= 'Z') then Inc(ch, 32);
  end
  else
  begin
    if (ch >= #192) and (ch <= #223) then
    begin
      Inc(ch, 32);
    end
    else
    begin
      case ch of
        #168, #170, #175: Inc(ch, 16);
        #161, #178: Inc(ch);
      end;
    end;
  end;
  result := ch;
end;

function IsValid1251 (ch: Word): Boolean;
begin
  result := ((ch = Ord('?')) or (wc2shitmap[ch] <> '?')) and (wc2shitmap[ch] <> #$98)
end;

function IsPrintable1251 (ch: AnsiChar): Boolean;
begin
  result := (ch >= #32) and (ch <> #127) and (ch <> #$98)
end;


function strEquCI1251 (const s0, s1: AnsiString): Boolean;
var
  i: Integer;
begin
  result := false;
  if length(s0) <> length(s1) then exit;
  for i := 1 to length(s0) do if UpCase1251(s0[i]) <> UpCase1251(s1[i]) then exit;
  result := true;
end;


function toLowerCase1251 (const s: AnsiString): AnsiString;
var
  f: Integer;
  ch: AnsiChar;
begin
  for ch in s do
  begin
    if (ch <> LoCase1251(ch)) then
    begin
      result := '';
      SetLength(result, Length(s));
      for f := 1 to Length(s) do result[f] := LoCase1251(s[f]);
      exit;
    end;
  end;
  // nothing to do
  result := s;
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


function decodeUtf8Char (s: AnsiString; var pos: Integer): AnsiChar;
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
  if b < $80 then begin result := AnsiChar(b); exit; end;

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
  for c := 128 to 255 do if uni2wint[c] = b then begin result := AnsiChar(c and $FF); exit; end;
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
// findFileCI eats case-insensitive path, traverses it and rewrites it to a
// case-sensetive. result value means success.
// if file/dir not founded than pathname is in undefined state!
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


function isWadNamesEqu (wna, wnb: AnsiString): Boolean;
var
  ext, newExt: AnsiString;
  found: Boolean;
begin
  result := StrEquCI1251(wna, wnb);
  if result then exit;
  // check first ext
  ext := getFilenameExt(wna);
  found := false;
  for newExt in wadExtensions do if (StrEquCI1251(ext, newExt)) then begin found := true; break; end;
  if not found then exit;
  // check second ext
  ext := getFilenameExt(wnb);
  found := false;
  for newExt in wadExtensions do if (StrEquCI1251(ext, newExt)) then begin found := true; break; end;
  if not found then exit;
  wna := forceFilenameExt(wna, '');
  wnb := forceFilenameExt(wnb, '');
  result := StrEquCI1251(wna, wnb);
end;

function findDiskWad (fname: AnsiString): AnsiString;
var
  origExt: AnsiString = '';
  newExt: AnsiString = '';
begin
  result := '';
  //writeln('findDiskWad00: fname=<', fname, '>');
  if (findFileCI(fname)) then begin result := fname; exit; end;
  origExt := getFilenameExt(fname);
  fname := forceFilenameExt(fname, '');
  //writeln(' findDiskWad01: fname=<', fname, '>; origExt=<', origExt, '>');
  for newExt in wadExtensions do
  begin
    //writeln(' findDiskWad02: fname=<', fname, '>; origExt=<', origExt, '>; newExt=<', newExt, '>');
    if (StrEquCI1251(newExt, origExt)) then
    begin
      //writeln('   SKIP');
      continue;
    end;
    result := fname+newExt;
    if (findFileCI(result)) then exit;
  end;
  result := '';
end;


function openDiskFileRO (pathname: AnsiString): TStream;
begin
  if not findFileCI(pathname) then raise EFileNotFoundException.Create('can''t open file "'+pathname+'"');
  result := TFileStream.Create(pathname, fmOpenRead or {fmShareDenyWrite}fmShareDenyNone);
end;

function createDiskFile (pathname: AnsiString): TStream;
var
  path: AnsiString;
begin
  path := ExtractFilePath(pathname);
  if length(path) > 0 then
  begin
    if not findFileCI(path, true) then raise Exception.Create('can''t create file "'+pathname+'"');
  end;
  result := TFileStream.Create(path+ExtractFileName(pathname), fmCreate);
end;


function openDiskFileRW (pathname: AnsiString): TStream;
var
  path: AnsiString;
  oldname: AnsiString;
begin
  //writeln('*** TRYING R/W FILE "', pathname, '"');
  path := ExtractFilePath(pathname);
  if length(path) > 0 then
  begin
    if not findFileCI(path, true) then raise Exception.Create('can''t create file "'+pathname+'"');
  end;
  oldname := pathname;
  if findFileCI(oldname) then
  begin
    //writeln('*** found old file "', oldname, '"');
    result := TFileStream.Create(oldname, fmOpenReadWrite or fmShareDenyWrite);
  end
  else
  begin
    result := TFileStream.Create(path+ExtractFileName(pathname), fmCreate);
  end;
end;


procedure writeIntegerLE (st: TStream; vp: Pointer; size: Integer);
{$IFDEF ENDIAN_LITTLE}
begin
  st.writeBuffer(vp^, size);
end;
{$ELSE}
var
  p: PByte;
begin
  p := PByte(vp)+size-1;
  while size > 0 do
  begin
    st.writeBuffer(p^, 1);
    Dec(size);
    Dec(p);
  end;
end;
{$ENDIF}

procedure writeIntegerBE (st: TStream; vp: Pointer; size: Integer);
{$IFDEF ENDIAN_LITTLE}
var
  p: PByte;
begin
  p := PByte(vp)+size-1;
  while size > 0 do
  begin
    st.writeBuffer(p^, 1);
    Dec(size);
    Dec(p);
  end;
end;
{$ELSE}
begin
  st.writeBuffer(vp^, size);
end;
{$ENDIF}

procedure writeSign (st: TStream; const sign: AnsiString);
begin
  if (Length(sign) > 0) then st.WriteBuffer(sign[1], Length(sign));
end;

function checkSign (st: TStream; const sign: AnsiString): Boolean;
var
  buf: packed array[0..7] of AnsiChar;
  f: Integer;
begin
  result := false;
  if (Length(sign) > 0) then
  begin
    if (Length(sign) <= 8) then
    begin
      st.ReadBuffer(buf[0], Length(sign));
      for f := 1 to Length(sign) do if (buf[f-1] <> sign[f]) then exit;
    end
    else
    begin
      for f := 1 to Length(sign) do
      begin
        st.ReadBuffer(buf[0], 1);
        if (buf[0] <> sign[f]) then exit;
      end;
    end;
  end;
  result := true;
end;

procedure writeInt (st: TStream; v: Byte); overload; begin writeIntegerLE(st, @v, 1); end;
procedure writeInt (st: TStream; v: ShortInt); overload; begin writeIntegerLE(st, @v, 1); end;
procedure writeInt (st: TStream; v: Word); overload; begin writeIntegerLE(st, @v, 2); end;
procedure writeInt (st: TStream; v: SmallInt); overload; begin writeIntegerLE(st, @v, 2); end;
procedure writeInt (st: TStream; v: LongWord); overload; begin writeIntegerLE(st, @v, 4); end;
procedure writeInt (st: TStream; v: LongInt); overload; begin writeIntegerLE(st, @v, 4); end;
procedure writeInt (st: TStream; v: Int64); overload; begin writeIntegerLE(st, @v, 8); end;
procedure writeInt (st: TStream; v: UInt64); overload; begin writeIntegerLE(st, @v, 8); end;

procedure writeIntBE (st: TStream; v: Byte); overload; begin writeIntegerBE(st, @v, 1); end;
procedure writeIntBE (st: TStream; v: ShortInt); overload; begin writeIntegerBE(st, @v, 1); end;
procedure writeIntBE (st: TStream; v: Word); overload; begin writeIntegerBE(st, @v, 2); end;
procedure writeIntBE (st: TStream; v: SmallInt); overload; begin writeIntegerBE(st, @v, 2); end;
procedure writeIntBE (st: TStream; v: LongWord); overload; begin writeIntegerBE(st, @v, 4); end;
procedure writeIntBE (st: TStream; v: LongInt); overload; begin writeIntegerBE(st, @v, 4); end;
procedure writeIntBE (st: TStream; v: Int64); overload; begin writeIntegerBE(st, @v, 8); end;
procedure writeIntBE (st: TStream; v: UInt64); overload; begin writeIntegerBE(st, @v, 8); end;

procedure writeBool (st: TStream; b: Boolean); begin writeInt(st, Byte(b)); end;
function readBool (st: TStream): Boolean; begin result := (readByte(st) <> 0); end;


procedure writeStr (st: TStream; const str: AnsiString; maxlen: LongWord=65535);
begin
  if (Length(str) > maxlen) then raise XStreamError.Create('string too long');
  if (maxlen <= 65535) then writeInt(st, Word(Length(str))) else writeInt(st, LongWord(Length(str)));
  if (Length(str) > 0) then st.WriteBuffer(str[1], Length(str));
end;

function readStr (st: TStream; maxlen: LongWord=65535): AnsiString;
var
  len: Integer;
begin
  result := '';
  if (maxlen <= 65535) then len := readWord(st) else len := Integer(readLongWord(st));
  if (len < 0) or (len > maxlen) then raise XStreamError.Create('string too long');
  if (len > 0) then
  begin
    SetLength(result, len);
    st.ReadBuffer(result[1], len);
  end;
end;


procedure readIntegerLE (st: TStream; vp: Pointer; size: Integer);
{$IFDEF ENDIAN_LITTLE}
begin
  st.readBuffer(vp^, size);
end;
{$ELSE}
var
  p: PByte;
begin
  p := PByte(vp)+size-1;
  while size > 0 do
  begin
    st.readBuffer(p^, 1);
    Dec(size);
    Dec(p);
  end;
end;
{$ENDIF}

procedure readIntegerBE (st: TStream; vp: Pointer; size: Integer);
{$IFDEF ENDIAN_LITTLE}
var
  p: PByte;
begin
  p := PByte(vp)+size-1;
  while size > 0 do
  begin
    st.readBuffer(p^, 1);
    Dec(size);
    Dec(p);
  end;
end;
{$ELSE}
begin
  st.readBuffer(vp^, size);
end;
{$ENDIF}

function readByte (st: TStream): Byte; begin readIntegerLE(st, @result, 1); end;
function readShortInt (st: TStream): ShortInt; begin readIntegerLE(st, @result, 1); end;
function readWord (st: TStream): Word; begin readIntegerLE(st, @result, 2); end;
function readSmallInt (st: TStream): SmallInt; begin readIntegerLE(st, @result, 2); end;
function readLongWord (st: TStream): LongWord; begin readIntegerLE(st, @result, 4); end;
function readLongInt (st: TStream): LongInt; begin readIntegerLE(st, @result, 4); end;
function readInt64 (st: TStream): Int64; begin readIntegerLE(st, @result, 8); end;
function readUInt64 (st: TStream): UInt64; begin readIntegerLE(st, @result, 8); end;

function readByteBE (st: TStream): Byte; begin readIntegerBE(st, @result, 1); end;
function readShortIntBE (st: TStream): ShortInt; begin readIntegerBE(st, @result, 1); end;
function readWordBE (st: TStream): Word; begin readIntegerBE(st, @result, 2); end;
function readSmallIntBE (st: TStream): SmallInt; begin readIntegerBE(st, @result, 2); end;
function readLongWordBE (st: TStream): LongWord; begin readIntegerBE(st, @result, 4); end;
function readLongIntBE (st: TStream): LongInt; begin readIntegerBE(st, @result, 4); end;
function readInt64BE (st: TStream): Int64; begin readIntegerBE(st, @result, 8); end;
function readUInt64BE (st: TStream): UInt64; begin readIntegerBE(st, @result, 8); end;


// ////////////////////////////////////////////////////////////////////////// //
function nlerp (a, b: Integer; t: Single): Integer; inline; begin result := round((1.0 - t) * a + t * b); end;

function nmin (a, b: Byte): Byte; inline; overload; begin if (a < b) then result := a else result := b; end;
function nmin (a, b: ShortInt): ShortInt; inline; overload; begin if (a < b) then result := a else result := b; end;
function nmin (a, b: Word): Word; inline; overload; begin if (a < b) then result := a else result := b; end;
function nmin (a, b: SmallInt): SmallInt; inline; overload; begin if (a < b) then result := a else result := b; end;
function nmin (a, b: LongWord): LongWord; inline; overload; begin if (a < b) then result := a else result := b; end;
function nmin (a, b: LongInt): LongInt; inline; overload; begin if (a < b) then result := a else result := b; end;
function nmin (a, b: Int64): Int64; inline; overload; begin if (a < b) then result := a else result := b; end;
function nmin (a, b: UInt64): UInt64; inline; overload; begin if (a < b) then result := a else result := b; end;
function nmin (a, b: Single): Single; inline; overload; begin if (a < b) then result := a else result := b; end;
function nmin (a, b: Double): Double; inline; overload; begin if (a < b) then result := a else result := b; end;
{$IFDEF FPC_HAS_TYPE_EXTENDED}
function nmin (a, b: Extended): Extended; inline; overload; begin if (a < b) then result := a else result := b; end;
{$ENDIF}

function nmax (a, b: Byte): Byte; inline; overload; begin if (a > b) then result := a else result := b; end;
function nmax (a, b: ShortInt): ShortInt; inline; overload; begin if (a > b) then result := a else result := b; end;
function nmax (a, b: Word): Word; inline; overload; begin if (a > b) then result := a else result := b; end;
function nmax (a, b: SmallInt): SmallInt; inline; overload; begin if (a > b) then result := a else result := b; end;
function nmax (a, b: LongWord): LongWord; inline; overload; begin if (a > b) then result := a else result := b; end;
function nmax (a, b: LongInt): LongInt; inline; overload; begin if (a > b) then result := a else result := b; end;
function nmax (a, b: Int64): Int64; inline; overload; begin if (a > b) then result := a else result := b; end;
function nmax (a, b: UInt64): UInt64; inline; overload; begin if (a > b) then result := a else result := b; end;
function nmax (a, b: Single): Single; inline; overload; begin if (a > b) then result := a else result := b; end;
function nmax (a, b: Double): Double; inline; overload; begin if (a > b) then result := a else result := b; end;
{$IFDEF FPC_HAS_TYPE_EXTENDED}
function nmax (a, b: Extended): Extended; inline; overload; begin if (a > b) then result := a else result := b; end;
{$ENDIF}

function nclamp (v, a, b: Byte): Byte; inline; overload; begin if (v < a) then result := a else if (v > b) then result := b else result := v; end;
function nclamp (v, a, b: ShortInt): ShortInt; inline; overload; begin if (v < a) then result := a else if (v > b) then result := b else result := v; end;
function nclamp (v, a, b: Word): Word; inline; overload; begin if (v < a) then result := a else if (v > b) then result := b else result := v; end;
function nclamp (v, a, b: SmallInt): SmallInt; inline; overload; begin if (v < a) then result := a else if (v > b) then result := b else result := v; end;
function nclamp (v, a, b: LongWord): LongWord; inline; overload; begin if (v < a) then result := a else if (v > b) then result := b else result := v; end;
function nclamp (v, a, b: LongInt): LongInt; inline; overload; begin if (v < a) then result := a else if (v > b) then result := b else result := v; end;
function nclamp (v, a, b: Int64): Int64; inline; overload; begin if (v < a) then result := a else if (v > b) then result := b else result := v; end;
function nclamp (v, a, b: UInt64): UInt64; inline; overload; begin if (v < a) then result := a else if (v > b) then result := b else result := v; end;
function nclamp (v, a, b: Single): Single; inline; overload; begin if (v < a) then result := a else if (v > b) then result := b else result := v; end;
function nclamp (v, a, b: Double): Double; inline; overload; begin if (v < a) then result := a else if (v > b) then result := b else result := v; end;
{$IFDEF FPC_HAS_TYPE_EXTENDED}
function nclamp (v, a, b: Extended): Extended; inline; overload; begin if (v < a) then result := a else if (v > b) then result := b else result := v; end;
{$ENDIF}

// ////////////////////////////////////////////////////////////////////////// //
{$IFDEF WINDOWS}
function snprintf (buf: PAnsiChar; bufsize: SizeUInt; const fmt: PAnsiChar): SizeUInt; cdecl; varargs; external 'msvcrt.dll' name '_snprintf';
{$ELSE}
function snprintf (buf: PAnsiChar; bufsize: SizeUInt; const fmt: PAnsiChar): SizeUInt; cdecl; varargs; external 'libc' name 'snprintf';
{$ENDIF}


(*
procedure conwriter (constref buf; len: SizeUInt);
var
  ss: ShortString;
  slen: Integer;
  b: PByte;
begin
  if (len < 1) then exit;
  b := PByte(@buf);
  while (len > 0) do
  begin
    if (len > 255) then slen := 255 else slen := Integer(len);
    Move(b^, ss[1], len);
    ss[0] := AnsiChar(slen);
    write(ss);
    b += slen;
    len -= slen;
  end;
end;
*)


function formatstrf (const fmt: AnsiString; const args: array of const; writerCB: TFormatStrFCallback=nil): AnsiString;
const
  PadSpaces: AnsiString = '                                                                       ';
  PadZeroes: AnsiString = '00000000000000000000000000000000000000000000000000000000000000000000000';
var
  curarg: Integer = 0; // current arg in `args`
  sign, fmtch: AnsiChar;
  zeropad: Boolean;
  width, prec: Integer; // width and precision
  spos, epos: Integer;
  ch: AnsiChar;
  strbuf: array[0..256] of AnsiChar;
  strblen: SizeUInt;
  fmtbuf: array[0..256] of AnsiChar;
  fmtblen: Integer;
  pclen: Integer;
  pc: PAnsiChar;
  ccname: ShortString;

  procedure writer (constref buf; len: SizeUInt);
  var
    ss: ShortString;
    slen: Integer;
    b: PByte;
  begin
    if (len < 1) then exit;
    b := PByte(@buf);
    if assigned(writerCB) then
    begin
      writerCB(b^, len);
    end
    else
    begin
      while (len > 0) do
      begin
        if (len > 255) then slen := 255 else slen := Integer(len);
        Move(b^, ss[1], slen);
        ss[0] := AnsiChar(slen);
        result += ss;
        b += slen;
        len -= slen;
      end;
    end;
  end;

  procedure xwrite (const s: AnsiString);
  begin
    if (Length(s) > 0) then writer(PAnsiChar(s)^, Length(s));
  end;

  procedure putFmtChar (ch: AnsiChar);
  begin
    fmtbuf[fmtblen] := ch;
    Inc(fmtblen);
  end;

  procedure putFmtInt (n: Integer);
  var
    len: SizeUInt;
  begin
    len := snprintf(@fmtbuf[fmtblen], Length(fmtbuf)-fmtblen, '%d', n);
    if (len > 0) then Inc(fmtblen, len);
  end;

  procedure buildCFormat (const pfx: AnsiString='');
  var
    f: Integer;
  begin
    fmtblen := 0;
    for f := 1 to Length(pfx) do putFmtChar(pfx[f]);
    putFmtChar('%');
    if (sign <> ' ') then putFmtChar(sign);
    if (width >= 0) then
    begin
      if (zeropad) then putFmtChar('0');
      putFmtInt(width);
      if (prec >= 0) then
      begin
        putFmtChar('.');
        putFmtInt(prec);
      end;
    end;
    putFmtChar(fmtch);
    fmtbuf[fmtblen] := #0;
  end;

  procedure writeStrBuf ();
  begin
    if (strblen > 0) then writer(strbuf, strblen);
  end;

  function i642str (n: Int64; hex: Boolean; hexup: Boolean): PAnsiChar;
  var
    neg: Boolean;
    xpos: Integer;
  begin
    if (n = $8000000000000000) then
    begin
      if hex then snprintf(@strbuf[0], Length(strbuf), '-8000000000000000')
      else snprintf(@strbuf[0], Length(strbuf), '-9223372036854775808');
      result := @strbuf[0];
    end
    else
    begin
      neg := (n < 0);
      if neg then n := -n;
      xpos := High(strbuf);
      strbuf[xpos] := #0; Dec(xpos);
      repeat
        if not hex then
        begin
          strbuf[xpos] := AnsiChar((n mod 10)+48);
          Dec(xpos);
          n := n div 10;
        end
        else
        begin
          if (n mod 16 > 9) then
          begin
            strbuf[xpos] := AnsiChar((n mod 16)+48+7);
            if not hexup then Inc(strbuf[xpos], 32);
          end
          else strbuf[xpos] := AnsiChar((n mod 16)+48);
          Dec(xpos);
          n := n div 16;
        end;
      until (n = 0);
      if neg then begin strbuf[xpos] := '-'; Dec(xpos); end;
      result := @strbuf[xpos+1];
    end;
  end;

  function ui642str (n: UInt64; hex: Boolean; hexup: Boolean): PAnsiChar;
  var
    xpos: Integer;
  begin
    xpos := High(strbuf);
    strbuf[xpos] := #0; Dec(xpos);
    repeat
      if not hex then
      begin
        strbuf[xpos] := AnsiChar((n mod 10)+48);
        Dec(xpos);
        n := n div 10;
      end
      else
      begin
        if (n mod 16 > 9) then
        begin
          strbuf[xpos] := AnsiChar((n mod 16)+48+7);
          if not hexup then Inc(strbuf[xpos], 32);
        end
        else strbuf[xpos] := AnsiChar((n mod 16)+48);
        Dec(xpos);
        n := n div 16;
      end;
    until (n = 0);
    result := @strbuf[xpos+1];
  end;

  procedure indent (len: Integer);
  var
    ilen: Integer;
  begin
    while (len > 0) do
    begin
      if (len > Length(PadSpaces)) then ilen := Length(PadSpaces) else ilen := len;
      writer(PAnsiChar(PadSpaces)^, ilen);
      Dec(len, ilen);
    end;
  end;

  procedure indent0 (len: Integer);
  var
    ilen: Integer;
  begin
    while (len > 0) do
    begin
      if (len > Length(PadZeroes)) then ilen := Length(PadZeroes) else ilen := len;
      writer(PAnsiChar(PadZeroes)^, ilen);
      Dec(len, ilen);
    end;
  end;

begin
  result := '';
  spos := 1;
  while (spos <= Length(fmt)) do
  begin
    // print literal part
    epos := spos;
    while (epos <= Length(fmt)) and (fmt[epos] <> '%') do Inc(epos);
    // output literal part
    if (epos > spos) then
    begin
      if (epos > Length(fmt)) then
      begin
        writer((PAnsiChar(fmt)+spos-1)^, epos-spos);
        break;
      end;
           if (epos+1 > Length(fmt)) then Inc(epos) // last percent, output literally
      else if (fmt[epos+1] = '%') then // special case
      begin
        Inc(epos);
        writer((PAnsiChar(fmt)+spos-1)^, epos-spos);
        spos := epos+1;
      end
      else
      begin
        writer((PAnsiChar(fmt)+spos-1)^, epos-spos);
        spos := epos;
      end;
      continue;
    end;
    // check if we have argument for this format string
    if (curarg > High(args)) then
    begin
      xwrite('<OUT OF ARGS>');
      writer((PAnsiChar(fmt)+spos-1)^, Length(fmt)-spos+1);
      break;
    end;
    // skip percent
    if (spos+1 > Length(fmt)) then break; // oops
    assert(fmt[spos] = '%');
    Inc(spos);
    // parse format; check for sign
         if (fmt[spos] = '-') then begin sign := '-'; Inc(spos); end
    else if (fmt[spos] = '+') then begin sign := '+'; Inc(spos); end
    else sign := ' ';
    // parse width
    if (spos > Length(fmt)) then begin xwrite('<INVALID FORMAT>'); break; end;
    if (sign <> ' ') or ((fmt[spos] >= '0') and (fmt[spos] <= '9')) then
    begin
      if (fmt[spos] < '0') or (fmt[spos] > '9') then begin xwrite('<INVALID FORMAT>'); writer((PAnsiChar(fmt)+spos-1)^, Length(fmt)-spos+1); break; end;
      zeropad := (fmt[spos] = '0');
      width := 0;
      while (spos <= Length(fmt)) do
      begin
        ch := fmt[spos];
        if (ch < '0') or (ch > '9') then break;
        width := width*10+Integer(ch)-48;
        Inc(spos);
      end;
    end
    else
    begin
      width := -1;
      zeropad := false;
    end;
    // parse precision
    prec := -1;
    if (spos <= Length(fmt)) and (fmt[spos] = '.') then
    begin
      Inc(spos);
      if (spos > Length(fmt)) then begin xwrite('<INVALID FORMAT>'); break; end;
      if (fmt[spos] < '0') or (fmt[spos] > '9') then begin xwrite('<INVALID FORMAT>'); writer((PAnsiChar(fmt)+spos-1)^, Length(fmt)-spos+1); break; end;
      prec := 0;
      while (spos <= Length(fmt)) do
      begin
        ch := fmt[spos];
        if (ch < '0') or (ch > '9') then break;
        prec := prec*10+Integer(ch)-48;
        Inc(spos);
      end;
    end;
    // get format char
    if (spos > Length(fmt)) then begin xwrite('<INVALID FORMAT>'); break; end;
    fmtch := fmt[spos];
    Inc(spos);
    // done parsing format, check for valid format chars
    if not (fmtch in ['s','u','d','x','X','p','f','g','c']) then begin xwrite('<INVALID FORMAT CHAR>'); writer((PAnsiChar(fmt)+spos-1)^, Length(fmt)-spos+1); break; end;
    // now write formatted string
    case args[curarg].VType of
      vtInteger: // args[curarg].VInteger
        begin
          if not (fmtch in ['s','u','d','x','X']) then begin xwrite('<INVALID FORMAT CHAR>'); writer((PAnsiChar(fmt)+spos-1)^, Length(fmt)-spos+1); break; end;
          if (fmtch = 's') then fmtch := 'd';
          buildCFormat();
          strblen := snprintf(@strbuf[0], Length(strbuf), @fmtbuf[0], args[curarg].VInteger);
          writeStrBuf();
        end;
      vtBoolean: // args[curarg].VBoolean
        case fmtch of
          's':
            begin
              buildCFormat();
              if args[curarg].VBoolean then strblen := snprintf(@strbuf[0], Length(strbuf), @fmtbuf[0], 'true')
              else strblen := snprintf(@strbuf[0], Length(strbuf), @fmtbuf[0], 'false');
              writeStrBuf();
            end;
          'c':
            begin
              buildCFormat();
              if args[curarg].VBoolean then strblen := snprintf(@strbuf[0], Length(strbuf), @fmtbuf[0], AnsiChar('t'))
              else strblen := snprintf(@strbuf[0], Length(strbuf), @fmtbuf[0], AnsiChar('f'));
              writeStrBuf();
            end;
          'u', 'd', 'x', 'X':
            begin
              buildCFormat();
              strblen := snprintf(@strbuf[0], Length(strbuf), @fmtbuf[0], Integer(args[curarg].VBoolean));
              writeStrBuf();
            end;
          else
            begin
              xwrite('<INVALID FORMAT CHAR>');
              writer((PAnsiChar(fmt)+spos-1)^, Length(fmt)-spos+1);
              break;
            end;
        end;
      vtChar: // args[curarg].VChar
        case fmtch of
          's', 'c':
            begin
              fmtch := 'c';
              buildCFormat();
              strblen := snprintf(@strbuf[0], Length(strbuf), @fmtbuf[0], args[curarg].VChar);
              writeStrBuf();
            end;
          'u', 'd', 'x', 'X':
            begin
              buildCFormat();
              strblen := snprintf(@strbuf[0], Length(strbuf), @fmtbuf[0], Integer(args[curarg].VChar));
              writeStrBuf();
            end;
          else
            begin
              xwrite('<INVALID FORMAT CHAR>');
              writer((PAnsiChar(fmt)+spos-1)^, Length(fmt)-spos+1);
              break;
            end;
        end;
      //vtWideChar: begin end; // args[curarg].VWideChar (WideChar)
      vtExtended: // args[curarg].VExtended^
        case fmtch of
          's', 'g':
            begin
              fmtch := 'g';
              buildCFormat();
              strblen := snprintf(@strbuf[0], Length(strbuf), @fmtbuf[0], Double(args[curarg].VExtended^));
              writeStrBuf();
            end;
          'f':
            begin
              buildCFormat();
              strblen := snprintf(@strbuf[0], Length(strbuf), @fmtbuf[0], Double(args[curarg].VExtended^));
              writeStrBuf();
            end;
          'd':
            begin
              buildCFormat();
              strblen := snprintf(@strbuf[0], Length(strbuf), @fmtbuf[0], Integer(trunc(args[curarg].VExtended^)));
              writeStrBuf();
            end;
          'u', 'x', 'X':
            begin
              buildCFormat();
              strblen := snprintf(@strbuf[0], Length(strbuf), @fmtbuf[0], LongWord(trunc(args[curarg].VExtended^)));
              writeStrBuf();
            end;
          else
            begin
              xwrite('<INVALID FORMAT CHAR>');
              writer((PAnsiChar(fmt)+spos-1)^, Length(fmt)-spos+1);
              break;
            end;
        end;
      vtString: // args[curarg].VString^ (PShortString)
        begin
          if (sign <> '-') then indent(width-Length(args[curarg].VString^));
          writer(args[curarg].VString^[1], Length(args[curarg].VString^));
          if (sign = '-') then indent(width-Length(args[curarg].VString^));
        end;
      vtPointer: // args[curarg].VPointer
        case fmtch of
          's':
            begin
              fmtch := 'x';
              if (width < 8) then width := 8;
              zeropad := true;
              buildCFormat('0x');
              strblen := snprintf(@strbuf[0], Length(strbuf), @fmtbuf[0], PtrUInt(args[curarg].VPointer));
              writeStrBuf();
            end;
          'u', 'd', 'x', 'p', 'X':
            begin
              if (fmtch = 'p') then fmtch := 'x';
              if (width < 8) then width := 8;
              zeropad := true;
              buildCFormat('0x');
              strblen := snprintf(@strbuf[0], Length(strbuf), @fmtbuf[0], PtrUInt(args[curarg].VPointer));
              writeStrBuf();
            end;
          else
            begin
              xwrite('<INVALID FORMAT CHAR>');
              writer((PAnsiChar(fmt)+spos-1)^, Length(fmt)-spos+1);
              break;
            end;
        end;
      vtPChar: // args[curarg].VPChar
        if (args[curarg].VPChar = nil) then
        begin
          if (sign <> '-') then indent(width-3);
          xwrite('nil');
          if (sign = '-') then indent(width-3);
        end
        else
        begin
          pclen := 0;
          while (args[curarg].VPChar[pclen] <> #0) do Inc(pclen);
          if (sign <> '-') then indent(width-pclen);
          writer(args[curarg].VPChar^, pclen);
          if (sign = '-') then indent(width-pclen);
        end;
      vtObject: // args[curarg].VObject.Classname (TObject)
        begin
          if (args[curarg].VObject <> nil) then ccname := args[curarg].VObject.Classname else ccname := '<nil>';
          if (sign <> '-') then indent(width-Length(ccname));
          xwrite(ccname);
          if (sign = '-') then indent(width-Length(ccname));
        end;
      vtClass: // args[curarg].VClass.Classname (TClass)
        begin
          if (args[curarg].VClass <> nil) then ccname := args[curarg].VClass.Classname else ccname := '<nil>';
          if (sign <> '-') then indent(width-Length(ccname));
          xwrite(ccname);
          if (sign = '-') then indent(width-Length(ccname));
        end;
      //vtPWideChar: begin end; // args[curarg].VPWideChar (PWideChar)
      vtAnsiString: // AnsiString(args[curarg].VAnsiString) (Pointer)
        begin
          if (sign <> '-') then indent(width-Length(AnsiString(args[curarg].VAnsiString)));
          xwrite(AnsiString(args[curarg].VAnsiString));
          if (sign = '-') then indent(width-Length(AnsiString(args[curarg].VAnsiString)));
        end;
      //vtCurrency: begin end; // args[curarg].VCurrency (PCurrency)
      //vtVariant: begin end; // args[curarg].VVariant^ (PVariant)
      //vtInterface: begin end; // args[curarg].VInterface (Pointer);
      //vtWideString: begin end; // args[curarg].VWideString (Pointer);
      vtInt64: // args[curarg].VInt64^ (PInt64)
        begin
          case fmtch of
            's','d','u': pc := i642str(args[curarg].VInt64^, false, false);
            'x': pc := i642str(args[curarg].VInt64^, true, false);
            'X': pc := i642str(args[curarg].VInt64^, true, true);
            else begin xwrite('<INVALID FORMAT CHAR>'); writer((PAnsiChar(fmt)+spos-1)^, Length(fmt)-spos+1); break; end;
          end;
          pclen := 0;
          while (pc[pclen] <> #0) do Inc(pclen);
          if (sign <> '-') and (width > pclen) then
          begin
            if zeropad then
            begin
              if (pc[0] = '-') or (pc[0] = '+') then
              begin
                writer(pc^, 1);
                indent0(width-pclen-1);
                Inc(pc);
                Dec(pclen);
              end
              else
              begin
                indent0(width-pclen);
              end;
            end
            else
            begin
              indent(width-pclen);
            end;
          end;
          writer(pc^, pclen);
          if (sign = '-') then indent(width-pclen);
        end;
      vtQWord: // args[curarg].VQWord^ (PQWord)
        begin
          case fmtch of
            's','d','u': pc := ui642str(args[curarg].VInt64^, false, false);
            'x': pc := ui642str(args[curarg].VInt64^, true, false);
            'X': pc := ui642str(args[curarg].VInt64^, true, true);
            else begin xwrite('<INVALID FORMAT CHAR>'); writer((PAnsiChar(fmt)+spos-1)^, Length(fmt)-spos+1); break; end;
          end;
          pclen := 0;
          while (pc[pclen] <> #0) do Inc(pclen);
          if (sign <> '-') then begin if zeropad then indent0(width-pclen) else indent(width-pclen); end;
          writer(pc^, pclen);
          if (sign = '-') then indent(width-pclen);
        end;
      else
        begin
          xwrite('<INVALID TYPE>');
          writer((PAnsiChar(fmt)+spos-1)^, Length(fmt)-spos+1);
          break;
        end;
    end;
    Inc(curarg);
  end;
end;


function GetDiskFileInfo (fname: AnsiString; var info: TDiskFileInfo): Boolean;
var
  age: LongInt;
  size: LongInt;
  handle: THandle;
begin
  result := false;
  if (length(fname) = 0) then exit;
  if not findFileCI(fname) then exit;
  // get age
  age := FileAge(fname);
  if (age = -1) then exit;
  // get size
  handle := FileOpen(fname, fmOpenRead or fmShareDenyNone);
  if (handle = THandle(-1)) then exit;
  size := FileSeek(handle, 0, fsFromEnd);
  FileClose(handle);
  if (size = -1) then exit;
  // fill info
  info.diskName := fname;
  info.size := size;
  info.age := age;
  result := true;
end;


(*
var
  ss: ShortString;
  ls: AnsiString;
  i64: Int64 = -$A000000000;
  ui64: UInt64 = $A000000000;
begin
  writef(conwriter, 'test int:<%s> bool:<%s:%02d:%c> bool:<%s:%02d:%c>; char:<%2s;%c;%d>!'#10, [42, true, true, true, false, false, false, 'A', 'A', 'A']);
  writef(conwriter, 'test float:<%s;%u;%f;%g>'#10, [666.6942, 666.6942, 666.6942, 666.6942]);
  ss := 'fuckit';
  ls := 'FUCKIT';
  writef(conwriter, 'test ss:<%5s;%040s>'#10, [ss, ss]);
  writef(conwriter, 'test ls:<%5s;%040s>'#10, [ls, ls]);
  writef(conwriter, 'test pointer:<%s;%x;%p>'#10, [@ss, @ss, @ss]);
  writef(conwriter, 'test i64:<%s;%x;%015d;%u;%X>'#10, [i64, i64, i64, i64, i64]);
  writef(conwriter, 'test ui64:<%s;%x;%15d;%015u;%X>'#10, [ui64, ui64, ui64, ui64, ui64]);
*)
end.
