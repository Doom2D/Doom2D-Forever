(* Copyright (C)  DooM 2D:Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
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
unit xparser;

interface


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
type
  TTextParser = class
  public
    const
      TTNone = -1;
      TTEOF = 0;
      TTId = 1;
      TTInt = 2;
      //TTFloat = 3; // not yet
      TTStr = 4; // string
      TTComma = 5; // ','
      TTColon = 6; // ':'
      TTSemi = 7; // ';'
      TTBegin = 8; // left curly
      TTEnd = 9; // right curly
      TTDelim = 10; // other delimiters

  private
    mLine, mCol: Integer;
    mCurChar, mNextChar: AnsiChar;

    mAllowSignedNumbers: Boolean; // internal control

    mTokLine, mTokCol: Integer; // token start
    mTokType: Integer;
    mTokStr: AnsiString; // string or identifier
    mTokChar: AnsiChar; // for delimiters
    mTokInt: Integer;

  protected
    procedure warmup (); virtual; abstract; // called in constructor to warm up the system
    procedure loadNextChar (); virtual; abstract; // loads next char into mNextChar; #0 means 'eof'

  public
    class function quote (const s: AnsiString): AnsiString;

  public
    constructor Create (loadToken: Boolean=true);
    destructor Destroy (); override;

    function isEOF (): Boolean; inline;

    function skipChar (): Boolean; // returns `false` on eof

    function skipBlanks (): Boolean; // ...and comments; returns `false` on eof

    function skipToken (): Boolean; // returns `false` on eof

    function expectId (): AnsiString;
    procedure expectId (const aid: AnsiString);
    function eatId (const aid: AnsiString): Boolean;

    function expectStr (allowEmpty: Boolean=false): AnsiString;
    function expectInt (): Integer;

    procedure expectTT (ttype: Integer);
    function eatTT (ttype: Integer): Boolean;

    function expectDelim (const ch: AnsiChar): AnsiChar;
    function eatDelim (const ch: AnsiChar): Boolean;

  public
    property col: Integer read mCol;
    property line: Integer read mLine;

    property curChar: AnsiChar read mCurChar;
    property nextChar: AnsiChar read mNextChar;

    // token start
    property tokCol: Integer read mTokCol;
    property tokLine: Integer read mTokLine;

    property tokType: Integer read mTokType; // see TTXXX constants
    property tokStr: AnsiString read mTokStr; // string or identifier
    property tokChar: AnsiChar read mTokChar; // for delimiters
    property tokInt: Integer read mTokInt;
  end;


// ////////////////////////////////////////////////////////////////////////// //
type
  TFileTextParser = class(TTextParser)
  private
    mFile: File;

  protected
    procedure warmup (); override; // called in constructor to warm up the system
    procedure loadNextChar (); override; // loads next char into mNextChar; #0 means 'eof'

  public
    constructor Create (const fname: AnsiString; loadToken: Boolean=true);
    destructor Destroy (); override;
  end;

  TStrTextParser = class(TTextParser)
  private
    mStr: AnsiString;
    mPos: Integer;

  protected
    procedure warmup (); override; // called in constructor to warm up the system
    procedure loadNextChar (); override; // loads next char into mNextChar; #0 means 'eof'

  public
    constructor Create (const astr: AnsiString; loadToken: Boolean=true);
    destructor Destroy (); override;
  end;


// ////////////////////////////////////////////////////////////////////////// //
type
  TTextWriter = class
  protected
    mIndent: Integer;

  protected
    procedure putBuf (constref buf; len: SizeUInt); virtual; abstract;

  public
    constructor Create ();

    procedure put (const s: AnsiString); overload;
    procedure put (v: Byte); overload;
    procedure put (v: Integer); overload;
    procedure put (const fmt: AnsiString; args: array of const); overload;
    procedure putIndent ();
    procedure indent ();
    procedure unindent ();
  end;


// ////////////////////////////////////////////////////////////////////////// //
type
  TFileTextWriter = class(TTextWriter)
  private
    mFile: File;

  protected
    procedure putBuf (constref buf; len: SizeUInt); override;

  public
    constructor Create (const fname: AnsiString);
    destructor Destroy (); override;
  end;


// ////////////////////////////////////////////////////////////////////////// //
function wcharTo1251 (wc: WideChar): AnsiChar; inline;
function utfTo1251 (const s: AnsiString): AnsiString;

function digitInBase (ch: AnsiChar; base: Integer): Integer;


implementation

uses
  SysUtils, utils;


var
  wc2shitmap: array[0..65535] of AnsiChar;
  wc2shitmapInited: Boolean = false;


// ////////////////////////////////////////////////////////////////////////// //
procedure initShitMap ();
const
  cp1251: array[0..127] of Word = (
    $0402,$0403,$201A,$0453,$201E,$2026,$2020,$2021,$20AC,$2030,$0409,$2039,$040A,$040C,$040B,$040F,
    $0452,$2018,$2019,$201C,$201D,$2022,$2013,$2014,$003F,$2122,$0459,$203A,$045A,$045C,$045B,$045F,
    $00A0,$040E,$045E,$0408,$00A4,$0490,$00A6,$00A7,$0401,$00A9,$0404,$00AB,$00AC,$00AD,$00AE,$0407,
    $00B0,$00B1,$0406,$0456,$0491,$00B5,$00B6,$00B7,$0451,$2116,$0454,$00BB,$0458,$0405,$0455,$0457,
    $0410,$0411,$0412,$0413,$0414,$0415,$0416,$0417,$0418,$0419,$041A,$041B,$041C,$041D,$041E,$041F,
    $0420,$0421,$0422,$0423,$0424,$0425,$0426,$0427,$0428,$0429,$042A,$042B,$042C,$042D,$042E,$042F,
    $0430,$0431,$0432,$0433,$0434,$0435,$0436,$0437,$0438,$0439,$043A,$043B,$043C,$043D,$043E,$043F,
    $0440,$0441,$0442,$0443,$0444,$0445,$0446,$0447,$0448,$0449,$044A,$044B,$044C,$044D,$044E,$044F
  );
var
  f: Integer;
begin
  for f := 0 to High(wc2shitmap) do wc2shitmap[f] := '?';
  for f := 0 to 127 do wc2shitmap[f] := AnsiChar(f);
  for f := 0 to 127 do wc2shitmap[cp1251[f]] := AnsiChar(f+128);
  wc2shitmapInited := true;
end;


// ////////////////////////////////////////////////////////////////////////// //
// TODO: make a hash or something
function wcharTo1251 (wc: WideChar): AnsiChar; inline;
begin
  if not wc2shitmapInited then initShitMap();
  if (LongWord(wc) > 65535) then result := '?' else result := wc2shitmap[LongWord(wc)];
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
function utfTo1251 (const s: AnsiString): AnsiString;
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
        if ud.decode(s[c]) then result += wcharTo1251(WideChar(ud.codepoint));
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
class function TTextParser.quote (const s: AnsiString): AnsiString;

  function squote (const s: AnsiString): AnsiString;
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

  function dquote (const s: AnsiString): AnsiString;
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

var
  needSingle: Boolean = false;
  f: Integer;
begin
  for f := 1 to Length(s) do
  begin
    if (s[f] = '''') then begin needSingle := true; continue; end;
    if (s[f] < ' ') or (s[f] = #127) then begin result := dquote(s); exit; end;
  end;
  if needSingle then result := squote(s) else result := ''''+s+'''';
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TTextParser.Create (loadToken: Boolean=true);
begin
  mLine := 1;
  mCol := 1;
  mCurChar := #0;
  mNextChar := #0;
  mTokType := TTNone;
  mTokStr := '';
  mTokChar := #0;
  mTokInt := 0;
  mAllowSignedNumbers := true;
  warmup(); // change `mAllowSignedNumbers` there, if necessary
  if loadToken then skipToken();
end;


destructor TTextParser.Destroy ();
begin
  inherited;
end;


function TTextParser.isEOF (): Boolean; inline; begin result := (mCurChar = #0); end;


function TTextParser.skipChar (): Boolean;
begin
  if (mCurChar = #0) then begin result := false; exit; end;
  if (mCurChar = #10) then begin mCol := 1; Inc(mLine); end else Inc(mCol);
  mCurChar := mNextChar;
  if (mCurChar = #0) then begin result := false; exit; end;
  loadNextChar();
  // skip CR in CR/LF
  if (mCurChar = #13) then
  begin
    if (mNextChar = #10) then loadNextChar();
    mCurChar := #10;
  end;
  result := true;
end;


function TTextParser.skipBlanks (): Boolean;
var
  level: Integer;
begin
  while not isEOF do
  begin
    if (curChar = '/') then
    begin
      // single-line comment
      if (nextChar = '/') then
      begin
        while not isEOF and (curChar <> #10) do skipChar();
        skipChar(); // skip EOL
        continue;
      end;
      // multline comment
      if (nextChar = '*') then
      begin
        // skip comment start
        skipChar();
        skipChar();
        while not isEOF do
        begin
          if (curChar = '*') and (nextChar = '/') then
          begin
            // skip comment end
            skipChar();
            skipChar();
            break;
          end;
          skipChar();
        end;
        continue;
      end;
      // nesting multline comment
      if (nextChar = '+') then
      begin
        // skip comment start
        skipChar();
        skipChar();
        level := 1;
        while not isEOF do
        begin
          if (curChar = '+') and (nextChar = '/') then
          begin
            // skip comment end
            skipChar();
            skipChar();
            Dec(level);
            if (level = 0) then break;
            continue;
          end;
          if (curChar = '/') and (nextChar = '+') then
          begin
            // skip comment start
            skipChar();
            skipChar();
            Inc(level);
            continue;
          end;
          skipChar();
        end;
        continue;
      end;
    end;
    if (curChar > ' ') then break;
    skipChar(); // skip blank
  end;
  result := not isEOF;
end;


function TTextParser.skipToken (): Boolean;

  procedure parseInt ();
  var
    neg: Boolean = false;
    base: Integer = -1;
    n: Integer;
  begin
    if mAllowSignedNumbers then
    begin
      if (curChar = '+') or (curChar = '-') then
      begin
        neg := (curChar = '-');
        skipChar();
        if (curChar < '0') or (curChar > '9') then
        begin
          mTokType := TTDelim;
          if (neg) then mTokChar := '-' else mTokChar := '+';
          exit;
        end;
      end;
    end;
    if (curChar = '0') then
    begin
      case nextChar of
        'b','B': base := 2;
        'o','O': base := 8;
        'd','D': base := 10;
        'h','H': base := 16;
      end;
      if (base > 0) then
      begin
        // skip prefix
        skipChar();
        skipChar();
      end;
    end;
    // default base
    if (base < 0) then base := 10;
    if (digitInBase(curChar, base) < 0) then raise Exception.Create('invalid number');
    mTokType := TTInt;
    mTokInt := 0; // just in case
    while not isEOF do
    begin
      n := digitInBase(curChar, base);
      if (n < 0) then break;
      n := mTokInt*10+n;
      if (n < 0) or (n < mTokInt) then raise Exception.Create('integer overflow');
      mTokInt := n;
      skipChar();
    end;
    // check for valid number end
    if not isEOF then
    begin
      if (curChar = '.') then raise Exception.Create('floating numbers aren''t supported yet');
      if (curChar = '_') or ((curChar >= 'A') and (curChar <= 'Z')) or ((curChar >= 'a') and (curChar <= 'z')) or (curChar >= #128) then
      begin
        raise Exception.Create('invalid number');
      end;
    end;
    if neg then mTokInt := -mTokInt;
  end;

  procedure parseString ();
  var
    qch, ch: AnsiChar;
    n: Integer;
  begin
    mTokType := TTStr;
    mTokStr := ''; // just in case
    qch := curChar;
    skipChar(); // skip starting quote
    while not isEOF do
    begin
      // escape
      if (qch = '"') and (curChar = '\') then
      begin
        if (nextChar = #0) then raise Exception.Create('unterminated string escape');
        ch := nextChar;
        // skip backslash and escape type
        skipChar();
        skipChar();
        case ch of
          't': mTokStr += #9;
          'n': mTokStr += #10;
          'r': mTokStr += #13;
          'z': mTokStr += #0;
          'e': mTokStr += #27;
          'x', 'X': // hex escape
            begin
              n := digitInBase(curChar, 16);
              if (n < 0) then raise Exception.Create('invalid hexstr escape');
              skipChar();
              if (digitInBase(curChar, 16) > 0) then
              begin
                n := n*16+digitInBase(curChar, 16);
                skipChar();
              end;
              mTokStr += AnsiChar(n);
            end;
          else mTokStr += ch;
        end;
        continue;
      end;
      // duplicate single quote (pascal style)
      if (qch = '''') and (curChar = '''') and (nextChar = '''') then
      begin
        // skip both quotes
        skipChar();
        skipChar();
        mTokStr += '''';
        continue;
      end;
      if (curChar = qch) then
      begin
        skipChar(); // skip ending quote
        break;
      end;
      mTokStr += curChar;
      skipChar();
    end;
  end;

  procedure parseId ();
  begin
    mTokType := TTId;
    mTokStr := ''; // just in case
    while (curChar = '_') or ((curChar >= '0') and (curChar <= '9')) or
          ((curChar >= 'A') and (curChar <= 'Z')) or
          ((curChar >= 'a') and (curChar <= 'z')) or
          (curChar >= #128) do
    begin
      mTokStr += curChar;
      skipChar();
    end;
  end;

begin
  mTokType := TTEOF;
  mTokStr := '';
  mTokChar := #0;
  mTokInt := 0;

  if not skipBlanks() then
  begin
    result := false;
    mTokLine := mLine;
    mTokCol := mCol;
    exit;
  end;

  mTokLine := mLine;
  mTokCol := mCol;

  result := true;

  // number?
  if mAllowSignedNumbers and ((curChar = '+') or (curChar = '-')) then begin parseInt(); exit; end;
  if (curChar >= '0') and (curChar <= '9') then begin parseInt(); exit; end;

  // string?
  if (curChar = '"') or (curChar = '''') then begin parseString(); exit; end;

  // identifier?
  if (curChar = '_') or ((curChar >= 'A') and (curChar <= 'Z')) or ((curChar >= 'a') and (curChar <= 'z')) or (curChar >= #128) then begin parseId(); exit; end;

  // known delimiters?
  case curChar of
    ',': mTokType := TTComma;
    ':': mTokType := TTColon;
    ';': mTokType := TTSemi;
    '{': mTokType := TTBegin;
    '}': mTokType := TTEnd;
    else mTokType := TTDelim;
  end;
  mTokChar := curChar;
  skipChar();
end;


function TTextParser.expectId (): AnsiString;
begin
  if (mTokType <> TTId) then raise Exception.Create('identifier expected');
  result := mTokStr;
  skipToken();
end;


procedure TTextParser.expectId (const aid: AnsiString);
begin
  if (mTokType <> TTId) or (CompareText(mTokStr, aid) <> 0) then raise Exception.Create('identifier '''+aid+''' expected');
  skipToken();
end;


function TTextParser.eatId (const aid: AnsiString): Boolean;
begin
  result := false;
  if (mTokType <> TTId) or (CompareText(mTokStr, aid) <> 0) then exit;
  result := true;
  skipToken();
end;


function TTextParser.expectStr (allowEmpty: Boolean=false): AnsiString;
begin
  if (mTokType <> TTStr) then raise Exception.Create('string expected');
  if (not allowEmpty) and (Length(mTokStr) = 0) then raise Exception.Create('non-empty string expected');
  result := mTokStr;
  skipToken();
end;


function TTextParser.expectInt (): Integer;
begin
  if (mTokType <> TTInt) then raise Exception.Create('string expected');
  result := mTokInt;
  skipToken();
end;


procedure TTextParser.expectTT (ttype: Integer);
begin
  if (mTokType <> ttype) then raise Exception.Create('unexpected token');
  skipToken();
end;


function TTextParser.eatTT (ttype: Integer): Boolean;
begin
  result := (mTokType = ttype);
  if result then skipToken();
end;


function TTextParser.expectDelim (const ch: AnsiChar): AnsiChar;
begin
  if (mTokType <> TTDelim) then raise Exception.Create(Format('delimiter ''%s'' expected', [ch]));
  result := mTokChar;
  skipToken();
end;


function TTextParser.eatDelim (const ch: AnsiChar): Boolean;
begin
  result := false;
  if (mTokType <> TTDelim) or (mTokChar <> ch) then exit;
  result := true;
  skipToken();
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TFileTextParser.Create (const fname: AnsiString; loadToken: Boolean=true);
begin
  AssignFile(mFile, fname);
  Reset(mFile, 1);
  inherited Create(loadToken);
end;


destructor TFileTextParser.Destroy ();
begin
  CloseFile(mFile);
  inherited;
end;


procedure TFileTextParser.warmup ();
var
  rd: Integer;
begin
  blockRead(mFile, mCurChar, 1, rd);
  if (rd = 0) then begin mCurChar := #0; exit; end;
  if (mCurChar = #0) then mCurChar := ' ';
  loadNextChar();
end;


procedure TFileTextParser.loadNextChar ();
var
  rd: Integer;
begin
  blockRead(mFile, mNextChar, 1, rd);
  if (rd = 0) then begin mNextChar := #0; exit; end;
  if (mNextChar = #0) then mNextChar := ' ';
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TStrTextParser.Create (const astr: AnsiString; loadToken: Boolean=true);
begin
  mStr := astr;
  mPos := 1;
  inherited Create(loadToken);
end;


destructor TStrTextParser.Destroy ();
begin
  mStr := '';
  inherited;
end;


procedure TStrTextParser.warmup ();
begin
  if (mPos > Length(mStr)) then
  begin
    mCurChar := #0;
    mNextChar := #0;
    exit;
  end;
  mCurChar := mStr[mPos]; Inc(mPos);
  if (mCurChar = #0) then mCurChar := ' ';
  loadNextChar();
end;


procedure TStrTextParser.loadNextChar ();
begin
  mNextChar := #0;
  if (mPos > Length(mStr)) then exit;
  mNextChar := mStr[mPos]; Inc(mPos);
  if (mNextChar = #0) then mNextChar := ' ';
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TTextWriter.Create (); begin mIndent := 0; end;
procedure TTextWriter.put (const s: AnsiString); overload; begin if (Length(s) > 0) then putBuf((@(s[1]))^, Length(s)); end;
procedure TTextWriter.put (v: Byte); overload; begin put('%d', [v]); end;
procedure TTextWriter.put (v: Integer); overload; begin put('%d', [v]); end;
procedure TTextWriter.put (const fmt: AnsiString; args: array of const); overload; begin put(formatstrf(fmt, args)); end;
procedure TTextWriter.putIndent (); var f: Integer; begin for f := 1 to mIndent do put(' '); end;
procedure TTextWriter.indent (); begin Inc(mIndent, 2); end;
procedure TTextWriter.unindent (); begin Dec(mIndent, 2); end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TFileTextWriter.Create (const fname: AnsiString);
begin
  AssignFile(mFile, fname);
  Rewrite(mFile, 1);
  inherited Create();
end;


destructor TFileTextWriter.Destroy ();
begin
  CloseFile(mFile);
end;


procedure TFileTextWriter.putBuf (constref buf; len: SizeUInt);
var
  wr: SizeUInt;
  pc: PChar;
begin
  if (len > 0) then
  begin
    pc := @buf;
    BlockWrite(mFile, pc^, len, wr);
    if (wr <> len) then raise Exception.Create('write error');
    {
    while (len > 0) do
    begin
      write(pc^);
      Inc(pc);
      Dec(len);
    end;
    }
  end;
end;


end.
