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

uses
  Classes;


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
    procedure warmup (); virtual; // called in constructor to warm up the system
    procedure loadNextChar (); virtual; abstract; // loads next char into mNextChar; #0 means 'eof'

  public
    constructor Create ();
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

    function expectStrOrId (allowEmpty: Boolean=false): AnsiString;

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
    const BufSize = 16384;

  private
    mFile: TStream;
    mStreamOwned: Boolean;
    mBuffer: PChar;
    mBufLen: Integer;
    mBufPos: Integer;

  protected
    procedure loadNextChar (); override; // loads next char into mNextChar; #0 means 'eof'

  public
    constructor Create (const fname: AnsiString);
    constructor Create (st: TStream; astOwned: Boolean=true); // will take ownership on st by default
    destructor Destroy (); override;
  end;

  TStrTextParser = class(TTextParser)
  private
    mStr: AnsiString;
    mPos: Integer;

  protected
    procedure loadNextChar (); override; // loads next char into mNextChar; #0 means 'eof'

  public
    constructor Create (const astr: AnsiString);
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

    procedure flush (); virtual;

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
    const BufSize = 16384;

  private
    mFile: TStream;
    mStreamOwned: Boolean;
    mBuffer: PAnsiChar;
    mBufUsed: Integer;

  protected
    procedure putBuf (constref buf; len: SizeUInt); override;

  public
    constructor Create (const fname: AnsiString);
    constructor Create (ast: TStream; astOwned: Boolean=true); // will own the stream by default
    destructor Destroy (); override;

    procedure flush (); override;
  end;

  TStrTextWriter = class(TTextWriter)
  private
    mStr: AnsiString;

  protected
    procedure putBuf (constref buf; len: SizeUInt); override;

  public
    constructor Create ();
    destructor Destroy (); override;

    property str: AnsiString read mStr;
  end;


implementation

uses
  SysUtils, utils;


// ////////////////////////////////////////////////////////////////////////// //
function StrEqu (const a, b: AnsiString): Boolean; inline; begin result := (a = b); end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TTextParser.Create ();
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
  skipToken();
end;


destructor TTextParser.Destroy ();
begin
  inherited;
end;


function TTextParser.isEOF (): Boolean; inline; begin result := (mCurChar = #0); end;


procedure TTextParser.warmup ();
begin
  mNextChar := ' ';
  loadNextChar();
  mCurChar := mNextChar;
  if (mNextChar <> #0) then loadNextChar();
end;


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
  if (mTokType <> TTId) or (not StrEqu(mTokStr, aid)) then raise Exception.Create('identifier '''+aid+''' expected');
  skipToken();
end;


function TTextParser.eatId (const aid: AnsiString): Boolean;
begin
  result := false;
  if (mTokType <> TTId) or (not StrEqu(mTokStr, aid)) then exit;
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


function TTextParser.expectStrOrId (allowEmpty: Boolean=false): AnsiString;
begin
  case mTokType of
    TTStr:
      if (not allowEmpty) and (Length(mTokStr) = 0) then raise Exception.Create('non-empty string expected');
    TTId:
      begin end;
    else
      raise Exception.Create('string or identifier expected');
  end;
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
constructor TFileTextParser.Create (const fname: AnsiString);
begin
  mBuffer := nil;
  mFile := openDiskFileRO(fname);
  mStreamOwned := true;
  GetMem(mBuffer, BufSize);
  mBufPos := 0;
  mBufLen := mFile.Read(mBuffer^, BufSize);
  if (mBufLen < 0) then raise Exception.Create('TFileTextParser: read error');
  inherited Create();
end;


constructor TFileTextParser.Create (st: TStream; astOwned: Boolean=true);
begin
  if (st = nil) then raise Exception.Create('cannot create parser for nil stream');
  mFile := st;
  mStreamOwned := astOwned;
  GetMem(mBuffer, BufSize);
  mBufPos := 0;
  mBufLen := mFile.Read(mBuffer^, BufSize);
  if (mBufLen < 0) then raise Exception.Create('TFileTextParser: read error');
  inherited Create();
end;


destructor TFileTextParser.Destroy ();
begin
  if (mBuffer <> nil) then FreeMem(mBuffer);
  mBuffer := nil;
  mBufPos := 0;
  mBufLen := 0;
  if mStreamOwned then mFile.Free();
  mFile := nil;
  inherited;
end;


procedure TFileTextParser.loadNextChar ();
begin
  if (mBufLen = 0) then begin mNextChar := #0; exit; end;
  if (mBufPos >= mBufLen) then
  begin
    mBufLen := mFile.Read(mBuffer^, BufSize);
    if (mBufLen < 0) then raise Exception.Create('TFileTextParser: read error');
    if (mBufLen = 0) then begin mNextChar := #0; exit; end;
    mBufPos := 0;
  end;
  assert(mBufPos < mBufLen);
  mNextChar := mBuffer[mBufPos];
  Inc(mBufPos);
  if (mNextChar = #0) then mNextChar := ' ';
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TStrTextParser.Create (const astr: AnsiString);
begin
  mStr := astr;
  mPos := 1;
  inherited Create();
end;


destructor TStrTextParser.Destroy ();
begin
  mStr := '';
  inherited;
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
procedure TTextWriter.flush (); begin end;
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
  mFile := createDiskFile(fname);
  mStreamOwned := true;
  mBufUsed := 0;
  GetMem(mBuffer, BufSize);
  assert(mBuffer <> nil);
  inherited Create();
end;


constructor TFileTextWriter.Create (ast: TStream; astOwned: Boolean=true);
begin
  if (ast = nil) then raise Exception.Create('cannot write to nil stream');
  mFile := ast;
  mStreamOwned := astOwned;
  mBufUsed := 0;
  GetMem(mBuffer, BufSize);
  assert(mBuffer <> nil);
end;


destructor TFileTextWriter.Destroy ();
begin
  flush();
  if (mBuffer <> nil) then FreeMem(mBuffer);
  mBufUsed := 0;
  mBuffer := nil;
  if (mStreamOwned) then mFile.Free();
  mFile := nil;
  inherited;
end;


procedure TFileTextWriter.flush ();
begin
  if (mFile <> nil) and (mBufUsed > 0) then
  begin
    mFile.WriteBuffer(mBuffer^, mBufUsed);
  end;
  mBufUsed := 0;
end;


procedure TFileTextWriter.putBuf (constref buf; len: SizeUInt);
var
  pc: PChar;
  left: Integer;
begin
  if (len = 0) then exit;
  pc := @buf;
  while (len > 0) do
  begin
    left := BufSize-mBufUsed;
    if (left = 0) then
    begin
      flush();
      left := BufSize-mBufUsed;
      assert(left > 0);
    end;
    if (left > len) then left := Integer(len);
    Move(pc^, (mBuffer+mBufUsed)^, left);
    Inc(mBufUsed, left);
    pc += left;
    len -= left;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TStrTextWriter.Create ();
begin
  mStr := '';
end;


destructor TStrTextWriter.Destroy ();
begin
  mStr := '';
  inherited;
end;


procedure TStrTextWriter.putBuf (constref buf; len: SizeUInt);
var
  st: AnsiString = '';
begin
  if (len > 0) then
  begin
    SetLength(st, Integer(len));
    Move(buf, PChar(st)^, Integer(len));
    mStr += st;
    st := '';
  end;
end;


end.
