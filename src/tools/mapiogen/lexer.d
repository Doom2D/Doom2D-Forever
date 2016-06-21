/* coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
 * Understanding is not required. Only obedience.
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
module lexer;
static assert(__VERSION__ >= 2071, "you need as least DMD 2.071 to compile this code");


// ////////////////////////////////////////////////////////////////////////// //
static if (!is(typeof(usize))) private alias usize = size_t;


// ////////////////////////////////////////////////////////////////////////// //
public struct Loc {
  string file;
  int line, col;
  uint tpos;

  string toString () const { import std.string : format; return "%s (%s,%s)".format(file, line, col); }
  string toStringNoFile () const { import std.string : format; return "(%s,%s)".format(line, col); }

  @property bool valid () const pure nothrow @safe @nogc { pragma(inline, true); return (line > 0 && col > 0); }
}


// ////////////////////////////////////////////////////////////////////////// //
public class ErrorAt : Exception {
  Loc loc;

  this (string msg, Throwable next=null, string file=__FILE__, usize line=__LINE__) pure nothrow @safe @nogc { super(msg, file, line, next); }
  this (in Loc aloc, string msg, Throwable next=null, string file=__FILE__, usize line=__LINE__) pure nothrow @safe @nogc { loc = aloc; super(msg, file, line, next); }
}


// ////////////////////////////////////////////////////////////////////////// //
public struct Token {
public:
  enum Type {
    EOF = -1,
    Id,
    Str,
    Num,
    Spec,
  }

private:
  const(char)[] tkstr;

public:
  Loc loc, eloc; // token start, token end (after last char)
  Type type = Type.EOF; // token type
  long num; // should be enough for everyone

@safe:
  void mustbeType (Token.Type tp, string msg="identifier expected", string file=__FILE__, usize line=__LINE__) {
    pragma(inline, true);
    if (type != tp) throw new ErrorAt(loc, msg, null, file, line);
  }
  void mustbeId (string msg="identifier expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); mustbeType(Type.Id, msg, file, line); }
  void mustbeStr (string msg="string expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); mustbeType(Type.Str, msg, file, line); }
  void mustbeNum (string msg="number expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); mustbeType(Type.Num, msg, file, line); }
  void mustbeSpec (string msg="punctuation expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); mustbeType(Type.Spec, msg, file, line); }

  string toString () const @trusted {
    import std.string : format;
    final switch (type) with (Type) {
      case EOF: return "(%s,%d): <EOF>".format(loc.line, loc.col);
      case Id: return "(%s,%d): Id:%s".format(loc.line, loc.col, tkstr);
      case Str: return "(%s,%d): Str:%s".format(loc.line, loc.col, Lexer.quote(tkstr));
      case Num: return "(%s,%d): Num:%s".format(loc.line, loc.col, num);
      case Spec: return "(%s,%d): Spec:<%s>".format(loc.line, loc.col, tkstr);
    }
    assert(0);
  }

nothrow:
  // get immutable string
  // this converts id to `string` via `.idup`, use with caution!
  // `.idup` is used to not anchor the whole source string
  @property string istr () const { pragma(inline, true); return (tkstr.length ? tkstr.idup : null); }

const pure nothrow @nogc @property:
  const(char)[] str () { pragma(inline, true); return tkstr; }
  bool isId () { pragma(inline, true); return (type == Type.Id); }
  bool isStr () { pragma(inline, true); return (type == Type.Str); }
  bool isNum () { pragma(inline, true); return (type == Type.Num); }
  bool isSpec () { pragma(inline, true); return (type == Type.Spec); }
  bool isEOF () { pragma(inline, true); return (type == Type.EOF); }
}


// ////////////////////////////////////////////////////////////////////////// //
public final class Lexer {
private:
  const(char)[] text;
  uint tpos;
  Loc cpos; // position for last `getChar()`
  Loc pend; // end of previous token, for better error messages
  bool eof;
  bool lastWasEOL = true;
  Token[] lookup;
  Token tokeof; // will be fixed by `nextToken()`

public:
  this(T) (const(char)[] atext, T afname=null) if (is(T : const(char)[])) {
    text = atext;
    if (afname.length > 0) { static if (is(T == string)) cpos.file = afname; else cpos.file = afname.idup; }
    tokeof.loc.file = cpos.file;
    nextToken();
    pend.line = 1;
    pend.col = 1;
    pend.tpos = 0;
  }

  void error (string msg, string file=__FILE__, usize line=__LINE__) {
    pragma(inline, true);
    throw new ErrorAt((lookup.length == 0 ? loc : lookup[0].loc), msg, null, file, line);
  }

  static private void error (in ref Token tk, string msg, string file=__FILE__, usize line=__LINE__) {
    pragma(inline, true);
    throw new ErrorAt(tk.loc, msg, null, file, line);
  }

  static private void error() (in auto ref Loc loc, string msg, string file=__FILE__, usize line=__LINE__) {
    pragma(inline, true);
    throw new ErrorAt(loc, msg, null, file, line);
  }

  const(char)[] line (uint idx) {
    if (idx == 0) ++idx;
    uint pos = 0;
    while (--idx > 0) {
      while (pos < text.length && text.ptr[pos] != '\n') ++pos;
      ++pos;
    }
    if (pos >= text.length) return null;
    uint epos = pos;
    while (epos < text.length && text.ptr[epos] != '\n') ++epos;
    while (epos > pos && text.ptr[epos-1] <= ' ') --epos;
    return text[pos..epos];
  }

  void popFront () {
    if (lookup.length > 0) {
      pend = lookup.ptr[0].eloc;
      ++pend.col; // for better error messages
      ++pend.tpos; // to be consistent
      foreach (immutable idx; 1..lookup.length) lookup.ptr[idx-1] = lookup.ptr[idx];
      lookup.length -= 1;
      lookup.assumeSafeAppend;
    }
    nextToken();
  }

  @property pure nothrow @safe @nogc {
    bool empty () const { pragma(inline, true); return (lookup.length == 0); }
    ref inout(Token) front () inout { pragma(inline, true); return (lookup.length ? lookup.ptr[0] : tokeof); }
    // current token's loc
    auto loc () inout { pragma(inline, true); return front.loc; }
    auto eloc () inout { pragma(inline, true); return front.eloc; }
    auto peloc () inout { pragma(inline, true); return pend; }

    bool isId () const { pragma(inline, true); return front.isId; }
    bool isStr () const { pragma(inline, true); return front.isStr; }
    bool isNum () const { pragma(inline, true); return front.isNum; }
    bool isSpec () const { pragma(inline, true); return front.isSpec; }
  }

  // this eats identifier
  void expect (const(char)[] id, string file=__FILE__, usize line=__LINE__) {
    if (!front.isId || front.str != id) error(loc, "`"~id.idup~"` expected", file, line);
    popFront();
  }

  // this eats identifier
  void expectCI (const(char)[] id, string file=__FILE__, usize line=__LINE__) {
    if (front.isId && id.length == front.str.length) {
      bool ok = true;
      foreach (immutable idx, char ch; front.str) {
        if (ch >= 'A' && ch <= 'Z') ch += 32; // poor man's `tolower()`
        char c1 = id[idx];
        if (c1 >= 'A' && c1 <= 'Z') c1 += 32; // poor man's `tolower()`
        if (ch != c1) { ok = false; break; }
      }
      if (ok) { popFront(); return; }
    }
    error(loc, "`"~id.idup~"` expected", file, line);
  }

  auto expectSpec (string msg="punctuation expected", string file=__FILE__, usize line=__LINE__) {
    mustbeSpec(msg, file, line);
    auto res = lookup[0].str;
    popFront();
    return res;
  }

  // this converts id to `string` via `.idup`, use with caution!
  // `.idup` is used to not anchor the whole source string
  string expectId (string msg="identifier expected", string file=__FILE__, usize line=__LINE__) {
    mustbeId(msg, file, line);
    auto res = lookup[0].istr;
    popFront();
    return res;
  }

  // this converts id to `string` via `.idup`, use with caution!
  // `.idup` is used to not anchor the whole source string
  string expectStr (string msg="string expected", string file=__FILE__, usize line=__LINE__) {
    //pragma(inline, true);
    mustbeStr(msg, file, line);
    auto res = lookup[0].istr;
    popFront();
    return res;
  }

  // `mustbe` doesn't eat token
  void mustbeType (Token.Type tp, string msg="identifier expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); return front.mustbeType(tp, msg, file, line); }
  void mustbeId (string msg="identifier expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); return front.mustbeId(msg, file, line); }
  void mustbeStr (string msg="string expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); return front.mustbeStr(msg, file, line); }
  void mustbeNum (string msg="number expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); return front.mustbeNum(msg, file, line); }
  void mustbeSpec (string msg="punctuation expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); return front.mustbeSpec(msg, file, line); }

  bool eat (const(char)[] id) {
    if (front.isId && front.str == id) { popFront(); return true; }
    return false;
  }

  const(char)[] eatCI (const(char)[] id) {
    if (front.isId && id.length == front.str.length) {
      bool ok = true;
      foreach (immutable idx, char ch; front.str) {
        if (ch >= 'A' && ch <= 'Z') ch += 32; // poor man's `tolower()`
        char c1 = id[idx];
        if (c1 >= 'A' && c1 <= 'Z') c1 += 32; // poor man's `tolower()`
        if (ch != c1) { ok = false; break; }
      }
      if (ok) { auto res = front.str; popFront(); return res; }
    }
    return null;
  }

  ref Token peek (uint dist) {
    while (!eof && lookup.length <= dist) nextToken();
    return (dist < lookup.length ? lookup.ptr[dist] : tokeof);
  }

  ref Token opIndex (usize dist) { pragma(inline, true); return peek(dist); }

  // return loc for next `getChar()`
  Loc nextLoc () nothrow @safe @nogc {
    Loc res = cpos;
    if (lastWasEOL) { ++res.line; res.col = 1; } else ++res.col;
    return res;
  }

  char peekChar (uint dist=0) nothrow @trusted @nogc {
    pragma(inline, true);
    return (tpos+dist >= text.length ? '\0' : (text.ptr[tpos+dist] ? text.ptr[tpos+dist] : ' '));
  }

  // return char or 0
  char getChar () nothrow @trusted @nogc {
    if (tpos >= text.length) { tpos = text.length; eof = true; }
    if (eof) return '\0';
    cpos.tpos = tpos;
    char ch = text.ptr[tpos++];
    if (ch == '\0') ch = ' ';
    if (lastWasEOL) { ++cpos.line; cpos.col = 1; } else ++cpos.col;
    lastWasEOL = (ch == '\n');
    return ch;
  }

  // skip blanks and comments
  //TODO: make special "comment" token(s)?
  void skipBlanks () @safe {
    for (;;) {
      char ch = peekChar;
      if (ch == '/' && peekChar(1) == '/') {
        // single-line comment
        do { ch = getChar(); } while (ch != 0 && ch != '\n');
        continue;
      } else if (ch == '(' && peekChar(1) == '*') {
        getChar(); // skip starting char
        auto lc = cpos;
        getChar(); // skip star
        char pch = ' ';
        ch = ' '; // we need this
        for (;;) {
          pch = ch;
          ch = getChar();
          if (ch == 0) error(lc, "unterminated comment");
          if (ch == ')' && pch == '*') break;
        }
        continue;
      } else if (ch == '{') {
        getChar(); // skip starting char
        auto lc = cpos;
        do {
          ch = getChar();
          if (ch == 0) error(lc, "unterminated comment");
        } while (ch != '}');
        continue;
      }
      if (ch == 0 || ch > 32) return;
      getChar();
    }
  }

  private void nextToken () {
    if (eof) return;

    skipBlanks();
    if (peekChar == '\0') {
      eof = true;
      tokeof.loc = cpos;
      tokeof.eloc = cpos;
      return;
    }

    Token tk;
    auto tkspos = tpos;
    char ch = getChar();
    tk.loc = cpos;

    // quoted string
    if (ch == '"' || ch == '\'') {
      char ech = ch;
      tk.type = Token.Type.Str;
      ++tkspos; // skip quote
      for (;;) {
        ch = getChar();
        if (ch == 0) error(tk, "unterminated string");
        if (ch == ech) break;
      }
      tk.tkstr = text[tkspos..tpos-1]; // -1 due to eaten quote
      tk.eloc = cpos;
      lookup ~= tk;
      return;
    }

    // hex number
    if (ch == '$') {
      long n = 0;
      tk.type = Token.Type.Num;
      getChar(); // skip dollar
      int dv = digitValue(peekChar);
      if (dv < 0 || dv > 15) error(tk, "hex number expected");
      for (;;) {
        dv = digitValue(peekChar);
        if (dv < 0 || dv > 15) break;
        n = n*16+dv;
        getChar();
      }
      ch = peekChar;
      if (isIdChar(ch) || ch == '.') error(tk, "hex number expected");
      tk.num = n;
      tk.tkstr = text[tkspos..tpos];
      tk.eloc = cpos;
      lookup ~= tk;
      return;
    }

    // number
    if (isDigit(ch)) {
      long n = ch-'0';
      tk.type = Token.Type.Num;
      for (;;) {
        if (!isDigit(peekChar)) break;
        ch = getChar();
        n = n*10+ch-'0';
      }
      tk.num = n;
      tk.tkstr = text[tkspos..tpos];
      tk.eloc = cpos;
      ch = peekChar;
      if (isIdChar(ch)) error(tk, "invalid number");
      lookup ~= tk;
      return;
    }

    // identifier
    if (isIdStart(ch)) {
      tk.type = Token.Type.Id;
      while (isIdChar(peekChar)) getChar();
      tk.tkstr = text[tkspos..tpos];
      tk.eloc = cpos;
      lookup ~= tk;
      return;
    }

    static immutable string[9] longSpecs = [
      "<=",
      ">=",
      ":=",
      "<>",
      "+=",
      "-=",
      "*=",
      "/=",
      "..",
    ];
    enum MaxSpecLength = {
      int ml = 0;
      foreach (string s; longSpecs) if (s.length > ml) ml = cast(int)s.length;
      return ml;
    }();

    // delimiter
    char[MaxSpecLength] dbuf;
    dbuf[0] = ch;
    uint len = 0;
    for (;;) {
      ch = dbuf[len];
      bool found = false;
      foreach (string s; longSpecs) if (len < s.length && s[len] == ch) { found = true; break; }
      if (!found) break;
      if (len > 0) getChar(); // this char should be eaten
      if (++len >= MaxSpecLength) break;
      dbuf[len] = peekChar(0);
    }
    tk.type = Token.Type.Spec;
    tk.tkstr = text[tkspos..tpos];
    tk.eloc = cpos;
    lookup ~= tk;
  }

  auto select(RetType, string mode="peek", A...) (scope A args) { pragma(inline, true); return selectN!(RetType, mode)(0, args); }

  auto selectN(RetType, string mode="peek", A...) (usize n, scope A args) {
    import std.traits : ReturnType;

    static assert(mode == "peek" || mode == "pop" || mode == "pop-nondefault", "selectN: invalid mode: '"~mode~"'");

    template isGoodDg(usize idx, T) {
      private import std.traits;
      static if (idx < A.length && isCallable!(A[idx]) && arity!(args[idx]) == 1) {
        enum isGoodDg = is(Parameters!(A[idx])[0] == T);
      } else {
        enum isGoodDg = false;
      }
    }

    template isGoodArglessDg(usize idx) {
      private import std.traits;
      static if (idx < A.length && isCallable!(A[idx]) && arity!(args[idx]) == 0) {
        enum isGoodArglessDg = true;
      } else {
        enum isGoodArglessDg = false;
      }
    }

    // sorry, but this has to be string mixin, due to possible empty `arg`
    enum DoCallDg(string arg) =
      "static if (!is(ReturnType!(A[xidx]) == void)) return cast(RetType)(args[xidx]("~arg~")); else { args[xidx]("~arg~"); return RetType.init; }";

    // we can't have inner mixin templates, so... sorry, it's string again
    enum CallDg = q{
           static if (isGoodDg!(xidx, Token)) { mixin(DoCallDg!"tk"); }
      else static if (isGoodDg!(xidx, Loc)) { mixin(DoCallDg!"tk.loc"); }
      else static if (isGoodDg!(xidx, Token.Type)) { mixin(DoCallDg!"tk.type"); }
      else static if (isGoodDg!(xidx, Keyword)) { mixin(DoCallDg!"tk.Kw"); }
      else static if (isGoodArglessDg!(xidx)) { mixin(DoCallDg!""); }
      else static assert(0, "selectN: invalid delegate #"~xidx.stringof);
    };

    auto tk = peek(n);
    bool found = false;
    foreach (immutable aidx, immutable arg; args) {
      static if (aidx%2 == 0) {
        static if (is(typeof(arg) == Keyword) || is(typeof(arg) == Token.Type)) {
               static if (is(typeof(arg) == Keyword)) found = (tk == arg);
          else static if (is(typeof(arg) == Token.Type)) found = (tk.type == arg);
          else static assert(0, "wtf?!");
          if (found) {
            // process `mode`
            static if (mode != "peek") popFront();
            // call delegate
            enum xidx = aidx+1;
            mixin(CallDg);
          }
        } else {
          // default
          // process `mode`
          static if (mode == "pop") popFront();
          // call delegate
          enum xidx = aidx;
          mixin(CallDg);
        }
      }
    }
    error(tk, "selectN is out of nodes");
    assert(0);
  }

static:
  private immutable byte[256] digitValues = {
    byte[256] res = -1;
    foreach (ubyte idx; '0'..'9'+1) res[idx] = cast(byte)(idx-'0');
    foreach (ubyte idx; 'A'..'Z'+1) res[idx] = cast(byte)(idx-'A'+10);
    foreach (ubyte idx; 'a'..'z'+1) res[idx] = cast(byte)(idx-'a'+10);
    return res;
  }();

  private immutable bool[256] idStartChars = {
    bool[256] res = false;
    foreach (ubyte idx; 'A'..'Z'+1) res[idx] = true;
    foreach (ubyte idx; 'a'..'z'+1) res[idx] = true;
    res['_'] = true;
    return res;
  }();

  private immutable bool[256] idChars = {
    bool[256] res = false;
    foreach (ubyte idx; '0'..'9'+1) res[idx] = true;
    foreach (ubyte idx; 'A'..'Z'+1) res[idx] = true;
    foreach (ubyte idx; 'a'..'z'+1) res[idx] = true;
    res['_'] = true;
    return res;
  }();

  bool isDigit() (char ch) { pragma(inline, true); return (ch >= '0' && ch <= '9'); }
  int digitValue() (char ch) { pragma(inline, true); return digitValues.ptr[cast(ubyte)ch]; }
  bool isIdStart() (char ch) { pragma(inline, true); return idStartChars.ptr[cast(ubyte)ch]; }
  bool isIdChar() (char ch) { pragma(inline, true); return idChars.ptr[cast(ubyte)ch]; }

  string gmlQuote (const(char)[] s) {
    import std.array : appender;
    auto res = appender!string();
    enum Prev { Nothing, Char, Spec }
    Prev prev = Prev.Nothing;
    foreach (char ch; s) {
      if (ch < ' ' || ch == 127 || ch == '"') {
        import std.conv : to;
        final switch (prev) with (Prev) {
          case Nothing: break;
          case Char: res.put(`"+`); break;
          case Spec: res.put(`+`); break;
        }
        prev = Prev.Spec;
        res.put("chr(");
        res.put(to!string(cast(uint)ch));
        res.put(")");
      } else {
        final switch (prev) with (Prev) {
          case Nothing: res.put('"'); break;
          case Char: break;
          case Spec: res.put(`+"`); break;
        }
        prev = Prev.Char;
        res.put(ch);
      }
    }
    if (prev == Prev.Nothing) return `""`;
    if (prev == Prev.Char) res.put('"');
    return res.data;
  }

  /// quote string: append double quotes, screen all special chars;
  /// so quoted string forms valid D string literal.
  /// allocates.
  string quote (const(char)[] s) {
    import std.array : appender;
    import std.format : formatElement, FormatSpec;
    auto res = appender!string();
    FormatSpec!char fspc; // defaults to 's'
    formatElement(res, s, fspc);
    return res.data;
  }
}


version(lexer_test) unittest {
  import std.file;
  import std.stdio;
  //enum FName = "z00.txt";
  enum FName = "shared/MAPDEF.pas";
  string s;
  {
    auto fl = File(FName);
    auto buf = new char[](cast(uint)fl.size);
    fl.rawRead(buf[]);
    s = cast(string)buf;
  }
  auto lex = new Lexer(s, FName);
  try {
    while (!lex.empty) {
      writeln(lex.front);
      lex.popFront();
    }
  } catch (ErrorAt e) {
    writeln("PARSE ERROR: ", e.line);
    writeln(e.loc);
  }
}
