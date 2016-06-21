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
module mapiogen;
static assert(__VERSION__ >= 2071, "you need as least DMD 2.071 to compile this code");

import std.stdio;

import lexer;


// ////////////////////////////////////////////////////////////////////////// //
bool useDelphiAlignment = false;
ubyte[string] triggers;
string[ubyte] trignums;


// ////////////////////////////////////////////////////////////////////////// //
struct Field {
  enum Type { Bytes, Chars, Integral, TPoint, Boolean }

  string name;
  string typename;
  uint size;
  uint ofs;
  Type type;
}

struct Record {
  string[] ids; // null: default
  Field[] fields;
  uint size; // max size
  bool normal;

  string getRWName () const {
    import std.string : capitalize;
    if (ids.length == 0) return "Default";
    if (normal) return ids[0].capitalize;
    return ids[0][8..$].capitalize;
  }

  // calc field offsets and record size
  void finalize (bool packed=false) {
    // calculate offsets
    uint ofs = 0;
    foreach (immutable idx, ref fld; fields) {
      fld.ofs = ofs;
      // delphi does this (roughly)
      if (!packed && fld.size != 1) {
        if (useDelphiAlignment && fld.type == Field.Type.TPoint) {
        } else {
          //ubyte pd = (fld.size > 4 ? 2 : fld.size > 2 ? 4 : 2);
          ubyte pd = (fld.size > 2 ? 4 : 2);
          if (fld.type == Field.Type.Chars && fld.size > 1) pd = 2;
          fld.ofs += ofs.padding(pd);
        }
      }
      ofs = fld.ofs+fld.size;
    }
    size = ofs;
    //if (fields.length > 0 && fields[$-1].size != 1 && fields[$-1].type == Field.Type.Integral) size += size.padding(2); // just in case
  }
}


Record[] tgrecords;
Record[] records;


// ////////////////////////////////////////////////////////////////////////// //
//  0 128  Default (Byte128)
void dumpRecord (in ref Record rec) {
  foreach (const ref fld; rec.fields) {
    writefln("%3s %3s  %s (%s)", fld.ofs, fld.size, fld.name, fld.typename);
  }
}


void dumpRecords () { foreach (const ref rec; tgrecords) rec.dumpRecord(); }


// ////////////////////////////////////////////////////////////////////////// //
void genMisc (File fo) {
  fo.write(
q{procedure getBytesAt (var dest; const buf; ofs, len: Integer);
begin
  Move((PChar(@buf)+ofs)^, dest, len);
end;

procedure getWordAt (var dest; const buf; ofs: Integer);
type PWord = ^Word; PByte = ^Byte;
var
  p: PByte;
  d: PWord;
begin
  p := PByte(@buf); Inc(p, ofs);
  d := PWord(@dest);
  d^ := p^;
  Inc(p);
  d^ := (d^) or ((p^) shl 8);
end;

procedure getIntAt (var dest; const buf; ofs: Integer);
type PInt = ^LongWord; PByte = ^Byte;
var
  p: PByte;
  d: PInt;
begin
  p := PByte(@buf); Inc(p, ofs);
  d := PInt(@dest);
  d^ := p^;
  Inc(p);
  d^ := (d^) or ((p^) shl 8);
  Inc(p);
  d^ := (d^) or ((p^) shl 16);
  Inc(p);
  d^ := (d^) or ((p^) shl 24);
end;

procedure putBytesAt (var buf; ofs: Integer; const src; len: Integer);
begin
  Move(src, (PChar(@buf)+ofs)^, len);
end;

procedure putWordAt (var buf; ofs: Integer; const src);
type PWord = ^Word; PByte = ^Byte;
var
  p: PByte;
  d: PWord;
begin
  p := PByte(PChar(@buf)+ofs);
  d := PWord(@src);
  p^ := (d^) and $ff;
  Inc(p);
  p^ := ((d^) shr 8) and $ff;
end;

procedure putIntAt (var buf; ofs: Integer; const src);
type PInt = ^LongWord; PByte = ^Byte;
var
  p: PByte;
  d: PInt;
begin
  p := PByte(PChar(@buf)+ofs);
  d := PInt(@src);
  p^ := (d^) and $ff;
  Inc(p);
  p^ := ((d^) shr 8) and $ff;
  Inc(p);
  p^ := ((d^) shr 16) and $ff;
  Inc(p);
  p^ := ((d^) shr 24) and $ff;
end;

});
}


void genReader (File fo, in ref Record rec) {
  fo.write(
    "  procedure xread", rec.getRWName, " ();\n"~
    "  begin\n"
  );
  foreach (const ref fld; rec.fields) {
    final switch (fld.type) {
      case Field.Type.Bytes:
      case Field.Type.Chars:
      case Field.Type.Boolean:
        fo.writeln("    getBytesAt(tr.", fld.name, ", buf, ", fld.ofs, ", ", fld.size, ");");
        break;
      case Field.Type.Integral:
        switch (fld.size) {
          case 1: fo.writeln("    getBytesAt(tr.", fld.name, ", buf, ", fld.ofs, ", ", fld.size, ");"); break;
          case 2: fo.writeln("    getWordAt(tr.", fld.name, ", buf, ", fld.ofs, ");"); break;
          case 4: fo.writeln("    getIntAt(tr.", fld.name, ", buf, ", fld.ofs, ");"); break;
          default: assert(0);
        }
        break;
      case Field.Type.TPoint:
        fo.writeln("    getIntAt(tr.", fld.name, ".x, buf, ", fld.ofs, ");");
        fo.writeln("    getIntAt(tr.", fld.name, ".y, buf, ", fld.ofs+4, ");");
        break;
    }
  }
  fo.writeln("  end;\n");
}


void genReaders (File fo) {
  fo.write(
    "procedure mb_Read_TriggerData (var tr: TTriggerData; ttype: Integer; const buf; bufsize: Integer);\n"
  );
  uint maxsize = 0;
  foreach (const ref rec; tgrecords) {
    if (rec.ids.length == 0) continue;
    if (rec.size > maxsize) maxsize = rec.size;
    fo.genReader(rec);
  }
  fo.write(
    "begin\n"~
    "  if (bufsize < ", maxsize, ") then raise Exception.Create('invalid buffer size in mb_Read_TriggerData');\n"
  );
  foreach (const ref rec; tgrecords) {
    foreach (string id; rec.ids) {
      fo.writeln("  if (ttype = ", id, ") then begin xread", rec.getRWName, "(); exit; end;");
    }
  }
  fo.writeln("  raise Exception.Create('invalid trigger type in mb_Read_TriggerData');");
  fo.writeln("end;\n\n");
  foreach (ref rec; records) {
    assert(rec.normal);
    fo.writeln("procedure mb_Read_", rec.ids[0], " (var tr: ", rec.ids[0], "; const buf; bufsize: Integer);");
    fo.genReader(rec);
    fo.write(
      "begin\n"~
      "  if (bufsize < ", rec.size, ") then raise Exception.Create('invalid buffer size in read", rec.ids[0], "');\n"
      "  xread", rec.getRWName, "();\n"~
      "end;\n\n"
    );
  }
}


void genWriter (File fo, in ref Record rec) {
  fo.write(
    "  procedure xwrite", rec.getRWName, " ();\n"~
    "  begin\n"
  );
  foreach (const ref fld; rec.fields) {
    final switch (fld.type) {
      case Field.Type.Bytes:
      case Field.Type.Chars:
      case Field.Type.Boolean:
        fo.writeln("    putBytesAt(buf, ", fld.ofs, ", tr.", fld.name, ", ", fld.size, ");");
        break;
      case Field.Type.Integral:
        switch (fld.size) {
          case 1: fo.writeln("    putBytesAt(buf, ", fld.ofs, ", tr.", fld.name, ", ", fld.size, ");"); break;
          case 2: fo.writeln("    putWordAt(buf, ", fld.ofs, ", tr.", fld.name, ");"); break;
          case 4: fo.writeln("    putIntAt(buf, ", fld.ofs, ", tr.", fld.name, ");"); break;
          default: assert(0);
        }
        break;
      case Field.Type.TPoint:
        fo.writeln("    putIntAt(buf, ", fld.ofs  , ", tr.", fld.name, ".x);");
        fo.writeln("    putIntAt(buf, ", fld.ofs+4, ", tr.", fld.name, ".y);");
        break;
    }
  }
  fo.writeln("  end;\n");
}


void genWriters (File fo) {
  fo.write(
    "procedure mb_Write_TriggerData (var buf; bufsize: Integer; ttype: Integer; var tr: TTriggerData);\n"
  );
  uint maxsize = 0;
  foreach (const ref rec; tgrecords) {
    assert(!rec.normal);
    if (rec.ids.length == 0) continue;
    if (rec.size > maxsize) maxsize = rec.size;
    fo.genWriter(rec);
  }
  fo.write(
    "begin\n"~
    "  if (bufsize < ", maxsize, ") then raise Exception.Create('invalid buffer size in mb_Write_TriggerData');\n"
  );
  foreach (const ref rec; tgrecords) {
    foreach (string id; rec.ids) {
      fo.writeln("  if (ttype = ", id, ") then begin xwrite", rec.getRWName, "(); exit; end;");
    }
  }
  fo.writeln("  raise Exception.Create('invalid trigger type in mb_Write_TriggerData');");
  fo.writeln("end;\n\n");
  foreach (ref rec; records) {
    assert(rec.normal);
    fo.writeln("procedure mb_Write_", rec.ids[0], " (var buf; bufsize: Integer; var tr: ", rec.ids[0], ");");
    fo.genWriter(rec);
    fo.write(
      "begin\n"~
      "  if (bufsize < ", rec.size, ") then raise Exception.Create('invalid buffer size in write", rec.ids[0], "');\n"
      "  xwrite", rec.getRWName, "();\n"~
      "end;\n\n"
    );
  }
}


// ////////////////////////////////////////////////////////////////////////// //
void printCaret (Lexer lex, Loc loc, File ofile=stdout) {
  auto line = lex.line(loc.line);
  if (line.length == 0) return;
  ofile.writeln(line);
  foreach (immutable _; 1..loc.col) ofile.write(' ');
  ofile.writeln('^');
}


// ////////////////////////////////////////////////////////////////////////// //
ubyte padding (uint size, ubyte alg) {
  uint nsz = (size+alg-1)/alg*alg;
  return cast(ubyte)(nsz-size);
}


// ////////////////////////////////////////////////////////////////////////// //
void parseType (ref Field fld, const(char)[] typestr, Lexer lex) {
  import std.algorithm : startsWith;
  import std.string : toLower;
  auto type = typestr.toLower;
  if (type.startsWith("byte") || type.startsWith("char")) {
    import std.conv : to;
    fld.type = (type[0] == 'b' ? Field.Type.Bytes : Field.Type.Chars);
    if (type.length == 4) {
      fld.size = 1;
      return;
    }
    try {
      auto sz = to!uint(type[4..$]);
      if (sz < 1 || sz > 32767) throw new Exception("invalid size");
      fld.size = sz;
      return;
    } catch (Exception) {}
  } else if (type == "tpoint") {
    fld.type = Field.Type.TPoint;
    fld.size = 4*2;
    return;
  } else if (type == "boolean") {
    fld.type = Field.Type.Boolean;
    fld.size = 1;
    return;
  } else if (type == "integer") {
    fld.type = Field.Type.Integral;
    fld.size = 4;
    return;
  } else if (type == "word") {
    fld.type = Field.Type.Integral;
    fld.size = 2;
    return;
  } else if (type == "shortint") {
    fld.type = Field.Type.Integral;
    fld.size = 1;
    return;
  }
  lex.error("invalid type: '"~typestr.idup~"'");
}


/*
(TargetPoint: TPoint;
 d2d_teleport: Boolean;
 silent_teleport: Boolean;
 TlpDir: Byte);
*/
Field[] parseFields (Lexer lex) {
  Field[] res;
  if (!lex.isSpec || lex.front.str != "(") lex.error("'(' expected");
  lex.popFront();
  for (;;) {
    if (lex.isSpec && lex.front.str == ")") { lex.popFront(); break; }
    string[] names;
    for (;;) {
      names ~= lex.expectId();
      if (lex.isSpec && lex.front.str == ":") break;
      if (lex.isSpec && lex.front.str == ",") { lex.popFront(); continue; }
      lex.error("':' expected");
    }
    if (!lex.isSpec || lex.front.str != ":") lex.error("':' expected");
    lex.popFront();
    auto type = lex.expectId();
    //writeln("  ", names[], ": <", type, ">");
    foreach (string name; names) {
      Field fld;
      fld.name = name;
      fld.typename = type;
      fld.parseType(type, lex);
      res ~= fld;
    }
    if (!lex.isSpec) lex.error("';' or ')' expected");
    if (lex.front.str == ";") { lex.popFront(); continue; }
    if (lex.front.str != ")") lex.error("';' or ')' expected");
  }
  if (lex.isSpec && lex.front.str == ";") lex.popFront();
  return res;
}


/*
TargetPoint: TPoint;
 d2d_teleport: Boolean;
 silent_teleport: Boolean;
 TlpDir: Byte;
 end;
*/
Field[] parseRecFields (Lexer lex) {
  Field[] res;
  for (;;) {
    if (lex.eatCI("end") !is null) break;
    string[] names;
    for (;;) {
      names ~= lex.expectId();
      if (lex.isSpec && lex.front.str == ":") break;
      if (lex.isSpec && lex.front.str == ",") { lex.popFront(); continue; }
      lex.error("':' expected");
    }
    if (!lex.isSpec || lex.front.str != ":") lex.error("':' expected");
    lex.popFront();
    auto type = lex.expectId();
    foreach (string name; names) {
      Field fld;
      fld.name = name;
      fld.typename = type;
      fld.parseType(type, lex);
      res ~= fld;
    }
    if (lex.eatCI("end") !is null) break;
    if (!lex.isSpec || lex.front.str != ";") lex.error("';' expected");
    lex.popFront();
  }
  return res;
}


// ////////////////////////////////////////////////////////////////////////// //
bool isGoodTriggerName (const(char)[] id) {
  import std.algorithm : startsWith;
  import std.string : indexOf;
  if (!id.startsWith("TRIGGER_")) return false;
  if (id == "TRIGGER_MAX") return false;
  if (id[8..$].indexOf('_') >= 0) return false;
  return true;
}


// ////////////////////////////////////////////////////////////////////////// //
void parseMapDef (string fname) {
  import std.string : format, toLower, toUpper;
  Lexer lex;
  {
    auto fl = File(fname);
    auto buf = new char[](cast(uint)fl.size);
    fl.rawRead(buf[]);
    lex = new Lexer(cast(string)buf, fname);
  }
  // find "interface"
  while (!lex.empty) {
    if (!lex.front.isId) { lex.popFront(); continue; }
    if (lex.front.str.toLower == "interface") break;
    lex.popFront();
  }
  if (lex.empty) throw new Exception("where is my interface?!");
  enum Section { Unknown, Const, Type }
  Section section;
  while (!lex.empty) {
    if (lex.front.isId) {
      auto kw = lex.front.str.toLower;
      if (kw == "implementation") break;
      if (kw == "const") {
        //writeln("CONST!");
        section = Section.Const;
        lex.popFront();
        continue;
      }
      if (kw == "type") {
        //writeln("TYPE!");
        section = Section.Type;
        lex.popFront();
        continue;
      }
    }
    if (section == Section.Const) {
      if (!lex.isId) lex.error("identifier expected");
      auto id = lex.front.istr.toUpper;
      lex.popFront();
      auto lc = lex.loc;
      if (lex.expectSpec() != "=") lex.error(lc, "'=' expected");
      if (isGoodTriggerName(id)) {
        lex.mustbeNum();
        auto lcn = lex.loc;
        auto n = lex.front.num;
        lex.popFront();
        if (n < 0 || n > 255) lex.error(lcn, "invalid value (%s) for '%s'".format(n, id));
        auto b = cast(ubyte)n;
        if (id in triggers) lex.error(lc, "duplicate constant '%s'".format(id));
        if (auto tg = b in trignums) lex.error(lcn, "same value (%s) for triggers '%s' and '%s'".format(n, id, *tg));
        triggers[id] = b;
        trignums[b] = id;
        //writeln("trigger: ", id, " (", b, ")");
      } else {
        while (!lex.empty) {
          if (lex.front.isSpec && lex.front.str == ";") break;
          lex.popFront();
        }
      }
      lc = lex.loc;
      if (lex.expectSpec() != ";") lex.error(lc, "';' expected");
      continue;
    }
    if (section == Section.Type) {
      if (!lex.isId) lex.error("identifier expected");
      auto id = lex.front.istr.toUpper;
      lex.popFront();
      auto lc = lex.loc;
      if (lex.expectSpec() != "=") lex.error(lc, "'=' expected");
      //writeln("id: ", id);
      if (id != "TTRIGGERDATA") {
        // skip definition
        while (!lex.empty) {
          if (lex.eatCI("end") !is null) break;
          lex.popFront();
        }
        lc = lex.loc;
        if (lex.expectSpec() != ";") lex.error(lc, "';' expected");
        continue;
      } else {
        lex.expectCI("record");
        lex.expectCI("case");
        lex.expectCI("byte");
        lex.expectCI("of");
        // now parse defs
        for (;;) {
          if (lex.eatCI("end") !is null) break;
          string[] ids;
          Field[] fields;
          if (lex.isNum) {
            if (lex.front.num != 0) lex.error(lc, "'0' expected");
            lex.popFront();
            if (!lex.isSpec || lex.front.str != ":") lex.error("':' expected");
            lex.popFront();
            //writeln("=== DEFAULT ===");
            ids = null;
            fields = lex.parseFields();
          } else {
            for (;;) {
              ids ~= lex.expectId();
              if (lex.isSpec && lex.front.str == ":") { lex.popFront(); break; }
              if (lex.isSpec && lex.front.str == ",") { lex.popFront(); continue; }
              lex.error("',' or ':' expected");
            }
            //writeln("=== ", ids[], " ===");
            fields = lex.parseFields();
          }
          tgrecords ~= Record(ids, fields);
          tgrecords[$-1].finalize;
          //writeln("=== ", ids[], " === : ", rcsize);
        }
        lc = lex.loc;
        if (lex.expectSpec() != ";") lex.error(lc, "';' expected");
        break; // we are done
      }
    }
    lex.popFront();
    continue;
  }
}


// ////////////////////////////////////////////////////////////////////////// //
void parseMapStruct (string fname) {
  import std.string : format, toLower, toUpper;

  static bool isGoodRecName (const(char)[] id) {
    import std.algorithm : startsWith, endsWith;
    import std.string : indexOf, toUpper;
    id = id.toUpper;
    if (!id.startsWith("T") || !id.endsWith("_1")) return false;
    return true;
  }

  static bool isGoodDef (Lexer lex) {
    import std.string : toLower;
    auto tk = lex.peek(1);
    if (!tk.isSpec || tk.str != "=") return false;
    tk = lex.peek(2);
    if (!tk.isId || tk.str.toLower != "packed") return false;
    tk = lex.peek(3);
    if (!tk.isId || tk.str.toLower != "record") return false;
    return true;
  }

  Lexer lex;
  {
    auto fl = File(fname);
    auto buf = new char[](cast(uint)fl.size);
    fl.rawRead(buf[]);
    lex = new Lexer(cast(string)buf, fname);
  }
  // find "interface"
  while (!lex.empty) {
    if (!lex.front.isId) { lex.popFront(); continue; }
    if (lex.front.str.toLower == "interface") break;
    lex.popFront();
  }
  if (lex.empty) throw new Exception("where is my interface?!");
  enum Section { Unknown, Type }
  Section section;
  while (!lex.empty) {
    if (lex.front.isId) {
      auto kw = lex.front.str.toLower;
      if (kw == "implementation") break;
      if (kw == "type") {
        section = Section.Type;
        lex.popFront();
        continue;
      }
    }
    if (section == Section.Type) {
      if (lex.isId && isGoodRecName(lex.front.str) && isGoodDef(lex)) {
        string origId = lex.front.istr;
        lex.popFront();
        lex.popFront(); // skip "="
        lex.expectCI("packed");
        lex.expectCI("record");
        // now parse fields
        Record rec;
        rec.ids ~= origId;
        rec.fields = lex.parseRecFields();
        rec.normal = true;
        rec.finalize(true);
        records ~= rec;
        {
          auto lc = lex.loc;
          if (lex.expectSpec() != ";") lex.error(lc, "';' expected");
        }
        continue;
      }
    }
    lex.popFront();
    continue;
  }
}


// ////////////////////////////////////////////////////////////////////////// //
void main () {
  try {
    parseMapDef("../../shared/MAPDEF.pas");
    parseMapStruct("../../shared/MAPSTRUCT.pas");
    debug {
      dumpRecords();
    } else {
      {
        auto fo = File("mapstructio.inc", "w");
        fo.genMisc();
        fo.genReaders();
        fo.genWriters();
      }
      {
        auto fo = File("mapstructsizes.inc", "w");
        fo.writeln("const");
        foreach (ref rec; records) fo.writeln("  SizeOf_", rec.ids[0], " = ", rec.size, ";");
        fo.writeln();
        fo.writeln("procedure mb_Read_TriggerData (var tr: TTriggerData; ttype: Integer; const buf; bufsize: Integer);");
        fo.writeln("procedure mb_Write_TriggerData (var buf; bufsize: Integer; ttype: Integer; var tr: TTriggerData);");
        foreach (ref rec; records) {
          fo.writeln("procedure mb_Read_", rec.ids[0], " (var tr: ", rec.ids[0], "; const buf; bufsize: Integer);");
          fo.writeln("procedure mb_Write_", rec.ids[0], " (var buf; bufsize: Integer; var tr: ", rec.ids[0], ");");
        }
      }
    }
  } catch (ErrorAt e) {
    writeln("PARSE ERROR: ", e.loc, ": ", e.msg);
    //lex.printCaret(e.loc);
  }
}
