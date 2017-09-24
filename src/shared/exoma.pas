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
unit exoma;

interface

uses
  {$IFDEF USE_MEMPOOL}mempool,{$ENDIF}
  typinfo, SysUtils, Variants,
  hashtable, xparser;


// ////////////////////////////////////////////////////////////////////////// //
type
  TExomaException = class(Exception)
  public
    constructor Create (const amsg: AnsiString);
    constructor CreateFmt (const afmt: AnsiString; const args: array of const);
  end;

  TExomaParseException = class(TExomaException)
  public
    tokLine, tokCol: Integer;

  public
    constructor Create (pr: TTextParser; const amsg: AnsiString);
    constructor CreateFmt (pr: TTextParser; const afmt: AnsiString; const args: array of const);
  end;

// ////////////////////////////////////////////////////////////////////////// //
type
  TPropHash = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  private
    mClass: TClass;
    mNames: THashStrInt;
    pl: PPropList;
    pc: Integer;

  public
    constructor Create (aklass: TClass; const apfx: AnsiString='');
    destructor Destroy (); override;

    function get (obj: TObject; const fldname: AnsiString; out v: Variant): Boolean;
    function put (obj: TObject; const fldname: AnsiString; var v: Variant): Boolean;
  end;


// ////////////////////////////////////////////////////////////////////////// //
type
  TExprConstList = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  public
    function valid (const cname: AnsiString): Boolean; virtual; abstract;
    function get (const cname: AnsiString; out v: Variant): Boolean; virtual; abstract;
  end;


// ////////////////////////////////////////////////////////////////////////// //
type
  TExprScope = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  public
    class procedure error (const amsg: AnsiString);
    class procedure errorfmt (const afmt: AnsiString; const args: array of const);

    function getObj (const aname: AnsiString): TObject; virtual;
    function getField (obj: TObject; const afldname: AnsiString): Variant; virtual;
    procedure setField (obj: TObject; const afldname: AnsiString; var aval: Variant); virtual;
  end;

  TExprBase = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  public
    class function coerce2bool (var v0: Variant): Boolean;
    class function toInt (var v: Variant): LongInt;
  public
    class procedure error (const amsg: AnsiString);
    class procedure errorfmt (const afmt: AnsiString; const args: array of const);

    class procedure parseError (pr: TTextParser; const amsg: AnsiString);
    class procedure parseErrorFmt (pr: TTextParser; const afmt: AnsiString; const args: array of const);

    class function parse (clist: TExprConstList; pr: TTextParser; allowAssign: Boolean=false): TExprBase;
    class function parse (clist: TExprConstList; const str: AnsiString; allowAssign: Boolean=false): TExprBase;
    class function parseStatList (clist: TExprConstList; const str: AnsiString): TExprBase;

    class function isFloat (var v: Variant): Boolean; inline;
    class function isInt (var v: Variant): Boolean; inline;
    class function isBool (var v: Variant): Boolean; inline;
    class function isStr (var v: Variant): Boolean; inline;

  public
    function value (scope: TExprScope): Variant; virtual; abstract;
    procedure assign (scope: TExprScope; var v: Variant); virtual;
    function clone (): TExprBase; virtual; abstract;
  end;

  TExprStatList = class(TExprBase)
  private
    mList: array of TExprBase;
  public
    constructor Create ();
    destructor Destroy (); override;
    procedure append (e: TExprBase);
    function value (scope: TExprScope): Variant; override;
    function toString (): AnsiString; override;
    function clone (): TExprBase; override;
  end;

  TObjExpr = class(TExprBase)
  private
    mName: AnsiString;
  public
    constructor Create (const aval: AnsiString);

    function value (scope: TExprScope): Variant; override;
    function toString (): AnsiString; override;
    function clone (): TExprBase; override;
  end;

  TLitExpr = class(TExprBase)
  private
    mValue: Variant;
  public
    constructor Create (aval: Boolean);
    constructor Create (aval: LongInt);
    constructor Create (const aval: AnsiString);
    constructor Create (var v: Variant);

    function value (scope: TExprScope): Variant; override;
    function toString (): AnsiString; override;
    function clone (): TExprBase; override;
  end;

  TUnExpr = class(TExprBase)
  private
    mOp0: TExprBase;
  public
    constructor Create (aop0: TExprBase);
    destructor Destroy (); override;
    function clone (): TExprBase; override;
  end;

  TUnExprNeg = class(TUnExpr)
  public
    function value (scope: TExprScope): Variant; override;
    function toString (): AnsiString; override;
  end;

  TUnExprNot = class(TUnExpr)
  public
    function value (scope: TExprScope): Variant; override;
    function toString (): AnsiString; override;
  end;

  TDotExpr = class(TExprBase)
  private
    mOp0: TExprBase;
    mField: AnsiString;
  public
    constructor Create (aop0: TExprBase; const afield: AnsiString);
    function value (scope: TExprScope): Variant; override;
    procedure assign (scope: TExprScope; var v: Variant); override;
    function toString (): AnsiString; override;
    function clone (): TExprBase; override;
  end;

  TBinExpr = class(TExprBase)
  private
    mOp0, mOp1: TExprBase;
  private
    class procedure coerce (var v0, v1: Variant); // modifies both variants
  public
    constructor Create (aop0, aop1: TExprBase);
    destructor Destroy (); override;
    function clone (): TExprBase; override;
  end;

  TBinExprAdd = class(TBinExpr) public function value (scope: TExprScope): Variant; override; function toString (): AnsiString; override; end;
  TBinExprSub = class(TBinExpr) public function value (scope: TExprScope): Variant; override; function toString (): AnsiString; override; end;
  TBinExprMul = class(TBinExpr) public function value (scope: TExprScope): Variant; override; function toString (): AnsiString; override; end;
  TBinExprDiv = class(TBinExpr) public function value (scope: TExprScope): Variant; override; function toString (): AnsiString; override; end;
  TBinExprMod = class(TBinExpr) public function value (scope: TExprScope): Variant; override; function toString (): AnsiString; override; end;

  TBinExprLogAnd = class(TBinExpr) public function value (scope: TExprScope): Variant; override; function toString (): AnsiString; override; end;
  TBinExprLogOr = class(TBinExpr) public function value (scope: TExprScope): Variant; override; function toString (): AnsiString; override; end;

  TBinExprCmpLess = class(TBinExpr) public function value (scope: TExprScope): Variant; override; function toString (): AnsiString; override; end;
  TBinExprCmpGreat = class(TBinExpr) public function value (scope: TExprScope): Variant; override; function toString (): AnsiString; override; end;
  TBinExprCmpLessEqu = class(TBinExpr) public function value (scope: TExprScope): Variant; override; function toString (): AnsiString; override; end;
  TBinExprCmpGreatEqu = class(TBinExpr) public function value (scope: TExprScope): Variant; override; function toString (): AnsiString; override; end;
  TBinExprCmpEqu = class(TBinExpr) public function value (scope: TExprScope): Variant; override; function toString (): AnsiString; override; end;
  TBinExprCmpNotEqu = class(TBinExpr) public function value (scope: TExprScope): Variant; override; function toString (): AnsiString; override; end;

  TBinAssign = class(TBinExpr) public function value (scope: TExprScope): Variant; override; function toString (): AnsiString; override; end;

  TExprCond = class(TExprBase)
  private
    mCond, mTrue, mFalse: TExprBase;
  public
    constructor Create ();
    destructor Destroy (); override;
    function value (scope: TExprScope): Variant; override;
    function toString (): AnsiString; override;
    function clone (): TExprBase; override;
  end;


// ////////////////////////////////////////////////////////////////////////// //
function typeKind2Str (t: TTypeKind): AnsiString;


implementation

uses
  utils;


// ////////////////////////////////////////////////////////////////////////// //
constructor TExomaException.Create (const amsg: AnsiString);
begin
  inherited Create(amsg);
end;

constructor TExomaException.CreateFmt (const afmt: AnsiString; const args: array of const);
begin
  inherited Create(formatstrf(afmt, args));
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TExomaParseException.Create (pr: TTextParser; const amsg: AnsiString);
begin
  if (pr <> nil) then begin tokLine := pr.tokLine; tokCol := pr.tokCol; end else begin tokLine := 0; tokCol := 0; end;
  inherited Create(amsg);
end;

constructor TExomaParseException.CreateFmt (pr: TTextParser; const afmt: AnsiString; const args: array of const);
begin
  if (pr <> nil) then begin tokLine := pr.tokLine; tokCol := pr.tokCol; end else begin tokLine := 0; tokCol := 0; end;
  inherited Create(formatstrf(afmt, args));
end;


// ////////////////////////////////////////////////////////////////////////// //
function typeKind2Str (t: TTypeKind): AnsiString;
begin
  case t of
    tkUnknown: result := 'Unknown';
    tkInteger: result := 'Integer';
    tkChar: result := 'AnsiChar';
    tkEnumeration: result := 'Enumeration';
    tkFloat: result := 'Float';
    tkSet: result := 'Set';
    tkMethod: result := 'Method';
    tkSString: result := 'ShortString';
    tkLString: result := 'LString';
    tkAString: result := 'AnsiString';
    tkWString: result := 'WideString';
    tkVariant: result := 'Variant';
    tkArray: result := 'Array';
    tkRecord: result := 'Record';
    tkInterface: result := 'Interface';
    tkClass: result := 'Class';
    tkObject: result := 'Object';
    tkWChar: result := 'WideChar';
    tkBool: result := 'Boolean';
    tkInt64: result := 'Int64';
    tkQWord: result := 'UInt64';
    tkDynArray: result := 'DynArray';
    tkInterfaceRaw: result := 'InterfaceRaw';
    tkProcVar: result := 'ProcVar';
    tkUString: result := 'UString';
    tkUChar: result := 'UChar';
    tkHelper: result := 'Helper';
    tkFile: result := 'File';
    tkClassRef: result := 'ClassRef';
    tkPointer: result := 'Pointer';
    else result := '<unknown>';
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
(*
procedure dumpPublishedProperties (obj: TObject);
var
  pt: PTypeData;
  pi: PTypeInfo;
  i, j: Integer;
  pp: PPropList;
begin
  if (obj = nil) then exit;
  //e_LogWritefln('Object of type ''%s'':', [obj.ClassName]);
  pi := obj.ClassInfo;
  pt := GetTypeData(pi);
  //e_LogWritefln('property count: %s', [pt.PropCount]);
  GetMem(pp, pt^.PropCount*sizeof(Pointer));
  try
    j := GetPropList(pi, [tkInteger, tkBool, tkSString, tkLString, tkAString, tkSet, tkEnumeration], pp);
    //e_LogWritefln('ordinal property count: %s', [j]);
    for i := 0 to j-1 do
    begin
      {
      if (typinfo.PropType(obj, pp^[i].name) in [tkSString, tkLString, tkAString]) then
      begin
        e_LogWritefln('  #%s: <%s>; type: %s; value: <%s>', [i+1, pp^[i].name, typeKind2Str(typinfo.PropType(obj, pp^[i].name)), GetStrProp(obj, pp^[i])]);
      end
      else if (typinfo.PropType(obj, pp^[i].name) = tkSet) then
      begin
        e_LogWritefln('  #%s: <%s>; type: %s; value: %s', [i+1, pp^[i].name, typeKind2Str(typinfo.PropType(obj, pp^[i].name)), GetSetProp(obj, pp^[i], true)]);
      end
      else if (typinfo.PropType(obj, pp^[i].name) = tkEnumeration) then
      begin
        e_LogWritefln('  #%s: <%s>; type: %s; value: <%s>', [i+1, pp^[i].name, typeKind2Str(typinfo.PropType(obj, pp^[i].name)), GetEnumProp(obj, pp^[i])]);
      end
      else
      begin
        e_LogWritefln('  #%s: <%s>; type: %s; value: %s', [i+1, pp^[i].name, typeKind2Str(typinfo.PropType(obj, pp^[i].name)), GetOrdProp(obj, pp^[i])]);
      end;
      }
    end;
  finally
    FreeMem(pp);
  end;
end;
*)


// ////////////////////////////////////////////////////////////////////////// //
constructor TPropHash.Create (aklass: TClass; const apfx: AnsiString='');
var
  pi: PTypeInfo;
  pt: PTypeData;
  idx: Integer;
  n: AnsiString;
begin
  mClass := aklass;
  mNames := THashStrInt.Create();
  pi := aklass.ClassInfo;
  pt := GetTypeData(pi);
  GetMem(pl, pt^.PropCount*sizeof(Pointer));
  pc := GetPropList(pi, [tkInteger, tkBool, tkSString, tkLString, tkAString, {tkSet,} tkEnumeration], pl);
  for idx := 0 to pc-1 do
  begin
    if (Length(apfx) > 0) then
    begin
      if (Length(pl^[idx].name) < Length(apfx)) then continue;
      n := pl^[idx].name;
      if (Copy(n, 1, Length(apfx)) <> apfx) then continue;
      Delete(n, 1, Length(apfx));
      mNames.put(n, idx);
    end
    else
    begin
      mNames.put(pl^[idx].name, idx);
    end;
  end;
end;

destructor TPropHash.Destroy ();
begin
  mNames.Free();
  mNames := nil;
  if (pl <> nil) then FreeMem(pl);
  pl := nil;
  pc := 0;
  mClass := nil;
end;

function TPropHash.get (obj: TObject; const fldname: AnsiString; out v: Variant): Boolean;
var
  idx: Integer;
begin
  result := false;
  if mNames.get(fldname, idx) then
  begin
    result := true;
    case pl^[idx].PropType.Kind of
      tkSString, tkLString, tkAString: v := GetStrProp(obj, pl^[idx]);
      tkEnumeration: v := GetEnumProp(obj, pl^[idx]);
      tkBool: if (GetOrdProp(obj, pl^[idx]) = 0) then v := false else v := true;
      tkInteger, tkChar: v := LongInt(GetOrdProp(obj, pl^[idx]));
      //tkFloat: result := 'Float';
      //tkClass: result := 'Class';
      //tkInt64: result := 'Int64';
      //tkClassRef: result := 'ClassRef';
      else result := false;
    end;
    if result then exit;
  end;
  v := Unassigned;
end;

function TPropHash.put (obj: TObject; const fldname: AnsiString; var v: Variant): Boolean;
var
  idx: Integer;
begin
  result := false;
  if mNames.get(fldname, idx) then
  begin
    result := true;
    case pl^[idx].PropType.Kind of
      tkSString, tkLString, tkAString: SetStrProp(obj, pl^[idx], VarToStr(v));
      tkEnumeration: SetEnumProp(obj, pl^[idx], VarToStr(v));
      tkBool: if TExprBase.coerce2bool(v) then SetOrdProp(obj, pl^[idx], 1) else SetOrdProp(obj, pl^[idx], 0);
      tkInteger, tkChar: SetOrdProp(obj, pl^[idx], TExprBase.toInt(v));
      //tkFloat: result := 'Float';
      //tkClass: result := 'Class';
      //tkInt64: result := 'Int64';
      //tkClassRef: result := 'ClassRef';
      else result := false;
    end;
    if result then exit;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
class procedure TExprScope.error (const amsg: AnsiString); begin raise TExomaException.Create(amsg); end;
class procedure TExprScope.errorfmt (const afmt: AnsiString; const args: array of const); begin raise TExomaException.CreateFmt(afmt, args); end;

function TExprScope.getObj (const aname: AnsiString): TObject; begin result := nil; errorfmt('unknown object ''%s''', [aname]); end;
function TExprScope.getField (obj: TObject; const afldname: AnsiString): Variant; begin result := Unassigned; errorfmt('unknown field ''%s''', [afldname]); end;
procedure TExprScope.setField (obj: TObject; const afldname: AnsiString; var aval: Variant); begin errorfmt('unknown field ''%s''', [afldname]); end;


// ////////////////////////////////////////////////////////////////////////// //
class procedure TExprBase.error (const amsg: AnsiString); begin raise TExomaException.Create(amsg); end;
class procedure TExprBase.errorfmt (const afmt: AnsiString; const args: array of const); begin raise TExomaException.CreateFmt(afmt, args); end;

class procedure TExprBase.parseError (pr: TTextParser; const amsg: AnsiString); begin raise TExomaParseException.Create(pr, amsg); end;
class procedure TExprBase.parseErrorFmt (pr: TTextParser; const afmt: AnsiString; const args: array of const); begin raise TExomaParseException.CreateFmt(pr, afmt, args); end;

class function TExprBase.coerce2bool (var v0: Variant): Boolean;
begin
  case varType(v0) of
    varEmpty: result := false;
    varNull: result := false;
    varSingle: result := (Single(v0) <> 0.0);
    varDouble: result := (Double(v0) <> 0.0);
    varString: result := (Length(AnsiString(v0)) <> 0);
    varBoolean: result := Boolean(v0);
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord: result := (LongInt(v0) <> 0);
    varInt64: result := (Int64(v0) <> 0);
    varQWord: result := (UInt64(v0) <> 0);
    else begin result := false; error('can''t coerce type to boolean'); end;
  end;
end;

class function TExprBase.isFloat (var v: Variant): Boolean; inline;
begin
  case varType(v) of
    varSingle, varDouble: result := true;
    else result := false;
  end;
end;

class function TExprBase.isInt (var v: Variant): Boolean; inline;
begin
  case varType(v) of
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord: result := true;
    else result := false;
  end;
end;

class function TExprBase.isBool (var v: Variant): Boolean; inline;
begin
  result := (varType(v) = varBoolean);
end;

class function TExprBase.isStr (var v: Variant): Boolean; inline;
begin
  result := (varType(v) = varString);
end;

class function TExprBase.toInt (var v: Variant): LongInt;
begin
  case varType(v) of
    varSingle: result := trunc(Single(v));
    varDouble: result := trunc(Double(v));
    varBoolean: if Boolean(v) then result := 1 else result := 0;
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord: result := LongInt(v);
    varInt64: result := LongInt(Int64(v));
    else begin result := 0; TExprBase.error('can''t coerce type to integer'); end;
  end;
end;

procedure TExprBase.assign (scope: TExprScope; var v: Variant); begin error('not an lvalue'); end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TExprStatList.Create (); begin mList := nil; end;
destructor TExprStatList.Destroy (); var f: Integer; begin for f := 0 to High(mList) do mList[f].Free(); mList := nil; end;

procedure TExprStatList.append (e: TExprBase);
begin
  if (e <> nil) then
  begin
    SetLength(mList, Length(mList)+1);
    mList[High(mList)] := e;
  end;
end;

function TExprStatList.value (scope: TExprScope): Variant;
var
  f: Integer;
begin
  result := false;
  for f := 0 to High(mList) do result := mList[f].value(scope);
end;
function TExprStatList.toString (): AnsiString;
var
  f: Integer;
begin
  result := '';
  for f := 0 to High(mList) do result += mList[f].toString()+';';
end;
function TExprStatList.clone (): TExprBase;
var
  r: TExprStatList;
  f: Integer;
begin
  r := TExprStatList.Create();
  SetLength(r.mList, Length(mList));
  for f := 0 to High(mList) do r.mList[f] := nil;
  try
    for f := 0 to High(mList) do r.mList[f] := mList[f].clone();
  except
    r.Free();
  end;
  result := r;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TExprCond.Create (); begin mCond := nil; mTrue := nil; mFalse := nil; end;
destructor TExprCond.Destroy (); begin mFalse.Free(); mTrue.Free(); mCond.Free(); end;

function TExprCond.value (scope: TExprScope): Variant;
begin
  result := mCond.value(scope);
  if coerce2bool(result) then result := mTrue.value(scope) else result := mFalse.value(scope);
end;

function TExprCond.toString (): AnsiString; begin result := '('+mCond.toString()+'?'+mTrue.toString()+':'+mFalse.toString()+')'; end;

function TExprCond.clone (): TExprBase;
begin
  result := TExprCond.Create();
  TExprCond(result).mCond := mCond.clone();
  TExprCond(result).mTrue := mTrue.clone();
  TExprCond(result).mFalse := mFalse.clone();
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TObjExpr.Create (const aval: AnsiString); begin mName := aval; end;
function TObjExpr.value (scope: TExprScope): Variant; begin result := UInt64(PtrUInt(Pointer(scope.getObj(mName)))); end;
function TObjExpr.toString (): AnsiString; begin result := mName; end;
function TObjExpr.clone (): TExprBase; begin result := TObjExpr.Create(mName); end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TLitExpr.Create (aval: Boolean); begin mValue := aval; end;
constructor TLitExpr.Create (aval: LongInt); begin mValue := aval; end;
constructor TLitExpr.Create (const aval: AnsiString); begin mValue := aval; end;
constructor TLitExpr.Create (var v: Variant); begin mValue := v; end;
function TLitExpr.value (scope: TExprScope): Variant; begin result := mValue; end;
function TLitExpr.toString (): AnsiString; begin result := VarToStr(mValue); if isStr(mValue) then result := quoteStr(result); end;
function TLitExpr.clone (): TExprBase; begin result := TLitExpr.Create(0); (result as TLitExpr).mValue := mValue; end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TUnExpr.Create (aop0: TExprBase); begin mOp0 := aop0; end;
destructor TUnExpr.Destroy (); begin mOp0.Free(); inherited; end;
function TUnExpr.clone (): TExprBase; begin result := (self.ClassType.Create() as TUnExpr); (result as TUnExpr).mOp0 := mOp0.clone(); end;

function TUnExprNeg.value (scope: TExprScope): Variant;
begin
  result := mOp0.value(scope);
  case varType(result) of
    varSingle: result := -Single(result);
    varDouble: result := -Double(result);
    varShortInt, varSmallInt, varInteger, varByte, varWord: result := -LongInt(result);
    varInt64: result := -Int64(result);
    varLongWord: result := -LongInt(result);
    else error('can''t negate non-number');
  end;
end;

function TUnExprNeg.toString (): AnsiString; begin result := '-('+mOp0.toString()+')'; end;

function TUnExprNot.value (scope: TExprScope): Variant;
begin
  result := mOp0.value(scope);
  result := not coerce2bool(result);
end;

function TUnExprNot.toString (): AnsiString; begin result := '!('+mOp0.toString()+')'; end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TDotExpr.Create (aop0: TExprBase; const afield: AnsiString);
begin
  mOp0 := aop0;
  mField := afield;
end;

function TDotExpr.value (scope: TExprScope): Variant;
begin
  result := mOp0.value(scope);
  if (varType(result) <> varQWord) then errorfmt('can''t take field ''%s'' value of non-object', [mField]);
  result := scope.getField(TObject(PtrUInt(UInt64(result))), mField);
end;

procedure TDotExpr.assign (scope: TExprScope; var v: Variant);
var
  o: Variant;
begin
  o := mOp0.value(scope);
  if (varType(o) <> varQWord) then errorfmt('can''t assign value to field ''%s'' of non-object', [mField]);
  scope.setField(TObject(PtrUInt(UInt64(o))), mField, v);
end;

function TDotExpr.clone (): TExprBase; begin result := TDotExpr.Create(mOp0, mField); end;

function TDotExpr.toString (): AnsiString; begin result := mOp0.toString()+'.'+mField; end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TBinExpr.Create (aop0, aop1: TExprBase); begin mOp0 := aop0; mOp1 := aop1; end;
destructor TBinExpr.Destroy (); begin mOp1.Free(); mOp0.Free(); inherited; end;
function TBinExpr.clone (): TExprBase; begin result := (self.ClassType.Create() as TBinExpr); (result as TBinExpr).mOp0 := mOp0.clone(); (result as TBinExpr).mOp1 := mOp1.clone(); end;

class procedure TBinExpr.coerce (var v0, v1: Variant);
begin
  if (varType(v0) <> varType(v1)) then
  begin
    if isStr(v0) or isStr(v1) then
    begin
           if isFloat(v0) then v0 := formatstrf('%s', [Double(v0)])
      else if isInt(v0) then v0 := formatstrf('%s', [LongInt(v0)])
      else if isBool(v0) then v0 := formatstrf('%s', [Boolean(v0)])
      else if isStr(v0) then begin end
      else error('can''t coerce value to string');
           if isFloat(v1) then v1 := formatstrf('%s', [Double(v1)])
      else if isInt(v1) then v1 := formatstrf('%s', [LongInt(v1)])
      else if isBool(v1) then v1 := formatstrf('%s', [Boolean(v1)])
      else if isStr(v0) then begin end
      else error('can''t coerce value to string');
    end
    else if isFloat(v0) or isFloat(v1) then
    begin
           if isFloat(v0) or isInt(v0) then v0 := Double(v0)
      else if isBool(v0) then begin if Boolean(v0) then v0 := Double(1.0) else v0 := Double(0.0); end
      else error('can''t coerce value to float');
           if isFloat(v1) or isInt(v1) then v1 := Double(v1)
      else if isBool(v1) then begin if Boolean(v1) then v1 := Double(1.0) else v1 := Double(0.0); end
      else error('can''t coerce value to float');
    end
    else if isInt(v0) or isInt(v1) then
    begin
           if isBool(v0) then begin if Boolean(v0) then v0 := LongInt(1) else v0 := LongInt(0); end
      else if isFloat(v0) then v0 := LongInt(trunc(Double(v0)))
      else if isInt(v0) then begin end
      else error('can''t coerce value to integer');
           if isBool(v1) then begin if Boolean(v1) then v1 := LongInt(1) else v1 := LongInt(0); end
      else if isFloat(v1) then v1 := LongInt(trunc(Double(v1)))
      else if isInt(v1) then begin end
      else error('can''t coerce value to integer');
    end
    else
    begin
      error('can''t operate with value of invalid type');
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBinExprAdd.value (scope: TExprScope): Variant;
var
  r1: Variant;
begin
  result := mOp0.value(scope);
  r1 := mOp1.value(scope);
  coerce(result, r1);
  case varType(result) of
    varSingle, varDouble: result := Double(result)+Double(r1);
    varString: result := AnsiString(result)+AnsiString(r1);
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord: result := LongInt(result)+LongInt(r1);
    varInt64: result := Int64(result)+Int64(r1);
    else error('can''t add non-numbers and non-strings');
  end;
end;
function TBinExprAdd.toString (): AnsiString; begin result := '('+mOp0.toString()+'+'+mOp1.toString+')'; end;

function TBinExprSub.value (scope: TExprScope): Variant;
var
  r1: Variant;
begin
  result := mOp0.value(scope);
  r1 := mOp1.value(scope);
  coerce(result, r1);
  case varType(result) of
    varSingle, varDouble: result := Double(result)-Double(r1);
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord: result := LongInt(result)-LongInt(r1);
    varInt64: result := Int64(result)-Int64(r1);
    else error('can''t subtract non-numbers');
  end;
end;
function TBinExprSub.toString (): AnsiString; begin result := '('+mOp0.toString()+'-'+mOp1.toString+')'; end;

function TBinExprMul.value (scope: TExprScope): Variant;
var
  r1: Variant;
begin
  result := mOp0.value(scope);
  r1 := mOp1.value(scope);
  coerce(result, r1);
  case varType(result) of
    varSingle, varDouble: result := Double(result)*Double(r1);
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord: result := LongInt(result)*LongInt(r1);
    varInt64: result := Int64(result)*Int64(r1);
    else error('can''t multiply non-numbers');
  end;
end;
function TBinExprMul.toString (): AnsiString; begin result := '('+mOp0.toString()+'*'+mOp1.toString+')'; end;

function TBinExprDiv.value (scope: TExprScope): Variant;
var
  r1: Variant;
begin
  result := mOp0.value(scope);
  r1 := mOp1.value(scope);
  coerce(result, r1);
  case varType(result) of
    varSingle, varDouble: result := Double(result)/Double(r1);
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord: result := LongInt(result) div LongInt(r1);
    varInt64: result := Int64(result) div Int64(r1);
    else error('can''t divide non-numbers');
  end;
end;
function TBinExprDiv.toString (): AnsiString; begin result := '('+mOp0.toString()+'/'+mOp1.toString+')'; end;

function TBinExprMod.value (scope: TExprScope): Variant;
var
  r1: Variant;
begin
  result := mOp0.value(scope);
  r1 := mOp1.value(scope);
  coerce(result, r1);
  case varType(result) of
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord: result := LongInt(result) mod LongInt(r1);
    varInt64: result := Int64(result) mod Int64(r1);
    else error('can''t do modulo on non-numbers');
  end;
end;
function TBinExprMod.toString (): AnsiString; begin result := '('+mOp0.toString()+'%'+mOp1.toString+')'; end;

function TBinExprLogAnd.value (scope: TExprScope): Variant;
begin
  result := mOp0.value(scope);
  if not coerce2bool(result) then begin result := false; exit; end;
  result := mOp1.value(scope);
  result := coerce2bool(result);
end;
function TBinExprLogAnd.toString (): AnsiString; begin result := '('+mOp0.toString()+'&&'+mOp1.toString+')'; end;

function TBinExprLogOr.value (scope: TExprScope): Variant;
begin
  result := mOp0.value(scope);
  if coerce2bool(result) then begin result := true; exit; end;
  result := mOp1.value(scope);
  result := coerce2bool(result);
end;
function TBinExprLogOr.toString (): AnsiString; begin result := '('+mOp0.toString()+'||'+mOp1.toString+')'; end;

function TBinExprCmpLess.value (scope: TExprScope): Variant;
var
  r1: Variant;
begin
  result := mOp0.value(scope);
  r1 := mOp1.value(scope);
  coerce(result, r1);
  case varType(result) of
    varSingle, varDouble: result := Boolean(Double(result) < Double(r1));
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord: result := Boolean(LongInt(result) < LongInt(r1));
    varInt64: result := Boolean(Int64(result) < Int64(r1));
    varString: result := Boolean(AnsiString(result) < AnsiString(r1));
    else error('can''t compare non-numbers and non-strings');
  end;
end;
function TBinExprCmpLess.toString (): AnsiString; begin result := '('+mOp0.toString()+'<'+mOp1.toString+')'; end;

function TBinExprCmpGreat.value (scope: TExprScope): Variant;
var
  r1: Variant;
begin
  result := mOp0.value(scope);
  r1 := mOp1.value(scope);
  coerce(result, r1);
  case varType(result) of
    varSingle, varDouble: result := Boolean(Double(result) > Double(r1));
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord: result := Boolean(LongInt(result) > LongInt(r1));
    varInt64: result := Boolean(Int64(result) > Int64(r1));
    varString: result := Boolean(AnsiString(result) > AnsiString(r1));
    else error('can''t compare non-numbers and non-strings');
  end;
end;
function TBinExprCmpGreat.toString (): AnsiString; begin result := '('+mOp0.toString()+'>'+mOp1.toString+')'; end;

function TBinExprCmpLessEqu.value (scope: TExprScope): Variant;
var
  r1: Variant;
begin
  result := mOp0.value(scope);
  r1 := mOp1.value(scope);
  coerce(result, r1);
  case varType(result) of
    varSingle, varDouble: result := Boolean(Double(result) <= Double(r1));
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord: result := Boolean(LongInt(result) <= LongInt(r1));
    varInt64: result := Boolean(Int64(result) <= Int64(r1));
    varString: result := Boolean(AnsiString(result) <= AnsiString(r1));
    else error('can''t compare non-numbers and non-strings');
  end;
end;
function TBinExprCmpLessEqu.toString (): AnsiString; begin result := '('+mOp0.toString()+'<='+mOp1.toString+')'; end;

function TBinExprCmpGreatEqu.value (scope: TExprScope): Variant;
var
  r1: Variant;
begin
  result := mOp0.value(scope);
  r1 := mOp1.value(scope);
  coerce(result, r1);
  case varType(result) of
    varSingle, varDouble: result := Boolean(Double(result) >= Double(r1));
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord: result := Boolean(LongInt(result) >= LongInt(r1));
    varInt64: result := Boolean(Int64(result) >= Int64(r1));
    varString: result := Boolean(AnsiString(result) >= AnsiString(r1));
    else error('can''t compare non-numbers and non-strings');
  end;
end;
function TBinExprCmpGreatEqu.toString (): AnsiString; begin result := '('+mOp0.toString()+'>='+mOp1.toString+')'; end;

function TBinExprCmpEqu.value (scope: TExprScope): Variant;
var
  r1: Variant;
begin
  result := mOp0.value(scope);
  r1 := mOp1.value(scope);
  coerce(result, r1);
  case varType(result) of
    varSingle, varDouble: result := Boolean(Double(result) = Double(r1));
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord: result := Boolean(LongInt(result) = LongInt(r1));
    varInt64: result := Boolean(Int64(result) = Int64(r1));
    varString: result := Boolean(AnsiString(result) = AnsiString(r1));
    varBoolean: result := (Boolean(result) = Boolean(r1));
    varQWord: result := (UInt64(result) = UInt64(r1));
    else error('can''t compare non-numbers and non-strings');
  end;
end;
function TBinExprCmpEqu.toString (): AnsiString; begin result := '('+mOp0.toString()+'=='+mOp1.toString+')'; end;

function TBinExprCmpNotEqu.value (scope: TExprScope): Variant;
var
  r1: Variant;
begin
  result := mOp0.value(scope);
  r1 := mOp1.value(scope);
  coerce(result, r1);
  case varType(result) of
    varSingle, varDouble: result := Boolean(Double(result) <> Double(r1));
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord: result := Boolean(LongInt(result) <> LongInt(r1));
    varInt64: result := Boolean(Int64(result) <> Int64(r1));
    varString: result := Boolean(AnsiString(result) <> AnsiString(r1));
    varBoolean: result := (Boolean(result) <> Boolean(r1));
    varQWord: result := (UInt64(result) <> UInt64(r1));
    else error('can''t compare non-numbers and non-strings');
  end;
end;
function TBinExprCmpNotEqu.toString (): AnsiString; begin result := '('+mOp0.toString()+'<>'+mOp1.toString+')'; end;


// ////////////////////////////////////////////////////////////////////////// //
function TBinAssign.value (scope: TExprScope): Variant;
begin
  result := mOp1.value(scope);
  mOp0.assign(scope, result);
end;

function TBinAssign.toString (): AnsiString; begin result := mOp0.toString()+'='+mOp1.toString(); end;


// ////////////////////////////////////////////////////////////////////////// //
class function TExprBase.parse (clist: TExprConstList; const str: AnsiString; allowAssign: Boolean=false): TExprBase;
var
  pr: TTextParser;
begin
  pr := TStrTextParser.Create(str);
  try
    result := parse(clist, pr, allowAssign);
    if (pr.tokType <> pr.TTEOF) then begin result.Free(); parseError(pr, 'extra code in expression'); end;
  finally
    pr.Free();
  end;
end;

class function TExprBase.parseStatList (clist: TExprConstList; const str: AnsiString): TExprBase;
var
  pr: TTextParser = nil;
  r: TExprStatList = nil;
  e: TExprBase = nil;
begin
  pr := TStrTextParser.Create(str);
  if (pr.tokType = pr.TTEOF) then begin pr.Free(); result := nil; exit; end;
  r := TExprStatList.Create();
  result := nil;
  try
    try
      while true do
      begin
        while pr.eatDelim(';') do begin end;
        if (pr.tokType = pr.TTEOF) then break;
        e := parse(clist, pr, true);
        if (e = nil) then break;
        //writeln(': ', e.toString());
        r.append(e);
        if (pr.tokType = pr.TTEOF) then break;
        //writeln('tt=', pr.tokType, ' <', pr.tokStr, '>');
        //writeln(r.toString());
        pr.expectDelim(';');
      end;
      result := r;
      r := nil;
    except
      on e: TExomaException do
        raise TExomaParseException.Create(pr, e.message);
      on e: Exception do
        raise TExomaParseException.Create(pr, e.message);
      else
        raise;
    end;
  finally
    r.Free();
    pr.Free();
  end;
end;


class function TExprBase.parse (clist: TExprConstList; pr: TTextParser; allowAssign: Boolean=false): TExprBase;

  function expr (): TExprBase; forward;

  function doTerm (): TExprBase;
  var
    id: AnsiString;
    v: Variant;
  begin
    result := nil;
    try
      if pr.eatDelim('(') then begin result := expr(); pr.expectDelim(')'); exit; end;
      if pr.eatDelim('!') then begin result := doTerm(); result := TUnExprNot.Create(result); exit; end;
      if pr.eatDelim('+') then begin result := doTerm(); exit; end;
      if pr.eatDelim('-') then begin result := doTerm(); result := TUnExprNeg.Create(result); exit; end;
      if (pr.tokType = pr.TTInt) then begin result := TLitExpr.Create(pr.expectInt()); exit; end;
      if (pr.tokType = pr.TTStr) then begin result := TLitExpr.Create(pr.expectStr(true)); exit; end;
      if (pr.tokType = pr.TTId) then
      begin
        if (pr.tokStr = 'true') then begin result := TLitExpr.Create(true); pr.skipToken(); exit; end;
        if (pr.tokStr = 'false') then begin result := TLitExpr.Create(false); pr.skipToken(); exit; end;
        if (CompareText(pr.tokStr, 'true') = 0) or (CompareText(pr.tokStr, 'false') = 0) then parseError(pr, '`true` and `false` are case-sensitive');
        id := pr.expectId();
        if (clist <> nil) then
        begin
          if clist.get(id, v) then
          begin
            result := TLitExpr.Create(v);
            exit;
          end;
          if not clist.valid(id) then parseErrorFmt(pr, 'unknown identifier ''%s''', [id]);
        end;
        result := TObjExpr.Create(id);
        while (pr.tokType = pr.TTDelim) and (pr.tokChar = '.') do
        begin
          pr.skipToken();
          result := TDotExpr.Create(result, pr.expectId());
        end;
        exit;
      end;
    except
      result.Free();
      raise;
    end;
    parseError(pr, 'invalid term');
  end;

  function doMulDiv (): TExprBase;
  begin
    result := doTerm();
    try
      while true do
      begin
             if pr.eatDelim('*') then result := TBinExprMul.Create(result, doTerm())
        else if pr.eatDelim('/') then result := TBinExprDiv.Create(result, doTerm())
        else if pr.eatDelim('%') then result := TBinExprMod.Create(result, doTerm())
        else break;
      end;
    except
      result.Free();
      raise;
    end;
  end;

  function doPlusMinus (): TExprBase;
  begin
    result := doMulDiv();
    try
      while true do
      begin
             if pr.eatDelim('+') then result := TBinExprAdd.Create(result, doMulDiv())
        else if pr.eatDelim('-') then result := TBinExprSub.Create(result, doMulDiv())
        else break;
      end;
    except
      result.Free();
      raise;
    end;
  end;

  function doCmp (): TExprBase;
  begin
    result := doPlusMinus();
    try
      while true do
      begin
             if pr.eatDelim('<') then result := TBinExprCmpLess.Create(result, doPlusMinus())
        else if pr.eatDelim('>') then result := TBinExprCmpGreat.Create(result, doPlusMinus())
        else if pr.eatTT(pr.TTLessEqu) then result := TBinExprCmpLessEqu.Create(result, doPlusMinus())
        else if pr.eatTT(pr.TTGreatEqu) then result := TBinExprCmpGreatEqu.Create(result, doPlusMinus())
        else break;
      end;
    except
      result.Free();
      raise;
    end;
  end;

  function doCmpEqu (): TExprBase;
  begin
    result := doCmp();
    try
      while true do
      begin
             if pr.eatTT(pr.TTEqu) then result := TBinExprCmpEqu.Create(result, doCmp())
        else if pr.eatTT(pr.TTNotEqu) then result := TBinExprCmpNotEqu.Create(result, doCmp())
        else break;
      end;
    except
      result.Free();
      raise;
    end;
  end;

  function doLogAnd (): TExprBase;
  begin
    result := doCmpEqu();
    try
      while true do
      begin
        if pr.eatTT(pr.TTLogAnd) then result := TBinExprLogAnd.Create(result, doCmpEqu()) else break;
      end;
    except
      result.Free();
      raise;
    end;
  end;

  function doLogOr (): TExprBase;
  begin
    result := doLogAnd();
    try
      while true do
      begin
        if pr.eatTT(pr.TTLogOr) then result := TBinExprLogOr.Create(result, doLogAnd()) else break;
      end;
    except
      result.Free();
      raise;
    end;
  end;

  // funcall, [], dot
  // !, ~
  // *, /, %
  // +, -
  // <<, >>
  // <, <=, >, >=
  // ==, !=
  // &
  // ^
  // |
  // &&
  // ||

  function expr0 (): TExprBase;
  var
    neg: Boolean;
    e: TExprBase = nil;
    list: TExprStatList = nil;
  begin
    result := nil;
    try
      while true do
      begin
             if pr.eatDelim('-') then neg := true
        else if pr.eatDelim('+') then neg := false
        else neg := false;
        e := doLogOr();
        if neg then e := TUnExprNeg.Create(e);
        if allowAssign and pr.eatDelim('=') then e := TBinAssign.Create(e, expr());
        if not pr.eatDelim(',') then
        begin
          if (result = nil) then result := e else list.append(e);
          break;
        end;
        //assert(false);
        if (list = nil) then
        begin
          list := TExprStatList.Create();
          result := list;
        end;
        list.append(e);
        e := nil;
      end;
    except
      e.Free();
      list.Free();
    end;
  end;

  function expr (): TExprBase;
  var
    c: TExprCond;
  begin
    result := expr0();
    // ternary
    if pr.eatDelim('?') then
    begin
      c := TExprCond.Create();
      c.mCond := result;
      try
        c.mTrue := expr();
        pr.expectDelim(':');
        c.mFalse := expr();
        result := c;
      except
        c.Free();
      end;
    end;
  end;

var
  oas: TTextParser.TOptions;
begin
  if (pr = nil) or (pr.tokType = pr.TTEOF) then begin result := nil; exit; end;
  oas := pr.options;
  try
    pr.options := pr.options-[pr.TOption.SignedNumbers];
    try
      result := expr();
    finally
      pr.options := oas;
    end;
  except
    on e: TExomaException do
      raise TExomaParseException.Create(pr, e.message);
    on e: Exception do
      raise TExomaParseException.Create(pr, e.message);
    else
      raise;
  end;
end;


{
    varEmpty:
    varNull:
    varSingle:
    varDouble:
    varDecimal:
    varCurrency:
    varDate:
    varOleStr:
    varStrArg:
    varString:
    varDispatch:
    varBoolean:
    varVariant:
    varUnknown:
    varShortInt:
    varSmallint:
    varInteger:
    varInt64:
    varByte:
    varWord:
    varLongWord:
    varQWord:
    varError:
}
end.
