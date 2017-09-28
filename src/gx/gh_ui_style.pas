(* coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
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
 *)
{$INCLUDE ../../shared/a_modes.inc}
unit gh_ui_style;

interface

uses
  SysUtils, Classes,
  glgfx,
  xstreams, xparser, utils, hashtable;


type
  TStyleValue = packed record
  public
    type TType = (Empty, Bool, Int, Color);

  public
    constructor Create (v: Boolean; okToInherit: Boolean=true);
    constructor Create (v: Integer; okToInherit: Boolean=true);
    constructor Create (ar, ag, ab: Integer; okToInherit: Boolean=true);
    constructor Create (ar, ag, ab, aa: Integer; okToInherit: Boolean=true);
    constructor Create (const v: TGxRGBA; okToInherit: Boolean=true);

    function isEmpty (): Boolean; inline;
    function canInherit (): Boolean; inline;

    function toString (): AnsiString;
    function asRGBA: TGxRGBA; inline;
    function asRGBADef (const def: TGxRGBA): TGxRGBA; inline;
    function asIntDef (const def: Integer): Integer; inline;
    function asBoolDef (const def: Boolean): Boolean; inline;

  public
    vtype: TType;
    allowInherit: Boolean;
    case TType of
      TType.Bool: (bval: Boolean);
      TType.Int: (ival: Integer);
      TType.Color: (r, g, b, a: Byte);
  end;

  TStyleSection = class;

  THashStrStyleVal = specialize THashBase<AnsiString, TStyleValue, THashKeyStrAnsiCI>;
  THashStrSection = specialize THashBase<AnsiString, TStyleSection, THashKeyStrAnsiCI>;

  TStyleSection = class
  private
    mVals: THashStrStyleVal;
    mHashVals: THashStrSection; // "#..."
    mCtlVals: THashStrSection;

  private
    // "text-color#inactive@label"
    function getValue (const path: AnsiString): TStyleValue;
    procedure putValue (const path: AnsiString; const val: TStyleValue);

  public
    constructor Create ();
    destructor Destroy (); override;

  public
    property value[const path: AnsiString]: TStyleValue read getValue write putValue; default;
  end;

  TUIStyle = class
  private
    mId: AnsiString; // style name ('default', for example)
    mMain: TStyleSection;

  private
    procedure parse (par: TTextParser);

    function getValue (const path: AnsiString): TStyleValue; inline;
    procedure putValue (const path: AnsiString; const val: TStyleValue); inline;

  public
    constructor Create (const aid: AnsiString);
    constructor Create (st: TStream); // parse from stream
    constructor CreateFromFile (const fname: AnsiString);
    destructor Destroy (); override;

  public
    property id: AnsiString read mId;
    property value[const path: AnsiString]: TStyleValue read getValue write putValue; default;
  end;


procedure uiLoadStyles (const fname: AnsiString);
procedure uiLoadStyles (st: TStream);

// will return "default" (or raise an exception if there is no "default")
function uiFindStyle (const stname: AnsiString): TUIStyle;


implementation


// ////////////////////////////////////////////////////////////////////////// //
var
  styles: array of TUIStyle = nil;


function createDefaultStyle (): TUIStyle;
begin
  result := TUIStyle.Create('default');

  result['back-color'] := TStyleValue.Create(TGxRGBA.Create(0, 0, 128));
  result['text-color'] := TStyleValue.Create(TGxRGBA.Create(255, 255, 255));
  result['frame-color'] := TStyleValue.Create(TGxRGBA.Create(255, 255, 255));
  result['frame-text-color'] := TStyleValue.Create(TGxRGBA.Create(255, 255, 255));
  result['frame-icon-color'] := TStyleValue.Create(TGxRGBA.Create(0, 255, 0));

  // disabled is always inactive too
  result['back-color#disabled'] := TStyleValue.Create(TGxRGBA.Create(0, 0, 128));
  result['text-color#disabled'] := TStyleValue.Create(TGxRGBA.Create(127, 127, 127));
  result['frame-text-color#disabled'] := TStyleValue.Create(TGxRGBA.Create(127, 127, 127));
  result['frame-icon-color#disabled'] := TStyleValue.Create(TGxRGBA.Create(0, 127, 0));
  result['darken#disabled'] := TStyleValue.Create(128, false); // darken inactive windows, no-inherit
  result['darken#inactive'] := TStyleValue.Create(128, false); // darken inactive windows, no-inherit

  result['text-color@label'] := TStyleValue.Create(TGxRGBA.Create(255, 255, 255));
  result['text-color#disabled@label'] := TStyleValue.Create(TGxRGBA.Create(127, 127, 127));

  result['frame-color@box'] := TStyleValue.Create(TGxRGBA.Create(255, 255, 0));
  result['frame-text-color@box'] := TStyleValue.Create(TGxRGBA.Create(255, 255, 0));
  result['frame-icon-color@box'] := TStyleValue.Create(TGxRGBA.Create(0, 255, 0));

  result['frame-color#disabled@box'] := TStyleValue.Create(TGxRGBA.Create(127, 127, 127));
  result['frame-text-color#disabled@box'] := TStyleValue.Create(TGxRGBA.Create(127, 127, 127));
  result['frame-icon-color#disabled@box'] := TStyleValue.Create(TGxRGBA.Create(127, 127, 127));
end;


function uiFindStyle (const stname: AnsiString): TUIStyle;
var
  stl: TUIStyle;
begin
  if (Length(stname) > 0) then
  begin
    for stl in styles do if (strEquCI1251(stl.mId, stname)) then begin result := stl; exit; end;
  end;
  for stl in styles do if (strEquCI1251(stl.mId, 'default')) then begin result := stl; exit; end;
  stl := createDefaultStyle();
  SetLength(styles, Length(styles)+1);
  styles[High(styles)] := stl;
  result := stl;
end;


procedure uiLoadStyles (const fname: AnsiString);
var
  st: TStream;
begin
  st := openDiskFileRO(fname);
  try
    uiLoadStyles(st);
  finally
    st.Free();
  end;
end;


procedure uiLoadStyles (st: TStream);
var
  par: TTextParser;
  stl: TUIStyle = nil;
  f: Integer;
begin
  if (st = nil) then raise Exception.Create('cannot load UI styles from nil stream');
  par := TFileTextParser.Create(st, false, [par.TOption.SignedNumbers, par.TOption.DollarIsId, par.TOption.DashIsId]);
  styles := nil;
  try
    while (not par.isEOF) do
    begin
      stl := TUIStyle.Create('');
      stl.parse(par);
      //writeln('new style: <', stl.mId, '>');
      f := 0;
      while (f < Length(styles)) do begin if (strEquCI1251(styles[f].mId, stl.mId)) then break; Inc(f); end;
      if (f < Length(styles)) then
      begin
        FreeAndNil(styles[f]);
      end
      else
      begin
        f := Length(styles);
        SetLength(styles, f+1);
      end;
      styles[f] := stl;
      stl := nil;
    end;
  finally
    stl.Free();
    par.Free();
  end;
  // we should have "default" style
  for f := 0 to High(styles) do if (strEquCI1251(styles[f].mId, 'default')) then exit;
  stl := createDefaultStyle();
  SetLength(styles, Length(styles)+1);
  styles[High(styles)] := stl;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TStyleValue.Create (v: Boolean; okToInherit: Boolean=true); begin vtype := TType.Bool; allowInherit := okToInherit; bval := v; end;
constructor TStyleValue.Create (v: Integer; okToInherit: Boolean=true); begin vtype := TType.Int; allowInherit := okToInherit; ival := v; end;

constructor TStyleValue.Create (ar, ag, ab: Integer; okToInherit: Boolean=true);
begin
  vtype := TType.Color;
  allowInherit := okToInherit;
  r := nmax(0, nmin(ar, 255));
  g := nmax(0, nmin(ag, 255));
  b := nmax(0, nmin(ab, 255));
  a := 255;
end;

constructor TStyleValue.Create (ar, ag, ab, aa: Integer; okToInherit: Boolean=true);
begin
  vtype := TType.Color;
  allowInherit := okToInherit;
  r := nmax(0, nmin(ar, 255));
  g := nmax(0, nmin(ag, 255));
  b := nmax(0, nmin(ab, 255));
  a := nmax(0, nmin(aa, 255));
end;

constructor TStyleValue.Create (const v: TGxRGBA; okToInherit: Boolean=true);
begin
  vtype := TType.Color;
  allowInherit := okToInherit;
  r := v.r;
  g := v.g;
  b := v.b;
  a := v.a;
end;

function TStyleValue.isEmpty (): Boolean; inline; begin result := (vtype = TType.Empty); end;
function TStyleValue.canInherit (): Boolean; inline; begin result := allowInherit; end;
function TStyleValue.asRGBA: TGxRGBA; inline; begin if (vtype = TType.Color) then result := TGxRGBA.Create(r, g, b, a) else result := TGxRGBA.Create(0, 0, 0, 0); end;
function TStyleValue.asRGBADef (const def: TGxRGBA): TGxRGBA; inline; begin if (vtype = TType.Color) then result := TGxRGBA.Create(r, g, b, a) else result := def; end;
function TStyleValue.asIntDef (const def: Integer): Integer; inline; begin  if (vtype = TType.Int) then result := ival else if (vtype = TType.Bool) then begin if (bval) then result := 1 else result := 0; end else result := def; end;
function TStyleValue.asBoolDef (const def: Boolean): Boolean; inline; begin  if (vtype = TType.Bool) then result := bval else if (vtype = TType.Int) then result := (ival <> 0) else result := def; end;


function TStyleValue.toString (): AnsiString;
begin
  case vtype of
    TType.Empty: result := '<empty>';
    TType.Bool: if bval then result := 'true' else result := 'false';
    TType.Int: result := formatstrf('%s', [ival]);
    TType.Color: if (a = 255) then result := formatstrf('rgb(%s,%s,%s)', [r, g, b]) else result := formatstrf('rgba(%s,%s,%s)', [r, g, b, a]);
    else result := '<invalid>';
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure freeSectionCB (var v: TStyleSection); begin FreeAndNil(v); end;


function splitPath (const path: AnsiString; out name, hash, ctl: AnsiString): Boolean;
var
  hashPos, atPos: Integer;
begin
  result := false;
  name := '';
  hash := '';
  ctl := '';
  hashPos := pos('#', path);
  atPos := pos('@', path);
  // split
  if (atPos > 0) then
  begin
    // has ctl, and (possible) hash
    if (hashPos > 0) then
    begin
      // has ctl and hash
      if (atPos < hashPos) then exit; // alas
      if (hashPos > 1) then name := Copy(path, 1, hashPos-1);
      Inc(hashPos); // skip hash
      if (hashPos < atPos) then hash := Copy(path, hashPos, atPos-hashPos);
    end
    else
    begin
      // has only ctl
      if (atPos > 1) then name := Copy(path, 1, atPos-1);
    end;
    Inc(atPos); // skip "at"
    if (atPos <= Length(path)) then ctl := Copy(path, atPos, Length(path)-atPos+1);
  end
  else if (hashPos > 0) then
  begin
    // has hash
    if (hashPos > 1) then name := Copy(path, 1, hashPos-1);
    Inc(hashPos); // skip hash
    if (hashPos <= Length(path)) then hash := Copy(path, hashPos, Length(path)-hashPos+1);
  end
  else
  begin
    // only name
    name := path;
  end;
  result := true;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TStyleSection.Create ();
begin
  mVals := THashStrStyleVal.Create();
  mHashVals := THashStrSection.Create();
  mCtlVals := THashStrSection.Create(freeSectionCB);
end;


destructor TStyleSection.Destroy ();
begin
  FreeAndNil(mVals);
  FreeAndNil(mHashVals);
  FreeAndNil(mCtlVals);
  inherited;
end;


// "text-color#inactive@label"
function TStyleSection.getValue (const path: AnsiString): TStyleValue;
var
  name, hash, ctl: AnsiString;
  sect: TStyleSection = nil;
  s1: TStyleSection = nil;
  checkInheritance: Boolean = false;
begin
  result.vtype := result.TType.Empty;
  if (not splitPath(path, name, hash, ctl)) then exit; // alas
  //writeln('name:<', name, '>; hash:<', hash, '>; ctl:<', ctl, '>');
  if (Length(name) = 0) then exit; // alas
  // try control
  if (Length(ctl) > 0) then
  begin
    // has ctl section?
    if not mCtlVals.get(ctl, sect) then
    begin
      sect := self;
      checkInheritance := true;
    end;
  end
  else
  begin
    sect := self;
  end;
  // has hash?
  if (Length(hash) > 0) then
  begin
    if sect.mHashVals.get(hash, s1) then
    begin
      if s1.mVals.get(name, result) then
      begin
        //writeln('hash: <', hash, '>: val=', result.toString);
        if (not result.isEmpty) and ((not checkInheritance) or (result.canInherit)) then exit;
      end;
    end;
    //writeln('NO hash: <', hash, '>: val=', result.toString);
    checkInheritance := true;
  end;
  // try just a name
  if sect.mVals.get(name, result) then
  begin
    if (not result.isEmpty) and ((not checkInheritance) or (result.canInherit)) then exit;
  end;
  // alas
  result.vtype := result.TType.Empty;
end;


procedure TStyleSection.putValue (const path: AnsiString; const val: TStyleValue);
var
  name, hash, ctl: AnsiString;
  sect: TStyleSection = nil;
  s1: TStyleSection = nil;
begin
  if (not splitPath(path, name, hash, ctl)) then exit; // alas
  // has name?
  if (Length(name) = 0) then exit; // no name -> nothing to do
  // has ctl?
  if (Length(ctl) > 0) then
  begin
    if not mCtlVals.get(ctl, sect) then
    begin
      // create new section
      sect := TStyleSection.Create();
      mCtlVals.put(ctl, sect);
    end;
  end
  else
  begin
    // no ctl, use default section
    sect := self;
  end;
  // has hash?
  if (Length(hash) > 0) then
  begin
    if not sect.mHashVals.get(hash, s1) then
    begin
      // create new section
      s1 := TStyleSection.Create();
      mHashVals.put(hash, s1);
    end;
  end
  else
  begin
    // no hash, use default section
    s1 := sect;
  end;
  s1.mVals.put(name, val);
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TUIStyle.Create (const aid: AnsiString);
begin
  mId := aid;
  mMain := TStyleSection.Create();
end;


constructor TUIStyle.Create (st: TStream); // parse from stream
var
  par: TTextParser;
begin
  mId := '';
  mMain := TStyleSection.Create();
  if (st = nil) then exit;
  par := TFileTextParser.Create(st, false, [par.TOption.SignedNumbers, par.TOption.DollarIsId, par.TOption.DashIsId]);
  try
    parse(par);
  finally
    par.Free();
  end;
end;


constructor TUIStyle.CreateFromFile (const fname: AnsiString);
var
  par: TTextParser;
  st: TStream;
begin
  mId := '';
  mMain := TStyleSection.Create();
  st := openDiskFileRO(fname);
  try
    par := TFileTextParser.Create(st, false, [par.TOption.SignedNumbers, par.TOption.DollarIsId, par.TOption.DashIsId]);
    try
      parse(par);
    finally
      par.Free();
    end;
  finally
    st.Free();
  end;
end;


destructor TUIStyle.Destroy ();
begin
  mId := '';
  FreeAndNil(mMain);
end;


function TUIStyle.getValue (const path: AnsiString): TStyleValue; inline;
begin
  result := mMain[path];
end;

procedure TUIStyle.putValue (const path: AnsiString; const val: TStyleValue); inline;
begin
  mMain.putValue(path, val);
end;


procedure TUIStyle.parse (par: TTextParser);
  function getByte (): Byte;
  begin
    if (par.tokType <> par.TTInt) then par.expectInt();
    if (par.tokInt < 0) or (par.tokInt > 255) then par.error('invalid byte value');
    result := Byte(par.tokInt);
    par.skipToken();
  end;

  procedure parseSection (sect: TStyleSection; ctlAllowed: Boolean; hashAllowed: Boolean);
  var
    s: AnsiString;
    sc: TStyleSection = nil;
    v: TStyleValue;
  begin
    par.expectDelim('{');
    while (not par.isDelim('}')) do
    begin
      while (par.eatDelim(';')) do begin end;
      // hash
      if hashAllowed and (par.eatDelim('#')) then
      begin
        s := par.expectIdOrStr();
        //writeln('hash: <', s, '>');
        par.eatDelim(':'); // optional
        if not sect.mHashVals.get(s, sc) then
        begin
          // create new section
          sc := TStyleSection.Create();
          sect.mHashVals.put(s, sc);
        end;
        parseSection(sc, false, false);
        continue;
      end;
      // ctl
      if ctlAllowed and (par.eatDelim('@')) then
      begin
        s := par.expectIdOrStr();
        //writeln('ctl: <', s, '>');
        par.eatDelim(':'); // optional
        if not sect.mCtlVals.get(s, sc) then
        begin
          // create new section
          sc := TStyleSection.Create();
          sect.mCtlVals.put(s, sc);
        end;
        parseSection(sc, false, true);
        continue;
      end;
      // name
      s := par.expectIdOrStr();
      //writeln('name: <', s, '>');
      v.allowInherit := true;
      par.expectDelim(':');
      if (par.eatId('rgb')) or (par.eatId('rgba')) then
      begin
        // color
        par.expectDelim('(');
        v.vtype := v.TType.Color;
        v.r := getByte(); par.eatDelim(','); // optional
        v.g := getByte(); par.eatDelim(','); // optional
        v.b := getByte(); par.eatDelim(','); // optional
        if (par.tokType = par.TTInt) then
        begin
          v.a := getByte(); par.eatDelim(','); // optional
        end
        else
        begin
          v.a := 255; // opaque
        end;
        par.expectDelim(')');
      end
      else if (par.eatId('true')) or (par.eatId('tan')) then
      begin
        v.vtype := v.TType.Bool;
        v.bval := true;
      end
      else if (par.eatId('false')) or (par.eatId('ona')) then
      begin
        v.vtype := v.TType.Bool;
        v.bval := false;
      end
      else
      begin
        // should be int
        v.vtype := v.TType.Int;
        v.ival := par.expectInt();
      end;
      // '!' flags
      while (par.eatDelim('!')) do
      begin
        if (par.eatId('no-inherit')) then v.allowInherit := false
        else par.error('unknown flag');
      end;
      par.expectDelim(';');
      sect.mVals.put(s, v);
    end;
    par.expectDelim('}');
  end;

begin
  // style name
  if (not par.isIdOrStr) then
  begin
    if (Length(mId) = 0) then par.error('style name expected');
  end
  else
  begin
    mId := par.tokStr;
  end;
  if (Length(mId) = 0) then mId := 'default';
  par.skipToken();
  parseSection(mMain, true, true);
end;


end.
