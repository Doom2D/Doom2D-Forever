(* coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
 * Understanding is not required. Only obedience.
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../shared/a_modes.inc}
{.$DEFINE UI_STYLE_DEBUG_SEARCH}
unit fui_style;

interface

uses
  SysUtils, Classes,
  fui_common, // for TGxRGBA
  xstreams, xparser, utils, hashtable;


type
  TStyleSection = class;

  TStyleValue = packed record
  public
    type TType = (Empty, Bool, Int, Color, Str);

  public
    constructor Create (v: Boolean);
    constructor Create (v: Integer);
    constructor Create (ar, ag, ab: Integer; aa: Integer=255);
    constructor Create (const v: TGxRGBA);
    constructor Create (const v: AnsiString);

    function isEmpty (): Boolean; inline;

    function toString (): AnsiString;
    function asRGBA: TGxRGBA; inline;
    function asRGBADef (const def: TGxRGBA): TGxRGBA; inline;
    function asInt (const def: Integer=0): Integer; inline;
    function asBool (const def: Boolean=false): Boolean; inline;
    function asStr (const def: AnsiString=''): AnsiString; inline;

  public
    vtype: TType;
    case TType of
      TType.Bool: (bval: Boolean);
      TType.Int: (ival: Integer);
      TType.Color: (r, g, b, a: Byte);
      TType.Str: (sval: Pointer); // AnsiString
  end;

  THashStrStyleVal = specialize THashBase<AnsiString, TStyleValue, THashKeyStrAnsiCI>;
  THashStrSection = specialize THashBase<AnsiString, TStyleSection, THashKeyStrAnsiCI>;

  TStyleSection = class
  private
    mParent: TStyleSection; // for inheritance
    mInherits: AnsiString;
    mHashName: AnsiString; // for this section
    mCtlName: AnsiString; // for this section
    mVals: THashStrStyleVal;
    mHashes: THashStrSection;
    mCtls: THashStrSection;

  private
    function getTopLevel (): TStyleSection; inline;
    // "text-color#inactive@label"
    function getValue (const path: AnsiString): TStyleValue;

  public
    constructor Create ();
    destructor Destroy (); override;

    function get (name, hash, ctl: AnsiString): TStyleValue;

  public
    property value[const path: AnsiString]: TStyleValue read getValue; default;
    property topLevel: TStyleSection read getTopLevel;
  end;

  TUIStyle = class
  private
    mId: AnsiString; // style name ('default', for example)
    mMain: TStyleSection;

  private
    procedure createMain ();

    procedure parse (par: TTextParser);

    function getValue (const path: AnsiString): TStyleValue; inline;

  public
    constructor Create (const aid: AnsiString);
    constructor Create (st: TStream); // parse from stream
    constructor CreateFromFile (const fname: AnsiString);
    destructor Destroy (); override;

    function get (name, hash, ctl: AnsiString): TStyleValue;

  public
    property id: AnsiString read mId;
    property value[const path: AnsiString]: TStyleValue read getValue; default;
  end;


procedure uiLoadStyles (const fname: AnsiString);
procedure uiLoadStyles (st: TStream);

// will return "default" (or raise an exception if there is no "default")
function uiFindStyle (const stname: AnsiString): TUIStyle;


implementation

uses
  fui_wadread;


var
  styles: array of TUIStyle = nil;


{
function createDefaultStyle (): TUIStyle;
var
  st: TStream;
begin
  result := nil;
  st := TStringStream.Create(defaultStyleStr);
  st.position := 0;
  try
    result := TUIStyle.Create(st);
  finally
    FreeAndNil(st);
  end;
end;
}


function uiFindStyle (const stname: AnsiString): TUIStyle;
var
  stl: TUIStyle;
begin
  if (Length(stname) > 0) then
  begin
    for stl in styles do if (strEquCI1251(stl.mId, stname)) then begin result := stl; exit; end;
  end;
  for stl in styles do if (strEquCI1251(stl.mId, 'default')) then begin result := stl; exit; end;
  raise Exception.Create('FlexUI FATAL: no "default" style in stylesheet');
  {
  stl := createDefaultStyle();
  SetLength(styles, Length(styles)+1);
  styles[High(styles)] := stl;
  result := stl;
  }
end;


procedure uiLoadStyles (const fname: AnsiString);
var
  st: TStream;
begin
  st := fuiOpenFile(fname);
  if (st = nil) then raise Exception.Create('FlexUI file '''+fname+''' not found!');
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
  par := TFileTextParser.Create(st, false, [par.TOption.SignedNumbers, par.TOption.DollarIsId, par.TOption.DashIsId, par.TOption.HtmlColors]);
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
  raise Exception.Create('FlexUI FATAL: no "default" style in stylesheet');
  {
  stl := createDefaultStyle();
  SetLength(styles, Length(styles)+1);
  styles[High(styles)] := stl;
  }
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure freeValueCB (var v: TStyleValue); begin
  if (v.vtype = v.TType.Str) then
  begin
    AnsiString(v.sval) := '';
  end;
  v.vtype := v.TType.Empty;
end;

constructor TStyleValue.Create (v: Boolean); begin vtype := TType.Bool; bval := v; end;
constructor TStyleValue.Create (v: Integer); begin vtype := TType.Int; ival := v; end;
constructor TStyleValue.Create (const v: AnsiString); begin vtype := TType.Str; sval := Pointer(v); end;

constructor TStyleValue.Create (ar, ag, ab: Integer; aa: Integer=255);
begin
  vtype := TType.Color;
  r := nmax(0, nmin(ar, 255));
  g := nmax(0, nmin(ag, 255));
  b := nmax(0, nmin(ab, 255));
  a := nmax(0, nmin(aa, 255));
end;

constructor TStyleValue.Create (const v: TGxRGBA);
begin
  vtype := TType.Color;
  r := v.r;
  g := v.g;
  b := v.b;
  a := v.a;
end;

function TStyleValue.isEmpty (): Boolean; inline; begin result := (vtype = TType.Empty); end;
function TStyleValue.asRGBA: TGxRGBA; inline; begin if (vtype = TType.Color) then result := TGxRGBA.Create(r, g, b, a) else result := TGxRGBA.Create(0, 0, 0, 0); end;
function TStyleValue.asRGBADef (const def: TGxRGBA): TGxRGBA; inline; begin if (vtype = TType.Color) then result := TGxRGBA.Create(r, g, b, a) else result := def; end;
function TStyleValue.asInt (const def: Integer=0): Integer; inline; begin if (vtype = TType.Int) then result := ival else if (vtype = TType.Bool) then begin if (bval) then result := 1 else result := 0; end else result := def; end;
function TStyleValue.asBool (const def: Boolean=false): Boolean; inline; begin if (vtype = TType.Bool) then result := bval else if (vtype = TType.Int) then result := (ival <> 0) else result := def; end;
function TStyleValue.asStr (const def: AnsiString=''): AnsiString; inline; begin if (vtype = TType.Str) then result := AnsiString(sval) else result := def; end;

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
      if (atPos < hashPos) then
      begin
        // @ctl#hash
        if (atPos > 1) then name := Copy(path, 1, atPos-1);
        Inc(atPos); // skip "at"
        if (atPos < hashPos) then ctl := Copy(path, atPos, hashPos-atPos);
        Inc(hashPos); // skip hash
        if (hashPos <= Length(path)) then hash := Copy(path, hashPos, Length(path)-hashPos+1);
      end
      else
      begin
        // #hash@ctl
        if (hashPos > 1) then name := Copy(path, 1, hashPos-1);
        Inc(hashPos); // skip hash
        if (hashPos < atPos) then hash := Copy(path, hashPos, atPos-hashPos);
        Inc(atPos); // skip "at"
        if (atPos <= Length(path)) then ctl := Copy(path, atPos, Length(path)-atPos+1);
      end;
    end
    else
    begin
      // has only ctl
      if (atPos > 1) then name := Copy(path, 1, atPos-1);
      Inc(atPos); // skip "at"
      if (atPos <= Length(path)) then ctl := Copy(path, atPos, Length(path)-atPos+1);
    end;
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
  mParent := nil;
  mInherits := '';
  mHashName := '';
  mCtlName := '';
  mVals := THashStrStyleVal.Create(freeValueCB);
  mHashes := THashStrSection.Create(freeSectionCB);
  mCtls := THashStrSection.Create(freeSectionCB);
end;


destructor TStyleSection.Destroy ();
begin
  FreeAndNil(mVals);
  FreeAndNil(mHashes);
  FreeAndNil(mCtls);
  mParent := nil;
  mInherits := '';
  mHashName := '';
  mCtlName := '';
  inherited;
end;


function TStyleSection.getTopLevel (): TStyleSection; inline;
begin
  result := self;
  while (result.mParent <> nil) do result := result.mParent;
end;


function TStyleSection.get (name, hash, ctl: AnsiString): TStyleValue;
var
  tmp: AnsiString;
  sect, s1, so: TStyleSection;
  jumpsLeft: Integer = 32; // max inheritance level
  skipInherits: Boolean = false;
begin
  result.vtype := result.TType.Empty;
  if (Length(name) = 0) then exit; // alas
  {$IFDEF UI_STYLE_DEBUG_SEARCH}writeln('***GET: <', name, '#', hash, '@', ctl, '>');{$ENDIF}
  // try control
  sect := self;
  if (Length(ctl) > 0) then
  begin
    if (not strEquCI1251(ctl, mCtlName)) then
    begin
      // has ctl section?
      if (not topLevel.mCtls.get(ctl, sect)) then sect := topLevel;
    end;
  end;
  // has hash?
  if (Length(hash) > 0) then
  begin
    if (not strEquCI1251(hash, sect.mHashName)) then
    begin
      if (sect.mHashes.get(hash, s1)) then sect := s1;
    end;
  end;
  // try name, go up with inheritance
  while (jumpsLeft > 0) do
  begin
    if (sect.mVals.get(name, result)) then
    begin
      if (not result.isEmpty) then exit; // i found her!
    end;
    // go up
    if (skipInherits) or (Length(sect.mInherits) = 0) then
    begin
      skipInherits := false;
      // for hash section: try parent section first
      if (Length(sect.mHashName) > 0) then
      begin
        {$IFDEF UI_STYLE_DEBUG_SEARCH}writeln('search for <#', hash, '@', ctl, '> at <#', sect.mHashName, '@', sect.mCtlName, '>: hash up');{$ENDIF}
        sect := sect.mParent;
        if (sect = nil) then break; // alas
        {$IFDEF UI_STYLE_DEBUG_SEARCH}writeln(' hash up: trying <#', sect.mHashName, '@', sect.mCtlName, '>');{$ENDIF}
        if (sect.mVals.get(name, result)) then
        begin
          if (not result.isEmpty) then exit; // i found her!
        end;
        // move another parent up
        sect := sect.mParent;
        if (sect = nil) then break; // alas
        {$IFDEF UI_STYLE_DEBUG_SEARCH}writeln(' hash up: jumped up twice to <#', sect.mHashName, '@', sect.mCtlName, '>');{$ENDIF}
      end
      else
      begin
        // one parent up
        {$IFDEF UI_STYLE_DEBUG_SEARCH}writeln('search for <#', hash, '@', ctl, '> at <#', sect.mHashName, '@', sect.mCtlName, '>: jump up');{$ENDIF}
        sect := sect.mParent;
        if (sect = nil) then break; // alas
      end;
      // here, we should have non-hash section
      assert(Length(sect.mHashName) = 0);
      // if we want hash, try to find it, otherwise do nothing
      if (Length(hash) > 0) then
      begin
        {$IFDEF UI_STYLE_DEBUG_SEARCH}writeln(' search for <#', hash, '@', ctl, '> at <#', sect.mHashName, '@', sect.mCtlName, '>: hash down');{$ENDIF}
        if (sect.mHashes.get(hash, s1)) then
        begin
          sect := s1;
          {$IFDEF UI_STYLE_DEBUG_SEARCH}writeln('  found <#', sect.mHashName, '@', sect.mCtlName, '>');{$ENDIF}
        end;
      end;
    end
    else
    begin
      // inheritance
      Dec(jumpsLeft);
      if (jumpsLeft < 1) then break; // alas
      {$IFDEF UI_STYLE_DEBUG_SEARCH}writeln('inherits: <', sect.mInherits, '>');{$ENDIF}
      // parse inherit string
      if (not splitPath(sect.mInherits, tmp, hash, ctl)) then exit; // alas
      {$IFDEF UI_STYLE_DEBUG_SEARCH}writeln('inherits: <', hash, '>:<', ctl, '>');{$ENDIF}
      // find section
      if (Length(ctl) > 0) then
      begin
        // ctl
             if (strEquCI1251(ctl, '$main$')) then sect := topLevel
        else if (strEquCI1251(ctl, '$up$')) then begin if (Length(sect.mHashName) <> 0) then sect := sect.mParent.mParent else sect := sect.mParent; end
        else if (not topLevel.mCtls.get(ctl, sect)) then sect := topLevel;
        if (sect = nil) then break; // alas
        if (Length(hash) > 0) then
        begin
          if (sect.mHashes.get(hash, s1)) then sect := s1;
        end;
      end
      else
      begin
        // hash
        assert(Length(hash) > 0);
        // dummy loop, so i can use `break`
        repeat
          // get out of hash section
          if (Length(sect.mHashName) > 0) then
          begin
            {$IFDEF UI_STYLE_DEBUG_SEARCH}writeln('hash-jump-up: <#', sect.mHashName, '@', sect.mCtlName, '>');{$ENDIF}
            sect := sect.mParent;
            if (sect = nil) then break; // alas
            // check for hash section in parent; use parent if there is no such hash section
            {$IFDEF UI_STYLE_DEBUG_SEARCH}writeln(' checking parent: <#', sect.mHashName, '@', sect.mCtlName, '> for <#', hash, '>');{$ENDIF}
            so := sect;
            if (sect.mHashes.get(hash, s1)) then
            begin
              if (s1 <> sect) and (s1 <> so) then
              begin
                {$IFDEF UI_STYLE_DEBUG_SEARCH}writeln('  found in parent: <#', sect.mHashName, '@', sect.mCtlName, '> for <#', hash, '>');{$ENDIF}
                sect := s1;
              end;
            end;
          end
          else
          begin
            // we're in parent, try to find hash section
            {$IFDEF UI_STYLE_DEBUG_SEARCH}writeln(' checking: <#', sect.mHashName, '@', sect.mCtlName, '> for <#', hash, '>');{$ENDIF}
            if (sect.mHashes.get(hash, s1)) then
            begin
              {$IFDEF UI_STYLE_DEBUG_SEARCH}writeln('  found: <#', sect.mHashName, '@', sect.mCtlName, '> for <#', hash, '>');{$ENDIF}
              sect := s1;
            end
            else
            begin
              // reuse current parent, but don't follow inheritance for it
              skipInherits := true;
            end;
          end;
        until true;
        if (sect = nil) then break;
      end;
    end;
  end;
  // alas
  result.vtype := result.TType.Empty;
end;


// "text-color#inactive@label"
function TStyleSection.getValue (const path: AnsiString): TStyleValue;
var
  name, hash, ctl: AnsiString;
begin
  result.vtype := result.TType.Empty;
  if (not splitPath(path, name, hash, ctl)) then exit; // alas
  //writeln('name:<', name, '>; hash:<', hash, '>; ctl:<', ctl, '>');
  result := get(name, hash, ctl);
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TUIStyle.Create (const aid: AnsiString);
begin
  mId := aid;
  createMain();
end;


constructor TUIStyle.Create (st: TStream); // parse from stream
var
  par: TTextParser;
begin
  mId := '';
  createMain();
  if (st = nil) then exit;
  par := TFileTextParser.Create(st, false, [par.TOption.SignedNumbers, par.TOption.DollarIsId, par.TOption.DashIsId, par.TOption.HtmlColors]);
  try
    parse(par);
  finally
    par.Free();
  end;
end;


constructor TUIStyle.CreateFromFile (const fname: AnsiString);
var
  st: TStream;
begin
  st := openDiskFileRO(fname);
  try
    Create(st);
  finally
    st.Free();
  end;
end;


destructor TUIStyle.Destroy ();
begin
  mId := '';
  FreeAndNil(mMain);
end;


procedure TUIStyle.createMain ();
begin
  mMain := TStyleSection.Create();
  mMain.mCtlName := '$main$';
end;


function TUIStyle.getValue (const path: AnsiString): TStyleValue; inline;
begin
  result := mMain[path];
end;

function TUIStyle.get (name, hash, ctl: AnsiString): TStyleValue;
begin
  result := mMain.get(name, hash, ctl);
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
    s, inh: AnsiString;
    sc: TStyleSection = nil;
    v: TStyleValue;

    procedure parseInherit ();
    begin
      inh := '';
      if (par.eatDelim('(')) then
      begin
        if (par.eatDelim(')')) then par.error('empty inheritance is not allowed');
        if (par.eatDelim('#')) then
        begin
          inh := '#';
          inh += par.expectId();
        end;
        if (par.eatDelim('@')) then
        begin
          inh += '#';
          inh += par.expectId();
        end;
        par.expectDelim(')');
      end;
    end;

    function nib2c (n: Integer): Byte; inline;
    begin
           if (n < 0) then result := 0
      else if (n > 15) then result := 255
      else result := Byte(255*n div 15);
    end;

  begin
    s := '';
    inh := '';
    par.expectDelim('{');
    while (not par.isDelim('}')) do
    begin
      while (par.eatDelim(';')) do begin end;
      // ctl
      if ctlAllowed and (par.eatDelim('@')) then
      begin
        s := par.expectId();
        parseInherit();
        par.eatDelim(':'); // optional
        if (not sect.mCtls.get(s, sc)) then
        begin
          // create new section
          sc := TStyleSection.Create();
          sc.mParent := sect;
          sc.mInherits := inh;
          sc.mHashName := '';
          sc.mCtlName := s;
          sect.mCtls.put(s, sc);
        end
        else
        begin
          assert(sc.mParent = sect);
          assert(sc.mHashName = '');
          assert(sc.mCtlName = s);
          if (Length(sc.mInherits) <> 0) and (Length(inh) <> 0) then par.error('double inheritance');
          sc.mInherits := inh;
        end;
        if (not par.eatDelim(';')) then parseSection(sc, false, true);
        continue;
      end;
      // hash
      if hashAllowed and (par.eatDelim('#')) then
      begin
        s := par.expectId();
        parseInherit();
        par.eatDelim(':'); // optional
        if (not sect.mHashes.get(s, sc)) then
        begin
          // create new section
          sc := TStyleSection.Create();
          sc.mParent := sect;
          sc.mInherits := inh;
          sc.mHashName := s;
          sc.mCtlName := '';
          sect.mHashes.put(s, sc);
        end
        else
        begin
          assert(sc.mParent = sect);
          assert(sc.mHashName = s);
          assert(sc.mCtlName = '');
          if (Length(sc.mInherits) <> 0) and (Length(inh) <> 0) then par.error('double inheritance');
          sc.mInherits := inh;
        end;
        if (not par.eatDelim(';')) then parseSection(sc, false, false);
        continue;
      end;
      // name
      s := par.expectId();
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
      else if (par.isId) and (par.tokStr[1] = '#') then
      begin
        // html color
        assert((Length(par.tokStr) = 4) or (Length(par.tokStr) = 7));
        //writeln('<', par.tokStr, '>; {', par.curChar, '}');
        v.vtype := v.TType.Color;
        if (Length(par.tokStr) = 4) then
        begin
          // #rgb
          v.r := nib2c(digitInBase(par.tokStr[2], 16));
          v.g := nib2c(digitInBase(par.tokStr[3], 16));
          v.b := nib2c(digitInBase(par.tokStr[4], 16));
        end
        else
        begin
         // #rrggbb
          v.r := Byte(digitInBase(par.tokStr[2], 16)*16+digitInBase(par.tokStr[3], 16));
          v.g := Byte(digitInBase(par.tokStr[4], 16)*16+digitInBase(par.tokStr[5], 16));
          v.b := Byte(digitInBase(par.tokStr[6], 16)*16+digitInBase(par.tokStr[7], 16));
        end;
        v.a := 255;
        //writeln('  r=', v.r, '; g=', v.g, '; b=', v.b);
        par.skipToken();
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
      else if (par.isStr) then
      begin
        // string value
        v := TStyleValue.Create(par.tokStr);
        par.skipToken();
      end
      else if (par.eatId('inherit')) then
      begin
        v.vtype := v.TType.Empty;
      end
      else
      begin
        // should be int
        v.vtype := v.TType.Int;
        v.ival := par.expectInt();
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
  if (not par.eatDelim(';')) then parseSection(mMain, true, true);
end;


end.
