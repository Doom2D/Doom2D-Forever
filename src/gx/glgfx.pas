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
{$INCLUDE ../shared/a_modes.inc}
unit glgfx;

interface

uses
  SysUtils, Classes,
  GL, GLExt, SDL2,
  sdlcarcass;


// ////////////////////////////////////////////////////////////////////////// //
type
  TGxRGBA = packed record
  public
    r, g, b, a: Byte;

  public
    constructor Create (ar, ag, ab: Integer; aa: Integer=255);

    function asUInt (): LongWord; inline;
    function isOpaque (): Boolean; inline;
    function isTransparent (): Boolean; inline;

    // WARNING! This function does blending in RGB space, and RGB space is not linear!
    // alpha value of `self` doesn't matter
    // `aa` means: 255 for replace color, 0 for keep `self`
    function blend (ar, ag, ab, aa: Integer): TGxRGBA; inline;
  end;


// ////////////////////////////////////////////////////////////////////////// //
type
  THMouseEvent = record
  public
    const
      // both for but and for bstate
      None = 0;
      Left = $0001;
      Right = $0002;
      Middle = $0004;
      WheelUp = $0008;
      WheelDown = $0010;

    // event types
    type
      TKind = (Release, Press, Motion);

  public
    kind: TKind; // motion, press, release
    x, y: Integer; // current mouse position
    dx, dy: Integer; // for wheel this is wheel motion, otherwise this is relative mouse motion
    but: Word; // current pressed/released button, or 0 for motion
    bstate: Word; // button state BEFORE event (i.e. press/release modifications aren't done yet)
    kstate: Word; // keyboard state (see THKeyEvent);

  public
    function press (): Boolean; inline;
    function release (): Boolean; inline;
    function motion (): Boolean; inline;
  end;

  THKeyEvent = record
  public
    const
      // modifiers
      ModCtrl = $0001;
      ModAlt = $0002;
      ModShift = $0004;
      ModHyper = $0008;

    // event types
    type
      TKind = (Release, Press);

  public
    kind: TKind;
    scan: Word; // SDL_SCANCODE_XXX
    sym: LongWord; // SDLK_XXX
    x, y: Integer; // current mouse position
    bstate: Word; // button state
    kstate: Word; // keyboard state BEFORE event (i.e. press/release modifications aren't done yet)

  public
    function press (): Boolean; inline;
    function release (): Boolean; inline;
  end;


// ////////////////////////////////////////////////////////////////////////// //
// setup 2D OpenGL mode; will be called automatically in `glInit()`
procedure oglSetup2D (winWidth, winHeight: Integer; upsideDown: Boolean=false);

type
  TScissorSave = record
  public
    wassc: Boolean;
    scxywh: packed array[0..3] of GLint;

  public

  public
    procedure save (enableScissoring: Boolean);
    procedure restore ();

    // set new scissor rect, bounded by the saved scissor rect
    procedure combineRect (x, y, w, h: Integer);
  end;


procedure oglDrawCursor ();
procedure oglDrawCursorAt (msX, msY: Integer);

// return `false` if destination rect is empty
// modifies rect0
function intersectRect (var x0, y0, w0, h0: Integer; const x1, y1, w1, h1: Integer): Boolean;

procedure normRGBA (var r, g, b, a: Integer); inline;
function setupGLColor (r, g, b, a: Integer): Boolean;
function setupGLColor (constref clr: TGxRGBA): Boolean;
function isScaled (): Boolean;

function textWidth6 (const s: AnsiString): Integer;
function textWidth8 (const s: AnsiString): Integer;
// return width (including last empty pixel)
function drawTextInternal (wdt, x, y: Integer; const s: AnsiString; constref clr: TGxRGBA; tid: GLuint; constref fontwdt: array of Byte; prop: Boolean): Integer;
procedure drawLine (x1, y1, x2, y2: Integer; constref clr: TGxRGBA);
procedure drawVLine (x, y, len: Integer; constref clr: TGxRGBA);
procedure drawHLine (x, y, len: Integer; constref clr: TGxRGBA);
procedure drawRect (x, y, w, h: Integer; constref clr: TGxRGBA);
procedure drawRectUI (x, y, w, h: Integer; constref clr: TGxRGBA);
procedure darkenRect (x, y, w, h: Integer; a: Integer);
procedure fillRect (x, y, w, h: Integer; constref clr: TGxRGBA);
function drawText6 (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
function drawText8 (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
function drawText6Prop (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
function drawText8Prop (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
// x-centered at `x`
function drawText6XC (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
function drawText8XC (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
function drawText6PropXC (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
function drawText8PropXC (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;


// ////////////////////////////////////////////////////////////////////////// //
// event handlers
var
  evMouseCB: function (var ev: THMouseEvent): Boolean = nil; // `true`: event eaten
  evKeyCB: function (var ev: THKeyEvent): Boolean = nil; // `true`: event eaten


// ////////////////////////////////////////////////////////////////////////// //
function getMouseX (): Integer; inline;
function getMouseY (): Integer; inline;
function getButState (): Word; inline;
function getModState (): Word; inline;


// ////////////////////////////////////////////////////////////////////////// //
property
  gMouseX: Integer read getMouseX;
  gMouseY: Integer read getMouseY;
  gButState: Word read getButState;
  gModState: Word read getModState;

var
  gGfxDoClear: Boolean = true;


// ////////////////////////////////////////////////////////////////////////// //
// any mods = 255: nothing was defined
function parseModKeys (const s: AnsiString; out kmods: Byte; out mbuts: Byte): AnsiString;

operator = (constref ev: THKeyEvent; const s: AnsiString): Boolean;
operator = (const s: AnsiString; constref ev: THKeyEvent): Boolean;

operator = (constref ev: THMouseEvent; const s: AnsiString): Boolean;
operator = (const s: AnsiString; constref ev: THMouseEvent): Boolean;


implementation


var
  curButState: Word = 0;
  curModState: Word = 0;
  curMsX: Integer = 0;
  curMsY: Integer = 0;


// ////////////////////////////////////////////////////////////////////////// //
function strEquCI (const s0, s1: AnsiString): Boolean;
var
  f: Integer;
  c0, c1: AnsiChar;
begin
  result := (Length(s0) = Length(s1));
  if result then
  begin
    for f := 1 to Length(s0) do
    begin
      c0 := s0[f];
      if (c0 >= 'a') and (c0 <= 'z') then Dec(c0, 32); // poor man's `toupper()`
      c1 := s1[f];
      if (c1 >= 'a') and (c1 <= 'z') then Dec(c1, 32); // poor man's `toupper()`
      if (c0 <> c1) then begin result := false; exit; end;
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function getMouseX (): Integer; inline; begin result := curMsX; end;
function getMouseY (): Integer; inline; begin result := curMsY; end;
function getButState (): Word; inline; begin result := curButState; end;
function getModState (): Word; inline; begin result := curModState; end;


// ////////////////////////////////////////////////////////////////////////// //
function THMouseEvent.press (): Boolean; inline; begin result := (kind = TKind.Press); end;
function THMouseEvent.release (): Boolean; inline; begin result := (kind = TKind.Release); end;
function THMouseEvent.motion (): Boolean; inline; begin result := (kind = TKind.Motion); end;

function THKeyEvent.press (): Boolean; inline; begin result := (kind = TKind.Press); end;
function THKeyEvent.release (): Boolean; inline; begin result := (kind = TKind.Release); end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TGxRGBA.Create (ar, ag, ab: Integer; aa: Integer=255);
begin
  if (ar < 0) then r := 0 else if (ar > 255) then r := 255 else r := Byte(ar);
  if (ag < 0) then g := 0 else if (ag > 255) then g := 255 else g := Byte(ag);
  if (ab < 0) then b := 0 else if (ab > 255) then b := 255 else b := Byte(ab);
  if (aa < 0) then a := 0 else if (aa > 255) then a := 255 else a := Byte(aa);
end;

function TGxRGBA.asUInt (): LongWord; inline; begin result := LongWord(r) or (LongWord(g) shl 8) or (LongWord(b) shl 16) or (LongWord(a) shl 24); end;

function TGxRGBA.isOpaque (): Boolean; inline; begin result := (a = 255); end;
function TGxRGBA.isTransparent (): Boolean; inline; begin result := (a = 0); end;

function TGxRGBA.blend (ar, ag, ab, aa: Integer): TGxRGBA; inline;
var
  me, it, a_tmp_, dc_tmp_, srb_tmp_, sg_tmp_, drb_tmp_, dg_tmp_, orb_tmp_, og_tmp_: LongWord;
begin
  if (aa <= 0) then begin result := self; exit; end;
  result := TGxRGBA.Create(ar, ag, ab, aa);
  if (aa >= 255) then begin result.a := a; exit; end;
  me := asUInt;
  it := result.asUInt;
  a_tmp_ := (256-(255-(it shr 24))) and (-(1-(((255-(it shr 24))+1) shr 8))); // to not loose bits, but 255 should become 0
  dc_tmp_ := me and $ffffff;
  srb_tmp_ := (it and $ff00ff);
  sg_tmp_ := (it and $00ff00);
  drb_tmp_ := (dc_tmp_ and $ff00ff);
  dg_tmp_ := (dc_tmp_ and $00ff00);
  orb_tmp_ := (drb_tmp_+(((srb_tmp_-drb_tmp_)*a_tmp_+$800080) shr 8)) and $ff00ff;
  og_tmp_ := (dg_tmp_+(((sg_tmp_-dg_tmp_)*a_tmp_+$008000) shr 8)) and $00ff00;
  me := (orb_tmp_ or og_tmp_); // or $ff000000; /* and $ffffff;*/
  result.r := Byte(me and $ff);
  result.g := Byte((me shr 8) and $ff);
  result.b := Byte((me shr 16) and $ff);
  result.a := a;
end;


// ////////////////////////////////////////////////////////////////////////// //
// any mods = 255: nothing was defined
function parseModKeys (const s: AnsiString; out kmods: Byte; out mbuts: Byte): AnsiString;
var
  pos, epos: Integer;
begin
  kmods := 255;
  mbuts := 255;
  pos := 1;
  //while (pos <= Length(s)) and (s[pos] <= ' ') do Inc(pos);
  if (pos < Length(s)) and ((s[pos] = '+') or (s[pos] = '-') or (s[pos] = '*')) then Inc(pos);
  while (pos < Length(s)) do
  begin
    if (Length(s)-pos >= 2) and (s[pos+1] = '-') then
    begin
      case s[pos] of
        'C', 'c': begin if (kmods = 255) then kmods := 0; kmods := kmods or THKeyEvent.ModCtrl; Inc(pos, 2); continue; end;
        'M', 'm': begin if (kmods = 255) then kmods := 0; kmods := kmods or THKeyEvent.ModAlt; Inc(pos, 2); continue; end;
        'S', 's': begin if (kmods = 255) then kmods := 0; kmods := kmods or THKeyEvent.ModShift; Inc(pos, 2); continue; end;
      end;
      break;
    end;
    if (Length(s)-pos >= 4) and ((s[pos+1] = 'M') or (s[pos+1] = 'm')) and ((s[pos+2] = 'B') or (s[pos+1] = 'b')) and (s[pos+3] = '-') then
    begin
      case s[pos] of
        'L', 'l': begin if (mbuts = 255) then mbuts := 0; mbuts := mbuts or THMouseEvent.Left; Inc(pos, 4); continue; end;
        'R', 'r': begin if (mbuts = 255) then mbuts := 0; mbuts := mbuts or THMouseEvent.Right; Inc(pos, 4); continue; end;
        'M', 'm': begin if (mbuts = 255) then mbuts := 0; mbuts := mbuts or THMouseEvent.Middle; Inc(pos, 4); continue; end;
      end;
      break;
    end;
    break;
  end;
  epos := Length(s)+1;
  while (epos > pos) and (s[epos-1] <= ' ') do Dec(epos);
  if (epos > pos) then result := Copy(s, pos, epos-pos) else result := '';
end;


operator = (constref ev: THKeyEvent; const s: AnsiString): Boolean;
var
  f: Integer;
  kmods: Byte = 255;
  mbuts: Byte = 255;
  kname: AnsiString;
begin
  result := false;
  if (Length(s) > 0) then
  begin
         if (s[1] = '+') then begin if (not ev.press) then exit; end
    else if (s[1] = '-') then begin if (not ev.release) then exit; end
    else if (s[1] = '*') then begin end
    else if (not ev.press) then exit;
  end;
  kname := parseModKeys(s, kmods, mbuts);
  if (kmods = 255) then kmods := 0;
  if (ev.kstate <> kmods) then exit;
  if (mbuts <> 255) and (ev.bstate <> mbuts) then exit;

  if (strEquCI(kname, 'Enter')) then kname := 'RETURN';

  for f := 0 to SDL_NUM_SCANCODES-1 do
  begin
    if strEquCI(kname, SDL_GetScancodeName(f)) then
    begin
      result := (ev.scan = f);
      exit;
    end;
  end;
end;


operator = (const s: AnsiString; constref ev: THKeyEvent): Boolean;
begin
  result := (ev = s);
end;


operator = (constref ev: THMouseEvent; const s: AnsiString): Boolean;
var
  kmods: Byte = 255;
  mbuts: Byte = 255;
  kname: AnsiString;
  but: Integer = -1;
begin
  result := false;

  if (Length(s) > 0) then
  begin
         if (s[1] = '+') then begin if (not ev.press) then exit; end
    else if (s[1] = '-') then begin if (not ev.release) then exit; end
    else if (s[1] = '*') then begin if (not ev.motion) then exit; end
    else if (not ev.press) then exit;
  end;

  kname := parseModKeys(s, kmods, mbuts);
       if strEquCI(kname, 'LMB') then but := THMouseEvent.Left
  else if strEquCI(kname, 'RMB') then but := THMouseEvent.Right
  else if strEquCI(kname, 'MMB') then but := THMouseEvent.Middle
  else if strEquCI(kname, 'WheelUp') or strEquCI(kname, 'WUP') then but := THMouseEvent.WheelUp
  else if strEquCI(kname, 'WheelDown') or strEquCI(kname, 'WDN') or strEquCI(kname, 'WDOWN') then but := THMouseEvent.WheelDown
  else if strEquCI(kname, 'None') then but := 0
  else exit;

  if (mbuts = 255) then mbuts := 0;
  if (kmods = 255) then kmods := 0;
  if (ev.kstate <> kmods) then exit;

  result := (ev.bstate = mbuts) and (ev.but = but);
end;


operator = (const s: AnsiString; constref ev: THMouseEvent): Boolean;
begin
  result := (ev = s);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure resetKMState (sendEvents: Boolean=true);
var
  mask: Word;
  mev: THMouseEvent;
  kev: THKeyEvent;
begin
  // generate mouse release events
  if (curButState <> 0) then
  begin
    if sendEvents then
    begin
      mask := 1;
      while (mask <> 0) do
      begin
        // checked each time, 'cause `evMouseCB` can be changed from the handler
        if ((curButState and mask) <> 0) and assigned(evMouseCB) then
        begin
          mev.kind := mev.TKind.Release;
          mev.x := curMsX;
          mev.y := curMsY;
          mev.dx := 0;
          mev.dy := 0;
          mev.but := mask;
          mev.bstate := curButState;
          mev.kstate := curModState;
          curButState := curButState and (not mask);
          evMouseCB(mev);
        end;
        mask := mask shl 1;
      end;
    end;
    curButState := 0;
  end;

  // generate modifier release events
  if (curModState <> 0) then
  begin
    if sendEvents then
    begin
      mask := 1;
      while (mask <= 8) do
      begin
        // checked each time, 'cause `evMouseCB` can be changed from the handler
        if ((curModState and mask) <> 0) and assigned(evKeyCB) then
        begin
          kev.kind := kev.TKind.Release;
          case mask of
            THKeyEvent.ModCtrl: begin kev.scan := SDL_SCANCODE_LCTRL; kev.sym := SDLK_LCTRL;{arbitrary} end;
            THKeyEvent.ModAlt: begin kev.scan := SDL_SCANCODE_LALT; kev.sym := SDLK_LALT;{arbitrary} end;
            THKeyEvent.ModShift: begin kev.scan := SDL_SCANCODE_LSHIFT; kev.sym := SDLK_LSHIFT;{arbitrary} end;
            THKeyEvent.ModHyper: begin kev.scan := SDL_SCANCODE_LGUI; kev.sym := SDLK_LGUI;{arbitrary} end;
            else assert(false);
          end;
          kev.x := curMsX;
          kev.y := curMsY;
          mev.bstate := 0{curMsButState}; // anyway
          mev.kstate := curModState;
          curModState := curModState and (not mask);
          evKeyCB(kev);
        end;
        mask := mask shl 1;
      end;
    end;
    curModState := 0;
  end;
end;


function onSDLEvent (var ev: TSDL_Event): Boolean;
var
  mev: THMouseEvent;
  kev: THKeyEvent;

  function buildBut (b: Byte): Word;
  begin
    result := 0;
    case b of
      SDL_BUTTON_LEFT: result := result or THMouseEvent.Left;
      SDL_BUTTON_MIDDLE: result := result or THMouseEvent.Middle;
      SDL_BUTTON_RIGHT: result := result or THMouseEvent.Right;
    end;
  end;

begin
  result := false;

  case ev.type_ of
    SDL_KEYDOWN, SDL_KEYUP:
      begin
        // fix left/right modifiers
        if (ev.type_ = SDL_KEYDOWN) then kev.kind := THKeyEvent.TKind.Press else kev.kind := THKeyEvent.TKind.Release;
        kev.scan := ev.key.keysym.scancode;
        kev.sym := ev.key.keysym.sym;

        if (kev.scan = SDL_SCANCODE_RCTRL) then kev.scan := SDL_SCANCODE_LCTRL;
        if (kev.scan = SDL_SCANCODE_RALT) then kev.scan := SDL_SCANCODE_LALT;
        if (kev.scan = SDL_SCANCODE_RSHIFT) then kev.scan := SDL_SCANCODE_LSHIFT;
        if (kev.scan = SDL_SCANCODE_RGUI) then kev.scan := SDL_SCANCODE_LGUI;

        if (kev.sym = SDLK_RCTRL) then kev.sym := SDLK_LCTRL;
        if (kev.sym = SDLK_RALT) then kev.sym := SDLK_LALT;
        if (kev.sym = SDLK_RSHIFT) then kev.sym := SDLK_LSHIFT;
        if (kev.sym = SDLK_RGUI) then kev.sym := SDLK_LGUI;

        kev.x := curMsX;
        kev.y := curMsY;
        kev.bstate := curButState;
        kev.kstate := curModState;

        case kev.scan of
          SDL_SCANCODE_LCTRL: if (kev.press) then curModState := curModState or THKeyEvent.ModCtrl else curModState := curModState and (not THKeyEvent.ModCtrl);
          SDL_SCANCODE_LALT: if (kev.press) then curModState := curModState or THKeyEvent.ModAlt else curModState := curModState and (not THKeyEvent.ModAlt);
          SDL_SCANCODE_LSHIFT: if (kev.press) then curModState := curModState or THKeyEvent.ModShift else curModState := curModState and (not THKeyEvent.ModShift);
        end;

        if assigned(evKeyCB) then result := evKeyCB(kev);
      end;

    SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP:
      begin
        mev.dx := ev.button.x-curMsX;
        mev.dy := ev.button.y-curMsY;
        curMsX := ev.button.x;
        curMsY := ev.button.y;
        if (ev.type_ = SDL_MOUSEBUTTONDOWN) then mev.kind := THMouseEvent.TKind.Press else mev.kind := THMouseEvent.TKind.Release;
        mev.but := buildBut(ev.button.button);
        mev.x := curMsX;
        mev.y := curMsY;
        mev.bstate := curButState;
        mev.kstate := curModState;
        if (mev.but <> 0) then
        begin
          // ev.button.clicks: Byte
          if (ev.type_ = SDL_MOUSEBUTTONDOWN) then curButState := curButState or mev.but else curButState := curButState and (not mev.but);
          if assigned(evMouseCB) then result := evMouseCB(mev);
        end;
      end;
    SDL_MOUSEWHEEL:
      begin
        if (ev.wheel.y <> 0) then
        begin
          mev.dx := 0;
          mev.dy := ev.wheel.y;
          mev.kind := THMouseEvent.TKind.Press;
          if (ev.wheel.y < 0) then mev.but := THMouseEvent.WheelUp else mev.but := THMouseEvent.WheelDown;
          mev.x := curMsX;
          mev.y := curMsY;
          mev.bstate := curButState;
          mev.kstate := curModState;
          if assigned(evMouseCB) then result := evMouseCB(mev);
        end;
      end;
    SDL_MOUSEMOTION:
      begin
        mev.dx := ev.button.x-curMsX;
        mev.dy := ev.button.y-curMsY;
        curMsX := ev.button.x;
        curMsY := ev.button.y;
        mev.kind := THMouseEvent.TKind.Motion;
        mev.but := 0;
        mev.x := curMsX;
        mev.y := curMsY;
        mev.bstate := curButState;
        mev.kstate := curModState;
        if assigned(evMouseCB) then result := evMouseCB(mev);
      end;

    {
    SDL_TEXTINPUT:
      begin
        Utf8ToUnicode(@uc, PChar(ev.text.text), 1);
        keychr := Word(uc);
        if (keychr > 127) then keychr := Word(wchar2win(WideChar(keychr)));
        CharPress(AnsiChar(keychr));
      end;
    }
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure oglSetup2D (winWidth, winHeight: Integer; upsideDown: Boolean=false);
begin
  glViewport(0, 0, winWidth, winHeight);

  glDisable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glDisable(GL_LINE_SMOOTH);
  glDisable(GL_POINT_SMOOTH);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glDisable(GL_DITHER);
  glDisable(GL_STENCIL_TEST);
  glDisable(GL_SCISSOR_TEST);
  glDisable(GL_CULL_FACE);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  if (upsideDown) then
  begin
    glOrtho(0, winWidth, 0, winHeight, -1, 1); // set origin to bottom left
  end
  else
  begin
    glOrtho(0, winWidth, winHeight, 0, -1, 1); // set origin to top left
  end;

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glClearColor(0, 0, 0, 0);
  glColor4f(1, 1, 1, 1);
end;


// ////////////////////////////////////////////////////////////////////////// //
// cursor (hi, Death Track!)
const curTexWidth = 32;
const curTexHeight = 32;
const curWidth = 17;
const curHeight = 23;

const cursorImg: array[0..curWidth*curHeight-1] of Byte = (
  2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  3,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  3,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  3,3,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,
  3,3,4,2,2,0,0,0,0,0,0,0,0,0,0,0,0,
  3,3,4,4,2,2,0,0,0,0,0,0,0,0,0,0,0,
  3,3,4,4,4,2,2,0,0,0,0,0,0,0,0,0,0,
  3,3,4,4,4,4,2,2,0,0,0,0,0,0,0,0,0,
  3,3,4,4,4,5,6,2,2,0,0,0,0,0,0,0,0,
  3,3,4,4,5,6,7,5,2,2,0,0,0,0,0,0,0,
  3,3,4,5,6,7,5,4,5,2,2,0,0,0,0,0,0,
  3,3,5,6,7,5,4,5,6,7,2,2,0,0,0,0,0,
  3,3,6,7,5,4,5,6,7,7,7,2,2,0,0,0,0,
  3,3,7,5,4,5,6,7,7,7,7,7,2,2,0,0,0,
  3,3,5,4,5,6,8,8,8,8,8,8,8,8,2,0,0,
  3,3,4,5,6,3,8,8,8,8,8,8,8,8,8,0,0,
  3,3,5,6,3,3,0,0,0,0,0,0,0,0,0,0,0,
  3,3,6,3,3,0,0,0,0,0,0,0,0,0,0,0,0,
  3,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,
  3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
);
const cursorPal: array[0..9*4-1] of Byte = (
    0,  0,  0,  0,
    0,  0,  0,163,
   85,255,255,255,
   85, 85,255,255,
  255, 85, 85,255,
  170,  0,170,255,
   85, 85, 85,255,
    0,  0,  0,255,
    0,  0,170,255
);


var
  curtexid: GLuint = 0;

procedure createCursorTexture ();
var
  tex, tpp: PByte;
  c: Integer;
  x, y: Integer;
begin
  if (curtexid <> 0) then exit; //begin glDeleteTextures(1, @curtexid); curtexid := 0; end;

  GetMem(tex, curTexWidth*curTexHeight*4);
  try
    FillChar(tex^, curTexWidth*curTexHeight*4, 0);

    // draw shadow
    for y := 0 to curHeight-1 do
    begin
      for x := 0 to curWidth-1 do
      begin
        if (cursorImg[y*curWidth+x] <> 0) then
        begin
          c := 1*4;
          tpp := tex+((y+1)*(curTexWidth*4)+(x+3)*4);
          tpp^ := cursorPal[c+0]; Inc(tpp);
          tpp^ := cursorPal[c+1]; Inc(tpp);
          tpp^ := cursorPal[c+2]; Inc(tpp);
          tpp^ := cursorPal[c+3]; Inc(tpp);
          tpp^ := cursorPal[c+0]; Inc(tpp);
          tpp^ := cursorPal[c+1]; Inc(tpp);
          tpp^ := cursorPal[c+2]; Inc(tpp);
          tpp^ := cursorPal[c+3]; Inc(tpp);
        end;
      end;
    end;

    // draw cursor
    for y := 0 to curHeight-1 do
    begin
      for x := 0 to curWidth-1 do
      begin
        c := cursorImg[y*curWidth+x]*4;
        if (c <> 0) then
        begin
          tpp := tex+(y*(curTexWidth*4)+x*4);
          tpp^ := cursorPal[c+0]; Inc(tpp);
          tpp^ := cursorPal[c+1]; Inc(tpp);
          tpp^ := cursorPal[c+2]; Inc(tpp);
          tpp^ := cursorPal[c+3]; Inc(tpp);
        end;
      end;
    end;

    glGenTextures(1, @curtexid);
    if (curtexid = 0) then raise Exception.Create('can''t create cursor texture');

    glBindTexture(GL_TEXTURE_2D, curtexid);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    //GLfloat[4] bclr = 0.0;
    //glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, bclr.ptr);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, curTexWidth, curTexHeight, 0, GL_RGBA{gltt}, GL_UNSIGNED_BYTE, tex);
    glFlush();
  finally
    FreeMem(tex);
  end;
end;

procedure oglDrawCursorAt (msX, msY: Integer);
begin
  //if (curtexid = 0) then createCursorTexture() else glBindTexture(GL_TEXTURE_2D, curtexid);
  glBindTexture(GL_TEXTURE_2D, curtexid);
  // blend it
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_TEXTURE_2D);
  glDisable(GL_STENCIL_TEST);
  glDisable(GL_SCISSOR_TEST);
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_CULL_FACE);
  // color and opacity
  glColor4f(1, 1, 1, 0.9);
  //Dec(msX, 2);
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0); glVertex2i(msX, msY); // top-left
    glTexCoord2f(1.0, 0.0); glVertex2i(msX+curTexWidth, msY); // top-right
    glTexCoord2f(1.0, 1.0); glVertex2i(msX+curTexWidth, msY+curTexHeight); // bottom-right
    glTexCoord2f(0.0, 1.0); glVertex2i(msX, msY+curTexHeight); // bottom-left
  glEnd();
  //Inc(msX, 2);
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, 0);
end;

procedure oglDrawCursor (); begin oglDrawCursorAt(curMsX, curMsY); end;


// ////////////////////////////////////////////////////////////////////////// //
// fonts
const kgiFont6: array[0..256*8-1] of Byte  = (
$00,$00,$00,$00,$00,$00,$00,$00,$3c,$42,$a5,$81,$a5,$99,$42,$3c,$3c,$7e,$db,$ff,$ff,$db,$66,$3c,$6c,$fe,
$fe,$fe,$7c,$38,$10,$00,$10,$38,$7c,$fe,$7c,$38,$10,$00,$10,$38,$54,$fe,$54,$10,$38,$00,$10,$38,$7c,$fe,
$fe,$10,$38,$00,$00,$00,$00,$30,$30,$00,$00,$00,$ff,$ff,$ff,$e7,$e7,$ff,$ff,$ff,$38,$44,$82,$82,$82,$44,
$38,$00,$c7,$bb,$7d,$7d,$7d,$bb,$c7,$ff,$0f,$03,$05,$79,$88,$88,$88,$70,$38,$44,$44,$44,$38,$10,$7c,$10,
$30,$28,$24,$24,$28,$20,$e0,$c0,$3c,$24,$3c,$24,$24,$e4,$dc,$18,$10,$54,$38,$ee,$38,$54,$10,$00,$10,$10,
$10,$7c,$10,$10,$10,$10,$10,$10,$10,$ff,$00,$00,$00,$00,$00,$00,$00,$ff,$10,$10,$10,$10,$10,$10,$10,$f0,
$10,$10,$10,$10,$10,$10,$10,$1f,$10,$10,$10,$10,$10,$10,$10,$ff,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
$10,$10,$00,$00,$00,$ff,$00,$00,$00,$00,$00,$00,$00,$1f,$10,$10,$10,$10,$00,$00,$00,$f0,$10,$10,$10,$10,
$10,$10,$10,$1f,$00,$00,$00,$00,$10,$10,$10,$f0,$00,$00,$00,$00,$81,$42,$24,$18,$18,$24,$42,$81,$01,$02,
$04,$08,$10,$20,$40,$80,$80,$40,$20,$10,$08,$04,$02,$01,$00,$10,$10,$ff,$10,$10,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$20,$20,$20,$20,$00,$00,$20,$00,$50,$50,$50,$00,$00,$00,$00,$00,$50,$50,$f8,$50,$f8,$50,
$50,$00,$20,$78,$a0,$70,$28,$f0,$20,$00,$c0,$c8,$10,$20,$40,$98,$18,$00,$40,$a0,$40,$a8,$90,$98,$60,$00,
$10,$20,$40,$00,$00,$00,$00,$00,$10,$20,$40,$40,$40,$20,$10,$00,$40,$20,$10,$10,$10,$20,$40,$00,$88,$50,
$20,$f8,$20,$50,$88,$00,$00,$20,$20,$f8,$20,$20,$00,$00,$00,$00,$00,$00,$00,$20,$20,$40,$00,$00,$00,$78,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$60,$60,$00,$00,$00,$08,$10,$20,$40,$80,$00,$70,$88,$98,$a8,$c8,$88,
$70,$00,$20,$60,$a0,$20,$20,$20,$f8,$00,$70,$88,$08,$10,$60,$80,$f8,$00,$70,$88,$08,$30,$08,$88,$70,$00,
$10,$30,$50,$90,$f8,$10,$10,$00,$f8,$80,$e0,$10,$08,$10,$e0,$00,$30,$40,$80,$f0,$88,$88,$70,$00,$f8,$88,
$10,$20,$20,$20,$20,$00,$70,$88,$88,$70,$88,$88,$70,$00,$70,$88,$88,$78,$08,$10,$60,$00,$00,$00,$20,$00,
$00,$20,$00,$00,$00,$00,$20,$00,$00,$20,$20,$40,$18,$30,$60,$c0,$60,$30,$18,$00,$00,$00,$f8,$00,$f8,$00,
$00,$00,$c0,$60,$30,$18,$30,$60,$c0,$00,$70,$88,$08,$10,$20,$00,$20,$00,$70,$88,$08,$68,$a8,$a8,$70,$00,
$20,$50,$88,$88,$f8,$88,$88,$00,$f0,$48,$48,$70,$48,$48,$f0,$00,$30,$48,$80,$80,$80,$48,$30,$00,$e0,$50,
$48,$48,$48,$50,$e0,$00,$f8,$80,$80,$f0,$80,$80,$f8,$00,$f8,$80,$80,$f0,$80,$80,$80,$00,$70,$88,$80,$b8,
$88,$88,$70,$00,$88,$88,$88,$f8,$88,$88,$88,$00,$70,$20,$20,$20,$20,$20,$70,$00,$38,$10,$10,$10,$90,$90,
$60,$00,$88,$90,$a0,$c0,$a0,$90,$88,$00,$80,$80,$80,$80,$80,$80,$f8,$00,$88,$d8,$a8,$a8,$88,$88,$88,$00,
$88,$c8,$c8,$a8,$98,$98,$88,$00,$70,$88,$88,$88,$88,$88,$70,$00,$f0,$88,$88,$f0,$80,$80,$80,$00,$70,$88,
$88,$88,$a8,$90,$68,$00,$f0,$88,$88,$f0,$a0,$90,$88,$00,$70,$88,$80,$70,$08,$88,$70,$00,$f8,$20,$20,$20,
$20,$20,$20,$00,$88,$88,$88,$88,$88,$88,$70,$00,$88,$88,$88,$88,$50,$50,$20,$00,$88,$88,$88,$a8,$a8,$d8,
$88,$00,$88,$88,$50,$20,$50,$88,$88,$00,$88,$88,$88,$70,$20,$20,$20,$00,$f8,$08,$10,$20,$40,$80,$f8,$00,
$70,$40,$40,$40,$40,$40,$70,$00,$00,$00,$80,$40,$20,$10,$08,$00,$70,$10,$10,$10,$10,$10,$70,$00,$20,$50,
$88,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$f8,$00,$40,$20,$10,$00,$00,$00,$00,$00,$00,$00,$70,$08,
$78,$88,$78,$00,$80,$80,$b0,$c8,$88,$c8,$b0,$00,$00,$00,$70,$88,$80,$88,$70,$00,$08,$08,$68,$98,$88,$98,
$68,$00,$00,$00,$70,$88,$f8,$80,$70,$00,$10,$28,$20,$f8,$20,$20,$20,$00,$00,$00,$68,$98,$98,$68,$08,$70,
$80,$80,$f0,$88,$88,$88,$88,$00,$20,$00,$60,$20,$20,$20,$70,$00,$10,$00,$30,$10,$10,$10,$90,$60,$40,$40,
$48,$50,$60,$50,$48,$00,$60,$20,$20,$20,$20,$20,$70,$00,$00,$00,$d0,$a8,$a8,$a8,$a8,$00,$00,$00,$b0,$c8,
$88,$88,$88,$00,$00,$00,$70,$88,$88,$88,$70,$00,$00,$00,$b0,$c8,$c8,$b0,$80,$80,$00,$00,$68,$98,$98,$68,
$08,$08,$00,$00,$b0,$c8,$80,$80,$80,$00,$00,$00,$78,$80,$f0,$08,$f0,$00,$40,$40,$f0,$40,$40,$48,$30,$00,
$00,$00,$90,$90,$90,$90,$68,$00,$00,$00,$88,$88,$88,$50,$20,$00,$00,$00,$88,$a8,$a8,$a8,$50,$00,$00,$00,
$88,$50,$20,$50,$88,$00,$00,$00,$88,$88,$98,$68,$08,$70,$00,$00,$f8,$10,$20,$40,$f8,$00,$18,$20,$20,$40,
$20,$20,$18,$00,$20,$20,$20,$00,$20,$20,$20,$00,$c0,$20,$20,$10,$20,$20,$c0,$00,$40,$a8,$10,$00,$00,$00,
$00,$00,$00,$00,$20,$50,$f8,$00,$00,$00,$00,$00,$00,$00,$00,$00,$ff,$ff,$f0,$f0,$f0,$f0,$0f,$0f,$0f,$0f,
$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3c,$3c,$00,$00,$00,$ff,$ff,
$ff,$ff,$ff,$ff,$00,$00,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$0f,$0f,$0f,$0f,$f0,$f0,$f0,$f0,$fc,$fc,$fc,$fc,
$fc,$fc,$fc,$fc,$03,$03,$03,$03,$03,$03,$03,$03,$3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f,$11,$22,$44,$88,$11,$22,
$44,$88,$88,$44,$22,$11,$88,$44,$22,$11,$fe,$7c,$38,$10,$00,$00,$00,$00,$00,$00,$00,$00,$10,$38,$7c,$fe,
$80,$c0,$e0,$f0,$e0,$c0,$80,$00,$01,$03,$07,$0f,$07,$03,$01,$00,$ff,$7e,$3c,$18,$18,$3c,$7e,$ff,$81,$c3,
$e7,$ff,$ff,$e7,$c3,$81,$f0,$f0,$f0,$f0,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,
$00,$00,$00,$00,$00,$00,$00,$00,$f0,$f0,$f0,$f0,$33,$33,$cc,$cc,$33,$33,$cc,$cc,$00,$20,$20,$50,$50,$88,
$f8,$00,$20,$20,$70,$20,$70,$20,$20,$00,$00,$00,$00,$50,$88,$a8,$50,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,
$00,$00,$00,$00,$ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$ff,$ff,
$ff,$ff,$00,$00,$00,$00,$00,$00,$68,$90,$90,$90,$68,$00,$30,$48,$48,$70,$48,$48,$70,$c0,$f8,$88,$80,$80,
$80,$80,$80,$00,$00,$50,$70,$88,$f8,$80,$70,$00,$00,$00,$78,$80,$f0,$80,$78,$00,$00,$00,$78,$90,$90,$90,
$60,$00,$20,$00,$60,$20,$20,$20,$70,$00,$50,$00,$70,$20,$20,$20,$70,$00,$f8,$20,$70,$a8,$a8,$70,$20,$f8,
$20,$50,$88,$f8,$88,$50,$20,$00,$70,$88,$88,$88,$50,$50,$d8,$00,$30,$40,$40,$20,$50,$50,$50,$20,$00,$00,
$00,$50,$a8,$a8,$50,$00,$08,$70,$a8,$a8,$a8,$70,$80,$00,$38,$40,$80,$f8,$80,$40,$38,$00,$70,$88,$88,$88,
$88,$88,$88,$00,$00,$f8,$00,$f8,$00,$f8,$00,$00,$20,$20,$f8,$20,$20,$00,$f8,$00,$c0,$30,$08,$30,$c0,$00,
$f8,$00,$50,$f8,$80,$f0,$80,$80,$f8,$00,$78,$80,$80,$f0,$80,$80,$78,$00,$20,$20,$20,$20,$20,$20,$a0,$40,
$70,$20,$20,$20,$20,$20,$70,$00,$50,$70,$20,$20,$20,$20,$70,$00,$00,$18,$24,$24,$18,$00,$00,$00,$00,$30,
$78,$78,$30,$00,$00,$00,$00,$00,$00,$00,$30,$00,$00,$00,$3e,$20,$20,$20,$a0,$60,$20,$00,$a0,$50,$50,$50,
$00,$00,$00,$00,$40,$a0,$20,$40,$e0,$00,$00,$00,$00,$38,$38,$38,$38,$38,$38,$00,$3c,$42,$99,$a1,$a1,$99,
$42,$3c,$00,$00,$90,$a8,$e8,$a8,$90,$00,$00,$00,$60,$10,$70,$90,$68,$00,$00,$00,$f0,$80,$f0,$88,$f0,$00,
$00,$00,$90,$90,$90,$f8,$08,$00,$00,$00,$30,$50,$50,$70,$88,$00,$00,$00,$70,$88,$f8,$80,$70,$00,$00,$20,
$70,$a8,$a8,$70,$20,$00,$00,$00,$78,$48,$40,$40,$40,$00,$00,$00,$88,$50,$20,$50,$88,$00,$00,$00,$88,$98,
$a8,$c8,$88,$00,$00,$50,$20,$00,$98,$a8,$c8,$00,$00,$00,$90,$a0,$c0,$a0,$90,$00,$00,$00,$38,$28,$28,$48,
$88,$00,$00,$00,$88,$d8,$a8,$88,$88,$00,$00,$00,$88,$88,$f8,$88,$88,$00,$00,$00,$70,$88,$88,$88,$70,$00,
$00,$00,$78,$48,$48,$48,$48,$00,$00,$00,$78,$88,$78,$28,$48,$00,$00,$00,$f0,$88,$f0,$80,$80,$00,$00,$00,
$78,$80,$80,$80,$78,$00,$00,$00,$f8,$20,$20,$20,$20,$00,$00,$00,$88,$50,$20,$40,$80,$00,$00,$00,$a8,$70,
$20,$70,$a8,$00,$00,$00,$f0,$48,$70,$48,$f0,$00,$00,$00,$40,$40,$70,$48,$70,$00,$00,$00,$88,$88,$c8,$a8,
$c8,$00,$00,$00,$f0,$08,$70,$08,$f0,$00,$00,$00,$a8,$a8,$a8,$a8,$f8,$00,$00,$00,$70,$88,$38,$88,$70,$00,
$00,$00,$a8,$a8,$a8,$f8,$08,$00,$00,$00,$48,$48,$78,$08,$08,$00,$00,$00,$c0,$40,$70,$48,$70,$00,$90,$a8,
$a8,$e8,$a8,$a8,$90,$00,$20,$50,$88,$88,$f8,$88,$88,$00,$f8,$88,$80,$f0,$88,$88,$f0,$00,$90,$90,$90,$90,
$90,$f8,$08,$00,$38,$28,$28,$48,$48,$f8,$88,$00,$f8,$80,$80,$f0,$80,$80,$f8,$00,$20,$70,$a8,$a8,$a8,$70,
$20,$00,$f8,$88,$88,$80,$80,$80,$80,$00,$88,$88,$50,$20,$50,$88,$88,$00,$88,$88,$98,$a8,$c8,$88,$88,$00,
$50,$20,$88,$98,$a8,$c8,$88,$00,$88,$90,$a0,$c0,$a0,$90,$88,$00,$18,$28,$48,$48,$48,$48,$88,$00,$88,$d8,
$a8,$a8,$88,$88,$88,$00,$88,$88,$88,$f8,$88,$88,$88,$00,$70,$88,$88,$88,$88,$88,$70,$00,$f8,$88,$88,$88,
$88,$88,$88,$00,$78,$88,$88,$78,$28,$48,$88,$00,$f0,$88,$88,$f0,$80,$80,$80,$00,$70,$88,$80,$80,$80,$88,
$70,$00,$f8,$20,$20,$20,$20,$20,$20,$00,$88,$88,$88,$50,$20,$40,$80,$00,$a8,$a8,$70,$20,$70,$a8,$a8,$00,
$f0,$48,$48,$70,$48,$48,$f0,$00,$80,$80,$80,$f0,$88,$88,$f0,$00,$88,$88,$88,$c8,$a8,$a8,$c8,$00,$f0,$08,
$08,$30,$08,$08,$f0,$00,$a8,$a8,$a8,$a8,$a8,$a8,$f8,$00,$70,$88,$08,$78,$08,$88,$70,$00,$a8,$a8,$a8,$a8,
$a8,$f8,$08,$00,$88,$88,$88,$88,$78,$08,$08,$00,$c0,$40,$40,$70,$48,$48,$70,$00
);

const kgiFont8: array[0..256*8-1] of Byte  = (
$00,$00,$00,$00,$00,$00,$00,$00,$7e,$81,$a5,$81,$bd,$99,$81,$7e,$7e,$ff,$db,$ff,$c3,$e7,$ff,$7e,$6c,$fe,
$fe,$fe,$7c,$38,$10,$00,$10,$38,$7c,$fe,$7c,$38,$10,$00,$38,$7c,$38,$fe,$fe,$d6,$10,$38,$10,$10,$38,$7c,
$fe,$7c,$10,$38,$00,$00,$18,$3c,$3c,$18,$00,$00,$ff,$ff,$e7,$c3,$c3,$e7,$ff,$ff,$00,$3c,$66,$42,$42,$66,
$3c,$00,$ff,$c3,$99,$bd,$bd,$99,$c3,$ff,$0f,$07,$0f,$7d,$cc,$cc,$cc,$78,$3c,$66,$66,$66,$3c,$18,$7e,$18,
$3f,$33,$3f,$30,$30,$70,$f0,$e0,$7f,$63,$7f,$63,$63,$67,$e6,$c0,$99,$5a,$3c,$e7,$e7,$3c,$5a,$99,$80,$e0,
$f8,$fe,$f8,$e0,$80,$00,$02,$0e,$3e,$fe,$3e,$0e,$02,$00,$18,$3c,$7e,$18,$18,$7e,$3c,$18,$66,$66,$66,$66,
$66,$00,$66,$00,$7f,$db,$db,$7b,$1b,$1b,$1b,$00,$7e,$c3,$78,$cc,$cc,$78,$8c,$f8,$00,$00,$00,$00,$7e,$7e,
$7e,$00,$18,$3c,$7e,$18,$7e,$3c,$18,$ff,$18,$3c,$7e,$18,$18,$18,$18,$00,$18,$18,$18,$18,$7e,$3c,$18,$00,
$00,$18,$0c,$fe,$0c,$18,$00,$00,$00,$30,$60,$fe,$60,$30,$00,$00,$00,$00,$c0,$c0,$c0,$fe,$00,$00,$00,$24,
$66,$ff,$66,$24,$00,$00,$00,$18,$3c,$7e,$ff,$ff,$00,$00,$00,$ff,$ff,$7e,$3c,$18,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$30,$78,$78,$30,$30,$00,$30,$00,$6c,$6c,$6c,$00,$00,$00,$00,$00,$6c,$6c,$fe,$6c,$fe,$6c,
$6c,$00,$30,$7c,$c0,$78,$0c,$f8,$30,$00,$00,$c6,$cc,$18,$30,$66,$c6,$00,$38,$6c,$38,$76,$dc,$cc,$76,$00,
$60,$60,$c0,$00,$00,$00,$00,$00,$18,$30,$60,$60,$60,$30,$18,$00,$60,$30,$18,$18,$18,$30,$60,$00,$00,$66,
$3c,$ff,$3c,$66,$00,$00,$00,$30,$30,$fc,$30,$30,$00,$00,$00,$00,$00,$00,$00,$70,$30,$60,$00,$00,$00,$fc,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$30,$30,$00,$06,$0c,$18,$30,$60,$c0,$80,$00,$78,$cc,$dc,$fc,$ec,$cc,
$78,$00,$30,$f0,$30,$30,$30,$30,$fc,$00,$78,$cc,$0c,$38,$60,$cc,$fc,$00,$78,$cc,$0c,$38,$0c,$cc,$78,$00,
$1c,$3c,$6c,$cc,$fe,$0c,$0c,$00,$fc,$c0,$f8,$0c,$0c,$cc,$78,$00,$38,$60,$c0,$f8,$cc,$cc,$78,$00,$fc,$cc,
$0c,$18,$30,$60,$60,$00,$78,$cc,$cc,$78,$cc,$cc,$78,$00,$78,$cc,$cc,$7c,$0c,$18,$70,$00,$00,$00,$30,$30,
$00,$30,$30,$00,$00,$00,$30,$30,$00,$70,$30,$60,$18,$30,$60,$c0,$60,$30,$18,$00,$00,$00,$fc,$00,$fc,$00,
$00,$00,$60,$30,$18,$0c,$18,$30,$60,$00,$78,$cc,$0c,$18,$30,$00,$30,$00,$7c,$c6,$de,$de,$de,$c0,$78,$00,
$30,$78,$cc,$cc,$fc,$cc,$cc,$00,$fc,$66,$66,$7c,$66,$66,$fc,$00,$3c,$66,$c0,$c0,$c0,$66,$3c,$00,$fc,$6c,
$66,$66,$66,$6c,$fc,$00,$fe,$62,$68,$78,$68,$62,$fe,$00,$fe,$62,$68,$78,$68,$60,$f0,$00,$3c,$66,$c0,$c0,
$ce,$66,$3e,$00,$cc,$cc,$cc,$fc,$cc,$cc,$cc,$00,$78,$30,$30,$30,$30,$30,$78,$00,$1e,$0c,$0c,$0c,$cc,$cc,
$78,$00,$e6,$66,$6c,$78,$6c,$66,$e6,$00,$f0,$60,$60,$60,$62,$66,$fe,$00,$c6,$ee,$fe,$d6,$c6,$c6,$c6,$00,
$c6,$e6,$f6,$de,$ce,$c6,$c6,$00,$38,$6c,$c6,$c6,$c6,$6c,$38,$00,$fc,$66,$66,$7c,$60,$60,$f0,$00,$78,$cc,
$cc,$cc,$dc,$78,$1c,$00,$fc,$66,$66,$7c,$78,$6c,$e6,$00,$78,$cc,$e0,$38,$1c,$cc,$78,$00,$fc,$b4,$30,$30,
$30,$30,$78,$00,$cc,$cc,$cc,$cc,$cc,$cc,$fc,$00,$cc,$cc,$cc,$cc,$cc,$78,$30,$00,$c6,$c6,$c6,$d6,$fe,$ee,
$c6,$00,$c6,$c6,$6c,$38,$6c,$c6,$c6,$00,$cc,$cc,$cc,$78,$30,$30,$78,$00,$fe,$cc,$98,$30,$62,$c6,$fe,$00,
$78,$60,$60,$60,$60,$60,$78,$00,$c0,$60,$30,$18,$0c,$06,$02,$00,$78,$18,$18,$18,$18,$18,$78,$00,$10,$38,
$6c,$c6,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$ff,$30,$30,$18,$00,$00,$00,$00,$00,$00,$00,$78,$0c,
$7c,$cc,$76,$00,$e0,$60,$7c,$66,$66,$66,$bc,$00,$00,$00,$78,$cc,$c0,$cc,$78,$00,$1c,$0c,$0c,$7c,$cc,$cc,
$76,$00,$00,$00,$78,$cc,$fc,$c0,$78,$00,$38,$6c,$60,$f0,$60,$60,$f0,$00,$00,$00,$76,$cc,$cc,$7c,$0c,$f8,
$e0,$60,$6c,$76,$66,$66,$e6,$00,$30,$00,$70,$30,$30,$30,$78,$00,$18,$00,$78,$18,$18,$18,$d8,$70,$e0,$60,
$66,$6c,$78,$6c,$e6,$00,$70,$30,$30,$30,$30,$30,$78,$00,$00,$00,$ec,$fe,$d6,$c6,$c6,$00,$00,$00,$f8,$cc,
$cc,$cc,$cc,$00,$00,$00,$78,$cc,$cc,$cc,$78,$00,$00,$00,$dc,$66,$66,$7c,$60,$f0,$00,$00,$76,$cc,$cc,$7c,
$0c,$1e,$00,$00,$d8,$6c,$6c,$60,$f0,$00,$00,$00,$7c,$c0,$78,$0c,$f8,$00,$10,$30,$7c,$30,$30,$34,$18,$00,
$00,$00,$cc,$cc,$cc,$cc,$76,$00,$00,$00,$cc,$cc,$cc,$78,$30,$00,$00,$00,$c6,$c6,$d6,$fe,$6c,$00,$00,$00,
$c6,$6c,$38,$6c,$c6,$00,$00,$00,$cc,$cc,$cc,$7c,$0c,$f8,$00,$00,$fc,$98,$30,$64,$fc,$00,$1c,$30,$30,$e0,
$30,$30,$1c,$00,$18,$18,$18,$00,$18,$18,$18,$00,$e0,$30,$30,$1c,$30,$30,$e0,$00,$76,$dc,$00,$00,$00,$00,
$00,$00,$10,$38,$6c,$c6,$c6,$c6,$fe,$00,$78,$cc,$c0,$cc,$78,$18,$0c,$78,$00,$cc,$00,$cc,$cc,$cc,$7e,$00,
$1c,$00,$78,$cc,$fc,$c0,$78,$00,$7e,$c3,$3c,$06,$3e,$66,$3f,$00,$cc,$00,$78,$0c,$7c,$cc,$7e,$00,$e0,$00,
$78,$0c,$7c,$cc,$7e,$00,$30,$30,$78,$0c,$7c,$cc,$7e,$00,$00,$00,$7c,$c0,$c0,$7c,$06,$3c,$7e,$c3,$3c,$66,
$7e,$60,$3c,$00,$cc,$00,$78,$cc,$fc,$c0,$78,$00,$e0,$00,$78,$cc,$fc,$c0,$78,$00,$cc,$00,$70,$30,$30,$30,
$78,$00,$7c,$c6,$38,$18,$18,$18,$3c,$00,$e0,$00,$70,$30,$30,$30,$78,$00,$cc,$30,$78,$cc,$cc,$fc,$cc,$00,
$30,$30,$00,$78,$cc,$fc,$cc,$00,$1c,$00,$fc,$60,$78,$60,$fc,$00,$00,$00,$7f,$0c,$7f,$cc,$7f,$00,$3e,$6c,
$cc,$fe,$cc,$cc,$ce,$00,$78,$cc,$00,$78,$cc,$cc,$78,$00,$00,$cc,$00,$78,$cc,$cc,$78,$00,$00,$e0,$00,$78,
$cc,$cc,$78,$00,$78,$cc,$00,$cc,$cc,$cc,$7e,$00,$00,$e0,$00,$cc,$cc,$cc,$7e,$00,$00,$cc,$00,$cc,$cc,$fc,
$0c,$f8,$c6,$38,$7c,$c6,$c6,$7c,$38,$00,$cc,$00,$cc,$cc,$cc,$cc,$78,$00,$18,$18,$7e,$c0,$c0,$7e,$18,$18,
$38,$6c,$64,$f0,$60,$e6,$fc,$00,$cc,$cc,$78,$fc,$30,$fc,$30,$00,$f0,$d8,$d8,$f4,$cc,$de,$cc,$0e,$0e,$1b,
$18,$7e,$18,$18,$d8,$70,$1c,$00,$78,$0c,$7c,$cc,$7e,$00,$38,$00,$70,$30,$30,$30,$78,$00,$00,$1c,$00,$78,
$cc,$cc,$78,$00,$00,$1c,$00,$cc,$cc,$cc,$7e,$00,$00,$f8,$00,$f8,$cc,$cc,$cc,$00,$fc,$00,$cc,$ec,$fc,$dc,
$cc,$00,$3c,$6c,$6c,$3e,$00,$7e,$00,$00,$3c,$66,$66,$3c,$00,$7e,$00,$00,$30,$00,$30,$60,$c0,$cc,$78,$00,
$00,$00,$00,$fc,$c0,$c0,$00,$00,$00,$00,$00,$fc,$0c,$0c,$00,$00,$c6,$cc,$d8,$3e,$63,$ce,$98,$1f,$c6,$cc,
$d8,$f3,$67,$cf,$9f,$03,$00,$18,$00,$18,$18,$3c,$3c,$18,$00,$33,$66,$cc,$66,$33,$00,$00,$00,$cc,$66,$33,
$66,$cc,$00,$00,$22,$88,$22,$88,$22,$88,$22,$88,$55,$aa,$55,$aa,$55,$aa,$55,$aa,$dc,$76,$dc,$76,$dc,$76,
$dc,$76,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$f8,$18,$18,$18,$18,$18,$f8,$18,$f8,$18,$18,$18,
$36,$36,$36,$36,$f6,$36,$36,$36,$00,$00,$00,$00,$fe,$36,$36,$36,$00,$00,$f8,$18,$f8,$18,$18,$18,$36,$36,
$f6,$06,$f6,$36,$36,$36,$36,$36,$36,$36,$36,$36,$36,$36,$00,$00,$fe,$06,$f6,$36,$36,$36,$36,$36,$f6,$06,
$fe,$00,$00,$00,$36,$36,$36,$36,$fe,$00,$00,$00,$18,$18,$f8,$18,$f8,$00,$00,$00,$00,$00,$00,$00,$f8,$18,
$18,$18,$18,$18,$18,$18,$1f,$00,$00,$00,$18,$18,$18,$18,$ff,$00,$00,$00,$00,$00,$00,$00,$ff,$18,$18,$18,
$18,$18,$18,$18,$1f,$18,$18,$18,$00,$00,$00,$00,$ff,$00,$00,$00,$18,$18,$18,$18,$ff,$18,$18,$18,$18,$18,
$1f,$18,$1f,$18,$18,$18,$36,$36,$36,$36,$37,$36,$36,$36,$36,$36,$37,$30,$3f,$00,$00,$00,$00,$00,$3f,$30,
$37,$36,$36,$36,$36,$36,$f7,$00,$ff,$00,$00,$00,$00,$00,$ff,$00,$f7,$36,$36,$36,$36,$36,$37,$30,$37,$36,
$36,$36,$00,$00,$ff,$00,$ff,$00,$00,$00,$36,$36,$f7,$00,$f7,$36,$36,$36,$18,$18,$ff,$00,$ff,$00,$00,$00,
$36,$36,$36,$36,$ff,$00,$00,$00,$00,$00,$ff,$00,$ff,$18,$18,$18,$00,$00,$00,$00,$ff,$36,$36,$36,$36,$36,
$36,$36,$3f,$00,$00,$00,$18,$18,$1f,$18,$1f,$00,$00,$00,$00,$00,$1f,$18,$1f,$18,$18,$18,$00,$00,$00,$00,
$3f,$36,$36,$36,$36,$36,$36,$36,$f7,$36,$36,$36,$18,$18,$ff,$00,$ff,$18,$18,$18,$18,$18,$18,$18,$f8,$00,
$00,$00,$00,$00,$00,$00,$1f,$18,$18,$18,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,
$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$ff,$ff,$ff,$ff,$00,$00,$00,$00,$00,$00,
$76,$dc,$c8,$dc,$76,$00,$00,$78,$cc,$f8,$cc,$f8,$c0,$c0,$00,$fe,$c6,$c0,$c0,$c0,$c0,$00,$00,$fe,$6c,$6c,
$6c,$6c,$6c,$00,$fe,$66,$30,$18,$30,$66,$fe,$00,$00,$00,$7e,$cc,$cc,$cc,$78,$00,$00,$66,$66,$66,$66,$7c,
$60,$c0,$00,$76,$dc,$18,$18,$18,$18,$00,$fc,$30,$78,$cc,$cc,$78,$30,$fc,$38,$6c,$c6,$fe,$c6,$6c,$38,$00,
$38,$6c,$c6,$c6,$6c,$6c,$ee,$00,$1c,$30,$18,$7c,$cc,$cc,$78,$00,$00,$00,$7e,$db,$db,$7e,$00,$00,$06,$0c,
$7e,$db,$db,$7e,$60,$c0,$3c,$60,$c0,$fc,$c0,$60,$3c,$00,$78,$cc,$cc,$cc,$cc,$cc,$cc,$00,$00,$fc,$00,$fc,
$00,$fc,$00,$00,$30,$30,$fc,$30,$30,$00,$fc,$00,$60,$30,$18,$30,$60,$00,$fc,$00,$18,$30,$60,$30,$18,$00,
$fc,$00,$0e,$1b,$1b,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$d8,$d8,$70,$30,$30,$00,$fc,$00,$30,$30,$00,
$00,$72,$9c,$00,$72,$9c,$00,$00,$38,$6c,$6c,$38,$00,$00,$00,$00,$00,$00,$00,$18,$18,$00,$00,$00,$00,$00,
$00,$00,$18,$00,$00,$00,$0f,$0c,$0c,$0c,$ec,$6c,$3c,$1c,$78,$6c,$6c,$6c,$6c,$00,$00,$00,$78,$0c,$38,$60,
$7c,$00,$00,$00,$00,$00,$3c,$3c,$3c,$3c,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
);

const kgiFont6PropWidth: array[0..256-1] of Byte = (
  $08,$08,$08,$07,$07,$07,$07,$04,$08,$07,$08,$08,$06,$06,$06,$07,
  $06,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,
  $85,$21,$13,$05,$05,$05,$05,$13,$13,$13,$05,$05,$12,$14,$12,$05,
  $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$21,$12,$05,$05,$05,$05,
  $05,$05,$05,$05,$05,$05,$05,$05,$05,$13,$05,$05,$05,$05,$05,$05,
  $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$13,$05,$13,$05,$05,
  $13,$05,$05,$05,$05,$05,$05,$05,$05,$13,$04,$14,$13,$05,$05,$05,
  $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$14,$21,$04,$05,$08,
  $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$04,
  $44,$08,$08,$08,$08,$08,$08,$08,$05,$04,$05,$08,$08,$08,$08,$08,
  $05,$05,$05,$05,$05,$05,$13,$13,$05,$05,$05,$04,$05,$05,$05,$05,
  $05,$05,$05,$05,$05,$03,$04,$04,$06,$05,$04,$07,$04,$03,$05,$08,
  $05,$05,$05,$05,$05,$05,$05,$14,$05,$05,$05,$04,$05,$05,$05,$05,
  $14,$05,$05,$05,$05,$05,$05,$05,$14,$05,$05,$05,$05,$05,$14,$05,
  $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,
  $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
);

const kgiFont8PropWidth: array[0..256-1] of Byte = (
  $08,$08,$08,$07,$07,$07,$07,$06,$08,$07,$08,$08,$07,$08,$08,$08,
  $07,$07,$07,$07,$08,$08,$07,$08,$07,$07,$07,$07,$07,$08,$08,$08,
  $85,$14,$15,$07,$06,$07,$07,$03,$14,$14,$08,$06,$13,$06,$22,$07,
  $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$22,$13,$05,$06,$15,$06,
  $07,$06,$07,$07,$07,$07,$07,$07,$06,$14,$07,$07,$07,$07,$07,$07,
  $07,$06,$07,$06,$06,$06,$06,$07,$07,$06,$07,$14,$07,$14,$07,$08,
  $23,$07,$07,$06,$07,$06,$06,$07,$07,$14,$05,$07,$14,$07,$06,$06,
  $07,$07,$06,$06,$15,$07,$06,$07,$07,$06,$06,$06,$32,$06,$07,$07,
  $06,$07,$06,$08,$07,$07,$07,$07,$08,$06,$06,$06,$07,$05,$06,$06,
  $06,$08,$07,$06,$06,$06,$07,$07,$06,$07,$06,$07,$07,$06,$07,$08,
  $07,$05,$06,$07,$06,$06,$16,$16,$06,$06,$06,$08,$08,$06,$08,$08,
  $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,
  $38,$08,$08,$38,$08,$08,$38,$28,$28,$28,$08,$08,$28,$08,$08,$08,
  $08,$08,$08,$28,$38,$38,$28,$08,$08,$08,$38,$08,$08,$08,$48,$08,
  $07,$06,$07,$07,$07,$07,$07,$07,$06,$07,$07,$06,$08,$08,$06,$06,
  $06,$06,$06,$06,$35,$05,$06,$07,$15,$32,$32,$08,$15,$15,$24,$08
);


function createFontTexture (constref font: array of Byte; constref fontwdt: array of Byte; prop: Boolean): GLuint;
const
  Width = 16*8;
  Height = 16*8;
var
  tex, tpp: PByte;
  b: Byte;
  cc: Integer;
  x, y, dx, dy: Integer;
begin
  GetMem(tex, Width*Height*4);

  for cc := 0 to 255 do
  begin
    x := (cc mod 16)*8;
    y := (cc div 16)*8;
    for dy := 0 to 7 do
    begin
      b := font[cc*8+dy];
      if prop then b := b shl (fontwdt[cc] shr 4);
      tpp := tex+((y+dy)*(Width*4))+x*4;
      for dx := 0 to 7 do
      begin
        if ((b and $80) <> 0) then
        begin
          tpp^ := 255; Inc(tpp);
          tpp^ := 255; Inc(tpp);
          tpp^ := 255; Inc(tpp);
          tpp^ := 255; Inc(tpp);
        end
        else
        begin
          tpp^ := 0; Inc(tpp);
          tpp^ := 0; Inc(tpp);
          tpp^ := 0; Inc(tpp);
          tpp^ := 0; Inc(tpp);
        end;
        b := (b and $7f) shl 1;
      end;
    end;
  end;

  glGenTextures(1, @result);
  if (result = 0) then raise Exception.Create('can''t create Holmes font texture');

  glBindTexture(GL_TEXTURE_2D, result);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

  //GLfloat[4] bclr = 0.0;
  //glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, bclr.ptr);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Width, Height, 0, GL_RGBA{gltt}, GL_UNSIGNED_BYTE, tex);
  glFlush();

  //FreeMem(tex);
end;


var
  font6texid: GLuint = 0;
  font8texid: GLuint = 0;
  prfont6texid: GLuint = 0;
  prfont8texid: GLuint = 0;


procedure deleteFonts ();
begin
  if (font6texid <> 0) then glDeleteTextures(1, @font6texid);
  if (font8texid <> 0) then glDeleteTextures(1, @font8texid);
  if (prfont6texid <> 0) then glDeleteTextures(1, @prfont6texid);
  if (prfont8texid <> 0) then glDeleteTextures(1, @prfont8texid);
  font6texid := 0;
  font8texid := 0;
  prfont6texid := 0;
  prfont8texid := 0;
end;


procedure createFonts ();
begin
  if (font6texid = 0) then font6texid := createFontTexture(kgiFont6, kgiFont6PropWidth, false);
  if (font8texid = 0) then font8texid := createFontTexture(kgiFont8, kgiFont8PropWidth, false);
  if (prfont6texid = 0) then prfont6texid := createFontTexture(kgiFont6, kgiFont6PropWidth, true);
  if (prfont8texid = 0) then prfont8texid := createFontTexture(kgiFont8, kgiFont8PropWidth, true);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TScissorSave.save (enableScissoring: Boolean);
begin
  wassc := (glIsEnabled(GL_SCISSOR_TEST) <> 0);
  if wassc then glGetIntegerv(GL_SCISSOR_BOX, @scxywh[0]) else glGetIntegerv(GL_VIEWPORT, @scxywh[0]);
  //conwritefln('(%d,%d)-(%d,%d)', [scxywh[0], scxywh[1], scxywh[2], scxywh[3]]);
  if enableScissoring and (not wassc) then glEnable(GL_SCISSOR_TEST);
end;

procedure TScissorSave.restore ();
begin
  glScissor(scxywh[0], scxywh[1], scxywh[2], scxywh[3]);
  if wassc then glEnable(GL_SCISSOR_TEST) else glDisable(GL_SCISSOR_TEST);
end;

procedure TScissorSave.combineRect (x, y, w, h: Integer);
//var ox, oy, ow, oh: Integer;
begin
  if (w < 1) or (h < 1) then begin glScissor(0, 0, 0, 0); exit; end;
  y := gScrHeight-(y+h);
  //ox := x; oy := y; ow := w; oh := h;
  if not intersectRect(x, y, w, h, scxywh[0], scxywh[1], scxywh[2], scxywh[3]) then
  begin
    //writeln('oops: COMBINE: old=(', ox, ',', oy, ')-(', ox+ow-1, ',', oy+oh-1, '); sci: (', scxywh[0], ',', scxywh[1], ')-(', scxywh[0]+scxywh[2]-1, ',', scxywh[1]+scxywh[3]-1, ')');
    //writeln('oops: COMBINE: oldx=<', ox, '-', ox+ow-1, '>; oldy=<', oy, ',', oy+oh-1, '> : scix=<', scxywh[0], '-', scxywh[0]+scxywh[2]-1, '>; sciy=<', scxywh[1], '-', scxywh[1]+scxywh[3]-1, '>');
    glScissor(0, 0, 0, 0);
  end
  else
  begin
    glScissor(x, y, w, h);
  end;
end;

//TODO: overflow checks
function intersectRect (var x0, y0, w0, h0: Integer; const x1, y1, w1, h1: Integer): Boolean;
var
  ex0, ey0: Integer;
  ex1, ey1: Integer;
begin
  result := false;
  if (w0 < 1) or (h0 < 1) or (w1 < 1) or (h1 < 1) then exit; // at least one rect is null
  // check for intersection
  ex0 := x0+w0;
  ey0 := y0+h0;
  ex1 := x1+w1;
  ey1 := y1+h1;
  if (ex0 <= x1) or (ey0 <= y1) or (ex1 <= x0) or (ey1 <= y0) then exit;
  if (x0 >= ex1) or (y0 >= ey1) or (x1 >= ex0) or (y1 >= ey0) then exit;
  // ok, intersects
  if (x0 < x1) then x0 := x1;
  if (y0 < y1) then y0 := y1;
  if (ex0 > ex1) then ex0 := ex1;
  if (ey0 > ey1) then ey0 := ey1;
  w0 := ex0-x0;
  h0 := ey0-y0;
  result := (w0 > 0) and (h0 > 0);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure normRGBA (var r, g, b, a: Integer); inline;
begin
  if (a < 0) then a := 0 else if (a > 255) then a := 255;
  if (r < 0) then r := 0 else if (r > 255) then r := 255;
  if (g < 0) then g := 0 else if (g > 255) then g := 255;
  if (b < 0) then b := 0 else if (b > 255) then b := 255;
end;

// returns `false` if the color is transparent
function setupGLColor (r, g, b, a: Integer): Boolean;
begin
  normRGBA(r, g, b, a);
  if (a < 255) then
  begin
    if (a = 0) then begin result := false; exit; end;
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end
  else
  begin
    glDisable(GL_BLEND);
  end;
  glColor4ub(Byte(r), Byte(g), Byte(b), Byte(a));
  result := true;
end;

// returns `false` if the color is transparent
function setupGLColor (constref clr: TGxRGBA): Boolean;
begin
  if (clr.a < 255) then
  begin
    if (clr.a = 0) then begin result := false; exit; end;
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end
  else
  begin
    glDisable(GL_BLEND);
  end;
  glColor4ub(clr.r, clr.g, clr.b, clr.a);
  result := true;
end;

function isScaled (): Boolean;
var
  mt: packed array [0..15] of Double;
begin
  glGetDoublev(GL_MODELVIEW_MATRIX, @mt[0]);
  result := (mt[0] <> 1.0) or (mt[1*4+1] <> 1.0);
end;


// ////////////////////////////////////////////////////////////////////////// //
function textWidth6 (const s: AnsiString): Integer;
var
  f: Integer;
begin
  result := 0;
  for f := 1 to Length(s) do Inc(result, Integer(kgiFont6PropWidth[Integer(s[f])] and $0f)+1);
  if (result > 0) then Dec(result); // don't count last empty pixel
end;


function textWidth8 (const s: AnsiString): Integer;
var
  f: Integer;
begin
  result := 0;
  for f := 1 to Length(s) do Inc(result, Integer(kgiFont8PropWidth[Integer(s[f])] and $0f)+1);
  if (result > 0) then Dec(result); // don't count last empty pixel
end;


// return width (including last empty pixel)
function drawTextInternal (wdt, x, y: Integer; const s: AnsiString; constref clr: TGxRGBA; tid: GLuint; constref fontwdt: array of Byte; prop: Boolean): Integer;
var
  f, c: Integer;
  tx, ty: Integer;
begin
  result := 0;
  if (Length(s) = 0) then exit;
  if not setupGLColor(clr) then exit;

  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_NOTEQUAL, 0.0);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, tid);

  for f := 1 to Length(s) do
  begin
    c := Integer(s[f]) and $ff;
    tx := (c mod 16)*8;
    ty := (c div 16)*8;
    glBegin(GL_QUADS);
      glTexCoord2f((tx+0)/128.0, (ty+0)/128.0); glVertex2i(x+0, y+0); // top-left
      glTexCoord2f((tx+8)/128.0, (ty+0)/128.0); glVertex2i(x+8, y+0); // top-right
      glTexCoord2f((tx+8)/128.0, (ty+8)/128.0); glVertex2i(x+8, y+8); // bottom-right
      glTexCoord2f((tx+0)/128.0, (ty+8)/128.0); glVertex2i(x+0, y+8); // bottom-left
    glEnd();
    if prop then
    begin
      x += Integer(fontwdt[c] and $0f)+1;
      result += Integer(fontwdt[c] and $0f)+1;
    end
    else
    begin
      x += wdt;
      result += wdt;
    end;
  end;

  glDisable(GL_ALPHA_TEST);
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, 0);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure drawHLine (x, y, len: Integer; constref clr: TGxRGBA);
begin
  if (len < 1) then exit;
  if not setupGLColor(clr) then exit;
  glDisable(GL_TEXTURE_2D);
  if (not isScaled) then
  begin
    glBegin(GL_LINES);
      glVertex2f(x+0.375, y+0.375);
      glVertex2f(x+len+0.375, y+0.375);
    glEnd();
  end
  else
  begin
    glBegin(GL_QUADS);
      glVertex2i(x, y);
      glVertex2i(x+len, y);
      glVertex2i(x+len, y+1);
      glVertex2i(x, y+1);
    glEnd();
  end;
end;


procedure drawVLine (x, y, len: Integer; constref clr: TGxRGBA);
begin
  if (len < 1) then exit;
  if not setupGLColor(clr) then exit;
  glDisable(GL_TEXTURE_2D);
  if (not isScaled) then
  begin
    glBegin(GL_LINES);
      glVertex2f(x+0.375, y+0.375);
      glVertex2f(x+0.375, y+len+0.375);
    glEnd();
  end
  else
  begin
    glBegin(GL_QUADS);
      glVertex2i(x, y);
      glVertex2i(x, y+len);
      glVertex2i(x+1, y+len);
      glVertex2i(x+1, y);
    glEnd();
  end;
end;


procedure drawLine (x1, y1, x2, y2: Integer; constref clr: TGxRGBA);
begin
  if not setupGLColor(clr) then exit;

  glDisable(GL_TEXTURE_2D);

  glLineWidth(1);
  glPointSize(1);

  if (not isScaled) then
  begin
    glBegin(GL_LINES);
      glVertex2f(x1+0.375, y1+0.375);
      glVertex2f(x2+0.375, y2+0.375);
    glEnd();

    if (x1 <> x2) or (y1 <> y2) then
    begin
      glBegin(GL_POINTS);
        glVertex2f(x2+0.375, y2+0.375);
      glEnd();
    end;
  end
  else
  begin
    glBegin(GL_LINES);
      glVertex2i(x1, y1);
      glVertex2i(x2, y2);
      // draw last point
      glVertex2i(x2, y2);
      glVertex2i(x2+1, y2+1);
    glEnd();
  end;

  glColor4f(1, 1, 1, 1);
  glDisable(GL_BLEND);
end;


procedure drawRect (x, y, w, h: Integer; constref clr: TGxRGBA);
begin
  if (w < 0) or (h < 0) then exit;
  if not setupGLColor(clr) then exit;
  glDisable(GL_TEXTURE_2D);
  glLineWidth(1);
  glDisable(GL_LINE_SMOOTH);
  glDisable(GL_POLYGON_SMOOTH);
  if (w = 1) and (h = 1) then
  begin
    glBegin(GL_POINTS);
      glVertex2f(x+0.375, y+0.375);
    glEnd();
  end
  else
  begin
    glBegin(GL_LINES);
      glVertex2i(x, y); glVertex2i(x+w, y); // top
      glVertex2i(x, y+h-1); glVertex2i(x+w, y+h-1); // bottom
      glVertex2f(x+0.375, y+1); glVertex2f(x+0.375, y+h-1); // left
      glVertex2f(x+w-1+0.375, y+1); glVertex2f(x+w-1+0.375, y+h-1); // right
    glEnd();
  end;
  //glRect(x, y, x+w, y+h);
  glColor4f(1, 1, 1, 1);
  glDisable(GL_BLEND);
end;


procedure drawRectUI (x, y, w, h: Integer; constref clr: TGxRGBA);
  procedure hline (x, y, len: Integer);
  begin
    if (len < 1) then exit;
    glBegin(GL_QUADS);
      glVertex2i(x, y);
      glVertex2i(x+len, y);
      glVertex2i(x+len, y+1);
      glVertex2i(x, y+1);
    glEnd();
  end;

  procedure vline (x, y, len: Integer);
  begin
    if (len < 1) then exit;
    glBegin(GL_QUADS);
      glVertex2i(x, y);
      glVertex2i(x, y+len);
      glVertex2i(x+1, y+len);
      glVertex2i(x+1, y);
    glEnd();
  end;

var
  scaled: Boolean;
begin
  if (w < 0) or (h < 0) then exit;
  if not setupGLColor(clr) then exit;
  glDisable(GL_TEXTURE_2D);
  glLineWidth(1);
  glDisable(GL_LINE_SMOOTH);
  glDisable(GL_POLYGON_SMOOTH);
  scaled := isScaled();
  if (w = 1) and (h = 1) then
  begin
    glBegin(GL_POINTS);
      if scaled then glVertex2i(x, y) else glVertex2f(x+0.375, y+0.375);
    glEnd();
  end
  else
  begin
    if not scaled then
    begin
      glBegin(GL_LINES);
        glVertex2i(x, y); glVertex2i(x+w, y); // top
        glVertex2i(x, y+h-1); glVertex2i(x+w, y+h-1); // bottom
        glVertex2f(x+0.375, y+1); glVertex2f(x+0.375, y+h-1); // left
        glVertex2f(x+w-1+0.375, y+1); glVertex2f(x+w-1+0.375, y+h-1); // right
      glEnd();
    end
    else
    begin
      hline(x, y, w);
      hline(x, y+h-1, w);
      vline(x, y+1, h-2);
      vline(x+w-1, y+1, h-2);
    end;
  end;
  //glRect(x, y, x+w, y+h);
  glColor4f(1, 1, 1, 1);
  glDisable(GL_BLEND);
end;


procedure darkenRect (x, y, w, h: Integer; a: Integer);
begin
  if (w < 0) or (h < 0) then exit;
  if (a < 0) then a := 0;
  if (a >= 255) then exit;
  glEnable(GL_BLEND);
  glBlendFunc(GL_ZERO, GL_SRC_ALPHA);
  glDisable(GL_LINE_SMOOTH);
  glDisable(GL_POLYGON_SMOOTH);
  glDisable(GL_TEXTURE_2D);
  glColor4f(0.0, 0.0, 0.0, a/255.0);
  glBegin(GL_QUADS);
    glVertex2i(x, y);
    glVertex2i(x+w, y);
    glVertex2i(x+w, y+h);
    glVertex2i(x, y+h);
  glEnd();
  //glRect(x, y, x+w, y+h);
  glColor4f(1, 1, 1, 1);
  glDisable(GL_BLEND);
  //glBlendEquation(GL_FUNC_ADD);
end;


procedure fillRect (x, y, w, h: Integer; constref clr: TGxRGBA);
begin
  if (w < 0) or (h < 0) then exit;
  if not setupGLColor(clr) then exit;
  glDisable(GL_LINE_SMOOTH);
  glDisable(GL_POLYGON_SMOOTH);
  glDisable(GL_TEXTURE_2D);
  glBegin(GL_QUADS);
    glVertex2f(x, y);
    glVertex2f(x+w, y);
    glVertex2f(x+w, y+h);
    glVertex2f(x, y+h);
  glEnd();
  glColor4f(1, 1, 1, 1);
  glDisable(GL_BLEND);
end;


// ////////////////////////////////////////////////////////////////////////// //
function drawText6 (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
begin
  if (font6texid = 0) then createFonts();
  drawTextInternal(6, x, y, s, clr, font6texid, kgiFont6PropWidth, false);
  result := Length(s)*6;
end;

function drawText8 (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
begin
  if (font8texid = 0) then createFonts();
  drawTextInternal(8, x, y, s, clr, font8texid, kgiFont8PropWidth, false);
  result := Length(s)*8;
end;

function drawText6Prop (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
begin
  if (prfont6texid = 0) then createFonts();
  result := drawTextInternal(6, x, y, s, clr, prfont6texid, kgiFont6PropWidth, true);
end;

function drawText8Prop (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
begin
  if (prfont8texid = 0) then createFonts();
  result := drawTextInternal(8, x, y, s, clr, prfont8texid, kgiFont8PropWidth, true);
end;


// ////////////////////////////////////////////////////////////////////////// //
// x-centered at `x`
function drawText6XC (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
begin
  if (font6texid = 0) then createFonts();
  x -= Length(s)*6 div 2;
  drawTextInternal(6, x, y, s, clr, font6texid, kgiFont6PropWidth, false);
  result := Length(s)*6;
end;

function drawText8XC (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
begin
  if (font8texid = 0) then createFonts();
  x -= Length(s)*8 div 2;
  drawTextInternal(8, x, y, s, clr, font8texid, kgiFont8PropWidth, false);
  result := Length(s)*8;
end;

function drawText6PropXC (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
begin
  if (prfont6texid = 0) then createFonts();
  x -= textWidth6(s) div 2;
  result := drawTextInternal(6, x, y, s, clr, prfont6texid, kgiFont6PropWidth, true);
end;

function drawText8PropXC (x, y: Integer; const s: AnsiString; constref clr: TGxRGBA): Integer;
begin
  if (prfont8texid = 0) then createFonts();
  x -= textWidth8(s) div 2;
  result := drawTextInternal(8, x, y, s, clr, prfont8texid, kgiFont8PropWidth, true);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure oglRestoreMode (doClear: Boolean);
begin
  oglSetup2D(gScrWidth, gScrHeight);
  glScissor(0, 0, gScrWidth, gScrHeight);

  glBindTexture(GL_TEXTURE_2D, 0);
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_STENCIL_TEST);
  glDisable(GL_SCISSOR_TEST);
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_CULL_FACE);
  glDisable(GL_LINE_SMOOTH);
  glDisable(GL_POINT_SMOOTH);
  glLineWidth(1);
  glPointSize(1);
  glColor4f(1, 1, 1, 1);

  if doClear then
  begin
    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_ACCUM_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
  end;

  // scale everything
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  //glScalef(4, 4, 1);
end;


procedure onWinFocus (); begin end;

procedure onWinBlur (); begin resetKMState(true); end;

procedure onPreRender (); begin oglRestoreMode(gGfxDoClear); end;

procedure onPostRender (); begin oglRestoreMode(false); oglDrawCursor(); end;

procedure onInit ();
begin
  oglSetup2D(gScrWidth, gScrHeight);

  createCursorTexture();
  createFonts();
end;

procedure onDeinit ();
begin
  resetKMState(false);
  if (curtexid <> 0) then glDeleteTextures(1, @curtexid);
  curtexid := 0;
  deleteFonts();
  curButState := 0;
  curModState := 0;
  curMsX := 0;
  curMsY := 0;
end;


// ////////////////////////////////////////////////////////////////////////// //
begin
  evSDLCB := onSDLEvent;
  winFocusCB := onWinFocus;
  winBlurCB := onWinBlur;
  prerenderFrameCB := onPreRender;
  postrenderFrameCB := onPostRender;
  oglInitCB := onInit;
  oglDeinitCB := onDeinit;
end.
