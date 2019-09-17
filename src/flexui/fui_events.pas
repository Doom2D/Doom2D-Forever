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
unit fui_events;

interface

uses
  SysUtils, Classes,
  SDL2;


// ////////////////////////////////////////////////////////////////////////// //
type
  TFUIEvent = packed record
  public
    // keyboard modifiers
    const
      ModCtrl = $0001;
      ModAlt = $0002;
      ModShift = $0004;
      ModHyper = $0008;

    // mouse buttons
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
      TType = (Key, Mouse, User);
      TKind = (Release, Press, Motion, SimpleChar, Other);
      // SimpleChar: keyboard event with `ch`, but without `scan` (it is zero)

    // event state
    type
      TState = (
        None, // or "mine"
        Sinking,
        Bubbling,
        Eaten,
        Cancelled
      );

  private
    mType: TType; // event type: keyboard, mouse, etc...
    mKind: TKind; // motion, press, release
    mState: TState;

    function getEaten (): Boolean; inline;
    function getCancelled (): Boolean; inline;
    function getNoState (): Boolean; inline;
    function getSinking (): Boolean; inline;
    function getBubbling (): Boolean; inline;

  public
    x, y: Integer; // current mouse position
    dx, dy: Integer; // for wheel this is wheel motion, otherwise this is relative mouse motion
    bstate: Word; // button state BEFORE event (i.e. press/release modifications aren't done yet)
    kstate: Word; // keyboard state (see TFUIKeyEvent);
    // mouse events
    but: Word; // current pressed/released button, or 0 for motion
    // keyboard events
    scan: Word; // SDL_SCANCODE_XXX or 0 for character event
    ch: AnsiChar; // converted to 1251; can be #0
    // user tags
    itag: Integer;
    ptag: Pointer;

  public
    // initial state is "None"
    constructor Create (atype: TType; akind: TKind);

    // event type checkers
    function mouse (): Boolean; inline;
    function key (): Boolean; inline;
    function user (): Boolean; inline;

    function press (): Boolean; inline;
    function release (): Boolean; inline;
    function motion (): Boolean; inline;
    function other (): Boolean; inline;
    function simpleChar (): Boolean; inline;

    function alive (): Boolean; inline; // not eaten and not cancelled
    procedure eat (); inline;
    procedure cancel (); inline;

    procedure setSinking (); inline;
    procedure setBubbling (); inline;
    procedure setMine (); inline;

    // compares `scan` with `c`
    function isHot (c: AnsiChar): Boolean;

  public
    property etype: TType read mType; // event type: keyboard, mouse, etc...
    property ekind: TKind read mKind; // motion, press, release
    property state: TState read mState;

    property eaten: Boolean read getEaten;
    property cancelled: Boolean read getCancelled;
    property nostate: Boolean read getNoState;
    property mine: Boolean read getNoState;
    property sinking: Boolean read getSinking;
    property bubbling: Boolean read getBubbling;
  end;


// ////////////////////////////////////////////////////////////////////////// //
// call this on window deactivation, for example
procedure fuiResetKMState (sendEvents: Boolean=true);


// ////////////////////////////////////////////////////////////////////////// //
// event handlers
var
  fuiEventCB: procedure (var ev: TFUIEvent) = nil;


// ////////////////////////////////////////////////////////////////////////// //
function fuiMouseX (): Integer; inline;
function fuiMouseY (): Integer; inline;
function fuiButState (): Word; inline;
function fuiModState (): Word; inline;

procedure fuiSetMouseX (v: Integer); inline;
procedure fuiSetMouseY (v: Integer); inline;
procedure fuiSetButState (v: Word); inline;
procedure fuiSetModState (v: Word); inline;


// ////////////////////////////////////////////////////////////////////////// //
// any mods = 255: nothing was defined
function parseModKeys (const s: AnsiString; out kmods: Byte; out mbuts: Byte): AnsiString;

operator = (constref ev: TFUIEvent; const s: AnsiString): Boolean;
operator = (const s: AnsiString; constref ev: TFUIEvent): Boolean;


implementation

var
  curButState: Word = 0;
  curModState: Word = 0;
  curMsX: Integer = 0;
  curMsY: Integer = 0;


// ////////////////////////////////////////////////////////////////////////// //
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
function fuiMouseX (): Integer; inline; begin result := curMsX; end;
function fuiMouseY (): Integer; inline; begin result := curMsY; end;
function fuiButState (): Word; inline; begin result := curButState; end;
function fuiModState (): Word; inline; begin result := curModState; end;

procedure fuiSetMouseX (v: Integer); inline; begin curMsX := v; end;
procedure fuiSetMouseY (v: Integer); inline; begin curMsY := v; end;
procedure fuiSetButState (v: Word); inline; begin curButState := v; end;
procedure fuiSetModState (v: Word); inline; begin curModState := v; end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TFUIEvent.Create (atype: TType; akind: TKind);
begin
  FillChar(self, sizeof(self), 0);
  mType := atype;
  mKind := akind;
  mState := TState.None;
end;

function TFUIEvent.mouse (): Boolean; inline; begin result := (mType = TType.Mouse); end;
function TFUIEvent.key (): Boolean; inline; begin result := (mType = TType.Key); end;
function TFUIEvent.user (): Boolean; inline; begin result := (mType = TType.User); end;

function TFUIEvent.press (): Boolean; inline; begin result := (mKind = TKind.Press); end;
function TFUIEvent.release (): Boolean; inline; begin result := (mKind = TKind.Release); end;
function TFUIEvent.motion (): Boolean; inline; begin result := (mKind = TKind.Motion); end;
function TFUIEvent.other (): Boolean; inline; begin result := (mKind = TKind.Other); end;
function TFUIEvent.simpleChar (): Boolean; inline; begin result := (mKind = TKind.SimpleChar); end;

function TFUIEvent.alive (): Boolean; inline; begin result := (mState <> TState.Cancelled) and (mState <> TState.Eaten); end;
procedure TFUIEvent.eat (); inline; begin if (alive) then mState := TState.Eaten; end;
procedure TFUIEvent.cancel (); inline; begin if (alive) then mState := TState.Cancelled; end;
procedure TFUIEvent.setSinking (); inline; begin if (alive) then mState := TState.Sinking; end;
procedure TFUIEvent.setBubbling (); inline; begin if (alive) then mState := TState.Bubbling; end;
procedure TFUIEvent.setMine (); inline; begin if (alive) then mState := TState.None; end;


function TFUIEvent.getEaten (): Boolean; inline; begin result := (mState = TState.Eaten); end;
function TFUIEvent.getCancelled (): Boolean; inline; begin result := (mState = TState.Cancelled); end;
function TFUIEvent.getNoState (): Boolean; inline; begin result := (mState = TState.None); end;
function TFUIEvent.getSinking (): Boolean; inline; begin result := (mState = TState.Sinking); end;
function TFUIEvent.getBubbling (): Boolean; inline; begin result := (mState = TState.Bubbling); end;


function TFUIEvent.isHot (c: AnsiChar): Boolean;
begin
  result := false;
  if (c = #0) then exit;
  if (not alive) or (not key) then exit;
  c := locase1251(c);
  if (simpleChar) then
  begin
    if (ch = #0) then exit;
    result := (locase1251(ch) = c);
  end
  else
  begin
    case scan of
      SDL_SCANCODE_A: result := (c = 'a') or (c = 'ô');
      SDL_SCANCODE_B: result := (c = 'b') or (c = 'è');
      SDL_SCANCODE_C: result := (c = 'c') or (c = 'ñ');
      SDL_SCANCODE_D: result := (c = 'd') or (c = 'â');
      SDL_SCANCODE_E: result := (c = 'e') or (c = 'ó');
      SDL_SCANCODE_F: result := (c = 'f') or (c = 'à');
      SDL_SCANCODE_G: result := (c = 'g') or (c = 'ï');
      SDL_SCANCODE_H: result := (c = 'h') or (c = 'ð');
      SDL_SCANCODE_I: result := (c = 'i') or (c = 'ø');
      SDL_SCANCODE_J: result := (c = 'j') or (c = 'î');
      SDL_SCANCODE_K: result := (c = 'k') or (c = 'ë');
      SDL_SCANCODE_L: result := (c = 'l') or (c = 'ä');
      SDL_SCANCODE_M: result := (c = 'm') or (c = 'ü');
      SDL_SCANCODE_N: result := (c = 'n') or (c = 'ò');
      SDL_SCANCODE_O: result := (c = 'o') or (c = 'ù');
      SDL_SCANCODE_P: result := (c = 'p') or (c = 'ç');
      SDL_SCANCODE_Q: result := (c = 'q') or (c = 'é');
      SDL_SCANCODE_R: result := (c = 'r') or (c = 'ê');
      SDL_SCANCODE_S: result := (c = 's') or (c = 'û');
      SDL_SCANCODE_T: result := (c = 't') or (c = 'å');
      SDL_SCANCODE_U: result := (c = 'u') or (c = 'ã');
      SDL_SCANCODE_V: result := (c = 'v') or (c = 'ì');
      SDL_SCANCODE_W: result := (c = 'w') or (c = 'ö');
      SDL_SCANCODE_X: result := (c = 'x') or (c = '÷');
      SDL_SCANCODE_Y: result := (c = 'y') or (c = 'í');
      SDL_SCANCODE_Z: result := (c = 'z') or (c = 'ÿ');

      SDL_SCANCODE_1: result := (c = '1') or (c = '!');
      SDL_SCANCODE_2: result := (c = '2') or (c = '@');
      SDL_SCANCODE_3: result := (c = '3') or (c = '#');
      SDL_SCANCODE_4: result := (c = '4') or (c = '$');
      SDL_SCANCODE_5: result := (c = '5') or (c = '%');
      SDL_SCANCODE_6: result := (c = '6') or (c = '^');
      SDL_SCANCODE_7: result := (c = '7') or (c = '&');
      SDL_SCANCODE_8: result := (c = '8') or (c = '*');
      SDL_SCANCODE_9: result := (c = '9') or (c = '(');
      SDL_SCANCODE_0: result := (c = '0') or (c = ')');

      SDL_SCANCODE_RETURN: result := (c = #13) or (c = #10);
      SDL_SCANCODE_ESCAPE: result := (c = #27);
      SDL_SCANCODE_BACKSPACE: result := (c = #8);
      SDL_SCANCODE_TAB: result := (c = #9);
      SDL_SCANCODE_SPACE: result := (c = ' ');

      SDL_SCANCODE_MINUS: result := (c = '-');
      SDL_SCANCODE_EQUALS: result := (c = '=');
      SDL_SCANCODE_LEFTBRACKET: result := (c = '[') or (c = '{') or (c = 'õ');
      SDL_SCANCODE_RIGHTBRACKET: result := (c = ']') or (c = '}') or (c = 'ú');
      SDL_SCANCODE_BACKSLASH, SDL_SCANCODE_NONUSHASH: result := (c = '\') or (c = '|');
      SDL_SCANCODE_SEMICOLON: result := (c = ';') or (c = ':') or (c = 'æ');
      SDL_SCANCODE_APOSTROPHE: result := (c = '''') or (c = '"') or (c = 'ý');
      SDL_SCANCODE_GRAVE: result := (c = '`') or (c = '~') or (c = '¸');
      SDL_SCANCODE_COMMA: result := (c = ',') or (c = '<') or (c = 'á');
      SDL_SCANCODE_PERIOD: result := (c = '.') or (c = '>') or (c = '.') or (c = 'þ');
      SDL_SCANCODE_SLASH: result := (c = '/') or (c = '?') or (c = 'þ'); // ju: not a bug!
    end;
  end;
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
  while (pos <= Length(s)) do
  begin
    if (Length(s)-pos >= 1) and (s[pos+1] = '-') then
    begin
      case s[pos] of
        'C', 'c': begin if (kmods = 255) then kmods := 0; kmods := kmods or TFUIEvent.ModCtrl; Inc(pos, 2); continue; end;
        'M', 'm': begin if (kmods = 255) then kmods := 0; kmods := kmods or TFUIEvent.ModAlt; Inc(pos, 2); continue; end;
        'S', 's': begin if (kmods = 255) then kmods := 0; kmods := kmods or TFUIEvent.ModShift; Inc(pos, 2); continue; end;
      end;
      break;
    end;
    if (Length(s)-pos >= 3) and (s[pos+3] = '-') and ((s[pos+1] = 'M') or (s[pos+1] = 'm')) and ((s[pos+2] = 'B') or (s[pos+2] = 'b')) then
    begin
      case s[pos] of
        'L', 'l': begin if (mbuts = 255) then mbuts := 0; mbuts := mbuts or TFUIEvent.Left; Inc(pos, 4); continue; end;
        'R', 'r': begin if (mbuts = 255) then mbuts := 0; mbuts := mbuts or TFUIEvent.Right; Inc(pos, 4); continue; end;
        'M', 'm': begin if (mbuts = 255) then mbuts := 0; mbuts := mbuts or TFUIEvent.Middle; Inc(pos, 4); continue; end;
      end;
      break;
    end;
    break;
  end;
  epos := Length(s)+1;
  while (epos > pos) and (s[epos-1] <= ' ') do Dec(epos);
  if (epos > pos) then result := Copy(s, pos, epos-pos) else result := '';
end;


operator = (const s: AnsiString; constref ev: TFUIEvent): Boolean;
begin
  result := (ev = s);
end;


operator = (constref ev: TFUIEvent; const s: AnsiString): Boolean;
var
  kmods: Byte = 255;
  mbuts: Byte = 255;
  kname: AnsiString;
  but: Integer = -1;
  modch: AnsiChar = ' ';
  kfound: Boolean;
  f: Integer;
begin
  result := false;
  if (Length(s) = 0) then exit;
  // oops; i still want to compare dead events
  //if (not ev.alive) then exit; // dead events aren't equal to anything
  if (ev.user) then exit; // user events aren't equal to anything
  if (ev.simpleChar) or (ev.other) then exit; // those events are uncomparable for now

       if (s[1] = '+') then begin if (not ev.press) then exit; modch := '+'; end
  else if (s[1] = '-') then begin if (not ev.release) then exit; modch := '-'; end
  else if (s[1] = '*') then begin if (not ev.motion) then exit; end
  else if (not ev.press) then exit;

  kname := parseModKeys(s, kmods, mbuts);
  //if (ev.mouse) then writeln('compare: ', ev.mKind, ':', ev.mType, '; kstate=', ev.kstate, '; bstate=', ev.bstate, '; s=<', s, '>; kname=<', kname, '>; kmods=', kmods, '; mbuts=', mbuts);
  if (Length(kname) = 0) then exit; // some error occured
  if (strEquCI(kname, 'Enter')) then kname := 'RETURN';

  if (mbuts = 255) then mbuts := 0;
  if (kmods = 255) then kmods := 0;
  if (ev.kstate <> kmods) then exit;

       if (strEquCI(kname, 'LMB')) then but := TFUIEvent.Left
  else if (strEquCI(kname, 'RMB')) then but := TFUIEvent.Right
  else if (strEquCI(kname, 'MMB')) then but := TFUIEvent.Middle
  else if (strEquCI(kname, 'WheelUp')) or strEquCI(kname, 'WUP') then but := TFUIEvent.WheelUp
  else if (strEquCI(kname, 'WheelDown')) or strEquCI(kname, 'WDN') or strEquCI(kname, 'WDOWN') then but := TFUIEvent.WheelDown
  else if (strEquCI(kname, 'None')) then but := 0
  else
  begin
    // try keyboard
    if (not ev.key) then exit;
    if (strEquCI(kname, 'Empty')) or (strEquCI(kname, 'NoKey')) then
    begin
      kfound := (ev.scan = 0);
    end
    else
    begin
      kfound := false;
      for f := 1 to SDL_NUM_SCANCODES-1 do
      begin
        if (strEquCI(kname, SDL_GetScancodeName(f))) then begin kfound := (ev.scan = f); break; end;
      end;
    end;
    if (not kfound) then exit;
  end;
  //writeln('  scan=', ev.scan, '; found=', kfound);

  if (but <> -1) and (not ev.mouse) then exit; // mouse kname, but not mouse event

  // fix mouse buttons
  if (ev.mouse) then
  begin
    if (modch = '-') then mbuts := mbuts or but else if (modch = '+') then mbuts := mbuts and (not but);
    result := (ev.bstate = mbuts) and (ev.but = but);
  end
  else
  begin
    result := (ev.bstate = mbuts);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure fuiResetKMState (sendEvents: Boolean=true);
var
  mask: Word;
  ev: TFUIEvent;
begin
  // generate mouse release events
  if (curButState <> 0) then
  begin
    if (sendEvents) then
    begin
      mask := 1;
      while (mask <> 0) do
      begin
        // checked each time, 'cause `evMouseCB` can be changed from the handler
        if ((curButState and mask) <> 0) and (assigned(fuiEventCB)) then
        begin
          ev := TFUIEvent.Create(TFUIEvent.TType.Mouse, TFUIEvent.TKind.Release);
          ev.x := curMsX;
          ev.y := curMsY;
          ev.but := mask;
          ev.bstate := curButState;
          ev.kstate := curModState;
          curButState := curButState and (not mask);
          fuiEventCB(ev);
        end;
        mask := mask shl 1;
      end;
    end;
    curButState := 0;
  end;

  // generate modifier release events
  if (curModState <> 0) then
  begin
    if (sendEvents) then
    begin
      mask := 1;
      while (mask <= 8) do
      begin
        // checked each time, 'cause `evMouseCB` can be changed from the handler
        if ((curModState and mask) <> 0) and (assigned(fuiEventCB)) then
        begin
          ev := TFUIEvent.Create(TFUIEvent.TType.Key, TFUIEvent.TKind.Release);
          case mask of
            TFUIEvent.ModCtrl: begin ev.scan := SDL_SCANCODE_LCTRL; {kev.sym := SDLK_LCTRL;}{arbitrary} end;
            TFUIEvent.ModAlt: begin ev.scan := SDL_SCANCODE_LALT; {kev.sym := SDLK_LALT;}{arbitrary} end;
            TFUIEvent.ModShift: begin ev.scan := SDL_SCANCODE_LSHIFT; {kev.sym := SDLK_LSHIFT;}{arbitrary} end;
            TFUIEvent.ModHyper: begin ev.scan := SDL_SCANCODE_LGUI; {kev.sym := SDLK_LGUI;}{arbitrary} end;
            else assert(false);
          end;
          ev.x := curMsX;
          ev.y := curMsY;
          //mev.bstate := 0{curMsButState}; // anyway
          ev.kstate := curModState;
          curModState := curModState and (not mask);
          fuiEventCB(ev);
        end;
        mask := mask shl 1;
      end;
    end;
    curModState := 0;
  end;
end;


end.
