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
{$INCLUDE ../shared/a_modes.inc}
unit fui_events;

interface

uses
  SysUtils, Classes,
  SDL2;


// ////////////////////////////////////////////////////////////////////////// //
type
  TFUIMouseEvent = record
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

  private
    mEaten: Boolean;
    mCancelled: Boolean;

  public
    kind: TKind; // motion, press, release
    x, y: Integer; // current mouse position
    dx, dy: Integer; // for wheel this is wheel motion, otherwise this is relative mouse motion
    but: Word; // current pressed/released button, or 0 for motion
    bstate: Word; // button state BEFORE event (i.e. press/release modifications aren't done yet)
    kstate: Word; // keyboard state (see TFUIKeyEvent);

  public
    procedure intrInit (); inline; // init hidden fields

    function press (): Boolean; inline;
    function release (): Boolean; inline;
    function motion (): Boolean; inline;
    function isAlive (): Boolean; inline;
    procedure eat (); inline;
    procedure cancel (); inline;

  public
    property eaten: Boolean read mEaten;
    property cancelled: Boolean read mCancelled;
    property alive: Boolean read isAlive; // not eaten and not cancelled
  end;

  TFUIKeyEvent = record
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

  private
    mEaten: Boolean;
    mCancelled: Boolean;

  public
    kind: TKind;
    scan: Word; // SDL_SCANCODE_XXX or 0 for character event
    //sym: LongWord; // SDLK_XXX
    ch: AnsiChar; // converted to 1251; can be #0
    x, y: Integer; // current mouse position
    bstate: Word; // button state
    kstate: Word; // keyboard state BEFORE event (i.e. press/release modifications aren't done yet)

  public
    procedure intrInit (); inline; // init hidden fields

    function press (): Boolean; inline;
    function release (): Boolean; inline;
    function isAlive (): Boolean; inline;
    procedure eat (); inline;
    procedure cancel (); inline;

    function isHot (c: AnsiChar): Boolean;

  public
    property eaten: Boolean read mEaten;
    property cancelled: Boolean read mCancelled;
    property alive: Boolean read isAlive; // not eaten and not cancelled
  end;


// ////////////////////////////////////////////////////////////////////////// //
// call this on window deactivation, for example
procedure fuiResetKMState (sendEvents: Boolean=true);


// ////////////////////////////////////////////////////////////////////////// //
// event handlers
var
  evMouseCB: procedure (var ev: TFUIMouseEvent) = nil;
  evKeyCB: procedure (var ev: TFUIKeyEvent) = nil;


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

operator = (constref ev: TFUIKeyEvent; const s: AnsiString): Boolean;
operator = (const s: AnsiString; constref ev: TFUIKeyEvent): Boolean;

operator = (constref ev: TFUIMouseEvent; const s: AnsiString): Boolean;
operator = (const s: AnsiString; constref ev: TFUIMouseEvent): Boolean;


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
function fuiMouseX (): Integer; inline; begin result := curMsX; end;
function fuiMouseY (): Integer; inline; begin result := curMsY; end;
function fuiButState (): Word; inline; begin result := curButState; end;
function fuiModState (): Word; inline; begin result := curModState; end;

procedure fuiSetMouseX (v: Integer); inline; begin curMsX := v; end;
procedure fuiSetMouseY (v: Integer); inline; begin curMsY := v; end;
procedure fuiSetButState (v: Word); inline; begin curButState := v; end;
procedure fuiSetModState (v: Word); inline; begin curModState := v; end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TFUIMouseEvent.intrInit (); inline; begin mEaten := false; mCancelled := false; end;
function TFUIMouseEvent.press (): Boolean; inline; begin result := (kind = TKind.Press); end;
function TFUIMouseEvent.release (): Boolean; inline; begin result := (kind = TKind.Release); end;
function TFUIMouseEvent.motion (): Boolean; inline; begin result := (kind = TKind.Motion); end;
function TFUIMouseEvent.isAlive (): Boolean; inline; begin result := (not mEaten) and (not mCancelled); end;
procedure TFUIMouseEvent.eat (); inline; begin mEaten := true; end;
procedure TFUIMouseEvent.cancel (); inline; begin mCancelled := true; end;

procedure TFUIKeyEvent.intrInit (); inline; begin mEaten := false; mCancelled := false; ch := #0; scan := 0; end;
function TFUIKeyEvent.press (): Boolean; inline; begin result := (kind = TKind.Press); end;
function TFUIKeyEvent.release (): Boolean; inline; begin result := (kind = TKind.Release); end;
function TFUIKeyEvent.isAlive (): Boolean; inline; begin result := (not mEaten) and (not mCancelled); end;
procedure TFUIKeyEvent.eat (); inline; begin mEaten := true; end;
procedure TFUIKeyEvent.cancel (); inline; begin mCancelled := true; end;

function TFUIKeyEvent.isHot (c: AnsiChar): Boolean;
begin
  if (c = #0) or (scan = 0) or (scan = $FFFF) then begin result := false; exit; end;
  case scan of
    SDL_SCANCODE_A: result := (c = 'A') or (c = 'a') or (c = 'Ô') or (c = 'ô');
    SDL_SCANCODE_B: result := (c = 'B') or (c = 'b') or (c = 'È') or (c = 'è');
    SDL_SCANCODE_C: result := (c = 'C') or (c = 'c') or (c = 'Ñ') or (c = 'ñ');
    SDL_SCANCODE_D: result := (c = 'D') or (c = 'd') or (c = 'Â') or (c = 'â');
    SDL_SCANCODE_E: result := (c = 'E') or (c = 'e') or (c = 'Ó') or (c = 'ó');
    SDL_SCANCODE_F: result := (c = 'F') or (c = 'f') or (c = 'À') or (c = 'à');
    SDL_SCANCODE_G: result := (c = 'G') or (c = 'g') or (c = 'Ï') or (c = 'ï');
    SDL_SCANCODE_H: result := (c = 'H') or (c = 'h') or (c = 'Ð') or (c = 'ð');
    SDL_SCANCODE_I: result := (c = 'I') or (c = 'i') or (c = 'Ø') or (c = 'ø');
    SDL_SCANCODE_J: result := (c = 'J') or (c = 'j') or (c = 'Î') or (c = 'î');
    SDL_SCANCODE_K: result := (c = 'K') or (c = 'k') or (c = 'Ë') or (c = 'ë');
    SDL_SCANCODE_L: result := (c = 'L') or (c = 'l') or (c = 'Ä') or (c = 'ä');
    SDL_SCANCODE_M: result := (c = 'M') or (c = 'm') or (c = 'Ü') or (c = 'ü');
    SDL_SCANCODE_N: result := (c = 'N') or (c = 'n') or (c = 'Ò') or (c = 'ò');
    SDL_SCANCODE_O: result := (c = 'O') or (c = 'o') or (c = 'Ù') or (c = 'ù');
    SDL_SCANCODE_P: result := (c = 'P') or (c = 'p') or (c = 'Ç') or (c = 'ç');
    SDL_SCANCODE_Q: result := (c = 'Q') or (c = 'q') or (c = 'É') or (c = 'é');
    SDL_SCANCODE_R: result := (c = 'R') or (c = 'r') or (c = 'Ê') or (c = 'ê');
    SDL_SCANCODE_S: result := (c = 'S') or (c = 's') or (c = 'Û') or (c = 'û');
    SDL_SCANCODE_T: result := (c = 'T') or (c = 't') or (c = 'Å') or (c = 'å');
    SDL_SCANCODE_U: result := (c = 'U') or (c = 'u') or (c = 'Ã') or (c = 'ã');
    SDL_SCANCODE_V: result := (c = 'V') or (c = 'v') or (c = 'Ì') or (c = 'ì');
    SDL_SCANCODE_W: result := (c = 'W') or (c = 'w') or (c = 'Ö') or (c = 'ö');
    SDL_SCANCODE_X: result := (c = 'X') or (c = 'x') or (c = '×') or (c = '÷');
    SDL_SCANCODE_Y: result := (c = 'Y') or (c = 'y') or (c = 'Í') or (c = 'í');
    SDL_SCANCODE_Z: result := (c = 'Z') or (c = 'z') or (c = 'ß') or (c = 'ÿ');

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
    SDL_SCANCODE_LEFTBRACKET: result := (c = '[') or (c = '{');
    SDL_SCANCODE_RIGHTBRACKET: result := (c = ']') or (c = '}');
    SDL_SCANCODE_BACKSLASH, SDL_SCANCODE_NONUSHASH: result := (c = '\') or (c = '|');
    SDL_SCANCODE_SEMICOLON: result := (c = ';') or (c = ':');
    SDL_SCANCODE_APOSTROPHE: result := (c = '''') or (c = '"');
    SDL_SCANCODE_GRAVE: result := (c = '`') or (c = '~');
    SDL_SCANCODE_COMMA: result := (c = ',') or (c = '<');
    SDL_SCANCODE_PERIOD: result := (c = '.') or (c = '>');
    SDL_SCANCODE_SLASH: result := (c = '/') or (c = '?');

    else result := false;
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
        'C', 'c': begin if (kmods = 255) then kmods := 0; kmods := kmods or TFUIKeyEvent.ModCtrl; Inc(pos, 2); continue; end;
        'M', 'm': begin if (kmods = 255) then kmods := 0; kmods := kmods or TFUIKeyEvent.ModAlt; Inc(pos, 2); continue; end;
        'S', 's': begin if (kmods = 255) then kmods := 0; kmods := kmods or TFUIKeyEvent.ModShift; Inc(pos, 2); continue; end;
      end;
      break;
    end;
    if (Length(s)-pos >= 3) and (s[pos+3] = '-') and ((s[pos+1] = 'M') or (s[pos+1] = 'm')) and ((s[pos+2] = 'B') or (s[pos+2] = 'b')) then
    begin
      case s[pos] of
        'L', 'l': begin if (mbuts = 255) then mbuts := 0; mbuts := mbuts or TFUIMouseEvent.Left; Inc(pos, 4); continue; end;
        'R', 'r': begin if (mbuts = 255) then mbuts := 0; mbuts := mbuts or TFUIMouseEvent.Right; Inc(pos, 4); continue; end;
        'M', 'm': begin if (mbuts = 255) then mbuts := 0; mbuts := mbuts or TFUIMouseEvent.Middle; Inc(pos, 4); continue; end;
      end;
      break;
    end;
    break;
  end;
  epos := Length(s)+1;
  while (epos > pos) and (s[epos-1] <= ' ') do Dec(epos);
  if (epos > pos) then result := Copy(s, pos, epos-pos) else result := '';
end;


operator = (constref ev: TFUIKeyEvent; const s: AnsiString): Boolean;
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


operator = (const s: AnsiString; constref ev: TFUIKeyEvent): Boolean;
begin
  result := (ev = s);
end;


operator = (constref ev: TFUIMouseEvent; const s: AnsiString): Boolean;
var
  kmods: Byte = 255;
  mbuts: Byte = 255;
  kname: AnsiString;
  but: Integer = -1;
  modch: AnsiChar = ' ';
begin
  result := false;

  if (Length(s) > 0) then
  begin
         if (s[1] = '+') then begin if (not ev.press) then exit; modch := '+'; end
    else if (s[1] = '-') then begin if (not ev.release) then exit; modch := '-'; end
    else if (s[1] = '*') then begin if (not ev.motion) then exit; end
    else if (not ev.press) then exit;
  end;

  kname := parseModKeys(s, kmods, mbuts);
       if strEquCI(kname, 'LMB') then but := TFUIMouseEvent.Left
  else if strEquCI(kname, 'RMB') then but := TFUIMouseEvent.Right
  else if strEquCI(kname, 'MMB') then but := TFUIMouseEvent.Middle
  else if strEquCI(kname, 'WheelUp') or strEquCI(kname, 'WUP') then but := TFUIMouseEvent.WheelUp
  else if strEquCI(kname, 'WheelDown') or strEquCI(kname, 'WDN') or strEquCI(kname, 'WDOWN') then but := TFUIMouseEvent.WheelDown
  else if strEquCI(kname, 'None') then but := 0
  else exit;

  if (mbuts = 255) then mbuts := 0;
  if (kmods = 255) then kmods := 0;
  if (ev.kstate <> kmods) then exit;
  if (modch = '-') then mbuts := mbuts or but else if (modch = '+') then mbuts := mbuts and (not but);

  result := (ev.bstate = mbuts) and (ev.but = but);
end;


operator = (const s: AnsiString; constref ev: TFUIMouseEvent): Boolean;
begin
  result := (ev = s);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure fuiResetKMState (sendEvents: Boolean=true);
var
  mask: Word;
  mev: TFUIMouseEvent;
  kev: TFUIKeyEvent;
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
          FillChar(mev, sizeof(mev), 0);
          mev.intrInit();
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
          FillChar(kev, sizeof(kev), 0);
          kev.intrInit();
          kev.kind := kev.TKind.Release;
          case mask of
            TFUIKeyEvent.ModCtrl: begin kev.scan := SDL_SCANCODE_LCTRL; {kev.sym := SDLK_LCTRL;}{arbitrary} end;
            TFUIKeyEvent.ModAlt: begin kev.scan := SDL_SCANCODE_LALT; {kev.sym := SDLK_LALT;}{arbitrary} end;
            TFUIKeyEvent.ModShift: begin kev.scan := SDL_SCANCODE_LSHIFT; {kev.sym := SDLK_LSHIFT;}{arbitrary} end;
            TFUIKeyEvent.ModHyper: begin kev.scan := SDL_SCANCODE_LGUI; {kev.sym := SDLK_LGUI;}{arbitrary} end;
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


end.
