(* Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
 * This file is part of Doom2D Forever.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation, version 3 of
 * the License ONLY.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 *)

implementation

uses
  SysUtils, Math,
{$IF DEFINED(DARWIN) OR DEFINED(MACOS)}
  Keyboards, Events,
  {$IFDEF CPU64}
    {$linkframework Carbon}
  {$ENDIF}
{$ENDIF}
  SDL, {$INCLUDE ../../nogl/noGLuses.inc}
  e_log, e_graphics, e_input, {$IFDEF ENABLE_SOUND}e_sound,{$ENDIF}
  g_main, g_basic, g_options, g_window, g_console, g_game, g_menu, g_gui;

const
  GameTitle = 'Doom2D Forever (SDL 1.2, %s, %s)';

var
  // window resize is broken both on linux and osx, so disabled by default
  userResize: Boolean;
  modeResize: Integer;
  useScancodes: Boolean = True;

  screen: PSDL_Surface;
  JoystickHandle: array [0..e_MaxJoys - 1] of PSDL_Joystick;
  JoystickHatState: array [0..e_MaxJoys - 1, 0..e_MaxJoyHats - 1, HAT_LEFT..HAT_DOWN] of Boolean;
  JoystickZeroAxes: array [0..e_MaxJoys - 1, 0..e_MaxJoyAxes - 1] of Integer;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

{$IF DEFINED(DARWIN) OR DEFINED(MACOS)}
var
  IsMacPlatform: Boolean;

function IsMacModSym (sym: Integer): Boolean;
begin
  case sym of
    SDLK_NUMLOCK, SDLK_CAPSLOCK,
    SDLK_RCTRL, SDLK_LCTRL,
    SDLK_RSHIFT, SDLK_LSHIFT,
    SDLK_RALT, SDLK_LALT,
    SDLK_RSUPER, SDLK_LSUPER,
    SDLK_RMETA, SDLK_LMETA:
      Result := True;
    otherwise
      Result := False;
  end;
end;

function Mac2Stub (scancode: Integer): Integer;
begin
  // SDL2 swap tilda/grave and paragraph/plus-minus buttons on ISO keyboards
  if ((scancode = 10) or (scancode = 50)) and (KBGetLayoutType(LMGetKbdType()) = kKeyboardISO) then
    scancode := 60 - scancode;

  case scancode of
    0: Result := IK_A;
    1: Result := IK_S;
    2: Result := IK_D;
    3: Result := IK_F;
    4: Result := IK_H;
    5: Result := IK_G;
    6: Result := IK_Z;
    7: Result := IK_X;
    8: Result := IK_C;
    9: Result := IK_V;
    10: Result := IK_NONUSBACKSLASH;
    11: Result := IK_B;
    12: Result := IK_Q;
    13: Result := IK_W;
    14: Result := IK_E;
    15: Result := IK_R;
    16: Result := IK_Y;
    17: Result := IK_T;
    18: Result := IK_1;
    19: Result := IK_2;
    20: Result := IK_3;
    21: Result := IK_4;
    22: Result := IK_6;
    23: Result := IK_5;
    24: Result := IK_EQUALS;
    25: Result := IK_9;
    26: Result := IK_7;
    27: Result := IK_MINUS;
    28: Result := IK_8;
    29: Result := IK_0;
    30: Result := {IK_RIGHTBRACKET} IK_RBRACKET;
    31: Result := IK_O;
    32: Result := IK_U;
    33: Result := {IK_LEFTBRACKET} IK_LBRACKET;
    34: Result := IK_I;
    35: Result := IK_P;
    36: Result := IK_RETURN;
    37: Result := IK_L;
    38: Result := IK_J;
    39: Result := {IK_APOSTROPHE} IK_QUOTE;
    40: Result := IK_K;
    41: Result := IK_SEMICOLON;
    42: Result := IK_BACKSLASH;
    43: Result := IK_COMMA;
    44: Result := IK_SLASH;
    45: Result := IK_N;
    46: Result := IK_M;
    47: Result := {IK_PERIOD} IK_DOT;
    48: Result := IK_TAB;
    49: Result := IK_SPACE;
    50: Result := IK_GRAVE;
    51: Result := IK_BACKSPACE;
    52: Result := {IK_KP_ENTER} IK_KPRETURN;
    53: Result := IK_ESCAPE;
    54: Result := {IK_RGUI} IK_RWIN;
    55: Result := {IK_LGUI} IK_WIN;
    56: Result := {IK_LSHIFT} IK_SHIFT;
    57: Result := IK_CAPSLOCK;
    58: Result := {IK_LALT} IK_ALT;
    59: Result := {IK_LCTRL} IK_CTRL;
    60: Result := IK_RSHIFT;
    61: Result := IK_RALT;
    62: Result := IK_RCTRL;
    63: Result := {IK_RGUI} IK_RWIN;
    //64: Result := IK_F17;
    65: Result := {IK_KP_PERIOD} IK_KPDOT;
    66: Result := IK_INVALID;  // unused?
    67: Result := {IK_KP_MULTIPLY} IK_KPMULTIPLE;
    68: Result := IK_INVALID;  // unused?
    69: Result := {IK_KP_PLUS} IK_KPPLUS;
    70: Result := IK_INVALID;  // unused?
    71: Result := {IK_NUMLOCKCLEAR} IK_NUMLOCK;
    //72: Result := IK_VOLUMEUP;
    //73: Result := IK_VOLUMEDOWN;
    //74: Result := IK_MUTE;
    75: Result := {IK_KP_DIVIDE} IK_KPDIVIDE;
    76: Result := {IK_KP_ENTER} IK_KPRETURN;
    77: Result := IK_INVALID;  // unused?
    78: Result := {IK_KP_MINUS} IK_KPMINUS;
    //79: Result := IK_F18;
    //80: Result := IK_F19;
    //81: Result := IK_KP_EQUALS;
    82: Result := {IK_KP_0} IK_KPINSERT;
    83: Result := {IK_KP_1} IK_KPEND;
    84: Result := {IK_KP_2} IK_KPDOWN;
    85: Result := {IK_KP_3} IK_KPPAGEDN;
    86: Result := {IK_KP_4} IK_KPLEFT;
    87: Result := {IK_KP_5} IK_KP5;
    88: Result := {IK_KP_6} IK_KPRIGHT;
    89: Result := {IK_KP_7} IK_KPHOME;
    90: Result := IK_INVALID;  // unused?
    91: Result := {IK_KP_8} IK_KPUP;
    92: Result := {IK_KP_9} IK_KPPAGEUP;
    //93: Result := IK_INTERNATIONAL3;
    //94: Result := IK_INTERNATIONAL1;
    //95: Result := IK_KP_COMMA;
    96: Result := IK_F5;
    97: Result := IK_F6;
    98: Result := IK_F7;
    99: Result := IK_F3;
    100: Result := IK_F8;
    101: Result := IK_F9;
    //102: Result := IK_LANG2;
    103: Result := IK_F11;
    //104: Result := IK_LANG1;
    105: Result := {IK_PRINTSCREEN} IK_PRINTSCR;
    //106: Result := IK_F16;
    107: Result := IK_SCROLLLOCK;
    108: Result := IK_INVALID;  // unused?
    109: Result := IK_F10;
    //110: Result := IK_APPLICATION;
    111: Result := IK_F12;
    112: Result := IK_INVALID;  // unused?
    113: Result := IK_PAUSE;
    114: Result := IK_INSERT;
    115: Result := IK_HOME;
    116: Result := IK_PAGEUP;
    117: Result := IK_DELETE;
    118: Result := IK_F4;
    119: Result := IK_END;
    120: Result := IK_F2;
    121: Result := {IK_PAGEDOWN} IK_PAGEDN;
    122: Result := IK_F1;
    123: Result := IK_LEFT;
    124: Result := IK_RIGHT;
    125: Result := IK_DOWN;
    126: Result := IK_UP;
    //127: Result := IK_POWER;
  otherwise
    Result := IK_INVALID;
  end;
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////////////////////////

function Key2Stub (key: Integer): Integer;
begin
  case key of
    SDLK_ESCAPE: Result := IK_ESCAPE;
    SDLK_RETURN: Result := IK_RETURN;
    SDLK_KP_ENTER: Result := IK_KPRETURN;
    SDLK_KP0: Result := IK_KPINSERT;
    SDLK_UP: Result := IK_UP;
    SDLK_KP8: Result := IK_KPUP;
    SDLK_DOWN: Result := IK_DOWN;
    SDLK_KP2: Result := IK_KPDOWN;
    SDLK_LEFT: Result := IK_LEFT;
    SDLK_KP4: Result := IK_KPLEFT;
    SDLK_RIGHT: Result := IK_RIGHT;
    SDLK_KP6: Result := IK_KPRIGHT;
    SDLK_DELETE: Result := IK_DELETE;
    SDLK_HOME: Result := IK_HOME;
    SDLK_KP7: Result := IK_KPHOME;
    SDLK_INSERT: Result := IK_INSERT;
    SDLK_SPACE: Result := IK_SPACE;
    SDLK_LSHIFT: Result := IK_SHIFT;
    SDLK_LALT: Result := IK_ALT;
    SDLK_TAB: Result := IK_TAB;
    SDLK_PAGEUP: Result := IK_PAGEUP;
    SDLK_KP9: Result := IK_KPPAGEUP;
    SDLK_PAGEDOWN: Result := IK_PAGEDN;
    SDLK_KP3: Result := IK_KPPAGEDN;
    SDLK_KP5: Result := IK_KP5;
    SDLK_NUMLOCK: Result := IK_NUMLOCK;
    SDLK_KP_DIVIDE: Result := IK_KPDIVIDE;
    SDLK_KP_MULTIPLY: Result := IK_KPMULTIPLE;
    SDLK_KP_MINUS: Result := IK_KPMINUS;
    SDLK_KP_PLUS: Result := IK_KPPLUS;
    SDLK_KP_PERIOD: Result := IK_KPDOT;
    SDLK_CAPSLOCK: Result := IK_CAPSLOCK;
    SDLK_RSHIFT: Result := IK_RSHIFT;
    SDLK_LCTRL: Result := IK_CTRL;
    SDLK_RCTRL: Result := IK_RCTRL;
    SDLK_RALT: Result := IK_RALT;
    SDLK_LSUPER: Result := IK_WIN;
    SDLK_RSUPER: Result := IK_RWIN;
    SDLK_MENU: Result := IK_MENU;
    SDLK_PRINT: Result := IK_PRINTSCR;
    SDLK_SCROLLOCK: Result := IK_SCROLLLOCK;
    SDLK_LEFTBRACKET: Result := IK_LBRACKET;
    SDLK_RIGHTBRACKET: Result := IK_RBRACKET;
    SDLK_SEMICOLON: Result := IK_SEMICOLON;
    SDLK_QUOTE: Result := IK_QUOTE;
    SDLK_BACKSLASH: Result := IK_BACKSLASH;
    SDLK_SLASH: Result := IK_SLASH;
    SDLK_COMMA: Result := IK_COMMA;
    SDLK_PERIOD: Result := IK_DOT;
    SDLK_EQUALS: Result := IK_EQUALS;
    SDLK_0: Result := IK_0;
    SDLK_1: Result := IK_1;
    SDLK_2: Result := IK_2;
    SDLK_3: Result := IK_3;
    SDLK_4: Result := IK_4;
    SDLK_5: Result := IK_5;
    SDLK_6: Result := IK_6;
    SDLK_7: Result := IK_7;
    SDLK_8: Result := IK_8;
    SDLK_9: Result := IK_9;
    SDLK_F1: Result := IK_F1;
    SDLK_F2: Result := IK_F2;
    SDLK_F3: Result := IK_F3;
    SDLK_F4: Result := IK_F4;
    SDLK_F5: Result := IK_F5;
    SDLK_F6: Result := IK_F6;
    SDLK_F7: Result := IK_F7;
    SDLK_F8: Result := IK_F8;
    SDLK_F9: Result := IK_F9;
    SDLK_F10: Result := IK_F10;
    SDLK_F11: Result := IK_F11;
    SDLK_F12: Result := IK_F12;
    SDLK_END: Result := IK_END;
    SDLK_KP1: Result := IK_KPEND;
    SDLK_BACKSPACE: Result := IK_BACKSPACE;
    SDLK_BACKQUOTE: Result := IK_BACKQUOTE;
    SDLK_PAUSE: Result := IK_PAUSE;
    SDLK_A..SDLK_Z: Result := IK_A + (key - SDLK_A);
    SDLK_MINUS: Result := IK_MINUS;
  else
    Result := IK_INVALID;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

function LoadGL (): Boolean;
var
  fbo: Boolean;
  ltmp: Integer;
begin
  Result := True;

{$IFNDEF NOGL_INIT}
  fbo := Load_GL_ARB_framebuffer_object();
{$ELSE}
  nogl_Init();
  fbo := nogl_ExtensionSupported('GL_OES_framebuffer_object');
{$ENDIF}

  if glRenderToFBO and not fbo then
  begin
    e_LogWriteln('GL: framebuffer objects not supported; disabling FBO rendering');
    glRenderToFBO := False;
  end;

  if SDL_GL_GetAttribute(SDL_GL_STENCIL_SIZE, ltmp) = 0 then
  begin
    e_LogWritefln('stencil buffer size: %s', [ltmp]);
    gwin_has_stencil := ltmp > 0;
  end;
end;

procedure FreeGL ();
begin
{$IFDEF NOGL_INIT}
  nogl_Quit();
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

function GetDriver (): AnsiString;
var
  buf: array [0..31] of AnsiChar = '';
begin
  SDL_VideoDriverName(buf, Length(buf));
  Result := AnsiString(buf);
end;

function GetTitle (): AnsiString;
var
  info: AnsiString;
begin
  info := g_GetBuildHash(False);
  if info = 'custom build' then
    info += ' by ' + g_GetBuilderName() + ' ' + GAME_BUILDDATE + ' ' + GAME_BUILDTIME;

  Result := Format(GameTitle, [GetDriver(), info]);
end;

procedure UpdateSize (w, h: Integer);
begin
  gWinSizeX := w;
  gWinSizeY := h;
  gRC_Width := w;
  gRC_Height := h;

  if glRenderToFBO then
  begin
    // store real window size in gWinSize, downscale resolution now
    w := Round(w / r_pixel_scale);
    h := Round(h / r_pixel_scale);
    if not e_ResizeFramebuffer(w, h) then
    begin
      e_LogWriteln('GL: could not create framebuffer, falling back to --no-fbo');
      glRenderToFBO := False;
      w := gWinSizeX;
      h := gWinSizeY;
    end;
  end;

  gScreenWidth := w;
  gScreenHeight := h;

{$IFDEF ENABLE_HOLMES}
  fuiScrWdt := w;
  fuiScrHgt := h;
{$ENDIF}

  e_ResizeWindow(w, h);
  e_InitGL();
  g_Game_SetupScreenSize();
  g_Menu_Reset();
  g_Game_ClearLoading();
end;

function InitWindow (w, h, bpp: Integer; fullScreen: Boolean): Boolean;
var
  flags: UInt32;
begin
  Result := False;
  e_LogWritefln('InitWindow %s %s %s %s', [w, h, bpp, fullScreen]);

  SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);

  // lights; it is enough to have 1-bit stencil buffer for lighting, but...
  SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);

  flags := SDL_OPENGL;
  if fullScreen then flags := flags or SDL_FULLSCREEN;
  if userResize then flags := flags or SDL_VIDEORESIZE;

  if (screen = nil) or (SDL_VideoModeOk(w, h, bpp, flags) <> 0) then
  begin
    SDL_FreeSurface(screen);
    screen := SDL_SetVideoMode(w, h, bpp, flags);
    if screen <> nil then
    begin
      if not LoadGL() then
      begin
        e_LogWriteln('GL: unable to load OpenGL functions', TMsgType.Fatal);
        Exit;
      end;

      Result := True;
      gFullScreen := fullscreen;
      gRC_FullScreen := fullscreen;

      SDL_WM_SetCaption(PChar(GetTitle()), nil);
      UpdateSize(w, h);
    end;
  end
  else
  begin
    e_LogWritefln('SDL: video mode not supported', []);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

procedure AddJoystick (which: Integer);
var
  i: Integer;
begin
  Assert(which < e_MaxJoys);

  JoystickHandle[which] := SDL_JoystickOpen(which);
  if JoystickHandle[which] = nil then
  begin
    e_LogWritefln('Failed to open Joystick %s', [which]);
    Exit;
  end;

  e_LogWritefln('Added Joystick %s', [which]);
  e_JoystickAvailable[which] := True;
  for i := 0 to Min(SDL_JoystickNumAxes(JoystickHandle[which]), e_MaxJoyAxes) - 1 do
    JoystickZeroAxes[which, i] := SDL_JoystickGetAxis(JoystickHandle[which], i);
end;

procedure RemoveJoystick (which: Integer);
begin
  Assert(which < e_MaxJoys);

  e_LogWritefln('Remove Joystick %s', [which]);
  e_JoystickAvailable[which] := False;

  if JoystickHandle[which] <> nil then
  begin
    SDL_JoystickClose(JoystickHandle[which]);
    JoystickHandle[which] := nil;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure HandleJoyButton (var ev: TSDL_JoyButtonEvent);
var
  down: Boolean;
  key: Integer;
begin
  if (ev.which < e_MaxJoys) and (ev.button < e_MaxJoyBtns) then
  begin
    key := e_JoyButtonToKey(ev.which, ev.button);
    down := ev.type_ = SDL_JOYBUTTONDOWN;
    if g_dbg_input then
      e_LogWritefln('Input Debug: jbutton, joy=%s, button=%s, keycode=%s, press=%s', [ev.which, ev.button, key, down]);
    e_KeyUpDown(key, down);
    g_Console_ProcessBind(key, down);
  end
  else
  begin
    if g_dbg_input then
    begin
      down := ev.type_ = SDL_JOYBUTTONDOWN;
      e_LogWritefln('Input Debug: NOT IN RANGE! jbutton, joy=%s, button=%s, press=%s', [ev.which, ev.button, down]);
    end;
  end;
end;

procedure HandleJoyAxis (var ev: TSDL_JoyAxisEvent);
var
  key, minuskey: Integer;
begin
  if (ev.which < e_MaxJoys) and (ev.axis < e_MaxJoyAxes) then
  begin
    key := e_JoyAxisToKey(ev.which, ev.axis, AX_PLUS);
    minuskey := e_JoyAxisToKey(ev.which, ev.axis, AX_MINUS);

    if g_dbg_input then
      e_LogWritefln('Input Debug: jaxis, joy=%s, axis=%s, value=%s, zeroaxes=%s, deadzone=%s',
        [ev.which, ev.axis, ev.value, JoystickZeroAxes[ev.which, ev.axis], e_JoystickDeadzones[ev.which]]);

    if ev.value < (JoystickZeroAxes[ev.which, ev.axis] - e_JoystickDeadzones[ev.which]) then
    begin
      if e_KeyPressed(key) then
      begin
        e_KeyUpDown(key, False);
        g_Console_ProcessBind(key, False);
      end;
      e_KeyUpDown(minuskey, True);
      g_Console_ProcessBind(minuskey, True);
    end
    else if ev.value > (JoystickZeroAxes[ev.which, ev.axis] + e_JoystickDeadzones[ev.which]) then
    begin
      if e_KeyPressed(minuskey) then
      begin
        e_KeyUpDown(minuskey, False);
        g_Console_ProcessBind(minuskey, False);
      end;
      e_KeyUpDown(key, True);
      g_Console_ProcessBind(key, True);
    end
    else
    begin
      if e_KeyPressed(minuskey) then
      begin
        e_KeyUpDown(minuskey, False);
        g_Console_ProcessBind(minuskey, False);
      end;
      if e_KeyPressed(key) then
      begin
        e_KeyUpDown(key, False);
        g_Console_ProcessBind(key, False);
      end;
    end;
  end
  else
  begin
    if g_dbg_input then
      e_LogWritefln('Input Debug: NOT IN RANGE! jaxis, joy=%s, axis=%s, value=%s, zeroaxes=%s, deadzone=%s',
        [ev.which, ev.axis, ev.value, JoystickZeroAxes[ev.which, ev.axis], e_JoystickDeadzones[ev.which]]);
  end;
end;

procedure HandleJoyHat (var ev: TSDL_JoyHatEvent);
var
  down: Boolean;
  i, key: Integer;
  hat: array [HAT_LEFT..HAT_DOWN] of Boolean;
begin
  if (ev.which < e_MaxJoys) and (ev.hat < e_MaxJoyHats) then
  begin
    if g_dbg_input then
      e_LogWritefln('Input Debug: jhat, joy=%s, hat=%s, value=%s', [ev.which, ev.hat, ev.value]);

    hat[HAT_UP] := LongBool(ev.value and SDL_HAT_UP);
    hat[HAT_DOWN] := LongBool(ev.value and SDL_HAT_DOWN);
    hat[HAT_LEFT] := LongBool(ev.value and SDL_HAT_LEFT);
    hat[HAT_RIGHT] := LongBool(ev.value and SDL_HAT_RIGHT);

    for i := HAT_LEFT to HAT_DOWN do
    begin
      if JoystickHatState[ev.which, ev.hat, i] <> hat[i] then
      begin
        down := hat[i];
        key := e_JoyHatToKey(ev.which, ev.hat, i);
        e_KeyUpDown(key, down);
        g_Console_ProcessBind(key, down);
      end;
    end;

    JoystickHatState[ev.which, ev.hat] := hat;
  end
  else
  begin
    if g_dbg_input then
      e_LogWritefln('Input Debug: NOT IN RANGE! jhat, joy=%s, hat=%s, value=%s', [ev.which, ev.hat, ev.value]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

procedure HandleKeyboard (var ev: TSDL_KeyboardEvent);
var
  down, repeated: Boolean;
  key: Integer;
  ch: Char;
begin
  key := IK_INVALID;

{$IF DEFINED(DARWIN) OR DEFINED(MACOS)}
  if useScancodes and IsMacPlatform then
    if not IsMacModSym(ev.keysym.sym) then
      key := Mac2Stub(ev.keysym.scancode);
{$ENDIF}

  if key = IK_INVALID then
    key := Key2Stub(ev.keysym.sym);

  down := (ev.type_ = SDL_KEYDOWN);
  repeated := down and e_KeyPressed(key);
  ch := wchar2win(WideChar(ev.keysym.unicode));

  if g_dbg_input then
    e_LogWritefln('Input Debug: keysym, scancode=%s, down=%s, sym=%s, state=%s, unicode=%s, stubsym=%s, cp1251=%s',
      [ev.keysym.scancode, down, ev.keysym.sym, ev.state, ev.keysym.unicode, key, Ord(ch)]);

  if not repeated then
  begin
    e_KeyUpDown(key, down);
    g_Console_ProcessBind(key, down);
  end
  else
  begin
    g_Console_ProcessBindRepeat(key);
  end;

  if down and IsValid1251(ev.keysym.unicode) and IsPrintable1251(ch) then
    CharPress(ch);
end;

procedure HandleResize (var ev: TSDL_ResizeEvent);
begin
  if g_dbg_input then
    e_LogWritefln('Input Debug: SDL_VIDEORESIZE %s %s', [ev.w, ev.h]);

  if modeResize = 1 then
    UpdateSize(ev.w, ev.h)
  else if modeResize > 1 then
    InitWindow(ev.w, ev.h, gBPP, gFullscreen);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

function sys_HandleEvents (): Boolean;
var
  ev: TSDL_Event;
begin
  Result := False;
  while SDL_PollEvent(@ev) <> 0 do
  begin
    case ev.type_ of
      SDL_QUITEV: Result := True;

      SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP: HandleJoyButton(ev.jbutton);
      SDL_JOYAXISMOTION: HandleJoyAxis(ev.jaxis);
      SDL_JOYHATMOTION: HandleJoyHat(ev.jhat);

      SDL_KEYUP, SDL_KEYDOWN: HandleKeyboard(ev.key);

      SDL_VIDEORESIZE: HandleResize(ev.resize);
      SDL_VIDEOEXPOSE: sys_Repaint();
    {$IFDEF ENABLE_SOUND}
      SDL_ACTIVEEVENT: e_MuteChannels((ev.active.gain = 0) and gMuteWhenInactive);
    {$ENDIF}
    end;
  end;
end;

procedure sys_RequestQuit ();
var
  ev: TSDL_Event;
begin
  ev.quit.type_ := SDL_QUITEV;
  SDL_PushEvent(@ev);
end;

procedure sys_YieldTimeSlice ();
begin
  // Unfortunately, SDL_WaitEventTimeout() was introduced only in SDL2, so we must enforce this.
  // ThreadSwitch() is not suitable here because on Windows it is implemented as Sleep(0) (see
  // systhrd.inc:SysThreadSwitch in RTL), which for some reason does not give the desired effect
  // of preventing 100% CPU load, according to my experiments.
  // The system Sleep() is used here instead of SDL_Delay() to ensure proper behavior.
  Sleep(1);
end;

procedure sys_Delay (ms: Integer);
begin
  SDL_Delay(ms);
end;

function sys_GetTicks (): Int64;
begin
  Result := SDL_GetTicks();
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

procedure sys_EnableVSync (yes: Boolean);
begin
(*
  Unimplemented, TODO. See this for details:
  https://github.com/libsdl-org/SDL-1.2/issues/727 - Wrong IOCTL used for VSYNC
  https://github.com/libsdl-org/SDL-1.2/issues/626 - VSync can not be reinitialized with another value
  https://github.com/libsdl-org/SDL-1.2/issues/569 - VSync is not activated under MacOS X Snow Leopard
  https://github.com/libsdl-org/SDL-1.2/issues/482 - SDL_GL_SetSwapInterval doesn't work with nvidia drivers
  https://github.com/libsdl-org/SDL/issues/4471 - SDL_GL_SetSwapInterval is partly broken on macOS
*)
end;

function sys_GetDisplayModes (bpp: Integer): SSArray;
var
  m: PPSDL_Rect;
  f: TSDL_PixelFormat;
  i, count: Integer;
begin
  Result := nil;

  FillChar(f, SizeOf(f), 0);
  f.palette := nil;
  f.BitsPerPixel := bpp;
  f.BytesPerPixel := (bpp + 7) div 8;

  m := SDL_ListModes(@f, SDL_OPENGL or SDL_FULLSCREEN);
  if (m <> nil) and (PtrUInt(m) <> PtrUInt(-1)) then
  begin
    count := 0;
    while m[count] <> nil do count += 1;
    SetLength(Result, count);
    for i := 0 to count - 1 do
      Result[i] := IntToStr(m[i].w) + 'x' + IntToStr(m[i].h);
  end;
end;

function sys_SetDisplayMode (w, h, bpp: Integer; fullscreen, maximized: Boolean): Boolean;
begin
  Result := InitWindow(w, h, bpp, fullscreen);
end;

procedure sys_Repaint ();
begin
  SDL_GL_SwapBuffers();
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

procedure sys_Init ();
var
  flags: UInt32;
  i: Integer;
  name: AnsiString;
begin
  e_WriteLog('Init SDL', TMsgType.Notify);

  flags := SDL_INIT_VIDEO or SDL_INIT_TIMER or SDL_INIT_JOYSTICK {or SDL_INIT_NOPARACHUTE}
    {$IFDEF USE_SDLMIXER} or SDL_INIT_AUDIO {$ENDIF};

  if SDL_Init(flags) <> 0 then
    raise Exception.Create('SDL: Init failed: ' + SDL_GetError());

  name := GetDriver();
  e_LogWritefln('SDL: Video Driver "%s"', [name]);

{$IF DEFINED(DARWIN) OR DEFINED(MACOS)}
  IsMacPlatform := (name = 'Quartz') or (name = 'toolbox') or (name = 'DSp');
{$ENDIF}

  SDL_EnableUNICODE(1);
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);

  for i := 0 to e_MaxJoys - 1 do
    AddJoystick(i);
end;

procedure sys_Final ();
var
  i: Integer;
begin
  e_WriteLog('Releasing SDL', TMsgType.Notify);

  for i := 0 to e_MaxJoys - 1 do
    RemoveJoystick(i);

  if screen <> nil then
  begin
    FreeGL();
    SDL_FreeSurface(screen);
  end;

  SDL_Quit();
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
initialization
  conRegVar('sdl_allow_resize', @userResize, 'allow to resize window by user', 'allow to resize window by user');
  conRegVar('sdl_resize_action', @modeResize, 'set window resize mode (0: ignore, 1: change, 2: reset)', '');
  conRegVar('sdl_use_scancodes', @useScancodes, 'use platform-specific scancodes when possible', '');
