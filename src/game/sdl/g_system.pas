(* Copyright (C)  Doom 2D: Forever Developers
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../shared/a_modes.inc}
unit g_system;

interface

  uses Utils;

  (* --- Utils --- *)
  procedure sys_Delay (ms: Integer);

  (* --- Graphics --- *)
  function sys_GetDisplayModes (bpp: Integer): SSArray;
  function sys_SetDisplayMode (w, h, bpp: Integer; fullscreen, maximized: Boolean): Boolean;
  procedure sys_EnableVSync (yes: Boolean);
  procedure sys_Repaint;

  (* --- Input --- *)
  function sys_HandleInput (): Boolean;
  procedure sys_RequestQuit;

  (* --- Init --- *)
  procedure sys_Init;
  procedure sys_Final;

  var (* hooks *)
    sys_CharPress: procedure (ch: AnsiChar) = nil;
    sys_ScreenResize: procedure (w, h: Integer) = nil;

implementation

  uses
    {$IF DEFINED(DARWIN) OR DEFINED(MACOS)}
      Keyboards, Events,
      {$IFDEF CPU64}
        {$linkframework Carbon}
      {$ENDIF}
    {$ENDIF}
    SysUtils, SDL, Math,
    e_log, e_input, e_sound,
    g_options, g_console, g_game, g_basic;

  const
    GameTitle = 'Doom 2D: Forever (SDL 1.2, %s, %s)';

  var
    userResize: Boolean;
    modeResize: Integer;
    useScancodes: Boolean;
    screen: PSDL_Surface;
    JoystickHandle: array [0..e_MaxJoys - 1] of PSDL_Joystick;
    JoystickHatState: array [0..e_MaxJoys - 1, 0..e_MaxJoyHats - 1, HAT_LEFT..HAT_DOWN] of Boolean;
    JoystickZeroAxes: array [0..e_MaxJoys - 1, 0..e_MaxJoyAxes - 1] of Integer;

  (* --------- Utils --------- *)

  procedure sys_Delay (ms: Integer);
  begin
    SDL_Delay(ms)
  end;

  (* --------- Graphics --------- *)

  function GetDriver (): AnsiString;
    var buf: array [0..31] of AnsiChar;
  begin
    buf := '';
    SDL_VideoDriverName(buf, Length(buf));
    result := AnsiString(buf);
  end;

  function GetTitle (): AnsiString;
    var info: AnsiString;
  begin
    info := g_GetBuildHash(false);
    if info = 'custom build' then
      info := info + ' by ' + g_GetBuilderName() + ' ' + GAME_BUILDDATE + ' ' + GAME_BUILDTIME;
    result := Format(GameTitle, [GetDriver(), info]);
  end;

  function InitWindow (w, h, bpp: Integer; fullScreen: Boolean): Boolean;
    var flags: Uint32; title: AnsiString;
  begin
    e_LogWritefln('InitWindow %s %s %s %s', [w, h, bpp, fullScreen]);
    result := false;
    SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8); // lights; it is enough to have 1-bit stencil buffer for lighting, but...
    flags := SDL_OPENGL;
    if fullScreen then flags := flags or SDL_FULLSCREEN;
    if userResize then flags := flags or SDL_VIDEORESIZE;
    if (screen = nil) or (SDL_VideoModeOk(w, h, bpp, flags) <> 0) then
    begin
      SDL_FreeSurface(screen);
      screen := SDL_SetVideoMode(w, h, bpp, flags);
      if screen <> nil then
      begin
        title := GetTitle();
        SDL_WM_SetCaption(PChar(title), nil);
        gFullScreen := fullscreen;
        gRC_FullScreen := fullscreen;
        if @sys_ScreenResize <> nil then
          sys_ScreenResize(w, h);
        result := True
      end
    end
    else
    begin
      e_LogWritefln('SDL: video mode not supported', [])
    end
  end;

  procedure sys_Repaint;
  begin
    SDL_GL_SwapBuffers
  end;

  procedure sys_EnableVSync (yes: Boolean);
  begin
    (* ??? *)
  end;

  function sys_GetDisplayModes (bpp: Integer): SSArray;
    var m: PPSDL_Rect; f: TSDL_PixelFormat; i, count: Integer;
  begin
    result := nil;
    FillChar(f, sizeof(f), 0);
    f.palette := nil;
    f.BitsPerPixel := bpp;
    f.BytesPerPixel := (bpp + 7) div 8;
    m := SDL_ListModes(@f, SDL_OPENGL or SDL_FULLSCREEN);
    if (m <> NIL) and (UIntPtr(m) <> UIntPtr(-1)) then
    begin
      count := 0;
      while m[count] <> nil do inc(count);
      SetLength(result, count);
      for i := 0 to count - 1 do
        result[i] := IntToStr(m[i].w) + 'x' + IntToStr(m[i].h);
    end
  end;

  function sys_SetDisplayMode (w, h, bpp: Integer; fullscreen, maximized: Boolean): Boolean;
  begin
    result := InitWindow(w, h, bpp, fullscreen)
  end;

  (* --------- Joystick --------- *)

  procedure HandleJoyButton (var ev: TSDL_JoyButtonEvent);
    var down: Boolean; key: Integer;
  begin
    if (ev.which < e_MaxJoys) and (ev.button < e_MaxJoyBtns) then
    begin
      key := e_JoyButtonToKey(ev.which, ev.button);
      down := ev.type_ = SDL_JOYBUTTONDOWN;
      if g_dbg_input then
        e_LogWritefln('Input Debug: jbutton, joy=%s, button=%s, keycode=%s, press=%s', [ev.which, ev.button, key, down]);
      e_KeyUpDown(key, down);
      g_Console_ProcessBind(key, down)
    end
    else
    begin
      if g_dbg_input then
      begin
        down := ev.type_ = SDL_JOYBUTTONDOWN;
        e_LogWritefln('Input Debug: NOT IN RANGE! jbutton, joy=%s, button=%s, press=%s', [ev.which, ev.button, down])
      end
    end
  end;

  procedure HandleJoyAxis (var ev: TSDL_JoyAxisEvent);
    var key, minuskey: Integer;
  begin
    if (ev.which < e_MaxJoys) and (ev.axis < e_MaxJoyAxes) then
    begin
      key := e_JoyAxisToKey(ev.which, ev.axis, AX_PLUS);
      minuskey := e_JoyAxisToKey(ev.which, ev.axis, AX_MINUS);

      if g_dbg_input then
          e_LogWritefln('Input Debug: jaxis, joy=%s, axis=%s, value=%s, zeroaxes=%s, deadzone=%s', [ev.which, ev.axis, ev.value, JoystickZeroAxes[ev.which, ev.axis], e_JoystickDeadzones[ev.which]]);

      if ev.value < JoystickZeroAxes[ev.which, ev.axis] - e_JoystickDeadzones[ev.which] then
      begin
        if (e_KeyPressed(key)) then
        begin
          e_KeyUpDown(key, False);
          g_Console_ProcessBind(key, False)
        end;
        e_KeyUpDown(minuskey, True);
        g_Console_ProcessBind(minuskey, True)
      end
      else if ev.value > JoystickZeroAxes[ev.which, ev.axis] + e_JoystickDeadzones[ev.which] then
      begin
        if (e_KeyPressed(minuskey)) then
        begin
          e_KeyUpDown(minuskey, False);
          g_Console_ProcessBind(minuskey, False)
        end;
        e_KeyUpDown(key, True);
        g_Console_ProcessBind(key, True)
      end
      else
      begin
        if (e_KeyPressed(minuskey)) then
        begin
          e_KeyUpDown(minuskey, False);
          g_Console_ProcessBind(minuskey, False)
        end;
        if (e_KeyPressed(key)) then
        begin
          e_KeyUpDown(key, False);
          g_Console_ProcessBind(key, False)
        end
      end
    end
    else
    begin
      if g_dbg_input then
        e_LogWritefln('Input Debug: NOT IN RANGE! jaxis, joy=%s, axis=%s, value=%s, zeroaxes=%s, deadzone=%s', [ev.which, ev.axis, ev.value, JoystickZeroAxes[ev.which, ev.axis], e_JoystickDeadzones[ev.which]])
    end
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
          g_Console_ProcessBind(key, down)
        end
      end;
      JoystickHatState[ev.which, ev.hat] := hat
    end
    else
    begin
      if g_dbg_input then
        e_LogWritefln('Input Debug: NOT IN RANGE! jhat, joy=%s, hat=%s, value=%s', [ev.which, ev.hat, ev.value])
    end
  end;

  procedure AddJoystick (which: Integer);
    var i: Integer;
  begin
    assert(which < e_MaxJoys);
    JoystickHandle[which] := SDL_JoystickOpen(which);
    if JoystickHandle[which] <> nil then
    begin
      e_LogWritefln('Added Joystick %s', [which]);
      e_JoystickAvailable[which] := True;
      for i := 0 to Min(SDL_JoystickNumAxes(JoystickHandle[which]), e_MaxJoyAxes) - 1 do
        JoystickZeroAxes[which, i] := SDL_JoystickGetAxis(JoystickHandle[which], i)
    end
    else
    begin
      e_LogWritefln('Failed to open Joystick %s', [which])
    end
  end;

  procedure RemoveJoystick (which: Integer);
  begin
    assert(which < e_MaxJoys);
    e_LogWritefln('Remove Joystick %s', [which]);
    e_JoystickAvailable[which] := False;
    if JoystickHandle[which] <> nil then
      SDL_JoystickClose(JoystickHandle[which]);
    JoystickHandle[which] := nil
  end;

  (* --------- Input --------- *)

{$IF DEFINED(DARWIN) OR DEFINED(MACOS)}
  var
    IsMacPlatform: Boolean;

  function IsMacModSym (sym: Integer): Boolean;
  begin
    case sym of
      SDLK_NUMLOCK,
      SDLK_CAPSLOCK,
      SDLK_RCTRL, SDLK_LCTRL,
      SDLK_RSHIFT, SDLK_LSHIFT,
      SDLK_RALT, SDLK_LALT,
      SDLK_RSUPER, SDLK_LSUPER,
      SDLK_RMETA, SDLK_LMETA:
        result := true;
      otherwise
        result := false;
    end;
  end;

  function Mac2Stub (scancode: Integer): Integer;
  begin
    (* SDL 2 swap tilda/grave and paragraph/plus-minus buttons on ISO keyboards *)
    if ((scancode = 10) or (scancode = 50)) and (KBGetLayoutType(LMGetKbdType()) = kKeyboardISO) then
      scancode := 60 - scancode;
    case scancode of
      0: result := IK_A;
      1: result := IK_S;
      2: result := IK_D;
      3: result := IK_F;
      4: result := IK_H;
      5: result := IK_G;
      6: result := IK_Z;
      7: result := IK_X;
      8: result := IK_C;
      9: result := IK_V;
      10: result := IK_NONUSBACKSLASH;
      11: result := IK_B;
      12: result := IK_Q;
      13: result := IK_W;
      14: result := IK_E;
      15: result := IK_R;
      16: result := IK_Y;
      17: result := IK_T;
      18: result := IK_1;
      19: result := IK_2;
      20: result := IK_3;
      21: result := IK_4;
      22: result := IK_6;
      23: result := IK_5;
      24: result := IK_EQUALS;
      25: result := IK_9;
      26: result := IK_7;
      27: result := IK_MINUS;
      28: result := IK_8;
      29: result := IK_0;
      30: result := {IK_RIGHTBRACKET} IK_RBRACKET;
      31: result := IK_O;
      32: result := IK_U;
      33: result := {IK_LEFTBRACKET} IK_LBRACKET;
      34: result := IK_I;
      35: result := IK_P;
      36: result := IK_RETURN;
      37: result := IK_L;
      38: result := IK_J;
      39: result := {IK_APOSTROPHE} IK_QUOTE;
      40: result := IK_K;
      41: result := IK_SEMICOLON;
      42: result := IK_BACKSLASH;
      43: result := IK_COMMA;
      44: result := IK_SLASH;
      45: result := IK_N;
      46: result := IK_M;
      47: result := {IK_PERIOD} IK_DOT;
      48: result := IK_TAB;
      49: result := IK_SPACE;
      50: result := IK_GRAVE;
      51: result := IK_BACKSPACE;
      52: result := {IK_KP_ENTER} IK_KPRETURN;
      53: result := IK_ESCAPE;
      54: result := {IK_RGUI} IK_RWIN;
      55: result := {IK_LGUI} IK_WIN;
      56: result := {IK_LSHIFT} IK_SHIFT;
      57: result := IK_CAPSLOCK;
      58: result := {IK_LALT} IK_ALT;
      59: result := {IK_LCTRL} IK_CTRL;
      60: result := IK_RSHIFT;
      61: result := IK_RALT;
      62: result := IK_RCTRL;
      63: result := {IK_RGUI} IK_RWIN;
      {64: result := IK_F17;}
      65: result := {IK_KP_PERIOD} IK_KPDOT;
      66: result := IK_INVALID; (* unused? *)
      67: result := {IK_KP_MULTIPLY} IK_KPMULTIPLE;
      68: result := IK_INVALID; (* unused? *)
      69: result := {IK_KP_PLUS} IK_KPPLUS;
      70: result := IK_INVALID; (* unused? *)
      71: result := {IK_NUMLOCKCLEAR} IK_NUMLOCK;
      {72: result := IK_VOLUMEUP;}
      {73: result := IK_VOLUMEDOWN;}
      {74: result := IK_MUTE;}
      75: result := {IK_KP_DIVIDE} IK_KPDIVIDE;
      76: result := {IK_KP_ENTER} IK_KPRETURN;
      77: result := IK_INVALID; (* unused? *)
      78: result := {IK_KP_MINUS} IK_KPMINUS;
      {79: result := IK_F18;}
      {80: result := IK_F19;}
      {81: result := IK_KP_EQUALS;}
      82: result := {IK_KP_0} IK_KPINSERT;
      83: result := {IK_KP_1} IK_KPEND;
      84: result := {IK_KP_2} IK_KPDOWN;
      85: result := {IK_KP_3} IK_KPPAGEDN;
      86: result := {IK_KP_4} IK_KPLEFT;
      87: result := {IK_KP_5} IK_KP5;
      88: result := {IK_KP_6} IK_KPRIGHT;
      89: result := {IK_KP_7} IK_KPHOME;
      90: result := IK_INVALID; (* unused? *)
      91: result := {IK_KP_8} IK_KPUP;
      92: result := {IK_KP_9} IK_KPPAGEUP;
      {93: result := IK_INTERNATIONAL3;}
      {94: result := IK_INTERNATIONAL1;}
      {95: result := IK_KP_COMMA;}
      96: result := IK_F5;
      97: result := IK_F6;
      98: result := IK_F7;
      99: result := IK_F3;
      100: result := IK_F8;
      101: result := IK_F9;
      {102: result := IK_LANG2;}
      103: result := IK_F11;
      {104: result := IK_LANG1;}
      105: result := {IK_PRINTSCREEN} IK_PRINTSCR;
      {106: result := IK_F16;}
      107: result := IK_SCROLLLOCK;
      108: result := IK_INVALID; (* unused? *)
      109: result := IK_F10;
      {110: result := IK_APPLICATION;}
      111: result := IK_F12;
      112: result := IK_INVALID; (* unused? *)
      113: result := IK_PAUSE;
      114: result := IK_INSERT;
      115: result := IK_HOME;
      116: result := IK_PAGEUP;
      117: result := IK_DELETE;
      118: result := IK_F4;
      119: result := IK_END;
      120: result := IK_F2;
      121: result := {IK_PAGEDOWN} IK_PAGEDN;
      122: result := IK_F1;
      123: result := IK_LEFT;
      124: result := IK_RIGHT;
      125: result := IK_DOWN;
      126: result := IK_UP;
      {127: result := IK_POWER;}
    otherwise result := IK_INVALID
    end
  end;
{$ENDIF}

  function Key2Stub (key: Integer): Integer;
    var x: Integer;
  begin
    case key of
      SDLK_ESCAPE: x := IK_ESCAPE;
      SDLK_RETURN: x := IK_RETURN;
      SDLK_KP_ENTER: x := IK_KPRETURN;
      SDLK_KP0: x := IK_KPINSERT;
      SDLK_UP: x := IK_UP;
      SDLK_KP8: x := IK_KPUP;
      SDLK_DOWN: x := IK_DOWN;
      SDLK_KP2: x := IK_KPDOWN;
      SDLK_LEFT: x := IK_LEFT;
      SDLK_KP4: x := IK_KPLEFT;
      SDLK_RIGHT: x := IK_RIGHT;
      SDLK_KP6: x := IK_KPRIGHT;
      SDLK_DELETE: x := IK_DELETE;
      SDLK_HOME: x := IK_HOME;
      SDLK_KP7: x := IK_KPHOME;
      SDLK_INSERT: x := IK_INSERT;
      SDLK_SPACE: x := IK_SPACE;
      SDLK_LSHIFT: x := IK_SHIFT;
      SDLK_LALT: x := IK_ALT;
      SDLK_TAB: x := IK_TAB;
      SDLK_PAGEUP: x := IK_PAGEUP;
      SDLK_KP9: x := IK_KPPAGEUP;
      SDLK_PAGEDOWN: x := IK_PAGEDN;
      SDLK_KP3: x := IK_KPPAGEDN;
      SDLK_KP5: x := IK_KP5;
      SDLK_NUMLOCK: x := IK_NUMLOCK;
      SDLK_KP_DIVIDE: x := IK_KPDIVIDE;
      SDLK_KP_MULTIPLY: x := IK_KPMULTIPLE;
      SDLK_KP_MINUS: x := IK_KPMINUS;
      SDLK_KP_PLUS: x := IK_KPPLUS;
      SDLK_KP_PERIOD: x := IK_KPDOT;
      SDLK_CAPSLOCK: x := IK_CAPSLOCK;
      SDLK_RSHIFT: x := IK_RSHIFT;
      SDLK_LCTRL: x := IK_CTRL;
      SDLK_RCTRL: x := IK_RCTRL;
      SDLK_RALT: x := IK_RALT;
      SDLK_LSUPER: x := IK_WIN;
      SDLK_RSUPER: x := IK_RWIN;
      SDLK_MENU: x := IK_MENU;
      SDLK_PRINT: x := IK_PRINTSCR;
      SDLK_SCROLLOCK: x := IK_SCROLLLOCK;
      SDLK_LEFTBRACKET: x := IK_LBRACKET;
      SDLK_RIGHTBRACKET: x := IK_RBRACKET;
      SDLK_SEMICOLON: x := IK_SEMICOLON;
      SDLK_QUOTE: x := IK_QUOTE;
      SDLK_BACKSLASH: x := IK_BACKSLASH;
      SDLK_SLASH: x := IK_SLASH;
      SDLK_COMMA: x := IK_COMMA;
      SDLK_PERIOD: x := IK_DOT;
      SDLK_EQUALS: x := IK_EQUALS;
      SDLK_0: x := IK_0;
      SDLK_1: x := IK_1;
      SDLK_2: x := IK_2;
      SDLK_3: x := IK_3;
      SDLK_4: x := IK_4;
      SDLK_5: x := IK_5;
      SDLK_6: x := IK_6;
      SDLK_7: x := IK_7;
      SDLK_8: x := IK_8;
      SDLK_9: x := IK_9;
      SDLK_F1: x := IK_F1;
      SDLK_F2: x := IK_F2;
      SDLK_F3: x := IK_F3;
      SDLK_F4: x := IK_F4;
      SDLK_F5: x := IK_F5;
      SDLK_F6: x := IK_F6;
      SDLK_F7: x := IK_F7;
      SDLK_F8: x := IK_F8;
      SDLK_F9: x := IK_F9;
      SDLK_F10: x := IK_F10;
      SDLK_F11: x := IK_F11;
      SDLK_F12: x := IK_F12;
      SDLK_END: x := IK_END;
      SDLK_KP1: x := IK_KPEND;
      SDLK_BACKSPACE: x := IK_BACKSPACE;
      SDLK_BACKQUOTE: x := IK_BACKQUOTE;
      SDLK_PAUSE: x := IK_PAUSE;
      SDLK_A..SDLK_Z: x := IK_A + (key - SDLK_A);
      SDLK_MINUS: x := IK_MINUS;
    else
      x := IK_INVALID
    end;
    result := x
  end;

  procedure HandleKeyboard (var ev: TSDL_KeyboardEvent);
    var down, repeated: Boolean; key: Integer; ch: Char;
  begin
    key := IK_INVALID;
    if useScancodes then
    begin
      {$IF DEFINED(DARWIN) OR DEFINED(MACOS)}
        if IsMacPlatform then
          if IsMacModSym(ev.keysym.sym) = false then
            key := Mac2Stub(ev.keysym.scancode);
      {$ENDIF}
    end;
    if key = IK_INVALID then
      key := Key2Stub(ev.keysym.sym);
    down := (ev.type_ = SDL_KEYDOWN);
    repeated := down and e_KeyPressed(key);
    ch := wchar2win(WideChar(ev.keysym.unicode));
    if g_dbg_input then
      e_LogWritefln('Input Debug: keysym, scancode=%s, down=%s, sym=%s, state=%s, unicode=%s, stubsym=%s, cp1251=%s', [ev.keysym.scancode, down, ev.keysym.sym, ev.state, ev.keysym.unicode, key, Ord(ch)]);
    if not repeated then
    begin
      e_KeyUpDown(key, down);
      g_Console_ProcessBind(key, down);
    end
    else
    begin
      g_Console_ProcessBindRepeat(key)
    end;
    if @sys_CharPress <> nil then
      if down and IsValid1251(ev.keysym.unicode) and IsPrintable1251(ch) then
        sys_CharPress(ch)
  end;

  procedure HandleResize (var ev: TSDL_ResizeEvent);
  begin
    if g_dbg_input then
      e_LogWritefln('Input Debug: SDL_VIDEORESIZE %s %s', [ev.w, ev.h]);
    if (modeResize = 1) and (@sys_ScreenResize <> nil) then
      sys_ScreenResize(ev.w, ev.h)
    else if modeResize > 1 then
      InitWindow(ev.w, ev.h, gBPP, gFullscreen)
  end;

  function sys_HandleInput (): Boolean;
    var ev: TSDL_Event;
  begin
    result := false;
    while SDL_PollEvent(@ev) <> 0 do
    begin
      case ev.type_ of
        SDL_QUITEV: result := true;
        SDL_VIDEORESIZE: HandleResize(ev.resize);
        SDL_KEYUP, SDL_KEYDOWN: HandleKeyboard(ev.key);
        SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP: HandleJoyButton(ev.jbutton);
        SDL_JOYAXISMOTION: HandleJoyAxis(ev.jaxis);
        SDL_JOYHATMOTION: HandleJoyHat(ev.jhat);
        SDL_VIDEOEXPOSE: sys_Repaint;
        SDL_ACTIVEEVENT: e_MuteChannels((ev.active.gain = 0) and gMuteWhenInactive);
      end
    end
  end;

  procedure sys_RequestQuit;
    var ev: TSDL_Event;
  begin
    ev.quit.type_ := SDL_QUITEV;
    SDL_PushEvent(@ev)
  end;

  (* --------- Init --------- *)

  procedure sys_Init;
    var flags: Uint32; i: Integer; name: AnsiString;
  begin
    e_WriteLog('Init SDL', TMsgType.Notify);
    flags := SDL_INIT_VIDEO or SDL_INIT_AUDIO or
             SDL_INIT_TIMER or SDL_INIT_JOYSTICK
             (*or SDL_INIT_NOPARACHUTE*);
    if SDL_Init(flags) <> 0 then
      raise Exception.Create('SDL: Init failed: ' + SDL_GetError);
    name := GetDriver();
    e_LogWritefln('SDL: Video Driver "%s"', [name]);
    {$IF DEFINED(DARWIN) OR DEFINED(MACOS)}
      IsMacPlatform := (name = 'Quartz') or (name = 'toolbox') or (name = 'DSp');
    {$ENDIF}
    SDL_EnableUNICODE(1);
    SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);
    for i := 0 to e_MaxJoys - 1 do
      AddJoystick(i)
  end;

  procedure sys_Final;
    var i: Integer;
  begin
    e_WriteLog('Releasing SDL', TMsgType.Notify);
    for i := 0 to e_MaxJoys - 1 do
      RemoveJoystick(i);
    if screen <> nil then
      SDL_FreeSurface(screen);
    SDL_Quit
  end;

initialization
  (* window resize are broken both on linux and osx, so disabled by default *)
  conRegVar('sdl_allow_resize', @userResize, 'allow to resize window by user', 'allow to resize window by user');
  conRegVar('sdl_resize_action', @modeResize, 'set window resize mode (0: ignore, 1: change, 2: reset)', '');
  conRegVar('sdl_use_scancodes', @useScancodes, 'use platform-specific scancodes when possible', '');
  userResize := false;
  modeResize := 0;
  useScancodes := true;
end.
