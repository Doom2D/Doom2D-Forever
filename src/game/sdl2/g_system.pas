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

  (* --- Graphics --- *)
  function sys_GetDisplayModes (bpp: Integer): SSArray;
  function sys_SetDisplayMode (w, h, bpp: Integer; fullscreen, maximized: Boolean): Boolean;
  procedure sys_EnableVSync (yes: Boolean);
  procedure sys_Repaint;

  (* --- Input --- *)
  function sys_HandleInput (): Boolean;
  procedure sys_RequestQuit;

{$IFDEF ENABLE_TOUCH}
  function sys_IsTextInputActive (): Boolean;
  procedure sys_ShowKeyboard (yes: Boolean);
{$ENDIF}

  (* --- Init --- *)
  procedure sys_Init;
  procedure sys_Final;

  var (* hooks *)
    sys_CharPress: procedure (ch: AnsiChar) = nil;
    sys_ScreenResize: procedure (w, h: Integer) = nil;

implementation

  uses
    {$IFDEF ENABLE_HOLMES}
      sdlcarcass,
    {$ENDIF}
    {$IFDEF ENABLE_RENDER}
      r_render,
    {$ENDIF}
    {$IFDEF ENABLE_MENU}
      g_gui,
    {$ENDIF}
    SysUtils, SDL2, Math, ctypes,
    e_log, e_input, e_sound,
    g_options, g_console, g_game, g_basic
  ;

  const
    GameTitle = 'Doom 2D: Forever (SDL 2, %s)';

  var
    window: PSDL_Window;
    context: TSDL_GLContext;
    display, wx, wy: Integer;
    wc: Boolean;
    JoystickHandle: array [0..e_MaxJoys - 1] of PSDL_Joystick;
    JoystickHatState: array [0..e_MaxJoys - 1, 0..e_MaxJoyHats - 1, HAT_LEFT..HAT_DOWN] of Boolean;
    JoystickZeroAxes: array [0..e_MaxJoys - 1, 0..e_MaxJoyAxes - 1] of Integer;

{$IFDEF ENABLE_TOUCH}
  var (* touch *)
    angleFire: Boolean;
    keyFinger: array [VK_FIRSTKEY..VK_LASTKEY] of Integer;
{$ENDIF}

  (* --------- Graphics --------- *)

  function GetTitle (): AnsiString;
    var info: AnsiString;
  begin
    info := g_GetBuildHash(false);
    if info = 'custom build' then
      info := info + ' by ' + g_GetBuilderName() + ' ' + GAME_BUILDDATE + ' ' + GAME_BUILDTIME;
    result := Format(GameTitle, [info]);
  end;

  function InitWindow (w, h, bpp: Integer; fullScreen, maximized: Boolean): Boolean;
    var flags: UInt32; x, y: cint; title: AnsiString;
  begin
    // note: on window close make: if assigned(oglDeinitCB) then oglDeinitCB;
    e_LogWritefln('InitWindow %s %s %s %s', [w, h, bpp, fullScreen]);
    result := false;
    if window = nil then
    begin
      {$IFDEF USE_GLES1}
        SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 1);
        SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 1);
        SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
      {$ELSE}
        SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
        SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 1);
        SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
        SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
        SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
        SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
        SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
        SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8); // lights; it is enough to have 1-bit stencil buffer for lighting, but...
      {$ENDIF}
      flags := SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE;
      if fullScreen then flags := flags or SDL_WINDOW_FULLSCREEN;
      if maximized then flags := flags or SDL_WINDOW_MAXIMIZED;
      if wc then
      begin
        x := SDL_WINDOWPOS_CENTERED;
        y := SDL_WINDOWPOS_CENTERED
      end
      else
      begin
        x := wx;
        y := wy
      end;
      title := GetTitle();
      window := SDL_CreateWindow(PChar(title), x, y, w, h, flags);
      if window <> nil then
      begin
        context := SDL_GL_CreateContext(window);
        if context <> nil then
        begin
          if (fullscreen = false) and (maximized = false) and (wc = false) then
          begin
            SDL_GetWindowPosition(window, @x, @y);
            wx := x; wy := y
          end;
          gFullScreen := fullscreen;
          gWinMaximized := maximized;
          gRC_FullScreen := fullscreen;
          gRC_Maximized := maximized;
          if @sys_ScreenResize <> nil then
            sys_ScreenResize(w, h);
          result := true
        end
        else
        begin
          // SDL_DestroyWindow(window);
          e_LogWritefln('SDL: unable to create OpenGL context: %s', [SDL_GetError])
        end
      end
      else
      begin
        e_LogWritefln('SDL: unable to create window: %s', [SDL_GetError])
      end
    end
    else
    begin
      if fullScreen then flags := SDL_WINDOW_FULLSCREEN else flags := 0;
      SDL_SetWindowFullscreen(window, flags);
      SDL_SetWindowSize(window, w, h);
      if maximized then SDL_MaximizeWindow(window);
      // always reset to center when changing fullscreen->windowed for safety purposes
      if wc or (gFullscreen and not fullscreen) or (gWinMaximized and not maximized) then
      begin
        x := SDL_WINDOWPOS_CENTERED;
        y := SDL_WINDOWPOS_CENTERED
      end
      else
      begin
        x := wx;
        y := wy
      end;
      SDL_SetWindowPosition(window, x, y);
      if (fullscreen = false) and (maximized = false) and (wc = false) then
      begin
        SDL_GetWindowPosition(window, @x, @y);
        wx := x; wy := y
      end;
      gFullScreen := fullscreen;
      gWinMaximized := maximized;
      gRC_FullScreen := fullscreen;
      gRC_Maximized := maximized;
      if @sys_ScreenResize <> nil then
        sys_ScreenResize(w, h);
      result := true
    end
  end;

  procedure sys_Repaint;
  begin
    SDL_GL_SwapWindow(window)
  end;

  procedure sys_EnableVSync (yes: Boolean);
  begin
    if yes then
      SDL_GL_SetSwapInterval(1)
    else
      SDL_GL_SetSwapInterval(0)
  end;

  function sys_GetDisplayModes (bpp: Integer): SSArray;
    var i, count, num, pw, ph: Integer; m: TSDL_DisplayMode;
  begin
    result := nil;
    num := SDL_GetNumDisplayModes(display);
    if num < 0 then
      e_LogWritefln('SDL: unable to get numer of available display modes: %s', [SDL_GetError]);
    if num > 0 then
    begin
      e_LogWritefln('Video modes for display %s:', [display]);
      SetLength(result, num);
      i := 0; count := 0; pw := 0; ph := 0;
      while i < num do
      begin
        SDL_GetDisplayMode(display, i, @m);
        if ((pw <> m.w) or (ph <> m.h)) then
        begin
          e_LogWritefln('* %sx%sx%s@%s', [m.w, m.h, SDL_BITSPERPIXEL(m.format), m.refresh_rate]);
          pw := m.w; ph := m.h;
          result[count] := IntToStr(m.w) + 'x' + IntToStr(m.h);
          Inc(count);
        end
        else
        begin
          e_LogWritefln('- %sx%sx%s@%s', [m.w, m.h, SDL_BITSPERPIXEL(m.format), m.refresh_rate]);
        end;
        Inc(i)
      end;
      SetLength(result, count)
    end
  end;

  function sys_SetDisplayMode (w, h, bpp: Integer; fullScreen, maximized: Boolean): Boolean;
  begin
    result := InitWindow(w, h, bpp, fullScreen, maximized)
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

  procedure HandleJoyAdd (var ev: TSDL_JoyDeviceEvent);
    var i: Integer;
  begin
    if (ev.which < e_MaxJoys) then
    begin
      JoystickHandle[ev.which] := SDL_JoystickOpen(ev.which);
      if JoystickHandle[ev.which] <> nil then
      begin
        e_LogWritefln('Added Joystick %s', [ev.which]);
        e_JoystickAvailable[ev.which] := True;
        for i := 0 to Min(SDL_JoystickNumAxes(JoystickHandle[ev.which]), e_MaxJoyAxes) - 1 do
          JoystickZeroAxes[ev.which, i] := SDL_JoystickGetAxis(JoystickHandle[ev.which], i)
      end
      else
      begin
        e_LogWritefln('Warning! Failed to open Joystick %s', [ev.which])
      end
    end
    else
    begin
      e_LogWritefln('Warning! Added Joystick %s, but we support only <= %s', [ev.which, e_MaxJoys])
    end
  end;

  procedure HandleJoyRemove (var ev: TSDL_JoyDeviceEvent);
  begin
    e_LogWritefln('Removed Joystick %s', [ev.which]);
    if (ev.which < e_MaxJoys) then
    begin
      e_JoystickAvailable[ev.which] := False;
      if JoystickHandle[ev.which] <> nil then
        SDL_JoystickClose(JoystickHandle[ev.which]);
      JoystickHandle[ev.which] := nil
    end
  end;

  (* --------- Touch --------- *)

{$IFDEF ENABLE_TOUCH}
  function sys_IsTextInputActive (): Boolean;
  begin
    Result := SDL_IsTextInputActive() = SDL_True
  end;

  procedure sys_ShowKeyboard (yes: Boolean);
  begin
    {$IFNDEF HEADLESS}
      if g_dbg_input then
        e_LogWritefln('g_Touch_ShowKeyboard(%s)', [yes]);
      (* on desktop we always receive text (needed for cheats) *)
      if yes or (SDL_HasScreenKeyboardSupport() = SDL_FALSE) then
        SDL_StartTextInput
      else
        SDL_StopTextInput
    {$ENDIF}
  end;

  procedure HandleTouch (const ev: TSDL_TouchFingerEvent);
    var
      x, y, i, finger: Integer;

    function IntersectControl(ctl, xx, yy: Integer): Boolean;
      {$IFDEF ENABLE_RENDER}
        var x, y, w, h: Integer; founded: Boolean;
      {$ENDIF}
    begin
      {$IFDEF ENABLE_RENDER}
        r_Render_GetKeyRect(ctl, x, y, w, h, founded);
        Result := founded and (xx >= x) and (yy >= y) and (xx <= x + w) and (yy <= y + h);
      {$ELSE}
        Result := False
      {$ENDIF}
    end;

    procedure KeyUp (finger, i: Integer);
    begin
      if g_dbg_input then
        e_LogWritefln('Input Debug: g_touch.KeyUp, finger=%s, key=%s', [finger, i]);

      keyFinger[i] := 0;
      e_KeyUpDown(i, False);
      g_Console_ProcessBind(i, False);

      (* up/down + fire hack *)
{$IFDEF ENABLE_MENU}
      if g_touch_fire and (gGameSettings.GameType <> GT_NONE) and (g_ActiveWindow = nil) and angleFire then
{$ELSE}
      if g_touch_fire and (gGameSettings.GameType <> GT_NONE) and angleFire then
{$ENDIF}
      begin
        if (i = VK_UP) or (i = VK_DOWN) then
        begin
          angleFire := False;
          keyFinger[VK_FIRE] := 0;
          e_KeyUpDown(VK_FIRE, False);
          g_Console_ProcessBind(VK_FIRE, False)
        end
      end
    end;

    procedure KeyDown (finger, i: Integer);
    begin
      if g_dbg_input then
        e_LogWritefln('Input Debug: g_touch.KeyDown, finger=%s, key=%s', [finger, i]);

      keyFinger[i] := finger;
      e_KeyUpDown(i, True);
      g_Console_ProcessBind(i, True);

      (* up/down + fire hack *)
{$IFDEF ENABLE_MENU}
      if g_touch_fire and (gGameSettings.GameType <> GT_NONE) and (g_ActiveWindow = nil) then
{$ELSE}
      if g_touch_fire and (gGameSettings.GameType <> GT_NONE) then
{$ENDIF}
      begin
        if i = VK_UP then
        begin
          angleFire := True;
          keyFinger[VK_FIRE] := -1;
          e_KeyUpDown(VK_FIRE, True);
          g_Console_ProcessBind(VK_FIRE, True)
        end
        else if i = VK_DOWN then
        begin
          angleFire := True;
          keyFinger[VK_FIRE] := -1;
          e_KeyUpDown(VK_FIRE, True);
          g_Console_ProcessBind(VK_FIRE, True)
        end
      end
    end;

    procedure KeyMotion (finger, i: Integer);
    begin
      if keyFinger[i] <> finger then
      begin
        KeyUp(finger, i);
        KeyDown(finger, i)
      end
    end;

  begin
    if not g_touch_enabled then
      Exit;

    finger := ev.fingerId + 2;
    x := Trunc(ev.x * gWinSizeX);
    y := Trunc(ev.y * gWinSizeY);

    for i := VK_FIRSTKEY to VK_LASTKEY do
    begin
      if IntersectControl(i, x, y) then
      begin
        if ev.type_ = SDL_FINGERUP then
          KeyUp(finger, i)
        else if ev.type_ = SDL_FINGERMOTION then
          KeyMotion(finger, i)
        else if ev.type_ = SDL_FINGERDOWN then
          keyDown(finger, i)
      end
      else if keyFinger[i] = finger then
      begin
        if ev.type_ = SDL_FINGERUP then
          KeyUp(finger, i)
        else if ev.type_ = SDL_FINGERMOTION then
          KeyUp(finger, i)
      end
    end
  end;
{$ENDIF} // TOUCH

  (* --------- Input --------- *)

  function HandleWindow (var ev: TSDL_WindowEvent): Boolean;
  begin
    result := false;
    if g_dbg_input then
      e_LogWritefln('Window Event: event = %s, data1 = %s, data2 = %s', [ev.event, ev.data1, ev.data2]);
    case ev.event of
      SDL_WINDOWEVENT_RESIZED:
        if @sys_ScreenResize <> nil then
          sys_ScreenResize(ev.data1, ev.data2);
      SDL_WINDOWEVENT_EXPOSED: sys_Repaint;
      SDL_WINDOWEVENT_CLOSE: result := true;
      SDL_WINDOWEVENT_MOVED:
        begin
          wx := ev.data1;
          wy := ev.data2
        end;
      SDL_WINDOWEVENT_FOCUS_LOST, SDL_WINDOWEVENT_MINIMIZED:
        begin
          e_UnpressAllKeys;
          if gMuteWhenInactive then
            e_MuteChannels(true);
          {$IFDEF ENABLE_HOLMES}
            if assigned(winBlurCB) then winBlurCB;
          {$ENDIF}
        end;
      SDL_WINDOWEVENT_FOCUS_GAINED, SDL_WINDOWEVENT_MAXIMIZED, SDL_WINDOWEVENT_RESTORED:
        begin
          if ev.event = SDL_WINDOWEVENT_MAXIMIZED then
          begin
            gWinMaximized := true;
            gRC_Maximized := true
          end
          else if ev.event = SDL_WINDOWEVENT_RESTORED then
          begin
            gWinMaximized := false;
            gRC_Maximized := false
          end;
          e_MuteChannels(false);
          {$IFDEF ENABLE_HOLMES}
            if assigned(winFocusCB) then winFocusCB;
          {$ENDIF}
        end;
    end
  end;

  procedure HandleKeyboard (var ev: TSDL_KeyboardEvent);
    var down: Boolean; key: Integer;
  begin
    key := ev.keysym.scancode;
    down := (ev.type_ = SDL_KEYDOWN);
    if key = SDL_SCANCODE_AC_BACK then
      key := SDL_SCANCODE_ESCAPE;
    {$IFDEF ENABLE_HOLMES}
      if fuiOnSDLEvent(PSDL_Event(@ev)^) then
      begin
        // event eaten, but...
        if not down then e_KeyUpDown(key, false);
        exit;
      end;
    {$ENDIF}
    if ev._repeat = 0 then
    begin
      if g_dbg_input then
        e_LogWritefln('Input Debug: keysym, press=%s, scancode=%s', [down, key]);
      e_KeyUpDown(key, down);
      g_Console_ProcessBind(key, down);
    end
    else
    begin
      if g_dbg_input then
        e_LogWritefln('Input Debug: keyrep, scancode=%s', [key]);
      g_Console_ProcessBindRepeat(key);
    end
  end;

  procedure HandleTextInput (var ev: TSDL_TextInputEvent);
    var ch: UnicodeChar; sch: AnsiChar;
  begin
    Utf8ToUnicode(@ch, PChar(ev.text), 1);
    sch := AnsiChar(wchar2win(ch));
    if g_dbg_input then
      e_LogWritefln('Input Debug: text, text="%s", ch = %s, sch = %s', [ev.text, Ord(ch), Ord(sch)]);
    if @sys_CharPress <> nil then
      if IsValid1251(Word(ch)) and IsPrintable1251(ch) then
        sys_CharPress(sch)
  end;

  function sys_HandleInput (): Boolean;
    var ev: TSDL_Event;
  begin
    result := false;
    ZeroMemory(@ev, sizeof(ev));
    while SDL_PollEvent(@ev) <> 0 do
    begin
      case ev.type_ of
        SDL_QUITEV: result := true;
        SDL_WINDOWEVENT: result := HandleWindow(ev.window);
        SDL_KEYUP, SDL_KEYDOWN: HandleKeyboard(ev.key);
        SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP: HandleJoyButton(ev.jbutton);
        SDL_JOYAXISMOTION: HandleJoyAxis(ev.jaxis);
        SDL_JOYHATMOTION: HandleJoyHat(ev.jhat);
        SDL_JOYDEVICEADDED: HandleJoyAdd(ev.jdevice);
        SDL_JOYDEVICEREMOVED: HandleJoyRemove(ev.jdevice);
        SDL_TEXTINPUT: HandleTextInput(ev.text);
        {$IFDEF ENABLE_TOUCH}
          SDL_FINGERMOTION, SDL_FINGERDOWN, SDL_FINGERUP: HandleTouch(ev.tfinger);
        {$ENDIF}
        {$IFDEF ENABLE_HOLMES}
          SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP, SDL_MOUSEWHEEL, SDL_MOUSEMOTION: fuiOnSDLEvent(ev);
        {$ENDIF}
      end
    end
  end;

  procedure sys_RequestQuit;
    var ev: TSDL_Event;
  begin
    ev.type_ := SDL_QUITEV;
    SDL_PushEvent(@ev)
  end;

  (* --------- Init --------- *)

  procedure sys_Init;
    var flags: UInt32;
  begin
    e_WriteLog('Init SDL2', TMsgType.Notify);
    {$IFDEF HEADLESS}
      {$IFDEF USE_SDLMIXER}
        flags := SDL_INIT_TIMER or SDL_INIT_AUDIO or $00004000;
      {$ELSE}
        flags := SDL_INIT_TIMER or $00004000;
      {$ENDIF}
    {$ELSE}
      flags := SDL_INIT_TIMER or SDL_INIT_VIDEO;
    {$ENDIF}
    SDL_SetHint(SDL_HINT_ACCELEROMETER_AS_JOYSTICK, '0');
    if SDL_Init(flags) <> 0 then
      raise Exception.Create('SDL: Init failed: ' + SDL_GetError);
    {$IFNDEF HEADLESS}
      if SDL_InitSubSystem(SDL_INIT_JOYSTICK) <> 0 then
        e_LogWritefln('SDL: Init subsystem failed: %s', [SDL_GetError()]);
    {$ENDIF}
    SDL_ShowCursor(SDL_DISABLE);
    {$IFDEF ENABLE_TOUCH}
      sys_ShowKeyboard(FALSE);
      g_touch_enabled := SDL_GetNumTouchDevices() > 0;
    {$ELSE}
      g_touch_enabled := False;
    {$ENDIF}
  end;

  procedure sys_Final;
  begin
    e_WriteLog('Releasing SDL2', TMsgType.Notify);
    if context <> nil then
    begin
      SDL_GL_DeleteContext(context);
      context := nil;
    end;
    if window <> nil then
    begin
      SDL_DestroyWindow(window);
      window := nil;
    end;
    SDL_Quit
  end;

initialization
  conRegVar('sdl2_display_index', @display, 'use display index as base', '');
  conRegVar('sdl2_window_x', @wx, 'window position x', '');
  conRegVar('sdl2_window_y', @wy, 'window position y', '');
  conRegVar('sdl2_window_center', @wc, 'force window creation at center', '');
  display := 0;
  wx := SDL_WINDOWPOS_CENTERED;
  wy := SDL_WINDOWPOS_CENTERED;
  wc := false
end.
