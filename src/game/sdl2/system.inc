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
  SysUtils, Math, ctypes,
  SDL2, {$INCLUDE ../../nogl/noGLuses.inc}
  e_log, e_graphics, e_input, {$IFDEF ENABLE_SOUND}e_sound,{$ENDIF}
{$IFDEF ENABLE_HOLMES}
  g_holmes, sdlcarcass, fui_ctls,
{$ENDIF}
  g_main, g_basic, g_options, g_language,
  g_window, g_console, g_game, g_menu, g_gui, g_touch;

var
  window: PSDL_Window;
  context: TSDL_GLContext;
  display: Integer;
  wx: Integer = SDL_WINDOWPOS_CENTERED;
  wy: Integer = SDL_WINDOWPOS_CENTERED;
  wc: Boolean;

  JoystickHandle: array [0..e_MaxJoys - 1] of PSDL_Joystick;
  JoystickHatState: array [0..e_MaxJoys - 1, 0..e_MaxJoyHats - 1, HAT_LEFT..HAT_DOWN] of Boolean;
  JoystickZeroAxes: array [0..e_MaxJoys - 1, 0..e_MaxJoyAxes - 1] of Integer;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

// TODO: Support OpenGL 3.0+ with compatibility (aka "legacy") profile? Would require to take that
// into account when loading Gl extensions (see `LoadAsCore` parameters in GLext functions), because
// newer contexts are not obliged to advertise the extensions that were promoted to base features.
// - https://stackoverflow.com/questions/23819439/to-be-backwards-compatible-are-you-supposed-to-use-arb-extensions-instead-of-co
// - https://stackoverflow.com/questions/50673534/opengl-extension-availability-on-newer-contexts
// - https://stackoverflow.com/questions/73745603/detect-the-opengl-context-profile-before-version-3-2
// - https://stackoverflow.com/questions/15998505/how-to-know-whether-particular-extension-was-promoted-to-a-core-profile-was-mad
// - https://stackoverflow.com/questions/26323410/using-opengl-core-functions-along-with-extension
// - https://stackoverflow.com/questions/42518428/opengl-command-implemented-but-not-advertised-with-extension-version
function LoadGL (): Boolean;
var
  fbo: Boolean;
  ltmp: Integer;
begin
  Result := True;

{$IFNDEF NOGL_INIT}
  // https://registry.khronos.org/OpenGL/extensions/ARB/ARB_framebuffer_object.txt
  fbo := Load_GL_ARB_framebuffer_object();
  // TODO: Support also the GL_EXT_framebuffer_object? https://gamedev.ru/code/forum/?id=199279
  // https://registry.khronos.org/OpenGL/extensions/EXT/EXT_framebuffer_object.txt
{$ELSE}
  nogl_Init();

  // https://registry.khronos.org/OpenGL/extensions/OES/OES_framebuffer_object.txt
  fbo := nogl_ExtensionSupported('GL_OES_framebuffer_object');
{$ENDIF}

  if glRenderToFBO and not fbo then
  begin
    e_LogWriteln('GL: framebuffer objects not supported; disabling FBO rendering');
    glRenderToFBO := False;
  end;

  // TODO: SDL is prone to false-negative conclusions here (e.g. on Intel HD Graphics 3000).
  // https://github.com/libsdl-org/SDL/issues/12070
  if SDL_GL_GetAttribute(SDL_GL_STENCIL_SIZE, @ltmp) = 0 then
  begin
    e_LogWritefln('stencil buffer size: %s', [ltmp]);
    gwin_has_stencil := ltmp > 0;
  end else begin
    e_LogWriteln('failed to retrieve stencil buffer size: ' + SDL_GetError());
  end;

  // TODO: Validate more requested attributes here?..
end;

procedure FreeGL ();
begin
{$IFDEF NOGL_INIT}
  nogl_Quit();
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

function GetTitle (): AnsiString;
var
  info: AnsiString;
begin
  info := g_GetBuildHash(False);
  if info = 'custom build' then
    info += ' by ' + g_GetBuilderName() + ' ' + GAME_BUILDDATE + ' ' + GAME_BUILDTIME;

  Result := Format(GAME_NAME+' (SDL2, %s)', [info]);
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

{$IFDEF ENABLE_HOLMES}
  if Assigned(oglInitCB) then oglInitCB();
{$ENDIF}
end;

function InitWindow (w, h, bpp: Integer; fullScreen, maximized: Boolean): Boolean;
var
  flags: UInt32;
  x, y: cint;
begin
  // NB: On window close, do this for FlexUI: if @oglDeinitCB <> nil then oglDeinitCB();
  Result := False;
  e_LogWritefln('InitWindow %s %s %s %s', [w, h, bpp, fullScreen]);

  // NB: Request only those requirements that are vital for the code to work correctly.
  // It's better to rely on chance and reasonable defaults for things like SDL_GL_DOUBLEBUFFER.
  if window = nil then
  begin
  {$IFNDEF USE_GLES1}
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, OGL_VERSION_MAJOR);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, OGL_VERSION_MINOR);
    (** TODO: Explicitly disallow core profile if only OpenGL 3.0+ context is available? We're tied
      strictly to the fixed pipeline at the moment, thus not supporting "core-only" rendering. But
      it's not clear if SDL will give us then a pre-3.0 context if there is no backward-compatible
      profiles (e.g. as on Mac OS X). See also SDL docs for SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG.
      - https://stackoverflow.com/questions/29421798/set-opengl-context-3-3-with-sdl2-on-mac-not-working
      - https://stackoverflow.com/questions/19658745/why-is-my-opengl-version-always-2-1-on-mac-os-x
      - https://stackoverflow.com/questions/32958071/why-does-my-mac-only-support-opengl-2-1/
      - https://github.com/glfw/glfw/issues/904 - OpenGL 3.2+ context creation poorly documented
      - https://github.com/glfw/glfw/issues/107 - OpenGL 3.2 only supported with forward-compatible core profile
    **)
    // NB: On Windows, this forces usage of wglCreateContextAttribsARB() even for pre-3.0 contexts.
    // Same for any of SDL_GL_CONTEXT_FLAGS. See WIN_GL_CreateContext() in SDL sources for details.
    //SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_COMPATIBILITY);
  {$ELSE}
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, GLES_VERSION_MAJOR);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, GLES_VERSION_MINOR);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
  {$ENDIF}

    SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);

    // TODO: Use 8-bit stencils (see g_game.renderDynLightsInternal() for the optimization comment).
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 1);  // for experimental stencil-shadowed lights

    flags := SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE;
    if fullScreen then flags := flags or SDL_WINDOW_FULLSCREEN;
    if maximized then flags := flags or SDL_WINDOW_MAXIMIZED;

    if wc then
    begin
      x := SDL_WINDOWPOS_CENTERED;
      y := SDL_WINDOWPOS_CENTERED;
    end
    else
    begin
      x := wx;
      y := wy;
    end;

    window := SDL_CreateWindow(PChar(GetTitle()), x, y, w, h, flags);
    if window = nil then
    begin
      e_LogWritefln('SDL2: unable to create window: %s', [SDL_GetError()]);
      Exit;
    end;

    context := SDL_GL_CreateContext(window);
    if context = nil then
    begin
      //SDL_DestroyWindow(window);
      e_LogWritefln('SDL2: unable to create OpenGL context: %s', [SDL_GetError()]);
      Exit;
    end;

    if not LoadGL() then
    begin
      e_LogWriteln('GL: unable to load OpenGL functions', TMsgType.Fatal);
      SDL_GL_DeleteContext(context);
      context := nil;
      Exit;
    end;
  end
  else
  begin
    if fullScreen
      then flags := SDL_WINDOW_FULLSCREEN
      else flags := 0;

    SDL_SetWindowFullscreen(window, flags);
    SDL_SetWindowSize(window, w, h);
    if maximized then SDL_MaximizeWindow(window);

    // always reset to center when changing fullscreen->windowed for safety purposes
    if wc or (gFullscreen and not fullscreen) or (gWinMaximized and not maximized) then
    begin
      x := SDL_WINDOWPOS_CENTERED;
      y := SDL_WINDOWPOS_CENTERED;
    end
    else
    begin
      x := wx;
      y := wy;
    end;

    SDL_SetWindowPosition(window, x, y);
  end;

  if not fullscreen and not maximized and not wc then
  begin
    SDL_GetWindowPosition(window, @x, @y);
    wx := x;
    wy := y;
  end;

  gFullScreen := fullscreen;
  gRC_FullScreen := fullscreen;
  gWinMaximized := maximized;
  gRC_Maximized := maximized;

  UpdateSize(w, h);
  Result := True;
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

procedure HandleJoyAdd (var ev: TSDL_JoyDeviceEvent);
var
  i: Integer;
begin
  if ev.which < e_MaxJoys then
  begin
    JoystickHandle[ev.which] := SDL_JoystickOpen(ev.which);
    if JoystickHandle[ev.which] <> nil then
    begin
      e_LogWritefln('Added Joystick %s', [ev.which]);
      e_JoystickAvailable[ev.which] := True;
      for i := 0 to Min(SDL_JoystickNumAxes(JoystickHandle[ev.which]), e_MaxJoyAxes) - 1 do
        JoystickZeroAxes[ev.which, i] := SDL_JoystickGetAxis(JoystickHandle[ev.which], i);
    end
    else
    begin
      e_LogWritefln('Warning! Failed to open Joystick %s', [ev.which]);
    end;
  end
  else
  begin
    e_LogWritefln('Warning! Added Joystick %s, but we support only <= %s', [ev.which, e_MaxJoys]);
  end;
end;

procedure HandleJoyRemove (var ev: TSDL_JoyDeviceEvent);
begin
  e_LogWritefln('Removed Joystick %s', [ev.which]);
  if ev.which < e_MaxJoys then
  begin
    e_JoystickAvailable[ev.which] := False;
    if JoystickHandle[ev.which] <> nil then
    begin
      SDL_JoystickClose(JoystickHandle[ev.which]);
      JoystickHandle[ev.which] := nil;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

function HandleWindow (var ev: TSDL_WindowEvent): Boolean;
begin
  Result := False;
  if g_dbg_input then
    e_LogWritefln('Window Event: event = %s, data1 = %s, data2 = %s', [ev.event, ev.data1, ev.data2]);

  case ev.event of
    SDL_WINDOWEVENT_RESIZED: UpdateSize(ev.data1, ev.data2);
    SDL_WINDOWEVENT_EXPOSED: sys_Repaint();
    SDL_WINDOWEVENT_CLOSE: Result := True;

    SDL_WINDOWEVENT_MOVED:
    begin
      wx := ev.data1;
      wy := ev.data2;
    end;

    SDL_WINDOWEVENT_FOCUS_LOST, SDL_WINDOWEVENT_MINIMIZED:
    begin
      e_UnpressAllKeys();
    {$IFDEF ENABLE_SOUND}
      if gMuteWhenInactive then e_MuteChannels(True);
    {$ENDIF}
    {$IFDEF ENABLE_HOLMES}
      if Assigned(winBlurCB) then winBlurCB();
    {$ENDIF}
    end;

    SDL_WINDOWEVENT_FOCUS_GAINED, SDL_WINDOWEVENT_MAXIMIZED, SDL_WINDOWEVENT_RESTORED:
    begin
      if ev.event = SDL_WINDOWEVENT_MAXIMIZED then
      begin
        gWinMaximized := True;
        gRC_Maximized := True;
      end
      else if ev.event = SDL_WINDOWEVENT_RESTORED then
      begin
        gWinMaximized := False;
        gRC_Maximized := False;
      end;
    {$IFDEF ENABLE_SOUND}
      e_MuteChannels(False);
    {$ENDIF}
    {$IFDEF ENABLE_HOLMES}
      if Assigned(winFocusCB) then winFocusCB();
    {$ENDIF}
    end;
  end;
end;

procedure HandleKeyboard (var ev: TSDL_KeyboardEvent);
var
  down: Boolean;
  key: Integer;
begin
  key := ev.keysym.scancode;
  down := ev.type_ = SDL_KEYDOWN;
  if key = SDL_SCANCODE_AC_BACK then
    key := SDL_SCANCODE_ESCAPE;

{$IFDEF ENABLE_HOLMES}
  if fuiOnSDLEvent(PSDL_Event(@ev)^) then
  begin
    // event eaten, but...
    if not down then e_KeyUpDown(key, False);
    Exit;
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
  end;
end;

procedure HandleTextInput (var ev: TSDL_TextInputEvent);
var
  ch: UnicodeChar;
  sch: AnsiChar;
begin
  Utf8ToUnicode(@ch, PChar(ev.text), 1);
  sch := AnsiChar(wchar2win(ch));

  if g_dbg_input then
    e_LogWritefln('Input Debug: text, text="%s", ch = %s, sch = %s', [ev.text, Ord(ch), Ord(sch)]);

  if IsValid1251(Word(ch)) and IsPrintable1251(ch) then
    CharPress(sch);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

function sys_HandleEvents (): Boolean;
var
  ev: TSDL_Event;
begin
  Result := False;
  ZeroMemory(@ev, SizeOf(ev));

  // NB: Non-zero timeout is required here by the engine because this prevents abnormal CPU load.
  while SDL_WaitEventTimeout(@ev, 1) <> 0 do
  begin
    case ev.type_ of
      SDL_QUITEV: Result := True;

      SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP: HandleJoyButton(ev.jbutton);
      SDL_JOYAXISMOTION: HandleJoyAxis(ev.jaxis);
      SDL_JOYHATMOTION: HandleJoyHat(ev.jhat);
      SDL_JOYDEVICEADDED: HandleJoyAdd(ev.jdevice);
      SDL_JOYDEVICEREMOVED: HandleJoyRemove(ev.jdevice);

      SDL_WINDOWEVENT: Result := HandleWindow(ev.window);
      SDL_KEYUP, SDL_KEYDOWN: HandleKeyboard(ev.key);
      SDL_TEXTINPUT: HandleTextInput(ev.text);
      SDL_FINGERMOTION, SDL_FINGERDOWN, SDL_FINGERUP: g_Touch_HandleEvent(ev.tfinger);
    {$IFDEF ENABLE_HOLMES}
      SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP, SDL_MOUSEWHEEL, SDL_MOUSEMOTION: fuiOnSDLEvent(ev);
    {$ENDIF}

      //else
      //  if g_dbg_input then
      //    e_LogWritefln('Input Debug: unhandled event %s', [ev.type_]);
    end;
  end;
end;

procedure sys_RequestQuit ();
var
  ev: TSDL_Event;
begin
  ev.type_ := SDL_QUITEV;
  SDL_PushEvent(@ev);
end;

procedure sys_YieldTimeSlice ();
begin
  // NB: At the moment, this is just a hint.
  // https://www.freepascal.org/docs-html/3.2.2/rtl/system/threadswitch.html
  ThreadSwitch();
end;

procedure sys_Delay (ms: Integer);
begin
  SDL_Delay(ms);
end;

function sys_GetTicks (): Int64;
begin
  Result := SDL_GetTicks64();
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

procedure sys_EnableVSync (yes: Boolean);
begin
  SDL_GL_SetSwapInterval(Ord(yes));
end;

function sys_GetDisplayModes (bpp: Integer): SSArray;
var
  m: TSDL_DisplayMode;
  i, count, num, pw, ph: Integer;
  c: AnsiChar;
begin
  Result := nil;
  num := SDL_GetNumDisplayModes(display);
  if num < 0 then
  begin
    e_LogWritefln('SDL2: unable to get number of available display modes: %s', [SDL_GetError()]);
    Exit;
  end;

  if num = 0 then
    Exit;

  e_LogWritefln('Video modes for display %s:', [display]);
  SetLength(Result, num);

  count := 0;
  pw := 0;
  ph := 0;

  for i := 0 to num-1 do
  begin
    SDL_GetDisplayMode(display, i, @m);
    if (pw <> m.w) or (ph <> m.h) then
    begin
      c := '*';
      pw := m.w;
      ph := m.h;
      Result[count] := IntToStr(m.w) + 'x' + IntToStr(m.h);
      count += 1;
    end
    else
      c := '-';

    e_LogWritefln(c+' %sx%sx%s@%s', [m.w, m.h, SDL_BITSPERPIXEL(m.format), m.refresh_rate]);
  end;

  SetLength(Result, count);
end;

function sys_SetDisplayMode (w, h, bpp: Integer; fullScreen, maximized, reset: Boolean): Boolean;
begin
  if (window = nil) or reset
    then Result := InitWindow(Max(1, w), Max(1, h), Max(1, bpp), fullScreen, maximized)
    else Result := True;
end;

procedure sys_Repaint ();
begin
  SDL_GL_SwapWindow(window);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

procedure sys_Init ();
var
  flags: UInt32;
begin
  e_WriteLog('Init SDL2', TMsgType.Notify);
{$IFDEF ANDROID}
  SDL_SetHint(SDL_HINT_ORIENTATIONS, 'LandscapeLeft LandscapeRight');
{$ENDIF}

  // NOTE: No explicit SDL_INIT_EVENTS here as it will be involved by other subsystems if needed.
  flags := SDL_INIT_TIMER
    {$IFNDEF HEADLESS} or SDL_INIT_VIDEO {$ENDIF};

  if SDL_Init(flags) <> 0 then
    raise Exception.Create('SDL2: Initialization failed: ' + SDL_GetError());

{$IFDEF USE_SDLMIXER}
  if SDL_InitSubSystem(SDL_INIT_AUDIO) <> 0 then
    e_LogWritefln('SDL2: Failed to initialize audio subsystem: %s', [SDL_GetError()]);
{$ENDIF}

{$IFNDEF HEADLESS}
  SDL_SetHint(SDL_HINT_ACCELEROMETER_AS_JOYSTICK, '0');
  if SDL_InitSubSystem(SDL_INIT_JOYSTICK) <> 0 then
    e_LogWritefln('SDL2: Failed to initialize joystick subsystem: %s', [SDL_GetError()]);
{$ENDIF}

  SDL_ShowCursor(SDL_DISABLE);
end;

procedure sys_Final ();
begin
  e_WriteLog('Releasing SDL2', TMsgType.Notify);

  if context <> nil then
  begin
    FreeGL();
    SDL_GL_DeleteContext(context);
    context := nil;
  end;

  if window <> nil then
  begin
    SDL_DestroyWindow(window);
    window := nil;
  end;

  SDL_Quit();
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
initialization
  conRegVar('sdl2_display_index', @display, 'use display index as base', '');
  conRegVar('sdl2_window_x', @wx, 'window position x', '');
  conRegVar('sdl2_window_y', @wy, 'window position y', '');
  conRegVar('sdl2_window_center', @wc, 'force window creation at center', '');
