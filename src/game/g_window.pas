(* Copyright (C)  Doom 2D: Forever Developers
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
unit g_window;

interface

uses
  utils;

function SDLMain (): Integer;
function GetTimer (): Int64;
procedure ResetTimer ();
procedure PushExitEvent ();
function ProcessMessage (): Boolean;
procedure ReDrawWindow ();
procedure SwapBuffers ();
procedure Sleep (ms: LongWord);
function GetDisplayModes (dbpp: LongWord; var selres: LongWord): SSArray;
function g_Window_SetDisplay (preserveGL: Boolean=false): Boolean;
function g_Window_SetSize (w, h: Word; fullscreen: Boolean): Boolean;
procedure g_SetVSync (vsync: Boolean);

procedure ProcessLoading (forceUpdate: Boolean=false);

// returns `true` if quit event was received
function g_ProcessMessages (): Boolean;


var
  gwin_dump_extensions: Boolean = false;
  gwin_has_stencil: Boolean = false;
  gwin_k8_enable_light_experiments: Boolean = false;
  g_dbg_aimline_on: Boolean = false;
  g_dbg_input: Boolean = False;


implementation

uses
{$IFDEF WINDOWS}Windows,{$ENDIF}
{$INCLUDE ../nogl/noGLuses.inc}
{$IFDEF ENABLE_HOLMES}
  g_holmes, sdlcarcass, fui_ctls,
{$ENDIF}
  SysUtils, Classes, MAPDEF, Math,
  SDL2, e_graphics, e_log, e_texture, g_main,
  g_console, e_input, g_options, g_game,
  g_basic, g_textures, e_sound, g_sound, g_menu, ENet, g_net,
  g_map, g_gfx, g_monsters, xprofiler,
  g_touch;


const
  ProgressUpdateMSecs = 1;//100;

var
  h_Wnd: PSDL_Window = nil;
  h_GL: TSDL_GLContext = nil;
  Time, Time_Delta, Time_Old: Int64;
  flag: Boolean;
{$IF not DEFINED(HEADLESS)}
  wTitle: PChar = nil;
  wasFullscreen: Boolean = true; // so we need to recreate the window
{$ENDIF}
  wNeedTimeReset: Boolean = false;
  wMinimized: Boolean = false;
  wMaximized: Boolean = false;
  wLoadingProgress: Boolean = false;
  wLoadingQuit: Boolean = false;
{$IFNDEF WINDOWS}
  ticksOverflow: Int64 = -1;
  lastTicks: Uint32 = 0; // to detect overflow
{$ENDIF}
  JoystickHatState: array [0..e_MaxJoys, 0..e_MaxJoyHats, HAT_LEFT..HAT_DOWN] of Boolean;
  JoystickZeroAxes: array [0..e_MaxJoys, 0..e_MaxJoyAxes] of Integer;

procedure KillGLWindow (preserveGL: Boolean);
begin
{$IFDEF ENABLE_HOLMES}
  if (h_GL <> nil) and (not preserveGL) then begin if (assigned(oglDeinitCB)) then oglDeinitCB(); end;
{$ENDIF}
  if (h_Wnd <> nil) then SDL_DestroyWindow(h_Wnd);
  if (h_GL <> nil) and (not preserveGL) then
  begin

{$IFDEF USE_NANOGL}
    nanoGL_Destroy;
{$ENDIF}

{$IFDEF USE_NOGL}
    nogl_Quit;
{$ENDIF}

    SDL_GL_DeleteContext(h_GL);
  end;
  h_Wnd := nil;
  if (not preserveGL) then h_GL := nil;
end;


function g_Window_SetDisplay (preserveGL: Boolean = false): Boolean;
{$IF not DEFINED(HEADLESS)}
var
  mode, cmode: TSDL_DisplayMode;
  wFlags: LongWord = 0;
  nw, nh: Integer;
{$ENDIF}
begin
{$IF not DEFINED(HEADLESS)}
  result := false;

  e_WriteLog('Setting display mode...', TMsgType.Notify);

  wFlags := SDL_WINDOW_OPENGL {or SDL_WINDOW_RESIZABLE};
  if gFullscreen then wFlags := wFlags {or SDL_WINDOW_FULLSCREEN} else wFlags := wFlags or SDL_WINDOW_RESIZABLE;
  if (not gFullscreen) and (not preserveGL) and gWinMaximized then wFlags := wFlags or SDL_WINDOW_MAXIMIZED else gWinMaximized := false;

  if gFullscreen then
  begin
    mode.w := gScreenWidth;
    mode.h := gScreenHeight;
    mode.format := 0;
    mode.refresh_rate := 0;
    mode.driverdata := nil;
    if (SDL_GetClosestDisplayMode(0, @mode, @cmode) = nil) then
    begin
      e_WriteLog('SDL: cannot find display mode for '+IntToStr(gScreenWidth), TMsgType.Notify);
      gScreenWidth := 800;
      gScreenHeight := 600;
    end
    else
    begin
      e_WriteLog('SDL: found display mode for '+IntToStr(gScreenWidth)+'x'+IntToStr(gScreenHeight)+': '+IntToStr(cmode.w)+'x'+IntToStr(cmode.h), TMsgType.Notify);
      gScreenWidth := cmode.w;
      gScreenHeight := cmode.h;
    end;
  end;

  if (preserveGL) and (h_Wnd <> nil) and (not gFullscreen) and (not wasFullscreen) then
  begin
    //SDL_SetWindowMaximumSize(h_Wnd, gScreenWidth, gScreenHeight);
    //SDL_SetWindowDisplayMode(h_Wnd, @cmode);
    if (wMaximized) then SDL_RestoreWindow(h_Wnd);
    wMaximized := false;
    gWinMaximized := false;
    SDL_SetWindowSize(h_Wnd, gScreenWidth, gScreenHeight);
    //SDL_SetWindowFullscreen(h_Wnd, SDL_WINDOW_FULLSCREEN);
    //SDL_SetWindowFullscreen(h_Wnd, 0);
  end
  else
  begin
    KillGLWindow(preserveGL);
    h_Wnd := SDL_CreateWindow(PChar(wTitle), gWinRealPosX, gWinRealPosY, gScreenWidth, gScreenHeight, wFlags);
    if gFullscreen then
      SDL_SetWindowFullscreen(h_Wnd, SDL_WINDOW_FULLSCREEN);
    if (h_Wnd = nil) then exit;
  end;
  wasFullscreen := gFullscreen;

  SDL_GL_MakeCurrent(h_Wnd, h_GL);
  SDL_ShowCursor(SDL_DISABLE);
  if (h_GL <> nil) then g_SetVSync(gVSync);
  if (gFullscreen) then
  begin
    nw := 0;
    nh := 0;
    SDL_GetWindowSize(h_Wnd, @nw, @nh);
    if (nw > 128) and (nh > 128) then
    begin
      e_WriteLog('SDL: fullscreen window got size '+IntToStr(nw)+'x'+IntToStr(nh)+': '+IntToStr(gScreenWidth)+'x'+IntToStr(gScreenHeight), TMsgType.Notify);
      gScreenWidth := nw;
      gScreenHeight := nh;
    end
    else
    begin
      e_WriteLog('SDL: fullscreen window got invalid size: '+IntToStr(nw)+'x'+IntToStr(nh), TMsgType.Notify);
    end;
  end;

  {$IFDEF ENABLE_HOLMES}
    fuiScrWdt := gScreenWidth;
    fuiScrHgt := gScreenHeight;
    if (h_GL <> nil) and (not preserveGL) then begin if (assigned(oglInitCB)) then oglInitCB(); end;
  {$ENDIF}
{$ENDIF}

  result := true;
end;


function GetDisplayModes (dbpp: LongWord; var selres: LongWord): SSArray;
var
  mode: TSDL_DisplayMode;
  res, i, k, n, pw, ph: Integer;
begin
  SetLength(result, 0);
  {$IFDEF HEADLESS}exit;{$ENDIF}
  k := 0; selres := 0;
  n := SDL_GetNumDisplayModes(0);
  pw := 0; ph := 0;
  for i := 0 to n do
  begin
    res := SDL_GetDisplayMode(0, i, @mode);
    if res < 0 then continue;
    if SDL_BITSPERPIXEL(mode.format) = gBPP then continue;
    if (mode.w = pw) and (mode.h = ph) then continue;
    if (mode.w = gScreenWidth) and (mode.h = gScreenHeight) then
      selres := k;
    Inc(k);
    SetLength(result, k);
    result[k-1] := IntToStr(mode.w) + 'x' + IntToStr(mode.h);
    pw := mode.w; ph := mode.h
  end;

  e_WriteLog('SDL: Got ' + IntToStr(k) + ' resolutions.', TMsgType.Notify);
end;


procedure Sleep (ms: LongWord);
begin
  SDL_Delay(ms);
end;


procedure ChangeWindowSize (requested: Boolean);
begin
  e_LogWritefln('  ChangeWindowSize: (ws=%dx%d) (ss=%dx%d)', [gWinSizeX, gWinSizeY, gScreenWidth, gScreenHeight]);
  gWinSizeX := gScreenWidth;
  gWinSizeY := gScreenHeight;
{$IF not DEFINED(HEADLESS)}
  {$IFDEF ENABLE_HOLMES}
    fuiScrWdt := gScreenWidth;
    fuiScrHgt := gScreenHeight;
  {$ENDIF}
  e_ResizeWindow(gScreenWidth, gScreenHeight);
  g_Game_SetupScreenSize();
  {$IF DEFINED(ANDROID)}
    (* This will fix menu reset on keyboard showing *)
    if requested then
      g_Menu_Reset;
  {$ELSE}
    g_Menu_Reset;
  {$ENDIF}
  g_Game_ClearLoading();
{$ENDIF}
end;


function g_Window_SetSize (w, h: Word; fullscreen: Boolean): Boolean;
{$IF not DEFINED(HEADLESS)}
var
  preserve: Boolean;
{$ENDIF}
begin
  result := false;
{$IF not DEFINED(HEADLESS)}
  preserve := false;

  if (gScreenWidth <> w) or (gScreenHeight <> h) then
  begin
    result := true;
    preserve := true;
    gScreenWidth := w;
    gScreenHeight := h;
  end;

  if (gFullscreen <> fullscreen) then
  begin
    result := true;
    preserve := true;
    gFullscreen := fullscreen;
    preserve := true;
  end;

  if result then
  begin
    g_Window_SetDisplay(preserve);
    ChangeWindowSize(true);
  end;
{$ENDIF}
end;


function WindowEventHandler (constref ev: TSDL_WindowEvent): Boolean;
var
  wActivate, wDeactivate: Boolean;
begin
  result := false;
  wActivate := false;
  wDeactivate := false;

  case ev.event of
    SDL_WINDOWEVENT_MOVED:
    begin
      if not (gFullscreen or gWinMaximized) then
      begin
        gWinRealPosX := ev.data1;
        gWinRealPosY := ev.data2;
      end;
    end;

    SDL_WINDOWEVENT_MINIMIZED:
    begin
      e_UnpressAllKeys();
      if not wMinimized then
      begin
        e_ResizeWindow(0, 0);
        wMinimized := true;
        if g_debug_WinMsgs then
        begin
          g_Console_Add('Now minimized');
          e_WriteLog('[DEBUG] WinMsgs: Now minimized', TMsgType.Notify);
        end;
        wDeactivate := true;
      end;
    end;

    SDL_WINDOWEVENT_RESIZED:
    begin
      e_LogWritefln('Resize: (os=%dx%d) (ns=%dx%d)', [gScreenWidth, gScreenHeight, Integer(ev.data1), Integer(ev.data2)]);
      {if (gFullscreen) then
      begin
        e_LogWriteln('  fullscreen fix applied.');
        if (gScreenWidth <> ev.data1) or (gScreenHeight <> ev.data2) then
        begin
          SDL_SetWindowSize(h_Wnd, gScreenWidth, gScreenHeight);
        end;
      end
      else}
      begin
        gScreenWidth := ev.data1;
        gScreenHeight := ev.data2;
      end;
      ChangeWindowSize(false);
      SwapBuffers();
      if g_debug_WinMsgs then
      begin
        g_Console_Add('Resized to ' + IntToStr(ev.data1) + 'x' + IntToStr(ev.data2));
        e_WriteLog('[DEBUG] WinMsgs: Resized to ' + IntToStr(ev.data1) + 'x' + IntToStr(ev.data2), TMsgType.Notify);
      end;
    end;

    SDL_WINDOWEVENT_EXPOSED:
      SwapBuffers();

    SDL_WINDOWEVENT_MAXIMIZED:
    begin
      wMaximized := true;
      if wMinimized then
      begin
        e_ResizeWindow(gScreenWidth, gScreenHeight);
        wMinimized := false;
        wActivate := true;
      end;
      if (not gWinMaximized) and (not gFullscreen) then
      begin
        gWinMaximized := true;
        if g_debug_WinMsgs then
        begin
          g_Console_Add('Now maximized');
          e_WriteLog('[DEBUG] WinMsgs: Now maximized', TMsgType.Notify);
        end;
      end;
    end;

    SDL_WINDOWEVENT_RESTORED:
    begin
      wMaximized := false;
      if wMinimized then
      begin
        e_ResizeWindow(gScreenWidth, gScreenHeight);
        wMinimized := false;
        wActivate := true;
      end;
      gWinMaximized := false;
      if g_debug_WinMsgs then
      begin
        g_Console_Add('Now restored');
        e_WriteLog('[DEBUG] WinMsgs: Now restored', TMsgType.Notify);
      end;
    end;

    SDL_WINDOWEVENT_FOCUS_GAINED:
    begin
      wActivate := true;
      //e_WriteLog('window gained focus!', MSG_NOTIFY);
    end;

    SDL_WINDOWEVENT_FOCUS_LOST:
    begin
      wDeactivate := true;
      e_UnpressAllKeys();
      //e_WriteLog('window lost focus!', MSG_NOTIFY);
    end;
  end;

  if wDeactivate then
  begin
    if gWinActive then
    begin
      e_WriteLog('deactivating window', TMsgType.Notify);
      e_UnpressAllKeys;

      if gMuteWhenInactive then
      begin
        //e_WriteLog('deactivating sounds', MSG_NOTIFY);
        e_MuteChannels(true);
      end;

      if g_debug_WinMsgs then
      begin
        g_Console_Add('Now inactive');
        e_WriteLog('[DEBUG] WinMsgs: Now inactive', TMsgType.Notify);
      end;

      gWinActive := false;

      {$IFDEF ENABLE_HOLMES}
        if assigned(winBlurCB) then winBlurCB();
      {$ENDIF}
    end;
  end
  else if wActivate then
  begin
    if not gWinActive then
    begin
      //e_WriteLog('activating window', MSG_NOTIFY);

      if gMuteWhenInactive then
      begin
        //e_WriteLog('activating sounds', MSG_NOTIFY);
        e_MuteChannels(false);
      end;

      if g_debug_WinMsgs then
      begin
        g_Console_Add('Now active');
        e_WriteLog('[DEBUG] WinMsgs: Now active', TMsgType.Notify);
      end;

      gWinActive := true;

      {$IFDEF ENABLE_HOLMES}
        if assigned(winFocusCB) then winFocusCB();
      {$ENDIF}
    end;
  end;
end;


function EventHandler (var ev: TSDL_Event): Boolean;
var
  key, keychr, minuskey: Word;
  uc: UnicodeChar;
  down: Boolean;
  i: Integer;
  hat: array [HAT_LEFT..HAT_DOWN] of Boolean;
  joy: PSDL_Joystick;
begin
  result := false;

  case ev.type_ of
    SDL_WINDOWEVENT:
      result := WindowEventHandler(ev.window);

    SDL_QUITEV:
      begin
        if (gExit <> EXIT_QUIT) then
        begin
          if not wLoadingProgress then
          begin
            g_Game_Free();
            g_Game_Quit();
          end
          else
          begin
            wLoadingQuit := true;
          end;
        end;
        result := true;
      end;

    SDL_KEYDOWN, SDL_KEYUP:
      begin
        key := ev.key.keysym.scancode;
        if key = SDL_SCANCODE_AC_BACK then
          key := SDL_SCANCODE_ESCAPE;
        down := (ev.type_ = SDL_KEYDOWN);
        {$IF not DEFINED(HEADLESS) and DEFINED(ENABLE_HOLMES)}
        if fuiOnSDLEvent(ev) then
        begin
          // event eaten, but...
          if not down then e_KeyUpDown(key, false);
          exit;
        end;
        {$ENDIF}
        if ev.key._repeat = 0 then
        begin
          if g_dbg_input then
            e_LogWritefln('Input Debug: keysym, press=%s, scancode=%s', [down, key]);
          e_KeyUpDown(key, down);
          g_Console_ProcessBind(key, down)
        end;
        if down then KeyPress(key);
      end;

    SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP:
      if (ev.jbutton.which < e_MaxJoys) and (ev.jbutton.button < e_MaxJoyBtns) then
      begin
        key := e_JoyButtonToKey(ev.jbutton.which, ev.jbutton.button);
        down := ev.type_ = SDL_JOYBUTTONDOWN;
        if g_dbg_input then
          e_LogWritefln('Input Debug: jbutton, joy=%s, button=%s, keycode=%s, press=%s', [ev.jbutton.which, ev.jbutton.button, key, down]);
        e_KeyUpDown(key, down);
        g_Console_ProcessBind(key, down);
        if down then KeyPress(key)
      end
      else
      begin
        if g_dbg_input then
        begin
          down := ev.type_ = SDL_JOYBUTTONDOWN;
          e_LogWritefln('Input Debug: NOT IN RANGE! jbutton, joy=%s, button=%s, press=%s', [ev.jbutton.which, ev.jbutton.button, down])
        end
      end;

    SDL_JOYAXISMOTION:
      if (ev.jaxis.which < e_MaxJoys) and (ev.jaxis.axis < e_MaxJoyAxes) then
      begin
        key := e_JoyAxisToKey(ev.jaxis.which, ev.jaxis.axis, AX_PLUS);
        minuskey := e_JoyAxisToKey(ev.jaxis.which, ev.jaxis.axis, AX_MINUS);

        if g_dbg_input then
          e_LogWritefln('Input Debug: jaxis, joy=%s, axis=%s, value=%s, zeroaxes=%s, deadzone=%s', [ev.jaxis.which, ev.jaxis.axis, ev.jaxis.value, JoystickZeroAxes[ev.jaxis.which, ev.jaxis.axis], e_JoystickDeadzones[ev.jaxis.which]]);

        if ev.jaxis.value < JoystickZeroAxes[ev.jaxis.which, ev.jaxis.axis] - e_JoystickDeadzones[ev.jaxis.which] then
        begin
          if (e_KeyPressed(key)) then
          begin
            e_KeyUpDown(key, False);
            g_Console_ProcessBind(key, False);
          end;
          e_KeyUpDown(minuskey, True);
          g_Console_ProcessBind(minuskey, True);
          KeyPress(minuskey);
        end
        else if ev.jaxis.value > JoystickZeroAxes[ev.jaxis.which, ev.jaxis.axis] + e_JoystickDeadzones[ev.jaxis.which] then
        begin
          if (e_KeyPressed(minuskey)) then
          begin
            e_KeyUpDown(minuskey, False);
            g_Console_ProcessBind(minuskey, False);
          end;
          e_KeyUpDown(key, True);
          g_Console_ProcessBind(key, True);
          KeyPress(key);
        end
        else
        begin
          if (e_KeyPressed(minuskey)) then
          begin
            e_KeyUpDown(minuskey, False);
            g_Console_ProcessBind(minuskey, False);
          end;
          if (e_KeyPressed(key)) then
          begin
            e_KeyUpDown(key, False);
            g_Console_ProcessBind(key, False);
          end;
        end;
      end
      else
      begin
        if g_dbg_input then
          e_LogWritefln('Input Debug: NOT IN RANGE! jaxis, joy=%s, axis=%s, value=%s, zeroaxes=%s, deadzone=%s', [ev.jaxis.which, ev.jaxis.axis, ev.jaxis.value, JoystickZeroAxes[ev.jaxis.which, ev.jaxis.axis], e_JoystickDeadzones[ev.jaxis.which]])
      end;

    SDL_JOYHATMOTION:
      if (ev.jhat.which < e_MaxJoys) and (ev.jhat.hat < e_MaxJoyHats) then
      begin
        if g_dbg_input then
          e_LogWritefln('Input Debug: jhat, joy=%s, hat=%s, value=%s', [ev.jhat.which, ev.jhat.hat, ev.jhat.value]);
        hat[HAT_UP] := LongBool(ev.jhat.value and SDL_HAT_UP);
        hat[HAT_DOWN] := LongBool(ev.jhat.value and SDL_HAT_DOWN);
        hat[HAT_LEFT] := LongBool(ev.jhat.value and SDL_HAT_LEFT);
        hat[HAT_RIGHT] := LongBool(ev.jhat.value and SDL_HAT_RIGHT);
        for i := HAT_LEFT to HAT_DOWN do
        begin
          if JoystickHatState[ev.jhat.which, ev.jhat.hat, i] <> hat[i] then
          begin
            down := hat[i];
            key := e_JoyHatToKey(ev.jhat.which, ev.jhat.hat, i);
            e_KeyUpDown(key, down);
            g_Console_ProcessBind(key, down);
            if down then KeyPress(key)
          end
        end;
        JoystickHatState[ev.jhat.which, ev.jhat.hat] := hat
      end
      else
      begin
        if g_dbg_input then
          e_LogWritefln('Input Debug: NOT IN RANGE! jhat, joy=%s, hat=%s, value=%s', [ev.jhat.which, ev.jhat.hat, ev.jhat.value])
      end;

    SDL_JOYDEVICEADDED:
      if (ev.jdevice.which < e_MaxJoys) then
      begin
        joy := SDL_JoystickOpen(ev.jdevice.which);
        ASSERT(joy <> nil);
        e_LogWritefln('Added Joystick %s', [ev.jdevice.which]);
        e_JoystickAvailable[ev.jdevice.which] := True;
        for i := 0 to Min(SDL_JoystickNumAxes(joy), e_MaxJoyAxes) do
          JoystickZeroAxes[ev.jdevice.which, i] := SDL_JoystickGetAxis(joy, i);
        SDL_JoystickClose(joy)
      end
      else
      begin
        e_LogWritefln('Warning! Added Joystick %s, but we support only <= %s', [ev.jdevice.which, e_MaxJoys])
      end;

    SDL_JOYDEVICEREMOVED:
      begin
        e_LogWritefln('Removed Joystick %s', [ev.jdevice.which]);
        if (ev.jdevice.which < e_MaxJoys) then
          e_JoystickAvailable[ev.jdevice.which] := False
      end;

    {$IF not DEFINED(HEADLESS) and DEFINED(ENABLE_HOLMES)}
    SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP, SDL_MOUSEWHEEL, SDL_MOUSEMOTION:
      fuiOnSDLEvent(ev);
    {$ENDIF}

    SDL_TEXTINPUT:
      begin
        if g_dbg_input then
          e_LogWritefln('Input Debug: text, text=%s', [ev.text.text]);
        Utf8ToUnicode(@uc, PChar(ev.text.text), 1);
        keychr := Word(uc);
        if (keychr > 127) then keychr := Word(wchar2win(WideChar(keychr)));
        if (keychr > 0) and (keychr <= 255) then CharPress(AnsiChar(keychr));
      end;

    SDL_FINGERMOTION, SDL_FINGERDOWN, SDL_FINGERUP:
      g_Touch_HandleEvent(ev.tfinger);

    // other key presses and joysticks are handled in e_input
  end;
end;


procedure SwapBuffers ();
begin
  {$IF not DEFINED(HEADLESS)}
  SDL_GL_SwapWindow(h_Wnd);
  {$ENDIF}
end;


function CreateGLWindow (Title: PChar): Boolean;
begin
  result := false;

  gWinSizeX := gScreenWidth;
  gWinSizeY := gScreenHeight;

{$IF not DEFINED(HEADLESS)}
  wTitle := Title;
{$ENDIF}
  e_WriteLog('Creating window', TMsgType.Notify);

  if not g_Window_SetDisplay() then
  begin
    KillGLWindow(false);
    e_WriteLog('Window creation error (resolution not supported?)', TMsgType.Fatal);
    exit;
  end;

{$IF not DEFINED(HEADLESS)}
  h_GL := SDL_GL_CreateContext(h_Wnd);
  if (h_GL = nil) then exit;
  {$IFDEF ENABLE_HOLMES}
    fuiScrWdt := gScreenWidth;
    fuiScrHgt := gScreenHeight;
  {$ENDIF}
  SDL_GL_MakeCurrent(h_Wnd, h_GL);
{$IFDEF USE_NANOGL}
  if nanoGL_Init() = 0 then
  begin
    KillGLWindow(false);
    e_WriteLog('nanoGL initialization error', TMsgType.Fatal);
    exit;
  end;
{$ENDIF}
{$IFDEF USE_NOGL}
  nogl_Init;
{$ENDIF}
  {$IFDEF ENABLE_HOLMES}
    if (assigned(oglInitCB)) then oglInitCB();
  {$ENDIF}
  if (h_GL <> nil) then g_SetVSync(gVSync);
{$ENDIF}

  e_ResizeWindow(gScreenWidth, gScreenHeight);
  e_InitGL();

  result := true;
end;


{$IFDEF WINDOWS}
// windoze sux; in headless mode `GetTickCount()` (and SDL) returns shit
function GetTimer (): Int64;
var
  F, C: Int64;
begin
  QueryPerformanceFrequency(F);
  QueryPerformanceCounter(C);
  result := Round(C/F*1000{000});
end;
{$ELSE}
function GetTimer (): Int64;
var
  t: Uint32;
  tt: Int64;
begin
  t := SDL_GetTicks();
  if (ticksOverflow = -1) then
  begin
    ticksOverflow := 0;
    lastTicks := t;
  end
  else
  begin
    if (lastTicks > t) then
    begin
      // overflow, increment overflow ;-)
      ticksOverflow := ticksOverflow+(Int64($ffffffff)+Int64(1));
      tt := (Int64($ffffffff)+Int64(1))+Int64(t);
      t := Uint32(tt-lastTicks);
    end;
  end;
  lastTicks := t;
  result := ticksOverflow+Int64(t);
end;
{$ENDIF}


procedure ResetTimer ();
begin
  wNeedTimeReset := true;
end;


procedure PushExitEvent ();
var
  ev: TSDL_Event;
begin
  ev.type_ := SDL_QUITEV;
  SDL_PushEvent(@ev);
end;


var
  prevLoadingUpdateTime: UInt64 = 0;

procedure ProcessLoading (forceUpdate: Boolean=false);
var
  ev: TSDL_Event;
  ID: LongWord;
  stt: UInt64;
begin
  FillChar(ev, sizeof(ev), 0);
  wLoadingProgress := true;

  while (SDL_PollEvent(@ev) > 0) do
  begin
    EventHandler(ev);
    if (ev.type_ = SDL_QUITEV) then break;
  end;
  //e_PollJoysticks();

  if (ev.type_ = SDL_QUITEV) or (gExit = EXIT_QUIT) then
  begin
    wLoadingProgress := false;
    exit;
  end;

  if not wMinimized then
  begin
    if forceUpdate then
    begin
      prevLoadingUpdateTime := getTimeMilli();
    end
    else
    begin
      stt := getTimeMilli();
      if (stt < prevLoadingUpdateTime) or (stt-prevLoadingUpdateTime >= ProgressUpdateMSecs) then
      begin
        prevLoadingUpdateTime := stt;
        forceUpdate := true;
      end;
    end;

    if forceUpdate then
    begin
      if g_Texture_Get('INTER', ID) then
      begin
        e_DrawSize(ID, 0, 0, 0, false, false, gScreenWidth, gScreenHeight);
        e_DarkenQuadWH(0, 0, gScreenWidth, gScreenHeight, 150);
      end
      else
      begin
        e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);
      end;

      DrawLoadingStat();
      SwapBuffers();
    end;
  end;

  e_SoundUpdate();

  if NetMode = NET_SERVER then
  begin
    g_Net_Host_Update();
  end
  else
  begin
    if (NetMode = NET_CLIENT) and (NetState <> NET_STATE_AUTH) then g_Net_Client_UpdateWhileLoading();
  end;

  wLoadingProgress := false;
end;


function g_ProcessMessages (): Boolean;
var
  ev: TSDL_Event;
begin
  result := false;
  FillChar(ev, SizeOf(ev), 0);
  while (SDL_PollEvent(@ev) > 0) do
  begin
    result := EventHandler(ev);
    if (ev.type_ = SDL_QUITEV) then exit;
  end;
  //e_PollJoysticks();
end;


function ProcessMessage (): Boolean;
var
  i, t: Integer;
begin
  result := g_ProcessMessages();

  Time := GetTimer();
  Time_Delta := Time-Time_Old;

  flag := false;

  if wNeedTimeReset then
  begin
    Time_Delta := 28;
    wNeedTimeReset := false;
  end;

  g_Map_ProfilersBegin();
  g_Mons_ProfilersBegin();

  t := Time_Delta div 28;
  if (t > 0) then
  begin
    flag := true;
    for i := 1 to t do
    begin
           if (NetMode = NET_SERVER) then g_Net_Host_Update()
      else if (NetMode = NET_CLIENT) then g_Net_Client_Update();
      Update();
    end;
  end
  else
  begin
         if (NetMode = NET_SERVER) then g_Net_Host_Update()
    else if (NetMode = NET_CLIENT) then g_Net_Client_Update();
  end;

  g_Map_ProfilersEnd();
  g_Mons_ProfilersEnd();

  if wLoadingQuit then
  begin
    g_Game_Free();
    g_Game_Quit();
  end;

  if (gExit = EXIT_QUIT) then
  begin
    result := true;
    exit;
  end;

  // Время предыдущего обновления
  if flag then
  begin
    Time_Old := Time-(Time_Delta mod 28);
    if (not wMinimized) then
    begin
      Draw();
      SwapBuffers();
    end;
  end
  else
  begin
    Sleep(1); // release time slice, so we won't eat 100% CPU
  end;

  e_SoundUpdate();
end;


procedure ReDrawWindow ();
begin
  SwapBuffers();
end;


procedure g_SetVSync (vsync: Boolean);
{$IF not DEFINED(HEADLESS)}
var
  v: Byte;
{$ENDIF}
begin
{$IF not DEFINED(HEADLESS)}
  if vsync then v := 1 else v := 0;
  if (SDL_GL_SetSwapInterval(v) <> 0) then
  begin
    e_WriteLog('oops; can''t change vsync option, restart required', TMsgType.Warning);
  end
  else
  begin
    if vsync then e_WriteLog('VSync: ON', TMsgType.Notify) else e_WriteLog('VSync: OFF', TMsgType.Notify);
  end;
{$ENDIF}
end;


procedure InitOpenGL ();
begin
{$IF not DEFINED(HEADLESS)}
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
{$ENDIF}
end;


function glHasExtension (const name: AnsiString): Boolean;
var
  exts: PChar;
  i: Integer;
  found: Boolean;
  extName: ShortString;
begin
  result := false;
  if (Length(name) = 0) then exit;
  exts := glGetString(GL_EXTENSIONS);
  if (exts = nil) then exit;
  while (exts[0] <> #0) and (exts[0] = ' ') do Inc(exts);
  while (exts[0] <> #0) do
  begin
    if gwin_dump_extensions then
    begin
      i := 0;
      while (exts[i] <> #0) and (exts[i] <> ' ') do Inc(i);
      if i > 255 then
      begin
        e_WriteLog('FUUUUUUUUUUUUU', TMsgType.Warning);
      end
      else
      begin
        Move(exts^, extName[1], i);
        extName[0] := Char(i);
        e_WriteLog(Format('EXT: %s', [extName]), TMsgType.Notify);
      end;
    end;
    found := true;
    for i := 0 to length(name)-1 do
    begin
      if (exts[i] = #0) then begin found := false; break; end;
      if (exts[i] <> name[i+1]) then begin found := false; break; end;
    end;
    if found and ((exts[Length(name)] = #0) or (exts[Length(name)] = ' ')) then begin result := true; exit; end;
    while (exts[0] <> #0) and (exts[0] <> ' ') do Inc(exts);
    while (exts[0] <> #0) and (exts[0] = ' ') do Inc(exts);
  end;
end;


function SDLMain (): Integer;
var
  idx: Integer;
  {$IF not DEFINED(HEADLESS)}
  ltmp: Integer;
  {$ENDIF}
  arg: AnsiString;
  mdfo: TStream;
  {$IFDEF ENABLE_HOLMES}
  itmp: Integer;
  valres: Word;
  {$ENDIF}
begin
{$IFDEF HEADLESS}
  e_NoGraphics := true;
{$ELSE}
  {$IFDEF ENABLE_HOLMES}
    if (not g_holmes_imfunctional) then
    begin
      uiInitialize();
      uiContext.font := 'win14';
    end;
  {$ENDIF}
{$ENDIF}

  idx := 1;
  while (idx <= ParamCount) do
  begin
    arg := ParamStr(idx);
    Inc(idx);
    if arg = '--opengl-dump-exts' then gwin_dump_extensions := true;
    //if arg = '--twinkletwinkle' then gwin_k8_enable_light_experiments := true;
    if arg = '--jah' then g_profile_history_size := 100;
    if arg = '--no-particles' then gpart_dbg_enabled := false;
    if arg = '--no-los' then gmon_dbg_los_enabled := false;

    if arg = '--profile-render' then g_profile_frame_draw := true;
    if arg = '--profile-coldet' then g_profile_collision := true;
    if arg = '--profile-los' then g_profile_los := true;

    if arg = '--no-part-phys' then gpart_dbg_phys_enabled := false;
    if arg = '--no-part-physics' then gpart_dbg_phys_enabled := false;
    if arg = '--no-particles-phys' then gpart_dbg_phys_enabled := false;
    if arg = '--no-particles-physics' then gpart_dbg_phys_enabled := false;
    if arg = '--no-particle-phys' then gpart_dbg_phys_enabled := false;
    if arg = '--no-particle-physics' then gpart_dbg_phys_enabled := false;

    if arg = '--debug-input' then g_dbg_input := True;

    {.$IF DEFINED(D2F_DEBUG)}
    if arg = '--aimline' then g_dbg_aimline_on := true;
    {.$ENDIF}

{$IFDEF ENABLE_HOLMES}
    if arg = '--holmes' then begin g_holmes_enabled := true; g_Game_SetDebugMode(); end;

    if (arg = '--holmes-ui-scale') or (arg = '-holmes-ui-scale') then
    begin
      if (idx <= ParamCount) then
      begin
        if not conParseFloat(fuiRenderScale, ParamStr(idx)) then fuiRenderScale := 1.0;
        Inc(idx);
      end;
    end;

    if (arg = '--holmes-font') or (arg = '-holmes-font') then
    begin
      if (idx <= ParamCount) then
      begin
        itmp := 0;
        val(ParamStr(idx), itmp, valres);
        {$IFNDEF HEADLESS}
        if (valres = 0) and (not g_holmes_imfunctional) then
        begin
          case itmp of
            8: uiContext.font := 'win8';
            14: uiContext.font := 'win14';
            16: uiContext.font := 'win16';
          end;
        end;
        {$ELSE}
        // fuck off, fpc!
        itmp := itmp;
        valres := valres;
        {$ENDIF}
        Inc(idx);
      end;
    end;
{$ENDIF}

    if (arg = '--game-scale') or (arg = '-game-scale') then
    begin
      if (idx <= ParamCount) then
      begin
        if not conParseFloat(g_dbg_scale, ParamStr(idx)) then g_dbg_scale := 1.0;
        Inc(idx);
      end;
    end;

    if (arg = '--write-mapdef') or (arg = '-write-mapdef') then
    begin
      mdfo := createDiskFile('mapdef.txt');
      mdfo.WriteBuffer(defaultMapDef[1], Length(defaultMapDef));
      mdfo.Free();
      Halt(0);
    end;
  end;

  e_WriteLog('Initializing OpenGL', TMsgType.Notify);
  InitOpenGL();

  e_WriteLog('Creating GL window', TMsgType.Notify);
  if not CreateGLWindow(PChar(Format('Doom 2D: Forever %s', [GAME_VERSION]))) then
  begin
    result := 0;
    e_WriteLog('Unable to create GL window: ' + SDL_GetError(), TMsgType.Fatal);
    exit;
  end;

  {EnumDisplayModes();}

{$IFDEF HEADLESS}
  //gwin_k8_enable_light_experiments := false;
  gwin_has_stencil := false;
  glLegacyNPOT := false;
  gwin_dump_extensions := false;
{$ELSE}
  SDL_GL_GetAttribute(SDL_GL_STENCIL_SIZE, @ltmp);
  e_LogWritefln('stencil buffer size: %s', [ltmp]);
  gwin_has_stencil := (ltmp > 0);

  if glHasExtension('GL_ARB_texture_non_power_of_two') or
     glHasExtension('GL_OES_texture_npot') then
  begin
    e_WriteLog('NPOT textures: YES', TMsgType.Notify);
    glLegacyNPOT := false;
  end
  else
  begin
    e_WriteLog('NPOT textures: NO', TMsgType.Warning);
    glLegacyNPOT := true;
  end;
  gwin_dump_extensions := false;
{$ENDIF}

  Init();
  Time_Old := GetTimer();

  // Командная строка
  if (ParamCount > 0) then g_Game_Process_Params();

  // Запрос языка
  if (not gGameOn) and gAskLanguage then g_Menu_AskLanguage();

  e_WriteLog('Entering the main loop', TMsgType.Notify);

  // main loop
  while not ProcessMessage() do begin end;

  Release();
  KillGLWindow(false);

  result := 0;
end;


initialization
  conRegVar('d_input', @g_dbg_input, '', '')
end.
