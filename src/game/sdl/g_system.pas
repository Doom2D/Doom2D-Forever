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

  (* To fix:
   *   - Text input
   *   - Joystick support
   *   - Window resizing using SDL_VIDEORESIZE
   *)

  uses Utils;

  (* --- Utils --- *)
  function sys_GetTicks (): Int64;
  procedure sys_Delay (ms: Integer);

  (* --- Graphics --- *)
  function sys_GetDispalyModes (bpp: Integer): SSArray;
  function sys_SetDisplayMode (w, h, bpp: Integer; fullscreen: Boolean): Boolean;
  procedure sys_EnableVSync (yes: Boolean);
  procedure sys_Repaint;

  (* --- Input --- *)
  function sys_HandleInput (): Boolean;
  procedure sys_RequestQuit;

  (* --- Init --- *)
  procedure sys_Init;
  procedure sys_Final;

implementation

  uses
    SysUtils, SDL, GL,
    e_log, e_graphics, e_input,
    g_options, g_window, g_console, g_game, g_menu;

  var
    screen: PSDL_Surface;

  (* --------- Utils --------- *)

  function sys_GetTicks (): Int64;
  begin
    Result := SDL_GetTicks()
  end;

  procedure sys_Delay (ms: Integer);
  begin
    SDL_Delay(ms)
  end;

  (* --------- Graphics --------- *)

  procedure UpdateSize (w, h: Integer);
  begin
    gWinSizeX := w;
    gWinSizeY := h;
    gWinRealPosX := 0;
    gWinRealPosY := 0;
    gScreenWidth := w;
    gScreenHeight := h;
    {$IFDEF ENABLE_HOLMES}
      fuiScrWdt := w;
      fuiScrHgt := h;
    {$ENDIF}
    e_ResizeWindow(w, h);
    e_InitGL;
    g_Game_SetupScreenSize;
    g_Menu_Reset;
    g_Game_ClearLoading;
  end;

  function InitWindow (w, h, bpp: Integer; fullScreen: Boolean): Boolean;
    var flags: Uint32;
  begin
    e_LogWritefln('InitWindow %s %s %s %s', [w, h, bpp, fullscreen]);
    Result := False;
    SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8); // lights; it is enough to have 1-bit stencil buffer for lighting, but...
    flags := SDL_OPENGL or SDL_VIDEORESIZE;
    if fullScreen then
      flags := flags or SDL_FULLSCREEN;
    if (screen = nil) or (SDL_VideoModeOk(w, h, bpp, flags) <> 0) then
    begin
      SDL_FreeSurface(screen);
      screen := SDL_SetVideoMode(w, h, bpp, flags);
      if screen <> nil then
      begin
        SDL_WM_SetCaption('Doom 2D: Forever (SDL 1.2)', nil);
        UpdateSize(w, h);
        Result := True
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

  function sys_GetDispalyModes (bpp: Integer): SSArray;
    var m: PPSDL_Rect; f: TSDL_PixelFormat; i, count: Integer;
  begin
    SetLength(result, 0);
    FillChar(f, sizeof(f), 0);
    f.palette := nil;
    f.BitsPerPixel := bpp;
    f.BytesPerPixel := (bpp + 7) div 8;
    m := SDL_ListModes(@f, SDL_OPENGL or SDL_FULLSCREEN);
    if (m <> NIL) and (IntPtr(m) <> -1) then
    begin
      count := 0;
      while m[count] <> nil do inc(count);
      SetLength(result, count);
      for i := 0 to count - 1 do
        result[i] := IntToStr(m[i].w) + 'x' + IntToStr(m[i].h);
    end
  end;

  function sys_SetDisplayMode (w, h, bpp: Integer; fullscreen: Boolean): Boolean;
  begin
    result := InitWindow(w, h, bpp, fullscreen)
  end;

  (* --------- Input --------- *)

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
    var down: Boolean; key, stb: Integer;
  begin
    down := (ev.type_ = SDL_KEYDOWN);
    key := ev.keysym.sym;
    stb := Key2Stub(key);
    if g_dbg_input then
      e_LogWritefln('Input Debug: keysym, press=%s, scancode=%s SDL.ev.key.state=%s', [down, key, ev.state]);
    e_KeyUpDown(stb, down);
    g_Console_ProcessBind(stb, down);
    (* !!! need text input -- console, cheaats, fields *)
  end;

  function sys_HandleInput (): Boolean;
    var ev: TSDL_Event;
  begin
    result := false;
    while SDL_PollEvent(@ev) <> 0 do
    begin
      case ev.type_ of
        SDL_QUITEV: result := true;
        SDL_VIDEORESIZE: InitWindow(ev.resize.w, ev.resize.h, gBPP, gFullscreen);
        SDL_KEYUP, SDL_KEYDOWN: HandleKeyboard(ev.key);
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
    var flags: Uint32; ok: Boolean;
  begin
    flags := SDL_INIT_VIDEO or SDL_INIT_AUDIO or
             SDL_INIT_TIMER or SDL_INIT_JOYSTICK
             (*or SDL_INIT_NOPARACHUTE*);
    if SDL_Init(flags) <> 0 then
      raise Exception.Create('SDL: Init failed: ' + SDL_GetError());
    ok := InitWindow(gScreenWidth, gScreenHeight, gBPP, gFullScreen);
    if not ok then
      raise Exception.Create('SDL: failed to set videomode: ' + SDL_GetError)
  end;

  procedure sys_Final;
  begin
    e_WriteLog('Releasing SDL', TMsgType.Notify);
    SDL_FreeSurface(screen);
    SDL_Quit
  end;

end.
