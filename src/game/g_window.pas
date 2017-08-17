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
unit g_window;

interface

uses
  wadreader;

function  SDLMain(): Integer;
function  GetTimer(): Int64;
procedure ResetTimer();
function  CreateGLWindow(Title: PChar): Boolean;
procedure KillGLWindow();
procedure PushExitEvent();
function  ProcessMessage(): Boolean;
procedure ProcessLoading();
procedure ReDrawWindow();
procedure SwapBuffers();
procedure Sleep(ms: LongWord);
function  GetDisplayModes(dBPP: DWORD; var SelRes: DWORD): SArray;
function  g_Window_SetDisplay(PreserveGL: Boolean = False): Boolean;
function  g_Window_SetSize(W, H: Word; FScreen: Boolean): Boolean;

var
  gwin_dump_extensions: Boolean = false;
  gwin_has_stencil: Boolean = false;
  gwin_k8_enable_light_experiments: Boolean = false;

implementation

uses
{$IFDEF WINDOWS}Windows,{$ENDIF}
  SDL2, GL, GLExt, e_graphics, e_log, g_main,
  g_console, SysUtils, e_input, g_options, g_game,
  g_basic, g_textures, e_sound, g_sound, g_menu, ENet, g_net,
  g_map;

var
  h_Wnd: PSDL_Window;
  h_GL: TSDL_GLContext;
  wFlags: LongWord = 0;
  Time, Time_Delta, Time_Old: Int64;
  flag: Boolean;
  wTitle: PChar = nil;
  wNeedTimeReset: Boolean = False;
  //wWindowCreated: Boolean = False;
  //wCursorShown: Boolean = False;
  wMinimized: Boolean = False;
  //wNeedFree: Boolean = True;
  wLoadingProgress: Boolean = False;
  wLoadingQuit: Boolean = False;
  {wWinPause: Byte = 0;}
{$IFNDEF WINDOWS}
  ticksOverflow: Int64 = -1;
  lastTicks: Uint32 = 0; // to detect overflow
{$ENDIF}

const
  // TODO: move this to a separate file
  CP1251: array [0..127] of Word = (
    $0402,$0403,$201A,$0453,$201E,$2026,$2020,$2021,$20AC,$2030,$0409,$2039,$040A,$040C,$040B,$040F,
    $0452,$2018,$2019,$201C,$201D,$2022,$2013,$2014,$003F,$2122,$0459,$203A,$045A,$045C,$045B,$045F,
    $00A0,$040E,$045E,$0408,$00A4,$0490,$00A6,$00A7,$0401,$00A9,$0404,$00AB,$00AC,$00AD,$00AE,$0407,
    $00B0,$00B1,$0406,$0456,$0491,$00B5,$00B6,$00B7,$0451,$2116,$0454,$00BB,$0458,$0405,$0455,$0457,
    $0410,$0411,$0412,$0413,$0414,$0415,$0416,$0417,$0418,$0419,$041A,$041B,$041C,$041D,$041E,$041F,
    $0420,$0421,$0422,$0423,$0424,$0425,$0426,$0427,$0428,$0429,$042A,$042B,$042C,$042D,$042E,$042F,
    $0430,$0431,$0432,$0433,$0434,$0435,$0436,$0437,$0438,$0439,$043A,$043B,$043C,$043D,$043E,$043F,
    $0440,$0441,$0442,$0443,$0444,$0445,$0446,$0447,$0448,$0449,$044A,$044B,$044C,$044D,$044E,$044F
  );

// TODO: make a transition table or something
function WCharToCP1251(wc: Word): Word;
var
  n: Word;
begin
  Result := 0;
  for n := 0 to 127 do
    if CP1251[n] = wc then begin Result := n; break end;
  Result := Result + 128;
end;

function g_Window_SetDisplay(PreserveGL: Boolean = False): Boolean;
var
  mode, cmode: TSDL_DisplayMode;
begin
{$IFDEF HEADLESS}
  Result := True;
  Exit;
{$ENDIF}

  Result := False;

  e_WriteLog('Setting display mode...', MSG_NOTIFY);

  wFlags := SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE;
  if gFullscreen then wFlags := wFlags or SDL_WINDOW_FULLSCREEN;
  if gWinMaximized then wFlags := wFlags or SDL_WINDOW_MAXIMIZED;

  if h_Wnd <> nil then
  begin
    SDL_DestroyWindow(h_Wnd);
    h_Wnd := nil;
  end;

  if gFullscreen then
  begin
    mode.w := gScreenWidth;
    mode.h := gScreenHeight;
    mode.format := 0;
    mode.refresh_rate := 0;
    mode.driverdata := nil;
    if SDL_GetClosestDisplayMode(0, @mode, @cmode) = nil then
    begin
      gScreenWidth := 800;
      gScreenHeight := 600;
    end
    else
    begin
      gScreenWidth := cmode.w;
      gScreenHeight := cmode.h;
    end;
  end;

  h_Wnd := SDL_CreateWindow(PChar(wTitle), gWinRealPosX, gWinRealPosY, gScreenWidth, gScreenHeight, wFlags);
  if h_Wnd = nil then Exit;

  SDL_GL_MakeCurrent(h_Wnd, h_GL);
  SDL_ShowCursor(SDL_DISABLE);

  Result := True;
end;

procedure ReShowCursor();
begin
  // TODO: what was this for?
end;

function GetDisplayModes(dBPP: DWORD; var SelRes: DWORD): SArray;
var
  mode: TSDL_DisplayMode;
  res, i, k, n, pw, ph: Integer;
begin
  SetLength(Result, 0);
  {$IFDEF HEADLESS}Exit;{$ENDIF}
  k := 0; SelRes := 0;
  n := SDL_GetNumDisplayModes(0);
  pw := 0; ph := 0;
  for i := 0 to n do
  begin
    res := SDL_GetDisplayMode(0, i, @mode);
    if res < 0 then continue;
    if SDL_BITSPERPIXEL(mode.format) = gBPP then continue;
    if (mode.w = pw) and (mode.h = ph) then continue;
    if (mode.w = gScreenWidth) and (mode.h = gScreenHeight) then
      SelRes := k;
    Inc(k);
    SetLength(Result, k);
    Result[k-1] := IntToStr(mode.w) + 'x' + IntToStr(mode.h);
    pw := mode.w; ph := mode.h
  end;

  e_WriteLog('SDL: Got ' + IntToStr(k) + ' resolutions.', MSG_NOTIFY);
end;

procedure Sleep(ms: LongWord);
begin
  SDL_Delay(ms);
end;

procedure ChangeWindowSize();
begin
  gWinSizeX := gScreenWidth;
  gWinSizeY := gScreenHeight;
  {$IFDEF HEADLESS}Exit;{$ENDIF}
  e_ResizeWindow(gScreenWidth, gScreenHeight);
  g_Game_SetupScreenSize();
  g_Menu_Reset();
  g_Game_ClearLoading();
end;

function g_Window_SetSize(W, H: Word; FScreen: Boolean): Boolean;
var
  Preserve: Boolean;
begin
  Result := False;
  {$IFDEF HEADLESS}Exit;{$ENDIF}
  Preserve := False;

  if (gScreenWidth <> W) or (gScreenHeight <> H) then
  begin
    Result := True;
    gScreenWidth := W;
    gScreenHeight := H;
  end;

  if gFullscreen <> FScreen then
  begin
    Result := True;
    gFullscreen := FScreen;
    Preserve := True;
  end;

  if Result then
  begin
    g_Window_SetDisplay(Preserve);
    ChangeWindowSize();
  end;
end;

function WindowEventHandler(ev: TSDL_WindowEvent): Boolean;
var
  wActivate, wDeactivate: Boolean;
begin
  Result := False;
  wActivate := False;
  wDeactivate := False;

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
      if not wMinimized then
      begin
        e_ResizeWindow(0, 0);
        wMinimized := True;

        if g_debug_WinMsgs then
        begin
          g_Console_Add('Now minimized');
          e_WriteLog('[DEBUG] WinMsgs: Now minimized', MSG_NOTIFY);
        end;
        wDeactivate := True;
      end;
    end;

    SDL_WINDOWEVENT_RESIZED:
    begin
      gScreenWidth := ev.data1;
      gScreenHeight := ev.data2;
      ChangeWindowSize();
      SwapBuffers();
      if g_debug_WinMsgs then
      begin
        g_Console_Add('Resized to ' + IntToStr(ev.data1) + 'x' + IntToStr(ev.data2));
        e_WriteLog('[DEBUG] WinMsgs: Resized to ' + IntToStr(ev.data1) + 'x' + IntToStr(ev.data2), MSG_NOTIFY);
      end;
    end;

    SDL_WINDOWEVENT_EXPOSED:
      SwapBuffers();

    SDL_WINDOWEVENT_MAXIMIZED:
    begin
      if wMinimized then
      begin
        e_ResizeWindow(gScreenWidth, gScreenHeight);
        wMinimized := False;
        wActivate := True;
      end;
      if not gWinMaximized then
      begin
        gWinMaximized := True;
        if g_debug_WinMsgs then
        begin
          g_Console_Add('Now maximized');
          e_WriteLog('[DEBUG] WinMsgs: Now maximized', MSG_NOTIFY);
        end;
      end;
    end;

    SDL_WINDOWEVENT_RESTORED:
    begin
      if wMinimized then
      begin
        e_ResizeWindow(gScreenWidth, gScreenHeight);
        wMinimized := False;
        wActivate := True;
      end;
      if gWinMaximized then
        gWinMaximized := False;
      if g_debug_WinMsgs then
      begin
        g_Console_Add('Now restored');
        e_WriteLog('[DEBUG] WinMsgs: Now restored', MSG_NOTIFY);
      end;
    end;

    SDL_WINDOWEVENT_FOCUS_GAINED:
    begin
      wActivate := True;
      //e_WriteLog('window gained focus!', MSG_NOTIFY);
    end;

    SDL_WINDOWEVENT_FOCUS_LOST:
    begin
      wDeactivate := True;
      //e_WriteLog('window lost focus!', MSG_NOTIFY);
    end;
  end;

  if wDeactivate then
  begin
    if gWinActive then
    begin
      e_WriteLog('deactivating window', MSG_NOTIFY);
      e_EnableInput := False;
      e_ClearInputBuffer();

      if gMuteWhenInactive then
      begin
        //e_WriteLog('deactivating sounds', MSG_NOTIFY);
        e_MuteChannels(True);
      end;

      if g_debug_WinMsgs then
      begin
        g_Console_Add('Now inactive');
        e_WriteLog('[DEBUG] WinMsgs: Now inactive', MSG_NOTIFY);
      end;

      gWinActive := False;
    end;
  end
  else if wActivate then
  begin
    if not gWinActive then
    begin
      //e_WriteLog('activating window', MSG_NOTIFY);
      e_EnableInput := True;

      if gMuteWhenInactive then
      begin
        //e_WriteLog('activating sounds', MSG_NOTIFY);
        e_MuteChannels(False);
      end;

      if g_debug_WinMsgs then
      begin
        g_Console_Add('Now active');
        e_WriteLog('[DEBUG] WinMsgs: Now active', MSG_NOTIFY);
      end;

      gWinActive := True;
    end;
  end;
end;

function EventHandler(ev: TSDL_Event): Boolean;
var
  key, keychr: Word;
  uc: UnicodeChar;
  //joy: Integer;
begin
  Result := False;
  case ev.type_ of
    SDL_WINDOWEVENT:
      Result := WindowEventHandler(ev.window);

    SDL_QUITEV:
    begin
      if gExit <> EXIT_QUIT then
      begin
        if not wLoadingProgress then
        begin
          g_Game_Free();
          g_Game_Quit();
        end
        else
          wLoadingQuit := True;
      end;
      Result := True;
    end;

    SDL_KEYDOWN:
    begin
      key := ev.key.keysym.scancode;
      KeyPress(key);
    end;

    SDL_TEXTINPUT:
    begin
      Utf8ToUnicode(@uc, PChar(ev.text.text), 1);
      keychr := Word(uc);
      if (keychr > 127) then
        keychr := WCharToCP1251(keychr);
      CharPress(Chr(keychr));
    end;

    // other key presses and joysticks are handled in e_input
  end;
end;

procedure SwapBuffers();
begin
  {$IFDEF HEADLESS}Exit;{$ENDIF}
  SDL_GL_SwapWindow(h_Wnd);
end;

procedure KillGLWindow();
begin
  if h_Wnd <> nil then SDL_DestroyWindow(h_Wnd);
  if h_GL <> nil then SDL_GL_DeleteContext(h_GL);
  h_Wnd := nil;
  h_GL := nil;
  //wWindowCreated := False;
end;

function CreateGLWindow(Title: PChar): Boolean;
//var
//  flags: LongWord;
begin
  Result := False;

  gWinSizeX := gScreenWidth;
  gWinSizeY := gScreenHeight;

  wTitle := Title;
  e_WriteLog('Creating window', MSG_NOTIFY);

  if not g_Window_SetDisplay() then
  begin
    KillGLWindow();
    e_WriteLog('Window creation error (resolution not supported?)', MSG_FATALERROR);
    exit;
  end;

{$IFNDEF HEADLESS}
  h_Gl := SDL_GL_CreateContext(h_Wnd);
  if h_Gl = nil then Exit;
{$ENDIF}
  //wWindowCreated := True;

  e_ResizeWindow(gScreenWidth, gScreenHeight);
  e_InitGL();

  Result := True;
end;

{$IFDEF WINDOWS}
// windoze sux; in headless mode `GetTickCount()` (and SDL) returns shit
function GetTimer(): Int64;
var
  F, C: Int64;
begin
  QueryPerformanceFrequency(F);
  QueryPerformanceCounter(C);
  Result := Round(C/F*1000{000});
end;
{$ELSE}
function GetTimer(): Int64;
var
  t: Uint32;
  tt: Int64;
begin
  t := SDL_GetTicks() {* 1000}; // TODO: do we really need microseconds here? k8: NOPE!
  if ticksOverflow = -1 then
  begin
    ticksOverflow := 0;
    lastTicks := t;
  end
  else
  begin
    if lastTicks > t then
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

procedure ResetTimer();
begin
  wNeedTimeReset := True;
end;

procedure PushExitEvent();
var
  ev: TSDL_Event;
begin
  ev.type_ := SDL_QUITEV;
  SDL_PushEvent(@ev);
end;

procedure ProcessLoading();
var
  ev: TSDL_Event;
  ID: DWORD;
begin
  FillChar(ev, SizeOf(ev), 0);
  //wNeedFree := False;
  wLoadingProgress := True;
  while SDL_PollEvent(@ev) > 0 do
  begin
    if (ev.type_ = SDL_QUITEV) then
      break;
  end;
  //wNeedFree := True;

  if (ev.type_ = SDL_QUITEV) or (gExit = EXIT_QUIT) then
  begin
    wLoadingProgress := False;
    exit;
  end;

  if not wMinimized then
  begin
    if g_Texture_Get('INTER', ID) then
      e_DrawSize(ID, 0, 0, 0, False, False, gScreenWidth, gScreenHeight)
    else
      e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);

    DrawLoadingStat();
    SwapBuffers();

    ReShowCursor();
  end;

  e_SoundUpdate();

  if NetMode = NET_SERVER then
    g_Net_Host_Update
  else
    if (NetMode = NET_CLIENT) and (NetState <> NET_STATE_AUTH) then
      g_Net_Client_UpdateWhileLoading;
  wLoadingProgress := False;
end;

function ProcessMessage(): Boolean;
var
  i, t: Integer;
  ev: TSDL_Event;
begin
  Result := False;
  FillChar(ev, SizeOf(ev), 0);

  while SDL_PollEvent(@ev) > 0 do
  begin
    Result := EventHandler(ev);
    if ev.type_ = SDL_QUITEV then exit;
  end;

  Time := GetTimer();
  Time_Delta := Time - Time_Old;

  flag := False;

  if wNeedTimeReset then
  begin
    Time_Delta := 28{(27777 div 1000)};
    wNeedTimeReset := False;
  end;

  g_Map_ProfilersBegin();

  t := Time_Delta div 28{(27777 div 1000)};
  if t > 0 then
  begin
    flag := True;
    for i := 1 to t do
    begin
      if NetMode = NET_SERVER then g_Net_Host_Update()
      else if NetMode = NET_CLIENT then g_Net_Client_Update();
      Update();
    end;
  end
  else
  begin
    if NetMode = NET_SERVER then g_Net_Host_Update()
    else if NetMode = NET_CLIENT then g_Net_Client_Update();
  end;

  g_Map_ProfilersEnd();

  if wLoadingQuit then
  begin
    g_Game_Free();
    g_Game_Quit();
  end;

  if gExit = EXIT_QUIT then
  begin
    Result := True;
    Exit;
  end;

// Время предыдущего обновления:
  if flag then
  begin
    Time_Old := Time - (Time_Delta mod 28{(27777 div 1000)});
    if (not wMinimized) then
    begin
      Draw();
      SwapBuffers();
      ReShowCursor();
    end;
  end
  else
    Sleep(1);

  e_SoundUpdate();
end;

procedure ReDrawWindow;
begin
  SwapBuffers();
  ReShowCursor();
end;

procedure InitOpenGL(VSync: Boolean);
var
  v: Byte;
begin
  {$IFDEF HEADLESS}Exit;{$ENDIF}
  if VSync then v := 1 else v := 0;
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 1);
  SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  if gwin_k8_enable_light_experiments then
  begin
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 1); // lights; it is enough to have 1-bit stencil buffer for lighting
  end;
  SDL_GL_SetSwapInterval(v);
end;

function glHasExtension (name: AnsiString): Boolean;
var
  exts: PChar;
  i: Integer;
  found: Boolean;
  extName: ShortString;
begin
  result := false;
  if length(name) = 0 then exit;
  exts := glGetString(GL_EXTENSIONS);
  if exts = nil then exit;
  while (exts[0] <> #0) and (exts[0] = ' ') do Inc(exts);
  while exts[0] <> #0 do
  begin
    if gwin_dump_extensions then
    begin
      i := 0;
      while (exts[i] <> #0) and (exts[i] <> ' ') do Inc(i);
      if i > 255 then
      begin
        e_WriteLog('FUUUUUUUUUUUUU', MSG_WARNING);
      end
      else
      begin
        Move(exts^, extName[1], i);
        extName[0] := Char(i);
        e_WriteLog(Format('EXT: %s', [extName]), MSG_NOTIFY);
      end;
    end;
    found := true;
    for i := 0 to length(name)-1 do
    begin
      if exts[i] = #0 then begin found := false; break; end;
      if exts[i] <> name[i+1] then begin found := false; break; end;
    end;
    if found and ((exts[length(name)] = #0) or (exts[length(name)] = ' ')) then begin result := true; exit; end;
    while (exts[0] <> #0) and (exts[0] <> ' ') do Inc(exts);
    while (exts[0] <> #0) and (exts[0] = ' ') do Inc(exts);
  end;
end;

function SDLMain(): Integer;
var
  idx: Integer;
  ltmp: Integer;
begin
{$IFDEF HEADLESS}
  e_NoGraphics := True;
{$ENDIF}

  for idx := 1 to ParamCount do
  begin
    if ParamStr(idx) = '--opengl-dump-exts' then gwin_dump_extensions := true;
    if ParamStr(idx) = '--twinkletwinkle' then gwin_k8_enable_light_experiments := true;
    if ParamStr(idx) = '--jah' then g_profile_history_size := 100;
  end;

  e_WriteLog('Initializing OpenGL', MSG_NOTIFY);
  InitOpenGL(gVSync);

  e_WriteLog('Creating GL window', MSG_NOTIFY);
  if not CreateGLWindow(PChar(Format('Doom 2D: Forever %s', [GAME_VERSION]))) then
  begin
    Result := 0;
    exit;
  end;

  {EnumDisplayModes();}

  if gwin_k8_enable_light_experiments then
  begin
    SDL_GL_GetAttribute(SDL_GL_STENCIL_SIZE, @ltmp);
    e_WriteLog(Format('stencil buffer size: %d', [ltmp]), MSG_WARNING);
    gwin_has_stencil := (ltmp > 0);
  end
  else
  begin
    gwin_has_stencil := false;
  end;

  if not glHasExtension('GL_ARB_texture_non_power_of_two') then
  begin
    e_WriteLog('Driver DID''T advertised NPOT textures support', MSG_WARNING);
    glLegacyNPOT := true;
  end
  else
  begin
    e_WriteLog('Driver advertised NPOT textures support', MSG_NOTIFY);
    glLegacyNPOT := false;
  end;
  gwin_dump_extensions := false;

  Init();
  Time_Old := GetTimer();

// Командная строка:
  if ParamCount > 0 then
    g_Game_Process_Params();

// Запрос языка:
  if (not gGameOn) and gAskLanguage then
    g_Menu_AskLanguage();

  e_WriteLog('Entering the main loop', MSG_NOTIFY);

  while not ProcessMessage() do
    { Main Loop } ;

  Release();
  KillGLWindow();

  Result := 0;
end;

end.
