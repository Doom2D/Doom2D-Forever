unit g_window;

interface

uses
  WADEDITOR;

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

implementation

uses
  SDL, GL, GLExt, e_graphics, e_log, g_main,
  g_console, SysUtils, e_input, g_options, g_game,
  g_basic, g_textures, e_sound, g_sound, g_menu, ENet, g_net;

var
  h_Wnd: PSDL_Surface;
  wFlags: LongWord = 0;
  Time, Time_Delta, Time_Old: Int64;
  flag: Boolean;
  wNeedTimeReset: Boolean = False;
  wWindowCreated: Boolean = False;
  //wCursorShown: Boolean = False;
  wMinimized: Boolean = False;
  //wNeedFree: Boolean = True;
  wLoadingProgress: Boolean = False;
  wLoadingQuit: Boolean = False;
  {wWinPause: Byte = 0;}

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
begin
  for Result := 0 to 127 do
    if CP1251[Result] = wc then
      break;
  Result := Result + 128;
end;

function g_Window_SetDisplay(PreserveGL: Boolean = False): Boolean;
begin
  Result := False;

  e_WriteLog('Setting display mode...', MSG_NOTIFY);

  if wWindowCreated and PreserveGL then
    e_SaveGLContext(); // we need this and restore because of a bug in SDL1.2, apparently

  wFlags := SDL_RESIZABLE or SDL_OPENGL;
  if gFullscreen then wFlags := wFlags or SDL_FULLSCREEN;

  h_Wnd := SDL_SetVideoMode(gScreenWidth, gScreenHeight, gBPP, wFlags);
  SDL_EnableUNICODE(SDL_ENABLE);
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);
  SDL_ShowCursor(SDL_DISABLE);

  if wWindowCreated and PreserveGL then
    e_RestoreGLContext();

  Result := h_Wnd <> nil;
end;

procedure ReShowCursor();
begin
  // TODO: what was this for?
end;

function GetDisplayModes(dBPP: DWORD; var SelRes: DWORD): SArray;
var
  modesp: PPSDL_Rect;
  tmpp: PSDL_Rect;
  tmpr: SDL_Rect;
  i: Integer;
begin
  SetLength(Result, 0);
  modesp := SDL_ListModes(nil, SDL_FULLSCREEN or SDL_HWSURFACE);
  if modesp = nil then exit;
  if Pointer(-1) = modesp then exit;

  tmpp := modesp^;
  i := 0;
  while tmpp <> nil do
  begin
    tmpr := tmpp^;
    if (tmpr.w = gScreenWidth) and (tmpr.h = gScreenHeight) then
      SelRes := i;
    SetLength(Result, Length(Result) + 1);
    Result[i] := IntToStr(tmpr.w) + 'x' + IntToStr(tmpr.h);

    modesp := Pointer(Cardinal(modesp) + SizeOf(PSDL_Rect));
    tmpp := modesp^;
    Inc(i);
  end;

  e_WriteLog('SDL: Got ' + IntToStr(Length(Result)) + ' resolutions.', MSG_NOTIFY);
end;

procedure Sleep(ms: LongWord);
begin
  SDL_Delay(ms);
end;

procedure ChangeWindowSize();
begin
  gWinSizeX := gScreenWidth;
  gWinSizeY := gScreenHeight;
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

function EventHandler(ev: TSDL_Event): Boolean;
var
  key, keychr: Word;
  //joy: Integer;
begin
  Result := False;
  case ev.type_ of
    SDL_VIDEORESIZE:
    begin
      g_Window_SetSize(ev.resize.w, ev.resize.h, gFullscreen);
      e_Clear();
    end;

    SDL_ACTIVEEVENT:
    begin
      if (ev.active.gain = 0) then
      begin
        if g_debug_WinMsgs then
        begin
          g_Console_Add('Inactive');
          e_WriteLog('[DEBUG] WinMsgs: Inactive', MSG_NOTIFY);
        end;

        if LongBool(ev.active.state and SDL_APPINPUTFOCUS) and gWinActive then
        begin
          e_EnableInput := False;
          e_ClearInputBuffer();

          if gMuteWhenInactive then
            e_MuteChannels(True);

          if g_debug_WinMsgs then
          begin
            g_Console_Add('Inactive indeed');
            e_WriteLog('[DEBUG] WinMsgs: Inactive indeed', MSG_NOTIFY);
          end;

          gWinActive := False;
        end;

        if LongBool(ev.active.state and SDL_APPACTIVE) and (not wMinimized) then
        begin
          e_ResizeWindow(0, 0);
          wMinimized := True;

          if g_debug_WinMsgs then
          begin
            g_Console_Add('Minimized indeed');
            e_WriteLog('[DEBUG] WinMsgs: Minimized indeed', MSG_NOTIFY);
          end;
        end;
      end
      else
      begin
        if g_debug_WinMsgs then
        begin
          g_Console_Add('Active');
          e_WriteLog('[DEBUG] WinMsgs: Active', MSG_NOTIFY);
        end;

        // Если окно было неактивным:
        if LongBool(ev.active.state and SDL_APPINPUTFOCUS) and (not gWinActive) then
        begin
          e_EnableInput := True;

          if gMuteWhenInactive then
            e_MuteChannels(False);

          if g_debug_WinMsgs then
          begin
            g_Console_Add('Active indeed');
            e_WriteLog('[DEBUG] WinMsgs: Active indeed', MSG_NOTIFY);
          end;

          gWinActive := True;
        end;

        if LongBool(ev.active.state and SDL_APPACTIVE) and wMinimized then
        begin
          e_ResizeWindow(gScreenWidth, gScreenHeight);

          wMinimized := False;

          if g_debug_WinMsgs then
          begin
            g_Console_Add('Restored indeed');
            e_WriteLog('[DEBUG] WinMsgs: Restored indeed', MSG_NOTIFY);
          end;
        end;
      end;
    end;

    SDL_VIDEOEXPOSE:
    begin
      // TODO: the fuck is this event?
      // Draw();
    end;

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
      key := ev.key.keysym.sym;
      keychr := ev.key.keysym.unicode;
      KeyPress(key);
      if (keychr > 7) and (key <> IK_BACKSPACE) then
      begin
        if (keychr >= 128) then
          keychr := WCharToCP1251(keychr);
        CharPress(Chr(keychr));
      end;
    end;

    // key presses and joysticks are handled in e_input
  end;
end;

procedure SwapBuffers();
begin
  SDL_GL_SwapBuffers();
end;

procedure KillGLWindow();
begin
  wWindowCreated := False;
end;

function CreateGLWindow(Title: PChar): Boolean;
//var
//  flags: LongWord;
begin
  Result := False;

  gWinSizeX := gScreenWidth;
  gWinSizeY := gScreenHeight;

  e_WriteLog('Creating window', MSG_NOTIFY);

  if not g_Window_SetDisplay() then
  begin
    KillGLWindow();
    e_WriteLog('Window creation error (resolution not supported?)', MSG_FATALERROR);
    exit;
  end;

  SDL_WM_SetCaption(Title, Title);
  wWindowCreated := True;

  e_ResizeWindow(gScreenWidth, gScreenHeight);
  e_InitGL();

  Result := True;
end;

function GetTimer(): Int64;
begin
  Result := SDL_GetTicks() * 1000; // TODO: do we really need microseconds here?
end;

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
    Time_Delta := 27777;
    wNeedTimeReset := False;
  end;

  t := Time_Delta div 27777;
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
    Time_Old := Time - (Time_Delta mod 27777);
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
  if VSync then v := 1 else v := 0;
  SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  SDL_GL_SetAttribute(SDL_GL_SWAP_CONTROL, v);
end;

function SDLMain(): Integer;
begin
  e_WriteLog('Creating GL window', MSG_NOTIFY);
  if not CreateGLWindow(PChar(Format('Doom 2D: Forever %s', [GAME_VERSION]))) then
  begin
    Result := 0;
    exit;
  end;

  e_WriteLog('Initializing OpenGL', MSG_NOTIFY);
  InitOpenGL(gVSync);

  {EnumDisplayModes();}

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
