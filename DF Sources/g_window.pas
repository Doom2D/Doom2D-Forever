unit g_window;

interface

uses
  Windows;

function  WinMain(hInstance: HINST; hPrevInstance: HINST; lpCmdLine: PChar;
                  nCmdShow: Integer): Integer; StdCall;
function  GetTimer(): Int64;
procedure ResetTimer();
function  CreateGLWindow(Title: PChar): Bool; StdCall;
procedure KillGLWindow();
function  ProcessMessage: Boolean;
procedure ProcessLoading;
procedure PreventWindowFromLockUp;
procedure ReDrawWindow;
function  g_Window_SetDisplay(): Boolean;

var
  h_Wnd: HWND;

const
  gWindowStyle: Array [1..2, 1..2] of DWORD =
    ( ( WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or
        WS_MINIMIZEBOX or WS_MAXIMIZEBOX or
        WS_CLIPCHILDREN or WS_CLIPSIBLINGS, // Оконный Style
        WS_EX_APPWINDOW or WS_EX_WINDOWEDGE ), // Оконный ExStyle
      ( WS_POPUP or
        WS_CLIPCHILDREN or WS_CLIPSIBLINGS, // Полноэкранный Style
        WS_EX_APPWINDOW ) ); // Полноэкранный ExStyle

implementation

uses
  messages, dglOpenGL, e_graphics, e_log, g_main,
  g_console, SysUtils, e_input, g_options, g_game,
  g_basic, g_textures, e_sound, g_sound, g_menu, ENet, g_net;

var
  h_RC:  HGLRC;
  h_DC:  HDC;
  Time, Time_Delta, Time_Old: Int64;
  flag: Boolean;
  wNeedTimeReset: Boolean = False;
  wWindowCreated: Boolean = False;
  wCursorShown: Boolean = False;
  wMinimized: Boolean = False;
  wNeedFree: Boolean = True;
  {wWinPause: Byte = 0;}

const
  MAX_HANDLED_MESSAGES = 15;
  D2DF_CLASSNAME = 'D2DFRVR';

function g_Window_SetDisplay(): Boolean;
var
  dmScreenSettings: DevMode;
  MaxFreq: LongWord;
  n: Word;
begin
  Result := False;

  if gFreq <> 0 then
    MaxFreq := gFreq
  else
    begin
      MaxFreq := 0;
      n := 0;
      dmScreenSettings.dmSize := SizeOf(DevMode);

      while EnumDisplaySettings(nil, n, dmScreenSettings) do
      begin
        with dmScreenSettings do
          if (dmBitsPerPel = gBPP) and
             (dmPelsWidth = gScreenWidth) and
             (dmPelsHeight = gScreenHeight) then
            if dmDisplayFrequency > MaxFreq then
              MaxFreq := dmDisplayFrequency;
        n := n + 1;
        dmScreenSettings.dmSize := SizeOf(DevMode);
      end;
    end;

  ZeroMemory(@dmScreenSettings, SizeOf(dmScreenSettings));

  with dmScreenSettings do
  begin
    dmSize := SizeOf(dmScreenSettings);
    dmDisplayFrequency := MaxFreq;
    dmPelsWidth := gScreenWidth;
    dmPelsHeight := gScreenHeight;
    dmBitsPerPel := gBPP;
    dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT or DM_DISPLAYFREQUENCY;
  end;

  e_WriteLog('Changing display settings', MSG_NOTIFY);
  if ChangeDisplaySettings(dmScreenSettings, CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL then
  begin
    e_WriteLog(Format('Failed to change display settings (%dHz). Try to use 60Hz', [MaxFreq]), MSG_WARNING);
    dmScreenSettings.dmDisplayFrequency := 60;
    if ChangeDisplaySettings(dmScreenSettings, CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL then
    begin
      e_WriteLog('Failed to change display settings', MSG_FATALERROR);
      Exit;
    end;
  end;

  if g_debug_WinMsgs then
    with dmScreenSettings do
    begin
      g_Console_Add(Format('Display settings changed to: %d x %d, %d bpp',
                           [dmPelsWidth, dmPelsHeight, dmBitsPerPel]));
      e_WriteLog(Format('[DEBUG] WinMsgs: Display settings changed to: %d x %d, %d bpp',
                        [dmPelsWidth, dmPelsHeight, dmBitsPerPel]), MSG_NOTIFY);
    end;

  Result := True;
end;

{
procedure SetWinPause(Enable: Boolean);
begin
  if Enable then
    begin // Поставить паузу
      if wWinPause <> 0 then
        Exit;
        
    // Игра в процессе, была пауза от игрока или меню:
      if gGameOn and gPause then
        wWinPause := 2
      else
        begin // Либо игра не идет, либо паузы от игрока не было
          g_Game_Pause(True);
          wWinPause := 1;
        end;
    end
  else // Убрать паузу
    begin
      if wWinPause = 1 then
        begin // Не было паузы от игрока или меню
          g_Game_Pause(False);
          wWinPause := 0;
        end
      else
        if wWinPause = 2 then
          wWinPause := 0 // Была пауза от игрока или меню
        else
          Exit;
    end;
end;
}

procedure ReShowCursor();
var
  pt: TPoint;
  b: Boolean;
begin
  if gFullscreen then
    Exit;
    
  GetCursorPos(pt);

  if (pt.X < gWinPosX) or
     (pt.Y < gWinPosY) or
     (pt.X > (gWinPosX + gScreenWidth)) or
     (pt.Y > (gWinPosY + gScreenHeight)) then
    b := True
  else
    b := False;

  if b <> wCursorShown then
  begin
    wCursorShown := b;
    ShowCursor(b);
  end;
end;

procedure ChangeWindowSize(newWidth, newHeight: Word);
begin
  if (gScreenWidth <> newWidth) or (gScreenHeight <> newHeight) then
  begin
    gScreenWidth := newWidth;
    gScreenHeight := newHeight;

    e_ResizeWindow(gScreenWidth, gScreenHeight);
    g_Game_SetupScreenSize();
    g_Menu_Reset();        
    g_Game_ClearLoading();
  end;
end;

function WndProc(hWnd: HWND; message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if message = WM_SYSCOMMAND then
  begin
    case wParam of
      SC_SCREENSAVE, SC_MONITORPOWER, // Включен скринсэйвер
      SC_KEYMENU: // Alt, F10
        begin
          Result := 0;
          Exit;
        end;
    end;
  end;

  case message of
    WM_ACTIVATEAPP:
      if wWindowCreated then
        begin // Окно активировано или деактивировано
          if LOWORD(wParam) = WA_ACTIVE then
            begin // Окно стало активным
              if g_debug_WinMsgs then
              begin
                g_Console_Add('Active');
                e_WriteLog('[DEBUG] WinMsgs: Active', MSG_NOTIFY);
              end;

            // Если окно было неактивным:
              if not gWinActive then
              begin
                e_EnableInput := True;

                if gFullscreen then
                begin
                  ShowWindow(h_Wnd, SW_SHOWNORMAL);
                end;

                if gMuteWhenInactive then
                  e_MuteChannels(False);

                if g_debug_WinMsgs then
                begin
                  g_Console_Add('Active indeed');
                  e_WriteLog('[DEBUG] WinMsgs: Active indeed', MSG_NOTIFY);
                end;

                gWinActive := True;
              end;
            end
          else // Окно стало неактивным
            begin
              if g_debug_WinMsgs then
              begin
                g_Console_Add('Inactive');
                e_WriteLog('[DEBUG] WinMsgs: Inactive', MSG_NOTIFY);
              end;

            // Если окно было активным:
              if gWinActive then
              begin
                e_EnableInput := False;
                e_ClearInputBuffer();

                if gFullscreen then
                begin
                  ShowWindow(h_Wnd, SW_SHOWMINIMIZED);
                end;

                if gMuteWhenInactive then
                  e_MuteChannels(True);

                if g_debug_WinMsgs then
                begin
                  g_Console_Add('Inactive indeed');
                  e_WriteLog('[DEBUG] WinMsgs: Inactive indeed', MSG_NOTIFY);
                end;

                gWinActive := False;
              end;
            end;

          Result := 0;
        end
      else
        Result := DefWindowProc(hWnd, message, wParam, lParam);

    WM_CLOSE:
      begin // Окно закрыто
        if gExit <> EXIT_QUIT then
        begin
          g_Game_Free();
          g_Game_Quit();
        end;
        Result := 0;
      end;

    WM_SIZE:
      if wWindowCreated then
        begin
          case wParam of
            SIZE_MINIMIZED: // Свернуто
              begin
                if g_debug_WinMsgs then
                begin
                  g_Console_Add('Minimized');
                  e_WriteLog('[DEBUG] WinMsgs: Minimized', MSG_NOTIFY);
                end;

                if gFullscreen then
                begin
                  ChangeDisplaySettings(_devicemodeA(nil^), CDS_FULLSCREEN);
                end;

                e_ResizeWindow(0, 0);

                if not wMinimized then
                begin
                  e_EnableInput := False;
                  e_ClearInputBuffer();

                  if gMuteWhenInactive then
                    e_MuteChannels(True);

                  if g_debug_WinMsgs then
                  begin
                    g_Console_Add('Minimized indeed');
                    e_WriteLog('[DEBUG] WinMsgs: Minimized indeed', MSG_NOTIFY);
                  end;

                  wMinimized := True;
                end;

                Result := 0;
              end;

            SIZE_MAXIMIZED, // Развернуто на весь экран
            SIZE_RESTORED:  // Восстановлено
              begin
                if wParam = SIZE_MAXIMIZED then
                  begin
                    if g_debug_WinMsgs then
                    begin
                      g_Console_Add('Maximized');
                      e_WriteLog('[DEBUG] WinMsgs: Maximized', MSG_NOTIFY);
                    end;

                    if not gWinMaximized then
                    begin // Стало на весь экран
                      if not gFullscreen then
                      begin
                        gWinSizeX := gScreenWidth;
                        gWinSizeY := gScreenHeight;
                      end;

                      if g_debug_WinMsgs then
                      begin
                        g_Console_Add('Maximized indeed');
                        e_WriteLog('[DEBUG] WinMsgs: Maximized indeed', MSG_NOTIFY);
                      end;

                      gWinMaximized := True;
                    end;
                  end
                else // SIZE_RESTORED
                  begin
                    if g_debug_WinMsgs then
                    begin
                      g_Console_Add('Restored');
                      e_WriteLog('[DEBUG] WinMsgs: Restored', MSG_NOTIFY);
                    end;

                    if gWinMaximized then
                    begin // Было на весь экран
                      if g_debug_WinMsgs then
                      begin
                        g_Console_Add('No more Maximized');
                        e_WriteLog('[DEBUG] WinMsgs: No more Maximized', MSG_NOTIFY);
                      end;

                      gWinMaximized := False;
                    end;
                  end;

                ChangeWindowSize(LOWORD(DWORD(lParam)), HIWORD(DWORD(lParam)));

                if gFullscreen then
                begin
                  g_Window_SetDisplay();
                end;

                if wMinimized then
                begin // Было свернуто
                  e_EnableInput := True;

                  if gMuteWhenInactive then
                    e_MuteChannels(False);

                  if g_debug_WinMsgs then
                  begin
                    g_Console_Add('No more Minimized');
                    e_WriteLog('[DEBUG] WinMsgs: No more Minimized', MSG_NOTIFY);
                  end;

                  wMinimized := False;
                end;

                e_ResizeWindow(gScreenWidth, gScreenHeight);

                Result := 0;
              end;

            else
              Result := DefWindowProc(hWnd, message, wParam, lParam);
          end;
        end
      else
        Result := DefWindowProc(hWnd, message, wParam, lParam);

    WM_MOVE:
      if wWindowCreated then
        begin // Окно перемещено
          gWinPosX := SmallInt(LOWORD(DWORD(lParam)));
          gWinPosY := SmallInt(HIWORD(DWORD(lParam)));
          if ((GetWindowLong(h_Wnd, GWL_STYLE) and WS_MAXIMIZE) = 0) and
             (not gFullscreen) then
          begin
            gWinRealPosX := gWinPosX - gWinFrameX;
            gWinRealPosY := gWinPosY - gWinFrameY - gWinCaption;
          end;

          if g_debug_WinMsgs then
          begin
            g_Console_Add('Moved');
            e_WriteLog('[DEBUG] WinMsgs: Moved', MSG_NOTIFY);
          end;

          Result := 0;
        end
      else
        Result := DefWindowProc(hWnd, message, wParam, lParam);

    WM_KEYDOWN:
      begin // Нажата клавиша
        KeyPress(wParam);
        Result := 0;
      end;

    WM_CHAR:
      begin // Нажатая клавиша передала символ
        if wParam in [32..255] then
          CharPress(Chr(wParam));
        Result := 0;
      end;

    else
      Result := DefWindowProc(hWnd, message, wParam, lParam);
  end;
end;

procedure KillGLWindow();
begin
  wWindowCreated := False;

  if gFullscreen then
    ChangeDisplaySettings(devmode(nil^), 0);

  ShowCursor(True);

  if (h_RC <> 0) and not wglDeleteContext(h_RC) then
  begin
    e_WriteLog('Release of Rendering Context failed', MSG_FATALERROR);
    h_RC := 0
  end;

  if (h_DC <> 0) and (ReleaseDC(h_Wnd, h_DC) = 0) then
  begin
    e_WriteLog('Release of Device Context failed', MSG_FATALERROR);
    h_Dc := 0;
  end;

  if (h_Wnd <> 0) and not DestroyWindow(h_Wnd) then
  begin
    e_WriteLog('Could not release window handle', MSG_FATALERROR);
    h_Wnd := 0;
  end;

  UnregisterClass(D2DF_CLASSNAME, hInstance);
end;

function CreateGLWindow(Title: PChar): Bool; stdcall;
var
  WC: TWndClass;
  h_Instance: HInst;
  hIC: Cardinal;
  ps, sz: TPoint;
  dwStyle, dwExStyle: DWORD;
begin
  Result := False;

  h_Instance := GetModuleHandle(nil);

  ShowCursor(False);

// Иконка:
  hIC := LoadImage(hInstance, 'DFICON',
                   IMAGE_ICON, 0, 0,
                   LR_DEFAULTCOLOR + LR_LOADTRANSPARENT + LR_COPYFROMRESOURCE);
 
  ZeroMemory(@WC, SizeOf(TWndClass));

// Класс окна:
  with WC do
  begin
    Style := CS_HREDRAW or CS_VREDRAW or CS_OWNDC;
    lpfnWndProc   := @WndProc;
    hInstance     := h_Instance;
    hCursor       := LoadCursor(0, IDC_ARROW);
    hIcon         := hIC;
    lpszClassname := D2DF_CLASSNAME;
  end;         

  e_WriteLog('Registering window class', MSG_NOTIFY);
  if RegisterClass(WC) = 0 then
  begin
    e_WriteLog('Failed to register the window class', MSG_FATALERROR);
    Exit;
  end;

  if gFullscreen then
    if not g_Window_SetDisplay() then
      Exit;

// Размеры рамок:
  gWinFrameX := GetSystemMetrics(SM_CXFIXEDFRAME);
  gWinFrameY := GetSystemMetrics(SM_CYFIXEDFRAME);
  gWinCaption := GetSystemMetrics(SM_CYCAPTION);

  if gFullscreen then
    begin // FullScreen
    // Стиль окна:
      dwStyle := gWindowStyle[2][1];
      dwExStyle := gWindowStyle[2][2];
    // Размер окна:
      sz.X := gScreenWidth;
      sz.Y := gScreenHeight;
    // Положение окна:
      ps.X := 0;
      ps.Y := 0;
    end
  else
    begin // Windowed
    // Стиль окна:
      dwStyle := gWindowStyle[1][1];
      dwExStyle := gWindowStyle[1][2];
    // Размер окна:
      sz.X := gScreenWidth + 2*gWinFrameX;
      sz.Y := gScreenHeight + 2*gWinFrameY + gWinCaption;
    // Положение окна:
      ps.X := gWinRealPosX;
      ps.Y := gWinRealPosY;
    end;

  gWinSizeX := gScreenWidth;
  gWinSizeY := gScreenHeight;

  e_WriteLog('Creating window', MSG_NOTIFY);
  h_Wnd := CreateWindowEx(dwExStyle, D2DF_CLASSNAME,
                          Title, dwStyle,
                          ps.X, ps.Y, sz.X, sz.Y,
                          0, 0, hInstance, nil);

  if h_Wnd = 0 then
  begin
    KillGLWindow();
    e_WriteLog('Window creation error', MSG_FATALERROR);
    Exit;
  end;

  wWindowCreated := True;

  h_DC := GetDC(h_Wnd);
  if h_DC = 0 then
  begin
    KillGLWindow();
    e_WriteLog('Cant''t create a GL device context', MSG_FATALERROR);
    Exit;
  end;

  e_WriteLog('Creating Rendering Context', MSG_NOTIFY);
  h_RC := CreateRenderingContext(h_DC, [opDoubleBuffered], gBPP, 0, 0, 0, 0, 0);
  if h_RC = 0 then
  begin
    KillGLWindow();
    e_WriteLog('Cant''t create a GL rendering context', MSG_FATALERROR);
    CreateGLWindow := False;
    Exit;
  end;

  wMinimized := False;
  gWinActive := False;

  e_WriteLog('Activate Rendering Context', MSG_NOTIFY);
  ActivateRenderingContext(h_DC, h_RC);

  if gFullscreen then
    begin // На полный экран
      ShowWindow(h_Wnd, SW_SHOW);
      SetForegroundWindow(h_Wnd);
      SetFocus(h_Wnd);
    end
  else
    if gWinMaximized then
      begin // Развернуто
        ShowWindow(h_Wnd, SW_SHOWMAXIMIZED);
      end
    else
      begin // Обычное
        ShowWindow(h_Wnd, SW_SHOW);
      end;

  e_ResizeWindow(gScreenWidth, gScreenHeight);
  e_InitGL(gVSync);

  Result := True;
end;

function GetTimer(): Int64;
var
  F, C: Int64;
begin
  QueryPerformanceFrequency(F);
  QueryPerformanceCounter(C);
  Result := Round(C*1000000/F);
end;

procedure ResetTimer();
begin
  wNeedTimeReset := True;
end;

procedure PreventWindowFromLockUp;
var
  msg: TMsg;
begin
  if PeekMessage(msg, 0, 0, 0, PM_REMOVE) then
  begin
    TranslateMessage(msg);
    DispatchMessage(msg);
  end;
end;

procedure ProcessLoading();
var
  msg: TMsg;
  ID: DWORD;
  k: Integer;
begin
  k := 0;
  wNeedFree := False;
  while PeekMessage(msg, 0, 0, 0, PM_REMOVE) and
        (k < MAX_HANDLED_MESSAGES) do
  begin
    if (msg.message = WM_QUIT) or
       (msg.message = WM_CLOSE) then
    begin
      msg.message := WM_NULL;
      Break;
    end;

    TranslateMessage(msg);
    DispatchMessage(msg);
    Inc(k);
  end;
  wNeedFree := True;

  if (msg.message = WM_QUIT) or (gExit = EXIT_QUIT) then
    Exit;

  if not wMinimized then
  begin
    if g_Texture_Get('INTER', ID) then
      e_DrawSize(ID, 0, 0, 0, False, False, gScreenWidth, gScreenHeight)
    else
      e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);

    DrawLoadingStat();
    SwapBuffers(h_DC);

    ReShowCursor();
  end;

  e_SoundUpdate();


  if NetMode = NET_SERVER then
    g_Net_Host_Update
  else
    if (NetMode = NET_CLIENT) and (NetState <> NET_STATE_AUTH) then
      g_Net_Client_UpdateWhileLoading;
end;

function ProcessMessage(): Boolean;
var
  i: Integer;
  msg: TMsg;
begin
  Result := False;

  if PeekMessage(msg, 0, 0, 0, PM_REMOVE) then
  begin
    TranslateMessage(msg);
    DispatchMessage(msg);
  end;

  if msg.message = WM_QUIT then
  begin
    Result := True;
    Exit;
  end;
    
  Time := GetTimer();
  Time_Delta := Time - Time_Old;

  flag := False;

  if wNeedTimeReset then
  begin
    Time_Delta := 27777;
    wNeedTimeReset := False;
  end;

  for i := 1 to Time_Delta div 27777 do
  begin
    Update();
    flag := True;
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
      SwapBuffers(h_DC);
      ReShowCursor();
    end;
  end
  else
    Sleep(1);

  e_SoundUpdate();
end;

{
procedure EnumDisplayModes();
var
  n: Integer;
  dm: DevMode;
  s: String;
begin
  e_WriteLog('List of display modes:', MSG_NOTIFY);

  n := 0;

  while EnumDisplaySettings(nil, n, dm) do
  begin
    s := '   ';
    s := s + IntToStr(n + 1) + '. ';
    s := s + IntToStr(dm.dmPelsWidth) + ' x ';
    s := s + IntToStr(dm.dmPelsHeight) + ' x ';
    s := s + IntToStr(dm.dmBitsPerPel) + 'b @';
    s := s + IntToStr(dm.dmDisplayFrequency) + ';';
    
    e_WriteLog(s, MSG_NOTIFY);

    Inc(n);
  end;
end;
}

procedure ReDrawWindow;
begin
  SwapBuffers(h_DC);
  ReShowCursor;
end;

function WinMain(hInstance: HINST; hPrevInstance: HINST; lpCmdLine: PChar;
                 nCmdShow: Integer): Integer; stdcall;
begin
  e_WriteLog('Initializing OpenGL', MSG_NOTIFY);
  InitOpenGL();

  e_WriteLog('Creating GL window', MSG_NOTIFY);
  if not CreateGLWindow(PChar(Format('Doom 2D: Forever %s', [GAME_VERSION]))) then
  begin
    Result := 0;
    Exit
  end;

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
