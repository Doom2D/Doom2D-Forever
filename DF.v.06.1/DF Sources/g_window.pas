unit g_window;

interface

uses windows;

function WinMain(hInstance: HINST; hPrevInstance: HINST; lpCmdLine: PChar;
                 nCmdShow: Integer): Integer; stdcall;
function GetTimer(): Int64;
procedure ResetTimer();
function SetDisplay(): Boolean;
function CreateGLWindow(Title: PChar): Bool; stdcall;
procedure KillGLWindow();

var
  h_Wnd: HWND;

implementation

uses
  messages, dglOpenGL, e_graphics, e_log, g_main, g_console, SysUtils, e_input,
  g_options, g_game, g_basic;

var
  h_RC:  HGLRC;
  h_DC:  HDC;
  WindowCreated: Boolean = False;
  Time, Time_Delta, Time_Old: Int64;
  flag: Boolean;
  a: Boolean = False;
  WinX, WinY: Word;

function SetDisplay(): Boolean;
var
  dmScreenSettings: DevMode;
  MaxFreq: LongWord;
  n: Word;
begin
 Result := False;

 if gFreq <> 0 then MaxFreq := gFreq
  else
 begin
  MaxFreq := 0;
  n := 0;

  while EnumDisplaySettings(nil, n, dmScreenSettings) do
  begin
   with dmScreenSettings do
    if (dmBitsPerPel = gBPP) and (dmPelsWidth = gScreenWidth) and (dmPelsHeight = gScreenHeight) then
     if dmDisplayFrequency > MaxFreq then MaxFreq := dmDisplayFrequency;

   n := n+1;
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

 Result := True;
end;

function WndProc(hWnd: HWND; message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
 if message = WM_SYSCOMMAND then
  case wParam of
   SC_SCREENSAVE, SC_MONITORPOWER:
    begin
     Result := 0;
     Exit;
    end
  end;

 case message of
  WM_ACTIVATEAPP:
   begin
    if LOWORD(wParam) = WA_ACTIVE then
    begin
     e_EnableInput := True;
     ShowWindow(h_Wnd, SW_SHOWNORMAL);
     if gFullscreen then SetDisplay();
    end
     else
    begin
     e_EnableInput := False;
     ShowWindow(h_Wnd, SW_SHOWMINIMIZED);
     if gFullscreen then ChangeDisplaySettings(_devicemodeA(nil^), CDS_FULLSCREEN);
    end;

    Result := 0;
   end;
  WM_CLOSE:
   begin
    if gExit <> EXIT_QUIT then g_Game_Quit();
    Result := 0;
   end;
  WM_SIZE:
   if WindowCreated then
   begin
    gScreenWidth := LOWORD(lParam);
    gScreenHeight := HIWORD(lParam);
    e_ResizeWindow(gScreenWidth, gScreenHeight);
    Result := 0;
   end else Result := DefWindowProc(hWnd, message, wParam, lParam);
  WM_MOVE:
  begin
   WinX := LOWORD(DWORD(lParam));
   WinY := HIWORD(DWORD(lParam));
   Result := 0;
  end;
  WM_CHAR:
   begin
    if wParam in [32..255] then CharPress(Chr(wParam));
    Result := 0;
   end;
  WM_KEYDOWN:
   begin
    KeyPress(wParam);
    Result := 0;
   end;
  else Result := DefWindowProc(hWnd, message, wParam, lParam);
 end;
end;

procedure KillGLWindow();
begin
 if gFullscreen then ChangeDisplaySettings(devmode(nil^), 0);

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

 UnregisterClass('OpenGL', hInstance);
end;

function CreateGLWindow(Title: PChar): Bool; stdcall;
var
  WC: TWndClass;
  dwExStyle: DWORD;
  dwStyle: DWORD;
  h_Instance: hInst;
begin
 Result := False;

 h_Instance := GetModuleHandle(nil);

 ShowCursor(False);

 ZeroMemory(@WC, SizeOf(TWndClass));

 with WC do
 begin
  Style := CS_HREDRAW or CS_VREDRAW or CS_OWNDC;
  lpfnWndProc   := @WndProc;
  hInstance     := h_Instance;
  hCursor       := 0;
  lpszClassname := 'OpenGL';
 end;

 e_WriteLog('Registering window class', MSG_NOTIFY);
 if RegisterClass(WC) = 0 then
 begin
  e_WriteLog('Failed to register the window class', MSG_FATALERROR);
  Exit;
 end;

 if gFullscreen then if not SetDisplay() then Exit;

 if gFullscreen then
 begin
  dwExStyle := WS_EX_APPWINDOW;
  dwStyle := WS_POPUP or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
 end
  else
 begin
  dwExStyle := WS_EX_APPWINDOW or WS_EX_WINDOWEDGE;
  dwStyle := WS_OVERLAPPEDWINDOW or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
 end;

 e_WriteLog('Creating window', MSG_NOTIFY);
 h_Wnd := CreateWindowEx(dwExStyle, 'OpenGL', Title, dwStyle, 0, 0,
                         gScreenWidth, gScreenHeight, 0, 0, hInstance, nil);

 if h_Wnd = 0 then
 begin
  KillGLWindow();
  e_WriteLog('Window creation error', MSG_FATALERROR);
  Exit;
 end;

 WindowCreated := True;

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

 e_WriteLog('Activate Rendering Context', MSG_NOTIFY);
 ActivateRenderingContext(h_DC, h_RC);

 ShowWindow(h_Wnd, SW_SHOW);
 SetForegroundWindow(h_Wnd);
 SetFocus(h_Wnd);
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
 a := True;
end;

function WinMain(hInstance: HINST; hPrevInstance: HINST; lpCmdLine: PChar;
                 nCmdShow: Integer): Integer; stdcall;
var
  msg: TMsg;
  i: Integer;
begin
 e_WriteLog('Initializing OpenGL', MSG_NOTIFY);
 InitOpenGL();

 e_WriteLog('Creating GL window', MSG_NOTIFY);
 if not CreateGLWindow(PChar(Format('Doom 2D: Forever %s', [GAME_VERSION]))) then
 begin
  Result := 0;
  Exit
 end;

 Init();

 e_WriteLog('Entering to main loop', MSG_NOTIFY);

 Time_Old := GetTimer;

 while msg.message <> WM_QUIT do
 begin
  if PeekMessage(msg, 0, 0, 0, PM_REMOVE) then
  begin
   TranslateMessage(msg);
   DispatchMessage(msg);
  end;

  if GetActiveWindow = h_Wnd then
   SetCursorPos(WinX+(gScreenWidth div 2), WinY+(gScreenHeight div 2));

  Time := GetTimer();
  Time_Delta := Time - Time_Old;

  flag := False;

  if a then
  begin
   Time_Delta := 27777;
   Time_Old := Time-30000;
   a := False;
  end;

  //if Time_Delta div 27777 > 1 then g_Console_Add('lag '+IntToStr(Time_Delta div 27777), True);

  //if Time_Delta div 27777 > 0 then
  for i := 1 to Time_Delta div 27777 do
  begin
   Update();
   flag := true;
  end;

  if flag then Time_Old := Time - (Time_Delta mod 27777);

  if GetActiveWindow() = h_Wnd then
  begin
   Draw();
   SwapBuffers(h_DC);
  end;
 end;

 Release();

 KillGLWindow();
 Result := msg.wParam;
end;

end.
