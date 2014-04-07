unit e_input;

interface

uses
  Windows,
  SysUtils,
  e_log,
  DirectInput;

type
  TMouseInfo = record
   X, Y:    Integer;
   Buttons: Array [0..3] of Boolean;
   Accel:   Real;
  end;

function e_PollKeyboard(): boolean;
procedure e_PollMouse();
function e_InitDirectInput(hWnd: HWND): Boolean;
procedure e_ReleaseDirectInput();
procedure e_ClearInputBuffer();

var
 e_KeyBuffer: array [0..255] of Byte;
 e_MouseInfo: TMouseInfo;
 e_EnableInput: Boolean = False;

implementation

var
  lpDI8:        IDirectInput8       = nil;
  lpDIKeyboard: IDirectInputDevice8 = nil;
  lpDIMouse:    IDirectInputDevice8 = nil;
  ms:           TDIMOUSESTATE;
  _h_Wnd:       HWND;

procedure e_PollMouse();
begin
  if (GetForegroundWindow = _h_Wnd) then
    if (lpDImouse.GetDeviceState(SizeOf(TDIMOUSESTATE), @ms) <> 0) then
    begin
      e_WriteLog('DirectInput mouse acquired', MSG_NOTIFY);
      lpDIMouse.Acquire();
      if FAILED(lpDImouse.GetDeviceState(SizeOf(TDIMOUSESTATE), @ms)) then
        Exit;
    end;

  if ms.lX < 0 then ms.lX := Round(ms.lX * e_MouseInfo.Accel) else
  if ms.lX > 0 then ms.lX := Round(ms.lX * e_MouseInfo.Accel);

  if ms.lY < 0 then ms.lY := Round(ms.lY * e_MouseInfo.Accel) else
  if ms.lY > 0 then ms.lY := Round(ms.lY * e_MouseInfo.Accel);

  e_MouseInfo.X := e_MouseInfo.X + ms.lX;
  e_MouseInfo.Y := e_MouseInfo.Y + ms.lY;

  e_MouseInfo.Buttons[0] := ms.rgbButtons[0] = $080;
  e_MouseInfo.Buttons[1] := ms.rgbButtons[1] = $080;
  e_MouseInfo.Buttons[2] := ms.rgbButtons[2] = $080;
  e_MouseInfo.Buttons[3] := ms.rgbButtons[3] = $080;
end;

function e_PollKeyboard(): boolean;
begin
  Result := False;

  if (GetForegroundWindow() = _h_Wnd) then
    if (lpDIKeyboard.GetDeviceState(SizeOf(e_KeyBuffer), @e_KeyBuffer) <> 0) then
    begin
      e_WriteLog('DirectInput keyboard acquired', MSG_NOTIFY);
      lpDIKeyboard.Acquire();
      if FAILED(lpDIKeyboard.GetDeviceState(SizeOf(e_KeyBuffer), @e_KeyBuffer)) then
        Exit;
    end;

  Result := True;
end;

function e_InitDirectInput(hWnd: HWND): Boolean;
begin
 Result := False;
 if FAILED(DirectInput8Create(GetModuleHandle(nil), DIRECTINPUT_VERSION,
                               IID_IDirectInput8, lpDI8, nil)) then exit;
 lpDI8._AddRef();

 if FAILED(lpDI8.CreateDevice(GUID_SysKeyboard, lpDIKeyboard, nil )) then Exit;
 lpDIKeyboard._AddRef();
 if FAILED(lpDIKeyboard.SetDataFormat(c_dfDIKeyboard)) then Exit;
 if FAILED(lpDIKeyboard.SetCooperativeLevel(hWnd, DISCL_FOREGROUND or
                                            DISCL_NONEXCLUSIVE)) then Exit;

 lpDIKeyboard.Acquire();

 if FAILED(lpDI8.CreateDevice(GUID_SysMouse, lpDIMouse, nil)) then Exit;
 lpDIMouse._AddRef();

 if FAILED(lpDIMouse.SetDataFormat(c_dfDIMouse)) then Exit;

 if FAILED(lpDIMouse.SetCooperativeLevel(hWnd, DISCL_FOREGROUND or DISCL_NONEXCLUSIVE)) then Exit;

 lpDIMouse.Acquire;

 e_EnableInput := True;

 _h_Wnd := hWnd;

 Result := True;
end;

procedure e_ReleaseDirectInput();
begin
 if lpDIKeyboard <> nil then
 begin
  lpDIKeyboard.Unacquire;
  lpDIKeyboard._Release;
  lpDIKeyboard := nil;
 end;

 if lpDIMouse <> nil then
  begin
    lpDIMouse.Unacquire;
    lpDIMouse._Release;
    lpDIMouse := nil;
  end;

 if lpDI8 <> nil then
 begin
  lpDI8._Release();
  lpDI8 := nil;
 end;
end;
                                                         
procedure e_ClearInputBuffer();
var
  i: Integer;

begin
  for i := 0 to 255 do
    e_KeyBuffer[i] := 0;
end;

end.
