Unit e_input;

Interface

Uses
  Windows,
  SysUtils,
  e_log,
  DirectInput;

{type
  TMouseInfo = record
    X, Y:    Integer;
    Buttons: Array [0..3] of Boolean;
    Accel:   Real;
  end;}

const
  e_MaxInputKeys    = 256 + 32 + 6 + 6 + 4 - 1;
  // 0..255   - 256 Keyboard buttons/keys
  // 256..287 -  32 Joystick buttons
  // 288..293 -   3 Joystick axises +/-
  // 294..299 -   3 Joystick axis rotations +/-
  // 300..303 -   2 Joystick sliders +/-

  e_WrongKey        = 65535;

  e_IKey_Escape     = DIK_ESCAPE;
  e_IKey_Backspace  = DIK_BACK;
  e_IKey_Tab        = DIK_TAB;
  e_IKey_Enter      = DIK_RETURN;
  e_IKey_Space      = DIK_SPACE;
  
  e_IKey_Up         = DIK_UP;
  e_IKey_Left       = DIK_LEFT;
  e_IKey_Right      = DIK_RIGHT;
  e_IKey_Down       = DIK_DOWN;

{procedure e_PollMouse();}
function  e_InitDirectInput(hWnd: HWND): Boolean;
procedure e_ReleaseDirectInput();
procedure e_ClearInputBuffer();
function  e_PollInput(): Boolean;
function  e_KeyPressed(Key: Word): Boolean;
function  e_AnyKeyPressed(): Boolean;
function  e_GetFirstKeyPressed(): Word;
function  e_JoystickStateToString(mode: Integer): String;

var
  {e_MouseInfo:          TMouseInfo;}
  e_EnableInput:        Boolean = False;
  e_JoystickAvailable:  Boolean = False;

Implementation

const
  CUSTOMIZABLE_JOYSTICK  = True;

type
  TJoystickCustomField = record
    here: Boolean;
    min, center, max: Integer;
  end;

  TJoystickCustom = record
    X: TJoystickCustomField;                        (* x-axis position      *)
    Y: TJoystickCustomField;                        (* y-axis position      *)
    Z: TJoystickCustomField;                        (* z-axis position      *)

    Rx: TJoystickCustomField;                       (* x-axis rotation      *)
    Ry: TJoystickCustomField;                       (* y-axis rotation      *)
    Rz: TJoystickCustomField;                       (* z-axis rotation      *)

    Slider: Array [0..1] of TJoystickCustomField;   (* extra axes positions *)
    POV: Array [0..3] of TJoystickCustomField;      (* POV directions       *)
  end;

var
  lpDI8:              IDirectInput8           = nil;
  lpDIKeyboard:       IDirectInputDevice8     = nil;
  {lpDIMouse:         IDirectInputDevice8     = nil;}
  lpDIJoystick:       IDirectInputDevice8     = nil;
  ms:                 TDIMOUSESTATE;
  _h_Wnd:             HWND;
  keyBuffer:          Array [0..255] of Byte;
  joystickState:      TDIJoyState;
  joystickCustomized: Boolean                 = False;
  joystickCustom:     TJoystickCustom;


function GetMaxFromCenter(center: Integer): Integer;
begin
  Result := center * 2;
  if (Result < center) or (Result > MaxInt) then
    Result := MaxInt;
end;

function PosRelation(pos: Integer; field: TJoystickCustomField): Integer;
begin
  if (not field.here) or (pos = field.center) then
    Result := 0
  else
    if (field.center < pos) then
      begin
        if (pos > (field.center + ((field.max - field.center) div 3))) then
          Result := 1
        else
          Result := 0;
      end
    else // pos < field.center
      begin
        if (pos < (field.center - ((field.center - field.min) div 3))) then
          Result := -1
        else
          Result := 0;
      end;
end;

procedure CustomizeJoystick();
var
  i: Integer;
begin
  joystickCustom.X.here := (joystickState.lX <> 0);
  if (joystickCustom.X.here) then
  begin
    joystickCustom.X.center := joystickState.lX;
    joystickCustom.X.min := 0;
    joystickCustom.X.max := GetMaxFromCenter(joystickCustom.X.center);
  end;

  joystickCustom.Y.here := (joystickState.lY <> 0);
  if (joystickCustom.Y.here) then
  begin
    joystickCustom.Y.center := joystickState.lY;
    joystickCustom.Y.min := 0;
    joystickCustom.Y.max := GetMaxFromCenter(joystickCustom.Y.center);
  end;

  joystickCustom.Z.here := (joystickState.lZ <> 0);
  if (joystickCustom.Z.here) then
  begin
    joystickCustom.Z.center := joystickState.lZ;
    joystickCustom.Z.min := 0;
    joystickCustom.Z.max := GetMaxFromCenter(joystickCustom.Z.center);
  end;

  joystickCustom.Rx.here := (joystickState.lRx <> 0);
  if (joystickCustom.Rx.here) then
  begin
    joystickCustom.Rx.center := joystickState.lRx;
    joystickCustom.Rx.min := 0;
    joystickCustom.Rx.max := GetMaxFromCenter(joystickCustom.Rx.center);
  end;

  joystickCustom.Ry.here := (joystickState.lRy <> 0);
  if (joystickCustom.Ry.here) then
  begin
    joystickCustom.Ry.center := joystickState.lRy;
    joystickCustom.Ry.min := 0;
    joystickCustom.Ry.max := GetMaxFromCenter(joystickCustom.Ry.center);
  end;

  joystickCustom.Rz.here := (joystickState.lRz <> 0);
  if (joystickCustom.Rz.here) then
  begin
    joystickCustom.Rz.center := joystickState.lRz;
    joystickCustom.Rz.min := 0;
    joystickCustom.Rz.max := GetMaxFromCenter(joystickCustom.Rz.center);
  end;

  for i := 0 to 1 do
  begin
    joystickCustom.Slider[i].here := (joystickState.rglSlider[i] <> 0);
    if (joystickCustom.Slider[i].here) then
    begin
      joystickCustom.Slider[i].center := joystickState.rglSlider[i];
      joystickCustom.Slider[i].min := 0;
      joystickCustom.Slider[i].max := GetMaxFromCenter(joystickCustom.Slider[i].center);
    end;
  end;
  
// TODO: POV 0..3:
// * value = $FFFFFFFF - no POV or it is in its center
// * value = Angle_In_Degrees * 100
// * 0 - Up, 9000 - Right, 18000 - Down, 27000 - Left
// * How to customize it?

end;

{procedure e_PollMouse();
begin
  if (GetForegroundWindow = _h_Wnd) then
    if (lpDImouse.GetDeviceState(SizeOf(TDIMOUSESTATE), @ms) <> 0) then
    begin
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
end;}

function PollKeyboard(): Boolean;
begin
  Result := False;

  if (GetForegroundWindow() = _h_Wnd) then
    if (lpDIKeyboard.GetDeviceState(SizeOf(keyBuffer), @keyBuffer) <> 0) then
    begin
      lpDIKeyboard.Acquire();
      if FAILED(lpDIKeyboard.GetDeviceState(SizeOf(keyBuffer), @keyBuffer)) then
        Exit;
    end;

  Result := True;
end;

function PollJoystick(): Boolean;
begin
  Result := False;

  if (lpDIJoystick = nil) then
    Exit;

  if (GetForegroundWindow() = _h_Wnd) then
    if (lpDIJoystick.GetDeviceState(SizeOf(TDIJoyState), @joystickState) <> 0) then
    begin
      lpDIJoystick.Acquire();
      if FAILED(lpDIJoystick.GetDeviceState(SizeOf(TDIJoyState), @joystickState)) then
        Exit;
    end;

  if (not joystickCustomized) and CUSTOMIZABLE_JOYSTICK then
  begin
    CustomizeJoystick();
    joystickCustomized := True;
  end;

  Result := True;
end;

function InitJoystick(hWnd: HWND): Boolean;
begin
  Result := False;

  if FAILED(lpDI8.CreateDevice(GUID_Joystick, lpDIJoystick, nil)) then
    Exit;
  lpDIJoystick._AddRef();

  if FAILED(lpDIJoystick.SetDataFormat(c_dfDIJoystick)) then
    Exit;

  if FAILED(lpDIJoystick.SetCooperativeLevel(hWnd, DISCL_FOREGROUND or
                                            DISCL_NONEXCLUSIVE)) then
    Exit;
  lpDIJoystick.Acquire();

  Result := True;
end;

function e_InitDirectInput(hWnd: HWND): Boolean;
begin
  Result := False;

  if FAILED(DirectInput8Create(GetModuleHandle(nil), DIRECTINPUT_VERSION,
                               IID_IDirectInput8, lpDI8, nil)) then
    Exit;
  lpDI8._AddRef();

// Keyboard:
  if FAILED(lpDI8.CreateDevice(GUID_SysKeyboard, lpDIKeyboard, nil)) then
    Exit;
  lpDIKeyboard._AddRef();

  if FAILED(lpDIKeyboard.SetDataFormat(c_dfDIKeyboard)) then
    Exit;

  if FAILED(lpDIKeyboard.SetCooperativeLevel(hWnd, DISCL_FOREGROUND or
                                            DISCL_NONEXCLUSIVE)) then
    Exit;
  lpDIKeyboard.Acquire();

// Mouse:
{  Since we don't actually need the mouse in the game, I commented this out.
  if FAILED(lpDI8.CreateDevice(GUID_SysMouse, lpDIMouse, nil)) then
    Exit;
  lpDIMouse._AddRef();

  if FAILED(lpDIMouse.SetDataFormat(c_dfDIMouse)) then
    Exit;

  if FAILED(lpDIMouse.SetCooperativeLevel(hWnd, DISCL_FOREGROUND or DISCL_NONEXCLUSIVE)) then
    Exit;
  lpDIMouse.Acquire();
}

// Joystick:
  e_JoystickAvailable := InitJoystick(hWnd);
  if (not e_JoystickAvailable) then
    lpDIJoystick := nil;

  e_EnableInput := True;
  _h_Wnd := hWnd;

  Result := True;
end;

procedure e_ReleaseDirectInput();
begin
  if lpDIKeyboard <> nil then
  begin
    lpDIKeyboard.Unacquire();
    lpDIKeyboard._Release();
    lpDIKeyboard := nil;
  end;

{ if lpDIMouse <> nil then
  begin
    lpDIMouse.Unacquire();
    lpDIMouse._Release();
    lpDIMouse := nil;
  end; }

  if lpDIJoystick <> nil then
  begin
    lpDIJoystick.Unacquire();
    lpDIJoystick._Release();
    lpDIJoystick := nil;
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
    keyBuffer[i] := 0;

  FillChar(joystickState, SizeOf(TDIJoyState), 0);
end;

function e_PollInput(): Boolean;
var
  kb, js: Boolean;
begin
  kb := PollKeyboard();
  js := PollJoystick();

  Result := kb or js;
end;

function e_KeyPressed(Key: Word): Boolean;
begin
  if ((Key >= 0) and (Key <= 255)) then
  begin // Keyboard buttons/keys
    Result := (keyBuffer[Key] = $80);
  end
  else if ((Key >= 256) and (Key <= 287)) then
  begin // Joystick buttons
    Key := Key - 256;
    Result := (joystickState.rgbButtons[Key] = $80);
  end
  else if (CUSTOMIZABLE_JOYSTICK) then
  begin // Joystick axises and sliders
    case Key of
      288: Result := (PosRelation(joystickState.lX, joystickCustom.X) = -1);  // X-
      289: Result := (PosRelation(joystickState.lX, joystickCustom.X) =  1);  // X+
      290: Result := (PosRelation(joystickState.lY, joystickCustom.Y) = -1);  // Y-
      291: Result := (PosRelation(joystickState.lY, joystickCustom.Y) =  1);  // Y+
      292: Result := (PosRelation(joystickState.lZ, joystickCustom.Z) = -1);  // Z-
      293: Result := (PosRelation(joystickState.lZ, joystickCustom.Z) =  1);  // Z+

      294: Result := (PosRelation(joystickState.lRx, joystickCustom.Rx) = -1);  // Rx-
      295: Result := (PosRelation(joystickState.lRx, joystickCustom.Rx) =  1);  // Rx+
      296: Result := (PosRelation(joystickState.lRy, joystickCustom.Ry) = -1);  // Ry-
      297: Result := (PosRelation(joystickState.lRy, joystickCustom.Ry) =  1);  // Ry+
      298: Result := (PosRelation(joystickState.lRz, joystickCustom.Rz) = -1);  // Rz-
      299: Result := (PosRelation(joystickState.lRz, joystickCustom.Rz) =  1);  // Rz+

      300: Result := (PosRelation(joystickState.rglSlider[0], joystickCustom.Slider[0]) = -1);  // Slider1-
      301: Result := (PosRelation(joystickState.rglSlider[0], joystickCustom.Slider[0]) =  1);  // Slider1+

      302: Result := (PosRelation(joystickState.rglSlider[1], joystickCustom.Slider[1]) = -1);  // Slider2-
      303: Result := (PosRelation(joystickState.rglSlider[1], joystickCustom.Slider[1]) =  1);  // Slider2+

      else Result := False;
    end;                  
  end
  else
    Result := False;
end;

function e_AnyKeyPressed(): Boolean;
var
  k: Word;
begin
  Result := False;

  for k := 0 to e_MaxInputKeys do
    if e_KeyPressed(k) then
    begin
      Result := True;
      Break;
    end;
end;

function e_GetFirstKeyPressed(): Word;
var
  k: Word;
begin
  Result := e_WrongKey;

  for k := 0 to e_MaxInputKeys do
    if e_KeyPressed(k) then
    begin
      Result := k;
      Break;
    end;
end;

////////////////////////////////////////////////////////////////////////////////

function e_JoystickStateToString(mode: Integer): String;
var
  i: Integer;
begin
  Result := '';

  if (mode = 1) then
  begin
  // 0..65535: Up/Left .. Down/Right: 
    Result := Result + 'X=' + IntToStr(joystickState.lX) + ', ';
    Result := Result + 'Y=' + IntToStr(joystickState.lY) + ', ';
    Result := Result + 'Z=' + IntToStr(joystickState.lZ) + '; ';
  end
  else if (mode = 2) then
  begin
  // 0..65535: Left .. Center .. Right:
    Result := Result + 'Rx=' + IntToStr(joystickState.lRx) + ', ';
    Result := Result + 'Ry=' + IntToStr(joystickState.lRy) + ', ';
    Result := Result + 'Rz=' + IntToStr(joystickState.lRz) + '; ';
  end
  else if (mode = 3) then
  begin
  // 0..65535: Up .. Down:
    Result := Result + 'Slider[0]=' + IntToStr(joystickState.rglSlider[0]) + ', ';
    Result := Result + 'Slider[1]=' + IntToStr(joystickState.rglSlider[1]) + '; ';
  end
  else if (mode = 4) then
  begin
  // 0..35999: POV angle, Up = 0, Clockwise, Center = $FFFFFFFF:
    Result := Result + 'POV[0]=' + IntToStr(joystickState.rgdwPOV[0]) + ', ';
    Result := Result + 'POV[1]=' + IntToStr(joystickState.rgdwPOV[1]) + ', ';
    Result := Result + 'POV[2]=' + IntToStr(joystickState.rgdwPOV[2]) + ', ';
    Result := Result + 'POV[3]=' + IntToStr(joystickState.rgdwPOV[3]) + '; ';
  end
  else if (mode = 5) then
  begin
  // 0 or 128 ($80): NotPressed or Pressed: 
    for i := 0 to 7 do
      Result := Result + 'B[' + IntToStr(i) + ']=' + IntToStr(joystickState.rgbButtons[i]) + ', ';
  end
  else if (mode = 6) then
  begin
  // 0 or 128 ($80): NotPressed or Pressed:
    for i := 8 to 15 do
      Result := Result + 'B[' + IntToStr(i) + ']=' + IntToStr(joystickState.rgbButtons[i]) + ', ';
  end
  else if (mode = 7) then
  begin
  // 0 or 128 ($80): NotPressed or Pressed:
    for i := 16 to 23 do
      Result := Result + 'B[' + IntToStr(i) + ']=' + IntToStr(joystickState.rgbButtons[i]) + ', ';
  end
  else if (mode = 8) then
  begin
  // 0 or 128 ($80): NotPressed or Pressed:
    for i := 24 to 31 do
      Result := Result + 'B[' + IntToStr(i) + ']=' + IntToStr(joystickState.rgbButtons[i]) + '; ';
  end
end;

end.
