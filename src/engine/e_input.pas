unit e_input;

interface

uses
  SysUtils,
  e_log,
  SDL;

const
  e_MaxKbdKeys  = 321;
  e_MaxJoys     = 4;
  e_MaxJoyBtns  = 32;
  e_MaxJoyAxes  = 4;
  e_MaxJoyHats  = 4;

  e_MaxJoyKeys = e_MaxJoyBtns + e_MaxJoyAxes*2 + e_MaxJoyHats*4;

  e_MaxInputKeys = e_MaxKbdKeys + e_MaxJoys*e_MaxJoyKeys - 1;
  // $$$..$$$ -  321 Keyboard buttons/keys
  // $$$..$$$ - 4*32 Joystick buttons
  // $$$..$$$ -  4*4 Joystick axes (- and +)
  // $$$..$$$ -  4*4 Joystick hats (L U R D)

  // these are apparently used in g_gui and g_game and elsewhere
  IK_UNKNOWN = SDLK_UNKNOWN;
  IK_INVALID = 65535;
  IK_ESCAPE  = SDLK_ESCAPE;
  IK_RETURN  = SDLK_RETURN;
  IK_ENTER   = SDLK_RETURN;
  IK_UP      = SDLK_UP;
  IK_DOWN    = SDLK_DOWN;
  IK_LEFT    = SDLK_LEFT;
  IK_RIGHT   = SDLK_RIGHT;
  IK_DELETE  = SDLK_DELETE;
  IK_HOME    = SDLK_HOME;
  IK_INSERT  = SDLK_INSERT;
  IK_SPACE   = SDLK_SPACE;
  IK_CONTROL = SDLK_LCTRL;
  IK_SHIFT   = SDLK_LSHIFT;
  IK_TAB     = SDLK_TAB;
  IK_PAGEUP  = SDLK_PAGEUP;
  IK_PAGEDN  = SDLK_PAGEDOWN;
  IK_F2      = SDLK_F2;
  IK_F3      = SDLK_F3;
  IK_F4      = SDLK_F4;
  IK_F5      = SDLK_F5;
  IK_F6      = SDLK_F6;
  IK_F7      = SDLK_F7;
  IK_F8      = SDLK_F8;
  IK_F9      = SDLK_F9;
  IK_F10     = SDLK_F10;
  IK_END     = SDLK_END;
  IK_BACKSPACE = SDLK_BACKSPACE;
  IK_BACKQUOTE = SDLK_BACKQUOTE;
  IK_PAUSE   = SDLK_PAUSE;
  // TODO: think of something better than this shit
  IK_LASTKEY = 320;

  AX_MINUS  = 0;
  AX_PLUS   = 1;
  HAT_LEFT  = 0;
  HAT_UP    = 1;
  HAT_RIGHT = 2;
  HAT_DOWN  = 3;

function  e_InitInput(): Boolean;
procedure e_ReleaseInput();
procedure e_ClearInputBuffer();
function  e_PollInput(): Boolean;
function  e_KeyPressed(Key: Word): Boolean;
function  e_AnyKeyPressed(): Boolean;
function  e_GetFirstKeyPressed(): Word;
function  e_JoystickStateToString(mode: Integer): String;
function  e_JoyByHandle(handle: Word): Integer;
function  e_JoyButtonToKey(id: Word; btn: Byte): Word;
function  e_JoyAxisToKey(id: Word; ax: Byte; dir: Byte): Word;
function  e_JoyHatToKey(id: Word; hat: Byte; dir: Byte): Word;
procedure e_SetKeyState(key: Word; state: Integer);

var
  {e_MouseInfo:          TMouseInfo;}
  e_EnableInput:        Boolean = False;
  e_JoysticksAvailable: Byte    = 0;
  e_JoystickDeadzones:  array [0..e_MaxJoys-1] of Integer = (8192, 8192, 8192, 8192);
  e_KeyNames:           array [0..e_MaxInputKeys] of String;

implementation

uses Math;

const
  KBRD_END = e_MaxKbdKeys;
  JOYK_BEG = KBRD_END;
  JOYK_END = JOYK_BEG + e_MaxJoyBtns*e_MaxJoys;
  JOYA_BEG = JOYK_END;
  JOYA_END = JOYA_BEG + e_MaxJoyAxes*2*e_MaxJoys;
  JOYH_BEG = JOYA_END;
  JOYH_END = JOYH_BEG + e_MaxJoyHats*4*e_MaxJoys;

type
  TJoystick = record
    ID:      Byte;
    Handle:  PSDL_Joystick;
    Axes:    Byte;
    Buttons: Byte;
    Hats:    Byte;
    ButtBuf: array [0..e_MaxJoyBtns] of Boolean;
    AxisBuf: array [0..e_MaxJoyAxes] of Integer;
    HatBuf:  array [0..e_MaxJoyHats] of array [HAT_LEFT..HAT_DOWN] of Boolean;
  end;

var
  KeyBuffer: array [0..e_MaxKbdKeys] of Boolean;
  Joysticks: array of TJoystick = nil;

function OpenJoysticks(): Byte;
var
  i, k, c: Integer;
  joy: PSDL_Joystick;
begin
  Result := 0;
  k := Min(e_MaxJoys, SDL_NumJoysticks());
  if k = 0 then Exit;
  c := 0;
  for i := 0 to k do
  begin
    joy := SDL_JoystickOpen(i);
    if joy <> nil then
    begin
      Inc(c);
      e_WriteLog('Input: Opened SDL joystick ' + IntToStr(i) + ' as joystick ' + IntToStr(c) + ':', MSG_NOTIFY);
      SetLength(Joysticks, c);
      with Joysticks[c-1] do
      begin
        ID := i;
        Handle := joy;
        Axes := Min(e_MaxJoyAxes, SDL_JoystickNumAxes(joy));
        Buttons := Min(e_MaxJoyBtns, SDL_JoystickNumButtons(joy));
        Hats := Min(e_MaxJoyHats, SDL_JoystickNumHats(joy));
        e_WriteLog('       ' + IntToStr(Axes) + ' axes, ' + IntToStr(Buttons) + ' buttons, ' +
                   IntToStr(Hats) + ' hats.', MSG_NOTIFY);
      end;
    end;
  end;
  Result := c;
end;

procedure ReleaseJoysticks();
var
  i: Integer;
begin
  if (Joysticks = nil) or (e_JoysticksAvailable = 0) then Exit;
  for i := Low(Joysticks) to High(Joysticks) do
    with Joysticks[i] do
      SDL_JoystickClose(Handle);
  SetLength(Joysticks, 0);
end;

function PollKeyboard(): Boolean;
var
  Keys: PByte;
  NKeys: Integer;
  i: Cardinal;
begin
  Result := False;
  Keys := SDL_GetKeyState(@NKeys);
  if (Keys = nil) or (NKeys < 1) then
    Exit;
  for i := 0 to NKeys do
    KeyBuffer[i] := ((PByte(Cardinal(Keys) + i)^) <> 0);
  for i := NKeys to High(KeyBuffer) do
    KeyBuffer[i] := False;
end;

function PollJoysticks(): Boolean;
var
  i, j: Word;
  hat: Byte;
begin
  Result := False;
  if (Joysticks = nil) or (e_JoysticksAvailable = 0) then Exit;
  SDL_JoystickUpdate();
  for j := Low(Joysticks) to High(Joysticks) do
    with Joysticks[j] do
    begin
      for i := 0 to Buttons do
        ButtBuf[i] := SDL_JoystickGetButton(Handle, i) <> 0;
      for i := 0 to Axes do
        AxisBuf[i] := SDL_JoystickGetAxis(Handle, i);
      for i := 0 to Hats do
      begin
        hat := SDL_JoystickGetHat(Handle, i);
        HatBuf[i, HAT_UP] := LongBool(hat and SDL_HAT_UP);
        HatBuf[i, HAT_DOWN] := LongBool(hat and SDL_HAT_DOWN);
        HatBuf[i, HAT_LEFT] := LongBool(hat and SDL_HAT_LEFT);
        HatBuf[i, HAT_RIGHT] := LongBool(hat and SDL_HAT_RIGHT);
      end;
    end;
end;

procedure GenerateKeyNames();
var
  i, j, k: LongWord;
begin
  // keyboard key names
  for i := 0 to IK_LASTKEY do
  begin
    e_KeyNames[i] := SDL_GetKeyName(i);
    if e_KeyNames[i] = 'unknown key' then
      e_KeyNames[i] := '';
  end;

  // joysticks
  for j := 0 to e_MaxJoys-1 do
  begin
    k := JOYK_BEG + j * e_MaxJoyBtns;
    // buttons
    for i := 0 to e_MaxJoyBtns-1 do
      e_KeyNames[k + i] := Format('JOY%d B%d', [j, i]);
    k := JOYA_BEG + j * e_MaxJoyAxes * 2;
    // axes
    for i := 0 to e_MaxJoyAxes-1 do
    begin
      e_KeyNames[k + i*2    ] := Format('JOY%d A%d+', [j, i]);
      e_KeyNames[k + i*2 + 1] := Format('JOY%d A%d-', [j, i]);
    end;
    k := JOYH_BEG + j * e_MaxJoyHats * 4;
    // hats
    for i := 0 to e_MaxJoyHats-1 do
    begin
      e_KeyNames[k + i*4    ] := Format('JOY%d D%dL', [j, i]);
      e_KeyNames[k + i*4 + 1] := Format('JOY%d D%dU', [j, i]);
      e_KeyNames[k + i*4 + 2] := Format('JOY%d D%dR', [j, i]);
      e_KeyNames[k + i*4 + 3] := Format('JOY%d D%dD', [j, i]);
    end;
  end;
end;

function e_InitInput(): Boolean;
begin
  Result := False;

  e_JoysticksAvailable := OpenJoysticks();
  e_EnableInput := True;
  GenerateKeyNames();

  Result := True;
end;

procedure e_ReleaseInput();
begin
  ReleaseJoysticks();
  e_JoysticksAvailable := 0;
end;

procedure e_ClearInputBuffer();
var
  i, j, d: Integer;
begin
  for i := Low(KeyBuffer) to High(KeyBuffer) do
    KeyBuffer[i] := False;
  if (Joysticks = nil) or (e_JoysticksAvailable = 0) then
  for i := Low(Joysticks) to High(Joysticks) do
  begin
    for j := Low(Joysticks[i].ButtBuf) to High(Joysticks[i].ButtBuf) do
      Joysticks[i].ButtBuf[j] := False;
    for j := Low(Joysticks[i].AxisBuf) to High(Joysticks[i].AxisBuf) do
      Joysticks[i].AxisBuf[j] := 0;
    for j := Low(Joysticks[i].HatBuf) to High(Joysticks[i].HatBuf) do
      for d := Low(Joysticks[i].HatBuf[j]) to High(Joysticks[i].HatBuf[j]) do
        Joysticks[i].HatBuf[j, d] := False;
  end;
end;

function e_PollInput(): Boolean;
var
  kb, js: Boolean;
begin
  kb := PollKeyboard();
  js := PollJoysticks();

  Result := kb or js;
end;

function e_KeyPressed(Key: Word): Boolean;
var
  joyi, dir: Integer;
begin
  Result := False;
  if (Key = IK_INVALID) or (Key = 0) then Exit;

  if (Key < KBRD_END) then
  begin // Keyboard buttons/keys
    Result := KeyBuffer[Key];
  end

  else if (Key >= JOYK_BEG) and (Key < JOYK_END) then
  begin // Joystick buttons
    JoyI := (Key - JOYK_BEG) div e_MaxJoyBtns;
    if JoyI >= e_JoysticksAvailable then
      Result := False
    else
    begin
      Key := (Key - JOYK_BEG) mod e_MaxJoyBtns;
      Result := Joysticks[JoyI].ButtBuf[Key];
    end;
  end

  else if (Key >= JOYA_BEG) and (Key < JOYA_END) then
  begin // Joystick axes
    JoyI := (Key - JOYA_BEG) div (e_MaxJoyAxes*2);
    if JoyI >= e_JoysticksAvailable then
      Result := False
    else
    begin
      Key := (Key - JOYA_BEG) mod (e_MaxJoyAxes*2);
      dir := Key mod 2;
      if dir = AX_MINUS then
        Result := Joysticks[JoyI].AxisBuf[Key div 2] < -e_JoystickDeadzones[JoyI]
      else
        Result := Joysticks[JoyI].AxisBuf[Key div 2] > e_JoystickDeadzones[JoyI]
    end;
  end

  else if (Key >= JOYH_BEG) and (Key < JOYH_END) then
  begin // Joystick hats
    JoyI := (Key - JOYH_BEG) div (e_MaxJoyHats*4);
    if JoyI >= e_JoysticksAvailable then
      Result := False
    else
    begin
      Key := (Key - JOYH_BEG) mod (e_MaxJoyHats*4);
      dir := Key mod 4;
      Result := Joysticks[JoyI].HatBuf[Key div 4, dir];
    end;
  end;
end;

procedure e_SetKeyState(key: Word; state: Integer);
var
  JoyI, dir: Integer;
begin
  if (Key = IK_INVALID) or (Key = 0) then Exit;

  if (Key < KBRD_END) then
  begin // Keyboard buttons/keys
    keyBuffer[key] := (state <> 0);
  end

  else if (Key >= JOYK_BEG) and (Key < JOYK_END) then
  begin // Joystick buttons
    JoyI := (Key - JOYK_BEG) div e_MaxJoyBtns;
    if JoyI >= e_JoysticksAvailable then
      Exit
    else
    begin
      Key := (Key - JOYK_BEG) mod e_MaxJoyBtns;
      Joysticks[JoyI].ButtBuf[Key] := (state <> 0);
    end;
  end

  else if (Key >= JOYA_BEG) and (Key < JOYA_END) then
  begin // Joystick axes
    JoyI := (Key - JOYA_BEG) div (e_MaxJoyAxes*2);
    if JoyI >= e_JoysticksAvailable then
      Exit
    else
    begin
      Key := (Key - JOYA_BEG) mod (e_MaxJoyAxes*2);
      Joysticks[JoyI].AxisBuf[Key div 2] := state;
    end;
  end

  else if (Key >= JOYH_BEG) and (Key < JOYH_END) then
  begin // Joystick hats
    JoyI := (Key - JOYH_BEG) div (e_MaxJoyHats*4);
    if JoyI >= e_JoysticksAvailable then
      Exit
    else
    begin
      Key := (Key - JOYH_BEG) mod (e_MaxJoyHats*4);
      dir := Key mod 4;
      Joysticks[JoyI].HatBuf[Key div 4, dir] := (state <> 0);
    end;
  end;
end;

function e_AnyKeyPressed(): Boolean;
var
  k: Word;
begin
  Result := False;

  for k := 1 to e_MaxInputKeys do
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
  Result := IK_INVALID;

  for k := 1 to e_MaxInputKeys do
    if e_KeyPressed(k) then
    begin
      Result := k;
      Break;
    end;
end;

////////////////////////////////////////////////////////////////////////////////

function e_JoystickStateToString(mode: Integer): String;
begin
  Result := '';
end;

function  e_JoyByHandle(handle: Word): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Joysticks = nil then Exit;
  for i := Low(Joysticks) to High(Joysticks) do
    if Joysticks[i].ID = handle then
    begin
      Result := i;
      Exit;
    end;
end;

function e_JoyButtonToKey(id: Word; btn: Byte): Word;
begin
  Result := 0;
  if id >= Length(Joysticks) then Exit;
  Result := JOYK_BEG + id*e_MaxJoyBtns + btn;
end;

function e_JoyAxisToKey(id: Word; ax: Byte; dir: Byte): Word;
begin
  Result := 0;
  if id >= Length(Joysticks) then Exit;
  Result := JOYA_BEG + id*e_MaxJoyAxes*2 + ax*2 + dir;
end;

function e_JoyHatToKey(id: Word; hat: Byte; dir: Byte): Word;
begin
  Result := 0;
  if id >= Length(Joysticks) then Exit;
  Result := JOYH_BEG + id*e_MaxJoyHats*4 + hat*4 + dir;
end;

end.
