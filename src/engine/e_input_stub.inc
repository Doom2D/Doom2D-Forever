interface

  uses SysUtils;

  const
    e_MaxKbdKeys  = 256;
    e_MaxJoys     = 4;
    e_MaxJoyBtns  = 32;
    e_MaxJoyAxes  = 8;
    e_MaxJoyHats  = 8;
    e_MaxVirtKeys = 48;

    e_MaxJoyKeys = e_MaxJoyBtns + e_MaxJoyAxes*2 + e_MaxJoyHats*4;

    e_MaxInputKeys = e_MaxKbdKeys + e_MaxJoys*e_MaxJoyKeys + e_MaxVirtKeys - 1;
    // $$$..$$$ -  321 Keyboard buttons/keys
    // $$$..$$$ - 4*32 Joystick buttons
    // $$$..$$$ -  8*2 Joystick axes (- and +)
    // $$$..$$$ -  4*4 Joystick hats (L U R D)
    // $$$..$$$ -   48 Virtual buttons/keys

    KBRD_END = e_MaxKbdKeys;
    JOYK_BEG = KBRD_END;
    JOYK_END = JOYK_BEG + e_MaxJoyBtns*e_MaxJoys;
    JOYA_BEG = JOYK_END;
    JOYA_END = JOYA_BEG + e_MaxJoyAxes*2*e_MaxJoys;
    JOYH_BEG = JOYA_END;
    JOYH_END = JOYH_BEG + e_MaxJoyHats*4*e_MaxJoys;
    VIRT_BEG = JOYH_END;
    VIRT_END = VIRT_BEG + e_MaxVirtKeys;

    // these are apparently used in g_gui and g_game and elsewhere
    IK_INVALID = 0;
    IK_ESCAPE  = 1;
    IK_RETURN  = 2;
    IK_KPRETURN= 3;
    IK_ENTER   = IK_RETURN;
    IK_KPINSERT = 5;
    IK_UP      = 6;
    IK_KPUP    = 7;
    IK_DOWN    = 8;
    IK_KPDOWN  = 9;
    IK_LEFT    = 10;
    IK_KPLEFT  = 11;
    IK_RIGHT   = 12;
    IK_KPRIGHT = 13;
    IK_DELETE  = 14;
    IK_HOME    = 15;
    IK_KPHOME  = 16;
    IK_INSERT  = 17;
    IK_SPACE   = 18;
    IK_SHIFT   = 20;
    IK_ALT     = 21;
    IK_TAB     = 22;
    IK_PAGEUP  = 23;
    IK_KPPAGEUP= 24;
    IK_PAGEDN  = 25;
    IK_KPPAGEDN= 26;
    IK_KP5     = 27;
    IK_NUMLOCK = 28;
    IK_KPDIVIDE= 29;
    IK_KPMULTIPLE= 30;
    IK_KPMINUS = 31;
    IK_KPPLUS  = 32;
    IK_KPENTER = IK_KPRETURN;
    IK_KPDOT   = 34;
    IK_CAPSLOCK= 35;
    IK_RSHIFT  = 36;
    IK_CTRL    = 37;
    IK_RCTRL   = 38;
    IK_RALT    = 39;
    IK_WIN     = 40;
    IK_RWIN    = 41;
    IK_MENU    = 42;
    IK_PRINTSCR= 43;
    IK_SCROLLLOCK= 44;
    IK_LBRACKET= 45;
    IK_RBRACKET= 46;
    IK_SEMICOLON= 47;
    IK_QUOTE   = 48;
    IK_BACKSLASH= 49;
    IK_SLASH   = 50;
    IK_COMMA   = 51;
    IK_DOT     = 52;
    IK_EQUALS  = 53;
    IK_0       = 54;
    IK_1       = 55;
    IK_2       = 56;
    IK_3       = 57;
    IK_4       = 58;
    IK_5       = 59;
    IK_6       = 60;
    IK_7       = 61;
    IK_8       = 62;
    IK_9       = 63;
    IK_F1      = 64;
    IK_F2      = 65;
    IK_F3      = 66;
    IK_F4      = 67;
    IK_F5      = 68;
    IK_F6      = 69;
    IK_F7      = 70;
    IK_F8      = 71;
    IK_F9      = 72;
    IK_F10     = 73;
    IK_F11     = 74;
    IK_F12     = 75;
    IK_END     = 76;
    IK_KPEND   = 77;
    IK_BACKSPACE = 78;
    IK_BACKQUOTE = 79;
    IK_GRAVE     = IK_BACKQUOTE;
    IK_PAUSE   = 81;
    IK_A       = 82;
    IK_B       = 83;
    IK_C       = 84;
    IK_D       = 85;
    IK_E       = 86;
    IK_F       = 87;
    IK_G       = 88;
    IK_H       = 89;
    IK_I       = 90;
    IK_J       = 91;
    IK_K       = 92;
    IK_L       = 93;
    IK_M       = 94;
    IK_N       = 95;
    IK_O       = 96;
    IK_P       = 97;
    IK_Q       = 98;
    IK_R       = 99;
    IK_S       = 100;
    IK_T       = 101;
    IK_U       = 102;
    IK_V       = 103;
    IK_W       = 104;
    IK_X       = 105;
    IK_Y       = 106;
    IK_Z       = 107;
    IK_MINUS   = 108;
    IK_RMETA   = 109;
    IK_LMETA   = 110;
    // TODO: think of something better than this shit
    IK_LASTKEY = e_MaxKbdKeys-1;

  VK_FIRSTKEY = e_MaxKbdKeys + e_MaxJoys*e_MaxJoyKeys;
  VK_LEFT     = VK_FIRSTKEY + 0;
  VK_RIGHT    = VK_FIRSTKEY + 1;
  VK_UP       = VK_FIRSTKEY + 2;
  VK_DOWN     = VK_FIRSTKEY + 3;
  VK_FIRE     = VK_FIRSTKEY + 4;
  VK_OPEN     = VK_FIRSTKEY + 5;
  VK_JUMP     = VK_FIRSTKEY + 6;
  VK_CHAT     = VK_FIRSTKEY + 7;
  VK_ESCAPE   = VK_FIRSTKEY + 8;
  VK_0        = VK_FIRSTKEY + 9;
  VK_1        = VK_FIRSTKEY + 10;
  VK_2        = VK_FIRSTKEY + 11;
  VK_3        = VK_FIRSTKEY + 12;
  VK_4        = VK_FIRSTKEY + 13;
  VK_5        = VK_FIRSTKEY + 14;
  VK_6        = VK_FIRSTKEY + 15;
  VK_7        = VK_FIRSTKEY + 16;
  VK_8        = VK_FIRSTKEY + 17;
  VK_9        = VK_FIRSTKEY + 18;
  VK_A        = VK_FIRSTKEY + 19;
  VK_B        = VK_FIRSTKEY + 20;
  VK_C        = VK_FIRSTKEY + 21;
  VK_D        = VK_FIRSTKEY + 22;
  VK_E        = VK_FIRSTKEY + 23;
  VK_F        = VK_FIRSTKEY + 24;
  VK_CONSOLE  = VK_FIRSTKEY + 25;
  VK_STATUS   = VK_FIRSTKEY + 26;
  VK_TEAM     = VK_FIRSTKEY + 27;
  VK_PREV     = VK_FIRSTKEY + 28;
  VK_NEXT     = VK_FIRSTKEY + 29;
  VK_STRAFE   = VK_FIRSTKEY + 30;
  VK_LSTRAFE  = VK_FIRSTKEY + 31;
  VK_RSTRAFE  = VK_FIRSTKEY + 32;
  VK_PRINTSCR = VK_FIRSTKEY + 33;
  VK_SHOWKBD  = VK_FIRSTKEY + 34;
  VK_HIDEKBD  = VK_FIRSTKEY + 35;
  VK_LASTKEY  = e_MaxKbdKeys + e_MaxJoys*e_MaxJoyKeys + e_MaxVirtKeys - 1;

  AX_MINUS  = 0;
  AX_PLUS   = 1;
  HAT_LEFT  = 0;
  HAT_UP    = 1;
  HAT_RIGHT = 2;
  HAT_DOWN  = 3;

  JOY0_ATTACK = JOYK_BEG + 0*e_MaxJoyBtns + 0;
  JOY1_ATTACK = JOYK_BEG + 1*e_MaxJoyBtns + 0;
  JOY2_ATTACK = JOYK_BEG + 2*e_MaxJoyBtns + 0;
  JOY3_ATTACK = JOYK_BEG + 3*e_MaxJoyBtns + 0;
  JOY0_NEXT = JOYK_BEG + 0*e_MaxJoyBtns + 1;
  JOY1_NEXT = JOYK_BEG + 1*e_MaxJoyBtns + 1;
  JOY2_NEXT = JOYK_BEG + 2*e_MaxJoyBtns + 1;
  JOY3_NEXT = JOYK_BEG + 3*e_MaxJoyBtns + 1;
  JOY0_JUMP = JOYK_BEG + 0*e_MaxJoyBtns + 2;
  JOY1_JUMP = JOYK_BEG + 1*e_MaxJoyBtns + 2;
  JOY2_JUMP = JOYK_BEG + 2*e_MaxJoyBtns + 2;
  JOY3_JUMP = JOYK_BEG + 3*e_MaxJoyBtns + 2;
  JOY0_ACTIVATE = JOYK_BEG + 0*e_MaxJoyBtns + 3;
  JOY1_ACTIVATE = JOYK_BEG + 1*e_MaxJoyBtns + 3;
  JOY2_ACTIVATE = JOYK_BEG + 2*e_MaxJoyBtns + 3;
  JOY3_ACTIVATE = JOYK_BEG + 3*e_MaxJoyBtns + 3;
  JOY0_PREV = JOYK_BEG + 0*e_MaxJoyBtns + 4;
  JOY1_PREV = JOYK_BEG + 1*e_MaxJoyBtns + 4;
  JOY2_PREV = JOYK_BEG + 2*e_MaxJoyBtns + 4;
  JOY3_PREV = JOYK_BEG + 3*e_MaxJoyBtns + 4;

  JOY0_LEFT = JOYH_BEG + 0*e_MaxJoyHats*4 + 0*4 + HAT_LEFT;
  JOY1_LEFT = JOYH_BEG + 1*e_MaxJoyHats*4 + 0*4 + HAT_LEFT;
  JOY2_LEFT = JOYH_BEG + 2*e_MaxJoyHats*4 + 0*4 + HAT_LEFT;
  JOY3_LEFT = JOYH_BEG + 3*e_MaxJoyHats*4 + 0*4 + HAT_LEFT;
  JOY0_RIGHT = JOYH_BEG + 0*e_MaxJoyHats*4 + 0*4 + HAT_RIGHT;
  JOY1_RIGHT = JOYH_BEG + 1*e_MaxJoyHats*4 + 0*4 + HAT_RIGHT;
  JOY2_RIGHT = JOYH_BEG + 2*e_MaxJoyHats*4 + 0*4 + HAT_RIGHT;
  JOY3_RIGHT = JOYH_BEG + 3*e_MaxJoyHats*4 + 0*4 + HAT_RIGHT;
  JOY0_UP = JOYH_BEG + 0*e_MaxJoyHats*4 + 0*4 + HAT_UP;
  JOY1_UP = JOYH_BEG + 1*e_MaxJoyHats*4 + 0*4 + HAT_UP;
  JOY2_UP = JOYH_BEG + 2*e_MaxJoyHats*4 + 0*4 + HAT_UP;
  JOY3_UP = JOYH_BEG + 3*e_MaxJoyHats*4 + 0*4 + HAT_UP;
  JOY0_DOWN = JOYH_BEG + 0*e_MaxJoyHats*4 + 0*4 + HAT_DOWN;
  JOY1_DOWN = JOYH_BEG + 1*e_MaxJoyHats*4 + 0*4 + HAT_DOWN;
  JOY2_DOWN = JOYH_BEG + 2*e_MaxJoyHats*4 + 0*4 + HAT_DOWN;
  JOY3_DOWN = JOYH_BEG + 3*e_MaxJoyHats*4 + 0*4 + HAT_DOWN;

function  e_InitInput: Boolean;
procedure e_ReleaseInput;
procedure e_UnpressAllKeys;
procedure e_KeyUpDown (key: Integer; down: Boolean);

function  e_KeyPressed (key: Integer): Boolean;
function  e_AnyKeyPressed: Boolean;
function  e_GetFirstKeyPressed: Integer;
function  e_HasJoysticks: Boolean;

function  e_JoyButtonToKey (id, btn: Integer): Integer;
function  e_JoyAxisToKey (id, ax, dir: Integer): Integer;
function  e_JoyHatToKey (id, hat, dir: Integer): Integer;

var
  e_JoystickAvailable: array [0..e_MaxJoys - 1] of Boolean;
  e_JoystickDeadzones: array [0..e_MaxJoys - 1] of Integer = (8192, 8192, 8192, 8192);
  e_KeyNames: array [0..e_MaxInputKeys] of String;

implementation

var
  InputBuffer: array [0..e_MaxInputKeys - 1] of Boolean;

procedure e_UnpressAllKeys;
  var i: Integer;
begin
  for i := 0 to High(InputBuffer) do
    InputBuffer[i] := False
end;

procedure e_KeyUpDown (key: Integer; down: Boolean);
begin
  ASSERT(key >= 0);
  ASSERT(key < e_MaxInputKeys);
  if key > 0 then
    InputBuffer[key] := down
end;

procedure GenerateKeyNames;
  var i, j, k: Integer;
begin
  // keyboard key names
  e_KeyNames[IK_0] := '0';
  e_KeyNames[IK_1] := '1';
  e_KeyNames[IK_2] := '2';
  e_KeyNames[IK_3] := '3';
  e_KeyNames[IK_4] := '4';
  e_KeyNames[IK_5] := '5';
  e_KeyNames[IK_6] := '6';
  e_KeyNames[IK_7] := '7';
  e_KeyNames[IK_8] := '8';
  e_KeyNames[IK_9] := '9';

  for i := IK_A to IK_Z do
    e_KeyNames[i] := '' + chr(ord('a') + (i - IK_a));

  e_KeyNames[IK_ESCAPE] := 'ESCAPE';
  e_KeyNames[IK_ENTER] := 'ENTER';
  e_KeyNames[IK_TAB] := 'TAB';
  e_KeyNames[IK_BACKSPACE] := 'BACKSPACE';
  e_KeyNames[IK_SPACE] := 'SPACE';
  e_KeyNames[IK_UP] := 'UP';
  e_KeyNames[IK_LEFT] := 'LEFT';
  e_KeyNames[IK_RIGHT] := 'RIGHT';
  e_KeyNames[IK_DOWN] := 'DOWN';
  e_KeyNames[IK_INSERT] := 'INSERT';
  e_KeyNames[IK_DELETE] := 'DELETE';
  e_KeyNames[IK_HOME] := 'HOME';
  e_KeyNames[IK_END] := 'END';
  e_KeyNames[IK_PAGEUP] := 'PGUP';
  e_KeyNames[IK_PAGEDN] := 'PGDOWN';
  e_KeyNames[IK_KPINSERT] := 'PAD0';
  e_KeyNames[IK_KPEND] := 'PAD1';
  e_KeyNames[IK_KPDOWN] := 'PAD2';
  e_KeyNames[IK_KPPAGEDN] := 'PAD3';
  e_KeyNames[IK_KPLEFT] := 'PAD4';
  e_KeyNames[IK_KP5] := 'PAD5';
  e_KeyNames[IK_KPRIGHT] := 'PAD6';
  e_KeyNames[IK_KPHOME] := 'PAD7';
  e_KeyNames[IK_KPUP] := 'PAD8';
  e_KeyNames[IK_KPPAGEUP] := 'PAD9';
  e_KeyNames[IK_NUMLOCK] := 'NUM';
  e_KeyNames[IK_KPDIVIDE] := 'PAD/';
  e_KeyNames[IK_KPMULTIPLE] := 'PAD*';
  e_KeyNames[IK_KPMINUS] := 'PAD-';
  e_KeyNames[IK_KPPLUS] := 'PAD+';
  e_KeyNames[IK_KPENTER] := 'PADENTER';
  e_KeyNames[IK_KPDOT] := 'PAD.';
  e_KeyNames[IK_CAPSLOCK] := 'CAPS';
  e_KeyNames[IK_BACKQUOTE] := 'BACKQUOTE';
  e_KeyNames[IK_F1] := 'F1';
  e_KeyNames[IK_F2] := 'F2';
  e_KeyNames[IK_F3] := 'F3';
  e_KeyNames[IK_F4] := 'F4';
  e_KeyNames[IK_F5] := 'F5';
  e_KeyNames[IK_F6] := 'F6';
  e_KeyNames[IK_F7] := 'F7';
  e_KeyNames[IK_F8] := 'F8';
  e_KeyNames[IK_F9] := 'F9';
  e_KeyNames[IK_F10] := 'F10';
  e_KeyNames[IK_F11] := 'F11';
  e_KeyNames[IK_F12] := 'F12';
  e_KeyNames[IK_SHIFT] := 'LSHIFT';
  e_KeyNames[IK_RSHIFT] := 'RSHIFT';
  e_KeyNames[IK_CTRL] := 'LCTRL';
  e_KeyNames[IK_RCTRL] := 'RCTRL';
  e_KeyNames[IK_ALT] := 'LALT';
  e_KeyNames[IK_RALT] := 'RALT';
  e_KeyNames[IK_WIN] := 'LWIN';
  e_KeyNames[IK_RWIN] := 'RWIN';
  e_KeyNames[IK_MENU] := 'MENU';
  e_KeyNames[IK_PRINTSCR] := 'PSCRN';
  e_KeyNames[IK_SCROLLLOCK] := 'SCROLL';
  e_KeyNames[IK_PAUSE] := 'PAUSE';
  e_KeyNames[IK_LBRACKET] := '[';
  e_KeyNames[IK_RBRACKET] := ']';
  e_KeyNames[IK_SEMICOLON] := ';';
  e_KeyNames[IK_QUOTE] := '''';
  e_KeyNames[IK_BACKSLASH] := '\';
  e_KeyNames[IK_SLASH] := '/';
  e_KeyNames[IK_COMMA] := ',';
  e_KeyNames[IK_DOT] := '.';
  e_KeyNames[IK_MINUS] := '-';
  e_KeyNames[IK_EQUALS] := '=';
  e_KeyNames[IK_RMETA] := 'RMETA';
  e_KeyNames[IK_LMETA] := 'LMETA';

  // joysticks
  for j := 0 to e_MaxJoys-1 do
  begin
    k := JOYK_BEG + j * e_MaxJoyBtns;
    // buttons
    for i := 0 to e_MaxJoyBtns-1 do
      e_KeyNames[k + i] := Format('JOY%dB%d', [j, i]);
    k := JOYA_BEG + j * e_MaxJoyAxes * 2;
    // axes
    for i := 0 to e_MaxJoyAxes-1 do
    begin
      e_KeyNames[k + i*2    ] := Format('JOY%dA%d+', [j, i]);
      e_KeyNames[k + i*2 + 1] := Format('JOY%dA%d-', [j, i]);
    end;
    k := JOYH_BEG + j * e_MaxJoyHats * 4;
    // hats
    for i := 0 to e_MaxJoyHats-1 do
    begin
      e_KeyNames[k + i*4    ] := Format('JOY%dD%dL', [j, i]);
      e_KeyNames[k + i*4 + 1] := Format('JOY%dD%dU', [j, i]);
      e_KeyNames[k + i*4 + 2] := Format('JOY%dD%dR', [j, i]);
      e_KeyNames[k + i*4 + 3] := Format('JOY%dD%dD', [j, i]);
    end;
  end;

  // vitrual keys
  for i := 0 to e_MaxVirtKeys-1 do
    e_KeyNames[VIRT_BEG + i] := 'VIRTUAL' + IntToStr(i);
end;

function e_HasJoysticks: Boolean;
  var i: Integer;
begin
  i := 0;
  while (i < e_MaxJoys) and (e_JoystickAvailable[i] = False) do inc(i);
  result := i < e_MaxJoys
end;

function e_InitInput: Boolean;
  var i: Integer;
begin
  for i := 0 to e_MaxJoys - 1 do
    e_JoystickAvailable[i] := False;
  GenerateKeyNames;
  result := True
end;

procedure e_ReleaseInput;
  var i: Integer;
begin
  for i := 0 to e_MaxJoys - 1 do
    e_JoystickAvailable[i] := False
end;

function e_KeyPressed (key: Integer): Boolean;
begin
  ASSERT(key >= 0);
  ASSERT(key < e_MaxInputKeys);
  result := InputBuffer[key]
end;

function e_AnyKeyPressed: Boolean;
begin
  result := e_GetFirstKeyPressed <> IK_INVALID;
end;

function e_GetFirstKeyPressed: Integer;
  var i: Integer;
begin
  i := 1;
  while (i < e_MaxInputKeys) and (InputBuffer[i] = False) do inc(i);
  if i < e_MaxInputKeys then
    result := i
  else
    result := IK_INVALID
end;

function e_JoyButtonToKey (id, btn: Integer): Integer;
begin
  ASSERT(id >= 0);
  ASSERT(id < e_MaxJoys);
  ASSERT(btn >= 0);
  ASSERT(btn < e_MaxJoyBtns);
  result := JOYK_BEG + id*e_MaxJoyBtns + btn
end;

function e_JoyAxisToKey (id, ax, dir: Integer): Integer;
begin
  ASSERT(id >= 0);
  ASSERT(id < e_MaxJoys);
  ASSERT(ax >= 0);
  ASSERT(ax < e_MaxJoyAxes);
  ASSERT(dir in [AX_MINUS, AX_PLUS]);
  result := JOYA_BEG + id*e_MaxJoyAxes*2 + ax*2 + dir
end;

function e_JoyHatToKey (id, hat, dir: Integer): Integer;
begin
  ASSERT(id >= 0);
  ASSERT(id < e_MaxJoys);
  ASSERT(hat >= 0);
  ASSERT(hat < e_MaxJoyHats);
  ASSERT(dir in [HAT_LEFT, HAT_UP, HAT_RIGHT, HAT_DOWN]);
  result := JOYH_BEG + id*e_MaxJoyHats*4 + hat*4 + dir
end;
