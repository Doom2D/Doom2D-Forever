unit g_gui;

interface

uses
  Windows,
  e_graphics, g_playermodel, g_basic, MAPSTRUCT, WADEDITOR;

const
  MAINMENU_HEADER_COLOR: TRGB = (R:255; G:255; B:255);
  MAINMENU_ITEMS_COLOR: TRGB = (R:255; G:255; B:255);
  MAINMENU_UNACTIVEITEMS_COLOR: TRGB = (R:192; G:192; B:192);
  MAINMENU_CLICKSOUND = 'MENU_SELECT';
  MAINMENU_CHANGESOUND = 'MENU_CHANGE';
  MAINMENU_SPACE = 4;
  MAINMENU_MARKER1 = 'MAINMENU_MARKER1';
  MAINMENU_MARKER2 = 'MAINMENU_MARKER2';
  MAINMENU_MARKERDELAY = 24;
  WINDOW_CLOSESOUND = 'MENU_CLOSE';
  MENU_HEADERCOLOR: TRGB = (R:255; G:255; B:255);
  MENU_ITEMSTEXT_COLOR: TRGB = (R:255; G:255; B:255);
  MENU_UNACTIVEITEMS_COLOR: TRGB = (R:128; G:128; B:128);
  MENU_ITEMSCTRL_COLOR: TRGB = (R:255; G:0; B:0);
  MENU_VSPACE = 2;
  MENU_HSPACE = 32;
  MENU_CLICKSOUND = 'MENU_SELECT';
  MENU_CHANGESOUND = 'MENU_CHANGE';
  MENU_MARKERDELAY = 24;
  SCROLL_LEFT = 'SCROLL_LEFT';
  SCROLL_RIGHT = 'SCROLL_RIGHT';
  SCROLL_MIDDLE = 'SCROLL_MIDDLE';
  SCROLL_MARKER = 'SCROLL_MARKER';
  SCROLL_ADDSOUND = 'SCROLL_ADD';
  SCROLL_SUBSOUND = 'SCROLL_SUB';
  EDIT_LEFT = 'EDIT_LEFT';
  EDIT_RIGHT = 'EDIT_RIGHT';
  EDIT_MIDDLE = 'EDIT_MIDDLE';
  EDIT_CURSORCOLOR: TRGB = (R:200; G:0; B:0);
  EDIT_CURSORLEN = 10;
  KEYREAD_QUERY = '<...>';
  KEYREAD_CLEAR = '???';
  KEYREAD_TIMEOUT = 24;
  MAPPREVIEW_WIDTH = 8;
  MAPPREVIEW_HEIGHT = 8;
  BOX1 = 'BOX1';
  BOX2 = 'BOX2';
  BOX3 = 'BOX3';
  BOX4 = 'BOX4';
  BOX5 = 'BOX5';
  BOX6 = 'BOX6';
  BOX7 = 'BOX7';
  BOX8 = 'BOX8';
  BOX9 = 'BOX9';
  BSCROLL_UPA = 'BSCROLL_UP_A';
  BSCROLL_UPU = 'BSCROLL_UP_U';
  BSCROLL_DOWNA = 'BSCROLL_DOWN_A';
  BSCROLL_DOWNU = 'BSCROLL_DOWN_U';
  BSCROLL_MIDDLE = 'BSCROLL_MIDDLE';

type
  TMessage = record
    Msg: DWORD;
    wParam: LongInt;
    lParam: LongInt;
  end;
 
  TFontType = (FONT_TEXTURE, FONT_CHAR);

  TFont = class(TObject)
  private
    ID: DWORD;
    FScale: Single;
    FFontType: TFontType;
  public
    constructor Create(FontID: DWORD; FontType: TFontType);
    destructor Destroy; override;
    procedure Draw(X, Y: Integer; Text: string; R, G, B: Byte);
    procedure GetTextSize(Text: string; var w, h: Word);
    property Scale: Single read FScale write FScale;
  end;

  TGUIControl = class;

  TOnKeyDownEvent = procedure(Key: Byte);
  TOnCloseEvent = procedure;
  TOnShowEvent = procedure;
  TOnClickEvent = procedure;
  TOnChangeEvent = procedure(Sender: TGUIControl);
  TOnEnterEvent = procedure(Sender: TGUIControl);

  TGUIWindow = class;

  TGUIControl = class
  private
    FX, FY: Integer;
    FEnabled: Boolean;
    FWindow : TGUIWindow;
    FName: string;
  public
    constructor Create;
    procedure OnMessage(var Msg: TMessage); virtual;
    procedure Update; virtual;
    procedure Draw; virtual;    
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Name: string read FName write FName;
  end;

  TGUIWindow = class
  private
    FActiveControl: TGUIControl;
    FDefControl: string;
    FPrevWindow: TGUIWindow;
    FName: string;
    FBackTexture: string;
    FMainWindow: Boolean;
    FOnKeyDown: TOnKeyDownEvent;
    FOnCloseEvent: TOnCloseEvent;
    FOnShowEvent: TOnShowEvent;
  public
    Childs: array of TGUIControl;
    constructor Create(Name: string);
    destructor Destroy; override;
    function AddChild(Child: TGUIControl): TGUIControl;
    procedure OnMessage(var Msg: TMessage);
    procedure Update;
    procedure Draw;
    procedure SetActive(Control: TGUIControl);
    function GetControl(Name: string): TGUIControl;
    property OnKeyDown: TOnKeyDownEvent read FOnKeyDown write FOnKeyDown;
    property OnClose: TOnCloseEvent read FOnCloseEvent write FOnCloseEvent;
    property OnShow: TOnShowEvent read FOnShowEvent write FOnShowEvent;
    property Name: string read FName;
    property DefControl: string read FDefControl write FDefControl;
    property BackTexture: string read FBackTexture write FBackTexture;
    property MainWindow: Boolean read FMainWindow write FMainWindow;
  end;

  TGUITextButton = class(TGUIControl)
  private
    FText: string;
    FColor: TRGB;
    FFont: TFont;
    FSound: string;
    FShowWindow: string;
  public
    Proc: procedure;
    constructor Create(Proc: Pointer; FontID: DWORD; Text: string);
    destructor Destroy(); override;
    procedure OnMessage(var Msg: TMessage); override;
    procedure Update(); override;
    procedure Draw(); override;
    function GetWidth(): Integer;
    function GetHeight(): Integer;
    procedure Click(Silent: Boolean = False);
    property Color: TRGB read FColor write FColor;
    property Font: TFont read FFont write FFont;
    property ShowWindow: string read FShowWindow write FShowWindow;
  end;

  TGUILabel = class(TGUIControl)
  private
    FText: string;
    FColor: TRGB;
    FFont: TFont;
    FFixedLen: Word;
    FOnClickEvent: TOnClickEvent;
  public
    constructor Create(Text: string; FontID: DWORD);
    procedure OnMessage(var Msg: TMessage); override;
    procedure Draw; override;
    function GetWidth: Integer;
    function GetHeight: Integer;
    property OnClick: TOnClickEvent read FOnClickEvent write FOnClickEvent;
    property FixedLength: Word read FFixedLen write FFixedLen;
    property Text: string read FText write FText;
    property Color: TRGB read FColor write FColor;
    property Font: TFont read FFont write FFont;
  end;

  TGUIScroll = class(TGUIControl)
  private
    FValue: Integer;
    FMax: Word;
    FLeftID: DWORD;
    FRightID: DWORD;
    FMiddleID: DWORD;
    FMarkerID: DWORD;
    FOnChangeEvent: TOnChangeEvent;
    procedure FSetValue(a: Integer);
  public
    constructor Create();
    procedure OnMessage(var Msg: TMessage); override;
    procedure Update; override;
    procedure Draw; override;
    function GetWidth(): Word;
    property OnChange: TOnChangeEvent read FOnChangeEvent write FOnChangeEvent;
    property Max: Word read FMax write FMax;
    property Value: Integer read FValue write FSetValue;
 end;

  TGUISwitch = class(TGUIControl)
  private
    FFont: TFont;
    FItems: array of string;
    FIndex: Integer;
    FColor: TRGB;
    FOnChangeEvent: TOnChangeEvent;
  public
    constructor Create(FontID: DWORD);
    procedure OnMessage(var Msg: TMessage); override;
    procedure AddItem(Item: string);
    procedure Update; override;
    procedure Draw; override;
    function GetWidth(): Word;
    function GetText: string;
    property ItemIndex: Integer read FIndex write FIndex;
    property Color: TRGB read FColor write FColor;
    property Font: TFont read FFont write FFont;
    property OnChange: TOnChangeEvent read FOnChangeEvent write FOnChangeEvent;
 end;

  TGUIEdit = class(TGUIControl)
  private
    FFont: TFont;
    FCaretPos: Integer;
    FMaxLength: Word;
    FWidth: Word;
    FText: string;
    FColor: TRGB;
    FOnlyDigits: Boolean;
    FLeftID: DWORD;
    FRightID: DWORD;
    FMiddleID: DWORD;
    FOnChangeEvent: TOnChangeEvent;
    FOnEnterEvent: TOnEnterEvent;
    procedure SetText(Text: string);
  public
    constructor Create(FontID: DWORD);
    procedure OnMessage(var Msg: TMessage); override;
    procedure Update; override;
    procedure Draw; override;
    function GetWidth(): Word;
    property OnChange: TOnChangeEvent read FOnChangeEvent write FOnChangeEvent;
    property OnEnter: TOnEnterEvent read FOnEnterEvent write FOnEnterEvent;
    property Width: Word read FWidth write FWidth;
    property MaxLength: Word read FMaxLength write FMaxLength;
    property OnlyDigits: Boolean read FOnlyDigits write FOnlyDigits;
    property Text: string read FText write SetText;
    property Color: TRGB read FColor write FColor;
    property Font: TFont read FFont write FFont;
  end;

  TGUIKeyRead = class(TGUIControl)
  private
    FFont: TFont;
    FColor: TRGB;
    FKey: Byte;
    FIsQuery: Boolean;
  public
    constructor Create(FontID: DWORD);
    procedure OnMessage(var Msg: TMessage); override;
    procedure Draw; override;
    function GetWidth(): Word;
    property Key: Byte read FKey write FKey;
    property Color: TRGB read FColor write FColor;
    property Font: TFont read FFont write FFont;
  end;

  TGUIModelView = class(TGUIControl)
  private
    FModel: TPlayerModel;
    a: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnMessage(var Msg: TMessage); override;
    procedure SetModel(ModelName: string);
    procedure SetColor(Red, Green, Blue: Byte);
    procedure NextAnim();
    procedure NextWeapon();
    procedure Update; override;
    procedure Draw; override;
    property  Model: TPlayerModel read FModel;
  end;

  TPreviewPanel = record
    X1, Y1, X2, Y2: Integer;
    PanelType: Word;
  end;

  TGUIMapPreview = class(TGUIControl)
  private
    FMapData: array of TPreviewPanel;
    FMapSize: TPoint;
    FScale: Single;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure OnMessage(var Msg: TMessage); override;
    procedure SetMap(Res: string);
    procedure ClearMap();
    procedure Update(); override;
    procedure Draw(); override;
    function GetScaleStr: String;
  end;

  TGUIImage = class(TGUIControl)
  private
    FImageRes: string;
    FDefaultRes: string;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure OnMessage(var Msg: TMessage); override;
    procedure SetImage(Res: string);
    procedure ClearImage();
    procedure Update(); override;
    procedure Draw(); override;
    property DefaultRes: string read FDefaultRes write FDefaultRes;
  end;

  TGUIListBox = class(TGUIControl)
  private
    FItems: SArray;
    FActiveColor: TRGB;
    FUnActiveColor: TRGB;
    FFont: TFont;
    FStartLine: Integer;
    FIndex: Integer;
    FWidth: Word;
    FHeight: Word;
    FSort: Boolean;
    FDrawBack: Boolean;
    FDrawScroll: Boolean;
    FOnChangeEvent: TOnChangeEvent;

    procedure FSetItems(Items: SArray);
    procedure FSetIndex(aIndex: Integer);
   
  public
    constructor Create(FontID: DWORD; Width, Height: Word);
    procedure OnMessage(var Msg: TMessage); override;
    procedure Draw(); override;
    procedure AddItem(Item: String);
    procedure SelectItem(Item: String);
    procedure Clear();
    function  GetWidth(): Word;
    function  GetHeight(): Word;
    function  SelectedItem(): String;

    property OnChange: TOnChangeEvent read FOnChangeEvent write FOnChangeEvent;
    property Sort: Boolean read FSort write FSort;
    property ItemIndex: Integer read FIndex write FSetIndex;
    property Items: SArray read FItems write FSetItems;
    property DrawBack: Boolean read FDrawBack write FDrawBack;
    property DrawScrollBar: Boolean read FDrawScroll write FDrawScroll;
    property ActiveColor: TRGB read FActiveColor write FActiveColor;
    property UnActiveColor: TRGB read FUnActiveColor write FUnActiveColor;
    property Font: TFont read FFont write FFont;
  end;

  TGUIFileListBox = class (TGUIListBox)
  private
    FBasePath: String;
    FPath: String;
    FFileMask: String;
    FDirs: Boolean;

    procedure OpenDir(path: String);

  public
    procedure OnMessage(var Msg: TMessage); override;
    procedure SetBase(path: String);
    function  SelectedItem(): String;
    procedure UpdateFileList();

    property Dirs: Boolean read FDirs write FDirs;
    property FileMask: String read FFileMask write FFileMask;
    property Path: String read FPath;
  end;

  TGUIMemo = class(TGUIControl)
  private
    FLines: SArray;
    FFont: TFont;
    FStartLine: Integer;
    FWidth: Word;
    FHeight: Word;
    FColor: TRGB;
    FDrawBack: Boolean;
    FDrawScroll: Boolean;
  public
    constructor Create(FontID: DWORD; Width, Height: Word);
    procedure OnMessage(var Msg: TMessage); override;
    procedure Draw; override;
    procedure Clear;
    function GetWidth(): Word;
    function GetHeight(): Word;
    procedure SetText(Text: string);
    property DrawBack: Boolean read FDrawBack write FDrawBack;
    property DrawScrollBar: Boolean read FDrawScroll write FDrawScroll;
    property Color: TRGB read FColor write FColor;
    property Font: TFont read FFont write FFont;
  end;

  TGUIMainMenu = class(TGUIControl)
  private
    FButtons: array of TGUITextButton;
    FHeader: TGUILabel;
    FIndex: Integer;
    FFontID: DWORD;
    FCounter: Byte;
    FMarkerID1: DWORD;
    FMarkerID2: DWORD;
  public
    constructor Create(FontID: DWORD; Header: string);
    destructor Destroy; override;
    procedure OnMessage(var Msg: TMessage); override;
    function AddButton(fProc: Pointer; Caption: string; ShowWindow: string = ''): TGUITextButton;
    function GetButton(Name: string): TGUITextButton;
    procedure EnableButton(Name: string; e: Boolean);
    procedure AddSpace();
    procedure Update; override;
    procedure Draw; override;
  end;

  TControlType = class of TGUIControl;

  PMenuItem = ^TMenuItem;
  TMenuItem = record
    Text: TGUILabel;
    ControlType: TControlType;
    Control: TGUIControl;
  end;

  TGUIMenu = class(TGUIControl)
  private
    FItems: array of TMenuItem;
    FHeader: TGUILabel;
    FIndex: Integer;
    FFontID: DWORD;
    FCounter: Byte;
    FAlign: Boolean;
    FLeft: Integer;
    function NewItem(): Integer;
  public
    constructor Create(HeaderFont, ItemsFont: DWORD; Header: string);
    destructor Destroy; override;
    procedure OnMessage(var Msg: TMessage); override;
    procedure AddSpace();
    procedure AddLine(fText: string);
    procedure AddText(fText: string; MaxWidth: Word);
    function AddLabel(fText: string): TGUILabel;
    function AddButton(Proc: Pointer; fText: string; _ShowWindow: string = ''): TGUITextButton;
    function AddScroll(fText: string): TGUIScroll;
    function AddSwitch(fText: string): TGUISwitch;
    function AddEdit(fText: string): TGUIEdit;
    function AddKeyRead(fText: string): TGUIKeyRead;
    function AddList(fText: string; Width, Height: Word): TGUIListBox;
    function AddFileList(fText: string; Width, Height: Word): TGUIFileListBox;
    function AddMemo(fText: string; Width, Height: Word): TGUIMemo;
    procedure ReAlign();
    function GetControl(Name: string): TGUIControl;
    function GetControlsText(Name: string): TGUILabel;
    procedure Draw; override;
    procedure Update; override;
    procedure UpdateIndex();
    property Align: Boolean read FAlign write FAlign;
    property Left: Integer read FLeft write FLeft;
  end;

var
  KEYTABLE: array[0..255] of string = (
    '', 'ESC', '1', '2', '3', '4', '5', '6',  // 0
    '7', '8', '9', '0', '-', '=', 'BackSpace', 'Tab',  // 8
    'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I',  // 16
    'O', 'P', '[', ']', 'Enter', 'LCtrl', 'A', 'S',  // 24
    'D', 'F', 'G', 'H', 'J', 'K', 'L', ';',  // 32
    '', '', 'LShift', '\', 'Z', 'X', 'C', 'V',  // 40
    'B', 'N', 'M', ',', '.', '/', 'RShift', 'Num *',  // 48
    'LAlt', 'Space', 'Caps Lock', 'F1', 'F2', 'F3', 'F4', 'F5',  // 56
    'F6', 'F7', 'F8', 'F9', 'F10', '', 'Scroll Lock', 'Num 7',  // 64
    'Num 8', 'Num 9', 'Num -', 'Num 4', 'Num 5', 'Num 6', 'Num +', 'Num 1',  // 72
    'Num 2', 'Num 3', 'Num 0', 'Num .', '', '', '', 'F11',  // 80
    'F12', '', '', '', '', '', '', '',  // 88
    '', '', '', '', '', '', '', '',  // 96
    '', '', '', '', '', '', '', 'Num /',  // 104
    '', '', '', '', '', '', '', '',  // 112
    '', '', '', '', '', '', '', '',  // 120
    '', '', '', '', '', '', '', '',  // 128
    '', '', '', '', '', '', '', '',  // 136
    '', '', '', '', '', '', '', '',  // 144
    '', '', '', '', 'Num Enter', 'RCtrl', '', '',  // 152
    '', '', '', '', '', '', '', '',  // 160
    '', '', '', '', '', '', '', '',  // 168
    '', '', '', '', '', '', '', 'Print Screen',  // 176
    'RAlt', '', '', '', '', '', '', '',  // 184
    '', '', '', '', '', '', '', 'Home',  // 192
    'Up', 'Page Up', '', 'Left', '', 'Right', '', 'End',  // 200
    'Down', 'Page Down', 'Insert', 'Delete', '', '', '', '',  // 208
    '', '', '', '', '', '', '', '',  // 216
    '', '', '', '', '', '', '', '',  // 224
    '', '', '', '', '', '', '', '',  // 232
    '', '', '', '', '', '', '', '',  // 240
    '', '', '', '', '', '', '', ''); // 248

var
  g_GUIWindows: array of TGUIWindow;
  g_ActiveWindow: TGUIWindow = nil;

procedure g_GUI_Init();
function  g_GUI_AddWindow(Window: TGUIWindow): TGUIWindow;
function  g_GUI_GetWindow(Name: string): TGUIWindow;
procedure g_GUI_ShowWindow(Name: string);
procedure g_GUI_HideWindow(PlaySound: Boolean = True);
function  g_GUI_Destroy(): Boolean;
procedure g_GUI_SaveMenuPos();
procedure g_GUI_LoadMenuPos();

implementation

uses
  dglOpenGL, g_textures, g_sound, Messages, SysUtils,
  g_game, Math, StrUtils, g_player, g_options, MAPREADER,
  g_map, MAPDEF, g_weapons;

var
  Box: Array [0..8] of DWORD;
  Saved_Windows: SArray;

procedure g_GUI_Init();
begin
  g_Texture_Get(BOX1, Box[0]);
  g_Texture_Get(BOX2, Box[1]);
  g_Texture_Get(BOX3, Box[2]);
  g_Texture_Get(BOX4, Box[3]);
  g_Texture_Get(BOX5, Box[4]);
  g_Texture_Get(BOX6, Box[5]);
  g_Texture_Get(BOX7, Box[6]);
  g_Texture_Get(BOX8, Box[7]);
  g_Texture_Get(BOX9, Box[8]);
end;

function g_GUI_Destroy(): Boolean;
var
  i: Integer;
begin
  Result := (Length(g_GUIWindows) > 0);

  for i := 0 to High(g_GUIWindows) do
    g_GUIWindows[i].Free();

  g_GUIWindows := nil;
  g_ActiveWindow := nil;
end;

function g_GUI_AddWindow(Window: TGUIWindow): TGUIWindow;
begin
  SetLength(g_GUIWindows, Length(g_GUIWindows)+1);
  g_GUIWindows[High(g_GUIWindows)] := Window;

  Result := Window;
end;

function g_GUI_GetWindow(Name: string): TGUIWindow;
var
  i: Integer;
begin
  Result := nil;

  if g_GUIWindows <> nil then
    for i := 0 to High(g_GUIWindows) do
      if g_GUIWindows[i].FName = Name then
      begin
        Result := g_GUIWindows[i];
        Break;
      end;

  Assert(Result <> nil, 'GUI_Window "'+Name+'" not found');
end;

procedure g_GUI_ShowWindow(Name: string);
var
  i: Integer;
begin
  if g_GUIWindows = nil then
    Exit;

  for i := 0 to High(g_GUIWindows) do
    if g_GUIWindows[i].FName = Name then
    begin
      g_GUIWindows[i].FPrevWindow := g_ActiveWindow;
      g_ActiveWindow := g_GUIWindows[i];

      if g_ActiveWindow.MainWindow then
        g_ActiveWindow.FPrevWindow := nil;

      if g_ActiveWindow.FDefControl <> '' then
        g_ActiveWindow.SetActive(g_ActiveWindow.GetControl(g_ActiveWindow.FDefControl))
      else
        g_ActiveWindow.SetActive(nil);

      if @g_ActiveWindow.FOnShowEvent <> nil then
        g_ActiveWindow.FOnShowEvent();

      Break;
    end;
end;

procedure g_GUI_HideWindow(PlaySound: Boolean = True);
begin
  if g_ActiveWindow <> nil then
  begin
    if @g_ActiveWindow.OnClose <> nil then
      g_ActiveWindow.OnClose();
    g_ActiveWindow := g_ActiveWindow.FPrevWindow;
    if PlaySound then
      g_Sound_PlayEx(WINDOW_CLOSESOUND);
  end;
end;

procedure g_GUI_SaveMenuPos();
var
  len: Integer;
  win: TGUIWindow;
begin
  SetLength(Saved_Windows, 0);
  win := g_ActiveWindow;

  while win <> nil do
  begin
    len := Length(Saved_Windows);
    SetLength(Saved_Windows, len + 1);

    Saved_Windows[len] := win.Name;

    if win.MainWindow then
      win := nil
    else
      win := win.FPrevWindow;
  end;
end;

procedure g_GUI_LoadMenuPos();
var
  i, j, k, len: Integer;
  ok: Boolean;
begin
  g_ActiveWindow := nil;
  len := Length(Saved_Windows);

  if len = 0 then
    Exit;

// Окно с главным меню:
  g_GUI_ShowWindow(Saved_Windows[len-1]);

// Не переключилось (или некуда дальше):
  if (len = 1) or (g_ActiveWindow = nil) then
    Exit;

// Ищем кнопки в остальных окнах:
  for k := len-1 downto 1 do
  begin
    ok := False;
    
    for i := 0 to Length(g_ActiveWindow.Childs)-1 do
    begin
      if g_ActiveWindow.Childs[i] is TGUIMainMenu then
        begin // GUI_MainMenu
          with TGUIMainMenu(g_ActiveWindow.Childs[i]) do
            for j := 0 to Length(FButtons)-1 do
              if FButtons[j].ShowWindow = Saved_Windows[k-1] then
              begin
                FButtons[j].Click(True);
                ok := True;
                Break;
              end;
        end
      else // GUI_Menu
        if g_ActiveWindow.Childs[i] is TGUIMenu then
          with TGUIMenu(g_ActiveWindow.Childs[i]) do
            for j := 0 to Length(FItems)-1 do
              if FItems[j].ControlType = TGUITextButton then
                if TGUITextButton(FItems[j].Control).ShowWindow = Saved_Windows[k-1] then
                begin
                  TGUITextButton(FItems[j].Control).Click(True);
                  ok := True;
                  Break;
                end;

      if ok then
        Break;
    end;

  // Не переключилось:
    if (not ok) or
       (g_ActiveWindow.Name = Saved_Windows[k]) then
      Break;
  end;
end;

procedure DrawBox(X, Y: Integer; Width, Height: Word);
begin
  e_Draw(Box[0], X, Y, 0, False, False);
  e_DrawFill(Box[1], X+4, Y, Width*4, 1, 0, False, False);
  e_Draw(Box[2], X+4+Width*16, Y, 0, False, False);
  e_DrawFill(Box[3], X, Y+4, 1, Height*4, 0, False, False);
  e_DrawFill(Box[4], X+4, Y+4, Width, Height, 0, False, False);
  e_DrawFill(Box[5], X+4+Width*16, Y+4, 1, Height*4, 0, False, False);
  e_Draw(Box[6], X, Y+4+Height*16, 0, False, False);
  e_DrawFill(Box[7], X+4, Y+4+Height*16, Width*4, 1, 0, False, False);
  e_Draw(Box[8], X+4+Width*16, Y+4+Height*16, 0, False, False);
end;

procedure DrawScroll(X, Y: Integer; Height: Word; Up, Down: Boolean);
var
  ID: DWORD;
begin
  if Height < 3 then Exit;

  if Up then
    g_Texture_Get(BSCROLL_UPA, ID)
  else
    g_Texture_Get(BSCROLL_UPU, ID);
  e_Draw(ID, X, Y, 0, False, False);

  if Down then
    g_Texture_Get(BSCROLL_DOWNA, ID)
  else
    g_Texture_Get(BSCROLL_DOWNU, ID);
  e_Draw(ID, X, Y+(Height-1)*16, 0, False, False);

  g_Texture_Get(BSCROLL_MIDDLE, ID);
  e_DrawFill(ID, X, Y+16, 1, Height-2, 0, False, False);
end;

{ TGUIWindow }

constructor TGUIWindow.Create(Name: string);
begin
  Childs := nil;
  FActiveControl := nil;
  FName := Name;
  FOnKeyDown := nil;
  FOnCloseEvent := nil;
  FOnShowEvent := nil;
end;

destructor TGUIWindow.Destroy;
var
  i: Integer;
begin
  if Childs = nil then
    Exit;

  for i := 0 to High(Childs) do
    Childs[i].Free();
end;

function TGUIWindow.AddChild(Child: TGUIControl): TGUIControl;
begin
  Child.FWindow := Self;

  SetLength(Childs, Length(Childs) + 1);
  Childs[High(Childs)] := Child;

  Result := Child;
end;

procedure TGUIWindow.Update;
var
  i: Integer;
begin
  for i := 0 to High(Childs) do
    if Childs[i] <> nil then Childs[i].Update;
end;

procedure TGUIWindow.Draw;
var
  i: Integer;
  ID: DWORD;
begin
  if FBackTexture <> '' then
    if g_Texture_Get(FBackTexture, ID) then
      e_DrawSize(ID, 0, 0, 0, False, False, gScreenWidth, gScreenHeight)
    else
      e_Clear(GL_COLOR_BUFFER_BIT, 0.5, 0.5, 0.5);

  for i := 0 to High(Childs) do
    if Childs[i] <> nil then Childs[i].Draw;
end;

procedure TGUIWindow.OnMessage(var Msg: TMessage);
begin
  if FActiveControl <> nil then FActiveControl.OnMessage(Msg);
  if @FOnKeyDown <> nil then FOnKeyDown(Msg.wParam);

  if Msg.Msg = WM_KEYDOWN then
    if Msg.wParam = VK_ESCAPE then
    begin
      g_GUI_HideWindow;
      Exit;
    end;
end;

procedure TGUIWindow.SetActive(Control: TGUIControl);
begin
  FActiveControl := Control;
end;

function TGUIWindow.GetControl(Name: String): TGUIControl;
var
  i: Integer;
begin
  Result := nil;

  if Childs <> nil then
    for i := 0 to High(Childs) do
      if Childs[i] <> nil then
        if LowerCase(Childs[i].FName) = LowerCase(Name) then
        begin
          Result := Childs[i];
          Break;
        end;

  Assert(Result <> nil, 'Window Control "'+Name+'" not Found!');
end;

{ TGUIControl }

constructor TGUIControl.Create();
begin
  FX := 0;
  FY := 0;

  FEnabled := True;
end;

procedure TGUIControl.OnMessage(var Msg: TMessage);
begin
  if not FEnabled then
    Exit;
end;

procedure TGUIControl.Update();
begin

end;

procedure TGUIControl.Draw();
begin

end;

{ TGUITextButton }

procedure TGUITextButton.Click(Silent: Boolean = False);
begin
  if (FSound <> '') and (not Silent) then
    g_Sound_PlayEx(FSound);

  if @Proc <> nil then
    Proc();
  if FShowWindow <> '' then
    g_GUI_ShowWindow(FShowWindow);
end;

constructor TGUITextButton.Create(Proc: Pointer; FontID: DWORD; Text: string);
begin
  inherited Create();

  Self.Proc := Proc;

  FFont := TFont.Create(FontID, FONT_CHAR);

  FText := Text;
end;

destructor TGUITextButton.Destroy;
begin

 inherited;
end;

procedure TGUITextButton.Draw;
begin
  FFont.Draw(FX, FY, FText, FColor.R, FColor.G, FColor.B)
end;

function TGUITextButton.GetHeight: Integer;
var
  w, h: Word;
begin
  FFont.GetTextSize(FText, w, h);
  Result := h;
end;

function TGUITextButton.GetWidth: Integer;
var
  w, h: Word;
begin
  FFont.GetTextSize(FText, w, h);
  Result := w;
end;

procedure TGUITextButton.OnMessage(var Msg: TMessage);
begin
  if not FEnabled then Exit;

  inherited;

  case Msg.Msg of
    WM_KEYDOWN:
      case Msg.wParam of
        VK_RETURN: Click();
      end;
  end;
end;

procedure TGUITextButton.Update;
begin
  inherited;
end;

{ TFont }

constructor TFont.Create(FontID: DWORD; FontType: TFontType);
begin
  ID := FontID;

  FScale := 1;
  FFontType := FontType;
end;

destructor TFont.Destroy;
begin

  inherited;
end;

procedure TFont.Draw(X, Y: Integer; Text: string; R, G, B: Byte);
begin
  if FFontType = FONT_CHAR then e_CharFont_PrintEx(ID, X, Y, Text, _RGB(R, G, B), FScale)
  else e_TextureFontPrintEx(X, Y, Text, ID, R, G, B, FScale); 
end;

procedure TFont.GetTextSize(Text: string; var w, h: Word);
var
  cw, ch: Byte;
begin
  if FFontType = FONT_CHAR then e_CharFont_GetSize(ID, Text, w, h)
  else
  begin
    e_TextureFontGetSize(ID, cw, ch);
    w := cw*Length(Text);
    h := ch;
  end;

  w := Round(w*FScale);
  h := Round(h*FScale);
end;

{ TGUIMainMenu }

function TGUIMainMenu.AddButton(fProc: Pointer; Caption: string; ShowWindow: string = ''): TGUITextButton;
var
  a, _x: Integer;
  h, hh: Word;
begin
  FIndex := 0;

  SetLength(FButtons, Length(FButtons)+1);
  FButtons[High(FButtons)] := TGUITextButton.Create(fProc, FFontID, Caption);
  FButtons[High(FButtons)].ShowWindow := ShowWindow;
  with FButtons[High(FButtons)] do
  begin
    if (fProc <> nil) or (ShowWindow <> '') then FColor := MAINMENU_ITEMS_COLOR
    else FColor := MAINMENU_UNACTIVEITEMS_COLOR;
    FSound := MAINMENU_CLICKSOUND;
  end;

  _x := gScreenWidth div 2;

  for a := 0 to High(FButtons) do
    if FButtons[a] <> nil then
      _x := Min(_x, (gScreenWidth div 2)-(FButtons[a].GetWidth div 2));

  hh := FHeader.GetHeight;

  h := hh*(2+Length(FButtons))+MAINMENU_SPACE*(Length(FButtons)-1);
  h := (gScreenHeight div 2)-(h div 2);

  with FHeader do
  begin
    FX := _x;
    FY := h;
  end;

  Inc(h, hh*2);

  for a := 0 to High(FButtons) do
  begin
    if FButtons[a] <> nil then
    with FButtons[a] do
    begin
      FX := _x;
      FY := h;
    end;

    Inc(h, hh+MAINMENU_SPACE);
  end;

  Result := FButtons[High(FButtons)];
end;

procedure TGUIMainMenu.AddSpace;
begin
  SetLength(FButtons, Length(FButtons)+1);
  FButtons[High(FButtons)] := nil;
end;

constructor TGUIMainMenu.Create(FontID: DWORD; Header: string);
begin
  inherited Create();

  FIndex := -1;
  FFontID := FontID;
  FCounter := MAINMENU_MARKERDELAY;

  g_Texture_Get(MAINMENU_MARKER1, FMarkerID1);
  g_Texture_Get(MAINMENU_MARKER2, FMarkerID2); 

  FHeader := TGUILabel.Create(Header, FFontID);
  with FHeader do
  begin
    FColor := MAINMENU_HEADER_COLOR;
    FX := (gScreenWidth div 2)-(GetWidth div 2);
    FY := (gScreenHeight div 2)-(GetHeight div 2);
  end;
end;

destructor TGUIMainMenu.Destroy;
var
  a: Integer;
begin
  if FButtons <> nil then
    for a := 0 to High(FButtons) do
      FButtons[a].Free();

  FHeader.Free();

  inherited;
end;

procedure TGUIMainMenu.Draw;
var
  a: Integer;
begin
  inherited;

  FHeader.Draw;

  if FButtons <> nil then
  begin
    for a := 0 to High(FButtons) do
      if FButtons[a] <> nil then FButtons[a].Draw;

    if FIndex <> -1 then
      e_Draw(FMarkerID1, FButtons[FIndex].FX-48, FButtons[FIndex].FY, 0, True, False); 
  end;
end;

procedure TGUIMainMenu.EnableButton(Name: string; e: Boolean);
var
  a: Integer;
begin
  if FButtons = nil then Exit;

  for a := 0 to High(FButtons) do
    if (FButtons[a] <> nil) and (FButtons[a].Name = Name) then
    begin
      if e then FButtons[a].FColor := MAINMENU_ITEMS_COLOR
      else FButtons[a].FColor := MAINMENU_UNACTIVEITEMS_COLOR;
      FButtons[a].Enabled := e;
      Break;
    end;
end;

function TGUIMainMenu.GetButton(Name: string): TGUITextButton;
var
  a: Integer;
begin
  Result := nil;

  if FButtons = nil then Exit;

  for a := 0 to High(FButtons) do
    if (FButtons[a] <> nil) and (FButtons[a].Name = Name) then
    begin
      Result := FButtons[a];
      Break;
    end;
end;

procedure TGUIMainMenu.OnMessage(var Msg: TMessage);
var
  ok: Boolean;
  a: Integer;
begin
  if not FEnabled then Exit;

  inherited;

  if FButtons = nil then Exit;

  ok := False;
  for a := 0 to High(FButtons) do
    if FButtons[a] <> nil then
    begin
      ok := True;
      Break;
    end;

  if not ok then Exit;

  case Msg.Msg of
    WM_KEYDOWN:
      case Msg.wParam of
        VK_UP:
        begin
          repeat
            Dec(FIndex);
            if FIndex < 0 then FIndex := High(FButtons);
          until FButtons[FIndex] <> nil;

          g_Sound_PlayEx(MENU_CHANGESOUND);
        end;
        VK_DOWN:
        begin
          repeat
            Inc(FIndex);
            if FIndex > High(FButtons) then FIndex := 0;
          until FButtons[FIndex] <> nil;

          g_Sound_PlayEx(MENU_CHANGESOUND);
        end;
        VK_RETURN: if (FIndex <> -1) and FButtons[FIndex].FEnabled then FButtons[FIndex].Click;
      end;
  end;
end;

procedure TGUIMainMenu.Update;
var
  t: DWORD;
begin
  inherited;

  if FCounter = 0 then
  begin
    t := FMarkerID1;
    FMarkerID1 := FMarkerID2;
    FMarkerID2 := t;

    FCounter := MAINMENU_MARKERDELAY;
  end else Dec(FCounter);
end;

{ TGUILabel }

constructor TGUILabel.Create(Text: string; FontID: DWORD);
begin
  inherited Create();

  FFont := TFont.Create(FontID, FONT_CHAR); 

  FText := Text;
  FFixedLen := 0;
  FOnClickEvent := nil;
end;

procedure TGUILabel.Draw;
begin
  FFont.Draw(FX, FY, FText, FColor.R, FColor.G, FColor.B);
end;

function TGUILabel.GetHeight: Integer;
var
  w, h: Word;
begin
  FFont.GetTextSize(FText, w, h);
  Result := h;
end;

function TGUILabel.GetWidth: Integer;
var
  w, h: Word;
begin
  if FFixedLen = 0 then
    FFont.GetTextSize(FText, w, h)
  else
    w := e_CharFont_GetMaxWidth(FFont.ID)*FFixedLen; 
  Result := w; 
end;

procedure TGUILabel.OnMessage(var Msg: TMessage);
begin
  if not FEnabled then Exit;

  inherited;

  case Msg.Msg of
    WM_KEYDOWN:
      case Msg.wParam of
        VK_RETURN: if @FOnClickEvent <> nil then FOnClickEvent();
      end;
  end;
end;

{ TGUIMenu }

function TGUIMenu.AddButton(Proc: Pointer; fText: string; _ShowWindow: string = ''): TGUITextButton;
var
  i: Integer;
begin
  i := NewItem();
  with FItems[i] do
  begin
    Control := TGUITextButton.Create(Proc, FFontID, fText);
    with Control as TGUITextButton  do
    begin
      ShowWindow := _ShowWindow;
      FColor := MENU_ITEMSCTRL_COLOR;
    end;

    Text := nil;
    ControlType := TGUITextButton;

    Result := (Control as TGUITextButton);
  end;

  if FIndex = -1 then FIndex := i;

  ReAlign();
end;

procedure TGUIMenu.AddLine(fText: string);
begin
  with FItems[NewItem()] do
  begin
    Text := TGUILabel.Create(fText, FFontID);
    with Text do
    begin
      FColor := MENU_ITEMSTEXT_COLOR;
    end;

    Control := nil;
  end;

  ReAlign();
end;

procedure TGUIMenu.AddText(fText: string; MaxWidth: Word);
var
  a: Integer;
  l: SArray;
begin
  l := GetLines(fText, FFontID, MaxWidth);

  if l = nil then Exit;

  for a := 0 to High(l) do
  begin
    with FItems[NewItem()] do
    begin
      Text := TGUILabel.Create(l[a], FFontID);
      with Text do
      begin
        FColor := MENU_ITEMSTEXT_COLOR;
      end;

      Control := nil;
    end;
  end;

  ReAlign();
end;

procedure TGUIMenu.AddSpace;
begin
  with FItems[NewItem()] do
  begin
    Text := nil;
    Control := nil;
  end;

  ReAlign();
end;

constructor TGUIMenu.Create(HeaderFont, ItemsFont: DWORD; Header: string);
begin
  inherited Create();

  FItems := nil;
  FIndex := -1;
  FFontID := ItemsFont;
  FCounter := MENU_MARKERDELAY;
  FAlign := True;

  FHeader := TGUILabel.Create(Header, HeaderFont);
  with FHeader do
  begin
    FX := (gScreenWidth div 2)-(GetWidth div 2);
    FY := 0;
    FColor := MAINMENU_HEADER_COLOR;
  end;
end;

destructor TGUIMenu.Destroy;
var
  a: Integer;
begin
  if FItems <> nil then
    for a := 0 to High(FItems) do
      with FItems[a] do
      begin
        Text.Free();
        Control.Free();
      end;

  FItems := nil;

  FHeader.Free();

  inherited;
end;

procedure TGUIMenu.Draw;
var
  a, x, y: Integer;
begin
  inherited;

  if FHeader <> nil then FHeader.Draw;

  if FItems <> nil then
    for a := 0 to High(FItems) do
    begin
      if FItems[a].Text <> nil then FItems[a].Text.Draw;
      if FItems[a].Control <> nil then FItems[a].Control.Draw;
    end;

  if (FIndex <> -1) and (FCounter > MENU_MARKERDELAY div 2) then
  begin
    x := 0;
    y := 0;

    if FItems[FIndex].Text <> nil then
    begin
      x := FItems[FIndex].Text.FX;
      y := FItems[FIndex].Text.FY;
    end
    else if FItems[FIndex].Control <> nil then
    begin
      x := FItems[FIndex].Control.FX;
      y := FItems[FIndex].Control.FY;
    end;

    x := x-e_CharFont_GetMaxWidth(FFontID); 

    e_CharFont_PrintEx(FFontID, x, y, #16, _RGB(255, 0, 0)); 
  end;
end;

function TGUIMenu.GetControl(Name: String): TGUIControl;
var
  a: Integer;
begin
  Result := nil;

  if FItems <> nil then
    for a := 0 to High(FItems) do
      if FItems[a].Control <> nil then
        if LowerCase(FItems[a].Control.Name) = LowerCase(Name) then
        begin
          Result := FItems[a].Control;
          Break;
        end;

  Assert(Result <> nil, 'GUI control "'+Name+'" not found!');
end;

function TGUIMenu.GetControlsText(Name: String): TGUILabel;
var
  a: Integer;
begin
  Result := nil;

  if FItems <> nil then
    for a := 0 to High(FItems) do
      if FItems[a].Control <> nil then
        if LowerCase(FItems[a].Control.Name) = LowerCase(Name) then
        begin
          Result := FItems[a].Text;
          Break;
        end;

  Assert(Result <> nil, 'GUI control''s text "'+Name+'" not found!');
end;

function TGUIMenu.NewItem: Integer;
begin
  SetLength(FItems, Length(FItems)+1);
  Result := High(FItems);
end;

procedure TGUIMenu.OnMessage(var Msg: TMessage);
var
  ok: Boolean;
  a, c: Integer;
begin
  if not FEnabled then Exit;

  inherited;

  if FItems = nil then Exit;

  ok := False;
  for a := 0 to High(FItems) do
    if FItems[a].Control <> nil then
    begin
      ok := True;
      Break;
    end;

  if not ok then Exit;

  case Msg.Msg of
    WM_KEYDOWN:
    begin
      case Msg.wParam of
        VK_UP:
        begin
          c := 0;
          repeat
            c := c+1;
            if c > Length(FItems) then
            begin
              FIndex := -1;
              Break;
            end;

            Dec(FIndex);
            if FIndex < 0 then FIndex := High(FItems);
          until (FItems[FIndex].Control <> nil) and
                (FItems[FIndex].Control.Enabled);

          FCounter := 0;

          g_Sound_PlayEx(MENU_CHANGESOUND);
        end;

        VK_DOWN:
        begin
          c := 0;
          repeat
            c := c+1;
            if c > Length(FItems) then
            begin
              FIndex := -1;
              Break;
            end;

            Inc(FIndex);
            if FIndex > High(FItems) then FIndex := 0;
          until (FItems[FIndex].Control <> nil) and
                (FItems[FIndex].Control.Enabled);

          FCounter := 0;

          g_Sound_PlayEx(MENU_CHANGESOUND);
        end;

        VK_LEFT, VK_RIGHT:
        begin
          if FIndex <> -1 then
            if FItems[FIndex].Control <> nil then
              FItems[FIndex].Control.OnMessage(Msg);
        end;
        VK_RETURN:
        begin
          if FIndex <> -1 then
            if FItems[FIndex].Control <> nil then
              FItems[FIndex].Control.OnMessage(Msg);

          g_Sound_PlayEx(MENU_CLICKSOUND);
        end;
      end;
    end;
  end;
end;

procedure TGUIMenu.ReAlign();
var
  a, tx, cx, w, h: Integer;
begin
  if FItems = nil then Exit;

  if not FAlign then tx := FLeft else
  begin
    tx := gScreenWidth;
    for a := 0 to High(FItems) do
    begin
      w := 0;
      if FItems[a].Text <> nil then w := FItems[a].Text.GetWidth;
      if FItems[a].Control <> nil then
      begin
        w := w+MENU_HSPACE;

        if FItems[a].ControlType = TGUILabel then
          w := w+(FItems[a].Control as TGUILabel).GetWidth
        else if FItems[a].ControlType = TGUITextButton then
          w := w+(FItems[a].Control as TGUITextButton).GetWidth
        else if FItems[a].ControlType = TGUIScroll then
          w := w+(FItems[a].Control as TGUIScroll).GetWidth
        else if FItems[a].ControlType = TGUISwitch then
          w := w+(FItems[a].Control as TGUISwitch).GetWidth
        else if FItems[a].ControlType = TGUIEdit then
          w := w+(FItems[a].Control as TGUIEdit).GetWidth
        else if FItems[a].ControlType = TGUIKeyRead then
          w := w+(FItems[a].Control as TGUIKeyRead).GetWidth
        else if (FItems[a].ControlType = TGUIListBox) then
          w := w+(FItems[a].Control as TGUIListBox).GetWidth
        else if (FItems[a].ControlType = TGUIFileListBox) then
          w := w+(FItems[a].Control as TGUIFileListBox).GetWidth
        else if FItems[a].ControlType = TGUIMemo then
          w := w+(FItems[a].Control as TGUIMemo).GetWidth;
      end;

      tx := Min(tx, (gScreenWidth div 2)-(w div 2));
    end;
  end;

  cx := 0;
  for a := 0 to High(FItems) do
    with FItems[a] do
    begin
      if (Text <> nil) and (Control = nil) then Continue;

      w := 0;
      if Text <> nil then w := tx+Text.GetWidth;

      if w > cx then cx := w;
    end;

  cx := cx+MENU_HSPACE;

  h := FHeader.GetHeight*2+MENU_VSPACE*(Length(FItems)-1);

  for a := 0 to High(FItems) do
    with FItems[a] do
    begin
      if (ControlType = TGUIListBox) or (ControlType = TGUIFileListBox) then
        h := h+(FItems[a].Control as TGUIListBox).GetHeight()
      else
        h := h+e_CharFont_GetMaxHeight(FFontID);
    end;

  h := (gScreenHeight div 2)-(h div 2);

  with FHeader do
  begin
    FX := (gScreenWidth div 2)-(GetWidth div 2);
    FY := h;

    Inc(h, GetHeight*2);
  end;

  for a := 0 to High(FItems) do
    with FItems[a] do
    begin
      if Text <> nil then
        with Text do
        begin
          FX := tx;
          FY := h;
        end;

        if Control <> nil then
          with Control do
            if Text <> nil then
            begin
              FX := cx;
              FY := h;
            end
            else
            begin
              FX := tx;
              FY := h;
            end;

        if (ControlType = TGUIListBox) or (ControlType = TGUIFileListBox) then
          Inc(h, (Control as TGUIListBox).GetHeight+MENU_VSPACE)
        else if ControlType = TGUIMemo then
          Inc(h, (Control as TGUIMemo).GetHeight+MENU_VSPACE)
        else
          Inc(h, e_CharFont_GetMaxHeight(FFontID)+MENU_VSPACE);
    end;
end;

function TGUIMenu.AddScroll(fText: string): TGUIScroll;
var
  i: Integer;
begin
  i := NewItem();
  with FItems[i] do
  begin
    Control := TGUIScroll.Create();

    Text := TGUILabel.Create(fText, FFontID);
    with Text do
    begin
      FColor := MENU_ITEMSTEXT_COLOR;
    end;

    ControlType := TGUIScroll;

    Result := (Control as TGUIScroll);
  end;

  if FIndex = -1 then FIndex := i;

  ReAlign();
end;

function TGUIMenu.AddSwitch(fText: string): TGUISwitch;
var
  i: Integer;
begin
  i := NewItem();
  with FItems[i] do
  begin
    Control := TGUISwitch.Create(FFontID);
   (Control as TGUISwitch).FColor := MENU_ITEMSCTRL_COLOR;

    Text := TGUILabel.Create(fText, FFontID);
    with Text do
    begin
      FColor := MENU_ITEMSTEXT_COLOR;
    end;

    ControlType := TGUISwitch;

    Result := (Control as TGUISwitch);
  end;

  if FIndex = -1 then FIndex := i;

  ReAlign();
end;

function TGUIMenu.AddEdit(fText: string): TGUIEdit;
var
  i: Integer;
begin
  i := NewItem();
  with FItems[i] do
  begin
    Control := TGUIEdit.Create(FFontID);
    with Control as TGUIEdit do
    begin
      FWindow := Self.FWindow;
      FColor := MENU_ITEMSCTRL_COLOR;
    end;

    if fText = '' then Text := nil else
    begin
      Text := TGUILabel.Create(fText, FFontID);
      Text.FColor := MENU_ITEMSTEXT_COLOR;
    end;

    ControlType := TGUIEdit;

    Result := (Control as TGUIEdit);
  end;

  if FIndex = -1 then FIndex := i;

  ReAlign();
end;

procedure TGUIMenu.Update;
var
  a: Integer;
begin
  inherited;

  if FCounter = 0 then FCounter := MENU_MARKERDELAY else Dec(FCounter);

  if FItems <> nil then
    for a := 0 to High(FItems) do
      if FItems[a].Control <> nil then
        (FItems[a].Control as FItems[a].ControlType).Update;
end;

function TGUIMenu.AddKeyRead(fText: string): TGUIKeyRead;
var
  i: Integer;
begin
  i := NewItem();
  with FItems[i] do
  begin
    Control := TGUIKeyRead.Create(FFontID);
    with Control as TGUIKeyRead do
    begin
      FWindow := Self.FWindow;
      FColor := MENU_ITEMSCTRL_COLOR;
    end;

    Text := TGUILabel.Create(fText, FFontID);
    with Text do
    begin
      FColor := MENU_ITEMSTEXT_COLOR;
    end;

    ControlType := TGUIKeyRead;

    Result := (Control as TGUIKeyRead);
  end;

  if FIndex = -1 then FIndex := i;

  ReAlign();
end;

function TGUIMenu.AddList(fText: string; Width, Height: Word): TGUIListBox;
var
  i: Integer;
begin
  i := NewItem();
  with FItems[i] do
  begin
    Control := TGUIListBox.Create(FFontID, Width, Height);
    with Control as TGUIListBox do
    begin
      FWindow := Self.FWindow;
      FActiveColor := MENU_ITEMSCTRL_COLOR;
      FUnActiveColor := MENU_ITEMSTEXT_COLOR;
    end;

    Text := TGUILabel.Create(fText, FFontID);
    with Text do
    begin
      FColor := MENU_ITEMSTEXT_COLOR;
    end;

    ControlType := TGUIListBox;

    Result := (Control as TGUIListBox);
  end;

  if FIndex = -1 then FIndex := i;

  ReAlign();
end;

function TGUIMenu.AddFileList(fText: string; Width, Height: Word): TGUIFileListBox;
var
  i: Integer;
begin
  i := NewItem();
  with FItems[i] do
  begin
    Control := TGUIFileListBox.Create(FFontID, Width, Height);
    with Control as TGUIFileListBox do
    begin
      FWindow := Self.FWindow;
      FActiveColor := MENU_ITEMSCTRL_COLOR;
      FUnActiveColor := MENU_ITEMSTEXT_COLOR;
    end;

    if fText = '' then Text := nil else
    begin
      Text := TGUILabel.Create(fText, FFontID);
      Text.FColor := MENU_ITEMSTEXT_COLOR;
    end;

    ControlType := TGUIFileListBox;

    Result := (Control as TGUIFileListBox);
  end;

  if FIndex = -1 then FIndex := i;

  ReAlign();
end;

function TGUIMenu.AddLabel(fText: string): TGUILabel;
var
  i: Integer;
begin
  i := NewItem();
  with FItems[i] do
  begin
    Control := TGUILabel.Create('', FFontID);
    with Control as TGUILabel do
    begin
      FWindow := Self.FWindow;
      FColor := MENU_ITEMSCTRL_COLOR;
    end;

    Text := TGUILabel.Create(fText, FFontID);
    with Text do
    begin
      FColor := MENU_ITEMSTEXT_COLOR;
    end;

    ControlType := TGUILabel;

    Result := (Control as TGUILabel);
  end;

  if FIndex = -1 then FIndex := i;

  ReAlign();
end;

function TGUIMenu.AddMemo(fText: string; Width, Height: Word): TGUIMemo;
var
  i: Integer;
begin
  i := NewItem();
  with FItems[i] do
  begin
    Control := TGUIMemo.Create(FFontID, Width, Height);
    with Control as TGUIMemo do
    begin
      FWindow := Self.FWindow;
      FColor := MENU_ITEMSTEXT_COLOR;
    end;

    if fText = '' then Text := nil else
    begin
      Text := TGUILabel.Create(fText, FFontID);
      Text.FColor := MENU_ITEMSTEXT_COLOR;
    end;

    ControlType := TGUIMemo;

    Result := (Control as TGUIMemo);
  end;

  if FIndex = -1 then FIndex := i;

  ReAlign();
end;

procedure TGUIMenu.UpdateIndex();
var
  res: Boolean;
begin
  res := True;

  while res do
  begin
    if (FIndex < 0) or (FIndex > High(FItems)) then
      begin
        FIndex := -1;
        res := False;
      end
    else
      if FItems[FIndex].Control.Enabled then
        res := False
      else
        Inc(FIndex);
  end;
end;

{ TGUIScroll }

constructor TGUIScroll.Create;
begin
  inherited Create();

  FMax := 0;
  FOnChangeEvent := nil;

  g_Texture_Get(SCROLL_LEFT, FLeftID);
  g_Texture_Get(SCROLL_RIGHT, FRightID);
  g_Texture_Get(SCROLL_MIDDLE, FMiddleID);
  g_Texture_Get(SCROLL_MARKER, FMarkerID);
end;

procedure TGUIScroll.Draw;
var
  a: Integer;
begin
  inherited;

  e_Draw(FLeftID, FX, FY, 0, True, False);
  e_Draw(FRightID, FX+8+(FMax+1)*8, FY, 0, True, False);

  for a := 0 to FMax do
    e_Draw(FMiddleID, FX+8+a*8, FY, 0, True, False);

  e_Draw(FMarkerID, FX+8+FValue*8, FY, 0, True, False);
end;

procedure TGUIScroll.FSetValue(a: Integer);
begin
  if a > FMax then FValue := FMax else FValue := a;
end;

function TGUIScroll.GetWidth: Word;
begin
  Result := 16+(FMax+1)*8;
end;

procedure TGUIScroll.OnMessage(var Msg: TMessage);
begin
  if not FEnabled then Exit;

  inherited;

  case Msg.Msg of
    WM_KEYDOWN:
    begin
      case Msg.wParam of
        VK_LEFT:
          if FValue > 0 then
          begin
            Dec(FValue);
            g_Sound_PlayEx(SCROLL_SUBSOUND);
            if @FOnChangeEvent <> nil then FOnChangeEvent(Self);
          end;
        VK_RIGHT:
          if FValue < FMax then
          begin
            Inc(FValue);
            g_Sound_PlayEx(SCROLL_ADDSOUND);
            if @FOnChangeEvent <> nil then FOnChangeEvent(Self);
          end;
      end;
    end;
  end;
end;

procedure TGUIScroll.Update;
begin
  inherited;

end;

{ TGUISwitch }

procedure TGUISwitch.AddItem(Item: string);
begin
  SetLength(FItems, Length(FItems)+1);
  FItems[High(FItems)] := Item;

  if FIndex = -1 then FIndex := 0;
end;

constructor TGUISwitch.Create(FontID: DWORD);
begin
  inherited Create();

  FIndex := -1;

  FFont := TFont.Create(FontID, FONT_CHAR);
end;

procedure TGUISwitch.Draw;
begin
  inherited;

  FFont.Draw(FX, FY, FItems[FIndex], FColor.R, FColor.G, FColor.B);
end;

function TGUISwitch.GetText: string;
begin
  if FIndex <> -1 then Result := FItems[FIndex]
  else Result := '';
end;

function TGUISwitch.GetWidth: Word;
var
  a: Integer;
  w, h: Word;
begin
  Result := 0;

  if FItems = nil then Exit;

  for a := 0 to High(FItems) do
  begin
    FFont.GetTextSize(FItems[a], w, h);
    if w > Result then Result := w;
  end;
end;

procedure TGUISwitch.OnMessage(var Msg: TMessage);
begin
  if not FEnabled then Exit;

  inherited;

  if FItems = nil then Exit;

  case Msg.Msg of
    WM_KEYDOWN:
      case Msg.wParam of
        VK_RETURN, VK_RIGHT:
        begin
          if FIndex < High(FItems) then
            Inc(FIndex)
          else
            FIndex := 0;

          if @FOnChangeEvent <> nil then
            FOnChangeEvent(Self);
        end;

    VK_LEFT:
      begin
        if FIndex > 0 then
          Dec(FIndex)
        else
          FIndex := High(FItems);

        if @FOnChangeEvent <> nil then
          FOnChangeEvent(Self);
      end;
    end;
  end;
end;

procedure TGUISwitch.Update;
begin
  inherited;

end;

{ TGUIEdit }

constructor TGUIEdit.Create(FontID: DWORD);
begin
  inherited Create();

  FFont := TFont.Create(FontID, FONT_CHAR);

  FMaxLength := 0;
  FWidth := 0;

  g_Texture_Get(EDIT_LEFT, FLeftID);
  g_Texture_Get(EDIT_RIGHT, FRightID);
  g_Texture_Get(EDIT_MIDDLE, FMiddleID);
end;

procedure TGUIEdit.Draw;
var
  c, w, h: Word;
begin
  inherited;

  e_Draw(FLeftID, FX, FY, 0, True, False);
  e_Draw(FRightID, FX+8+FWidth*16, FY, 0, True, False);

  for c := 0 to FWidth-1 do
    e_Draw(FMiddleID, FX+8+c*16, FY, 0, True, False);

  FFont.Draw(FX+8, FY, FText, FColor.R, FColor.G, FColor.B);

  if FWindow.FActiveControl = Self then
  begin
    FFont.GetTextSize(Copy(FText, 1, FCaretPos), w, h);
    h := e_CharFont_GetMaxHeight(FFont.ID);
    e_DrawLine(2, FX+8+w, FY+h-3, FX+8+w+EDIT_CURSORLEN, FY+h-3,
               EDIT_CURSORCOLOR.R, EDIT_CURSORCOLOR.G, EDIT_CURSORCOLOR.B);
  end;
end;

function TGUIEdit.GetWidth: Word;
begin
  Result := 16+FWidth*16;
end;

procedure TGUIEdit.OnMessage(var Msg: TMessage);
begin
  if not FEnabled then Exit;

  inherited;

  with Msg do
    case Msg of
      WM_CHAR:
        if FOnlyDigits then
        begin
          if (wParam in [48..57]) and (Chr(wParam) <> '`') then
            if Length(Text) < FMaxLength then
            begin
              Insert(Chr(wParam), FText, FCaretPos + 1);
              Inc(FCaretPos);
            end;
        end
        else
        begin
          if (wParam in [32..255]) and (Chr(wParam) <> '`') then
            if Length(Text) < FMaxLength then
            begin
              Insert(Chr(wParam), FText, FCaretPos + 1);
              Inc(FCaretPos);
            end;
        end;
      WM_KEYDOWN:
        case wParam of
          VK_BACK:
          begin
            Delete(FText, FCaretPos, 1);
            if FCaretPos > 0 then Dec(FCaretPos);
          end;
          VK_DELETE: Delete(FText, FCaretPos + 1, 1);
          VK_END: FCaretPos := Length(FText);
          VK_HOME: FCaretPos := 0;
          VK_LEFT: if FCaretPos > 0 then Dec(FCaretPos);
          VK_RIGHT: if FCaretPos < Length(FText) then Inc(FCaretPos);
          VK_RETURN:
            with FWindow do
            begin
              if FActiveControl <> Self then
              begin
                SetActive(Self);
                if @FOnEnterEvent <> nil then FOnEnterEvent(Self);
              end
              else
              begin
                if FDefControl <> '' then SetActive(GetControl(FDefControl))
                else SetActive(nil);
                if @FOnChangeEvent <> nil then FOnChangeEvent(Self);
              end;
            end;
        end;
    end;
end;

procedure TGUIEdit.SetText(Text: string);
begin
  if Length(Text) > FMaxLength then SetLength(Text, FMaxLength);
  FText := Text;
  FCaretPos := Length(FText);
end;

procedure TGUIEdit.Update;
begin
  inherited;
end;

{ TGUIKeyRead }

constructor TGUIKeyRead.Create(FontID: DWORD);
begin
  inherited Create();

  FFont := TFont.Create(FontID, FONT_CHAR);
end;

procedure TGUIKeyRead.Draw;
begin
  inherited;

  FFont.Draw(FX, FY, IfThen(FIsQuery, KEYREAD_QUERY, IfThen(FKey <> 0, KEYTABLE[FKey], KEYREAD_CLEAR)),
             FColor.R, FColor.G, FColor.B);
end;

function TGUIKeyRead.GetWidth: Word;
var
  a: Byte;
  w, h: Word;
begin
  Result := 0;
 
  for a := 0 to 255 do
  begin
    FFont.GetTextSize(KEYTABLE[a], w, h);
    Result := Max(Result, w);
  end;

  FFont.GetTextSize(KEYREAD_QUERY, w, h);
  if w > Result then Result := w;

  FFont.GetTextSize(KEYREAD_CLEAR, w, h);
  if w > Result then Result := w;
end;

procedure TGUIKeyRead.OnMessage(var Msg: TMessage);
begin
  inherited;

  if not FEnabled then
    Exit;

  with Msg do
    case Msg of
      WM_KEYDOWN:
        case wParam of
          VK_ESCAPE:
            begin
              if FIsQuery then
                with FWindow do
                  if FDefControl <> '' then
                    SetActive(GetControl(FDefControl))
                  else
                    SetActive(nil);

              FIsQuery := False;
            end;
          VK_RETURN:
            begin
              if not FIsQuery then
                begin
                  with FWindow do
                    if FActiveControl <> Self then
                      SetActive(Self);

                  FIsQuery := True;
                end
              else
                begin
                  FKey := 28; // <Enter>
                  FIsQuery := False;
                  
                  with FWindow do
                    if FDefControl <> '' then
                      SetActive(GetControl(FDefControl))
                    else
                      SetActive(nil);
                end;   
            end;
        end;

      MESSAGE_DIKEY:
        if FIsQuery and (wParam <> 28) then
        begin
          if KEYTABLE[wParam] <> '' then
            FKey := wParam;
          FIsQuery := False;

          with FWindow do
            if FDefControl <> '' then
              SetActive(GetControl(FDefControl))
            else
              SetActive(nil);
        end;
    end;
end;

{ TGUIModelView }

constructor TGUIModelView.Create;
begin
  inherited Create();

  FModel := nil;
end;

destructor TGUIModelView.Destroy;
begin
  FModel.Free();

  inherited;
end;

procedure TGUIModelView.Draw;
begin
  inherited;

  DrawBox(FX, FY, 4, 4);

  if FModel <> nil then FModel.Draw(FX+4, FY+4); 
end;

procedure TGUIModelView.NextAnim();
begin
  if FModel = nil then
    Exit;

  if FModel.Animation < A_PAIN then
    FModel.ChangeAnimation(FModel.Animation+1, True)
  else
    FModel.ChangeAnimation(A_STAND, True);
end;

procedure TGUIModelView.NextWeapon();
begin
  if FModel = nil then
    Exit;

  if FModel.Weapon < WEAPON_SUPERPULEMET then
    FModel.SetWeapon(FModel.Weapon+1)
  else
    FModel.SetWeapon(WEAPON_KASTET);
end;

procedure TGUIModelView.OnMessage(var Msg: TMessage);
begin
  inherited;

end;

procedure TGUIModelView.SetColor(Red, Green, Blue: Byte);
begin
  if FModel <> nil then FModel.SetColor(Red, Green, Blue); 
end;

procedure TGUIModelView.SetModel(ModelName: string);
begin
  FModel.Free();

  FModel := g_PlayerModel_Get(ModelName);
end;

procedure TGUIModelView.Update;
begin
  inherited;

  a := not a;
  if a then Exit;
 
  if FModel <> nil then FModel.Update;
end;

{ TGUIMapPreview }

constructor TGUIMapPreview.Create();
begin
  inherited Create();
  ClearMap;
end;

destructor TGUIMapPreview.Destroy();
begin
  ClearMap;
  inherited;
end;

procedure TGUIMapPreview.Draw();
var
  a: Integer;
  r, g, b: Byte;
begin
  inherited;

  DrawBox(FX, FY, MAPPREVIEW_WIDTH, MAPPREVIEW_HEIGHT);

  if (FMapSize.X <= 0) or (FMapSize.Y <= 0) then
    Exit;

  e_DrawFillQuad(FX+4, FY+4,
    FX+4 + Trunc(FMapSize.X / FScale) - 1,
    FY+4 + Trunc(FMapSize.Y / FScale) - 1,
    32, 32, 32, 0);     

  if FMapData <> nil then
    for a := 0 to High(FMapData) do
      with FMapData[a] do
      begin
        if X1 > MAPPREVIEW_WIDTH*16 then Continue;
        if Y1 > MAPPREVIEW_HEIGHT*16 then Continue;

        if X2 < 0 then Continue;
        if Y2 < 0 then Continue;
     
        if X2 > MAPPREVIEW_WIDTH*16 then X2 := MAPPREVIEW_WIDTH*16;
        if Y2 > MAPPREVIEW_HEIGHT*16 then Y2 := MAPPREVIEW_HEIGHT*16;

        if X1 < 0 then X1 := 0;
        if Y1 < 0 then Y1 := 0;

        case PanelType of
          PANEL_WALL:
            begin
              r := 255;
              g := 255;
              b := 255;
            end;
          PANEL_CLOSEDOOR:
            begin
              r := 255;
              g := 255;
              b := 0;
            end;
          PANEL_WATER:
            begin
              r := 0;
              g := 0;
              b := 255;
            end;
          PANEL_ACID1:
            begin
              r := 0;
              g := 255;
              b := 0;
            end;
          PANEL_ACID2:
            begin
              r := 255;
              g := 0;
              b := 0;
            end;
          else
            begin
              r := 127;
              g := 127;
              b := 127;
            end;
        end;

        if ((X2-X1) > 0) and ((Y2-Y1) > 0) then
          e_DrawFillQuad(FX+4 + X1, FY+4 + Y1,
            FX+4 + X2 - 1, FY+4 + Y2 - 1, r, g, b, 0);
      end;
end;

procedure TGUIMapPreview.OnMessage(var Msg: TMessage);
begin
  inherited;

end;

procedure TGUIMapPreview.SetMap(Res: string);
var
  WAD: TWADEditor_1;
  MapReader: TMapReader_1;
  panels: TPanelsRec1Array;
  header: TMapHeaderRec_1;
  a: Integer;
  FileName, SectionName, ResName: string;
  Data: Pointer;
  Len: Integer;
  rX, rY: Single;
begin
  g_ProcessResourceStr(Res, FileName, SectionName, ResName);

  WAD := TWADEditor_1.Create();
  if not WAD.ReadFile(FileName) then
  begin
    WAD.Free();
    Exit;
  end;

  if not WAD.GetResource('', ResName, Data, Len) then
  begin
    WAD.Free();
    Exit;
  end;

  WAD.Free();

  MapReader := TMapReader_1.Create();

  if not MapReader.LoadMap(Data) then
  begin
    FreeMem(Data);
    MapReader.Free();
    FMapSize.X := 0;
    FMapSize.Y := 0;
    FScale := 0.0;
    FMapData := nil;
    Exit;
  end;

  FreeMem(Data);

  panels := MapReader.GetPanels();
  header := MapReader.GetMapHeader();

  FMapSize.X := header.Width div 16;
  FMapSize.Y := header.Height div 16;

  rX := Ceil(header.Width / (MAPPREVIEW_WIDTH*256.0));
  rY := Ceil(header.Height / (MAPPREVIEW_HEIGHT*256.0));
  FScale := max(rX, rY);

  FMapData := nil;

  if panels <> nil then
    for a := 0 to High(panels) do
      if WordBool(panels[a].PanelType and (PANEL_WALL or PANEL_CLOSEDOOR or
                                           PANEL_STEP or PANEL_WATER or
                                           PANEL_ACID1 or PANEL_ACID2)) then
      begin
        SetLength(FMapData, Length(FMapData)+1);
        with FMapData[High(FMapData)] do
        begin
          X1 := panels[a].X div 16;
          Y1 := panels[a].Y div 16;

          X2 := (panels[a].X + panels[a].Width) div 16;
          Y2 := (panels[a].Y + panels[a].Height) div 16;

          X1 := Trunc(X1/FScale + 0.5);
          Y1 := Trunc(Y1/FScale + 0.5);
          X2 := Trunc(X2/FScale + 0.5);
          Y2 := Trunc(Y2/FScale + 0.5);

          if (X1 <> X2) or (Y1 <> Y2) then
          begin
            if X1 = X2 then
              X2 := X2 + 1;
            if Y1 = Y2 then
              Y2 := Y2 + 1;
          end;

          PanelType := panels[a].PanelType;
      end;
   end;

  panels := nil;

  MapReader.Free();
end;

procedure TGUIMapPreview.ClearMap();
begin
  SetLength(FMapData, 0);
  FMapData := nil;
  FMapSize.X := 0;
  FMapSize.Y := 0;
  FScale := 0.0;
end;

procedure TGUIMapPreview.Update();
begin
  inherited;

end;

function TGUIMapPreview.GetScaleStr(): String;
begin
  if FScale > 0.0 then
    begin
      Result := FloatToStrF(FScale*16.0, ffFixed, 3, 3);
      while (Result[Length(Result)] = '0') do
        Delete(Result, Length(Result), 1);
      if (Result[Length(Result)] = ',') or (Result[Length(Result)] = '.') then
        Delete(Result, Length(Result), 1);
      Result := '1 : ' + Result;
    end
  else
    Result := '';
end;

{ TGUIListBox }

procedure TGUIListBox.AddItem(Item: string);
begin
  SetLength(FItems, Length(FItems)+1);
  FItems[High(FItems)] := Item;

  if FSort then g_Basic.Sort(FItems);
end;

procedure TGUIListBox.Clear();
begin
  FItems := nil;

  FStartLine := 0;
  FIndex := -1;
end;

constructor TGUIListBox.Create(FontID: DWORD; Width, Height: Word);
begin
  inherited Create();

  FFont := TFont.Create(FontID, FONT_CHAR);

  FWidth := Width;
  FHeight := Height;
  FIndex := -1;
  FOnChangeEvent := nil;
  FDrawBack := True;
  FDrawScroll := True;
end;

procedure TGUIListBox.Draw;
var
  w2, h2: Word;
  a: Integer;
  s: string;
begin
  inherited;

  if FDrawBack then DrawBox(FX, FY, FWidth+1, FHeight);
  if FDrawScroll then
    DrawScroll(FX+4+FWidth*16, FY+4, FHeight, (FStartLine > 0) and (FItems <> nil),
              (FStartLine+FHeight-1 < High(FItems)) and (FItems <> nil));

  if FItems <> nil then
    for a := FStartLine to Min(High(FItems), FStartLine+FHeight-1) do
    begin
      s := Items[a];

      FFont.GetTextSize(s, w2, h2);
      while (Length(s) > 0) and (w2 > FWidth*16) do
      begin
        SetLength(s, Length(s)-1);
        FFont.GetTextSize(s, w2, h2);
      end;

      if a = FIndex then
        FFont.Draw(FX+4, FY+4+(a-FStartLine)*16, s, FActiveColor.R, FActiveColor.G, FActiveColor.B)
      else
        FFont.Draw(FX+4, FY+4+(a-FStartLine)*16, s, FUnActiveColor.R, FUnActiveColor.G, FUnActiveColor.B);
    end;
end;

function TGUIListBox.GetHeight: Word;
begin
  Result := 8+FHeight*16; 
end;

function TGUIListBox.GetWidth: Word;
begin
  Result := 8+(FWidth+1)*16; 
end;

procedure TGUIListBox.OnMessage(var Msg: TMessage);
var
  a: Integer;
begin
  if not FEnabled then Exit;

  inherited;

  if FItems = nil then Exit;

  with Msg do
    case Msg of
      WM_KEYDOWN:
        case wParam of
          VK_HOME:
          begin
            FIndex := 0;
            FStartLine := 0;
          end;
          VK_END:
          begin
            FIndex := High(FItems);
            FStartLine := Max(High(FItems)-FHeight+1, 0);
          end;
          VK_UP, VK_LEFT:
            if FIndex > 0 then
            begin
              Dec(FIndex);
              if FIndex < FStartLine then Dec(FStartLine);
              if @FOnChangeEvent <> nil then FOnChangeEvent(Self);
            end;
          VK_DOWN, VK_RIGHT:
            if FIndex < High(FItems) then
            begin
              Inc(FIndex);
              if FIndex > FStartLine+FHeight-1 then Inc(FStartLine);
              if @FOnChangeEvent <> nil then FOnChangeEvent(Self);
            end;
          VK_RETURN:
            with FWindow do
            begin
              if FActiveControl <> Self then SetActive(Self)
              else
                if FDefControl <> '' then SetActive(GetControl(FDefControl))
                else SetActive(nil);
            end;
        end;
      WM_CHAR:
        for a := 0 to High(FItems) do
          if (Length(FItems[a]) > 0) and (LowerCase(FItems[a][1]) = LowerCase(Chr(wParam))) then
          begin
            FIndex := a;
            FStartLine := Min(Max(FIndex-1, 0), Length(FItems)-FHeight);
            if @FOnChangeEvent <> nil then FOnChangeEvent(Self);
            Break;
          end;
    end;
end;

function TGUIListBox.SelectedItem(): String;
begin
  Result := '';

  if (FIndex < 0) or (FItems = nil) or
     (FIndex > High(FItems)) then
    Exit;

  Result := FItems[FIndex];
end;

procedure TGUIListBox.FSetItems(Items: SArray);
begin
  if FItems <> nil then
    FItems := nil;
   
  FItems := Items;

  FStartLine := 0;
  FIndex := -1;

  if FSort then g_Basic.Sort(FItems);
end;

procedure TGUIListBox.SelectItem(Item: String);
var
  a: Integer;
begin
  if FItems = nil then
    Exit;

  FIndex := 0;
  Item := LowerCase(Item);

  for a := 0 to High(FItems) do
    if LowerCase(FItems[a]) = Item then
    begin
      FIndex := a;
      Break;
    end;

  if FIndex <= FHeight then
    FStartLine := 0
  else
    FStartLine := Min(FIndex, Length(FItems)-FHeight);
end;

procedure TGUIListBox.FSetIndex(aIndex: Integer);
begin
  if FItems = nil then
    Exit;

  if (aIndex < 0) or (aIndex > High(FItems)) then
    Exit;

  FIndex := aIndex;

  if FIndex <= FHeight then
    FStartLine := 0
  else
    FStartLine := Min(FIndex, Length(FItems)-FHeight);
end;

{ TGUIFileListBox }

procedure TGUIFileListBox.OnMessage(var Msg: TMessage);
var
  a: Integer;
begin
  if not FEnabled then
    Exit;

  if FItems = nil then
    Exit;

  with Msg do
    case Msg of
      WM_KEYDOWN:
        case wParam of
          VK_HOME:
            begin
              FIndex := 0;
              FStartLine := 0;
              if @FOnChangeEvent <> nil then
                FOnChangeEvent(Self);
            end;

          VK_END:
            begin
              FIndex := High(FItems);
              FStartLine := Max(High(FItems)-FHeight+1, 0);
              if @FOnChangeEvent <> nil then
                FOnChangeEvent(Self);
            end;

          VK_PRIOR:
            begin
              if FIndex > FHeight then
                FIndex := FIndex-FHeight
              else
                FIndex := 0;

              if FStartLine > FHeight then
                FStartLine := FStartLine-FHeight
              else
                FStartLine := 0;
            end;

          VK_NEXT:
            begin
              if FIndex < High(FItems)-FHeight then
                FIndex := FIndex+FHeight
              else
                FIndex := High(FItems);

              if FStartLine < High(FItems)-FHeight then
                FStartLine := FStartLine+FHeight
              else
                FStartLine := High(FItems)-FHeight+1;
            end;

          VK_UP, VK_LEFT:
            if FIndex > 0 then
            begin
              Dec(FIndex);
              if FIndex < FStartLine then
                Dec(FStartLine);
              if @FOnChangeEvent <> nil then
                FOnChangeEvent(Self);
            end;

          VK_DOWN, VK_RIGHT:
            if FIndex < High(FItems) then
            begin
              Inc(FIndex);
              if FIndex > FStartLine+FHeight-1 then
                Inc(FStartLine);
              if @FOnChangeEvent <> nil then
                FOnChangeEvent(Self);
            end;

          VK_RETURN:
            with FWindow do
            begin
              if FActiveControl <> Self then
                SetActive(Self)
              else
                begin
                  if FItems[FIndex][1] = #127 then // Папка
                  begin
                    OpenDir(FPath+Copy(FItems[FIndex], 2, 255));
                    FIndex := 0;
                    Exit;
                  end;

                  if FDefControl <> '' then
                    SetActive(GetControl(FDefControl))
                  else
                    SetActive(nil);
                end;
            end;
        end;

      WM_CHAR:
        for a := 0 to High(FItems) do
          if ( (Length(FItems[a]) > 0) and
               (LowerCase(FItems[a][1]) = LowerCase(Chr(wParam))) ) or
             ( (Length(FItems[a]) > 1) and
               (FItems[a][1] = #127) and // Папка
               (LowerCase(FItems[a][2]) = LowerCase(Chr(wParam))) ) then
          begin
            FIndex := a;
            FStartLine := Min(Max(FIndex-1, 0), Length(FItems)-FHeight);
            if @FOnChangeEvent <> nil then
              FOnChangeEvent(Self);
            Break;
          end;
    end;
end;

procedure TGUIFileListBox.OpenDir(path: String);
var
  SR: TSearchRec;
  i: Integer;
begin
  Clear();

  path := IncludeTrailingPathDelimiter(path);
  path := ExpandFileName(path);

// Каталоги:
  if FDirs then
  begin
    if FindFirst(path+'*', faDirectory, SR) = 0 then
    repeat
      if SR.Attr <> faDirectory then
        Continue;
      if (SR.Name = '.') or
         ((SR.Name = '..') and (path = FBasePath)) then
        Continue;

      AddItem(#1 + SR.Name);
    until FindNext(SR) <> 0;

    FindClose(SR);
  end;

// Файлы:
  if FindFirst(path+FFileMask, faAnyFile, SR) = 0 then
    repeat
      AddItem(SR.Name);
    until FindNext(SR) <> 0;
    
  FindClose(SR);

  for i := 0 to High(FItems) do
    if FItems[i][1] = #1 then
      FItems[i][1] := #127;

  FPath := path;
end;

procedure TGUIFileListBox.SetBase(path: String);
begin
  FBasePath := path;
  OpenDir(FBasePath);
end;

function TGUIFileListBox.SelectedItem(): String;
begin
  Result := '';

  if (FIndex = -1) or (FItems = nil) or
     (FIndex > High(FItems)) or
     (FItems[FIndex][1] = '\') then
    Exit;

  Result := FPath + FItems[FIndex];
end;

procedure TGUIFileListBox.UpdateFileList();
var
  fn: String;
begin
  if (FIndex = -1) or (FItems = nil) or
     (FIndex > High(FItems)) or
     (FItems[FIndex][1] = '\') then
    fn := ''
  else
    fn := FItems[FIndex];

  OpenDir(FPath);

  if fn <> '' then
    SelectItem(fn);
end;

{ TGUIMemo }

procedure TGUIMemo.Clear;
begin
  FLines := nil;
  FStartLine := 0;
end;

constructor TGUIMemo.Create(FontID: DWORD; Width, Height: Word);
begin
  inherited Create();

  FFont := TFont.Create(FontID, FONT_CHAR);

  FWidth := Width;
  FHeight := Height;
  FDrawBack := True;
  FDrawScroll := True;
end;

procedure TGUIMemo.Draw;
var
  a: Integer;
begin
  inherited;

  if FDrawBack then DrawBox(FX, FY, FWidth+1, FHeight);
  if FDrawScroll then
    DrawScroll(FX+4+FWidth*16, FY+4, FHeight, (FStartLine > 0) and (FLines <> nil),
              (FStartLine+FHeight-1 < High(FLines)) and (FLines <> nil));

  if FLines <> nil then
    for a := FStartLine to Min(High(FLines), FStartLine+FHeight-1) do
      FFont.Draw(FX+4, FY+4+(a-FStartLine)*16, FLines[a], FColor.R, FColor.G, FColor.B);
end;

function TGUIMemo.GetHeight: Word;
begin
  Result := 8+FHeight*16;
end;

function TGUIMemo.GetWidth: Word;
begin
  Result := 8+(FWidth+1)*16;
end;

procedure TGUIMemo.OnMessage(var Msg: TMessage);
begin
  if not FEnabled then Exit;

  inherited;

  if FLines = nil then Exit;

  with Msg do
    case Msg of
      WM_KEYDOWN:
        case wParam of
          VK_UP, VK_LEFT:
            if FStartLine > 0 then
              Dec(FStartLine);
          VK_DOWN, VK_RIGHT:
            if FStartLine < Length(FLines)-FHeight then
              Inc(FStartLine);
          VK_RETURN:
            with FWindow do
            begin
              if FActiveControl <> Self then
              begin
                SetActive(Self);
                {FStartLine := 0;}
              end
              else
              if FDefControl <> '' then SetActive(GetControl(FDefControl))
                else SetActive(nil);
            end;
        end;
    end;
end;

procedure TGUIMemo.SetText(Text: string);
begin
  FStartLine := 0;
  FLines := GetLines(Text, FFont.ID, FWidth*16);
end;

{ TGUIimage }

procedure TGUIimage.ClearImage();
begin
  if FImageRes = '' then Exit;

  g_Texture_Delete(FImageRes);
  FImageRes := '';
end;

constructor TGUIimage.Create();
begin
  inherited Create();

  FImageRes := '';
end;

destructor TGUIimage.Destroy();
begin
  inherited;
end;

procedure TGUIimage.Draw();
var
  ID: DWORD;
begin
  inherited;

  if FImageRes = '' then
    (if g_Texture_Get(FDefaultRes, ID) then e_Draw(ID, FX, FY, 0, True, False))
  else
    if g_Texture_Get(FImageRes, ID) then e_Draw(ID, FX, FY, 0, True, False);
end;

procedure TGUIimage.OnMessage(var Msg: TMessage);
begin
  inherited;
end;

procedure TGUIimage.SetImage(Res: string);
begin
  ClearImage();

  if g_Texture_CreateWADEx(Res, Res) then FImageRes := Res;
end;

procedure TGUIimage.Update();
begin
  inherited;
end;

end.
