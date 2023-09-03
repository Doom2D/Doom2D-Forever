unit f_main;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Buttons,
  ComCtrls, ValEdit, Types, Menus, ExtCtrls,
  CheckLst, Grids, OpenGLContext, Utils, UTF8Process;

type

  { TMainForm }

  TMainForm = class(TForm)
    MapTestTimer: TTimer;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ColorDialog: TColorDialog;

  // Menu:
    MainMenu: TMainMenu;
    ImageList: TImageList;
  // Apple menu:
    miApple: TMenuItem;
    miAppleAbout: TMenuItem;
    miAppleLine0: TMenuItem;
    miApplePref: TMenuItem;
    miAppleLine1: TMenuItem;
  // File menu:
    miMenuFile: TMenuItem;
    miNewMap: TMenuItem;
    miOpenMap: TMenuItem;
    miMacRecentSubMenu: TMenuItem;
    miMacRecentEnd: TMenuItem;
    miMacRecentClear: TMenuItem;
    Separator1: TMenuItem;
    miSaveMap: TMenuItem;
    miSaveMapAs: TMenuItem;
    miOpenWadMap: TMenuItem;
    miLine1: TMenuItem;
    miReopenMap: TMenuItem;
    miSaveMiniMap: TMenuItem;
    miDeleteMap: TMenuItem;
    miPackMap: TMenuItem;
    miWinRecentStart: TMenuItem;
    miWinRecent: TMenuItem;
    miLine2: TMenuItem;
    miExit: TMenuItem;
  // Edit menu:
    miMenuEdit: TMenuItem;
    miUndo: TMenuItem;
    miLine3: TMenuItem;
    miCopy: TMenuItem;
    miCut: TMenuItem;
    miPaste: TMenuItem;
    miLine4: TMenuItem;
    miSelectAll: TMenuItem;
    miLine5: TMenuItem;
    miSnapToGrid: TMenuItem;
    miSwitchGrid: TMenuItem;
    Separator2: TMenuItem;
    miToFore: TMenuItem;
    miToBack: TMenuItem;
    miLine6: TMenuItem;
    miMapOptions: TMenuItem;
    miOptions: TMenuItem;
  // View menu:
    miMenuView: TMenuItem;
    miLayers: TMenuItem;
    miLayer1: TMenuItem;
    miLayer2: TMenuItem;
    miLayer3: TMenuItem;
    miLayer4: TMenuItem;
    miLayer5: TMenuItem;
    miLayer6: TMenuItem;
    miLayer7: TMenuItem;
    miLayer8: TMenuItem;
    miLayer9: TMenuItem;
    miViewLine1: TMenuItem;
    miMiniMap: TMenuItem;
    miShowEdges: TMenuItem;
    miViewLine2: TMenuItem;
    miMapPreview: TMenuItem;
  // Service menu:
    miMenuService: TMenuItem;
    miCheckMap: TMenuItem;
    miOptimmization: TMenuItem;
    miTestMap: TMenuItem;
  // Window menu:
    miMenuWindow: TMenuItem;
    miMacMinimize: TMenuItem;
    miMacZoom: TMenuItem;
  // Help Menu:
    miMenuHelp: TMenuItem;
    miAbout: TMenuItem;
  // HIDDEN menu:
    miMenuHidden: TMenuItem;
    minexttab: TMenuItem;
    selectall1: TMenuItem;

  // Toolbar:
    ilToolbar: TImageList;
    MainToolBar: TToolBar;
    tbNewMap: TToolButton;
    tbOpenMap: TToolButton;
    tbSaveMap: TToolButton;
    tbOpenWadMap: TToolButton;
    tbLine1: TToolButton;
    tbShowMap: TToolButton;
    tbLine2: TToolButton;
    tbShow: TToolButton;
    pmShow: TPopupMenu;
    miLayerP1: TMenuItem;
    miLayerP2: TMenuItem;
    miLayerP3: TMenuItem;
    miLayerP4: TMenuItem;
    miLayerP5: TMenuItem;
    miLayerP6: TMenuItem;
    miLayerP7: TMenuItem;
    miLayerP8: TMenuItem;
    miLayerP9: TMenuItem;
    tbLine3: TToolButton;
    tbGridOn: TToolButton;
    tbGrid: TToolButton;
    tbLine4: TToolButton;
    tbTestMap: TToolButton;

  // Progress bar:
    pLoadProgress: TPanel;
    lLoad: TLabel;
    pbLoad: TProgressBar;

  // Map edit area:
    PanelMap: TPanel;
    RenderPanel: TOpenGLControl;
    sbHorizontal: TScrollBar;
    sbVertical: TScrollBar;

  // Object propertiy editor:
    PanelProps: TPanel;
    PanelPropApply: TPanel;
    bApplyProperty: TButton;
    vleObjectProperty: TValueListEditor;

  // Object palette:
    PanelObjs: TPanel;
    pcObjects: TPageControl;
  // Panels Tab:
    tsPanels: TTabSheet;
    PanelPanelType: TPanel;
    lbPanelType: TListBox;
    lbTextureList: TListBox;
    PanelTextures: TPanel;
    LabelTxW: TLabel;
    lTextureWidth: TLabel;
    LabelTxH: TLabel;
    lTextureHeight: TLabel;
    cbPreview: TCheckBox;
    bbAddTexture: TBitBtn;
    bbRemoveTexture: TBitBtn;
    bClearTexture: TButton;
  // Items Tab:
    tsItems: TTabSheet;
    lbItemList: TListBox;
    cbOnlyDM: TCheckBox;
    cbFall: TCheckBox;
  // Monsters Tab:
    tsMonsters: TTabSheet;
    lbMonsterList: TListBox;
    rbMonsterLeft: TRadioButton;
    rbMonsterRight: TRadioButton;
  // Areas Tab:
    tsAreas: TTabSheet;
    lbAreasList: TListBox;
    rbAreaLeft: TRadioButton;
    rbAreaRight: TRadioButton;
  // Triggers Tab:
    tsTriggers: TTabSheet;
    lbTriggersList: TListBox;
    clbActivationType: TCheckListBox;
    clbKeys: TCheckListBox;

    procedure aAboutExecute(Sender: TObject);
    procedure aCheckMapExecute(Sender: TObject);
    procedure aMoveToFore(Sender: TObject);
    procedure aMoveToBack(Sender: TObject);
    procedure aCopyObjectExecute(Sender: TObject);
    procedure aCutObjectExecute(Sender: TObject);
    procedure aEditorOptionsExecute(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure aMapOptionsExecute(Sender: TObject);
    procedure aNewMapExecute(Sender: TObject);
    procedure aOpenMapExecute(Sender: TObject);
    procedure aOptimizeExecute(Sender: TObject);
    procedure aPasteObjectExecute(Sender: TObject);
    procedure aSelectAllExecute(Sender: TObject);
    procedure aSaveMapExecute(Sender: TObject);
    procedure aSaveMapAsExecute(Sender: TObject);
    procedure aUndoExecute(Sender: TObject);
    procedure aDeleteMap(Sender: TObject);
    procedure bApplyPropertyClick(Sender: TObject);
    procedure bbAddTextureClick(Sender: TObject);
    procedure bbRemoveTextureClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure miMacRecentClearClick(Sender: TObject);
    procedure miMacZoomClick(Sender: TObject);
    procedure lbTextureListClick(Sender: TObject);
    procedure lbTextureListDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure miMacMinimizeClick(Sender: TObject);
    procedure miReopenMapClick(Sender: TObject);
    procedure RenderPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RenderPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure RenderPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RenderPanelPaint(Sender: TObject);
    procedure RenderPanelResize(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure MapTestCheck(Sender: TObject);
    procedure vleObjectPropertyEditButtonClick(Sender: TObject);
    procedure vleObjectPropertyApply(Sender: TObject);
    procedure vleObjectPropertyGetPickList(Sender: TObject; const KeyName: String; Values: TStrings);
    procedure vleObjectPropertyKeyDown(Sender: TObject; var Key: Word;
                                       Shift: TShiftState);
    procedure tbGridOnClick(Sender: TObject);
    procedure miMapPreviewClick(Sender: TObject);
    procedure miLayer1Click(Sender: TObject);
    procedure miLayer2Click(Sender: TObject);
    procedure miLayer3Click(Sender: TObject);
    procedure miLayer4Click(Sender: TObject);
    procedure miLayer5Click(Sender: TObject);
    procedure miLayer6Click(Sender: TObject);
    procedure miLayer7Click(Sender: TObject);
    procedure miLayer8Click(Sender: TObject);
    procedure miLayer9Click(Sender: TObject);
    procedure tbShowClick(Sender: TObject);
    procedure miSnapToGridClick(Sender: TObject);
    procedure miMiniMapClick(Sender: TObject);
    procedure miSwitchGridClick(Sender: TObject);
    procedure miShowEdgesClick(Sender: TObject);
    procedure minexttabClick(Sender: TObject);
    procedure miSaveMiniMapClick(Sender: TObject);
    procedure bClearTextureClick(Sender: TObject);
    procedure miPackMapClick(Sender: TObject);
    procedure miTestMapClick(Sender: TObject);
    procedure sbVerticalScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure sbHorizontalScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure miOpenWadMapClick(Sender: TObject);
    procedure selectall1Click(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure Splitter2CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure vleObjectPropertyEnter(Sender: TObject);
    procedure vleObjectPropertyExit(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    procedure Draw();
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure RefillRecentMenu (menu: TMenuItem; start: Integer; fmt: AnsiString);
  public
    procedure RefreshRecentMenu();
    procedure OpenMapFile(FileName: String);
    function RenderMousePos(): TPoint;
    procedure RecountSelectedObjects();
  end;

const
  LAYER_BACK       = 0;
  LAYER_WALLS      = 1;
  LAYER_FOREGROUND = 2;
  LAYER_STEPS      = 3;
  LAYER_WATER      = 4;
  LAYER_ITEMS      = 5;
  LAYER_MONSTERS   = 6;
  LAYER_AREAS      = 7;
  LAYER_TRIGGERS   = 8;

  TEST_MAP_NAME = '$$$_TEST_$$$';
  LANGUAGE_FILE_NAME = '_Editor.txt';

var
  MainForm: TMainForm;
  StartMap: String;
  OpenedMap: String;
  OpenedWAD: String;

  DotColor: TColor;
  DotEnable: Boolean;
  DotStep: Word;
  DotStepOne, DotStepTwo: Word;
  DotSize: Byte;
  DrawTexturePanel: Boolean;
  DrawPanelSize: Boolean;
  BackColor: TColor;
  PreviewColor: TColor;
  UseCheckerboard: Boolean;
  Scale: Byte;
  RecentCount: Integer;
  RecentFiles: TStringList;
  slInvalidTextures: TStringList;

  TestGameMode: String;
  TestLimTime: String;
  TestLimScore: String;
  TestOptionsTwoPlayers: Boolean;
  TestOptionsTeamDamage: Boolean;
  TestOptionsAllowExit: Boolean;
  TestOptionsWeaponStay: Boolean;
  TestOptionsMonstersDM: Boolean;
  TestD2dExe, TestD2DArgs: String;
  TestMapOnce: Boolean;

  LayerEnabled: Array [LAYER_BACK..LAYER_TRIGGERS] of Boolean =
    (True, True, True, True, True, True, True, True, True);
  ContourEnabled: Array [LAYER_BACK..LAYER_TRIGGERS] of Boolean =
    (False, False, False, False, False, False, False, False, False);
  PreviewMode: Byte = 0;
  gLanguage: String;

  FormCaption: String;


procedure OpenMap(FileName: String; mapN: String);
function  AddTexture(aWAD, aSection, aTex: String; silent: Boolean): Boolean;
procedure RemoveSelectFromObjects();
procedure ChangeShownProperty(Name: String; NewValue: String);

implementation

uses
  f_options, e_graphics, e_log, GL, Math,
  f_mapoptions, g_basic, f_about, f_mapoptimization,
  f_mapcheck, f_addresource_texture, g_textures,
  f_activationtype, f_keys, wadreader, fileutil,
  MAPREADER, f_selectmap, f_savemap, WADEDITOR, MAPDEF,
  g_map, f_saveminimap, f_addresource, CONFIG, f_packmap,
  f_addresource_sound, f_choosetype,
  g_language, ClipBrd, g_resources, g_options;

const
  UNDO_DELETE_PANEL   = 1;
  UNDO_DELETE_ITEM    = 2;
  UNDO_DELETE_AREA    = 3;
  UNDO_DELETE_MONSTER = 4;
  UNDO_DELETE_TRIGGER = 5;
  UNDO_ADD_PANEL      = 6;
  UNDO_ADD_ITEM       = 7;
  UNDO_ADD_AREA       = 8;
  UNDO_ADD_MONSTER    = 9;
  UNDO_ADD_TRIGGER    = 10;
  UNDO_MOVE_PANEL     = 11;
  UNDO_MOVE_ITEM      = 12;
  UNDO_MOVE_AREA      = 13;
  UNDO_MOVE_MONSTER   = 14;
  UNDO_MOVE_TRIGGER   = 15;
  UNDO_RESIZE_PANEL   = 16;
  UNDO_RESIZE_TRIGGER = 17;

  MOUSEACTION_NONE        = 0;
  MOUSEACTION_DRAWPANEL   = 1;
  MOUSEACTION_DRAWTRIGGER = 2;
  MOUSEACTION_MOVEOBJ     = 3;
  MOUSEACTION_RESIZE      = 4;
  MOUSEACTION_MOVEMAP     = 5;
  MOUSEACTION_DRAWPRESS   = 6;
  MOUSEACTION_NOACTION    = 7;

  RESIZETYPE_NONE       = 0;
  RESIZETYPE_VERTICAL   = 1;
  RESIZETYPE_HORIZONTAL = 2;

  RESIZEDIR_NONE  = 0;
  RESIZEDIR_DOWN  = 1;
  RESIZEDIR_UP    = 2;
  RESIZEDIR_RIGHT = 3;
  RESIZEDIR_LEFT  = 4;

  SELECTFLAG_NONE       = 0;
  SELECTFLAG_TELEPORT   = 1;
  SELECTFLAG_DOOR       = 2;
  SELECTFLAG_TEXTURE    = 3;
  SELECTFLAG_LIFT       = 4;
  SELECTFLAG_MONSTER    = 5;
  SELECTFLAG_SPAWNPOINT = 6;
  SELECTFLAG_SHOTPANEL  = 7;
  SELECTFLAG_SELECTED   = 8;

  RECENT_FILES_MENU_START = 12;

  CLIPBOARD_SIG         = 'DF:ED';

type
  TUndoRec = record
    case UndoType: Byte of
      UNDO_DELETE_PANEL:   (Panel: ^TPanel);
      UNDO_DELETE_ITEM:    (Item: TItem);
      UNDO_DELETE_AREA:    (Area: TArea);
      UNDO_DELETE_MONSTER: (Monster: TMonster);
      UNDO_DELETE_TRIGGER: (Trigger: TTrigger);
      UNDO_ADD_PANEL,
      UNDO_ADD_ITEM,
      UNDO_ADD_AREA,
      UNDO_ADD_MONSTER,
      UNDO_ADD_TRIGGER:    (AddID: DWORD);
      UNDO_MOVE_PANEL,
      UNDO_MOVE_ITEM,
      UNDO_MOVE_AREA,
      UNDO_MOVE_MONSTER,
      UNDO_MOVE_TRIGGER:   (MoveID: DWORD; dX, dY: Integer);
      UNDO_RESIZE_PANEL,
      UNDO_RESIZE_TRIGGER: (ResizeID: DWORD; dW, dH: Integer);
  end;

  TCopyRec = record
    ID: Cardinal;
    case ObjectType: Byte of
      OBJECT_PANEL: (Panel: ^TPanel);
      OBJECT_ITEM: (Item: TItem);
      OBJECT_AREA: (Area: TArea);
      OBJECT_MONSTER: (Monster: TMonster);
      OBJECT_TRIGGER: (Trigger: TTrigger);
  end;

  TCopyRecArray = Array of TCopyRec;

var
  gEditorFont: DWORD;
  gDataLoaded: Boolean = False;
  ShowMap: Boolean = False;
  DrawRect: PRect = nil;
  SnapToGrid: Boolean = True;

  MousePos: Types.TPoint;
  LastMovePoint: Types.TPoint;
  MouseLDown: Boolean;
  MouseRDown: Boolean;
  MouseMDown: Boolean;
  MouseLDownPos: Types.TPoint;
  MouseRDownPos: Types.TPoint;
  MouseMDownPos: Types.TPoint;

  SelectFlag: Byte = SELECTFLAG_NONE;
  MouseAction: Byte = MOUSEACTION_NONE;
  ResizeType: Byte = RESIZETYPE_NONE;
  ResizeDirection: Byte = RESIZEDIR_NONE;

  DrawPressRect: Boolean = False;
  EditingProperties: Boolean = False;

  UndoBuffer: Array of Array of TUndoRec = nil;

  MapTestProcess: TProcessUTF8;
  MapTestFile: String;

{$R *.lfm}

//----------------------------------------
//Далее идут вспомогательные процедуры
//----------------------------------------

function NameToBool(Name: String): Boolean;
begin
  if Name = BoolNames[True] then
    Result := True
  else
    Result := False;
end;

function NameToDir(Name: String): TDirection;
begin
  if Name = DirNames[D_LEFT] then
    Result := D_LEFT
  else
    Result := D_RIGHT;
end;

function NameToDirAdv(Name: String): Byte;
begin
  if Name = DirNamesAdv[1] then
    Result := 1
  else
    if Name = DirNamesAdv[2] then
      Result := 2
    else
      if Name = DirNamesAdv[3] then
        Result := 3
      else
        Result := 0;
end;

function ActivateToStr(ActivateType: Byte): String;
begin
  Result := '';

  if ByteBool(ACTIVATE_PLAYERCOLLIDE and ActivateType) then
    Result := Result + '+PC';
  if ByteBool(ACTIVATE_MONSTERCOLLIDE and ActivateType) then
    Result := Result + '+MC';
  if ByteBool(ACTIVATE_PLAYERPRESS and ActivateType) then
    Result := Result + '+PP';
  if ByteBool(ACTIVATE_MONSTERPRESS and ActivateType) then
    Result := Result + '+MP';
  if ByteBool(ACTIVATE_SHOT and ActivateType) then
    Result := Result + '+SH';
  if ByteBool(ACTIVATE_NOMONSTER and ActivateType) then
    Result := Result + '+NM';

  if (Result <> '') and (Result[1] = '+') then
    Delete(Result, 1, 1);
end;

function StrToActivate(Str: String): Byte;
begin
  Result := 0;

  if Pos('PC', Str) > 0 then
    Result := ACTIVATE_PLAYERCOLLIDE;
  if Pos('MC', Str) > 0 then
    Result := Result or ACTIVATE_MONSTERCOLLIDE;
  if Pos('PP', Str) > 0 then
    Result := Result or ACTIVATE_PLAYERPRESS;
  if Pos('MP', Str) > 0 then
    Result := Result or ACTIVATE_MONSTERPRESS;
  if Pos('SH', Str) > 0 then
    Result := Result or ACTIVATE_SHOT;
  if Pos('NM', Str) > 0 then
    Result := Result or ACTIVATE_NOMONSTER;
end;

function KeyToStr(Key: Byte): String;
begin
  Result := '';

  if ByteBool(KEY_RED and Key) then
    Result := Result + '+RK';
  if ByteBool(KEY_GREEN and Key) then
    Result := Result + '+GK';
  if ByteBool(KEY_BLUE and Key) then
    Result := Result + '+BK';
  if ByteBool(KEY_REDTEAM and Key) then
    Result := Result + '+RT';
  if ByteBool(KEY_BLUETEAM and Key) then
    Result := Result + '+BT';

  if (Result <> '') and (Result[1] = '+') then
    Delete(Result, 1, 1);
end;

function StrToKey(Str: String): Byte;
begin
  Result := 0;

  if Pos('RK', Str) > 0 then
    Result := KEY_RED;
  if Pos('GK', Str) > 0 then
    Result := Result or KEY_GREEN;
  if Pos('BK', Str) > 0 then
    Result := Result or KEY_BLUE;
  if Pos('RT', Str) > 0 then
    Result := Result or KEY_REDTEAM;
  if Pos('BT', Str) > 0 then
    Result := Result or KEY_BLUETEAM;
end;

function EffectToStr(Effect: Byte): String;
begin
  if Effect in [EFFECT_TELEPORT..EFFECT_FIRE] then
    Result := EffectNames[Effect]
  else
    Result := EffectNames[EFFECT_NONE];
end;

function StrToEffect(Str: String): Byte;
var
  i: Integer;
begin
  Result := EFFECT_NONE;
  for i := EFFECT_TELEPORT to EFFECT_FIRE do
    if EffectNames[i] = Str then
      begin
        Result := i;
        Exit;
      end;
end;

function MonsterToStr(MonType: Byte): String;
begin
  if MonType in [MONSTER_DEMON..MONSTER_MAN] then
    Result := MonsterNames[MonType]
  else
    Result := MonsterNames[MONSTER_ZOMBY];
end;

function StrToMonster(Str: String): Byte;
var
  i: Integer;
begin
  Result := MONSTER_ZOMBY;
  for i := MONSTER_DEMON to MONSTER_MAN do
    if MonsterNames[i] = Str then
      begin
        Result := i;
        Exit;
      end;
end;

function ItemToStr(ItemType: Byte): String;
begin
  if ItemType in [ITEM_MEDKIT_SMALL..ITEM_MAX] then
    Result := ItemNames[ItemType]
  else
    Result := ItemNames[ITEM_AMMO_BULLETS];
end;

function StrToItem(Str: String): Byte;
var
  i: Integer;
begin
  Result := ITEM_AMMO_BULLETS;
  for i := ITEM_MEDKIT_SMALL to ITEM_MAX do
    if ItemNames[i] = Str then
      begin
        Result := i;
        Exit;
      end;
end;

function ShotToStr(ShotType: Byte): String;
begin
  if ShotType in [TRIGGER_SHOT_PISTOL..TRIGGER_SHOT_MAX] then
    Result := ShotNames[ShotType]
  else
    Result := ShotNames[TRIGGER_SHOT_PISTOL];
end;

function StrToShot(Str: String): Byte;
var
  i: Integer;
begin
  Result := TRIGGER_SHOT_PISTOL;
  for i := TRIGGER_SHOT_PISTOL to TRIGGER_SHOT_MAX do
    if ShotNames[i] = Str then
      begin
        Result := i;
        Exit;
      end;
end;

function SelectedObjectCount(): Word;
var
  a: Integer;
begin
  Result := 0;

  if SelectedObjects = nil then
    Exit;

  for a := 0 to High(SelectedObjects) do
    if SelectedObjects[a].Live then
      Result := Result + 1;
end;

function GetFirstSelected(): Integer;
var
  a: Integer;
begin
  Result := -1;

  if SelectedObjects = nil then
    Exit;

  for a := 0 to High(SelectedObjects) do
    if SelectedObjects[a].Live then
    begin
      Result := a;
      Exit;
    end;
end;

function Normalize16(x: Integer): Integer;
begin
  Result := (x div 16) * 16;
end;

procedure MoveMap(X, Y: Integer);
var
  rx, ry, ScaleSz: Integer;
begin
  with MainForm.RenderPanel do
  begin
    ScaleSz := 16 div Scale;
  // Размер видимой части карты:
    rx := Min(Normalize16(Width), Normalize16(gMapInfo.Width)) div 2;
    ry := Min(Normalize16(Height), Normalize16(gMapInfo.Height)) div 2;
  // Место клика на мини-карте:
    MapOffset.X := X - (Width - Max(gMapInfo.Width div ScaleSz, 1) - 1);
    MapOffset.Y := Y - 1;
  // Это же место на "большой" карте:
    MapOffset.X := MapOffset.X * ScaleSz;
    MapOffset.Y := MapOffset.Y * ScaleSz;
  // Левый верхний угол новой видимой части карты:
    MapOffset.X := MapOffset.X - rx;
    MapOffset.Y := MapOffset.Y - ry;
  // Выход за границы:
    MapOffset.X := EnsureRange(MapOffset.X, MainForm.sbHorizontal.Min, MainForm.sbHorizontal.Max);
    MapOffset.Y := EnsureRange(MapOffset.Y, MainForm.sbVertical.Min, MainForm.sbVertical.Max);
  // Кратно 16:
  //  MapOffset.X := Normalize16(MapOffset.X);
  //  MapOffset.Y := Normalize16(MapOffset.Y);
  end;

  MainForm.sbHorizontal.Position := MapOffset.X;
  MainForm.sbVertical.Position := MapOffset.Y;

  MapOffset.X := -MapOffset.X;
  MapOffset.Y := -MapOffset.Y;

  MainForm.Resize();
end;

function IsTexturedPanel(PanelType: Word): Boolean;
begin
  Result := WordBool(PanelType and (PANEL_WALL or PANEL_BACK or PANEL_FORE or
                                    PANEL_STEP or PANEL_OPENDOOR or PANEL_CLOSEDOOR or
                                    PANEL_WATER or PANEL_ACID1 or PANEL_ACID2));
end;

procedure FillProperty();
var
  _id: DWORD;
  str: String;
begin
  MainForm.vleObjectProperty.Strings.Clear();
  MainForm.RecountSelectedObjects();

// Отображаем свойства если выделен только один объект:
  if SelectedObjectCount() <> 1 then
    Exit;
    
  _id := GetFirstSelected();
  if not SelectedObjects[_id].Live then
    Exit;

  with MainForm.vleObjectProperty do
    with ItemProps[InsertRow(MsgPropId, IntToStr(SelectedObjects[_id].ID), True)] do
    begin
      EditStyle := esSimple;
      ReadOnly := True;
    end;

  case SelectedObjects[0].ObjectType of
    OBJECT_PANEL:
      begin
        with MainForm.vleObjectProperty,
             gPanels[SelectedObjects[_id].ID] do
        begin
          with ItemProps[InsertRow(MsgPropX, IntToStr(X), True)] do
          begin
            EditStyle := esSimple;
            MaxLength := 5;
          end;

          with ItemProps[InsertRow(MsgPropY, IntToStr(Y), True)] do
          begin
            EditStyle := esSimple;
            MaxLength := 5;
          end;

          with ItemProps[InsertRow(MsgPropWidth, IntToStr(Width), True)] do
          begin
            EditStyle := esSimple;
            MaxLength := 5;
          end;

          with ItemProps[InsertRow(MsgPropHeight, IntToStr(Height), True)] do
          begin
            EditStyle := esSimple;
            MaxLength := 5;
          end;

          with ItemProps[InsertRow(MsgPropPanelType, GetPanelName(PanelType), True)] do
          begin
            EditStyle := esEllipsis;
            ReadOnly := True;
          end;

          if IsTexturedPanel(PanelType) then
          begin // Может быть текстура
            with ItemProps[InsertRow(MsgPropPanelTex, TextureName, True)] do
            begin
              EditStyle := esEllipsis;
              ReadOnly := True;
            end;

            if TextureName <> '' then
            begin // Есть текстура
              with ItemProps[InsertRow(MsgPropPanelAlpha, IntToStr(Alpha), True)] do
              begin
                EditStyle := esSimple;
                MaxLength := 3;
              end;

              with ItemProps[InsertRow(MsgPropPanelBlend, BoolNames[Blending], True)] do
              begin
                EditStyle := esPickList;
                ReadOnly := True;
              end;
            end;
          end;
        end;
      end;

    OBJECT_ITEM:
      begin
        with MainForm.vleObjectProperty,
             gItems[SelectedObjects[_id].ID] do
        begin
          with ItemProps[InsertRow(MsgPropX, IntToStr(X), True)] do
          begin
            EditStyle := esSimple;
            MaxLength := 5;
          end;

          with ItemProps[InsertRow(MsgPropY, IntToStr(Y), True)] do
          begin
            EditStyle := esSimple;
            MaxLength := 5;
          end;

          with ItemProps[InsertRow(MsgPropDmOnly, BoolNames[OnlyDM], True)] do
          begin
            EditStyle := esPickList;
            ReadOnly := True;
          end;

          with ItemProps[InsertRow(MsgPropItemFalls, BoolNames[Fall], True)] do
          begin
            EditStyle := esPickList;
            ReadOnly := True;
          end;
        end;
      end;

    OBJECT_MONSTER:
      begin
        with MainForm.vleObjectProperty,
             gMonsters[SelectedObjects[_id].ID] do
        begin
          with ItemProps[InsertRow(MsgPropX, IntToStr(X), True)] do
          begin
            EditStyle := esSimple;
            MaxLength := 5;
          end;

          with ItemProps[InsertRow(MsgPropY, IntToStr(Y), True)] do
          begin
            EditStyle := esSimple;
            MaxLength := 5;
          end;

          with ItemProps[InsertRow(MsgPropDirection, DirNames[Direction], True)] do
          begin
            EditStyle := esPickList;
            ReadOnly := True;
          end;
        end;
      end;

    OBJECT_AREA:
      begin
        with MainForm.vleObjectProperty,
             gAreas[SelectedObjects[_id].ID] do
        begin
          with ItemProps[InsertRow(MsgPropX, IntToStr(X), True)] do
          begin
            EditStyle := esSimple;
            MaxLength := 5;
          end;

          with ItemProps[InsertRow(MsgPropY, IntToStr(Y), True)] do
          begin
            EditStyle := esSimple;
            MaxLength := 5;
          end;

          with ItemProps[InsertRow(MsgPropDirection, DirNames[Direction], True)] do
          begin
            EditStyle := esPickList;
            ReadOnly := True;
          end;
        end;
      end;

    OBJECT_TRIGGER:
      begin
        with MainForm.vleObjectProperty,
             gTriggers[SelectedObjects[_id].ID] do
        begin
          with ItemProps[InsertRow(MsgPropTrType, GetTriggerName(TriggerType), True)] do
          begin
            EditStyle := esSimple;
            ReadOnly := True;
          end;

          with ItemProps[InsertRow(MsgPropX, IntToStr(X), True)] do
          begin
            EditStyle := esSimple;
            MaxLength := 5;
          end;

          with ItemProps[InsertRow(MsgPropY, IntToStr(Y), True)] do
          begin
            EditStyle := esSimple;
            MaxLength := 5;
          end;

          with ItemProps[InsertRow(MsgPropWidth, IntToStr(Width), True)] do
          begin
            EditStyle := esSimple;
            MaxLength := 5;
          end;

          with ItemProps[InsertRow(MsgPropHeight, IntToStr(Height), True)] do
          begin
            EditStyle := esSimple;
            MaxLength := 5;
          end;

          with ItemProps[InsertRow(MsgPropTrEnabled, BoolNames[Enabled], True)] do
          begin
            EditStyle := esPickList;
            ReadOnly := True;
          end;

          with ItemProps[InsertRow(MsgPropTrTexturePanel, IntToStr(TexturePanel), True)] do
          begin
            EditStyle := esEllipsis;
            ReadOnly := True;
          end;

          with ItemProps[InsertRow(MsgPropTrActivation, ActivateToStr(ActivateType), True)] do
          begin
            EditStyle := esEllipsis;
            ReadOnly := True;
          end;

          with ItemProps[InsertRow(MsgPropTrKeys, KeyToStr(Key), True)] do
          begin
            EditStyle := esEllipsis;
            ReadOnly := True;
          end;

          case TriggerType of
            TRIGGER_EXIT:
              begin
                str := win2utf(Data.MapName);
                with ItemProps[InsertRow(MsgPropTrNextMap, str, True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;
              end;

            TRIGGER_TELEPORT:
              begin
                with ItemProps[InsertRow(MsgPropTrTeleportTo, Format('(%d:%d)', [Data.TargetPoint.X, Data.TargetPoint.Y]), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrD2d, BoolNames[Data.d2d_teleport], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrTeleportSilent, BoolNames[Data.silent_teleport], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrTeleportDir, DirNamesAdv[Data.TlpDir], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
              end;

            TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR,
            TRIGGER_DOOR, TRIGGER_DOOR5:
              begin
                with ItemProps[InsertRow(MsgPropTrDoorPanel, IntToStr(Data.PanelID), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrSilent, BoolNames[Data.NoSound], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrD2d, BoolNames[Data.d2d_doors], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
              end;

            TRIGGER_CLOSETRAP, TRIGGER_TRAP:
              begin
                with ItemProps[InsertRow(MsgPropTrTrapPanel, IntToStr(Data.PanelID), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrSilent, BoolNames[Data.NoSound], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrD2d, BoolNames[Data.d2d_doors], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
              end;

            TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF,
            TRIGGER_ONOFF:
              begin
                with ItemProps[InsertRow(MsgPropTrExArea,
                                 Format('(%d:%d %d:%d)', [Data.tX, Data.tY, Data.tWidth, Data.tHeight]), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrExDelay, IntToStr(Data.Wait), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;

                with ItemProps[InsertRow(MsgPropTrExCount, IntToStr(Data.Count), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;

                with ItemProps[InsertRow(MsgPropTrExMonster, IntToStr(Data.MonsterID-1), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                if TriggerType = TRIGGER_PRESS then
                  with ItemProps[InsertRow(MsgPropTrExRandom, BoolNames[Data.ExtRandom], True)] do
                  begin
                    EditStyle := esPickList;
                    ReadOnly := True;
                  end;
              end;

            TRIGGER_SECRET:
              ;

            TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT:
              begin
                with ItemProps[InsertRow(MsgPropTrLiftPanel, IntToStr(Data.PanelID), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrSilent, BoolNames[Data.NoSound], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrD2d, BoolNames[Data.d2d_doors], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
              end;

            TRIGGER_TEXTURE:
              begin
                with ItemProps[InsertRow(MsgPropTrTextureOnce, BoolNames[Data.ActivateOnce], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrTextureAnimOnce, BoolNames[Data.AnimOnce], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
              end;

            TRIGGER_SOUND:
              begin
                str := win2utf(Data.SoundName);
                with ItemProps[InsertRow(MsgPropTrSoundName, str, True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrSoundVolume, IntToStr(Data.Volume), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 3;
                end;

                with ItemProps[InsertRow(MsgPropTrSoundPan, IntToStr(Data.Pan), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 3;
                end;

                with ItemProps[InsertRow(MsgPropTrSoundCount, IntToStr(Data.PlayCount), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 3;
                end;

                with ItemProps[InsertRow(MsgPropTrSoundLocal, BoolNames[Data.Local], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrSoundSwitch, BoolNames[Data.SoundSwitch], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
              end;

            TRIGGER_SPAWNMONSTER:
              begin
                with ItemProps[InsertRow(MsgPropTrMonsterType, MonsterToStr(Data.MonType), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrSpawnTo,
                                 Format('(%d:%d)', [Data.MonPos.X, Data.MonPos.Y]), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropDirection, DirNames[TDirection(Data.MonDir)], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrHealth, IntToStr(Data.MonHealth), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;

                with ItemProps[InsertRow(MsgPropTrMonsterActive, BoolNames[Data.MonActive], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrCount, IntToStr(Data.MonCount), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;

                with ItemProps[InsertRow(MsgPropTrFxType, EffectToStr(Data.MonEffect), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrSpawnMax, IntToStr(Data.MonMax), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;

                with ItemProps[InsertRow(MsgPropTrSpawnDelay, IntToStr(Data.MonDelay), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;

                case Data.MonBehav of
                  1: str := MsgPropTrMonsterBehaviour1;
                  2: str := MsgPropTrMonsterBehaviour2;
                  3: str := MsgPropTrMonsterBehaviour3;
                  4: str := MsgPropTrMonsterBehaviour4;
                  5: str := MsgPropTrMonsterBehaviour5;
                  else str := MsgPropTrMonsterBehaviour0;
                end;
                with ItemProps[InsertRow(MsgPropTrMonsterBehaviour, str, True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
              end;

            TRIGGER_SPAWNITEM:
              begin
                with ItemProps[InsertRow(MsgPropTrItemType, ItemToStr(Data.ItemType), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrSpawnTo,
                                 Format('(%d:%d)', [Data.ItemPos.X, Data.ItemPos.Y]), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropDmOnly, BoolNames[Data.ItemOnlyDM], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropItemFalls, BoolNames[Data.ItemFalls], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrCount, IntToStr(Data.ItemCount), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;

                with ItemProps[InsertRow(MsgPropTrFxType, EffectToStr(Data.ItemEffect), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrSpawnMax, IntToStr(Data.ItemMax), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;

                with ItemProps[InsertRow(MsgPropTrSpawnDelay, IntToStr(Data.ItemDelay), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;
              end;

           TRIGGER_MUSIC:
             begin
               str := win2utf(Data.MusicName);
               with ItemProps[InsertRow(MsgPropTrMusicName, str, True)] do
               begin
                 EditStyle := esEllipsis;
                 ReadOnly := True;
               end;

               if Data.MusicAction = 1 then
                 str := MsgPropTrMusicOn
               else
                 str := MsgPropTrMusicOff;

               with ItemProps[InsertRow(MsgPropTrMusicAct, str, True)] do
               begin
                 EditStyle := esPickList;
                 ReadOnly := True;
               end;
             end;

            TRIGGER_PUSH:
              begin
                with ItemProps[InsertRow(MsgPropTrPushAngle, IntToStr(Data.PushAngle), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 4;
                end;
                with ItemProps[InsertRow(MsgPropTrPushForce, IntToStr(Data.PushForce), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 4;
                end;
                with ItemProps[InsertRow(MsgPropTrPushReset, BoolNames[Data.ResetVel], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
              end;

            TRIGGER_SCORE:
              begin
                case Data.ScoreAction of
                  1: str := MsgPropTrScoreAct1;
                  2: str := MsgPropTrScoreAct2;
                  3: str := MsgPropTrScoreAct3;
                  else str := MsgPropTrScoreAct0;
                end;
                with ItemProps[InsertRow(MsgPropTrScoreAct, str, True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
                with ItemProps[InsertRow(MsgPropTrCount, IntToStr(Data.ScoreCount), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 3;
                end;
                case Data.ScoreTeam of
                  1: str := MsgPropTrScoreTeam1;
                  2: str := MsgPropTrScoreTeam2;
                  3: str := MsgPropTrScoreTeam3;
                  else str := MsgPropTrScoreTeam0;
                end;
                with ItemProps[InsertRow(MsgPropTrScoreTeam, str, True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
                with ItemProps[InsertRow(MsgPropTrScoreCon, BoolNames[Data.ScoreCon], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
                with ItemProps[InsertRow(MsgPropTrScoreMsg, BoolNames[Data.ScoreMsg], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
              end;

            TRIGGER_MESSAGE:
              begin
                case Data.MessageKind of
                  1: str := MsgPropTrMessageKind1;
                  else str := MsgPropTrMessageKind0;
                end;
                with ItemProps[InsertRow(MsgPropTrMessageKind, str, True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
                case Data.MessageSendTo of
                  1: str := MsgPropTrMessageTo1;
                  2: str := MsgPropTrMessageTo2;
                  3: str := MsgPropTrMessageTo3;
                  4: str := MsgPropTrMessageTo4;
                  5: str := MsgPropTrMessageTo5;
                  else str := MsgPropTrMessageTo0;
                end;
                with ItemProps[InsertRow(MsgPropTrMessageTo, str, True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
                str := win2utf(Data.MessageText);
                with ItemProps[InsertRow(MsgPropTrMessageText, str, True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 100;
                end;
                with ItemProps[InsertRow(MsgPropTrMessageTime, IntToStr(Data.MessageTime), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;
              end;

            TRIGGER_DAMAGE:
              begin
                with ItemProps[InsertRow(MsgPropTrDamageValue, IntToStr(Data.DamageValue), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;
                with ItemProps[InsertRow(MsgPropTrInterval, IntToStr(Data.DamageInterval), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;
                case Data.DamageKind of
                  3: str := MsgPropTrDamageKind3;
                  4: str := MsgPropTrDamageKind4;
                  5: str := MsgPropTrDamageKind5;
                  6: str := MsgPropTrDamageKind6;
                  7: str := MsgPropTrDamageKind7;
                  8: str := MsgPropTrDamageKind8;
                  else str := MsgPropTrDamageKind0;
                end;
                with ItemProps[InsertRow(MsgPropTrDamageKind, str, True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
              end;

            TRIGGER_HEALTH:
              begin
                with ItemProps[InsertRow(MsgPropTrHealth, IntToStr(Data.HealValue), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;
                with ItemProps[InsertRow(MsgPropTrInterval, IntToStr(Data.HealInterval), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;
                with ItemProps[InsertRow(MsgPropTrHealthMax, BoolNames[Data.HealMax], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
                with ItemProps[InsertRow(MsgPropTrSilent, BoolNames[Data.HealSilent], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;
              end;

            TRIGGER_SHOT:
              begin
                with ItemProps[InsertRow(MsgPropTrShotType, ShotToStr(Data.ShotType), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrShotSound, BoolNames[Data.ShotSound], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrShotPanel, IntToStr(Data.ShotPanelID), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                case Data.ShotTarget of
                  1: str := MsgPropTrShotTo1;
                  2: str := MsgPropTrShotTo2;
                  3: str := MsgPropTrShotTo3;
                  4: str := MsgPropTrShotTo4;
                  5: str := MsgPropTrShotTo5;
                  6: str := MsgPropTrShotTo6;
                  else str := MsgPropTrShotTo0;
                end;
                with ItemProps[InsertRow(MsgPropTrShotTo, str, True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrShotSight, IntToStr(Data.ShotIntSight), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 3;
                end;

                case Data.ShotAim of
                  1: str := MsgPropTrShotAim1;
                  2: str := MsgPropTrShotAim2;
                  3: str := MsgPropTrShotAim3;
                  else str := MsgPropTrShotAim0;
                end;
                with ItemProps[InsertRow(MsgPropTrShotAim, str, True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrSpawnTo,
                                 Format('(%d:%d)', [Data.ShotPos.X, Data.ShotPos.Y]), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrShotAngle, IntToStr(Data.ShotAngle), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 4;
                end;

                with ItemProps[InsertRow(MsgPropTrExDelay, IntToStr(Data.ShotWait), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;

                with ItemProps[InsertRow(MsgPropTrShotAcc, IntToStr(Data.ShotAccuracy), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;

                with ItemProps[InsertRow(MsgPropTrShotAmmo, IntToStr(Data.ShotAmmo), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;

                with ItemProps[InsertRow(MsgPropTrShotReload, IntToStr(Data.ShotIntReload), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 4;
                end;
              end;

            TRIGGER_EFFECT:
              begin
                with ItemProps[InsertRow(MsgPropTrCount, IntToStr(Data.FXCount), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 3;
                end;

                if Data.FXType = 0 then
                  str := MsgPropTrEffectParticle
                else
                  str := MsgPropTrEffectAnimation;
                with ItemProps[InsertRow(MsgPropTrEffectType, str, True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                str := '';
                if Data.FXType = 0 then
                  case Data.FXSubType of
                    TRIGGER_EFFECT_SLIQUID:
                      str := MsgPropTrEffectSliquid;
                    TRIGGER_EFFECT_LLIQUID:
                      str := MsgPropTrEffectLliquid;
                    TRIGGER_EFFECT_DLIQUID:
                      str := MsgPropTrEffectDliquid;
                    TRIGGER_EFFECT_BLOOD:
                      str := MsgPropTrEffectBlood;
                    TRIGGER_EFFECT_SPARK:
                      str := MsgPropTrEffectSpark;
                    TRIGGER_EFFECT_BUBBLE:
                      str := MsgPropTrEffectBubble;
                  end;
                if Data.FXType = 1 then
                begin
                  if (Data.FXSubType = 0) or (Data.FXSubType > EFFECT_FIRE) then
                    Data.FXSubType := EFFECT_TELEPORT;
                  str := EffectToStr(Data.FXSubType);
                end;
                with ItemProps[InsertRow(MsgPropTrEffectSubtype, str, True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrEffectColor, IntToStr(Data.FXColorR or (Data.FXColorG shl 8) or (Data.FXColorB shl 16)), True)] do
                begin
                  EditStyle := esEllipsis;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrEffectCenter, BoolNames[Data.FXPos = 0], True)] do
                begin
                  EditStyle := esPickList;
                  ReadOnly := True;
                end;

                with ItemProps[InsertRow(MsgPropTrExDelay, IntToStr(Data.FXWait), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 5;
                end;

                with ItemProps[InsertRow(MsgPropTrEffectVelx, IntToStr(Data.FXVelX), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 4;
                end;

                with ItemProps[InsertRow(MsgPropTrEffectVely, IntToStr(Data.FXVelY), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 4;
                end;

                with ItemProps[InsertRow(MsgPropTrEffectSpl, IntToStr(Data.FXSpreadL), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 3;
                end;

                with ItemProps[InsertRow(MsgPropTrEffectSpr, IntToStr(Data.FXSpreadR), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 3;
                end;

                with ItemProps[InsertRow(MsgPropTrEffectSpu, IntToStr(Data.FXSpreadU), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 3;
                end;

                with ItemProps[InsertRow(MsgPropTrEffectSpd, IntToStr(Data.FXSpreadD), True)] do
                begin
                  EditStyle := esSimple;
                  MaxLength := 3;
                end;
              end;
          end; //case TriggerType
        end;
      end; // OBJECT_TRIGGER:
  end;
end;

procedure ChangeShownProperty(Name: String; NewValue: String);
var
  row: Integer;
begin
  if SelectedObjectCount() <> 1 then
    Exit;
  if not SelectedObjects[GetFirstSelected()].Live then
    Exit;

// Есть ли такой ключ:
  if MainForm.vleObjectProperty.FindRow(Name, row) then
  begin
    MainForm.vleObjectProperty.Values[Name] := NewValue;
  end;
end;

procedure SelectObject(fObjectType: Byte; fID: DWORD; Multi: Boolean);
var
  a: Integer;
  b: Boolean;
begin
  if Multi then
    begin
      b := False;

    // Уже выделен - убираем:
      if SelectedObjects <> nil then
        for a := 0 to High(SelectedObjects) do
          with SelectedObjects[a] do
           if Live and (ID = fID) and
              (ObjectType = fObjectType) then
           begin
             Live := False;
             b := True;
           end;

      if b then
        Exit;

      SetLength(SelectedObjects, Length(SelectedObjects)+1);

      with SelectedObjects[High(SelectedObjects)] do
      begin
        ObjectType := fObjectType;
        ID := fID;
        Live := True;
      end;
    end
  else // not Multi
    begin
      SetLength(SelectedObjects, 1);

      with SelectedObjects[0] do
      begin
        ObjectType := fObjectType;
        ID := fID;
        Live := True;
      end;
    end;

  MainForm.miCopy.Enabled := True;
  MainForm.miCut.Enabled := True;

  if fObjectType = OBJECT_PANEL then
  begin
    MainForm.miToFore.Enabled := True;
    MainForm.miToBack.Enabled := True;
  end;
end;

procedure RemoveSelectFromObjects();
begin
  SelectedObjects := nil;
  DrawPressRect := False;
  MouseLDown := False;
  MouseRDown := False;
  MouseAction := MOUSEACTION_NONE;
  SelectFlag := SELECTFLAG_NONE;
  ResizeType := RESIZETYPE_NONE;
  ResizeDirection := RESIZEDIR_NONE;

  MainForm.vleObjectProperty.Strings.Clear();
  
  MainForm.miCopy.Enabled := False;
  MainForm.miCut.Enabled := False;
  MainForm.miToFore.Enabled := False;
  MainForm.miToBack.Enabled := False;
end;

procedure DeleteSelectedObjects();
var
  i, a, ii: Integer;
  b: Boolean;
begin
  if SelectedObjects = nil then
    Exit;

  b := False;
  i := 0;

  for a := 0 to High(SelectedObjects) do
    with SelectedObjects[a] do
      if Live then
      begin
        if not b then
        begin
          SetLength(UndoBuffer, Length(UndoBuffer)+1);
          i := High(UndoBuffer);
          b := True;
        end;

        SetLength(UndoBuffer[i], Length(UndoBuffer[i])+1);
        ii := High(UndoBuffer[i]);

        case ObjectType of
          OBJECT_PANEL:
            begin
              UndoBuffer[i, ii].UndoType := UNDO_DELETE_PANEL;
              New(UndoBuffer[i, ii].Panel);
              UndoBuffer[i, ii].Panel^ := gPanels[ID];
            end;
          OBJECT_ITEM:
            begin
              UndoBuffer[i, ii].UndoType := UNDO_DELETE_ITEM;
              UndoBuffer[i, ii].Item := gItems[ID];
            end;
          OBJECT_AREA:
            begin
              UndoBuffer[i, ii].UndoType := UNDO_DELETE_AREA;
              UndoBuffer[i, ii].Area := gAreas[ID];
            end;
          OBJECT_TRIGGER:
            begin
              UndoBuffer[i, ii].UndoType := UNDO_DELETE_TRIGGER;
              UndoBuffer[i, ii].Trigger := gTriggers[ID];
            end;
        end;

        RemoveObject(ID, ObjectType);
      end;

  RemoveSelectFromObjects();

  MainForm.miUndo.Enabled := UndoBuffer <> nil;
  MainForm.RecountSelectedObjects();
end;

procedure Undo_Add(ObjectType: Byte; ID: DWORD; Group: Boolean = False);
var
  i, ii: Integer;
begin
  if (not Group) or (Length(UndoBuffer) = 0) then
    SetLength(UndoBuffer, Length(UndoBuffer)+1);
  SetLength(UndoBuffer[High(UndoBuffer)], Length(UndoBuffer[High(UndoBuffer)])+1);
  i := High(UndoBuffer);
  ii := High(UndoBuffer[i]);

  case ObjectType of
    OBJECT_PANEL:
      UndoBuffer[i, ii].UndoType := UNDO_ADD_PANEL;
    OBJECT_ITEM:
      UndoBuffer[i, ii].UndoType := UNDO_ADD_ITEM;
    OBJECT_MONSTER:
      UndoBuffer[i, ii].UndoType := UNDO_ADD_MONSTER;
    OBJECT_AREA:
      UndoBuffer[i, ii].UndoType := UNDO_ADD_AREA;
    OBJECT_TRIGGER:
      UndoBuffer[i, ii].UndoType := UNDO_ADD_TRIGGER;
  end;

  UndoBuffer[i, ii].AddID := ID;

  MainForm.miUndo.Enabled := UndoBuffer <> nil;
end;

procedure FullClear();
begin
  RemoveSelectFromObjects();
  ClearMap();
  LoadSky(gMapInfo.SkyName);
  UndoBuffer := nil;
  slInvalidTextures.Clear();
  MapCheckForm.lbErrorList.Clear();
  MapCheckForm.mErrorDescription.Clear();

  MainForm.miUndo.Enabled := False;
  MainForm.sbHorizontal.Position := 0;
  MainForm.sbVertical.Position := 0;
  MainForm.FormResize(nil);
  MainForm.Caption := FormCaption;
  OpenedMap := '';
  OpenedWAD := '';
end;

procedure ErrorMessageBox(str: String);
begin
  Application.MessageBox(PChar(str), PChar(MsgMsgError),
             MB_ICONINFORMATION or MB_OK or MB_DEFBUTTON1);
end;

function CheckProperty(): Boolean;
var
  _id: Integer;
begin
  Result := False;

  _id := GetFirstSelected();

  if SelectedObjects[_id].ObjectType = OBJECT_PANEL then
    with gPanels[SelectedObjects[_id].ID] do
    begin
      if TextureWidth <> 0 then
        if StrToIntDef(MainForm.vleObjectProperty.Values[MsgPropWidth], 1) mod TextureWidth <> 0 then
        begin
          ErrorMessageBox(Format(MsgMsgWrongTexwidth,
                                 [TextureWidth]));
          Exit;
        end;

      if TextureHeight <> 0 then
        if StrToIntDef(Trim(MainForm.vleObjectProperty.Values[MsgPropHeight]), 1) mod TextureHeight <> 0 then
        begin
          ErrorMessageBox(Format(MsgMsgWrongTexheight,
                                 [TextureHeight]));
          Exit;
        end;

      if IsTexturedPanel(PanelType) and (TextureName <> '') then
        if not (StrToIntDef(MainForm.vleObjectProperty.Values[MsgPropPanelAlpha], -1) in [0..255]) then
        begin
          ErrorMessageBox(MsgMsgWrongAlpha);
          Exit;
        end;
    end;

  if SelectedObjects[_id].ObjectType in [OBJECT_PANEL, OBJECT_TRIGGER] then
    if (StrToIntDef(MainForm.vleObjectProperty.Values[MsgPropWidth], 0) <= 0) or
       (StrToIntDef(MainForm.vleObjectProperty.Values[MsgPropHeight], 0) <= 0) then
    begin
      ErrorMessageBox(MsgMsgWrongSize);
      Exit;
    end;

  if (Trim(MainForm.vleObjectProperty.Values[MsgPropX]) = '') or
     (Trim(MainForm.vleObjectProperty.Values[MsgPropY]) = '') then
  begin
    ErrorMessageBox(MsgMsgWrongXy);
    Exit;
  end;

  Result := True;
end;

procedure SelectTexture(ID: Integer);
begin
  MainForm.lbTextureList.ItemIndex := ID;
  MainForm.lbTextureListClick(nil);
end;

function AddTexture(aWAD, aSection, aTex: String; silent: Boolean): Boolean;
var
  a, FrameLen: Integer;
  ok: Boolean;
  FileName: String;
  ResourceName: String;
  FullResourceName: String;
  SectionName: String;
  Data: Pointer;
  Width, Height: Word;
  fn: String;
begin
  Data := nil;
  FrameLen := 0;
  Width := 0;
  Height := 0;

  if aSection = '..' then
    SectionName := ''
  else
    SectionName := aSection;

  if aWAD = '' then
    aWAD := MsgWadSpecialMap;

  if aWAD = MsgWadSpecialMap then
    begin // Файл карты
      g_ProcessResourceStr(OpenedMap, @fn, nil, nil);
      FileName := fn;
      ResourceName := ':'+SectionName+'\'+aTex;
    end
  else
    if aWAD = MsgWadSpecialTexs then
      begin // Спец. текстуры
        FileName := '';
        ResourceName := aTex;
      end
    else
      begin // Внешний WAD
        FileName := WadsDir + DirectorySeparator + aWAD;
        ResourceName := aWAD+':'+SectionName+'\'+aTex;
      end;

  ok := True;

// Есть ли уже такая текстура:
  for a := 0 to MainForm.lbTextureList.Items.Count-1 do
    if ResourceName = MainForm.lbTextureList.Items[a] then
    begin
      if not silent then
        ErrorMessageBox(Format(MsgMsgTextureAlready,
                               [ResourceName]));
      ok := False;
    end;

// Название ресурса <= 64 символов:
  if Length(ResourceName) > 64 then
  begin
    if not silent then
      ErrorMessageBox(Format(MsgMsgResName64,
                             [ResourceName]));
    ok := False;
  end;

  if ok then
  begin
    a := -1;
    if aWAD = MsgWadSpecialTexs then
    begin
      a := MainForm.lbTextureList.Items.Add(ResourceName);
      if not silent then
        SelectTexture(a);
      Result := True;
      Exit;
    end;

    FullResourceName := FileName+':'+SectionName+'\'+aTex;

    if IsAnim(FullResourceName) then
      begin // Аним. текстура
        GetFrame(FullResourceName, Data, FrameLen, Width, Height);

        if not g_CreateTextureMemorySize(Data, FrameLen, ResourceName, 0, 0, Width, Height, 1) then
          ok := False;
        a := MainForm.lbTextureList.Items.Add(ResourceName);
      end
    else // Обычная текстура
      begin
        if not g_CreateTextureWAD(ResourceName, FullResourceName) then
          ok := False;
        a := MainForm.lbTextureList.Items.Add(ResourceName);
      end;
    if (not ok) and (slInvalidTextures.IndexOf(ResourceName) = -1) then
    begin
      slInvalidTextures.Add(ResourceName);
      ok := True;
    end;
    if (a > -1) and (not silent) then
      SelectTexture(a);
  end;

  Result := ok;
end;

procedure UpdateCaption(sMap, sFile, sRes: String);
begin
  with MainForm do
    if (sFile = '') and (sRes = '') and (sMap = '') then
      Caption := FormCaption
    else
      if sMap = '' then
        Caption := Format('%s - %s:%s', [FormCaption, sFile, sRes])
      else
        if (sFile <> '') and (sRes <> '') then
          Caption := Format('%s - %s (%s:%s)', [FormCaption, sMap, sFile, sRes])
        else
          Caption := Format('%s - %s', [FormCaption, sMap]);
end;

procedure OpenMap(FileName: String; mapN: String);
var
  MapName: String;
  idx: Integer;
begin
  SelectMapForm.Caption := MsgCapOpen;
  SelectMapForm.GetMaps(FileName);

  if (FileName = OpenedWAD) and
     (OpenedMap <> '') then
  begin
    MapName := OpenedMap;
    while (Pos(':\', MapName) > 0) do
      Delete(MapName, 1, Pos(':\', MapName) + 1);

    idx := SelectMapForm.lbMapList.Items.IndexOf(MapName);
    SelectMapForm.lbMapList.ItemIndex := idx;
  end
  else
    if SelectMapForm.lbMapList.Count > 0 then
      SelectMapForm.lbMapList.ItemIndex := 0
    else
      SelectMapForm.lbMapList.ItemIndex := -1;

  if mapN = '' then
    idx := -1
  else
    idx := SelectMapForm.lbMapList.Items.IndexOf(mapN);

  if idx < 0 then
  begin
    if (SelectMapForm.ShowModal() = mrOK) and
       (SelectMapForm.lbMapList.ItemIndex <> -1) then
      idx := SelectMapForm.lbMapList.ItemIndex
    else
      Exit;
  end;

  MapName := SelectMapForm.lbMapList.Items[idx];

  with MainForm do
  begin
    FullClear();

    pLoadProgress.Left := (RenderPanel.Width div 2)-(pLoadProgress.Width div 2);
    pLoadProgress.Top := (RenderPanel.Height div 2)-(pLoadProgress.Height div 2);
    pLoadProgress.Show();

    OpenedMap := FileName+':\'+MapName;
    OpenedWAD := FileName;

    idx := RecentFiles.IndexOf(OpenedMap);
  // Такая карта уже недавно открывалась:
    if idx >= 0 then
      RecentFiles.Delete(idx);
    RecentFiles.Insert(0, OpenedMap);
    RefreshRecentMenu();

    LoadMap(OpenedMap);

    pLoadProgress.Hide();
    FormResize(nil);

    lbTextureList.Sorted := True;
    lbTextureList.Sorted := False;

    UpdateCaption(gMapInfo.Name, ExtractFileName(FileName), MapName);
  end;
end;

procedure MoveSelectedObjects(Wall, alt: Boolean; dx, dy: Integer);
var
  okX, okY: Boolean;
  a: Integer;
begin
  if SelectedObjects = nil then
    Exit;

  okX := True;
  okY := True;

  if Wall then
    for a := 0 to High(SelectedObjects) do
      if SelectedObjects[a].Live then
      begin
        if ObjectCollideLevel(SelectedObjects[a].ID, SelectedObjects[a].ObjectType, dx, 0) then
          okX := False;

        if ObjectCollideLevel(SelectedObjects[a].ID, SelectedObjects[a].ObjectType, 0, dy) then
          okY := False;

        if (not okX) or (not okY) then
          Break;
      end;

  if okX or okY then
  begin
    for a := 0 to High(SelectedObjects) do
      if SelectedObjects[a].Live then
      begin
        if okX then
          MoveObject(SelectedObjects[a].ObjectType, SelectedObjects[a].ID, dx, 0);

        if okY then
          MoveObject(SelectedObjects[a].ObjectType, SelectedObjects[a].ID, 0, dy);

        if alt and (SelectedObjects[a].ObjectType = OBJECT_TRIGGER) then
        begin
          if gTriggers[SelectedObjects[a].ID].TriggerType in [TRIGGER_PRESS,
               TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF] then
          begin // Двигаем зону Расширителя
            if okX then
              gTriggers[SelectedObjects[a].ID].Data.tX := gTriggers[SelectedObjects[a].ID].Data.tX+dx;
            if okY then
              gTriggers[SelectedObjects[a].ID].Data.tY := gTriggers[SelectedObjects[a].ID].Data.tY+dy;
          end;

          if gTriggers[SelectedObjects[a].ID].TriggerType in [TRIGGER_TELEPORT] then
          begin // Двигаем точку назначения Телепорта
            if okX then
              gTriggers[SelectedObjects[a].ID].Data.TargetPoint.X := gTriggers[SelectedObjects[a].ID].Data.TargetPoint.X+dx;
            if okY then
              gTriggers[SelectedObjects[a].ID].Data.TargetPoint.Y := gTriggers[SelectedObjects[a].ID].Data.TargetPoint.Y+dy;
          end;

          if gTriggers[SelectedObjects[a].ID].TriggerType in [TRIGGER_SPAWNMONSTER] then
          begin // Двигаем точку создания монстра
            if okX then
              gTriggers[SelectedObjects[a].ID].Data.MonPos.X := gTriggers[SelectedObjects[a].ID].Data.MonPos.X+dx;
            if okY then
              gTriggers[SelectedObjects[a].ID].Data.MonPos.Y := gTriggers[SelectedObjects[a].ID].Data.MonPos.Y+dy;
          end;

          if gTriggers[SelectedObjects[a].ID].TriggerType in [TRIGGER_SPAWNITEM] then
          begin // Двигаем точку создания предмета
            if okX then
              gTriggers[SelectedObjects[a].ID].Data.ItemPos.X := gTriggers[SelectedObjects[a].ID].Data.ItemPos.X+dx;
            if okY then
              gTriggers[SelectedObjects[a].ID].Data.ItemPos.Y := gTriggers[SelectedObjects[a].ID].Data.ItemPos.Y+dy;
          end;

          if gTriggers[SelectedObjects[a].ID].TriggerType in [TRIGGER_SHOT] then
          begin // Двигаем точку создания выстрела
            if okX then
              gTriggers[SelectedObjects[a].ID].Data.ShotPos.X := gTriggers[SelectedObjects[a].ID].Data.ShotPos.X+dx;
            if okY then
              gTriggers[SelectedObjects[a].ID].Data.ShotPos.Y := gTriggers[SelectedObjects[a].ID].Data.ShotPos.Y+dy;
          end;
        end;
      end;

    LastMovePoint := MousePos;
  end;
end;

procedure ShowLayer(Layer: Byte; show: Boolean);
begin
  LayerEnabled[Layer] := show;

  case Layer of
    LAYER_BACK:
      begin
        MainForm.miLayer1.Checked := show;
        MainForm.miLayerP1.Checked := show;
      end;
    LAYER_WALLS:
      begin
        MainForm.miLayer2.Checked := show;
        MainForm.miLayerP2.Checked := show;
      end;
    LAYER_FOREGROUND:
      begin
        MainForm.miLayer3.Checked := show;
        MainForm.miLayerP3.Checked := show;
      end;
    LAYER_STEPS:
      begin
        MainForm.miLayer4.Checked := show;
        MainForm.miLayerP4.Checked := show;
      end;
    LAYER_WATER:
      begin
        MainForm.miLayer5.Checked := show;
        MainForm.miLayerP5.Checked := show;
      end;
    LAYER_ITEMS:
      begin
        MainForm.miLayer6.Checked := show;
        MainForm.miLayerP6.Checked := show;
      end;
    LAYER_MONSTERS:
      begin
        MainForm.miLayer7.Checked := show;
        MainForm.miLayerP7.Checked := show;
      end;
    LAYER_AREAS:
      begin
        MainForm.miLayer8.Checked := show;
        MainForm.miLayerP8.Checked := show;
      end;
    LAYER_TRIGGERS:
      begin
        MainForm.miLayer9.Checked := show;
        MainForm.miLayerP9.Checked := show;
      end;
  end;

  RemoveSelectFromObjects();
end;

procedure SwitchLayer(Layer: Byte);
begin
  ShowLayer(Layer, not LayerEnabled[Layer]);
end;

procedure SwitchMap();
begin
  ShowMap := not ShowMap;
  MainForm.tbShowMap.Down := ShowMap;
  MainForm.miMiniMap.Checked := ShowMap;
end;

procedure ShowEdges();
begin
  if drEdge[3] < 255 then
    drEdge[3] := 255
  else
    drEdge[3] := gAlphaEdge;
  MainForm.miShowEdges.Checked := drEdge[3] <> 255;
end;

function SelectedTexture(): String;
begin
  if MainForm.lbTextureList.ItemIndex <> -1 then
    Result := MainForm.lbTextureList.Items[MainForm.lbTextureList.ItemIndex]
  else
    Result := '';
end;

function IsSpecialTextureSel(): Boolean;
begin
  Result := (MainForm.lbTextureList.ItemIndex <> -1) and
            IsSpecialTexture(MainForm.lbTextureList.Items[MainForm.lbTextureList.ItemIndex]);
end;

function CopyBufferToString(var CopyBuf: TCopyRecArray): String;
var
  i, j: Integer;
  Res: String;

  procedure AddInt(x: Integer);
  begin
    Res := Res + IntToStr(x) + ' ';
  end;

begin
  Result := '';

  if Length(CopyBuf) = 0 then
    Exit;

  Res := CLIPBOARD_SIG + ' ';

  for i := 0 to High(CopyBuf) do
  begin
    if (CopyBuf[i].ObjectType = OBJECT_PANEL) and
       (CopyBuf[i].Panel = nil) then
      Continue;

  // Тип объекта:
    AddInt(CopyBuf[i].ObjectType);
    Res := Res + '; ';

  // Свойства объекта:
    case CopyBuf[i].ObjectType of
      OBJECT_PANEL:
        with CopyBuf[i].Panel^ do
        begin
          AddInt(PanelType);
          AddInt(X);
          AddInt(Y);
          AddInt(Width);
          AddInt(Height);
          Res := Res + '"' + TextureName + '" ';
          AddInt(Alpha);
          AddInt(IfThen(Blending, 1, 0));
        end;

      OBJECT_ITEM:
        with CopyBuf[i].Item do
        begin
          AddInt(ItemType);
          AddInt(X);
          AddInt(Y);
          AddInt(IfThen(OnlyDM, 1, 0));
          AddInt(IfThen(Fall, 1, 0));
        end;

      OBJECT_MONSTER:
        with CopyBuf[i].Monster do
        begin
          AddInt(MonsterType);
          AddInt(X);
          AddInt(Y);
          AddInt(IfThen(Direction = D_LEFT, 1, 0));
        end;

      OBJECT_AREA:
        with CopyBuf[i].Area do
        begin
          AddInt(AreaType);
          AddInt(X);
          AddInt(Y);
          AddInt(IfThen(Direction = D_LEFT, 1, 0));
        end;

      OBJECT_TRIGGER:
        with CopyBuf[i].Trigger do
        begin
          AddInt(TriggerType);
          AddInt(X);
          AddInt(Y);
          AddInt(Width);
          AddInt(Height);
          AddInt(ActivateType);
          AddInt(Key);
          AddInt(IfThen(Enabled, 1, 0));
          AddInt(TexturePanel);

          for j := 0 to 127 do
            AddInt(Data.Default[j]);
        end;
    end;
  end;

  Result := Res;
end;

procedure StringToCopyBuffer(Str: String; var CopyBuf: TCopyRecArray;
  var pmin: TPoint);
var
  i, j, t: Integer;

  function GetNext(): String;
  var
    p: Integer;

  begin
    if Str[1] = '"' then
      begin
        Delete(Str, 1, 1);
        p := Pos('"', Str);

        if p = 0 then
          begin
            Result := Str;
            Str := '';
          end
        else
          begin
            Result := Copy(Str, 1, p-1);
            Delete(Str, 1, p);
            Str := Trim(Str);
          end;
      end
    else
      begin
        p := Pos(' ', Str);

        if p = 0 then
          begin
            Result := Str;
            Str := '';
          end
        else
          begin
            Result := Copy(Str, 1, p-1);
            Delete(Str, 1, p);
            Str := Trim(Str);
          end;
      end;
  end;

begin
  Str := Trim(Str);

  if GetNext() <> CLIPBOARD_SIG then
    Exit;

  while Str <> '' do
  begin
  // Тип объекта:
    t := StrToIntDef(GetNext(), 0);

    if (t < OBJECT_PANEL) or (t > OBJECT_TRIGGER) or
       (GetNext() <> ';') then
    begin // Что-то не то => пропускаем:
      t := Pos(';', Str);
      Delete(Str, 1, t);
      Str := Trim(Str);

      Continue;
    end;

    i := Length(CopyBuf);
    SetLength(CopyBuf, i + 1);

    CopyBuf[i].ObjectType := t;
    CopyBuf[i].Panel := nil;

  // Свойства объекта:
    case t of
      OBJECT_PANEL:
        begin
          New(CopyBuf[i].Panel);
          
          with CopyBuf[i].Panel^ do
          begin
            PanelType := StrToIntDef(GetNext(), PANEL_WALL);
            X := StrToIntDef(GetNext(), 0);
            Y := StrToIntDef(GetNext(), 0);
            pmin.X := Min(X, pmin.X);
            pmin.Y := Min(Y, pmin.Y);
            Width := StrToIntDef(GetNext(), 16);
            Height := StrToIntDef(GetNext(), 16);
            TextureName := GetNext();
            Alpha := StrToIntDef(GetNext(), 0);
            Blending := (GetNext() = '1');
          end;
        end;

      OBJECT_ITEM:
        with CopyBuf[i].Item do
        begin
          ItemType := StrToIntDef(GetNext(), ITEM_MEDKIT_SMALL);
          X := StrToIntDef(GetNext(), 0);
          Y := StrToIntDef(GetNext(), 0);
          pmin.X := Min(X, pmin.X);
          pmin.Y := Min(Y, pmin.Y);
          OnlyDM := (GetNext() = '1');
          Fall := (GetNext() = '1');
        end;

      OBJECT_MONSTER:
        with CopyBuf[i].Monster do
        begin
          MonsterType := StrToIntDef(GetNext(), MONSTER_DEMON);
          X := StrToIntDef(GetNext(), 0);
          Y := StrToIntDef(GetNext(), 0);
          pmin.X := Min(X, pmin.X);
          pmin.Y := Min(Y, pmin.Y);

          if GetNext() = '1' then
            Direction := D_LEFT
          else
            Direction := D_RIGHT;
        end;

      OBJECT_AREA:
        with CopyBuf[i].Area do
        begin
          AreaType := StrToIntDef(GetNext(), AREA_PLAYERPOINT1);
          X := StrToIntDef(GetNext(), 0);
          Y := StrToIntDef(GetNext(), 0);
          pmin.X := Min(X, pmin.X);
          pmin.Y := Min(Y, pmin.Y);
          if GetNext() = '1' then
            Direction := D_LEFT
          else
            Direction := D_RIGHT;
        end;

      OBJECT_TRIGGER:
        with CopyBuf[i].Trigger do
        begin
          TriggerType := StrToIntDef(GetNext(), TRIGGER_EXIT);
          X := StrToIntDef(GetNext(), 0);
          Y := StrToIntDef(GetNext(), 0);
          pmin.X := Min(X, pmin.X);
          pmin.Y := Min(Y, pmin.Y);
          Width := StrToIntDef(GetNext(), 16);
          Height := StrToIntDef(GetNext(), 16);
          ActivateType := StrToIntDef(GetNext(), 0);
          Key := StrToIntDef(GetNext(), 0);
          Enabled := (GetNext() = '1');
          TexturePanel := StrToIntDef(GetNext(), 0);

          for j := 0 to 127 do
            Data.Default[j] := StrToIntDef(GetNext(), 0);

          case TriggerType of
            TRIGGER_TELEPORT:
              begin
                pmin.X := Min(Data.TargetPoint.X, pmin.X);
                pmin.Y := Min(Data.TargetPoint.Y, pmin.Y);
              end;
            TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF:
              begin
                pmin.X := Min(Data.tX, pmin.X);
                pmin.Y := Min(Data.tY, pmin.Y);
              end;
            TRIGGER_SPAWNMONSTER:
              begin
                pmin.X := Min(Data.MonPos.X, pmin.X);
                pmin.Y := Min(Data.MonPos.Y, pmin.Y);
              end;
            TRIGGER_SPAWNITEM:
              begin
                pmin.X := Min(Data.ItemPos.X, pmin.X);
                pmin.Y := Min(Data.ItemPos.Y, pmin.Y);
              end;
            TRIGGER_SHOT:
              begin
                pmin.X := Min(Data.ShotPos.X, pmin.X);
                pmin.Y := Min(Data.ShotPos.Y, pmin.Y);
              end;
          end;
        end;
    end;
  end;
end;

//----------------------------------------
//Закончились вспомогательные процедуры
//----------------------------------------

type
  TRecentHandler = class
    private
      FForm: TMainForm;
      FPath: String;
    public
      constructor Create (form: TMainForm; path: String);
      procedure Execute (Sender: TObject);
  end;

constructor TRecentHandler.Create (form: TMainForm; path: String);
begin
  Assert(form <> nil);
  FForm := form;
  FPath := path;
end;

procedure TRecentHandler.Execute (Sender: TObject);
  var fn: AnsiString;
begin
  fn := g_ExtractWadName(FPath);
  if FileExists(fn) then
    OpenMap(fn, g_ExtractFilePathName(FPath))
  else
    Application.MessageBox('', 'File not available anymore', MB_OK);
//  if Application.MessageBox(PChar(MsgMsgDelRecentPromt), PChar(MsgMsgDelRecent), MB_ICONQUESTION or MB_YESNO) = idYes then
//  begin
//    RecentFiles.Delete(n);
//    RefreshRecentMenu();
//  end;
end;

procedure TMainForm.RefillRecentMenu (menu: TMenuItem; start: Integer; fmt: AnsiString);
  var i: Integer; MI: TMenuItem; cb: TMethod; h: TRecentHandler; s: AnsiString;
begin
  Assert(menu <> nil);
  Assert(start >= 0);
  Assert(start <= menu.Count);

  // clear all recent entries from menu
  i := start;
  while i < menu.Count do
  begin
    MI := menu.Items[i];
    cb := TMethod(MI.OnClick);
    if cb.Code = @TRecentHandler.Execute then
    begin
      // this is recent menu entry
      // remove it and free callback handler
      h := TRecentHandler(cb.Data);
      menu.Delete(i);
      MI.Free();
      h.Free();
    end
    else
      Inc(i);
  end;

  // fill with a new ones
  for i := 0 to RecentFiles.Count - 1 do
  begin
    s := RecentFiles[i];
    h := TRecentHandler.Create(self, s);
    MI := TMenuItem.Create(menu);
    MI.Caption := Format(fmt, [i + 1, g_ExtractWadNameNoPath(s), g_ExtractFilePathName(s)]);
    MI.OnClick := h.Execute;
    menu.Insert(start + i, MI);
  end;
end;

procedure TMainForm.RefreshRecentMenu();
  var start: Integer;
begin
  while RecentFiles.Count > RecentCount do
    RecentFiles.Delete(RecentFiles.Count - 1);

  if miMacRecentSubMenu.Visible then
  begin
    // Reconstruct OSX-like recent list
    RefillRecentMenu(miMacRecentSubMenu, 0, '%1:s - %2:s');
    miMacRecentEnd.Enabled := RecentFiles.Count <> 0;
    miMacRecentEnd.Visible := RecentFiles.Count <> 0;
  end;

  if miWinRecentStart.Visible then
  begin
    // Reconstruct Windows-like recent list
    start := miMenuFile.IndexOf(miWinRecent);
    if start < 0 then start := miMenuFile.Count else start := start + 1;
    RefillRecentMenu(miMenuFile, start, '%0:d %1:s:%2:s');
    miWinRecent.Enabled := False;
    miWinRecent.Visible := RecentFiles.Count = 0;
  end;
end;

procedure TMainForm.miMacRecentClearClick(Sender: TObject);
begin
  RecentFiles.Clear();
  RefreshRecentMenu();
end;

procedure TMainForm.aEditorOptionsExecute(Sender: TObject);
begin
  OptionsForm.ShowModal();
end;

procedure LoadStdFont(cfgres, texture: string; var FontID: DWORD);
var
  cwdt, chgt: Byte;
  spc: ShortInt;
  ID: DWORD;
  cfgdata: Pointer;
  cfglen: Integer;
  config: TConfig;
begin
  ID := 0;
  g_ReadResource(GameWad, 'FONTS', cfgres, cfgdata, cfglen);
  if cfgdata <> nil then
  begin
    if not g_CreateTextureWAD('FONT_STD', GameWad + ':FONTS\' + texture) then
      e_WriteLog('ERROR ERROR ERROR', MSG_WARNING);

    config := TConfig.CreateMem(cfgdata, cfglen);
    cwdt := Min(Max(config.ReadInt('FontMap', 'CharWidth', 0), 0), 255);
    chgt := Min(Max(config.ReadInt('FontMap', 'CharHeight', 0), 0), 255);
    spc := Min(Max(config.ReadInt('FontMap', 'Kerning', 0), -128), 127);

    if g_GetTexture('FONT_STD', ID) then
      e_TextureFontBuild(ID, FontID, cwdt, chgt, spc - 2);

    config.Free();
    FreeMem(cfgdata)
  end
  else
  begin
    e_WriteLog('Could not load FONT_STD', MSG_WARNING)
  end
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  config: TConfig;
  i: Integer;
  s: String;
begin
  Randomize();

  {$IFDEF DARWIN}
    miApple.Enabled := True;
    miApple.Visible := True;
    miMacRecentSubMenu.Enabled := True;
    miMacRecentSubMenu.Visible := True;
    miWinRecentStart.Enabled := False;
    miWinRecentStart.Visible := False;
    miWinRecent.Enabled := False;
    miWinRecent.Visible := False;
    miLine2.Enabled := False;
    miLine2.Visible := False;
    miExit.Enabled := False;
    miExit.Visible := False;
    miOptions.Enabled := False;
    miOptions.Visible := False;
    miMenuWindow.Enabled := True;
    miMenuWindow.Visible := True;
    miAbout.Enabled := False;
    miAbout.Visible := False;
  {$ELSE}
    miApple.Enabled := False;
    miApple.Visible := False;
    miMacRecentSubMenu.Enabled := False;
    miMacRecentSubMenu.Visible := False;
    miWinRecentStart.Enabled := True;
    miWinRecentStart.Visible := True;
    miWinRecent.Enabled := True;
    miWinRecent.Visible := True;
    miLine2.Enabled := True;
    miLine2.Visible := True;
    miExit.Enabled := True;
    miExit.Visible := True;
    miOptions.Enabled := True;
    miOptions.Visible := True;
    miMenuWindow.Enabled := False;
    miMenuWindow.Visible := False;
    miAbout.Enabled := True;
    miAbout.Visible := True;
  {$ENDIF}

  miNewMap.ShortCut := ShortCut(VK_N, [ssModifier]);
  miOpenMap.ShortCut := ShortCut(VK_O, [ssModifier]);
  miSaveMap.ShortCut := ShortCut(VK_S, [ssModifier]);
  {$IFDEF DARWIN}
    miSaveMapAs.ShortCut := ShortCut(VK_S, [ssModifier, ssShift]);
    miReopenMap.ShortCut := ShortCut(VK_F5, [ssModifier]);
  {$ENDIF}
  miUndo.ShortCut := ShortCut(VK_Z, [ssModifier]);
  miCopy.ShortCut := ShortCut(VK_C, [ssModifier]);
  miCut.ShortCut := ShortCut(VK_X, [ssModifier]);
  miPaste.ShortCut := ShortCut(VK_V, [ssModifier]);
  miSelectAll.ShortCut := ShortCut(VK_A, [ssModifier]);
  miToFore.ShortCut := ShortCut(VK_LCL_CLOSE_BRACKET, [ssModifier]);
  miToBack.ShortCut := ShortCut(VK_LCL_OPEN_BRACKET, [ssModifier]);
  {$IFDEF DARWIN}
    miMapOptions.Shortcut := ShortCut(VK_P, [ssModifier, ssAlt]);
    selectall1.Shortcut := ShortCut(VK_A, [ssModifier, ssAlt]);
  {$ENDIF}

  e_WriteLog('Doom 2D: Forever Editor version ' + EDITOR_VERSION, MSG_NOTIFY);
  e_WriteLog('Build date: ' + EDITOR_BUILDDATE + ' ' + EDITOR_BUILDTIME, MSG_NOTIFY);
  e_WriteLog('Build hash: ' + g_GetBuildHash(), MSG_NOTIFY);
  e_WriteLog('Build by: ' + g_GetBuilderName(), MSG_NOTIFY);

  slInvalidTextures := TStringList.Create;

  ShowLayer(LAYER_BACK, True);
  ShowLayer(LAYER_WALLS, True);
  ShowLayer(LAYER_FOREGROUND, True);
  ShowLayer(LAYER_STEPS, True);
  ShowLayer(LAYER_WATER, True);
  ShowLayer(LAYER_ITEMS, True);
  ShowLayer(LAYER_MONSTERS, True);
  ShowLayer(LAYER_AREAS, True);
  ShowLayer(LAYER_TRIGGERS, True);

  ClearMap();

  FormCaption := MainForm.Caption;
  OpenedMap := '';
  OpenedWAD := '';

  config := TConfig.CreateFile(CfgFileName);

  if config.ReadInt('Editor', 'XPos', -1) = -1 then
    Position := poDesktopCenter
  else begin
    Left := config.ReadInt('Editor', 'XPos', Left);
    Top := config.ReadInt('Editor', 'YPos', Top);
    Width := config.ReadInt('Editor', 'Width', Width);
    Height := config.ReadInt('Editor', 'Height', Height);
  end;
  if config.ReadBool('Editor', 'Maximize', False) then
    WindowState := wsMaximized;
  ShowMap := config.ReadBool('Editor', 'Minimap', False);
  PanelProps.Width := config.ReadInt('Editor', 'PanelProps', PanelProps.ClientWidth);
  Splitter1.Left := PanelProps.Left;
  PanelObjs.Height := config.ReadInt('Editor', 'PanelObjs', PanelObjs.ClientHeight);
  Splitter2.Top := PanelObjs.Top;
  StatusBar.Top := PanelObjs.BoundsRect.Bottom;
  DotEnable := config.ReadBool('Editor', 'DotEnable', True);
  DotColor := config.ReadInt('Editor', 'DotColor', $FFFFFF);
  DotStepOne := config.ReadInt('Editor', 'DotStepOne', 16);
  DotStepTwo := config.ReadInt('Editor', 'DotStepTwo', 8);
  DotStep := config.ReadInt('Editor', 'DotStep', DotStepOne);
  DrawTexturePanel := config.ReadBool('Editor', 'DrawTexturePanel', True);
  DrawPanelSize := config.ReadBool('Editor', 'DrawPanelSize', True);
  BackColor := config.ReadInt('Editor', 'BackColor', $7F6040);
  PreviewColor := config.ReadInt('Editor', 'PreviewColor', $00FF00);
  UseCheckerboard := config.ReadBool('Editor', 'UseCheckerboard', True);
  gColorEdge := config.ReadInt('Editor', 'EdgeColor', COLOR_EDGE);
  gAlphaEdge := config.ReadInt('Editor', 'EdgeAlpha', ALPHA_EDGE);
  if gAlphaEdge = 255 then
    gAlphaEdge := ALPHA_EDGE;
  drEdge[0] := GetRValue(gColorEdge);
  drEdge[1] := GetGValue(gColorEdge);
  drEdge[2] := GetBValue(gColorEdge);
  if not config.ReadBool('Editor', 'EdgeShow', True) then
    drEdge[3] := 255
  else
    drEdge[3] := gAlphaEdge;
  gAlphaTriggerLine := config.ReadInt('Editor', 'LineAlpha', ALPHA_LINE);
  if gAlphaTriggerLine = 255 then
    gAlphaTriggerLine := ALPHA_LINE;
  gAlphaTriggerArea := config.ReadInt('Editor', 'TriggerAlpha', ALPHA_AREA);
  if gAlphaTriggerArea = 255 then
    gAlphaTriggerArea := ALPHA_AREA;
  gAlphaMonsterRect := config.ReadInt('Editor', 'MonsterRectAlpha', 0);
  gAlphaAreaRect := config.ReadInt('Editor', 'AreaRectAlpha', 0);
  Scale := Max(config.ReadInt('Editor', 'Scale', 1), 1);
  DotSize := Max(config.ReadInt('Editor', 'DotSize', 1), 1);
  OpenDialog.InitialDir := config.ReadStr('Editor', 'LastOpenDir', MapsDir);
  SaveDialog.InitialDir := config.ReadStr('Editor', 'LastSaveDir', MapsDir);

  s := config.ReadStr('Editor', 'Language', '');
  gLanguage := s;

  Compress := config.ReadBool('Editor', 'Compress', True);
  Backup := config.ReadBool('Editor', 'Backup', True);

  TestGameMode := config.ReadStr('TestRun', 'GameMode', 'DM');
  TestLimTime := config.ReadStr('TestRun', 'LimTime', '0');
  TestLimScore := config.ReadStr('TestRun', 'LimScore', '0');
  TestOptionsTwoPlayers := config.ReadBool('TestRun', 'TwoPlayers', False);
  TestOptionsTeamDamage := config.ReadBool('TestRun', 'TeamDamage', False);
  TestOptionsAllowExit := config.ReadBool('TestRun', 'AllowExit', True);
  TestOptionsWeaponStay := config.ReadBool('TestRun', 'WeaponStay', False);
  TestOptionsMonstersDM := config.ReadBool('TestRun', 'MonstersDM', False);
  TestMapOnce := config.ReadBool('TestRun', 'MapOnce', False);
  {$IF DEFINED(DARWIN)}
    TestD2dExe := config.ReadStr('TestRun', 'ExeDrawin', GameExeFile);
  {$ELSEIF DEFINED(WINDOWS)}
    TestD2dExe := config.ReadStr('TestRun', 'ExeWindows', GameExeFile);
  {$ELSE}
    TestD2dExe := config.ReadStr('TestRun', 'ExeUnix', GameExeFile);
  {$ENDIF}
  TestD2DArgs := config.ReadStr('TestRun', 'Args', '');

  RecentCount := config.ReadInt('Editor', 'RecentCount', 5);
  if RecentCount > 10 then
    RecentCount := 10;
  if RecentCount < 2 then
    RecentCount := 2;

  RecentFiles := TStringList.Create();
  for i := 0 to RecentCount-1 do
  begin
    {$IFDEF WINDOWS}
      s := config.ReadStr('RecentFilesWin', IntToStr(i), '');
    {$ELSE}
      s := config.ReadStr('RecentFilesUnix', IntToStr(i), '');
    {$ENDIF}
    if s <> '' then
      RecentFiles.Add(s);
  end;
  RefreshRecentMenu();

  config.Free();

  tbShowMap.Down := ShowMap;
  tbGridOn.Down := DotEnable;
  pcObjects.ActivePageIndex := 0;
  Application.Title := MsgEditorTitle;

  Application.OnIdle := OnIdle;
end;

procedure PrintBlack(X, Y: Integer; Text: string; FontID: DWORD);
begin
  // NOTE: all the font printing routines assume CP1251
  e_TextureFontPrintEx(X, Y, Text, FontID, 0, 0, 0, 1.0);
end;

procedure TMainForm.Draw();
var
  x, y: Integer;
  a, b: Integer;
  ID, PID: DWORD;
  Width, Height: Word;
  Rect: TRectWH;
  ObjCount: Word;
  aX, aY, aX2, aY2, XX, ScaleSz: Integer;
begin
  ID := 0;
  PID := 0;
  Width := 0;
  Height := 0;

  e_BeginRender();

  e_Clear(GL_COLOR_BUFFER_BIT,
          GetRValue(BackColor)/255,
          GetGValue(BackColor)/255,
          GetBValue(BackColor)/255);

  DrawMap();

  ObjCount := SelectedObjectCount();

// Обводим выделенные объекты красной рамкой:
  if ObjCount > 0 then
  begin
    for a := 0 to High(SelectedObjects) do
      if SelectedObjects[a].Live then
      begin
        Rect := ObjectGetRect(SelectedObjects[a].ObjectType, SelectedObjects[a].ID);

        with Rect do
        begin
          e_DrawQuad(X+MapOffset.X, Y+MapOffset.Y,
                     X+MapOffset.X+Width-1, Y+MapOffset.Y+Height-1,
                     255, 0, 0);

        // Рисуем точки изменения размеров:
          if (ObjCount = 1) and
             (SelectedObjects[GetFirstSelected].ObjectType in [OBJECT_PANEL, OBJECT_TRIGGER]) then
          begin
            e_DrawPoint(5, X+MapOffset.X, Y+MapOffset.Y+(Height div 2), 255, 255, 255);
            e_DrawPoint(5, X+MapOffset.X+Width-1, Y+MapOffset.Y+(Height div 2), 255, 255, 255);
            e_DrawPoint(5, X+MapOffset.X+(Width div 2), Y+MapOffset.Y, 255, 255, 255);
            e_DrawPoint(5, X+MapOffset.X+(Width div 2), Y+MapOffset.Y+Height-1, 255, 255, 255);

            e_DrawPoint(3, X+MapOffset.X, Y+MapOffset.Y+(Height div 2), 255, 0, 0);
            e_DrawPoint(3, X+MapOffset.X+Width-1, Y+MapOffset.Y+(Height div 2), 255, 0, 0);
            e_DrawPoint(3, X+MapOffset.X+(Width div 2), Y+MapOffset.Y, 255, 0, 0);
            e_DrawPoint(3, X+MapOffset.X+(Width div 2), Y+MapOffset.Y+Height-1, 255, 0, 0);
          end;
        end;
      end;
  end;

// Рисуем сетку:
  if DotEnable and (PreviewMode = 0) then
  begin
    if DotSize = 2 then
      a := -1
    else
      a := 0;

    x := MapOffset.X mod DotStep;
    y := MapOffset.Y mod DotStep;

    while x < RenderPanel.Width do
    begin
      while y < RenderPanel.Height do
      begin
        e_DrawPoint(DotSize, x + a, y + a,
                    GetRValue(DotColor),
                    GetGValue(DotColor),
                    GetBValue(DotColor));
        y += DotStep;
      end;
      x += DotStep;
      y := MapOffset.Y mod DotStep;
    end;
  end;

// Превью текстуры:
  if (lbTextureList.ItemIndex <> -1) and (cbPreview.Checked) and
     (not IsSpecialTextureSel()) and (PreviewMode = 0) then
  begin
    if not g_GetTexture(SelectedTexture(), ID) then
      g_GetTexture('NOTEXTURE', ID);
    g_GetTextureSizeByID(ID, Width, Height);
    if UseCheckerboard then
    begin
      if g_GetTexture('PREVIEW', PID) then
        e_DrawFill(PID, RenderPanel.Width-Width, RenderPanel.Height-Height, Width div 16 + 1, Height div 16 + 1, 0, True, False);
    end else
      e_DrawFillQuad(RenderPanel.Width-Width-2, RenderPanel.Height-Height-2,
                     RenderPanel.Width-1, RenderPanel.Height-1,
                     GetRValue(PreviewColor), GetGValue(PreviewColor), GetBValue(PreviewColor), 0);
    e_Draw(ID, RenderPanel.Width-Width, RenderPanel.Height-Height, 0, True, False);
  end;

// Подсказка при выборе точки Телепорта:
  if SelectFlag = SELECTFLAG_TELEPORT then
  begin
    with gTriggers[SelectedObjects[GetFirstSelected()].ID] do
      if Data.d2d_teleport then
        e_DrawLine(2, MousePos.X-16, MousePos.Y-1,
                   MousePos.X+16, MousePos.Y-1,
                   0, 0, 255)
      else
        e_DrawQuad(MousePos.X, MousePos.Y, MousePos.X+AreaSize[AREA_DMPOINT].Width-1,
                   MousePos.Y+AreaSize[AREA_DMPOINT].Height-1, 255, 255, 255);

    e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+180, MousePos.Y+18, 192, 192, 192, 127);
    e_DrawQuad(MousePos.X, MousePos.Y, MousePos.X+180, MousePos.Y+18, 255, 255, 255);
    PrintBlack(MousePos.X+2, MousePos.Y+2, utf8to1251(MsgHintTeleport), gEditorFont);
  end;

// Подсказка при выборе точки появления:
  if SelectFlag = SELECTFLAG_SPAWNPOINT then
  begin
    e_DrawLine(2, MousePos.X-16, MousePos.Y-1,
               MousePos.X+16, MousePos.Y-1,
               0, 0, 255);
    e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+180, MousePos.Y+18, 192, 192, 192, 127);
    e_DrawQuad(MousePos.X, MousePos.Y, MousePos.X+180, MousePos.Y+18, 255, 255, 255);
    PrintBlack(MousePos.X+2, MousePos.Y+2, utf8to1251(MsgHintSpawn), gEditorFont);
  end;

// Подсказка при выборе панели двери:
  if SelectFlag = SELECTFLAG_DOOR then
  begin
    e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+180, MousePos.Y+18, 192, 192, 192, 127);
    e_DrawQuad(MousePos.X, MousePos.Y, MousePos.X+180, MousePos.Y+18, 255, 255, 255);
    PrintBlack(MousePos.X+2, MousePos.Y+2, utf8to1251(MsgHintPanelDoor), gEditorFont);
  end;

// Подсказка при выборе панели с текстурой:
  if SelectFlag = SELECTFLAG_TEXTURE then
  begin
    e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+196, MousePos.Y+18, 192, 192, 192, 127);
    e_DrawQuad(MousePos.X, MousePos.Y, MousePos.X+196, MousePos.Y+18, 255, 255, 255);
    PrintBlack(MousePos.X+2, MousePos.Y+2, utf8to1251(MsgHintPanelTexture), gEditorFont);
  end;

// Подсказка при выборе панели индикации выстрела:
  if SelectFlag = SELECTFLAG_SHOTPANEL then
  begin
    e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+316, MousePos.Y+18, 192, 192, 192, 127);
    e_DrawQuad(MousePos.X, MousePos.Y, MousePos.X+316, MousePos.Y+18, 255, 255, 255);
    PrintBlack(MousePos.X+2, MousePos.Y+2, utf8to1251(MsgHintPanelShot), gEditorFont);
  end;

// Подсказка при выборе панели лифта:
  if SelectFlag = SELECTFLAG_LIFT then
  begin
    e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+180, MousePos.Y+18, 192, 192, 192, 127);
    e_DrawQuad(MousePos.X, MousePos.Y, MousePos.X+180, MousePos.Y+18, 255, 255, 255);
    PrintBlack(MousePos.X+2, MousePos.Y+2, utf8to1251(MsgHintPanelLift), gEditorFont);
  end;

// Подсказка при выборе монстра:
  if SelectFlag = SELECTFLAG_MONSTER then
  begin
    e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+120, MousePos.Y+18, 192, 192, 192, 127);
    e_DrawQuad(MousePos.X, MousePos.Y, MousePos.X+120, MousePos.Y+18, 255, 255, 255);
    PrintBlack(MousePos.X+2, MousePos.Y+2, utf8to1251(MsgHintMonster), gEditorFont);
  end;

// Подсказка при выборе области воздействия:
  if DrawPressRect then
  begin
    e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+204, MousePos.Y+18, 192, 192, 192, 127);
    e_DrawQuad(MousePos.X, MousePos.Y, MousePos.X+204, MousePos.Y+18, 255, 255, 255);
    PrintBlack(MousePos.X+2, MousePos.Y+2, utf8to1251(MsgHintExtArea), gEditorFont);
  end;

// Рисуем текстуры, если чертим панель:
  if (MouseAction = MOUSEACTION_DRAWPANEL) and (DrawTexturePanel) and
     (lbTextureList.ItemIndex <> -1) and (DrawRect <> nil) and
     (lbPanelType.ItemIndex in [0..8]) and not IsSpecialTextureSel() then
  begin
    if not g_GetTexture(SelectedTexture(), ID) then
      g_GetTexture('NOTEXTURE', ID);
    g_GetTextureSizeByID(ID, Width, Height);
    with DrawRect^ do
      if (Abs(Right-Left) >= Width) and (Abs(Bottom-Top) >= Height) then
        e_DrawFill(ID, Min(Left, Right), Min(Top, Bottom), Abs(Right-Left) div Width,
                   Abs(Bottom-Top) div Height, 64, True, False);
  end;

// Прямоугольник выделения:
  if DrawRect <> nil then
    with DrawRect^ do
      e_DrawQuad(Left, Top, Right-1, Bottom-1, 255, 255, 255);

// Чертим мышью панель/триггер или меняем мышью их размер:
  if (((MouseAction in [MOUSEACTION_DRAWPANEL, MOUSEACTION_DRAWTRIGGER]) and
     not(ssCtrl in GetKeyShiftState())) or (MouseAction = MOUSEACTION_RESIZE)) and
     (DrawPanelSize) then
  begin
    e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+88, MousePos.Y+33, 192, 192, 192, 127);
    e_DrawQuad(MousePos.X, MousePos.Y, MousePos.X+88, MousePos.Y+33, 255, 255, 255);

    if MouseAction in [MOUSEACTION_DRAWPANEL, MOUSEACTION_DRAWTRIGGER] then
      begin // Чертим новый
        PrintBlack(MousePos.X+2, MousePos.Y+2, Format(utf8to1251(MsgHintWidth),
                          [Abs(MousePos.X-MouseLDownPos.X)]), gEditorFont);
        PrintBlack(MousePos.X+2, MousePos.Y+16, Format(utf8to1251(MsgHintHeight),
                          [Abs(MousePos.Y-MouseLDownPos.Y)]), gEditorFont);
      end
    else // Растягиваем существующий
      if SelectedObjects[GetFirstSelected].ObjectType in [OBJECT_PANEL, OBJECT_TRIGGER] then
      begin
        if SelectedObjects[GetFirstSelected].ObjectType = OBJECT_PANEL then
          begin
            Width := gPanels[SelectedObjects[GetFirstSelected].ID].Width;
            Height := gPanels[SelectedObjects[GetFirstSelected].ID].Height;
          end
        else
          begin
            Width := gTriggers[SelectedObjects[GetFirstSelected].ID].Width;
            Height := gTriggers[SelectedObjects[GetFirstSelected].ID].Height;
          end;

        PrintBlack(MousePos.X+2, MousePos.Y+2, Format(utf8to1251(MsgHintWidth), [Width]),
                          gEditorFont);
        PrintBlack(MousePos.X+2, MousePos.Y+16, Format(utf8to1251(MsgHintHeight), [Height]),
                          gEditorFont);
      end;
  end;

// Ближайшая к курсору мыши точка на сетке:
  e_DrawPoint(3, MousePos.X, MousePos.Y, 0, 0, 255);

// Мини-карта:
  if ShowMap then
  begin
  // Сколько пикселов карты в 1 пикселе мини-карты:
    ScaleSz := 16 div Scale;
  // Размеры мини-карты:
    aX := max(gMapInfo.Width div ScaleSz, 1);
    aY := max(gMapInfo.Height div ScaleSz, 1);
  // X-координата на RenderPanel нулевой x-координаты карты:
    XX := RenderPanel.Width - aX - 1;
  // Рамка карты:
    e_DrawFillQuad(XX-1, 0, RenderPanel.Width-1, aY+1, 0, 0, 0, 0);
    e_DrawQuad(XX-1, 0, RenderPanel.Width-1, aY+1, 197, 197, 197);

    if gPanels <> nil then
    begin
    // Рисуем панели:
      for a := 0 to High(gPanels) do
        with gPanels[a] do
          if PanelType <> 0 then
          begin
          // Левый верхний угол:
            aX := XX + (X div ScaleSz);
            aY := 1 + (Y div ScaleSz);
          // Размеры:
            aX2 := max(Width div ScaleSz, 1);
            aY2 := max(Height div ScaleSz, 1);
          // Правый нижний угол:
            aX2 := aX + aX2 - 1;
            aY2 := aY + aY2 - 1;

            case PanelType of
              PANEL_WALL:      e_DrawFillQuad(aX, aY, aX2, aY2, 208, 208, 208, 0);
              PANEL_WATER:     e_DrawFillQuad(aX, aY, aX2, aY2,   0,   0, 192, 0);
              PANEL_ACID1:     e_DrawFillQuad(aX, aY, aX2, aY2,   0, 176,   0, 0);
              PANEL_ACID2:     e_DrawFillQuad(aX, aY, aX2, aY2, 176,   0,   0, 0);
              PANEL_STEP:      e_DrawFillQuad(aX, aY, aX2, aY2, 128, 128, 128, 0);
              PANEL_LIFTUP:    e_DrawFillQuad(aX, aY, aX2, aY2, 116,  72,  36, 0);
              PANEL_LIFTDOWN:  e_DrawFillQuad(aX, aY, aX2, aY2, 116, 124,  96, 0);
              PANEL_LIFTLEFT:  e_DrawFillQuad(aX, aY, aX2, aY2, 200,  80,   4, 0);
              PANEL_LIFTRIGHT: e_DrawFillQuad(aX, aY, aX2, aY2, 252, 140,  56, 0);
              PANEL_OPENDOOR:  e_DrawFillQuad(aX, aY, aX2, aY2, 100, 220,  92, 0);
              PANEL_CLOSEDOOR: e_DrawFillQuad(aX, aY, aX2, aY2, 212, 184,  64, 0);
              PANEL_BLOCKMON:  e_DrawFillQuad(aX, aY, aX2, aY2, 192,   0, 192, 0);
            end;
          end;

    // Рисуем красным выделенные панели:
      if SelectedObjects <> nil then
        for b := 0 to High(SelectedObjects) do
          with SelectedObjects[b] do
            if Live and (ObjectType = OBJECT_PANEL) then
              with gPanels[SelectedObjects[b].ID] do
                if PanelType and not(PANEL_BACK or PANEL_FORE) <> 0 then
                begin
                // Левый верхний угол:
                  aX := XX + (X div ScaleSz);
                  aY := 1 + (Y div ScaleSz);
                // Размеры:
                  aX2 := max(Width div ScaleSz, 1);
                  aY2 := max(Height div ScaleSz, 1);
                // Правый нижний угол:
                  aX2 := aX + aX2 - 1;
                  aY2 := aY + aY2 - 1;

                  e_DrawFillQuad(aX, aY, aX2, aY2, 255, 0, 0, 0)
                end;
    end;

    if (gMapInfo.Width > RenderPanel.Width) or
       (gMapInfo.Height > RenderPanel.Height) then
    begin
    // Окно, показывающее текущее положение экрана на карте:
    // Размеры окна:
      x := max(min(RenderPanel.Width, gMapInfo.Width) div ScaleSz, 1);
      y := max(min(RenderPanel.Height, gMapInfo.Height) div ScaleSz, 1);
    // Левый верхний угол:
      aX := XX + ((-MapOffset.X) div ScaleSz);
      aY := 1 + ((-MapOffset.Y) div ScaleSz);
    // Правый нижний угол:
      aX2 := aX + x - 1;
      aY2 := aY + y - 1;

      e_DrawFillQuad(aX, aY, aX2, aY2, 127, 192, 127, 127, B_BLEND);
      e_DrawQuad(aX, aY, aX2, aY2, 255, 0, 0);
    end;
  end; // Мини-карта

  e_EndRender();
  RenderPanel.SwapBuffers();
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  e_SetViewPort(0, 0, RenderPanel.Width, RenderPanel.Height);

  sbHorizontal.Min := Min(gMapInfo.Width - RenderPanel.Width, -RenderPanel.Width div 2);
  sbHorizontal.Max := Max(0, gMapInfo.Width - RenderPanel.Width div 2);
  sbVertical.Min := Min(gMapInfo.Height - RenderPanel.Height, -RenderPanel.Height div 2);
  sbVertical.Max := Max(0, gMapInfo.Height - RenderPanel.Height div 2);

  MapOffset.X := -sbHorizontal.Position;
  MapOffset.Y := -sbVertical.Position;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
  {$IFDEF DARWIN}
    var e: Boolean;
  {$ENDIF}
begin
  {$IFDEF DARWIN}
    // deactivate all menus when main window minimized
    e := self.WindowState <> wsMinimized;
    miMenuFile.Enabled := e;
    miMenuEdit.Enabled := e;
    miMenuView.Enabled := e;
    miMenuService.Enabled := e;
    miMenuWindow.Enabled := e;
    miMenuHelp.Enabled := e;
    miMenuHidden.Enabled := e;
  {$ENDIF}
end;

procedure SelectNextObject(X, Y: Integer; ObjectType: Byte; ID: DWORD);
var
  j, j_max: Integer;
  res: Boolean;
begin
  j_max := 0; // shut up compiler
  case ObjectType of
    OBJECT_PANEL:
      begin
        res := (gPanels <> nil) and
               PanelInShownLayer(gPanels[ID].PanelType) and
               g_CollidePoint(X, Y, gPanels[ID].X, gPanels[ID].Y,
                              gPanels[ID].Width,
                              gPanels[ID].Height);
        j_max := Length(gPanels) - 1;
      end;

    OBJECT_ITEM:
      begin
        res := (gItems <> nil) and
               LayerEnabled[LAYER_ITEMS] and
               g_CollidePoint(X, Y, gItems[ID].X, gItems[ID].Y,
                              ItemSize[gItems[ID].ItemType][0],
                              ItemSize[gItems[ID].ItemType][1]);
        j_max := Length(gItems) - 1;
      end;

    OBJECT_MONSTER:
      begin
        res := (gMonsters <> nil) and
               LayerEnabled[LAYER_MONSTERS] and
               g_CollidePoint(X, Y, gMonsters[ID].X, gMonsters[ID].Y,
                              MonsterSize[gMonsters[ID].MonsterType].Width,
                              MonsterSize[gMonsters[ID].MonsterType].Height);
        j_max := Length(gMonsters) - 1;
      end;

    OBJECT_AREA:
      begin
        res := (gAreas <> nil) and
               LayerEnabled[LAYER_AREAS] and
               g_CollidePoint(X, Y, gAreas[ID].X, gAreas[ID].Y,
                              AreaSize[gAreas[ID].AreaType].Width,
                              AreaSize[gAreas[ID].AreaType].Height);
        j_max := Length(gAreas) - 1;
      end;

    OBJECT_TRIGGER:
      begin
        res := (gTriggers <> nil) and
               LayerEnabled[LAYER_TRIGGERS] and 
               g_CollidePoint(X, Y, gTriggers[ID].X, gTriggers[ID].Y,
                              gTriggers[ID].Width,
                              gTriggers[ID].Height);
        j_max := Length(gTriggers) - 1;
      end;

    else
      res := False;
  end;

  if not res then
    Exit;

// Перебор ID: от ID-1 до 0; потом от High до ID+1:
  j := ID;
  
  while True do
  begin
    Dec(j);
            
    if j < 0 then
      j := j_max;
    if j = Integer(ID) then
      Break;

    case ObjectType of
      OBJECT_PANEL:
        res := PanelInShownLayer(gPanels[j].PanelType) and
               g_CollidePoint(X, Y, gPanels[j].X, gPanels[j].Y,
                              gPanels[j].Width,
                              gPanels[j].Height);
      OBJECT_ITEM:
        res := (gItems[j].ItemType <> ITEM_NONE) and
               g_CollidePoint(X, Y, gItems[j].X, gItems[j].Y,
                              ItemSize[gItems[j].ItemType][0],
                              ItemSize[gItems[j].ItemType][1]);
      OBJECT_MONSTER:
        res := (gMonsters[j].MonsterType <> MONSTER_NONE) and
               g_CollidePoint(X, Y, gMonsters[j].X, gMonsters[j].Y,
                              MonsterSize[gMonsters[j].MonsterType].Width,
                              MonsterSize[gMonsters[j].MonsterType].Height);
      OBJECT_AREA:
        res := (gAreas[j].AreaType <> AREA_NONE) and
               g_CollidePoint(X, Y, gAreas[j].X, gAreas[j].Y,
                              AreaSize[gAreas[j].AreaType].Width,
                              AreaSize[gAreas[j].AreaType].Height);
      OBJECT_TRIGGER:
        res := (gTriggers[j].TriggerType <> TRIGGER_NONE) and
               g_CollidePoint(X, Y, gTriggers[j].X, gTriggers[j].Y,
                              gTriggers[j].Width,
                              gTriggers[j].Height);
      else
        res := False;
    end;

    if res then
    begin
      SetLength(SelectedObjects, 1);

      SelectedObjects[0].ObjectType := ObjectType;
      SelectedObjects[0].ID := j;
      SelectedObjects[0].Live := True;

      FillProperty();
      Break;
    end;
  end;
end;

procedure TMainForm.RenderPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  Rect: TRectWH;
  c1, c2, c3, c4: Boolean;
  item: TItem;
  area: TArea;
  monster: TMonster;
  IDArray: DWArray;
begin
  MainForm.ActiveControl := RenderPanel;
  RenderPanel.SetFocus();

  RenderPanelMouseMove(RenderPanel, Shift, X, Y);

  if Button = mbLeft then // Left Mouse Button
  begin
  // Двигаем карту с помощью мыши и мини-карты:
    if ShowMap and
       g_CollidePoint(X, Y,
                      RenderPanel.Width-max(gMapInfo.Width div (16 div Scale), 1)-1,
                      1,
                      max(gMapInfo.Width div (16 div Scale), 1),
                      max(gMapInfo.Height div (16 div Scale), 1) ) then
      begin
        MoveMap(X, Y);
        MouseAction := MOUSEACTION_MOVEMAP;
      end
    else // Ставим предмет/монстра/область:
      if (pcObjects.ActivePageIndex in [1, 2, 3]) and
         (not (ssShift in Shift)) then
        begin
          case pcObjects.ActivePageIndex of
            1:
              if lbItemList.ItemIndex = -1 then
                ErrorMessageBox(MsgMsgChooseItem)
              else
                begin
                  item.ItemType := lbItemList.ItemIndex + ITEM_MEDKIT_SMALL;
                  if item.ItemType >= ITEM_WEAPON_KASTET then
                    item.ItemType := item.ItemType + 2;
                  item.X := MousePos.X-MapOffset.X;
                  item.Y := MousePos.Y-MapOffset.Y;

                  if not (ssCtrl in Shift) then
                  begin
                    item.X := item.X - (ItemSize[item.ItemType][0] div 2);
                    item.Y := item.Y - ItemSize[item.ItemType][1];
                  end;

                  item.OnlyDM := cbOnlyDM.Checked;
                  item.Fall := cbFall.Checked;
                  Undo_Add(OBJECT_ITEM, AddItem(item));
                end;
            2:
              if lbMonsterList.ItemIndex = -1 then
                ErrorMessageBox(MsgMsgChooseMonster)
              else
                begin
                  monster.MonsterType := lbMonsterList.ItemIndex + MONSTER_DEMON;
                  monster.X := MousePos.X-MapOffset.X;
                  monster.Y := MousePos.Y-MapOffset.Y;

                  if not (ssCtrl in Shift) then
                  begin
                    monster.X := monster.X - (MonsterSize[monster.MonsterType].Width div 2);
                    monster.Y := monster.Y - MonsterSize[monster.MonsterType].Height;
                  end;

                  if rbMonsterLeft.Checked then
                    monster.Direction := D_LEFT
                  else
                    monster.Direction := D_RIGHT;
                  Undo_Add(OBJECT_MONSTER, AddMonster(monster));
                end;
            3:
              if lbAreasList.ItemIndex = -1 then
                ErrorMessageBox(MsgMsgChooseArea)
              else
                if (lbAreasList.ItemIndex + 1) <> AREA_DOMFLAG then
                  begin
                    area.AreaType := lbAreasList.ItemIndex + AREA_PLAYERPOINT1;
                    area.X := MousePos.X-MapOffset.X;
                    area.Y := MousePos.Y-MapOffset.Y;

                    if not (ssCtrl in Shift) then
                    begin
                      area.X := area.X - (AreaSize[area.AreaType].Width div 2);
                      area.Y := area.Y - AreaSize[area.AreaType].Height;
                    end;

                    if rbAreaLeft.Checked then
                      area.Direction := D_LEFT
                    else
                      area.Direction := D_RIGHT;
                    Undo_Add(OBJECT_AREA, AddArea(area));
                  end;
          end;
        end
      else
        begin
          i := GetFirstSelected();

        // Выбираем объект под текущим:
          if (SelectedObjects <> nil) and
             (ssShift in Shift) and (i >= 0) and
             (SelectedObjects[i].Live) then
            begin
              if SelectedObjectCount() = 1 then
                SelectNextObject(X-MapOffset.X, Y-MapOffset.Y,
                                 SelectedObjects[i].ObjectType,
                                 SelectedObjects[i].ID);
            end
          else
            begin
            // Рисуем область триггера "Расширитель":
              if DrawPressRect and (i >= 0) and
                 (SelectedObjects[i].ObjectType = OBJECT_TRIGGER) and
                 (gTriggers[SelectedObjects[i].ID].TriggerType in
                   [TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF]) then
                MouseAction := MOUSEACTION_DRAWPRESS
              else // Рисуем панель:
                if pcObjects.ActivePageIndex = 0 then
                  begin
                    if (lbPanelType.ItemIndex >= 0) then
                      MouseAction := MOUSEACTION_DRAWPANEL
                  end
                else // Рисуем триггер:
                  if (lbTriggersList.ItemIndex >= 0) then
                  begin
                    MouseAction := MOUSEACTION_DRAWTRIGGER;
                  end;
            end;
        end;
  end; // if Button = mbLeft

  if Button = mbRight then // Right Mouse Button
  begin
  // Клик по мини-карте:
    if ShowMap and
       g_CollidePoint(X, Y,
                      RenderPanel.Width-max(gMapInfo.Width div (16 div Scale), 1)-1,
                      1,
                      max(gMapInfo.Width div (16 div Scale), 1),
                      max(gMapInfo.Height div (16 div Scale), 1) ) then
      begin
        MouseAction := MOUSEACTION_NOACTION;
      end
    else // Нужно что-то выбрать мышью:
      if SelectFlag <> SELECTFLAG_NONE then
        begin
          case SelectFlag of
            SELECTFLAG_TELEPORT:
            // Точку назначения телепортации:
              with gTriggers[SelectedObjects[
                     GetFirstSelected() ].ID].Data.TargetPoint do
              begin
                X := MousePos.X-MapOffset.X;
                Y := MousePos.Y-MapOffset.Y;
              end;

            SELECTFLAG_SPAWNPOINT:
            // Точку создания монстра:
              with gTriggers[SelectedObjects[GetFirstSelected()].ID] do
                if TriggerType = TRIGGER_SPAWNMONSTER then
                  begin
                    Data.MonPos.X := MousePos.X-MapOffset.X;
                    Data.MonPos.Y := MousePos.Y-MapOffset.Y;
                  end
                else if TriggerType = TRIGGER_SPAWNITEM then
                  begin // Точка создания предмета:
                    Data.ItemPos.X := MousePos.X-MapOffset.X;
                    Data.ItemPos.Y := MousePos.Y-MapOffset.Y;
                  end
                else if TriggerType = TRIGGER_SHOT then
                  begin // Точка создания выстрела:
                    Data.ShotPos.X := MousePos.X-MapOffset.X;
                    Data.ShotPos.Y := MousePos.Y-MapOffset.Y;
                  end;

            SELECTFLAG_DOOR:
            // Дверь:
              begin
                IDArray := ObjectInRect(X-MapOffset.X,
                                        Y-MapOffset.Y,
                                        2, 2, OBJECT_PANEL, True);
                if IDArray <> nil then
                  begin
                    for i := 0 to High(IDArray) do
                      if (gPanels[IDArray[i]].PanelType = PANEL_OPENDOOR) or
                         (gPanels[IDArray[i]].PanelType = PANEL_CLOSEDOOR) then
                      begin
                        gTriggers[SelectedObjects[
                          GetFirstSelected() ].ID].Data.PanelID := IDArray[i];
                        Break;
                      end;
                  end
                else
                  gTriggers[SelectedObjects[
                    GetFirstSelected() ].ID].Data.PanelID := -1;
              end;

            SELECTFLAG_TEXTURE:
            // Панель с текстурой:
              begin
                IDArray := ObjectInRect(X-MapOffset.X,
                                        Y-MapOffset.Y,
                             2, 2, OBJECT_PANEL, True);
                if IDArray <> nil then
                  begin
                    for i := 0 to High(IDArray) do
                      if ((gPanels[IDArray[i]].PanelType in
                           [PANEL_WALL, PANEL_BACK, PANEL_FORE,
                            PANEL_WATER, PANEL_ACID1, PANEL_ACID2,
                            PANEL_STEP]) or
                          (gPanels[IDArray[i]].PanelType = PANEL_OPENDOOR) or
                          (gPanels[IDArray[i]].PanelType = PANEL_CLOSEDOOR)) and
                         (gPanels[IDArray[i]].TextureName <> '') then
                      begin
                        gTriggers[SelectedObjects[
                          GetFirstSelected() ].ID].TexturePanel := IDArray[i];
                        Break;
                      end;
                  end
                else
                  gTriggers[SelectedObjects[
                    GetFirstSelected() ].ID].TexturePanel := -1;
              end;

            SELECTFLAG_LIFT:
            // Лифт:
              begin
                IDArray := ObjectInRect(X-MapOffset.X,
                                        Y-MapOffset.Y,
                                        2, 2, OBJECT_PANEL, True);
                if IDArray <> nil then
                  begin
                    for i := 0 to High(IDArray) do
                      if (gPanels[IDArray[i]].PanelType = PANEL_LIFTUP) or
                         (gPanels[IDArray[i]].PanelType = PANEL_LIFTDOWN) or
                         (gPanels[IDArray[i]].PanelType = PANEL_LIFTLEFT) or
                         (gPanels[IDArray[i]].PanelType = PANEL_LIFTRIGHT) then
                      begin
                        gTriggers[SelectedObjects[
                          GetFirstSelected() ].ID].Data.PanelID := IDArray[i];
                        Break;
                      end;
                  end
                else
                  gTriggers[SelectedObjects[
                    GetFirstSelected() ].ID].Data.PanelID := -1;
              end;

            SELECTFLAG_MONSTER:
            // Монстра:
              begin
                IDArray := ObjectInRect(X-MapOffset.X,
                                        Y-MapOffset.Y,
                                        2, 2, OBJECT_MONSTER, False);
                if IDArray <> nil then
                  gTriggers[SelectedObjects[
                    GetFirstSelected() ].ID].Data.MonsterID := IDArray[0]+1
                else
                  gTriggers[SelectedObjects[
                    GetFirstSelected() ].ID].Data.MonsterID := 0;
              end;

            SELECTFLAG_SHOTPANEL:
            // Панель индикации выстрела:
              begin
                if gTriggers[SelectedObjects[
                     GetFirstSelected() ].ID].TriggerType = TRIGGER_SHOT then
                begin
                  IDArray := ObjectInRect(X-MapOffset.X,
                                          Y-MapOffset.Y,
                                          2, 2, OBJECT_PANEL, True);
                  if IDArray <> nil then
                    begin
                      for i := 0 to High(IDArray) do
                        if ((gPanels[IDArray[i]].PanelType in
                             [PANEL_WALL, PANEL_BACK, PANEL_FORE,
                              PANEL_WATER, PANEL_ACID1, PANEL_ACID2,
                              PANEL_STEP]) or
                            (gPanels[IDArray[i]].PanelType = PANEL_OPENDOOR) or
                            (gPanels[IDArray[i]].PanelType = PANEL_CLOSEDOOR)) and
                           (gPanels[IDArray[i]].TextureName <> '') then
                        begin
                          gTriggers[SelectedObjects[
                            GetFirstSelected() ].ID].Data.ShotPanelID := IDArray[i];
                          Break;
                        end;
                    end
                  else
                    gTriggers[SelectedObjects[
                      GetFirstSelected() ].ID].Data.ShotPanelID := -1;
                end;
              end;
          end;

          SelectFlag := SELECTFLAG_SELECTED;
        end
      else // if SelectFlag <> SELECTFLAG_NONE...
        begin
        // Что уже выбрано и не нажат Ctrl:
          if (SelectedObjects <> nil) and
             (not (ssCtrl in Shift)) then
            for i := 0 to High(SelectedObjects) do
              with SelectedObjects[i] do
                if Live then
                begin
                  if (ObjectType in [OBJECT_PANEL, OBJECT_TRIGGER]) and
                     (SelectedObjectCount() = 1) then
                  begin
                    Rect := ObjectGetRect(ObjectType, ID);

                    c1 := g_Collide(X-MapOffset.X-1, Y-MapOffset.Y-1, 2, 2,
                            Rect.X-2, Rect.Y+(Rect.Height div 2)-2, 4, 4);
                    c2 := g_Collide(X-MapOffset.X-1, Y-MapOffset.Y-1, 2, 2,
                            Rect.X+Rect.Width-3, Rect.Y+(Rect.Height div 2)-2, 4, 4);
                    c3 := g_Collide(X-MapOffset.X-1, Y-MapOffset.Y-1, 2, 2,
                            Rect.X+(Rect.Width div 2)-2, Rect.Y-2, 4, 4);
                    c4 := g_Collide(X-MapOffset.X-1, Y-MapOffset.Y-1, 2, 2,
                            Rect.X+(Rect.Width div 2)-2, Rect.Y+Rect.Height-3, 4, 4);

                  // Меняем размер панели или триггера:
                    if c1 or c2 or c3 or c4 then
                    begin
                      MouseAction := MOUSEACTION_RESIZE;
                      LastMovePoint := MousePos;

                      if c1 or c2 then
                        begin // Шире/уже
                          ResizeType := RESIZETYPE_HORIZONTAL;
                          if c1 then
                            ResizeDirection := RESIZEDIR_LEFT
                          else
                            ResizeDirection := RESIZEDIR_RIGHT;
                          RenderPanel.Cursor := crSizeWE;
                        end
                      else
                        begin // Выше/ниже
                          ResizeType := RESIZETYPE_VERTICAL;
                          if c3 then
                            ResizeDirection := RESIZEDIR_UP
                          else
                            ResizeDirection := RESIZEDIR_DOWN;
                          RenderPanel.Cursor := crSizeNS;
                        end;

                      Break;
                    end;
                  end;

                // Перемещаем панель или триггер:
                  if ObjectCollide(ObjectType, ID,
                       X-MapOffset.X-1,
                       Y-MapOffset.Y-1, 2, 2) then
                  begin
                    MouseAction := MOUSEACTION_MOVEOBJ;
                    LastMovePoint := MousePos;

                    Break;
                  end;
                end;
        end;
  end; // if Button = mbRight

  if Button = mbMiddle then // Middle Mouse Button
  begin
    SetCapture(RenderPanel.Handle);
    RenderPanel.Cursor := crSize;
  end;

  MouseMDown := Button = mbMiddle;
  if MouseMDown then
    MouseMDownPos := Mouse.CursorPos;

  MouseRDown := Button = mbRight;
  if MouseRDown then
    MouseRDownPos := MousePos;

  MouseLDown := Button = mbLeft;
  if MouseLDown then
    MouseLDownPos := MousePos;
end;

procedure TMainForm.RenderPanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  panel: TPanel;
  trigger: TTrigger;
  rRect: TRectWH;
  rSelectRect: Boolean;
  wWidth, wHeight: Word;
  TextureID: DWORD;

  procedure SelectObjects(ObjectType: Byte);
  var
    i: Integer;
    IDArray: DWArray;
  begin
    IDArray := ObjectInRect(rRect.X, rRect.Y,
                 rRect.Width, rRect.Height,
                 ObjectType, rSelectRect);

    if IDArray <> nil then
      for i := 0 to High(IDArray) do
        SelectObject(ObjectType, IDArray[i], (ssCtrl in Shift) or rSelectRect);
  end;
begin
  if Button = mbLeft then
    MouseLDown := False;
  if Button = mbRight then
    MouseRDown := False;
  if Button = mbMiddle then
    MouseMDown := False;

  DrawRect := nil;
  ResizeType := RESIZETYPE_NONE;
  TextureID := 0;

  if Button = mbLeft then // Left Mouse Button
    begin
      if MouseAction <> MOUSEACTION_NONE then
      begin // Было действие мышью
      // Мышь сдвинулась во время удержания клавиши,
      // либо активирован режим быстрого рисования:
        if ((MousePos.X <> MouseLDownPos.X) and
           (MousePos.Y <> MouseLDownPos.Y)) or
           ((MouseAction in [MOUSEACTION_DRAWPANEL, MOUSEACTION_DRAWTRIGGER]) and
           (ssCtrl in Shift)) then
          case MouseAction of
          // Рисовали панель:
            MOUSEACTION_DRAWPANEL:
              begin
              // Фон или передний план без текстуры - ошибка:
                if (lbPanelType.ItemIndex in [1, 2]) and
                   (lbTextureList.ItemIndex = -1) then
                  ErrorMessageBox(MsgMsgChooseTexture)
                else // Назначаем параметры панели:
                  begin
                    case lbPanelType.ItemIndex of
                      0: Panel.PanelType := PANEL_WALL;
                      1: Panel.PanelType := PANEL_BACK;
                      2: Panel.PanelType := PANEL_FORE;
                      3: Panel.PanelType := PANEL_OPENDOOR;
                      4: Panel.PanelType := PANEL_CLOSEDOOR;
                      5: Panel.PanelType := PANEL_STEP;
                      6: Panel.PanelType := PANEL_WATER;
                      7: Panel.PanelType := PANEL_ACID1;
                      8: Panel.PanelType := PANEL_ACID2;
                      9: Panel.PanelType := PANEL_LIFTUP;
                      10: Panel.PanelType := PANEL_LIFTDOWN;
                      11: Panel.PanelType := PANEL_LIFTLEFT;
                      12: Panel.PanelType := PANEL_LIFTRIGHT;
                      13: Panel.PanelType := PANEL_BLOCKMON;
                    end;

                    Panel.X := Min(MousePos.X-MapOffset.X, MouseLDownPos.X-MapOffset.X);
                    Panel.Y := Min(MousePos.Y-MapOffset.Y, MouseLDownPos.Y-MapOffset.Y);
                    if ssCtrl in Shift then
                      begin
                        wWidth := DotStep;
                        wHeight := DotStep;
                        if (lbTextureList.ItemIndex <> -1) and
                           (not IsSpecialTextureSel()) then
                        begin
                          if not g_GetTexture(SelectedTexture(), TextureID) then
                            g_GetTexture('NOTEXTURE', TextureID);
                          g_GetTextureSizeByID(TextureID, wWidth, wHeight);
                        end;
                        Panel.Width := wWidth;
                        Panel.Height := wHeight;
                      end
                    else
                      begin
                        Panel.Width := Abs(MousePos.X-MouseLDownPos.X);
                        Panel.Height := Abs(MousePos.Y-MouseLDownPos.Y);
                      end;

                  // Лифты, блокМон или отсутствие текстуры - пустая текстура:
                    if (lbPanelType.ItemIndex in [9, 10, 11, 12, 13]) or
                       (lbTextureList.ItemIndex = -1) then
                      begin
                        Panel.TextureHeight := 1;
                        Panel.TextureWidth := 1;
                        Panel.TextureName := '';
                        Panel.TextureID := TEXTURE_SPECIAL_NONE;
                      end
                    else // Есть текстура:
                      begin
                        Panel.TextureName := SelectedTexture();

                      // Обычная текстура:
                        if not IsSpecialTextureSel() then
                          begin
                            g_GetTextureSizeByName(Panel.TextureName,
                              Panel.TextureWidth, Panel.TextureHeight);
                            g_GetTexture(Panel.TextureName, Panel.TextureID);
                          end
                        else // Спец.текстура:
                          begin
                            Panel.TextureHeight := 1;
                            Panel.TextureWidth := 1;
                            Panel.TextureID := SpecialTextureID(SelectedTexture());
                          end;
                      end;

                    Panel.Alpha := 0;
                    Panel.Blending := False;

                    Undo_Add(OBJECT_PANEL, AddPanel(Panel));
                  end;
              end;

          // Рисовали триггер:
            MOUSEACTION_DRAWTRIGGER:
              begin
                trigger.X := Min(MousePos.X-MapOffset.X, MouseLDownPos.X-MapOffset.X);
                trigger.Y := Min(MousePos.Y-MapOffset.Y, MouseLDownPos.Y-MapOffset.Y);
                if ssCtrl in Shift then
                  begin
                    wWidth := DotStep;
                    wHeight := DotStep;
                    trigger.Width := wWidth;
                    trigger.Height := wHeight;
                  end
                else
                  begin
                    trigger.Width := Abs(MousePos.X-MouseLDownPos.X);
                    trigger.Height := Abs(MousePos.Y-MouseLDownPos.Y);
                  end;

                trigger.Enabled := True;
                trigger.TriggerType := lbTriggersList.ItemIndex+1;
                trigger.TexturePanel := -1;

              // Типы активации:
                trigger.ActivateType := 0;

                if clbActivationType.Checked[0] then
                  trigger.ActivateType := Trigger.ActivateType or ACTIVATE_PLAYERCOLLIDE;
                if clbActivationType.Checked[1] then
                  trigger.ActivateType := Trigger.ActivateType or ACTIVATE_MONSTERCOLLIDE;
                if clbActivationType.Checked[2] then
                  trigger.ActivateType := Trigger.ActivateType or ACTIVATE_PLAYERPRESS;
                if clbActivationType.Checked[3] then
                  trigger.ActivateType := Trigger.ActivateType or ACTIVATE_MONSTERPRESS;
                if clbActivationType.Checked[4] then
                  trigger.ActivateType := Trigger.ActivateType or ACTIVATE_SHOT;
                if clbActivationType.Checked[5] then
                  trigger.ActivateType := Trigger.ActivateType or ACTIVATE_NOMONSTER;

              // Необходимые для активации ключи:
                trigger.Key := 0;

                if clbKeys.Checked[0] then
                  trigger.Key := Trigger.Key or KEY_RED;
                if clbKeys.Checked[1] then
                  trigger.Key := Trigger.Key or KEY_GREEN;
                if clbKeys.Checked[2] then
                  trigger.Key := Trigger.Key or KEY_BLUE;
                if clbKeys.Checked[3] then
                  trigger.Key := Trigger.Key or KEY_REDTEAM;
                if clbKeys.Checked[4] then
                  trigger.Key := Trigger.Key or KEY_BLUETEAM;

              // Параметры триггера:
                FillByte(trigger.Data.Default[0], 128, 0);

                case trigger.TriggerType of
                // Переключаемая панель:
                  TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR,
                  TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP,
                  TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT:
                    begin
                      Trigger.Data.PanelID := -1;
                    end;

                // Телепортация:
                  TRIGGER_TELEPORT:
                    begin
                      trigger.Data.TargetPoint.X := trigger.X-64;
                      trigger.Data.TargetPoint.Y := trigger.Y-64;
                      trigger.Data.d2d_teleport := True;
                      trigger.Data.TlpDir := 0;
                    end;

                // Изменение других триггеров:
                  TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF,
                  TRIGGER_ONOFF:
                    begin
                      trigger.Data.Count := 1;
                    end;

                // Звук:
                  TRIGGER_SOUND:
                    begin
                      trigger.Data.Volume := 255;
                      trigger.Data.Pan := 127;
                      trigger.Data.PlayCount := 1;
                      trigger.Data.Local := True;
                      trigger.Data.SoundSwitch := False;
                    end;

                // Музыка:
                  TRIGGER_MUSIC:
                    begin
                      trigger.Data.MusicAction := 1;
                    end;

                // Создание монстра:
                  TRIGGER_SPAWNMONSTER:
                    begin
                      trigger.Data.MonType := MONSTER_ZOMBY;
                      trigger.Data.MonPos.X := trigger.X-64;
                      trigger.Data.MonPos.Y := trigger.Y-64;
                      trigger.Data.MonHealth := 0;
                      trigger.Data.MonActive := False;
                      trigger.Data.MonCount := 1;
                    end;

                // Создание предмета:
                  TRIGGER_SPAWNITEM:
                    begin
                      trigger.Data.ItemType := ITEM_AMMO_BULLETS;
                      trigger.Data.ItemPos.X := trigger.X-64;
                      trigger.Data.ItemPos.Y := trigger.Y-64;
                      trigger.Data.ItemOnlyDM := False;
                      trigger.Data.ItemFalls := False;
                      trigger.Data.ItemCount := 1;
                      trigger.Data.ItemMax := 0;
                      trigger.Data.ItemDelay := 0;
                    end;

                // Ускорение:
                  TRIGGER_PUSH:
                    begin
                      trigger.Data.PushAngle := 90;
                      trigger.Data.PushForce := 10;
                      trigger.Data.ResetVel := True;
                    end;

                  TRIGGER_SCORE:
                    begin
                      trigger.Data.ScoreCount := 1;
                      trigger.Data.ScoreCon := True;
                      trigger.Data.ScoreMsg := True;
                    end;

                  TRIGGER_MESSAGE:
                    begin
                      trigger.Data.MessageKind := 0;
                      trigger.Data.MessageSendTo := 0;
                      trigger.Data.MessageText := '';
                      trigger.Data.MessageTime := 144;
                    end;

                  TRIGGER_DAMAGE:
                    begin
                      trigger.Data.DamageValue := 5;
                      trigger.Data.DamageInterval := 12;
                    end;

                  TRIGGER_HEALTH:
                    begin
                      trigger.Data.HealValue := 5;
                      trigger.Data.HealInterval := 36;
                    end;

                  TRIGGER_SHOT:
                    begin
                      trigger.Data.ShotType := TRIGGER_SHOT_BULLET;
                      trigger.Data.ShotSound := True;
                      trigger.Data.ShotPanelID := -1;
                      trigger.Data.ShotTarget := 0;
                      trigger.Data.ShotIntSight := 0;
                      trigger.Data.ShotAim := TRIGGER_SHOT_AIM_DEFAULT;
                      trigger.Data.ShotPos.X := trigger.X-64;
                      trigger.Data.ShotPos.Y := trigger.Y-64;
                      trigger.Data.ShotAngle := 0;
                      trigger.Data.ShotWait := 18;
                      trigger.Data.ShotAccuracy := 0;
                      trigger.Data.ShotAmmo := 0;
                      trigger.Data.ShotIntReload := 0;
                    end;

                  TRIGGER_EFFECT:
                    begin
                      trigger.Data.FXCount := 1;
                      trigger.Data.FXType := TRIGGER_EFFECT_PARTICLE;
                      trigger.Data.FXSubType := TRIGGER_EFFECT_SLIQUID;
                      trigger.Data.FXColorR := 0;
                      trigger.Data.FXColorG := 0;
                      trigger.Data.FXColorB := 255;
                      trigger.Data.FXPos := TRIGGER_EFFECT_POS_CENTER;
                      trigger.Data.FXWait := 1;
                      trigger.Data.FXVelX := 0;
                      trigger.Data.FXVelY := -20;
                      trigger.Data.FXSpreadL := 5;
                      trigger.Data.FXSpreadR := 5;
                      trigger.Data.FXSpreadU := 4;
                      trigger.Data.FXSpreadD := 0;
                    end;
                end;

                Undo_Add(OBJECT_TRIGGER, AddTrigger(trigger));
              end;

          // Рисовали область триггера "Расширитель":
            MOUSEACTION_DRAWPRESS:
              with gTriggers[SelectedObjects[GetFirstSelected].ID] do
              begin
                Data.tX := Min(MousePos.X-MapOffset.X, MouseLDownPos.X-MapOffset.X);
                Data.tY := Min(MousePos.Y-MapOffset.Y, MouseLDownPos.Y-MapOffset.Y);
                Data.tWidth := Abs(MousePos.X-MouseLDownPos.X);
                Data.tHeight := Abs(MousePos.Y-MouseLDownPos.Y);

                DrawPressRect := False;
              end;
          end;

        MouseAction := MOUSEACTION_NONE;
      end;
    end // if Button = mbLeft...
  else if Button = mbRight then // Right Mouse Button:
    begin
      if MouseAction = MOUSEACTION_NOACTION then
      begin
        MouseAction := MOUSEACTION_NONE;
        Exit;
      end;

    // Объект передвинут или изменен в размере:
      if MouseAction in [MOUSEACTION_MOVEOBJ, MOUSEACTION_RESIZE] then
      begin
        RenderPanel.Cursor := crDefault;
        MouseAction := MOUSEACTION_NONE;
        FillProperty();
        Exit;
      end;

    // Еще не все выбрали:
      if SelectFlag <> SELECTFLAG_NONE then
      begin
        if SelectFlag = SELECTFLAG_SELECTED then
          SelectFlag := SELECTFLAG_NONE;
        FillProperty();
        Exit;
      end;

    // Мышь сдвинулась во время удержания клавиши:
      if (MousePos.X <> MouseRDownPos.X) and
         (MousePos.Y <> MouseRDownPos.Y) then
        begin
          rSelectRect := True;

          rRect.X := Min(MousePos.X, MouseRDownPos.X)-MapOffset.X;
          rRect.Y := Min(MousePos.Y, MouseRDownPos.Y)-MapOffset.Y;
          rRect.Width := Abs(MousePos.X-MouseRDownPos.X);
          rRect.Height := Abs(MousePos.Y-MouseRDownPos.Y);
        end
      else // Мышь не сдвинулась - нет прямоугольника:
        begin
          rSelectRect := False;

          rRect.X := X-MapOffset.X-1;
          rRect.Y := Y-MapOffset.Y-1;
          rRect.Width := 2;
          rRect.Height := 2;
        end;

    // Если зажат Ctrl - выделять еще, иначе только один выделенный объект: 
      if not (ssCtrl in Shift) then
        RemoveSelectFromObjects();

    // Выделяем всё в выбранном прямоугольнике:
      if (ssCtrl in Shift) and (ssAlt in Shift) then
        begin
          SelectObjects(OBJECT_PANEL);
          SelectObjects(OBJECT_ITEM);
          SelectObjects(OBJECT_MONSTER);
          SelectObjects(OBJECT_AREA);
          SelectObjects(OBJECT_TRIGGER);
        end
      else
        SelectObjects(pcObjects.ActivePageIndex+1);

      FillProperty();
    end

  else // Middle Mouse Button
    begin
      RenderPanel.Cursor := crDefault;
      ReleaseCapture();
    end;
end;

procedure TMainForm.RenderPanelPaint(Sender: TObject);
begin
  Draw();
end;

function TMainForm.RenderMousePos(): Types.TPoint;
begin
  Result := RenderPanel.ScreenToClient(Mouse.CursorPos);
end;

procedure TMainForm.RecountSelectedObjects();
begin
  if SelectedObjectCount() = 0 then
    StatusBar.Panels[0].Text := ''
  else
    StatusBar.Panels[0].Text := Format(MsgCapStatSelected, [SelectedObjectCount()]);
end;

procedure TMainForm.RenderPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  sX, sY: Integer;
  dWidth, dHeight: Integer;
  _id: Integer;
  TextureID: DWORD;
  wWidth, wHeight: Word;
begin
  _id := GetFirstSelected();
  TextureID := 0;

// Рисуем панель с текстурой, сетка - размеры текстуры:
  if (MouseAction = MOUSEACTION_DRAWPANEL) and
     (lbPanelType.ItemIndex in [0..8]) and
     (lbTextureList.ItemIndex <> -1) and
     (not IsSpecialTextureSel()) then
    begin
      sX := StrToIntDef(lTextureWidth.Caption, DotStep);
      sY := StrToIntDef(lTextureHeight.Caption, DotStep);
    end
  else
  // Меняем размер панели с текстурой, сетка - размеры текстуры:
    if (MouseAction = MOUSEACTION_RESIZE) and
       ( (SelectedObjects[_id].ObjectType = OBJECT_PANEL) and
         IsTexturedPanel(gPanels[SelectedObjects[_id].ID].PanelType) and
         (gPanels[SelectedObjects[_id].ID].TextureName <> '') and
         (not IsSpecialTexture(gPanels[SelectedObjects[_id].ID].TextureName)) ) then
      begin
        sX := gPanels[SelectedObjects[_id].ID].TextureWidth;
        sY := gPanels[SelectedObjects[_id].ID].TextureHeight;
      end
    else
    // Выравнивание по сетке:
      if SnapToGrid then
        begin
          sX := DotStep;
          sY := DotStep;
        end
      else // Нет выравнивания по сетке:
        begin
          sX := 1;
          sY := 1;
        end;

// Новая позиция мыши:
  if MouseLDown then
    begin // Зажата левая кнопка мыши
      MousePos.X := (Round((X-MouseLDownPos.X)/sX)*sX)+MouseLDownPos.X;
      MousePos.Y := (Round((Y-MouseLDownPos.Y)/sY)*sY)+MouseLDownPos.Y;
    end
  else
    if MouseRDown then
      begin // Зажата правая кнопка мыши
        MousePos.X := (Round((X-MouseRDownPos.X)/sX)*sX)+MouseRDownPos.X;
        MousePos.Y := (Round((Y-MouseRDownPos.Y)/sY)*sY)+MouseRDownPos.Y;
      end
    else
      begin // Кнопки мыши не зажаты
        MousePos.X := Round((-MapOffset.X + X) / sX) * sX + MapOffset.X;
        MousePos.Y := Round((-MapOffset.Y + Y) / sY) * sY + MapOffset.Y;
      end;

// Зажата только правая кнопка мыши:
  if (not MouseLDown) and (MouseRDown) and (not MouseMDown) then
  begin
  // Рисуем прямоугольник выделения:
    if MouseAction = MOUSEACTION_NONE then
      begin
        if DrawRect = nil then
          New(DrawRect);
        DrawRect.Top := MouseRDownPos.y;
        DrawRect.Left := MouseRDownPos.x;
        DrawRect.Bottom := MousePos.y;
        DrawRect.Right := MousePos.x;
      end
    else
    // Двигаем выделенные объекты:
      if MouseAction = MOUSEACTION_MOVEOBJ then
        begin
          MoveSelectedObjects(ssShift in Shift, ssCtrl in Shift,
                              MousePos.X-LastMovePoint.X,
                              MousePos.Y-LastMovePoint.Y);
        end
      else
      // Меняем размер выделенного объекта:
        if MouseAction = MOUSEACTION_RESIZE then
        begin
          if (SelectedObjectCount = 1) and
             (SelectedObjects[GetFirstSelected].Live) then
          begin
            dWidth := MousePos.X-LastMovePoint.X;
            dHeight := MousePos.Y-LastMovePoint.Y;

            case ResizeType of
              RESIZETYPE_VERTICAL: dWidth := 0;
              RESIZETYPE_HORIZONTAL: dHeight := 0;
            end;

            case ResizeDirection of
              RESIZEDIR_UP: dHeight := -dHeight;
              RESIZEDIR_LEFT: dWidth := -dWidth;
            end;

            if ResizeObject(SelectedObjects[GetFirstSelected].ObjectType,
                           SelectedObjects[GetFirstSelected].ID,
                           dWidth, dHeight, ResizeDirection) then
              LastMovePoint := MousePos;
          end;
        end;
  end;

// Зажата только левая кнопка мыши:
  if (not MouseRDown) and (MouseLDown) and (not MouseMDown) then
  begin
  // Рисуем прямоугольник планирования панели:
    if MouseAction in [MOUSEACTION_DRAWPANEL,
                       MOUSEACTION_DRAWTRIGGER,
                       MOUSEACTION_DRAWPRESS] then
      begin
        if DrawRect = nil then
          New(DrawRect);
        if ssCtrl in Shift then
          begin
            wWidth := DotStep;
            wHeight := DotStep;
            if (lbTextureList.ItemIndex <> -1) and (not IsSpecialTextureSel()) and
               (MouseAction = MOUSEACTION_DRAWPANEL) then
            begin
              if not g_GetTexture(SelectedTexture(), TextureID) then
                g_GetTexture('NOTEXTURE', TextureID);
              g_GetTextureSizeByID(TextureID, wWidth, wHeight);
            end;
            DrawRect.Top := MouseLDownPos.y;
            DrawRect.Left := MouseLDownPos.x;
            DrawRect.Bottom := DrawRect.Top + wHeight;
            DrawRect.Right := DrawRect.Left + wWidth;
          end
        else
          begin
            DrawRect.Top := MouseLDownPos.y;
            DrawRect.Left := MouseLDownPos.x;
            DrawRect.Bottom := MousePos.y;
            DrawRect.Right := MousePos.x;
          end;
      end
    else // Двигаем карту:
      if MouseAction = MOUSEACTION_MOVEMAP then
      begin
        MoveMap(X, Y);
      end;
  end;

// Only Middle Mouse Button is pressed
  if (not MouseLDown) and (not MouseRDown) and (MouseMDown) then
  begin
    MapOffset.X := -EnsureRange(-MapOffset.X + MouseMDownPos.X - Mouse.CursorPos.X,
                                sbHorizontal.Min, sbHorizontal.Max);
    sbHorizontal.Position := -MapOffset.X;
    MapOffset.Y := -EnsureRange(-MapOffset.Y + MouseMDownPos.Y - Mouse.CursorPos.Y,
                                sbVertical.Min, sbVertical.Max);
    sbVertical.Position := -MapOffset.Y;
    MouseMDownPos := Mouse.CursorPos;
  end;

// Клавиши мыши не зажаты:
  if (not MouseRDown) and (not MouseLDown) then
    DrawRect := nil;

// Строка состояния - координаты мыши:
  StatusBar.Panels[1].Text := Format('(%d:%d)',
    [MousePos.X-MapOffset.X, MousePos.Y-MapOffset.Y]);

  RenderPanel.Invalidate;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := Application.MessageBox(PChar(MsgMsgExitPromt),
                         PChar(MsgMsgExit),
                         MB_ICONQUESTION or MB_YESNO or
                         MB_DEFBUTTON1) = idYes;
end;

procedure TMainForm.aExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  config: TConfig;
  s: AnsiString;
  i: Integer;
begin
  config := TConfig.CreateFile(CfgFileName);

  if WindowState <> wsMaximized then
  begin
    config.WriteInt('Editor', 'XPos', Left);
    config.WriteInt('Editor', 'YPos', Top);
    config.WriteInt('Editor', 'Width', Width);
    config.WriteInt('Editor', 'Height', Height);
  end
  else
  begin
    config.WriteInt('Editor', 'XPos', RestoredLeft);
    config.WriteInt('Editor', 'YPos', RestoredTop);
    config.WriteInt('Editor', 'Width', RestoredWidth);
    config.WriteInt('Editor', 'Height', RestoredHeight);
  end;
  config.WriteBool('Editor', 'Maximize', WindowState = wsMaximized);
  config.WriteBool('Editor', 'Minimap', ShowMap);
  config.WriteInt('Editor', 'PanelProps', PanelProps.ClientWidth);
  config.WriteInt('Editor', 'PanelObjs', PanelObjs.ClientHeight);
  config.WriteBool('Editor', 'DotEnable', DotEnable);
  config.WriteInt('Editor', 'DotStep', DotStep);
  config.WriteStr('Editor', 'LastOpenDir', OpenDialog.InitialDir);
  config.WriteStr('Editor', 'LastSaveDir', SaveDialog.InitialDir);
  config.WriteStr('Editor', 'Language', gLanguage);
  config.WriteBool('Editor', 'EdgeShow', drEdge[3] < 255);
  config.WriteInt('Editor', 'EdgeColor', gColorEdge);
  config.WriteInt('Editor', 'EdgeAlpha', gAlphaEdge);
  config.WriteInt('Editor', 'LineAlpha', gAlphaTriggerLine);
  config.WriteInt('Editor', 'TriggerAlpha', gAlphaTriggerArea);
  config.WriteInt('Editor', 'MonsterRectAlpha', gAlphaMonsterRect);
  config.WriteInt('Editor', 'AreaRectAlpha', gAlphaAreaRect);

  for i := 0 to RecentCount - 1 do
  begin
    if i < RecentFiles.Count then s := RecentFiles[i] else s := '';
    {$IFDEF WINDOWS}
      config.WriteStr('RecentFilesWin', IntToStr(i), s);
    {$ELSE}
      config.WriteStr('RecentFilesUnix', IntToStr(i), s);
    {$ENDIF}
  end;
  RecentFiles.Free();

  config.SaveFile(CfgFileName);
  config.Free();

  slInvalidTextures.Free;
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  if Length(FileNames) <> 1 then
    Exit;

  OpenMapFile(FileNames[0]);
end;

procedure TMainForm.RenderPanelResize(Sender: TObject);
begin
  if MainForm.Visible then
    MainForm.Resize();
end;

procedure TMainForm.Splitter1Moved(Sender: TObject);
begin
  FormResize(Sender);
end;

procedure TMainForm.MapTestCheck(Sender: TObject);
begin
  if MapTestProcess <> nil then
  begin
    if MapTestProcess.Running = false then
    begin
      if MapTestProcess.ExitCode <> 0 then
        Application.MessageBox(PChar(MsgMsgExecError), 'FIXME', MB_OK or MB_ICONERROR);
      SysUtils.DeleteFile(MapTestFile);
      MapTestFile := '';
      FreeAndNil(MapTestProcess);
      tbTestMap.Enabled := True;
    end;
  end;
end;

procedure TMainForm.aMapOptionsExecute(Sender: TObject);
var
  ResName: String;
begin
  MapOptionsForm.ShowModal();

  ResName := OpenedMap;
  while (Pos(':\', ResName) > 0) do
    Delete(ResName, 1, Pos(':\', ResName) + 1);

  UpdateCaption(gMapInfo.Name, ExtractFileName(OpenedWAD), ResName);
end;

procedure TMainForm.aAboutExecute(Sender: TObject);
begin
  AboutForm.ShowModal();
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  dx, dy, i: Integer;
  FileName: String;
  ok: Boolean;
begin
  if (not EditingProperties) then
  begin
    if ssCtrl in Shift then
    begin
      case Chr(Key) of
        '1': ContourEnabled[LAYER_BACK] := not ContourEnabled[LAYER_BACK];
        '2': ContourEnabled[LAYER_WALLS] := not ContourEnabled[LAYER_WALLS];
        '3': ContourEnabled[LAYER_FOREGROUND] := not ContourEnabled[LAYER_FOREGROUND];
        '4': ContourEnabled[LAYER_STEPS] := not ContourEnabled[LAYER_STEPS];
        '5': ContourEnabled[LAYER_WATER] := not ContourEnabled[LAYER_WATER];
        '6': ContourEnabled[LAYER_ITEMS] := not ContourEnabled[LAYER_ITEMS];
        '7': ContourEnabled[LAYER_MONSTERS] := not ContourEnabled[LAYER_MONSTERS];
        '8': ContourEnabled[LAYER_AREAS] := not ContourEnabled[LAYER_AREAS];
        '9': ContourEnabled[LAYER_TRIGGERS] := not ContourEnabled[LAYER_TRIGGERS];
        '0':
           begin
             ok := False;
             for i := Low(ContourEnabled) to High(ContourEnabled) do
               if ContourEnabled[i] then
                 ok := True;
             for i := Low(ContourEnabled) to High(ContourEnabled) do
               ContourEnabled[i] := not ok
           end
      end
    end
    else
    begin
      case Chr(key) of
        '1': SwitchLayer(LAYER_BACK);
        '2': SwitchLayer(LAYER_WALLS);
        '3': SwitchLayer(LAYER_FOREGROUND);
        '4': SwitchLayer(LAYER_STEPS);
        '5': SwitchLayer(LAYER_WATER);
        '6': SwitchLayer(LAYER_ITEMS);
        '7': SwitchLayer(LAYER_MONSTERS);
        '8': SwitchLayer(LAYER_AREAS);
        '9': SwitchLayer(LAYER_TRIGGERS);
        '0': tbShowClick(tbShow);
      end
    end;

    if Key = Ord('V') then
    begin // Поворот монстров и областей:
      if (SelectedObjects <> nil) then
      begin
        for i := 0 to High(SelectedObjects) do
          if (SelectedObjects[i].Live) then
          begin
            if (SelectedObjects[i].ObjectType = OBJECT_MONSTER) then
              begin
                g_ChangeDir(gMonsters[SelectedObjects[i].ID].Direction);
              end
            else
              if (SelectedObjects[i].ObjectType = OBJECT_AREA) then
              begin
                g_ChangeDir(gAreas[SelectedObjects[i].ID].Direction);
              end;
          end;
      end
      else
      begin
        if pcObjects.ActivePage = tsMonsters then
        begin
          if rbMonsterLeft.Checked then
            rbMonsterRight.Checked := True
          else
            rbMonsterLeft.Checked := True;
        end;
        if pcObjects.ActivePage = tsAreas then
        begin
          if rbAreaLeft.Checked then
            rbAreaRight.Checked := True
          else
            rbAreaLeft.Checked := True;
        end;
      end;
    end;

    if not (ssCtrl in Shift) then
    begin
    // Быстрое превью карты:
      if Key = Ord('E') then
      begin
        if PreviewMode = 0 then
          PreviewMode := 2;
      end;

    // Вертикальный скролл карты:
      with sbVertical do
      begin
        if Key = Ord('W') then
        begin
          dy := Position;
          if ssShift in Shift then Position := EnsureRange(Position - DotStep * 4, Min, Max)
          else Position := EnsureRange(Position - DotStep, Min, Max);
          MapOffset.Y := -Position;
          dy -= Position;

          if (MouseLDown or MouseRDown) then
          begin
            if DrawRect <> nil then
            begin
              Inc(MouseLDownPos.y, dy);
              Inc(MouseRDownPos.y, dy);
            end;
            Inc(LastMovePoint.Y, dy);
            RenderPanelMouseMove(Sender, Shift, RenderMousePos().X, RenderMousePos().Y);
          end;
        end;

        if Key = Ord('S') then
        begin
          dy := Position;
          if ssShift in Shift then Position := EnsureRange(Position + DotStep * 4, Min, Max)
          else Position := EnsureRange(Position + DotStep, Min, Max);
          MapOffset.Y := -Position;
          dy -= Position;

          if (MouseLDown or MouseRDown) then
          begin
            if DrawRect <> nil then
            begin
              Inc(MouseLDownPos.y, dy);
              Inc(MouseRDownPos.y, dy);
            end;
            Inc(LastMovePoint.Y, dy);
            RenderPanelMouseMove(Sender, Shift, RenderMousePos().X, RenderMousePos().Y);
          end;
        end;
      end;

    // Горизонтальный скролл карты:
      with sbHorizontal do
      begin
        if Key = Ord('A') then
        begin
          dx := Position;
          if ssShift in Shift then Position := EnsureRange(Position - DotStep * 4, Min, Max)
          else Position := EnsureRange(Position - DotStep, Min, Max);
          MapOffset.X := -Position;
          dx -= Position;

          if (MouseLDown or MouseRDown) then
          begin
            if DrawRect <> nil then
            begin
              Inc(MouseLDownPos.x, dx);
              Inc(MouseRDownPos.x, dx);
            end;
            Inc(LastMovePoint.X, dx);
            RenderPanelMouseMove(Sender, Shift, RenderMousePos().X, RenderMousePos().Y);
          end;
        end;

        if Key = Ord('D') then
        begin
          dx := Position;
          if ssShift in Shift then Position := EnsureRange(Position + DotStep * 4, Min, Max)
          else Position := EnsureRange(Position + DotStep, Min, Max);
          MapOffset.X := -Position;
          dx -= Position;

          if (MouseLDown or MouseRDown) then
          begin
            if DrawRect <> nil then
            begin
              Inc(MouseLDownPos.x, dx);
              Inc(MouseRDownPos.x, dx);
            end;
            Inc(LastMovePoint.X, dx);
            RenderPanelMouseMove(Sender, Shift, RenderMousePos().X, RenderMousePos().Y);
          end;
        end;
      end;
    end
    else // ssCtrl in Shift
    begin
      if ssShift in Shift then
      begin
      // Вставка по абсолютному смещению:
        if Key = Ord('V') then
          aPasteObjectExecute(Sender);
      end;
      RenderPanelMouseMove(Sender, Shift, RenderMousePos().X, RenderMousePos().Y);
    end;
  end;

// Удалить выделенные объекты:
  if (Key = VK_DELETE) and (SelectedObjects <> nil) and
     RenderPanel.Focused() then
    DeleteSelectedObjects();

// Снять выделение:
  if (Key = VK_ESCAPE) and (SelectedObjects <> nil) then
    RemoveSelectFromObjects();

// Передвинуть объекты:
  if MainForm.ActiveControl = RenderPanel then
  begin
    dx := 0;
    dy := 0;

    if Key = VK_NUMPAD4 then
      dx := IfThen(ssAlt in Shift, -1, -DotStep);
    if Key = VK_NUMPAD6 then
      dx := IfThen(ssAlt in Shift, 1, DotStep);
    if Key = VK_NUMPAD8 then
      dy := IfThen(ssAlt in Shift, -1, -DotStep);
    if Key = VK_NUMPAD5 then
      dy := IfThen(ssAlt in Shift, 1, DotStep);

    if (dx <> 0) or (dy <> 0) then
    begin
      MoveSelectedObjects(ssShift in Shift, ssCtrl in Shift, dx, dy);
      Key := 0;
    end;
  end;

  if ssCtrl in Shift then
  begin
  // Выбор панели с текстурой для триггера
    if Key = Ord('T') then
    begin
      DrawPressRect := False;
      if SelectFlag = SELECTFLAG_TEXTURE then
      begin
        SelectFlag := SELECTFLAG_NONE;
        Exit;
      end;
      vleObjectProperty.FindRow(MsgPropTrTexturePanel, i);
      if i > 0 then
        SelectFlag := SELECTFLAG_TEXTURE;
    end;

    if Key = Ord('D') then
    begin
      SelectFlag := SELECTFLAG_NONE;
      if DrawPressRect then
      begin
        DrawPressRect := False;
        Exit;
      end;
      i := -1;

    // Выбор области воздействия, в зависимости от типа триггера
      vleObjectProperty.FindRow(MsgPropTrExArea, i);
      if i > 0 then
      begin
        DrawPressRect := True;
        Exit;
      end;
      vleObjectProperty.FindRow(MsgPropTrDoorPanel, i);
      if i <= 0 then
        vleObjectProperty.FindRow(MsgPropTrTrapPanel, i);
      if i > 0 then
      begin
        SelectFlag := SELECTFLAG_DOOR;
        Exit;
      end;
      vleObjectProperty.FindRow(MsgPropTrLiftPanel, i);
      if i > 0 then
      begin
        SelectFlag := SELECTFLAG_LIFT;
        Exit;
      end;
      vleObjectProperty.FindRow(MsgPropTrTeleportTo, i);
      if i > 0 then
      begin
        SelectFlag := SELECTFLAG_TELEPORT;
        Exit;
      end;
      vleObjectProperty.FindRow(MsgPropTrSpawnTo, i);
      if i > 0 then
      begin
        SelectFlag := SELECTFLAG_SPAWNPOINT;
        Exit;
      end;

    // Выбор основного параметра, в зависимости от типа триггера
      vleObjectProperty.FindRow(MsgPropTrNextMap, i);
      if i > 0 then
      begin
        g_ProcessResourceStr(OpenedMap, @FileName, nil, nil);
        SelectMapForm.Caption := MsgCapSelect;
        SelectMapForm.GetMaps(FileName);

        if SelectMapForm.ShowModal() = mrOK then
        begin
          vleObjectProperty.Cells[1, i] := SelectMapForm.lbMapList.Items[SelectMapForm.lbMapList.ItemIndex];
          bApplyProperty.Click();
        end;
        Exit;
      end;
      vleObjectProperty.FindRow(MsgPropTrSoundName, i);
      if i <= 0 then
        vleObjectProperty.FindRow(MsgPropTrMusicName, i);
      if i > 0 then
      begin
        AddSoundForm.OKFunction := nil;
        AddSoundForm.lbResourcesList.MultiSelect := False;
        AddSoundForm.SetResource := vleObjectProperty.Cells[1, i];

        if (AddSoundForm.ShowModal() = mrOk) then
        begin
          vleObjectProperty.Cells[1, i] := AddSoundForm.ResourceName;
          bApplyProperty.Click();
        end;
        Exit;
      end;
      vleObjectProperty.FindRow(MsgPropTrPushAngle, i);
      if i <= 0 then
        vleObjectProperty.FindRow(MsgPropTrMessageText, i);
      if i > 0 then
      begin
        vleObjectProperty.Row := i;
        vleObjectProperty.SetFocus();
        Exit;
      end;
    end;
  end;
end;

procedure TMainForm.aOptimizeExecute(Sender: TObject);
begin
  RemoveSelectFromObjects();
  MapOptimizationForm.ShowModal();
end;

procedure TMainForm.aCheckMapExecute(Sender: TObject);
begin
  MapCheckForm.ShowModal();
end;

procedure TMainForm.bbAddTextureClick(Sender: TObject);
begin
  AddTextureForm.lbResourcesList.MultiSelect := True;
  AddTextureForm.ShowModal();
end;

procedure TMainForm.lbTextureListClick(Sender: TObject);
var
  TextureID: DWORD;
  TextureWidth, TextureHeight: Word;
begin
  TextureID := 0;
  TextureWidth := 0;
  TextureHeight := 0;
  if (lbTextureList.ItemIndex <> -1) and
     (not IsSpecialTextureSel()) then
    begin
      if g_GetTexture(SelectedTexture(), TextureID) then
      begin
        g_GetTextureSizeByID(TextureID, TextureWidth, TextureHeight);

        lTextureWidth.Caption := IntToStr(TextureWidth);
        lTextureHeight.Caption := IntToStr(TextureHeight);
      end else
      begin
        lTextureWidth.Caption := MsgNotAccessible;
        lTextureHeight.Caption := MsgNotAccessible;
      end;
    end
  else
    begin
      lTextureWidth.Caption := '';
      lTextureHeight.Caption := '';
    end;
end;

procedure TMainForm.lbTextureListDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with Control as TListBox do
  begin
    if LCLType.odSelected in State then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText;
    end else
      if (Items <> nil) and (Index >= 0) then
        if slInvalidTextures.IndexOf(Items[Index]) > -1 then
        begin
          Canvas.Brush.Color := clRed;
          Canvas.Font.Color := clWhite;
        end;
    Canvas.FillRect(ARect);
    Canvas.TextRect(ARect, ARect.Left, ARect.Top, Items[Index]);
  end;
end;

procedure TMainForm.miMacMinimizeClick(Sender: TObject);
begin
  self.WindowState := wsMinimized;
  self.FormWindowStateChange(Sender);
end;

procedure TMainForm.miMacZoomClick(Sender: TObject);
begin
  if self.WindowState = wsMaximized then
    self.WindowState := wsNormal
  else
    self.WindowState := wsMaximized;
  self.FormWindowStateChange(Sender);
end;

procedure TMainForm.miReopenMapClick(Sender: TObject);
var
  FileName, Resource: String;
begin
  if OpenedMap = '' then
    Exit;

  if Application.MessageBox(PChar(MsgMsgReopenMapPromt),
  PChar(MsgMenuFileReopen), MB_ICONQUESTION or MB_YESNO) <> idYes then
    Exit;

  g_ProcessResourceStr(OpenedMap, @FileName, nil, @Resource);
  OpenMap(FileName, Resource);
end;

procedure TMainForm.vleObjectPropertyGetPickList(Sender: TObject;
  const KeyName: String; Values: TStrings);
begin
  if vleObjectProperty.ItemProps[KeyName].EditStyle = esPickList then
  begin
    if KeyName = MsgPropDirection then
      begin
        Values.Add(DirNames[D_LEFT]);
        Values.Add(DirNames[D_RIGHT]);
      end
    else if KeyName = MsgPropTrTeleportDir then
      begin
        Values.Add(DirNamesAdv[0]);
        Values.Add(DirNamesAdv[1]);
        Values.Add(DirNamesAdv[2]);
        Values.Add(DirNamesAdv[3]);
      end
    else if KeyName = MsgPropTrMusicAct then
      begin
        Values.Add(MsgPropTrMusicOn);
        Values.Add(MsgPropTrMusicOff);
      end
    else if KeyName = MsgPropTrMonsterBehaviour then
      begin
        Values.Add(MsgPropTrMonsterBehaviour0);
        Values.Add(MsgPropTrMonsterBehaviour1);
        Values.Add(MsgPropTrMonsterBehaviour2);
        Values.Add(MsgPropTrMonsterBehaviour3);
        Values.Add(MsgPropTrMonsterBehaviour4);
        Values.Add(MsgPropTrMonsterBehaviour5);
      end
    else if KeyName = MsgPropTrScoreAct then
      begin
        Values.Add(MsgPropTrScoreAct0);
        Values.Add(MsgPropTrScoreAct1);
        Values.Add(MsgPropTrScoreAct2);
        Values.Add(MsgPropTrScoreAct3);
      end
    else if KeyName = MsgPropTrScoreTeam then
      begin
        Values.Add(MsgPropTrScoreTeam0);
        Values.Add(MsgPropTrScoreTeam1);
        Values.Add(MsgPropTrScoreTeam2);
        Values.Add(MsgPropTrScoreTeam3);
      end
    else if KeyName = MsgPropTrMessageKind then
      begin
        Values.Add(MsgPropTrMessageKind0);
        Values.Add(MsgPropTrMessageKind1);
      end
    else if KeyName = MsgPropTrMessageTo then
      begin
        Values.Add(MsgPropTrMessageTo0);
        Values.Add(MsgPropTrMessageTo1);
        Values.Add(MsgPropTrMessageTo2);
        Values.Add(MsgPropTrMessageTo3);
        Values.Add(MsgPropTrMessageTo4);
        Values.Add(MsgPropTrMessageTo5);
      end
    else if KeyName = MsgPropTrShotTo then
      begin
        Values.Add(MsgPropTrShotTo0);
        Values.Add(MsgPropTrShotTo1);
        Values.Add(MsgPropTrShotTo2);
        Values.Add(MsgPropTrShotTo3);
        Values.Add(MsgPropTrShotTo4);
        Values.Add(MsgPropTrShotTo5);
        Values.Add(MsgPropTrShotTo6);
      end
    else if KeyName = MsgPropTrShotAim then
      begin
        Values.Add(MsgPropTrShotAim0);
        Values.Add(MsgPropTrShotAim1);
        Values.Add(MsgPropTrShotAim2);
        Values.Add(MsgPropTrShotAim3);
      end
    else if KeyName = MsgPropTrDamageKind then
      begin
        Values.Add(MsgPropTrDamageKind0);
        Values.Add(MsgPropTrDamageKind3);
        Values.Add(MsgPropTrDamageKind4);
        Values.Add(MsgPropTrDamageKind5);
        Values.Add(MsgPropTrDamageKind6);
        Values.Add(MsgPropTrDamageKind7);
        Values.Add(MsgPropTrDamageKind8);
      end
    else if (KeyName = MsgPropPanelBlend) or
            (KeyName = MsgPropDmOnly) or
            (KeyName = MsgPropItemFalls) or
            (KeyName = MsgPropTrEnabled) or
            (KeyName = MsgPropTrD2d) or
            (KeyName = MsgPropTrSilent) or
            (KeyName = MsgPropTrTeleportSilent) or
            (KeyName = MsgPropTrExRandom) or
            (KeyName = MsgPropTrTextureOnce) or
            (KeyName = MsgPropTrTextureAnimOnce) or
            (KeyName = MsgPropTrSoundLocal) or
            (KeyName = MsgPropTrSoundSwitch) or
            (KeyName = MsgPropTrMonsterActive) or
            (KeyName = MsgPropTrPushReset) or
            (KeyName = MsgPropTrScoreCon) or
            (KeyName = MsgPropTrScoreMsg) or
            (KeyName = MsgPropTrHealthMax) or
            (KeyName = MsgPropTrShotSound) or
            (KeyName = MsgPropTrEffectCenter) then
      begin
        Values.Add(BoolNames[True]);
        Values.Add(BoolNames[False]);
      end;
  end;
end;

procedure TMainForm.bApplyPropertyClick(Sender: TObject);
var
  _id, a, r, c: Integer;
  s: String;
  res: Boolean;
  NoTextureID: DWORD;
  NW, NH: Word;
begin
  NoTextureID := 0;
  NW := 0;
  NH := 0;

  if SelectedObjectCount() <> 1 then
    Exit;
  if not SelectedObjects[GetFirstSelected()].Live then
    Exit;

  try
    if not CheckProperty() then
      Exit;
  except
    Exit;
  end;

  _id := GetFirstSelected();

  r := vleObjectProperty.Row;
  c := vleObjectProperty.Col;

  case SelectedObjects[_id].ObjectType of
    OBJECT_PANEL:
      begin
        with gPanels[SelectedObjects[_id].ID] do
        begin
          X := StrToInt(Trim(vleObjectProperty.Values[MsgPropX]));
          Y := StrToInt(Trim(vleObjectProperty.Values[MsgPropY]));
          Width := StrToInt(Trim(vleObjectProperty.Values[MsgPropWidth]));
          Height := StrToInt(Trim(vleObjectProperty.Values[MsgPropHeight]));

          PanelType := GetPanelType(vleObjectProperty.Values[MsgPropPanelType]);

        // Сброс ссылки на триггеры смены текстуры:
          if not WordBool(PanelType and (PANEL_WALL or PANEL_FORE or PANEL_BACK)) then
            if gTriggers <> nil then
              for a := 0 to High(gTriggers) do
              begin
                if (gTriggers[a].TriggerType <> 0) and
                   (gTriggers[a].TexturePanel = Integer(SelectedObjects[_id].ID)) then
                  gTriggers[a].TexturePanel := -1;
                if (gTriggers[a].TriggerType = TRIGGER_SHOT) and
                   (gTriggers[a].Data.ShotPanelID = Integer(SelectedObjects[_id].ID)) then
                  gTriggers[a].Data.ShotPanelID := -1;
              end;

        // Сброс ссылки на триггеры лифта:
          if not WordBool(PanelType and (PANEL_LIFTUP or PANEL_LIFTDOWN or PANEL_LIFTLEFT or PANEL_LIFTRIGHT)) then
            if gTriggers <> nil then
              for a := 0 to High(gTriggers) do
                if (gTriggers[a].TriggerType in [TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT]) and
                   (gTriggers[a].Data.PanelID = Integer(SelectedObjects[_id].ID)) then
                     gTriggers[a].Data.PanelID := -1;

        // Сброс ссылки на триггеры двери:
          if not WordBool(PanelType and (PANEL_OPENDOOR or PANEL_CLOSEDOOR)) then
            if gTriggers <> nil then
              for a := 0 to High(gTriggers) do
                if (gTriggers[a].TriggerType in [TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR,
                                                 TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP]) and
                   (gTriggers[a].Data.PanelID = Integer(SelectedObjects[_id].ID)) then
                  gTriggers[a].Data.PanelID := -1;

          if IsTexturedPanel(PanelType) then
            begin // Может быть текстура
              if TextureName <> '' then
                begin // Была текстура
                  Alpha := StrToInt(Trim(vleObjectProperty.Values[MsgPropPanelAlpha]));
                  Blending := NameToBool(vleObjectProperty.Values[MsgPropPanelBlend]);
                end
              else // Не было
                begin
                  Alpha := 0;
                  Blending := False;
                end;

            // Новая текстура:
              TextureName := vleObjectProperty.Values[MsgPropPanelTex];

              if TextureName <> '' then
                begin // Есть текстура
                // Обычная текстура:
                  if not IsSpecialTexture(TextureName) then
                    begin
                      g_GetTextureSizeByName(TextureName,
                        TextureWidth, TextureHeight);

                    // Проверка кратности размеров панели:
                      res := True;
                      if TextureWidth <> 0 then
                        if gPanels[SelectedObjects[_id].ID].Width mod TextureWidth <> 0 then
                        begin
                          ErrorMessageBox(Format(MsgMsgWrongTexwidth,
                                          [TextureWidth]));
                          Res := False;
                        end;
                      if Res and (TextureHeight <> 0) then
                        if gPanels[SelectedObjects[_id].ID].Height mod TextureHeight <> 0 then
                        begin
                          ErrorMessageBox(Format(MsgMsgWrongTexheight,
                                          [TextureHeight]));
                          Res := False;
                        end;

                      if Res then
                      begin
                        if not g_GetTexture(TextureName, TextureID) then
                          // Не удалось загрузить текстуру, рисуем NOTEXTURE
                          if g_GetTexture('NOTEXTURE', NoTextureID) then
                          begin
                            TextureID := TEXTURE_SPECIAL_NOTEXTURE;
                            g_GetTextureSizeByID(NoTextureID, NW, NH);
                            TextureWidth := NW;
                            TextureHeight := NH;
                          end else
                          begin
                            TextureID := TEXTURE_SPECIAL_NONE;
                            TextureWidth := 1;
                            TextureHeight := 1;
                          end;
                      end
                      else
                        begin
                          TextureName := '';
                          TextureWidth := 1;
                          TextureHeight := 1;
                          TextureID := TEXTURE_SPECIAL_NONE;
                        end;
                    end
                  else // Спец.текстура
                    begin
                      TextureHeight := 1;
                      TextureWidth := 1;
                      TextureID := SpecialTextureID(TextureName);
                    end;
                end
              else // Нет текстуры
                begin
                  TextureWidth := 1;
                  TextureHeight := 1;
                  TextureID := TEXTURE_SPECIAL_NONE;
                end;
            end
          else // Не может быть текстуры
            begin
              Alpha := 0;
              Blending := False;
              TextureName := '';
              TextureWidth := 1;
              TextureHeight := 1;
              TextureID := TEXTURE_SPECIAL_NONE;
            end;
        end;
      end;

    OBJECT_ITEM:
      begin
        with gItems[SelectedObjects[_id].ID] do
        begin
          X := StrToInt(Trim(vleObjectProperty.Values[MsgPropX]));
          Y := StrToInt(Trim(vleObjectProperty.Values[MsgPropY]));
          OnlyDM := NameToBool(vleObjectProperty.Values[MsgPropDmOnly]);
          Fall := NameToBool(vleObjectProperty.Values[MsgPropItemFalls]);
        end;
      end;

    OBJECT_MONSTER:
      begin
        with gMonsters[SelectedObjects[_id].ID] do
        begin
          X := StrToInt(Trim(vleObjectProperty.Values[MsgPropX]));
          Y := StrToInt(Trim(vleObjectProperty.Values[MsgPropY]));
          Direction := NameToDir(vleObjectProperty.Values[MsgPropDirection]);
        end;
      end;

    OBJECT_AREA:
      begin
        with gAreas[SelectedObjects[_id].ID] do
        begin
          X := StrToInt(Trim(vleObjectProperty.Values[MsgPropX]));
          Y := StrToInt(Trim(vleObjectProperty.Values[MsgPropY]));
          Direction := NameToDir(vleObjectProperty.Values[MsgPropDirection]);
        end;
      end;

    OBJECT_TRIGGER:
      begin
        with gTriggers[SelectedObjects[_id].ID] do
        begin
          X := StrToInt(Trim(vleObjectProperty.Values[MsgPropX]));
          Y := StrToInt(Trim(vleObjectProperty.Values[MsgPropY]));
          Width := StrToInt(Trim(vleObjectProperty.Values[MsgPropWidth]));
          Height := StrToInt(Trim(vleObjectProperty.Values[MsgPropHeight]));
          Enabled := NameToBool(vleObjectProperty.Values[MsgPropTrEnabled]);
          ActivateType := StrToActivate(vleObjectProperty.Values[MsgPropTrActivation]);
          Key := StrToKey(vleObjectProperty.Values[MsgPropTrKeys]);

          case TriggerType of
            TRIGGER_EXIT:
              begin
                s := utf2win(vleObjectProperty.Values[MsgPropTrNextMap]);
                FillByte(Data.MapName[0], 16, 0);
                if s <> '' then
                  Move(s[1], Data.MapName[0], Min(Length(s), 16));
              end;

            TRIGGER_TEXTURE:
              begin
                Data.ActivateOnce := NameToBool(vleObjectProperty.Values[MsgPropTrTextureOnce]);
                Data.AnimOnce := NameToBool(vleObjectProperty.Values[MsgPropTrTextureAnimOnce]);
              end;

            TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF:
              begin
                Data.Wait := Min(StrToIntDef(vleObjectProperty.Values[MsgPropTrExDelay], 0), 65535);
                Data.Count := Min(StrToIntDef(vleObjectProperty.Values[MsgPropTrExCount], 0), 65535);
                if Data.Count < 1 then
                  Data.Count := 1;
                if TriggerType = TRIGGER_PRESS then
                  Data.ExtRandom := NameToBool(vleObjectProperty.Values[MsgPropTrExRandom]);
              end;

            TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR, TRIGGER_DOOR5,
            TRIGGER_CLOSETRAP, TRIGGER_TRAP, TRIGGER_LIFTUP, TRIGGER_LIFTDOWN,
            TRIGGER_LIFT:
              begin
                Data.NoSound := NameToBool(vleObjectProperty.Values[MsgPropTrSilent]);
                Data.d2d_doors := NameToBool(vleObjectProperty.Values[MsgPropTrD2d]);
              end;

            TRIGGER_TELEPORT:
              begin
                Data.d2d_teleport := NameToBool(vleObjectProperty.Values[MsgPropTrD2d]);
                Data.silent_teleport := NameToBool(vleObjectProperty.Values[MsgPropTrTeleportSilent]);
                Data.TlpDir := NameToDirAdv(vleObjectProperty.Values[MsgPropTrTeleportDir]);
              end;

            TRIGGER_SOUND:
              begin
                s := utf2win(vleObjectProperty.Values[MsgPropTrSoundName]);
                FillByte(Data.SoundName[0], 64, 0);
                if s <> '' then
                  Move(s[1], Data.SoundName[0], Min(Length(s), 64));

                Data.Volume := Min(StrToIntDef(vleObjectProperty.Values[MsgPropTrSoundVolume], 0), 255);
                Data.Pan := Min(StrToIntDef(vleObjectProperty.Values[MsgPropTrSoundPan], 0), 255);
                Data.PlayCount := Min(StrToIntDef(vleObjectProperty.Values[MsgPropTrSoundCount], 0), 255);
                Data.Local := NameToBool(vleObjectProperty.Values[MsgPropTrSoundLocal]);
                Data.SoundSwitch := NameToBool(vleObjectProperty.Values[MsgPropTrSoundSwitch]);
              end;

            TRIGGER_SPAWNMONSTER:
              begin
                Data.MonType := StrToMonster(vleObjectProperty.Values[MsgPropTrMonsterType]);
                Data.MonDir := Byte(NameToDir(vleObjectProperty.Values[MsgPropDirection]));
                Data.MonHealth := Min(StrToIntDef(vleObjectProperty.Values[MsgPropTrHealth], 0), 1000000);
                if Data.MonHealth < 0 then
                  Data.MonHealth := 0;
                Data.MonActive := NameToBool(vleObjectProperty.Values[MsgPropTrMonsterActive]);
                Data.MonCount := Min(StrToIntDef(vleObjectProperty.Values[MsgPropTrCount], 0), 64);
                if Data.MonCount < 1 then
                  Data.MonCount := 1;
                Data.MonEffect := StrToEffect(vleObjectProperty.Values[MsgPropTrFxType]);
                Data.MonMax := Min(StrToIntDef(vleObjectProperty.Values[MsgPropTrSpawnMax], 0), 65535);
                Data.MonDelay := Min(StrToIntDef(vleObjectProperty.Values[MsgPropTrSpawnDelay], 0), 65535);
                Data.MonBehav := 0;
                if vleObjectProperty.Values[MsgPropTrMonsterBehaviour] = MsgPropTrMonsterBehaviour1 then
                  Data.MonBehav := 1;
                if vleObjectProperty.Values[MsgPropTrMonsterBehaviour] = MsgPropTrMonsterBehaviour2 then
                  Data.MonBehav := 2;
                if vleObjectProperty.Values[MsgPropTrMonsterBehaviour] = MsgPropTrMonsterBehaviour3 then
                  Data.MonBehav := 3;
                if vleObjectProperty.Values[MsgPropTrMonsterBehaviour] = MsgPropTrMonsterBehaviour4 then
                  Data.MonBehav := 4;
                if vleObjectProperty.Values[MsgPropTrMonsterBehaviour] = MsgPropTrMonsterBehaviour5 then
                  Data.MonBehav := 5;
              end;

            TRIGGER_SPAWNITEM:
              begin
                Data.ItemType := StrToItem(vleObjectProperty.Values[MsgPropTrItemType]);
                Data.ItemOnlyDM := NameToBool(vleObjectProperty.Values[MsgPropDmOnly]);
                Data.ItemFalls := NameToBool(vleObjectProperty.Values[MsgPropItemFalls]);
                Data.ItemCount := Min(StrToIntDef(vleObjectProperty.Values[MsgPropTrCount], 0), 64);
                if Data.ItemCount < 1 then
                  Data.ItemCount := 1;
                Data.ItemEffect := StrToEffect(vleObjectProperty.Values[MsgPropTrFxType]);
                Data.ItemMax := Min(StrToIntDef(vleObjectProperty.Values[MsgPropTrSpawnMax], 0), 65535);
                Data.ItemDelay := Min(StrToIntDef(vleObjectProperty.Values[MsgPropTrSpawnDelay], 0), 65535);
              end;

            TRIGGER_MUSIC:
              begin
                s := utf2win(vleObjectProperty.Values[MsgPropTrMusicName]);
                FillByte(Data.MusicName[0], 64, 0);
                if s <> '' then
                  Move(s[1], Data.MusicName[0], Min(Length(s), 64));

                if vleObjectProperty.Values[MsgPropTrMusicAct] = MsgPropTrMusicOn then
                  Data.MusicAction := 1
                else
                  Data.MusicAction := 0;
              end;

            TRIGGER_PUSH:
              begin
                Data.PushAngle := Min(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrPushAngle], 0), 360);
                Data.PushForce := Min(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrPushForce], 0), 255);
                Data.ResetVel := NameToBool(vleObjectProperty.Values[MsgPropTrPushReset]);
              end;

            TRIGGER_SCORE:
              begin
                Data.ScoreAction := 0;
                if vleObjectProperty.Values[MsgPropTrScoreAct] = MsgPropTrScoreAct1 then
                  Data.ScoreAction := 1
                else if vleObjectProperty.Values[MsgPropTrScoreAct] = MsgPropTrScoreAct2 then
                  Data.ScoreAction := 2
                else if vleObjectProperty.Values[MsgPropTrScoreAct] = MsgPropTrScoreAct3 then
                  Data.ScoreAction := 3;
                Data.ScoreCount := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrCount], 0), 0), 255);
                Data.ScoreTeam := 0;
                if vleObjectProperty.Values[MsgPropTrScoreTeam] = MsgPropTrScoreTeam1 then
                  Data.ScoreTeam := 1
                else if vleObjectProperty.Values[MsgPropTrScoreTeam] = MsgPropTrScoreTeam2 then
                  Data.ScoreTeam := 2
                else if vleObjectProperty.Values[MsgPropTrScoreTeam] = MsgPropTrScoreTeam3 then
                  Data.ScoreTeam := 3;
                Data.ScoreCon := NameToBool(vleObjectProperty.Values[MsgPropTrScoreCon]);
                Data.ScoreMsg := NameToBool(vleObjectProperty.Values[MsgPropTrScoreMsg]);
              end;

            TRIGGER_MESSAGE:
              begin
                Data.MessageKind := 0;
                if vleObjectProperty.Values[MsgPropTrMessageKind] = MsgPropTrMessageKind1 then
                  Data.MessageKind := 1;

                Data.MessageSendTo := 0;
                if vleObjectProperty.Values[MsgPropTrMessageTo] = MsgPropTrMessageTo1 then
                  Data.MessageSendTo := 1
                else if vleObjectProperty.Values[MsgPropTrMessageTo] = MsgPropTrMessageTo2 then
                  Data.MessageSendTo := 2
                else if vleObjectProperty.Values[MsgPropTrMessageTo] = MsgPropTrMessageTo3 then
                  Data.MessageSendTo := 3
                else if vleObjectProperty.Values[MsgPropTrMessageTo] = MsgPropTrMessageTo4 then
                  Data.MessageSendTo := 4
                else if vleObjectProperty.Values[MsgPropTrMessageTo] = MsgPropTrMessageTo5 then
                  Data.MessageSendTo := 5;

                s := utf2win(vleObjectProperty.Values[MsgPropTrMessageText]);
                FillByte(Data.MessageText[0], 100, 0);
                if s <> '' then
                  Move(s[1], Data.MessageText[0], Min(Length(s), 100));

                Data.MessageTime := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrMessageTime], 0), 0), 65535);
              end;

            TRIGGER_DAMAGE:
              begin
                Data.DamageValue := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrDamageValue], 0), 0), 65535);
                Data.DamageInterval := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrInterval], 0), 0), 65535);
                s := vleObjectProperty.Values[MsgPropTrDamageKind];
                if s = MsgPropTrDamageKind3 then
                  Data.DamageKind := 3
                else if s = MsgPropTrDamageKind4 then
                  Data.DamageKind := 4
                else if s = MsgPropTrDamageKind5 then
                  Data.DamageKind := 5
                else if s = MsgPropTrDamageKind6 then
                  Data.DamageKind := 6
                else if s = MsgPropTrDamageKind7 then
                  Data.DamageKind := 7
                else if s = MsgPropTrDamageKind8 then
                  Data.DamageKind := 8
                else
                  Data.DamageKind := 0;
              end;

            TRIGGER_HEALTH:
              begin
                Data.HealValue := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrHealth], 0), 0), 65535);
                Data.HealInterval := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrInterval], 0), 0), 65535);
                Data.HealMax := NameToBool(vleObjectProperty.Values[MsgPropTrHealthMax]);
                Data.HealSilent := NameToBool(vleObjectProperty.Values[MsgPropTrSilent]);
              end;

            TRIGGER_SHOT:
              begin
                Data.ShotType := StrToShot(vleObjectProperty.Values[MsgPropTrShotType]);
                Data.ShotSound := NameToBool(vleObjectProperty.Values[MsgPropTrShotSound]);
                Data.ShotTarget := 0;
                if vleObjectProperty.Values[MsgPropTrShotTo] = MsgPropTrShotTo1 then
                  Data.ShotTarget := 1
                else if vleObjectProperty.Values[MsgPropTrShotTo] = MsgPropTrShotTo2 then
                  Data.ShotTarget := 2
                else if vleObjectProperty.Values[MsgPropTrShotTo] = MsgPropTrShotTo3 then
                  Data.ShotTarget := 3
                else if vleObjectProperty.Values[MsgPropTrShotTo] = MsgPropTrShotTo4 then
                  Data.ShotTarget := 4
                else if vleObjectProperty.Values[MsgPropTrShotTo] = MsgPropTrShotTo5 then
                  Data.ShotTarget := 5
                else if vleObjectProperty.Values[MsgPropTrShotTo] = MsgPropTrShotTo6 then
                  Data.ShotTarget := 6;
                Data.ShotIntSight := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrShotSight], 0), 0), 65535);
                Data.ShotAim := 0;
                if vleObjectProperty.Values[MsgPropTrShotAim] = MsgPropTrShotAim1 then
                  Data.ShotAim := 1
                else if vleObjectProperty.Values[MsgPropTrShotAim] = MsgPropTrShotAim2 then
                  Data.ShotAim := 2
                else if vleObjectProperty.Values[MsgPropTrShotAim] = MsgPropTrShotAim3 then
                  Data.ShotAim := 3;
                Data.ShotAngle := Min(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrShotAngle], 0), 360);
                Data.ShotWait := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrExDelay], 0), 0), 65535);
                Data.ShotAccuracy := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrShotAcc], 0), 0), 65535);
                Data.ShotAmmo := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrShotAmmo], 0), 0), 65535);
                Data.ShotIntReload := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrShotReload], 0), 0), 65535);
              end;

            TRIGGER_EFFECT:
              begin
                Data.FXCount := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrCount], 0), 0), 255);
                if vleObjectProperty.Values[MsgPropTrEffectType] = MsgPropTrEffectParticle then
                begin
                  Data.FXType := TRIGGER_EFFECT_PARTICLE;
                  Data.FXSubType := TRIGGER_EFFECT_SLIQUID;
                  if vleObjectProperty.Values[MsgPropTrEffectSubtype] = MsgPropTrEffectSliquid then
                    Data.FXSubType := TRIGGER_EFFECT_SLIQUID
                  else if vleObjectProperty.Values[MsgPropTrEffectSubtype] = MsgPropTrEffectLliquid then
                    Data.FXSubType := TRIGGER_EFFECT_LLIQUID
                  else if vleObjectProperty.Values[MsgPropTrEffectSubtype] = MsgPropTrEffectDliquid then
                    Data.FXSubType := TRIGGER_EFFECT_DLIQUID
                  else if vleObjectProperty.Values[MsgPropTrEffectSubtype] = MsgPropTrEffectBlood then
                    Data.FXSubType := TRIGGER_EFFECT_BLOOD
                  else if vleObjectProperty.Values[MsgPropTrEffectSubtype] = MsgPropTrEffectSpark then
                    Data.FXSubType := TRIGGER_EFFECT_SPARK
                  else if vleObjectProperty.Values[MsgPropTrEffectSubtype] = MsgPropTrEffectBubble then
                    Data.FXSubType := TRIGGER_EFFECT_BUBBLE;
                end else
                begin
                  Data.FXType := TRIGGER_EFFECT_ANIMATION;
                  Data.FXSubType := StrToEffect(vleObjectProperty.Values[MsgPropTrEffectSubtype]);
                end;
                a := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrEffectColor], 0), 0), $FFFFFF);
                Data.FXColorR := a and $FF;
                Data.FXColorG := (a shr 8) and $FF;
                Data.FXColorB := (a shr 16) and $FF;
                if NameToBool(vleObjectProperty.Values[MsgPropTrEffectCenter]) then
                  Data.FXPos := 0
                else
                  Data.FXPos := 1;
                Data.FXWait := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrExDelay], 0), 0), 65535);
                Data.FXVelX := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrEffectVelx], 0), -128), 127);
                Data.FXVelY := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrEffectVely], 0), -128), 127);
                Data.FXSpreadL := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrEffectSpl], 0), 0), 255);
                Data.FXSpreadR := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrEffectSpr], 0), 0), 255);
                Data.FXSpreadU := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrEffectSpu], 0), 0), 255);
                Data.FXSpreadD := Min(Max(
                  StrToIntDef(vleObjectProperty.Values[MsgPropTrEffectSpd], 0), 0), 255);
              end;
          end;
        end;
      end;
  end;

  FillProperty();

  vleObjectProperty.Row := r;
  vleObjectProperty.Col := c;
end;

procedure TMainForm.bbRemoveTextureClick(Sender: TObject);
var
  a, i: Integer;
begin
  i := lbTextureList.ItemIndex;
  if i = -1 then
    Exit;

  if Application.MessageBox(PChar(Format(MsgMsgDelTexturePromt,
                                [SelectedTexture()])),
                PChar(MsgMsgDelTexture),
                MB_ICONQUESTION or MB_YESNO or
                MB_DEFBUTTON1) <> idYes then
    Exit;

  if gPanels <> nil then
    for a := 0 to High(gPanels) do
      if (gPanels[a].PanelType <> 0) and
         (gPanels[a].TextureName = SelectedTexture()) then
      begin
        ErrorMessageBox(MsgMsgDelTextureCant);
        Exit;
      end;

  g_DeleteTexture(SelectedTexture());
  i := slInvalidTextures.IndexOf(lbTextureList.Items[i]);
  if i > -1 then
    slInvalidTextures.Delete(i);
  if lbTextureList.ItemIndex > -1 then
    lbTextureList.Items.Delete(lbTextureList.ItemIndex)
end;

procedure TMainForm.aNewMapExecute(Sender: TObject);
begin
  if Application.MessageBox(PChar(MsgMsgClearMapPromt), PChar(MsgMsgClearMap), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1) = mrYes then
    FullClear();
end;

procedure TMainForm.aUndoExecute(Sender: TObject);
var
  a: Integer;
begin
  if UndoBuffer = nil then
    Exit;
  if UndoBuffer[High(UndoBuffer)] = nil then
    Exit;

  for a := 0 to High(UndoBuffer[High(UndoBuffer)]) do
    with UndoBuffer[High(UndoBuffer)][a] do
    begin
      case UndoType of
        UNDO_DELETE_PANEL:
          begin
            AddPanel(Panel^);
            Panel := nil;
          end;
        UNDO_DELETE_ITEM: AddItem(Item);
        UNDO_DELETE_AREA: AddArea(Area);
        UNDO_DELETE_MONSTER: AddMonster(Monster);
        UNDO_DELETE_TRIGGER: AddTrigger(Trigger);
        UNDO_ADD_PANEL: RemoveObject(AddID, OBJECT_PANEL);
        UNDO_ADD_ITEM: RemoveObject(AddID, OBJECT_ITEM);
        UNDO_ADD_AREA: RemoveObject(AddID, OBJECT_AREA);
        UNDO_ADD_MONSTER: RemoveObject(AddID, OBJECT_MONSTER);
        UNDO_ADD_TRIGGER: RemoveObject(AddID, OBJECT_TRIGGER);
      end;
    end;

  SetLength(UndoBuffer, Length(UndoBuffer)-1);

  RemoveSelectFromObjects();

  miUndo.Enabled := UndoBuffer <> nil;
end;


procedure TMainForm.aCopyObjectExecute(Sender: TObject);
var
  a, b: Integer;
  CopyBuffer: TCopyRecArray;
  str: String;
  ok: Boolean;

  function CB_Compare(I1, I2: TCopyRec): Integer;
  begin
    Result := Integer(I1.ObjectType) - Integer(I2.ObjectType);

    if Result = 0 then // Одного типа
      Result := Integer(I1.ID) - Integer(I2.ID);
  end;

  procedure QuickSortCopyBuffer(L, R: Integer);
  var
    I, J: Integer;
    P, T: TCopyRec;
  begin
    repeat
      I := L;
      J := R;
      P := CopyBuffer[(L + R) shr 1];

      repeat
        while CB_Compare(CopyBuffer[I], P) < 0 do
          Inc(I);
        while CB_Compare(CopyBuffer[J], P) > 0 do
          Dec(J);

        if I <= J then
        begin
          T := CopyBuffer[I];
          CopyBuffer[I] := CopyBuffer[J];
          CopyBuffer[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;

      if L < J then
        QuickSortCopyBuffer(L, J);

      L := I;
    until I >= R;
  end;

begin
  if SelectedObjects = nil then
    Exit;

  b := -1;
  CopyBuffer := nil;

// Копируем объекты:
  for a := 0 to High(SelectedObjects) do
    if SelectedObjects[a].Live then
      with SelectedObjects[a] do
      begin
        SetLength(CopyBuffer, Length(CopyBuffer)+1);
        b := High(CopyBuffer);
        CopyBuffer[b].ID := ID;
        CopyBuffer[b].Panel := nil;

        case ObjectType of
          OBJECT_PANEL:
            begin
              CopyBuffer[b].ObjectType := OBJECT_PANEL;
              New(CopyBuffer[b].Panel);
              CopyBuffer[b].Panel^ := gPanels[ID];
            end;

          OBJECT_ITEM:
            begin
              CopyBuffer[b].ObjectType := OBJECT_ITEM;
              CopyBuffer[b].Item := gItems[ID];
            end;

          OBJECT_MONSTER:
            begin
              CopyBuffer[b].ObjectType := OBJECT_MONSTER;
              CopyBuffer[b].Monster := gMonsters[ID];
            end;

          OBJECT_AREA:
            begin
              CopyBuffer[b].ObjectType := OBJECT_AREA;
              CopyBuffer[b].Area := gAreas[ID];
            end;

          OBJECT_TRIGGER:
            begin
              CopyBuffer[b].ObjectType := OBJECT_TRIGGER;
              CopyBuffer[b].Trigger := gTriggers[ID];
            end;
        end;
      end;

// Сортировка по ID:
  if CopyBuffer <> nil then
  begin
    QuickSortCopyBuffer(0, b);
  end;

// Постановка ссылок триггеров:
  for a := 0 to Length(CopyBuffer)-1 do
    if CopyBuffer[a].ObjectType = OBJECT_TRIGGER then
    begin
      case CopyBuffer[a].Trigger.TriggerType of
        TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR,
        TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP,
        TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT:
          if CopyBuffer[a].Trigger.Data.PanelID <> -1 then
          begin
            ok := False;

            for b := 0 to Length(CopyBuffer)-1 do
              if (CopyBuffer[b].ObjectType = OBJECT_PANEL) and
                 (Integer(CopyBuffer[b].ID) = CopyBuffer[a].Trigger.Data.PanelID) then
              begin
                CopyBuffer[a].Trigger.Data.PanelID := b;
                ok := True;
                Break;
              end;

          // Этих панелей нет среди копируемых:
            if not ok then
              CopyBuffer[a].Trigger.Data.PanelID := -1;
          end;
              
        TRIGGER_PRESS, TRIGGER_ON,
        TRIGGER_OFF, TRIGGER_ONOFF:
          if CopyBuffer[a].Trigger.Data.MonsterID <> 0 then
          begin
            ok := False;

            for b := 0 to Length(CopyBuffer)-1 do
              if (CopyBuffer[b].ObjectType = OBJECT_MONSTER) and
                 (Integer(CopyBuffer[b].ID) = CopyBuffer[a].Trigger.Data.MonsterID-1) then
              begin
                CopyBuffer[a].Trigger.Data.MonsterID := b+1;
                ok := True;
                Break;
              end;

          // Этих монстров нет среди копируемых:
            if not ok then
              CopyBuffer[a].Trigger.Data.MonsterID := 0;
          end;

        TRIGGER_SHOT:
          if CopyBuffer[a].Trigger.Data.ShotPanelID <> -1 then
          begin
            ok := False;

            for b := 0 to Length(CopyBuffer)-1 do
              if (CopyBuffer[b].ObjectType = OBJECT_PANEL) and
                 (Integer(CopyBuffer[b].ID) = CopyBuffer[a].Trigger.Data.ShotPanelID) then
              begin
                CopyBuffer[a].Trigger.Data.ShotPanelID := b;
                ok := True;
                Break;
              end;

          // Этих панелей нет среди копируемых:
            if not ok then
              CopyBuffer[a].Trigger.Data.ShotPanelID := -1;
          end;
      end;

      if CopyBuffer[a].Trigger.TexturePanel <> -1 then
      begin
        ok := False;

        for b := 0 to Length(CopyBuffer)-1 do
          if (CopyBuffer[b].ObjectType = OBJECT_PANEL) and
             (Integer(CopyBuffer[b].ID) = CopyBuffer[a].Trigger.TexturePanel) then
          begin
            CopyBuffer[a].Trigger.TexturePanel := b;
            ok := True;
            Break;
          end;

      // Этих панелей нет среди копируемых:
        if not ok then
          CopyBuffer[a].Trigger.TexturePanel := -1;
      end;
    end;

// В буфер обмена:
  str := CopyBufferToString(CopyBuffer);
  ClipBoard.AsText := str;

  for a := 0 to Length(CopyBuffer)-1 do
    if (CopyBuffer[a].ObjectType = OBJECT_PANEL) and
       (CopyBuffer[a].Panel <> nil) then
      Dispose(CopyBuffer[a].Panel);

  CopyBuffer := nil;
end;

procedure TMainForm.aPasteObjectExecute(Sender: TObject);
var
  a, h: Integer;
  CopyBuffer: TCopyRecArray;
  res, rel: Boolean;
  swad, ssec, sres: String;
  NoTextureID: DWORD;
  pmin: TPoint;
  xadj, yadj: LongInt;
begin
  CopyBuffer := nil;
  NoTextureID := 0;

  pmin.X := High(pmin.X);
  pmin.Y := High(pmin.Y);

  StringToCopyBuffer(ClipBoard.AsText, CopyBuffer, pmin);
  if CopyBuffer = nil then
    Exit;

  rel := not(ssShift in GetKeyShiftState());
  h := High(CopyBuffer);
  RemoveSelectFromObjects();

  if h > 0 then
  begin
    xadj := -pmin.X - Floor((MapOffset.X - 32) / DotStep) * DotStep;
    yadj := -pmin.Y - Floor((MapOffset.Y - 32) / DotStep) * DotStep;
  end
  else
  begin
    xadj := DotStep;
    yadj := DotStep;
  end;

  for a := 0 to h do
    with CopyBuffer[a] do
    begin
      case ObjectType of
        OBJECT_PANEL:
          if Panel <> nil then
          begin
            if rel then
            begin
              Panel^.X += xadj;
              Panel^.Y += yadj;
            end;

            Panel^.TextureID := TEXTURE_SPECIAL_NONE;
            Panel^.TextureWidth := 1;
            Panel^.TextureHeight := 1;

            if (Panel^.PanelType = PANEL_LIFTUP) or
               (Panel^.PanelType = PANEL_LIFTDOWN) or
               (Panel^.PanelType = PANEL_LIFTLEFT) or
               (Panel^.PanelType = PANEL_LIFTRIGHT) or
               (Panel^.PanelType = PANEL_BLOCKMON) or
               (Panel^.TextureName = '') then
              begin // Нет или не может быть текстуры:
              end
            else // Есть текстура:
              begin
              // Обычная текстура:
                if not IsSpecialTexture(Panel^.TextureName) then
                  begin
                    res := g_GetTexture(Panel^.TextureName, Panel^.TextureID);

                    if not res then
                    begin
                      g_ProcessResourceStr(Panel^.TextureName, swad, ssec, sres);
                      AddTexture(swad, ssec, sres, True);
                      res := g_GetTexture(Panel^.TextureName, Panel^.TextureID);
                    end;

                    if res then
                      g_GetTextureSizeByName(Panel^.TextureName,
                        Panel^.TextureWidth, Panel^.TextureHeight)
                    else
                      if g_GetTexture('NOTEXTURE', NoTextureID) then
                      begin
                        Panel^.TextureID := TEXTURE_SPECIAL_NOTEXTURE;
                        g_GetTextureSizeByID(NoTextureID, Panel^.TextureWidth, Panel^.TextureHeight);
                      end;
                  end
                else // Спец.текстура:
                  begin
                    Panel^.TextureID := SpecialTextureID(Panel^.TextureName);
                    with MainForm.lbTextureList.Items do
                      if IndexOf(Panel^.TextureName) = -1 then
                        Add(Panel^.TextureName);
                  end;
              end;

            ID := AddPanel(Panel^);
            Dispose(Panel);
            Undo_Add(OBJECT_PANEL, ID, a > 0);
            SelectObject(OBJECT_PANEL, ID, True);
          end;

        OBJECT_ITEM:
          begin
            if rel then
            begin
              Item.X += xadj;
              Item.Y += yadj;
            end;

            ID := AddItem(Item);
            Undo_Add(OBJECT_ITEM, ID, a > 0);
            SelectObject(OBJECT_ITEM, ID, True);
          end;

        OBJECT_MONSTER:
          begin
            if rel then
            begin
              Monster.X += xadj;
              Monster.Y += yadj;
            end;

            ID := AddMonster(Monster);
            Undo_Add(OBJECT_MONSTER, ID, a > 0);
            SelectObject(OBJECT_MONSTER, ID, True);
          end;

        OBJECT_AREA:
          begin
            if rel then
            begin
              Area.X += xadj;
              Area.Y += yadj;
            end;

            ID := AddArea(Area);
            Undo_Add(OBJECT_AREA, ID, a > 0);
            SelectObject(OBJECT_AREA, ID, True);
          end;

        OBJECT_TRIGGER:
          begin
            if rel then
              with Trigger do
              begin
                X += xadj;
                Y += yadj;

                case TriggerType of
                  TRIGGER_TELEPORT:
                    begin
                      Data.TargetPoint.X += xadj;
                      Data.TargetPoint.Y += yadj;
                    end;
                  TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF:
                    begin
                      Data.tX += xadj;
                      Data.tY += yadj;
                    end;
                  TRIGGER_SPAWNMONSTER:
                    begin
                      Data.MonPos.X += xadj;
                      Data.MonPos.Y += yadj;
                    end;
                  TRIGGER_SPAWNITEM:
                    begin
                      Data.ItemPos.X += xadj;
                      Data.ItemPos.Y += yadj;
                    end;
                  TRIGGER_SHOT:
                    begin
                      Data.ShotPos.X += xadj;
                      Data.ShotPos.Y += yadj;
                    end;
                end;
              end;

            ID := AddTrigger(Trigger);
            Undo_Add(OBJECT_TRIGGER, ID, a > 0);
            SelectObject(OBJECT_TRIGGER, ID, True);
          end;
      end;
    end;

// Переставляем ссылки триггеров:
  for a := 0 to High(CopyBuffer) do
    if CopyBuffer[a].ObjectType = OBJECT_TRIGGER then
    begin
      case CopyBuffer[a].Trigger.TriggerType of
        TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR,
        TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP,
        TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT:
          if CopyBuffer[a].Trigger.Data.PanelID <> -1 then
            gTriggers[CopyBuffer[a].ID].Data.PanelID :=
              CopyBuffer[CopyBuffer[a].Trigger.Data.PanelID].ID;

        TRIGGER_PRESS, TRIGGER_ON,
        TRIGGER_OFF, TRIGGER_ONOFF:
          if CopyBuffer[a].Trigger.Data.MonsterID <> 0 then
            gTriggers[CopyBuffer[a].ID].Data.MonsterID :=
              CopyBuffer[CopyBuffer[a].Trigger.Data.MonsterID-1].ID+1;

        TRIGGER_SHOT:
          if CopyBuffer[a].Trigger.Data.ShotPanelID <> -1 then
            gTriggers[CopyBuffer[a].ID].Data.ShotPanelID :=
              CopyBuffer[CopyBuffer[a].Trigger.Data.ShotPanelID].ID;
      end;

      if CopyBuffer[a].Trigger.TexturePanel <> -1 then
        gTriggers[CopyBuffer[a].ID].TexturePanel :=
          CopyBuffer[CopyBuffer[a].Trigger.TexturePanel].ID;
    end;

  CopyBuffer := nil;

  if h = 0 then
    FillProperty();
end;

procedure TMainForm.aCutObjectExecute(Sender: TObject);
begin
  miCopy.Click();
  DeleteSelectedObjects();
end;

procedure TMainForm.vleObjectPropertyEditButtonClick(Sender: TObject);
var
  Key, FileName: String;
  b: Byte;
begin
  Key := vleObjectProperty.Keys[vleObjectProperty.Row];

  if Key = MsgPropPanelType then
    begin
      with ChooseTypeForm, vleObjectProperty do
      begin // Выбор типа панели:
        Caption := MsgPropPanelType;
        lbTypeSelect.Items.Clear();

        for b := 0 to High(PANELNAMES) do
        begin
          lbTypeSelect.Items.Add(PANELNAMES[b]);
          if Values[Key] = PANELNAMES[b] then
            lbTypeSelect.ItemIndex := b;
        end;

        if ShowModal() = mrOK then
        begin
          b := lbTypeSelect.ItemIndex;
          Values[Key] := PANELNAMES[b];
          vleObjectPropertyApply(Sender);
        end;
      end
    end
  else if Key = MsgPropTrTeleportTo then
    SelectFlag := SELECTFLAG_TELEPORT
  else if Key = MsgPropTrSpawnTo then
    SelectFlag := SELECTFLAG_SPAWNPOINT
  else if (Key = MsgPropTrDoorPanel) or
          (Key = MsgPropTrTrapPanel) then
    SelectFlag := SELECTFLAG_DOOR
  else if Key = MsgPropTrTexturePanel then
  begin
    DrawPressRect := False;
    SelectFlag := SELECTFLAG_TEXTURE;
  end
  else if Key = MsgPropTrShotPanel then
    SelectFlag := SELECTFLAG_SHOTPANEL
  else if Key = MsgPropTrLiftPanel then
    SelectFlag := SELECTFLAG_LIFT
  else if key = MsgPropTrExMonster then
    SelectFlag := SELECTFLAG_MONSTER
  else if Key = MsgPropTrExArea then
  begin
    SelectFlag := SELECTFLAG_NONE;
    DrawPressRect := True;
  end
  else if Key = MsgPropTrNextMap then
    begin // Выбор следующей карты:
      g_ProcessResourceStr(OpenedMap, @FileName, nil, nil);
      SelectMapForm.Caption := MsgCapSelect;
      SelectMapForm.GetMaps(FileName);

      if SelectMapForm.ShowModal() = mrOK then
      begin
        vleObjectProperty.Values[Key] := SelectMapForm.lbMapList.Items[SelectMapForm.lbMapList.ItemIndex];
        vleObjectPropertyApply(Sender);
      end;
    end
  else if (Key = MsgPropTrSoundName) or
          (Key = MsgPropTrMusicName) then
    begin // Выбор файла звука/музыки:
      AddSoundForm.OKFunction := nil;
      AddSoundForm.lbResourcesList.MultiSelect := False;
      AddSoundForm.SetResource := vleObjectProperty.Values[Key];

      if (AddSoundForm.ShowModal() = mrOk) then
      begin
        vleObjectProperty.Values[Key] := AddSoundForm.ResourceName;
        vleObjectPropertyApply(Sender);
      end;
    end
  else if Key = MsgPropTrActivation then
    with ActivationTypeForm, vleObjectProperty do
    begin // Выбор типов активации:
      cbPlayerCollide.Checked := Pos('PC', Values[Key]) > 0;
      cbMonsterCollide.Checked := Pos('MC', Values[Key]) > 0;
      cbPlayerPress.Checked := Pos('PP', Values[Key]) > 0;
      cbMonsterPress.Checked := Pos('MP', Values[Key]) > 0;
      cbShot.Checked := Pos('SH', Values[Key]) > 0;
      cbNoMonster.Checked := Pos('NM', Values[Key]) > 0;

      if ShowModal() = mrOK then
      begin
        b := 0;
        if cbPlayerCollide.Checked then
          b := ACTIVATE_PLAYERCOLLIDE;
        if cbMonsterCollide.Checked then
          b := b or ACTIVATE_MONSTERCOLLIDE;
        if cbPlayerPress.Checked then
          b := b or ACTIVATE_PLAYERPRESS;
        if cbMonsterPress.Checked then
          b := b or ACTIVATE_MONSTERPRESS;
        if cbShot.Checked then
          b := b or ACTIVATE_SHOT;
        if cbNoMonster.Checked then
          b := b or ACTIVATE_NOMONSTER;

        Values[Key] := ActivateToStr(b);
        vleObjectPropertyApply(Sender);
      end;
    end
  else if Key = MsgPropTrKeys then
    with KeysForm, vleObjectProperty do
    begin // Выбор необходимых ключей:
      cbRedKey.Checked := Pos('RK', Values[Key]) > 0;
      cbGreenKey.Checked := Pos('GK', Values[Key]) > 0;
      cbBlueKey.Checked := Pos('BK', Values[Key]) > 0;
      cbRedTeam.Checked := Pos('RT', Values[Key]) > 0;
      cbBlueTeam.Checked := Pos('BT', Values[Key]) > 0;
      
      if ShowModal() = mrOK then
      begin
        b := 0;
        if cbRedKey.Checked then
          b := KEY_RED;
        if cbGreenKey.Checked then
          b := b or KEY_GREEN;
        if cbBlueKey.Checked then
          b := b or KEY_BLUE;
        if cbRedTeam.Checked then
          b := b or KEY_REDTEAM;
        if cbBlueTeam.Checked then
          b := b or KEY_BLUETEAM;

        Values[Key] := KeyToStr(b);
        vleObjectPropertyApply(Sender);
      end;
    end
  else if Key = MsgPropTrFxType then
    with ChooseTypeForm, vleObjectProperty do
    begin // Выбор типа эффекта:
      Caption := MsgCapFxType;
      lbTypeSelect.Items.Clear();

      for b := EFFECT_NONE to EFFECT_FIRE do
        lbTypeSelect.Items.Add(EffectToStr(b));

      lbTypeSelect.ItemIndex := StrToEffect(Values[Key]);

      if ShowModal() = mrOK then
      begin
        b := lbTypeSelect.ItemIndex;
        Values[Key] := EffectToStr(b);
        vleObjectPropertyApply(Sender);
      end;
    end
  else if Key = MsgPropTrMonsterType then
    with ChooseTypeForm, vleObjectProperty do
    begin // Выбор типа монстра:
      Caption := MsgCapMonsterType;
      lbTypeSelect.Items.Clear();

      for b := MONSTER_DEMON to MONSTER_MAN do
        lbTypeSelect.Items.Add(MonsterToStr(b));

      lbTypeSelect.ItemIndex := StrToMonster(Values[Key]) - MONSTER_DEMON;

      if ShowModal() = mrOK then
      begin
        b := lbTypeSelect.ItemIndex + MONSTER_DEMON;
        Values[Key] := MonsterToStr(b);
        vleObjectPropertyApply(Sender);
      end;
    end
  else if Key = MsgPropTrItemType then
    with ChooseTypeForm, vleObjectProperty do
    begin // Выбор типа предмета:
      Caption := MsgCapItemType;
      lbTypeSelect.Items.Clear();

      for b := ITEM_MEDKIT_SMALL to ITEM_KEY_BLUE do
        lbTypeSelect.Items.Add(ItemToStr(b));
      lbTypeSelect.Items.Add(ItemToStr(ITEM_BOTTLE));
      lbTypeSelect.Items.Add(ItemToStr(ITEM_HELMET));
      lbTypeSelect.Items.Add(ItemToStr(ITEM_JETPACK));
      lbTypeSelect.Items.Add(ItemToStr(ITEM_INVIS));
      lbTypeSelect.Items.Add(ItemToStr(ITEM_WEAPON_FLAMETHROWER));
      lbTypeSelect.Items.Add(ItemToStr(ITEM_AMMO_FUELCAN));

      b := StrToItem(Values[Key]);
      if b >= ITEM_BOTTLE then
        b := b - 2;
      lbTypeSelect.ItemIndex := b - ITEM_MEDKIT_SMALL;

      if ShowModal() = mrOK then
      begin
        b := lbTypeSelect.ItemIndex + ITEM_MEDKIT_SMALL;
        if b >= ITEM_WEAPON_KASTET then
          b := b + 2;
        Values[Key] := ItemToStr(b);
        vleObjectPropertyApply(Sender);
      end;
    end
  else if Key = MsgPropTrShotType then
    with ChooseTypeForm, vleObjectProperty do
    begin // Выбор типа предмета:
      Caption := MsgPropTrShotType;
      lbTypeSelect.Items.Clear();

      for b := TRIGGER_SHOT_PISTOL to TRIGGER_SHOT_MAX do
        lbTypeSelect.Items.Add(ShotToStr(b));

      lbTypeSelect.ItemIndex := StrToShot(Values[Key]);

      if ShowModal() = mrOK then
      begin
        b := lbTypeSelect.ItemIndex;
        Values[Key] := ShotToStr(b);
        vleObjectPropertyApply(Sender);
      end;
    end
  else if Key = MsgPropTrEffectType then
    with ChooseTypeForm, vleObjectProperty do
    begin // Выбор типа эффекта:
      Caption := MsgCapFxType;
      lbTypeSelect.Items.Clear();

      lbTypeSelect.Items.Add(MsgPropTrEffectParticle);
      lbTypeSelect.Items.Add(MsgPropTrEffectAnimation);
      if Values[Key] = MsgPropTrEffectAnimation then
        lbTypeSelect.ItemIndex := 1
      else
        lbTypeSelect.ItemIndex := 0;

      if ShowModal() = mrOK then
      begin
        b := lbTypeSelect.ItemIndex;
        if b = 0 then
          Values[Key] := MsgPropTrEffectParticle
        else
          Values[Key] := MsgPropTrEffectAnimation;
        vleObjectPropertyApply(Sender);
      end;
    end
  else if Key = MsgPropTrEffectSubtype then
    with ChooseTypeForm, vleObjectProperty do
    begin // Выбор подтипа эффекта:
      Caption := MsgCapFxType;
      lbTypeSelect.Items.Clear();

      if Values[MsgPropTrEffectType] = MsgPropTrEffectAnimation then
      begin
        for b := EFFECT_TELEPORT to EFFECT_FIRE do
          lbTypeSelect.Items.Add(EffectToStr(b));

        lbTypeSelect.ItemIndex := StrToEffect(Values[Key]) - 1;
      end else
      begin
        lbTypeSelect.Items.Add(MsgPropTrEffectSliquid);
        lbTypeSelect.Items.Add(MsgPropTrEffectLliquid);
        lbTypeSelect.Items.Add(MsgPropTrEffectDliquid);
        lbTypeSelect.Items.Add(MsgPropTrEffectBlood);
        lbTypeSelect.Items.Add(MsgPropTrEffectSpark);
        lbTypeSelect.Items.Add(MsgPropTrEffectBubble);
        lbTypeSelect.ItemIndex := TRIGGER_EFFECT_SLIQUID;
        if Values[Key] = MsgPropTrEffectLliquid then
          lbTypeSelect.ItemIndex := TRIGGER_EFFECT_LLIQUID;
        if Values[Key] = MsgPropTrEffectDliquid then
          lbTypeSelect.ItemIndex := TRIGGER_EFFECT_DLIQUID;
        if Values[Key] = MsgPropTrEffectBlood then
          lbTypeSelect.ItemIndex := TRIGGER_EFFECT_BLOOD;
        if Values[Key] = MsgPropTrEffectSpark then
          lbTypeSelect.ItemIndex := TRIGGER_EFFECT_SPARK;
        if Values[Key] = MsgPropTrEffectBubble then
          lbTypeSelect.ItemIndex := TRIGGER_EFFECT_BUBBLE;
      end;

      if ShowModal() = mrOK then
      begin
        b := lbTypeSelect.ItemIndex;

        if Values[MsgPropTrEffectType] = MsgPropTrEffectAnimation then
          Values[Key] := EffectToStr(b + 1)
        else begin
          Values[Key] := MsgPropTrEffectSliquid;
          if b = TRIGGER_EFFECT_LLIQUID then
            Values[Key] := MsgPropTrEffectLliquid;
          if b = TRIGGER_EFFECT_DLIQUID then
            Values[Key] := MsgPropTrEffectDliquid;
          if b = TRIGGER_EFFECT_BLOOD then
            Values[Key] := MsgPropTrEffectBlood;
          if b = TRIGGER_EFFECT_SPARK then
            Values[Key] := MsgPropTrEffectSpark;
          if b = TRIGGER_EFFECT_BUBBLE then
            Values[Key] := MsgPropTrEffectBubble;
        end;

        vleObjectPropertyApply(Sender);
      end;
    end
  else if Key = MsgPropTrEffectColor then
    with vleObjectProperty do
    begin // Выбор цвета эффекта:
      ColorDialog.Color := StrToIntDef(Values[Key], 0);
      if ColorDialog.Execute then
      begin
        Values[Key] := IntToStr(ColorDialog.Color);
        vleObjectPropertyApply(Sender);
      end;
    end
  else if Key = MsgPropPanelTex then
    begin // Смена текстуры:
      vleObjectProperty.Values[Key] := SelectedTexture();
      vleObjectPropertyApply(Sender);
    end;
end;

procedure TMainForm.vleObjectPropertyApply(Sender: TObject);
begin
  // hack to prevent empty ID in list
  RenderPanel.SetFocus();
  bApplyProperty.Click();
  vleObjectProperty.SetFocus();
end;

procedure TMainForm.aSaveMapExecute(Sender: TObject);
var
  FileName, Section, Res: String;
begin
  if OpenedMap = '' then
  begin
    aSaveMapAsExecute(nil);
    Exit;
  end;

  g_ProcessResourceStr(OpenedMap, FileName, Section, Res);

  SaveMap(FileName+':\'+Res);
end;

procedure TMainForm.aOpenMapExecute(Sender: TObject);
begin
  OpenDialog.Filter := MsgFileFilterAll;

  if OpenDialog.Execute() then
  begin
    OpenMapFile(OpenDialog.FileName);
    OpenDialog.InitialDir := ExtractFileDir(OpenDialog.FileName);
  end;
end;

procedure TMainForm.OpenMapFile(FileName: String);
begin
  if (Pos('.ini', LowerCase(ExtractFileName(FileName))) > 0) then
    begin // INI карты:
      FullClear();

      pLoadProgress.Left := (RenderPanel.Width div 2)-(pLoadProgress.Width div 2);
      pLoadProgress.Top := (RenderPanel.Height div 2)-(pLoadProgress.Height div 2);
      pLoadProgress.Show();

      OpenedMap := '';
      OpenedWAD := '';

      LoadMapOld(FileName);

      MainForm.Caption := Format('%s - %s', [FormCaption, ExtractFileName(FileName)]);

      pLoadProgress.Hide();
      MainForm.FormResize(Self);
    end
  else // Карты из WAD:
    begin
      OpenMap(FileName, '');
    end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  MainForm.ActiveControl := RenderPanel;
end;

procedure TMainForm.aDeleteMap(Sender: TObject);
var
  res: Integer;
  FileName: String;
  MapName: String;
begin
  OpenDialog.Filter := MsgFileFilterWad;

  if not OpenDialog.Execute() then
    Exit;

  FileName := OpenDialog.FileName;
  SelectMapForm.Caption := MsgCapRemove;
  SelectMapForm.lbMapList.Items.Clear();
  SelectMapForm.GetMaps(FileName);

  if SelectMapForm.ShowModal() <> mrOK then
    Exit;

  MapName := SelectMapForm.lbMapList.Items[SelectMapForm.lbMapList.ItemIndex];
  if Application.MessageBox(PChar(Format(MsgMsgDeleteMapPromt, [MapName, OpenDialog.FileName])), PChar(MsgMsgDeleteMap), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON2) <> mrYes then
    Exit;

  g_DeleteResource(FileName, '', MapName, res);
  if res <> 0 then
  begin
    Application.MessageBox(PChar('Cant delete map res=' + IntToStr(res)), PChar('Map not deleted!'), MB_ICONINFORMATION or MB_OK or MB_DEFBUTTON1);
    Exit
  end;

  Application.MessageBox(
    PChar(Format(MsgMsgMapDeletedPromt, [MapName])),
    PChar(MsgMsgMapDeleted),
    MB_ICONINFORMATION or MB_OK or MB_DEFBUTTON1
  );

  // Удалили текущую карту - сохранять по старому ее нельзя:
  if OpenedMap = (FileName + ':\' + MapName) then
  begin
    OpenedMap := '';
    OpenedWAD := '';
    MainForm.Caption := FormCaption
  end
end;

procedure TMainForm.vleObjectPropertyKeyDown(Sender: TObject;
            var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    vleObjectPropertyApply(Sender);
end;

procedure MovePanel(var ID: DWORD; MoveType: Byte);
var
  _id, a: Integer;
  tmp: TPanel;
begin
  if (ID = 0) and (MoveType = 0) then
    Exit;
  if (ID = DWORD(High(gPanels))) and (MoveType <> 0) then
    Exit;
  if (ID > DWORD(High(gPanels))) then
    Exit;

  _id := Integer(ID);

  if MoveType = 0 then // to Back
    begin
      if gTriggers <> nil then
        for a := 0 to High(gTriggers) do
          with gTriggers[a] do
          begin
            if TriggerType = TRIGGER_NONE then
              Continue;

            if TexturePanel = _id then
              TexturePanel := 0
            else
              if (TexturePanel >= 0) and (TexturePanel < _id) then
                Inc(TexturePanel);

            case TriggerType of
              TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR,
              TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP,
              TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT:
                if Data.PanelID = _id then
                  Data.PanelID := 0
                else
                  if (Data.PanelID >= 0) and (Data.PanelID < _id) then
                    Inc(Data.PanelID);

              TRIGGER_SHOT:
                if Data.ShotPanelID = _id then
                  Data.ShotPanelID := 0
                else
                  if (Data.ShotPanelID >= 0) and (Data.ShotPanelID < _id) then
                    Inc(Data.ShotPanelID);
            end;
          end;

      tmp := gPanels[_id];

      for a := _id downto 1 do
        gPanels[a] := gPanels[a-1];

      gPanels[0] := tmp;

      ID := 0;
    end
  else // to Front
    begin
      if gTriggers <> nil then
        for a := 0 to High(gTriggers) do
          with gTriggers[a] do
          begin
            if TriggerType = TRIGGER_NONE then
              Continue;

            if TexturePanel = _id then
              TexturePanel := High(gPanels)
            else
              if TexturePanel > _id then
                Dec(TexturePanel);

            case TriggerType of
              TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR,
              TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP,
              TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT:
                if Data.PanelID = _id then
                  Data.PanelID := High(gPanels)
                else
                  if Data.PanelID > _id then
                    Dec(Data.PanelID);

              TRIGGER_SHOT:
                if Data.ShotPanelID = _id then
                  Data.ShotPanelID := High(gPanels)
                else
                  if Data.ShotPanelID > _id then
                    Dec(Data.ShotPanelID);
            end;
          end;

      tmp := gPanels[_id];

      for a := _id to High(gPanels)-1 do
        gPanels[a] := gPanels[a+1];

      gPanels[High(gPanels)] := tmp;

      ID := High(gPanels);
    end;
end;

procedure TMainForm.aMoveToBack(Sender: TObject);
var
  a: Integer;
begin
  if SelectedObjects = nil then
    Exit;

  for a := 0 to High(SelectedObjects) do
    with SelectedObjects[a] do
      if Live and (ObjectType = OBJECT_PANEL) then
      begin
        SelectedObjects[0] := SelectedObjects[a];
        SetLength(SelectedObjects, 1);
        MovePanel(ID, 0);
        FillProperty();
        Break;
      end;
end;

procedure TMainForm.aMoveToFore(Sender: TObject);
var
  a: Integer;
begin
  if SelectedObjects = nil then
    Exit;

  for a := 0 to High(SelectedObjects) do
    with SelectedObjects[a] do
      if Live and (ObjectType = OBJECT_PANEL) then
      begin
        SelectedObjects[0] := SelectedObjects[a];
        SetLength(SelectedObjects, 1);
        MovePanel(ID, 1);
        FillProperty();
        Break;
      end;
end;

procedure TMainForm.aSaveMapAsExecute(Sender: TObject);
var
  idx: Integer;
begin
  SaveDialog.Filter := MsgFileFilterWad;

  if not SaveDialog.Execute() then
    Exit;

  SaveMapForm.GetMaps(SaveDialog.FileName, True);

  if SaveMapForm.ShowModal() <> mrOK then
    Exit;

  SaveDialog.InitialDir := ExtractFileDir(SaveDialog.FileName);
  OpenedMap := SaveDialog.FileName+':\'+SaveMapForm.eMapName.Text;
  OpenedWAD := SaveDialog.FileName;

  idx := RecentFiles.IndexOf(OpenedMap);
// Такая карта уже недавно открывалась:
  if idx >= 0 then
    RecentFiles.Delete(idx);
  RecentFiles.Insert(0, OpenedMap);
  RefreshRecentMenu;

  SaveMap(OpenedMap);

  gMapInfo.FileName := SaveDialog.FileName;
  gMapInfo.MapName := SaveMapForm.eMapName.Text;
  UpdateCaption(gMapInfo.Name, ExtractFileName(gMapInfo.FileName), gMapInfo.MapName);
end;

procedure TMainForm.aSelectAllExecute(Sender: TObject);
var
  a: Integer;
begin
  RemoveSelectFromObjects();

  case pcObjects.ActivePageIndex+1 of
    OBJECT_PANEL:
      if gPanels <> nil then
        for a := 0 to High(gPanels) do
          if gPanels[a].PanelType <> PANEL_NONE then
            SelectObject(OBJECT_PANEL, a, True);
    OBJECT_ITEM:
      if gItems <> nil then
        for a := 0 to High(gItems) do
          if gItems[a].ItemType <> ITEM_NONE then
            SelectObject(OBJECT_ITEM, a, True);
    OBJECT_MONSTER:
      if gMonsters <> nil then
        for a := 0 to High(gMonsters) do
          if gMonsters[a].MonsterType <> MONSTER_NONE then
            SelectObject(OBJECT_MONSTER, a, True);
    OBJECT_AREA:
      if gAreas <> nil then
        for a := 0 to High(gAreas) do
          if gAreas[a].AreaType <> AREA_NONE then
            SelectObject(OBJECT_AREA, a, True);
    OBJECT_TRIGGER:
      if gTriggers <> nil then
        for a := 0 to High(gTriggers) do
          if gTriggers[a].TriggerType <> TRIGGER_NONE then
            SelectObject(OBJECT_TRIGGER, a, True);
  end;

  RecountSelectedObjects();
end;

procedure TMainForm.tbGridOnClick(Sender: TObject);
begin
  DotEnable := not DotEnable;
  (Sender as TToolButton).Down := DotEnable;
end;

procedure TMainForm.OnIdle(Sender: TObject; var Done: Boolean);
  var f: AnsiString;
begin
  // FIXME: this is a shitty hack
  if not gDataLoaded then
  begin
    e_WriteLog('Init OpenGL', MSG_NOTIFY);
    e_InitGL();
    e_WriteLog('Loading data', MSG_NOTIFY);
    LoadStdFont('STDTXT', 'STDFONT', gEditorFont);
    e_WriteLog('Loading more data', MSG_NOTIFY);
    LoadData();
    e_WriteLog('Loading even more data', MSG_NOTIFY);
    gDataLoaded := True;
    MainForm.FormResize(nil);
  end;
  Draw();
  if StartMap <> '' then
  begin
    f := StartMap;
    StartMap := '';
    OpenMap(f, '');
  end;
end;

procedure TMainForm.miMapPreviewClick(Sender: TObject);
begin
  if PreviewMode = 2 then
    Exit;

  if PreviewMode = 0 then
    begin
      Splitter2.Visible := False;
      Splitter1.Visible := False;
      StatusBar.Visible := False;
      PanelObjs.Visible := False;
      PanelProps.Visible := False;
      MainToolBar.Visible := False;
      sbHorizontal.Visible := False;
      sbVertical.Visible := False;
    end
  else
    begin
      StatusBar.Visible := True;
      PanelObjs.Visible := True;
      PanelProps.Visible := True;
      Splitter2.Visible := True;
      Splitter1.Visible := True;
      MainToolBar.Visible := True;
      sbHorizontal.Visible := True;
      sbVertical.Visible := True;
    end;

  PreviewMode := PreviewMode xor 1;
  (Sender as TMenuItem).Checked := PreviewMode > 0;

  FormResize(Self);
end;

procedure TMainForm.miLayer1Click(Sender: TObject);
begin
  SwitchLayer(LAYER_BACK);
end;

procedure TMainForm.miLayer2Click(Sender: TObject);
begin
  SwitchLayer(LAYER_WALLS);
end;

procedure TMainForm.miLayer3Click(Sender: TObject);
begin
  SwitchLayer(LAYER_FOREGROUND);
end;

procedure TMainForm.miLayer4Click(Sender: TObject);
begin
  SwitchLayer(LAYER_STEPS);
end;

procedure TMainForm.miLayer5Click(Sender: TObject);
begin
  SwitchLayer(LAYER_WATER);
end;

procedure TMainForm.miLayer6Click(Sender: TObject);
begin
  SwitchLayer(LAYER_ITEMS);
end;

procedure TMainForm.miLayer7Click(Sender: TObject);
begin
  SwitchLayer(LAYER_MONSTERS);
end;

procedure TMainForm.miLayer8Click(Sender: TObject);
begin
  SwitchLayer(LAYER_AREAS);
end;

procedure TMainForm.miLayer9Click(Sender: TObject);
begin
  SwitchLayer(LAYER_TRIGGERS);
end;

procedure TMainForm.tbShowClick(Sender: TObject);
var
  a: Integer;
  b: Boolean;
begin
  b := True;
  for a := 0 to High(LayerEnabled) do
    b := b and LayerEnabled[a];

  b := not b;

  ShowLayer(LAYER_BACK, b);
  ShowLayer(LAYER_WALLS, b);
  ShowLayer(LAYER_FOREGROUND, b);
  ShowLayer(LAYER_STEPS, b);
  ShowLayer(LAYER_WATER, b);
  ShowLayer(LAYER_ITEMS, b);
  ShowLayer(LAYER_MONSTERS, b);
  ShowLayer(LAYER_AREAS, b);
  ShowLayer(LAYER_TRIGGERS, b);
end;

procedure TMainForm.miMiniMapClick(Sender: TObject);
begin
  SwitchMap();
end;

procedure TMainForm.miSwitchGridClick(Sender: TObject);
begin
  if DotStep = DotStepOne then
    DotStep := DotStepTwo
  else
    DotStep := DotStepOne;

  MousePos.X := (MousePos.X div DotStep) * DotStep;
  MousePos.Y := (MousePos.Y div DotStep) * DotStep;
end;

procedure TMainForm.miShowEdgesClick(Sender: TObject);
begin
  ShowEdges();
end;

procedure TMainForm.miSnapToGridClick(Sender: TObject);
begin
  SnapToGrid := not SnapToGrid;

  MousePos.X := (MousePos.X div DotStep) * DotStep;
  MousePos.Y := (MousePos.Y div DotStep) * DotStep;

  miSnapToGrid.Checked := SnapToGrid;
end;

procedure TMainForm.minexttabClick(Sender: TObject);
begin
  if pcObjects.ActivePageIndex < pcObjects.PageCount-1 then
    pcObjects.ActivePageIndex := pcObjects.ActivePageIndex+1
  else
    pcObjects.ActivePageIndex := 0;
end;

procedure TMainForm.miSaveMiniMapClick(Sender: TObject);
begin
  SaveMiniMapForm.ShowModal();
end;

procedure TMainForm.bClearTextureClick(Sender: TObject);
begin
  lbTextureList.ItemIndex := -1;
  lTextureWidth.Caption := '';
  lTextureHeight.Caption := '';
end;

procedure TMainForm.miPackMapClick(Sender: TObject);
begin
  PackMapForm.ShowModal();
end;

type SSArray = array of String;

function ParseString (Str: AnsiString): SSArray;
  function GetStr (var Str: AnsiString): AnsiString;
    var a, b: Integer;
  begin
    Result := '';
    if Str[1] = '"' then
      for b := 1 to Length(Str) do
        if (b = Length(Str)) or (Str[b + 1] = '"') then
        begin
          Result := Copy(Str, 2, b - 1);
          Delete(Str, 1, b + 1);
          Str := Trim(Str);
          Exit;
        end;
    for a := 1 to Length(Str) do
      if (a = Length(Str)) or (Str[a + 1] = ' ') then
      begin
        Result := Copy(Str, 1, a);
        Delete(Str, 1, a + 1);
        Str := Trim(Str);
        Exit;
      end;
  end;
begin
  Result := nil;
  Str := Trim(Str);
  while Str <> '' do
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := GetStr(Str);
  end;
end;

procedure TMainForm.miTestMapClick(Sender: TObject);
var
  newWAD, oldWAD, tempMap, ext: String;
  args: SSArray;
  opt: LongWord;
  time, i: Integer;
  proc: TProcessUTF8;
  res: Boolean;
begin
  // Ignore while map testing in progress
  if MapTestProcess <> nil then
    Exit;

  // Сохраняем временную карту:
  time := 0;
  repeat
    newWAD := Format('%s/temp%.4d', [MapsDir, time]);
    Inc(time);
  until not FileExists(newWAD);
  if OpenedMap <> '' then
  begin
    oldWad := g_ExtractWadName(OpenedMap);
    newWad := newWad + ExtractFileExt(oldWad);
    if CopyFile(oldWad, newWad) = false then
      e_WriteLog('MapTest: unable to copy [' + oldWad + '] to [' + newWad + ']', MSG_WARNING)
  end
  else
  begin
    newWad := newWad + '.wad'
  end;
  tempMap := newWAD + ':\' + TEST_MAP_NAME;
  SaveMap(tempMap);

// Опции игры:
  opt := 32 + 64;
  if TestOptionsTwoPlayers then
    opt := opt + 1;
  if TestOptionsTeamDamage then
    opt := opt + 2;
  if TestOptionsAllowExit then
    opt := opt + 4;
  if TestOptionsWeaponStay then
    opt := opt + 8;
  if TestOptionsMonstersDM then
    opt := opt + 16;

// Запускаем:
  proc := TProcessUTF8.Create(nil);
  proc.Executable := TestD2dExe;
  {$IFDEF DARWIN}
    // TODO: get real executable name from Info.plist
    if LowerCase(ExtractFileExt(TestD2dExe)) = '.app' then
      proc.Executable := TestD2dExe + DirectorySeparator + 'Contents' + DirectorySeparator + 'MacOS' + DirectorySeparator + 'Doom2DF';
  {$ENDIF}
  proc.Parameters.Add('-map');
  proc.Parameters.Add(tempMap);
  proc.Parameters.Add('-gm');
  proc.Parameters.Add(TestGameMode);
  proc.Parameters.Add('-limt');
  proc.Parameters.Add(TestLimTime);
  proc.Parameters.Add('-lims');
  proc.Parameters.Add(TestLimScore);
  proc.Parameters.Add('-opt');
  proc.Parameters.Add(IntToStr(opt));
  proc.Parameters.Add('--debug');
  if TestMapOnce then
    proc.Parameters.Add('--close');

  args := ParseString(TestD2DArgs);
  for i := 0 to High(args) do
    proc.Parameters.Add(args[i]);

  res := True;
  try
    proc.Execute();
  except
    res := False;
  end;
  if res then
  begin
    tbTestMap.Enabled := False;
    MapTestFile := newWAD;
    MapTestProcess := proc;
  end
  else
  begin
    Application.MessageBox(PChar(MsgMsgExecError), 'FIXME', MB_OK or MB_ICONERROR);
    SysUtils.DeleteFile(newWAD);
    proc.Free();
  end;
end;

procedure TMainForm.sbVerticalScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  MapOffset.Y := -sbVertical.Position;
  RenderPanel.Invalidate;
end;

procedure TMainForm.sbHorizontalScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  MapOffset.X := -sbHorizontal.Position;
  RenderPanel.Invalidate;
end;

procedure TMainForm.miOpenWadMapClick(Sender: TObject);
begin
  if OpenedWAD <> '' then
  begin
    OpenMap(OpenedWAD, '');
  end;
end;

procedure TMainForm.selectall1Click(Sender: TObject);
var
  a: Integer;
begin
  RemoveSelectFromObjects();

  if gPanels <> nil then
    for a := 0 to High(gPanels) do
      if gPanels[a].PanelType <> PANEL_NONE then
        SelectObject(OBJECT_PANEL, a, True);

  if gItems <> nil then
    for a := 0 to High(gItems) do
      if gItems[a].ItemType <> ITEM_NONE then
        SelectObject(OBJECT_ITEM, a, True);

  if gMonsters <> nil then
    for a := 0 to High(gMonsters) do
      if gMonsters[a].MonsterType <> MONSTER_NONE then
        SelectObject(OBJECT_MONSTER, a, True);

  if gAreas <> nil then
    for a := 0 to High(gAreas) do
      if gAreas[a].AreaType <> AREA_NONE then
        SelectObject(OBJECT_AREA, a, True);

  if gTriggers <> nil then
    for a := 0 to High(gTriggers) do
      if gTriggers[a].TriggerType <> TRIGGER_NONE then
        SelectObject(OBJECT_TRIGGER, a, True);

  RecountSelectedObjects();
end;

procedure TMainForm.Splitter1CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept := (NewSize > 140);
end;

procedure TMainForm.Splitter2CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept := (NewSize > 110);
end;

procedure TMainForm.vleObjectPropertyEnter(Sender: TObject);
begin
  EditingProperties := True;
end;

procedure TMainForm.vleObjectPropertyExit(Sender: TObject);
begin
  EditingProperties := False;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
// Объекты передвигались:
  if MainForm.ActiveControl = RenderPanel then
  begin
    if (Key = VK_NUMPAD4) or
       (Key = VK_NUMPAD6) or
       (Key = VK_NUMPAD8) or
       (Key = VK_NUMPAD5) or
       (Key = Ord('V')) then
      FillProperty();
  end;
// Быстрое превью карты:
  if Key = Ord('E') then
  begin
    if PreviewMode = 2 then
      PreviewMode := 0;
  end;
  RenderPanelMouseMove(Sender, Shift, RenderMousePos().X, RenderMousePos().Y);
end;

end.
