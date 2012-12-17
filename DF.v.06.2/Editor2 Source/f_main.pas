unit f_main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, Buttons, ComCtrls, ValEdit, Types, ToolWin,
  Menus, ExtCtrls, CheckLst, Grids;

type
  TMainForm = class(TForm)
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Panel1: TPanel;
    vleObjectProperty: TValueListEditor;
    Panel6: TPanel;
    bApplyProperty: TButton;
    Panel2: TPanel;
    RenderPanel: TPanel;
    StatusBar: TStatusBar;
    Panel3: TPanel;
    pcObjects: TPageControl;
    tsPanels: TTabSheet;
    lbTextureList: TListBox;
    Panel4: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    lTextureHeight: TLabel;
    lTextureWidth: TLabel;
    cbPreview: TCheckBox;
    Panel5: TPanel;
    GroupBox1: TGroupBox;
    cbPanelType: TComboBox;
    tsItems: TTabSheet;
    lbItemList: TListBox;
    cbOnlyDM: TCheckBox;
    tsMonsters: TTabSheet;
    lbMonsterList: TListBox;
    rbMonsterLeft: TRadioButton;
    rbMonsterRight: TRadioButton;
    tsAreas: TTabSheet;
    lbAreasList: TListBox;
    rbAreaLeft: TRadioButton;
    rbAreaRight: TRadioButton;
    tsTriggers: TTabSheet;
    lbTriggersList: TListBox;
    OpenDialog: TOpenDialog;
    ImageList: TImageList;
    SaveDialog: TSaveDialog;
    pLoadProgress: TPanel;
    pbLoad: TProgressBar;
    lLoad: TLabel;
    MainMenu: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    miNewMap: TMenuItem;
    miOpenINI: TMenuItem;
    miOpenMap: TMenuItem;
    N6: TMenuItem;
    miExit: TMenuItem;
    miSaveMap: TMenuItem;
    miUndo: TMenuItem;
    miCopy: TMenuItem;
    miCut: TMenuItem;
    miPaste: TMenuItem;
    N11: TMenuItem;
    miCheckMap: TMenuItem;
    miOptimmization: TMenuItem;
    miMapOptions: TMenuItem;
    N8: TMenuItem;
    miOptions: TMenuItem;
    miAbout: TMenuItem;
    ToolBar1: TToolBar;
    tbNewMap: TToolButton;
    tbOpenMap: TToolButton;
    tbSaveMap: TToolButton;
    ToolButton4: TToolButton;
    tbShowMap: TToolButton;
    ToolButton8: TToolButton;
    tbShow: TToolButton;
    ToolButton7: TToolButton;
    tbGrid: TToolButton;
    pmShow: TPopupMenu;
    miLayerP1: TMenuItem;
    miLayerP2: TMenuItem;
    miLayerP3: TMenuItem;
    miLayerP4: TMenuItem;
    miLayerP5: TMenuItem;
    sbHorizontal: TScrollBar;
    sbVertical: TScrollBar;
    miLayerP6: TMenuItem;
    miLayerP8: TMenuItem;
    miLayerP9: TMenuItem;
    miDeleteMap: TMenuItem;
    N13: TMenuItem;
    miToFore: TMenuItem;
    miToBack: TMenuItem;
    miSaveMapAs: TMenuItem;
    N14: TMenuItem;
    miSelectAll: TMenuItem;
    clbActivationType: TCheckListBox;
    clbKeys: TCheckListBox;
    tbGridOn: TToolButton;
    miMapPreview: TMenuItem;
    ilToolbar: TImageList;
    N9: TMenuItem;
    miMiniMap: TMenuItem;
    miLayers: TMenuItem;
    miLayer1: TMenuItem;
    miLayer2: TMenuItem;
    miLayer3: TMenuItem;
    miLayer4: TMenuItem;
    miLayer5: TMenuItem;
    miLayer6: TMenuItem;
    miLayer8: TMenuItem;
    miLayer9: TMenuItem;
    miSwitchGrid: TMenuItem;
    miSnapToGrid: TMenuItem;
    hidden1: TMenuItem;
    minexttab: TMenuItem;
    N7: TMenuItem;
    miSaveMiniMap: TMenuItem;
    miLayer7: TMenuItem;
    miLayerP7: TMenuItem;
    bbAddTexture: TBitBtn;
    bbRemoveTexture: TBitBtn;
    bClearTexture: TButton;
    cbFall: TCheckBox;
    miPackMap: TMenuItem;
    N10: TMenuItem;
    miMapTestSettings: TMenuItem;
    miTestMap: TMenuItem;
    ToolButton2: TToolButton;
    tbTestMap: TToolButton;
    pmMapTest: TPopupMenu;
    miMapTestPMSet: TMenuItem;
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
    procedure aOpenINIExecute(Sender: TObject);
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
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure lbTextureListClick(Sender: TObject);
    procedure RenderPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RenderPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure RenderPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RenderPanelResize(Sender: TObject);
    procedure sbHorizontalChange(Sender: TObject);
    procedure sbVerticalChange(Sender: TObject);
    procedure vleObjectPropertyEditButtonClick(Sender: TObject);
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
    procedure miMiniMapClick(Sender: TObject);
    procedure miSwitchGridClick(Sender: TObject);
    procedure miSnapToGridClick(Sender: TObject);
    procedure minexttabClick(Sender: TObject);
    procedure miSaveMiniMapClick(Sender: TObject);
    procedure bClearTextureClick(Sender: TObject);
    procedure miPackMapClick(Sender: TObject);
    procedure aRecentFileExecute(Sender: TObject);
    procedure miMapTestSettingsClick(Sender: TObject);
    procedure miTestMapClick(Sender: TObject);
   private
    procedure Draw();
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure RefreshRecentMenu;
   public
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

var
  MainForm: TMainForm;
  EditorDir: string;
  OpenedMap: string;

  DotColor: TColor;
  DotEnable: Boolean;
  DotStep: Byte;
  DrawTexturePanel: Boolean;
  DrawPanelSize: Boolean;
  BackColor: TColor;
  PreviewColor: TColor;
  Scale: Byte;

  RecentFiles: TStringList;

  TestGameMode: String;
  TestLimTime: String;
  TestLimGoal: String;
  TestOptionsTwoPlayers: Boolean;
  TestOptionsTeamDamage: Boolean;
  TestOptionsAllowExit: Boolean;
  TestOptionsWeaponStay: Boolean;
  TestOptionsMonstersDM: Boolean;
  TestD2dExe: String;
  TestMapOnce: Boolean;

  LayerEnabled: array[LAYER_BACK..LAYER_TRIGGERS] of Boolean =
  (True, True, True, True, True, True, True, True, True);
  PreviewMode: Boolean = False;

procedure OpenMap(FileName: string; mapN: string);
function  AddTexture(aWAD, aSection, aTex: String): Boolean;

implementation

uses f_options, e_graphics, e_log, dglOpenGL, Math, f_mapoptions,
  g_basic, f_about, f_mapoptimization, f_mapcheck, f_addresource_texture,
  g_textures, f_activationtype, f_keys, MAPWRITER, MAPSTRUCT, MAPREADER,
  f_selectmap, f_savemap, WADEDITOR, MAPDEF, g_map, f_saveminimap,
  f_addresource, CONFIG, f_packmap, f_addresource_sound, f_maptest,
  f_choosetype;

const
  BoolNames: array[False..True] of string = ('Нет', 'Да');
  DirNames: array[D_LEFT..D_RIGHT] of string = ('Влево', 'Вправо');
  DirNamesAdv: array [0..2] of string = ('Не менять', 'Влево', 'Вправо');

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

  MAX_RECENT_FILES    = 10;
  RECENT_FILES_MENU_START = 12;

type
  TUndoRec = record
   UndoType: Byte;
   case Byte of
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
   ObjectType: Byte;
   case Byte of
    OBJECT_PANEL: (Panel: ^TPanel);
    OBJECT_ITEM: (Item: TItem);                               
    OBJECT_AREA: (Area: TArea);
    OBJECT_MONSTER: (Monster: TMonster);
    OBJECT_TRIGGER: (Trigger: TTrigger);
  end;

var
  FormCaption: string;

  hDC: THandle;
  hRC: THandle;
  gEditorFont: DWORD;
  ShowMap: Boolean = False;
  DrawRect: PRect = nil;
  _DotEnabled: Boolean;
  SnapToGrid: Boolean = True;
  
  MousePos: Types.TPoint;
  LastMovePoint: Types.TPoint;
  MouseLDown: Boolean;
  MouseRDown: Boolean;
  MouseLDownPos: Types.TPoint;
  MouseRDownPos: Types.TPoint;

  SelectFlag: Byte = SELECTFLAG_NONE;
  MouseAction: Byte = MOUSEACTION_NONE;
  ResizeType: Byte = RESIZETYPE_NONE;
  ResizeDirection: Byte = RESIZEDIR_NONE;

  DrawPressRect: Boolean = False;

  UndoBuffer: array of array of TUndoRec = nil;
  CopyBuffer: array of TCopyRec = nil;

{$R *.dfm}

//----------------------------------------
//Далее идут вспомогательные процедуры
//----------------------------------------
function NameToBool(Name: string): Boolean;
begin
 if Name = BoolNames[True] then Result := True else Result := False;
end;

function NameToDir(Name: string): TDirection;
begin
 if Name = DirNames[D_LEFT] then Result := D_LEFT else Result := D_RIGHT;
end;

function NameToDirAdv(Name: string): Byte;
begin
 if Name = DirNamesAdv[1] then
   Result := 1
 else
   if Name = DirNamesAdv[2] then
     Result := 2
 else
   Result := 0;
end;

function ActivateToStr(ActivateType: Cardinal): string;
begin
 Result := '';

 if LongBool(ACTIVATE_PLAYERCOLLIDE and ActivateType) then Result := Result+'+PC';
 if LongBool(ACTIVATE_MONSTERCOLLIDE and ActivateType) then Result := Result+'+MC';
 if LongBool(ACTIVATE_PLAYERPRESS and ActivateType) then Result := Result+'+PP';
 if LongBool(ACTIVATE_MONSTERPRESS and ActivateType) then Result := Result+'+MP';
 if LongBool(ACTIVATE_SHOT and ActivateType) then Result := Result+'+SH';
 if LongBool(ACTIVATE_NOMONSTER and ActivateType) then Result := Result+'+NM';
 if LongBool(ACTIVATE_NOPLAYER and ActivateType) then Result := Result+'+NP';
 if LongBool(ACTIVATE_ITEMCOLLIDE and ActivateType) then Result := Result+'+IC';
 if LongBool(ACTIVATE_NOITEM and ActivateType) then Result := Result+'+NI';

 if (Result <> '') and (Result[1] = '+') then Delete(Result, 1, 1);
end;

function StrToActivate(Str: string): Cardinal;
begin
 Result := 0;

 if Pos('PC', Str) > 0 then Result := ACTIVATE_PLAYERCOLLIDE;
 if Pos('MC', Str) > 0 then Result := Result or ACTIVATE_MONSTERCOLLIDE;
 if Pos('PP', Str) > 0 then Result := Result or ACTIVATE_PLAYERPRESS;
 if Pos('MP', Str) > 0 then Result := Result or ACTIVATE_MONSTERPRESS;
 if Pos('SH', Str) > 0 then Result := Result or ACTIVATE_SHOT;
 if Pos('NM', Str) > 0 then Result := Result or ACTIVATE_NOMONSTER;
 if Pos('NP', Str) > 0 then Result := Result or ACTIVATE_NOPLAYER;
 if Pos('IC', Str) > 0 then Result := Result or ACTIVATE_ITEMCOLLIDE;
 if Pos('NI', Str) > 0 then Result := Result or ACTIVATE_NOITEM;
end;

function KeyToStr(Key: Word): string;
begin
 Result := '';

 if WordBool(KEY_RED and Key) then Result := Result+'+RK';
 if WordBool(KEY_GREEN and Key) then Result := Result+'+GK';
 if WordBool(KEY_BLUE and Key) then Result := Result+'+BK';
 if WordBool(KEY_REDTEAM and Key) then Result := Result+'+RT';
 if WordBool(KEY_BLUETEAM and Key) then Result := Result+'+BT';

 if (Result <> '') and (Result[1] = '+') then Delete(Result, 1, 1);
end;

function StrToKey(Str: string): Word;
begin
 Result := 0;

 if Pos('RK', Str) > 0 then Result := KEY_RED;
 if Pos('GK', Str) > 0 then Result := Result or KEY_GREEN;
 if Pos('BK', Str) > 0 then Result := Result or KEY_BLUE;
 if Pos('RT', Str) > 0 then Result := Result or KEY_REDTEAM;
 if Pos('BT', Str) > 0 then Result := Result or KEY_BLUETEAM;
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
  if ItemType in [ITEM_MEDKIT_SMALL..ITEM_KEY_BLUE] then
    Result := ItemNames[ItemType]
  else
    Result := ItemNames[ITEM_AMMO_BULLETS];
end;

function StrToItem(Str: String): Byte;
var
  i: Integer;

begin
  Result := ITEM_AMMO_BULLETS;
  for i := ITEM_MEDKIT_SMALL to ITEM_KEY_BLUE do
    if ItemNames[i] = Str then
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

 if SelectedObjects = nil then Exit;

 for a := 0 to High(SelectedObjects) do
  if SelectedObjects[a].Live then Result := Result+1;
end;

function GetFirstSelected(): Integer;
var
  a: Integer;
begin
 Result := -1;

 if SelectedObjects = nil then Exit;

 for a := 0 to High(SelectedObjects) do
  if SelectedObjects[a].Live then
  begin
   Result := a;
   Exit;
  end;
end;

procedure MoveMap(X, Y: Integer);
begin
 with MainForm.RenderPanel do
 begin
  MapOffset.X := ((-(X-(Width-(gMapInfo.Width div 16)*Scale))*16)+
                  (Width div 32)*Scale*16) div Scale;
  if MapOffset.X > 0 then MapOffset.X := 0;
  if MapOffset.X-Width < -gMapInfo.Width then
   MapOffset.X := -(gMapInfo.Width div 16)*16+(Width div 16)*16;

  MapOffset.Y := ((-Y*16)+(Height div 32)*16) div Scale;
  if MapOffset.Y > 0 then MapOffset.Y := 0;
  if MapOffset.Y-Height < -gMapInfo.Height then
   MapOffset.Y := -(gMapInfo.Height div 16)*16+(Height div 16)*16;
 end;

 MainForm.sbVertical.Position := Abs(MapOffset.Y);
 MainForm.sbHorizontal.Position := Abs(MapOffset.X);

 MainForm.Resize;
end;

function IsTexturedPanel(PanelType: Word): Boolean;
begin
 Result := WordBool(PanelType and (PANEL_WALL or PANEL_BACK or PANEL_FORE or
                                   PANEL_STEP or PANEL_OPENDOOR or PANEL_CLOSEDOOR));
end;

procedure FillProperty();
var
  _id: DWORD;
begin
 MainForm.vleObjectProperty.Strings.Clear;

 if SelectedObjectCount() <> 1 then Exit;
 _id := GetFirstSelected();
 if not SelectedObjects[_id].Live then Exit;

 with MainForm.vleObjectProperty do
  with ItemProps[InsertRow('ID', IntToStr(SelectedObjects[_id].ID), True)-1] do
  begin
   EditStyle := esSimple;
   ReadOnly := True;
  end;

 case SelectedObjects[0].ObjectType of
  OBJECT_PANEL:
  begin
   with MainForm.vleObjectProperty, gPanels[SelectedObjects[_id].ID] do
   begin
    with ItemProps[InsertRow('X', IntToStr(X), True)-1] do
    begin
     EditStyle := esSimple;
     MaxLength := 5;
    end;

    with ItemProps[InsertRow('Y', IntToStr(Y), True)-1] do
    begin
     EditStyle := esSimple;
     MaxLength := 5;
    end;

    with ItemProps[InsertRow('Длина', IntToStr(Width), True)-1] do
    begin
     EditStyle := esSimple;
     MaxLength := 5;
    end;

    with ItemProps[InsertRow('Высота', IntToStr(Height), True)-1] do
    begin
     EditStyle := esSimple;
     MaxLength := 5;
    end;

    if IsTexturedPanel(PanelType) then
    begin
     with ItemProps[InsertRow('Тип панели', GetPanelName(PanelType), True)-1] do
     begin
      EditStyle := esPickList;
      ReadOnly := True;
     end;

     if TextureName <> '' then
     begin
      with ItemProps[InsertRow('Прозрачность', IntToStr(Alpha), True)-1] do
      begin
       EditStyle := esSimple;
       MaxLength := 3;
      end;

      with ItemProps[InsertRow('Смешивание', BoolNames[Blending], True)-1] do
      begin
       EditStyle := esPickList;
       ReadOnly := True;
      end;
     end;
    end
     else
    begin
     with ItemProps[InsertRow('Тип панели', GetPanelName(PanelType), True)-1] do
     begin
      EditStyle := esSimple;
      ReadOnly := True;
     end;
    end;
   end;
  end;

  OBJECT_ITEM:
  begin
   with MainForm.vleObjectProperty, gItems[SelectedObjects[_id].ID] do
   begin
    with ItemProps[InsertRow('X', IntToStr(X), True)-1] do
    begin
     EditStyle := esSimple;
     MaxLength := 5;
    end;

    with ItemProps[InsertRow('Y', IntToStr(Y), True)-1] do
    begin
     EditStyle := esSimple;
     MaxLength := 5;
    end;

    with ItemProps[InsertRow('Только DM', BoolNames[OnlyDM], True)-1] do
    begin
     EditStyle := esPickList;
     ReadOnly := True;
    end;

    with ItemProps[InsertRow('Падает', BoolNames[Fall], True)-1] do
    begin
     EditStyle := esPickList;
     ReadOnly := True;
    end;
   end;
  end;

  OBJECT_MONSTER:
  begin
   with MainForm.vleObjectProperty, gMonsters[SelectedObjects[_id].ID] do
   begin
    with ItemProps[InsertRow('X', IntToStr(X), True)-1] do
    begin
     EditStyle := esSimple;
     MaxLength := 5;
    end;

    with ItemProps[InsertRow('Y', IntToStr(Y), True)-1] do
    begin
     EditStyle := esSimple;
     MaxLength := 5;
    end;

    with ItemProps[InsertRow('Направление', DirNames[Direction], True)-1] do
    begin
     EditStyle := esPickList;
     ReadOnly := True;
    end;
   end;
  end;

  OBJECT_AREA:
  begin
   with MainForm.vleObjectProperty, gAreas[SelectedObjects[_id].ID] do
   begin
    with ItemProps[InsertRow('X', IntToStr(X), True)-1] do
    begin
     EditStyle := esSimple;
     MaxLength := 5;
    end;

    with ItemProps[InsertRow('Y', IntToStr(Y), True)-1] do
    begin
     EditStyle := esSimple;
     MaxLength := 5;
    end;

    with ItemProps[InsertRow('Направление', DirNames[Direction], True)-1] do
    begin
     EditStyle := esPickList;
     ReadOnly := True;
    end;
   end;
  end;

  OBJECT_TRIGGER:
  begin
   with MainForm.vleObjectProperty, gTriggers[SelectedObjects[_id].ID] do
   begin
    with ItemProps[InsertRow('Тип триггера', GetTriggerName(TriggerType), True)-1] do
    begin
     EditStyle := esSimple;
     ReadOnly := True;
    end;

    with ItemProps[InsertRow('X', IntToStr(X), True)-1] do
    begin
     EditStyle := esSimple;
     MaxLength := 5;
    end;

    with ItemProps[InsertRow('Y', IntToStr(Y), True)-1] do
    begin
     EditStyle := esSimple;
     MaxLength := 5;
    end;

    with ItemProps[InsertRow('Длина', IntToStr(Width), True)-1] do
    begin
     EditStyle := esSimple;
     MaxLength := 5;
    end;

    with ItemProps[InsertRow('Высота', IntToStr(Height), True)-1] do
    begin
     EditStyle := esSimple;
     MaxLength := 5;
    end;

    with ItemProps[InsertRow('Включен', BoolNames[Enabled], True)-1] do
    begin
     EditStyle := esPickList;
     ReadOnly := True;
    end;

    with ItemProps[InsertRow('Панель с текстурой', IntToStr(TexturePanel), True)-1] do
    begin
     EditStyle := esEllipsis;
     ReadOnly := True;
    end;

    with ItemProps[InsertRow('Тип активации', ActivateToStr(ActivateType), True)-1] do
    begin
     EditStyle := esEllipsis;
     ReadOnly := True;
    end;

    with ItemProps[InsertRow('Ключи', KeyToStr(Key), True)-1] do
    begin
     EditStyle := esEllipsis;
     ReadOnly := True;
    end;

    case TriggerType of
    TRIGGER_EXIT:
    begin
     with ItemProps[InsertRow('След. карта', Data.MapName, True)-1] do
     begin
      EditStyle := esEllipsis;
      ReadOnly := True;
     end;
    end;

    TRIGGER_TELEPORT:
    begin
     with ItemProps[InsertRow('Точка телепорта', Format('(%d:%d)', [Data.TargetPoint.X, Data.TargetPoint.Y]), True)-1] do
     begin
      EditStyle := esEllipsis;
      ReadOnly := True;
     end;

     with ItemProps[InsertRow('Как в D2D', BoolNames[Data.d2d_teleport], True)-1] do
     begin
      EditStyle := esPickList;
      ReadOnly := True;
     end;

     with ItemProps[InsertRow('Тихий телепорт', BoolNames[Data.silent_teleport], True)-1] do
     begin
      EditStyle := esPickList;
      ReadOnly := True;
     end;

     with ItemProps[InsertRow('Направление после', DirNamesAdv[Data.TlpDir], True)-1] do
       begin
         EditStyle := esPickList;
         ReadOnly := True;
       end;

     {with ItemProps[InsertRow('Телепорт итемов', BoolNames[Data.items_teleport], True)-1] do
     begin
      EditStyle := esPickList;
      ReadOnly := True;
     end;}
    end;

    TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR, TRIGGER_DOOR5:
    begin
     with ItemProps[InsertRow('Панель двери', IntToStr(Data.PanelID), True)-1] do
     begin
      EditStyle := esEllipsis;
      ReadOnly := True;
     end;

     with ItemProps[InsertRow('Без звука', BoolNames[Data.NoSound], True)-1] do
     begin
      EditStyle := esPickList;
      ReadOnly := True;
     end;

     with ItemProps[InsertRow('Как в D2D', BoolNames[Data.d2d_doors], True)-1] do
     begin
      EditStyle := esPickList;
      ReadOnly := True;
     end;
    end;

    TRIGGER_CLOSETRAP, TRIGGER_TRAP:
    begin
     with ItemProps[InsertRow('Панель ловушки', IntToStr(Data.PanelID), True)-1] do
     begin
      EditStyle := esEllipsis;
      ReadOnly := True;
     end;

     with ItemProps[InsertRow('Без звука', BoolNames[Data.NoSound], True)-1] do
     begin
      EditStyle := esPickList;
      ReadOnly := True;
     end;

     with ItemProps[InsertRow('Как в D2D', BoolNames[Data.d2d_doors], True)-1] do
     begin
      EditStyle := esPickList;
      ReadOnly := True;
     end;
    end;

    TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF:
    begin
     with ItemProps[InsertRow('Область воздействия', Format('(%d:%d %d:%d)',
                              [Data.tX, Data.tY, Data.tWidth, Data.tHeight]), True)-1] do
     begin
      EditStyle := esEllipsis;
      ReadOnly := True;
     end;

     with ItemProps[InsertRow('Задержка', IntToStr(Data.Wait), True)-1] do
     begin
      EditStyle := esSimple;
      MaxLength := 5;
     end;

     with ItemProps[InsertRow('Счетчик', IntToStr(Data.Count), True)-1] do
     begin
      EditStyle := esSimple;
      MaxLength := 5;
     end;

     with ItemProps[InsertRow('ID монстра', IntToStr(Data.MonsterID-1), True)-1] do
     begin
      EditStyle := esEllipsis;
      ReadOnly := True;
     end;
    end;

    TRIGGER_SECRET: ;

    TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT:
    begin
     with ItemProps[InsertRow('Панель лифта', IntToStr(Data.PanelID), True)-1] do
     begin
      EditStyle := esEllipsis;
      ReadOnly := True;
     end;

     with ItemProps[InsertRow('Без звука', BoolNames[Data.NoSound], True)-1] do
     begin
      EditStyle := esPickList;
      ReadOnly := True;
     end;

     with ItemProps[InsertRow('Как в D2D', BoolNames[Data.d2d_doors], True)-1] do
     begin
      EditStyle := esPickList;
      ReadOnly := True;
     end;
    end;

    TRIGGER_TEXTURE:
    begin
     with ItemProps[InsertRow('Один раз', BoolNames[Data.ActivateOnce], True)-1] do
     begin
      EditStyle := esPickList;
      ReadOnly := True;
     end;

     with ItemProps[InsertRow('Аним. один раз', BoolNames[Data.AnimOnce], True)-1] do
     begin
      EditStyle := esPickList;
      ReadOnly := True;
     end;
    end;

    TRIGGER_SOUND:
    begin
     with ItemProps[InsertRow('Звук', Data.SoundName, True)-1] do
     begin
      EditStyle := esEllipsis;
      ReadOnly := True;
     end;

     with ItemProps[InsertRow('Громкость', IntToStr(Data.Volume), True)-1] do
     begin
      EditStyle := esSimple;
      MaxLength := 3;
     end;

     with ItemProps[InsertRow('Стерео', IntToStr(Data.Pan), True)-1] do
     begin
      EditStyle := esSimple;
      MaxLength := 3;
     end;

     with ItemProps[InsertRow('Цикл', IntToStr(Data.PlayCount), True)-1] do
     begin
      EditStyle := esSimple;
      MaxLength := 3;
     end;

     {with ItemProps[InsertRow('Дистанция', IntToStr(Data.SoundDistance), True)-1] do
     begin
      EditStyle := esSimple;
      MaxLength := 3;
     end;}

     with ItemProps[InsertRow('Локальный', BoolNames[Data.Local], True)-1] do
     begin
      EditStyle := esPickList;
      ReadOnly := True;
     end;

     with ItemProps[InsertRow('Переключение', BoolNames[Data.SoundSwitch], True)-1] do
     begin
      EditStyle := esPickList;
      ReadOnly := True;
     end;
    end;

    TRIGGER_SPAWNMONSTER:
      begin
        with ItemProps[InsertRow('Тип монстра', MonsterToStr(Data.MonType), True)-1] do
          begin
            EditStyle := esEllipsis;
            ReadOnly := True;
          end;

        with ItemProps[InsertRow('Точка появления', Format('(%d:%d)',
             [Data.MonPos.X, Data.MonPos.Y]), True)-1] do
          begin
            EditStyle := esEllipsis;
            ReadOnly := True;
          end;

        with ItemProps[InsertRow('Направление', DirNames[TDirection(Data.MonDir)], True)-1] do
          begin
            EditStyle := esPickList;
            ReadOnly := True;
          end;

        with ItemProps[InsertRow('Здоровье', IntToStr(Data.MonHealth), True)-1] do
          begin
           EditStyle := esSimple;
           MaxLength := 5;
          end;
      end;

    TRIGGER_SPAWNITEM:
      begin
        with ItemProps[InsertRow('Тип предмета', ItemToStr(Data.ItemType), True)-1] do
          begin
            EditStyle := esEllipsis;
            ReadOnly := True;
          end;

        with ItemProps[InsertRow('Точка появления', Format('(%d:%d)',
             [Data.ItemPos.X, Data.ItemPos.Y]), True)-1] do
          begin
            EditStyle := esEllipsis;
            ReadOnly := True;
          end;

        with ItemProps[InsertRow('Падает', BoolNames[Data.ItemFalls], True)-1] do
          begin
            EditStyle := esPickList;
            ReadOnly := True;
          end;
      end;

   TRIGGER_MUSIC:
     begin
       with ItemProps[InsertRow('Музыка', Data.MusicName, True)-1] do
         begin
           EditStyle := esEllipsis;
           ReadOnly := True;
         end;
     end;
    end; //case TriggerType
   end;
  end; // OBJECT_TRIGGER:
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

  if SelectedObjects <> nil then
   for a := 0 to High(SelectedObjects) do
    with SelectedObjects[a] do
    if Live and (ID = fID) and (ObjectType = fObjectType) then
    begin
     Live := False;
     b := True;
    end;

  if b then Exit;

  SetLength(SelectedObjects, Length(SelectedObjects)+1);

  with SelectedObjects[High(SelectedObjects)] do
  begin
   ObjectType := fObjectType;
   ID := fID;
   Live := True;
  end;
 end
  else
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

procedure RemoveSelect();
begin
 SelectedObjects := nil;
 DrawPressRect := False;
 MouseLDown := False;
 MouseRDown := False;
 MouseAction := MOUSEACTION_NONE;
 SelectFlag := SELECTFLAG_NONE;
 ResizeType := RESIZETYPE_NONE;
 ResizeDirection := RESIZEDIR_NONE;
 MainForm.vleObjectProperty.Strings.Clear;
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
 if SelectedObjects = nil then Exit;

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

 RemoveSelect();

 MainForm.miUndo.Enabled := UndoBuffer <> nil;
end;

procedure Undo_Add(ObjectType: Byte; ID: DWORD; Group: Boolean = False);
var
  i, ii: Integer;
begin
 if not Group then SetLength(UndoBuffer, Length(UndoBuffer)+1);
 SetLength(UndoBuffer[High(UndoBuffer)], Length(UndoBuffer[High(UndoBuffer)])+1);
 i := High(UndoBuffer);
 ii := High(UndoBuffer[i]);

 case ObjectType of
  OBJECT_PANEL: UndoBuffer[i, ii].UndoType := UNDO_ADD_PANEL;
  OBJECT_ITEM: UndoBuffer[i, ii].UndoType := UNDO_ADD_ITEM;
  OBJECT_MONSTER: UndoBuffer[i, ii].UndoType := UNDO_ADD_MONSTER;
  OBJECT_AREA: UndoBuffer[i, ii].UndoType := UNDO_ADD_AREA;
  OBJECT_TRIGGER: UndoBuffer[i, ii].UndoType := UNDO_ADD_TRIGGER;
 end;

 UndoBuffer[i, ii].AddID := ID;

 MainForm.miUndo.Enabled := UndoBuffer <> nil;
end;

procedure FullClear();
begin
 RemoveSelect();
 ClearMap();
 UndoBuffer := nil;
 CopyBuffer := nil;
 MapCheckForm.lbErrorList.Clear;
 MapCheckForm.mErrorDescription.Clear;
 MainForm.miUndo.Enabled := False;
 MainForm.miPaste.Enabled := False;
 MainForm.sbHorizontal.Position := 0;
 MainForm.sbVertical.Position := 0;
 MainForm.FormResize(nil);
 MainForm.Caption := FormCaption;
 OpenedMap := '';
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
   if StrToIntDef(MainForm.vleObjectProperty.Values['Длина'], 1) mod TextureWidth <> 0 then
   begin
    MessageBox(0, PChar(Format('Длина панели должна быть кратна длине текстуры (%d)',
               [gPanels[SelectedObjects[_id].ID].TextureWidth])), 'Ошибка',
               MB_ICONINFORMATION or MB_OK or MB_TASKMODAL or MB_DEFBUTTON1);
    Exit;
   end;

  if TextureHeight <> 0 then
   if StrToIntDef(Trim(MainForm.vleObjectProperty.Values['Высота']), 1) mod TextureHeight <> 0 then
   begin
    MessageBox(0, PChar(Format('Высота панели должна быть кратна высоте текстуры (%d)',
               [TextureHeight])), 'Ошибка', MB_ICONINFORMATION or MB_OK or
               MB_TASKMODAL or MB_DEFBUTTON1);
    Exit;
   end;

  if IsTexturedPanel(PanelType) and (TextureName <> '') then
   if not (StrToIntDef(MainForm.vleObjectProperty.Values['Прозрачность'], -1) in [0..255]) then
   begin
    MessageBox(0, PChar('Прозрачность должна быть в интервале [0..255]'), 'Ошибка',
              MB_ICONINFORMATION or MB_OK or MB_TASKMODAL or MB_DEFBUTTON1);
    Exit;
   end;
 end;

 if SelectedObjects[_id].ObjectType in [OBJECT_PANEL, OBJECT_TRIGGER] then
  if (StrToIntDef(MainForm.vleObjectProperty.Values['Длина'], 0) <= 0) or
     (StrToIntDef(MainForm.vleObjectProperty.Values['Высота'], 0) <= 0) then
  begin
   MessageBox(0, PChar('Высота и длина должны быть больше 0'), 'Ошибка',
              MB_ICONINFORMATION or MB_OK or MB_TASKMODAL or MB_DEFBUTTON1);
   Exit;
  end;

 if (Trim(MainForm.vleObjectProperty.Values['X']) = '') or
    (Trim(MainForm.vleObjectProperty.Values['Y']) = '') then
 begin
  MessageBox(0, PChar('Не заданы координаты X и Y'), 'Ошибка',
             MB_ICONINFORMATION or MB_OK or MB_TASKMODAL or MB_DEFBUTTON1);
  Exit;
 end;

 Result := True;
end;

function AddTexture(aWAD, aSection, aTex: String): Boolean;
var
  a: Integer;
  ok: Boolean;
  FileName: String;
  ResourceName: String;
  FullResourceName: String;
  SectionName: String;
  Data: Pointer;
  Width, Height: Word;
  fn: String;
  
begin
  if aSection = '..' then
    SectionName := ''
  else
    SectionName := aSection;

  if aWAD = WAD_SPECIAL_MAP then
    begin
      g_ProcessResourceStr(OpenedMap, @fn, nil, nil);
    //FileName := EditorDir+'maps\'+ExtractFileName(fn);
      FileName := fn;
      ResourceName := ':'+SectionName+'\'+aTex;
    end
  else
    if aWAD = WAD_SPECIAL_TEXTURES then
      begin
        FileName := '';
        ResourceName := aTex;
      end
    else
      begin
        FileName := EditorDir+'wads\'+aWAD;
        ResourceName := aWAD+':'+SectionName+'\'+aTex;
      end;

  ok := True;

  for a := 0 to MainForm.lbTextureList.Items.Count-1 do
    if ResourceName = MainForm.lbTextureList.Items[a] then
      begin
        MessageBox(0, PChar(Format('Текстура "%s" уже существует', [ResourceName])),
                   'Ошибка', MB_ICONINFORMATION or MB_OK or MB_TASKMODAL or MB_DEFBUTTON1);
        ok := False;
      end;

  if Length(ResourceName) > 64 then
    begin
      MessageBox(0, PChar(Format('Имя ресурса "%s" должно быть <= 64 символам', [ResourceName])),
                 'Ошибка', MB_ICONINFORMATION or MB_OK or MB_TASKMODAL or MB_DEFBUTTON1);
      ok := False;
    end;

  if ok then
    begin
      if aWAD = WAD_SPECIAL_TEXTURES then
        begin
          MainForm.lbTextureList.Items.Add(ResourceName);
          Result := True;
          Exit;
        end;

      FullResourceName := FileName+':'+SectionName+'\'+aTex;

      if IsAnim(FullResourceName) then
        begin
          GetFrame(FullResourceName, Data, Width, Height);

          if g_CreateTextureMemorySize(Data, ResourceName, 0, 0, Width, Height, 1) then
            MainForm.lbTextureList.Items.Add(ResourceName);
        end
      else
        begin
          if g_CreateTextureWAD(ResourceName, FullResourceName) then
            MainForm.lbTextureList.Items.Add(ResourceName);
        end;
    end;

  Result := ok;
end;

procedure OpenMap(FileName: string; mapN: string);
var
  MapName: string;
  idx: Integer;

begin
 SelectMapForm.GetMaps(FileName);
 
 if mapN = '' then
   idx := -1
 else
   idx := SelectMapForm.lbMapList.Items.IndexOf(mapN);

 if idx < 0 then
   begin
     if (SelectMapForm.ShowModal = mrOK) and (SelectMapForm.lbMapList.ItemIndex <> -1) then
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
  pLoadProgress.Show;

  OpenedMap := FileName+':\'+MapName;

  idx := RecentFiles.IndexOf(OpenedMap);
  if idx >= 0 then
    RecentFiles.Delete(idx);
  RecentFiles.Insert(0, OpenedMap);
  while RecentFiles.Count > MAX_RECENT_FILES do
    RecentFiles.Delete(RecentFiles.Count-1);
  RefreshRecentMenu;
    
  LoadMap(FileName+':\'+MapName);

  pLoadProgress.Hide;
  FormResize(nil);

  lbTextureList.Sorted := True;
  lbTextureList.Sorted := False;

  Caption := Format('%s - %s:%s', [FormCaption, ExtractFileName(FileName), MapName]);
 end;
end;

procedure MoveSelectedObjects(Wall, alt: Boolean; dx, dy: Integer);
var
  okX, okY: Boolean;
  a: Integer;
begin
 if SelectedObjects = nil then Exit;

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

   if not (okX and okY) then Break;
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
     begin
      if okX then gTriggers[SelectedObjects[a].ID].Data.tX := gTriggers[SelectedObjects[a].ID].Data.tX+dx;
      if okY then gTriggers[SelectedObjects[a].ID].Data.tY := gTriggers[SelectedObjects[a].ID].Data.tY+dy;
     end;

     if gTriggers[SelectedObjects[a].ID].TriggerType in [TRIGGER_TELEPORT] then
     begin
      if okX then gTriggers[SelectedObjects[a].ID].Data.TargetPoint.X := gTriggers[SelectedObjects[a].ID].Data.TargetPoint.X+dx;
      if okY then gTriggers[SelectedObjects[a].ID].Data.TargetPoint.Y := gTriggers[SelectedObjects[a].ID].Data.TargetPoint.Y+dy;
     end;

     if gTriggers[SelectedObjects[a].ID].TriggerType in [TRIGGER_SPAWNMONSTER] then
     begin
      if okX then gTriggers[SelectedObjects[a].ID].Data.MonPos.X := gTriggers[SelectedObjects[a].ID].Data.MonPos.X+dx;
      if okY then gTriggers[SelectedObjects[a].ID].Data.MonPos.Y := gTriggers[SelectedObjects[a].ID].Data.MonPos.Y+dy;
     end;

     if gTriggers[SelectedObjects[a].ID].TriggerType in [TRIGGER_SPAWNITEM] then
     begin
      if okX then gTriggers[SelectedObjects[a].ID].Data.ItemPos.X := gTriggers[SelectedObjects[a].ID].Data.ItemPos.X+dx;
      if okY then gTriggers[SelectedObjects[a].ID].Data.ItemPos.Y := gTriggers[SelectedObjects[a].ID].Data.ItemPos.Y+dy;
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

 RemoveSelect();
end;

procedure SwitchLayer(Layer: Byte);
begin
 ShowLayer(Layer, not LayerEnabled[Layer]);
end;

procedure SwitchMap();
begin
 ShowMap := not ShowMap;
 MainForm.tbShowMap.Down := ShowMap;
end;

function SelectedTexture(): string;
begin
 if MainForm.lbTextureList.ItemIndex <> -1 then
  Result := MainForm.lbTextureList.Items[MainForm.lbTextureList.ItemIndex]
   else Result := '';
end;

function IsSpecialTextureSel(): Boolean;
begin
 Result := (MainForm.lbTextureList.ItemIndex <> -1) and
           IsSpecialTexture(MainForm.lbTextureList.Items[MainForm.lbTextureList.ItemIndex]);
end;

//----------------------------------------
//Закончились вспомогательные процедуры
//----------------------------------------

procedure TMainForm.RefreshRecentMenu;
var
  i: Integer;
  MI: TMenuItem;

begin
  if MainMenu.Items[0].Count < RECENT_FILES_MENU_START then
    begin
      if RecentFiles.Count > 0 then
        begin
          MI := TMenuItem.Create(MainMenu.Items[0]);
          MI.Caption := '-';
          MainMenu.Items[0].Add(MI);
        end;
    end
  else
    begin
      while MainMenu.Items[0].Count > RECENT_FILES_MENU_START do
        MainMenu.Items[0].Delete(MainMenu.Items[0].Count-1);
      if RecentFiles.Count = 0 then
        MainMenu.Items[0].Delete(MainMenu.Items[0].Count-1);
    end;

  for i := 0 to RecentFiles.Count-1 do
    begin
      MI := TMenuItem.Create(MainMenu.Items[0]);
      MI.Caption := IntToStr(i+1) + '  ' + RecentFiles[i];
      MI.OnClick := aRecentFileExecute;
      MainMenu.Items[0].Add(MI);
    end;
end;

procedure TMainForm.aRecentFileExecute(Sender: TObject);
var
  n: Integer;
  s, fn: String;

begin
  s := LowerCase((Sender as TMenuItem).Caption);
  n := StrToInt(Copy(s, 2, 1)) - 1;
  s := LowerCase(RecentFiles[n]);
  if Pos('.wad:\', s) > 0 then
    begin // map name included
      fn := Copy(s, 1, Pos('.wad:\', s)+3);
      Delete(s, 1, Pos('.wad:\', s)+5);
      if (FileExists(fn)) then
        OpenMap(fn, s);
    end
  else // only wad name
    if (FileExists(s)) then
        OpenMap(s, '');
end;

procedure TMainForm.aEditorOptionsExecute(Sender: TObject);
begin
 OptionsForm.ShowModal;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  PixelFormat: GLuint;
  pfd: TPIXELFORMATDESCRIPTOR;
  config: TConfig;
  i: Integer;
  s: String;
begin
 Randomize;

 EditorDir := ExtractFilePath(Application.ExeName);

 OpenDialog.InitialDir := EditorDir;
 SaveDialog.InitialDir := EditorDir;

 e_InitLog(EditorDir+'Editor.log', WM_NEWFILE);

 e_WriteLog('Init OpenGL', MSG_NOTIFY);

 InitOpenGL();
 hDC := GetDC(RenderPanel.Handle);

 FillChar(pfd, SizeOf(pfd), 0);
 with pfd do begin
  nSize := SizeOf(pfd);
  nVersion := 1;
  dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  iPixelType := PFD_TYPE_RGBA;
  cColorBits := 24;
  cDepthBits := 32;
  iLayerType := PFD_MAIN_PLANE;
 end;
 PixelFormat := ChoosePixelFormat (hDC, @pfd);
 SetPixelFormat (hDC, PixelFormat, @pfd);

 hRC := wglCreateContext(hDC);
 ActivateRenderingContext(hDC, hRC);

 e_InitGL(False);

 gEditorFont := e_SimpleFontCreate('Arial Cyr', 12, FW_BOLD, hDC);

 e_WriteLog('Loading data', MSG_NOTIFY);
 LoadData();

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

 config := TConfig.CreateFile(EditorDir+'\editor.cfg');
 if config.ReadBool('Editor', 'Maximize', False) then WindowState := wsMaximized;
 ShowMap := config.ReadBool('Editor', 'Minimap', False);

 RecentFiles := TStringList.Create;
 for i := 0 to MAX_RECENT_FILES-1 do
   begin
     s := config.ReadStr('RecentFiles', IntToStr(i+1), '');
     if s <> '' then
       RecentFiles.Add(s);
   end;
 RefreshRecentMenu;

 config.Destroy;

 lbItemList.Clear;
 for i := ITEM_MEDKIT_SMALL to ITEM_KEY_BLUE do
   lbItemList.Items.Add(ItemNames[i]);

 lbMonsterList.Clear;
 for i := MONSTER_DEMON to MONSTER_MAN do
   lbMonsterList.Items.Add(MonsterNames[i]);

 lbAreasList.Clear;
 for i := AREA_PLAYERPOINT1 to AREA_BLUETEAMPOINT do
   lbAreasList.Items.Add(AreaNames[i]);

 Application.OnIdle := OnIdle;
end;

procedure TMainForm.Draw();
var
 ps: TPaintStruct;
 x, y: Integer;
 a, b: Integer;
 ID: DWORD;
 Width, Height: Word;
 Rect: TRectWH;
 ObjCount: Word;
 aX, aY, aX2, aY2, XX: Integer;
begin
 BeginPaint(Handle, ps);
 e_BeginRender();

 e_Clear(GL_COLOR_BUFFER_BIT, GetRValue(BackColor)/255, GetGValue(BackColor)/255,
         GetBValue(BackColor)/255);

 DrawMap();

 ObjCount := SelectedObjectCount();

 if ObjCount > 0 then
 begin
  for a := 0 to High(SelectedObjects) do
   if SelectedObjects[a].Live then
   begin
    Rect := ObjectGetRect(SelectedObjects[a].ObjectType, SelectedObjects[a].ID);

    with Rect do
    begin
     e_DrawQuad(X+MapOffset.X, Y+MapOffset.Y, X+MapOffset.X+Width, Y+MapOffset.Y+Height, 1, 255, 0, 0);

     if (ObjCount = 1) and
        (SelectedObjects[GetFirstSelected].ObjectType in [OBJECT_PANEL, OBJECT_TRIGGER]) then
     begin
      e_DrawPoint(5, X+MapOffset.X, Y+MapOffset.Y+(Height div 2), 255, 255, 255);
      e_DrawPoint(5, X+MapOffset.X+Width, Y+MapOffset.Y+(Height div 2), 255, 255, 255);
      e_DrawPoint(5, X+MapOffset.X+(Width div 2), Y+MapOffset.Y, 255, 255, 255);
      e_DrawPoint(5, X+MapOffset.X+(Width div 2), Y+MapOffset.Y+Height, 255, 255, 255);

      e_DrawPoint(3, X+MapOffset.X, Y+MapOffset.Y+(Height div 2), 255, 0, 0);
      e_DrawPoint(3, X+MapOffset.X+Width, Y+MapOffset.Y+(Height div 2), 255, 0, 0);
      e_DrawPoint(3, X+MapOffset.X+(Width div 2), Y+MapOffset.Y, 255, 0, 0);
      e_DrawPoint(3, X+MapOffset.X+(Width div 2), Y+MapOffset.Y+Height, 255, 0, 0);
     end;
    end;
   end;
 end;

 if DotEnable then
  for x := 0 to (RenderPanel.Width div DotStep) do
   for y := 0 to (RenderPanel.Height div DotStep) do
    e_DrawPoint(1, x*DotStep, y*DotStep, GetRValue(DotColor),
                GetGValue(DotColor), GetBValue(DotColor));

 if (lbTextureList.ItemIndex <> -1) and (cbPreview.Checked) and
    (not IsSpecialTextureSel()) and (not PreviewMode) then
 begin
  g_GetTexture(SelectedTexture(), ID);
  g_GetTextureSizeByID(ID, Width, Height);
  e_DrawFillQuad(RenderPanel.Width-Width-1, RenderPanel.Height-Height-1,
                 RenderPanel.Width, RenderPanel.Height, GetRValue(PreviewColor),
                 GetGValue(PreviewColor), GetBValue(PreviewColor), 0);
  e_Draw(ID, RenderPanel.Width-Width, RenderPanel.Height-Height, 0, True, False);
 end;

 if SelectFlag = SELECTFLAG_TELEPORT then
 begin
  with gTriggers[SelectedObjects[GetFirstSelected()].ID] do
   if Data.d2d_teleport then
    e_DrawLine(2, MousePos.X-16, MousePos.Y-1,
               MousePos.X+16, MousePos.Y-1,
               0, 0, 255)
   else
    e_DrawQuad(MousePos.X, MousePos.Y, MousePos.X+AreaSize[AREA_DMPOINT].Width,
               MousePos.Y+AreaSize[AREA_DMPOINT].Height, 1, 255, 255, 255);

  e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+180, MousePos.Y+18, 192, 192, 192, 127);
  e_DrawQuad(MousePos.X+1, MousePos.Y+1, MousePos.X+179, MousePos.Y+17, 1, 255, 255, 255);
  e_SimpleFontPrint(MousePos.X+8, MousePos.Y+14, 'Выберите точку телепорта', gEditorFont, 0, 0, 0);
 end;

 if SelectFlag = SELECTFLAG_SPAWNPOINT then
 begin
  e_DrawLine(2, MousePos.X-16, MousePos.Y-1,
             MousePos.X+16, MousePos.Y-1,
             0, 0, 255);
  e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+180, MousePos.Y+18, 192, 192, 192, 127);
  e_DrawQuad(MousePos.X+1, MousePos.Y+1, MousePos.X+179, MousePos.Y+17, 1, 255, 255, 255);
  e_SimpleFontPrint(MousePos.X+8, MousePos.Y+14, 'Выберите точку появления', gEditorFont, 0, 0, 0);
 end;

 if SelectFlag = SELECTFLAG_DOOR then
 begin
  e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+180, MousePos.Y+18, 192, 192, 192, 127);
  e_DrawQuad(MousePos.X+1, MousePos.Y+1, MousePos.X+179, MousePos.Y+17, 1, 255, 255, 255);
  e_SimpleFontPrint(MousePos.X+8, MousePos.Y+14, 'Выберите панель двери', gEditorFont, 0, 0, 0);
 end;

 if SelectFlag = SELECTFLAG_TEXTURE then
 begin
  e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+196, MousePos.Y+18, 192, 192, 192, 127);
  e_DrawQuad(MousePos.X+1, MousePos.Y+1, MousePos.X+195, MousePos.Y+17, 1, 255, 255, 255);
  e_SimpleFontPrint(MousePos.X+8, MousePos.Y+14, 'Выберите панель с текстурой', gEditorFont, 0, 0, 0);
 end;

 if SelectFlag = SELECTFLAG_LIFT then
 begin
  e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+180, MousePos.Y+18, 192, 192, 192, 127);
  e_DrawQuad(MousePos.X+1, MousePos.Y+1, MousePos.X+179, MousePos.Y+17, 1, 255, 255, 255);
  e_SimpleFontPrint(MousePos.X+8, MousePos.Y+14, 'Выберите панель лифта', gEditorFont, 0, 0, 0);
 end;

 if SelectFlag = SELECTFLAG_MONSTER then
 begin
  e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+120, MousePos.Y+18, 192, 192, 192, 127);
  e_DrawQuad(MousePos.X+1, MousePos.Y+1, MousePos.X+119, MousePos.Y+17, 1, 255, 255, 255);
  e_SimpleFontPrint(MousePos.X+8, MousePos.Y+14, 'Выберите монстра', gEditorFont, 0, 0, 0);
 end;

 if DrawPressRect then
 begin
  e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+204, MousePos.Y+18, 192, 192, 192, 127);
  e_DrawQuad(MousePos.X+1, MousePos.Y+1, MousePos.X+203, MousePos.Y+17, 1, 255, 255, 255);
  e_SimpleFontPrint(MousePos.X+8, MousePos.Y+14, 'Нарисуйте область воздействия', gEditorFont, 0, 0, 0);
 end;

 if (MouseAction in [MOUSEACTION_DRAWPANEL, MOUSEACTION_DRAWTRIGGER, MOUSEACTION_RESIZE]) and
    (DrawPanelSize) then
 begin
  e_DrawFillQuad(MousePos.X, MousePos.Y, MousePos.X+88, MousePos.Y+32, 192, 192, 192, 127);
  e_DrawQuad(MousePos.X+1, MousePos.Y+1, MousePos.X+87, MousePos.Y+31, 1, 255, 255, 255);

  if MouseAction in [MOUSEACTION_DRAWPANEL, MOUSEACTION_DRAWTRIGGER] then
  begin
   e_SimpleFontPrint(MousePos.X+8, MousePos.Y+14, PChar(Format('Длина: %d',
                     [Abs(MousePos.X-MouseLDownPos.X)])), gEditorFont, 0, 0, 0);
   e_SimpleFontPrint(MousePos.X+8, MousePos.Y+28, PChar(Format('Высота: %d',
                     [Abs(MousePos.Y-MouseLDownPos.Y)])), gEditorFont, 0, 0, 0);
  end
   else if SelectedObjects[GetFirstSelected].ObjectType in [OBJECT_PANEL, OBJECT_TRIGGER] then
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

   e_SimpleFontPrint(MousePos.X+8, MousePos.Y+14, PChar(Format('Длина: %d', [Width])),
                     gEditorFont, 0, 0, 0);
   e_SimpleFontPrint(MousePos.X+8, MousePos.Y+28, PChar(Format('Высота: %d', [Height])),
                     gEditorFont, 0, 0, 0);
  end;
 end;

 if (MouseAction = MOUSEACTION_DRAWPANEL) and (DrawTexturePanel) and
    (lbTextureList.ItemIndex <> -1) and (DrawRect <> nil) and
    (cbPanelType.ItemIndex in [0..8]) and not IsSpecialTextureSel() then
 begin
  g_GetTexture(SelectedTexture(), ID);
  g_GetTextureSizeByID(ID, Width, Height);
  with DrawRect^ do
   e_DrawFill(ID, Min(Left, Right), Min(Top, Bottom), Abs(Right-Left) div Width,
              Abs(Bottom-Top) div Height, 0, True, False);
 end;

 if DrawRect <> nil then
  with DrawRect^ do e_DrawQuad(Left, Top, Right, Bottom, 1, 255, 255, 255);

 e_DrawPoint(3, MousePos.X, MousePos.Y, 0, 0, 255);

 if ShowMap then
 begin
  XX := RenderPanel.Width-((gMapInfo.Width div 16)*Scale);
  e_DrawFillQuad(XX-1, 0, RenderPanel.Width-1, ((gMapInfo.Height div 16)*Scale), 0, 0, 0, 0);
  e_DrawQuad(XX-1, 0, RenderPanel.Width-1, ((gMapInfo.Height div 16)*Scale)+1, 1, 197, 197, 197);

  if gPanels <> nil then
  begin
   for a := 0 to High(gPanels) do
   with gPanels[a] do
    if PanelType <> 0 then
    begin
     aX := XX+(X div (16 div Scale));
     aY := Y div (16 div Scale);

     if Width div (16 div Scale) = 0 then aX2 := aX+1
      else aX2 := aX+Width div (16 div Scale);
     if Height div (16 div Scale) = 0 then aY2 := aY+1
      else aY2 := aY+Height div (16 div Scale);

     case PanelType of
      PANEL_WALL: e_DrawFillQuad(aX, aY, aX2, aY2, 208, 208, 208, 0);
      PANEL_WATER: e_DrawFillQuad(aX, aY, aX2, aY2, 0, 0, 252, 0);
      PANEL_ACID1: e_DrawFillQuad(aX, aY, aX2, aY2, 200, 80, 4, 0);
      PANEL_ACID2: e_DrawFillQuad(aX, aY, aX2, aY2, 252, 140, 56, 0);
      PANEL_STEP: e_DrawFillQuad(aX, aY, aX2, aY2, 128, 128, 128, 0);
      PANEL_LIFTUP: e_DrawFillQuad(aX, aY, aX2, aY2, 116, 72, 36, 0);
      PANEL_LIFTDOWN: e_DrawFillQuad(aX, aY, aX2, aY2, 116, 124, 96, 0);
      PANEL_OPENDOOR: e_DrawFillQuad(aX, aY, aX2, aY2, 100, 220, 92, 0);
      PANEL_CLOSEDOOR: e_DrawFillQuad(aX, aY, aX2, aY2, 212, 184, 64, 0);
      PANEL_BLOCKMON: e_DrawFillQuad(aX, aY, aX2, aY2, 255, 0, 255, 0);
     end;
    end;

   if SelectedObjects <> nil then
   for b := 0 to High(SelectedObjects) do
    with SelectedObjects[b] do
    if Live and (ObjectType = OBJECT_PANEL) then
    with gPanels[SelectedObjects[b].ID] do
     if PanelType and not(PANEL_BACK or PANEL_FORE) <> 0 then
     begin
      aX := XX+(X div (16 div Scale));
      aY := Y div (16 div Scale);

      if Width div (16 div Scale) = 0 then aX2 := aX+1
       else aX2 := aX+Width div (16 div Scale);
      if Height div (16 div Scale) = 0 then aY2 := aY+1
       else aY2 := aY+Height div (16 div Scale);

      e_DrawFillQuad(aX, aY, aX2, aY2, 255, 0, 0, 0)
     end;

   e_DrawFillQuad(XX+(Abs(MapOffset.X) div (16 div Scale)), Abs(MapOffset.Y) div (16 div Scale),
                  XX+(Abs(MapOffset.X) div (16 div Scale))+(RenderPanel.Width div (16 div Scale)),
                  (Abs(MapOffset.Y) div (16 div Scale))+(RenderPanel.Height div (16 div Scale)),
                  127, 192, 127, 127, B_BLEND);

   e_DrawQuad(XX+(Abs(MapOffset.X) div (16 div Scale)), Abs(MapOffset.Y) div (16 div Scale),
              XX+(Abs(MapOffset.X) div (16 div Scale))+(RenderPanel.Width div (16 div Scale)),
              (Abs(MapOffset.Y) div (16 div Scale))+(RenderPanel.Height div (16 div Scale)),
              1, 255, 0, 0);
  end;
 end;

 e_EndRender();
 SwapBuffers(hDC);
 EndPaint(Handle, ps);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
 e_SetViewPort(0, 0, RenderPanel.Width, RenderPanel.Height);

 if gMapInfo.Width >= RenderPanel.Width
  then sbHorizontal.Max := gMapInfo.Width-RenderPanel.Width
   else sbHorizontal.Max := 0;

 if gMapInfo.Height >= RenderPanel.Height
  then sbVertical.Max := gMapInfo.Height-RenderPanel.Height
   else sbVertical.Max := 0;
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
 RenderPanel.SetFocus;

 if Button = mbLeft then
 begin
  if ShowMap and g_CollidePoint(X, Y, RenderPanel.Width-(gMapInfo.Width div 16)*Scale, 0,
                               RenderPanel.Width-1, (gMapInfo.Height div 16)*Scale) then
  begin
   MoveMap(X, Y);
   MouseAction := MOUSEACTION_MOVEMAP;
  end
   else
  if pcObjects.ActivePageIndex in [1, 2, 3] then
  begin
   case pcObjects.ActivePageIndex of
    1: if lbItemList.ItemIndex = -1 then
        MessageBox(0, 'Выберите итем', 'Ошибка', MB_ICONINFORMATION or MB_OK or
                   MB_TASKMODAL or MB_DEFBUTTON1)
        else
       begin
        item.X := MousePos.X-MapOffset.X;
        item.Y := MousePos.Y-MapOffset.Y;
        item.ItemType := lbItemList.ItemIndex + ITEM_MEDKIT_SMALL;
        item.OnlyDM := cbOnlyDM.Checked;
        item.Fall := (item.ItemType in [ITEM_KEY_RED, ITEM_KEY_GREEN, ITEM_KEY_BLUE]) or cbFall.Checked;
        Undo_Add(OBJECT_ITEM, AddItem(item));
       end;
    2: if lbMonsterList.ItemIndex = -1 then
        MessageBox(0, 'Выберите монстра', 'Ошибка', MB_ICONINFORMATION or MB_OK or
                   MB_TASKMODAL or MB_DEFBUTTON1)
        else
       begin
        monster.X := MousePos.X-MapOffset.X;
        monster.Y := MousePos.Y-MapOffset.Y;
        monster.MonsterType := lbMonsterList.ItemIndex + MONSTER_DEMON;
        if rbMonsterLeft.Checked then monster.Direction := D_LEFT else
         monster.Direction := D_RIGHT;
        Undo_Add(OBJECT_MONSTER, AddMonster(monster));
       end;
    3: if lbAreasList.ItemIndex = -1 then
        MessageBox(0, 'Выберите область', 'Ошибка', MB_ICONINFORMATION or MB_OK or
                   MB_TASKMODAL or MB_DEFBUTTON1)
        else if lbAreasList.ItemIndex <> 5 then
       begin
        area.X := MousePos.X-MapOffset.X;
        area.Y := MousePos.Y-MapOffset.Y;
        area.AreaType := lbAreasList.ItemIndex+1;
        if rbAreaLeft.Checked then area.Direction := D_LEFT else area.Direction := D_RIGHT;
        Undo_Add(OBJECT_AREA, AddArea(area));
       end else Beep;
   end;
  end
   else
  begin
   i := GetFirstSelected();
   if DrawPressRect and (i <> -1) and
      (SelectedObjects[i].ObjectType = OBJECT_TRIGGER) and
      (gTriggers[SelectedObjects[i].ID].TriggerType in
      [TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF]) then
    MouseAction := MOUSEACTION_DRAWPRESS
   else
    if pcObjects.ActivePageIndex = 0 then MouseAction := MOUSEACTION_DRAWPANEL
     else MouseAction := MOUSEACTION_DRAWTRIGGER;
  end;
 end; //if Button = mbLeft

 if Button = mbRight then
 begin
  if SelectFlag <> SELECTFLAG_NONE then
  begin
   case SelectFlag of
    SELECTFLAG_TELEPORT:
    with gTriggers[SelectedObjects[GetFirstSelected()].ID].Data.TargetPoint do
    begin
     X := MousePos.X-MapOffset.X;
     Y := MousePos.Y-MapOffset.Y;
    end;

    SELECTFLAG_SPAWNPOINT:
      with gTriggers[SelectedObjects[GetFirstSelected()].ID] do
        if TriggerType = TRIGGER_SPAWNMONSTER then
          begin
            Data.MonPos.X := MousePos.X-MapOffset.X;
            Data.MonPos.Y := MousePos.Y-MapOffset.Y;
          end
        else if TriggerType = TRIGGER_SPAWNITEM then
          begin
            Data.ItemPos.X := MousePos.X-MapOffset.X;
            Data.ItemPos.Y := MousePos.Y-MapOffset.Y;
          end;

    SELECTFLAG_DOOR:
    begin
     IDArray := ObjectInRect(MousePos.X-MapOffset.X, MousePos.Y-MapOffset.Y, 2, 2,
                             OBJECT_PANEL, True);
     if IDArray <> nil then
     begin
      for i := 0 to High(IDArray) do
       if (gPanels[IDArray[i]].PanelType = PANEL_OPENDOOR) or
          (gPanels[IDArray[i]].PanelType = PANEL_CLOSEDOOR) then
       begin
        gTriggers[SelectedObjects[GetFirstSelected()].ID].Data.PanelID := IDArray[i];
        Break;
       end;
     end else gTriggers[SelectedObjects[GetFirstSelected()].ID].Data.PanelID := -1;
    end;

    SELECTFLAG_TEXTURE:
    begin
     IDArray := ObjectInRect(MousePos.X-MapOffset.X, MousePos.Y-MapOffset.Y, 2, 2,
                             OBJECT_PANEL, True);
     if IDArray <> nil then
     begin
      for i := 0 to High(IDArray) do
       if (gPanels[IDArray[i]].PanelType in [PANEL_WALL, PANEL_BACK, PANEL_FORE]) and
          (gPanels[IDArray[i]].TextureName <> '') then
       begin
        gTriggers[SelectedObjects[GetFirstSelected()].ID].TexturePanel := IDArray[i];
        Break;
       end;
     end else gTriggers[SelectedObjects[GetFirstSelected()].ID].TexturePanel := -1;
    end;

    SELECTFLAG_LIFT:
    begin
     IDArray := ObjectInRect(MousePos.X-MapOffset.X, MousePos.Y-MapOffset.Y, 2, 2,
                             OBJECT_PANEL, True);
     if IDArray <> nil then
     begin
      for i := 0 to High(IDArray) do
       if (gPanels[IDArray[i]].PanelType = PANEL_LIFTUP) or
          (gPanels[IDArray[i]].PanelType = PANEL_LIFTDOWN) then
       begin
        gTriggers[SelectedObjects[GetFirstSelected()].ID].Data.PanelID := IDArray[i];
        Break;
       end;
     end else gTriggers[SelectedObjects[GetFirstSelected()].ID].Data.PanelID := -1;
    end;

    SELECTFLAG_MONSTER:
    begin
     IDArray := ObjectInRect(MousePos.X-MapOffset.X, MousePos.Y-MapOffset.Y, 2, 2,
                             OBJECT_MONSTER, False);
     if IDArray <> nil then
      gTriggers[SelectedObjects[GetFirstSelected()].ID].Data.MonsterID := IDArray[0]+1
       else gTriggers[SelectedObjects[GetFirstSelected()].ID].Data.MonsterID := 0;
    end;
   end;

   SelectFlag := SELECTFLAG_NONE;
  end
   else
  begin
   if (SelectedObjects <> nil) and not (ssCtrl in Shift) then
    for i := 0 to High(SelectedObjects) do
     with SelectedObjects[i] do
      if Live then
      begin
       if (ObjectType in [OBJECT_PANEL, OBJECT_TRIGGER]) and (SelectedObjectCount = 1) then
       begin
        Rect := ObjectGetRect(ObjectType, ID);

        c1 := g_Collide(X-MapOffset.X-1, Y-MapOffset.Y-1, 2, 2,
                        Rect.X-2, Rect.Y+(Rect.Height div 2)-2, 4, 4);
        c2 := g_Collide(X-MapOffset.X-1, Y-MapOffset.Y-1, 2, 2,
                        Rect.X+Rect.Width-2, Rect.Y+(Rect.Height div 2)-2, 4, 4);
        c3 := g_Collide(X-MapOffset.X-1, Y-MapOffset.Y-1, 2, 2,
                        Rect.X+(Rect.Width div 2)-2, Rect.Y-2, 4, 4);
        c4 := g_Collide(X-MapOffset.X-1, Y-MapOffset.Y-1, 2, 2,
                        Rect.X+(Rect.Width div 2)-2, Rect.Y+Rect.Height-2, 4, 4);

        if c1 or c2 or c3 or c4 then
        begin
         MouseAction := MOUSEACTION_RESIZE;
         LastMovePoint := MousePos;

         if c1 or c2 then
         begin
          ResizeType := RESIZETYPE_HORIZONTAL;
          if c1 then ResizeDirection := RESIZEDIR_LEFT
           else ResizeDirection := RESIZEDIR_RIGHT;
          RenderPanel.Cursor := crSizeWE;
         end
          else
         begin
          ResizeType := RESIZETYPE_VERTICAL;
          if c3 then ResizeDirection := RESIZEDIR_UP
           else ResizeDirection := RESIZEDIR_DOWN;
          RenderPanel.Cursor := crSizeNS;
         end;

         Break;
        end;
       end;

       if ObjectCollide(ObjectType, ID, MousePos.X-MapOffset.X-1, MousePos.Y-MapOffset.Y-1, 2, 2) then
       begin
        MouseAction := MOUSEACTION_MOVEOBJ;
        LastMovePoint := MousePos;

        Break;
       end;
      end;
  end;
 end;

 MouseRDown := Button = mbRight;
 if MouseRDown then MouseRDownPos := MousePos;

 MouseLDown := Button = mbLeft;
 if MouseLDown then MouseLDownPos := MousePos;
end;

procedure TMainForm.RenderPanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  panel: TPanel;
  trigger: TTrigger;
  i: Integer;
  IDArray: DWArray;
  rRect: TRectWH;
  rSelectRect: Boolean;
begin
 if Button = mbLeft then MouseLDown := False;
 if Button = mbRight then MouseRDown := False;

 DrawRect := nil;
 ResizeType := RESIZETYPE_NONE;

 if Button = mbLeft then
 begin
  if MouseAction <> MOUSEACTION_NONE then
  begin
   if (MousePos.X <> MouseLDownPos.X) and (MousePos.Y <> MouseLDownPos.Y) then
   case MouseAction of
    MOUSEACTION_DRAWPANEL:
    begin
     if (cbPanelType.ItemIndex in [1, 2]) and (lbTextureList.ItemIndex = -1) then
      MessageBox(0, 'Выберите текстуру', 'Ошибка', MB_ICONINFORMATION or MB_OK
                 or MB_TASKMODAL or MB_DEFBUTTON1)
     else
      begin
       case cbPanelType.ItemIndex of
        0: Panel.PanelType := PANEL_WALL;
        1: Panel.PanelType := PANEL_BACK;
        2: Panel.PanelType := PANEL_FORE;
        3: Panel.PanelType := PANEL_CLOSEDOOR;
        4: Panel.PanelType := PANEL_OPENDOOR;
        5: Panel.PanelType := PANEL_STEP;
        6: Panel.PanelType := PANEL_WATER;
        7: Panel.PanelType := PANEL_ACID1;
        8: Panel.PanelType := PANEL_ACID2;
        9: Panel.PanelType := PANEL_LIFTUP;
        10: Panel.PanelType := PANEL_LIFTDOWN;
        11: Panel.PanelType := PANEL_BLOCKMON;
       end;

       Panel.X := Min(MousePos.X-MapOffset.X, MouseLDownPos.X-MapOffset.X);
       Panel.Y := Min(MousePos.Y-MapOffset.Y, MouseLDownPos.Y-MapOffset.Y);
       Panel.Width := Abs(MousePos.X-MouseLDownPos.X);
       Panel.Height := Abs(MousePos.Y-MouseLDownPos.Y);

       if (cbPanelType.ItemIndex in [9, 10, 11]) or
          (lbTextureList.ItemIndex = -1) then
       begin
        Panel.TextureHeight := 1;
        Panel.TextureWidth := 1;
        Panel.TextureName := '';
        Panel.TextureID := TEXTURE_SPECIAL_NONE;
       end
        else
       begin
        Panel.TextureName := SelectedTexture();

        if not IsSpecialTextureSel() then
        begin
         g_GetTextureSizeByName(Panel.TextureName, Panel.TextureWidth, Panel.TextureHeight);
         g_GetTexture(Panel.TextureName, Panel.TextureID);
        end
         else
        begin
         Panel.TextureHeight := 1;
         Panel.TextureWidth := 1;
         Panel.TextureID := SpecialTextureID(SelectedTexture());
        end;
       end;

       Panel.Alpha := 0;
       Panel.Blending := False;

       Undo_Add(OBJECT_PANEL, AddPanel(Panel));
      end
    end;

    MOUSEACTION_DRAWTRIGGER:
    begin
     trigger.X := Min(MousePos.X-MapOffset.X, MouseLDownPos.X-MapOffset.X);
     trigger.Y := Min(MousePos.Y-MapOffset.Y, MouseLDownPos.Y-MapOffset.Y);
     trigger.Width := Abs(MousePos.X-MouseLDownPos.X);
     trigger.Height := Abs(MousePos.Y-MouseLDownPos.Y);

     trigger.Enabled := True;
     trigger.TriggerType := lbTriggersList.ItemIndex+1;
     trigger.TexturePanel := -1;
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
     if clbActivationType.Checked[6] then
      trigger.ActivateType := Trigger.ActivateType or ACTIVATE_NOPLAYER;
     if clbActivationType.Checked[7] then
      trigger.ActivateType := Trigger.ActivateType or ACTIVATE_ITEMCOLLIDE;
     if clbActivationType.Checked[8] then
      trigger.ActivateType := Trigger.ActivateType or ACTIVATE_NOITEM;

     trigger.Key := 0;

     if clbKeys.Checked[0] then trigger.Key := Trigger.Key+KEY_RED;
     if clbKeys.Checked[1] then trigger.Key := Trigger.Key+KEY_GREEN;
     if clbKeys.Checked[2] then trigger.Key := Trigger.Key+KEY_BLUE;
     if clbKeys.Checked[3] then trigger.Key := Trigger.Key+KEY_REDTEAM;
     if clbKeys.Checked[4] then trigger.Key := Trigger.Key+KEY_BLUETEAM;

     ZeroMemory(@trigger.Data.Default[0], 128);

     case trigger.TriggerType of
      TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR, TRIGGER_DOOR5,
      TRIGGER_CLOSETRAP, TRIGGER_TRAP, TRIGGER_LIFTUP, TRIGGER_LIFTDOWN,
      TRIGGER_LIFT: Trigger.Data.PanelID := -1;
      TRIGGER_TELEPORT:
      begin
       trigger.Data.TargetPoint.X := trigger.X-64;
       trigger.Data.TargetPoint.Y := trigger.Y-64;
       trigger.Data.d2d_teleport := True;
       trigger.Data.TlpDir := 0;
      end;
      TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF:
      begin
        trigger.Data.Count := 1;
      end;
      TRIGGER_SOUND:
      begin
       trigger.Data.Volume := 255;
       trigger.Data.Pan := 127;
       trigger.Data.PlayCount := 1;
       trigger.Data.Local := True;
       //trigger.Data.SoundDistance := 255;
       trigger.Data.SoundSwitch := False;
      end;
      TRIGGER_SPAWNMONSTER:
        begin
          trigger.Data.MonType := MONSTER_ZOMBY;
          trigger.Data.MonPos.X := trigger.X-64;
          trigger.Data.MonPos.Y := trigger.Y-64;
          trigger.Data.MonHealth := 0;
        end;
      TRIGGER_SPAWNITEM:
        begin
          trigger.Data.ItemType := ITEM_AMMO_BULLETS;
          trigger.Data.ItemPos.X := trigger.X-64;
          trigger.Data.ItemPos.Y := trigger.Y-64;
          trigger.Data.ItemFalls := False;
        end;
     end;

     Undo_Add(OBJECT_TRIGGER, AddTrigger(trigger));
    end;

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
 end
  else
 begin
  if MouseAction in [MOUSEACTION_MOVEOBJ, MOUSEACTION_RESIZE] then
  begin
   FillProperty();
   MouseAction := MOUSEACTION_NONE;
   Exit;
  end;

  if SelectFlag <> SELECTFLAG_NONE then Exit;

  if (MousePos.X <> MouseRDownPos.X) and (MousePos.Y <> MouseRDownPos.Y) then
  begin
   rSelectRect := True;

   rRect.X := Min(MousePos.X, MouseRDownPos.X)-MapOffset.X;
   rRect.Y := Min(MousePos.Y, MouseRDownPos.Y)-MapOffset.Y;
   rRect.Width := Abs(MousePos.X-MouseRDownPos.X);
   rRect.Height := Abs(MousePos.Y-MouseRDownPos.Y);
  end
   else
  begin
   rSelectRect := False;

   rRect.X := MousePos.X-MapOffset.X-1;
   rRect.Y := MousePos.Y-MapOffset.Y-1;
   rRect.Width := 2;
   rRect.Height := 2;
  end;

  if not (ssCtrl in Shift) then RemoveSelect();

  IDArray := ObjectInRect(rRect.X, rRect.Y, rRect.Width, rRect.Height,
                          pcObjects.ActivePageIndex+1, rSelectRect);

  if IDArray <> nil then
   for i := 0 to High(IDArray) do
    SelectObject(pcObjects.ActivePageIndex+1, IDArray[i], (ssCtrl in Shift) or rSelectRect);

  FillProperty();
 end;
end;

procedure TMainForm.RenderPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  sX, sY: Integer;
  dWidth, dHeight: Integer;
  _id: Integer;
begin
 sX := 1;
 sY := 1;

 _id := GetFirstSelected();
 if (MouseAction = MOUSEACTION_DRAWPANEL) and
    (cbPanelType.ItemIndex in [0..8]) and
    (lbTextureList.ItemIndex <> -1) and not IsSpecialTextureSel() then
 begin
  sX := StrToIntDef(lTextureWidth.Caption, DotStep);
  sY := StrToIntDef(lTextureHeight.Caption, DotStep);
 end
  else if (MouseAction = MOUSEACTION_RESIZE) and
          ((SelectedObjects[_id].ObjectType = OBJECT_PANEL) and
           IsTexturedPanel(gPanels[SelectedObjects[_id].ID].PanelType) and
           (gPanels[SelectedObjects[_id].ID].TextureName <> '') and
           (not IsSpecialTexture(gPanels[SelectedObjects[_id].ID].TextureName))) then
 begin
  sX := gPanels[SelectedObjects[_id].ID].TextureWidth;
  sY := gPanels[SelectedObjects[_id].ID].TextureHeight;
 end
  else if SnapToGrid then
 begin
  sX := DotStep;
  sY := DotStep;
 end;

 if MouseLDown then
 begin
  MousePos.X := (Round((X-MouseLDownPos.X)/sX)*sX)+MouseLDownPos.X;
  MousePos.Y := (Round((Y-MouseLDownPos.Y)/sY)*sY)+MouseLDownPos.Y;
 end
  else if MouseRDown then
 begin
  MousePos.X := (Round((X-MouseRDownPos.X)/sX)*sX)+MouseRDownPos.X;
  MousePos.Y := (Round((Y-MouseRDownPos.Y)/sY)*sY)+MouseRDownPos.Y;
 end
  else
 begin
  MousePos.X := (Round(X/sX)*sX);
  MousePos.Y := (Round(Y/sY)*sY);
 end;

 if ResizeType = RESIZETYPE_NONE then RenderPanel.Cursor := crDefault;

 if (not MouseLDown) and (MouseRDown) then
 begin
  if MouseAction = MOUSEACTION_NONE then
  begin
   if DrawRect = nil then New(DrawRect);
   DrawRect.TopLeft := MouseRDownPos;
   DrawRect.BottomRight := MousePos;
  end
   else if MouseAction = MOUSEACTION_MOVEOBJ then
  begin
   MoveSelectedObjects(ssShift in Shift, ssCtrl in Shift, MousePos.X-LastMovePoint.X, MousePos.Y-LastMovePoint.Y);
  end
   else if MouseAction = MOUSEACTION_RESIZE then
  begin
   if (SelectedObjectCount = 1) and (SelectedObjects[GetFirstSelected].Live) then
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

    ResizeObject(SelectedObjects[GetFirstSelected].ObjectType, SelectedObjects[GetFirstSelected].ID,
                 dWidth, dHeight, ResizeDirection);
                  
    LastMovePoint := MousePos;
   end;
  end;
 end;

 if (not MouseRDown) and (MouseLDown) then
 begin                                
  if MouseAction in [MOUSEACTION_DRAWPANEL, MOUSEACTION_DRAWTRIGGER, MOUSEACTION_DRAWPRESS] then
  begin
   if DrawRect = nil then New(DrawRect);
   DrawRect.TopLeft := MouseLDownPos;
   DrawRect.BottomRight := MousePos;
  end
   else if MouseAction = MOUSEACTION_MOVEMAP then
  begin
   MoveMap(X, Y);
  end;
 end;

 if not (MouseRDown or MouseLDown) then DrawRect := nil;

 StatusBar.Panels[1].Text := Format('(%d:%d)', [MousePos.X-MapOffset.X, MousePos.Y-MapOffset.Y]);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 CanClose := MessageBox(0, 'Уже уходите?', 'Выход', MB_ICONQUESTION or MB_YESNO
                        or MB_TASKMODAL or MB_DEFBUTTON1) = idYes;
end;

procedure TMainForm.aExitExecute(Sender: TObject);
begin
 Close;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  config: TConfig;
  i: Integer;
begin
 config := TConfig.CreateFile(EditorDir+'\editor.cfg');
 config.WriteBool('Editor', 'Maximize', WindowState = wsMaximized);
 config.WriteBool('Editor', 'Minimap', ShowMap);

 for i := 0 to MAX_RECENT_FILES-1 do
   if i < RecentFiles.Count then
     config.WriteStr('RecentFiles', IntToStr(i+1), RecentFiles[i])
   else
     config.WriteStr('RecentFiles', IntToStr(i+1), '');
 RecentFiles.Free;

 config.SaveFile(EditorDir+'\editor.cfg');
 config.Destroy;

 wglDeleteContext(hRC);
end;

procedure TMainForm.sbHorizontalChange(Sender: TObject);
begin
 MapOffset.X := -Round(sbHorizontal.Position/16)*16;
end;

procedure TMainForm.sbVerticalChange(Sender: TObject);
begin
 MapOffset.Y := -Round(sbVertical.Position/16)*16;
end;

procedure TMainForm.RenderPanelResize(Sender: TObject);
begin
 if MainForm.Visible then MainForm.Resize;
end;

procedure TMainForm.aMapOptionsExecute(Sender: TObject);
begin
 MapOptionsForm.ShowModal;
end;

procedure TMainForm.aAboutExecute(Sender: TObject);
begin
 AboutForm.ShowModal;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  dx, dy: Integer;
begin
 if (Key = VK_DELETE) and (SelectedObjects <> nil) and
 RenderPanel.Focused then DeleteSelectedObjects();
 if (Key = VK_ESCAPE) and (SelectedObjects <> nil) then RemoveSelect();

 if MainForm.ActiveControl = RenderPanel then
 begin
  dx := 0;
  dy := 0;

  if Key = VK_NUMPAD4 then dx := IfThen(ssAlt in Shift, -1, -DotStep);
  if Key = VK_NUMPAD6 then dx := IfThen(ssAlt in Shift, 1, DotStep);
  if Key = VK_NUMPAD8 then dy := IfThen(ssAlt in Shift, -1, -DotStep);
  if Key = VK_NUMPAD5 then dy := IfThen(ssAlt in Shift, 1, DotStep);

  if (dx <> 0) or (dy <> 0) then
  begin
   MoveSelectedObjects(ssShift in Shift, ssCtrl in Shift, dx, dy);
   Key := 0;
  end;
 end;

 with sbVertical do
 begin
  if Key = Ord('W') then Position := ifThen(Position > DotStep, Position-DotStep, 0);
  if Key = Ord('S') then Position := ifThen(Position+DotStep < Max, Position+DotStep, Max);
 end;

 with sbHorizontal do
 begin
  if Key = Ord('A') then Position := ifThen(Position > DotStep, Position-DotStep, 0);
  if Key = Ord('D') then Position := ifThen(Position+DotStep < Max, Position+DotStep, Max);
 end;
end;

procedure TMainForm.aOptimizeExecute(Sender: TObject);
begin
 RemoveSelect();
 MapOptimizationForm.Show;
end;

procedure TMainForm.aCheckMapExecute(Sender: TObject);
begin
 MapCheckForm.Show;
end;

procedure TMainForm.bbAddTextureClick(Sender: TObject);
begin
  AddTextureForm.lbResourcesList.MultiSelect := True;
  AddTextureForm.ShowModal();
end;

procedure TMainForm.lbTextureListClick(Sender: TObject);
var
  TextureWidth, TextureHeight: Word;
begin
 if (lbTextureList.ItemIndex <> -1) and not IsSpecialTextureSel() then
 begin
  g_GetTextureSizeByName(SelectedTexture(), TextureWidth, TextureHeight);

  lTextureWidth.Caption := IntToStr(TextureWidth);
  lTextureHeight.Caption := IntToStr(TextureHeight);
 end
  else
 begin
  lTextureWidth.Caption := '';
  lTextureHeight.Caption := '';
 end;
end;

procedure TMainForm.vleObjectPropertyGetPickList(Sender: TObject;
  const KeyName: string; Values: TStrings);
var
  i: Integer;

begin
 if vleObjectProperty.ItemProps[KeyName].EditStyle = esPickList then
 begin
  if KeyName = 'Тип панели' then
  begin
   Values.Add(GetPanelName(PANEL_WALL));
   Values.Add(GetPanelName(PANEL_BACK));
   Values.Add(GetPanelName(PANEL_FORE));
   Values.Add(GetPanelName(PANEL_CLOSEDOOR));
   Values.Add(GetPanelName(PANEL_OPENDOOR));
   Values.Add(GetPanelName(PANEL_STEP));
   Values.Add(GetPanelName(PANEL_WATER));
   Values.Add(GetPanelName(PANEL_ACID1));
   Values.Add(GetPanelName(PANEL_ACID2));
   Values.Add(GetPanelName(PANEL_LIFTUP));
   Values.Add(GetPanelName(PANEL_LIFTDOWN));
   Values.Add(GetPanelName(PANEL_BLOCKMON));
  end
   else if KeyName = 'Направление' then
  begin
   Values.Add(DirNames[D_LEFT]);
   Values.Add(DirNames[D_RIGHT]);
  end
   else if KeyName = 'Направление после' then
  begin
   Values.Add(DirNamesAdv[0]);
   Values.Add(DirNamesAdv[1]);
   Values.Add(DirNamesAdv[2]);
  end
   else if (KeyName = 'Только DM') or (KeyName = 'Падает') or (KeyName = 'Смешивание') or
           (KeyName = 'Один раз') or (KeyName = 'Включен') or (KeyName = 'Без звука') or
           (KeyName = 'Аним. один раз') or (KeyName = 'Как в D2D') or (KeyName = 'Локальный') or
           (KeyName = 'Цикл') or (KeyName = 'Тихий телепорт') or (KeyName = 'Телепорт итемов') or
           (KeyName = 'Переключение') then
  begin
   Values.Add(BoolNames[True]);
   Values.Add(BoolNames[False]);
  end;
 end;
end;

procedure TMainForm.bApplyPropertyClick(Sender: TObject);
var
  _id, a, r, c: Integer;
  s: string;
begin
 if SelectedObjectCount() <> 1 then Exit;
 if not SelectedObjects[GetFirstSelected()].Live then Exit;
 try
  if not CheckProperty() then Exit;
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
    X := StrToInt(Trim(vleObjectProperty.Values['X']));
    Y := StrToInt(Trim(vleObjectProperty.Values['Y']));
    Width := StrToInt(Trim(vleObjectProperty.Values['Длина']));
    Height := StrToInt(Trim(vleObjectProperty.Values['Высота']));

    if IsTexturedPanel(PanelType) then
    begin
     PanelType := GetPanelType(vleObjectProperty.Values['Тип панели']);

     if TextureName <> '' then
     begin
      Alpha := StrToInt(Trim(vleObjectProperty.Values['Прозрачность']));
      Blending := NameToBool(vleObjectProperty.Values['Смешивание']);
     end;

     if not IsTexturedPanel(PanelType) then
     begin
      Alpha := 0;
      Blending := False;
      TextureName := '';
     end;

     if not WordBool(PanelType and (PANEL_WALL or PANEL_FORE or PANEL_BACK)) then
      if gTriggers <> nil then
       for a := 0 to High(gTriggers) do
        if (gTriggers[a].TriggerType <> 0) and
           (gTriggers[a].TexturePanel = Integer(SelectedObjects[_id].ID)) then
         gTriggers[a].TexturePanel := -1;

     if not WordBool(PanelType and (PANEL_LIFTUP or PANEL_LIFTDOWN)) then
      if gTriggers <> nil then
       for a := 0 to High(gTriggers) do
         if (gTriggers[a].TriggerType in [TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT]) and
            (gTriggers[a].Data.PanelID = Integer(SelectedObjects[_id].ID)) then gTriggers[a].Data.PanelID := -1;

     if not WordBool(PanelType and (PANEL_OPENDOOR or PANEL_CLOSEDOOR)) then
      if gTriggers <> nil then
       for a := 0 to High(gTriggers) do
         if (gTriggers[a].TriggerType in [TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR,
                                          TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP]) and
            (gTriggers[a].Data.PanelID = Integer(SelectedObjects[_id].ID)) then gTriggers[a].Data.PanelID := -1;
    end;
   end;
  end;

  OBJECT_ITEM:
  begin
   with gItems[SelectedObjects[_id].ID] do
   begin
    X := StrToInt(Trim(vleObjectProperty.Values['X']));
    Y := StrToInt(Trim(vleObjectProperty.Values['Y']));
    OnlyDM := NameToBool(vleObjectProperty.Values['Только DM']);
    Fall := NameToBool(vleObjectProperty.Values['Падает']);
   end;
  end;
  OBJECT_MONSTER:
  begin
   with gMonsters[SelectedObjects[_id].ID] do
   begin
    X := StrToInt(Trim(vleObjectProperty.Values['X']));
    Y := StrToInt(Trim(vleObjectProperty.Values['Y']));
    Direction := NameToDir(vleObjectProperty.Values['Направление']);
   end;
  end;
  OBJECT_AREA:
  begin
   with gAreas[SelectedObjects[_id].ID] do
   begin
    X := StrToInt(Trim(vleObjectProperty.Values['X']));
    Y := StrToInt(Trim(vleObjectProperty.Values['Y']));
    Direction := NameToDir(vleObjectProperty.Values['Направление']);
   end;
  end;
  OBJECT_TRIGGER:
  begin
   with gTriggers[SelectedObjects[_id].ID] do
   begin
    X := StrToInt(Trim(vleObjectProperty.Values['X']));
    Y := StrToInt(Trim(vleObjectProperty.Values['Y']));
    Width := StrToInt(Trim(vleObjectProperty.Values['Длина']));
    Height := StrToInt(Trim(vleObjectProperty.Values['Высота']));
    Enabled := NameToBool(vleObjectProperty.Values['Включен']);
    ActivateType := StrToActivate(vleObjectProperty.Values['Тип активации']);
    Key := StrToKey(vleObjectProperty.Values['Ключи']);

    case TriggerType of
     TRIGGER_EXIT:
     begin
      s := vleObjectProperty.Values['След. карта'];

      ZeroMemory(@Data.MapName[0], 16);
      if s <> '' then CopyMemory(@Data.MapName[0], @s[1], Min(Length(s), 16));
     end;
     TRIGGER_TEXTURE:
     begin
      Data.ActivateOnce := NameToBool(vleObjectProperty.Values['Один раз']);
      Data.AnimOnce := NameToBool(vleObjectProperty.Values['Аним. один раз']);
     end;
     TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF:
     begin
      Data.Wait := Min(StrToIntDef(vleObjectProperty.Values['Задержка'], 0), 65535);
      Data.Count := Min(StrToIntDef(vleObjectProperty.Values['Счетчик'], 0), 65535);
      if Data.Count < 1 then Data.Count := 1;
     end;
     TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR, TRIGGER_DOOR5,
     TRIGGER_CLOSETRAP, TRIGGER_TRAP, TRIGGER_LIFTUP, TRIGGER_LIFTDOWN,
     TRIGGER_LIFT:
     begin
      Data.NoSound := NameToBool(vleObjectProperty.Values['Без звука']);
      Data.d2d_doors := NameToBool(vleObjectProperty.Values['Как в D2D']);
     end;
     TRIGGER_TELEPORT:
     begin
      Data.d2d_teleport := NameToBool(vleObjectProperty.Values['Как в D2D']);
      Data.silent_teleport := NameToBool(vleObjectProperty.Values['Тихий телепорт']);
      Data.TlpDir := NameToDirAdv(vleObjectProperty.Values['Направление после']);
      //Data.items_teleport := NameToBool(vleObjectProperty.Values['Телепорт итемов']);
     end;
     TRIGGER_SOUND:
     begin
      s := vleObjectProperty.Values['Звук'];
      ZeroMemory(@Data.SoundName[0], 64);
      if s <> '' then CopyMemory(@Data.SoundName[0], @s[1], Min(Length(s), 64));

      Data.Volume := Min(StrToIntDef(vleObjectProperty.Values['Громкость'], 0), 255);
      Data.Pan := Min(StrToIntDef(vleObjectProperty.Values['Стерео'], 0), 255);
      Data.PlayCount := Min(StrToIntDef(vleObjectProperty.Values['Цикл'], 0), 255);
      Data.Local := NameToBool(vleObjectProperty.Values['Локальный']);
      //Data.SoundDistance := Min(StrToIntDef(vleObjectProperty.Values['Дистанция'], 0), 255);
      Data.SoundSwitch := NameToBool(vleObjectProperty.Values['Переключение']);
     end;
     TRIGGER_SPAWNMONSTER:
       begin
         Data.MonType := StrToMonster(vleObjectProperty.Values['Тип монстра']);
         Data.MonDir := Byte(NameToDir(vleObjectProperty.Values['Направление']));
         Data.MonHealth := Min(StrToIntDef(vleObjectProperty.Values['Здоровье'], 0), 1000000);
         if Data.MonHealth < 0 then
           Data.MonHealth := 0;
       end;
     TRIGGER_SPAWNITEM:
       begin
         Data.ItemType := StrToItem(vleObjectProperty.Values['Тип предмета']);
         Data.ItemFalls := NameToBool(vleObjectProperty.Values['Падает']);
       end;
     TRIGGER_MUSIC:
       begin
         s := vleObjectProperty.Values['Музыка'];
         ZeroMemory(@Data.MusicName[0], 64);
         if s <> '' then
           CopyMemory(@Data.MusicName[0], @s[1], Min(Length(s), 64));
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
  a: Integer;
begin
 if lbTextureList.ItemIndex = -1 then Exit;
 //if IsSpecialTextureSel() then Exit;

 if MessageBox(0, PChar(Format('Удалить текстуру %s ?', [SelectedTexture()])),
               'Удалить текстуру', MB_ICONQUESTION or MB_YESNO or MB_TASKMODAL
               or MB_DEFBUTTON1) <> idYes then Exit;

 if gPanels <> nil then
  for a := 0 to High(gPanels) do
   if (gPanels[a].PanelType <> 0) and
      (gPanels[a].TextureName = SelectedTexture()) then
   begin
    MessageBox(0, 'Нельзя удалить текстуру пока она используется. Удалите все '+
               'панели с этой текстурой.', 'Нельзя', MB_ICONINFORMATION or MB_OK
               or MB_TASKMODAL or MB_DEFBUTTON1);
    Exit;
   end;

 g_DeleteTexture(SelectedTexture());
 lbTextureList.DeleteSelected;
end;

procedure TMainForm.aOpenINIExecute(Sender: TObject);
begin
 OpenDialog.Filter := 'Doom2D: Forever maps (*.ini)|*.ini|All files (*.*)|*.*';

 if OpenDialog.Execute then
 begin
  FullClear();

  pLoadProgress.Left := (RenderPanel.Width div 2)-(pLoadProgress.Width div 2);
  pLoadProgress.Top := (RenderPanel.Height div 2)-(pLoadProgress.Height div 2);
  pLoadProgress.Show;

  LoadMapOld(OpenDialog.FileName);

  MainForm.Caption := Format('%s - %s', [FormCaption, ExtractFileName(OpenDialog.FileName)]);
                                             
  pLoadProgress.Hide;
  MainForm.FormResize(Self);
 end;
end;

procedure TMainForm.aNewMapExecute(Sender: TObject);
begin
 if (MessageBox(0, 'Очистить всю карту ?', 'Новая карта', MB_ICONQUESTION or
     MB_YESNO or MB_TASKMODAL or MB_DEFBUTTON1) = mrYes) then FullClear();
end;

procedure TMainForm.aUndoExecute(Sender: TObject);
var
  a: Integer;
begin
 if UndoBuffer = nil then Exit;
 if UndoBuffer[High(UndoBuffer)] = nil then Exit;

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

 RemoveSelect();

 miUndo.Enabled := UndoBuffer <> nil;
end;

procedure TMainForm.aCopyObjectExecute(Sender: TObject);
var
  a, i: Integer;
begin
 if SelectedObjects = nil then Exit;

 CopyBuffer := nil;

 for a := 0 to High(SelectedObjects) do
  if SelectedObjects[a].Live then
  with SelectedObjects[a] do
  begin
   SetLength(CopyBuffer, Length(CopyBuffer)+1);
   i := High(CopyBuffer);
   
   case ObjectType of
    OBJECT_PANEL:
    begin
     CopyBuffer[i].ObjectType := OBJECT_PANEL;
     New(CopyBuffer[i].Panel);
     CopyBuffer[i].Panel^ := gPanels[ID];
    end;

    OBJECT_ITEM:
    begin
     CopyBuffer[i].ObjectType := OBJECT_ITEM;
     CopyBuffer[i].Item := gItems[ID];
    end;

    OBJECT_MONSTER:
    begin
     CopyBuffer[i].ObjectType := OBJECT_MONSTER;
     CopyBuffer[i].Monster := gMonsters[ID];
    end;

    OBJECT_AREA:
    begin
     CopyBuffer[i].ObjectType := OBJECT_AREA;
     CopyBuffer[i].Area := gAreas[ID];
    end;

    OBJECT_TRIGGER:
    begin
     CopyBuffer[i].ObjectType := OBJECT_TRIGGER;
     CopyBuffer[i].Trigger := gTriggers[ID];
    end;
   end;
  end;

 miPaste.Enabled := CopyBuffer <> nil;
end;

procedure TMainForm.aPasteObjectExecute(Sender: TObject);
var
  a, h: Integer;
  ID: DWORD;
begin
 if CopyBuffer = nil then Exit;

 RemoveSelect();

 h := High(CopyBuffer);
 for a := h downto 0 do
 with CopyBuffer[a] do
 begin
  case ObjectType of
   OBJECT_PANEL:
   begin
    Panel^.X := Panel^.X+16;
    Panel^.Y := Panel^.Y+16;
    //Panel^.X := ((sbHorizontal.Position+RenderPanel.Width div 2) div 16)*16;
    //Panel^.Y := ((sbVertical.Position+RenderPanel.Height div 2) div 16)*16;
    ID := AddPanel(Panel^);
    Undo_Add(OBJECT_PANEL, ID, a < h);
    SelectObject(OBJECT_PANEL, ID, True);
   end;

   OBJECT_ITEM:
   begin
    Item.X := Item.X+16;
    Item.Y := Item.Y+16;
    //Item.X := ((sbHorizontal.Position+RenderPanel.Width div 2) div 16)*16;
    //Item.Y := ((sbVertical.Position+RenderPanel.Height div 2) div 16)*16;
    ID := AddItem(Item);
    Undo_Add(OBJECT_ITEM, ID, a < h);
    SelectObject(OBJECT_ITEM, ID, True);
   end;

   OBJECT_MONSTER:
   begin
    Monster.X := Monster.X+16;
    Monster.Y := Monster.Y+16;
    //Monster.X := ((sbHorizontal.Position+RenderPanel.Width div 2) div 16)*16;
    //Monster.Y := ((sbVertical.Position+RenderPanel.Height div 2) div 16)*16;
    ID := AddMonster(Monster);
    Undo_Add(OBJECT_MONSTER, ID, a < h);
    SelectObject(OBJECT_MONSTER, ID, True);
   end;

   OBJECT_AREA:
   begin
    Area.X := Area.X+16;
    Area.Y := Area.Y+16;
    //Area.X := ((sbHorizontal.Position+RenderPanel.Width div 2) div 16)*16;
    //Area.Y := ((sbVertical.Position+RenderPanel.Height div 2) div 16)*16;
    ID := AddArea(Area);
    Undo_Add(OBJECT_AREA, ID, a < h);
    SelectObject(OBJECT_AREA, ID, True);
   end;

   OBJECT_TRIGGER:
   begin
    Trigger.X := Trigger.X+16;
    Trigger.Y := Trigger.Y+16;
    //Trigger.X := ((sbHorizontal.Position+RenderPanel.Width div 2) div 16)*16;
    //Trigger.Y := ((sbVertical.Position+RenderPanel.Height div 2) div 16)*16;
    Trigger.TexturePanel := -1;
    ID := AddTrigger(Trigger);
    Undo_Add(OBJECT_TRIGGER, ID, a < h);
    SelectObject(OBJECT_TRIGGER, ID, True);
   end;
  end;
 end;
end;

procedure TMainForm.aCutObjectExecute(Sender: TObject);
begin
 miCopy.Click;
 DeleteSelectedObjects();
end;

procedure TMainForm.vleObjectPropertyEditButtonClick(Sender: TObject);
var
  Key, FileName: string;
  c: Cardinal;
  w: Word;
  b: Byte;
begin
 Key := vleObjectProperty.Keys[vleObjectProperty.Row];

 if Key = 'Точка телепорта' then SelectFlag := SELECTFLAG_TELEPORT
 else if Key = 'Точка появления' then SelectFlag := SELECTFLAG_SPAWNPOINT
 else if (Key = 'Панель двери') or (Key = 'Панель ловушки') then SelectFlag := SELECTFLAG_DOOR
 else if Key = 'Панель с текстурой' then SelectFlag := SELECTFLAG_TEXTURE
 else if Key = 'Панель лифта' then SelectFlag := SELECTFLAG_LIFT
 else if key = 'ID монстра' then SelectFlag := SELECTFLAG_MONSTER
 else if Key = 'Область воздействия' then DrawPressRect := True
 else if Key = 'След. карта' then
 begin
  g_ProcessResourceStr(OpenedMap, @FileName, nil, nil);
  SelectMapForm.GetMaps(FileName); 
  if SelectMapForm.ShowModal = mrOK then
    begin
      vleObjectProperty.Values[Key] := SelectMapForm.lbMapList.Items[SelectMapForm.lbMapList.ItemIndex];
      bApplyProperty.Click();
    end;
 end
 else if (Key = 'Звук') or (Key = 'Музыка') then
 begin
  AddSoundForm.OKFunction := nil;
  AddSoundForm.lbResourcesList.MultiSelect := False;
  AddSoundForm.SetResource := vleObjectProperty.Values[Key];
  AddSoundForm.ShowModal();
  if AddSoundForm.ResourceName <> '' then
    begin
      vleObjectProperty.Values[Key] := AddSoundForm.ResourceName;
      bApplyProperty.Click();
    end;
 end
 else if Key = 'Тип активации' then
  with ActivationTypeForm, vleObjectProperty do
  begin
   cbPlayerCollide.Checked := Pos('PC', Values[Key]) > 0;
   cbMonsterCollide.Checked := Pos('MC', Values[Key]) > 0;
   cbPlayerPress.Checked := Pos('PP', Values[Key]) > 0;
   cbMonsterPress.Checked := Pos('MP', Values[Key]) > 0;
   cbShot.Checked := Pos('SH', Values[Key]) > 0;
   cbNoMonster.Checked := Pos('NM', Values[Key]) > 0;
   cbNoPlayer.Checked := Pos('NP', Values[Key]) > 0;
   cbItemCollide.Checked := Pos('IC', Values[Key]) > 0;
   cbNoItem.Checked := Pos('NI', Values[Key]) > 0;

   if ShowModal = mrOK then
   begin
    c := 0;
    if cbPlayerCollide.Checked then c := ACTIVATE_PLAYERCOLLIDE;
    if cbMonsterCollide.Checked then c := c or ACTIVATE_MONSTERCOLLIDE;
    if cbPlayerPress.Checked then c := c or ACTIVATE_PLAYERPRESS;
    if cbMonsterPress.Checked then c := c or ACTIVATE_MONSTERPRESS;
    if cbShot.Checked then c := c or ACTIVATE_SHOT;
    if cbNoMonster.Checked then c := c or ACTIVATE_NOMONSTER;
    if cbNoPlayer.Checked then c := c or ACTIVATE_NOPLAYER;
    if cbItemCollide.Checked then c := c or ACTIVATE_ITEMCOLLIDE;
    if cbNoItem.Checked then c := c or ACTIVATE_NOITEM;
    Values[Key] := ActivateToStr(c);
    bApplyProperty.Click();
   end;
 end
 else if Key = 'Ключи' then
  with KeysForm, vleObjectProperty do
  begin
   cbRedKey.Checked := Pos('RK', Values[Key]) > 0;
   cbGreenKey.Checked := Pos('GK', Values[Key]) > 0;
   cbBlueKey.Checked := Pos('BK', Values[Key]) > 0;
   cbRedTeam.Checked := Pos('RT', Values[Key]) > 0;
   cbBlueTeam.Checked := Pos('BT', Values[Key]) > 0;
   if ShowModal = mrOK then
   begin
    w := 0;
    if cbRedKey.Checked then w := KEY_RED;
    if cbGreenKey.Checked then w := w or KEY_GREEN;
    if cbBlueKey.Checked then w := w or KEY_BLUE;
    if cbRedTeam.Checked then w := w or KEY_REDTEAM;
    if cbBlueTeam.Checked then w := w or KEY_BLUETEAM;
    Values[Key] := KeyToStr(w);
    bApplyProperty.Click();
   end;
  end
 else if Key = 'Тип монстра' then
   with ChooseTypeForm, vleObjectProperty do
     begin
       Caption := 'Выберите тип монстра';
       lbTypeSelect.Items.Clear;

       for b := MONSTER_DEMON to MONSTER_MAN do
         lbTypeSelect.Items.Add(MonsterToStr(b));

       lbTypeSelect.ItemIndex := StrToMonster(Values[Key]) - MONSTER_DEMON;

       if ShowModal = mrOK then
         begin
           b := lbTypeSelect.ItemIndex + MONSTER_DEMON;
           Values[Key] := MonsterToStr(b);
           bApplyProperty.Click();
         end;
     end
 else if Key = 'Тип предмета' then
   with ChooseTypeForm, vleObjectProperty do
     begin
       Caption := 'Выберите тип предмета';
       lbTypeSelect.Items.Clear;

       for b := ITEM_MEDKIT_SMALL to ITEM_KEY_BLUE do
         lbTypeSelect.Items.Add(ItemToStr(b));

       lbTypeSelect.ItemIndex := StrToItem(Values[Key]) - ITEM_MEDKIT_SMALL;

       if ShowModal = mrOK then
         begin
           b := lbTypeSelect.ItemIndex + ITEM_MEDKIT_SMALL;
           Values[Key] := ItemToStr(b);
           bApplyProperty.Click();
         end;
     end;
end;

procedure TMainForm.aSaveMapExecute(Sender: TObject);
var
  FileName, Section, Res: string;
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
 OpenDialog.Filter := 'Карты Doom2D: Forever (*.wad)|*.wad|All files (*.*)|*.*';

 if OpenDialog.Execute then OpenMap(OpenDialog.FileName, '');
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
 MainForm.ActiveControl := RenderPanel;
end;

procedure TMainForm.aDeleteMap(Sender: TObject);
var
  WAD: TWADEditor_1;
  MapList: SArray;
  MapName: Char16;
  a: Integer;
  str: string;
begin
 OpenDialog.Filter := 'Карты Doom2D: Forever (*.wad)|*.wad|All files (*.*)|*.*';

 if not OpenDialog.Execute then Exit;

 WAD := TWADEditor_1.Create;

 if not WAD.ReadFile(OpenDialog.FileName) then
 begin
  WAD.Destroy;
  Exit;
 end;

 WAD.CreateImage;

 MapList := WAD.GetResourcesList('');

 SelectMapForm.lbMapList.Items.Clear;

 if MapList <> nil then
  for a := 0 to High(MapList) do
   SelectMapForm.lbMapList.Items.Add(MapList[a]);

 if (SelectMapForm.ShowModal = mrOK) then
 begin
  str := SelectMapForm.lbMapList.Items[SelectMapForm.lbMapList.ItemIndex];
  MapName := '';
  CopyMemory(@MapName[0], @str[1], Min(16, Length(str)));

  if MessageBox(0, PChar(Format('Удалить карту %s из %s ?', [MapName, OpenDialog.FileName])),
                'Удалить карту', MB_ICONQUESTION or MB_YESNO or MB_TASKMODAL or
                MB_DEFBUTTON2) <> mrYes then Exit;

  WAD.RemoveResource('', MapName);
  MessageBox(0, PChar(Format('Карта %s удалена', [MapName])), 'Карта удалена',
             MB_ICONINFORMATION or MB_OK or MB_TASKMODAL or MB_DEFBUTTON1);

  WAD.SaveTo(OpenDialog.FileName);
 end;

 WAD.Destroy;
end;

procedure TMainForm.vleObjectPropertyKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
 if Key = VK_RETURN then bApplyProperty.Click();
end;

procedure MovePanel(var ID: DWORD; MoveType: Byte);
var
  _id: Integer;
  a: Integer;
begin
 if (ID = 0) and (MoveType = 0) then Exit;
 if (ID = DWORD(High(gPanels))) and (MoveType <> 0) then Exit;

 _id := Integer(ID);

 SetLength(gPanels, Length(gPanels)+1);

 if MoveType = 0 then
 begin
  for a := High(gPanels)-1 downto 0 do
   gPanels[a+1] := gPanels[a];
  gPanels[0] := gPanels[ID+1];
  gPanels[ID+1].PanelType := PANEL_NONE;

  if gTriggers <> nil then
  for a := 0 to High(gTriggers) do
   with gTriggers[a] do
   begin
    if TriggerType = TRIGGER_NONE then Continue;

    if TexturePanel = _id then TexturePanel := 0
     else if TexturePanel <> -1 then TexturePanel := TexturePanel+1;

    case TriggerType of
     TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR, TRIGGER_DOOR5,
     TRIGGER_CLOSETRAP, TRIGGER_TRAP, TRIGGER_LIFTUP, TRIGGER_LIFTDOWN,
     TRIGGER_LIFT: if Data.PanelID = _id then Data.PanelID := 0
                    else if Data.PanelID <> -1 then Data.PanelID := Data.PanelID+1;
    end;
   end;

  ID := 0;
 end
  else
 begin
  if gTriggers <> nil then
  for a := 0 to High(gTriggers) do
   with gTriggers[a] do
   begin
    if TriggerType = TRIGGER_NONE then Continue;

    if TexturePanel = _id then TexturePanel := High(gPanels);

    case TriggerType of
     TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR, TRIGGER_DOOR5,
     TRIGGER_CLOSETRAP, TRIGGER_TRAP, TRIGGER_LIFTUP, TRIGGER_LIFTDOWN,
     TRIGGER_LIFT: if Data.PanelID = _id then Data.PanelID := High(gPanels);
    end;
   end;

  gPanels[High(gPanels)] := gPanels[ID];
  gPanels[ID].PanelType := PANEL_NONE;

  ID := High(gPanels);
 end;
end;

procedure TMainForm.aMoveToBack(Sender: TObject);
var
  a: Integer;
begin
 if SelectedObjects = nil then Exit;

 for a := 0 to High(SelectedObjects) do
  with SelectedObjects[a] do
   if Live and (ObjectType = OBJECT_PANEL) then MovePanel(ID, 0);
end;

procedure TMainForm.aMoveToFore(Sender: TObject);
var
  a: Integer;
begin
 if SelectedObjects = nil then Exit;

 for a := 0 to High(SelectedObjects) do
  with SelectedObjects[a] do
   if Live and (ObjectType = OBJECT_PANEL) then MovePanel(ID, 1);
end;

procedure TMainForm.aSaveMapAsExecute(Sender: TObject);
begin
 if not SaveDialog.Execute then Exit;
 SaveMapForm.GetMaps(SaveDialog.FileName);

 if SaveMapForm.ShowModal <> mrOK then Exit;

 SaveMap(SaveDialog.FileName+':\'+SaveMapForm.eMapName.Text);

 Caption := Format('%s - %s:%s', [FormCaption, ExtractFileName(SaveDialog.FileName),
                                  SaveMapForm.eMapName.Text]);

 OpenedMap := SaveDialog.FileName+':\'+SaveMapForm.eMapName.Text;
end;

procedure TMainForm.aSelectAllExecute(Sender: TObject);
var
  a: Integer;
begin
 RemoveSelect();

 case pcObjects.ActivePageIndex+1 of
  OBJECT_PANEL:
   if gPanels <> nil then
    for a := 0 to High(gPanels) do
     if gPanels[a].PanelType <> PANEL_NONE then SelectObject(OBJECT_PANEL, a, True);
  OBJECT_ITEM:
   if gItems <> nil then
    for a := 0 to High(gItems) do
     if gItems[a].ItemType <> ITEM_NONE then SelectObject(OBJECT_ITEM, a, True);
  OBJECT_MONSTER:
   if gMonsters <> nil then
    for a := 0 to High(gMonsters) do
     if gMonsters[a].MonsterType <> MONSTER_NONE then SelectObject(OBJECT_MONSTER, a, True);
  OBJECT_AREA:
   if gAreas <> nil then
    for a := 0 to High(gAreas) do
     if gAreas[a].AreaType <> AREA_NONE then SelectObject(OBJECT_AREA, a, True);
  OBJECT_TRIGGER:
   if gTriggers <> nil then
    for a := 0 to High(gTriggers) do
     if gTriggers[a].TriggerType <> TRIGGER_NONE then SelectObject(OBJECT_TRIGGER, a, True);
 end;
end;

procedure TMainForm.tbGridOnClick(Sender: TObject);
begin
 DotEnable := not DotEnable;
end;

procedure TMainForm.OnIdle(Sender: TObject; var Done: Boolean);
begin
 Draw();
end;

procedure TMainForm.miMapPreviewClick(Sender: TObject);
begin
 if not PreviewMode then
 begin
  StatusBar.Visible := False;
  Panel1.Visible := False;
  Panel3.Visible := False;
  Splitter2.Visible := False;
  Splitter1.Visible := False;
  ToolBar1.Visible := False;
  sbHorizontal.Visible := False;
  sbVertical.Visible := False;
  _DotEnabled := DotEnable;
  DotEnable := False;
 end
  else
 begin
  StatusBar.Visible := True;
  Panel1.Visible := True;
  Panel3.Visible := True;
  Splitter2.Visible := True;
  Splitter1.Visible := True;
  ToolBar1.Visible := True;
  sbHorizontal.Visible := True;
  sbVertical.Visible := True;
  DotEnable := _DotEnabled;
 end;

 PreviewMode := not PreviewMode;
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
 DotStep := 24-DotStep;
end;

procedure TMainForm.miSnapToGridClick(Sender: TObject);
begin
 SnapToGrid := not SnapToGrid;

 MousePos.X := (MousePos.X div DotStep)*DotStep;
 MousePos.Y := (MousePos.Y div DotStep)*DotStep;

 miSnapToGrid.Checked := SnapToGrid;
end;

procedure TMainForm.minexttabClick(Sender: TObject);
begin
 if pcObjects.ActivePageIndex < pcObjects.PageCount-1 then
  pcObjects.ActivePageIndex := pcObjects.ActivePageIndex+1
   else pcObjects.ActivePageIndex := 0;
end;

procedure TMainForm.miSaveMiniMapClick(Sender: TObject);
begin
 SaveMiniMapForm.ShowModal;
end;

procedure TMainForm.miLayer7Click(Sender: TObject);
begin
 SwitchLayer(LAYER_MONSTERS);
end;

procedure TMainForm.bClearTextureClick(Sender: TObject);
begin
 lbTextureList.ItemIndex := -1;
end;

procedure TMainForm.miPackMapClick(Sender: TObject);
begin
 PackMapForm.ShowModal();
end;

procedure TMainForm.miMapTestSettingsClick(Sender: TObject);
begin
  MapTestForm.ShowModal;
end;

procedure TMainForm.miTestMapClick(Sender: TObject);
var
  cmd, dir: String;
  opt: LongWord;
  si: STARTUPINFO;
  pi: PROCESS_INFORMATION;
  lpMsgBuf: PAnsiChar;

begin
  if OpenedMap = '' then
    begin
      aSaveMapAsExecute(nil);
      Exit;
    end;

  aSaveMapExecute(nil);

  opt := 0;
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

  dir := ExtractFilePath(TestD2dExe);

  cmd := '"' + TestD2dExe + '"';
  cmd := cmd + ' -map "' + OpenedMap + '"';
  cmd := cmd + ' -gmode ' + TestGameMode;
  cmd := cmd + ' -glimt ' + TestLimTime;
  cmd := cmd + ' -glimg ' + TestLimGoal;
  cmd := cmd + ' -gopt ' + IntToStr(opt);

  if TestMapOnce then
    cmd := cmd + ' +gclose';

  ZeroMemory(@si, SizeOf(si));
  si.cb := SizeOf(si);
  ZeroMemory(@pi, SizeOf(pi));

  if not CreateProcess(0, PAnsiChar(cmd),
                       nil, nil, False, 0, nil,
                       PAnsiChar(dir),
                       si, pi) then
    begin
      FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
        nil, GetLastError(), LANG_SYSTEM_DEFAULT, @lpMsgBuf, 0, nil);
      MessageBox(0, lpMsgBuf, 'Ошибка запуска', MB_OK or MB_ICONERROR);
    end;
end;

end.
