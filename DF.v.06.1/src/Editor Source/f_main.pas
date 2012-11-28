unit f_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, ValEdit, Buttons, ComCtrls, StdCtrls, dglOpenGL,
  e_graphics, e_log, IniFiles, g_textures, g_items, ToolWin, ImgList,
  ActnList, ActnCtrls, CustomizeDlg, ActnMan, ActnMenus, XPStyleActnCtrls,
  StdStyleActnCtrls, StdActns, BandActn, Grids, Math, g_basic, g_monsters,
  g_areas, g_wad, g_triggers;

type
  TPanelType=(PANEL_WALL, PANEL_BACKGROUND, PANEL_FOREGROUND, PANEL_STEP,
              PANEL_WATER, PANEL_ACID1, PANEL_ACID2);
  TGameObjectType=(GO_NONE, GO_PANEL, GO_ITEM, GO_MONSTER, GO_AREA, GO_TRIGGER);
  TUndoType=(U_PANEL, U_ITEM, U_MONSTER, U_AREA, U_TRIGGER);
  TUndoSubType=(US_CHANGE, US_DELETE);

  TUndoRec = record
   UndoType: TUndoType;
   UndoSubType: TUndoSubType;
   reserved1: Integer;
   reserved2: Integer;
   reserved3: Integer;
   reserved4: Integer;
   reserved5: Integer;
   reserved6: Integer;
   reserved7: Integer;
   reserved8: Integer;
   reserved9: String;
   reserved10: Integer;
  end;

  TBuffer = record
   ObjectType: TGameObjectType;
   reserved1: Integer;
   reserved2: Integer;
   reserved3: Integer;
   reserved4: Integer;
   reserved5: Integer;
   reserved6: Integer;
   reserved7: Integer;
   reserved8: Integer;
   reserved9: String;
  end;

  TPanelRec = packed record
   X, Y:       Integer;
   Width,
   Height:     Word;
   TextureNum: Word;
   PanelType:  Byte;
   Alpha:      Byte;
   Effects:    Byte;
  end;

  TItemRec = packed record
   X, Y:     Integer;
   ItemType: Byte;
   OnlyDM:   Boolean;
  end;

  TMonsterRec = packed record
   X, Y:        Integer;
   MonsterType: Byte;
   Active:      Boolean;
   Direction:   Byte;
  end;

  TAreaRec = packed record
   X, Y:      Integer;
   AreaType:  Byte;
   Direction: Byte;
  end;

  TTriggerRec = packed record
   X, Y:        Integer;
   Width,
   Height:      Word;
   TriggerType: Byte;
   TriggerActivateType: TTriggerActivateSet;
   TriggerKey:  TTriggerActivateKeySet;
   case Byte of
    0: (Default: Byte16);
    1: (MapName: Char16);
    2: (TeleportPoint: TPoint);
    3: (PanelID: DWORD; Direction: Byte);
  end;

  TTextureRec = packed record                            //------------//
   TextureName: Char64;                                  //Signature   //
  end;                                                   //------------//
                                                         //Version     //
  TMapRec = packed record                                //------------//
   MapName: Char16;                                      //Level Header//
   Address: LongWord;                                    //------------//
   Length: LongWord;                                     //Array MapRec//
  end;                                                   //------------//
                                                         //Maps        //
  TLevelHeaderRec = packed record                        //------------//
   MapCount: Word;
  end;

  TMapHeaderRec = packed record
   MapName:        Char32;
   MapDescription: Char256;
   MusicName:      Char64;
   Width:          Word;
   Height:         Word;
   TexturesCount:  Word;
   PanelsCount:    Word;
   ItemsCount:     Word;
   MonstersCount:  Word;
   AreasCount:     Word;
   TriggersCount:  Word;
  end;

  TNewLevelTexture = record
   TextureID:   DWORD;
   TextureName: String;
   Width,
   Height:      Word;
  end;

  TGamePanel = packed record
   X, Y:          Integer;
   Width,
   Height:        Word;
   TextureWidth,
   TextureHeight: Word;
   TextureName:   String;
   TextureID:     DWORD;
   PanelType:     TPanelType;
   Alpha:         Byte;
  end;

  TEditorPanelSystem = class(TObject)
   private
    LevelTexturesArray: Array of TNewLevelTexture;
    procedure    GetTextureByName(TextureName: String; var ID: DWORD);
    procedure    GetTextureSizeByName(TextureName: String; var Width: Word; var Height: Word);
    function     GetTexturesCount: Word;
    function     GetPanelCount: Word;
    function     FindPanel: DWORD;
   public
    PanelsArray: packed Array of TGamePanel;
    procedure    CreateGamePanel(Panel: TGamePanel);
    procedure    CreateRecPanel(Panel: TPanelRec);
    procedure    DeletePanel(ID: DWORD);
    procedure    CreateTexture(ResourceStr: String);
    procedure    DeleteTextureByName(TextureName: ShortString);
    procedure    RenderPanels(XInc, YInc: Integer; PanelType: TPanelType);
    procedure    RemoveAllTexture;
    procedure    RemoveAllPanels;
    property     TexturesCount: Word read GetTexturesCount;
    property     PanelCount: Word read GetPanelCount;
  end;

  TEditorMap = class(TObject)
   private
    FVersion:     Word;
    FMapsList:    Array of Char16;
   public
    FDescription: Char256;
    FName:        Char32;
    FMusicName:   Char64;
    FHeight:      Word;
    FWidth:       Word;
    PanelSystem:  TEditorPanelSystem;
    constructor   Create;
    destructor    Destroy; override;
    function      LoadFrom(Filename: String; MapName: String): Boolean;
    procedure     LoadOldMap(Filename: String);
    procedure     ClearMap;
    property      Version: Word read FVersion;
  end;

  TLevel = class(TObject)
   public
    X, Y:          Integer;
    Map:           TEditorMap;
    ItemSystem:    TItemSystem;
    MonsterSystem: TMonsterSystem;
    AreaSystem:    TAreaSystem;
    TriggersSystem: TTriggersSystem;
    constructor Create;
    destructor  Destroy; override;
    procedure   FreeLevel;
    procedure   Render;
  end;

  TMainForm = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    vleObjectProperty: TValueListEditor;
    OpenDialog: TOpenDialog;
    Timer1: TTimer;
    Panel2: TPanel;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    StatusBar: TStatusBar;
    Panel3: TPanel;
    ImageList: TImageList;
    ActionMainMenuBar1: TActionMainMenuBar;
    CustomizeDlg1: TCustomizeDlg;
    ActionToolBar1: TActionToolBar;
    RenderPanel: TPanel;
    SaveDialog: TSaveDialog;
    pcObjects: TPageControl;
    tsPanels: TTabSheet;
    lbTextureList: TListBox;
    Splitter2: TSplitter;
    tsItems: TTabSheet;
    lbItemList: TListBox;
    tsMonsters: TTabSheet;
    lbMonsterList: TListBox;
    tsAreas: TTabSheet;
    cbOnlyDM: TCheckBox;
    cbLockCursor: TCheckBox;
    ActionManager: TActionManager;
    aNewMap: TAction;
    aOpenMap: TAction;
    aExit: TAction;
    aEditorOptions: TAction;
    CustomizeActionBars1: TCustomizeActionBars;
    aMapOptions: TAction;
    aHelp: TAction;
    aAbout: TAction;
    rbLeft: TRadioButton;
    rbRight: TRadioButton;
    lbAreasList: TListBox;
    aDrawBackground: TAction;
    aDrawForeground: TAction;
    aDrawWall: TAction;
    aDrawStep: TAction;
    aDrawWater: TAction;
    aShowMap: TAction;
    aUndo: TAction;
    aCopyObject: TAction;
    aCutObject: TAction;
    aPasteObject: TAction;
    aCheckMap: TAction;
    aOptimize: TAction;
    rbAreaLeft: TRadioButton;
    rbAreaRight: TRadioButton;
    aSaveAsDFL: TAction;
    aOpenDFL: TAction;
    Panel4: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    lTextureHeight: TLabel;
    lTextureWidth: TLabel;
    bbAddTexture: TBitBtn;
    bbRemoveTexture: TBitBtn;
    cbPreview: TCheckBox;
    Panel5: TPanel;
    GroupBox1: TGroupBox;
    cbPanelType: TComboBox;
    aChangeDotStep: TAction;
    tsTriggers: TTabSheet;
    GroupBox2: TGroupBox;
    cbPlayerCollide: TCheckBox;
    lbTriggersList: TListBox;
    cbMonsterCollide: TCheckBox;
    GroupBox3: TGroupBox;
    cbRedKey: TCheckBox;
    cbGreenKey: TCheckBox;
    cbBlueKey: TCheckBox;
    cbPlayerPress: TCheckBox;
    Panel6: TPanel;
    bApplyProperty: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure ScrollBar2Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure aOpenMapExecute(Sender: TObject);
    procedure CustomizeActionBars1Execute(Sender: TObject);
    procedure aEditorOptionsExecute(Sender: TObject);
    procedure RenderPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RenderPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure RenderPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RenderPanelClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure aExitExecute(Sender: TObject);
    procedure aNewMapExecute(Sender: TObject);
    procedure RenderPanelResize(Sender: TObject);
    procedure bbAddTextureClick(Sender: TObject);
    procedure lbTextureListClick(Sender: TObject);
    procedure aMapOptionsExecute(Sender: TObject);
    procedure vleObjectPropertyGetPickList(Sender: TObject;
      const KeyName: String; Values: TStrings);
    procedure aAboutExecute(Sender: TObject);
    procedure aDrawBackgroundExecute(Sender: TObject);
    procedure aDrawWallExecute(Sender: TObject);
    procedure aDrawForegroundExecute(Sender: TObject);
    procedure bbRemoveTextureClick(Sender: TObject);
    procedure aDrawStepExecute(Sender: TObject);
    procedure aDrawWaterExecute(Sender: TObject);
    procedure cbPanelTypeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure aShowMapExecute(Sender: TObject);
    procedure aUndoExecute(Sender: TObject);
    procedure aCopyObjectUpdate(Sender: TObject);
    procedure aCutObjectUpdate(Sender: TObject);
    procedure aPasteObjectExecute(Sender: TObject);
    procedure aPasteObjectUpdate(Sender: TObject);
    procedure aCopyObjectExecute(Sender: TObject);
    procedure aUndoUpdate(Sender: TObject);
    procedure aCutObjectExecute(Sender: TObject);
    procedure aCheckMapExecute(Sender: TObject);
    procedure aSaveAsDFLExecute(Sender: TObject);
    procedure aOpenDFLExecute(Sender: TObject);
    procedure aOptimizeExecute(Sender: TObject);
    procedure aChangeDotStepExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure bApplyPropertyClick(Sender: TObject);
    procedure vleObjectPropertyEditButtonClick(Sender: TObject);
    procedure FillProperty(ID: DWORD; ObjectType: TGameObjectType);
  private
    { Private declarations }
  public
    { Public declarations }
  protected
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  end;

const
  MapSignature: Array[0..2] of Char = (('D'), ('F'), ('L'));
  MapVersion: Word = $0001;

var
  MainForm:       TMainForm;
  h_DC:           HDC;
  h_RC:           HGLRC;
  PointColor:     TColor = clWhite;
  PointStep:      Word = 16;
  PointEnable:    Boolean = True;
  MouseDowned:    Boolean = False;
  MouseDownPoint: TPoint;
  MousePoint:     TPoint;
  MouseDrag:      Boolean = False;
  MainLevel:      TLevel;
  SelectRect:     PRect = nil;
  SelectedID:     DWORD = 0;
  SelectedObject: TGameObjectType = GO_NONE;
  BackColor:      TColor = clBlack;
  PreviewBGColor: TColor = clLime;
  DrawBackground: Boolean;
  DrawWall:       Boolean;
  DrawForeground: Boolean;
  DrawStep:       Boolean;
  DrawWater:      Boolean;
  ShowMap:        Boolean;
  Scale:          Byte = 1;
  Moving:         Boolean = False;
  MoveObject:     TGameObjectType = GO_NONE;
  MoveObjectID:   DWORD = 0;
  EditorDir:      String;
  InitX,
  InitY:          Integer;
  MapMoving:      Boolean;
  UndoArray:      Array of TUndoRec;
  Buffer:         TBuffer;
  EditorFont:     DWORD;
  DrawTexturePanel: Boolean;
  DrawPanelSize: Boolean;
  SelectingDoor: Boolean;
  SelectTriggerID: DWORD;
  SelectingPoint: Boolean;

implementation

uses f_options, f_addtexture, f_mapoptions, f_about, f_mapcheck,
  f_mapoptimization, f_savemap, f_selectmap, f_activationtype, f_keys;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  PixelFormat: GLuint;
  pfd:         TPIXELFORMATDESCRIPTOR;
begin
 Randomize;

 GetDir(0, EditorDir);

 OpenDialog.InitialDir := EditorDir;
 SaveDialog.InitialDir := EditorDir;

 InitOpenGL;
 h_DC := GetDC(RenderPanel.Handle);

 FillChar(pfd, SizeOf(pfd), 0);
 with pfd do begin
  nSize     := SizeOf(pfd);
  nVersion  := 1;
  dwFlags   := PFD_DRAW_TO_WINDOW or
               PFD_SUPPORT_OPENGL or
               PFD_DOUBLEBUFFER;
  iPixelType:= PFD_TYPE_RGBA;
  cColorBits:= 24;
  cDepthBits:= 32;
  iLayerType:= PFD_MAIN_PLANE;
 end;
 PixelFormat := ChoosePixelFormat (h_DC, @pfd);
 SetPixelFormat (h_DC, PixelFormat, @pfd);

 h_RC := wglCreateContext(h_DC);
 ActivateRenderingContext(h_DC, h_RC);

 e_InitGL;

 e_InitLog(EditorDir+'\Editor.log', WM_NEWFILE);
 MainLevel := TLevel.Create;

 EditorFont := e_CreateFont('Arial Cyr', 12, FW_BOLD, h_DC);

 DrawForeground := True;
 DrawWall := True;
 DrawBackground := True;
 DrawStep := True;
 DrawWater := True;
 aDrawForeground.Checked := True;
 aDrawWall.Checked := True;
 aDrawBackground.Checked := True;
 aDrawStep.Checked := True;
 aDrawWater.Checked := True;
 ShowMap := False;
 
 MainLevel.FreeLevel;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
 wglDeleteContext(h_RC);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
 e_SetViewPort(0, 0, RenderPanel.Width, RenderPanel.Height);
 if MainLevel.Map.FWidth >= RenderPanel.Width
  then ScrollBar1.Max := MainLevel.Map.FWidth-RenderPanel.Width
   else ScrollBar1.Max := 0;

 if MainLevel.Map.FHeight >= RenderPanel.Height
  then ScrollBar2.Max := MainLevel.Map.FHeight-RenderPanel.Height
   else ScrollBar2.Max := 0;
end;

procedure TMainForm.ScrollBar1Change(Sender: TObject);
begin
 MainLevel.X := -Round(ScrollBar1.Position / 16)*16;
end;

procedure TMainForm.ScrollBar2Change(Sender: TObject);
begin
 MainLevel.Y := -Round(ScrollBar2.Position / 16)*16;
end;

procedure TMainForm.WMPaint(var Msg: TWMPaint);
var
 ps: TPaintStruct;
 i: Integer;
 a: Integer;
 ID: DWORD;
 Width,
 Height: Word;
 XX: Integer;
 aX, aY, aX2, aY2: Integer;
begin
 BeginPaint(Handle, ps);
 e_BeginRender;
  e_Clear(GL_COLOR_BUFFER_BIT, 0+(GetRValue(BackColor)/255),
          0+(GetGValue(BackColor)/255), 0+(GetBValue(BackColor)/255));

  MainLevel.Render;

  if PointEnable then
  for i := 0 to (RenderPanel.Width div PointStep) do
   for a := 0 to (RenderPanel.Height div PointStep) do
    e_DrawPoint(1, i*PointStep, a*PointStep, GetRValue(PointColor),
                GetGValue(PointColor), GetBValue(PointColor));

  if SelectRect <> nil then
   e_DrawQuad(SelectRect.TopLeft.X+MainLevel.X, SelectRect.TopLeft.Y+MainLevel.Y,
              SelectRect.BottomRight.X+MainLevel.X, SelectRect.BottomRight.Y+MainLevel.Y, 2, 255, 0, 0);

  if (MouseDowned) and not(Moving) then
  begin
   if DrawTexturePanel then
   case cbPanelType.ItemIndex of
    0, 1, 2, 3:
     if lbTextureList.ItemIndex <> -1 then
     begin
      MainLevel.Map.PanelSystem.GetTextureByName(lbTextureList.Items[lbTextureList.ItemIndex], ID);
      MainLevel.Map.PanelSystem.GetTextureSizeByName(lbTextureList.Items[lbTextureList.ItemIndex], Width, Height);
      e_DrawFill(ID, Min(MouseDownPoint.X, MousePoint.X), Min(MouseDownPoint.Y, MousePoint.Y), (Abs(MousePoint.X-MouseDownPoint.X) div Width),
                 Abs(MousePoint.Y-MouseDownPoint.Y) div Height, 0, False, False);
     end;
    4: e_DrawFillQuad(MouseDownPoint.X, MouseDownPoint.Y, MousePoint.X, MousePoint.Y,
                      52, 62, 180, 100, False);
    5: e_DrawFillQuad(MouseDownPoint.X, MouseDownPoint.Y, MousePoint.X, MousePoint.Y,
                      64, 127, 0, 80, False);
    6: e_DrawFillQuad(MouseDownPoint.X, MouseDownPoint.Y, MousePoint.X, MousePoint.Y,
                      214, 39, 44, 60, False);
   end;
   e_DrawQuad(MouseDownPoint.X, MouseDownPoint.Y, MousePoint.X, MousePoint.Y, 1, 255, 255, 255);
   if DrawPanelSize then
   begin
    e_DrawFillQuad(MousePoint.X, MousePoint.Y, MousePoint.X+88, MousePoint.Y+32, 192, 192, 192, 127, False);
    e_DrawQuad(MousePoint.X+1, MousePoint.Y+1, MousePoint.X+87, MousePoint.Y+31, 1, 255, 255, 255);
    e_TextOut(MousePoint.X+8, MousePoint.Y+14, PChar('Длина:   '+IntToStr(Abs(MousePoint.X-MouseDownPoint.X))), EditorFont, 0, 0, 0);
    e_TextOut(MousePoint.X+8, MousePoint.Y+28, PChar('Высота: '+IntToStr(Abs(MousePoint.Y-MouseDownPoint.Y))), EditorFont, 0, 0, 0);
   end;

  end;

  e_DrawPoint(3, MousePoint.X - 1, MousePoint.Y - 1, 0, 0, 255);

  if (lbTextureList.ItemIndex > -1) and (cbPreview.Checked) then
  begin
   MainLevel.Map.PanelSystem.GetTextureByName(lbTextureList.Items[lbTextureList.ItemIndex], ID);
   MainLevel.Map.PanelSystem.GetTextureSizeByName(lbTextureList.Items[lbTextureList.ItemIndex], Width, Height);
   e_DrawFillQuad(RenderPanel.Width - Width - 1, RenderPanel.Height - Height - 1,
                  RenderPanel.Width, RenderPanel.Height, GetRValue(PreviewBGColor),
                  GetGValue(PreviewBGColor), GetBValue(PreviewBGColor), 0, False);
   e_Draw(ID, RenderPanel.Width - Width, RenderPanel.Height - Height, 0, True, False);
  end;

  if ShowMap then
  begin
   XX := RenderPanel.Width-((MainLevel.Map.FWidth div 16)*Scale);
   e_DrawFillQuad(XX-1, 0, RenderPanel.Width-1, ((MainLevel.Map.FHeight div 16)*Scale),
                  0, 0, 0, 0, False);
   e_DrawQuad(XX-1, 0, RenderPanel.Width-1, ((MainLevel.Map.FHeight div 16)*Scale)+1,
              1, 197, 197, 197);
  
   // draw water panels
   for a := 0 to High(MainLevel.Map.PanelSystem.PanelsArray) do
    with MainLevel.Map.PanelSystem.PanelsArray[a] do
     if (Width <> 0) and (PanelType = PANEL_WATER) then
     begin
      aX := XX+(X div (16 div Scale));
      aY := Y div (16 div Scale);

      if Width div (16 div Scale) = 0 then
       aX2 := aX+1
        else aX2 := aX+Width div (16 div Scale);
      if Height div (16 div Scale) = 0 then
       aY2 := aY+1
        else aY2 := aY+Height div (16 div Scale);

      if (SelectedObject = GO_PANEL) and (SelectedID = DWORD(a)) then
       e_DrawFillQuad(aX, aY, aX2, aY2, 255, 0, 0, 0, False)
        else e_DrawFillQuad(aX, aY, aX2, aY2, 0, 0, 255, 0, False);
     end;

   // draw acid1 panels
   for a := 0 to High(MainLevel.Map.PanelSystem.PanelsArray) do
    with MainLevel.Map.PanelSystem.PanelsArray[a] do
     if (Width <> 0) and (PanelType = PANEL_ACID1) then
     begin
      aX := XX+(X div (16 div Scale));
      aY := Y div (16 div Scale);

      if Width div (16 div Scale) = 0 then
       aX2 := aX+1
        else aX2 := aX+Width div (16 div Scale);
      if Height div (16 div Scale) = 0 then
       aY2 := aY+1
        else aY2 := aY+Height div (16 div Scale);

      if (SelectedObject = GO_PANEL) and (SelectedID = DWORD(a)) then
       e_DrawFillQuad(aX, aY, aX2, aY2, 255, 0, 0, 0, False)
        else e_DrawFillQuad(aX, aY, aX2, aY2, 255, 0, 0, 0, False);
     end;

    // draw acid2 panels
   for a := 0 to High(MainLevel.Map.PanelSystem.PanelsArray) do
    with MainLevel.Map.PanelSystem.PanelsArray[a] do
     if (Width <> 0) and (PanelType = PANEL_ACID2) then
     begin
      aX := XX+(X div (16 div Scale));
      aY := Y div (16 div Scale);

      if Width div (16 div Scale) = 0 then
       aX2 := aX+1
        else aX2 := aX+Width div (16 div Scale);
      if Height div (16 div Scale) = 0 then
       aY2 := aY+1
        else aY2 := aY+Height div (16 div Scale);

      if (SelectedObject = GO_PANEL) and (SelectedID = DWORD(a)) then
       e_DrawFillQuad(aX, aY, aX2, aY2, 255, 0, 0, 0, False)
        else e_DrawFillQuad(aX, aY, aX2, aY2, 255, 128, 64, 0, False);
     end;

    // Drawing step panels
    for a := 0 to High(MainLevel.Map.PanelSystem.PanelsArray) do
    with MainLevel.Map.PanelSystem.PanelsArray[a] do
     if (Width <> 0) and (PanelType = PANEL_STEP) then
     begin
      aX := XX+(X div (16 div Scale));
      aY := Y div (16 div Scale);

      if Width div (16 div Scale) = 0 then
       aX2 := aX+1
        else aX2 := aX+Width div (16 div Scale);
      if Height div (16 div Scale) = 0 then
       aY2 := aY+1
        else aY2 := aY+Height div (16 div Scale);

      if (SelectedObject = GO_PANEL) and (SelectedID = DWORD(a)) then
       e_DrawFillQuad(aX, aY, aX2, aY2, 255, 0, 0, 0, False)
        else e_DrawFillQuad(aX, aY, aX2, aY2, 192, 192, 192, 0, False);
     end;

    // draw wall panels
    for a := 0 to High(MainLevel.Map.PanelSystem.PanelsArray) do
    with MainLevel.Map.PanelSystem.PanelsArray[a] do
     if (Width <> 0) and (PanelType = PANEL_WALL) then
     begin
      aX := XX+(X div (16 div Scale));
      aY := Y div (16 div Scale);

      if Width div (16 div Scale) = 0 then
       aX2 := aX+1
        else aX2 := aX+Width div (16 div Scale);
      if Height div (16 div Scale) = 0 then
       aY2 := aY+1
        else aY2 := aY+Height div (16 div Scale);

      if (SelectedObject = GO_PANEL) and (SelectedID = DWORD(a)) then
       e_DrawFillQuad(aX, aY, aX2, aY2, 255, 0, 0, 0, False)
        else e_DrawFillQuad(aX, aY, aX2, aY2, 255, 255, 255, 0, False);
     end;

   for a := 0 to High(MainLevel.TriggersSystem.TriggersArray) do
    with MainLevel.TriggersSystem.TriggersArray[a] do
     if TriggerType <> TRIGGER_NONE then
     begin
      aX := XX+(X div (16 div Scale));
      aY := Y div (16 div Scale);

      if Width div (16 div Scale) = 0 then
       aX2 := aX+1
        else aX2 := aX+Width div (16 div Scale);
      if Height div (16 div Scale) = 0 then
       aY2 := aY+1
        else aY2 := aY+Height div (16 div Scale);

      if (SelectedObject = GO_TRIGGER) and (SelectedID = DWORD(a)) then
       e_DrawFillQuad(aX, aY, aX2, aY2, 255, 0, 0, 0, False)
        else e_DrawFillQuad(aX, aY, aX2, aY2, 155, 117, 70, 0, False);
     end;

   e_DrawFillQuad(XX+(Abs(MainLevel.X) div (16 div Scale)), Abs(MainLevel.Y) div (16 div Scale),
                  XX+(Abs(MainLevel.X) div (16 div Scale))+(RenderPanel.Width div (16 div Scale)),
                  (Abs(MainLevel.Y) div (16 div Scale))+(RenderPanel.Height div (16 div Scale)),
                  0, 255, 0, 127, True);

   e_DrawQuad(XX+(Abs(MainLevel.X) div (16 div Scale)), Abs(MainLevel.Y) div (16 div Scale),
              XX+(Abs(MainLevel.X) div (16 div Scale))+(RenderPanel.Width div (16 div Scale)),
              (Abs(MainLevel.Y) div (16 div Scale))+(RenderPanel.Height div (16 div Scale)),
              1, 255, 0, 0);
  end;

 if SelectingDoor then
 begin
  e_DrawFillQuad(MousePoint.X, MousePoint.Y, MousePoint.X+180, MousePoint.Y+18, 192, 192, 192, 127, False);
  e_DrawQuad(MousePoint.X+1, MousePoint.Y+1, MousePoint.X+179, MousePoint.Y+17, 1, 255, 255, 255);
  e_TextOut(MousePoint.X+8, MousePoint.Y+14, 'Выберите панель для двери', EditorFont, 0, 0, 0);
 end;

 if SelectingPoint then
 begin
  e_DrawFillQuad(MousePoint.X, MousePoint.Y, MousePoint.X+180, MousePoint.Y+18, 192, 192, 192, 127, False);
  e_DrawQuad(MousePoint.X+1, MousePoint.Y+1, MousePoint.X+179, MousePoint.Y+17, 1, 255, 255, 255);
  e_TextOut(MousePoint.X+8, MousePoint.Y+14, 'Выберите точку телепорта', EditorFont, 0, 0, 0);
 end;

 StatusBar.Panels[1].Text := Format('(%d:%d)', [MousePoint.X-MainLevel.X,
                                           MousePoint.Y-MainLevel.Y]);
 e_EndRender;
 SwapBuffers(h_DC);
 EndPaint(Handle, ps);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
 InvalidateRect(Handle, nil, False);
end;

procedure TMainForm.aOpenMapExecute(Sender: TObject);
begin
 OpenDialog.Filter := 'Doom2D: Forever maps (*.ini)|*.ini|All files (*.*)|*.*';
 OpenDialog.DefaultExt := 'ini';
 if OpenDialog.Execute then
 begin
  MainLevel.FreeLevel;
  lbTextureList.Clear;
  MainForm.lTextureHeight.Caption := '';
  MainForm.lTextureWidth.Caption := '';
  SelectRect := nil;
  MouseDowned := False;
  MouseDrag := False;
  SelectedID := 0;
  SelectedObject := GO_NONE;
  MainForm.vleObjectProperty.Strings.Clear;
  MainForm.Resize;
  UndoArray := nil;
  ScrollBar1.Position := 0;
  ScrollBar2.Position := 0;

  MainLevel.Map.LoadOldMap(OpenDialog.FileName);

  MainForm.Resize;
 end;
end;

procedure TMainForm.CustomizeActionBars1Execute(Sender: TObject);
begin
 CustomizeDlg1.Show;
end;

procedure TMainForm.aEditorOptionsExecute(Sender: TObject);
begin
 OptionsForm.ShowModal;
end;

procedure TMainForm.RenderPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  sX,
  sY: Word;
  i: Integer;
  ok: Boolean;
begin
if Button = mbRight then // Select or Drag
begin
 case pcObjects.ActivePageIndex of
  0:
  begin
   // Проверяем, щелкнули ли мы на уже выбранной панели
   if (SelectedObject = GO_PANEL) and
      g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y, 4, 4,
                MainLevel.Map.PanelSystem.PanelsArray[SelectedID].X,
                MainLevel.Map.PanelSystem.PanelsArray[SelectedID].Y,
                MainLevel.Map.PanelSystem.PanelsArray[SelectedID].Width,
                MainLevel.Map.PanelSystem.PanelsArray[SelectedID].Height) then
   begin
    Moving := True;
    MoveObjectID := SelectedID;
    MoveObject := SelectedObject;
    MouseDownPoint.X := MousePoint.X-MainLevel.X;
    MouseDownPoint.Y := MousePoint.Y-MainLevel.Y;
    InitX := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].X;
    InitY := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].Y;
    MouseDowned := True;
    Exit;
   end;

   ok := False;
   i := 0;

   if not(ok) and (DrawWater) then
    for i := High(MainLevel.Map.PanelSystem.PanelsArray) downto 0 do
    if (MainLevel.Map.PanelSystem.PanelsArray[i].Width <> 0) and
       (MainLevel.Map.PanelSystem.PanelsArray[i].PanelType in [PANEL_WATER, PANEL_ACID1, PANEL_ACID2]) then
     if g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y, 4, 4,
                  MainLevel.Map.PanelSystem.PanelsArray[i].X,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Y,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Width,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Height) then
    begin
     ok := True;
     Break;
    end;

   if not(ok) and (DrawForeground) then
    for i := High(MainLevel.Map.PanelSystem.PanelsArray) downto 0 do
    if (MainLevel.Map.PanelSystem.PanelsArray[i].Width <> 0) and
       (MainLevel.Map.PanelSystem.PanelsArray[i].PanelType = PANEL_FOREGROUND) then
     if g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y, 4, 4,
                  MainLevel.Map.PanelSystem.PanelsArray[i].X,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Y,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Width,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Height) then
    begin
     ok := True;
     Break;
    end;

   if not(ok) and (DrawWall) then
   for i := High(MainLevel.Map.PanelSystem.PanelsArray) downto 0 do
    if (MainLevel.Map.PanelSystem.PanelsArray[i].Width <> 0) and
       (MainLevel.Map.PanelSystem.PanelsArray[i].PanelType = PANEL_WALL) then
     if g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y, 4, 4,
                  MainLevel.Map.PanelSystem.PanelsArray[i].X,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Y,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Width,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Height) then
    begin
     ok := True;
     Break;
    end;

   if not(ok) and (DrawStep) then
   for i := High(MainLevel.Map.PanelSystem.PanelsArray) downto 0 do
    if (MainLevel.Map.PanelSystem.PanelsArray[i].Width <> 0) and
       (MainLevel.Map.PanelSystem.PanelsArray[i].PanelType = PANEL_STEP) then
     if g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y, 4, 4,
                  MainLevel.Map.PanelSystem.PanelsArray[i].X,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Y,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Width,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Height) then
    begin
     ok := True;
     Break;
    end;

    if not(ok) and (DrawBackground) then
    for i := High(MainLevel.Map.PanelSystem.PanelsArray) downto 0 do
    if (MainLevel.Map.PanelSystem.PanelsArray[i].Width <> 0) and
       (MainLevel.Map.PanelSystem.PanelsArray[i].PanelType = PANEL_BACKGROUND) then
     if g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y, 4, 4,
                  MainLevel.Map.PanelSystem.PanelsArray[i].X,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Y,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Width,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Height) then
    begin
     ok := True;
     Break;
    end;

    SelectRect := nil;

    if ok then
    begin
     FillProperty(i, GO_PANEL);

     New(SelectRect);
     SelectRect.Left := MainLevel.Map.PanelSystem.PanelsArray[i].X;
     SelectRect.Top := MainLevel.Map.PanelSystem.PanelsArray[i].Y;
     SelectRect.Right := MainLevel.Map.PanelSystem.PanelsArray[i].Width+SelectRect.Left;
     SelectRect.Bottom := MainLevel.Map.PanelSystem.PanelsArray[i].Height+SelectRect.Top;

     SelectedID := i;
     SelectedObject := GO_PANEL;
    end
     else
    begin
     vleObjectProperty.Strings.Clear;
     SelectedObject := GO_NONE;
    end;

    Exit;
  end;
  1:
  begin
   if (SelectedObject = GO_ITEM) and
      g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y, 4, 4,
                MainLevel.ItemSystem.ItemsArray[SelectedID].GameX,
                MainLevel.ItemSystem.ItemsArray[SelectedID].GameY,
                MainLevel.ItemSystem.ItemsArray[SelectedID].Width,
                MainLevel.ItemSystem.ItemsArray[SelectedID].Height) then
   begin
    Moving := True;
    MoveObjectID := SelectedID;
    MoveObject := SelectedObject;
    MouseDownPoint.X := MousePoint.X-MainLevel.X;
    MouseDownPoint.Y := MousePoint.Y-MainLevel.Y;
    InitX := MainLevel.ItemSystem.ItemsArray[SelectedID].GameX;
    InitY := MainLevel.ItemSystem.ItemsArray[SelectedID].GameY;
    MouseDowned := True;
    Exit;
   end;

   SelectRect := nil;

   for i := High(MainLevel.ItemSystem.ItemsArray) downto 0 do
    begin
     if (MainLevel.ItemSystem.ItemsArray[i].ItemType <> ITEM_NONE) then
      if g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y-2, 4, 4,
                   MainLevel.ItemSystem.ItemsArray[i].GameX,
                   MainLevel.ItemSystem.ItemsArray[i].GameY,
                   MainLevel.ItemSystem.ItemsArray[i].Width,
                   MainLevel.ItemSystem.ItemsArray[i].Height) then
     begin
      FillProperty(i, GO_ITEM); 

      New(SelectRect);
      SelectRect.Left := MainLevel.ItemSystem.ItemsArray[i].GameX;
      SelectRect.Top := MainLevel.ItemSystem.ItemsArray[i].GameY;
      SelectRect.Right := MainLevel.ItemSystem.ItemsArray[i].Width+SelectRect.Left;
      SelectRect.Bottom := MainLevel.ItemSystem.ItemsArray[i].Height+SelectRect.Top;

      SelectedID := i;
      SelectedObject := GO_ITEM;

      Exit;
     end;
   end;

   vleObjectProperty.Strings.Clear;
   SelectedObject := GO_NONE;
  end;
  2:
  begin
   with MainLevel.MonsterSystem.MonstersArray[SelectedID] do
   begin
    if (SelectedObject = GO_MONSTER) and
        g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y, 4, 4,
                  GameX+Size.Left, GameY+Size.Top, Size.Right-Size.Left, Size.Bottom-Size.Top) then
    begin
     Moving := True;
     MoveObjectID := SelectedID;
     MoveObject := SelectedObject;
     MouseDownPoint.X := MousePoint.X-MainLevel.X;
     MouseDownPoint.Y := MousePoint.Y-MainLevel.Y;
     InitX := GameX;
     InitY := GameY;
     MouseDowned := True;
     Exit;
    end;
   end;

   SelectRect := nil;

   for i := High(MainLevel.MonsterSystem.MonstersArray) downto 0 do
    with MainLevel.MonsterSystem.MonstersArray[i] do
    begin
     if (MonsterType <> MONSTER_NONE) then
      if g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y-2, 4, 4,
                   GameX+Size.Left, GameY+Size.Top, Size.Right-Size.Left, Size.Bottom-Size.Top) then
     begin
      FillProperty(i, GO_MONSTER); 

      New(SelectRect);
      SelectRect.Left := GameX+Size.Left;
      SelectRect.Top := GameY+Size.Top;
      SelectRect.Right := GameX+Size.Right;
      SelectRect.Bottom := GameY+Size.Bottom;

      SelectedID := i;
      SelectedObject := GO_MONSTER;

      Exit;
     end;
    end;
   vleObjectProperty.Strings.Clear;
   SelectedObject := GO_NONE;
  end;
  3:
  with MainLevel.AreaSystem.AreasArray[SelectedID] do
  begin
   if (SelectedObject = GO_AREA) and
      g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y, 4, 4,
                GameX+Size.Left, GameY+Size.Top, Size.Right-Size.Left, Size.Bottom-Size.Top) then
   begin
    Moving := True;
    MoveObjectID := SelectedID;
    MoveObject := SelectedObject;
    MouseDownPoint.X := MousePoint.X-MainLevel.X;
    MouseDownPoint.Y := MousePoint.Y-MainLevel.Y;
    InitX := GameX;
    InitY := GameY;
    MouseDowned := True;
    Exit;
   end;

   SelectRect := nil;

   for i := High(MainLevel.AreaSystem.AreasArray) downto 0 do
   with MainLevel.AreaSystem.AreasArray[i] do
    begin
     if (AreaType <> AREA_NONE) then
      if g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y, 4, 4,
                   GameX+Size.Left, GameY+Size.Top, Size.Right-Size.Left, Size.Bottom-Size.Top) then
     begin
      FillProperty(i, GO_AREA); 

      New(SelectRect);
      SelectRect.Left := GameX+Size.Left;
      SelectRect.Top := GameY+Size.Top;
      SelectRect.Right := GameX+Size.Right;
      SelectRect.Bottom := GameY+Size.Bottom;

      SelectedID := i;
      SelectedObject := GO_AREA;

      Exit;
     end;
    end;
   vleObjectProperty.Strings.Clear;
   SelectedObject := GO_NONE;
  end;
  4:
  with MainLevel.TriggersSystem.TriggersArray[SelectedID] do
  begin
   if (SelectedObject = GO_TRIGGER) and
      g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y, 4, 4,
                X, Y, Width, Height) then
   begin
    Moving := True;
    MoveObjectID := SelectedID;
    MoveObject := SelectedObject;
    MouseDownPoint.X := MousePoint.X-MainLevel.X;
    MouseDownPoint.Y := MousePoint.Y-MainLevel.Y;
    InitX := X;
    InitY := Y;
    MouseDowned := True;
    Exit;
   end;

   SelectRect := nil;

   for i := High(MainLevel.TriggersSystem.TriggersArray) downto 0 do
   with MainLevel.TriggersSystem.TriggersArray[i] do
    begin
     if (TriggerType <> TRIGGER_NONE) then
      if g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y, 4, 4,
                   X, Y, Width, Height) then
     begin
      SelectTriggerID := i;

      FillProperty(i, GO_TRIGGER);

      New(SelectRect);
      SelectRect.Left := X;
      SelectRect.Top := Y;
      SelectRect.Right := X+Width;
      SelectRect.Bottom := Y+Height;

      SelectedID := i;
      SelectedObject := GO_TRIGGER;

      Exit;
     end;
    end;
   vleObjectProperty.Strings.Clear;
   SelectedObject := GO_NONE;
  end;
 end; // case ActivePageIndex
end // Button = mbRight
 else if Button = mbLeft then
begin
 if Moving then Exit;

 if SelectingDoor then
 begin
  if DrawWall then
   for i := High(MainLevel.Map.PanelSystem.PanelsArray) downto 0 do
    if (MainLevel.Map.PanelSystem.PanelsArray[i].Width <> 0) and
       (MainLevel.Map.PanelSystem.PanelsArray[i].PanelType = PANEL_WALL) then
     if g_Collide(MousePoint.X-MainLevel.X-2, MousePoint.Y-MainLevel.Y, 4, 4,
                  MainLevel.Map.PanelSystem.PanelsArray[i].X,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Y,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Width,
                  MainLevel.Map.PanelSystem.PanelsArray[i].Height) then
    begin
     vleObjectProperty.Values['Target door'] := IntToStr(i);
    end;
  SelectingDoor := False;
  Exit;
 end;

 if SelectingPoint then
 begin
  vleObjectProperty.Values['Target point'] := Format('(%d:%d)', [MousePoint.X-MainLevel.X, MousePoint.Y-MainLevel.Y]);
  SelectingPoint := False;
  Exit;
 end;

 if (ShowMap) and
    g_Collide(RenderPanel.Width-(MainLevel.Map.FWidth div 16)*Scale, 0,
              (MainLevel.Map.FWidth div 16)*Scale, (MainLevel.Map.FHeight div 16)*Scale,
              X, Y, 2, 2) then
 begin
  MainLevel.X := ((-(X-(RenderPanel.Width-(MainLevel.Map.FWidth div 16)*Scale))*16)+
             (RenderPanel.Width div 32)*Scale*16) div Scale;
  if MainLevel.X > 0 then MainLevel.X := 0;
  if MainLevel.X-RenderPanel.Width < -MainLevel.Map.FWidth then
   MainLevel.X := -(MainLevel.Map.FWidth div 16)*16+(RenderPanel.Width div 16)*16;

  MainLevel.Y := ((-Y*16)+(RenderPanel.Height div 32)*16) div Scale;
  if MainLevel.Y > 0 then MainLevel.Y := 0;
  if MainLevel.Y-RenderPanel.Height < -MainLevel.Map.FHeight then
   MainLevel.Y := -(MainLevel.Map.FHeight div 16)*16+(RenderPanel.Height div 16)*16;

  ScrollBar1.Position := Abs(MainLevel.X);
  ScrollBar2.Position := Abs(MainLevel.Y);

  MapMoving := True;
  Exit;
 end;

 case pcObjects.ActivePageIndex of
  0, 4:
  begin
   MouseDowned := True;
   if cbLockCursor.Checked then
   begin
    sX := PointStep;
    sY := PointStep;
    MouseDownPoint.X := Round(X / sX)*sX;
    MouseDownPoint.Y := Round(Y / sY)*sY;
   end
    else
   begin
    MouseDownPoint.X := X;
    MouseDownPoint.Y := Y;
   end;
  end;
 end;
end; // Button = mbLeft

Moving := False;
SelectingDoor := False;
SelectingPoint := False;
SelectRect := nil;
SelectedObject := GO_NONE;
vleObjectProperty.Strings.Clear;
end;

procedure TMainForm.RenderPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  sX,
  sY: Word;
begin
 if ShowMap and
    g_Collide(RenderPanel.Width-(MainLevel.Map.FWidth div 16)*Scale, 0,
              (MainLevel.Map.FWidth div 16)*Scale, (MainLevel.Map.FHeight div 16)*Scale,
              X, Y, 2, 2) then
 begin
  if MapMoving then
  begin
   MainLevel.X := ((-(X-(RenderPanel.Width-(MainLevel.Map.FWidth div 16)*Scale))*16)+
             (RenderPanel.Width div 32)*Scale*16) div Scale;
   if MainLevel.X > 0 then MainLevel.X := 0;
   if MainLevel.X-RenderPanel.Width < -MainLevel.Map.FWidth then
    MainLevel.X := -(MainLevel.Map.FWidth div 16)*16+(RenderPanel.Width div 16)*16;

   MainLevel.Y := ((-Y*16)+(RenderPanel.Height div 32)*16) div Scale;
   if MainLevel.Y > 0 then MainLevel.Y := 0;
   if MainLevel.Y-RenderPanel.Height < -MainLevel.Map.FHeight then
    MainLevel.Y := -(MainLevel.Map.FHeight div 16)*16+(RenderPanel.Height div 16)*16;

   ScrollBar1.Position := Abs(MainLevel.X);
   ScrollBar2.Position := Abs(MainLevel.Y);

   Exit;
  end;
 end;

 if cbLockCursor.Checked then
 begin
  sX := PointStep;
  sY := PointStep;
  MousePoint.X := Round(X / sX)*sX;
  MousePoint.Y := Round(Y / sY)*sY;
 end
  else
 begin
  MousePoint.X := X;
  MousePoint.Y := Y;
 end;
 
 if Moving then
 begin
  if MoveObject = GO_PANEL then
  begin
    MainLevel.Map.PanelSystem.PanelsArray[MoveObjectID].X := MouseDownPoint.X+
      ((MousePoint.X-MainLevel.X)-MouseDownPoint.X)-(MouseDownPoint.X-InitX);
    MainLevel.Map.PanelSystem.PanelsArray[MoveObjectID].Y := MouseDownPoint.Y+
      ((MousePoint.Y-MainLevel.Y)-MouseDownPoint.Y)-(MouseDownPoint.Y-InitY);
    SelectRect.Left := MainLevel.Map.PanelSystem.PanelsArray[MoveObjectID].X;
    SelectRect.Top := MainLevel.Map.PanelSystem.PanelsArray[MoveObjectID].Y;
    SelectRect.Right := MainLevel.Map.PanelSystem.PanelsArray[MoveObjectID].Width+SelectRect.Left;
    SelectRect.Bottom := MainLevel.Map.PanelSystem.PanelsArray[MoveObjectID].Height+SelectRect.Top;
  end;

  if MoveObject = GO_ITEM then
  begin
   if not(ssShift in Shift) or
      not(g_CollideLevel(MouseDownPoint.X+((MousePoint.X-MainLevel.X)-MouseDownPoint.X)-(MouseDownPoint.X-InitX),
                         MouseDownPoint.Y+((MousePoint.Y-MainLevel.Y)-MouseDownPoint.Y)-(MouseDownPoint.Y-InitY),
                         MainLevel.ItemSystem.ItemsArray[MoveObjectID].Width,
                         MainLevel.ItemSystem.ItemsArray[MoveObjectID].Height)) then
   begin
    MainLevel.ItemSystem.ItemsArray[MoveObjectID].GameX := MouseDownPoint.X+
      ((MousePoint.X-MainLevel.X)-MouseDownPoint.X)-(MouseDownPoint.X-InitX);
    MainLevel.ItemSystem.ItemsArray[MoveObjectID].GameY := MouseDownPoint.Y+
      ((MousePoint.Y-MainLevel.Y)-MouseDownPoint.Y)-(MouseDownPoint.Y-InitY);
    SelectRect.Left := MainLevel.ItemSystem.ItemsArray[MoveObjectID].GameX;
    SelectRect.Top := MainLevel.ItemSystem.ItemsArray[MoveObjectID].GameY;
    SelectRect.Right := MainLevel.ItemSystem.ItemsArray[MoveObjectID].Width+SelectRect.Left;
    SelectRect.Bottom := MainLevel.ItemSystem.ItemsArray[MoveObjectID].Height+SelectRect.Top;
   end;
  end;

  if MoveObject = GO_MONSTER then
  with MainLevel.MonsterSystem.MonstersArray[MoveObjectID] do
  begin
   if not(ssShift in Shift) or
      not(g_CollideLevel(MouseDownPoint.X+((MousePoint.X-MainLevel.X)-MouseDownPoint.X)-(MouseDownPoint.X-InitX)+Size.Left,
                         MouseDownPoint.Y+((MousePoint.Y-MainLevel.Y)-MouseDownPoint.Y)-(MouseDownPoint.Y-InitY)+Size.Top,
                         Size.Right-Size.Left, Size.Bottom-Size.Top)) then
   begin
    GameX := MouseDownPoint.X+((MousePoint.X-MainLevel.X)-MouseDownPoint.X)-(MouseDownPoint.X-InitX);
    GameY := MouseDownPoint.Y+((MousePoint.Y-MainLevel.Y)-MouseDownPoint.Y)-(MouseDownPoint.Y-InitY);
    SelectRect.Left := GameX+Size.Left;
    SelectRect.Top := GameY+Size.Top;
    SelectRect.Right := GameX+Size.Right;
    SelectRect.Bottom := GameY+Size.Bottom;
   end;
  end;

  if MoveObject = GO_AREA then
  with MainLevel.AreaSystem.AreasArray[MoveObjectID] do
  begin
   if not(ssShift in Shift) or
      not(g_CollideLevel(MouseDownPoint.X+((MousePoint.X-MainLevel.X)-MouseDownPoint.X)-(MouseDownPoint.X-InitX)+Size.Left,
                         MouseDownPoint.Y+((MousePoint.Y-MainLevel.Y)-MouseDownPoint.Y)-(MouseDownPoint.Y-InitY)+Size.Top,
                         Size.Right-Size.Left, Size.Bottom-Size.Top)) then
   begin
    GameX := MouseDownPoint.X+((MousePoint.X-MainLevel.X)-MouseDownPoint.X)-(MouseDownPoint.X-InitX);
    GameY := MouseDownPoint.Y+((MousePoint.Y-MainLevel.Y)-MouseDownPoint.Y)-(MouseDownPoint.Y-InitY);

    SelectRect.Left := GameX+Size.Left;
    SelectRect.Top := GameY+Size.Top;
    SelectRect.Right := GameX+Size.Right;
    SelectRect.Bottom := GameY+Size.Bottom;
   end;
  end;

  if MoveObject = GO_TRIGGER then
  begin
   if not(ssShift in Shift) or
      not(g_CollideLevel(MouseDownPoint.X+((MousePoint.X-MainLevel.X)-MouseDownPoint.X)-(MouseDownPoint.X-InitX),
                         MouseDownPoint.Y+((MousePoint.Y-MainLevel.Y)-MouseDownPoint.Y)-(MouseDownPoint.Y-InitY),
                         MainLevel.TriggersSystem.TriggersArray[MoveObjectID].Width,
                         MainLevel.TriggersSystem.TriggersArray[MoveObjectID].Height)) then
   begin
    MainLevel.TriggersSystem.TriggersArray[MoveObjectID].X := MouseDownPoint.X+
      ((MousePoint.X-MainLevel.X)-MouseDownPoint.X)-(MouseDownPoint.X-InitX);
    MainLevel.TriggersSystem.TriggersArray[MoveObjectID].Y := MouseDownPoint.Y+
      ((MousePoint.Y-MainLevel.Y)-MouseDownPoint.Y)-(MouseDownPoint.Y-InitY);
    SelectRect.Left := MainLevel.TriggersSystem.TriggersArray[MoveObjectID].X;
    SelectRect.Top := MainLevel.TriggersSystem.TriggersArray[MoveObjectID].Y;
    SelectRect.Right := MainLevel.TriggersSystem.TriggersArray[MoveObjectID].Width+SelectRect.Left;
    SelectRect.Bottom := MainLevel.TriggersSystem.TriggersArray[MoveObjectID].Height+SelectRect.Top;
   end;
  end;

  Exit;
 end;

 // Если не перемещаем объект
 case pcObjects.ActivePageIndex of
  0: if MouseDrag then
     begin
      if not(cbPanelType.ItemIndex in [4, 5, 6]) and (lbTextureList.ItemIndex <> -1) then
      begin
       sX := StrToInt(lTextureWidth.Caption);
       sY := StrToInt(lTextureHeight.Caption);
      end
       else
      begin
       sX := PointStep;
       sY := PointStep;
      end;
      X := X-MouseDownPoint.X;
      Y := Y-MouseDownPoint.Y;
      MousePoint.X := (Round(X / sX)*sX)+MouseDownPoint.X;
      MousePoint.Y := (Round(Y / sY)*sY)+MouseDownPoint.Y;
      MouseDrag := MouseDowned;
      Exit;
     end;
  4: if MouseDrag then
     begin
      sX := PointStep;
      sY := PointStep;
      X := X-MouseDownPoint.X;
      Y := Y-MouseDownPoint.Y;
      MousePoint.X := (Round(X / sX)*sX)+MouseDownPoint.X;
      MousePoint.Y := (Round(Y / sY)*sY)+MouseDownPoint.Y;
      MouseDrag := MouseDowned;
      Exit;
     end;
 end;

 MouseDrag := MouseDowned;
end;

procedure TMainForm.RenderPanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Panel: TGamePanel;
  Trigger: TTrigger;
begin
 MapMoving := False;

 if Moving then
 begin
  Moving := False;
  MoveObject := GO_NONE;
  MouseDowned := False;
  Exit;
 end;

 if not(ShowMap) or
    not(g_Collide(RenderPanel.Width-(MainLevel.Map.FWidth div 16)*Scale, 0,
              (MainLevel.Map.FWidth div 16)*Scale, (MainLevel.Map.FHeight div 16)*Scale,
              X, Y, 2, 2)) then
 case pcObjects.ActivePageIndex of
  0: if (MouseDrag) and (MousePoint.X <> MouseDownPoint.X) and
        (MousePoint.Y <> MouseDownPoint.Y) then
     begin
      if (lbTextureList.ItemIndex = -1) and not(cbPanelType.ItemIndex in [4, 5, 6]) then
      begin
       MessageBox(MainForm.Handle, PChar('Выберите текстуру'), PChar('Ошибка'), MB_OK);
       MouseDowned := False;
       MouseDrag := False;
       Exit;
      end;
      Panel.X := Min(MousePoint.X-MainLevel.X, MouseDownPoint.X-MainLevel.X);
      Panel.Y := Min(MousePoint.Y-MainLevel.Y, MouseDownPoint.Y-MainLevel.Y);
      Panel.Height := Max(MousePoint.Y-MouseDownPoint.Y, MouseDownPoint.Y-MousePoint.Y);
      Panel.Width := Max(MousePoint.X-MouseDownPoint.X, MouseDownPoint.X-MousePoint.X);
      case cbPanelType.ItemIndex of
       0: Panel.PanelType := PANEL_WALL;
       1: Panel.PanelType := PANEL_FOREGROUND;
       2: Panel.PanelType := PANEL_BACKGROUND;
       3: Panel.PanelType := PANEL_STEP;
       4: Panel.PanelType := PANEL_WATER;
       5: Panel.PanelType := PANEL_ACID1;
       6: Panel.PanelType := PANEL_ACID2;
      end;
      if not(Panel.PanelType in [PANEL_WATER, PANEL_ACID1, PANEL_ACID2]) then
      begin
       Panel.TextureHeight := StrToInt(lTextureHeight.Caption);
       Panel.TextureWidth := StrToInt(lTextureWidth.Caption);
       Panel.TextureName := lbTextureList.Items[lbTextureList.ItemIndex];
      end
       else
      begin
       Panel.TextureHeight := 1;
       Panel.TextureWidth := 1;
       Panel.TextureName := '';
      end;
      Panel.Alpha := 0;
      MainLevel.Map.PanelSystem.CreateGamePanel(Panel);
     end;
  4: if (MouseDrag) and (MousePoint.X <> MouseDownPoint.X) and
        (MousePoint.Y <> MouseDownPoint.Y) and (lbTriggersList.ItemIndex <> -1) then
     begin
      ZeroMemory(@Trigger, SizeOf(TTrigger));
      Trigger.X := Min(MousePoint.X-MainLevel.X, MouseDownPoint.X-MainLevel.X);
      Trigger.Y := Min(MousePoint.Y-MainLevel.Y, MouseDownPoint.Y-MainLevel.Y);
      Trigger.Height := Max(MousePoint.Y-MouseDownPoint.Y, MouseDownPoint.Y-MousePoint.Y);
      Trigger.Width := Max(MousePoint.X-MouseDownPoint.X, MouseDownPoint.X-MousePoint.X);
      Trigger.TriggerType := TTriggerType(lbTriggersList.ItemIndex+1);
      if cbPlayerCollide.Checked then
       Trigger.TriggerActivateType := Trigger.TriggerActivateType+[TAT_PLAYERCOLLIDE];
      if cbMonsterCollide.Checked then
       Trigger.TriggerActivateType := Trigger.TriggerActivateType+[TAT_MONSTERCOLLIDE];
      if cbPlayerPress.Checked then
       Trigger.TriggerActivateType := Trigger.TriggerActivateType+[TAT_PLAYERPRESS];
      if cbRedKey.Checked then
       Trigger.TriggerActivateKey := Trigger.TriggerActivateKey+[KEY_RED];
      if cbGreenKey.Checked then
       Trigger.TriggerActivateKey := Trigger.TriggerActivateKey+[KEY_GREEN];
      if cbBlueKey.Checked then
       Trigger.TriggerActivateKey := Trigger.TriggerActivateKey+[KEY_BLUE];
      MainLevel.TriggersSystem.CreateTrigger(Trigger);
     end;
 end;

 MouseDowned := False;
 MouseDrag := False;
end;

procedure TMainForm.RenderPanelClick(Sender: TObject);
begin

if not(ShowMap) or
   not(g_Collide(RenderPanel.Width-(MainLevel.Map.FWidth div 16)*Scale, 0,
                 (MainLevel.Map.FWidth div 16)*Scale, (MainLevel.Map.FHeight div 16)*Scale,
                 MousePoint.X, MousePoint.Y, 2, 2)) then
case pcObjects.ActivePageIndex of
1: if (lbItemList.ItemIndex <> -1) then
   begin
    MainLevel.ItemSystem.CreateItem(TItemType(lbItemList.ItemIndex + 1),
                          MousePoint.X-MainLevel.X, MousePoint.Y-MainLevel.Y);
   end;
2: if lbMonsterList.ItemIndex <> -1 then
   begin
    if rbLeft.Checked then
     MainLevel.MonsterSystem.CreateMonster(MousePoint.X-MainLevel.X, MousePoint.Y-MainLevel.Y,
                                 TMonsterType(lbMonsterList.ItemIndex + 1), D_LEFT)
    else
     MainLevel.MonsterSystem.CreateMonster(MousePoint.X-MainLevel.X, MousePoint.Y-MainLevel.Y,
                                 TMonsterType(lbMonsterList.ItemIndex + 1), D_RIGHT);
   end;
3: if lbAreasList.ItemIndex <> -1 then
   begin
    if rbAreaLeft.Checked then
     MainLevel.AreaSystem.CreateArea(MousePoint.X-MainLevel.X, MousePoint.Y-MainLevel.Y,
                          TAreaType(lbAreasList.ItemIndex + 1), D_LEFT)
    else
     MainLevel.AreaSystem.CreateArea(MousePoint.X-MainLevel.X, MousePoint.Y-MainLevel.Y,
                          TAreaType(lbAreasList.ItemIndex + 1), D_RIGHT);
   end;
end;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if (Key = VK_DELETE) and (SelectRect <> nil) then
 begin
  case SelectedObject of
   GO_PANEL:
   begin
    SetLength(UndoArray, Length(UndoArray)+1);
    with UndoArray[High(UndoArray)] do
    begin
     UndoType := U_PANEL;
     UndoSubType := US_DELETE;
     reserved1 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].X;
     reserved2 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].Y;
     reserved3 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].Height;
     reserved4 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].Width;
     reserved5 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].TextureHeight;
     reserved6 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].TextureWidth;
     reserved7 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].Alpha;
     reserved8 := Ord(MainLevel.Map.PanelSystem.PanelsArray[SelectedID].PanelType);
     reserved9 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].TextureName;
    end;
    MainLevel.Map.PanelSystem.DeletePanel(SelectedID);
   end;
   GO_ITEM:
   begin
    SetLength(UndoArray, Length(UndoArray)+1);
    with UndoArray[High(UndoArray)] do
    begin
     UndoType := U_ITEM;
     UndoSubType := US_DELETE;
     reserved1 := MainLevel.ItemSystem.ItemsArray[SelectedID].GameX;
     reserved2 := MainLevel.ItemSystem.ItemsArray[SelectedID].GameY;
     reserved3 := Ord(MainLevel.ItemSystem.ItemsArray[SelectedID].ItemType);
    end;
    MainLevel.ItemSystem.RemoveItem(SelectedID);
   end;
   GO_MONSTER:
    begin
     SetLength(UndoArray, Length(UndoArray)+1);
     with UndoArray[High(UndoArray)] do
     begin
      UndoType := U_MONSTER;
      UndoSubType := US_DELETE;
      reserved1 := MainLevel.MonsterSystem.MonstersArray[SelectedID].GameX;
      reserved2 := MainLevel.MonsterSystem.MonstersArray[SelectedID].GameY;
      reserved3 := Ord(MainLevel.MonsterSystem.MonstersArray[SelectedID].MonsterType);
      reserved4 := Ord(MainLevel.MonsterSystem.MonstersArray[SelectedID].Direction);
     end;

     MainLevel.MonsterSystem.RemoveMonster(SelectedID);
    end;
   GO_AREA:
    begin
     SetLength(UndoArray, Length(UndoArray)+1);
     with UndoArray[High(UndoArray)] do
     begin
      UndoType := U_AREA;
      UndoSubType := US_DELETE;
      reserved1 := MainLevel.AreaSystem.AreasArray[SelectedID].GameX;
      reserved2 := MainLevel.AreaSystem.AreasArray[SelectedID].GameY;
      reserved3 := Ord(MainLevel.AreaSystem.AreasArray[SelectedID].AreaType);
      reserved4 := Ord(MainLevel.AreaSystem.AreasArray[SelectedID].Direction);
     end;
     
     MainLevel.AreaSystem.RemoveArea(SelectedID);
    end;
   GO_TRIGGER:
    begin
     MainLevel.TriggersSystem.RemoveTrigger(SelectedID);
    end;
  end;
  SelectRect := nil;
  SelectedObject := GO_NONE;
  SelectedID := 0;
  vleObjectProperty.Strings.Clear;
 end;
end;

procedure TMainForm.aExitExecute(Sender: TObject);
begin
 Close;
end;

procedure TMainForm.aNewMapExecute(Sender: TObject);
begin
 if (MessageDlg('Очистить весь уровень ?', mtConfirmation, mbOKCancel, 0)) = 2 then Exit;

 MainLevel.FreeLevel;
 lbTextureList.Clear;
 MainForm.lTextureHeight.Caption := '';
 MainForm.lTextureWidth.Caption := '';
 SelectRect := nil;
 MouseDowned := False;
 MouseDrag := False;
 SelectingDoor := False;
 SelectingPoint := False;
 SelectedID := 0;
 SelectedObject := GO_NONE;
 MainForm.vleObjectProperty.Strings.Clear;
 MainForm.Resize;
 UndoArray := nil;
end;

procedure TMainForm.RenderPanelResize(Sender: TObject);
begin
 if MainForm.Visible then MainForm.Resize;
end;

procedure TMainForm.bbAddTextureClick(Sender: TObject);
begin
 AddTextureForm.ShowModal;
end;

procedure TMainForm.lbTextureListClick(Sender: TObject);
var
  ID:     DWORD;
  Width,
  Height: Word;
begin
 if lbTextureList.ItemIndex > -1 then
 begin
  MainLevel.Map.PanelSystem.GetTextureByName(lbTextureList.Items[lbTextureList.ItemIndex], ID);
  MainLevel.Map.PanelSystem.GetTextureSizeByName(lbTextureList.Items[lbTextureList.ItemIndex], Width, Height);
  lTextureWidth.Caption := IntToStr(Width);
  lTextureHeight.Caption := IntToStr(Height);
 end
  else
 begin
  lTextureWidth.Caption := '';
  lTextureHeight.Caption := '';
 end;
end;

procedure TMainForm.aMapOptionsExecute(Sender: TObject);
begin
 MapOptionsForm.ShowModal;
end;

procedure TMainForm.vleObjectPropertyGetPickList(Sender: TObject;
  const KeyName: String; Values: TStrings);
begin
 if vleObjectProperty.ItemProps[KeyName].EditStyle = esPickList then
 begin
  if KeyName = 'Panel Type' then
  begin
   Values.Add('WALL');
   Values.Add('BACKGROUND');
   Values.Add('FOREGROUND');
   Values.Add('STEP');
   Values.Add('WATER');
   Values.Add('ACID1');
   Values.Add('ACID2');
  end
   else if KeyName = 'Direction' then
  begin
   Values.Add('LEFT');
   Values.Add('RIGHT');
  end
   else if KeyName = 'Door direction' then
  begin
   Values.Add('Left');
   Values.Add('Right');
   Values.Add('Up');
   Values.Add('Down');
  end;
 end;
end;

function Calculate(Cal: string): integer;
var
  WasError: Boolean;
  Res: Integer;
  I: Integer;
  Ns: Integer;

function Step(St: string):Integer;
var
  i, Code, Num: Integer;
  GoNext: Boolean;
  NumSkob: Integer;
  S1, S2: String;
begin
  if not WasError then
  begin
    if st='' then
    begin
     Messagedlg('Где-то ищите ошибку.', mterror, [mbok], 0);
     WasError:=true;
     Result:=0;
     Exit;
    end;
    val(St, Num, Code);
    Result:=0;
    GoNext:=false;
    NumSkob:=0;
    if Code=0 then
      Result:=Num
    else begin
      if St[1]='(' then begin
        GoNext:=true;
        for i:=1 to length(St) do begin
          if St[i]='(' then inc(NumSkob);
          if st[i]=')' then dec(NumSkob);
          if (NumSkob=0) and (i<>length(St)) then GoNext:=false;
        end;
      end;
      NumSkob:=0;
        If GoNext then
        Result:=Step(copy(st, 2, length(st)-2))
      else begin
        if not WasError then for i:=1 to length(St) do begin
          if st[i]='(' then inc(NumSkob);
          if st[i]=')' then dec(NumSkob);
          if (NumSkob=0) and ((St[i]='+') or (St[i]='-')) then begin
            S1:=copy(St, 1, i-1);
            S2:=copy(St, i+1, length(st)-i);
            if St[i]='+' then
              Result:=step(S1)+step(S2)
            else
              Result:=step(S1)-step(S2);
            exit;
          end;
        end;
        numskob:=0;
        if not WasError then for i:=1 to length(St) do begin
          if st[i]='(' then inc(NumSkob);
          if st[i]=')' then dec(NumSkob);
          if (NumSkob=0) and ((St[i]='*') or ((St[i]='/') or (St[i]='\'))) then begin
            S1:=copy(St,1,i-1);
            S2:=copy(St,i+1,length(St)-i);
            if St[i]='*' then
              Result:=step(S1)*step(S2)
            else begin
              if step(S2)<>0 then
                Result:=step(S1) div step(S2)
              else begin
                Messagedlg('Делить на ноль нельзя, товарищ.',mterror,[mbok],0);
                WasError:=true;
                Result:=0;
              end;
            end;
            exit;
          end;
        end;
      end;
    end;
  end else Result:=0;
end;

procedure DeleteSpaces(var S:String);
var
  i:integer;
begin
  i:=0;
  while i<length(S) do begin
    inc(i);
    if S[i]=' ' then delete(S,i,1) else i:=i+1;
  end;
end;

begin
  DeleteSpaces(Cal);
  WasError:=false;
  Ns:=0;
  Res:=0;
  for i:=1 to length(Cal) do begin
    if Cal[i]='(' then inc(Ns);
    if Cal[i]=')' then dec(Ns);
    if Ns<0 then begin
      Messagedlg('Конструкция скобок неверна.', mterror,[mbok],0);
      WasError:=true;
      break;
    end;
  end;
  if (Ns<>0) and (not WasError) then begin
    Messagedlg('Конструкция скобок неверна.',mterror,[mbok],0);
    WasError:=true;
  end;
  if not WasError then Res:=Step(Cal);
  if WasError then Res:=0;
  result:=Res;
end;

procedure TMainForm.aAboutExecute(Sender: TObject);
begin
 AboutForm.ShowModal;
end;

procedure TMainForm.aDrawBackgroundExecute(Sender: TObject);
begin
 aDrawBackground.Checked := not(aDrawBackground.Checked);
 DrawBackground := aDrawBackground.Checked;
 SelectRect := nil;
 SelectedID := 0;
 SelectedObject := GO_NONE;
end;

procedure TMainForm.aDrawWallExecute(Sender: TObject);
begin
 aDrawWall.Checked := not(aDrawWall.Checked);
 DrawWall := aDrawWall.Checked;
 SelectRect := nil;
 SelectedID := 0;
 SelectedObject := GO_NONE;
end;

procedure TMainForm.aDrawForegroundExecute(Sender: TObject);
begin
 aDrawForeground.Checked := not(aDrawForeground.Checked);
 DrawForeground := aDrawForeground.Checked;
 SelectRect := nil;
 SelectedID := 0;
 SelectedObject := GO_NONE;
end;

procedure TMainForm.bbRemoveTextureClick(Sender: TObject);
var
  i: Integer;
begin
 if lbTextureList.Items[lbTextureList.ItemIndex] = '<notexture>' then Exit;

 if lbTextureList.ItemIndex <> -1 then
 begin
  for i := 0 to High(MainLevel.Map.PanelSystem.PanelsArray) do
   if MainLevel.Map.PanelSystem.PanelsArray[i].TextureName =
       lbTextureList.Items[lbTextureList.ItemIndex] then
   begin
    MessageBox(MainForm.Handle, PChar('Текстура '+lbTextureList.Items[lbTextureList.ItemIndex]+
              ' уже используется. Удалите все панели, использующие эту текстуру'), PChar('Ошибка'), MB_OK);
    Exit;
   end;
  MainLevel.Map.PanelSystem.DeleteTextureByName(lbTextureList.Items[lbTextureList.ItemIndex]);
  lbTextureList.DeleteSelected;
 end;
end;

procedure TMainForm.aDrawStepExecute(Sender: TObject);
begin
 aDrawStep.Checked := not(aDrawStep.Checked);
 DrawStep := aDrawStep.Checked;
 SelectRect := nil;
 SelectedID := 0;
 SelectedObject := GO_NONE;
end;

procedure TMainForm.aDrawWaterExecute(Sender: TObject);
begin
 aDrawWater.Checked := not(aDrawWater.Checked);
 DrawWater := aDrawWater.Checked;
 SelectRect := nil;
 SelectedID := 0;
 SelectedObject := GO_NONE;
end;

procedure TMainForm.cbPanelTypeChange(Sender: TObject);
begin
 if cbPanelType.ItemIndex in [4, 5, 6] then lbTextureList.Enabled := False
  else lbTextureList.Enabled := True;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 CanClose := (MessageDlg('Уже уходите ?', mtConfirmation, mbOKCancel, 0)) = 1;
end;

procedure TMainForm.aShowMapExecute(Sender: TObject);
begin
 aShowMap.Checked := not(aShowMap.Checked);
 ShowMap := aShowMap.Checked;
end;

procedure TMainForm.aUndoExecute(Sender: TObject);
var
  Panel: TGamePanel;
begin
 if (UndoArray = nil) or (Length(UndoArray) = 0) then Exit;

 with UndoArray[High(UndoArray)] do
 case UndoType of
  U_PANEL:
   begin
    Panel.X := reserved1;
    Panel.Y := reserved2;
    Panel.Height := reserved3;
    Panel.Width := reserved4;
    Panel.TextureHeight := reserved5;
    Panel.TextureWidth := reserved6;
    Panel.Alpha := reserved7;
    Panel.PanelType := TPanelType(reserved8);
    Panel.TextureName := reserved9;

    if UndoSubType = US_DELETE then
     MainLevel.Map.PanelSystem.CreateGamePanel(Panel)
    else
     MainLevel.Map.PanelSystem.PanelsArray[reserved10] := Panel;
   end;
  U_ITEM:
   begin
    if UndoSubType = US_DELETE then
     MainLevel.ItemSystem.CreateItem(TItemType(reserved3), reserved1, reserved2)
    else
    begin
     MainLevel.ItemSystem.ItemsArray[reserved4].GameX := reserved1;
     MainLevel.ItemSystem.ItemsArray[reserved4].GameY := reserved2;
     MainLevel.ItemSystem.ItemsArray[reserved4].ItemType := TItemType(reserved3);
    end;
   end;
  U_MONSTER:
   begin
    if UndoSubType = US_DELETE then
     MainLevel.MonsterSystem.CreateMonster(reserved1, reserved2, TMonsterType(reserved3), TDirection(reserved4))
    else
    begin
     MainLevel.MonsterSystem.MonstersArray[reserved5].GameX := reserved1;
     MainLevel.MonsterSystem.MonstersArray[reserved5].GameY := reserved2;
     MainLevel.MonsterSystem.MonstersArray[reserved5].MonsterType := TMonsterType(reserved3);
     MainLevel.MonsterSystem.MonstersArray[reserved5].Direction := TDirection(reserved4);
    end;
   end;
  U_AREA:
   begin
    if UndoSubType = US_DELETE then
     MainLevel.AreaSystem.CreateArea(reserved1, reserved2, TAreaType(reserved3), TDirection(reserved4))
    else
    begin
     MainLevel.AreaSystem.AreasArray[reserved4].GameX := reserved1;
     MainLevel.AreaSystem.AreasArray[reserved4].GameY := reserved2;
     MainLevel.AreaSystem.AreasArray[reserved4].AreaType := TAreaType(reserved3);
     MainLevel.AreaSystem.AreasArray[reserved4].Direction := TDirection(reserved4);
    end;
   end;
 end;

 SetLength(UndoArray, Length(UndoArray)-1);
end;

procedure TMainForm.aCopyObjectUpdate(Sender: TObject);
begin
 aCopyObject.Enabled := (SelectedObject <> GO_NONE) and (SelectedObject <> GO_TRIGGER);
end;

procedure TMainForm.aCutObjectUpdate(Sender: TObject);
begin
 aCutObject.Enabled := (SelectedObject <> GO_NONE) and (SelectedObject <> GO_TRIGGER);
end;

procedure TMainForm.aPasteObjectExecute(Sender: TObject);
var
  Panel: TGamePanel;
begin
 case Buffer.ObjectType of
  GO_PANEL:
   begin
    Panel.X := Buffer.reserved1+16;
    Panel.Y := Buffer.reserved2+16;
    Panel.Height := Buffer.reserved3;
    Panel.Width := Buffer.reserved4;
    Panel.TextureHeight := Buffer.reserved5;
    Panel.TextureWidth := Buffer.reserved6;
    Panel.Alpha := Buffer.reserved7;
    Panel.PanelType := TPanelType(Buffer.reserved8);
    Panel.TextureName := Buffer.reserved9;
    MainLevel.Map.PanelSystem.CreateGamePanel(Panel); 
   end;
  GO_ITEM: MainLevel.ItemSystem.CreateItem(TItemType(Buffer.reserved3), Buffer.reserved1+16, Buffer.reserved2+16);
  GO_MONSTER: MainLevel.MonsterSystem.CreateMonster(Buffer.reserved1+16, Buffer.reserved2+16,
                                          TMonsterType(Buffer.reserved3),
                                          TDirection(Buffer.reserved4));
  GO_AREA: MainLevel.AreaSystem.CreateArea(Buffer.reserved1+16, Buffer.reserved2+16,
                                 TAreaType(Buffer.reserved3), TDirection(Buffer.reserved4)); 
 end;
end;

procedure TMainForm.aPasteObjectUpdate(Sender: TObject);
begin
 aPasteObject.Enabled := Buffer.ObjectType <> GO_NONE;
end;

procedure TMainForm.aCopyObjectExecute(Sender: TObject);
begin
 Buffer.ObjectType := SelectedObject;
 case SelectedObject of
  GO_PANEL:
   begin
    Buffer.reserved1 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].X;
    Buffer.reserved2 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].Y;
    Buffer.reserved3 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].Height;
    Buffer.reserved4 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].Width;
    Buffer.reserved5 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].TextureHeight;
    Buffer.reserved6 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].TextureWidth;
    Buffer.reserved7 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].Alpha;
    Buffer.reserved8 := Ord(MainLevel.Map.PanelSystem.PanelsArray[SelectedID].PanelType);
    Buffer.reserved9 := MainLevel.Map.PanelSystem.PanelsArray[SelectedID].TextureName;
   end;
  GO_ITEM:
   begin
    Buffer.reserved1 := MainLevel.ItemSystem.ItemsArray[SelectedID].GameX;
    Buffer.reserved2 := MainLevel.ItemSystem.ItemsArray[SelectedID].GameY;
    Buffer.reserved3 := Ord(MainLevel.ItemSystem.ItemsArray[SelectedID].ItemType);
   end;
  GO_MONSTER:
   begin
    Buffer.reserved1 := MainLevel.MonsterSystem.MonstersArray[SelectedID].GameX;
    Buffer.reserved2 := MainLevel.MonsterSystem.MonstersArray[SelectedID].GameY;
    Buffer.reserved3 := Ord(MainLevel.MonsterSystem.MonstersArray[SelectedID].MonsterType);
    Buffer.reserved4 := Ord(MainLevel.MonsterSystem.MonstersArray[SelectedID].Direction);
   end;
  GO_AREA:
   begin
    Buffer.reserved1 := MainLevel.AreaSystem.AreasArray[SelectedID].GameX;
    Buffer.reserved2 := MainLevel.AreaSystem.AreasArray[SelectedID].GameY;
    Buffer.reserved3 := Ord(MainLevel.AreaSystem.AreasArray[SelectedID].AreaType);
    Buffer.reserved4 := Ord(MainLevel.AreaSystem.AreasArray[SelectedID].Direction);
   end;
 end;
end;

procedure TMainForm.aUndoUpdate(Sender: TObject);
begin
 aUndo.Enabled := UndoArray <> nil;
end;

procedure TMainForm.aCutObjectExecute(Sender: TObject);
begin
 aCopyObject.Execute;
 case SelectedObject of
  GO_PANEL: MainLevel.Map.PanelSystem.DeletePanel(SelectedID);
  GO_ITEM: MainLevel.ItemSystem.RemoveItem(SelectedID);
  GO_MONSTER: MainLevel.MonsterSystem.RemoveMonster(SelectedID);
  GO_AREA: MainLevel.AreaSystem.RemoveArea(SelectedID);
 end;
 SelectedObject := GO_NONE;
 vleObjectProperty.Strings.Clear;
 SelectRect := nil;
end;

procedure TMainForm.aCheckMapExecute(Sender: TObject);
begin
 MapCheckForm.Show;
 MapCheckForm.bCheckMap.Click;
end;

procedure TMainForm.aSaveAsDFLExecute(Sender: TObject);
var
  i, c, a:  Integer;
  FileName: String;
  Header:   TMapHeaderRec;
  MapFile:  File;
  TempPanelsArray:   packed Array of TPanelRec;
  TempTexturesArray: packed Array of TTextureRec;
  TempItemsArray:    packed Array of TItemRec;
  TempMonstersArray: packed Array of TMonsterRec;
  TempAreasArray:    packed Array of TAreaRec;
  TempTriggersArray: packed Array of TTriggerRec;
  TempStr: String;
  TempTextureName: Char64;
  OldLevelHeader: TLevelHeaderRec;
  OldMapRecArray: packed Array of TMapRec;
  Signature:  Array[0..2] of Char;
  Version: Word;
  MapImage: Pointer;
  NewMapSize: LongWord;
  MemoryAddress: LongWord;
  LevelImage: Pointer;
  LevelImageSize: LongWord;
  MapName: String;
  OldMapImage: Pointer;
  OldMapSize: LongWord;
  NewLevelHeader: TLevelHeaderRec;
  NewMapRecArray: packed Array of TMapRec;
  MapExists: Boolean;
begin
 if not(SaveDialog.Execute) then Exit
  else FileName := SaveDialog.FileName;

 OldMapSize := 0;
 OldMapImage := nil;
 NewMapRecArray := nil;
 NewLevelHeader.MapCount := 0;
 MapExists := False;

 if FileExists(FileName) then
 begin
  AssignFile(MapFile, FileName);
  Reset(MapFile, 1);

  BlockRead(MapFile, Signature, SizeOf(MapSignature));

  if Signature <> MapSignature then
  begin
   CloseFile(MapFile);
   Exit;
  end;

  BlockRead(MapFile, Version, SizeOf(Version));

  BlockRead(MapFile, OldLevelHeader, SizeOf(TLevelHeaderRec));
  SetLength(OldMapRecArray, OldLevelHeader.MapCount);
  BlockRead(MapFile, OldMapRecArray[0], SizeOf(TMapRec)*OldLevelHeader.MapCount);

  CloseFile(MapFile);
 end;

 SaveMapForm.lbMapList.Clear;
 if OldMapRecArray <> nil then
 for i := 0 to High(OldMapRecArray) do
  SaveMapForm.lbMapList.Items.Add(OldMapRecArray[i].MapName);

 if SaveMapForm.ShowModal <> mrOk then Exit;

 MapName := SaveMapForm.eMapName.Text;
 if Length(MapName) > 16 then SetLength(MapName, 16);

 // Читаем в память старые карты
 if OldLevelHeader.MapCount <> 0 then
 begin
  for i := 0 to High(OldMapRecArray) do
   if not(OldMapRecArray[i].MapName = MapName) then
    OldMapSize := OldMapSize+OldMapRecArray[i].Length;

  GetMem(OldMapImage, OldMapSize);
  MemoryAddress := Integer(OldMapImage);

  AssignFile(MapFile, FileName);
  Reset(MapFile, 1);

  for i := 0 to High(OldMapRecArray) do
  if not(OldMapRecArray[i].MapName = MapName) then
  begin
   Seek(MapFile, OldMapRecArray[i].Address);
   BlockRead(MapFile, Pointer(MemoryAddress)^, OldMapRecArray[i].Length);
   MemoryAddress := MemoryAddress+OldMapRecArray[i].Length;

   SetLength(NewMapRecArray, Length(NewMapRecArray)+1);
   NewMapRecArray[High(NewMapRecArray)].MapName := OldMapRecArray[i].MapName;
   NewMapRecArray[High(NewMapRecArray)].Address := 0;
   NewMapRecArray[High(NewMapRecArray)].Length := OldMapRecArray[i].Length;
   NewLevelHeader.MapCount := NewLevelHeader.MapCount+1;
  end;

  CloseFile(MapFile);
 end;

 // Подсчитываем размер новой карты
 NewMapSize := 0;
 NewMapSize := NewMapSize+SizeOf(TMapHeaderRec);
 NewMapSize := NewMapSize+MainLevel.Map.PanelSystem.TexturesCount*SizeOf(TTextureRec);
 NewMapSize := NewMapSize+MainLevel.Map.PanelSystem.PanelCount*SizeOf(TPanelRec);
 NewMapSize := NewMapSize+MainLevel.ItemSystem.GetCount*SizeOf(TItemRec);
 NewMapSize := NewMapSize+MainLevel.MonsterSystem.GetCount*SizeOf(TMonsterRec);
 NewMapSize := NewMapSize+MainLevel.AreaSystem.GetCount*SizeOf(TAreaRec);
 NewMapSize := NewMapSize+MainLevel.TriggersSystem.Count*SizeOf(TTriggerRec);
 
 GetMem(MapImage, NewMapSize);

 // Записываем карту в память
 Header.MapName := MainLevel.Map.FName;
 Header.MapDescription := MainLevel.Map.FDescription;
 Header.MusicName := MainLevel.Map.FMusicName;
 Header.Height := MainLevel.Map.FHeight;
 Header.Width := MainLevel.Map.FWidth;
 Header.TexturesCount := MainLevel.Map.PanelSystem.TexturesCount;
 Header.PanelsCount := MainLevel.Map.PanelSystem.PanelCount;
 Header.ItemsCount := MainLevel.ItemSystem.GetCount;
 Header.MonstersCount := MainLevel.MonsterSystem.GetCount;
 Header.AreasCount := MainLevel.AreaSystem.GetCount;
 Header.TriggersCount := MainLevel.TriggersSystem.Count;

 MemoryAddress := Integer(MapImage);
 CopyMemory(Pointer(MemoryAddress), @Header, SizeOf(Header));
 MemoryAddress := MemoryAddress+SizeOf(Header);

 if Header.TexturesCount > 0 then
 begin
  c := 0;

  SetLength(TempTexturesArray, Header.TexturesCount);
  for i := 0 to High(MainLevel.Map.PanelSystem.LevelTexturesArray) do
   if MainLevel.Map.PanelSystem.LevelTexturesArray[i].TextureName <> '' then
   begin
    TempStr := MainLevel.Map.PanelSystem.LevelTexturesArray[i].TextureName;
    if Length(TempStr) > 64 then SetLength(TempStr, 64);
    CopyMemory(@TempTexturesArray[c].TextureName[0], @TempStr[1], Length(TempStr));
    Inc(c);
   end;

  CopyMemory(Pointer(MemoryAddress), @TempTexturesArray[0], SizeOf(TTextureRec)*Header.TexturesCount);
  MemoryAddress := MemoryAddress+SizeOf(TTextureRec)*Header.TexturesCount;
 end;

 if Header.PanelsCount > 0 then
 begin
  c := 0;

  SetLength(TempPanelsArray, Header.PanelsCount);
  for i := 0 to High(MainLevel.Map.PanelSystem.PanelsArray) do
  with MainLevel.Map.PanelSystem.PanelsArray[i] do
   if ((TextureName = '') and (PanelType in [PANEL_WATER, PANEL_ACID1, PANEL_ACID2])) or
       (TextureName <> '') then
   begin
    TempPanelsArray[c].X := X;
    TempPanelsArray[c].Y := Y;
    TempPanelsArray[c].Width := Width;
    TempPanelsArray[c].Height := Height;
    if not(PanelType in [PANEL_WATER, PANEL_ACID1, PANEL_ACID2]) then
    begin
     if TempTexturesArray <> nil then
      for a := 0 to High(TempTexturesArray) do
      begin
       ZeroMemory(@TempTextureName, SizeOf(TempTextureName));
       CopyMemory(@TempTextureName[0], @TextureName[1], Length(TextureName));
       if TempTexturesArray[a].TextureName = TempTextureName then
       begin
        TempPanelsArray[c].TextureNum := a;
        Break;
       end;
      end
     else TempPanelsArray[c].TextureNum := 0;
    end;
    TempPanelsArray[c].PanelType := Ord(PanelType);
    TempPanelsArray[c].Alpha := Alpha;
    TempPanelsArray[c].Effects := 0;
    Inc(c);
   end;

  CopyMemory(Pointer(MemoryAddress), @TempPanelsArray[0], SizeOf(TPanelRec)*Header.PanelsCount);
  MemoryAddress := MemoryAddress+SizeOf(TPanelRec)*Header.PanelsCount;

  TempPanelsArray := nil;
  TempTexturesArray := nil;
 end;

 if Header.ItemsCount > 0 then
 begin
  c := 0;

  SetLength(TempItemsArray, Header.ItemsCount);
  for i := 0 to High(MainLevel.ItemSystem.ItemsArray) do
   if MainLevel.ItemSystem.ItemsArray[i].ItemType <> ITEM_NONE then
   with MainLevel.ItemSystem.ItemsArray[i] do
   begin
    TempItemsArray[c].X := GameX;
    TempItemsArray[c].Y := GameY;
    TempItemsArray[c].ItemType := Ord(ItemType);
    TempItemsArray[c].OnlyDM := False;
    Inc(c);
   end;

  CopyMemory(Pointer(MemoryAddress), @TempItemsArray[0], SizeOf(TItemRec)*Header.ItemsCount);
  MemoryAddress := MemoryAddress+SizeOf(TItemRec)*Header.ItemsCount;

  TempItemsArray := nil;
 end;

 if Header.MonstersCount > 0 then
 begin
  c := 0;

  SetLength(TempMonstersArray, Header.MonstersCount);
  for i := 0 to High(MainLevel.MonsterSystem.MonstersArray) do
   if MainLevel.MonsterSystem.MonstersArray[i].MonsterType <> MONSTER_NONE then
   with MainLevel.MonsterSystem.MonstersArray[i] do
   begin
    TempMonstersArray[c].X := GameX;
    TempMonstersArray[c].Y := GameY;
    TempMonstersArray[c].MonsterType := Ord(MonsterType);
    TempMonstersArray[c].Active := False;
    TempMonstersArray[c].Direction := Ord(Direction);
    Inc(c);
   end;

  CopyMemory(Pointer(MemoryAddress), @TempMonstersArray[0], SizeOf(TMonsterRec)*Header.MonstersCount);
  MemoryAddress := MemoryAddress+SizeOf(TMonsterRec)*Header.MonstersCount;

  TempMonstersArray := nil;
 end;

 if Header.AreasCount > 0 then
 begin
  c := 0;

  SetLength(TempAreasArray, Header.AreasCount);
  for i := 0 to High(MainLevel.AreaSystem.AreasArray) do
   if MainLevel.AreaSystem.AreasArray[i].AreaType <> AREA_NONE then
   with MainLevel.AreaSystem.AreasArray[i] do
   begin
    TempAreasArray[c].X := GameX;
    TempAreasArray[c].Y := GameY;
    TempAreasArray[c].AreaType := Ord(AreaType);
    TempAreasArray[c].Direction := Ord(Direction);
    Inc(c);
   end;

  CopyMemory(Pointer(MemoryAddress), @TempAreasArray[0], SizeOf(TAreaRec)*Header.AreasCount);
  MemoryAddress := MemoryAddress+SizeOf(TAreaRec)*Header.AreasCount;

  TempAreasArray := nil;
 end;

 if Header.TriggersCount > 0 then
 begin
  c := 0;

  SetLength(TempTriggersArray, Header.TriggersCount);
  for i := 0 to High(MainLevel.TriggersSystem.TriggersArray) do
   if MainLevel.TriggersSystem.TriggersArray[i].TriggerType <> TRIGGER_NONE then
   with MainLevel.TriggersSystem.TriggersArray[i] do
   begin
    TempTriggersArray[c].X := X;
    TempTriggersArray[c].Y := Y;
    TempTriggersArray[c].Width := Width;
    TempTriggersArray[c].Height := Height;
    TempTriggersArray[c].TriggerType := Ord(TriggerType);
    TempTriggersArray[c].TriggerActivateType := TriggerActivateType;
    TempTriggersArray[c].TriggerKey := TriggerActivateKey;
    TempTriggersArray[c].Default := Default;
    Inc(c);
   end;

  CopyMemory(Pointer(MemoryAddress), @TempTriggersArray[0], SizeOf(TTriggerRec)*Header.TriggersCount);

  TempAreasArray := nil;
 end;

 NewLevelHeader.MapCount := NewLevelHeader.MapCount+1;

 // Подсчитываем размер левела
 LevelImageSize := 0;
 if NewMapRecArray <> nil then
 for i := 0 to High(NewMapRecArray) do
  LevelImageSize := LevelImageSize+NewMapRecArray[i].Length;
 LevelImageSize := LevelImageSize+SizeOf(MapSignature);
 LevelImageSize := LevelImageSize+SizeOf(MapVersion);
 LevelImageSize := LevelImageSize+SizeOf(TLevelHeaderRec);
 LevelImageSize := LevelImageSize+SizeOf(TMapRec)*NewLevelHeader.MapCount;
 LevelImageSize := LevelImageSize+NewMapSize;

 GetMem(LevelImage, LevelImageSize);

 // Начинаем заполнять левел в памяти
 MemoryAddress := Integer(LevelImage);
 CopyMemory(Pointer(MemoryAddress), @MapSignature, SizeOf(MapSignature));
 MemoryAddress := MemoryAddress+SizeOf(MapSignature);
 CopyMemory(Pointer(MemoryAddress), @MapVersion, SizeOf(MapVersion));
 MemoryAddress := MemoryAddress+SizeOf(MapVersion);
 CopyMemory(Pointer(MemoryAddress), @NewLevelHeader, SizeOf(TLevelHeaderRec));
 MemoryAddress := MemoryAddress+SizeOf(TLevelHeaderRec);

 SetLength(NewMapRecArray, Length(NewMapRecArray)+1);
 NewMapRecArray[High(NewMapRecArray)].Length := NewMapSize;
 
 if NewMapRecArray <> nil then
 for i := 0 to High(NewMapRecArray) do
 begin
  if i = 0 then NewMapRecArray[i].Address :=
   MemoryAddress+SizeOf(TMapRec)*NewLevelHeader.MapCount-LongWord(LevelImage)
   else NewMapRecArray[i].Address := NewMapRecArray[i-1].Address+NewMapRecArray[i-1].Length;
 end;

 CopyMemory(@NewMapRecArray[High(NewMapRecArray)].MapName[0], @MapName[1], Length(MapName));

 CopyMemory(Pointer(MemoryAddress), @NewMapRecArray[0], SizeOf(TMapRec)*NewLevelHeader.MapCount);
 MemoryAddress := MemoryAddress+SizeOf(TMapRec)*NewLevelHeader.MapCount;

 CopyMemory(Pointer(MemoryAddress), OldMapImage, OldMapSize);
 MemoryAddress := MemoryAddress+OldMapSize;

 CopyMemory(Pointer(MemoryAddress), MapImage, NewMapSize);

 AssignFile(MapFile, FileName);
 Rewrite(MapFile, 1);

 BlockWrite(MapFile, LevelImage^, LevelImageSize);

 CloseFile(MapFile);

 FreeMem(OldMapImage);
 FreeMem(MapImage);
 FreeMem(LevelImage);
end;

{ TEditorMap }

procedure TEditorMap.ClearMap;
begin
 PanelSystem.RemoveAllTexture;
 PanelSystem.RemoveAllPanels;

 FDescription := '';
 FName := '';
 FHeight := 480;
 FWidth := 640;
 FVersion := MapVersion;
 FMapsList := nil;
end;

constructor TEditorMap.Create;
begin
 PanelSystem := TEditorPanelSystem.Create;
end;

destructor TEditorMap.Destroy;
begin

end;

function TEditorMap.LoadFrom(Filename: String; MapName: String): Boolean;
var
  MapFile: File;
  Header:            TMapHeaderRec;
  TempPanelsArray:   Array of TPanelRec;
  TempTexturesArray: Array of TTextureRec;
  TempItemsArray:    Array of TItemRec;
  TempMonstersArray: Array of TMonsterRec;
  TempAreasArray:    Array of TAreaRec;
  TempTriggersArray: Array of TTriggerRec;
  i: DWORD;
  Trigger: TTrigger;
  LevelHeader: TLevelHeaderRec;
  MapRecArray: Array of TMapRec;
begin
 Result := False;
 if not(FileExists(FileName)) then Exit;

 try
  AssignFile(MapFile, Filename);
  Reset(MapFile, 1);

  Seek(MapFile, SizeOf(MapSignature)+SizeOf(Version));

  BlockRead(MapFile, LevelHeader, SizeOf(TLevelHeaderRec));
  SetLength(MapRecArray, LevelHeader.MapCount);
  BlockRead(MapFile, MapRecArray[0], SizeOf(TMapRec)*LevelHeader.MapCount);

  for i := 0 to High(MapRecArray) do
  begin
   SetLength(FMapsList, Length(FMapsList)+1);
   FMapsList[High(FMapsList)] := MapRecArray[i].MapName;
  end;

  for i := 0 to High(MapRecArray) do
   if MapRecArray[i].MapName = MapName then
   begin
    Seek(MapFile, MapRecArray[i].Address);
    Break;
   end;

  BlockRead(MapFile, Header, SizeOf(Header));

  Self.FDescription := Header.MapDescription;
  Self.FName := Header.MapName;
  Self.FWidth := Header.Width;
  Self.FHeight := Header.Height;

  // Загрузка массива текстур
  SetLength(TempTexturesArray, Header.TexturesCount);
  BlockRead(MapFile, TempTexturesArray[0], SizeOf(TTextureRec)*Header.TexturesCount);
  if TempTexturesArray <> nil then
  begin
   for i := 0 to High(TempTexturesArray) do
    PanelSystem.CreateTexture(TempTexturesArray[i].TextureName);
   TempTexturesArray := nil;
  end;

  // Загрузка массива панелей
  SetLength(TempPanelsArray, Header.PanelsCount);
  BlockRead(MapFile, TempPanelsArray[0], SizeOf(TPanelRec)*Header.PanelsCount);
  if TempPanelsArray <> nil then
  begin
   for i := 0 to High(TempPanelsArray) do
    PanelSystem.CreateRecPanel(TempPanelsArray[i]);
   TempPanelsArray := nil;
  end;

  // Загрузка массива итемов
  SetLength(TempItemsArray, Header.ItemsCount);
  BlockRead(MapFile, TempItemsArray[0], SizeOf(TItemRec)*Header.ItemsCount);
  if TempItemsArray <> nil then
  begin
   for i := 0 to High(TempItemsArray) do
    with TempItemsArray[i] do
    begin
      MainLevel.ItemSystem.CreateItem(TItemType(ItemType), X, Y);
    end;
   TempItemsArray := nil;
  end;

  // Загрузка массива монстров
  SetLength(TempMonstersArray, Header.MonstersCount);
  BlockRead(MapFile, TempMonstersArray[0], SizeOf(TMonsterRec)*Header.MonstersCount);
  if TempMonstersArray <> nil then
  begin
   for i := 0 to High(TempMonstersArray) do
    with TempMonstersArray[i] do
    begin
     MainLevel.MonsterSystem.CreateMonster(X, Y, TMonsterType(MonsterType), TDirection(Direction));
    end;
   TempMonstersArray := nil;
  end;

  // Загрузка массива областей
  SetLength(TempAreasArray, Header.AreasCount);
  BlockRead(MapFile, TempAreasArray[0], SizeOf(TAreaRec)*Header.AreasCount);
  if TempAreasArray <> nil then
  begin
   for i := 0 to High(TempAreasArray) do
    with TempAreasArray[i] do
    begin
     MainLevel.AreaSystem.CreateArea(X, Y, TAreaType(AreaType), TDirection(Direction));
    end;
   TempAreasArray := nil;
  end;

  SetLength(TempTriggersArray, Header.TriggersCount);
  BlockRead(MapFile, TempTriggersArray[0], SizeOf(TTriggerRec)*Header.TriggersCount);
  if TempTriggersArray <> nil then
  begin
   for i := 0 to High(TempTriggersArray) do
    with TempTriggersArray[i] do
    begin
     Trigger.X := X;
     Trigger.Y := Y;
     Trigger.Width := Width;
     Trigger.Height := Height;
     Trigger.TriggerType := TTriggerType(TriggerType);
     Trigger.TriggerActivateType := TriggerActivateType;
     Trigger.TriggerActivateKey := TriggerKey;
     Trigger.Default := Default;
     MainLevel.TriggersSystem.CreateTrigger(Trigger); 
    end;
   TempAreasArray := nil;
  end;

  CloseFile(MapFile);
 except
  CloseFile(MapFile);
  Exit;
 end;

 Result := True;
end;

procedure TEditorMap.LoadOldMap(Filename: String);
var
  Panel: TGamePanel;
  Ini: TIniFile;
  i,p: Integer;
  TextureName: String;
  TempStr: String;
  TexturesCount: Word;
  PanelCount: Word;
  ItemCount: Word;
  MonsterCount: Word;
  AreaCount: Word;
begin
 Ini := TIniFile.Create(FileName);

 TempStr := Ini.ReadString('MapOptions', 'MapName', '');
 if Length(TempStr) > 32 then SetLength(TempStr, 32);
 CopyMemory(@FName[0], @TempStr[1], Length(TempStr));

 TempStr := Ini.ReadString('MapOptions', 'MapDescription', '');
 if Length(TempStr) > 256 then SetLength(TempStr, 256);
 CopyMemory(@FDescription[0], @TempStr[1], Length(TempStr));

 TexturesCount := Ini.ReadInteger('MapOptions', 'TextureCount', 0);
 PanelCount := Ini.ReadInteger('MapOptions', 'PanelCount', 0);
 ItemCount := Ini.ReadInteger('MapOptions', 'ItemCount', 0);
 MonsterCount := Ini.ReadInteger('MapOptions', 'MonsterCount', 0);
 AreaCount := Ini.ReadInteger('MapOptions', 'AreaCount', 0);

 FWidth := Ini.ReadInteger('MapOptions', 'Width', 640);
 FHeight := Ini.ReadInteger('MapOptions', 'Height', 480);

 MainForm.Resize;

 ChDir(EditorDir);

 for i := 1 to TexturesCount do
 begin
  TextureName := UpperCase(Ini.ReadString('Textures', 'TextureName'+intToStr(i), ''));
  if TextureName  <> '' then
  begin
   PanelSystem.CreateTexture('Standart.wad:STDTEXTURES\'+TextureName);
   MainForm.lbTextureList.Items.Add('Standart.wad:STANDARTTEXTURES\'+TextureName)
   end;
 end;

 for i := 1 to PanelCount do
 begin
  ZeroMemory(@Panel, SizeOf(TGamePanel));
  Panel.X := Ini.ReadInteger('Panel'+IntToStr(i), 'X1', 0);
  Panel.Y := Ini.ReadInteger('Panel'+IntToStr(i), 'Y1', 0);
  Panel.Height := Ini.ReadInteger('Panel'+IntToStr(i), 'Height', 0);
  Panel.Width := Ini.ReadInteger('Panel'+IntToStr(i), 'Width', 0);
  Panel.TextureName := 'Standart.wad:STANDARTTEXTURES\'+UpperCase(Ini.ReadString('Panel'+IntToStr(i), 'TextureName', ''));
  p := Ini.ReadInteger('Panel'+IntToStr(i), 'PanelType', 0);
  case p of
   0: Panel.PanelType := PANEL_WALL;
   1: Panel.PanelType := PANEL_BACKGROUND;
   2: Panel.PanelType := PANEL_FOREGROUND;
   3: Panel.PanelType := PANEL_STEP;
   4: Panel.PanelType := PANEL_WATER;
   5: Panel.PanelType := PANEL_ACID1;
   6: Panel.PanelType := PANEL_ACID2;
  end;

  if not(Panel.PanelType in [PANEL_WATER, PANEL_ACID1, PANEL_ACID2]) then
   Panel.Alpha := Ini.ReadInteger('Panel'+IntToStr(i), 'Alpha', 0);

  PanelSystem.CreateGamePanel(Panel);
 end;

 for i := 1 to ItemCount do
 begin
  MainLevel.ItemSystem.CreateItem(TItemType(Ini.ReadInteger('Item'+IntToStr(i), 'Type', 0)),
                        Ini.ReadInteger('Item'+IntToStr(i), 'X', 0),
                        Ini.ReadInteger('Item'+IntToStr(i), 'Y', 0));
 end;

 for i := 1 to MonsterCount do
 begin
  MainLevel.MonsterSystem.CreateMonster(Ini.ReadInteger('Monster'+IntToStr(i), 'X', 0),
                              Ini.ReadInteger('Monster'+IntToStr(i), 'Y', 0),
                              TMonsterType(Ini.ReadInteger('Monster'+IntToStr(i), 'Type', 0)),
                              TDirection(Ini.ReadInteger('Monster'+IntToStr(i), 'Direction', 0)));
 end;

 for i := 1 to AreaCount do
 begin
  MainLevel.AreaSystem.CreateArea(Ini.ReadInteger('Area'+IntToStr(i), 'X', 0),
                        Ini.ReadInteger('Area'+IntToStr(i), 'Y', 0),
                        TAreaType(Ini.ReadInteger('Area'+IntToStr(i), 'Type', 0)),
                        TDirection(Ini.ReadInteger('Area'+IntToStr(i), 'Direction', 0)));
 end;
end;

{ TEditorPanelSystem }

procedure TEditorPanelSystem.CreateGamePanel(Panel: TGamePanel);
var
  ID: DWORD;
begin
 ID := FindPanel;
 
 PanelsArray[ID] := Panel;

 if not(Panel.PanelType in [PANEL_WATER, PANEL_ACID1, PANEL_ACID2]) then
 begin
  GetTextureByName(PanelsArray[ID].TextureName, PanelsArray[ID].TextureID);
  GetTextureSizeByName(PanelsArray[ID].TextureName, PanelsArray[ID].TextureWidth,
                       PanelsArray[ID].TextureHeight);
 end;
end;

procedure TEditorPanelSystem.CreateRecPanel(Panel: TPanelRec);
var
  ID: DWORD;
begin
 ID := FindPanel;
 with PanelsArray[ID] do
 begin
  X := Panel.X;
  Y := Panel.Y;
  Width := Panel.Width;
  Height := Panel.Height;
  PanelType := TPanelType(Panel.PanelType);
  if not(TPanelType(Panel.PanelType) in [PANEL_WATER, PANEL_ACID1, PANEL_ACID2]) then
  begin
   TextureHeight := LevelTexturesArray[Panel.TextureNum].Height;
   TextureWidth := LevelTexturesArray[Panel.TextureNum].Width;
   TextureID := LevelTexturesArray[Panel.TextureNum].TextureID;
   TextureName := LevelTexturesArray[Panel.TextureNum].TextureName;
   Alpha := Panel.Alpha;
  end;
 end;
end;

procedure TEditorPanelSystem.CreateTexture(ResourceStr: String);
var
  WAD: TWADReader;
  i:   Integer;
  TextureData: Pointer;
  WADName:     String;
  SectionName: String;
  TextureName: String;
  a: Integer;
  ResLength: Integer;
begin
 // Standart.wad:SECTION\STD_1_16_16
 for i := 1 to Length(ResourceStr) do
  if ResourceStr[i] = ':' then Break;

 WADName := Copy(ResourceStr, 1, i-1);
                                                         
 for a := i+1 to Length(ResourceStr) do
  if ResourceStr[a] = '\' then Break;

 TextureName := Copy(ResourceStr, a+1, Length(ResourceStr)-a);

 SectionName := Copy(ResourceStr, i+1, Length(ResourceStr)-Length(TextureName)-Length(WADName)-2);


 WAD := TWADReader.Create(EditorDir+'\WADS\'+WADName);
 if WAD.Read(SectionName, TextureName, TextureData, ResLength) then
 begin
  SetLength(LevelTexturesArray, Length(LevelTexturesArray)+1);
  e_CreateTextureMem(TextureData, LevelTexturesArray[High(LevelTexturesArray)].TextureID);
  e_GetTextureSize(LevelTexturesArray[High(LevelTexturesArray)].TextureID,
                   LevelTexturesArray[High(LevelTexturesArray)].Width,
                   LevelTexturesArray[High(LevelTexturesArray)].Height);
  FreeMem(TextureData);
  LevelTexturesArray[High(LevelTexturesArray)].TextureName := ResourceStr;
 end
  else
 begin
  e_WriteLog(Format('Error loading texture %s', [ResourceStr]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
 end;
 WAD.Destroy;
end;

procedure TEditorPanelSystem.DeletePanel(ID: DWORD);
begin
 ZeroMemory(@PanelsArray[ID], SizeOf(TGamePanel));
end;

procedure TEditorPanelSystem.DeleteTextureByName(TextureName: ShortString);
var
  i: DWORD;
begin
 if LevelTexturesArray = nil then Exit;

 for i := 0 to High(LevelTexturesArray) do
  if LevelTexturesArray[i].TextureName = TextureName then
  begin
   e_DeleteTexture(LevelTexturesArray[i].TextureID);
   LevelTexturesArray[i].TextureName := '';
   LevelTexturesArray[i].Width := 0;
   LevelTexturesArray[i].Height := 0;
   Exit;
  end;
end;

function TEditorPanelSystem.FindPanel: DWORD;
var
  i: Integer;
begin
if PanelsArray <> nil then
 for i := 0 to High(PanelsArray) do
  if PanelsArray[i].Width = 0 then
  begin
   Result := i;
   Exit;
  end;

if PanelsArray = nil then
begin
 SetLength(PanelsArray, 1);
 Result := 0;
end
 else
begin
 Result := High(PanelsArray) + 1;
 SetLength(PanelsArray, Length(PanelsArray) + 1);
end;
end;

function TEditorPanelSystem.GetPanelCount: Word;
var
  i: DWORD;
begin
 Result := 0;

 if PanelsArray = nil then Exit;

 for i := 0 to High(PanelsArray) do
  if PanelsArray[i].Width <> 0 then Inc(Result);
end;
                          
procedure TEditorPanelSystem.GetTextureByName(TextureName: String;
  var ID: DWORD);
var
  i: DWORD;
begin
 if LevelTexturesArray = nil then Exit;

 for i := 0 to High(LevelTexturesArray) do
  if LevelTexturesArray[i].TextureName = TextureName then
  begin
   ID := LevelTexturesArray[i].TextureID;
   Break;
  end;
end;

function TEditorPanelSystem.GetTexturesCount: Word;
var
  i: DWORD;
begin
 Result := 0;
 if LevelTexturesArray = nil then Exit;

 for i := 0 to High(LevelTexturesArray) do
  if LevelTexturesArray[i].TextureName <> '' then Inc(Result);
end;

procedure TEditorPanelSystem.GetTextureSizeByName(TextureName: String;
  var Width, Height: Word);
var
  i: DWORD;
begin
 if LevelTexturesArray = nil then Exit;

 for i := 0 to High(LevelTexturesArray) do
  if LevelTexturesArray[i].TextureName = TextureName then
  begin
   Width := LevelTexturesArray[i].Width;
   Height := LevelTexturesArray[i].Height;
   Break;
  end;
end;

procedure TEditorPanelSystem.RemoveAllPanels;
begin
 PanelsArray := nil;
end;

procedure TEditorPanelSystem.RemoveAllTexture;
var
  i: DWORD;
begin
 if LevelTexturesArray = nil then Exit;

 for i := 0 to High(LevelTexturesArray) do
  e_DeleteTexture(LevelTexturesArray[i].TextureID);

 LevelTexturesArray := nil;
end;

procedure TEditorPanelSystem.RenderPanels(XInc, YInc: Integer;
  PanelType: TPanelType);
var
  i: Integer;
begin
 if PanelsArray = nil then Exit;

 for i := 0 to High(PanelsArray) do
  if (PanelsArray[i].Width <> 0) and (PanelsArray[i].PanelType = PanelType) then
  with PanelsArray[i] do
  begin
   if not(PanelType in [PANEL_WATER, PANEL_ACID1, PANEL_ACID2]) then
    e_DrawFill(TextureID, X+XInc, Y+YInc, Width div TextureWidth,
               Height div TextureHeight, Alpha, False, False)

   else
    case PanelType of
     PANEL_WATER: e_DrawFillQuad(X+XInc, Y+YInc, X+XInc+Width, Y+YInc+Height,
                                 52, 62, 180, 100, False);
     PANEL_ACID1: e_DrawFillQuad(X+XInc, Y+YInc, X+XInc+Width, Y+YInc+Height,
                                 64, 127, 0, 80, False);
     PANEL_ACID2: e_DrawFillQuad(X+XInc, Y+YInc, X+XInc+Width, Y+YInc+Height,
                                 214, 39, 44, 60, False);
    end;
  end;
end;

procedure TMainForm.aOpenDFLExecute(Sender: TObject);
var
  i: DWORD;
  MapFile: File;
  LevelHeader: TLevelHeaderRec;
  MapRecArray: Array of TMapRec;
  Signature:  Array[0..2] of Char;
  Version: Word;
  MapName: String;
begin
 OpenDialog.Filter := 'Doom2D: Forever levels (*.dfl)|*.dfl|All files (*.*)|*.';
 OpenDialog.DefaultExt := 'dfl';
 if OpenDialog.Execute then
 begin
  AssignFile(MapFile, OpenDialog.FileName);
  Reset(MapFile, 1);

  BlockRead(MapFile, Signature, SizeOf(MapSignature));

  if Signature <> MapSignature then
  begin
   CloseFile(MapFile);
   Exit;
  end;

  BlockRead(MapFile, Version, SizeOf(Version));

  BlockRead(MapFile, LevelHeader, SizeOf(TLevelHeaderRec));
  SetLength(MapRecArray, LevelHeader.MapCount);
  BlockRead(MapFile, MapRecArray[0], SizeOf(TMapRec)*LevelHeader.MapCount);

  CloseFile(MapFile);

  SelectMapForm.lbMapList.Clear;
  if MapRecArray <> nil then
  for i := 0 to High(MapRecArray) do
  SelectMapForm.lbMapList.Items.Add(MapRecArray[i].MapName);

  if SelectMapForm.ShowModal = mrCancel then Exit;

  MainLevel.FreeLevel;
  lbTextureList.Clear;

  MainForm.lTextureHeight.Caption := '';
  MainForm.lTextureWidth.Caption := '';
  SelectRect := nil;
  MouseDowned := False;
  MouseDrag := False;
  SelectedID := 0;
  SelectedObject := GO_NONE;
  MainForm.vleObjectProperty.Strings.Clear;
  MainForm.Resize;
  UndoArray := nil;
  ScrollBar1.Position := 0;
  ScrollBar2.Position := 0;

  MapName := SelectMapForm.lbMapList.Items[SelectMapForm.lbMapList.ItemIndex];
  if MapName = '' then Exit;

  MainLevel.Map.LoadFrom(OpenDialog.FileName, MapName);
  if MainLevel.Map.PanelSystem.LevelTexturesArray <> nil then
   for i := 0 to High(MainLevel.Map.PanelSystem.LevelTexturesArray) do
    lbTextureList.Items.Add(MainLevel.Map.PanelSystem.LevelTexturesArray[i].TextureName);

  MainForm.Resize;
 end;
end;

{ TLevel }

constructor TLevel.Create;
begin
 Map := TEditorMap.Create;
 ItemSystem := TItemSystem.Create;
 MonsterSystem := TMonsterSystem.Create;
 AreaSystem := TAreaSystem.Create;
 TriggersSystem := TTriggersSystem.Create;
end;

destructor TLevel.Destroy;
begin

end;

procedure TLevel.FreeLevel;
begin
 Map.ClearMap;
 ItemSystem.ItemsArray := nil;
 MonsterSystem.MonstersArray := nil;
 AreaSystem.AreasArray := nil;
 TriggersSystem.RemoveAll;
 X := 0;
 Y := 0;
end;

procedure TLevel.Render;
begin
 if DrawBackground then Map.PanelSystem.RenderPanels(X, Y, PANEL_BACKGROUND);
 if DrawWall then Map.PanelSystem.RenderPanels(X, Y, PANEL_WALL);
 if DrawStep then Map.PanelSystem.RenderPanels(X, Y, PANEL_STEP);
 if DrawWater then
 begin
  Map.PanelSystem.RenderPanels(X, Y, PANEL_WATER);
  Map.PanelSystem.RenderPanels(X, Y, PANEL_ACID1);
  Map.PanelSystem.RenderPanels(X, Y, PANEL_ACID2);
 end;
 ItemSystem.Render(X, Y);
 MonsterSystem.Render(X, Y);
 AreaSystem.Render(X, Y);
 if DrawForeground then Map.PanelSystem.RenderPanels(X, Y, PANEL_FOREGROUND);
 TriggersSystem.Render(X, Y); 
end;

procedure TMainForm.aOptimizeExecute(Sender: TObject);
begin
 MapOptimizationForm.ShowModal;
end;

procedure TMainForm.aChangeDotStepExecute(Sender: TObject);
begin
 if PointStep = 8 then PointStep := 16 else PointStep := 8;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
 RenderPanel.SetFocus;
end;

procedure TMainForm.bApplyPropertyClick(Sender: TObject);
var
  temp: String;
  a: Integer;
begin
 if (SelectRect = nil) or (SelectedObject = GO_NONE) then Exit;

 if SelectedObject = GO_PANEL then
 begin
  if Calculate(vleObjectProperty.Values['Width']) mod
     MainLevel.Map.PanelSystem.PanelsArray[SelectedID].TextureWidth <> 0 then
  begin
   MessageBox(MainForm.Handle, PChar(Format('Длина панели должна быть кратна длине текстуры (%d)',
              [MainLevel.Map.PanelSystem.PanelsArray[SelectedID].TextureWidth])), PChar('Ошибка'), MB_OK);
   Exit;
  end;

  if Calculate(vleObjectProperty.Values['Height']) mod
     MainLevel.Map.PanelSystem.PanelsArray[SelectedID].TextureHeight <> 0 then
  begin
   MessageBox(MainForm.Handle, PChar(Format('Высота панели должна быть кратна высоте текстуры (%d)',
              [MainLevel.Map.PanelSystem.PanelsArray[SelectedID].TextureHeight])), PChar('Ошибка'), MB_OK);
   Exit;
  end;

  if (Calculate(vleObjectProperty.Values['Width']) <= 0) or
     (Calculate(vleObjectProperty.Values['Height']) <= 0) then
  begin
   MessageBox(MainForm.Handle, PChar('Высота и длина должны быть больше 0'), PChar('Ошибка'), MB_OK);
   Exit;
  end;

  if not(MainLevel.Map.PanelSystem.PanelsArray[SelectedID].PanelType in [PANEL_WATER, PANEL_ACID1, PANEL_ACID2]) then
  begin
   if not(StrToIntDef(vleObjectProperty.Values['Alpha'], -1) in [0..255]) then
   begin
    MessageBox(MainForm.Handle, PChar('Прозрачность должна быть в интервале [0..255]'), PChar('Ошибка'), MB_OK);
    Exit;
   end;
  end;

  with MainLevel.Map.PanelSystem.PanelsArray[SelectedID] do
  begin
   X := StrToInt(vleObjectProperty.Values['X']);
   SelectRect.Left := X;

   Y := StrToInt(vleObjectProperty.Values['Y']);
   SelectRect.Top := Y;

   Width := Calculate(vleObjectProperty.Values['Width']);
   SelectRect.Right := Width+SelectRect.Left;

   Height := Calculate(vleObjectProperty.Values['Height']);
   SelectRect.Bottom := Height+SelectRect.Top;

   if not(PanelType in [PANEL_WATER, PANEL_ACID1, PANEL_ACID2]) then
   begin
    if vleObjectProperty.Values['Panel Type'] = 'WALL' then PanelType := PANEL_WALL
    else if vleObjectProperty.Values['Panel Type'] = 'BACKGROUND' then PanelType := PANEL_BACKGROUND
    else if vleObjectProperty.Values['Panel Type'] = 'FOREGROUND' then PanelType := PANEL_FOREGROUND
    else if vleObjectProperty.Values['Panel Type'] = 'STEP' then PanelType := PANEL_STEP
    else if vleObjectProperty.Values['Panel Type'] = 'WATER' then
    begin
     PanelType := PANEL_WATER;
     Alpha := 0;
     TextureName := '';
    end
    else if vleObjectProperty.Values['Panel Type'] = 'ACID1' then
    begin
     PanelType := PANEL_ACID1;
     Alpha := 0;
     TextureName := '';
    end
    else if vleObjectProperty.Values['Panel Type'] = 'ACID2' then
    begin
     PanelType := PANEL_ACID2;
     Alpha := 0;
     TextureName := '';
    end;

    Alpha := StrToInt(vleObjectProperty.Values['Alpha']);
   end;
  end;
 end;

 if SelectedObject = GO_ITEM then
 begin
  with MainLevel.ItemSystem.ItemsArray[SelectedID] do
  begin
   GameX := StrToIntDef(vleObjectProperty.Values['X'], GameX);
   SelectRect.Left := GameX;

   GameY := StrToIntDef(vleObjectProperty.Values['Y'], GameY);
   SelectRect.Top := GameY;

   SelectRect.Right := Width+SelectRect.Left;
   SelectRect.Bottom := Height+SelectRect.Top;
  end;
 end;

 if SelectedObject = GO_MONSTER then
 begin
  with MainLevel.MonsterSystem.MonstersArray[SelectedID] do
  begin
   GameX := StrToIntDef(vleObjectProperty.Values['X'], GameX);
   SelectRect.Left := GameX+Size.Left;

   GameY := StrToIntDef(vleObjectProperty.Values['Y'], GameY);
   SelectRect.Top := GameY+Size.Top;

   if vleObjectProperty.Values['Direction'] = 'LEFT' then Direction := D_LEFT
    else Direction := D_RIGHT;

   SelectRect.Right := GameX+Size.Right;
   SelectRect.Bottom := GameY+Size.Bottom;
  end;
 end;

 if SelectedObject = GO_AREA then
 begin
  with MainLevel.AreaSystem.AreasArray[SelectedID] do
  begin
   GameX := StrToIntDef(vleObjectProperty.Values['X'], GameX);
   GameY := StrToIntDef(vleObjectProperty.Values['Y'], GameY);
   if vleObjectProperty.Values['Direction'] = 'LEFT' then Direction := D_LEFT
    else Direction := D_RIGHT;

   SelectRect.Left := GameX+Size.Left;
   SelectRect.Top := GameY+Size.Top;
   SelectRect.Right := GameX+Size.Right;
   SelectRect.Bottom := GameY+Size.Bottom;
  end;
 end;

 if SelectedObject = GO_TRIGGER then
 begin
  with MainLevel.TriggersSystem.TriggersArray[SelectedID] do
  begin
   if (Calculate(vleObjectProperty.Values['Width']) <= 0) or
      (Calculate(vleObjectProperty.Values['Height']) <= 0) then
   begin
    MessageBox(MainForm.Handle, PChar('Высота и длина должна быть больше 0'), PChar('Ошибка'), MB_OK);
    Exit;
   end;

   X := StrToInt(vleObjectProperty.Values['X']);
   Y := StrToInt(vleObjectProperty.Values['Y']);

   Width := Calculate(vleObjectProperty.Values['Width']);
   Height := Calculate(vleObjectProperty.Values['Height']);

   TriggerActivateType := [];
   if Pos('PC', vleObjectProperty.Values['Activate type']) > 0 then Include(TriggerActivateType, TAT_PLAYERCOLLIDE);
   if Pos('MC', vleObjectProperty.Values['Activate type']) > 0 then Include(TriggerActivateType, TAT_MONSTERCOLLIDE);
   if Pos('PP', vleObjectProperty.Values['Activate type']) > 0 then Include(TriggerActivateType, TAT_PLAYERPRESS);

   TriggerActivateKey := [];
   if Pos('R', vleObjectProperty.Values['Keys']) > 0 then Include(TriggerActivateKey, KEY_RED);
   if Pos('G', vleObjectProperty.Values['Keys']) > 0 then Include(TriggerActivateKey, KEY_GREEN);
   if Pos('B', vleObjectProperty.Values['Keys']) > 0 then Include(TriggerActivateKey, KEY_BLUE);

   case TriggerType of
    TRIGGERST_EXIT:
     begin
      temp := vleObjectProperty.Values['Next map'];
      CopyMemory(@MapName[0], @temp[1], Length(temp));
     end;
    TRIGGERST_TELEPORT:
     begin
      temp := vleObjectProperty.Values['Target point'];
      for a := 1 to Length(temp) do
       if temp[a] = ':' then
       begin
        TeleportPoint.X := StrToInt(Copy(temp, 2, a-2));
        TeleportPoint.Y := StrToInt(Copy(temp, a+1, Length(temp)-a-1));
       end;
     end;
    TRIGGERST_OPENDOOR,
    TRIGGERST_DOOR5:
     begin
      PanelID := StrToInt(vleObjectProperty.Values['Target door']);
      if vleObjectProperty.Values['Door direction'] = 'Up' then Direction := 0;
      if vleObjectProperty.Values['Door direction'] = 'Down' then Direction := 1;
      if vleObjectProperty.Values['Door direction'] = 'Left' then Direction := 2;
      if vleObjectProperty.Values['Door direction'] = 'Right' then Direction := 3;
     end;
   end;

   SelectRect.Left := X;
   SelectRect.Top := Y;
   SelectRect.Right := X+Width;
   SelectRect.Bottom := Y+Height;
  end;
 end;

 FillProperty(SelectedID, SelectedObject); 
end;

procedure TMainForm.vleObjectPropertyEditButtonClick(Sender: TObject);
var
  Key: String;
  temp: String;
begin
 Key := vleObjectProperty.Keys[vleObjectProperty.Row];
 if Key = 'Next map' then
 begin
  if SelectMapForm.ShowModal = mrOK then
   vleObjectProperty.Values[Key] := SelectMapForm.lbMapList.Items[SelectMapForm.lbMapList.ItemIndex];
 end
  else if Key = 'Target point' then
 begin
  SelectingDoor := False;
  SelectingPoint := True;
 end
  else if Key = 'Target door' then
 begin
  SelectingPoint := False;
  SelectingDoor := True;
 end
  else if Key = 'Activate type' then
 begin
  ActivationTypeForm.cbPlayerCollide.Checked := Pos('PC', vleObjectProperty.Values[Key]) > 0;
  ActivationTypeForm.cbMonsterCollide.Checked := Pos('MC', vleObjectProperty.Values[Key]) > 0;
  ActivationTypeForm.cbPlayerPress.Checked := Pos('PP', vleObjectProperty.Values[Key]) > 0;
  if ActivationTypeForm.ShowModal = mrOK then
  begin
   vleObjectProperty.Values[Key] := '';
   if ActivationTypeForm.cbPlayerCollide.Checked then
   begin
    vleObjectProperty.Values[Key] := vleObjectProperty.Values[Key]+'+PC';
   end;
   if ActivationTypeForm.cbMonsterCollide.Checked then
    vleObjectProperty.Values[Key] := vleObjectProperty.Values[Key]+'+MC';
   if ActivationTypeForm.cbPlayerPress.Checked then
    vleObjectProperty.Values[Key] := vleObjectProperty.Values[Key]+'+PP';
   temp := vleObjectProperty.Values[Key];
   if (temp <> '') and (temp[1] = '+') then Delete(temp, 1, 1);
   vleObjectProperty.Values[Key] := temp;
  end;
 end
  else if Key = 'Keys' then
 begin
  KeysForm.cbRedKey.Checked := Pos('R', vleObjectProperty.Values[Key]) > 0;
  KeysForm.cbGreenKey.Checked := Pos('G', vleObjectProperty.Values[Key]) > 0;
  KeysForm.cbBlueKey.Checked := Pos('B', vleObjectProperty.Values[Key]) > 0;
  if KeysForm.ShowModal = mrOK then
  begin
   vleObjectProperty.Values[Key] := '';
   if KeysForm.cbRedKey.Checked then
    vleObjectProperty.Values[Key] := vleObjectProperty.Values[Key]+'+R';
   if KeysForm.cbGreenKey.Checked then
    vleObjectProperty.Values[Key] := vleObjectProperty.Values[Key]+'+G';
   if KeysForm.cbBlueKey.Checked then
    vleObjectProperty.Values[Key] := vleObjectProperty.Values[Key]+'+B';
   temp := vleObjectProperty.Values[Key];
   if (temp <> '') and (temp[1] = '+') then Delete(temp, 1, 1);
   vleObjectProperty.Values[Key] := temp;
  end;
 end;
end;

procedure TMainForm.FillProperty(ID: DWORD; ObjectType: TGameObjectType);
var
  a: Integer;
  temp: String;
begin
 vleObjectProperty.Strings.Clear;

 if ObjectType = GO_PANEL then
 with vleObjectProperty, MainLevel.Map.PanelSystem.PanelsArray[ID] do
 begin
  a := InsertRow('X', IntToStr(X), True);
  ItemProps[a-1].EditStyle := esSimple;

  a := InsertRow('Y', IntToStr(Y), True);
  ItemProps[a-1].EditStyle := esSimple;

  a := InsertRow('Width', IntToStr(Width), True);
  ItemProps[a-1].EditStyle := esSimple;

  a := InsertRow('Height', IntToStr(Height), True);
  ItemProps[a-1].EditStyle := esSimple;

  if not(PanelType in [PANEL_WATER, PANEL_ACID1, PANEL_ACID2]) then
  begin
   case PanelType of
    PANEL_WALL: a := InsertRow('Panel Type', 'WALL', True);
    PANEL_BACKGROUND: a := InsertRow('Panel Type', 'BACKGROUND', True);
    PANEL_FOREGROUND: a := InsertRow('Panel Type', 'FOREGROUND', True);
    PANEL_STEP: a := InsertRow('Panel Type', 'STEP', True);
   end;
   ItemProps[a-1].EditStyle := esPickList;
   ItemProps[a-1].ReadOnly := True;

   a := InsertRow('Alpha', IntToStr(Alpha), True);
   ItemProps[a-1].EditStyle := esSimple;
  end;
 end;

 if ObjectType = GO_ITEM then
 with vleObjectProperty, MainLevel.ItemSystem.ItemsArray[ID] do
 begin
  a := InsertRow('X', IntToStr(GameX), True);
  ItemProps[a-1].EditStyle := esSimple;

  a := InsertRow('Y', IntToStr(GameY), True);
  ItemProps[a-1].EditStyle := esSimple;
 end;

 if ObjectType = GO_MONSTER then
 with vleObjectProperty, MainLevel.MonsterSystem.MonstersArray[ID] do
 begin
  a := InsertRow('X', IntToStr(GameX), True);
  ItemProps[a-1].EditStyle := esSimple;

  a := InsertRow('Y', IntToStr(GameY), True);
  ItemProps[a-1].EditStyle := esSimple;

  case Direction of
   D_LEFT: a := InsertRow('Direction', 'LEFT', True);
   D_RIGHT: a := InsertRow('Direction', 'RIGHT', True);
  end;
  ItemProps[a-1].EditStyle := esPickList;
  ItemProps[a-1].ReadOnly := True;
 end;

 if ObjectType = GO_AREA then
 with vleObjectProperty, MainLevel.AreaSystem.AreasArray[ID] do
 begin
  a := InsertRow('X', IntToStr(GameX), True);
  ItemProps[a-1].EditStyle := esSimple;

  a := InsertRow('Y', IntToStr(GameY), True);
  ItemProps[a-1].EditStyle := esSimple;

  case Direction of
   D_LEFT: a := InsertRow('Direction', 'LEFT', True);
   D_RIGHT: a := InsertRow('Direction', 'RIGHT', True);
  end;
  ItemProps[a-1].EditStyle := esPickList;
  ItemProps[a-1].ReadOnly := True;
 end;

 if ObjectType = GO_TRIGGER then
 with vleObjectProperty, MainLevel.TriggersSystem.TriggersArray[ID] do
 begin
  case TriggerType of
   TRIGGERST_EXIT:
   begin
    a := InsertRow('Trigger type', 'EXIT', True);
    ItemProps[a-1].EditStyle := esSimple;
    ItemProps[a-1].ReadOnly := True;

    a := InsertRow('Next map', '', True);
    ItemProps[a-1].EditStyle := esEllipsis;
    ItemProps[a-1].ReadOnly := True;
   end;
   TRIGGERST_TELEPORT:
   begin
    a := InsertRow('Trigger type', 'TELEPORT', True);
    ItemProps[a-1].EditStyle := esSimple;
    ItemProps[a-1].ReadOnly := True;

    a := InsertRow('Target point', Format('(%d:%d)', [TeleportPoint.X, TeleportPoint.Y]), True);
    ItemProps[a-1].EditStyle := esEllipsis;
    ItemProps[a-1].ReadOnly := True;
   end;
   TRIGGERST_DOOR5,
   TRIGGERST_OPENDOOR:
   begin
    if TriggerType = TRIGGERST_DOOR5 then
     a := InsertRow('Trigger type', 'DOOR5', True)
    else a := InsertRow('Trigger type', 'OPENDOOR', True);
    ItemProps[a-1].ReadOnly := True;

     a := InsertRow('Target door', IntToStr(PanelID), True);
     ItemProps[a-1].EditStyle := esEllipsis;
     ItemProps[a-1].ReadOnly := True;

     if Direction = 0 then temp := 'Up';
     if Direction = 1 then temp := 'Down';
     if Direction = 2 then temp := 'Left';
     if Direction = 3 then temp := 'Right';
     a := InsertRow('Door direction', temp, True);
     ItemProps[a-1].EditStyle := esPickList;
     ItemProps[a-1].ReadOnly := True;
    end;
   end;

   InsertRow('X', IntToStr(X), True);
   InsertRow('Y', IntToStr(Y), True);
   InsertRow('Width', IntToStr(Width), True);
   InsertRow('Height', IntToStr(Height), True);

   temp := '';
   if TAT_PLAYERCOLLIDE in TriggerActivateType then temp := temp+'+PC';
   if TAT_MONSTERCOLLIDE in TriggerActivateType then temp := temp+'+MC';
   if TAT_PLAYERCOLLIDE in TriggerActivateType then temp := temp+'+PP';

   if (temp <> '') and (temp[1] = '+') then Delete(temp, 1, 1);
   a := InsertRow('Activate type', temp, True);
   ItemProps[a-1].EditStyle := esEllipsis;
   ItemProps[a-1].ReadOnly := True;

   temp := '';
   if KEY_RED in TriggerActivateKey then temp := temp+'+R';
   if KEY_GREEN in TriggerActivateKey then temp := temp+'+G';
   if KEY_BLUE in TriggerActivateKey then temp := temp+'+B';

   if (temp <> '') and (temp[1] = '+') then Delete(temp, 1, 1);
   a := InsertRow('Keys', temp, True);
   ItemProps[a-1].EditStyle := esEllipsis;
   ItemProps[a-1].ReadOnly := True;
 end;
end;

end.
