unit f_options;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ActnList, Registry, Math, Types;

type

  { TOptionsForm }

  TOptionsForm = class (TForm)
    bOK: TButton;
    bCancel: TButton;
    cbCheckerboard: TCheckBox;
    cbCompress: TCheckBox;
    cbBackup: TCheckBox;
    cbLanguage: TComboBox;
    PageControl: TPageControl;
    TabGeneral: TTabSheet;
    TabFiles: TTabSheet;
    TabTesting: TTabSheet;
    ColorDialog: TColorDialog;
  // Общие настройки:
    cbShowDots: TCheckBox;
    cbShowTexture: TCheckBox;
    cbShowSize: TCheckBox;
  // Шаги сетки:
    LabelGrid: TLabel;
    eDotStepOne: TEdit;
    UpDown1: TUpDown;
    eDotStepTwo: TEdit;
    UpDown2: TUpDown;
  // Цвет сетки:
    LabelGridCol: TLabel;
    sDotColor: TShape;
    bGrid: TButton;
  // Цвет фона:
    LabelBack: TLabel;
    sBackColor: TShape;
    bBack: TButton;
  // Цвет превью:
    LabelPreview: TLabel;
    sPreviewColor: TShape;
    bPreview: TButton;
  // Масштаб миникарты:
    LabelMinimap: TLabel;
    cbScale: TComboBox;
  // Количество недавно открытых:
    LabelRecent: TLabel;
    eRecent: TEdit;
    UpDown3: TUpDown;
    LabelLanguage: TLabel;
    LabelGridSize: TLabel;
    cbDotSize: TComboBox;
  // Map testing:
    LabelPath: TLabel;
    edD2dexe: TEdit;
    bChooseD2d: TButton;
    FindD2dDialog: TOpenDialog;
    LabelArgs: TLabel;
    edD2DArgs: TEdit;
    rbCOOP: TRadioButton;
    rbCTF: TRadioButton;
    rbDM: TRadioButton;
    rbTDM: TRadioButton;
    cbAllowExit: TCheckBox;
    cbMapOnce: TCheckBox;
    cbMonstersDM: TCheckBox;
    cbTeamDamage: TCheckBox;
    cbTwoPlayers: TCheckBox;
    cbWeaponStay: TCheckBox;
    LabelScore: TLabel;
    LabelSecs: TLabel;
    edScore: TEdit;
    LabelTime: TLabel;
    edTime: TEdit;


    procedure bGridClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bBackClick(Sender: TObject);
    procedure bPreviewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bChooseD2dClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;

procedure RegisterFileType(ext: String; FileName: String);

implementation

uses
  LazFileUtils, StrUtils, f_main, StdConvs, CONFIG, g_language, g_resources, g_options;

{$R *.lfm}

procedure RegisterFileType(ext: String; FileName: String);
var
  reg: TRegistry;

begin
  reg := TRegistry.Create();

  with reg do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey('.'+ext,True);
    WriteString('',ext+'file');
    CloseKey();
    CreateKey(ext+'file');
    OpenKey(ext+'file\DefaultIcon',True);
    WriteString('',FileName+',0');
    CloseKey();
    OpenKey(ext+'file\shell\open\command',True);
    WriteString('',FileName+' "%1"');
    CloseKey();
    Free();
  end;
end;

procedure TOptionsForm.bGridClick(Sender: TObject);
begin
  if ColorDialog.Execute then
    sDotColor.Brush.Color := ColorDialog.Color;
end;

procedure TOptionsForm.bChooseD2dClick(Sender: TObject);
begin
  if FindD2dDialog.Execute then
    edD2dExe.Text := FindD2dDialog.FileName;
end;

procedure TOptionsForm.FormActivate(Sender: TObject);
  var info: TSearchRec; s: String; i: Integer;
begin
  sDotColor.Brush.Color := DotColor;
  cbShowDots.Checked := DotEnable;
  cbShowTexture.Checked := DrawTexturePanel;
  cbShowSize.Checked := DrawPanelSize;
  eDotStepOne.Text := IntToStr(DotStepOne);
  eDotStepTwo.Text := IntToStr(DotStepTwo);
  sBackColor.Brush.Color := BackColor;
  sPreviewColor.Brush.Color := PreviewColor;
  cbCheckerboard.Checked := UseCheckerboard;
  cbCompress.Checked := Compress;
  cbBackup.Checked := Backup;
  if Scale = 2 then
    cbScale.ItemIndex := 1
  else
    cbScale.ItemIndex := 0;
  if DotSize = 2 then
    cbDotSize.ItemIndex := 1
  else
    cbDotSize.ItemIndex := 0;
  eRecent.Text := IntToStr(RecentCount);

  try
    cbLanguage.Items.BeginUpdate;
    cbLanguage.Items.Clear;
    cbLanguage.Items.Add('Auto');
    if FindFirst(LangDir + DirectorySeparator + '*.mo', faAnyFile, info) = 0 then
    begin
      repeat
        s := ExtractFileNameWithoutExt(info.Name);
        // TODO: check encoding part in name (editor.ru_RU.UTF-8.mo)
        i := Max(RPos('.', s), 1);
        s := Copy(s, i + 1, Length(s) - i);
        cbLanguage.Items.Add(s);
      until FindNext(info) <> 0;
      FindClose(info);
    end;
    cbLanguage.ItemIndex := IfThen(gLanguage = '', 0, cbLanguage.Items.IndexOf(gLanguage));
  finally
    cbLanguage.Items.EndUpdate;
  end;

  if TestGameMode = 'TDM' then
    rbTDM.Checked := True
  else if TestGameMode = 'CTF' then
    rbCTF.Checked := True
  else if TestGameMode = 'COOP' then
    rbCOOP.Checked := True
  else
    rbDM.Checked := True;

  edTime.Text := TestLimTime;
  edScore.Text := TestLimScore;
  cbTwoPlayers.Checked := TestOptionsTwoPlayers;
  cbTeamDamage.Checked := TestOptionsTeamDamage;
  cbAllowExit.Checked := TestOptionsAllowExit;
  cbWeaponStay.Checked := TestOptionsWeaponStay;
  cbMonstersDM.Checked := TestOptionsMonstersDM;
  cbMapOnce.Checked := TestMapOnce;
  edD2dExe.Text := TestD2dExe;
  edD2DArgs.Text := TestD2DArgs;
end;

procedure TOptionsForm.bOKClick(Sender: TObject);
var
  config: TConfig;
  re, n: Integer;
  d1: Boolean;
  str: String;

begin
  // General tab

  if cbLanguage.ItemIndex <= 0 then str := '' else str := cbLanguage.Items[cbLanguage.ItemIndex];
  if (str = '') or (gLanguage <> str) then
  begin
    gLanguage := str;
    g_Language_Set(gLanguage);
  end;
  
  DotColor := sDotColor.Brush.Color;
  DotEnable := cbShowDots.Checked;

  if DotStep = DotStepOne then
    d1 := True
  else
    d1 := False;
  DotStepOne := EnsureRange(StrToIntDef(eDotStepOne.Text, 16), 4, 2048);
  DotStepTwo := EnsureRange(StrToIntDef(eDotStepTwo.Text, 8), 4, 2048);
  if d1 then
    DotStep := DotStepOne
  else
    DotStep := DotStepTwo;

  DrawTexturePanel := cbShowTexture.Checked;
  DrawPanelSize := cbShowSize.Checked;
  BackColor := sBackColor.Brush.Color;
  PreviewColor := sPreviewColor.Brush.Color;
  UseCheckerboard := cbCheckerboard.Checked;

  if cbScale.ItemIndex = 1 then
    Scale := 2
  else
    Scale := 1;

  if cbDotSize.ItemIndex = 1 then
    DotSize := 2
  else
    DotSize := 1;

  // Files tab

  re := Min(Max(StrToIntDef(eRecent.Text, 5), 2), 10);
  Compress := cbCompress.Checked;
  Backup := cbBackup.Checked;

  // Testing tab

  if rbTDM.Checked then
    TestGameMode := 'TDM'
  else if rbCTF.Checked then
    TestGameMode := 'CTF'
  else if rbCOOP.Checked then
    TestGameMode := 'COOP'
  else
    TestGameMode := 'DM';

  TestLimTime := edTime.Text;
  if (not TryStrToInt(TestLimTime, n)) then
    TestLimTime := '0';

  TestLimScore := edScore.Text;
  if (not TryStrToInt(TestLimScore, n)) then
    TestLimScore := '0';

  TestOptionsTwoPlayers := cbTwoPlayers.Checked;
  TestOptionsTeamDamage := cbTeamDamage.Checked;
  TestOptionsAllowExit := cbAllowExit.Checked;
  TestOptionsWeaponStay := cbWeaponStay.Checked;
  TestOptionsMonstersDM := cbMonstersDM.Checked;
  TestMapOnce := cbMapOnce.Checked;

  TestD2dExe := edD2dExe.Text;
  TestD2DArgs := edD2DArgs.Text;

  // save into config

  config := TConfig.CreateFile(CfgFileName);

  config.WriteInt('Editor', 'DotColor', DotColor);
  config.WriteBool('Editor', 'DotEnable', DotEnable);
  config.WriteInt('Editor', 'DotStepOne', DotStepOne);
  config.WriteInt('Editor', 'DotStepTwo', DotStepTwo);
  config.WriteInt('Editor', 'DotStep', DotStep);
  config.WriteInt('Editor', 'DotSize', cbDotSize.ItemIndex);
  config.WriteBool('Editor', 'DrawTexturePanel', DrawTexturePanel);
  config.WriteBool('Editor', 'DrawPanelSize', DrawPanelSize);
  config.WriteInt('Editor', 'BackColor', BackColor);
  config.WriteInt('Editor', 'PreviewColor', PreviewColor);
  config.WriteBool('Editor', 'UseCheckerboard', UseCheckerboard);
  config.WriteInt('Editor', 'Scale', cbScale.ItemIndex);
  config.WriteStr('Editor', 'Language', gLanguage);

  config.WriteInt('Editor', 'RecentCount', re);
  config.WriteBool('Editor', 'Compress', Compress);
  config.WriteBool('Editor', 'Backup', Backup);

  config.WriteStr('TestRun', 'GameMode', TestGameMode);
  config.WriteStr('TestRun', 'LimTime', TestLimTime);
  config.WriteStr('TestRun', 'LimScore', TestLimScore);
  config.WriteBool('TestRun', 'TwoPlayers', TestOptionsTwoPlayers);
  config.WriteBool('TestRun', 'TeamDamage', TestOptionsTeamDamage);
  config.WriteBool('TestRun', 'AllowExit', TestOptionsAllowExit);
  config.WriteBool('TestRun', 'WeaponStay', TestOptionsWeaponStay);
  config.WriteBool('TestRun', 'MonstersDM', TestOptionsMonstersDM);
  config.WriteBool('TestRun', 'MapOnce', TestMapOnce);
  {$IF DEFINED(DARWIN)}
    config.WriteStr('TestRun', 'ExeDrawin', TestD2dExe);
  {$ELSEIF DEFINED(WINDOWS)}
    config.WriteStr('TestRun', 'ExeWindows', TestD2dExe);
  {$ELSE}
    config.WriteStr('TestRun', 'ExeUnix', TestD2dExe);
  {$ENDIF}
  config.WriteStr('TestRun', 'Args', TestD2DArgs);

  if RecentCount <> re then
  begin
    RecentCount := re;
    MainForm.RefreshRecentMenu();
  end;

  config.SaveFile(CfgFileName);
  config.Free();
  Close();
end;

procedure TOptionsForm.bCancelClick(Sender: TObject);
begin
  Close();
end;

procedure TOptionsForm.bBackClick(Sender: TObject);
begin
  if ColorDialog.Execute then
    sBackColor.Brush.Color := ColorDialog.Color;
end;

procedure TOptionsForm.bPreviewClick(Sender: TObject);
begin
  if ColorDialog.Execute then
    sPreviewColor.Brush.Color := ColorDialog.Color;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  {$IF DEFINED(DARWIN)}
    if LowerCase(ExtractFileExt(TestD2dExe)) = '.app' then
      FindD2dDialog.InitialDir := ExtractFileDir(TestD2dExe)
    else
      FindD2dDialog.InitialDir := TestD2dExe;
    FindD2dDialog.DefaultExt := '.app';
    FindD2dDialog.Filter := 'Doom 2D Forever.app|*.app|Doom 2D Forever (Unix Executable)|Doom2DF;*';
  {$ELSEIF DEFINED(WINDOWS)}
    FindD2dDialog.InitialDir := TestD2dExe;
    FindD2dDialog.DefaultExt := '.exe';
    FindD2dDialog.Filter := 'Doom2DF.exe|Doom2DF.exe;*.exe';
  {$ELSE}
    FindD2dDialog.InitialDir := TestD2dExe;
    FindD2dDialog.DefaultExt := '';
    FindD2dDialog.Filter := 'Doom2DF|Doom2DF;*';
  {$ENDIF}
end;

end.
