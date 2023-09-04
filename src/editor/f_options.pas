unit f_options;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ActnList, Spin, EditBtn, Registry, Math, Types;

type
  TOptionsForm = class (TForm)
    bOK: TButton;
    bCancel: TButton;
    cbAllowExit: TCheckBox;
    cbCheckerboard: TCheckBox;
    cbLanguage: TComboBox;
    cbMapOnce: TCheckBox;
    cbMonstersDM: TCheckBox;
    cbShowDots: TCheckBox;
    cbShowSize: TCheckBox;
    cbShowTexture: TCheckBox;
    cbTeamDamage: TCheckBox;
    cbTwoPlayers: TCheckBox;
    cbWeaponStay: TCheckBox;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    ColorButton3: TColorButton;
    edD2DArgs: TEdit;
    edScore: TEdit;
    edTime: TEdit;
    ExeEdit: TFileNameEdit;
    LabelArgs: TLabel;
    LabelBack: TLabel;
    LabelGrid: TLabel;
    LabelGridCol: TLabel;
    LabelGridSize: TLabel;
    LabelLanguage: TLabel;
    LabelMinimap: TLabel;
    LabelPath: TLabel;
    LabelPreview: TLabel;
    LabelRecent: TLabel;
    LabelScore: TLabel;
    LabelSecs: TLabel;
    LabelTime: TLabel;
    PageControl: TPageControl;
    rbCOOP: TRadioButton;
    rbCTF: TRadioButton;
    rbDM: TRadioButton;
    rbTDM: TRadioButton;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    TabFiles: TTabSheet;
    TabGeneral: TTabSheet;
    TabTesting: TTabSheet;

    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  OptionsForm: TOptionsForm;

procedure RegisterFileType(ext: String; FileName: String);

implementation

uses
  LazFileUtils, f_main, StdConvs, CONFIG, g_language, g_options;

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

procedure TOptionsForm.FormShow(Sender: TObject);
  var list: TStringList;
begin
  // General Tab:
  ColorButton1.ButtonColor := DotColor;
  ColorButton2.ButtonColor := BackColor;
  ColorButton3.ButtonColor := PreviewColor;
  SpinEdit1.Value := DotStepOne;
  SpinEdit2.Value := DotStepTwo;

  cbShowDots.Checked := DotEnable;
  cbShowTexture.Checked := DrawTexturePanel;
  cbShowSize.Checked := DrawPanelSize;
  cbCheckerboard.Checked := UseCheckerboard;

  SpinEdit4.Value := DotSize;
  SpinEdit5.Value := Scale;

  try
    cbLanguage.Items.BeginUpdate;
    cbLanguage.Items.Clear;
    cbLanguage.Items.Add(MsgLabEsLanguageAuto);
    list := g_Language_GetList();
    cbLanguage.Items.AddStrings(list);
    list.Free();
    cbLanguage.ItemIndex := IfThen(gLanguage = '', 0, cbLanguage.Items.IndexOf(gLanguage));
  finally
    cbLanguage.Items.EndUpdate;
  end;

  // Files Tab:
  SpinEdit3.Value    := RecentCount;

  // Testing Tab:
  ExeEdit.Text   := TestD2dExe;
  edD2DArgs.Text := TestD2DArgs;

  rbDM.Checked   := TestGameMode = 'DM';
  rbTDM.Checked  := TestGameMode = 'TDM';
  rbCTF.Checked  := TestGameMode = 'CTF';
  rbCOOP.Checked := TestGameMode = 'COOP';

  edTime.Text := TestLimTime;
  edScore.Text := TestLimScore;
  cbTwoPlayers.Checked := TestOptionsTwoPlayers;
  cbTeamDamage.Checked := TestOptionsTeamDamage;
  cbAllowExit.Checked := TestOptionsAllowExit;
  cbWeaponStay.Checked := TestOptionsWeaponStay;
  cbMonstersDM.Checked := TestOptionsMonstersDM;
  cbMapOnce.Checked := TestMapOnce;
end;

procedure TOptionsForm.bOKClick(Sender: TObject);
var
  config: TConfig;
  re, n: Integer;
  str: String;

begin
  // General tab:
  if cbLanguage.ItemIndex >= 0 then
  begin
    if cbLanguage.ItemIndex = 0 then str := '' else str := cbLanguage.Items[cbLanguage.ItemIndex];
    if (str = '') or (gLanguage <> str) then
    begin
      gLanguage := str;
      g_Language_Set(gLanguage);
    end;
  end;

  DotColor     := ColorButton1.ButtonColor;
  BackColor    := ColorButton2.ButtonColor;
  PreviewColor := ColorButton3.ButtonColor;

  DotEnable  := cbShowDots.Checked;
  DotStep    := IfThen(DotStep = DotStepOne, SpinEdit1.Value, SpinEdit2.Value);
  DotStepOne := SpinEdit1.Value;
  DotStepTwo := SpinEdit2.Value;

  DrawTexturePanel := cbShowTexture.Checked;
  DrawPanelSize := cbShowSize.Checked;
  UseCheckerboard := cbCheckerboard.Checked;
  DotSize := SpinEdit4.Value;
  Scale   := SpinEdit5.Value;

  // Files tab:
  re := SpinEdit3.Value;

  // Testing tab:
  TestD2DExe  := ExeEdit.Text;
  TestD2DArgs := edD2DArgs.Text;

  TestGameMode := 'DM';
  if rbTDM.Checked then  TestGameMode := 'TDM';
  if rbCTF.Checked then  TestGameMode := 'CTF';
  if rbCOOP.Checked then TestGameMode := 'COOP';
  if rbDM.Checked then   TestGameMode := 'DM';

  TestLimTime := edTime.Text;
  if (not TryStrToInt(TestLimTime, n)) then
    TestLimTime := '0';

  TestLimScore := edScore.Text;
  if (not TryStrToInt(TestLimScore, n)) then
    TestLimScore := '0';

  TestOptionsTwoPlayers := cbTwoPlayers.Checked;
  TestOptionsTeamDamage := cbTeamDamage.Checked;
  TestOptionsAllowExit  := cbAllowExit.Checked;
  TestOptionsWeaponStay := cbWeaponStay.Checked;
  TestOptionsMonstersDM := cbMonstersDM.Checked;
  TestMapOnce := cbMapOnce.Checked;

  // save into config
  config := TConfig.CreateFile(CfgFileName);

  config.WriteInt('Editor', 'DotColor', DotColor);
  config.WriteBool('Editor', 'DotEnable', DotEnable);
  config.WriteInt('Editor', 'DotStepOne', DotStepOne);
  config.WriteInt('Editor', 'DotStepTwo', DotStepTwo);
  config.WriteInt('Editor', 'DotStep', DotStep);
  config.WriteInt('Editor', 'DotSize', SpinEdit4.Value);
  config.WriteBool('Editor', 'DrawTexturePanel', DrawTexturePanel);
  config.WriteBool('Editor', 'DrawPanelSize', DrawPanelSize);
  config.WriteInt('Editor', 'BackColor', BackColor);
  config.WriteInt('Editor', 'PreviewColor', PreviewColor);
  config.WriteBool('Editor', 'UseCheckerboard', UseCheckerboard);
  config.WriteInt('Editor', 'Scale', SpinEdit5.Value);
  config.WriteStr('Editor', 'Language', gLanguage);

  config.WriteInt('Editor', 'RecentCount', re);

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

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  {$IF DEFINED(DARWIN)}
    if LowerCase(ExtractFileExt(TestD2DExe)) = '.app' then
      ExeEdit.InitialDir := ExtractFileDir(TestD2DExe)
    else
      ExeEdit.InitialDir := TestD2DExe;
  {$ELSE}
    ExeEdit.InitialDir := TestD2DExe;
  {$ENDIF}
end;

end.
