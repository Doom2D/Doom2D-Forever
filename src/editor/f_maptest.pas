unit f_maptest;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls;

type

  { TMapTestForm }

  TMapTestForm = class (TForm)
    bOK: TButton;
    bCancel: TButton;
    GroupBox1: TGroupBox;
    LabelArgs: TLabel;
  // Режим игры:
    rbDM: TRadioButton;
    rbTDM: TRadioButton;
    rbCTF: TRadioButton;
    rbCOOP: TRadioButton;
  // Опции:
    cbTwoPlayers: TCheckBox;
    cbTeamDamage: TCheckBox;
    cbAllowExit: TCheckBox;
    cbWeaponStay: TCheckBox;
    cbMonstersDM: TCheckBox;
    cbMapOnce: TCheckBox;
  // Лимит времени:
    LabelTime: TLabel;
    edTime: TEdit;
    UpDown2: TUpDown;
    LabelSecs: TLabel;
  // Лимит очков:
    LabelScore: TLabel;
    edScore: TEdit;
    UpDown1: TUpDown;
  // Путь:
    LabelPath: TLabel;
    edD2dexe: TEdit;
    edD2DArgs: TEdit;
    bChooseD2d: TButton;
    FindD2dDialog: TOpenDialog;

    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bChooseD2dClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MapTestForm: TMapTestForm;

implementation

uses
  f_main, g_options, CONFIG;

{$R *.lfm}

procedure TMapTestForm.bOKClick(Sender: TObject);
var
  config: TConfig;
  s: String;
  n: Integer;
  
begin
  config := TConfig.CreateFile(CfgFileName);

  if rbTDM.Checked then
    s := 'TDM'
  else
    if rbCTF.Checked then
      s := 'CTF'
    else
      if rbCOOP.Checked then
        s := 'COOP'
      else
        s := 'DM';
  config.WriteStr('TestRun', 'GameMode', s);
  TestGameMode := s;

  s := edTime.Text;
  if (not TryStrToInt(s, n)) then
    s := '0';
  config.WriteStr('TestRun', 'LimTime', s);
  TestLimTime := s;

  s := edScore.Text;
  if (not TryStrToInt(s, n)) then
    s := '0';
  config.WriteStr('TestRun', 'LimScore', s);
  TestLimScore := s;

  config.WriteBool('TestRun', 'TwoPlayers', cbTwoPlayers.Checked);
  TestOptionsTwoPlayers := cbTwoPlayers.Checked;
  config.WriteBool('TestRun', 'TeamDamage', cbTeamDamage.Checked);
  TestOptionsTeamDamage := cbTeamDamage.Checked;
  config.WriteBool('TestRun', 'AllowExit', cbAllowExit.Checked);
  TestOptionsAllowExit := cbAllowExit.Checked;
  config.WriteBool('TestRun', 'WeaponStay', cbWeaponStay.Checked);
  TestOptionsWeaponStay := cbWeaponStay.Checked;
  config.WriteBool('TestRun', 'MonstersDM', cbMonstersDM.Checked);
  TestOptionsMonstersDM := cbMonstersDM.Checked;

  config.WriteBool('TestRun', 'MapOnce', cbMapOnce.Checked);
  TestMapOnce := cbMapOnce.Checked;
  
  config.WriteStr('TestRun', 'Exe', edD2dExe.Text);
  TestD2dExe := edD2dExe.Text;
  config.WriteStr('TestRun', 'Args', edD2DArgs.Text);
  TestD2DArgs := edD2DArgs.Text;

  config.SaveFile(CfgFileName);
  config.Free();
  Close();
end;

procedure TMapTestForm.bCancelClick(Sender: TObject);
begin
  Close();
end;

procedure TMapTestForm.FormActivate(Sender: TObject);
begin
  if TestGameMode = 'TDM' then
    rbTDM.Checked := True
  else
    if TestGameMode = 'CTF' then
      rbCTF.Checked := True
    else
      if TestGameMode = 'COOP' then
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

procedure TMapTestForm.FormCreate(Sender: TObject);
var
  config: TConfig;
  
begin
  config := TConfig.CreateFile(CfgFileName);

  TestGameMode := config.ReadStr('TestRun', 'GameMode', 'DM');
  TestLimTime := config.ReadStr('TestRun', 'LimTime', '0');
  TestLimScore := config.ReadStr('TestRun', 'LimScore', '0');
  TestOptionsTwoPlayers := config.ReadBool('TestRun', 'TwoPlayers', False);
  TestOptionsTeamDamage := config.ReadBool('TestRun', 'TeamDamage', False);
  TestOptionsAllowExit := config.ReadBool('TestRun', 'AllowExit', True);
  TestOptionsWeaponStay := config.ReadBool('TestRun', 'WeaponStay', False);
  TestOptionsMonstersDM := config.ReadBool('TestRun', 'MonstersDM', False);
  TestMapOnce := config.ReadBool('TestRun', 'MapOnce', False);
  TestD2dExe := config.ReadStr('TestRun', 'Exe', GameExeFile);
  TestD2DArgs := config.ReadStr('TestRun', 'Args', '');

  config.Free();

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

procedure TMapTestForm.bChooseD2dClick(Sender: TObject);
begin
  if FindD2dDialog.Execute then
  begin
    edD2dExe.Text := FindD2dDialog.FileName;
  end;
end;

end.
