unit f_maptest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TMapTestForm = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    GroupBox1: TGroupBox;
    rbDM: TRadioButton;
    rbTDM: TRadioButton;
    rbCTF: TRadioButton;
    rbCOOP: TRadioButton;
    cbTwoPlayers: TCheckBox;
    cbTeamDamage: TCheckBox;
    cbAllowExit: TCheckBox;
    cbWeaponStay: TCheckBox;
    cbMonstersDM: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edTime: TEdit;
    edGoal: TEdit;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    edD2dexe: TEdit;
    Label4: TLabel;
    bChooseD2d: TButton;
    FindD2dDialog: TOpenDialog;
    cbMapOnce: TCheckBox;
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

uses f_main, CONFIG;

{$R *.dfm}

procedure TMapTestForm.bOKClick(Sender: TObject);
var
  config: TConfig;
  s: String;
  n: Integer;
  
begin
  config := TConfig.CreateFile(EditorDir+'\editor.cfg');

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

  s := edGoal.Text;
  if (not TryStrToInt(s, n)) then
    s := '0';
  config.WriteStr('TestRun', 'LimGoal', s);
  TestLimGoal := s;

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

  config.SaveFile(EditorDir+'\editor.cfg');
  config.Destroy;
  Close;
end;

procedure TMapTestForm.bCancelClick(Sender: TObject);
begin
  Close;
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
  edGoal.Text := TestLimGoal;
  cbTwoPlayers.Checked := TestOptionsTwoPlayers;
  cbTeamDamage.Checked := TestOptionsTeamDamage;
  cbAllowExit.Checked := TestOptionsAllowExit;
  cbWeaponStay.Checked := TestOptionsWeaponStay;
  cbMonstersDM.Checked := TestOptionsMonstersDM;
  cbMapOnce.Checked := TestMapOnce;
  edD2dExe.Text := TestD2dExe;
end;

procedure TMapTestForm.FormCreate(Sender: TObject);
var
  config: TConfig;
  
begin
  config := TConfig.CreateFile(EditorDir+'\editor.cfg');

  TestGameMode := config.ReadStr('TestRun', 'GameMode', 'DM');
  TestLimTime := config.ReadStr('TestRun', 'LimTime', '0');
  TestLimGoal := config.ReadStr('TestRun', 'LimGoal', '0');
  TestOptionsTwoPlayers := config.ReadBool('TestRun', 'TwoPlayers', False);
  TestOptionsTeamDamage := config.ReadBool('TestRun', 'TeamDamage', False);
  TestOptionsAllowExit := config.ReadBool('TestRun', 'AllowExit', True);
  TestOptionsWeaponStay := config.ReadBool('TestRun', 'WeaponStay', False);
  TestOptionsMonstersDM := config.ReadBool('TestRun', 'MonstersDM', False);
  TestMapOnce := config.ReadBool('TestRun', 'MapOnce', False);
  TestD2dExe := config.ReadStr('TestRun', 'Exe', EditorDir+'DoomForever.exe');

  config.Destroy;

  FindD2dDialog.InitialDir := TestD2dExe;
end;

procedure TMapTestForm.bChooseD2dClick(Sender: TObject);
begin
  if FindD2dDialog.Execute then
    begin
      edD2dExe.Text := FindD2dDialog.FileName;
    end;
end;

end.
