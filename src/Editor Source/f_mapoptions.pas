unit f_mapoptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, f_main, ComCtrls, g_items, g_areas, g_monsters;

type
  TMapOptionsForm = class(TForm)
    leMapName: TLabeledEdit;
    GroupBox1: TGroupBox;
    lPanelCount: TLabel;
    lTextureCount: TLabel;
    lItemCount: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label1: TLabel;
    bOK: TButton;
    bCancel: TButton;
    Label2: TLabel;
    lMonsterCount: TLabel;
    Label3: TLabel;
    lAreaCount: TLabel;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    eMapHeight: TEdit;
    eMapWidth: TEdit;
    UpDown2: TUpDown;
    UpDown1: TUpDown;
    mMapDescription: TMemo;
    procedure FormActivate(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MapOptionsForm: TMapOptionsForm;

implementation

{$R *.dfm}

procedure TMapOptionsForm.FormActivate(Sender: TObject);
begin
 leMapName.Text := MainLevel.Map.FName;
 mMapDescription.Text := MainLevel.Map.FDescription;
 lTextureCount.Caption := IntToStr(MainLevel.Map.PanelSystem.TexturesCount);
 lPanelCount.Caption := IntToStr(MainLevel.Map.PanelSystem.PanelCount);
 lItemCount.Caption := IntToStr(MainLevel.ItemSystem.GetCount);
 lMonsterCount.Caption := IntToStr(MainLevel.MonsterSystem.GetCount);
 lAreaCount.Caption := IntToStr(MainLevel.AreaSystem.GetCount);
 eMapWidth.Text := IntToStr(MainLevel.Map.FWidth);
 eMapHeight.Text := IntToStr(MainLevel.Map.FHeight);
end;

procedure TMapOptionsForm.bCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TMapOptionsForm.bOKClick(Sender: TObject);
begin
 CopyMemory(@MainLevel.Map.FName[0], @leMapName.Text[1], Length(leMapName.Text));
 CopyMemory(@MainLevel.Map.FDescription[0], @mMapDescription.Text[1], Length(mMapDescription.Text));
 MainLevel.Map.FWidth := StrToInt(eMapWidth.Text);
 MainLevel.Map.FHeight := StrToInt(eMapHeight.Text);
 MainForm.FormResize(Self);
 Close;
end;

end.
