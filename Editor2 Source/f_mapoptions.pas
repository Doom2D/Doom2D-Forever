unit f_mapoptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, f_main, ComCtrls;

type
  TMapOptionsForm = class(TForm)
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
    eMapDescription: TEdit;
    lCharCountName: TLabel;
    lCharCountDescription: TLabel;
    Label9: TLabel;
    eMapName: TEdit;
    UpDown2: TUpDown;
    UpDown1: TUpDown;
    Label7: TLabel;
    Label8: TLabel;
    eMapHeight: TEdit;
    eMapWidth: TEdit;
    Label10: TLabel;
    eBack: TEdit;
    Label12: TLabel;
    eMusic: TEdit;
    bSelectBack: TButton;
    bSelectMusic: TButton;
    Label11: TLabel;
    eAuthor: TEdit;
    lCharCountAuthor: TLabel;
    Label13: TLabel;
    lTriggerCount: TLabel;
    bRemoveBack: TButton;
    bRemoveMusic: TButton;
    procedure FormActivate(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure eMapNameChange(Sender: TObject);
    procedure eMapDescriptionChange(Sender: TObject);
    procedure bSelectBackClick(Sender: TObject);
    procedure bSelectMusicClick(Sender: TObject);
    procedure eAuthorChange(Sender: TObject);
    procedure bRemoveBackClick(Sender: TObject);
    procedure bRemoveMusicClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MapOptionsForm: TMapOptionsForm;

implementation

uses
  g_map, f_addresource_sky, f_addresource_sound;

{$R *.dfm}

function SetSky: Boolean;
begin
 MapOptionsForm.eBack.Text := AddSkyForm.ResourceName;
 Result := True;
end;

function SetMusic: Boolean;
begin
 MapOptionsForm.eMusic.Text := AddSoundForm.ResourceName;
 Result := True;
end;

procedure TMapOptionsForm.FormActivate(Sender: TObject);
var
  a, b: Integer;
begin
 eMapName.Text := gMapInfo.Name;
 eMapDescription.Text := gMapInfo.Description;
 eAuthor.Text := gMapInfo.Author;
 lTextureCount.Caption := IntToStr(MainForm.lbTextureList.Count);

 eBack.Text := gMapInfo.SkyName;
 eMusic.Text := gMapInfo.MusicName;

 b := 0;
 if gPanels <> nil then
 for a := 0 to High(gPanels) do
  if gPanels[a].PanelType <> 0 then b := b+1;
 lPanelCount.Caption := IntToStr(b);

 b := 0;
 if gItems <> nil then
 for a := 0 to High(gItems) do
  if gItems[a].ItemType <> 0 then b := b+1;
 lItemCount.Caption := IntToStr(b);

 b := 0;
 if gAreas <> nil then
 for a := 0 to High(gAreas) do
  if gAreas[a].AreaType <> 0 then b := b+1;
 lAreaCount.Caption := IntToStr(b);

 b := 0;
 if gMonsters <> nil then
 for a := 0 to High(gMonsters) do
  if gMonsters[a].MonsterType <> 0 then b := b+1;
 lMonsterCount.Caption := IntToStr(b);

 b := 0;
 if gTriggers <> nil then
 for a := 0 to High(gTriggers) do
  if gTriggers[a].TriggerType <> 0 then b := b+1;
 lTriggerCount.Caption := IntToStr(b);

 eMapWidth.Text := IntToStr(gMapInfo.Width);
 eMapHeight.Text := IntToStr(gMapInfo.Height);
end;

procedure TMapOptionsForm.bCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TMapOptionsForm.bOKClick(Sender: TObject);
begin
 with gMapInfo do
 begin
  Name := eMapName.Text;
  Description := eMapDescription.Text;
  Author := eAuthor.Text;
  SkyName := eBack.Text;
  MusicName := eMusic.Text;
  Height := StrToIntDef(eMapHeight.Text, 768);
  Width := StrToIntDef(eMapWidth.Text, 1024);
 end;

 LoadSky(gMapInfo.SkyName);

 MainForm.FormResize(Self);
 Close;
end;

procedure TMapOptionsForm.eMapNameChange(Sender: TObject);
begin
 lCharCountName.Caption := Format('%.2d\32', [Length(eMapName.Text)]);
end;

procedure TMapOptionsForm.eMapDescriptionChange(Sender: TObject);
begin
 lCharCountDescription.Caption := Format('%.3d\256', [Length(eMapDescription.Text)]);
end;

procedure TMapOptionsForm.bSelectBackClick(Sender: TObject);
begin
 AddSkyForm.OKFunction := SetSky;
 AddSkyForm.lbResourcesList.MultiSelect := False;
 AddSkyForm.SetResource := eBack.Text;
 AddSkyForm.ShowModal;
end;

procedure TMapOptionsForm.bSelectMusicClick(Sender: TObject);
begin
 AddSoundForm.OKFunction := SetMusic;
 AddSoundForm.lbResourcesList.MultiSelect := False;
 AddSoundForm.SetResource := eMusic.Text;
 AddSoundForm.ShowModal;
end;

procedure TMapOptionsForm.eAuthorChange(Sender: TObject);
begin
 lCharCountAuthor.Caption := Format('%.2d\32', [Length(eAuthor.Text)]);
end;

procedure TMapOptionsForm.bRemoveBackClick(Sender: TObject);
begin
 eBack.Clear;
end;

procedure TMapOptionsForm.bRemoveMusicClick(Sender: TObject);
begin
 eMusic.Clear;
end;

end.
