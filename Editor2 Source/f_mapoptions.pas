unit f_mapoptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, f_main, ComCtrls;

type
  TMapOptionsForm = class (TForm)
  // Имя карты:
    LabelName: TLabel;
    lCharCountName: TLabel;
    eMapName: TEdit;
  // Описание карты:
    LabelDesc: TLabel;
    lCharCountDescription: TLabel;
    eMapDescription: TEdit;
  // Автор карты:
    LabelAuthor: TLabel;
    lCharCountAuthor: TLabel;
    eAuthor: TEdit;
  // Фон:
    LabelBack: TLabel;
    eBack: TEdit;
    bRemoveBack: TButton;
    bSelectBack: TButton;
  // Музыка:
    LabelMusic: TLabel;
    eMusic: TEdit;
    bRemoveMusic: TButton;
    bSelectMusic: TButton;

  // Статистика:
    GBStats: TGroupBox;
    LabelTexs: TLabel;
    lTextureCount: TLabel;
    LabelPanels: TLabel;
    lPanelCount: TLabel;
    LabelItems: TLabel;
    lItemCount: TLabel;
    LabelMonsters: TLabel;
    lMonsterCount: TLabel;
    LabelAreas: TLabel;
    lAreaCount: TLabel;
    LabelTriggers: TLabel;
    lTriggerCount: TLabel;

  // Размеры:
    GBSizes: TGroupBox;
  // Сдвинуть левую границу:
    LabelLeftBorder: TLabel;
    bLeftB: TButton;
    LabelFor1: TLabel;
    eLeftB: TEdit;
    UpDown1: TUpDown;
  // Сдвинуть правую границу:
    LabelRightBorder: TLabel;
    bRightB: TButton;
    LabelFor2: TLabel;
    eRightB: TEdit;
    UpDown2: TUpDown;
  // Сдвинуть верхнюю границу:
    LabelTopBorder: TLabel;
    bUpB: TButton;
    LabelFor3: TLabel;
    eUpB: TEdit;
    UpDown3: TUpDown;
  // Сдвинуть нижнюю границу:
    LabelBottomBorder: TLabel;
    bDownB: TButton;
    LabelFor4: TLabel;
    eDownB: TEdit;
    UpDown4: TUpDown;
  // Размеры карты:
    LabelMapSize: TLabel;
    LabelWidth: TLabel;
    lMapWidth: TLabel;
    LabelHeight: TLabel;
    lMapHeight: TLabel;

  // Кнопки:
    bOK: TButton;
    bCancel: TButton;
    
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
    procedure bLeftBClick(Sender: TObject);
    procedure bRightBClick(Sender: TObject);
    procedure bUpBClick(Sender: TObject);
    procedure bDownBClick(Sender: TObject);
    procedure eLeftBChange(Sender: TObject);

  private
    bpLeft: Boolean;
    bpRight: Boolean;
    bpUp: Boolean;
    bpDown: Boolean;

    procedure RecalcMapSize();
    procedure ResetDirButtons();

  public
    { Public declarations }
  end;

var
  MapOptionsForm: TMapOptionsForm;

implementation

uses
  g_map, f_addresource_sky, f_addresource_sound, math,
  g_language;

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
      if gPanels[a].PanelType <> 0 then
        b := b + 1;
  lPanelCount.Caption := IntToStr(b);

  b := 0;
  if gItems <> nil then
    for a := 0 to High(gItems) do
      if gItems[a].ItemType <> 0 then
        b := b + 1;
  lItemCount.Caption := IntToStr(b);

  b := 0;
  if gAreas <> nil then
    for a := 0 to High(gAreas) do
      if gAreas[a].AreaType <> 0 then
        b := b + 1;
  lAreaCount.Caption := IntToStr(b);

  b := 0;
  if gMonsters <> nil then
    for a := 0 to High(gMonsters) do
      if gMonsters[a].MonsterType <> 0 then
        b := b + 1;
  lMonsterCount.Caption := IntToStr(b);

  b := 0;
  if gTriggers <> nil then
    for a := 0 to High(gTriggers) do
      if gTriggers[a].TriggerType <> 0 then
        b := b + 1;
  lTriggerCount.Caption := IntToStr(b);

  ResetDirButtons();
end;

procedure TMapOptionsForm.bCancelClick(Sender: TObject);
begin
  Close();
end;

procedure TMapOptionsForm.bOKClick(Sender: TObject);
var
  newWidth, newHeight: Integer;
  newLeft, newUp: Integer;

begin
  newLeft := StrToIntDef(eLeftB.Text, 0);
  newUp := StrToIntDef(eUpB.Text, 0);

  if not bpLeft then
    newLeft := -newLeft;
  if not bpUp then
    newUp := -newUp;

  newWidth := StrToIntDef(lMapWidth.Caption, gMapInfo.Width);
  newHeight := StrToIntDef(lMapHeight.Caption, gMapInfo.Height);

  with gMapInfo do
  begin
    Name := eMapName.Text;
    Description := eMapDescription.Text;
    Author := eAuthor.Text;
    SkyName := eBack.Text;
    MusicName := eMusic.Text;

    if Width > newWidth then
      MapOffset.X := 0;
    if Height > newHeight then
      MapOffset.Y := 0;
    Width := newWidth;
    Height := newHeight;
  end;

  if (newLeft <> 0) or (newUp <> 0) then
    ShiftMapObjects(newLeft, newUp);

  LoadSky(gMapInfo.SkyName);

  MainForm.FormResize(Self);
  Close();
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
  AddSkyForm.ShowModal();
end;

procedure TMapOptionsForm.bSelectMusicClick(Sender: TObject);
begin
  AddSoundForm.OKFunction := SetMusic;
  AddSoundForm.lbResourcesList.MultiSelect := False;
  AddSoundForm.SetResource := eMusic.Text;
  AddSoundForm.ShowModal();
end;

procedure TMapOptionsForm.eAuthorChange(Sender: TObject);
begin
  lCharCountAuthor.Caption := Format('%.2d\32', [Length(eAuthor.Text)]);
end;

procedure TMapOptionsForm.bRemoveBackClick(Sender: TObject);
begin
  eBack.Clear();
end;

procedure TMapOptionsForm.bRemoveMusicClick(Sender: TObject);
begin
  eMusic.Clear();
end;

procedure TMapOptionsForm.RecalcMapSize();
var
  newWidth, newHeight: Integer;
  nLeft, nRight, nUp, nDown: Integer;

begin
  nLeft := StrToIntDef(eLeftB.Text, 0);
  nRight := StrToIntDef(eRightB.Text, 0);
  nUp := StrToIntDef(eUpB.Text, 0);
  nDown := StrToIntDef(eDownB.Text, 0);

  if not bpLeft then
    nLeft := -nLeft;
  if not bpRight then
    nRight := -nRight;
  if not bpUp then
    nUp := -nUp;
  if not bpDown then
    nDown := -nDown;

  newWidth := gMapInfo.Width;
  newHeight := gMapInfo.Height;

  newWidth := newWidth + nLeft + nRight;
  newHeight := newHeight + nUp + nDown;

  lMapWidth.Caption := IntToStr(Max(newWidth, 0));
  lMapHeight.Caption := IntToStr(Max(newHeight, 0));
end;

procedure TMapOptionsForm.ResetDirButtons();
begin
  bpLeft := False;
  bpRight := False;
  bpUp := False;
  bpDown := False;
  
  bLeftBClick(bLeftB);
  bRightBClick(bRightB);
  bUpBClick(bUpB);
  bDownBClick(bDownB);

  eLeftB.Text := '0';
  eRightB.Text := '0';
  eUpB.Text := '0';
  eDownB.Text := '0';

  RecalcMapSize();
end;

procedure TMapOptionsForm.bLeftBClick(Sender: TObject);
begin
  bpLeft := not bpLeft;
  if bpLeft then
    (Sender as TButton).Caption := DirButtonNames[1]
  else
    (Sender as TButton).Caption := DirButtonNames[2];
  RecalcMapSize();
end;

procedure TMapOptionsForm.bRightBClick(Sender: TObject);
begin
  bpRight := not bpRight;
  if bpRight then
    (Sender as TButton).Caption := DirButtonNames[2]
  else
    (Sender as TButton).Caption := DirButtonNames[1];
  RecalcMapSize();
end;

procedure TMapOptionsForm.bUpBClick(Sender: TObject);
begin
  bpUp := not bpUp;
  if bpUp then
    (Sender as TButton).Caption := DirButtonNames[3]
  else
    (Sender as TButton).Caption := DirButtonNames[4];
  RecalcMapSize();
end;

procedure TMapOptionsForm.bDownBClick(Sender: TObject);
begin
  bpDown := not bpDown;
  if bpDown then
    (Sender as TButton).Caption := DirButtonNames[4]
  else
    (Sender as TButton).Caption := DirButtonNames[3];
  RecalcMapSize();
end;

procedure TMapOptionsForm.eLeftBChange(Sender: TObject);
var
  s: String;
  i, len: Integer;

begin
  s := (Sender as TEdit).Text;

  i := 1;
  len := Length(s);
  while (i <= Len) do
    if (s[i] < '0') or (s[i] > '9') then
      begin
        Delete(s, i, 1);
        Dec(len);
      end
    else
      Inc(i);
      
  (Sender as TEdit).Text := s;
  RecalcMapSize();
end;

end.
