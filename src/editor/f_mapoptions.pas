unit f_mapoptions;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  SysUtils, Classes, Forms, Dialogs,
  Controls, StdCtrls, ComCtrls, Buttons,
  f_main, utils;

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
    eMapWidth: TEdit;
    UpDown1: TUpDown;
    eMapHeight: TEdit;
    UpDown2: TUpDown;
    LabelWidth: TLabel;
    LabelHeight: TLabel;
    LabelCurSize: TLabel;
    lCurrentMapSizes: TLabel;

  // Навигатор направления смещения
    sbMoveCenter: TSpeedButton;
    sbMoveLeft: TSpeedButton;
    sbMoveRight: TSpeedButton;
    sbMoveUp: TSpeedButton;
    sbMoveUpLeft: TSpeedButton;
    sbMoveUpRight: TSpeedButton;
    sbMoveDown: TSpeedButton;
    sbMoveDownLeft: TSpeedButton;
    sbMoveDownRight: TSpeedButton;
    LabelMapMove: TLabel;
    cbSnapping: TCheckBox;

  // Кнопки:
    bOK: TButton;
    bCancel: TButton;
    
    procedure FormActivate(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);

    procedure eMapNameChange(Sender: TObject);
    procedure eMapDescriptionChange(Sender: TObject);
    procedure eAuthorChange(Sender: TObject);

    procedure bSelectBackClick(Sender: TObject);
    procedure bSelectMusicClick(Sender: TObject);
    procedure bRemoveBackClick(Sender: TObject);
    procedure bRemoveMusicClick(Sender: TObject);
    procedure eMapSizeKeyPress(Sender: TObject; var Key: Char);

  private
    function CalcOffsetX(WidthDiff: Integer): Integer;
    function CalcOffsetY(HeightDiff: Integer): Integer;

  public
    { Public declarations }
  end;

var
  MapOptionsForm: TMapOptionsForm;

implementation

uses
  g_map, f_addresource_sky, f_addresource_sound;

{$R *.lfm}

// Callbacks to receive results from resource choosing dialogs
function SetSky: Boolean;
begin
  MapOptionsForm.eBack.Text := win2utf(AddSkyForm.ResourceName);
  Result := True;
end;

function SetMusic: Boolean;
begin
  MapOptionsForm.eMusic.Text := win2utf(AddSoundForm.ResourceName);
  Result := True;
end;

// Form processing
procedure TMapOptionsForm.FormActivate(Sender: TObject);
var
  a, b: Integer;
begin
  // General map options
  eMapName.Text := win2utf(gMapInfo.Name);
  eMapDescription.Text := win2utf(gMapInfo.Description);
  eAuthor.Text := win2utf(gMapInfo.Author);

  eBack.Text := win2utf(gMapInfo.SkyName);
  eMusic.Text := win2utf(gMapInfo.MusicName);

  eMapWidth.Text := IntToStr(gMapInfo.Width);
  eMapHeight.Text := IntToStr(gMapInfo.Height);
  lCurrentMapSizes.Caption := eMapWidth.Text + 'x' + eMapHeight.Text;

  sbMoveCenter.Down := True;

  // Map statistics
  lTextureCount.Caption := IntToStr(MainForm.lbTextureList.Count);

  b := 0; // Panels
  if gPanels <> nil then
    for a := 0 to High(gPanels) do
      if gPanels[a].PanelType <> 0 then b := b+1;
  lPanelCount.Caption := IntToStr(b);

  b := 0; // Items
  if gItems <> nil then
    for a := 0 to High(gItems) do
      if gItems[a].ItemType <> 0 then b := b+1;
  lItemCount.Caption := IntToStr(b);

  b := 0; // Areas
  if gAreas <> nil then
    for a := 0 to High(gAreas) do
      if gAreas[a].AreaType <> 0 then b := b+1;
  lAreaCount.Caption := IntToStr(b);

  b := 0; // Monsters
  if gMonsters <> nil then
    for a := 0 to High(gMonsters) do
      if gMonsters[a].MonsterType <> 0 then b := b+1;
  lMonsterCount.Caption := IntToStr(b);

  b := 0; // Triggers
  if gTriggers <> nil then
    for a := 0 to High(gTriggers) do
      if gTriggers[a].TriggerType <> 0 then
        b := b + 1;
  lTriggerCount.Caption := IntToStr(b);

end;

procedure TMapOptionsForm.bCancelClick(Sender: TObject);
begin
  Close();
end;

procedure TMapOptionsForm.bOKClick(Sender: TObject);
var
  newWidth, newHeight: Integer;
begin  
  newWidth := StrToInt(eMapWidth.Text);
  newHeight := StrToInt(eMapHeight.Text);
              
  with gMapInfo do
  begin
    Name := utf2win(eMapName.Text);
    Description := utf2win(eMapDescription.Text);
    Author := utf2win(eAuthor.Text);
    SkyName := utf2win(eBack.Text);
    MusicName := utf2win(eMusic.Text);

    if Width > newWidth then
      MapOffset.X := 0;
    if Height > newHeight then
      MapOffset.Y := 0;

    ShiftMapObjects( CalcOffsetX(newWidth - Width),
                     CalcOffsetY(newHeight - Height) );

    Width := newWidth;
    Height := newHeight;
  end;

  LoadSky(gMapInfo.SkyName);

  MainForm.FormResize(Self);
  Close();
end;

// Counters of chars in edit fields
procedure TMapOptionsForm.eMapNameChange(Sender: TObject);
begin
  lCharCountName.Caption := Format('%.2d\32', [Length(eMapName.Text)]);
end;

procedure TMapOptionsForm.eMapDescriptionChange(Sender: TObject);
begin
  lCharCountDescription.Caption := Format('%.3d\256', [Length(eMapDescription.Text)]);
end;

procedure TMapOptionsForm.eAuthorChange(Sender: TObject);
begin
  lCharCountAuthor.Caption := Format('%.2d\32', [Length(eAuthor.Text)]);
end;

// Buttons processing
procedure TMapOptionsForm.bSelectBackClick(Sender: TObject);
begin
  AddSkyForm.OKFunction := SetSky;
  AddSkyForm.lbResourcesList.MultiSelect := False;
  AddSkyForm.SetResource := utf2win(eBack.Text);
  AddSkyForm.ShowModal();
end;

procedure TMapOptionsForm.bSelectMusicClick(Sender: TObject);
begin
  AddSoundForm.OKFunction := SetMusic;
  AddSoundForm.lbResourcesList.MultiSelect := False;
  AddSoundForm.SetResource := utf2win(eMusic.Text);
  AddSoundForm.ShowModal();
end;

procedure TMapOptionsForm.bRemoveBackClick(Sender: TObject);
begin
  eBack.Clear();
end;

procedure TMapOptionsForm.bRemoveMusicClick(Sender: TObject);
begin
  eMusic.Clear();
end;

// Map width/height edit fields input processor: only digits are allowed
procedure TMapOptionsForm.eMapSizeKeyPress( Sender: TObject;
  var Key: Char );
begin
  if not ( Key in ['0'..'9', #8, #127] ) then // #8 - bs, #127 - del
    Key := #0;
end;

// Offsets calculating for shifting map objects
function TMapOptionsForm.CalcOffsetX(WidthDiff: Integer): Integer;
begin
  Result := 0;
  if (sbMoveCenter.Down or
      sbMoveUp.Down or
      sbMoveDown.Down) then Result := WidthDiff div 2
  else
  if (sbMoveRight.Down or
      sbMoveUpRight.Down or
      sbMoveDownRight.Down) then Result := WidthDiff;

  if cbSnapping.Checked then Result := Trunc(Result / DotStep) * DotStep;
end;

function TMapOptionsForm.CalcOffsetY(HeightDiff: Integer): Integer;
begin
  Result := 0;
  if (sbMoveCenter.Down or
      sbMoveLeft.Down or
      sbMoveRight.Down) then Result := HeightDiff div 2
  else
  if (sbMoveDown.Down or
      sbMoveDownLeft.Down or
      sbMoveDownRight.Down) then Result := HeightDiff;

  if cbSnapping.Checked then Result := Trunc(Result / DotStep) * DotStep;
end;

end.
