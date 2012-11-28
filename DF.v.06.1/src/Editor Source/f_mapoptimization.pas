unit f_mapoptimization;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TMapOptimizationForm = class(TForm)
    GroupBox1: TGroupBox;
    pcOptimizationOptions: TPageControl;
    tsTextureOptimization: TTabSheet;
    tsPanelOptimization: TTabSheet;
    rbTexturesOptimization: TRadioButton;
    rbPanelsOptimization: TRadioButton;
    Bevel1: TBevel;
    lOptimizationDescription: TLabel;
    cbOptimizeWalls: TCheckBox;
    bBeginPanelsOptimization: TButton;
    cbOptimizeForeGround: TCheckBox;
    cbOptimizeBackGround: TCheckBox;
    cbOptimizeSteps: TCheckBox;
    cbOptimizeWater: TCheckBox;
    cbOptimizeAcid1: TCheckBox;
    cbOptimizeAcid2: TCheckBox;
    bBeginTextureOptimization: TButton;
    mOptimizationResult: TMemo;
    procedure FormActivate(Sender: TObject);
    procedure rbTexturesOptimizationClick(Sender: TObject);
    procedure rbPanelsOptimizationClick(Sender: TObject);
    procedure bBeginTextureOptimizationClick(Sender: TObject);
    procedure bBeginPanelsOptimizationClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MapOptimizationForm: TMapOptimizationForm;

implementation

uses f_main;

{$R *.dfm}

procedure TMapOptimizationForm.FormActivate(Sender: TObject);
begin
 rbTexturesOptimization.Checked := True;
 mOptimizationResult.Clear;
end;

procedure TMapOptimizationForm.rbTexturesOptimizationClick(
  Sender: TObject);
begin
 pcOptimizationOptions.ActivePage := tsTextureOptimization;
 lOptimizationDescription.Caption := 'Удаляет неиспользуемые текстуры из списка';
end;

procedure TMapOptimizationForm.rbPanelsOptimizationClick(Sender: TObject);
begin
 pcOptimizationOptions.ActivePage := tsPanelOptimization;
 lOptimizationDescription.Caption :=
  '"Склеивает" расположенные вплотную друг к другу одинаковые панели в одну. '+
  'Увеличивает производительность игры';
end;

procedure TMapOptimizationForm.bBeginTextureOptimizationClick(
  Sender: TObject);
var
  i: Integer;
  a: Integer;
  ok: Boolean;
  b: Boolean;
  c: Integer;
begin
 mOptimizationResult.Clear;
 b := False;

 if MainForm.lbTextureList.Count = 0 then
 begin
  mOptimizationResult.Lines.Add('Список текстур пустой');
  Exit;
 end;

 c := MainForm.lbTextureList.Count;
 a := 0;

 while a <= MainForm.lbTextureList.Count-1 do
 begin
  ok := True;
  for i := 0 to High(MainLevel.Map.PanelSystem.PanelsArray) do
   if (MainLevel.Map.PanelSystem.PanelsArray[i].TextureName =
       MainForm.lbTextureList.Items[a]) then
   begin
    ok := False;
    Break;
   end;
  if ok then
  begin
   MainLevel.Map.PanelSystem.DeleteTextureByName(MainForm.lbTextureList.Items[a]);
   if not(b) then
   begin
    mOptimizationResult.Lines.Add('Удаленные текстуры: ');
    b := True;
   end;
   mOptimizationResult.Lines.Add('  '+MainForm.lbTextureList.Items[a]);
   MainForm.lbTextureList.Items.Delete(a);
   a := -1;
  end;
  Inc(a);
 end;
 mOptimizationResult.Lines.Add(#13#10+'Всего текстур: '+#9+IntToStr(c));
 mOptimizationResult.Lines.Add('Удалено текстур: '+#9+IntToStr(c-MainForm.lbTextureList.Count));
end;

procedure TMapOptimizationForm.bBeginPanelsOptimizationClick(
  Sender: TObject);

function OptimizePanels(PanelsType: TPanelType): Integer;
 var
  a: Boolean;
  i: Integer;
  n: Integer;
begin
 Result := 0;
 
 a := True;
 while a do
 begin
  a := False;

  for i := 0 to High(MainLevel.Map.PanelSystem.PanelsArray) do
   for n := 0 to High(MainLevel.Map.PanelSystem.PanelsArray) do
    with MainLevel.Map.PanelSystem do
    begin
     if (i <> n) and
        (PanelsArray[i].Width <> 0) and
        (PanelsArray[n].Width <> 0) and
        (PanelsArray[n].TextureID = PanelsArray[i].TextureID) and
        (PanelsArray[n].PanelType = PanelsArray[i].PanelType) and
        (PanelsArray[n].PanelType = PanelsType) and
        (PanelsArray[n].Alpha = PanelsArray[i].Alpha) then
     begin
      if (PanelsArray[n].X = PanelsArray[i].X + PanelsArray[i].Width) and
         (PanelsArray[n].Y = PanelsArray[i].Y) and
         (PanelsArray[n].Height = PanelsArray[i].Height) then
      begin
       PanelsArray[i].Width := PanelsArray[i].Width+PanelsArray[n].Width;
       DeletePanel(n);
       a := True;
       Inc(Result);
       Continue;
      end;

      if (PanelsArray[n].Y = PanelsArray[i].Y + PanelsArray[i].Height) and
         (PanelsArray[n].X = PanelsArray[i].X) and
         (PanelsArray[n].Width = PanelsArray[i].Width) then
      begin
       PanelsArray[i].Height := PanelsArray[i].Height+PanelsArray[n].Height;
       DeletePanel(n);
       a := True;
       Inc(Result);
       Continue;
      end;
     end;
    end;
 end;
end;

var
  count: Integer;
  panelcount: Integer;

begin
 panelcount := MainLevel.Map.PanelSystem.PanelCount;

 mOptimizationResult.Clear;

 if cbOptimizeWalls.Checked then
 begin
  mOptimizationResult.Lines.Add('Оптимизация стен...');
  count := OptimizePanels(PANEL_WALL);
  mOptimizationResult.Lines.Add('Оптимизированно панелей: '+IntToStr(count)+#13#10);
 end;

 if cbOptimizeForeGround.Checked then
 begin
  mOptimizationResult.Lines.Add('Оптимизация переднего плана...');
  count := OptimizePanels(PANEL_FOREGROUND);
  mOptimizationResult.Lines.Add('Оптимизированно панелей: '+IntToStr(count)+#13#10);
 end;

 if cbOptimizeBackGround.Checked then
 begin
  mOptimizationResult.Lines.Add('Оптимизация заднего плана...');
  count := OptimizePanels(PANEL_BACKGROUND);
  mOptimizationResult.Lines.Add('Оптимизированно панелей: '+IntToStr(count)+#13#10);
 end;

 if cbOptimizeSteps.Checked then
 begin
  mOptimizationResult.Lines.Add('Оптимизация ступеней...');
  count := OptimizePanels(PANEL_STEP);
  mOptimizationResult.Lines.Add('Оптимизированно панелей: '+IntToStr(count)+#13#10);
 end;

 if cbOptimizeWater.Checked then
 begin
  mOptimizationResult.Lines.Add('Оптимизация воды...');
  count := OptimizePanels(PANEL_WATER);
  mOptimizationResult.Lines.Add('Оптимизированно панелей: '+IntToStr(count)+#13#10);
 end;

 if cbOptimizeAcid1.Checked then
 begin
  mOptimizationResult.Lines.Add('Оптимизация кислоты 1...');
  count := OptimizePanels(PANEL_ACID1);
  mOptimizationResult.Lines.Add('Оптимизированно панелей: '+IntToStr(count)+#13#10);
 end;

 if cbOptimizeAcid2.Checked then
 begin
  mOptimizationResult.Lines.Add('Оптимизация кислоты 2...');
  count := OptimizePanels(PANEL_ACID2);
  mOptimizationResult.Lines.Add('Оптимизированно панелей: '+IntToStr(count)+#13#10);
 end;

 mOptimizationResult.Lines.Add('-----------------------');
 mOptimizationResult.Lines.Add('Всего панелей: '+IntToStr(panelcount));
 mOptimizationResult.Lines.Add('Панелей после оптимизации: '+IntToStr(MainLevel.Map.PanelSystem.PanelCount));
 mOptimizationResult.Lines.Add('Оптимизированно панелей: '+IntToStr(panelcount-MainLevel.Map.PanelSystem.PanelCount));
end;

end.
