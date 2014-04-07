unit f_mapoptimization;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls;

type
  TMapOptimizationForm = class (TForm)
  // Выбор оптимизации:
    GroupBoxOpt: TGroupBox;
    Bevel1: TBevel;
    lOptimizationDescription: TLabel;
    rbTexturesOptimization: TRadioButton;
    rbPanelsOptimization: TRadioButton;
  // Результаты:
    mOptimizationResult: TMemo;
  // Настройки:
    pcOptimizationOptions: TPageControl;
  // Оптимизация текстур:
    tsTextureOptimization: TTabSheet;
    bBeginTextureOptimization: TButton;
  // Оптимизация панелей:
    tsPanelOptimization: TTabSheet;
    cbOptimizeWalls: TCheckBox;
    cbOptimizeForeGround: TCheckBox;
    cbOptimizeBackGround: TCheckBox;
    cbOptimizeSteps: TCheckBox;
    cbOptimizeWater: TCheckBox;
    cbOptimizeAcid1: TCheckBox;
    cbOptimizeAcid2: TCheckBox;
    cbOptimizeLift: TCheckBox;
    cbOptimizeBlockMon: TCheckBox;
    bBeginPanelsOptimization: TButton;

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

uses
  f_main, g_map, g_textures, MAPDEF, g_language;

{$R *.dfm}

procedure TMapOptimizationForm.FormActivate(Sender: TObject);
begin
  rbTexturesOptimization.Checked := True;
  mOptimizationResult.Clear();
end;

procedure TMapOptimizationForm.rbTexturesOptimizationClick(Sender: TObject);
begin
  pcOptimizationOptions.ActivePage := tsTextureOptimization;
  lOptimizationDescription.Caption := _lc[I_CTRL_OPT_DESC_TEXTURE];
end;

procedure TMapOptimizationForm.rbPanelsOptimizationClick(Sender: TObject);
begin
  pcOptimizationOptions.ActivePage := tsPanelOptimization;
  lOptimizationDescription.Caption := _lc[I_CTRL_OPT_DESC_PANEL];
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
  mOptimizationResult.Clear();
  b := False;

  if MainForm.lbTextureList.Count = 0 then
  begin
    mOptimizationResult.Lines.Add(_lc[I_OPT_NO_TEXTURES]);
    Exit;
  end;

  c := MainForm.lbTextureList.Count;
  a := 0;

  while a <= MainForm.lbTextureList.Count-1 do
  begin
    ok := True;

    for i := 0 to High(gPanels) do
      if (gPanels[i].PanelType <> 0) and
         (gPanels[i].TextureName = MainForm.lbTextureList.Items[a]) then
      begin
        ok := False;
        Break;
      end;

  // Нашли неиспользуемую текстуру:
    if ok then
      begin
        g_DeleteTexture(MainForm.lbTextureList.Items[a]);
        if not b then
        begin
          mOptimizationResult.Lines.Add(_lc[I_OPT_DELETED_TEXTURES]);
          b := True;
        end;
        mOptimizationResult.Lines.Add('  '+MainForm.lbTextureList.Items[a]);
        g_DeleteTexture(MainForm.lbTextureList.Items[a]);
        MainForm.lbTextureList.Items.Delete(a);
      end
    else
      a := a + 1;
  end;

  with mOptimizationResult.Lines do
  begin
    Add(#13#10+_lc[I_OPT_TOTAL_TEXTURES]+' '+#9+IntToStr(c));
    Add(_lc[I_OPT_TEX_DELETED]+#9+IntToStr(c-MainForm.lbTextureList.Count));
  end;
end;

procedure TMapOptimizationForm.bBeginPanelsOptimizationClick(
  Sender: TObject);

  function OptimizePanels(PanelsType: Word): Integer;
  var
    a, c: Boolean;
    i, n, b: Integer;
    list: Array of DWORD;

  begin
    Result := 0;

  // Составляем список переключаемых лифтов:
    list := nil;
    if WordBool(PanelsType and (PANEL_LIFTUP or PANEL_LIFTDOWN)) then
    begin
      SetLength(list, 32);
      n := 0;

      if gTriggers <> nil then
        for i := 0 to High(gTriggers) do
          if (gTriggers[i].Data.PanelID <> -1) and
             ((gTriggers[i].TriggerType = TRIGGER_LIFT) or
              (gTriggers[i].TriggerType = TRIGGER_LIFTUP) or
              (gTriggers[i].TriggerType = TRIGGER_LIFTDOWN)) then
          begin
            if n > High(list) then
              SetLength(list, Length(list)+32);
            list[n] := gTriggers[i].Data.PanelID;
            n := n + 1;
          end;

      SetLength(list, n);
    end;

  // Оптимизация:
    a := True;
    while a do
    begin
      a := False;

      for i := 0 to High(gPanels) do
        if gPanels[i].PanelType <> PANEL_NONE then
        begin
          c := False;
          if list <> nil then
            for b := 0 to High(list) do
              if list[b] = DWORD(i) then
              begin
                c := True;
                Break;
             end;

        // Это переключаемый лифт:
          if c then
            Continue;

          for n := 0 to High(gPanels) do
            if gPanels[i].PanelType <> PANEL_NONE then
            begin
              c := False;
              if list <> nil then
                for b := 0 to High(list) do
                  if list[b] = DWORD(n) then
                  begin
                    c := True;
                    Break;
                  end;

            // Это тоже переключаемый лифт:
              if c then
                Continue;

            // Если можно - объединяем панели:
              if (gPanels[i].PanelType <> 0) and
                 (gPanels[n].PanelType <> 0) then
              begin
                if (i <> n) and
                   (gPanels[i].Width <> 0) and
                   (gPanels[n].Width <> 0) and
                   (gPanels[n].TextureID = gPanels[i].TextureID) and
                   (gPanels[n].PanelType = gPanels[i].PanelType) and
                   (gPanels[n].PanelType = PanelsType) and
                   (gPanels[n].Alpha = gPanels[i].Alpha) and
                   (gPanels[n].Blending = gPanels[i].Blending) and
                   (gPanels[n].TextureName = gPanels[i].TextureName) then
                begin
                // Рядом по-горизонтали:
                  if (gPanels[n].X = gPanels[i].X + gPanels[i].Width) and
                     (gPanels[n].Y = gPanels[i].Y) and
                     (gPanels[n].Height = gPanels[i].Height) then
                  begin
                    gPanels[i].Width := gPanels[i].Width+gPanels[n].Width;
                    RemoveObject(n, OBJECT_PANEL);
                    a := True;
                    Inc(Result);
                    Continue;
                  end;

                // Рядом по-вертикали:
                  if (gPanels[n].Y = gPanels[i].Y + gPanels[i].Height) and
                     (gPanels[n].X = gPanels[i].X) and
                     (gPanels[n].Width = gPanels[i].Width) then
                  begin
                    gPanels[i].Height := gPanels[i].Height+gPanels[n].Height;
                    RemoveObject(n, OBJECT_PANEL);
                    a := True;
                    Inc(Result);
                    Continue;
                  end;
                end;
              end;
            end;
        end;
    end;
  end;

var
  count: Integer;
  panelcount1, panelcount2: Integer;
  a: Integer;

begin
  mOptimizationResult.Clear();

  if gPanels = nil then
    Exit;

  panelcount1 := 0;
  for a := 0 to High(gPanels) do
    if gPanels[a].PanelType <> 0 then
      panelcount1 := panelcount1 + 1;

  if panelcount1 = 0 then
    Exit;

  if cbOptimizeWalls.Checked then
  begin
    mOptimizationResult.Lines.Add(_lc[I_OPT_WALLS]);
    count := OptimizePanels(PANEL_WALL);
    mOptimizationResult.Lines.Add(_lc[I_OPT_PANELS_OPT]+' '+IntToStr(count)+#13#10);
  end;

  if cbOptimizeForeGround.Checked then
  begin
    mOptimizationResult.Lines.Add(_lc[I_OPT_FORES]);
    count := OptimizePanels(PANEL_FORE);
    mOptimizationResult.Lines.Add(_lc[I_OPT_PANELS_OPT]+' '+IntToStr(count)+#13#10);
  end;

  if cbOptimizeBackGround.Checked then
  begin
    mOptimizationResult.Lines.Add(_lc[I_OPT_BACKS]);
    count := OptimizePanels(PANEL_BACK);
    mOptimizationResult.Lines.Add(_lc[I_OPT_PANELS_OPT]+' '+IntToStr(count)+#13#10);
  end;

  if cbOptimizeSteps.Checked then
  begin
    mOptimizationResult.Lines.Add(_lc[I_OPT_STAIRS]);
    count := OptimizePanels(PANEL_STEP);
    mOptimizationResult.Lines.Add(_lc[I_OPT_PANELS_OPT]+' '+IntToStr(count)+#13#10);
  end;

  if cbOptimizeWater.Checked then
  begin
    mOptimizationResult.Lines.Add(_lc[I_OPT_WATER]);
    count := OptimizePanels(PANEL_WATER);
    mOptimizationResult.Lines.Add(_lc[I_OPT_PANELS_OPT]+' '+IntToStr(count)+#13#10);
  end;

  if cbOptimizeAcid1.Checked then
  begin
    mOptimizationResult.Lines.Add(_lc[I_OPT_ACID1]);
    count := OptimizePanels(PANEL_ACID1);
    mOptimizationResult.Lines.Add(_lc[I_OPT_PANELS_OPT]+' '+IntToStr(count)+#13#10);
  end;

  if cbOptimizeAcid2.Checked then
  begin
    mOptimizationResult.Lines.Add(_lc[I_OPT_ACID2]);
    count := OptimizePanels(PANEL_ACID2);
    mOptimizationResult.Lines.Add(_lc[I_OPT_PANELS_OPT]+' '+IntToStr(count)+#13#10);
  end;

  if cbOptimizeLift.Checked then
  begin
    mOptimizationResult.Lines.Add(_lc[I_OPT_LIFTS]);
    count := OptimizePanels(PANEL_LIFTUP)+OptimizePanels(PANEL_LIFTDOWN);
    mOptimizationResult.Lines.Add(_lc[I_OPT_PANELS_OPT]+' '+IntToStr(count)+#13#10);
  end;

  if cbOptimizeBlockMon.Checked then
  begin
    mOptimizationResult.Lines.Add(_lc[I_OPT_BLOCKMON]);
    count := OptimizePanels(PANEL_BLOCKMON);
    mOptimizationResult.Lines.Add(_lc[I_OPT_PANELS_OPT]+' '+IntToStr(count)+#13#10);
  end;

  panelcount2 := 0;
  for a := 0 to High(gPanels) do
    if gPanels[a].PanelType <> 0 then
      panelcount2 := panelcount2 + 1;

  mOptimizationResult.Lines.Add('-----------------------');
  mOptimizationResult.Lines.Add(_lc[I_OPT_TOTAL_PANELS]+' '+IntToStr(panelcount1));
  mOptimizationResult.Lines.Add(_lc[I_OPT_PANELS_AFTER]+' '+IntToStr(panelcount2));
  mOptimizationResult.Lines.Add(_lc[I_OPT_PANELS_OPT]+' '+IntToStr(panelcount1-panelcount2));
end;

end.
