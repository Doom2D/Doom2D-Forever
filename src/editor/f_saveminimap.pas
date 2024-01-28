unit f_saveminimap;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TSaveMiniMapForm = class(TForm)
    SaveDialog: TSaveDialog;
    pbMiniMap: TPaintBox;
    Panel1: TPanel;
    LabelScale: TLabel;
    cbScale: TComboBox;
    bSave: TButton;
    bClose: TButton;

    procedure bCloseClick(Sender: TObject);
    procedure cbScaleChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure pbMiniMapPaint(Sender: TObject);
    procedure bSaveClick(Sender: TObject);

  private
    procedure DrawMiniMap(canv: TCanvas);
  public
    { Public declarations }
  end;

var
  SaveMiniMapForm: TSaveMiniMapForm;

implementation

uses
  e_graphics, g_map, MAPDEF, Math, f_main;

{$R *.lfm}

procedure TSaveMiniMapForm.DrawMiniMap(canv: TCanvas);
var
  a, w, h, Scale, aX, aY, aX2, aY2: Integer;
  c: TRGB;
  
begin
  if cbScale.ItemIndex = 0 then
    Scale := 1
  else
    Scale := 2;

  w := (gMapInfo.Width div (16 div Scale))+4;
  h := (gMapInfo.Height div (16 div Scale))+4;

  canv.Brush.Color := $00000000;
  canv.FillRect(canv.ClipRect);

  canv.Pen.Color := $00FFFFFF;
  canv.Rectangle(0, 0, w, h);

  if gPanels = nil then
    Exit;

  for a := 0 to High(gPanels) do
    with gPanels[a] do
      if PanelType <> 0 then
      begin
        aX := 2+(X div (16 div Scale));
        aY := 2+(Y div (16 div Scale));

        if Width div (16 div Scale) = 0 then
          aX2 := aX+1
        else
          aX2 := aX+Width div (16 div Scale);

        if Height div (16 div Scale) = 0 then
          aY2 := aY+1
        else
          aY2 := aY+Height div (16 div Scale);

        case PanelType of
          PANEL_WALL: c := _RGB(208, 208, 208);
          PANEL_CLOSEDOOR: c := _RGB(255, 255, 0);
          PANEL_WATER: c := _RGB(0, 0, 252);
          PANEL_ACID1: c := _RGB(200, 80, 4);
          PANEL_ACID2: c := _RGB(252, 140, 56);
          PANEL_STEP: c := _RGB(128, 128, 128);
          else Continue;
        end;

        canv.Brush.Color := c.R+c.G*256+c.B*256*256;
        canv.FillRect(Rect(aX, aY, aX2, aY2));
      end;
end;

procedure TSaveMiniMapForm.bCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TSaveMiniMapForm.cbScaleChange(Sender: TObject);
begin
  DrawMiniMap(pbMiniMap.Canvas)
end;

procedure TSaveMiniMapForm.FormActivate(Sender: TObject);
begin
  DrawMiniMap(pbMiniMap.Canvas);
end;

procedure TSaveMiniMapForm.pbMiniMapPaint(Sender: TObject);
begin
  DrawMiniMap(pbMiniMap.Canvas);
end;

procedure TSaveMiniMapForm.bSaveClick(Sender: TObject);
var
  bmp: TBitmap;
  Scale, w, h: Integer;

begin
  SaveDialog.FileName := ChangeFileExt(ExtractFileName(gMapInfo.FileName), '')
  + '_' + LowerCase(gMapInfo.MapName);

  // Filter out special characters
  SaveDialog.FileName := StringReplace(SaveDialog.FileName, '/', '', [rfReplaceAll]);
  SaveDialog.FileName := StringReplace(SaveDialog.FileName, '\', '', [rfReplaceAll]);
  SaveDialog.FileName := StringReplace(SaveDialog.FileName, ':', '', [rfReplaceAll]);
  SaveDialog.FileName := StringReplace(SaveDialog.FileName, '*', '', [rfReplaceAll]);
  SaveDialog.FileName := StringReplace(SaveDialog.FileName, '?', '', [rfReplaceAll]);
  SaveDialog.FileName := StringReplace(SaveDialog.FileName, '"', '', [rfReplaceAll]);
  SaveDialog.FileName := StringReplace(SaveDialog.FileName, '<', '', [rfReplaceAll]);
  SaveDialog.FileName := StringReplace(SaveDialog.FileName, '>', '', [rfReplaceAll]);
  SaveDialog.FileName := StringReplace(SaveDialog.FileName, '|', '', [rfReplaceAll]);

  if SaveDialog.FileName = '_' then
    SaveDialog.FileName := '';

  if SaveDialog.Execute() then
  begin
    bmp := TBitmap.Create();

    if cbScale.ItemIndex = 0 then
      Scale := 1
    else
      Scale := 2;

    w := (gMapInfo.Width div (16 div Scale))+4;
    h := (gMapInfo.Height div (16 div Scale))+4;

    bmp.Width := w;
    bmp.Height := h;

    DrawMiniMap(bmp.Canvas);

    bmp.SaveToFile(SaveDialog.FileName);

    bmp.Free();
  end;
end;

initialization
  SaveMiniMapForm := TSaveMiniMapForm.Create(Application);
end.
