unit f_addresource_sky;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, f_addresource,
  ExtCtrls, StdCtrls, utils, Imaging, ImagingTypes, ImagingUtility;

type
  TAddSkyForm = class (TAddResourceForm)
  var
    PanelTexPreview: TPanel;
    iPreview: TImage;

    procedure bOKClick(Sender: TObject);
    procedure lbResourcesListClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

  private
    FSetResource: String;
    
  public
    property SetResource: String read FSetResource write FSetResource;
  end;

var
  AddSkyForm: TAddSkyForm;

implementation

uses
  BinEditor, WADEDITOR, f_main, g_language;

{$R *.lfm}

function ShowTGATexture(ResourceStr: String): TBitMap;
var
  img:        TImageData;
  clr:        TColor32Rec;
  bgc:        TColor32Rec;
  Width,
  Height:     Integer;
  x, y:       Integer;
  BitMap:     TBitMap;

  TextureData:  Pointer;
  ImageSize:    Integer;
  WAD:          TWADEditor_1;
  WADName:      String;
  SectionName:  String;
  ResourceName: String;

begin
  Result := nil;

// Загружаем ресурс текстуры из WAD:
  g_ProcessResourceStr(ResourceStr, WADName, SectionName, ResourceName);

  WAD := TWADEditor_1.Create();
  WAD.ReadFile(WADName);

  WAD.GetResource(utf2win(SectionName), utf2win(ResourceName), TextureData, ImageSize);

  WAD.Free();

  (* !!! copypaste from f_addresource_texture.CreateBitMap *)

  InitImage(img);
  if not LoadImageFromMemory(TextureData, ImageSize, img) then
  begin
    FreeMem(TextureData);
    Exit;
  end;

  Width  := img.width;
  Height := img.height;
  BitMap := TBitMap.Create();
  BitMap.PixelFormat := pf24bit;
  BitMap.Width := Width;
  BitMap.Height := Height;
  for y := 0 to height - 1 do
  begin
    for x := 0 to width - 1 do
    begin
      clr := GetPixel32(img, x, y);
      // HACK: Lazarus's TBitMap doesn't seem to have a working 32 bit mode, so
      //       mix color with checkered background. Also, can't really read
      //       CHECKERS.tga from here. FUCK!
      if UseCheckerboard then
      begin
        if (((x shr 3) and 1) = 0) xor (((y shr 3) and 1) = 0) then
          bgc.Color := $FDFDFD
        else
          bgc.Color := $CBCBCB
      end
      else
      begin
        bgc.r := GetRValue(PreviewColor);
        bgc.g := GetGValue(PreviewColor);
        bgc.b := GetBValue(PreviewColor)
      end;
      clr.r := ClampToByte((Byte(255 - clr.a) * bgc.r + clr.a * clr.r) div 255);
      clr.g := ClampToByte((Byte(255 - clr.a) * bgc.g + clr.a * clr.g) div 255);
      clr.b := ClampToByte((Byte(255 - clr.a) * bgc.b + clr.a * clr.b) div 255);
      BitMap.Canvas.Pixels[x, y] := RGBToColor(clr.r, clr.g, clr.b)
    end
  end;
  FreeMem(TextureData);
  FreeImage(img);
  Result := BitMap;
end;

procedure TAddSkyForm.bOKClick(Sender: TObject);
begin
  Inherited;

  ModalResult := mrOk;
end;

procedure TAddSkyForm.lbResourcesListClick(Sender: TObject);
var
  Texture: TBitMap;

begin
  Inherited;

  if lbResourcesList.ItemIndex = -1 then
    Exit;
  if FResourceName = '' then
    Exit;

  Texture := ShowTGATexture(FFullResourceName);
  iPreview.Canvas.FillRect(iPreview.Canvas.ClipRect);
  if Texture = nil then
    Exit;
  iPreview.Canvas.StretchDraw(iPreview.Canvas.ClipRect, Texture);
  Texture.Free();
end;

procedure TAddSkyForm.FormActivate(Sender: TObject);
var
  FileName,
  SectionName,
  ResourceName: String;
  a: Integer;

begin
  Inherited;

  iPreview.Canvas.FillRect(iPreview.Canvas.ClipRect);

// Уже есть выбранный ресурс:
  if FSetResource <> '' then
  begin
    g_ProcessResourceStr(FSetResource, FileName, SectionName, ResourceName);

    if FileName = '' then
      FileName := MsgWadSpecialMap;
    if SectionName = '' then
      SectionName := '..';

  // WAD файл:
    a := cbWADList.Items.IndexOf(FileName);
    if a <> -1 then
    begin
      cbWADList.ItemIndex := a;
      cbWADList.OnChange(nil);
    end;

  // Секция:
    a := cbSectionsList.Items.IndexOf(SectionName);
    if a <> -1 then
    begin
      cbSectionsList.ItemIndex := a;
      cbSectionsList.OnChange(nil);
    end;

  // Ресурс:
    a := lbResourcesList.Items.IndexOf(ResourceName);
    if a <> -1 then
    begin
      lbResourcesList.ItemIndex := a;
      lbResourcesList.OnClick(nil);
    end;
  end;
end;

initialization
  AddSkyForm := TAddSkyForm.Create(Application);
end.
