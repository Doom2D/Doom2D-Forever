unit f_addresource_sky;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, f_addresource,
  ExtCtrls, StdCtrls, utils, Imaging, ImagingTypes, ImagingUtility;

type
  TAddSkyForm = class (TAddResourceForm)
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
  ii:         PByte;
  Width,
  Height:     Integer;
  ColorDepth: Integer;
  ImageSize:  Integer;
  I, x, y:    Integer;
  BitMap:     TBitMap;

  TextureData:  Pointer;
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

  WAD.GetResource(SectionName, ResourceName, TextureData, ImageSize);

  WAD.Free();

  InitImage(img);
  if not LoadImageFromMemory(TextureData, ImageSize, img) then
    Exit;

  Width  := img.width;
  Height := img.height;
  ColorDepth := 24;
  ImageSize  := Width*Height*(ColorDepth div 8);

  BitMap := TBitMap.Create();
  BitMap.PixelFormat := pf24bit;

  BitMap.Width := Width;
  BitMap.Height := Height;

// Копируем в BitMap:
  ii := BitMap.RawImage.Data;
  for y := 0 to height-1 do
  begin
    for x := 0 to width-1 do
    begin
      clr := GetPixel32(img, x, y);
      // assuming sky has no alpha
      // TODO: check for ARGB/RGBA/BGRA/ABGR somehow?
      ii^ := clr.b; Inc(ii);
      ii^ := clr.g; Inc(ii);
      ii^ := clr.r; Inc(ii);
    end;
  end;

  FreeMem(TextureData);
  FreeImage(img);
  Result := BitMap;
end;

procedure TAddSkyForm.bOKClick(Sender: TObject);
begin
  Inherited;

  if not FResourceSelected then
    Exit;
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
      FileName := _lc[I_WAD_SPECIAL_MAP];
    if SectionName = '' then
      SectionName := '..';

  // WAD файл:
    a := cbWADList.Items.IndexOf(win2utf(FileName));
    if a <> -1 then
    begin
      cbWADList.ItemIndex := a;
      cbWADList.OnChange(nil);
    end;

  // Секция:
    a := cbSectionsList.Items.IndexOf(win2utf(SectionName));
    if a <> -1 then
    begin
      cbSectionsList.ItemIndex := a;
      cbSectionsList.OnChange(nil);
    end;

  // Ресурс:
    a := lbResourcesList.Items.IndexOf(win2utf(ResourceName));
    if a <> -1 then
    begin
      lbResourcesList.ItemIndex := a;
      lbResourcesList.OnClick(nil);
    end;
  end;
end;

end.
