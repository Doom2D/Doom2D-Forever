unit f_addresource_sky;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, f_addresource,
  ExtCtrls, StdCtrls;

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

procedure SwapRGB(data: Pointer; Size: Integer);
asm
  mov ebx, eax
  mov ecx, size

@@loop :
  mov al,[ebx+0]
  mov ah,[ebx+2]
  mov [ebx+2],al
  mov [ebx+0],ah
  add ebx,3
  dec ecx
  jnz @@loop
end;

function ShowTGATexture(ResourceStr: String): TBitMap;
var
  TGAHeader: packed record // Header type for TGA images
    FileType:     Byte;
    ColorMapType: Byte;
    ImageType:    Byte;
    ColorMapSpec: Array[0..4] of Byte;
    OrigX:        Array [0..1] of Byte;
    OrigY:        Array [0..1] of Byte;
    Width:        Array [0..1] of Byte;
    Height:       Array [0..1] of Byte;
    BPP:          Byte;
    ImageInfo:    Byte;
  end;
  image:      Pointer;    {or PRGBTRIPLE}
  Width,
  Height:     Integer;
  ColorDepth: Integer;
  ImageSize:  Integer;
  I:          Integer;
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

// Заголовок TGA:
  CopyMemory(@TGAHeader, TextureData, SizeOf(TGAHeader));

  if TGAHeader.ImageType <> 2 then
    Exit;
  if TGAHeader.ColorMapType <> 0 then
    Exit;
  if TGAHeader.BPP < 24 then
    Exit;

  Width  := TGAHeader.Width[0]+TGAHeader.Width[1]*256;
  Height := TGAHeader.Height[0]+TGAHeader.Height[1]*256;
  ColorDepth := TGAHeader.BPP;
  ImageSize  := Width*Height*(ColorDepth div 8);

// Само изображение:
  GetMem(Image, ImageSize);

  CopyMemory(Image, Pointer(Integer(TextureData)+SizeOf(TGAHeader)), ImageSize);

  BitMap := TBitMap.Create();

  if TGAHeader.BPP = 24 then
    BitMap.PixelFormat := pf24bit
  else
    BitMap.PixelFormat := pf32bit;
  
  BitMap.Width := Width;
  BitMap.Height := Height;

// Копируем изображение в BitMap:
  for I := Height-1 downto 0 do
    CopyMemory(BitMap.ScanLine[Height-1-I],
               Pointer(Integer(Image)+(Width*I*(TGAHeader.BPP div 8))),
               Width*(TGAHeader.BPP div 8));

  FreeMem(Image, ImageSize);
  FreeMem(TextureData);
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

end.
