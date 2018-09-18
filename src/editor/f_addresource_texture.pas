unit f_addresource_texture;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, f_addresource,
  StdCtrls, ExtCtrls, utils, Imaging, ImagingTypes, ImagingUtility;

type

  { TAddTextureForm }

  TAddTextureForm = class (TAddResourceForm)
    lStats: TLabel;
    PanelTexPreview: TPanel;
    iPreview: TImage;
    eTextureName: TEdit;
    bAddTexture: TButton;
    bClose: TButton;
    bAddClose: TButton;

    procedure FormActivate(Sender: TObject);
    procedure lbResourcesListClick(Sender: TObject);
    procedure eTextureNameChange(Sender: TObject);
    procedure cbWADListChange(Sender: TObject);
    procedure cbSectionsListChange(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure bAddTextureClick(Sender: TObject);
    procedure bAddCloseClick(Sender: TObject);

  private
    {}
  public
    {}
  end;

var
  AddTextureForm: TAddTextureForm;
  NumFrames: Integer = 0;

function IsAnim(Res: String): Boolean;
function GetFrame(Res: String; var Data: Pointer; var DataLen: Integer;
                  var Width, Height: Word): Boolean;

implementation

uses
  BinEditor, WADEDITOR, WADSTRUCT, f_main, g_textures, CONFIG, g_map,
  g_language, e_Log, g_resources;

{$R *.lfm}

function IsAnim(Res: String): Boolean;
  var
    data: Pointer;
    len: Integer;
    WADName, SectionName, ResourceName: String;
begin
  g_ProcessResourceStr(Res, WADName, SectionName, ResourceName);
  (* just check file existance *)
  g_ReadSubResource(WADName, SectionName, ResourceName, 'TEXT', 'ANIM', data, len);
  (* TODO check section TEXTURES *)
  Result := data <> nil;
  if data <> nil then
    FreeMem(data)
end;

function GetFrame (Res: String; var Data: Pointer; var DataLen: Integer; var Width, Height: Word): Boolean;
  var
    Len: Integer;
    TextData: Pointer;
    WADName, SectionName, ResourceName: String;
    config: TConfig;
begin
  Result := False; Data := nil; DataLen := 0; Width := 0; Height := 0;
  g_ProcessResourceStr(Res, WADName, SectionName, ResourceName);
  g_ReadSubResource(WADName, SectionName, ResourceName, 'TEXT', 'ANIM', TextData, Len);
  if TextData <> nil then
  begin
    config := TConfig.CreateMem(TextData, Len);
    g_ReadSubResource(WADName, SectionName, ResourceName, 'TEXTURES', config.ReadStr('', 'resource', ''), Data, DataLen);
    if Data <> nil then
    begin
      Height := config.ReadInt('', 'frameheight', 0);
      Width := config.ReadInt('', 'framewidth', 0);
      Result := True
    end;
    config.Free();
    FreeMem(TextData)
  end
end;

function CreateBitMap(Data: Pointer; DataSize: Cardinal): TBitMap;
var
  img:        TImageData;
  clr:        TColor32Rec;
  bgc:        TColor32Rec;
  ii:         PByte;
  Width,
  Height:     Integer;
  x, y:       Integer;
  BitMap:     TBitMap;

begin
  Result := nil;

  InitImage(img);
  if not LoadImageFromMemory(Data, DataSize, img) then
    Exit;

  Width  := img.width;
  Height := img.height;

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
      // HACK: Lazarus's TBitMap doesn't seem to have a working 32 bit mode, so
      //       mix color with checkered background. Also, can't really read
      //       CHECKERS.tga from here. FUCK!
      if UseCheckerboard then
        begin
          if (((x shr 3) and 1) = 0) xor (((y shr 3) and 1) = 0) then
            bgc.Color := $FDFDFD
          else
            bgc.Color := $CBCBCB;
        end
      else
        begin
          bgc.r := GetRValue(PreviewColor);
          bgc.g := GetGValue(PreviewColor);
          bgc.b := GetBValue(PreviewColor);
        end;
      clr.r := ClampToByte((Byte(255 - clr.a) * bgc.r + clr.a * clr.r) div 255);
      clr.g := ClampToByte((Byte(255 - clr.a) * bgc.g + clr.a * clr.g) div 255);
      clr.b := ClampToByte((Byte(255 - clr.a) * bgc.b + clr.a * clr.b) div 255);
      // TODO: check for RGB/BGR somehow?
      ii^ := clr.b; Inc(ii);
      ii^ := clr.g; Inc(ii);
      ii^ := clr.r; Inc(ii);

      (* Why this works in linux? *)
      {$IFNDEF WINDOWS}Inc(ii){$ENDIF}
    end;
  end;
  FreeImage(img);
  Result := BitMap;
end;

function ShowAnim(Res: String): TBitMap;
  var
    Len: Integer;
    TextData, TextureData: Pointer;
    WADName, SectionName, ResourceName: String;
    config: TConfig;
begin
  Result := nil;
  g_ProcessResourceStr(Res, WADName, SectionName, ResourceName);
  g_ReadSubResource(WADName, SectionName, ResourceName, 'TEXT', 'ANIM', TextData, Len);
  if TextData <> nil then
  begin
    config := TConfig.CreateMem(TextData, Len);
    g_ReadSubResource(WADName, SectionName, ResourceName, 'TEXTURES', config.ReadStr('', 'resource', ''), TextureData, Len);
    if TextureData <> nil then
    begin
      Result := CreateBitMap(TextureData, Len);
      (* view only first frame *)
      NumFrames := config.ReadInt('', 'framecount', 0);
      Result.Height := config.ReadInt('', 'frameheight', 0);
      Result.Width := config.ReadInt('', 'framewidth', 0);
      FreeMem(TextureData)
    end;
    config.Free();
    FreeMem(TextData)
  end
end;

function ShowTGATexture(ResourceStr: String): TBitMap;
  var
    Len: Integer;
    TextureData: Pointer;
    WADName, SectionName, ResourceName: String;
begin
  Result := nil;
  g_ProcessResourceStr(ResourceStr, WADName, SectionName, ResourceName);
  g_ReadResource(WADName, SectionName, ResourceName, TextureData, Len);
  if TextureData <> nil then
    Result := CreateBitMap(TextureData, Len)
end;

procedure TAddTextureForm.FormActivate(Sender: TObject);
begin
  Inherited;

  lStats.Caption := '';
  cbWADList.Items.Add(_lc[I_WAD_SPECIAL_TEXS]);

  eTextureName.Text := '';
  iPreview.Canvas.FillRect(iPreview.Canvas.ClipRect);

  bOK.Visible := False;
  bCancel.Visible := False;
end;

procedure TAddTextureForm.lbResourcesListClick(Sender: TObject);
var
  Texture: TBitMap;
  wad: String;
  Anim: Boolean;

begin
  Inherited;

  lStats.Caption := '';
  if lbResourcesList.ItemIndex = -1 then
    Exit;
  if FResourceName = '' then
    Exit;
  if cbWADList.Text = _lc[I_WAD_SPECIAL_TEXS] then
    Exit;

  g_ProcessResourceStr(FFullResourceName, @wad, nil, nil);
  if wad = _lc[I_WAD_SPECIAL_TEXS] then
    Exit;

  Anim := IsAnim(FFullResourceName);
  if Anim then
    Texture := ShowAnim(FFullResourceName)
  else
    Texture := ShowTGATexture(FFullResourceName);

  if Texture = nil then
    Exit;

  if Anim then
    lStats.Caption := Format(_lc[I_CAP_ANIMATION], [Texture.Width, Texture.Height, NumFrames])
  else
    lStats.Caption := Format(_lc[I_CAP_TEXTURE], [Texture.Width, Texture.Height]);

  iPreview.Canvas.FillRect(iPreview.Canvas.ClipRect);
  iPreview.Canvas.CopyRect(Texture.Canvas.ClipRect, Texture.Canvas, Texture.Canvas.ClipRect);
  Texture.Free();
end;

procedure TAddTextureForm.eTextureNameChange(Sender: TObject);
var
  a: Integer;
  first: Boolean;

begin
// Убираем старые выделения:
  for a := 0 to lbResourcesList.Items.Count-1 do
    lbResourcesList.Selected[a] := False;

// Нечего искать:
  if (lbResourcesList.Items.Count = 0) or
     (eTextureName.Text = '') then
    Exit;

  first := True;

  for a := 0 to lbResourcesList.Items.Count-1 do
    if LowerCase(Copy(lbResourcesList.Items[a], 1,
                 Length(eTextureName.Text))) =
       LowerCase(eTextureName.Text) then
    begin
      lbResourcesList.Selected[a] := True;

      if first then
      begin
      // Показываем первую текстуру из найденных:
        lbResourcesList.TopIndex := a;
        lbResourcesList.OnClick(nil);

        first := False;
      end;
    end;
end;

procedure TAddTextureForm.cbWADListChange(Sender: TObject);
begin
  if cbWADList.Text = _lc[I_WAD_SPECIAL_TEXS] then
  begin
    cbSectionsList.Clear();
    cbSectionsList.Items.Add('..');
    Exit;
  end;

  Inherited;
end;

procedure TAddTextureForm.cbSectionsListChange(Sender: TObject);
begin
  if cbWADList.Text = _lc[I_WAD_SPECIAL_TEXS] then
  begin
    lbResourcesList.Clear();
    lbResourcesList.Items.Add(TEXTURE_NAME_WATER);
    lbResourcesList.Items.Add(TEXTURE_NAME_ACID1);
    lbResourcesList.Items.Add(TEXTURE_NAME_ACID2);
    Exit;
  end;

  Inherited;
end;

procedure TAddTextureForm.bCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TAddTextureForm.bAddTextureClick(Sender: TObject);
var
  i: Integer;

begin
  for i := 0 to lbResourcesList.Count-1 do
    if lbResourcesList.Selected[i] then
    begin
      AddTexture(cbWADlist.Text, cbSectionsList.Text,
                 lbResourcesList.Items[i], False);
      lbResourcesList.Selected[i] := False;
    end;
end;

procedure TAddTextureForm.bAddCloseClick(Sender: TObject);
begin
  bAddTextureClick(bAddTexture);
  Close();
end;

end.
