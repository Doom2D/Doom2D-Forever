unit f_addresource_texture;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, f_addresource,
  StdCtrls, ExtCtrls;

type
  TAddTextureForm = class (TAddResourceForm)
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

function IsAnim(Res: String): Boolean;
function GetFrame(Res: String; var Data: Pointer;
                  var Width, Height: Word): Boolean;

implementation

uses
  WADEDITOR, f_main, g_textures, WADSTRUCT, CONFIG, g_map,
  g_language;

type
  TTGAHeader = packed record
    FileType:     Byte;
    ColorMapType: Byte;
    ImageType:    Byte;
    ColorMapSpec: Array [0..4] of Byte;
    OrigX:        Array [0..1] of Byte;
    OrigY:        Array [0..1] of Byte;
    Width:        Array [0..1] of Byte;
    Height:       Array [0..1] of Byte;
    BPP:          Byte;
    ImageInfo:    Byte;
  end;

{$R *.dfm}

function IsAnim(Res: String): Boolean;
var
  WAD:          TWADEditor_1;
  WADName:      String;
  SectionName:  String;
  ResourceName: String;
  Data:         Pointer;
  Size:         Integer;
  Sign:         Array [0..4] of Char;
  Sections,
  Resources:    SArray;
  a:            Integer;
  ok:           Boolean;

begin
  Result := False;

// Читаем файл и ресурс в нем:
  g_ProcessResourceStr(Res, WADName, SectionName, ResourceName);

  WAD := TWADEditor_1.Create();

  if (not WAD.ReadFile(WADName)) or
     (not WAD.GetResource(SectionName, ResourceName, Data, Size)) then
  begin
    WAD.Free();
    Exit;
  end;

  WAD.FreeWAD();

// Проверка сигнатуры. Если есть - это WAD внутри WAD:
  CopyMemory(@Sign[0], Data, 5);

  if not (Sign = DFWAD_SIGNATURE) then
  begin
    WAD.Free();
    FreeMem(Data);
    Exit;
  end;

// Пробуем прочитать данные:
  if not WAD.ReadMemory(Data, Size) then
  begin
    WAD.Free();
    FreeMem(Data);
    Exit;
  end;

  FreeMem(Data);

// Читаем секции:
  Sections := WAD.GetSectionList();

  if Sections = nil then
  begin
    WAD.Free();
    Exit;
  end;

// Ищем в секциях "TEXT":
  ok := False;
  for a := 0 to High(Sections) do
    if Sections[a] = 'TEXT' then
    begin
      ok := True;
      Break;
    end;

// Ищем в секциях лист текстур - "TEXTURES":
  for a := 0 to High(Sections) do
    if Sections[a] = 'TEXTURES' then
    begin
      ok := ok and True;
      Break;
    end;

  if not ok then
  begin
    WAD.Free();
    Exit;
  end;

// Получаем ресурсы секции "TEXT":
  Resources := WAD.GetResourcesList('TEXT');

  if Resources = nil then
  begin
    WAD.Free();
    Exit;
  end;

// Ищем в них описание анимации - "AINM": 
  ok := False;
  for a := 0 to High(Resources) do
    if Resources[a] = 'ANIM' then
    begin
      ok := True;
      Break;
    end;

  WAD.Free();

// Если все получилось, то это аним. текстура:
  Result := ok;
end;

function GetFrame(Res: String; var Data: Pointer;
                  var Width, Height: Word): Boolean;
var
  AnimWAD:      Pointer;
  WAD:          TWADEditor_1;
  WADName:      String;
  SectionName:  String;
  ResourceName: String;
  Len:          Integer;
  config:       TConfig;
  TextData:     Pointer;

begin
  Result := False;

// Читаем WAD:
  g_ProcessResourceStr(Res, WADName, SectionName, ResourceName);

  WAD := TWADEditor_1.Create();

  if not WAD.ReadFile(WADName) then
  begin
    WAD.Free();
    Exit;
  end;

// Читаем WAD-ресурс из WAD:
  if not WAD.GetResource(SectionName, ResourceName, AnimWAD, Len) then
  begin
    WAD.Free();
    Exit;
  end;
 
  WAD.FreeWAD();

// Читаем WAD в WAD'е:
  if not WAD.ReadMemory(AnimWAD, Len) then
  begin
    FreeMem(AnimWAD);
    WAD.Free();
    Exit;
  end;

// Читаем описание анимации:
  if not WAD.GetResource('TEXT', 'ANIM', TextData, Len) then
  begin
    FreeMem(TextData);
    FreeMem(AnimWAD);
    WAD.Free();
    Exit;
  end;

  config := TConfig.CreateMem(TextData, Len);

// Читаем ресурс - лист текстур:
  if not WAD.GetResource('TEXTURES', config.ReadStr('', 'resource', ''), Data, Len) then
  begin
    FreeMem(TextData);
    FreeMem(AnimWAD);
    WAD.Free();
    Exit;
  end;

  Height := config.ReadInt('', 'frameheight', 0);
  Width := config.ReadInt('', 'framewidth', 0);

  config.Free();
  WAD.Free();

  FreeMem(TextData);
  FreeMem(AnimWAD);

  Result := True;
end;

function CreateBitMap(Data: Pointer): TBitMap;
var
  TGAHeader:  TTGAHeader;
  image:      Pointer;
  Width,
  Height:     Integer;
  ColorDepth: Integer;
  ImageSize:  Integer;
  i:          Integer;
  BitMap:     TBitMap;

begin
  Result := nil;

// Читаем заголовок TGA:
  CopyMemory(@TGAHeader, Data, SizeOf(TGAHeader));

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

  CopyMemory(Image, Pointer(Integer(Data)+SizeOf(TGAHeader)), ImageSize);

  BitMap := TBitMap.Create();

  if TGAHeader.BPP = 24 then
    BitMap.PixelFormat := pf24bit
  else
    BitMap.PixelFormat := pf32bit;
  
  BitMap.Width := Width;
  BitMap.Height := Height;

// Копируем в BitMap:
  for I := Height-1 downto 0 do
    CopyMemory(BitMap.ScanLine[Height-1-I],
               Pointer(Integer(Image)+(Width*I*(TGAHeader.BPP div 8))),
               Width*(TGAHeader.BPP div 8));

  FreeMem(Image, ImageSize);

  Result := BitMap;
end;

function ShowAnim(Res: String): TBitMap;
var
  AnimWAD:      Pointer;
  WAD:          TWADEditor_1;
  WADName:      String;
  SectionName:  String;
  ResourceName: String;
  Len:          Integer;
  config:       TConfig;
  TextData:     Pointer;
  TextureData:  Pointer;
  
begin
  Result := nil;

// Читаем WAD файл и ресурс в нем:
  g_ProcessResourceStr(Res, WADName, SectionName, ResourceName);

  WAD := TWADEditor_1.Create();
  WAD.ReadFile(WADName);
  WAD.GetResource(SectionName, ResourceName, AnimWAD, Len);
  WAD.FreeWAD();

// Читаем описание анимации:
  WAD.ReadMemory(AnimWAD, Len);
  WAD.GetResource('TEXT', 'ANIM', TextData, Len);

  config := TConfig.CreateMem(TextData, Len);

// Читаем лист текстур:
  WAD.GetResource('TEXTURES', config.ReadStr('', 'resource', ''), TextureData, Len);

  if (TextureData <> nil) and
     (WAD.GetLastError = DFWAD_NOERROR) then
  begin
  // Создаем BitMap из листа текстур:
    Result := CreateBitMap(TextureData);
    
  // Размеры одного кадра - виден только первый кадр:
    Result.Height := config.ReadInt('', 'frameheight', 0);
    Result.Width := config.ReadInt('', 'framewidth', 0);
  end;
 
  config.Free();
  WAD.Free();

  FreeMem(TextureData);
  FreeMem(TextData);
  FreeMem(AnimWAD);
end;

function ShowTGATexture(ResourceStr: String): TBitMap;
var
  TextureData:  Pointer;
  WAD:          TWADEditor_1;
  WADName:      String;
  SectionName:  String;
  ResourceName: String;
  Len:          Integer;

begin
  Result := nil;

// Читаем WAD:
  g_ProcessResourceStr(ResourceStr, WADName, SectionName, ResourceName);

  WAD := TWADEditor_1.Create();
  if not WAD.ReadFile(WADName) then
  begin
    WAD.Free();
    Exit;
  end;

// Читаем ресурс текстуры в нем:
  WAD.GetResource(SectionName, ResourceName, TextureData, Len);

  WAD.Free();

// Создаем на его основе BitMap:
  Result := CreateBitMap(TextureData);

  FreeMem(TextureData, Len);
end;

procedure TAddTextureForm.FormActivate(Sender: TObject);
begin
  Inherited;

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

begin
  Inherited;

  if lbResourcesList.ItemIndex = -1 then
    Exit;
  if FResourceName = '' then
    Exit;
  if cbWADList.Text = _lc[I_WAD_SPECIAL_TEXS] then
    Exit;

  g_ProcessResourceStr(FFullResourceName, @wad, nil, nil);
  if wad = _lc[I_WAD_SPECIAL_TEXS] then
    Exit;

  if IsAnim(FFullResourceName) then
    Texture := ShowAnim(FFullResourceName)
  else
    Texture := ShowTGATexture(FFullResourceName);

  if Texture = nil then
    Exit;
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
        lbResourcesList.Perform(LB_SETTOPINDEX, a, 0);
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
