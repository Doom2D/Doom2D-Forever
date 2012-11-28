unit f_addresource_texture;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, f_addresource, StdCtrls, ExtCtrls;

type
  TAddTextureForm = class(TAddResourceForm)
   Panel1: TPanel;
   iPreview: TImage;
   eTextureName: TEdit;
   procedure bOKClick(Sender: TObject);
   procedure FormActivate(Sender: TObject);
   procedure lbResourcesListClick(Sender: TObject);
   procedure eTextureNameChange(Sender: TObject);
   procedure cbWADListChange(Sender: TObject);
   procedure cbSectionsListChange(Sender: TObject);
  private
  public
  end;

var
  AddTextureForm: TAddTextureForm;

function IsAnim(Res: string): Boolean;
function GetFrame(Res: string; var Data: Pointer; var Width, Height: Word): Boolean;

implementation

uses
  WADEDITOR, f_main, g_textures, WADSTRUCT, CONFIG, g_map;

type
  TTGAHeader = packed record
   FileType:     Byte;
   ColorMapType: Byte;
   ImageType:    Byte;
   ColorMapSpec: array [0..4] of Byte;
   OrigX:        array [0..1] of Byte;
   OrigY:        array [0..1] of Byte;
   Width:        array [0..1] of Byte;
   Height:       array [0..1] of Byte;
   BPP:          Byte;
   ImageInfo:    Byte;
  end;

{$R *.dfm}

function IsAnim(Res: string): Boolean;
var
  WAD:          TWADEditor_1;
  WADName:      string;
  SectionName:  string;
  ResourceName: string;
  Data:         Pointer;
  Size:         Integer;
  Sign:         array[0..4] of Char;
  Sections,
  Resources:    SArray;
  a:            Integer;
  ok:           Boolean;
begin
 Result := False;

 g_ProcessResourceStr(Res, WADName, SectionName, ResourceName); 

 WAD := TWADEditor_1.Create;

 if not (WAD.ReadFile(WADName) and WAD.GetResource(SectionName, ResourceName, Data, Size)) then
 begin
  WAD.Destroy;
  Exit;
 end;

 WAD.FreeWAD;

 CopyMemory(@Sign[0], Data, 5);

 if not (Sign = DFWAD_SIGNATURE) then
 begin
  WAD.Destroy;
  FreeMem(Data);
  Exit;
 end;

 if not WAD.ReadMemory(Data, Size) then
 begin
  WAD.Destroy;
  FreeMem(Data);
  Exit;
 end;

 FreeMem(Data);

 Sections := WAD.GetSectionList;

 if Sections = nil then
 begin
  WAD.Destroy;
  Exit;
 end;

 ok := False;
 for a := 0 to High(Sections) do
  if Sections[a] = 'TEXT' then
  begin
   ok := True;
   Break;
  end;

 for a := 0 to High(Sections) do
  if Sections[a] = 'TEXTURES' then
  begin
   ok := ok and True;
   Break;
  end;

 if not ok then
 begin
  WAD.Destroy;
  Exit;
 end;
 
 Resources := WAD.GetResourcesList('TEXT');

 if Resources = nil then
 begin
  WAD.Destroy;
  Exit;
 end;

 ok := False;
 for a := 0 to High(Resources) do
  if Resources[a] = 'ANIM' then
  begin
   ok := True;
   Break;
  end;

 WAD.Destroy;

 Result := ok;
end;

function GetFrame(Res: string; var Data: Pointer; var Width, Height: Word): Boolean;
var
  AnimWAD:      Pointer;
  WAD:          TWADEditor_1;
  WADName:      string;
  SectionName:  string;
  ResourceName: string;
  Len:          Integer;
  config:       TConfig;
  TextData:     Pointer;
begin
 Result := False;
 
 g_ProcessResourceStr(Res, WADName, SectionName, ResourceName); 

 WAD := TWADEditor_1.Create;

 if not WAD.ReadFile(WADName) then
 begin
  WAD.Destroy;
  Exit;
 end;

 if not WAD.GetResource(SectionName, ResourceName, AnimWAD, Len) then
 begin
  WAD.Destroy;
  Exit;
 end;
 
 WAD.FreeWAD;

 if not WAD.ReadMemory(AnimWAD, Len) then
 begin
  FreeMem(AnimWAD);
  WAD.Destroy;
  Exit;
 end;
 
 if not WAD.GetResource('TEXT', 'ANIM', TextData, Len) then
 begin
  FreeMem(TextData);
  FreeMem(AnimWAD);
  WAD.Destroy;
  Exit;
 end;

 config := TConfig.CreateMem(TextData, Len);

 if not WAD.GetResource('TEXTURES', config.ReadStr('', 'resource', ''), Data, Len) then
 begin
  FreeMem(TextData);
  FreeMem(AnimWAD);
  WAD.Destroy;
  Exit;
 end;

 Height := config.ReadInt('', 'frameheight', 0);
 Width := config.ReadInt('', 'framewidth', 0);

 config.Destroy;
 WAD.Destroy;

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

 CopyMemory(@TGAHeader, Data, SizeOf(TGAHeader));

 if TGAHeader.ImageType <> 2 then Exit;
 if TGAHeader.ColorMapType <> 0 then Exit;
 if TGAHeader.BPP < 24 then Exit;

 Width  := TGAHeader.Width[0]+TGAHeader.Width[1]*256;
 Height := TGAHeader.Height[0]+TGAHeader.Height[1]*256;
 ColorDepth := TGAHeader.BPP;
 ImageSize  := Width*Height*(ColorDepth div 8);

 GetMem(Image, ImageSize);

 CopyMemory(Image, Pointer(Integer(Data)+SizeOf(TGAHeader)), ImageSize);

 BitMap := TBitMap.Create;

 if TGAHeader.BPP = 24 then BitMap.PixelFormat := pf24bit
  else BitMap.PixelFormat := pf32bit;
  
 BitMap.Width := Width;
 BitMap.Height := Height;

 for I := Height-1 downto 0 do
  CopyMemory(BitMap.ScanLine[Height-1-I], Pointer(Integer(Image)+(Width*I*(TGAHeader.BPP div 8))),
             Width*(TGAHeader.BPP div 8));

 FreeMem(Image, ImageSize);

 Result := BitMap;
end;

function ShowAnim(Res: string): TBitMap;
var
  AnimWAD:      Pointer;
  WAD:          TWADEditor_1;
  WADName:      string;
  SectionName:  string;
  ResourceName: string;
  Len:          Integer;
  config:       TConfig;
  TextData:     Pointer;
  TextureData:  Pointer;
label
  _end;
begin
 Result := nil;

 g_ProcessResourceStr(Res, WADName, SectionName, ResourceName); 

 WAD := TWADEditor_1.Create;
 WAD.ReadFile(WADName);
 WAD.GetResource(SectionName, ResourceName, AnimWAD, Len);
 WAD.FreeWAD;

 WAD.ReadMemory(AnimWAD, Len);
 WAD.GetResource('TEXT', 'ANIM', TextData, Len);

 config := TConfig.CreateMem(TextData, Len);
 WAD.GetResource('TEXTURES', config.ReadStr('', 'resource', ''), TextureData, Len);

 if (TextureData = nil) or (WAD.GetLastError <> DFWAD_NOERROR) then goto _end;

 Result := CreateBitMap(TextureData);

 Result.Height := config.ReadInt('', 'frameheight', 0);
 Result.Width := config.ReadInt('', 'framewidth', 0);

 _end:
 
 config.Destroy;
 WAD.Destroy;

 FreeMem(TextureData);
 FreeMem(TextData);
 FreeMem(AnimWAD);
end;

function ShowTGATexture(ResourceStr: string): TBitMap;
var
  TextureData:  Pointer;
  WAD:          TWADEditor_1;
  WADName:      string;
  SectionName:  string;
  ResourceName: string;
  Len:          Integer;
begin
 Result := nil;

 g_ProcessResourceStr(ResourceStr, WADName, SectionName, ResourceName);

 WAD := TWADEditor_1.Create;
 if not WAD.ReadFile(WADName) then
 begin
  WAD.Destroy;
  Exit;
 end;

 WAD.GetResource(SectionName, ResourceName, TextureData, Len);

 WAD.Destroy;

 Result := CreateBitMap(TextureData);

 FreeMem(TextureData, Len);
end;

procedure TAddTextureForm.bOKClick(Sender: TObject);
begin
 inherited;

 if not FResourceSelected then Exit;

 MainForm.lbTextureList.Selected[MainForm.lbTextureList.Count-1] := True;
 MainForm.lbTextureList.OnClick(nil);

 Close;
end;

procedure TAddTextureForm.FormActivate(Sender: TObject);
begin
 inherited;

 cbWADList.Items.Add(WAD_SPECIAL_TEXTURES);

 eTextureName.Text := '';
 iPreview.Canvas.FillRect(iPreview.Canvas.ClipRect);
end;

procedure TAddTextureForm.lbResourcesListClick(Sender: TObject);
var
  Texture: TBitMap;
  wad: string;
begin
 inherited;

 if lbResourcesList.ItemIndex = -1 then Exit;
 if FResourceName = '' then Exit;
 if cbWADList.Text = WAD_SPECIAL_TEXTURES then Exit;

 g_ProcessResourceStr(FFullResourceName, @wad, nil, nil);
 if wad = WAD_SPECIAL_TEXTURES then Exit;

 if IsAnim(FFullResourceName) then Texture := ShowAnim(FFullResourceName)
  else Texture := ShowTGATexture(FFullResourceName);

 if Texture = nil then Exit;
 iPreview.Canvas.FillRect(iPreview.Canvas.ClipRect);
 iPreview.Canvas.CopyRect(Texture.Canvas.ClipRect, Texture.Canvas, Texture.Canvas.ClipRect);
 Texture.Destroy;
end;

procedure TAddTextureForm.eTextureNameChange(Sender: TObject);
var
  a: Integer;
begin
 if (lbResourcesList.Items.Count = 0) or (eTextureName.Text = '') then Exit;

 for a := 0 to lbResourcesList.Items.Count-1 do
  if LowerCase(Copy(lbResourcesList.Items[a], 1, Length(eTextureName.Text))) =
     LowerCase(eTextureName.Text) then
  begin
   lbResourcesList.Selected[a] := True;
   lbResourcesList.Perform(LB_SETTOPINDEX, a, 0);
   lbResourcesList.OnClick(nil);
   Exit;
  end;
end;

procedure TAddTextureForm.cbWADListChange(Sender: TObject);
begin
 if cbWADList.Text = WAD_SPECIAL_TEXTURES then
 begin
  cbSectionsList.Clear;
  cbSectionsList.Items.Add('..');
  Exit;
 end;

 inherited;
end;

procedure TAddTextureForm.cbSectionsListChange(Sender: TObject);
begin
 if cbWADList.Text = WAD_SPECIAL_TEXTURES then
 begin
  lbResourcesList.Clear;
  lbResourcesList.Items.Add(TEXTURE_NAME_WATER);
  lbResourcesList.Items.Add(TEXTURE_NAME_ACID1);
  lbResourcesList.Items.Add(TEXTURE_NAME_ACID2);
  Exit;
 end;

 inherited;
end;

end.
