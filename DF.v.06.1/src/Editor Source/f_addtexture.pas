unit f_addtexture;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExtDlgs, e_Log, e_textures, g_wad;

type
  TAddTextureForm = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    Panel1: TPanel;
    iPreview: TImage;
    cbWADList: TComboBox;
    lbResourcesList: TListBox;
    Label1: TLabel;
    cbSectionsList: TComboBox;
    Label2: TLabel;
    procedure bCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure lbResourcesListClick(Sender: TObject);
    procedure cbWADListChange(Sender: TObject);
    procedure cbSectionsListChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddTextureForm: TAddTextureForm;

implementation

uses f_main;

{$R *.dfm}

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
  TGAHeader: packed record   // Header type for TGA images
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
  i:          Integer;
  BitMap:     TBitMap;

  TextureData:  Pointer;
  WAD:          TWADReader;
  WADName:      String;
  SectionName:  String;
  ResourceName: String;
  a: Integer;
begin
 Result := nil;

 for i := 1 to Length(ResourceStr) do
  if ResourceStr[i] = ':' then Break;

 WADName := Copy(ResourceStr, 1, i-1);                          //123:56\78
                                                                //   i  a
 for a := i+1 to Length(ResourceStr) do
  if ResourceStr[a] = '\' then Break;

 ResourceName := Copy(ResourceStr, a+1, Length(ResourceStr)-a);

 SectionName := Copy(ResourceStr, i+1, Length(ResourceStr)-Length(ResourceName)-Length(WADName)-2);

 WAD := TWADReader.Create(EditorDir+'\WADS\'+WADName);
 WAD.Read(SectionName, ResourceName, TextureData, ImageSize);
 WAD.Destroy;

 CopyMemory(@TGAHeader, TextureData, SizeOf(TGAHeader));

 if (TGAHeader.ImageType <> 2) then Exit;

 if TGAHeader.ColorMapType <> 0 then Exit;

 Width  := TGAHeader.Width[0]  + TGAHeader.Width[1]  * 256;
 Height := TGAHeader.Height[0] + TGAHeader.Height[1] * 256;
 ColorDepth := TGAHeader.BPP;
 ImageSize  := Width*Height*(ColorDepth div 8);

 if ColorDepth < 24 then Exit;

 GetMem(Image, ImageSize);

 CopyMemory(image, Pointer(Integer(TextureData)+SizeOf(TGAHeader)), ImageSize);

 BitMap := TBitMap.Create;

 if TGAHeader.BPP = 24 then
  BitMap.PixelFormat := pf24bit
   else BitMap.PixelFormat := pf32bit;
  
 BitMap.Width := Width;
 BitMap.Height := Height;

 for I := Height-1 downto 0 do
  CopyMemory(BitMap.ScanLine[Height-1-I], Pointer(Integer(Image)+(Width*I*(TGAHeader.BPP div 8))),
             Width*(TGAHeader.BPP div 8));

 FreeMem(Image);
 FreeMem(TextureData);
 
 Result := BitMap;
end;

procedure TAddTextureForm.bCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TAddTextureForm.FormActivate(Sender: TObject);
var
  SR: TSearchRec;
begin
 cbWADList.Clear;
 cbSectionsList.Clear;
 lbResourcesList.Clear;
 iPreview.Picture.Bitmap.Width := 256;
 iPreview.Picture.Bitmap.Height := 256;
 iPreview.Canvas.FillRect(iPreview.Canvas.ClipRect); 

 ChDir(EditorDir);
 if FindFirst(EditorDir+'\WADS\*.wad', faAnyFile, SR) = 0 then
 repeat
  cbWADList.Items.Add(SR.Name);
 until FindNext(SR) <> 0;
 FindClose(SR);
end;

procedure TAddTextureForm.bOKClick(Sender: TObject);
var
  i: Integer;
  a: Integer;
  ok: Boolean;
  ResourceName: String;
  SectionName: String;
begin
 if lbResourcesList.SelCount = 0 then
 begin
  MessageDlg('Не выбран ресурс', mtError, [mbOK], 0);
  Exit;
 end;

 ok := False;

 if lbResourcesList.SelCount > 1 then
 for i := 0 to lbResourcesList.Count-1 do
  if lbResourcesList.Selected[i] then
  begin
   ok := True;

   if cbSectionsList.Text = '..' then
    SectionName := '' else SectionName := cbSectionsList.Text;
   ResourceName := cbWADList.Text+':'+SectionName+'\'+lbResourcesList.Items[i];

   for a := 0 to MainForm.lbTextureList.Items.Count-1 do
    if ResourceName = MainForm.lbTextureList.Items[a] then
    begin
     MessageDlg(Format('Текстура "%s" уже существует', [ResourceName]), mtError, [mbOK], 0);
     ok := False;
    end;

   if Length(cbWADList.Text) > 30 then
   begin
    MessageDlg(Format('Имя WAD''а "%s" должно быть <= 30 символам', [ResourceName]), mtError, [mbOK], 0);
    ok := False;
   end;

   if ok then
   begin
    MainForm.lbTextureList.Items.Add(ResourceName);
    MainLevel.Map.PanelSystem.CreateTexture(ResourceName);
   end;
 end;

 if lbResourcesList.SelCount = 1 then
 begin
  ok := True;

  if cbSectionsList.Text = '..' then
   SectionName := '' else SectionName := cbSectionsList.Text;
  ResourceName := cbWADList.Text+':'+SectionName+'\'+lbResourcesList.Items[lbResourcesList.ItemIndex];

  for a := 0 to MainForm.lbTextureList.Items.Count-1 do
  begin
   if ResourceName = MainForm.lbTextureList.Items[a] then
   begin
    MessageDlg(Format('Текстура "%s" уже существует', [ResourceName]), mtError, [mbOK], 0);
    ok := False;
   end;
  end;

  if ok then
  begin
   MainForm.lbTextureList.Items.Add(ResourceName);
   MainLevel.Map.PanelSystem.CreateTexture(ResourceName);
  end;
 end;

 if ok then Close;
end;

procedure TAddTextureForm.lbResourcesListClick(Sender: TObject);
var
  Texture: TBitMap;
  ResourceName: String;
  SectionName: String;
begin
 if lbResourcesList.ItemIndex = -1 then Exit;

 if cbSectionsList.Text = '..' then
   SectionName := '' else SectionName := cbSectionsList.Text;
  ResourceName := cbWADList.Text+':'+SectionName+'\'+lbResourcesList.Items[lbResourcesList.ItemIndex];

 Texture := ShowTGATexture(ResourceName);
 iPreview.Picture.Bitmap.Assign(Texture);
end;

procedure TAddTextureForm.cbWADListChange(Sender: TObject);
var
  WAD: TWADReader;
  SectionList: ArrayStr16;
  i: Integer;
begin
 WAD := TWADReader.Create(EditorDir+'\WADS\'+cbWADList.Text);
 SectionList := WAD.GetSectionList;
 WAD.Destroy;

 cbSectionsList.Clear;

 if SectionList <> nil then
  for i := 0 to High(SectionList) do
   cbSectionsList.Items.Add(SectionList[i]); 
end;

procedure TAddTextureForm.cbSectionsListChange(Sender: TObject);
var
  ResourceList: ArrayStr16;
  WAD: TWADReader;
  i: DWORD;
begin
 WAD := TWADReader.Create(EditorDir+'\WADS\'+cbWADList.Text);
 ResourceList := WAD.GetResourceList(cbSectionsList.Text);
 WAD.Destroy;

 lbResourcesList.Clear;

 if ResourceList <> nil then
  for i := 0 to High(ResourceList) do
   if ResourceList[i] = '' then lbResourcesList.Items.Add('..')
    else lbResourcesList.Items.Add(ResourceList[i]);
end;

end.
