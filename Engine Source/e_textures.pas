unit e_textures;

interface

uses
  Windows,
  dglOpenGL,
  SysUtils,
  e_log;

var
  TEXTUREFILTER: Integer= GL_NEAREST;

function LoadTexture(Filename: String; var Texture: GLuint; var pWidth, pHeight: Word): Boolean;
function LoadTextureEx(Filename: String; var Texture: GLuint; fX, fY, fWidth, fHeight: Word): Boolean;
function LoadTextureMem(pData: Pointer; var Texture: GLuint; var pWidth, pHeight: Word): Boolean;
function LoadTextureMemEx(pData: Pointer; var Texture: GLuint; fX, fY, fWidth, fHeight: Word): Boolean;

implementation

type
 TTGAHeader = packed record
  FileType:     Byte;
  ColorMapType: Byte;
  ImageType:    Byte;
  ColorMapSpec: array[0..4] of Byte;
  OrigX:        array[0..1] of Byte;
  OrigY:        array[0..1] of Byte;
  Width:        array[0..1] of Byte;
  Height:       array[0..1] of Byte;
  BPP:          Byte;
  ImageInfo:    Byte;
 end;

function CreateTexture(Width, Height, Format: Word; pData: Pointer) : Integer;
var
  Texture: PGLuint;
begin
 New(Texture);
 glGenTextures(1, Texture);
 glBindTexture(GL_TEXTURE_2D, Texture^);
 glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);  {Texture blends with object background}
// glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);  {Texture does NOT blend with object background}

 { Select a filtering type. BiLinear filtering produces very good results with little performance impact
   GL_NEAREST               - Basic texture (grainy looking texture)
   GL_LINEAR                - BiLinear filtering
   GL_LINEAR_MIPMAP_NEAREST - Basic mipmapped texture
   GL_LINEAR_MIPMAP_LINEAR  - BiLinear Mipmapped texture
 }

 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, TEXTUREFILTER{GL_LINEAR}{GL_NEAREST}); { only first two can be used }
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, TEXTUREFILTER{GL_LINEAR}{GL_NEAREST}); { all of the above can be used }

 if Format = GL_RGBA then
  gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData)
 else
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, Width, Height, GL_RGB, GL_UNSIGNED_BYTE, pData);
// glTexImage2D(GL_TEXTURE_2D, 0, 3, Width, Height, 0, GL_RGB, GL_UNSIGNED_BYTE, pData);  // Use when not wanting mipmaps to be built by openGL

 Result := Texture^;
 
 Dispose(Texture);
end;

function LoadTextureMem(pData: Pointer; var Texture: GLuint; var pWidth, pHeight: Word): Boolean;
var
  TGAHeader:     TTGAHeader;
  image:         Pointer;  
  Width, Height: Integer;
  ImageSize:     Integer;
  i:             Integer;
  Front:         ^Byte;
  Back:          ^Byte;
  Temp:          Byte;
  BPP:           Byte;
begin
 Result := False;
 pWidth := 0;
 pHeight := 0;

 CopyMemory(@TGAHeader, pData, SizeOf(TGAHeader));

 if TGAHeader.ImageType <> 2 then
 begin
  e_WriteLog('Error loading texture: Bad ImageType', MSG_WARNING);
  Exit;
 end;

 if TGAHeader.ColorMapType <> 0 then
 begin
  e_WriteLog('Error loading texture: Bad ColorMapType', MSG_WARNING);
  Exit;
 end;

 if TGAHeader.BPP < 24 then
 begin
  e_WriteLog('Error loading texture: BPP less than 24', MSG_WARNING);
  Exit;
 end;

 Width := TGAHeader.Width[0] + TGAHeader.Width[1] * 256;
 Height := TGAHeader.Height[0] + TGAHeader.Height[1] * 256;
 BPP := TGAHeader.BPP;

 ImageSize := Width*Height*(BPP div 8);

 GetMem(Image, ImageSize);
 CopyMemory(image, Pointer(Integer(pData)+SizeOf(TGAHeader)), ImageSize);

 for i := 0 to Width * Height - 1 do
 begin
  Front := Pointer(Integer(Image) + i*(BPP div 8));
  Back := Pointer(Integer(Image) + i*(BPP div 8) + 2);
  Temp := Front^;
  Front^ := Back^;
  Back^ := Temp;
 end;

 if BPP = 24 then Texture := CreateTexture(Width, Height, GL_RGB, Image)
  else Texture := CreateTexture(Width, Height, GL_RGBA, Image);

 FreeMem(Image);

 pWidth := Width;
 pHeight := Height;

 Result := True;
end;

function LoadTextureMemEx(pData: Pointer; var Texture: GLuint; fX, fY, fWidth, fHeight: Word): Boolean;
var
  TGAHeader:     TTGAHeader;
  image, image2: Pointer;
  Width, Height: Integer;
  ImageSize:     Integer;
  i, a, b:       Integer;
  Front:         ^Byte;
  Back:          ^Byte;
  Temp:          Byte;
  BPP:           Byte;
  Base:          Integer;
begin
 Result := False;

 CopyMemory(@TGAHeader, pData, SizeOf(TGAHeader));

 if TGAHeader.ImageType <> 2 then
 begin
  e_WriteLog('Error loading texture: Bad ImageType', MSG_WARNING);
  Exit;
 end;

 if TGAHeader.ColorMapType <> 0 then
 begin
  e_WriteLog('Error loading texture: Bad ColorMapType', MSG_WARNING);
  Exit;
 end;

 if TGAHeader.BPP < 24 then
 begin
  e_WriteLog('Error loading texture: BPP less than 24', MSG_WARNING);
  Exit;
 end;

 Width := TGAHeader.Width[0]  + TGAHeader.Width[1]  * 256;
 Height := TGAHeader.Height[0] + TGAHeader.Height[1] * 256;
 BPP := TGAHeader.BPP;

 if fX > Width then Exit;
 if fY > Height then Exit;
 if fX+fWidth > Width then Exit;
 if fY+fHeight > Height then Exit;

 ImageSize := Width*Height*(BPP div 8);
 GetMem(image2, ImageSize);
 CopyMemory(image2, Pointer(Integer(pData)+SizeOf(TGAHeader)), ImageSize);

 a := BPP div 8;

 for i := 0 to Width*Height-1 do
 begin
  Front := Pointer(Integer(Image2)+i*a);
  Back := Pointer(Integer(Image2)+i*a+2);
  Temp := Front^;
  Front^ := Back^;
  Back^ := Temp;
 end;

 fY := Height-(fY+fHeight);

 ImageSize := fHeight*fWidth*(BPP div 8);
 GetMem(image, ImageSize);

 Base := Integer(image2)+fY*Width*(BPP div 8)+fX*(BPP div 8);
 a := fWidth*(BPP div 8);
 b := Width*(BPP div 8);

 for i := 0 to fHeight-1 do
  CopyMemory(Pointer(Integer(image)+a*i), Pointer(Base+b*i), a);

 if BPP = 24 then Texture := CreateTexture(fWidth, fHeight, GL_RGB, image)
  else Texture := CreateTexture(fWidth, fHeight, GL_RGBA, image);

 FreeMem(image);
 FreeMem(image2);

 Result := True;
end;

function LoadTexture(Filename: String; var Texture: GLuint; var pWidth, pHeight: Word): Boolean;
var
  TGAHeader:     TTGAHeader;
  TGAFile:       File;
  bytesRead:     Integer;
  image:         Pointer;
  Width, Height: Integer;
  ImageSize:     Integer;
  i:             Integer;
  Front:         ^Byte;
  Back:          ^Byte;
  Temp:          Byte;
  BPP:           Byte;
begin
 Result := False;
 pWidth := 0;
 pHeight := 0;

 if not FileExists(Filename) then
 begin
  e_WriteLog('Texture '+Filename+' not found', MSG_WARNING);
  Exit;
 end;

 AssignFile(TGAFile, Filename);
 Reset(TGAFile, 1);
 BlockRead(TGAFile, TGAHeader, SizeOf(TGAHeader));

 if TGAHeader.ImageType <> 2 then
 begin
  CloseFile(TGAFile);
  e_WriteLog('Error loading texture: Bad ImageType', MSG_WARNING);
  Exit;
 end;

 if TGAHeader.ColorMapType <> 0 then
 begin
  CloseFile(TGAFile);
  e_WriteLog('Error loading texture: Bad ColorMapType', MSG_WARNING);
  Exit;
 end;

 if TGAHeader.BPP < 24 then
 begin
  CloseFile(TGAFile);
  e_WriteLog('Error loading texture: BPP less than 24', MSG_WARNING);
  Exit;
 end;

 Width  := TGAHeader.Width[0]  + TGAHeader.Width[1]  * 256;
 Height := TGAHeader.Height[0] + TGAHeader.Height[1] * 256;
 BPP := TGAHeader.BPP;

 ImageSize := Width*Height*(BPP div 8);

 GetMem(Image, ImageSize);

 BlockRead(TGAFile, image^, ImageSize, bytesRead);
 if bytesRead <> ImageSize then
 begin
  CloseFile(TGAFile);
  Exit;
 end;

 CloseFile(TGAFile);

 for i := 0 to Width * Height - 1 do
 begin
  Front := Pointer(Integer(Image) + i*(BPP div 8));
  Back := Pointer(Integer(Image) + i*(BPP div 8) + 2);
  Temp := Front^;
  Front^ := Back^;
  Back^ := Temp;
 end;

 if BPP = 24 then Texture := CreateTexture(Width, Height, GL_RGB, Image)
  else Texture :=CreateTexture(Width, Height, GL_RGBA, Image);

 FreeMem(Image);

 pWidth := Width;
 pHeight := Height;

 Result := True;
end;

function LoadTextureEx(Filename: String; var Texture: GLuint; fX, fY, fWidth, fHeight: Word): Boolean;
var
  TGAHeader:     TTGAHeader;
  TGAFile:       File;
  image, image2: Pointer;
  Width, Height: Integer;
  ImageSize:     Integer;
  i:             Integer;
  Front:         ^Byte;
  Back:          ^Byte;
  Temp:          Byte;
  BPP:           Byte;
  Base:          Integer;
begin
 Result := False;

 if not FileExists(Filename) then
 begin
  e_WriteLog('Texture '+Filename+' not found', MSG_WARNING);
  Exit;
 end;

 AssignFile(TGAFile, Filename);
 Reset(TGAFile, 1);
 BlockRead(TGAFile, TGAHeader, SizeOf(TGAHeader));

 if TGAHeader.ImageType <> 2 then
 begin
  CloseFile(TGAFile);
  e_WriteLog('Error loading texture: Bad ImageType', MSG_WARNING);
  Exit;
 end;

 if TGAHeader.ColorMapType <> 0 then
 begin
  CloseFile(TGAFile);
  e_WriteLog('Error loading texture: Bad ColorMapType', MSG_WARNING);
  Exit;
 end;

 if TGAHeader.BPP < 24 then
 begin
  CloseFile(TGAFile);
  e_WriteLog('Error loading texture: BPP less than 24', MSG_WARNING);
  Exit;
 end;

 Width := TGAHeader.Width[0] + TGAHeader.Width[1] * 256;
 Height := TGAHeader.Height[0] + TGAHeader.Height[1] * 256;
 BPP := TGAHeader.BPP;

 if fX > Width then Exit;
 if fY > Height then Exit;
 if fX+fWidth > Width then Exit;
 if fY+fHeight > Height then Exit;

 ImageSize := Width*Height*(BPP div 8);
 GetMem(image2, ImageSize);
 BlockRead(TGAFile, image2^, ImageSize);

 CloseFile(TGAFile);

 for i := 0 to Width * Height - 1 do
 begin
  Front := Pointer(Integer(Image2) + i*(BPP div 8));
  Back := Pointer(Integer(Image2) + i*(BPP div 8) + 2);
  Temp := Front^;
  Front^ := Back^;
  Back^ := Temp;
 end;

 fY := Height-(fY+fHeight);

 ImageSize := fHeight*fWidth*(BPP div 8);
 GetMem(image, ImageSize);

 Base := Integer(image2)+fY*Width*(BPP div 8)+fX*(BPP div 8);

 for i := 0 to fHeight-1 do
 begin
  CopyMemory(Pointer(Integer(image)+fWidth*(BPP div 8)*i), Pointer(Base+Width*(BPP div 8)*i), fWidth*(BPP div 8));
 end;                                                

 if BPP = 24 then Texture := CreateTexture(fWidth, fHeight, GL_RGB, Image)
  else Texture := CreateTexture(fWidth, fHeight, GL_RGBA, Image);

 FreeMem(Image);
 FreeMem(Image2);

 Result := True;
end;

end.
