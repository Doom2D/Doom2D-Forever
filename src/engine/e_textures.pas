(* Copyright (C)  DooM 2D:Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$MODE DELPHI}
unit e_textures;

{ This unit provides interface to load 24-bit and 32-bit uncompressed images
  from Truevision Targa (TGA) graphic files, and create OpenGL textures
  from it's data. }

interface

uses
  GL, GLExt, SysUtils, e_log,
  ImagingTypes, Imaging, ImagingUtility;

type
  GLTexture = record
    id: GLuint;
    width, height: Word; // real
    glwidth, glheight: Word; // powerof2
    u, v: Single; // usually 1.0
    fmt: GLuint;
  end;

var
  e_DummyTextures: Boolean = False;
  e_glLegacyNPOT: Boolean = False;
  TEXTUREFILTER: Integer = GL_NEAREST;

function CreateTexture (var tex: GLTexture; Width, Height, aFormat: Word; pData: Pointer): Boolean;

// Standard set of images loading functions
function LoadTexture (Filename: String; var Texture: GLTexture; var pWidth, pHeight: Word; Fmt: PWord=nil): Boolean;
function LoadTextureEx (Filename: String; var Texture: GLTexture; fX, fY, fWidth, fHeight: Word; Fmt: PWord=nil): Boolean;
function LoadTextureMem (pData: Pointer; dataSize: LongInt; var Texture: GLTexture; var pWidth, pHeight: Word; Fmt: PWord=nil): Boolean;
function LoadTextureMemEx (pData: Pointer; dataSize: LongInt; var Texture: GLTexture; fX, fY, fWidth, fHeight: Word; Fmt: PWord=nil): Boolean;

// `img` must be valid!
function LoadTextureImg (var img: TImageData; var Texture: GLTexture; var pWidth, pHeight: Word; Fmt: PWord=nil): Boolean;


implementation

uses
  Classes, BinEditor, utils;


function AlignP2 (n: Word): Word;
begin
  Dec(n);
  n := n or (n shr 1);
  n := n or (n shr 2);
  n := n or (n shr 4);
  n := n or (n shr 8);
  n := n or (n shr 16);
  Inc(n);
  Result := n;
end;


// This is auxiliary function that creates OpenGL texture from raw image data
function CreateTexture (var tex: GLTexture; Width, Height, aFormat: Word; pData: Pointer): Boolean;
var
  Texture: GLuint;
  fmt: GLenum;
  buf: PByte;
  f, c: Integer;
begin
  tex.width := Width;
  tex.height := Height;
  tex.glwidth := Width;
  tex.glheight := Height;
  tex.u := 1;
  tex.v := 1;

  if e_glLegacyNPOT then
  begin
    tex.glwidth := AlignP2(Width);
    tex.glheight := AlignP2(Height);
    if tex.glwidth <> tex.width then tex.u := (tex.width+0.0)/(tex.glwidth+0.0);
    if tex.glheight <> tex.height then tex.v := (tex.height+0.0)/(tex.glheight+0.0);
  end;

  //if (tex.glwidth <> tex.width) or (tex.glheight <> tex.height) then
  //  e_WriteLog(Format('NPOT: orig is %ux%u; gl is %ux%u; u=%f; v=%f', [Width, Height, tex.glwidth, tex.glheight, tex.u, tex.v]), MSG_NOTIFY);

  if e_DummyTextures then
  begin
    tex.id := GLuint(-1);
    Result := True;
    Exit;
  end;

  glGenTextures(1, @Texture);
  tex.id := Texture;
  glBindTexture(GL_TEXTURE_2D, Texture);

  if (tex.glwidth <> tex.width) or (tex.glheight <> tex.height) then
    e_WriteLog(Format('NPOT: %u is %ux%u; gl is %ux%u; u=%f; v=%f', [tex.id, Width, Height, tex.glwidth, tex.glheight, tex.u, tex.v]), MSG_NOTIFY);

  // texture blends with object background
  glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  // texture does NOT blend with object background
  //glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);

  {
    Select a filtering type.
    BiLinear filtering produces very good results with little performance impact

    GL_NEAREST               - Basic texture (grainy looking texture)
    GL_LINEAR                - BiLinear filtering
    GL_LINEAR_MIPMAP_NEAREST - Basic mipmapped texture
    GL_LINEAR_MIPMAP_LINEAR  - BiLinear Mipmapped texture
  }

  // for GL_TEXTURE_MAG_FILTER only first two can be used
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, TEXTUREFILTER);
  // for GL_TEXTURE_MIN_FILTER all of the above can be used
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, TEXTUREFILTER);

  // create empty texture
  if aFormat = GL_RGBA then fmt := GL_RGBA else fmt := GL_RGB; // silly, yeah?
  glTexImage2D(GL_TEXTURE_2D, 0, fmt, tex.glwidth, tex.glheight, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);

  (*
  GetMem(buf, tex.glwidth*4*tex.glheight);
  try
    FillChar(buf^, tex.glwidth*4*tex.glheight, 255);
    glTexSubImage2D(GL_TEXTURE_2D, 0,  0, 0, tex.glwidth, tex.glheight, fmt, GL_UNSIGNED_BYTE, buf);
    if (tex.glheight = 128) and (tex.height = 80) then
    begin
      for f := 0 to tex.glheight-1 do
      begin
        for c := 0 to tex.glwidth-1 do
        begin
          buf[f*(tex.glwidth*4)+c*4+0] := 255;
          buf[f*(tex.glwidth*4)+c*4+1] := 127;
          buf[f*(tex.glwidth*4)+c*4+2] := 0;
        end;
      end;
      glTexSubImage2D(GL_TEXTURE_2D, 0,  0, 82, tex.glwidth, {tex.glheight}1, fmt, GL_UNSIGNED_BYTE, buf);
    end;
  finally
    FreeMem(buf);
  end;
  *)

  glTexSubImage2D(GL_TEXTURE_2D, 0,  0, 0, Width, Height, fmt, GL_UNSIGNED_BYTE, pData);
  //glTexSubImage2D(GL_TEXTURE_2D, 0,  0, tex.glheight-tex.height, Width, Height, fmt, GL_UNSIGNED_BYTE, pData);

  glBindTexture(GL_TEXTURE_2D, 0);

  Result := true;
end;

// `img` must be valid!
function LoadTextureImg (var img: TImageData; var Texture: GLTexture; var pWidth, pHeight: Word; Fmt: PWord=nil): Boolean;
var
  image, ii: PByte;
  width, height: Integer;
  imageSize: Integer;
  x, y: Integer;
  clr: TColor32Rec;
begin
  result := false;
  pWidth := 0;
  pHeight := 0;
  if Fmt <> nil then Fmt^ := GL_RGBA; // anyway

  if (img.width < 1) or (img.width > 32768) or (img.height < 1) or (img.height > 32768) then
  begin
    e_WriteLog('Error loading texture: invalid image dimensions', MSG_WARNING);
    exit;
  end;
  //ConvertImage(img, ifA8R8G8B8);
  width := img.width;
  height := img.height;
  pWidth := width;
  pHeight := height;
  imageSize := Width*Height*4;
  GetMem(image, imageSize);
  try
    // it's slow, but i don't care for now
    ii := image;
    for y := height-1 downto 0 do
    begin
      for x := 0 to width-1 do
      begin
        clr := GetPixel32(img, x, y);
        ii^ := clr.r; Inc(ii);
        ii^ := clr.g; Inc(ii);
        ii^ := clr.b; Inc(ii);
        ii^ := clr.a; Inc(ii);
      end;
    end;
    CreateTexture(Texture, width, height, GL_RGBA, image);
    result := true;
  finally
    FreeMem(image);
  end;
end;


function LoadTextureMem (pData: Pointer; dataSize: LongInt; var Texture: GLTexture; var pWidth, pHeight: Word; Fmt: PWord=nil): Boolean;
var
  image, ii: PByte;
  width, height: Integer;
  imageSize: Integer;
  img: TImageData;
  x, y: Integer;
  clr: TColor32Rec;
begin
  result := false;
  pWidth := 0;
  pHeight := 0;
  if Fmt <> nil then Fmt^ := GL_RGBA; // anyway

  InitImage(img);
  if not LoadImageFromMemory(pData, dataSize, img) then
  begin
    e_WriteLog('Error loading texture: unknown image format', MSG_WARNING);
    exit;
  end;
  try
    result := LoadTextureImg(img, Texture, pWidth, pHeight, Fmt);
  finally
    FreeImage(img);
  end;
end;


function LoadTextureMemEx (pData: Pointer; dataSize: LongInt; var Texture: GLTexture; fX, fY, fWidth, fHeight: Word; Fmt: PWord=nil): Boolean;
var
  image, ii: PByte;
  width, height: Integer;
  imageSize: Integer;
  img: TImageData;
  x, y: Integer;
  clr: TColor32Rec;
begin
  result := false;
  if Fmt <> nil then Fmt^ := GL_RGBA; // anyway

  InitImage(img);
  if not LoadImageFromMemory(pData, dataSize, img) then
  begin
    e_WriteLog('Error loading texture: unknown image format', MSG_WARNING);
    exit;
  end;
  try
    if (img.width < 1) or (img.width > 32768) or (img.height < 1) or (img.height > 32768) then
    begin
      e_WriteLog('Error loading texture: invalid image dimensions', MSG_WARNING);
      exit;
    end;
    //ConvertImage(img, ifA8R8G8B8);
    if fX > img.width then exit;
    if fY > img.height then exit;
    if fX+fWidth > img.width then exit;
    if fY+fHeight > img.height then exit;
    //writeln('fX=', fX, '; fY=', fY, '; fWidth=', fWidth, '; fHeight=', fHeight);
    imageSize := img.width*img.height*4;
    GetMem(image, imageSize);
    try
      // it's slow, but i don't care for now
      ii := image;
      for y := fY+fHeight-1 downto fY do
      begin
        for x := fX to fX+fWidth-1 do
        begin
          clr := GetPixel32(img, x, y);
          ii^ := clr.r; Inc(ii);
          ii^ := clr.g; Inc(ii);
          ii^ := clr.b; Inc(ii);
          ii^ := clr.a; Inc(ii);
        end;
      end;
      CreateTexture(Texture, fWidth, fHeight, GL_RGBA, image);
      result := true;
    finally
      FreeMem(image);
    end;
  finally
    FreeImage(img);
  end;
end;


function LoadTexture (filename: AnsiString; var Texture: GLTexture; var pWidth, pHeight: Word; Fmt: PWord=nil): Boolean;
var
  fs: TStream;
  img: Pointer;
  imageSize: LongInt;
begin
  result := False;
  pWidth := 0;
  pHeight := 0;
  if Fmt <> nil then Fmt^ := GL_RGBA; // anyway
  fs := nil;

  try
    fs := openDiskFileRO(filename);
  except
    fs := nil;
  end;
  if fs = nil then
  begin
    e_WriteLog('Texture "'+filename+'" not found', MSG_WARNING);
    exit;
  end;

  try
    imageSize := fs.size;
    GetMem(img, imageSize);
    try
      fs.readBuffer(img^, imageSize);
      result := LoadTextureMem(img, imageSize, Texture, pWidth, pHeight, Fmt);
    finally
      FreeMem(img);
    end;
  finally
    fs.Free();
  end;
end;


function LoadTextureEx (filename: AnsiString; var Texture: GLTexture; fX, fY, fWidth, fHeight: Word; Fmt: PWord=nil): Boolean;
var
  fs: TStream;
  img: Pointer;
  imageSize: LongInt;
begin
  result := False;
  if Fmt <> nil then Fmt^ := GL_RGBA; // anyway
  fs := nil;

  try
    fs := openDiskFileRO(filename);
  except
    fs := nil;
  end;
  if fs = nil then
  begin
    e_WriteLog('Texture "'+filename+'" not found', MSG_WARNING);
    exit;
  end;

  try
    imageSize := fs.size;
    GetMem(img, imageSize);
    try
      fs.readBuffer(img^, imageSize);
      result := LoadTextureMemEx(img, imageSize, Texture, fX, fY, fWidth, fHeight, Fmt);
    finally
      FreeMem(img);
    end;
  finally
    fs.Free();
  end;
end;

end.
