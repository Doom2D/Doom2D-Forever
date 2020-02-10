(* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../shared/a_modes.inc}
unit e_graphics;

interface

uses
  {$INCLUDE ../nogl/noGLuses.inc}
  {$IFDEF USE_SDL2}
    SDL2,
  {$ENDIF}
  SysUtils, Classes, Math, e_log, e_texture,
  MAPDEF, ImagingTypes, Imaging, ImagingUtility;

type
  TMirrorType=(None, Horizontal, Vertical);
  TBlending=(None, Blend, Filter, Invert);

  TPoint2i = record
    X, Y: Integer;
  end;

  TPoint2f = record
    X, Y: Double;
  end;

  TRect = record
    Left, Top, Right, Bottom: Integer;
  end;

  TRectWH = record
   X, Y: Integer;
   Width, Height: Word;
  end;

  TRGB = packed record
   R, G, B: Byte;
  end;

  PDFPoint = ^TDFPoint;
  PPoint2f = ^TPoint2f;
  PRect = ^TRect;
  PRectWH = ^TRectWH;


//------------------------------------------------------------------
// прототипы функций
//------------------------------------------------------------------
procedure e_InitGL();
procedure e_SetViewPort(X, Y, Width, Height: Word);
procedure e_ResizeWindow(Width, Height: Integer);
procedure e_ResizeFramebuffer(Width, Height: Integer);
procedure e_BlitFramebuffer(WinWidth, WinHeight: Integer);

procedure e_Draw(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                 Blending: Boolean; Mirror: TMirrorType = TMirrorType.None);
procedure e_DrawAdv(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                    Blending: Boolean; Angle: Single; RC: PDFPoint; Mirror: TMirrorType = TMirrorType.None);
procedure e_DrawSize(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                     Blending: Boolean; Width, Height: Word; Mirror: TMirrorType = TMirrorType.None);
procedure e_DrawSizeMirror(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                           Blending: Boolean; Width, Height: Word; Mirror: TMirrorType = TMirrorType.None);

procedure e_DrawFill(ID: DWORD; X, Y: Integer; XCount, YCount: Word; Alpha: Integer;
                     AlphaChannel: Boolean; Blending: Boolean; ambientBlendMode: Boolean=false);

procedure e_DrawFillX (id: DWORD; x, y, wdt, hgt: Integer; alpha: Integer; alphachannel: Boolean;
                       blending: Boolean; scale: Single; ambientBlendMode: Boolean=false);

procedure e_AmbientQuad (x, y, w, h: Integer; r, g, b, a: Byte);

procedure e_DrawPoint(Size: Byte; X, Y: Integer; Red, Green, Blue: Byte);
procedure e_DrawLine(Width: Byte; X1, Y1, X2, Y2: Integer; Red, Green, Blue: Byte; Alpha: Byte = 0);
procedure e_DrawQuad(X1, Y1, X2, Y2: Integer; Red, Green, Blue: Byte; Alpha: Byte = 0);
procedure e_DrawFillQuad(X1, Y1, X2, Y2: Integer; Red, Green, Blue, Alpha: Byte;
                         Blending: TBlending = TBlending.None);
procedure e_DarkenQuad (x0, y0, x1, y1: Integer; a: Integer);
procedure e_DarkenQuadWH (x, y, w, h: Integer; a: Integer);

function e_CreateTextureImg (var img: TImageData; var ID: DWORD): Boolean;
function e_CreateTexture(FileName: string; var ID: DWORD): Boolean;
function e_CreateTextureEx(FileName: string; var ID: DWORD; fX, fY, fWidth, fHeight: Word): Boolean;
function e_CreateTextureMem(pData: Pointer; dataSize: LongInt; var ID: DWORD): Boolean;
function e_CreateTextureMemEx(pData: Pointer; dataSize: LongInt; var ID: DWORD; fX, fY, fWidth, fHeight: Word): Boolean;
procedure e_GetTextureSize(ID: DWORD; Width, Height: PWord);
procedure e_DeleteTexture(ID: DWORD);
procedure e_RemoveAllTextures();

// CharFont
function e_CharFont_Create(sp: ShortInt=0): DWORD;
procedure e_CharFont_AddChar(FontID: DWORD; Texture: Integer; c: Char; w: Byte);
procedure e_CharFont_Print(FontID: DWORD; X, Y: Integer; Text: string);
procedure e_CharFont_PrintEx(FontID: DWORD; X, Y: Integer; Text: string;
                             Color: TRGB; Scale: Single = 1.0);
procedure e_CharFont_PrintFmt(FontID: DWORD; X, Y: Integer; Text: string);
procedure e_CharFont_GetSize(FontID: DWORD; Text: string; var w, h: Word);
procedure e_CharFont_GetSizeFmt(FontID: DWORD; Text: string; var w, h: Word);
function e_CharFont_GetMaxWidth(FontID: DWORD): Word;
function e_CharFont_GetMaxHeight(FontID: DWORD): Word;
procedure e_CharFont_Remove(FontID: DWORD);
procedure e_CharFont_RemoveAll();

// TextureFont
procedure e_TextureFontBuild(Tex: DWORD; var FontID: DWORD; XCount, YCount: Word;
                             Space: ShortInt=0);
procedure e_TextureFontKill(FontID: DWORD);
procedure e_TextureFontPrint(X, Y: GLint; Text: string; FontID: DWORD);
procedure e_TextureFontPrintEx(X, Y: GLint; Text: string; FontID: DWORD; Red, Green,
                               Blue: Byte; Scale: Single; Shadow: Boolean = False);
procedure e_TextureFontPrintFmt(X, Y: GLint; Text: string; FontID: DWORD;
                                Shadow: Boolean = False; Newlines: Boolean = False);
procedure e_TextureFontGetSize(ID: DWORD; out CharWidth, CharHeight: Byte);
procedure e_RemoveAllTextureFont();

function e_TextureFontCharWidth (ch: Char; FontID: DWORD): Integer;
procedure e_TextureFontPrintCharEx (X, Y: Integer; Ch: Char; FontID: DWORD; Shadow: Boolean = False);

procedure e_ReleaseEngine();
procedure e_BeginRender();
procedure e_Clear(Mask: TGLbitfield; Red, Green, Blue: Single); overload;
procedure e_Clear(); overload;
procedure e_EndRender();

{$IFDEF USE_SDL2}
function e_GetGamma(win: PSDL_Window): Byte;
procedure e_SetGamma(win: PSDL_Window;Gamma: Byte);
{$ENDIF}

procedure e_MakeScreenshot(st: TStream; Width, Height: Word);

function _RGB(Red, Green, Blue: Byte): TRGB;
function _Point(X, Y: Integer): TPoint2i;
function _Rect(X, Y: Integer; Width, Height: Word): TRectWH;
function _TRect(L, T, R, B: LongInt): TRect;

//function e_getTextGLId (ID: DWORD): GLuint;

var
  e_Colors: TRGB;
  e_NoGraphics: Boolean = False;
  e_FastScreenshots: Boolean = true; // it's REALLY SLOW with `false`
  g_dbg_scale: Single = 1.0;
  r_pixel_scale: Single = 1.0;


implementation

uses
  paszlib, crc, utils;


type
  TTexture = record
   tx:     GLTexture;
  end;

  TTextureFont = record
   Texture:     DWORD;
   TextureID:   DWORD;
   Base:        Uint32;
   CharWidth:   Byte;
   CharHeight:  Byte;
   XC, YC:      WORD;
   SPC:         ShortInt;
  end;

  TCharFont = record
   Chars: array[0..255] of
    record
     TextureID: Integer;
     Width: Byte;
    end;
   Space: ShortInt;
   Height: ShortInt;
   alive: Boolean;
  end;

  TSavedTexture = record
    TexID:  DWORD;
    OldID:  DWORD;
    Pixels: Pointer;
  end;

var
  e_Textures: array of TTexture = nil;
  e_TextureFonts: array of TTextureFont = nil;
  e_CharFonts: array of TCharFont;
  //e_SavedTextures: array of TSavedTexture;
{$IF NOT DEFINED(HEADLESS) AND NOT DEFINED(USE_GLES1)}
  e_FBO: GLuint = 0;
  e_RBO: GLuint = 0;
  e_Frame: GLuint = 0;
  e_FrameW: Integer = -1;
  e_FrameH: Integer = -1;
{$ENDIF}

//function e_getTextGLId (ID: DWORD): GLuint; begin result := e_Textures[ID].tx.id; end;

//------------------------------------------------------------------
// Инициализирует OpenGL
//------------------------------------------------------------------
procedure e_InitGL();
begin
  if e_NoGraphics then
  begin
    e_DummyTextures := True;
    Exit;
  end;
  e_Colors.R := 255;
  e_Colors.G := 255;
  e_Colors.B := 255;
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_SCISSOR_TEST);
  glClearColor(0, 0, 0, 0);
end;

procedure e_SetViewPort(X, Y, Width, Height: Word);
var
  mat: Array [0..15] of GLDouble;

begin
  if e_NoGraphics then Exit;
  glLoadIdentity();
  glScissor(X, Y, Width, Height);
  glViewport(X, Y, Width, Height);
  //gluOrtho2D(0, Width, Height, 0);

  glMatrixMode(GL_PROJECTION);

  mat[ 0] := 2.0 / Width;
  mat[ 1] := 0.0;
  mat[ 2] := 0.0;
  mat[ 3] := 0.0;

  mat[ 4] := 0.0;
  mat[ 5] := -2.0 / Height;
  mat[ 6] := 0.0;
  mat[ 7] := 0.0;

  mat[ 8] := 0.0;
  mat[ 9] := 0.0;
  mat[10] := 1.0;
  mat[11] := 0.0;

  mat[12] := -1.0;
  mat[13] := 1.0;
  mat[14] := 0.0;
  mat[15] := 1.0;

  glLoadMatrixd(@mat[0]);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

//------------------------------------------------------------------
// Ищет свободный элемент в массиве текстур
//------------------------------------------------------------------
function FindTexture(): DWORD;
var
  i: integer;
begin
 if e_Textures <> nil then
 for i := 0 to High(e_Textures) do
  if e_Textures[i].tx.Width = 0 then
  begin
   Result := i;
   Exit;
  end;

 if e_Textures = nil then
 begin
  SetLength(e_Textures, 32);
  Result := 0;
 end
  else
 begin
  Result := High(e_Textures) + 1;
  SetLength(e_Textures, Length(e_Textures) + 32);
 end;
end;

//------------------------------------------------------------------
// Создает текстуру
//------------------------------------------------------------------
function e_CreateTexture(FileName: String; var ID: DWORD): Boolean;
var
  find_id: DWORD;
  fmt:     Word;
begin
 Result := False;

 e_WriteLog('Loading texture from '+FileName, TMsgType.Notify);

 find_id := FindTexture();

 if not LoadTexture(FileName, e_Textures[find_id].tx, e_Textures[find_id].tx.Width,
                    e_Textures[find_id].tx.Height, @fmt) then Exit;

 ID := find_id;

 Result := True;
end;

function e_CreateTextureEx(FileName: String; var ID: DWORD; fX, fY, fWidth, fHeight: Word): Boolean;
var
  find_id: DWORD;
  fmt:     Word;
begin
 Result := False;

 find_id := FindTexture();

 if not LoadTextureEx(FileName, e_Textures[find_id].tx, fX, fY, fWidth, fHeight, @fmt) then exit;

 ID := find_id;

 Result := True;
end;

function e_CreateTextureMem(pData: Pointer; dataSize: LongInt; var ID: DWORD): Boolean;
var
  find_id: DWORD;
  fmt:     Word;
begin
 Result := False;

 find_id := FindTexture;

 if not LoadTextureMem(pData, dataSize, e_Textures[find_id].tx, e_Textures[find_id].tx.Width, e_Textures[find_id].tx.Height, @fmt) then exit;

 id := find_id;

 Result := True;
end;

function e_CreateTextureMemEx(pData: Pointer; dataSize: LongInt; var ID: DWORD; fX, fY, fWidth, fHeight: Word): Boolean;
var
  find_id: DWORD;
  fmt:     Word;
begin
 Result := False;

 find_id := FindTexture();

 if not LoadTextureMemEx(pData, dataSize, e_Textures[find_id].tx, fX, fY, fWidth, fHeight, @fmt) then exit;

 ID := find_id;

 Result := True;
end;

function e_CreateTextureImg (var img: TImageData; var ID: DWORD): Boolean;
var
  find_id: DWORD;
  fmt, tw, th: Word;
begin
  result := false;
  find_id := FindTexture();
  if not LoadTextureImg(img, e_Textures[find_id].tx, tw, th, @fmt) then exit;
  ID := find_id;
  result := True;
end;

procedure e_GetTextureSize(ID: DWORD; Width, Height: PWord);
begin
 if Width <> nil then Width^ := e_Textures[ID].tx.Width;
 if Height <> nil then Height^ := e_Textures[ID].tx.Height;
end;

procedure e_ResizeFramebuffer(Width, Height: Integer);
begin
{$IF NOT DEFINED(HEADLESS) AND NOT DEFINED(USE_GLES1)}
  if e_NoGraphics then Exit;

  glBindTexture(GL_TEXTURE_2D, 0);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  if e_Frame > 0 then
  begin
    glDeleteTextures(1, @e_Frame);
    e_Frame := 0;
  end;

  if e_RBO > 0 then
  begin
    glDeleteRenderbuffers(1, @e_RBO);
    e_RBO := 0;
  end;

  if e_FBO > 0 then
  begin
    glDeleteFramebuffers(1, @e_FBO);
    e_FBO := 0;
  end;

  e_FrameW := Width;
  e_FrameH := Height;

  glGenFramebuffers(1, @e_FBO);

  glGenTextures(1, @e_Frame);
  glBindTexture(GL_TEXTURE_2D, e_Frame);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, Width, Height, 0, GL_RGB, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

  glGenRenderbuffers(1, @e_RBO);
  glBindRenderbuffer(GL_RENDERBUFFER, e_RBO);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8, Width, Height);

  glBindFramebuffer(GL_FRAMEBUFFER, e_FBO);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, e_Frame, 0);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER, e_RBO);
{$ENDIF}
end;

procedure e_ResizeWindow(Width, Height: Integer);
begin
  if Height = 0 then
    Height := 1;
  e_SetViewPort(0, 0, Width, Height);
end;

procedure drawTxQuad (x0, y0, w, h, tw, th: Integer; u, v: single; Mirror: TMirrorType);
var
  x1, y1, tmp: Integer;
begin
  if (w < 1) or (h < 1) then exit;
  x1 := x0+w;
  y1 := y0+h;
       if Mirror = TMirrorType.Horizontal then begin tmp := x1; x1 := x0; x0 := tmp; end
  else if Mirror = TMirrorType.Vertical then begin tmp := y1; y1 := y0; y0 := tmp; end;
  //HACK: make texture one pixel shorter, so it won't wrap
  if (g_dbg_scale <> 1.0) then
  begin
    u := u*tw/(tw+1);
    v := v*th/(th+1);
  end;
  glTexCoord2f(0, v); glVertex2i(x0, y0);
  glTexCoord2f(0, 0); glVertex2i(x0, y1);
  glTexCoord2f(u, 0); glVertex2i(x1, y1);
  glTexCoord2f(u, v); glVertex2i(x1, y0);
end;

procedure e_BlitFramebuffer(WinWidth, WinHeight: Integer);
begin
{$IF NOT DEFINED(HEADLESS) AND NOT DEFINED(USE_GLES1)}
  if (e_FBO = 0) or (e_Frame = 0) or e_NoGraphics then exit;
  glDisable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glBindTexture(GL_TEXTURE_2D, e_Frame);
  glColor4ub(255, 255, 255, 255);
  e_SetViewPort(0, 0, WinWidth, WinHeight);
  glBegin(GL_QUADS);
  drawTxQuad(0, 0, WinWidth, WinHeight, e_FrameW, e_FrameH, 1, 1, TMirrorType.None);
  glEnd();
  glBindFramebuffer(GL_FRAMEBUFFER, e_FBO);
  e_SetViewPort(0, 0, e_FrameW, e_FrameH);
{$ENDIF}
end;

procedure e_Draw(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                 Blending: Boolean; Mirror: TMirrorType = TMirrorType.None);
begin
  if e_NoGraphics then Exit;
  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);

  if (Alpha > 0) or (AlphaChannel) or (Blending) then
    glEnable(GL_BLEND)
  else
    glDisable(GL_BLEND);

  if (AlphaChannel) or (Alpha > 0) then
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  if Alpha > 0 then
    glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255-Alpha);

  if Blending then
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, e_Textures[ID].tx.id);
  glBegin(GL_QUADS);

  drawTxQuad(X, Y, e_Textures[id].tx.width, e_Textures[id].tx.height, e_Textures[id].tx.width, e_Textures[id].tx.height, e_Textures[ID].tx.u, e_Textures[ID].tx.v, Mirror);

  //u := e_Textures[ID].tx.u;
  //v := e_Textures[ID].tx.v;

  {
  if Mirror = M_NONE then
    begin
      glTexCoord2f(u,  0); glVertex2i(X + e_Textures[id].tx.Width, Y);
      glTexCoord2f(0,  0); glVertex2i(X,                        Y);
      glTexCoord2f(0, -v); glVertex2i(X,                        Y + e_Textures[id].tx.Height);
      glTexCoord2f(u, -v); glVertex2i(X + e_Textures[id].tx.Width, Y + e_Textures[id].tx.Height);
    end
  else
    if Mirror = M_HORIZONTAL then
      begin
        glTexCoord2f(u,  0); glVertex2i(X,                        Y);
        glTexCoord2f(0,  0); glVertex2i(X + e_Textures[id].tx.Width, Y);
        glTexCoord2f(0, -v); glVertex2i(X + e_Textures[id].tx.Width, Y + e_Textures[id].tx.Height);
        glTexCoord2f(u, -v); glVertex2i(X,                        Y + e_Textures[id].tx.Height);
      end
    else
      if Mirror = M_VERTICAL then
      begin
        glTexCoord2f(u, -v); glVertex2i(X + e_Textures[id].tx.Width, Y);
        glTexCoord2f(0, -v); glVertex2i(X,                        Y);
        glTexCoord2f(0,  0); glVertex2i(X,                        Y + e_Textures[id].tx.Height);
        glTexCoord2f(u,  0); glVertex2i(X + e_Textures[id].tx.Width, Y + e_Textures[id].tx.Height);
      end;
  }

  glEnd();

  glDisable(GL_BLEND);
end;

procedure e_DrawSize(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                     Blending: Boolean; Width, Height: Word; Mirror: TMirrorType = TMirrorType.None);
var
  u, v: Single;
begin
  if e_NoGraphics then Exit;
  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);

  if (Alpha > 0) or (AlphaChannel) or (Blending) then
    glEnable(GL_BLEND)
  else
    glDisable(GL_BLEND);

  if (AlphaChannel) or (Alpha > 0) then
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  if Alpha > 0 then
    glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255-Alpha);

  if Blending then
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, e_Textures[ID].tx.id);

  u := e_Textures[ID].tx.u;
  v := e_Textures[ID].tx.v;

  glBegin(GL_QUADS);
    glTexCoord2f(0, v); glVertex2i(X,         Y);
    glTexCoord2f(u, v); glVertex2i(X + Width, Y);
    glTexCoord2f(u, 0); glVertex2i(X + Width, Y + Height);
    glTexCoord2f(0, 0); glVertex2i(X,         Y + Height);
  glEnd();

  glDisable(GL_BLEND);
end;

procedure e_DrawSizeMirror(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                           Blending: Boolean; Width, Height: Word; Mirror: TMirrorType = TMirrorType.None);
begin
  if e_NoGraphics then Exit;
  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);

  if (Alpha > 0) or (AlphaChannel) or (Blending) then
    glEnable(GL_BLEND)
  else
    glDisable(GL_BLEND);

  if (AlphaChannel) or (Alpha > 0) then
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  if Alpha > 0 then
    glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255-Alpha);

  if Blending then
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, e_Textures[ID].tx.id);
  glBegin(GL_QUADS);
  drawTxQuad(X, Y, Width, Height, e_Textures[id].tx.width, e_Textures[id].tx.height, e_Textures[ID].tx.u, e_Textures[ID].tx.v, Mirror);
  glEnd();

  glDisable(GL_BLEND);
end;

procedure e_DrawFill(ID: DWORD; X, Y: Integer; XCount, YCount: Word; Alpha: Integer;
                     AlphaChannel: Boolean; Blending: Boolean; ambientBlendMode: Boolean=false);
var
  X2, Y2, dx, w, h: Integer;
  u, v: Single;
begin
  if e_NoGraphics then Exit;
  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);
  ambientBlendMode := false;

  if (Alpha > 0) or AlphaChannel or Blending then
  begin
    glEnable(GL_BLEND);
  end
  else
  begin
    if not ambientBlendMode then glDisable(GL_BLEND);
  end;
  if AlphaChannel or (Alpha > 0) then glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  if (Alpha > 0) then glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255-Alpha);
  if Blending then glBlendFunc(GL_SRC_ALPHA, GL_ONE);

  if (XCount = 0) then XCount := 1;
  if (YCount = 0) then YCount := 1;

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, e_Textures[ID].tx.id);

  X2 := X+e_Textures[ID].tx.width*XCount;
  Y2 := Y+e_Textures[ID].tx.height*YCount;

  //k8: this SHOULD work... i hope
  if (e_Textures[ID].tx.width = e_Textures[ID].tx.glwidth) and (e_Textures[ID].tx.height = e_Textures[ID].tx.glheight) then
  begin
    glBegin(GL_QUADS);
      glTexCoord2i(0,      YCount); glVertex2i(X,  Y);
      glTexCoord2i(XCount, YCount); glVertex2i(X2, Y);
      glTexCoord2i(XCount, 0);      glVertex2i(X2, Y2);
      glTexCoord2i(0,      0);      glVertex2i(X,  Y2);
    glEnd();
  end
  else
  begin
    glBegin(GL_QUADS);
    // hard day's night
    u := e_Textures[ID].tx.u;
    v := e_Textures[ID].tx.v;
    w := e_Textures[ID].tx.width;
    h := e_Textures[ID].tx.height;
    while YCount > 0 do
    begin
      dx := XCount;
      x2 := X;
      while dx > 0 do
      begin
        glTexCoord2f(0, v); glVertex2i(X,  Y);
        glTexCoord2f(u, v); glVertex2i(X+w, Y);
        glTexCoord2f(u, 0); glVertex2i(X+w, Y+h);
        glTexCoord2f(0, 0); glVertex2i(X,  Y+h);
        Inc(X, w);
        Dec(dx);
      end;
      X := x2;
      Inc(Y, h);
      Dec(YCount);
    end;
    glEnd();
  end;

  glDisable(GL_BLEND);
end;


//TODO: overflow checks
function intersectRect (var x0, y0, w0, h0: Integer; const x1, y1, w1, h1: Integer): Boolean;
var
  ex0, ey0: Integer;
begin
  result := false;
  if (w0 < 1) or (h0 < 1) or (w1 < 1) or (h1 < 1) then exit;
  // check for intersection
  if (x0+w0 <= x1) or (y0+h0 <= y1) or (x1+w1 <= x0) or (y1+h1 <= y0) then exit;
  if (x0 >= x1+w1) or (y0 >= y1+h1) or (x1 >= x0+h0) or (y1 >= y0+h0) then exit;
  // ok, intersects
  ex0 := x0+w0;
  ey0 := y0+h0;
  if (x0 < x1) then x0 := x1;
  if (y0 < y1) then y0 := y1;
  if (ex0 > x1+w1) then ex0 := x1+w1;
  if (ey0 > y1+h1) then ey0 := y1+h1;
  w0 := ex0-x0;
  h0 := ey0-y0;
  result := (w0 > 0) and (h0 > 0);
end;


procedure e_DrawFillX (id: DWORD; x, y, wdt, hgt: Integer; alpha: Integer; alphachannel: Boolean;
                       blending: Boolean; scale: Single; ambientBlendMode: Boolean=false);
var
  x2, y2: Integer;
  {
  wassc: Boolean;
  scxywh: array[0..3] of GLint;
  vpxywh: array[0..3] of GLint;
  }
  w, h, dw, cw, ch, yofs: Integer;
  u, v, cu, cv: Single;
  onlyOneY: Boolean;

  {
  procedure setScissorGLInternal (x, y, w, h: Integer);
  begin
    //if not scallowed then exit;
    x := trunc(x*scale);
    y := trunc(y*scale);
    w := trunc(w*scale);
    h := trunc(h*scale);
    y := vpxywh[3]-(y+h);
    if not intersectRect(x, y, w, h, scxywh[0], scxywh[1], scxywh[2], scxywh[3]) then
    begin
      glScissor(0, 0, 0, 0);
    end
    else
    begin
      //writeln('  (', x, ',', y, ')-(', w, ',', h, ')');
      glScissor(x, y, w, h);
    end;
  end;
  }

begin
  if e_NoGraphics then exit;
  ambientBlendMode := false;

  if (wdt < 1) or (hgt < 1) then exit;

  if (wdt mod e_Textures[ID].tx.width = 0) and (hgt mod e_Textures[ID].tx.height = 0) then
  begin
    e_DrawFill(id, x, y, wdt div e_Textures[ID].tx.width, hgt div e_Textures[ID].tx.height, alpha, alphachannel, blending, ambientBlendMode);
    exit;
  end;

  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);

  if (Alpha > 0) or AlphaChannel or Blending then
  begin
    glEnable(GL_BLEND);
  end
  else
  begin
    if not ambientBlendMode then glDisable(GL_BLEND);
  end;
  if AlphaChannel or (Alpha > 0) then glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  if (Alpha > 0) then glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255-Alpha);
  if Blending then glBlendFunc(GL_SRC_ALPHA, GL_ONE);

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, e_Textures[ID].tx.id);

  x2 := x+wdt;
  y2 := y+hgt;

  //k8: this SHOULD work... i hope
  if {false and} (e_Textures[ID].tx.width = e_Textures[ID].tx.glwidth) and (e_Textures[ID].tx.height = e_Textures[ID].tx.glheight) then
  begin
    glBegin(GL_QUADS);
      glTexCoord2f(0, hgt/e_Textures[ID].tx.height); glVertex2i(x,  y);
      glTexCoord2f(wdt/e_Textures[ID].tx.width, hgt/e_Textures[ID].tx.height); glVertex2i(x2, y);
      glTexCoord2f(wdt/e_Textures[ID].tx.width, 0); glVertex2i(x2, y2);
      glTexCoord2f(0, 0); glVertex2i(x, y2);
    glEnd();
  end
  else
  begin
    // hard day's night; setup scissor
    {
    glGetIntegerv(GL_VIEWPORT, @vpxywh[0]);
    wassc := (glIsEnabled(GL_SCISSOR_TEST) <> 0);
    if wassc then glGetIntegerv(GL_SCISSOR_BOX, @scxywh[0]) else glGetIntegerv(GL_VIEWPORT, @scxywh[0]);
    //writeln('(', scxywh[0], ',', scxywh[1], ')-(', scxywh[2], ',', scxywh[3], ')');
    //glEnable(GL_SCISSOR_TEST);
    setScissorGLInternal(x, y, wdt, hgt);
    }
    // draw quads
    u := e_Textures[ID].tx.u;
    v := e_Textures[ID].tx.v;
    w := e_Textures[ID].tx.width;
    h := e_Textures[ID].tx.height;
    x2 := x;
    if (hgt > h) then begin y += hgt-h; onlyOneY := false; end else onlyOneY := true;
    glBegin(GL_QUADS);
    while (hgt > 0) do
    begin
      if (hgt >= h) then begin ch := h; cv := v; yofs := 0; end else begin ch := hgt; cv := v/(h/hgt); yofs := h-hgt; end;
      if onlyOneY then yofs := 0;
      Dec(hgt, h);
      dw := wdt;
      x := x2;
      while (dw > 0) do
      begin
        if (dw >= w) then begin cw := w; cu := u; end else begin cw := dw; cu := u/(w/dw); end;
        Dec(dw, w);
        glTexCoord2f(0, cv); glVertex2i(X,   Y+yofs);
        glTexCoord2f(cu, cv); glVertex2i(X+cw, Y+yofs);
        glTexCoord2f(cu, 0); glVertex2i(X+cw, Y+ch+yofs);
        glTexCoord2f(0, 0); glVertex2i(X,   Y+ch+yofs);
        Inc(X, w);
      end;
      Dec(Y, h);
    end;
    glEnd();
    //if wassc then glEnable(GL_SCISSOR_TEST) else glDisable(GL_SCISSOR_TEST);
  end;

  glDisable(GL_BLEND);
end;


procedure e_AmbientQuad (x, y, w, h: Integer; r, g, b, a: Byte);
begin
  if e_NoGraphics then exit;
  if (w < 1) or (h < 1) then exit;
  if (a <> 255) or ((r or g or b) <> 0) then
  begin
    glEnable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
    glColor4ub(r, g, b, a);
    if ((r or g or b) <> 0) then
    begin
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glBegin(GL_QUADS);
        glVertex2i(x, y);
        glVertex2i(x+w, y);
        glVertex2i(x+w, y+h);
        glVertex2i(x, y+h);
      glEnd();
    end;
    glBlendFunc(GL_ZERO, GL_SRC_ALPHA);
    glBegin(GL_QUADS);
      glVertex2i(x, y);
      glVertex2i(x+w, y);
      glVertex2i(x+w, y+h);
      glVertex2i(x, y+h);
    glEnd();
    glDisable(GL_BLEND);
  end;
end;


procedure e_DrawAdv(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                    Blending: Boolean; Angle: Single; RC: PDFPoint; Mirror: TMirrorType = TMirrorType.None);
begin
  if e_NoGraphics then Exit;

  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);

  if (Alpha > 0) or (AlphaChannel) or (Blending) then
    glEnable(GL_BLEND)
  else
    glDisable(GL_BLEND);

  if (AlphaChannel) or (Alpha > 0) then
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  if Alpha > 0 then
    glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255-Alpha);

  if Blending then
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);

  if (Angle <> 0) and (RC <> nil) then
  begin
    glPushMatrix();
    glTranslatef(X+RC.X, Y+RC.Y, 0);
    glRotatef(Angle, 0, 0, 1);
    glTranslatef(-(X+RC.X), -(Y+RC.Y), 0);
  end;

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, e_Textures[id].tx.id);
  glBegin(GL_QUADS);                           //0-1        1-1
                                               //00         10
  drawTxQuad(X, Y, e_Textures[id].tx.width, e_Textures[id].tx.height, e_Textures[id].tx.width, e_Textures[id].tx.height, e_Textures[ID].tx.u, e_Textures[ID].tx.v, Mirror);
  glEnd();

  if Angle <> 0 then
    glPopMatrix();

  glDisable(GL_BLEND);
end;

procedure e_DrawPoint(Size: Byte; X, Y: Integer; Red, Green, Blue: Byte);
begin
  if e_NoGraphics then Exit;
  glDisable(GL_TEXTURE_2D);
  glColor3ub(Red, Green, Blue);
  glPointSize(Size);

  if (Size = 2) or (Size = 4) then
    X := X + 1;

  glBegin(GL_POINTS);
    glVertex2f(X+0.3, Y+1.0);
  glEnd();

  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);
end;

procedure e_LineCorrection(var X1, Y1, X2, Y2: Integer);
begin
  // Make lines only top-left/bottom-right and top-right/bottom-left
  if Y2 < Y1 then
  begin
    X1 := X1 xor X2;
    X2 := X1 xor X2;
    X1 := X1 xor X2;

    Y1 := Y1 xor Y2;
    Y2 := Y1 xor Y2;
    Y1 := Y1 xor Y2;
  end;

  // Pixel-perfect hack
  if X1 < X2 then
    Inc(X2)
  else
    Inc(X1);
  Inc(Y2);
end;

procedure e_DrawQuad(X1, Y1, X2, Y2: Integer; Red, Green, Blue: Byte; Alpha: Byte = 0);
var
  nX1, nY1, nX2, nY2: Integer;
begin
  if e_NoGraphics then Exit;
  // Only top-left/bottom-right quad
  if X1 > X2 then
  begin
    X1 := X1 xor X2;
    X2 := X1 xor X2;
    X1 := X1 xor X2;
  end;
  if Y1 > Y2 then
  begin
    Y1 := Y1 xor Y2;
    Y2 := Y1 xor Y2;
    Y1 := Y1 xor Y2;
  end;

  if Alpha > 0 then
  begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end
  else
    glDisable(GL_BLEND);

  glDisable(GL_TEXTURE_2D);
  glColor4ub(Red, Green, Blue, 255-Alpha);
  glLineWidth(1);
  glBegin(GL_LINES);
    nX1 := X1; nY1 := Y1;
    nX2 := X2; nY2 := Y1;
    e_LineCorrection(nX1, nY1, nX2, nY2); // Pixel-perfect lines
    glVertex2i(nX1, nY1);
    glVertex2i(nX2, nY2);

    nX1 := X2; nY1 := Y1;
    nX2 := X2; nY2 := Y2;
    e_LineCorrection(nX1, nY1, nX2, nY2);
    glVertex2i(nX1, nY1);
    glVertex2i(nX2, nY2);

    nX1 := X2; nY1 := Y2;
    nX2 := X1; nY2 := Y2;
    e_LineCorrection(nX1, nY1, nX2, nY2);
    glVertex2i(nX1, nY1);
    glVertex2i(nX2, nY2);

    nX1 := X1; nY1 := Y2;
    nX2 := X1; nY2 := Y1;
    e_LineCorrection(nX1, nY1, nX2, nY2);
    glVertex2i(nX1, nY1);
    glVertex2i(nX2, nY2);
  glEnd();
  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);
  glDisable(GL_BLEND);
end;

procedure e_DrawFillQuad(X1, Y1, X2, Y2: Integer; Red, Green, Blue, Alpha: Byte;
                         Blending: TBlending = TBlending.None);
begin
  if e_NoGraphics then Exit;
  if (Alpha > 0) or (Blending <> TBlending.None) then
    glEnable(GL_BLEND)
  else
    glDisable(GL_BLEND);

  if Blending = TBlending.Blend then
    glBlendFunc(GL_SRC_ALPHA, GL_ONE)
  else
    if Blending = TBlending.Filter then
      glBlendFunc(GL_DST_COLOR, GL_SRC_COLOR)
    else
      if Blending = TBlending.Invert then
        glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ZERO)
      else
        if Alpha > 0 then
          glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glDisable(GL_TEXTURE_2D);
  glColor4ub(Red, Green, Blue, 255-Alpha);

  X2 := X2 + 1;
  Y2 := Y2 + 1;

  glBegin(GL_QUADS);
    glVertex2i(X1, Y1);
    glVertex2i(X2, Y1);
    glVertex2i(X2, Y2);
    glVertex2i(X1, Y2);
  glEnd();

  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);

  glDisable(GL_BLEND);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure e_DarkenQuad (x0, y0, x1, y1: Integer; a: Integer);
begin
  if (a < 0) then a := 0;
  if (a > 255) then a := 255;
  glEnable(GL_BLEND);
  glBlendFunc(GL_ZERO, GL_SRC_ALPHA);
  glDisable(GL_TEXTURE_2D);
  glColor4ub(0, 0, 0, Byte(255-a));
  glBegin(GL_QUADS);
    glVertex2i(x0, y0);
    glVertex2i(x1, y0);
    glVertex2i(x1, y1);
    glVertex2i(x0, y1);
  glEnd();
  //glRect(x, y, x+w, y+h);
  glColor4ub(1, 1, 1, 1);
  glDisable(GL_BLEND);
  //glBlendEquation(GL_FUNC_ADD);
end;

procedure e_DarkenQuadWH (x, y, w, h: Integer; a: Integer);
begin
  if (w > 0) and (h > 0) then e_DarkenQuad(x, y, x+w, y+h, a);
end;


procedure e_DrawLine(Width: Byte; X1, Y1, X2, Y2: Integer; Red, Green, Blue: Byte; Alpha: Byte = 0);
begin
  if e_NoGraphics then Exit;
  // Pixel-perfect lines
  if Width = 1 then
    e_LineCorrection(X1, Y1, X2, Y2);

  if Alpha > 0 then
  begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end else
    glDisable(GL_BLEND);

  glDisable(GL_TEXTURE_2D);
  glColor4ub(Red, Green, Blue, 255-Alpha);
  glLineWidth(Width);
  glBegin(GL_LINES);
    glVertex2i(X1, Y1);
    glVertex2i(X2, Y2);
  glEnd();
  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);

  glDisable(GL_BLEND);
end;

//------------------------------------------------------------------
// Удаляет текстуру из массива
//------------------------------------------------------------------
procedure e_DeleteTexture(ID: DWORD);
begin
  if not e_NoGraphics then
    glDeleteTextures(1, @e_Textures[ID].tx.id);
  e_Textures[ID].tx.id := 0;
  e_Textures[ID].tx.Width := 0;
  e_Textures[ID].tx.Height := 0;
end;

//------------------------------------------------------------------
// Удаляет все текстуры
//------------------------------------------------------------------
procedure e_RemoveAllTextures();
var
  i: integer;
begin
 if e_Textures = nil then Exit;

 for i := 0 to High(e_Textures) do
  if e_Textures[i].tx.Width <> 0 then e_DeleteTexture(i);
 e_Textures := nil;
end;

//------------------------------------------------------------------
// Удаляет движок
//------------------------------------------------------------------
procedure e_ReleaseEngine();
begin
 e_RemoveAllTextures;
 e_RemoveAllTextureFont;
end;

procedure e_BeginRender();
begin
  if e_NoGraphics then Exit;
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GREATER, 0.0);
end;

procedure e_Clear(Mask: TGLbitfield; Red, Green, Blue: Single); overload;
begin
  if e_NoGraphics then Exit;
  glClearColor(Red, Green, Blue, 0);
  glClear(Mask);
end;

procedure e_Clear(); overload;
begin
  if e_NoGraphics then Exit;
  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT);
end;

procedure e_EndRender();
begin
  if e_NoGraphics then Exit;
  glPopMatrix();
end;

{$IFDEF USE_SDL2}
function e_GetGamma(win: PSDL_Window): Byte;
var
  ramp: array [0..256*3-1] of Word;
  rgb: array [0..2] of Double;
  sum: double;
  count: integer;
  min: integer;
  max: integer;
  A, B: double;
  i, j: integer;
begin
 Result := 0;
 if e_NoGraphics then Exit;
 rgb[0] := 1.0;
 rgb[1] := 1.0;
 rgb[2] := 1.0;

 SDL_GetWindowGammaRamp(win, @ramp[0], @ramp[256], @ramp[512]);

 for i := 0 to 2 do
 begin
  sum := 0;
  count := 0;
  min := 256 * i;
  max := min + 256;

  for j := min to max - 1 do
  if ramp[j] > 0 then
  begin
   B := (j mod 256)/256;
   A := ramp[j]/65536;
   sum := sum + ln(A)/ln(B);
   inc(count);
  end;
  rgb[i] := sum / count;
 end;

 Result := 100 - Trunc(((rgb[0] + rgb[1] + rgb[2])/3 - 0.23) * 100/(2.7 - 0.23));
end;

procedure e_SetGamma(win: PSDL_Window; Gamma: Byte);
var
  ramp: array [0..256*3-1] of Word;
  i: integer;
  r: double;
  g: double;
begin
 if e_NoGraphics then Exit;
 g := (100 - Gamma)*(2.7 - 0.23)/100 + 0.23;

 for i := 0 to 255 do
 begin
  r := Exp(g * ln(i/256))*65536;
  if r < 0 then r := 0
   else if r > 65535 then r := 65535;
  ramp[i] := trunc(r);
  ramp[i + 256] := trunc(r);
  ramp[i + 512] := trunc(r);
 end;

 SDL_SetWindowGammaRamp(win, @ramp[0], @ramp[256], @ramp[512]);
end;
{$ENDIF}

function e_CharFont_Create(sp: ShortInt=0): DWORD;
var
  i, id: DWORD;
begin
 e_WriteLog('Creating CharFont...', TMsgType.Notify);

 id := DWORD(-1);

 if e_CharFonts <> nil then
 for i := 0 to High(e_CharFonts) do
  if not e_CharFonts[i].alive then
  begin
   id := i;
   Break;
  end;

 if id = DWORD(-1) then
 begin
  SetLength(e_CharFonts, Length(e_CharFonts) + 1);
  id := High(e_CharFonts);
 end;

 with e_CharFonts[id] do
 begin
  for i := 0 to High(Chars) do
   with Chars[i] do
   begin
    TextureID := -1;
    Width := 0;
   end;

  Space := sp;
  alive := True;
 end;

 Result := id;
end;

procedure e_CharFont_AddChar(FontID: DWORD; Texture: Integer; c: Char; w: Byte);
begin
 with e_CharFonts[FontID].Chars[Ord(c)] do
 begin
  TextureID := Texture;
  Width := w;
 end;
end;

procedure e_CharFont_Print(FontID: DWORD; X, Y: Integer; Text: string);
var
  a: Integer;
begin
 if e_NoGraphics then Exit;
 if Text = '' then Exit;
 if e_CharFonts = nil then Exit;
 if Integer(FontID) > High(e_CharFonts) then Exit;

 with e_CharFonts[FontID] do
 begin
  for a := 1 to Length(Text) do
   with Chars[Ord(Text[a])] do
   if TextureID <> -1 then
   begin
    e_Draw(TextureID, X, Y, 0, True, False);
    X := X+Width+IfThen(a = Length(Text), 0, Space);
   end;
 end;
end;

procedure e_CharFont_PrintEx(FontID: DWORD; X, Y: Integer; Text: string;
                             Color: TRGB; Scale: Single = 1.0);
var
  a: Integer;
  c: TRGB;
begin
 if e_NoGraphics then Exit;
 if Text = '' then Exit;
 if e_CharFonts = nil then Exit;
 if Integer(FontID) > High(e_CharFonts) then Exit;

 with e_CharFonts[FontID] do
 begin
  for a := 1 to Length(Text) do
   with Chars[Ord(Text[a])] do
   if TextureID <> -1 then
   begin
    if Scale <> 1.0 then
    begin
     glPushMatrix;
     glScalef(Scale, Scale, 0);
    end;

    c := e_Colors;
    e_Colors := Color;
    e_Draw(TextureID, X, Y, 0, True, False);
    e_Colors := c;

    if Scale <> 1.0 then glPopMatrix;

    X := X+Width+IfThen(a = Length(Text), 0, Space);
   end;
 end;
end;

procedure e_CharFont_PrintFmt(FontID: DWORD; X, Y: Integer; Text: string);
var
  a, TX, TY, len: Integer;
  tc, c: TRGB;
  w, h: Word;
begin
  if e_NoGraphics then Exit;
  if Text = '' then Exit;
  if e_CharFonts = nil then Exit;
  if Integer(FontID) > High(e_CharFonts) then Exit;

  c.R := 255;
  c.G := 255;
  c.B := 255;

  TX := X;
  TY := Y;
  len := Length(Text);

  e_CharFont_GetSize(FontID, 'A', w, h);

  with e_CharFonts[FontID] do
  begin
    for a := 1 to len do
    begin
      case Text[a] of
        #10: // line feed
        begin
          TX := X;
          TY := TY + h;
          continue;
        end;
        #1: // black
        begin
          c.R := 0; c.G := 0; c.B := 0;
          continue;
        end;
        #2: // white
        begin
          c.R := 255; c.G := 255; c.B := 255;
          continue;
        end;
        #3: // darker
        begin
          c.R := c.R div 2; c.G := c.G div 2; c.B := c.B div 2;
          continue;
        end;
        #4: // lighter
        begin
          c.R := Min(c.R * 2, 255); c.G := Min(c.G * 2, 255); c.B := Min(c.B * 2, 255);
          continue;
        end;
        #18: // red
        begin
          c.R := 255; c.G := 0; c.B := 0;
          continue;
        end;
        #19: // green
        begin
          c.R := 0; c.G := 255; c.B := 0;
          continue;
        end;
        #20: // blue
        begin
          c.R := 0; c.G := 0; c.B := 255;
          continue;
        end;
        #21: // yellow
        begin
          c.R := 255; c.G := 255; c.B := 0;
          continue;
        end;
      end;

      with Chars[Ord(Text[a])] do
      if TextureID <> -1 then
      begin
        tc := e_Colors;
        e_Colors := c;
        e_Draw(TextureID, TX, TY, 0, True, False);
        e_Colors := tc;

        TX := TX+Width+IfThen(a = Length(Text), 0, Space);
      end;
    end;
  end;
end;

procedure e_CharFont_GetSize(FontID: DWORD; Text: string; var w, h: Word);
var
  a: Integer;
  h2: Word;
begin
 w := 0;
 h := 0;

 if Text = '' then Exit;
 if e_CharFonts = nil then Exit;
 if Integer(FontID) > High(e_CharFonts) then Exit;

 with e_CharFonts[FontID] do
 begin
  for a := 1 to Length(Text) do
   with Chars[Ord(Text[a])] do
   if TextureID <> -1 then
   begin
    w := w+Width+IfThen(a = Length(Text), 0, Space);
    e_GetTextureSize(TextureID, nil, @h2);
    if h2 > h then h := h2;
   end;
 end;
end;

procedure e_CharFont_GetSizeFmt(FontID: DWORD; Text: string; var w, h: Word);
var
  a, lines, len: Integer;
  h2, w2, tw, th: Word;
begin
  w2 := 0;
  h2 := 0;
  tw := 0;
  th := 0;

  if Text = '' then Exit;
  if e_CharFonts = nil then Exit;
  if Integer(FontID) > High(e_CharFonts) then Exit;

  lines := 1;
  len := Length(Text);

  with e_CharFonts[FontID] do
  begin
    for a := 1 to len do
    begin
      if Text[a] = #10 then
      begin
        Inc(lines);
        if w2 > tw then tw := w2;
        w2 := 0;
        continue;
      end;

      with Chars[Ord(Text[a])] do
        if TextureID <> -1 then
        begin
          w2 := w2 + Width + IfThen(a = len, 0, Space);
          e_GetTextureSize(TextureID, nil, @h2);
          if h2 > th then th := h2;
        end;
    end;
  end;

  if w2 > tw then
    tw := w2;

  w := tw;
  h := th * lines;
end;

function e_CharFont_GetMaxWidth(FontID: DWORD): Word;
var
  a: Integer;
begin
 Result := 0;

 if e_CharFonts = nil then Exit;
 if Integer(FontID) > High(e_CharFonts) then Exit;

 for a := 0 to High(e_CharFonts[FontID].Chars) do
  Result := Max(Result, e_CharFonts[FontID].Chars[a].Width);
end;

function e_CharFont_GetMaxHeight(FontID: DWORD): Word;
var
  a: Integer;
  h2: Word;
begin
 Result := 0;

 if e_CharFonts = nil then Exit;
 if Integer(FontID) > High(e_CharFonts) then Exit;

 for a := 0 to High(e_CharFonts[FontID].Chars) do
 begin
  if e_CharFonts[FontID].Chars[a].TextureID <> -1 then
   e_GetTextureSize(e_CharFonts[FontID].Chars[a].TextureID, nil, @h2)
    else h2 := 0;
  if h2 > Result then Result := h2;
 end;
end;

procedure e_CharFont_Remove(FontID: DWORD);
var
  a: Integer;
begin
 with e_CharFonts[FontID] do
  for a := 0 to High(Chars) do
   if Chars[a].TextureID <> -1 then e_DeleteTexture(Chars[a].TextureID);

 e_CharFonts[FontID].alive := False;
end;

procedure e_CharFont_RemoveAll();
var
  a: Integer;
begin
 if e_CharFonts = nil then Exit;

 for a := 0 to High(e_CharFonts) do
  e_CharFont_Remove(a);

 e_CharFonts := nil;
end;

procedure e_TextureFontBuild(Tex: DWORD; var FontID: DWORD; XCount, YCount: Word;
                             Space: ShortInt=0);
var
{$IFDEF NOGL_LISTS}
  loop1 : GLuint;
  cx, cy : real;
{$ENDIF}
  i, id: DWORD;
begin
 if e_NoGraphics then Exit;
 e_WriteLog('Creating texture font...', TMsgType.Notify);

 id := DWORD(-1);

 if e_TextureFonts <> nil then
 for i := 0 to High(e_TextureFonts) do
  if e_TextureFonts[i].Base = 0 then
  begin
   id := i;
   Break;
  end;

 if id = DWORD(-1) then
 begin
  SetLength(e_TextureFonts, Length(e_TextureFonts) + 1);
  id := High(e_TextureFonts);
 end;

 with e_TextureFonts[id] do
 begin
{$IFDEF NOGL_LISTS}
  Base := glGenLists(XCount*YCount);
{$ENDIF}
  TextureID := e_Textures[Tex].tx.id;
  CharWidth := (e_Textures[Tex].tx.Width div XCount)+Space;
  CharHeight := e_Textures[Tex].tx.Height div YCount;
  XC := XCount;
  YC := YCount;
  Texture := Tex;
  SPC := Space;
 end;

{$IFDEF NOGL_LISTS}
 glBindTexture(GL_TEXTURE_2D, e_Textures[Tex].tx.id);
 for loop1 := 0 to XCount*YCount-1 do
 begin
  cx := (loop1 mod XCount)/XCount;
  cy := (loop1 div YCount)/YCount;

  glNewList(e_TextureFonts[id].Base+loop1, GL_COMPILE);
   glBegin(GL_QUADS);
    glTexCoord2f(cx, 1.0-cy-1/YCount);
    glVertex2i(0, e_Textures[Tex].tx.Height div YCount);

    glTexCoord2f(cx+1/XCount, 1.0-cy-1/YCount);
    glVertex2i(e_Textures[Tex].tx.Width div XCount, e_Textures[Tex].tx.Height div YCount);

    glTexCoord2f(cx+1/XCount, 1.0-cy);
    glVertex2i(e_Textures[Tex].tx.Width div XCount, 0);

    glTexCoord2f(cx, 1.0-cy);
    glVertex2i(0, 0);
   glEnd();
   glTranslated((e_Textures[Tex].tx.Width div XCount)+Space, 0, 0);
  glEndList();
 end;
{$ENDIF}

 FontID := id;
end;

procedure e_TextureFontKill(FontID: DWORD);
begin
  if e_NoGraphics then Exit;
{$IFDEF NOGL_LISTS}
  glDeleteLists(e_TextureFonts[FontID].Base, 256);
{$ENDIF}
  e_TextureFonts[FontID].Base := 0;
end;

{$IFNDEF NOGL_LISTS}
procedure e_TextureFontDrawChar(ch: Char; FontID: DWORD);
  var
    index: Integer;
    cx, cy: GLfloat;
    Tex: Integer;
    Width, Height: Integer;
    XCount, YCount: Integer;
begin
  index := Ord(ch) - 32;
  Tex := e_TextureFonts[FontID].Texture;
  Width := e_Textures[Tex].tx.Width;
  Height := e_Textures[Tex].tx.Height;
  XCount := e_TextureFonts[FontID].XC;
  YCount := e_TextureFonts[FontID].YC;
  cx := (index mod XCount)/XCount;
  cy := (index div YCount)/YCount;
  glBegin(GL_QUADS);
    glTexCoord2f(cx, 1 - cy - 1/YCount);
    glVertex2i(0, Height div YCount);
    glTexCoord2f(cx + 1/XCount, 1 - cy - 1/YCount);
    glVertex2i(Width div XCount, Height div YCount);
    glTexCoord2f(cx + 1/XCount, 1 - cy);
    glVertex2i(Width div XCount, 0);
    glTexCoord2f(cx, 1 - cy);
    glVertex2i(0, 0);
  glEnd();
  glTranslatef((e_Textures[Tex].tx.Width div XCount) + e_TextureFonts[FontID].SPC, 0, 0);
end;

procedure e_TextureFontDrawString(Text: String; FontID: DWORD);
  var
    i: Integer;
begin
  for i := 1 to High(Text) do
    e_TextureFontDrawChar(Text[i], FontID);
end;
{$ENDIF}

procedure e_TextureFontPrint(X, Y: GLint; Text: string; FontID: DWORD);
begin
  if e_NoGraphics then Exit;
  if Integer(FontID) > High(e_TextureFonts) then Exit;
  if Text = '' then Exit;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);

  glPushMatrix;
  glBindTexture(GL_TEXTURE_2D, e_TextureFonts[FontID].TextureID);
  glEnable(GL_TEXTURE_2D);
  glTranslatef(x, y, 0);
{$IFDEF NOGL_LISTS}
  glListBase(DWORD(Integer(e_TextureFonts[FontID].Base)-32));
  glCallLists(Length(Text), GL_UNSIGNED_BYTE, PChar(Text));
{$ELSE}
  e_TextureFontDrawString(Text, FontID);
{$ENDIF}
  glDisable(GL_TEXTURE_2D);
  glPopMatrix;

  glDisable(GL_BLEND);
end;

// god forgive me for this, but i cannot figure out how to do it without lists
procedure e_TextureFontPrintChar(X, Y: Integer; Ch: Char; FontID: DWORD; Shadow: Boolean = False);
begin
  if e_NoGraphics then Exit;
  glPushMatrix;

  if Shadow then
  begin
   glColor4ub(0, 0, 0, 128);
   glTranslatef(X+1, Y+1, 0);
{$IFDEF NOGL_LISTS}
   glCallLists(1, GL_UNSIGNED_BYTE, @Ch);
{$ELSE}
   e_TextureFontDrawChar(Ch, FontID);
{$ENDIF}
   glPopMatrix;
   glPushMatrix;
  end;

  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);
  glTranslatef(X, Y, 0);
{$IFDEF NOGL_LISTS}
  glCallLists(1, GL_UNSIGNED_BYTE, @Ch);
{$ELSE}
  e_TextureFontDrawChar(Ch, FontID);
{$ENDIF}

  glPopMatrix;
end;

procedure e_TextureFontPrintCharEx (X, Y: Integer; Ch: Char; FontID: DWORD; Shadow: Boolean = False);
begin
  glBindTexture(GL_TEXTURE_2D, e_TextureFonts[FontID].TextureID);
  glEnable(GL_TEXTURE_2D);
  //glListBase(DWORD(Integer(e_TextureFonts[FontID].Base)-32));

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  e_TextureFontPrintChar(X, Y, Ch, FontID, Shadow);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
end;

function e_TextureFontCharWidth (ch: Char; FontID: DWORD): Integer;
begin
  result := e_TextureFonts[FontID].CharWidth;
end;

procedure e_TextureFontPrintFmt(X, Y: GLint; Text: string; FontID: DWORD;
                                Shadow: Boolean = False; Newlines: Boolean = False);
var
  a, TX, TY, len: Integer;
  tc, c: TRGB;
  w, h: Word;
begin
  if e_NoGraphics then Exit;
  if Text = '' then Exit;
  if e_TextureFonts = nil then Exit;
  if Integer(FontID) > High(e_TextureFonts) then Exit;

  c.R := 255;
  c.G := 255;
  c.B := 255;

  TX := X;
  TY := Y;
  len := Length(Text);

  w := e_TextureFonts[FontID].CharWidth;
  h := e_TextureFonts[FontID].CharHeight;

  with e_TextureFonts[FontID] do
  begin
    glBindTexture(GL_TEXTURE_2D, e_TextureFonts[FontID].TextureID);
    glEnable(GL_TEXTURE_2D);

{$IFDEF NOGL_LISTS}
    glListBase(DWORD(Integer(e_TextureFonts[FontID].Base)-32));
{$ENDIF}

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);

    for a := 1 to len do
    begin
      case Text[a] of
        #10: // line feed
        begin
          if Newlines then
          begin
            TX := X;
            TY := TY + h;
            continue;
          end;
        end;
        #1: // black
        begin
          c.R := 0; c.G := 0; c.B := 0;
          continue;
        end;
        #2: // white
        begin
          c.R := 255; c.G := 255; c.B := 255;
          continue;
        end;
        #3: // darker
        begin
          c.R := c.R div 2; c.G := c.G div 2; c.B := c.B  div 2;
          continue;
        end;
        #4: // lighter
        begin
          c.R := Min(c.R * 2, 255); c.G := Min(c.G * 2, 255); c.B := Min(c.B * 2, 255);
          continue;
        end;
        #18: // red
        begin
          c.R := 255; c.G := 0; c.B := 0;
          continue;
        end;
        #19: // green
        begin
          c.R := 0; c.G := 255; c.B := 0;
          continue;
        end;
        #20: // blue
        begin
          c.R := 0; c.G := 0; c.B := 255;
          continue;
        end;
        #21: // yellow
        begin
          c.R := 255; c.G := 255; c.B := 0;
          continue;
        end;
      end;

      tc := e_Colors;
      e_Colors := c;
      e_TextureFontPrintChar(TX, TY, Text[a], FontID, Shadow);
      e_Colors := tc;

      TX := TX+w;
    end;
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
  end;
end;

procedure e_TextureFontPrintEx(X, Y: GLint; Text: string; FontID: DWORD; Red, Green,
                    Blue: Byte; Scale: Single; Shadow: Boolean = False);
begin
  if e_NoGraphics then Exit;
  if Text = '' then Exit;

  glPushMatrix;
  glBindTexture(GL_TEXTURE_2D, e_TextureFonts[FontID].TextureID);
  glEnable(GL_TEXTURE_2D);

{$IFDEF NOGL_LISTS}
  glListBase(DWORD(Integer(e_TextureFonts[FontID].Base)-32));
{$ENDIF}

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  if Shadow then
  begin
   glColor4ub(0, 0, 0, 128);
   glTranslatef(x+1, y+1, 0);
   glScalef(Scale, Scale, 0);
{$IFDEF NOGL_LISTS}
   glCallLists(Length(Text), GL_UNSIGNED_BYTE, PChar(Text));
{$ELSE}
   e_TextureFontDrawString(Text, FontID);
{$ENDIF}
   glPopMatrix;
   glPushMatrix;
  end;

  glColor4ub(Red, Green, Blue, 255);
  glTranslatef(x, y, 0);
  glScalef(Scale, Scale, 0);
{$IFDEF NOGL_LISTS}
  glCallLists(Length(Text), GL_UNSIGNED_BYTE, PChar(Text));
{$ELSE}
  e_TextureFontDrawString(Text, FontID);
{$ENDIF}

  glDisable(GL_TEXTURE_2D);
  glPopMatrix;
  glColor3ub(e_Colors.R, e_Colors.G, e_Colors.B);
  glDisable(GL_BLEND);
end;

procedure e_TextureFontGetSize(ID: DWORD; out CharWidth, CharHeight: Byte);
begin
  CharWidth := 16;
  CharHeight := 16;
  if e_NoGraphics then Exit;
  if Integer(ID) > High(e_TextureFonts) then
    Exit;
  CharWidth := e_TextureFonts[ID].CharWidth;
  CharHeight := e_TextureFonts[ID].CharHeight;
end;

procedure e_RemoveAllTextureFont();
var
 i: integer;
begin
 if e_NoGraphics then Exit;
 if e_TextureFonts = nil then Exit;

 for i := 0 to High(e_TextureFonts) do
  if e_TextureFonts[i].Base <> 0 then
  begin
{$IFDEF NOGL_LISTS}
   glDeleteLists(e_TextureFonts[i].Base, 256);
{$ENDIF}
   e_TextureFonts[i].Base := 0;
  end;

 e_TextureFonts := nil;
end;

function _RGB(Red, Green, Blue: Byte): TRGB;
begin
 Result.R := Red;
 Result.G := Green;
 Result.B := Blue;
end;

function _Point(X, Y: Integer): TPoint2i;
begin
 Result.X := X;
 Result.Y := Y;
end;

function _Rect(X, Y: Integer; Width, Height: Word): TRectWH;
begin
 Result.X := X;
 Result.Y := Y;
 Result.Width := Width;
 Result.Height := Height;
end;

function _TRect(L, T, R, B: LongInt): TRect;
begin
 Result.Top := T;
 Result.Left := L;
 Result.Right := R;
 Result.Bottom := B;
end;


procedure e_MakeScreenshot (st: TStream; Width, Height: Word);
var
  pixels, obuf, scln, ps, pd: PByte;
  obufsize: Integer;
  dlen: Cardinal;
  i, x, y, res: Integer;
  sign: array [0..7] of Byte;
  hbuf: array [0..12] of Byte;
  crc: LongWord;
  img: TImageData;
  clr: TColor32Rec;
begin
  if e_NoGraphics then Exit;
  obuf := nil;

  // first, extract and pack graphics data
  if (Width mod 4) > 0 then Width := Width+4-(Width mod 4);

  GetMem(pixels, Width*Height*3);
  try
    FillChar(pixels^, Width*Height*3, 0);
    glReadPixels(0, 0, Width, Height, GL_RGB, GL_UNSIGNED_BYTE, pixels);
    //e_WriteLog('PNG: pixels read', MSG_NOTIFY);

    if e_FastScreenshots then
    begin
      // create scanlines
      GetMem(scln, (Width*3+1)*Height);
      try
        ps := pixels;
        pd := scln;
        Inc(ps, (Width*3)*(Height-1));
        for i := 0 to Height-1 do
        begin
          pd^ := 0; // filter
          Inc(pd);
          Move(ps^, pd^, Width*3);
          Dec(ps, Width*3);
          Inc(pd, Width*3);
        end;
      except
        FreeMem(scln);
        raise;
      end;
      FreeMem(pixels);
      pixels := scln;

      // pack it
      obufsize := (Width*3+1)*Height*2;
      GetMem(obuf, obufsize);
      try
        while true do
        begin
          dlen := obufsize;
          res := compress2(Pointer(obuf), dlen, Pointer(pixels), (Width*3+1)*Height, 9);
          if res = Z_OK then break;
          if res <> Z_BUF_ERROR then raise Exception.Create('can''t pack data for PNG');
          obufsize := obufsize*2;
          FreeMem(obuf);
          obuf := nil;
          GetMem(obuf, obufsize);
        end;
        //e_WriteLog(Format('PNG: pixels compressed from %d to %d', [Integer(Width*Height*3), Integer(dlen)]), MSG_NOTIFY);

        // now write PNG

        // signature
        sign[0] := 137;
        sign[1] := 80;
        sign[2] := 78;
        sign[3] := 71;
        sign[4] := 13;
        sign[5] := 10;
        sign[6] := 26;
        sign[7] := 10;
        st.writeBuffer(sign, 8);
        //e_WriteLog('PNG: signature written', MSG_NOTIFY);

        // header
        writeIntBE(st, LongWord(13));
        sign[0] := 73;
        sign[1] := 72;
        sign[2] := 68;
        sign[3] := 82;
        st.writeBuffer(sign, 4);
        crc := crc32(0, @sign[0], 4);
        hbuf[0] := 0;
        hbuf[1] := 0;
        hbuf[2] := (Width shr 8) and $ff;
        hbuf[3] := Width and $ff;
        hbuf[4] := 0;
        hbuf[5] := 0;
        hbuf[6] := (Height shr 8) and $ff;
        hbuf[7] := Height and $ff;
        hbuf[8] := 8; // bit depth
        hbuf[9] := 2; // RGB
        hbuf[10] := 0; // compression method
        hbuf[11] := 0; // filter method
        hbuf[12] := 0; // no interlace
        crc := crc32(crc, @hbuf[0], 13);
        st.writeBuffer(hbuf, 13);
        writeIntBE(st, crc);
        //e_WriteLog('PNG: header written', MSG_NOTIFY);

        // image data
        writeIntBE(st, LongWord(dlen));
        sign[0] := 73;
        sign[1] := 68;
        sign[2] := 65;
        sign[3] := 84;
        st.writeBuffer(sign, 4);
        crc := crc32(0, @sign[0], 4);
        crc := crc32(crc, obuf, dlen);
        st.writeBuffer(obuf^, dlen);
        writeIntBE(st, crc);
        //e_WriteLog('PNG: image data written', MSG_NOTIFY);

        // image data end
        writeIntBE(st, LongWord(0));
        sign[0] := 73;
        sign[1] := 69;
        sign[2] := 78;
        sign[3] := 68;
        st.writeBuffer(sign, 4);
        crc := crc32(0, @sign[0], 4);
        writeIntBE(st, crc);
        //e_WriteLog('PNG: end marker written', MSG_NOTIFY);
      finally
        if obuf <> nil then FreeMem(obuf);
      end;
    end
    else
    begin
      Imaging.SetOption(ImagingPNGCompressLevel, 9);
      Imaging.SetOption(ImagingPNGPreFilter, 6);
      InitImage(img);
      try
        NewImage(Width, Height, TImageFormat.ifR8G8B8, img);
        ps := pixels;
        //writeln(stderr, 'moving pixels...');
        for y := Height-1 downto 0 do
        begin
          for x := 0 to Width-1 do
          begin
            clr.r := ps^; Inc(ps);
            clr.g := ps^; Inc(ps);
            clr.b := ps^; Inc(ps);
            clr.a := 255;
            SetPixel32(img, x, y, clr);
          end;
        end;
        GlobalMetadata.ClearMetaItems();
        GlobalMetadata.ClearMetaItemsForSaving();
        //writeln(stderr, 'compressing image...');
        if not SaveImageToStream('png', st, img) then raise Exception.Create('screenshot writing error');
        //writeln(stderr, 'done!');
      finally
        FreeImage(img);
      end;
    end;
  finally
    FreeMem(pixels);
  end;
end;


end.
