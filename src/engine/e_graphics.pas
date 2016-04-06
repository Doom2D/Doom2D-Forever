unit e_graphics;

interface

uses
  SysUtils, Math, e_log, e_textures, SDL, GL, GLExt, MAPDEF;

type
  TMirrorType=(M_NONE, M_HORIZONTAL, M_VERTICAL);
  TBlending=(B_NONE, B_BLEND, B_FILTER, B_INVERT);

  TPoint2i = record
    X, Y: Integer;
  end;

  TPoint = MAPDEF.TPoint; // TODO: create an utiltypes.pas or something
                          //       for other types like rect as well

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

  PPoint = ^TPoint;
  PPoint2f = ^TPoint2f;
  PRect = ^TRect;
  PRectWH = ^TRectWH;


//------------------------------------------------------------------
// прототипы функций
//------------------------------------------------------------------
procedure e_InitGL();
procedure e_SetViewPort(X, Y, Width, Height: Word);
procedure e_ResizeWindow(Width, Height: Integer);

procedure e_Draw(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                 Blending: Boolean; Mirror: TMirrorType = M_NONE);
procedure e_DrawAdv(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                    Blending: Boolean; Angle: Single; RC: PPoint; Mirror: TMirrorType = M_NONE);
procedure e_DrawSize(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                     Blending: Boolean; Width, Height: Word; Mirror: TMirrorType = M_NONE);
procedure e_DrawSizeMirror(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                           Blending: Boolean; Width, Height: Word; Mirror: TMirrorType = M_NONE);
procedure e_DrawFill(ID: DWORD; X, Y: Integer; XCount, YCount: Word; Alpha: Integer;
                     AlphaChannel: Boolean; Blending: Boolean);
procedure e_DrawPoint(Size: Byte; X, Y: Integer; Red, Green, Blue: Byte);
procedure e_DrawLine(Width: Byte; X1, Y1, X2, Y2: Integer; Red, Green, Blue: Byte; Alpha: Byte = 0);
procedure e_DrawQuad(X1, Y1, X2, Y2: Integer; Red, Green, Blue: Byte; Alpha: Byte = 0);
procedure e_DrawFillQuad(X1, Y1, X2, Y2: Integer; Red, Green, Blue, Alpha: Byte;
                         Blending: TBlending = B_NONE);

function e_CreateTexture(FileName: string; var ID: DWORD): Boolean;
function e_CreateTextureEx(FileName: string; var ID: DWORD; fX, fY, fWidth, fHeight: Word): Boolean;
function e_CreateTextureMem(pData: Pointer; var ID: DWORD): Boolean;
function e_CreateTextureMemEx(pData: Pointer; var ID: DWORD; fX, fY, fWidth, fHeight: Word): Boolean;
procedure e_GetTextureSize(ID: DWORD; Width, Height: PWord);
function e_GetTextureSize2(ID: DWORD): TRectWH;
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
procedure e_TextureFontPrintFmt(X, Y: GLint; Text: string; FontID: DWORD; Shadow: Boolean = False);
procedure e_TextureFontGetSize(ID: DWORD; var CharWidth, CharHeight: Byte);
procedure e_RemoveAllTextureFont();

procedure e_ReleaseEngine();
procedure e_BeginRender();
procedure e_Clear(Mask: TGLbitfield; Red, Green, Blue: Single); overload;
procedure e_Clear(); overload;
procedure e_EndRender();

procedure e_SaveGLContext();
procedure e_RestoreGLContext();

function e_GetGamma(): Byte;
procedure e_SetGamma(Gamma: Byte);

procedure e_MakeScreenshot(FileName: string; Width, Height: Word);

function _RGB(Red, Green, Blue: Byte): TRGB;
function _Point(X, Y: Integer): TPoint2i;
function _Rect(X, Y: Integer; Width, Height: Word): TRectWH;
function _TRect(L, T, R, B: LongInt): TRect;


var
  e_Colors: TRGB;

implementation

type
  TTexture = record
   ID:     DWORD;
   Width:  Word;
   Height: Word;
   Fmt:    Word;
  end;

  TTextureFont = record
   Texture:     DWORD;
   TextureID:   DWORD;
   Base:        Uint32;
   CharWidth:   Byte;
   CharHeight:  Byte;
   XC, YC, SPC: Word;
  end;

  TCharFont = record
   Chars: array[0..255] of
    record
     TextureID: Integer;
     Width: Byte;
    end;
   Space: ShortInt;
   Height: ShortInt;
   Live: Boolean;
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
  e_SavedTextures: array of TSavedTexture;

//------------------------------------------------------------------
// Инициализирует OpenGL
//------------------------------------------------------------------
procedure e_InitGL();
begin
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_SCISSOR_TEST);
  e_Colors.R := 255;
  e_Colors.G := 255;
  e_Colors.B := 255;
  glClearColor(0, 0, 0, 0);
end;

procedure e_SetViewPort(X, Y, Width, Height: Word);
var
  mat: Array [0..15] of GLDouble;

begin
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
  if e_Textures[i].Width = 0 then
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

 e_WriteLog('Loading texture from '+FileName, MSG_NOTIFY);

 find_id := FindTexture();

 if not LoadTexture(FileName, e_Textures[find_id].ID, e_Textures[find_id].Width,
                    e_Textures[find_id].Height, @fmt) then Exit;

 ID := find_id;
 e_Textures[ID].Fmt := fmt;

 Result := True;
end;

function e_CreateTextureEx(FileName: String; var ID: DWORD; fX, fY, fWidth, fHeight: Word): Boolean;
var
  find_id: DWORD;
  fmt:     Word;
begin
 Result := False;

 find_id := FindTexture();

 if not LoadTextureEx(FileName, e_Textures[find_id].ID, fX, fY, fWidth, fHeight, @fmt) then exit;

 e_Textures[find_id].Width := fWidth;
 e_Textures[find_id].Height := fHeight;
 e_Textures[find_id].Fmt := fmt;

 ID := find_id;

 Result := True;
end;

function e_CreateTextureMem(pData: Pointer; var ID: DWORD): Boolean;
var
  find_id: DWORD;
  fmt:     Word;
begin
 Result := False;

 find_id := FindTexture;

 if not LoadTextureMem(pData, e_Textures[find_id].ID, e_Textures[find_id].Width,
                   e_Textures[find_id].Height, @fmt) then exit;

 id := find_id;
 e_Textures[id].Fmt := fmt;

 Result := True;
end;

function e_CreateTextureMemEx(pData: Pointer; var ID: DWORD; fX, fY, fWidth, fHeight: Word): Boolean;
var
  find_id: DWORD;
  fmt:     Word;
begin
 Result := False;

 find_id := FindTexture();

 if not LoadTextureMemEx(pData, e_Textures[find_id].ID, fX, fY, fWidth, fHeight, @fmt) then exit;

 e_Textures[find_id].Width := fWidth;
 e_Textures[find_id].Height := fHeight;
 e_Textures[find_id].Fmt := fmt;

 ID := find_id;

 Result := True;
end;

procedure e_GetTextureSize(ID: DWORD; Width, Height: PWord);
begin
 if Width <> nil then Width^ := e_Textures[ID].Width;
 if Height <> nil then Height^ := e_Textures[ID].Height;
end;

function e_GetTextureSize2(ID: DWORD): TRectWH;
var
  data: PChar;
  x, y: Integer;
  w, h: Word;
  a: Boolean;
  lastline: Integer;
begin
 w := e_Textures[ID].Width;
 h := e_Textures[ID].Height;
 data := GetMemory(w*h*4);
 glEnable(GL_TEXTURE_2D);
 glBindTexture(GL_TEXTURE_2D, e_Textures[ID].ID);
 glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, data);

 Result.Y := 0;
 Result.X := 0;
 Result.Width := w;
 Result.Height := h;

 for y := h-1 downto 0 do
 begin
  lastline := y;
  a := True;

  for x := 1 to w-4 do
  begin
   a := Byte((data+y*w*4+x*4+3)^) <> 0;
   if a then Break;
  end;

  if a then
  begin
   Result.Y := h-lastline;
   Break;
  end;
 end;

 for y := 0 to h-1 do
 begin
  lastline := y;
  a := True;

  for x := 1 to w-4 do
  begin
   a := Byte((data+y*w*4+x*4+3)^) <> 0;
   if a then Break;
  end;

  if a then
  begin
   Result.Height := h-lastline-Result.Y;
   Break;
  end;
 end;

 for x := 0 to w-1 do
 begin
  lastline := x;
  a := True;

  for y := 1 to h-4 do
  begin
   a := Byte((data+y*w*4+x*4+3)^) <> 0;
   if a then Break;
  end;

  if a then
  begin
   Result.X := lastline+1;
   Break;
  end;
 end;

 for x := w-1 downto 0 do
 begin
  lastline := x;
  a := True;

  for y := 1 to h-4 do
  begin
   a := Byte((data+y*w*4+x*4+3)^) <> 0;
   if a then Break;
  end;

  if a then
  begin
   Result.Width := lastline-Result.X+1;
   Break;
  end;
 end;

 FreeMemory(data);
end;

procedure e_ResizeWindow(Width, Height: Integer);
begin
  if Height = 0 then
    Height := 1;
  e_SetViewPort(0, 0, Width, Height);
end;

procedure e_Draw(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                 Blending: Boolean; Mirror: TMirrorType = M_NONE);
begin
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
  glBindTexture(GL_TEXTURE_2D, e_Textures[ID].ID);
  glBegin(GL_QUADS);

  if Mirror = M_NONE then
    begin
      glTexCoord2i(1,  0); glVertex2i(X + e_Textures[id].Width, Y);
      glTexCoord2i(0,  0); glVertex2i(X,                        Y);
      glTexCoord2i(0, -1); glVertex2i(X,                        Y + e_Textures[id].Height);
      glTexCoord2i(1, -1); glVertex2i(X + e_Textures[id].Width, Y + e_Textures[id].Height);
    end
  else
    if Mirror = M_HORIZONTAL then
      begin
        glTexCoord2i(1,  0); glVertex2i(X,                        Y);
        glTexCoord2i(0,  0); glVertex2i(X + e_Textures[id].Width, Y);
        glTexCoord2i(0, -1); glVertex2i(X + e_Textures[id].Width, Y + e_Textures[id].Height);
        glTexCoord2i(1, -1); glVertex2i(X,                        Y + e_Textures[id].Height);
      end
    else
      if Mirror = M_VERTICAL then
      begin
        glTexCoord2i(1, -1); glVertex2i(X + e_Textures[id].Width, Y);
        glTexCoord2i(0, -1); glVertex2i(X,                        Y);
        glTexCoord2i(0,  0); glVertex2i(X,                        Y + e_Textures[id].Height);
        glTexCoord2i(1,  0); glVertex2i(X + e_Textures[id].Width, Y + e_Textures[id].Height);
      end;

  glEnd();

  glDisable(GL_BLEND);
end;

procedure e_DrawSize(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                     Blending: Boolean; Width, Height: Word; Mirror: TMirrorType = M_NONE);
begin
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
  glBindTexture(GL_TEXTURE_2D, e_Textures[ID].ID);

  glBegin(GL_QUADS);
    glTexCoord2i(0, 1); glVertex2i(X,         Y);
    glTexCoord2i(1, 1); glVertex2i(X + Width, Y);
    glTexCoord2i(1, 0); glVertex2i(X + Width, Y + Height);
    glTexCoord2i(0, 0); glVertex2i(X,         Y + Height);
  glEnd();

  glDisable(GL_BLEND);
end;

procedure e_DrawSizeMirror(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                           Blending: Boolean; Width, Height: Word; Mirror: TMirrorType = M_NONE);
begin
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
  glBindTexture(GL_TEXTURE_2D, e_Textures[ID].ID);
  glBegin(GL_QUADS);

  if Mirror = M_NONE then
    begin
      glTexCoord2i(1,  0); glVertex2i(X + Width, Y);
      glTexCoord2i(0,  0); glVertex2i(X,         Y);
      glTexCoord2i(0, -1); glVertex2i(X,         Y + Height);
      glTexCoord2i(1, -1); glVertex2i(X + Width, Y + Height);
    end
  else
    if Mirror = M_HORIZONTAL then
      begin
        glTexCoord2i(1,  0); glVertex2i(X,         Y);
        glTexCoord2i(0,  0); glVertex2i(X + Width, Y);
        glTexCoord2i(0, -1); glVertex2i(X + Width, Y + Height);
        glTexCoord2i(1, -1); glVertex2i(X,         Y + Height);
      end
    else
      if Mirror = M_VERTICAL then
      begin
        glTexCoord2i(1, -1); glVertex2i(X + Width, Y);
        glTexCoord2i(0, -1); glVertex2i(X,         Y);
        glTexCoord2i(0,  0); glVertex2i(X,         Y + Height);
        glTexCoord2i(1,  0); glVertex2i(X + Width, Y + Height);
      end;

  glEnd();

  glDisable(GL_BLEND);
end;

procedure e_DrawFill(ID: DWORD; X, Y: Integer; XCount, YCount: Word; Alpha: Integer;
                     AlphaChannel: Boolean; Blending: Boolean);
var
  X2, Y2: Integer;

begin
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

  if XCount = 0 then
    XCount := 1;

  if YCount = 0 then
    YCount := 1;

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, e_Textures[ID].ID);

  X2 := X + e_Textures[ID].Width * XCount;
  Y2 := Y + e_Textures[ID].Height * YCount;

  glBegin(GL_QUADS);
    glTexCoord2i(0,      YCount); glVertex2i(X,  Y);
    glTexCoord2i(XCount, YCount); glVertex2i(X2, Y);
    glTexCoord2i(XCount, 0);      glVertex2i(X2, Y2);
    glTexCoord2i(0,      0);      glVertex2i(X,  Y2);
  glEnd();

  glDisable(GL_BLEND);
end;

procedure e_DrawAdv(ID: DWORD; X, Y: Integer; Alpha: Byte; AlphaChannel: Boolean;
                    Blending: Boolean; Angle: Single; RC: PPoint; Mirror: TMirrorType = M_NONE);
begin
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
  glBindTexture(GL_TEXTURE_2D, e_Textures[id].ID);
  glBegin(GL_QUADS);                           //0-1        1-1
                                               //00         10
  if Mirror = M_NONE then
    begin
      glTexCoord2i(1,  0); glVertex2i(X + e_Textures[id].Width, Y);
      glTexCoord2i(0,  0); glVertex2i(X,                        Y);
      glTexCoord2i(0, -1); glVertex2i(X,                        Y + e_Textures[id].Height);
      glTexCoord2i(1, -1); glVertex2i(X + e_Textures[id].Width, Y + e_Textures[id].Height);
    end
  else
    if Mirror = M_HORIZONTAL then
      begin
        glTexCoord2i(1,  0); glVertex2i(X,                        Y);
        glTexCoord2i(0,  0); glVertex2i(X + e_Textures[id].Width, Y);
        glTexCoord2i(0, -1); glVertex2i(X + e_Textures[id].Width, Y + e_Textures[id].Height);
        glTexCoord2i(1, -1); glVertex2i(X,                        Y + e_Textures[id].Height);
      end
    else
      if Mirror = M_VERTICAL then
      begin
        glTexCoord2i(1, -1); glVertex2i(X + e_Textures[id].Width, Y);
        glTexCoord2i(0, -1); glVertex2i(X,                        Y);
        glTexCoord2i(0,  0); glVertex2i(X,                        Y + e_Textures[id].Height);
        glTexCoord2i(1,  0); glVertex2i(X + e_Textures[id].Width, Y + e_Textures[id].Height);
      end;

  glEnd();

  if Angle <> 0 then
    glPopMatrix();

  glDisable(GL_BLEND);
end;

procedure e_DrawPoint(Size: Byte; X, Y: Integer; Red, Green, Blue: Byte);
begin
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
  end else
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
                         Blending: TBlending = B_NONE);
begin
  if (Alpha > 0) or (Blending <> B_NONE) then
    glEnable(GL_BLEND)
  else
    glDisable(GL_BLEND);

  if Blending = B_BLEND then
    glBlendFunc(GL_SRC_ALPHA, GL_ONE)
  else
    if Blending = B_FILTER then
      glBlendFunc(GL_DST_COLOR, GL_SRC_COLOR)
    else
      if Blending = B_INVERT then
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

procedure e_DrawLine(Width: Byte; X1, Y1, X2, Y2: Integer; Red, Green, Blue: Byte; Alpha: Byte = 0);
begin
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
  glDeleteTextures(1, @e_Textures[ID].ID);
  e_Textures[ID].ID := 0;
  e_Textures[ID].Width := 0;
  e_Textures[ID].Height := 0;
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
  if e_Textures[i].Width <> 0 then e_DeleteTexture(i);
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
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GREATER, 0.0);
end;

procedure e_Clear(Mask: TGLbitfield; Red, Green, Blue: Single); overload;
begin
 glClearColor(Red, Green, Blue, 0);
 glClear(Mask);
end;

procedure e_Clear(); overload;
begin
 glClearColor(0, 0, 0, 0);
 glClear(GL_COLOR_BUFFER_BIT);
end;

procedure e_EndRender();
begin
  glPopMatrix();
end;

procedure e_MakeScreenshot(FileName: String; Width, Height: Word);
begin
end;

{type
  aRGB  = Array [0..1] of TRGB;
  PaRGB = ^aRGB;

  TByteArray = Array [0..1] of Byte;
  PByteArray = ^TByteArray;

var
  FILEHEADER: BITMAPFILEHEADER;
  INFOHEADER: BITMAPINFOHEADER;
  pixels: PByteArray;
  tmp:    Byte;
  i:      Integer;
  F:      File of Byte;

begin
  if (Width mod 4) > 0 then
    Width := Width + 4 - (Width mod 4);

  GetMem(pixels, Width*Height*3);
  glReadPixels(0, 0, Width, Height, GL_RGB, GL_UNSIGNED_BYTE, pixels);

  for i := 0 to Width * Height - 1 do
    with PaRGB(pixels)[i] do
    begin
      tmp := R;
      R := B;
      B := tmp;
    end;

  with FILEHEADER do
  begin
    bfType := $4D42; // "BM"
    bfSize := Width*Height*3 + SizeOf(BITMAPFILEHEADER) + SizeOf(BITMAPINFOHEADER);
    bfReserved1 := 0;
    bfReserved2 := 0;
    bfOffBits := SizeOf(BITMAPFILEHEADER) + SizeOf(BITMAPINFOHEADER);
  end;

  with INFOHEADER do
  begin
    biSize := SizeOf(BITMAPINFOHEADER);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := 24;
    biCompression := 0;
    biSizeImage := Width*Height*3;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrUsed := 0;
    biClrImportant := 0;
  end;

  AssignFile(F, FileName);
  Rewrite(F);

  BlockWrite(F, FILEHEADER, SizeOf(FILEHEADER));
  BlockWrite(F, INFOHEADER, SizeOf(INFOHEADER));
  BlockWrite(F, pixels[0], Width*Height*3);

  CloseFile(F);

  FreeMem(pixels);
end;}

function e_GetGamma(): Byte;
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
 rgb[0] := 1.0;
 rgb[1] := 1.0;
 rgb[2] := 1.0;

 SDL_GetGammaRamp(@ramp[0], @ramp[256], @ramp[512]);

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

procedure e_SetGamma(Gamma: Byte);
var
  ramp: array [0..256*3-1] of Word;
  i: integer;
  r: double;
  g: double;
begin
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

 SDL_SetGammaRamp(@ramp[0], @ramp[256], @ramp[512]);
end;

function e_CharFont_Create(sp: ShortInt=0): DWORD;
var
  i, id: DWORD;
begin
 e_WriteLog('Creating CharFont...', MSG_NOTIFY);

 id := DWORD(-1);

 if e_CharFonts <> nil then
 for i := 0 to High(e_CharFonts) do
  if not e_CharFonts[i].Live then
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
  Live := True;
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
  h2, w2: Word;
begin
  w2 := 0;
  w := 0;
  h := 0;

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
        if w2 > w then
        begin
          w := w2;
          w2 := 0;
        end;
        continue;
      end
      else if Text[a] in [#1, #2, #3, #4, #18, #19, #20, #21] then
        continue;

      with Chars[Ord(Text[a])] do
      if TextureID <> -1 then
      begin
        w2 := w2 + Width + IfThen(a = len, 0, Space);
        e_GetTextureSize(TextureID, nil, @h2);
        if h2 > h then h := h2;
      end;
    end;
  end;

  if w2 > w then
    w := w2;
  h := h * lines;
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

 e_CharFonts[FontID].Live := False;
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
  loop1 : GLuint;
  cx, cy : real;
  i, id: DWORD;
begin
 e_WriteLog('Creating texture font...', MSG_NOTIFY);

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
  Base := glGenLists(XCount*YCount);
  TextureID := e_Textures[Tex].ID;
  CharWidth := (e_Textures[Tex].Width div XCount)+Space;
  CharHeight := e_Textures[Tex].Height div YCount;
  XC := XCount;
  YC := YCount;
  Texture := Tex;
  SPC := Space;
 end;

 glBindTexture(GL_TEXTURE_2D, e_Textures[Tex].ID);
 for loop1 := 0 to XCount*YCount-1 do
 begin
  cx := (loop1 mod XCount)/XCount;
  cy := (loop1 div YCount)/YCount;

  glNewList(e_TextureFonts[id].Base+loop1, GL_COMPILE);
   glBegin(GL_QUADS);
    glTexCoord2f(cx, 1.0-cy-1/YCount);
    glVertex2d(0, e_Textures[Tex].Height div YCount);

    glTexCoord2f(cx+1/XCount, 1.0-cy-1/YCount);
    glVertex2i(e_Textures[Tex].Width div XCount, e_Textures[Tex].Height div YCount);

    glTexCoord2f(cx+1/XCount, 1.0-cy);
    glVertex2i(e_Textures[Tex].Width div XCount, 0);

    glTexCoord2f(cx, 1.0-cy);
    glVertex2i(0, 0);
   glEnd();
   glTranslated((e_Textures[Tex].Width div XCount)+Space, 0, 0);
  glEndList();
 end;

 FontID := id;
end;

procedure e_TextureFontBuildInPlace(id: DWORD);
var
  loop1 : GLuint;
  cx, cy : real;
  XCount, YCount, Space: Integer;
  {i,} Tex: DWORD;
begin
 with e_TextureFonts[id] do
 begin
  Base := glGenLists(XC*YC);
  TextureID := e_Textures[Texture].ID;
  XCount := XC;
  YCount := YC;
  Space := SPC;
  Tex := Texture;
 end;

 glBindTexture(GL_TEXTURE_2D, e_Textures[Tex].ID);
 for loop1 := 0 to XCount*YCount-1 do
 begin
  cx := (loop1 mod XCount)/XCount;
  cy := (loop1 div YCount)/YCount;

  glNewList(e_TextureFonts[id].Base+loop1, GL_COMPILE);
   glBegin(GL_QUADS);
    glTexCoord2f(cx, 1.0-cy-1/YCount);
    glVertex2d(0, e_Textures[Tex].Height div YCount);

    glTexCoord2f(cx+1/XCount, 1.0-cy-1/YCount);
    glVertex2i(e_Textures[Tex].Width div XCount, e_Textures[Tex].Height div YCount);

    glTexCoord2f(cx+1/XCount, 1.0-cy);
    glVertex2i(e_Textures[Tex].Width div XCount, 0);

    glTexCoord2f(cx, 1.0-cy);
    glVertex2i(0, 0);
   glEnd();
   glTranslated((e_Textures[Tex].Width div XCount)+Space, 0, 0);
  glEndList();
 end;
end;

procedure e_TextureFontKill(FontID: DWORD);
begin
  glDeleteLists(e_TextureFonts[FontID].Base, 256);
  e_TextureFonts[FontID].Base := 0;
end;

procedure e_TextureFontPrint(X, Y: GLint; Text: string; FontID: DWORD);
begin
  if Integer(FontID) > High(e_TextureFonts) then Exit;
  if Text = '' then Exit;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);

  glPushMatrix;
  glBindTexture(GL_TEXTURE_2D, e_TextureFonts[FontID].TextureID);
  glEnable(GL_TEXTURE_2D);
  glTranslated(x, y, 0);
  glListBase(DWORD(Integer(e_TextureFonts[FontID].Base)-32));
  glCallLists(Length(Text), GL_UNSIGNED_BYTE, PChar(Text));
  glDisable(GL_TEXTURE_2D);
  glPopMatrix;

  glDisable(GL_BLEND);
end;

// god forgive me for this, but i cannot figure out how to do it without lists
procedure e_TextureFontPrintChar(X, Y: Integer; Ch: Char; FontID: DWORD; Shadow: Boolean = False);
begin
  glPushMatrix;

  if Shadow then
  begin
   glColor4ub(0, 0, 0, 128);
   glTranslated(X+1, Y+1, 0);
   glCallLists(1, GL_UNSIGNED_BYTE, @Ch);
   glPopMatrix;
   glPushMatrix;
  end;

  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);
  glTranslated(X, Y, 0);
  glCallLists(1, GL_UNSIGNED_BYTE, @Ch);

  glPopMatrix;
end;

procedure e_TextureFontPrintFmt(X, Y: Integer; Text: string; FontID: DWORD; Shadow: Boolean = False);
var
  a, TX, TY, len: Integer;
  tc, c: TRGB;
  w: Word;
begin
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

  with e_TextureFonts[FontID] do
  begin
    glBindTexture(GL_TEXTURE_2D, e_TextureFonts[FontID].TextureID);
    glEnable(GL_TEXTURE_2D);
    glListBase(DWORD(Integer(e_TextureFonts[FontID].Base)-32));

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);

    for a := 1 to len do
    begin
      case Text[a] of
        {#10: // line feed
        begin
          TX := X;
          TY := TY + h;
          continue;
        end;}
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
  if Text = '' then Exit;

  glPushMatrix;
  glBindTexture(GL_TEXTURE_2D, e_TextureFonts[FontID].TextureID);
  glEnable(GL_TEXTURE_2D);
  glListBase(DWORD(Integer(e_TextureFonts[FontID].Base)-32));

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  if Shadow then
  begin
   glColor4ub(0, 0, 0, 128);
   glTranslated(x+1, y+1, 0);
   glScalef(Scale, Scale, 0);
   glCallLists(Length(Text), GL_UNSIGNED_BYTE, PChar(Text));
   glPopMatrix;
   glPushMatrix;
  end;

  glColor4ub(Red, Green, Blue, 255);
  glTranslated(x, y, 0);
  glScalef(Scale, Scale, 0);
  glCallLists(Length(Text), GL_UNSIGNED_BYTE, PChar(Text));

  glDisable(GL_TEXTURE_2D);
  glPopMatrix;
  glColor3ub(e_Colors.R, e_Colors.G, e_Colors.B);
  glDisable(GL_BLEND);
end;

procedure e_TextureFontGetSize(ID: DWORD; var CharWidth, CharHeight: Byte);
begin
  if Integer(ID) > High(e_TextureFonts) then
    Exit;
  CharWidth := e_TextureFonts[ID].CharWidth;
  CharHeight := e_TextureFonts[ID].CharHeight;
end;

procedure e_RemoveAllTextureFont();
var
 i: integer;
begin
 if e_TextureFonts = nil then Exit;

 for i := 0 to High(e_TextureFonts) do
  if e_TextureFonts[i].Base <> 0 then
  begin
   glDeleteLists(e_TextureFonts[i].Base, 256);
   e_TextureFonts[i].Base := 0;
  end;

 e_TextureFonts := nil;
end;

procedure e_SaveGLContext();
var
  PxLen: Cardinal;
  i: Integer;
begin
  e_WriteLog('Backing up GL context:', MSG_NOTIFY);

  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS);

  if e_Textures <> nil then
  begin
    e_WriteLog('  Backing up textures...', MSG_NOTIFY);
    SetLength(e_SavedTextures, Length(e_Textures));
    for i := Low(e_Textures) to High(e_Textures) do
    begin
      e_SavedTextures[i].Pixels := nil;
      if e_Textures[i].Width > 0 then
      begin
        with e_SavedTextures[i] do
        begin
          PxLen := 3;
          if e_Textures[i].Fmt = GL_RGBA then Inc(PxLen);
          Pixels := GetMem(PxLen * e_Textures[i].Width * e_Textures[i].Height);
          glBindTexture(GL_TEXTURE_2D, e_Textures[i].ID);
          glGetTexImage(GL_TEXTURE_2D, 0, e_Textures[i].Fmt, GL_UNSIGNED_BYTE, Pixels);
          glBindTexture(GL_TEXTURE_2D, 0);
          OldID := e_Textures[i].ID;
          TexId := i;
        end;
      end;
    end;
  end;

  if e_TextureFonts <> nil then
  begin
    e_WriteLog('  Releasing texturefonts...', MSG_NOTIFY);
    for i := 0 to High(e_TextureFonts) do
      if e_TextureFonts[i].Base <> 0 then
      begin
       glDeleteLists(e_TextureFonts[i].Base, 256);
       e_TextureFonts[i].Base := 0;
      end;
  end;
end;

procedure e_RestoreGLContext();
var
  GLID: GLuint;
  i: Integer;
begin
  e_WriteLog('Restoring GL context:', MSG_NOTIFY);

  glPopClientAttrib();
  glPopAttrib();

  if e_SavedTextures <> nil then
  begin
    e_WriteLog('  Regenerating textures...', MSG_NOTIFY);
    for i := Low(e_SavedTextures) to High(e_SavedTextures) do
    begin
      if e_SavedTextures[i].Pixels <> nil then
        with e_SavedTextures[i] do
        begin
          GLID := CreateTexture(e_Textures[TexID].Width, e_Textures[TexID].Height,
                                e_Textures[TexID].Fmt, Pixels);
          e_Textures[TexID].ID := GLID;
          FreeMem(Pixels);
        end;
    end;
  end;

  if e_TextureFonts <> nil then
  begin
    e_WriteLog('  Regenerating texturefonts...', MSG_NOTIFY);
    for i := Low(e_TextureFonts) to High(e_TextureFonts) do
      with e_TextureFonts[i] do
      begin
        TextureID := e_Textures[Texture].ID;
        Base := 0;
        e_TextureFontBuildInPlace(i);
      end;
  end;

  SetLength(e_SavedTextures, 0);
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

end.
