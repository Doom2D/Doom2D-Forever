unit e_graphics;

interface

uses
  windows, SysUtils, Math, e_log, e_textures, dglOpenGL;

type
  TMirrorType=(M_NONE, M_HORIZONTAL, M_VERTICAL);
  TBlending=(B_NONE, B_BLEND, B_FILTER, B_INVERT);

  TPoint2i = record
    X, Y: Integer;
  end;

  TPoint2f = record
    X, Y: Double;
  end;

  TRect = windows.TRect;

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
procedure e_InitGL(VSync: Boolean);
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
procedure e_DrawLine(Width: Byte; X1, Y1, X2, Y2: Integer; Red, Green, Blue: Byte);
procedure e_DrawQuad(X1, Y1, X2, Y2: Integer; Red, Green, Blue: Byte);
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

// SimpleFont
function e_SimpleFontCreate(FontName: PChar; Size: Byte; Weight: Word; DC: HDC): DWORD;
procedure e_SimpleFontFree(Font: DWORD);
procedure e_SimpleFontPrint(X, Y: SmallInt; Text: PChar; Font: Integer; Red, Green, Blue: Byte);
procedure e_SimpleFontPrintEx(X, Y: SmallInt; Text: PChar; Font: DWORD; Red, Green, Blue,
                      sRed, sGreen, sBlue, sWidth: Byte);

// CharFont
function e_CharFont_Create(sp: ShortInt=0): DWORD;
procedure e_CharFont_AddChar(FontID: DWORD; Texture: Integer; c: Char; w: Byte);
procedure e_CharFont_Print(FontID: DWORD; X, Y: Integer; Text: string);
procedure e_CharFont_PrintEx(FontID: DWORD; X, Y: Integer; Text: string;
                             Color: TRGB; Scale: Single = 1.0);
procedure e_CharFont_GetSize(FontID: DWORD; Text: string; var w, h: Word);
function e_CharFont_GetMaxWidth(FontID: DWORD): Word;
function e_CharFont_GetMaxHeight(FontID: DWORD): Word;
procedure e_CharFont_Remove(FontID: DWORD);
procedure e_CharFont_RemoveAll();

// TextureFont
procedure e_TextureFontBuild(Texture: DWORD; var FontID: DWORD; XCount, YCount: Word;
                             Space: ShortInt=0);
procedure e_TextureFontKill(FontID: DWORD);
procedure e_TextureFontPrint(X, Y: GLint; Text: string; FontID: DWORD);
procedure e_TextureFontPrintFmt(X, Y: GLint; Text: string; FontID: DWORD);
procedure e_TextureFontPrintEx(X, Y: GLint; Text: string; FontID: DWORD; Red, Green,
                               Blue: Byte; Scale: Single; Shadow: Boolean = False);
procedure e_TextureFontGetSize(ID: DWORD; var CharWidth, CharHeight: Byte);
procedure e_RemoveAllTextureFont();

procedure e_ReleaseEngine();
procedure e_BeginRender();
procedure e_Clear(Mask: TGLbitfield; Red, Green, Blue: Single);
procedure e_EndRender();

function e_GetGamma(DC: HDC): Byte;
procedure e_SetGamma(Gamma: Byte; DC: HDC);

procedure e_MakeScreenshot(FileName: string; Width, Height: Word);

function _RGB(Red, Green, Blue: Byte): TRGB;
function _Point(X, Y: Integer): TPoint2i;
function _Rect(X, Y: Integer; Width, Height: Word): TRectWH;


var
  e_Colors: TRGB;

implementation

type
  TTexture = record
   ID:     DWORD;
   Width:  Word;
   Height: Word;
  end;

  TTextureFont = record
   TextureID:  DWORD;
   Base:       GLuint;
   CharWidth:  Byte;
   CharHeight: Byte;
  end;

  TCharFont = record
   Chars: array[0..255] of
    record
     TextureID: Integer;
     Width: Byte;
    end;
   Space: ShortInt;
   Live: Boolean;
  end;

var
  e_Textures: array of TTexture = nil;
  e_TextureFonts: array of TTextureFont = nil;
  e_CharFonts: array of TCharFont;

//------------------------------------------------------------------
// Инициализирует OpenGL
//------------------------------------------------------------------
procedure e_InitGL(VSync: Boolean);
begin
  if VSync then
    wglSwapIntervalEXT(1)
  else
    wglSwapIntervalEXT(0);
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
begin
 Result := False;

 e_WriteLog('Loading texture from '+FileName, MSG_NOTIFY);

 find_id := FindTexture();

 if not LoadTexture(FileName, e_Textures[find_id].ID, e_Textures[find_id].Width,
                    e_Textures[find_id].Height) then Exit;

 ID := find_id;

 Result := True;
end;

function e_CreateTextureEx(FileName: String; var ID: DWORD; fX, fY, fWidth, fHeight: Word): Boolean;
var
  find_id: DWORD;
begin
 Result := False;

 e_WriteLog(Format('Loading texture segment (X:%d Y:%d W:%d H:%d) from %s',
                   [fX, fY, fWidth, fHeight, FileName]), MSG_NOTIFY);

 find_id := FindTexture();

 if not LoadTextureEx(FileName, e_Textures[find_id].ID, fX, fY, fWidth, fHeight) then Exit;

 e_Textures[find_id].Width := fWidth;
 e_Textures[find_id].Height := fHeight;

 ID := find_id;

 Result := True;
end;

function e_CreateTextureMem(pData: Pointer; var ID: DWORD): Boolean;
var
  find_id: DWORD;
begin
 Result := False;

 e_WriteLog('Loading texture from $'+IntToHex(Integer(pData), 8), MSG_NOTIFY);

 find_id := FindTexture;

 if not LoadTextureMem(pData, e_Textures[find_id].ID, e_Textures[find_id].Width,
                   e_Textures[find_id].Height) then Exit;

 id := find_id;

 Result := True;
end;

function e_CreateTextureMemEx(pData: Pointer; var ID: DWORD; fX, fY, fWidth, fHeight: Word): Boolean;
var
  find_id: DWORD;
begin
 Result := False;

 e_WriteLog('Loading texture segment from $'+IntToHex(Integer(pData), 8), MSG_NOTIFY);

 find_id := FindTexture();

 if not LoadTextureMemEx(pData, e_Textures[find_id].ID, fX, fY, fWidth, fHeight) then Exit;

 e_Textures[find_id].Width := fWidth;
 e_Textures[find_id].Height := fHeight;

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
  data: Pointer;
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
   a := Byte(Pointer(Integer(data)+y*w*4+x*4+3)^) <> 0;
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
   a := Byte(Pointer(Integer(data)+y*w*4+x*4+3)^) <> 0;
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
   a := Byte(Pointer(Integer(data)+y*w*4+x*4+3)^) <> 0;
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
   a := Byte(Pointer(Integer(data)+y*w*4+x*4+3)^) <> 0;
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

procedure e_DrawQuad(X1, Y1, X2, Y2: Integer; Red, Green, Blue: Byte);
begin
  glDisable(GL_TEXTURE_2D);
  glColor3ub(Red, Green, Blue);
  glLineWidth(1);

  glBegin(GL_LINES);
    glVertex2f(X1+0.5, Y1+0.5);
    glVertex2f(X2+0.5, Y1+0.5);

    glVertex2f(X2+0.5, Y1+0.5);
    glVertex2f(X2+0.5, Y2+0.5);

    glVertex2f(X2+0.5, Y2+0.5);
    glVertex2f(X1+0.5, Y2+0.5);

    glVertex2f(X1+0.5, Y2+0.5);
    glVertex2f(X1+0.5, Y1+0.5);
  glEnd();

  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);
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

procedure e_DrawLine(Width: Byte; X1, Y1, X2, Y2: Integer; Red, Green, Blue: Byte);
begin
  glDisable(GL_TEXTURE_2D);
  glColor3ub(Red, Green, Blue);
  glLineWidth(Width);

  glBegin(GL_LINES);
    glVertex2i(X1, Y1);
    glVertex2i(X2, Y2);
  glEnd();
  
  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);
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

procedure e_Clear(Mask: TGLbitfield; Red, Green, Blue: Single);
begin
 glClearColor(Red, Green, Blue, 0);
 glClear(Mask);
end;

procedure e_EndRender();
begin
  glPopMatrix();
end;

procedure e_MakeScreenshot(FileName: String; Width, Height: Word);
type
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

  {$R-}
  for i := 0 to Width * Height - 1 do
    with PaRGB(pixels)[i] do
    begin
      tmp := R;
      R := B;
      B := tmp;
    end;
  {$R+}

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
end;

function e_GetGamma(DC: HDC): Byte;
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

 GetDeviceGammaRamp(DC, ramp);

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

procedure e_SetGamma(Gamma: Byte; DC: HDC);
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

 SetDeviceGammaRamp(DC, ramp);
end;

function e_SimpleFontCreate(FontName: PChar; Size: Byte; Weight: Word; DC: HDC): DWORD;
var
 font: HFONT;
begin
 Result := glGenLists(96);                           // Generate enough display lists to hold
 font := CreateFont(-Size,                             // height of font
                    0,                             // average character width
                    0,                             // angle of escapement
		                0,                             // base-line orientation angle
		                Weight,                       // font weight
                    0,			            // italic
		                0,                             // underline
		                0,			            // strikeout
		                RUSSIAN_CHARSET,               // character set
		                OUT_TT_PRECIS,	            // output precision
                    CLIP_DEFAULT_PRECIS,           // clipping precision
                    ANTIALIASED_QUALITY,           // output quality
		                FF_DONTCARE or DEFAULT_PITCH,  // pitch and family
		                FontName);                      // font
 SelectObject(DC, font);                   // Sets the new font as the current font in the device context
 wglUseFontBitmaps(DC, 32, 224, Result); // Creates a set display lists containing the bitmap fonts
end;

procedure e_SimpleFontFree(Font: DWORD);
begin
 glDeleteLists(Font, 223);             // Delete the font display lists, returning used memory
end;

procedure e_SimpleFontPrint(X, Y: SmallInt; Text: PChar; Font: Integer; Red, Green, Blue: Byte);
begin
 glColor3ub(Red, Green, Blue);
 glDisable(GL_TEXTURE_2D);     // Turn off textures, don't want our text textured
 glRasterPos2i(X, Y);                                // Position the Text
 glPushAttrib(GL_LIST_BIT);                          // Save's the current base list
  glListBase(DWORD(Font-32));                              // Set the base list to our character list
  glCallLists(Length(Text), GL_UNSIGNED_BYTE, Text);  // Display the text
 glPopAttrib();                                     // Restore the old base list
end;

procedure e_SimpleFontPrintEx(X, Y: SmallInt; Text: PChar; Font: DWORD; Red, Green, Blue,
                      sRed, sGreen, sBlue, sWidth: Byte);
begin
 e_SimpleFontPrint(X, Y, Text, Font, Red, Green, Blue);
 e_SimpleFontPrint(X+sWidth, Y+sWidth, Text, Font, sRed, sGreen, sBlue);
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
 if FontID > DWORD(High(e_CharFonts)) then Exit;

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
                             Color: TRGB; Scale: SIngle = 1.0);
var
  a: Integer;
  c: TRGB;
begin
 if Text = '' then Exit;
 if e_CharFonts = nil then Exit;
 if FontID > DWORD(High(e_CharFonts)) then Exit;

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

procedure e_CharFont_GetSize(FontID: DWORD; Text: string; var w, h: Word);
var
  a: Integer;
  h2: Word;
begin
 w := 0;
 h := 0;

 if Text = '' then Exit;
 if e_CharFonts = nil then Exit;
 if FontID > DWORD(High(e_CharFonts)) then Exit;

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

function e_CharFont_GetMaxWidth(FontID: DWORD): Word;
var
  a: Integer;
begin
 Result := 0;

 if e_CharFonts = nil then Exit;
 if FontID > DWORD(High(e_CharFonts)) then Exit;

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
 if FontID > DWORD(High(e_CharFonts)) then Exit;

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

procedure e_TextureFontBuild(Texture: DWORD; var FontID: DWORD; XCount, YCount: Word;
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
  TextureID := e_Textures[Texture].ID;
  CharWidth := (e_Textures[Texture].Width div XCount)+Space;
  CharHeight := e_Textures[Texture].Height div YCount;
 end;
 
 glBindTexture(GL_TEXTURE_2D, e_Textures[Texture].ID);
 for loop1 := 0 to XCount*YCount-1 do
 begin
  cx := (loop1 mod XCount)/XCount;
	cy := (loop1 div YCount)/YCount;

	glNewList(e_TextureFonts[id].Base+loop1, GL_COMPILE);
	 glBegin(GL_QUADS);
    glTexCoord2f(cx, 1.0-cy-1/YCount);
    glVertex2d(0, e_Textures[Texture].Height div YCount);

	  glTexCoord2f(cx+1/XCount, 1.0-cy-1/YCount);
    glVertex2i(e_Textures[Texture].Width div XCount, e_Textures[Texture].Height div YCount);

		glTexCoord2f(cx+1/XCount, 1.0-cy);
    glVertex2i(e_Textures[Texture].Width div XCount, 0);

		glTexCoord2f(cx, 1.0-cy);
    glVertex2i(0, 0);
   glEnd();
	 glTranslated((e_Textures[Texture].Width div XCount)+Space, 0, 0);
	glEndList();
 end;

 FontID := id;
end;

procedure e_TextureFontKill(FontID: DWORD);
begin
	glDeleteLists(e_TextureFonts[FontID].Base, 256);
  e_TextureFonts[FontID].Base := 0;
end;

procedure e_TextureFontPrint(X, Y: GLint; Text: string; FontID: DWORD);
begin
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

procedure e_TextureFontPrintFmt(X, Y: GLint; Text: string; FontID: DWORD);
var
  R, G, B: Byte;
  L, P, EP: Integer;
  cw, ch: Byte;
begin
  if Text = '' then Exit;
  R := 255;
  G := 255;
  B := 255;

  e_TextureFontGetSize(FontID, cw, ch);

  L := Length(Text);
  P := AnsiPos('^', Text);
  EP := L;

  if (P > 0) and (P < L) then
  begin
    EP := P + 2;
    case Text[P + 1] of
      'r':
      begin
        B := 0;
        G := 0;
      end;
      'g':
      begin
        R := 0;
        B := 0;
      end;
      'b':
      begin
        R := 0;
        G := 0;
      end;
      'y':
      begin
        R := 255;
        G := 255;
        B := 0;
      end;
      'd':
      begin
        R := 127;
        G := 127;
        B := 127;
      end;
      'l':
      begin
        R := 200;
        G := 200;
        B := 200;
      end;
      '0':
      begin
        R := 0;
        G := 0;
        B := 0;
      end;
      's':
      begin
        if (P + 10 > L) then
          P := 0
        else
        begin
          R := StrToIntDef(Copy(Text, P + 2, 3), 0);
          G := StrToIntDef(Copy(Text, P + 5, 3), 0);
          B := StrToIntDef(Copy(Text, P + 8, 3), 0);
          EP := P + 11;
        end;
      end;
      'w':
      begin
        R := 255;
        G := 255;
        B := 255;               
      end;
      else P := 0;
    end;
  end
  else
    P := 0;

  if P > 0 then
  begin
    e_TextureFontPrintEx(X, Y, Copy(Text, 1, P - 1), FontID, R, G, B, 1, True);
    if EP < L then
      e_TextureFontPrintFmt(X + cw * (P - 1), Y, Copy(Text, EP, L - EP + 1), FontID);
  end
  else
    e_TextureFontPrintEx(X, Y, Text, FontID, R, G, B, 1, True);
end;

procedure e_TextureFontGetSize(ID: DWORD; var CharWidth, CharHeight: Byte);
begin
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


end.

