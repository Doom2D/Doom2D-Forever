(* coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
 * Understanding is not required. Only obedience.
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../shared/a_modes.inc}
unit fui_gfx_gl;

interface

uses
  SysUtils, Classes,
  GL, GLExt, SDL2,
  sdlcarcass,
  fui_common, fui_events;


// ////////////////////////////////////////////////////////////////////////// //
type
  TGxFont = class
  protected
    mName: AnsiString;
    mHeight: Integer;
    mBaseLine: Integer;

  public
    function charWidth (const ch: AnsiChar): Integer; virtual; abstract;
    function textWidth (const s: AnsiString): Integer; virtual; abstract;

  public
    property name: AnsiString read mName;
    property height: Integer read mHeight;
    property baseLine: Integer read mBaseLine;
  end;

  TGxContext = class
  public
    type
      TMarkIcon = (
        Checkbox,
        Radiobox
      );

    type
      TWinIcon = (
        Close
      );

  protected
    mActive: Boolean;
    mColor: TGxRGBA;
    mFont: TGxFont;
    // for active contexts
    mScaled: Boolean;
    mScale: Single;
    mClipRect: TGxRect;
    mClipOfs: TGxOfs;

  protected
    function getFont (): AnsiString;
    procedure setFont (const aname: AnsiString);

    procedure onActivate ();
    procedure onDeactivate ();

    procedure setColor (const clr: TGxRGBA);

    procedure realizeClip (); // setup scissoring

    procedure setClipOfs (const aofs: TGxOfs);
    procedure setClipRect (const aclip: TGxRect);

  public
    constructor Create ();
    destructor Destroy (); override;

    procedure line (x1, y1, x2, y2: Integer);
    procedure hline (x, y, len: Integer);
    procedure vline (x, y, len: Integer);
    procedure rect (x, y, w, h: Integer);
    procedure fillRect (x, y, w, h: Integer);
    procedure darkenRect (x, y, w, h: Integer; a: Integer);

    function charWidth (const ch: AnsiChar): Integer;
    function charHeight (const ch: AnsiChar): Integer;
    function textWidth (const s: AnsiString): Integer;
    function textHeight (const s: AnsiString): Integer;
    function drawChar (x, y: Integer; const ch: AnsiChar): Integer; // returns char width
    function drawText (x, y: Integer; const s: AnsiString): Integer; // returns text width

    function iconMarkWidth (ic: TMarkIcon): Integer;
    function iconMarkHeight (ic: TMarkIcon): Integer;
    procedure drawIconMark (ic: TMarkIcon; x, y: Integer; marked: Boolean);

    function iconWinWidth (ic: TWinIcon): Integer;
    function iconWinHeight (ic: TWinIcon): Integer;
    procedure drawIconWin (ic: TWinIcon; x, y: Integer; pressed: Boolean);

    procedure resetClip ();

    function setOffset (constref aofs: TGxOfs): TGxOfs; // returns previous offset
    function setClip (constref aclip: TGxRect): TGxRect; // returns previous clip

    function combineClip (constref aclip: TGxRect): TGxRect; // returns previous clip

  public //HACK!
    procedure glSetScale (ascale: Single);
    procedure glSetTrans (ax, ay: Single);
    procedure glSetScaleTrans (ascale, ax, ay: Single);

  public
    property active: Boolean read mActive;
    property color: TGxRGBA read mColor write setColor;
    property font: AnsiString read getFont write setFont;
    property offset: TGxOfs read mClipOfs write setClipOfs;
    property clip: TGxRect read mClipRect write setClipRect; // clipping is unaffected by offset
  end;


// set active context; `ctx` can be `nil`
procedure gxSetContext (ctx: TGxContext; ascale: Single=1.0);
procedure gxSetContextNoMatrix (ctx: TGxContext);


// setup 2D OpenGL mode; will be called automatically in `glInit()`
procedure oglSetup2D (winWidth, winHeight: Integer; upsideDown: Boolean=false);
procedure oglSetup2DState (); // don't modify viewports and matrices

procedure oglDrawCursor ();
procedure oglDrawCursorAt (msX, msY: Integer);



// ////////////////////////////////////////////////////////////////////////// //
var
  gGfxDoClear: Boolean = true;


implementation


// ////////////////////////////////////////////////////////////////////////// //
// returns `false` if the color is transparent
// returns `false` if the color is transparent
function setupGLColor (constref clr: TGxRGBA): Boolean;
begin
  if (clr.a < 255) then
  begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end
  else
  begin
    glDisable(GL_BLEND);
  end;
  glColor4ub(clr.r, clr.g, clr.b, clr.a);
  result := (clr.a <> 0);
end;

function isScaled (): Boolean;
var
  mt: packed array [0..15] of Double;
begin
  glGetDoublev(GL_MODELVIEW_MATRIX, @mt[0]);
  result := (mt[0] <> 1.0) or (mt[1*4+1] <> 1.0);
end;


// ////////////////////////////////////////////////////////////////////////// //
//TODO: OpenGL framebuffers and shaders state
type
  TSavedGLState = record
  public
    glmatmode: GLint;
    gltextbinding: GLint;
    //oldprg: GLint;
    //oldfbr, oldfbw: GLint;
    glvport: packed array [0..3] of GLint;
    saved: Boolean;

  public
    constructor Create (dosave: Boolean);
    procedure save ();
    procedure restore ();
  end;

constructor TSavedGLState.Create (dosave: Boolean);
begin
  FillChar(self, sizeof(self), 0);
  if (dosave) then save();
end;

procedure TSavedGLState.save ();
begin
  if (saved) then raise Exception.Create('cannot save into already saved OpenGL state');
  glGetIntegerv(GL_MATRIX_MODE, @glmatmode);
  glGetIntegerv(GL_TEXTURE_BINDING_2D, @gltextbinding);
  glGetIntegerv(GL_VIEWPORT, @glvport[0]);
  //glGetIntegerv(GL_CURRENT_PROGRAM, &oldprg);
  //glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, &oldfbr);
  //glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &oldfbw);
  glMatrixMode(GL_PROJECTION); glPushMatrix();
  glMatrixMode(GL_MODELVIEW); glPushMatrix();
  glMatrixMode(GL_TEXTURE); glPushMatrix();
  glMatrixMode(GL_COLOR); glPushMatrix();
  glPushAttrib({GL_ENABLE_BIT|GL_COLOR_BUFFER_BIT|GL_CURRENT_BIT}GL_ALL_ATTRIB_BITS); // let's play safe
  saved := true;
end;

procedure TSavedGLState.restore ();
begin
  if (not saved) then raise Exception.Create('cannot restore unsaved OpenGL state');
  glPopAttrib({GL_ENABLE_BIT});
  glMatrixMode(GL_PROJECTION); glPopMatrix();
  glMatrixMode(GL_MODELVIEW); glPopMatrix();
  glMatrixMode(GL_TEXTURE); glPopMatrix();
  glMatrixMode(GL_COLOR); glPopMatrix();
  glMatrixMode(glmatmode);
  //if (glHasFunc!"glBindFramebufferEXT") glBindFramebufferEXT(GL_READ_FRAMEBUFFER_EXT, oldfbr);
  //if (glHasFunc!"glBindFramebufferEXT") glBindFramebufferEXT(GL_DRAW_FRAMEBUFFER_EXT, oldfbw);
  glBindTexture(GL_TEXTURE_2D, gltextbinding);
  //if (glHasFunc!"glUseProgram") glUseProgram(oldprg);
  glViewport(glvport[0], glvport[1], glvport[2], glvport[3]);
  saved := false;
end;


var
  curCtx: TGxContext = nil;
  savedGLState: TSavedGLState;


// ////////////////////////////////////////////////////////////////////////// //
// set active context; `ctx` can be `nil`
procedure gxSetContextInternal (ctx: TGxContext; ascale: Single; domatrix: Boolean);
var
  mt: packed array [0..15] of Double;
begin
  if (savedGLState.saved) then savedGLState.restore();

  if (curCtx <> nil) then
  begin
    curCtx.onDeactivate();
    curCtx.mActive := false;
  end;

  curCtx := ctx;
  if (ctx <> nil) then
  begin
    ctx.mActive := true;
    savedGLState.save();
    if (domatrix) then
    begin
      oglSetup2D(fuiScrWdt, fuiScrHgt);
      glScalef(ascale, ascale, 1.0);
      ctx.mScaled := (ascale <> 1.0);
      ctx.mScale := ascale;
    end
    else
    begin
      // assume uniform scale
      glGetDoublev(GL_MODELVIEW_MATRIX, @mt[0]);
      ctx.mScaled := (mt[0] <> 1.0) or (mt[1*4+1] <> 1.0);
      ctx.mScale := mt[0];
      oglSetup2DState();
    end;
    ctx.onActivate();
  end;
end;


procedure gxSetContext (ctx: TGxContext; ascale: Single=1.0); begin gxSetContextInternal(ctx, ascale, true); end;
procedure gxSetContextNoMatrix (ctx: TGxContext); begin gxSetContextInternal(ctx, 1, false); end;


// ////////////////////////////////////////////////////////////////////////// //
type
  TScissorSave = record
  public
    wassc: Boolean;
    scxywh: packed array[0..3] of GLint;

  public

  public
    procedure save (enableScissoring: Boolean);
    procedure restore ();

    // set new scissor rect, bounded by the saved scissor rect
    procedure combineRect (x, y, w, h: Integer);
  end;


procedure TScissorSave.save (enableScissoring: Boolean);
begin
  wassc := (glIsEnabled(GL_SCISSOR_TEST) <> 0);
  if wassc then glGetIntegerv(GL_SCISSOR_BOX, @scxywh[0]) else glGetIntegerv(GL_VIEWPORT, @scxywh[0]);
  //conwritefln('(%d,%d)-(%d,%d)', [scxywh[0], scxywh[1], scxywh[2], scxywh[3]]);
  if enableScissoring and (not wassc) then glEnable(GL_SCISSOR_TEST);
end;

procedure TScissorSave.restore ();
begin
  glScissor(scxywh[0], scxywh[1], scxywh[2], scxywh[3]);
  if wassc then glEnable(GL_SCISSOR_TEST) else glDisable(GL_SCISSOR_TEST);
end;

procedure TScissorSave.combineRect (x, y, w, h: Integer);
//var ox, oy, ow, oh: Integer;
begin
  if (w < 1) or (h < 1) then begin glScissor(0, 0, 0, 0); exit; end;
  y := fuiScrHgt-(y+h);
  //ox := x; oy := y; ow := w; oh := h;
  if not intersectRect(x, y, w, h, scxywh[0], scxywh[1], scxywh[2], scxywh[3]) then
  begin
    //writeln('oops: COMBINE: old=(', ox, ',', oy, ')-(', ox+ow-1, ',', oy+oh-1, '); sci: (', scxywh[0], ',', scxywh[1], ')-(', scxywh[0]+scxywh[2]-1, ',', scxywh[1]+scxywh[3]-1, ')');
    //writeln('oops: COMBINE: oldx=<', ox, '-', ox+ow-1, '>; oldy=<', oy, ',', oy+oh-1, '> : scix=<', scxywh[0], '-', scxywh[0]+scxywh[2]-1, '>; sciy=<', scxywh[1], '-', scxywh[1]+scxywh[3]-1, '>');
    glScissor(0, 0, 0, 0);
  end
  else
  begin
    glScissor(x, y, w, h);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
{$INCLUDE fui_gfx_gl_fonts.inc}

type
  TGxBmpFont = class(TGxFont)
  private
    mTexId: GLuint; // OpenGL texture id
    mWidth: Integer; // <=0: proportional
    mFontBmp: PByte;
    mFontWdt: PByte;
    mFreeFontWdt: Boolean;

  protected
    procedure oglCreateTexture ();
    procedure oglDestroyTexture ();

    function drawTextInternal (x, y: Integer; const s: AnsiString): Integer; // return width (not including last empty pixel)

  public
    constructor Create (const aname: AnsiString; awdt, ahgt: Integer; const afont: PByte; const awdtable: PByte=nil);
    destructor Destroy (); override;

    function charWidth (const ch: AnsiChar): Integer; override;
    function textWidth (const s: AnsiString): Integer; override;
  end;


constructor TGxBmpFont.Create (const aname: AnsiString; awdt, ahgt: Integer; const afont: PByte; const awdtable: PByte=nil);
var
  c: Integer;
begin
  if (afont = nil) then raise Exception.Create('internal error in font creation');
  if (ahgt < 1) then raise Exception.Create('internal error in font creation');
  if (awdt > 0) then
  begin
    //if (awdtable <> nil) then raise Exception.Create('internal error in font creation');
    mFreeFontWdt := true;
    // create width table
    GetMem(mFontWdt, 256);
    for c := 0 to 255 do mFontWdt[c] := awdt-1;
  end
  else
  begin
    if (awdtable = nil) then raise Exception.Create('internal error in font creation');
    awdt := 0;
    mFontWdt := awdtable;
  end;
  mName := aname;
  mWidth := awdt;
  mHeight := ahgt;
  mBaseLine := ahgt-1; //FIXME
  mFontBmp := afont;
  mTexId := 0;
end;


destructor TGxBmpFont.Destroy ();
begin
  if (mFreeFontWdt) and (mFontWdt <> nil) then FreeMem(mFontWdt);
  mName := '';
  mWidth := 0;
  mHeight := 0;
  mBaseLine := 0;
  mFontBmp := nil;
  mFontWdt := nil;
  mFreeFontWdt := false;
  mTexId := 0;
  inherited;
end;


procedure TGxBmpFont.oglCreateTexture ();
begin
  mTexId := createFontTexture(mFontBmp, mFontWdt, mHeight, (mWidth <= 0));
end;


procedure TGxBmpFont.oglDestroyTexture ();
begin
  if (mTexId <> 0) then
  begin
    glDeleteTextures(1, @mTexId);
    mTexId := 0;
  end;
end;


function TGxBmpFont.charWidth (const ch: AnsiChar): Integer;
begin
  result := (mFontWdt[Byte(ch)] and $0f);
end;


function TGxBmpFont.textWidth (const s: AnsiString): Integer;
var
  ch: AnsiChar;
begin
  if (Length(s) > 0) then
  begin
    result := -1;
    for ch in s do result += (mFontWdt[Byte(ch)] and $0f)+1;
  end
  else
  begin
    result := 0;
  end;
end;


// return width (not including last empty pixel)
function TGxBmpFont.drawTextInternal (x, y: Integer; const s: AnsiString): Integer;
var
  ch: AnsiChar;
  tx, ty: Integer;
begin
  if (Length(s) = 0) then begin result := 0; exit; end;

  result := -1;

  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_NOTEQUAL, 0.0);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, mTexId);

  for ch in s do
  begin
    tx := (Integer(ch) mod 16)*8;
    ty := (Integer(ch) div 16)*16;
    glBegin(GL_QUADS);
      glTexCoord2f((tx+0)/128.0, (ty+0)/256.0); glVertex2i(x+0, y+0); // top-left
      glTexCoord2f((tx+8)/128.0, (ty+0)/256.0); glVertex2i(x+8, y+0); // top-right
      glTexCoord2f((tx+8)/128.0, (ty+mHeight)/256.0); glVertex2i(x+8, y+mHeight); // bottom-right
      glTexCoord2f((tx+0)/128.0, (ty+mHeight)/256.0); glVertex2i(x+0, y+mHeight); // bottom-left
    glEnd();
    x += (mFontWdt[Byte(ch)] and $0f)+1;
    result += (mFontWdt[Byte(ch)] and $0f)+1;
  end;

  glDisable(GL_ALPHA_TEST);
  glDisable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);
end;


// ////////////////////////////////////////////////////////////////////////// //
var
  fontList: array of TGxBmpFont = nil;
  defaultFontName: AnsiString = 'dos';


function strEquCI (const s0, s1: AnsiString): Boolean;
var
  f: Integer;
  c0, c1: AnsiChar;
begin
  result := (Length(s0) = Length(s1));
  if (result) then
  begin
    for f := 1 to Length(s0) do
    begin
      c0 := s0[f];
      if (c0 >= 'a') and (c0 <= 'z') then Dec(c0, 32); // poor man's `toupper()`
      c1 := s1[f];
      if (c1 >= 'a') and (c1 <= 'z') then Dec(c1, 32); // poor man's `toupper()`
      if (c0 <> c1) then begin result := false; exit; end;
    end;
  end;
end;


function getFontByName (const aname: AnsiString): TGxBmpFont;
var
  f: Integer;
  fname: AnsiString;
begin
  if (Length(fontList) = 0) then raise Exception.Create('font subsystem not initialized');
  if (Length(aname) = 0) or (strEquCI(aname, 'default')) then fname := defaultFontName else fname := aname;
  for f := 0 to High(fontList) do
  begin
    result := fontList[f];
    if (result = nil) then continue;
    if (strEquCI(result.name, fname)) then exit;
  end;
  if (fontList[0] = nil) then raise Exception.Create('font subsystem not properly initialized');
  result := fontList[0];
end;


procedure deleteFonts ();
var
  f: Integer;
begin
  for f := 0 to High(fontList) do freeAndNil(fontList[f]);
  fontList := nil;
end;


procedure createFonts ();
begin
  deleteFonts();
  SetLength(fontList, 10);
  fontList[0] := TGxBmpFont.Create('dos', 8, 8, @kgiFont8[0], @kgiFont8PropWidth[0]);
  fontList[1] := TGxBmpFont.Create('dos-prop', 0, 8, @kgiFont8[0], @kgiFont8PropWidth[0]);
  fontList[2] := TGxBmpFont.Create('msx', 6, 8, @kgiFont6[0], @kgiFont6PropWidth[0]);
  fontList[3] := TGxBmpFont.Create('msx-prop', 0, 8, @kgiFont6[0], @kgiFont6PropWidth[0]);
  fontList[4] := TGxBmpFont.Create('win8', 8, 8, @kgiWFont8[0], @kgiWFont8Wdt[0]);
  fontList[5] := TGxBmpFont.Create('win8-prop', 0, 8, @kgiWFont8[0], @kgiWFont8Wdt[0]);
  fontList[6] := TGxBmpFont.Create('win14', 8, 14, @kgiFont14[0], @kgiFont14Wdt[0]);
  fontList[7] := TGxBmpFont.Create('win14-prop', 0, 14, @kgiFont14[0], @kgiFont14Wdt[0]);
  fontList[8] := TGxBmpFont.Create('win16', 8, 16, @kgiFont16[0], @kgiFont16Wdt[0]);
  fontList[9] := TGxBmpFont.Create('win16-prop', 0, 16, @kgiFont16[0], @kgiFont16Wdt[0]);
end;


procedure oglInitFonts ();
var
  f: Integer;
begin
  for f := 0 to High(fontList) do if (fontList[f] <> nil) then fontList[f].oglCreateTexture();
end;


procedure oglDeinitFonts ();
var
  f: Integer;
begin
  for f := 0 to High(fontList) do if (fontList[f] <> nil) then fontList[f].oglDestroyTexture();
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure oglSetup2DState ();
begin
  glDisable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glDisable(GL_LINE_SMOOTH);
  glDisable(GL_POLYGON_SMOOTH);
  glDisable(GL_POINT_SMOOTH);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glDisable(GL_DITHER);
  glDisable(GL_STENCIL_TEST);
  glDisable(GL_SCISSOR_TEST);
  glDisable(GL_CULL_FACE);
  glDisable(GL_ALPHA_TEST);

  glClearColor(0, 0, 0, 0);
  glColor4f(1, 1, 1, 1);
end;


procedure oglSetup2D (winWidth, winHeight: Integer; upsideDown: Boolean=false);
begin
  glViewport(0, 0, winWidth, winHeight);

  oglSetup2DState();

  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();

  glMatrixMode(GL_COLOR);
  glLoadIdentity();

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  if (upsideDown) then
  begin
    glOrtho(0, winWidth, 0, winHeight, -1, 1); // set origin to bottom left
  end
  else
  begin
    glOrtho(0, winWidth, winHeight, 0, -1, 1); // set origin to top left
  end;

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;


// ////////////////////////////////////////////////////////////////////////// //
{$INCLUDE fui_gfx_gl_cursor.inc}

procedure oglDrawCursor (); begin oglDrawCursorAt(fuiMouseX, fuiMouseY); end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TGxContext.Create ();
begin
  mActive := false;
  mColor := TGxRGBA.Create(255, 255, 255);
  mFont := getFontByName('default');
  mScaled := false;
  mScale := 1.0;
  mClipRect := TGxRect.Create(0, 0, 8192, 8192);
  mClipOfs := TGxOfs.Create(0, 0);
end;


destructor TGxContext.Destroy ();
begin
  if (mActive) then gxSetContext(nil);
  inherited;
end;


function TGxContext.getFont (): AnsiString;
begin
  result := mFont.name;
end;

procedure TGxContext.setFont (const aname: AnsiString);
begin
  mFont := getFontByName(aname);
end;


procedure TGxContext.onActivate ();
begin
  setupGLColor(mColor);
  realizeClip();
end;

procedure TGxContext.onDeactivate ();
begin
end;


procedure TGxContext.setColor (const clr: TGxRGBA);
begin
  mColor := clr;
  if (mActive) then setupGLColor(mColor);
end;


procedure TGxContext.realizeClip ();
var
  sx, sy, sw, sh: Integer;
begin
  if (not mActive) then exit; // just in case
  if (mClipRect.w <= 0) or (mClipRect.h <= 0) then
  begin
    glEnable(GL_SCISSOR_TEST);
    glScissor(0, 0, 0, 0);
  end
  else
  begin
    if (mScaled) then
    begin
      sx := trunc(mClipRect.x*mScale);
      sy := trunc(mClipRect.y*mScale);
      sw := trunc(mClipRect.w*mScale);
      sh := trunc(mClipRect.h*mScale);
    end
    else
    begin
      sx := mClipRect.x;
      sy := mClipRect.y;
      sw := mClipRect.w;
      sh := mClipRect.h;
    end;
    if (not intersectRect(sx, sy, sw, sh, 0, 0, fuiScrWdt, fuiScrHgt)) then
    begin
      glEnable(GL_SCISSOR_TEST);
      glScissor(0, 0, 0, 0);
    end
    else if (sx = 0) and (sy = 0) and (sw = fuiScrWdt) and (sh = fuiScrHgt) then
    begin
      glDisable(GL_SCISSOR_TEST);
    end
    else
    begin
      glEnable(GL_SCISSOR_TEST);
      sy := fuiScrHgt-(sy+sh);
      glScissor(sx, sy, sw, sh);
    end;
  end;
end;


procedure TGxContext.resetClip ();
begin
  mClipRect := TGxRect.Create(0, 0, 8192, 8192);
  if (mActive) then realizeClip();
end;


procedure TGxContext.setClipOfs (const aofs: TGxOfs);
begin
  mClipOfs := aofs;
end;


procedure TGxContext.setClipRect (const aclip: TGxRect);
begin
  mClipRect := aclip;
  if (mActive) then realizeClip();
end;


function TGxContext.setOffset (constref aofs: TGxOfs): TGxOfs;
begin
  result := mClipOfs;
  mClipOfs := aofs;
end;


function TGxContext.setClip (constref aclip: TGxRect): TGxRect;
begin
  result := mClipRect;
  mClipRect := aclip;
  if (mActive) then realizeClip();
end;


function TGxContext.combineClip (constref aclip: TGxRect): TGxRect;
begin
  result := mClipRect;
  mClipRect.intersect(aclip);
  if (mActive) then realizeClip();
end;


procedure TGxContext.line (x1, y1, x2, y2: Integer);
begin
  if (not mActive) or (mClipRect.w < 1) or (mClipRect.h < 1) or (mColor.a = 0) then exit;

  if (not mScaled) then
  begin
    glLineWidth(1);
    glBegin(GL_LINES);
      glVertex2f(x1+0.375, y1+0.375);
      glVertex2f(x2+0.375, y2+0.375);
    glEnd();

    if (x1 <> x2) or (y1 <> y2) then
    begin
      glPointSize(1);
      glBegin(GL_POINTS);
        glVertex2f(x2+0.375, y2+0.375);
      glEnd();
    end;
  end
  else
  begin
    glLineWidth(1);
    glBegin(GL_LINES);
      glVertex2i(x1, y1);
      glVertex2i(x2, y2);
      // draw last point
      glVertex2i(x2, y2);
      glVertex2i(x2+1, y2+1);
    glEnd();
  end;
end;


procedure TGxContext.hline (x, y, len: Integer);
begin
  if (not mActive) or (mClipRect.w < 1) or (mClipRect.h < 1) or (mColor.a = 0) then exit;
  if (len < 1) then exit;
  if (not mScaled) then
  begin
    glLineWidth(1);
    glBegin(GL_LINES);
      glVertex2f(x+0.375, y+0.375);
      glVertex2f(x+len+0.375, y+0.375);
    glEnd();
  end
  else if (mScale > 1.0) then
  begin
    glBegin(GL_QUADS);
      glVertex2i(x, y);
      glVertex2i(x+len, y);
      glVertex2i(x+len, y+1);
      glVertex2i(x, y+1);
    glEnd();
  end
  else
  begin
    glPointSize(1);
    glBegin(GL_POINTS);
      while (len > 0) do begin glVertex2i(x, y); Inc(x); Dec(len); end;
    glEnd();
  end;
end;


procedure TGxContext.vline (x, y, len: Integer);
begin
  if (not mActive) or (mClipRect.w < 1) or (mClipRect.h < 1) or (mColor.a = 0) then exit;
  if (len < 1) then exit;
  if (not mScaled) then
  begin
    glLineWidth(1);
    glBegin(GL_LINES);
      glVertex2f(x+0.375, y+0.375);
      glVertex2f(x+0.375, y+len+0.375);
    glEnd();
  end
  else if (mScale > 1.0) then
  begin
    glBegin(GL_QUADS);
      glVertex2i(x, y);
      glVertex2i(x, y+len);
      glVertex2i(x+1, y+len);
      glVertex2i(x+1, y);
    glEnd();
  end
  else
  begin
    glPointSize(1);
    glBegin(GL_POINTS);
      while (len > 0) do begin glVertex2i(x, y); Inc(y); Dec(len); end;
    glEnd();
  end;
end;


procedure TGxContext.rect (x, y, w, h: Integer);
begin
  if (not mActive) or (mClipRect.w < 1) or (mClipRect.h < 1) or (mColor.a = 0) then exit;
  if (w < 0) or (h < 0) then exit;
  if (w = 1) and (h = 1) then
  begin
    glPointSize(1);
    glBegin(GL_POINTS);
      if mScaled then glVertex2i(x, y) else glVertex2f(x+0.375, y+0.375);
    glEnd();
  end
  else
  begin
    if (not mScaled) then
    begin
      glLineWidth(1);
      glBegin(GL_LINES);
        glVertex2i(x, y); glVertex2i(x+w, y); // top
        glVertex2i(x, y+h-1); glVertex2i(x+w, y+h-1); // bottom
        glVertex2f(x+0.375, y+1); glVertex2f(x+0.375, y+h-1); // left
        glVertex2f(x+w-1+0.375, y+1); glVertex2f(x+w-1+0.375, y+h-1); // right
      glEnd();
    end
    else
    begin
      hline(x, y, w);
      hline(x, y+h-1, w);
      vline(x, y+1, h-2);
      vline(x+w-1, y+1, h-2);
    end;
  end;
end;


procedure TGxContext.fillRect (x, y, w, h: Integer);
begin
  if (not mActive) or (mClipRect.w < 1) or (mClipRect.h < 1) or (mColor.a = 0) then exit;
  if (w < 0) or (h < 0) then exit;
  glBegin(GL_QUADS);
    glVertex2f(x, y);
    glVertex2f(x+w, y);
    glVertex2f(x+w, y+h);
    glVertex2f(x, y+h);
  glEnd();
end;


procedure TGxContext.darkenRect (x, y, w, h: Integer; a: Integer);
begin
  if (not mActive) or (mClipRect.w < 1) or (mClipRect.h < 1) or (a >= 255) then exit;
  if (w < 0) or (h < 0) then exit;
  if (a < 0) then a := 0;
  glEnable(GL_BLEND);
  glBlendFunc(GL_ZERO, GL_SRC_ALPHA);
  glColor4f(0.0, 0.0, 0.0, a/255.0);
  glBegin(GL_QUADS);
    glVertex2i(x, y);
    glVertex2i(x+w, y);
    glVertex2i(x+w, y+h);
    glVertex2i(x, y+h);
  glEnd();
  setupGLColor(mColor);
end;


function TGxContext.charWidth (const ch: AnsiChar): Integer;
begin
  result := mFont.charWidth(ch);
end;

function TGxContext.charHeight (const ch: AnsiChar): Integer;
begin
  result := mFont.height;
end;


function TGxContext.textWidth (const s: AnsiString): Integer;
begin
  result := mFont.textWidth(s);
end;

function TGxContext.textHeight (const s: AnsiString): Integer;
begin
  result := mFont.height;
end;


function TGxContext.drawChar (x, y: Integer; const ch: AnsiChar): Integer; // returns char width
begin
  result := mFont.charWidth(ch);
  if (not mActive) or (mClipRect.w < 1) or (mClipRect.h < 1) or (mColor.a = 0) then exit;
  TGxBmpFont(mFont).drawTextInternal(x, y, ch);
end;

function TGxContext.drawText (x, y: Integer; const s: AnsiString): Integer; // returns text width
begin
  result := mFont.textWidth(s);
  if (not mActive) or (mClipRect.w < 1) or (mClipRect.h < 1) or (mColor.a = 0) or (Length(s) = 0) then exit;
  TGxBmpFont(mFont).drawTextInternal(x, y, s);
end;


function TGxContext.iconMarkWidth (ic: TMarkIcon): Integer; begin result := 11; end;
function TGxContext.iconMarkHeight (ic: TMarkIcon): Integer; begin result := 8; end;

procedure TGxContext.drawIconMark (ic: TMarkIcon; x, y: Integer; marked: Boolean);
var
  f: Integer;
begin
  if (not mActive) or (mClipRect.w < 1) or (mClipRect.h < 1) or (mColor.a = 0) then exit;
  if (ic = TMarkIcon.Checkbox) then
  begin
    vline(x, y, 7);
    vline(x+10, y, 7);
    hline(x+1, y, 1);
    hline(x+1, y+6, 1);
    hline(x+9, y, 1);
    hline(x+9, y+6, 1);
  end
  else
  begin
    vline(x, y+1, 5);
    vline(x+10, y+1, 5);
    hline(x+1, y, 1);
    hline(x+1, y+6, 1);
    hline(x+9, y, 1);
    hline(x+9, y+6, 1);
  end;
  if (not marked) then exit;
  case ic of
    TMarkIcon.Checkbox:
      begin
        for f := 0 to 4 do
        begin
          vline(x+3+f, y+1+f, 1);
          vline(x+7-f, y+1+f, 1);
        end;
      end;
    TMarkIcon.Radiobox:
      begin
        hline(x+4, y+1, 3);
        hline(x+3, y+2, 5);
        hline(x+3, y+3, 5);
        hline(x+3, y+4, 5);
        hline(x+4, y+5, 3);
      end;
  end;
end;


function TGxContext.iconWinWidth (ic: TWinIcon): Integer; begin result := 9; end;
function TGxContext.iconWinHeight (ic: TWinIcon): Integer; begin result := 8; end;

procedure TGxContext.drawIconWin (ic: TWinIcon; x, y: Integer; pressed: Boolean);
var
  f: Integer;
begin
  if (not mActive) or (mClipRect.w < 1) or (mClipRect.h < 1) or (mColor.a = 0) then exit;
  if pressed then rect(x, y, 9, 8);
  for f := 1 to 5 do
  begin
    vline(x+1+f, y+f, 1);
    vline(x+1+6-f, y+f, 1);
  end;
end;


procedure TGxContext.glSetScale (ascale: Single);
begin
  if (ascale < 0.01) then ascale := 0.01;
  glLoadIdentity();
  glScalef(ascale, ascale, 1.0);
  mScale := ascale;
  mScaled := (ascale <> 1.0);
end;

procedure TGxContext.glSetTrans (ax, ay: Single);
begin
  glLoadIdentity();
  glScalef(mScale, mScale, 1.0);
  glTranslatef(ax, ay, 0);
end;


procedure TGxContext.glSetScaleTrans (ascale, ax, ay: Single);
begin
  glSetScale(ascale);
  glTranslatef(ax, ay, 0);
end;


// ////////////////////////////////////////////////////////////////////////// //
(*
procedure oglRestoreMode (doClear: Boolean);
begin
  oglSetup2D(fuiScrWdt, fuiScrHgt);
  glScissor(0, 0, fuiScrWdt, fuiScrHgt);

  glBindTexture(GL_TEXTURE_2D, 0);
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_STENCIL_TEST);
  glDisable(GL_SCISSOR_TEST);
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_CULL_FACE);
  glDisable(GL_LINE_SMOOTH);
  glDisable(GL_POINT_SMOOTH);
  glLineWidth(1);
  glPointSize(1);
  glColor4f(1, 1, 1, 1);

  if doClear then
  begin
    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_ACCUM_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
  end;

  // scale everything
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  //glScalef(4, 4, 1);
end;
*)


//procedure onWinFocus (); begin end;
//procedure onWinBlur (); begin fuiResetKMState(true); end;

//procedure onPreRender (); begin oglRestoreMode(gGfxDoClear); end;
procedure onPostRender (); begin oglDrawCursor(); end;

procedure onInit ();
begin
  //oglSetup2D(fuiScrWdt, fuiScrHgt);
  createCursorTexture();
  oglInitFonts();
end;

procedure onDeinit ();
begin
  fuiResetKMState(false);
  if (curtexid <> 0) then glDeleteTextures(1, @curtexid);
  curtexid := 0;
  oglDeinitFonts();
  fuiSetButState(0);
  fuiSetModState(0);
  fuiSetMouseX(0);
  fuiSetMouseY(0);
end;


// ////////////////////////////////////////////////////////////////////////// //
initialization
  savedGLState := TSavedGLState.Create(false);
  createFonts();
  //winFocusCB := onWinFocus;
  //winBlurCB := onWinBlur;
  //prerenderFrameCB := onPreRender;
  postrenderFrameCB := onPostRender;
  oglInitCB := onInit;
  oglDeinitCB := onDeinit;
end.
