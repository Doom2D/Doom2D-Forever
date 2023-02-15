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
{$INCLUDE ../../../shared/a_modes.inc}
unit r_draw;

interface

  uses
    g_animations,
    r_textures
  ;

  procedure r_Draw_Texture (img: TGLTexture; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean);
  procedure r_Draw_TextureRepeat (img: TGLTexture; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean);
  procedure r_Draw_TextureRepeatRotate (img: TGLTexture; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean; rx, ry, angle: Integer);

  procedure r_Draw_MultiTextureRepeat (m: TGLMultiTexture; const anim: TAnimState; backanim: Boolean; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean);
  procedure r_Draw_MultiTextureRepeatRotate (m: TGLMultiTexture; const anim: TAnimState; backanim: Boolean; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean; rx, ry, angle: Integer);

  procedure r_Draw_Filter (l, t, r, b: Integer; rr, gg, bb, aa: Byte);
  procedure r_Draw_Rect (l, t, r, b: Integer; rr, gg, bb, aa: Byte);
  procedure r_Draw_FillRect (l, t, r, b: Integer; rr, gg, bb, aa: Byte);
  procedure r_Draw_InvertRect (l, t, r, b: Integer; rr, gg, bb, aa: Byte);

  procedure r_Draw_Text (const text: AnsiString; x, y: Integer; r, g, b, a: Byte; f: TGLFont);
  procedure r_Draw_GetTextSize (const text: AnsiString; f: TGLFont; out w, h: Integer);

  procedure r_Draw_Setup (sw, sh, gw, gh: Integer);
  procedure r_Draw_SetRect (l, t, r, b: Integer);
  procedure r_Draw_GetRect (out l, t, r, b: Integer);

  procedure r_Draw_EnableTexture2D (enable: Boolean);
  procedure r_Draw_SetColor (r, g, b, a: Byte);

implementation

  uses
    {$I ../../../nogl/noGLuses.inc}
    SysUtils, Classes, Math,
    e_log, utils
  ;

  const
    NTR = $FF;
    NTG = $00;
    NTB = $00;
    NTA = $FF;

  var
    sl, st, sr, sb: Integer;
    ScreenWidth, ScreenHeight: Integer;
    GameWidth, GameHeight: Integer;

    enableTexture2D: Boolean;
    curR, curG, curB, curA: Byte;

  procedure r_Draw_EnableTexture2D (enable: Boolean);
  begin
    if enable <> enableTexture2D then
    begin
      if enable then glEnable(GL_TEXTURE_2D) else glDisable(GL_TEXTURE_2D);
      enableTexture2D := enable;
    end;
  end;

  procedure r_Draw_SetColor (r, g, b, a: Byte);
  begin
    if (r <> curR) or (g <> curG) or (b <> curB) or (curA <> a) then
    begin
      glColor4ub(r, g, b, a);
      curR := r;
      curG := g;
      curB := b;
      curA := a;
    end;
  end;

  procedure r_Draw_Setup (sw, sh, gw, gh: Integer);
  begin
    ASSERT((sw >= 0) and (sh >= 0)); // screen/window size
    ASSERT((gw >= 0) and (gh >= 0)); // virtual screen size
    ScreenWidth := sw;
    ScreenHeight := sh;
    GameWidth := gw;
    GameHeight := gh;
    glScissor(0, 0, sw, sh);
    glViewport(0, 0, sw, sh);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glOrtho(0, gw, gh, 0, 0, 1);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
    glEnable(GL_SCISSOR_TEST);
    r_Draw_SetRect(0, 0, gw - 1, gh - 1);
  end;

  procedure DrawQuad (x, y, w, h: Integer);
  begin
    glBegin(GL_QUADS);
      glVertex2i(x + w, y);
      glVertex2i(x,     y);
      glVertex2i(x,     y + h);
      glVertex2i(x + w, y + h);
    glEnd();
  end;

  procedure DrawTile (tile: TGLAtlasNode; x, y, w, h: Integer; flip: Boolean; rr, gg, bb, aa: Byte; blend: Boolean);
    var nw, nh, ax, bx, ay, by: GLfloat; l, t, r, b: Integer;
  begin
    if tile = nil then
    begin
      r_Draw_SetColor(rr, gg, bb, aa);
      if blend then glBlendFunc(GL_SRC_ALPHA, GL_ONE) else glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      r_Draw_EnableTexture2D(false);
      glEnable(GL_BLEND);
      DrawQuad(x, y, w, h);
    end
    else
    begin
      nw := tile.base.w;
      nh := tile.base.h;
      ax := IfThen(flip, tile.l, tile.r + 1) / nw;
      bx := IfThen(flip, tile.r + 1, tile.l) / nh;
      ay := (tile.t) / nw;
      by := (tile.b + 1) / nh;
      l := x; t := y; r := x + w; b := y + h;
      r_Textures_GL_Bind(tile.id);
      r_Draw_SetColor(rr, gg, bb, aa);
      r_Draw_EnableTexture2D(true);
      if blend then glBlendFunc(GL_SRC_ALPHA, GL_ONE) else glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);
      glBegin(GL_QUADS);
        glTexCoord2f(ax, ay); glVertex2i(r, t);
        glTexCoord2f(bx, ay); glVertex2i(l, t);
        glTexCoord2f(bx, by); glVertex2i(l, b);
        glTexCoord2f(ax, by); glVertex2i(r, b);
      glEnd();
    end
  end;

  procedure DrawHWTexture (gltex: GLint; nw, nh, x, y, w, h: Integer; flip: Boolean; rr, gg, bb, aa: Byte; blend: Boolean);
    var ax, bx, ay, by: GLfloat; l, t, r, b: Integer;
  begin
    ax := IfThen(flip, 0, w) / nw;
    bx := IfThen(flip, w, 0) / nh;
    ay := 0 / nw;
    by := h / nh;
    l := x; t := y; r := x + w; b := y + h;
    r_Textures_GL_Bind(gltex);
    r_Draw_SetColor(rr, gg, bb, aa);
    r_Draw_EnableTexture2D(true);
    if blend then glBlendFunc(GL_SRC_ALPHA, GL_ONE) else glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glBegin(GL_QUADS);
      glTexCoord2f(ax, ay); glVertex2i(r, t);
      glTexCoord2f(bx, ay); glVertex2i(l, t);
      glTexCoord2f(bx, by); glVertex2i(l, b);
      glTexCoord2f(ax, by); glVertex2i(r, b);
    glEnd();
  end;

  procedure r_Draw_Texture (img: TGLTexture; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean);
    var i, j, first, last, step: Integer; n: TGLAtlasNode;
  begin
    ASSERT(w >= 0);
    ASSERT(h >= 0);
    if img = nil then
      DrawTile(nil, x, y, w, h, flip, NTR, NTB, NTG, NTA, blend)
    else
    begin
      if flip then first := img.cols - 1 else first := 0;
      if flip then last  := -1           else last  := img.cols;
      if flip then step  := -1           else step  := +1;
      glPushMatrix;
      glTranslatef(x, y, 0);
      glScalef(w / img.width, h / img.height, 1);
      for j := 0 to img.lines - 1 do
      begin
        i := first;
        repeat
          n := img.GetTile(i, j);
          ASSERT(n <> nil);
          DrawTile(n, 0, 0, n.width, n.height, flip, r, g, b, a, blend);
          glTranslatef(n.width, 0, 0);
          i := i + step;
        until i = last;
        glTranslatef(-img.width, n.height, 0);
      end;
      glPopMatrix;
    end
  end;

  function r_Draw_IsHWRepeatable (img: TGLTexture): Boolean;
    var n: TGLAtlasNode; a: TGLAtlas;
  begin
    ASSERT(img <> nil);
    result := false;
    if (img.cols = 1) and (img.lines = 1) then
    begin
      n := img.GetTile(0, 0);
      if (n.width = img.width) and (n.height = img.height) then
      begin
        a := n.base;
        result := (a.GetWidth() = img.width) and (a.GetHeight() = img.height)
      end;
    end;
  end;

  procedure r_Draw_TextureRepeat (img: TGLTexture; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean);
    var i, j: Integer;
  begin
    ASSERT(w >= 0);
    ASSERT(h >= 0);
    if img = nil then
      r_Draw_Texture(nil, x, y, w, h, flip, NTR, NTG, NTB, NTB, blend)
    else if r_Draw_IsHWRepeatable(img) then
      DrawHWTexture(img.GetTile(0, 0).base.id, img.width, img.height, x, y, w, h, flip, r, g, b, a, blend)
    else
      for j := 0 to (h - 1) div img.height do
        for i := 0 to (w - 1) div img.width do
          r_Draw_Texture(img, x + i * img.width, y + j * img.height, img.width, img.height, flip, r, g, b, a, blend);
  end;

  procedure r_Draw_TextureRepeatRotate (img: TGLTexture; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean; rx, ry, angle: Integer);
  begin
    ASSERT(w >= 0);
    ASSERT(h >= 0);
    if a <> 0 then
    begin
      glPushMatrix;
      glTranslatef(x + rx, y + ry, 0);
      glRotatef(angle, 0, 0, 1);
      glTranslatef(-(x + rx), -(y + ry), 0);
      r_Draw_TextureRepeat(img, x, y, w, h, flip, r, g, b, a, blend);
      glPopMatrix;
    end
    else
      r_Draw_TextureRepeat(img, x, y, w, h, flip, r, g, b, a, blend);
  end;

  procedure r_Draw_MultiTextureRepeat (m: TGLMultiTexture; const anim: TAnimState; backanim: Boolean; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean);
    var img: TGLTexture; frame: LongInt;
  begin
    ASSERT(anim.IsValid());
    if m = nil then
      r_Draw_TextureRepeat(nil, x, y, w, h, flip, NTR, NTG, NTB, NTB, blend)
    else
    begin
      g_Anim_GetFrameFromState(anim, backanim, frame);
      ASSERT(frame >= 0);
      ASSERT(frame < m.count);
      img := m.GetTexture(frame);
      r_Draw_TextureRepeat(img, x, y, w, h, flip, r, g, b, a, blend);
    end
  end;

  procedure r_Draw_MultiTextureRepeatRotate (m: TGLMultiTexture; const anim: TAnimState; backanim: Boolean; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean; rx, ry, angle: Integer);
  begin
    ASSERT(w >= 0);
    ASSERT(h >= 0);
    if a <> 0 then
    begin
      glPushMatrix;
      glTranslatef(x + rx, y + ry, 0);
      glRotatef(angle, 0, 0, 1);
      glTranslatef(-(x + rx), -(y + ry), 0);
      r_Draw_MultiTextureRepeat(m, anim, backanim, x, y, w, h, flip, r, g, b, a, blend);
      glPopMatrix;
    end
    else
      r_Draw_MultiTextureRepeat(m, anim, backanim, x, y, w, h, flip, r, g, b, a, blend);
  end;

  procedure r_Draw_Rect (l, t, r, b: Integer; rr, gg, bb, aa: Byte);
  begin
    ASSERT(l <= r);
    ASSERT(t <= b);
    if (l < r) and (t < b) then
    begin
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      r_Draw_EnableTexture2D(false);
      r_Draw_SetColor(rr, gg, bb, aa);
      glBegin(GL_QUADS);
        (* top *)
        glVertex2i(l, t);
        glVertex2i(r, t);
        glVertex2i(r, t+1);
        glVertex2i(l, t+1);
        (* bottom *)
        glVertex2i(l, b-1);
        glVertex2i(r, b-1);
        glVertex2i(r, b);
        glVertex2i(l, b);
        (* left *)
        glVertex2i(l,   t+1);
        glVertex2i(l+1, t+1);
        glVertex2i(l+1, b-1);
        glVertex2i(l,   b-1);
        (* right *)
        glVertex2i(r-1, t+1);
        glVertex2i(r,   t+1);
        glVertex2i(r,   b-1);
        glVertex2i(r-1, b-1);
      glEnd;
    end;
  end;

  procedure r_Draw_FillRect (l, t, r, b: Integer; rr, gg, bb, aa: Byte);
  begin
    ASSERT(l <= r);
    ASSERT(t <= b);
    if (l < r) and (t < b) then
    begin
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      r_Draw_EnableTexture2D(false);
      r_Draw_SetColor(rr, gg, bb, aa);
      glBegin(GL_QUADS);
        glVertex2i(l, t);
        glVertex2i(r, t);
        glVertex2i(r, b);
        glVertex2i(l, b);
      glEnd;
    end;
  end;

  procedure r_Draw_Filter (l, t, r, b: Integer; rr, gg, bb, aa: Byte);
  begin
    ASSERT(l <= r);
    ASSERT(t <= b);
    if (l < r) and (t < b) then
    begin
      glEnable(GL_BLEND);
      glBlendFunc(GL_ZERO, GL_SRC_COLOR);
      r_Draw_EnableTexture2D(false);
      r_Draw_SetColor(rr, gg, bb, aa);
      glBegin(GL_QUADS);
        glVertex2i(l, t);
        glVertex2i(r, t);
        glVertex2i(r, b);
        glVertex2i(l, b);
      glEnd;
    end;
  end;

  procedure r_Draw_InvertRect (l, t, r, b: Integer; rr, gg, bb, aa: Byte);
  begin
    ASSERT(l <= r);
    ASSERT(t <= b);
    if (l < r) and (t < b) then
    begin
      glEnable(GL_BLEND);
      glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ZERO);
      r_Draw_EnableTexture2D(false);
      r_Draw_SetColor(rr, gg, bb, aa);
      glBegin(GL_QUADS);
        glVertex2i(l, t);
        glVertex2i(r, t);
        glVertex2i(r, b);
        glVertex2i(l, b);
      glEnd;
    end;
  end;

  procedure r_Draw_Text (const text: AnsiString; x, y: Integer; r, g, b, a: Byte; f: TGLFont);
    var i, xoff, spc: Integer; t: TGLTexture; ch: AnsiChar;
  begin
    xoff := x; spc := MAX(0, f.GetSpace());
    for i := 1 to Length(text) do
    begin
      ch := text[i];
      t := f.GetChar(ch);
      if t <> nil then
        r_Draw_Texture(t, xoff, y, t.width, t.height, false, r, g, b, a, false);
      Inc(xoff, f.GetWidth(ch) + spc);
    end;
  end;

  procedure r_Draw_GetTextSize (const text: AnsiString; f: TGLFont; out w, h: Integer);
    var i, spc, len: Integer;
  begin
    w := 0;
    h := f.GetMaxHeight();
    len := Length(text);
    if len > 0 then
    begin
      spc := MAX(0, f.GetSpace());
      for i := 1 to len - 1 do
        Inc(w, f.GetWidth(text[i]) + spc);
      Inc(w, f.GetWidth(text[len]));
    end;
  end;

  procedure r_Draw_SetRect (l, t, r, b: Integer);
    var x, y, w, h: Integer;
  begin
    ASSERT(l <= r);
    ASSERT(t <= b);
    x := l * ScreenWidth div GameWidth;
    y := t * ScreenHeight div GameHeight;
    w := (r - l + 1) * ScreenWidth div GameWidth;
    h := (b - t + 1) * ScreenHeight div GameHeight;
    glScissor(x, ScreenHeight - h - y, w, h);
    sl := l; st := t; sr := r; sb := b;
  end;

  procedure r_Draw_GetRect (out l, t, r, b: Integer);
  begin
    l := sl; t := st; r := sr; b := sb;
  end;

end.
