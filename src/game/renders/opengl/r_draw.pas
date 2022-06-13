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

  procedure r_Draw_MultiTextureRepeat (m: TGLMultiTexture; const anim: TAnimState; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean);
  procedure r_Draw_MultiTextureRepeatRotate (m: TGLMultiTexture; const anim: TAnimState; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean; rx, ry, angle: Integer);

  procedure r_Draw_Filter (l, t, r, b: Integer; rr, gg, bb, aa: Byte);
  procedure r_Draw_FillRect (l, t, r, b: Integer; rr, gg, bb, aa: Byte);
  procedure r_Draw_InvertRect (l, t, r, b: Integer; rr, gg, bb, aa: Byte);

  procedure r_Draw_Text (const text: AnsiString; x, y: Integer; r, g, b, a: Byte; f: TGLFont);
  procedure r_Draw_GetTextSize (const text: AnsiString; f: TGLFont; out w, h: Integer);

implementation

  uses
    {$IFDEF USE_GLES1}
      GLES11,
    {$ELSE}
      GL, GLEXT,
    {$ENDIF}
    SysUtils, Classes, Math,
    e_log, utils,
    g_game // gScreenWidth, gScreenHeight
  ;

  const
    NTR = $FF;
    NTG = $00;
    NTB = $00;
    NTA = $FF;

  procedure SetupMatrix;
  begin
    glScissor(0, 0, gScreenWidth, gScreenHeight);
    glViewport(0, 0, gScreenWidth, gScreenHeight);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glOrtho(0, gScreenWidth, gScreenHeight, 0, 0, 1);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
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
      glColor4ub(rr, gg, bb, aa);
      if blend then glBlendFunc(GL_SRC_ALPHA, GL_ONE) else glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glDisable(GL_TEXTURE_2D);
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
      glBindTexture(GL_TEXTURE_2D, tile.id);
      glColor4ub(rr, gg, bb, aa);
      if blend then glBlendFunc(GL_SRC_ALPHA, GL_ONE) else glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
      glBegin(GL_QUADS);
        glTexCoord2f(ax, ay); glVertex2i(r, t);
        glTexCoord2f(bx, ay); glVertex2i(l, t);
        glTexCoord2f(bx, by); glVertex2i(l, b);
        glTexCoord2f(ax, by); glVertex2i(r, b);
      glEnd();
      glDisable(GL_TEXTURE_2D);
      glBindTexture(GL_TEXTURE_2D, 0);
    end
  end;

  procedure r_Draw_Texture (img: TGLTexture; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean);
    var i, j, offx, offy: Integer; n: TGLAtlasNode;
  begin
    ASSERT(w >= 0);
    ASSERT(h >= 0);
    if img = nil then
      DrawTile(nil, x, y, w, h, flip, NTR, NTB, NTG, NTA, blend)
    else
    begin
      offx := 0;
      offy := 0;
      for j := 0 to img.lines - 1 do
      begin
        for i := 0 to img.cols - 1 do
        begin
          n := img.GetTile(i, j);
          ASSERT(n <> nil);
          glPushMatrix;
          glTranslatef(x + offx, y + offy, 0);
          glScalef(w / img.width, h / img.height, 1);
          DrawTile(n, 0, 0, n.width, n.height, flip, r, g, b, a, blend);
          glPopMatrix;
          offx := offx + n.width;
        end;
        offx := 0;
        offy := offy + n.height;
      end;
    end
  end;

  procedure r_Draw_TextureRepeat (img: TGLTexture; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean);
    var i, j: Integer;
  begin
    ASSERT(w >= 0);
    ASSERT(h >= 0);
    if img = nil then
      r_Draw_Texture(nil, x, y, w, h, flip, NTR, NTG, NTB, NTB, blend)
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

  procedure r_Draw_MultiTextureRepeat (m: TGLMultiTexture; const anim: TAnimState; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean);
    var img: TGLTexture; cur, total, i: Integer;
  begin
    ASSERT(anim.IsValid());
    if m = nil then
      r_Draw_TextureRepeat(nil, x, y, w, h, flip, NTR, NTG, NTB, NTB, blend)
    else
    begin
      if m.BackAnim then
      begin
        total := m.count * 2 - 1;
        cur := anim.CurrentFrame mod total;
        if cur < m.count then i := cur else i := total - cur - 1;
      end
      else
        i := anim.CurrentFrame mod m.count;
      img := m.GetTexture(i);
      r_Draw_TextureRepeat(img, x, y, w, h, flip, r, g, b, a, blend);
    end
  end;

  procedure r_Draw_MultiTextureRepeatRotate (m: TGLMultiTexture; const anim: TAnimState; x, y, w, h: Integer; flip: Boolean; r, g, b, a: Byte; blend: Boolean; rx, ry, angle: Integer);
  begin
    ASSERT(w >= 0);
    ASSERT(h >= 0);
    if a <> 0 then
    begin
      glPushMatrix;
      glTranslatef(x + rx, y + ry, 0);
      glRotatef(angle, 0, 0, 1);
      glTranslatef(-(x + rx), -(y + ry), 0);
      r_Draw_MultiTextureRepeat(m, anim, x, y, w, h, flip, r, g, b, a, blend);
      glPopMatrix;
    end
    else
      r_Draw_MultiTextureRepeat(m, anim, x, y, w, h, flip, r, g, b, a, blend);
  end;

  procedure r_Draw_Filter (l, t, r, b: Integer; rr, gg, bb, aa: Byte);
  begin
    ASSERT(r >= l);
    ASSERT(b >= t);
    glEnable(GL_BLEND);
    glBlendFunc(GL_ZERO, GL_SRC_COLOR);
    glDisable(GL_TEXTURE_2D);
    glColor4ub(rr, gg, bb, aa);
    glBegin(GL_QUADS);
      glVertex2i(l, t);
      glVertex2i(r, t);
      glVertex2i(r, b);
      glVertex2i(l, b);
    glEnd;
  end;

  procedure r_Draw_FillRect (l, t, r, b: Integer; rr, gg, bb, aa: Byte);
  begin
    ASSERT(r >= l);
    ASSERT(b >= t);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_TEXTURE_2D);
    glColor4ub(rr, gg, bb, aa);
    glBegin(GL_QUADS);
      glVertex2i(l, t);
      glVertex2i(r, t);
      glVertex2i(r, b);
      glVertex2i(l, b);
    glEnd;
  end;

  procedure r_Draw_InvertRect (l, t, r, b: Integer; rr, gg, bb, aa: Byte);
  begin
    ASSERT(r >= l);
    ASSERT(b >= t);
    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ZERO);
    glDisable(GL_TEXTURE_2D);
    glColor4ub(rr, gg, bb, aa);
    glBegin(GL_QUADS);
      glVertex2i(l, t);
      glVertex2i(r, t);
      glVertex2i(r, b);
      glVertex2i(l, b);
    glEnd;
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

end.
