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
    g_textures,
    r_textures
  ;

  procedure r_Draw_Texture (img: TGLTexture; x, y, w, h: Integer; flip: Boolean);
  procedure r_Draw_TextureRepeat (img: TGLTexture; x, y, w, h: Integer; flip: Boolean);

  procedure r_Draw_MultiTextureRepeat (m: TGLMultiTexture; const a: TAnimState; x, y, w, h: Integer; flip: Boolean);
  procedure r_Draw_MultiTextureRepeatRotate (img: TGLMultiTexture; const anim: TAnimState; x, y, w, h: Integer; flip: Boolean; rx, ry, a: Integer);

  procedure r_Draw_Filter (l, t, r, b: Integer; rr, gg, bb, aa: Byte);

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

  procedure DrawTile (tile: TGLAtlasNode; x, y, w, h: Integer; flip: Boolean);
    var nw, nh, ax, bx, ay, by: GLfloat; l, t, r, b: Integer;
  begin
    if tile = nil then
    begin
      glColor3ub(255, 0, 0);
      glDisable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glDisable(GL_TEXTURE_2D);
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
      glColor3ub(255, 255, 255);
      glBindTexture(GL_TEXTURE_2D, tile.id);
      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
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

(*
  procedure r_Draw_Texture (img: TGLTexture; x, y, w, h: Integer; flip: Boolean);
    var i, j, offx, offy, nw, nh: Integer; n: TGLAtlasNode;
  begin
    ASSERT(w >= 0);
    ASSERT(h >= 0);
    if img = nil then
      DrawTile(nil, x, y, w, h, flip)
    else
    begin
      offx := 0;
      offy := 0;
      nw := w div img.cols;
      nh := h div img.lines;
      for j := 0 to img.lines - 1 do
      begin
        for i := 0 to img.cols - 1 do
        begin
          n := img.GetTile(i, j);
          ASSERT(n <> nil);
          DrawTile(n, x + offx, y + offy, nw, nh, flip);
          offx := offx + nw;
        end;
        offx := 0;
        offy := offy + nh;
      end
    end
  end;
*)

  procedure r_Draw_Texture (img: TGLTexture; x, y, w, h: Integer; flip: Boolean);
    var i, j, offx, offy: Integer; n: TGLAtlasNode;
  begin
    ASSERT(w >= 0);
    ASSERT(h >= 0);
    if img = nil then
      DrawTile(nil, x, y, w, h, flip)
    else
    begin
      glPushMatrix;
      glScalef(w / img.width, h / img.height, 1);
      offx := 0;
      offy := 0;
      for j := 0 to img.lines - 1 do
      begin
        for i := 0 to img.cols - 1 do
        begin
          n := img.GetTile(i, j);
          ASSERT(n <> nil);
          DrawTile(n, x + offx, y + offy, n.width, n.height, flip);
          offx := offx + n.width;
        end;
        offx := 0;
        offy := offy + n.height;
      end;
      glPopMatrix;
    end
  end;

  procedure r_Draw_TextureRepeat (img: TGLTexture; x, y, w, h: Integer; flip: Boolean);
    var i, j: Integer;
  begin
    ASSERT(w >= 0);
    ASSERT(h >= 0);
    if img = nil then
      r_Draw_Texture(nil, x, y, w, h, flip)
    else
      for j := 0 to h div img.height - 1 do
        for i := 0 to w div img.width - 1 do
          r_Draw_Texture(img, x + i * img.width, y + j * img.height, img.width, img.height, flip);
  end;

  procedure r_Draw_MultiTextureRepeat (m: TGLMultiTexture; const a: TAnimState; x, y, w, h: Integer; flip: Boolean);
    var img: TGLTexture; cur, total, i: Integer;
  begin
    ASSERT(a.IsValid());
    if m = nil then
      r_Draw_TextureRepeat(nil, x, y, w, h, flip)
    else
    begin
      if m.BackAnim then
      begin
        total := m.count * 2 - 1;
        cur := a.CurrentFrame mod total;
        if cur < m.count then i := cur else i := total - cur - 1;
      end
      else
        i := a.CurrentFrame mod m.count;
      img := m.GetTexture(i);
      r_Draw_TextureRepeat(img, x, y, w, h, flip)
    end
  end;

  procedure r_Draw_MultiTextureRepeatRotate (img: TGLMultiTexture; const anim: TAnimState; x, y, w, h: Integer; flip: Boolean; rx, ry, a: Integer);
    var i, j: Integer;
  begin
    ASSERT(w >= 0);
    ASSERT(h >= 0);
    if a <> 0 then
    begin
      glPushMatrix;
      glTranslatef(x + rx, y + ry, 0);
      glRotatef(a, 0, 0, 1);
      glTranslatef(-(x + rx), -(y + ry), 0);
      r_Draw_MultiTextureRepeat(img, anim, x, y, w, h, flip);
      glPopMatrix;
    end
    else
      r_Draw_MultiTextureRepeat(img, anim, x, y, w, h, flip);
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

end.
