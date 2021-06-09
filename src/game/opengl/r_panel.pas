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
{$INCLUDE ../../shared/a_modes.inc}
unit r_panel;

interface

  uses g_panel, MAPDEF; // TPanel + TDFColor

  procedure r_Panel_Draw (constref p: TPanel; hasAmbient: Boolean; constref ambColor: TDFColor);
  procedure r_Panel_DrawShadowVolume (constref p: TPanel; lightX, lightY: Integer; radius: Integer);

implementation

  uses
    {$INCLUDE ../nogl/noGLuses.inc}
    SysUtils, Classes, Math, utils,
    r_graphics, g_options,
    g_base, g_basic, g_textures, g_game
  ;

  procedure Panel_Lerp (p: TPanel; t: Single; out tX, tY, tW, tH: Integer);
  begin
    if p.movingActive then
    begin
      tX := nlerp(p.OldX, p.X, t);
      tY := nlerp(p.OldY, p.Y, t);
      tW := nlerp(p.OldWidth, p.Width, t);
      tH := nlerp(p.OldHeight, p.Height, t);
    end
    else
    begin
      tX := p.X;
      tY := p.Y;
      tW := p.Width;
      tH := p.Height;
    end;
  end;

  // TODO: remove WITH operator

  procedure r_Panel_Draw (constref p: TPanel; hasAmbient: Boolean; constref ambColor: TDFColor);
    var tx, ty, tw, th, xx, yy: Integer; NoTextureID: DWORD; NW, NH: Word;
  begin
    with p do
    begin
      if {Enabled and} (FCurTexture >= 0) and (Width > 0) and (Height > 0) and (Alpha < 255) {and g_Collide(X, Y, Width, Height, sX, sY, sWidth, sHeight)} then
      begin
        Panel_Lerp(p, gLerpFactor, tx, ty, tw, th);
        if TextureIDs[FCurTexture].Anim then
        begin
          if TextureIDs[FCurTexture].AnTex = nil then
            Exit;
          for xx := 0 to tw div TextureWidth - 1 do
            for yy := 0 to th div TextureHeight - 1 do
              TextureIDs[FCurTexture].AnTex.Draw(tx + xx * TextureWidth, ty + yy * TextureHeight, TMirrorType.None);
        end
        else
        begin
          case TextureIDs[FCurTexture].Tex of
            LongWord(TEXTURE_SPECIAL_WATER): e_DrawFillQuad(tx, ty, tx + tw - 1, ty + th - 1, 0, 0, 255, 0, TBlending.Filter);
            LongWord(TEXTURE_SPECIAL_ACID1): e_DrawFillQuad(tx, ty, tx + tw - 1, ty + th - 1, 0, 230, 0, 0, TBlending.Filter);
            LongWord(TEXTURE_SPECIAL_ACID2): e_DrawFillQuad(tx, ty, tx + tw - 1, ty + th - 1, 230, 0, 0, 0, TBlending.Filter);
            LongWord(TEXTURE_NONE):
              if g_Texture_Get('NOTEXTURE', NoTextureID) then
              begin
                e_GetTextureSize(NoTextureID, @NW, @NH);
                e_DrawFill(NoTextureID, tx, ty, tw div NW, th div NH, 0, False, False);
              end
              else
              begin
                xx := tx + (tw div 2);
                yy := ty + (th div 2);
                e_DrawFillQuad(tx, ty, xx, yy, 255, 0, 255, 0);
                e_DrawFillQuad(xx, ty, tx + tw - 1, yy, 255, 255, 0, 0);
                e_DrawFillQuad(X, yy, xx, ty + th - 1, 255, 255, 0, 0);
                e_DrawFillQuad(xx, yy, tx + tw - 1, ty + th - 1, 255, 0, 255, 0);
              end;
            else
            begin
              if not movingActive then
                e_DrawFill(TextureIDs[FCurTexture].Tex, tx, ty, tw div TextureWidth, th div TextureHeight, Alpha, True, Blending, hasAmbient)
              else
                e_DrawFillX(TextureIDs[FCurTexture].Tex, tx, ty, tw, th, Alpha, True, Blending, g_dbg_scale, hasAmbient);
              if hasAmbient then
                e_AmbientQuad(tx, ty, tw, th, ambColor.r, ambColor.g, ambColor.b, ambColor.a);
            end
          end
        end
      end
    end
  end;

  procedure r_Panel_DrawShadowVolume (constref p: TPanel; lightX, lightY: Integer; radius: Integer);
    var tx, ty, tw, th: Integer;

    procedure extrude (x: Integer; y: Integer);
    begin
      glVertex2i(x + (x - lightX) * 500, y + (y - lightY) * 500);
      //e_WriteLog(Format('  : (%d,%d)', [x + (x - lightX) * 300, y + (y - lightY) * 300]), MSG_WARNING);
    end;

    procedure drawLine (x0: Integer; y0: Integer; x1: Integer; y1: Integer);
    begin
      // does this side facing the light?
      if ((x1 - x0) * (lightY - y0) - (lightX - x0) * (y1 - y0) >= 0) then exit;
      //e_WriteLog(Format('lightpan: (%d,%d)-(%d,%d)', [x0, y0, x1, y1]), MSG_WARNING);
      // this edge is facing the light, extrude and draw it
      glVertex2i(x0, y0);
      glVertex2i(x1, y1);
      extrude(x1, y1);
      extrude(x0, y0);
    end;

  begin
    with p do
    begin
      if radius < 4 then exit;
      if Enabled and (FCurTexture >= 0) and (Width > 0) and (Height > 0) and (Alpha < 255) {and g_Collide(X, Y, tw, th, sX, sY, sWidth, sHeight)} then
      begin
        Panel_Lerp(p, gLerpFactor, tx, ty, tw, th);
        if not TextureIDs[FCurTexture].Anim then
        begin
          case TextureIDs[FCurTexture].Tex of
            LongWord(TEXTURE_SPECIAL_WATER): exit;
            LongWord(TEXTURE_SPECIAL_ACID1): exit;
            LongWord(TEXTURE_SPECIAL_ACID2): exit;
            LongWord(TEXTURE_NONE): exit;
          end;
        end;
        if (tx + tw < lightX - radius) then exit;
        if (ty + th < lightY - radius) then exit;
        if (tx > lightX + radius) then exit;
        if (ty > lightY + radius) then exit;
        //e_DrawFill(TextureIDs[FCurTexture].Tex, X, Y, tw div TextureWidth, th div TextureHeight, Alpha, True, Blending);
        glBegin(GL_QUADS);
          drawLine(tx,      ty,      tx + tw, ty); // top
          drawLine(tx + tw, ty,      tx + tw, ty + th); // right
          drawLine(tx + tw, ty + th, tx,      ty + th); // bottom
          drawLine(tx,      ty + th, tx,      ty); // left
        glEnd;
      end
    end
  end;

end.
