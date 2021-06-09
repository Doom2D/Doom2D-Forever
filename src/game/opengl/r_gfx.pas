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
unit r_gfx;

interface

  procedure r_GFX_Draw;

implementation

  uses
    {$INCLUDE ../nogl/noGLuses.inc}
    SysUtils, Classes, Math,
    utils,
    g_base, r_graphics, g_options,
    g_game,
    g_gfx
  ;

procedure r_GFX_Draw;
  var
    a, len, fx, fy: Integer;
begin
  if not gpart_dbg_enabled then exit;

  if (Particles <> nil) then
  begin
    glDisable(GL_TEXTURE_2D);
         if (g_dbg_scale < 0.6) then glPointSize(1)
    else if (g_dbg_scale > 1.3) then glPointSize(g_dbg_scale+1)
    else glPointSize(2);
    glDisable(GL_POINT_SMOOTH);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glBegin(GL_POINTS);

    len := High(Particles);
    for a := 0 to len do
    begin
      with Particles[a] do
      begin
        if not alive then continue;
        if (x >= sX) and (y >= sY) and (x <= sX+sWidth) and (sY <= sY+sHeight) then
        begin
          fx := nlerp(oldx, x, gLerpFactor);
          fy := nlerp(oldy, y, gLerpFactor);
          glColor4ub(red, green, blue, alpha);
          glVertex2f(fx+0.37, fy+0.37);
        end;
      end;
    end;

    glEnd();

    glDisable(GL_BLEND);
  end;

  if (OnceAnims <> nil) then
  begin
    len := High(OnceAnims);
    for a := 0 to len do
    begin
      if (OnceAnims[a].Animation <> nil) then
      begin
        with OnceAnims[a] do
        begin
          fx := nlerp(oldx, x, gLerpFactor);
          fy := nlerp(oldy, y, gLerpFactor);
          Animation.Draw(x, y, TMirrorType.None);
        end;
      end;
    end;
  end;
end;

end.
