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
unit r_animations;

interface

  uses g_base, g_textures, MAPDEF; // TMirrorType, TAnimation, TDFPoint

  procedure r_Animation_Draw (t: TAnimation; x, y: Integer; mirror: TMirrorType);
  procedure r_Animation_DrawEx (t: TAnimation; x, y: Integer; mirror: TMirrorType; rpoint: TDFPoint; angle: SmallInt);

implementation

  uses r_graphics;

  procedure r_Animation_Draw (t: TAnimation; x, y: Integer; mirror: TMirrorType);
  begin
    if t.enabled then
      e_DrawAdv(framesArray[t.id].TexturesID[t.currentFrame], x, y, t.alpha, true, t.blending, 0, nil, mirror)
  end;

  procedure r_Animation_DrawEx (t: TAnimation; x, y: Integer; mirror: TMirrorType; rpoint: TDFPoint; angle: SmallInt);
  begin
    if t.enabled then
      e_DrawAdv(framesArray[t.id].TexturesID[t.currentFrame], x, y, t.alpha, true, t.blending, angle, @rpoint, mirror)
  end;

end.
