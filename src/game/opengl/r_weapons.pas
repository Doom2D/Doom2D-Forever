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
unit r_weapons;

interface

  procedure r_Weapon_Draw;

implementation

  uses
    SysUtils, Classes, Math,
    MAPDEF,
    r_graphics, r_animations,
    g_base, g_basic, g_game,
    g_weapons
  ;

  procedure r_Weapon_Draw;
    var i, fX, fY, xx, yy: Integer; a: SmallInt; p: TDFPoint;
  begin
    if Shots = nil then
      Exit;

    for i := 0 to High(Shots) do
    begin
      if Shots[i].ShotType <> 0 then
      begin
        with Shots[i] do
        begin
          if Shots[i].ShotType in [WEAPON_ROCKETLAUNCHER, WEAPON_BARON_FIRE, WEAPON_MANCUB_FIRE, WEAPON_SKEL_FIRE] then
            a := -GetAngle2(Obj.Vel.X, Obj.Vel.Y)
          else
            a := 0;

          Obj.lerp(gLerpFactor, fX, fY);
          p.X := Obj.Rect.Width div 2;
          p.Y := Obj.Rect.Height div 2;

          if Animation <> nil then
          begin
            if Shots[i].ShotType in [WEAPON_BARON_FIRE, WEAPON_MANCUB_FIRE, WEAPON_SKEL_FIRE] then
              r_Animation_DrawEx(Animation, fX, fY, TMirrorType.None, p, a)
            else
              r_Animation_Draw(Animation, fX, fY, TMirrorType.None);
          end
          else if TextureID <> 0 then
          begin
            if (Shots[i].ShotType = WEAPON_ROCKETLAUNCHER) then
              e_DrawAdv(TextureID, fX, fY, 0, True, False, a, @p, TMirrorType.None)
            else if (Shots[i].ShotType <> WEAPON_FLAMETHROWER) then
              e_Draw(TextureID, fX, fY, 0, True, False);
          end;

          if g_debug_Frames then
          begin
            xx := Obj.X + Obj.Rect.X;
            yy := Obj.Y + Obj.Rect.Y;
            e_DrawQuad(xx, yy, xx + Obj.Rect.Width - 1, yy + Obj.Rect.Height - 1, 0, 255, 0);
          end
        end
      end
    end
  end;

end.
