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
unit r_playermodel;

interface

  uses g_playermodel; // TPlayerModel

  procedure r_PlayerModel_Initialize;
  procedure r_PlayerModel_Finalize;
  procedure r_PlayerModel_Draw (pm: TPlayerModel; X, Y: Integer; Alpha: Byte = 0);

implementation

  uses
    SysUtils, Classes, Math,
    MAPDEF,
    e_graphics,
    g_basic, g_map, g_weapons, g_textures, g_main
  ;

  const
    WeapNames: Array [WP_FIRST + 1..WP_LAST] of String = ('csaw', 'hgun', 'sg', 'ssg', 'mgun', 'rkt', 'plz', 'bfg', 'spl', 'flm');

  var
    WeaponID: Array [WP_FIRST + 1..WP_LAST, W_POS_NORMAL..W_POS_DOWN, W_ACT_NORMAL..W_ACT_FIRE] of DWORD;

procedure r_PlayerModel_Initialize;
var
  a: Integer;
begin
  for a := WP_FIRST + 1 to WP_LAST do
  begin
    g_Texture_CreateWAD(WeaponID[a][W_POS_NORMAL][W_ACT_NORMAL], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a]));
    g_Texture_CreateWAD(WeaponID[a][W_POS_NORMAL][W_ACT_FIRE], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_FIRE');
    g_Texture_CreateWAD(WeaponID[a][W_POS_UP][W_ACT_NORMAL], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_UP');
    g_Texture_CreateWAD(WeaponID[a][W_POS_UP][W_ACT_FIRE], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_UP_FIRE');
    g_Texture_CreateWAD(WeaponID[a][W_POS_DOWN][W_ACT_NORMAL], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_DN');
    g_Texture_CreateWAD(WeaponID[a][W_POS_DOWN][W_ACT_FIRE], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_DN_FIRE');
  end;
end;

  procedure r_PlayerModel_Finalize;
    var a, b, c: Integer;
  begin
    for a := WP_FIRST + 1 to WP_LAST do
      for b := W_POS_NORMAL to W_POS_DOWN do
        for c := W_ACT_NORMAL to W_ACT_FIRE do
          e_DeleteTexture(WeaponID[a][b][c])
  end;

procedure r_PlayerModel_Draw (pm: TPlayerModel; X, Y: Integer; Alpha: Byte = 0);
var
  Mirror: TMirrorType;
  pos, act: Byte;
  p: TDFPoint;
begin
// Флаги:
  if pm.Direction = TDirection.D_LEFT then
    Mirror := TMirrorType.None
  else
    Mirror := TMirrorType.Horizontal;

  if (pm.Flag <> FLAG_NONE) and (pm.FlagAnim <> nil) and (not (pm.CurrentAnimation in [A_DIE1, A_DIE2])) then
  begin
    p.X := IfThen(pm.Direction = TDirection.D_LEFT, FLAG_BASEPOINT.X, 64 - FLAG_BASEPOINT.X);
    p.Y := FLAG_BASEPOINT.Y;

    pm.FlagAnim.DrawEx(X+IfThen(pm.Direction = TDirection.D_LEFT, pm.FlagPoint.X-1, 2*FLAG_BASEPOINT.X-pm.FlagPoint.X+1)-FLAG_BASEPOINT.X,
                     Y+pm.FlagPoint.Y-FLAG_BASEPOINT.Y+1, Mirror, p,
                     IfThen(pm.Direction = TDirection.D_RIGHT, pm.FlagAngle, -pm.FlagAngle));
  end;

// Оружие:
  if pm.Direction = TDirection.D_RIGHT then
    Mirror := TMirrorType.None
  else
    Mirror := TMirrorType.Horizontal;

  if pm.DrawWeapon and (not (pm.CurrentAnimation in [A_DIE1, A_DIE2, A_PAIN])) and  (pm.CurrentWeapon in [WP_FIRST + 1..WP_LAST]) then
  begin
    if pm.CurrentAnimation in [A_SEEUP, A_ATTACKUP] then
      pos := W_POS_UP
    else
      if pm.CurrentAnimation in [A_SEEDOWN, A_ATTACKDOWN] then
        pos := W_POS_DOWN
      else
        pos := W_POS_NORMAL;

    if (pm.CurrentAnimation in [A_ATTACK, A_ATTACKUP, A_ATTACKDOWN]) or pm.Fire then
      act := W_ACT_FIRE
    else
      act := W_ACT_NORMAL;

    if Alpha < 201 then
      e_Draw(WeaponID[pm.CurrentWeapon][pos][act],
             X + pm.WeaponPoints[pm.CurrentWeapon, pm.CurrentAnimation, pm.Direction,
             pm.Anim[TDirection.D_RIGHT][pm.CurrentAnimation].CurrentFrame].X,
             Y + pm.WeaponPoints[pm.CurrentWeapon, pm.CurrentAnimation, pm.Direction,
             pm.Anim[TDirection.D_RIGHT][pm.CurrentAnimation].CurrentFrame].Y,
             0, True, False, Mirror);
  end;

// Модель:
  if (pm.Direction = TDirection.D_LEFT) and (pm.Anim[TDirection.D_LEFT][pm.CurrentAnimation] <> nil) then
  begin
    pm.Anim[TDirection.D_LEFT][pm.CurrentAnimation].Alpha := Alpha;
    pm.Anim[TDirection.D_LEFT][pm.CurrentAnimation].Draw(X, Y, TMirrorType.None);
  end
  else
  begin
    pm.Anim[TDirection.D_RIGHT][pm.CurrentAnimation].Alpha := Alpha;
    pm.Anim[TDirection.D_RIGHT][pm.CurrentAnimation].Draw(X, Y, Mirror);
  end;

// Маска модели:
  e_Colors := pm.Color;

  if (pm.Direction = TDirection.D_LEFT) and (pm.MaskAnim[TDirection.D_LEFT][pm.CurrentAnimation] <> nil) then
  begin
    pm.MaskAnim[TDirection.D_LEFT][pm.CurrentAnimation].Alpha := Alpha;
    pm.MaskAnim[TDirection.D_LEFT][pm.CurrentAnimation].Draw(X, Y, TMirrorType.None);
  end
  else
  begin
    pm.MaskAnim[TDirection.D_RIGHT][pm.CurrentAnimation].Alpha := Alpha;
    pm.MaskAnim[TDirection.D_RIGHT][pm.CurrentAnimation].Draw(X, Y, Mirror);
  end;

  e_Colors.R := 255;
  e_Colors.G := 255;
  e_Colors.B := 255;
end;

end.
