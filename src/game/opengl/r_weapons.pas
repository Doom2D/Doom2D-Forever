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

  procedure r_Weapon_Load;
  procedure r_Weapon_Free;
  procedure r_Weapon_Draw;

implementation

  uses
    SysUtils, Classes, Math,
    MAPDEF,
    r_graphics, r_animations, r_textures,
    g_base, g_basic, g_game, g_options,
    g_weapons
  ;

  var
    ShotTexture, ShotFrames: array [WEAPON_KASTET..WEAPON_SKEL_FIRE] of DWORD;

  procedure r_Weapon_Load;
  begin
    g_Texture_CreateWADEx('TEXTURE_WEAPON_ROCKET', GameWAD+':TEXTURES\BROCKET');
    g_Frames_CreateWAD(nil, 'FRAMES_WEAPON_SKELFIRE', GameWAD+':TEXTURES\BSKELFIRE', 64, 16, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_WEAPON_BFG', GameWAD+':TEXTURES\BBFG', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_WEAPON_PLASMA', GameWAD+':TEXTURES\BPLASMA', 16, 16, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_WEAPON_IMPFIRE', GameWAD+':TEXTURES\BIMPFIRE', 16, 16, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_WEAPON_BSPFIRE', GameWAD+':TEXTURES\BBSPFIRE', 16, 16, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_WEAPON_CACOFIRE', GameWAD+':TEXTURES\BCACOFIRE', 16, 16, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_WEAPON_BARONFIRE', GameWAD+':TEXTURES\BBARONFIRE', 64, 16, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_WEAPON_MANCUBFIRE', GameWAD+':TEXTURES\BMANCUBFIRE', 64, 32, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_EXPLODE_ROCKET', GameWAD+':TEXTURES\EROCKET', 128, 128, 6);
    g_Frames_CreateWAD(nil, 'FRAMES_EXPLODE_SKELFIRE', GameWAD+':TEXTURES\ESKELFIRE', 64, 64, 3);
    g_Frames_CreateWAD(nil, 'FRAMES_EXPLODE_BFG', GameWAD+':TEXTURES\EBFG', 128, 128, 6);
    g_Frames_CreateWAD(nil, 'FRAMES_EXPLODE_IMPFIRE', GameWAD+':TEXTURES\EIMPFIRE', 64, 64, 3);
    g_Frames_CreateWAD(nil, 'FRAMES_BFGHIT', GameWAD+':TEXTURES\BFGHIT', 64, 64, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_FIRE', GameWAD+':TEXTURES\FIRE', 64, 128, 8);
    g_Frames_CreateWAD(nil, 'FRAMES_FLAME', GameWAD+':TEXTURES\FLAME', 32, 32, 11);
    g_Frames_CreateWAD(nil, 'FRAMES_EXPLODE_PLASMA', GameWAD+':TEXTURES\EPLASMA', 32, 32, 4, True);
    g_Frames_CreateWAD(nil, 'FRAMES_EXPLODE_BSPFIRE', GameWAD+':TEXTURES\EBSPFIRE', 32, 32, 5);
    g_Frames_CreateWAD(nil, 'FRAMES_EXPLODE_CACOFIRE', GameWAD+':TEXTURES\ECACOFIRE', 64, 64, 3);
    g_Frames_CreateWAD(nil, 'FRAMES_EXPLODE_BARONFIRE', GameWAD+':TEXTURES\EBARONFIRE', 64, 64, 3);
    g_Frames_CreateWAD(nil, 'FRAMES_SMOKE', GameWAD+':TEXTURES\SMOKE', 32, 32, 10, False);

    g_Texture_CreateWADEx('TEXTURE_SHELL_BULLET', GameWAD+':TEXTURES\EBULLET');
    g_Texture_CreateWADEx('TEXTURE_SHELL_SHELL', GameWAD+':TEXTURES\ESHELL');

    (* WEAPON_ROCKETLAUNCHER *)
    g_Texture_Get('TEXTURE_WEAPON_ROCKET', ShotTexture[WEAPON_ROCKETLAUNCHER]);

    (* WEAPON_PLASMA *)
    g_Frames_Get(ShotFrames[WEAPON_PLASMA], 'FRAMES_WEAPON_PLASMA');
    // Animation := TAnimation.Create(FramesID, True, 5);

    (* WEAPON_BFG *)
    g_Frames_Get(ShotFrames[WEAPON_BFG], 'FRAMES_WEAPON_BFG');
    // Animation := TAnimation.Create(FramesID, True, 6);

    (* WEAPON_FLAMETHROWER *)
    //g_Frames_Get(ShotTexture[WEAPON_FLAMETHROWER], 'FRAMES_FLAME');
    //g_Frames_Get(ShotFrames[WEAPON_FLAMETHROWER], 'FRAMES_FLAME');

    (* WEAPON_IMP_FIRE *)
    g_Frames_Get(ShotFrames[WEAPON_IMP_FIRE], 'FRAMES_WEAPON_IMPFIRE');
    // Animation := TAnimation.Create(FramesID, True, 4);

    (* WEAPON_CACO_FIRE *)
    g_Frames_Get(ShotFrames[WEAPON_CACO_FIRE], 'FRAMES_WEAPON_CACOFIRE');
    // Animation := TAnimation.Create(FramesID, True, 4);

    (* WEAPON_MANCUB_FIRE *)
    g_Frames_Get(ShotFrames[WEAPON_MANCUB_FIRE], 'FRAMES_WEAPON_MANCUBFIRE');
    // Animation := TAnimation.Create(FramesID, True, 4);

    (* WEAPON_BARON_FIRE *)
    g_Frames_Get(ShotFrames[WEAPON_BARON_FIRE], 'FRAMES_WEAPON_BARONFIRE');
    // Animation := TAnimation.Create(FramesID, True, 4);

    (* WEAPON_BSP_FIRE *)
    g_Frames_Get(ShotFrames[WEAPON_BSP_FIRE], 'FRAMES_WEAPON_BSPFIRE');
    // Animation := TAnimation.Create(FramesID, True, 4);

    (* WEAPON_SKEL_FIRE *)
    g_Frames_Get(ShotFrames[WEAPON_SKEL_FIRE], 'FRAMES_WEAPON_SKELFIRE');
    // Animation := TAnimation.Create(FramesID, True, 5);
  end;

  procedure r_Weapon_Free;
  begin
    g_Texture_Delete('TEXTURE_WEAPON_ROCKET');
    g_Frames_DeleteByName('FRAMES_WEAPON_BFG');
    g_Frames_DeleteByName('FRAMES_WEAPON_PLASMA');
    g_Frames_DeleteByName('FRAMES_WEAPON_IMPFIRE');
    g_Frames_DeleteByName('FRAMES_WEAPON_BSPFIRE');
    g_Frames_DeleteByName('FRAMES_WEAPON_CACOFIRE');
    g_Frames_DeleteByName('FRAMES_WEAPON_MANCUBFIRE');
    g_Frames_DeleteByName('FRAMES_EXPLODE_ROCKET');
    g_Frames_DeleteByName('FRAMES_EXPLODE_BFG');
    g_Frames_DeleteByName('FRAMES_EXPLODE_IMPFIRE');
    g_Frames_DeleteByName('FRAMES_BFGHIT');
    g_Frames_DeleteByName('FRAMES_FIRE');
    g_Frames_DeleteByName('FRAMES_EXPLODE_PLASMA');
    g_Frames_DeleteByName('FRAMES_EXPLODE_BSPFIRE');
    g_Frames_DeleteByName('FRAMES_EXPLODE_CACOFIRE');
    g_Frames_DeleteByName('FRAMES_SMOKE');
    g_Frames_DeleteByName('FRAMES_WEAPON_BARONFIRE');
    g_Frames_DeleteByName('FRAMES_EXPLODE_BARONFIRE');
  end;

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
              r_AnimationState_DrawEx(ShotFrames[Shots[i].ShotType], Animation, fX, fY, 0, TMirrorType.None, False, p, a)
            else
              r_AnimationState_Draw(ShotFrames[Shots[i].ShotType], Animation, fX, fY, 0, TMirrorType.None, False);
          end
          else if ShotTexture[Shots[i].ShotType] <> 0 then
          begin
            if (Shots[i].ShotType = WEAPON_ROCKETLAUNCHER) then
              e_DrawAdv(ShotTexture[Shots[i].ShotType], fX, fY, 0, True, False, a, @p, TMirrorType.None)
            else if (Shots[i].ShotType <> WEAPON_FLAMETHROWER) then
              e_Draw(ShotTexture[Shots[i].ShotType], fX, fY, 0, True, False);
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
