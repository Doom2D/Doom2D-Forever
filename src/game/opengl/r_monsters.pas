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
unit r_monsters;

interface

  procedure r_Monsters_Load;
  procedure r_Monsters_Free;
  procedure r_Monsters_Draw;
  procedure r_Monsters_DrawHealth;

implementation

  uses
    SysUtils, Classes, Math, e_log,
    r_graphics, g_options, r_animations, r_game,
    MAPDEF,
    g_base, g_basic, g_game, g_phys,
    g_monsters
  ;

  type
    TMonsterDirected = array [TDirection.D_LEFT..TDirection.D_RIGHT] of DWORD;
    TMonsterAnims = array [ANIM_SLEEP..ANIM_PAIN] of TMonsterDirected;

  var
    VileFire: DWORD;
    monFrames: array [MONSTER_DEMON..MONSTER_MAN] of TMonsterAnims;

  procedure r_Monsters_Free;
  begin
    g_Frames_DeleteByName('FRAMES_MONSTER_BARREL_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_BARREL_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_BARREL_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_MESS');
    g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_ATTACK_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_SERG_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_SERG_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_SERG_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_SERG_MESS');
    g_Frames_DeleteByName('FRAMES_MONSTER_SERG_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_SERG_ATTACK_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_SERG_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_MAN_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_MAN_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_MAN_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_MAN_MESS');
    g_Frames_DeleteByName('FRAMES_MONSTER_MAN_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_MAN_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_SLEEP_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_GO_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_MESS');
    g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_ATTACK_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_PAIN_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_IMP_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_IMP_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_IMP_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_IMP_MESS');
    g_Frames_DeleteByName('FRAMES_MONSTER_IMP_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_IMP_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_DEMON_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_DEMON_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_DEMON_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_DEMON_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_DEMON_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_SOUL_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_SOUL_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_SOUL_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_SOUL_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_SOUL_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_FISH_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_FISH_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_FISH_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_FISH_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_FISH_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_SPIDER_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_SPIDER_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_SPIDER_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_SPIDER_PAIN_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_SPIDER_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_SPIDER_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_BSP_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_BSP_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_BSP_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_BSP_PAIN_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_BSP_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_BSP_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_CACO_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_CACO_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_CACO_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_CACO_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_CACO_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_PAIN_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_PAIN_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_PAIN_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_PAIN_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_PAIN_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_BARON_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_BARON_SLEEP_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_BARON_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_BARON_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_BARON_PAIN_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_BARON_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_BARON_ATTACK_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_BARON_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_SLEEP_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_PAIN_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_ATTACK_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_MANCUB_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_MANCUB_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_MANCUB_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_MANCUB_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_MANCUB_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_SLEEP_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_PAIN_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_ATTACK_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_ATTACK2');
    g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_ATTACK2_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_VILE_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_VILE_SLEEP_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_VILE_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_VILE_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_VILE_PAIN_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_VILE_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_VILE_ATTACK_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_VILE_ATTACK2');
    g_Frames_DeleteByName('FRAMES_MONSTER_VILE_ATTACK2_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_VILE_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_ROBO_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_ROBO_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_ROBO_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_ROBO_ATTACK2');
    g_Frames_DeleteByName('FRAMES_MONSTER_ROBO_DIE');
    g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_SLEEP');
    g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_SLEEP_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_GO');
    g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_GO_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_PAIN');
    g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_PAIN_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_ATTACK');
    g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_ATTACK_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_ATTACK2');
    g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_ATTACK2_L');
    g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_DIE');
  end;

  procedure r_Monsters_Load;
    var m, a: Integer; s, info: String; FramesID: DWORD;
  begin
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARREL_SLEEP', GameWAD+':MTEXTURES\BARREL_SLEEP', 64, 64, 3);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARREL_DIE', GameWAD+':MTEXTURES\BARREL_DIE', 64, 64, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARREL_PAIN', GameWAD+':MTEXTURES\BARREL_PAIN', 64, 64, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_SLEEP', GameWAD+':MTEXTURES\ZOMBY_SLEEP', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_GO', GameWAD+':MTEXTURES\ZOMBY_GO', 64, 64, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_DIE', GameWAD+':MTEXTURES\ZOMBY_DIE', 64, 64, 6);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_MESS', GameWAD+':MTEXTURES\ZOMBY_MESS', 64, 64, 9);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_ATTACK', GameWAD+':MTEXTURES\ZOMBY_ATTACK', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_ATTACK_L', GameWAD+':MTEXTURES\ZOMBY_ATTACK_L', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_PAIN', GameWAD+':MTEXTURES\ZOMBY_PAIN', 64, 64, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_SLEEP', GameWAD+':MTEXTURES\SERG_SLEEP', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_GO', GameWAD+':MTEXTURES\SERG_GO', 64, 64, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_DIE', GameWAD+':MTEXTURES\SERG_DIE', 64, 64, 5);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_MESS', GameWAD+':MTEXTURES\SERG_MESS', 64, 64, 9);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_ATTACK', GameWAD+':MTEXTURES\SERG_ATTACK', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_ATTACK_L', GameWAD+':MTEXTURES\SERG_ATTACK_L', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_PAIN', GameWAD+':MTEXTURES\SERG_PAIN', 64, 64, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_SLEEP', GameWAD+':MTEXTURES\MAN_SLEEP', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_GO', GameWAD+':MTEXTURES\MAN_GO', 64, 64, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_DIE', GameWAD+':MTEXTURES\MAN_DIE', 64, 64, 7);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_MESS', GameWAD+':MTEXTURES\MAN_MESS', 64, 64, 9);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_ATTACK', GameWAD+':MTEXTURES\MAN_ATTACK', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_PAIN', GameWAD+':MTEXTURES\MAN_PAIN', 64, 64, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_SLEEP', GameWAD+':MTEXTURES\CGUN_SLEEP', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_SLEEP_L', GameWAD+':MTEXTURES\CGUN_SLEEP_L', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_GO', GameWAD+':MTEXTURES\CGUN_GO', 64, 64, 4);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_GO_L', GameWAD+':MTEXTURES\CGUN_GO_L', 64, 64, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_DIE', GameWAD+':MTEXTURES\CGUN_DIE', 64, 64, 7);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_MESS', GameWAD+':MTEXTURES\CGUN_MESS', 64, 64, 6);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_ATTACK', GameWAD+':MTEXTURES\CGUN_ATTACK', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_ATTACK_L', GameWAD+':MTEXTURES\CGUN_ATTACK_L', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_PAIN', GameWAD+':MTEXTURES\CGUN_PAIN', 64, 64, 1);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_PAIN_L', GameWAD+':MTEXTURES\CGUN_PAIN_L', 64, 64, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_SLEEP', GameWAD+':MTEXTURES\IMP_SLEEP', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_GO', GameWAD+':MTEXTURES\IMP_GO', 64, 64, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_DIE', GameWAD+':MTEXTURES\IMP_DIE', 64, 64, 5);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_MESS', GameWAD+':MTEXTURES\IMP_MESS', 64, 64, 8);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_ATTACK', GameWAD+':MTEXTURES\IMP_ATTACK', 64, 64, 3);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_PAIN', GameWAD+':MTEXTURES\IMP_PAIN', 64, 64, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_SLEEP', GameWAD+':MTEXTURES\DEMON_SLEEP', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_GO', GameWAD+':MTEXTURES\DEMON_GO', 64, 64, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_DIE', GameWAD+':MTEXTURES\DEMON_DIE', 64, 64, 6);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_ATTACK', GameWAD+':MTEXTURES\DEMON_ATTACK', 64, 64, 3);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_PAIN', GameWAD+':MTEXTURES\DEMON_PAIN', 64, 64, 1);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_SLEEP', GameWAD+':MTEXTURES\SOUL_SLEEP', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_GO', GameWAD+':MTEXTURES\SOUL_GO', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_PAIN', GameWAD+':MTEXTURES\SOUL_PAIN', 64, 64, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_ATTACK', GameWAD+':MTEXTURES\SOUL_ATTACK', 64, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_DIE', GameWAD+':MTEXTURES\SOUL_DIE', 128, 128, 7);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_SLEEP', GameWAD+':MTEXTURES\FISH_SLEEP', 32, 32, 2);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_GO', GameWAD+':MTEXTURES\FISH_GO', 32, 32, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_PAIN', GameWAD+':MTEXTURES\FISH_PAIN', 32, 32, 3);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_ATTACK', GameWAD+':MTEXTURES\FISH_ATTACK', 32, 32, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_DIE', GameWAD+':MTEXTURES\FISH_DIE', 32, 32, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_SLEEP', GameWAD+':MTEXTURES\SPIDER_SLEEP', 256, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_GO', GameWAD+':MTEXTURES\SPIDER_GO', 256, 128, 6);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_PAIN', GameWAD+':MTEXTURES\SPIDER_PAIN', 256, 128, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_PAIN_L', GameWAD+':MTEXTURES\SPIDER_PAIN_L', 256, 128, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_ATTACK', GameWAD+':MTEXTURES\SPIDER_ATTACK', 256, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_DIE', GameWAD+':MTEXTURES\SPIDER_DIE', 256, 128, 10);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_SLEEP', GameWAD+':MTEXTURES\BSP_SLEEP', 128, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_GO', GameWAD+':MTEXTURES\BSP_GO', 128, 64, 6);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_PAIN', GameWAD+':MTEXTURES\BSP_PAIN', 128, 64, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_PAIN_L', GameWAD+':MTEXTURES\BSP_PAIN_L', 128, 64, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_ATTACK', GameWAD+':MTEXTURES\BSP_ATTACK', 128, 64, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_DIE', GameWAD+':MTEXTURES\BSP_DIE', 128, 64, 7);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_SLEEP', GameWAD+':MTEXTURES\CACO_SLEEP', 128, 128, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_GO', GameWAD+':MTEXTURES\CACO_GO', 128, 128, 1);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_PAIN', GameWAD+':MTEXTURES\CACO_PAIN', 128, 128, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_ATTACK', GameWAD+':MTEXTURES\CACO_ATTACK', 128, 128, 6);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_DIE', GameWAD+':MTEXTURES\CACO_DIE', 128, 128, 7);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_SLEEP', GameWAD+':MTEXTURES\PAIN_SLEEP', 128, 128, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_GO', GameWAD+':MTEXTURES\PAIN_GO', 128, 128, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_PAIN', GameWAD+':MTEXTURES\PAIN_PAIN', 128, 128, 1);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_ATTACK', GameWAD+':MTEXTURES\PAIN_ATTACK', 128, 128, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_DIE', GameWAD+':MTEXTURES\PAIN_DIE', 128, 128, 7);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_SLEEP', GameWAD+':MTEXTURES\BARON_SLEEP', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_SLEEP_L', GameWAD+':MTEXTURES\BARON_SLEEP_L', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_GO', GameWAD+':MTEXTURES\BARON_GO', 128, 128, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_PAIN', GameWAD+':MTEXTURES\BARON_PAIN', 128, 128, 1);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_PAIN_L', GameWAD+':MTEXTURES\BARON_PAIN_L', 128, 128, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_ATTACK', GameWAD+':MTEXTURES\BARON_ATTACK', 128, 128, 3);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_ATTACK_L', GameWAD+':MTEXTURES\BARON_ATTACK_L', 128, 128, 3);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_DIE', GameWAD+':MTEXTURES\BARON_DIE', 128, 128, 7);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_SLEEP', GameWAD+':MTEXTURES\KNIGHT_SLEEP', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_SLEEP_L', GameWAD+':MTEXTURES\KNIGHT_SLEEP_L', 128, 128, 2);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_GO', GameWAD+':MTEXTURES\KNIGHT_GO', 128, 128, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_PAIN', GameWAD+':MTEXTURES\KNIGHT_PAIN', 128, 128, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_PAIN_L', GameWAD+':MTEXTURES\KNIGHT_PAIN_L', 128, 128, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_ATTACK', GameWAD+':MTEXTURES\KNIGHT_ATTACK', 128, 128, 3);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_ATTACK_L', GameWAD+':MTEXTURES\KNIGHT_ATTACK_L', 128, 128, 3);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_DIE', GameWAD+':MTEXTURES\KNIGHT_DIE', 128, 128, 7);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_SLEEP', GameWAD+':MTEXTURES\MANCUB_SLEEP', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_GO', GameWAD+':MTEXTURES\MANCUB_GO', 128, 128, 6);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_PAIN', GameWAD+':MTEXTURES\MANCUB_PAIN', 128, 128, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_ATTACK', GameWAD+':MTEXTURES\MANCUB_ATTACK', 128, 128, 3);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_DIE', GameWAD+':MTEXTURES\MANCUB_DIE', 128, 128, 10);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_SLEEP', GameWAD+':MTEXTURES\SKEL_SLEEP', 128, 128, 2);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_SLEEP_L', GameWAD+':MTEXTURES\SKEL_SLEEP_L', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_GO', GameWAD+':MTEXTURES\SKEL_GO', 128, 128, 6);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_PAIN', GameWAD+':MTEXTURES\SKEL_PAIN', 128, 128, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_PAIN_L', GameWAD+':MTEXTURES\SKEL_PAIN_L', 128, 128, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_ATTACK', GameWAD+':MTEXTURES\SKEL_ATTACK', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_ATTACK_L', GameWAD+':MTEXTURES\SKEL_ATTACK_L', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_ATTACK2', GameWAD+':MTEXTURES\SKEL_ATTACK2', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_ATTACK2_L', GameWAD+':MTEXTURES\SKEL_ATTACK2_L', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_DIE', GameWAD+':MTEXTURES\SKEL_DIE', 128, 128, 5);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_SLEEP', GameWAD+':MTEXTURES\VILE_SLEEP', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_SLEEP_L', GameWAD+':MTEXTURES\VILE_SLEEP_L', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_GO', GameWAD+':MTEXTURES\VILE_GO', 128, 128, 6);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_PAIN', GameWAD+':MTEXTURES\VILE_PAIN', 128, 128, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_PAIN_L', GameWAD+':MTEXTURES\VILE_PAIN_L', 128, 128, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_ATTACK', GameWAD+':MTEXTURES\VILE_ATTACK', 128, 128, 10);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_ATTACK_L', GameWAD+':MTEXTURES\VILE_ATTACK_L', 128, 128, 10);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_ATTACK2', GameWAD+':MTEXTURES\VILE_ATTACK2', 128, 128, 3);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_ATTACK2_L', GameWAD+':MTEXTURES\VILE_ATTACK2_L', 128, 128, 3);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_DIE', GameWAD+':MTEXTURES\VILE_DIE', 128, 128, 9);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_SLEEP', GameWAD+':MTEXTURES\ROBO_SLEEP', 128, 128, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_GO', GameWAD+':MTEXTURES\ROBO_GO', 128, 128, 12);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_ATTACK', GameWAD+':MTEXTURES\ROBO_ATTACK', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_ATTACK2', GameWAD+':MTEXTURES\ROBO_ATTACK2', 128, 128, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_DIE', GameWAD+':MTEXTURES\ROBO_DIE', 128, 128, 1);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_SLEEP', GameWAD+':MTEXTURES\CYBER_SLEEP', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_SLEEP_L', GameWAD+':MTEXTURES\CYBER_SLEEP_L', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_GO', GameWAD+':MTEXTURES\CYBER_GO', 128, 128, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_GO_L', GameWAD+':MTEXTURES\CYBER_GO_L', 128, 128, 4);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_PAIN', GameWAD+':MTEXTURES\CYBER_PAIN', 128, 128, 1);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_PAIN_L', GameWAD+':MTEXTURES\CYBER_PAIN_L', 128, 128, 1);

    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_ATTACK', GameWAD+':MTEXTURES\CYBER_ATTACK', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_ATTACK_L', GameWAD+':MTEXTURES\CYBER_ATTACK_L', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_ATTACK2', GameWAD+':MTEXTURES\CYBER_ATTACK2', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_ATTACK2_L', GameWAD+':MTEXTURES\CYBER_ATTACK2_L', 128, 128, 2);
    g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_DIE', GameWAD+':MTEXTURES\CYBER_DIE', 128, 128, 9);

    g_Frames_CreateWAD(nil, 'FRAMES_FIRE', GameWAD+':TEXTURES\FIRE', 64, 128, 8);

    g_Frames_Get(vilefire, 'FRAMES_FIRE');
    for m := MONSTER_DEMON to MONSTER_MAN do
    begin
      for a := ANIM_SLEEP to ANIM_PAIN do
      begin
        monFrames[m, a, TDirection.D_LEFT] := DWORD(-1);
        monFrames[m, a, TDirection.D_RIGHT] := DWORD(-1);
        if (ANIMTABLE[a].name <> '') and (MONSTER_ANIMTABLE[m].AnimSpeed[a] <> 0) then
        begin
          s := 'FRAMES_MONSTER_' + MONSTERTABLE[m].Name + '_' + ANIMTABLE[a].name;
          if not (g_Frames_Exists(s) and g_Frames_Get(FramesID, s)) then
          begin
            // Заменяем только ANIM_MESS на ANIM_DIE:
            if a <> ANIM_MESS then
              Continue;
            if g_Frames_Get(FramesID, 'FRAMES_MONSTER_' + MONSTERTABLE[m].Name + '_' + ANIMTABLE[ANIM_DIE].name) then
            begin
              monFrames[m, a, TDirection.D_RIGHT] := FramesID;
              monFrames[m, a, TDirection.D_LEFT] := FramesID;
              Continue;
            end;
          end;
          monFrames[m, a, TDirection.D_RIGHT] := FramesID;
          // Если есть отдельная левая анимация - загружаем:
          if MONSTER_ANIMTABLE[m].LeftAnim then
          begin
            s := 'FRAMES_MONSTER_' + MONSTERTABLE[m].Name + '_' + ANIMTABLE[a].name + '_L';
            if g_Frames_Exists(s) then
              g_Frames_Get(FramesID, s);
          end;
          monFrames[m, a, TDirection.D_LEFT] := FramesID;
        end
      end
    end
  end;

  procedure r_Monsters_Draw (constref monster: TMonster);
    var m: TMirrorType; dx, dy, c, fX, fY: Integer; o: TObj;
  begin
    with monster do
    begin
      //e_CharFont_Print(gMenuSmallFont, Obj.X + Obj.Rect.X, Obj.Y + Obj.Rect.Y, 'TYPE: ' + IntToStr(MonsterType));
      //e_CharFont_Print(gMenuSmallFont, Obj.X + Obj.Rect.X, Obj.Y + Obj.Rect.Y + 16, 'STATE: ' + IntToStr(MonsterState));

      Obj.lerp(gLerpFactor, fX, fY);

      // Если колдун стреляет, то рисуем огонь:
      if MonsterType = MONSTER_VILE then
        if MonsterState = MONSTATE_SHOOT then
          if GetPos(MonsterTargetUID, @o) then
            r_AnimationState_Draw(VileFire, VileFireAnim, o.X + o.Rect.X + (o.Rect.Width div 2) - 32, o.Y + o.Rect.Y + o.Rect.Height - 128, TMirrorType.None);

      // Не в области рисования не ресуем:
      //FIXME!
      if (g_dbg_scale = 1.0) then
      begin
        if not g_Collide(Obj.X + Obj.Rect.X, Obj.Y + Obj.Rect.Y, Obj.Rect.Width, Obj.Rect.Height, sX - 128, sY - 128, sWidth + 256, sHeight + 256) then
          Exit;
      end;

      // Эти монстры, умирая, не оставляют трупов:
      if MonsterState = MONSTATE_DEAD then
        case MonsterType of
          MONSTER_BARREL, MONSTER_SOUL, MONSTER_PAIN: Exit;
        end;

      // Есть что рисовать при текущем поведении:
      if monFrames[MonsterType, MonsterAnim, GameDirection] <> DWORD(-1) then
      begin
        // Если нет левой анимации или она совпадает с правой => отражаем правую:
        if (GameDirection = TDirection.D_LEFT) and ((not MONSTER_ANIMTABLE[MonsterType].LeftAnim) or (monFrames[MonsterType, MonsterAnim, TDirection.D_LEFT] = monFrames[MonsterType, MonsterAnim, TDirection.D_RIGHT])) and (MonsterType <> MONSTER_BARREL) then
          m := TMirrorType.Horizontal
        else
          m := TMirrorType.None;

        // Левая анимация => меняем смещение относительно центра:
        if (GameDirection = TDirection.D_LEFT) and (MonsterType <> MONSTER_BARREL) then
        begin
          dx := MONSTER_ANIMTABLE[MonsterType].AnimDeltaLeft[MonsterAnim].X;
          dy := MONSTER_ANIMTABLE[MonsterType].AnimDeltaLeft[MonsterAnim].Y;

          if m = TMirrorType.Horizontal then
          begin
            // Нет отдельной левой анимации
            // Расстояние от края текстуры до края визуального положения объекта на текстуре:
            c := (MONSTERTABLE[MonsterType].Rect.X - dx) + MONSTERTABLE[MonsterType].Rect.Width;
            // Расстояние от края хит бокса до края визуального положения объекта на текстуре:
            //dx := DirAnim[MonsterAnim, GameDirection].Width - c - MONSTERTABLE[MonsterType].Rect.X;
            dx := 64 - c - MONSTERTABLE[MonsterType].Rect.X; // !!! ^^^
            // Т.к. двигать текстуру нужно будет в противоположном направлении:
            dx := -dx;
            // Это значит: dX := -frameWidth - animDeltaX + hitX + hitWidth + hitX
          end
        end
        else // Правая анимация
        begin
          dx := MONSTER_ANIMTABLE[MonsterType].AnimDeltaRight[MonsterAnim].X;
          dy := MONSTER_ANIMTABLE[MonsterType].AnimDeltaRight[MonsterAnim].Y;
        end;

        r_AnimationState_Draw(monFrames[MonsterType, MonsterAnim, GameDirection], DirAnim[MonsterAnim, GameDirection], fX + dx, fY + dy, m);
      end;

      if g_debug_Frames then
      begin
        e_DrawQuad(Obj.X + Obj.Rect.X, Obj.Y + Obj.Rect.Y, Obj.X + Obj.Rect.X + Obj.Rect.Width - 1, Obj.Y + Obj.Rect.Y + Obj.Rect.Height - 1, 0, 255, 0);
      end
    end
  end;

  procedure r_Monsters_Draw;
    var a: Integer;
  begin
    if gMonsters <> nil then
      for a := 0 to High(gMonsters) do
        if (gMonsters[a] <> nil) then r_Monsters_Draw(gMonsters[a]);
  end;

  procedure r_Monsters_DrawHealth;
    var a: Integer; fW, fH: Byte;
  begin
    if gMonsters = nil then Exit;
    e_TextureFontGetSize(gStdFont, fW, fH);
    for a := 0 to High(gMonsters) do
    begin
      if gMonsters[a] <> nil then
      begin
        e_TextureFontPrint(gMonsters[a].Obj.X + gMonsters[a].Obj.Rect.X,
        gMonsters[a].Obj.Y + gMonsters[a].Obj.Rect.Y + gMonsters[a].Obj.Rect.Height - fH,
        IntToStr(gMonsters[a].MonsterHealth), gStdFont);
      end
    end
  end;

end.
