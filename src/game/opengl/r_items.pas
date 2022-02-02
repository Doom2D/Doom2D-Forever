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
unit r_items;

interface

  uses MAPDEF; // ITEM_MAX

  procedure r_Items_Load;
  procedure r_Items_Free;
  procedure r_Items_Draw;
  procedure r_Items_DrawDrop;
  procedure r_Items_Update;

implementation

  uses
    SysUtils, Classes, Math,
    r_graphics, r_animations, r_textures,
    g_base, g_basic, g_game, g_options, g_textures,
    g_items
  ;

  var
    items: Array [0..ITEM_MAX] of record
      id: DWORD;
      anim: TAnimState;
    end;

  procedure LoadItem (i: Integer; name: String; w, h, delay, n: Integer; backanim: Boolean);
  begin
    g_Frames_CreateWAD(@items[i].id, '', GameWAD + ':TEXTURES\' + name, w, h, n, backanim);
    if backanim then n := n * 2 - 2;
    items[i].anim := TAnimState.Create(True, delay, n);
  end;

  procedure r_Items_Load;
    var i: Integer;
  begin
    //       i                            name           w    h    d  n  backanim
    LoadItem(ITEM_NONE,                  'NOTEXTURE',    16,  16,  0, 1, False);
    LoadItem(ITEM_MEDKIT_SMALL,          'MED1',         16,  16,  0, 1, False);
    LoadItem(ITEM_MEDKIT_LARGE,          'MED2',         32,  32,  0, 1, False);
    LoadItem(ITEM_MEDKIT_BLACK,          'BMED',         32,  32,  0, 1, False);
    LoadItem(ITEM_ARMOR_GREEN,           'ARMORGREEN',   32,  16, 20, 3, True);
    LoadItem(ITEM_ARMOR_BLUE,            'ARMORBLUE',    32,  16, 20, 3, True);
    LoadItem(ITEM_SPHERE_BLUE,           'SBLUE',        32,  32, 15, 4, True);
    LoadItem(ITEM_SPHERE_WHITE,          'SWHITE',       32,  32, 20, 4, True);
    LoadItem(ITEM_SUIT,                  'SUIT',         32,  64,  0, 1, False);
    LoadItem(ITEM_OXYGEN,                'OXYGEN',       16,  32,  0, 1, False);
    LoadItem(ITEM_INVUL,                 'INVUL',        32,  32, 20, 4, True);
    LoadItem(ITEM_WEAPON_SAW,            'SAW',          64,  32,  0, 1, False);
    LoadItem(ITEM_WEAPON_SHOTGUN1,       'SHOTGUN1',     64,  16,  0, 1, False);
    LoadItem(ITEM_WEAPON_SHOTGUN2,       'SHOTGUN2',     64,  16,  0, 1, False);
    LoadItem(ITEM_WEAPON_CHAINGUN,       'MGUN',         64,  16,  0, 1, False);
    LoadItem(ITEM_WEAPON_ROCKETLAUNCHER, 'RLAUNCHER',    64,  16,  0, 1, False);
    LoadItem(ITEM_WEAPON_PLASMA,         'PGUN',         64,  16,  0, 1, False);
    LoadItem(ITEM_WEAPON_BFG,            'BFG',          64,  64,  0, 1, False);
    LoadItem(ITEM_WEAPON_SUPERPULEMET,   'SPULEMET',     64,  16,  0, 1, False);
    LoadItem(ITEM_AMMO_BULLETS,          'CLIP',         16,  16,  0, 1, False);
    LoadItem(ITEM_AMMO_BULLETS_BOX,      'AMMO',         32,  16,  0, 1, False);
    LoadItem(ITEM_AMMO_SHELLS,           'SHELL1',       16,   8,  0, 1, False);
    LoadItem(ITEM_AMMO_SHELLS_BOX,       'SHELL2',       32,  16,  0, 1, False);
    LoadItem(ITEM_AMMO_ROCKET,           'ROCKET',       16,  32,  0, 1, False);
    LoadItem(ITEM_AMMO_ROCKET_BOX,       'ROCKETS',      64,  32,  0, 1, False);
    LoadItem(ITEM_AMMO_CELL,             'CELL',         16,  16,  0, 1, False);
    LoadItem(ITEM_AMMO_CELL_BIG,         'CELL2',        32,  32,  0, 1, False);
    LoadItem(ITEM_AMMO_BACKPACK,         'BPACK',        32,  32,  0, 1, False);
    LoadItem(ITEM_KEY_RED,               'KEYR',         16,  16,  0, 1, False);
    LoadItem(ITEM_KEY_GREEN,             'KEYG',         16,  16,  0, 1, False);
    LoadItem(ITEM_KEY_BLUE,              'KEYB',         16,  16,  0, 1, False);
    LoadItem(ITEM_WEAPON_KASTET,         'KASTET',       64,  32,  0, 1, False);
    LoadItem(ITEM_WEAPON_PISTOL,         'PISTOL',       64,  16,  0, 1, False);
    LoadItem(ITEM_BOTTLE,                'BOTTLE',       64,  32, 20, 4, True);
    LoadItem(ITEM_HELMET,                'HELMET',       64,  16, 20, 4, True);
    LoadItem(ITEM_JETPACK,               'JETPACK',      96,  32, 15, 3, True);
    LoadItem(ITEM_INVIS,                 'INVIS',        128, 32, 20, 4, True);
    LoadItem(ITEM_WEAPON_FLAMETHROWER,   'FLAMETHROWER', 64,  32,  0, 1, False);
    LoadItem(ITEM_AMMO_FUELCAN,          'FUELCAN',      16,  32,  0, 1, False);

    // fill with NOTEXURE forgotten item
    for i := ITEM_AMMO_FUELCAN + 1 to ITEM_MAX do
      LoadItem(i,'NOTEXTURE',    16,  16,  0, 1, False);

    // hud
    g_Frames_CreateWAD(nil, 'FRAMES_ITEM_BLUESPHERE', GameWAD+':TEXTURES\SBLUE', 32, 32, 4, True);
    g_Frames_CreateWAD(nil, 'FRAMES_ITEM_WHITESPHERE', GameWAD+':TEXTURES\SWHITE', 32, 32, 4, True);
    g_Frames_CreateWAD(nil, 'FRAMES_ITEM_ARMORGREEN', GameWAD+':TEXTURES\ARMORGREEN', 32, 16, 3, True);
    g_Frames_CreateWAD(nil, 'FRAMES_ITEM_ARMORBLUE', GameWAD+':TEXTURES\ARMORBLUE', 32, 16, 3, True);
    g_Frames_CreateWAD(nil, 'FRAMES_ITEM_JETPACK', GameWAD+':TEXTURES\JETPACK', 32, 32, 3, True);
    g_Frames_CreateWAD(nil, 'FRAMES_ITEM_INVUL', GameWAD+':TEXTURES\INVUL', 32, 32, 4, True);
    g_Frames_CreateWAD(nil, 'FRAMES_ITEM_INVIS', GameWAD+':TEXTURES\INVIS', 32, 32, 4, True);
    g_Frames_CreateWAD(nil, 'FRAMES_ITEM_RESPAWN', GameWAD+':TEXTURES\ITEMRESPAWN', 32, 32, 5, True);
    g_Frames_CreateWAD(nil, 'FRAMES_ITEM_BOTTLE', GameWAD+':TEXTURES\BOTTLE', 16, 32, 4, True);
    g_Frames_CreateWAD(nil, 'FRAMES_ITEM_HELMET', GameWAD+':TEXTURES\HELMET', 16, 16, 4, True);
    g_Frames_CreateWAD(nil, 'FRAMES_FLAG_RED', GameWAD+':TEXTURES\FLAGRED', 64, 64, 5, False);
    g_Frames_CreateWAD(nil, 'FRAMES_FLAG_BLUE', GameWAD+':TEXTURES\FLAGBLUE', 64, 64, 5, False);
    g_Frames_CreateWAD(nil, 'FRAMES_FLAG_DOM', GameWAD+':TEXTURES\FLAGDOM', 64, 64, 5, False);
    g_Texture_CreateWADEx('ITEM_MEDKIT_SMALL', GameWAD+':TEXTURES\MED1');
    g_Texture_CreateWADEx('ITEM_MEDKIT_LARGE', GameWAD+':TEXTURES\MED2');
    g_Texture_CreateWADEx('ITEM_WEAPON_SAW', GameWAD+':TEXTURES\SAW');
    g_Texture_CreateWADEx('ITEM_WEAPON_PISTOL', GameWAD+':TEXTURES\PISTOL');
    g_Texture_CreateWADEx('ITEM_WEAPON_KASTET', GameWAD+':TEXTURES\KASTET');
    g_Texture_CreateWADEx('ITEM_WEAPON_SHOTGUN1', GameWAD+':TEXTURES\SHOTGUN1');
    g_Texture_CreateWADEx('ITEM_WEAPON_SHOTGUN2', GameWAD+':TEXTURES\SHOTGUN2');
    g_Texture_CreateWADEx('ITEM_WEAPON_CHAINGUN', GameWAD+':TEXTURES\MGUN');
    g_Texture_CreateWADEx('ITEM_WEAPON_ROCKETLAUNCHER', GameWAD+':TEXTURES\RLAUNCHER');
    g_Texture_CreateWADEx('ITEM_WEAPON_PLASMA', GameWAD+':TEXTURES\PGUN');
    g_Texture_CreateWADEx('ITEM_WEAPON_BFG', GameWAD+':TEXTURES\BFG');
    g_Texture_CreateWADEx('ITEM_WEAPON_SUPERPULEMET', GameWAD+':TEXTURES\SPULEMET');
    g_Texture_CreateWADEx('ITEM_WEAPON_FLAMETHROWER', GameWAD+':TEXTURES\FLAMETHROWER');
    g_Texture_CreateWADEx('ITEM_AMMO_BULLETS', GameWAD+':TEXTURES\CLIP');
    g_Texture_CreateWADEx('ITEM_AMMO_BULLETS_BOX', GameWAD+':TEXTURES\AMMO');
    g_Texture_CreateWADEx('ITEM_AMMO_SHELLS', GameWAD+':TEXTURES\SHELL1');
    g_Texture_CreateWADEx('ITEM_AMMO_SHELLS_BOX', GameWAD+':TEXTURES\SHELL2');
    g_Texture_CreateWADEx('ITEM_AMMO_ROCKET', GameWAD+':TEXTURES\ROCKET');
    g_Texture_CreateWADEx('ITEM_AMMO_ROCKET_BOX', GameWAD+':TEXTURES\ROCKETS');
    g_Texture_CreateWADEx('ITEM_AMMO_CELL', GameWAD+':TEXTURES\CELL');
    g_Texture_CreateWADEx('ITEM_AMMO_CELL_BIG', GameWAD+':TEXTURES\CELL2');
    g_Texture_CreateWADEx('ITEM_AMMO_FUELCAN', GameWAD+':TEXTURES\FUELCAN');
    g_Texture_CreateWADEx('ITEM_AMMO_BACKPACK', GameWAD+':TEXTURES\BPACK');
    g_Texture_CreateWADEx('ITEM_KEY_RED', GameWAD+':TEXTURES\KEYR');
    g_Texture_CreateWADEx('ITEM_KEY_GREEN', GameWAD+':TEXTURES\KEYG');
    g_Texture_CreateWADEx('ITEM_KEY_BLUE', GameWAD+':TEXTURES\KEYB');
    g_Texture_CreateWADEx('ITEM_OXYGEN', GameWAD+':TEXTURES\OXYGEN');
    g_Texture_CreateWADEx('ITEM_SUIT', GameWAD+':TEXTURES\SUIT');
    g_Texture_CreateWADEx('ITEM_WEAPON_KASTET', GameWAD+':TEXTURES\KASTET');
    g_Texture_CreateWADEx('ITEM_MEDKIT_BLACK', GameWAD+':TEXTURES\BMED');
  end;

  procedure r_Items_Free;
    var i: Integer;
  begin
    for i := 0 to ITEM_MAX do
      g_Frames_DeleteByID(items[i].id);
  end;

procedure itemsDrawInternal (dropflag: Boolean);
var
  i, fX, fY: Integer;
  it: PItem;
begin
  if (ggItems = nil) then exit;

  for i := 0 to High(ggItems) do
  begin
    it := @ggItems[i];
    if (not it.used) or (it.ItemType = ITEM_NONE) then continue; // just in case
    if not it.alive then continue;
    if (it.dropped <> dropflag) then continue;

    with it^ do
    begin
      if g_Collide(Obj.X, Obj.Y, Obj.Rect.Width, Obj.Rect.Height, sX, sY, sWidth, sHeight) then
      begin
        Obj.lerp(gLerpFactor, fX, fY);

        r_AnimState_Draw(items[it.ItemType].id, items[it.ItemType].anim, fX, fY, 0, TMirrorType.None, False);

        if g_debug_Frames then
        begin
          e_DrawQuad(Obj.X+Obj.Rect.X,
                     Obj.Y+Obj.Rect.Y,
                     Obj.X+Obj.Rect.X+Obj.Rect.Width-1,
                     Obj.Y+Obj.Rect.Y+Obj.Rect.Height-1,
                     0, 255, 0);
        end;
      end;
    end;
  end;
end;

procedure r_Items_Draw;
begin
  itemsDrawInternal(false);
end;

procedure r_Items_DrawDrop;
begin
  itemsDrawInternal(true);
end;

  procedure r_Items_Update;
    var i: Integer;
  begin
    for i := 0 to ITEM_MAX do
      items[i].anim.Update;
  end;

end.
