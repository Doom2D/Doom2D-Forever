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

  var
    gItemsTexturesID: Array [1..ITEM_MAX] of DWORD;

implementation

  uses
    SysUtils, Classes, Math,
    r_graphics, r_animations, r_textures,
    g_base, g_basic, g_game, g_options,
    g_items
  ;

  var
    itemFrames: Array [0..ITEM_MAX] of DWORD;

  procedure r_Items_Load;
  begin
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
    //
    g_Texture_Get('ITEM_MEDKIT_SMALL',     gItemsTexturesID[ITEM_MEDKIT_SMALL]);
    g_Texture_Get('ITEM_MEDKIT_LARGE',     gItemsTexturesID[ITEM_MEDKIT_LARGE]);
    g_Texture_Get('ITEM_MEDKIT_BLACK',     gItemsTexturesID[ITEM_MEDKIT_BLACK]);
    g_Texture_Get('ITEM_SUIT',             gItemsTexturesID[ITEM_SUIT]);
    g_Texture_Get('ITEM_OXYGEN',           gItemsTexturesID[ITEM_OXYGEN]);
    g_Texture_Get('ITEM_WEAPON_SAW',       gItemsTexturesID[ITEM_WEAPON_SAW]);
    g_Texture_Get('ITEM_WEAPON_SHOTGUN1',  gItemsTexturesID[ITEM_WEAPON_SHOTGUN1]);
    g_Texture_Get('ITEM_WEAPON_SHOTGUN2',  gItemsTexturesID[ITEM_WEAPON_SHOTGUN2]);
    g_Texture_Get('ITEM_WEAPON_CHAINGUN',  gItemsTexturesID[ITEM_WEAPON_CHAINGUN]);
    g_Texture_Get('ITEM_WEAPON_ROCKETLAUNCHER', gItemsTexturesID[ITEM_WEAPON_ROCKETLAUNCHER]);
    g_Texture_Get('ITEM_WEAPON_PLASMA',    gItemsTexturesID[ITEM_WEAPON_PLASMA]);
    g_Texture_Get('ITEM_WEAPON_BFG',       gItemsTexturesID[ITEM_WEAPON_BFG]);
    g_Texture_Get('ITEM_WEAPON_SUPERPULEMET', gItemsTexturesID[ITEM_WEAPON_SUPERPULEMET]);
    g_Texture_Get('ITEM_WEAPON_FLAMETHROWER', gItemsTexturesID[ITEM_WEAPON_FLAMETHROWER]);
    g_Texture_Get('ITEM_AMMO_BULLETS',     gItemsTexturesID[ITEM_AMMO_BULLETS]);
    g_Texture_Get('ITEM_AMMO_BULLETS_BOX', gItemsTexturesID[ITEM_AMMO_BULLETS_BOX]);
    g_Texture_Get('ITEM_AMMO_SHELLS',      gItemsTexturesID[ITEM_AMMO_SHELLS]);
    g_Texture_Get('ITEM_AMMO_SHELLS_BOX',  gItemsTexturesID[ITEM_AMMO_SHELLS_BOX]);
    g_Texture_Get('ITEM_AMMO_ROCKET',      gItemsTexturesID[ITEM_AMMO_ROCKET]);
    g_Texture_Get('ITEM_AMMO_ROCKET_BOX',  gItemsTexturesID[ITEM_AMMO_ROCKET_BOX]);
    g_Texture_Get('ITEM_AMMO_CELL',        gItemsTexturesID[ITEM_AMMO_CELL]);
    g_Texture_Get('ITEM_AMMO_CELL_BIG',    gItemsTexturesID[ITEM_AMMO_CELL_BIG]);
    g_Texture_Get('ITEM_AMMO_FUELCAN',     gItemsTexturesID[ITEM_AMMO_FUELCAN]);
    g_Texture_Get('ITEM_AMMO_BACKPACK',    gItemsTexturesID[ITEM_AMMO_BACKPACK]);
    g_Texture_Get('ITEM_KEY_RED',          gItemsTexturesID[ITEM_KEY_RED]);
    g_Texture_Get('ITEM_KEY_GREEN',        gItemsTexturesID[ITEM_KEY_GREEN]);
    g_Texture_Get('ITEM_KEY_BLUE',         gItemsTexturesID[ITEM_KEY_BLUE]);
    g_Texture_Get('ITEM_WEAPON_KASTET',    gItemsTexturesID[ITEM_WEAPON_KASTET]);
    g_Texture_Get('ITEM_WEAPON_PISTOL',    gItemsTexturesID[ITEM_WEAPON_PISTOL]);
    // Frames
    g_Frames_Get(itemFrames[ITEM_ARMOR_GREEN], 'FRAMES_ITEM_ARMORGREEN');
    g_Frames_Get(itemFrames[ITEM_ARMOR_BLUE], 'FRAMES_ITEM_ARMORBLUE');
    g_Frames_Get(itemFrames[ITEM_JETPACK], 'FRAMES_ITEM_JETPACK');
    g_Frames_Get(itemFrames[ITEM_SPHERE_BLUE], 'FRAMES_ITEM_BLUESPHERE');
    g_Frames_Get(itemFrames[ITEM_SPHERE_WHITE], 'FRAMES_ITEM_WHITESPHERE');
    g_Frames_Get(itemFrames[ITEM_INVUL], 'FRAMES_ITEM_INVUL');
    g_Frames_Get(itemFrames[ITEM_INVIS], 'FRAMES_ITEM_INVIS');
    g_Frames_Get(itemFrames[ITEM_BOTTLE], 'FRAMES_ITEM_BOTTLE');
    g_Frames_Get(itemFrames[ITEM_HELMET], 'FRAMES_ITEM_HELMET');
  end;

  procedure r_Items_Free;
  begin
    g_Frames_DeleteByName('FRAMES_ITEM_BLUESPHERE');
    g_Frames_DeleteByName('FRAMES_ITEM_WHITESPHERE');
    g_Frames_DeleteByName('FRAMES_ITEM_ARMORGREEN');
    g_Frames_DeleteByName('FRAMES_ITEM_ARMORBLUE');
    g_Frames_DeleteByName('FRAMES_ITEM_JETPACK');
    g_Frames_DeleteByName('FRAMES_ITEM_INVUL');
    g_Frames_DeleteByName('FRAMES_ITEM_INVIS');
    g_Frames_DeleteByName('FRAMES_ITEM_RESPAWN');
    g_Frames_DeleteByName('FRAMES_ITEM_BOTTLE');
    g_Frames_DeleteByName('FRAMES_ITEM_HELMET');
    g_Frames_DeleteByName('FRAMES_FLAG_RED');
    g_Frames_DeleteByName('FRAMES_FLAG_BLUE');
    g_Frames_DeleteByName('FRAMES_FLAG_DOM');
    g_Texture_Delete('ITEM_MEDKIT_SMALL');
    g_Texture_Delete('ITEM_MEDKIT_LARGE');
    g_Texture_Delete('ITEM_WEAPON_SAW');
    g_Texture_Delete('ITEM_WEAPON_PISTOL');
    g_Texture_Delete('ITEM_WEAPON_KASTET');
    g_Texture_Delete('ITEM_WEAPON_SHOTGUN1');
    g_Texture_Delete('ITEM_WEAPON_SHOTGUN2');
    g_Texture_Delete('ITEM_WEAPON_CHAINGUN');
    g_Texture_Delete('ITEM_WEAPON_ROCKETLAUNCHER');
    g_Texture_Delete('ITEM_WEAPON_PLASMA');
    g_Texture_Delete('ITEM_WEAPON_BFG');
    g_Texture_Delete('ITEM_WEAPON_SUPERPULEMET');
    g_Texture_Delete('ITEM_WEAPON_FLAMETHROWER');
    g_Texture_Delete('ITEM_AMMO_BULLETS');
    g_Texture_Delete('ITEM_AMMO_BULLETS_BOX');
    g_Texture_Delete('ITEM_AMMO_SHELLS');
    g_Texture_Delete('ITEM_AMMO_SHELLS_BOX');
    g_Texture_Delete('ITEM_AMMO_ROCKET');
    g_Texture_Delete('ITEM_AMMO_ROCKET_BOX');
    g_Texture_Delete('ITEM_AMMO_CELL');
    g_Texture_Delete('ITEM_AMMO_CELL_BIG');
    g_Texture_Delete('ITEM_AMMO_FUELCAN');
    g_Texture_Delete('ITEM_AMMO_BACKPACK');
    g_Texture_Delete('ITEM_KEY_RED');
    g_Texture_Delete('ITEM_KEY_GREEN');
    g_Texture_Delete('ITEM_KEY_BLUE');
    g_Texture_Delete('ITEM_OXYGEN');
    g_Texture_Delete('ITEM_SUIT');
    g_Texture_Delete('ITEM_WEAPON_KASTET');
    g_Texture_Delete('ITEM_MEDKIT_BLACK');
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
        if (Animation = nil) then
        begin
          e_Draw(gItemsTexturesID[ItemType], fX, fY, 0, true, false);
        end
        else if itemFrames[it.ItemType] <> 0 then
        begin
          r_AnimationState_Draw(itemFrames[it.ItemType], Animation, fX, fY, TMirrorType.None)
        end;

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

end.
