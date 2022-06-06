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
unit r_map;

interface

  procedure r_Map_Initialize;
  procedure r_Map_Finalize;

  procedure r_Map_Load;
  procedure r_Map_Free;

  procedure r_Map_LoadTextures;
  procedure r_Map_FreeTextures;

  procedure r_Map_Update;

  procedure r_Map_Draw (x, y, w, h, camx, camy: Integer);

implementation

  uses
    {$IFDEF USE_GLES1}
      GLES11,
    {$ELSE}
      GL, GLEXT,
    {$ENDIF}
    e_log,
    binheap, MAPDEF, utils,
    g_options, g_textures, g_basic, g_base, g_phys,
    g_game, g_map, g_panel, g_items, g_monsters, g_playermodel, g_player, g_weapons,
    {$IFDEF ENABLE_CORPSES}
      g_corpses,
    {$ENDIF}
    r_textures, r_draw
  ;

  const
    MTABLE: array [0..MONSTER_MAN] of record
      w, h: Integer;
    end = (
      (w: 64;  h: 64),  // NONE
      (w: 64;  h: 64),  // DEMON
      (w: 64;  h: 64),  // IMP
      (w: 64;  h: 64),  // ZOMBY
      (w: 64;  h: 64),  // SERG
      (w: 128; h: 128), // CYBER
      (w: 64;  h: 64),  // CGUN
      (w: 128; h: 128), // BARON
      (w: 128; h: 128), // KNIGHT
      (w: 128; h: 128), // CACO
      (w: 64;  h: 64),  // SOUL (DIE is 128x128, see load code)
      (w: 128; h: 128), // PAIN
      (w: 256; h: 128), // SPIDER
      (w: 128; h: 64),  // BSP
      (w: 128; h: 128), // MANCUB
      (w: 128; h: 128), // SKEL
      (w: 128; h: 128), // VILE
      (w: 32;  h: 32),  // FISH
      (w: 64;  h: 64),  // BARREL
      (w: 128; h: 128), // ROBO
      (w: 64;  h: 64)   // MAN
    );
    VILEFIRE_DX = 32;
    VILEFIRE_DY = 128;

  type
    TBinHeapPanelDrawCmp = class
      public
        class function less (const a, b: TPanel): Boolean; inline;
    end;

    TBinHeapPanelDraw = specialize TBinaryHeapBase<TPanel, TBinHeapPanelDrawCmp>;

    TMonsterAnims = array [0..ANIM_LAST, TDirection] of TGLMultiTexture;

  var
    SkyTexture: TGLTexture;
    RenTextures: array of record
      spec: LongInt;
      tex: TGLMultiTexture;
    end;
    Items: array [0..ITEM_MAX] of record
      tex: TGLMultiTexture;
      anim: TAnimState;
    end;
    MonTextures: array [0..MONSTER_MAN] of TMonsterAnims;
    WeapTextures: array [0..WP_LAST, 0..W_POS_LAST, 0..W_ACT_LAST] of TGLTexture;
    VileFire: TGLMultiTexture;
    Models: array of record
      anim: array [TDirection, 0..A_LAST] of record
        base, mask: TGLMultiTexture;
      end;
(*
      {$IFDEF ENABLE_GIBS}
        gibs: array of record
          base, mask: TGLTexture;
          rect: TRectWH;
        end;
      {$ENDIF}
*)
    end;

    plist: TBinHeapPanelDraw = nil;

  class function TBinHeapPanelDrawCmp.less (const a, b: TPanel): Boolean; inline;
  begin
    if a.tag < b.tag then begin result := true; exit; end;
    if a.tag > b.tag then begin result := false; exit; end;
    result := a.arrIdx < b.arrIdx;
  end;

  procedure r_Map_Initialize;
  begin
    plist := TBinHeapPanelDraw.Create();
  end;

  procedure r_Map_Finalize;
  begin
    plist.Free
  end;

  procedure r_Map_LoadModel (i: Integer);
    var prefix: AnsiString; a: Integer; d: TDirection; m: ^TPlayerModelInfo;
  begin
    m := @PlayerModelsArray[i];
    prefix := m.FileName + ':TEXTURES/';
    for d := TDirection.D_LEFT to TDirection.D_RIGHT do
    begin
      for a := A_STAND to A_LAST do
      begin
        Models[i].anim[d, a].base := nil;
        Models[i].anim[d, a].mask := nil;
        if m.anim[d, a].resource <> '' then
          Models[i].anim[d, a].base := r_Textures_LoadMultiFromFileAndInfo(prefix + m.anim[d, a].resource, 64, 64, m.anim[d, a].frames, m.anim[d, a].back, true);
        if m.anim[d, a].mask <> '' then
          Models[i].anim[d, a].mask := r_Textures_LoadMultiFromFileAndInfo(prefix + m.anim[d, a].mask, 64, 64, m.anim[d, a].frames, m.anim[d, a].back, true);
      end
    end;
(*
    {$IFDEF ENABLE_GIBS}
      Models[i].gibs := nil;
      if m.GibsCount > 0 then
      begin
        SetLength(Models[i].gibs, m.GibsCount);
      end;
    {$ENDIF}
*)
  end;


  procedure r_Map_Load;
    const
      WeapName: array [0..WP_LAST] of AnsiString = ('', 'CSAW', 'HGUN', 'SG', 'SSG', 'MGUN', 'RKT', 'PLZ', 'BFG', 'SPL', 'FLM');
      WeapPos: array [0..W_POS_LAST] of AnsiString = ('', '_UP', '_DN');
      WeapAct: array [0..W_ACT_LAST] of AnsiString = ('', '_FIRE');
    var
      i, j, k: Integer; d: TDirection;

    procedure LoadItem (i: Integer; const name: AnsiString; w, h, delay, count: Integer; backanim: Boolean);
    begin
      ASSERT(i >= 0);
      ASSERT(i <= ITEM_MAX);
      Items[i].tex := r_Textures_LoadMultiFromFileAndInfo(GameWAD + ':TEXTURES/' + name, w, h, count, backanim, false);
      if backanim then count := count * 2 - 2;
      Items[i].anim := TAnimState.Create(True, delay, count);
      ASSERT(Items[i].tex <> NIL);
    end;

    procedure LoadMonster (m, a: Integer; d: TDirection);
      const
        dir: array [TDirection] of AnsiString = ('_L', '');
      var
        w, h, count: Integer;
    begin
      count := MONSTER_ANIMTABLE[m].AnimCount[a];
      if count > 0 then
      begin
        w := MTABLE[m].w;
        h := MTABLE[m].h;
        if (m = MONSTER_SOUL) and (a = ANIM_DIE) then
        begin
          // special case
          w := 128;
          h := 128;
        end;
        MonTextures[m, a, d] := r_Textures_LoadMultiFromFileAndInfo(
          GameWAD + ':MTEXTURES/' + MONSTERTABLE[m].name + '_' + ANIMTABLE[a].name + dir[d],
          w,
          h,
          count,
          False,
          False
        )
      end
      else
        MonTextures[m, a, d] := nil
    end;

  begin
    // --------- items --------- //
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
    LoadItem(ITEM_BOTTLE,                'BOTTLE',       16,  32, 20, 4, True);
    LoadItem(ITEM_HELMET,                'HELMET',       16,  16, 20, 4, True);
    LoadItem(ITEM_JETPACK,               'JETPACK',      32,  32, 15, 3, True);
    LoadItem(ITEM_INVIS,                 'INVIS',        32,  32, 20, 4, True);
    LoadItem(ITEM_WEAPON_FLAMETHROWER,   'FLAMETHROWER', 64,  32,  0, 1, False);
    LoadItem(ITEM_AMMO_FUELCAN,          'FUELCAN',      16,  32,  0, 1, False);
    // fill with NOTEXURE forgotten item
    for i := ITEM_AMMO_FUELCAN + 1 to ITEM_MAX do
      LoadItem(i,'NOTEXTURE', 16, 16, 0, 1, False);
    // --------- monsters --------- //
    for i := MONSTER_DEMON to MONSTER_MAN do
      for j := 0 to ANIM_LAST do
        for d := TDirection.D_LEFT to TDirection.D_RIGHT do
          LoadMonster(i, j, d);
    VileFire := r_Textures_LoadMultiFromFileAndInfo(GameWAD + ':TEXTURES/FIRE', 64, 128, 8, False, False);
    // --------- player models --------- //
    if PlayerModelsArray <> nil then
    begin
      SetLength(Models, Length(PlayerModelsArray));
      for i := 0 to High(PlayerModelsArray) do
        r_Map_LoadModel(i);
    end;
    // --------- player weapons --------- //
    for i := 1 to WP_LAST do
      for j := 0 to W_POS_LAST do
        for k := 0 to W_ACT_LAST do
          WeapTextures[i, j, k] := r_Textures_LoadFromFile(GameWAD + ':WEAPONS\' + WeapName[i] + WeapPos[j] + WeapAct[k]);
  end;

  procedure r_Map_Free;
    var i, j: Integer; d: TDirection;
  begin
    for i := MONSTER_DEMON to MONSTER_MAN do
    begin
      for j := 0 to ANIM_LAST do
      begin
        for d := TDirection.D_LEFT to TDirection.D_RIGHT do
        begin
          if MonTextures[i, j, d] <> nil then
            MonTextures[i, j, d].Free;
          MonTextures[i, j, d] := nil;
        end;
      end;
    end;
    for i := 0 to ITEM_MAX do
    begin
      if Items[i].tex <> nil then
      begin
        Items[i].tex.Free;
        Items[i].tex := nil;
      end;
      Items[i].anim.Invalidate;
    end;
  end;

  procedure r_Map_LoadTextures;
    var i, n: Integer;
  begin
    if Textures <> nil then
    begin
      n := Length(Textures);
      SetLength(RenTextures, n);
      for i := 0 to n - 1 do
      begin
        RenTextures[i].tex := nil;
        case Textures[i].TextureName of
          TEXTURE_NAME_WATER: RenTextures[i].spec := TEXTURE_SPECIAL_WATER;
          TEXTURE_NAME_ACID1: RenTextures[i].spec := TEXTURE_SPECIAL_ACID1;
          TEXTURE_NAME_ACID2: RenTextures[i].spec := TEXTURE_SPECIAL_ACID2;
          else
            RenTextures[i].spec := 0;
            RenTextures[i].tex := r_Textures_LoadMultiFromFile(Textures[i].FullName);
            if RenTextures[i].tex = nil then
              e_LogWritefln('r_Map_LoadTextures: failed to load texture: %s', [Textures[i].FullName]);
        end;
      end;
    end;
    if gMapInfo.SkyFullName <> '' then
      SkyTexture := r_Textures_LoadFromFile(gMapInfo.SkyFullName);
    plist.Clear;
  end;

  procedure r_Map_FreeTextures;
    var i: Integer;
  begin
    plist.Clear;
    if SkyTexture <> nil then
      SkyTexture.Free;
    SkyTexture := nil;
    if RenTextures <> nil then
      for i := 0 to High(RenTextures) do
        if RenTextures[i].tex <> nil then
          RenTextures[i].tex.Free;
    RenTextures := nil;
  end;

  procedure r_Map_Update;
    var i: Integer;
  begin
    for i := 0 to ITEM_MAX do
      Items[i].anim.Update;
  end;

  procedure r_Map_DrawPanel (p: TPanel);
    var Texture: Integer; t: TGLMultiTexture;
  begin
    ASSERT(p <> nil);
    if p.FCurTexture >= 0 then
    begin
      Texture := p.TextureIDs[p.FCurTexture].Texture;
      t := RenTextures[Texture].tex;

      if (RenTextures[Texture].spec = 0) or (t <> nil) then
      begin
        // TODO set alpha and blending type
        if t = nil then
          r_Draw_TextureRepeat(nil, p.x, p.y, p.width, p.height, false)
        else if p.TextureIDs[p.FCurTexture].AnTex.IsValid() then
          r_Draw_MultiTextureRepeat(t, p.TextureIDs[p.FCurTexture].AnTex, p.x, p.y, p.width, p.height, false)
        else
          r_Draw_TextureRepeat(t.GetTexture(0), p.x, p.y, p.width, p.height, false)
      end;

      if t = nil then
      begin
        case RenTextures[Texture].spec of
          TEXTURE_SPECIAL_WATER: r_Draw_Filter(p.x, p.y, p.x + p.width, p.y + p.height, 0, 0, 255, 255);
          TEXTURE_SPECIAL_ACID1: r_Draw_Filter(p.x, p.y, p.x + p.width, p.y + p.height, 0, 230, 0, 255);
          TEXTURE_SPECIAL_ACID2: r_Draw_Filter(p.x, p.y, p.x + p.width, p.y + p.height, 230, 0, 0, 255);
        end
      end
    end
  end;

  procedure r_Map_DrawPanelType (panelTyp: DWORD);
    var tagMask, i: Integer; p: TPanel;
  begin
    i := 0;
    tagMask := PanelTypeToTag(panelTyp);
    while plist.count > 0 do
    begin
      p := TPanel(plist.Front());
      if (p.tag and tagMask) = 0 then
        break;
      r_Map_DrawPanel(p);
      Inc(i);
      plist.PopFront
    end;
  end;

  procedure r_Map_DrawItems (x, y, w, h: Integer; drop: Boolean);
    var i, fX, fY: Integer; it: PItem; t: TGLMultiTexture;
  begin
    if ggItems <> nil then
    begin
      for i := 0 to High(ggItems) do
      begin
        it := @ggItems[i];
        if it.used and it.alive and (it.dropped = drop) and (it.ItemType <> ITEM_NONE) then
        begin
          t := Items[it.ItemType].tex;
          if g_Collide(it.obj.x, it.obj.y, t.width, t.height, x, y, w, h) then
          begin
            it.obj.Lerp(gLerpFactor, fX, fY);
            r_Draw_MultiTextureRepeat(t, Items[it.ItemType].anim, fX, fY, t.width, t.height, false);
            // if g_debug_frames then // TODO draw collision frame
          end;
        end;
      end;
    end;
  end;

  function r_Map_GetMonsterTexture (m, a: Integer; d: TDirection; out t: TGLMultiTexture; out dx, dy: Integer; out flip: Boolean): Boolean;
    // var c: Integer;
  begin
    t := nil; dx := 0; dy := 0; flip := false;
    result := MonTextures[m, a, d] <> nil;
    if result = false then
    begin
      flip := true;
      if d = TDirection.D_RIGHT then d := TDirection.D_LEFT else d := TDirection.D_RIGHT;
      result := MonTextures[m, a, d] <> nil;
    end;
    if result = true then
    begin
      t := MonTextures[m, a, d];
      if d = TDirection.D_LEFT then
      begin
        dx := MONSTER_ANIMTABLE[m].AnimDeltaLeft[a].X;
        dy := MONSTER_ANIMTABLE[m].AnimDeltaLeft[a].Y;
      end
      else
      begin
        dx := MONSTER_ANIMTABLE[m].AnimDeltaRight[a].X;
        dy := MONSTER_ANIMTABLE[m].AnimDeltaRight[a].Y;
      end;
      if flip then
      begin
//        c := (MONSTERTABLE[MonsterType].Rect.X - dx) + MONSTERTABLE[MonsterType].Rect.Width;
//        dx := MTABLE[m].width - c - MONSTERTABLE[MonsterType].Rect.X;
        dx := -dx;
      end;
    end;
  end;

  procedure r_Map_DrawMonsterAttack (constref mon: TMonster);
    var o: TObj;
  begin
    if VileFire <> nil then
      if (mon.MonsterType = MONSTER_VILE) and (mon.MonsterState = MONSTATE_SHOOT) then
        if mon.VileFireAnim.IsValid() and GetPos(mon.MonsterTargetUID, @o) then
          r_Draw_MultiTextureRepeat(VileFire, mon.VileFireAnim, o.x + o.rect.x + (o.rect.width div 2) - VILEFIRE_DX, o.y + o.rect.y + o.rect.height - VILEFIRE_DY, VileFire.width, VileFire.height, False);
  end;

  procedure r_Map_DrawMonster (constref mon: TMonster);
    var m, a, fX, fY, dx, dy: Integer; d: TDirection; flip: Boolean; t: TGLMultiTexture;
  begin
    m := mon.MonsterType;
    a := mon.MonsterAnim;
    d := mon.GameDirection;

    mon.obj.Lerp(gLerpFactor, fX, fY);

    if r_Map_GetMonsterTexture(m, a, d, t, dx, dy, flip) then
      r_Draw_MultiTextureRepeat(t, mon.DirAnim[a, d], fX + dx, fY + dy, t.width, t.height, flip);

{
    if g_debug_frames
      // TODO draw frame
}
  end;

  procedure r_Map_DrawMonsters (x, y, w, h: Integer);
    var i: Integer; m: TMonster;
  begin
    if gMonsters <> nil then
    begin
      for i := 0 to High(gMonsters) do
      begin
        m := gMonsters[i];
        if m <> nil then
        begin
          r_Map_DrawMonsterAttack(m);
          // TODO select from grid
          if g_Collide(m.obj.x + m.obj.rect.x, m.obj.y + m.obj.rect.y, m.obj.rect.width, m.obj.rect.height, x, y, w, h) then
            r_Map_DrawMonster(m);
        end;
      end;
    end;
  end;

  function r_Map_GetPlayerModelTex (i: Integer; var a: Integer; var d: TDirection; out flip: Boolean): Boolean;
  begin
    flip := false;
    result := Models[i].anim[d, a].base <> nil;
    if result = false then
    begin
      flip := true;
      if d = TDirection.D_LEFT then d := TDirection.D_RIGHT else d := TDirection.D_LEFT;
      result := Models[i].anim[d, a].base <> nil;
    end;
  end;

  procedure r_Map_DrawPlayerModel (pm: TPlayerModel; x, y: Integer);
    var a, pos, act, xx, yy: Integer; d: TDirection; flip: Boolean; t: TGLMultiTexture; tex: TGLTexture;
  begin
    a := pm.CurrentAnimation;
    d := pm.Direction;
    // TODO draw flag
    if PlayerModelsArray[pm.id].HaveWeapon and not (pm.CurrentAnimation in [A_DIE1, A_DIE2, A_PAIN]) then
    begin
      case a of
        A_SEEUP, A_ATTACKUP: pos := W_POS_UP;
        A_SEEDOWN, A_ATTACKDOWN: pos := W_POS_DOWN;
      else pos := W_POS_NORMAL;
      end;
      if (a in [A_ATTACK, A_ATTACKUP, A_ATTACKDOWN]) or pm.GetFire() then
        act := W_ACT_FIRE
      else
        act := W_ACT_NORMAL;
      tex := WeapTextures[pm.CurrentWeapon, pos, act];
      if tex <> nil then
      begin
        xx := PlayerModelsArray[pm.id].WeaponPoints[pm.CurrentWeapon, a, d, pm.AnimState.CurrentFrame].X;
        yy := PlayerModelsArray[pm.id].WeaponPoints[pm.CurrentWeapon, a, d, pm.AnimState.CurrentFrame].Y;
        r_Draw_Texture(
          tex,
          x + xx,
          y + yy,
          tex.width,
          tex.height,
          d = TDirection.D_LEFT
        );
      end;
    end;
    if r_Map_GetPlayerModelTex(pm.id, a, d, flip) then
    begin
      t := Models[pm.id].anim[d, a].base;
      r_Draw_MultiTextureRepeat(t, pm.AnimState, x, y, t.width, t.height, flip);
      // TODO colorize mask
      t := Models[pm.id].anim[d, a].mask;
      if t <> nil then
        r_Draw_MultiTextureRepeat(t, pm.AnimState, x, y, t.width, t.height, flip);
    end;
  end;

  procedure r_Map_DrawPlayer (p: TPlayer);
    var fX, fY, fSlope: Integer;
  begin
    if p.alive then
    begin
      fX := p.obj.x; fY := p.obj.y;
      // TODO fix lerp
      //p.obj.Lerp(gLerpFactor, fX, fY);
      fSlope := nlerp(p.SlopeOld, p.obj.slopeUpLeft, gLerpFactor);
      // TODO draw punch
      // TODO invul pentagram
      // TODO draw it with transparency
      r_Map_DrawPlayerModel(p.Model, fX, fY + fSlope);
    end;
    // TODO draw g_debug_frames
    // TODO draw chat bubble
    // TODO draw aim
  end;

  procedure r_Map_DrawPlayers (x, y, w, h: Integer);
    var i: Integer;
  begin
    // TODO draw only visible
    if gPlayers <> nil then
      for i := 0 to High(gPlayers) do
        if gPlayers[i] <> nil then
          r_Map_DrawPlayer(gPlayers[i]);
  end;

{$IFDEF ENABLE_CORPSES}
  procedure r_Map_DrawCorpses (x, y, w, h: Integer);
    var i, fX, fY: Integer; p: TCorpse;
  begin
    if gCorpses <> nil then
    begin
      for i := 0 to High(gCorpses) do
      begin
        p := gCorpses[i];
        if (p <> nil) and (p.state <> CORPSE_STATE_REMOVEME) and (p.model <> nil) then
        begin
          p.obj.Lerp(gLerpFactor, fX, fY);
          r_Map_DrawPlayerModel(p.model, fX, fY);
        end;
      end;
    end;
  end;
{$ENDIF}

  procedure r_Map_Draw (x, y, w, h, camx, camy: Integer);
    var iter: TPanelGrid.Iter; p: PPanel; cx, cy, xx, yy, ww, hh: Integer;
  begin
    cx := camx - w div 2;
    cy := camy - h div 2;
    xx := x + cx;
    yy := y + cy;
    ww := w;
    hh := h;

    if SkyTexture <> nil then
      r_Draw_Texture(SkyTexture, x, y, w, h, false);

    plist.Clear;
    iter := mapGrid.ForEachInAABB(xx, yy, ww, hh, GridDrawableMask);
    for p in iter do
      if ((p^.tag and GridTagDoor) <> 0) = p^.door then
        plist.Insert(p^);
    iter.Release;

    glPushMatrix;
    glTranslatef(-cx, -cy, 0);
    r_Map_DrawPanelType(PANEL_BACK);
    r_Map_DrawPanelType(PANEL_STEP);
    r_Map_DrawItems(xx, yy, ww, hh, false);
    // TODO draw weapons
    // TODO draw shells
    r_Map_DrawPlayers(xx, yy, ww, hh);
    // TODO draw gibs
    {$IFDEF ENABLE_CORPSES}
      r_Map_DrawCorpses(xx, yy, ww, hh);
    {$ENDIF}
    r_Map_DrawPanelType(PANEL_WALL);
    r_Map_DrawMonsters(xx, yy, ww, hh);
    r_Map_DrawItems(xx, yy, ww, hh, true);
    r_Map_DrawPanelType(PANEL_CLOSEDOOR);
    // TODO draw gfx
    // TODO draw flags
    r_Map_DrawPanelType(PANEL_ACID1);
    r_Map_DrawPanelType(PANEL_ACID2);
    r_Map_DrawPanelType(PANEL_WATER);
    r_Map_DrawPanelType(PANEL_FORE);
    glPopMatrix;
  end;

end.
