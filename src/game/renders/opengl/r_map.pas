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

  uses g_base; // TRectWH

  procedure r_Map_Initialize;
  procedure r_Map_Finalize;

  procedure r_Map_Load;
  procedure r_Map_Free;

  procedure r_Map_LoadTextures;
  procedure r_Map_FreeTextures;

{$IFDEF ENABLE_GFX}
  procedure r_Map_NewGFX (typ, x, y: Integer);
{$ENDIF}
{$IFDEF ENABLE_GIBS}
  function r_Map_GetGibSize (m, i: Integer): TRectWH;
{$ENDIF}

  procedure r_Map_Update;

  procedure r_Map_Draw (x, y, w, h, camx, camy: Integer);

implementation

  uses
    Math,
    {$IFDEF USE_GLES1}
      GLES11,
    {$ELSE}
      GL, GLEXT,
    {$ENDIF}
    e_log,
    binheap, MAPDEF, utils,
    g_options, g_textures, g_basic, g_phys,
    g_game, g_map, g_panel, g_items, g_monsters, g_playermodel, g_player, g_weapons,
    {$IFDEF ENABLE_CORPSES}
      g_corpses,
    {$ENDIF}
    {$IFDEF ENABLE_SHELLS}
      g_shells,
    {$ENDIF}
    {$IFDEF ENABLE_GIBS}
      g_gibs,
    {$ENDIF}
    {$IFDEF ENABLE_GFX}
      g_gfx,
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

    ItemAnim: array [0..ITEM_LAST] of record
      name: AnsiString;
      w, h: Integer;
      d: Integer; // delay
      n: Integer; // count
      b: Boolean; // backanim
    end = (
      (name: '';             w: 0;  h: 0;  d: 0;  n: 0; b: False),
      (name: 'MED1';         w: 16; h: 16; d: 0;  n: 1; b: False),
      (name: 'MED2';         w: 32; h: 32; d: 0;  n: 1; b: False),
      (name: 'BMED';         w: 32; h: 32; d: 0;  n: 1; b: False),
      (name: 'ARMORGREEN';   w: 32; h: 16; d: 20; n: 3; b: True),
      (name: 'ARMORBLUE';    w: 32; h: 16; d: 20; n: 3; b: True),
      (name: 'SBLUE';        w: 32; h: 32; d: 15; n: 4; b: True),
      (name: 'SWHITE';       w: 32; h: 32; d: 20; n: 4; b: True),
      (name: 'SUIT';         w: 32; h: 64; d: 0;  n: 1; b: False),
      (name: 'OXYGEN';       w: 16; h: 32; d: 0;  n: 1; b: False),
      (name: 'INVUL';        w: 32; h: 32; d: 20; n: 4; b: True),
      (name: 'SAW';          w: 64; h: 32; d: 0;  n: 1; b: False),
      (name: 'SHOTGUN1';     w: 64; h: 16; d: 0;  n: 1; b: False),
      (name: 'SHOTGUN2';     w: 64; h: 16; d: 0;  n: 1; b: False),
      (name: 'MGUN';         w: 64; h: 16; d: 0;  n: 1; b: False),
      (name: 'RLAUNCHER';    w: 64; h: 16; d: 0;  n: 1; b: False),
      (name: 'PGUN';         w: 64; h: 16; d: 0;  n: 1; b: False),
      (name: 'BFG';          w: 64; h: 64; d: 0;  n: 1; b: False),
      (name: 'SPULEMET';     w: 64; h: 16; d: 0;  n: 1; b: False),
      (name: 'CLIP';         w: 16; h: 16; d: 0;  n: 1; b: False),
      (name: 'AMMO';         w: 32; h: 16; d: 0;  n: 1; b: False),
      (name: 'SHELL1';       w: 16; h: 8;  d: 0;  n: 1; b: False),
      (name: 'SHELL2';       w: 32; h: 16; d: 0;  n: 1; b: False),
      (name: 'ROCKET';       w: 16; h: 32; d: 0;  n: 1; b: False),
      (name: 'ROCKETS';      w: 64; h: 32; d: 0;  n: 1; b: False),
      (name: 'CELL';         w: 16; h: 16; d: 0;  n: 1; b: False),
      (name: 'CELL2';        w: 32; h: 32; d: 0;  n: 1; b: False),
      (name: 'BPACK';        w: 32; h: 32; d: 0;  n: 1; b: False),
      (name: 'KEYR';         w: 16; h: 16; d: 0;  n: 1; b: False),
      (name: 'KEYG';         w: 16; h: 16; d: 0;  n: 1; b: False),
      (name: 'KEYB';         w: 16; h: 16; d: 0;  n: 1; b: False),
      (name: 'KASTET';       w: 64; h: 32; d: 0;  n: 1; b: False),
      (name: 'PISTOL';       w: 64; h: 16; d: 0;  n: 1; b: False),
      (name: 'BOTTLE';       w: 16; h: 32; d: 20; n: 4; b: True),
      (name: 'HELMET';       w: 16; h: 16; d: 20; n: 4; b: True),
      (name: 'JETPACK';      w: 32; h: 32; d: 15; n: 3; b: True),
      (name: 'INVIS';        w: 32; h: 32; d: 20; n: 4; b: True),
      (name: 'FLAMETHROWER'; w: 64; h: 32; d: 0;  n: 1; b: False),
      (name: 'FUELCAN';      w: 16; h: 32; d: 0;  n: 1; b: False)
    );

{$IFDEF ENABLE_GFX}
    GFXAnim: array [0..R_GFX_LAST] of record
      name: AnsiString;
      w, h: Integer;
      count: Integer;
      back: Boolean;
      speed: Integer;
      rspeed: Integer;
      alpha: Integer;
    end = (
      (name: '';            w: 0;   h: 0;   count: 0;  back: false; speed: 0; rspeed: 0; alpha: 0),
      (name: 'TELEPORT';    w: 64;  h: 64;  count: 10; back: false; speed: 6; rspeed: 0; alpha: 0),
      (name: 'FLAME';       w: 32;  h: 32;  count: 11; back: false; speed: 3; rspeed: 0; alpha: 0),
      (name: 'EROCKET';     w: 128; h: 128; count: 6;  back: false; speed: 6; rspeed: 0; alpha: 0),
      (name: 'EBFG';        w: 128; h: 128; count: 6;  back: false; speed: 6; rspeed: 0; alpha: 0),
      (name: 'BFGHIT';      w: 64;  h: 64;  count: 4;  back: false; speed: 4; rspeed: 0; alpha: 0),
      (name: 'FIRE';        w: 64;  h: 128; count: 8;  back: false; speed: 4; rspeed: 2; alpha: 0),
      (name: 'ITEMRESPAWN'; w: 32;  h: 32;  count: 5;  back: true;  speed: 4; rspeed: 0; alpha: 0),
      (name: 'SMOKE';       w: 32;  h: 32;  count: 10; back: false; speed: 3; rspeed: 0; alpha: 0),
      (name: 'ESKELFIRE';   w: 64;  h: 64;  count: 3;  back: false; speed: 8; rspeed: 0; alpha: 0),
      (name: 'EPLASMA';     w: 32;  h: 32;  count: 4;  back: true;  speed: 3; rspeed: 0; alpha: 0),
      (name: 'EBSPFIRE';    w: 32;  h: 32;  count: 5;  back: false; speed: 3; rspeed: 0; alpha: 0),
      (name: 'EIMPFIRE';    w: 64;  h: 64;  count: 3;  back: false; speed: 6; rspeed: 0; alpha: 0),
      (name: 'ECACOFIRE';   w: 64;  h: 64;  count: 3;  back: false; speed: 6; rspeed: 0; alpha: 0),
      (name: 'EBARONFIRE';  w: 64;  h: 64;  count: 3;  back: false; speed: 6; rspeed: 0; alpha: 0),
      (name: 'TELEPORT';    w: 64;  h: 64;  count: 10; back: false; speed: 3; rspeed: 0; alpha: 0),   // fast
      (name: 'SMOKE';       w: 32;  h: 32;  count: 10; back: false; speed: 3; rspeed: 0; alpha: 150), // transparent
      (name: 'FLAME';       w: 32;  h: 32;  count: 11; back: false; speed: 3; rspeed: 2; alpha: 0)    // random
    );
{$ENDIF}

    ShotAnim: array [0..WEAPON_LAST] of record
      name: AnsiString;
      w, h: Integer;
      count: Integer;
    end = (
      (name: '';            w: 0;  h: 0;  count: 0),  // 0  KASTET
      (name: '';            w: 0;  h: 0;  count: 0),  // 1  SAW
      (name: '';            w: 0;  h: 0;  count: 0),  // 2  PISTOL
      (name: '';            w: 0;  h: 0;  count: 0),  // 3  SHOTGUN1
      (name: '';            w: 0;  h: 0;  count: 0),  // 4  SHOTGUN2
      (name: '';            w: 0;  h: 0;  count: 0),  // 5  CHAINGUN
      (name: 'BROCKET';     w: 64; h: 32; count: 1),  // 6  ROCKETLAUNCHER
      (name: 'BPLASMA';     w: 16; h: 16; count: 2),  // 7  PLASMA
      (name: 'BBFG';        w: 64; h: 64; count: 2),  // 8  BFG
      (name: '';            w: 0;  h: 0;  count: 0),  // 9  SUPERPULEMET
      (name: 'FLAME';       w: 32; h: 32; count: 0{11}), // 10 FLAMETHROWER
      (name: '';            w: 0;  h: 0;  count: 0),  // 11
      (name: '';            w: 0;  h: 0;  count: 0),  // 12
      (name: '';            w: 0;  h: 0;  count: 0),  // 13
      (name: '';            w: 0;  h: 0;  count: 0),  // 14
      (name: '';            w: 0;  h: 0;  count: 0),  // 15
      (name: '';            w: 0;  h: 0;  count: 0),  // 16
      (name: '';            w: 0;  h: 0;  count: 0),  // 17
      (name: '';            w: 0;  h: 0;  count: 0),  // 18
      (name: '';            w: 0;  h: 0;  count: 0),  // 19
      (name: '';            w: 0;  h: 0;  count: 0),  // 20 ZOMPY_PISTOL
      (name: 'BIMPFIRE';    w: 16; h: 16; count: 2),  // 21 IMP_FIRE
      (name: 'BBSPFIRE';    w: 16; h: 16; count: 2),  // 22 BSP_FIRE
      (name: 'BCACOFIRE';   w: 16; h: 16; count: 2),  // 23 CACO_FIRE
      (name: 'BBARONFIRE';  w: 64; h: 16; count: 2),  // 24 BARON_FIRE
      (name: 'BMANCUBFIRE'; w: 64; h: 32; count: 2),  // 25 MANCUB_FIRE
      (name: 'BSKELFIRE';   w: 64; h: 64; count: 2)   // 26 SKEL_FIRE
    );

{$IFDEF ENABLE_SHELLS}
    ShellAnim: array [0..SHELL_LAST] of record
      name: AnsiString;
      dx, dy: Integer;
    end = (
      (name: 'EBULLET'; dx: 2; dy: 1), // 0 SHELL_BULLET
      (name: 'ESHELL';  dx: 4; dy: 2), // 1 SHELL_SHELL
      (name: 'ESHELL';  dx: 4; dy: 2)  // 2 SHELL_DBLSHELL
    );
{$ENDIF}

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
    Items: array [0..ITEM_LAST] of record
      tex: TGLMultiTexture;
      anim: TAnimState;
    end;
    MonTextures: array [0..MONSTER_MAN] of TMonsterAnims;
    WeapTextures: array [0..WP_LAST, 0..W_POS_LAST, 0..W_ACT_LAST] of TGLTexture;
    ShotTextures: array [0..WEAPON_LAST] of TGLMultiTexture;
    FlagTextures: array [0..FLAG_LAST] of TGLMultiTexture;
    VileFire: TGLMultiTexture;
    Models: array of record
      anim: array [TDirection, 0..A_LAST] of record
        base, mask: TGLMultiTexture;
      end;
{$IFDEF ENABLE_GIBS}
      gibs: record
        base, mask: TGLTextureArray;
        rect: TRectArray;
      end;
{$ENDIF}
    end;

    StubShotAnim: TAnimState;
    FlagAnim: TAnimState;

{$IFDEF ENABLE_SHELLS}
    ShellTextures: array [0..SHELL_LAST] of TGLTexture;
{$ENDIF}
{$IFDEF ENABLE_GFX}
    GFXTextures: array [0..R_GFX_LAST] of TGLMultiTexture;
    gfxlist: array of record
      typ: Byte;
      alpha: Byte;
      x, y: Integer;
      oldX, oldY: Integer;
      anim: TAnimState;
    end = nil;
{$ENDIF}

    plist: TBinHeapPanelDraw = nil;

  class function TBinHeapPanelDrawCmp.less (const a, b: TPanel): Boolean; inline;
  begin
    if a.tag < b.tag then begin result := true; exit; end;
    if a.tag > b.tag then begin result := false; exit; end;
    result := a.arrIdx < b.arrIdx;
  end;

  procedure r_Map_Initialize;
  begin
    StubShotAnim := TAnimState.Create(true, 1, 1);
    FlagAnim := TAnimState.Create(true, 8, 5);
    plist := TBinHeapPanelDraw.Create();
  end;

  procedure r_Map_Finalize;
  begin
    plist.Free;
    StubShotAnim.Invalidate;
  end;

  procedure r_Map_FreeModel (i: Integer);
    var a: Integer; d: TDirection;
  begin
    for d := TDirection.D_LEFT to TDirection.D_RIGHT do
    begin
      for a := A_STAND to A_LAST do
      begin
        if Models[i].anim[d, a].base <> nil then
          Models[i].anim[d, a].base.Free;
        if Models[i].anim[d, a].mask <> nil then
          Models[i].anim[d, a].mask.Free;
        Models[i].anim[d, a].base := nil;
        Models[i].anim[d, a].mask := nil;
      end;
    end;
    {$IFDEF ENABLE_GIBS}
      if Models[i].gibs.base <> nil then
        for a := 0 to High(Models[i].gibs.base) do
          Models[i].gibs.base[a].Free;
      if Models[i].gibs.mask <> nil then
        for a := 0 to High(Models[i].gibs.mask) do
          Models[i].gibs.mask[a].Free;
      Models[i].gibs.base := nil;
      Models[i].gibs.mask := nil;
      Models[i].gibs.rect := nil;
    {$ENDIF}
  end;

  procedure r_Map_LoadModel (i: Integer);
    var prefix: AnsiString; a: Integer; d: TDirection; m: ^TPlayerModelInfo;
  begin
    ASSERT(i < Length(Models));
    ASSERT(i < Length(PlayerModelsArray));
    r_Map_FreeModel(i);
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
    {$IFDEF ENABLE_GIBS}
      Models[i].gibs.base := nil;
      Models[i].gibs.mask := nil;
      Models[i].gibs.rect := nil;
      if m.GibsCount > 0 then
      begin
        SetLength(Models[i].gibs.base, m.GibsCount);
        SetLength(Models[i].gibs.mask, m.GibsCount);
        SetLength(Models[i].gibs.rect, m.GibsCount);
        for a := 0 to m.GibsCount - 1 do
          Models[i].gibs.rect[a] := DefaultGibSize;
        if r_Textures_LoadStreamFromFile(prefix + m.GibsResource, 32, 32, m.GibsCount, Models[i].gibs.base, Models[i].gibs.rect) then
        begin
          if r_Textures_LoadStreamFromFile(prefix + m.GibsMask, 32, 32, m.GibsCount, Models[i].gibs.mask, nil) then
          begin
            // ok
          end;
        end;
        for a := 0 to m.GibsCount - 1 do
          e_logwritefln('model %s gib %s: %sx%s:%sx%s', [i, a, Models[i].gibs.rect[a].x, Models[i].gibs.rect[a].y, Models[i].gibs.rect[a].width, Models[i].gibs.rect[a].height]);
      end;
    {$ENDIF}
  end;

  procedure r_Map_LoadMonsterAnim (m, a: Integer; d: TDirection);
    const dir: array [TDirection] of AnsiString = ('_L', '');
    var w, h, count: Integer;
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
      );
    end
    else
      MonTextures[m, a, d] := nil;
  end;

  procedure r_Map_Load;
    const
      WeapName: array [0..WP_LAST] of AnsiString = ('', 'CSAW', 'HGUN', 'SG', 'SSG', 'MGUN', 'RKT', 'PLZ', 'BFG', 'SPL', 'FLM');
      WeapPos: array [0..W_POS_LAST] of AnsiString = ('', '_UP', '_DN');
      WeapAct: array [0..W_ACT_LAST] of AnsiString = ('', '_FIRE');
    var
      i, j, k: Integer; d: TDirection;
  begin
    // --------- items --------- //
    for i := 0 to ITEM_LAST do
    begin
      if ItemAnim[i].n > 0 then
      begin
        Items[i].tex := r_Textures_LoadMultiFromFileAndInfo(
          GameWAD + ':TEXTURES/' + ItemAnim[i].name,
          ItemAnim[i].w,
          ItemAnim[i].h,
          ItemAnim[i].n,
          ItemAnim[i].b,
          false
        );
        k := IfThen(ItemAnim[i].b, ItemAnim[i].n * 2 - 2, ItemAnim[i].n);
        Items[i].anim := TAnimState.Create(True, ItemAnim[i].d, k);
      end;
    end;
    // --------- monsters --------- //
    for i := MONSTER_DEMON to MONSTER_MAN do
      for j := 0 to ANIM_LAST do
        for d := TDirection.D_LEFT to TDirection.D_RIGHT do
          r_Map_LoadMonsterAnim(i, j, d);
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
    // --------- gfx animations --------- //
    {$IFDEF ENABLE_GFX}
      for i := 1 to R_GFX_LAST do
        if GFXAnim[i].count > 0 then
          GFXTextures[i] := r_Textures_LoadMultiFromFileAndInfo(GameWad + ':TEXTURES/' + GFXAnim[i].name, GFXAnim[i].w, GFXAnim[i].h, GFXAnim[i].count, GFXAnim[i].back);
    {$ENDIF}
    // --------- shots --------- //
    for i := 0 to WEAPON_LAST do
      if ShotAnim[i].count > 0 then
        ShotTextures[i] := r_Textures_LoadMultiFromFileAndInfo(GameWad + ':TEXTURES/' + ShotAnim[i].name, ShotAnim[i].w, ShotAnim[i].h, ShotAnim[i].count, false);
    // --------- flags --------- //
    FlagTextures[FLAG_NONE] := nil;
    FlagTextures[FLAG_RED]  := r_Textures_LoadMultiFromFileAndInfo(GameWad + ':TEXTURES/FLAGRED',  64, 64, 5, false);
    FlagTextures[FLAG_BLUE] := r_Textures_LoadMultiFromFileAndInfo(GameWad + ':TEXTURES/FLAGBLUE', 64, 64, 5, false);
    // FlagTextures[FLAG_DOM]  := r_Textures_LoadMultiFromFileAndInfo(GameWad + ':TEXTURES/FLAGDOM',  64, 64, 8, false);
    // --------- shells --------- //
    {$IFDEF ENABLE_SHELLS}
      for i := 0 to SHELL_LAST do
        ShellTextures[i] := r_Textures_LoadFromFile(GameWad + ':TEXTURES/' + ShellAnim[i].name);
    {$ENDIF}
  end;

  procedure r_Map_Free;
    var i, j, k, a: Integer; d: TDirection;
  begin
    {$IFDEF ENABLE_SHELLS}
      for i := 0 to SHELL_LAST do
      begin
        if ShellTextures[i] <> nil then
          ShellTextures[i].Free;
        ShellTextures[i] := nil;
      end;
    {$ENDIF}
    for i := 0 to FLAG_LAST do
    begin
      if FlagTextures[i] <> nil then
        FlagTextures[i].Free;
      FlagTextures[i] := nil;
    end;
    for i := 0 to WEAPON_LAST do
    begin
      if ShotTextures[i] <> nil then
        ShotTextures[i].Free;
      ShotTextures[i] := nil;
    end;
    {$IFDEF ENABLE_GFX}
      gfxlist := nil;
      for i := 0 to R_GFX_LAST do
      begin
        if GFXTextures[i] <> nil then
          GFXTextures[i].Free;
        GFXTextures[i] := nil;
      end;
    {$ENDIF}
    for i := 1 to WP_LAST do
    begin
      for j := 0 to W_POS_LAST do
      begin
        for k := 0 to W_ACT_LAST do
        begin
          if WeapTextures[i, j, k] <> nil then
            WeapTextures[i, j, k].Free;
          WeapTextures[i, j, k] := nil;
        end;
      end;
    end;
    if Models <> nil then
      for i := 0 to High(Models) do
        r_Map_FreeModel(i);
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
    for i := 0 to ITEM_LAST do
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
        if t = nil then
          r_Draw_TextureRepeat(nil, p.x, p.y, p.width, p.height, false, 255, 255, 255, 255 - p.alpha, p.blending)
        else if p.TextureIDs[p.FCurTexture].AnTex.IsValid() then
          r_Draw_MultiTextureRepeat(t, p.TextureIDs[p.FCurTexture].AnTex, p.x, p.y, p.width, p.height, false, 255, 255, 255, 255 - p.alpha, p.blending)
        else
          r_Draw_TextureRepeat(t.GetTexture(0), p.x, p.y, p.width, p.height, false, 255, 255, 255, 255 - p.alpha, p.blending)
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
        if it.used and it.alive and (it.dropped = drop) and (it.ItemType > ITEM_NONE) and (it.ItemType <= ITEM_LAST) then
        begin
          t := Items[it.ItemType].tex;
          if g_Collide(it.obj.x, it.obj.y, t.width, t.height, x, y, w, h) then
          begin
            it.obj.Lerp(gLerpFactor, fX, fY);
            r_Draw_MultiTextureRepeat(t, Items[it.ItemType].anim, fX, fY, t.width, t.height, false, 255, 255, 255, 255, false);
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
          r_Draw_MultiTextureRepeat(VileFire, mon.VileFireAnim, o.x + o.rect.x + (o.rect.width div 2) - VILEFIRE_DX, o.y + o.rect.y + o.rect.height - VILEFIRE_DY, VileFire.width, VileFire.height, False, 255, 255, 255, 255, false);
  end;

  procedure r_Map_DrawMonster (constref mon: TMonster);
    var m, a, fX, fY, dx, dy: Integer; d: TDirection; flip: Boolean; t: TGLMultiTexture;
  begin
    m := mon.MonsterType;
    a := mon.MonsterAnim;
    d := mon.GameDirection;

    mon.obj.Lerp(gLerpFactor, fX, fY);

    if r_Map_GetMonsterTexture(m, a, d, t, dx, dy, flip) then
      r_Draw_MultiTextureRepeat(t, mon.DirAnim[a, d], fX + dx, fY + dy, t.width, t.height, flip, 255, 255, 255, 255, false);

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
    var a, pos, act, xx, yy, angle: Integer; d: TDirection; flip: Boolean; t: TGLMultiTexture; tex: TGLTexture; c: TRGB;
  begin
    a := pm.CurrentAnimation;
    d := pm.Direction;

    (* draw flag*)
    t := FlagTextures[pm.Flag];
    if (t <> nil) and not (a in [A_DIE1, A_DIE2])  then
    begin
      flip := d = TDirection.D_RIGHT;
      angle := PlayerModelsArray[pm.id].FlagAngle;
      xx := PlayerModelsArray[pm.id].FlagPoint.X;
      yy := PlayerModelsArray[pm.id].FlagPoint.Y;
      r_Draw_MultiTextureRepeatRotate(
        t,
        FlagAnim,
        x + IfThen(flip, 2 * FLAG_BASEPOINT.X - xx + 1, xx - 1) - FLAG_BASEPOINT.X,
        y + yy - FLAG_BASEPOINT.Y + 1,
        t.width,
        t.height,
        flip,
        255, 255, 255, 255, false,
        IfThen(flip, 64 - FLAG_BASEPOINT.X, FLAG_BASEPOINT.X),
        FLAG_BASEPOINT.Y,
        IfThen(flip, angle, -angle)
      );
    end;

    (* draw weapon *)
    if PlayerModelsArray[pm.id].HaveWeapon and not (a in [A_DIE1, A_DIE2, A_PAIN]) then
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
          d = TDirection.D_LEFT,
          255, 255, 255, 255, false
        );
      end;
    end;

    (* draw body *)
    if r_Map_GetPlayerModelTex(pm.id, a, d, flip) then
    begin
      t := Models[pm.id].anim[d, a].base;
      r_Draw_MultiTextureRepeat(t, pm.AnimState, x, y, t.width, t.height, flip, 255, 255, 255, 255, false);
      t := Models[pm.id].anim[d, a].mask;
      if t <> nil then
      begin
        c := pm.Color;
        r_Draw_MultiTextureRepeat(t, pm.AnimState, x, y, t.width, t.height, flip, c.r, c.g, c.b, 255, false);
      end;
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

{$IFDEF ENABLE_GIBS}
  function r_Map_GetGibSize (m, i: Integer): TRectWH;
  begin
    result := Models[m].gibs.rect[i];
  end;

  procedure r_Map_DrawGibs (x, y, w, h: Integer);
    var i, fx, fy, m, id, rx, ry, ra: Integer; p: PObj; t: TGLTexture;
  begin
    if gGibs <> nil then
    begin
      for i := 0 to High(gGibs) do
      begin
        if gGibs[i].alive then
        begin
          p := @gGibs[i].Obj;
          if g_Obj_Collide(x, y, w, h, p) then
          begin
            p.Lerp(gLerpFactor, fx, fy);
            id := gGibs[i].GibID;
            m := gGibs[i].ModelID;
            t := Models[m].gibs.base[id];
            if t <> nil then
            begin
              rx := p.Rect.X + p.Rect.Width div 2;
              ry := p.Rect.Y + p.Rect.Height div 2;
              ra := gGibs[i].RAngle;
              r_Draw_TextureRepeatRotate(t, fx, fy, t.width, t.height, false, 255, 255, 255, 255, false, rx, ry, ra);
              t := Models[m].gibs.mask[id];
              if t <> nil then
                r_Draw_TextureRepeatRotate(t, fx, fy, t.width, t.height, false, gGibs[i].Color.R, gGibs[i].Color.G, gGibs[i].Color.B, 255, false, rx, ry, ra);
              // r_Draw_TextureRepeatRotate(nil, fx + p.Rect.X, fy + p.Rect.Y, p.Rect.Width, p.Rect.Height, false, 255, 255, 255, 255, false, p.Rect.Width div 2, p.Rect.Height div 2, ra);
            end;
          end;
        end;
      end;
    end;
  end;
{$ENDIF}

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

{$IFDEF ENABLE_GFX}
  function r_Map_GetGFXID (): Integer;
    var i: Integer;
  begin
    i := 0;
    if gfxlist <> nil then
    begin
      while (i < Length(gfxlist)) and gfxlist[i].anim.IsValid() do
        Inc(i);
      if i >= Length(gfxlist) then
        SetLength(gfxlist, Length(gfxlist) + 1)
    end
    else
      SetLength(gfxlist, 1);
    gfxlist[i].typ := R_GFX_NONE;
    gfxlist[i].anim.Invalidate;
    result := i
  end;

  procedure r_Map_NewGFX (typ, x, y: Integer);
    var i: Integer;
  begin
    if gpart_dbg_enabled and (typ > 0) and (typ <= R_GFX_LAST) then
    begin
      i := r_Map_GetGFXID();
      if i >= 0 then
      begin
        gfxlist[i].typ := typ;
        gfxlist[i].x := x;
        gfxlist[i].y := y;
        gfxlist[i].oldX := x;
        gfxlist[i].oldY := y;
        gfxlist[i].anim := TAnimState.Create(false, GFXAnim[typ].speed + Random(GFXAnim[typ].rspeed), GFXAnim[typ].count);
        gfxlist[i].anim.Reset();
        gfxlist[i].anim.Enable();
      end;
    end;
  end;

  procedure r_Map_UpdateGFX;
    var i: Integer;
  begin
    if gfxlist <> nil then
    begin
      for i := 0 to High(gfxlist) do
      begin
        if gfxlist[i].anim.IsValid() then
        begin
          gfxlist[i].oldX := gfxlist[i].x;
          gfxlist[i].oldY := gfxlist[i].y;
          case gfxlist[i].typ of
            R_GFX_FLAME, R_GFX_SMOKE:
            begin
              if Random(3) = 0 then
                gfxlist[i].x := gfxlist[i].x - 1 + Random(3);
              if Random(2) = 0 then
                gfxlist[i].y := gfxlist[i].y - Random(2);
            end;
          end;
          if gfxlist[i].anim.played then
            gfxlist[i].anim.Invalidate
          else
            gfxlist[i].anim.Update
        end;
      end;
    end;
  end;

  procedure r_Map_DrawParticles (x, y, w, h: Integer);
    var i, fx, fy: Integer;
  begin
    if gpart_dbg_enabled and (Particles <> nil) then
    begin
      glDisable(GL_TEXTURE_2D);
      if (g_dbg_scale < 0.6) then
        glPointSize(1)
      else if (g_dbg_scale > 1.3) then
        glPointSize(g_dbg_scale + 1)
      else
        glPointSize(2);
      glDisable(GL_POINT_SMOOTH);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

      glBegin(GL_POINTS);
        for i := 0 to High(Particles) do
        begin
          if Particles[i].alive then
          begin
            fx := nlerp(Particles[i].oldX, Particles[i].x, gLerpFactor);
            fy := nlerp(Particles[i].oldY, Particles[i].y, gLerpFactor);
            glColor4ub(Particles[i].red, Particles[i].green, Particles[i].blue, Particles[i].alpha);
            glVertex2f(fx, fy);
          end;
        end;
      glEnd;

      glDisable(GL_BLEND);
    end;
  end;

  procedure r_Map_DrawGFX (x, y, w, h: Integer);
    var i, fx, fy, typ: Integer; tex: TGLMultiTexture;
  begin
    if gfxlist <> nil then
    begin
      for i := 0 to High(gfxlist) do
      begin
        if gfxlist[i].anim.IsValid() then
        begin
          typ := gfxlist[i].typ;
          tex := GFXTextures[typ];
          if tex <> nil then
          begin
            fx := nlerp(gfxlist[i].oldX, gfxlist[i].x, gLerpFactor);
            fy := nlerp(gfxlist[i].oldY, gfxlist[i].y, gLerpFactor);
            r_Draw_MultiTextureRepeat(tex, gfxlist[i].anim, fx, fy, tex.width, tex.height, false, 255, 255, 255, 255 - GFXAnim[typ].alpha, false);
          end;
        end;
      end;
    end;
  end;
{$ENDIF}

  procedure r_Map_DrawShots (x, y, w, h: Integer);
    var i, a, fX, fY, pX, pY, typ: Integer; tex: TGLMultiTexture; anim: ^TAnimState;
  begin
    if Shots <> nil then
    begin
      for i := 0 to High(Shots) do
      begin
        typ := Shots[i].ShotType;
        if typ <> 0 then
        begin
          tex := ShotTextures[typ];
          if tex <> nil then
          begin
            a := 0;
            case typ of
              WEAPON_ROCKETLAUNCHER, WEAPON_BARON_FIRE, WEAPON_MANCUB_FIRE, WEAPON_SKEL_FIRE:
                a := -GetAngle2(Shots[i].Obj.Vel.X, Shots[i].Obj.Vel.Y)
            end;
            Shots[i].Obj.Lerp(gLerpFactor, fX, fY);
            pX := Shots[i].Obj.Rect.Width div 2;
            pY := Shots[i].Obj.Rect.Height div 2;
            // TODO fix this hack
            if Shots[i].Animation.IsValid() then anim := @Shots[i].Animation else anim := @StubShotAnim;
            r_Draw_MultiTextureRepeatRotate(tex, anim^, fX, fY, tex.width, tex.height, false, 255, 255, 255, 255, false, pX, pY, a);
          end;
        end;
      end;
    end;
  end;

  procedure r_Map_DrawFlags (x, y, w, h: Integer);
    var i, dx, fx, fy: Integer; flip: Boolean; tex: TGLMultiTexture;
  begin
    if gGameSettings.GameMode = GM_CTF then
    begin
      for i := FLAG_RED to FLAG_BLUE do
      begin
        if not (gFlags[i].state in [FLAG_STATE_NONE, FLAG_STATE_CAPTURED]) then
        begin
          gFlags[i].Obj.Lerp(gLerpFactor, fx, fy);
          flip := gFlags[i].Direction = TDirection.D_LEFT;
          if flip then dx := -1 else dx := +1;
          tex := FlagTextures[i];
          r_Draw_MultiTextureRepeat(tex, FlagAnim, fx + dx, fy + 1, tex.width, tex.height, flip, 255, 255, 255, 255, false)
          // TODO g_debug_frames
        end;
      end;
    end;
  end;

{$IFDEF ENABLE_SHELLS}
  procedure r_Map_DrawShells (x, y, w, h: Integer);
    var i, fx, fy, typ: Integer; t: TGLTexture; p: PObj;
  begin
    if gShells <> nil then
    begin
      for i := 0 to High(gShells) do
      begin
        if gShells[i].alive then
        begin
          typ := gShells[i].SType;
          if typ <= SHELL_LAST then
          begin
            p := @gShells[i].Obj;
            if g_Obj_Collide(x, y, w, h, p) then
            begin
              t := ShellTextures[typ];
              if t <> nil then
              begin
                p.Lerp(gLerpFactor, fx, fy);
                r_Draw_TextureRepeatRotate(t, fx, fy, t.width, t.height, false, 255, 255, 255, 255, false, ShellAnim[typ].dx, ShellAnim[typ].dy, gShells[i].RAngle);
              end;
            end;
          end;
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
      r_Draw_Texture(SkyTexture, x, y, w, h, false, 255, 255, 255, 255, false);

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
    r_Map_DrawShots(xx, yy, ww, hh);
    {$IFDEF ENABLE_SHELLS}
      r_Map_DrawShells(xx, yy, ww, hh);
    {$ENDIF}
    r_Map_DrawPlayers(xx, yy, ww, hh);
    {$IFDEF ENABLE_GIBS}
      r_Map_DrawGibs(xx, yy, ww, hh);
    {$ENDIF}
    {$IFDEF ENABLE_CORPSES}
      r_Map_DrawCorpses(xx, yy, ww, hh);
    {$ENDIF}
    r_Map_DrawPanelType(PANEL_WALL);
    r_Map_DrawMonsters(xx, yy, ww, hh);
    r_Map_DrawItems(xx, yy, ww, hh, true);
    r_Map_DrawPanelType(PANEL_CLOSEDOOR);
    {$IFDEF ENABLE_GFX}
      r_Map_DrawParticles(xx, yy, ww, hh);
      r_Map_DrawGFX(xx, yy, ww, hh);
    {$ENDIF}
    r_Map_DrawFlags(xx, yy, ww, hh);
    r_Map_DrawPanelType(PANEL_ACID1);
    r_Map_DrawPanelType(PANEL_ACID2);
    r_Map_DrawPanelType(PANEL_WATER);
    r_Map_DrawPanelType(PANEL_FORE);
    glPopMatrix;
  end;

  procedure r_Map_Update;
    var i: Integer;
  begin
    for i := 0 to ITEM_LAST do
      Items[i].anim.Update;
    r_Map_UpdateGFX;
    FlagAnim.Update;
  end;

end.
