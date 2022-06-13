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

  uses g_base, g_player, g_playermodel; // TRectWH, TPlayer, TPlayerModel

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
{$IFDEF ENABLE_MENU}
  procedure r_Map_DrawPlayerModel (pm: TPlayerModel; x, y: Integer; alpha: Byte);
{$ENDIF}

  procedure r_Map_Update;

  procedure r_Map_Draw (x, y, w, h, camx, camy: Integer; player: TPlayer);

implementation

  uses
    Math, SysUtils,
    {$IFDEF USE_GLES1}
      GLES11,
    {$ELSE}
      GL, GLEXT,
    {$ENDIF}
    e_log,
    binheap, MAPDEF, utils,
    g_options, g_animations, g_basic, g_phys,
    g_game, g_map, g_panel, g_items, g_monsters, g_weapons,
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
    r_textures, r_draw, r_common
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
      anim: TAnimInfo;
    end = (
      (name: 'NOTEXTURE';    w: 16; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'MED1';         w: 16; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'MED2';         w: 32; h: 32; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'BMED';         w: 32; h: 32; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'ARMORGREEN';   w: 32; h: 16; anim: (loop: true; delay: 20; frames: 3; back: true)),
      (name: 'ARMORBLUE';    w: 32; h: 16; anim: (loop: true; delay: 20; frames: 3; back: true)),
      (name: 'SBLUE';        w: 32; h: 32; anim: (loop: true; delay: 15; frames: 4; back: true)),
      (name: 'SWHITE';       w: 32; h: 32; anim: (loop: true; delay: 20; frames: 4; back: true)),
      (name: 'SUIT';         w: 32; h: 64; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'OXYGEN';       w: 16; h: 32; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'INVUL';        w: 32; h: 32; anim: (loop: true; delay: 20; frames: 4; back: true)),
      (name: 'SAW';          w: 64; h: 32; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'SHOTGUN1';     w: 64; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'SHOTGUN2';     w: 64; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'MGUN';         w: 64; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'RLAUNCHER';    w: 64; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'PGUN';         w: 64; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'BFG';          w: 64; h: 64; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'SPULEMET';     w: 64; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'CLIP';         w: 16; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'AMMO';         w: 32; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'SHELL1';       w: 16; h: 8;  anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'SHELL2';       w: 32; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'ROCKET';       w: 16; h: 32; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'ROCKETS';      w: 64; h: 32; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'CELL';         w: 16; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'CELL2';        w: 32; h: 32; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'BPACK';        w: 32; h: 32; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'KEYR';         w: 16; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'KEYG';         w: 16; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'KEYB';         w: 16; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'KASTET';       w: 64; h: 32; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'PISTOL';       w: 64; h: 16; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'BOTTLE';       w: 16; h: 32; anim: (loop: true; delay: 20; frames: 4; back: true)),
      (name: 'HELMET';       w: 16; h: 16; anim: (loop: true; delay: 20; frames: 4; back: true)),
      (name: 'JETPACK';      w: 32; h: 32; anim: (loop: true; delay: 15; frames: 3; back: true)),
      (name: 'INVIS';        w: 32; h: 32; anim: (loop: true; delay: 20; frames: 4; back: true)),
      (name: 'FLAMETHROWER'; w: 64; h: 32; anim: (loop: true; delay: 1;  frames: 1; back: false)),
      (name: 'FUELCAN';      w: 16; h: 32; anim: (loop: true; delay: 1;  frames: 1; back: false))
    );

{$IFDEF ENABLE_GFX}
    GFXAnim: array [0..R_GFX_LAST] of record
      name: AnsiString;
      w, h: Integer;
      anim: TAnimInfo;
      rdelay: Integer;
      alpha: Integer;
    end = (
      (name: '';            w: 0;   h: 0;   anim: (loop: false; delay: 0; frames: 0;  back: false); rdelay: 0; alpha: 0),
      (name: 'TELEPORT';    w: 64;  h: 64;  anim: (loop: false; delay: 6; frames: 10; back: false); rdelay: 0; alpha: 0),
      (name: 'FLAME';       w: 32;  h: 32;  anim: (loop: false; delay: 3; frames: 11; back: false); rdelay: 0; alpha: 0),
      (name: 'EROCKET';     w: 128; h: 128; anim: (loop: false; delay: 6; frames: 6;  back: false); rdelay: 0; alpha: 0),
      (name: 'EBFG';        w: 128; h: 128; anim: (loop: false; delay: 6; frames: 6;  back: false); rdelay: 0; alpha: 0),
      (name: 'BFGHIT';      w: 64;  h: 64;  anim: (loop: false; delay: 4; frames: 4;  back: false); rdelay: 0; alpha: 0),
      (name: 'FIRE';        w: 64;  h: 128; anim: (loop: false; delay: 4; frames: 8;  back: false); rdelay: 2; alpha: 0),
      (name: 'ITEMRESPAWN'; w: 32;  h: 32;  anim: (loop: false; delay: 4; frames: 5;  back: true);  rdelay: 0; alpha: 0),
      (name: 'SMOKE';       w: 32;  h: 32;  anim: (loop: false; delay: 3; frames: 10; back: false); rdelay: 0; alpha: 0),
      (name: 'ESKELFIRE';   w: 64;  h: 64;  anim: (loop: false; delay: 8; frames: 3;  back: false); rdelay: 0; alpha: 0),
      (name: 'EPLASMA';     w: 32;  h: 32;  anim: (loop: false; delay: 3; frames: 4;  back: true);  rdelay: 0; alpha: 0),
      (name: 'EBSPFIRE';    w: 32;  h: 32;  anim: (loop: false; delay: 3; frames: 5;  back: false); rdelay: 0; alpha: 0),
      (name: 'EIMPFIRE';    w: 64;  h: 64;  anim: (loop: false; delay: 6; frames: 3;  back: false); rdelay: 0; alpha: 0),
      (name: 'ECACOFIRE';   w: 64;  h: 64;  anim: (loop: false; delay: 6; frames: 3;  back: false); rdelay: 0; alpha: 0),
      (name: 'EBARONFIRE';  w: 64;  h: 64;  anim: (loop: false; delay: 6; frames: 3;  back: false); rdelay: 0; alpha: 0),
      (name: 'TELEPORT';    w: 64;  h: 64;  anim: (loop: false; delay: 3; frames: 10; back: false); rdelay: 0; alpha: 0),   // fast
      (name: 'SMOKE';       w: 32;  h: 32;  anim: (loop: false; delay: 3; frames: 10; back: false); rdelay: 0; alpha: 150), // transparent
      (name: 'FLAME';       w: 32;  h: 32;  anim: (loop: false; delay: 3; frames: 11; back: false); rdelay: 2; alpha: 0)    // random
    );
{$ENDIF}

    ShotAnim: array [0..WEAPON_LAST] of record
      name: AnsiString;
      w, h: Integer;
      anim: TAnimInfo;
    end = (
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 0  KASTET
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 1  SAW
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 2  PISTOL
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 3  SHOTGUN1
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 4  SHOTGUN2
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 5  CHAINGUN
      (name: 'BROCKET';     w: 64; h: 32; anim: (loop: true; delay: 1; frames: 1; back: false)),  // 6  ROCKETLAUNCHER
      (name: 'BPLASMA';     w: 16; h: 16; anim: (loop: true; delay: 5; frames: 2; back: false)),  // 7  PLASMA
      (name: 'BBFG';        w: 64; h: 64; anim: (loop: true; delay: 6; frames: 2; back: false)),  // 8  BFG
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 9  SUPERPULEMET
      (name: 'FLAME';       w: 32; h: 32; anim: (loop: true; delay: 6; frames: 0{11}; back: false)), // 10 FLAMETHROWER
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 11
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 12
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 13
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 14
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 15
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 16
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 17
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 18
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 19
      (name: '';            w: 0;  h: 0;  anim: (loop: true; delay: 0; frames: 0; back: false)),  // 20 ZOMPY_PISTOL
      (name: 'BIMPFIRE';    w: 16; h: 16; anim: (loop: true; delay: 4; frames: 2; back: false)),  // 21 IMP_FIRE
      (name: 'BBSPFIRE';    w: 16; h: 16; anim: (loop: true; delay: 4; frames: 2; back: false)),  // 22 BSP_FIRE
      (name: 'BCACOFIRE';   w: 16; h: 16; anim: (loop: true; delay: 4; frames: 2; back: false)),  // 23 CACO_FIRE
      (name: 'BBARONFIRE';  w: 64; h: 16; anim: (loop: true; delay: 4; frames: 2; back: false)),  // 24 BARON_FIRE
      (name: 'BMANCUBFIRE'; w: 64; h: 32; anim: (loop: true; delay: 4; frames: 2; back: false)),  // 25 MANCUB_FIRE
      (name: 'BSKELFIRE';   w: 64; h: 64; anim: (loop: true; delay: 5; frames: 2; back: false))   // 26 SKEL_FIRE
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

    PunchAnim: TAnimInfo = (loop: false; delay: 1; frames: 4; back: false);
    FlagAnim: TAnimInfo = (loop: true; delay: 8; frames: 5; back: false);
    VileFireAnim: TAnimInfo = (loop: true; delay:  2; frames: 8; back: false);

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
      frame: Integer;
    end;
    MonTextures: array [0..MONSTER_MAN] of TMonsterAnims;
    WeapTextures: array [0..WP_LAST, 0..W_POS_LAST, 0..W_ACT_LAST] of TGLTexture;
    ShotTextures: array [0..WEAPON_LAST] of TGLMultiTexture;
    FlagTextures: array [0..FLAG_LAST] of TGLMultiTexture;
    PunchTextures: array [Boolean, 0..2] of TGLMultiTexture; // [berserk, center/up/down]
    VileFire: TGLMultiTexture;
    InvulPenta: TGLTexture;
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

{$IFDEF ENABLE_SHELLS}
    ShellTextures: array [0..SHELL_LAST] of TGLTexture;
{$ENDIF}
{$IFDEF ENABLE_GFX}
    GFXTextures: array [0..R_GFX_LAST] of TGLMultiTexture;
    gfxlist: array of record
      typ: Byte;
      x, y: Integer;
      oldX, oldY: Integer;
      anim: TAnimInfo;
      time: LongWord;
      frame: LongInt;
    end = nil;
{$ENDIF}

    FlagFrame: LongInt;
    plist: TBinHeapPanelDraw = nil;

  class function TBinHeapPanelDrawCmp.less (const a, b: TPanel): Boolean; inline;
  begin
    if a.tag < b.tag then begin result := true; exit; end;
    if a.tag > b.tag then begin result := false; exit; end;
    result := a.arrIdx < b.arrIdx;
  end;

  procedure r_Map_Initialize;
  begin
    FlagFrame := 0;
    plist := TBinHeapPanelDraw.Create();
  end;

  procedure r_Map_Finalize;
  begin
    plist.Free;
    FlagFrame := 0;
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
        if r_Textures_LoadStreamFromFile(prefix + m.GibsResource, 32, 32, m.GibsCount, m.GibsCount, Models[i].gibs.base, Models[i].gibs.rect) then
        begin
          if r_Textures_LoadStreamFromFile(prefix + m.GibsMask, 32, 32, m.GibsCount, m.GibsCount, Models[i].gibs.mask, nil) then
          begin
            // ok
          end;
        end;
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
      PunchName: array [Boolean] of AnsiString = ('PUNCH', 'PUNCHB');
    var
      i, j, k: Integer; d: TDirection; b: Boolean;
  begin
    // --------- items --------- //
    for i := 0 to ITEM_LAST do
    begin
      Items[i].tex := r_Textures_LoadMultiFromFileAndInfo(
        GameWAD + ':TEXTURES/' + ItemAnim[i].name,
        ItemAnim[i].w,
        ItemAnim[i].h,
        ItemAnim[i].anim.frames,
        ItemAnim[i].anim.back,
        false
      );
      Items[i].frame := 0;
    end;
    // --------- monsters --------- //
    for i := MONSTER_DEMON to MONSTER_MAN do
      for j := 0 to ANIM_LAST do
        for d := TDirection.D_LEFT to TDirection.D_RIGHT do
          r_Map_LoadMonsterAnim(i, j, d);
    VileFire := r_Textures_LoadMultiFromFileAndInfo(GameWAD + ':TEXTURES/FIRE', 64, 128, VileFireAnim.frames, VileFireAnim.back);
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
        if GFXAnim[i].anim.frames > 0 then
          GFXTextures[i] := r_Textures_LoadMultiFromFileAndInfo(GameWad + ':TEXTURES/' + GFXAnim[i].name, GFXAnim[i].w, GFXAnim[i].h, GFXAnim[i].anim.frames, GFXAnim[i].anim.back);
    {$ENDIF}
    // --------- shots --------- //
    for i := 0 to WEAPON_LAST do
      if ShotAnim[i].anim.frames > 0 then
        ShotTextures[i] := r_Textures_LoadMultiFromFileAndInfo(GameWad + ':TEXTURES/' + ShotAnim[i].name, ShotAnim[i].w, ShotAnim[i].h, ShotAnim[i].anim.frames, ShotAnim[i].anim.back);
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
    // --------- punch --------- //
    for b := false to true do
    begin
      for i := 0 to 2 do
        PunchTextures[b, i] := r_Textures_LoadMultiFromFileAndInfo(GameWad + ':WEAPONS/' + PunchName[b] + WeapPos[i], 64, 64, PunchAnim.frames, PunchAnim.back);
    end;
    // --------- other --------- //
    InvulPenta := r_Textures_LoadFromFile(GameWad + ':TEXTURES/PENTA');
  end;

  procedure r_Map_Free;
    var i, j, k: Integer; d: TDirection; b: Boolean;
  begin
    if InvulPenta <> nil then
      InvulPenta.Free;
    InvulPenta := nil;
    for b := false to true do
    begin
      for i := 0 to 2 do
      begin
        if PunchTextures[b, i] <> nil then
          PunchTextures[b, i].Free;
        PunchTextures[b, i] := nil;
      end;
    end;
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
        Items[i].tex.Free;
      Items[i].tex := nil;
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
    var i, fX, fY: Integer; it: PItem; t: TGLMultiTexture; tex: TGLTexture;
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
            tex := t.GetTexture(Items[it.ItemType].frame);
            r_Draw_TextureRepeat(tex, fX, fY, tex.width, tex.height, false, 255, 255, 255, 255, false);
          end;
        end;
      end;
    end;
    // TODO draw g_debug_frames
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
    var o: TObj; count, frame: LongInt; tex: TGLTexture;
  begin
    if VileFire <> nil then
      if (mon.MonsterType = MONSTER_VILE) and (mon.MonsterState = MONSTATE_SHOOT) then
        if (mon.VileFireTime <= gTime) and GetPos(mon.MonsterTargetUID, @o) then
        begin
          g_Anim_GetFrameByTime(VileFireAnim, (gTime - mon.VileFireTime) DIV GAME_TICK, count, frame);
          tex := VileFire.GetTexture(frame);
          r_Draw_TextureRepeat(tex, o.x + o.rect.x + (o.rect.width div 2) - VILEFIRE_DX, o.y + o.rect.y + o.rect.height - VILEFIRE_DY, tex.width, tex.height, False, 255, 255, 255, 255, false);
        end;
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

    // TODO draw g_debug_frames
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

  procedure r_Map_DrawPlayerModel (pm: TPlayerModel; x, y: Integer; alpha: Byte);
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
      tex := t.GetTexture(FlagFrame);
      r_Draw_TextureRepeatRotate(
        tex,
        x + IfThen(flip, 2 * FLAG_BASEPOINT.X - xx + 1, xx - 1) - FLAG_BASEPOINT.X,
        y + yy - FLAG_BASEPOINT.Y + 1,
        tex.width,
        tex.height,
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
          255, 255, 255, alpha, false
        );
      end;
    end;

    (* draw body *)
    if r_Map_GetPlayerModelTex(pm.id, a, d, flip) then
    begin
      t := Models[pm.id].anim[d, a].base;
      r_Draw_MultiTextureRepeat(t, pm.AnimState, x, y, t.width, t.height, flip, 255, 255, 255, alpha, false);
      t := Models[pm.id].anim[d, a].mask;
      if t <> nil then
      begin
        c := pm.Color;
        r_Draw_MultiTextureRepeat(t, pm.AnimState, x, y, t.width, t.height, flip, c.r, c.g, c.b, alpha, false);
      end;
    end;
  end;

  procedure r_Map_DrawPlayer (p, drawed: TPlayer);
    var fX, fY, fSlope, ax, ay, w, h: Integer; b, flip: Boolean; t: TGLMultiTexture; tex: TGLTexture; alpha: Byte; count, frame: LongInt;
  begin
    if p.alive then
    begin
      fX := p.obj.x; fY := p.obj.y;
      // TODO fix lerp
      //p.obj.Lerp(gLerpFactor, fX, fY);
      fSlope := nlerp(p.SlopeOld, p.obj.slopeUpLeft, gLerpFactor);

      (* punch effect *)
      if p.PunchTime <= gTime then
      begin
        g_Anim_GetFrameByTime(PunchAnim, (gTime - p.PunchTime) DIV GAME_TICK, count, frame);
        if count < 1 then
        begin
          b := R_BERSERK in p.FRulez;
          if p.FKeys[KEY_DOWN].pressed then
            t := PunchTextures[b, 2]
          else if p.FKeys[KEY_UP].pressed then
            t := PunchTextures[b, 1]
          else
            t := PunchTextures[b, 0];
          if t <> nil then
          begin
            flip := p.Direction = TDirection.D_LEFT;
            ax := IfThen(flip, 15 - p.Obj.Rect.X, p.Obj.Rect.X - 15); // ???
            ay := p.Obj.Rect.Y - 11;
            tex := t.GetTexture(frame);
            r_Draw_TextureRepeat(tex, fx + ax, fy + fSlope + ay, tex.width, tex.height, flip, 255, 255, 255, 255, false)
          end;
        end;
      end;

      (* invulnerability effect *)
      if (InvulPenta <> nil) and (p.FMegaRulez[MR_INVUL] > gTime) and ((p <> drawed) or (p.SpawnInvul >= gTime)) then
      begin
        w := InvulPenta.width;
        h := InvulPenta.height;
        ax := p.Obj.Rect.X + (p.Obj.Rect.Width div 2) - (w div 2); // + IfThen(flip, +4, -2) // ???
        ay := p.Obj.Rect.Y + (p.Obj.Rect.Height div 2) - (h div 2) - 7; // ???
        r_Draw_Texture(InvulPenta, fx + ax, fy + ay + fSlope, w, h, false, 255, 255, 255, 255, false);
      end;

      (* invisibility effect *)
      alpha := 255;
      if p.FMegaRulez[MR_INVIS] > gTime then
      begin
        if (drawed <> nil) and ((p = drawed) or ((p.Team = drawed.Team) and (gGameSettings.GameMode <> GM_DM))) then
        begin
          if (p.FMegaRulez[MR_INVIS] - gTime > 2100) or not ODD((p.FMegaRulez[MR_INVIS] - gTime) div 300) then
            alpha := 55;
        end
        else
          alpha := 1; // ???
      end;

      r_Map_DrawPlayerModel(p.Model, fX, fY + fSlope, alpha);
    end;
    // TODO draw g_debug_frames
    // TODO draw chat bubble
    // TODO draw aim
  end;

  procedure r_Map_DrawPlayers (x, y, w, h: Integer; player: TPlayer);
    var i: Integer;
  begin
    if gPlayers <> nil then
      for i := 0 to High(gPlayers) do
        if gPlayers[i] <> nil then
          r_Map_DrawPlayer(gPlayers[i], player);
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
    // TODO draw g_debug_frames
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
          r_Map_DrawPlayerModel(p.model, fX, fY, 255);
        end;
      end;
    end;
    // TODO draw g_debug_frames
  end;
{$ENDIF}

{$IFDEF ENABLE_GFX}
  function r_Map_GetGFXID (): Integer;
    var i: Integer;
  begin
    i := 0;
    if gfxlist <> nil then
    begin
      while (i < Length(gfxlist)) and (gfxlist[i].typ > 0) do
        Inc(i);
      if i >= Length(gfxlist) then
        SetLength(gfxlist, Length(gfxlist) + 1)
    end
    else
      SetLength(gfxlist, 1);
    gfxlist[i].typ := R_GFX_NONE;
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
        gfxlist[i].anim := GFXAnim[typ].anim;
        gfxlist[i].time := gTime DIV GAME_TICK;
        gfxlist[i].frame := 0;
        INC(gfxlist[i].anim.delay, Random(GFXAnim[typ].rdelay));
      end;
    end;
  end;

  procedure r_Map_UpdateGFX (tick: LongWord);
    var i: Integer; count: LongInt;
  begin
    if gfxlist <> nil then
    begin
      for i := 0 to High(gfxlist) do
      begin
        if (gfxlist[i].typ > 0) and (tick >= gfxlist[i].time) then
        begin
          g_Anim_GetFrameByTime(gfxlist[i].anim, tick - gfxlist[i].time, count, gfxlist[i].frame);
          if count < 1 then
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
          end
          else
            gfxlist[i].typ := R_GFX_NONE;
        end;
      end;
    end;
  end;

  procedure r_Map_DrawGFX (x, y, w, h: Integer);
    var i, fx, fy, typ: Integer; t: TGLMultiTexture; tex: TGLTexture;
  begin
    if gfxlist <> nil then
    begin
      for i := 0 to High(gfxlist) do
      begin
        if gfxlist[i].typ > 0 then
        begin
          typ := gfxlist[i].typ;
          t := GFXTextures[typ];
          if t <> nil then
          begin
            fx := nlerp(gfxlist[i].oldX, gfxlist[i].x, gLerpFactor);
            fy := nlerp(gfxlist[i].oldY, gfxlist[i].y, gLerpFactor);
            tex := t.GetTexture(gfxlist[i].frame);
            r_Draw_TextureRepeat(tex, fx, fy, tex.width, tex.height, false, 255, 255, 255, 255 - GFXAnim[typ].alpha, false);
          end;
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
{$ENDIF}

  procedure r_Map_DrawShots (x, y, w, h: Integer);
    var i, a, fX, fY, pX, pY, typ: Integer; count, frame: LongInt; t: TGLMultiTexture; tex: TGLTexture;
  begin
    if Shots <> nil then
    begin
      for i := 0 to High(Shots) do
      begin
        typ := Shots[i].ShotType;
        if typ <> 0 then
        begin
          t := ShotTextures[typ];
          if t <> nil then
          begin
            a := 0;
            case typ of
              WEAPON_ROCKETLAUNCHER, WEAPON_BARON_FIRE, WEAPON_MANCUB_FIRE, WEAPON_SKEL_FIRE:
                a := -GetAngle2(Shots[i].Obj.Vel.X, Shots[i].Obj.Vel.Y)
            end;
            Shots[i].Obj.Lerp(gLerpFactor, fX, fY);
            pX := Shots[i].Obj.Rect.Width div 2;
            pY := Shots[i].Obj.Rect.Height div 2;
            g_Anim_GetFrameByTime(ShotAnim[typ].anim, (gTime - Shots[i].time) DIV GAME_TICK, count, frame);
            tex := t.GetTexture(frame);
            r_Draw_TextureRepeatRotate(tex, fX, fY, tex.width, tex.height, false, 255, 255, 255, 255, false, pX, pY, a);
          end;
        end;
      end;
    end;
    // TODO draw g_debug_frames
  end;

  procedure r_Map_DrawFlags (x, y, w, h: Integer);
    var i, dx, fx, fy: Integer; flip: Boolean; t: TGLMultiTexture; tex: TGLTexture;
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
          t := FlagTextures[i];
          tex := t.GetTexture(FlagFrame);
          r_Draw_TextureRepeat(tex, fx + dx, fy + 1, tex.width, tex.height, flip, 255, 255, 255, 255, false)
        end;
      end;
    end;
    // TODO g_debug_frames
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

  procedure r_Map_CalcAspect (ow, oh, nw, nh: LongInt; horizontal: Boolean; out ww, hh: LongInt);
  begin
    if horizontal then
    begin
      ww := nw;
      hh := nw * oh div ow;
    end
    else
    begin
      ww := nh * ow div oh;
      hh := nh;
    end;
  end;

  procedure r_Map_CalcSkyParallax (cx, cy, vw, vh, sw, sh, mw, mh: LongInt; out x, y, w, h: LongInt);
    const
      factor = 120; (* size ratio between view and sky (120%) *)
      limit = 100;  (* max speed for parallax *)
    var
      msw, msh, mvw, mvh, svw, svh: LongInt;
  begin
    msw := vw * factor div 100;
    msh := vh * factor div 100;
    r_Map_CalcAspect(sw, sh, msw, msh, (sw / sh) <= (msw / msh), w, h);

    (* calc x parallax or sky center on speed limit *)
    mvw := MAX(1, mw - vw);
    svw := w - vw;
    if 100 * svw div mvw <= limit then
      x := -cx * svw div mvw
    else
      x := -svw div 2;

    (* calc y parallax or sky center on speed limit *)
    mvh := MAX(1, mh - vh);
    svh := h - vh;
    if 100 * svh div mvh <= limit then
      y := -cy * svh div mvh
    else
      y := -svh div 2;

    (* handle out of map bounds *)
    if x > 0 then x := 0;
    if y > 0 then y := 0;
    if x < -svw then x := -svw;
    if y < -svh then y := -svh;
  end;

  procedure r_Map_DrawScreenEffect (x, y, w, h, level: Integer; r, g, b: Byte);
    var i: Integer;
  begin
    if level > 0 then
    begin
      case level of
        0..14: i := 0;
        15..34: i := 1;
        35..54: i := 2;
        55..74: i := 3;
        75..94: i := 4;
      else i := 5
      end;
      r_Draw_FillRect(x, y, x + w, y + h, r, g, b, i * 50)
    end;
  end;

  procedure r_Map_DrawScreenEffects (x, y, w, h: Integer; p: TPlayer);
  begin
    if p <> nil then
    begin
      r_Map_DrawScreenEffect(x, y, w, h, p.pain, 255, 0, 0);
      r_Map_DrawScreenEffect(x, y, w, h, p.pickup, 150, 200, 150);
      if (p.FMegaRulez[MR_INVUL] >= gTime) and (p.SpawnInvul < gTime) then
      begin
        if ((p.FMegaRulez[MR_INVUL] - gTime) > 2100) or not ODD((p.FMegaRulez[MR_INVUL] - gTime) div 300) then
          r_Draw_InvertRect(x, y, x + w, y + h, 191, 191, 191, 255);
      end;
      if p.FMegaRulez[MR_SUIT] >= gTime then
      begin
        if ((p.FMegaRulez[MR_SUIT] - gTime) > 2100) or not ODD((p.FMegaRulez[MR_SUIT] - gTime) div 300) then
          r_Draw_FillRect(x, y, x + w, y + h, 0, 96, 0, 55);
      end;
      if (p.Berserk >= 0) and (p.Berserk >= gTime) and (gFlash = 2) then
      begin
        r_Draw_FillRect(x, y, x + w, y + h, 255, 0, 0, 55);
      end;
    end;
  end;

  procedure r_Map_Draw (x, y, w, h, camx, camy: Integer; player: TPlayer);
    var iter: TPanelGrid.Iter; p: PPanel; cx, cy, xx, yy, ww, hh: Integer; sx, sy, sw, sh: LongInt;
  begin
    cx := camx - w div 2;
    cy := camy - h div 2;
    xx := x + cx;
    yy := y + cy;
    ww := w;
    hh := h;

    if g_dbg_ignore_bounds = false then
    begin
      if xx + ww > gMapInfo.Width then
        xx := gMapInfo.Width - ww;
      if yy + hh > gMapInfo.Height then
        yy := gMapInfo.Height - hh;
      if xx < 0 then
        xx := 0;
      if yy < 0 then
        yy := 0;
      cx := xx - x;
      cy := yy - y;
    end;

    if SkyTexture <> nil then
    begin
      r_Map_CalcSkyParallax(cx, cy, ww, hh, SkyTexture.width, SkyTexture.height, gMapInfo.Width, gMapInfo.Height, sx, sy, sw, sh);
      r_Draw_Texture(SkyTexture, x + sx, y + sy, sw, sh, false, 255, 255, 255, 255, false);
    end;

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
    r_Map_DrawPlayers(xx, yy, ww, hh, player);
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
    // TODO draw monsters health bar
    // TODO draw players health bar
    // TODO draw players indicators
    glPopMatrix;

    r_Map_DrawScreenEffects(x, y, w, h, player);

    // TODO draw minimap (gShowMap)
    // TODO draw g_debug_player
  end;

  procedure r_Map_Update;
    var i, count, tick: LongInt;
  begin
    tick := gTime div GAME_TICK;
    for i := 0 to ITEM_LAST do
      g_Anim_GetFrameByTime(ItemAnim[i].anim, tick, count, Items[i].frame);
    r_Map_UpdateGFX(tick);
    g_Anim_GetFrameByTime(FlagAnim, tick, count, FlagFrame);
  end;

end.
