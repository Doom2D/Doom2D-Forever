unit g_items;

interface

uses
  Windows, g_textures, g_phys, g_saveload, BinEditor;

Type
  TItem = record
    ItemType:      Byte;
    Respawnable:   Boolean;
    InitX, InitY:  Integer;
    RespawnTime:   Word;
    Live:          Boolean;
    Fall:          Boolean;
    Obj:           TObj;
    Animation:     TAnimation;
  end;

procedure g_Items_LoadData();
procedure g_Items_FreeData();
procedure g_Items_Init();
procedure g_Items_Free();
function g_Items_Create(X, Y: Integer; ItemType: Byte;
           Fall, Respawnable: Boolean; AdjCoord: Boolean = False; ForcedID: Integer = -1): DWORD;
procedure g_Items_Update();
procedure g_Items_Draw();
procedure g_Items_Pick(ID: DWORD);
procedure g_Items_Remove(ID: DWORD);
procedure g_Items_SaveState(var Mem: TBinMemoryWriter);
procedure g_Items_LoadState(var Mem: TBinMemoryReader);

var
  gItems: Array of TItem = nil;
  gItemsTexturesID: Array [1..32] of DWORD;
  gMaxDist: Integer = 1;
  ITEM_RESPAWNTIME: Integer = 60 * 36;

implementation

uses
  g_basic, e_graphics, g_sound, g_main, g_gfx, g_map,
  Math, g_game, g_console, SysUtils, g_player, g_net, g_netmsg, MAPDEF,
  e_log;

const
  ITEM_SIGNATURE = $4D455449; // 'ITEM'

  ITEMSIZE: Array [ITEM_MEDKIT_SMALL..ITEM_HELMET] of Array [0..1] of Byte =
    (((14), (15)), // MEDKIT_SMALL
     ((28), (19)), // MEDKIT_LARGE
     ((28), (19)), // MEDKIT_BLACK
     ((31), (16)), // ARMOR_GREEN
     ((31), (16)), // ARMOR_BLUE
     ((25), (25)), // SPHERE_BLUE
     ((25), (25)), // SPHERE_WHITE
     ((24), (47)), // SUIT
     ((14), (27)), // OXYGEN
     ((25), (25)), // INV
     ((62), (24)), // WEAPON_SAW
     ((63), (12)), // WEAPON_SHOTGUN1
     ((54), (13)), // WEAPON_SHOTGUN2
     ((54), (16)), // WEAPON_CHAINGUN
     ((62), (16)), // WEAPON_ROCKETLAUNCHER
     ((54), (16)), // WEAPON_PLASMA
     ((61), (36)), // WEAPON_BFG
     ((54), (16)), // WEAPON_SUPERPULEMET
     (( 9), (11)), // AMMO_BULLETS
     ((28), (16)), // AMMO_BULLETS_BOX
     ((15), ( 7)), // AMMO_SHELLS
     ((32), (12)), // AMMO_SHELLS_BOX
     ((12), (27)), // AMMO_ROCKET
     ((54), (21)), // AMMO_ROCKET_BOX
     ((15), (12)), // AMMO_CELL
     ((32), (21)), // AMMO_CELL_BIG
     ((22), (29)), // AMMO_BACKPACK
     ((16), (16)), // KEY_RED
     ((16), (16)), // KEY_GREEN
     ((16), (16)), // KEY_BLUE
     (( 1), ( 1)), // WEAPON_KASTET
     ((43), (16)), // WEAPON_PISTOL
     ((14), (18)), // BOTTLE
     ((16), (15))); // HELMET

procedure InitTextures();
begin
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
  g_Texture_Get('ITEM_AMMO_BULLETS',     gItemsTexturesID[ITEM_AMMO_BULLETS]);
  g_Texture_Get('ITEM_AMMO_BULLETS_BOX', gItemsTexturesID[ITEM_AMMO_BULLETS_BOX]);
  g_Texture_Get('ITEM_AMMO_SHELLS',      gItemsTexturesID[ITEM_AMMO_SHELLS]);
  g_Texture_Get('ITEM_AMMO_SHELLS_BOX',  gItemsTexturesID[ITEM_AMMO_SHELLS_BOX]);
  g_Texture_Get('ITEM_AMMO_ROCKET',      gItemsTexturesID[ITEM_AMMO_ROCKET]);
  g_Texture_Get('ITEM_AMMO_ROCKET_BOX',  gItemsTexturesID[ITEM_AMMO_ROCKET_BOX]);
  g_Texture_Get('ITEM_AMMO_CELL',        gItemsTexturesID[ITEM_AMMO_CELL]);
  g_Texture_Get('ITEM_AMMO_CELL_BIG',    gItemsTexturesID[ITEM_AMMO_CELL_BIG]);
  g_Texture_Get('ITEM_AMMO_BACKPACK',    gItemsTexturesID[ITEM_AMMO_BACKPACK]);
  g_Texture_Get('ITEM_KEY_RED',          gItemsTexturesID[ITEM_KEY_RED]);
  g_Texture_Get('ITEM_KEY_GREEN',        gItemsTexturesID[ITEM_KEY_GREEN]);
  g_Texture_Get('ITEM_KEY_BLUE',         gItemsTexturesID[ITEM_KEY_BLUE]);
  g_Texture_Get('ITEM_WEAPON_KASTET',    gItemsTexturesID[ITEM_WEAPON_KASTET]);
  g_Texture_Get('ITEM_WEAPON_PISTOL',    gItemsTexturesID[ITEM_WEAPON_PISTOL]);
end;

procedure g_Items_LoadData();
begin
  e_WriteLog('Loading items data...', MSG_NOTIFY);

  g_Sound_CreateWADEx('SOUND_ITEM_RESPAWNITEM', GameWAD+':SOUNDS\RESPAWNITEM');
  g_Sound_CreateWADEx('SOUND_ITEM_GETRULEZ', GameWAD+':SOUNDS\GETRULEZ');
  g_Sound_CreateWADEx('SOUND_ITEM_GETITEM', GameWAD+':SOUNDS\GETITEM');
  g_Sound_CreateWADEx('SOUND_ITEM_GETMED', GameWAD+':SOUNDS\GETMED');

  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_BLUESPHERE', GameWAD+':TEXTURES\SBLUE', 32, 32, 4, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_WHITESPHERE', GameWAD+':TEXTURES\SWHITE', 32, 32, 4, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_ARMORGREEN', GameWAD+':TEXTURES\ARMORGREEN', 32, 16, 3, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_ARMORBLUE', GameWAD+':TEXTURES\ARMORBLUE', 32, 16, 3, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_INV', GameWAD+':TEXTURES\INV', 32, 32, 4, True);
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
  g_Texture_CreateWADEx('ITEM_AMMO_BULLETS', GameWAD+':TEXTURES\CLIP');
  g_Texture_CreateWADEx('ITEM_AMMO_BULLETS_BOX', GameWAD+':TEXTURES\AMMO');
  g_Texture_CreateWADEx('ITEM_AMMO_SHELLS', GameWAD+':TEXTURES\SHELL1');
  g_Texture_CreateWADEx('ITEM_AMMO_SHELLS_BOX', GameWAD+':TEXTURES\SHELL2');
  g_Texture_CreateWADEx('ITEM_AMMO_ROCKET', GameWAD+':TEXTURES\ROCKET');
  g_Texture_CreateWADEx('ITEM_AMMO_ROCKET_BOX', GameWAD+':TEXTURES\ROCKETS');
  g_Texture_CreateWADEx('ITEM_AMMO_CELL', GameWAD+':TEXTURES\CELL');
  g_Texture_CreateWADEx('ITEM_AMMO_CELL_BIG', GameWAD+':TEXTURES\CELL2');
  g_Texture_CreateWADEx('ITEM_AMMO_BACKPACK', GameWAD+':TEXTURES\BPACK');
  g_Texture_CreateWADEx('ITEM_KEY_RED', GameWAD+':TEXTURES\KEYR');
  g_Texture_CreateWADEx('ITEM_KEY_GREEN', GameWAD+':TEXTURES\KEYG');
  g_Texture_CreateWADEx('ITEM_KEY_BLUE', GameWAD+':TEXTURES\KEYB');
  g_Texture_CreateWADEx('ITEM_OXYGEN', GameWAD+':TEXTURES\OXYGEN');
  g_Texture_CreateWADEx('ITEM_SUIT', GameWAD+':TEXTURES\SUIT');
  g_Texture_CreateWADEx('ITEM_WEAPON_KASTET', GameWAD+':TEXTURES\KASTET');
  g_Texture_CreateWADEx('ITEM_MEDKIT_BLACK', GameWAD+':TEXTURES\BMED');

  InitTextures();
end;

procedure g_Items_FreeData();
begin
  e_WriteLog('Releasing items data...', MSG_NOTIFY);

  g_Sound_Delete('SOUND_ITEM_RESPAWNITEM');
  g_Sound_Delete('SOUND_ITEM_GETRULEZ');
  g_Sound_Delete('SOUND_ITEM_GETITEM');
  g_Sound_Delete('SOUND_ITEM_GETMED');

  g_Frames_DeleteByName('FRAMES_ITEM_BLUESPHERE');
  g_Frames_DeleteByName('FRAMES_ITEM_WHITESPHERE');
  g_Frames_DeleteByName('FRAMES_ITEM_ARMORGREEN');
  g_Frames_DeleteByName('FRAMES_ITEM_ARMORBLUE');
  g_Frames_DeleteByName('FRAMES_ITEM_INV');
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
  g_Texture_Delete('ITEM_AMMO_BULLETS');
  g_Texture_Delete('ITEM_AMMO_BULLETS_BOX');
  g_Texture_Delete('ITEM_AMMO_SHELLS');
  g_Texture_Delete('ITEM_AMMO_SHELLS_BOX');
  g_Texture_Delete('ITEM_AMMO_ROCKET');
  g_Texture_Delete('ITEM_AMMO_ROCKET_BOX');
  g_Texture_Delete('ITEM_AMMO_CELL');
  g_Texture_Delete('ITEM_AMMO_CELL_BIG');
  g_Texture_Delete('ITEM_AMMO_BACKPACK');
  g_Texture_Delete('ITEM_KEY_RED');
  g_Texture_Delete('ITEM_KEY_GREEN');
  g_Texture_Delete('ITEM_KEY_BLUE');
  g_Texture_Delete('ITEM_OXYGEN');
  g_Texture_Delete('ITEM_SUIT');
  g_Texture_Delete('ITEM_WEAPON_KASTET');
  g_Texture_Delete('ITEM_MEDKIT_BLACK');
end;

function FindItem(): DWORD;
var
  i: Integer;
begin
  if gItems <> nil then
    for i := 0 to High(gItems) do
      if gItems[i].ItemType = ITEM_NONE then
      begin
        Result := i;
        Exit;
      end;

  if gItems = nil then
    begin
      SetLength(gItems, 32);
      Result := 0;
    end
  else
    begin
      Result := High(gItems) + 1;
      SetLength(gItems, Length(gItems) + 32);
    end;
end;

procedure g_Items_Init();
var
  a, b: Integer;
begin
  if gMapInfo.Height > gPlayerScreenSize.Y then
    a := gMapInfo.Height - gPlayerScreenSize.Y
  else
    a := gMapInfo.Height;

  if gMapInfo.Width > gPlayerScreenSize.X then
    b := gMapInfo.Width - gPlayerScreenSize.X
  else
    b := gMapInfo.Width;

  gMaxDist := Trunc(Hypot(a, b));
end;

procedure g_Items_Free();
var
  i: Integer;
begin
  if gItems <> nil then
  begin
    for i := 0 to High(gItems) do
      gItems[i].Animation.Free();
    gItems := nil;
  end;
end;

function g_Items_Create(X, Y: Integer; ItemType: Byte;
           Fall, Respawnable: Boolean; AdjCoord: Boolean = False; ForcedID: Integer = -1): DWORD;
var
  find_id: DWORD;
  ID: DWORD;
begin
  if ForcedID < 0 then
    find_id := FindItem()
  else
  begin
    find_id := ForcedID;

    if find_id > DWORD(High(gItems)) then
      SetLength(gItems, find_id + 32);
  end;

  gItems[find_id].ItemType := ItemType;
  gItems[find_id].Respawnable := Respawnable;
  if g_Game_IsServer and (ITEM_RESPAWNTIME = 0) then
    gItems[find_id].Respawnable := False;
  gItems[find_id].InitX := X;
  gItems[find_id].InitY := Y;
  gItems[find_id].RespawnTime := 0;
  gItems[find_id].Fall := Fall;
  gItems[find_id].Live := True;

  g_Obj_Init(@gItems[find_id].Obj);
  gItems[find_id].Obj.X := X;
  gItems[find_id].Obj.Y := Y;
  gItems[find_id].Obj.Rect.Width := ITEMSIZE[ItemType][0];
  gItems[find_id].Obj.Rect.Height := ITEMSIZE[ItemType][1];

  gItems[find_id].Animation := nil;

// Координаты относительно центра нижнего ребра:
  if AdjCoord then
    with gItems[find_id] do
    begin
      Obj.X := X - (Obj.Rect.Width div 2);
      Obj.Y := Y - Obj.Rect.Height;
      InitX := Obj.X;
      InitY := Obj.Y;
    end;

// Установка анимации:
  with gItems[find_id] do
  begin
    case ItemType of
      ITEM_ARMOR_GREEN:
        if g_Frames_Get(ID, 'FRAMES_ITEM_ARMORGREEN') then
          Animation := TAnimation.Create(ID, True, 20);
      ITEM_ARMOR_BLUE:
        if g_Frames_Get(ID, 'FRAMES_ITEM_ARMORBLUE') then
          Animation := TAnimation.Create(ID, True, 20);
      ITEM_SPHERE_BLUE:
        if g_Frames_Get(ID, 'FRAMES_ITEM_BLUESPHERE') then
          Animation := TAnimation.Create(ID, True, 15);
      ITEM_SPHERE_WHITE:
        if g_Frames_Get(ID, 'FRAMES_ITEM_WHITESPHERE') then
          Animation := TAnimation.Create(ID, True, 20);
      ITEM_INV:
        if g_Frames_Get(ID, 'FRAMES_ITEM_INV') then
          Animation := TAnimation.Create(ID, True, 20);
      ITEM_BOTTLE:
        if g_Frames_Get(ID, 'FRAMES_ITEM_BOTTLE') then
          Animation := TAnimation.Create(ID, True, 20);
      ITEM_HELMET:
        if g_Frames_Get(ID, 'FRAMES_ITEM_HELMET') then
          Animation := TAnimation.Create(ID, True, 20);
    end;
  end;

  Result := find_id;
end;

procedure g_Items_Update();
var
  i, j, k: Integer;
  ID: DWORD;
  Anim: TAnimation;
  m: Word;
  r, nxt: Boolean;
begin
  if gItems <> nil then
    for i := 0 to High(gItems) do
      if gItems[i].ItemType <> ITEM_NONE then
        with gItems[i] do
        begin
          nxt := False;
          
          if Live then
          begin
            if Fall then
            begin
              m := g_Obj_Move(@Obj, True, True);
                
            // Сопротивление воздуха:
              if gTime mod (GAME_TICK*2) = 0 then
                Obj.Vel.X := z_dec(Obj.Vel.X, 1);
              if WordBool(m and MOVE_FALLOUT) then
              begin
                g_Items_Pick(i);
                Continue;
              end;
            end;

          // Если игроки поблизости:
            if gPlayers <> nil then
            begin
              j := Random(Length(gPlayers)) - 1;

              for k := 0 to High(gPlayers) do
              begin
                Inc(j);
                if j > High(gPlayers) then
                  j := 0;

                if (gPlayers[j] <> nil) and gPlayers[j].Live and
                   g_Obj_Collide(@gPlayers[j].Obj, @Obj) then
                begin
                  if g_Game_IsClient then Continue;

                  if not gPlayers[j].PickItem(ItemType, Respawnable, r) then
                    Continue;

                  if g_Game_IsNet then MH_SEND_PlayerStats(gPlayers[j].UID);

{
  Doom 2D: Original:
  1. I_NONE,I_CLIP,I_SHEL,I_ROCKET,I_CELL,I_AMMO,I_SBOX,I_RBOX,I_CELP,I_BPACK,I_CSAW,I_SGUN,I_SGUN2,I_MGUN,I_LAUN,I_PLAS,I_BFG,I_GUN2
  +2. I_MEGA,I_INVL,I_SUPER
  3. I_STIM,I_MEDI,I_ARM1,I_ARM2,I_AQUA,I_KEYR,I_KEYG,I_KEYB,I_SUIT,I_RTORCH,I_GTORCH,I_BTORCH,I_GOR1,I_FCAN
}

                  if ItemType in [ITEM_SPHERE_BLUE, ITEM_SPHERE_WHITE, ITEM_INV] then
                    g_Sound_PlayExAt('SOUND_ITEM_GETRULEZ',
                      gPlayers[j].Obj.X, gPlayers[j].Obj.Y)
                  else
                    if ItemType in [ITEM_MEDKIT_SMALL, ITEM_MEDKIT_LARGE,
                                    ITEM_MEDKIT_BLACK, ITEM_BOTTLE, ITEM_HELMET, ITEM_ARMOR_GREEN,
                                    ITEM_ARMOR_BLUE, ITEM_KEY_RED, ITEM_KEY_GREEN, ITEM_KEY_BLUE] then
                      g_Sound_PlayExAt('SOUND_ITEM_GETMED',
                        gPlayers[j].Obj.X, gPlayers[j].Obj.Y)
                    else
                      g_Sound_PlayExAt('SOUND_ITEM_GETITEM',
                        gPlayers[j].Obj.X, gPlayers[j].Obj.Y);

                // Надо убрать с карты, если это не ключ, которым нужно поделиться с другим игроком:
                  if r then
                  begin
                    if not Respawnable then
                      g_Items_Remove(i)
                    else
                      g_Items_Pick(i);

                    if g_Game_IsNet then MH_SEND_ItemDestroy(False, i);
                    nxt := True;
                    Break;
                  end;
                end;
              end;
            end;

            if nxt then
              Continue;
          end;

          if Respawnable and g_Game_IsServer then
          begin
            DecMin(RespawnTime, 0);
            if (RespawnTime = 0) and (not Live) then
            begin
              g_Sound_PlayExAt('SOUND_ITEM_RESPAWNITEM', InitX, InitY);

              if g_Frames_Get(ID, 'FRAMES_ITEM_RESPAWN') then
              begin
                Anim := TAnimation.Create(ID, False, 4);
                g_GFX_OnceAnim(InitX+(Obj.Rect.Width div 2)-16, InitY+(Obj.Rect.Height div 2)-16, Anim);
                Anim.Free();
              end;

              Obj.X := InitX;
              Obj.Y := InitY;
              Obj.Vel.X := 0;
              Obj.Vel.Y := 0;
              Obj.Accel.X := 0;
              Obj.Accel.Y := 0;

              Live := True;

              if g_Game_IsNet then MH_SEND_ItemSpawn(False, i);
            end;
          end;

          if Animation <> nil then
            Animation.Update();
        end;
end;

procedure g_Items_Draw();
var
  i: Integer;
begin
  if gItems <> nil then
    for i := 0 to High(gItems) do
      if gItems[i].Live then
        with gItems[i] do
          if g_Collide(Obj.X, Obj.Y, Obj.Rect.Width, Obj.Rect.Height,
                       sX, sY, sWidth, sHeight) then
          begin
            if Animation = nil then
              e_Draw(gItemsTexturesID[ItemType], Obj.X, Obj.Y, 0, True, False)
            else
              Animation.Draw(Obj.X, Obj.Y, M_NONE);

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

procedure g_Items_Pick(ID: DWORD);
begin
  gItems[ID].Live := False;
  gItems[ID].RespawnTime := ITEM_RESPAWNTIME;
end;

procedure g_Items_Remove(ID: DWORD);
begin
  gItems[ID].ItemType := ITEM_NONE;

  if gItems[ID].Animation <> nil then
  begin
    gItems[ID].Animation.Free();
    gItems[ID].Animation := nil;
  end;

  gItems[ID].Live := False;
end;

procedure g_Items_SaveState(var Mem: TBinMemoryWriter);
var
  count, i: Integer;
  sig: DWORD;
begin
// Считаем количество существующих предметов:
  count := 0;
  if gItems <> nil then
    for i := 0 to High(gItems) do
      if gItems[i].ItemType <> ITEM_NONE then
        count := count + 1;

  Mem := TBinMemoryWriter.Create((count+1) * 60);

// Количество предметов:
  Mem.WriteInt(count);

  if count = 0 then
    Exit;

  for i := 0 to High(gItems) do
    if gItems[i].ItemType <> ITEM_NONE then
    begin
    // Сигнатура предмета:
      sig := ITEM_SIGNATURE; // 'ITEM'
      Mem.WriteDWORD(sig);
    // Тип предмета:
      Mem.WriteByte(gItems[i].ItemType);
    // Есть ли респаун:
      Mem.WriteBoolean(gItems[i].Respawnable);
    // Координаты респуна:
      Mem.WriteInt(gItems[i].InitX);
      Mem.WriteInt(gItems[i].InitY);
    // Время до респауна:
      Mem.WriteWord(gItems[i].RespawnTime);
    // Существует ли этот предмет:
      Mem.WriteBoolean(gItems[i].Live);
    // Может ли он падать:
      Mem.WriteBoolean(gItems[i].Fall);
    // Объект предмета:
      Obj_SaveState(@gItems[i].Obj, Mem);
    end;
end;

procedure g_Items_LoadState(var Mem: TBinMemoryReader);
var
  count, i, a: Integer;
  sig: DWORD;
  b: Byte;
begin
  if Mem = nil then
    Exit;

  g_Items_Free();

// Количество предметов:
  Mem.ReadInt(count);

  if count = 0 then
    Exit;

  for a := 0 to count-1 do
  begin
  // Сигнатура предмета:
    Mem.ReadDWORD(sig);
    if sig <> ITEM_SIGNATURE then // 'ITEM'
    begin
      raise EBinSizeError.Create('g_Items_LoadState: Wrong Item Signature');
    end;
  // Тип предмета:
    Mem.ReadByte(b);
  // Создаем предмет:
    i := g_Items_Create(0, 0, b, False, False);
  // Есть ли респаун:
    Mem.ReadBoolean(gItems[i].Respawnable);
  // Координаты респуна:
    Mem.ReadInt(gItems[i].InitX);
    Mem.ReadInt(gItems[i].InitY);
  // Время до респауна:
    Mem.ReadWord(gItems[i].RespawnTime);
  // Существует ли этот предмет:
    Mem.ReadBoolean(gItems[i].Live);
  // Может ли он падать:
    Mem.ReadBoolean(gItems[i].Fall);
  // Объект предмета:
    Obj_LoadState(@gItems[i].Obj, Mem);
  end;
end;

end.
