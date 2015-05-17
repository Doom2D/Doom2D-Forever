unit g_weapons;

interface

uses
  windows, g_textures, g_basic, e_graphics, g_phys, BinEditor;

const
  HIT_SOME    = 0;
  HIT_ROCKET  = 1;
  HIT_BFG     = 2;
  HIT_TRAP    = 3;
  HIT_FALL    = 4;
  HIT_WATER   = 5;
  HIT_ACID    = 6;
  HIT_ELECTRO = 7;
  HIT_FLAME   = 8;

type
  TShot = record
    ShotType: Byte;
    Target: Word;
    SpawnerUID: Word;
    Triggers: DWArray;
    Obj: TObj;
    Animation: TAnimation;
    TextureID: DWORD;
    Timeout: DWORD;
  end;

var
  Shots: array of TShot = nil;
  LastShotID: Integer = 0;

procedure g_Weapon_LoadData();
procedure g_Weapon_FreeData();
procedure g_Weapon_Init();
procedure g_Weapon_Free();
function g_Weapon_Hit(obj: PObj; d: Integer; SpawnerUID: Word; t: Byte): Byte;
function g_Weapon_HitUID(UID: Word; d: Integer; SpawnerUID: Word; t: Byte): Boolean;
function g_Weapon_CreateShot(I: Integer; ShotType: Byte; Spawner, Target: Word; X, Y, XV, YV: Integer): LongWord;

procedure g_Weapon_gun(x, y, xd, yd, v: Integer; SpawnerUID: Word; CheckTrigger: Boolean);
procedure g_Weapon_punch(x, y: Integer; d, SpawnerUID: Word);
function g_Weapon_chainsaw(x, y: Integer; d, SpawnerUID: Word): Integer;
procedure g_Weapon_rocket(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
procedure g_Weapon_revf(x, y, xd, yd: Integer; SpawnerUID, TargetUID: Word; WID: Integer = -1);
procedure g_Weapon_plasma(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
procedure g_Weapon_ball1(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
procedure g_Weapon_ball2(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
procedure g_Weapon_ball7(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
procedure g_Weapon_aplasma(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
procedure g_Weapon_manfire(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
procedure g_Weapon_bfgshot(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
procedure g_Weapon_bfghit(x, y: Integer);
procedure g_Weapon_pistol(x, y, xd, yd: Integer; SpawnerUID: Word);
procedure g_Weapon_mgun(x, y, xd, yd: Integer; SpawnerUID: Word);
procedure g_Weapon_shotgun(x, y, xd, yd: Integer; SpawnerUID: Word);
procedure g_Weapon_dshotgun(x, y, xd, yd: Integer; SpawnerUID: Word);

function g_Weapon_Explode(X, Y: Integer; rad: Integer; SpawnerUID: Word): Boolean;
procedure g_Weapon_Update();
procedure g_Weapon_Draw();
function g_Weapon_Danger(UID: Word; X, Y: Integer; Width, Height: Word; Time: Byte): Boolean;
procedure g_Weapon_DestroyShot(I: Integer; X, Y: Integer; Loud: Boolean = True);

procedure g_Weapon_SaveState(var Mem: TBinMemoryWriter);
procedure g_Weapon_LoadState(var Mem: TBinMemoryReader);

const
  WEAPON_KASTET         = 0;
  WEAPON_SAW            = 1;
  WEAPON_PISTOL         = 2;
  WEAPON_SHOTGUN1       = 3;
  WEAPON_SHOTGUN2       = 4;
  WEAPON_CHAINGUN       = 5;
  WEAPON_ROCKETLAUNCHER = 6;
  WEAPON_PLASMA         = 7;
  WEAPON_BFG            = 8;
  WEAPON_SUPERPULEMET   = 9;
  WEAPON_MEGAKASTET     = 10;
  WEAPON_ZOMBY_PISTOL   = 20;
  WEAPON_IMP_FIRE       = 21;
  WEAPON_BSP_FIRE       = 22;
  WEAPON_CACO_FIRE      = 23;
  WEAPON_BARON_FIRE     = 24;
  WEAPON_MANCUB_FIRE    = 25;
  WEAPON_SKEL_FIRE      = 26;

implementation

uses
  Math, g_map, g_player, g_gfx, g_sound, g_main,
  g_console, SysUtils, g_options, g_game,
  g_triggers, MAPDEF, e_log, g_monsters, g_saveload,
  g_language, g_netmsg;

type
  TWaterPanel = record
    X, Y: Integer;
    Width, Height: Word;
    Active: Boolean;
  end;

const
  SHOT_ROCKETLAUNCHER_WIDTH = 27;
  SHOT_ROCKETLAUNCHER_HEIGHT = 12;

  SHOT_SKELFIRE_WIDTH = 32;
  SHOT_SKELFIRE_HEIGHT = 16;

  SHOT_PLASMA_WIDTH = 16;
  SHOT_PLASMA_HEIGHT = 16;

  SHOT_BFG_WIDTH = 32;
  SHOT_BFG_HEIGHT = 32;
  SHOT_BFG_DAMAGE = 100;
  SHOT_BFG_RADIUS = 256;

  SHOT_SIGNATURE = $544F4853; // 'SHOT'

var
  WaterMap: array of array of DWORD = nil;

function FindShot(): DWORD;
var
  i: Integer;
begin
  if Shots <> nil then
  for i := 0 to High(Shots) do
    if Shots[i].ShotType = 0 then
    begin
      Result := i;
      LastShotID := Result;
      Exit;
    end;

  if Shots = nil then
  begin
    SetLength(Shots, 128);
    Result := 0;
  end
  else
  begin
    Result := High(Shots) + 1;
    SetLength(Shots, Length(Shots) + 128);
  end;
  LastShotID := Result;
end;

procedure CreateWaterMap();
var
  WaterArray: Array of TWaterPanel;
  a, b, c, m: Integer;
  ok: Boolean;
begin
  if gWater = nil then
    Exit;

  SetLength(WaterArray, Length(gWater));

  for a := 0 to High(gWater) do
  begin
    WaterArray[a].X := gWater[a].X;
    WaterArray[a].Y := gWater[a].Y;
    WaterArray[a].Width := gWater[a].Width;
    WaterArray[a].Height := gWater[a].Height;
    WaterArray[a].Active := True;
  end;

  g_Game_SetLoadingText(_lc[I_LOAD_WATER_MAP], High(WaterArray), False);

  for a := 0 to High(WaterArray) do
    if WaterArray[a].Active then
    begin
      WaterArray[a].Active := False;
      m := Length(WaterMap);
      SetLength(WaterMap, m+1);
      SetLength(WaterMap[m], 1);
      WaterMap[m][0] := a;
      ok := True;

      while ok do
      begin
        ok := False;
        for b := 0 to High(WaterArray) do
          if WaterArray[b].Active then
            for c := 0 to High(WaterMap[m]) do
              if g_CollideAround(WaterArray[b].X,
                                 WaterArray[b].Y,
                                 WaterArray[b].Width,
                                 WaterArray[b].Height,
                                 WaterArray[WaterMap[m][c]].X,
                                 WaterArray[WaterMap[m][c]].Y,
                                 WaterArray[WaterMap[m][c]].Width,
                                 WaterArray[WaterMap[m][c]].Height) then
              begin
                WaterArray[b].Active := False;
                SetLength(WaterMap[m],
                          Length(WaterMap[m])+1);
                WaterMap[m][High(WaterMap[m])] := b;
                ok := True;
                Break;
              end;
      end;

      g_Game_StepLoading();
    end;

  WaterArray := nil;
end;

procedure CheckTrap(ID: DWORD; dm: Integer; t: Byte);
var
  a, b, c, d, i1, i2: Integer;
  pl, mn: WArray;
begin
  if (gWater = nil) or (WaterMap = nil) then Exit;

  i1 := -1;
  i2 := -1;

  SetLength(pl, 1024);
  SetLength(mn, 1024);
  for d := 0 to 1023 do pl[d] := $FFFF;
  for d := 0 to 1023 do mn[d] := $FFFF;

  for a := 0 to High(WaterMap) do
    for b := 0 to High(WaterMap[a]) do
    begin
      if not g_Obj_Collide(gWater[WaterMap[a][b]].X, gWater[WaterMap[a][b]].Y,
                           gWater[WaterMap[a][b]].Width, gWater[WaterMap[a][b]].Height,
                           @Shots[ID].Obj) then Continue;

      for c := 0 to High(WaterMap[a]) do
      begin
        if gPlayers <> nil then
        begin
          for d := 0 to High(gPlayers) do
            if (gPlayers[d] <> nil) and (gPlayers[d].Live) then
              if gPlayers[d].Collide(gWater[WaterMap[a][c]]) then
                if not InWArray(d, pl) then
                  if i1 < 1023 then
                  begin
                    i1 := i1+1;
                    pl[i1] := d;
                  end;
        end;

        if gMonsters <> nil then
        begin
          for d := 0 to High(gMonsters) do
            if (gMonsters[d] <> nil) and (gMonsters[d].Live) then
              if gMonsters[d].Collide(gWater[WaterMap[a][c]]) then
                if not InWArray(d, mn) then
                  if i2 < 1023 then
                  begin
                    i2 := i2+1;
                    mn[i2] := d;
                  end;
        end;
      end;

      if i1 <> -1 then
        for d := 0 to i1 do
          gPlayers[pl[d]].Damage(dm, Shots[ID].SpawnerUID, 0, 0, t);

      if i2 <> -1 then
        for d := 0 to i2 do
          gMonsters[mn[d]].Damage(dm, 0, 0, Shots[ID].SpawnerUID, t);
    end;

  pl := nil;
  mn := nil;
end;

function HitMonster(m: TMonster; d: Integer; vx, vy: Integer; SpawnerUID: Word; t: Byte): Boolean;
var
  tt, mt: Byte;
  mon: TMonster;
begin
  Result := False;

  tt := g_GetUIDType(SpawnerUID);
  if tt = UID_MONSTER then
  begin
    mon := g_Monsters_Get(SpawnerUID);
    if mon <> nil then
      mt := g_Monsters_Get(SpawnerUID).MonsterType
    else
      mt := 0;
  end
  else
    mt := 0;

  if m = nil then Exit;
  if m.UID = SpawnerUID then
  begin
  // Сам себя может ранить только ракетой и током:
    if (t <> HIT_ROCKET) and (t <> HIT_ELECTRO) then
      Exit;
  // Кибер демон и бочка вообще не могут себя ранить:
    if (m.MonsterType = MONSTER_CYBER) or
       (m.MonsterType = MONSTER_BARREL) then
    begin
      Result := True;
      Exit;
    end;
  end;

  if tt = UID_MONSTER then
  begin
  // Lost_Soul не может ранить Pain_Elemental'а:
    if (mt = MONSTER_SOUL) and (m.MonsterType = MONSTER_PAIN) then
      Exit;

  // Оба монстра одного вида:
    if mt = m.MonsterType then
      case mt of
        MONSTER_IMP, MONSTER_DEMON, MONSTER_BARON, MONSTER_KNIGHT, MONSTER_CACO,
        MONSTER_SOUL, MONSTER_MANCUB, MONSTER_SKEL, MONSTER_FISH:
          Exit; // Эти не бьют своих
      end;
  end;

  if g_Game_IsServer then
    Result := m.Damage(d, vx, vy, SpawnerUID, t)
  else
    Result := True;
end;

function HitPlayer(p: TPlayer; d: Integer; vx, vy: Integer; SpawnerUID: Word; t: Byte): Boolean;
begin
  Result := False;

// Сам себя может ранить только ракетой и током:
  if (p.UID = SpawnerUID) and (t <> HIT_ROCKET) and (t <> HIT_ELECTRO) then
    Exit;

  if g_Game_IsServer then p.Damage(d, SpawnerUID, vx, vy, t);

  Result := True;
end;

function GunHit(X, Y: Integer; vx, vy: Integer; SpawnerUID: Word; AllowPush: Boolean): Byte;
var
  i, h: Integer;
begin
  Result := 0;

  h := High(gPlayers);

  if h <> -1 then
    for i := 0 to h do
      if (gPlayers[i] <> nil) and gPlayers[i].Live and gPlayers[i].Collide(X, Y) then
        if HitPlayer(gPlayers[i], 3, vx*10, vy*10-3, SpawnerUID, HIT_SOME) then
        begin
          if AllowPush then gPlayers[i].Push(vx, vy);
          Result := 1;
        end;

  if Result <> 0 then Exit;

  h := High(gMonsters);

  if h <> -1 then
    for i := 0 to h do
      if (gMonsters[i] <> nil) and gMonsters[i].Live and gMonsters[i].Collide(X, Y) then
        if HitMonster(gMonsters[i], 3, vx*10, vy*10-3, SpawnerUID, HIT_SOME) then
        begin
          if AllowPush then gMonsters[i].Push(vx, vy);
          Result := 2;
          Exit;
        end;
end;

procedure BFG9000(X, Y: Integer; SpawnerUID: Word);
var
  i, h: Integer;
begin
  //g_Sound_PlayEx('SOUND_WEAPON_EXPLODEBFG', 255);

  h := High(gPlayers);

  if h <> -1 then
    for i := 0 to h do
      if (gPlayers[i] <> nil) and (gPlayers[i].Live) and (gPlayers[i].UID <> SpawnerUID) then
        with gPlayers[i] do
          if (g_PatchLength(X, Y, GameX+PLAYER_RECT.X+(PLAYER_RECT.Width div 2),
                            GameY+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)) <= SHOT_BFG_RADIUS) and
              g_TraceVector(X, Y, GameX+PLAYER_RECT.X+(PLAYER_RECT.Width div 2),
                            GameY+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)) then
            if HitPlayer(gPlayers[i], 50, 0, 0, SpawnerUID, HIT_SOME) then gPlayers[i].BFGHit();

  h := High(gMonsters);

  if h <> -1 then
    for i := 0 to h do
      if (gMonsters[i] <> nil) and (gMonsters[i].Live) and (gMonsters[i].UID <> SpawnerUID) then
        with gMonsters[i] do
          if (g_PatchLength(X, Y, Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2),
                            Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)) <= SHOT_BFG_RADIUS) and
              g_TraceVector(X, Y, Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2),
                            Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)) then
            if HitMonster(gMonsters[i], 50, 0, 0, SpawnerUID, HIT_SOME) then gMonsters[i].BFGHit();
end;

function g_Weapon_CreateShot(I: Integer; ShotType: Byte; Spawner, Target: Word; X, Y, XV, YV: Integer): LongWord;
var
  find_id, FramesID: DWORD;
begin
  if I < 0 then
    find_id := FindShot()
  else
  begin
    find_id := I;
    if Integer(find_id) >= High(Shots) then
      SetLength(Shots, find_id + 64)
  end;
  
  case ShotType of
    WEAPON_ROCKETLAUNCHER:
    begin
      with Shots[find_id] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := SHOT_ROCKETLAUNCHER_WIDTH;
        Obj.Rect.Height := SHOT_ROCKETLAUNCHER_HEIGHT;

        Animation := nil;
        Triggers := nil;
        ShotType := WEAPON_ROCKETLAUNCHER;
        g_Texture_Get('TEXTURE_WEAPON_ROCKET', TextureID);
      end;
    end;

    WEAPON_PLASMA:
    begin
      with Shots[find_id] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := SHOT_PLASMA_WIDTH;
        Obj.Rect.Height := SHOT_PLASMA_HEIGHT;

        Triggers := nil;
        ShotType := WEAPON_PLASMA;
        g_Frames_Get(FramesID, 'FRAMES_WEAPON_PLASMA');
        Animation := TAnimation.Create(FramesID, True, 5);
      end;
    end;

    WEAPON_BFG:
    begin
      with Shots[find_id] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := SHOT_BFG_WIDTH;
        Obj.Rect.Height := SHOT_BFG_HEIGHT;

        Triggers := nil;
        ShotType := WEAPON_BFG;
        g_Frames_Get(FramesID, 'FRAMES_WEAPON_BFG');
        Animation := TAnimation.Create(FramesID, True, 6);
      end;
    end;
  end;

  Shots[find_id].Obj.X := X;
  Shots[find_id].Obj.Y := Y;
  Shots[find_id].Obj.Vel.X := XV;
  Shots[find_id].Obj.Vel.Y := YV;
  Shots[find_id].Obj.Accel.X := 0;
  Shots[find_id].Obj.Accel.Y := 0;
  Shots[find_id].SpawnerUID := Spawner;
  Result := find_id;
end;

procedure throw(i, x, y, xd, yd, s: Integer);
var
  a: Integer;
begin
  yd := yd - y;
  xd := xd - x;

  a := Max(Abs(xd), Abs(yd));
  if a = 0 then
    a := 1;

  Shots[i].Obj.X := x;
  Shots[i].Obj.Y := y;
  Shots[i].Obj.Vel.X := (xd*s) div a;
  Shots[i].Obj.Vel.Y := (yd*s) div a;
  Shots[i].Obj.Accel.X := 0;
  Shots[i].Obj.Accel.Y := 0;
  if Shots[i].ShotType in [WEAPON_BSP_FIRE, WEAPON_PLASMA] then
    Shots[i].Timeout := 550 // ~15 sec
  else
    Shots[i].Timeout := 900 // ~25 sec
end;

function g_Weapon_Hit(obj: PObj; d: Integer; SpawnerUID: Word; t: Byte): Byte;
var
  i, h: Integer;
begin
  Result := 0;

  h := High(gPlayers);

  if h <> -1 then
    for i := 0 to h do
      if (gPlayers[i] <> nil) and gPlayers[i].Live and g_Obj_Collide(obj, @gPlayers[i].Obj) then
        if HitPlayer(gPlayers[i], d, obj^.Vel.X, obj^.Vel.Y, SpawnerUID, t) then
        begin
          gPlayers[i].Push((obj^.Vel.X+obj^.Accel.X)*IfThen(t = HIT_BFG, 8, 1) div 4,
                           (obj^.Vel.Y+obj^.Accel.Y)*IfThen(t = HIT_BFG, 8, 1) div 4);
          if t = HIT_BFG then g_Monsters_goodsnd();
          Result := 1;
          // TODO: Exit?
        end;

  if Result <> 0 then Exit;

  h := High(gMonsters);

  if h <> -1 then
    for i := 0 to h do
      if (gMonsters[i] <> nil) and gMonsters[i].Live and g_Obj_Collide(obj, @gMonsters[i].Obj) then
        if HitMonster(gMonsters[i], d, obj^.Vel.X, obj^.Vel.Y, SpawnerUID, t) then
        begin
          gMonsters[i].Push((obj^.Vel.X+obj^.Accel.X)*IfThen(t = HIT_BFG, 8, 1) div 4,
                            (obj^.Vel.Y+obj^.Accel.Y)*IfThen(t = HIT_BFG, 8, 1) div 4);
          Result := 2;
          Exit;
        end;
end;

function g_Weapon_HitUID(UID: Word; d: Integer; SpawnerUID: Word; t: Byte): Boolean;
begin
  Result := False;

  case g_GetUIDType(UID) of
    UID_PLAYER: Result := HitPlayer(g_Player_Get(UID), d, 0, 0, SpawnerUID, t);
    UID_MONSTER: Result := HitMonster(g_Monsters_Get(UID), d, 0, 0, SpawnerUID, t);
    else Exit;
  end;
end;

function g_Weapon_Explode(X, Y: Integer; rad: Integer; SpawnerUID: Word): Boolean;
var
  i, h, r, dx, dy, m, mm: Integer;
  _angle: SmallInt;
begin
  Result := False;

  g_Triggers_PressC(X, Y, rad, SpawnerUID, ACTIVATE_SHOT);

  r := rad*rad;

  h := High(gPlayers);

  if h <> -1 then
    for i := 0 to h do
      if (gPlayers[i] <> nil) and gPlayers[i].Live then
        with gPlayers[i] do
        begin
          dx := Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2)-X;
          dy := Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)-Y;

          if dx > 1000 then dx := 1000;
          if dy > 1000 then dy := 1000;

          if dx*dx+dy*dy < r then
          begin
            //m := PointToRect(X, Y, GameX+PLAYER_RECT.X, GameY+PLAYER_RECT.Y,
            //                 PLAYER_RECT.Width, PLAYER_RECT.Height);

            mm := Max(abs(dx), abs(dy));
            if mm = 0 then mm := 1;

            HitPlayer(gPlayers[i], (100*(rad-mm)) div rad, (dx*10) div mm, (dy*10) div mm, SpawnerUID, HIT_ROCKET);
            gPlayers[i].Push((dx*7) div mm, (dy*7) div mm);
          end;
        end;

  h := High(gMonsters);

  if h <> -1 then
    for i := 0 to h do
      if gMonsters[i] <> nil then
        with gMonsters[i] do
        begin
          dx := Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2)-X;
          dy := Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)-Y;

          if dx > 1000 then dx := 1000;
          if dy > 1000 then dy := 1000;

          if dx*dx+dy*dy < r then
          begin
            //m := PointToRect(X, Y, Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y,
            //                 Obj.Rect.Width, Obj.Rect.Height);

            mm := Max(abs(dx), abs(dy));
            if mm = 0 then mm := 1;

            if gMonsters[i].Live then
              HitMonster(gMonsters[i], ((gMonsters[i].Obj.Rect.Width div 4)*10*(rad-mm)) div rad,
                         0, 0, SpawnerUID, HIT_ROCKET);

            gMonsters[i].Push((dx*7) div mm, (dy*7) div mm);
          end;
        end;

  h := High(gCorpses);

  if gAdvCorpses and (h <> -1) then
    for i := 0 to h do
      if (gCorpses[i] <> nil) and (gCorpses[i].State <> CORPSE_STATE_REMOVEME) then
        with gCorpses[i] do
        begin
          dx := Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2)-X;
          dy := Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)-Y;

          if dx > 1000 then dx := 1000;
          if dy > 1000 then dy := 1000;

          if dx*dx+dy*dy < r then
          begin
            m := PointToRect(X, Y, Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y,
                             Obj.Rect.Width, Obj.Rect.Height);

            mm := Max(abs(dx), abs(dy));
            if mm = 0 then mm := 1;

            Damage(Round(100*(rad-m)/rad), (dx*10) div mm, (dy*10) div mm);
          end;
        end;

  h := High(gGibs);

  if gAdvGibs and (h <> -1) then
    for i := 0 to h do
      if gGibs[i].Live then
        with gGibs[i] do
        begin
          dx := Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2)-X;
          dy := Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)-Y;

          if dx > 1000 then dx := 1000;
          if dy > 1000 then dy := 1000;

          if dx*dx+dy*dy < r then
          begin
            m := PointToRect(X, Y, Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y,
                             Obj.Rect.Width, Obj.Rect.Height);
            _angle := GetAngle(Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2),
                               Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2), X, Y);

            g_Obj_PushA(@Obj, Round(15*(rad-m)/rad), _angle);
          end;
        end;
end;

procedure g_Weapon_Init();
begin
  CreateWaterMap();
end;

procedure g_Weapon_Free();
var
  i: Integer;
begin
  if Shots <> nil then
  begin
    for i := 0 to High(Shots) do
      if Shots[i].ShotType <> 0 then
        Shots[i].Animation.Free();

    Shots := nil;
  end;

  WaterMap := nil;
end;

procedure g_Weapon_LoadData();
begin
  e_WriteLog('Loading weapons data...', MSG_NOTIFY);

  g_Sound_CreateWADEx('SOUND_WEAPON_HITPUNCH', GameWAD+':SOUNDS\HITPUNCH');
  g_Sound_CreateWADEx('SOUND_WEAPON_MISSPUNCH', GameWAD+':SOUNDS\MISSPUNCH');
  g_Sound_CreateWADEx('SOUND_WEAPON_HITBERSERK', GameWAD+':SOUNDS\HITBERSERK');
  g_Sound_CreateWADEx('SOUND_WEAPON_MISSBERSERK', GameWAD+':SOUNDS\MISSBERSERK');
  g_Sound_CreateWADEx('SOUND_WEAPON_SELECTSAW', GameWAD+':SOUNDS\SELECTSAW');
  g_Sound_CreateWADEx('SOUND_WEAPON_IDLESAW', GameWAD+':SOUNDS\IDLESAW');
  g_Sound_CreateWADEx('SOUND_WEAPON_HITSAW', GameWAD+':SOUNDS\HITSAW');
  g_Sound_CreateWADEx('SOUND_WEAPON_FIRESHOTGUN2', GameWAD+':SOUNDS\FIRESHOTGUN2');
  g_Sound_CreateWADEx('SOUND_WEAPON_FIRESHOTGUN', GameWAD+':SOUNDS\FIRESHOTGUN');
  g_Sound_CreateWADEx('SOUND_WEAPON_FIRESAW', GameWAD+':SOUNDS\FIRESAW');
  g_Sound_CreateWADEx('SOUND_WEAPON_FIREROCKET', GameWAD+':SOUNDS\FIREROCKET');
  g_Sound_CreateWADEx('SOUND_WEAPON_FIREPLASMA', GameWAD+':SOUNDS\FIREPLASMA');
  g_Sound_CreateWADEx('SOUND_WEAPON_FIREPISTOL', GameWAD+':SOUNDS\FIREPISTOL');
  g_Sound_CreateWADEx('SOUND_WEAPON_FIRECGUN', GameWAD+':SOUNDS\FIRECGUN');
  g_Sound_CreateWADEx('SOUND_WEAPON_FIREBFG', GameWAD+':SOUNDS\FIREBFG');
  g_Sound_CreateWADEx('SOUND_FIRE', GameWAD+':SOUNDS\FIRE');
  g_Sound_CreateWADEx('SOUND_WEAPON_STARTFIREBFG', GameWAD+':SOUNDS\STARTFIREBFG');
  g_Sound_CreateWADEx('SOUND_WEAPON_EXPLODEROCKET', GameWAD+':SOUNDS\EXPLODEROCKET');
  g_Sound_CreateWADEx('SOUND_WEAPON_EXPLODEBFG', GameWAD+':SOUNDS\EXPLODEBFG');
  g_Sound_CreateWADEx('SOUND_WEAPON_BFGWATER', GameWAD+':SOUNDS\BFGWATER');
  g_Sound_CreateWADEx('SOUND_WEAPON_EXPLODEPLASMA', GameWAD+':SOUNDS\EXPLODEPLASMA');
  g_Sound_CreateWADEx('SOUND_WEAPON_PLASMAWATER', GameWAD+':SOUNDS\PLASMAWATER');
  g_Sound_CreateWADEx('SOUND_WEAPON_FIREBALL', GameWAD+':SOUNDS\FIREBALL');
  g_Sound_CreateWADEx('SOUND_WEAPON_EXPLODEBALL', GameWAD+':SOUNDS\EXPLODEBALL');
  g_Sound_CreateWADEx('SOUND_WEAPON_FIREREV', GameWAD+':SOUNDS\FIREREV');
  g_Sound_CreateWADEx('SOUND_PLAYER_JETFLY', GameWAD+':SOUNDS\JETPAKI');
  g_Sound_CreateWADEx('SOUND_PLAYER_JETON', GameWAD+':SOUNDS\JETPAKON');
  g_Sound_CreateWADEx('SOUND_PLAYER_JETOFF', GameWAD+':SOUNDS\JETPAKOF');

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
  g_Frames_CreateWAD(nil, 'FRAMES_EXPLODE_PLASMA', GameWAD+':TEXTURES\EPLASMA', 32, 32, 4, True);
  g_Frames_CreateWAD(nil, 'FRAMES_EXPLODE_BSPFIRE', GameWAD+':TEXTURES\EBSPFIRE', 32, 32, 5);
  g_Frames_CreateWAD(nil, 'FRAMES_EXPLODE_CACOFIRE', GameWAD+':TEXTURES\ECACOFIRE', 64, 64, 3);
  g_Frames_CreateWAD(nil, 'FRAMES_EXPLODE_BARONFIRE', GameWAD+':TEXTURES\EBARONFIRE', 64, 64, 3);
  g_Frames_CreateWAD(nil, 'FRAMES_SMOKE', GameWAD+':TEXTURES\SMOKE', 32, 32, 10, False);
end;

procedure g_Weapon_FreeData();
begin
  e_WriteLog('Releasing weapons data...', MSG_NOTIFY);

  g_Sound_Delete('SOUND_WEAPON_HITPUNCH');
  g_Sound_Delete('SOUND_WEAPON_MISSPUNCH');
  g_Sound_Delete('SOUND_WEAPON_HITBERSERK');
  g_Sound_Delete('SOUND_WEAPON_MISSBERSERK');
  g_Sound_Delete('SOUND_WEAPON_SELECTSAW');
  g_Sound_Delete('SOUND_WEAPON_IDLESAW');
  g_Sound_Delete('SOUND_WEAPON_HITSAW');
  g_Sound_Delete('SOUND_WEAPON_FIRESHOTGUN2');
  g_Sound_Delete('SOUND_WEAPON_FIRESHOTGUN');
  g_Sound_Delete('SOUND_WEAPON_FIRESAW');
  g_Sound_Delete('SOUND_WEAPON_FIREROCKET');
  g_Sound_Delete('SOUND_WEAPON_FIREPLASMA');
  g_Sound_Delete('SOUND_WEAPON_FIREPISTOL');
  g_Sound_Delete('SOUND_WEAPON_FIRECGUN');
  g_Sound_Delete('SOUND_WEAPON_FIREBFG');
  g_Sound_Delete('SOUND_FIRE');
  g_Sound_Delete('SOUND_WEAPON_STARTFIREBFG');
  g_Sound_Delete('SOUND_WEAPON_EXPLODEROCKET');
  g_Sound_Delete('SOUND_WEAPON_EXPLODEBFG');
  g_Sound_Delete('SOUND_WEAPON_BFGWATER');
  g_Sound_Delete('SOUND_WEAPON_EXPLODEPLASMA');
  g_Sound_Delete('SOUND_WEAPON_PLASMAWATER');
  g_Sound_Delete('SOUND_WEAPON_FIREBALL');
  g_Sound_Delete('SOUND_WEAPON_EXPLODEBALL');

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

procedure g_Weapon_gun(x, y, xd, yd, v: Integer; SpawnerUID: Word; CheckTrigger: Boolean);
var
  a: Integer;
  x2, y2: Integer;
  dx, dy: Integer;
  xe, ye: Integer;
  xi, yi: Integer;
  s, c: Extended;
  //vx, vy: Integer;
  xx, yy, d: Integer;

  i: Integer;
  t1, _collide: Boolean;
  w, h: Word;
begin
  a := GetAngle(x, y, xd, yd)+180;

  SinCos(DegToRad(-a), s, c);

  if Abs(s) < 0.01 then s := 0;
  if Abs(c) < 0.01 then c := 0;

  x2 := x+Round(c*gMapInfo.Width);
  y2 := y+Round(s*gMapInfo.Width);

  t1 := gWalls <> nil;
  _collide := False;
  w := gMapInfo.Width;
  h := gMapInfo.Height;

  xe := 0;
  ye := 0;
  dx := x2-x;
  dy := y2-y;

  if (xd = 0) and (yd = 0) then Exit;

  if dx > 0 then xi := 1 else if dx < 0 then xi := -1 else xi := 0;
  if dy > 0 then yi := 1 else if dy < 0 then yi := -1 else yi := 0;

  dx := Abs(dx);
  dy := Abs(dy);

  if dx > dy then d := dx else d := dy;

  //blood vel, for Monster.Damage()
  //vx := (dx*10 div d)*xi;
  //vy := (dy*10 div d)*yi;

  xx := x;
  yy := y;

  for i := 1 to d do
  begin
    xe := xe+dx;
    ye := ye+dy;

    if xe > d then
    begin
      xe := xe-d;
      xx := xx+xi;
    end;

    if ye > d then
    begin
      ye := ye-d;
      yy := yy+yi;
    end;

    if (yy > h) or (yy < 0) then Break;
    if (xx > w) or (xx < 0) then Break;

    if t1 then
      if ByteBool(gCollideMap[yy, xx] and MARK_BLOCKED) then
      begin
        _collide := True;
        g_GFX_Spark(xx-xi, yy-yi, 2+Random(2), 180+a, 0, 0);
        if g_Game_IsServer and g_Game_IsNet then
          MH_SEND_Effect(xx-xi, yy-yi, 180+a, NET_GFX_SPARK);
      end;

    if not _collide then
    _collide := GunHit(xx, yy, xi*v, yi*v, SpawnerUID, v <> 0) <> 0;

    if _collide then Break;
  end;

  if CheckTrigger and g_Game_IsServer then
    g_Triggers_PressL(X, Y, xx-xi, yy-yi, SpawnerUID, ACTIVATE_SHOT);
end;

procedure g_Weapon_punch(x, y: Integer; d, SpawnerUID: Word);
var
  obj: TObj;
begin
  obj.X := X;
  obj.Y := Y;
  obj.rect.X := 0;
  obj.rect.Y := 0;
  obj.rect.Width := 39;
  obj.rect.Height := 52;
  obj.Vel.X := 0;
  obj.Vel.Y := 0;
  obj.Accel.X := 0;
  obj.Accel.Y := 0;

  if g_Weapon_Hit(@obj, d, SpawnerUID, HIT_SOME) <> 0 then
    g_Sound_PlayExAt('SOUND_WEAPON_HITPUNCH', x, y)
  else
    g_Sound_PlayExAt('SOUND_WEAPON_MISSPUNCH', x, y);
end;

function g_Weapon_chainsaw(x, y: Integer; d, SpawnerUID: Word): Integer;
var
  obj: TObj;
begin
  obj.X := X;
  obj.Y := Y;
  obj.rect.X := 0;
  obj.rect.Y := 0;
  obj.rect.Width := 32;
  obj.rect.Height := 52;
  obj.Vel.X := 0;
  obj.Vel.Y := 0;
  obj.Accel.X := 0;
  obj.Accel.Y := 0;

  Result := g_Weapon_Hit(@obj, d, SpawnerUID, HIT_SOME);
end;

procedure g_Weapon_rocket(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
var
  find_id: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    find_id := FindShot()
  else
  begin
    find_id := WID;
    if Integer(find_id) >= High(Shots) then
      SetLength(Shots, find_id + 64)
  end;

  with Shots[find_id] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := SHOT_ROCKETLAUNCHER_WIDTH;
    Obj.Rect.Height := SHOT_ROCKETLAUNCHER_HEIGHT;

    dx := IfThen(xd>x, -Obj.Rect.Width, 0);
    dy := -(Obj.Rect.Height div 2);
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 12);

    Animation := nil;
    triggers := nil;
    ShotType := WEAPON_ROCKETLAUNCHER;
    g_Texture_Get('TEXTURE_WEAPON_ROCKET', TextureID);
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  g_Sound_PlayExAt('SOUND_WEAPON_FIREROCKET', x, y);
end;

procedure g_Weapon_revf(x, y, xd, yd: Integer; SpawnerUID, TargetUID: Word; WID: Integer = -1);
var
  find_id, FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    find_id := FindShot()
  else
  begin
    find_id := WID;
    if Integer(find_id) >= High(Shots) then
      SetLength(Shots, find_id + 64)
  end;

  with Shots[find_id] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := SHOT_SKELFIRE_WIDTH;
    Obj.Rect.Height := SHOT_SKELFIRE_HEIGHT;

    dx := IfThen(xd>x, -Obj.Rect.Width, 0);
    dy := -(Obj.Rect.Height div 2);
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 12);

    triggers := nil;
    ShotType := WEAPON_SKEL_FIRE;
    target := TargetUID;
    g_Frames_Get(FramesID, 'FRAMES_WEAPON_SKELFIRE');
    Animation := TAnimation.Create(FramesID, True, 5);
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  g_Sound_PlayExAt('SOUND_WEAPON_FIREROCKET', x, y);
end;

procedure g_Weapon_plasma(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
var
  find_id, FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    find_id := FindShot()
  else
  begin
    find_id := WID;
    if Integer(find_id) >= High(Shots) then
      SetLength(Shots, find_id + 64);
  end;

  with Shots[find_id] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := SHOT_PLASMA_WIDTH;
    Obj.Rect.Height := SHOT_PLASMA_HEIGHT;

    dx := IfThen(xd>x, -Obj.Rect.Width, 0);
    dy := -(Obj.Rect.Height div 2);
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    ShotType := WEAPON_PLASMA;
    g_Frames_Get(FramesID, 'FRAMES_WEAPON_PLASMA');
    Animation := TAnimation.Create(FramesID, True, 5);
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  g_Sound_PlayExAt('SOUND_WEAPON_FIREPLASMA', x, y);
end;

procedure g_Weapon_ball1(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
var
  find_id, FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    find_id := FindShot()
  else
  begin
    find_id := WID;
    if Integer(find_id) >= High(Shots) then
      SetLength(Shots, find_id + 64)
  end;

  with Shots[find_id] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := 16;
    Obj.Rect.Height := 16;

    dx := IfThen(xd>x, -Obj.Rect.Width, 0);
    dy := -(Obj.Rect.Height div 2);
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    ShotType := WEAPON_IMP_FIRE;
    g_Frames_Get(FramesID, 'FRAMES_WEAPON_IMPFIRE');
    Animation := TAnimation.Create(FramesID, True, 4);
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  g_Sound_PlayExAt('SOUND_WEAPON_FIREBALL', x, y);
end;

procedure g_Weapon_ball2(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
var
  find_id, FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    find_id := FindShot()
  else
  begin
    find_id := WID;
    if Integer(find_id) >= High(Shots) then
      SetLength(Shots, find_id + 64)
  end;

  with Shots[find_id] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := 16;
    Obj.Rect.Height := 16;

    dx := IfThen(xd>x, -Obj.Rect.Width, 0);
    dy := -(Obj.Rect.Height div 2);
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    ShotType := WEAPON_CACO_FIRE;
    g_Frames_Get(FramesID, 'FRAMES_WEAPON_CACOFIRE');
    Animation := TAnimation.Create(FramesID, True, 4);
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  g_Sound_PlayExAt('SOUND_WEAPON_FIREBALL', x, y);
end;

procedure g_Weapon_ball7(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
var
  find_id, FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    find_id := FindShot()
  else
  begin
    find_id := WID;
    if Integer(find_id) >= High(Shots) then
      SetLength(Shots, find_id + 64)
  end;

  with Shots[find_id] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := 32;
    Obj.Rect.Height := 16;

    dx := IfThen(xd>x, -Obj.Rect.Width, 0);
    dy := -(Obj.Rect.Height div 2);
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    ShotType := WEAPON_BARON_FIRE;
    g_Frames_Get(FramesID, 'FRAMES_WEAPON_BARONFIRE');
    Animation := TAnimation.Create(FramesID, True, 4);
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  g_Sound_PlayExAt('SOUND_WEAPON_FIREBALL', x, y);
end;

procedure g_Weapon_aplasma(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
var
  find_id, FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    find_id := FindShot()
  else
  begin
    find_id := WID;
    if Integer(find_id) >= High(Shots) then
      SetLength(Shots, find_id + 64)
  end;

  with Shots[find_id] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := 16;
    Obj.Rect.Height := 16;

    dx := IfThen(xd>x, -Obj.Rect.Width, 0);
    dy := -(Obj.Rect.Height div 2);
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    ShotType := WEAPON_BSP_FIRE;
    g_Frames_Get(FramesID, 'FRAMES_WEAPON_BSPFIRE');
    Animation := TAnimation.Create(FramesID, True, 4);
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  g_Sound_PlayExAt('SOUND_WEAPON_FIREPLASMA', x, y);
end;

procedure g_Weapon_manfire(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
var
  find_id, FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    find_id := FindShot()
  else
  begin
    find_id := WID;
    if Integer(find_id) >= High(Shots) then
      SetLength(Shots, find_id + 64)
  end;

  with Shots[find_id] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := 32;
    Obj.Rect.Height := 32;

    dx := IfThen(xd>x, -Obj.Rect.Width, 0);
    dy := -(Obj.Rect.Height div 2);
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    ShotType := WEAPON_MANCUB_FIRE;
    g_Frames_Get(FramesID, 'FRAMES_WEAPON_MANCUBFIRE'); 
    Animation := TAnimation.Create(FramesID, True, 4);
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  g_Sound_PlayExAt('SOUND_WEAPON_FIREBALL', x, y);
end;

procedure g_Weapon_bfgshot(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1);
var
  find_id, FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    find_id := FindShot()
  else
  begin
    find_id := WID;
    if Integer(find_id) >= High(Shots) then
      SetLength(Shots, find_id + 64)
  end;
 
  with Shots[find_id] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := SHOT_BFG_WIDTH;
    Obj.Rect.Height := SHOT_BFG_HEIGHT;

    dx := IfThen(xd>x, -Obj.Rect.Width, 0);
    dy := -(Obj.Rect.Height div 2);
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    ShotType := WEAPON_BFG;
    g_Frames_Get(FramesID, 'FRAMES_WEAPON_BFG');
    Animation := TAnimation.Create(FramesID, True, 6);
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  g_Sound_PlayExAt('SOUND_WEAPON_FIREBFG', x, y);
end;

procedure g_Weapon_bfghit(x, y: Integer);
var
  ID: DWORD;
  Anim: TAnimation;
begin
  if g_Frames_Get(ID, 'FRAMES_BFGHIT') then
  begin
    Anim := TAnimation.Create(ID, False, 4);
    g_GFX_OnceAnim(x-32, y-32, Anim);
    Anim.Free();
  end;
end;

procedure g_Weapon_pistol(x, y, xd, yd: Integer; SpawnerUID: Word);
begin
  g_Sound_PlayExAt('SOUND_WEAPON_FIREPISTOL', x, y);
  g_Weapon_gun(x, y, xd, yd, 1, SpawnerUID, True);
  if gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF] then
  begin
    g_Weapon_gun(x, y+1, xd, yd+1, 1, SpawnerUID, False);
    g_Weapon_gun(x, y-1, xd, yd-1, 1, SpawnerUID, False);
 end;
end;

procedure g_Weapon_mgun(x, y, xd, yd: Integer; SpawnerUID: Word);
begin
  g_Sound_PlayExAt('SOUND_WEAPON_FIRECGUN', x, y);
  g_Weapon_gun(x, y, xd, yd, 1, SpawnerUID, True);

  if (gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF]) and
     (g_GetUIDType(SpawnerUID) = UID_PLAYER) then
    g_Weapon_gun(x, y+1, xd, yd+1, 1, SpawnerUID, True);
end;

procedure g_Weapon_shotgun(x, y, xd, yd: Integer; SpawnerUID: Word);
var
  i, j: Integer;
begin
  g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN', x, y);
  for i := 0 to 9 do
  begin
    j := Random(17)-8; // -8 .. 8
    g_Weapon_gun(x, y+j, xd, yd+j, IfThen(i mod 2 <> 0, 1, 0), SpawnerUID, i=0);
  end;
end;

procedure g_Weapon_dshotgun(x, y, xd, yd: Integer; SpawnerUID: Word);
var
  a, i, j: Integer;
begin
  g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN2', x, y);

  if gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF] then a := 25 else a := 20;
  for i := 0 to a do
  begin
    j := Random(41)-20; // -20 .. 20
    g_Weapon_gun(x, y+j, xd, yd+j, IfThen(i mod 3 <> 0, 0, 1), SpawnerUID, i=0);
  end;
end;

procedure g_Weapon_Update();
var
  i, a, h, cx, cy, oldvx, oldvy: Integer;
  _id: DWORD;
  Anim: TAnimation;
  t: DWArray;
  st: Word;
  s: String;
  o: TObj;
  spl: Boolean;
  Loud: Boolean;
begin
  if Shots = nil then
    Exit;

  for i := 0 to High(Shots) do
  begin
    if Shots[i].ShotType = 0 then
      Continue;

    Loud := True;

    with Shots[i] do
    begin
      Timeout := Timeout - 1;
      oldvx := Obj.Vel.X;
      oldvy := Obj.Vel.Y;
    // Активировать триггеры по пути (кроме уже активированных):
      if g_Game_IsServer then
        t := g_Triggers_PressR(Obj.X, Obj.Y, Obj.Rect.Width, Obj.Rect.Height,
                               SpawnerUID, ACTIVATE_SHOT, triggers)
      else
        t := nil;

      if t <> nil then
      begin
      // Пополняем список активированных триггеров:
        if triggers = nil then
          triggers := t
        else
          begin
            h := High(t);

            for a := 0 to h do
              if not InDWArray(t[a], triggers) then
              begin
                SetLength(triggers, Length(triggers)+1);
                triggers[High(triggers)] := t[a];
              end;
          end;
      end;

    // Анимация снаряда:
      if Animation <> nil then
        Animation.Update();

    // Движение:
      spl := (ShotType <> WEAPON_PLASMA) and
             (ShotType <> WEAPON_BFG) and
             (ShotType <> WEAPON_BSP_FIRE);

      st := g_Obj_Move(@Obj, False, spl);

      if WordBool(st and MOVE_FALLOUT) or (Obj.X < -1000) or
        (Obj.X > gMapInfo.Width+1000) or (Obj.Y < -1000) then
      begin
        // На клиенте скорее всего и так уже выпал.
        ShotType := 0;
        Animation.Free();
        Continue;
      end;

      cx := Obj.X + (Obj.Rect.Width div 2);
      cy := Obj.Y + (Obj.Rect.Height div 2);

      case ShotType of
        WEAPON_ROCKETLAUNCHER, WEAPON_SKEL_FIRE: // Ракеты и снаряды Скелета
          begin
          // Вылетела из воды:
            if WordBool(st and MOVE_HITAIR) then
              g_Obj_SetSpeed(@Obj, 12);

          // В воде шлейф - пузыри, в воздухе шлейф - дым:
            if WordBool(st and MOVE_INWATER) then
              g_GFX_Bubbles(Obj.X+(Obj.Rect.Width div 2),
                            Obj.Y+(Obj.Rect.Height div 2),
                            1+Random(3), 16, 16)
            else
              if g_Frames_Get(_id, 'FRAMES_SMOKE') then
              begin
                Anim := TAnimation.Create(_id, False, 3);
                Anim.Alpha := 150;
                g_GFX_OnceAnim(Obj.X-8+Random(9),
                               Obj.Y+(Obj.Rect.Height div 2)-20+Random(9),
                               Anim, ONCEANIM_SMOKE);
                Anim.Free();
              end;

          // Попали в кого-то или в стену:
            if WordBool(st and (MOVE_HITWALL or MOVE_HITLAND or MOVE_HITCEIL)) or
               (g_Weapon_Hit(@Obj, 10, SpawnerUID, HIT_SOME) <> 0) or
               (Timeout < 1) then
            begin
              Obj.Vel.X := 0;
              Obj.Vel.Y := 0;

              g_Weapon_Explode(cx, cy, 60, SpawnerUID);

              if ShotType = WEAPON_SKEL_FIRE then
                begin // Взрыв снаряда Скелета
                  if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_SKELFIRE') then
                  begin
                    Anim := TAnimation.Create(TextureID, False, 8);
                    Anim.Blending := False;
                    g_GFX_OnceAnim((Obj.X+32)-32, (Obj.Y+8)-32, Anim);
                    Anim.Free();
                  end;
                end
              else
                begin // Взрыв Ракеты
                  if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_ROCKET') then
                  begin
                    Anim := TAnimation.Create(TextureID, False, 6);
                    Anim.Blending := False;
                    g_GFX_OnceAnim(cx-64, cy-64, Anim);
                    Anim.Free();
                  end;
                end;

              g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEROCKET', Obj.X, Obj.Y);

              ShotType := 0;
            end;

            if ShotType = WEAPON_SKEL_FIRE then
            begin // Самонаводка снаряда Скелета:
              if GetPos(target, @o) then
                throw(i, Obj.X, Obj.Y,
                      o.X+o.Rect.X+(o.Rect.Width div 2)+o.Vel.X+o.Accel.X,
                      o.Y+o.Rect.Y+(o.Rect.Height div 2)+o.Vel.Y+o.Accel.Y,
                      12);
            end;
          end;

        WEAPON_PLASMA, WEAPON_BSP_FIRE: // Плазма, плазма Арахнатрона
          begin
          // Попала в воду - электрошок по воде:
            if WordBool(st and (MOVE_INWATER or MOVE_HITWATER)) then
            begin
              g_Sound_PlayExAt('SOUND_WEAPON_PLASMAWATER', Obj.X, Obj.Y);
              if g_Game_IsServer then CheckTrap(i, 10, HIT_ELECTRO);
              ShotType := 0;
              Continue;
            end;

          // Величина урона:
            if (ShotType = WEAPON_PLASMA) and
               (gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF]) then
              a := 10
            else
              a := 5;

            if ShotType = WEAPON_BSP_FIRE then
              a := 10;

          // Попали в кого-то или в стену:
            if WordBool(st and (MOVE_HITWALL or MOVE_HITLAND or MOVE_HITCEIL)) or
               (g_Weapon_Hit(@Obj, a, SpawnerUID, HIT_SOME) <> 0) or
               (Timeout < 1) then
            begin
              if ShotType = WEAPON_PLASMA then
                s := 'FRAMES_EXPLODE_PLASMA'
              else
                s := 'FRAMES_EXPLODE_BSPFIRE';

            // Взрыв Плазмы:
              if g_Frames_Get(TextureID, s) then
              begin
                Anim := TAnimation.Create(TextureID, False, 3);
                Anim.Blending := False;
                g_GFX_OnceAnim(cx-16, cy-16, Anim);
                Anim.Free();
              end;

              g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEPLASMA', Obj.X, Obj.Y);

              ShotType := 0;
            end;
          end;

        WEAPON_BFG: // BFG
          begin
          // Попала в воду - электрошок по воде:
            if WordBool(st and (MOVE_INWATER or MOVE_HITWATER)) then
            begin
              g_Sound_PlayExAt('SOUND_WEAPON_BFGWATER', Obj.X, Obj.Y);
              if g_Game_IsServer then CheckTrap(i, 1000, HIT_ELECTRO);
              ShotType := 0;
              Continue;
            end;

          // Попали в кого-то или в стену:
            if WordBool(st and (MOVE_HITWALL or MOVE_HITLAND or MOVE_HITCEIL)) or
               (g_Weapon_Hit(@Obj, SHOT_BFG_DAMAGE, SpawnerUID, HIT_BFG) <> 0) or
               (Timeout < 1) then
            begin
            // Лучи BFG:
              if g_Game_IsServer then BFG9000(cx, cy, SpawnerUID);

            // Взрыв BFG:
              if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_BFG') then
              begin
                Anim := TAnimation.Create(TextureID, False, 6);
                Anim.Blending := False;
                g_GFX_OnceAnim(cx-64, cy-64, Anim);
                Anim.Free();
              end;

              g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBFG', Obj.X, Obj.Y);

              ShotType := 0;
            end;
          end;

        WEAPON_IMP_FIRE, WEAPON_CACO_FIRE, WEAPON_BARON_FIRE: // Выстрелы Беса, Какодемона Рыцаря/Барона ада
          begin
          // Вылетел из воды:
            if WordBool(st and MOVE_HITAIR) then
              g_Obj_SetSpeed(@Obj, 16);

          // Величина урона:
            if ShotType = WEAPON_IMP_FIRE then
              a := 5
            else
              if ShotType = WEAPON_CACO_FIRE then
                a := 20
              else
                a := 40;

          // Попали в кого-то или в стену:
            if WordBool(st and (MOVE_HITWALL or MOVE_HITLAND or MOVE_HITCEIL)) or
               (g_Weapon_Hit(@Obj, a, SpawnerUID, HIT_SOME) <> 0) or
               (Timeout < 1) then
            begin
              if ShotType = WEAPON_IMP_FIRE then
                s := 'FRAMES_EXPLODE_IMPFIRE'
              else
                if ShotType = WEAPON_CACO_FIRE then
                  s := 'FRAMES_EXPLODE_CACOFIRE'
                else
                  s := 'FRAMES_EXPLODE_BARONFIRE';

            // Взрыв:
              if g_Frames_Get(TextureID, s) then
              begin
                Anim := TAnimation.Create(TextureID, False, 6);
                Anim.Blending := False;
                g_GFX_OnceAnim(cx-32, cy-32, Anim);
                Anim.Free();
              end;

              g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBALL', Obj.X, Obj.Y);

              ShotType := 0;
            end;
          end;

        WEAPON_MANCUB_FIRE: // Выстрел Манкубуса
          begin
          // Вылетел из воды:
            if WordBool(st and MOVE_HITAIR) then
              g_Obj_SetSpeed(@Obj, 16);

          // Попали в кого-то или в стену:
            if WordBool(st and (MOVE_HITWALL or MOVE_HITLAND or MOVE_HITCEIL)) or
               (g_Weapon_Hit(@Obj, 40, SpawnerUID, HIT_SOME) <> 0) or
               (Timeout < 1) then
            begin
            // Взрыв:
              if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_ROCKET') then
              begin
                Anim := TAnimation.Create(TextureID, False, 6);
                Anim.Blending := False;
                g_GFX_OnceAnim(cx-64, cy-64, Anim);
                Anim.Free();
              end;

              g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBALL', Obj.X, Obj.Y);

              ShotType := 0;
            end;
          end;
      end; // case ShotType of...

    // Если снаряда уже нет, удаляем анимацию:
      if (ShotType = 0) then
      begin
        if gGameSettings.GameType = GT_SERVER then
          MH_SEND_DeleteShot(i, Obj.X, Obj.Y, Loud);
        Animation.Free();
        Animation := nil;
      end
      else if (oldvx <> Obj.Vel.X) or (oldvy <> Obj.Vel.Y) then
        if gGameSettings.GameType = GT_SERVER then
          MH_SEND_UpdateShot(i);
    end;
  end;
end;

procedure g_Weapon_Draw();
var
  i: Integer;
  a: SmallInt;
  p: TPoint;
begin
  if Shots = nil then
    Exit;

  for i := 0 to High(Shots) do
    if Shots[i].ShotType <> 0 then
      with Shots[i] do
      begin
        if (Shots[i].ShotType = WEAPON_ROCKETLAUNCHER) or
           (Shots[i].ShotType = WEAPON_BARON_FIRE) or
           (Shots[i].ShotType = WEAPON_MANCUB_FIRE) or
           (Shots[i].ShotType = WEAPON_SKEL_FIRE) then
          a := -GetAngle2(Obj.Vel.X, Obj.Vel.Y)
        else
          a := 0;

        p.X := Obj.Rect.Width div 2;
        p.Y := Obj.Rect.Height div 2;

        if Animation <> nil then
          begin
            if (Shots[i].ShotType = WEAPON_BARON_FIRE) or
               (Shots[i].ShotType = WEAPON_MANCUB_FIRE) or
               (Shots[i].ShotType = WEAPON_SKEL_FIRE) then
              Animation.DrawEx(Obj.X, Obj.Y, M_NONE, p, a)
            else
              Animation.Draw(Obj.X, Obj.Y, M_NONE);
          end
        else
          begin
            if (Shots[i].ShotType = WEAPON_ROCKETLAUNCHER) then
              e_DrawAdv(TextureID, Obj.X, Obj.Y, 0, True, False, a, @p, M_NONE)
            else
              e_Draw(TextureID, Obj.X, Obj.Y, 0, True, False);
          end;
      end;
end;

function g_Weapon_Danger(UID: Word; X, Y: Integer; Width, Height: Word; Time: Byte): Boolean;
var
  a: Integer;
begin
  Result := False;

  if Shots = nil then
    Exit;

  for a := 0 to High(Shots) do
    if (Shots[a].ShotType <> 0) and (Shots[a].SpawnerUID <> UID) then
      if ((Shots[a].Obj.Vel.Y = 0) and (Shots[a].Obj.Vel.X > 0) and (Shots[a].Obj.X < X)) or
          (Shots[a].Obj.Vel.Y = 0) and (Shots[a].Obj.Vel.X < 0) and (Shots[a].Obj.X > X) then
        if (Abs(X-Shots[a].Obj.X) < Abs(Shots[a].Obj.Vel.X*Time)) and
            g_Collide(X, Y, Width, Height, X, Shots[a].Obj.Y,
                      Shots[a].Obj.Rect.Width, Shots[a].Obj.Rect.Height) and
            g_TraceVector(X, Y, Shots[a].Obj.X, Shots[a].Obj.Y) then
        begin
          Result := True;
          Exit;
        end;
end;

procedure g_Weapon_SaveState(var Mem: TBinMemoryWriter);
var
  count, i, j: Integer;
  dw: DWORD;
begin
// Считаем количество существующих снарядов:
  count := 0;
  if Shots <> nil then
    for i := 0 to High(Shots) do
      if Shots[i].ShotType <> 0 then
        count := count + 1;

  Mem := TBinMemoryWriter.Create((count+1) * 80);

// Количество снарядов:
  Mem.WriteInt(count);

  if count = 0 then
    Exit;

  for i := 0 to High(Shots) do
    if Shots[i].ShotType <> 0 then
    begin
    // Сигнатура снаряда:
      dw := SHOT_SIGNATURE; // 'SHOT'
      Mem.WriteDWORD(dw);
    // Тип снаряда:
      Mem.WriteByte(Shots[i].ShotType);
    // Цель:
      Mem.WriteWord(Shots[i].Target);
    // UID стрелявшего:
      Mem.WriteWord(Shots[i].SpawnerUID);
    // Размер поля Triggers:
      dw := Length(Shots[i].Triggers);
      Mem.WriteDWORD(dw);
    // Триггеры, активированные выстрелом:
      for j := 0 to Integer(dw)-1 do
        Mem.WriteDWORD(Shots[i].Triggers[j]);
    // Объект снаряда:
      Obj_SaveState(@Shots[i].Obj, Mem);
    end;
end;

procedure g_Weapon_LoadState(var Mem: TBinMemoryReader);
var
  count, i, j: Integer;
  dw: DWORD;
begin
  if Mem = nil then
    Exit;

// Количество снарядов:
  Mem.ReadInt(count);

  SetLength(Shots, count);

  if count = 0 then
    Exit;

  for i := 0 to count-1 do
  begin
  // Сигнатура снаряда:
    Mem.ReadDWORD(dw);
    if dw <> SHOT_SIGNATURE then // 'SHOT'
    begin
      raise EBinSizeError.Create('g_Weapons_LoadState: Wrong Shot Signature');
    end;
  // Тип снаряда:
    Mem.ReadByte(Shots[i].ShotType);
  // Цель:
    Mem.ReadWord(Shots[i].Target);
  // UID стрелявшего:
    Mem.ReadWord(Shots[i].SpawnerUID);
  // Размер поля Triggers:
    Mem.ReadDWORD(dw);
    SetLength(Shots[i].Triggers, dw);
  // Триггеры, активированные выстрелом:
    for j := 0 to Integer(dw)-1 do
      Mem.ReadDWORD(Shots[i].Triggers[j]);
  // Объект предмета:
    Obj_LoadState(@Shots[i].Obj, Mem);

  // Установка текстуры или анимации:
    Shots[i].TextureID := DWORD(-1);
    Shots[i].Animation := nil;

    case Shots[i].ShotType of
      WEAPON_ROCKETLAUNCHER, WEAPON_SKEL_FIRE:
        begin
          g_Texture_Get('TEXTURE_WEAPON_ROCKET', Shots[i].TextureID);
        end;
      WEAPON_PLASMA:
        begin
          g_Frames_Get(dw, 'FRAMES_WEAPON_PLASMA');
          Shots[i].Animation := TAnimation.Create(dw, True, 5);
        end;
      WEAPON_BFG:
        begin
          g_Frames_Get(dw, 'FRAMES_WEAPON_BFG');
          Shots[i].Animation := TAnimation.Create(dw, True, 6);
        end;
      WEAPON_IMP_FIRE:
        begin
          g_Frames_Get(dw, 'FRAMES_WEAPON_IMPFIRE');
          Shots[i].Animation := TAnimation.Create(dw, True, 4);
        end;
      WEAPON_BSP_FIRE:
        begin
          g_Frames_Get(dw, 'FRAMES_WEAPON_BSPFIRE');
          Shots[i].Animation := TAnimation.Create(dw, True, 4);
        end;
      WEAPON_CACO_FIRE:
        begin
          g_Frames_Get(dw, 'FRAMES_WEAPON_CACOFIRE');
          Shots[i].Animation := TAnimation.Create(dw, True, 4);
        end;
      WEAPON_BARON_FIRE:
        begin
          g_Frames_Get(dw, 'FRAMES_WEAPON_BARONFIRE');
          Shots[i].Animation := TAnimation.Create(dw, True, 4);
        end;
      WEAPON_MANCUB_FIRE:
        begin
          g_Frames_Get(dw, 'FRAMES_WEAPON_MANCUBFIRE');
          Shots[i].Animation := TAnimation.Create(dw, True, 4);
        end;
    end;
  end;
end;

procedure g_Weapon_DestroyShot(I: Integer; X, Y: Integer; Loud: Boolean = True);
var
  cx, cy: Integer;
  Anim: TAnimation;
  s: string;
begin
  if Shots = nil then
    Exit;
  if (I > High(Shots)) or (I < 0) then Exit;
  
  with Shots[I] do
  begin
    if ShotType = 0 then Exit;
    Obj.X := X;
    Obj.Y := Y;
    cx := Obj.X + (Obj.Rect.Width div 2);
    cy := Obj.Y + (Obj.Rect.Height div 2);

    case ShotType of
      WEAPON_ROCKETLAUNCHER, WEAPON_SKEL_FIRE: // Ракеты и снаряды Скелета
      begin
        if Loud then
        begin
          if ShotType = WEAPON_SKEL_FIRE then
          begin // Взрыв снаряда Скелета
            if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_SKELFIRE') then
            begin
              Anim := TAnimation.Create(TextureID, False, 8);
              Anim.Blending := False;
              g_GFX_OnceAnim((Obj.X+32)-32, (Obj.Y+8)-32, Anim);
              Anim.Free();
            end;
          end
          else
          begin // Взрыв Ракеты
            if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_ROCKET') then
            begin
              Anim := TAnimation.Create(TextureID, False, 6);
              Anim.Blending := False;
              g_GFX_OnceAnim(cx-64, cy-64, Anim);
              Anim.Free();
            end;
          end;
          g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEROCKET', Obj.X, Obj.Y);
        end;
      end;

      WEAPON_PLASMA, WEAPON_BSP_FIRE: // Плазма, плазма Арахнатрона
      begin
        if ShotType = WEAPON_PLASMA then
          s := 'FRAMES_EXPLODE_PLASMA'
        else
          s := 'FRAMES_EXPLODE_BSPFIRE';
        
        if g_Frames_Get(TextureID, s) and loud then
        begin
          Anim := TAnimation.Create(TextureID, False, 3);
          Anim.Blending := False;
          g_GFX_OnceAnim(cx-16, cy-16, Anim);
          Anim.Free();

          g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEPLASMA', Obj.X, Obj.Y);
        end;
      end;

      WEAPON_BFG: // BFG
      begin
        // Взрыв BFG:
        if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_BFG') and Loud then
        begin
          Anim := TAnimation.Create(TextureID, False, 6);
          Anim.Blending := False;
          g_GFX_OnceAnim(cx-64, cy-64, Anim);
          Anim.Free();

          g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBFG', Obj.X, Obj.Y);
        end;
      end;

      WEAPON_IMP_FIRE, WEAPON_CACO_FIRE, WEAPON_BARON_FIRE: // Выстрелы Беса, Какодемона Рыцаря/Барона ада
      begin
        if ShotType = WEAPON_IMP_FIRE then
          s := 'FRAMES_EXPLODE_IMPFIRE'
        else
          if ShotType = WEAPON_CACO_FIRE then
            s := 'FRAMES_EXPLODE_CACOFIRE'
          else
            s := 'FRAMES_EXPLODE_BARONFIRE';

        if g_Frames_Get(TextureID, s) and Loud then
        begin
          Anim := TAnimation.Create(TextureID, False, 6);
          Anim.Blending := False;
          g_GFX_OnceAnim(cx-32, cy-32, Anim);
          Anim.Free();

          g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBALL', Obj.X, Obj.Y);
        end;
      end;

      WEAPON_MANCUB_FIRE: // Выстрел Манкубуса
      begin
        if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_ROCKET') and Loud then
        begin
          Anim := TAnimation.Create(TextureID, False, 6);
          Anim.Blending := False;
          g_GFX_OnceAnim(cx-64, cy-64, Anim);
          Anim.Free();

          g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBALL', Obj.X, Obj.Y);
        end;
      end;
    end; // case ShotType of...

    ShotType := 0;
    Animation.Free();
  end;
end;

end.
