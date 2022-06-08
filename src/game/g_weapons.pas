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
{$INCLUDE ../shared/a_modes.inc}
{.$DEFINE GWEP_HITSCAN_TRACE_BITMAP_CHECKER}
unit g_weapons;

interface

uses
  SysUtils, Classes, mempool,
  g_textures, g_basic, g_phys, xprofiler;


type
  TShot = record
    ShotType: Byte;
    Target: Word;
    SpawnerUID: Word;
    Triggers: DWArray;
    Obj: TObj;
    Animation: TAnimState;
    Timeout: DWORD;
    Stopped: Byte;

    procedure positionChanged (); //WARNING! call this after monster position was changed, or coldet will not work right!
  end;


var
  Shots: array of TShot = nil;
  LastShotID: Integer = 0;

procedure g_Weapon_LoadData();
procedure g_Weapon_FreeData();
procedure g_Weapon_Init();
procedure g_Weapon_Free();
function g_Weapon_Hit(obj: PObj; d: Integer; SpawnerUID: Word; t: Byte; HitCorpses: Boolean = True): Byte;
function g_Weapon_HitUID(UID: Word; d: Integer; SpawnerUID: Word; t: Byte): Boolean;
function g_Weapon_CreateShot(I: Integer; ShotType: Byte; Spawner, TargetUID: Word; X, Y, XV, YV: Integer): LongWord;

procedure g_Weapon_gun(const x, y, xd, yd, v, indmg: Integer; SpawnerUID: Word; CheckTrigger: Boolean);
procedure g_Weapon_punch(x, y: Integer; d, SpawnerUID: Word);
function g_Weapon_chainsaw(x, y: Integer; d, SpawnerUID: Word): Integer;
procedure g_Weapon_rocket(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1; Silent: Boolean = False; compat: Boolean = true);
procedure g_Weapon_revf(x, y, xd, yd: Integer; SpawnerUID, TargetUID: Word; WID: Integer = -1; Silent: Boolean = False);
procedure g_Weapon_flame(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1; Silent: Boolean = False; compat: Boolean = true);
procedure g_Weapon_plasma(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1; Silent: Boolean = False; compat: Boolean = true);
procedure g_Weapon_ball1(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1; Silent: Boolean = False; compat: Boolean = true);
procedure g_Weapon_ball2(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1; Silent: Boolean = False; compat: Boolean = true);
procedure g_Weapon_ball7(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1; Silent: Boolean = False; compat: Boolean = true);
procedure g_Weapon_aplasma(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1; Silent: Boolean = False; compat: Boolean = true);
procedure g_Weapon_manfire(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1; Silent: Boolean = False; compat: Boolean = true);
procedure g_Weapon_bfgshot(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1; Silent: Boolean = False; compat: Boolean = true);
procedure g_Weapon_bfghit(x, y: Integer);
procedure g_Weapon_pistol(x, y, xd, yd: Integer; SpawnerUID: Word; Silent: Boolean = False);
procedure g_Weapon_mgun(x, y, xd, yd: Integer; SpawnerUID: Word; Silent: Boolean = False);
procedure g_Weapon_shotgun(x, y, xd, yd: Integer; SpawnerUID: Word; Silent: Boolean = False);
procedure g_Weapon_dshotgun(x, y, xd, yd: Integer; SpawnerUID: Word; Silent: Boolean = False);

function g_Weapon_Explode(X, Y: Integer; rad: Integer; SpawnerUID: Word): Boolean;
procedure g_Weapon_BFG9000(X, Y: Integer; SpawnerUID: Word);
procedure g_Weapon_PreUpdate();
procedure g_Weapon_Update();
function g_Weapon_Danger(UID: Word; X, Y: Integer; Width, Height: Word; Time: Byte): Boolean;
procedure g_Weapon_DestroyShot(I: Integer; X, Y: Integer; Loud: Boolean = True);

procedure g_Weapon_SaveState (st: TStream);
procedure g_Weapon_LoadState (st: TStream);

procedure g_Weapon_AddDynLights();

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
  WEAPON_FLAMETHROWER   = 10;
  WEAPON_ZOMBY_PISTOL   = 20;
  WEAPON_IMP_FIRE       = 21;
  WEAPON_BSP_FIRE       = 22;
  WEAPON_CACO_FIRE      = 23;
  WEAPON_BARON_FIRE     = 24;
  WEAPON_MANCUB_FIRE    = 25;
  WEAPON_SKEL_FIRE      = 26;
  WEAPON_LAST           = WEAPON_SKEL_FIRE;

  WP_FIRST          = WEAPON_KASTET;
  WP_LAST           = WEAPON_FLAMETHROWER;

var
  gwep_debug_fast_trace: Boolean = true;


implementation

  uses
    {$IFDEF ENABLE_GFX}
      g_gfx,
    {$ENDIF}
    {$IFDEF ENABLE_GIBS}
      g_gibs,
    {$ENDIF}
    {$IFDEF ENABLE_CORPSES}
      g_corpses,
    {$ENDIF}
    Math, g_map, g_player, g_sound, g_panel,
    g_console, g_options, g_game,
    g_triggers, MAPDEF, e_log, g_monsters, g_saveload,
    g_language, g_netmsg, g_grid,
    geom, binheap, hashtable, utils, xstreams
  ;

type
  TWaterPanel = record
    X, Y: Integer;
    Width, Height: Word;
    Active: Boolean;
  end;

const
  SHOT_ROCKETLAUNCHER_WIDTH = 14;
  SHOT_ROCKETLAUNCHER_HEIGHT = 14;

  SHOT_SKELFIRE_WIDTH = 14;
  SHOT_SKELFIRE_HEIGHT = 14;

  SHOT_PLASMA_WIDTH = 16;
  SHOT_PLASMA_HEIGHT = 16;

  SHOT_BFG_WIDTH = 32;
  SHOT_BFG_HEIGHT = 32;
  SHOT_BFG_DAMAGE = 100;
  SHOT_BFG_RADIUS = 256;

  SHOT_FLAME_WIDTH = 4;
  SHOT_FLAME_HEIGHT = 4;
  SHOT_FLAME_LIFETIME = 180;

  SHOT_SIGNATURE = $544F4853; // 'SHOT'

type
  PHitTime = ^THitTime;
  THitTime = record
    distSq: Integer;
    mon: TMonster;
    plridx: Integer; // if mon=nil
    x, y: Integer;
  end;

  TBinHeapKeyHitTime = class
  public
    class function less (const a, b: Integer): Boolean; inline;
  end;

  // indicies in `wgunHitTime` array
  TBinaryHeapHitTimes = specialize TBinaryHeapBase<Integer, TBinHeapKeyHitTime>;

var
  WaterMap: array of array of DWORD = nil;
  //wgunMonHash: THashIntInt = nil;
  wgunHitHeap: TBinaryHeapHitTimes = nil;
  wgunHitTime: array of THitTime = nil;
  wgunHitTimeUsed: Integer = 0;


class function TBinHeapKeyHitTime.less (const a, b: Integer): Boolean;
var
  hta, htb: PHitTime;
begin
  hta := @wgunHitTime[a];
  htb := @wgunHitTime[b];
  if (hta.distSq <> htb.distSq) then begin result := (hta.distSq < htb.distSq); exit; end;
  if (hta.mon <> nil) then
  begin
    // a is monster
    if (htb.mon = nil) then begin result := false; exit; end; // players first
    result := (hta.mon.UID < htb.mon.UID); // why not?
  end
  else
  begin
    // a is player
    if (htb.mon <> nil) then begin result := true; exit; end; // players first
    result := (hta.plridx < htb.plridx); // why not?
  end;
end;


procedure appendHitTimeMon (adistSq: Integer; amon: TMonster; ax, ay: Integer);
begin
  if (wgunHitTimeUsed = Length(wgunHitTime)) then SetLength(wgunHitTime, wgunHitTimeUsed+128);
  with wgunHitTime[wgunHitTimeUsed] do
  begin
    distSq := adistSq;
    mon := amon;
    plridx := -1;
    x := ax;
    y := ay;
  end;
  wgunHitHeap.insert(wgunHitTimeUsed);
  Inc(wgunHitTimeUsed);
end;


procedure appendHitTimePlr (adistSq: Integer; aplridx: Integer; ax, ay: Integer);
begin
  if (wgunHitTimeUsed = Length(wgunHitTime)) then SetLength(wgunHitTime, wgunHitTimeUsed+128);
  with wgunHitTime[wgunHitTimeUsed] do
  begin
    distSq := adistSq;
    mon := nil;
    plridx := aplridx;
    x := ax;
    y := ay;
  end;
  wgunHitHeap.insert(wgunHitTimeUsed);
  Inc(wgunHitTimeUsed);
end;


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


var
  chkTrap_pl: array [0..256] of Integer;
  chkTrap_mn: array [0..65535] of TMonster;

procedure CheckTrap(ID: DWORD; dm: Integer; t: Byte);
var
  //a, b, c, d, i1, i2: Integer;
  //chkTrap_pl, chkTrap_mn: WArray;
  plaCount: Integer = 0;
  mnaCount: Integer = 0;
  frameId: DWord;

  {
  function monsWaterCheck (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    if mon.alive and mon.Collide(gWater[WaterMap[a][c]]) and (not InWArray(monidx, chkTrap_mn)) and (i2 < 1023) then //FIXME
    begin
      i2 += 1;
      chkTrap_mn[i2] := monidx;
    end;
  end;
  }

  function monsWaterCheck (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    if (mon.trapCheckFrameId <> frameId) then
    begin
      mon.trapCheckFrameId := frameId;
      chkTrap_mn[mnaCount] := mon;
      Inc(mnaCount);
    end;
  end;

var
  a, b, c, d, f: Integer;
  pan: TPanel;
begin
  if (gWater = nil) or (WaterMap = nil) then Exit;

  frameId := g_Mons_getNewTrapFrameId();

  //i1 := -1;
  //i2 := -1;

  //SetLength(chkTrap_pl, 1024);
  //SetLength(chkTrap_mn, 1024);
  //for d := 0 to 1023 do chkTrap_pl[d] := $FFFF;
  //for d := 0 to 1023 do chkTrap_mn[d] := $FFFF;

  for a := 0 to High(WaterMap) do
  begin
    for b := 0 to High(WaterMap[a]) do
    begin
      pan := gWater[WaterMap[a][b]];
      if not g_Obj_Collide(pan.X, pan.Y, pan.Width, pan.Height, @Shots[ID].Obj) then continue;

      for c := 0 to High(WaterMap[a]) do
      begin
        pan := gWater[WaterMap[a][c]];
        for d := 0 to High(gPlayers) do
        begin
          if (gPlayers[d] <> nil) and (gPlayers[d].alive) then
          begin
            if gPlayers[d].Collide(pan) then
            begin
              f := 0;
              while (f < plaCount) and (chkTrap_pl[f] <> d) do Inc(f);
              if (f = plaCount) then
              begin
                chkTrap_pl[plaCount] := d;
                Inc(plaCount);
                if (plaCount = Length(chkTrap_pl)) then break;
              end;
            end;
          end;
        end;

        //g_Mons_ForEach(monsWaterCheck);
        g_Mons_ForEachAliveAt(pan.X, pan.Y, pan.Width, pan.Height, monsWaterCheck);
      end;

      for f := 0 to plaCount-1 do gPlayers[chkTrap_pl[f]].Damage(dm, Shots[ID].SpawnerUID, 0, 0, t);
      for f := 0 to mnaCount-1 do chkTrap_mn[f].Damage(dm, 0, 0, Shots[ID].SpawnerUID, t);
    end;
  end;

  //chkTrap_pl := nil;
  //chkTrap_mn := nil;
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
    mon := g_Monsters_ByUID(SpawnerUID);
    if mon <> nil then
      mt := g_Monsters_ByUID(SpawnerUID).MonsterType
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
  begin
    if (t <> HIT_FLAME) or (m.FFireTime = 0) or (vx <> 0) or (vy <> 0) then
      Result := m.Damage(d, vx, vy, SpawnerUID, t)
    else
      Result := (gLMSRespawn = LMS_RESPAWN_NONE); // don't hit monsters when it's warmup time
    if t = HIT_FLAME then
      m.CatchFire(SpawnerUID);
  end
  else
    Result := (gLMSRespawn = LMS_RESPAWN_NONE); // don't hit monsters when it's warmup time
end;


function HitPlayer (p: TPlayer; d: Integer; vx, vy: Integer; SpawnerUID: Word; t: Byte): Boolean;
begin
  result := False;

  // Сам себя может ранить только ракетой и током
  if (p.UID = SpawnerUID) and (t <> HIT_ROCKET) and (t <> HIT_ELECTRO) then exit;

  if g_Game_IsServer then
  begin
    if (t <> HIT_FLAME) or (p.FFireTime = 0) or (vx <> 0) or (vy <> 0) then p.Damage(d, SpawnerUID, vx, vy, t);
    if (t = HIT_FLAME) then p.CatchFire(SpawnerUID);
  end;

  result := true;
end;


procedure g_Weapon_BFG9000(X, Y: Integer; SpawnerUID: Word);

  function monsCheck (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    if (mon.alive) and (mon.UID <> SpawnerUID) then
    begin
      with mon do
      begin
        if (g_PatchLength(X, Y, Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2),
                                Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)) <= SHOT_BFG_RADIUS) and
            g_TraceVector(X, Y, Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2),
                                Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)) then
        begin
          if HitMonster(mon, 50, 0, 0, SpawnerUID, HIT_SOME) then mon.BFGHit();
        end;
      end;
    end;
  end;

var
  i, h: Integer;
  st: Byte;
  pl: TPlayer;
  b: Boolean;
begin
  //g_Sound_PlayEx('SOUND_WEAPON_EXPLODEBFG', 255);

  {$IFDEF ENABLE_CORPSES}
    h := High(gCorpses);
    if gAdvCorpses and (h <> -1) then
    begin
      for i := 0 to h do
      begin
        if (gCorpses[i] <> nil) and (gCorpses[i].State <> CORPSE_STATE_REMOVEME) then
        begin
          with gCorpses[i] do
          begin
            if (g_PatchLength(X, Y, Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2),
                              Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)) <= SHOT_BFG_RADIUS) and
                g_TraceVector(X, Y, Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2),
                              Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)) then
            begin
              Damage(50, SpawnerUID, 0, 0);
              g_Weapon_BFGHit(Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2), Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2));
            end;
          end;
        end;
      end;
    end;
  {$ENDIF}

  st := TEAM_NONE;
  pl := g_Player_Get(SpawnerUID);
  if pl <> nil then
    st := pl.Team;

  h := High(gPlayers);

  if h <> -1 then
    for i := 0 to h do
      if (gPlayers[i] <> nil) and (gPlayers[i].alive) and (gPlayers[i].UID <> SpawnerUID) then
        with gPlayers[i] do
          if (g_PatchLength(X, Y, GameX+PLAYER_RECT.X+(PLAYER_RECT.Width div 2),
                            GameY+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)) <= SHOT_BFG_RADIUS) and
              g_TraceVector(X, Y, GameX+PLAYER_RECT.X+(PLAYER_RECT.Width div 2),
                            GameY+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)) then
          begin
            if (st = TEAM_NONE) or (st <> gPlayers[i].Team) then
              b := HitPlayer(gPlayers[i], 50, 0, 0, SpawnerUID, HIT_SOME)
            else
              b := HitPlayer(gPlayers[i], 25, 0, 0, SpawnerUID, HIT_SOME);
            if b then
              gPlayers[i].BFGHit();
          end;

  //FIXME
  g_Mons_ForEachAlive(monsCheck);
end;

function g_Weapon_CreateShot(I: Integer; ShotType: Byte; Spawner, TargetUID: Word; X, Y, XV, YV: Integer): LongWord;
var
  find_id: DWord;
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

        Triggers := nil;
        ShotType := WEAPON_ROCKETLAUNCHER;
        Animation.Invalidate;
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
        Animation := TAnimState.Create(True, 5, 2); // !!! put values into table
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
        Animation := TAnimState.Create(True, 6, 2); // !!! put values into table
      end;
    end;

    WEAPON_FLAMETHROWER:
    begin
      with Shots[find_id] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := SHOT_FLAME_WIDTH;
        Obj.Rect.Height := SHOT_FLAME_HEIGHT;

        Triggers := nil;
        ShotType := WEAPON_FLAMETHROWER;
        Animation.Invalidate;
        // Animation := TAnimState.Create(True, 6, 0); // drawed as gfx
      end;
    end;

    WEAPON_IMP_FIRE:
    begin
      with Shots[find_id] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := 16;
        Obj.Rect.Height := 16;

        Triggers := nil;
        ShotType := WEAPON_IMP_FIRE;
        Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
      end;
    end;

    WEAPON_CACO_FIRE:
    begin
      with Shots[find_id] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := 16;
        Obj.Rect.Height := 16;

        Triggers := nil;
        ShotType := WEAPON_CACO_FIRE;
        Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
      end;
    end;

    WEAPON_MANCUB_FIRE:
    begin
      with Shots[find_id] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := 32;
        Obj.Rect.Height := 32;

        Triggers := nil;
        ShotType := WEAPON_MANCUB_FIRE;
        Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
      end;
    end;

    WEAPON_BARON_FIRE:
    begin
      with Shots[find_id] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := 16;
        Obj.Rect.Height := 16;

        Triggers := nil;
        ShotType := WEAPON_BARON_FIRE;
        Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
      end;
    end;

    WEAPON_BSP_FIRE:
    begin
      with Shots[find_id] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := 16;
        Obj.Rect.Height := 16;

        Triggers := nil;
        ShotType := WEAPON_BSP_FIRE;
        Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
      end;
    end;

    WEAPON_SKEL_FIRE:
    begin
      with Shots[find_id] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := SHOT_SKELFIRE_WIDTH;
        Obj.Rect.Height := SHOT_SKELFIRE_HEIGHT;

        Triggers := nil;
        ShotType := WEAPON_SKEL_FIRE;
        target := TargetUID;
        Animation := TAnimState.Create(True, 5, 2); // !!! put values into table
      end;
    end;
  end;

  Shots[find_id].Obj.oldX := X;
  Shots[find_id].Obj.oldY := Y;
  Shots[find_id].Obj.X := X;
  Shots[find_id].Obj.Y := Y;
  Shots[find_id].Obj.Vel.X := XV;
  Shots[find_id].Obj.Vel.Y := YV;
  Shots[find_id].Obj.Accel.X := 0;
  Shots[find_id].Obj.Accel.Y := 0;
  Shots[find_id].SpawnerUID := Spawner;
  if (ShotType = WEAPON_FLAMETHROWER) and (XV = 0) and (YV = 0) then
    Shots[find_id].Stopped := 255
  else
    Shots[find_id].Stopped := 0;
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

  Shots[i].Obj.oldX := x;
  Shots[i].Obj.oldY := y;
  Shots[i].Obj.X := x;
  Shots[i].Obj.Y := y;
  Shots[i].Obj.Vel.X := (xd*s) div a;
  Shots[i].Obj.Vel.Y := (yd*s) div a;
  Shots[i].Obj.Accel.X := 0;
  Shots[i].Obj.Accel.Y := 0;
  Shots[i].Stopped := 0;
  if Shots[i].ShotType in [WEAPON_ROCKETLAUNCHER, WEAPON_BFG] then
    Shots[i].Timeout := 900 // ~25 sec
  else
  begin
    if Shots[i].ShotType = WEAPON_FLAMETHROWER then
      Shots[i].Timeout := SHOT_FLAME_LIFETIME
    else
      Shots[i].Timeout := 550; // ~15 sec
  end;
end;

function g_Weapon_Hit(obj: PObj; d: Integer; SpawnerUID: Word; t: Byte; HitCorpses: Boolean = True): Byte;
  {$IFDEF ENABLE_CORPSES}
    var i: Integer;
  {$ENDIF}
  var h: Integer;

  function PlayerHit(Team: Byte = 0): Boolean;
  var
    i: Integer;
    ChkTeam: Boolean;
    p: TPlayer;
  begin
    Result := False;
    h := High(gPlayers);

    if h <> -1 then
      for i := 0 to h do
        if (gPlayers[i] <> nil) and gPlayers[i].alive and g_Obj_Collide(obj, @gPlayers[i].Obj) then
        begin
          ChkTeam := True;
          if (Team > 0) and (g_GetUIDType(SpawnerUID) = UID_PLAYER) then
          begin
            p := g_Player_Get(SpawnerUID);
            if p <> nil then
              ChkTeam := (p.Team = gPlayers[i].Team) xor (Team = 2);
          end;
          if ChkTeam then
            if HitPlayer(gPlayers[i], d, obj^.Vel.X, obj^.Vel.Y, SpawnerUID, t) then
            begin
              if t <> HIT_FLAME then
                gPlayers[i].Push((obj^.Vel.X+obj^.Accel.X)*IfThen(t = HIT_BFG, 8, 1) div 4,
                                 (obj^.Vel.Y+obj^.Accel.Y)*IfThen(t = HIT_BFG, 8, 1) div 4);
              if t = HIT_BFG then
                g_Game_DelayEvent(DE_BFGHIT, 1000, SpawnerUID);
              Result := True;
              break;
            end;
        end;
  end;

  {
  function monsCheckHit (monidx: Integer; mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    if mon.alive and g_Obj_Collide(obj, @mon.Obj) then
    begin
      if HitMonster(mon, d, obj^.Vel.X, obj^.Vel.Y, SpawnerUID, t) then
      begin
        if (t <> HIT_FLAME) then
        begin
          mon.Push((obj^.Vel.X+obj^.Accel.X)*IfThen(t = HIT_BFG, 8, 1) div 4,
                              (obj^.Vel.Y+obj^.Accel.Y)*IfThen(t = HIT_BFG, 8, 1) div 4);
        end;
        result := True;
      end;
    end;
  end;
  }

  function monsCheckHit (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    if HitMonster(mon, d, obj.Vel.X, obj.Vel.Y, SpawnerUID, t) then
    begin
      if (t <> HIT_FLAME) then
      begin
        mon.Push((obj.Vel.X+obj.Accel.X)*IfThen(t = HIT_BFG, 8, 1) div 4,
                 (obj.Vel.Y+obj.Accel.Y)*IfThen(t = HIT_BFG, 8, 1) div 4);
      end;
      result := true;
    end;
  end;

  function MonsterHit(): Boolean;
  begin
    //result := g_Mons_ForEach(monsCheckHit);
    //FIXME: accelerate this!
    result := g_Mons_ForEachAliveAt(obj.X+obj.Rect.X, obj.Y+obj.Rect.Y, obj.Rect.Width, obj.Rect.Height, monsCheckHit);
  end;

begin
  Result := 0;

  {$IFDEF ENABLE_CORPSES}
    if HitCorpses then
    begin
      h := High(gCorpses);
      if gAdvCorpses and (h <> -1) then
      begin
        for i := 0 to h do
        begin
          if (gCorpses[i] <> nil) and (gCorpses[i].State <> CORPSE_STATE_REMOVEME) and
             g_Obj_Collide(obj, @gCorpses[i].Obj) then
          begin
            // Распиливаем труп:
            gCorpses[i].Damage(d, SpawnerUID, (obj^.Vel.X+obj^.Accel.X) div 4,
                                              (obj^.Vel.Y+obj^.Accel.Y) div 4);
            Result := 1;
          end;
        end;
      end;
    end;
  {$ENDIF}

  case gGameSettings.GameMode of
    // Кампания:
    GM_COOP, GM_SINGLE:
    begin
      // Сначала бьём монстров, если есть, потом пытаемся бить игроков
      if MonsterHit() then
      begin
        Result := 2;
        Exit;
      end;

      // И в конце игроков, но только если положено
      // (или снаряд от монстра, или friendlyfire, или friendly_hit_projectile)
      if (g_GetUIDType(SpawnerUID) <> UID_PLAYER) or
         LongBool(gGameSettings.Options and (GAME_OPTION_TEAMDAMAGE or GAME_OPTION_TEAMHITPROJECTILE)) then
      begin
        if PlayerHit() then
        begin
          Result := 1;
          Exit;
        end;
      end;
    end;

    // Дезматч:
    GM_DM:
    begin
      // Сначала бьём игроков, если есть, потом пытаемся бить монстров
      if PlayerHit() then
      begin
        Result := 1;
        Exit;
      end;

      if MonsterHit() then
      begin
        Result := 2;
        Exit;
      end;
    end;

    // Командные:
    GM_TDM, GM_CTF:
    begin
      // Сначала бьём игроков команды соперника
      if PlayerHit(2) then
      begin
        Result := 1;
        Exit;
      end;

      // Потом монстров
      if MonsterHit() then
      begin
        Result := 2;
        Exit;
      end;

      // И в конце своих игроков, но только если положено
      // (или friendlyfire, или friendly_hit_projectile)
      if LongBool(gGameSettings.Options and (GAME_OPTION_TEAMDAMAGE or GAME_OPTION_TEAMHITPROJECTILE)) then
      begin
        if PlayerHit(1) then
        begin
          Result := 1;
          Exit;
        end;
      end;
    end;

  end;
end;

function g_Weapon_HitUID(UID: Word; d: Integer; SpawnerUID: Word; t: Byte): Boolean;
begin
  Result := False;

  case g_GetUIDType(UID) of
    UID_PLAYER: Result := HitPlayer(g_Player_Get(UID), d, 0, 0, SpawnerUID, t);
    UID_MONSTER: Result := HitMonster(g_Monsters_ByUID(UID), d, 0, 0, SpawnerUID, t);
    else Exit;
  end;
end;

function g_Weapon_Explode(X, Y: Integer; rad: Integer; SpawnerUID: Word): Boolean;
var
  r: Integer; // squared radius

  function monsExCheck (mon: TMonster): Boolean;
  var
    dx, dy, mm: Integer;
  begin
    result := false; // don't stop
    begin
      dx := mon.Obj.X+mon.Obj.Rect.X+(mon.Obj.Rect.Width div 2)-X;
      dy := mon.Obj.Y+mon.Obj.Rect.Y+(mon.Obj.Rect.Height div 2)-Y;

      if dx > 1000 then dx := 1000;
      if dy > 1000 then dy := 1000;

      if (dx*dx+dy*dy < r) then
      begin
        //m := PointToRect(X, Y, Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y, Obj.Rect.Width, Obj.Rect.Height);
        //e_WriteLog(Format('explo monster #%d: x=%d; y=%d; rad=%d; dx=%d; dy=%d', [monidx, X, Y, rad, dx, dy]), MSG_NOTIFY);

        mm := Max(abs(dx), abs(dy));
        if mm = 0 then mm := 1;

        if mon.alive then
        begin
          HitMonster(mon, ((mon.Obj.Rect.Width div 4)*10*(rad-mm)) div rad, 0, 0, SpawnerUID, HIT_ROCKET);
        end;

        mon.Push((dx*7) div mm, (dy*7) div mm);
      end;
    end;
  end;

  var i, h, dx, dy, mm: Integer;
  {$IFDEF ENABLE_GIBS}
    var _angle: SmallInt;
  {$ENDIF}
  {$IF DEFINED(ENABLE_GIBS) OR DEFINED(ENABLE_CORPSES)}
    var m: Integer;
  {$ENDIF}
begin
  result := false;

  g_Triggers_PressC(X, Y, rad, SpawnerUID, ACTIVATE_SHOT);

  r := rad*rad;

  h := High(gPlayers);

  if h <> -1 then
    for i := 0 to h do
      if (gPlayers[i] <> nil) and gPlayers[i].alive then
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

  //g_Mons_ForEach(monsExCheck);
  g_Mons_ForEachAt(X-(rad+32), Y-(rad+32), (rad+32)*2, (rad+32)*2, monsExCheck);

  {$IFDEF ENABLE_CORPSES}
    h := High(gCorpses);
    if gAdvCorpses and (h <> -1) then
    begin
      for i := 0 to h do
      begin
        if (gCorpses[i] <> nil) and (gCorpses[i].State <> CORPSE_STATE_REMOVEME) then
        begin
          with gCorpses[i] do
          begin
            dx := Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2)-X;
            dy := Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)-Y;
            if dx > 1000 then dx := 1000;
            if dy > 1000 then dy := 1000;
            if dx*dx+dy*dy < r then
            begin
              m := PointToRect(X, Y, Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y, Obj.Rect.Width, Obj.Rect.Height);
              mm := Max(abs(dx), abs(dy));
              if mm = 0 then
                mm := 1;
              Damage(Round(100*(rad-m)/rad), SpawnerUID, (dx*10) div mm, (dy*10) div mm);
            end;
          end;
        end;
      end;
    end;
  {$ENDIF}

  {$IFDEF ENABLE_GIBS}
    h := High(gGibs);
    if gAdvGibs and (h <> -1) then
      for i := 0 to h do
        if gGibs[i].alive then
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
              positionChanged(); // this updates spatial accelerators
            end;
          end;
  {$ENDIF}
end;

procedure g_Weapon_Init();
begin
  CreateWaterMap();
end;

procedure g_Weapon_Free();
begin
  Shots := nil;
  WaterMap := nil;
end;

procedure g_Weapon_LoadData();
begin
  e_WriteLog('Loading weapons data...', TMsgType.Notify);

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
  g_Sound_CreateWADEx('SOUND_IGNITE', GameWAD+':SOUNDS\IGNITE');
  g_Sound_CreateWADEx('SOUND_WEAPON_STARTFIREBFG', GameWAD+':SOUNDS\STARTFIREBFG');
  g_Sound_CreateWADEx('SOUND_WEAPON_EXPLODEROCKET', GameWAD+':SOUNDS\EXPLODEROCKET');
  g_Sound_CreateWADEx('SOUND_WEAPON_EXPLODEBFG', GameWAD+':SOUNDS\EXPLODEBFG');
  g_Sound_CreateWADEx('SOUND_WEAPON_BFGWATER', GameWAD+':SOUNDS\BFGWATER');
  g_Sound_CreateWADEx('SOUND_WEAPON_EXPLODEPLASMA', GameWAD+':SOUNDS\EXPLODEPLASMA');
  g_Sound_CreateWADEx('SOUND_WEAPON_PLASMAWATER', GameWAD+':SOUNDS\PLASMAWATER');
  g_Sound_CreateWADEx('SOUND_WEAPON_FIREBALL', GameWAD+':SOUNDS\FIREBALL');
  g_Sound_CreateWADEx('SOUND_WEAPON_EXPLODEBALL', GameWAD+':SOUNDS\EXPLODEBALL');
  g_Sound_CreateWADEx('SOUND_WEAPON_FIREREV', GameWAD+':SOUNDS\FIREREV');
  g_Sound_CreateWADEx('SOUND_WEAPON_FLAMEON', GameWAD+':SOUNDS\STARTFLM');
  g_Sound_CreateWADEx('SOUND_WEAPON_FLAMEOFF', GameWAD+':SOUNDS\STOPFLM');
  g_Sound_CreateWADEx('SOUND_WEAPON_FLAMEWORK', GameWAD+':SOUNDS\WORKFLM');
  g_Sound_CreateWADEx('SOUND_PLAYER_JETFLY', GameWAD+':SOUNDS\WORKJETPACK');
  g_Sound_CreateWADEx('SOUND_PLAYER_JETON', GameWAD+':SOUNDS\STARTJETPACK');
  g_Sound_CreateWADEx('SOUND_PLAYER_JETOFF', GameWAD+':SOUNDS\STOPJETPACK');
  g_Sound_CreateWADEx('SOUND_PLAYER_CASING1', GameWAD+':SOUNDS\CASING1');
  g_Sound_CreateWADEx('SOUND_PLAYER_CASING2', GameWAD+':SOUNDS\CASING2');
  g_Sound_CreateWADEx('SOUND_PLAYER_SHELL1', GameWAD+':SOUNDS\SHELL1');
  g_Sound_CreateWADEx('SOUND_PLAYER_SHELL2', GameWAD+':SOUNDS\SHELL2');

  //wgunMonHash := hashNewIntInt();
  wgunHitHeap := TBinaryHeapHitTimes.Create();
end;

procedure g_Weapon_FreeData();
begin
  e_WriteLog('Releasing weapons data...', TMsgType.Notify);

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
  g_Sound_Delete('SOUND_IGNITE');
  g_Sound_Delete('SOUND_WEAPON_STARTFIREBFG');
  g_Sound_Delete('SOUND_WEAPON_EXPLODEROCKET');
  g_Sound_Delete('SOUND_WEAPON_EXPLODEBFG');
  g_Sound_Delete('SOUND_WEAPON_BFGWATER');
  g_Sound_Delete('SOUND_WEAPON_EXPLODEPLASMA');
  g_Sound_Delete('SOUND_WEAPON_PLASMAWATER');
  g_Sound_Delete('SOUND_WEAPON_FIREBALL');
  g_Sound_Delete('SOUND_WEAPON_EXPLODEBALL');
  g_Sound_Delete('SOUND_WEAPON_FIREREV');
  g_Sound_Delete('SOUND_WEAPON_FLAMEON');
  g_Sound_Delete('SOUND_WEAPON_FLAMEOFF');
  g_Sound_Delete('SOUND_WEAPON_FLAMEWORK');
  g_Sound_Delete('SOUND_PLAYER_JETFLY');
  g_Sound_Delete('SOUND_PLAYER_JETON');
  g_Sound_Delete('SOUND_PLAYER_JETOFF');
  g_Sound_Delete('SOUND_PLAYER_CASING1');
  g_Sound_Delete('SOUND_PLAYER_CASING2');
  g_Sound_Delete('SOUND_PLAYER_SHELL1');
  g_Sound_Delete('SOUND_PLAYER_SHELL2');
end;


function GunHitPlayer (X, Y: Integer; vx, vy: Integer; dmg: Integer; SpawnerUID: Word; AllowPush: Boolean): Boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to High(gPlayers) do
  begin
    if (gPlayers[i] <> nil) and gPlayers[i].alive and gPlayers[i].Collide(X, Y) then
    begin
      if HitPlayer(gPlayers[i], dmg, vx*10, vy*10-3, SpawnerUID, HIT_SOME) then
      begin
        if AllowPush then gPlayers[i].Push(vx, vy);
        result := true;
      end;
    end;
  end;
end;


function GunHit (X, Y: Integer; vx, vy: Integer; dmg: Integer; SpawnerUID: Word; AllowPush: Boolean): Byte;

  function monsCheck (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    if HitMonster(mon, dmg, vx*10, vy*10-3, SpawnerUID, HIT_SOME) then
    begin
      if AllowPush then mon.Push(vx, vy);
      result := true;
    end;
  end;

begin
  result := 0;
       if GunHitPlayer(X, Y, vx, vy, dmg, SpawnerUID, AllowPush) then result := 1
  else if g_Mons_ForEachAliveAt(X, Y, 1, 1, monsCheck) then result := 2;
end;


(*
procedure g_Weapon_gunOld(const x, y, xd, yd, v, dmg: Integer; SpawnerUID: Word; CheckTrigger: Boolean);
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
  {$IF DEFINED(D2F_DEBUG)}
  stt: UInt64;
  showTime: Boolean = true;
  {$ENDIF}
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

  {$IF DEFINED(D2F_DEBUG)}
  stt := getTimeMicro();
  {$ENDIF}

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
        {$IF DEFINED(D2F_DEBUG)}
        stt := getTimeMicro()-stt;
        e_WriteLog(Format('*** old trace time: %u microseconds', [LongWord(stt)]), MSG_NOTIFY);
        showTime := false;
        {$ENDIF}
        g_GFX_Spark(xx-xi, yy-yi, 2+Random(2), 180+a, 0, 0);
        if g_Game_IsServer and g_Game_IsNet then
          MH_SEND_Effect(xx-xi, yy-yi, 180+a, NET_GFX_SPARK);
      end;

    if not _collide then
    begin
      _collide := GunHit(xx, yy, xi*v, yi*v, dmg, SpawnerUID, v <> 0) <> 0;
    end;

    if _collide then Break;
  end;

  {$IF DEFINED(D2F_DEBUG)}
  if showTime then
  begin
    stt := getTimeMicro()-stt;
    e_WriteLog(Format('*** old trace time: %u microseconds', [LongWord(stt)]), MSG_NOTIFY);
  end;
  {$ENDIF}

  if CheckTrigger and g_Game_IsServer then
    g_Triggers_PressL(X, Y, xx-xi, yy-yi, SpawnerUID, ACTIVATE_SHOT);
end;
*)


//!!!FIXME!!!
procedure g_Weapon_gun (const x, y, xd, yd, v, indmg: Integer; SpawnerUID: Word; CheckTrigger: Boolean);
var
  x0, y0: Integer;
  x2, y2: Integer;
  xi, yi: Integer;
  wallDistSq: Integer = $3fffffff;
  spawnerPlr: TPlayer = nil;
  dmg: Integer;

  function doPlayerHit (idx: Integer; hx, hy: Integer): Boolean;
  begin
    result := false;
    if (idx < 0) or (idx > High(gPlayers)) then exit;
    if (gPlayers[idx] = nil) or not gPlayers[idx].alive then exit;
    if (spawnerPlr <> nil) then
    begin
      if ((gGameSettings.Options and (GAME_OPTION_TEAMHITTRACE or GAME_OPTION_TEAMDAMAGE)) = 0) and
         (spawnerPlr.Team <> TEAM_NONE) and (spawnerPlr.Team = gPlayers[idx].Team) then
      begin
        if (spawnerPlr <> gPlayers[idx]) and ((gGameSettings.Options and GAME_OPTION_TEAMABSORBDAMAGE) = 0) then
          dmg := Max(1, dmg div 2);
        exit;
      end;
    end;
    result := HitPlayer(gPlayers[idx], dmg, (xi*v)*10, (yi*v)*10-3, SpawnerUID, HIT_SOME);
    if result and (v <> 0) then gPlayers[idx].Push((xi*v), (yi*v));
    {$IF DEFINED(D2F_DEBUG)}
    //if result then e_WriteLog(Format('  PLAYER #%d HIT', [idx]), MSG_NOTIFY);
    {$ENDIF}
  end;

  function doMonsterHit (mon: TMonster; hx, hy: Integer): Boolean;
  begin
    result := false;
    if (mon = nil) then exit;
    result := HitMonster(mon, dmg, (xi*v)*10, (yi*v)*10-3, SpawnerUID, HIT_SOME);
    if result and (v <> 0) then mon.Push((xi*v), (yi*v));
    {$IF DEFINED(D2F_DEBUG)}
    //if result then e_WriteLog(Format('  MONSTER #%u HIT', [LongWord(mon.UID)]), MSG_NOTIFY);
    {$ENDIF}
  end;

  // collect players along hitray
  // return `true` if instant hit was detected
  function playerPossibleHit (): Boolean;
  var
    i: Integer;
    px, py, pw, ph: Integer;
    inx, iny: Integer;
    distSq: Integer;
    plr: TPlayer;
  begin
    result := false;
    for i := 0 to High(gPlayers) do
    begin
      plr := gPlayers[i];
      if (plr <> nil) and plr.alive then
      begin
        plr.getMapBox(px, py, pw, ph);
        if lineAABBIntersects(x, y, x2, y2, px, py, pw, ph, inx, iny) then
        begin
          distSq := distanceSq(x, y, inx, iny);
          if (distSq = 0) then
          begin
            // contains
            if doPlayerHit(i, x, y) then begin result := true; exit; end;
          end
          else if (distSq < wallDistSq) then
          begin
            appendHitTimePlr(distSq, i, inx, iny);
          end;
        end;
      end;
    end;
  end;

  procedure sqchecker (mon: TMonster);
  var
    mx, my, mw, mh: Integer;
    inx, iny: Integer;
    distSq: Integer;
  begin
    mon.getMapBox(mx, my, mw, mh);
    if lineAABBIntersects(x0, y0, x2, y2, mx, my, mw, mh, inx, iny) then
    begin
      distSq := distanceSq(x0, y0, inx, iny);
      if (distSq < wallDistSq) then appendHitTimeMon(distSq, mon, inx, iny);
    end;
  end;

var
  a: Integer;
  dx, dy: Integer;
  xe, ye: Integer;
  s, c: Extended;
  i: Integer;
  wallHitFlag: Boolean = false;
  wallHitX: Integer = 0;
  wallHitY: Integer = 0;
  didHit: Boolean = false;
  {$IF DEFINED(D2F_DEBUG)}
  stt: UInt64;
  {$ENDIF}
  mit: PMonster;
  it: TMonsterGrid.Iter;
begin
  (*
  if not gwep_debug_fast_trace then
  begin
    g_Weapon_gunOld(x, y, xd, yd, v, dmg, SpawnerUID, CheckTrigger);
    exit;
  end;
  *)

  if (xd = 0) and (yd = 0) then exit;

  if (g_GetUIDType(SpawnerUID) = UID_PLAYER) then
    spawnerPlr := g_Player_Get(SpawnerUID);

  dmg := indmg;

  //wgunMonHash.reset(); //FIXME: clear hash on level change
  wgunHitHeap.clear();
  wgunHitTimeUsed := 0;

  a := GetAngle(x, y, xd, yd)+180;

  SinCos(DegToRad(-a), s, c);

  if Abs(s) < 0.01 then s := 0;
  if Abs(c) < 0.01 then c := 0;

  x0 := x;
  y0 := y;
  x2 := x+Round(c*gMapInfo.Width);
  y2 := y+Round(s*gMapInfo.Width);

  dx := x2-x;
  dy := y2-y;

  if (dx > 0) then xi := 1 else if (dx < 0) then xi := -1 else xi := 0;
  if (dy > 0) then yi := 1 else if (dy < 0) then yi := -1 else yi := 0;

  {$IF DEFINED(D2F_DEBUG)}
  e_WriteLog(Format('GUN TRACE: (%d,%d) to (%d,%d)', [x, y, x2, y2]), TMsgType.Notify);
  stt := getTimeMicro();
  {$ENDIF}

  wallHitFlag := (g_Map_traceToNearestWall(x, y, x2, y2, @wallHitX, @wallHitY) <> nil);
  if wallHitFlag then
  begin
    x2 := wallHitX;
    y2 := wallHitY;
    wallDistSq := distanceSq(x, y, wallHitX, wallHitY);
  end
  else
  begin
    wallHitX := x2;
    wallHitY := y2;
  end;

  if playerPossibleHit() then exit; // instant hit

  // collect monsters
  //g_Mons_AlongLine(x, y, x2, y2, sqchecker);

  it := monsGrid.forEachAlongLine(x, y, x2, y2, -1);
  for mit in it do sqchecker(mit^);
  it.release();

  // here, we collected all monsters and players in `wgunHitHeap` and `wgunHitTime`
  // also, if `wallWasHit` is `true`, then `wallHitX` and `wallHitY` contains spark coords
  while (wgunHitHeap.count > 0) do
  begin
    // has some entities to check, do it
    i := wgunHitHeap.front;
    wgunHitHeap.popFront();
    // hitpoint
    xe := wgunHitTime[i].x;
    ye := wgunHitTime[i].y;
    // check if it is not behind the wall
    if (wgunHitTime[i].mon <> nil) then
    begin
      didHit := doMonsterHit(wgunHitTime[i].mon, xe, ye);
    end
    else
    begin
      didHit := doPlayerHit(wgunHitTime[i].plridx, xe, ye);
    end;
    if didHit then
    begin
      // need new coords for trigger
      wallHitX := xe;
      wallHitY := ye;
      wallHitFlag := false; // no sparks
      break;
    end;
  end;

  // need sparks?
  if wallHitFlag then
  begin
    {$IF DEFINED(D2F_DEBUG)}
    stt := getTimeMicro()-stt;
    e_WriteLog(Format('*** new trace time: %u microseconds', [LongWord(stt)]), TMsgType.Notify);
    {$ENDIF}
    {$IFDEF ENABLE_GFX}
      g_GFX_Spark(wallHitX, wallHitY, 2+Random(2), 180+a, 0, 0);
    {$ENDIF}
    if g_Game_IsServer and g_Game_IsNet then MH_SEND_Effect(wallHitX, wallHitY, 180+a, NET_GFX_SPARK);
  end
  else
  begin
    {$IF DEFINED(D2F_DEBUG)}
    stt := getTimeMicro()-stt;
    e_WriteLog(Format('*** new trace time: %u microseconds', [LongWord(stt)]), TMsgType.Notify);
    {$ENDIF}
  end;

  if CheckTrigger and g_Game_IsServer then g_Triggers_PressL(X, Y, wallHitX, wallHitY, SpawnerUID, ACTIVATE_SHOT);
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

procedure g_Weapon_rocket(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1;
  Silent: Boolean = False; compat: Boolean = true);
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

    if compat then
      dx := IfThen(xd > x, -Obj.Rect.Width, 0)
    else
      dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_ROCKETLAUNCHER;
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 12);

    Animation.Invalidate;
    triggers := nil;
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREROCKET', x, y);
end;

procedure g_Weapon_revf(x, y, xd, yd: Integer; SpawnerUID, TargetUID: Word;
  WID: Integer = -1; Silent: Boolean = False);
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

    Obj.Rect.Width := SHOT_SKELFIRE_WIDTH;
    Obj.Rect.Height := SHOT_SKELFIRE_HEIGHT;

    dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_SKEL_FIRE;
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 12);

    triggers := nil;
    target := TargetUID;
    Animation := TAnimState.Create(True, 5, 2); // !!! put values into table
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREREV', x, y);
end;

procedure g_Weapon_plasma(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1;
  Silent: Boolean = False; compat: Boolean = true);
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
      SetLength(Shots, find_id + 64);
  end;

  with Shots[find_id] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := SHOT_PLASMA_WIDTH;
    Obj.Rect.Height := SHOT_PLASMA_HEIGHT;

    if compat then
      dx := IfThen(xd > x, -Obj.Rect.Width, 0)
    else
      dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_PLASMA;
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    Animation := TAnimState.Create(True, 5, 2); // !!! put values into table
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREPLASMA', x, y);
end;

procedure g_Weapon_flame(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1;
  Silent: Boolean = False; compat: Boolean = true);
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
      SetLength(Shots, find_id + 64);
  end;

  with Shots[find_id] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := SHOT_FLAME_WIDTH;
    Obj.Rect.Height := SHOT_FLAME_HEIGHT;

    if compat then
      dx := IfThen(xd > x, -Obj.Rect.Width, 0)
    else
      dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_FLAMETHROWER;
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    Animation.Invalidate;
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  // if not Silent then
  //  g_Sound_PlayExAt('SOUND_WEAPON_FIREPLASMA', x, y);
end;

procedure g_Weapon_ball1(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1;
  Silent: Boolean = False; compat: Boolean = true);
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

    Obj.Rect.Width := 16;
    Obj.Rect.Height := 16;

    if compat then
      dx := IfThen(xd > x, -Obj.Rect.Width, 0)
    else
      dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_IMP_FIRE;
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREBALL', x, y);
end;

procedure g_Weapon_ball2(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1;
  Silent: Boolean = False; compat: Boolean = true);
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

    Obj.Rect.Width := 16;
    Obj.Rect.Height := 16;

    if compat then
      dx := IfThen(xd > x, -Obj.Rect.Width, 0)
    else
      dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_CACO_FIRE;
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREBALL', x, y);
end;

procedure g_Weapon_ball7(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1;
  Silent: Boolean = False; compat: Boolean = true);
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

    Obj.Rect.Width := 32;
    Obj.Rect.Height := 16;

    if compat then
      dx := IfThen(xd > x, -Obj.Rect.Width, 0)
    else
      dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_BARON_FIRE;
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREBALL', x, y);
end;

procedure g_Weapon_aplasma(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1;
  Silent: Boolean = False; compat: Boolean = true);
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

    Obj.Rect.Width := 16;
    Obj.Rect.Height := 16;

    if compat then
      dx := IfThen(xd > x, -Obj.Rect.Width, 0)
    else
      dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_BSP_FIRE;
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;

    Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREPLASMA', x, y);
end;

procedure g_Weapon_manfire(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1;
  Silent: Boolean = False; compat: Boolean = true);
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

    Obj.Rect.Width := 32;
    Obj.Rect.Height := 32;

    if compat then
      dx := IfThen(xd > x, -Obj.Rect.Width, 0)
    else
      dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_MANCUB_FIRE;
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;

    Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREBALL', x, y);
end;

procedure g_Weapon_bfgshot(x, y, xd, yd: Integer; SpawnerUID: Word; WID: Integer = -1;
  Silent: Boolean = False; compat: Boolean = true);
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

    Obj.Rect.Width := SHOT_BFG_WIDTH;
    Obj.Rect.Height := SHOT_BFG_HEIGHT;

    if compat then
      dx := IfThen(xd > x, -Obj.Rect.Width, 0)
    else
      dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_BFG;
    throw(find_id, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    Animation := TAnimState.Create(True, 6, 2); // !!! put values into table
  end;

  Shots[find_id].SpawnerUID := SpawnerUID;

  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREBFG', x, y);
end;

procedure g_Weapon_bfghit(x, y: Integer);
begin
  {$IFDEF ENABLE_GFX}
    g_GFX_QueueEffect(R_GFX_BFG_HIT, x - 32, y - 32);
  {$ENDIF}
end;

procedure g_Weapon_pistol(x, y, xd, yd: Integer; SpawnerUID: Word;
  Silent: Boolean = False);
begin
  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREPISTOL', x, y);

  g_Weapon_gun(x, y, xd, yd, 1, 3, SpawnerUID, True);
  if gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF] then
  begin
    if ABS(x-xd) >= ABS(y-yd) then
    begin
      g_Weapon_gun(x, y+1, xd, yd+1, 1, 3, SpawnerUID, False);
      g_Weapon_gun(x, y-1, xd, yd-1, 1, 2, SpawnerUID, False);
    end
    else
    begin
      g_Weapon_gun(x+1, y, xd+1, yd, 1, 3, SpawnerUID, False);
      g_Weapon_gun(x-1, y, xd-1, yd, 1, 2, SpawnerUID, False);
    end;
  end;
end;

procedure g_Weapon_mgun(x, y, xd, yd: Integer; SpawnerUID: Word;
  Silent: Boolean = False);
begin
  if not Silent then
    if gSoundEffectsDF then g_Sound_PlayExAt('SOUND_WEAPON_FIRECGUN', x, y);

  g_Weapon_gun(x, y, xd, yd, 1, 3, SpawnerUID, True);
  if (gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF]) and
     (g_GetUIDType(SpawnerUID) = UID_PLAYER) then
  begin
    if ABS(x-xd) >= ABS(y-yd) then
    begin
      g_Weapon_gun(x, y+1, xd, yd+1, 1, 2, SpawnerUID, False);
      g_Weapon_gun(x, y-1, xd, yd-1, 1, 2, SpawnerUID, False);
    end
    else
    begin
      g_Weapon_gun(x+1, y, xd+1, yd, 1, 2, SpawnerUID, False);
      g_Weapon_gun(x-1, y, xd-1, yd, 1, 2, SpawnerUID, False);
    end;
  end;
end;

procedure g_Weapon_shotgun(x, y, xd, yd: Integer; SpawnerUID: Word;
  Silent: Boolean = False);
var
  i, j, k: Integer;
begin
  if not Silent then
    if gSoundEffectsDF then g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN', x, y);

  for i := 0 to 9 do
  begin
    j := 0; k := 0;
    if ABS(x-xd) >= ABS(y-yd) then j := Random(17) - 8 else k := Random(17) - 8; // -8 .. 8
    g_Weapon_gun(x+k, y+j, xd+k, yd+j, IfThen(i mod 2 <> 0, 1, 0), 3, SpawnerUID, i=0);
  end;
end;

procedure g_Weapon_dshotgun(x, y, xd, yd: Integer; SpawnerUID: Word;
  Silent: Boolean = False);
var
  a, i, j, k: Integer;
begin
  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN2', x, y);

  if gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF] then a := 25 else a := 20;
  for i := 0 to a do
  begin
    j := 0; k := 0;
    if ABS(x-xd) >= ABS(y-yd) then j := Random(41) - 20 else k := Random(41) - 20; // -20 .. 20
    g_Weapon_gun(x+k, y+j, xd+k, yd+j, IfThen(i mod 3 <> 0, 0, 1), 3, SpawnerUID, i=0);
  end;
end;

procedure g_Weapon_PreUpdate();
var
  i: Integer;
begin
  if Shots = nil then Exit;
  for i := 0 to High(Shots) do
    if Shots[i].ShotType <> 0 then
    begin
      Shots[i].Obj.oldX := Shots[i].Obj.X;
      Shots[i].Obj.oldY := Shots[i].Obj.Y;
    end;
end;

procedure g_Weapon_Update();
var
  i, a, h, cx, cy, oldvx, oldvy, tf: Integer;
  t: DWArray;
  st: Word;
  o: TObj;
  spl: Boolean;
  Loud: Boolean;
  {$IFDEF ENABLE_GFX}
    var tcx, tcy: Integer;
  {$ENDIF}
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
      if (Stopped = 0) and g_Game_IsServer then
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
      if Animation.IsValid() then
        Animation.Update();

    // Движение:
      spl := (ShotType <> WEAPON_PLASMA) and
             (ShotType <> WEAPON_BFG) and
             (ShotType <> WEAPON_BSP_FIRE) and
             (ShotType <> WEAPON_FLAMETHROWER);

      if Stopped = 0 then
      begin
        st := g_Obj_Move_Projectile(@Obj, False, spl);
      end
      else
      begin
        st := 0;
      end;
      positionChanged(); // this updates spatial accelerators

      if WordBool(st and MOVE_FALLOUT) or (Obj.X < -1000) or
        (Obj.X > gMapInfo.Width+1000) or (Obj.Y < -1000) then
      begin
        // На клиенте скорее всего и так уже выпал.
        ShotType := 0;
        Animation.Invalidate();
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
            begin
              {$IFDEF ENABLE_GFX}
                g_GFX_Bubbles(cx, cy, 1+Random(3), 16, 16);
              {$ENDIF}
              if Random(2) = 0
                then g_Sound_PlayExAt('SOUND_GAME_BUBBLE1', cx, cy)
                else g_Sound_PlayExAt('SOUND_GAME_BUBBLE2', cx, cy);
            end
            else
            begin
              {$IFDEF ENABLE_GFX}
                g_GFX_QueueEffect(R_GFX_SMOKE_TRANS, Obj.X-14+Random(9), cy-20+Random(9));
              {$ENDIF}
            end;

          // Попали в кого-то или в стену:
            if WordBool(st and (MOVE_HITWALL or MOVE_HITLAND or MOVE_HITCEIL)) or
               (g_Weapon_Hit(@Obj, 10, SpawnerUID, HIT_SOME, False) <> 0) or
               (Timeout < 1) then
            begin
              Obj.Vel.X := 0;
              Obj.Vel.Y := 0;

              g_Weapon_Explode(cx, cy, 60, SpawnerUID);

              if ShotType = WEAPON_SKEL_FIRE then
              begin // Взрыв снаряда Скелета
                {$IFDEF ENABLE_GFX}
                  g_GFX_QueueEffect(R_GFX_EXPLODE_SKELFIRE, Obj.X + 32 - 58, Obj.Y + 8 - 36);
                  g_DynLightExplosion((Obj.X+32), (Obj.Y+8), 64, 1, 0, 0);
                {$ENDIF}
              end
              else
              begin // Взрыв Ракеты
                {$IFDEF ENABLE_GFX}
                  g_GFX_QueueEffect(R_GFX_EXPLODE_ROCKET, cx - 64, cy - 64);
                  g_DynLightExplosion(cx, cy, 64, 1, 0, 0);
                {$ENDIF}
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
               (g_Weapon_Hit(@Obj, a, SpawnerUID, HIT_SOME, False) <> 0) or
               (Timeout < 1) then
            begin
              {$IFDEF ENABLE_GFX}
                if ShotType = WEAPON_PLASMA then
                  g_GFX_QueueEffect(R_GFX_EXPLODE_PLASMA, cx - 16, cy - 16)
                else
                  g_GFX_QueueEffect(R_GFX_EXPLODE_BSPFIRE, cx - 16, cy - 16);
                g_DynLightExplosion(cx, cy, 32, 0, 0.5, 0.5);
              {$ENDIF}
              g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEPLASMA', Obj.X, Obj.Y);
              ShotType := 0;
            end;
          end;

        WEAPON_FLAMETHROWER: // Огнемет
          begin
          // Со временем умирает
            if (Timeout < 1) then
            begin
              ShotType := 0;
              Continue;
            end;
          // Под водой тоже
            if WordBool(st and (MOVE_HITWATER or MOVE_INWATER)) then
            begin
              if WordBool(st and MOVE_HITWATER) then
              begin
                {$IFDEF ENABLE_GFX}
                  tcx := Random(8);
                  tcy := Random(8);
                  g_GFX_QueueEffect(R_GFX_SMOKE, cx-4+tcx-(R_GFX_SMOKE_WIDTH div 2), cy-4+tcy-(R_GFX_SMOKE_HEIGHT div 2));
                {$ENDIF}
              end
              else
              begin
                {$IFDEF ENABLE_GFX}
                  g_GFX_Bubbles(cx, cy, 1+Random(3), 16, 16);
                {$ENDIF}
                if Random(2) = 0
                  then g_Sound_PlayExAt('SOUND_GAME_BUBBLE1', cx, cy)
                  else g_Sound_PlayExAt('SOUND_GAME_BUBBLE2', cx, cy);
              end;
              ShotType := 0;
              Continue;
            end;

          // Гравитация
            if Stopped = 0 then
              Obj.Accel.Y := Obj.Accel.Y + 1;
          // Попали в стену или в воду:
            if WordBool(st and (MOVE_HITWALL or MOVE_HITLAND or MOVE_HITCEIL or MOVE_HITWATER)) then
            begin
            // Прилипаем:
              Obj.Vel.X := 0;
              Obj.Vel.Y := 0;
              Obj.Accel.Y := 0;
              if WordBool(st and MOVE_HITWALL) then
                Stopped := MOVE_HITWALL
              else if WordBool(st and MOVE_HITLAND) then
                Stopped := MOVE_HITLAND
              else if WordBool(st and MOVE_HITCEIL) then
                Stopped := MOVE_HITCEIL;
            end;

            a := IfThen(Stopped = 0, 10, 1);
          // Если в кого-то попали
            if g_Weapon_Hit(@Obj, a, SpawnerUID, HIT_FLAME, False) <> 0 then
            begin
            // HIT_FLAME сам подожжет
            // Если в полете попали, исчезаем
              if Stopped = 0 then
                ShotType := 0;
            end;

            if Stopped = 0 then
              tf := 2
            else
              tf := 3;

            if (gTime mod LongWord(tf) = 0) then
            begin
              {$IFDEF ENABLE_GFX}
                case Stopped of
                  MOVE_HITWALL: begin tcx := cx-4+Random(8); tcy := cy-12+Random(24); end;
                  MOVE_HITLAND: begin tcx := cx-12+Random(24); tcy := cy-10+Random(8); end;
                  MOVE_HITCEIL: begin tcx := cx-12+Random(24); tcy := cy+6+Random(8); end;
                  else begin tcx := cx-4+Random(8); tcy := cy-4+Random(8); end;
                end;
                g_GFX_QueueEffect(R_GFX_FLAME_RAND, tcx - (R_GFX_FLAME_WIDTH div 2), tcy - (R_GFX_FLAME_HEIGHT div 2));
                //g_DynLightExplosion(tcx, tcy, 1, 1, 0.8, 0.3);
              {$ENDIF}
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
               (g_Weapon_Hit(@Obj, SHOT_BFG_DAMAGE, SpawnerUID, HIT_BFG, False) <> 0) or
               (Timeout < 1) then
            begin
            // Лучи BFG:
              if g_Game_IsServer then g_Weapon_BFG9000(cx, cy, SpawnerUID);
              {$IFDEF ENABLE_GFX}
                g_GFX_QueueEffect(R_GFX_EXPLODE_BFG, cx - 64, cy - 64);
                g_DynLightExplosion(cx, cy, 96, 0, 1, 0);
              {$ENDIF}
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
              {$IFDEF ENABLE_GFX}
                case ShotType of
                  WEAPON_IMP_FIRE: g_GFX_QueueEffect(R_GFX_EXPLODE_IMPFIRE, cx - 32, cy - 32);
                  WEAPON_CACO_FIRE: g_GFX_QueueEffect(R_GFX_EXPLODE_CACOFIRE, cx - 32, cy - 32);
                  WEAPON_BARON_FIRE: g_GFX_QueueEffect(R_GFX_EXPLODE_BARONFIRE, cx - 32, cy - 32);
                end;
              {$ENDIF}
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
               (g_Weapon_Hit(@Obj, 40, SpawnerUID, HIT_SOME, False) <> 0) or
               (Timeout < 1) then
            begin
              // Взрыв:
              {$IFDEF ENABLE_GFX}
                g_GFX_QueueEffect(R_GFX_EXPLODE_ROCKET, cx - 64, cy - 64);
              {$ENDIF}
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
        Animation.Invalidate;
      end
      else if (ShotType <> WEAPON_FLAMETHROWER) and ((oldvx <> Obj.Vel.X) or (oldvy <> Obj.Vel.Y)) then
        if gGameSettings.GameType = GT_SERVER then
          MH_SEND_UpdateShot(i);
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

procedure g_Weapon_SaveState (st: TStream);
var
  count, i, j: Integer;
begin
  // Считаем количество существующих снарядов
  count := 0;
  for i := 0 to High(Shots) do if (Shots[i].ShotType <> 0) then Inc(count);

  // Количество снарядов
  utils.WriteInt(st, count);

  if (count = 0) then exit;

  for i := 0 to High(Shots) do
  begin
    if Shots[i].ShotType <> 0 then
    begin
      // Сигнатура снаряда
      utils.writeSign(st, 'SHOT');
      utils.writeInt(st, Byte(0)); // version
      // Тип снаряда
      utils.writeInt(st, Byte(Shots[i].ShotType));
      // Цель
      utils.writeInt(st, Word(Shots[i].Target));
      // UID стрелявшего
      utils.writeInt(st, Word(Shots[i].SpawnerUID));
      // Размер поля Triggers
      utils.writeInt(st, Integer(Length(Shots[i].Triggers)));
      // Триггеры, активированные выстрелом
      for j := 0 to Length(Shots[i].Triggers)-1 do utils.writeInt(st, LongWord(Shots[i].Triggers[j]));
      // Объект снаряда
      Obj_SaveState(st, @Shots[i].Obj);
      // Костылина ебаная
      utils.writeInt(st, Byte(Shots[i].Stopped));
    end;
  end;
end;

procedure g_Weapon_LoadState (st: TStream);
var
  count, tc, i, j: Integer;
begin
  if (st = nil) then exit;

  // Количество снарядов
  count := utils.readLongInt(st);
  if (count < 0) or (count > 1024*1024) then raise XStreamError.Create('invalid shots counter');

  SetLength(Shots, count);

  if (count = 0) then exit;

  for i := 0 to count-1 do
  begin
    // Сигнатура снаряда
    if not utils.checkSign(st, 'SHOT') then raise XStreamError.Create('invalid shot signature');
    if (utils.readByte(st) <> 0) then raise XStreamError.Create('invalid shot version');
    // Тип снаряда:
    Shots[i].ShotType := utils.readByte(st);
    // Цель
    Shots[i].Target := utils.readWord(st);
    // UID стрелявшего
    Shots[i].SpawnerUID := utils.readWord(st);
    // Размер поля Triggers
    tc := utils.readLongInt(st);
    if (tc < 0) or (tc > 1024*1024) then raise XStreamError.Create('invalid shot triggers counter');
    SetLength(Shots[i].Triggers, tc);
    // Триггеры, активированные выстрелом
    for j := 0 to tc-1 do Shots[i].Triggers[j] := utils.readLongWord(st);
    // Объект предмета
    Obj_LoadState(@Shots[i].Obj, st);
    // Костылина ебаная
    Shots[i].Stopped := utils.readByte(st);

    // Установка текстуры или анимации
    Shots[i].Animation.Invalidate;

    case Shots[i].ShotType of
      WEAPON_ROCKETLAUNCHER, WEAPON_SKEL_FIRE:
        begin
        end;
      WEAPON_PLASMA:
        begin
          Shots[i].Animation := TAnimState.Create(True, 5, 2); // !!! put values into table
        end;
      WEAPON_BFG:
        begin
          Shots[i].Animation := TAnimState.Create(True, 6, 2); // !!! put values into table
        end;
      WEAPON_IMP_FIRE:
        begin
          Shots[i].Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
        end;
      WEAPON_BSP_FIRE:
        begin
          Shots[i].Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
        end;
      WEAPON_CACO_FIRE:
        begin
          Shots[i].Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
        end;
      WEAPON_BARON_FIRE:
        begin
          Shots[i].Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
        end;
      WEAPON_MANCUB_FIRE:
        begin
          Shots[i].Animation := TAnimState.Create(True, 4, 2); // !!! put values into table
        end;
    end;
  end;
end;

procedure g_Weapon_DestroyShot(I: Integer; X, Y: Integer; Loud: Boolean = True);
  {$IFDEF ENABLE_GFX}
    var cx, cy: Integer;
  {$ENDIF}
begin
  if Shots = nil then
    Exit;
  if (I > High(Shots)) or (I < 0) then Exit;

  with Shots[I] do
  begin
    if ShotType = 0 then Exit;
    Obj.X := X;
    Obj.Y := Y;
    {$IFDEF ENABLE_GFX}
      cx := Obj.X + (Obj.Rect.Width div 2);
      cy := Obj.Y + (Obj.Rect.Height div 2);
    {$ENDIF}

    case ShotType of
      WEAPON_ROCKETLAUNCHER, WEAPON_SKEL_FIRE: // Ракеты и снаряды Скелета
      begin
        if Loud then
        begin
          {$IFDEF ENABLE_GFX}
            if ShotType = WEAPON_SKEL_FIRE then
              g_GFX_QueueEffect(R_GFX_EXPLODE_SKELFIRE, (Obj.X + 32) - 32, (Obj.Y + 8) - 32)
            else
              g_GFX_QueueEffect(R_GFX_EXPLODE_ROCKET, cx - 64, cy - 64);
          {$ENDIF}
          g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEROCKET', Obj.X, Obj.Y);
        end;
      end;

      WEAPON_PLASMA, WEAPON_BSP_FIRE: // Плазма, плазма Арахнатрона
      begin
        if loud then
        begin
          {$IFDEF ENABLE_GFX}
            if ShotType = WEAPON_PLASMA then
              g_GFX_QueueEffect(R_GFX_EXPLODE_PLASMA, cx - 16, cy - 16)
            else
              g_GFX_QueueEffect(R_GFX_EXPLODE_BSPFIRE, cx - 16, cy - 16);
          {$ENDIF}
          g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEPLASMA', Obj.X, Obj.Y);
        end;
      end;

      WEAPON_BFG: // BFG
      begin
        {$IFDEF ENABLE_GFX}
          g_GFX_QueueEffect(R_GFX_EXPLODE_BFG, cx - 64, cy - 64);
        {$ENDIF}
        g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBFG', Obj.X, Obj.Y);
      end;

      WEAPON_IMP_FIRE, WEAPON_CACO_FIRE, WEAPON_BARON_FIRE: // Выстрелы Беса, Какодемона Рыцаря/Барона ада
      begin
        if loud then
        begin
          {$IFDEF ENABLE_GFX}
            case ShotType of
              WEAPON_IMP_FIRE: g_GFX_QueueEffect(R_GFX_EXPLODE_IMPFIRE, cx - 32, cy - 32);
              WEAPON_CACO_FIRE: g_GFX_QueueEffect(R_GFX_EXPLODE_CACOFIRE, cx - 32, cy - 32);
              WEAPON_BARON_FIRE: g_GFX_QueueEffect(R_GFX_EXPLODE_BARONFIRE, cx - 32, cy - 32);
            end;
          {$ENDIF}
          g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBALL', Obj.X, Obj.Y);
        end;
      end;

      WEAPON_MANCUB_FIRE: // Выстрел Манкубуса
      begin
        {$IFDEF ENABLE_GFX}
          g_GFX_QueueEffect(R_GFX_EXPLODE_ROCKET, cx - 64, cy - 64);
        {$ENDIF}
        g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBALL', Obj.X, Obj.Y);
      end;
    end; // case ShotType of...

    ShotType := 0;
    Animation.Invalidate;
  end;
end;


procedure g_Weapon_AddDynLights();
var
  i: Integer;
begin
  if Shots = nil then Exit;
  for i := 0 to High(Shots) do
  begin
    if Shots[i].ShotType = 0 then continue;
    if (Shots[i].ShotType = WEAPON_ROCKETLAUNCHER) or
       (Shots[i].ShotType = WEAPON_BARON_FIRE) or
       (Shots[i].ShotType = WEAPON_MANCUB_FIRE) or
       (Shots[i].ShotType = WEAPON_SKEL_FIRE) or
       (Shots[i].ShotType = WEAPON_IMP_FIRE) or
       (Shots[i].ShotType = WEAPON_CACO_FIRE) or
       (Shots[i].ShotType = WEAPON_MANCUB_FIRE) or
       (Shots[i].ShotType = WEAPON_BSP_FIRE) or
       (Shots[i].ShotType = WEAPON_PLASMA) or
       (Shots[i].ShotType = WEAPON_BFG) or
       (Shots[i].ShotType = WEAPON_FLAMETHROWER) or
       false then
    begin
      if (Shots[i].ShotType = WEAPON_PLASMA) then
        g_AddDynLight(Shots[i].Obj.X+(Shots[i].Obj.Rect.Width div 2), Shots[i].Obj.Y+(Shots[i].Obj.Rect.Height div 2), 128,  0, 0.3, 1, 0.4)
      else if (Shots[i].ShotType = WEAPON_BFG) then
        g_AddDynLight(Shots[i].Obj.X+(Shots[i].Obj.Rect.Width div 2), Shots[i].Obj.Y+(Shots[i].Obj.Rect.Height div 2), 128,  0, 1, 0, 0.5)
      else if (Shots[i].ShotType = WEAPON_FLAMETHROWER) then
        g_AddDynLight(Shots[i].Obj.X+(Shots[i].Obj.Rect.Width div 2), Shots[i].Obj.Y+(Shots[i].Obj.Rect.Height div 2), 42,  1, 0.8, 0, 0.4)
      else
        g_AddDynLight(Shots[i].Obj.X+(Shots[i].Obj.Rect.Width div 2), Shots[i].Obj.Y+(Shots[i].Obj.Rect.Height div 2), 128,  1, 0, 0, 0.4);
    end;
  end;
end;


procedure TShot.positionChanged (); begin end;


end.
