(* Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
 * This file is part of Doom2D Forever.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation, version 3 of
 * the License ONLY.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 *)

{$INCLUDE ../shared/a_modes.inc}
unit g_weapons;

interface

uses
  SysUtils, Classes, mempool,
  g_textures, g_basic, e_graphics, g_phys, xprofiler;


type
  TProjectile = record
    ShotType: Byte;
    Target: Word;
    SpawnerUID: Word;
    Triggers: DWArray;
    Obj: TObj;
    Animation: TAnimation;
    TextureID: DWORD;
    Timeout: DWORD;
    Stopped: Byte;

    procedure positionChanged ();  // WARNING! call this after monster position was changed, or coldet will not work right!
  end;


var
  Projectiles: array of TProjectile;

procedure g_Weapon_LoadData();
procedure g_Weapon_FreeData();
procedure g_Weapon_Init();
procedure g_Weapon_Free();
function g_Weapon_Hit(obj: PObj; d: Integer; SpawnerUID: Word; t: Byte; HitCorpses: Boolean = True): Byte;
function g_Weapon_HitUID(UID: Word; d: Integer; SpawnerUID: Word; t: Byte): Boolean;
function g_Weapon_CreateProj(I: SizeInt; ShotType: Byte; Spawner, TargetUID: Word; X, Y, XV, YV: Integer): SizeInt;

procedure g_Weapon_gun(const x, y, xd, yd, v, indmg: Integer; SpawnerUID: Word; CheckTrigger: Boolean);
procedure g_Weapon_punch(x, y: Integer; d, SpawnerUID: Word);
function g_Weapon_chainsaw(x, y: Integer; d, SpawnerUID: Word): Integer;
function g_Weapon_rocket(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt = -1; Silent: Boolean = False; compat: Boolean = True): SizeInt;
function g_Weapon_revf(x, y, xd, yd: Integer; SpawnerUID, TargetUID: Word; WID: SizeInt = -1; Silent: Boolean = False): SizeInt;
function g_Weapon_flame(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt = -1; Silent: Boolean = False; compat: Boolean = True): SizeInt;
function g_Weapon_plasma(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt = -1; Silent: Boolean = False; compat: Boolean = True): SizeInt;
function g_Weapon_ball1(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt = -1; Silent: Boolean = False; compat: Boolean = True): SizeInt;
function g_Weapon_ball2(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt = -1; Silent: Boolean = False; compat: Boolean = True): SizeInt;
function g_Weapon_ball7(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt = -1; Silent: Boolean = False; compat: Boolean = True): SizeInt;
function g_Weapon_aplasma(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt = -1; Silent: Boolean = False; compat: Boolean = True): SizeInt;
function g_Weapon_manfire(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt = -1; Silent: Boolean = False; compat: Boolean = True): SizeInt;
function g_Weapon_bfgshot(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt = -1; Silent: Boolean = False; compat: Boolean = True): SizeInt;
procedure g_Weapon_bfghit(x, y: Integer);
procedure g_Weapon_pistol(x, y, xd, yd: Integer; SpawnerUID: Word; Silent: Boolean = False);
procedure g_Weapon_mgun(x, y, xd, yd: Integer; SpawnerUID: Word; Silent: Boolean = False);
procedure g_Weapon_shotgun(x, y, xd, yd: Integer; SpawnerUID: Word; Silent: Boolean = False);
procedure g_Weapon_dshotgun(x, y, xd, yd: Integer; SpawnerUID: Word; Silent: Boolean = False);

function g_Weapon_Explode(X, Y: Integer; rad: Integer; SpawnerUID: Word): Boolean;
procedure g_Weapon_BFG9000(X, Y: Integer; SpawnerUID: Word);
procedure g_Weapon_PreUpdate();
procedure g_Weapon_Update();
procedure g_Weapon_Draw();
function g_Weapon_Danger(UID: Word; X, Y: Integer; Width, Height: Word; Time: Byte): Boolean;
procedure g_Weapon_DestroyProj(I: Integer; X, Y: Integer; Loud: Boolean = True);

procedure g_Weapon_SaveState (st: TStream);
procedure g_Weapon_LoadState (st: TStream);

procedure g_Weapon_AddDynLights();

const
  WEAPON_IRONFIST       = 0;
  WEAPON_SAW            = 1;
  WEAPON_PISTOL         = 2;
  WEAPON_SHOTGUN1       = 3;
  WEAPON_SHOTGUN2       = 4;
  WEAPON_CHAINGUN       = 5;
  WEAPON_ROCKETLAUNCHER = 6;
  WEAPON_PLASMA         = 7;
  WEAPON_BFG            = 8;
  WEAPON_SUPERCHAINGUN  = 9;
  WEAPON_FLAMETHROWER   = 10;
  WEAPON_ZOMBY_PISTOL   = 20;
  WEAPON_IMP_FIRE       = 21;
  WEAPON_BSP_FIRE       = 22;
  WEAPON_CACO_FIRE      = 23;
  WEAPON_BARON_FIRE     = 24;
  WEAPON_MANCUB_FIRE    = 25;
  WEAPON_SKEL_FIRE      = 26;

  WP_FIRST          = WEAPON_IRONFIST;
  WP_LAST           = WEAPON_FLAMETHROWER;


var
  gwep_debug_fast_trace: Boolean = true;


implementation

uses
{$IFDEF ENABLE_SOUND}
  g_sound,
{$ENDIF}
  Math, g_map, g_player, g_gfx, g_main, g_panel,
  g_console, g_options, g_game,
  g_triggers, MAPDEF, e_log, g_monsters, g_saveload,
  g_language, g_netmsg, g_grid,
  geom, binheap, hashtable, utils, xstreams;

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
  WaterMap: array of array of DWORD;
  //wgunMonHash: THashIntInt;
  wgunHitHeap: TBinaryHeapHitTimes;
  wgunHitTime: array of THitTime;
  wgunHitTimeUsed: Integer;


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


function FindProjectileSlot(): SizeInt;
begin
  for Result := 0 to High(Projectiles) do
    if Projectiles[Result].ShotType = 0 then
      Exit;

  if Projectiles = nil
    then Result := 0
    else Result := Length(Projectiles);

  SetLength(Projectiles, Result + 128);
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
      if not g_Obj_Collide(pan.X, pan.Y, pan.Width, pan.Height, @Projectiles[ID].Obj) then continue;

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
              while (f < plaCount) and (chkTrap_pl[f] <> d) do f += 1;
              if (f = plaCount) then
              begin
                chkTrap_pl[plaCount] := d;
                plaCount += 1;
                if (plaCount = Length(chkTrap_pl)) then break;
              end;
            end;
          end;
        end;

        //g_Mons_ForEach(monsWaterCheck);
        g_Mons_ForEachAliveAt(pan.X, pan.Y, pan.Width, pan.Height, monsWaterCheck);
      end;

      for f := 0 to plaCount-1 do
        gPlayers[chkTrap_pl[f]].Damage(dm, Projectiles[ID].SpawnerUID, 0, 0, t);
      for f := 0 to mnaCount-1 do
        chkTrap_mn[f].Damage(dm, 0, 0, Projectiles[ID].SpawnerUID, t);
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

  h := High(gCorpses);

  if gAdvCorpses and (h <> -1) then
    for i := 0 to h do
      if (gCorpses[i] <> nil) and (gCorpses[i].State <> CORPSE_STATE_REMOVEME) then
        with gCorpses[i] do
          if (g_PatchLength(X, Y, Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2),
                            Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)) <= SHOT_BFG_RADIUS) and
              g_TraceVector(X, Y, Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2),
                            Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)) then
          begin
            Damage(50, SpawnerUID, 0, 0);
            g_Weapon_BFGHit(Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2),
                            Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2));
          end;

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

function g_Weapon_CreateProj(I: SizeInt; ShotType: Byte; Spawner, TargetUID: Word; X, Y, XV, YV: Integer): SizeInt;
var
  FramesID: DWORD = 0;
begin
  if I < 0 then
    Result := FindProjectileSlot()
  else
  begin
    Result := I;
    if Result >= High(Projectiles) then
      SetLength(Projectiles, Result + 64)
  end;

  case ShotType of
    WEAPON_ROCKETLAUNCHER:
    begin
      with Projectiles[Result] do
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
      with Projectiles[Result] do
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
      with Projectiles[Result] do
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

    WEAPON_FLAMETHROWER:
    begin
      with Projectiles[Result] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := SHOT_FLAME_WIDTH;
        Obj.Rect.Height := SHOT_FLAME_HEIGHT;

        Triggers := nil;
        ShotType := WEAPON_FLAMETHROWER;
        Animation := nil;
        TextureID := 0;
        g_Frames_Get(TextureID, 'FRAMES_FLAME');
      end;
    end;

    WEAPON_IMP_FIRE:
    begin
      with Projectiles[Result] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := 16;
        Obj.Rect.Height := 16;

        Triggers := nil;
        ShotType := WEAPON_IMP_FIRE;
        g_Frames_Get(FramesID, 'FRAMES_WEAPON_IMPFIRE');
        Animation := TAnimation.Create(FramesID, True, 4);
      end;
    end;

    WEAPON_CACO_FIRE:
    begin
      with Projectiles[Result] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := 16;
        Obj.Rect.Height := 16;

        Triggers := nil;
        ShotType := WEAPON_CACO_FIRE;
        g_Frames_Get(FramesID, 'FRAMES_WEAPON_CACOFIRE');
        Animation := TAnimation.Create(FramesID, True, 4);
      end;
    end;

    WEAPON_MANCUB_FIRE:
    begin
      with Projectiles[Result] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := 32;
        Obj.Rect.Height := 32;

        Triggers := nil;
        ShotType := WEAPON_MANCUB_FIRE;
        g_Frames_Get(FramesID, 'FRAMES_WEAPON_MANCUBFIRE');
        Animation := TAnimation.Create(FramesID, True, 4);
      end;
    end;

    WEAPON_BARON_FIRE:
    begin
      with Projectiles[Result] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := 16;
        Obj.Rect.Height := 16;

        Triggers := nil;
        ShotType := WEAPON_BARON_FIRE;
        g_Frames_Get(FramesID, 'FRAMES_WEAPON_BARONFIRE');
        Animation := TAnimation.Create(FramesID, True, 4);
      end;
    end;

    WEAPON_BSP_FIRE:
    begin
      with Projectiles[Result] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := 16;
        Obj.Rect.Height := 16;

        Triggers := nil;
        ShotType := WEAPON_BSP_FIRE;
        g_Frames_Get(FramesID, 'FRAMES_WEAPON_BSPFIRE');
        Animation := TAnimation.Create(FramesID, True, 4);
      end;
    end;

    WEAPON_SKEL_FIRE:
    begin
      with Projectiles[Result] do
      begin
        g_Obj_Init(@Obj);

        Obj.Rect.Width := SHOT_SKELFIRE_WIDTH;
        Obj.Rect.Height := SHOT_SKELFIRE_HEIGHT;

        Triggers := nil;
        ShotType := WEAPON_SKEL_FIRE;
        target := TargetUID;
        g_Frames_Get(FramesID, 'FRAMES_WEAPON_SKELFIRE');
        Animation := TAnimation.Create(FramesID, True, 5);
      end;
    end;
  end;

  Projectiles[Result].Obj.oldX := X;
  Projectiles[Result].Obj.oldY := Y;
  Projectiles[Result].Obj.X := X;
  Projectiles[Result].Obj.Y := Y;
  Projectiles[Result].Obj.Vel.X := XV;
  Projectiles[Result].Obj.Vel.Y := YV;
  Projectiles[Result].Obj.Accel.X := 0;
  Projectiles[Result].Obj.Accel.Y := 0;
  Projectiles[Result].SpawnerUID := Spawner;

  if (ShotType = WEAPON_FLAMETHROWER) and (XV = 0) and (YV = 0)
    then Projectiles[Result].Stopped := 255
    else Projectiles[Result].Stopped := 0;
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

  Projectiles[i].Obj.oldX := x;
  Projectiles[i].Obj.oldY := y;
  Projectiles[i].Obj.X := x;
  Projectiles[i].Obj.Y := y;
  Projectiles[i].Obj.Vel.X := (xd*s) div a;
  Projectiles[i].Obj.Vel.Y := (yd*s) div a;
  Projectiles[i].Obj.Accel.X := 0;
  Projectiles[i].Obj.Accel.Y := 0;
  Projectiles[i].Stopped := 0;

  if Projectiles[i].ShotType in [WEAPON_ROCKETLAUNCHER, WEAPON_BFG] then
    Projectiles[i].Timeout := 900 // ~25 sec
  else
  begin
    if Projectiles[i].ShotType = WEAPON_FLAMETHROWER
      then Projectiles[i].Timeout := SHOT_FLAME_LIFETIME
      else Projectiles[i].Timeout := 550; // ~15 sec
  end;
end;

function g_Weapon_Hit(obj: PObj; d: Integer; SpawnerUID: Word; t: Byte; HitCorpses: Boolean = True): Byte;

  function PlayerHit(Team: Byte = 0): Boolean;
  var
    i: SizeInt;
    ChkTeam: Boolean;
    p: TPlayer;
  begin
    Result := False;
    for i := 0 to High(gPlayers) do
    begin
      if (gPlayers[i] <> nil) and gPlayers[i].alive and g_Obj_Collide(obj, @gPlayers[i].Obj) then
      begin
        ChkTeam := True;

        if (Team > 0) and (g_GetUIDType(SpawnerUID) = UID_PLAYER) then
        begin
          p := g_Player_Get(SpawnerUID);
          if p <> nil then
            ChkTeam := (p.Team = gPlayers[i].Team) xor (Team = 2);
        end;

        if ChkTeam and HitPlayer(gPlayers[i], d, obj^.Vel.X, obj^.Vel.Y, SpawnerUID, t) then
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
  end;

  {
  function monsCheckHit (monidx: Integer; mon: TMonster): Boolean;
  begin
    Result := False;  // don't stop
    if mon.alive and g_Obj_Collide(obj, @mon.Obj) then
    begin
      if HitMonster(mon, d, obj^.Vel.X, obj^.Vel.Y, SpawnerUID, t) then
      begin
        if t <> HIT_FLAME then
        begin
          mon.Push((obj^.Vel.X+obj^.Accel.X)*IfThen(t = HIT_BFG, 8, 1) div 4,
                   (obj^.Vel.Y+obj^.Accel.Y)*IfThen(t = HIT_BFG, 8, 1) div 4);
        end;
        Result := True;
      end;
    end;
  end;
  }

  function monsCheckHit (mon: TMonster): Boolean;
  begin
    Result := False;  // don't stop
    if HitMonster(mon, d, obj.Vel.X, obj.Vel.Y, SpawnerUID, t) then
    begin
      if t <> HIT_FLAME then
      begin
        mon.Push((obj.Vel.X+obj.Accel.X)*IfThen(t = HIT_BFG, 8, 1) div 4,
                 (obj.Vel.Y+obj.Accel.Y)*IfThen(t = HIT_BFG, 8, 1) div 4);
      end;
      Result := True;
    end;
  end;

  function MonsterHit(): Boolean;
  begin
    //Result := g_Mons_ForEach(monsCheckHit);
    //FIXME: accelerate this!
    Result := g_Mons_ForEachAliveAt(obj.X+obj.Rect.X, obj.Y+obj.Rect.Y, obj.Rect.Width, obj.Rect.Height, monsCheckHit);
  end;

var
  k: SizeInt;
begin
  Result := 0;

  if HitCorpses and gAdvCorpses then
  begin
    for k := 0 to High(gCorpses) do
      if (gCorpses[k] <> nil) and (gCorpses[k].State <> CORPSE_STATE_REMOVEME) and
        g_Obj_Collide(obj, @gCorpses[k].Obj) then
      begin
        // Распиливаем труп:
        gCorpses[k].Damage(d, SpawnerUID, (obj^.Vel.X+obj^.Accel.X) div 4,
                                          (obj^.Vel.Y+obj^.Accel.Y) div 4);
        Result := 1;
      end;
  end;

  case gGameSettings.GameMode of
    // Кампания:
    GM_COOP, GM_SINGLE: begin
      // Сначала бьём монстров, если есть
      if MonsterHit() then Exit(2);

      // И в конце игроков, но только если положено или снаряд от монстра
      if (g_GetUIDType(SpawnerUID) <> UID_PLAYER) or (gGameSettings.Options *
        [TGameOption.FRIENDLY_FIRE, TGameOption.TEAM_HIT_PROJECTILE] <> []) then
      begin
        if PlayerHit() then Exit(1);
      end;
    end;

    // Дезматч:
    GM_DM: begin
      if PlayerHit() then Exit(1);  // Сначала бьём игроков, если есть
      if MonsterHit() then Exit(2);  // потом пытаемся бить монстров
    end;

    // Командные:
    GM_TDM, GM_CTF: begin
      if PlayerHit(2) then Exit(1);  // Сначала бьём игроков команды соперника
      if MonsterHit() then Exit(2);  // Потом монстров

      // И в конце своих игроков, но только если положено
      // (или friendlyfire, или friendly_hit_projectile)
      if gGameSettings.Options * [TGameOption.FRIENDLY_FIRE, TGameOption.TEAM_HIT_PROJECTILE] <>
        [] then
      begin
        if PlayerHit(1) then Exit(1);
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

var
  i, h, dx, dy, m, mm: Integer;
  _angle: SmallInt;
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

            Damage(Round(100*(rad-m)/rad), SpawnerUID, (dx*10) div mm, (dy*10) div mm);
          end;
        end;

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
end;

procedure g_Weapon_Init();
begin
  CreateWaterMap();
end;

procedure g_Weapon_Free();
var
  i: Integer;
begin
  for i := 0 to High(Projectiles) do
    Projectiles[i].Animation.Free();

  Projectiles := nil;
  WaterMap := nil;
end;

procedure g_Weapon_LoadData();
begin
  e_WriteLog('Loading weapons data...', TMsgType.Notify);

{$IFDEF ENABLE_SOUND}
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
{$ENDIF}

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

  //wgunMonHash := hashNewIntInt();
  wgunHitHeap := TBinaryHeapHitTimes.Create();
end;

procedure g_Weapon_FreeData();
begin
  e_WriteLog('Releasing weapons data...', TMsgType.Notify);

{$IFDEF ENABLE_SOUND}
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
{$ENDIF}

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

  //wgunMonHash.Destroy();
  wgunHitHeap.Destroy();
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
    Result := False;
    if (idx < 0) or (idx > High(gPlayers)) then Exit;
    if (gPlayers[idx] = nil) or not gPlayers[idx].alive then Exit;

    // TODO: Simplify. This is just a very long condition split down into several nested IF checks.
    if spawnerPlr <> nil then
    begin
      if (gGameSettings.Options * [TGameOption.TEAM_HIT_TRACE, TGameOption.FRIENDLY_FIRE] = []) and
         (spawnerPlr.Team <> TEAM_NONE) and (spawnerPlr.Team = gPlayers[idx].Team) then
      begin
        if (spawnerPlr <> gPlayers[idx]) and (TGameOption.TEAM_ABSORB_ATTACKS in gGameSettings.Options) then
          dmg := Max(1, dmg div 2);
        exit;
      end;
    end;

    Result := HitPlayer(gPlayers[idx], dmg, (xi*v)*10, (yi*v)*10-3, SpawnerUID, HIT_SOME);
    if Result and (v <> 0) then gPlayers[idx].Push((xi*v), (yi*v));
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
    g_GFX_Spark(wallHitX, wallHitY, 2+Random(2), 180+a, 0, 0);
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
  begin
{$IFDEF ENABLE_SOUND}
    g_Sound_PlayExAt('SOUND_WEAPON_HITPUNCH', x, y)
{$ENDIF}
  end
  else
  begin
{$IFDEF ENABLE_SOUND}
    g_Sound_PlayExAt('SOUND_WEAPON_MISSPUNCH', x, y);
{$ENDIF}
  end;
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

function g_Weapon_rocket(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt; Silent: Boolean;
  compat: Boolean): SizeInt;
var
  dx, dy: Integer;
begin
  if WID < 0 then
    Result := FindProjectileSlot()
  else
  begin
    Result := WID;
    if Result >= High(Projectiles) then
      SetLength(Projectiles, Result + 64)
  end;

  with Projectiles[Result] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := SHOT_ROCKETLAUNCHER_WIDTH;
    Obj.Rect.Height := SHOT_ROCKETLAUNCHER_HEIGHT;

    if compat
      then dx := IfThen(xd > x, -Obj.Rect.Width, 0)
      else dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_ROCKETLAUNCHER;
    throw(Result, x+dx, y+dy, xd+dx, yd+dy, 12);

    Animation := nil;
    triggers := nil;
    g_Texture_Get('TEXTURE_WEAPON_ROCKET', TextureID);
  end;

  Projectiles[Result].SpawnerUID := SpawnerUID;

{$IFDEF ENABLE_SOUND}
  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREROCKET', x, y);
{$ENDIF}
end;

function g_Weapon_revf(x, y, xd, yd: Integer; SpawnerUID, TargetUID: Word; WID: SizeInt;
  Silent: Boolean): SizeInt;
var
  FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    Result := FindProjectileSlot()
  else
  begin
    Result := WID;
    if Result >= High(Projectiles) then
      SetLength(Projectiles, Result + 64)
  end;

  with Projectiles[Result] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := SHOT_SKELFIRE_WIDTH;
    Obj.Rect.Height := SHOT_SKELFIRE_HEIGHT;

    dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_SKEL_FIRE;
    throw(Result, x+dx, y+dy, xd+dx, yd+dy, 12);

    triggers := nil;
    target := TargetUID;
    g_Frames_Get(FramesID, 'FRAMES_WEAPON_SKELFIRE');
    Animation := TAnimation.Create(FramesID, True, 5);
  end;

  Projectiles[Result].SpawnerUID := SpawnerUID;

{$IFDEF ENABLE_SOUND}
  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREREV', x, y);
{$ENDIF}
end;

function g_Weapon_plasma(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt; Silent: Boolean;
  compat: Boolean): SizeInt;
var
  FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    Result := FindProjectileSlot()
  else
  begin
    Result := WID;
    if Result >= High(Projectiles) then
      SetLength(Projectiles, Result + 64);
  end;

  with Projectiles[Result] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := SHOT_PLASMA_WIDTH;
    Obj.Rect.Height := SHOT_PLASMA_HEIGHT;

    if compat
      then dx := IfThen(xd > x, -Obj.Rect.Width, 0)
      else dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_PLASMA;
    throw(Result, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    g_Frames_Get(FramesID, 'FRAMES_WEAPON_PLASMA');
    Animation := TAnimation.Create(FramesID, True, 5);
  end;

  Projectiles[Result].SpawnerUID := SpawnerUID;

{$IFDEF ENABLE_SOUND}
  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREPLASMA', x, y);
{$ENDIF}
end;

function g_Weapon_flame(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt; Silent: Boolean;
  compat: Boolean): SizeInt;
var
  dx, dy: Integer;
begin
  if WID < 0 then
    Result := FindProjectileSlot()
  else
  begin
    Result := WID;
    if Result >= High(Projectiles) then
      SetLength(Projectiles, Result + 64);
  end;

  with Projectiles[Result] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := SHOT_FLAME_WIDTH;
    Obj.Rect.Height := SHOT_FLAME_HEIGHT;

    if compat
      then dx := IfThen(xd > x, -Obj.Rect.Width, 0)
      else dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_FLAMETHROWER;
    throw(Result, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    Animation := nil;
    TextureID := 0;
    g_Frames_Get(TextureID, 'FRAMES_FLAME');
  end;

  Projectiles[Result].SpawnerUID := SpawnerUID;

  // if not Silent then
  //  g_Sound_PlayExAt('SOUND_WEAPON_FIREPLASMA', x, y);
end;

function g_Weapon_ball1(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt; Silent: Boolean;
  compat: Boolean): SizeInt;
var
  FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    Result := FindProjectileSlot()
  else
  begin
    Result := WID;
    if Result >= High(Projectiles) then
      SetLength(Projectiles, Result + 64)
  end;

  with Projectiles[Result] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := 16;
    Obj.Rect.Height := 16;

    if compat
      then dx := IfThen(xd > x, -Obj.Rect.Width, 0)
      else dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_IMP_FIRE;
    throw(Result, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    g_Frames_Get(FramesID, 'FRAMES_WEAPON_IMPFIRE');
    Animation := TAnimation.Create(FramesID, True, 4);
  end;

  Projectiles[Result].SpawnerUID := SpawnerUID;

{$IFDEF ENABLE_SOUND}
  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREBALL', x, y);
{$ENDIF}
end;

function g_Weapon_ball2(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt; Silent: Boolean;
  compat: Boolean): SizeInt;
var
  FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    Result := FindProjectileSlot()
  else
  begin
    Result := WID;
    if Result >= High(Projectiles) then
      SetLength(Projectiles, Result + 64)
  end;

  with Projectiles[Result] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := 16;
    Obj.Rect.Height := 16;

    if compat
      then dx := IfThen(xd > x, -Obj.Rect.Width, 0)
      else dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_CACO_FIRE;
    throw(Result, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    g_Frames_Get(FramesID, 'FRAMES_WEAPON_CACOFIRE');
    Animation := TAnimation.Create(FramesID, True, 4);
  end;

  Projectiles[Result].SpawnerUID := SpawnerUID;

{$IFDEF ENABLE_SOUND}
  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREBALL', x, y);
{$ENDIF}
end;

function g_Weapon_ball7(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt; Silent: Boolean;
  compat: Boolean): SizeInt;
var
  FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    Result := FindProjectileSlot()
  else
  begin
    Result := WID;
    if Result >= High(Projectiles) then
      SetLength(Projectiles, Result + 64)
  end;

  with Projectiles[Result] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := 16;
    Obj.Rect.Height := 16;

    if compat
      then dx := IfThen(xd > x, -Obj.Rect.Width, 0)
      else dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_BARON_FIRE;
    throw(Result, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    g_Frames_Get(FramesID, 'FRAMES_WEAPON_BARONFIRE');
    Animation := TAnimation.Create(FramesID, True, 4);
  end;

  Projectiles[Result].SpawnerUID := SpawnerUID;

{$IFDEF ENABLE_SOUND}
  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREBALL', x, y);
{$ENDIF}
end;

function g_Weapon_aplasma(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt; Silent: Boolean;
  compat: Boolean): SizeInt;
var
  FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    Result := FindProjectileSlot()
  else
  begin
    Result := WID;
    if Result >= High(Projectiles) then
      SetLength(Projectiles, Result + 64)
  end;

  with Projectiles[Result] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := 16;
    Obj.Rect.Height := 16;

    if compat
      then dx := IfThen(xd > x, -Obj.Rect.Width, 0)
      else dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_BSP_FIRE;
    throw(Result, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;

    g_Frames_Get(FramesID, 'FRAMES_WEAPON_BSPFIRE');
    Animation := TAnimation.Create(FramesID, True, 4);
  end;

  Projectiles[Result].SpawnerUID := SpawnerUID;

{$IFDEF ENABLE_SOUND}
  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREPLASMA', x, y);
{$ENDIF}
end;

function g_Weapon_manfire(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt; Silent: Boolean;
  compat: Boolean): SizeInt;
var
  FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    Result := FindProjectileSlot()
  else
  begin
    Result := WID;
    if Result >= High(Projectiles) then
      SetLength(Projectiles, Result + 64)
  end;

  with Projectiles[Result] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := 32;
    Obj.Rect.Height := 32;

    if compat
      then dx := IfThen(xd > x, -Obj.Rect.Width, 0)
      else dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_MANCUB_FIRE;
    throw(Result, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;

    g_Frames_Get(FramesID, 'FRAMES_WEAPON_MANCUBFIRE');
    Animation := TAnimation.Create(FramesID, True, 4);
  end;

  Projectiles[Result].SpawnerUID := SpawnerUID;

{$IFDEF ENABLE_SOUND}
  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREBALL', x, y);
{$ENDIF}
end;

function g_Weapon_bfgshot(x, y, xd, yd: Integer; SpawnerUID: Word; WID: SizeInt; Silent: Boolean;
  compat: Boolean): SizeInt;
var
  FramesID: DWORD;
  dx, dy: Integer;
begin
  if WID < 0 then
    Result := FindProjectileSlot()
  else
  begin
    Result := WID;
    if Result >= High(Projectiles) then
      SetLength(Projectiles, Result + 64)
  end;

  with Projectiles[Result] do
  begin
    g_Obj_Init(@Obj);

    Obj.Rect.Width := SHOT_BFG_WIDTH;
    Obj.Rect.Height := SHOT_BFG_HEIGHT;

    if compat
      then dx := IfThen(xd > x, -Obj.Rect.Width, 0)
      else dx := -(Obj.Rect.Width div 2);
    dy := -(Obj.Rect.Height div 2);

    ShotType := WEAPON_BFG;
    throw(Result, x+dx, y+dy, xd+dx, yd+dy, 16);

    triggers := nil;
    g_Frames_Get(FramesID, 'FRAMES_WEAPON_BFG');
    Animation := TAnimation.Create(FramesID, True, 6);
  end;

  Projectiles[Result].SpawnerUID := SpawnerUID;

{$IFDEF ENABLE_SOUND}
  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREBFG', x, y);
{$ENDIF}
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
    Anim.Destroy();
  end;
end;

procedure g_Weapon_pistol(x, y, xd, yd: Integer; SpawnerUID: Word; Silent: Boolean);
begin
{$IFDEF ENABLE_SOUND}
  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIREPISTOL', x, y);
{$ENDIF}

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

procedure g_Weapon_mgun(x, y, xd, yd: Integer; SpawnerUID: Word; Silent: Boolean);
begin
{$IFDEF ENABLE_SOUND}
  if not Silent then
    if gSoundEffectsDF then g_Sound_PlayExAt('SOUND_WEAPON_FIRECGUN', x, y);
{$ENDIF}

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

procedure g_Weapon_shotgun(x, y, xd, yd: Integer; SpawnerUID: Word; Silent: Boolean);
var
  i, j, k: Integer;
begin
{$IFDEF ENABLE_SOUND}
  if not Silent then
    if gSoundEffectsDF then g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN', x, y);
{$ENDIF}

  for i := 0 to 9 do
  begin
    j := 0; k := 0;
    if ABS(x-xd) >= ABS(y-yd) then j := Random(17) - 8 else k := Random(17) - 8; // -8 .. 8
    g_Weapon_gun(x+k, y+j, xd+k, yd+j, IfThen(i mod 2 <> 0, 1, 0), 3, SpawnerUID, i=0);
  end;
end;

procedure g_Weapon_dshotgun(x, y, xd, yd: Integer; SpawnerUID: Word; Silent: Boolean);
var
  a, i, j, k: Integer;
begin
{$IFDEF ENABLE_SOUND}
  if not Silent then
    g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN2', x, y);
{$ENDIF}

  if gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF]
    then a := 25
    else a := 20;

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
  for i := 0 to High(Projectiles) do
    if Projectiles[i].ShotType <> 0 then
    begin
      Projectiles[i].Obj.oldX := Projectiles[i].Obj.X;
      Projectiles[i].Obj.oldY := Projectiles[i].Obj.Y;
    end;
end;

procedure g_Weapon_Update();
var
  i, a, h, cx, cy, oldvx, oldvy, tf: Integer;
  _id: DWORD;
  Anim: TAnimation;
  t: DWArray;
  st: Word;
  s: String;
  o: TObj;
  spl: Boolean;
  Loud: Boolean;
  tcx, tcy: Integer;
label
  finish_update;  // uhh, FreePascal doesn't have a 'break' for 'case of'
begin
  for i := 0 to High(Projectiles) do
  begin
    if Projectiles[i].ShotType = 0 then
      Continue;

    Loud := True;

    with Projectiles[i] do
    begin
      Timeout -= 1;
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
      if Animation <> nil then
        Animation.Update();

      // Движение:
      spl := not (ShotType in [WEAPON_PLASMA, WEAPON_BFG, WEAPON_BSP_FIRE, WEAPON_FLAMETHROWER]);

      if Stopped = 0
        then st := g_Obj_Move_Projectile(@Obj, False, spl)
        else st := 0;
      positionChanged();  // this updates spatial accelerators

      if WordBool(st and MOVE_FALLOUT) or (Obj.X < -1000) or
        (Obj.X > gMapInfo.Width+1000) or (Obj.Y < -1000) then
      begin
        ShotType := 0;
        //Goto finish_update;
        FreeAndNil(Animation);
        Continue;  // На клиенте скорее всего и так уже выпал.
      end;

      cx := Obj.X + (Obj.Rect.Width div 2);
      cy := Obj.Y + (Obj.Rect.Height div 2);

      case ShotType of
        WEAPON_ROCKETLAUNCHER, WEAPON_SKEL_FIRE:
        begin
          // Вылетела из воды:
          if WordBool(st and MOVE_HITAIR) then
            g_Obj_SetSpeed(@Obj, 12);

          // В воде шлейф - пузыри, в воздухе шлейф - дым:
          if WordBool(st and MOVE_INWATER) then
          begin
            g_Game_Effect_Bubbles(cx, cy, 1+Random(3), 16, 16);
          end
          else if g_Frames_Get(_id, 'FRAMES_SMOKE') then
          begin
            Anim := TAnimation.Create(_id, False, 3);
            Anim.Alpha := 150;
            g_GFX_OnceAnim(Obj.X-14+Random(9), cy-20+Random(9),
                           Anim, ONCEANIM_SMOKE);
            Anim.Destroy();
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
            begin  // Взрыв снаряда Скелета
              if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_SKELFIRE') then
              begin
                Anim := TAnimation.Create(TextureID, False, 8);
                Anim.Blending := False;
                g_GFX_OnceAnim((Obj.X+32)-58, (Obj.Y+8)-36, Anim);
                Anim.Destroy();
                g_DynLightExplosion((Obj.X+32), (Obj.Y+8), 64, 1, 0, 0);
              end;
            end
            else
            begin  // Взрыв Ракеты
              if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_ROCKET') then
              begin
                Anim := TAnimation.Create(TextureID, False, 6);
                Anim.Blending := False;
                g_GFX_OnceAnim(cx-64, cy-64, Anim);
                Anim.Destroy();
                g_DynLightExplosion(cx, cy, 64, 1, 0, 0);
              end;
            end;

{$IFDEF ENABLE_SOUND}
            g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEROCKET', Obj.X, Obj.Y);
{$ENDIF}

            ShotType := 0;
          end;

          if ShotType = WEAPON_SKEL_FIRE then
          begin  // Самонаводка снаряда Скелета:
            if GetPos(target, @o) then
              throw(i, Obj.X, Obj.Y,
                    o.X+o.Rect.X+(o.Rect.Width div 2)+o.Vel.X+o.Accel.X,
                    o.Y+o.Rect.Y+(o.Rect.Height div 2)+o.Vel.Y+o.Accel.Y,
                    12);
          end;
        end;

        WEAPON_PLASMA, WEAPON_BSP_FIRE:
        begin
          // Попала в воду - электрошок по воде:
          if WordBool(st and (MOVE_INWATER or MOVE_HITWATER)) then
          begin
{$IFDEF ENABLE_SOUND}
            g_Sound_PlayExAt('SOUND_WEAPON_PLASMAWATER', Obj.X, Obj.Y);
{$ENDIF}
            if g_Game_IsServer then CheckTrap(i, 10, HIT_ELECTRO);
            ShotType := 0;
            Goto finish_update;
          end;

          // Величина урона:
          if (ShotType = WEAPON_PLASMA) and (gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF])
            then a := 10
            else a := 5;

          if ShotType = WEAPON_BSP_FIRE then
            a := 10;

          // Попали в кого-то или в стену:
          if WordBool(st and (MOVE_HITWALL or MOVE_HITLAND or MOVE_HITCEIL)) or
             (g_Weapon_Hit(@Obj, a, SpawnerUID, HIT_SOME, False) <> 0) or
             (Timeout < 1) then
          begin
            if ShotType = WEAPON_PLASMA
              then s := 'FRAMES_EXPLODE_PLASMA'
              else s := 'FRAMES_EXPLODE_BSPFIRE';

            // Взрыв Плазмы:
            if g_Frames_Get(TextureID, s) then
            begin
              Anim := TAnimation.Create(TextureID, False, 3);
              Anim.Blending := False;
              g_GFX_OnceAnim(cx-16, cy-16, Anim);
              Anim.Destroy();
              g_DynLightExplosion(cx, cy, 32, 0, 0.5, 0.5);
            end;

{$IFDEF ENABLE_SOUND}
            g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEPLASMA', Obj.X, Obj.Y);
{$ENDIF}

            ShotType := 0;
          end;
        end;

        WEAPON_FLAMETHROWER:
        begin
          // Со временем умирает
          if (Timeout < 1) then
          begin
            ShotType := 0;
            Goto finish_update;
          end;
          // Под водой тоже
          if WordBool(st and (MOVE_HITWATER or MOVE_INWATER)) then
          begin
            if WordBool(st and MOVE_HITWATER) then
            begin
              if g_Frames_Get(_id, 'FRAMES_SMOKE') then
              begin
                Anim := TAnimation.Create(_id, False, 3);
                Anim.Alpha := 0;
                tcx := Random(8);
                tcy := Random(8);
                g_GFX_OnceAnim(
                  cx-4+tcx-(Anim.Width div 2),
                  cy-4+tcy-(Anim.Height div 2),
                  Anim, ONCEANIM_SMOKE
                );
                Anim.Destroy();
              end;
            end
            else
              g_Game_Effect_Bubbles(cx, cy, 1+Random(3), 16, 16);

            ShotType := 0;
            Goto finish_update;
          end;

          // Гравитация
          if Stopped = 0 then
            Obj.Accel.Y += 1;

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

          if Stopped = 0
            then tf := 2
            else tf := 3;

          if (gTime mod LongWord(tf) = 0) then
          begin
            Anim := TAnimation.Create(TextureID, False, 2 + Random(2));
            Anim.Alpha := 0;
            case Stopped of
              MOVE_HITWALL: begin tcx := cx-4+Random(8); tcy := cy-12+Random(24); end;
              MOVE_HITLAND: begin tcx := cx-12+Random(24); tcy := cy-10+Random(8); end;
              MOVE_HITCEIL: begin tcx := cx-12+Random(24); tcy := cy+6+Random(8); end;
              else begin tcx := cx-4+Random(8); tcy := cy-4+Random(8); end;
            end;
            g_GFX_OnceAnim(tcx-(Anim.Width div 2), tcy-(Anim.Height div 2), Anim, ONCEANIM_SMOKE);
            Anim.Destroy();
            //g_DynLightExplosion(tcx, tcy, 1, 1, 0.8, 0.3);
          end;
        end;

        WEAPON_BFG:
        begin
          // Попала в воду - электрошок по воде:
          if WordBool(st and (MOVE_INWATER or MOVE_HITWATER)) then
          begin
{$IFDEF ENABLE_SOUND}
            g_Sound_PlayExAt('SOUND_WEAPON_BFGWATER', Obj.X, Obj.Y);
{$ENDIF}
            if g_Game_IsServer then CheckTrap(i, 1000, HIT_ELECTRO);
            ShotType := 0;
            Goto finish_update;
          end;

          // Попали в кого-то или в стену:
          if WordBool(st and (MOVE_HITWALL or MOVE_HITLAND or MOVE_HITCEIL)) or
             (g_Weapon_Hit(@Obj, SHOT_BFG_DAMAGE, SpawnerUID, HIT_BFG, False) <> 0) or
             (Timeout < 1) then
          begin
            // Лучи BFG:
            if g_Game_IsServer then g_Weapon_BFG9000(cx, cy, SpawnerUID);

            // Взрыв BFG:
            if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_BFG') then
            begin
              Anim := TAnimation.Create(TextureID, False, 6);
              Anim.Blending := False;
              g_GFX_OnceAnim(cx-64, cy-64, Anim);
              Anim.Destroy();
              g_DynLightExplosion(cx, cy, 96, 0, 1, 0);
            end;

{$IFDEF ENABLE_SOUND}
            g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBFG', Obj.X, Obj.Y);
{$ENDIF}

            ShotType := 0;
          end;
        end;

        WEAPON_IMP_FIRE, WEAPON_CACO_FIRE, WEAPON_BARON_FIRE:
        begin
          // Вылетел из воды:
          if WordBool(st and MOVE_HITAIR) then
            g_Obj_SetSpeed(@Obj, 16);

          // Величина урона:
          if ShotType = WEAPON_IMP_FIRE then
            a := 5
          else
            if ShotType = WEAPON_CACO_FIRE
              then a := 20
              else a := 40;

          // Попали в кого-то или в стену:
          if WordBool(st and (MOVE_HITWALL or MOVE_HITLAND or MOVE_HITCEIL)) or
             (g_Weapon_Hit(@Obj, a, SpawnerUID, HIT_SOME) <> 0) or
             (Timeout < 1) then
          begin
            if ShotType = WEAPON_IMP_FIRE then
              s := 'FRAMES_EXPLODE_IMPFIRE'
            else
              if ShotType = WEAPON_CACO_FIRE
                then s := 'FRAMES_EXPLODE_CACOFIRE'
                else s := 'FRAMES_EXPLODE_BARONFIRE';

            // Взрыв:
            if g_Frames_Get(TextureID, s) then
            begin
              Anim := TAnimation.Create(TextureID, False, 6);
              Anim.Blending := False;
              g_GFX_OnceAnim(cx-32, cy-32, Anim);
              Anim.Destroy();
            end;

{$IFDEF ENABLE_SOUND}
            g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBALL', Obj.X, Obj.Y);
{$ENDIF}

            ShotType := 0;
          end;
        end;

        WEAPON_MANCUB_FIRE:
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
            if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_ROCKET') then
            begin
              Anim := TAnimation.Create(TextureID, False, 6);
              Anim.Blending := False;
              g_GFX_OnceAnim(cx-64, cy-64, Anim);
              Anim.Destroy();
            end;

{$IFDEF ENABLE_SOUND}
            g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBALL', Obj.X, Obj.Y);
{$ENDIF}

            ShotType := 0;
          end;
        end;
      end;

finish_update:
      // Если снаряда уже нет, удаляем анимацию:
      if ShotType = 0 then
      begin
        if gGameSettings.GameType = GT_SERVER then
          MH_SEND_DeleteProj(i, Obj.X, Obj.Y, Loud);
        FreeAndNil(Animation);
      end
      else if (ShotType <> WEAPON_FLAMETHROWER) and ((oldvx <> Obj.Vel.X) or (oldvy <> Obj.Vel.Y)) then
        if gGameSettings.GameType = GT_SERVER then
          MH_SEND_UpdateProj(i);
    end;
  end;
end;

procedure g_Weapon_Draw();
var
  i, fX, fY: Integer;
  a: SmallInt;
  p: TDFPoint;
begin
  for i := 0 to High(Projectiles) do
    if Projectiles[i].ShotType <> 0 then
      with Projectiles[i] do
      begin
        if (Projectiles[i].ShotType in [WEAPON_ROCKETLAUNCHER, WEAPON_BARON_FIRE,
            WEAPON_MANCUB_FIRE, WEAPON_SKEL_FIRE])
          then a := -GetAngle2(Obj.Vel.X, Obj.Vel.Y)
          else a := 0;

        Obj.lerp(gLerpFactor, fX, fY);
        p.X := Obj.Rect.Width div 2;
        p.Y := Obj.Rect.Height div 2;

        if Projectiles[i].ShotType = WEAPON_BFG then
        begin
          fX -= 6;
          fY -= 7;
        end;

        if Animation <> nil then
          begin
            if Projectiles[i].ShotType in [WEAPON_BARON_FIRE, WEAPON_MANCUB_FIRE, WEAPON_SKEL_FIRE]
              then Animation.DrawEx(fX, fY, TMirrorType.None, p, a)
              else Animation.Draw(fX, fY, TMirrorType.None);
          end
        else if TextureID <> 0 then
          begin
            if (Projectiles[i].ShotType = WEAPON_ROCKETLAUNCHER) then
              e_DrawAdv(TextureID, fX, fY, 0, True, False, a, @p, TMirrorType.None)
            else if (Projectiles[i].ShotType <> WEAPON_FLAMETHROWER) then
              e_Draw(TextureID, fX, fY, 0, True, False);
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

function g_Weapon_Danger(UID: Word; X, Y: Integer; Width, Height: Word; Time: Byte): Boolean;
var
  a: Integer;
begin
  Result := False;

  for a := 0 to High(Projectiles) do
    if (Projectiles[a].ShotType <> 0) and (Projectiles[a].SpawnerUID <> UID) then
      if ((Projectiles[a].Obj.Vel.Y = 0) and (Projectiles[a].Obj.Vel.X > 0) and (Projectiles[a].Obj.X < X)) or
          (Projectiles[a].Obj.Vel.Y = 0) and (Projectiles[a].Obj.Vel.X < 0) and (Projectiles[a].Obj.X > X) then
        if (Abs(X-Projectiles[a].Obj.X) < Abs(Projectiles[a].Obj.Vel.X*Time)) and
            g_Collide(X, Y, Width, Height, X, Projectiles[a].Obj.Y,
                      Projectiles[a].Obj.Rect.Width, Projectiles[a].Obj.Rect.Height) and
            g_TraceVector(X, Y, Projectiles[a].Obj.X, Projectiles[a].Obj.Y) then
        begin
          Result := True;
          Exit;
        end;
end;

procedure g_Weapon_SaveState (st: TStream);
var
  count: SizeUInt;
  i, j: SizeInt;
begin
  // Считаем количество существующих снарядов
  count := 0;
  for i := 0 to High(Projectiles) do
    if Projectiles[i].ShotType <> 0 then count += 1;

  // Количество снарядов
  st.WriteDWordLE(count);
  if count = 0 then Exit;

  for i := 0 to High(Projectiles) do
  begin
    if Projectiles[i].ShotType <> 0 then
    begin
      // Сигнатура снаряда
      utils.writeSign(st, 'SHOT');
      st.WriteByte(0);  // version

      st.WriteByte(Projectiles[i].ShotType);  // Тип снаряда
      st.WriteWordLE(Projectiles[i].Target);  // Цель
      st.WriteWordLE(Projectiles[i].SpawnerUID);  // UID стрелявшего
      st.WriteDWordLE(Length(Projectiles[i].Triggers));  // Размер поля Triggers

      // Триггеры, активированные выстрелом
      for j := 0 to High(Projectiles[i].Triggers) do
        st.WriteDWordLE(Projectiles[i].Triggers[j]);

      Obj_SaveState(st, @Projectiles[i].Obj);  // Объект снаряда
      st.WriteByte(Projectiles[i].Stopped);  // Костылина
    end;
  end;
end;

procedure g_Weapon_LoadState (st: TStream);
var
  count, tc, i: SizeUInt;
  j: SizeInt;
  dw: LongWord;
begin
  if st = nil then Exit;

  // Количество снарядов
  count := st.ReadDWordLE();
  if count > 1024*1024 then
    Raise XStreamError.Create('invalid shots counter');

  SetLength(Projectiles, count);
  if count = 0 then Exit;

  for i := 0 to count-1 do
  begin
    // Сигнатура снаряда
    if not utils.checkSign(st, 'SHOT') then
      Raise XStreamError.Create('invalid shot signature');
    if st.ReadByte() <> 0 then
      Raise XStreamError.Create('invalid shot version');

    Projectiles[i].ShotType := st.ReadByte();  // Тип снаряда:
    Projectiles[i].Target := st.ReadWordLE();  // Цель
    Projectiles[i].SpawnerUID := st.ReadWordLE();  // UID стрелявшего

    // Размер поля Triggers
    tc := st.ReadDWordLE();
    if tc > 1024*1024 then
      Raise XStreamError.Create('invalid shot triggers counter');
    SetLength(Projectiles[i].Triggers, tc);

    // Триггеры, активированные выстрелом
    for j := 0 to tc - 1 do
      Projectiles[i].Triggers[j] := st.ReadDWordLE();

    Obj_LoadState(@Projectiles[i].Obj, st);  // Объект предмета
    Projectiles[i].Stopped := st.ReadByte();  // Костылина

    // Установка текстуры или анимации
    Projectiles[i].TextureID := DWORD(-1);
    Projectiles[i].Animation := nil;

    case Projectiles[i].ShotType of
      WEAPON_ROCKETLAUNCHER, WEAPON_SKEL_FIRE:
        g_Texture_Get('TEXTURE_WEAPON_ROCKET', Projectiles[i].TextureID);
      WEAPON_PLASMA:
      begin
        g_Frames_Get(dw, 'FRAMES_WEAPON_PLASMA');
        Projectiles[i].Animation := TAnimation.Create(dw, True, 5);
      end;
      WEAPON_BFG:
      begin
        g_Frames_Get(dw, 'FRAMES_WEAPON_BFG');
        Projectiles[i].Animation := TAnimation.Create(dw, True, 6);
      end;
      WEAPON_IMP_FIRE:
      begin
        g_Frames_Get(dw, 'FRAMES_WEAPON_IMPFIRE');
        Projectiles[i].Animation := TAnimation.Create(dw, True, 4);
      end;
      WEAPON_BSP_FIRE:
      begin
        g_Frames_Get(dw, 'FRAMES_WEAPON_BSPFIRE');
        Projectiles[i].Animation := TAnimation.Create(dw, True, 4);
      end;
      WEAPON_CACO_FIRE:
      begin
        g_Frames_Get(dw, 'FRAMES_WEAPON_CACOFIRE');
        Projectiles[i].Animation := TAnimation.Create(dw, True, 4);
      end;
      WEAPON_BARON_FIRE:
      begin
        g_Frames_Get(dw, 'FRAMES_WEAPON_BARONFIRE');
        Projectiles[i].Animation := TAnimation.Create(dw, True, 4);
      end;
      WEAPON_MANCUB_FIRE:
      begin
        g_Frames_Get(dw, 'FRAMES_WEAPON_MANCUBFIRE');
        Projectiles[i].Animation := TAnimation.Create(dw, True, 4);
      end;
    end;
  end;
end;

procedure g_Weapon_DestroyProj(I: Integer; X, Y: Integer; Loud: Boolean);
var
  cx, cy: Integer;
  Anim: TAnimation;
  s: String;
begin
  if (Low(Projectiles) > I) or (High(Projectiles) < I) then
    Exit;

  with Projectiles[I] do
  begin
    if ShotType = 0 then Exit;
    Obj.X := X;
    Obj.Y := Y;
    cx := Obj.X + (Obj.Rect.Width div 2);
    cy := Obj.Y + (Obj.Rect.Height div 2);

    case ShotType of
      WEAPON_ROCKETLAUNCHER, WEAPON_SKEL_FIRE:
      begin
        if Loud then
        begin
          if ShotType = WEAPON_SKEL_FIRE then
          begin
            if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_SKELFIRE') then
            begin
              Anim := TAnimation.Create(TextureID, False, 8);
              Anim.Blending := False;
              g_GFX_OnceAnim((Obj.X+32)-58, (Obj.Y+8)-36, Anim);
              Anim.Destroy();
              g_DynLightExplosion((Obj.X+32), (Obj.Y+8), 64, 1, 0, 0);
            end;
          end
          else
          begin
            if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_ROCKET') then
            begin
              Anim := TAnimation.Create(TextureID, False, 6);
              Anim.Blending := False;
              g_GFX_OnceAnim(cx-64, cy-64, Anim);
              Anim.Destroy();
              g_DynLightExplosion(cx, cy, 64, 1, 0, 0);
            end;
          end;
{$IFDEF ENABLE_SOUND}
          g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEROCKET', Obj.X, Obj.Y);
{$ENDIF}
        end;
      end;

      WEAPON_PLASMA, WEAPON_BSP_FIRE:
      begin
        if ShotType = WEAPON_PLASMA
          then s := 'FRAMES_EXPLODE_PLASMA'
          else s := 'FRAMES_EXPLODE_BSPFIRE';

        if g_Frames_Get(TextureID, s) and loud then
        begin
          Anim := TAnimation.Create(TextureID, False, 3);
          Anim.Blending := False;
          g_GFX_OnceAnim(cx-16, cy-16, Anim);
          Anim.Destroy();
          g_DynLightExplosion(cx, cy, 32, 0, 0.5, 0.5);

{$IFDEF ENABLE_SOUND}
          g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEPLASMA', Obj.X, Obj.Y);
{$ENDIF}
        end;
      end;

      WEAPON_BFG:
      begin
        if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_BFG') and Loud then
        begin
          Anim := TAnimation.Create(TextureID, False, 6);
          Anim.Blending := False;
          g_GFX_OnceAnim(cx-64, cy-64, Anim);
          Anim.Destroy();
          g_DynLightExplosion(cx, cy, 96, 0, 1, 0);

{$IFDEF ENABLE_SOUND}
          g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBFG', Obj.X, Obj.Y);
{$ENDIF}
        end;
      end;

      WEAPON_IMP_FIRE, WEAPON_CACO_FIRE, WEAPON_BARON_FIRE:
      begin
        if ShotType = WEAPON_IMP_FIRE then
          s := 'FRAMES_EXPLODE_IMPFIRE'
        else
          if ShotType = WEAPON_CACO_FIRE
            then s := 'FRAMES_EXPLODE_CACOFIRE'
            else s := 'FRAMES_EXPLODE_BARONFIRE';

        if g_Frames_Get(TextureID, s) and Loud then
        begin
          Anim := TAnimation.Create(TextureID, False, 6);
          Anim.Blending := False;
          g_GFX_OnceAnim(cx-32, cy-32, Anim);
          Anim.Destroy();

{$IFDEF ENABLE_SOUND}
          g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBALL', Obj.X, Obj.Y);
{$ENDIF}
        end;
      end;

      WEAPON_MANCUB_FIRE:
      begin
        if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_ROCKET') and Loud then
        begin
          Anim := TAnimation.Create(TextureID, False, 6);
          Anim.Blending := False;
          g_GFX_OnceAnim(cx-64, cy-64, Anim);
          Anim.Destroy();

{$IFDEF ENABLE_SOUND}
          g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBALL', Obj.X, Obj.Y);
{$ENDIF}
        end;
      end;
    end;

    ShotType := 0;
    FreeAndNil(Animation);
  end;
end;


procedure g_Weapon_AddDynLights();
var
  i: Integer;
begin
  for i := 0 to High(Projectiles) do
  begin
    if Projectiles[i].ShotType = 0 then continue;
    if Projectiles[i].ShotType in [WEAPON_ROCKETLAUNCHER, WEAPON_PLASMA, WEAPON_BFG,
      WEAPON_FLAMETHROWER, WEAPON_IMP_FIRE, WEAPON_BSP_FIRE, WEAPON_CACO_FIRE, WEAPON_BARON_FIRE,
      WEAPON_MANCUB_FIRE, WEAPON_SKEL_FIRE] then
    begin
      if (Projectiles[i].ShotType = WEAPON_PLASMA) then
        g_AddDynLight(Projectiles[i].Obj.X+(Projectiles[i].Obj.Rect.Width div 2),
          Projectiles[i].Obj.Y+(Projectiles[i].Obj.Rect.Height div 2), 128, 0, 0.3, 1, 0.4)
      else if (Projectiles[i].ShotType = WEAPON_BFG) then
        g_AddDynLight(Projectiles[i].Obj.X+(Projectiles[i].Obj.Rect.Width div 2),
          Projectiles[i].Obj.Y+(Projectiles[i].Obj.Rect.Height div 2), 128, 0, 1, 0, 0.5)
      else if (Projectiles[i].ShotType = WEAPON_FLAMETHROWER) then
        g_AddDynLight(Projectiles[i].Obj.X+(Projectiles[i].Obj.Rect.Width div 2),
          Projectiles[i].Obj.Y+(Projectiles[i].Obj.Rect.Height div 2), 42, 1, 0.8, 0, 0.4)
      else
        g_AddDynLight(Projectiles[i].Obj.X+(Projectiles[i].Obj.Rect.Width div 2),
          Projectiles[i].Obj.Y+(Projectiles[i].Obj.Rect.Height div 2), 128, 1, 0, 0, 0.4);
    end;
  end;
end;

procedure TProjectile.positionChanged ();
begin
end;

end.
