(* Copyright (C)  DooM 2D:Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$MODE DELPHI}
unit g_scriptprocs;

interface

uses 
  g_scripts;

function SP_Lua_ConPrint(L: PScriptContext): Integer; cdecl;
function SP_Lua_PlaySound(L: PScriptContext): Integer; cdecl;
function SP_Lua_Message(L: PScriptContext): Integer; cdecl;
function SP_Lua_GetGameMode(L: PScriptContext): Integer; cdecl;
function SP_Lua_GetGameType(L: PScriptContext): Integer; cdecl;
function SP_Lua_GetTime(L: PScriptContext): Integer; cdecl;

function SP_Lua_PlayerGetKeys(L: PScriptContext): Integer; cdecl;
function SP_Lua_PlayerGetArmor(L: PScriptContext): Integer; cdecl;
function SP_Lua_PlayerGetTeam(L: PScriptContext): Integer; cdecl;
function SP_Lua_PlayerGetScore(L: PScriptContext): Integer; cdecl;
function SP_Lua_PlayerGetName(L: PScriptContext): Integer; cdecl;

function SP_Lua_ActorGetPos(L: PScriptContext): Integer; cdecl;
function SP_Lua_ActorGetHealth(L: PScriptContext): Integer; cdecl;
function SP_Lua_ActorGetState(L: PScriptContext): Integer; cdecl;
function SP_Lua_ActorGetType(L: PScriptContext): Integer; cdecl;
function SP_Lua_ActorNearest(L: PScriptContext): Integer; cdecl;
function SP_Lua_ActorFarthest(L: PScriptContext): Integer; cdecl;
function SP_Lua_ActorDamage(L: PScriptContext): Integer; cdecl;
function SP_Lua_ActorTeleport(L: PScriptContext): Integer; cdecl;
function SP_Lua_ActorPush(L: PScriptContext): Integer; cdecl;

function SP_Lua_TriggerActivate(L: PScriptContext): Integer; cdecl;
function SP_Lua_TriggerGetEnabled(L: PScriptContext): Integer; cdecl;
function SP_Lua_TriggerSetEnabled(L: PScriptContext): Integer; cdecl;
function SP_Lua_TriggerGetPos(L: PScriptContext): Integer; cdecl;
function SP_Lua_TriggerSetPos(L: PScriptContext): Integer; cdecl;

function SP_Lua_PanelGetType(L: PScriptContext): Integer; cdecl;
function SP_Lua_PanelGetPos(L: PScriptContext): Integer; cdecl;
function SP_Lua_PanelGetSize(L: PScriptContext): Integer; cdecl;
function SP_Lua_PanelSetPos(L: PScriptContext): Integer; cdecl;
function SP_Lua_PanelSwitchTexture(L: PScriptContext): Integer; cdecl;

function SP_Lua_DoorGetState(L: PScriptContext): Integer; cdecl;
function SP_Lua_DoorOpen(L: PScriptContext): Integer; cdecl;
function SP_Lua_DoorClose(L: PScriptContext): Integer; cdecl;
function SP_Lua_DoorToggle(L: PScriptContext): Integer; cdecl;
function SP_Lua_DoorCloseTrap(L: PScriptContext): Integer; cdecl;
function SP_Lua_LiftSetDir(L: PScriptContext): Integer; cdecl;
function SP_Lua_LiftGetDir(L: PScriptContext): Integer; cdecl;

function SP_Lua_SpawnShot(L: PScriptContext): Integer; cdecl;
function SP_Lua_SpawnEffect(L: PScriptContext): Integer; cdecl;
function SP_Lua_SpawnMonster(L: PScriptContext): Integer; cdecl;
function SP_Lua_SpawnItem(L: PScriptContext): Integer; cdecl;

implementation

uses
  lua, lauxlib,
  g_player, g_map, Math, g_gfx, g_game, g_textures,
  g_console, g_monsters, g_items, g_phys, g_weapons,
  g_main, SysUtils, e_log, g_language, g_basic,
  g_options, g_net, g_netmsg, g_triggers, g_panel,
  g_sound, MAPDEF;

function CheckArgs(L: PScriptContext; MinArgs: Integer; Error: Boolean = True): Boolean; inline;
begin
  Result := True;
  if lua_gettop(L) < MinArgs then
  begin
    if Error then g_Console_Add('SCRIPT: ERROR: expected at least ' + IntToStr(MinArgs) + ' argument(s)');
    Result := False;
  end;
end;

// system //

function SP_Lua_ConPrint(L: PScriptContext): Integer; cdecl;
// game.con_print(text)
const
  MINARGS = 1;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then Exit;
  g_Console_Add(luaL_checkstring(L, 1));
end;

function SP_Lua_Message(L: PScriptContext): Integer; cdecl;
// game.message(type, text, channel[, uid=0, timeout=144])
const
  MINARGS = 3;
var
  MText: string;
  MChan, MKind, MID, MTime: Integer;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  MKind := luaL_checkinteger(L, 1);
  MText := luaL_checkstring(L, 2);
  MChan := luaL_checkinteger(L, 3);

  MID := lua_tointeger(L, 4);
  MTime := lua_tointeger(L, 5);
  if MTime = 0 then MTime := 144;

  tr_Message(MKind, MText, MChan, MTime, MID);
end;

function SP_Lua_GetGameMode(L: PScriptContext): Integer; cdecl;
// game.get_gamemode()
const
  MINARGS = 0;
begin
  Result := 1;
  lua_pushinteger(L, gGameSettings.GameMode);
end;

function SP_Lua_GetGameType(L: PScriptContext): Integer; cdecl;
// game.get_gametype()
const
  MINARGS = 0;
begin
  Result := 1;
  lua_pushinteger(L, gGameSettings.GameType);
end;

function SP_Lua_GetTime(L: PScriptContext): Integer; cdecl;
// game.get_time()
const
  MINARGS = 0;
begin
  Result := 1;
  lua_pushinteger(L, gTime);
end;

function SP_Lua_PlaySound(L: PScriptContext): Integer; cdecl;
// game.sound(name[positional=false, x=0, y=0])
const
  MINARGS = 1;
var
  SName: string;
  SPos: Boolean;
  SX, SY: Integer;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  SName := luaL_checkstring(L, 1);

  SPos := lua_toboolean(L, 2);
  SX := lua_tointeger(L, 3);
  SY := lua_tointeger(L, 4);

  if SPos then
    g_Sound_PlayExAt(SName, SX, SY)
  else
    g_Sound_PlayEx(SName);

  if g_Game_IsNet then
    MH_SEND_Sound(SX, SY, SName, SPos);
end;

// players //

function SP_Lua_PlayerGetKeys(L: PScriptContext): Integer; cdecl;
// game.player_get_keys(uid)
const
  MINARGS = 1;
var
  UID: Integer;
  P: TPlayer;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  UID := luaL_checkinteger(L, 1);

  if g_GetUIDType(UID) <> UID_PLAYER then
    lua_pushnil(L)
  else
  begin
    P := g_Player_Get(UID);
    if P = nil then
    begin
      lua_pushnil(L);
      exit;
    end;
    lua_pushinteger(L, P.GetKeys);
  end;
end;

function SP_Lua_PlayerGetArmor(L: PScriptContext): Integer; cdecl;
// game.player_get_armor(uid)
const
  MINARGS = 1;
var
  UID: Integer;
  P: TPlayer;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  UID := luaL_checkinteger(L, 1);

  if g_GetUIDType(UID) <> UID_PLAYER then
    lua_pushnil(L)
  else
  begin
    P := g_Player_Get(UID);
    if P = nil then
    begin
      lua_pushnil(L);
      exit;
    end;
    lua_pushinteger(L, P.Armor);
  end;
end;

function SP_Lua_PlayerGetName(L: PScriptContext): Integer; cdecl;
// game.player_get_name(uid)
const
  MINARGS = 1;
var
  UID: Integer;
  P: TPlayer;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  UID := luaL_checkinteger(L, 1);

  if g_GetUIDType(UID) <> UID_PLAYER then
    lua_pushnil(L)
  else
  begin
    P := g_Player_Get(UID);
    if P = nil then
    begin
      lua_pushnil(L);
      exit;
    end;
    lua_pushstring(L, P.Name);
  end;
end;

function SP_Lua_PlayerGetTeam(L: PScriptContext): Integer; cdecl;
// game.player_get_team(uid)
const
  MINARGS = 1;
var
  UID: Integer;
  P: TPlayer;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  UID := luaL_checkinteger(L, 1);

  if g_GetUIDType(UID) <> UID_PLAYER then
    lua_pushnil(L)
  else
  begin
    P := g_Player_Get(UID);
    if P = nil then
    begin
      lua_pushnil(L);
      exit;
    end;
    lua_pushinteger(L, P.Team);
  end;
end;

function SP_Lua_PlayerGetScore(L: PScriptContext): Integer; cdecl;
// game.player_get_score(uid)
const
  MINARGS = 1;
var
  UID: Integer;
  P: TPlayer;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  UID := luaL_checkinteger(L, 1);

  if g_GetUIDType(UID) <> UID_PLAYER then
    lua_pushnil(L)
  else
  begin
    P := g_Player_Get(UID);
    if P = nil then
    begin
      lua_pushnil(L);
      exit;
    end;
    lua_pushinteger(L, P.Frags);
  end;
end;

// actors //

function SP_Lua_ActorGetPos(L: PScriptContext): Integer; cdecl;
// game.uid_get_pos(uid)
const
  MINARGS = 1;
var
  UID: Integer;
  P: TPlayer;
  M: TMonster;
begin
  Result := 2;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  UID := luaL_checkinteger(L, 1);

  if g_GetUIDType(UID) = UID_PLAYER then
  begin
    P := g_Player_Get(UID);
    if P = nil then
    begin
      lua_pushnil(L);
      lua_pushnil(L);
      exit;
    end;
    lua_pushinteger(L, P.GameX + PLAYER_RECT_CX);
    lua_pushinteger(L, P.GameY + PLAYER_RECT_CY);
  end
  else if g_GetUIDType(UID) = UID_MONSTER then
  begin
    M := g_Monsters_Get(UID);
    if M = nil then
    begin
      lua_pushnil(L);
      lua_pushnil(L);
      exit;
    end;
    lua_pushinteger(L, M.Obj.X+M.Obj.Rect.X+(M.Obj.Rect.Width div 2));
    lua_pushinteger(L, M.Obj.Y+M.Obj.Rect.Y+(M.Obj.Rect.Height div 2));
  end
  else
  begin
    lua_pushnil(L);
    lua_pushnil(L);
  end;
end;

function SP_Lua_ActorNearest(L: PScriptContext): Integer; cdecl;
// game.uid_nearest(x, y, [type=all, min, max])
const
  MINARGS = 2;
var
  i, UID, AType: Integer;
  X, Y, Dist, MinDist, MaxDist, FMin: Double;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then 
  begin
    Result := 0;
    exit;
  end;
  
  X := luaL_checkinteger(L, 1);
  Y := luaL_checkinteger(L, 2);

  AType := lua_tointeger(L, 3);
  MinDist := Sqr(lua_tonumber(L, 4));
  MaxDist := Sqr(lua_tonumber(L, 5));
  if MaxDist < 0.01 then MaxDist := 1e15;
  if MinDist < 0.01 then MinDist := 0;
  FMin := MaxDist;
  UID := -1;

  if (AType in [UID_GAME, UID_PLAYER]) and (gPlayers <> nil) then
    for i := 0 to High(gPlayers) do
    begin
      if gPlayers[i] = nil then continue;
      Dist := Sqr(X - gPlayers[i].GameX) + Sqr(Y - gPlayers[i].GameY);
      if (Dist > MinDist) and (Dist < FMin) then
      begin
        UID := gPlayers[i].UID;
        FMin := Dist;
      end;
    end;
  if (AType in [UID_GAME, UID_MONSTER]) and (gMonsters <> nil) then
    for i := 0 to High(gMonsters) do
    begin
      if gMonsters[i] = nil then continue;
      Dist := Sqr(X - gMonsters[i].GameX) + Sqr(Y - gMonsters[i].GameY);
      if (Dist > MinDist) and (Dist < FMin) then
      begin
        UID := gMonsters[i].UID;
        FMin := Dist;
      end;
    end;

  lua_pushinteger(L, UID);
end;

function SP_Lua_ActorFarthest(L: PScriptContext): Integer; cdecl;
// game.uid_farthest(x, y, [type=all, min, max])
const
  MINARGS = 2;
var
  i, UID, AType: Integer;
  X, Y, Dist, MinDist, MaxDist, FMax: Double;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then 
  begin
    Result := 0;
    exit;
  end;

  X := luaL_checkinteger(L, 1);
  Y := luaL_checkinteger(L, 2);

  AType := lua_tointeger(L, 3);
  MinDist := Sqr(lua_tonumber(L, 4));
  MaxDist := Sqr(lua_tonumber(L, 5));
  if MaxDist < 0.01 then MaxDist := 1e15;
  if MinDist < 0.01 then MinDist := 0;
  FMax := MinDist;
  UID := -1;

  if (AType in [UID_GAME, UID_PLAYER]) and (gPlayers <> nil) then
    for i := 0 to High(gPlayers) do
    begin
      if gPlayers[i] = nil then continue;
      Dist := Sqr(X - gPlayers[i].GameX) + Sqr(Y - gPlayers[i].GameY);
      if (Dist < MaxDist) and (Dist > FMax) then
      begin
        UID := gPlayers[i].UID;
        FMax := Dist;
      end;
    end;
  if (AType in [UID_GAME, UID_MONSTER]) and (gMonsters <> nil) then
    for i := 0 to High(gMonsters) do
    begin
      if gMonsters[i] = nil then continue;
      Dist := Sqr(X - gMonsters[i].GameX) + Sqr(Y - gMonsters[i].GameY);
      if (Dist < MaxDist) and (Dist > FMax) then
      begin
        UID := gMonsters[i].UID;
        FMax := Dist;
      end;
    end;

  lua_pushinteger(L, UID);
end;

function SP_Lua_ActorGetType(L: PScriptContext): Integer; cdecl;
// game.uid_type(uid)
const
  MINARGS = 2;
var
  UID, UType: Integer;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then 
  begin
    Result := 0;
    exit;
  end;

  UID := luaL_checkinteger(L, 1);
  if (UID < 0) or (UID > $FFFF) then
    lua_pushnil(L)
  else
  begin
    UType := g_GetUIDType(UID);
    if not (UType in [UID_PLAYER, UID_MONSTER{*, UID_ITEM*}]) then
      lua_pushnil(L)
    else
      lua_pushinteger(L, UType);
  end;
end;

function SP_Lua_ActorDamage(L: PScriptContext): Integer; cdecl;
// game.uid_damage(uid, amount[, hit_type=hit_some, vx=0, vy=0, attacker_uid=0])
const
  MINARGS = 2;
var
  UID, AUID, Dmg, Atk, VX, VY: Integer;
  P: TPlayer;
  M: TMonster;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  UID := luaL_checkinteger(L, 1);
  Dmg := luaL_checkinteger(L, 2);

  Atk := lua_tointeger(L, 3);
  VX := lua_tointeger(L, 4);
  VY := lua_tointeger(L, 5);
  AUID := lua_tointeger(L, 6);

  if g_GetUIDType(UID) = UID_PLAYER then
  begin
    P := g_Player_Get(UID);
    if P = nil then exit;
    if Dmg >= 0 then
      P.Damage(Dmg, AUID, VX, VY, Atk)
    else
      P.Heal(-Dmg, False);
  end
  else if g_GetUIDType(UID) = UID_MONSTER then
  begin
    M := g_Monsters_Get(UID);
    if M = nil then exit;
    if Dmg >= 0 then
      M.Damage(Dmg, AUID, VX, VY, Atk)
    else
      M.Heal(-Dmg);
  end;
end;

function SP_Lua_ActorGetHealth(L: PScriptContext): Integer; cdecl;
// game.uid_get_health(uid)
const
  MINARGS = 1;
var
  UID: Integer;
  P: TPlayer;
  M: TMonster;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  UID := luaL_checkinteger(L, 1);

  if g_GetUIDType(UID) = UID_PLAYER then
  begin
    P := g_Player_Get(UID);
    if P = nil then
    begin
      lua_pushnil(L);
      exit;
    end;
    lua_pushinteger(L, P.Health);
  end
  else if g_GetUIDType(UID) = UID_MONSTER then
  begin
    M := g_Monsters_Get(UID);
    if M = nil then
    begin
      lua_pushnil(L);
      exit;
    end;
    lua_pushinteger(L, M.MonsterHealth);
  end
  else
    lua_pushnil(L);
end;

function SP_Lua_ActorGetState(L: PScriptContext): Integer; cdecl;
// game.uid_get_state(uid)
const
  MINARGS = 1;
var
  UID: Integer;
  P: TPlayer;
  M: TMonster;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  UID := luaL_checkinteger(L, 1);

  if g_GetUIDType(UID) = UID_PLAYER then
  begin
    P := g_Player_Get(UID);
    if P = nil then
    begin
      lua_pushnil(L);
      exit;
    end;
    if P.Live then
      lua_pushinteger(L, 1)
    else if P.FSpectator then
      lua_pushinteger(L, -1)
    else
      lua_pushinteger(L, 0);
  end
  else if g_GetUIDType(UID) = UID_MONSTER then
  begin
    M := g_Monsters_Get(UID);
    if M = nil then
    begin
      lua_pushnil(L);
      exit;
    end;
    lua_pushinteger(L, IfThen(M.Live, 1, 0));
  end
  else
    lua_pushnil(L);
end;

function SP_Lua_ActorTeleport(L: PScriptContext): Integer; cdecl;
// game.uid_teleport(uid, to_x, to_y[, dir=left, silent=false, d2d_like=false])
const
  MINARGS = 3;
var
  D2D, NoSound: Boolean;
  UID, TX, TY: Integer;
  TDir: Integer;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  UID := luaL_checkinteger(L, 1);
  TX := luaL_checkinteger(L, 2);
  TY := luaL_checkinteger(L, 3);

  TDir := lua_tointeger(L, 4) mod 2;
  NoSound := lua_toboolean(L, 5);
  D2D := lua_toboolean(L, 6);

  tr_Teleport(UID, TX, TY, TDir, NoSound, D2D);
end;

function SP_Lua_ActorPush(L: PScriptContext): Integer; cdecl;
// game.uid_push(uid, vx, vy[, reset_vel=false])
const
  MINARGS = 3;
var
  ResetVel: Boolean;
  UID, VX, VY: Integer;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  UID := luaL_checkinteger(L, 1);
  VX := luaL_checkinteger(L, 2);
  VY := luaL_checkinteger(L, 3);

  ResetVel := lua_toboolean(L, 4);

  tr_Push(UID, VX, VY, ResetVel);
end;

// triggers //

function SP_Lua_TriggerSetEnabled(L: PScriptContext): Integer; cdecl;
// game.trigger_set_enabled(id, active)
const
  MINARGS = 2;
var
  TID: Integer;
  Enabled: Boolean;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  TID := luaL_checkinteger(L, 1);
  Enabled := lua_toboolean(L, 2);

  if (TID < 0) or (TID > High(gTriggers)) then exit;
  if gTriggers[TID].TriggerType = TRIGGER_NONE then exit;
  gTriggers[TID].Enabled := Enabled;
end;

function SP_Lua_TriggerGetEnabled(L: PScriptContext): Integer; cdecl;
// game.trigger_get_enabled(id)
const
  MINARGS = 1;
var
  TID: Integer;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then 
  begin
    Result := 0;
    exit;
  end;

  TID := luaL_checkinteger(L, 1);

  if (TID < 0) or (TID > High(gTriggers)) or (gTriggers[TID].TriggerType = TRIGGER_NONE) then
    lua_pushnil(L)
  else
    lua_pushboolean(L, gTriggers[TID].Enabled);
end;

function SP_Lua_TriggerActivate(L: PScriptContext): Integer; cdecl;
// game.trigger_activate(id[, activate_type=custom, activator_uid=0])
const
  MINARGS = 1;
var
  TID, UID: Integer;
  TAct: Byte;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  TID := luaL_checkinteger(L, 1);

  TAct := lua_tointeger(L, 2);
  if TAct = 0 then TAct := 255;
  UID := lua_tointeger(L, 3);

  if (TID < 0) or (TID > High(gTriggers)) then exit;
  if gTriggers[TID].TriggerType = TRIGGER_NONE then exit;
  if ByteBool(gTriggers[TID].ActivateType and TAct) then
    g_Triggers_Press(TID, TAct, UID);
end;

function SP_Lua_TriggerGetPos(L: PScriptContext): Integer; cdecl;
// game.trigger_get_pos(id)
const
  MINARGS = 1;
var
  TID: Integer;
begin
  Result := 2;
  if not CheckArgs(L, MINARGS) then 
  begin
    Result := 0;
    exit;
  end;

  TID := luaL_checkinteger(L, 1);

  if (TID < 0) or (TID > High(gTriggers)) or (gTriggers[TID].TriggerType = TRIGGER_NONE) then
  begin
    lua_pushnil(L);
    lua_pushnil(L);
  end
  else
  begin
    lua_pushinteger(L, gTriggers[TID].X);
    lua_pushinteger(L, gTriggers[TID].Y);
  end;
end;

function SP_Lua_TriggerSetPos(L: PScriptContext): Integer; cdecl;
// game.trigger_set_pos(id, x, y)
const
  MINARGS = 3;
var
  TID, X, Y: Integer;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  TID := luaL_checkinteger(L, 1);
  X := luaL_checkinteger(L, 2);
  Y := luaL_checkinteger(L, 3);

  if (TID < 0) or (TID > High(gTriggers)) then exit;
  if gTriggers[TID].TriggerType in [TRIGGER_NONE, TRIGGER_SOUND] then exit;
  gTriggers[TID].X := X;
  gTriggers[TID].Y := Y;
end;

// doors & panels //

function SP_Lua_PanelGetType(L: PScriptContext): Integer; cdecl;
// game.panel_get_type(panel_id)
const
  MINARGS = 1;
var
  PID, WID: Integer;
  Pan: PPanel;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  PID := luaL_checkinteger(L, 1);

  WID := -1;
  Pan := g_Map_PanelForPID(PID, WID);
  if (Pan = nil) then
    lua_pushnil(L)
  else
    lua_pushinteger(L, Pan^.PanelType);
end;

function SP_Lua_PanelGetPos(L: PScriptContext): Integer; cdecl;
// game.panel_get_pos(panel_id)
const
  MINARGS = 1;
var
  PID, WID: Integer;
  Pan: PPanel;
begin
  Result := 2;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  PID := luaL_checkinteger(L, 1);

  WID := -1;
  Pan := g_Map_PanelForPID(PID, WID);
  if (Pan = nil) then
  begin
    lua_pushnil(L);
    lua_pushnil(L);
  end
  else
  begin
    lua_pushinteger(L, Pan^.X);
    lua_pushinteger(L, Pan^.Y);
  end;
end;

function SP_Lua_PanelGetSize(L: PScriptContext): Integer; cdecl;
// game.panel_get_size(panel_id)
const
  MINARGS = 1;
var
  PID, WID: Integer;
  Pan: PPanel;
begin
  Result := 2;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  PID := luaL_checkinteger(L, 1);

  WID := -1;
  Pan := g_Map_PanelForPID(PID, WID);
  if (Pan = nil) then
  begin
    lua_pushnil(L);
    lua_pushnil(L);
  end
  else
  begin
    lua_pushinteger(L, Pan^.Width);
    lua_pushinteger(L, Pan^.Height);
  end;
end;

function SP_Lua_PanelSwitchTexture(L: PScriptContext): Integer; cdecl;
// game.panel_switch_texture(panel_id)
const
  MINARGS = 1;
var
  PID, WID: Integer;
  Pan: PPanel;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  PID := luaL_checkinteger(L, 1);

  WID := -1;
  Pan := g_Map_PanelForPID(PID, WID);
  if (Pan = nil) then exit;
  g_Map_SwitchTexture(Pan^.PanelType, WID, 0);
end;

function SP_Lua_PanelSetPos(L: PScriptContext): Integer; cdecl;
// game.panel_set_pos(panel_id, x, y)
const
  MINARGS = 3;
var
  PID, WID, X, Y: Integer;
  Pan: PPanel;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  PID := luaL_checkinteger(L, 1);
  X := luaL_checkinteger(L, 2);
  Y := luaL_checkinteger(L, 3);

  WID := -1;
  Pan := g_Map_PanelForPID(PID, WID);
  if (Pan = nil) then exit;
  if not (Pan.PanelType in [PANEL_BACK, PANEL_FORE]) then
  begin
    g_Console_Add('SCRIPT: panel_set_pos(): can only set pos for BACK and FORE right now');
    exit;
  end;
  Pan^.X := X;
  Pan^.Y := Y;
  if g_Game_IsNet then
    MH_SEND_PanelState(Pan^.PanelType, WID);
end;

function SP_Lua_DoorGetState(L: PScriptContext): Integer; cdecl;
// game.door_get_open(panel_id)
const
  MINARGS = 1;
var
  PID, WID: Integer;
  Pan: PPanel;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  PID := luaL_checkinteger(L, 1);

  WID := -1;
  Pan := g_Map_PanelForPID(PID, WID);
  if (Pan = nil) or (not Pan^.Door) then
    lua_pushnil(L)
  else
    lua_pushboolean(L, Pan^.Enabled);
end;

function SP_Lua_LiftGetDir(L: PScriptContext): Integer; cdecl;
// game.lift_get_dir(panel_id)
const
  MINARGS = 1;
var
  PID, WID: Integer;
  Pan: PPanel;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  PID := luaL_checkinteger(L, 1);

  WID := -1;
  Pan := g_Map_PanelForPID(PID, WID);
  if (Pan = nil) or ((Pan^.LiftType = 0) and (Pan^.PanelType <> PANEL_LIFTUP)) then
    lua_pushnil(L)
  else
    lua_pushinteger(L, Pan^.LiftType);
end;

function SP_Lua_DoorOpen(L: PScriptContext): Integer; cdecl;
// game.door_open(panel_id[, silent=false, d2d_like=false])
const
  MINARGS = 1;
var
  D2D, NoSound: Boolean;
  PID, WID: Integer;
  Pan: PPanel;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  PID := luaL_checkinteger(L, 1);
  NoSound := lua_toboolean(L, 2);
  D2D := lua_toboolean(L, 3);

  WID := -1;
  Pan := g_Map_PanelForPID(PID, WID);
  if (Pan = nil) or (not Pan^.Door) then exit;
  tr_OpenDoor(WID, NoSound, D2D);
end;

function SP_Lua_DoorClose(L: PScriptContext): Integer; cdecl;
// game.door_close(panel_id[, silent=false, d2d_like=false])
const
  MINARGS = 1;
var
  D2D, NoSound: Boolean;
  PID, WID: Integer;
  Pan: PPanel;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  PID := luaL_checkinteger(L, 1);
  NoSound := lua_toboolean(L, 2);
  D2D := lua_toboolean(L, 3);

  WID := -1;
  Pan := g_Map_PanelForPID(PID, WID);
  if (Pan = nil) or (not Pan^.Door) then exit;
  tr_CloseDoor(WID, NoSound, D2D);
end;

function SP_Lua_DoorToggle(L: PScriptContext): Integer; cdecl;
// game.door_toggle(panel_id[, silent=false, d2d_like=false])
const
  MINARGS = 1;
var
  D2D, NoSound: Boolean;
  PID, WID: Integer;
  Pan: PPanel;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  PID := luaL_checkinteger(L, 1);
  NoSound := lua_toboolean(L, 2);
  D2D := lua_toboolean(L, 3);

  WID := -1;
  Pan := g_Map_PanelForPID(PID, WID);
  if (Pan = nil) or (not Pan^.Door) then exit;
  if gWalls[WID].Enabled then
    tr_OpenDoor(WID, NoSound, D2D)
  else
    tr_CloseDoor(WID, NoSound, D2D);
end;

function SP_Lua_DoorCloseTrap(L: PScriptContext): Integer; cdecl;
// game.door_close_trap(panel_id[, silent=false, d2d_like=false])
const
  MINARGS = 1;
var
  D2D, NoSound: Boolean;
  PID, WID: Integer;
  Pan: PPanel;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  PID := luaL_checkinteger(L, 1);
  NoSound := lua_toboolean(L, 2);
  D2D := lua_toboolean(L, 3);

  WID := -1;
  Pan := g_Map_PanelForPID(PID, WID);
  if (Pan = nil) or (not Pan^.Door) then exit;
  tr_CloseTrap(WID, NoSound, D2D);
end;

function SP_Lua_LiftSetDir(L: PScriptContext): Integer; cdecl;
// game.lift_set_dir(panel_id, dir[, silent=false, d2d_like=false])
const
  MINARGS = 2;
var
  D2D, NoSound: Boolean;
  PID, WID, Dir: Integer;
  Pan: PPanel;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  PID := luaL_checkinteger(L, 1);
  Dir := luaL_checkinteger(L, 2);
  NoSound := lua_toboolean(L, 3);
  D2D := lua_toboolean(L, 4);

  WID := -1;
  Pan := g_Map_PanelForPID(PID, WID);
  if (Pan = nil) or ((Pan^.LiftType = 0) and (Pan^.PanelType <> PANEL_LIFTUP)) then
    exit;
  tr_SetLift(WID, Dir, NoSound, D2D);
end;

// spawners //

function SP_Lua_SpawnShot(L: PScriptContext): Integer; cdecl;
// game.spawn_shot(type, x, y, vel_x, vel_y[, silent=false, target_id=0(noone)])
const
  MINARGS = 5;
var
  wx, wy, dx, dy, ShotType, ShotTarget: Integer;
  Silent: Boolean;
  ShotTargetW: Word;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  ShotType := luaL_checkinteger(L, 1);
  wx := luaL_checkinteger(L, 2);
  wy := luaL_checkinteger(L, 3);
  dx := luaL_checkinteger(L, 4);
  dy := luaL_checkinteger(L, 5);

  Silent := lua_toboolean(L, 6);
  ShotTarget := lua_tointeger(L, 7);
  ShotTargetW := 0;
  if (ShotTarget > 0) and (ShotTarget < $FFFF) then
    ShotTargetW := ShotTarget;

  lua_pushinteger(L, tr_SpawnShot(ShotType, wx, wy, dx, dy, not Silent, ShotTargetW));
end;

function SP_Lua_SpawnEffect(L: PScriptContext): Integer; cdecl;
// game.spawn_effect(type, subtype, x, y, vel_x, vel_y[, silent=false, r=0, g=0, b=0])
const
  MINARGS = 6;
var
  X, Y, VX, VY: Integer;
  EType, ESubType: Integer;
  ER, EG, EB: Integer;
  Silent: Boolean;
begin
  Result := 0;
  if not CheckArgs(L, MINARGS) then exit;

  EType := luaL_checkinteger(L, 1);
  ESubType := luaL_checkinteger(L, 2);
  X := luaL_checkinteger(L, 3);
  Y := luaL_checkinteger(L, 4);
  VX := luaL_checkinteger(L, 5);
  VY := luaL_checkinteger(L, 6);

  Silent := lua_toboolean(L, 7);
  ER := lua_tointeger(L, 8);
  EG := lua_tointeger(L, 8);
  EB := lua_tointeger(L, 8);

  tr_MakeEffect(X, Y, VX, VY, EType, ESubType, ER, EG, EB, Silent, True);
end;

function SP_Lua_SpawnMonster(L: PScriptContext): Integer; cdecl;
// game.spawn_monster(type, x, y[, dir=left, awake=false, health=default, aitype=normal])
const
  MINARGS = 3;
var
  X, Y: Integer;
  Dir: TDirection;
  MType, MBType, MHP: Integer;
  MAwake: Boolean;
  MID, i: Integer;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  MType := luaL_checkinteger(L, 1);
  X := luaL_checkinteger(L, 2);
  Y := luaL_checkinteger(L, 3);

  Dir := TDirection(lua_tointeger(L, 4) mod 2);
  MAwake := lua_toboolean(L, 5);
  MHP := lua_tointeger(L, 6);
  MBType := lua_tointeger(L, 7);

  MID := -1;

  if MType in [MONSTER_DEMON..MONSTER_MAN] then
  begin
    i := g_Monsters_Create(MType, X, Y, Dir, True);
    if i < 0 then exit;
    if MHP > 0 then
      gMonsters[i].SetHealth(MHP);
    gMonsters[i].MonsterBehaviour := MBType;
    gMonsters[i].FNoRespawn := True;
    if g_Game_IsNet then
      MH_SEND_MonsterSpawn(gMonsters[i].UID);
    if MAwake then
      gMonsters[i].WakeUp();
    if MType <> MONSTER_BARREL then Inc(gTotalMonsters);
    if g_Game_IsNet then
    begin
      SetLength(gMonstersSpawned, Length(gMonstersSpawned)+1);
      gMonstersSpawned[High(gMonstersSpawned)] := gMonsters[i].UID;
      MH_SEND_GameStats();
      MH_SEND_CoopStats();
    end;
    MID := gMonsters[i].UID;
  end;

  lua_pushinteger(L, MID);
end;

function SP_Lua_SpawnItem(L: PScriptContext): Integer; cdecl;
// game.spawn_item(type, x, y[, falls=false])
const
  MINARGS = 3;
var
  X, Y: Integer;
  IType: Integer;
  IFalls: Boolean;
  i: Integer;
begin
  Result := 1;
  if not CheckArgs(L, MINARGS) then
  begin
    Result := 0;
    exit;
  end;

  IType := luaL_checkinteger(L, 1);
  X := luaL_checkinteger(L, 2);
  Y := luaL_checkinteger(L, 3);

  IFalls := lua_toboolean(L, 4);

  i := -1;
  if IType in [ITEM_MEDKIT_SMALL..ITEM_MAX] then
  begin
    i := g_Items_Create(X, Y, IType, IFalls, False, True);
    if g_Game_IsNet then
      MH_SEND_ItemSpawn(True, i);
  end;

  lua_pushinteger(L, i);
end;

end.
