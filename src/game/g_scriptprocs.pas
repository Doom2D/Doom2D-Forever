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

implementation

uses
<<<<<<< HEAD
  lua, g_console;
=======
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
>>>>>>> 3132944... made TRIGGER_SCRIPT work

function SP_Lua_ConPrint(L: PScriptContext): Integer; cdecl;
<<<<<<< HEAD
=======
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
  FMin := MinDist;
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
>>>>>>> 081369f... scripts: uid_get_pos() now returns center
begin
  g_Console_Add(lua_tostring(L, -1));
  Result := 0;
end;

end.
