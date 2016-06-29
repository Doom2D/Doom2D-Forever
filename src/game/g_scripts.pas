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
unit g_scripts;

interface

uses 
  lua, lualib, lauxlib;

const
  // reset levels
  RESET_SRV_BIT  = 4;
  RESET_WAD_BIT  = 2;
  RESET_MAP_BIT  = 1;
  RESET_SRV      = RESET_SRV_BIT or RESET_WAD_BIT or RESET_MAP_BIT;
  RESET_WAD      = RESET_WAD_BIT or RESET_MAP_BIT;
  RESET_MAP      = RESET_MAP_BIT;
  RESET_ALL      = 255;

type
  PScriptContext = Plua_State;
  PScriptProc = lua_CFunction;

var
  gScriptCtx: PScriptContext = nil;
  gScriptInit: Boolean = False;

function g_Scripts_Init(): Boolean;
procedure g_Scripts_Reset(What: Byte);
function g_Scripts_ProcExec(PName: string; const Args: array of const; Namespace: string = ''): Integer;
function g_Scripts_ProcExists(PName: string): Boolean;
function g_Scripts_ProcInstall(PName: string; PPtr: PScriptProc): Boolean;
function g_Scripts_Load(Text: string): Boolean;
procedure g_Scripts_Free();

implementation

uses
  SysUtils, g_console, g_scriptprocs;

type
  TLuaReg = record
    name: PAnsiChar;
    func: lua_CFunction;
  end;

const
  LUA_LIBS: array [0..6] of TLuaReg = (
    (name: ''; func: luaopen_base),
    (name: LUA_LOADLIBNAME; func: luaopen_package),
    (name: LUA_TABLIBNAME; func: luaopen_table),
    (name: LUA_STRLIBNAME; func: luaopen_string),
    (name: LUA_MATHLIBNAME; func: luaopen_math),
    (name: LUA_OSLIBNAME; func: luaopen_os), // TODO: something to restrict these two
    (name: LUA_IOLIBNAME; func: luaopen_io)
  );

function LuaInstallGameFuncs(): Boolean;
begin
  Result := False;

  if not g_Scripts_ProcInstall('conprint', SP_Lua_ConPrint) then Exit;
  if not g_Scripts_ProcInstall('message', SP_Lua_Message) then Exit;
  if not g_Scripts_ProcInstall('sound', SP_Lua_PlaySound) then Exit;
  if not g_Scripts_ProcInstall('get_gamemode', SP_Lua_GetGameMode) then Exit;
  if not g_Scripts_ProcInstall('get_gametype', SP_Lua_GetGameType) then Exit;
  if not g_Scripts_ProcInstall('get_time', SP_Lua_GetTime) then Exit;

  if not g_Scripts_ProcInstall('player_get_keys', SP_Lua_PlayerGetKeys) then Exit;
  if not g_Scripts_ProcInstall('player_get_armor', SP_Lua_PlayerGetArmor) then Exit;
  if not g_Scripts_ProcInstall('player_get_score', SP_Lua_PlayerGetScore) then Exit;
  if not g_Scripts_ProcInstall('player_get_name', SP_Lua_PlayerGetName) then Exit;
  if not g_Scripts_ProcInstall('player_get_team', SP_Lua_PlayerGetTeam) then Exit;

  if not g_Scripts_ProcInstall('uid_get_health', SP_Lua_ActorGetHealth) then Exit;
  if not g_Scripts_ProcInstall('uid_get_pos', SP_Lua_ActorGetPos) then Exit;
  if not g_Scripts_ProcInstall('uid_get_state', SP_Lua_ActorGetState) then Exit;
  if not g_Scripts_ProcInstall('uid_get_type', SP_Lua_ActorGetType) then Exit;
  if not g_Scripts_ProcInstall('uid_nearest', SP_Lua_ActorNearest) then Exit;
  if not g_Scripts_ProcInstall('uid_farthest', SP_Lua_ActorFarthest) then Exit;
  if not g_Scripts_ProcInstall('uid_damage', SP_Lua_ActorDamage) then Exit;
  if not g_Scripts_ProcInstall('uid_push', SP_Lua_ActorPush) then Exit;
  if not g_Scripts_ProcInstall('uid_teleport', SP_Lua_ActorTeleport) then Exit;

  if not g_Scripts_ProcInstall('trigger_get_enabled', SP_Lua_TriggerGetEnabled) then Exit;
  if not g_Scripts_ProcInstall('trigger_set_enabled', SP_Lua_TriggerSetEnabled) then Exit;
  if not g_Scripts_ProcInstall('trigger_activate', SP_Lua_TriggerActivate) then Exit;
  if not g_Scripts_ProcInstall('trigger_get_pos', SP_Lua_TriggerGetPos) then Exit;
  if not g_Scripts_ProcInstall('trigger_set_pos', SP_Lua_TriggerSetPos) then Exit;

  if not g_Scripts_ProcInstall('panel_get_type', SP_Lua_PanelGetType) then Exit;
  if not g_Scripts_ProcInstall('panel_get_pos', SP_Lua_PanelGetPos) then Exit;
  if not g_Scripts_ProcInstall('panel_get_size', SP_Lua_PanelGetSize) then Exit;
  if not g_Scripts_ProcInstall('panel_set_pos', SP_Lua_PanelSetPos) then Exit;
  if not g_Scripts_ProcInstall('panel_switch_texture', SP_Lua_PanelSwitchTexture) then Exit;

  if not g_Scripts_ProcInstall('door_get_open', SP_Lua_DoorGetState) then Exit;
  if not g_Scripts_ProcInstall('door_close', SP_Lua_DoorClose) then Exit;
  if not g_Scripts_ProcInstall('door_close_trap', SP_Lua_DoorCloseTrap) then Exit;
  if not g_Scripts_ProcInstall('door_open', SP_Lua_DoorOpen) then Exit;
  if not g_Scripts_ProcInstall('door_toggle', SP_Lua_DoorToggle) then Exit;
  if not g_Scripts_ProcInstall('lift_get_dir', SP_Lua_LiftGetDir) then Exit;
  if not g_Scripts_ProcInstall('lift_set_dir', SP_Lua_LiftSetDir) then Exit;

  if not g_Scripts_ProcInstall('spawn_item', SP_Lua_SpawnItem) then Exit;
  if not g_Scripts_ProcInstall('spawn_shot', SP_Lua_SpawnShot) then Exit;
  if not g_Scripts_ProcInstall('spawn_effect', SP_Lua_SpawnEffect) then Exit;
  if not g_Scripts_ProcInstall('spawn_monster', SP_Lua_SpawnMonster) then Exit;

  Result := True;
end;

function g_Scripts_Init(): Boolean;
var
  i: Integer;
begin
  Result := False;
  if gScriptInit then Exit;

  gScriptCtx := luaL_newstate();
  if gScriptCtx = nil then Exit;

  // don't open all the libs
  for i := 0 to High(LUA_LIBS) do
  begin
    lua_pushcfunction(gScriptCtx, LUA_LIBS[i].func);
    lua_pushstring(gScriptCtx, LUA_LIBS[i].name);
    lua_call(gScriptCtx, 1, 0);
  end;

  // create a table for game-related functions
  lua_newtable(gScriptCtx);
  lua_setglobal(gScriptCtx, 'game');

  // create game-related tables
  g_Scripts_Reset(RESET_ALL);
  
  gScriptInit := True;
  // try to install game-related shit
  if not LuaInstallGameFuncs() then
  begin
    g_Console_Add('SCRIPT: Could not init game callbacks');
    lua_close(gScriptCtx);
    gScriptCtx := nil;
    gScriptInit := False;
    Exit;
  end;

  Result := True;
end;

// TODO: maybe actually put some fields into these?
procedure g_Scripts_Reset(What: Byte);
begin
  if not gScriptInit then Exit;
  if LongBool(What and RESET_SRV_BIT) then
  begin
    lua_newtable(gScriptCtx);
    lua_setglobal(gScriptCtx, 'srv');
  end;
  if LongBool(What and RESET_WAD_BIT) then
  begin
    lua_newtable(gScriptCtx);
    lua_setglobal(gScriptCtx, 'wad');
  end;
  if LongBool(What and RESET_MAP_BIT) then
  begin
    lua_newtable(gScriptCtx);
    lua_setglobal(gScriptCtx, 'map');

    // disable io and os modules on any kind of reset
    lua_pushnil(gScriptCtx);
    lua_pushnil(gScriptCtx);
    lua_setglobal(gScriptCtx, 'os');
    lua_setglobal(gScriptCtx, 'io');
  end;
end;

function g_Scripts_ProcInstall(PName: string; PPtr: PScriptProc): Boolean;
begin
  Result := False;
  if not gScriptInit then Exit;

  if g_Scripts_ProcExists(PName) then
  begin
    g_Console_Add('SCRIPT: ProcInstall(' + PName + '): function already exists');
    Exit;
  end;

  lua_getglobal(gScriptCtx, 'game');
  lua_pushstring(gScriptCtx, PName);
  lua_pushcfunction(gScriptCtx, PPtr);
  lua_settable(gScriptCtx, -3);
  lua_setglobal(gScriptCtx, 'game');

  Result := True;
end;

function g_Scripts_ProcExists(PName: string): Boolean;
begin
  Result := False;
  if not gScriptInit then Exit;

  lua_getglobal(gScriptCtx, 'game');
  lua_pushstring(gScriptCtx, PName);
  lua_gettable(gScriptCtx, -2);
  
  if lua_isfunction(gScriptCtx, -1) then
    Result := True;
end;

function g_Scripts_ProcExec(PName: string; const Args: array of const; Namespace: string = ''): Integer;
var
  i: Integer;
begin
  Result := -255;
  if not gScriptInit then Exit;

  if Namespace = '' then
    lua_getglobal(gScriptCtx, PChar(PName))
  else
  begin
    lua_getglobal(gScriptCtx, PChar(Namespace));
    lua_pushstring(gScriptCtx, PName);
    lua_gettable(gScriptCtx, -2);
  end;

  if not lua_isfunction(gScriptCtx, -1) then
  begin
    g_Console_Add('SCRIPT: ProcExec(' + Namespace + '.' + PName + ') error: no such function');
    Exit;
  end;

  for i := 0 to High(Args) do
    with Args[i] do
    begin
      case VType of
        vtInteger: lua_pushinteger(gScriptCtx, vInteger);
        vtBoolean: lua_pushboolean(gScriptCtx, vBoolean);
        vtString: lua_pushstring(gScriptCtx, vString^);
        vtAnsiString: lua_pushstring(gScriptCtx, PAnsiString(vAnsiString)^);
        vtExtended: lua_pushnumber(gScriptCtx, vExtended^);
      end;
    end;

  if lua_pcall(gScriptCtx, Length(Args), 1, 0) <> 0 then
  begin
    g_Console_Add('SCRIPT: ProcExec(' + Namespace + '.' + PName + ') error: ' + lua_tostring(gScriptCtx, -1));
    Exit;
  end;

  Result := 0;
  if lua_isnumber(gScriptCtx, -1) then
  begin
    Result := lua_tointeger(gScriptCtx, -1);
    lua_pop(gScriptCtx, 1);
  end;
end;

function g_Scripts_Load(Text: string): Boolean;
begin
  Result := False;
  if not gScriptInit then Exit;

  if lua_dostring(gScriptCtx, PChar(Text)) <> 0 then
  begin
    g_Console_Add('SCRIPT: Load() error: ' + lua_tostring(gScriptCtx, -1));
    Exit;
  end;

  Result := True;
end;

procedure g_Scripts_Free();
begin
  if not gScriptInit then Exit;
  lua_close(gScriptCtx);
  gScriptInit := False;
  gScriptCtx := nil;
end;

end.
