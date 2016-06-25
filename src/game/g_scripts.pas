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
  RESET_ALL  = 0;
  RESET_SRV  = 1;
  RESET_WAD  = 2;
  RESET_MAP  = 3;

type
  PScriptContext = Plua_State;
  PScriptProc = lua_CFunction;

var
  gScriptCtx: PScriptContext = nil;
  gScriptInit: Boolean = False;

function g_Scripts_Init(): Boolean;
procedure g_Scripts_Reset(What: Integer);
function g_Scripts_ProcExec(PName: string; const Args: array of const; Namespace: string = ''): Integer;
function g_Scripts_ProcExists(PName: string): Boolean;
function g_Scripts_ProcInstall(PName: string; PPtr: PScriptProc): Boolean;
function g_Scripts_Load(Text: string): Boolean;
procedure g_Scripts_Free();

implementation

uses
  SysUtils, g_console, g_scriptprocs;

type
  POpenFunc = function(L: Plua_State): LongBool; cdecl;
  TLuaReg = record
    name: PAnsiChar;
    func: POpenFunc;
  end;

const
  LUA_LIBS: array [0..3] of TLuaReg = (
    (name: ''; func: luaopen_base),
    (name: LUA_TABLIBNAME; func: luaopen_table),
    (name: LUA_STRLINAME; func: luaopen_string), // STRLINAME is actually a typo in fpc's lua module
    (name: LUA_MATHLIBNAME; func: luaopen_math)
  );

function LuaInstallGameFuncs(): Boolean;
begin
  Result := False;

  if not g_Scripts_ProcInstall('conprint', SP_Lua_ConPrint) then Exit;

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
    //lua_pushcfunction(gScriptCtx, LUA_LIBS[i].func);
    lua_pushstring(gScriptCtx, LUA_LIBS[i].name);
    //lua_call(gScriptCtx, 1, 0);
    LUA_LIBS[i].func(gScriptCtx);
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
procedure g_Scripts_Reset(What: Integer);
begin
  if not gScriptInit then Exit;
  if What in [RESET_ALL, RESET_SRV] then
  begin
    lua_newtable(gScriptCtx);
    lua_setglobal(gScriptCtx, 'srv');
  end;
  if What in [RESET_ALL, RESET_WAD] then
  begin
    lua_newtable(gScriptCtx);
    lua_setglobal(gScriptCtx, 'wad');
  end;
  if What in [RESET_ALL, RESET_MAP] then
  begin
    lua_newtable(gScriptCtx);
    lua_setglobal(gScriptCtx, 'map');
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
