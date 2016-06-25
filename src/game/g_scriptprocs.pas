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
begin
  g_Console_Add(lua_tostring(L, -1));
  Result := 0;
end;

end.
