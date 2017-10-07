(* Copyright (C)  Doom 2D: Forever Developers
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
{$INCLUDE a_modes.inc}
{.$MODE OBJFPC}
unit envvars;

interface

uses SysUtils, CTypes;

function SetEnvVar(const VarName: AnsiString; const VarVal: AnsiString): Boolean;

implementation

{$IFDEF WINDOWS}
uses Windows;
function setenv(const VarStr: PChar; const VarVal: PChar; Repl: cint): cint;
begin
  if (SetEnvironmentVariable(VarStr, VarVal)) then
    Result := 0
  else
    Result := -1;
end;
{$ELSE}
{$LINKLIB c}
const clib = 'c';
function setenv(const VarStr: PChar; const VarVal: PChar; Repl: cint): cint;
cdecl; external clib name 'setenv';
{$ENDIF}

function SetEnvVar(const VarName: AnsiString; const VarVal: AnsiString): Boolean;
begin
  Result := (setenv(PChar(VarName), PChar(VarVal), 1) = 0);
end;

end.
