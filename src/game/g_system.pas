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
unit g_system;

interface

uses utils;

function sys_HandleEvents (): Boolean;
procedure sys_RequestQuit ();
procedure sys_YieldTimeSlice (); inline;
procedure sys_Delay (ms: Integer); inline;

// use signed type as a result because timer can be not monotonic
function sys_GetTicks (): Int64; inline;

procedure sys_EnableVSync (yes: Boolean);
function sys_GetDisplayModes (bpp: Integer): SSArray;
function sys_SetDisplayMode (w, h, bpp: Integer; fullscreen, maximized: Boolean): Boolean;
procedure sys_Repaint ();

procedure sys_Init ();
procedure sys_Final ();

////////////////////////////////////////////////////////////////////////////////////////////////////

{$IFDEF USE_SYSSTUB}
  {$INCLUDE ./stub/system.inc}
{$ENDIF}
{$IFDEF USE_SDL}
  {$INCLUDE ./sdl/system.inc}
{$ENDIF}
{$IFDEF USE_SDL2}
  {$INCLUDE ./sdl2/system.inc}
{$ENDIF}

end.
