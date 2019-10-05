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
unit e_input;

{$IF DEFINED(USE_SYSSTUB) OR DEFINED(USE_SDL)}
  {$I e_input_stub.inc}
{$ELSEIF DEFINED(USE_SDL2)}
  {$I e_input_sdl2.inc}
{$ELSE}
  {$ERROR e_input driver not implemented?}
{$ENDIF}

end.
