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
unit e_sound;

{$IFDEF USE_SDLMIXER}
  {$I e_sound_sdl.inc}
{$ELSE}
  {$IFDEF USE_OPENAL}
    {$I e_sound_al.inc}
  {$ELSE}
    {$I e_sound_fmod.inc}
  {$ENDIF}
{$ENDIF}
