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
unit e_sound;

{$IF DEFINED(USE_SDLMIXER)}
  {$I e_sound_sdl.inc}
{$ELSEIF DEFINED(USE_OPENAL)}
  {$I e_sound_al.inc}
{$ELSEIF DEFINED(USE_FMOD)}
  {$I e_sound_fmod.inc}
{$ELSEIF DEFINED(USE_SOUNDSTUB)}
  {$I e_sound_stub.inc}
{$ELSE}
  {$ERROR e_sound driver not implemented?}
{$ENDIF}
