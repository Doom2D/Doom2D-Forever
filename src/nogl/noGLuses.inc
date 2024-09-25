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

{$IF DEFINED(USE_GLES1) OR DEFINED(USE_GLSTUB)}
  {$UNDEF NOGL_LISTS}
  {$DEFINE NOGL_INIT}
  noGL in '../nogl/noGL.pas',
{$ENDIF}
{$IFDEF USE_OPENGL}
  {$DEFINE NOGL_LISTS}
  GL, GLExt,
{$ENDIF}
