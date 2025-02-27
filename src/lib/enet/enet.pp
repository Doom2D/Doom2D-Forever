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

{$MACRO ON}

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$IFDEF ENET_WINDOWS_STATIC}
    {$LINKLIB libenet.a}
    {$LINKLIB libwinmm.a}
    {$LINKLIB libws2_32.a}
    {$LINKLIB libkernel32.a}
    {$LINKLIB libm.a}
    {$LINKLIB libmingwex.a}
    {$LINKLIB libmingw32.a}
    {$LINKLIB libmsvcrt.a}
    {$LINKLIB libgcc.a}
    {$DEFINE libraryENet := cdecl; external}
  {$ENDIF}
{$ELSE}
  {$IFDEF DARWIN}
    {$LINKLIB libenet}
    {$DEFINE libraryENet := cdecl; external}
  {$ENDIF}
{$ENDIF}


{$INCLUDE 'ENet-Pascal/enet.pp'}
