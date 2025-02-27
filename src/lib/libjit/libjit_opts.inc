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

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{.$MODE OBJFPC}
{$PACKRECORDS C}
{$MACRO ON}

{$Z4} // Force four-byte enums


{$IFDEF MSWINDOWS}
  {$IFDEF LIBJIT_WINDOWS_STATIC}
    {$LINKLIB libjit.a}
    {$LINKLIB libkernel32.a}
    {$LINKLIB libm.a}
    {$LINKLIB libmingwex.a}
    {$LINKLIB libmingw32.a}
    {$LINKLIB libmsvcrt.a}
    {$LINKLIB libgcc.a}
    {$DEFINE libraryLibJITDecl := cdecl}
    {$DEFINE libraryLibJITImp := cdecl; external}
    {$DEFINE libraryLibJITVar := cvar; external}
  {$ELSE}
    {$DEFINE libraryLibJITDecl := cdecl}
    {$DEFINE libraryLibJITImp := cdecl; external 'libjit-0.dll'}
    {.$DEFINE libraryLibJITVar := cvar; external}
    {$DEFINE libraryLibJITVar := external 'libjit-0.dll'}
    // external LIBNAME name 'var_name' would've been more correct here
    // because just external is case insensitive, but fuck it
  {$ENDIF}
{$ELSE}
  {$DEFINE libraryLibJITDecl := cdecl}
  {$DEFINE libraryLibJITImp := cdecl; external 'jit'}
  {$DEFINE libraryLibJITVar := cvar; external 'jit'}
{$ENDIF}
