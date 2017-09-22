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
{$INCLUDE ../shared/a_modes.inc}
unit sdlcarcass;

interface

uses
  SDL2;


// ////////////////////////////////////////////////////////////////////////// //
// event handlers
var
  evSDLCB: function (var ev: TSDL_Event): Boolean = nil;
  winFocusCB: procedure () = nil;
  winBlurCB: procedure () = nil;
  //buildFrameCB: procedure () = nil;
  //renderFrameCB: procedure () = nil; // no need to call `glSwap()` here
  prerenderFrameCB: procedure () = nil;
  postrenderFrameCB: procedure () = nil;
  oglInitCB: procedure () = nil;
  oglDeinitCB: procedure () = nil;


function getScrWdt (): Integer; inline;
function getScrHgt (): Integer; inline;

property
  gScrWidth: Integer read getScrWdt;
  gScrHeight: Integer read getScrHgt;


implementation

uses
  g_options;


function getScrWdt (): Integer; inline; begin result := gScreenWidth; end;
function getScrHgt (): Integer; inline; begin result := gScreenHeight; end;


end.
