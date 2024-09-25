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
unit g_touch;

interface

  var
    g_touch_enabled: Boolean = False;
    g_touch_size: Single = 1.0;
    g_touch_offset: Single = 50.0;
    g_touch_fire: Boolean = True;
    g_touch_alt: Boolean = False;

  procedure g_Touch_Init;
  procedure g_Touch_ShowKeyboard(yes: Boolean);
  procedure g_Touch_Draw;

implementation

  uses g_console;

  procedure g_Touch_Init;
  begin
  end;

  procedure g_Touch_ShowKeyboard(yes: Boolean);
  begin
  end;

  procedure g_Touch_Draw;
  begin
  end;

initialization
  conRegVar('touch_enable', @g_touch_enabled, 'enable/disable virtual buttons', 'draw buttons');
  conRegVar('touch_fire', @g_touch_fire, 'enable/disable fire when press virtual up/down', 'fire when press up/down');
  conRegVar('touch_size', @g_touch_size, 0.1, 10, 'size of virtual buttons', 'button size');
  conRegVar('touch_offset', @g_touch_offset, 0, 100, '', '');
  conRegVar('touch_alt', @g_touch_alt, 'althernative virtual buttons layout', 'althernative layout');
end.
