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
unit g_system;

interface

  uses Utils;

  (* --- Utils --- *)
  procedure sys_Delay (ms: Integer);

  (* --- Graphics --- *)
  function sys_GetDisplayModes (bpp: Integer): SSArray;
  function sys_SetDisplayMode (w, h, bpp: Integer; fullscreen, maximized: Boolean): Boolean;
  procedure sys_EnableVSync (yes: Boolean);
  procedure sys_Repaint;

  (* --- Input --- *)
  function sys_HandleInput (): Boolean;
  procedure sys_RequestQuit;

  (* --- Init --- *)
  procedure sys_Init;
  procedure sys_Final;

  var (* hooks *)
    sys_CharPress: procedure (ch: AnsiChar) = nil;
    sys_ScreenResize: procedure (w, h: Integer) = nil;

implementation

  uses SysUtils;

  (* --------- Utils --------- *)

  procedure sys_Delay (ms: Integer);
  begin
    Sleep(ms)
  end;

  (* --------- Graphics --------- *)

  procedure sys_Repaint;
  begin
  end;

  procedure sys_EnableVSync (yes: Boolean);
  begin
  end;

  function sys_GetDisplayModes (bpp: Integer): SSArray;
  begin
    result := nil
  end;

  function sys_SetDisplayMode (w, h, bpp: Integer; fullscreen, maximized: Boolean): Boolean;
  begin
    result := true
  end;

  (* --------- Input --------- *)

  function sys_HandleInput (): Boolean;
  begin
    result := false
  end;

  procedure sys_RequestQuit;
  begin
  end;

  (* --------- Init --------- *)

  procedure sys_Init;
  begin
  end;

  procedure sys_Final;
  begin
  end;

end.
