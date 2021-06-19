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
{$INCLUDE ../../shared/a_modes.inc}
unit r_render;

interface

  procedure r_Render_Initialize;
  procedure r_Render_Finalize;
  procedure r_Render_Resize (w, h: Integer);

implementation

  uses SysUtils, Classes, e_log, g_system, g_game, g_options, r_window, r_graphics, r_console, r_playermodel;

  procedure r_Render_Initialize;
  begin
    if sys_SetDisplayMode(gRC_Width, gRC_Height, gBPP, gRC_FullScreen, gRC_Maximized) = False then
      raise Exception.Create('Failed to set videomode on startup.');
    
    r_Window_Initialize;
    r_Console_Init;
    r_PlayerModel_Initialize;
  end;

  procedure r_Render_Finalize;
  begin
    r_PlayerModel_Finalize;
    e_ReleaseEngine
  end;

  procedure r_Render_Resize (w, h: Integer);
  begin
    gWinSizeX := w;
    gWinSizeY := h;
    gRC_Width := w;
    gRC_Height := h;
    if glRenderToFBO then
    begin
      // store real window size in gWinSize, downscale resolution now
      w := round(w / r_pixel_scale);
      h := round(h / r_pixel_scale);
      if not e_ResizeFramebuffer(w, h) then
      begin
        e_LogWriteln('GL: could not create framebuffer, falling back to --no-fbo');
        glRenderToFBO := False;
        w := gWinSizeX;
        h := gWinSizeY;
      end;
    end;
    gScreenWidth := w;
    gScreenHeight := h;
    e_ResizeWindow(w, h);
    e_InitGL
  end;

end.
