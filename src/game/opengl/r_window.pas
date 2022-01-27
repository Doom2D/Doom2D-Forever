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
unit r_window;

interface

  procedure r_Window_DrawLoading (forceUpdate: Boolean);

implementation

  uses
    {$INCLUDE ../nogl/noGLuses.inc}
    SysUtils, Classes,
    e_log, utils,
    r_graphics, r_game, r_console, g_system,
    g_options, g_game, g_console,
    xprofiler
  ;

  const
    ProgressUpdateMSecs = 35; //1;//100;

  var
    prevLoadingUpdateTime: UInt64;

  procedure r_Window_DrawLoading (forceUpdate: Boolean);
    var stt: UInt64;
  begin
    if e_NoGraphics = False then
    begin
      if not forceUpdate then
      begin
        stt := getTimeMilli();
        forceUpdate := (stt < prevLoadingUpdateTime) or (stt-prevLoadingUpdateTime >= ProgressUpdateMSecs);
      end;

      if forceUpdate then
      begin
        e_SetRendertarget(True);
        e_SetViewPort(0, 0, gScreenWidth, gScreenHeight);

        r_Game_DrawMenuBackground('INTER');
        e_DarkenQuadWH(0, 0, gScreenWidth, gScreenHeight, 150);
        r_Game_DrawLoadingStat();
        r_Console_Draw(True);

        e_SetRendertarget(False);
        e_SetViewPort(0, 0, gWinSizeX, gWinSizeY);
        e_BlitFramebuffer(gWinSizeX, gWinSizeY);

        sys_Repaint;
        prevLoadingUpdateTime := getTimeMilli();
      end;
    end;
  end;

end.
