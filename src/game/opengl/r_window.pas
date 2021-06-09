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
  procedure r_Window_Initialize;

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

  function GLExtensionList (): SSArray;
    var s: PChar; i, j, num: GLint;
  begin
    result := nil;
    s := glGetString(GL_EXTENSIONS);
    if s <> nil then
    begin
      num := 0;
      i := 0;
      j := 0;
      while (s[i] <> #0) and (s[i] = ' ') do Inc(i);
      while (s[i] <> #0) do
      begin
        while (s[i] <> #0) and (s[i] <> ' ') do Inc(i);
        SetLength(result, num+1);
        result[num] := Copy(s, j+1, i-j);
        while (s[i] <> #0) and (s[i] = ' ') do Inc(i);
        j := i;
        Inc(num)
      end
    end
  end;

  function GLExtensionSupported (ext: AnsiString): Boolean;
    var e: AnsiString;
  begin
    result := false;
    for e in GLExtensionList() do
    begin
      if strEquCI1251(e, ext) then
      begin
        result := true;
        exit
      end
    end
  end;

  procedure PrintGLSupportedExtensions;
  begin
    e_LogWritefln('GL Vendor: %s', [glGetString(GL_VENDOR)]);
    e_LogWritefln('GL Renderer: %s', [glGetString(GL_RENDERER)]);
    e_LogWritefln('GL Version: %s', [glGetString(GL_VERSION)]);
    e_LogWritefln('GL Shaders: %s', [glGetString(GL_SHADING_LANGUAGE_VERSION)]);
    e_LogWritefln('GL Extensions: %s', [glGetString(GL_EXTENSIONS)]);
  end;

  procedure r_Window_Initialize;
  begin
{$IFNDEF USE_SYSSTUB}
    PrintGLSupportedExtensions;
    glLegacyNPOT := not (GLExtensionSupported('GL_ARB_texture_non_power_of_two') or GLExtensionSupported('GL_OES_texture_npot'));
{$ELSE}
    glLegacyNPOT := False;
    glRenderToFBO := False;
{$ENDIF}
    if glNPOTOverride and glLegacyNPOT then
    begin
      glLegacyNPOT := true;
      e_logWriteln('NPOT texture emulation: FORCED')
    end
    else
    begin
      if glLegacyNPOT then
        e_logWriteln('NPOT texture emulation: enabled')
      else
        e_logWriteln('NPOT texture emulation: disabled')
    end
  end;

end.
