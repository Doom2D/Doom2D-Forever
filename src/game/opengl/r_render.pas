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

  uses g_base; // TRectWH

  procedure r_Render_Initialize;
  procedure r_Render_Finalize;

  procedure r_Render_Load;
  procedure r_Render_Free;

  procedure r_Render_LoadTextures;
  procedure r_Render_FreeTextures;

  procedure r_Render_Update;
  procedure r_Render_Draw;

  procedure r_Render_Resize (w, h: Integer);
  procedure r_Render_Apply;

  function r_Render_WriteScreenShot (filename: String): Boolean;

  {$IFDEF ENABLE_GIBS}
    function r_Render_GetGibRect (m, id: Integer): TRectWH;
  {$ENDIF}

  {$IFDEF ENABLE_GFX}
    procedure r_Render_QueueEffect (AnimType, X, Y: Integer);
  {$ENDIF}

  {$IFDEF ENABLE_TOUCH}
    // touch screen button location and size
    procedure r_Render_GetKeyRect (key: Integer; out x, y, w, h: Integer; out founded: Boolean);
  {$ENDIF}

  procedure r_Render_DrawLoading (force: Boolean); // !!! remove it

implementation

  uses
    {$INCLUDE ../../nogl/noGLuses.inc}
    {$IFDEF ENABLE_TOUCH}
      r_touch,
    {$ENDIF}
    {$IFDEF ENABLE_GFX}
      r_gfx,
    {$ENDIF}
    {$IFDEF ENABLE_SYSTEM}
      g_system,
    {$ENDIF}
    SysUtils, Classes, Math,
    e_log, utils,
    g_game, g_options, g_console,
    r_window, r_graphics, r_console, r_playermodel, r_textures, r_animations,
    r_weapons, r_items, r_monsters, r_map, r_player, r_game
  ;

  var
    LoadedGL: Boolean = false;

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
    {$IFDEF NOGL_INIT}
      Result := nogl_ExtensionSupported(ext);
    {$ELSE}
      result := false;
      for e in GLExtensionList() do
      begin
        if strEquCI1251(e, ext) then
        begin
          result := true;
          exit
        end
      end
    {$ENDIF}
  end;

  function HaveNPOTSupport (): Boolean;
  begin
    Result := GLExtensionSupported('GL_ARB_texture_non_power_of_two') or
              GLExtensionSupported('GL_OES_texture_npot')
  end;

  function HaveFBOSupport (): Boolean;
  begin
    Result := GLExtensionSupported('GL_ARB_framebuffer_object') or
              GLExtensionSupported('GL_OES_framebuffer_object')
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
    glLegacyNPOT := not HaveNPOTSupport();
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

  procedure LoadGL;
    var fboload: Boolean;
  begin
    if LoadedGL = false then
    begin
      {$IFDEF NOGL_INIT}
        nogl_Init;
      {$ENDIF}
      if glRenderToFBO then
      begin
        fboload := True;
        {$IFDEF NOGL_INIT}
          fboload := True; // !!! but if not?
        {$ELSE}
          fboload := Load_GL_ARB_framebuffer_object();
        {$ENDIF}
        if (fboload = False) or (HaveFBOSupport() = False) or (HaveNPOTSupport() = False) then
        begin
          e_LogWriteln('GL: framebuffer objects not supported; disabling FBO rendering');
          glRenderToFBO := false
        end;
      end;
      LoadedGL := true
    end
  end;

  procedure FreeGL;
  begin
    if LoadedGL = true then
    begin
      {$IFDEF NOGL_INIT}
        nogl_Quit;
      {$ENDIF}
      LoadedGL := false
    end
  end;

  procedure r_Render_LoadTextures;
  begin
    r_Game_LoadTextures;
    r_Map_LoadTextures;
  end;

  procedure r_Render_FreeTextures;
  begin
    r_Map_FreeTextures;
    r_Game_FreeTextures;
  end;

  procedure r_Render_Load;
  begin
    r_Game_Load; // load first!
    r_Player_Load;
    r_Map_Load;
    r_PlayerModel_Load;
    r_Monsters_Load;
    r_Weapon_Load;
    r_Items_Load;
    {$IFDEF ENABLE_GFX}
      r_GFX_Load;
    {$ENDIF}
  end;

  procedure r_Render_Free;
  begin
    {$IFDEF ENABLE_GFX}
      r_GFX_Free;
    {$ENDIF}
    r_Items_Free;
    r_Weapon_Free;
    r_Monsters_Free;
    r_PlayerModel_Free;
    r_Map_Free;
    r_Player_Free;
    r_Game_Free;
    g_Texture_DeleteAll;
    g_Frames_DeleteAll;
  end;

  procedure r_Render_Initialize;
  begin
    {$IFDEF ENABLE_SYSTEM}
      if sys_SetDisplayMode(gRC_Width, gRC_Height, gBPP, gRC_FullScreen, gRC_Maximized) = False then
        raise Exception.Create('Failed to set videomode on startup.');
    {$ENDIF}
    LoadGL;
    r_Window_Initialize;
    r_Console_Init;
    r_PlayerModel_Initialize;
    r_Map_Initialize;
  end;

  procedure r_Render_Finalize;
  begin
    r_Map_Finalize;
    r_PlayerModel_Finalize;
    FreeGL;
    e_ReleaseEngine
  end;

  procedure r_Render_Update;
  begin
    {$IFDEF ENABLE_GFX}
      r_GFX_Update;
    {$ENDIF}
    r_Map_Update;
    r_PlayerModel_Update;
    r_Console_Update;
  end;

  procedure r_Render_Draw;
  begin
    r_Game_Draw;
    {$IFDEF ENABLE_TOUCH}
      r_Touch_Draw;
    {$ENDIF}
  end;

  procedure r_Render_Resize (w, h: Integer);
  begin
    LoadGL;
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
    e_InitGL;
    r_Game_SetupScreenSize;
  end;

  procedure r_Render_Apply;
  begin
    {$IFDEF ENABLE_SYSTEM}
      if sys_SetDisplayMode(Max(1, gRC_Width), Max(1, gRC_Height), Max(1, gBPP), gRC_FullScreen, gRC_Maximized) then
        e_LogWriteln('resolution changed')
      else
        e_LogWriteln('resolution not changed');
      sys_EnableVSync(gVSync)
    {$ENDIF}
  end;

  function r_Render_WriteScreenShot (filename: String): Boolean;
    var s: TStream;
  begin
    Result := False;
    try
      s := CreateDiskFile(filename);
      try
        e_MakeScreenshot(s, gWinSizeX, gWinSizeX);
        Result := True;
      except
        DeleteFile(filename)
      end;
      s.Free;
    finally
    end
  end;

{$IFDEF ENABLE_GIBS}
  function r_Render_GetGibRect (m, id: Integer): TRectWH;
  begin
    Result := r_PlayerModel_GetGibRect(m, id)
  end;
{$ENDIF}

{$IFDEF ENABLE_GFX}
  procedure r_Render_QueueEffect (AnimType, X, Y: Integer);
  begin
    r_GFX_OnceAnim(AnimType, X, Y)
  end;
{$ENDIF}

{$IFDEF ENABLE_TOUCH}
  procedure r_Render_GetKeyRect (key: Integer; out x, y, w, h: Integer; out founded: Boolean);
  begin
    r_Touch_GetKeyRect (key, x, y, w, h, founded)
  end;
{$ENDIF}

  procedure r_Render_DrawLoading (force: Boolean);
  begin
    r_Window_DrawLoading(force)
  end;

end.
