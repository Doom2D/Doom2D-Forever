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

  procedure r_Render_Load;
  procedure r_Render_Free;

  procedure r_Render_LoadTextures;
  procedure r_Render_FreeTextures;

  procedure r_Render_Resize (w, h: Integer);

  procedure r_Render_Update;

  procedure r_Render_Apply;

implementation

  uses
    {$INCLUDE ../../nogl/noGLuses.inc}
    SysUtils, Classes, Math,
    e_log, g_system,
    g_game, g_options, g_console,
    r_window, r_graphics, r_console, r_playermodel, r_textures, r_animations,
    r_weapons, r_items, r_gfx, r_monsters, r_map, r_player, r_game
  ;

  var
    LoadedGL: Boolean = false;

  procedure LoadGL;
  begin
    if LoadedGL = false then
    begin
      {$IFDEF NOGL_INIT}
        nogl_Init;
        if glRenderToFBO and (not nogl_ExtensionSupported('GL_OES_framebuffer_object')) then
        begin
          e_LogWriteln('GL: framebuffer objects not supported; disabling FBO rendering');
          glRenderToFBO := false
        end;
      {$ELSE}
        if glRenderToFBO and (not Load_GL_ARB_framebuffer_object) then
        begin
          e_LogWriteln('GL: framebuffer objects not supported; disabling FBO rendering');
          glRenderToFBO := false
        end;
      {$ENDIF}
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
    r_GFX_Load;
  end;

  procedure r_Render_Free;
  begin
    r_GFX_Free;
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
    if sys_SetDisplayMode(gRC_Width, gRC_Height, gBPP, gRC_FullScreen, gRC_Maximized) = False then
      raise Exception.Create('Failed to set videomode on startup.');
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
    r_Map_Update;
    r_PlayerModel_Update;
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
    e_InitGL
  end;

  procedure r_Render_Apply;
  begin
    if sys_SetDisplayMode(Max(1, gRC_Width), Max(1, gRC_Height), Max(1, gBPP), gRC_FullScreen, gRC_Maximized) then
      e_LogWriteln('resolution changed')
    else
      e_LogWriteln('resolution not changed');
    sys_EnableVSync(gVSync)
  end;

end.
