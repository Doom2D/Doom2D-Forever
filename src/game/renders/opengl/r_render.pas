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
{$INCLUDE ../../../shared/a_modes.inc}
unit r_render;

interface

  uses
    {$IFDEF ENABLE_MENU}
      g_gui,
    {$ENDIF}
    g_base // TRectWH
  ;

  (* render startup *)
  procedure r_Render_Initialize;
  procedure r_Render_Finalize;

  (* load globally used textures *)
  procedure r_Render_Load;
  procedure r_Render_Free;

  (* load map specific textures *)
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

  {$IFDEF ENABLE_MENU}
    procedure r_Render_GetControlSize (ctrl: TGUIControl; out w, h: Integer);
    procedure r_Render_GetLogoSize (out w, h: Integer);
    procedure r_Render_GetMaxFontSize (BigFont: Boolean; out w, h: Integer);
    procedure r_Render_GetStringSize (BigFont: Boolean; str: String; out w, h: Integer);
  {$ENDIF}

  procedure r_Render_DrawLoading (force: Boolean); // !!! remove it

implementation

  uses
    {$IFDEF USE_GLES1}
      GLES11,
    {$ELSE}
      GL, GLEXT,
    {$ENDIF}
    {$IFDEF ENABLE_SYSTEM}
      g_system,
    {$ENDIF}
    SysUtils, Classes, Math,
    e_log, utils,
    g_game, g_options, g_console,
    r_draw, r_textures, r_fonts, r_map
  ;

  var
    menuBG: TGLTexture;
    stdfont: TGLFont;
    smallfont: TGLFont;
    menufont: TGLFont;

  procedure r_Render_LoadTextures;
  begin
    r_Map_LoadTextures;
  end;

  procedure r_Render_FreeTextures;
  begin
    r_Map_FreeTextures;
  end;

  function r_Render_LoadFont (const name: AnsiString): TGLFont;
    var info: TFontInfo; skiphack: Integer;
  begin
    result := nil;
    if name = 'STD' then skiphack := 144 else skiphack := 0;
    if r_Font_LoadInfoFromFile(GameWad + ':FONTS/' + name + 'TXT', info) then
      result := r_Textures_LoadFontFromFile(GameWad + ':FONTS/' + name + 'FONT', info, skiphack, true);
    if result = nil then
      e_logwritefln('failed to load font %s', [name]);
  end;

  procedure r_Render_Load;
  begin
    menuBG := r_Textures_LoadFromFile(GameWAD + ':' + 'TEXTURES/TITLE');
    stdfont := r_Render_LoadFont('STD');
    smallfont := r_Render_LoadFont('SMALL');
    menufont := r_Render_LoadFont('MENU');
    r_Map_Load;
  end;

  procedure r_Render_Free;
  begin
    r_Map_Free;
    menufont.Free;
    smallfont.Free;
    stdfont.Free;
    menuBG.Free;
  end;

{$IFDEF ENABLE_SYSTEM}
  function GetInfo (): TGLDisplayInfo;
    var info: TGLDisplayInfo;
  begin
    info := Default(TGLDisplayInfo);
    info.w := Max(1, gRC_Width);
    info.h := Max(1, gRC_Height);
    info.bpp := Max(1, gBPP);
    info.fullscreen := gRC_FullScreen;
    info.maximized := gRC_Maximized;
    info.major := 1;
    info.minor := 1;
    info.profile := TGLProfile.Compat;
    result := info;
  end;
{$ENDIF}

  procedure r_Render_Initialize;
  begin
    {$IFDEF ENABLE_SYSTEM}
      if sys_SetDisplayModeGL(GetInfo()) = False then
        raise Exception.Create('Failed to set videomode on startup.');
      sys_EnableVSync(gVSync);
    {$ENDIF}
    r_Textures_Initialize;
    r_Map_Initialize;
  end;

  procedure r_Render_Finalize;
  begin
    r_Map_Finalize;
    r_Textures_Finalize;
  end;

  procedure r_Render_Update;
  begin
    r_Map_Update;
  end;

  procedure SetupMatrix;
  begin
    glViewport(0, 0, gScreenWidth, gScreenHeight);
    glScissor(0, 0, gScreenWidth, gScreenHeight);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glOrtho(0, gScreenWidth, gScreenHeight, 0, 0, 1);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
  end;

  procedure r_Render_Draw;
  begin
    if gExit = EXIT_QUIT then
      exit;

    SetupMatrix;

    glClearColor(0.0, 0.0, 0.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    glColor4ub(255, 255, 255, 255);

    //e_LogWritefln('r_render_draw: %sx%s', [gScreenWidth, gScreenHeight]);

    if gGameOn or (gState = STATE_FOLD) then
    begin
      // TODO setup player view
      // TODO setup sectator mode
      // TODO setup player hear point
      // TODO setup player view siz

      // TODO draw player view + setup screen coords
      r_Map_Draw(0, 0, gScreenWidth, gScreenHeight, gPlayer1.obj.x + 32, gPlayer1.obj.y + 32, gPlayer1); // !!! remove unnamed consts

      // TODO draw holmes inspector

      // TODO draw messages
      // TODO draw stats (?)
      // TODO draw spectator hud
    end;

    if gPauseMain and gGameOn {$IFDEF ENABLE_MENU}and (g_ActiveWindow = nil){$ENDIF} then
    begin
      // TODO draw pause screen
    end;

    if not gGameOn then
    begin
      case gState of
        STATE_MENU: ; // TODO draw menu bg
        STATE_FOLD: ;
        STATE_INTERCUSTOM: ;
        STATE_INTERSINGLE: ;
        STATE_ENDPIC: ;
        STATE_SLIST: ;
      end;
    end;

    {$IFDEF ENABLE_MENU}
      if g_ActiveWindow <> nil then
      begin
        // TODO draw menu widgets
      end;
    {$ENDIF}

    // TODO draw console

    // TODO draw holmes interface

    glFinish();
    glFlush();
    sys_Repaint;
  end;

  procedure r_Render_Resize (w, h: Integer);
  begin
    gWinSizeX := w;
    gWinSizeY := h;
    gRC_Width := w;
    gRC_Height := h;
    gScreenWidth := w;
    gScreenHeight := h;
  end;

  procedure r_Render_Apply;
  begin
    {$IFDEF ENABLE_SYSTEM}
      if sys_SetDisplayModeGL(GetInfo()) then
        e_LogWriteln('resolution changed')
      else
        e_LogWriteln('resolution not changed');
      sys_EnableVSync(gVSync)
    {$ENDIF}
  end;

  function r_Render_WriteScreenShot (filename: String): Boolean;
  begin
    Result := False;
  end;

{$IFDEF ENABLE_GIBS}
  function r_Render_GetGibRect (m, id: Integer): TRectWH;
  begin
    result := r_Map_GetGibSize(m, id);
  end;
{$ENDIF}

{$IFDEF ENABLE_GFX}
  procedure r_Render_QueueEffect (AnimType, X, Y: Integer);
  begin
    r_Map_NewGFX(AnimType, X, Y);
  end;
{$ENDIF}

{$IFDEF ENABLE_TOUCH}
  procedure r_Render_GetKeyRect (key: Integer; out x, y, w, h: Integer; out founded: Boolean);
  begin
    founded := False;
  end;
{$ENDIF}

{$IFDEF ENABLE_MENU}
  procedure r_Render_GetControlSize (ctrl: TGUIControl; out w, h: Integer);
  begin
    w := 0; h := 0;
  end;

  procedure r_Render_GetLogoSize (out w, h: Integer);
  begin
    w := 0; h := 0;
  end;

  procedure r_Render_GetMaxFontSize (BigFont: Boolean; out w, h: Integer);
  begin
    w := 0; h := 0;
  end;

  procedure r_Render_GetStringSize (BigFont: Boolean; str: String; out w, h: Integer);
  begin
    w := 0; h := 0;
  end;
{$ENDIF}

  procedure r_Render_DrawLoading (force: Boolean);
  begin
  end;

end.
