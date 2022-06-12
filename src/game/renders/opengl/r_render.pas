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
    {$IFDEF ENABLE_MENU}
      r_gui,
    {$ENDIF}
    {$IFDEF ENABLE_SYSTEM}
      g_system,
    {$ENDIF}
    SysUtils, Classes, Math,
    e_log, utils,
    g_game, g_options, g_console, g_player, g_weapons, g_language,
    g_net,
    r_draw, r_textures, r_fonts, r_common, r_console, r_map
  ;

  type
    TBasePoint = (
      BP_LEFTUP,   BP_UP,     BP_RIGHTUP,
      BP_LEFT,     BP_CENTER, BP_RIGHT,
      BP_LEFTDOWN, BP_DOWN,   BP_RIGHTDOWN
    );

  var
    menuBG: TGLTexture;

    hud, hudbg: TGLTexture;
    hudhp: array [Boolean] of TGLTexture;
    hudap: TGLTexture;
    hudwp: array [0..WP_LAST] of TGLTexture;
    hudkey: array [0..2] of TGLTexture;
    hudair: TGLTexture;
    hudjet: TGLTexture;

  procedure r_Render_LoadTextures;
  begin
    r_Map_LoadTextures;
  end;

  procedure r_Render_FreeTextures;
  begin
    r_Map_FreeTextures;
  end;

  procedure r_Render_Load;
    const
      WeapName: array [0..WP_LAST] of AnsiString = ('KASTET', 'SAW', 'PISTOL', 'SHOTGUN1', 'SHOTGUN2', 'MGUN', 'RLAUNCHER', 'PGUN', 'BFG', 'SPULEMET', 'FLAMETHROWER');
    var
      i: Integer;
  begin
    r_Common_Load;
    menuBG := r_Textures_LoadFromFile(GameWAD + ':TEXTURES/TITLE');
    hud :=  r_Textures_LoadFromFile(GameWAD + ':TEXTURES/HUD');
    hudbg :=  r_Textures_LoadFromFile(GameWAD + ':TEXTURES/HUDBG');
    hudhp[false] := r_Textures_LoadFromFile(GameWAD + ':TEXTURES/MED2');
    hudhp[true] := r_Textures_LoadFromFile(GameWAD + ':TEXTURES/BMED');
    hudap := r_Textures_LoadFromFile(GameWAD + ':TEXTURES/ARMORHUD');
    for i := 0 to WP_LAST do
      hudwp[i] := r_Textures_LoadFromFile(GameWAD + ':TEXTURES/' + WeapName[i]);
    hudkey[0] := r_Textures_LoadFromFile(GameWAD + ':TEXTURES/KEYR');
    hudkey[1] := r_Textures_LoadFromFile(GameWAD + ':TEXTURES/KEYG');
    hudkey[2] := r_Textures_LoadFromFile(GameWAD + ':TEXTURES/KEYB');
    hudair := r_Textures_LoadFromFile(GameWAD + ':TEXTURES/AIRBAR');
    hudjet := r_Textures_LoadFromFile(GameWAD + ':TEXTURES/JETBAR');
    r_Console_Load;
    r_Map_Load;
    {$IFDEF ENABLE_MENU}
      r_GUI_Load;
    {$ENDIF}
  end;

  procedure r_Render_Free;
    var i: Integer;
  begin
    {$IFDEF ENABLE_MENU}
      r_GUI_Free;
    {$ENDIF}
    r_Map_Free;
    r_Console_Free;
    hudjet.Free;
    hudair.Free;
    hudkey[0].Free;
    hudkey[1].Free;
    hudkey[2].Free;
    for i := 0 to WP_LAST do
    begin
      if hudwp[i] <> nil then
        hudwp[i].Free;
      hudwp[i] := nil;
    end;
    hudap.Free;
    hudhp[true].Free;
    hudhp[false].Free;
    hudbg.Free;
    hud.Free;
    menuBG.Free;
    r_Common_Free;
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
    r_Console_Initialize;
    r_Map_Initialize;
  end;

  procedure r_Render_Finalize;
  begin
    r_Map_Finalize;
    r_Console_Finalize;
    r_Textures_Finalize;
  end;

  procedure r_Render_Update;
  begin
    r_Console_Update;
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

  procedure r_Render_GetBasePoint (x, y, w, h: Integer; p: TBasePoint; out xx, yy: Integer);
  begin
    case p of
      TBasePoint.BP_LEFTUP,  TBasePoint.BP_LEFT,   TBasePoint.BP_LEFTDOWN:  xx := x;
      TBasePoint.BP_UP,      TBasePoint.BP_CENTER, TBasePoint.BP_DOWN:      xx := x - w div 2;
      TBasePoint.BP_RIGHTUP, TBasePoint.BP_RIGHT,  TBasePoint.BP_RIGHTDOWN: xx := x - w;
    end;
    case p of
      TBasePoint.BP_LEFTUP,   TBasePoint.BP_UP,     TBasePoint.BP_RIGHTUP:   yy := y;
      TBasePoint.BP_LEFT,     TBasePoint.BP_CENTER, TBasePoint.BP_RIGHT:     yy := y - h div 2;
      TBasePoint.BP_LEFTDOWN, TBasePoint.BP_DOWN,   TBasePoint.BP_RIGHTDOWN: yy := y - h;
    end;
  end;

  procedure r_Render_DrawText (const text: AnsiString; x, y: Integer; r, g, b, a: Byte; f: TGLFont; p: TBasePoint);
    var w, h: Integer;
  begin
    if p <> TBasePoint.BP_LEFTUP then
    begin
      r_Draw_GetTextSize(text, f, w, h);
      r_Render_GetBasePoint(x, y, w, h, p, x, y);
    end;
    r_Draw_Text(text, x, y, r, g, b, a, f);
  end;

  procedure r_Render_DrawTexture (img: TGLTexture; x, y, w, h: Integer; p: TBasePoint);
  begin
    r_Render_GetBasePoint(x, y, w, h, p, x, y);
    r_Draw_TextureRepeat(img, x, y, w, h, false, 255, 255, 255, 255, false);
  end;

  procedure r_Render_DrawHUD (x, y: Integer; p: TPlayer);
    var t: TGLTexture; s: AnsiString;
  begin
    ASSERT(p <> nil);

    // hud area is 196 x 240 pixels
    r_Render_DrawTexture(hud, x, y, hud.width, hud.height, TBasePoint.BP_LEFTUP);
    r_Render_DrawText(p.name, x + 98, y + 16, 255, 0, 0, 255, smallfont, TBasePoint.BP_CENTER);

    t := hudhp[R_BERSERK in p.FRulez];
    r_Render_DrawTexture(t, x + 51, y + 61, t.width, t.height, TBasePoint.BP_CENTER);
    r_Render_DrawTexture(hudap, x + 50, y + 85, hudap.width, hudap.height, TBasePoint.BP_CENTER);

    r_Render_DrawText(IntToStr(MAX(0, p.health)), x + 174, y + 56, 255, 0, 0, 255, menufont, TBasePoint.BP_RIGHT);
    r_Render_DrawText(IntToStr(MAX(0, p.armor)), x + 174, y + 84, 255, 0, 0, 255, menufont, TBasePoint.BP_RIGHT);

    case p.CurrWeap of
      WEAPON_KASTET, WEAPON_SAW: s := '--';
      else s := IntToStr(p.GetAmmoByWeapon(p.CurrWeap));
    end;
    r_Render_DrawText(s, x + 174, y + 174, 255, 0, 0, 255, menufont, TBasePoint.BP_RIGHT);

    if p.CurrWeap <= WP_LAST then
    begin
      t := hudwp[p.CurrWeap];
      r_Render_DrawTexture(t, x + 18, y + 160, t.width, t.height, TBasePoint.BP_LEFTUP);
    end;

    if R_KEY_RED in p.FRulez then
      r_Render_DrawTexture(hudkey[0], x + 76, y + 214, 16, 16, TBasePoint.BP_LEFTUP);
    if R_KEY_GREEN in p.FRulez then
      r_Render_DrawTexture(hudkey[1], x + 93, y + 214, 16, 16, TBasePoint.BP_LEFTUP);
    if R_KEY_BLUE in p.FRulez then
      r_Render_DrawTexture(hudkey[2], x + 110, y + 214, 16, 16, TBasePoint.BP_LEFTUP);

    if p.JetFuel > 0 then
    begin
      r_Render_DrawTexture(hudair, x, y + 116, hudair.width, hudair.height, TBasePoint.BP_LEFTUP);
      if p.air > 0 then
        r_Draw_FillRect(x + 14, y + 116 + 4, x + 14 + 168 * p.air div AIR_MAX, y + 116 + 4 + 4, 0, 0, 196, 255);
      r_Render_DrawTexture(hudjet, x, y + 126, hudjet.width, hudjet.height, TBasePoint.BP_LEFTUP);
      r_Draw_FillRect(x + 14, y + 126 + 4, x + 14 + 168 * p.JetFuel div JET_MAX, y + 126 + 4 + 4, 208, 0, 0, 255);
    end
    else
    begin
      r_Render_DrawTexture(hudair, x, y + 124, hudair.width, hudair.height, TBasePoint.BP_LEFTUP);
      if p.air > 0 then
        r_Draw_FillRect(x + 14, y + 124 + 4, x + 14 + 168 * p.air div AIR_MAX, y + 124 + 4 + 4, 0, 0, 196, 255);
    end;
  end;

  procedure r_Render_DrawHUDArea (x, y, w, h: Integer; p: TPlayer);
    var s: AnsiString;
  begin
    r_Render_DrawTexture(hudbg, x, y, w, h, TBasePoint.BP_LEFTUP);

    if p <> nil then
      r_Render_DrawHUD(x + w - 196 + 2, y, p);

    if gShowPing and g_Game_IsClient then
    begin
      s := _lc[I_GAME_PING_HUD] + IntToStr(NetPeer.lastRoundTripTime) + _lc[I_NET_SLIST_PING_MS];
      r_Render_DrawText(s, x + 4, y + 242, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
    end;

    if p.Spectator then
    begin
      r_Render_DrawText(_lc[I_PLAYER_SPECT], x + 4, y + 242, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Render_DrawText(_lc[I_PLAYER_SPECT2], x + 4, y + 258, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Render_DrawText(_lc[I_PLAYER_SPECT1], x + 4, y + 274, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
      if p.NoRespawn then
        r_Render_DrawText(_lc[I_PLAYER_SPECT1S], x + 4, y + 290, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
    end;
  end;

  procedure r_Render_DrawView (x, y, w, h: Integer; p: TPlayer);
  begin
    if p <> nil then
      r_Map_Draw(x, y, w, h, p.obj.x + PLAYER_RECT_CX, p.obj.y + PLAYER_RECT_CY, p)
    else
      r_Map_Draw(x, y, w, h, 0, 0, nil);

    // TODO draw stats

    if p <> nil then
    begin
      if p.Spectator and p.NoRespawn then
        r_Render_DrawText(_lc[I_PLAYER_SPECT4], x div 2 + w div 2, y div 2 + h div 2, 255, 255, 255, 255, stdfont, TBasePoint.BP_CENTER);
    end;
  end;

  procedure r_Render_DrawPlayerView (x, y, w, h: Integer; p: TPlayer);
  begin
    r_Render_DrawView(x, y, w - 196, h, p);
    r_Render_DrawHUDArea(x + w - 196, y, 196, h, p);
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
      r_Render_DrawPlayerView(0, 0, gScreenWidth, gScreenHeight, gPlayer1);

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
        if gGameOn then
          r_Draw_FillRect(0, 0, gScreenWidth - 1, gScreenHeight - 1, 0, 0, 0, 105);
        r_GUI_Draw_Window(g_ActiveWindow);
      end;
    {$ENDIF}

    r_Console_Draw(false);

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
    r_GUI_GetSize(ctrl, w, h);
  end;

  procedure r_Render_GetLogoSize (out w, h: Integer);
  begin
    r_GUI_GetLogoSize(w, h);
  end;

  procedure r_Render_GetMaxFontSize (BigFont: Boolean; out w, h: Integer);
  begin
    r_GUI_GetMaxFontSize(BigFont, w, h);
  end;

  procedure r_Render_GetStringSize (BigFont: Boolean; str: String; out w, h: Integer);
  begin
    r_GUI_GetStringSize(BigFont, str, w, h);
  end;
{$ENDIF}

  procedure r_Render_DrawLoading (force: Boolean);
  begin
  end;

end.
