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

  type
    TProcedure = procedure;

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

  procedure r_Render_SetProcessLoadingCallback (p: TProcedure);
  procedure r_Render_ClearLoading;
  procedure r_Render_SetLoading (const text: String; maxval: Integer);
  procedure r_Render_StepLoading (incval: Integer);
  procedure r_Render_DrawLoading (force: Boolean);

  {$IFDEF ENABLE_HOLMES}
    function pmsCurMapX (): Integer;
    function pmsCurMapY (): Integer;
    function r_Render_HolmesViewIsSet (): Boolean;
  {$ENDIF}

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
    {$IFDEF ENABLE_HOLMES}
      r_holmes,
    {$ENDIF}
    SysUtils, Classes, Math,
    g_basic,
    e_log, utils, wadreader, mapdef,
    g_game, g_map, g_panel, g_options, g_console, g_player, g_weapons, g_language, g_triggers, g_monsters,
    g_net, g_netmaster,
    r_draw, r_textures, r_fonts, r_common, r_console, r_map, r_loadscreen
  ;

  var
    hud, hudbg: TGLTexture;
    hudhp: array [Boolean] of TGLTexture;
    hudap: TGLTexture;
    hudwp: array [0..WP_LAST] of TGLTexture;
    hudkey: array [0..2] of TGLTexture;
    hudair: TGLTexture;
    hudjet: TGLTexture;
    hudrflag, hudrflags, hudrflagd: TGLTexture;
    hudbflag, hudbflags, hudbflagd: TGLTexture;

    FPS, FPSCounter, FPSTime: LongWord;

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
    r_LoadScreen_Load;
    r_Common_Load;
    r_Common_SetLoading('HUD Textures', 5 + (WP_LAST + 1) + 11);
    hud :=  r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/HUD', [TGLHints.txNoRepeat]);
    hudbg :=  r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/HUDBG', []);
    hudhp[false] := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/MED2', [TGLHints.txNoRepeat]);
    hudhp[true] := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/BMED', [TGLHints.txNoRepeat]);
    hudap := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/ARMORHUD', [TGLHints.txNoRepeat]);
    for i := 0 to WP_LAST do
      hudwp[i] := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/' + WeapName[i], [TGLHints.txNoRepeat]);
    hudkey[0] := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/KEYR', [TGLHints.txNoRepeat]);
    hudkey[1] := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/KEYG', [TGLHints.txNoRepeat]);
    hudkey[2] := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/KEYB', [TGLHints.txNoRepeat]);
    hudair := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/AIRBAR', [TGLHints.txNoRepeat]);
    hudjet := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/JETBAR', [TGLHints.txNoRepeat]);
    hudrflag := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/FLAGHUD_R_BASE', [TGLHints.txNoRepeat]);
    hudrflags := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/FLAGHUD_R_STOLEN', [TGLHints.txNoRepeat]);
    hudrflagd := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/FLAGHUD_R_DROP', [TGLHints.txNoRepeat]);
    hudbflag := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/FLAGHUD_B_BASE', [TGLHints.txNoRepeat]);
    hudbflags := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/FLAGHUD_B_STOLEN', [TGLHints.txNoRepeat]);
    hudbflagd := r_Common_LoadTextureFromFile(GameWAD + ':TEXTURES/FLAGHUD_B_DROP', [TGLHints.txNoRepeat]);
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
    r_Common_FreeAndNil(hudbflagd);
    r_Common_FreeAndNil(hudbflags);
    r_Common_FreeAndNil(hudbflag);
    r_Common_FreeAndNil(hudrflagd);
    r_Common_FreeAndNil(hudrflags);
    r_Common_FreeAndNil(hudrflag);
    r_Common_FreeAndNil(hudjet);
    r_Common_FreeAndNil(hudair);
    r_Common_FreeAndNil(hudkey[0]);
    r_Common_FreeAndNil(hudkey[1]);
    r_Common_FreeAndNil(hudkey[2]);
    for i := 0 to WP_LAST do
      r_Common_FreeAndNil(hudwp[i]);
    r_Common_FreeAndNil(hudap);
    r_Common_FreeAndNil(hudhp[true]);
    r_Common_FreeAndNil(hudhp[false]);
    r_Common_FreeAndNil(hudbg);
    r_Common_FreeAndNil(hud);
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
    r_LoadScreen_Initialize;
    r_Textures_Initialize;
    r_Console_Initialize;
    r_Map_Initialize;
  end;

  procedure r_Render_Finalize;
  begin
    r_Map_Finalize;
    r_Console_Finalize;
    r_Textures_Finalize;
    r_LoadScreen_Finalize;
  end;

  procedure r_Render_Update;
  begin
    r_Console_Update;
    r_Map_Update;
  end;

  procedure r_Render_DrawHUD (x, y: Integer; p: TPlayer);
    var t: TGLTexture; s: AnsiString;
  begin
    ASSERT(p <> nil);

    // hud area is 196 x 240 pixels
    r_Common_DrawTexture(hud, x, y, hud.width, hud.height, TBasePoint.BP_LEFTUP);
    r_Common_DrawText(p.name, x + 98, y + 16, 255, 0, 0, 255, smallfont, TBasePoint.BP_CENTER);

    t := hudhp[R_BERSERK in p.FRulez];
    r_Common_DrawTexture(t, x + 51, y + 61, t.width, t.height, TBasePoint.BP_CENTER);
    r_Common_DrawTexture(hudap, x + 50, y + 85, hudap.width, hudap.height, TBasePoint.BP_CENTER);

    r_Common_DrawText(IntToStr(MAX(0, p.health)), x + 174, y + 56, 255, 0, 0, 255, menufont, TBasePoint.BP_RIGHT);
    r_Common_DrawText(IntToStr(MAX(0, p.armor)), x + 174, y + 84, 255, 0, 0, 255, menufont, TBasePoint.BP_RIGHT);

    case p.CurrWeap of
      WEAPON_KASTET, WEAPON_SAW: s := '--';
      else s := IntToStr(p.GetAmmoByWeapon(p.CurrWeap));
    end;
    r_Common_DrawText(s, x + 174, y + 174, 255, 0, 0, 255, menufont, TBasePoint.BP_RIGHT);

    if p.CurrWeap <= WP_LAST then
    begin
      t := hudwp[p.CurrWeap];
      r_Common_DrawTexture(t, x + 18, y + 160, t.width, t.height, TBasePoint.BP_LEFTUP);
    end;

    if R_KEY_RED in p.FRulez then
      r_Common_DrawTexture(hudkey[0], x + 76, y + 214, 16, 16, TBasePoint.BP_LEFTUP);
    if R_KEY_GREEN in p.FRulez then
      r_Common_DrawTexture(hudkey[1], x + 93, y + 214, 16, 16, TBasePoint.BP_LEFTUP);
    if R_KEY_BLUE in p.FRulez then
      r_Common_DrawTexture(hudkey[2], x + 110, y + 214, 16, 16, TBasePoint.BP_LEFTUP);

    if p.JetFuel > 0 then
    begin
      r_Common_DrawTexture(hudair, x, y + 116, hudair.width, hudair.height, TBasePoint.BP_LEFTUP);
      if p.air > 0 then
        r_Draw_FillRect(x + 14, y + 116 + 4, x + 14 + 168 * p.air div AIR_MAX, y + 116 + 4 + 4, 0, 0, 196, 255);
      r_Common_DrawTexture(hudjet, x, y + 126, hudjet.width, hudjet.height, TBasePoint.BP_LEFTUP);
      r_Draw_FillRect(x + 14, y + 126 + 4, x + 14 + 168 * p.JetFuel div JET_MAX, y + 126 + 4 + 4, 208, 0, 0, 255);
    end
    else
    begin
      r_Common_DrawTexture(hudair, x, y + 124, hudair.width, hudair.height, TBasePoint.BP_LEFTUP);
      if p.air > 0 then
        r_Draw_FillRect(x + 14, y + 124 + 4, x + 14 + 168 * p.air div AIR_MAX, y + 124 + 4 + 4, 0, 0, 196, 255);
    end;
  end;

  procedure r_Render_DrawHUDArea (x, y, w, h: Integer; p: TPlayer);
    var s: AnsiString;
  begin
    r_Common_DrawTexture(hudbg, x, y, w, h, TBasePoint.BP_LEFTUP);

    if p <> nil then
    begin
      r_Render_DrawHUD(x + w - 196 + 2, y, p);
      if p.Spectator then
      begin
        r_Common_DrawText(_lc[I_PLAYER_SPECT], x + 4, y + 242, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
        r_Common_DrawText(_lc[I_PLAYER_SPECT2], x + 4, y + 258, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
        r_Common_DrawText(_lc[I_PLAYER_SPECT1], x + 4, y + 274, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
        if p.NoRespawn then
          r_Common_DrawText(_lc[I_PLAYER_SPECT1S], x + 4, y + 290, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
      end;
    end;

    if gShowPing and g_Game_IsClient then
    begin
      s := _lc[I_GAME_PING_HUD] + IntToStr(NetPeer.lastRoundTripTime) + _lc[I_NET_SLIST_PING_MS];
      r_Common_DrawText(s, x + 4, y + 242, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
    end;
  end;

  procedure r_Render_DrawStatsView (x, y, w, h: Integer; p: TPlayer);
    var fw, i, maxFrags, top, totalPlayers: Integer; sign: Char; stat: TPlayerStatArray; f: TGLTexture;
  begin
    ASSERT(p <> nil);

    if gShowScore and (gGameSettings.GameMode in [GM_TDM, GM_CTF]) then
    begin
      (* RED TEAM GOALS *)
      fw := 0;
      if gGameSettings.GameMode = GM_CTF then
      begin
        case gFlags[FLAG_RED].State of
          FLAG_STATE_CAPTURED: f := hudrflags;
          FLAG_STATE_DROPPED:  f := hudrflagd;
          otherwise            f := hudrflag;
        end;
        if f <> nil then
        begin
          fw := f.width + 8; (* + space *)
          r_Common_DrawTexture(f, x + w - 16, y + 240 - 72 - 4, f.width, f.height, TBasePoint.BP_RIGHTUP);
        end;
      end;
      r_Common_DrawText(IntToStr(gTeamStat[TEAM_RED].Score), x + w - 16 - fw, y + 240 - 72 - 4, TEAMCOLOR[TEAM_RED].R, TEAMCOLOR[TEAM_RED].G, TEAMCOLOR[TEAM_RED].B, 255, menufont, TBasePoint.BP_RIGHTUP);

      (* BLUE TEAM GOALS *)
      fw := 0;
      if gGameSettings.GameMode = GM_CTF then
      begin
        case gFlags[FLAG_BLUE].State of
          FLAG_STATE_CAPTURED: f := hudbflags;
          FLAG_STATE_DROPPED:  f := hudbflagd;
          otherwise            f := hudbflag;
        end;
        if f <> nil then
        begin
          fw := f.width + 8; (* + space *)
          r_Common_DrawTexture(f, x + w - 16, y + 240 - 32 - 4, f.width, f.height, TBasePoint.BP_RIGHTUP);
        end;
      end;
      r_Common_DrawText(IntToStr(gTeamStat[TEAM_BLUE].Score), x + w - 16 - fw, y + 240 - 32 - 4, TEAMCOLOR[TEAM_BLUE].R, TEAMCOLOR[TEAM_BLUE].G, TEAMCOLOR[TEAM_BLUE].B, 255, menufont, TBasePoint.BP_RIGHTUP);
    end;

    if gGameSettings.GameType in [GT_CUSTOM, GT_SERVER, GT_CLIENT] then
    begin
      if gShowStat then
      begin
        r_Common_DrawText(IntToStr(p.Frags), x + w - 16, y, 255, 0, 0, 255, menufont, TBasePoint.BP_RIGHTUP);

        top := 1;
        maxFrags := 0;
        totalPlayers := 0;
        stat := g_Player_GetStats();
        if stat <> nil then
        begin
          totalPlayers := Length(stat);
          for i := 0 to High(stat) do
          begin
            if stat[i].Name <> p.Name then
            begin
              maxFrags := MAX(maxFrags, stat[i].Frags);
              if stat[i].Frags > p.Frags then
                top := top + 1;
            end;
          end;
        end;
        if p.Frags >= maxFrags then sign := '+' else sign := '-';
        r_Common_DrawText(IntToStr(top) + ' / ' + IntToStr(totalPlayers) + ' ' + sign + IntToStr(ABS(p.Frags - maxFrags)), x + w - 16, y + 32, 255, 0, 0, 255, smallfont, TBasePoint.BP_RIGHTUP);
      end;

      if gLMSRespawn > LMS_RESPAWN_NONE then
      begin
        r_Common_DrawText(_lc[I_GAME_WARMUP], x + w - 16 - 64, y + h, 0, 255, 0, 255, menufont, TBasePoint.BP_RIGHTDOWN);
        r_Common_DrawText(': ' + IntToStr((gLMSRespawnTime - gTime) div 1000), x + w - 16 - 64, y + h, 0, 255, 0, 255, menufont, TBasePoint.BP_LEFTDOWN);
      end
      else if gShowLives and (gGameSettings.MaxLives > 0) then
      begin
        r_Common_DrawText(IntToStr(p.Lives), x + w - 16, y + h, 0, 255, 0, 255, menufont, TBasePoint.BP_RIGHTDOWN);
      end;
    end;
  end;

  procedure r_Render_DrawView (x, y, w, h: Integer; p: TPlayer);
    var l, t, r, b, xx, yy, cx, cy: Integer;
  begin
    r_Draw_GetRect(l, t, r, b);
    r_Draw_SetRect(x, y, x + w, y + h);

    r_Common_GetCameraPos(p, true, xx, yy);
    if p <> nil then
    begin
      r_Map_Draw(x, y, w, h, xx, yy, p, cx, cy);
      {$IFDEF ENABLE_HOLMES}
        if p = gPlayer1 then
        begin
          r_Holmes_plrViewPos(cx, cy);
          r_Holmes_plrViewSize(h, w);
        end;
      {$ENDIF}
      r_Render_DrawStatsView(x, y, w, h, p);
      if p.Spectator and p.NoRespawn then
        r_Common_DrawText(_lc[I_PLAYER_SPECT4], x div 2 + w div 2, y div 2 + h div 2, 255, 255, 255, 255, stdfont, TBasePoint.BP_CENTER);
    end
    else
    begin
      r_Map_Draw(x, y, w, h, xx, yy, nil, cx, cy);
    end;

    r_Draw_SetRect(l, t, r, b);
  end;

  procedure r_Render_DrawMapView (x, y, w, h, camx, camy: Integer);
    var l, t, r, b, cx, cy: Integer;
  begin
    r_Draw_GetRect(l, t, r, b);
    r_Draw_SetRect(x, y, x + w, y + h);
    r_Map_Draw(x, y, w, h, camx, camy, nil, cx, cy);
    r_Draw_SetRect(l, t, r, b);
  end;

  procedure r_Render_DrawPlayerView (x, y, w, h: Integer; p: TPlayer);
    var l, t, r, b: Integer;
  begin
    r_Draw_GetRect(l, t, r, b);
    r_Draw_SetRect(x, y, x + w, y + h);
    r_Render_DrawView(x, y, w - 196, h, p);
    r_Render_DrawHUDArea(x + w - 196, y, 196, h, p);
    r_Draw_SetRect(l, t, r, b);
  end;

  procedure r_Render_DrawServerList (var SL: TNetServerList; var ST: TNetServerTable);
    var ip: AnsiString; ww, hh, cw, ch, mw, mh, motdh, scrx, scry, i, mx, y: Integer; msg: SSArray; Srv: TNetServer;
  begin
    scrx := gScreenWidth div 2;
    scry := gScreenHeight div 2;

    r_Draw_GetTextSize(_lc[I_NET_SLIST], menufont, ww, hh);
    r_Common_DrawText(_lc[I_NET_SLIST], gScreenWidth div 2, 16, 255, 255, 255, 255, menufont, TBasePoint.BP_UP);

    r_Draw_GetTextSize('W', stdfont, cw, ch);
    motdh := gScreenHeight - 49 - ch * b_Text_LineCount(slMOTD);

    r_Draw_FillRect(16, 64, gScreenWidth - 16, motdh, 64, 64, 64, 145);
    r_Draw_Rect(16, 64, gScreenWidth - 16, motdh, 255, 127, 0, 255);

    r_Common_DrawText(_lc[I_NET_SLIST_HELP], gScreenWidth div 2, gScreenHeight - 8, 255, 255, 255, 255, stdfont, TBasePoint.BP_DOWN);

    if slMOTD <> '' then
    begin
      r_Draw_FillRect(16, motdh, gScreenWidth - 16, gScreenHeight - 44, 64, 64, 64, 110);
      r_Draw_Rect(16, motdh, gScreenWidth - 16, gScreenHeight - 44, 255, 127, 0, 255);
      r_Common_DrawFormatText(slMOTD, 20, motdh + 3, 255, stdfont, TBasePoint.BP_LEFTUP);
    end;

    if not slReadUrgent and (slUrgent <> '') then
    begin
      r_Draw_FillRect(17, 65, gScreenWidth - 17, motdh - 1, 64, 64, 64, 127);
      r_Draw_FillRect(scrx - 256, scry - 60, scrx + 256, scry + 60, 64, 64, 64, 127);
      r_Draw_Rect(scrx - 256, scry - 60, scrx + 256, scry + 60, 255, 127, 0, 255);
      r_Draw_FillRect(scrx - 256, scry - 40, scrx + 256, scry - 40, 255, 127, 0, 255);
      r_Common_DrawText(_lc[I_NET_SLIST_URGENT], scrx, scry - 58, 255, 255, 255, 255, stdfont, TBasePoint.BP_UP);
      r_Common_DrawFormatText(slUrgent, scrx - 253, scry - 38, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Common_DrawText(_lc[I_NET_SLIST_URGENT_CONT], scrx, scry + 41, 255, 255, 255, 255, stdfont, TBasePoint.BP_UP);
      r_Draw_FillRect(scrx - 256, scry + 40, scrx + 256, scry + 40, 255, 127, 0, 255);
    end
    else if SL = nil then
    begin
      r_Draw_FillRect(17, 65, gScreenWidth - 17, motdh - 1, 64, 64, 64, 127);
      r_Draw_Rect(scrx - 192, scry - 10, scrx + 192, scry + 11, 255, 127, 0, 255);
      r_Common_DrawText(slWaitStr, scrx, scry, 255, 255, 255, 255, stdfont, TBasePoint.BP_CENTER);
    end
    else
    begin
      y := 90;
      if slSelection < Length(ST) then
      begin
        sy := y + 42 * slSelection - 4;
        Srv := GetServerFromTable(slSelection, SL, ST);
        ip := _lc[I_NET_ADDRESS] + ' ' + Srv.IP + ':' + IntToStr(Srv.Port);
        ip := ip + '  ' + _lc[I_NET_SERVER_PASSWORD] + ' ';
        if Srv.Password then ip := ip + _lc[I_MENU_YES] else ip := ip +_lc[I_MENU_NO];
      end;

      mw := gScreenWidth - 188;
      mx := 16 + mw;

      r_Draw_FillRect(16 + 1, sy, gScreenWidth - 16 - 1, sy + 40, 64, 64, 64, 255);
      r_Draw_FillRect(16 + 1, sy, gScreenWidth - 16 - 1, sy, 205, 205, 205, 255);
      r_Draw_FillRect(16 + 1, sy + 41, gScreenWidth - 16 - 1, sy + 41, 255, 255, 255, 255);

      r_Draw_FillRect(16, 85, gScreenWidth - 16, 85, 255, 127, 0, 255);
      r_Draw_FillRect(16, motdh - 20, gScreenWidth - 16, motdh - 20, 255, 127, 0, 255);

      r_Draw_FillRect(mx - 70, 64, mx - 70, motdh, 255, 127, 0, 255);
      r_Draw_FillRect(mx, 64, mx, motdh - 20, 255, 127, 0, 255);
      r_Draw_FillRect(mx + 52, 64, mx + 52, motdh - 20, 255, 127, 0, 255);
      r_Draw_FillRect(mx + 104, 64, mx + 104, motdh - 20, 255, 127, 0, 255);

      r_Common_DrawText('NAME/MAP', 18, 68, 255, 127, 0, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Common_DrawText('PING', mx - 68, 68, 255, 127, 0, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Common_DrawText('MODE', mx + 2, 68, 255, 127, 0, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Common_DrawText('PLRS', mx + 54, 68, 255, 127, 0, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Common_DrawText('VER', mx + 106, 68, 255, 127, 0, 255, stdfont, TBasePoint.BP_LEFTUP);

      for i := 0 to High(ST) do
      begin
        Srv := GetServerFromTable(i, SL, ST);
        r_Common_DrawText(Srv.Name, 18, y, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
        r_Common_DrawText(Srv.Map,  18, y + 16, 210, 210, 210, 255, stdfont, TBasePoint.BP_LEFTUP);

        if Srv.Ping = 0 then
          r_Common_DrawText('<1' + _lc[I_NET_SLIST_PING_MS], mx - 68, y, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP)
        else if (Srv.Ping >= 0) and (Srv.Ping <= 999) then
          r_Common_DrawText(IntToStr(Srv.Ping) + _lc[I_NET_SLIST_PING_MS], mx - 68, y, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP)
        else
          r_Common_DrawText(_lc[I_NET_SLIST_NO_ACCESS], mx - 68, y, 255, 0, 0, 255, stdfont, TBasePoint.BP_LEFTUP);
        if Length(ST[I].Indices) > 1 then
          r_Common_DrawText('<' + IntToStr(Length(ST[I].Indices)) + '>', mx - 68, y + 16, 210, 210, 210, 255, stdfont, TBasePoint.BP_LEFTUP);

        r_Common_DrawText(g_Game_ModeToText(Srv.GameMode), mx + 2, y, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);

        r_Common_DrawText(IntToStr(Srv.Players) + '/' + IntToStr(Srv.MaxPlayers), mx + 54, y, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
        r_Common_DrawText(IntToStr(Srv.LocalPl) + '+' + IntToStr(Srv.Bots), mx + 54, y + 16, 210, 210, 210, 255, stdfont, TBasePoint.BP_LEFTUP);

        r_Common_DrawText(IntToStr(Srv.Protocol), mx + 106, y, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);

        y := y + 42;
      end;

      r_Common_DrawText(ip, 20, motdh - 20 + 3, 205, 205, 205, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Common_DrawText(IntToStr(Length(ST)) + _lc[I_NET_SLIST_SERVERS], gScreenWidth - 48, motdh - 20 + 3, 255, 255, 255, 255, stdfont, TBasePoint.BP_RIGHTUP);
    end;
  end;

  procedure r_Render_DrawStatsColumns (constref cs: TEndCustomGameStat; x, y, w: Integer; endview: Boolean);
    var i, cw, ch, yy, team, players, w1, w2, w3, w4, tw: Integer; r, g, b, rr, gg, bb: Byte; s: AnsiString;
  begin
    r_Draw_GetTextSize('W', stdfont, cw, ch);
    w4 := cw * 6;           (* deaths width *)
    w3 := cw * 8;           (* frags width *)
    w2 := cw * 12;          (* ping/loss width *)
    w1 := w - w2 - w3 - w4; (* name width *)
    tw := w1 - cw * 2 - w2; (* team goals *)
    if cs.PlayerStat = nil then players := 0 else players := Length(cs.PlayerStat);
    yy := y;
    if cs.GameMode in [GM_TDM, GM_CTF] then
    begin
      for team := TEAM_RED to TEAM_BLUE do
      begin
        case team of
          TEAM_RED:
            begin
              s := _lc[I_GAME_TEAM_RED];
              r := 255; g := 0; b := 0;
            end;
          TEAM_BLUE:
            begin
              s := _lc[I_GAME_TEAM_BLUE];
              r := 0; g := 0; b := 255;
            end;
         end;
         r_Common_DrawText(s, x, yy, r, g, b, 255, stdfont, TBasePoint.BP_LEFTUP);
         r_Common_DrawText(IntToStr(cs.TeamStat[team].Score), x + tw, yy, r, g, b, 255, stdfont, TBasePoint.BP_UP);
         if endview = false then
           r_Common_DrawText(_lc[I_GAME_PING], x + w1, yy, r, g, b, 255, stdfont, TBasePoint.BP_UP);
         r_Common_DrawText(_lc[I_GAME_FRAGS], x + w1 + w2, yy, r, g, b, 255, stdfont, TBasePoint.BP_UP);
         r_Common_DrawText(_lc[I_GAME_DEATHS], x + w1 + w2 + w3, yy, r, g, b, 255, stdfont, TBasePoint.BP_UP);
         INC(yy, ch);

         INC(yy, ch div 4);
         r_Draw_FillRect(x, yy, x + w - 1, yy, r, g, b, 255);
         INC(yy, ch div 4);

         for i := 0 to players - 1 do
         begin
           if cs.PlayerStat[i].Team = team then
           begin
             rr := r; gg := g; bb := b;
             if cs.PlayerStat[i].Spectator then
             begin
               rr := r div 2; gg := g div 2; bb := b div 2;
             end;

             // Player name
             if gShowPIDs then s := Format('[%5d] %s', [cs.PlayerStat[i].UID, cs.PlayerStat[i].Name]) else s := cs.PlayerStat[i].Name;
             if (gPlayers[cs.PlayerStat[i].Num] <> nil) and (gPlayers[cs.PlayerStat[i].Num].FReady) then s := s + ' *';
             r_Common_DrawText(s, x, yy, rr, gg, bb, 255, stdfont, TBasePoint.BP_LEFTUP);
             if endview = false then
             begin
               // Player ping/loss
               s := Format(_lc[I_GAME_PING_MS], [cs.PlayerStat[i].Ping, cs.PlayerStat[i].Loss]);
               r_Common_DrawText(s, x + w1, yy, rr, gg, bb, 255, stdfont, TBasePoint.BP_UP);
             end;
             // Player frags
             s := IntToStr(cs.PlayerStat[i].Frags);
             r_Common_DrawText(s, x + w1 + w2, yy, rr, gg, bb, 255, stdfont, TBasePoint.BP_UP);
             // Player deaths
             s := IntToStr(cs.PlayerStat[i].Deaths);
             r_Common_DrawText(s, x + w1 + w2 + w3, yy, rr, gg, bb, 255, stdfont, TBasePoint.BP_UP);

             INC(yy, ch);
           end;
         end;
         INC(yy, ch);
      end;
    end
    else if cs.GameMode in [GM_DM, GM_COOP] then
    begin
      r_Common_DrawText(_lc[I_GAME_PLAYER_NAME], x, yy, 255, 127, 0, 255, stdfont, TBasePoint.BP_LEFTUP);
      if endview = false then
        r_Common_DrawText(_lc[I_GAME_PING], x + w1, yy, 255, 127, 0, 255, stdfont, TBasePoint.BP_UP);
      r_Common_DrawText(_lc[I_GAME_FRAGS], x + w1 + w2, yy, 255, 127, 0, 255, stdfont, TBasePoint.BP_UP);
      r_Common_DrawText(_lc[I_GAME_DEATHS], x + w1 + w2 + w3, yy, 255, 127, 0, 255, stdfont, TBasePoint.BP_UP);
      INC(yy, ch + ch div 2);
      for i := 0 to players - 1 do
      begin
        // rr := 255; gg := 127; bb := 0;
        rr := 255; gg := 255; bb := 255;
        if cs.PlayerStat[i].Spectator then
        begin
          rr := rr div 2; gg := gg div 2; bb := bb div 2;
        end;

        // Player color
        r_Draw_Rect(x, yy, x + 16 - 1, yy + 16 - 1, 192, 192, 192, 255);
        r_Draw_FillRect(x + 1, yy + 1, x + 16 - 1, yy + 16 - 1, cs.PlayerStat[i].Color.R, cs.PlayerStat[i].Color.G, cs.PlayerStat[i].Color.B, 255);
        // Player name
        if gShowPIDs then s := Format('[%5d] %s', [cs.PlayerStat[i].UID, cs.PlayerStat[i].Name]) else s := cs.PlayerStat[i].Name;
        if (gPlayers[cs.PlayerStat[i].Num] <> nil) and (gPlayers[cs.PlayerStat[i].Num].FReady) then s := s + ' *';
        r_Common_DrawText(s, x + 16 + 8, yy, rr, gg, bb, 255, stdfont, TBasePoint.BP_LEFTUP);
        if endview = false then
        begin
          // Player ping/loss
          s := Format(_lc[I_GAME_PING_MS], [cs.PlayerStat[i].Ping, cs.PlayerStat[i].Loss]);
          r_Common_DrawText(s, x + w1, yy, rr, gg, bb, 255, stdfont, TBasePoint.BP_UP);
        end;
        // Player frags
        s := IntToStr(cs.PlayerStat[i].Frags);
        r_Common_DrawText(s, x + w1 + w2, yy, rr, gg, bb, 255, stdfont, TBasePoint.BP_UP);
        // Player deaths
        s := IntToStr(cs.PlayerStat[i].Deaths);
        r_Common_DrawText(s, x + w1 + w2 + w3, yy, rr, gg, bb, 255, stdfont, TBasePoint.BP_UP);

        INC(yy, ch + ch div 2);
      end;
    end;
  end;

  procedure r_Render_DrawStatsWindow (x, y, w, h: Integer; cs: TEndCustomGameStat; endview: Boolean);
    var xoff, yoff, cw, ch: Integer; s: AnsiString;
  begin
    xoff := 0; yoff := 8;
    r_Draw_GetTextSize('W', stdfont, cw, ch);
    r_Draw_FillRect(x, y, x + w - 1, y + h - 1, 64, 64, 64, 224);
    r_Draw_Rect(x, y, x + w - 1, y + h - 1, 255, 127, 0, 255);

    (* LINE 1 *)

    if endview = false then
    begin
      case NetMode of
        NET_SERVER: s := _lc[I_NET_SERVER];
        NET_CLIENT: s := NetClientIP + ':' + IntToStr(NetClientPort);
        otherwise   s := '';
      end;
      r_Common_DrawText(s, x + 16, y + yoff, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
    end;

    case cs.GameMode of
      GM_DM:    if gGameSettings.MaxLives = 0 then s := _lc[I_GAME_DM] else s := _lc[I_GAME_LMS];
      GM_TDM:   if gGameSettings.MaxLives = 0 then s := _lc[I_GAME_TDM] else s := _lc[I_GAME_TLMS];
      GM_CTF:   s := _lc[I_GAME_CTF];
      GM_COOP:  if gGameSettings.MaxLives = 0 then s := _lc[I_GAME_COOP] else s := _lc[I_GAME_SURV];
      otherwise s := 'GAME MODE ' + IntToStr(gGameSettings.GameMode);
    end;
    r_Common_DrawText(s, x + w div 2, y + yoff, 255, 255, 255, 255, stdfont, TBasePoint.BP_UP);

    if endview = false then
    begin
      s := r_Common_TimeToStr(cs.GameTime);
      r_Common_DrawText(s, x + w - 16, y + yoff, 255, 255, 255, 255, stdfont, TBasePoint.BP_RIGHTUP);
    end;

    INC(yoff, ch + ch div 2);

    (* LINE 2/3 *)

    s := cs.Map;
    if cs.MapName <> '' then
      s := s + ' - ' + cs.MapName;

    if endview = false then
    begin
      r_Common_DrawText(s, x + w div 2, y + yoff, 200, 200, 200, 255, stdfont, TBasePoint.BP_UP);
      INC(yoff, ch + ch div 2);
      case cs.GameMode of
        GM_DM, GM_TDM: s := Format(_lc[I_GAME_FRAG_LIMIT], [gGameSettings.ScoreLimit]);
        GM_CTF:        s := Format(_lc[I_GAME_SCORE_LIMIT], [gGameSettings.ScoreLimit]);
        GM_COOP:       s := _lc[I_GAME_MONSTERS] + ' ' + IntToStr(gCoopMonstersKilled) + '/' + IntToStr(gTotalMonsters);
        otherwise      s := '';
      end;
      r_Common_DrawText(s, x + 16, y + yoff, 200, 200, 200, 255, stdfont, TBasePoint.BP_LEFTUP);
      case cs.GameMode of
        GM_DM, GM_TDM, GM_CTF: s := Format(_lc[I_GAME_TIME_LIMIT], [gGameSettings.TimeLimit div 3600, (gGameSettings.TimeLimit div 60) mod 60, gGameSettings.TimeLimit mod 60]);
        GM_COOP:               s := _lc[I_GAME_SECRETS] + ' ' + IntToStr(gCoopSecretsFound) + '/' + IntToStr(gSecretsCount);
        otherwise              s := '';
      end;
      r_Common_DrawText(s, x + w - 16, y + yoff, 200, 200, 200, 255, stdfont, TBasePoint.BP_RIGHTUP);
      INC(yoff, ch);
    end
    else
    begin
      xoff := MAX(Length(_lc[I_MENU_MAP]) + 1, Length(_lc[I_GAME_GAME_TIME]) + 1) * cw;
      r_Common_DrawText(_lc[I_MENU_MAP], x + 16, y + yoff, 255, 127, 0, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Common_DrawText(s, x + 16 + xoff, y + yoff, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
      INC(yoff, ch);
      r_Common_DrawText(_lc[I_GAME_GAME_TIME], x + 16, y + yoff, 255, 127, 0, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Common_DrawText(r_Common_TimeToStr(cs.GameTime), x + 16 + xoff, y + yoff, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
      INC(yoff, ch);
    end;

    INC(yoff, ch);

    (* LINE 4/5 *)

    if endview and (cs.GameMode = GM_COOP) then
    begin
      xoff := MAX(Length(_lc[I_GAME_MONSTERS]) + 1, Length(_lc[I_GAME_SECRETS]) + 1) * cw;
      r_Common_DrawText(_lc[I_GAME_MONSTERS], x + 16, y + yoff, 255, 127, 0, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Common_DrawText(IntToStr(gCoopMonstersKilled) + '/' + IntToStr(gTotalMonsters), x + 16 + xoff, y + yoff, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
      INC(yoff, ch);
      r_Common_DrawText(_lc[I_GAME_SECRETS], x + 16, y + yoff, 255, 127, 0, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Common_DrawText(IntToStr(gCoopSecretsFound) + '/' + IntToStr(gSecretsCount), x + 16 + xoff, y + yoff, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);      
      INC(yoff, ch);
      INC(yoff, ch);
    end;

    (* LINE 6/7 *)

    if endview and (cs.GameMode = GM_COOP) and gLastMap then
    begin
      xoff := MAX(Length(_lc[I_GAME_MONSTERS_TOTAL]) + 1, Length(_lc[I_GAME_SECRETS_TOTAL]) + 1) * cw;
      r_Common_DrawText(_lc[I_GAME_MONSTERS_TOTAL], x + 16, y + yoff, 255, 127, 0, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Common_DrawText(IntToStr(gCoopTotalMonstersKilled) + '/' + IntToStr(gCoopTotalMonsters), x + 16 + xoff, y + yoff, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
      INC(yoff, ch);
      r_Common_DrawText(_lc[I_GAME_SECRETS_TOTAL], x + 16, y + yoff, 255, 127, 0, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Common_DrawText(IntToStr(gCoopTotalSecretsFound) + '/' + IntToStr(gCoopTotalSecrets), x + 16 + xoff, y + yoff, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
      INC(yoff, ch);
      INC(yoff, ch);
    end;

    (* LINE *)

    if endview and (cs.GameMode in [GM_TDM, GM_CTF]) then
    begin
      if cs.TeamStat[TEAM_RED].Score > cs.TeamStat[TEAM_BLUE].Score then s := _lc[I_GAME_WIN_RED]
      else if cs.TeamStat[TEAM_BLUE].Score > cs.TeamStat[TEAM_RED].Score then s := _lc[I_GAME_WIN_BLUE]
      else s := _lc[I_GAME_WIN_DRAW];
      r_Common_DrawText(s, x + w div 2, y + yoff, 255, 255, 255, 255, stdfont, TBasePoint.BP_UP);
      INC(yoff, ch);
      INC(yoff, ch);
    end;

    (* LINE n *)

    r_Render_DrawStatsColumns(cs, x + 16, y + yoff, w - 16 - 16, endview);
  end;

  function r_Render_StatsHeight (players: Integer): Integer;
    var cw, ch: Integer;
  begin
    ASSERT(players >= 0);
    r_Draw_GetTextSize('W', stdfont, cw, ch);
    case gGameSettings.GameMode of
      GM_TDM, GM_CTF: result := 32 + ch * (11 + players);
      otherwise       result := 40 + ch * 5 + (ch + 8) * players;
    end;
  end;

  procedure r_Render_DrawStats;
    var x, y, w, h, players: Integer; cs: TEndCustomGameStat;
  begin
    cs.PlayerStat := g_Player_GetStats();
    SortGameStat(cs.PlayerStat);
    cs.TeamStat := gTeamStat;
    cs.GameTime := gTime;
    cs.GameMode := gGameSettings.GameMode;
    cs.Map := g_ExtractWadNameNoPath(gMapInfo.Map) + ':' + g_ExtractFileName(gMapInfo.Map);
    cs.MapName := gMapInfo.Name;
    if cs.PlayerStat = nil then players := 0 else players := Length(cs.PlayerStat);
    w := gScreenWidth - (gScreenWidth div 5);
    h := r_Render_StatsHeight(players);
    x := (gScreenWidth div 2) - (w div 2);
    y := (gScreenHeight div 2) - (h div 2);
    r_Render_DrawStatsWindow(x, y, w, h, cs, false);
  end;

  procedure r_Render_DrawCustomStats;
    var cw, ch, s: AnsiString;
  begin
    if gStatsOff then
    begin
      r_Common_DrawText(_lc[I_MENU_INTER_NOTICE_TAB], gScreenWidth div 2, 8, 255, 255, 255, 255, stdfont, TBasePoint.BP_UP);
    end
    else
    begin
      case gGameSettings.GameMode of
        GM_COOP: if gMissionFailed then s := _lc[I_MENU_INTER_MISSION_FAIL] else s := _lc[I_MENU_INTER_LEVEL_COMPLETE];
        otherwise s := _lc[I_MENU_INTER_ROUND_OVER];
      end;
      r_Common_DrawText(s, gScreenWidth div 2, 16, 255, 255, 255, 255, menufont, TBasePoint.BP_UP);

      if gChatShow = false then
      begin
        if g_Game_IsClient then s := _lc[I_MENU_INTER_NOTICE_MAP] else s := _lc[I_MENU_INTER_NOTICE_SPACE];
        r_Common_DrawText(s, gScreenWidth div 2, gScreenHeight - 4, 255, 255, 255, 255, stdfont, TBasePoint.BP_DOWN);
        if g_Game_IsNet then
        begin
          s := Format(_lc[I_MENU_INTER_NOTICE_TIME], [gServInterTime]);
          r_Common_DrawText(s, gScreenWidth div 2, gScreenHeight - 16 - 4, 255, 255, 255, 255, stdfont, TBasePoint.BP_DOWN);
        end;
      end;

      r_Render_DrawStatsWindow(32, 64, gScreenWidth - 32 * 2, gScreenHeight - 64 * 2, CustomStat, true);
    end;
  end;

  procedure r_Render_DrawValueOf (a, b, x, y: Integer; f: TGLFont);
    var wa, wb, ch: Integer; sa, sb: AnsiString;
  begin
    sa := IntToStr(a);
    sb := IntToStr(b);
    r_Draw_GetTextSize(sa, f, wa, ch);
    r_Draw_GetTextSize(sa + ' / ', f, wb, ch);
    r_Common_DrawText(sa, x, y, 255, 0, 0, 255, f, TBasePoint.BP_LEFTUP);
    r_Common_DrawText(' / ', x + wa, y, 255, 255, 255, 255, f, TBasePoint.BP_LEFTUP);
    r_Common_DrawText(sb, x + wb, y, 255, 0, 0, 255, f, TBasePoint.BP_LEFTUP);
  end;

  procedure r_Render_DrawSinglStatsPlayer (player, x, y, w1: Integer);
    var time, kpm: Single;
  begin
    r_Common_DrawText(_lc[I_MENU_INTER_KILLS], x, y, 255, 255, 255, 255, menufont, TBasePoint.BP_LEFTUP);
    r_Render_DrawValueOf(SingleStat.PlayerStat[player].Kills, gTotalMonsters, x + w1, y, MenuFont);
    r_Common_DrawText(_lc[I_MENU_INTER_KPM], x, y + 32, 255, 255, 255, 255, menufont, TBasePoint.BP_LEFTUP);
    time := SingleStat.GameTime / 1000;
    kpm := SingleStat.PlayerStat[player].Kills;
    if time > 0 then kpm := kpm / time * 60;
    r_Common_DrawText(Format('%.1f', [kpm]), x + w1, y + 32, 255, 0, 0, 255, menufont, TBasePoint.BP_LEFTUP);
    r_Common_DrawText(_lc[I_MENU_INTER_SECRETS], x, y + 64, 255, 255, 255, 255, menufont, TBasePoint.BP_LEFTUP);
    r_Render_DrawValueOf(SingleStat.PlayerStat[player].Secrets, SingleStat.TotalSecrets, x + w1, y + 64, MenuFont);
  end;

  procedure r_Render_DrawSingleStats;
    var xx, wa, wb, ww, ch: Integer; s: AnsiString;
  begin
    r_Common_DrawText(_lc[I_MENU_INTER_LEVEL_COMPLETE], gScreenWidth div 2, 32, 255, 255, 255, 255, menufont, TBasePoint.BP_UP);

    r_Draw_GetTextSize(_lc[I_MENU_INTER_KPM] + '  ', menufont, wa, ch);
    r_Draw_GetTextSize(' 9999.9', menufont, wb, ch);
    ww := wa + wb;
    xx := gScreenWidth div 2 - ww div 2;

    s := r_Common_TimeToStr(SingleStat.GameTime);
    r_Common_DrawText(_lc[I_MENU_INTER_TIME], xx, 80, 255, 255, 255, 255, menufont, TBasePoint.BP_LEFTUP);
    r_Common_DrawText(s, xx + wa, 80, 255, 0, 0, 255, menufont, TBasePoint.BP_LEFTUP);

    if SingleStat.TwoPlayers then
    begin
      r_Common_DrawText(_lc[I_MENU_PLAYER_1], gScreenWidth div 2, 128, 255, 255, 255, 255, menufont, TBasePoint.BP_UP);
      r_Render_DrawSinglStatsPlayer(0, xx, 176, wa);
      r_Common_DrawText(_lc[I_MENU_PLAYER_2], gScreenWidth div 2, 288, 255, 255, 255, 255, menufont, TBasePoint.BP_UP);
      r_Render_DrawSinglStatsPlayer(1, xx, 336, wa);
    end
    else
    begin
      r_Render_DrawSinglStatsPlayer(0, xx, 128, wa);
    end;
  end;

  procedure r_Render_DrawSpectHud;
    var xoff: Integer; s: AnsiString;

    procedure AddText (s1, s2: AnsiString);
      var w1, w2, ww, ch: Integer;
    begin
      r_Draw_GetTextSize(s1, stdfont, w1, ch);
      r_Draw_GetTextSize(s2, stdfont, w2, ch);
      ww := MAX(w1, w2);
      r_Common_DrawText(s1, xoff + ww div 2, gScreenHeight - ch, 255, 255, 255, 255, stdfont, TBasePoint.BP_DOWN);
      r_Common_DrawText(s2, xoff + ww div 2, gScreenHeight - ch, 255, 255, 255, 255, stdfont, TBasePoint.BP_UP);
      xoff := xoff + ww + 16;
    end;

  begin
    xoff := 0;
    case gSpectMode of
      SPECT_STATS:   s := 'MODE: Stats';
      SPECT_MAPVIEW: s := 'MODE: Observe Map';
      SPECT_PLAYERS: s := 'MODE: Watch Players';
      otherwise      s := 'MODE: ' + IntToStr(gSpectMode);
    end;
    AddText(s, '< jump >');
    if gSpectMode = SPECT_STATS then
      AddText('Autoview', '< fire >');
    if gSpectMode = SPECT_MAPVIEW then
      AddText('[-] Step ' + IntToStr(gSpectStep) + ' [+]', '<prev weap> <next weap>');
    if gSpectMode = SPECT_PLAYERS then
    begin
      AddText('Player 1', '<left/right>');
      if gSpectViewTwo then
        AddText('Player 2', '<prev w/next w>');
      AddText('2x View', '<up/down>');
    end;
  end;

  function GetActivePlayer_ByID (id: Integer): TPlayer;
    var i, len: Integer; p: TPlayer;
  begin
    p := nil;
    if (id >= 0) and (gPlayers <> nil) then
    begin
      i := 0; len := Length(gPlayers);
      while (i < len) and ((IsActivePlayer(gPlayers[i]) = false) or (gPlayers[i].UID <> id)) do INC(i);
      if i < len then p := gPlayers[i];
    end;
    result := p;
  end;

  procedure r_Render_DrawMinimap (x, y: Integer; alpha: Byte);
    const scale = 16;

    function IsMinimapPanel (const p: TPanel): Boolean;
    begin
      result := (p <> nil) and p.Enabled;
      if result then
      begin
        case p.PanelType of
          PANEL_WALL, PANEL_WATER, PANEL_ACID1, PANEL_ACID2,
          PANEL_STEP, PANEL_OPENDOOR, PANEL_CLOSEDOOR,
          PANEL_LIFTUP, PANEL_LIFTDOWN, PANEL_LIFTLEFT, PANEL_LIFTRIGHT:
            result := true;
          otherwise
            result := false;
        end;
      end;
    end;

    procedure DrawObject (xx, yy, ww, hh: Integer; r, g, b: Byte);
      var x0, y0, x1, y1: Integer;
    begin
      x0 := x + xx div scale;
      y0 := y + yy div scale;
      x1 := x + (xx + ww - 1) div scale;
      y1 := y + (yy + hh - 1) div scale;
      r_Draw_FillRect(x0, y0, x1, y1, r, g, b, alpha);
    end;

    procedure DrawPanels (const a: TPanelArray);
      var i: Integer; p: TPanel; c: TRGB;
    begin
      if a <> nil then
      begin
        for i := 0 to HIGH(a) do
        begin
          p := a[i];
          if IsMinimapPanel(p) then
          begin
            case p.PanelType of
              PANEL_WALL:       c := _RGB(208, 208, 208);
              PANEL_OPENDOOR:   c := _RGB(160, 160, 160);
              PANEL_CLOSEDOOR:  c := _RGB(160, 160, 160);
              PANEL_STEP:       c := _RGB(128, 128, 128);
              PANEL_LIFTUP, PANEL_LIFTDOWN, PANEL_LIFTLEFT, PANEL_LIFTRIGHT:
              case p.LiftType of
                LIFTTYPE_UP:    c := _RGB(116, 72, 36);
                LIFTTYPE_DOWN:  c := _RGB(116, 124, 96);
                LIFTTYPE_LEFT:  c := _RGB(116, 200, 80);
                LIFTTYPE_RIGHT: c := _RGB(116, 252, 140);
                otherwise       c := _RGB(255, 0, 0);
              end;
              PANEL_WATER:      c := _RGB(0, 0, 192);
              PANEL_ACID1:      c := _RGB(0, 176, 0);
              PANEL_ACID2:      c := _RGB(176, 0, 0);
              otherwise         c := _RGB(255, 0, 0);
            end;
            DrawObject(p.x, p.y, p.width, p.height, c.r, c.g, c.b);
          end;
        end;
      end;
    end;

    procedure DrawPlayers;
      var i: Integer; p: TPlayer; c: TRGB;
    begin
      if gPlayers <> nil then
      begin
        for i := 0 to HIGH(gPlayers) do
        begin
          p := gPlayers[i];
          if p.Alive then
          begin
            case p.Team of
              TEAM_RED:  c := _RGB(255, 0, 0);
              TEAM_BLUE: c := _RGB(0, 0, 255);
              otherwise  c := _RGB(255, 128, 0);
            end;
            DrawObject(p.obj.x, p.obj.y, p.obj.rect.width, p.obj.rect.height, c.r, c.g, c.b);
          end;
        end;
      end;
    end;

    function DrawMonster (m: TMonster): Boolean;
    begin
      result := false; // don't stop
      if m.alive then
        DrawObject(m.obj.x, m.obj.y, m.obj.rect.width, m.obj.rect.height, 255, 255, 0);
    end;

  begin
    r_Draw_FillRect(x, y, (x + gMapInfo.Width - 1) div scale, (y + gMapInfo.Height - 1) div scale, 0, 0, 0, alpha);
    DrawPanels(gSteps);
    DrawPanels(gLifts);
    DrawPanels(gWater);
    DrawPanels(gAcid1);
    DrawPanels(gAcid2);
    DrawPanels(gWalls);
    g_Mons_ForEach(DrawMonster);
    DrawPlayers;
  end;

  procedure r_Render_Draw;
    var p1, p2: TPlayer; time: LongWord;
  begin
    if gExit = EXIT_QUIT then
      exit;

    INC(FPSCounter);
    time := GetTickCount64();
    if time - FPSTime >= 1000 then
    begin
      FPS := FPSCounter;
      FPSCounter := 0;
      FPSTime := time;
    end;

    r_Draw_Setup(gWinSizeX, gWinSizeY, gScreenWidth, gScreenHeight);

    glClearColor(0.0, 0.0, 0.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    p1 := nil;
    p2 := nil;
    if gGameOn or (gState = STATE_FOLD) then
    begin
      if (gPlayer1 <> nil) and (gPlayer2 <> nil) then
      begin
        if gRevertPlayers then
        begin
          p1 := gPlayer2;
          p2 := gPlayer1;
        end
        else
        begin
          p1 := gPlayer1;
          p2 := gPlayer2;
        end;
      end
      else if gPlayer1 <> nil then
      begin
        p1 := gPlayer1;
      end
      else if gPlayer2 <> nil then
      begin
        p1 := gPlayer2;
      end;
      if (gSpectMode = SPECT_PLAYERS) and (gPlayers <> nil) then
      begin
        p1 := GetActivePlayer_ByID(gSpectPID1);
        if p1 = nil then
          p1 := GetActivePlayer_ByID(GetActivePlayerID_Next());
        if gSpectViewTwo then
        begin
          p2 := GetActivePlayer_ByID(gSpectPID2);
          if p2 = nil then
            p2 := GetActivePlayer_ByID(GetActivePlayerID_Next());
        end;
      end;
    end;

    if gGameOn or ((gState in [STATE_FOLD]) and (EndingGameCounter < 255)) then
    begin
      if gSpectMode = SPECT_MAPVIEW then
      begin
        r_Render_DrawMapView(0, 0, gScreenWidth, gScreenHeight, gSpectX + gScreenWidth div 2, gSpectY + gScreenHeight div 2);
      end
      else if (p1 <> nil) and (p2 <> nil) then
      begin
        r_Render_DrawPlayerView(0, 0, gScreenWidth, gScreenHeight div 2 - 2, p1);
        r_Render_DrawPlayerView(0, gScreenHeight div 2 + 2, gScreenWidth, gScreenHeight div 2, p2);
      end
      else if p1 <> nil then
      begin
        r_Render_DrawPlayerView(0, 0, gScreenWidth, gScreenHeight, p1);
      end
      else if p2 <> nil then
      begin
        r_Render_DrawPlayerView(0, 0, gScreenWidth, gScreenHeight, p2);
      end;

      if gShowMap then
        r_Render_DrawMiniMap(0, 0, 160);

      {$IFDEF ENABLE_HOLMES}
        r_Holmes_Draw;
      {$ENDIF}

      if MessageText <> '' then
        r_Common_DrawFormatText(MessageText, (gScreenWidth - 196) div 2, gScreenHeight div 2, 255, menufont, TBasePoint.BP_CENTER);

      if IsDrawStat or (gSpectMode = SPECT_STATS) then
        r_Render_DrawStats;

      if gSpectHUD and (gChatShow = false) and (gSpectMode <> SPECT_NONE) and (gSpectAuto = false) then
        r_Render_DrawSpectHud;
    end;

    if gPauseMain and gGameOn {$IFDEF ENABLE_MENU}and (g_ActiveWindow = nil){$ENDIF} then
    begin
      r_Draw_FillRect(0, 0, gScreenWidth - 1, gScreenHeight - 1, 0, 0, 0, 105);
      r_Common_DrawText(_lc[I_MENU_PAUSE], gScreenWidth div 2, gScreenHeight div 2, 255, 255, 255, 255, menufont, TBasePoint.BP_CENTER);
    end;

    if not gGameOn then
    begin
      // TODO F key handle
      case gState of
        STATE_NONE: (* do nothing *) ;
        STATE_MENU: r_Common_DrawBackground(GameWad + ':TEXTURES/TITLE');
        STATE_FOLD:
        begin
          if EndingGameCounter > 0 then
            r_Draw_FillRect(0, 0, gScreenWidth - 1, gScreenHeight - 1, 0, 0, 0, MIN(MAX(255 - EndingGameCounter, 0), 255));
        end;
        STATE_INTERCUSTOM:
        begin
          if gLastMap and (gGameSettings.GameMode = GM_COOP) then
            if EndPicPath <> '' then
              r_Common_DrawBackground(EndPicPath)
            else
              r_Common_DrawBackground(GameWad + ':TEXTURES/' + _lc[I_TEXTURE_ENDPIC])
          else
            r_Common_DrawBackground(GameWad + ':TEXTURES/INTER');

          r_Render_DrawCustomStats;

          {$IFDEF ENABLE_MENU}
            if g_ActiveWindow <> nil then
              r_Draw_FillRect(0, 0, gScreenWidth - 1, gScreenHeight - 1, 0, 0, 0, 105);
          {$ENDIF}
        end;
        STATE_INTERSINGLE, STATE_INTERTEXT, STATE_INTERPIC:
        begin
          if EndingGameCounter > 0 then
          begin
            r_Draw_FillRect(0, 0, gScreenWidth - 1, gScreenHeight - 1, 0, 0, 0, MIN(MAX(255 - EndingGameCounter, 0), 255));
          end
          else
          begin
            r_Common_DrawBackground(GameWad + ':TEXTURES/INTER');
            r_Render_DrawSingleStats;
            {$IFDEF ENABLE_MENU}
              if g_ActiveWindow <> nil then
                r_Draw_FillRect(0, 0, gScreenWidth - 1, gScreenHeight - 1, 0, 0, 0, 105);
            {$ENDIF}
          end;
        end;
        STATE_ENDPIC:
        begin
          if EndPicPath <> '' then
            r_Common_DrawBackground(EndPicPath)
          else
            r_Common_DrawBackground(GameWad + ':TEXTURES/' + _lc[I_TEXTURE_ENDPIC]);
          {$IFDEF ENABLE_MENU}
            if g_ActiveWindow <> nil then
              r_Draw_FillRect(0, 0, gScreenWidth - 1, gScreenHeight - 1, 0, 0, 0, 105);
          {$ENDIF}
        end;
        STATE_SLIST:
        begin
          r_Common_DrawBackground(GameWad + ':TEXTURES/TITLE');
          r_Draw_FillRect(0, 0, gScreenWidth - 1, gScreenHeight - 1, 0, 0, 0, 105);
          r_Render_DrawServerList(slCurrent, slTable);
        end;
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

    // TODO g_debug_Sounds

    if gShowFPS then
    begin
      r_Common_DrawText('FPS: ' + IntToStr(FPS), 0, 0, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
      r_Common_DrawText('UPS: ' + IntToStr(UPS), 0, 16, 255, 255, 255, 255, stdfont, TBasePoint.BP_LEFTUP);
    end;

    if gGameOn and gShowTime then
    begin
      r_Common_DrawText(r_Common_TimeToStr(gTime), gScreenWidth - 4, gScreenHeight - 1, 255, 255, 255, 255, stdfont, TBasePoint.BP_RIGHTDOWN);
    end;

    // TODO draw profilers

    {$IFDEF ENABLE_HOLMES}
      r_Holmes_DrawUI;
    {$ENDIF}

    // TODO draw touch screen controls

    sys_Repaint;
  end;

  procedure r_Render_Resize (w, h: Integer);
  begin
    gWinSizeX := w;
    gWinSizeY := h;
    gRC_Width := w;
    gRC_Height := h;
    gScreenWidth := Round(w / r_pixel_scale);
    gScreenHeight := Round(h / r_pixel_scale);
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
    // TODO write screenshot file
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
    // TODO implement touchscreen
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

  procedure r_Render_SetProcessLoadingCallback (p: TProcedure);
  begin
    r_Common_ProcessLoadingCallback := p;
  end;

  procedure r_Render_ClearLoading;
  begin
    r_Common_ClearLoading;
  end;

  procedure r_Render_SetLoading (const text: String; maxval: Integer);
  begin
    r_Common_SetLoading(text, maxval);
  end;

  procedure r_Render_StepLoading (incval: Integer);
  begin
    r_Common_StepLoading(incval);
  end;

  procedure r_Render_DrawLoading (force: Boolean);
  begin
    r_Common_DrawLoading(force);
  end;

{$IFDEF ENABLE_HOLMES}
  function pmsCurMapX (): Integer;
  begin
    result := r_holmes.pmsCurMapX();
  end;

  function pmsCurMapY (): Integer;
  begin
    result := r_holmes.pmsCurMapY();
  end;

  function r_Render_HolmesViewIsSet (): Boolean;
  begin
    result := vpSet;
  end;
{$ENDIF}

end.
