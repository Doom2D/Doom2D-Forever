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
unit r_game;

interface

  procedure r_Game_Draw;
  procedure r_Game_DrawLoadingStat;
  procedure r_Game_DrawMenuBackground (tex: AnsiString);

implementation

  uses
    {$INCLUDE ../nogl/noGLuses.inc}
{$IFDEF ENABLE_HOLMES}
    g_holmes,
{$ENDIF}
    SysUtils, Classes, Math,
    g_base, r_graphics,
    g_system, g_touch,
    MAPDEF, xprofiler, utils, wadreader,
    g_textures, e_input, e_sound,
    g_language, g_console, g_menu, g_triggers, g_player, g_options, g_monsters, g_map, g_panel,
    g_items, g_weapons, g_gfx, g_phys, g_net, g_gui, g_netmaster,
    g_game, r_console, r_gfx, r_items, r_map, r_panel, r_monsters, r_weapons, r_netmaster, r_player
  ;

  var
    profileFrameDraw: TProfiler = nil;

    FPS: Word;
    FPSCounter: Word;
    FPSTime: LongWord;

function GetActivePlayer_ByID(ID: Integer): TPlayer;
var
  a: Integer;
begin
  Result := nil;
  if ID < 0 then
    Exit;
  if gPlayers = nil then
    Exit;
  for a := Low(gPlayers) to High(gPlayers) do
    if IsActivePlayer(gPlayers[a]) then
    begin
      if gPlayers[a].UID <> ID then
        continue;
      Result := gPlayers[a];
      break;
    end;
end;

function calcProfilesHeight (prof: TProfiler): Integer;
begin
  result := 0;
  if (prof = nil) then exit;
  if (length(prof.bars) = 0) then exit;
  result := length(prof.bars)*(16+2);
end;

// returns width
function drawProfiles (x, y: Integer; prof: TProfiler): Integer;
var
  wdt, hgt: Integer;
  yy: Integer;
  ii: Integer;
begin
  result := 0;
  if (prof = nil) then exit;
  // gScreenWidth
  if (length(prof.bars) = 0) then exit;
  wdt := 192;
  hgt := calcProfilesHeight(prof);
  if (x < 0) then x := gScreenWidth-(wdt-1)+x;
  if (y < 0) then y := gScreenHeight-(hgt-1)+y;
  // background
  //e_DrawFillQuad(x, y, x+wdt-1, y+hgt-1, 255, 255, 255, 200, B_BLEND);
  //e_DrawFillQuad(x, y, x+wdt-1, y+hgt-1, 20, 20, 20, 0, B_NONE);
  e_DarkenQuadWH(x, y, wdt, hgt, 150);
  // title
  yy := y+2;
  for ii := 0 to High(prof.bars) do
  begin
    e_TextureFontPrintEx(x+2+4*prof.bars[ii].level, yy, Format('%s: %d', [prof.bars[ii].name, prof.bars[ii].value]), gStdFont, 255, 255, 0, 1, false);
    Inc(yy, 16+2);
  end;
  result := wdt;
end;

procedure drawTime(X, Y: Integer); inline;
begin
  e_TextureFontPrint(x, y, Format('%d:%.2d:%.2d', [gTime div 1000 div 3600, (gTime div 1000 div 60) mod 60, gTime div 1000 mod 60]), gStdFont);
end;

procedure DrawStat();
var
  pc, x, y, w, h: Integer;
  w1, w2, w3, w4: Integer;
  a, aa: Integer;
  cw, ch, r, g, b, rr, gg, bb: Byte;
  s1, s2, s3: String;
  _y: Integer;
  stat: TPlayerStatArray;
  wad, map: string;
  mapstr: string;
  namestr: string;
begin
  s1 := '';
  s2 := '';
  s3 := '';
  pc := g_Player_GetCount;
  e_TextureFontGetSize(gStdFont, cw, ch);

  w := gScreenWidth-(gScreenWidth div 5);
  if gGameSettings.GameMode in [GM_TDM, GM_CTF] then
    h := 32+ch*(11+pc)
  else
    h := 40+ch*5+(ch+8)*pc;
  x := (gScreenWidth div 2)-(w div 2);
  y := (gScreenHeight div 2)-(h div 2);

  e_DrawFillQuad(x, y, x+w-1, y+h-1, 64, 64, 64, 32);
  e_DrawQuad(x, y, x+w-1, y+h-1, 255, 127, 0);

  drawTime(x+w-78, y+8);

  wad := g_ExtractWadNameNoPath(gMapInfo.Map);
  map := g_ExtractFileName(gMapInfo.Map);
  mapstr := wad + ':\' + map + ' - ' + gMapInfo.Name;

  case gGameSettings.GameMode of
    GM_DM:
    begin
      if gGameSettings.MaxLives = 0 then
        s1 := _lc[I_GAME_DM]
      else
        s1 := _lc[I_GAME_LMS];
      s2 := Format(_lc[I_GAME_FRAG_LIMIT], [gGameSettings.ScoreLimit]);
      s3 := Format(_lc[I_GAME_TIME_LIMIT], [gGameSettings.TimeLimit div 3600, (gGameSettings.TimeLimit div 60) mod 60, gGameSettings.TimeLimit mod 60]);
    end;

    GM_TDM:
    begin
      if gGameSettings.MaxLives = 0 then
        s1 := _lc[I_GAME_TDM]
      else
        s1 := _lc[I_GAME_TLMS];
      s2 := Format(_lc[I_GAME_FRAG_LIMIT], [gGameSettings.ScoreLimit]);
      s3 := Format(_lc[I_GAME_TIME_LIMIT], [gGameSettings.TimeLimit div 3600, (gGameSettings.TimeLimit div 60) mod 60, gGameSettings.TimeLimit mod 60]);
    end;

    GM_CTF:
    begin
      s1 := _lc[I_GAME_CTF];
      s2 := Format(_lc[I_GAME_SCORE_LIMIT], [gGameSettings.ScoreLimit]);
      s3 := Format(_lc[I_GAME_TIME_LIMIT], [gGameSettings.TimeLimit div 3600, (gGameSettings.TimeLimit div 60) mod 60, gGameSettings.TimeLimit mod 60]);
    end;

    GM_COOP:
    begin
      if gGameSettings.MaxLives = 0 then
        s1 := _lc[I_GAME_COOP]
      else
        s1 := _lc[I_GAME_SURV];
      s2 := _lc[I_GAME_MONSTERS] + ' ' + IntToStr(gCoopMonstersKilled) + '/' + IntToStr(gTotalMonsters);
      s3 := _lc[I_GAME_SECRETS] + ' ' + IntToStr(gCoopSecretsFound) + '/' + IntToStr(gSecretsCount);
    end;

    else
    begin
      s1 := '';
      s2 := '';
    end;
  end;
  _y := y+8;
  e_TextureFontPrintEx(x+(w div 2)-(Length(s1)*cw div 2), _y, s1, gStdFont, 255, 255, 255, 1);
  _y := _y+ch+8;
  e_TextureFontPrintEx(x+(w div 2)-(Length(mapstr)*cw div 2), _y, mapstr, gStdFont, 200, 200, 200, 1);
  _y := _y+ch+8;
  e_TextureFontPrintEx(x+16, _y, s2, gStdFont, 200, 200, 200, 1);

  e_TextureFontPrintEx(x+w-16-(Length(s3))*cw, _y, s3,
                       gStdFont, 200, 200, 200, 1);

  if NetMode = NET_SERVER then
    e_TextureFontPrintEx(x+8, y + 8, _lc[I_NET_SERVER], gStdFont, 255, 255, 255, 1)
  else
    if NetMode = NET_CLIENT then
      e_TextureFontPrintEx(x+8, y + 8,
        NetClientIP + ':' + IntToStr(NetClientPort), gStdFont, 255, 255, 255, 1);

  if pc = 0 then
    Exit;
  stat := g_Player_GetStats();
  SortGameStat(stat);

  w2 := (w-16) div 6 + 48; // ширина 2 столбца
  w3 := (w-16) div 6; // ширина 3 и 4 столбцов
  w4 := w3;
  w1 := w-16-w2-w3-w4; // оставшееся пространство - для цвета и имени игрока

  if gGameSettings.GameMode in [GM_TDM, GM_CTF] then
  begin
    _y := _y+ch+ch;

    for a := TEAM_RED to TEAM_BLUE do
    begin
      if a = TEAM_RED then
      begin
        s1 := _lc[I_GAME_TEAM_RED];
        r := 255;
        g := 0;
        b := 0;
      end
      else
      begin
        s1 := _lc[I_GAME_TEAM_BLUE];
        r := 0;
        g := 0;
        b := 255;
      end;

      e_TextureFontPrintEx(x+16, _y, s1, gStdFont, r, g, b, 1);
      e_TextureFontPrintEx(x+w1+16, _y, IntToStr(gTeamStat[a].Score),
                           gStdFont, r, g, b, 1);

      _y := _y+ch+(ch div 4);
      e_DrawLine(1, x+16, _y, x+w-16, _y, r, g, b);
      _y := _y+(ch div 4);

      for aa := 0 to High(stat) do
        if stat[aa].Team = a then
          with stat[aa] do
          begin
            if Spectator then
            begin
              rr := r div 2;
              gg := g div 2;
              bb := b div 2;
            end
            else
            begin
              rr := r;
              gg := g;
              bb := b;
            end;
            if gShowPIDs then
              namestr := Format('[%5d] %s', [UID, Name])
            else
              namestr := Name;
            // Имя
            e_TextureFontPrintEx(x+16, _y, namestr, gStdFont, rr, gg, bb, 1);
            // Пинг/потери
            e_TextureFontPrintEx(x+w1+16, _y, Format(_lc[I_GAME_PING_MS], [Ping, Loss]), gStdFont, rr, gg, bb, 1);
            // Фраги
            e_TextureFontPrintEx(x+w1+w2+16, _y, IntToStr(Frags), gStdFont, rr, gg, bb, 1);
            // Смерти
            e_TextureFontPrintEx(x+w1+w2+w3+16, _y, IntToStr(Deaths), gStdFont, rr, gg, bb, 1);
            _y := _y+ch;
          end;

          _y := _y+ch;
    end;
  end
  else if gGameSettings.GameMode in [GM_DM, GM_COOP] then
  begin
    _y := _y+ch+ch;
    e_TextureFontPrintEx(x+16, _y, _lc[I_GAME_PLAYER_NAME], gStdFont, 255, 127, 0, 1);
    e_TextureFontPrintEx(x+16+w1, _y, _lc[I_GAME_PING], gStdFont, 255, 127, 0, 1);
    e_TextureFontPrintEx(x+16+w1+w2, _y, _lc[I_GAME_FRAGS], gStdFont, 255, 127, 0, 1);
    e_TextureFontPrintEx(x+16+w1+w2+w3, _y, _lc[I_GAME_DEATHS], gStdFont, 255, 127, 0, 1);

    _y := _y+ch+8;
    for aa := 0 to High(stat) do
      with stat[aa] do
      begin
        if Spectator then
        begin
          r := 127;
          g := 64;
        end
        else
        begin
          r := 255;
          g := 127;
        end;
        if gShowPIDs then
          namestr := Format('[%5d] %s', [UID, Name])
        else
          namestr := Name;
        // Цвет игрока
        e_DrawFillQuad(x+16, _y+4, x+32-1, _y+16+4-1, Color.R, Color.G, Color.B, 0);
        e_DrawQuad(x+16, _y+4, x+32-1, _y+16+4-1, 192, 192, 192);
        // Имя
        e_TextureFontPrintEx(x+16+16+8, _y+4, namestr, gStdFont, r, g, 0, 1);
        // Пинг/потери
        e_TextureFontPrintEx(x+w1+16, _y+4, Format(_lc[I_GAME_PING_MS], [Ping, Loss]), gStdFont, r, g, 0, 1);
        // Фраги
        e_TextureFontPrintEx(x+w1+w2+16, _y+4, IntToStr(Frags), gStdFont, r, g, 0, 1);
        // Смерти
        e_TextureFontPrintEx(x+w1+w2+w3+16, _y+4, IntToStr(Deaths), gStdFont, r, g, 0, 1);
        _y := _y+ch+8;
      end;
  end
end;

procedure DrawCustomStat();
var
  pc, x, y, w, _y,
  w1, w2, w3,
  t, p, m: Integer;
  ww1, hh1: Word;
  ww2, hh2, r, g, b, rr, gg, bb: Byte;
  s1, s2, topstr: String;
begin
  e_TextureFontGetSize(gStdFont, ww2, hh2);

  sys_HandleInput;

  if g_Console_Action(ACTION_SCORES) then
  begin
    if not gStatsPressed then
    begin
      gStatsOff := not gStatsOff;
      gStatsPressed := True;
    end;
  end
  else
    gStatsPressed := False;

  if gStatsOff then
  begin
    s1 := _lc[I_MENU_INTER_NOTICE_TAB];
    w := (Length(s1) * ww2) div 2;
    x := gScreenWidth div 2 - w;
    y := 8;
    e_TextureFontPrint(x, y, s1, gStdFont);
    Exit;
  end;

  if (gGameSettings.GameMode = GM_COOP) then
  begin
    if gMissionFailed then
      topstr := _lc[I_MENU_INTER_MISSION_FAIL]
    else
      topstr := _lc[I_MENU_INTER_LEVEL_COMPLETE];
  end
  else
    topstr := _lc[I_MENU_INTER_ROUND_OVER];

  e_CharFont_GetSize(gMenuFont, topstr, ww1, hh1);
  e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(ww1 div 2), 16, topstr);

  if g_Game_IsNet then
  begin
    topstr := Format(_lc[I_MENU_INTER_NOTICE_TIME], [gServInterTime]);
    if not gChatShow then
      e_TextureFontPrintEx((gScreenWidth div 2)-(Length(topstr)*ww2 div 2),
                           gScreenHeight-(hh2+4)*2, topstr, gStdFont, 255, 255, 255, 1);
  end;

  if g_Game_IsClient then
    topstr := _lc[I_MENU_INTER_NOTICE_MAP]
  else
    topstr := _lc[I_MENU_INTER_NOTICE_SPACE];
  if not gChatShow then
    e_TextureFontPrintEx((gScreenWidth div 2)-(Length(topstr)*ww2 div 2),
                         gScreenHeight-(hh2+4), topstr, gStdFont, 255, 255, 255, 1);

  x := 32;
  y := 16+hh1+16;

  w := gScreenWidth-x*2;

  w2 := (w-16) div 6;
  w3 := w2;
  w1 := w-16-w2-w3;

  e_DrawFillQuad(x, y, gScreenWidth-x-1, gScreenHeight-y-1, 64, 64, 64, 32);
  e_DrawQuad(x, y, gScreenWidth-x-1, gScreenHeight-y-1, 255, 127, 0);

  m := Max(Length(_lc[I_MENU_MAP])+1, Length(_lc[I_GAME_GAME_TIME])+1)*ww2;

  case CustomStat.GameMode of
    GM_DM:
    begin
      if gGameSettings.MaxLives = 0 then
        s1 := _lc[I_GAME_DM]
      else
        s1 := _lc[I_GAME_LMS];
    end;
    GM_TDM:
    begin
      if gGameSettings.MaxLives = 0 then
        s1 := _lc[I_GAME_TDM]
      else
        s1 := _lc[I_GAME_TLMS];
    end;
    GM_CTF: s1 := _lc[I_GAME_CTF];
    GM_COOP:
    begin
      if gGameSettings.MaxLives = 0 then
        s1 := _lc[I_GAME_COOP]
      else
        s1 := _lc[I_GAME_SURV];
    end;
    else s1 := '';
  end;

  _y := y+16;
  e_TextureFontPrintEx(x+(w div 2)-(Length(s1)*ww2 div 2), _y, s1, gStdFont, 255, 255, 255, 1);
  _y := _y+8;

  _y := _y+16;
  e_TextureFontPrintEx(x+8, _y, _lc[I_MENU_MAP], gStdFont, 255, 127, 0, 1);
  e_TextureFontPrint(x+8+m, _y, Format('%s - %s', [CustomStat.Map, CustomStat.MapName]), gStdFont);

  _y := _y+16;
  e_TextureFontPrintEx(x+8, _y, _lc[I_GAME_GAME_TIME], gStdFont, 255, 127, 0, 1);
  e_TextureFontPrint(x+8+m, _y, Format('%d:%.2d:%.2d', [CustomStat.GameTime div 1000 div 3600,
                                                       (CustomStat.GameTime div 1000 div 60) mod 60,
                                                        CustomStat.GameTime div 1000 mod 60]), gStdFont);

  pc := Length(CustomStat.PlayerStat);
  if pc = 0 then Exit;

  if CustomStat.GameMode = GM_COOP then
  begin
    m := Max(Length(_lc[I_GAME_MONSTERS])+1, Length(_lc[I_GAME_SECRETS])+1)*ww2;
    _y := _y+32;
    s2 := _lc[I_GAME_MONSTERS];
    e_TextureFontPrintEx(x+8, _y, s2, gStdFont, 255, 127, 0, 1);
    e_TextureFontPrintEx(x+8+m, _y, IntToStr(gCoopMonstersKilled) + '/' + IntToStr(gTotalMonsters), gStdFont, 255, 255, 255, 1);
    _y := _y+16;
    s2 := _lc[I_GAME_SECRETS];
    e_TextureFontPrintEx(x+8, _y, s2, gStdFont, 255, 127, 0, 1);
    e_TextureFontPrintEx(x+8+m, _y, IntToStr(gCoopSecretsFound) + '/' + IntToStr(gSecretsCount), gStdFont, 255, 255, 255, 1);
    if gLastMap then
    begin
      m := Max(Length(_lc[I_GAME_MONSTERS_TOTAL])+1, Length(_lc[I_GAME_SECRETS_TOTAL])+1)*ww2;
      _y := _y-16;
      s2 := _lc[I_GAME_MONSTERS_TOTAL];
      e_TextureFontPrintEx(x+250, _y, s2, gStdFont, 255, 127, 0, 1);
      e_TextureFontPrintEx(x+250+m, _y, IntToStr(gCoopTotalMonstersKilled) + '/' + IntToStr(gCoopTotalMonsters), gStdFont, 255, 255, 255, 1);
      _y := _y+16;
      s2 := _lc[I_GAME_SECRETS_TOTAL];
      e_TextureFontPrintEx(x+250, _y, s2, gStdFont, 255, 127, 0, 1);
      e_TextureFontPrintEx(x+250+m, _y, IntToStr(gCoopTotalSecretsFound) + '/' + IntToStr(gCoopTotalSecrets), gStdFont, 255, 255,  255, 1);
    end;
  end;

  if CustomStat.GameMode in [GM_TDM, GM_CTF] then
  begin
    _y := _y+16+16;

    with CustomStat do
      if TeamStat[TEAM_RED].Score > TeamStat[TEAM_BLUE].Score then s1 := _lc[I_GAME_WIN_RED]
        else if TeamStat[TEAM_BLUE].Score > TeamStat[TEAM_RED].Score then s1 := _lc[I_GAME_WIN_BLUE]
          else s1 := _lc[I_GAME_WIN_DRAW];

    e_TextureFontPrintEx(x+8+(w div 2)-(Length(s1)*ww2 div 2), _y, s1, gStdFont, 255, 255, 255, 1);
    _y := _y+40;

    for t := TEAM_RED to TEAM_BLUE do
    begin
      if t = TEAM_RED then
      begin
        e_TextureFontPrintEx(x+8, _y, _lc[I_GAME_TEAM_RED],
                             gStdFont, 255, 0, 0, 1);
        e_TextureFontPrintEx(x+w1+8, _y, IntToStr(CustomStat.TeamStat[TEAM_RED].Score),
                             gStdFont, 255, 0, 0, 1);
        r := 255;
        g := 0;
        b := 0;
      end
      else
      begin
        e_TextureFontPrintEx(x+8, _y, _lc[I_GAME_TEAM_BLUE],
                             gStdFont, 0, 0, 255, 1);
        e_TextureFontPrintEx(x+w1+8, _y, IntToStr(CustomStat.TeamStat[TEAM_BLUE].Score),
                             gStdFont, 0, 0, 255, 1);
        r := 0;
        g := 0;
        b := 255;
      end;

      e_DrawLine(1, x+8, _y+20, x-8+w, _y+20, r, g, b);
      _y := _y+24;

      for p := 0 to High(CustomStat.PlayerStat) do
        if CustomStat.PlayerStat[p].Team = t then
          with CustomStat.PlayerStat[p] do
          begin
            if Spectator then
            begin
              rr := r div 2;
              gg := g div 2;
              bb := b div 2;
            end
            else
            begin
              rr := r;
              gg := g;
              bb := b;
            end;
            if (gPlayers[Num] <> nil) and (gPlayers[Num].FReady) then
              e_TextureFontPrintEx(x+16, _y, Name + ' *', gStdFont, rr, gg, bb, 1)
            else
              e_TextureFontPrintEx(x+16, _y, Name, gStdFont, rr, gg, bb, 1);
            e_TextureFontPrintEx(x+w1+16, _y, IntToStr(Frags), gStdFont, rr, gg, bb, 1);
            e_TextureFontPrintEx(x+w1+w2+16, _y, IntToStr(Deaths), gStdFont, rr, gg, bb, 1);
            _y := _y+24;
          end;

      _y := _y+16+16;
    end;
  end
  else if CustomStat.GameMode in [GM_DM, GM_COOP] then
  begin
    _y := _y+40;
    e_TextureFontPrintEx(x+8, _y, _lc[I_GAME_PLAYER_NAME], gStdFont, 255, 127, 0, 1);
    e_TextureFontPrintEx(x+8+w1, _y, _lc[I_GAME_FRAGS], gStdFont, 255, 127, 0, 1);
    e_TextureFontPrintEx(x+8+w1+w2, _y, _lc[I_GAME_DEATHS], gStdFont, 255, 127, 0, 1);

    _y := _y+24;
    for p := 0 to High(CustomStat.PlayerStat) do
      with CustomStat.PlayerStat[p] do
      begin
        e_DrawFillQuad(x+8, _y+4, x+24-1, _y+16+4-1, Color.R, Color.G, Color.B, 0);

        if Spectator then
          r := 127
        else
          r := 255;

        if (gPlayers[Num] <> nil) and (gPlayers[Num].FReady) then
          e_TextureFontPrintEx(x+8+16+8, _y+4, Name + ' *', gStdFont, r, r, r, 1, True)
        else
          e_TextureFontPrintEx(x+8+16+8, _y+4, Name, gStdFont, r, r, r, 1, True);
        e_TextureFontPrintEx(x+w1+8+16+8, _y+4, IntToStr(Frags), gStdFont, r, r, r, 1, True);
        e_TextureFontPrintEx(x+w1+w2+8+16+8, _y+4, IntToStr(Deaths), gStdFont, r, r, r, 1, True);
        _y := _y+24;
      end;
  end;

  // HACK: take stats screenshot immediately after the first frame of the stats showing
  if gScreenshotStats and (not StatShotDone) and (Length(CustomStat.PlayerStat) > 1) then
  begin
    g_TakeScreenShot('stats/' + StatFilename);
    StatShotDone := True;
  end;
end;

procedure DrawSingleStat();
var
  tm, key_x, val_x, y: Integer;
  w1, w2, h: Word;
  s1, s2: String;

  procedure player_stat(n: Integer);
  var
    kpm: Real;

  begin
  // "Kills: # / #":
    s1 := Format(' %d ', [SingleStat.PlayerStat[n].Kills]);
    s2 := Format(' %d', [gTotalMonsters]);

    e_CharFont_Print(gMenuFont, key_x, y, _lc[I_MENU_INTER_KILLS]);
    e_CharFont_PrintEx(gMenuFont, val_x, y, s1, _RGB(255, 0, 0));
    e_CharFont_GetSize(gMenuFont, s1, w1, h);
    e_CharFont_Print(gMenuFont, val_x+w1, y, '/');
    s1 := s1 + '/';
    e_CharFont_GetSize(gMenuFont, s1, w1, h);
    e_CharFont_PrintEx(gMenuFont, val_x+w1, y, s2, _RGB(255, 0, 0));

  // "Kills-per-minute: ##.#":
    s1 := _lc[I_MENU_INTER_KPM];
    if tm > 0 then
      kpm := (SingleStat.PlayerStat[n].Kills / tm) * 60
    else
      kpm := SingleStat.PlayerStat[n].Kills;
    s2 := Format(' %.1f', [kpm]);

    e_CharFont_Print(gMenuFont, key_x, y+32, s1);
    e_CharFont_PrintEx(gMenuFont, val_x, y+32, s2, _RGB(255, 0, 0));

  // "Secrets found: # / #":
    s1 := Format(' %d ', [SingleStat.PlayerStat[n].Secrets]);
    s2 := Format(' %d', [SingleStat.TotalSecrets]);

    e_CharFont_Print(gMenuFont, key_x, y+64, _lc[I_MENU_INTER_SECRETS]);
    e_CharFont_PrintEx(gMenuFont, val_x, y+64, s1, _RGB(255, 0, 0));
    e_CharFont_GetSize(gMenuFont, s1, w1, h);
    e_CharFont_Print(gMenuFont, val_x+w1, y+64, '/');
    s1 := s1 + '/';
    e_CharFont_GetSize(gMenuFont, s1, w1, h);
    e_CharFont_PrintEx(gMenuFont, val_x+w1, y+64, s2, _RGB(255, 0, 0));
  end;

begin
// "Level Complete":
  e_CharFont_GetSize(gMenuFont, _lc[I_MENU_INTER_LEVEL_COMPLETE], w1, h);
  e_CharFont_Print(gMenuFont, (gScreenWidth-w1) div 2, 32, _lc[I_MENU_INTER_LEVEL_COMPLETE]);

// Определяем координаты выравнивания по самой длинной строке:
  s1 := _lc[I_MENU_INTER_KPM];
  e_CharFont_GetSize(gMenuFont, s1, w1, h);
  Inc(w1, 16);
  s1 := ' 9999.9';
  e_CharFont_GetSize(gMenuFont, s1, w2, h);

  key_x := (gScreenWidth-w1-w2) div 2;
  val_x := key_x + w1;

// "Time: #:##:##":
  tm := SingleStat.GameTime div 1000;
  s1 := _lc[I_MENU_INTER_TIME];
  s2 := Format(' %d:%.2d:%.2d', [tm div (60*60), (tm mod (60*60)) div 60, tm mod 60]);

  e_CharFont_Print(gMenuFont, key_x, 80, s1);
  e_CharFont_PrintEx(gMenuFont, val_x, 80, s2, _RGB(255, 0, 0));

  if SingleStat.TwoPlayers then
    begin
    // "Player 1":
      s1 := _lc[I_MENU_PLAYER_1];
      e_CharFont_GetSize(gMenuFont, s1, w1, h);
      e_CharFont_Print(gMenuFont, (gScreenWidth-w1) div 2, 128, s1);

    // Статистика первого игрока:
      y := 176;
      player_stat(0);

    // "Player 2":
      s1 := _lc[I_MENU_PLAYER_2];
      e_CharFont_GetSize(gMenuFont, s1, w1, h);
      e_CharFont_Print(gMenuFont, (gScreenWidth-w1) div 2, 288, s1);

    // Статистика второго игрока:
      y := 336;
      player_stat(1);
    end
  else
    begin
    // Статистика первого игрока:
      y := 128;
      player_stat(0);
    end;
end;

procedure r_Game_DrawLoadingStat;
  procedure drawRect (x, y, w, h: Integer);
  begin
    if (w < 1) or (h < 1) then exit;
    glBegin(GL_QUADS);
      glVertex2f(x+0.375, y+0.375);
      glVertex2f(x+w+0.375, y+0.375);
      glVertex2f(x+w+0.375, y+h+0.375);
      glVertex2f(x+0.375, y+h+0.375);
    glEnd();
  end;

  function drawPBar (cur, total: Integer; washere: Boolean): Boolean;
  var
    rectW, rectH: Integer;
    x0, y0: Integer;
    wdt: Integer;
    wl, hl: Integer;
    wr, hr: Integer;
    wb, hb: Integer;
    wm, hm: Integer;
    idl, idr, idb, idm: LongWord;
    f, my: Integer;
  begin
    result := false;
    if (total < 1) then exit;
    if (cur < 1) then exit; // don't blink
    if (not washere) and (cur >= total) then exit; // don't blink
    //if (cur < 0) then cur := 0;
    //if (cur > total) then cur := total;
    result := true;

    if (hasPBarGfx) then
    begin
      g_Texture_Get('UI_GFX_PBAR_LEFT', idl);
      g_Texture_GetSize('UI_GFX_PBAR_LEFT', wl, hl);
      g_Texture_Get('UI_GFX_PBAR_RIGHT', idr);
      g_Texture_GetSize('UI_GFX_PBAR_RIGHT', wr, hr);
      g_Texture_Get('UI_GFX_PBAR_MIDDLE', idb);
      g_Texture_GetSize('UI_GFX_PBAR_MIDDLE', wb, hb);
      g_Texture_Get('UI_GFX_PBAR_MARKER', idm);
      g_Texture_GetSize('UI_GFX_PBAR_MARKER', wm, hm);

      //rectW := gScreenWidth-360;
      rectW := trunc(624.0*gScreenWidth/1024.0);
      rectH := hl;

      x0 := (gScreenWidth-rectW) div 2;
      y0 := gScreenHeight-rectH-64;
      if (y0 < 2) then y0 := 2;

      glEnable(GL_SCISSOR_TEST);

      // left and right
      glScissor(x0, gScreenHeight-y0-rectH, rectW, rectH);
      e_DrawSize(idl, x0, y0, 0, true, false, wl, hl);
      e_DrawSize(idr, x0+rectW-wr, y0, 0, true, false, wr, hr);

      // body
      glScissor(x0+wl, gScreenHeight-y0-rectH, rectW-wl-wr, rectH);
      f := x0+wl;
      while (f < x0+rectW) do
      begin
        e_DrawSize(idb, f, y0, 0, true, false, wb, hb);
        f += wb;
      end;

      // filled part
      wdt := (rectW-wl-wr)*cur div total;
      if (wdt > rectW-wl-wr) then wdt := rectW-wr-wr;
      if (wdt > 0) then
      begin
        my := y0; // don't be so smart, ketmar: +(rectH-wm) div 2;
        glScissor(x0+wl, gScreenHeight-my-rectH, wdt, hm);
        f := x0+wl;
        while (wdt > 0) do
        begin
          e_DrawSize(idm, f, y0, 0, true, false, wm, hm);
          f += wm;
          wdt -= wm;
        end;
      end;

      glScissor(0, 0, gScreenWidth, gScreenHeight);
    end
    else
    begin
      rectW := gScreenWidth-64;
      rectH := 16;

      x0 := (gScreenWidth-rectW) div 2;
      y0 := gScreenHeight-rectH-64;
      if (y0 < 2) then y0 := 2;

      glDisable(GL_BLEND);
      glDisable(GL_TEXTURE_2D);

      //glClearColor(0, 0, 0, 0);
      //glClear(GL_COLOR_BUFFER_BIT);

      glColor4ub(127, 127, 127, 255);
      drawRect(x0-2, y0-2, rectW+4, rectH+4);

      glColor4ub(0, 0, 0, 255);
      drawRect(x0-1, y0-1, rectW+2, rectH+2);

      glColor4ub(127, 127, 127, 255);
      wdt := rectW*cur div total;
      if (wdt > rectW) then wdt := rectW;
      drawRect(x0, y0, wdt, rectH);
    end;
  end;

var
  ww, hh: Word;
  xx, yy, i: Integer;
  s: String;
begin
  if (Length(LoadingStat.Msgs) = 0) then exit;

  e_CharFont_GetSize(gMenuFont, _lc[I_MENU_LOADING], ww, hh);
  yy := (gScreenHeight div 3);
  e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(ww div 2), yy-2*hh, _lc[I_MENU_LOADING]);
  xx := (gScreenWidth div 3);

  with LoadingStat do
  begin
    for i := 0 to NextMsg-1 do
    begin
      if (i = (NextMsg-1)) and (MaxValue > 0) then
        s := Format('%s:  %d/%d', [Msgs[i], CurValue, MaxValue])
      else
        s := Msgs[i];

      e_CharFont_PrintEx(gMenuSmallFont, xx, yy, s, _RGB(255, 0, 0));
      yy := yy + LOADING_INTERLINE;
      PBarWasHere := drawPBar(CurValue, MaxValue, PBarWasHere);
    end;
  end;
end;

procedure r_Game_DrawMenuBackground (tex: AnsiString);
var
  w, h: Word;
  ID: DWord;

begin
  if g_Texture_Get(tex, ID) then
  begin
    e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);
    e_GetTextureSize(ID, @w, @h);
    if w = h then
      w := round(w * 1.333 * (gScreenHeight / h))
    else
      w := trunc(w * (gScreenHeight / h));
    e_DrawSize(ID, (gScreenWidth - w) div 2, 0, 0, False, False, w, gScreenHeight);
  end
  else e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);
end;

procedure DrawMinimap(p: TPlayer; RenderRect: TRect);
var
  a, aX, aY, aX2, aY2, Scale, ScaleSz: Integer;

  function monDraw (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    with mon do
    begin
      if alive then
      begin
        // Левый верхний угол
        aX := Obj.X div ScaleSz + 1;
        aY := Obj.Y div ScaleSz + 1;
        // Размеры
        aX2 := max(Obj.Rect.Width div ScaleSz, 1);
        aY2 := max(Obj.Rect.Height div ScaleSz, 1);
        // Правый нижний угол
        aX2 := aX + aX2 - 1;
        aY2 := aY + aY2 - 1;
        e_DrawFillQuad(aX, aY, aX2, aY2, 255, 255, 0, 0);
      end;
    end;
  end;

begin
  if (gMapInfo.Width > RenderRect.Right - RenderRect.Left) or
     (gMapInfo.Height > RenderRect.Bottom - RenderRect.Top) then
  begin
    Scale := 1;
  // Сколько пикселов карты в 1 пикселе мини-карты:
    ScaleSz := 16 div Scale;
  // Размеры мини-карты:
    aX := max(gMapInfo.Width div ScaleSz, 1);
    aY := max(gMapInfo.Height div ScaleSz, 1);
  // Рамка карты:
    e_DrawFillQuad(0, 0, aX-1, aY-1, 0, 0, 0, 0);

    if gWalls <> nil then
    begin
    // Рисуем стены:
      for a := 0 to High(gWalls) do
        with gWalls[a] do
          if PanelType <> 0 then
          begin
          // Левый верхний угол:
            aX := X div ScaleSz;
            aY := Y div ScaleSz;
          // Размеры:
            aX2 := max(Width div ScaleSz, 1);
            aY2 := max(Height div ScaleSz, 1);
          // Правый нижний угол:
            aX2 := aX + aX2 - 1;
            aY2 := aY + aY2 - 1;

            case PanelType of
              PANEL_WALL:      e_DrawFillQuad(aX, aY, aX2, aY2, 208, 208, 208, 0);
              PANEL_OPENDOOR, PANEL_CLOSEDOOR:
                if Enabled then e_DrawFillQuad(aX, aY, aX2, aY2, 160, 160, 160, 0);
            end;
          end;
    end;
    if gSteps <> nil then
    begin
    // Рисуем ступени:
      for a := 0 to High(gSteps) do
        with gSteps[a] do
          if PanelType <> 0 then
          begin
          // Левый верхний угол:
            aX := X div ScaleSz;
            aY := Y div ScaleSz;
          // Размеры:
            aX2 := max(Width div ScaleSz, 1);
            aY2 := max(Height div ScaleSz, 1);
          // Правый нижний угол:
            aX2 := aX + aX2 - 1;
            aY2 := aY + aY2 - 1;

            e_DrawFillQuad(aX, aY, aX2, aY2, 128, 128, 128, 0);
          end;
    end;
    if gLifts <> nil then
    begin
    // Рисуем лифты:
      for a := 0 to High(gLifts) do
        with gLifts[a] do
          if PanelType <> 0 then
          begin
          // Левый верхний угол:
            aX := X div ScaleSz;
            aY := Y div ScaleSz;
          // Размеры:
            aX2 := max(Width div ScaleSz, 1);
            aY2 := max(Height div ScaleSz, 1);
          // Правый нижний угол:
            aX2 := aX + aX2 - 1;
            aY2 := aY + aY2 - 1;

            case LiftType of
              LIFTTYPE_UP:    e_DrawFillQuad(aX, aY, aX2, aY2, 116,  72,  36, 0);
              LIFTTYPE_DOWN:  e_DrawFillQuad(aX, aY, aX2, aY2, 116, 124,  96, 0);
              LIFTTYPE_LEFT:  e_DrawFillQuad(aX, aY, aX2, aY2, 200,  80,   4, 0);
              LIFTTYPE_RIGHT: e_DrawFillQuad(aX, aY, aX2, aY2, 252, 140,  56, 0);
            end;
          end;
    end;
    if gWater <> nil then
    begin
    // Рисуем воду:
      for a := 0 to High(gWater) do
        with gWater[a] do
          if PanelType <> 0 then
          begin
          // Левый верхний угол:
            aX := X div ScaleSz;
            aY := Y div ScaleSz;
          // Размеры:
            aX2 := max(Width div ScaleSz, 1);
            aY2 := max(Height div ScaleSz, 1);
          // Правый нижний угол:
            aX2 := aX + aX2 - 1;
            aY2 := aY + aY2 - 1;

            e_DrawFillQuad(aX, aY, aX2, aY2, 0, 0, 192, 0);
          end;
    end;
    if gAcid1 <> nil then
    begin
    // Рисуем кислоту 1:
      for a := 0 to High(gAcid1) do
        with gAcid1[a] do
          if PanelType <> 0 then
          begin
          // Левый верхний угол:
            aX := X div ScaleSz;
            aY := Y div ScaleSz;
          // Размеры:
            aX2 := max(Width div ScaleSz, 1);
            aY2 := max(Height div ScaleSz, 1);
          // Правый нижний угол:
            aX2 := aX + aX2 - 1;
            aY2 := aY + aY2 - 1;

            e_DrawFillQuad(aX, aY, aX2, aY2, 0, 176, 0, 0);
          end;
    end;
    if gAcid2 <> nil then
    begin
    // Рисуем кислоту 2:
      for a := 0 to High(gAcid2) do
        with gAcid2[a] do
          if PanelType <> 0 then
          begin
          // Левый верхний угол:
            aX := X div ScaleSz;
            aY := Y div ScaleSz;
          // Размеры:
            aX2 := max(Width div ScaleSz, 1);
            aY2 := max(Height div ScaleSz, 1);
          // Правый нижний угол:
            aX2 := aX + aX2 - 1;
            aY2 := aY + aY2 - 1;

            e_DrawFillQuad(aX, aY, aX2, aY2, 176, 0, 0, 0);
          end;
    end;
    if gPlayers <> nil then
    begin
    // Рисуем игроков:
      for a := 0 to High(gPlayers) do
        if gPlayers[a] <> nil then with gPlayers[a] do
          if alive then begin
          // Левый верхний угол:
            aX := Obj.X div ScaleSz + 1;
            aY := Obj.Y div ScaleSz + 1;
          // Размеры:
            aX2 := max(Obj.Rect.Width div ScaleSz, 1);
            aY2 := max(Obj.Rect.Height div ScaleSz, 1);
          // Правый нижний угол:
            aX2 := aX + aX2 - 1;
            aY2 := aY + aY2 - 1;

            if gPlayers[a] = p then
              e_DrawFillQuad(aX, aY, aX2, aY2, 0, 255, 0, 0)
            else
              case Team of
                TEAM_RED:  e_DrawFillQuad(aX, aY, aX2, aY2, 255,   0,   0, 0);
                TEAM_BLUE: e_DrawFillQuad(aX, aY, aX2, aY2, 0,     0, 255, 0);
                else       e_DrawFillQuad(aX, aY, aX2, aY2, 255, 128,   0, 0);
              end;
          end;
    end;
    // Рисуем монстров
    g_Mons_ForEach(monDraw);
  end;
end;


procedure renderAmbientQuad (hasAmbient: Boolean; constref ambColor: TDFColor);
begin
  if not hasAmbient then exit;
  e_AmbientQuad(sX, sY, sWidth, sHeight, ambColor.r, ambColor.g, ambColor.b, ambColor.a);
end;

// ////////////////////////////////////////////////////////////////////////// //
var
  ltexid: GLuint = 0;

function g_Texture_Light (): Integer;
const
  Radius: Integer = 128;
var
  tex, tpp: PByte;
  x, y, a: Integer;
  dist: Double;
begin
  if ltexid = 0 then
  begin
    GetMem(tex, (Radius*2)*(Radius*2)*4);
    tpp := tex;
    for y := 0 to Radius*2-1 do
    begin
      for x := 0 to Radius*2-1 do
      begin
        dist := 1.0-sqrt((x-Radius)*(x-Radius)+(y-Radius)*(y-Radius))/Radius;
        if (dist < 0) then
        begin
          tpp^ := 0; Inc(tpp);
          tpp^ := 0; Inc(tpp);
          tpp^ := 0; Inc(tpp);
          tpp^ := 0; Inc(tpp);
        end
        else
        begin
          //tc.setPixel(x, y, Color(cast(int)(dist*255), cast(int)(dist*255), cast(int)(dist*255)));
          if (dist > 0.5) then dist := 0.5;
          a := round(dist*255);
          if (a < 0) then a := 0 else if (a > 255) then a := 255;
          tpp^ := 255; Inc(tpp);
          tpp^ := 255; Inc(tpp);
          tpp^ := 255; Inc(tpp);
          tpp^ := Byte(a); Inc(tpp);
        end;
      end;
    end;

    glGenTextures(1, @ltexid);
    //if (tid == 0) assert(0, "VGL: can't create screen texture");

    glBindTexture(GL_TEXTURE_2D, ltexid);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    //GLfloat[4] bclr = 0.0;
    //glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, bclr.ptr);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Radius*2, Radius*2, 0, GL_RGBA{gltt}, GL_UNSIGNED_BYTE, tex);
  end;

  result := ltexid;
end;

// setup sX, sY, sWidth, sHeight, and transformation matrix before calling this!
//FIXME: broken for splitscreen mode
procedure renderDynLightsInternal ();
var
  //hasAmbient: Boolean;
  //ambColor: TDFColor;
  lln: Integer;
  lx, ly, lrad: Integer;
  scxywh: array[0..3] of GLint;
  wassc: Boolean;
begin
  if e_NoGraphics then exit;

  //TODO: lights should be in separate grid, i think
  //      but on the other side: grid may be slower for dynlights, as their lifetime is short
  if (not gwin_k8_enable_light_experiments) or (not gwin_has_stencil) or (g_dynLightCount < 1) then exit;

  // rendering mode
  //ambColor := gCurrentMap['light_ambient'].rgba;
  //hasAmbient := (not ambColor.isOpaque) or (not ambColor.isBlack);

  { // this will multiply incoming color to alpha from framebuffer
    glEnable(GL_BLEND);
    glBlendFunc(GL_DST_ALPHA, GL_ONE);
  }

  (*
   * light rendering: (INVALID!)
   *   glStencilFunc(GL_EQUAL, 0, $ff);
   *   for each light:
   *     glClear(GL_STENCIL_BUFFER_BIT);
   *     glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);
   *     draw shadow volume into stencil buffer
   *     glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE); // modify color buffer
   *     glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP); // don't modify stencil buffer
   *     turn off blending
   *     draw color-less quad with light alpha (WARNING! don't touch color!)
   *     glEnable(GL_BLEND);
   *     glBlendFunc(GL_DST_ALPHA, GL_ONE);
   *     draw all geometry up to and including walls (with alpha-testing, probably) -- this does lighting
   *)
  wassc := (glIsEnabled(GL_SCISSOR_TEST) <> 0);
  if wassc then glGetIntegerv(GL_SCISSOR_BOX, @scxywh[0]) else glGetIntegerv(GL_VIEWPORT, @scxywh[0]);

  // setup OpenGL parameters
  glStencilMask($FFFFFFFF);
  glStencilFunc(GL_ALWAYS, 0, $FFFFFFFF);
  glEnable(GL_STENCIL_TEST);
  glEnable(GL_SCISSOR_TEST);
  glClear(GL_STENCIL_BUFFER_BIT);
  glStencilFunc(GL_EQUAL, 0, $ff);

  for lln := 0 to g_dynLightCount-1 do
  begin
    lx := g_dynLights[lln].x;
    ly := g_dynLights[lln].y;
    lrad := g_dynLights[lln].radius;
    if (lrad < 3) then continue;

    if (lx-sX+lrad < 0) then continue;
    if (ly-sY+lrad < 0) then continue;
    if (lx-sX-lrad >= gPlayerScreenSize.X) then continue;
    if (ly-sY-lrad >= gPlayerScreenSize.Y) then continue;

    // set scissor to optimize drawing
    if (g_dbg_scale = 1.0) then
    begin
      glScissor((lx-sX)-lrad+2, gPlayerScreenSize.Y-(ly-sY)-lrad-1+2, lrad*2-4, lrad*2-4);
    end
    else
    begin
      glScissor(0, 0, gScreenWidth, gScreenHeight);
    end;
    // no need to clear stencil buffer, light blitting will do it for us... but only for normal scale
    if (g_dbg_scale <> 1.0) then glClear(GL_STENCIL_BUFFER_BIT);
    glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);
    // draw extruded panels
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
    glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE); // no need to modify color buffer
    if (lrad > 4) then r_Map_DrawPanelShadowVolumes(lx, ly, lrad);
    // render light texture
    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE); // modify color buffer
    glStencilOp(GL_ZERO, GL_ZERO, GL_ZERO); // draw light, and clear stencil buffer
    // blend it
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_TEXTURE_2D);
    // color and opacity
    glColor4f(g_dynLights[lln].r, g_dynLights[lln].g, g_dynLights[lln].b, g_dynLights[lln].a);
    glBindTexture(GL_TEXTURE_2D, g_Texture_Light());
    glBegin(GL_QUADS);
      glTexCoord2f(0.0, 0.0); glVertex2i(lx-lrad, ly-lrad); // top-left
      glTexCoord2f(1.0, 0.0); glVertex2i(lx+lrad, ly-lrad); // top-right
      glTexCoord2f(1.0, 1.0); glVertex2i(lx+lrad, ly+lrad); // bottom-right
      glTexCoord2f(0.0, 1.0); glVertex2i(lx-lrad, ly+lrad); // bottom-left
    glEnd();
  end;

  // done
  glDisable(GL_STENCIL_TEST);
  glDisable(GL_BLEND);
  glDisable(GL_SCISSOR_TEST);
  //glScissor(0, 0, sWidth, sHeight);

  glScissor(scxywh[0], scxywh[1], scxywh[2], scxywh[3]);
  if wassc then glEnable(GL_SCISSOR_TEST) else glDisable(GL_SCISSOR_TEST);
end;


function fixViewportForScale (): Boolean;
var
  nx0, ny0, nw, nh: Integer;
begin
  result := false;
  if (g_dbg_scale <> 1.0) then
  begin
    result := true;
    nx0 := round(sX-(gPlayerScreenSize.X-(sWidth*g_dbg_scale))/2/g_dbg_scale);
    ny0 := round(sY-(gPlayerScreenSize.Y-(sHeight*g_dbg_scale))/2/g_dbg_scale);
    nw := round(sWidth/g_dbg_scale);
    nh := round(sHeight/g_dbg_scale);
    sX := nx0;
    sY := ny0;
    sWidth := nw;
    sHeight := nh;
  end;
end;


// setup sX, sY, sWidth, sHeight, and transformation matrix before calling this!
// WARNING! this WILL CALL `glTranslatef()`, but won't restore matrices!
procedure renderMapInternal (backXOfs, backYOfs: Integer; setTransMatrix: Boolean);
type
  TDrawCB = procedure ();

var
  hasAmbient: Boolean;
  ambColor: TDFColor;
  doAmbient: Boolean = false;

  procedure drawPanelType (profname: AnsiString; panType: DWord; doDraw: Boolean);
  var
    tagmask: Integer;
    pan: TPanel;
  begin
    if (profileFrameDraw <> nil) then profileFrameDraw.sectionBegin(profname);
    if gdbg_map_use_accel_render then
    begin
      tagmask := panelTypeToTag(panType);
      while (gDrawPanelList.count > 0) do
      begin
        pan := TPanel(gDrawPanelList.front());
        if ((pan.tag and tagmask) = 0) then break;
        if doDraw then r_Panel_Draw(pan, doAmbient, ambColor);
        gDrawPanelList.popFront();
      end;
    end
    else
    begin
      if doDraw then r_Map_DrawPanels(panType, hasAmbient, ambColor);
    end;
    if (profileFrameDraw <> nil) then profileFrameDraw.sectionEnd();
  end;

  procedure drawOther (profname: AnsiString; cb: TDrawCB);
  begin
    if (profileFrameDraw <> nil) then profileFrameDraw.sectionBegin(profname);
    if assigned(cb) then cb();
    if (profileFrameDraw <> nil) then profileFrameDraw.sectionEnd();
  end;

begin
  if (profileFrameDraw <> nil) then profileFrameDraw.sectionBegin('total');

  // our accelerated renderer will collect all panels to gDrawPanelList
  // we can use panel tag to render level parts (see GridTagXXX in g_map.pas)
  if (profileFrameDraw <> nil) then profileFrameDraw.sectionBegin('collect');
  if gdbg_map_use_accel_render then
  begin
    r_Map_CollectDrawPanels(sX, sY, sWidth, sHeight);
  end;
  if (profileFrameDraw <> nil) then profileFrameDraw.sectionEnd();

  if (profileFrameDraw <> nil) then profileFrameDraw.sectionBegin('skyback');
  r_Map_DrawBack(backXOfs, backYOfs);
  if (profileFrameDraw <> nil) then profileFrameDraw.sectionEnd();

  if setTransMatrix then
  begin
    //if (g_dbg_scale <> 1.0) then glTranslatef(0.0, -0.375/2, 0);
    glScalef(g_dbg_scale, g_dbg_scale, 1.0);
    glTranslatef(-sX, -sY, 0);
  end;

  // rendering mode
  ambColor := gCurrentMap['light_ambient'].rgba;
  hasAmbient := (not ambColor.isOpaque) or (not ambColor.isBlack);

  {
  if hasAmbient then
  begin
    //writeln('color: (', ambColor.r, ',', ambColor.g, ',', ambColor.b, ',', ambColor.a, ')');
    glColor4ub(ambColor.r, ambColor.g, ambColor.b, ambColor.a);
    glClear(GL_COLOR_BUFFER_BIT);
  end;
  }
  //writeln('color: (', ambColor.r, ',', ambColor.g, ',', ambColor.b, ',', ambColor.a, ')');


  drawPanelType('*back', PANEL_BACK, g_rlayer_back);
  drawPanelType('*step', PANEL_STEP, g_rlayer_step);
  drawOther('items', @r_Items_Draw);
  drawOther('weapons', @r_Weapon_Draw);
  drawOther('shells', @r_Player_DrawShells);
  drawOther('drawall', @r_Player_DrawAll);
  drawOther('corpses', @r_Player_DrawCorpses);
  drawPanelType('*wall', PANEL_WALL, g_rlayer_wall);
  drawOther('monsters', @r_Monsters_Draw);
  drawOther('itemdrop', @r_Items_DrawDrop);
  drawPanelType('*door', PANEL_CLOSEDOOR, g_rlayer_door);
  drawOther('gfx', @r_GFX_Draw);
  drawOther('flags', @r_Map_DrawFlags);
  drawPanelType('*acid1', PANEL_ACID1, g_rlayer_acid1);
  drawPanelType('*acid2', PANEL_ACID2, g_rlayer_acid2);
  drawPanelType('*water', PANEL_WATER, g_rlayer_water);
  drawOther('dynlights', @renderDynLightsInternal);

  if hasAmbient {and ((not g_playerLight) or (not gwin_has_stencil) or (g_dynLightCount < 1))} then
  begin
    renderAmbientQuad(hasAmbient, ambColor);
  end;

  doAmbient := true;
  drawPanelType('*fore', PANEL_FORE, g_rlayer_fore);


  if g_debug_HealthBar then
  begin
    r_Monsters_DrawHealth();
    r_Player_DrawHealth();
  end;

  if (profileFrameDraw <> nil) then profileFrameDraw.mainEnd(); // map rendering
end;


procedure DrawMapView(x, y, w, h: Integer);

var
  bx, by: Integer;
begin
  glPushMatrix();

  bx := Round(x/(gMapInfo.Width - w)*(gBackSize.X - w));
  by := Round(y/(gMapInfo.Height - h)*(gBackSize.Y - h));

  sX := x;
  sY := y;
  sWidth := w;
  sHeight := h;

  fixViewportForScale();
  renderMapInternal(-bx, -by, true);

  glPopMatrix();
end;


procedure DrawPlayer(p: TPlayer);
var
  px, py, a, b, c, d, i, fX, fY: Integer;
  camObj: TObj;
  //R: TRect;
begin
  if (p = nil) or (p.FDummy) then
  begin
    glPushMatrix();
    r_Map_DrawBack(0, 0);
    glPopMatrix();
    Exit;
  end;

  if (profileFrameDraw = nil) then profileFrameDraw := TProfiler.Create('RENDER', g_profile_history_size);
  if (profileFrameDraw <> nil) then profileFrameDraw.mainBegin(g_profile_frame_draw);

  gPlayerDrawn := p;

  glPushMatrix();

  camObj := p.getCameraObj();
  camObj.lerp(gLerpFactor, fX, fY);
  px := fX + PLAYER_RECT_CX;
  py := fY + PLAYER_RECT_CY+nlerp(p.SlopeOld, camObj.slopeUpLeft, gLerpFactor);

  if (g_dbg_scale = 1.0) and (not g_dbg_ignore_bounds) then
  begin
    if (px > (gPlayerScreenSize.X div 2)) then a := -px+(gPlayerScreenSize.X div 2) else a := 0;
    if (py > (gPlayerScreenSize.Y div 2)) then b := -py+(gPlayerScreenSize.Y div 2) else b := 0;

    if (px > gMapInfo.Width-(gPlayerScreenSize.X div 2)) then a := -gMapInfo.Width+gPlayerScreenSize.X;
    if (py > gMapInfo.Height-(gPlayerScreenSize.Y div 2)) then b := -gMapInfo.Height+gPlayerScreenSize.Y;

         if (gMapInfo.Width = gPlayerScreenSize.X) then a := 0
    else if (gMapInfo.Width < gPlayerScreenSize.X) then
    begin
      // hcenter
      a := (gPlayerScreenSize.X-gMapInfo.Width) div 2;
    end;

         if (gMapInfo.Height = gPlayerScreenSize.Y) then b := 0
    else if (gMapInfo.Height < gPlayerScreenSize.Y) then
    begin
      // vcenter
      b := (gPlayerScreenSize.Y-gMapInfo.Height) div 2;
    end;
  end
  else
  begin
    // scaled, ignore level bounds
    a := -px+(gPlayerScreenSize.X div 2);
    b := -py+(gPlayerScreenSize.Y div 2);
  end;

  sX := -a;
  sY := -b;
  sWidth := gPlayerScreenSize.X;
  sHeight := gPlayerScreenSize.Y;
  fixViewportForScale();

  i := py - (sY + sHeight div 2);
  if (p.IncCam > 0) then
  begin
    // clamp to level bounds
    if (sY - p.IncCam < 0) then
      p.IncCam := nclamp(sY, 0, 120);
    // clamp around player position
    if (i > 0) then
      p.IncCam := nclamp(p.IncCam, 0, max(0, 120 - i));
  end
  else if (p.IncCam < 0) then
  begin
    // clamp to level bounds
    if (sY + sHeight - p.IncCam > gMapInfo.Height) then
      p.IncCam := nclamp(sY + sHeight - gMapInfo.Height, -120, 0);
    // clamp around player position
    if (i < 0) then
      p.IncCam := nclamp(p.IncCam, min(0, -120 - i), 0);
  end;

  sY := sY - nlerp(p.IncCamOld, p.IncCam, gLerpFactor);

  if (not g_dbg_ignore_bounds) then
  begin
    if (sX+sWidth > gMapInfo.Width) then sX := gMapInfo.Width-sWidth;
    if (sY+sHeight > gMapInfo.Height) then sY := gMapInfo.Height-sHeight;
    if (sX < 0) then sX := 0;
    if (sY < 0) then sY := 0;
  end;

  if (gBackSize.X <= gPlayerScreenSize.X) or (gMapInfo.Width <= sWidth) then c := 0 else c := trunc((gBackSize.X-gPlayerScreenSize.X)*sX/(gMapInfo.Width-sWidth));
  if (gBackSize.Y <= gPlayerScreenSize.Y) or (gMapInfo.Height <= sHeight) then d := 0 else d := trunc((gBackSize.Y-gPlayerScreenSize.Y)*sY/(gMapInfo.Height-sHeight));

  //r_smallmap_h: 0: left; 1: center; 2: right
  //r_smallmap_v: 0: top; 1: center; 2: bottom
  // horiz small map?
  if (gMapInfo.Width = sWidth) then
  begin
    sX := 0;
  end
  else if (gMapInfo.Width < sWidth) then
  begin
    case r_smallmap_h of
      1: sX := -((sWidth-gMapInfo.Width) div 2); // center
      2: sX := -(sWidth-gMapInfo.Width); // right
      else sX := 0; // left
    end;
  end;
  // vert small map?
  if (gMapInfo.Height = sHeight) then
  begin
    sY := 0;
  end
  else if (gMapInfo.Height < sHeight) then
  begin
    case r_smallmap_v of
      1: sY := -((sHeight-gMapInfo.Height) div 2); // center
      2: sY := -(sHeight-gMapInfo.Height); // bottom
      else sY := 0; // top
    end;
  end;

  p.viewPortX := sX;
  p.viewPortY := sY;
  p.viewPortW := sWidth;
  p.viewPortH := sHeight;

{$IFDEF ENABLE_HOLMES}
  if (p = gPlayer1) then
  begin
    g_Holmes_plrViewPos(sX, sY);
    g_Holmes_plrViewSize(sWidth, sHeight);
  end;
{$ENDIF}

  renderMapInternal(-c, -d, true);

  if (gGameSettings.GameMode <> GM_SINGLE) and (gPlayerIndicator > 0) then
    case gPlayerIndicator of
      1:
        r_Player_DrawIndicator(p, _RGB(255, 255, 255));

      2:
        for i := 0 to High(gPlayers) do
          if gPlayers[i] <> nil then
            if gPlayers[i] = p then
              r_Player_DrawIndicator(p, _RGB(255, 255, 255))
            else if (gPlayers[i].Team = p.Team) and (gPlayers[i].Team <> TEAM_NONE) then
              if gPlayerIndicatorStyle = 1 then
                r_Player_DrawIndicator(gPlayers[i], _RGB(192, 192, 192))
              else
                r_Player_DrawIndicator(gPlayers[i], gPlayers[i].GetColor);
    end;

  {
  for a := 0 to High(gCollideMap) do
    for b := 0 to High(gCollideMap[a]) do
    begin
      d := 0;
      if ByteBool(gCollideMap[a, b] and MARK_WALL) then
        d := d + 1;
      if ByteBool(gCollideMap[a, b] and MARK_DOOR) then
        d := d + 2;

      case d of
        1: e_DrawPoint(1, b, a, 200, 200, 200);
        2: e_DrawPoint(1, b, a, 64, 64, 255);
        3: e_DrawPoint(1, b, a, 255, 0, 255);
      end;
    end;
  }

  glPopMatrix();

  r_Player_DrawPain(p);
  r_Player_DrawPickup(p);
  r_Player_DrawRulez(p);
  if gShowMap then DrawMinimap(p, _TRect(0, 0, 128, 128));
  if g_Debug_Player then
    r_Player_DrawDebug(p);
  r_Player_DrawGUI(p);
end;

procedure drawProfilers ();
var
  px: Integer = -1;
  py: Integer = -1;
begin
  if g_profile_frame_draw and (profileFrameDraw <> nil) then px := px-drawProfiles(px, py, profileFrameDraw);
  if g_profile_collision and (profMapCollision <> nil) then begin px := px-drawProfiles(px, py, profMapCollision); py -= calcProfilesHeight(profMonsLOS); end;
  if g_profile_los and (profMonsLOS <> nil) then begin px := px-drawProfiles(px, py, profMonsLOS); py -= calcProfilesHeight(profMonsLOS); end;
end;

procedure r_Game_Draw();
var
  ID: DWORD;
  w, h: Word;
  ww, hh: Byte;
  Time: Int64;
  back: string;
  plView1, plView2: TPlayer;
  Split: Boolean;
begin
  if gExit = EXIT_QUIT then Exit;

  Time := GetTickCount64() {div 1000};
  FPSCounter := FPSCounter+1;
  if Time - FPSTime >= 1000 then
  begin
    FPS := FPSCounter;
    FPSCounter := 0;
    FPSTime := Time;
  end;

  e_SetRendertarget(True);
  e_SetViewPort(0, 0, gScreenWidth, gScreenHeight);

  if gGameOn or (gState = STATE_FOLD) then
  begin
    if (gPlayer1 <> nil) and (gPlayer2 <> nil) then
    begin
      gSpectMode := SPECT_NONE;
      if not gRevertPlayers then
      begin
        plView1 := gPlayer1;
        plView2 := gPlayer2;
      end
      else
      begin
        plView1 := gPlayer2;
        plView2 := gPlayer1;
      end;
    end
    else
      if (gPlayer1 <> nil) or (gPlayer2 <> nil) then
      begin
        gSpectMode := SPECT_NONE;
        if gPlayer2 = nil then
          plView1 := gPlayer1
        else
          plView1 := gPlayer2;
        plView2 := nil;
      end
      else
      begin
        plView1 := nil;
        plView2 := nil;
      end;

    if (plView1 = nil) and (plView2 = nil) and (gSpectMode = SPECT_NONE) then
      gSpectMode := SPECT_STATS;

    if gSpectMode = SPECT_PLAYERS then
      if gPlayers <> nil then
      begin
        plView1 := GetActivePlayer_ByID(gSpectPID1);
        if plView1 = nil then
        begin
          gSpectPID1 := GetActivePlayerID_Next();
          plView1 := GetActivePlayer_ByID(gSpectPID1);
        end;
        if gSpectViewTwo then
        begin
          plView2 := GetActivePlayer_ByID(gSpectPID2);
          if plView2 = nil then
          begin
            gSpectPID2 := GetActivePlayerID_Next();
            plView2 := GetActivePlayer_ByID(gSpectPID2);
          end;
        end;
      end;

    if gSpectMode = SPECT_MAPVIEW then
    begin
    // Режим просмотра карты
      Split := False;
      e_SetViewPort(0, 0, gScreenWidth, gScreenHeight);
      DrawMapView(gSpectX, gSpectY, gScreenWidth, gScreenHeight);
      gHearPoint1.Active := True;
      gHearPoint1.Coords.X := gScreenWidth div 2 + gSpectX;
      gHearPoint1.Coords.Y := gScreenHeight div 2 + gSpectY;
      gHearPoint2.Active := False;
    end
    else
    begin
      Split := (plView1 <> nil) and (plView2 <> nil);

    // Точки слуха игроков
      if plView1 <> nil then
      begin
        gHearPoint1.Active := True;
        gHearPoint1.Coords.X := plView1.GameX + PLAYER_RECT.Width;
        gHearPoint1.Coords.Y := plView1.GameY + PLAYER_RECT.Height DIV 2;
      end else
        gHearPoint1.Active := False;
      if plView2 <> nil then
      begin
        gHearPoint2.Active := True;
        gHearPoint2.Coords.X := plView2.GameX + PLAYER_RECT.Width;
        gHearPoint2.Coords.Y := plView2.GameY + PLAYER_RECT.Height DIV 2;
      end else
        gHearPoint2.Active := False;

    // Размер экранов игроков:
      gPlayerScreenSize.X := gScreenWidth-196;
      if Split then
      begin
        gPlayerScreenSize.Y := gScreenHeight div 2;
        if gScreenHeight mod 2 = 0 then
          Dec(gPlayerScreenSize.Y);
      end
      else
        gPlayerScreenSize.Y := gScreenHeight;

      if Split then
        if gScreenHeight mod 2 = 0 then
          e_SetViewPort(0, gPlayerScreenSize.Y+2, gPlayerScreenSize.X+196, gPlayerScreenSize.Y)
        else
          e_SetViewPort(0, gPlayerScreenSize.Y+1, gPlayerScreenSize.X+196, gPlayerScreenSize.Y);

      DrawPlayer(plView1);
      gPlayer1ScreenCoord.X := sX;
      gPlayer1ScreenCoord.Y := sY;

      if Split then
      begin
        e_SetViewPort(0, 0, gPlayerScreenSize.X+196, gPlayerScreenSize.Y);

        DrawPlayer(plView2);
        gPlayer2ScreenCoord.X := sX;
        gPlayer2ScreenCoord.Y := sY;
      end;

      e_SetViewPort(0, 0, gScreenWidth, gScreenHeight);

      if Split then
        e_DrawLine(2, 0, gScreenHeight div 2, gScreenWidth, gScreenHeight div 2, 0, 0, 0);
    end;

{$IFDEF ENABLE_HOLMES}
    // draw inspector
    if (g_holmes_enabled) then g_Holmes_Draw();
{$ENDIF}

    if MessageText <> '' then
    begin
      w := 0;
      h := 0;
      e_CharFont_GetSizeFmt(gMenuFont, MessageText, w, h);
      if Split then
        e_CharFont_PrintFmt(gMenuFont, (gScreenWidth div 2)-(w div 2),
                        (gScreenHeight div 2)-(h div 2), MessageText)
      else
        e_CharFont_PrintFmt(gMenuFont, (gScreenWidth div 2)-(w div 2),
                  Round(gScreenHeight / 2.75)-(h div 2), MessageText);
    end;

    if IsDrawStat or (gSpectMode = SPECT_STATS) then
      DrawStat();

    if gSpectHUD and (not gChatShow) and (gSpectMode <> SPECT_NONE) and (not gSpectAuto) then
    begin
    // Draw spectator GUI
      ww := 0;
      hh := 0;
      e_TextureFontGetSize(gStdFont, ww, hh);
      case gSpectMode of
        SPECT_STATS:
          e_TextureFontPrintEx(0, gScreenHeight - (hh+2)*2, 'MODE: Stats', gStdFont, 255, 255, 255, 1);
        SPECT_MAPVIEW:
          e_TextureFontPrintEx(0, gScreenHeight - (hh+2)*2, 'MODE: Observe Map', gStdFont, 255, 255, 255, 1);
        SPECT_PLAYERS:
          e_TextureFontPrintEx(0, gScreenHeight - (hh+2)*2, 'MODE: Watch Players', gStdFont, 255, 255, 255, 1);
      end;
      e_TextureFontPrintEx(2*ww, gScreenHeight - (hh+2), '< jump >', gStdFont, 255, 255, 255, 1);
      if gSpectMode = SPECT_STATS then
      begin
        e_TextureFontPrintEx(16*ww, gScreenHeight - (hh+2)*2, 'Autoview', gStdFont, 255, 255, 255, 1);
        e_TextureFontPrintEx(16*ww, gScreenHeight - (hh+2), '< fire >', gStdFont, 255, 255, 255, 1);
      end;
      if gSpectMode = SPECT_MAPVIEW then
      begin
        e_TextureFontPrintEx(22*ww, gScreenHeight - (hh+2)*2, '[-]', gStdFont, 255, 255, 255, 1);
        e_TextureFontPrintEx(26*ww, gScreenHeight - (hh+2)*2, 'Step ' + IntToStr(gSpectStep), gStdFont, 255, 255, 255, 1);
        e_TextureFontPrintEx(34*ww, gScreenHeight - (hh+2)*2, '[+]', gStdFont, 255, 255, 255, 1);
        e_TextureFontPrintEx(18*ww, gScreenHeight - (hh+2), '<prev weap>', gStdFont, 255, 255, 255, 1);
        e_TextureFontPrintEx(30*ww, gScreenHeight - (hh+2), '<next weap>', gStdFont, 255, 255, 255, 1);
      end;
      if gSpectMode = SPECT_PLAYERS then
      begin
        e_TextureFontPrintEx(22*ww, gScreenHeight - (hh+2)*2, 'Player 1', gStdFont, 255, 255, 255, 1);
        e_TextureFontPrintEx(20*ww, gScreenHeight - (hh+2), '<left/right>', gStdFont, 255, 255, 255, 1);
        if gSpectViewTwo then
        begin
          e_TextureFontPrintEx(37*ww, gScreenHeight - (hh+2)*2, 'Player 2', gStdFont, 255, 255, 255, 1);
          e_TextureFontPrintEx(34*ww, gScreenHeight - (hh+2), '<prev w/next w>', gStdFont, 255, 255, 255, 1);
          e_TextureFontPrintEx(52*ww, gScreenHeight - (hh+2)*2, '2x View', gStdFont, 255, 255, 255, 1);
          e_TextureFontPrintEx(51*ww, gScreenHeight - (hh+2), '<up/down>', gStdFont, 255, 255, 255, 1);
        end
        else
        begin
          e_TextureFontPrintEx(35*ww, gScreenHeight - (hh+2)*2, '2x View', gStdFont, 255, 255, 255, 1);
          e_TextureFontPrintEx(34*ww, gScreenHeight - (hh+2), '<up/down>', gStdFont, 255, 255, 255, 1);
        end;
      end;
    end;
  end;

  if gPauseMain and gGameOn and (g_ActiveWindow = nil) then
  begin
    //e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 48, 48, 48, 180);
    e_DarkenQuadWH(0, 0, gScreenWidth, gScreenHeight, 150);

    e_CharFont_GetSize(gMenuFont, _lc[I_MENU_PAUSE], w, h);
    e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(w div 2),
                    (gScreenHeight div 2)-(h div 2), _lc[I_MENU_PAUSE]);
  end;

  if not gGameOn then
  begin
    if (gState = STATE_MENU) then
    begin
      if (g_ActiveWindow = nil) or (g_ActiveWindow.BackTexture = '') then r_Game_DrawMenuBackground('MENU_BACKGROUND');
      // F3 at menu will show game loading dialog
      if e_KeyPressed(IK_F3) then g_Menu_Show_LoadMenu(true);
      if (g_ActiveWindow <> nil) then
      begin
        //e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 48, 48, 48, 180);
        e_DarkenQuadWH(0, 0, gScreenWidth, gScreenHeight, 150);
      end
      else
      begin
        // F3 at titlepic will show game loading dialog
        if e_KeyPressed(IK_F3) then
        begin
          g_Menu_Show_LoadMenu(true);
          if (g_ActiveWindow <> nil) then e_DarkenQuadWH(0, 0, gScreenWidth, gScreenHeight, 150);
        end;
      end;
    end;

    if gState = STATE_FOLD then
    begin
      e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 0, 0, 0, EndingGameCounter);
    end;

    if gState = STATE_INTERCUSTOM then
    begin
      if gLastMap and (gGameSettings.GameMode = GM_COOP) then
      begin
        back := 'TEXTURE_endpic';
        if not g_Texture_Get(back, ID) then
          back := _lc[I_TEXTURE_ENDPIC];
      end
      else
        back := 'INTER';

      r_Game_DrawMenuBackground(back);

      DrawCustomStat();

      if g_ActiveWindow <> nil then
      begin
        //e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 48, 48, 48, 180);
        e_DarkenQuadWH(0, 0, gScreenWidth, gScreenHeight, 150);
      end;
    end;

    if gState = STATE_INTERSINGLE then
    begin
      if EndingGameCounter > 0 then
      begin
        e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 0, 0, 0, EndingGameCounter);
      end
      else
      begin
        back := 'INTER';

        r_Game_DrawMenuBackground(back);

        DrawSingleStat();

        if g_ActiveWindow <> nil then
        begin
          //e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 48, 48, 48, 180);
          e_DarkenQuadWH(0, 0, gScreenWidth, gScreenHeight, 150);
        end;
      end;
    end;

    if gState = STATE_ENDPIC then
    begin
      ID := DWORD(-1);
      if g_Texture_Get('TEXTURE_endpic', ID) then r_Game_DrawMenuBackground('TEXTURE_endpic')
      else r_Game_DrawMenuBackground(_lc[I_TEXTURE_ENDPIC]);

      if g_ActiveWindow <> nil then
      begin
        //e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 48, 48, 48, 180);
        e_DarkenQuadWH(0, 0, gScreenWidth, gScreenHeight, 150);
      end;
    end;

    if gState = STATE_SLIST then
    begin
//      if g_Texture_Get('MENU_BACKGROUND', ID) then
//      begin
//        e_DrawSize(ID, 0, 0, 0, False, False, gScreenWidth, gScreenHeight);
//        //e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 48, 48, 48, 180);
//      end;
      r_Game_DrawMenuBackground('MENU_BACKGROUND');
      e_DarkenQuadWH(0, 0, gScreenWidth, gScreenHeight, 150);
      r_Serverlist_Draw(slCurrent, slTable);
    end;
  end;

  if g_ActiveWindow <> nil then
  begin
    if gGameOn then
    begin
      //e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 48, 48, 48, 180);
      e_DarkenQuadWH(0, 0, gScreenWidth, gScreenHeight, 150);
    end;
    g_ActiveWindow.Draw();
  end;

{$IFNDEF HEADLESS}
  r_Console_Draw();
{$ENDIF}

  if g_debug_Sounds and gGameOn then
  begin
    for w := 0 to High(e_SoundsArray) do
      for h := 0 to e_SoundsArray[w].nRefs do
        e_DrawPoint(1, w+100, h+100, 255, 0, 0);
  end;

  if gShowFPS then
  begin
    e_TextureFontPrint(0, 0, Format('FPS: %d', [FPS]), gStdFont);
    e_TextureFontPrint(0, 16, Format('UPS: %d', [UPS]), gStdFont);
  end;

  if gGameOn and gShowTime then
    drawTime(gScreenWidth-72, gScreenHeight-16);

  if gGameOn then drawProfilers();

  // TODO: draw this after the FBO and remap mouse click coordinates

{$IFDEF ENABLE_HOLMES}
  g_Holmes_DrawUI();
{$ENDIF}

  // blit framebuffer to screen

  e_SetRendertarget(False);
  e_SetViewPort(0, 0, gWinSizeX, gWinSizeY);
  e_BlitFramebuffer(gWinSizeX, gWinSizeY);

  // draw the overlay stuff on top of it

  g_Touch_Draw;
end;

end.
