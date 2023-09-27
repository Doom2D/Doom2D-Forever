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
unit g_options;

interface

uses
  g_language, g_weapons, utils;

function GenPlayerName (n: Integer): String;

procedure g_Options_SetDefault;
procedure g_Options_SetDefaultVideo;
procedure g_Options_ApplyGameSettings;

const DF_Default_Megawad_Start = 'megawads/DOOM2D.WAD:\MAP01';

var
  gBPP: Integer;
  gFreq: Byte;
  gFullscreen: Boolean;
  gWinSizeX, gWinSizeY: Integer;
  gWinMaximized: Boolean;
  gVSync: Boolean;
  glLegacyNPOT: Boolean;
  glRenderToFBO: Boolean = True;
  gTextureFilter: Boolean;
  gLerpActors: Boolean = True;
  gFrameTime: Integer = 5;
  gMaxFPS: Integer = 200;
  gNoSound: Boolean;
  gSoundLevel: Integer;
  gMusicLevel: Integer;
  gMaxSimSounds: Integer;
  gMuteWhenInactive: Boolean;
  gAdvCorpses: Boolean;
  gAdvBlood: Boolean;
  gAdvGibs: Boolean;
  gGibsCount: Integer;
  gBloodCount: Integer;
  gFlash: Integer;
  gDrawBackGround: Boolean;
  gShowMessages: Boolean;
  gRevertPlayers: Boolean;
  gLanguage: String;
  gAskLanguage: Boolean;
  gSaveStats: Boolean = False;
  gScreenshotStats: Boolean = False;
  gsSDLSampleRate: Integer;
  gsSDLBufferSize: Integer;
  gDefaultMegawadStart: AnsiString;
  glNPOTOverride: Boolean = false;

  (* Latched game settings *)
  gsMap: String;
  gsGameMode: String;
  gsTimeLimit: Word;
  gsScoreLimit: Word;
  gsMaxLives: Byte;
  gsPlayers: Byte;
  gsGameFlags: LongWord;
  gsSpawnInvul: Integer = 0;
  gsItemRespawnTime: Word = 60;
  gsItemRespawnRandom: Word = 0;
  gsRulezRespawnTime: Word = 60;
  gsRulezRespawnRandom: Word = 0;
  gsWarmupTime: Word = 30;

implementation

uses
  {$INCLUDE ../nogl/noGLuses.inc}
  {$IFDEF USE_SDL2}
    SDL2,
  {$ENDIF}
  e_log, e_input, g_console, g_window, g_sound, g_gfx, g_player, Math,
  g_map, g_net, g_netmaster, SysUtils, CONFIG, g_game, g_main, e_texture,
  g_items, wadreader, e_graphics, g_touch, envvars, g_system;

  var
    machine: Integer;

  function GenPlayerName (n: Integer): String;
  begin
    ASSERT(n >= 1);
    Result := GetUserName;
    if Result = '' then
      Result := 'Player' + IntToStr(machine MOD 10000);
    if n = 1 then
      Result := Copy(Result, 1, 12)
    else
      Result := Copy(Result, 1, 10) + ' ' + IntToStr(n)
  end;

{$IFDEF USE_SDL2}
procedure g_Options_SetDefaultVideo;
  var display: TSDL_DisplayMode;
  {$IFNDEF ANDROID}
    var target, closest: TSDL_DisplayMode; percentage: Integer;
  {$ENDIF}
begin
  (* Display 0 = Primary display *)
  gScreenWidth := 640;
  gScreenHeight := 480;
  gWinSizeX := 640;
  gWinSizeY := 480;
  //gBPP := SDL_BITSPERPIXEL(display.format);
  gBPP := 32;
  {$IFDEF ANDROID}
    gFullScreen := True; (* rotation not allowed? *)
  {$ELSE}
    gFullScreen := False;
  {$ENDIF}
  if SDL_GetDesktopDisplayMode(0, @display) = 0 then
  begin
  {$IFDEF ANDROID}
    gWinSizeX := display.w;
    gWinSizeY := display.h;
  {$ELSE}
    (* Window must be smaller than display *)
    closest.w := display.w;
    closest.h := display.h;
    percentage := 75;
    while (display.w - closest.w < 48) or (display.h - closest.h < 48) do
    begin
      if percentage < 25 then
      begin
        closest.w := display.w * 75 div 100;
        closest.h := display.h * 75 div 100;
        break;
      end;
      target.w := display.w * percentage div 100;
      target.h := display.h * percentage div 100;
      target.format := 0; (* didn't care *)
      target.refresh_rate := 0; (* didn't care *)
      target.driverdata := nil; (* init *)
      SDL_GetClosestDisplayMode(0, @target, @closest);
      Dec(percentage);
    end;
    gWinSizeX := closest.w;
    gWinSizeY := closest.h;
    //gBPP := SDL_BITSPERPIXEL(closest.format); (* Resolution list didn't work for some reason *)
  {$ENDIF}
  end
  else
  begin
    e_LogWritefln('SDL: Failed to get desktop display mode: %s', [SDL_GetError])
  end;
  (* Must be positioned on primary display *)
  gWinMaximized := False;
  gVSync := True;
  gTextureFilter := True;
  glLegacyNPOT := False;
  gRC_Width := gWinSizeX;
  gRC_Height := gWinSizeY;
  gRC_FullScreen := gFullScreen;
  gRC_Maximized := gWinMaximized;
  e_LogWriteLn('g_Options_SetDefaultVideo: w = ' + IntToStr(gWinSizeX) + ' h = ' + IntToStr(gWinSizeY));
  g_Console_ResetBinds;
end;
{$ELSE}
procedure g_Options_SetDefaultVideo;
begin
  gWinSizeX := 640;
  gWinSizeY := 480;
  gBPP := 32;
  gFullScreen := False;
  gWinMaximized := False;
  gVSync := True;
  gTextureFilter := True;
  glLegacyNPOT := False;
  gScreenWidth := gWinSizeX;
  gScreenHeight := gWinSizeY;
  gRC_Width := gWinSizeX;
  gRC_Height := gWinSizeY;
  gRC_FullScreen := gFullScreen;
  gRC_Maximized := gWinMaximized;
  e_LogWriteLn('g_Options_SetDefaultVideo: w = ' + IntToStr(gWinSizeX) + ' h = ' + IntToStr(gWinSizeY));
  g_Console_ResetBinds;
end;
{$ENDIF}

procedure g_Options_SetDefault();
var
  i: Integer;
begin
  (* section Sound *)
  gNoSound := False;
  gSoundLevel := 75;
  gMusicLevel := 65;
  gMaxSimSounds := 8;
  gMuteWhenInactive := False;
  gAnnouncer := ANNOUNCE_MEPLUS;
  gSoundEffectsDF := True;
  gUseChatSounds := True;
  gsSDLSampleRate := 44100;
  gsSDLBufferSize := 2048;

  g_Sound_SetupAllVolumes(gSoundLevel, gMusicLevel);

  with gPlayer1Settings do
  begin
    Name := GenPlayerName(1);
    Model := STD_PLAYER_MODEL;
    Color.R := PLAYER1_DEF_COLOR.R;
    Color.G := PLAYER1_DEF_COLOR.G;
    Color.B := PLAYER1_DEF_COLOR.B;
    Team := TEAM_RED;
  end;

  with gPlayer2Settings do
  begin
    Name := GenPlayerName(2);
    Model := STD_PLAYER_MODEL;
    Color.R := PLAYER2_DEF_COLOR.R;
    Color.G := PLAYER2_DEF_COLOR.G;
    Color.B := PLAYER2_DEF_COLOR.B;
    Team := TEAM_BLUE;
  end;

  (* section Joysticks *)
  for i := 0 to e_MaxJoys - 1 do
  begin
    e_JoystickDeadzones[i] := 8192
  end;

  (* section Game *)
  g_GFX_SetMax(2000);
  g_Shells_SetMax(300);
  g_Gibs_SetMax(150);
  g_Corpses_SetMax(20);
  gGibsCount := 32;
  gBloodCount := 4;
  gAdvBlood := True;
  gAdvCorpses := True;
  gAdvGibs := True;
  gFlash := 1;
  gDrawBackGround := True;
  gShowMessages := True;
  gRevertPlayers := False;
  gChatBubble := 4;
  wadoptDebug := False;
  wadoptFast := False;
  e_FastScreenshots := True;
  gDefaultMegawadStart := DF_Default_Megawad_Start;
  g_dbg_scale := 1.0;
  gSaveStats := False;

  gAskLanguage := True;
  gLanguage := LANGUAGE_ENGLISH;

  gsMap := '';
  gsGameMode := _lc[I_MENU_GAME_TYPE_DM];
  gsTimeLimit := 0;
  gsScoreLimit := 0;
  gsMaxLives := 0;
  gsPlayers := 1;
  gsSpawnInvul := 0;
  gsItemRespawnTime := 60;
  gsItemRespawnRandom := 0;
  gsRulezRespawnTime := 60;
  gsRulezRespawnRandom := 0;
  gsGameFlags := GAME_OPTION_ALLOWEXIT or GAME_OPTION_DMKEYS or
    GAME_OPTION_BOTVSPLAYER or GAME_OPTION_BOTVSMONSTER or
    GAME_OPTION_TEAMHITTRACE or GAME_OPTION_TEAMHITPROJECTILE or
    GAME_OPTION_ALLOWDROPFLAG;
  gsPlayers := 1;

  if not gGameOn then
    g_Options_ApplyGameSettings;

  (* section MasterServer *)
  NetMasterList := 'mpms.doom2d.org:25665, deadsoftware.ru:25665, terminalcorner.ru:25665';
  g_Net_Slist_Set(NetMasterList);

  (* section Server *)
  NetServerName := 'Unnamed Server';
  NetPassword := '';
  NetPort := 25666;
  NetMaxClients := 16;
  NetAllowRCON := False;
  NetRCONPassword := 'default';
  NetUseMaster := True;
  NetUpdateRate := 0;
  NetRelupdRate := 18;
  NetMasterRate := 60000;
  NetForwardPorts := False;

  (* section Client *)
  NetInterpLevel := 2;
  NetForcePlayerUpdate := False;
  NetPredictSelf := True;
  NetClientIP := '127.0.0.1';
  NetClientPort := NetPort;
end;

procedure g_Options_ApplyGameSettings;
begin
  with gGameSettings do
  begin
    GameMode := g_Game_TextToMode(gsGameMode);
    if GameMode = GM_NONE then
      GameMode := GM_DM;
    if GameMode = GM_SINGLE then
      GameMode := GM_COOP;
    TimeLimit := gsTimeLimit;
    ScoreLimit := gsScoreLimit;
    MaxLives := gsMaxLives;
    SpawnInvul := gsSpawnInvul;
    ItemRespawnTime := gsItemRespawnTime;
    ItemRespawnRandom := gsItemRespawnRandom;
    RulezRespawnTime := gsRulezRespawnTime;
    RulezRespawnRandom := gsRulezRespawnRandom;
    WarmupTime := gsWarmupTime;
    Options := gsGameFlags;
  end;
end;

initialization
  Randomize;
  machine := Random(10000);

  (* Video *)
  conRegVar('r_width', @gRC_Width, '', '');
  conRegVar('r_height', @gRC_Height, '', '');
  conRegVar('r_fullscreen', @gRC_FullScreen, '', '');
  conRegVar('r_maximized', @gRC_Maximized, '', '');
  conRegVar('r_bpp', @gBPP, '', '');
  conRegVar('r_vsync', @gVSync, '', '');
  conRegVar('r_texfilter', @gTextureFilter, '', '');
  conRegVar('r_npot', @glNPOTOverride, '', '');
  conRegVar('r_interp', @gLerpActors, '', 'interpolate actors');

  (* Sound *)
  conRegVar('s_nosound', @gNoSound, '', '');
  conRegVar('s_soundvolume', @gSoundLevel, '', '');
  conRegVar('s_musicvolume', @gMusicLevel, '', '');
  conRegVar('s_maxsim', @gMaxSimSounds, '', ''); // e_sound_fmod/sdl?
  conRegVar('s_muteinactive', @gMuteWhenInactive, '', '');
  conRegVar('s_announcer', @gAnnouncer, '', '');
  conRegVar('s_sfx', @gSoundEffectsDF, '', '');
  conRegVar('s_chatsounds', @gUseChatSounds, '', '');
  {$IFDEF USE_SDLMIXER}
    conRegVar('sdl_mixer_samplerate', @gsSDLSampleRate, '', '');
    conRegVar('sdl_mixer_buffersize', @gsSDLBufferSize, '', '');
  {$ENDIF}

  (* Game *)
  conRegVar('g_gibs_count', @gGibsCount, '', '');
  conRegVar('g_blood_count', @gBloodCount, '', '');
  conRegVar('g_adv_blood', @gAdvBlood, '', '');
  conRegVar('g_adv_corpses', @gAdvCorpses, '', '');
  conRegVar('g_adv_gibs', @gAdvGibs, '', '');
  conRegVar('r_flash', @gFlash, '', '');
  conRegVar('r_background', @gDrawBackGround, '', '');
  conRegVar('g_show_messages', @gShowMessages, '', '');
  conRegVar('r_revert_players', @gRevertPlayers, '', '');
  conRegVar('r_chat_bubble', @gChatBubble, '', '');
  conRegVar('sfs_debug', @wadoptDebug, '', '');
  conRegVar('sfs_fastmode', @wadoptFast, '', '');
  conRegVar('g_fast_screenshots', @e_FastScreenshots, '', '');
  conRegVar('g_default_megawad', @gDefaultMegawadStart, '', '');
  conRegVar('g_save_stats', @gSaveStats, '', '');
  conRegVar('g_screenshot_stats', @gScreenshotStats, '', '');
  conRegVar('g_lastmap', @gsMap, '', '');
end.
