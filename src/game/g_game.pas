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
unit g_game;

interface

uses
  SysUtils, Classes,
  MAPDEF,
  g_basic, g_player, r_graphics, g_res_downloader,
  g_sound, g_gui, utils, md5, mempool, xprofiler,
  g_touch, g_weapons;

type
  TGameSettings = record
    GameType: Byte;
    GameMode: Byte;
    TimeLimit: Word;
    ScoreLimit: Word;
    WarmupTime: Word;
    SpawnInvul: Word;
    ItemRespawnTime: Word;
    MaxLives: Byte;
    Options: LongWord;
    WAD: String;
  end;

  TGameEvent = record
    Name: String;
    Command: String;
  end;

  TDelayedEvent = record
    Pending: Boolean;
    Time: LongWord;
    DEType: Byte;
    DENum: Integer;
    DEStr: String;
  end;

  TChatSound = record
    Sound: TPlayableSound;
    Tags: Array of String;
    FullWord: Boolean;
  end;

  TPlayerSettings = record
    Name: String;
    Model: String;
    Color: TRGB;
    Team: Byte;
    // ones below are sent only to the server
    WeaponSwitch: Byte;
    WeaponPreferences: Array[WP_FIRST..WP_LAST+1] of Byte;
    SwitchToEmpty: Byte;
    SkipFist: Byte;
  end;

  TMegaWADInfo = record
    Name: String;
    Description: String;
    Author: String;
    Pic: String;
  end;

  THearPoint = record
    Active: Boolean;
    Coords: TDFPoint;
  end;

function  g_Game_IsNet(): Boolean;
function  g_Game_IsServer(): Boolean;
function  g_Game_IsClient(): Boolean;
procedure g_Game_Init();
procedure g_Game_Free (freeTextures: Boolean=true);
procedure g_Game_LoadData();
procedure g_Game_FreeData();
procedure g_Game_Update();
procedure g_Game_PreUpdate();
procedure g_Game_Quit();
procedure g_Game_SetupScreenSize();
procedure g_Game_ChangeResolution(newWidth, newHeight: Word; nowFull, nowMax: Boolean);
function  g_Game_ModeToText(Mode: Byte): string;
function  g_Game_TextToMode(Mode: string): Byte;
procedure g_Game_ExecuteEvent(Name: String);
function  g_Game_DelayEvent(DEType: Byte; Time: LongWord; Num: Integer = 0; Str: String = ''): Integer;
procedure g_Game_AddPlayer(Team: Byte = TEAM_NONE);
procedure g_Game_RemovePlayer();
procedure g_Game_Spectate();
procedure g_Game_SpectateCenterView();
procedure g_Game_StartSingle(Map: String; TwoPlayers: Boolean; nPlayers: Byte);
procedure g_Game_StartCustom(Map: String; GameMode: Byte; TimeLimit, ScoreLimit: Word; MaxLives: Byte; Options: LongWord; nPlayers: Byte);
procedure g_Game_StartServer(Map: String; GameMode: Byte; TimeLimit, ScoreLimit: Word; MaxLives: Byte; Options: LongWord; nPlayers: Byte; IPAddr: LongWord; Port: Word);
procedure g_Game_StartClient(Addr: String; Port: Word; PW: String);
procedure g_Game_Restart();
procedure g_Game_RestartLevel();
procedure g_Game_RestartRound(NoMapRestart: Boolean = False);
function  g_Game_ClientWAD (NewWAD: String; const WHash: TMD5Digest): AnsiString;
function  g_Game_StartMap(asMegawad: Boolean; Map: String; Force: Boolean = False; const oldMapPath: AnsiString=''): Boolean;
procedure g_Game_ChangeMap(const MapPath: String);
procedure g_Game_ExitLevel(const Map: AnsiString);
function  g_Game_GetFirstMap(WAD: String): String;
function  g_Game_GetNextMap(): String;
procedure g_Game_NextLevel();
procedure g_Game_Pause(Enable: Boolean);
procedure g_Game_HolmesPause(Enable: Boolean);
procedure g_Game_InGameMenu(Show: Boolean);
function  g_Game_IsWatchedPlayer(UID: Word): Boolean;
function  g_Game_IsWatchedTeam(Team: Byte): Boolean;
procedure g_Game_Message(Msg: String; Time: Word);
procedure g_Game_LoadMapList(FileName: String);
procedure g_Game_PauseAllSounds(Enable: Boolean);
procedure g_Game_StopAllSounds(all: Boolean);
procedure g_Game_UpdateTriggerSounds();
function  g_Game_GetMegaWADInfo(WAD: String): TMegaWADInfo;
procedure g_Game_ChatSound(Text: String; Taunt: Boolean = True);
procedure g_Game_Announce_GoodShot(SpawnerUID: Word);
procedure g_Game_Announce_KillCombo(Param: Integer);
procedure g_Game_Announce_BodyKill(SpawnerUID: Word);
procedure g_Game_StartVote(Command, Initiator: string);
procedure g_Game_CheckVote;
procedure g_TakeScreenShot(Filename: string = '');
procedure g_FatalError(Text: String);
procedure g_SimpleError(Text: String);
function  g_Game_IsTestMap(): Boolean;
procedure g_Game_DeleteTestMap();
procedure GameCVars(P: SSArray);
procedure PlayerSettingsCVars(P: SSArray);
procedure SystemCommands(P: SSArray);
procedure GameCommands(P: SSArray);
procedure GameCheats(P: SSArray);
procedure DebugCommands(P: SSArray);
procedure g_Game_Process_Params;
procedure g_Game_SetLoadingText(Text: String; Max: Integer; reWrite: Boolean);
procedure g_Game_StepLoading(Value: Integer = -1);
procedure g_Game_ClearLoading();
procedure g_Game_SetDebugMode();

function IsActivePlayer(p: TPlayer): Boolean;
function GetActivePlayerID_Next(Skip: Integer = -1): Integer;
procedure SortGameStat(var stat: TPlayerStatArray);

{ procedure SetWinPause(Enable: Boolean); }

const
  GAME_TICK = 28;

  LOADING_SHOW_STEP = 100;
  LOADING_INTERLINE = 20;

  GT_NONE   = 0;
  GT_SINGLE = 1;
  GT_CUSTOM = 2;
  GT_SERVER = 3;
  GT_CLIENT = 4;

  GM_NONE = 0;
  GM_DM   = 1;
  GM_TDM  = 2;
  GM_CTF  = 3;
  GM_COOP = 4;
  GM_SINGLE = 5;

  MESSAGE_DIKEY = WM_USER + 1;

  EXIT_QUIT            = 1;
  EXIT_SIMPLE          = 2;
  EXIT_RESTART         = 3;
  EXIT_ENDLEVELSINGLE  = 4;
  EXIT_ENDLEVELCUSTOM  = 5;

  GAME_OPTION_RESERVED          = 1;
  GAME_OPTION_TEAMDAMAGE        = 2;
  GAME_OPTION_ALLOWEXIT         = 4;
  GAME_OPTION_WEAPONSTAY        = 8;
  GAME_OPTION_MONSTERS          = 16;
  GAME_OPTION_BOTVSPLAYER       = 32;
  GAME_OPTION_BOTVSMONSTER      = 64;
  GAME_OPTION_DMKEYS            = 128;
  GAME_OPTION_TEAMHITTRACE      = 256;
  GAME_OPTION_TEAMHITPROJECTILE = 512;
  GAME_OPTION_TEAMABSORBDAMAGE  = 1024;
  GAME_OPTION_ALLOWDROPFLAG     = 2048;
  GAME_OPTION_THROWFLAG         = 4096;

  STATE_NONE        = 0;
  STATE_MENU        = 1;
  STATE_FOLD        = 2;
  STATE_INTERCUSTOM = 3;
  STATE_INTERSINGLE = 4;
  STATE_INTERTEXT   = 5;
  STATE_INTERPIC    = 6;
  STATE_ENDPIC      = 7;
  STATE_SLIST       = 8;

  LMS_RESPAWN_NONE   = 0;
  LMS_RESPAWN_WARMUP = 1;
  LMS_RESPAWN_FINAL  = 2;

  SPECT_NONE    = 0;
  SPECT_STATS   = 1;
  SPECT_MAPVIEW = 2;
  SPECT_PLAYERS = 3;

  DE_GLOBEVENT = 0;
  DE_BFGHIT    = 1;
  DE_KILLCOMBO = 2;
  DE_BODYKILL  = 3;

  ANNOUNCE_NONE   = 0;
  ANNOUNCE_ME     = 1;
  ANNOUNCE_MEPLUS = 2;
  ANNOUNCE_ALL    = 3;

  CONFIG_FILENAME = 'Doom2DF.cfg';

  TEST_MAP_NAME = '$$$_TEST_$$$';

  STD_PLAYER_MODEL = 'Doomer';

{$IFDEF HEADLESS}
  DEFAULT_PLAYERS = 0;
{$ELSE}
  DEFAULT_PLAYERS = 1;
{$ENDIF}

  STATFILE_VERSION = $03;

var
  gStdFont: DWORD;
  gGameSettings: TGameSettings;
  gPlayer1Settings: TPlayerSettings;
  gPlayer2Settings: TPlayerSettings;
  gGameOn: Boolean;
  gPlayerScreenSize: TDFPoint;
  gPlayer1ScreenCoord: TDFPoint;
  gPlayer2ScreenCoord: TDFPoint;
  gPlayer1: TPlayer = nil;
  gPlayer2: TPlayer = nil;
  gPlayerDrawn: TPlayer = nil;
  gTime: LongWord;
  gLerpFactor: Single = 1.0;
  gSwitchGameMode: Byte = GM_DM;
  gHearPoint1, gHearPoint2: THearPoint;
  gSoundEffectsDF: Boolean = False;
  gSoundTriggerTime: Word = 0;
  gAnnouncer: Integer = ANNOUNCE_NONE;
  goodsnd: array[0..3] of TPlayableSound;
  killsnd: array[0..3] of TPlayableSound;
  hahasnd: array[0..2] of TPlayableSound;
  sound_get_flag: array[0..1] of TPlayableSound;
  sound_lost_flag: array[0..1] of TPlayableSound;
  sound_ret_flag: array[0..1] of TPlayableSound;
  sound_cap_flag: array[0..1] of TPlayableSound;
  gBodyKillEvent: Integer = -1;
  gDefInterTime: ShortInt = -1;
  gInterEndTime: LongWord = 0;
  gInterTime: LongWord = 0;
  gServInterTime: Byte = 0;
  gGameStartTime: LongWord = 0;
  gTotalMonsters: Integer = 0;
  gPauseMain: Boolean = false;
  gPauseHolmes: Boolean = false;
  gShowTime: Boolean = False;
  gShowFPS: Boolean = False;
  gShowScore: Boolean = True;
  gShowStat: Boolean = True;
  gShowPIDs: Boolean = False;
  gShowKillMsg: Boolean = True;
  gShowLives: Boolean = True;
  gShowPing: Boolean = False;
  gShowMap: Boolean = False;
  gExit: Byte = 0;
  gState: Byte = STATE_NONE;
  sX, sY: Integer;
  sWidth, sHeight: Word;
  gSpectMode: Byte = SPECT_NONE;
  gSpectHUD: Boolean = True;
  gSpectKeyPress: Boolean = False;
  gSpectX: Integer = 0;
  gSpectY: Integer = 0;
  gSpectStep: Byte = 8;
  gSpectViewTwo: Boolean = False;
  gSpectPID1: Integer = -1;
  gSpectPID2: Integer = -1;
  gSpectAuto: Boolean = False;
  gSpectAutoNext: LongWord;
  gSpectAutoStepX: Integer;
  gSpectAutoStepY: Integer;
  gMusic: TMusic = nil;
  gLoadGameMode: Boolean;
  gCheats: Boolean = False;
  gMapOnce: Boolean = False;
  gMapToDelete: String;
  gTempDelete: Boolean = False;
  gLastMap: Boolean = False;
  gScreenWidth: Word;
  gScreenHeight: Word;
  gResolutionChange: Boolean = False;
  gRC_Width, gRC_Height: Integer;
  gRC_FullScreen, gRC_Maximized: Boolean;
  gLanguageChange: Boolean = False;
  gDebugMode: Boolean = False;
  g_debug_Sounds: Boolean = False;
  g_debug_Frames: Boolean = False;
  g_debug_WinMsgs: Boolean = False;
  g_debug_MonsterOff: Boolean = False;
  g_debug_BotAIOff: Byte = 0;
  g_debug_HealthBar: Boolean = False;
  g_Debug_Player: Boolean = False;
  gCoopMonstersKilled: Word = 0;
  gCoopSecretsFound: Word = 0;
  gCoopTotalMonstersKilled: Word = 0;
  gCoopTotalSecretsFound: Word = 0;
  gCoopTotalMonsters: Word = 0;
  gCoopTotalSecrets: Word = 0;
  gStatsOff: Boolean = False;
  gStatsPressed: Boolean = False;
  gExitByTrigger: Boolean = False;
  gNextMap: String = '';
  gLMSRespawn: Byte = LMS_RESPAWN_NONE;
  gLMSRespawnTime: Cardinal = 0;
  gLMSSoftSpawn: Boolean = False;
  gMissionFailed: Boolean = False;
  gVoteInProgress: Boolean = False;
  gVotePassed: Boolean = False;
  gVoteCommand: string = '';
  gVoteTimer: Cardinal = 0;
  gVoteCmdTimer: Cardinal = 0;
  gVoteCount: Integer = 0;
  gVoteTimeout: Cardinal = 30;
  gVoted: Boolean = False;
  gVotesEnabled: Boolean = True;
  gEvents: Array of TGameEvent;
  gDelayedEvents: Array of TDelayedEvent;
  gUseChatSounds: Boolean = True;
  gChatSounds: Array of TChatSound;
  gWeaponAction: Array [0..1, WP_FACT..WP_LACT] of Boolean; // [player, weapon_action]
  gSelectWeapon: Array [0..1, WP_FIRST..WP_LAST] of Boolean; // [player, weapon]
  gInterReadyCount: Integer = 0;
  gMaxBots: Integer = 127;

  g_dbg_ignore_bounds: Boolean = false;
  r_smallmap_h: Integer = 0; // 0: left; 1: center; 2: right
  r_smallmap_v: Integer = 2; // 0: top; 1: center; 2: bottom

  // move button values:
  // bits 0-1: l/r state:
  //   0: neither left, nor right pressed
  //   1: left pressed
  //   2: right pressed
  // bits 4-5: l/r state when strafe was pressed
  P1MoveButton: Byte = 0;
  P2MoveButton: Byte = 0;

  g_profile_frame_update: Boolean = false;
  g_profile_frame_draw: Boolean = false;
  g_profile_collision: Boolean = false;
  g_profile_los: Boolean = false;
  g_profile_history_size: Integer = 1000;

  g_rlayer_back: Boolean = true;
  g_rlayer_step: Boolean = true;
  g_rlayer_wall: Boolean = true;
  g_rlayer_door: Boolean = true;
  g_rlayer_acid1: Boolean = true;
  g_rlayer_acid2: Boolean = true;
  g_rlayer_water: Boolean = true;
  g_rlayer_fore: Boolean = true;


procedure g_ResetDynlights ();
procedure g_AddDynLight (x, y, radius: Integer; r, g, b, a: Single);
procedure g_DynLightExplosion (x, y, radius: Integer; r, g, b: Single);

function conIsCheatsEnabled (): Boolean; inline;
function gPause (): Boolean; inline;

  type (* private state *)
    TEndCustomGameStat = record
      PlayerStat: TPlayerStatArray;
      TeamStat: TTeamStat;
      GameTime: LongWord;
      GameMode: Byte;
      Map, MapName: String;
    end;

    TEndSingleGameStat = record
      PlayerStat: Array [0..1] of record
        Kills: Integer;
        Secrets: Integer;
      end;
      GameTime: LongWord;
      TwoPlayers: Boolean;
      TotalSecrets: Integer;
    end;

    TLoadingStat = record
      CurValue: Integer;
      MaxValue: Integer;
      ShowCount: Integer;
      Msgs: Array of String;
      NextMsg: Word;
      PBarWasHere: Boolean; // did we draw a progress bar for this message?
    end;

    TDynLight = record
      x, y, radius: Integer;
      r, g, b, a: Single;
      exploCount: Integer;
      exploRadius: Integer;
    end;

  var (* private state *)
    CustomStat: TEndCustomGameStat;
    StatShotDone: Boolean;
    StatFilename: string = ''; // used by stat screenshot to save with the same name as the csv
    SingleStat: TEndSingleGameStat;
    hasPBarGfx: Boolean;
    LoadingStat: TLoadingStat;
    MessageText: String;
    IsDrawStat: Boolean;
    EndingGameCounter: Byte;
    UPS: Word;
    g_playerLight: Boolean;
    g_dynLights: array of TDynLight = nil;
    g_dynLightCount: Integer = 0;

implementation

uses
{$INCLUDE ../nogl/noGLuses.inc}
{$IFDEF ENABLE_HOLMES}
  g_holmes,
{$ENDIF}
  e_texture, e_res, g_textures, g_window, g_menu,
  e_input, e_log, g_console, r_console, g_items, g_map, g_panel,
  g_playermodel, g_gfx, g_options, Math,
  g_triggers, g_monsters, e_sound, CONFIG,
  g_language, g_net, g_main, g_phys,
  ENet, e_msg, g_netmsg, g_netmaster,
  sfs, wadreader, g_system, r_playermodel;


// ////////////////////////////////////////////////////////////////////////// //
function gPause (): Boolean; inline; begin result := gPauseMain or gPauseHolmes; end;

procedure g_ResetDynlights ();
var
  lnum, idx: Integer;
begin
  if not gwin_has_stencil then begin g_dynLightCount := 0; exit; end;
  lnum := 0;
  for idx := 0 to g_dynLightCount-1 do
  begin
    if g_dynLights[idx].exploCount = -666 then
    begin
      // skip it
    end
    else
    begin
      // explosion
      Inc(g_dynLights[idx].exploCount);
      if (g_dynLights[idx].exploCount < 10) then
      begin
        g_dynLights[idx].radius := g_dynLights[idx].exploRadius+g_dynLights[idx].exploCount*8;
        g_dynLights[idx].a := 0.4+g_dynLights[idx].exploCount/10;
        if (g_dynLights[idx].a > 0.8) then g_dynLights[idx].a := 0.8;
        if lnum <> idx then g_dynLights[lnum] := g_dynLights[idx];
        Inc(lnum);
      end;
    end;
  end;
  g_dynLightCount := lnum;
end;

procedure g_AddDynLight (x, y, radius: Integer; r, g, b, a: Single);
begin
  if not gwin_has_stencil then exit;
  if g_dynLightCount = length(g_dynLights) then SetLength(g_dynLights, g_dynLightCount+1024);
  g_dynLights[g_dynLightCount].x := x;
  g_dynLights[g_dynLightCount].y := y;
  g_dynLights[g_dynLightCount].radius := radius;
  g_dynLights[g_dynLightCount].r := r;
  g_dynLights[g_dynLightCount].g := g;
  g_dynLights[g_dynLightCount].b := b;
  g_dynLights[g_dynLightCount].a := a;
  g_dynLights[g_dynLightCount].exploCount := -666;
  Inc(g_dynLightCount);
end;

procedure g_DynLightExplosion (x, y, radius: Integer; r, g, b: Single);
begin
  if not gwin_has_stencil then exit;
  if g_dynLightCount = length(g_dynLights) then SetLength(g_dynLights, g_dynLightCount+1024);
  g_dynLights[g_dynLightCount].x := x;
  g_dynLights[g_dynLightCount].y := y;
  g_dynLights[g_dynLightCount].radius := 0;
  g_dynLights[g_dynLightCount].exploRadius := radius;
  g_dynLights[g_dynLightCount].r := r;
  g_dynLights[g_dynLightCount].g := g;
  g_dynLights[g_dynLightCount].b := b;
  g_dynLights[g_dynLightCount].a := 0;
  g_dynLights[g_dynLightCount].exploCount := 0;
  Inc(g_dynLightCount);
end;

// ////////////////////////////////////////////////////////////////////////// //
function conIsCheatsEnabled (): Boolean; inline;
begin
  result := false;
  if g_Game_IsNet then exit;
  if not gDebugMode then
  begin
    //if not gCheats then exit;
    if not (gGameSettings.GameType in [GT_SINGLE, GT_CUSTOM]) then exit;
    if not (gGameSettings.GameMode in [GM_COOP, GM_SINGLE]) then exit;
  end;
  result := true;
end;

// ////////////////////////////////////////////////////////////////////////// //
type
  TParamStrValue = record
    Name: String;
    Value: String;
  end;

  TParamStrValues = Array of TParamStrValue;

const
  INTER_ACTION_TEXT = 1;
  INTER_ACTION_PIC = 2;
  INTER_ACTION_MUSIC = 3;

var
  UPSCounter: Word;
  UPSTime: LongWord;
  DataLoaded: Boolean = False;
  MessageTime: Word;
  MessageLineLength: Integer = 80;
  MapList: SSArray = nil;
  MapIndex: Integer = -1;
  InterReadyTime: Integer = -1;
  StatDate: string = '';
  MegaWAD: record
    info: TMegaWADInfo;
    endpic: String;
    endmus: String;
    res: record
      text: Array of ShortString;
      anim: Array of ShortString;
      pic: Array of ShortString;
      mus: Array of ShortString;
    end;
    triggers: Array of record
      event: ShortString;
      actions: Array of record
        action, p1, p2: Integer;
      end;
    end;
    cur_trigger: Integer;
    cur_action: Integer;
  end;
  //InterPic: String;
  InterText: record
    lines: SSArray;
    img: String;
    cur_line: Integer;
    cur_char: Integer;
    counter: Integer;
    endtext: Boolean;
  end;

function Compare(a, b: TPlayerStat): Integer;
begin
  if a.Spectator then Result := 1
    else if b.Spectator then Result := -1
      else if a.Frags < b.Frags then Result := 1
        else if a.Frags > b.Frags then Result := -1
          else if a.Deaths < b.Deaths then Result := -1
            else if a.Deaths > b.Deaths then Result := 1
              else if a.Kills < b.Kills then Result := -1
                else Result := 1;
end;

procedure SortGameStat(var stat: TPlayerStatArray);
var
  I, J: Integer;
  T: TPlayerStat;
begin
  if stat = nil then Exit;

  for I := High(stat) downto Low(stat) do
    for J := Low(stat) to High(stat) - 1 do
      if Compare(stat[J], stat[J + 1]) = 1  then
      begin
        T := stat[J];
        stat[J] := stat[J + 1];
        stat[J + 1] := T;
      end;
end;

// saves a shitty CSV containing the game stats passed to it
procedure SaveGameStat(Stat: TEndCustomGameStat; Path: string);
var 
  s: TextFile;
  dir, fname, map, mode, etime: String;
  I: Integer;
begin
  try
    dir := e_GetWriteableDir(StatsDirs);
    // stats are placed in stats/yy/mm/dd/*.csv
    fname := e_CatPath(dir, Path);
    ForceDirectories(fname); // ensure yy/mm/dd exists within the stats dir
    fname := e_CatPath(fname, StatFilename + '.csv');
    AssignFile(s, fname);
    try
      SetTextCodePage(s, CP_UTF8);
      Rewrite(s);
      // line 1: stats ver, datetime, server name, map name, game mode, time limit, score limit, dmflags, game time, num players
      if g_Game_IsNet then fname := NetServerName else fname := '';
      map := g_ExtractWadNameNoPath(gMapInfo.Map) + ':/' + g_ExtractFileName(gMapInfo.Map);
      mode := g_Game_ModeToText(Stat.GameMode);
      etime := Format('%d:%.2d:%.2d', [
        Stat.GameTime div 1000 div 3600,
        (Stat.GameTime div 1000 div 60) mod 60,
        Stat.GameTime div 1000 mod 60
      ]);
      WriteLn(s, 'stats_ver,datetime,server,map,mode,timelimit,scorelimit,dmflags,time,num_players');
      WriteLn(s, Format('%d,%s,%s,%s,%s,%u,%u,%u,%s,%d', [
        STATFILE_VERSION,
        StatDate,
        dquoteStr(fname),
        dquoteStr(map),
        mode,
        gGameSettings.TimeLimit,
        gGameSettings.ScoreLimit,
        gGameSettings.Options,
        etime,
        Length(Stat.PlayerStat)
      ]));
      // line 2: game specific shit
      //   if it's a team game: red score, blue score
      //   if it's a coop game: monsters killed, monsters total, secrets found, secrets total
      //   otherwise nothing
      if Stat.GameMode in [GM_TDM, GM_CTF] then
        WriteLn(s, 
          Format('red_score,blue_score' + LineEnding + '%d,%d', [Stat.TeamStat[TEAM_RED].Score, Stat.TeamStat[TEAM_BLUE].Score]))
      else if Stat.GameMode in [GM_COOP, GM_SINGLE] then
        WriteLn(s,
          Format('mon_killed,mon_total,secrets_found,secrets_total' + LineEnding + '%d,%d,%d,%d',[gCoopMonstersKilled, gTotalMonsters, gCoopSecretsFound, gSecretsCount]));
      // lines 3-...: team, player name, frags, deaths
      WriteLn(s, 'team,name,frags,deaths');
      for I := Low(Stat.PlayerStat) to High(Stat.PlayerStat) do
        with Stat.PlayerStat[I] do
          WriteLn(s, Format('%d,%s,%d,%d', [Team, dquoteStr(Name), Frags, Deaths]));
    except
      g_Console_Add(Format(_lc[I_CONSOLE_ERROR_WRITE], [fname]));
    end;
  except
    g_Console_Add('could not create gamestats file "' + fname + '"');
  end;
  CloseFile(s);
end;

procedure ClearDebugCvars();
begin
  g_debug_Sounds := False;
  g_debug_Frames := False;
  g_debug_WinMsgs := False;
  g_debug_MonsterOff := False;
  g_debug_BotAIOff := 0;
  g_debug_HealthBar := False;
  g_Debug_Player := False;
end;

function g_Game_ModeToText(Mode: Byte): string;
begin
  Result := '';
  case Mode of
    GM_DM:   Result := _lc[I_MENU_GAME_TYPE_DM];
    GM_TDM:  Result := _lc[I_MENU_GAME_TYPE_TDM];
    GM_CTF:  Result := _lc[I_MENU_GAME_TYPE_CTF];
    GM_COOP: Result := _lc[I_MENU_GAME_TYPE_COOP];
    GM_SINGLE: Result := _lc[I_MENU_GAME_TYPE_SINGLE];
  end;
end;

function g_Game_TextToMode(Mode: string): Byte;
begin
  Result := GM_NONE;
  Mode := UpperCase(Mode);
  if Mode = _lc[I_MENU_GAME_TYPE_DM] then
  begin
    Result := GM_DM;
    Exit;
  end;
  if Mode = _lc[I_MENU_GAME_TYPE_TDM] then
  begin
    Result := GM_TDM;
    Exit;
  end;
  if Mode = _lc[I_MENU_GAME_TYPE_CTF] then
  begin
    Result := GM_CTF;
    Exit;
  end;
  if Mode = _lc[I_MENU_GAME_TYPE_COOP] then
  begin
    Result := GM_COOP;
    Exit;
  end;
  if Mode = _lc[I_MENU_GAME_TYPE_SINGLE] then
  begin
    Result := GM_SINGLE;
    Exit;
  end;
end;

function g_Game_IsNet(): Boolean;
begin
  Result := (gGameSettings.GameType in [GT_SERVER, GT_CLIENT]);
end;

function g_Game_IsServer(): Boolean;
begin
  Result := (gGameSettings.GameType in [GT_SINGLE, GT_CUSTOM, GT_SERVER]);
end;

function g_Game_IsClient(): Boolean;
begin
  Result := (gGameSettings.GameType = GT_CLIENT);
end;

function g_Game_GetMegaWADInfo(WAD: String): TMegaWADInfo;
var
  w: TWADFile;
  cfg: TConfig;
  p: Pointer;
  len: Integer;
begin
  Result.name := ExtractFileName(WAD);
  Result.description := '';
  Result.author := '';

  w := TWADFile.Create();
  w.ReadFile(WAD);

  if not w.GetResource('INTERSCRIPT', p, len) then
  begin
    w.Free();
    Exit;
  end;

  cfg := TConfig.CreateMem(p, len);
  Result.name := cfg.ReadStr('megawad', 'name', ExtractFileName(WAD));
  Result.description := cfg.ReadStr('megawad', 'description', '');
  Result.author := cfg.ReadStr('megawad', 'author', '');
  Result.pic := cfg.ReadStr('megawad', 'pic', '');
  cfg.Free();

  FreeMem(p);
end;

procedure g_Game_FreeWAD();
var
  a: Integer;
begin
  for a := 0 to High(MegaWAD.res.pic) do
    if MegaWAD.res.pic[a] <> '' then
      g_Texture_Delete(MegaWAD.res.pic[a]);

  for a := 0 to High(MegaWAD.res.mus) do
    if MegaWAD.res.mus[a] <> '' then
      g_Sound_Delete(MegaWAD.res.mus[a]);

  MegaWAD.res.pic := nil;
  MegaWAD.res.text := nil;
  MegaWAD.res.anim := nil;
  MegaWAD.res.mus := nil;
  MegaWAD.triggers := nil;

  g_Texture_Delete('TEXTURE_endpic');
  g_Sound_Delete('MUSIC_endmus');

  ZeroMemory(@MegaWAD, SizeOf(MegaWAD));
  gGameSettings.WAD := '';
end;

procedure g_Game_LoadWAD(WAD: string);
var
  w: TWADFile;
  cfg: TConfig;
  p: Pointer;
  {b, }len: Integer;
  s: AnsiString;
begin
  g_Game_FreeWAD();
  gGameSettings.WAD := WAD;
  if not (gGameSettings.GameMode in [GM_COOP, GM_SINGLE]) then
    Exit;

  MegaWAD.info := g_Game_GetMegaWADInfo(WAD);

  w := TWADFile.Create();
  w.ReadFile(WAD);

  if not w.GetResource('INTERSCRIPT', p, len) then
  begin
    w.Free();
    Exit;
  end;

  cfg := TConfig.CreateMem(p, len);

 {b := 1;
 while True do
 begin
  s := cfg.ReadStr('pic', 'pic'+IntToStr(b), '');
  if s = '' then Break;
  b := b+1;

  SetLength(MegaWAD.res.pic, Length(MegaWAD.res.pic)+1);
  MegaWAD.res.pic[High(MegaWAD.res.pic)] := s;

  g_Texture_CreateWADEx(s, s);
 end;

 b := 1;
 while True do
 begin
  s := cfg.ReadStr('mus', 'mus'+IntToStr(b), '');
  if s = '' then Break;
  b := b+1;

  SetLength(MegaWAD.res.mus, Length(MegaWAD.res.mus)+1);
  MegaWAD.res.mus[High(MegaWAD.res.mus)] := s;

  g_Music_CreateWADEx(s, s);
 end;}

  MegaWAD.endpic := cfg.ReadStr('megawad', 'endpic', '');
  if MegaWAD.endpic <> '' then
  begin
    TEXTUREFILTER := GL_LINEAR;
    s := e_GetResourcePath(WadDirs, MegaWAD.endpic, WAD);
    g_Texture_CreateWADEx('TEXTURE_endpic', s);
    TEXTUREFILTER := GL_NEAREST;
  end;
  MegaWAD.endmus := cfg.ReadStr('megawad', 'endmus', 'Standart.wad:D2DMUS\КОНЕЦ');
  if MegaWAD.endmus <> '' then
  begin
    s := e_GetResourcePath(WadDirs, MegaWAD.endmus, WAD);
    g_Sound_CreateWADEx('MUSIC_endmus', s, True);
  end;

  cfg.Free();
  FreeMem(p);
  w.Free();
end;

{procedure start_trigger(t: string);
begin
end;

function next_trigger(): Boolean;
begin
end;}

procedure DisableCheats();
begin
  MAX_RUNVEL := 8;
  VEL_JUMP := 10;
  gFly := False;

  if gPlayer1 <> nil then gPlayer1.GodMode := False;
  if gPlayer2 <> nil then gPlayer2.GodMode := False;
  if gPlayer1 <> nil then gPlayer1.NoTarget := False;
  if gPlayer2 <> nil then gPlayer2.NoTarget := False;

  {$IF DEFINED(D2F_DEBUG)}
  if gPlayer1 <> nil then gPlayer1.NoTarget := True;
  gAimLine := g_dbg_aimline_on;
  {$ENDIF}
end;

procedure g_Game_ExecuteEvent(Name: String);
var
  a: Integer;
begin
  if Name = '' then
    Exit;
  if gEvents = nil then
    Exit;
  for a := 0 to High(gEvents) do
    if gEvents[a].Name = Name then
    begin
      if gEvents[a].Command <> '' then
        g_Console_Process(gEvents[a].Command, True);
      break;
    end;
end;

function g_Game_DelayEvent(DEType: Byte; Time: LongWord; Num: Integer = 0; Str: String = ''): Integer;
var
  a, n: Integer;
begin
  n := -1;
  if gDelayedEvents <> nil then
    for a := 0 to High(gDelayedEvents) do
      if not gDelayedEvents[a].Pending then
      begin
        n := a;
        break;
      end;
  if n = -1 then
  begin
    SetLength(gDelayedEvents, Length(gDelayedEvents) + 1);
    n := High(gDelayedEvents);
  end;
  gDelayedEvents[n].Pending := True;
  gDelayedEvents[n].DEType := DEType;
  gDelayedEvents[n].DENum := Num;
  gDelayedEvents[n].DEStr := Str;
  if DEType = DE_GLOBEVENT then
    gDelayedEvents[n].Time := (sys_GetTicks() {div 1000}) + Time
  else
    gDelayedEvents[n].Time := gTime + Time;
  Result := n;
end;

procedure EndGame();
var
  a: Integer;
  FileName: string;
  t: TDateTime;
begin
  if g_Game_IsNet and g_Game_IsServer then
    MH_SEND_GameEvent(NET_EV_MAPEND, Byte(gMissionFailed));

// Стоп игра:
  gPauseMain := false;
  gPauseHolmes := false;
  gGameOn := false;

  g_Game_StopAllSounds(False);

  MessageTime := 0;
  MessageText := '';

  EndingGameCounter := 0;
  g_ActiveWindow := nil;

  gLMSRespawn := LMS_RESPAWN_NONE;
  gLMSRespawnTime := 0;

  case gExit of
    EXIT_SIMPLE: // Выход через меню или конец теста
      begin
        g_Game_Free();

        if gMapOnce  then
          begin // Это был тест
            g_Game_Quit();
          end
        else
          begin // Выход в главное меню
            gMusic.SetByName('MUSIC_MENU');
            gMusic.Play();
            if gState <> STATE_SLIST then
            begin
              g_GUI_ShowWindow('MainMenu');
              gState := STATE_MENU;
            end else
            begin
              // Обновляем список серверов
              slReturnPressed := True;
              if g_Net_Slist_Fetch(slCurrent) then
              begin
                if slCurrent = nil then
                  slWaitStr := _lc[I_NET_SLIST_NOSERVERS];
              end
              else
                slWaitStr := _lc[I_NET_SLIST_ERROR];
              g_Serverlist_GenerateTable(slCurrent, slTable);
            end;

            g_Game_ExecuteEvent('ongameend');
          end;
      end;

    EXIT_RESTART: // Начать уровень сначала
      begin
        if not g_Game_IsClient then g_Game_Restart();
      end;

    EXIT_ENDLEVELCUSTOM: // Закончился уровень в Своей игре
      begin
      // Статистика Своей игры:
        FileName := g_ExtractWadName(gMapInfo.Map);

        CustomStat.GameTime := gTime;
        CustomStat.Map := ExtractFileName(FileName)+':'+g_ExtractFileName(gMapInfo.Map); //ResName;
        CustomStat.MapName := gMapInfo.Name;
        CustomStat.GameMode := gGameSettings.GameMode;
        if gGameSettings.GameMode in [GM_TDM, GM_CTF] then
          CustomStat.TeamStat := gTeamStat;

        CustomStat.PlayerStat := nil;

      // Статистика игроков:
        if gPlayers <> nil then
        begin
          for a := 0 to High(gPlayers) do
            if gPlayers[a] <> nil then
            begin
              SetLength(CustomStat.PlayerStat, Length(CustomStat.PlayerStat)+1);
              with CustomStat.PlayerStat[High(CustomStat.PlayerStat)] do
              begin
                Num := a;
                Name := gPlayers[a].Name;
                Frags := gPlayers[a].Frags;
                Deaths := gPlayers[a].Death;
                Kills := gPlayers[a].Kills;
                Team := gPlayers[a].Team;
                Color := gPlayers[a].Model.Color;
                Spectator := gPlayers[a].FSpectator;
              end;
            end;

          SortGameStat(CustomStat.PlayerStat);

          if (gSaveStats or gScreenshotStats) and (Length(CustomStat.PlayerStat) > 1) then
          begin
            t := Now;
            if g_Game_IsNet then StatFilename := NetServerName else StatFilename := 'local';
            StatDate := FormatDateTime('yymmdd_hhnnss', t);
            StatFilename := StatFilename + '_' + CustomStat.Map + '_' + g_Game_ModeToText(CustomStat.GameMode);
            StatFilename := sanitizeFilename(StatFilename) + '_' + StatDate;
            if gSaveStats then
              SaveGameStat(CustomStat, FormatDateTime('yyyy"/"mm"/"dd', t));
          end;

          StatShotDone := False;
        end;

        g_Game_ExecuteEvent('onmapend');
        if not g_Game_IsClient then g_Player_ResetReady;
        gInterReadyCount := 0;

      // Затухающий экран:
        EndingGameCounter := 255;
        gState := STATE_FOLD;
        gInterTime := 0;
        if gDefInterTime < 0 then
          gInterEndTime := IfThen((gGameSettings.GameType = GT_SERVER) and (gPlayer1 = nil), 15000, 25000)
        else
          gInterEndTime := gDefInterTime * 1000;
      end;

    EXIT_ENDLEVELSINGLE: // Закончился уровень в Одиночной игре
      begin
      // Статистика Одиночной игры:
        SingleStat.GameTime := gTime;
        SingleStat.TwoPlayers := gPlayer2 <> nil;
        SingleStat.TotalSecrets := gSecretsCount;
      // Статистика первого игрока:
        SingleStat.PlayerStat[0].Kills := gPlayer1.MonsterKills;
        SingleStat.PlayerStat[0].Secrets := gPlayer1.Secrets;
      // Статистика второго игрока (если есть):
        if SingleStat.TwoPlayers then
        begin
          SingleStat.PlayerStat[1].Kills := gPlayer2.MonsterKills;
          SingleStat.PlayerStat[1].Secrets := gPlayer2.Secrets;
        end;

        g_Game_ExecuteEvent('onmapend');

      // Есть еще карты:
        if gNextMap <> '' then
          begin
            gMusic.SetByName('MUSIC_INTERMUS');
            gMusic.Play();
            gState := STATE_INTERSINGLE;
            e_UnpressAllKeys();

            g_Game_ExecuteEvent('oninter');
          end
        else // Больше нет карт
          begin
          // Затухающий экран:
            EndingGameCounter := 255;
            gState := STATE_FOLD;
          end;
      end;
  end;

// Окончание обработано:
  if gExit <> EXIT_QUIT then
    gExit := 0;
end;

procedure g_Game_Init();
var
  SR: TSearchRec;
  knownFiles: array of AnsiString = nil;
  found: Boolean;
  wext, s: AnsiString;
  f: Integer;
begin
  gExit := 0;
  gMapToDelete := '';
  gTempDelete := False;

  sfsGCDisable(); // temporary disable removing of temporary volumes

  try
    TEXTUREFILTER := GL_LINEAR;
    g_Texture_CreateWADEx('MENU_BACKGROUND', GameWAD+':TEXTURES\TITLE');
    g_Texture_CreateWADEx('INTER', GameWAD+':TEXTURES\INTER');
    g_Texture_CreateWADEx('ENDGAME_EN', GameWAD+':TEXTURES\ENDGAME_EN');
    g_Texture_CreateWADEx('ENDGAME_RU', GameWAD+':TEXTURES\ENDGAME_RU');
    TEXTUREFILTER := GL_NEAREST;

    LoadStdFont('STDTXT', 'STDFONT', gStdFont);
    LoadFont('MENUTXT', 'MENUFONT', gMenuFont);
    LoadFont('SMALLTXT', 'SMALLFONT', gMenuSmallFont);

    g_Game_ClearLoading();
    g_Game_SetLoadingText(Format('Doom 2D: Forever %s', [GAME_VERSION]), 0, False);
    g_Game_SetLoadingText('', 0, False);

    g_Game_SetLoadingText(_lc[I_LOAD_CONSOLE], 0, False);
    r_Console_Init;
    g_Console_Init();

    g_Game_SetLoadingText(_lc[I_LOAD_MODELS], 0, False);
    r_PlayerModel_Initialize;

    // load models from all possible wad types, in all known directories
    // this does a loosy job (linear search, ooph!), but meh
    for wext in wadExtensions do
    begin
      for f := High(ModelDirs) downto Low(ModelDirs) do
      begin
        if (FindFirst(ModelDirs[f]+DirectorySeparator+'*'+wext, faAnyFile, SR) = 0) then
        begin
          repeat
            found := false;
            for s in knownFiles do
            begin
              if (strEquCI1251(forceFilenameExt(SR.Name, ''), forceFilenameExt(ExtractFileName(s), ''))) then
              begin
                found := true;
                break;
              end;
            end;
            if not found then
            begin
              SetLength(knownFiles, length(knownFiles)+1);
              knownFiles[High(knownFiles)] := ModelDirs[f]+DirectorySeparator+SR.Name;
            end;
          until (FindNext(SR) <> 0);
        end;
        FindClose(SR);
      end;
    end;

    if (length(knownFiles) = 0) then raise Exception.Create('no player models found!');

    if (length(knownFiles) = 1) then e_LogWriteln('1 player model found.', TMsgType.Notify) else e_LogWritefln('%d player models found.', [Integer(length(knownFiles))], TMsgType.Notify);
    for s in knownFiles do
    begin
      if not g_PlayerModel_Load(s) then e_LogWritefln('Error loading model "%s"', [s], TMsgType.Warning);
    end;

    gGameOn := false;
    gPauseMain := false;
    gPauseHolmes := false;
    gTime := 0;

    {e_MouseInfo.Accel := 1.0;}

    g_Game_SetLoadingText(_lc[I_LOAD_GAME_DATA], 0, False);
    g_Game_LoadData();

    g_Game_SetLoadingText(_lc[I_LOAD_MUSIC], 0, False);
    g_Sound_CreateWADEx('MUSIC_INTERMUS', GameWAD+':MUSIC\INTERMUS', True);
    g_Sound_CreateWADEx('MUSIC_MENU', GameWAD+':MUSIC\MENU', True);
    g_Sound_CreateWADEx('MUSIC_ROUNDMUS', GameWAD+':MUSIC\ROUNDMUS', True, True);
    g_Sound_CreateWADEx('MUSIC_STDENDMUS', GameWAD+':MUSIC\ENDMUS', True);

{$IFNDEF HEADLESS}
    g_Game_SetLoadingText(_lc[I_LOAD_MENUS], 0, False);
    g_Menu_Init();
{$ENDIF}

    gMusic := TMusic.Create();
    gMusic.SetByName('MUSIC_MENU');
    gMusic.Play();

    gGameSettings.WarmupTime := 30;

    gState := STATE_MENU;

    SetLength(gEvents, 6);
    gEvents[0].Name := 'ongamestart';
    gEvents[1].Name := 'ongameend';
    gEvents[2].Name := 'onmapstart';
    gEvents[3].Name := 'onmapend';
    gEvents[4].Name := 'oninter';
    gEvents[5].Name := 'onwadend';
  finally
    sfsGCEnable(); // enable releasing unused volumes
  end;
end;

procedure g_Game_Free(freeTextures: Boolean=true);
begin
  if NetMode = NET_CLIENT then g_Net_Disconnect();
  if NetMode = NET_SERVER then g_Net_Host_Die();

  g_Map_Free(freeTextures);
  g_Player_Free();
  g_Player_RemoveAllCorpses();

  gGameSettings.GameType := GT_NONE;
  if gGameSettings.GameMode = GM_SINGLE then
    gGameSettings.GameMode := GM_DM;
  gSwitchGameMode := gGameSettings.GameMode;

  gChatShow := False;
  gExitByTrigger := False;
end;

function IsActivePlayer(p: TPlayer): Boolean;
begin
  Result := False;
  if p = nil then
    Exit;
  Result := (not p.FDummy) and (not p.FSpectator);
end;

function GetActivePlayerID_Next(Skip: Integer = -1): Integer;
var
  a, idx: Integer;
  ids: Array of Word;
begin
  Result := -1;
  if gPlayers = nil then
    Exit;
  SetLength(ids, 0);
  idx := -1;
  for a := Low(gPlayers) to High(gPlayers) do
    if IsActivePlayer(gPlayers[a]) then
    begin
      SetLength(ids, Length(ids) + 1);
      ids[High(ids)] := gPlayers[a].UID;
      if gPlayers[a].UID = Skip then
        idx := High(ids);
    end;
  if Length(ids) = 0 then
    Exit;
  if idx = -1 then
    Result := ids[0]
  else
    Result := ids[(idx + 1) mod Length(ids)];
end;

function GetActivePlayerID_Prev(Skip: Integer = -1): Integer;
var
  a, idx: Integer;
  ids: Array of Word;
begin
  Result := -1;
  if gPlayers = nil then
    Exit;
  SetLength(ids, 0);
  idx := -1;
  for a := Low(gPlayers) to High(gPlayers) do
    if IsActivePlayer(gPlayers[a]) then
    begin
      SetLength(ids, Length(ids) + 1);
      ids[High(ids)] := gPlayers[a].UID;
      if gPlayers[a].UID = Skip then
        idx := High(ids);
    end;
  if Length(ids) = 0 then
    Exit;
  if idx = -1 then
    Result := ids[Length(ids) - 1]
  else
    Result := ids[(Length(ids) - 1 + idx) mod Length(ids)];
end;

function GetActivePlayerID_Random(Skip: Integer = -1): Integer;
var
  a, idx: Integer;
  ids: Array of Word;
begin
  Result := -1;
  if gPlayers = nil then
    Exit;
  SetLength(ids, 0);
  idx := -1;
  for a := Low(gPlayers) to High(gPlayers) do
    if IsActivePlayer(gPlayers[a]) then
    begin
      SetLength(ids, Length(ids) + 1);
      ids[High(ids)] := gPlayers[a].UID;
      if gPlayers[a].UID = Skip then
        idx := High(ids);
    end;
  if Length(ids) = 0 then
    Exit;
  if Length(ids) = 1 then
  begin
    Result := ids[0];
    Exit;
  end;
  Result := ids[Random(Length(ids))];
  a := 10;
  while (idx <> -1) and (Result = Skip) and (a > 0) do
  begin
    Result := ids[Random(Length(ids))];
    Dec(a);
  end;
end;

function GetRandomSpectMode(Current: Byte): Byte;
label
  retry;
begin
  Result := Current;
retry:
  case Random(7) of
    0: Result := SPECT_STATS;
    1: Result := SPECT_MAPVIEW;
    2: Result := SPECT_MAPVIEW;
    3: Result := SPECT_PLAYERS;
    4: Result := SPECT_PLAYERS;
    5: Result := SPECT_PLAYERS;
    6: Result := SPECT_PLAYERS;
  end;
  if (Current in [SPECT_STATS, SPECT_MAPVIEW]) and (Current = Result) then
    goto retry;
end;

procedure ProcessPlayerControls (plr: TPlayer; p: Integer; var MoveButton: Byte);
  var
    time: Word;
    strafeDir: Byte;
    i: Integer;
begin
  if (plr = nil) then exit;
  if (p = 2) then time := 1000 else time := 1;
  strafeDir := MoveButton shr 4;
  MoveButton := MoveButton and $0F;

  if gPlayerAction[p, ACTION_MOVELEFT] and (not gPlayerAction[p, ACTION_MOVERIGHT]) then
    MoveButton := 1 // Нажата только "Влево"
  else if (not gPlayerAction[p, ACTION_MOVELEFT]) and gPlayerAction[p, ACTION_MOVERIGHT] then
    MoveButton := 2 // Нажата только "Вправо"
  else if (not gPlayerAction[p, ACTION_MOVELEFT]) and (not gPlayerAction[p, ACTION_MOVERIGHT]) then
    MoveButton := 0; // Не нажаты ни "Влево", ни "Вправо"

  // Сейчас или раньше были нажаты "Влево"/"Вправо" => передаем игроку:
  if MoveButton = 1 then
    plr.PressKey(KEY_LEFT, time)
  else if MoveButton = 2 then
    plr.PressKey(KEY_RIGHT, time);

  // if we have "strafe" key, turn off old strafe mechanics
  if gPlayerAction[p, ACTION_STRAFE] then
  begin
    // new strafe mechanics
    if (strafeDir = 0) then
      strafeDir := MoveButton; // start strafing
    // now set direction according to strafe (reversed)
    if (strafeDir = 2) then
      plr.SetDirection(TDirection.D_LEFT)
    else if (strafeDir = 1) then
      plr.SetDirection(TDirection.D_RIGHT)
  end
  else
  begin
    strafeDir := 0; // not strafing anymore
    // Раньше была нажата "Вправо", а сейчас "Влево" => бежим вправо, смотрим влево:
    if (MoveButton = 2) and gPlayerAction[p, ACTION_MOVELEFT] then
      plr.SetDirection(TDirection.D_LEFT)
    // Раньше была нажата "Влево", а сейчас "Вправо" => бежим влево, смотрим вправо:
    else if (MoveButton = 1) and gPlayerAction[p, ACTION_MOVERIGHT] then
      plr.SetDirection(TDirection.D_RIGHT)
    // Что-то было нажато и не изменилось => куда бежим, туда и смотрим:
    else if MoveButton <> 0 then
      plr.SetDirection(TDirection(MoveButton-1))
  end;

  // fix movebutton state
  MoveButton := MoveButton or (strafeDir shl 4);

  // Остальные клавиши:
  if gPlayerAction[p, ACTION_JUMP] then plr.PressKey(KEY_JUMP, time);
  if gPlayerAction[p, ACTION_LOOKUP] then plr.PressKey(KEY_UP, time);
  if gPlayerAction[p, ACTION_LOOKDOWN] then plr.PressKey(KEY_DOWN, time);
  if gPlayerAction[p, ACTION_ATTACK] then plr.PressKey(KEY_FIRE);
  if gPlayerAction[p, ACTION_ACTIVATE] then plr.PressKey(KEY_OPEN);

  for i := WP_FACT to WP_LACT do
  begin
    if gWeaponAction[p, i] then
    begin
      plr.ProcessWeaponAction(i);
      gWeaponAction[p, i] := False
    end
  end;

  for i := WP_FIRST to WP_LAST do
  begin
    if gSelectWeapon[p, i] then
    begin
      plr.QueueWeaponSwitch(i); // all choices are passed there, and god will take the best
      gSelectWeapon[p, i] := False
    end
  end;

  // HACK: add dynlight here
  if gwin_k8_enable_light_experiments then
  begin
    if e_KeyPressed(IK_F8) and gGameOn and (not gConsoleShow) and (g_ActiveWindow = nil) then
    begin
      g_playerLight := true;
    end;
    if e_KeyPressed(IK_F9) and gGameOn and (not gConsoleShow) and (g_ActiveWindow = nil) then
    begin
      g_playerLight := false;
    end;
  end;

  if gwin_has_stencil and g_playerLight then g_AddDynLight(plr.GameX+32, plr.GameY+40, 128, 1, 1, 0, 0.6);
end;

// HACK: don't have a "key was pressed" function
procedure InterReady();
begin
  if InterReadyTime > gTime then Exit;
  InterReadyTime := gTime + 3000;
  MC_SEND_CheatRequest(NET_CHEAT_READY);
end;

procedure g_Game_PreUpdate();
begin
  // these are in separate PreUpdate functions because they can interact during Update()
  // and are synced over the net
  // we don't care that much about corpses and gibs
  g_Player_PreUpdate();
  g_Monsters_PreUpdate();
  g_Items_PreUpdate();
  g_Weapon_PreUpdate();
end;

procedure g_Game_Update();
var
  Msg: g_gui.TMessage;
  Time: Int64;
  a: Byte;
  w: Word;
  i, b: Integer;

  function sendMonsPos (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    // this will also reset "need-send" flag
    if mon.gncNeedSend then
    begin
      MH_SEND_MonsterPos(mon.UID);
    end
    else if (mon.MonsterType = MONSTER_BARREL) then
    begin
      if (mon.GameVelX <> 0) or (mon.GameVelY <> 0) then MH_SEND_MonsterPos(mon.UID);
    end
    else if (mon.MonsterState <> MONSTATE_SLEEP) then
    begin
      if (mon.MonsterState <> MONSTATE_DEAD) or (mon.GameVelX <> 0) or (mon.GameVelY <> 0) then MH_SEND_MonsterPos(mon.UID);
    end;
  end;

  function sendMonsPosUnexpected (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    // this will also reset "need-send" flag
    if mon.gncNeedSend then MH_SEND_MonsterPos(mon.UID);
  end;

  function sendItemPos (it: PItem): Boolean;
  begin
    result := false; // don't stop
    if it.needSend then
    begin
      MH_SEND_ItemPos(it.myId);
      it.needSend := False;
    end;
  end;

var
  reliableUpdate: Boolean;
begin
  g_ResetDynlights();
  framePool.reset();

// Пора выключать игру:
  if gExit = EXIT_QUIT then
    Exit;
// Игра закончилась - обрабатываем:
  if gExit <> 0 then
  begin
    EndGame();
    if gExit = EXIT_QUIT then
      Exit;
  end;

  // Читаем клавиатуру и джойстик, если окно активно
  // no need to, as we'll do it in event handler

// Обновляем консоль (движение и сообщения):
  r_Console_Update;
  g_Console_Update();

  if (NetMode = NET_NONE) and (g_Game_IsNet) and (gGameOn or (gState in [STATE_FOLD, STATE_INTERCUSTOM])) then
  begin
    gExit := EXIT_SIMPLE;
    EndGame();
    Exit;
  end;

  // process master server communications
  g_Net_Slist_Pulse();

  case gState of
    STATE_INTERSINGLE, // Статистка после прохождения уровня в Одиночной игре
    STATE_INTERCUSTOM, // Статистка после прохождения уровня в Своей игре
    STATE_INTERTEXT, // Текст между уровнями
    STATE_INTERPIC: // Картинка между уровнями
      begin
        if g_Game_IsNet and g_Game_IsServer then
        begin
          gInterTime := gInterTime + GAME_TICK;
          a := Min((gInterEndTime - gInterTime) div 1000 + 1, 255);
          if a <> gServInterTime then
          begin
            gServInterTime := a;
            MH_SEND_TimeSync(gServInterTime);
          end;
        end;

        if (not g_Game_IsClient) and
        (
          (
            (
              e_KeyPressed(IK_RETURN) or e_KeyPressed(IK_KPRETURN) or e_KeyPressed(IK_SPACE) or
              e_KeyPressed(VK_FIRE) or e_KeyPressed(VK_OPEN) or
              e_KeyPressed(JOY0_ATTACK) or e_KeyPressed(JOY1_ATTACK) or
              e_KeyPressed(JOY2_ATTACK) or e_KeyPressed(JOY3_ATTACK)
            )
            and (not gJustChatted) and (not gConsoleShow) and (not gChatShow)
            and (g_ActiveWindow = nil)
          )
          or (g_Game_IsNet and ((gInterTime > gInterEndTime) or ((gInterReadyCount >= NetClientCount) and (NetClientCount > 0))))
        )
        then
        begin // Нажали <Enter>/<Пробел> или прошло достаточно времени:
          g_Game_StopAllSounds(True);

          if gMapOnce then // Это был тест
            gExit := EXIT_SIMPLE
          else
            if gNextMap <> '' then // Переходим на следующую карту
              g_Game_ChangeMap(gNextMap)
            else // Следующей карты нет
            begin
              if gGameSettings.GameType in [GT_CUSTOM, GT_SERVER] then
              begin
              // Выход в главное меню:
                g_Game_Free;
                g_GUI_ShowWindow('MainMenu');
                gMusic.SetByName('MUSIC_MENU');
                gMusic.Play();
                gState := STATE_MENU;
              end else
              begin
              // Финальная картинка:
                g_Game_ExecuteEvent('onwadend');
                g_Game_Free();
                if not gMusic.SetByName('MUSIC_endmus') then
                  gMusic.SetByName('MUSIC_STDENDMUS');
                gMusic.Play();
                gState := STATE_ENDPIC;
              end;
              g_Game_ExecuteEvent('ongameend');
            end;

          Exit;
        end
        else if g_Game_IsClient and
        (
          (
            e_KeyPressed(IK_RETURN) or e_KeyPressed(IK_KPRETURN) or e_KeyPressed(IK_SPACE) or
            e_KeyPressed(VK_FIRE) or e_KeyPressed(VK_OPEN) or
            e_KeyPressed(JOY0_ATTACK) or e_KeyPressed(JOY1_ATTACK) or
            e_KeyPressed(JOY2_ATTACK) or e_KeyPressed(JOY3_ATTACK)
          )
          and (not gJustChatted) and (not gConsoleShow) and (not gChatShow)
          and (g_ActiveWindow = nil)
        )
        then
        begin
          // ready / unready
          InterReady();
        end;

        if gState = STATE_INTERTEXT then
          if InterText.counter > 0 then
            InterText.counter := InterText.counter - 1;
      end;

    STATE_FOLD: // Затухание экрана
      begin
        if EndingGameCounter = 0 then
          begin
          // Закончился уровень в Своей игре:
            if gGameSettings.GameType in [GT_CUSTOM, GT_SERVER, GT_CLIENT] then
              begin
                gState := STATE_INTERCUSTOM;
                InterReadyTime := -1;
                if gLastMap and (gGameSettings.GameMode = GM_COOP) then
                begin
                  g_Game_ExecuteEvent('onwadend');
                  if not gMusic.SetByName('MUSIC_endmus') then
                    gMusic.SetByName('MUSIC_STDENDMUS');
                end
                else
                  gMusic.SetByName('MUSIC_ROUNDMUS');
                gMusic.Play();
                e_UnpressAllKeys();
              end
            else // Закончилась последняя карта в Одиночной игре
              begin
                gMusic.SetByName('MUSIC_INTERMUS');
                gMusic.Play();
                gState := STATE_INTERSINGLE;
                e_UnpressAllKeys();
              end;
            g_Game_ExecuteEvent('oninter');
          end
        else
          DecMin(EndingGameCounter, 6, 0);
      end;

    STATE_ENDPIC: // Картинка окончания мегаВада
      begin
        if gMapOnce then // Это был тест
        begin
          gExit := EXIT_SIMPLE;
          Exit;
        end;
      end;

    STATE_SLIST:
        g_Serverlist_Control(slCurrent, slTable);
  end;

// Статистика по Tab:
  if gGameOn then
    IsDrawStat := (not gConsoleShow) and (not gChatShow) and (gGameSettings.GameType <> GT_SINGLE) and g_Console_Action(ACTION_SCORES);

// Игра идет:
  if gGameOn and not gPause and (gState <> STATE_FOLD) then
  begin
  // Время += 28 миллисекунд:
    gTime := gTime + GAME_TICK;

  // Сообщение посередине экрана:
    if MessageTime = 0 then
      MessageText := '';
    if MessageTime > 0 then
      MessageTime := MessageTime - 1;

    if (g_Game_IsServer) then
    begin
    // Был задан лимит времени:
      if (gGameSettings.TimeLimit > 0) then
        if (gTime - gGameStartTime) div 1000 >= gGameSettings.TimeLimit then
        begin // Он прошел => конец уровня
          g_Game_NextLevel();
          Exit;
        end;

    // Надо респавнить игроков в LMS:
      if (gLMSRespawn > LMS_RESPAWN_NONE) and (gLMSRespawnTime < gTime) then
        g_Game_RestartRound(gLMSSoftSpawn);

    // Проверим результат голосования, если время прошло
      if gVoteInProgress and (gVoteTimer < gTime) then
        g_Game_CheckVote
      else if gVotePassed and (gVoteCmdTimer < gTime) then
      begin
        g_Console_Process(gVoteCommand);
        gVoteCommand := '';
        gVotePassed := False;
      end;

    // Замеряем время захвата флагов
      if gFlags[FLAG_RED].State = FLAG_STATE_CAPTURED then
        gFlags[FLAG_RED].CaptureTime := gFlags[FLAG_RED].CaptureTime + GAME_TICK;
      if gFlags[FLAG_BLUE].State = FLAG_STATE_CAPTURED then
        gFlags[FLAG_BLUE].CaptureTime := gFlags[FLAG_BLUE].CaptureTime + GAME_TICK;

    // Был задан лимит побед:
      if (gGameSettings.ScoreLimit > 0) then
      begin
        b := 0;

        if gGameSettings.GameMode = GM_DM then
          begin // В DM ищем игрока с max фрагами
            for i := 0 to High(gPlayers) do
              if gPlayers[i] <> nil then
                if gPlayers[i].Frags > b then
                  b := gPlayers[i].Frags;
          end
        else
          if gGameSettings.GameMode in [GM_TDM, GM_CTF] then
          begin // В CTF/TDM выбираем команду с наибольшим счетом
            b := Max(gTeamStat[TEAM_RED].Score, gTeamStat[TEAM_BLUE].Score);
          end;

      // Лимит побед набран => конец уровня:
        if b >= gGameSettings.ScoreLimit then
        begin
          g_Game_NextLevel();
          Exit;
        end;
      end;

    // Обрабатываем клавиши игроков:
      if gPlayer1 <> nil then gPlayer1.ReleaseKeys();
      if gPlayer2 <> nil then gPlayer2.ReleaseKeys();
      if (not gConsoleShow) and (not gChatShow) and (g_ActiveWindow = nil) then
      begin
        ProcessPlayerControls(gPlayer1, 0, P1MoveButton);
        ProcessPlayerControls(gPlayer2, 1, P2MoveButton);
      end  // if not console
      else
      begin
        if g_Game_IsNet and (gPlayer1 <> nil) then gPlayer1.PressKey(KEY_CHAT, 10000);
      end;
      // process weapon switch queue
    end; // if server

  // Наблюдатель
    if (gPlayer1 = nil) and (gPlayer2 = nil) and
       (not gConsoleShow) and (not gChatShow) and (g_ActiveWindow = nil) then
    begin
      if not gSpectKeyPress then
      begin
        if gPlayerAction[0, ACTION_JUMP] and (not gSpectAuto) then
        begin
          // switch spect mode
          case gSpectMode of
            SPECT_NONE: ; // not spectator
            SPECT_STATS,
            SPECT_MAPVIEW: Inc(gSpectMode);
            SPECT_PLAYERS: gSpectMode := SPECT_STATS; // reset to 1
          end;
          gSpectKeyPress := True;
        end;
        if (gSpectMode = SPECT_MAPVIEW)
           and (not gSpectAuto) then
        begin
          if gPlayerAction[0, ACTION_MOVELEFT] then
            gSpectX := Max(gSpectX - gSpectStep, 0);
          if gPlayerAction[0, ACTION_MOVERIGHT] then
            gSpectX := Min(gSpectX + gSpectStep, gMapInfo.Width - gScreenWidth);
          if gPlayerAction[0, ACTION_LOOKUP] then
            gSpectY := Max(gSpectY - gSpectStep, 0);
          if gPlayerAction[0, ACTION_LOOKDOWN] then
            gSpectY := Min(gSpectY + gSpectStep, gMapInfo.Height - gScreenHeight);
          if gWeaponAction[0, WP_PREV] then
          begin
            // decrease step
            if gSpectStep > 4 then gSpectStep := gSpectStep shr 1;
            gWeaponAction[0, WP_PREV] := False;
          end;
          if gWeaponAction[0, WP_NEXT] then
          begin
            // increase step
            if gSpectStep < 64 then gSpectStep := gSpectStep shl 1;
            gWeaponAction[0, WP_NEXT] := False;
          end;
        end;
        if (gSpectMode = SPECT_PLAYERS)
           and (not gSpectAuto) then
        begin
          if gPlayerAction[0, ACTION_LOOKUP] then
          begin
            // add second view
            gSpectViewTwo := True;
            gSpectKeyPress := True;
          end;
          if gPlayerAction[0, ACTION_LOOKDOWN] then
          begin
            // remove second view
            gSpectViewTwo := False;
            gSpectKeyPress := True;
          end;
          if gPlayerAction[0, ACTION_MOVELEFT] then
          begin
            // prev player (view 1)
            gSpectPID1 := GetActivePlayerID_Prev(gSpectPID1);
            gSpectKeyPress := True;
          end;
          if gPlayerAction[0, ACTION_MOVERIGHT] then
          begin
            // next player (view 1)
            gSpectPID1 := GetActivePlayerID_Next(gSpectPID1);
            gSpectKeyPress := True;
          end;
          if gWeaponAction[0, WP_PREV] then
          begin
            // prev player (view 2)
            gSpectPID2 := GetActivePlayerID_Prev(gSpectPID2);
            gWeaponAction[0, WP_PREV] := False;
          end;
          if gWeaponAction[0, WP_NEXT] then
          begin
            // next player (view 2)
            gSpectPID2 := GetActivePlayerID_Next(gSpectPID2);
            gWeaponAction[0, WP_NEXT] := False;
          end;
        end;
        if gPlayerAction[0, ACTION_ATTACK] then
        begin
          if (gSpectMode = SPECT_STATS) and (not gSpectAuto) then
          begin
            gSpectAuto := True;
            gSpectAutoNext := 0;
            gSpectViewTwo := False;
            gSpectKeyPress := True;
          end
          else
            if gSpectAuto then
            begin
              gSpectMode := SPECT_STATS;
              gSpectAuto := False;
              gSpectKeyPress := True;
            end;
        end;
      end
      else
        if (not gPlayerAction[0, ACTION_JUMP]) and
           (not gPlayerAction[0, ACTION_ATTACK]) and
           (not gPlayerAction[0, ACTION_MOVELEFT]) and
           (not gPlayerAction[0, ACTION_MOVERIGHT]) and
           (not gPlayerAction[0, ACTION_LOOKUP]) and
           (not gPlayerAction[0, ACTION_LOOKDOWN]) then
          gSpectKeyPress := False;

      if gSpectAuto then
      begin
        if gSpectMode = SPECT_MAPVIEW then
        begin
          i := Min(Max(gSpectX + gSpectAutoStepX, 0), gMapInfo.Width - gScreenWidth);
          if i = gSpectX then
            gSpectAutoNext := gTime
          else
            gSpectX := i;
          i := Min(Max(gSpectY + gSpectAutoStepY, 0), gMapInfo.Height - gScreenHeight);
          if i = gSpectY then
            gSpectAutoNext := gTime
          else
            gSpectY := i;
        end;
        if gSpectAutoNext <= gTime then
        begin
          if gSpectAutoNext > 0 then
          begin
            gSpectMode := GetRandomSpectMode(gSpectMode);
            case gSpectMode of
              SPECT_MAPVIEW:
              begin
                gSpectX := Random(gMapInfo.Width - gScreenWidth);
                gSpectY := Random(gMapInfo.Height - gScreenHeight);
                gSpectAutoStepX := Random(9) - 4;
                gSpectAutoStepY := Random(9) - 4;
                if ((gSpectX < 800) and (gSpectAutoStepX < 0)) or
                   ((gSpectX > gMapInfo.Width - gScreenWidth - 800) and (gSpectAutoStepX > 0)) then
                  gSpectAutoStepX := gSpectAutoStepX * -1;
                if ((gSpectY < 800) and (gSpectAutoStepY < 0)) or
                   ((gSpectY > gMapInfo.Height - gScreenHeight - 800) and (gSpectAutoStepY > 0)) then
                  gSpectAutoStepY := gSpectAutoStepY * -1;
              end;
              SPECT_PLAYERS:
              begin
                gSpectPID1 := GetActivePlayerID_Random(gSpectPID1);
              end;
            end;
          end;
          case gSpectMode of
            SPECT_STATS:   gSpectAutoNext := gTime + (Random(3) + 5) * 1000;
            SPECT_MAPVIEW: gSpectAutoNext := gTime + (Random(4) + 7) * 1000;
            SPECT_PLAYERS: gSpectAutoNext := gTime + (Random(7) + 8) * 1000;
          end;
        end;
      end;
    end;

  // Обновляем все остальное:
    g_Map_Update();
    g_Items_Update();
    g_Triggers_Update();
    g_Weapon_Update();
    g_Monsters_Update();
    g_GFX_Update();
    g_Player_UpdateAll();
    g_Player_UpdatePhysicalObjects();

    // server: send newly spawned monsters unconditionally
    if (gGameSettings.GameType = GT_SERVER) then
    begin
      if (Length(gMonstersSpawned) > 0) then
      begin
        for I := 0 to High(gMonstersSpawned) do MH_SEND_MonsterSpawn(gMonstersSpawned[I]);
        SetLength(gMonstersSpawned, 0);
      end;
    end;

    if (gSoundTriggerTime > 8) then
    begin
      g_Game_UpdateTriggerSounds();
      gSoundTriggerTime := 0;
    end
    else
    begin
      Inc(gSoundTriggerTime);
    end;

    if (NetMode = NET_SERVER) then
    begin
      Inc(NetTimeToUpdate);
      Inc(NetTimeToReliable);

      // send monster updates
      if (NetTimeToReliable >= NetRelupdRate) or (NetTimeToUpdate >= NetUpdateRate) then
      begin
        // send all monsters (periodic sync)
        reliableUpdate := (NetTimeToReliable >= NetRelupdRate);

        for I := 0 to High(gPlayers) do
        begin
          if (gPlayers[I] <> nil) then MH_SEND_PlayerPos(reliableUpdate, gPlayers[I].UID);
        end;

        g_Mons_ForEach(sendMonsPos);

        // update flags that aren't stationary
        if gGameSettings.GameMode = GM_CTF then
          for I := FLAG_RED to FLAG_BLUE do
            if gFlags[I].NeedSend then
            begin
              gFlags[I].NeedSend := False;
              MH_SEND_FlagPos(I);
            end;

        // update items that aren't stationary
        g_Items_ForEachAlive(sendItemPos);

        if reliableUpdate then
        begin
          NetTimeToReliable := 0;
          NetTimeToUpdate := NetUpdateRate;
        end
        else
        begin
          NetTimeToUpdate := 0;
        end;
      end
      else
      begin
        // send only mosters with some unexpected changes
        g_Mons_ForEach(sendMonsPosUnexpected);
      end;

      // send unexpected platform changes
      g_Map_NetSendInterestingPanels();

      g_Net_Slist_ServerUpdate();
      {
      if NetUseMaster then
      begin
        if (gTime >= NetTimeToMaster) or g_Net_Slist_IsConnectionInProgress then
        begin
          if (not g_Net_Slist_IsConnectionActive) then g_Net_Slist_Connect(false); // non-blocking connection to the master
          g_Net_Slist_Update;
          NetTimeToMaster := gTime + NetMasterRate;
        end;
      end;
      }
    end
    else if (NetMode = NET_CLIENT) then
    begin
      MC_SEND_PlayerPos();
    end;
  end; // if gameOn ...

// Активно окно интерфейса - передаем клавиши ему:
  if g_ActiveWindow <> nil then
  begin
    w := e_GetFirstKeyPressed();

    if (w <> IK_INVALID) then
      begin
        Msg.Msg := MESSAGE_DIKEY;
        Msg.wParam := w;
        g_ActiveWindow.OnMessage(Msg);
      end;

  // Если оно от этого не закрылось, то обновляем:
    if g_ActiveWindow <> nil then
      g_ActiveWindow.Update();

  // Нужно сменить разрешение:
    if gResolutionChange then
    begin
      e_WriteLog('Changing resolution', TMsgType.Notify);
      g_Game_ChangeResolution(gRC_Width, gRC_Height, gRC_FullScreen, gRC_Maximized);
      gResolutionChange := False;
      g_ActiveWindow := nil;
    end;

  // Нужно сменить язык:
    if gLanguageChange then
    begin
      //e_WriteLog('Read language file', MSG_NOTIFY);
      //g_Language_Load(DataDir + gLanguage + '.txt');
      g_Language_Set(gLanguage);
{$IFNDEF HEADLESS}
      g_Menu_Reset();
{$ENDIF}
      gLanguageChange := False;
    end;
  end;

// Горячая клавиша для вызова меню выхода из игры (F10):
  if e_KeyPressed(IK_F10) and
     gGameOn and
     (not gConsoleShow) and
     (g_ActiveWindow = nil) then
  begin
    KeyPress(IK_F10);
  end;

  Time := sys_GetTicks() {div 1000};

// Обработка отложенных событий:
  if gDelayedEvents <> nil then
    for a := 0 to High(gDelayedEvents) do
      if gDelayedEvents[a].Pending and
      (
        ((gDelayedEvents[a].DEType = DE_GLOBEVENT) and (gDelayedEvents[a].Time <= Time)) or
        ((gDelayedEvents[a].DEType > DE_GLOBEVENT) and (gDelayedEvents[a].Time <= gTime))
      ) then
      begin
        case gDelayedEvents[a].DEType of
          DE_GLOBEVENT:
            g_Game_ExecuteEvent(gDelayedEvents[a].DEStr);
          DE_BFGHIT:
            if gGameOn then
              g_Game_Announce_GoodShot(gDelayedEvents[a].DENum);
          DE_KILLCOMBO:
            if gGameOn then
            begin
              g_Game_Announce_KillCombo(gDelayedEvents[a].DENum);
              if g_Game_IsNet and g_Game_IsServer then
                MH_SEND_GameEvent(NET_EV_KILLCOMBO, gDelayedEvents[a].DENum);
            end;
          DE_BODYKILL:
            if gGameOn then
              g_Game_Announce_BodyKill(gDelayedEvents[a].DENum);
        end;
        gDelayedEvents[a].Pending := False;
      end;

// Каждую секунду обновляем счетчик обновлений:
  UPSCounter := UPSCounter + 1;
  if Time - UPSTime >= 1000 then
  begin
    UPS := UPSCounter;
    UPSCounter := 0;
    UPSTime := Time;
  end;

  if gGameOn then
  begin
    g_Weapon_AddDynLights();
    g_Items_AddDynLights();
  end;
end;

procedure g_Game_LoadChatSounds(Resource: string);
var
  WAD: TWADFile;
  FileName, Snd: string;
  p: Pointer;
  len, cnt, tags, i, j: Integer;
  cfg: TConfig;
begin
  FileName := g_ExtractWadName(Resource);

  WAD := TWADFile.Create();
  WAD.ReadFile(FileName);

  if not WAD.GetResource(g_ExtractFilePathName(Resource), p, len) then
  begin
    gChatSounds := nil;
    WAD.Free();
    Exit;
  end;

  cfg := TConfig.CreateMem(p, len);
  cnt := cfg.ReadInt('ChatSounds', 'Count', 0);

  SetLength(gChatSounds, cnt);
  for i := 0 to Length(gChatSounds) - 1 do
  begin
    gChatSounds[i].Sound := nil;
    Snd := Trim(cfg.ReadStr(IntToStr(i), 'Sound', ''));
    tags := cfg.ReadInt(IntToStr(i), 'Tags', 0);
    if (Snd = '') or (Tags <= 0) then
      continue;
    g_Sound_CreateWADEx('SOUND_CHAT_MACRO' + IntToStr(i), GameWAD+':'+Snd);
    gChatSounds[i].Sound := TPlayableSound.Create();
    gChatSounds[i].Sound.SetByName('SOUND_CHAT_MACRO' + IntToStr(i));
    SetLength(gChatSounds[i].Tags, tags);
    for j := 0 to tags - 1 do
      gChatSounds[i].Tags[j] := toLowerCase1251(cfg.ReadStr(IntToStr(i), 'Tag' + IntToStr(j), ''));
    gChatSounds[i].FullWord := cfg.ReadBool(IntToStr(i), 'FullWord', False);
  end;

  cfg.Free();
  WAD.Free();
end;

procedure g_Game_FreeChatSounds();
var
  i: Integer;
begin
  for i := 0 to Length(gChatSounds) - 1 do
  begin
    gChatSounds[i].Sound.Free();
    g_Sound_Delete('SOUND_CHAT_MACRO' + IntToStr(i));
  end;
  SetLength(gChatSounds, 0);
  gChatSounds := nil;
end;

procedure g_Game_LoadData();
var
  wl, hl: Integer;
  wr, hr: Integer;
  wb, hb: Integer;
  wm, hm: Integer;
begin
  if DataLoaded then Exit;

  e_WriteLog('Loading game data...', TMsgType.Notify);

  g_Texture_CreateWADEx('NOTEXTURE', GameWAD+':TEXTURES\NOTEXTURE');
  g_Texture_CreateWADEx('TEXTURE_PLAYER_HUD', GameWAD+':TEXTURES\HUD');
  g_Texture_CreateWADEx('TEXTURE_PLAYER_HUDAIR', GameWAD+':TEXTURES\AIRBAR');
  g_Texture_CreateWADEx('TEXTURE_PLAYER_HUDJET', GameWAD+':TEXTURES\JETBAR');
  g_Texture_CreateWADEx('TEXTURE_PLAYER_HUDBG', GameWAD+':TEXTURES\HUDBG');
  g_Texture_CreateWADEx('TEXTURE_PLAYER_ARMORHUD', GameWAD+':TEXTURES\ARMORHUD');
  g_Texture_CreateWADEx('TEXTURE_PLAYER_REDFLAG', GameWAD+':TEXTURES\FLAGHUD_R_BASE');
  g_Texture_CreateWADEx('TEXTURE_PLAYER_REDFLAG_S', GameWAD+':TEXTURES\FLAGHUD_R_STOLEN');
  g_Texture_CreateWADEx('TEXTURE_PLAYER_REDFLAG_D', GameWAD+':TEXTURES\FLAGHUD_R_DROP');
  g_Texture_CreateWADEx('TEXTURE_PLAYER_BLUEFLAG', GameWAD+':TEXTURES\FLAGHUD_B_BASE');
  g_Texture_CreateWADEx('TEXTURE_PLAYER_BLUEFLAG_S', GameWAD+':TEXTURES\FLAGHUD_B_STOLEN');
  g_Texture_CreateWADEx('TEXTURE_PLAYER_BLUEFLAG_D', GameWAD+':TEXTURES\FLAGHUD_B_DROP');
  g_Texture_CreateWADEx('TEXTURE_PLAYER_TALKBUBBLE', GameWAD+':TEXTURES\TALKBUBBLE');
  g_Texture_CreateWADEx('TEXTURE_PLAYER_INVULPENTA', GameWAD+':TEXTURES\PENTA');
  g_Texture_CreateWADEx('TEXTURE_PLAYER_INDICATOR', GameWAD+':TEXTURES\PLRIND');

  hasPBarGfx := true;
  if not g_Texture_CreateWADEx('UI_GFX_PBAR_LEFT', GameWAD+':TEXTURES\LLEFT') then hasPBarGfx := false;
  if not g_Texture_CreateWADEx('UI_GFX_PBAR_MARKER', GameWAD+':TEXTURES\LMARKER') then hasPBarGfx := false;
  if not g_Texture_CreateWADEx('UI_GFX_PBAR_MIDDLE', GameWAD+':TEXTURES\LMIDDLE') then hasPBarGfx := false;
  if not g_Texture_CreateWADEx('UI_GFX_PBAR_RIGHT', GameWAD+':TEXTURES\LRIGHT') then hasPBarGfx := false;

  if hasPBarGfx then
  begin
    g_Texture_GetSize('UI_GFX_PBAR_LEFT', wl, hl);
    g_Texture_GetSize('UI_GFX_PBAR_RIGHT', wr, hr);
    g_Texture_GetSize('UI_GFX_PBAR_MIDDLE', wb, hb);
    g_Texture_GetSize('UI_GFX_PBAR_MARKER', wm, hm);
    if (wl > 0) and (hl > 0) and (wr > 0) and (hr = hl) and (wb > 0) and (hb = hl) and (wm > 0) and (hm > 0) and (hm <= hl) then
    begin
      // yay!
    end
    else
    begin
      hasPBarGfx := false;
    end;
  end;

  g_Frames_CreateWAD(nil, 'FRAMES_TELEPORT', GameWAD+':TEXTURES\TELEPORT', 64, 64, 10, False);
  g_Frames_CreateWAD(nil, 'FRAMES_PUNCH', GameWAD+':WEAPONS\PUNCH', 64, 64, 4, False);
  g_Frames_CreateWAD(nil, 'FRAMES_PUNCH_UP', GameWAD+':WEAPONS\PUNCH_UP', 64, 64, 4, False);
  g_Frames_CreateWAD(nil, 'FRAMES_PUNCH_DN', GameWAD+':WEAPONS\PUNCH_DN', 64, 64, 4, False);
  g_Frames_CreateWAD(nil, 'FRAMES_PUNCH_BERSERK', GameWAD+':WEAPONS\PUNCHB', 64, 64, 4, False);
  g_Frames_CreateWAD(nil, 'FRAMES_PUNCH_BERSERK_UP', GameWAD+':WEAPONS\PUNCHB_UP', 64, 64, 4, False);
  g_Frames_CreateWAD(nil, 'FRAMES_PUNCH_BERSERK_DN', GameWAD+':WEAPONS\PUNCHB_DN', 64, 64, 4, False);
  g_Sound_CreateWADEx('SOUND_GAME_TELEPORT', GameWAD+':SOUNDS\TELEPORT');
  g_Sound_CreateWADEx('SOUND_GAME_NOTELEPORT', GameWAD+':SOUNDS\NOTELEPORT');
  g_Sound_CreateWADEx('SOUND_GAME_SECRET', GameWAD+':SOUNDS\SECRET');
  g_Sound_CreateWADEx('SOUND_GAME_DOOROPEN', GameWAD+':SOUNDS\DOOROPEN');
  g_Sound_CreateWADEx('SOUND_GAME_DOORCLOSE', GameWAD+':SOUNDS\DOORCLOSE');
  g_Sound_CreateWADEx('SOUND_GAME_BULK1', GameWAD+':SOUNDS\BULK1');
  g_Sound_CreateWADEx('SOUND_GAME_BULK2', GameWAD+':SOUNDS\BULK2');
  g_Sound_CreateWADEx('SOUND_GAME_BUBBLE1', GameWAD+':SOUNDS\BUBBLE1');
  g_Sound_CreateWADEx('SOUND_GAME_BUBBLE2', GameWAD+':SOUNDS\BUBBLE2');
  g_Sound_CreateWADEx('SOUND_GAME_BURNING', GameWAD+':SOUNDS\BURNING');
  g_Sound_CreateWADEx('SOUND_GAME_SWITCH1', GameWAD+':SOUNDS\SWITCH1');
  g_Sound_CreateWADEx('SOUND_GAME_SWITCH0', GameWAD+':SOUNDS\SWITCH0');
  g_Sound_CreateWADEx('SOUND_GAME_RADIO', GameWAD+':SOUNDS\RADIO');
  g_Sound_CreateWADEx('SOUND_ANNOUNCER_GOOD1', GameWAD+':SOUNDS\GOOD1');
  g_Sound_CreateWADEx('SOUND_ANNOUNCER_GOOD2', GameWAD+':SOUNDS\GOOD2');
  g_Sound_CreateWADEx('SOUND_ANNOUNCER_GOOD3', GameWAD+':SOUNDS\GOOD3');
  g_Sound_CreateWADEx('SOUND_ANNOUNCER_GOOD4', GameWAD+':SOUNDS\GOOD4');
  g_Sound_CreateWADEx('SOUND_ANNOUNCER_KILL2X', GameWAD+':SOUNDS\KILL2X');
  g_Sound_CreateWADEx('SOUND_ANNOUNCER_KILL3X', GameWAD+':SOUNDS\KILL3X');
  g_Sound_CreateWADEx('SOUND_ANNOUNCER_KILL4X', GameWAD+':SOUNDS\KILL4X');
  g_Sound_CreateWADEx('SOUND_ANNOUNCER_KILLMX', GameWAD+':SOUNDS\KILLMX');
  g_Sound_CreateWADEx('SOUND_ANNOUNCER_MUHAHA1', GameWAD+':SOUNDS\MUHAHA1');
  g_Sound_CreateWADEx('SOUND_ANNOUNCER_MUHAHA2', GameWAD+':SOUNDS\MUHAHA2');
  g_Sound_CreateWADEx('SOUND_ANNOUNCER_MUHAHA3', GameWAD+':SOUNDS\MUHAHA3');
  g_Sound_CreateWADEx('SOUND_CTF_GET1', GameWAD+':SOUNDS\GETFLAG1');
  g_Sound_CreateWADEx('SOUND_CTF_GET2', GameWAD+':SOUNDS\GETFLAG2');
  g_Sound_CreateWADEx('SOUND_CTF_LOST1', GameWAD+':SOUNDS\LOSTFLG1');
  g_Sound_CreateWADEx('SOUND_CTF_LOST2', GameWAD+':SOUNDS\LOSTFLG2');
  g_Sound_CreateWADEx('SOUND_CTF_RETURN1', GameWAD+':SOUNDS\RETFLAG1');
  g_Sound_CreateWADEx('SOUND_CTF_RETURN2', GameWAD+':SOUNDS\RETFLAG2');
  g_Sound_CreateWADEx('SOUND_CTF_CAPTURE1', GameWAD+':SOUNDS\CAPFLAG1');
  g_Sound_CreateWADEx('SOUND_CTF_CAPTURE2', GameWAD+':SOUNDS\CAPFLAG2');

  goodsnd[0] := TPlayableSound.Create();
  goodsnd[1] := TPlayableSound.Create();
  goodsnd[2] := TPlayableSound.Create();
  goodsnd[3] := TPlayableSound.Create();

  goodsnd[0].SetByName('SOUND_ANNOUNCER_GOOD1');
  goodsnd[1].SetByName('SOUND_ANNOUNCER_GOOD2');
  goodsnd[2].SetByName('SOUND_ANNOUNCER_GOOD3');
  goodsnd[3].SetByName('SOUND_ANNOUNCER_GOOD4');

  killsnd[0] := TPlayableSound.Create();
  killsnd[1] := TPlayableSound.Create();
  killsnd[2] := TPlayableSound.Create();
  killsnd[3] := TPlayableSound.Create();

  killsnd[0].SetByName('SOUND_ANNOUNCER_KILL2X');
  killsnd[1].SetByName('SOUND_ANNOUNCER_KILL3X');
  killsnd[2].SetByName('SOUND_ANNOUNCER_KILL4X');
  killsnd[3].SetByName('SOUND_ANNOUNCER_KILLMX');

  hahasnd[0] := TPlayableSound.Create();
  hahasnd[1] := TPlayableSound.Create();
  hahasnd[2] := TPlayableSound.Create();

  hahasnd[0].SetByName('SOUND_ANNOUNCER_MUHAHA1');
  hahasnd[1].SetByName('SOUND_ANNOUNCER_MUHAHA2');
  hahasnd[2].SetByName('SOUND_ANNOUNCER_MUHAHA3');

  sound_get_flag[0] := TPlayableSound.Create();
  sound_get_flag[1] := TPlayableSound.Create();
  sound_lost_flag[0] := TPlayableSound.Create();
  sound_lost_flag[1] := TPlayableSound.Create();
  sound_ret_flag[0] := TPlayableSound.Create();
  sound_ret_flag[1] := TPlayableSound.Create();
  sound_cap_flag[0] := TPlayableSound.Create();
  sound_cap_flag[1] := TPlayableSound.Create();

  sound_get_flag[0].SetByName('SOUND_CTF_GET1');
  sound_get_flag[1].SetByName('SOUND_CTF_GET2');
  sound_lost_flag[0].SetByName('SOUND_CTF_LOST1');
  sound_lost_flag[1].SetByName('SOUND_CTF_LOST2');
  sound_ret_flag[0].SetByName('SOUND_CTF_RETURN1');
  sound_ret_flag[1].SetByName('SOUND_CTF_RETURN2');
  sound_cap_flag[0].SetByName('SOUND_CTF_CAPTURE1');
  sound_cap_flag[1].SetByName('SOUND_CTF_CAPTURE2');

  g_Game_LoadChatSounds(GameWAD+':CHATSND\SNDCFG');

  g_Game_SetLoadingText(_lc[I_LOAD_ITEMS_DATA], 0, False);
  g_Items_LoadData();

  g_Game_SetLoadingText(_lc[I_LOAD_WEAPONS_DATA], 0, False);
  g_Weapon_LoadData();

  g_Monsters_LoadData();

  DataLoaded := True;
end;

procedure g_Game_Quit();
begin
  g_Game_StopAllSounds(True);
  gMusic.Free();
  g_Game_FreeData();
  r_PlayerModel_Finalize;
  g_PlayerModel_FreeData();
  g_Texture_DeleteAll();
  g_Frames_DeleteAll();
{$IFNDEF HEADLESS}
  //g_Menu_Free(); //k8: this segfaults after resolution change; who cares?
{$ENDIF}

  if NetInitDone then g_Net_Free;

// remove map after test
  if gMapToDelete <> '' then
    g_Game_DeleteTestMap();

  gExit := EXIT_QUIT;
  sys_RequestQuit;
end;

procedure g_Game_FreeData();
begin
  if not DataLoaded then Exit;

  g_Items_FreeData();
  g_Weapon_FreeData();
  g_Monsters_FreeData();

  e_WriteLog('Releasing game data...', TMsgType.Notify);

  g_Texture_Delete('NOTEXTURE');
  g_Texture_Delete('TEXTURE_PLAYER_HUD');
  g_Texture_Delete('TEXTURE_PLAYER_HUDBG');
  g_Texture_Delete('TEXTURE_PLAYER_ARMORHUD');
  g_Texture_Delete('TEXTURE_PLAYER_REDFLAG');
  g_Texture_Delete('TEXTURE_PLAYER_REDFLAG_S');
  g_Texture_Delete('TEXTURE_PLAYER_REDFLAG_D');
  g_Texture_Delete('TEXTURE_PLAYER_BLUEFLAG');
  g_Texture_Delete('TEXTURE_PLAYER_BLUEFLAG_S');
  g_Texture_Delete('TEXTURE_PLAYER_BLUEFLAG_D');
  g_Texture_Delete('TEXTURE_PLAYER_TALKBUBBLE');
  g_Texture_Delete('TEXTURE_PLAYER_INVULPENTA');
  g_Frames_DeleteByName('FRAMES_TELEPORT');
  g_Frames_DeleteByName('FRAMES_PUNCH');
  g_Frames_DeleteByName('FRAMES_PUNCH_UP');
  g_Frames_DeleteByName('FRAMES_PUNCH_DN');
  g_Frames_DeleteByName('FRAMES_PUNCH_BERSERK');
  g_Frames_DeleteByName('FRAMES_PUNCH_BERSERK_UP');
  g_Frames_DeleteByName('FRAMES_PUNCH_BERSERK_DN');
  g_Sound_Delete('SOUND_GAME_TELEPORT');
  g_Sound_Delete('SOUND_GAME_NOTELEPORT');
  g_Sound_Delete('SOUND_GAME_SECRET');
  g_Sound_Delete('SOUND_GAME_DOOROPEN');
  g_Sound_Delete('SOUND_GAME_DOORCLOSE');
  g_Sound_Delete('SOUND_GAME_BULK1');
  g_Sound_Delete('SOUND_GAME_BULK2');
  g_Sound_Delete('SOUND_GAME_BUBBLE1');
  g_Sound_Delete('SOUND_GAME_BUBBLE2');
  g_Sound_Delete('SOUND_GAME_BURNING');
  g_Sound_Delete('SOUND_GAME_SWITCH1');
  g_Sound_Delete('SOUND_GAME_SWITCH0');

  goodsnd[0].Free();
  goodsnd[1].Free();
  goodsnd[2].Free();
  goodsnd[3].Free();

  g_Sound_Delete('SOUND_ANNOUNCER_GOOD1');
  g_Sound_Delete('SOUND_ANNOUNCER_GOOD2');
  g_Sound_Delete('SOUND_ANNOUNCER_GOOD3');
  g_Sound_Delete('SOUND_ANNOUNCER_GOOD4');

  killsnd[0].Free();
  killsnd[1].Free();
  killsnd[2].Free();
  killsnd[3].Free();

  g_Sound_Delete('SOUND_ANNOUNCER_KILL2X');
  g_Sound_Delete('SOUND_ANNOUNCER_KILL3X');
  g_Sound_Delete('SOUND_ANNOUNCER_KILL4X');
  g_Sound_Delete('SOUND_ANNOUNCER_KILLMX');

  hahasnd[0].Free();
  hahasnd[1].Free();
  hahasnd[2].Free();

  g_Sound_Delete('SOUND_ANNOUNCER_MUHAHA1');
  g_Sound_Delete('SOUND_ANNOUNCER_MUHAHA2');
  g_Sound_Delete('SOUND_ANNOUNCER_MUHAHA3');

  sound_get_flag[0].Free();
  sound_get_flag[1].Free();
  sound_lost_flag[0].Free();
  sound_lost_flag[1].Free();
  sound_ret_flag[0].Free();
  sound_ret_flag[1].Free();
  sound_cap_flag[0].Free();
  sound_cap_flag[1].Free();

  g_Sound_Delete('SOUND_CTF_GET1');
  g_Sound_Delete('SOUND_CTF_GET2');
  g_Sound_Delete('SOUND_CTF_LOST1');
  g_Sound_Delete('SOUND_CTF_LOST2');
  g_Sound_Delete('SOUND_CTF_RETURN1');
  g_Sound_Delete('SOUND_CTF_RETURN2');
  g_Sound_Delete('SOUND_CTF_CAPTURE1');
  g_Sound_Delete('SOUND_CTF_CAPTURE2');

  g_Game_FreeChatSounds();

  DataLoaded := False;
end;

procedure g_FatalError(Text: String);
begin
  g_Console_Add(Format(_lc[I_FATAL_ERROR], [Text]), True);
  e_WriteLog(Format(_lc[I_FATAL_ERROR], [Text]), TMsgType.Warning);

  gExit := EXIT_SIMPLE;
  if gGameOn then EndGame;
end;

procedure g_SimpleError(Text: String);
begin
  g_Console_Add(Format(_lc[I_SIMPLE_ERROR], [Text]), True);
  e_WriteLog(Format(_lc[I_SIMPLE_ERROR], [Text]), TMsgType.Warning);
end;

procedure g_Game_SetupScreenSize();
const
  RES_FACTOR = 4.0 / 3.0;
var
  s: Single;
  rf: Single;
  bw, bh: Word;
begin
// Размер экранов игроков:
  gPlayerScreenSize.X := gScreenWidth-196;
  if (gPlayer1 <> nil) and (gPlayer2 <> nil) then
    gPlayerScreenSize.Y := gScreenHeight div 2
  else
    gPlayerScreenSize.Y := gScreenHeight;

// Размер заднего плана:
  if BackID <> DWORD(-1) then
  begin
    s := SKY_STRETCH;
    if (gScreenWidth*s > gMapInfo.Width) or
       (gScreenHeight*s > gMapInfo.Height) then
    begin
      gBackSize.X := gScreenWidth;
      gBackSize.Y := gScreenHeight;
    end
    else
    begin
      e_GetTextureSize(BackID, @bw, @bh);
      rf := Single(bw) / Single(bh);
      if (rf > RES_FACTOR) then bw := Round(Single(bh) * RES_FACTOR)
      else if (rf < RES_FACTOR) then bh := Round(Single(bw) / RES_FACTOR);
      s := Max(gScreenWidth / bw, gScreenHeight / bh);
      if (s < 1.0) then s := 1.0;
      gBackSize.X := Round(bw*s);
      gBackSize.Y := Round(bh*s);
    end;
  end;
end;

procedure g_Game_ChangeResolution(newWidth, newHeight: Word; nowFull, nowMax: Boolean);
begin
  sys_SetDisplayMode(newWidth, newHeight, gBPP, nowFull, nowMax);
end;

procedure g_Game_AddPlayer(Team: Byte = TEAM_NONE);
begin
  if ((not gGameOn) and (gState <> STATE_INTERCUSTOM))
  or (not (gGameSettings.GameType in [GT_CUSTOM, GT_SERVER, GT_CLIENT])) then
    Exit;

  if (gGameSettings.MaxLives > 0) and (gLMSRespawn = LMS_RESPAWN_NONE) then
    Exit;

  if gPlayer1 = nil then
  begin
    if g_Game_IsClient then
    begin
      if NetPlrUID1 > -1 then
        MC_SEND_CheatRequest(NET_CHEAT_SPECTATE);
      Exit;
    end;

    if not (Team in [TEAM_RED, TEAM_BLUE]) then
      Team := gPlayer1Settings.Team;

    // Создание первого игрока:
    gPlayer1 := g_Player_Get(g_Player_Create(gPlayer1Settings.Model,
                                             gPlayer1Settings.Color,
                                             Team, False));
    if gPlayer1 = nil then
      g_FatalError(Format(_lc[I_GAME_ERROR_PLAYER_CREATE], [1]))
    else
    begin
      gPlayer1.Name := gPlayer1Settings.Name;
      gPlayer1.WeapSwitchMode := gPlayer1Settings.WeaponSwitch;
      gPlayer1.setWeaponPrefs(gPlayer1Settings.WeaponPreferences);
      gPlayer1.SwitchToEmpty := gPlayer1Settings.SwitchToEmpty;
      gPlayer1.SkipFist := gPlayer1Settings.SkipFist;
      g_Console_Add(Format(_lc[I_PLAYER_JOIN], [gPlayer1.Name]), True);
      if g_Game_IsServer and g_Game_IsNet then
        MH_SEND_PlayerCreate(gPlayer1.UID);
      gPlayer1.Respawn(False, True);
      g_Net_Slist_ServerPlayerComes();
    end;

    Exit;
  end;
  if gPlayer2 = nil then
  begin
    if g_Game_IsClient then
    begin
      if NetPlrUID2 > -1 then
        gPlayer2 := g_Player_Get(NetPlrUID2);
      Exit;
    end;

    if not (Team in [TEAM_RED, TEAM_BLUE]) then
      Team := gPlayer2Settings.Team;

    // Создание второго игрока:
    gPlayer2 := g_Player_Get(g_Player_Create(gPlayer2Settings.Model,
                                             gPlayer2Settings.Color,
                                             Team, False));
    if gPlayer2 = nil then
      g_FatalError(Format(_lc[I_GAME_ERROR_PLAYER_CREATE], [2]))
    else
    begin
      gPlayer2.Name := gPlayer2Settings.Name;
      gPlayer2.WeapSwitchMode := gPlayer2Settings.WeaponSwitch;
      gPlayer2.setWeaponPrefs(gPlayer2Settings.WeaponPreferences);
      gPlayer2.SwitchToEmpty := gPlayer2Settings.SwitchToEmpty;
      gPlayer2.SkipFist := gPlayer2Settings.SkipFist;
      g_Console_Add(Format(_lc[I_PLAYER_JOIN], [gPlayer2.Name]), True);
      if g_Game_IsServer and g_Game_IsNet then
        MH_SEND_PlayerCreate(gPlayer2.UID);
      gPlayer2.Respawn(False, True);
      g_Net_Slist_ServerPlayerComes();
    end;

    Exit;
  end;
end;

procedure g_Game_RemovePlayer();
var
  Pl: TPlayer;
begin
  if ((not gGameOn) and (gState <> STATE_INTERCUSTOM))
  or (not (gGameSettings.GameType in [GT_CUSTOM, GT_SERVER, GT_CLIENT])) then
    Exit;
  Pl := gPlayer2;
  if Pl <> nil then
  begin
    if g_Game_IsServer then
    begin
      Pl.Lives := 0;
      Pl.Kill(K_SIMPLEKILL, 0, HIT_DISCON);
      g_Console_Add(Format(_lc[I_PLAYER_LEAVE], [Pl.Name]), True);
      g_Player_Remove(Pl.UID);
      g_Net_Slist_ServerPlayerLeaves();
    end
    else
    begin
      gSpectLatchPID2 := Pl.UID;
      gPlayer2 := nil;
    end;
    Exit;
  end;
  Pl := gPlayer1;
  if Pl <> nil then
  begin
    if g_Game_IsServer then
    begin
      Pl.Lives := 0;
      Pl.Kill(K_SIMPLEKILL, 0, HIT_DISCON);
      g_Console_Add(Format(_lc[I_PLAYER_LEAVE], [Pl.Name]), True);
      g_Player_Remove(Pl.UID);
      g_Net_Slist_ServerPlayerLeaves();
    end else
    begin
      gSpectLatchPID1 := Pl.UID;
      gPlayer1 := nil;
      MC_SEND_CheatRequest(NET_CHEAT_SPECTATE);
    end;
    Exit;
  end;
  g_Net_Slist_ServerPlayerLeaves();
end;

procedure g_Game_Spectate();
begin
  g_Game_RemovePlayer();
  if gPlayer1 <> nil then
    g_Game_RemovePlayer();
end;

procedure g_Game_SpectateCenterView();
begin
  gSpectX := Max(gMapInfo.Width div 2 - gScreenWidth div 2, 0);
  gSpectY := Max(gMapInfo.Height div 2 - gScreenHeight div 2, 0);
end;

procedure g_Game_StartSingle(Map: String; TwoPlayers: Boolean; nPlayers: Byte);
var
  i, nPl: Integer;
  tmps: AnsiString;
begin
  g_Game_Free();

  e_WriteLog('Starting singleplayer game...', TMsgType.Notify);

  g_Game_ClearLoading();

// Настройки игры:
  FillByte(gGameSettings, SizeOf(TGameSettings), 0);
  gAimLine := False;
  gShowMap := False;
  gGameSettings.GameType := GT_SINGLE;
  gGameSettings.MaxLives := 0;
  gGameSettings.Options := gGameSettings.Options + GAME_OPTION_ALLOWEXIT;
  gGameSettings.Options := gGameSettings.Options + GAME_OPTION_MONSTERS;
  gGameSettings.Options := gGameSettings.Options + GAME_OPTION_BOTVSMONSTER;
  gGameSettings.Options := gGameSettings.Options + GAME_OPTION_TEAMHITPROJECTILE;
  gGameSettings.Options := gGameSettings.Options + GAME_OPTION_TEAMHITTRACE;
  gSwitchGameMode := GM_SINGLE;

  gLMSRespawn := LMS_RESPAWN_NONE;
  gLMSRespawnTime := 0;
  gSpectLatchPID1 := 0;
  gSpectLatchPID2 := 0;

  g_Game_ExecuteEvent('ongamestart');

// Установка размеров окон игроков:
  g_Game_SetupScreenSize();

// Создание первого игрока:
  gPlayer1 := g_Player_Get(g_Player_Create(gPlayer1Settings.Model,
                                           gPlayer1Settings.Color,
                                           gPlayer1Settings.Team, False));
  if gPlayer1 = nil then
  begin
    g_FatalError(Format(_lc[I_GAME_ERROR_PLAYER_CREATE], [1]));
    Exit;
  end;

  gPlayer1.Name := gPlayer1Settings.Name;
  gPlayer1.WeapSwitchMode := gPlayer1Settings.WeaponSwitch;
  gPlayer1.setWeaponPrefs(gPlayer1Settings.WeaponPreferences);
  gPlayer1.SwitchToEmpty := gPlayer1Settings.SwitchToEmpty;
  gPlayer1.SkipFist := gPlayer1Settings.SkipFist;
  nPl := 1;

// Создание второго игрока, если есть:
  if TwoPlayers then
  begin
    gPlayer2 := g_Player_Get(g_Player_Create(gPlayer2Settings.Model,
                                             gPlayer2Settings.Color,
                                             gPlayer2Settings.Team, False));
    if gPlayer2 = nil then
    begin
      g_FatalError(Format(_lc[I_GAME_ERROR_PLAYER_CREATE], [2]));
      Exit;
    end;

    gPlayer2.Name := gPlayer2Settings.Name;
    gPlayer2.WeapSwitchMode := gPlayer2Settings.WeaponSwitch;
    gPlayer2.setWeaponPrefs(gPlayer2Settings.WeaponPreferences);
    gPlayer2.SwitchToEmpty := gPlayer2Settings.SwitchToEmpty;
    gPlayer2.SkipFist := gPlayer2Settings.SkipFist;
    Inc(nPl);
  end;

// Загрузка и запуск карты:
  if not g_Game_StartMap(false{asMegawad}, MAP, True) then
  begin
    if (Pos(':\', Map) > 0) or (Pos(':/', Map) > 0) then tmps := Map else tmps := gGameSettings.WAD + ':\' + MAP;
    g_FatalError(Format(_lc[I_GAME_ERROR_MAP_LOAD], [tmps]));
    Exit;
  end;

// Настройки игроков и ботов:
  g_Player_Init();

// Создаем ботов:
  for i := nPl+1 to nPlayers do
    g_Player_Create(STD_PLAYER_MODEL, _RGB(0, 0, 0), 0, True);
end;

procedure g_Game_StartCustom(Map: String; GameMode: Byte;
                             TimeLimit, ScoreLimit: Word;
                             MaxLives: Byte;
                             Options: LongWord; nPlayers: Byte);
var
  i, nPl: Integer;
begin
  g_Game_Free();

  e_WriteLog('Starting custom game...', TMsgType.Notify);

  g_Game_ClearLoading();

// Настройки игры:
  gGameSettings.GameType := GT_CUSTOM;
  gGameSettings.GameMode := GameMode;
  gSwitchGameMode := GameMode;
  gGameSettings.TimeLimit := TimeLimit;
  gGameSettings.ScoreLimit := ScoreLimit;
  gGameSettings.MaxLives := IfThen(GameMode = GM_CTF, 0, MaxLives);
  gGameSettings.Options := Options;

  gCoopTotalMonstersKilled := 0;
  gCoopTotalSecretsFound := 0;
  gCoopTotalMonsters := 0;
  gCoopTotalSecrets := 0;
  gAimLine := False;
  gShowMap := False;

  gLMSRespawn := LMS_RESPAWN_NONE;
  gLMSRespawnTime := 0;
  gSpectLatchPID1 := 0;
  gSpectLatchPID2 := 0;

  g_Game_ExecuteEvent('ongamestart');

// Установка размеров окон игроков:
  g_Game_SetupScreenSize();

// Режим наблюдателя:
  if nPlayers = 0 then
  begin
    gPlayer1 := nil;
    gPlayer2 := nil;
  end;

  nPl := 0;
  if nPlayers >= 1 then
  begin
  // Создание первого игрока:
    gPlayer1 := g_Player_Get(g_Player_Create(gPlayer1Settings.Model,
                                             gPlayer1Settings.Color,
                                             gPlayer1Settings.Team, False));
    if gPlayer1 = nil then
    begin
      g_FatalError(Format(_lc[I_GAME_ERROR_PLAYER_CREATE], [1]));
      Exit;
    end;

    gPlayer1.Name := gPlayer1Settings.Name;
    gPlayer1.WeapSwitchMode := gPlayer1Settings.WeaponSwitch;
    gPlayer1.setWeaponPrefs(gPlayer1Settings.WeaponPreferences);
    gPlayer1.SwitchToEmpty := gPlayer1Settings.SwitchToEmpty;
    gPlayer1.SkipFist := gPlayer1Settings.SkipFist;
    Inc(nPl);
  end;

  if nPlayers >= 2 then
  begin
  // Создание второго игрока:
    gPlayer2 := g_Player_Get(g_Player_Create(gPlayer2Settings.Model,
                                             gPlayer2Settings.Color,
                                             gPlayer2Settings.Team, False));
    if gPlayer2 = nil then
    begin
      g_FatalError(Format(_lc[I_GAME_ERROR_PLAYER_CREATE], [2]));
      Exit;
    end;

    gPlayer2.Name := gPlayer2Settings.Name;
    gPlayer2.WeapSwitchMode := gPlayer2Settings.WeaponSwitch;
    gPlayer2.setWeaponPrefs(gPlayer2Settings.WeaponPreferences);
    gPlayer2.SwitchToEmpty := gPlayer2Settings.SwitchToEmpty;
    gPlayer2.SkipFist := gPlayer2Settings.SkipFist;
    Inc(nPl);
  end;

// Загрузка и запуск карты:
  if not g_Game_StartMap(true{asMegawad}, Map, True) then
  begin
    g_FatalError(Format(_lc[I_GAME_ERROR_MAP_LOAD], [Map]));
    Exit;
  end;

// Нет точек появления:
  if (g_Map_GetPointCount(RESPAWNPOINT_PLAYER1) +
      g_Map_GetPointCount(RESPAWNPOINT_PLAYER2) +
      g_Map_GetPointCount(RESPAWNPOINT_DM) +
      g_Map_GetPointCount(RESPAWNPOINT_RED)+
      g_Map_GetPointCount(RESPAWNPOINT_BLUE)) < 1 then
  begin
    g_FatalError(_lc[I_GAME_ERROR_GET_SPAWN]);
    Exit;
  end;

// Настройки игроков и ботов:
  g_Player_Init();

// Создаем ботов:
  for i := nPl+1 to nPlayers do
    g_Player_Create(STD_PLAYER_MODEL, _RGB(0, 0, 0), 0, True);
end;

procedure g_Game_StartServer(Map: String; GameMode: Byte;
                             TimeLimit, ScoreLimit: Word; MaxLives: Byte;
                             Options: LongWord; nPlayers: Byte;
                             IPAddr: LongWord; Port: Word);
begin
  g_Game_Free();
  g_Net_Slist_ServerClosed();

  e_WriteLog('Starting net game (server)...', TMsgType.Notify);

  g_Game_ClearLoading();

  ClearDebugCvars();

// Настройки игры:
  gGameSettings.GameType := GT_SERVER;
  gGameSettings.GameMode := GameMode;
  gSwitchGameMode := GameMode;
  gGameSettings.TimeLimit := TimeLimit;
  gGameSettings.ScoreLimit := ScoreLimit;
  gGameSettings.MaxLives := IfThen(GameMode = GM_CTF, 0, MaxLives);
  gGameSettings.Options := Options;

  gCoopTotalMonstersKilled := 0;
  gCoopTotalSecretsFound := 0;
  gCoopTotalMonsters := 0;
  gCoopTotalSecrets := 0;
  gAimLine := False;
  gShowMap := False;

  gLMSRespawn := LMS_RESPAWN_NONE;
  gLMSRespawnTime := 0;
  gSpectLatchPID1 := 0;
  gSpectLatchPID2 := 0;

  g_Game_ExecuteEvent('ongamestart');

// Установка размеров окна игрока
  g_Game_SetupScreenSize();

// Режим наблюдателя:
  if nPlayers = 0 then
  begin
    gPlayer1 := nil;
    gPlayer2 := nil;
  end;

  if nPlayers >= 1 then
  begin
  // Создание первого игрока:
    gPlayer1 := g_Player_Get(g_Player_Create(gPlayer1Settings.Model,
                                             gPlayer1Settings.Color,
                                             gPlayer1Settings.Team, False));
    if gPlayer1 = nil then
    begin
      g_FatalError(Format(_lc[I_GAME_ERROR_PLAYER_CREATE], [1]));
      Exit;
    end;

    gPlayer1.Name := gPlayer1Settings.Name;
    gPlayer1.WeapSwitchMode := gPlayer1Settings.WeaponSwitch;
    gPlayer1.setWeaponPrefs(gPlayer1Settings.WeaponPreferences);
    gPlayer1.SwitchToEmpty := gPlayer1Settings.SwitchToEmpty;
    gPlayer1.SkipFist := gPlayer1Settings.SkipFist;
  end;

  if nPlayers >= 2 then
  begin
  // Создание второго игрока:
    gPlayer2 := g_Player_Get(g_Player_Create(gPlayer2Settings.Model,
                                             gPlayer2Settings.Color,
                                             gPlayer2Settings.Team, False));
    if gPlayer2 = nil then
    begin
      g_FatalError(Format(_lc[I_GAME_ERROR_PLAYER_CREATE], [2]));
      Exit;
    end;

    gPlayer2.Name := gPlayer2Settings.Name;
    gPlayer2.WeapSwitchMode := gPlayer2Settings.WeaponSwitch;
    gPlayer2.setWeaponPrefs(gPlayer2Settings.WeaponPreferences);
    gPlayer2.SwitchToEmpty := gPlayer2Settings.SwitchToEmpty;
    gPlayer2.SkipFist := gPlayer2Settings.SkipFist;
  end;

  g_Game_SetLoadingText(_lc[I_LOAD_HOST], 0, False);
  if NetForwardPorts then
    g_Game_SetLoadingText(_lc[I_LOAD_PORTS], 0, False);

// Стартуем сервер
  if not g_Net_Host(IPAddr, Port, NetMaxClients) then
  begin
    g_FatalError(_lc[I_NET_MSG] + Format(_lc[I_NET_ERR_HOST], [Port]));
    Exit;
  end;

  g_Net_Slist_Set(NetMasterList);

  g_Net_Slist_ServerStarted();

// Загрузка и запуск карты:
  if not g_Game_StartMap(false{asMegawad}, Map, True) then
  begin
    g_Net_Slist_ServerClosed();
    g_FatalError(Format(_lc[I_GAME_ERROR_MAP_LOAD], [Map]));
    Exit;
  end;

// Нет точек появления:
  if (g_Map_GetPointCount(RESPAWNPOINT_PLAYER1) +
      g_Map_GetPointCount(RESPAWNPOINT_PLAYER2) +
      g_Map_GetPointCount(RESPAWNPOINT_DM) +
      g_Map_GetPointCount(RESPAWNPOINT_RED)+
      g_Map_GetPointCount(RESPAWNPOINT_BLUE)) < 1 then
  begin
    g_Net_Slist_ServerClosed();
    g_FatalError(_lc[I_GAME_ERROR_GET_SPAWN]);
    Exit;
  end;

// Настройки игроков и ботов:
  g_Player_Init();

  g_Net_Slist_ServerMapStarted();
  NetState := NET_STATE_GAME;
end;

procedure g_Game_StartClient(Addr: String; Port: Word; PW: String);
var
  Map: String;
  WadName: string;
  Ptr: Pointer;
  T: Cardinal;
  MID: Byte;
  State: Byte;
  OuterLoop: Boolean;
  newResPath: string;
  InMsg: TMsg;
begin
  g_Game_Free();

  State := 0;
  e_WriteLog('Starting net game (client)...', TMsgType.Notify);
  e_WriteLog('NET: Trying to connect to ' + Addr + ':' + IntToStr(Port) + '...', TMsgType.Notify);

  g_Game_ClearLoading();

  ClearDebugCvars();

// Настройки игры:
  gGameSettings.GameType := GT_CLIENT;

  gCoopTotalMonstersKilled := 0;
  gCoopTotalSecretsFound := 0;
  gCoopTotalMonsters := 0;
  gCoopTotalSecrets := 0;
  gAimLine := False;
  gShowMap := False;

  g_Game_ExecuteEvent('ongamestart');

// Установка размеров окон игроков:
  g_Game_SetupScreenSize();

  NetState := NET_STATE_AUTH;

  g_Game_SetLoadingText(_lc[I_LOAD_CONNECT], 0, False);

  // create (or update) map/resource databases
  g_Res_CreateDatabases(true);

  gLMSRespawn := LMS_RESPAWN_NONE;
  gLMSRespawnTime := 0;
  gSpectLatchPID1 := 0;
  gSpectLatchPID2 := 0;

// Стартуем клиент
  if not g_Net_Connect(Addr, Port) then
  begin
    g_FatalError(_lc[I_NET_MSG] + _lc[I_NET_ERR_CONN]);
    NetState := NET_STATE_NONE;
    Exit;
  end;

  g_Game_SetLoadingText(_lc[I_LOAD_SEND_INFO], 0, False);
  MC_SEND_Info(PW);
  g_Game_SetLoadingText(_lc[I_LOAD_WAIT_INFO], 0, False);

  OuterLoop := True;
  while OuterLoop do
  begin
    // fuck! https://www.mail-archive.com/enet-discuss@cubik.org/msg00852.html
    // tl;dr: on shitdows, we can get -1 sometimes, and it is *NOT* a failure.
    //        thank you, enet. let's ignore failures altogether then.
    while (enet_host_service(NetHost, @NetEvent, 50) > 0) do
    begin
      if (NetEvent.kind = ENET_EVENT_TYPE_RECEIVE) then
      begin
        Ptr := NetEvent.packet^.data;
        if not InMsg.Init(Ptr, NetEvent.packet^.dataLength, True) then
        begin
          enet_packet_destroy(NetEvent.packet);
          continue;
        end;

        InMsg.ReadLongWord(); // skip size
        MID := InMsg.ReadByte();

        if (MID = NET_MSG_INFO) and (State = 0) then
        begin
          NetMyID := InMsg.ReadByte();
          NetPlrUID1 := InMsg.ReadWord();

          WadName := InMsg.ReadString();
          Map := InMsg.ReadString();

          gWADHash := InMsg.ReadMD5();

          gGameSettings.GameMode := InMsg.ReadByte();
          gSwitchGameMode := gGameSettings.GameMode;
          gGameSettings.ScoreLimit := InMsg.ReadWord();
          gGameSettings.TimeLimit := InMsg.ReadWord();
          gGameSettings.MaxLives := InMsg.ReadByte();
          gGameSettings.Options := InMsg.ReadLongWord();
          T := InMsg.ReadLongWord();

          //newResPath := g_Res_SearchSameWAD(MapsDir, WadName, gWADHash);
          //if newResPath = '' then
          begin
            //g_Game_SetLoadingText(_lc[I_LOAD_DL_RES], 0, False);
            newResPath := g_Res_DownloadMapWAD(ExtractFileName(WadName), gWADHash);
            if newResPath = '' then
            begin
              g_FatalError(_lc[I_NET_ERR_HASH]);
              enet_packet_destroy(NetEvent.packet);
              NetState := NET_STATE_NONE;
              Exit;
            end;
            e_LogWritefln('using downloaded map wad [%s] for [%s]`', [newResPath, WadName], TMsgType.Notify);
          end;
          //newResPath := ExtractRelativePath(MapsDir, newResPath);


          gPlayer1 := g_Player_Get(g_Player_Create(gPlayer1Settings.Model,
                                                   gPlayer1Settings.Color,
                                                   gPlayer1Settings.Team, False));

          if gPlayer1 = nil then
          begin
            g_FatalError(Format(_lc[I_GAME_ERROR_PLAYER_CREATE], [1]));

            enet_packet_destroy(NetEvent.packet);
            NetState := NET_STATE_NONE;
            Exit;
          end;

          gPlayer1.Name := gPlayer1Settings.Name;
          gPlayer1.WeapSwitchMode := gPlayer1Settings.WeaponSwitch;
          gPlayer1.setWeaponPrefs(gPlayer1Settings.WeaponPreferences);
          gPlayer1.SwitchToEmpty := gPlayer1Settings.SwitchToEmpty;
          gPlayer1.SkipFist := gPlayer1Settings.SkipFist;
          gPlayer1.UID := NetPlrUID1;
          gPlayer1.Reset(True);

          if not g_Game_StartMap(false{asMegawad}, newResPath + ':\' + Map, True) then
          begin
            g_FatalError(Format(_lc[I_GAME_ERROR_MAP_LOAD], [WadName + ':\' + Map]));

            enet_packet_destroy(NetEvent.packet);
            NetState := NET_STATE_NONE;
            Exit;
          end;

          gTime := T;

          State := 1;
          OuterLoop := False;
          enet_packet_destroy(NetEvent.packet);
          break;
        end
        else
          enet_packet_destroy(NetEvent.packet);
      end
      else
      begin
        if (NetEvent.kind = ENET_EVENT_TYPE_DISCONNECT) then
        begin
          State := 0;
          if (NetEvent.data <= NET_DISC_MAX) then
            g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' ' +
            _lc[TStrings_Locale(Cardinal(I_NET_DISC_NONE) + NetEvent.data)], True);
          OuterLoop := False;
          Break;
        end;
      end;
    end;

    ProcessLoading(True);
    if g_Net_UserRequestExit() then
    begin
      State := 0;
      break;
    end;
  end;

  if State <> 1 then
  begin
    g_FatalError(_lc[I_NET_MSG] + _lc[I_NET_ERR_CONN]);
    NetState := NET_STATE_NONE;
    Exit;
  end;

  g_Player_Init();
  NetState := NET_STATE_GAME;
  MC_SEND_FullStateRequest;
  e_WriteLog('NET: Connection successful.', TMsgType.Notify);
end;

var
  lastAsMegaWad: Boolean = false;

procedure g_Game_ChangeMap(const MapPath: String);
var
  Force: Boolean;
begin
  g_Game_ClearLoading();

  Force := gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF];
  // Если уровень завершился по триггеру Выход, не очищать инвентарь
  if gExitByTrigger then
  begin
    Force := False;
    gExitByTrigger := False;
  end;
  if not g_Game_StartMap(lastAsMegaWad, MapPath, Force) then
    g_FatalError(Format(_lc[I_GAME_ERROR_MAP_LOAD], [MapPath]));
end;

procedure g_Game_Restart();
var
  Map: string;
begin
  if g_Game_IsClient then
    Exit;
  map := g_ExtractFileName(gMapInfo.Map);
  e_LogWritefln('g_Game_Restart: map = "%s" gCurrentMapFileName = "%s"', [map, gCurrentMapFileName]);

  MessageTime := 0;
  gGameOn := False;
  g_Game_ClearLoading();
  g_Game_StartMap(lastAsMegaWad, Map, True, gCurrentMapFileName);
end;

function g_Game_StartMap (asMegawad: Boolean; Map: String; Force: Boolean = False; const oldMapPath: AnsiString=''): Boolean;
var
  NewWAD, ResName: String;
  I: Integer;
  nws: AnsiString;
begin
  g_Map_Free((Map <> gCurrentMapFileName) and (oldMapPath <> gCurrentMapFileName));
  g_Player_RemoveAllCorpses();

  if (not g_Game_IsClient) and
     (gSwitchGameMode <> gGameSettings.GameMode) and
     (gGameSettings.GameMode <> GM_SINGLE) then
  begin
    if gSwitchGameMode = GM_CTF then
      gGameSettings.MaxLives := 0;
    gGameSettings.GameMode := gSwitchGameMode;
    Force := True;
  end else
    gSwitchGameMode := gGameSettings.GameMode;

  g_Player_ResetTeams();

  lastAsMegaWad := asMegawad;
  if isWadPath(Map) then
  begin
    NewWAD := g_ExtractWadName(Map);
    ResName := g_ExtractFileName(Map);
    if g_Game_IsServer then
    begin
      nws := findDiskWad(NewWAD);
      //writeln('000: Map=[', Map, ']; nws=[', nws, ']; NewWAD=[', NewWAD, ']');
      if (asMegawad) then
      begin
        if (length(nws) = 0) then nws := e_FindWad(MegawadDirs, NewWAD);
        if (length(nws) = 0) then nws := e_FindWad(MapDirs, NewWAD);
      end
      else
      begin
        if (length(nws) = 0) then nws := e_FindWad(MapDirs, NewWAD);
        if (length(nws) = 0) then nws := e_FindWad(MegawadDirs, NewWAD);
      end;
      //if (length(nws) = 0) then nws := e_FindWad(MapDownloadDirs, NewWAD);
      //writeln('001: Map=[', Map, ']; nws=[', nws, ']; NewWAD=[', NewWAD, ']');
      //nws := NewWAD;
      if (length(nws) = 0) then
      begin
        ResName := ''; // failed
      end
      else
      begin
        NewWAD := nws;
        if (g_Game_IsNet) then gWADHash := MD5File(nws);
        //writeln('********: nws=', nws, ' : Map=', Map, ' : nw=', NewWAD, ' : resname=', ResName);
        g_Game_LoadWAD(NewWAD);
      end;
    end
    else
    begin
      // hash received in MC_RECV_GameEvent -> NET_EV_MAPSTART
      NewWAD := g_Game_ClientWAD(NewWAD, gWADHash);
    end;
  end
  else
  begin
    NewWAD := gGameSettings.WAD;
    ResName := Map;
  end;

  gTime := 0;

  //writeln('********: gsw=', gGameSettings.WAD, '; rn=', ResName);
  result := false;
  if (ResName <> '') and (NewWAD <> '') then
  begin
    //result := g_Map_Load(gGameSettings.WAD + ':\' + ResName);
    result := g_Map_Load(NewWAD+':\'+ResName);
  end;
  if Result then
    begin
      g_Player_ResetAll(Force or gLastMap, gGameSettings.GameType = GT_SINGLE);

      gState := STATE_NONE;
      g_ActiveWindow := nil;
      gGameOn := True;

      DisableCheats();
      ResetTimer();

      if gGameSettings.GameMode = GM_CTF then
      begin
        g_Map_ResetFlag(FLAG_RED);
        g_Map_ResetFlag(FLAG_BLUE);
        // CTF, а флагов нет:
        if not g_Map_HaveFlagPoints() then
          g_SimpleError(_lc[I_GAME_ERROR_CTF]);
      end;
    end
  else
    begin
      gState := STATE_MENU;
      gGameOn := False;
    end;

  gExit := 0;
  gPauseMain := false;
  gPauseHolmes := false;
  NetTimeToUpdate := 1;
  NetTimeToReliable := 0;
  NetTimeToMaster := NetMasterRate;
  gSpectLatchPID1 := 0;
  gSpectLatchPID2 := 0;
  gMissionFailed := False;
  gNextMap := '';

  gCoopMonstersKilled := 0;
  gCoopSecretsFound := 0;

  gVoteInProgress := False;
  gVotePassed := False;
  gVoteCount := 0;
  gVoted := False;

  gStatsOff := False;

  if not gGameOn then Exit;

  g_Game_SpectateCenterView();

  if g_Game_IsServer then
  begin
    if (gGameSettings.MaxLives > 0) and (gGameSettings.WarmupTime > 0) then
    begin
      gLMSRespawn := LMS_RESPAWN_WARMUP;
      gLMSRespawnTime := gTime + gGameSettings.WarmupTime*1000;
      gLMSSoftSpawn := True;
      if g_Game_IsNet then
        MH_SEND_GameEvent(NET_EV_LMS_WARMUP, gLMSRespawnTime - gTime);
    end
    else
    begin
      gLMSRespawn := LMS_RESPAWN_NONE;
      gLMSRespawnTime := 0;
    end;
  end;

  if NetMode = NET_SERVER then
  begin
  // reset full state flags
    if NetClients <> nil then
      for I := 0 to High(NetClients) do
        NetClients[I].FullUpdateSent := False;

    MH_SEND_GameEvent(NET_EV_MAPSTART, gGameSettings.GameMode, Map);

  // Мастерсервер
    g_Net_Slist_ServerMapStarted();

    if NetClients <> nil then
      for I := 0 to High(NetClients) do
        if NetClients[I].Used then
        begin
          NetClients[I].Voted := False;
          if NetClients[I].RequestedFullUpdate then
          begin
            MH_SEND_Everything((NetClients[I].State = NET_STATE_AUTH), I);
            NetClients[I].RequestedFullUpdate := False;
          end;
        end;

    g_Net_UnbanNonPermHosts();
  end;

  if gLastMap then
  begin
    gCoopTotalMonstersKilled := 0;
    gCoopTotalSecretsFound := 0;
    gCoopTotalMonsters := 0;
    gCoopTotalSecrets := 0;
    gLastMap := False;
  end;

  g_Game_ExecuteEvent('onmapstart');
end;

procedure SetFirstLevel;
begin
  gNextMap := '';

  MapList := g_Map_GetMapsList(gGameSettings.WAD);
  if MapList = nil then
    Exit;

  SortSArray(MapList);
  gNextMap := MapList[Low(MapList)];

  MapList := nil;
end;

procedure g_Game_ExitLevel(const Map: AnsiString);
begin
  gNextMap := Map;

  gCoopTotalMonstersKilled := gCoopTotalMonstersKilled + gCoopMonstersKilled;
  gCoopTotalSecretsFound := gCoopTotalSecretsFound + gCoopSecretsFound;
  gCoopTotalMonsters := gCoopTotalMonsters + gTotalMonsters;
  gCoopTotalSecrets := gCoopTotalSecrets + gSecretsCount;

// Вышли в выход в Одиночной игре:
  if gGameSettings.GameType = GT_SINGLE then
    gExit := EXIT_ENDLEVELSINGLE
  else // Вышли в выход в Своей игре
  begin
    gExit := EXIT_ENDLEVELCUSTOM;
    if gGameSettings.GameMode = GM_COOP then
      g_Player_RememberAll;

    if not g_Map_Exist(gGameSettings.WAD + ':\' + gNextMap) then
    begin
      gLastMap := True;
      if gGameSettings.GameMode = GM_COOP then
        gStatsOff := True;

      gStatsPressed := True;
      gNextMap := 'MAP01';

      if not g_Map_Exist(gGameSettings.WAD + ':\' + gNextMap) then
        g_Game_NextLevel;

      if g_Game_IsNet then
      begin
        MH_SEND_GameStats();
        MH_SEND_CoopStats();
      end;
    end;
  end;
end;

procedure g_Game_RestartLevel();
var
  Map: string;
begin
  if gGameSettings.GameMode = GM_SINGLE then
  begin
    g_Game_Restart();
    Exit;
  end;
  gExit := EXIT_ENDLEVELCUSTOM;
  Map := g_ExtractFileName(gMapInfo.Map);
  gNextMap := Map;
end;

function g_Game_ClientWAD (NewWAD: String; const WHash: TMD5Digest): AnsiString;
var
  gWAD{, xwad}: String;
begin
  result := NewWAD;
  if not g_Game_IsClient then Exit;
  //e_LogWritefln('*** g_Game_ClientWAD: `%s`', [NewWAD]);

  gWAD := g_Res_DownloadMapWAD(ExtractFileName(NewWAD), WHash);
  if gWAD = '' then
  begin
    result := '';
    g_Game_Free();
    g_FatalError(Format(_lc[I_GAME_ERROR_MAP_WAD], [ExtractFileName(NewWAD)]));
    Exit;
  end;

  e_LogWritefln('using downloaded client map wad [%s] for [%s]', [gWAD, NewWAD], TMsgType.Notify);
  NewWAD := gWAD;

  g_Game_LoadWAD(NewWAD);
  result := NewWAD;

  {
  if LowerCase(NewWAD) = LowerCase(gGameSettings.WAD) then Exit;
  gWAD := g_Res_SearchSameWAD(MapsDir, ExtractFileName(NewWAD), WHash);
  if gWAD = '' then
  begin
    g_Game_SetLoadingText(_lc[I_LOAD_DL_RES], 0, False);
    gWAD := g_Res_DownloadMapWAD(ExtractFileName(NewWAD), WHash);
    if gWAD = '' then
    begin
      g_Game_Free();
      g_FatalError(Format(_lc[I_GAME_ERROR_MAP_WAD], [ExtractFileName(NewWAD)]));
      Exit;
    end;
  end;
  NewWAD := ExtractRelativePath(MapsDir, gWAD);
  g_Game_LoadWAD(NewWAD);
  }
end;

procedure g_Game_RestartRound(NoMapRestart: Boolean = False);
var
  i, n, nb, nr: Integer;
begin
  if not g_Game_IsServer then Exit;
  if gLMSRespawn = LMS_RESPAWN_NONE then Exit;
  gLMSRespawn := LMS_RESPAWN_NONE;
  gLMSRespawnTime := 0;
  MessageTime := 0;

  if (gGameSettings.GameMode = GM_COOP) and not NoMapRestart then
  begin
    gMissionFailed := True;
    g_Game_RestartLevel;
    Exit;
  end;

  n := 0; nb := 0; nr := 0;
  for i := Low(gPlayers) to High(gPlayers) do
    if (gPlayers[i] <> nil) and
       ((not gPlayers[i].FSpectator) or gPlayers[i].FWantsInGame or
        (gPlayers[i] is TBot)) then
      begin
        Inc(n);
        if gPlayers[i].Team = TEAM_RED then Inc(nr)
        else if gPlayers[i].Team = TEAM_BLUE then Inc(nb)
      end;

  if (n < 1) or ((gGameSettings.GameMode = GM_TDM) and ((nr = 0) or (nb = 0))) then
  begin
    // wait a second until the fuckers finally decide to join
    gLMSRespawn := LMS_RESPAWN_WARMUP;
    gLMSRespawnTime := gTime + gGameSettings.WarmupTime*1000;
    gLMSSoftSpawn := NoMapRestart;
    if g_Game_IsNet then
      MH_SEND_GameEvent(NET_EV_LMS_WARMUP, gLMSRespawnTime - gTime);
    Exit;
  end;

  g_Player_RemoveAllCorpses;
  g_Game_Message(_lc[I_MESSAGE_LMS_START], 144);
  if g_Game_IsNet then
    MH_SEND_GameEvent(NET_EV_LMS_START);

  for i := Low(gPlayers) to High(gPlayers) do
  begin
    if gPlayers[i] = nil then continue;
    if gPlayers[i] is TBot then gPlayers[i].FWantsInGame := True;
    // don't touch normal spectators
    if gPlayers[i].FSpectator and not gPlayers[i].FWantsInGame then
    begin
      gPlayers[i].FNoRespawn := True;
      gPlayers[i].Lives := 0;
      if g_Game_IsNet then
        MH_SEND_PlayerStats(gPlayers[I].UID);
      continue;
    end;
    gPlayers[i].FNoRespawn := False;
    gPlayers[i].Lives := gGameSettings.MaxLives;
    gPlayers[i].Respawn(False, True);
    if gGameSettings.GameMode = GM_COOP then
    begin
      gPlayers[i].Frags := 0;
      gPlayers[i].RecallState;
    end;
    if (gPlayer1 = nil) and (gSpectLatchPID1 > 0) then
      gPlayer1 := g_Player_Get(gSpectLatchPID1);
    if (gPlayer2 = nil) and (gSpectLatchPID2 > 0) then
      gPlayer2 := g_Player_Get(gSpectLatchPID2);
  end;

  g_Items_RestartRound();

  gLMSSoftSpawn := False;
end;

function g_Game_GetFirstMap(WAD: String): String;
begin
  Result := '';

  MapList := g_Map_GetMapsList(WAD);
  if MapList = nil then
    Exit;

  SortSArray(MapList);
  Result := MapList[Low(MapList)];

  if not g_Map_Exist(WAD + ':\' + Result) then
    Result := '';

  MapList := nil;
end;

function g_Game_GetNextMap(): String;
var
  I: Integer;
  Map: string;
begin
  Result := '';

  MapList := g_Map_GetMapsList(gGameSettings.WAD);
  if MapList = nil then
    Exit;

  Map := g_ExtractFileName(gMapInfo.Map);

  SortSArray(MapList);
  MapIndex := -255;
  for I := Low(MapList) to High(MapList) do
    if Map = MapList[I] then
    begin
      MapIndex := I;
      Break;
    end;

  if MapIndex <> -255 then
  begin
    if MapIndex = High(MapList) then
     Result := MapList[Low(MapList)]
    else
      Result := MapList[MapIndex + 1];

    if not g_Map_Exist(gGameSettings.WAD + ':\' + Result) then Result := Map;
  end;

  MapList := nil;
end;

procedure g_Game_NextLevel();
begin
  if gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF, GM_COOP] then
    gExit := EXIT_ENDLEVELCUSTOM
  else
  begin
    gExit := EXIT_ENDLEVELSINGLE;
    Exit;
  end;

  if gNextMap <> '' then Exit;
  gNextMap := g_Game_GetNextMap();
end;

function g_Game_IsTestMap(): Boolean;
begin
  result := StrEquCI1251(TEST_MAP_NAME, g_ExtractFileName(gMapInfo.Map));
end;

procedure g_Game_DeleteTestMap();
var
  a: Integer;
  //MapName: AnsiString;
  WadName: string;
{
  WAD: TWADFile;
  MapList: SSArray;
  time: Integer;
}
begin
  a := Pos('.wad:\', toLowerCase1251(gMapToDelete));
  if (a = 0) then a := Pos('.wad:/', toLowerCase1251(gMapToDelete));
  if (a = 0) then exit;

  // Выделяем имя wad-файла и имя карты
  WadName := Copy(gMapToDelete, 1, a+3);
  Delete(gMapToDelete, 1, a+5);
  gMapToDelete := UpperCase(gMapToDelete);
  //MapName := '';
  //CopyMemory(@MapName[0], @gMapToDelete[1], Min(16, Length(gMapToDelete)));

{
// Имя карты не стандартное тестовое:
  if MapName <> TEST_MAP_NAME then
    Exit;

  if not gTempDelete then
  begin
    time := g_GetFileTime(WadName);
    WAD := TWADFile.Create();

  // Читаем Wad-файл:
    if not WAD.ReadFile(WadName) then
    begin // Нет такого WAD-файла
      WAD.Free();
      Exit;
    end;

  // Составляем список карт и ищем нужную:
    WAD.CreateImage();
    MapList := WAD.GetResourcesList('');

    if MapList <> nil then
      for a := 0 to High(MapList) do
        if MapList[a] = MapName then
        begin
        // Удаляем и сохраняем:
          WAD.RemoveResource('', MapName);
          WAD.SaveTo(WadName);
          Break;
        end;

    WAD.Free();
    g_SetFileTime(WadName, time);
  end else
}
  if gTempDelete then DeleteFile(WadName);
end;

procedure GameCVars(P: SSArray);
var
  a, b: Integer;
  stat: TPlayerStatArray;
  cmd: string;

  procedure ParseGameFlag(Flag: LongWord; OffMsg, OnMsg: TStrings_Locale; OnMapChange: Boolean = False);
  var
    x: Boolean;
  begin
    if Length(P) > 1 then
    begin
      x := P[1] = '1';

      if x then
        gsGameFlags := gsGameFlags or Flag
      else
        gsGameFlags := gsGameFlags and (not Flag);

      if g_Game_IsServer then
      begin
        if x then
          gGameSettings.Options := gGameSettings.Options or Flag
        else
          gGameSettings.Options := gGameSettings.Options and (not Flag);
        if g_Game_IsNet then MH_SEND_GameSettings;
      end;
    end;

    if LongBool(gsGameFlags and Flag) then
      g_Console_Add(_lc[OnMsg])
    else
      g_Console_Add(_lc[OffMsg]);

    if OnMapChange and g_Game_IsServer then
      g_Console_Add(_lc[I_MSG_ONMAPCHANGE]);
  end;

begin
  stat := nil;
  cmd := LowerCase(P[0]);

  if cmd = 'g_gamemode' then
  begin
    if (Length(P) > 1) then
    begin
      a := g_Game_TextToMode(P[1]);
      if a = GM_SINGLE then a := GM_COOP;
      gsGameMode := g_Game_ModeToText(a);
      if g_Game_IsServer then
      begin
        gSwitchGameMode := a;
        if (gGameOn and (gGameSettings.GameMode = GM_SINGLE)) or
           (gState = STATE_INTERSINGLE) then
          gSwitchGameMode := GM_SINGLE;
        if not gGameOn then
          gGameSettings.GameMode := gSwitchGameMode;
      end;
    end;

    if gSwitchGameMode = gGameSettings.GameMode then
      g_Console_Add(Format(_lc[I_MSG_GAMEMODE_CURRENT],
                          [g_Game_ModeToText(gGameSettings.GameMode)]))
    else
      g_Console_Add(Format(_lc[I_MSG_GAMEMODE_CHANGE],
                          [g_Game_ModeToText(gGameSettings.GameMode),
                           g_Game_ModeToText(gSwitchGameMode)]));
  end
  else if cmd = 'g_friendlyfire' then
  begin
    ParseGameFlag(GAME_OPTION_TEAMDAMAGE, I_MSG_FRIENDLY_FIRE_OFF, I_MSG_FRIENDLY_FIRE_ON);
  end
  else if cmd = 'g_friendly_absorb_damage' then
  begin
    ParseGameFlag(GAME_OPTION_TEAMABSORBDAMAGE, I_MSG_FRIENDLY_ABSORB_DAMAGE_OFF, I_MSG_FRIENDLY_ABSORB_DAMAGE_ON);
  end
  else if cmd = 'g_friendly_hit_trace' then
  begin
    ParseGameFlag(GAME_OPTION_TEAMHITTRACE, I_MSG_FRIENDLY_HIT_TRACE_OFF, I_MSG_FRIENDLY_HIT_TRACE_ON);
  end
  else if cmd = 'g_friendly_hit_projectile' then
  begin
    ParseGameFlag(GAME_OPTION_TEAMHITPROJECTILE, I_MSG_FRIENDLY_PROJECT_TRACE_OFF, I_MSG_FRIENDLY_PROJECT_TRACE_ON);
  end
  else if cmd = 'g_weaponstay' then
  begin
    ParseGameFlag(GAME_OPTION_WEAPONSTAY, I_MSG_WEAPONSTAY_OFF, I_MSG_WEAPONSTAY_ON);
  end
  else if cmd = 'g_allow_exit' then
  begin
    ParseGameFlag(GAME_OPTION_ALLOWEXIT, I_MSG_ALLOWEXIT_OFF, I_MSG_ALLOWEXIT_ON, True);
  end
  else if cmd = 'g_allow_monsters' then
  begin
    ParseGameFlag(GAME_OPTION_MONSTERS, I_MSG_ALLOWMON_OFF, I_MSG_ALLOWMON_ON, True);
  end
  else if cmd = 'g_allow_dropflag' then
  begin
    ParseGameFlag(GAME_OPTION_ALLOWDROPFLAG, I_MSG_ALLOWDROPFLAG_OFF, I_MSG_ALLOWDROPFLAG_ON);
  end
  else if cmd = 'g_throw_flag' then
  begin
    ParseGameFlag(GAME_OPTION_THROWFLAG, I_MSG_THROWFLAG_OFF, I_MSG_THROWFLAG_ON);
  end
  else if cmd = 'g_bot_vsplayers' then
  begin
    ParseGameFlag(GAME_OPTION_BOTVSPLAYER, I_MSG_BOTSVSPLAYERS_OFF, I_MSG_BOTSVSPLAYERS_ON);
  end
  else if cmd = 'g_bot_vsmonsters' then
  begin
    ParseGameFlag(GAME_OPTION_BOTVSMONSTER, I_MSG_BOTSVSMONSTERS_OFF, I_MSG_BOTSVSMONSTERS_ON);
  end
  else if cmd = 'g_dm_keys' then
  begin
    ParseGameFlag(GAME_OPTION_DMKEYS, I_MSG_DMKEYS_OFF, I_MSG_DMKEYS_ON, True);
  end
  else if cmd = 'g_gameflags' then
  begin
    if Length(P) > 1 then
    begin
      gsGameFlags := StrToDWordDef(P[1], gsGameFlags);
      if g_Game_IsServer then
      begin
        gGameSettings.Options := gsGameFlags;
        if g_Game_IsNet then MH_SEND_GameSettings;
      end;
    end;

    g_Console_Add(Format('%s %u', [cmd, gsGameFlags]));
  end
  else if cmd = 'g_warmup_time' then
  begin
    if Length(P) > 1 then
    begin
      gsWarmupTime := nclamp(StrToIntDef(P[1], gsWarmupTime), 0, $FFFF);
      if g_Game_IsServer then
      begin
        gGameSettings.WarmupTime := gsWarmupTime;
        // extend warmup if it's already going
        if gLMSRespawn = LMS_RESPAWN_WARMUP then
        begin
          gLMSRespawnTime := gTime + gsWarmupTime * 1000;
          if g_Game_IsNet then MH_SEND_GameEvent(NET_EV_LMS_WARMUP, gLMSRespawnTime - gTime);
        end;
        if g_Game_IsNet then MH_SEND_GameSettings;
      end;
    end;

    g_Console_Add(Format(_lc[I_MSG_WARMUP], [Integer(gsWarmupTime)]));
    if g_Game_IsServer then g_Console_Add(_lc[I_MSG_ONMAPCHANGE]);
  end
  else if cmd = 'g_spawn_invul' then
  begin
    if Length(P) > 1 then
    begin
      gsSpawnInvul := nclamp(StrToIntDef(P[1], gsSpawnInvul), 0, $FFFF);
      if g_Game_IsServer then
      begin
        gGameSettings.SpawnInvul := gsSpawnInvul;
        if g_Game_IsNet then MH_SEND_GameSettings;
      end;
    end;

    g_Console_Add(Format('%s %d', [cmd, Integer(gsSpawnInvul)]));
  end
  else if cmd = 'g_item_respawn_time' then
  begin
    if Length(P) > 1 then
    begin
      gsItemRespawnTime := nclamp(StrToIntDef(P[1], gsItemRespawnTime), 0, $FFFF);
      if g_Game_IsServer then
      begin
        gGameSettings.ItemRespawnTime := gsItemRespawnTime;
        if g_Game_IsNet then MH_SEND_GameSettings;
      end;
    end;

    g_Console_Add(Format('%s %d', [cmd, Integer(gsItemRespawnTime)]));
    if g_Game_IsServer then g_Console_Add(_lc[I_MSG_ONMAPCHANGE]);
  end
  else if cmd = 'sv_intertime' then
  begin
    if (Length(P) > 1) then
      gDefInterTime := Min(Max(StrToIntDef(P[1], gDefInterTime), -1), 120);

    g_Console_Add(cmd + ' = ' + IntToStr(gDefInterTime));
  end
  else if cmd = 'g_max_particles' then
  begin
    if Length(p) = 2 then
    begin
      a := Max(0, StrToIntDef(p[1], 0));
      g_GFX_SetMax(a)
    end
    else if Length(p) = 1 then
    begin
      e_LogWritefln('%s', [g_GFX_GetMax()])
    end
    else
    begin
      e_LogWritefln('usage: %s <n>', [cmd])
    end
  end
  else if cmd = 'g_max_shells' then
  begin
    if Length(p) = 2 then
    begin
      a := Max(0, StrToIntDef(p[1], 0));
      g_Shells_SetMax(a)
    end
    else if Length(p) = 1 then
    begin
      e_LogWritefln('%s', [g_Shells_GetMax()])
    end
    else
    begin
      e_LogWritefln('usage: %s <n>', [cmd])
    end
  end
  else if cmd = 'g_max_gibs' then
  begin
    if Length(p) = 2 then
    begin
      a := Max(0, StrToIntDef(p[1], 0));
      g_Gibs_SetMax(a)
    end
    else if Length(p) = 1 then
    begin
      e_LogWritefln('%s', [g_Gibs_GetMax()])
    end
    else
    begin
      e_LogWritefln('usage: %s <n>', [cmd])
    end
  end
  else if cmd = 'g_max_corpses' then
  begin
    if Length(p) = 2 then
    begin
      a := Max(0, StrToIntDef(p[1], 0));
      g_Corpses_SetMax(a)
    end
    else if Length(p) = 1 then
    begin
      e_LogWritefln('%s', [g_Corpses_GetMax()])
    end
    else
    begin
      e_LogWritefln('usage: %s <n>', [cmd])
    end
  end
  else if cmd = 'g_force_model' then
  begin
    if Length(p) = 2 then
    begin
      a := StrToIntDef(p[1], 0);
      g_Force_Model_Set(a);
      if (g_Force_Model_Get() <> 0) and (gPlayers <> nil) then
      begin
        for a := Low(gPlayers) to High(gPlayers) do
        begin
          if (gPlayers[a] <> nil) then
          begin
            if (gPlayers[a].UID = gPlayer1.UID) then
              continue
            else if (gPlayer2 <> nil) and (gPlayers[a].UID = gPlayer2.UID) then
              continue;
            gPlayers[a].setModel(g_Forced_Model_GetName());
          end;
        end
      end
      else if (g_Force_Model_Get() = 0) and (gPlayers <> nil) then
      begin
        for a := Low(gPlayers) to High(gPlayers) do
        begin
          if (gPlayers[a] <> nil) then
          begin
            if (gPlayers[a].UID = gPlayer1.UID) then
              continue
            else if (gPlayer2 <> nil) and (gPlayers[a].UID = gPlayer2.UID) then
              continue;
            gPlayers[a].setModel(gPlayers[a].FActualModelName);
          end;
        end
      end
    end
  end
  else if cmd = 'g_force_model_name' then
  begin
    if (Length(P) > 1) then
    begin
      cmd := b_Text_Unformat(P[1]);
      g_Forced_Model_SetName(cmd);
      if (g_Force_Model_Get() <> 0) and (gPlayers <> nil) then
      begin
        for a := Low(gPlayers) to High(gPlayers) do
        begin
          if (gPlayers[a] <> nil) then
          begin
            if (gPlayers[a].UID = gPlayer1.UID) then
              continue
            else if (gPlayer2 <> nil) and (gPlayers[a].UID = gPlayer2.UID) then
              continue;
            gPlayers[a].setModel(g_Forced_Model_GetName());
          end;
        end
      end
    end
  end
  else if cmd = 'g_scorelimit' then
  begin
    if Length(P) > 1 then
    begin
      gsScoreLimit := nclamp(StrToIntDef(P[1], gsScoreLimit), 0, $FFFF);

      if g_Game_IsServer then
      begin
        b := 0;
        if gGameSettings.GameMode = GM_DM then
        begin // DM
          stat := g_Player_GetStats();
          if stat <> nil then
            for a := 0 to High(stat) do
              if stat[a].Frags > b then
                b := stat[a].Frags;
        end
        else // TDM/CTF
          b := Max(gTeamStat[TEAM_RED].Score, gTeamStat[TEAM_BLUE].Score);

        // if someone has a higher score, set it to that instead
        gsScoreLimit := max(gsScoreLimit, b);
        gGameSettings.ScoreLimit := gsScoreLimit;
 
        if g_Game_IsNet then MH_SEND_GameSettings;
      end;
    end;

    g_Console_Add(Format(_lc[I_MSG_SCORE_LIMIT], [Integer(gsScoreLimit)]));
  end
  else if cmd = 'g_timelimit' then
  begin
    if Length(P) > 1 then
    begin
      gsTimeLimit := nclamp(StrToIntDef(P[1], gsTimeLimit), 0, $FFFF);
      if g_Game_IsServer then
      begin
        gGameSettings.TimeLimit := gsTimeLimit;
        if g_Game_IsNet then MH_SEND_GameSettings;
      end;
    end;
    g_Console_Add(Format(_lc[I_MSG_TIME_LIMIT],
                         [gsTimeLimit div 3600,
                         (gsTimeLimit div 60) mod 60,
                          gsTimeLimit mod 60]));
  end
  else if cmd = 'g_max_bots' then
  begin
    if Length(P) > 1 then
      gMaxBots := nclamp(StrToIntDef(P[1], gMaxBots), 0, 127);
    g_Console_Add('g_max_bots = ' + IntToStr(gMaxBots));
  end
  else if cmd = 'g_maxlives' then
  begin
    if Length(P) > 1 then
    begin
      gsMaxLives := nclamp(StrToIntDef(P[1], gsMaxLives), 0, $FFFF);
      if g_Game_IsServer then
      begin
        gGameSettings.MaxLives := gsMaxLives;
        if g_Game_IsNet then MH_SEND_GameSettings;
      end;
    end;

    g_Console_Add(Format(_lc[I_MSG_LIVES], [Integer(gsMaxLives)]));
  end;
end;

procedure PlayerSettingsCVars(P: SSArray);
var
  cmd: string;
  team: Byte;

  function ParseTeam(s: string): Byte;
  begin
    result := 0;
    case s of
      'red', '1':  result := TEAM_RED;
      'blue', '2': result := TEAM_BLUE;
      else         result := TEAM_NONE;
    end;
  end;
begin
  cmd := LowerCase(P[0]);
  case cmd of
    'p1_name':
      begin
        if (Length(P) > 1) then
        begin
          gPlayer1Settings.Name := b_Text_Unformat(P[1]);
          if g_Game_IsClient then
            MC_SEND_PlayerSettings
          else if gGameOn and (gPlayer1 <> nil) then
          begin
            gPlayer1.Name := b_Text_Unformat(P[1]);
            if g_Game_IsNet then MH_SEND_PlayerSettings(gPlayer1.UID);
          end;
        end;
      end;
    'p2_name':
      begin
        if (Length(P) > 1) then
        begin
          gPlayer2Settings.Name := b_Text_Unformat(P[1]);
          if g_Game_IsClient then
            MC_SEND_PlayerSettings
          else if gGameOn and (gPlayer2 <> nil) then
          begin
            gPlayer2.Name := b_Text_Unformat(P[1]);
            if g_Game_IsNet then MH_SEND_PlayerSettings(gPlayer2.UID);
          end;
        end;
      end;
    'p1_color':
      begin
        if Length(P) > 3 then
        begin
          gPlayer1Settings.Color := _RGB(EnsureRange(StrToIntDef(P[1], 0), 0, 255),
                                         EnsureRange(StrToIntDef(P[2], 0), 0, 255),
                                         EnsureRange(StrToIntDef(P[3], 0), 0, 255));
          if g_Game_IsClient then
            MC_SEND_PlayerSettings
          else if gGameOn and (gPlayer1 <> nil) then
          begin
            gPlayer1.SetColor(gPlayer1Settings.Color);
            if g_Game_IsNet then MH_SEND_PlayerSettings(gPlayer1.UID);
          end;
        end;
      end;
    'p2_color':
      begin
        if Length(P) > 3 then
        begin
          gPlayer2Settings.Color := _RGB(EnsureRange(StrToIntDef(P[1], 0), 0, 255),
                                         EnsureRange(StrToIntDef(P[2], 0), 0, 255),
                                         EnsureRange(StrToIntDef(P[3], 0), 0, 255));
          if g_Game_IsClient then
            MC_SEND_PlayerSettings
          else if gGameOn and (gPlayer2 <> nil) then
          begin
            gPlayer2.SetColor(gPlayer2Settings.Color);
            if g_Game_IsNet then MH_SEND_PlayerSettings(gPlayer2.UID);
          end;
        end;
      end;
    'p1_model':
      begin
        if (Length(P) > 1) then
        begin
          gPlayer1Settings.Model := P[1];
          if g_Game_IsClient then
            MC_SEND_PlayerSettings
          else if gGameOn and (gPlayer1 <> nil) then
          begin
            gPlayer1.FActualModelName := gPlayer1Settings.Model;
            gPlayer1.SetModel(gPlayer1Settings.Model);
            if g_Game_IsNet then MH_SEND_PlayerSettings(gPlayer1.UID);
          end;
        end;
      end;
    'p2_model':
      begin
        if (Length(P) > 1) then
        begin
          gPlayer2Settings.Model := P[1];
          if g_Game_IsClient then
            MC_SEND_PlayerSettings
          else if gGameOn and (gPlayer2 <> nil) then
          begin
            gPlayer2.FActualModelName := gPlayer2Settings.Model;
            gPlayer2.SetModel(gPlayer2Settings.Model);
            if g_Game_IsNet then MH_SEND_PlayerSettings(gPlayer2.UID);
          end;
        end;
      end;
    'p1_team':
      begin
        // TODO: switch teams if in game or store this separately
        if (Length(P) > 1) then
        begin
          team := ParseTeam(P[1]);
          if team = TEAM_NONE then
            g_Console_Add('expected ''red'', ''blue'', 1 or 2')
          else if not gGameOn and not g_Game_IsNet then
            gPlayer1Settings.Team := team
          else
            g_Console_Add(_lc[I_MSG_ONMAPCHANGE]);
        end;
      end;
    'p2_team':
      begin
        // TODO: switch teams if in game or store this separately
        if (Length(P) > 1) then
        begin
          team := ParseTeam(P[1]);
          if team = TEAM_NONE then
            g_Console_Add('expected ''red'', ''blue'', 1 or 2')
          else if not gGameOn and not g_Game_IsNet then
            gPlayer2Settings.Team := team
          else
            g_Console_Add(_lc[I_MSG_ONMAPCHANGE]);
        end;
      end;
    'p1_autoswitch':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.WeaponSwitch := EnsureRange(StrTointDef(P[1], 0), 0, 2);
        end;
    'p2_autoswitch':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.WeaponSwitch := EnsureRange(StrTointDef(P[1], 0), 0, 2);
        end;
    'p1_switch_empty':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.SwitchToEmpty := EnsureRange(StrTointDef(P[1], 0), 0, 1);
        end;
    'p2_switch_empty':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.SwitchToEmpty := EnsureRange(StrTointDef(P[1], 0), 0, 1);
        end;
    'p1_skip_fist':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.SkipFist := EnsureRange(StrTointDef(P[1], 0), 0, 1);
        end;
    'p2_skip_fist':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.SkipFist := EnsureRange(StrTointDef(P[1], 0), 0, 1);
        end;
    'p1_priority_kastet':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.WeaponPreferences[WEAPON_KASTET] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1); 
        end;
    'p2_priority_kastet':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.WeaponPreferences[WEAPON_KASTET] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1); 
      end;        
    'p1_priority_saw':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.WeaponPreferences[WEAPON_SAW] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
        end;
    'p2_priority_saw':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.WeaponPreferences[WEAPON_SAW] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p1_priority_pistol':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.WeaponPreferences[WEAPON_KASTET] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1); 
        end;
    'p2_priority_pistol':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.WeaponPreferences[WEAPON_KASTET] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1); 
      end;         
    'p1_priority_shotgun1':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.WeaponPreferences[WEAPON_SHOTGUN1] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p2_priority_shotgun1':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.WeaponPreferences[WEAPON_SHOTGUN1] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p1_priority_shotgun2':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.WeaponPreferences[WEAPON_SHOTGUN2] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p2_priority_shotgun2':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.WeaponPreferences[WEAPON_SHOTGUN2] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p1_priority_chaingun':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.WeaponPreferences[WEAPON_CHAINGUN] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p2_priority_chaingun':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.WeaponPreferences[WEAPON_CHAINGUN] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p1_priority_rocketlauncher':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.WeaponPreferences[WEAPON_ROCKETLAUNCHER] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p2_priority_rocketlauncher':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.WeaponPreferences[WEAPON_ROCKETLAUNCHER] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p1_priority_plasma':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.WeaponPreferences[WEAPON_PLASMA] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p2_priority_plasma':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.WeaponPreferences[WEAPON_PLASMA] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p1_priority_bfg':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.WeaponPreferences[WEAPON_BFG] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p2_priority_bfg':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.WeaponPreferences[WEAPON_BFG] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p1_priority_super':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.WeaponPreferences[WEAPON_SUPERPULEMET] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p2_priority_super':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.WeaponPreferences[WEAPON_SUPERPULEMET] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p1_priority_flamethrower':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.WeaponPreferences[WEAPON_FLAMETHROWER] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;
    'p2_priority_flamethrower':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.WeaponPreferences[WEAPON_FLAMETHROWER] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1);
      end;      
    'p1_priority_berserk':
      begin
        if (Length(P) = 2) then
          gPlayer1Settings.WeaponPreferences[WP_LAST+1] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1); 
        end;
    'p2_priority_berserk':
      begin
        if (Length(P) = 2) then
          gPlayer2Settings.WeaponPreferences[WP_LAST+1] := EnsureRange(StrToIntDef(P[1], WP_FIRST), WP_FIRST, WP_LAST+1); 
      end;                                                                                  
  end;
end;

procedure PrintHeapStats();
var
  hs: TFPCHeapStatus;
begin
  hs := GetFPCHeapStatus();
  e_LogWriteLn ('v===== heap status =====v');
  e_LogWriteFln('max heap size = %d k', [hs.MaxHeapSize div 1024]);
  e_LogWriteFln('max heap used = %d k', [hs.MaxHeapUsed div 1024]);
  e_LogWriteFln('cur heap size = %d k', [hs.CurrHeapSize div 1024]);
  e_LogWriteFln('cur heap used = %d k', [hs.CurrHeapUsed div 1024]);
  e_LogWriteFln('cur heap free = %d k', [hs.CurrHeapFree div 1024]);
  e_LogWriteLn ('^=======================^');
end;

procedure DebugCommands(P: SSArray);
var
  a, b: Integer;
  cmd: string;
  //pt: TDFPoint;
  mon: TMonster;
begin
// Команды отладочного режима:
  if {gDebugMode}conIsCheatsEnabled then
  begin
    cmd := LowerCase(P[0]);
    if cmd = 'd_window' then
    begin
      g_Console_Add(Format('gScreenWidth = %d, gScreenHeight = %d', [gScreenWidth, gScreenHeight]));
      g_Console_Add(Format('gScreenWidth = %d, gScreenHeight = %d', [gScreenWidth, gScreenHeight]));
    end
    else if cmd = 'd_sounds' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        g_Debug_Sounds := (P[1][1] = '1');

      g_Console_Add(Format('d_sounds is %d', [Byte(g_Debug_Sounds)]));
    end
    else if cmd = 'd_frames' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        g_Debug_Frames := (P[1][1] = '1');

      g_Console_Add(Format('d_frames is %d', [Byte(g_Debug_Frames)]));
    end
    else if cmd = 'd_winmsg' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        g_Debug_WinMsgs := (P[1][1] = '1');

      g_Console_Add(Format('d_winmsg is %d', [Byte(g_Debug_WinMsgs)]));
    end
    else if (cmd = 'd_monoff') and not g_Game_IsNet then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        g_Debug_MonsterOff := (P[1][1] = '1');

      g_Console_Add(Format('d_monoff is %d', [Byte(g_debug_MonsterOff)]));
    end
    else if (cmd = 'd_botoff') and not g_Game_IsNet then
    begin
      if Length(P) > 1 then
        case P[1][1] of
          '0': g_debug_BotAIOff := 0;
          '1': g_debug_BotAIOff := 1;
          '2': g_debug_BotAIOff := 2;
          '3': g_debug_BotAIOff := 3;
        end;

      g_Console_Add(Format('d_botoff is %d', [g_debug_BotAIOff]));
    end
    else if cmd = 'd_monster' then
    begin
      if gGameOn and (gPlayer1 <> nil) and (gPlayer1.alive) and (not g_Game_IsNet) then
        if Length(P) < 2 then
        begin
          g_Console_Add(cmd + ' [ID | Name] [behaviour]');
          g_Console_Add('ID | Name');
          for b := MONSTER_DEMON to MONSTER_MAN do
            g_Console_Add(Format('%2d | %s', [b, g_Mons_NameByTypeId(b)]));
          conwriteln('behav.   num'#10'normal    0'#10'killer    1'#10'maniac    2'#10'insane    3'#10'cannibal  4'#10'good      5');
        end else
        begin
          a := StrToIntDef(P[1], 0);
          if (a < MONSTER_DEMON) or (a > MONSTER_MAN) then
            a := g_Mons_TypeIdByName(P[1]);

          if (a < MONSTER_DEMON) or (a > MONSTER_MAN) then
            g_Console_Add(Format(_lc[I_MSG_NO_MONSTER], [P[1]]))
          else
            begin
              with gPlayer1.Obj do
              begin
                mon := g_Monsters_Create(a,
                     X + Rect.X + (Rect.Width div 2),
                     Y + Rect.Y + Rect.Height,
                     gPlayer1.Direction, True);
              end;
              if (Length(P) > 2) and (mon <> nil) then
              begin
                     if (CompareText(P[2], 'normal') = 0) then mon.MonsterBehaviour := BH_NORMAL
                else if (CompareText(P[2], 'killer') = 0) then mon.MonsterBehaviour := BH_KILLER
                else if (CompareText(P[2], 'maniac') = 0) then mon.MonsterBehaviour := BH_MANIAC
                else if (CompareText(P[2], 'insane') = 0) then mon.MonsterBehaviour := BH_INSANE
                else if (CompareText(P[2], 'cannibal') = 0) then mon.MonsterBehaviour := BH_CANNIBAL
                else if (CompareText(P[2], 'good') = 0) then mon.MonsterBehaviour := BH_GOOD
                else if (CompareText(P[2], 'friend') = 0) then mon.MonsterBehaviour := BH_GOOD
                else if (CompareText(P[2], 'friendly') = 0) then mon.MonsterBehaviour := BH_GOOD
                else mon.MonsterBehaviour := Min(Max(StrToIntDef(P[2], BH_NORMAL), BH_NORMAL), BH_GOOD);
              end;
            end;
        end;
    end
    else if (cmd = 'd_health') then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        g_debug_HealthBar := (P[1][1] = '1');

      g_Console_Add(Format('d_health is %d', [Byte(g_debug_HealthBar)]));
    end
    else if (cmd = 'd_player') then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        g_debug_Player := (P[1][1] = '1');

      g_Console_Add(Format(cmd + ' is %d', [Byte(g_Debug_Player)]));
    end
    else if (cmd = 'd_mem') then
    begin
      PrintHeapStats();
    end;
  end
    else
      g_Console_Add(_lc[I_MSG_NOT_DEBUG]);
end;


procedure GameCheats(P: SSArray);
var
  cmd: string;
  f, a: Integer;
  plr: TPlayer;
begin
  if (not gGameOn) or (not conIsCheatsEnabled) then
  begin
    g_Console_Add('not available');
    exit;
  end;
  plr := gPlayer1;
  if plr = nil then
  begin
    g_Console_Add('where is the player?!');
    exit;
  end;
  cmd := LowerCase(P[0]);
  // god
  if cmd = 'god' then
  begin
    plr.GodMode := not plr.GodMode;
    if plr.GodMode then g_Console_Add('player is godlike now') else g_Console_Add('player is mortal now');
    exit;
  end;
  // give <health|exit|weapons|air|suit|jetpack|berserk|all>
  if cmd = 'give' then
  begin
    if length(P) < 2 then begin g_Console_Add('give what?!'); exit; end;
    for f := 1 to High(P) do
    begin
      cmd := LowerCase(P[f]);
      if cmd = 'health' then begin plr.RestoreHealthArmor(); g_Console_Add('player feels himself better'); continue; end;
      if (cmd = 'all') {or (cmd = 'weapons')} then begin plr.AllRulez(False); g_Console_Add('player got the gifts'); continue; end;
      if cmd = 'exit' then
      begin
        if gTriggers <> nil then
        begin
          for a := 0 to High(gTriggers) do
          begin
            if gTriggers[a].TriggerType = TRIGGER_EXIT then
            begin
              g_Console_Add('player left the map');
              gExitByTrigger := True;
              //g_Game_ExitLevel(gTriggers[a].Data.MapName);
              g_Game_ExitLevel(gTriggers[a].tgcMap);
              break;
            end;
          end;
        end;
        continue;
      end;

      if cmd = 'air' then begin plr.GiveItem(ITEM_OXYGEN); g_Console_Add('player got some air'); continue; end;
      if cmd = 'jetpack' then begin plr.GiveItem(ITEM_JETPACK); g_Console_Add('player got a jetpack'); continue; end;
      if cmd = 'suit' then begin plr.GiveItem(ITEM_SUIT); g_Console_Add('player got an envirosuit'); continue; end;
      if cmd = 'berserk' then begin plr.GiveItem(ITEM_MEDKIT_BLACK); g_Console_Add('player got a berserk pack'); continue; end;
      if cmd = 'backpack' then begin plr.GiveItem(ITEM_AMMO_BACKPACK); g_Console_Add('player got a backpack'); continue; end;

      if cmd = 'helmet' then begin plr.GiveItem(ITEM_HELMET); g_Console_Add('player got a helmet'); continue; end;
      if cmd = 'bottle' then begin plr.GiveItem(ITEM_BOTTLE); g_Console_Add('player got a bottle of health'); continue; end;

      if cmd = 'stimpack' then begin plr.GiveItem(ITEM_MEDKIT_SMALL); g_Console_Add('player got a stimpack'); continue; end;
      if (cmd = 'medkit') or (cmd = 'medikit') or (cmd = 'medpack') or (cmd = 'medipack') then begin plr.GiveItem(ITEM_MEDKIT_LARGE); g_Console_Add('player got a '+cmd); continue; end;

      if cmd = 'greenarmor' then begin plr.GiveItem(ITEM_ARMOR_GREEN); g_Console_Add('player got a security armor'); continue; end;
      if cmd = 'bluearmor' then begin plr.GiveItem(ITEM_ARMOR_BLUE); g_Console_Add('player got a combat armor'); continue; end;

      if (cmd = 'megasphere') or (cmd = 'mega') then begin plr.GiveItem(ITEM_SPHERE_BLUE); g_Console_Add('player got a megasphere'); continue; end;
      if (cmd = 'soulsphere') or (cmd = 'soul')then begin plr.GiveItem(ITEM_SPHERE_WHITE); g_Console_Add('player got a soul sphere'); continue; end;

      if (cmd = 'invul') or (cmd = 'invulnerability') then begin plr.GiveItem(ITEM_INVUL); g_Console_Add('player got invulnerability'); continue; end;
      if (cmd = 'invis') or (cmd = 'invisibility') then begin plr.GiveItem(ITEM_INVIS); g_Console_Add('player got invisibility'); continue; end;

      if cmd = 'redkey' then begin plr.GiveItem(ITEM_KEY_RED); g_Console_Add('player got the red key'); continue; end;
      if cmd = 'greenkey' then begin plr.GiveItem(ITEM_KEY_GREEN); g_Console_Add('player got the green key'); continue; end;
      if cmd = 'bluekey' then begin plr.GiveItem(ITEM_KEY_BLUE); g_Console_Add('player got the blue key'); continue; end;

      if (cmd = 'shotgun') or (cmd = 'sg') then begin plr.GiveItem(ITEM_WEAPON_SHOTGUN1); g_Console_Add('player got a shotgun'); continue; end;
      if (cmd = 'supershotgun') or (cmd = 'ssg') then begin plr.GiveItem(ITEM_WEAPON_SHOTGUN2); g_Console_Add('player got a supershotgun'); continue; end;
      if cmd = 'chaingun' then begin plr.GiveItem(ITEM_WEAPON_CHAINGUN); g_Console_Add('player got a chaingun'); continue; end;
      if (cmd = 'launcher') or (cmd = 'rocketlauncher') or (cmd = 'rl') then begin plr.GiveItem(ITEM_WEAPON_ROCKETLAUNCHER); g_Console_Add('player got a rocket launcher'); continue; end;
      if cmd = 'plasmagun' then begin plr.GiveItem(ITEM_WEAPON_PLASMA); g_Console_Add('player got a plasma gun'); continue; end;
      if cmd = 'bfg' then begin plr.GiveItem(ITEM_WEAPON_BFG); g_Console_Add('player got a BFG-9000'); continue; end;

      if (cmd = 'shotgunzz') or (cmd = 'sgzz') then begin plr.GiveItem(ITEM_WEAPON_SHOTGUN1); plr.GiveItem(ITEM_AMMO_SHELLS_BOX); g_Console_Add('player got a shotgun'); continue; end;
      if (cmd = 'supershotgunzz') or (cmd = 'ssgzz') then begin plr.GiveItem(ITEM_WEAPON_SHOTGUN2); plr.GiveItem(ITEM_AMMO_SHELLS_BOX); g_Console_Add('player got a supershotgun'); continue; end;
      if cmd = 'chaingunzz' then begin plr.GiveItem(ITEM_WEAPON_CHAINGUN); plr.GiveItem(ITEM_AMMO_BULLETS_BOX); g_Console_Add('player got a chaingun'); continue; end;
      if (cmd = 'launcherzz') or (cmd = 'rocketlauncherzz') or (cmd = 'rlzz') then begin plr.GiveItem(ITEM_WEAPON_ROCKETLAUNCHER); plr.GiveItem(ITEM_AMMO_ROCKET_BOX); g_Console_Add('player got a rocket launcher'); continue; end;
      if cmd = 'plasmagunzz' then begin plr.GiveItem(ITEM_WEAPON_PLASMA); plr.GiveItem(ITEM_AMMO_CELL_BIG); g_Console_Add('player got a plasma gun'); continue; end;
      if cmd = 'bfgzz' then begin plr.GiveItem(ITEM_WEAPON_BFG); plr.GiveItem(ITEM_AMMO_CELL_BIG); g_Console_Add('player got a BFG-9000'); continue; end;

      if cmd = 'superchaingun' then begin plr.GiveItem(ITEM_WEAPON_SUPERPULEMET); g_Console_Add('player got a superchaingun'); continue; end;
      if cmd = 'superchaingunzz' then begin plr.GiveItem(ITEM_WEAPON_SUPERPULEMET); plr.GiveItem(ITEM_AMMO_BULLETS_BOX); g_Console_Add('player got a superchaingun'); continue; end;

      if (cmd = 'flamer') or (cmd = 'flamethrower') or (cmd = 'ft') then begin plr.GiveItem(ITEM_WEAPON_FLAMETHROWER); g_Console_Add('player got a flame thrower'); continue; end;
      if (cmd = 'flamerzz') or (cmd = 'flamethrowerzz') or (cmd = 'ftzz') then begin plr.GiveItem(ITEM_WEAPON_FLAMETHROWER); plr.GiveItem(ITEM_AMMO_FUELCAN); g_Console_Add('player got a flame thrower'); continue; end;

      if cmd = 'chainsaw' then begin plr.GiveItem(ITEM_WEAPON_SAW); g_Console_Add('player got a chainsaw'); continue; end;

      if cmd = 'ammo' then
      begin
        plr.GiveItem(ITEM_AMMO_SHELLS_BOX);
        plr.GiveItem(ITEM_AMMO_BULLETS_BOX);
        plr.GiveItem(ITEM_AMMO_CELL_BIG);
        plr.GiveItem(ITEM_AMMO_ROCKET_BOX);
        plr.GiveItem(ITEM_AMMO_FUELCAN);
        g_Console_Add('player got some ammo');
        continue;
      end;

      if cmd = 'clip' then begin plr.GiveItem(ITEM_AMMO_BULLETS); g_Console_Add('player got some bullets'); continue; end;
      if cmd = 'bullets' then begin plr.GiveItem(ITEM_AMMO_BULLETS_BOX); g_Console_Add('player got a box of bullets'); continue; end;

      if cmd = 'shells' then begin plr.GiveItem(ITEM_AMMO_SHELLS); g_Console_Add('player got some shells'); continue; end;
      if cmd = 'shellbox' then begin plr.GiveItem(ITEM_AMMO_SHELLS_BOX); g_Console_Add('player got a box of shells'); continue; end;

      if cmd = 'cells' then begin plr.GiveItem(ITEM_AMMO_CELL); g_Console_Add('player got some cells'); continue; end;
      if cmd = 'battery' then begin plr.GiveItem(ITEM_AMMO_CELL_BIG); g_Console_Add('player got cell battery'); continue; end;

      if cmd = 'rocket' then begin plr.GiveItem(ITEM_AMMO_ROCKET); g_Console_Add('player got a rocket'); continue; end;
      if cmd = 'rocketbox' then begin plr.GiveItem(ITEM_AMMO_ROCKET_BOX); g_Console_Add('player got some rockets'); continue; end;

      if (cmd = 'fuel') or (cmd = 'fuelcan') then begin plr.GiveItem(ITEM_AMMO_FUELCAN); g_Console_Add('player got fuel canister'); continue; end;

      if cmd = 'weapons' then
      begin
        plr.GiveItem(ITEM_WEAPON_SHOTGUN1);
        plr.GiveItem(ITEM_WEAPON_SHOTGUN2);
        plr.GiveItem(ITEM_WEAPON_CHAINGUN);
        plr.GiveItem(ITEM_WEAPON_ROCKETLAUNCHER);
        plr.GiveItem(ITEM_WEAPON_PLASMA);
        plr.GiveItem(ITEM_WEAPON_BFG);
        g_Console_Add('player got weapons');
        continue;
      end;

      if cmd = 'keys' then
      begin
        plr.GiveItem(ITEM_KEY_RED);
        plr.GiveItem(ITEM_KEY_GREEN);
        plr.GiveItem(ITEM_KEY_BLUE);
        g_Console_Add('player got all keys');
        continue;
      end;

      g_Console_Add('i don''t know how to give '''+cmd+'''!');
    end;
    exit;
  end;
  // open
  if cmd = 'open' then
  begin
    g_Console_Add('player activated sesame');
    g_Triggers_OpenAll();
    exit;
  end;
  // fly
  if cmd = 'fly' then
  begin
    gFly := not gFly;
    if gFly then g_Console_Add('player feels himself lighter') else g_Console_Add('player lost his wings');
    exit;
  end;
  // noclip
  if cmd = 'noclip' then
  begin
    plr.SwitchNoClip;
    g_Console_Add('wall hardeness adjusted');
    exit;
  end;
  // notarget
  if cmd = 'notarget' then
  begin
    plr.NoTarget := not plr.NoTarget;
    if plr.NoTarget then g_Console_Add('player hides in shadows') else g_Console_Add('player is brave again');
    exit;
  end;
  // noreload
  if cmd = 'noreload' then
  begin
    plr.NoReload := not plr.NoReload;
    if plr.NoReload then g_Console_Add('player is action hero now') else g_Console_Add('player is ordinary man now');
    exit;
  end;
  // speedy
  if cmd = 'speedy' then
  begin
    MAX_RUNVEL := 32-MAX_RUNVEL;
    g_Console_Add('speed adjusted');
    exit;
  end;
  // jumpy
  if cmd = 'jumpy' then
  begin
    VEL_JUMP := 30-VEL_JUMP;
    g_Console_Add('jump height adjusted');
    exit;
  end;
  // automap
  if cmd = 'automap' then
  begin
    gShowMap := not gShowMap;
    if gShowMap then g_Console_Add('player gains second sight') else g_Console_Add('player lost second sight');
    exit;
  end;
  // aimline
  if cmd = 'aimline' then
  begin
    gAimLine := not gAimLine;
    if gAimLine then g_Console_Add('player gains laser sight') else g_Console_Add('player lost laser sight');
    exit;
  end;
end;

procedure GameCommands(P: SSArray);
var
  a, b: Integer;
  s, pw: String;
  chstr: string;
  cmd: string;
  pl: pTNetClient = nil;
  plr: TPlayer;
  prt: Word;
  nm: Boolean;
  listen: LongWord;
  found: Boolean;
begin
// Общие команды:
  cmd := LowerCase(P[0]);
  chstr := '';
  if cmd = 'pause' then
  begin
    if (g_ActiveWindow = nil) then
      g_Game_Pause(not gPauseMain);
  end
  else if cmd = 'endgame' then
    gExit := EXIT_SIMPLE
  else if cmd = 'restart' then
  begin
    if gGameOn or (gState in [STATE_INTERSINGLE, STATE_INTERCUSTOM]) then
    begin
      if g_Game_IsClient then
      begin
        g_Console_Add(_lc[I_MSG_SERVERONLY]);
        Exit;
      end;
      g_Game_Restart();
    end else
      g_Console_Add(_lc[I_MSG_NOT_GAME]);
  end
  else if cmd = 'kick' then
  begin
    if g_Game_IsServer then
    begin
      if Length(P) < 2 then
      begin
        g_Console_Add('kick <name>');
        Exit;
      end;
      if P[1] = '' then
      begin
        g_Console_Add('kick <name>');
        Exit;
      end;

      if g_Game_IsNet then
        pl := g_Net_Client_ByName(P[1]);
      if (pl <> nil) then
      begin
        s := g_Net_ClientName_ByID(pl^.ID);
        g_Net_Host_Kick(pl^.ID, NET_DISC_KICK);
        g_Console_Add(Format(_lc[I_PLAYER_KICK], [s]));
        MH_SEND_GameEvent(NET_EV_PLAYER_KICK, 0, s);
        g_Net_Slist_ServerPlayerLeaves();
      end else if gPlayers <> nil then
        for a := Low(gPlayers) to High(gPlayers) do
          if gPlayers[a] <> nil then
            if Copy(LowerCase(gPlayers[a].Name), 1, Length(P[1])) = LowerCase(P[1]) then
            begin
              // Не отключать основных игроков в сингле
              if not(gPlayers[a] is TBot) and (gGameSettings.GameType = GT_SINGLE) then
                continue;
              gPlayers[a].Lives := 0;
              gPlayers[a].Kill(K_SIMPLEKILL, 0, HIT_DISCON);
              g_Console_Add(Format(_lc[I_PLAYER_LEAVE], [gPlayers[a].Name]), True);
              g_Player_Remove(gPlayers[a].UID);
              g_Net_Slist_ServerPlayerLeaves();
              // Если не перемешать, при добавлении новых ботов появятся старые
              g_Bot_MixNames();
            end;
    end else
      g_Console_Add(_lc[I_MSG_GM_UNAVAIL]);
  end
  else if cmd = 'kick_id' then
  begin
    if g_Game_IsServer and g_Game_IsNet then
    begin
      if Length(P) < 2 then
      begin
        g_Console_Add('kick_id <client ID>');
        Exit;
      end;
      if P[1] = '' then
      begin
        g_Console_Add('kick_id <client ID>');
        Exit;
      end;

      a := StrToIntDef(P[1], 0);
      if (NetClients <> nil) and (a <= High(NetClients)) then
      begin
        if NetClients[a].Used and (NetClients[a].Peer <> nil) then
        begin
          s := g_Net_ClientName_ByID(NetClients[a].ID);
          g_Net_Host_Kick(NetClients[a].ID, NET_DISC_KICK);
          g_Console_Add(Format(_lc[I_PLAYER_KICK], [s]));
          MH_SEND_GameEvent(NET_EV_PLAYER_KICK, 0, s);
          g_Net_Slist_ServerPlayerLeaves();
        end;
      end;
    end else
      g_Console_Add(_lc[I_MSG_SERVERONLY]);
  end
  else if cmd = 'kick_pid' then
  begin
    if g_Game_IsServer and g_Game_IsNet then
    begin
      if Length(P) < 2 then
      begin
        g_Console_Add('kick_pid <player ID>');
        Exit;
      end;
      if P[1] = '' then
      begin
        g_Console_Add('kick_pid <player ID>');
        Exit;
      end;

      a := StrToIntDef(P[1], 0);
      pl := g_Net_Client_ByPlayer(a);
      if (pl <> nil) and pl^.Used and (pl^.Peer <> nil) then
      begin
        s := g_Net_ClientName_ByID(pl^.ID);
        g_Net_Host_Kick(pl^.ID, NET_DISC_KICK);
        g_Console_Add(Format(_lc[I_PLAYER_KICK], [s]));
        MH_SEND_GameEvent(NET_EV_PLAYER_KICK, 0, s);
        g_Net_Slist_ServerPlayerLeaves();
      end;
    end else
      g_Console_Add(_lc[I_MSG_SERVERONLY]);
  end
  else if cmd = 'ban' then
  begin
    if g_Game_IsServer and g_Game_IsNet then
    begin
      if Length(P) < 2 then
      begin
        g_Console_Add('ban <name>');
        Exit;
      end;
      if P[1] = '' then
      begin
        g_Console_Add('ban <name>');
        Exit;
      end;

      pl := g_Net_Client_ByName(P[1]);
      if (pl <> nil) then
      begin
        s := g_Net_ClientName_ByID(pl^.ID);
        g_Net_BanHost(pl^.Peer^.address.host, False);
        g_Net_Host_Kick(pl^.ID, NET_DISC_TEMPBAN);
        g_Console_Add(Format(_lc[I_PLAYER_BAN], [s]));
        MH_SEND_GameEvent(NET_EV_PLAYER_BAN, 0, s);
        g_Net_Slist_ServerPlayerLeaves();
      end else
        g_Console_Add(Format(_lc[I_NET_ERR_NAME404], [P[1]]));
    end else
      g_Console_Add(_lc[I_MSG_SERVERONLY]);
  end
  else if cmd = 'ban_id' then
  begin
    if g_Game_IsServer and g_Game_IsNet then
    begin
      if Length(P) < 2 then
      begin
        g_Console_Add('ban_id <client ID>');
        Exit;
      end;
      if P[1] = '' then
      begin
        g_Console_Add('ban_id <client ID>');
        Exit;
      end;

      a := StrToIntDef(P[1], 0);
      if (NetClients <> nil) and (a <= High(NetClients)) then
        if NetClients[a].Used and (NetClients[a].Peer <> nil) then
        begin
          s := g_Net_ClientName_ByID(NetClients[a].ID);
          g_Net_BanHost(NetClients[a].Peer^.address.host, False);
          g_Net_Host_Kick(NetClients[a].ID, NET_DISC_TEMPBAN);
          g_Console_Add(Format(_lc[I_PLAYER_BAN], [s]));
          MH_SEND_GameEvent(NET_EV_PLAYER_BAN, 0, s);
          g_Net_Slist_ServerPlayerLeaves();
        end;
    end else
      g_Console_Add(_lc[I_MSG_SERVERONLY]);
  end
  else if cmd = 'ban_pid' then
  begin
    if g_Game_IsServer and g_Game_IsNet then
    begin
      if Length(P) < 2 then
      begin
        g_Console_Add('ban_pid <player ID>');
        Exit;
      end;
      if P[1] = '' then
      begin
        g_Console_Add('ban_pid <player ID>');
        Exit;
      end;

      a := StrToIntDef(P[1], 0);
      pl := g_Net_Client_ByPlayer(a);
      if (pl <> nil) and pl^.Used and (pl^.Peer <> nil) then
      begin
        s := g_Net_ClientName_ByID(pl^.ID);
        g_Net_BanHost(pl^.Peer^.address.host, False);
        g_Net_Host_Kick(pl^.ID, NET_DISC_TEMPBAN);
        g_Console_Add(Format(_lc[I_PLAYER_BAN], [s]));
        MH_SEND_GameEvent(NET_EV_PLAYER_BAN, 0, s);
        g_Net_Slist_ServerPlayerLeaves();
      end;
    end else
      g_Console_Add(_lc[I_MSG_SERVERONLY]);
  end
  else if cmd = 'permban' then
  begin
    if g_Game_IsServer and g_Game_IsNet then
    begin
      if Length(P) < 2 then
      begin
        g_Console_Add('permban <name>');
        Exit;
      end;
      if P[1] = '' then
      begin
        g_Console_Add('permban <name>');
        Exit;
      end;

      pl := g_Net_Client_ByName(P[1]);
      if (pl <> nil) then
      begin
        s := g_Net_ClientName_ByID(pl^.ID);
        g_Net_BanHost(pl^.Peer^.address.host);
        g_Net_Host_Kick(pl^.ID, NET_DISC_BAN);
        g_Net_SaveBanList();
        g_Console_Add(Format(_lc[I_PLAYER_BAN], [s]));
        MH_SEND_GameEvent(NET_EV_PLAYER_BAN, 0, s);
        g_Net_Slist_ServerPlayerLeaves();
      end else
        g_Console_Add(Format(_lc[I_NET_ERR_NAME404], [P[1]]));
    end else
      g_Console_Add(_lc[I_MSG_SERVERONLY]);
  end
  else if cmd = 'permban_id' then
  begin
    if g_Game_IsServer and g_Game_IsNet then
    begin
      if Length(P) < 2 then
      begin
        g_Console_Add('permban_id <client ID>');
        Exit;
      end;
      if P[1] = '' then
      begin
        g_Console_Add('permban_id <client ID>');
        Exit;
      end;

      a := StrToIntDef(P[1], 0);
      if (NetClients <> nil) and (a <= High(NetClients)) then
        if NetClients[a].Used and (NetClients[a].Peer <> nil) then
        begin
          s := g_Net_ClientName_ByID(NetClients[a].ID);
          g_Net_BanHost(NetClients[a].Peer^.address.host);
          g_Net_Host_Kick(NetClients[a].ID, NET_DISC_BAN);
          g_Net_SaveBanList();
          g_Console_Add(Format(_lc[I_PLAYER_BAN], [s]));
          MH_SEND_GameEvent(NET_EV_PLAYER_BAN, 0, s);
          g_Net_Slist_ServerPlayerLeaves();
        end;
    end else
      g_Console_Add(_lc[I_MSG_SERVERONLY]);
  end
  else if cmd = 'permban_pid' then
  begin
    if g_Game_IsServer and g_Game_IsNet then
    begin
      if Length(P) < 2 then
      begin
        g_Console_Add('permban_pid <player ID>');
        Exit;
      end;
      if P[1] = '' then
      begin
        g_Console_Add('permban_pid <player ID>');
        Exit;
      end;

      a := StrToIntDef(P[1], 0);
      pl := g_Net_Client_ByPlayer(a);
      if (pl <> nil) and pl^.Used and (pl^.Peer <> nil) then
      begin
        s := g_Net_ClientName_ByID(pl^.ID);
        g_Net_BanHost(pl^.Peer^.address.host);
        g_Net_Host_Kick(pl^.ID, NET_DISC_TEMPBAN);
        g_Net_SaveBanList();
        g_Console_Add(Format(_lc[I_PLAYER_BAN], [s]));
        MH_SEND_GameEvent(NET_EV_PLAYER_BAN, 0, s);
        g_Net_Slist_ServerPlayerLeaves();
      end;
    end else
      g_Console_Add(_lc[I_MSG_SERVERONLY]);
  end
  else if cmd = 'permban_ip' then
  begin
    if g_Game_IsServer and g_Game_IsNet then
    begin
      if Length(P) < 2 then
      begin
        g_Console_Add('permban_ip <IP address>');
        Exit;
      end;
      if P[1] = '' then
      begin
        g_Console_Add('permban_ip <IP address>');
        Exit;
      end;

      g_Net_BanHost(P[1]);
      g_Net_SaveBanList();
      g_Console_Add(Format(_lc[I_PLAYER_BAN], [P[1]]));
    end else
      g_Console_Add(_lc[I_MSG_SERVERONLY]);
  end
  else if cmd = 'unban' then
  begin
    if g_Game_IsServer and g_Game_IsNet then
    begin
      if Length(P) < 2 then
      begin
        g_Console_Add('unban <IP Address>');
        Exit;
      end;
      if P[1] = '' then
      begin
        g_Console_Add('unban <IP Address>');
        Exit;
      end;

      if g_Net_UnbanHost(P[1]) then
      begin
        g_Console_Add(Format(_lc[I_MSG_UNBAN_OK], [P[1]]));
        g_Net_SaveBanList();
      end else
        g_Console_Add(Format(_lc[I_MSG_UNBAN_FAIL], [P[1]]));
    end else
      g_Console_Add(_lc[I_MSG_SERVERONLY]);
  end
  else if cmd = 'clientlist' then
  begin
    if g_Game_IsServer and g_Game_IsNet then
    begin
      b := 0;
      if NetClients <> nil then
        for a := Low(NetClients) to High(NetClients) do
          if NetClients[a].Used and (NetClients[a].Peer <> nil) then
          begin
            plr := g_Player_Get(NetClients[a].Player);
            if plr = nil then continue;
            Inc(b);
            g_Console_Add(Format('#%2d: %-15s | %s', [a,
                          IpToStr(NetClients[a].Peer^.address.host), plr.Name]));
          end;
      if b = 0 then
        g_Console_Add(_lc[I_MSG_NOCLIENTS]);
    end else
      g_Console_Add(_lc[I_MSG_SERVERONLY]);
  end
  else if cmd = 'connect' then
  begin
    if (NetMode = NET_NONE) then
    begin
      if Length(P) < 2 then
      begin
        g_Console_Add('connect <IP> [port] [password]');
        Exit;
      end;
      if P[1] = '' then
      begin
        g_Console_Add('connect <IP> [port] [password]');
        Exit;
      end;

      if Length(P) > 2 then
        prt := StrToIntDef(P[2], 25666)
      else
        prt := 25666;

      if Length(P) > 3 then
        pw := P[3]
      else
        pw := '';

      g_Game_StartClient(P[1], prt, pw);
    end;
  end
  else if cmd = 'disconnect' then
  begin
    if (NetMode = NET_CLIENT) then
      g_Net_Disconnect();
  end
  else if cmd = 'reconnect' then
  begin
    if (NetMode = NET_SERVER) then
      Exit;

    if (NetMode = NET_CLIENT) then
    begin
      g_Net_Disconnect();
      gExit := EXIT_SIMPLE;
      EndGame;
    end;

    //TODO: Use last successful password to reconnect, instead of ''
    g_Game_StartClient(NetClientIP, NetClientPort, '');
  end
  else if (cmd = 'addbot') or
     (cmd = 'bot_add') then
  begin
    if Length(P) > 2 then
      g_Bot_Add(TEAM_NONE, StrToIntDef(P[1], 2), StrToIntDef(P[2], 100))
    else if Length(P) > 1 then
      g_Bot_Add(TEAM_NONE, StrToIntDef(P[1], 2))
    else
      g_Bot_Add(TEAM_NONE, 2);
  end
  else if cmd = 'bot_addlist' then
  begin
    if Length(P) > 1 then
    begin
      if Length(P) = 2 then
        g_Bot_AddList(TEAM_NONE, P[1], StrToIntDef(P[1], -1))
      else if Length(P) = 3 then
        g_Bot_AddList(TEAM_NONE, P[1], StrToIntDef(P[1], -1), StrToIntDef(P[2], 100))
      else
        g_Bot_AddList(IfThen(P[2] = 'red', TEAM_RED, TEAM_BLUE), P[1], StrToIntDef(P[1], -1));
    end;
  end
  else if cmd = 'bot_removeall' then
    g_Bot_RemoveAll()
  else if cmd = 'chat' then
  begin
    if g_Game_IsNet then
    begin
      if Length(P) > 1 then
      begin
        for a := 1 to High(P) do
          chstr := chstr + P[a] + ' ';

        if Length(chstr) > 200 then SetLength(chstr, 200);

        if Length(chstr) < 1 then
        begin
          g_Console_Add('chat <text>');
          Exit;
        end;

        chstr := b_Text_Format(chstr);
        if g_Game_IsClient then
          MC_SEND_Chat(chstr, NET_CHAT_PLAYER)
        else
          MH_SEND_Chat(gPlayer1Settings.Name + ': ' + chstr, NET_CHAT_PLAYER);
      end
      else
        g_Console_Add('chat <text>');
    end else
      g_Console_Add(_lc[I_MSG_GM_UNAVAIL]);
  end
  else if cmd = 'teamchat' then
  begin
    if g_Game_IsNet and (gGameSettings.GameMode in [GM_TDM, GM_CTF]) then
    begin
      if Length(P) > 1 then
      begin
        for a := 1 to High(P) do
          chstr := chstr + P[a] + ' ';

        if Length(chstr) > 200 then SetLength(chstr, 200);

        if Length(chstr) < 1 then
        begin
          g_Console_Add('teamchat <text>');
          Exit;
        end;

        chstr := b_Text_Format(chstr);
        if g_Game_IsClient then
          MC_SEND_Chat(chstr, NET_CHAT_TEAM)
        else
          MH_SEND_Chat(gPlayer1Settings.Name + ': ' + chstr, NET_CHAT_TEAM,
            gPlayer1Settings.Team);
      end
      else
        g_Console_Add('teamchat <text>');
    end else
      g_Console_Add(_lc[I_MSG_GM_UNAVAIL]);
  end
  else if (cmd = 'an') or (cmd = 'announce') then
  begin
    if g_Game_IsNet then
    begin
      if Length(P) > 1 then
      begin
        for a := 1 to High(P) do
          chstr := chstr + P[a] + ' ';

        if Length(chstr) > 200 then SetLength(chstr, 200);

        if Length(chstr) < 1 then
        begin
          g_Console_Add('announce <text>');
          Exit;
        end;

        chstr := 'centerprint 100 ' + b_Text_Format(chstr);
        if g_Game_IsClient then
          MC_SEND_RCONCommand(chstr)
        else
          g_Console_Process(chstr, True);
      end
      else
        g_Console_Add('announce <text>');
    end else
      g_Console_Add(_lc[I_MSG_GM_UNAVAIL]);
  end
  else if cmd = 'game' then
  begin
    if gGameSettings.GameType <> GT_NONE then
    begin
      g_Console_Add(_lc[I_MSG_GM_UNAVAIL]);
      Exit;
    end;
    if Length(P) = 1 then
    begin
      g_Console_Add(cmd + ' <WAD> [MAP] [# players]');
      Exit;
    end;
    // game not started yet, load fist map from some wad
    found := false;
    s := addWadExtension(P[1]);
    found := e_FindResource(AllMapDirs, s);
    P[1] := s;
    if found then
    begin
      P[1] := ExpandFileName(P[1]);
      // if map not choosed then set first map
      if Length(P) < 3 then
      begin
        SetLength(P, 3);
        P[2] := g_Game_GetFirstMap(P[1]);
      end;

      s := P[1] + ':\' + UpperCase(P[2]);

      if g_Map_Exist(s) then
      begin
        // start game
        g_Game_Free();
        with gGameSettings do
        begin
          Options := gsGameFlags;
          GameMode := g_Game_TextToMode(gsGameMode);
          if gSwitchGameMode <> GM_NONE then
            GameMode := gSwitchGameMode;
          if GameMode = GM_NONE then GameMode := GM_DM;
          if GameMode = GM_SINGLE then GameMode := GM_COOP;
          b := 1;
          if Length(P) >= 4 then
            b := StrToIntDef(P[3], 1);
          g_Game_StartCustom(s, GameMode, TimeLimit,
                             ScoreLimit, MaxLives, Options, b);
        end;
      end
      else
        if P[2] = '' then
          g_Console_Add(Format(_lc[I_MSG_NO_MAPS], [P[1]]))
        else
          g_Console_Add(Format(_lc[I_MSG_NO_MAP_FALLBACK], [UpperCase(P[2]), P[1]]));
    end else
      g_Console_Add(Format(_lc[I_MSG_NO_WAD], [P[1]]));
  end
  else if cmd = 'host' then
  begin
    if gGameSettings.GameType <> GT_NONE then
    begin
      g_Console_Add(_lc[I_MSG_GM_UNAVAIL]);
      Exit;
    end;
    if Length(P) < 4 then
    begin
      g_Console_Add(cmd + ' <listen IP> <port> <WAD> [MAP] [# players]');
      Exit;
    end;
    if not StrToIp(P[1], listen) then
      Exit;
    prt := StrToIntDef(P[2], 25666);

    s := addWadExtension(P[3]);
    found := e_FindResource(AllMapDirs, s);
    P[3] := s;
    if found then
    begin
      // get first map in wad, if not specified
      if Length(P) < 5 then
      begin
        SetLength(P, 5);
        P[4] := g_Game_GetFirstMap(P[1]);
      end;
      s := P[3] + ':\' + UpperCase(P[4]);
      if g_Map_Exist(s) then
      begin
        // start game
        g_Game_Free();
        with gGameSettings do
        begin
          Options := gsGameFlags;
          GameMode := g_Game_TextToMode(gsGameMode);
          if gSwitchGameMode <> GM_NONE then GameMode := gSwitchGameMode;
          if GameMode = GM_NONE then GameMode := GM_DM;
          if GameMode = GM_SINGLE then GameMode := GM_COOP;
          b := 0;
          if Length(P) >= 6 then
            b := StrToIntDef(P[5], 0);
          g_Game_StartServer(s, GameMode, TimeLimit, ScoreLimit, MaxLives, Options, b, listen, prt)
        end
      end
      else
      begin
        if P[4] = '' then
          g_Console_Add(Format(_lc[I_MSG_NO_MAPS], [P[3]]))
        else
          g_Console_Add(Format(_lc[I_MSG_NO_MAP_FALLBACK], [UpperCase(P[4]), P[3]]))
      end
    end
    else
    begin
      g_Console_Add(Format(_lc[I_MSG_NO_WAD], [P[3]]))
    end
  end
  else if cmd = 'map' then
  begin
    if Length(P) = 1 then
    begin
      if g_Game_IsServer and (gGameSettings.GameType <> GT_SINGLE) then
      begin
        g_Console_Add(cmd + ' <MAP>');
        g_Console_Add(cmd + ' <WAD> [MAP]')
      end
      else
      begin
        g_Console_Add(_lc[I_MSG_GM_UNAVAIL])
      end
    end
    else
    begin
      if g_Game_IsServer and (gGameSettings.GameType <> GT_SINGLE) then
      begin
        if Length(P) < 3 then
        begin
          // first param is map or wad
          s := UpperCase(P[1]);
          if g_Map_Exist(gGameSettings.WAD + ':\' + s) then
          begin
            gExitByTrigger := False;
            if gGameOn then
            begin
              // already in game, finish current map
              gNextMap := s;
              gExit := EXIT_ENDLEVELCUSTOM;
            end
            else
            begin
              // intermission, so change map immediately
              g_Game_ChangeMap(s)
            end
          end
          else
          begin
            s := P[1];
            found := e_FindResource(AllMapDirs, s);
            P[1] := s;
            g_Console_Add(Format(_lc[I_MSG_NO_MAP_FALLBACK], [s, 'WAD ' + P[1]]));
            if found then
            begin
              // no such map, found wad
              pw := P[1];
              SetLength(P, 3);
              P[1] := ExpandFileName(pw);
              P[2] := g_Game_GetFirstMap(P[1]);
              s := P[1] + ':\' + P[2];
              if g_Map_Exist(s) then
              begin
                gExitByTrigger := False;
                if gGameOn then
                begin
                  // already in game, finish current map
                  gNextMap := s;
                  gExit := EXIT_ENDLEVELCUSTOM
                end
                else
                begin
                  // intermission, so change map immediately
                  g_Game_ChangeMap(s)
                end
              end
              else
              begin
                if P[2] = '' then
                  g_Console_Add(Format(_lc[I_MSG_NO_MAPS], [P[1]]))
                else
                  g_Console_Add(Format(_lc[I_MSG_NO_MAP], [P[2]]))
              end
            end
            else
            begin
              g_Console_Add(Format(_lc[I_MSG_NO_WAD], [P[1]]))
            end
          end;
        end
        else
        begin
          s := addWadExtension(P[1]);
          found := e_FindResource(AllMapDirs, s);
          P[1] := s;
          if found then
          begin
            P[2] := UpperCase(P[2]);
            s := P[1] + ':\' + P[2];
            if g_Map_Exist(s) then
            begin
              gExitByTrigger := False;
              if gGameOn then
              begin
                gNextMap := s;
                gExit := EXIT_ENDLEVELCUSTOM
              end
              else
              begin
                g_Game_ChangeMap(s)
              end
            end
            else
            begin
              g_Console_Add(Format(_lc[I_MSG_NO_MAP], [P[2]]))
            end
          end
          else
          begin
            g_Console_Add(Format(_lc[I_MSG_NO_WAD], [P[1]]))
          end
        end
      end
      else
      begin
        g_Console_Add(_lc[I_MSG_GM_UNAVAIL])
      end
    end
  end
  else if cmd = 'nextmap' then
  begin
    if not(gGameOn or (gState = STATE_INTERCUSTOM)) then
    begin
      g_Console_Add(_lc[I_MSG_NOT_GAME])
    end
    else
    begin
      nm := True;
      if Length(P) = 1 then
      begin
        if g_Game_IsServer and (gGameSettings.GameType <> GT_SINGLE) then
        begin
          g_Console_Add(cmd + ' <MAP>');
          g_Console_Add(cmd + ' <WAD> [MAP]');
        end
        else
        begin
          nm := False;
          g_Console_Add(_lc[I_MSG_GM_UNAVAIL]);
        end;
      end
      else
      begin
        nm := False;
        if g_Game_IsServer and (gGameSettings.GameType <> GT_SINGLE) then
        begin
          if Length(P) < 3 then
          begin
            // first param is map or wad
            s := UpperCase(P[1]);
            if g_Map_Exist(gGameSettings.WAD + ':\' + s) then
            begin
              // map founded
              gExitByTrigger := False;
              gNextMap := s;
              nm := True;
            end
            else
            begin
              // no such map, found wad
              pw := addWadExtension(P[1]);
              found := e_FindResource(MapDirs, pw);
              if not found then
                found := e_FindResource(WadDirs, pw);
              P[1] := pw;
              g_Console_Add(Format(_lc[I_MSG_NO_MAP_FALLBACK], [s, P[1]]));
              if found then
              begin
                // map not specified, select first map
                SetLength(P, 3);
                P[2] := g_Game_GetFirstMap(P[1]);
                s := P[1] + ':\' + P[2];
                if g_Map_Exist(s) then
                begin
                  gExitByTrigger := False;
                  gNextMap := s;
                  nm := True
                end
                else
                begin
                  if P[2] = '' then
                    g_Console_Add(Format(_lc[I_MSG_NO_MAPS], [P[1]]))
                  else
                    g_Console_Add(Format(_lc[I_MSG_NO_MAP], [P[2]]))
                end
              end
              else
              begin
                g_Console_Add(Format(_lc[I_MSG_NO_WAD], [P[1]]))
              end
            end
          end
          else
          begin
            // specified two params wad + map
            pw := addWadExtension(P[1]);
            found := e_FindResource(MapDirs, pw);
            if not found then
              found := e_FindResource(MapDirs, pw);
            P[1] := pw;
            if found then
            begin
              P[2] := UpperCase(P[2]);
              s := P[1] + ':\' + P[2];
              if g_Map_Exist(s) then
              begin
                gExitByTrigger := False;
                gNextMap := s;
                nm := True
              end
              else
              begin
                g_Console_Add(Format(_lc[I_MSG_NO_MAP], [P[2]]))
              end
            end
            else
            begin
              g_Console_Add(Format(_lc[I_MSG_NO_WAD], [P[1]]))
            end
          end
        end
        else
        begin
          g_Console_Add(_lc[I_MSG_GM_UNAVAIL])
        end
      end;
      if nm then
      begin
        if gNextMap = '' then
          g_Console_Add(_lc[I_MSG_NEXTMAP_UNSET])
        else
          g_Console_Add(Format(_lc[I_MSG_NEXTMAP_SET], [gNextMap]))
      end
    end
  end
  else if (cmd = 'endmap') or (cmd = 'goodbye') then
  begin
    if not gGameOn then
    begin
      g_Console_Add(_lc[I_MSG_NOT_GAME])
    end
    else
    begin
      if g_Game_IsServer and (gGameSettings.GameType <> GT_SINGLE) then
      begin
        gExitByTrigger := False;
        // next map not specified, try to find trigger EXIT
        if (gNextMap = '') and (gTriggers <> nil) then
        begin
          for a := 0 to High(gTriggers) do
          begin
            if gTriggers[a].TriggerType = TRIGGER_EXIT then
            begin
              gExitByTrigger := True;
              //gNextMap := gTriggers[a].Data.MapName;
              gNextMap := gTriggers[a].tgcMap;
              Break
            end
          end
        end;
        if gNextMap = '' then
          gNextMap := g_Game_GetNextMap();
        if not isWadPath(gNextMap) then
          s := gGameSettings.WAD + ':\' + gNextMap
        else
          s := gNextMap;
        if g_Map_Exist(s) then
          gExit := EXIT_ENDLEVELCUSTOM
        else
          g_Console_Add(Format(_lc[I_MSG_NO_MAP], [gNextMap]))
      end
      else
      begin
        g_Console_Add(_lc[I_MSG_GM_UNAVAIL])
      end
    end
  end
  else if (cmd = 'event') then
  begin
    if (Length(P) <= 1) then
    begin
      for a := 0 to High(gEvents) do
        if gEvents[a].Command = '' then
          g_Console_Add(gEvents[a].Name + ' <none>')
        else
          g_Console_Add(gEvents[a].Name + ' "' + gEvents[a].Command + '"');
      Exit;
    end;
    if (Length(P) = 2) then
    begin
      for a := 0 to High(gEvents) do
        if gEvents[a].Name = P[1] then
          if gEvents[a].Command = '' then
            g_Console_Add(gEvents[a].Name + ' <none>')
          else
            g_Console_Add(gEvents[a].Name + ' "' + gEvents[a].Command + '"');
      Exit;
    end;
    for a := 0 to High(gEvents) do
      if gEvents[a].Name = P[1] then
      begin
        gEvents[a].Command := '';
        for b := 2 to High(P) do
          if Pos(' ', P[b]) = 0 then
            gEvents[a].Command := gEvents[a].Command + ' ' + P[b]
          else
            gEvents[a].Command := gEvents[a].Command + ' "' + P[b] + '"';
        gEvents[a].Command := Trim(gEvents[a].Command);
        Exit;
      end;
  end
  else if cmd = 'suicide' then
  begin
    if gGameOn then
    begin
      if g_Game_IsClient then
        MC_SEND_CheatRequest(NET_CHEAT_SUICIDE)
      else
      begin
        if gPlayer1 <> nil then
          gPlayer1.Damage(SUICIDE_DAMAGE, gPlayer1.UID, 0, 0, HIT_SELF);
        if gPlayer2 <> nil then
          gPlayer2.Damage(SUICIDE_DAMAGE, gPlayer2.UID, 0, 0, HIT_SELF);
      end;
    end;
  end
  else if cmd = 'screenshot' then
  begin
    g_TakeScreenShot()
  end
  else if (cmd = 'weapnext') or (cmd = 'weapprev') then
  begin
    a := 1 - (ord(cmd[5]) - ord('n'));
    if a = -1 then
      gWeaponAction[0, WP_PREV] := True;
    if a = 1 then
      gWeaponAction[0, WP_NEXT] := True;
  end
  else if cmd = 'weapon' then
  begin
    if Length(p) = 2 then
    begin
      a := WP_FIRST + StrToIntDef(p[1], 0) - 1;
      if (a >= WP_FIRST) and (a <= WP_LAST) then
        gSelectWeapon[0, a] := True
    end
  end
  else if (cmd = 'p1_weapnext') or (cmd = 'p1_weapprev')
       or (cmd = 'p2_weapnext') or (cmd = 'p2_weapprev') then
  begin
    a := 1 - (ord(cmd[8]) - ord('n'));
    b := ord(cmd[2]) - ord('1');
    if a = -1 then
      gWeaponAction[b, WP_PREV] := True;
    if a = 1 then
      gWeaponAction[b, WP_NEXT] := True;
  end
  else if (cmd = 'p1_weapon') or (cmd = 'p2_weapon') then
  begin
    if Length(p) = 2 then
    begin
      a := WP_FIRST + StrToIntDef(p[1], 0) - 1;
      b := ord(cmd[2]) - ord('1');
      if (a >= WP_FIRST) and (a <= WP_LAST) then
        gSelectWeapon[b, a] := True
    end
  end
  else if (cmd = 'p1_weapbest') or (cmd = 'p2_weapbest') then
  begin
    b := ord(cmd[2]) - ord('1');
    if b = 0 then
      gSelectWeapon[b, gPlayer1.GetMorePrefered()] := True
    else
      gSelectWeapon[b, gPlayer2.GetMorePrefered()] := True;
  end
  else if (cmd = 'dropflag') then
  begin
    if g_Game_IsServer then
    begin
      if gPlayer2 <> nil then gPlayer2.TryDropFlag();
      if gPlayer1 <> nil then gPlayer1.TryDropFlag();
    end
    else
      MC_SEND_CheatRequest(NET_CHEAT_DROPFLAG);
  end
  else if (cmd = 'p1_dropflag') or (cmd = 'p2_dropflag') then
  begin
    b := ord(cmd[2]) - ord('1');
    if g_Game_IsServer then
    begin
      if (b = 1) and (gPlayer2 <> nil) then gPlayer2.TryDropFlag()
      else if (b = 0) and (gPlayer1 <> nil) then gPlayer1.TryDropFlag();
    end
    else
      MC_SEND_CheatRequest(NET_CHEAT_DROPFLAG);
  end
// Команды Своей игры:
  else if gGameSettings.GameType in [GT_CUSTOM, GT_SERVER, GT_CLIENT] then
  begin
    if cmd = 'bot_addred' then
    begin
      if Length(P) > 1 then
        g_Bot_Add(TEAM_RED, StrToIntDef(P[1], 2))
      else
        g_Bot_Add(TEAM_RED, 2);
    end
    else if cmd = 'bot_addblue' then
    begin
      if Length(P) > 1 then
        g_Bot_Add(TEAM_BLUE, StrToIntDef(P[1], 2))
      else
        g_Bot_Add(TEAM_BLUE, 2);
    end
    else if cmd = 'spectate' then
    begin
      if not gGameOn then
        Exit;
      g_Game_Spectate();
    end
    else if cmd = 'say' then
    begin
      if g_Game_IsServer and g_Game_IsNet then
      begin
        if Length(P) > 1 then
        begin
          chstr := '';
          for a := 1 to High(P) do
            chstr := chstr + P[a] + ' ';

          if Length(chstr) > 200 then SetLength(chstr, 200);

          if Length(chstr) < 1 then
          begin
            g_Console_Add('say <text>');
            Exit;
          end;

          chstr := b_Text_Format(chstr);
          MH_SEND_Chat(chstr, NET_CHAT_PLAYER);
        end
        else g_Console_Add('say <text>');
      end else
        g_Console_Add(_lc[I_MSG_SERVERONLY]);
    end
    else if cmd = 'tell' then
    begin
      if g_Game_IsServer and g_Game_IsNet then
      begin
        if (Length(P) > 2) and (P[1] <> '') then
        begin
          chstr := '';
          for a := 2 to High(P) do
            chstr := chstr + P[a] + ' ';

          if Length(chstr) > 200 then SetLength(chstr, 200);

          if Length(chstr) < 1 then
          begin
            g_Console_Add('tell <playername> <text>');
            Exit;
          end;

          pl := g_Net_Client_ByName(P[1]);
          if pl <> nil then
            MH_SEND_Chat(b_Text_Format(chstr), NET_CHAT_PLAYER, pl^.ID)
          else
            g_Console_Add(Format(_lc[I_NET_ERR_NAME404], [P[1]]));
        end
        else g_Console_Add('tell <playername> <text>');
      end else
        g_Console_Add(_lc[I_MSG_SERVERONLY]);
    end
    else if cmd = 'centerprint' then
    begin
      if (Length(P) > 2) and (P[1] <> '') then
      begin
        chstr := '';
        for a := 2 to High(P) do
          chstr := chstr + P[a] + ' ';

        if Length(chstr) > 200 then SetLength(chstr, 200);

        if Length(chstr) < 1 then
        begin
          g_Console_Add('centerprint <timeout> <text>');
          Exit;
        end;

        a := StrToIntDef(P[1], 100);
        chstr := b_Text_Format(chstr);
        g_Game_Message(chstr, a);
        if g_Game_IsNet and g_Game_IsServer then
          MH_SEND_GameEvent(NET_EV_BIGTEXT, a, chstr);
      end
      else g_Console_Add('centerprint <timeout> <text>');
    end
    else if (cmd = 'overtime') and not g_Game_IsClient then
    begin
      if (Length(P) = 1) or (StrToIntDef(P[1], -1) <= 0) then
        Exit;
      // Дополнительное время:
      gGameSettings.TimeLimit := (gTime - gGameStartTime) div 1000 + Word(StrToIntDef(P[1], 0));

      g_Console_Add(Format(_lc[I_MSG_TIME_LIMIT],
                           [gGameSettings.TimeLimit div 3600,
                           (gGameSettings.TimeLimit div 60) mod 60,
                            gGameSettings.TimeLimit mod 60]));
      if g_Game_IsNet then MH_SEND_GameSettings;
    end
    else if (cmd = 'rcon_password') and g_Game_IsClient then
    begin
      if (Length(P) <= 1) then
        g_Console_Add('rcon_password <password>')
      else
        MC_SEND_RCONPassword(P[1]);
    end
    else if cmd = 'rcon' then
    begin
      if g_Game_IsClient then
      begin
        if Length(P) > 1 then
        begin
          chstr := '';
          for a := 1 to High(P) do
            chstr := chstr + P[a] + ' ';

          if Length(chstr) > 200 then SetLength(chstr, 200);

          if Length(chstr) < 1 then
          begin
            g_Console_Add('rcon <command>');
            Exit;
          end;

          MC_SEND_RCONCommand(chstr);
        end
        else g_Console_Add('rcon <command>');
      end;
    end
    else if cmd = 'ready' then
    begin
      if g_Game_IsServer and (gLMSRespawn = LMS_RESPAWN_WARMUP) then
        gLMSRespawnTime := gTime + 100;
    end
    else if (cmd = 'callvote') and g_Game_IsNet then
    begin
      if Length(P) > 1 then
      begin
        chstr := '';
        for a := 1 to High(P) do begin
          if a > 1 then chstr := chstr + ' ';
          chstr := chstr + P[a];
        end;

        if Length(chstr) > 200 then SetLength(chstr, 200);

        if Length(chstr) < 1 then
        begin
          g_Console_Add('callvote <command>');
          Exit;
        end;

        if g_Game_IsClient then
          MC_SEND_Vote(True, chstr)
        else
          g_Game_StartVote(chstr, gPlayer1Settings.Name);
        g_Console_Process('vote', True);
      end
      else
        g_Console_Add('callvote <command>');
    end
    else if (cmd = 'vote') and g_Game_IsNet then
    begin
      if g_Game_IsClient then
        MC_SEND_Vote(False)
      else if gVoteInProgress then
      begin
        if (gPlayer1 <> nil) or (gPlayer2 <> nil) then
          a := Floor((NetClientCount+1)/2.0) + 1
        else
          a := Floor(NetClientCount/2.0) + 1;
        if gVoted then
        begin
          Dec(gVoteCount);
          gVoted := False;
          g_Console_Add(Format(_lc[I_MESSAGE_VOTE_REVOKED], [gPlayer1Settings.Name, gVoteCount, a]), True);
          MH_SEND_VoteEvent(NET_VE_REVOKE, gPlayer1Settings.Name, 'a', gVoteCount, a);
        end
        else
        begin
          Inc(gVoteCount);
          gVoted := True;
          g_Console_Add(Format(_lc[I_MESSAGE_VOTE_VOTE], [gPlayer1Settings.Name, gVoteCount, a]), True);
          MH_SEND_VoteEvent(NET_VE_VOTE, gPlayer1Settings.Name, 'a', gVoteCount, a);
          g_Game_CheckVote;
        end;
      end;
    end
  end;
end;

procedure SystemCommands(P: SSArray);
var
  cmd: string;
begin
  cmd := LowerCase(P[0]);
  case cmd of
    'exit', 'quit':
      begin
        g_Game_Free();
        g_Game_Quit();
      end;
    'r_reset':
      begin
        gRC_Width := Max(1, gRC_Width);
        gRC_Height := Max(1, gRC_Height);
        gBPP := Max(1, gBPP);
        if sys_SetDisplayMode(gRC_Width, gRC_Height, gBPP, gRC_FullScreen, gRC_Maximized) = True then
          e_LogWriteln('resolution changed')
        else
          e_LogWriteln('resolution not changed');
        sys_EnableVSync(gVSync);
      end;
    'r_maxfps':
      begin
        if Length(p) = 2 then
        begin
          gMaxFPS := StrToIntDef(p[1], gMaxFPS);
          if gMaxFPS > 0 then
            gFrameTime := 1000 div gMaxFPS
          else
            gFrameTime := 0;
        end;
        e_LogWritefln('r_maxfps %d', [gMaxFPS]);
      end;
    'g_language':
      begin
        if Length(p) = 2 then
        begin
          gAskLanguage := true;
          gLanguage := LANGUAGE_ENGLISH;
          case LowerCase(p[1]) of
            'english':
               begin
                 gAskLanguage := false;
                 gLanguage := LANGUAGE_ENGLISH;
               end;
            'russian':
               begin
                 gAskLanguage := false;
                 gLanguage := LANGUAGE_RUSSIAN;
               end;
            'ask':
               begin
                 gAskLanguage := true;
                 gLanguage := LANGUAGE_ENGLISH;
               end;
          end;
          g_Language_Set(gLanguage);
        end
        else
        begin
          e_LogWritefln('usage: %s <English|Russian|Ask>', [cmd]);
        end
      end;
  end;
end;

procedure g_TakeScreenShot(Filename: string = '');
  var s: TStream; t: TDateTime; dir, date, name: String;
begin
  if e_NoGraphics then Exit;
  try
    dir := e_GetWriteableDir(ScreenshotDirs);

    if Filename = '' then
    begin
      t := Now;
      DateTimeToString(date, 'yyyy-mm-dd-hh-nn-ss', t);
      Filename := 'screenshot-' + date;
    end;
    
    name := e_CatPath(dir, Filename + '.png');
    s := createDiskFile(name);
    try
      e_MakeScreenshot(s, gWinSizeX, gWinSizeY);
      s.Free;
      g_Console_Add(Format(_lc[I_CONSOLE_SCREENSHOT], [name]))
    except
      g_Console_Add(Format(_lc[I_CONSOLE_ERROR_WRITE], [name]));
      s.Free;
      DeleteFile(name)
    end
  except
    g_Console_Add('oh shit, i can''t create screenshot!')
  end
end;

procedure g_Game_InGameMenu(Show: Boolean);
begin
  if (g_ActiveWindow = nil) and Show then
    begin
      if gGameSettings.GameType = GT_SINGLE then
        g_GUI_ShowWindow('GameSingleMenu')
      else
      begin
        if g_Game_IsClient then
          g_GUI_ShowWindow('GameClientMenu')
        else
          if g_Game_IsNet then
            g_GUI_ShowWindow('GameServerMenu')
          else
            g_GUI_ShowWindow('GameCustomMenu');
      end;
      g_Sound_PlayEx('MENU_OPEN');

    // Пауза при меню только в одиночной игре:
      if (not g_Game_IsNet) then
        g_Game_Pause(True);
    end
  else
    if (g_ActiveWindow <> nil) and (not Show) then
    begin
    // Пауза при меню только в одиночной игре:
      if (not g_Game_IsNet) then
        g_Game_Pause(False);
    end;
end;

procedure g_Game_Pause (Enable: Boolean);
var
  oldPause: Boolean;
begin
  if not gGameOn then exit;

  if not (gGameSettings.GameType in [GT_SINGLE, GT_CUSTOM]) then exit;

  oldPause := gPause;
  gPauseMain := Enable;

  if (gPause <> oldPause) then g_Game_PauseAllSounds(gPause);
end;

procedure g_Game_HolmesPause (Enable: Boolean);
var
  oldPause: Boolean;
begin
  if not gGameOn then exit;
  if not (gGameSettings.GameType in [GT_SINGLE, GT_CUSTOM]) then exit;

  oldPause := gPause;
  gPauseHolmes := Enable;

  if (gPause <> oldPause) then g_Game_PauseAllSounds(gPause);
end;

procedure g_Game_PauseAllSounds(Enable: Boolean);
var
  i: Integer;
begin
// Триггеры:
  if gTriggers <> nil then
    for i := 0 to High(gTriggers) do
      with gTriggers[i] do
        if (TriggerType = TRIGGER_SOUND) and
           (Sound <> nil) and
           Sound.IsPlaying() then
        begin
          Sound.Pause(Enable);
        end;

// Звуки игроков:
  if gPlayers <> nil then
    for i := 0 to High(gPlayers) do
      if gPlayers[i] <> nil then
        gPlayers[i].PauseSounds(Enable);

// Музыка:
  if gMusic <> nil then
    gMusic.Pause(Enable);
end;

procedure g_Game_StopAllSounds(all: Boolean);
var
  i: Integer;
begin
  if gTriggers <> nil then
    for i := 0 to High(gTriggers) do
      with gTriggers[i] do
        if (TriggerType = TRIGGER_SOUND) and
           (Sound <> nil) then
          Sound.Stop();

  if gMusic <> nil then
    gMusic.Stop();

  if all then
    e_StopChannels();
end;

procedure g_Game_UpdateTriggerSounds;
  var i: Integer;
begin
  if gTriggers <> nil then
    for i := 0 to High(gTriggers) do
      with gTriggers[i] do
        if (TriggerType = TRIGGER_SOUND) and (Sound <> nil) and tgcLocal and Sound.IsPlaying() then
          Sound.SetCoordsRect(X, Y, Width, Height, tgcVolume / 255.0)
end;

function g_Game_IsWatchedPlayer(UID: Word): Boolean;
begin
  Result := False;
  if (gPlayer1 <> nil) and (gPlayer1.UID = UID) then
  begin
    Result := True;
    Exit;
  end;
  if (gPlayer2 <> nil) and (gPlayer2.UID = UID) then
  begin
    Result := True;
    Exit;
  end;
  if gSpectMode <> SPECT_PLAYERS then
    Exit;
  if gSpectPID1 = UID then
  begin
    Result := True;
    Exit;
  end;
  if gSpectViewTwo and (gSpectPID2 = UID) then
  begin
    Result := True;
    Exit;
  end;
end;

function g_Game_IsWatchedTeam(Team: Byte): Boolean;
var
  Pl: TPlayer;
begin
  Result := False;
  if (gPlayer1 <> nil) and (gPlayer1.Team = Team) then
  begin
    Result := True;
    Exit;
  end;
  if (gPlayer2 <> nil) and (gPlayer2.Team = Team) then
  begin
    Result := True;
    Exit;
  end;
  if gSpectMode <> SPECT_PLAYERS then
    Exit;
  Pl := g_Player_Get(gSpectPID1);
  if (Pl <> nil) and (Pl.Team = Team) then
  begin
    Result := True;
    Exit;
  end;
  if gSpectViewTwo then
  begin
    Pl := g_Player_Get(gSpectPID2);
    if (Pl <> nil) and (Pl.Team = Team) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure g_Game_Message(Msg: string; Time: Word);
begin
  MessageLineLength := (gScreenWidth - 204) div e_CharFont_GetMaxWidth(gMenuFont);
  MessageText := b_Text_Wrap(b_Text_Format(Msg), MessageLineLength);
  MessageTime := Time;
end;

procedure g_Game_ChatSound(Text: String; Taunt: Boolean = True);
const
  punct: Array[0..13] of String =
  ('.', ',', ':', ';', '!', '?', '(', ')', '''', '"', '/', '\', '*', '^');
var
  i, j: Integer;
  ok: Boolean;
  fpText: String;

  function IsPunctuation(S: String): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    if Length(S) <> 1 then
      Exit;
    for i := Low(punct) to High(punct) do
      if S = punct[i] then
      begin
        Result := True;
        break;
      end;
  end;
  function FilterPunctuation(S: String): String;
  var
    i: Integer;
  begin
    for i := Low(punct) to High(punct) do
      S := StringReplace(S, punct[i], ' ', [rfReplaceAll]);
    Result := S;
  end;
begin
  ok := False;

  if gUseChatSounds and Taunt and (gChatSounds <> nil) and (Pos(': ', Text) > 0) then
  begin
    // remove player name
    Delete(Text, 1, Pos(': ', Text) + 2 - 1);
    // for FullWord check
    Text := toLowerCase1251(' ' + Text + ' ');
    fpText := FilterPunctuation(Text);

    for i := 0 to Length(gChatSounds) - 1 do
    begin
      ok := True;
      for j := 0 to Length(gChatSounds[i].Tags) - 1 do
      begin
        if gChatSounds[i].FullWord and (not IsPunctuation(gChatSounds[i].Tags[j])) then
          ok := Pos(' ' + gChatSounds[i].Tags[j] + ' ', fpText) > 0
        else
          ok := Pos(gChatSounds[i].Tags[j], Text) > 0;
        if not ok then
          break;
      end;
      if ok then
      begin
        gChatSounds[i].Sound.Play();
        break;
      end;
    end;
  end;
  if not ok then
    g_Sound_PlayEx('SOUND_GAME_RADIO');
end;

procedure g_Game_Announce_GoodShot(SpawnerUID: Word);
var
  a: Integer;
begin
  case gAnnouncer of
    ANNOUNCE_NONE:
      Exit;
    ANNOUNCE_ME,
    ANNOUNCE_MEPLUS:
      if not g_Game_IsWatchedPlayer(SpawnerUID) then
        Exit;
  end;
  for a := 0 to 3 do
    if goodsnd[a].IsPlaying() then
      Exit;

  goodsnd[Random(4)].Play();
end;

procedure g_Game_Announce_KillCombo(Param: Integer);
var
  UID: Word;
  c, n: Byte;
  Pl: TPlayer;
  Name: String;
begin
  UID := Param and $FFFF;
  c := Param shr 16;
  if c < 2 then
    Exit;

  Pl := g_Player_Get(UID);
  if Pl = nil then
    Name := '?'
  else
    Name := Pl.Name;

  case c of
    2: begin
      n := 0;
      g_Console_Add(Format(_lc[I_PLAYER_KILL_2X], [Name]), True);
    end;
    3: begin
      n := 1;
      g_Console_Add(Format(_lc[I_PLAYER_KILL_3X], [Name]), True);
    end;
    4: begin
      n := 2;
      g_Console_Add(Format(_lc[I_PLAYER_KILL_4X], [Name]), True);
    end;
    else begin
      n := 3;
      g_Console_Add(Format(_lc[I_PLAYER_KILL_MX], [Name]), True);
    end;
  end;

  case gAnnouncer of
    ANNOUNCE_NONE:
      Exit;
    ANNOUNCE_ME:
      if not g_Game_IsWatchedPlayer(UID) then
        Exit;
    ANNOUNCE_MEPLUS:
      if (not g_Game_IsWatchedPlayer(UID)) and (c < 4) then
        Exit;
  end;

  if killsnd[n].IsPlaying() then
    killsnd[n].Stop();
  killsnd[n].Play();
end;

procedure g_Game_Announce_BodyKill(SpawnerUID: Word);
var
  a: Integer;
begin
  case gAnnouncer of
    ANNOUNCE_NONE:
      Exit;
    ANNOUNCE_ME:
      if not g_Game_IsWatchedPlayer(SpawnerUID) then
        Exit;
  end;
  for a := 0 to 2 do
    if hahasnd[a].IsPlaying() then
      Exit;

  hahasnd[Random(3)].Play();
end;

procedure g_Game_StartVote(Command, Initiator: string);
var
  Need: Integer;
begin
  if not gVotesEnabled then Exit;
  if gGameSettings.GameType <> GT_SERVER then Exit;
  if gVoteInProgress or gVotePassed then
  begin
    g_Console_Add(Format(_lc[I_MESSAGE_VOTE_INPROGRESS], [gVoteCommand]), True);
    MH_SEND_VoteEvent(NET_VE_INPROGRESS, gVoteCommand);
    Exit;
  end;
  gVoteInProgress := True;
  gVotePassed := False;
  gVoteTimer := gTime + gVoteTimeout * 1000;
  gVoteCount := 0;
  gVoted := False;
  gVoteCommand := Command;

  if (gPlayer1 <> nil) or (gPlayer2 <> nil) then
    Need := Floor((NetClientCount+1)/2.0)+1
  else
    Need := Floor(NetClientCount/2.0)+1;
  g_Console_Add(Format(_lc[I_MESSAGE_VOTE_STARTED], [Initiator, Command, Need]), True);
  MH_SEND_VoteEvent(NET_VE_STARTED, Initiator, Command, Need);
end;

procedure g_Game_CheckVote;
var
  I, Need: Integer;
begin
  if gGameSettings.GameType <> GT_SERVER then Exit;
  if not gVoteInProgress then Exit;

  if (gTime >= gVoteTimer) then
  begin
    if (gPlayer1 <> nil) or (gPlayer2 <> nil) then
      Need := Floor((NetClientCount+1)/2.0) + 1
    else
      Need := Floor(NetClientCount/2.0) + 1;
    if gVoteCount >= Need then
    begin
      g_Console_Add(Format(_lc[I_MESSAGE_VOTE_PASSED], [gVoteCommand]), True);
      MH_SEND_VoteEvent(NET_VE_PASSED, gVoteCommand);
      gVotePassed := True;
      gVoteCmdTimer := gTime + 5000;
    end
    else
    begin
      g_Console_Add(_lc[I_MESSAGE_VOTE_FAILED], True);
      MH_SEND_VoteEvent(NET_VE_FAILED);
    end;
    if NetClients <> nil then
      for I := Low(NetClients) to High(NetClients) do
        if NetClients[i].Used then
          NetClients[i].Voted := False;
    gVoteInProgress := False;
    gVoted := False;
    gVoteCount := 0;
  end
  else
  begin
    if (gPlayer1 <> nil) or (gPlayer2 <> nil) then
      Need := Floor((NetClientCount+1)/2.0) + 1
    else
      Need := Floor(NetClientCount/2.0) + 1;
    if gVoteCount >= Need then
    begin
      g_Console_Add(Format(_lc[I_MESSAGE_VOTE_PASSED], [gVoteCommand]), True);
      MH_SEND_VoteEvent(NET_VE_PASSED, gVoteCommand);
      gVoteInProgress := False;
      gVotePassed := True;
      gVoteCmdTimer := gTime + 5000;
      gVoted := False;
      gVoteCount := 0;
      if NetClients <> nil then
        for I := Low(NetClients) to High(NetClients) do
          if NetClients[i].Used then
            NetClients[i].Voted := False;
    end;
  end;
end;

procedure g_Game_LoadMapList(FileName: string);
var
  ListFile: TextFile;
  s: string;
begin
  MapList := nil;
  MapIndex := -1;

  if not FileExists(FileName) then Exit;

  AssignFile(ListFile, FileName);
  Reset(ListFile);
  while not EOF(ListFile) do
  begin
    ReadLn(ListFile, s);

    s := Trim(s);
    if s = '' then Continue;

    SetLength(MapList, Length(MapList)+1);
    MapList[High(MapList)] := s;
  end;
  CloseFile(ListFile);
end;

procedure g_Game_SetDebugMode();
begin
  gDebugMode := True;
// Читы (даже в своей игре):
  gCheats := True;
end;

procedure g_Game_SetLoadingText(Text: String; Max: Integer; reWrite: Boolean);
var
  i: Word;
begin
  if Length(LoadingStat.Msgs) = 0 then
    Exit;

  with LoadingStat do
  begin
    if not reWrite then
    begin // Переходим на следующую строку или скроллируем:
      if NextMsg = Length(Msgs) then
        begin // scroll
          for i := 0 to High(Msgs)-1 do
            Msgs[i] := Msgs[i+1];
        end
      else
        Inc(NextMsg);
    end else
      if NextMsg = 0 then
        Inc(NextMsg);

    Msgs[NextMsg-1] := Text;
    CurValue := 0;
    MaxValue := Max;
    ShowCount := 0;
    PBarWasHere := false;
  end;

  g_ActiveWindow := nil;
  ProcessLoading(True);
end;

procedure g_Game_StepLoading(Value: Integer = -1);
begin
  with LoadingStat do
  begin
    if Value = -1 then
    begin
      Inc(CurValue);
      Inc(ShowCount);
    end
    else
      CurValue := Value;

    if (ShowCount > LOADING_SHOW_STEP) or (Value > -1) then
    begin
      ShowCount := 0;
      ProcessLoading(False);
    end;
  end;
end;

procedure g_Game_ClearLoading();
var
  len: Word;
begin
  with LoadingStat do
  begin
    CurValue := 0;
    MaxValue := 0;
    ShowCount := 0;
    len := ((gScreenHeight div 3)*2 - 50) div LOADING_INTERLINE;
    if len < 1 then len := 1;
    SetLength(Msgs, len);
    for len := Low(Msgs) to High(Msgs) do
      Msgs[len] := '';
    NextMsg := 0;
    PBarWasHere := false;
  end;
end;

procedure Parse_Params(var pars: TParamStrValues);
var
  i: Integer;
  s: String;
begin
  SetLength(pars, 0);
  i := 1;
  while i <= ParamCount do
  begin
    s := ParamStr(i);
    if (Length(s) > 1) and (s[1] = '-') then
    begin
      if (Length(s) > 2) and (s[2] = '-') then
        begin // Одиночный параметр
          SetLength(pars, Length(pars) + 1);
          with pars[High(pars)] do
          begin
            Name := LowerCase(s);
            Value := '+';
          end;
        end
      else
        if (i < ParamCount) then
        begin // Параметр со значением
          Inc(i);
          SetLength(pars, Length(pars) + 1);
          with pars[High(pars)] do
          begin
            Name := LowerCase(s);
            Value := LowerCase(ParamStr(i));
          end;
        end;
    end;

    Inc(i);
  end;
end;

function Find_Param_Value(var pars: TParamStrValues; aName: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(pars) do
    if pars[i].Name = aName then
    begin
      Result := pars[i].Value;
      Break;
    end;
end;

procedure g_Game_Process_Params();
var
  pars: TParamStrValues;
  map: String;
  GMode, n: Byte;
  LimT, LimS: Integer;
  Opt: LongWord;
  Lives: Integer;
  s: String;
  Port: Integer;
  ip: String;
  F: TextFile;
begin
  Parse_Params(pars);

// Debug mode:
  s := Find_Param_Value(pars, '--debug');
  if (s <> '') then
  begin
    g_Game_SetDebugMode();
    s := Find_Param_Value(pars, '--netdump');
    if (s <> '') then
      NetDump := True;
  end;

// Connect when game loads
  ip := Find_Param_Value(pars, '-connect');

  if ip <> '' then
  begin
    s := Find_Param_Value(pars, '-port');
    if (s = '') or not TryStrToInt(s, Port) then
      Port := 25666;

    s := Find_Param_Value(pars, '-pw');

    g_Game_StartClient(ip, Port, s);
    Exit;
  end;

  s := LowerCase(Find_Param_Value(pars, '-dbg-mainwad'));
  if (s <> '') then
  begin
    gDefaultMegawadStart := s;
  end;

  if (Find_Param_Value(pars, '--dbg-mainwad-restore') <> '') or
     (Find_Param_Value(pars, '--dbg-mainwad-default') <> '') then
  begin
    gDefaultMegawadStart := DF_Default_Megawad_Start;
  end;

// Start map when game loads:
  map := LowerCase(Find_Param_Value(pars, '-map'));
  if isWadPath(map) then
  begin
  // Game mode:
    s := Find_Param_Value(pars, '-gm');
    GMode := g_Game_TextToMode(s);
    if GMode = GM_NONE then GMode := GM_DM;
    if GMode = GM_SINGLE then GMode := GM_COOP;

  // Time limit:
    s := Find_Param_Value(pars, '-limt');
    if (s = '') or (not TryStrToInt(s, LimT)) then
      LimT := 0;
    if LimT < 0 then
      LimT := 0;

  // Score limit:
    s := Find_Param_Value(pars, '-lims');
    if (s = '') or (not TryStrToInt(s, LimS)) then
      LimS := 0;
    if LimS < 0 then
      LimS := 0;

  // Lives limit:
    s := Find_Param_Value(pars, '-lives');
    if (s = '') or (not TryStrToInt(s, Lives)) then
      Lives := 0;
    if Lives < 0 then
      Lives := 0;

  // Options:
    s := Find_Param_Value(pars, '-opt');
    if (s = '') then
      Opt := gsGameFlags
    else
      Opt := StrToIntDef(s, 0);

  // Close after map:
    s := Find_Param_Value(pars, '--close');
    if (s <> '') then
      gMapOnce := True;

  // Override map to test:
    s := LowerCase(Find_Param_Value(pars, '-testmap'));
    if s <> '' then
    begin
      if e_IsValidResourceName(s) then
        e_FindResource(AllMapDirs, s);
      gTestMap := ExpandFileName(s);
    end;

  // Delete test map after play:
    s := Find_Param_Value(pars, '--testdelete');
    if (s <> '') then
    begin
      //gMapToDelete := MapsDir + map;
      e_WriteLog('"--testdelete" is deprecated, use --tempdelete.', TMsgType.Fatal);
      Halt(1);
    end;

  // Delete temporary WAD after play:
    s := Find_Param_Value(pars, '--tempdelete');
    if (s <> '') and (gTestMap <> '') then
    begin
      gMapToDelete := gTestMap;
      gTempDelete := True;
    end;

  // Number of players:
    s := Find_Param_Value(pars, '-pl');
    if (s = '') then
      n := DEFAULT_PLAYERS
    else
      n := StrToIntDef(s, DEFAULT_PLAYERS);

  // Start:
    s := Find_Param_Value(pars, '-port');
    if (s = '') or not TryStrToInt(s, Port) then
      g_Game_StartCustom(map, GMode, LimT, LimS, Lives, Opt, n)
    else
      g_Game_StartServer(map, GMode, LimT, LimS, Lives, Opt, n, 0, Port);
  end;

// Execute script when game loads:
  s := Find_Param_Value(pars, '-exec');
  if s <> '' then
  begin
//    if not isWadPath(s) then
//      s := GameDir + '/' + s;

    {$I-}
    AssignFile(F, s);
    Reset(F);
    if IOResult <> 0 then
    begin
      e_WriteLog(Format(_lc[I_SIMPLE_ERROR], ['Failed to read file: ' + s]), TMsgType.Warning);
      g_Console_Add(Format(_lc[I_CONSOLE_ERROR_READ], [s]));
      CloseFile(F);
      Exit;
    end;
    e_WriteLog('Executing script: ' + s, TMsgType.Notify);
    g_Console_Add(Format(_lc[I_CONSOLE_EXEC], [s]));

    while not EOF(F) do
    begin
      ReadLn(F, s);
      if IOResult <> 0 then
      begin
        e_WriteLog(Format(_lc[I_SIMPLE_ERROR], ['Failed to read file: ' + s]), TMsgType.Warning);
        g_Console_Add(Format(_lc[I_CONSOLE_ERROR_READ], [s]));
        CloseFile(F);
        Exit;
      end;
      if Pos('#', s) <> 1 then // script comment
        g_Console_Process(s, True);
    end;

    CloseFile(F);
    {$I+}
  end;

  SetLength(pars, 0);
end;

begin
  conRegVar('pf_draw_frame', @g_profile_frame_draw, 'draw frame rendering profiles', 'render profiles');
  //conRegVar('pf_update_frame', @g_profile_frame_update, 'draw frame updating profiles', 'update profiles');
  conRegVar('pf_coldet', @g_profile_collision, 'draw collision detection profiles', 'coldet profiles');
  conRegVar('pf_los', @g_profile_los, 'draw monster LOS profiles', 'monster LOS profiles');

  conRegVar('r_sq_draw', @gdbg_map_use_accel_render, 'accelerated spatial queries in rendering', 'accelerated rendering');
  conRegVar('cd_sq_enabled', @gdbg_map_use_accel_coldet, 'accelerated spatial queries in map coldet', 'accelerated map coldet');
  conRegVar('mon_sq_enabled', @gmon_debug_use_sqaccel, 'accelerated spatial queries for monsters', 'accelerated monster coldet');
  conRegVar('wtrace_sq_enabled', @gwep_debug_fast_trace, 'accelerated spatial queries for weapon hitscan trace', 'accelerated weapon hitscan');

  conRegVar('pr_enabled', @gpart_dbg_enabled, 'enable/disable particles', 'particles');
  conRegVar('pr_phys_enabled', @gpart_dbg_phys_enabled, 'enable/disable particle physics', 'particle physics');

  conRegVar('los_enabled', @gmon_dbg_los_enabled, 'enable/disable monster LOS calculations', 'monster LOS', true);
  conRegVar('mon_think', @gmon_debug_think, 'enable/disable monster thinking', 'monster thinking', true);

{$IFDEF ENABLE_HOLMES}
  conRegVar('dbg_holmes', @g_holmes_enabled, 'enable/disable Holmes', 'Holmes', true);
{$ENDIF}

  conRegVar('dbg_ignore_level_bounds', @g_dbg_ignore_bounds, 'ignore level bounds', '',  false);

  conRegVar('r_scale', @g_dbg_scale, 0.01, 100.0, 'render scale', '',  false);
  conRegVar('r_resolution_scale', @r_pixel_scale, 0.01, 100.0, 'upscale factor', '', false);

  conRegVar('light_enabled', @gwin_k8_enable_light_experiments, 'enable/disable dynamic lighting', 'lighting');
  conRegVar('light_player_halo', @g_playerLight, 'enable/disable player halo', 'player light halo');

  conRegVar('r_smallmap_align_h', @r_smallmap_h, 'halign: 0: left; 1: center; 2: right', 'horizontal aligning of small maps');
  conRegVar('r_smallmap_align_v', @r_smallmap_v, 'valign: 0: top; 1: center; 2: bottom', 'vertial aligning of small maps');

  conRegVar('r_showfps', @gShowFPS, 'draw fps counter', 'draw fps counter');
  conRegVar('r_showtime', @gShowTime, 'show game time', 'show game time');
  conRegVar('r_showping', @gShowPing, 'show ping', 'show ping');
  conRegVar('r_showscore', @gShowScore, 'show score', 'show score');
  conRegVar('r_showkillmsg', @gShowKillMsg, 'show kill log', 'show kill log');
  conRegVar('r_showlives', @gShowLives, 'show lives', 'show lives');
  conRegVar('r_showspect', @gSpectHUD, 'show spectator hud', 'show spectator hud');
  conRegVar('r_showstat', @gShowStat, 'show stats', 'show stats');
  conRegVar('r_showpids', @gShowPIDs, 'show PIDs', 'show PIDs');
end.
