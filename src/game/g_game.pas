(* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
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
  g_basic, g_player, e_graphics, g_res_downloader,
  g_sound, g_gui, utils, md5, mempool, xprofiler,
  g_touch;

type
  TGameSettings = record
    GameType: Byte;
    GameMode: Byte;
    TimeLimit: Word;
    GoalLimit: Word;
    WarmupTime: Word;
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
procedure g_Game_Draw();
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
procedure g_Game_StartCustom(Map: String; GameMode: Byte; TimeLimit, GoalLimit: Word; MaxLives: Byte; Options: LongWord; nPlayers: Byte);
procedure g_Game_StartServer(Map: String; GameMode: Byte; TimeLimit, GoalLimit: Word; MaxLives: Byte; Options: LongWord; nPlayers: Byte; IPAddr: LongWord; Port: Word);
procedure g_Game_StartClient(Addr: String; Port: Word; PW: String);
procedure g_Game_Restart();
procedure g_Game_RestartLevel();
procedure g_Game_RestartRound(NoMapRestart: Boolean = False);
procedure g_Game_ClientWAD(NewWAD: String; WHash: TMD5Digest);
procedure g_Game_SaveOptions();
function  g_Game_StartMap(Map: String; Force: Boolean = False; const oldMapPath: AnsiString=''): Boolean;
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
procedure g_Game_StartVote(Command, Initiator: string);
procedure g_Game_CheckVote;
procedure g_TakeScreenShot();
procedure g_FatalError(Text: String);
procedure g_SimpleError(Text: String);
function  g_Game_IsTestMap(): Boolean;
procedure g_Game_DeleteTestMap();
procedure GameCVars(P: SSArray);
procedure GameCommands(P: SSArray);
procedure GameCheats(P: SSArray);
procedure DebugCommands(P: SSArray);
procedure g_Game_Process_Params;
procedure g_Game_SetLoadingText(Text: String; Max: Integer; reWrite: Boolean);
procedure g_Game_StepLoading(Value: Integer = -1);
procedure g_Game_ClearLoading();
procedure g_Game_SetDebugMode();
procedure DrawLoadingStat();

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

  GAME_OPTION_RESERVED     = 1;
  GAME_OPTION_TEAMDAMAGE   = 2;
  GAME_OPTION_ALLOWEXIT    = 4;
  GAME_OPTION_WEAPONSTAY   = 8;
  GAME_OPTION_MONSTERS     = 16;
  GAME_OPTION_BOTVSPLAYER  = 32;
  GAME_OPTION_BOTVSMONSTER = 64;

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

  ANNOUNCE_NONE   = 0;
  ANNOUNCE_ME     = 1;
  ANNOUNCE_MEPLUS = 2;
  ANNOUNCE_ALL    = 3;

  CONFIG_FILENAME = 'Doom2DF.cfg';
  LOG_FILENAME = 'Doom2DF.log';

  TEST_MAP_NAME = '$$$_TEST_$$$';

  STD_PLAYER_MODEL = 'Doomer';

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
  gSwitchGameMode: Byte = GM_DM;
  gHearPoint1, gHearPoint2: THearPoint;
  gSoundEffectsDF: Boolean = False;
  gSoundTriggerTime: Word = 0;
  gAnnouncer: Byte = ANNOUNCE_NONE;
  goodsnd: array[0..3] of TPlayableSound;
  killsnd: array[0..3] of TPlayableSound;
  gDefInterTime: ShortInt = -1;
  gInterEndTime: LongWord = 0;
  gInterTime: LongWord = 0;
  gServInterTime: Byte = 0;
  gGameStartTime: LongWord = 0;
  gTotalMonsters: Integer = 0;
  gPauseMain: Boolean = false;
  gPauseHolmes: Boolean = false;
  gShowTime: Boolean = True;
  gShowFPS: Boolean = False;
  gShowGoals: Boolean = True;
  gShowStat: Boolean = True;
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
  gMusic: TMusic = nil;
  gLoadGameMode: Boolean;
  gCheats: Boolean = False;
  gMapOnce: Boolean = False;
  gMapToDelete: String;
  gTempDelete: Boolean = False;
  gLastMap: Boolean = False;
  gWinPosX, gWinPosY: Integer;
  gWinSizeX, gWinSizeY: Integer;
  gWinFrameX, gWinFrameY, gWinCaption: Integer;
  gWinActive: Boolean = True; // by default window is active, lol
  gResolutionChange: Boolean = False;
  gRC_Width, gRC_Height: Word;
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


implementation

uses
{$IFDEF USE_NANOGL}
  nanoGL,
{$ELSE}
  GL, GLExt,
{$ENDIF}
  e_texture, g_textures, g_main, g_window, g_menu,
  e_input, e_log, g_console, g_items, g_map, g_panel,
  g_playermodel, g_gfx, g_options, g_weapons, Math,
  g_triggers, g_monsters, e_sound, CONFIG,
  g_language, g_net,
  ENet, e_msg, g_netmsg, g_netmaster,
  sfs, wadreader, g_holmes;


var
  hasPBarGfx: Boolean = false;


// ////////////////////////////////////////////////////////////////////////// //
function gPause (): Boolean; inline; begin result := gPauseMain or gPauseHolmes; end;


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
var
  profileFrameDraw: TProfiler = nil;


// ////////////////////////////////////////////////////////////////////////// //
type
  TDynLight = record
    x, y, radius: Integer;
    r, g, b, a: Single;
    exploCount: Integer;
    exploRadius: Integer;
  end;

var
  g_dynLights: array of TDynLight = nil;
  g_dynLightCount: Integer = 0;
  g_playerLight: Boolean = false;

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


// ////////////////////////////////////////////////////////////////////////// //
type
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
  FPS, UPS: Word;
  FPSCounter, UPSCounter: Word;
  FPSTime, UPSTime: LongWord;
  DataLoaded: Boolean = False;
  LastScreenShot: Int64;
  IsDrawStat: Boolean = False;
  CustomStat: TEndCustomGameStat;
  SingleStat: TEndSingleGameStat;
  LoadingStat: TLoadingStat;
  EndingGameCounter: Byte = 0;
  MessageText: String;
  MessageTime: Word;
  MessageLineLength: Integer = 80;
  MapList: SSArray = nil;
  MapIndex: Integer = -1;
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
  s: string;
begin
  g_Game_FreeWAD();
  gGameSettings.WAD := WAD;
  if not (gGameSettings.GameMode in [GM_COOP, GM_SINGLE]) then
    Exit;

  MegaWAD.info := g_Game_GetMegaWADInfo(MapsDir + WAD);

  w := TWADFile.Create();
  w.ReadFile(MapsDir + WAD);

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
    s := g_ExtractWadName(MegaWAD.endpic);
    if s = '' then s := MapsDir+WAD else s := GameDir+'/wads/';
    g_Texture_CreateWADEx('TEXTURE_endpic', s+MegaWAD.endpic);
  end;
  MegaWAD.endmus := cfg.ReadStr('megawad', 'endmus', 'Standart.wad:D2DMUS\КОНЕЦ');
  if MegaWAD.endmus <> '' then
  begin
    s := g_ExtractWadName(MegaWAD.endmus);
    if s = '' then s := MapsDir+WAD else s := GameDir+'/wads/';
    g_Sound_CreateWADEx('MUSIC_endmus', s+MegaWAD.endmus, True);
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
    gDelayedEvents[n].Time := (GetTimer() {div 1000}) + Time
  else
    gDelayedEvents[n].Time := gTime + Time;
  Result := n;
end;

procedure EndGame();
var
  a: Integer;
  FileName: string;
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
        end;

        g_Game_ExecuteEvent('onmapend');

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

procedure drawTime(X, Y: Integer); inline;
begin
  e_TextureFontPrint(x, y,
                     Format('%d:%.2d:%.2d', [
                       gTime div 1000 div 3600,
                       (gTime div 1000 div 60) mod 60,
                       gTime div 1000 mod 60
                     ]),
                     gStdFont);
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
      s2 := Format(_lc[I_GAME_FRAG_LIMIT], [gGameSettings.GoalLimit]);
      s3 := Format(_lc[I_GAME_TIME_LIMIT], [gGameSettings.TimeLimit div 3600, (gGameSettings.TimeLimit div 60) mod 60, gGameSettings.TimeLimit mod 60]);
    end;

    GM_TDM:
    begin
      if gGameSettings.MaxLives = 0 then
        s1 := _lc[I_GAME_TDM]
      else
        s1 := _lc[I_GAME_TLMS];
      s2 := Format(_lc[I_GAME_FRAG_LIMIT], [gGameSettings.GoalLimit]);
      s3 := Format(_lc[I_GAME_TIME_LIMIT], [gGameSettings.TimeLimit div 3600, (gGameSettings.TimeLimit div 60) mod 60, gGameSettings.TimeLimit mod 60]);
    end;

    GM_CTF:
    begin
      s1 := _lc[I_GAME_CTF];
      s2 := Format(_lc[I_GAME_SCORE_LIMIT], [gGameSettings.GoalLimit]);
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
      e_TextureFontPrintEx(x+w1+16, _y, IntToStr(gTeamStat[a].Goals),
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
            // Имя
            e_TextureFontPrintEx(x+16, _y, Name, gStdFont, rr, gg, bb, 1);
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
        // Цвет игрока
        e_DrawFillQuad(x+16, _y+4, x+32-1, _y+16+4-1, Color.R, Color.G, Color.B, 0);
        e_DrawQuad(x+16, _y+4, x+32-1, _y+16+4-1, 192, 192, 192);
        // Имя
        e_TextureFontPrintEx(x+16+16+8, _y+4, Name, gStdFont, r, g, 0, 1);
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

procedure g_Game_Init();
var
  SR: TSearchRec;
begin
  gExit := 0;
  gMapToDelete := '';
  gTempDelete := False;

  sfsGCDisable(); // temporary disable removing of temporary volumes

  try
    g_Texture_CreateWADEx('MENU_BACKGROUND', GameWAD+':TEXTURES\TITLE');
    g_Texture_CreateWADEx('INTER', GameWAD+':TEXTURES\INTER');
    g_Texture_CreateWADEx('ENDGAME_EN', GameWAD+':TEXTURES\ENDGAME_EN');
    g_Texture_CreateWADEx('ENDGAME_RU', GameWAD+':TEXTURES\ENDGAME_RU');

    LoadStdFont('STDTXT', 'STDFONT', gStdFont);
    LoadFont('MENUTXT', 'MENUFONT', gMenuFont);
    LoadFont('SMALLTXT', 'SMALLFONT', gMenuSmallFont);

    g_Game_ClearLoading();
    g_Game_SetLoadingText(Format('Doom 2D: Forever %s', [GAME_VERSION]), 0, False);
    g_Game_SetLoadingText('', 0, False);

    g_Game_SetLoadingText(_lc[I_LOAD_CONSOLE], 0, False);
    g_Console_Init();

    g_Game_SetLoadingText(_lc[I_LOAD_MODELS], 0, False);
    g_PlayerModel_LoadData();

    if FindFirst(ModelsDir+'*.wad', faAnyFile, SR) = 0 then
      repeat
        if not g_PlayerModel_Load(ModelsDir+SR.Name) then
          e_WriteLog(Format('Error loading model %s', [SR.Name]), TMsgType.Warning);
      until FindNext(SR) <> 0;
    FindClose(SR);

    if FindFirst(ModelsDir+'*.pk3', faAnyFile, SR) = 0 then
      repeat
        if not g_PlayerModel_Load(ModelsDir+SR.Name) then
          e_WriteLog(Format('Error loading model %s', [SR.Name]), TMsgType.Warning);
      until FindNext(SR) <> 0;
    FindClose(SR);

    if FindFirst(ModelsDir+'*.zip', faAnyFile, SR) = 0 then
      repeat
        if not g_PlayerModel_Load(ModelsDir+SR.Name) then
          e_WriteLog(Format('Error loading model %s', [SR.Name]), TMsgType.Warning);
      until FindNext(SR) <> 0;
    FindClose(SR);

    gGameOn := false;
    gPauseMain := false;
    gPauseHolmes := false;
    gTime := 0;
    LastScreenShot := 0;

    {e_MouseInfo.Accel := 1.0;}

    g_Game_SetLoadingText(_lc[I_LOAD_GAME_DATA], 0, False);
    g_Game_LoadData();

    g_Game_SetLoadingText(_lc[I_LOAD_MUSIC], 0, False);
    g_Sound_CreateWADEx('MUSIC_INTERMUS', GameWAD+':MUSIC\INTERMUS', True);
    g_Sound_CreateWADEx('MUSIC_MENU', GameWAD+':MUSIC\MENU', True);
    g_Sound_CreateWADEx('MUSIC_ROUNDMUS', GameWAD+':MUSIC\ROUNDMUS', True);
    g_Sound_CreateWADEx('MUSIC_STDENDMUS', GameWAD+':MUSIC\ENDMUS', True);

    g_Game_SetLoadingText(_lc[I_LOAD_MENUS], 0, False);
    g_Menu_Init();

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

function isKeyPressed (key1: Word; key2: Word): Boolean;
begin
  if (key1 <> 0) and e_KeyPressed(key1) then begin result := true; exit; end;
  if (key2 <> 0) and e_KeyPressed(key2) then begin result := true; exit; end;
  result := false;
end;

procedure processPlayerControls (plr: TPlayer; var ctrl: TPlayerControl; var MoveButton: Byte; p2hack: Boolean=false);
var
  time: Word;
  strafeDir: Byte;
  i: Integer;
begin
  if (plr = nil) then exit;
  if (p2hack) then time := 1000 else time := 1;
  strafeDir := MoveButton shr 4;
  MoveButton := MoveButton and $0F;
  with ctrl do
  begin
         if isKeyPressed(KeyLeft, KeyLeft2) and (not isKeyPressed(KeyRight, KeyRight2)) then MoveButton := 1 // Нажата только "Влево"
    else if (not isKeyPressed(KeyLeft, KeyLeft2)) and isKeyPressed(KeyRight, KeyRight2) then MoveButton := 2 // Нажата только "Вправо"
    else if (not isKeyPressed(KeyLeft, KeyLeft2)) and (not isKeyPressed(KeyRight, KeyRight2)) then MoveButton := 0; // Не нажаты ни "Влево", ни "Вправо"

    // Сейчас или раньше были нажаты "Влево"/"Вправо" => передаем игроку:
         if MoveButton = 1 then plr.PressKey(KEY_LEFT, time)
    else if MoveButton = 2 then plr.PressKey(KEY_RIGHT, time);

    // if we have "strafe" key, turn off old strafe mechanics
    if isKeyPressed(KeyStrafe, KeyStrafe2) then
    begin
      // new strafe mechanics
      if (strafeDir = 0) then strafeDir := MoveButton; // start strafing
      // now set direction according to strafe (reversed)
           if (strafeDir = 2) then plr.SetDirection(TDirection.D_LEFT)
      else if (strafeDir = 1) then plr.SetDirection(TDirection.D_RIGHT);
    end
    else
    begin
      strafeDir := 0; // not strafing anymore
      // Раньше была нажата "Вправо", а сейчас "Влево" => бежим вправо, смотрим влево:
           if (MoveButton = 2) and isKeyPressed(KeyLeft, KeyLeft2) then plr.SetDirection(TDirection.D_LEFT)
      // Раньше была нажата "Влево", а сейчас "Вправо" => бежим влево, смотрим вправо:
      else if (MoveButton = 1) and isKeyPressed(KeyRight, KeyRight2) then plr.SetDirection(TDirection.D_RIGHT)
      // Что-то было нажато и не изменилось => куда бежим, туда и смотрим:
      else if MoveButton <> 0 then plr.SetDirection(TDirection(MoveButton-1));
    end;

    // fix movebutton state
    MoveButton := MoveButton or (strafeDir shl 4);

    // Остальные клавиши:
    if isKeyPressed(KeyJump, KeyJump2) then plr.PressKey(KEY_JUMP, time);
    if isKeyPressed(KeyUp, KeyUp2) then plr.PressKey(KEY_UP, time);
    if isKeyPressed(KeyDown, KeyDown2) then plr.PressKey(KEY_DOWN, time);
    if isKeyPressed(KeyFire, KeyFire2) then plr.PressKey(KEY_FIRE);
    if isKeyPressed(KeyNextWeapon, KeyNextWeapon2) then plr.PressKey(KEY_NEXTWEAPON);
    if isKeyPressed(KeyPrevWeapon, KeyPrevWeapon2) then plr.PressKey(KEY_PREVWEAPON);
    if isKeyPressed(KeyOpen, KeyOpen2) then plr.PressKey(KEY_OPEN);

    for i := 0 to High(KeyWeapon) do
      if isKeyPressed(KeyWeapon[i], KeyWeapon2[i]) then
        plr.QueueWeaponSwitch(i); // all choices are passed there, and god will take the best
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
  g_Console_Update();

  if (NetMode = NET_NONE) and (g_Game_IsNet) and (gGameOn or (gState in [STATE_FOLD, STATE_INTERCUSTOM])) then
  begin
    gExit := EXIT_SIMPLE;
    EndGame();
    Exit;
  end;

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
              e_KeyPressed(VK_FIRE) or e_KeyPressed(VK_OPEN)
            )
            and (not gJustChatted) and (not gConsoleShow) and (not gChatShow)
            and (g_ActiveWindow = nil)
          )
          or (g_Game_IsNet and (gInterTime > gInterEndTime))
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
                if gLastMap and (gGameSettings.GameMode = GM_COOP) then
                begin
                  g_Game_ExecuteEvent('onwadend');
                  if not gMusic.SetByName('MUSIC_endmus') then
                    gMusic.SetByName('MUSIC_STDENDMUS');
                end
                else
                  gMusic.SetByName('MUSIC_ROUNDMUS');

                gMusic.Play();
                gState := STATE_INTERCUSTOM;
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
        g_Serverlist_Control(slCurrent);
  end;

  if g_Game_IsNet then
    if not gConsoleShow then
      if not gChatShow then
      begin
        if g_ActiveWindow = nil then
        begin
          if e_KeyPressed(gGameControls.GameControls.Chat) or e_KeyPressed(VK_CHAT) then
            g_Console_Chat_Switch(False)
          else if (e_KeyPressed(gGameControls.GameControls.TeamChat) or e_KeyPressed(VK_TEAM)) and
                  (gGameSettings.GameMode in [GM_TDM, GM_CTF]) then
            g_Console_Chat_Switch(True);
        end;
      end else
        if not gChatEnter then
          if (not e_KeyPressed(gGameControls.GameControls.Chat))
             and (not e_KeyPressed(gGameControls.GameControls.TeamChat))
             and (not e_KeyPressed(VK_CHAT))
             and (not e_KeyPressed(VK_TEAM)) then
            gChatEnter := True;

// Статистика по Tab:
  if gGameOn then
    IsDrawStat := (not gConsoleShow) and (not gChatShow) and
                  (gGameSettings.GameType <> GT_SINGLE) and
                  (e_KeyPressed(gGameControls.GameControls.Stat) or e_KeyPressed(VK_STATUS));

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
      if (gGameSettings.GoalLimit > 0) then
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
            b := Max(gTeamStat[TEAM_RED].Goals, gTeamStat[TEAM_BLUE].Goals);
          end;

      // Лимит побед набран => конец уровня:
        if b >= gGameSettings.GoalLimit then
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
        processPlayerControls(gPlayer1, gGameControls.P1Control, P1MoveButton);
        processPlayerControls(gPlayer2, gGameControls.P2Control, P2MoveButton, true);
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
        if isKeyPressed(gGameControls.P1Control.KeyJump, gGameControls.P1Control.KeyJump2) then
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
        if gSpectMode = SPECT_MAPVIEW then
        begin
          if isKeyPressed(gGameControls.P1Control.KeyLeft, gGameControls.P1Control.KeyLeft2) then
            gSpectX := Max(gSpectX - gSpectStep, 0);
          if isKeyPressed(gGameControls.P1Control.KeyRight, gGameControls.P1Control.KeyRight2) then
            gSpectX := Min(gSpectX + gSpectStep, gMapInfo.Width - gScreenWidth);
          if isKeyPressed(gGameControls.P1Control.KeyUp, gGameControls.P1Control.KeyUp2) then
            gSpectY := Max(gSpectY - gSpectStep, 0);
          if isKeyPressed(gGameControls.P1Control.KeyDown, gGameControls.P1Control.KeyDown2) then
            gSpectY := Min(gSpectY + gSpectStep, gMapInfo.Height - gScreenHeight);
          if isKeyPressed(gGameControls.P1Control.KeyPrevWeapon, gGameControls.P1Control.KeyPrevWeapon2) then
          begin
            // decrease step
            if gSpectStep > 4 then gSpectStep := gSpectStep shr 1;
            gSpectKeyPress := True;
          end;
          if isKeyPressed(gGameControls.P1Control.KeyNextWeapon, gGameControls.P1Control.KeyNextWeapon2) then
          begin
            // increase step
            if gSpectStep < 64 then gSpectStep := gSpectStep shl 1;
            gSpectKeyPress := True;
          end;
        end;
        if gSpectMode = SPECT_PLAYERS then
        begin
          if isKeyPressed(gGameControls.P1Control.KeyUp, gGameControls.P1Control.KeyUp2) then
          begin
            // add second view
            gSpectViewTwo := True;
            gSpectKeyPress := True;
          end;
          if isKeyPressed(gGameControls.P1Control.KeyDown, gGameControls.P1Control.KeyDown2) then
          begin
            // remove second view
            gSpectViewTwo := False;
            gSpectKeyPress := True;
          end;
          if isKeyPressed(gGameControls.P1Control.KeyLeft, gGameControls.P1Control.KeyLeft2) then
          begin
            // prev player (view 1)
            gSpectPID1 := GetActivePlayerID_Prev(gSpectPID1);
            gSpectKeyPress := True;
          end;
          if isKeyPressed(gGameControls.P1Control.KeyRight, gGameControls.P1Control.KeyRight2) then
          begin
            // next player (view 1)
            gSpectPID1 := GetActivePlayerID_Next(gSpectPID1);
            gSpectKeyPress := True;
          end;
          if isKeyPressed(gGameControls.P1Control.KeyPrevWeapon, gGameControls.P1Control.KeyPrevWeapon2) then
          begin
            // prev player (view 2)
            gSpectPID2 := GetActivePlayerID_Prev(gSpectPID2);
            gSpectKeyPress := True;
          end;
          if isKeyPressed(gGameControls.P1Control.KeyNextWeapon, gGameControls.P1Control.KeyNextWeapon2) then
          begin
            // next player (view 2)
            gSpectPID2 := GetActivePlayerID_Next(gSpectPID2);
            gSpectKeyPress := True;
          end;
        end;
      end
      else
        if (not isKeyPressed(gGameControls.P1Control.KeyJump, gGameControls.P1Control.KeyJump2)) and
           (not isKeyPressed(gGameControls.P1Control.KeyLeft, gGameControls.P1Control.KeyLeft2)) and
           (not isKeyPressed(gGameControls.P1Control.KeyRight, gGameControls.P1Control.KeyRight2)) and
           (not isKeyPressed(gGameControls.P1Control.KeyUp, gGameControls.P1Control.KeyUp2)) and
           (not isKeyPressed(gGameControls.P1Control.KeyDown, gGameControls.P1Control.KeyDown2)) and
           (not isKeyPressed(gGameControls.P1Control.KeyPrevWeapon, gGameControls.P1Control.KeyPrevWeapon2)) and
           (not isKeyPressed(gGameControls.P1Control.KeyNextWeapon, gGameControls.P1Control.KeyNextWeapon2)) then
          gSpectKeyPress := False;
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

      if NetUseMaster then
      begin
        if gTime >= NetTimeToMaster then
        begin
          if (NetMHost = nil) or (NetMPeer = nil) then
          begin
            if not g_Net_Slist_Connect then g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_SLIST_ERROR]);
          end;

          g_Net_Slist_Update;
          NetTimeToMaster := gTime + NetMasterRate;
        end;
      end;
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
      g_Menu_Reset();
      gLanguageChange := False;
    end;
  end;

// Делаем скриншот (не чаще 200 миллисекунд):
  if e_KeyPressed(gGameControls.GameControls.TakeScreenshot) or e_KeyPressed(VK_PRINTSCR) then
    if (GetTimer()-LastScreenShot) > 200000 div 1000 then
    begin
      g_TakeScreenShot();
      LastScreenShot := GetTimer();
    end;

// Горячая клавиша для вызова меню выхода из игры (F10):
  if e_KeyPressed(IK_F10) and
     gGameOn and
     (not gConsoleShow) and
     (g_ActiveWindow = nil) then
  begin
    KeyPress(IK_F10);
  end;

  Time := GetTimer() {div 1000};

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
  g_Frames_CreateWAD(nil, 'FRAMES_PUNCH', GameWAD+':TEXTURES\PUNCH', 64, 64, 4, False);
  g_Sound_CreateWADEx('SOUND_GAME_TELEPORT', GameWAD+':SOUNDS\TELEPORT');
  g_Sound_CreateWADEx('SOUND_GAME_NOTELEPORT', GameWAD+':SOUNDS\NOTELEPORT');
  g_Sound_CreateWADEx('SOUND_GAME_DOOROPEN', GameWAD+':SOUNDS\DOOROPEN');
  g_Sound_CreateWADEx('SOUND_GAME_DOORCLOSE', GameWAD+':SOUNDS\DOORCLOSE');
  g_Sound_CreateWADEx('SOUND_GAME_BULK1', GameWAD+':SOUNDS\BULK1');
  g_Sound_CreateWADEx('SOUND_GAME_BULK2', GameWAD+':SOUNDS\BULK2');
  g_Sound_CreateWADEx('SOUND_GAME_BUBBLE1', GameWAD+':SOUNDS\BUBBLE1');
  g_Sound_CreateWADEx('SOUND_GAME_BUBBLE2', GameWAD+':SOUNDS\BUBBLE2');
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

  g_Game_LoadChatSounds(GameWAD+':CHATSND\SNDCFG');

  g_Game_SetLoadingText(_lc[I_LOAD_ITEMS_DATA], 0, False);
  g_Items_LoadData();

  g_Game_SetLoadingText(_lc[I_LOAD_WEAPONS_DATA], 0, False);
  g_Weapon_LoadData();

  g_Monsters_LoadData();

  DataLoaded := True;
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
  g_Sound_Delete('SOUND_GAME_TELEPORT');
  g_Sound_Delete('SOUND_GAME_NOTELEPORT');
  g_Sound_Delete('SOUND_GAME_DOOROPEN');
  g_Sound_Delete('SOUND_GAME_DOORCLOSE');
  g_Sound_Delete('SOUND_GAME_BULK1');
  g_Sound_Delete('SOUND_GAME_BULK2');
  g_Sound_Delete('SOUND_GAME_BUBBLE1');
  g_Sound_Delete('SOUND_GAME_BUBBLE2');
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

  g_Game_FreeChatSounds();

  DataLoaded := False;
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

  g_ProcessMessages();

  if e_KeyPressed(IK_TAB) or e_KeyPressed(VK_STATUS) then
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
      if TeamStat[TEAM_RED].Goals > TeamStat[TEAM_BLUE].Goals then s1 := _lc[I_GAME_WIN_RED]
        else if TeamStat[TEAM_BLUE].Goals > TeamStat[TEAM_RED].Goals then s1 := _lc[I_GAME_WIN_BLUE]
          else s1 := _lc[I_GAME_WIN_DRAW];

    e_TextureFontPrintEx(x+8+(w div 2)-(Length(s1)*ww2 div 2), _y, s1, gStdFont, 255, 255, 255, 1);
    _y := _y+40;

    for t := TEAM_RED to TEAM_BLUE do
    begin
      if t = TEAM_RED then
      begin
        e_TextureFontPrintEx(x+8, _y, _lc[I_GAME_TEAM_RED],
                             gStdFont, 255, 0, 0, 1);
        e_TextureFontPrintEx(x+w1+8, _y, IntToStr(CustomStat.TeamStat[TEAM_RED].Goals),
                             gStdFont, 255, 0, 0, 1);
        r := 255;
        g := 0;
        b := 0;
      end
      else
      begin
        e_TextureFontPrintEx(x+8, _y, _lc[I_GAME_TEAM_BLUE],
                             gStdFont, 0, 0, 255, 1);
        e_TextureFontPrintEx(x+w1+8, _y, IntToStr(CustomStat.TeamStat[TEAM_BLUE].Goals),
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
            e_TextureFontPrintEx(x+8, _y, Name, gStdFont, rr, gg, bb, 1);
            e_TextureFontPrintEx(x+w1+8, _y, IntToStr(Frags), gStdFont, rr, gg, bb, 1);
            e_TextureFontPrintEx(x+w1+w2+8, _y, IntToStr(Deaths), gStdFont, rr, gg, bb, 1);
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

        e_TextureFontPrintEx(x+8+16+8, _y+4, Name, gStdFont, r, r, r, 1, True);
        e_TextureFontPrintEx(x+w1+8+16+8, _y+4, IntToStr(Frags), gStdFont, r, r, r, 1, True);
        e_TextureFontPrintEx(x+w1+w2+8+16+8, _y+4, IntToStr(Deaths), gStdFont, r, r, r, 1, True);
        _y := _y+24;
      end;
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

procedure DrawLoadingStat();
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

procedure DrawMinimap(p: TPlayer; RenderRect: e_graphics.TRect);
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
              0: e_DrawFillQuad(aX, aY, aX2, aY2, 116,  72,  36, 0);
              1: e_DrawFillQuad(aX, aY, aX2, aY2, 116, 124,  96, 0);
              2: e_DrawFillQuad(aX, aY, aX2, aY2, 200,  80,   4, 0);
              3: e_DrawFillQuad(aX, aY, aX2, aY2, 252, 140,  56, 0);
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
      glScissor(0, 0, gWinSizeX, gWinSizeY);
    end;
    // no need to clear stencil buffer, light blitting will do it for us... but only for normal scale
    if (g_dbg_scale <> 1.0) then glClear(GL_STENCIL_BUFFER_BIT);
    glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);
    // draw extruded panels
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
    glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE); // no need to modify color buffer
    if (lrad > 4) then g_Map_DrawPanelShadowVolumes(lx, ly, lrad);
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
        if doDraw then pan.Draw(doAmbient, ambColor);
        gDrawPanelList.popFront();
      end;
    end
    else
    begin
      if doDraw then g_Map_DrawPanels(panType, hasAmbient, ambColor);
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
    g_Map_CollectDrawPanels(sX, sY, sWidth, sHeight);
  end;
  if (profileFrameDraw <> nil) then profileFrameDraw.sectionEnd();

  if (profileFrameDraw <> nil) then profileFrameDraw.sectionBegin('skyback');
  g_Map_DrawBack(backXOfs, backYOfs);
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
  drawOther('items', @g_Items_Draw);
  drawOther('weapons', @g_Weapon_Draw);
  drawOther('shells', @g_Player_DrawShells);
  drawOther('drawall', @g_Player_DrawAll);
  drawOther('corpses', @g_Player_DrawCorpses);
  drawPanelType('*wall', PANEL_WALL, g_rlayer_wall);
  drawOther('monsters', @g_Monsters_Draw);
  drawOther('itemdrop', @g_Items_DrawDrop);
  drawPanelType('*door', PANEL_CLOSEDOOR, g_rlayer_door);
  drawOther('gfx', @g_GFX_Draw);
  drawOther('flags', @g_Map_DrawFlags);
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
    g_Monsters_DrawHealth();
    g_Player_DrawHealth();
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
  px, py, a, b, c, d: Integer;
  //R: TRect;
begin
  if (p = nil) or (p.FDummy) then
  begin
    glPushMatrix();
    g_Map_DrawBack(0, 0);
    glPopMatrix();
    Exit;
  end;

  if (profileFrameDraw = nil) then profileFrameDraw := TProfiler.Create('RENDER', g_profile_history_size);
  if (profileFrameDraw <> nil) then profileFrameDraw.mainBegin(g_profile_frame_draw);

  gPlayerDrawn := p;

  glPushMatrix();

  px := p.GameX + PLAYER_RECT_CX;
  py := p.GameY + PLAYER_RECT_CY+p.Obj.slopeUpLeft;

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

  if p.IncCam <> 0 then
  begin
    if py > gMapInfo.Height-(gPlayerScreenSize.Y div 2) then
    begin
      if p.IncCam > 120-(py-(gMapInfo.Height-(gPlayerScreenSize.Y div 2))) then
      begin
        p.IncCam := 120-(py-(gMapInfo.Height-(gPlayerScreenSize.Y div 2)));
      end;
    end;

    if py < gPlayerScreenSize.Y div 2 then
    begin
      if p.IncCam < -120+((gPlayerScreenSize.Y div 2)-py) then
      begin
        p.IncCam := -120+((gPlayerScreenSize.Y div 2)-py);
      end;
    end;

    if p.IncCam < 0 then
    begin
      while (py+(gPlayerScreenSize.Y div 2)-p.IncCam > gMapInfo.Height) and (p.IncCam < 0) do p.IncCam := p.IncCam+1; //Inc(p.IncCam);
    end;

    if p.IncCam > 0 then
    begin
      while (py-(gPlayerScreenSize.Y div 2)-p.IncCam < 0) and (p.IncCam > 0) do p.IncCam := p.IncCam-1; //Dec(p.IncCam);
    end;
  end;

       if (px < gPlayerScreenSize.X div 2) or (gMapInfo.Width-gPlayerScreenSize.X <= 256) then c := 0
  else if (px > gMapInfo.Width-(gPlayerScreenSize.X div 2)) then c := gBackSize.X-gPlayerScreenSize.X
  else c := round((px-(gPlayerScreenSize.X div 2))/(gMapInfo.Width-gPlayerScreenSize.X)*(gBackSize.X-gPlayerScreenSize.X));

       if (py-p.IncCam <= gPlayerScreenSize.Y div 2) or (gMapInfo.Height-gPlayerScreenSize.Y <= 256) then d := 0
  else if (py-p.IncCam >= gMapInfo.Height-(gPlayerScreenSize.Y div 2)) then d := gBackSize.Y-gPlayerScreenSize.Y
  else d := round((py-p.IncCam-(gPlayerScreenSize.Y div 2))/(gMapInfo.Height-gPlayerScreenSize.Y)*(gBackSize.Y-gPlayerScreenSize.Y));

  sX := -a;
  sY := -(b+p.IncCam);
  sWidth := gPlayerScreenSize.X;
  sHeight := gPlayerScreenSize.Y;

  //glTranslatef(a, b+p.IncCam, 0);

  //if (p = gPlayer1) and (g_dbg_scale >= 1.0) then g_Holmes_plrViewSize(sWidth, sHeight);

  //conwritefln('OLD: (%s,%s)-(%s,%s)', [sX, sY, sWidth, sHeight]);
  fixViewportForScale();
  //conwritefln('     (%s,%s)-(%s,%s)', [sX, sY, sWidth, sHeight]);

  if (g_dbg_scale <> 1.0) and (not g_dbg_ignore_bounds) then
  begin
    if (sX+sWidth > gMapInfo.Width) then sX := gMapInfo.Width-sWidth;
    if (sY+sHeight > gMapInfo.Height) then sY := gMapInfo.Height-sHeight;
    if (sX < 0) then sX := 0;
    if (sY < 0) then sY := 0;

    if (gBackSize.X <= gPlayerScreenSize.X) or (gMapInfo.Width <= sWidth) then c := 0 else c := trunc((gBackSize.X-gPlayerScreenSize.X)*sX/(gMapInfo.Width-sWidth));
    if (gBackSize.Y <= gPlayerScreenSize.Y) or (gMapInfo.Height <= sHeight) then d := 0 else d := trunc((gBackSize.Y-gPlayerScreenSize.Y)*sY/(gMapInfo.Height-sHeight));
  end;

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

  if (p = gPlayer1) then
  begin
    g_Holmes_plrViewPos(sX, sY);
    g_Holmes_plrViewSize(sWidth, sHeight);
  end;

  renderMapInternal(-c, -d, true);

  if p.FSpectator then
    e_TextureFontPrintEx(p.GameX + PLAYER_RECT_CX - 4,
                         p.GameY + PLAYER_RECT_CY - 4,
                         'X', gStdFont, 255, 255, 255, 1, True);
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

  p.DrawPain();
  p.DrawPickup();
  p.DrawRulez();
  if gShowMap then DrawMinimap(p, _TRect(0, 0, 128, 128));
  if g_Debug_Player then
    g_Player_DrawDebug(p);
  p.DrawGUI();
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

procedure g_Game_Draw();
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

  Time := GetTimer() {div 1000};
  FPSCounter := FPSCounter+1;
  if Time - FPSTime >= 1000 then
  begin
    FPS := FPSCounter;
    FPSCounter := 0;
    FPSTime := Time;
  end;

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
        gHearPoint1.Coords.X := plView1.GameX;
        gHearPoint1.Coords.Y := plView1.GameY;
      end else
        gHearPoint1.Active := False;
      if plView2 <> nil then
      begin
        gHearPoint2.Active := True;
        gHearPoint2.Coords.X := plView2.GameX;
        gHearPoint2.Coords.Y := plView2.GameY;
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

    // draw inspector
    if (g_holmes_enabled) then g_Holmes_Draw();

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

    if IsDrawStat or (gSpectMode = 1) then DrawStat();

    if gSpectHUD and (not gChatShow) and (gSpectMode <> SPECT_NONE) then
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
      if (g_ActiveWindow = nil) or (g_ActiveWindow.BackTexture = '') then
      begin
        if g_Texture_Get('MENU_BACKGROUND', ID) then e_DrawSize(ID, 0, 0, 0, False, False, gScreenWidth, gScreenHeight)
        else e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);
      end;
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

      if g_Texture_Get(back, ID) then
        e_DrawSize(ID, 0, 0, 0, False, False, gScreenWidth, gScreenHeight)
      else
        e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);

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

        if g_Texture_Get(back, ID) then
          e_DrawSize(ID, 0, 0, 0, False, False, gScreenWidth, gScreenHeight)
        else
          e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);

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
      if not g_Texture_Get('TEXTURE_endpic', ID) then
        g_Texture_Get(_lc[I_TEXTURE_ENDPIC], ID);

      if ID <> DWORD(-1) then
        e_DrawSize(ID, 0, 0, 0, False, False, gScreenWidth, gScreenHeight)
      else
        e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);

      if g_ActiveWindow <> nil then
      begin
        //e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 48, 48, 48, 180);
        e_DarkenQuadWH(0, 0, gScreenWidth, gScreenHeight, 150);
      end;
    end;

    if gState = STATE_SLIST then
    begin
      if g_Texture_Get('MENU_BACKGROUND', ID) then
      begin
        e_DrawSize(ID, 0, 0, 0, False, False, gScreenWidth, gScreenHeight);
        //e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 48, 48, 48, 180);
        e_DarkenQuadWH(0, 0, gScreenWidth, gScreenHeight, 150);
      end;
      g_Serverlist_Draw(slCurrent);
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

  g_Console_Draw();

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

  if gGameOn and gShowTime and (gGameSettings.GameType in [GT_CUSTOM, GT_SERVER, GT_CLIENT]) then
    drawTime(gScreenWidth-72, gScreenHeight-16);

  if gGameOn then drawProfilers();

  g_Holmes_DrawUI();

  g_Touch_Draw;
end;

procedure g_Game_Quit();
begin
  g_Game_StopAllSounds(True);
  gMusic.Free();
  g_Game_SaveOptions();
  g_Game_FreeData();
  g_PlayerModel_FreeData();
  g_Texture_DeleteAll();
  g_Frames_DeleteAll();
  //g_Menu_Free(); //k8: this segfaults after resolution change; who cares?

  if NetInitDone then g_Net_Free;

// Надо удалить карту после теста:
  if gMapToDelete <> '' then
    g_Game_DeleteTestMap();

  gExit := EXIT_QUIT;
  PushExitEvent();
end;

procedure g_FatalError(Text: String);
begin
  g_Console_Add(Format(_lc[I_FATAL_ERROR], [Text]), True);
  e_WriteLog(Format(_lc[I_FATAL_ERROR], [Text]), TMsgType.Warning);

  gExit := EXIT_SIMPLE;
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
  g_Window_SetSize(newWidth, newHeight, nowFull);
end;

procedure g_Game_AddPlayer(Team: Byte = TEAM_NONE);
begin
  if ((not gGameOn) and (gState <> STATE_INTERCUSTOM))
  or (not (gGameSettings.GameType in [GT_CUSTOM, GT_SERVER, GT_CLIENT])) then
    Exit;
  if gPlayer1 = nil then
  begin
    if g_Game_IsClient then
    begin
      if NetPlrUID1 > -1 then
      begin
        MC_SEND_CheatRequest(NET_CHEAT_SPECTATE);
        gPlayer1 := g_Player_Get(NetPlrUID1);
      end;
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
      g_Console_Add(Format(_lc[I_PLAYER_JOIN], [gPlayer1.Name]), True);
      if g_Game_IsServer and g_Game_IsNet then
        MH_SEND_PlayerCreate(gPlayer1.UID);
      gPlayer1.Respawn(False, True);

      if g_Game_IsNet and NetUseMaster then
        g_Net_Slist_Update;
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
      g_Console_Add(Format(_lc[I_PLAYER_JOIN], [gPlayer2.Name]), True);
      if g_Game_IsServer and g_Game_IsNet then
        MH_SEND_PlayerCreate(gPlayer2.UID);
      gPlayer2.Respawn(False, True);

      if g_Game_IsNet and NetUseMaster then
        g_Net_Slist_Update;
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

      if g_Game_IsNet and NetUseMaster then
        g_Net_Slist_Update;
    end else
      gPlayer2 := nil;
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

      if g_Game_IsNet and NetUseMaster then
        g_Net_Slist_Update;
    end else
    begin
      gPlayer1 := nil;
      MC_SEND_CheatRequest(NET_CHEAT_SPECTATE);
    end;
    Exit;
  end;
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
  gSwitchGameMode := GM_SINGLE;

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
    Inc(nPl);
  end;

// Загрузка и запуск карты:
  if not g_Game_StartMap(MAP, True) then
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
                             TimeLimit, GoalLimit: Word;
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
  gGameSettings.GoalLimit := GoalLimit;
  gGameSettings.MaxLives := IfThen(GameMode = GM_CTF, 0, MaxLives);
  gGameSettings.Options := Options;

  gCoopTotalMonstersKilled := 0;
  gCoopTotalSecretsFound := 0;
  gCoopTotalMonsters := 0;
  gCoopTotalSecrets := 0;
  gAimLine := False;
  gShowMap := False;

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
    Inc(nPl);
  end;

// Загрузка и запуск карты:
  if not g_Game_StartMap(Map, True) then
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
                             TimeLimit, GoalLimit: Word; MaxLives: Byte;
                             Options: LongWord; nPlayers: Byte;
                             IPAddr: LongWord; Port: Word);
begin
  g_Game_Free();

  e_WriteLog('Starting net game (server)...', TMsgType.Notify);

  g_Game_ClearLoading();

// Настройки игры:
  gGameSettings.GameType := GT_SERVER;
  gGameSettings.GameMode := GameMode;
  gSwitchGameMode := GameMode;
  gGameSettings.TimeLimit := TimeLimit;
  gGameSettings.GoalLimit := GoalLimit;
  gGameSettings.MaxLives := IfThen(GameMode = GM_CTF, 0, MaxLives);
  gGameSettings.Options := Options;

  gCoopTotalMonstersKilled := 0;
  gCoopTotalSecretsFound := 0;
  gCoopTotalMonsters := 0;
  gCoopTotalSecrets := 0;
  gAimLine := False;
  gShowMap := False;

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
  end;

  g_Game_SetLoadingText(_lc[I_LOAD_HOST], 0, False);
  if NetForwardPorts then
    g_Game_SetLoadingText(_lc[I_LOAD_PORTS], 0, False);

// Стартуем сервер
  if not g_Net_Host(IPAddr, Port, NetMaxClients) then
  begin
    g_FatalError(_lc[I_NET_MSG] + _lc[I_NET_ERR_HOST]);
    Exit;
  end;

  g_Net_Slist_Set(NetSlistIP, NetSlistPort);

// Загрузка и запуск карты:
  if not g_Game_StartMap(Map, True) then
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
    while (enet_host_service(NetHost, @NetEvent, 0) > 0) do
    begin
      if (NetEvent.kind = ENET_EVENT_TYPE_RECEIVE) then
      begin
        Ptr := NetEvent.packet^.data;
        if not InMsg.Init(Ptr, NetEvent.packet^.dataLength, True) then
          continue;

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
          gGameSettings.GoalLimit := InMsg.ReadWord();
          gGameSettings.TimeLimit := InMsg.ReadWord();
          gGameSettings.MaxLives := InMsg.ReadByte();
          gGameSettings.Options := InMsg.ReadLongWord();
          T := InMsg.ReadLongWord();

          newResPath := g_Res_SearchSameWAD(MapsDir, WadName, gWADHash);
          if newResPath = '' then
          begin
            g_Game_SetLoadingText(_lc[I_LOAD_DL_RES], 0, False);
            newResPath := g_Res_DownloadWAD(WadName);
            if newResPath = '' then
            begin
              g_FatalError(_lc[I_NET_ERR_HASH]);
              enet_packet_destroy(NetEvent.packet);
              NetState := NET_STATE_NONE;
              Exit;
            end;
          end;
          newResPath := ExtractRelativePath(MapsDir, newResPath);

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
          gPlayer1.UID := NetPlrUID1;
          gPlayer1.Reset(True);

          if not g_Game_StartMap(newResPath + ':\' + Map, True) then
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

    ProcessLoading(true);

    if e_KeyPressed(IK_ESCAPE) or e_KeyPressed(IK_SPACE) or e_KeyPressed(VK_ESCAPE) then
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

  gLMSRespawn := LMS_RESPAWN_NONE;
  gLMSRespawnTime := 0;

  g_Player_Init();
  NetState := NET_STATE_GAME;
  MC_SEND_FullStateRequest;
  e_WriteLog('NET: Connection successful.', TMsgType.Notify);
end;

procedure g_Game_SaveOptions();
begin
  g_Options_Write_Video(GameDir+'/'+CONFIG_FILENAME);
end;

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
  if not g_Game_StartMap(MapPath, Force) then
    g_FatalError(Format(_lc[I_GAME_ERROR_MAP_LOAD], [MapPath]));
end;

procedure g_Game_Restart();
var
  Map: string;
begin
  if g_Game_IsClient then
    Exit;
  map := g_ExtractFileName(gMapInfo.Map);

  MessageTime := 0;
  gGameOn := False;
  g_Game_ClearLoading();
  g_Game_StartMap(Map, True, gCurrentMapFileName);
end;

function g_Game_StartMap(Map: String; Force: Boolean = False; const oldMapPath: AnsiString=''): Boolean;
var
  NewWAD, ResName: String;
  I: Integer;
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

  if isWadPath(Map) then
  begin
    NewWAD := g_ExtractWadName(Map);
    ResName := g_ExtractFileName(Map);
    if g_Game_IsServer then
    begin
      gWADHash := MD5File(MapsDir + NewWAD);
      g_Game_LoadWAD(NewWAD);
    end else
      // hash received in MC_RECV_GameEvent -> NET_EV_MAPSTART
      g_Game_ClientWAD(NewWAD, gWADHash);
  end else
    ResName := Map;

  Result := g_Map_Load(MapsDir + gGameSettings.WAD + ':\' + ResName);
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
  gTime := 0;
  NetTimeToUpdate := 1;
  NetTimeToReliable := 0;
  NetTimeToMaster := NetMasterRate;
  gLMSRespawn := LMS_RESPAWN_NONE;
  gLMSRespawnTime := 0;
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

  if (gGameSettings.MaxLives > 0) and (gGameSettings.WarmupTime > 0) then
  begin
    gLMSRespawn := LMS_RESPAWN_WARMUP;
    gLMSRespawnTime := gTime + gGameSettings.WarmupTime*1000;
    gLMSSoftSpawn := True;
    if NetMode = NET_SERVER then
      MH_SEND_GameEvent(NET_EV_LMS_WARMUP, (gLMSRespawnTime - gTime) div 1000)
    else
      g_Console_Add(Format(_lc[I_MSG_WARMUP_START], [(gLMSRespawnTime - gTime) div 1000]), True);
  end;

  if NetMode = NET_SERVER then
  begin
    MH_SEND_GameEvent(NET_EV_MAPSTART, gGameSettings.GameMode, Map);

  // Мастерсервер
    if NetUseMaster then
    begin
      if (NetMHost = nil) or (NetMPeer = nil) then
        if not g_Net_Slist_Connect then
          g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_SLIST_ERROR]);

      g_Net_Slist_Update;
    end;

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

procedure SetFirstLevel();
begin
  gNextMap := '';

  MapList := g_Map_GetMapsList(MapsDir + gGameSettings.WAD);
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

    if not g_Map_Exist(MapsDir + gGameSettings.WAD + ':\' + gNextMap) then
    begin
      gLastMap := True;
      if gGameSettings.GameMode = GM_COOP then
        gStatsOff := True;

      gStatsPressed := True;
      gNextMap := 'MAP01';

      if not g_Map_Exist(MapsDir + gGameSettings.WAD + ':\' + gNextMap) then
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

procedure g_Game_ClientWAD(NewWAD: String; WHash: TMD5Digest);
var
  gWAD: String;
begin
  if LowerCase(NewWAD) = LowerCase(gGameSettings.WAD) then
    Exit;
  if not g_Game_IsClient then
    Exit;
  gWAD := g_Res_SearchSameWAD(MapsDir, ExtractFileName(NewWAD), WHash);
  if gWAD = '' then
  begin
    g_Game_SetLoadingText(_lc[I_LOAD_DL_RES], 0, False);
    gWAD := g_Res_DownloadWAD(ExtractFileName(NewWAD));
    if gWAD = '' then
    begin
      g_Game_Free();
      g_FatalError(Format(_lc[I_GAME_ERROR_MAP_WAD], [ExtractFileName(NewWAD)]));
      Exit;
    end;
  end;
  NewWAD := ExtractRelativePath(MapsDir, gWAD);
  g_Game_LoadWAD(NewWAD);
end;

procedure g_Game_RestartRound(NoMapRestart: Boolean = False);
var
  i, n, nb, nr: Integer;

  function monRespawn (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    if not mon.FNoRespawn then mon.Respawn();
  end;

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

  if (n < 2) or ((gGameSettings.GameMode = GM_TDM) and ((nr = 0) or (nb = 0))) then
  begin
    // wait a second until the fuckers finally decide to join
    gLMSRespawn := LMS_RESPAWN_WARMUP;
    gLMSRespawnTime := gTime + 1000;
    gLMSSoftSpawn := NoMapRestart;
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
    if (gPlayer1 = nil) and (gLMSPID1 > 0) then
      gPlayer1 := g_Player_Get(gLMSPID1);
    if (gPlayer2 = nil) and (gLMSPID2 > 0) then
      gPlayer2 := g_Player_Get(gLMSPID2);
  end;

  g_Items_RestartRound();


  g_Mons_ForEach(monRespawn);

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

  MapList := g_Map_GetMapsList(MapsDir + gGameSettings.WAD);
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

    if not g_Map_Exist(MapsDir + gGameSettings.WAD + ':\' + Result) then Result := Map;
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
  cmd, s: string;
  config: TConfig;
begin
  stat := nil;
  cmd := LowerCase(P[0]);
  if cmd = 'r_showfps' then
  begin
    if (Length(P) > 1) and
       ((P[1] = '1') or (P[1] = '0')) then
      gShowFPS := (P[1][1] = '1');

    if gShowFPS then
      g_Console_Add(_lc[I_MSG_SHOW_FPS_ON])
    else
      g_Console_Add(_lc[I_MSG_SHOW_FPS_OFF]);
  end
  else if (cmd = 'g_friendlyfire') and not g_Game_IsClient then
  begin
    with gGameSettings do
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
      begin
        if (P[1][1] = '1') then
          Options := Options or GAME_OPTION_TEAMDAMAGE
        else
          Options := Options and (not GAME_OPTION_TEAMDAMAGE);
      end;

      if (LongBool(Options and GAME_OPTION_TEAMDAMAGE)) then
        g_Console_Add(_lc[I_MSG_FRIENDLY_FIRE_ON])
      else
        g_Console_Add(_lc[I_MSG_FRIENDLY_FIRE_OFF]);

      if g_Game_IsNet then MH_SEND_GameSettings;
    end;
  end
  else if (cmd = 'g_weaponstay') and not g_Game_IsClient then
  begin
    with gGameSettings do
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
      begin
        if (P[1][1] = '1') then
          Options := Options or GAME_OPTION_WEAPONSTAY
        else
          Options := Options and (not GAME_OPTION_WEAPONSTAY);
      end;

      if (LongBool(Options and GAME_OPTION_WEAPONSTAY)) then
        g_Console_Add(_lc[I_MSG_WEAPONSTAY_ON])
      else
        g_Console_Add(_lc[I_MSG_WEAPONSTAY_OFF]);

      if g_Game_IsNet then MH_SEND_GameSettings;
    end;
  end
  else if cmd = 'g_gamemode' then
  begin
    a := g_Game_TextToMode(P[1]);
    if a = GM_SINGLE then a := GM_COOP;
    if (Length(P) > 1) and (a <> GM_NONE) and (not g_Game_IsClient) then
    begin
      gSwitchGameMode := a;
      if (gGameOn and (gGameSettings.GameMode = GM_SINGLE)) or
         (gState = STATE_INTERSINGLE) then
        gSwitchGameMode := GM_SINGLE;
      if not gGameOn then
        gGameSettings.GameMode := gSwitchGameMode;
    end;
    if gSwitchGameMode = gGameSettings.GameMode then
      g_Console_Add(Format(_lc[I_MSG_GAMEMODE_CURRENT],
                          [g_Game_ModeToText(gGameSettings.GameMode)]))
    else
      g_Console_Add(Format(_lc[I_MSG_GAMEMODE_CHANGE],
                          [g_Game_ModeToText(gGameSettings.GameMode),
                           g_Game_ModeToText(gSwitchGameMode)]));
  end
  else if (cmd = 'g_allow_exit') and not g_Game_IsClient then
  begin
    with gGameSettings do
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
      begin
        if (P[1][1] = '1') then
          Options := Options or GAME_OPTION_ALLOWEXIT
        else
          Options := Options and (not GAME_OPTION_ALLOWEXIT);
      end;

      if (LongBool(Options and GAME_OPTION_ALLOWEXIT)) then
        g_Console_Add(_lc[I_MSG_ALLOWEXIT_ON])
      else
        g_Console_Add(_lc[I_MSG_ALLOWEXIT_OFF]);
      g_Console_Add(_lc[I_MSG_ONMAPCHANGE]);

      if g_Game_IsNet then MH_SEND_GameSettings;
    end;
  end
  else if (cmd = 'g_allow_monsters') and not g_Game_IsClient then
  begin
    with gGameSettings do
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
      begin
        if (P[1][1] = '1') then
          Options := Options or GAME_OPTION_MONSTERS
        else
          Options := Options and (not GAME_OPTION_MONSTERS);
      end;

      if (LongBool(Options and GAME_OPTION_MONSTERS)) then
        g_Console_Add(_lc[I_MSG_ALLOWMON_ON])
      else
        g_Console_Add(_lc[I_MSG_ALLOWMON_OFF]);
      g_Console_Add(_lc[I_MSG_ONMAPCHANGE]);

      if g_Game_IsNet then MH_SEND_GameSettings;
    end;
  end
  else if (cmd = 'g_bot_vsplayers') and not g_Game_IsClient then
  begin
    with gGameSettings do
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
      begin
        if (P[1][1] = '1') then
          Options := Options or GAME_OPTION_BOTVSPLAYER
        else
          Options := Options and (not GAME_OPTION_BOTVSPLAYER);
      end;

      if (LongBool(Options and GAME_OPTION_BOTVSPLAYER)) then
        g_Console_Add(_lc[I_MSG_BOTSVSPLAYERS_ON])
      else
        g_Console_Add(_lc[I_MSG_BOTSVSPLAYERS_OFF]);

      if g_Game_IsNet then MH_SEND_GameSettings;
    end;
  end
  else if (cmd = 'g_bot_vsmonsters') and not g_Game_IsClient then
  begin
    with gGameSettings do
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
      begin
        if (P[1][1] = '1') then
          Options := Options or GAME_OPTION_BOTVSMONSTER
        else
          Options := Options and (not GAME_OPTION_BOTVSMONSTER);
      end;

      if (LongBool(Options and GAME_OPTION_BOTVSMONSTER)) then
        g_Console_Add(_lc[I_MSG_BOTSVSMONSTERS_ON])
      else
        g_Console_Add(_lc[I_MSG_BOTSVSMONSTERS_OFF]);

      if g_Game_IsNet then MH_SEND_GameSettings;
    end;
  end
  else if (cmd = 'g_warmuptime') and not g_Game_IsClient then
  begin
    if Length(P) > 1 then
    begin
      if StrToIntDef(P[1], gGameSettings.WarmupTime) = 0 then
        gGameSettings.WarmupTime := 30
      else
        gGameSettings.WarmupTime := StrToIntDef(P[1], gGameSettings.WarmupTime);
    end;

    g_Console_Add(Format(_lc[I_MSG_WARMUP],
                 [gGameSettings.WarmupTime]));
    g_Console_Add(_lc[I_MSG_ONMAPCHANGE]);
  end
  else if cmd = 'net_interp' then
  begin
    if (Length(P) > 1) then
      NetInterpLevel := StrToIntDef(P[1], NetInterpLevel);

    g_Console_Add('net_interp = ' + IntToStr(NetInterpLevel));
    config := TConfig.CreateFile(GameDir+'/'+CONFIG_FILENAME);
    config.WriteInt('Client', 'InterpolationSteps', NetInterpLevel);
    config.SaveFile(GameDir+'/'+CONFIG_FILENAME);
    config.Free();
  end
  else if cmd = 'net_forceplayerupdate' then
  begin
    if (Length(P) > 1) and
       ((P[1] = '1') or (P[1] = '0')) then
      NetForcePlayerUpdate := (P[1][1] = '1');

    if NetForcePlayerUpdate then
      g_Console_Add('net_forceplayerupdate = 1')
    else
      g_Console_Add('net_forceplayerupdate = 0');
    config := TConfig.CreateFile(GameDir+'/'+CONFIG_FILENAME);
    config.WriteBool('Client', 'ForcePlayerUpdate', NetForcePlayerUpdate);
    config.SaveFile(GameDir+'/'+CONFIG_FILENAME);
    config.Free();
  end
  else if cmd = 'net_predictself' then
  begin
    if (Length(P) > 1) and
       ((P[1] = '1') or (P[1] = '0')) then
      NetPredictSelf := (P[1][1] = '1');

    if NetPredictSelf then
      g_Console_Add('net_predictself = 1')
    else
      g_Console_Add('net_predictself = 0');
    config := TConfig.CreateFile(GameDir+'/'+CONFIG_FILENAME);
    config.WriteBool('Client', 'PredictSelf', NetPredictSelf);
    config.SaveFile(GameDir+'/'+CONFIG_FILENAME);
    config.Free();
  end
  else if cmd = 'sv_name' then
  begin
    if (Length(P) > 1) and (Length(P[1]) > 0) then
    begin
      NetServerName := P[1];
      if Length(NetServerName) > 64 then
        SetLength(NetServerName, 64);
      if g_Game_IsServer and g_Game_IsNet and NetUseMaster then
        g_Net_Slist_Update;
    end;

    g_Console_Add(cmd + ' = "' + NetServerName + '"');
  end
  else if cmd = 'sv_passwd' then
  begin
    if (Length(P) > 1) and (Length(P[1]) > 0) then
    begin
      NetPassword := P[1];
      if Length(NetPassword) > 24 then
        SetLength(NetPassword, 24);
      if g_Game_IsServer and g_Game_IsNet and NetUseMaster then
        g_Net_Slist_Update;
    end;

    g_Console_Add(cmd + ' = "' + AnsiLowerCase(NetPassword) + '"');
  end
  else if cmd = 'sv_maxplrs' then
  begin
    if (Length(P) > 1) then
    begin
      NetMaxClients := Min(Max(StrToIntDef(P[1], NetMaxClients), 1), NET_MAXCLIENTS);
      if g_Game_IsServer and g_Game_IsNet then
      begin
        b := 0;
        for a := 0 to High(NetClients) do
          if NetClients[a].Used then
          begin
            Inc(b);
            if b > NetMaxClients then
            begin
              s := g_Player_Get(NetClients[a].Player).Name;
              enet_peer_disconnect(NetClients[a].Peer, NET_DISC_FULL);
              g_Console_Add(Format(_lc[I_PLAYER_KICK], [s]));
              MH_SEND_GameEvent(NET_EV_PLAYER_KICK, 0, s);
            end;
          end;
        if NetUseMaster then
          g_Net_Slist_Update;
      end;
    end;

    g_Console_Add(cmd + ' = ' + IntToStr(NetMaxClients));
  end
  else if cmd = 'sv_public' then
  begin
    if (Length(P) > 1) then
    begin
      NetUseMaster := StrToIntDef(P[1], Byte(NetUseMaster)) > 0;
      if g_Game_IsServer and g_Game_IsNet then
        if NetUseMaster then
        begin
          if NetMPeer = nil then
            if not g_Net_Slist_Connect() then
              g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_SLIST_ERROR]);
          g_Net_Slist_Update();
        end
        else
          if NetMPeer <> nil then
            g_Net_Slist_Disconnect();
    end;

    g_Console_Add(cmd + ' = ' + IntToStr(Byte(NetUseMaster)));
  end
  else if cmd = 'sv_intertime' then
  begin
    if (Length(P) > 1) then
      gDefInterTime := Min(Max(StrToIntDef(P[1], gDefInterTime), -1), 120);

    g_Console_Add(cmd + ' = ' + IntToStr(gDefInterTime));
  end
  else if cmd = 'p1_name' then
  begin
    if (Length(P) > 1) and gGameOn then
    begin
      if g_Game_IsClient then
      begin
        gPlayer1Settings.Name := b_Text_Unformat(P[1]);
        MC_SEND_PlayerSettings;
      end
      else
        if gPlayer1 <> nil then
        begin
          gPlayer1.Name := b_Text_Unformat(P[1]);
          if g_Game_IsNet then MH_SEND_PlayerSettings(gPlayer1.UID);
        end
        else
          gPlayer1Settings.Name := b_Text_Unformat(P[1]);
    end;
  end
  else if cmd = 'p2_name' then
  begin
    if (Length(P) > 1) and gGameOn then
    begin
      if g_Game_IsClient then
      begin
        gPlayer2Settings.Name := b_Text_Unformat(P[1]);
        MC_SEND_PlayerSettings;
      end
      else
        if gPlayer2 <> nil then
        begin
          gPlayer2.Name := b_Text_Unformat(P[1]);
          if g_Game_IsNet then MH_SEND_PlayerSettings(gPlayer2.UID);
        end
        else
          gPlayer2Settings.Name := b_Text_Unformat(P[1]);
    end;
  end
  else if cmd = 'p1_color' then
  begin
    if Length(P) > 3 then
      if g_Game_IsClient then
      begin
        gPlayer1Settings.Color := _RGB(EnsureRange(StrToIntDef(P[1], 0), 0, 255),
                                       EnsureRange(StrToIntDef(P[2], 0), 0, 255),
                                       EnsureRange(StrToIntDef(P[3], 0), 0, 255));
        MC_SEND_PlayerSettings;
      end
      else
        if gPlayer1 <> nil then
        begin
          gPlayer1.Model.SetColor(EnsureRange(StrToIntDef(P[1], 0), 0, 255),
                                  EnsureRange(StrToIntDef(P[2], 0), 0, 255),
                                  EnsureRange(StrToIntDef(P[3], 0), 0, 255));
          if g_Game_IsNet then MH_SEND_PlayerSettings(gPlayer1.UID);
        end
        else
          gPlayer1Settings.Color := _RGB(EnsureRange(StrToIntDef(P[1], 0), 0, 255),
                                         EnsureRange(StrToIntDef(P[2], 0), 0, 255),
                                         EnsureRange(StrToIntDef(P[3], 0), 0, 255));
  end
  else if (cmd = 'p2_color') and not g_Game_IsNet then
  begin
    if Length(P) > 3 then
      if g_Game_IsClient then
      begin
        gPlayer2Settings.Color := _RGB(EnsureRange(StrToIntDef(P[1], 0), 0, 255),
                                       EnsureRange(StrToIntDef(P[2], 0), 0, 255),
                                       EnsureRange(StrToIntDef(P[3], 0), 0, 255));
        MC_SEND_PlayerSettings;
      end
      else
        if gPlayer2 <> nil then
        begin
          gPlayer2.Model.SetColor(EnsureRange(StrToIntDef(P[1], 0), 0, 255),
                                  EnsureRange(StrToIntDef(P[2], 0), 0, 255),
                                  EnsureRange(StrToIntDef(P[3], 0), 0, 255));
          if g_Game_IsNet then MH_SEND_PlayerSettings(gPlayer2.UID);
        end
        else
          gPlayer2Settings.Color := _RGB(EnsureRange(StrToIntDef(P[1], 0), 0, 255),
                                         EnsureRange(StrToIntDef(P[2], 0), 0, 255),
                                         EnsureRange(StrToIntDef(P[3], 0), 0, 255));
  end
  else if gGameSettings.GameType in [GT_CUSTOM, GT_SERVER, GT_CLIENT] then
  begin
    if cmd = 'r_showtime' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        gShowTime := (P[1][1] = '1');

      if gShowTime then
        g_Console_Add(_lc[I_MSG_TIME_ON])
      else
        g_Console_Add(_lc[I_MSG_TIME_OFF]);
    end
    else if cmd = 'r_showscore' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        gShowGoals := (P[1][1] = '1');

      if gShowGoals then
        g_Console_Add(_lc[I_MSG_SCORE_ON])
      else
        g_Console_Add(_lc[I_MSG_SCORE_OFF]);
    end
    else if cmd = 'r_showstat' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        gShowStat := (P[1][1] = '1');

      if gShowStat then
        g_Console_Add(_lc[I_MSG_STATS_ON])
      else
        g_Console_Add(_lc[I_MSG_STATS_OFF]);
    end
    else if cmd = 'r_showkillmsg' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        gShowKillMsg := (P[1][1] = '1');

      if gShowKillMsg then
        g_Console_Add(_lc[I_MSG_KILL_MSGS_ON])
      else
        g_Console_Add(_lc[I_MSG_KILL_MSGS_OFF]);
    end
    else if cmd = 'r_showlives' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        gShowLives := (P[1][1] = '1');

      if gShowLives then
        g_Console_Add(_lc[I_MSG_LIVES_ON])
      else
        g_Console_Add(_lc[I_MSG_LIVES_OFF]);
    end
    else if cmd = 'r_showspect' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        gSpectHUD := (P[1][1] = '1');

      if gSpectHUD then
        g_Console_Add(_lc[I_MSG_SPECT_HUD_ON])
      else
        g_Console_Add(_lc[I_MSG_SPECT_HUD_OFF]);
    end
    else if cmd = 'r_showping' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        gShowPing := (P[1][1] = '1');

      if gShowPing then
        g_Console_Add(_lc[I_MSG_PING_ON])
      else
        g_Console_Add(_lc[I_MSG_PING_OFF]);
    end
    else if (cmd = 'g_scorelimit') and not g_Game_IsClient then
    begin
      if Length(P) > 1 then
      begin
        if StrToIntDef(P[1], gGameSettings.GoalLimit) = 0 then
          gGameSettings.GoalLimit := 0
        else
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
              b := Max(gTeamStat[TEAM_RED].Goals, gTeamStat[TEAM_BLUE].Goals);

            gGameSettings.GoalLimit := Max(StrToIntDef(P[1], gGameSettings.GoalLimit), b);
          end;

        if g_Game_IsNet then MH_SEND_GameSettings;
      end;

      g_Console_Add(Format(_lc[I_MSG_SCORE_LIMIT], [gGameSettings.GoalLimit]));
    end
    else if (cmd = 'g_timelimit') and not g_Game_IsClient then
    begin
      if (Length(P) > 1) and (StrToIntDef(P[1], -1) >= 0) then
        gGameSettings.TimeLimit := StrToIntDef(P[1], -1);

      g_Console_Add(Format(_lc[I_MSG_TIME_LIMIT],
                           [gGameSettings.TimeLimit div 3600,
                           (gGameSettings.TimeLimit div 60) mod 60,
                            gGameSettings.TimeLimit mod 60]));
      if g_Game_IsNet then MH_SEND_GameSettings;
    end
    else if (cmd = 'g_maxlives') and not g_Game_IsClient then
    begin
      if Length(P) > 1 then
      begin
        if StrToIntDef(P[1], gGameSettings.MaxLives) = 0 then
          gGameSettings.MaxLives := 0
        else
        begin
          b := 0;
          stat := g_Player_GetStats();
          if stat <> nil then
            for a := 0 to High(stat) do
              if stat[a].Lives > b then
                b := stat[a].Lives;
          gGameSettings.MaxLives :=
            Max(StrToIntDef(P[1], gGameSettings.MaxLives), b);
        end;
      end;

      g_Console_Add(Format(_lc[I_MSG_LIVES],
                           [gGameSettings.MaxLives]));
      if g_Game_IsNet then MH_SEND_GameSettings;
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
      g_Console_Add(Format('gWinPosX = %d, gWinPosY %d', [gWinPosX, gWinPosY]));
      g_Console_Add(Format('gWinRealPosX = %d, gWinRealPosY %d', [gWinRealPosX, gWinRealPosY]));
      g_Console_Add(Format('gScreenWidth = %d, gScreenHeight = %d', [gScreenWidth, gScreenHeight]));
      g_Console_Add(Format('gWinSizeX = %d, gWinSizeY = %d', [gWinSizeX, gWinSizeY]));
      g_Console_Add(Format('Frame X = %d, Y = %d, Caption Y = %d', [gWinFrameX, gWinFrameY, gWinCaption]));
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
    else if (cmd = 'd_joy') then
    begin
      for a := 1 to 8 do
        g_Console_Add(e_JoystickStateToString(a));
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
begin
// Общие команды:
  cmd := LowerCase(P[0]);
  chstr := '';
  if (cmd = 'quit') or
     (cmd = 'exit') then
  begin
    g_Game_Free();
    g_Game_Quit();
    Exit;
  end
  else if cmd = 'pause' then
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
        enet_peer_disconnect(pl^.Peer, NET_DISC_KICK);
        g_Console_Add(Format(_lc[I_PLAYER_KICK], [s]));
        MH_SEND_GameEvent(NET_EV_PLAYER_KICK, 0, s);
        if NetUseMaster then
          g_Net_Slist_Update;
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
              if NetUseMaster then
                g_Net_Slist_Update;
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
          enet_peer_disconnect(NetClients[a].Peer, NET_DISC_KICK);
          g_Console_Add(Format(_lc[I_PLAYER_KICK], [s]));
          MH_SEND_GameEvent(NET_EV_PLAYER_KICK, 0, s);
          if NetUseMaster then
            g_Net_Slist_Update;
        end;
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
        enet_peer_disconnect(pl^.Peer, NET_DISC_TEMPBAN);
        g_Console_Add(Format(_lc[I_PLAYER_BAN], [s]));
        MH_SEND_GameEvent(NET_EV_PLAYER_BAN, 0, s);
        if NetUseMaster then
          g_Net_Slist_Update;
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
          enet_peer_disconnect(NetClients[a].Peer, NET_DISC_TEMPBAN);
          g_Console_Add(Format(_lc[I_PLAYER_BAN], [s]));
          MH_SEND_GameEvent(NET_EV_PLAYER_BAN, 0, s);
          if NetUseMaster then
            g_Net_Slist_Update;
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
        enet_peer_disconnect(pl^.Peer, NET_DISC_BAN);
        g_Net_SaveBanList();
        g_Console_Add(Format(_lc[I_PLAYER_BAN], [s]));
        MH_SEND_GameEvent(NET_EV_PLAYER_BAN, 0, s);
        if NetUseMaster then
          g_Net_Slist_Update;
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
          enet_peer_disconnect(NetClients[a].Peer, NET_DISC_BAN);
          g_Net_SaveBanList();
          g_Console_Add(Format(_lc[I_PLAYER_BAN], [s]));
          MH_SEND_GameEvent(NET_EV_PLAYER_BAN, 0, s);
          if NetUseMaster then
            g_Net_Slist_Update;
        end;
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
    if Length(P) > 1 then
      g_Bot_Add(TEAM_NONE, StrToIntDef(P[1], 2))
    else
      g_Bot_Add(TEAM_NONE, 2);
  end
  else if cmd = 'bot_addlist' then
  begin
    if Length(P) > 1 then
      if Length(P) = 2 then
        g_Bot_AddList(TEAM_NONE, P[1], StrToIntDef(P[1], -1))
      else
        g_Bot_AddList(IfThen(P[2] = 'red', TEAM_RED, TEAM_BLUE), P[1], StrToIntDef(P[1], -1));
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
    // Игра ещё не запущена, сначала нам надо загрузить какой-то WAD
    P[1] := addWadExtension(P[1]);
    if FileExists(MapsDir + P[1]) then
    begin
      // Если карта не указана, берём первую карту в файле
      if Length(P) < 3 then
      begin
        SetLength(P, 3);
        P[2] := g_Game_GetFirstMap(MapsDir + P[1]);
      end;

      s := P[1] + ':\' + UpperCase(P[2]);

      if g_Map_Exist(MapsDir + s) then
      begin
        // Запускаем свою игру
        g_Game_Free();
        with gGameSettings do
        begin
          GameMode := g_Game_TextToMode(gcGameMode);
          if gSwitchGameMode <> GM_NONE then
            GameMode := gSwitchGameMode;
          if GameMode = GM_NONE then GameMode := GM_DM;
          if GameMode = GM_SINGLE then GameMode := GM_COOP;
          b := 1;
          if Length(P) >= 4 then
            b := StrToIntDef(P[3], 1);
          g_Game_StartCustom(s, GameMode, TimeLimit,
                             GoalLimit, MaxLives, Options, b);
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

    P[3] := addWadExtension(P[3]);
    if FileExists(MapsDir + P[3]) then
    begin
      // Если карта не указана, берём первую карту в файле
      if Length(P) < 5 then
      begin
        SetLength(P, 5);
        P[4] := g_Game_GetFirstMap(MapsDir + P[1]);
      end;

      s := P[3] + ':\' + UpperCase(P[4]);

      if g_Map_Exist(MapsDir + s) then
      begin
        // Запускаем свою игру
        g_Game_Free();
        with gGameSettings do
        begin
          GameMode := g_Game_TextToMode(gcGameMode);
          if gSwitchGameMode <> GM_NONE then
            GameMode := gSwitchGameMode;
          if GameMode = GM_NONE then GameMode := GM_DM;
          if GameMode = GM_SINGLE then GameMode := GM_COOP;
          b := 0;
          if Length(P) >= 6 then
            b := StrToIntDef(P[5], 0);
          g_Game_StartServer(s, GameMode, TimeLimit,
                             GoalLimit, MaxLives, Options, b, listen, prt);
        end;
      end
      else
        if P[4] = '' then
          g_Console_Add(Format(_lc[I_MSG_NO_MAPS], [P[3]]))
        else
          g_Console_Add(Format(_lc[I_MSG_NO_MAP_FALLBACK], [UpperCase(P[4]), P[3]]));
    end else
      g_Console_Add(Format(_lc[I_MSG_NO_WAD], [P[3]]));
  end
  else if cmd = 'map' then
  begin
    if Length(P) = 1 then
    begin
      if g_Game_IsServer and (gGameSettings.GameType <> GT_SINGLE) then
      begin
        g_Console_Add(cmd + ' <MAP>');
        g_Console_Add(cmd + ' <WAD> [MAP]');
      end else
        g_Console_Add(_lc[I_MSG_GM_UNAVAIL]);
    end else
      if g_Game_IsServer and (gGameSettings.GameType <> GT_SINGLE) then
      begin
        // Идёт своя игра или сервер
        if Length(P) < 3 then
        begin
          // Первый параметр - либо карта, либо имя WAD файла
          s := UpperCase(P[1]);
          if g_Map_Exist(MapsDir + gGameSettings.WAD + ':\' + s) then
          begin // Карта нашлась
            gExitByTrigger := False;
            if gGameOn then
            begin // Идёт игра - завершаем уровень
              gNextMap := s;
              gExit := EXIT_ENDLEVELCUSTOM;
            end
            else // Интермиссия - сразу загружаем карту
              g_Game_ChangeMap(s);
          end else
          begin
            // Такой карты нет, ищем WAD файл
            P[1] := addWadExtension(P[1]);
            g_Console_Add(Format(_lc[I_MSG_NO_MAP_FALLBACK], [s, P[1]]));
            if FileExists(MapsDir + P[1]) then
            begin
              // Параметра карты нет, поэтому ставим первую из файла
              SetLength(P, 3);
              P[2] := g_Game_GetFirstMap(MapsDir + P[1]);

              s := P[1] + ':\' + P[2];

              if g_Map_Exist(MapsDir + s) then
              begin
                gExitByTrigger := False;
                if gGameOn then
                begin // Идёт игра - завершаем уровень
                  gNextMap := s;
                  gExit := EXIT_ENDLEVELCUSTOM;
                end
                else // Интермиссия - сразу загружаем карту
                  g_Game_ChangeMap(s);
              end else
                if P[2] = '' then
                  g_Console_Add(Format(_lc[I_MSG_NO_MAPS], [P[1]]))
                else
                  g_Console_Add(Format(_lc[I_MSG_NO_MAP], [P[2]]));
            end else
              g_Console_Add(Format(_lc[I_MSG_NO_WAD], [P[1]]));
          end;
        end else
        begin
          // Указано два параметра, значит первый - WAD файл, а второй - карта
          P[1] := addWadExtension(P[1]);
          if FileExists(MapsDir + P[1]) then
          begin
            // Нашли WAD файл
            P[2] := UpperCase(P[2]);
            s := P[1] + ':\' + P[2];

            if g_Map_Exist(MapsDir + s) then
            begin // Нашли карту
              gExitByTrigger := False;
              if gGameOn then
              begin // Идёт игра - завершаем уровень
                gNextMap := s;
                gExit := EXIT_ENDLEVELCUSTOM;
              end
              else // Интермиссия - сразу загружаем карту
                g_Game_ChangeMap(s);
            end else
              g_Console_Add(Format(_lc[I_MSG_NO_MAP], [P[2]]));
          end else
            g_Console_Add(Format(_lc[I_MSG_NO_WAD], [P[1]]));
        end;
      end else
        g_Console_Add(_lc[I_MSG_GM_UNAVAIL]);
  end
  else if cmd = 'nextmap' then
  begin
    if not(gGameOn or (gState = STATE_INTERCUSTOM)) then
      g_Console_Add(_lc[I_MSG_NOT_GAME])
    else begin
      nm := True;
      if Length(P) = 1 then
      begin
        if g_Game_IsServer and (gGameSettings.GameType <> GT_SINGLE) then
        begin
          g_Console_Add(cmd + ' <MAP>');
          g_Console_Add(cmd + ' <WAD> [MAP]');
        end else begin
          nm := False;
          g_Console_Add(_lc[I_MSG_GM_UNAVAIL]);
        end;
      end else
      begin
        nm := False;
        if g_Game_IsServer and (gGameSettings.GameType <> GT_SINGLE) then
        begin
          if Length(P) < 3 then
          begin
            // Первый параметр - либо карта, либо имя WAD файла
            s := UpperCase(P[1]);
            if g_Map_Exist(MapsDir + gGameSettings.WAD + ':\' + s) then
            begin // Карта нашлась
              gExitByTrigger := False;
              gNextMap := s;
              nm := True;
            end else
            begin
              // Такой карты нет, ищем WAD файл
              P[1] := addWadExtension(P[1]);
              g_Console_Add(Format(_lc[I_MSG_NO_MAP_FALLBACK], [s, P[1]]));
              if FileExists(MapsDir + P[1]) then
              begin
                // Параметра карты нет, поэтому ставим первую из файла
                SetLength(P, 3);
                P[2] := g_Game_GetFirstMap(MapsDir + P[1]);

                s := P[1] + ':\' + P[2];

                if g_Map_Exist(MapsDir + s) then
                begin // Устанавливаем карту
                  gExitByTrigger := False;
                  gNextMap := s;
                  nm := True;
                end else
                  if P[2] = '' then
                    g_Console_Add(Format(_lc[I_MSG_NO_MAPS], [P[1]]))
                  else
                    g_Console_Add(Format(_lc[I_MSG_NO_MAP], [P[2]]));
              end else
                g_Console_Add(Format(_lc[I_MSG_NO_WAD], [P[1]]));
            end;
          end else
          begin
            // Указано два параметра, значит первый - WAD файл, а второй - карта
            P[1] := addWadExtension(P[1]);
            if FileExists(MapsDir + P[1]) then
            begin
              // Нашли WAD файл
              P[2] := UpperCase(P[2]);
              s := P[1] + ':\' + P[2];

              if g_Map_Exist(MapsDir + s) then
              begin // Нашли карту
                gExitByTrigger := False;
                gNextMap := s;
                nm := True;
              end else
                g_Console_Add(Format(_lc[I_MSG_NO_MAP], [P[2]]));
            end else
              g_Console_Add(Format(_lc[I_MSG_NO_WAD], [P[1]]));
          end;
        end else
          g_Console_Add(_lc[I_MSG_GM_UNAVAIL]);
      end;
      if nm then
        if gNextMap = '' then
          g_Console_Add(_lc[I_MSG_NEXTMAP_UNSET])
        else
          g_Console_Add(Format(_lc[I_MSG_NEXTMAP_SET], [gNextMap]));
    end;
  end
  else if (cmd = 'endmap') or (cmd = 'goodbye') then
  begin
    if not gGameOn then
      g_Console_Add(_lc[I_MSG_NOT_GAME])
    else
      if g_Game_IsServer and (gGameSettings.GameType <> GT_SINGLE) then
      begin
        gExitByTrigger := False;
        // Следующая карта не задана, пробуем найти триггер Выход
        if (gNextMap = '') and (gTriggers <> nil) then
          for a := 0 to High(gTriggers) do
            if gTriggers[a].TriggerType = TRIGGER_EXIT then
            begin
              gExitByTrigger := True;
              //gNextMap := gTriggers[a].Data.MapName;
              gNextMap := gTriggers[a].tgcMap;
              Break;
            end;
        // Ищем следующую карту в WAD файле
        if gNextMap = '' then
          gNextMap := g_Game_GetNextMap();
        // Проверяем, не задан ли WAD файл ресурсной строкой
        if not isWadPath(gNextMap) then
          s := gGameSettings.WAD + ':\' + gNextMap
        else
          s := gNextMap;
        // Если карта найдена, выходим с уровня
        if g_Map_Exist(MapsDir + s) then
          gExit := EXIT_ENDLEVELCUSTOM
        else
          g_Console_Add(Format(_lc[I_MSG_NO_MAP], [gNextMap]));
      end else
        g_Console_Add(_lc[I_MSG_GM_UNAVAIL]);
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

procedure g_TakeScreenShot();
var
  a: Word;
  FileName: string;
  ssdir, t: string;
  st: TStream;
  ok: Boolean;
begin
  if e_NoGraphics then Exit;
  ssdir := GameDir+'/screenshots';
  if not findFileCI(ssdir, true) then
  begin
    // try to create dir
    try
      CreateDir(ssdir);
    except
    end;
    if not findFileCI(ssdir, true) then exit; // alas
  end;
  try
    for a := 1 to High(Word) do
    begin
      FileName := Format(ssdir+'screenshot%.3d.png', [a]);
      t := FileName;
      if findFileCI(t, true) then continue;
      if not findFileCI(FileName) then
      begin
        ok := false;
        st := createDiskFile(FileName);
        try
          e_MakeScreenshot(st, gScreenWidth, gScreenHeight);
          ok := true;
        finally
          st.Free();
        end;
        if not ok then try DeleteFile(FileName); except end else g_Console_Add(Format(_lc[I_CONSOLE_SCREENSHOT], [ExtractFileName(FileName)]));
        break;
      end;
    end;
  except
  end;
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

procedure g_Game_UpdateTriggerSounds();
var
  i: Integer;
begin
  if gTriggers <> nil then
    for i := 0 to High(gTriggers) do
      with gTriggers[i] do
        if (TriggerType = TRIGGER_SOUND) and
           (Sound <> nil) and
           (tgcLocal) and
           Sound.IsPlaying() then
        begin
          if ((gPlayer1 <> nil) and g_CollidePoint(gPlayer1.GameX, gPlayer1.GameY, X, Y, Width, Height)) or
             ((gPlayer2 <> nil) and g_CollidePoint(gPlayer2.GameX, gPlayer2.GameY, X, Y, Width, Height)) then
          begin
            Sound.SetPan(0.5 - tgcPan/255.0);
            Sound.SetVolume(tgcVolume/255.0);
          end
          else
            Sound.SetCoords(X+(Width div 2), Y+(Height div 2), tgcVolume/255.0);
        end;
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

  ProcessLoading(true);
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
      ProcessLoading();
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
    if (s[1] = '-') and (Length(s) > 1) then
    begin
      if (s[2] = '-') and (Length(s) > 2) then
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

  // Goal limit:
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
      Opt := GAME_OPTION_ALLOWEXIT or GAME_OPTION_BOTVSPLAYER or GAME_OPTION_BOTVSMONSTER
    else
      Opt := StrToIntDef(s, 0);
    if Opt = 0 then
      Opt := GAME_OPTION_ALLOWEXIT or GAME_OPTION_BOTVSPLAYER or GAME_OPTION_BOTVSMONSTER;

  // Close after map:
    s := Find_Param_Value(pars, '--close');
    if (s <> '') then
      gMapOnce := True;

  // Override map to test:
    s := LowerCase(Find_Param_Value(pars, '-testmap'));
    if s <> '' then
      gTestMap := MapsDir + s;

  // Delete test map after play:
    s := Find_Param_Value(pars, '--testdelete');
    if (s <> '') then
    begin
      gMapToDelete := MapsDir + map;
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
      n := 1
    else
      n := StrToIntDef(s, 1);

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
    if not isWadPath(s) then
      s := GameDir + '/' + s;

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

  conRegVar('dbg_holmes', @g_holmes_enabled, 'enable/disable Holmes', 'Holmes', true);

  conRegVar('dbg_ignore_level_bounds', @g_dbg_ignore_bounds, 'ignore level bounds', '',  false);

  conRegVar('r_scale', @g_dbg_scale, 0.01, 100.0, 'render scale', '',  false);

  conRegVar('light_enabled', @gwin_k8_enable_light_experiments, 'enable/disable dynamic lighting', 'lighting');
  conRegVar('light_player_halo', @g_playerLight, 'enable/disable player halo', 'player light halo');

  conRegVar('r_smallmap_align_h', @r_smallmap_h, 'halign: 0: left; 1: center; 2: right', 'horizontal aligning of small maps');
  conRegVar('r_smallmap_align_v', @r_smallmap_v, 'valign: 0: top; 1: center; 2: bottom', 'vertial aligning of small maps');
end.
