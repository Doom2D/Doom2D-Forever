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
  g_language, g_weapons;

function GenPlayerName (n: Integer): String;

procedure g_Options_SetDefault();
procedure g_Options_Read(FileName: String);
procedure g_Options_Write(FileName: String);
procedure g_Options_Write_Language(FileName: String);
procedure g_Options_Write_Video(FileName: String);
procedure g_Options_Write_Gameplay_Custom(FileName: String);
procedure g_Options_Write_Gameplay_Net(FileName: String);
procedure g_Options_Write_Net_Server(FileName: String);
procedure g_Options_Write_Net_Client(FileName: String);

const DF_Default_Megawad_Start = 'megawads/DOOM2D.WAD:\MAP01';

var
//  gGameControls: TControls;
  gScreenWidth: Word;
  gScreenHeight: Word;
  gWinRealPosX: Integer;
  gWinRealPosY: Integer;
  gBPP: Byte;
  gFreq: Byte;
  gFullscreen: Boolean;
  gWinMaximized: Boolean;
  gVSync: Boolean;
  glLegacyNPOT: Boolean;
  gTextureFilter: Boolean;
  gNoSound: Boolean;
  gSoundLevel: Byte;
  gMusicLevel: Byte;
  gMaxSimSounds: Byte;
  gMuteWhenInactive: Boolean;
  gAdvCorpses: Boolean;
  gAdvBlood: Boolean;
  gAdvGibs: Boolean;
  gGibsCount: Integer;
  gBloodCount: Byte;
  gFlash: Byte;
  gDrawBackGround: Boolean;
  gShowMessages: Boolean;
  gRevertPlayers: Boolean;
  gLanguage: String;
  gAskLanguage: Boolean;
  gcMap: String;
  gcGameMode: String;
  gcTimeLimit: Word;
  gcGoalLimit: Word;
  gcMaxLives: Byte;
  gcPlayers: Byte;
  gcTeamDamage: Boolean;
  gcAllowExit: Boolean;
  gcWeaponStay: Boolean;
  gcMonsters: Boolean;
  gcBotsVS: String;
  gnMap: String;
  gnGameMode: String;
  gnTimeLimit: Word;
  gnGoalLimit: Word;
  gnMaxLives: Byte;
  gnPlayers: Byte;
  gnTeamDamage: Boolean;
  gnAllowExit: Boolean;
  gnWeaponStay: Boolean;
  gnMonsters: Boolean;
  gnBotsVS: String;
  gsSDLSampleRate: Integer;
  gsSDLBufferSize: Integer;
  gSFSDebug: Boolean;
  gSFSFastMode: Boolean;
  gDefaultMegawadStart: AnsiString;
  gBerserkAutoswitch: Boolean;

implementation

uses
  {$INCLUDE ../nogl/noGLuses.inc}
  e_log, e_input, g_console, g_window, g_sound, g_gfx, g_player, Math,
  g_map, g_net, g_netmaster, SysUtils, CONFIG, g_game, g_main, e_texture,
  g_items, wadreader, e_graphics, g_touch, SDL2, envvars;

  var
    machine: Integer;

  function GenPlayerName (n: Integer): String;
  begin
    ASSERT(n >= 1);
    Result := GetUserName;
    if Result = '' then
      Result := 'Player' + IntToStr(machine MOD 10000);
    if n = 1 then
      Result := Copy(Result, 1, 12) + ' '
    else
      Result := Copy(Result, 1, 10) + ' ' + IntToStr(n)
  end;

procedure g_Options_SetDefaultVideo;
var
  target, closest, display: TSDL_DisplayMode;
  percentage: Integer;
begin
  (* Display 0 = Primary display *)
  SDL_GetDesktopDisplayMode(0, @display);
  {$IF DEFINED(ANDROID)}
    gScreenWidth := display.w;
    gScreenHeight := display.h;
    //gBPP := SDL_BITSPERPIXEL(dispaly.format);
    gBPP := 32;
    gFullScreen := True; (* rotation not allowed? *)
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
    gScreenWidth := closest.w;
    gScreenHeight := closest.h;
    //gBPP := SDL_BITSPERPIXEL(closest.format); (* Resolution list didn't work for some reason *)
    gBPP := 32;
    gFullScreen := False;
  {$ENDIF}
  (* Must be positioned on primary display *)
  gWinRealPosX := SDL_WINDOWPOS_CENTERED;
  gWinRealPosY := SDL_WINDOWPOS_CENTERED;
  gWinMaximized := False;
  gVSync := True;
  gTextureFilter := True;
  glLegacyNPOT := False;
  e_LogWriteLn('g_Options_SetDefaultVideo: w = ' + IntToStr(gScreenWidth) + ' h = ' + IntToStr(gScreenHeight));
  g_Console_ResetBinds;
end;

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
  ITEM_RESPAWNTIME := 60 * 36;
  gBloodCount := 4;
  gAdvBlood := True;
  gAdvCorpses := True;
  gAdvGibs := True;
  gFlash := 1;
  gDrawBackGround := True;
  gShowMessages := True;
  gRevertPlayers := False;
  gChatBubble := 4;
  gSFSDebug := False;
  gSFSFastMode := False;
  e_FastScreenshots := True;
  gDefaultMegawadStart := DF_Default_Megawad_Start;
  gBerserkAutoswitch := True;
  g_dbg_scale := 1.0;

  gAskLanguage := True;
  gLanguage := LANGUAGE_ENGLISH;

  (* section GameplayCustom *)
  gcMap := '';
  gcGameMode := _lc[I_MENU_GAME_TYPE_DM];
  gcTimeLimit := 0;
  gcGoalLimit := 0;
  gcMaxLives := 0;
  gcPlayers := 1;
  gcTeamDamage := False;
  gcAllowExit := True;
  gcWeaponStay := False;
  gcMonsters := False;
  gcBotsVS := 'Everybody';

  (* section GameplayNetwork *)
  gnMap := '';
  gnGameMode := _lc[I_MENU_GAME_TYPE_DM];
  gnTimeLimit := 0;
  gnGoalLimit := 0;
  gnMaxLives := 0;
  gnPlayers := 1;
  gnTeamDamage := False;
  gnAllowExit := True;
  gnWeaponStay := False;
  gnMonsters := False;
  gnBotsVS := 'Everybody';

  (* section MasterServer *)
  NetSlistIP := 'mpms.doom2d.org';
  NetSlistPort := 25665;
  g_Net_Slist_Set(NetSlistIP, NetSlistPort);

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

procedure g_Options_Read(FileName: String);
var
  i: Integer;
  config: TConfig;
  section: String;
  
  procedure ReadInteger (VAR v: Integer; param: String; minv: Integer = Low(Integer); maxv: Integer = High(Integer));
  begin
    v := Max(Min(config.ReadInt(section, param, v), maxv), minv)
  end;

  procedure ReadInteger (VAR v: LongWord; param: String; minv: LongWord = Low(LongWord); maxv: LongWord = High(LongWord)); overload;
  begin
    v := Max(Min(config.ReadInt(section, param, v), maxv), minv)
  end;

  procedure ReadInteger (VAR v: Word; param: String; minv: Word = Low(Word); maxv: Word = High(Word)); overload;
  begin
    v := Max(Min(config.ReadInt(section, param, v), maxv), minv)
  end;

  procedure ReadInteger (VAR v: Byte; param: String; minv: Byte = Low(Byte); maxv: Byte = High(Byte)); overload;
  begin
    v := Max(Min(config.ReadInt(section, param, v), maxv), minv)
  end;

  procedure ReadBoolean (VAR v: Boolean; param: String);
  begin
    v := config.ReadBool(section, param, v)
  end;

  procedure ReadString (VAR v: String; param: String);
  begin
    v := config.ReadStr(section, param, v)
  end;

begin
  gAskLanguage := True;
  e_WriteLog('Reading config', TMsgType.Notify);
  g_Options_SetDefault;

  if FileExists(FileName) = False then
  begin
    e_WriteLog('Config file '+FileName+' not found', TMsgType.Warning);
    g_Options_SetDefaultVideo;
    Exit
  end;

  config := TConfig.CreateFile(FileName);

  section := 'Video';
  ReadInteger(gScreenWidth, 'ScreenWidth', 0);
  ReadInteger(gScreenHeight, 'ScreenHeight', 0);
  ReadInteger(gWinRealPosX, 'WinPosX', 60);
  ReadInteger(gWinRealPosY, 'WinPosY', 60);
  ReadBoolean(gFullScreen, 'Fullscreen');
  ReadBoolean(gWinMaximized, 'Maximized');
  ReadInteger(gBPP, 'BPP', 0);
  ReadInteger(gFreq, 'Freq', 0);
  ReadBoolean(gVSync, 'VSync');
  ReadBoolean(gTextureFilter, 'TextureFilter');
  ReadBoolean(glLegacyNPOT, 'LegacyCompatible');

  section := 'Sound';
  ReadBoolean(gNoSound, 'NoSound');
  ReadInteger(gSoundLevel, 'SoundLevel', 0, 255);
  ReadInteger(gMusicLevel, 'MusicLevel', 0, 255);
  ReadInteger(gMaxSimSounds, 'MaxSimSounds', 2, 66);
  ReadBoolean(gMuteWhenInactive, 'MuteInactive');
  ReadInteger(gAnnouncer, 'Announcer', ANNOUNCE_NONE, ANNOUNCE_ALL);
  ReadBoolean(gSoundEffectsDF, 'SoundEffectsDF');
  ReadBoolean(gUseChatSounds, 'ChatSounds');
  ReadInteger(gsSDLSampleRate, 'SDLSampleRate', 11025, 96000);
  ReadInteger(gsSDLBufferSize, 'SDLBufferSize', 64, 16384);

  section := 'Player1';
  with gPlayer1Settings do
  begin
    ReadString(Name, 'name');
    ReadString(Model, 'model');
    ReadInteger(Color.R, 'red', 0, 255);
    ReadInteger(Color.G, 'green', 0, 255);
    ReadInteger(Color.B, 'blue', 0, 255);
    ReadInteger(Team, 'team');
    if (Team < TEAM_RED) or (Team > TEAM_BLUE) then
      Team := TEAM_RED;
  end;

  section := 'Player2';
  with gPlayer2Settings do
  begin
    ReadString(Name, 'name');
    ReadString(Model, 'model');
    ReadInteger(Color.R, 'red', 0, 255);
    ReadInteger(Color.G, 'green', 0, 255);
    ReadInteger(Color.B, 'blue', 0, 255);
    ReadInteger(Team, 'team');
    if (Team < TEAM_RED) or (Team > TEAM_BLUE) then
      Team := TEAM_RED;
  end;

  section := 'Joysticks';
  for i := 0 to e_MaxJoys - 1 do
  begin
    ReadInteger(e_JoystickDeadzones[i], 'Deadzone' + IntToStr(i))
  end;

  section := 'Game';
  ReadInteger(i, 'MaxParticles', 0, 50000); g_GFX_SetMax(i);
  ReadInteger(i, 'MaxShells', 0, 600); g_Shells_SetMax(i);
  ReadInteger(i, 'MaxGibs', 0, 500); g_Gibs_SetMax(i);
  ReadInteger(i, 'MaxCorpses', 0, 100); g_Corpses_SetMax(i);
  ReadInteger(i, 'GibsCount');
  case i of
    0: gGibsCount := 0;
    1: gGibsCount := 8;
    2: gGibsCount := 16;
    3: gGibsCount := 32;
    else gGibsCount := 48;
  end;
  i := ITEM_RESPAWNTIME div 36; ReadInteger(i, 'ItemRespawnTime', 0); ITEM_RESPAWNTIME := i * 36;
  ReadInteger(gBloodCount, 'BloodCount', 0, 4);
  ReadBoolean(gAdvBlood, 'AdvancesBlood');
  ReadBoolean(gAdvCorpses, 'AdvancesCorpses');
  ReadBoolean(gAdvGibs, 'AdvancesGibs');
  ReadInteger(gFlash, 'Flash', 0, 2);
  ReadBoolean(gDrawBackGround, 'BackGround');
  ReadBoolean(gShowMessages, 'Messages');
  ReadBoolean(gRevertPlayers, 'RevertPlayers');
  ReadInteger(gChatBubble, 'ChatBubble', 0, 4);
  ReadBoolean(gSFSDebug, 'SFSDebug'); wadoptDebug := gSFSDebug;
  ReadBoolean(gSFSFastMode, 'SFSFastMode'); wadoptFast := gSFSFastMode;
  ReadBoolean(e_FastScreenshots, 'FastScreenshots');
  ReadString(gDefaultMegawadStart, 'DefaultMegawadStart');
  ReadBoolean(gBerserkAutoswitch, 'BerserkAutoswitching');
  i := Trunc(g_dbg_scale * 100); ReadInteger(i, 'Scale', 100); g_dbg_scale := i / 100;
  ReadString(gLanguage, 'Language');
  if (gLanguage = LANGUAGE_RUSSIAN) or (gLanguage = LANGUAGE_ENGLISH) then
    gAskLanguage := False
  else
    gLanguage := LANGUAGE_ENGLISH;

  section := 'GameplayCustom';
  ReadString(gcMap, 'Map');
  ReadString(gcGameMode, 'GameMode');
  ReadInteger(gcTimeLimit, 'TimeLimit', 0, 65535);
  ReadInteger(gcGoalLimit, 'GoalLimit', 0, 65535);
  ReadInteger(gcMaxLives, 'MaxLives', 0, 255);
  ReadInteger(gcPlayers, 'Players', 0, 2);
  ReadBoolean(gcTeamDamage, 'TeamDamage');
  ReadBoolean(gcAllowExit, 'AllowExit');
  ReadBoolean(gcWeaponStay, 'WeaponStay');
  ReadBoolean(gcMonsters, 'Monsters');
  ReadString(gcBotsVS, 'BotsVS');

  with gGameSettings do
  begin
    GameMode := g_Game_TextToMode(gcGameMode);
    if GameMode = GM_NONE then
      GameMode := GM_DM;
    if GameMode = GM_SINGLE then
      GameMode := GM_COOP;
    TimeLimit := gcTimeLimit;
    GoalLimit := gcGoalLimit;
    MaxLives := gcMaxLives;

    Options := 0;
    if gcTeamDamage then
      Options := Options or GAME_OPTION_TEAMDAMAGE;
    if gcAllowExit then
      Options := Options or GAME_OPTION_ALLOWEXIT;
    if gcWeaponStay then
      Options := Options or GAME_OPTION_WEAPONSTAY;
    if gcMonsters then
      Options := Options or GAME_OPTION_MONSTERS;
    if gcBotsVS = 'Everybody' then
      Options := Options or GAME_OPTION_BOTVSPLAYER or GAME_OPTION_BOTVSMONSTER;
    if gcBotsVS = 'Players' then
      Options := Options or GAME_OPTION_BOTVSPLAYER;
    if gcBotsVS = 'Monsters' then
      Options := Options or GAME_OPTION_BOTVSMONSTER;
  end;

  section := 'GameplayNetwork';
  ReadString(gnMap, 'Map');
  ReadString(gnGameMode, 'GameMode');
  ReadInteger(gnTimeLimit, 'TimeLimit', 0, 65535);
  ReadInteger(gnGoalLimit, 'GoalLimit', 0, 65535);
  ReadInteger(gnMaxLives, 'MaxLives', 0, 255);
  ReadInteger(gnPlayers, 'Players', 0, 2);
  ReadBoolean(gnTeamDamage, 'TeamDamage');
  ReadBoolean(gnAllowExit, 'AllowExit');
  ReadBoolean(gnWeaponStay, 'WeaponStay');
  ReadBoolean(gnMonsters, 'Monsters');
  ReadString(gnBotsVS, 'BotsVS');

  section := 'MasterServer';
  ReadString(NetSlistIP, 'IP');
  ReadInteger(NetSlistPort, 'Port', 0, 65535);
  g_Net_Slist_Set(NetSlistIP, NetSlistPort);

  section := 'Server';
  ReadString(NetServerName, 'Name');
  ReadString(NetPassword, 'Password');
  ReadInteger(NetPort, 'Port', 0, 65535);
  ReadInteger(NetMaxClients, 'MaxClients', 0, NET_MAXCLIENTS);
  ReadBoolean(NetAllowRCON, 'RCON');
  ReadString(NetRCONPassword, 'RCONPassword');
  ReadBoolean(NetUseMaster, 'SyncWithMaster');
  ReadInteger(NetUpdateRate, 'UpdateInterval', 0);
  ReadInteger(NetRelupdRate, 'ReliableUpdateInterval', 0);
  ReadInteger(NetMasterRate, 'MasterSyncInterval', 1);
  ReadBoolean(NetForwardPorts, 'ForwardPorts');

  section := 'Client';
  ReadInteger(NetInterpLevel, 'InterpolationSteps', 0);
  ReadBoolean(NetForcePlayerUpdate, 'ForcePlayerUpdate');
  ReadBoolean(NetPredictSelf, 'PredictSelf');
  ReadString(NetClientIP, 'LastIP');
  ReadInteger(NetClientPort, 'LastPort', 0, 65535);

  config.Free();

  //if gTextureFilter then TEXTUREFILTER := GL_LINEAR else TEXTUREFILTER := GL_NEAREST;
end;

procedure g_Options_Write(FileName: String);
var
  config: TConfig;
  i: Integer;
begin
  e_WriteLog('Writing config', TMsgType.Notify);

  config := TConfig.CreateFile(FileName);

  config.WriteInt('Video', 'ScreenWidth', gScreenWidth);
  config.WriteInt('Video', 'ScreenHeight', gScreenHeight);
  config.WriteInt('Video', 'WinPosX', gWinRealPosX);
  config.WriteInt('Video', 'WinPosY', gWinRealPosY);
  config.WriteBool('Video', 'Fullscreen', gFullScreen);
  config.WriteBool('Video', 'Maximized', gWinMaximized);
  config.WriteInt('Video', 'BPP', gBPP);
  config.WriteBool('Video', 'VSync', gVSync);
  config.WriteBool('Video', 'TextureFilter', gTextureFilter);
  config.WriteBool('Video', 'LegacyCompatible', glLegacyNPOT);

  config.WriteBool('Sound', 'NoSound', gNoSound);
  config.WriteInt('Sound', 'SoundLevel', gSoundLevel);
  config.WriteInt('Sound', 'MusicLevel', gMusicLevel);
  config.WriteInt('Sound', 'MaxSimSounds', gMaxSimSounds);
  config.WriteBool('Sound', 'MuteInactive', gMuteWhenInactive);
  config.WriteInt('Sound', 'Announcer', gAnnouncer);
  config.WriteBool('Sound', 'SoundEffectsDF', gSoundEffectsDF);
  config.WriteBool('Sound', 'ChatSounds', gUseChatSounds);
  config.WriteInt('Sound', 'SDLSampleRate', gsSDLSampleRate);
  config.WriteInt('Sound', 'SDLBufferSize', gsSDLBufferSize);

  with config, gPlayer1Settings do
  begin
    WriteStr('Player1', 'Name', Name);
    WriteStr('Player1', 'model', Model);
    WriteInt('Player1', 'red', Color.R);
    WriteInt('Player1', 'green', Color.G);
    WriteInt('Player1', 'blue', Color.B);
    WriteInt('Player1', 'team', Team);
  end;

  with config, gPlayer2Settings do
  begin
    WriteStr('Player2', 'Name', Name);
    WriteStr('Player2', 'model', Model);
    WriteInt('Player2', 'red', Color.R);
    WriteInt('Player2', 'green', Color.G);
    WriteInt('Player2', 'blue', Color.B);
    WriteInt('Player2', 'team', Team);
  end;

  for i := 0 to e_MaxJoys-1 do
    config.WriteInt('Joysticks', 'Deadzone' + IntToStr(i), e_JoystickDeadzones[i]);

  with config do
    case gGibsCount of
      0: config.WriteInt('Game', 'GibsCount', 0);
      8: config.WriteInt('Game', 'GibsCount', 1);
      16: config.WriteInt('Game', 'GibsCount', 2);
      32: config.WriteInt('Game', 'GibsCount', 3);
      else config.WriteInt('Game', 'GibsCount', 4);
    end;

  config.WriteInt('Game', 'ItemRespawnTime', ITEM_RESPAWNTIME div 36);
  config.WriteInt('Game', 'MaxParticles', g_GFX_GetMax());
  config.WriteInt('Game', 'MaxShells', g_Shells_GetMax());
  config.WriteInt('Game', 'MaxGibs', g_Gibs_GetMax());
  config.WriteInt('Game', 'MaxCorpses', g_Corpses_GetMax());
  config.WriteInt('Game', 'BloodCount', gBloodCount);
  config.WriteBool('Game', 'AdvancesBlood', gAdvBlood);
  config.WriteBool('Game', 'AdvancesCorpses', gAdvCorpses);
  config.WriteBool('Game', 'AdvancesGibs', gAdvGibs);
  config.WriteInt('Game', 'Flash', gFlash);
  config.WriteBool('Game', 'BackGround', gDrawBackGround);
  config.WriteBool('Game', 'Messages', gShowMessages);
  config.WriteBool('Game', 'RevertPlayers', gRevertPlayers);
  config.WriteInt('Game', 'ChatBubble', gChatBubble);
  config.WriteBool('Game', 'SFSDebug', gSFSDebug);
  config.WriteBool('Game', 'SFSFastMode', gSFSFastMode);
  config.WriteBool('Game', 'FastScreenshots', e_FastScreenshots);
  config.WriteStr('Game', 'DefaultMegawadStart', gDefaultMegawadStart);
  config.WriteBool('Game', 'BerserkAutoswitching', gBerserkAutoswitch);
  config.WriteInt('Game', 'Scale', Round(g_dbg_scale * 100));

  config.WriteStr ('GameplayCustom', 'Map', gcMap);
  config.WriteStr ('GameplayCustom', 'GameMode', gcGameMode);
  config.WriteInt ('GameplayCustom', 'TimeLimit', gcTimeLimit);
  config.WriteInt ('GameplayCustom', 'GoalLimit', gcGoalLimit);
  config.WriteInt ('GameplayCustom', 'MaxLives', gcMaxLives);
  config.WriteInt ('GameplayCustom', 'Players', gcPlayers);
  config.WriteBool('GameplayCustom', 'TeamDamage', gcTeamDamage);
  config.WriteBool('GameplayCustom', 'AllowExit', gcAllowExit);
  config.WriteBool('GameplayCustom', 'WeaponStay', gcWeaponStay);
  config.WriteBool('GameplayCustom', 'Monsters', gcMonsters);
  config.WriteStr ('GameplayCustom', 'BotsVS', gcBotsVS);

  config.WriteStr ('GameplayNetwork', 'Map', gnMap);
  config.WriteStr ('GameplayNetwork', 'GameMode', gnGameMode);
  config.WriteInt ('GameplayNetwork', 'TimeLimit', gnTimeLimit);
  config.WriteInt ('GameplayNetwork', 'GoalLimit', gnGoalLimit);
  config.WriteInt ('GameplayNetwork', 'MaxLives', gnMaxLives);
  config.WriteInt ('GameplayNetwork', 'Players', gnPlayers);
  config.WriteBool('GameplayNetwork', 'TeamDamage', gnTeamDamage);
  config.WriteBool('GameplayNetwork', 'AllowExit', gnAllowExit);
  config.WriteBool('GameplayNetwork', 'WeaponStay', gnWeaponStay);
  config.WriteBool('GameplayNetwork', 'Monsters', gnMonsters);
  config.WriteStr ('GameplayNetwork', 'BotsVS', gnBotsVS);

  config.WriteStr('MasterServer', 'IP', NetSlistIP);
  config.WriteInt('MasterServer', 'Port', NetSlistPort);

  config.WriteStr ('Server', 'Name', NetServerName);
  config.WriteStr ('Server', 'Password', NetPassword);
  config.WriteInt ('Server', 'Port', NetPort);
  config.WriteInt ('Server', 'MaxClients', NetMaxClients);
  config.WriteBool('Server', 'RCON', NetAllowRCON);
  config.WriteStr ('Server', 'RCONPassword', NetRCONPassword);
  config.WriteBool('Server', 'SyncWithMaster', NetUseMaster);
  config.WriteBool('Server', 'ForwardPorts', NetForwardPorts);
  config.WriteInt ('Server', 'UpdateInterval', NetUpdateRate);
  config.WriteInt ('Server', 'ReliableUpdateInterval', NetRelupdRate);
  config.WriteInt ('Server', 'MasterSyncInterval', NetMasterRate);

  config.WriteInt  ('Client', 'InterpolationSteps', NetInterpLevel);
  config.WriteBool ('Client', 'ForcePlayerUpdate', NetForcePlayerUpdate);
  config.WriteBool ('Client', 'PredictSelf', NetPredictSelf);
  config.WriteStr  ('Client', 'LastIP', NetClientIP);
  config.WriteInt  ('Client', 'LastPort', NetClientPort);

  config.SaveFile(FileName);
  config.Free();
end;

procedure g_Options_Write_Language(FileName: String);
var
  config: TConfig;
begin
  e_WriteLog('Writing language config', TMsgType.Notify);

  config := TConfig.CreateFile(FileName);
  config.WriteStr('Game', 'Language', gLanguage);
  config.SaveFile(FileName);
  config.Free();
end;

procedure g_Options_Write_Video(FileName: String);
var
  config: TConfig;
  sW, sH: Integer;
begin
  e_WriteLog('Writing resolution to config', TMsgType.Notify);

  config := TConfig.CreateFile(FileName);

  if gWinMaximized and (not gFullscreen) then
    begin
      sW := gWinSizeX;
      sH := gWinSizeY;
    end
  else
    begin
      sW := gScreenWidth;
      sH := gScreenHeight;
    end;
  e_LogWritefln('  (ws=%dx%d) (ss=%dx%d)', [gWinSizeX, gWinSizeY, gScreenWidth, gScreenHeight]);

  config.WriteInt('Video', 'ScreenWidth', sW);
  config.WriteInt('Video', 'ScreenHeight', sH);
  config.WriteInt('Video', 'WinPosX', gWinRealPosX);
  config.WriteInt('Video', 'WinPosY', gWinRealPosY);
  config.WriteBool('Video', 'Fullscreen', gFullscreen);
  config.WriteBool('Video', 'Maximized', gWinMaximized);

  config.WriteStr('Player1', 'Name', gPlayer1Settings.Name);
  config.WriteStr('Player2', 'Name', gPlayer2Settings.Name);

  config.SaveFile(FileName);
  config.Free();
end;

procedure g_Options_Write_Gameplay_Custom(FileName: String);
var
  config: TConfig;
begin
  e_WriteLog('Writing custom gameplay config', TMsgType.Notify);

  config := TConfig.CreateFile(FileName);

  config.WriteStr ('GameplayCustom', 'Map', gcMap);
  config.WriteStr ('GameplayCustom', 'GameMode', gcGameMode);
  config.WriteInt ('GameplayCustom', 'TimeLimit', gcTimeLimit);
  config.WriteInt ('GameplayCustom', 'GoalLimit', gcGoalLimit);
  config.WriteInt ('GameplayCustom', 'MaxLives', gcMaxLives);
  config.WriteInt ('GameplayCustom', 'Players', gcPlayers);
  config.WriteBool('GameplayCustom', 'TeamDamage', gcTeamDamage);
  config.WriteBool('GameplayCustom', 'AllowExit', gcAllowExit);
  config.WriteBool('GameplayCustom', 'WeaponStay', gcWeaponStay);
  config.WriteBool('GameplayCustom', 'Monsters', gcMonsters);
  config.WriteStr ('GameplayCustom', 'BotsVS', gcBotsVS);

  config.SaveFile(FileName);
  config.Free();
end;

procedure g_Options_Write_Gameplay_Net(FileName: String);
var
  config: TConfig;
begin
  e_WriteLog('Writing network gameplay config', TMsgType.Notify);

  config := TConfig.CreateFile(FileName);

  config.WriteStr ('GameplayNetwork', 'Map', gnMap);
  config.WriteStr ('GameplayNetwork', 'GameMode', gnGameMode);
  config.WriteInt ('GameplayNetwork', 'TimeLimit', gnTimeLimit);
  config.WriteInt ('GameplayNetwork', 'GoalLimit', gnGoalLimit);
  config.WriteInt ('GameplayNetwork', 'MaxLives', gnMaxLives);
  config.WriteInt ('GameplayNetwork', 'Players', gnPlayers);
  config.WriteBool('GameplayNetwork', 'TeamDamage', gnTeamDamage);
  config.WriteBool('GameplayNetwork', 'AllowExit', gnAllowExit);
  config.WriteBool('GameplayNetwork', 'WeaponStay', gnWeaponStay);
  config.WriteBool('GameplayNetwork', 'Monsters', gnMonsters);
  config.WriteStr ('GameplayNetwork', 'BotsVS', gnBotsVS);

  config.SaveFile(FileName);
  config.Free();
end;

procedure g_Options_Write_Net_Server(FileName: String);
var
  config: TConfig;
begin
  e_WriteLog('Writing server config', TMsgType.Notify);

  config := TConfig.CreateFile(FileName);

  config.WriteStr ('Server', 'Name', NetServerName);
  config.WriteStr ('Server', 'Password', NetPassword);
  config.WriteInt ('Server', 'Port', NetPort);
  config.WriteInt ('Server', 'MaxClients', NetMaxClients);
  config.WriteBool('Server', 'SyncWithMaster', NetUseMaster);
  config.WriteBool('Server', 'ForwardPorts', NetForwardPorts);

  config.SaveFile(FileName);
  config.Free();
end;

procedure g_Options_Write_Net_Client(FileName: String);
var
  config: TConfig;
begin
  e_WriteLog('Writing client config', TMsgType.Notify);

  config := TConfig.CreateFile(FileName);

  config.WriteStr('Client', 'LastIP', NetClientIP);
  config.WriteInt('Client', 'LastPort', NetClientPort);

  config.SaveFile(FileName);
  config.Free();
end;

initialization
  Randomize;
  machine := Random(10000)
end.
