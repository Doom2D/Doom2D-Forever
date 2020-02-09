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
procedure g_Options_Read(FileName: String);
procedure g_Options_Write(FileName: String);
procedure g_Options_Write_Gameplay_Custom(FileName: String);
procedure g_Options_Write_Gameplay_Net(FileName: String);
procedure g_Options_Write_Net_Server(FileName: String);
procedure g_Options_Write_Net_Client(FileName: String);
procedure g_Options_Commands (p: SSArray);

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
  gcDeathmatchKeys: Boolean = True;
  gcRespawnItems: Boolean = True;
  gcSpawnInvul: Integer = 0;
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
  gnDeathmatchKeys: Boolean = True;
  gnRespawnItems: Boolean = True;
  gnSpawnInvul: Integer = 0;
  gsSDLSampleRate: Integer;
  gsSDLBufferSize: Integer;
  gDefaultMegawadStart: AnsiString;
  gBerserkAutoswitch: Boolean;
  glNPOTOverride: Boolean = false;

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
      Result := Copy(Result, 1, 12) + ' '
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
  //gBPP := SDL_BITSPERPIXEL(dispaly.format);
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
  wadoptDebug := False;
  wadoptFast := False;
  e_FastScreenshots := True;
  gDefaultMegawadStart := DF_Default_Megawad_Start;
  gBerserkAutoswitch := True;
  g_dbg_scale := 1.0;
  gSaveStats := False;

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
  gcDeathmatchKeys := True;
  gcRespawnItems := True;
  gcSpawnInvul := 0;

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
  gnDeathmatchKeys := True;
  gnRespawnItems := True;
  gnSpawnInvul := 0;

  (* section MasterServer *)
  NetSlistIP := 'mpms.doom2d.org';
  NetSlistPort := 25665;
  g_Net_Slist_Set(NetSlistIP, NetSlistPort, NetSlistList);

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
  ReadBoolean(gcDeathmatchKeys, 'DeathmatchKeys');
  ReadBoolean(gcRespawnItems, 'RespawnItems');
  ReadInteger(gcSpawnInvul, 'SpawnInvul');

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
    if gcDeathmatchKeys then
      Options := Options or GAME_OPTION_DMKEYS;
    if gcRespawnItems then
      Options := Options or GAME_OPTION_RESPAWNITEMS;
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
  ReadBoolean(gnDeathmatchKeys, 'DeathmatchKeys');
  ReadBoolean(gnRespawnItems, 'RespawnItems');
  ReadInteger(gnSpawnInvul, 'SpawnInvul');

  section := 'MasterServer';
  ReadString(NetSlistIP, 'IP');
  ReadInteger(NetSlistPort, 'Port', 0, 65535);
  ReadString(NetSlistList, 'List');
  g_Net_Slist_Set(NetSlistIP, NetSlistPort, NetSlistList);

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
  var config: TConfig;
begin
  e_WriteLog('Writing config', TMsgType.Notify);

  config := TConfig.CreateFile(FileName);

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
  config.WriteBool('GameplayCustom', 'DeathmatchKeys', gcDeathmatchKeys);
  config.WriteBool('GameplayCustom', 'RespawnItems', gcRespawnItems);
  config.WriteInt ('GameplayCustom', 'SpawnInvul', gcSpawnInvul);

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
  config.WriteBool('GameplayNetwork', 'DeathmatchKeys', gnDeathmatchKeys);
  config.WriteBool('GameplayNetwork', 'RespawnItems', gnRespawnItems);
  config.WriteInt ('GameplayNetwork', 'SpawnInvul', gnSpawnInvul);

  config.WriteStr('MasterServer', 'IP', NetSlistIP);
  config.WriteInt('MasterServer', 'Port', NetSlistPort);
  config.WriteStr('MasterServer', 'List', NetSlistList);

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
  config.WriteBool('GameplayCustom', 'DeathmatchKeys', gcDeathmatchKeys);
  config.WriteBool('GameplayCustom', 'RespawnItems', gcRespawnItems);
  config.WriteInt ('GameplayCustom', 'SpawnInvul', gcSpawnInvul);

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
  config.WriteBool('GameplayNetwork', 'DeathmatchKeys', gnDeathmatchKeys);
  config.WriteBool('GameplayNetwork', 'RespawnItems', gnRespawnItems);
  config.WriteInt ('GameplayNetwork', 'SpawnInvul', gnSpawnInvul);

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

procedure g_Options_Commands (p: SSArray);
  var cmd: AnsiString; i: Integer;
begin
  cmd := LowerCase(p[0]);
  case cmd of
    'r_reset':
      begin
        sys_EnableVSync(gVSync);
        gRC_Width := Max(1, gRC_Width);
        gRC_Height := Max(1, gRC_Height);
        gBPP := Max(1, gBPP);
        if sys_SetDisplayMode(gRC_Width, gRC_Height, gBPP, gRC_FullScreen, gRC_Maximized) = True then
          e_LogWriteln('resolution changed')
        else
          e_LogWriteln('resolution not changed')
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
          g_Language_Set(gLanguage)
        end
        else
        begin
          e_LogWritefln('usage: %s <English|Russian|Ask>', [cmd])
        end
      end;
    'g_max_particles':
      begin
        if Length(p) = 2 then
        begin
          i := Max(0, StrToInt(p[1]));
          g_GFX_SetMax(i)
        end
        else if Length(p) = 1 then
        begin
          e_LogWritefln('%s', [g_GFX_GetMax()])
        end
        else
        begin
          e_LogWritefln('usage: %s <n>', [cmd])
        end
      end;
    'g_max_shells':
      begin
        if Length(p) = 2 then
        begin
          i := Max(0, StrToInt(p[1]));
          g_Shells_SetMax(i)
        end
        else if Length(p) = 1 then
        begin
          e_LogWritefln('%s', [g_Shells_GetMax()])
        end
        else
        begin
          e_LogWritefln('usage: %s <n>', [cmd])
        end
      end;
    'g_max_gibs':
      begin
        if Length(p) = 2 then
        begin
          i := Max(0, StrToInt(p[1]));
          g_Gibs_SetMax(i)
        end
        else if Length(p) = 1 then
        begin
          e_LogWritefln('%s', [g_Gibs_GetMax()])
        end
        else
        begin
          e_LogWritefln('usage: %s <n>', [cmd])
        end
      end;
    'g_max_corpses':
      begin
        if Length(p) = 2 then
        begin
          i := Max(0, StrToInt(p[1]));
          g_Corpses_SetMax(i)
        end
        else if Length(p) = 1 then
        begin
          e_LogWritefln('%s', [g_Corpses_GetMax()])
        end
        else
        begin
          e_LogWritefln('usage: %s <n>', [cmd])
        end
      end;
    'g_item_respawn_time':
      begin
        if Length(p) = 2 then
          ITEM_RESPAWNTIME := Max(0, StrToInt(p[1])) * 36
        else if Length(p) = 1 then
          e_LogWritefln('%s', [ITEM_RESPAWNTIME div 36])
        else
          e_LogWritefln('usage: %s <n>', [cmd])
      end;
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
  conRegVar('r_fbo', @glRenderToFBO, '', '');

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
end.
