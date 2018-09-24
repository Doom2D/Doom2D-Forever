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
unit g_options;

interface

uses
  g_language, g_weapons;

type
  TPlayerControl = record
    KeyRight:      Word;
    KeyLeft:       Word;
    KeyUp:         Word;
    KeyDown:       Word;
    KeyFire:       Word;
    KeyJump:       Word;
    KeyNextWeapon: Word;
    KeyPrevWeapon: Word;
    KeyOpen:       Word;
    KeyStrafe:     Word;
    KeyWeapon:     array [WP_FIRST..WP_LAST] of Word;

    KeyRight2:      Word;
    KeyLeft2:       Word;
    KeyUp2:         Word;
    KeyDown2:       Word;
    KeyFire2:       Word;
    KeyJump2:       Word;
    KeyNextWeapon2: Word;
    KeyPrevWeapon2: Word;
    KeyOpen2:       Word;
    KeyStrafe2:     Word;
    KeyWeapon2:     array [WP_FIRST..WP_LAST] of Word;
  end;

  TGameControls = record
    TakeScreenshot: Word;
    Stat:           Word;
    Chat:           Word;
    TeamChat:       Word;
  end;

  TControls = record
    GameControls: TGameControls;
    P1Control:    TPlayerControl;
    P2Control:    TPlayerControl;
  end;

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
  gGameControls: TControls;
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
  e_log, e_input, g_window, g_sound, g_gfx, g_player, Math,
  g_map, g_net, g_netmaster, SysUtils, CONFIG, g_game, g_main, e_texture,
  g_items, wadreader, e_graphics, g_touch, SDL2;

procedure g_Options_SetDefaultVideo;
var
  target, closest, display: TSDL_DisplayMode;
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
    target.w := display.w * 75 div 100;
    target.h := display.h * 75 div 100;
    target.format := 0; (* didn't care *)
    target.refresh_rate := 0; (* didn't care *)
    target.driverdata := nil; (* init *)
    SDL_GetClosestDisplayMode(0, @target, @closest);
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

  (* section GameControls *)
  with gGameControls.GameControls do
  begin
    TakeScreenshot := SDL_SCANCODE_F12;
    Stat := SDL_SCANCODE_TAB;
    Chat := SDL_SCANCODE_T;
    TeamChat := SDL_SCANCODE_Y;
  end;

  (* section Player1 *)
  with gGameControls.P1Control do
  begin
    KeyRight := SDL_SCANCODE_KP_6;
    KeyLeft := SDL_SCANCODE_KP_4;
    KeyUp := SDL_SCANCODE_KP_8;
    KeyDown := SDL_SCANCODE_KP_5;
    KeyFire := SDL_SCANCODE_SLASH;
    KeyJump := SDL_SCANCODE_RCTRL;
    KeyNextWeapon := SDL_SCANCODE_KP_9;
    KeyPrevWeapon := SDL_SCANCODE_KP_7;
    KeyOpen := SDL_SCANCODE_RSHIFT;
    KeyStrafe := SDL_SCANCODE_PERIOD;

    for i := 0 to 9 do
    begin
      KeyWeapon[i] := SDL_SCANCODE_1 + i (* 1, ..., 9, 0 *)
    end;
    KeyWeapon[10] := SDL_SCANCODE_MINUS;
    for i := 11 to High(KeyWeapon) do
    begin
      KeyWeapon[i] := 0
    end;

    KeyRight2 := VK_RIGHT;
    KeyLeft2 := VK_LEFT;
    KeyUp2 := VK_UP;
    KeyDown2 := VK_DOWN;
    KeyFire2 := VK_FIRE;
    KeyJump2 := VK_JUMP;
    KeyNextWeapon2 := VK_NEXT;
    KeyPrevWeapon2 := VK_PREV;
    KeyOpen2 := VK_OPEN;
    KeyStrafe2 := VK_STRAFE;

    for i := 0 to High(KeyWeapon2) do
    begin
      KeyWeapon2[i] := VK_0 + i
    end;
  end;

  with gPlayer1Settings do
  begin
    Name := 'Player1';
    Model := STD_PLAYER_MODEL;
    Color.R := PLAYER1_DEF_COLOR.R;
    Color.G := PLAYER1_DEF_COLOR.G;
    Color.B := PLAYER1_DEF_COLOR.B;
    Team := TEAM_RED;
  end;

  (* section Player2 *)
  with gGameControls.P2Control do
  begin
    KeyRight := SDL_SCANCODE_D;
    KeyLeft := SDL_SCANCODE_A;
    KeyUp := SDL_SCANCODE_W;
    KeyDown := SDL_SCANCODE_S;
    KeyFire := SDL_SCANCODE_G;
    KeyJump := SDL_SCANCODE_SPACE;
    KeyNextWeapon := SDL_SCANCODE_E;
    KeyPrevWeapon := SDL_SCANCODE_Q;
    KeyOpen := SDL_SCANCODE_F;
    KeyStrafe := SDL_SCANCODE_LSHIFT;
    for i := 0 to High(KeyWeapon) do
    begin
      KeyWeapon[i] := 0
    end;

    KeyRight2 := 0;
    KeyLeft2 := 0;
    KeyUp2 := 0;
    KeyDown2 := 0;
    KeyFire2 := 0;
    KeyJump2 := 0;
    KeyNextWeapon2 := 0;
    KeyPrevWeapon2 := 0;
    KeyOpen2 := 0;
    KeyStrafe2 := 0;

    for i := 0 to High(KeyWeapon2) do
    begin
      KeyWeapon2[i] := 0;
    end
  end;

  with gPlayer2Settings do
  begin
    Name := 'Player2';
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

  (* section Touch *)
  g_touch_size := 1.0;
  g_touch_fire := True;
  g_touch_offset := 50;
  g_touch_alt := False;
  
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

  section := 'GameControls';
  with gGameControls.GameControls do
  begin
    ReadInteger(TakeScreenshot, 'TakeScreenshot');
    ReadInteger(Stat, 'Stat');
    ReadInteger(Chat, 'Chat');
    ReadInteger(TeamChat, 'TeamChat');
  end;

  section := 'Player1';
  with gGameControls.P1Control do
  begin
    ReadInteger(KeyRight, 'KeyRight');
    ReadInteger(KeyLeft, 'KeyLeft');
    ReadInteger(KeyUp, 'KeyUp');
    ReadInteger(KeyDown, 'KeyDown');
    ReadInteger(KeyFire, 'KeyFire');
    ReadInteger(KeyJump, 'KeyJump');
    ReadInteger(KeyNextWeapon, 'KeyNextWeapon');
    ReadInteger(KeyPrevWeapon, 'KeyPrevWeapon');
    ReadInteger(KeyOpen, 'KeyOpen');
    ReadInteger(KeyStrafe, 'KeyStrafe');

    for i := 0 to High(KeyWeapon) do
    begin
      ReadInteger(KeyWeapon[i], 'KeyWeapon' + IntToStr(i))
    end;

    ReadInteger(KeyRight2, 'KeyRight2');
    ReadInteger(KeyLeft2, 'KeyLeft2');
    ReadInteger(KeyUp2, 'KeyUp2');
    ReadInteger(KeyDown2, 'KeyDown2');
    ReadInteger(KeyFire2, 'KeyFire2');
    ReadInteger(KeyJump2, 'KeyJump2');
    ReadInteger(KeyNextWeapon2, 'KeyNextWeapon2');
    ReadInteger(KeyPrevWeapon2, 'KeyPrevWeapon2');
    ReadInteger(KeyOpen2, 'KeyOpen2');
    ReadInteger(KeyStrafe2, 'KeyStrafe2');

    for i := 0 to High(KeyWeapon2) do
    begin
      ReadInteger(KeyWeapon2[i], 'KeyWeapon2' + IntToStr(i))
    end;
  end;

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
  with gGameControls.P2Control do
  begin
    ReadInteger(KeyRight, 'KeyRight');
    ReadInteger(KeyLeft, 'KeyLeft');
    ReadInteger(KeyUp, 'KeyUp');
    ReadInteger(KeyDown, 'KeyDown');
    ReadInteger(KeyFire, 'KeyFire');
    ReadInteger(KeyJump, 'KeyJump');
    ReadInteger(KeyNextWeapon, 'KeyNextWeapon');
    ReadInteger(KeyPrevWeapon, 'KeyPrevWeapon');
    ReadInteger(KeyOpen, 'KeyOpen');
    ReadInteger(KeyStrafe, 'KeyStrafe');

    for i := 0 to High(KeyWeapon) do
    begin
      ReadInteger(KeyWeapon[i], 'KeyWeapon' + IntToStr(i))
    end;

    ReadInteger(KeyRight2, 'KeyRight2');
    ReadInteger(KeyLeft2, 'KeyLeft2');
    ReadInteger(KeyUp2, 'KeyUp2');
    ReadInteger(KeyDown2, 'KeyDown2');
    ReadInteger(KeyFire2, 'KeyFire2');
    ReadInteger(KeyJump2, 'KeyJump2');
    ReadInteger(KeyNextWeapon2, 'KeyNextWeapon2');
    ReadInteger(KeyPrevWeapon2, 'KeyPrevWeapon2');
    ReadInteger(KeyOpen2, 'KeyOpen2');
    ReadInteger(KeyStrafe2, 'KeyStrafe2');

    for i := 0 to High(KeyWeapon2) do
    begin
      ReadInteger(KeyWeapon2[i], 'KeyWeapon2' + IntToStr(i))
    end;
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

  section := 'Touch';
  i := Trunc(g_touch_size * 10); ReadInteger(i, 'Size', 0); g_touch_size := i / 10;
  ReadBoolean(g_touch_fire, 'Fire');
  i := Round(g_touch_offset); ReadInteger(i, 'Offset', 0, 100); g_touch_offset := i;
  ReadBoolean(g_touch_alt, 'Alt');

  section := 'Game';
  ReadInteger(i, 'MaxParticles', 1000, 50000); g_GFX_SetMax(i);
  ReadInteger(i, 'MaxShells', 300, 600); g_Shells_SetMax(i);
  ReadInteger(i, 'MaxGibs', 150, 500); g_Gibs_SetMax(i);
  ReadInteger(i, 'MaxCorpses', 20, 100); g_Corpses_SetMax(i);
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

  with config, gGameControls.GameControls do
  begin
    WriteInt('GameControls', 'TakeScreenshot', TakeScreenshot);
    WriteInt('GameControls', 'Stat', Stat);
    WriteInt('GameControls', 'Chat', Chat);
    WriteInt('GameControls', 'TeamChat', TeamChat);
  end;

  with config, gGameControls.P1Control, gPlayer1Settings do
  begin
    WriteInt('Player1', 'KeyRight', KeyRight);
    WriteInt('Player1', 'KeyLeft', KeyLeft);
    WriteInt('Player1', 'KeyUp', KeyUp);
    WriteInt('Player1', 'KeyDown', KeyDown);
    WriteInt('Player1', 'KeyFire', KeyFire);
    WriteInt('Player1', 'KeyJump', KeyJump);
    WriteInt('Player1', 'KeyNextWeapon', KeyNextWeapon);
    WriteInt('Player1', 'KeyPrevWeapon', KeyPrevWeapon);
    WriteInt('Player1', 'KeyOpen', KeyOpen);
    WriteInt('Player1', 'KeyStrafe', KeyStrafe);
    for i := 0 to High(KeyWeapon) do
      WriteInt('Player1', 'KeyWeapon' + IntToStr(i), KeyWeapon[i]);

    WriteInt('Player1', 'KeyRight2', KeyRight2);
    WriteInt('Player1', 'KeyLeft2', KeyLeft2);
    WriteInt('Player1', 'KeyUp2', KeyUp2);
    WriteInt('Player1', 'KeyDown2', KeyDown2);
    WriteInt('Player1', 'KeyFire2', KeyFire2);
    WriteInt('Player1', 'KeyJump2', KeyJump2);
    WriteInt('Player1', 'KeyNextWeapon2', KeyNextWeapon2);
    WriteInt('Player1', 'KeyPrevWeapon2', KeyPrevWeapon2);
    WriteInt('Player1', 'KeyOpen2', KeyOpen2);
    WriteInt('Player1', 'KeyStrafe2', KeyStrafe2);
    for i := 0 to High(KeyWeapon2) do
      WriteInt('Player1', 'KeyWeapon2' + IntToStr(i), KeyWeapon2[i]);

    WriteStr('Player1', 'Name', Name);
    WriteStr('Player1', 'model', Model);
    WriteInt('Player1', 'red', Color.R);
    WriteInt('Player1', 'green', Color.G);
    WriteInt('Player1', 'blue', Color.B);
    WriteInt('Player1', 'team', Team);
  end;

  with config, gGameControls.P2Control, gPlayer2Settings do
  begin
    WriteInt('Player2', 'KeyRight', KeyRight);
    WriteInt('Player2', 'KeyLeft', KeyLeft);
    WriteInt('Player2', 'KeyUp', KeyUp);
    WriteInt('Player2', 'KeyDown', KeyDown);
    WriteInt('Player2', 'KeyFire', KeyFire);
    WriteInt('Player2', 'KeyJump', KeyJump);
    WriteInt('Player2', 'KeyNextWeapon', KeyNextWeapon);
    WriteInt('Player2', 'KeyPrevWeapon', KeyPrevWeapon);
    WriteInt('Player2', 'KeyOpen', KeyOpen);
    WriteInt('Player2', 'KeyStrafe', KeyStrafe);
    for i := 0 to High(KeyWeapon) do
      WriteInt('Player2', 'KeyWeapon' + IntToStr(i), KeyWeapon[i]);

    WriteInt('Player2', 'KeyRight2', KeyRight2);
    WriteInt('Player2', 'KeyLeft2', KeyLeft2);
    WriteInt('Player2', 'KeyUp2', KeyUp2);
    WriteInt('Player2', 'KeyDown2', KeyDown2);
    WriteInt('Player2', 'KeyFire2', KeyFire2);
    WriteInt('Player2', 'KeyJump2', KeyJump2);
    WriteInt('Player2', 'KeyNextWeapon2', KeyNextWeapon2);
    WriteInt('Player2', 'KeyPrevWeapon2', KeyPrevWeapon2);
    WriteInt('Player2', 'KeyOpen2', KeyOpen2);
    WriteInt('Player2', 'KeyStrafe2', KeyStrafe2);
    for i := 0 to High(KeyWeapon2) do
      WriteInt('Player2', 'KeyWeapon2' + IntToStr(i), KeyWeapon2[i]);

    WriteStr('Player2', 'Name', Name);
    WriteStr('Player2', 'model', Model);
    WriteInt('Player2', 'red', Color.R);
    WriteInt('Player2', 'green', Color.G);
    WriteInt('Player2', 'blue', Color.B);
    WriteInt('Player2', 'team', Team);
  end;

  for i := 0 to e_MaxJoys-1 do
    config.WriteInt('Joysticks', 'Deadzone' + IntToStr(i), e_JoystickDeadzones[i]);

  config.WriteInt('Touch', 'Size', Round(g_touch_size * 10));
  config.WriteBool('Touch', 'Fire', g_touch_fire);
  config.WriteInt('Touch', 'Offset', Round(g_touch_offset));
  config.WriteBool('Touch', 'Alt', g_touch_alt);

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

end.
