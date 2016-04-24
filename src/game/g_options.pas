{$MODE DELPHI}
unit g_options;

interface

uses
  g_language;

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

var
  gGameControls: TControls;
  gScreenWidth: Word          = 800;
  gScreenHeight: Word         = 600;
  gWinRealPosX: Integer       = 0;
  gWinRealPosY: Integer       = 0;
  gBPP: Byte                  = 32;
  gFreq: Byte                 = 0;
  gFullscreen: Boolean        = False;
  gWinMaximized: Boolean      = False;
  gVSync: Boolean             = False;
  glLegacyNPOT: Boolean       = False;
  gTextureFilter: Boolean     = True;
  gNoSound: Boolean           = False;
  gSoundLevel: Byte           = 75;
  gMusicLevel: Byte           = 65;
  gMaxSimSounds: Byte         = 8;
  gMuteWhenInactive: Boolean  = False;
  gAdvCorpses: Boolean        = True;
  gAdvBlood: Boolean          = True;
  gAdvGibs: Boolean           = True;
  gGibsCount: Integer         = 32;
  gBloodCount: Byte           = 3;
  gFlash: Byte                = 1;
  gDrawBackGround: Boolean    = True;
  gShowMessages: Boolean      = True;
  gRevertPlayers: Boolean     = False;
  gLanguage: String           = LANGUAGE_ENGLISH;
  gAskLanguage: Boolean       = True;
  gcMap: String               = '';
  gcGameMode: String          = '';
  gcTimeLimit: Word           = 0;
  gcGoalLimit: Word           = 0;
  gcMaxLives: Byte            = 0;
  gcPlayers: Byte             = 1;
  gcTeamDamage: Boolean       = False;
  gcAllowExit: Boolean        = True;
  gcWeaponStay: Boolean       = False;
  gcMonsters: Boolean         = False;
  gcBotsVS: String            = 'Everybody';
  gnMap: String               = '';
  gnGameMode: String          = '';
  gnTimeLimit: Word           = 0;
  gnGoalLimit: Word           = 0;
  gnMaxLives: Byte            = 0;
  gnPlayers: Byte             = 1;
  gnTeamDamage: Boolean       = False;
  gnAllowExit: Boolean        = True;
  gnWeaponStay: Boolean       = False;
  gnMonsters: Boolean         = False;
  gnBotsVS: String            = 'Everybody';
  gsSDLSampleRate: Integer    = 44100;
  gsSDLBufferSize: Integer    = 2048;
  gSFSDebug: Boolean          = False;
  gSFSFastMode: Boolean       = False;

implementation

uses
  e_log, e_input, g_window, g_sound, g_gfx, g_player, Math,
  g_map, g_net, g_netmaster, SysUtils, CONFIG, g_game, g_main, e_textures,
  g_items, GL, GLExt, wadreader;

procedure g_Options_SetDefault();
var
  i: Integer;
begin
  g_Sound_SetupAllVolumes(75, 65);
  gMaxSimSounds := 8;
  gMuteWhenInactive := False;
  gAnnouncer := ANNOUNCE_MEPLUS;
  gSoundEffectsDF := True;
  g_GFX_SetMax(2000);
  g_Gibs_SetMax(150);
  g_Corpses_SetMax(20);
  g_Shells_SetMax(300);
  gGibsCount := 32;
  gBloodCount := 3;
  gAdvBlood := True;
  gAdvCorpses := True;
  gAdvGibs := True;
  gFlash := 1;
  gDrawBackGround := True;
  gShowMessages := True;
  gRevertPlayers := False;

  for i := 0 to e_MaxJoys-1 do
    e_JoystickDeadzones[i] := 8192;

  with gGameControls.GameControls do
  begin
    TakeScreenshot := 183;
    Stat := 15;
    Chat := 20; // [T]
    TeamChat := 21; // [Y]
  end;

  with gGameControls.P1Control do
  begin
    KeyRight := 77;
    KeyLeft := 75;
    KeyUp := 72;
    KeyDown := 76;
    KeyFire := 184;
    KeyJump := 157;
    KeyNextWeapon := 73;
    KeyPrevWeapon := 71;
    KeyOpen := 54;
  end;

  with gGameControls.P2Control do
  begin
    KeyRight := 33;
    KeyLeft := 31;
    KeyUp := 18;
    KeyDown := 32;
    KeyFire := 30;
    KeyJump := 16;
    KeyNextWeapon := 19;
    KeyPrevWeapon := 17;
    KeyOpen := 58;
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

  with gPlayer2Settings do
  begin
    Name := 'Player2';
    Model := STD_PLAYER_MODEL;
    Color.R := PLAYER2_DEF_COLOR.R;
    Color.G := PLAYER2_DEF_COLOR.G;
    Color.B := PLAYER2_DEF_COLOR.B;
    Team := TEAM_BLUE;
  end;

  NetUseMaster := True;
  g_Net_Slist_Set('mpms.doom2d.org', 25665);
end;

procedure g_Options_Read(FileName: String);
var
  config: TConfig;
  str: String;
  i: Integer;
begin
  gAskLanguage := True;
  e_WriteLog('Reading config', MSG_NOTIFY);

  if not FileExists(FileName) then
  begin
    e_WriteLog('Config file '+FileName+' not found', MSG_WARNING);
    g_Options_SetDefault();

  // Default video options:
    gScreenWidth := 800;
    gScreenHeight := 600;
    gWinRealPosX := 0;
    gWinRealPosY := 0;
    gWinMaximized := False;
    gFullScreen := False;
    gBPP := 32;
    gVSync := False;
    gTextureFilter := True;
    glLegacyNPOT := False;

    Exit;
  end;

  config := TConfig.CreateFile(FileName);

  gScreenWidth := config.ReadInt('Video', 'ScreenWidth', 800);
  if gScreenWidth < 640 then
    gScreenWidth := 640;
  gScreenHeight := config.ReadInt('Video', 'ScreenHeight', 600);
  if gScreenHeight < 480 then
    gScreenHeight := 480;
  gWinRealPosX := config.ReadInt('Video', 'WinPosX', 0);
  if gWinRealPosX < 0 then
    gWinRealPosX := 0;
  gWinRealPosY := config.ReadInt('Video', 'WinPosY', 0);
  if gWinRealPosY < 0 then
    gWinRealPosY := 0;
  gFullScreen := config.ReadBool('Video', 'Fullscreen', False);
  gWinMaximized := config.ReadBool('Video', 'Maximized', False);
  gBPP := config.ReadInt('Video', 'BPP', 32);
  gFreq := config.ReadInt('Video', 'Freq', 0);
  gVSync := config.ReadBool('Video', 'VSync', True);
  gTextureFilter := config.ReadBool('Video', 'TextureFilter', True);
  glLegacyNPOT := config.ReadBool('Video', 'LegacyCompatible', False);

  gNoSound := config.ReadBool('Sound', 'NoSound', False);
  gSoundLevel := Min(config.ReadInt('Sound', 'SoundLevel', 75), 255);
  gMusicLevel := Min(config.ReadInt('Sound', 'MusicLevel', 65), 255);
  gMaxSimSounds := Max(Min(config.ReadInt('Sound', 'MaxSimSounds', 8), 66), 2);
  gMuteWhenInactive := config.ReadBool('Sound', 'MuteInactive', False);
  gAnnouncer := Min(Max(config.ReadInt('Sound', 'Announcer', ANNOUNCE_MEPLUS), ANNOUNCE_NONE), ANNOUNCE_ALL);
  gSoundEffectsDF := config.ReadBool('Sound', 'SoundEffectsDF', True);
  gsSDLSampleRate := Min(Max(config.ReadInt('Sound', 'SDLSampleRate', 44100), 11025), 96000);
  gsSDLBufferSize := Min(Max(config.ReadInt('Sound', 'SDLBufferSize', 2048), 64), 16384);

  with gGameControls.GameControls do
  begin
    TakeScreenshot := config.ReadInt('GameControls', 'TakeScreenshot', 183);
    Stat := config.ReadInt('GameControls', 'Stat', 15);
    Chat := config.ReadInt('GameControls', 'Chat', 20);
    TeamChat := config.ReadInt('GameControls', 'TeamChat', 21);
  end;

  with gGameControls.P1Control, config do
  begin
    KeyRight := ReadInt('Player1', 'KeyRight', 33);
    KeyLeft := ReadInt('Player1', 'KeyLeft', 31);
    KeyUp := ReadInt('Player1', 'KeyUp', 18);
    KeyDown := ReadInt('Player1', 'KeyDown', 32);
    KeyFire := ReadInt('Player1', 'KeyFire', 30);
    KeyJump := ReadInt('Player1', 'KeyJump', 16);
    KeyNextWeapon := ReadInt('Player1', 'KeyNextWeapon', 19);
    KeyPrevWeapon := ReadInt('Player1', 'KeyPrevWeapon', 17);
    KeyOpen := ReadInt('Player1', 'KeyOpen', 58);
  end;

  with gPlayer1Settings, config do
  begin
    Name := ReadStr('Player1', 'name', 'Player1');
    Model := ReadStr('Player1', 'model', STD_PLAYER_MODEL);
    Color.R := Min(Abs(ReadInt('Player1', 'red', PLAYER1_DEF_COLOR.R)), 255);
    Color.G := Min(Abs(ReadInt('Player1', 'green', PLAYER1_DEF_COLOR.G)), 255);
    Color.B := Min(Abs(ReadInt('Player1', 'blue', PLAYER1_DEF_COLOR.B)), 255);
    Team := ReadInt('Player1', 'team', TEAM_RED);
    if (Team < TEAM_RED) or (Team > TEAM_BLUE) then
      Team := TEAM_RED;
  end;

  with gGameControls.P2Control, config do
  begin
    KeyRight := ReadInt('Player2', 'KeyRight', 205);
    KeyLeft := ReadInt('Player2', 'KeyLeft', 203);
    KeyUp := ReadInt('Player2', 'KeyUp', 200);
    KeyDown := ReadInt('Player2', 'KeyDown', 208);
    KeyFire := ReadInt('Player2', 'KeyFire', 184);
    KeyJump := ReadInt('Player2', 'KeyJump', 157);
    KeyNextWeapon := ReadInt('Player2', 'KeyNextWeapon', 73);
    KeyPrevWeapon := ReadInt('Player2', 'KeyPrevWeapon', 71);
    KeyOpen := ReadInt('Player2', 'KeyOpen', 54);
  end;

  with gPlayer2Settings, config do
  begin
    Name := ReadStr('Player2', 'name', 'Player2');
    Model := ReadStr('Player2', 'model', STD_PLAYER_MODEL);
    Color.R := Min(Abs(ReadInt('Player2', 'red', PLAYER2_DEF_COLOR.R)), 255);
    Color.G := Min(Abs(ReadInt('Player2', 'green', PLAYER2_DEF_COLOR.G)), 255);
    Color.B := Min(Abs(ReadInt('Player2', 'blue', PLAYER2_DEF_COLOR.B)), 255);
    Team := ReadInt('Player2', 'team', TEAM_BLUE);
    if (Team < TEAM_RED) or (Team > TEAM_BLUE) then
      Team := TEAM_RED;
  end;

  for i := 0 to e_MaxJoys-1 do
    e_JoystickDeadzones[i] := config.ReadInt('Joysticks', 'Deadzone' + IntToStr(i), 8192);

  g_GFX_SetMax(Min(config.ReadInt('Game', 'MaxParticles', 1000), 50000));
  g_Shells_SetMax(Min(config.ReadInt('Game', 'MaxShells', 300), 600));
  g_Gibs_SetMax(Min(config.ReadInt('Game', 'MaxGibs', 150), 500));
  g_Corpses_SetMax(Min(config.ReadInt('Game', 'MaxCorpses', 20), 100));

  case config.ReadInt('Game', 'GibsCount', 3) of
    0: gGibsCount := 0;
    1: gGibsCount := 8;
    2: gGibsCount := 16;
    3: gGibsCount := 32;
    else gGibsCount := 48;
  end;

  ITEM_RESPAWNTIME := 36*Max(config.ReadInt('Game', 'ItemRespawnTime', 60), 0);
  gBloodCount := Min(config.ReadInt('Game', 'BloodCount', 4), 4);
  gAdvBlood := config.ReadBool('Game', 'AdvancesBlood', True);
  gAdvCorpses := config.ReadBool('Game', 'AdvancesCorpses', True);
  gAdvGibs := config.ReadBool('Game', 'AdvancesGibs', True);
  gFlash := Min(Max(config.ReadInt('Game', 'Flash', 1), 0), 2);
  gDrawBackGround := config.ReadBool('Game', 'BackGround', True);
  gShowMessages := config.ReadBool('Game', 'Messages', True);
  gRevertPlayers := config.ReadBool('Game', 'RevertPlayers', False);
  gChatBubble := Min(Max(config.ReadInt('Game', 'ChatBubble', 4), 0), 4);
  gSFSDebug := config.ReadBool('Game', 'SFSDebug', False);
  wadoptDebug := gSFSDebug;
  gSFSFastMode := config.ReadBool('Game', 'SFSFastMode', False);
  wadoptFast := gSFSFastMode;

// Геймплей в своей игре
  gcMap := config.ReadStr('GameplayCustom', 'Map', '');
  gcGameMode := config.ReadStr('GameplayCustom', 'GameMode', _lc[I_MENU_GAME_TYPE_DM]);
  gcTimeLimit := Min(Max(config.ReadInt('GameplayCustom', 'TimeLimit', 0), 0), 65535);
  gcGoalLimit := Min(Max(config.ReadInt('GameplayCustom', 'GoalLimit', 0), 0), 65535);
  gcMaxLives := Min(Max(config.ReadInt('GameplayCustom', 'MaxLives', 0), 0), 255);
  gcPlayers := Min(Max(config.ReadInt('GameplayCustom', 'Players', 1), 0), 2);
  gcTeamDamage := config.ReadBool('GameplayCustom', 'TeamDamage', False);
  gcAllowExit := config.ReadBool('GameplayCustom', 'AllowExit', True);
  gcWeaponStay := config.ReadBool('GameplayCustom', 'WeaponStay', False);
  gcMonsters := config.ReadBool('GameplayCustom', 'Monsters', False);
  gcBotsVS := config.ReadStr('GameplayCustom', 'BotsVS', 'Everybody');

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

// Геймплей в сетевой игре
  gnMap := config.ReadStr('GameplayNetwork', 'Map', '');
  gnGameMode := config.ReadStr('GameplayNetwork', 'GameMode', _lc[I_MENU_GAME_TYPE_DM]);
  gnTimeLimit := Min(Max(config.ReadInt('GameplayNetwork', 'TimeLimit', 0), 0), 65535);
  gnGoalLimit := Min(Max(config.ReadInt('GameplayNetwork', 'GoalLimit', 0), 0), 65535);
  gnMaxLives := Min(Max(config.ReadInt('GameplayNetwork', 'MaxLives', 0), 0), 255);
  gnPlayers := Min(Max(config.ReadInt('GameplayNetwork', 'Players', 1), 0), 2);
  gnTeamDamage := config.ReadBool('GameplayNetwork', 'TeamDamage', False);
  gnAllowExit := config.ReadBool('GameplayNetwork', 'AllowExit', True);
  gnWeaponStay := config.ReadBool('GameplayNetwork', 'WeaponStay', False);
  gnMonsters := config.ReadBool('GameplayNetwork', 'Monsters', False);
  gnBotsVS := config.ReadStr('GameplayNetwork', 'BotsVS', 'Everybody');

// Общие сетевые
  NetSlistIP := config.ReadStr('MasterServer', 'IP', 'mpms.doom2d.org');
  NetSlistPort := config.ReadInt('MasterServer', 'Port', 25665);

// Сервер
  NetServerName := config.ReadStr('Server', 'Name', 'Unnamed Server');
  NetPassword :=  config.ReadStr('Server', 'Password', '');
  NetPort := Min(Max(0, config.ReadInt('Server', 'Port', 25666)), 65535);
  NetMaxClients := Min(Max(0, config.ReadInt('Server', 'MaxClients', 16)), NET_MAXCLIENTS);
  NetAllowRCON := config.ReadBool('Server', 'RCON', False);
  NetRCONPassword := config.ReadStr('Server', 'RCONPassword', 'default');
  NetUseMaster := config.ReadBool('Server', 'SyncWithMaster', True);
  NetUpdateRate := Max(0, config.ReadInt('Server', 'UpdateInterval', 0));
  NetRelupdRate := Max(0, config.ReadInt('Server', 'ReliableUpdateInterval', 18));
  NetMasterRate := Max(1, config.ReadInt('Server', 'MasterSyncInterval', 60000));

// Клиент
  NetInterpLevel := Max(0, config.ReadInt('Client', 'InterpolationSteps', 2));
  NetForcePlayerUpdate := config.ReadBool('Client', 'ForcePlayerUpdate', False);
  NetPredictSelf := config.ReadBool('Client', 'PredictSelf', True);
  NetClientIP := config.ReadStr('Client', 'LastIP', '127.0.0.1');
  NetClientPort := Max(0, config.ReadInt('Client', 'LastPort', 25666));

// Язык:
  str := config.ReadStr('Game', 'Language', '');
  if (str = LANGUAGE_RUSSIAN) or
     (str = LANGUAGE_ENGLISH) then
  begin
    gLanguage := str;
    gAskLanguage := False;
  end
  else
    gLanguage := LANGUAGE_ENGLISH;

  config.Free();

  if gTextureFilter then
    TEXTUREFILTER := GL_LINEAR
  else
    TEXTUREFILTER := GL_NEAREST;
end;

procedure g_Options_Write(FileName: String);
var
  config: TConfig;
  i: Integer;
begin
  e_WriteLog('Writing config', MSG_NOTIFY);

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
  e_WriteLog('Writing language config', MSG_NOTIFY);

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
  e_WriteLog('Writing resolution to config', MSG_NOTIFY);

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
  e_WriteLog('Writing custom gameplay config', MSG_NOTIFY);

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
  e_WriteLog('Writing network gameplay config', MSG_NOTIFY);

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
  e_WriteLog('Writing server config', MSG_NOTIFY);

  config := TConfig.CreateFile(FileName);

  config.WriteStr ('Server', 'Name', NetServerName);
  config.WriteStr ('Server', 'Password', NetPassword);
  config.WriteInt ('Server', 'Port', NetPort);
  config.WriteInt ('Server', 'MaxClients', NetMaxClients);
  config.WriteBool('Server', 'SyncWithMaster', NetUseMaster);

  config.SaveFile(FileName);
  config.Free();
end;

procedure g_Options_Write_Net_Client(FileName: String);
var
  config: TConfig;
begin
  e_WriteLog('Writing client config', MSG_NOTIFY);

  config := TConfig.CreateFile(FileName);

  config.WriteStr('Client', 'LastIP', NetClientIP);
  config.WriteInt('Client', 'LastPort', NetClientPort);

  config.SaveFile(FileName);
  config.Free();
end;

end.
