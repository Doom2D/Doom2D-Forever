unit g_menu;

interface

uses
  windows;

procedure g_Menu_Init();
procedure g_Menu_Free();
procedure g_Menu_Reset();
procedure LoadFont(txtres, fntres: string; cwdt, chgt: Byte; spc: ShortInt;
                   var FontID: DWORD);
procedure g_Menu_AskLanguage();

procedure g_Menu_Show_SaveMenu(custom: Boolean);
procedure g_Menu_Show_LoadMenu(custom: Boolean);
procedure g_Menu_Show_GameSetGame(custom: Boolean);
procedure g_Menu_Show_OptionsVideo(custom: Boolean);
procedure g_Menu_Show_OptionsSound(custom: Boolean);
procedure g_Menu_Show_EndGameMenu(custom: Boolean);
procedure g_Menu_Show_QuitGameMenu(custom: Boolean);

var
  gMenuFont: DWORD;
  gMenuSmallFont: DWORD;
  PromptIP: string;
  PromptPort: Word;

implementation

uses
  g_gui, g_textures, e_graphics, g_main, g_window, g_game, g_map,
  g_basic, g_console, g_sound, g_gfx, g_player, g_options,
  e_log, SysUtils, CONFIG, g_playermodel, DateUtils,
  MAPSTRUCT, WADEDITOR, Math, WADSTRUCT, g_saveload,
  e_textures, dglOpenGL, g_language,
  g_net, g_netmsg, g_netmaster, g_items;

procedure ProcChangeColor(Sender: TGUIControl); forward;
procedure ProcSelectModel(Sender: TGUIControl); forward;

procedure ProcApplyOptions();
var
  menu: TGUIMenu;
  config: TConfig;
  BPP: DWORD;
begin
  menu := TGUIMenu(g_GUI_GetWindow('OptionsVideoMenu').GetControl('mOptionsVideoMenu'));

  if TGUISwitch(menu.GetControl('swBPP')).ItemIndex = 0 then
    BPP := 16
  else
    BPP := 32;
  gVSync := TGUISwitch(menu.GetControl('swVSync')).ItemIndex = 0;
  gTextureFilter := TGUISwitch(menu.GetControl('swTextureFilter')).ItemIndex = 0;

  menu := TGUIMenu(g_GUI_GetWindow('OptionsSoundMenu').GetControl('mOptionsSoundMenu'));

  g_Sound_SetupAllVolumes(
    Min(TGUIScroll(menu.GetControl('scSoundLevel')).Value*16, 255),
    Min(TGUIScroll(menu.GetControl('scMusicLevel')).Value*16, 255)
  );

  gMaxSimSounds := Max(Min(TGUIScroll(menu.GetControl('scMaxSimSounds')).Value*4+2, 66), 2);
  gMuteWhenInactive := TGUISwitch(menu.GetControl('swInactiveSounds')).ItemIndex = 1;

  menu := TGUIMenu(g_GUI_GetWindow('OptionsGameMenu').GetControl('mOptionsGameMenu'));

  g_GFX_SetMax(TGUIScroll(menu.GetControl('scParticlesCount')).Value*1000);
  g_Gibs_SetMax(TGUIScroll(menu.GetControl('scGibsMax')).Value*25);
  g_Corpses_SetMax(TGUIScroll(menu.GetControl('scCorpsesMax')).Value*5);

  case TGUISwitch(menu.GetControl('swGibsCount')).ItemIndex of
    0: gGibsCount := 0;
    1: gGibsCount := 8;
    2: gGibsCount := 16;
    3: gGibsCount := 32;
    else gGibsCount := 48;
  end;

  gBloodCount := TGUISwitch(menu.GetControl('swBloodCount')).ItemIndex;
  gFlash := TGUISwitch(menu.GetControl('swScreenFlash')).ItemIndex = 0;
  gAdvBlood := TGUISwitch(menu.GetControl('swBloodType')).ItemIndex = 1;
  gAdvCorpses := TGUISwitch(menu.GetControl('swCorpseType')).ItemIndex = 1;
  gAdvGibs := TGUISwitch(menu.GetControl('swGibsType')).ItemIndex = 1;
  gDrawBackGround := TGUISwitch(menu.GetControl('swBackGround')).ItemIndex = 0;
  gShowMessages := TGUISwitch(menu.GetControl('swMessages')).ItemIndex = 0;
  gRevertPlayers := TGUISwitch(menu.GetControl('swRevertPlayers')).ItemIndex = 0;

  menu := TGUIMenu(g_GUI_GetWindow('OptionsControlsMenu').GetControl('mOptionsControlsMenu'));

  with menu, gGameControls.GameControls do
  begin
    TakeScreenshot := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_SCREENSHOT])).Key;
    Stat := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_STAT])).Key;
    Chat := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_CHAT])).Key;
  end;

  menu := TGUIMenu(g_GUI_GetWindow('OptionsControlsP1Menu').GetControl('mOptionsControlsP1Menu'));

  with menu, gGameControls.P1Control do
  begin
    KeyRight := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_RIGHT])).Key;
    KeyLeft := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_LEFT])).Key;
    KeyUp := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_UP])).Key;
    KeyDown := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_DOWN])).Key;
    KeyFire := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_FIRE])).Key;
    KeyJump := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_JUMP])).Key;
    KeyNextWeapon := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_NEXT_WEAPON])).Key;
    KeyPrevWeapon := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_PREV_WEAPON])).Key;
    KeyOpen := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_USE])).Key;
  end;

  menu := TGUIMenu(g_GUI_GetWindow('OptionsControlsP2Menu').GetControl('mOptionsControlsP2Menu'));

  with menu, gGameControls.P2Control do
  begin
    KeyRight := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_RIGHT])).Key;
    KeyLeft := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_LEFT])).Key;
    KeyUp := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_UP])).Key;
    KeyDown := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_DOWN])).Key;
    KeyFire := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_FIRE])).Key;
    KeyJump := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_JUMP])).Key;
    KeyNextWeapon := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_NEXT_WEAPON])).Key;
    KeyPrevWeapon := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_PREV_WEAPON])).Key;
    KeyOpen := TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_USE])).Key;
  end;

  menu := TGUIMenu(g_GUI_GetWindow('OptionsPlayersP1Menu').GetControl('mOptionsPlayersP1Menu'));

  gPlayer1Settings.Name := TGUIEdit(menu.GetControl('edP1Name')).Text;
  gPlayer1Settings.Team := IfThen(TGUISwitch(menu.GetControl('swP1Team')).ItemIndex = 0,
                                  TEAM_RED, TEAM_BLUE);

  with TGUIModelView(g_GUI_GetWindow('OptionsPlayersP1Menu').GetControl('mvP1Model')) do
  begin
    gPlayer1Settings.Model := Model.Name;
    gPlayer1Settings.Color := Model.Color;
  end;

  menu := TGUIMenu(g_GUI_GetWindow('OptionsPlayersP2Menu').GetControl('mOptionsPlayersP2Menu'));

  gPlayer2Settings.Name := TGUIEdit(menu.GetControl('edP2Name')).Text;
  gPlayer2Settings.Team := IfThen(TGUISwitch(menu.GetControl('swP2Team')).ItemIndex = 0,
                                  TEAM_RED, TEAM_BLUE);
  with TGUIModelView(g_GUI_GetWindow('OptionsPlayersP2Menu').GetControl('mvP2Model')) do
  begin
    gPlayer2Settings.Model := Model.Name;
    gPlayer2Settings.Color := Model.Color;
  end;

  if gPlayer1Settings.Name = '' then gPlayer1Settings.Name := 'Player1';
  if gPlayer2Settings.Name = '' then gPlayer2Settings.Name := 'Player2';

  if (gGameSettings.GameType in [GT_CUSTOM, GT_SERVER]) then
  begin
    if (gPlayer1 <> nil) and not (g_Game_IsNet and NetDedicated) then
    begin
      gPlayer1.SetModel(gPlayer1Settings.Model);
      gPlayer1.Name := gPlayer1Settings.Name;
      if (gGameSettings.GameMode <> GM_TDM) and (gGameSettings.GameMode <> GM_CTF) then
        gPlayer1.SetColor(gPlayer1Settings.Color)
      else
        if gPlayer1.Team <> gPlayer1Settings.Team then
          gPlayer1.ChangeTeam;

      if g_Game_IsNet then MH_SEND_PlayerSettings(gPlayer1.UID);
    end;

    if gPlayer2 <> nil then
    begin
      gPlayer2.SetModel(gPlayer2Settings.Model);
      gPlayer2.Name := gPlayer2Settings.Name;
      if (gGameSettings.GameMode <> GM_TDM) and (gGameSettings.GameMode <> GM_CTF) then
        gPlayer2.SetColor(gPlayer2Settings.Color)
      else
        if gPlayer2.Team <> gPlayer2Settings.Team then
          gPlayer2.ChangeTeam;
    end;
  end;

  if g_Game_IsClient then MC_SEND_PlayerSettings;

  e_WriteLog('Writing config', MSG_NOTIFY);
 
  config := TConfig.CreateFile(GameDir+'\'+CONFIG_FILENAME);

  config.WriteInt('Video', 'ScreenWidth', gScreenWidth);
  config.WriteInt('Video', 'ScreenHeight', gScreenHeight);
  config.WriteInt('Video', 'WinPosX', gWinRealPosX);
  config.WriteInt('Video', 'WinPosY', gWinRealPosY);
  config.WriteBool('Video', 'Fullscreen', gFullScreen);
  config.WriteBool('Video', 'Maximized', gWinMaximized);
  config.WriteInt('Video', 'BPP', BPP);
  config.WriteBool('Video', 'VSync', gVSync);
  config.WriteBool('Video', 'TextureFilter', gTextureFilter);

  config.WriteInt('Sound', 'SoundLevel', gSoundLevel);
  config.WriteInt('Sound', 'MusicLevel', gMusicLevel);
  config.WriteInt('Sound', 'MaxSimSounds', gMaxSimSounds);
  config.WriteBool('Sound', 'MuteInactive', gMuteWhenInactive);

  with config, gGameControls.GameControls do
  begin
    WriteInt('GameControls', 'TakeScreenshot', TakeScreenshot);
    WriteInt('GameControls', 'Stat', Stat);
    WriteInt('GameControls', 'Chat', Chat);
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
  config.WriteInt('Game', 'MaxGibs', g_Gibs_GetMax());
  config.WriteInt('Game', 'MaxCorpses', g_Corpses_GetMax());
  config.WriteInt('Game', 'BloodCount', gBloodCount);
  config.WriteBool('Game', 'AdvancesBlood', gAdvBlood);
  config.WriteBool('Game', 'AdvancesCorpses', gAdvCorpses);
  config.WriteBool('Game', 'AdvancesGibs', gAdvGibs);
  config.WriteBool('Game', 'Flash', gFlash);
  config.WriteBool('Game', 'BackGround', gDrawBackGround);
  config.WriteBool('Game', 'Messages', gShowMessages);
  config.WriteBool('Game', 'RevertPlayers', gRevertPlayers);

  config.WriteStr('MasterServer', 'IP', NetSlistIP);
  config.WriteInt('MasterServer', 'Port', NetSlistPort);

  config.WriteBool('Server', 'Dedicated', NetDedicated);
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
  config.WriteStr  ('Client', 'LastIP', NetLastIP);
  config.WriteInt  ('Client', 'LastPort', NetLastPort);

  config.SaveFile(GameDir+'\'+CONFIG_FILENAME);
  config.Free();
end;

procedure ReadOptions();
var
  menu: TGUIMenu;
begin
  menu := TGUIMenu(g_GUI_GetWindow('OptionsVideoMenu').GetControl('mOptionsVideoMenu'));

  with TGUISwitch(menu.GetControl('swBPP')) do
    if gBPP = 16 then
      ItemIndex := 0
    else
      ItemIndex := 1;

  with TGUISwitch(menu.GetControl('swTextureFilter')) do
    if gTextureFilter then ItemIndex := 0 else ItemIndex := 1;

  with TGUISwitch(menu.GetControl('swVSync')) do
    if gVSync then ItemIndex := 0 else ItemIndex := 1;

  menu := TGUIMenu(g_GUI_GetWindow('OptionsSoundMenu').GetControl('mOptionsSoundMenu'));

  TGUIScroll(menu.GetControl('scSoundLevel')).Value := Round(gSoundLevel/16);
  TGUIScroll(menu.GetControl('scMusicLevel')).Value := Round(gMusicLevel/16);
  TGUIScroll(menu.GetControl('scMaxSimSounds')).Value := Round((gMaxSimSounds-2)/4);

  with TGUISwitch(menu.GetControl('swInactiveSounds')) do
    if gMuteWhenInactive then
      ItemIndex := 1
    else
      ItemIndex := 0;

  menu := TGUIMenu(g_GUI_GetWindow('OptionsControlsP1Menu').GetControl('mOptionsControlsP1Menu'));

  with menu, gGameControls.P1Control do
  begin
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_RIGHT])).Key := KeyRight;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_LEFT])).Key := KeyLeft;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_UP])).Key := KeyUp;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_DOWN])).Key := KeyDown;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_FIRE])).Key := KeyFire;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_JUMP])).Key := KeyJump;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_NEXT_WEAPON])).Key := KeyNextWeapon;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_PREV_WEAPON])).Key := KeyPrevWeapon;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_USE])).Key := KeyOpen;
  end;

  menu := TGUIMenu(g_GUI_GetWindow('OptionsControlsP2Menu').GetControl('mOptionsControlsP2Menu'));

  with menu, gGameControls.P2Control do
  begin
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_RIGHT])).Key := KeyRight;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_LEFT])).Key := KeyLeft;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_UP])).Key := KeyUp;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_DOWN])).Key := KeyDown;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_FIRE])).Key := KeyFire;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_JUMP])).Key := KeyJump;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_NEXT_WEAPON])).Key := KeyNextWeapon;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_PREV_WEAPON])).Key := KeyPrevWeapon;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_USE])).Key := KeyOpen;
  end;

  menu := TGUIMenu(g_GUI_GetWindow('OptionsControlsMenu').GetControl('mOptionsControlsMenu'));
  with menu, gGameControls.GameControls do
  begin
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_SCREENSHOT])).Key := TakeScreenshot;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_STAT])).Key := Stat;
    TGUIKeyRead(GetControl(_lc[I_MENU_CONTROL_CHAT])).Key := Chat;
  end;

  menu := TGUIMenu(g_GUI_GetWindow('OptionsGameMenu').GetControl('mOptionsGameMenu'));

   TGUIScroll(menu.GetControl('scParticlesCount')).Value := g_GFX_GetMax() div 1000;
   TGUIScroll(menu.GetControl('scGibsMax')).Value := g_Gibs_GetMax() div 25;
   TGUIScroll(menu.GetControl('scCorpsesMax')).Value := g_Corpses_GetMax() div 5;
   TGUISwitch(menu.GetControl('swBloodCount')).ItemIndex := gBloodCount;

  with TGUISwitch(menu.GetControl('swScreenFlash')) do
    if gFlash then ItemIndex := 0 else ItemIndex := 1;

  with TGUISwitch(menu.GetControl('swBloodType')) do
    if gAdvBlood then ItemIndex := 1 else ItemIndex := 0;

  with TGUISwitch(menu.GetControl('swCorpseType')) do
    if gAdvCorpses then ItemIndex := 1 else ItemIndex := 0;

  with TGUISwitch(menu.GetControl('swGibsType')) do
    if gAdvGibs then ItemIndex := 1 else ItemIndex := 0;

  with TGUISwitch(menu.GetControl('swGibsCount')) do
    case gGibsCount of
      0: ItemIndex := 0;
      8: ItemIndex := 1;
      16: ItemIndex := 2;
      32: ItemIndex := 3;
      else ItemIndex := 4;
    end;

  with TGUISwitch(menu.GetControl('swBackGround')) do
    if gDrawBackGround then ItemIndex := 0 else ItemIndex := 1;

  with TGUISwitch(menu.GetControl('swMessages')) do
    if gShowMessages then ItemIndex := 0 else ItemIndex := 1;

  with TGUISwitch(menu.GetControl('swRevertPlayers')) do
    if gRevertPlayers then ItemIndex := 0 else ItemIndex := 1;

  menu := TGUIMenu(g_GUI_GetWindow('OptionsPlayersP1Menu').GetControl('mOptionsPlayersP1Menu'));

  TGUIListBox(menu.GetControl('lsP1Model')).SelectItem(gPlayer1Settings.Model);
  TGUIEdit(menu.GetControl('edP1Name')).Text := gPlayer1Settings.Name;

  TGUISwitch(menu.GetControl('swP1Team')).ItemIndex :=
    IfThen(gPlayer1Settings.Team = TEAM_BLUE, 1, 0);

  TGUIScroll(menu.GetControl('scP1Red')).Value := Round(gPlayer1Settings.Color.R/16);
  TGUIScroll(menu.GetControl('scP1Green')).Value := Round(gPlayer1Settings.Color.G/16);
  TGUIScroll(menu.GetControl('scP1Blue')).Value := Round(gPlayer1Settings.Color.B/16);

  menu := TGUIMenu(g_GUI_GetWindow('OptionsPlayersP2Menu').GetControl('mOptionsPlayersP2Menu'));

  TGUIListBox(menu.GetControl('lsP2Model')).SelectItem(gPlayer2Settings.Model);
  TGUIEdit(menu.GetControl('edP2Name')).Text := gPlayer2Settings.Name;

  TGUISwitch(menu.GetControl('swP2Team')).ItemIndex :=
    IfThen(gPlayer2Settings.Team = TEAM_BLUE, 1, 0);

  TGUIScroll(menu.GetControl('scP2Red')).Value := Round(gPlayer2Settings.Color.R/16);
  TGUIScroll(menu.GetControl('scP2Green')).Value := Round(gPlayer2Settings.Color.G/16);
  TGUIScroll(menu.GetControl('scP2Blue')).Value := Round(gPlayer2Settings.Color.B/16);

  ProcSelectModel(nil);
end;

procedure ProcStartCustomGame();
var
  Map: String;
  GameMode, n, Lives: Byte;
  TimeLimit, GoalLimit: Word;
  Options: LongWord;
begin
  with TGUIMenu(g_ActiveWindow.GetControl('mCustomGameMenu')) do
  begin
    if TGUILabel(GetControl('lbMap')).Text = '' then
      Exit;

    GameMode := TGUISwitch(GetControl('swGameMode')).ItemIndex+1;
    TimeLimit := StrToIntDef(TGUIEdit(GetControl('edTimeLimit')).Text, 0);
    GoalLimit := StrToIntDef(TGUIEdit(GetControl('edGoalLimit')).Text, 0);
    Lives := StrToIntDef(TGUIEdit(GetControl('edMaxLives')).Text, 0);

    Options := 0;
    if TGUISwitch(GetControl('swTeamDamage')).ItemIndex = 0 then
      Options := Options or GAME_OPTION_TEAMDAMAGE;
    if TGUISwitch(GetControl('swEnableExits')).ItemIndex = 0 then
      Options := Options or GAME_OPTION_ALLOWEXIT;
    if TGUISwitch(GetControl('swWeaponStay')).ItemIndex = 0 then
      Options := Options or GAME_OPTION_WEAPONSTAY;
    if TGUISwitch(GetControl('swMonsterDM')).ItemIndex = 0 then
      Options := Options or GAME_OPTION_MONSTERDM;
    if TGUISwitch(GetControl('swPlayers')).ItemIndex = 1 then
      begin
        Options := Options or GAME_OPTION_TWOPLAYER;
        n := 2;
      end
    else
      n := 1;

    case TGUISwitch(GetControl('swBotsVS')).ItemIndex of
      1: Options := Options or GAME_OPTION_BOTVSMONSTER;
      2: Options := Options or GAME_OPTION_BOTVSPLAYER or GAME_OPTION_BOTVSMONSTER;
      else Options := Options or GAME_OPTION_BOTVSPLAYER;
    end;

    Map := MapsDir+TGUILabel(GetControl('lbMap')).Text;
  end;

  g_Game_StartCustom(Map, GameMode, TimeLimit, GoalLimit,
                     Lives, Options, n);
end;


procedure ProcStartNetGame();
var
  Map: String;
  GameMode: Byte;
  TimeLimit, GoalLimit: Word;
  Lives: Byte;
  Options: LongWord;
  config: TConfig;
begin
  with TGUIMenu(g_ActiveWindow.GetControl('mNetServerMenu')) do
  begin
    if TGUILabel(GetControl('lbMap')).Text = '' then
      Exit;

    GameMode := TGUISwitch(GetControl('swGameMode')).ItemIndex+1;
    TimeLimit := StrToIntDef(TGUIEdit(GetControl('edTimeLimit')).Text, 0);
    GoalLimit := StrToIntDef(TGUIEdit(GetControl('edGoalLimit')).Text, 0);
    Lives := StrToIntDef(TGUIEdit(GetControl('edMaxLives')).Text, 0);
    NetPort := StrToIntDef(TGUIEdit(GetControl('edPort')).Text, 0);

    Options := 0;
    if TGUISwitch(GetControl('swTeamDamage')).ItemIndex = 0 then
      Options := Options or GAME_OPTION_TEAMDAMAGE;
    if TGUISwitch(GetControl('swEnableExits')).ItemIndex = 0 then
      Options := Options or GAME_OPTION_ALLOWEXIT;
    if TGUISwitch(GetControl('swWeaponStay')).ItemIndex = 0 then
      Options := Options or GAME_OPTION_WEAPONSTAY;
    if TGUISwitch(GetControl('swMonsterDM')).ItemIndex = 0 then
      Options := Options or GAME_OPTION_MONSTERDM;

    case TGUISwitch(GetControl('swBotsVS')).ItemIndex of
      1: Options := Options or GAME_OPTION_BOTVSMONSTER;
      2: Options := Options or GAME_OPTION_BOTVSPLAYER or GAME_OPTION_BOTVSMONSTER;
      else Options := Options or GAME_OPTION_BOTVSPLAYER;
    end;

    Map := MapsDir+TGUILabel(GetControl('lbMap')).Text;
    gRelativeMapResStr := TGUILabel(GetControl('lbMap')).Text;
    NetServerName := TGUIEdit(GetControl('edSrvName')).Text;
    NetMaxClients := Max(1, StrToIntDef(TGUIEdit(GetControl('edMaxPlayers')).Text, 1));
    NetMaxClients := Min(NET_MAXCLIENTS, NetMaxClients);
    NetPassword := TGUIEdit(GetControl('edSrvPassword')).Text;
  end;

  config := TConfig.CreateFile(GameDir+'\'+CONFIG_FILENAME);

  config.WriteBool('Server', 'Dedicated', NetDedicated);
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

  config.SaveFile(GameDir+'\'+CONFIG_FILENAME);
  config.Free();

  g_Game_StartServer(Map, GameMode, TimeLimit, GoalLimit, Lives,
                     Options, NetPort);
end;

procedure ProcConnectNetGame();
var
  IP, PW: String;
  Port: Word;
  config: TConfig;
begin
  with TGUIMenu(g_ActiveWindow.GetControl('mNetClientMenu')) do
  begin
    IP := TGUIEdit(GetControl('edIP')).Text;
    Port := StrToIntDef(TGUIEdit(GetControl('edPort')).Text, 0);
    PW := TGUIEdit(GetControl('edPW')).Text;
  end;

  config := TConfig.CreateFile(GameDir+'\'+CONFIG_FILENAME);

  config.WriteStr ('Client', 'LastIP', IP);
  config.WriteInt ('Client', 'LastPort', Port);

  config.SaveFile(GameDir+'\'+CONFIG_FILENAME);
  config.Free();

  if PW = '' then PW := 'ASS';
  g_Game_StartClient(IP, Port, PW);
end;

procedure ProcEnterPassword();
var
  IP, PW: string;
  Port: Word;
  config: TConfig;
begin
  with TGUIMenu(g_ActiveWindow.GetControl('mClientPasswordMenu')) do
  begin
    IP := PromptIP;
    Port := PromptPort;
    PW := TGUIEdit(GetControl('edPW')).Text;
  end;
  config := TConfig.CreateFile(GameDir+'\'+CONFIG_FILENAME);

  config.WriteStr ('Client', 'LastIP', IP);
  config.WriteInt ('Client', 'LastPort', Port);

  config.SaveFile(GameDir+'\'+CONFIG_FILENAME);
  config.Free();

  if PW = '' then PW := 'ASS';
  g_Game_StartClient(IP, Port, PW);
end;

procedure ProcServerlist();
begin
  if not NetInitDone then
  begin
    if (not g_Net_Init()) then
    begin
      g_Console_Add('NET: ERROR: Failed to init ENet!');
      Exit;
    end
    else
      NetInitDone := True;
  end;

  g_Net_Slist_Set(NetSlistIP, NetSlistPort);

  gState := STATE_SLIST;
  g_ActiveWindow := nil;

  slWaitStr := _lc[I_NET_SLIST_WAIT];
  
  g_Game_Draw;
  ReDrawWindow;

  slReturnPressed := True;
  slCurrent := g_Net_Slist_Fetch;
  if slCurrent = nil then slWaitStr := _lc[I_NET_SLIST_NOSERVERS];
end;

procedure ProcStartCampaign();
var
  WAD: String;
  TwoPlayers: Boolean;
  n: Byte;
begin
  with TGUIMenu(g_ActiveWindow.GetControl('mCampaignMenu')) do
  begin
    WAD := TGUIFileListBox(GetControl('lsWAD')).SelectedItem();
    TwoPlayers := TGUISwitch(GetControl('swPlayers')).ItemIndex = 1;
  end;

  if TwoPlayers then
    n := 2
  else
    n := 1;
  g_Game_StartSingle(WAD, 'MAP01', TwoPlayers, n);
end;

procedure ProcSelectMap(Sender: TGUIControl);
var
  win: TGUIWindow;
  a: TMapInfo;
  wad, map, res: String;
begin
  win := g_GUI_GetWindow('SelectMapMenu');
  with TGUIMenu(win.GetControl('mSelectMapMenu')) do
  begin
    wad := TGUIFileListBox(GetControl('lsMapWAD')).SelectedItem();
    map := TGUIListBox(GetControl('lsMapRes')).SelectedItem();

    if (wad = '') or (map = '') then
      begin // Это не карта
        TGUILabel(GetControl('lbMapName')).Text := '';
        TGUILabel(GetControl('lbMapAuthor')).Text := '';
        TGUILabel(GetControl('lbMapSize')).Text := '';
        TGUIMemo(GetControl('meMapDescription')).SetText('');
        TGUIMapPreview(win.GetControl('mpMapPreview')).ClearMap();
        TGUILabel(win.GetControl('lbMapScale')).Text := '';
      end
    else // Это карта
      begin
        res := wad+':\'+map;

        a := g_Map_GetMapInfo(res);

        TGUILabel(GetControl('lbMapName')).Text := a.Name;
        TGUILabel(GetControl('lbMapAuthor')).Text := a.Author;
        TGUILabel(GetControl('lbMapSize')).Text := Format('%dx%d', [a.Width, a.Height]);
        TGUIMemo(GetControl('meMapDescription')).SetText(a.Description);
        TGUIMapPreview(win.GetControl('mpMapPreview')).SetMap(res);
        TGUILabel(win.GetControl('lbMapScale')).Text :=
          TGUIMapPreview(win.GetControl('mpMapPreview')).GetScaleStr;
      end;
  end;
end;

procedure ProcSelectWAD(Sender: TGUIControl);
var
  wad: String;
  list: SArray;
begin
  with TGUIMenu(g_GUI_GetWindow('SelectMapMenu').GetControl('mSelectMapMenu')) do
  begin
    wad := TGUIFileListBox(GetControl('lsMapWAD')).SelectedItem();

    with TGUIListBox(GetControl('lsMapRes')) do
    begin
      Clear();

      if wad <> '' then
      begin
        list := g_Map_GetMapsList(wad);

        if list <> nil then
        begin
          Items := list;
          ItemIndex := 0;
        end
      end;
    end;
 end;
 
 ProcSelectMap(nil);
end;

procedure ProcSelectCampaignWAD(Sender: TGUIControl);
var
  win: TGUIWindow;
  a: TMegaWADInfo;
  wad, fn: String;
begin
  win := g_GUI_GetWindow('CampaignMenu');
  with TGUIMenu(win.GetControl('mCampaignMenu')) do
  begin
    wad := TGUIFileListBox(GetControl('lsWAD')).SelectedItem();

    if wad = '' then
    begin
      TGUILabel(GetControl('lbWADName')).Text := '';
      TGUILabel(GetControl('lbWADAuthor')).Text := '';
      TGUIMemo(GetControl('meWADDescription')).SetText('');
    end;

    a := g_Game_GetMegaWADInfo(wad);

    TGUILabel(GetControl('lbWADName')).Text := a.Name;
    TGUILabel(GetControl('lbWADAuthor')).Text := a.Author;
    TGUIMemo(GetControl('meWADDescription')).SetText(a.Description);

    TGUIImage(win.GetControl('mpWADImage')).ClearImage();

    if a.pic <> '' then
    begin
      g_ProcessResourceStr(a.pic, @fn, nil, nil);
      if fn = '' then
        TGUIImage(win.GetControl('mpWADImage')).SetImage(wad+a.pic)
      else
        TGUIImage(win.GetControl('mpWADImage')).SetImage(a.pic);
    end;
  end;
end;

procedure ProcChangeColor(Sender: TGUIControl);
var
  window: TGUIWindow;
begin
  window := g_GUI_GetWindow('OptionsPlayersP1Menu');
  with TGUIMenu(window.GetControl('mOptionsPlayersP1Menu')) do
    TGUIModelView(window.GetControl('mvP1Model')).SetColor(
                  Min(TGUIScroll(GetControl('scP1Red')).Value*16, 255),
                  Min(TGUIScroll(GetControl('scP1Green')).Value*16, 255),
                  Min(TGUIScroll(GetControl('scP1Blue')).Value*16, 255));

  window := g_GUI_GetWindow('OptionsPlayersP2Menu');
  with TGUIMenu(window.GetControl('mOptionsPlayersP2Menu')) do
    TGUIModelView(window.GetControl('mvP2Model')).SetColor(
                  Min(TGUIScroll(GetControl('scP2Red')).Value*16, 255),
                  Min(TGUIScroll(GetControl('scP2Green')).Value*16, 255),
                  Min(TGUIScroll(GetControl('scP2Blue')).Value*16, 255));
end;

procedure ProcSelectModel(Sender: TGUIControl);
var
  a: string;
  window: TGUIWindow;
begin
  window := g_GUI_GetWindow('OptionsPlayersP1Menu');
  a := TGUIListBox(TGUIMenu(window.GetControl('mOptionsPlayersP1Menu')).GetControl('lsP1Model')).SelectedItem;
  if a <> '' then TGUIModelView(window.GetControl('mvP1Model')).SetModel(a);

  window := g_GUI_GetWindow('OptionsPlayersP2Menu');
  a := TGUIListBox(TGUIMenu(window.GetControl('mOptionsPlayersP2Menu')).GetControl('lsP2Model')).SelectedItem;
  if a <> '' then TGUIModelView(window.GetControl('mvP2Model')).SetModel(a);

  ProcChangeColor(nil);
end;

procedure LoadFont(txtres, fntres: string; cwdt, chgt: Byte; spc: ShortInt;
                   var FontID: DWORD);
var
  CharID: DWORD;
  wad: TWADEditor_1;
  cfgdata, fntdata: Pointer;
  cfglen, fntlen: Integer;
  config: TConfig;
  chrwidth: Integer;
  a: Byte;
begin
  cfglen := 0;
  fntlen := 0;

  wad := TWADEditor_1.Create;
  if wad.ReadFile(GameWAD) then
  begin
    wad.GetResource('FONTS', txtres, cfgdata, cfglen);
    wad.GetResource('FONTS', fntres, fntdata, fntlen);
  end;
  wad.Free();

  if cfglen <> 0 then
  begin
    FontID := e_CharFont_Create(spc);

    config := TConfig.CreateMem(cfgdata, cfglen);
    for a := 0 to 255 do
    begin
      chrwidth := config.ReadInt(IntToStr(a), 'Width', 0);
      if chrwidth = 0 then Continue;

      if e_CreateTextureMemEx(fntdata, CharID, cwdt*(a mod 16), chgt*(a div 16),
                              cwdt, chgt) then
        e_CharFont_AddChar(FontID, CharID, Chr(a), chrwidth);
    end;

    config.Free();
  end;

  if cfglen <> 0 then FreeMem(cfgdata);
  if fntlen <> 0 then FreeMem(fntdata);
end;

procedure MenuLoadData();
begin
  e_WriteLog('Loading menu data...', MSG_NOTIFY);

  g_Texture_CreateWADEx('MAINMENU_MARKER1', GameWAD+':TEXTURES\MARKER1');
  g_Texture_CreateWADEx('MAINMENU_MARKER2', GameWAD+':TEXTURES\MARKER2');
  g_Texture_CreateWADEx('SCROLL_LEFT', GameWAD+':TEXTURES\SLEFT');
  g_Texture_CreateWADEx('SCROLL_RIGHT', GameWAD+':TEXTURES\SRIGHT');
  g_Texture_CreateWADEx('SCROLL_MIDDLE', GameWAD+':TEXTURES\SMIDDLE');
  g_Texture_CreateWADEx('SCROLL_MARKER', GameWAD+':TEXTURES\SMARKER');
  g_Texture_CreateWADEx('EDIT_LEFT', GameWAD+':TEXTURES\ELEFT');
  g_Texture_CreateWADEx('EDIT_RIGHT', GameWAD+':TEXTURES\ERIGHT');
  g_Texture_CreateWADEx('EDIT_MIDDLE', GameWAD+':TEXTURES\EMIDDLE');
  g_Texture_CreateWADEx('BOX1', GameWAD+':TEXTURES\BOX1');
  g_Texture_CreateWADEx('BOX2', GameWAD+':TEXTURES\BOX2');
  g_Texture_CreateWADEx('BOX3', GameWAD+':TEXTURES\BOX3');
  g_Texture_CreateWADEx('BOX4', GameWAD+':TEXTURES\BOX4');
  g_Texture_CreateWADEx('BOX5', GameWAD+':TEXTURES\BOX5');
  g_Texture_CreateWADEx('BOX6', GameWAD+':TEXTURES\BOX6');
  g_Texture_CreateWADEx('BOX7', GameWAD+':TEXTURES\BOX7');
  g_Texture_CreateWADEx('BOX8', GameWAD+':TEXTURES\BOX8');
  g_Texture_CreateWADEx('BOX9', GameWAD+':TEXTURES\BOX9');
  g_Texture_CreateWADEx('BSCROLL_UP_A', GameWAD+':TEXTURES\SCROLLUPA');
  g_Texture_CreateWADEx('BSCROLL_UP_U', GameWAD+':TEXTURES\SCROLLUPU');
  g_Texture_CreateWADEx('BSCROLL_DOWN_A', GameWAD+':TEXTURES\SCROLLDOWNA');
  g_Texture_CreateWADEx('BSCROLL_DOWN_U', GameWAD+':TEXTURES\SCROLLDOWNU');
  g_Texture_CreateWADEx('BSCROLL_MIDDLE', GameWAD+':TEXTURES\SCROLLMIDDLE');
  g_Texture_CreateWADEx('NOPIC', GameWAD+':TEXTURES\NOPIC');

  g_Sound_CreateWADEx('MENU_SELECT', GameWAD+':SOUNDS\MENUSELECT');
  g_Sound_CreateWADEx('MENU_OPEN', GameWAD+':SOUNDS\MENUOPEN');
  g_Sound_CreateWADEx('MENU_CLOSE', GameWAD+':SOUNDS\MENUCLOSE');
  g_Sound_CreateWADEx('MENU_CHANGE', GameWAD+':SOUNDS\MENUCHANGE');
  g_Sound_CreateWADEx('SCROLL_ADD', GameWAD+':SOUNDS\SCROLLADD');
  g_Sound_CreateWADEx('SCROLL_SUB', GameWAD+':SOUNDS\SCROLLSUB');
  g_Sound_CreateWADEx('SOUND_PLAYER_FALL', GameWAD+':SOUNDS\FALL');
end;

procedure MenuFreeData();
begin
  e_CharFont_Remove(gMenuFont);
  e_CharFont_Remove(gMenuSmallFont);

  g_Texture_Delete('MAINMENU_MARKER1');
  g_Texture_Delete('MAINMENU_MARKER2');
  g_Texture_Delete('SCROLL_LEFT');
  g_Texture_Delete('SCROLL_RIGHT');
  g_Texture_Delete('SCROLL_MIDDLE');
  g_Texture_Delete('SCROLL_MARKER');
  g_Texture_Delete('EDIT_LEFT');
  g_Texture_Delete('EDIT_RIGHT');
  g_Texture_Delete('EDIT_MIDDLE');
  g_Texture_Delete('BOX1');
  g_Texture_Delete('BOX2');
  g_Texture_Delete('BOX3');
  g_Texture_Delete('BOX4');
  g_Texture_Delete('BOX5');
  g_Texture_Delete('BOX6');
  g_Texture_Delete('BOX7');
  g_Texture_Delete('BOX8');
  g_Texture_Delete('BOX9');
  g_Texture_Delete('BSCROLL_UP_A');
  g_Texture_Delete('BSCROLL_UP_U');
  g_Texture_Delete('BSCROLL_DOWN_A');
  g_Texture_Delete('BSCROLL_DOWN_U');
  g_Texture_Delete('BSCROLL_MIDDLE');
  g_Texture_Delete('NOPIC');

  g_Sound_Delete('MENU_SELECT');
  g_Sound_Delete('MENU_OPEN');
  g_Sound_Delete('MENU_CLOSE');
  g_Sound_Delete('MENU_CHANGE');
  g_Sound_Delete('SCROLL_ADD');
  g_Sound_Delete('SCROLL_SUB');
  g_Sound_Delete('SOUND_PLAYER_FALL');
end;

procedure ProcAuthorsMenu();
begin
  gMusic.SetByName('MUSIC_INTERMUS');
  gMusic.Play();
end;

procedure ProcExitMenuKeyDown(Key: Byte);
var
  s: ShortString;
  snd: TPlayableSound;
  res: Boolean;
begin
  if Key = Ord('Y') then
  begin
    g_Game_StopAllSounds(True);
    case (Random(18)) of
      0: s := 'SOUND_MONSTER_PAIN';
      1: s := 'SOUND_MONSTER_DIE_3';
      2: s := 'SOUND_MONSTER_SLOP';
      3: s := 'SOUND_MONSTER_DEMON_DIE';
      4: s := 'SOUND_MONSTER_IMP_DIE_2';
      5: s := 'SOUND_MONSTER_MAN_DIE';
      6: s := 'SOUND_MONSTER_BSP_DIE';
      7: s := 'SOUND_MONSTER_VILE_DIE';
      8: s := 'SOUND_MONSTER_SKEL_DIE';
      9: s := 'SOUND_MONSTER_MANCUB_ALERT';
      10: s := 'SOUND_MONSTER_PAIN_PAIN';
      11: s := 'SOUND_MONSTER_BARON_DIE';
      12: s := 'SOUND_MONSTER_CACO_DIE';
      13: s := 'SOUND_MONSTER_CYBER_DIE';
      14: s := 'SOUND_MONSTER_KNIGHT_ALERT';
      15: s := 'SOUND_MONSTER_SPIDER_ALERT';
      else s := 'SOUND_PLAYER_FALL';
    end;

    snd := TPlayableSound.Create();
    res := snd.SetByName(s);
    if not res then
      res := snd.SetByName('SOUND_PLAYER_FALL');

    if res then
    begin
      snd.Play(True);
      while snd.IsPlaying() do
        ;
    end;

    g_Game_Quit();
  end
    else
      if Key = Ord('N') then
        g_GUI_HideWindow();
end;

procedure ProcLoadMenu();
var
  a: Integer;
begin
  for a := 1 to 8 do
    TGUIEdit(TGUIMenu(g_GUI_GetWindow('LoadMenu').GetControl('mmLoadMenu')).GetControl('edSlot'+IntToStr(a))).Text :=
    g_GetSaveName(a);
end;

procedure ProcSaveMenu();
var
  a: Integer;
begin
  for a := 1 to 8 do
    TGUIEdit(TGUIMenu(g_GUI_GetWindow('SaveMenu').GetControl('mmSaveMenu')).GetControl('edSlot'+IntToStr(a))).Text :=
    g_GetSaveName(a);
end;

procedure ProcSaveGame(Sender: TGUIControl);
var
  a: Integer;
begin
  if g_Game_IsNet then Exit;
  a := StrToInt(Copy(Sender.Name, Length(Sender.Name), 1));
  g_Game_PauseAllSounds(True);
  g_SaveGame(a, TGUIEdit(Sender).Text);

  g_ActiveWindow := nil;
  g_Game_Pause(False);
end;

procedure ProcLoadGame(Sender: TGUIControl);
var
  a: Integer;
begin
  if g_Game_IsNet then Exit;
  a := StrToInt(Copy(Sender.Name, Length(Sender.Name), 1));
  if g_LoadGame(a) then
    g_Game_PauseAllSounds(False)
  else // Не загрузилось - возврат в меню
    g_GUI_GetWindow('LoadMenu').SetActive(g_GUI_GetWindow('LoadMenu').GetControl('mmLoadMenu'));
end;

procedure ProcSingle1Player();
begin
  g_Game_StartSingle(MapsDir+'megawads\DOOM2D.WAD', 'MAP01', False, 1);
end;

procedure ProcSingle2Players();
begin
  g_Game_StartSingle(MapsDir+'megawads\DOOM2D.WAD', 'MAP01', True, 2);
end;

procedure ProcSelectMapMenu();
var
  menu: TGUIMenu;
  wad_lb: TGUIFileListBox;
  map_lb: TGUIListBox;
  map: String;
begin
  menu := TGUIMenu(g_GUI_GetWindow('SelectMapMenu').GetControl('mSelectMapMenu'));
  wad_lb := TGUIFileListBox(menu.GetControl('lsMapWAD'));
  map_lb := TGUIListBox(menu.GetControl('lsMapRes'));

  if wad_lb.SelectedItem() <> '' then
    map := map_lb.SelectedItem()
  else
    map := '';

  wad_lb.UpdateFileList();
  map_lb.Clear();

  if wad_lb.SelectedItem() <> '' then
  begin
    ProcSelectWAD(nil);
    map_lb.SelectItem(map);

    if map_lb.SelectedItem() <> '' then
      ProcSelectMap(nil);
  end;

  g_GUI_ShowWindow('SelectMapMenu');
end;

procedure ProcSelectCampaignMenu();
var
  menu: TGUIMenu;
  wad_lb: TGUIFileListBox;
begin
  menu := TGUIMenu(g_GUI_GetWindow('CampaignMenu').GetControl('mCampaignMenu'));
  wad_lb := TGUIFileListBox(menu.GetControl('lsWAD'));

  wad_lb.UpdateFileList();

  if wad_lb.SelectedItem() <> '' then
    ProcSelectCampaignWAD(nil);
end;

procedure ProcSetMap();
var
  wad, map, res: String;
begin
  with TGUIMenu(g_ActiveWindow.GetControl('mSelectMapMenu')) do
  begin
    wad := ExtractRelativePath(MapsDir, TGUIFileListBox(GetControl('lsMapWAD')).SelectedItem());
    map := TGUIListBox(GetControl('lsMapRes')).SelectedItem();
  end;

  if (wad = '') or (map = '') then
    Exit;

  res := wad+':\'+map;

  TGUILabel(TGUIMenu(g_GUI_GetWindow('CustomGameMenu').GetControl('mCustomGameMenu')).GetControl('lbMap')).Text := res;
  TGUILabel(TGUIMenu(g_GUI_GetWindow('NetServerMenu').GetControl('mNetServerMenu')).GetControl('lbMap')).Text := res;
end;

procedure ProcChangeSoundSettings(Sender: TGUIControl);
var
  menu: TGUIMenu;
begin
  menu := TGUIMenu(g_GUI_GetWindow('OptionsSoundMenu').GetControl('mOptionsSoundMenu'));

  g_Sound_SetupAllVolumes(
    Min(TGUIScroll(menu.GetControl('scSoundLevel')).Value*16, 255),
    Min(TGUIScroll(menu.GetControl('scMusicLevel')).Value*16, 255)
  );
end;

procedure ProcOptionsPlayersMIMenu();
var
  s, a: string;
  b: TModelInfo;
begin
  if g_ActiveWindow.Name = 'OptionsPlayersP1Menu' then s := 'P1' else s := 'P2';

  a := TGUIListBox(TGUIMenu(g_ActiveWindow.GetControl('mOptionsPlayers'+s+'Menu')).GetControl('ls'+s+'Model')).SelectedItem;

  if a = '' then Exit;

  b := g_PlayerModel_GetInfo(a); 

  with TGUIMenu(g_GUI_GetWindow('OptionsPlayersMIMenu').GetControl('mOptionsPlayersMIMenu')) do
  begin
    TGUILabel(GetControl('lbName')).Text := b.Name;
    TGUILabel(GetControl('lbAuthor')).Text := b.Author;
    TGUIMemo(GetControl('meComment')).SetText(b.Description);

    if b.HaveWeapon then
      TGUILabel(GetControl('lbWeapon')).Text := _lc[I_MENU_YES]
    else
      TGUILabel(GetControl('lbWeapon')).Text := _lc[I_MENU_NO];
  end;
 
  g_GUI_ShowWindow('OptionsPlayersMIMenu');
end;

procedure ProcOptionsPlayersAnim();
var
  s: String;
begin
  if g_ActiveWindow.Name = 'OptionsPlayersP1Menu' then
    s := 'P1'
  else
    s := 'P2';

  with TGUIModelView(g_ActiveWindow.GetControl('mv'+s+'Model')) do
  begin
    NextAnim();
    Model.GetCurrentAnimation.Loop := True;
    Model.GetCurrentAnimationMask.Loop := True;
  end;
end;

procedure ProcOptionsPlayersWeap();
var
  s: String;
begin
  if g_ActiveWindow.Name = 'OptionsPlayersP1Menu' then
    s := 'P1'
  else
    s := 'P2';

  with TGUIModelView(g_ActiveWindow.GetControl('mv'+s+'Model')) do
    NextWeapon();
end;

procedure ProcOptionsPlayersRot();
var
  s: string;
begin
  if g_ActiveWindow.Name = 'OptionsPlayersP1Menu' then s := 'P1' else s := 'P2';
  with TGUIModelView(g_ActiveWindow.GetControl('mv'+s+'Model')).Model do
    if Direction = D_LEFT then Direction := D_RIGHT else Direction := D_LEFT;
end;

procedure ProcDefaultMenuKeyDown(Key: Byte);
begin
  if Key = Ord('Y') then
  begin
    g_Options_SetDefault();
    ReadOptions();
    g_GUI_HideWindow();
  end else
    if Key = Ord('N') then g_GUI_HideWindow;
end;

procedure ProcSavedMenuKeyDown(Key: Byte);
begin
  if Key = Ord('Y') then
  begin
    ReadOptions();
    g_GUI_HideWindow();
  end else
    if Key = Ord('N') then g_GUI_HideWindow;
end;

procedure ProcAuthorsClose();
begin
  gMusic.SetByName('MUSIC_MENU');
  gMusic.Play();
  gState := STATE_MENU;
end;

procedure ProcGMClose();
begin
  g_Game_InGameMenu(False);
end;

procedure ProcGMShow();
begin
  if (gGameSettings.GameType = GT_SINGLE) and
     ((gPlayer1 = nil) or (not gPlayer1.Live)) and
     ((gPlayer2 = nil) or (not gPlayer2.Live)) then
    TGUIMainMenu(g_ActiveWindow.GetControl(
      g_ActiveWindow.DefControl )).EnableButton('save', False)
  else
    TGUIMainMenu(g_ActiveWindow.GetControl(
      g_ActiveWindow.DefControl )).EnableButton('save', True);
end;

procedure ProcRestartMenuKeyDown(Key: Byte);
begin
  if Key = Ord('Y') then g_Game_Restart()
    else if Key = Ord('N') then g_GUI_HideWindow;
end;

procedure ProcEndMenuKeyDown(Key: Byte);
begin
  if Key = Ord('Y') then gExit := EXIT_SIMPLE
    else if Key = Ord('N') then g_GUI_HideWindow;
end;

procedure ProcSetRussianLanguage();
var
  config: TConfig;
begin
  if gLanguage <> LANGUAGE_RUSSIAN then
  begin
    gLanguage := LANGUAGE_RUSSIAN;
    gLanguageChange := True;
    gAskLanguage := False;

    config := TConfig.CreateFile(GameDir+'\'+CONFIG_FILENAME);
    config.WriteStr('Game', 'Language', gLanguage);
    config.SaveFile(GameDir+'\'+CONFIG_FILENAME);
    config.Free();

  // Сохраняем изменения всех настроек:
    ProcApplyOptions();
  end;
end;

procedure ProcSetEnglishLanguage();
var
  config: TConfig;
begin
  if gLanguage <> LANGUAGE_ENGLISH then
  begin
    gLanguage := LANGUAGE_ENGLISH;
    gLanguageChange := True;
    gAskLanguage := False;

    config := TConfig.CreateFile(GameDir+'\'+CONFIG_FILENAME);
    config.WriteStr('Game', 'Language', gLanguage);
    config.SaveFile(GameDir+'\'+CONFIG_FILENAME);
    config.Free();

  // Сохраняем изменения всех настроек:
    ProcApplyOptions();
  end;
end;

procedure ReadGameSettings();
var
  menu: TGUIMenu;
begin
  menu := TGUIMenu(g_GUI_GetWindow('GameSetGameMenu').GetControl('mGameSetGameMenu'));

  with gGameSettings do
  begin
    with TGUISwitch(menu.GetControl('swTeamDamage')) do
      if LongBool(Options and GAME_OPTION_TEAMDAMAGE) then
        ItemIndex := 0
      else
        ItemIndex := 1;

    TGUIEdit(menu.GetControl('edTimeLimit')).Text := IntToStr(TimeLimit);
    TGUIEdit(menu.GetControl('edGoalLimit')).Text := IntToStr(GoalLimit);
    TGUIEdit(menu.GetControl('edMaxLives')).Text := IntToStr(MaxLives);

    with TGUISwitch(menu.GetControl('swBotsVS')) do
      if LongBool(Options and GAME_OPTION_BOTVSPLAYER) and
         LongBool(Options and GAME_OPTION_BOTVSMONSTER) then
        ItemIndex := 2
      else
        if LongBool(Options and GAME_OPTION_BOTVSMONSTER) then
          ItemIndex := 1
        else
          ItemIndex := 0;

    if GameType in [GT_CUSTOM, GT_SERVER] then
      begin
        TGUISwitch(menu.GetControl('swTeamDamage')).Enabled := True;
        TGUIEdit(menu.GetControl('edTimeLimit')).Enabled := True;
        TGUILabel(menu.GetControlsText('edTimeLimit')).Color := MENU_ITEMSTEXT_COLOR;
        TGUIEdit(menu.GetControl('edGoalLimit')).Enabled := True;
        TGUILabel(menu.GetControlsText('edGoalLimit')).Color := MENU_ITEMSTEXT_COLOR;
        TGUIEdit(menu.GetControl('edMaxLives')).Enabled := True;
        TGUILabel(menu.GetControlsText('edMaxLives')).Color := MENU_ITEMSTEXT_COLOR;
        TGUISwitch(menu.GetControl('swBotsVS')).Enabled := True;
      end
    else
      begin
        TGUISwitch(menu.GetControl('swTeamDamage')).Enabled := True;
        with TGUIEdit(menu.GetControl('edTimeLimit')) do
        begin
          Enabled := False;
          Text := '';
        end;
        TGUILabel(menu.GetControlsText('edTimeLimit')).Color := MENU_UNACTIVEITEMS_COLOR;
        with TGUIEdit(menu.GetControl('edGoalLimit')) do
        begin
          Enabled := False;
          Text := '';
        end;
        TGUILabel(menu.GetControlsText('edGoalLimit')).Color := MENU_UNACTIVEITEMS_COLOR;
        with TGUIEdit(menu.GetControl('edMaxLives')) do
        begin
          Enabled := False;
          Text := '';
        end;
        TGUILabel(menu.GetControlsText('edMaxLives')).Color := MENU_UNACTIVEITEMS_COLOR;
        TGUISwitch(menu.GetControl('swBotsVS')).Enabled := True;
      end;
  end;
end;

procedure ProcApplyGameSet();
var
  menu: TGUIMenu;
  a, b, n: Integer;
  stat: TPlayerStatArray;
begin
  menu := TGUIMenu(g_GUI_GetWindow('GameSetGameMenu').GetControl('mGameSetGameMenu'));

  if not g_Game_IsServer then Exit;

  with gGameSettings do
  begin
    if TGUISwitch(menu.GetControl('swTeamDamage')).Enabled then
    begin
      if TGUISwitch(menu.GetControl('swTeamDamage')).ItemIndex = 0 then
        Options := Options or GAME_OPTION_TEAMDAMAGE
      else
        Options := Options and (not GAME_OPTION_TEAMDAMAGE);
    end;

    if TGUIEdit(menu.GetControl('edTimeLimit')).Enabled then
    begin
      n := StrToIntDef(TGUIEdit(menu.GetControl('edTimeLimit')).Text, TimeLimit);

      if n = 0 then
        TimeLimit := 0
      else
        begin
          b := (gTime - gGameStartTime) div 1000 + 10; // 10 секунд на смену

          TimeLimit := Max(n, b);
        end;
    end;

    if TGUIEdit(menu.GetControl('edGoalLimit')).Enabled then
    begin
      n := StrToIntDef(TGUIEdit(menu.GetControl('edGoalLimit')).Text, GoalLimit);

      if n = 0 then
        GoalLimit := 0
      else
        begin
          b := 0;
          if GameMode = GM_DM then
            begin // DM
              stat := g_Player_GetStats();
              if stat <> nil then
                for a := 0 to High(stat) do
                  if stat[a].Frags > b then
                    b := stat[a].Frags;
            end
          else // CTF
            b := Max(gTeamStat[TEAM_RED].Goals, gTeamStat[TEAM_BLUE].Goals);

          GoalLimit := Max(n, b);
        end;
    end;

    if TGUIEdit(menu.GetControl('edMaxLives')).Enabled then
    begin
      n := StrToIntDef(TGUIEdit(menu.GetControl('edMaxLives')).Text, GoalLimit);
      if n < 0 then n := 0;
      if n > 255 then n := 255;
      if n = 0 then
        MaxLives := 0
      else
        begin
          b := 0;
          stat := g_Player_GetStats();
          if stat <> nil then
            for a := 0 to High(stat) do
              if stat[a].Lives > b then
                b := stat[a].Lives;

          MaxLives := Max(n, b);
        end;
    end;

    if TGUISwitch(menu.GetControl('swBotsVS')).Enabled then
    begin
      case TGUISwitch(menu.GetControl('swBotsVS')).ItemIndex of
        1:
          begin
            Options := Options and (not GAME_OPTION_BOTVSPLAYER);
            Options := Options or GAME_OPTION_BOTVSMONSTER;
          end;
        2:
          begin
            Options := Options or GAME_OPTION_BOTVSPLAYER;
            Options := Options or GAME_OPTION_BOTVSMONSTER;
          end;
        else
          begin
            Options := Options or GAME_OPTION_BOTVSPLAYER;
            Options := Options and (not GAME_OPTION_BOTVSMONSTER);
          end;
      end;
    end;
  end;

  if g_Game_IsNet then MH_SEND_GameSettings;
end;

function GetDisplayModes(dBPP: DWORD; var SelRes: DWORD): SArray;
var
  n, i, j: Word;
  dm: DevMode;
  DModes: Array of record
                     Width: DWORD;
                     Height: DWORD;
                   end;
begin
  SetLength(Result, 0);
  SetLength(DModes, 0);
  n := 0;
  dm.dmSize := SizeOf(DevMode); // = 156

// Составляем список всех разрешений:
  while EnumDisplaySettings(nil, n, dm) do
  begin
    if (dm.dmBitsPerPel = dBPP) then
    begin
      SetLength(DModes, Length(DModes)+1);
      with DModes[High(DModes)] do
      begin
        Width := dm.dmPelsWidth;
        Height := dm.dmPelsHeight;
      end;
    end;

    Inc(n);
    dm.dmSize := SizeOf(DevMode);
  end;

// Удаляем повторяющиеся (которые с разными частотами):
  for i := 0 to High(DModes) do
    if DModes[i].Width > 0 then
    // Слишком маленький размер:
      if (DModes[i].Width < 640) or
         (DModes[i].Height < 480) then
        DModes[i].Width := 0
      else
        for j := i+1 to High(DModes) do
          if DModes[j].Width > 0 then
          // Такое разрешение уже в списке:
            if (DModes[i].Width = DModes[j].Width) and
               (DModes[i].Height = DModes[j].Height) then
              DModes[j].Width := 0;

  SelRes := 0;
// Возвращаем оставшиеся:
  for i := 0 to High(DModes) do
    if DModes[i].Width > 0 then
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := IntToStr(DModes[i].Width) +
        ' x ' + IntToStr(DModes[i].Height);

      if (DModes[i].Width = gScreenWidth) and
         (DModes[i].Height = gScreenHeight) then
        SelRes := High(Result);
    end;

  SetLength(DModes, 0);
end;

procedure ProcVideoOptionsRes();
var
  menu: TGUIMenu;
  list: SArray;
  SR: DWORD;
begin
  menu := TGUIMenu(g_GUI_GetWindow('OptionsVideoResMenu').GetControl('mOptionsVideoResMenu'));

  TGUILabel(menu.GetControl('lbCurrentRes')).Text :=
    IntToStr(gScreenWidth) +
    ' x ' + IntToStr(gScreenHeight) +
    ', ' + IntToStr(gBPP) + ' bpp';

  with TGUIListBox(menu.GetControl('lsResolution')) do
  begin
    list := GetDisplayModes(gBPP, SR);

    if list <> nil then
      begin
        Items := list;
        ItemIndex := SR;
      end
    else
      Clear();
  end;

  with TGUISwitch(menu.GetControl('swFullScreen')) do
    if gFullscreen then
      ItemIndex := 0
    else
      ItemIndex := 1;
end;

procedure ProcApplyVideoOptions();
var
  menu: TGUIMenu;
  Fullscreen: Boolean;
  SWidth, SHeight: Word;
  str: String;
begin
  menu := TGUIMenu(g_GUI_GetWindow('OptionsVideoResMenu').GetControl('mOptionsVideoResMenu'));

  str := TGUIListBox(menu.GetControl('lsResolution')).SelectedItem;
  SWidth := StrToIntDef(Copy(str, 1, Pos('x', str)-2), gScreenWidth);
  Delete(str, 1, Pos('x', str)+1);
  SHeight := StrToIntDef(Copy(str, 1, Length(str)), gScreenHeight);

  Fullscreen := TGUISwitch(menu.GetControl('swFullScreen')).ItemIndex = 0;

  if (SWidth <> gScreenWidth) or
     (SHeight <> gScreenHeight) or
     (Fullscreen <> gFullscreen) then
  begin
    gResolutionChange := True;
    gRC_Width := SWidth;
    gRC_Height := SHeight;
    gRC_FullScreen := Fullscreen;
    gRC_Maximized := gWinMaximized;
  end;

// Сохраняем изменения всех настроек:
  ProcApplyOptions();
end;

function CreateYNMenu(Name, Text: String; MaxLen: Word; FontID: DWORD;
                      KeyDownProc: Pointer): TGUIWindow;
var
  a: Integer;
  h, _x: Word;
  lines: SArray;
begin
  Result := TGUIWindow.Create(Name);

  with Result do
  begin
    OnKeyDown := KeyDownProc;

    lines := GetLines(Text, FontID, MaxLen);

    h := e_CharFont_GetMaxHeight(FontID);
    _x := (gScreenHeight div 2)-(h*Length(lines) div 2);

    if lines <> nil then
    begin
      for a := 0 to High(lines) do
        with TGUILabel(Result.AddChild(TGUILabel.Create(lines[a], FontID))) do
        begin
          X := (gScreenWidth div 2)-(GetWidth div 2);
          Y := _x;
          Color := _RGB(255, 0, 0);
          _x := _x+h;
        end;

      with TGUILabel(Result.AddChild(TGUILabel.Create('(Y\N)', FontID))) do
      begin
        X := (gScreenWidth div 2)-(GetWidth div 2);
        Y := _x;
        Color := _RGB(255, 0, 0);
      end;
    end;

    DefControl := '';
    SetActive(nil);
  end;
end;

procedure ProcSetFirstRussianLanguage();
var
  config: TConfig;
begin
  gLanguage := LANGUAGE_RUSSIAN;
  gLanguageChange := True;
  gAskLanguage := False;

  config := TConfig.CreateFile(GameDir+'\'+CONFIG_FILENAME);
  config.WriteStr('Game', 'Language', gLanguage);
  config.SaveFile(GameDir+'\'+CONFIG_FILENAME);
  config.Free();
end;

procedure ProcSetFirstEnglishLanguage();
var
  config: TConfig;
begin
  gLanguage := LANGUAGE_ENGLISH;
  gLanguageChange := True;
  gAskLanguage := False;

  config := TConfig.CreateFile(GameDir+'\'+CONFIG_FILENAME);
  config.WriteStr('Game', 'Language', gLanguage);
  config.SaveFile(GameDir+'\'+CONFIG_FILENAME);
  config.Free();
end;

procedure ProcRecallAddress();
begin
  with TGUIMenu(g_GUI_GetWindow('NetClientMenu').GetControl('mNetClientMenu')) do
  begin
    TGUIEdit(GetControl('edIP')).Text := NetLastIP;
    TGUIEdit(GetControl('edPort')).Text := IntToStr(NetLastPort);
  end;
end;

procedure CreateFirstLanguageMenu();
var
  Menu: TGUIWindow;
begin
  Menu := TGUIWindow.Create('FirstLanguageMenu');

  with TGUIMainMenu(Menu.AddChild(TGUIMainMenu.Create(gMenuFont, ' '))) do
  begin
    Name := 'mmFirstLanguageMenu';
    AddButton(@ProcSetFirstRussianLanguage, 'Русский', '');
    AddButton(@ProcSetFirstEnglishLanguage, 'English', '');
  end;

  Menu.DefControl := 'mmFirstLanguageMenu';
  Menu.MainWindow := True;
  g_GUI_AddWindow(Menu);
end;

procedure g_Menu_AskLanguage();
begin
  CreateFirstLanguageMenu();
  g_GUI_ShowWindow('FirstLanguageMenu');
end;

procedure CreatePlayerOptionsMenu(s: String);
var
  Menu: TGUIWindow;
  a: String;
begin
  Menu := TGUIWindow.Create('OptionsPlayers'+s+'Menu');
  if s = 'P1' then
    a := _lc[I_MENU_PLAYER_1]
  else
    a := _lc[I_MENU_PLAYER_2];
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, a))) do
  begin
    Name := 'mOptionsPlayers'+s+'Menu';
    with AddEdit(_lc[I_MENU_PLAYER_NAME]) do
    begin
      Name := 'ed'+s+'Name';
      MaxLength := 12;
      Width := 12;
    end;
    with AddSwitch(_lc[I_MENU_PLAYER_TEAM]) do
    begin
      Name := 'sw'+s+'Team';
      AddItem(_lc[I_MENU_PLAYER_TEAM_RED]);
      AddItem(_lc[I_MENU_PLAYER_TEAM_BLUE]);
    end ;
    with AddList(_lc[I_MENU_PLAYER_MODEL], 12, 6) do
    begin
      Name := 'ls'+s+'Model';
      Sort := True;
      Items := g_PlayerModel_GetNames();
      OnChange := ProcSelectModel;
    end;
    with AddScroll(_lc[I_MENU_PLAYER_RED]) do
    begin
      Name := 'sc'+s+'Red';
      Max := 16;
      OnChange := ProcChangeColor;
    end;
    with AddScroll(_lc[I_MENU_PLAYER_GREEN]) do
    begin
      Name := 'sc'+s+'Green';
      Max := 16;
      OnChange := ProcChangeColor;
    end;
    with AddScroll(_lc[I_MENU_PLAYER_BLUE]) do
    begin
      Name := 'sc'+s+'Blue';
      Max := 16;
      OnChange := ProcChangeColor;
    end;
    AddSpace();
    AddButton(@ProcOptionsPlayersMIMenu, _lc[I_MENU_MODEL_INFO]);
    AddButton(@ProcOptionsPlayersAnim, _lc[I_MENU_MODEL_ANIMATION]);
    AddButton(@ProcOptionsPlayersWeap, _lc[I_MENU_MODEL_CHANGE_WEAPON]);
    AddButton(@ProcOptionsPlayersRot, _lc[I_MENU_MODEL_ROTATE]);
  
    with TGUIModelView(Menu.AddChild(TGUIModelView.Create)) do
    begin
      Name := 'mv'+s+'Model';
      X := GetControl('ls'+s+'Model').X+TGUIListBox(GetControl('ls'+s+'Model')).GetWidth+16;
      Y := GetControl('ls'+s+'Model').Y;
    end;
  end;
  Menu.DefControl := 'mOptionsPlayers'+s+'Menu';
  g_GUI_AddWindow(Menu);
end;

procedure CreateAllMenus();
var
  Menu: TGUIWindow;
  SR: TSearchRec;
  a, cx, _y, lsi: Integer;
  list: SArray;
begin
  Menu := TGUIWindow.Create('MainMenu');
  with TGUIMainMenu(Menu.AddChild(TGUIMainMenu.Create(gMenuFont, _lc[I_MENU_MAIN_MENU]))) do
  begin
    Name := 'mmMainMenu';
    AddButton(nil, _lc[I_MENU_NEW_GAME], 'NewGameMenu');
    AddButton(nil, _lc[I_MENU_MULTIPLAYER], 'NetGameMenu');
    AddButton(nil, _lc[I_MENU_LOAD_GAME], 'LoadMenu');
    AddButton(@ReadOptions, _lc[I_MENU_OPTIONS], 'OptionsMenu');
    AddButton(@ProcAuthorsMenu, _lc[I_MENU_AUTHORS], 'AuthorsMenu');
    AddButton(nil, _lc[I_MENU_EXIT], 'ExitMenu');
  end;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(Format(_lc[I_VERSION], [GAME_VERSION]), gMenuSmallFont))) do
  begin
    Color := _RGB(255, 255, 255);
    X := gScreenWidth-GetWidth-8;
    Y := gScreenHeight-GetHeight-8;
  end;
  Menu.DefControl := 'mmMainMenu';
  Menu.MainWindow := True;
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('NewGameMenu');
  with TGUIMainMenu(Menu.AddChild(TGUIMainMenu.Create(gMenuFont, _lc[I_MENU_NEW_GAME]))) do
  begin
    Name := 'mmNewGameMenu';
    AddButton(@ProcSingle1Player, _lc[I_MENU_1_PLAYER]);
    AddButton(@ProcSingle2Players, _lc[I_MENU_2_PLAYERS]);
    AddButton(nil, _lc[I_MENU_CUSTOM_GAME], 'CustomGameMenu');
    AddButton(@ProcSelectCampaignMenu, _lc[I_MENU_CAMPAIGN], 'CampaignMenu');
  end;
  Menu.DefControl := 'mmNewGameMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('NetGameMenu');
  with TGUIMainMenu(Menu.AddChild(TGUIMainMenu.Create(gMenuFont, _lc[I_MENU_MULTIPLAYER]))) do
  begin
    Name := 'mmNetGameMenu';
    AddButton(nil, _lc[I_MENU_START_SERVER], 'NetServerMenu');
    AddButton(@ProcRecallAddress, _lc[I_MENU_START_CLIENT], 'NetClientMenu');
  end;
  Menu.DefControl := 'mmNetGameMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('NetServerMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_START_SERVER]))) do
  begin
    Name := 'mNetServerMenu';
    with AddEdit(_lc[I_NET_SERVER_NAME]) do
    begin
      Name := 'edSrvName';
      OnlyDigits := False;
      Width := 16;
      MaxLength := 64;
      Text := NetServerName;
    end;
    with AddEdit(_lc[I_NET_SERVER_PASSWORD]) do
    begin
      Name := 'edSrvPassword';
      OnlyDigits := False;
      Width := 16;
      MaxLength := 24;
      Text := NetPassword;
    end;
    with AddEdit(_lc[I_NET_PORT]) do
    begin
      Name := 'edPort';
      OnlyDigits := True;
      Width := 4;
      MaxLength := 5;
      Text := IntToStr(NetPort);
    end;
    with AddEdit(_lc[I_NET_MAX_CLIENTS]) do
    begin
      Name := 'edMaxPlayers';
      OnlyDigits := True;
      Width := 4;
      MaxLength := 2;
      Text := IntToStr(NetMaxClients);
    end;
    AddSpace();
    with AddLabel(_lc[I_MENU_MAP]) do
    begin
      Name := 'lbMap';
      FixedLength := 16;
      OnClick := @ProcSelectMapMenu;
    end;
    with AddSwitch(_lc[I_MENU_GAME_TYPE]) do
    begin
      Name := 'swGameMode';
      AddItem(_lc[I_MENU_GAME_TYPE_DM]);
      AddItem(_lc[I_MENU_GAME_TYPE_TDM]);
      AddItem(_lc[I_MENU_GAME_TYPE_CTF]);
      AddItem(_lc[I_MENU_GAME_TYPE_COOP]);
      ItemIndex := 0;
    end;
    with AddEdit(_lc[I_MENU_TIME_LIMIT]) do
    begin
      Name := 'edTimeLimit';
      OnlyDigits := True;
      Width := 3;
      MaxLength := 3;
    end;
    with AddEdit(_lc[I_MENU_GOAL_LIMIT]) do
    begin
      Name := 'edGoalLimit';
      OnlyDigits := True;
      Width := 3;
      MaxLength := 3;
    end;
    with AddEdit(_lc[I_MENU_MAX_LIVES]) do
    begin
      Name := 'edMaxLives';
      OnlyDigits := True;
      Width := 3;
      MaxLength := 3;
    end;
    with AddSwitch(_lc[I_MENU_TEAM_DAMAGE]) do
    begin
      Name := 'swTeamDamage';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
      ItemIndex := 1;
    end;
    with AddSwitch(_lc[I_MENU_ENABLE_EXITS]) do
    begin
      Name := 'swEnableExits';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
      ItemIndex := 0;
    end;
    with AddSwitch(_lc[I_MENU_WEAPONS_STAY]) do
    begin
      Name := 'swWeaponStay';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
      ItemIndex := 1;
    end;
    with AddSwitch(_lc[I_MENU_ENABLE_MONSTERS]) do
    begin
      Name := 'swMonsterDM';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
      ItemIndex := 1;
    end;
    with AddSwitch(_lc[I_MENU_BOTS_VS]) do
    begin
      Name := 'swBotsVS';
      AddItem(_lc[I_MENU_BOTS_VS_PLAYERS]);
      AddItem(_lc[I_MENU_BOTS_VS_MONSTERS]);
      AddItem(_lc[I_MENU_BOTS_VS_ALL]);
      ItemIndex := 2;
    end;
    AddSpace();
    AddButton(@ProcStartNetGame, _lc[I_MENU_START_GAME]);

    ReAlign();
  end;
  Menu.DefControl := 'mNetServerMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('NetClientMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_START_CLIENT]))) do
  begin
    Name := 'mNetClientMenu';
  
    AddButton(@ProcServerlist, _lc[I_NET_SLIST]);
    AddSpace();
  
    with AddEdit(_lc[I_NET_ADDRESS]) do
    begin
      Name := 'edIP';
      OnlyDigits :=False;
      Width := 12;
      MaxLength := 64;
      Text := 'localhost';
    end;
    with AddEdit(_lc[I_NET_PORT]) do
    begin
      Name := 'edPort';
      OnlyDigits := True;
      Width := 4;
      MaxLength := 5;
      Text := '25666';
    end;
    with AddEdit(_lc[I_NET_SERVER_PASSWORD]) do
    begin
      Name := 'edPW';
      OnlyDigits := False;
      Width := 12;
      MaxLength := 32;
      Text := '';
    end;

    AddSpace();
    AddButton(@ProcConnectNetGame, _lc[I_MENU_START_GAME]);

    ReAlign();
  end;
  Menu.DefControl := 'mNetClientMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('LoadMenu');
  Menu.OnShow := ProcLoadMenu;
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_LOAD_GAME]))) do
  begin
    Name := 'mmLoadMenu';

    for a := 1 to 8 do
      with AddEdit('') do
      begin
        Name := 'edSlot'+IntToStr(a);
        Width := 16;
        MaxLength := 16;
        OnEnter := ProcLoadGame;
      end;
  end;
  Menu.DefControl := 'mmLoadMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('SaveMenu');
  Menu.OnShow := ProcSaveMenu;
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_SAVE_GAME]))) do
  begin
    Name := 'mmSaveMenu';

    for a := 1 to 8 do
      with AddEdit('') do
      begin
        Name := 'edSlot'+IntToStr(a);
        Width := 16;
        MaxLength := 16;
        OnChange := ProcSaveGame;
      end;
  end;
  Menu.DefControl := 'mmSaveMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('CustomGameMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_CUSTOM_GAME]))) do
  begin
    Name := 'mCustomGameMenu';
    with AddLabel(_lc[I_MENU_MAP]) do
    begin
      Name := 'lbMap';
      FixedLength := 16;
      OnClick := @ProcSelectMapMenu;
    end;
    with AddSwitch(_lc[I_MENU_GAME_TYPE]) do
    begin
      Name := 'swGameMode';
      AddItem(_lc[I_MENU_GAME_TYPE_DM]);
      AddItem(_lc[I_MENU_GAME_TYPE_TDM]);
      AddItem(_lc[I_MENU_GAME_TYPE_CTF]);
      AddItem(_lc[I_MENU_GAME_TYPE_COOP]);
      ItemIndex := 0;
    end;
    with AddEdit(_lc[I_MENU_TIME_LIMIT]) do
    begin
      Name := 'edTimeLimit';
      OnlyDigits := True;
      Width := 3;
      MaxLength := 3;
    end;
    with AddEdit(_lc[I_MENU_GOAL_LIMIT]) do
    begin
      Name := 'edGoalLimit';
      OnlyDigits := True;
      Width := 3;
      MaxLength := 3;
    end;
    with AddEdit(_lc[I_MENU_MAX_LIVES]) do
    begin
      Name := 'edMaxLives';
      OnlyDigits := True;
      Width := 3;
      MaxLength := 3;
    end;
    with AddSwitch(_lc[I_MENU_PLAYERS]) do
    begin
      Name := 'swPlayers';
      AddItem(_lc[I_MENU_PLAYERS_ONE]);
      AddItem(_lc[I_MENU_PLAYERS_TWO]);
      ItemIndex := 0;
    end;
    with AddSwitch(_lc[I_MENU_TEAM_DAMAGE]) do
    begin
      Name := 'swTeamDamage';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
      ItemIndex := 1;
    end;
    with AddSwitch(_lc[I_MENU_ENABLE_EXITS]) do
    begin
      Name := 'swEnableExits';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
      ItemIndex := 0;
    end;
    with AddSwitch(_lc[I_MENU_WEAPONS_STAY]) do
    begin
      Name := 'swWeaponStay';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
      ItemIndex := 1;
    end;
    with AddSwitch(_lc[I_MENU_ENABLE_MONSTERS]) do
    begin
      Name := 'swMonsterDM';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
      ItemIndex := 1;
    end;
    with AddSwitch(_lc[I_MENU_BOTS_VS]) do
    begin
      Name := 'swBotsVS';
      AddItem(_lc[I_MENU_BOTS_VS_PLAYERS]);
      AddItem(_lc[I_MENU_BOTS_VS_MONSTERS]);
      AddItem(_lc[I_MENU_BOTS_VS_ALL]);
      ItemIndex := 2;
    end;
    AddSpace();
    AddButton(@ProcStartCustomGame, _lc[I_MENU_START_GAME]);

    ReAlign();
  end;
  Menu.DefControl := 'mCustomGameMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('CampaignMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_CAMPAIGN]))) do
  begin
    Name := 'mCampaignMenu';

    AddSpace();
    AddSpace();
    AddSpace();
    AddSpace();
    AddSpace();
    AddSpace();

    with AddFileList('', 15, 4) do
    begin
      Name := 'lsWAD';
      OnChange := ProcSelectCampaignWAD;

      Sort := True;
      Dirs := True;
      FileMask := '*.wad';
      SetBase(MapsDir+'megawads\');
    end;

    with AddLabel(_lc[I_MENU_MAP_NAME]) do
    begin
      Name := 'lbWADName';
      FixedLength := 8;
      Enabled := False;
    end;
    with AddLabel(_lc[I_MENU_MAP_AUTHOR]) do
    begin
      Name := 'lbWADAuthor';
      FixedLength := 8;
      Enabled := False;
    end;
    AddLine(_lc[I_MENU_MAP_DESCRIPTION]);
    with AddMemo('', 15, 3) do
    begin
      Name := 'meWADDescription';
      Color := MENU_ITEMSCTRL_COLOR;
    end;
    with AddSwitch(_lc[I_MENU_PLAYERS]) do
    begin
      Name := 'swPlayers';
      AddItem(_lc[I_MENU_PLAYERS_ONE]);
      AddItem(_lc[I_MENU_PLAYERS_TWO]);
    end;
    AddSpace();
    AddButton(@ProcStartCampaign, _lc[I_MENU_START_GAME]);

    ReAlign();

    with TGUIImage(Menu.AddChild(TGUIImage.Create)) do
    begin
      Name := 'mpWADImage';
      DefaultRes := 'NOPIC';
      X := GetControl('lsWAD').X+4;
      Y := GetControl('lsWAD').Y-128-MENU_VSPACE;
    end;
  end;
  Menu.DefControl := 'mCampaignMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('SelectMapMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_SELECT_MAP]))) do
  begin
    Name := 'mSelectMapMenu';
    with AddFileList(_lc[I_MENU_MAP_WAD], 12, 4) do
    begin
      Name := 'lsMapWAD';
      OnChange := ProcSelectWAD;

      Sort := True;
      Dirs := True;
      FileMask := '*.wad';
      SetBase(MapsDir);
    end;
    with AddList(_lc[I_MENU_MAP_RESOURCE], 12, 4) do
    begin
      Name := 'lsMapRes';
      Sort := True;
      OnChange := ProcSelectMap;
    end;
    AddSpace();
    with AddLabel(_lc[I_MENU_MAP_NAME]) do
    begin
      Name := 'lbMapName';
      FixedLength := 24;
      Enabled := False;
    end;
    with AddLabel(_lc[I_MENU_MAP_AUTHOR]) do
    begin
      Name := 'lbMapAuthor';
      FixedLength := 16;
      Enabled := False;
    end;
    with AddLabel(_lc[I_MENU_MAP_SIZE]) do
    begin
      Name := 'lbMapSize';
      FixedLength := 10;
      Enabled := False;
    end;
    with AddMemo(_lc[I_MENU_MAP_DESCRIPTION], 12, 4) do
    begin
      Name := 'meMapDescription';
    end;

    ReAlign();

    with TGUIMapPreview(Menu.AddChild(TGUIMapPreview.Create)) do
    begin
      Name := 'mpMapPreview';
      X := GetControl('lsMapWAD').X+TGUIListBox(GetControl('lsMapWAD')).GetWidth()+2;
      Y := GetControl('lsMapWAD').Y;
    end;
    with TGUILabel(Menu.AddChild(TGUILabel.Create('', gMenuSmallFont))) do
    begin
      Name := 'lbMapScale';
      FixedLength := 8;
      Enabled := False;
      Color := MENU_ITEMSCTRL_COLOR;
      X := GetControl('lsMapWAD').X +
        TGUIListBox(GetControl('lsMapWAD')).GetWidth() +
        2 + MAPPREVIEW_WIDTH*4;
      Y := GetControl('lsMapWAD').Y + MAPPREVIEW_HEIGHT*16 + 16;
    end;
  end;
  Menu.OnClose := ProcSetMap;
  Menu.DefControl := 'mSelectMapMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('OptionsMenu');
  with TGUIMainMenu(Menu.AddChild(TGUIMainMenu.Create(gMenuFont, _lc[I_MENU_OPTIONS]))) do
  begin
    Name := 'mmOptionsMenu';
    AddButton(nil, _lc[I_MENU_VIDEO_OPTIONS], 'OptionsVideoMenu');
    AddButton(nil, _lc[I_MENU_SOUND_OPTIONS], 'OptionsSoundMenu');
    AddButton(nil, _lc[I_MENU_GAME_OPTIONS], 'OptionsGameMenu');
    AddButton(nil, _lc[I_MENU_CONTROLS_OPTIONS], 'OptionsControlsMenu');
    AddButton(nil, _lc[I_MENU_PLAYER_OPTIONS], 'OptionsPlayersMenu');
    AddButton(nil, _lc[I_MENU_LANGUAGE_OPTIONS], 'OptionsLanguageMenu');
    AddSpace();
    AddButton(nil, _lc[I_MENU_SAVED_OPTIONS], 'SavedOptionsMenu').Color := _RGB(255, 0, 0);
    AddButton(nil, _lc[I_MENU_DEFAULT_OPTIONS], 'DefaultOptionsMenu').Color := _RGB(255, 0, 0);
  end;
  Menu.OnClose := ProcApplyOptions;
  Menu.DefControl := 'mmOptionsMenu';
  g_GUI_AddWindow(Menu);

  Menu := CreateYNMenu('SavedOptionsMenu', _lc[I_MENU_LOAD_SAVED_PROMT], Round(gScreenWidth*0.6),
                       gMenuSmallFont, @ProcSavedMenuKeyDown);
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('OptionsVideoMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_VIDEO_OPTIONS]))) do
  begin
    Name := 'mOptionsVideoMenu';
    AddButton(@ProcVideoOptionsRes, _lc[I_MENU_VIDEO_RESOLUTION], 'OptionsVideoResMenu');
    with AddSwitch(_lc[I_MENU_VIDEO_BPP]) do
    begin
      Name := 'swBPP';
      AddItem('16');
      AddItem('32');
    end;
    with AddSwitch(_lc[I_MENU_VIDEO_VSYNC]) do
    begin
      Name := 'swVSync';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
    end;
    with AddSwitch(_lc[I_MENU_VIDEO_FILTER_SKY]) do
    begin
      Name := 'swTextureFilter';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
    end;
    AddSpace();
    AddText(_lc[I_MENU_VIDEO_NEED_RESTART], Round(gScreenWidth*0.6));
    ReAlign();
  end;
  Menu.DefControl := 'mOptionsVideoMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('OptionsVideoResMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_RESOLUTION_SELECT]))) do
  begin
    Name := 'mOptionsVideoResMenu';
    with AddLabel(_lc[I_MENU_RESOLUTION_CURRENT]) do
    begin
      Name := 'lbCurrentRes';
      FixedLength := 24;
      Enabled := False;
    end;
    with AddList(_lc[I_MENU_RESOLUTION_LIST], 12, 6) do
    begin
      Name := 'lsResolution';
      Sort := False;
    end;
    with AddSwitch(_lc[I_MENU_RESOLUTION_FULLSCREEN]) do
    begin
      Name := 'swFullScreen';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
    end;
    AddSpace();
    AddButton(@ProcApplyVideoOptions, _lc[I_MENU_RESOLUTION_APPLY]);
    UpdateIndex();
  end;
  Menu.DefControl := 'mOptionsVideoResMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('OptionsSoundMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_SOUND_OPTIONS]))) do
  begin
    Name := 'mOptionsSoundMenu';
    with AddScroll(_lc[I_MENU_SOUND_MUSIC_LEVEL]) do
    begin
      Name := 'scMusicLevel';
      Max := 16;
      OnChange := ProcChangeSoundSettings;
    end;
    with AddScroll(_lc[I_MENU_SOUND_SOUND_LEVEL]) do
    begin
      Name := 'scSoundLevel';
      Max := 16;
      OnChange := ProcChangeSoundSettings;
    end;
    with AddScroll(_lc[I_MENU_SOUND_MAX_SIM_SOUNDS]) do
    begin
      Name := 'scMaxSimSounds';
      Max := 16;
    end;
    with AddSwitch(_lc[I_MENU_SOUND_INACTIVE_SOUNDS]) do
    begin
      Name := 'swInactiveSounds';
      AddItem(_lc[I_MENU_SOUND_INACTIVE_SOUNDS_ON]);
      AddItem(_lc[I_MENU_SOUND_INACTIVE_SOUNDS_OFF]);
    end;
    ReAlign();
  end;
  Menu.DefControl := 'mOptionsSoundMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('OptionsGameMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_GAME_OPTIONS]))) do
  begin
    Name := 'mOptionsGameMenu';
    with AddScroll(_lc[I_MENU_GAME_PARTICLES_COUNT]) do
    begin
      Name := 'scParticlesCount';
      Max := 20;
    end;
    with AddSwitch(_lc[I_MENU_GAME_BLOOD_COUNT]) do
    begin
      Name := 'swBloodCount';
      AddItem(_lc[I_MENU_COUNT_NONE]);
      AddItem(_lc[I_MENU_COUNT_SMALL]);
      AddItem(_lc[I_MENU_COUNT_NORMAL]);
      AddItem(_lc[I_MENU_COUNT_BIG]);
      AddItem(_lc[I_MENU_COUNT_VERYBIG]);
    end;
    with AddScroll(_lc[I_MENU_GAME_GIBS_COUNT]) do
    begin
      Name := 'scGibsMax';
      Max := 20;
    end;
    with AddScroll(_lc[I_MENU_GAME_MAX_CORPSES]) do
    begin
      Name := 'scCorpsesMax';
      Max := 20;
    end;
    with AddSwitch(_lc[I_MENU_GAME_MAX_GIBS]) do
    begin
      Name := 'swGibsCount';
      AddItem(_lc[I_MENU_COUNT_NONE]);
      AddItem(_lc[I_MENU_COUNT_SMALL]);
      AddItem(_lc[I_MENU_COUNT_NORMAL]);
      AddItem(_lc[I_MENU_COUNT_BIG]);
      AddItem(_lc[I_MENU_COUNT_VERYBIG]);
    end;
    with AddSwitch(_lc[I_MENU_GAME_CORPSE_TYPE]) do
    begin
      Name := 'swCorpseType';
      AddItem(_lc[I_MENU_GAME_CORPSE_TYPE_SIMPLE]);
      AddItem(_lc[I_MENU_GAME_CORPSE_TYPE_ADV]);
    end;
    with AddSwitch(_lc[I_MENU_GAME_GIBS_TYPE]) do
    begin
      Name := 'swGibsType';
      AddItem(_lc[I_MENU_GAME_GIBS_TYPE_SIMPLE]);
      AddItem(_lc[I_MENU_GAME_GIBS_TYPE_ADV]);
    end;
    with AddSwitch(_lc[I_MENU_GAME_BLOOD_TYPE]) do
    begin
      Name := 'swBloodType';
      AddItem(_lc[I_MENU_GAME_BLOOD_TYPE_SIMPLE]);
      AddItem(_lc[I_MENU_GAME_BLOOD_TYPE_ADV]);
    end;
    with AddSwitch(_lc[I_MENU_GAME_SCREEN_FLASH]) do
    begin
      Name := 'swScreenFlash';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
    end;
    with AddSwitch(_lc[I_MENU_GAME_BACKGROUND]) do
    begin
      Name := 'swBackground';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
    end;
    with AddSwitch(_lc[I_MENU_GAME_MESSAGES]) do
    begin
      Name := 'swMessages';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
    end;
    with AddSwitch(_lc[I_MENU_GAME_REVERT_PLAYERS]) do
    begin
      Name := 'swRevertPlayers';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
    end;
    ReAlign();
  end;
  Menu.DefControl := 'mOptionsGameMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('OptionsControlsMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_CONTROLS_OPTIONS]))) do
  begin
    Name := 'mOptionsControlsMenu';
    AddLine(_lc[I_MENU_CONTROL_GLOBAL]);
    AddKeyRead(_lc[I_MENU_CONTROL_SCREENSHOT]).Name := _lc[I_MENU_CONTROL_SCREENSHOT];
    AddKeyRead(_lc[I_MENU_CONTROL_STAT]).Name := _lc[I_MENU_CONTROL_STAT];
    AddKeyRead(_lc[I_MENU_CONTROL_CHAT]).Name := _lc[I_MENU_CONTROL_CHAT];
    AddSpace();
    AddButton(nil, _lc[I_MENU_PLAYER_1], 'OptionsControlsP1Menu');
    AddButton(nil, _lc[I_MENU_PLAYER_2], 'OptionsControlsP2Menu');
  end;
  Menu.DefControl := 'mOptionsControlsMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('OptionsControlsP1Menu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_PLAYER_1]))) do
  begin
    Name := 'mOptionsControlsP1Menu';
    AddKeyRead(_lc[I_MENU_CONTROL_LEFT]).Name := _lc[I_MENU_CONTROL_LEFT];
    AddKeyRead(_lc[I_MENU_CONTROL_RIGHT]).Name := _lc[I_MENU_CONTROL_RIGHT];
    AddKeyRead(_lc[I_MENU_CONTROL_UP]).Name := _lc[I_MENU_CONTROL_UP];
    AddKeyRead(_lc[I_MENU_CONTROL_DOWN]).Name := _lc[I_MENU_CONTROL_DOWN];
    AddKeyRead(_lc[I_MENU_CONTROL_JUMP]).Name := _lc[I_MENU_CONTROL_JUMP];
    AddKeyRead(_lc[I_MENU_CONTROL_FIRE]).Name := _lc[I_MENU_CONTROL_FIRE];
    AddKeyRead(_lc[I_MENU_CONTROL_USE]).Name := _lc[I_MENU_CONTROL_USE];
    AddKeyRead(_lc[I_MENU_CONTROL_NEXT_WEAPON]).Name := _lc[I_MENU_CONTROL_NEXT_WEAPON];
    AddKeyRead(_lc[I_MENU_CONTROL_PREV_WEAPON]).Name := _lc[I_MENU_CONTROL_PREV_WEAPON];
  end;
  Menu.DefControl := 'mOptionsControlsP1Menu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('OptionsControlsP2Menu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_PLAYER_2]))) do
  begin
    Name := 'mOptionsControlsP2Menu';
    AddKeyRead(_lc[I_MENU_CONTROL_LEFT]).Name := _lc[I_MENU_CONTROL_LEFT];
    AddKeyRead(_lc[I_MENU_CONTROL_RIGHT]).Name := _lc[I_MENU_CONTROL_RIGHT];
    AddKeyRead(_lc[I_MENU_CONTROL_UP]).Name := _lc[I_MENU_CONTROL_UP];
    AddKeyRead(_lc[I_MENU_CONTROL_DOWN]).Name := _lc[I_MENU_CONTROL_DOWN];
    AddKeyRead(_lc[I_MENU_CONTROL_JUMP]).Name := _lc[I_MENU_CONTROL_JUMP];
    AddKeyRead(_lc[I_MENU_CONTROL_FIRE]).Name := _lc[I_MENU_CONTROL_FIRE];
    AddKeyRead(_lc[I_MENU_CONTROL_USE]).Name := _lc[I_MENU_CONTROL_USE];
    AddKeyRead(_lc[I_MENU_CONTROL_NEXT_WEAPON]).Name := _lc[I_MENU_CONTROL_NEXT_WEAPON];
    AddKeyRead(_lc[I_MENU_CONTROL_PREV_WEAPON]).Name := _lc[I_MENU_CONTROL_PREV_WEAPON];
  end;
  Menu.DefControl := 'mOptionsControlsP2Menu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('OptionsPlayersMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_PLAYER_OPTIONS]))) do
  begin
    Name := 'mOptionsPlayersMenu';
    AddButton(nil, _lc[I_MENU_PLAYER_1], 'OptionsPlayersP1Menu');
    AddButton(nil, _lc[I_MENU_PLAYER_2], 'OptionsPlayersP2Menu');
  end;
  Menu.DefControl := 'mOptionsPlayersMenu';
  g_GUI_AddWindow(Menu);

  CreatePlayerOptionsMenu('P1');
  CreatePlayerOptionsMenu('P2'); 

  Menu := TGUIWindow.Create('OptionsPlayersMIMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_MODEL_INFO]))) do
  begin
    Name := 'mOptionsPlayersMIMenu';
    with AddLabel(_lc[I_MENU_MODEL_NAME]) do
    begin
      Name := 'lbName';
      FixedLength := 16;
    end;
    with AddLabel(_lc[I_MENU_MODEL_AUTHOR]) do
    begin
      Name := 'lbAuthor';
      FixedLength := 16;
    end;
    with AddMemo(_lc[I_MENU_MODEL_COMMENT], 14, 6) do
    begin
      Name := 'meComment';
    end;
    AddSpace();
    AddLine(_lc[I_MENU_MODEL_OPTIONS]);
    with AddLabel(_lc[I_MENU_MODEL_WEAPON]) do
    begin
      Name := 'lbWeapon';
      FixedLength := Max(Length(_lc[I_MENU_YES]), Length(_lc[I_MENU_NO]));
    end;
    ReAlign();
  end;
  Menu.DefControl := 'mOptionsPlayersMIMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('OptionsLanguageMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_LANGUAGE_OPTIONS]))) do
  begin
    Name := 'mOptionsLanguageMenu';
    AddButton(@ProcSetRussianLanguage, _lc[I_MENU_LANGUAGE_RUSSIAN]);
    AddButton(@ProcSetEnglishLanguage, _lc[I_MENU_LANGUAGE_ENGLISH]);
    ReAlign();
  end;
  Menu.DefControl := 'mOptionsLanguageMenu';
  g_GUI_AddWindow(Menu);

  Menu := CreateYNMenu('DefaultOptionsMenu', _lc[I_MENU_SET_DEFAULT_PROMT], Round(gScreenWidth*0.6),
                       gMenuSmallFont, @ProcDefaultMenuKeyDown);
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('AuthorsMenu');
  Menu.BackTexture := 'INTER';
  Menu.OnClose := ProcAuthorsClose;

// Заголовок:
  _y := 16;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(_lc[I_CREDITS_CAP_1], gMenuFont))) do
  begin
    Color := _RGB(255, 0, 0);
    X := (gScreenWidth div 2)-(GetWidth() div 2);
    Y := _y;
    _y := _y+GetHeight();
  end;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(Format(_lc[I_CREDITS_CAP_2], [GAME_VERSION, NET_PROTOCOL_VER]), gMenuSmallFont))) do
  begin
    Color := _RGB(255, 0, 0);
    X := (gScreenWidth div 2)-(GetWidth() div 2);
    Y := _y;
    _y := _y+GetHeight()+32;
  end;
// Что делал: Кто делал
  cx := gScreenWidth div 2 - 320 + 64;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(_lc[I_CREDITS_A_1], gMenuSmallFont))) do
  begin
    Color := _RGB(255, 0, 0);
    X := cx;
    Y := _y;
    _y := _y+22;
  end;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(_lc[I_CREDITS_A_1_1], gMenuSmallFont))) do
  begin
    Color := _RGB(255, 255, 255);
    X := cx+32;
    Y := _y;
    _y := _y+36;
  end;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(_lc[I_CREDITS_A_2], gMenuSmallFont))) do
  begin
    Color := _RGB(255, 0, 0);
    X := cx;
    Y := _y;
    _y := _y+22;
  end;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(_lc[I_CREDITS_A_2_1], gMenuSmallFont))) do
  begin
    Color := _RGB(255, 255, 255);
    X := cx+32;
    Y := _y;
    _y := _y+36;
  end;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(_lc[I_CREDITS_A_3], gMenuSmallFont))) do
  begin
    Color := _RGB(255, 0, 0);
    X := cx;
    Y := _y;
    _y := _y+22;
  end;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(_lc[I_CREDITS_A_3_1], gMenuSmallFont))) do
  begin
    Color := _RGB(255, 255, 255);
    X :=  cx+32;
    Y := _y;
   _y := _y+36;
  end;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(_lc[I_CREDITS_A_4], gMenuSmallFont))) do
  begin
    Color := _RGB(255, 0, 0);
    X := cx;
    Y := _y;
    _y := _y+22;
  end;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(_lc[I_CREDITS_A_4_1], gMenuSmallFont))) do
  begin
    Color := _RGB(255, 255, 255);
    X := cx+32;
    Y := _y;
    _y := gScreenHeight - 128;
  end;
// Заключение:
  with TGUILabel(Menu.AddChild(TGUILabel.Create(_lc[I_CREDITS_CAP_3], gMenuSmallFont))) do
  begin
    Color := _RGB(255, 0, 0);
    X := cx;
    Y := _y;
    _y := _y+16;
  end;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(_lc[I_CREDITS_CLO_1], gMenuSmallFont))) do
  begin
    Color := _RGB(255, 255, 255);
    X := cx+32;
    Y := _y;
    _y := _y+GetHeight();
  end;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(_lc[I_CREDITS_CLO_2], gMenuSmallFont))) do
  begin
    Color := _RGB(255, 255, 255);
    X := cx+32;
    Y := _y;
    _y := _y+GetHeight();
  end;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(_lc[I_CREDITS_CLO_3], gMenuSmallFont))) do
  begin
    Color := _RGB(255, 255, 255);
    X := cx+32;
    Y := _y;
    _y := gScreenHeight - 32;
  end;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(_lc[I_CREDITS_CLO_4], gMenuSmallFont))) do
  begin
    Color := _RGB(255, 0, 0);
    X := gScreenWidth div 2 - GetWidth() div 2;
    Y := _y;
  end;
  g_GUI_AddWindow(Menu);

  Menu := CreateYNMenu('ExitMenu', _lc[I_MENU_EXIT_PROMT], Round(gScreenWidth*0.6),
                       gMenuSmallFont, @ProcExitMenuKeyDown);
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('GameSingleMenu');
  with TGUIMainMenu(Menu.AddChild(TGUIMainMenu.Create(gMenuFont, _lc[I_MENU_MAIN_MENU]))) do
  begin
    Name := 'mmGameSingleMenu';
    AddButton(nil, _lc[I_MENU_LOAD_GAME], 'LoadMenu');
    AddButton(nil, _lc[I_MENU_SAVE_GAME], 'SaveMenu').Name := 'save';
    AddButton(@ReadGameSettings, _lc[I_MENU_SET_GAME], 'GameSetGameMenu');
    AddButton(@ReadOptions, _lc[I_MENU_OPTIONS], 'OptionsMenu');
    AddButton(nil, _lc[I_MENU_RESTART], 'RestartGameMenu');
    AddButton(nil, _lc[I_MENU_END_GAME], 'EndGameMenu');
  end;
  Menu.DefControl := 'mmGameSingleMenu';
  Menu.MainWindow := True;
  Menu.OnClose := ProcGMClose;
  Menu.OnShow := ProcGMShow;
  g_GUI_AddWindow(Menu);

  Menu := CreateYNMenu('EndGameMenu', _lc[I_MENU_END_GAME_PROMT], Round(gScreenWidth*0.6),
                       gMenuSmallFont, @ProcEndMenuKeyDown);
  g_GUI_AddWindow(Menu);

  Menu := CreateYNMenu('RestartGameMenu', _lc[I_MENU_RESTART_GAME_PROMT], Round(gScreenWidth*0.6),
                       gMenuSmallFont, @ProcRestartMenuKeyDown);
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('GameCustomMenu');
  with TGUIMainMenu(Menu.AddChild(TGUIMainMenu.Create(gMenuFont, _lc[I_MENU_MAIN_MENU]))) do
  begin
    Name := 'mmGameCustomMenu';
    AddButton(nil, _lc[I_MENU_LOAD_GAME], 'LoadMenu');
    AddButton(nil, _lc[I_MENU_SAVE_GAME], 'SaveMenu').Name := 'save';
    AddButton(@ReadGameSettings, _lc[I_MENU_SET_GAME], 'GameSetGameMenu');
    AddButton(@ReadOptions, _lc[I_MENU_OPTIONS], 'OptionsMenu');
    AddButton(nil, _lc[I_MENU_RESTART], 'RestartGameMenu');
    AddButton(nil, _lc[I_MENU_END_GAME], 'EndGameMenu');
  end;
  Menu.DefControl := 'mmGameCustomMenu';
  Menu.MainWindow := True;
  Menu.OnClose := ProcGMClose;
  Menu.OnShow := ProcGMShow;
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('GameServerMenu');
  with TGUIMainMenu(Menu.AddChild(TGUIMainMenu.Create(gMenuFont, _lc[I_MENU_MAIN_MENU]))) do
  begin
    Name := 'mmGameServerMenu';
    AddButton(@ReadGameSettings, _lc[I_MENU_SET_GAME], 'GameSetGameMenu');
    AddButton(@ReadOptions, _lc[I_MENU_OPTIONS], 'OptionsMenu');
    AddButton(nil, _lc[I_MENU_RESTART], 'RestartGameMenu');
    AddButton(nil, _lc[I_MENU_END_GAME], 'EndGameMenu');
  end;
  Menu.DefControl := 'mmGameServerMenu';
  Menu.MainWindow := True;
  Menu.OnClose := ProcGMClose;
  Menu.OnShow := ProcGMShow;
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('GameClientMenu');
  with TGUIMainMenu(Menu.AddChild(TGUIMainMenu.Create(gMenuFont, _lc[I_MENU_MAIN_MENU]))) do
  begin
    Name := 'mmGameClientMenu';
    AddButton(@ReadOptions, _lc[I_MENU_OPTIONS], 'OptionsMenu');
    AddButton(nil, _lc[I_MENU_END_GAME], 'EndGameMenu');
  end;
  Menu.DefControl := 'mmGameClientMenu';
  Menu.MainWindow := True;
  Menu.OnClose := ProcGMClose;
  Menu.OnShow := ProcGMShow;
  g_GUI_AddWindow(Menu);
 
  Menu := TGUIWindow.Create('ClientPasswordMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuSmallFont, gMenuSmallFont, _lc[I_MENU_ENTERPASSWORD]))) do
  begin
    Name := 'mClientPasswordMenu';
    with AddEdit(_lc[I_NET_SERVER_PASSWORD]) do
    begin
      Name := 'edPW';
      Width := 12;
      MaxLength := 32;
    end;
    AddSpace;

    AddButton(@ProcEnterPassword, _lc[I_MENU_START_GAME]);
    ReAlign();
  end;
  Menu.DefControl := 'mClientPasswordMenu';
  g_GUI_AddWindow(Menu);

  Menu := TGUIWindow.Create('GameSetGameMenu');
  with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, _lc[I_MENU_SET_GAME]))) do
  begin
    Name := 'mGameSetGameMenu';
    with AddSwitch(_lc[I_MENU_TEAM_DAMAGE]) do
    begin
      Name := 'swTeamDamage';
      AddItem(_lc[I_MENU_YES]);
      AddItem(_lc[I_MENU_NO]);
      ItemIndex := 1;
    end;
    with AddEdit(_lc[I_MENU_TIME_LIMIT]) do
    begin
      Name := 'edTimeLimit';
      OnlyDigits := True;
      Width := 3;
      MaxLength := 3;
    end;
    with AddEdit(_lc[I_MENU_GOAL_LIMIT]) do
    begin
      Name := 'edGoalLimit';
      OnlyDigits := True;
      Width := 3;
      MaxLength := 3;
    end;
    with AddEdit(_lc[I_MENU_MAX_LIVES]) do
    begin
      Name := 'edMaxLives';
      OnlyDigits := True;
      Width := 3;
      MaxLength := 3;
    end;
    with AddSwitch(_lc[I_MENU_BOTS_VS]) do
    begin
      Name := 'swBotsVS';
      AddItem(_lc[I_MENU_BOTS_VS_PLAYERS]);
      AddItem(_lc[I_MENU_BOTS_VS_MONSTERS]);
      AddItem(_lc[I_MENU_BOTS_VS_ALL]);
      ItemIndex := 2;
    end;

    ReAlign();               
  end;
  Menu.DefControl := 'mGameSetGameMenu';
  Menu.OnClose := ProcApplyGameSet;
  g_GUI_AddWindow(Menu);
end;

procedure g_Menu_Show_SaveMenu(custom: Boolean);
begin
  if custom then
    g_GUI_ShowWindow('GameCustomMenu')
  else
    g_GUI_ShowWindow('GameSingleMenu');
  g_GUI_ShowWindow('SaveMenu');
end;

procedure g_Menu_Show_LoadMenu(custom: Boolean);
begin
  if custom then
    g_GUI_ShowWindow('GameCustomMenu')
  else
    g_GUI_ShowWindow('GameSingleMenu');
  g_GUI_ShowWindow('LoadMenu');
end;

procedure g_Menu_Show_GameSetGame(custom: Boolean);
begin
  if custom then
    g_GUI_ShowWindow('GameCustomMenu')
  else
    g_GUI_ShowWindow('GameSingleMenu');
  ReadGameSettings();
  g_GUI_ShowWindow('GameSetGameMenu');
end;

procedure g_Menu_Show_OptionsVideo(custom: Boolean);
begin
  if custom then
    g_GUI_ShowWindow('GameCustomMenu')
  else
    g_GUI_ShowWindow('GameSingleMenu');
  ReadOptions();
  g_GUI_ShowWindow('OptionsMenu');
  g_GUI_ShowWindow('OptionsVideoMenu');
end;

procedure g_Menu_Show_OptionsSound(custom: Boolean);
begin
  if custom then
    g_GUI_ShowWindow('GameCustomMenu')
  else
    g_GUI_ShowWindow('GameSingleMenu');
  ReadOptions();
  g_GUI_ShowWindow('OptionsMenu');
  g_GUI_ShowWindow('OptionsSoundMenu');
end;

procedure g_Menu_Show_EndGameMenu(custom: Boolean);
begin
  if custom then
    g_GUI_ShowWindow('GameCustomMenu')
  else
    g_GUI_ShowWindow('GameSingleMenu');
  g_GUI_ShowWindow('EndGameMenu');
end;

procedure g_Menu_Show_QuitGameMenu(custom: Boolean);
begin
  if custom then
    g_GUI_ShowWindow('GameCustomMenu')
  else
    g_GUI_ShowWindow('GameSingleMenu');
  g_GUI_ShowWindow('ExitMenu');
end;

procedure g_Menu_Init();
begin
  MenuLoadData();
  g_GUI_Init();
  CreateAllMenus();
end;

procedure g_Menu_Free();
begin
  g_GUI_Destroy();

  e_WriteLog('Releasing menu data...', MSG_NOTIFY);

  MenuFreeData();
end;

procedure g_Menu_Reset();
var
  ex: Boolean;
begin
  g_GUI_SaveMenuPos();
  ex := g_GUI_Destroy();

  if ex then
  begin
    e_WriteLog('Recreating menu...', MSG_NOTIFY);

    CreateAllMenus();

    if gDebugMode then
      g_Game_SetDebugMode();

    g_GUI_LoadMenuPos();
  end;
end;

end.
