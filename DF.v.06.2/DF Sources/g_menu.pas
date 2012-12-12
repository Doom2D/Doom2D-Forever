unit g_menu;

interface

uses windows;

procedure g_Menu_Init();
procedure g_Menu_Free();
procedure LoadFont(txtres, fntres: string; cwdt, chgt: Byte; spc: ShortInt;
                   var FontID: DWORD);

var
  gMenuFont: DWORD;
  gMenuSmallFont: DWORD;

implementation

uses
  g_gui, g_textures, e_graphics, g_main, g_window, g_game, g_basic,
  g_console, g_sound, inter, g_gfx, g_player, g_options, e_log, SysUtils,
  g_map, CONFIG, g_playermodel, DateUtils, MAPSTRUCT, WADEDITOR, Math,
  WADSTRUCT, g_saveload;

procedure ProcChangeColor(Sender: TGUIControl); forward;
procedure ProcSelectModel(Sender: TGUIControl); forward;

procedure ProcApplyOptions();
var
  menu: TGUIMenu;
  config: TConfig;
  SWidth, SHeight: Word;
  Fullscreen: Boolean;
  BPP: Byte;
begin
 menu := TGUIMenu(g_GUI_GetWindow('OptionsVideoMenu').GetControl('mOptionsVideoMenu'));

 case TGUISwitch(menu.GetControl('swResolution')).ItemIndex of
  0: begin
      SWidth := 640;
      SHeight := 480;
     end;
  1: begin
      SWidth := 800;
      SHeight := 600;
     end;
  2: begin
      SWidth := 1024;
      SHeight := 768;
     end;
 end;

 Fullscreen := TGUISwitch(menu.GetControl('swFullScreen')).ItemIndex = 0;
 gVSync := TGUISwitch(menu.GetControl('swVSync')).ItemIndex = 0;
 if TGUISwitch(menu.GetControl('swBPP')).ItemIndex = 0 then BPP := 16 else BPP := 32;
 gTextureFilter := TGUISwitch(menu.GetControl('swTextureFilter')).ItemIndex = 0;

 menu := TGUIMenu(g_GUI_GetWindow('OptionsSoundMenu').GetControl('mOptionsSoundMenu'));

 gSoundLevel := Min(TGUIScroll(menu.GetControl('scSoundLevel')).Value*16, 255);
 gMusicLevel := Min(TGUIScroll(menu.GetControl('scMusicLevel')).Value*16, 255);

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
  TakeScreenshot := TGUIKeyRead(GetControl(I_MENU_CONTROL_SCREENSHOT)).Key;
  Stat := TGUIKeyRead(GetControl(I_MENU_CONTROL_STAT)).Key;
 end;

 menu := TGUIMenu(g_GUI_GetWindow('OptionsControlsP1Menu').GetControl('mOptionsControlsP1Menu'));

 with menu, gGameControls.P1Control do
 begin
  KeyRight := TGUIKeyRead(GetControl(I_MENU_CONTROL_RIGHT)).Key;
  KeyLeft := TGUIKeyRead(GetControl(I_MENU_CONTROL_LEFT)).Key;
  KeyUp := TGUIKeyRead(GetControl(I_MENU_CONTROL_UP)).Key;
  KeyDown := TGUIKeyRead(GetControl(I_MENU_CONTROL_DOWN)).Key;
  KeyFire := TGUIKeyRead(GetControl(I_MENU_CONTROL_FIRE)).Key;
  KeyJump := TGUIKeyRead(GetControl(I_MENU_CONTROL_JUMP)).Key;
  KeyNextWeapon := TGUIKeyRead(GetControl(I_MENU_CONTROL_NEXTWEAPON)).Key;
  KeyPrevWeapon := TGUIKeyRead(GetControl(I_MENU_CONTROL_PREVWEAPON)).Key;
  KeyOpen := TGUIKeyRead(GetControl(I_MENU_CONTROL_USE)).Key;
 end;

 menu := TGUIMenu(g_GUI_GetWindow('OptionsControlsP2Menu').GetControl('mOptionsControlsP2Menu'));

 with menu, gGameControls.P2Control do
 begin
  KeyRight := TGUIKeyRead(GetControl(I_MENU_CONTROL_RIGHT)).Key;
  KeyLeft := TGUIKeyRead(GetControl(I_MENU_CONTROL_LEFT)).Key;
  KeyUp := TGUIKeyRead(GetControl(I_MENU_CONTROL_UP)).Key;
  KeyDown := TGUIKeyRead(GetControl(I_MENU_CONTROL_DOWN)).Key;
  KeyFire := TGUIKeyRead(GetControl(I_MENU_CONTROL_FIRE)).Key;
  KeyJump := TGUIKeyRead(GetControl(I_MENU_CONTROL_JUMP)).Key;
  KeyNextWeapon := TGUIKeyRead(GetControl(I_MENU_CONTROL_NEXTWEAPON)).Key;
  KeyPrevWeapon := TGUIKeyRead(GetControl(I_MENU_CONTROL_PREVWEAPON)).Key;
  KeyOpen := TGUIKeyRead(GetControl(I_MENU_CONTROL_USE)).Key;
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

 if gGameOn and (gGameSettings.GameType = GT_CUSTOM) then
 begin
  if gPlayer1 <> nil then
  begin
   gPlayer1.SetModel(gPlayer1Settings.Model);
   if (gGameSettings.GameMode <> GM_TDM) and (gGameSettings.GameMode <> GM_CTF) then
    gPlayer1.SetColor(gPlayer1Settings.Color);
  end;

  if gPlayer2 <> nil then
  begin
   gPlayer2.SetModel(gPlayer2Settings.Model);
   if (gGameSettings.GameMode <> GM_TDM) and (gGameSettings.GameMode <> GM_CTF) then
    gPlayer2.SetColor(gPlayer2Settings.Color);
  end;
 end;

 e_WriteLog('Writing config', MSG_NOTIFY);
 
 config := TConfig.CreateFile(GameDir+'\doomforever.cfg');

 config.WriteInt('Video', 'ScreenWidth', SWidth);
 config.WriteInt('Video', 'ScreenHeight', SHeight);
 config.WriteBool('Video', 'Fullscreen', FullScreen);
 config.WriteInt('Video', 'BPP', BPP);
 config.WriteBool('Video', 'VSync', gVSync);
 config.WriteBool('Video', 'TextureFilter', gTextureFilter);

 config.WriteInt('Sound', 'SoundLevel', gSoundLevel);
 config.WriteInt('Sound', 'MusicLevel', gMusicLevel);

 with config, gGameControls.GameControls do
 begin
  WriteInt('GameControls', 'TakeScreenshot', TakeScreenshot);
  WriteInt('GameControls', 'Stat', Stat);
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

 config.SaveFile(GameDir+'\doomforever.cfg');
 config.Destroy;
end;

procedure ReadOptions();
var
  menu: TGUIMenu;
begin
 menu := TGUIMenu(g_GUI_GetWindow('OptionsVideoMenu').GetControl('mOptionsVideoMenu'));

 with TGUISwitch(menu.GetControl('swResolution')) do
 case gScreenWidth of
  640: ItemIndex := 0;
  800: ItemIndex := 1;
  else
    ItemIndex := 2;
 end;

 with TGUISwitch(menu.GetControl('swFullScreen')) do
  if gFullscreen then ItemIndex := 0 else ItemIndex := 1;

 with TGUISwitch(menu.GetControl('swBPP')) do
  if gBPP = 16 then ItemIndex := 0 else ItemIndex := 1;

 with TGUISwitch(menu.GetControl('swTextureFilter')) do
  if gTextureFilter then ItemIndex := 0 else ItemIndex := 1;

 with TGUISwitch(menu.GetControl('swVSync')) do
  if gVSync then ItemIndex := 0 else ItemIndex := 1;

 menu := TGUIMenu(g_GUI_GetWindow('OptionsSoundMenu').GetControl('mOptionsSoundMenu'));

 TGUIScroll(menu.GetControl('scSoundLevel')).Value := Round(gSoundLevel/16);
 TGUIScroll(menu.GetControl('scMusicLevel')).Value := Round(gMusicLevel/16);

 menu := TGUIMenu(g_GUI_GetWindow('OptionsControlsP1Menu').GetControl('mOptionsControlsP1Menu'));

 with menu, gGameControls.P1Control do
 begin
  TGUIKeyRead(GetControl(I_MENU_CONTROL_RIGHT)).Key := KeyRight;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_LEFT)).Key := KeyLeft;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_UP)).Key := KeyUp;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_DOWN)).Key := KeyDown;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_FIRE)).Key := KeyFire;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_JUMP)).Key := KeyJump;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_NEXTWEAPON)).Key := KeyNextWeapon;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_PREVWEAPON)).Key := KeyPrevWeapon;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_USE)).Key := KeyOpen;
 end;

 menu := TGUIMenu(g_GUI_GetWindow('OptionsControlsP2Menu').GetControl('mOptionsControlsP2Menu'));

 with menu, gGameControls.P2Control do
 begin
  TGUIKeyRead(GetControl(I_MENU_CONTROL_RIGHT)).Key := KeyRight;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_LEFT)).Key := KeyLeft;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_UP)).Key := KeyUp;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_DOWN)).Key := KeyDown;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_FIRE)).Key := KeyFire;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_JUMP)).Key := KeyJump;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_NEXTWEAPON)).Key := KeyNextWeapon;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_PREVWEAPON)).Key := KeyPrevWeapon;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_USE)).Key := KeyOpen;
 end;

 menu := TGUIMenu(g_GUI_GetWindow('OptionsControlsMenu').GetControl('mOptionsControlsMenu'));
 with menu, gGameControls.GameControls do
 begin
  TGUIKeyRead(GetControl(I_MENU_CONTROL_SCREENSHOT)).Key := TakeScreenshot;
  TGUIKeyRead(GetControl(I_MENU_CONTROL_STAT)).Key := Stat;
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
 Map: string;
 GameMode: Byte;
 TimeLimit, GoalLimit: Word;
 Options: LongWord;
begin
 with TGUIMenu(g_ActiveWindow.GetControl('mCustomGameMenu')) do
 begin
  if TGUILabel(GetControl('lbMap')).Text = '' then Exit;

  GameMode := TGUISwitch(GetControl('swGameMode')).ItemIndex+1;
  TimeLimit := StrToIntDef(TGUIEdit(GetControl('edTimeLimit')).Text, 0)*60;
  GoalLimit := StrToIntDef(TGUIEdit(GetControl('edGoalLimit')).Text, 0);

  Options := 0;
  if TGUISwitch(GetControl('swTeamDamage')).ItemIndex = 0 then Options := Options or GAME_OPTION_TEAMDAMAGE;
  if TGUISwitch(GetControl('swEnableExits')).ItemIndex = 0 then Options := Options or GAME_OPTION_ALLOWEXIT;
  if TGUISwitch(GetControl('swWeaponStay')).ItemIndex = 0 then Options := Options or GAME_OPTION_WEAPONSTAY;
  if TGUISwitch(GetControl('swMonsterDM')).ItemIndex = 0 then Options := Options or GAME_OPTION_MONSTERDM;
  if TGUISwitch(GetControl('swPlayers')).ItemIndex = 1 then Options := Options or GAME_OPTION_TWOPLAYER;

  Map := MapsDir+TGUILabel(GetControl('lbMap')).Text;
 end;

 g_Game_StartCustom(Map, GameMode, TimeLimit, GoalLimit, Options);
end;

procedure ProcStartEpisode();
var
  WAD: string;
  TwoPlayers: Boolean;
begin
 with TGUIMenu(g_ActiveWindow.GetControl('mEpisodeMenu')) do
 begin
  WAD := TGUIFileListBox(GetControl('lsWAD')).SelectedItem();
  TwoPlayers := TGUISwitch(GetControl('swPlayers')).ItemIndex = 1;
 end;

 g_Game_StartSingle(WAD, 'MAP01', TwoPlayers);
end;

procedure ProcSelectMap(Sender: TGUIControl);
var
  a: TMapInfo;
  wad, map, res: string;
begin
 with TGUIMenu(g_ActiveWindow.GetControl('mSelectMapMenu')) do
 begin
  wad := TGUIFileListBox(GetControl('lsMapWAD')).SelectedItem();
  map := TGUIListBox(GetControl('lsMapRes')).SelectedItem();

  if (wad = '') or (map = '') then
  begin
   TGUILabel(GetControl('lbMapName')).Text := '';
   TGUILabel(GetControl('lbMapAuthor')).Text := '';
   TGUILabel(GetControl('lbMapSize')).Text := '';
   TGUIMemo(GetControl('meMapDescription')).Clear();
   TGUIMemo(GetControl('meMapDescription')).SetText('');
   TGUIMapPreview(g_ActiveWindow.GetControl('mpMapPreview')).ClearMap();
  end;

  res := wad+':\'+map;

  a := g_Map_GetMapInfo(res);

  TGUILabel(GetControl('lbMapName')).Text := a.Name;
  TGUILabel(GetControl('lbMapAuthor')).Text := a.Author;
  TGUILabel(GetControl('lbMapSize')).Text := Format('%dx%d', [a.Width, a.Height]);
  TGUIMemo(GetControl('meMapDescription')).Clear();
  TGUIMemo(GetControl('meMapDescription')).SetText(a.Description);

  TGUIMapPreview(g_ActiveWindow.GetControl('mpMapPreview')).SetMap(res);
 end;
end;

procedure ProcSelectWAD(Sender: TGUIControl);
var
  wad: string;
  list: SArray;
begin
 with TGUIMenu(g_ActiveWindow.GetControl('mSelectMapMenu')) do
 begin
  wad := TGUIFileListBox(GetControl('lsMapWAD')).SelectedItem();
  if wad = '' then Exit;

  with TGUIListBox(GetControl('lsMapRes')) do
  begin
   list := g_Map_GetMapsList(wad);

   if list <> nil then
   begin
    Items := list;
    ItemIndex := 0;
   end
    else Clear();
  end;
 end;
 
 ProcSelectMap(nil);
end;

procedure ProcSelectEpisodeWAD(Sender: TGUIControl);
var
  a: TMegaWADInfo;
  wad, fn: string;
begin
 with TGUIMenu(g_ActiveWindow.GetControl('mEpisodeMenu')) do
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

  TGUIImage(g_ActiveWindow.GetControl('mpWADImage')).ClearImage();

  if a.pic <> '' then
  begin
   g_ProcessResourceStr(a.pic, @fn, nil, nil);
   if fn = '' then TGUIImage(g_ActiveWindow.GetControl('mpWADImage')).SetImage(wad+a.pic)
    else TGUIImage(g_ActiveWindow.GetControl('mpWADImage')).SetImage(a.pic);
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
 wad.Destroy;

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

  config.Destroy;
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
 g_Sound_CreateWADEx('SOUND_PLAYER_FALL', GameWAD+':SOUNDS\FALL');
end;

procedure MenuFreeData();
begin
 e_CharFont_Remove(gMenuFont);

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
 g_Sound_Delete('SOUND_PLAYER_FALL');
end;

procedure ProcAuthorsMenu();
begin
 g_Game_StopMusic();
 g_Game_PlayMusic('INTERMUS');
end;

procedure ProcExitMenuKeyDown(Key: Byte);
var
  sn: ShortString;

begin
  if Key = Ord('Y') then
  begin
    g_Game_StopMusic;
    case (Random(18)) of
      0: sn := 'SOUND_MONSTER_PAIN';
      1: sn := 'SOUND_MONSTER_DIE_3';
      2: sn := 'SOUND_MONSTER_SLOP';
      3: sn := 'SOUND_MONSTER_DEMON_DIE';
      4: sn := 'SOUND_MONSTER_IMP_DIE_2';
      5: sn := 'SOUND_MONSTER_MAN_DIE';
      6: sn := 'SOUND_MONSTER_BSP_DIE';
      7: sn := 'SOUND_MONSTER_VILE_DIE';
      8: sn := 'SOUND_MONSTER_SKEL_DIE';
      9: sn := 'SOUND_MONSTER_MANCUB_ALERT';
      10: sn := 'SOUND_MONSTER_PAIN_PAIN';
      11: sn := 'SOUND_MONSTER_BARON_DIE';
      12: sn := 'SOUND_MONSTER_CACO_DIE';
      13: sn := 'SOUND_MONSTER_CYBER_DIE';
      14: sn := 'SOUND_MONSTER_KNIGHT_ALERT';
      15: sn := 'SOUND_MONSTER_SPIDER_ALERT';
     else sn := 'SOUND_PLAYER_FALL';
    end;
    if not g_Sound_PlayEx(sn, 127, 255) then
      g_Sound_PlayEx('SOUND_PLAYER_FALL', 127, 255);
    Sleep(1500);
    g_Game_Quit;
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
 a := StrToInt(Copy(Sender.Name, Length(Sender.Name), 1));
 g_SaveGame(a, TGUIEdit(Sender).Text);

 g_ActiveWindow := nil;
 gPause := False;
end;

procedure ProcLoadGame(Sender: TGUIControl);
var
  a: Integer;
begin
 a := StrToInt(Copy(Sender.Name, Length(Sender.Name), 1));
 if not g_LoadGame(a) then g_GUI_GetWindow('LoadMenu').SetActive(g_GUI_GetWindow('LoadMenu').GetControl('mmLoadMenu'));
end;

procedure ProcSingle1Player();
begin
 g_Game_StartSingle(MapsDir+'megawads\DOOM2D.WAD', 'MAP01', False);
end;

procedure ProcSingle2Players();
begin
 g_Game_StartSingle(MapsDir+'megawads\DOOM2D.WAD', 'MAP01', True);
end;

procedure ProcSelectMapMenu();
begin
 g_GUI_ShowWindow('SelectMapMenu');
end;

procedure ProcSetMap();
var
  wad, map, res: string;
begin
 with TGUIMenu(g_ActiveWindow.GetControl('mSelectMapMenu')) do
 begin
  wad := ExtractRelativePath(MapsDir, TGUIFileListBox(GetControl('lsMapWAD')).SelectedItem());
  map := TGUIListBox(GetControl('lsMapRes')).SelectedItem();
 end;

 if (wad = '') or (map = '') then Exit;

 res := wad+':\'+map;

 TGUILabel(TGUIMenu(g_GUI_GetWindow('CustomGameMenu').GetControl('mCustomGameMenu')).GetControl('lbMap')).Text := res;
end;

procedure ProcChangeSoundSettings(Sender: TGUIControl);
var
  menu: TGUIMenu;
begin
 menu := TGUIMenu(g_GUI_GetWindow('OptionsSoundMenu').GetControl('mOptionsSoundMenu'));

 gSoundLevel := Min(TGUIScroll(menu.GetControl('scSoundLevel')).Value*16, 255);
 gMusicLevel := Min(TGUIScroll(menu.GetControl('scMusicLevel')).Value*16, 255);

 g_Game_SetMusicVolume();
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

  if b.HaveWeapon then TGUILabel(GetControl('lbWeapon')).Text := I_MENU_YES
   else TGUILabel(GetControl('lbWeapon')).Text := I_MENU_NO;
 end;
 
 g_GUI_ShowWindow('OptionsPlayersMIMenu');
end;

procedure ProcOptionsPlayersAnim();
var
  s: string;
begin
 if g_ActiveWindow.Name = 'OptionsPlayersP1Menu' then s := 'P1' else s := 'P2';
 with TGUIModelView(g_ActiveWindow.GetControl('mv'+s+'Model')) do
 begin
  NextAnim();
  Model.GetCurrentAnimation.Loop := True;
  Model.GetCurrentAnimationMask.Loop := True;
 end;
end;

procedure ProcOptionsPlayersWeap();
var
  s: string;
begin
 if g_ActiveWindow.Name = 'OptionsPlayersP1Menu' then s := 'P1' else s := 'P2';
 with TGUIModelView(g_ActiveWindow.GetControl('mv'+s+'Model')) do NextWeapon();
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
 end else if Key = Ord('N') then g_GUI_HideWindow;
end;

procedure ProcSavedMenuKeyDown(Key: Byte);
begin
 if Key = Ord('Y') then
 begin
  ReadOptions();
  g_GUI_HideWindow();
 end else if Key = Ord('N') then g_GUI_HideWindow;
end;

procedure ProcAuthorsClose();
begin
 g_Game_StopMusic();
 g_Game_PlayMusic('MENU');
end;

procedure ProcGMClose();
begin
 g_Game_InGameMenu(False);
end;

procedure ProcGMShow();
begin
 if ((gGameSettings.GameType = GT_CUSTOM) and (gGameSettings.GameMode <> GM_DM)) or
    ((gGameSettings.GameType = GT_SINGLE) and
     ((gPlayer1 = nil) or (not gPlayer1.Live)) and ((gPlayer2 = nil) or (not gPlayer2.Live))) then
  TGUIMainMenu(g_ActiveWindow.GetControl(g_ActiveWindow.DefControl)).EnableButton('save', False)
   else TGUIMainMenu(g_ActiveWindow.GetControl(g_ActiveWindow.DefControl)).EnableButton('save', True);
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

function CreateYNMenu(Name, Text: string; MaxLen: Word; FontID: DWORD;
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

procedure CreatePlayerOptionsMenu(s: string);
var
  Menu: TGUIWindow;
  a: string;
begin
 Menu := TGUIWindow.Create('OptionsPlayers'+s+'Menu');
 if s = 'P1' then a := I_MENU_PLAYER1 else a := I_MENU_PLAYER2;
 with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, a))) do
 begin
  Name := 'mOptionsPlayers'+s+'Menu';
  with AddEdit(I_MENU_PLAYERNAME) do
  begin
   Name := 'ed'+s+'Name';
   MaxLength := 12;
   Width := 12;
  end;
  with AddSwitch(I_MENU_TEAM) do
  begin
   Name := 'sw'+s+'Team';
   AddItem(I_MENU_REDTEAM);
   AddItem(I_MENU_BLUETEAM); 
  end ;
  with AddList(I_MENU_MODEL, 12, 6) do
  begin
   Name := 'ls'+s+'Model';
   Sort := True;
   Items := g_PlayerModel_GetNames();
   OnChange := ProcSelectModel;
  end;
  with AddScroll(I_MENU_RED) do
  begin
   Name := 'sc'+s+'Red';
   Max := 16;
   OnChange := ProcChangeColor;
  end;
  with AddScroll(I_MENU_GREEN) do
  begin
   Name := 'sc'+s+'Green';
   Max := 16;
   OnChange := ProcChangeColor;
  end;
  with AddScroll(I_MENU_BLUE) do
  begin
   Name := 'sc'+s+'Blue';
   Max := 16;
   OnChange := ProcChangeColor;
  end;
  AddSpace();
  AddButton(@ProcOptionsPlayersMIMenu, I_MENU_MODELINFO);
  AddButton(@ProcOptionsPlayersAnim, I_MENU_MODELANIM);
  AddButton(@ProcOptionsPlayersWeap, I_MENU_MODELWEAP);
  AddButton(@ProcOptionsPlayersRot, I_MENU_MODELROT);
  
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

procedure g_Menu_Init();
var
  Menu: TGUIWindow;
  SR: TSearchRec;
  a, cx, _y: Integer;
begin
 MenuLoadData();
 g_GUI_Init();

 Menu := TGUIWindow.Create('MainMenu');
 with TGUIMainMenu(Menu.AddChild(TGUIMainMenu.Create(gMenuFont, I_MENU_MAINMENU))) do
 begin
  Name := 'mmMainMenu';
  AddButton(nil, I_MENU_NEWGAME, 'NewGameMenu');
  AddButton(nil, I_MENU_MULTIPLAYER, '');
  AddButton(nil, I_MENU_LOADGAME, 'LoadMenu');
  AddButton(@ReadOptions, I_MENU_OPTIONS, 'OptionsMenu');
  AddButton(@ProcAuthorsMenu, I_MENU_AUTHORS, 'AuthorsMenu');
  AddButton(nil, I_MENU_EXIT, 'ExitMenu');
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create('Doom 2D: Forever '+GAME_VERSION, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 255, 255);
  X := gScreenWidth-GetWidth-8;
  Y := gScreenHeight-GetHeight-8;
 end;
 Menu.DefControl := 'mmMainMenu';
 Menu.MainWindow := True;
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('NewGameMenu');
 with TGUIMainMenu(Menu.AddChild(TGUIMainMenu.Create(gMenuFont, I_MENU_NEWGAME))) do
 begin
  Name := 'mmNewGameMenu';
  AddButton(@ProcSingle1Player, I_MENU_1PLAYER);
  AddButton(@ProcSingle2Players, I_MENU_2PLAYERS);
  AddButton(nil, I_MENU_CUSTOMGAME, 'CustomGameMenu');
  AddButton(nil, I_MENU_EPISODE, 'EpisodeMenu');
 end;
 Menu.DefControl := 'mmNewGameMenu';
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('LoadMenu');
 Menu.OnShow := ProcLoadMenu;
 with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, I_MENU_LOADGAME))) do
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
 with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, I_MENU_SAVEGAME))) do
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
 with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, I_MENU_CUSTOMGAME))) do
 begin
  Name := 'mCustomGameMenu';
  with AddLabel(I_MENU_MAP) do
  begin
   Name := 'lbMap';
   FixedLength := 16;
   OnClick := @ProcSelectMapMenu;
  end;
  with AddSwitch(I_MENU_GAMETYPE) do
  begin
   Name := 'swGameMode';
   AddItem(I_MENU_DM);
   AddItem(I_MENU_TDM);
   AddItem(I_MENU_CTF);
   AddItem(I_MENU_COOP);
  end;
  with AddEdit(I_MENU_TIMELIMIT) do
  begin
   Name := 'edTimeLimit';
   OnlyDigits := True;
   Width := 3;
   MaxLength := 3;
  end;
  with AddEdit(I_MENU_GOALLIMIT) do
  begin
   Name := 'edGoalLimit';
   OnlyDigits := True;
   Width := 3;
   MaxLength := 3;
  end;
  with AddSwitch(I_MENU_PLAYERS) do
  begin
   Name := 'swPlayers';
   AddItem(I_MENU_ONEPLAYER);
   AddItem(I_MENU_TWOPLAYER);
  end;
  with AddSwitch(I_MENU_TEAMDAMAGE) do
  begin
   Name := 'swTeamDamage';
   AddItem(I_MENU_YES);
   AddItem(I_MENU_NO);
   ItemIndex := 1;
  end;
  with AddSwitch(I_MENU_ENABLEEXITS) do
  begin
   Name := 'swEnableExits';
   AddItem(I_MENU_YES);
   AddItem(I_MENU_NO);
  end;
  with AddSwitch(I_MENU_WEAPONSTAY) do
  begin
   Name := 'swWeaponStay';
   AddItem(I_MENU_YES);
   AddItem(I_MENU_NO);
   ItemIndex := 1;
  end;
  with AddSwitch(I_MENU_MONSTERDM) do
  begin
   Name := 'swMonsterDM';
   AddItem(I_MENU_YES);
   AddItem(I_MENU_NO);
   ItemIndex := 1;
  end;
  AddSpace();
  AddButton(@ProcStartCustomGame, I_MENU_STARTGAME);

  ReAlign();
 end;
 Menu.DefControl := 'mCustomGameMenu';
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('EpisodeMenu');
 with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, I_MENU_EPISODE))) do
 begin
  Name := 'mEpisodeMenu';

  AddSpace();
  AddSpace();
  AddSpace();
  AddSpace();
  AddSpace();
  AddSpace();

  with AddFileList('', 15, 4) do
  begin
   Name := 'lsWAD';
   OnChange := ProcSelectEpisodeWAD;

   Dirs := True;
   FileMask := '*.wad';
   SetBase(MapsDir+'megawads\');
  end;

  with AddLabel(I_MENU_MAPNAME) do
  begin
   Name := 'lbWADName';
   FixedLength := 8;
   Enabled := False;
  end;
  with AddLabel(I_MENU_MAPAUTHOR) do
  begin
   Name := 'lbWADAuthor';
   FixedLength := 8;
   Enabled := False;
  end;
  AddLine(I_MENU_MAPDESCRIPTION);
  with AddMemo('', 15, 3) do
  begin
   Name := 'meWADDescription';
   Color := MENU_ITEMSCTRL_COLOR;
  end;
  with AddSwitch(I_MENU_PLAYERS2) do
  begin
   Name := 'swPlayers';
   AddItem(I_MENU_ONEPLAYER);
   AddItem(I_MENU_TWOPLAYER);
  end;
  AddSpace();
  AddButton(@ProcStartEpisode, I_MENU_STARTGAME);

  ReAlign();

  with TGUIImage(Menu.AddChild(TGUIImage.Create)) do
  begin
   Name := 'mpWADImage';
   DefaultRes := 'NOPIC';
   X := GetControl('lsWAD').X+4;
   Y := GetControl('lsWAD').Y-128-MENU_VSPACE;
  end;
 end;
 Menu.DefControl := 'mEpisodeMenu';
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('SelectMapMenu');
 with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, I_MENU_SELECTMAP))) do
 begin
  Name := 'mSelectMapMenu';
  with AddFileList(I_MENU_MAPWAD, 12, 4) do
  begin
   Name := 'lsMapWAD';
   OnChange := ProcSelectWAD;

   Dirs := True;
   FileMask := '*.wad';
   SetBase(MapsDir);
  end;
  with AddList(I_MENU_MAPRES, 12, 4) do
  begin
   Name := 'lsMapRes';
   Sort := True;
   OnChange := ProcSelectMap;
  end;
  AddSpace();
  with AddLabel(I_MENU_MAPNAME) do
  begin
   Name := 'lbMapName';
   FixedLength := 24;
   Enabled := False;
  end;
  with AddLabel(I_MENU_MAPAUTHOR) do
  begin
   Name := 'lbMapAuthor';
   FixedLength := 16;
   Enabled := False;
  end;
  with AddLabel(I_MENU_MAPSIZE) do
  begin
   Name := 'lbMapSize';
   FixedLength := 10;
   Enabled := False;
  end;
  with AddMemo(I_MENU_MAPDESCRIPTION, 12, 4) do
  begin
    Name := 'meMapDescription';
    {FStartLine := 0;}
  end;

  ReAlign();

  with TGUIMapPreview(Menu.AddChild(TGUIMapPreview.Create)) do
  begin
   Name := 'mpMapPreview';
   X := GetControl('lsMapWAD').X+TGUIListBox(GetControl('lsMapWAD')).GetWidth()+2;
   Y := GetControl('lsMapWAD').Y;
  end;
 end;
 Menu.OnClose := ProcSetMap;
 Menu.DefControl := 'mSelectMapMenu';
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('OptionsMenu');
 with TGUIMainMenu(Menu.AddChild(TGUIMainMenu.Create(gMenuFont, I_MENU_OPTIONS))) do
 begin
  Name := 'mmOptionsMenu';
  AddButton(nil, I_MENU_VIDEOOPTIONS, 'OptionsVideoMenu');
  AddButton(nil, I_MENU_SOUNDOPTIONS, 'OptionsSoundMenu');
  AddButton(nil, I_MENU_GAMEOPTIONS, 'OptionsGameMenu');
  AddButton(nil, I_MENU_CONTROLSOPTIONS, 'OptionsControlsMenu');
  AddButton(nil, I_MENU_PLAYEROPTIONS, 'OptionsPlayersMenu');
  AddSpace();
  AddButton(nil, I_MENU_SAVEDOPTIONS, 'SavedOptionsMenu').Color := _RGB(255, 0, 0);
  AddButton(nil, I_MENU_DEFAULTOPTIONS, 'DefaultOptionsMenu').Color := _RGB(255, 0, 0);
 end;
 Menu.OnClose := ProcApplyOptions;
 Menu.DefControl := 'mmOptionsMenu';
 g_GUI_AddWindow(Menu);

 Menu := CreateYNMenu('SavedOptionsMenu', I_MENU_SETSAVED, Round(gScreenWidth*0.6),
                      gMenuSmallFont, @ProcSavedMenuKeyDown);
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('OptionsVideoMenu');
 with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, I_MENU_VIDEOOPTIONS))) do
 begin
  Name := 'mOptionsVideoMenu';
  with AddSwitch(I_MENU_RESOLUTION) do
  begin
   Name := 'swResolution';
   AddItem('640x480');
   AddItem('800x600');
   AddItem('1024x768'); 
  end;
  with AddSwitch(I_MENU_FULLSCREEN) do
  begin
   Name := 'swFullScreen';
   AddItem(I_MENU_YES);
   AddItem(I_MENU_NO);
  end;
  with AddSwitch(I_MENU_BPP) do
  begin
   Name := 'swBPP';
   AddItem('16');
   AddItem('32');
  end;
  with AddSwitch(I_MENU_VSYNC) do
  begin
   Name := 'swVSync';
   AddItem(I_MENU_YES);
   AddItem(I_MENU_NO);
  end;
  with AddSwitch(I_MENU_TEXTUREFILTER) do
  begin
   Name := 'swTextureFilter';
   AddItem(I_MENU_YES);
   AddItem(I_MENU_NO);
  end;
  AddSpace();
  AddText(I_MENU_VIDEORESTART, Round(gScreenWidth*0.6)); 
  ReAlign();
 end;
 Menu.DefControl := 'mOptionsVideoMenu';
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('OptionsSoundMenu');
 with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, I_MENU_SOUNDOPTIONS))) do
 begin
  Name := 'mOptionsSoundMenu';
  with AddScroll(I_MENU_MUSICLEVEL) do
  begin
   Name := 'scMusicLevel';
   Max := 16;
   OnChange := ProcChangeSoundSettings;
  end;
  with AddScroll(I_MENU_SOUNDLEVEL) do
  begin
   Name := 'scSoundLevel';
   Max := 16;
   OnChange := ProcChangeSoundSettings;
  end;
  ReAlign();
 end;
 Menu.DefControl := 'mOptionsSoundMenu';
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('OptionsGameMenu');
 with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, I_MENU_GAMEOPTIONS))) do
 begin
  Name := 'mOptionsGameMenu';
  with AddScroll(I_MENU_PARTICLESCOUNT) do
  begin
   Name := 'scParticlesCount';
   Max := 20;
  end;
  with AddSwitch(I_MENU_BLOODCOUNT) do
  begin
   Name := 'swBloodCount';
   AddItem(I_MENU_BLOODNONE);
   AddItem(I_MENU_BLOODSMALL);
   AddItem(I_MENU_BLOODNORMAL);
   AddItem(I_MENU_BLOODBIG);
   AddItem(I_MENU_BLOODVERYBIG);
  end;
  with AddScroll(I_MENU_GIBSCOUNTMAX) do
  begin
   Name := 'scGibsMax';
   Max := 20;
  end;
  with AddScroll(I_MENU_CORPSESCOUNTMAX) do
  begin
   Name := 'scCorpsesMax';
   Max := 20;
  end;
  with AddSwitch(I_MENU_GIBSCOUNT) do
  begin
   Name := 'swGibsCount';
   AddItem(I_MENU_GIBSNONE);
   AddItem(I_MENU_BLOODSMALL);
   AddItem(I_MENU_BLOODNORMAL);
   AddItem(I_MENU_BLOODBIG);
   AddItem(I_MENU_BLOODVERYBIG);
  end;
  with AddSwitch(I_MENU_CORPSETYPE) do
  begin
   Name := 'swCorpseType';
   AddItem(I_MENU_CORPSESIMPLE);
   AddItem(I_MENU_CORPSEADV);
  end;
  with AddSwitch(I_MENU_GIBSTYPE) do
  begin
   Name := 'swGibsType';
   AddItem(I_MENU_GIBSSIMPLE);
   AddItem(I_MENU_GIBSADV);
  end;
  with AddSwitch(I_MENU_BLOODTYPE) do
  begin
   Name := 'swBloodType';
   AddItem(I_MENU_BLOODSIMPLE);
   AddItem(I_MENU_BLOODADV);
  end;
  with AddSwitch(I_MENU_SCREENFLASH) do
  begin
   Name := 'swScreenFlash';
   AddItem(I_MENU_YES);
   AddItem(I_MENU_NO);
  end;
  with AddSwitch(I_MENU_BACKGROUND) do
  begin
   Name := 'swBackground';
   AddItem(I_MENU_YES);
   AddItem(I_MENU_NO);
  end;
  with AddSwitch(I_MENU_MESSAGES) do
  begin
   Name := 'swMessages';
   AddItem(I_MENU_YES);
   AddItem(I_MENU_NO);
  end;
  with AddSwitch(I_MENU_REVERTPLAYERS) do
  begin
   Name := 'swRevertPlayers';
   AddItem(I_MENU_YES);
   AddItem(I_MENU_NO);
  end;
  ReAlign();
 end;
 Menu.DefControl := 'mOptionsGameMenu';
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('OptionsControlsMenu');
 with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, I_MENU_CONTROLSOPTIONS))) do
 begin
  Name := 'mOptionsControlsMenu';
  AddLine(I_MENU_CONTROL_GLOBAL);
  AddKeyRead(I_MENU_CONTROL_SCREENSHOT).Name := I_MENU_CONTROL_SCREENSHOT;
  AddKeyRead(I_MENU_CONTROL_STAT).Name := I_MENU_CONTROL_STAT;
  AddSpace();
  AddButton(nil, I_MENU_PLAYER1, 'OptionsControlsP1Menu');
  AddButton(nil, I_MENU_PLAYER2, 'OptionsControlsP2Menu');
 end;
 Menu.DefControl := 'mOptionsControlsMenu';
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('OptionsControlsP1Menu');
 with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, I_MENU_PLAYER1))) do
 begin
  Name := 'mOptionsControlsP1Menu';
  AddKeyRead(I_MENU_CONTROL_LEFT).Name := I_MENU_CONTROL_LEFT;
  AddKeyRead(I_MENU_CONTROL_RIGHT).Name := I_MENU_CONTROL_RIGHT;
  AddKeyRead(I_MENU_CONTROL_UP).Name := I_MENU_CONTROL_UP;
  AddKeyRead(I_MENU_CONTROL_DOWN).Name := I_MENU_CONTROL_DOWN;
  AddKeyRead(I_MENU_CONTROL_JUMP).Name := I_MENU_CONTROL_JUMP;
  AddKeyRead(I_MENU_CONTROL_FIRE).Name := I_MENU_CONTROL_FIRE;
  AddKeyRead(I_MENU_CONTROL_USE).Name := I_MENU_CONTROL_USE;
  AddKeyRead(I_MENU_CONTROL_NEXTWEAPON).Name := I_MENU_CONTROL_NEXTWEAPON;
  AddKeyRead(I_MENU_CONTROL_PREVWEAPON).Name := I_MENU_CONTROL_PREVWEAPON;
 end;
 Menu.DefControl := 'mOptionsControlsP1Menu';
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('OptionsControlsP2Menu');
 with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, I_MENU_PLAYER2))) do
 begin
  Name := 'mOptionsControlsP2Menu';
  AddKeyRead(I_MENU_CONTROL_LEFT).Name := I_MENU_CONTROL_LEFT;
  AddKeyRead(I_MENU_CONTROL_RIGHT).Name := I_MENU_CONTROL_RIGHT;
  AddKeyRead(I_MENU_CONTROL_UP).Name := I_MENU_CONTROL_UP;
  AddKeyRead(I_MENU_CONTROL_DOWN).Name := I_MENU_CONTROL_DOWN;
  AddKeyRead(I_MENU_CONTROL_JUMP).Name := I_MENU_CONTROL_JUMP;
  AddKeyRead(I_MENU_CONTROL_FIRE).Name := I_MENU_CONTROL_FIRE;
  AddKeyRead(I_MENU_CONTROL_USE).Name := I_MENU_CONTROL_USE;
  AddKeyRead(I_MENU_CONTROL_NEXTWEAPON).Name := I_MENU_CONTROL_NEXTWEAPON;
  AddKeyRead(I_MENU_CONTROL_PREVWEAPON).Name := I_MENU_CONTROL_PREVWEAPON;
 end;
 Menu.DefControl := 'mOptionsControlsP2Menu';
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('OptionsPlayersMenu');
 with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, I_MENU_PLAYEROPTIONS))) do
 begin
  Name := 'mOptionsPlayersMenu';
  AddButton(nil, I_MENU_PLAYER1+' '+I_MENU_UP, 'OptionsPlayersP1Menu');
  AddButton(nil, I_MENU_PLAYER2+' '+I_MENU_DOWN, 'OptionsPlayersP2Menu');
 end;
 Menu.DefControl := 'mOptionsPlayersMenu';
 g_GUI_AddWindow(Menu);

 CreatePlayerOptionsMenu('P1');
 CreatePlayerOptionsMenu('P2'); 

 Menu := TGUIWindow.Create('OptionsPlayersMIMenu');
 with TGUIMenu(Menu.AddChild(TGUIMenu.Create(gMenuFont, gMenuSmallFont, I_MENU_MODELINFO))) do
 begin
  Name := 'mOptionsPlayersMIMenu';
  with AddLabel(I_MENU_MODELNAME) do
  begin
   Name := 'lbName';
   FixedLength := 16;
  end;
  with AddLabel(I_MENU_MODELAUTHOR) do
  begin
   Name := 'lbAuthor';
   FixedLength := 16;
  end;
  with AddMemo(I_MENU_MODELCOMMENT, 14, 6) do
  begin
   Name := 'meComment';
  end;
  AddSpace();
  AddLine(I_MENU_MODELOPTIONS);
  with AddLabel(I_MENU_MODELWEAPON) do
  begin
   Name := 'lbWeapon';
   FixedLength := Max(Length(I_MENU_YES), Length(I_MENU_NO));
  end;
  ReAlign();
 end;
 Menu.DefControl := 'mOptionsPlayersMIMenu';
 g_GUI_AddWindow(Menu);

 Menu := CreateYNMenu('DefaultOptionsMenu', I_MENU_SETDEFAULT, Round(gScreenWidth*0.6),
                      gMenuSmallFont, @ProcDefaultMenuKeyDown);
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('AuthorsMenu');
 Menu.BackTexture := 'INTER';
 Menu.OnClose := ProcAuthorsClose;
 _y := 16;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS01, gMenuFont))) do
 begin
  Color := _RGB(255, 0, 0);
  X := (gScreenWidth div 2)-(GetWidth() div 2);
  Y := _y;
  _y := _y+GetHeight();
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS02, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 0, 0);
  X := (gScreenWidth div 2)-(GetWidth() div 2);
  Y := _y;
  _y := _y+GetHeight()+32;
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS03, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 0, 0);
  X := (gScreenWidth div 2)-(GetWidth() div 2);
  Y := _y;
  _y := _y+GetHeight()+16;
 end;
 cx := (gScreenWidth div 2)+96;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS04, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 0, 0);
  X := cx-GetWidth()-16;
  Y := _y;
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS05, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 255, 255);
  X := cx+16;
  Y := _y;
  _y := _y+GetHeight()+16;
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS06, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 0, 0);
  X := cx-GetWidth()-16;
  Y := _y;
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS08, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 255, 255);
  X := cx+16;
  Y := _y;
  _y := _y+GetHeight();
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS07, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 0, 0);
  X := cx-GetWidth()-16;
  Y := _y;
  _y := _y+GetHeight()+16;
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS09, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 0, 0);
  X := cx-GetWidth()-16;
  Y := _y;
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS10, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 255, 255);
  X := cx+16;
  Y := _y;
  _y := _y+GetHeight()+16;
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS11, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 0, 0);
  X := cx-GetWidth()-16;
  Y := _y;
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS12, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 255, 255);
  X := cx+16;
  Y := _y;
  _y := _y+GetHeight()+16;
 end;

 if (I_CREDITS13 <> '') or (I_CREDITS14 <> '') then
 begin
  with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS13, gMenuSmallFont))) do
  begin
   Color := _RGB(255, 0, 0);
   X := cx-GetWidth()-16;
   Y := _y;
  end;
  with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS14, gMenuSmallFont))) do
  begin
   Color := _RGB(255, 255, 255);
   X := cx+16;
   Y := _y;
   _y := _y+GetHeight()+32;
  end;
 end;

 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS15, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 0, 0);
  X := (gScreenWidth div 2)-(GetWidth() div 2);
  Y := _y;
  _y := _y+GetHeight()+16;
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS16, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 0, 0);
  X := (gScreenWidth div 2)-(GetWidth() div 2);
  Y := _y;
  _y := _y+GetHeight();
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS17, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 0, 0);
  X := (gScreenWidth div 2)-(GetWidth() div 2);
  Y := _y;
  _y := _y+GetHeight();
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS18, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 0, 0);
  X := (gScreenWidth div 2)-(GetWidth() div 2);
  Y := _y;
  _y := _y+GetHeight();
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS19, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 0, 0);
  X := (gScreenWidth div 2)-(GetWidth() div 2);
  Y := _y;
  _y := _y+GetHeight();
 end;
 with TGUILabel(Menu.AddChild(TGUILabel.Create(I_CREDITS20, gMenuSmallFont))) do
 begin
  Color := _RGB(255, 0, 0);
  X := (gScreenWidth div 2)-(GetWidth() div 2);
  Y := _y;
 end;
 g_GUI_AddWindow(Menu);

 Menu := CreateYNMenu('ExitMenu', I_MENU_EXIT1, Round(gScreenWidth*0.6),
                      gMenuSmallFont, @ProcExitMenuKeyDown);
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('GameSingleMenu');
 with TGUIMainMenu(Menu.AddChild(TGUIMainMenu.Create(gMenuFont, I_MENU_MAINMENU))) do
 begin
  Name := 'mmGameSingleMenu';
  AddButton(nil, I_MENU_LOADGAME, 'LoadMenu');
  AddButton(nil, I_MENU_SAVEGAME, 'SaveMenu').Name := 'save';
  AddButton(@ReadOptions, I_MENU_OPTIONS, 'OptionsMenu');
  AddButton(nil, I_MENU_RESTART, 'RestartGameMenu');
  AddButton(nil, I_MENU_ENDGAME, 'EndGameMenu');
 end;
 Menu.DefControl := 'mmGameSingleMenu';
 Menu.MainWindow := True;
 Menu.OnClose := ProcGMClose;
 Menu.OnShow := ProcGMShow;
 g_GUI_AddWindow(Menu);

 Menu := CreateYNMenu('EndGameMenu', I_MENU_ENDGAME1, Round(gScreenWidth*0.6),
                      gMenuSmallFont, @ProcEndMenuKeyDown);
 g_GUI_AddWindow(Menu);

 Menu := CreateYNMenu('RestartGameMenu', I_MENU_RESTARTGAME, Round(gScreenWidth*0.6),
                      gMenuSmallFont, @ProcRestartMenuKeyDown);
 g_GUI_AddWindow(Menu);

 Menu := TGUIWindow.Create('GameCustomMenu');
 with TGUIMainMenu(Menu.AddChild(TGUIMainMenu.Create(gMenuFont, I_MENU_MAINMENU))) do
 begin
  Name := 'mmGameCustomMenu';
  AddButton(nil, I_MENU_LOADGAME, 'LoadMenu');
  AddButton(nil, I_MENU_SAVEGAME, 'SaveMenu').Name := 'save';
  AddButton(@ReadOptions, I_MENU_OPTIONS, 'OptionsMenu');
  AddButton(nil, I_MENU_RESTART, 'RestartGameMenu');
  AddButton(nil, I_MENU_ENDGAME, 'EndGameMenu');
 end;
 Menu.DefControl := 'mmGameCustomMenu';
 Menu.MainWindow := True;
 Menu.OnClose := ProcGMClose;
 Menu.OnShow := ProcGMShow;
 g_GUI_AddWindow(Menu);
end;

procedure g_Menu_Free();
begin
 //g_GUIDestroy;

 e_WriteLog('Releasing menu data...', MSG_NOTIFY);

 MenuFreeData();
end;

end.
