Unit g_game;

Interface

Uses
  Windows, g_basic, g_player, Messages, e_graphics,
  SysUtils, g_sound, MAPSTRUCT, WADEDITOR;

Type
  TGameSettings = record
    GameType: Byte;
    GameMode: Byte;
    TimeLimit: Word;
    GoalLimit: Word;
    Options: LongWord;
    WAD: String;
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

procedure g_Game_Init();
procedure g_Game_Free();
procedure g_Game_LoadData();
procedure g_Game_FreeData();
procedure g_Game_Update();
procedure g_Game_Draw();
procedure g_Game_Quit();
procedure g_Game_SetupScreenSize();
procedure g_Game_ChangeResolution(newWidth, newHeight: Word; nowFull, nowMax: Boolean);
procedure g_Game_StartSingle(WAD, MAP: String; TwoPlayers: Boolean; nPlayers: Byte);
procedure g_Game_StartCustom(Map: String; GameMode: Byte; TimeLimit, GoalLimit: Word; Options: LongWord; nPlayers: Byte);
procedure g_Game_Restart();
procedure g_Game_SaveOptions();
function g_Game_StartMap(Map: String; Force: Boolean = False): Boolean;
procedure g_Game_ExitLevel(Map: Char16);
procedure g_Game_NextLevel();
procedure g_Game_Pause(Enable: Boolean);
procedure g_Game_InGameMenu(Show: Boolean);
procedure g_Game_Message(Msg: String; Time: Word);
procedure g_Game_LoadMapList(FileName: String);
procedure g_Game_PauseAllSounds(Enable: Boolean);
procedure g_Game_StopAllSounds();
function g_Game_GetMegaWADInfo(WAD: String): TMegaWADInfo;
procedure g_TakeScreenShot();
procedure g_FatalError(Text: String);
procedure g_Game_DeleteTestMap();
procedure GameCommands(P: SArray);
procedure g_Game_Process_Params;
procedure g_Game_SetLoadingText(Text: String; Max: Integer; reWrite: Boolean);
procedure g_Game_StepLoading();
procedure g_Game_ClearLoading();
procedure g_Game_SetDebugMode();
procedure DrawLoadingStat();
{ procedure SetWinPause(Enable: Boolean); }


Const
  GAME_TICK = 28;

  LOADING_SHOW_STEP = 100;
  LOADING_INTERLINE = 20;

  GT_NONE   = 0;
  GT_SINGLE = 1;
  GT_CUSTOM = 2;
  
  GM_NONE = 0;
  GM_DM   = 1;
  GM_TDM  = 2;
  GM_CTF  = 3;
  GM_COOP = 4;

  MESSAGE_DIKEY = WM_USER + 1;

  EXIT_QUIT            = 1;
  EXIT_SIMPLE          = 2;
  EXIT_RESTART         = 3;
  EXIT_ENDLEVELSINGLE  = 4;
  EXIT_ENDLEVELCUSTOM  = 5;

  GAME_OPTION_TWOPLAYER    = 1;
  GAME_OPTION_TEAMDAMAGE   = 2;
  GAME_OPTION_ALLOWEXIT    = 4;
  GAME_OPTION_WEAPONSTAY   = 8;
  GAME_OPTION_MONSTERDM    = 16;
  GAME_OPTION_BOTVSPLAYER  = 32;
  GAME_OPTION_BOTVSMONSTER = 64;
  //GAME_OPTION_MAPLIST    = 8;
  //GAME_OPTION_MAPCYCLE   = 16;
  //GAME_OPTION_MAPCHANGE  = 32;

  STATE_NONE        = 0;
  STATE_MENU        = 1;
  STATE_FOLD        = 2;
  STATE_INTERCUSTOM = 3;
  STATE_INTERSINGLE = 4;
  STATE_INTERTEXT   = 5;
  STATE_INTERPIC    = 6;
  STATE_ENDPIC      = 7;

  CONFIG_FILENAME = 'Doom2DF.cfg';
  LOG_FILENAME = 'Doom2DF.log';

  TEST_MAP_NAME = '$$$_TEST_$$$';

  STD_PLAYER_MODEL = 'Doomer';

Var
  gStdFont: DWORD;
  gGameSettings: TGameSettings;
  gPlayer1Settings: TPlayerSettings;
  gPlayer2Settings: TPlayerSettings;
  gGameOn: Boolean;
  gPlayerScreenSize: TPoint;
  gPlayer1ScreenCoord: TPoint;
  gPlayer2ScreenCoord: TPoint;
  gPlayer1: TPlayer = nil;
  gPlayer2: TPlayer = nil;
  gTime: LongWord;
  gGameStartTime: LongWord = 0;
  gPause: Boolean;
  gShowTime: Boolean = True;
  gShowFPS: Boolean = False;
  gShowGoals: Boolean = True;
  gShowStat: Boolean = True;
  gShowKillMsg: Boolean = True;
  gExit: Byte = 0;
  gState: Byte = STATE_NONE;
  sX, sY: Integer;
  sWidth, sHeight: Word;
  gMusic: TMusic = nil;
  gLoadGameMode: Boolean;
  gCheats: Boolean = False;
  gMapOnce: Boolean = False;
  gMapToDelete: String;
  gWinPosX, gWinPosY: Integer;
  gWinSizeX, gWinSizeY: Integer;
  gWinFrameX, gWinFrameY, gWinCaption: Integer;
  gWinActive: Boolean = False;
  gResolutionChange: Boolean = False;
  gRC_Width, gRC_Height: Word;
  gRC_FullScreen, gRC_Maximized: Boolean;
  gLanguageChange: Boolean = False;
  gIsNetGame: Boolean = False;
  gDebugMode: Boolean = False;
  g_debug_Sounds: Boolean = False;
  g_debug_Frames: Boolean = False;
  g_debug_WinMsgs: Boolean = False;
  g_debug_MonsterOff: Boolean = False;


Implementation

Uses
  g_textures, g_main, g_window, dglOpenGL, g_menu,
  g_gui, e_input, e_log, g_console, g_items, g_map,
  g_playermodel, g_gfx, g_options, g_weapons, Math,
  g_triggers, MAPDEF, g_monsters, e_sound, CONFIG,
  BinEditor, g_language;

Type
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
  end;

  TParamStrValue = record
    Name: String;
    Value: String;
  end;

  TParamStrValues = Array of TParamStrValue;

Const
  INTER_ACTION_TEXT = 1;
  INTER_ACTION_PIC = 2;
  INTER_ACTION_MUSIC = 3;

Var
  FPS, UPS: Word;
  FPSCounter, UPSCounter: Word;
  FPSTime, UPSTime: LongWord;
  DataLoaded: Boolean;
  LastScreenShot: Int64;
  IsDrawStat: Boolean = False;
  CustomStat: TEndCustomGameStat;
  SingleStat: TEndSingleGameStat;
  LoadingStat: TLoadingStat;
  EndingGameCounter: Byte = 0;
  P1MoveButton: Byte = 0;
  P2MoveButton: Byte = 0;
  MessageText: String;
  MessageTime: Word;
  NextMap: String;
  MapList: Array of String = nil;
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
  InterPic: String;
  InterText: record
    lines: SArray;
    img: String;
    cur_line: Integer;
    cur_char: Integer;
    counter: Integer;
    endtext: Boolean;
  end;

function Compare(a, b: TPlayerStat): Integer;
begin
 if a.Frags < b.Frags then Result := 1
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

function g_Game_GetMegaWADInfo(WAD: string): TMegaWADInfo;
var
  w: TWADEditor_1;
  cfg: TConfig;
  p: Pointer;
  len: Integer;
begin
 Result.name := ExtractFileName(WAD);
 Result.description := '';
 Result.author := '';

 w := TWADEditor_1.Create();
 w.ReadFile(WAD);

 if not w.GetResource('', 'INTERSCRIPT', p, len) then
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

procedure FreeMegaWAD();
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
end;

procedure LoadMegaWAD(WAD: string);
var
  w: TWADEditor_1;
  cfg: TConfig;
  p: Pointer;
  {b, }len: Integer;
  s: string;
begin
 FreeMegaWAD();

 MegaWAD.info := g_Game_GetMegaWADInfo(WAD);

 w := TWADEditor_1.Create();
 w.ReadFile(WAD);

 if not w.GetResource('', 'INTERSCRIPT', p, len) then
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
  g_ProcessResourceStr(MegaWAD.endpic, @s, nil, nil);
  if s = '' then s := WAD else s := GameDir+'\wads\';
  g_Texture_CreateWADEx('TEXTURE_endpic', s+MegaWAD.endpic);
 end;
 MegaWAD.endmus := cfg.ReadStr('megawad', 'endmus', 'Standart.wad:D2DMUS\КОНЕЦ');
 if MegaWAD.endmus <> '' then
 begin
  g_ProcessResourceStr(MegaWAD.endmus, @s, nil, nil);
  if s = '' then s := WAD else s := GameDir+'\wads\';
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
end;

procedure EndGame();
var
  a: Integer;
  FileName, SectionName, ResName: string;
begin
// Стоп игра:
  gPause := False;
  gGameOn := False;

  g_Game_StopAllSounds();

  MessageTime := 0;
  MessageText := '';

  EndingGameCounter := 0;
  g_ActiveWindow := nil;

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
            g_GUI_ShowWindow('MainMenu');
            gMusic.SetByName('MUSIC_MENU');
            gMusic.Play();
            gState := STATE_MENU;
          end;
      end;

    EXIT_RESTART: // Начать уровень сначала
      begin
        g_Game_Restart();
      end;

    EXIT_ENDLEVELCUSTOM: // Закончился уровень в Своей игре
      begin
      // Статистика Своей игры:
        g_ProcessResourceStr(gMapInfo.Map, FileName, SectionName, ResName);

        CustomStat.GameTime := gTime;
        CustomStat.Map := ExtractFileName(FileName)+':'+ResName;
        CustomStat.MapName := gMapInfo.Name;
        CustomStat.GameMode := gGameSettings.GameMode;
        if gGameSettings.GameMode in [GM_CTF, GM_TDM] then
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
              end;
            end;

          SortGameStat(CustomStat.PlayerStat);
        end;

      // Затухающий экран:
        EndingGameCounter := 255;
        gState := STATE_FOLD;
      end;

    EXIT_ENDLEVELSINGLE: // Закончился уровень в Одиночной игре
      begin
      // Статистика Одиночной игры:
        SingleStat.GameTime := gTime;
        SingleStat.TwoPlayers := LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER);
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

      // Есть еще карты:
        if NextMap <> '' then
          begin
            gMusic.SetByName('MUSIC_INTERMUS');
            gMusic.Play();
            gState := STATE_INTERSINGLE;
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

procedure DrawStat();
var
  pc, x, y, w, h: Integer;
  w1, w2, w3: Integer;
  a, aa: Integer;
  cw, ch, r, g, b: Byte;
  s1, s2: String;
  _y: Integer;
  stat: TPlayerStatArray;

begin
 pc := g_Player_GetCount();
 if pc = 0 then Exit;

 e_TextureFontGetSize(gStdFont, cw, ch);

 w := gScreenWidth-(gScreenWidth div 5);
 if gGameSettings.GameMode in [GM_CTF, GM_TDM] then
  h := 8+8+ch*2+ch+ch+(ch div 2)+pc*ch+ch+(ch div 2)+ch+ch+8
   else h := 8+8+ch*2+ch+ch+8+(ch+8)*pc+8;
 x := (gScreenWidth div 2)-(w div 2);
 y := (gScreenHeight div 2)-(h div 2);

 e_DrawFillQuad(x, y, x+w-1, y+h-1, 64, 64, 64, 32);
 e_DrawQuad(x, y, x+w-1, y+h-1, 255, 127, 0);

 stat := g_Player_GetStats();
 SortGameStat(stat); 

 case gGameSettings.GameMode of
  GM_DM:
   begin
    s1 := _lc[I_GAME_DM];
    s2 := Format(_lc[I_GAME_FRAG_LIMIT], [gGameSettings.GoalLimit]);
   end;

  GM_TDM:
   begin
    s1 := _lc[I_GAME_TDM];
    s2 := Format(_lc[I_GAME_FRAG_LIMIT], [gGameSettings.GoalLimit]);
   end;

  GM_CTF:
   begin
    s1 := _lc[I_GAME_CTF];
    s2 := Format(_lc[I_GAME_SCORE_LIMIT], [gGameSettings.GoalLimit]);
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
 e_TextureFontPrintEx(x+16, _y, s2, gStdFont, 200, 200, 200, 1);
 s1 := _lc[I_GAME_TIME_LIMIT];
 e_TextureFontPrintEx(x+w-16-(Length(s1)-4)*cw, _y, Format(s1,
                      [gGameSettings.TimeLimit div 60, gGameSettings.TimeLimit mod 60]),
                      gStdFont, 200, 200, 200, 1);

 w2 := (w-16) div 6;
 w3 := w2;
 w1 := w-16-w2-w3;

 if gGameSettings.GameMode in [GM_CTF, GM_TDM] then
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

   e_TextureFontPrintEx(x+16, _y, Format(s1, [gTeamStat[a].Goals]), gStdFont, r, g, b, 1);

   _y := _y+ch+(ch div 4);
   e_DrawLine(1, x+16, _y, x+w-16, _y, r, g, b);
   _y := _y+(ch div 4);

   for aa := 0 to High(stat) do
    if stat[aa].Team = a then
    with stat[aa] do
    begin
     e_TextureFontPrintEx(x+16, _y, Name, gStdFont, r, g, b, 1);
     e_TextureFontPrintEx(x+w1+16, _y, IntToStr(Frags), gStdFont, r, g, b, 1);
     e_TextureFontPrintEx(x+w1+w2+16, _y, IntToStr(Deaths), gStdFont, r, g, b, 1);
     _y := _y+ch;
    end;

   _y := _y+ch;
  end;
 end
  else if gGameSettings.GameMode = GM_DM then
 begin
  _y := _y+ch+ch;
  e_TextureFontPrintEx(x+16, _y, _lc[I_GAME_PLAYER_NAME], gStdFont, 255, 127, 0, 1);
  e_TextureFontPrintEx(x+16+w1, _y, _lc[I_GAME_FRAGS], gStdFont, 255, 127, 0, 1);
  e_TextureFontPrintEx(x+16+w1+w2, _y, _lc[I_GAME_DEATHS], gStdFont, 255, 127, 0, 1);

  _y := _y+ch+8;
  for aa := 0 to High(stat) do
   with stat[aa] do
   begin
    e_DrawFillQuad(x+16, _y+4, x+32-1, _y+16+4-1, Color.R, Color.G, Color.B, 0);
    e_DrawQuad(x+16, _y+4, x+32-1, _y+16+4-1, 192, 192, 192);
    e_TextureFontPrintEx(x+16+16+8, _y+4, Name, gStdFont, 255, 127, 0, 1);
    e_TextureFontPrintEx(x+w1+16, _y+4, IntToStr(Frags), gStdFont, 255, 127, 0, 1);
    e_TextureFontPrintEx(x+w1+w2+16, _y+4, IntToStr(Deaths), gStdFont, 255, 127, 0, 1);
    _y := _y+ch+8;
   end;
 end
end;

procedure g_Game_Init();
const
  CHRWDT1 = 32;
  CHRHGT1 = 32;
  FNTSPC1 = -2;
  CHRWDT2 = 16;
  CHRHGT2 = 16;
  FNTSPC2 = -1;
var
  ID: DWORD;
  SR: TSearchRec;
begin
 gExit := 0;
 gMapToDelete := '';

 g_Texture_CreateWADEx('MENU_BACKGROUND', GameWAD+':TEXTURES\TITLE');
 g_Texture_CreateWADEx('INTER', GameWAD+':TEXTURES\INTER');
 g_Texture_CreateWADEx('ENDGAME_EN', GameWAD+':TEXTURES\ENDGAME_EN');
 g_Texture_CreateWADEx('ENDGAME_RU', GameWAD+':TEXTURES\ENDGAME_RU');
 g_Texture_CreateWADEx('FONT_STD', GameWAD+':FONTS\STDFONT');
 if g_Texture_Get('FONT_STD', ID) then
   e_TextureFontBuild(ID, gStdFont, 16, 16, -6);

 LoadFont('MENUTXT', 'MENUFONT', CHRWDT1, CHRHGT1, FNTSPC1, gMenuFont);
 LoadFont('SMALLTXT', 'SMALLFONT', CHRWDT2, CHRHGT2, FNTSPC2, gMenuSmallFont);

 g_Game_ClearLoading();
 g_Game_SetLoadingText(_lc[I_LOAD_MUSIC], 0, False);
 g_Sound_CreateWADEx('MUSIC_INTERMUS', GameWAD+':MUSIC\INTERMUS', True);
 g_Sound_CreateWADEx('MUSIC_MENU', GameWAD+':MUSIC\MENU', True);
 g_Sound_CreateWADEx('SOUND_ROUNDMUS', GameWAD+':MUSIC\ROUNDMUS');
 g_Sound_CreateWADEx('MUSIC_STDENDMUS', GameWAD+':MUSIC\ENDMUS', True);

 g_Game_SetLoadingText(_lc[I_LOAD_MODELS], 0, False);
 g_PlayerModel_LoadData();
 
 if FindFirst(ModelsDir+'*.wad', faAnyFile, SR) = 0 then
 repeat
  if not g_PlayerModel_Load(ModelsDir+SR.Name) then
   e_WriteLog(Format('Error loading model %s', [SR.Name]), MSG_WARNING);
 until FindNext(SR) <> 0;
 FindClose(SR);

 g_Game_SetLoadingText(_lc[I_LOAD_MENUS], 0, False);
 g_Menu_Init();

 g_Game_SetLoadingText(_lc[I_LOAD_CONSOLE], 0, False);
 g_Console_Init();

 DataLoaded := False;

 gGameOn := False;
 gPause := False;
 gTime := 0;
 LastScreenShot := 0;

 e_MouseInfo.Accel := 1.0;

 gMusic := TMusic.Create();
 gMusic.SetByName('MUSIC_MENU');
 gMusic.Play();

 gState := STATE_MENU;
end;

procedure g_Game_Free();
begin
 g_Map_Free();
 g_Player_Free();
 g_Player_RemoveAllCorpses();

 gPlayer1 := nil;
 gPlayer2 := nil;
end;

procedure g_Game_Update();
var
  Msg: g_gui.TMessage;
  Time: Int64;
  a: Byte;
  i, b: Integer;
begin
// Пора выключать игру:
  if gExit = EXIT_QUIT then
    Exit;
// Игра закончилась - обрабатываем:
  if gExit <> 0 then
    EndGame();

// Читаем клавиатуру, если окно активно:
  e_PollKeyboard();
// Обновляем консоль (движение и сообщения):
  g_Console_Update();

  case gState of
    STATE_INTERSINGLE, // Статистка после прохождения уровня в Одиночной игре
    STATE_INTERCUSTOM, // Статистка после прохождения уровня в Своей игре
    STATE_INTERTEXT, // Текст между уровнями
    STATE_INTERPIC: // Картинка между уровнями
      begin
        if ((e_KeyBuffer[28] = $080) or (e_KeyBuffer[57] = $080)) and
           (not gConsoleShow) and (g_ActiveWindow = nil) then
        begin // Нажали <Enter> или <Пробел>:
          g_Game_StopAllSounds();

          if gMapOnce then // Это был тест
            gExit := EXIT_SIMPLE
          else
            if NextMap <> '' then
              begin // Переходим на следующую карту
                g_Game_ClearLoading();
                g_Game_StartMap(NextMap);
              end
            else // Следующей карты нет
              begin
                if gGameSettings.GameType = GT_CUSTOM then
                  begin
                  // Выход в главное меню:
                    g_GUI_ShowWindow('MainMenu');
                    gMusic.SetByName('MUSIC_MENU');
                    gMusic.Play();
                    gState := STATE_MENU;
                  end
                else
                  begin
                  // Финальная картинка:
                    g_Game_Free();
                    if not gMusic.SetByName('MUSIC_endmus') then
                      gMusic.SetByName('MUSIC_STDENDMUS');
                    gMusic.Play();
                    gState := STATE_ENDPIC;
                  end;
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
            if gGameSettings.GameType = GT_CUSTOM then
              begin
                g_Sound_PlayEx('SOUND_ROUNDMUS');
                gState := STATE_INTERCUSTOM;
              end
            else // Закончилась последняя карта в Одиночной игре
              begin
                gMusic.SetByName('MUSIC_INTERMUS');
                gMusic.Play();
                gState := STATE_INTERSINGLE;
              end;
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
  end;

// Статистика по Tab:
  if gGameOn then
    IsDrawStat := (not gConsoleShow) and (gGameSettings.GameType <> GT_SINGLE) and
                  (e_KeyBuffer[gGameControls.GameControls.Stat] = $080);

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

  // Был задан лимит времени:
    if gGameSettings.TimeLimit > 0 then
      if (gTime - gGameStartTime) div 1000 >= gGameSettings.TimeLimit then
      begin // Он прошел => конец уровня
        gExit := EXIT_ENDLEVELCUSTOM;
        g_Game_NextLevel();
        Exit;
      end;

  // Был задан лимит побед:
    if gGameSettings.GoalLimit > 0 then
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
        if gGameSettings.GameMode in [GM_CTF, GM_TDM] then
        begin // В CTF/TDM выбираем команду с наибольшим счетом
          b := Max(gTeamStat[TEAM_RED].Goals, gTeamStat[TEAM_BLUE].Goals);
        end;

    // Лимит побед набран => конец уровня:
      if b >= gGameSettings.GoalLimit then
      begin
        gExit := EXIT_ENDLEVELCUSTOM;
        g_Game_NextLevel();
        Exit;
      end;
    end;

  // Обрабатываем клавиши игроков:
    if (not gConsoleShow) and (g_ActiveWindow = nil) then
    begin
      with gGameControls.P1Control do
      begin
        if (e_KeyBuffer[KeyLeft] = $080) and (e_KeyBuffer[KeyRight] <> $080) then
          P1MoveButton := 1 // Нажата только "Влево"
        else
          if (e_KeyBuffer[KeyLeft] <> $080) and (e_KeyBuffer[KeyRight] = $080) then
            P1MoveButton := 2 // Нажата только "Вправо"
          else
            if (e_KeyBuffer[KeyLeft] <> $080) and (e_KeyBuffer[KeyRight] <> $080) then
              P1MoveButton := 0; // Не нажаты ни "Влево", ни "Вправо"

        gPlayer1.ReleaseKeys();

      // Сейчас или раньше были нажаты "Влево"/"Вправо" => передаем игроку:
        if P1MoveButton = 1 then
          gPlayer1.PressKey(KEY_LEFT)
        else
         if P1MoveButton = 2 then
           gPlayer1.PressKey(KEY_RIGHT);

      // Раньше была нажата "Вправо", а сейчас "Влево" => бежим вправо, смотрим влево:
        if (P1MoveButton = 2) and (e_KeyBuffer[KeyLeft] = $080) then
          gPlayer1.SetDirection(D_LEFT)
        else
        // Раньше была нажата "Влево", а сейчас "Вправо" => бежим влево, смотрим вправо:
          if (P1MoveButton = 1) and (e_KeyBuffer[KeyRight] = $080) then
            gPlayer1.SetDirection(D_RIGHT)
          else
          // Что-то было нажато и не изменилось => куда бежим, туда и смотрим:
            if P1MoveButton <> 0 then
              gPlayer1.SetDirection(TDirection(P1MoveButton-1));

      // Остальные клавиши:
        if e_KeyBuffer[KeyJump] = $080 then gPlayer1.PressKey(KEY_JUMP);
        if e_KeyBuffer[KeyUp] = $080 then gPlayer1.PressKey(KEY_UP);
        if e_KeyBuffer[KeyDown] = $080 then gPlayer1.PressKey(KEY_DOWN);
        if e_KeyBuffer[KeyFire] = $080 then gPlayer1.PressKey(KEY_FIRE);
        if e_KeyBuffer[KeyNextWeapon] = $080 then gPlayer1.PressKey(KEY_NEXTWEAPON);
        if e_KeyBuffer[KeyPrevWeapon] = $080 then gPlayer1.PressKey(KEY_PREVWEAPON);
        if e_KeyBuffer[KeyOpen] = $080 then gPlayer1.PressKey(KEY_OPEN);
      end;

    // Второй игрок (если есть):
      if gPlayer2 <> nil then
        with gGameControls.P2Control do
        begin
          if (e_KeyBuffer[KeyLeft] = $080) and (e_KeyBuffer[KeyRight] <> $080) then
            P2MoveButton := 1 // Нажата только "Влево"
          else
            if (e_KeyBuffer[KeyLeft] <> $080) and (e_KeyBuffer[KeyRight] = $080) then
              P2MoveButton := 2 // Нажата только "Вправо"
            else
              if (e_KeyBuffer[KeyLeft] <> $080) and (e_KeyBuffer[KeyRight] <> $080) then
                P2MoveButton := 0; // Не нажаты ни "Влево", ни "Вправо"

          gPlayer2.ReleaseKeys();

        // Сейчас или раньше были нажаты "Влево"/"Вправо" => передаем игроку:
          if P2MoveButton = 1 then
            gPlayer2.PressKey(KEY_LEFT)
          else
            if P2MoveButton = 2 then
              gPlayer2.PressKey(KEY_RIGHT);

        // Раньше была нажата "Вправо", а сейчас "Влево" => бежим вправо, смотрим влево:
          if (P2MoveButton = 2) and (e_KeyBuffer[KeyLeft] = $080) then
            gPlayer2.SetDirection(D_LEFT)
          else
          // Раньше была нажата "Влево", а сейчас "Вправо" => бежим влево, смотрим вправо:
            if (P2MoveButton = 1) and (e_KeyBuffer[KeyRight] = $080) then
              gPlayer2.SetDirection(D_RIGHT)
            else
            // Что-то было нажато и не изменилось => куда бежим, туда и смотрим:
              if P2MoveButton <> 0 then
                gPlayer2.SetDirection(TDirection(P2MoveButton-1));

        // Остальные клавиши:
          if e_KeyBuffer[KeyJump] = $080 then gPlayer2.PressKey(KEY_JUMP);
          if e_KeyBuffer[KeyUp] = $080 then gPlayer2.PressKey(KEY_UP);
          if e_KeyBuffer[KeyDown] = $080 then gPlayer2.PressKey(KEY_DOWN);
          if e_KeyBuffer[KeyFire] = $080 then gPlayer2.PressKey(KEY_FIRE);
          if e_KeyBuffer[KeyNextWeapon] = $080 then gPlayer2.PressKey(KEY_NEXTWEAPON);
          if e_KeyBuffer[KeyPrevWeapon] = $080 then gPlayer2.PressKey(KEY_PREVWEAPON);
          if e_KeyBuffer[KeyOpen] = $080 then gPlayer2.PressKey(KEY_OPEN);
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
    g_Player_UpdateCorpse();
  end; // if gameOn ...

// Активно окно интерфейса - передаем клавиши ему:
  if g_ActiveWindow <> nil then
  begin
    for a := 0 to 255 do
      if e_KeyBuffer[a] = $080 then
      begin
        Msg.Msg := MESSAGE_DIKEY;
        Msg.wParam := a;
        g_ActiveWindow.OnMessage(Msg);
        Break;
      end;

  // Если оно от этого не закрылось, то обновляем:
    if g_ActiveWindow <> nil then
      g_ActiveWindow.Update();

  // Нужно сменить разрешение:
    if gResolutionChange then
    begin
      e_WriteLog('Change resolution', MSG_NOTIFY);
      g_Game_ChangeResolution(gRC_Width, gRC_Height, gRC_FullScreen, gRC_Maximized);
      gResolutionChange := False;
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
  if e_KeyBuffer[gGameControls.GameControls.TakeScreenshot] = $080 then
    if (GetTimer()-LastScreenShot) > 200000 then
    begin
      g_TakeScreenShot();
      LastScreenShot := GetTimer();
    end;

// Каждую секунду обновляем счетчик обновлений:
  Time := GetTimer() div 1000;
  UPSCounter := UPSCounter + 1;
  if Time - UPSTime >= 1000 then
  begin
    UPS := UPSCounter;
    UPSCounter := 0;
    UPSTime := Time;
  end;
end;

procedure g_Game_LoadData();
begin
 if DataLoaded then Exit;

 g_Game_SetLoadingText(_lc[I_LOAD_ITEMS_DATA], 0, False);
 g_Items_LoadData();

 g_Game_SetLoadingText(_lc[I_LOAD_WEAPONS_DATA], 0, False);
 g_Weapon_LoadData();

 g_Monsters_LoadData();

 e_WriteLog('Loading game data...', MSG_NOTIFY);

 g_Game_SetLoadingText(_lc[I_LOAD_GAME_DATA], 0, False);

 g_Texture_CreateWADEx('TEXTURE_PLAYER_HUD', GameWAD+':TEXTURES\HUD');
 g_Texture_CreateWADEx('TEXTURE_PLAYER_HUDBG', GameWAD+':TEXTURES\HUDBG');
 g_Texture_CreateWADEx('TEXTURE_PLAYER_ARMORHUD', GameWAD+':TEXTURES\ARMORHUD');
 g_Texture_CreateWADEx('TEXTURE_PLAYER_REDFLAG', GameWAD+':TEXTURES\FLAGREDS');
 g_Texture_CreateWADEx('TEXTURE_PLAYER_BLUEFLAG', GameWAD+':TEXTURES\FLAGBLUES');
 g_Frames_CreateWAD(nil, 'FRAMES_TELEPORT', GameWAD+':TEXTURES\TELEPORT', 48, 64, 10, False);
 g_Sound_CreateWADEx('SOUND_GAME_TELEPORT', GameWAD+':SOUNDS\TELEPORT');
 g_Sound_CreateWADEx('SOUND_GAME_DOOROPEN', GameWAD+':SOUNDS\DOOROPEN');
 g_Sound_CreateWADEx('SOUND_GAME_DOORCLOSE', GameWAD+':SOUNDS\DOORCLOSE');
 g_Sound_CreateWADEx('SOUND_GAME_BULK1', GameWAD+':SOUNDS\BULK1');
 g_Sound_CreateWADEx('SOUND_GAME_BULK2', GameWAD+':SOUNDS\BULK2');
 g_Sound_CreateWADEx('SOUND_GAME_BUBBLES', GameWAD+':SOUNDS\BUBBLES');
 g_Sound_CreateWADEx('SOUND_GAME_SWITCH1', GameWAD+':SOUNDS\SWITCH1');
 g_Sound_CreateWADEx('SOUND_GAME_SWITCH0', GameWAD+':SOUNDS\SWITCH0');
 g_Sound_CreateWADEx('SOUND_GAME_RADIO', GameWAD+':SOUNDS\RADIO');

 DataLoaded := True;
end;

procedure g_Game_FreeData();
begin
 if not DataLoaded then Exit;

 g_Items_FreeData();
 g_Weapon_FreeData();
 g_Monsters_FreeData();

 e_WriteLog('Releasing game data...', MSG_NOTIFY);

 g_Texture_Delete('TEXTURE_PLAYER_HUD');
 g_Texture_Delete('TEXTURE_PLAYER_HUDBG');
 g_Texture_Delete('TEXTURE_PLAYER_ARMORHUD');
 g_Texture_Delete('TEXTURE_PLAYER_REDFLAG');
 g_Texture_Delete('TEXTURE_PLAYER_BLUEFLAG');
 g_Frames_DeleteByName('FRAMES_TELEPORT');
 g_Sound_Delete('SOUND_GAME_TELEPORT');
 g_Sound_Delete('SOUND_GAME_DOOROPEN');
 g_Sound_Delete('SOUND_GAME_DOORCLOSE');
 g_Sound_Delete('SOUND_GAME_BULK1');
 g_Sound_Delete('SOUND_GAME_BULK2');
 g_Sound_Delete('SOUND_GAME_BUBBLES');
 g_Sound_Delete('SOUND_GAME_SWITCH1');
 g_Sound_Delete('SOUND_GAME_SWITCH0');

 DataLoaded := False;
end;

procedure DrawCustomStat();
var
  pc, x, y, w, _y,
  w1, w2, w3,
  t, p, m: Integer;
  ww1, hh1: Word;
  ww2, hh2, r, g, b: Byte;
  s1: String;

begin
 pc := Length(CustomStat.PlayerStat);

 if pc = 0 then Exit;

 e_CharFont_GetSize(gMenuFont, _lc[I_MENU_INTER1], ww1, hh1);
 e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(ww1 div 2), 16, _lc[I_MENU_INTER1]);

 e_TextureFontGetSize(gStdFont, ww2, hh2);

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
  GM_DM: s1 := _lc[I_GAME_DM];
  GM_TDM: s1 := _lc[I_GAME_TDM];
  GM_CTF: s1 := _lc[I_GAME_CTF];
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
 e_TextureFontPrint(x+8+m, _y, Format('%.2d:%.2d', [CustomStat.GameTime div 1000 div 60,
                                                    CustomStat.GameTime div 1000 mod 60]), gStdFont);

 if CustomStat.GameMode in [GM_CTF, GM_TDM] then
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
    e_TextureFontPrintEx(x+8, _y, Format(_lc[I_GAME_TEAM_RED], [CustomStat.TeamStat[TEAM_RED].Goals]),
                         gStdFont, 255, 0, 0, 1);
    r := 255;
    g := 0;
    b := 0;
   end
    else
   begin
    e_TextureFontPrintEx(x+8, _y, Format(_lc[I_GAME_TEAM_BLUE], [CustomStat.TeamStat[TEAM_BLUE].Goals]),
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
     e_TextureFontPrintEx(x+8, _y, Name, gStdFont, r, g, b, 1);
     e_TextureFontPrintEx(x+w1+8, _y, IntToStr(Frags), gStdFont, r, g, b, 1);
     e_TextureFontPrintEx(x+w1+w2+8, _y, IntToStr(Deaths), gStdFont, r, g, b, 1);
     _y := _y+24;
    end;

   _y := _y+16+16;
  end;
 end
  else if CustomStat.GameMode in [GM_DM] then
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

   e_TextureFontPrint(x+8+16+8, _y+4, Name, gStdFont);
   e_TextureFontPrint(x+w1+8+16+8, _y+4, IntToStr(Frags), gStdFont);
   e_TextureFontPrint(x+w1+w2+8+16+8, _y+4, IntToStr(Deaths), gStdFont);
   _y := _y+24;
  end;
 end;
end;

procedure DrawSingleStat();
var
  x, y, t: Integer;
  ww1, hh1, ww2, hh2: Word;
  s1, s2, s3, s4: string;

 procedure player_stat(n: Integer);
 begin
  s1 := _lc[I_MENU_INTER5];
  s2 := Format(' %.1f', [(SingleStat.PlayerStat[n].Kills/t)*60]);
  e_CharFont_GetSize(gMenuFont, s1, ww1, hh1);
  e_CharFont_GetSize(gMenuFont, s2, ww2, hh2);

  x := (gScreenWidth div 2)-((ww1+ww2) div 2);
  e_CharFont_Print(gMenuFont, x, y+32, s1);
  e_CharFont_PrintEx(gMenuFont, x+ww1, y+32, s2, _RGB(255, 0, 0));

  s1 := _lc[I_MENU_INTER4];
  s2 := Format(' %d', [SingleStat.PlayerStat[n].Kills]);
  e_CharFont_Print(gMenuFont, x, y, s1);
  e_CharFont_PrintEx(gMenuFont, x+ww1, y, s2, _RGB(255, 0, 0));

  s1 := _lc[I_MENU_INTER6];
  s2 := _lc[I_MENU_INTER7];
  s3 := Format(' %d ', [SingleStat.PlayerStat[n].Secrets]);
  s4 := Format(' %d', [SingleStat.TotalSecrets]);

  e_CharFont_Print(gMenuFont, x, y+64, s1);
  e_CharFont_GetSize(gMenuFont, s1, ww1, hh1);
  e_CharFont_PrintEx(gMenuFont, x+ww1, y+64, s3, _RGB(255, 0, 0));
  e_CharFont_GetSize(gMenuFont, s1+s3, ww1, hh1);
  e_CharFont_Print(gMenuFont, x+ww1, y+64, s2);
  e_CharFont_GetSize(gMenuFont, s1+s3+s2, ww1, hh1);
  e_CharFont_PrintEx(gMenuFont, x+ww1, y+64, s4, _RGB(255, 0, 0));
 end;

begin
 e_CharFont_GetSize(gMenuFont, _lc[I_MENU_INTER2], ww1, hh1);
 e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(ww1 div 2), 32, _lc[I_MENU_INTER2]);

 t := SingleStat.GameTime div 1000;
 s1 := _lc[I_MENU_INTER3];
 s2 := Format(' %d:%.2d:%.2d', [t div (60*60), (t mod (60*60)) div 60, t mod 60]);

 e_CharFont_GetSize(gMenuFont, s1, ww1, hh1);
 e_CharFont_GetSize(gMenuFont, s2, ww2, hh2);
 e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-((ww1+ww2) div 2), 80, s1);
 e_CharFont_PrintEx(gMenuFont, (gScreenWidth div 2)-((ww1+ww2) div 2)+ww1, 80, s2, _RGB(255, 0, 0));

 if SingleStat.TwoPlayers then
 begin
  s1 := _lc[I_MENU_PLAYER_1];
  e_CharFont_GetSize(gMenuFont, s1, ww1, hh1);
  e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(ww1 div 2), 128, s1);
  y := 176;
  player_stat(0);

  s1 := _lc[I_MENU_PLAYER_2];
  e_CharFont_GetSize(gMenuFont, s1, ww1, hh1);
  e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(ww1 div 2), 288, s1);
  y := 336;
  player_stat(1);
 end
  else
 begin
  y := 128;
  player_stat(0);
 end;
end;

procedure DrawLoadingStat();
var
  ww, hh: Word;
  xx, yy, i: Integer;
  s: String;

begin
  if Length(LoadingStat.Msgs) = 0 then
    Exit;

  e_CharFont_GetSize(gMenuFont, _lc[I_MENU_LOADING], ww, hh);
  yy := (gScreenHeight div 3);
  e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(ww div 2), yy-2*hh, _lc[I_MENU_LOADING]);
  xx := (gScreenWidth div 3);

  with LoadingStat do
    for i := 0 to NextMsg-1 do
      begin
        if (i = (NextMsg-1)) and (MaxValue > 0) then
          s := Format('%s:  %d/%d', [Msgs[i], CurValue, MaxValue])
        else
          s := Msgs[i];

        e_CharFont_PrintEx(gMenuSmallFont, xx, yy, s, _RGB(255, 0, 0));
        yy := yy + LOADING_INTERLINE;
      end;
end;

procedure DrawPlayer(p: TPlayer);
var
 px, py, a, b, c, d: Integer;
begin
 glPushMatrix;
 px := p.GameX+PLAYER_RECT_CX;
 py := p.GameY+PLAYER_RECT_CY;
 if px > (gPlayerScreenSize.X div 2) then a := -px+(gPlayerScreenSize.X div 2) else a := 0;
 if py > (gPlayerScreenSize.Y div 2) then b := -py+(gPlayerScreenSize.Y div 2) else b := 0;
 if px > gMapInfo.Width-(gPlayerScreenSize.X div 2) then a := -gMapInfo.Width+gPlayerScreenSize.X;
 if py > gMapInfo.Height-(gPlayerScreenSize.Y div 2) then b := -gMapInfo.Height+gPlayerScreenSize.Y;
 if gMapInfo.Width <= gPlayerScreenSize.X then a := 0;
 if gMapInfo.Height <= gPlayerScreenSize.Y then b := 0;

 if p.IncCam <> 0 then
 begin
  if py > gMapInfo.Height-(gPlayerScreenSize.Y div 2) then
  begin
   if p.IncCam > 120-(py-(gMapInfo.Height-(gPlayerScreenSize.Y div 2))) then
    p.IncCam := 120-(py-(gMapInfo.Height-(gPlayerScreenSize.Y div 2)));
  end;

  if py < (gPlayerScreenSize.Y div 2) then
  begin
   if p.IncCam < -120+((gPlayerScreenSize.Y div 2)-py) then
    p.IncCam := -120+((gPlayerScreenSize.Y div 2)-py);
  end;

  if p.IncCam < 0 then
   while (py+(gPlayerScreenSize.Y div 2)-p.IncCam > gMapInfo.Height) and
         (p.IncCam < 0) do p.IncCam := p.IncCam+1;

  if p.IncCam > 0 then
   while (py-(gPlayerScreenSize.Y div 2)-p.IncCam < 0) and
         (p.IncCam > 0) do p.IncCam := p.IncCam-1;
 end;

 if ((px< gPlayerScreenSize.X div 2) or (gMapInfo.Width-gPlayerScreenSize.X <= 256)) then c := 0
  else if (px > gMapInfo.Width-(gPlayerScreenSize.X div 2)) then c := gBackSize.X-gPlayerScreenSize.X
   else c := Round((px-(gPlayerScreenSize.X div 2))/(gMapInfo.Width-gPlayerScreenSize.X)*(gBackSize.X-gPlayerScreenSize.X));

 if (py-p.IncCam <= gPlayerScreenSize.Y div 2) or (gMapInfo.Height-gPlayerScreenSize.Y <= 256) then d := 0
  else if (py-p.IncCam >= gMapInfo.Height-(gPlayerScreenSize.Y div 2)) then d := gBackSize.Y-gPlayerScreenSize.Y
   else d := Round((py-p.IncCam-(gPlayerScreenSize.Y div 2))/(gMapInfo.Height-gPlayerScreenSize.Y)*(gBackSize.Y-gPlayerScreenSize.Y));

 g_Map_DrawBack(-c, -d);

 sX := -a;
 sY := -(b+p.IncCam);
 sWidth := gPlayerScreenSize.X;
 sHeight := gPlayerScreenSize.Y;

 glTranslatef(a, b+p.IncCam, 0);
 g_Map_DrawPanels(PANEL_BACK);
 g_Map_DrawPanels(PANEL_STEP);
 g_Items_Draw();
 g_Weapon_Draw();
 g_Player_DrawAll();
 g_Player_DrawCorpses();
 g_Map_DrawPanels(PANEL_WALL);
 g_Monsters_Draw();
 g_Map_DrawPanels(PANEL_CLOSEDOOR);
 g_GFX_Draw();
 g_Map_DrawFlags();
 g_Map_DrawPanels(PANEL_ACID1);
 g_Map_DrawPanels(PANEL_ACID2);
 g_Map_DrawPanels(PANEL_WATER);
 g_Map_DrawPanels(PANEL_FORE);
 glPopMatrix();

 p.DrawPain();
 p.DrawInv();
 p.DrawGUI();
end;

procedure g_Game_Draw();
var
  ID: DWORD;
  w, h: Word;
  Time: Int64;
  back: string;

begin
 if gExit = EXIT_QUIT then Exit;

 Time := GetTimer() div 1000;
 FPSCounter := FPSCounter+1;
 if Time - FPSTime >= 1000 then
 begin
  FPS := FPSCounter;
  FPSCounter := 0;
  FPSTime := Time;
 end;
 
 if gGameOn or (gState = STATE_FOLD) then
 begin
  if LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER) then
   e_SetViewPort(0, gPlayerScreenSize.Y+1, gPlayerScreenSize.X+196, gPlayerScreenSize.Y);

  if LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER) and gRevertPlayers then
  begin
   DrawPlayer(gPlayer2);
   gPlayer2ScreenCoord.X := sX;
   gPlayer2ScreenCoord.Y := sY;
  end
   else
  begin
   DrawPlayer(gPlayer1);
   gPlayer1ScreenCoord.X := sX;
   gPlayer1ScreenCoord.Y := sY;
  end;
  
  if LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER) then
  begin
   e_SetViewPort(0, 0, gPlayerScreenSize.X+196, gPlayerScreenSize.Y);

   if gRevertPlayers then
   begin
    DrawPlayer(gPlayer1);
    gPlayer1ScreenCoord.X := sX;
    gPlayer1ScreenCoord.Y := sY;
   end
    else
   begin
    DrawPlayer(gPlayer2);
    gPlayer2ScreenCoord.X := sX;
    gPlayer2ScreenCoord.Y := sY;
   end;
  end;

  e_SetViewPort(0, 0, gScreenWidth, gScreenHeight);

  if LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER) then
   e_DrawLine(2, 0, gScreenHeight div 2, gScreenWidth, gScreenHeight div 2, 0, 0, 0);

  if MessageText <> '' then
  begin
   e_CharFont_GetSize(gMenuFont, MessageText, w, h);
   e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(w div 2),
                    (gScreenHeight div 2)-(h div 2), MessageText);
  end;

  if IsDrawStat then DrawStat();
 end;

 if gPause and gGameOn and (g_ActiveWindow = nil) then
 begin
  e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 0, 0, 0, 180);

  e_CharFont_GetSize(gMenuFont, _lc[I_MENU_PAUSE], w, h);
  e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(w div 2),
                   (gScreenHeight div 2)-(h div 2), _lc[I_MENU_PAUSE]);
 end;

 if not gGameOn then
 begin
  if (gState = STATE_MENU) and
     ((g_ActiveWindow = nil) or (g_ActiveWindow.BackTexture = '')) then
   if g_Texture_Get('MENU_BACKGROUND', ID) then
    e_DrawSize(ID, 0, 0, 0, False, False, gScreenWidth, gScreenHeight)
     else e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);

  if gState = STATE_FOLD then
   e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 0, 0, 0, EndingGameCounter);

  if gState = STATE_INTERCUSTOM then
  begin
   back := 'INTER';

   if g_Texture_Get(back, ID) then
    e_DrawSize(ID, 0, 0, 0, False, False, gScreenWidth, gScreenHeight)
     else e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);

   DrawCustomStat();
  end;

  if gState = STATE_INTERSINGLE then
  begin
   if EndingGameCounter > 0 then
    e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 0, 0, 0, EndingGameCounter)
    else
   begin
    back := 'INTER';

    if g_Texture_Get(back, ID) then
     e_DrawSize(ID, 0, 0, 0, False, False, gScreenWidth, gScreenHeight)
      else e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);

    DrawSingleStat();
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
  end;
 end;

 if g_ActiveWindow <> nil then
   begin
     if gGameOn then
       e_DrawFillQuad(0, 0, gScreenWidth-1, gScreenHeight-1, 0, 0, 0, 180);
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

 if gGameOn and gShowTime and (gGameSettings.GameType = GT_CUSTOM) then
  e_TextureFontPrint(gScreenWidth-64, 0,
                     Format('%.2d:%.2d', [gTime div 1000 div 60, gTime div 1000 mod 60]),
                     gStdFont);
end;

procedure g_Game_Quit();
begin
  g_Game_StopAllSounds();
  gMusic.Free();
  g_Game_SaveOptions();
  g_Game_FreeData();
  g_PlayerModel_FreeData();
  g_Texture_DeleteAll();
  g_Frames_DeleteAll();
  g_Menu_Free();

// Надо удалить карту после теста:
  if gMapToDelete <> '' then
    g_Game_DeleteTestMap();

  gExit := EXIT_QUIT;
  PostQuitMessage(0);
end;

procedure g_FatalError(Text: String);
begin
  g_Console_Add(Format(_lc[I_FATAL_ERROR], [Text]), True);
  e_WriteLog(Format(_lc[I_FATAL_ERROR], [Text]), MSG_WARNING);

  gExit := EXIT_SIMPLE;
end;

procedure g_Game_SetupScreenSize();
var
  d: Single;

begin
// Размер экранов игроков:
  gPlayerScreenSize.X := gScreenWidth-196;
  if LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER) then
    gPlayerScreenSize.Y := gScreenHeight div 2
  else
    gPlayerScreenSize.Y := gScreenHeight;
    
// Размер заднего плана:
  if BackID <> DWORD(-1) then
  begin
    d := SKY_STRETCH;

    if (gScreenWidth*d > gMapInfo.Width) or
       (gScreenHeight*d > gMapInfo.Height) then
      d := 1.0;

    gBackSize.X := Round(gScreenWidth*d);
    gBackSize.Y := Round(gScreenHeight*d);
  end;
end;

procedure g_Game_ChangeResolution(newWidth, newHeight: Word; nowFull, nowMax: Boolean);
var
  sz: TPoint;

begin
// Если окно было развернуто на весь экран, то восстанавливаем:
  if gWinMaximized then
  begin
    if not gFullscreen then
    begin // Был оконный режим
      ShowWindow(h_Wnd, SW_RESTORE);
    end;

    gWinMaximized := False;
  end;

// Меняем режим: оконный или полноэкранный:
  if gFullscreen <> nowFull then
  begin
    gFullscreen := nowFull;

    if not gFullscreen then
      begin // Включить оконный:
      // Выключить полноэкранный:
        SetForegroundWindow(0);
        ChangeDisplaySettings(_devicemodeA(nil^), CDS_FULLSCREEN);
      // Стиль окна:
        SetWindowLong(h_Wnd, GWL_STYLE, Integer(gWindowStyle[1][1]));
        SetWindowLong(h_Wnd, GWL_EXSTYLE, Integer(gWindowStyle[1][2]));
      end
    else
      begin // Включить полноэкранный
      // Стиль окна:
        SetWindowLong(h_Wnd, GWL_STYLE, Integer(gWindowStyle[2][1]));
        SetWindowLong(h_Wnd, GWL_EXSTYLE, Integer(gWindowStyle[2][2]));
        SetForegroundWindow(h_Wnd);
      end;
  end;

// Вызываем WM_SIZE с новыми размерами:
  if gFullScreen then
    begin // Полноэкранный
      g_Window_SetDisplay();

      SetWindowPos(h_Wnd, HWND_TOP,
                   0, 0, newWidth, newHeight,
                   SWP_SHOWWINDOW);
    end
  else // Оконный
    begin
    // Размер окна:
      sz.X := newWidth + 2*gWinFrameX;
      sz.Y := newHeight + 2*gWinFrameY + gWinCaption;

      gWinMaximized := nowMax;
      
      SetWindowPos(h_Wnd, HWND_TOP,
                   gWinRealPosX, gWinRealPosY, sz.X, sz.Y,
                   SWP_SHOWWINDOW);

      if gWinMaximized then
        ShowWindow(h_Wnd, SW_SHOWMAXIMIZED);
    end;
end;

procedure g_Game_StartSingle(WAD, MAP: String; TwoPlayers: Boolean; nPlayers: Byte);
var
  i, nPl: Integer;

begin
  g_Game_Free();

  e_WriteLog('Starting single game...', MSG_NOTIFY);

  g_Game_ClearLoading();
  g_Game_LoadData();

// Настройки игры:
  ZeroMemory(@gGameSettings, SizeOf(TGameSettings));
  gGameSettings.GameType := GT_SINGLE;
  if TwoPlayers then
    gGameSettings.Options := GAME_OPTION_TWOPLAYER;
  gGameSettings.Options := gGameSettings.Options + GAME_OPTION_ALLOWEXIT;
  gGameSettings.Options := gGameSettings.Options + GAME_OPTION_MONSTERDM;
  gGameSettings.Options := gGameSettings.Options + GAME_OPTION_BOTVSMONSTER;
  gGameSettings.WAD := WAD;

// Загружаем МегаВАД:
  LoadMegaWAD(WAD);

// Установка размеров окон игроков:
  g_Game_SetupScreenSize();

// Создание первого игрока:
  gPlayer1 := g_Player_Get(g_Player_Create(STD_PLAYER_MODEL,
                                           PLAYER1_DEF_COLOR,
                                           TEAM_RED, False,
                                           PLAYERNUM_1));
  if gPlayer1 = nil then
  begin
    g_FatalError(Format(_lc[I_GAME_ERROR_PLAYER_CREATE], [1]));
    Exit;
  end;

  gPlayer1.Name := _lc[I_MENU_PLAYER_1];
  nPl := 1;

// Создание второго игрока, если есть:
  if TwoPlayers then
  begin
    gPlayer2 := g_Player_Get(g_Player_Create(STD_PLAYER_MODEL,
                                             PLAYER2_DEF_COLOR,
                                             TEAM_RED, False,
                                             PLAYERNUM_2));
    if gPlayer2 = nil then
    begin
      g_FatalError(Format(_lc[I_GAME_ERROR_PLAYER_CREATE], [2]));
      Exit;
    end;

    gPlayer2.Name := _lc[I_MENU_PLAYER_2];
    Inc(nPl);
  end;

// Загрузка и запуск карты:
  if not g_Game_StartMap(MAP, True) then
  begin
    g_FatalError(Format(_lc[I_GAME_ERROR_MAP_LOAD],
                        [ExtractFileName(gGameSettings.WAD) + ':\' + MAP]));
    Exit;
  end;

// Настройки игроков и ботов:
  g_Player_Init();

// Создаем ботов:
  for i := nPl+1 to nPlayers do
  begin
    g_Player_Create(STD_PLAYER_MODEL, _RGB(0, 0, 0), 0, True, 0);
  end;
end;

procedure g_Game_StartCustom(Map: String; GameMode: Byte;
                             TimeLimit, GoalLimit: Word;
                             Options: LongWord; nPlayers: Byte);
var
  ResName: String;
  Team: Byte;
  i, nPl: Integer;

begin
  g_Game_Free();

  e_WriteLog('Starting custom game...', MSG_NOTIFY);

  g_Game_ClearLoading();
  g_Game_LoadData();

// Настройки игры:
  gGameSettings.GameType := GT_CUSTOM;
  gGameSettings.GameMode := GameMode;
  gGameSettings.TimeLimit := TimeLimit;
  gGameSettings.GoalLimit := GoalLimit;
  gGameSettings.Options := Options;
  g_ProcessResourceStr(Map, @gGameSettings.WAD, nil, @ResName);

// Установка размеров окон игроков:
  g_Game_SetupScreenSize();

// Режим кооператива (доступен в отладочном режиме):
  if gGameSettings.GameMode = GM_COOP then
  begin
    gGameSettings.GameType := GT_SINGLE;
  end;

// Создание первого игрока:
  if (gGameSettings.GameType = GT_SINGLE) then
    begin
      gPlayer1 := g_Player_Get(g_Player_Create(STD_PLAYER_MODEL,
                                               PLAYER1_DEF_COLOR,
                                               TEAM_RED, False,
                                               PLAYERNUM_1));
    end
  else
    begin
      if GameMode = GM_DM then
        Team := TEAM_NONE
      else
        Team := gPlayer1Settings.Team;

      gPlayer1 := g_Player_Get(g_Player_Create(gPlayer1Settings.Model,
                                               gPlayer1Settings.Color,
                                               Team, False,
                                               PLAYERNUM_1));
    end;

  if gPlayer1 = nil then
  begin
    g_FatalError(Format(_lc[I_GAME_ERROR_PLAYER_CREATE], [1]));
    Exit;
  end;

  gPlayer1.Name := gPlayer1Settings.Name;
  nPl := 1;

// Создание второго игрока, если есть:
  if LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER) then
  begin
    if (gGameSettings.GameType = GT_SINGLE) then
      begin
        gPlayer2 := g_Player_Get(g_Player_Create(STD_PLAYER_MODEL,
                                                 PLAYER2_DEF_COLOR,
                                                 TEAM_RED, False,
                                                 PLAYERNUM_2));
      end
    else
      begin
        if GameMode = GM_DM then
          Team := TEAM_NONE
        else
          Team := gPlayer2Settings.Team;

        gPlayer2 := g_Player_Get(g_Player_Create(gPlayer2Settings.Model,
                                                 gPlayer2Settings.Color,
                                                 Team, False,
                                                 PLAYERNUM_2));
      end;

    if gPlayer2 = nil then
    begin
      g_FatalError(Format(_lc[I_GAME_ERROR_PLAYER_CREATE], [2]));
      Exit;
    end;

    gPlayer2.Name := gPlayer2Settings.Name;
    Inc(nPl);
  end;

// Загрузка и запуск карты:
  if not g_Game_StartMap(ResName, True) then
  begin
    g_FatalError(Format(_lc[I_GAME_ERROR_MAP_LOAD], [ExtractFileName(gGameSettings.WAD)+':\'+ResName]));
    Exit;
  end;

// CTF, а флагов нет:
  if (GameMode = GM_CTF) and not g_Map_HaveFlagPoints() then
  begin
    g_FatalError(_lc[I_GAME_ERROR_CTF]);
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
  begin
    g_Player_Create(STD_PLAYER_MODEL, _RGB(0, 0, 0), 0, True, 0);
  end;
end;

procedure g_Game_SaveOptions();
var
  config: TConfig;
  sW, sH: Integer;

begin
  e_WriteLog('Writing resolution to config', MSG_NOTIFY);
  config := TConfig.CreateFile(GameDir+'\'+CONFIG_FILENAME);

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

  config.SaveFile(GameDir+'\'+CONFIG_FILENAME);
  config.Free();
end;

procedure g_Game_Restart();
var
  Map: string;
begin
 g_ProcessResourceStr(gMapInfo.Map, nil, nil, @Map);

 gGameOn := False;
 g_Game_ClearLoading();
 g_Game_StartMap(Map, True);
end;

function g_Game_StartMap(Map: String; Force: Boolean = False): Boolean;
begin
  g_Map_Free();
  g_Player_RemoveAllCorpses();

  Result := g_Map_Load(gGameSettings.WAD+':\'+Map);
  if Result then
    g_Player_ResetAll(Force, True);

  gExit := 0;
  gState := STATE_NONE;
  g_ActiveWindow := nil;

  gGameOn := True;
  gPause := False;
  gTime := 0;

  DisableCheats();
  ResetTimer();
end;

procedure g_Game_ExitLevel(Map: Char16);
begin
  NextMap := Map;

// Вышли в выход в Одиночной игре:
  if gGameSettings.GameType = GT_SINGLE then
    gExit := EXIT_ENDLEVELSINGLE
  else // Вышли в выход в Своей игре
    gExit := EXIT_ENDLEVELCUSTOM;
end;

procedure g_Game_NextLevel();
{var
  Map, FileName: string;}
begin
  {if gGameSettings.GameType = GT_CUSTOM then
    gExit := EXIT_ENDLEVELCUSTOM
  else
    gExit := EXIT_ENDLEVELSINGLE; }

 NextMap := '';

 { TODO 5 : Добавить переход по картам}
 { в своей игре, по таймлимиту. Список карт или MAPXX}

 Exit;

 {if not LongBool(gGameSettings.Options and GAME_OPTION_MAPCHANGE) then Exit;
 if MapList = nil then Exit;
 if (MapIndex+1 > High(MapList)) and
    not LongBool(gGameSettings.Options and GAME_OPTION_MAPCYCLE) then Exit;

 if MapIndex+1 > High(MapList) then MapIndex := -1;

 g_ProcessResourceStr(gGameSettings.Map, @FileName, nil, nil);
 Inc(MapIndex);
 Map := FileName+':\'+MapList[MapIndex];}
 
 { else
 begin
  Map := Copy(gGameSettings.Map, 1, Length(gGameSettings.Map)-2);
  if UpperCase(Copy(Map, Length(Map)-2, 3)) = 'MAP' then
  begin
   Num := Copy(gGameSettings.Map, Length(gGameSettings.Map)-1, 2);
   if StrToIntDef(Num, 100) < 99 then
   begin
    Num := IntToStr(StrToInt(Num)+1);
    if Length(Num) = 1 then Num := '0'+Num;

    Map := Map+Num;

    ok := True;
   end;
  end;
 end;}

 {if not g_Map_Exist(Map) then Exit;

 NextMap := Map;}
end;

procedure g_Game_DeleteTestMap();
var
  WAD: TWADEditor_1;
  MapName: Char16;
  MapList: SArray;
  a: Integer;
  WadName: string;

begin
  a := Pos('.wad:\', gMapToDelete);
  if a = 0 then
    Exit;

// Выделяем имя wad-файла и имя карты:
  WadName := Copy(gMapToDelete, 1, a + 3);
  Delete(gMapToDelete, 1, a + 5);
  gMapToDelete := UpperCase(gMapToDelete);
  MapName := '';
  CopyMemory(@MapName[0], @gMapToDelete[1], Min(16, Length(gMapToDelete)));

// Имя карты не стандартное тестовое:
  if MapName <> TEST_MAP_NAME then
    Exit;

  WAD := TWADEditor_1.Create();

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
end;

procedure GameCommands(P: SArray);
var
  a, b: Integer;
  s: String;
  stat: TPlayerStatArray;
  pt: TPoint;
  
begin
// Общие команды:
  if (LowerCase(P[0]) = 'quit') or
     (LowerCase(P[0]) = 'exit') then
  begin
    g_Game_Quit();
    Exit;
  end;

  if LowerCase(P[0]) = 'pause' then
    if (g_ActiveWindow = nil) then
      g_Game_Pause(not gPause);

  if LowerCase(P[0]) = 'endgame' then
   gExit := EXIT_SIMPLE;

  if LowerCase(P[0]) = 'restart' then
    if gGameOn then
      g_Game_Restart();

  if LowerCase(P[0]) = 'g_showfps' then
  begin
    if (Length(P) > 1) and
       ((P[1] = '1') or (P[1] = '0')) then
      gShowFPS := (P[1][1] = '1');

    if gShowFPS then
      g_Console_Add(_lc[I_MSG_SHOW_FPS_ON])
    else
      g_Console_Add(_lc[I_MSG_SHOW_FPS_OFF]);
  end;

  if LowerCase(P[0]) = 'ffire' then
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
    end;

  if (LowerCase(P[0]) = 'addbot') or
     (LowerCase(P[0]) = 'bot_add') then
    if Length(P) > 1 then
      g_Bot_Add(TEAM_NONE, StrToIntDef(P[1], 2))
    else
      g_Bot_Add(TEAM_NONE, 2);

  if LowerCase(P[0]) = 'bot_addlist' then
    if Length(P) > 1 then
      if Length(P) = 2 then
        g_Bot_AddList(TEAM_NONE, P[1], StrToIntDef(P[1], -1))
      else
        g_Bot_AddList(IfThen(P[2] = 'red', TEAM_RED, TEAM_BLUE), P[1], StrToIntDef(P[1], -1));

  if LowerCase(P[0]) = 'bot_removeall' then
    g_Bot_RemoveAll();

// Команды отладочного режима:
  if gDebugMode then
  begin
    if LowerCase(P[0]) = 'd_window' then
    begin
      GetCursorPos(pt);
      g_Console_Add(Format('Cursor at %d : %d', [pt.X-gWinPosX, pt.Y-gWinPosY]));
      g_Console_Add(Format('gWinPosX = %d, gWinPosY %d', [gWinPosX, gWinPosY]));
      g_Console_Add(Format('gWinRealPosX = %d, gWinRealPosY %d', [gWinRealPosX, gWinRealPosY]));
      g_Console_Add(Format('gScreenWidth = %d, gScreenHeight = %d', [gScreenWidth, gScreenHeight]));
      g_Console_Add(Format('gWinSizeX = %d, gWinSizeY = %d', [gWinSizeX, gWinSizeY]));
      g_Console_Add(Format('Frame X = %d, Y = %d, Caption Y = %d', [gWinFrameX, gWinFrameY, gWinCaption]));
    end;

    if LowerCase(P[0]) = 'd_sounds' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        g_debug_Sounds := (P[1][1] = '1');

      g_Console_Add(Format('d_sounds is %d', [Byte(g_debug_Sounds)]));
    end;
    
    if LowerCase(P[0]) = 'd_frames' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        g_debug_Frames := (P[1][1] = '1');

      g_Console_Add(Format('d_frames is %d', [Byte(g_debug_Frames)]));
    end;

    if LowerCase(P[0]) = 'd_winmsg' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        g_debug_WinMsgs := (P[1][1] = '1');

      g_Console_Add(Format('d_winmsg is %d', [Byte(g_debug_WinMsgs)]));
    end;

    if LowerCase(P[0]) = 'd_monoff' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        g_debug_MonsterOff := (P[1][1] = '1');

      g_Console_Add(Format('d_monoff is %d', [Byte(g_debug_MonsterOff)]));
    end;

    if LowerCase(P[0]) = 'map' then
      if (Length(P) > 1) then
      begin
        if Pos('.wad', LowerCase(P[1])) = 0 then
          P[1] := P[1] + '.wad';

        if Length(P) > 2 then
          s := MapsDir + P[1] + ':\' + UpperCase(P[2])
        else
          s := MapsDir + P[1] + ':\MAP01';

        if g_Map_Exist(s) then
          begin
            g_Game_Free();
            with gGameSettings do
            begin
              if LongBool(Options and GAME_OPTION_TWOPLAYER) then
                b := 2
              else
                b := 1;
              g_Game_StartCustom(s, GameMode, TimeLimit,
                                 GoalLimit, Options, b);
            end;
          end
        else
          g_Console_Add(Format(_lc[I_MSG_NO_MAP], [s]));
      end;

    if LowerCase(P[0]) = 'monster' then
      if (Length(P) > 1) and gGameOn and
         (gPlayer1 <> nil) and (gPlayer1.Live) then
      begin
        a := StrToIntDef(P[1], 0);
        if (a < MONSTER_DEMON) or (a > MONSTER_MAN) then
          a := g_Monsters_GetIDByName(P[1]);
          
        if (a < MONSTER_DEMON) or (a > MONSTER_MAN) then
          g_Console_Add(Format(_lc[I_MSG_NO_MONSTER], [P[1]]))
        else
          begin
            with gPlayer1.Obj do
              g_Monsters_Create(a,
                X + Rect.X + (Rect.Width div 2),
                Y + Rect.Y + Rect.Height - 5,
                gPlayer1.Direction, True);
          end;
      end;
  end; // gDebugMode

// Команды Своей игры:
  if gGameSettings.GameType = GT_CUSTOM then
  begin
    if LowerCase(P[0]) = 'p1_name' then
      if (Length(P) > 1) and gGameOn and (gPlayer1 <> nil) then
        gPlayer1.Name := P[1];

    if LowerCase(P[0]) = 'p2_name' then
      if (Length(P) > 1) and gGameOn and (gPlayer2 <> nil) then
        gPlayer2.Name := P[1];

    if LowerCase(P[0]) = 'g_showtime' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        gShowTime := (P[1][1] = '1');

      if gShowTime then
        g_Console_Add(_lc[I_MSG_TIME_ON])
      else
        g_Console_Add(_lc[I_MSG_TIME_OFF]);
    end;

    if LowerCase(P[0]) = 'g_showscore' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        gShowGoals := (P[1][1] = '1');

      if gShowGoals then
        g_Console_Add(_lc[I_MSG_SCORE_ON])
      else
        g_Console_Add(_lc[I_MSG_SCORE_OFF]);
    end;

    if LowerCase(P[0]) = 'g_showstat' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        gShowStat := (P[1][1] = '1');

      if gShowStat then
        g_Console_Add(_lc[I_MSG_STATS_ON])
      else
        g_Console_Add(_lc[I_MSG_STATS_OFF]);
    end;

    if LowerCase(P[0]) = 'g_showkillmsg' then
    begin
      if (Length(P) > 1) and
         ((P[1] = '1') or (P[1] = '0')) then
        gShowKillMsg := (P[1][1] = '1');

      if gShowKillMsg then
        g_Console_Add(_lc[I_MSG_KILL_MSGS_ON])
      else
        g_Console_Add(_lc[I_MSG_KILL_MSGS_OFF]);
    end;

    if LowerCase(P[0]) = 'p1_color' then
      if (gPlayer1 <> nil) and (Length(P) > 3) then
        gPlayer1.Model.SetColor(EnsureRange(StrToIntDef(P[1], 0), 0, 255),
                                EnsureRange(StrToIntDef(P[2], 0), 0, 255),
                                EnsureRange(StrToIntDef(P[3], 0), 0, 255));

    if LowerCase(P[0]) = 'p2_color' then
      if (gPlayer2 <> nil) and (Length(P) > 3) then
        gPlayer2.Model.SetColor(EnsureRange(StrToIntDef(P[1], 0), 0, 255),
                                EnsureRange(StrToIntDef(P[2], 0), 0, 255),
                                EnsureRange(StrToIntDef(P[3], 0), 0, 255));

    if LowerCase(P[0]) = 'scorelimit' then
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
      end;

      g_Console_Add(Format(_lc[I_MSG_SCORE_LIMIT], [gGameSettings.GoalLimit]));
    end;

    if LowerCase(P[0]) = 'timelimit' then
    begin
      if Length(P) > 1 then
      begin
        if StrToIntDef(P[1], gGameSettings.TimeLimit) = 0 then
          gGameSettings.TimeLimit := 0
        else
          begin
          // 10 секунд на смену:
            b := (gTime - gGameStartTime) div 1000 + 10;

            gGameSettings.TimeLimit := Max(StrToIntDef(P[1], gGameSettings.TimeLimit), b);
          end;
      end;

      g_Console_Add(Format(_lc[I_MSG_TIME_LIMIT],
                           [gGameSettings.TimeLimit div 60,
                            gGameSettings.TimeLimit mod 60]));
    end;

    if LowerCase(P[0]) = 'bot_addred' then
      if Length(P) > 1 then
        g_Bot_Add(TEAM_RED, StrToIntDef(P[1], 2))
      else
        g_Bot_Add(TEAM_RED, 2);

    if LowerCase(P[0]) = 'bot_addblue' then
      if Length(P) > 1 then
        g_Bot_Add(TEAM_BLUE, StrToIntDef(P[1], 2))
      else
        g_Bot_Add(TEAM_BLUE, 2);
  end;
end;

procedure g_TakeScreenShot();
var
  a: Word;
  FileName: String;
  
begin
 for a := 1 to High(Word) do
 begin
  FileName := Format(GameDir+'\Screenshots\Screenshot%.3d.bmp', [a]);
  if not FileExists(FileName) then
  begin
   e_MakeScreenshot(FileName, gScreenWidth, gScreenHeight);
   g_Console_Add(Format(_lc[I_CONSOLE_SCREENSHOT], [ExtractFileName(FileName)]));
   Break;
  end;
 end;
end;

procedure g_Game_InGameMenu(Show: Boolean);
begin
  if not gGameOn then
    Exit;

  if (g_ActiveWindow = nil) and Show then
    begin
      if gGameSettings.GameType = GT_SINGLE then
        g_GUI_ShowWindow('GameSingleMenu')
      else
        g_GUI_ShowWindow('GameCustomMenu');
      g_Sound_PlayEx('MENU_OPEN');

    // Пауза при меню только в одиночной игре:
      if (not gIsNetGame) then
        g_Game_Pause(True);
    end
  else
    if (g_ActiveWindow <> nil) and (not Show) then
    begin
    // Пауза при меню только в одиночной игре:
      if (not gIsNetGame) then
        g_Game_Pause(False);
    end;
end;

procedure g_Game_Pause(Enable: Boolean);
begin
  if not gGameOn then
    Exit;

  if gPause = Enable then
    Exit;

  if not (gGameSettings.GameType in [GT_SINGLE, GT_CUSTOM]) then
    Exit;

  gPause := Enable;
  g_Game_PauseAllSounds(Enable);
end;

procedure g_Game_PauseAllSounds(Enable: Boolean);
var
  i: Integer;

begin
// Триггеры:
  if gTriggers <> nil then
    for i := 0 to High(gTriggers) do
      with gTriggers[i] do
        if (TriggerType = TRIGGER_SOUND) and (Sound <> nil) then
          if Sound.IsPlaying() then
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

procedure g_Game_StopAllSounds();
var
  i: Integer;

begin
  if gTriggers <> nil then
    for i := 0 to High(gTriggers) do
      with gTriggers[i] do
        if (TriggerType = TRIGGER_SOUND) and (Sound <> nil) then
          Sound.Stop();

  if gMusic <> nil then
    gMusic.Stop();
    
  e_StopChannels();
end;

procedure g_Game_Message(Msg: string; Time: Word);
begin
 MessageText := Msg;
 MessageTime := Time;
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
var
  menu: TGUIMenu;

begin
  gDebugMode := True;
// Читы (даже в своей игре):
  gCheats := True;

// Возможность выбора COOP в Своей игре:
  menu := TGUIMenu(g_GUI_GetWindow('CustomGameMenu').GetControl('mCustomGameMenu'));
  TGUISwitch(menu.GetControl('swGameMode')).AddItem(_lc[I_MENU_GAME_TYPE_COOP]);
  menu.ReAlign();
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
    end;

    Msgs[NextMsg-1] := Text;
    CurValue := 0;
    MaxValue := Max;
    ShowCount := 0;
  end;

  g_ActiveWindow := nil;

  ProcessLoading;
end;

procedure g_Game_StepLoading();
begin
  with LoadingStat do
  begin
    Inc(CurValue);
    Inc(ShowCount);
    if (ShowCount > LOADING_SHOW_STEP) then
    begin
      ShowCount := 0;
      ProcessLoading;
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
    SetLength(Msgs, len);
    for len := 0 to High(Msgs) do
      Msgs[len] := '';
    NextMsg := 0;
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
  LimT, LimS, Opt: Integer;
  s: String;

begin
  Parse_Params(pars);

// Debug mode:
  s := Find_Param_Value(pars, '--debug');
  if (s <> '') then
  begin
    g_Game_SetDebugMode();
  end;

// Start map when game loads:
  map := Find_Param_Value(pars, '-map');
  if (map <> '') and (Pos('.wad:\', map) > 0) then
  begin
  // Game mode:
    s := Find_Param_Value(pars, '-gm');
    if s = 'tdm' then GMode := GM_TDM
    else if s = 'ctf' then GMode := GM_CTF
    else if s = 'coop' then GMode := GM_COOP
    else GMode := GM_DM;
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
  // Options:
    s := Find_Param_Value(pars, '-opt');
    if (s = '') or (not TryStrToInt(s, Opt)) then
      Opt := GAME_OPTION_ALLOWEXIT;
    if Opt < 0 then
      Opt := GAME_OPTION_ALLOWEXIT;
  // Close after map:
    s := Find_Param_Value(pars, '--close');
    if (s <> '') then
      gMapOnce := True;
  // Delete test map after play:
    s := Find_Param_Value(pars, '--testdelete');
    if (s <> '') then
      gMapToDelete := map;
  // Number of players:
    if LongBool(Opt and GAME_OPTION_TWOPLAYER) then
      n := 2
    else
      n := 1;
  // Start:
    g_Game_StartCustom(map, GMode, LimT, LimS, Opt, n);
  end;

  SetLength(pars, 0);
end;

end.
