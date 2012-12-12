unit g_game;

interface

uses windows, g_basic, g_player, Messages, e_graphics, SysUtils,
  MAPSTRUCT, WADEDITOR;

type
  TGameSettings = record
   GameType: Byte;
   GameMode: Byte;
   TimeLimit: Word;
   GoalLimit: Word;
   Options: LongWord;
   WAD: string;
  end;

  TPlayerSettings = record
   Name: string;
   Model: string;
   Color: TRGB;
   Team: Byte;
  end;

  TMegaWADInfo = record
   name: string;
   description: string;
   author: string;
   pic: string;
  end;

procedure g_Game_Init();
procedure g_Game_Free();
procedure g_Game_LoadData();
procedure g_Game_FreeData();
procedure g_Game_Update();
procedure g_Game_Draw();
procedure g_Game_Quit();
procedure g_Game_StartSingle(WAD, MAP: string; TwoPlayers: Boolean);
procedure g_Game_StartCustom(Map: string; GameMode: Byte; TimeLimit, GoalLimit: Word; Options: LongWord);
procedure g_Game_Restart();
function g_Game_StartMap(Map: string; Force: Boolean = False): Boolean;
procedure g_Game_ExitLevel(Map: Char16);
procedure g_Game_NextLevel();
procedure g_Game_InGameMenu(Show: Boolean);
procedure g_Game_Pause(Enable: Boolean);
procedure g_Game_Message(Msg: string; Time: Word);
procedure g_Game_LoadMapList(FileName: string);
procedure g_Game_PlayMusic(name: string);
procedure g_Game_StopMusic();
procedure g_Game_SetMusicVolume();
function g_Game_GetMegaWADInfo(WAD: string): TMegaWADInfo;
procedure g_TakeScreenShot();
procedure g_FatalError(Text: string);
procedure GameCommands(P: SArray);
procedure g_Game_Process_Params;
procedure g_Game_SetLoadingText(Text: String; Max: Integer);
procedure g_Game_StepLoading();
procedure DrawLoadingStat();

const
  GAME_TICK = 28;
  LOADING_SHOW_STEP = 100;
  GT_NONE   = 0;
  GT_SINGLE = 1;
  GT_CUSTOM = 2;
  GM_NONE = 0;
  GM_DM   = 1;
  GM_TDM  = 2;
  GM_CTF  = 3;
  GM_COOP = 4;
  MESSAGE_DIKEY = WM_USER+1;
  EXIT_QUIT            = 1;
  EXIT_SIMPLE          = 2;
  EXIT_RESTART         = 3;
  EXIT_ENDLEVELSINGLE  = 4;
  EXIT_ENDLEVELCUSTOM  = 5;
  EXIT_ENDGAMESINGLE   = 6;
  EXIT_ENDGAMECUSTOM   = 7;
  GAME_OPTION_TWOPLAYER  = 1;
  GAME_OPTION_TEAMDAMAGE = 2;
  GAME_OPTION_ALLOWEXIT  = 4;
  GAME_OPTION_WEAPONSTAY = 8;
  GAME_OPTION_MONSTERDM  = 16;
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

var
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
  gMusic: string = '';
  gLoadGameMode: Boolean;
  gCheats: Boolean = False;

implementation

uses g_textures, g_main, g_window, dglOpenGL, g_menu, g_gui,
     e_input, e_log, g_console, g_sound, g_items, g_map,
     inter, g_playermodel, g_gfx, g_options, g_weapons, Math, 
     g_triggers, MAPDEF, g_monsters, e_sound, CONFIG;

type
  TEndCustomGameStat = record
   PlayerStat: TPlayerStatArray;
   TeamStat: TTeamStat;
   GameTime: LongWord;
   GameMode: Byte;
   Map, MapName: string;
  end;

  TEndSingleGameStat = record
   PlayerStat: array[0..1] of record
    Kills: Integer;
    Secrets: Integer;
   end;
   GameTime: LongWord;
   TwoPlayers: Boolean;
   TotalSecrets: Integer;
  end;

  TLoadingStat = record
    Text: String;
    CurValue: Integer;
    MaxValue: Integer;
    ShowCount: Integer;
  end;

  TParamStrValue = record
    Name: String;
    Value: String;
  end;
  TParamStrValues = array of TParamStrValue;

const
  INTER_ACTION_TEXT = 1;
  INTER_ACTION_PIC = 2;
  INTER_ACTION_MUSIC = 3;

var
  FPS, UPS: Word;
  FPSCounter, UPSCounter: Word;
  FPSTime, UPSTime: LongWord;
  GameStartTime: LongWord;
  DataLoaded: Boolean;
  LastScreenShot: Int64;
  IsDrawStat: Boolean = False;
  CustomStat: TEndCustomGameStat;
  SingleStat: TEndSingleGameStat;
  LoadingStat: TLoadingStat;
  EndingGameCounter: Byte = 0;
  P1MoveButton: Byte = 0;
  P2MoveButton: Byte = 0;
  MessageText: string;
  MessageTime: Word;
  NextMap: string;
  MapList: array of string = nil;
  MapIndex: Integer = -1;
  MegaWAD: record
   info: TMegaWADInfo;
   endpic: string;
   endmus: string;
   res: record
    text: array of ShortString;
    anim: array of ShortString;
    pic: array of ShortString;
    mus: array of ShortString;
   end;
   triggers: array of record
    event: ShortString;
    actions: array of record
     action, p1, p2: Integer;
    end;
   end;
   cur_trigger: Integer;
   cur_action: Integer;
  end;
  InterPic: string;
  InterText: record
   lines: SArray;
   img: string;
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
  w.Destroy();
  Exit;
 end;

 cfg := TConfig.CreateMem(p, len);
 Result.name := cfg.ReadStr('megawad', 'name', ExtractFileName(WAD));
 Result.description := cfg.ReadStr('megawad', 'description', '');
 Result.author := cfg.ReadStr('megawad', 'author', '');
 Result.pic := cfg.ReadStr('megawad', 'pic', '');
 cfg.Destroy();

 FreeMem(p);
end;

procedure FreeMegaWAD();
var
  a: Integer;
begin
 for a := 0 to High(MegaWAD.res.pic) do
  if MegaWAD.res.pic[a] <> '' then g_Texture_Delete(MegaWAD.res.pic[a]);

 for a := 0 to High(MegaWAD.res.mus) do
  if MegaWAD.res.mus[a] <> '' then g_Music_Delete(MegaWAD.res.mus[a]);

 MegaWAD.res.pic := nil;
 MegaWAD.res.text := nil;
 MegaWAD.res.anim := nil;
 MegaWAD.res.mus := nil;
 MegaWAD.triggers := nil;

 g_Texture_Delete('endpic');
 g_Music_Delete('endmus');

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
  w.Destroy();
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
  g_Texture_CreateWADEx('endpic', s+MegaWAD.endpic);
 end;
 MegaWAD.endmus := cfg.ReadStr('megawad', 'endmus', 'Standart.wad:D2DMUS\КОНЕЦ');
 if MegaWAD.endmus <> '' then
 begin
  g_ProcessResourceStr(MegaWAD.endmus, @s, nil, nil);
  if s = '' then s := WAD else s := GameDir+'\wads\';
  g_Music_CreateWADEx('endmus', s+MegaWAD.endmus);
 end;

 cfg.Destroy();
 FreeMem(p);
 w.Destroy();
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
 gPause := False;
 gGameOn := False;

 MessageTime := 0;
 MessageText := '';

 g_Game_StopMusic();

 case gExit of
  EXIT_SIMPLE:
  begin
   EndingGameCounter := 0;
   g_Game_Free();
   g_GUI_ShowWindow('MainMenu');
   g_Game_PlayMusic('MENU');
   gState := STATE_MENU;
  end;

  EXIT_RESTART:
  begin
   EndingGameCounter := 0;
   g_Game_Restart();
  end;

  EXIT_ENDLEVELCUSTOM:
  begin
   //g_Console_Add('Custom Уровень пройден');

   g_ProcessResourceStr(gMapInfo.Map, FileName, SectionName, ResName);

   CustomStat.GameTime := gTime;
   CustomStat.Map := ExtractFileName(FileName)+':'+ResName;
   CustomStat.MapName := gMapInfo.Name;
   CustomStat.GameMode := gGameSettings.GameMode;
   if gGameSettings.GameMode in [GM_CTF, GM_TDM] then
    CustomStat.TeamStat := gTeamStat;

   CustomStat.PlayerStat := nil;

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

   EndingGameCounter := 255;
   g_ActiveWindow := nil;   
   gState := STATE_FOLD;
  end;

  EXIT_ENDLEVELSINGLE:
  begin
   //g_Console_Add('Single Уровень пройден');
   g_Game_PlayMusic('INTERMUS');

   SingleStat.GameTime := gTime;
   SingleStat.TwoPlayers := LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER);
   SingleStat.TotalSecrets := gSecretsCount;

   SingleStat.PlayerStat[0].Kills := gPlayer1.MonsterKills;
   SingleStat.PlayerStat[0].Secrets := gPlayer1.Secrets;
   if SingleStat.TwoPlayers then
   begin
    SingleStat.PlayerStat[1].Kills := gPlayer2.MonsterKills;
    SingleStat.PlayerStat[1].Secrets := gPlayer2.Secrets;
   end;

   g_ActiveWindow := nil;
   gState := STATE_INTERSINGLE  ;
  end;

  EXIT_ENDGAMECUSTOM:
  begin
   //g_Console_Add('Custom Игра закончена');
   g_Game_Free();
   g_GUI_ShowWindow('MainMenu');
   g_Game_PlayMusic('MENU');
   gState := STATE_MENU;
  end;

  EXIT_ENDGAMESINGLE:
  begin
   //g_Console_Add('Single Игра закончена');
   EndingGameCounter := 255;
   gState := STATE_FOLD;
  end;
 end;

 gExit := 0;
end;

procedure DrawStat();
var
  pc, x, y, w, h: Integer;
  w1, w2, w3: Integer;
  a, aa: Integer;
  cw, ch, r, g, b: Byte;
  s1, s2: string;
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

 e_DrawFillQuad(x, y, x+w, y+h, 64, 64, 64, 32);
 e_DrawQuad(x, y, x+w, y+h, 1, 255, 127, 0);

 stat := g_Player_GetStats();
 SortGameStat(stat); 

 case gGameSettings.GameMode of
  GM_DM:
   begin
    s1 := I_GAME_DM;
    s2 := Format(I_GAME_FRAGLIMIT, [gGameSettings.GoalLimit]);
   end;

  GM_TDM:
   begin
    s1 := I_GAME_TDM;
    s2 := Format(I_GAME_FRAGLIMIT, [gGameSettings.GoalLimit]);
   end;

  GM_CTF:
   begin
    s1 := I_GAME_CTF;
    s2 := Format(I_GAME_POINTLIMIT, [gGameSettings.GoalLimit]);
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
 s1 := I_GAME_TIMELIMIT;
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
    s1 := I_GAME_REDTEAM;
    r := 255;
    g := 0;
    b := 0;
   end
    else
   begin
    s1 := I_GAME_BLUETEAM;
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
  e_TextureFontPrintEx(x+16, _y, I_GAME_PLAYERNAME, gStdFont, 255, 127, 0, 1);
  e_TextureFontPrintEx(x+16+w1, _y, I_GAME_FRAGS, gStdFont, 255, 127, 0, 1);
  e_TextureFontPrintEx(x+16+w1+w2, _y, I_GAME_DEATHS, gStdFont, 255, 127, 0, 1);

  _y := _y+ch+8;
  for aa := 0 to High(stat) do
   with stat[aa] do
   begin
    e_DrawFillQuad(x+16, _y+4, x+32, _y+16+4, Color.R, Color.G, Color.B, 0);
    e_DrawQuad(x+16, _y+4, x+32, _y+16+4, 1, 192, 192, 192);
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

 g_Texture_CreateWADEx('MENU_BACKGROUND', GameWAD+':TEXTURES\TITLE');
 g_Texture_CreateWADEx('INTER', GameWAD+':TEXTURES\INTER');
 g_Texture_CreateWADEx('FONT_STD', GameWAD+':FONTS\STDFONT');
 if g_Texture_Get('FONT_STD', ID) then
   e_TextureFontBuild(ID, gStdFont, 16, 16, -6);

 LoadFont('MENUTXT', 'MENUFONT', CHRWDT1, CHRHGT1, FNTSPC1, gMenuFont);
 LoadFont('SMALLTXT', 'SMALLFONT', CHRWDT2, CHRHGT2, FNTSPC2, gMenuSmallFont);

 g_Game_SetLoadingText('Music', 0);
 g_Music_CreateWADEx('INTERMUS', GameWAD+':MUSIC\INTERMUS');
 g_Music_CreateWADEx('MENU', GameWAD+':MUSIC\MENU');
 g_Sound_CreateWADEx('INTER', GameWAD+':MUSIC\INTER');

 g_Game_SetLoadingText('Models', 0);
 g_PlayerModel_LoadData();
 
 if FindFirst(ModelsDir+'*.wad', faAnyFile, SR) = 0 then
 repeat
  if not g_PlayerModel_Load(ModelsDir+SR.Name) then
   e_WriteLog(Format('Error loading model %s', [SR.Name]), MSG_WARNING);
 until FindNext(SR) <> 0;
 FindClose(SR);

 g_Game_SetLoadingText('Menus', 0);
 g_Menu_Init();

 g_Game_SetLoadingText('Console', 0);
 g_Console_Init();

 DataLoaded := False;

 gGameOn := False;
 gPause := False;
 gTime := 0;
 LastScreenShot := 0;

 e_MouseInfo.Accel := 1.0;

 g_Game_PlayMusic('MENU');
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
 if gExit = EXIT_QUIT then Exit;
 if gExit <> 0 then EndGame();

 e_PollKeyboard();
 g_Console_Update();

 if gState = STATE_INTERCUSTOM then
 begin
  if ((e_KeyBuffer[28] = $080) or (e_KeyBuffer[57] = $080))
     and not gConsoleShow then
  begin
   g_Sound_All_Stop();
   g_Game_StopMusic();

   if NextMap <> '' then
   begin
    gExit := 0;
    g_Game_StartMap(NextMap);
   end
    else
   begin
    //gState := STATE_ENDGAMECUSTOM;
    gExit := EXIT_SIMPLE;
   end;

   Exit;
  end;
 end;

 if gState = STATE_FOLD then
 begin
  if EndingGameCounter = 0 then
  begin
   if NextMap = '' then g_Game_Free();
   if gGameSettings.GameType = GT_CUSTOM then
   begin
    g_Sound_PlayEx('INTER', 127, 255);
    gState := STATE_INTERCUSTOM;
   end
    else
   begin
    //g_GUI_ShowWindow('MainMenu');
    //g_Game_PlayMusic('MENU');
    //gState := STATE_MENU;

    //start_trigger('megawad.end');

    g_Game_PlayMusic('endmus');
    gState := STATE_ENDPIC;
   end;
  end else DecMin(EndingGameCounter, 6, 0);
 end;

 if gState = STATE_INTERSINGLE then
 begin
  if ((e_KeyBuffer[28] = $080) or (e_KeyBuffer[57] = $080)) and not gConsoleShow then
  begin
   g_Game_StopMusic();

   if NextMap <> '' then
   begin
    gExit := 0;
    g_Game_StartMap(NextMap);
   end else gExit := EXIT_SIMPLE;
   Exit;
  end;
 end;

 if gState = STATE_INTERTEXT then
 begin
  if ((e_KeyBuffer[28] = $080) or (e_KeyBuffer[57] = $080))
     and not gConsoleShow then
  begin
   g_Game_StopMusic();

   //if next_trigger() then Exit;

   if NextMap <> '' then
   begin
    gExit := 0;
    g_Game_StartMap(NextMap);
   end
    else
   begin
    //gState := STATE_ENDGAMECUSTOM;
    gExit := EXIT_SIMPLE;
   end;

   Exit;
  end;

  if InterText.counter > 0 then InterText.counter := InterText.counter-1;
 end;

 if gState = STATE_INTERPIC then
 begin
  if ((e_KeyBuffer[28] = $080) or (e_KeyBuffer[57] = $080))
     and not gConsoleShow then
  begin
   g_Game_StopMusic();

   //if next_trigger() then Exit;

   if NextMap <> '' then
   begin
    gExit := 0;
    g_Game_StartMap(NextMap);
   end
    else
   begin
    //gState := STATE_ENDGAMECUSTOM;
    gExit := EXIT_SIMPLE;
   end;

   Exit;
  end;
 end;

 if gState = STATE_ENDPIC then
 begin
 end;

 if gGameOn then
  IsDrawStat := (not gConsoleShow) and (gGameSettings.GameType <> GT_SINGLE) and
                (e_KeyBuffer[gGameControls.GameControls.Stat] = $080);

 if gGameOn and not gPause and (gState <> STATE_FOLD) then
 begin
  gTime := gTime+GAME_TICK;

  if MessageTime = 0 then MessageText := '';
  if MessageTime > 0 then MessageTime := MessageTime-1;

  if gGameSettings.TimeLimit > 0 then
   if (gTime - GameStartTime) div 1000 >= gGameSettings.TimeLimit then
   begin
    gExit := EXIT_ENDLEVELCUSTOM;
    g_Game_NextLevel();
    Exit;
   end;

  if gGameSettings.GoalLimit > 0 then
  begin
   b := 0;

   if gGameSettings.GameMode = GM_DM then
   begin
    for i := 0 to High(gPlayers) do
     if gPlayers[i] <> nil then
      if gPlayers[i].Frags > b then b := gPlayers[i].Frags;
   end
    else if gGameSettings.GameMode in [GM_CTF, GM_TDM] then
   begin
    b := Max(gTeamStat[TEAM_RED].Goals, gTeamStat[TEAM_BLUE].Goals);
   end;

   if b >= gGameSettings.GoalLimit then
   begin
    gExit := EXIT_ENDLEVELCUSTOM;
    g_Game_NextLevel();
    Exit;
   end;
  end;

  if not gConsoleShow then
  begin
   with gGameControls.P1Control do
   begin
    if (e_KeyBuffer[KeyLeft] = $080) and (e_KeyBuffer[KeyRight] <> $080) then
     P1MoveButton := 1
    else if (e_KeyBuffer[KeyLeft] <> $080) and (e_KeyBuffer[KeyRight] = $080) then
     P1MoveButton := 2
    else if (e_KeyBuffer[KeyLeft] <> $080) and (e_KeyBuffer[KeyRight] <> $080) then
     P1MoveButton := 0;

    gPlayer1.ReleaseKeys();

    if P1MoveButton = 1 then gPlayer1.PressKey(KEY_LEFT) else
     if P1MoveButton = 2 then gPlayer1.PressKey(KEY_RIGHT);

    if (P1MoveButton = 2) and (e_KeyBuffer[KeyLeft] = $080) then gPlayer1.SetDirection(D_LEFT)
     else if (P1MoveButton = 1) and (e_KeyBuffer[KeyRight] = $080) then gPlayer1.SetDirection(D_RIGHT)
      else if P1MoveButton <> 0 then gPlayer1.SetDirection(TDirection(P1MoveButton-1));

    if e_KeyBuffer[KeyJump] = $080 then gPlayer1.PressKey(KEY_JUMP);
    if e_KeyBuffer[KeyUp] = $080 then gPlayer1.PressKey(KEY_UP);
    if e_KeyBuffer[KeyDown] = $080 then gPlayer1.PressKey(KEY_DOWN);
    if e_KeyBuffer[KeyFire] = $080 then gPlayer1.PressKey(KEY_FIRE);
    if e_KeyBuffer[KeyNextWeapon] = $080 then gPlayer1.PressKey(KEY_NEXTWEAPON);
    if e_KeyBuffer[KeyPrevWeapon] = $080 then gPlayer1.PressKey(KEY_PREVWEAPON);
    if e_KeyBuffer[KeyOpen] = $080 then gPlayer1.PressKey(KEY_OPEN);
   end;

   if gPlayer2 <> nil then
    with gGameControls.P2Control do
    begin
     if (e_KeyBuffer[KeyLeft] = $080) and (e_KeyBuffer[KeyRight] <> $080) then
      P2MoveButton := 1
     else if (e_KeyBuffer[KeyLeft] <> $080) and (e_KeyBuffer[KeyRight] = $080) then
      P2MoveButton := 2
     else if (e_KeyBuffer[KeyLeft] <> $080) and (e_KeyBuffer[KeyRight] <> $080) then
      P2MoveButton := 0;

     if P2MoveButton = 1 then gPlayer2.PressKey(KEY_LEFT) else
     if P2MoveButton = 2 then gPlayer2.PressKey(KEY_RIGHT);

     if (P2MoveButton = 2) and (e_KeyBuffer[KeyLeft] = $080) then gPlayer2.SetDirection(D_LEFT)
      else if (P2MoveButton = 1) and (e_KeyBuffer[KeyRight] = $080) then gPlayer2.SetDirection(D_RIGHT)
       else if P2MoveButton <> 0 then gPlayer2.SetDirection(TDirection(P2MoveButton-1));

     if e_KeyBuffer[KeyJump] = $080 then gPlayer2.PressKey(KEY_JUMP);
     if e_KeyBuffer[KeyUp] = $080 then gPlayer2.PressKey(KEY_UP);
     if e_KeyBuffer[KeyDown] = $080 then gPlayer2.PressKey(KEY_DOWN);
     if e_KeyBuffer[KeyFire] = $080 then gPlayer2.PressKey(KEY_FIRE);
     if e_KeyBuffer[KeyNextWeapon] = $080 then gPlayer2.PressKey(KEY_NEXTWEAPON);
     if e_KeyBuffer[KeyPrevWeapon] = $080 then gPlayer2.PressKey(KEY_PREVWEAPON);
     if e_KeyBuffer[KeyOpen] = $080 then gPlayer2.PressKey(KEY_OPEN);
    end;
  end;

  g_Map_Update();
  g_Items_Update();
  g_Triggers_Update();
  g_Weapon_Update();
  g_Monsters_Update();
  g_GFX_Update();
  g_Player_UpdateAll();
  g_Player_UpdateCorpse();
 end;

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

  if g_ActiveWindow <> nil then g_ActiveWindow.Update;
 end;

 if e_KeyBuffer[gGameControls.GameControls.TakeScreenshot] = $080 then
  if GetTimer()-LastScreenShot > 200000 then
   begin
    g_TakeScreenShot();
    LastScreenShot := GetTimer();
   end;

 Time := GetTimer() div 1000;
 UPSCounter := UPSCounter+1;
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

 g_Game_SetLoadingText('Items Data', 0);
 g_Items_LoadData();

 g_Game_SetLoadingText('Weapons Data', 0);
 g_Weapon_LoadData();

 g_Monsters_LoadData();

 e_WriteLog('Loading game data...', MSG_NOTIFY);

 g_Game_SetLoadingText('Game Data', 0);

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
  s1: string;
begin
 pc := Length(CustomStat.PlayerStat);

 if pc = 0 then Exit;

 e_CharFont_GetSize(gMenuFont, I_MENU_INTER1, ww1, hh1);
 e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(ww1 div 2), 16, I_MENU_INTER1);

 e_TextureFontGetSize(gStdFont, ww2, hh2);

 x := 32;
 y := 16+hh1+16;

 w := gScreenWidth-x*2;

 w2 := (w-16) div 6;
 w3 := w2;
 w1 := w-16-w2-w3;

 e_DrawFillQuad(x, y, gScreenWidth-x, gScreenHeight-y, 64, 64, 64, 32);
 e_DrawQuad(x, y, gScreenWidth-x, gScreenHeight-y, 1, 255, 127, 0);

 m := Max(Length(I_MENU_MAP)+1, Length(I_GAME_GAMETIME)+1)*ww2;

 case CustomStat.GameMode of
  GM_DM: s1 := I_GAME_DM;
  GM_TDM: s1 := I_GAME_TDM;
  GM_CTF: s1 := I_GAME_CTF;
  else s1 := '';
 end;

 _y := y+16;
 e_TextureFontPrintEx(x+(w div 2)-(Length(s1)*ww2 div 2), _y, s1, gStdFont, 255, 255, 255, 1);
 _y := _y+8;

 _y := _y+16;
 e_TextureFontPrintEx(x+8, _y, I_MENU_MAP, gStdFont, 255, 127, 0, 1);
 e_TextureFontPrint(x+8+m, _y, Format('%s - %s', [CustomStat.Map, CustomStat.MapName]), gStdFont);

 _y := _y+16;
 e_TextureFontPrintEx(x+8, _y, I_GAME_GAMETIME, gStdFont, 255, 127, 0, 1);
 e_TextureFontPrint(x+8+m, _y, Format('%.2d:%.2d', [CustomStat.GameTime div 1000 div 60,
                                                    CustomStat.GameTime div 1000 mod 60]), gStdFont);

 if CustomStat.GameMode in [GM_CTF, GM_TDM] then
 begin
  _y := _y+16+16;

  with CustomStat do
  if TeamStat[TEAM_RED].Goals > TeamStat[TEAM_BLUE].Goals then s1 := I_GAME_REDWIN
   else if TeamStat[TEAM_BLUE].Goals > TeamStat[TEAM_RED].Goals then s1 := I_GAME_BLUEWIN
    else s1 := I_GAME_NOWIN;

  e_TextureFontPrintEx(x+8+(w div 2)-(Length(s1)*ww2 div 2), _y, s1, gStdFont, 255, 255, 255, 1);
  _y := _y+40;

  for t := TEAM_RED to TEAM_BLUE do
  begin
   if t = TEAM_RED then
   begin
    e_TextureFontPrintEx(x+8, _y, Format(I_GAME_REDTEAM, [CustomStat.TeamStat[TEAM_RED].Goals]),
                         gStdFont, 255, 0, 0, 1);
    r := 255;
    g := 0;
    b := 0;
   end
    else
   begin
    e_TextureFontPrintEx(x+8, _y, Format(I_GAME_BLUETEAM, [CustomStat.TeamStat[TEAM_BLUE].Goals]),
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
  e_TextureFontPrintEx(x+8, _y, I_GAME_PLAYERNAME, gStdFont, 255, 127, 0, 1);
  e_TextureFontPrintEx(x+8+w1, _y, I_GAME_FRAGS, gStdFont, 255, 127, 0, 1);
  e_TextureFontPrintEx(x+8+w1+w2, _y, I_GAME_DEATHS, gStdFont, 255, 127, 0, 1);

  _y := _y+24;
  for p := 0 to High(CustomStat.PlayerStat) do
   with CustomStat.PlayerStat[p] do
  begin
   e_DrawFillQuad(x+8, _y+4, x+24, _y+16+4, Color.R, Color.G, Color.B, 0);

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
  s1 := I_MENU_INTER5;
  s2 := Format(' %.1f', [(SingleStat.PlayerStat[n].Kills/t)*60]);
  e_CharFont_GetSize(gMenuFont, s1, ww1, hh1);
  e_CharFont_GetSize(gMenuFont, s2, ww2, hh2);

  x := (gScreenWidth div 2)-((ww1+ww2) div 2);
  e_CharFont_Print(gMenuFont, x, y+32, s1);
  e_CharFont_PrintEx(gMenuFont, x+ww1, y+32, s2, _RGB(255, 0, 0));

  s1 := I_MENU_INTER4;
  s2 := Format(' %d', [SingleStat.PlayerStat[n].Kills]);
  e_CharFont_Print(gMenuFont, x, y, s1);
  e_CharFont_PrintEx(gMenuFont, x+ww1, y, s2, _RGB(255, 0, 0));

  s1 := I_MENU_INTER6;
  s2 := I_MENU_INTER7;
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
 e_CharFont_GetSize(gMenuFont, I_MENU_INTER2, ww1, hh1);
 e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(ww1 div 2), 32, I_MENU_INTER2);

 t := SingleStat.GameTime div 1000;
 s1 := I_MENU_INTER3;
 s2 := Format(' %d:%.2d:%.2d', [t div (60*60), (t mod (60*60)) div 60, t mod 60]);

 e_CharFont_GetSize(gMenuFont, s1, ww1, hh1);
 e_CharFont_GetSize(gMenuFont, s2, ww2, hh2);
 e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-((ww1+ww2) div 2), 80, s1);
 e_CharFont_PrintEx(gMenuFont, (gScreenWidth div 2)-((ww1+ww2) div 2)+ww1, 80, s2, _RGB(255, 0, 0));

 if SingleStat.TwoPlayers then
 begin
  s1 := I_MENU_PLAYER1;
  e_CharFont_GetSize(gMenuFont, s1, ww1, hh1);
  e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(ww1 div 2), 128, s1);
  y := 176;
  player_stat(0);

  s1 := I_MENU_PLAYER2;
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
  ww, hh, yy: Word;
  s: String;

begin
  e_CharFont_GetSize(gMenuFont, I_MENU_LOADING, ww, hh);
  yy := (gScreenHeight div 2)-(hh div 2);
  e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(ww div 2), yy-2*hh, I_MENU_LOADING);

  with LoadingStat do
    if MaxValue > 0 then
      s := Format('%s:  %d/%d', [Text, CurValue, MaxValue])
    else
      s := Text;

  e_CharFont_GetSize(gMenuFont, s, ww, hh);
  e_CharFont_PrintEx(gMenuFont, (gScreenWidth div 2)-(ww div 2), yy+hh, s, _RGB(255, 0, 0));
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
 glPopMatrix;

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
  e_DrawFillQuad(0, 0, gScreenWidth, gScreenHeight, 0, 0, 0, 180);

  e_CharFont_GetSize(gMenuFont, I_MENU_PAUSE, w, h);
  e_CharFont_Print(gMenuFont, (gScreenWidth div 2)-(w div 2),
                   (gScreenHeight div 2)-(h div 2), I_MENU_PAUSE);
 end;

 if not gGameOn then
 begin
  if (gState = STATE_MENU) and
     ((g_ActiveWindow = nil) or (g_ActiveWindow.BackTexture = '')) then
   if g_Texture_Get('MENU_BACKGROUND', ID) then
    e_DrawSize(ID, 0, 0, 0, False, False, gScreenWidth, gScreenHeight)
     else e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);

  if gState = STATE_FOLD then
   e_DrawFillQuad(0, 0, gScreenWidth, gScreenHeight, 0, 0, 0, EndingGameCounter);

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
    e_DrawFillQuad(0, 0, gScreenWidth, gScreenHeight, 0, 0, 0, EndingGameCounter)
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
   if not g_Texture_Get('endpic', ID) then g_Texture_Get('INTER', ID);

   if ID <> DWORD(-1) then
    e_DrawSize(ID, 0, 0, 0, False, False, gScreenWidth, gScreenHeight)
     else e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);
  end;
 end;

 if g_ActiveWindow <> nil then
   begin
     if gGameOn then
       e_DrawFillQuad(0, 0, gScreenWidth, gScreenHeight, 0, 0, 0, 180);
       g_ActiveWindow.Draw();
     end;

 g_Console_Draw();

 if gShowFPS then
   begin
     e_TextureFontPrint(0, 0, Format('FPS: %d', [FPS]), gStdFont);
     e_TextureFontPrint(0, 16, Format('UPS: %d', [UPS*2]), gStdFont);
   end;

 if gGameOn and gShowTime and (gGameSettings.GameType = GT_CUSTOM) then
  e_TextureFontPrint(gScreenWidth-64, 0,
                     Format('%.2d:%.2d', [gTime div 1000 div 60, gTime div 1000 mod 60]),
                     gStdFont);
end;

procedure g_Game_Quit();
begin
 g_Sound_All_Stop();
 g_Game_StopMusic();
 g_Game_FreeData();
 g_PlayerModel_FreeData();
 g_Texture_DeleteAll();
 g_Frames_DeleteAll();
 g_Menu_Free();
 gExit := EXIT_QUIT;
 PostQuitMessage(0);
end;

procedure g_FatalError(Text: string);
begin
 g_Console_Add(Format('Fatal error: %s', [Text]));
 e_WriteLog(Format('Fatal error: %s', [Text]), MSG_WARNING);

 gExit := EXIT_SIMPLE;
end;

procedure g_Game_StartSingle(WAD, MAP: string; TwoPlayers: Boolean);
begin
 g_Game_Free();

 e_WriteLog('Starting single game...', MSG_NOTIFY);

 g_Game_LoadData();

 ZeroMemory(@gGameSettings, sizeof(TGameSettings));
 gGameSettings.GameType := GT_SINGLE;
 if TwoPlayers then gGameSettings.Options := GAME_OPTION_TWOPLAYER;
 gGameSettings.Options := gGameSettings.Options+GAME_OPTION_ALLOWEXIT;
 gGameSettings.Options := gGameSettings.Options+GAME_OPTION_MONSTERDM;
 gGameSettings.WAD := WAD;

 LoadMegaWAD(WAD);

 gPlayerScreenSize.X := gScreenWidth-196;
 if LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER) then
  gPlayerScreenSize.Y := gScreenHeight div 2 else gPlayerScreenSize.Y := gScreenHeight;

 with gPlayer1Settings do
  gPlayer1 := g_Player_Get(g_Player_Create('Doomer', PLAYER1_DEF_COLOR,
                           TEAM_RED, False, PLAYERNUM_1));

 if gPlayer1 = nil then
 begin
  g_FatalError(Format(I_GAME_PLAYERCREATEERROR, [1]));
  Exit;
 end;
 gPlayer1.Name := 'Player1';

 if TwoPlayers then
 begin
  with gPlayer2Settings do
    gPlayer2 := g_Player_Get(g_Player_Create('Doomer', PLAYER2_DEF_COLOR,
                             TEAM_RED, False, PLAYERNUM_2));

  if gPlayer2 = nil then
  begin
   g_FatalError(Format(I_GAME_PLAYERCREATEERROR, [2]));
   Exit;
  end;

  gPlayer2.Name := 'Player2';
 end;

 if not g_Game_StartMap(MAP, True) then
 begin
  g_FatalError(Format(I_GAME_MAPLOADERROR, [ExtractFileName(gGameSettings.WAD)+':\'+MAP]));
  Exit;
 end;

 g_Player_Init();
 //g_Player_ResetAll(False, True);
end;

procedure g_Game_StartCustom(Map: string; GameMode: Byte; TimeLimit, GoalLimit: Word; Options: LongWord);
var
  ResName: string;
begin
 g_Game_Free();

 e_WriteLog('Starting custom game...', MSG_NOTIFY);

 g_Game_LoadData();

 gGameSettings.GameType := GT_CUSTOM;
 gGameSettings.GameMode := GameMode;
 gGameSettings.TimeLimit := TimeLimit;
 gGameSettings.GoalLimit := GoalLimit;
 gGameSettings.Options := Options;
 g_ProcessResourceStr(Map, @gGameSettings.WAD, nil, @ResName);

 gPlayerScreenSize.X := gScreenWidth-196;
 if LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER) then
  gPlayerScreenSize.Y := gScreenHeight div 2 else gPlayerScreenSize.Y := gScreenHeight;

 if gGameSettings.GameMode = GM_COOP then
 begin
   gGameSettings.GameType := GT_SINGLE;
 end;

 with gPlayer1Settings do
   if (gGameSettings.GameType = GT_SINGLE) then
     begin
       if LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER) then
         gPlayer1 := g_Player_Get(g_Player_Create('Doomer', PLAYER1_DEF_COLOR,
           TEAM_RED, False, PLAYERNUM_1))
       else
         gPlayer1 := g_Player_Get(g_Player_Create('Doomer', PLAYER1_DEF_COLOR,
           TEAM_NONE, False, PLAYERNUM_1));
     end
   else
     gPlayer1 := g_Player_Get(g_Player_Create(Model, Color,
       IfThen(GameMode = GM_DM, TEAM_NONE, Team), False, PLAYERNUM_1));

 if gPlayer1 = nil then
 begin
  g_FatalError(Format(I_GAME_PLAYERCREATEERROR, [1]));
  Exit;
 end;

 gPlayer1.Name := gPlayer1Settings.Name;

 if LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER) then
 begin
  with gPlayer2Settings do
   if gGameSettings.GameType = GT_SINGLE then
     gPlayer2 := g_Player_Get(g_Player_Create('Doomer', PLAYER2_DEF_COLOR,
       TEAM_RED, False, PLAYERNUM_2))
   else
     gPlayer2 := g_Player_Get(g_Player_Create(Model, Color,
       IfThen(GameMode = GM_DM, TEAM_NONE, Team), False, PLAYERNUM_2));

  if gPlayer2 = nil then
   begin
    g_FatalError(Format(I_GAME_PLAYERCREATEERROR, [2]));
    Exit;
   end;

  gPlayer2.Name := gPlayer2Settings.Name;
 end;

 if not g_Game_StartMap(ResName, True) then
 begin
  g_FatalError(Format(I_GAME_MAPLOADERROR, [ExtractFileName(gGameSettings.WAD)+':\'+ResName]));
  Exit;
 end;

 if (GameMode = GM_CTF) and not g_Map_HaveFlagPoints() then
 begin
  g_FatalError(I_GAME_CTFERROR);
  Exit;
 end;

 if ((gGameSettings.GameMode = GM_DM) and (g_Map_GetPointCount(RESPAWNPOINT_DM) < 2)) or
    (((gGameSettings.GameMode = GM_TDM) or (gGameSettings.GameMode = GM_CTF)) and
    (g_Map_GetPointCount(RESPAWNPOINT_DM)+g_Map_GetPointCount(RESPAWNPOINT_RED)+
     g_Map_GetPointCount(RESPAWNPOINT_BLUE) < 2)) then
 begin
  g_FatalError(I_GAME_DMERROR);
  Exit;
 end;

 g_Player_Init();
end;

procedure g_Game_Restart();
var
  Map: string;
begin
 g_ProcessResourceStr(gMapInfo.Map, nil, nil, @Map);

 gGameOn := False;
 g_Game_StartMap(Map, True);
end;

function g_Game_StartMap(Map: string; Force: Boolean = False): Boolean;
begin
 g_Map_Free();
 g_Player_RemoveAllCorpses();

 Result := g_Map_Load(gGameSettings.WAD+':\'+Map);
 if Result then g_Player_ResetAll(Force, True);

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

 if Map = '' then
  (if gGameSettings.GameType = GT_SINGLE then gExit := EXIT_ENDGAMESINGLE
    else gExit := EXIT_ENDLEVELCUSTOM)
 else
  (if gGameSettings.GameType = GT_SINGLE then gExit := EXIT_ENDLEVELSINGLE
    else gExit := EXIT_ENDLEVELCUSTOM)
end;

procedure g_Game_NextLevel();
{var
  Map, FileName: string;}
begin
 if gGameSettings.GameType = GT_CUSTOM then gExit := EXIT_ENDLEVELCUSTOM
   else gExit := EXIT_ENDLEVELSINGLE;

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

procedure GameCommands(P: SArray);
var
  a, b: Integer;
  s: string;
  stat: TPlayerStatArray;
begin
 if (LowerCase(P[0]) = 'quit') or (LowerCase(P[0]) = 'exit') then g_Game_Quit();
 if LowerCase(P[0]) = 'pause' then if gGameOn then gPause := not gPause;
 if LowerCase(P[0]) = 'endgame' then gExit := EXIT_SIMPLE;
 if LowerCase(P[0]) = 'restart' then if gGameOn then g_Game_Restart();

 if LowerCase(P[0]) = 'g_showfps' then
  if (Length(P) > 1) and ((P[1] = '1') or (P[1] = '0')) then
   gShowFPS := P[1][1] = '1'
    else g_Console_Add(Format('g_showfps is %d', [Byte(gShowFPS)]));

 if LowerCase(P[0]) = 'map' then if (Length(P) > 1) then
 begin
  if Pos('.wad', LowerCase(P[1])) = 0 then P[1] := P[1]+'.wad';

  if Length(P) > 2 then s := MapsDir+P[1]+':\'+UpperCase(P[2])
   else s := MapsDir+P[1]+':\MAP01';

  if g_Map_Exist(s) then
  begin
    g_Game_Free();
    with gGameSettings do
     g_Game_StartCustom(s, GameMode, TimeLimit, GoalLimit, Options);
  end;
 end;

 if LowerCase(P[0]) = 'ffire' then
   with gGameSettings do
     begin
       if (Length(P) > 1) and ((P[1] = '1') or (P[1] = '0')) then
         begin
           if (P[1][1] = '1') then
             Options := Options or GAME_OPTION_TEAMDAMAGE
           else
             Options := Options and (not GAME_OPTION_TEAMDAMAGE);
         end;
       if (LongBool(Options and GAME_OPTION_TEAMDAMAGE)) then
         g_Console_Add('FriendlyFire is On')
       else
         g_Console_Add('FriendlyFire is Off');
     end;

 if gGameSettings.GameType = GT_CUSTOM then
 begin

  if LowerCase(P[0]) = 'p1_name' then
   if (Length(P) > 1) and gGameOn and (gPlayer1 <> nil) then gPlayer1.Name := P[1];

  if LowerCase(P[0]) = 'p2_name' then
   if (Length(P) > 1) and gGameOn and (gPlayer2 <> nil) then gPlayer2.Name := P[1];

  if LowerCase(P[0]) = 'g_showtime' then
   if (Length(P) > 1) and ((P[1] = '1') or (P[1] = '0')) then
    gShowTime := P[1][1] = '1'
     else g_Console_Add(Format('g_showtime is %d', [Byte(gShowTime)]));

  if LowerCase(P[0]) = 'g_showgoals' then
   if (Length(P) > 1) and ((P[1] = '1') or (P[1] = '0')) then
    gShowGoals := P[1][1] = '1'
     else g_Console_Add(Format('g_showgoals is %d', [Byte(gShowGoals)]));

  if LowerCase(P[0]) = 'g_showstat' then
   if (Length(P) > 1) and ((P[1] = '1') or (P[1] = '0')) then
    gShowStat := P[1][1] = '1'
     else g_Console_Add(Format('g_showstat is %d', [Byte(gShowStat)]));

  if LowerCase(P[0]) = 'g_showkillmsg' then
   if (Length(P) > 1) and ((P[1] = '1') or (P[1] = '0')) then
    gShowKillMsg := P[1][1] = '1'
     else g_Console_Add(Format('g_showkillmsg is %d', [Byte(gShowKillMsg)]));

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

  if LowerCase(P[0]) = 'fraglimit' then
   if Length(P) > 1 then
   begin
    b := 0;

    if gGameSettings.GameMode = GM_DM then
    begin
     stat := g_Player_GetStats();
     if stat <> nil then
      for a := 0 to High(stat) do
       if stat[a].Frags > b then b := stat[a].Frags;
    end else b := Max(gTeamStat[TEAM_RED].Goals, gTeamStat[TEAM_BLUE].Goals);

    gGameSettings.GoalLimit := Max(StrToIntDef(P[1], b), b)
   end else g_Console_Add(Format('fraglimit is %d', [gGameSettings.GoalLimit]));

 if (LowerCase(P[0]) = 'monster') and (Length(P) > 1) then
  g_Monsters_Create(StrToIntDef(P[1], 255), gPlayer1.GameX, gPlayer1.GameY-48, D_LEFT);

  if (LowerCase(P[0]) = 'addbot') or (LowerCase(P[0]) = 'bot_add') then
   if Length(P) > 1 then g_Bot_Add(TEAM_NONE, StrToIntDef(P[1], 2))
    else g_Bot_Add(TEAM_NONE, 2);

  if LowerCase(P[0]) = 'bot_addlist' then
   if Length(P) > 1 then
    if Length(P) = 2 then g_Bot_AddList(TEAM_NONE, P[1], StrToIntDef(P[1], -1))
     else g_Bot_AddList(IfThen(P[2] = 'red', TEAM_RED, TEAM_BLUE), P[1], StrToIntDef(P[1], -1));

  if LowerCase(P[0]) = 'bot_addred' then
   if Length(P) > 1 then g_Bot_Add(TEAM_RED, StrToIntDef(P[1], 2))
    else g_Bot_Add(TEAM_RED, 2);

  if LowerCase(P[0]) = 'bot_addblue' then
   if Length(P) > 1 then g_Bot_Add(TEAM_BLUE, StrToIntDef(P[1], 2))
    else g_Bot_Add(TEAM_BLUE, 2);

  if LowerCase(P[0]) = 'bot_removeall' then g_Bot_RemoveAll();
 end;
end;

procedure g_Game_PlayMusic(name: string);
begin
 g_Game_StopMusic();

 if g_Music_PlayEx(name, 255) then gMusic := name;
end;

procedure g_Game_StopMusic();
begin
 if gMusic = '' then Exit;

 g_Music_StopEx(gMusic);
 gMusic := '';
end;

procedure g_Game_SetMusicVolume();
var
  ID: DWORD;
begin
 if gMusic = '' then Exit;

 if g_Music_Get(ID, gMusic) then e_SetSongVolume(ID, gMusicLevel);
end;

procedure g_TakeScreenShot();
var
  a: Word;
  FileName: string;
begin
 for a := 1 to High(Word) do
 begin
  FileName := Format(GameDir+'\Screenshots\Screenshot%.3d.bmp', [a]);
  if not FileExists(FileName) then
  begin
   e_MakeScreenshot(FileName, gScreenWidth, gScreenHeight);
   g_Console_Add(Format(I_CONSOLE_SCREENSHOT, [ExtractFileName(FileName)]));
   Break;
  end;
 end;
end;

procedure g_Game_InGameMenu(Show: Boolean);
begin
 if not gGameOn then Exit;

 if (g_ActiveWindow = nil) and Show then
 begin
  if gGameSettings.GameType = GT_SINGLE then g_GUI_ShowWindow('GameSingleMenu')
   else g_GUI_ShowWindow('GameCustomMenu');
  g_Sound_PlayEx('MENU_OPEN', 127, 255);
  g_Game_Pause(True);
 end else if (g_ActiveWindow <> nil) and not Show then gPause := False;
end;

procedure g_Game_Pause(Enable: Boolean);
begin
 if not gGameOn then Exit;

 if Enable then
 begin
  if gGameSettings.GameType in [GT_SINGLE, GT_CUSTOM] then gPause := True;
 end else if g_ActiveWindow = nil then gPause := False;
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

procedure g_Game_SetLoadingText(Text: String; Max: Integer);
begin
  LoadingStat.Text := Text;
  LoadingStat.CurValue := 0;
  LoadingStat.MaxValue := Max;
  LoadingStat.ShowCount := 0;
  ProcessLoading;
end;

procedure g_Game_StepLoading();
begin
  Inc(LoadingStat.CurValue);
  Inc(LoadingStat.ShowCount);
  if (LoadingStat.ShowCount > LOADING_SHOW_STEP) then
    begin
      LoadingStat.ShowCount := 0;
      ProcessLoading;
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
      if (s[1] = '-') and (Length(s) > 1) and (i < ParamCount) then
        begin
          Inc(i);
          SetLength(pars, Length(pars) + 1);
          with pars[High(pars)] do
            begin
              Name := LowerCase(s);
              Value := LowerCase(ParamStr(i));
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

procedure g_Game_Process_Params;
var
  pars: TParamStrValues;
  map: String;
  GMode: Byte;
  LimT, LimG, Opt: Integer;
  s: String;

begin
  Parse_Params(pars);

  map := Find_Param_Value(pars, '-map');
  if (map <> '') and (Pos('.wad:\', map) > 0) then
    begin
    // Game mode:
      s := Find_Param_Value(pars, '-gmode');
      if s = 'tdm' then GMode := GM_TDM
      else if s = 'ctf' then GMode := GM_CTF
      else if s = 'coop' then GMode := GM_COOP
      else GMode := GM_DM;
    // Time limit:
      s := Find_Param_Value(pars, '-glimt');
      if (s = '') or (not TryStrToInt(s, LimT)) then
        LimT := 0;
      if LimT < 0 then
        LimT := 0;
    // Goal limit:
      s := Find_Param_Value(pars, '-glimg');
      if (s = '') or (not TryStrToInt(s, LimG)) then
        LimG := 0;
      if LimG < 0 then
        LimG := 0;
    // Options:
      s := Find_Param_Value(pars, '-gopt');
      if (s = '') or (not TryStrToInt(s, Opt)) then
        Opt := GAME_OPTION_ALLOWEXIT;
      if Opt < 0 then
        Opt := GAME_OPTION_ALLOWEXIT;
    // Start:
      g_Game_StartCustom(map, GMode, LimT, LimG, Opt);
    end;

  SetLength(pars, 0);
end;

end.
