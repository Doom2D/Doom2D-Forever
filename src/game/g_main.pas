unit g_main;

interface

procedure Main();
procedure Init();
procedure Release();
procedure Update();
procedure Draw();
procedure KeyPress(K: Word);
procedure CharPress(C: Char);

var
  GameDir: string;
  DataDir: string;
  MapsDir: string;
  ModelsDir: string;
  GameWAD: string;

implementation

uses
  SDL2, GL, GLExt, WADEDITOR, e_log, g_window,
  e_graphics, e_input, g_game, g_console, g_gui,
  e_sound, g_options, g_sound, g_player,
  g_weapons, SysUtils, g_triggers, MAPDEF, g_map,
  MAPSTRUCT, g_menu, g_language, g_net;

var
  charbuff: Array [0..15] of Char;

procedure Main();
begin
  GetDir(0, GameDir);
  MapsDir := GameDir + '/maps/';
  DataDir := GameDir + '/data/';
  ModelsDir := DataDir + 'models/';
  GameWAD := DataDir + 'Game.wad';

  e_InitLog(GameDir + '/' + LOG_FILENAME, WM_NEWFILE);

  e_WriteLog('Read config file', MSG_NOTIFY);
  g_Options_Read(GameDir + '/' + CONFIG_FILENAME);

  //GetSystemDefaultLCID()

  //e_WriteLog('Read language file', MSG_NOTIFY);
  //g_Language_Load(DataDir + gLanguage + '.txt');
  e_WriteLog(gLanguage, MSG_NOTIFY);
  g_Language_Set(gLanguage);

  if SDL_Init(SDL_INIT_JOYSTICK or SDL_INIT_TIMER or SDL_INIT_VIDEO) < 0 then
    raise Exception.Create('SDL: Init failed: ' + SDL_GetError());
    
  SDL_StartTextInput();

  e_WriteLog('Entering SDLMain', MSG_NOTIFY);

  {$WARNINGS OFF}
  SDLMain();
  {$WARNINGS ON}

  SDL_StopTextInput();
  
  e_WriteLog('Releasing SDL', MSG_NOTIFY);
  SDL_Quit();
end;

procedure Init();
var
  a: Integer;
begin
  Randomize;

  e_WriteLog('Init Input', MSG_NOTIFY);
  e_InitInput();

  if (e_JoysticksAvailable > 0) then
    e_WriteLog('Input: Joysticks available.', MSG_NOTIFY)
  else
    e_WriteLog('Input: No Joysticks.', MSG_NOTIFY);

  if not gNoSound then
  begin
    e_WriteLog('Init FMOD', MSG_NOTIFY);
    if not e_InitSoundSystem(44100, False) then e_InitSoundSystem(48000, True);
  end;

  e_WriteLog('Init game', MSG_NOTIFY);
  g_Game_Init();

  for a := 0 to 15 do charbuff[a] := ' ';
end;

procedure Release();
begin
  e_WriteLog('Releasing engine', MSG_NOTIFY);
  e_ReleaseEngine();

  e_WriteLog('Releasing Input', MSG_NOTIFY);
  e_ReleaseInput();

  if not gNoSound then
  begin
    e_WriteLog('Releasing FMOD', MSG_NOTIFY);
    e_ReleaseSoundSystem();
  end;
end;

procedure Update();
begin
  g_Game_Update();
end;

procedure Draw();
begin
  g_Game_Draw();
end;

function Translit(S: String): String;
var
  i: Integer;
begin
  Result := S;
  for i := 1 to Length(Result) do
    case Result[i] of
      'É': Result[i] := 'Q';
      'Ö': Result[i] := 'W';
      'Ó': Result[i] := 'E';
      'Ê': Result[i] := 'R';
      'Å': Result[i] := 'T';
      'Í': Result[i] := 'Y';
      'Ã': Result[i] := 'U';
      'Ø': Result[i] := 'I';
      'Ù': Result[i] := 'O';
      'Ç': Result[i] := 'P';
      'Õ': Result[i] := Chr(219);
      'Ú': Result[i] := Chr(221);
      'Ô': Result[i] := 'A';
      'Û': Result[i] := 'S';
      'Â': Result[i] := 'D';
      'À': Result[i] := 'F';
      'Ï': Result[i] := 'G';
      'Ð': Result[i] := 'H';
      'Î': Result[i] := 'J';
      'Ë': Result[i] := 'K';
      'Ä': Result[i] := 'L';
      'Æ': Result[i] := Chr(186);
      'Ý': Result[i] := Chr(222);
      'ß': Result[i] := 'Z';
      '×': Result[i] := 'X';
      'Ñ': Result[i] := 'C';
      'Ì': Result[i] := 'V';
      'È': Result[i] := 'B';
      'Ò': Result[i] := 'N';
      'Ü': Result[i] := 'M';
      'Á': Result[i] := Chr(188);
      'Þ': Result[i] := Chr(190);
    end;
end;

procedure Cheat();
const
  CHEAT_DAMAGE = 500;
label
  Cheated;
var
  s, s2, ls1, ls2: string;
  c: Char16;
  a: Integer;
begin
  if (not gGameOn) or (not gCheats) or ((gGameSettings.GameType <> GT_SINGLE) and
    (gGameSettings.GameMode <> GM_COOP) and (not gDebugMode))
    or g_Game_IsNet then Exit;

  s := 'SOUND_GAME_RADIO';

  //
  ls1 :=          CheatEng[I_GAME_CHEAT_GODMODE];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_GODMODE]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    if gPlayer1 <> nil then gPlayer1.GodMode := not gPlayer1.GodMode;
    if gPlayer2 <> nil then gPlayer2.GodMode := not gPlayer2.GodMode;
    goto Cheated;
  end;
  // RAMBO
  ls1 :=          CheatEng[I_GAME_CHEAT_WEAPONS];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_WEAPONS]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    if gPlayer1 <> nil then gPlayer1.AllRulez(False);
    if gPlayer2 <> nil then gPlayer2.AllRulez(False);
    goto Cheated;
  end;
  // TANK
  ls1 :=          CheatEng[I_GAME_CHEAT_HEALTH];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_HEALTH]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    if gPlayer1 <> nil then gPlayer1.AllRulez(True);
    if gPlayer2 <> nil then gPlayer2.AllRulez(True);
    goto Cheated;
  end;
  // IDDQD
  ls1 :=          CheatEng[I_GAME_CHEAT_DEATH];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_DEATH]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    if gPlayer1 <> nil then gPlayer1.Damage(CHEAT_DAMAGE, 0, 0, 0, HIT_TRAP);
    if gPlayer2 <> nil then gPlayer2.Damage(CHEAT_DAMAGE, 0, 0, 0, HIT_TRAP);
    s := 'SOUND_MONSTER_HAHA';
    goto Cheated;
  end;
  //
  ls1 :=          CheatEng[I_GAME_CHEAT_DOORS];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_DOORS]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    g_Triggers_OpenAll();
    goto Cheated;
  end;
  // GOODBYE
  ls1 :=          CheatEng[I_GAME_CHEAT_NEXTMAP];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_NEXTMAP]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    if gTriggers <> nil then
      for a := 0 to High(gTriggers) do
        if gTriggers[a].TriggerType = TRIGGER_EXIT then
        begin
          gExitByTrigger := True;
          g_Game_ExitLevel(gTriggers[a].Data.MapName);
          Break;
        end;
    goto Cheated;
  end;
  //
  ls1 :=          CheatEng[I_GAME_CHEAT_CHANGEMAP];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_CHANGEMAP]);
  s2 := Copy(charbuff, 15, 2);
  if ((Copy(charbuff, 15 - Length(ls1), Length(ls1)) = ls1) or
      (Copy(charbuff, 15 - Length(ls2), Length(ls2)) = ls2))
     and (s2[1] >= '0') and (s2[1] <= '9')
     and (s2[2] >= '0') and (s2[2] <= '9') then
  begin
    if g_Map_Exist(MapsDir+gGameSettings.WAD+':\MAP'+s2) then
    begin
      c := 'MAP00';
      c[3] := s2[1];
      c[4] := s2[2];
      g_Game_ExitLevel(c);
    end;
    goto Cheated;
  end;
  //
  ls1 :=          CheatEng[I_GAME_CHEAT_FLY];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_FLY]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    gFly := not gFly;
    goto Cheated;
  end;
  // BULLFROG
  ls1 :=          CheatEng[I_GAME_CHEAT_JUMPS];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_JUMPS]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    VEL_JUMP := 30-VEL_JUMP;
    goto Cheated;
  end;
  // FORMULA1
  ls1 :=          CheatEng[I_GAME_CHEAT_SPEED];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_SPEED]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    MAX_RUNVEL := 32-MAX_RUNVEL;
    goto Cheated;
  end;
  // CONDOM
  ls1 :=          CheatEng[I_GAME_CHEAT_SUIT];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_SUIT]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    if gPlayer1 <> nil then gPlayer1.GiveItem(ITEM_SUIT);
    if gPlayer2 <> nil then gPlayer2.GiveItem(ITEM_SUIT);
    goto Cheated;
  end;
  //
  ls1 :=          CheatEng[I_GAME_CHEAT_AIR];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_AIR]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    if gPlayer1 <> nil then gPlayer1.GiveItem(ITEM_OXYGEN);
    if gPlayer2 <> nil then gPlayer2.GiveItem(ITEM_OXYGEN);
    goto Cheated;
  end;
  // PURELOVE
  ls1 :=          CheatEng[I_GAME_CHEAT_BERSERK];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_BERSERK]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    if gPlayer1 <> nil then gPlayer1.GiveItem(ITEM_MEDKIT_BLACK);
    if gPlayer2 <> nil then gPlayer2.GiveItem(ITEM_MEDKIT_BLACK);
    goto Cheated;
  end;
  //
  ls1 :=          CheatEng[I_GAME_CHEAT_JETPACK];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_JETPACK]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    if gPlayer1 <> nil then gPlayer1.GiveItem(ITEM_JETPACK);
    if gPlayer2 <> nil then gPlayer2.GiveItem(ITEM_JETPACK);
    goto Cheated;
  end;
  // CASPER
  ls1 :=          CheatEng[I_GAME_CHEAT_NOCLIP];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_NOCLIP]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    if gPlayer1 <> nil then gPlayer1.SwitchNoClip;
    if gPlayer2 <> nil then gPlayer2.SwitchNoClip;
    goto Cheated;
  end;
  //
  ls1 :=          CheatEng[I_GAME_CHEAT_NOTARGET];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_NOTARGET]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    if gPlayer1 <> nil then gPlayer1.NoTarget := not gPlayer1.NoTarget;
    if gPlayer2 <> nil then gPlayer2.NoTarget := not gPlayer2.NoTarget;
    goto Cheated;
  end;
  // INFERNO
  ls1 :=          CheatEng[I_GAME_CHEAT_NORELOAD];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_NORELOAD]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    if gPlayer1 <> nil then gPlayer1.NoReload := not gPlayer1.NoReload;
    if gPlayer2 <> nil then gPlayer2.NoReload := not gPlayer2.NoReload;
    goto Cheated;
  end;
  ls1 :=          CheatEng[I_GAME_CHEAT_AIMLINE];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_AIMLINE]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    gAimLine := not gAimLine;
    goto Cheated;
  end;
  ls1 :=          CheatEng[I_GAME_CHEAT_AUTOMAP];
  ls2 := Translit(CheatRus[I_GAME_CHEAT_AUTOMAP]);
  if (Copy(charbuff, 17 - Length(ls1), Length(ls1)) = ls1) or
     (Copy(charbuff, 17 - Length(ls2), Length(ls2)) = ls2) then
  begin
    gShowMap := not gShowMap;
    goto Cheated;
  end;
  Exit;

Cheated:
  g_Sound_PlayEx(s);
end;

procedure KeyPress(K: Word);
var
  Msg: g_gui.TMessage;
begin
  case K of
    IK_PAUSE: // <Pause/Break>:
      begin
        if (g_ActiveWindow = nil) then
          g_Game_Pause(not gPause);
      end;

    IK_BACKQUOTE: // <`/~/¨/¸>:
      begin
        g_Console_Switch();
      end;

    IK_ESCAPE: // <Esc>:
      begin
        if gChatShow then
        begin
          g_Console_Chat_Switch();
          Exit;
        end;

        if gConsoleShow then
          g_Console_Switch()
        else
          if g_ActiveWindow <> nil then
            begin
              Msg.Msg := WM_KEYDOWN;
              Msg.WParam := IK_ESCAPE;
              g_ActiveWindow.OnMessage(Msg);
            end
          else
            if gState <> STATE_FOLD then
              if gGameOn
              or (gState = STATE_INTERSINGLE)
              or (gState = STATE_INTERCUSTOM)
              then
                g_Game_InGameMenu(True)
              else
                if (gExit = 0) and (gState <> STATE_SLIST) then
                  begin
                    if gState <> STATE_MENU then
                      if NetMode <> NET_NONE then
                      begin
                        g_Game_StopAllSounds(True);
                        g_Game_Free;
                        gState := STATE_MENU;
                        Exit;
                      end;

                    g_GUI_ShowWindow('MainMenu');
                    g_Sound_PlayEx('MENU_OPEN');
                  end;
      end;

    IK_F2, IK_F3, IK_F4, IK_F5, IK_F6, IK_F7, IK_F10:
      begin // <F2> .. <F6> ï¿½ <F12>
        if gGameOn and (not gConsoleShow) and (not gChatShow) then
        begin
          while g_ActiveWindow <> nil do
            g_GUI_HideWindow(False);

          if (not g_Game_IsNet) then
            g_Game_Pause(True);

          case K of
            IK_F2:
              g_Menu_Show_SaveMenu();
            IK_F3:
              g_Menu_Show_LoadMenu();
            IK_F4:
              g_Menu_Show_GameSetGame();
            IK_F5:
              g_Menu_Show_OptionsVideo();
            IK_F6:
              g_Menu_Show_OptionsSound();
            IK_F7:
              g_Menu_Show_EndGameMenu();
            IK_F10:
              g_Menu_Show_QuitGameMenu();
          end;
        end;
      end;

    else
      begin
        gJustChatted := False;
        if gConsoleShow or gChatShow then
          g_Console_Control(K)
        else
          if g_ActiveWindow <> nil then
          begin
            Msg.Msg := WM_KEYDOWN;
            Msg.WParam := K;
            g_ActiveWindow.OnMessage(Msg);
          end
          else
          begin
            if (gState = STATE_MENU) then
            begin
              g_GUI_ShowWindow('MainMenu');
              g_Sound_PlayEx('MENU_OPEN');
            end;
          end;
      end;
  end;
end;

procedure CharPress(C: Char);
var
  Msg: g_gui.TMessage;
  a: Integer;
begin
  if (not gChatShow) and ((C = '`') or (C = '~') or (C = '¸') or (C = '¨')) then
    Exit;

  if gConsoleShow or gChatShow then
    g_Console_Char(C)
  else
    if g_ActiveWindow <> nil then
    begin
      Msg.Msg := WM_CHAR;
      Msg.WParam := Ord(C);
      g_ActiveWindow.OnMessage(Msg);
    end
    else
    begin
      for a := 0 to 14 do
        charbuff[a] := charbuff[a+1];
      charbuff[15] := UpCase(C);
      Cheat();
    end;
end;

end.
