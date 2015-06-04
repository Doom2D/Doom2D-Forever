unit g_main;

interface

procedure Main();
procedure Init();
procedure Release();
procedure Update();
procedure Draw();
procedure KeyPress(K: Byte);
procedure CharPress(C: Char);

var
  GameDir: string;
  DataDir: string;
  MapsDir: string;
  ModelsDir: string;
  GameWAD: string;

implementation

uses
  Windows, WADEDITOR, e_log, g_window, dglOpenGL,
  e_graphics, e_input, g_game, g_console, g_gui,
  messages, e_sound, g_options, g_sound, g_player,
  g_weapons, SysUtils, g_triggers, MAPDEF, g_map,
  MAPSTRUCT, g_menu, g_language, g_net;

var
  charbuff: Array [0..15] of Char;

procedure Main();
begin
  GetDir(0, GameDir);
  MapsDir := GameDir + '\maps\';
  DataDir := GameDir + '\data\';
  ModelsDir := DataDir + 'models\';
  GameWAD := DataDir + 'Game.wad';

  e_InitLog(GameDir + '\' + LOG_FILENAME, WM_NEWFILE);

  e_WriteLog('Read config file', MSG_NOTIFY);
  g_Options_Read(GameDir + '\' + CONFIG_FILENAME);

  //GetSystemDefaultLCID()

  //e_WriteLog('Read language file', MSG_NOTIFY);
  //g_Language_Load(DataDir + gLanguage + '.txt');
  e_WriteLog(gLanguage, MSG_NOTIFY);
  g_Language_Set(gLanguage);

  e_WriteLog('Entering WinMain', MSG_NOTIFY);
  
  {$WARNINGS OFF}
  WinMain(hInstance, 0, CmdLine, CmdShow);
  {$WARNINGS ON}
end;

procedure Init();
var
  a: Integer;
begin
  Randomize;

  e_WriteLog('Init DirectInput', MSG_NOTIFY);
  e_InitDirectInput(h_Wnd);

  e_WriteLog('Init FMOD', MSG_NOTIFY);
  e_InitSoundSystem(48000); 

  e_WriteLog('Init game', MSG_NOTIFY);
  g_Game_Init();

  for a := 0 to 15 do charbuff[a] := ' ';
end;

procedure Release();
begin
  e_WriteLog('Releasing engine', MSG_NOTIFY);
  e_ReleaseEngine();

  e_WriteLog('Releasing DirectInput', MSG_NOTIFY);
  e_ReleaseDirectInput();

  e_WriteLog('Releasing FMOD', MSG_NOTIFY);
  e_ReleaseSoundSystem();
end;

procedure Update();
begin
  g_Game_Update();
end;

procedure Draw();
begin
  g_Game_Draw();
end;

function LocalCheatStr(LocStr: TStrings_Locale): String;
var
  i: Integer;
begin
  Result := _lc[LocStr];
  for i := 1 to Length(Result) do
    case Result[i] of
      'Й': Result[i] := 'Q';
      'Ц': Result[i] := 'W';
      'У': Result[i] := 'E';
      'К': Result[i] := 'R';
      'Е': Result[i] := 'T';
      'Н': Result[i] := 'Y';
      'Г': Result[i] := 'U';
      'Ш': Result[i] := 'I';
      'Щ': Result[i] := 'O';
      'З': Result[i] := 'P';
      'Х': Result[i] := Chr(219);
      'Ъ': Result[i] := Chr(221);
      'Ф': Result[i] := 'A';
      'Ы': Result[i] := 'S';
      'В': Result[i] := 'D';
      'А': Result[i] := 'F';
      'П': Result[i] := 'G';
      'Р': Result[i] := 'H';
      'О': Result[i] := 'J';
      'Л': Result[i] := 'K';
      'Д': Result[i] := 'L';
      'Ж': Result[i] := Chr(186);
      'Э': Result[i] := Chr(222);
      'Я': Result[i] := 'Z';
      'Ч': Result[i] := 'X';
      'С': Result[i] := 'C';
      'М': Result[i] := 'V';
      'И': Result[i] := 'B';
      'Т': Result[i] := 'N';
      'Ь': Result[i] := 'M';
      'Б': Result[i] := Chr(188);
      'Ю': Result[i] := Chr(190);
    end;
end;

procedure Cheat();
const
  CHEAT_DAMAGE = 500;
label
  Cheated;
var
  s, s2, ls: string;
  c: Char16;
  a: Integer;
begin
  if (not gGameOn) or (not gCheats) or ((gGameSettings.GameType <> GT_SINGLE) and
    (gGameSettings.GameMode <> GM_COOP) and (not gDebugMode))
    or g_Game_IsNet then Exit;

  s := 'SOUND_GAME_RADIO';

  // ГОРЕЦ включает неуязвимость
  ls := LocalCheatStr(I_GAME_CHEAT_GODMODE);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    if gPlayer1 <> nil then gPlayer1.GodMode := not gPlayer1.GodMode;
    if gPlayer2 <> nil then gPlayer2.GodMode := not gPlayer2.GodMode;
    goto Cheated;
  end;
  // RAMBO даёт всё оружие и ключи
  ls := LocalCheatStr(I_GAME_CHEAT_WEAPONS);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    if gPlayer1 <> nil then gPlayer1.AllRulez(False);
    if gPlayer2 <> nil then gPlayer2.AllRulez(False);
    goto Cheated;
  end;
  // TANK поднимает здоровье и броню
  ls := LocalCheatStr(I_GAME_CHEAT_HEALTH);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    if gPlayer1 <> nil then gPlayer1.AllRulez(True);
    if gPlayer2 <> nil then gPlayer2.AllRulez(True);
    goto Cheated;
  end;
  // IDDQD наказывает нечестного игрока
  ls := LocalCheatStr(I_GAME_CHEAT_DEATH);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    if gPlayer1 <> nil then gPlayer1.Damage(CHEAT_DAMAGE, 0, 0, 0, HIT_TRAP);
    if gPlayer2 <> nil then gPlayer2.Damage(CHEAT_DAMAGE, 0, 0, 0, HIT_TRAP);
    s := 'SOUND_MONSTER_HAHA';
    goto Cheated;
  end;
  // СИМСИМ открывает все двери
  ls := LocalCheatStr(I_GAME_CHEAT_DOORS);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    g_Triggers_OpenAll();
    goto Cheated;
  end;
  // GOODBYE нажимает кнопку выхода на уровне
  ls := LocalCheatStr(I_GAME_CHEAT_NEXTMAP);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    if gTriggers <> nil then
      for a := 0 to High(gTriggers) do
        if gTriggers[a].TriggerType = TRIGGER_EXIT then
        begin
          g_Game_ExitLevel(gTriggers[a].Data.MapName);
          Break;
        end;
    goto Cheated;
  end;
  // ПОШЕЛНА посылает на нужный уровень
  ls := LocalCheatStr(I_GAME_CHEAT_CHANGEMAP);
  if Copy(charbuff, 17 - Length(ls) - 2, Length(ls)) = ls then
  begin
    s2 := Copy(charbuff, 15, 2);
    if g_Map_Exist(gGameSettings.WAD+':\MAP'+s2) then
    begin
      c := 'MAP00';
      c[3] := s2[1];
      c[4] := s2[2];
      g_Game_ExitLevel(c);
    end;
    goto Cheated;
  end;
  // БЕЛЫЙОРЕЛ позволяет летать
  ls := LocalCheatStr(I_GAME_CHEAT_FLY);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    gFly := not gFly;
    goto Cheated;
  end;
  // BULLFROG даёт высокие прыжки
  ls := LocalCheatStr(I_GAME_CHEAT_JUMPS);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    VEL_JUMP := 30-VEL_JUMP;
    goto Cheated;
  end;
  // FORMULA1 ускоряет бег
  ls := LocalCheatStr(I_GAME_CHEAT_SPEED);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    MAX_RUNVEL := 32-MAX_RUNVEL;
    goto Cheated;
  end;
  // CONDOM даёт защитный костюм
  ls := LocalCheatStr(I_GAME_CHEAT_SUIT);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    if gPlayer1 <> nil then gPlayer1.GiveItem(ITEM_SUIT);
    if gPlayer2 <> nil then gPlayer2.GiveItem(ITEM_SUIT);
    goto Cheated;
  end;
  // ЧЕРНОМОР даёт полный запас кислорода
  ls := LocalCheatStr(I_GAME_CHEAT_AIR);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    if gPlayer1 <> nil then gPlayer1.GiveItem(ITEM_OXYGEN);
    if gPlayer2 <> nil then gPlayer2.GiveItem(ITEM_OXYGEN);
    goto Cheated;
  end;
  // PURELOVE даёт силушку богатырскую
  ls := LocalCheatStr(I_GAME_CHEAT_BERSERK);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    if gPlayer1 <> nil then gPlayer1.GiveItem(ITEM_MEDKIT_BLACK);
    if gPlayer2 <> nil then gPlayer2.GiveItem(ITEM_MEDKIT_BLACK);
    goto Cheated;
  end;
  // ЯСВОБОДЕН даёт реактивный ранец
  ls := LocalCheatStr(I_GAME_CHEAT_JETPACK);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    if gPlayer1 <> nil then gPlayer1.GiveItem(ITEM_JETPACK);
    if gPlayer2 <> nil then gPlayer2.GiveItem(ITEM_JETPACK);
    goto Cheated;
  end;
  // CASPER позволяет ходить сквозь стены
  ls := LocalCheatStr(I_GAME_CHEAT_NOCLIP);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    if gPlayer1 <> nil then gPlayer1.SwitchNoClip;
    if gPlayer2 <> nil then gPlayer2.SwitchNoClip;
    goto Cheated;
  end;
  // ШТИРЛИЦ скрывает от глаз монстров
  ls := LocalCheatStr(I_GAME_CHEAT_NOTARGET);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    if gPlayer1 <> nil then gPlayer1.NoTarget := not gPlayer1.NoTarget;
    if gPlayer2 <> nil then gPlayer2.NoTarget := not gPlayer2.NoTarget;
    goto Cheated;
  end;
  // INFERNO отменяет необходимость перезарядки оружий
  ls := LocalCheatStr(I_GAME_CHEAT_NORELOAD);
  if Copy(charbuff, 17 - Length(ls), Length(ls)) = ls then
  begin
    if gPlayer1 <> nil then gPlayer1.NoReload := not gPlayer1.NoReload;
    if gPlayer2 <> nil then gPlayer2.NoReload := not gPlayer2.NoReload;
    goto Cheated;
  end;
  Exit;

Cheated:
  g_Sound_PlayEx(s);
end;

procedure KeyPress(K: Byte);
var
  Msg: g_gui.TMessage;
  a: Integer;
  b: Boolean;
begin
  case K of
    VK_PAUSE: // <Pause/Break>:
      begin
      // Переключить паузу:
        if (g_ActiveWindow = nil) then
          g_Game_Pause(not gPause);
      end;
                          
    192: // <`/~/ё/Ё>:
      begin
        g_Console_Switch();
      end;

    VK_ESCAPE: // <Esc>:
      begin
        if gChatShow then
        begin
          g_Console_Chat_Switch();
          Exit;
        end;
          
        if gConsoleShow then // Убрать консоль
          g_Console_Switch()
        else
          if g_ActiveWindow <> nil then
            begin // Выйти из меню
              Msg.Msg := WM_KEYDOWN;
              Msg.WParam := VK_ESCAPE;
              g_ActiveWindow.OnMessage(Msg);
            end
          else
            if gGameOn then // Войти во внутриигровое меню
              g_Game_InGameMenu(True)
            else
              if (gExit = 0) then
                begin // Войти в главное меню
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

    VK_F2, VK_F3, VK_F4, VK_F5, VK_F6, VK_F7, VK_F10:
      begin // <F2> .. <F6> и <F12>
      // Только во время игры, в отсутствии консоли и чата:
        if gGameOn and (not gConsoleShow) and (not gChatShow) then
        begin
        // Закрываем все окна:
          while g_ActiveWindow <> nil do
            g_GUI_HideWindow(False);

        // Пауза при меню только в одиночной игре:
          if (not g_Game_IsNet) then
            g_Game_Pause(True);

          b := (gGameSettings.GameType = GT_CUSTOM);

          case K of
            VK_F2:
              g_Menu_Show_SaveMenu(b);
            VK_F3:
              g_Menu_Show_LoadMenu(b);
            VK_F4:
              g_Menu_Show_GameSetGame(b);
            VK_F5:
              g_Menu_Show_OptionsVideo(b);
            VK_F6:
              g_Menu_Show_OptionsSound(b);
            VK_F7:
              g_Menu_Show_EndGameMenu(b);
            VK_F10:
              g_Menu_Show_QuitGameMenu(b);
          end;
        end;
      end;

    else // Остальные клавиши:
      begin
        gJustChatted := False; // костылина
        if gConsoleShow or gChatShow then // Клавиши -> консоли
          g_Console_Control(K)
        else
          if g_ActiveWindow <> nil then
            begin // Клавиши -> меню
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
            end
            else
            begin // Клавиши -> набору кода
              for a := 0 to 14 do
                charbuff[a] := charbuff[a+1];
              charbuff[15] := Chr(K);

              Cheat();
            end;
          end;
      end;
  end;
end;

procedure CharPress(C: Char);
var
  Msg: g_gui.TMessage;
begin
  if (not gChatShow) and ((C = '`') or (C = '~') or (C = 'ё') or (C = 'Ё')) then
    Exit;

  if gConsoleShow or gChatShow then // Символы -> консоли
    g_Console_Char(C)
  else
    if g_ActiveWindow <> nil then
    begin // Символы -> меню
      Msg.Msg := WM_CHAR;
      Msg.WParam := Ord(C);
      g_ActiveWindow.OnMessage(Msg);
    end;
end;

end.
