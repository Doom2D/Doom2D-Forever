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
  WADEDITOR, e_log, g_window, dglOpenGL, e_graphics, e_input, g_game, g_console,
  g_gui, messages, e_sound, g_options, g_sound, g_player, g_weapons, SysUtils,
  g_triggers, MAPDEF, g_map, MAPSTRUCT;

var
  charbuff: array[0..15] of Char;

procedure Main();
begin
 GetDir(0, GameDir);
 MapsDir := GameDir+'\maps\';
 DataDir := GameDir+'\data\';
 ModelsDir := DataDir+'models\';
 GameWAD := DataDir+'Game.wad';
 e_InitLog(GameDir+'\Doom2DF.log', WM_NEWFILE);

 e_WriteLog('Read config file', MSG_NOTIFY);
 g_Options_Read(GameDir+'\Doom2DF.cfg');

 e_WriteLog('Call WinMain', MSG_NOTIFY);
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
 e_InitSound(44100); 

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
 e_ReleaseSoundEngine();
end;

procedure Update();
begin
 g_Game_Update();
end;

procedure Draw();
begin
 g_Game_Draw();
end;

procedure Cheat();
var
  s, s2: string;
  c: Char16;
  a: Integer;
const
  c1 = 'UJHTW';
  c2 = 'RAMBO';
  c3 = 'TANK';
  c4 = 'IDDQD';
  c5 = 'CBVCBV';
  c6 = 'GOODBYE';
  c7 = 'GJITKYF';
  c8 = 'TKSQJHTK';
  c9 = 'BULLFROG';
  c10 = 'FORMULA1';
begin
 if (not gGameOn) or (not gCheats) or (gGameSettings.GameType <> GT_SINGLE) then Exit;

 s := 'SOUND_GAME_RADIO';

 if Copy(charbuff, 12, 5) = c1 then
 begin
  if gPlayer1 <> nil then gPlayer1.GodMode := not gPlayer1.GodMode;
  if gPlayer2 <> nil then gPlayer2.GodMode := not gPlayer2.GodMode;
 end
  else if Copy(charbuff, 12, 5) = c2 then
 begin
  if gPlayer1 <> nil then gPlayer1.AllRulez(False);
  if gPlayer2 <> nil then gPlayer2.AllRulez(False);
 end
  else if Copy(charbuff, 13, 4) = c3 then
 begin
  if gPlayer1 <> nil then gPlayer1.AllRulez(True);
  if gPlayer2 <> nil then gPlayer2.AllRulez(True);
 end
  else if Copy(charbuff, 12, 5) = c4 then
 begin
  if gPlayer1 <> nil then gPlayer1.Damage(500, 0, 0, 0, HIT_TRAP);
  if gPlayer2 <> nil then gPlayer2.Damage(500, 0, 0, 0, HIT_TRAP);
  s := 'SOUND_MONSTER_HAHA';
 end
  else if Copy(charbuff, 11, 6) = c5 then
 begin
  g_Triggers_OpenAll();
 end
  else if Copy(charbuff, 10, 7) = c6 then
 begin
  if gTriggers <> nil then
   for a := 0 to High(gTriggers) do
    if gTriggers[a].TriggerType = TRIGGER_EXIT then
    begin
     g_Game_ExitLevel(gTriggers[a].Data.MapName);
     Break;
    end;
 end
  else if Copy(charbuff, 8, 7) = c7 then
 begin
  s2 := Copy(charbuff, 15, 2);
  if g_Map_Exist(gGameSettings.WAD+':\MAP'+s2) then
  begin
   c := 'MAP00';
   c[3] := s2[1];
   c[4] := s2[2];
   g_Game_ExitLevel(c);
  end;
 end
  else if (Copy(charbuff, 9, 8) = c8) and
          (charbuff[7] = Chr(188)) then
 begin
  gFly := not gFly;
 end
  else if Copy(charbuff, 9, 8) = c9 then
 begin
  VEL_JUMP := 30-VEL_JUMP;
 end
  else if Copy(charbuff, 9, 8) = c10 then
 begin
  MAX_RUNVEL := 32-MAX_RUNVEL;
 end
 else Exit;

 g_Sound_PlayEx(s, 127, 255);
end;

procedure KeyPress(K: Byte);
var
  Msg: g_gui.TMessage;
  a: Integer;
begin
 if K = 19 then
   if (g_ActiveWindow = nil) then
     begin
       g_Game_Pause(not gPause);
     end;

 if (K = 192) then
   g_Console_Switch()
 else
   if gConsoleShow then
     g_Console_Control(K);

 if (K = 27) and (gConsoleShow) then
 begin
  gConsoleShow := False;
  Exit;
 end;

 if g_ActiveWindow <> nil then
 begin
  if not gConsoleShow then
  begin
   Msg.Msg := WM_KEYDOWN;
   Msg.WParam := K;
   g_ActiveWindow.OnMessage(Msg);
  end;
  Exit;
 end
  else if not gGameOn then
 begin
  if (K = 27) and (not gGameOn) and (gExit = 0) then
  begin
   g_GUI_ShowWindow('MainMenu');
   g_Sound_PlayEx('MENU_OPEN', 127, 255);
  end;
 end;

 if K = 27 then g_Game_InGameMenu(True);

 if not gConsoleShow then
 begin
  for a := 0 to 14 do charbuff[a] := charbuff[a+1];
  charbuff[15] := Chr(K);

  Cheat();
 end;
end;

procedure CharPress(C: Char);
var
  Msg: g_gui.TMessage;
begin
 if C = '`' then Exit;

 if gConsoleShow then g_Console_Char(C) else if g_ActiveWindow <> nil then
 begin
  Msg.Msg := WM_CHAR;
  Msg.WParam := Ord(C);
  g_ActiveWindow.OnMessage(Msg);
 end;
end;

end.
