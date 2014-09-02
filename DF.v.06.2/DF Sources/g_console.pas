unit g_console;

interface

procedure g_Console_Init();
procedure g_Console_Update();
procedure g_Console_Draw();
procedure g_Console_Switch();
procedure g_Console_Char(C: Char);
procedure g_Console_Control(K: Byte);
procedure g_Console_Process(L: String);
procedure g_Console_Add(L: String; Show: Boolean = False);
procedure g_Console_Clear();

procedure g_Console_Chat_Switch();

var
  gConsoleShow: Boolean; // True - консоль открыта или открывается
  gChatShow: Boolean;
  gAllowConsoleMessages: Boolean = True;
  gJustChatted: Boolean = False; // чтобы админ в интере чатясь не проматывал статистику

implementation

uses
  g_textures, g_main, windows, e_graphics, g_game,
  SysUtils, g_basic, g_options, WADEDITOR, Math,
  g_menu, g_language, g_net, g_netmsg;

type
  TCmdProc = procedure (P: SArray);

  TCommand = record
    Cmd: String;
    Proc: TCmdProc;
  end;

const
  Step = 32;
  Alpha = 25;
  MsgTime = 144;

  DEBUG_STRING = 'DEBUG MODE';

var
  ID: DWORD;
  Cons_Y: SmallInt;
  Cons_Shown: Boolean; // Рисовать ли консоль?
  Line: String;
  CPos: Word;
  ConsoleHistory: SArray;
  CommandHistory: SArray;
  Commands: Array of TCommand;
  CmdIndex: Word;
  Offset: Word;
  MsgArray: Array [0..4] of record
                              Msg: String;
                              Time: Word;
                            end;

procedure ConsoleCommands(P: SArray);
var
  Cmd, s: String;
  a: Integer;
  F: TextFile;
begin
  Cmd := LowerCase(P[0]);

  if Cmd = 'clear' then
  begin
    ConsoleHistory := nil;

    for a := 0 to High(MsgArray) do
      with MsgArray[a] do
      begin
        Msg := '';
        Time := 0;
      end;
  end;
 
  if Cmd = 'clearhistory' then
    CommandHistory := nil;

  if Cmd = 'showhistory' then
    if CommandHistory <> nil then
    begin
      g_Console_Add('');
      for a := 0 to High(CommandHistory) do
        g_Console_Add('  '+CommandHistory[a]);
    end;

  if Cmd = 'commands' then
  begin
    g_Console_Add('');
    g_Console_Add('Commands list:');
    for a := High(Commands) downto 0 do
      g_Console_Add('  '+Commands[a].Cmd);
  end;

  if Cmd = 'time' then
    g_Console_Add(TimeToStr(Now));

  if Cmd = 'date' then
    g_Console_Add(DateToStr(Now));

  if Cmd = 'echo' then
    if Length(P) > 1 then
      begin
        if P[1] = 'ololo' then
          gCheats := True
        else
          g_Console_Add(P[1]);
      end
    else
      g_Console_Add('');

  if Cmd = 'dump' then
  begin
    if ConsoleHistory <> nil then
    begin
      if Length(P) > 1 then
        s := P[1]
      else
        s := GameDir+'\console.txt';

      {$I-}
      AssignFile(F, s);
      Rewrite(F);
      if IOResult <> 0 then
      begin
        g_Console_Add(Format(_lc[I_CONSOLE_ERROR_WRITE], [s]));
        CloseFile(F);
        Exit;
      end;

      for a := 0 to High(ConsoleHistory) do
        WriteLn(F, ConsoleHistory[a]);

      CloseFile(F);
      g_Console_Add(Format(_lc[I_CONSOLE_DUMPED], [s]));
      {$I+}
    end;
  end;
end;

procedure AddCommand(Cmd: String; Proc: TCmdProc);
var
  a: Integer;

begin
  SetLength(Commands, Length(Commands)+1);
  a := High(Commands);
  Commands[a].Cmd := Cmd;
  Commands[a].Proc := Proc;
end;

procedure g_Console_Init();
var
  a: Integer;
  
begin
  g_Texture_CreateWAD(ID, GameWAD+':TEXTURES\CONSOLE');
  Cons_Y := -(gScreenHeight div 2);
  gConsoleShow := False;
  gChatShow := False;
  Cons_Shown := False;
  CPos := 1;

  for a := 0 to High(MsgArray) do
    with MsgArray[a] do
    begin
      Msg := '';
      Time := 0;
    end;

  AddCommand('quit', GameCommands);
  AddCommand('exit', GameCommands);
  AddCommand('pause', GameCommands);
  AddCommand('endgame', GameCommands);
  AddCommand('restart', GameCommands);
  AddCommand('p1_name', GameCommands);
  AddCommand('p2_name', GameCommands);
  AddCommand('p1_color', GameCommands);
  AddCommand('p2_color', GameCommands);
  AddCommand('g_showfps', GameCommands);
  AddCommand('g_showtime', GameCommands);
  AddCommand('g_showscore', GameCommands);
  AddCommand('g_showstat', GameCommands);
  AddCommand('g_showkillmsg', GameCommands);
  AddCommand('ffire', GameCommands);
  AddCommand('addbot', GameCommands);
  AddCommand('bot_add', GameCommands);
  AddCommand('bot_addlist', GameCommands);
  AddCommand('bot_addred', GameCommands);
  AddCommand('bot_addblue', GameCommands);
  AddCommand('bot_removeall', GameCommands);
  AddCommand('scorelimit', GameCommands);
  AddCommand('timelimit', GameCommands);
  AddCommand('clear', ConsoleCommands);
  AddCommand('clearhistory', ConsoleCommands);
  AddCommand('showhistory', ConsoleCommands);
  AddCommand('commands', ConsoleCommands);
  AddCommand('time', ConsoleCommands);
  AddCommand('date', ConsoleCommands);
  AddCommand('echo', ConsoleCommands);
  AddCommand('dump', ConsoleCommands);
  AddCommand('d_window', GameCommands);
  AddCommand('d_sounds', GameCommands);
  AddCommand('d_frames', GameCommands);
  AddCommand('d_winmsg', GameCommands);
  AddCommand('d_monoff', GameCommands);
  AddCommand('map', GameCommands);
  AddCommand('monster', GameCommands);
  AddCommand('say', GameCommands);
  AddCommand('changemap', GameCommands);
  AddCommand('nextmap', GameCommands);
  AddCommand('suicide', GameCommands);
  AddCommand('spectate', GameCommands);
  AddCommand('kick', GameCommands);
  AddCommand('connect', GameCommands);
  AddCommand('disconnect', GameCommands);
  AddCommand('reconnect', GameCommands);

  g_Console_Add(Format(_lc[I_CONSOLE_WELCOME], [GAME_VERSION]));
  g_Console_Add('');
end;

procedure g_Console_Update();
var
  a, b: Integer;
  
begin
  if Cons_Shown then
  begin
  // В процессе открытия:
    if gConsoleShow and (Cons_Y < 0) then
    begin
      Cons_Y := Cons_Y+Step;
    end;

  // В процессе закрытия:
    if (not gConsoleShow) and
       (Cons_Y > -(gScreenHeight div 2)) then
      Cons_Y := Cons_Y-Step;

  // Окончательно открылась:
    if Cons_Y > 0 then
      Cons_Y := 0;

  // Окончательно закрылась:
    if Cons_Y <= (-(gScreenHeight div 2)) then
    begin
      Cons_Y := -(gScreenHeight div 2);
      Cons_Shown := False;
    end;
  end;

  a := 0;
  while a <= High(MsgArray) do
  begin
    if MsgArray[a].Time > 0 then
    begin
      if MsgArray[a].Time = 1 then
        begin
          if a < High(MsgArray) then
          begin
            for b := a to High(MsgArray)-1 do
              MsgArray[b] := MsgArray[b+1];

            MsgArray[High(MsgArray)].Time := 0;

            a := a - 1;
          end;
        end
      else
        Dec(MsgArray[a].Time);
    end;

    a := a + 1;
  end;
end;

procedure g_Console_Draw();
var
  CWidth, CHeight: Byte;
  mfW, mfH: Word;
  a, b, c, d: Integer;
  
begin
  e_TextureFontGetSize(gStdFont, CWidth, CHeight);

  for a := 0 to High(MsgArray) do
    if MsgArray[a].Time > 0 then
      e_TextureFontPrintEx(0, CHeight*a, MsgArray[a].Msg,
        gStdFont, 255, 255, 255, 1, True);

  if not Cons_Shown then
  begin
    if gChatShow then
    begin
      e_TextureFontPrintEx(0, gScreenHeight - 16, 'say> ' + Line,
        gStdFont, 255, 255, 255, 1, True);
      e_TextureFontPrintEx((CPos + 4)*CWidth, gScreenHeight - 16, '_',
        gStdFont, 255, 255, 255, 1, True);
    end;
    Exit;
  end;

  if gDebugMode then
  begin
    e_CharFont_GetSize(gMenuFont, DEBUG_STRING, mfW, mfH);
    a := (gScreenWidth - 2*mfW) div 2;
    b := Cons_Y + ((gScreenHeight div 2) - 2*mfH) div 2;
    e_CharFont_PrintEx(gMenuFont, a div 2, b div 2, DEBUG_STRING,
                       _RGB(128, 0, 0), 2.0);
  end;

  e_DrawSize(ID, 0, Cons_Y, Alpha, False, False, gScreenWidth, gScreenHeight div 2);
  e_TextureFontPrint(0, Cons_Y+(gScreenHeight div 2)-CHeight, '>'+Line, gStdFont);

  if ConsoleHistory <> nil then
  begin
    if Length(ConsoleHistory) > ((gScreenHeight div 2) div CHeight)-1 then
      b := Length(ConsoleHistory)-((gScreenHeight div 2) div CHeight)+1
    else
      b := 0;

    b := Max(b-Offset, 0);
    d := Max(High(ConsoleHistory)-Offset, 0);

    c := 2;
    for a := d downto b do
    begin
      e_TextureFontPrintEx(0, (gScreenHeight div 2)-c*CHeight-Abs(Cons_Y), ConsoleHistory[a],
                           gStdFont, 240, 240, 240, 1, True);
      c := c + 1;
    end;
  end;

  e_TextureFontPrint(CPos*CWidth, Cons_Y+(gScreenHeight div 2)-17, '_', gStdFont);
end;

procedure g_Console_Switch();
begin
  if gChatShow then Exit;
  gConsoleShow := not gConsoleShow;
  Cons_Shown := True;
end;

procedure g_Console_Chat_Switch();
begin
  if gConsoleShow then Exit;
  if not g_Game_IsNet then Exit;
  gChatShow := not gChatShow;
  Line := '';
  CPos := 1;
end;

procedure g_Console_Char(C: Char);
begin
  Insert(C, Line, CPos);
  CPos := CPos + 1;
end;

procedure Complete();
var
  i: Integer;
  t: Array of String;

begin
  if Line = '' then
    Exit;

  t := nil;

  for i := 0 to High(Commands) do
    if LowerCase(Line) = LowerCase(Copy(Commands[i].Cmd, 0, Length(Line))) then
    begin
      SetLength(t, Length(t) + 1);
      t[Length(t)-1] := Commands[i].Cmd;
    end;

  if t = nil then
    Exit;

  if Length(t) = 1 then
    begin
      Line := t[0]+' ';
      CPos := Length(Line)+1;
    end
  else
    begin
      g_Console_Add('');
      for i := 0 to High(t) do
        g_Console_Add('  '+t[i]);
    end;
end;

procedure g_Console_Control(K: Byte);
begin
  case K of
    VK_BACK:
      if Length(Line) > 0 then
      begin
        Delete(Line, CPos-1, 1);
        CPos := CPos-1;
      end;
    VK_DELETE:
      if (Length(Line) > 0) and (CPos <= Length(Line)) then
        Delete(Line, CPos, 1);
    VK_LEFT:
      if CPos > 1 then
        CPos := CPos - 1;
    VK_RIGHT:
      if CPos <= Length(Line) then
        CPos := CPos + 1;
    VK_RETURN:
    begin
      if Cons_Shown then
        g_Console_Process(Line)
      else
        if gChatShow then
        begin
          if (Length(Line) > 0) and g_Game_IsNet then
          begin
            if g_Game_IsClient then
              MC_SEND_Chat(Line)
            else
              MH_SEND_Chat('[' + gPlayer1Settings.Name + ']: ' + Line);
          end;

          Line := '';
          CPos := 1;
          gChatShow := False;
          gJustChatted := True;
        end;
    end;
    VK_TAB:
     if not gChatShow then
      Complete();
    VK_DOWN:
     if not gChatShow then
      if (CommandHistory <> nil) and
         (CmdIndex < Length(CommandHistory)) then
      begin
        if CmdIndex < Length(CommandHistory)-1 then
          CmdIndex := CmdIndex + 1;
        Line := CommandHistory[CmdIndex];
        CPos := Length(Line) + 1;
      end;
    VK_UP:
     if not gChatShow then
      if (CommandHistory <> nil) and
         (CmdIndex <= Length(CommandHistory)) then
      begin
        if CmdIndex > 0 then
          CmdIndex := CmdIndex - 1;
        Line := CommandHistory[CmdIndex];
        Cpos := Length(Line) + 1;
      end;
    VK_PRIOR: // PgUp
     if not gChatShow then
      IncMax(OffSet, Length(ConsoleHistory)-1);
    VK_NEXT: // PgDown
     if not gChatShow then
      DecMin(OffSet, 0);
    VK_HOME:
      CPos := 1;
    VK_END:
      CPos := Length(Line) + 1;
  end;
end;

function GetStr(var Str: String): String;
var
  a, b: Integer;

begin
  if Str[1] = '"' then
  begin
    for b := 1 to Length(Str) do
      if (b = Length(Str)) or (Str[b+1] = '"') then
      begin
        Result := Copy(Str, 2, b-1);
        Delete(Str, 1, b+1);
        Str := Trim(Str);
        Exit;
      end;
  end;

  for a := 1 to Length(Str) do
    if (a = Length(Str)) or (Str[a+1] = ' ') then
    begin
      Result := Copy(Str, 1, a);
      Delete(Str, 1, a+1);
      Str := Trim(Str);
      Exit;
    end;
end;

function ParseString(Str: String): SArray;
begin
  Result := nil;

  Str := Trim(Str);

  if Str = '' then
    Exit;

  while Str <> '' do
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := GetStr(Str);
  end;
end;

procedure g_Console_Add(L: String; Show: Boolean = False);
var
  a: Integer;

begin
  SetLength(ConsoleHistory, Length(ConsoleHistory)+1);
  ConsoleHistory[High(ConsoleHistory)] := L;

  Show := Show and gAllowConsoleMessages;

  if Show and gShowMessages then
  begin
    for a := 0 to High(MsgArray) do
      with MsgArray[a] do
        if Time = 0 then
        begin
          Msg := L;
          Time := MsgTime;
          Exit;
        end;

    for a := 0 to High(MsgArray)-1 do
      MsgArray[a] := MsgArray[a+1];

    with MsgArray[High(MsgArray)] do
    begin
      Msg := L;
      Time := MsgTime;
    end;
  end;
end;

procedure g_Console_Clear();
begin
  ConsoleHistory := nil;
  Offset := 0;
end;

procedure AddToHistory(L: String);
var
  len: Integer;

begin
  len := Length(CommandHistory);

  if (len = 0) or
     (LowerCase(CommandHistory[len-1]) <> LowerCase(L)) then
  begin
    SetLength(CommandHistory, len+1);
    CommandHistory[len] := L;
  end;

  CmdIndex := Length(CommandHistory);
end;

procedure g_Console_Process(L: String);
var
  Arr: SArray;
  i: Integer;

begin
  Arr := nil;

  g_Console_Add(L);

  if L = '' then
    Exit;

  Arr := ParseString(L);

  Line := '';
  CPos := 1;

  if Arr = nil then
    Exit;

  if Commands = nil then
    Exit;

  AddToHistory(L);

  for i := 0 to High(Commands) do
    if Commands[i].Cmd = LowerCase(Arr[0]) then
      if @Commands[i].Proc <> nil then
      begin
        Commands[i].Proc(Arr);
        Exit;
      end;

  g_Console_Add(Format(_lc[I_CONSOLE_UNKNOWN], [Arr[0]]));
end;

end.
