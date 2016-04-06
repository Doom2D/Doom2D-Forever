unit g_console;

interface

procedure g_Console_Init();
procedure g_Console_Update();
procedure g_Console_Draw();
procedure g_Console_Switch();
procedure g_Console_Char(C: Char);
procedure g_Console_Control(K: Word);
procedure g_Console_Process(L: String; Quiet: Boolean = False);
procedure g_Console_Add(L: String; Show: Boolean = False);
procedure g_Console_Clear();
function  g_Console_CommandBlacklisted(C: String): Boolean;

procedure g_Console_Chat_Switch(Team: Boolean = False);

var
  gConsoleShow: Boolean; // True - консоль открыта или открывается
  gChatShow: Boolean;
  gChatTeam: Boolean = False;
  gAllowConsoleMessages: Boolean = True;
  gChatEnter: Boolean = True;
  gJustChatted: Boolean = False; // чтобы админ в интере чатясь не проматывал статистику

implementation

uses
  g_textures, g_main, e_graphics, e_input, g_game,
  SysUtils, g_basic, g_options, WADEDITOR, Math,
  g_menu, g_language, g_net, g_netmsg;

type
  TCmdProc = procedure (P: SArray);

  TCommand = record
    Cmd: String;
    Proc: TCmdProc;
  end;

  TAlias = record
    Name: String;
    Commands: SArray;
  end;

const
  Step = 32;
  Alpha = 25;
  MsgTime = 144;
  MaxScriptRecursion = 16;

  DEBUG_STRING = 'DEBUG MODE';

var
  ID: DWORD;
  RecursionDepth: Word = 0;
  RecursionLimitHit: Boolean = False;
  Cons_Y: SmallInt;
  Cons_Shown: Boolean; // Рисовать ли консоль?
  Line: String;
  CPos: Word;
  ConsoleHistory: SArray;
  CommandHistory: SArray;
  Whitelist: SArray;
  Commands: Array of TCommand;
  Aliases: Array of TAlias;
  CmdIndex: Word;
  Offset: Word;
  MsgArray: Array [0..4] of record
                              Msg: String;
                              Time: Word;
                            end;

function GetStrACmd(var Str: String): String;
var
  a: Integer;
begin
  Result := '';
  for a := 1 to Length(Str) do
    if (a = Length(Str)) or (Str[a+1] = ';') then
    begin
      Result := Copy(Str, 1, a);
      Delete(Str, 1, a+1);
      Str := Trim(Str);
      Exit;
    end;
end;

function ParseAlias(Str: String): SArray;
begin
  Result := nil;

  Str := Trim(Str);

  if Str = '' then
    Exit;

  while Str <> '' do
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := GetStrACmd(Str);
  end;
end;

procedure ConsoleCommands(P: SArray);
var
  Cmd, s: String;
  a, b: Integer;
  F: TextFile;
begin
  Cmd := LowerCase(P[0]);
  s := '';

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
    g_Console_Add(TimeToStr(Now), True);

  if Cmd = 'date' then
    g_Console_Add(DateToStr(Now), True);

  if Cmd = 'echo' then
    if Length(P) > 1 then
      begin
        if P[1] = 'ololo' then
          gCheats := True
        else
        begin
          s := '';
          for a := 1 to High(P) do
            s := s + P[a] + ' ';
          g_Console_Add(b_Text_Format(s), True);
        end;
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
        s := GameDir+'/console.txt';

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

  if Cmd = 'exec' then
  begin
    // exec <filename>
    if Length(P) > 1 then
    begin
      s := GameDir+'/'+P[1];

      {$I-}
      AssignFile(F, s);
      Reset(F);
      if IOResult <> 0 then
      begin
        g_Console_Add(Format(_lc[I_CONSOLE_ERROR_READ], [s]));
        CloseFile(F);
        Exit;
      end;
      g_Console_Add(Format(_lc[I_CONSOLE_EXEC], [s]));

      while not EOF(F) do
      begin
        ReadLn(F, s);
        if IOResult <> 0 then
        begin
          g_Console_Add(Format(_lc[I_CONSOLE_ERROR_READ], [s]));
          CloseFile(F);
          Exit;
        end;
        if Pos('#', s) <> 1 then // script comment
        begin
          // prevents endless loops
          Inc(RecursionDepth);
          RecursionLimitHit := (RecursionDepth > MaxScriptRecursion) or RecursionLimitHit;
          if not RecursionLimitHit then
            g_Console_Process(s, True);
          Dec(RecursionDepth);
        end;
      end;
      if (RecursionDepth = 0) and RecursionLimitHit then
      begin
        g_Console_Add(Format(_lc[I_CONSOLE_ERROR_CALL], [s]));
        RecursionLimitHit := False;
      end;

      CloseFile(F);
      {$I+}
    end
    else
      g_Console_Add('exec <script file>');
  end;

  if Cmd = 'alias' then
  begin
    // alias [alias_name] [commands]
    if Length(P) > 1 then
    begin
      for a := 0 to High(Aliases) do
        if Aliases[a].Name = P[1] then
        begin
          if Length(P) > 2 then
            Aliases[a].Commands := ParseAlias(P[2])
          else
            for b := 0 to High(Aliases[a].Commands) do
              g_Console_Add(Aliases[a].Commands[b]);
          Exit;
        end;
      SetLength(Aliases, Length(Aliases)+1);
      a := High(Aliases);
      Aliases[a].Name := P[1];
      if Length(P) > 2 then
        Aliases[a].Commands := ParseAlias(P[2])
      else
        for b := 0 to High(Aliases[a].Commands) do
          g_Console_Add(Aliases[a].Commands[b]);
    end else
      for a := 0 to High(Aliases) do
        if Aliases[a].Commands <> nil then
          g_Console_Add(Aliases[a].Name);
  end;

  if Cmd = 'call' then
  begin
    // call <alias_name>
    if Length(P) > 1 then
    begin
      if Aliases = nil then
        Exit;
      for a := 0 to High(Aliases) do
        if Aliases[a].Name = P[1] then
        begin
          if Aliases[a].Commands <> nil then
          begin
            // with this system proper endless loop detection seems either impossible
            // or very dirty to implement, so let's have this instead
            // prevents endless loops
            for b := 0 to High(Aliases[a].Commands) do
            begin
              Inc(RecursionDepth);
              RecursionLimitHit := (RecursionDepth > MaxScriptRecursion) or RecursionLimitHit;
              if not RecursionLimitHit then
                g_Console_Process(Aliases[a].Commands[b], True);
              Dec(RecursionDepth);
            end;
            if (RecursionDepth = 0) and RecursionLimitHit then
            begin
              g_Console_Add(Format(_lc[I_CONSOLE_ERROR_CALL], [s]));
              RecursionLimitHit := False;
            end;
          end;
          Exit;
        end;
    end
    else
      g_Console_Add('call <alias name>');
  end;
end;

procedure WhitelistCommand(Cmd: string);
var
  a: Integer;
begin
  SetLength(Whitelist, Length(Whitelist)+1);
  a := High(Whitelist);
  Whitelist[a] := Cmd;
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

  AddCommand('clear', ConsoleCommands);
  AddCommand('clearhistory', ConsoleCommands);
  AddCommand('showhistory', ConsoleCommands);
  AddCommand('commands', ConsoleCommands);
  AddCommand('time', ConsoleCommands);
  AddCommand('date', ConsoleCommands);
  AddCommand('echo', ConsoleCommands);
  AddCommand('dump', ConsoleCommands);
  AddCommand('exec', ConsoleCommands);
  AddCommand('alias', ConsoleCommands);
  AddCommand('call', ConsoleCommands);

  AddCommand('d_window', DebugCommands);
  AddCommand('d_sounds', DebugCommands);
  AddCommand('d_frames', DebugCommands);
  AddCommand('d_winmsg', DebugCommands);
  AddCommand('d_monoff', DebugCommands);
  AddCommand('d_botoff', DebugCommands);
  AddCommand('d_monster', DebugCommands);
  AddCommand('d_health', DebugCommands);
  AddCommand('d_player', DebugCommands);
  AddCommand('d_joy', DebugCommands);

  AddCommand('p1_name', GameCVars);
  AddCommand('p2_name', GameCVars);
  AddCommand('p1_color', GameCVars);
  AddCommand('p2_color', GameCVars);
  AddCommand('r_showfps', GameCVars);
  AddCommand('r_showtime', GameCVars);
  AddCommand('r_showscore', GameCVars);
  AddCommand('r_showlives', GameCVars);
  AddCommand('r_showstat', GameCVars);
  AddCommand('r_showkillmsg', GameCVars);
  AddCommand('r_showspect', GameCVars);
  AddCommand('r_showping', GameCVars);
  AddCommand('g_gamemode', GameCVars);
  AddCommand('g_friendlyfire', GameCVars);
  AddCommand('g_weaponstay', GameCVars);
  AddCommand('g_allow_exit', GameCVars);
  AddCommand('g_allow_monsters', GameCVars);
  AddCommand('g_bot_vsmonsters', GameCVars);
  AddCommand('g_bot_vsplayers', GameCVars);
  AddCommand('g_scorelimit', GameCVars);
  AddCommand('g_timelimit', GameCVars);
  AddCommand('g_maxlives', GameCVars);
  AddCommand('g_warmuptime', GameCVars);
  AddCommand('net_interp', GameCVars);
  AddCommand('net_forceplayerupdate', GameCVars);
  AddCommand('net_predictself', GameCVars);
  AddCommand('sv_name', GameCVars);
  AddCommand('sv_passwd', GameCVars);
  AddCommand('sv_maxplrs', GameCVars);
  AddCommand('sv_public', GameCVars);
  AddCommand('sv_intertime', GameCVars);

  AddCommand('quit', GameCommands);
  AddCommand('exit', GameCommands);
  AddCommand('pause', GameCommands);
  AddCommand('endgame', GameCommands);
  AddCommand('restart', GameCommands);
  AddCommand('addbot', GameCommands);
  AddCommand('bot_add', GameCommands);
  AddCommand('bot_addlist', GameCommands);
  AddCommand('bot_addred', GameCommands);
  AddCommand('bot_addblue', GameCommands);
  AddCommand('bot_removeall', GameCommands);
  AddCommand('chat', GameCommands);
  AddCommand('teamchat', GameCommands);
  AddCommand('game', GameCommands);
  AddCommand('host', GameCommands);
  AddCommand('map', GameCommands);
  AddCommand('nextmap', GameCommands);
  AddCommand('endmap', GameCommands);
  AddCommand('goodbye', GameCommands);
  AddCommand('suicide', GameCommands);
  AddCommand('spectate', GameCommands);
  AddCommand('ready', GameCommands);
  AddCommand('kick', GameCommands);
  AddCommand('kick_id', GameCommands);
  AddCommand('ban', GameCommands);
  AddCommand('permban', GameCommands);
  AddCommand('ban_id', GameCommands);
  AddCommand('permban_id', GameCommands);
  AddCommand('unban', GameCommands);
  AddCommand('connect', GameCommands);
  AddCommand('disconnect', GameCommands);
  AddCommand('reconnect', GameCommands);
  AddCommand('say', GameCommands);
  AddCommand('tell', GameCommands);
  AddCommand('overtime', GameCommands);
  AddCommand('rcon_password', GameCommands);
  AddCommand('rcon', GameCommands);
  AddCommand('callvote', GameCommands);
  AddCommand('vote', GameCommands);
  AddCommand('clientlist', GameCommands);
  AddCommand('event', GameCommands);

  WhitelistCommand('say');
  WhitelistCommand('tell');
  WhitelistCommand('overtime');
  WhitelistCommand('ready');
  WhitelistCommand('map');
  WhitelistCommand('nextmap');
  WhitelistCommand('endmap');
  WhitelistCommand('restart');
  WhitelistCommand('kick');
  WhitelistCommand('ban');

  WhitelistCommand('addbot');
  WhitelistCommand('bot_add');
  WhitelistCommand('bot_addred');
  WhitelistCommand('bot_addblue');
  WhitelistCommand('bot_removeall');

  WhitelistCommand('g_gamemode');
  WhitelistCommand('g_friendlyfire');
  WhitelistCommand('g_weaponstay');
  WhitelistCommand('g_allow_exit');
  WhitelistCommand('g_allow_monsters');
  WhitelistCommand('g_scorelimit');
  WhitelistCommand('g_timelimit');

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
      e_TextureFontPrintFmt(0, CHeight*a, MsgArray[a].Msg,
        gStdFont, True);

  if not Cons_Shown then
  begin
    if gChatShow then
    begin
      if gChatTeam then
      begin
        e_TextureFontPrintEx(0, gScreenHeight - CHeight - 1, 'say team> ' + Line,
          gStdFont, 255, 255, 255, 1, True);
        e_TextureFontPrintEx((CPos + 9)*CWidth, gScreenHeight - CHeight - 1, '_',
          gStdFont, 255, 255, 255, 1, True);
      end
      else
      begin
        e_TextureFontPrintEx(0, gScreenHeight - CHeight - 1, 'say> ' + Line,
          gStdFont, 255, 255, 255, 1, True);
        e_TextureFontPrintEx((CPos + 4)*CWidth, gScreenHeight - CHeight - 1, '_',
          gStdFont, 255, 255, 255, 1, True);
      end;
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
  e_TextureFontPrint(0, Cons_Y+(gScreenHeight div 2)-CHeight-4, '> '+Line, gStdFont);

  if ConsoleHistory <> nil then
  begin
    b := 0;
    if CHeight > 0 then
      if Length(ConsoleHistory) > ((gScreenHeight div 2) div CHeight)-1 then
        b := Length(ConsoleHistory)-((gScreenHeight div 2) div CHeight)+1;

    b := Max(b-Offset, 0);
    d := Max(High(ConsoleHistory)-Offset, 0);

    c := 2;
    for a := d downto b do
    begin
      e_TextureFontPrintFmt(0, (gScreenHeight div 2)-4-c*CHeight-Abs(Cons_Y), ConsoleHistory[a],
                           gStdFont, True);
      c := c + 1;
    end;
  end;

  e_TextureFontPrint((CPos+1)*CWidth, Cons_Y+(gScreenHeight div 2)-21, '_', gStdFont);
end;

procedure g_Console_Switch();
begin
  if gChatShow then Exit;
  gConsoleShow := not gConsoleShow;
  Cons_Shown := True;
end;

procedure g_Console_Chat_Switch(Team: Boolean = False);
begin
  if gConsoleShow then Exit;
  if not g_Game_IsNet then Exit;
  gChatShow := not gChatShow;
  gChatTeam := Team;
  if gChatShow then
    gChatEnter := False;
  Line := '';
  CPos := 1;
end;

procedure g_Console_Char(C: Char);
begin
  if gChatShow and (not gChatEnter) then
    Exit;
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

procedure g_Console_Control(K: Word);
begin
  case K of
    IK_BACKSPACE:
      if (Length(Line) > 0) and (CPos > 1) then
      begin
        Delete(Line, CPos-1, 1);
        CPos := CPos-1;
      end;
    IK_DELETE:
      if (Length(Line) > 0) and (CPos <= Length(Line)) then
        Delete(Line, CPos, 1);
    IK_LEFT, IK_KPLEFT:
      if CPos > 1 then
        CPos := CPos - 1;
    IK_RIGHT, IK_KPRIGHT:
      if CPos <= Length(Line) then
        CPos := CPos + 1;
    IK_RETURN, IK_KPRETURN:
    begin
      if Cons_Shown then
        g_Console_Process(Line)
      else
        if gChatShow then
        begin
          if (Length(Line) > 0) and g_Game_IsNet then
          begin
            if gChatTeam then
            begin
              if g_Game_IsClient then
                MC_SEND_Chat(b_Text_Format(Line), NET_CHAT_TEAM)
              else
                MH_SEND_Chat('[' + gPlayer1Settings.Name + ']: ' + b_Text_Format(Line),
                  NET_CHAT_TEAM, gPlayer1Settings.Team);
            end
            else
            begin
              if g_Game_IsClient then
                MC_SEND_Chat(b_Text_Format(Line), NET_CHAT_PLAYER)
              else
                MH_SEND_Chat('[' + gPlayer1Settings.Name + ']: ' + b_Text_Format(Line),
                NET_CHAT_PLAYER);
            end;
          end;

          Line := '';
          CPos := 1;
          gChatShow := False;
          gJustChatted := True;
        end;
    end;
    IK_TAB:
      if not gChatShow then
        Complete();
    IK_DOWN, IK_KPDOWN:
      if not gChatShow then
        if (CommandHistory <> nil) and
           (CmdIndex < Length(CommandHistory)) then
        begin
          if CmdIndex < Length(CommandHistory)-1 then
            CmdIndex := CmdIndex + 1;
          Line := CommandHistory[CmdIndex];
          CPos := Length(Line) + 1;
        end;
    IK_UP, IK_KPUP:
      if not gChatShow then
        if (CommandHistory <> nil) and
           (CmdIndex <= Length(CommandHistory)) then
        begin
          if CmdIndex > 0 then
            CmdIndex := CmdIndex - 1;
          Line := CommandHistory[CmdIndex];
          Cpos := Length(Line) + 1;
        end;
    IK_PAGEUP, IK_KPPAGEUP: // PgUp
      if not gChatShow then
        IncMax(OffSet, Length(ConsoleHistory)-1);
    IK_PAGEDN, IK_KPPAGEDN: // PgDown
      if not gChatShow then
        DecMin(OffSet, 0);
    IK_HOME, IK_KPHOME:
      CPos := 1;
    IK_END, IK_KPEND:
      CPos := Length(Line) + 1;
  end;
end;

function GetStr(var Str: String): String;
var
  a, b: Integer;
begin
  Result := '';
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
  // Вывод строк с переносами по очереди
  while Pos(#10, L) > 0 do
  begin
    g_Console_Add(Copy(L, 1, Pos(#10, L) - 1), Show);
    Delete(L, 1, Pos(#10, L));
  end;

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

function g_Console_CommandBlacklisted(C: String): Boolean;
var
  Arr: SArray;
  i: Integer;
begin
  Result := True;

  Arr := nil;

  if Trim(C) = '' then
    Exit;

  Arr := ParseString(C);
  if Arr = nil then
    Exit;

  for i := 0 to High(Whitelist) do
    if Whitelist[i] = LowerCase(Arr[0]) then
      Result := False;
end;

procedure g_Console_Process(L: String; Quiet: Boolean = False);
var
  Arr: SArray;
  i: Integer;
begin
  Arr := nil;

  if Trim(L) = '' then
    Exit;

  if not Quiet then
  begin
    g_Console_Add('> '+L);
    Line := '';
    CPos := 1;
  end;

  Arr := ParseString(L);
  if Arr = nil then
    Exit;

  if Commands = nil then
    Exit;

  if not Quiet then
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
