unit g_console;

interface

procedure g_Console_Init();
procedure g_Console_Update();
procedure g_Console_Draw();
procedure g_Console_Switch();
procedure g_Console_Char(C: Char);
procedure g_Console_Control(K: Byte);
procedure g_Console_Process(L: string);
procedure g_Console_Add(L: string; Show: Boolean = False);
procedure g_Console_Clear();

var
  gConsoleShow: Boolean;
  gAllowConsoleMessages: Boolean = True;

implementation

uses g_textures, g_main, windows, e_graphics, g_game, SysUtils,
     g_basic, inter, g_options, WADEDITOR, Math;

type
  TCmdProc = procedure(P: SArray);

  TCommand = record
   Cmd: string;
   Proc: TCmdProc;
  end;

const
  Step = 16;
  Alpha = 150;
  MsgTime = 144;

var
  ID: DWORD;
  Y: SmallInt;
  Line: string;
  CPos: Word;
  ConsoleHistory: SArray;
  CommandHistory: SArray;
  Commands: array of TCommand;
  CmdIndex: Word;
  Offset: Word;
  MsgArray: array[0..4] of record
                            Msg: string;
                            Time: Word;
                           end;

procedure ConsoleCommands(P: SArray);
var
  Cmd: string;
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
 
 if Cmd = 'clearhistory' then CommandHistory := nil;

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

 if Cmd = 'time' then g_Console_Add(TimeToStr(Now));
 if Cmd = 'date' then g_Console_Add(DateToStr(Now));
 if Cmd = 'echo' then if Length(P) > 1 then
 begin
  if P[1] = 'ololo' then gCheats := True else g_Console_Add(P[1]);
 end else g_Console_Add('');

 if Cmd = 'dump' then
  if ConsoleHistory <> nil then
  begin
   if Length(P) > 1 then
   begin
    {$I-}
    AssignFile(F, P[1]);
    Rewrite(F);
    if IOResult <> 0 then
    begin
     g_Console_Add(Format(I_CONSOLE_ERRORWRITE, [P[1]]));
     CloseFile(F);
     Exit;
    end;
    for a := 0 to High(ConsoleHistory) do
     WriteLn(F, ConsoleHistory[a]);
    CloseFile(F);
    g_Console_Add(Format(I_CONSOLE_DUMPED, [P[1]]));
    {$I+}
   end
    else
   begin
    {$I-}
    AssignFile(F, GameDir+'\console.txt');
    Rewrite(F);
    if IOResult <> 0 then
    begin
     g_Console_Add(Format(I_CONSOLE_ERRORWRITE, [GameDir+'\console.txt']));
     CloseFile(F);
     Exit;
    end;
    for a := 0 to High(ConsoleHistory) do
     WriteLn(F, ConsoleHistory[a]);
    CloseFile(F);
    g_Console_Add(Format(I_CONSOLE_DUMPED, [GameDir+'\console.txt']));
   {$I+}
   end;
  end;
end;

procedure AddCommand(Cmd: string; Proc: TCmdProc);
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
 Y := -(gScreenHeight div 2);
 gConsoleShow := False;
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
 AddCommand('map', GameCommands);
 AddCommand('p1_name', GameCommands);
 AddCommand('p2_name', GameCommands);
 AddCommand('p1_color', GameCommands);
 AddCommand('p2_color', GameCommands);
 AddCommand('g_showfps', GameCommands);
 AddCommand('g_showtime', GameCommands);
 AddCommand('g_showgoals', GameCommands);
 AddCommand('g_showstat', GameCommands);
 AddCommand('g_showkillmsg', GameCommands);
 //AddCommand('monster', GameCommands);
 AddCommand('addbot', GameCommands);
 AddCommand('bot_add', GameCommands);
 AddCommand('bot_addlist', GameCommands);
 AddCommand('bot_addred', GameCommands);
 AddCommand('bot_addblue', GameCommands);
 AddCommand('bot_removeall', GameCommands);
 AddCommand('fraglimit', GameCommands);
 AddCommand('clear', ConsoleCommands);
 AddCommand('clearhistory', ConsoleCommands);
 AddCommand('showhistory', ConsoleCommands);
 AddCommand('commands', ConsoleCommands);
 AddCommand('time', ConsoleCommands);
 AddCommand('date', ConsoleCommands);
 AddCommand('echo', ConsoleCommands);
 AddCommand('dump', ConsoleCommands);

 g_Console_Add(Format(I_CONSOLE_WELCOME, [GAME_VERSION]));
 g_Console_Add('');
end;

procedure g_Console_Update();
var
  a, b: Integer;
begin
 if gConsoleShow and (Y < 0) then Y := Y+Step;
 if not gConsoleShow and (Y > -(gScreenHeight div 2)) then Y := Y-Step;
 if Y > 0 then Y := 0;
 if Y < -(gScreenHeight div 2) then Y := -(gScreenHeight div 2);

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

     a := a-1;
    end;
   end else Dec(MsgArray[a].Time);
  end;

  a := a+1;
 end;
end;

procedure g_Console_Draw();
var
  CWidth: Byte;
  CHeight: Byte;
  a, b, c, d: Integer;
begin
 e_TextureFontGetSize(gStdFont, CWidth, CHeight);

 for a := 0 to High(MsgArray) do
  if MsgArray[a].Time > 0 then
   e_TextureFontPrint(0, CHeight*a, MsgArray[a].Msg, gStdFont);

 if Y = -(gScreenHeight div 2) then Exit;

 e_DrawSize(ID, 0, Y, Alpha, False, False, gScreenWidth, gScreenHeight div 2);
 e_TextureFontPrint(0, Y+(gScreenHeight div 2)-CHeight, '>'+Line, gStdFont);

 if ConsoleHistory <> nil then
 begin
  if Length(ConsoleHistory) > ((gScreenHeight div 2) div CHeight)-1 then
   b := Length(ConsoleHistory)-((gScreenHeight div 2) div CHeight)+1
    else b := 0;

  b := Max(b-Offset, 0);
  d := Max(High(ConsoleHistory)-Offset, 0);

  c := 2;
  for a := d downto b do
  begin
   e_TextureFontPrintEx(0, (gScreenHeight div 2)-c*CHeight-Abs(Y), ConsoleHistory[a],
                        gStdFont, 200, 200, 200, 1.0);
   c := c+1;
  end;
 end;

 e_TextureFontPrint(CPos*CWidth, Y+(gScreenHeight div 2)-17, '_', gStdFont);
end;

procedure g_Console_Switch();
begin
 gConsoleShow := not gConsoleShow;
 Line := '';
 CPos := 1;
 if gConsoleShow then Offset := 0;
end;

procedure g_Console_Char(C: Char);
begin
 Insert(C, Line, CPos);
 CPos := CPos+1;
end;

procedure Complete();
var
  i: Integer;
  t: array of string;
begin
 if Line = '' then Exit;

 t := nil;

 for i := 0 to High(Commands) do
  if LowerCase(Line) = LowerCase(Copy(Commands[i].Cmd, 0, Length(Line))) then
  begin
   SetLength(t, Length(t) + 1);
   t[Length(t)-1] := Commands[i].Cmd;
  end;

 if t = nil then Exit;

 if Length(t) = 1 then
 begin
  Line := t[0]+' ';
  CPos := Length(Line)+1;
 end
  else
 begin
  g_Console_Add('');
  for i := 0 to High(t) do g_Console_Add('  '+t[i]);
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
  VK_DELETE: if (Length(Line) > 0) and (CPos <= Length(Line)) then Delete(Line, CPos, 1);
  VK_LEFT: if CPos > 1 then CPos := CPos - 1;
  VK_RIGHT: if CPos <= Length(Line) then CPos := CPos+1;
  VK_RETURN: g_Console_Process(Line);
  VK_TAB: Complete();
  VK_DOWN:
   if (CommandHistory <> nil) and (CmdIndex < Length(CommandHistory)) then
   begin
    if CmdIndex < Length(CommandHistory)-1 then CmdIndex := CmdIndex+1;
    Line := CommandHistory[CmdIndex];
    CPos := Length(Line)+1;
   end;
  VK_UP:
   if (CommandHistory <> nil) and (CmdIndex <= Length(CommandHistory)) then
   begin
    if CmdIndex > 0 then CmdIndex := CmdIndex-1;
    Line := CommandHistory[CmdIndex];
    Cpos := Length(Line)+1;
   end;
  VK_PRIOR: IncMax(OffSet, Length(ConsoleHistory)-1);
  VK_NEXT: DecMin(OffSet, 0);
  VK_HOME: CPos := 1;
  VK_END: CPos := Length(Line)+1;
 end;
end;

function GetStr(var Str: string): string;
var
  a, b: Integer;
begin
 if Str[1] = '"' then
  begin
   for b := 1 to Length(Str) do
    if (Str[b+1] = '"') or (b = Length(Str)) then
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

function ParseString(Str: string): SArray;
begin
 Result := nil;

 Str := Trim(Str);

 if Str = '' then Exit;

 while Str <> '' do
 begin
  SetLength(Result, Length(Result)+1);
  Result[High(Result)] := GetStr(Str);
 end;
end;

procedure g_Console_Add(L: string; Show: Boolean = False);
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

procedure AddToHistory(L: string);
begin
 SetLength(CommandHistory, Length(CommandHistory)+1);
 CommandHistory[High(CommandHistory)] := L;

 CmdIndex := Length(CommandHistory);
end;

procedure g_Console_Process(L: string);
var
 Arr: SArray;
 i: Integer;
begin
 Arr := nil;

 g_Console_Add(L);

 if L = '' then Exit;

 Arr := ParseString(L);

 Line := '';
 CPos := 1;

 if Arr = nil then Exit;
 if Commands = nil then Exit;

 AddToHistory(L);

 for i := 0 to High(Commands) do
  if Commands[i].Cmd = LowerCase(Arr[0]) then
   if @Commands[i].Proc <> nil then
   begin
    Commands[i].Proc(Arr);
    Exit;
   end;

 g_Console_Add(Format(I_CONSOLE_UNKNOWN, [Arr[0]]));
end;

end.
