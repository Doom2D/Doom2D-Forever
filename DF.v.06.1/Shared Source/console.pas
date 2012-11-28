unit console;

interface

procedure print(Text: string);

implementation

uses
  windows;

procedure print(Text: string);
var
  a, len: Integer;
  attr: Word;
  h: LongWord;
begin
 h := GetStdHandle(STD_OUTPUT_HANDLE);

 attr := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE;
 SetConsoleTextAttribute(h, attr);

 len := Length(Text);

 for a := 1 to len do
 begin
  if Text[a] = '^' then Continue;
  if Byte(Text[a]) = 7 then Continue;

  if (a = 1) or (Text[a-1] <> '^') then
  begin
   write(Text[a]);
   Continue;
  end;

  if a < len then
  begin
   case Text[a] of
    '1': attr := FOREGROUND_RED;
    '2': attr := FOREGROUND_GREEN;
    '3': attr := FOREGROUND_BLUE;
    '4': attr := FOREGROUND_BLUE or FOREGROUND_RED;
    '5': attr := FOREGROUND_GREEN or FOREGROUND_RED;
    '6': attr := FOREGROUND_BLUE or FOREGROUND_GREEN;
    '7': attr := FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED;
    else Continue;
   end;

   if (a > 2) and (Text[a-2] = '^') then attr := attr or FOREGROUND_INTENSITY;

   SetConsoleTextAttribute(h, attr)
  end;
 end;

 WriteLn;

 SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
end;

end.
