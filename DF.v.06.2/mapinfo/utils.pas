unit utils;

interface

function tostr(i: Integer): string;
procedure print(s: string; nolog: Boolean = False);
procedure CreateLog(FileName: string);
procedure CloseLog();

var
  log: Boolean = False;
  logfile: TextFile;

implementation

uses windows, SysUtils;

function tostr(i: Integer): string;
begin
 Str(i, Result);
end;

procedure print(s: string; nolog: Boolean = False);
var
  a, b: Integer;
  attr: Word;
  s2: string;
begin
 attr := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE;
 SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), attr);

 for a := 1 to Length(s) do
  if s[a] = '^' then Continue else
   if s[a-1] <> '^' then write(s[a]) else
    if a < Length(s) then
    begin
     case s[a] of
      '1': attr := FOREGROUND_RED;
      '2': attr := FOREGROUND_GREEN;
      '3': attr := FOREGROUND_BLUE;
      '4': attr := FOREGROUND_BLUE or FOREGROUND_RED;
      '5': attr := FOREGROUND_GREEN or FOREGROUND_RED;
      '6': attr := FOREGROUND_BLUE or FOREGROUND_GREEN;
      '7': attr := FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED;
     end;

     if (a >= 3) and (s[a-2] = '^') then attr := attr or FOREGROUND_INTENSITY;

     SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), attr)
    end;

 WriteLn;

 if log and not nolog then
 begin
  SetLength(s2, Length(s));
  b := 1;
  
  for a := 1 to Length(s) do
   if s[a] = '^' then Continue else
    if s[a-1] <> '^' then
    begin
     s2[b] := s[a];
     b := b+1;
    end;

  SetLength(s2, b-1);
  WriteLn(logfile, s2);
 end;

 SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
end;

procedure CreateLog(FileName: string);
begin
 if log then CloseLog();

 AssignFile(logfile, FileName);
 Rewrite(logfile);

 log := True;
end;

procedure CloseLog();
begin
 if log then CloseFile(logfile);
 log := False;
end;

end.
