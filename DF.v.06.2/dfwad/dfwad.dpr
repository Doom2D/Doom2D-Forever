program dfwad;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  WADSTRUCT in '..\Shared Source\WADSTRUCT.pas',
  WADEDITOR in '..\Shared Source\WADEDITOR.pas',
  codepage in '..\Shared Source\codepage.pas',
  //inter_en in 'inter_en.pas',
  inter_ru in 'inter_ru.pas',
  utils in 'utils.pas',
  console in '..\Shared Source\console.pas';

const
  VERSION = '1.3';

var
  _i: Boolean;
  WAD: TWADEditor_1;
  ListFile: TextFile;
  List: array of string;
  ResourceStr, CurrentSection: string;
  TotalResources: LongWord;
  WritedResources: LongWord;
  TotalSections: LongWord;
  ErrorsCount, a, b: Integer;
  alias: Boolean;

function GetFileName(Str: string): string;
var
  i: Integer;
begin
 for i := 1 to Length(Str) do
  if Str[i] = '|' then Break;

 Result := Copy(Str, 1, i-1);
end;

function GetResourceName(Str: string): string;
var
  i: Integer;
begin
 for i := 1 to Length(Str) do
  if Str[i] = '|' then Break;

 Result := Copy(Str, i+1, Length(Str)-i);
end;

function GetSectionName(Str: string): string;
begin
 Delete(Str, 1, 1);
 Result := Str;
end;

procedure ShowHelp();
begin
 WriteLn('DFWAD v. ', VERSION);
 WriteLn('');
 WriteLn(Format(INTER_HELP1, [ExtractFileName(ParamStr(0))]));
 WriteLn(Format(INTER_HELP2, [ExtractFileName(ParamStr(0))]));
 WriteLn(INTER_HELP2);
 Writeln('');
 Writeln('(c) rs.falcon rmw.falcon@mail.ru http://www.doom2d.org/');
end;

begin
  if (ParamStr(1) = '') or (ParamStr(2) = '') then
  begin
   ShowHelp();
   Exit;
  end;

  if not FileExists(ParamStr(1)) then
  begin
   print(INTER_LSTERROR);
   Exit;
  end;

  _i := Pos('i', ParamStr(3)) <> 0;

  WAD := TWADEditor_1.Create;

  CurrentSection := '';
  TotalResources := 0;
  WritedResources := 0;
  TotalSections := 0;
  ErrorsCount := 0;

  AssignFile(ListFile, ParamStr(1));
  Reset(ListFile);

  while not EOF(ListFile) do
  begin
   ReadLn(ListFile, ResourceStr);

   if Length(Trim(ResourceStr)) < 2 then Continue;
   if ResourceStr[1] = ';' then Continue;

   SetLength(List, Length(List)+1);
   List[High(List)] := ResourceStr;
  end;

  if List <> nil then
   for a := 0 to High(List) do
   begin
    ResourceStr := List[a];

    if ResourceStr[1] = ':' then
    begin
     Inc(TotalSections);
     CurrentSection := GetSectionName(ResourceStr);
     WAD.AddSection(CurrentSection);
     Continue;
    end;

    Inc(TotalResources);

    if not FileExists(GetFileName(ResourceStr)) then
    begin
     print(Format(INTER_RESERROR, [GetFileName(ResourceStr)]));
     WriteLn('');
     Inc(ErrorsCount);
     //readln;
     Continue;
    end;

    alias := False;
    for b := 1 to a-1 do
     if GetFileName(List[b]) = GetFileName(List[a]) then
     begin
      if WAD.AddAlias(GetResourceName(List[b]), GetResourceName(List[a])) then
      begin
       if _i then
       begin
        print(Format(INTER_RESNAME, [GetResourceName(List[a])]));
        print(Format(INTER_ALIAS, [GetResourceName(List[b])]));
        WriteLn('');
       end;

       Inc(WritedResources);
      end
       else
      begin
       print(Format(INTER_ALIASERROR, [GetResourceName(List[b])]));
       Inc(ErrorsCount);
       //readln;
       Continue;
      end;

      alias := True;

      Break;
     end;

    if alias then Continue;

    if not WAD.AddResource(GetFileName(ResourceStr), GetResourceName(ResourceStr), CurrentSection) then
    begin
     print(Format(INTER_DFWADERROR, [WAD.GetLastErrorStr]));
     print(Format(INTER_RESINFO, [GetFileName(ResourceStr), GetResourceName(ResourceStr)]));
     Inc(ErrorsCount);
     //readln;
     Continue;
    end;

    if _i then
    begin
     print(Format(INTER_FILENAME, [GetFileName(ResourceStr)]));
     print(Format(INTER_RESNAME, [GetResourceName(ResourceStr)]));
     WriteLn('');
    end;
    
    Inc(WritedResources);
   end;

  CloseFile(ListFile);

  WAD.SaveTo(ParamStr(2));

  if _i then
  begin
   WriteLn('');
   print(Format(INTER_INFO1, [TotalResources]));
   print(Format(INTER_INFO2, [WritedResources]));
   print(Format(INTER_INFO3, [TotalSections]));
   WriteLn('');
   print(Format(INTER_INFO4, [ParamStr(2)]));
   print(Format(INTER_INFO5, [WAD.GetVersion]));
   print(Format(INTER_INFO6, [WAD.GetResourcesCount]));
   WriteLn('');
  end;

  if ErrorsCount = 0 then
   (if _i then print(Format(INTER_INFO7+#13#10, ['^^20'])))
    else print(Format(INTER_INFO7+#13#10, ['^^1'+tostr(ErrorsCount)]));

  WAD.Destroy();

  print(INTER_WADCREATED);
end.
