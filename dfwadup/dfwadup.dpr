program dfwadup;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  ZLib,
  WADSTRUCT in '..\Shared Source\WADSTRUCT.pas',
  WADEDITOR in '..\Shared Source\WADEDITOR.pas',
  //inter_en in 'inter_en.pas',
  inter_ru in 'inter_ru.pas',
  codepage in '..\Shared Source\codepage.pas',
  console in '..\Shared Source\console.pas';

const
  VERSION = '1.2';

var
  WAD:           TWADEditor_1;
  SectionsList:  SArray;
  ResourcesList: SArray;
  i, ii:         Integer;
  ResData:       Pointer;
  ResLen:        Integer;
  ResourceFile:  File;
  Dir:           string;
  FileName:      string;
  CurrentDir:    string;
  DirToUnpack:   string;

procedure ShowHelp();
begin
 WriteLn('DFWADUP v. ', VERSION);
 WriteLn('');
 WriteLn(Format(INTER_HELP1, [ExtractFileName(ParamStr(0))]));
 WriteLn(Format(INTER_HELP2, [ExtractFileName(ParamStr(0))]));
 Writeln('');
 Writeln('(c) rs.falcon rmw.falcon@mail.ru http://www.doom2d.org/');
end;

function GetFileName(ResName: string): string;
var
  TempStr: string;
  a: Integer;
begin
 TempStr := ResName;

 for a := 1 to Length(TempStr) do
  if not (TempStr[a] in ['a'..'z', 'A'..'Z', 'à'..'ÿ', 'À'..'ß', '0'..'9', '-',
                         '_', '=', '+', ';', '''', ',', '.', '`', '~', '!', '@',
                         '#', '$', '%', '^', '&', '(', ')', '[', ']', '{', '}',
                         '\', '/']) then TempStr[a] := '_';

 Result := TempStr;
end;

begin
  if ParamStr(1) = '' then
  begin
   ShowHelp;
   Exit;
  end;

  if not FileExists(ParamStr(1)) then
  begin
   print(Format(INTER_WADERROR, [ParamStr(1)]));
   Exit;
  end;

  GetDir(0, Dir);
  DirToUnpack := ParamStr(2);

  if DirToUnpack <> '' then
  begin
   if DirToUnpack[1] <> '\' then DirToUnpack := '\'+DirToUnpack;
   if DirToUnpack[Length(DirToUnpack)] <> '\' then DirToUnpack := DirToUnpack+'\';

   if not ForceDirectories(Dir+DirToUnpack) then
   begin
    Print(Format(INTER_DIRERROR, [Dir]));
    Exit;
   end;
  end else DirToUnpack := '\';

  Dir := Dir+DirToUnpack;

  WAD := TWADEditor_1.Create;
  if not WAD.ReadFile(ParamStr(1)) then
  begin
   Print(Format(INTER_DFWADERROR, [WAD.GetLastErrorStr]));
   Exit;
  end;

  print(Format(INTER_INFO1, [WAD.GetVersion]));
  print(Format(INTER_INFO2, [WAD.GetResourcesCount]));
  WriteLn('');

  SectionsList := WAD.GetSectionList;

  if SectionsList <> nil then
   for i := 0 to High(SectionsList) do
   begin
    CurrentDir := GetFileName(SectionsList[i]);
    if not DirectoryExists(Dir+CurrentDir) then CreateDirectory(PChar(Dir+CurrentDir), nil);
    print(Format(INTER_INFO3, [SectionsList[i]]));
    print(Format(INTER_INFO4, ['\'+CurrentDir]));
    WriteLn('');

    ResourcesList := WAD.GetResourcesList(SectionsList[i]);

    if ResourcesList <> nil then
     for ii := 0 to High(ResourcesList) do
     begin
      WAD.GetResource(SectionsList[i], ResourcesList[ii], ResData, ResLen);

      if WAD.GetLastError <> DFWAD_NOERROR then
      begin
       Print(Format(INTER_RESINFO, [SectionsList[i], ResourcesList[ii]]));
       Print(Format(INTER_DFWADERROR, [WAD.GetLastErrorStr]));
       Continue;
      end;

      FileName := Dir+CurrentDir+'\'+GetFileName(ResourcesList[ii]);

      AssignFile(ResourceFile, FileName);
       Rewrite(ResourceFile, 1);
       BlockWrite(ResourceFile, ResData^, ResLen);
      CloseFile(ResourceFile);

      print(Format(INTER_INFO5, [ResourcesList[ii]]));
      print(Format(INTER_INFO6, [ResLen]));
      print(Format(INTER_INFO7, ['\'+CurrentDir+'\'+GetFileName(ResourcesList[ii])]));
      WriteLn('');

      FreeMemory(ResData);
     end;
   end;

  Writeln;
  print(INTER_WADUNPACKED);

  readln;
end.
 