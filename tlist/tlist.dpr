program tlist;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  ZLib,
  WADSTRUCT in '..\Shared Source\WADSTRUCT.pas',
  WADEDITOR in '..\Shared Source\WADEDITOR.pas',
  codepage in '..\Shared Source\codepage.pas',
  console in '..\Shared Source\console.pas';


const
  slist: array[0..1] of string = ('D2DTEXTURES', 'D2DSYMBOLS');

type
 TTGAHeader = packed record
  FileType:     Byte;
  ColorMapType: Byte;
  ImageType:    Byte;
  ColorMapSpec: array[0..4] of Byte;
  OrigX:        array[0..1] of Byte;
  OrigY:        array[0..1] of Byte;
  Width:        Word;
  Height:       Word;
  BPP:          Byte;
  ImageInfo:    Byte;
 end;

var
  WAD: TWADEditor_1;
  ResourcesList: SArray;
  i, ii: Integer;
  ResData: Pointer;
  ResLen: Integer;
  list: TextFile;
  c: Integer;
  header: TTGAHeader;

begin
  if ParamStr(1) = '' then Exit;

  if not FileExists(ParamStr(1)) then
  begin
   print(Format('WAD %s не найден', [ParamStr(1)]));
   Exit;
  end;

  WAD := TWADEditor_1.Create;
  if not WAD.ReadFile(ParamStr(1)) then
  begin
   Print(Format('ошибка чтения WAD: %s', [WAD.GetLastErrorStr]));
   Exit;
  end;

  print(Format('всего ресурсов: %d', [WAD.GetResourcesCount]));
  WriteLn('');

  AssignFile(list, '_textures.txt');
  Rewrite(list);

  c := 0;

  for i := 0 to High(slist) do
  begin
   ResourcesList := WAD.GetResourcesList(slist[i]);
   if ResourcesList = nil then
   begin
    Print(Format('секция %s не найдена', [slist[i]]));
    Exit;
   end;

   for ii := 0 to High(ResourcesList) do
   begin
    if not WAD.GetResource(slist[i], ResourcesList[ii], ResData, ResLen) then
    begin
     Print(Format('ошибка чтения ресурса %s', [ResourcesList[ii]]));
     continue;
    end;

    if ResLen <= sizeof(TTGAHeader) then continue;

    CopyMemory(@header, ResData, sizeof(TTGAHeader));

    if header.ImageType <> 2 then Continue;
    if header.ColorMapType <> 0 then Continue;
    if header.BPP < 24 then Continue;

    writeln(list, Format('[%d]', [c]));
    writeln(list, Format('dfname=%s:%s\%s', [ParamStr(1), slist[i], ResourcesList[ii]]));
    writeln(list, Format('size=%d,%d', [header.Width, header.Height]));
    writeln(list, '');
    c := c+1;

    FreeMemory(ResData);
   end;
  end;

  CloseFile(list);

  Writeln;
  print(Format('всего записей: %d', [c]));
  print('список создан');

  readln;
end.
 