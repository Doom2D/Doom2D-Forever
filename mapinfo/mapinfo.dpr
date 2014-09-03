program mapinfo;

{$APPTYPE CONSOLE}

uses
  windows,
  SysUtils,
  WADSTRUCT in '..\Shared Source\WADSTRUCT.pas',
  MAPDEF in '..\Shared Source\MAPDEF.pas',
  MAPREADER in '..\Shared Source\MAPREADER.pas',
  MAPSTRUCT in '..\Shared Source\MAPSTRUCT.pas',
  WADEDITOR in '..\Shared Source\WADEDITOR.pas',
  utils in 'utils.pas',
  inter_en in 'inter_en.pas',
  //inter_ru in 'inter_ru.pas',
  codepage in '..\Shared Source\codepage.pas';

const
  VERSION = '1.1';
  REPORTFILE0 = 'info.txt';
  REPORTFILE1 = 'info_%s.txt';
  STDWADS: array[0..1] of string = ('Standart.wad', 'ShrShade.wad');
  WADTYPE_STD = 0;
  WADTYPE_CST = 1;
  WADTYPE: array[WADTYPE_STD..WADTYPE_CST] of string = (INTER_WADTYPE_STD, INTER_WADTYPE_CST);

var
  Dir, opt, path, s, t: string;
  _f, _r, _p: Boolean;
  wadlist, maplist: SArray;
  usedlist: array of record
             wadname: string;
             wadtype: Byte;
            end;
  SR: TSearchRec;
  a, b, c: Integer;
  WAD: TWADEditor_1;
  MAP: TMapReader_1;
  MapData: Pointer;
  MapLen: Integer;
  MapHeader: TMapHeaderRec_1;
  MapTextures: TTexturesRec1Array;

procedure ShowHelp();
begin
 WriteLn('MAPINFO v.', VERSION);
 WriteLn('');
 WriteLn(Format(INTER_HELP1, [ExtractFileName(ParamStr(0))]));
 Writeln(Format(INTER_HELP2, [ExtractFileName(ParamStr(0))]));
 Writeln('');
 Writeln(INTER_HELP3);
 Writeln(Format(INTER_HELP4, [REPORTFILE0]));
 Writeln(Format(INTER_HELP5, [REPORTFILE1]));
 Writeln('');
 Writeln('(c) rs.falcon rmw.falcon@mail.ru http://www.doom2d.org/');
end;

procedure GetMaps();
var
  a: Integer;
  ResList: SArray;
  Data: Pointer;
  Len: Integer;
  Sign: array[0..2] of Char;
begin
 maplist := nil;
 ResList := WAD.GetResourcesList('');

 if ResList <> nil then
  for a := 0 to High(ResList) do
  begin
   if not WAD.GetResource('', ResList[a], Data, Len) then Continue;
   CopyMemory(@Sign[0], Data, 3);
   FreeMem(Data);
   
   if Sign = MAP_SIGNATURE then
   begin
    SetLength(maplist, Length(maplist)+1);
    maplist[High(maplist)] := ResList[a];
   end;

   Sign := '';
  end;
end;

procedure GetUsed();
var
  s: string;
  a: Integer;

procedure Add();
var
  a, b: Integer;
  ok: Boolean;
  _wadtype: Byte;
begin
 if s = '' then Exit;

 ok := True;
 if usedlist <> nil then
  for a := 0 to High(usedlist) do
   if usedlist[a].wadname = s then
   begin
    ok := False;
    Break;
   end;

 if ok then
 begin
  SetLength(usedlist, Length(usedlist)+1);
  with usedlist[High(usedlist)] do
  begin
   wadname := s;

   _wadtype := WADTYPE_CST;
   for b := 0 to High(STDWADS) do
    if s = STDWADS[b] then
    begin
     _wadtype := WADTYPE_STD;
     Break;
    end;

   wadtype := _wadtype;
  end;
 end;
end;

begin
 usedlist := nil;

 g_ProcessResourceStr(MapHeader.SkyName, @s, nil, nil);
 Add();

 g_ProcessResourceStr(MapHeader.MusicName, @s, nil, nil);
 Add();

 if MapTextures <> nil then
  for a := 0 to High(MapTextures) do
  begin
   g_ProcessResourceStr(MapTextures[a].Resource, @s, nil, nil);
   Add();
  end;
end;

begin
 if ParamStr(1) = '' then
 begin
  ShowHelp;
  Exit;
 end;

 _f := False;
 _r := False;
 _p := False;
 opt := ParamStr(2);
 if (Length(opt) > 1) and (opt[1] = '-') then
 begin
  _f := Pos('f', opt) > 0;
  _r := Pos('r', opt) > 0;
  if _r then _p := False else _p := Pos('p', opt) > 0;
 end;

 Dir := ExtractFilePath(ParamStr(0));
 path := ExtractFilePath(ParamStr(1));
 if path <> '' then path := path+'\';

 s := ParamStr(1);
 if FindFirst(Dir+s, faAnyFile, SR) = 0 then
 repeat
  SetLength(wadlist, Length(wadlist)+1);
  wadlist[High(wadlist)] := SR.Name;
 until FindNext(SR) <> 0;

 if _r then CreateLog(REPORTFILE0);

 print(Format(INTER_TOTALWADS, [Length(wadlist)]));
 print('');

 if wadlist <> nil then
 begin
  WAD := TWADEditor_1.Create;

  for a := 0 to High(wadlist) do
  begin
   if _p then
   begin
    s := ExtractFileName(wadlist[a]);
    if LowerCase(Copy(s, Length(s)-3, 4)) = '.wad' then Delete(s, Length(s)-3, 4);
    CreateLog(Format(REPORTFILE1, [s]));
   end;

   WAD.FreeWAD();

   print('************************************');
   print(Format(INTER_WADFILE, [wadlist[a]]));

   if not WAD.ReadFile(path+wadlist[a]) then
   begin
    print(INTER_WADERROR);
    print(Format(INTER_ERROR, [WAD.GetLastErrorStr]));
    Continue;
   end;

   GetMaps();
   print(Format(INTER_TOTALMAPS, [Length(maplist)]));
   if maplist <> nil then
    for b := 0 to High(maplist) do
    begin
     WAD.GetResource('', maplist[b], MapData, MapLen);
     MAP := TMapReader_1.Create;
     MAP.LoadMap(MapData);
     FreeMem(MapData);

     MapHeader := MAP.GetMapHeader();

     print('');
     print('^^2=== '+maplist[b]+' ===');
     print(Format(INTER_MAPNAME, [MapHeader.MapName]));
     print(Format(INTER_MAPAUTHOR, [MapHeader.MapAuthor]));
     print(Format(INTER_MAPDESCR, [MapHeader.MapDescription]));
     print(Format(INTER_MAPWIDTH, [MapHeader.Width]));
     print(Format(INTER_MAPHEIGHT, [MapHeader.Height]));
     if _f then
     begin
      print(Format(INTER_MAPMUSIC, [MapHeader.MusicName]));
      print(Format(INTER_MAPSKY, [MapHeader.SkyName]));
     end;

     MapTextures := MAP.GetTextures();

     if _f then
     begin
      print('');
      print(Format(INTER_TEXTURESTOTAL, [Length(MapTextures)]));

      if MapTextures <> nil then
       for c := 0 to High(MapTextures) do
       begin
        if ByteBool(MapTextures[c].Anim) then t := '^^3A' else t := '^^2S';
        print('^7 ['+t+'^7] ^2'+MapTextures[c].Resource);
       end;
     end;

     GetUsed();

     print('');
     print(Format(INTER_USEDWADS, [Length(usedlist)]));
     if usedlist <> nil then
      for c := 0 to High(usedlist) do
       print('^7 ['+WADTYPE[usedlist[c].wadtype]+'^7] ^2'+usedlist[c].wadname);

     Map.Destroy;
    end;

   print('');

   if _p then
   begin
    print('');
    print('------------------------------------------');
    print(Format(INTER_STAT, [VERSION]));
    CloseLog();
   end;
  end;

  WAD.Destroy;
 end;

 if _r then
 begin
  print('');
  print('------------------------------------------');
  print(Format(INTER_STAT, [VERSION]));
  CloseLog();
 end;

 readln;
end.
