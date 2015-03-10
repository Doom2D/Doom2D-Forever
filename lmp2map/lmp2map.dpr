program lmp2map;

{$APPTYPE CONSOLE}
{.$DEFINE BUILD_ENG}

uses
  SysUtils,
  windows,
  MAP in 'MAP.pas',
  MAPSTRUCT in '..\Shared Source\MAPSTRUCT.pas',
  MAPWRITER in '..\Shared Source\MAPWRITER.pas',
  CONFIG in '..\Shared Source\CONFIG.pas',
  d2ddef in 'd2ddef.pas',
  MAPDEF in '..\Shared Source\MAPDEF.pas',
  utils in 'utils.pas',
{$IFNDEF BUILD_ENG}
  inter_ru in 'inter_ru.pas',
{$ELSE}
  inter_en in 'inter_en.pas',
{$ENDIF}
  codepage in '..\Shared Source\codepage.pas',
  console in '..\Shared Source\console.pas';

type
  TTexture = record
   d2dname: SArray;
   dfname: Char64;
   width, height: Integer;
   offset: TPoint;
   anim: Boolean;
  end;

  TRectWH = record
   X, Y: Integer;
   Width, Height: Word;
  end;

const
  VERSION = '2.1';
  TABLE_FILENAME = 'textures.txt';
  NO_TEXTURE = 'Standart.wad:STDTEXTURES\NO_TEXTURE';

  ItemSize: Array [ITEM_MEDKIT_SMALL..ITEM_KEY_BLUE] of Array [0..1] of Byte =
    (((14), (15)), // MEDKIT_SMALL
     ((28), (19)), // MEDKIT_LARGE
     ((28), (19)), // MEDKIT_BLACK
     ((31), (16)), // ARMOR_GREEN
     ((31), (16)), // ARMOR_BLUE
     ((25), (25)), // SPHERE_BLUE
     ((25), (25)), // SPHERE_WHITE
     ((24), (47)), // SUIT
     ((14), (27)), // OXYGEN
     ((25), (25)), // INV
     ((62), (24)), // WEAPON_SAW
     ((63), (12)), // WEAPON_SHOTGUN1
     ((54), (13)), // WEAPON_SHOTGUN2
     ((54), (16)), // WEAPON_CHAINGUN
     ((62), (16)), // WEAPON_ROCKETLAUNCHER
     ((54), (16)), // WEAPON_PLASMA
     ((61), (36)), // WEAPON_BFG
     ((54), (16)), // WEAPON_SUPERPULEMET
     (( 9), (11)), // AMMO_BULLETS
     ((28), (16)), // AMMO_BULLETS_BOX
     ((15), ( 7)), // AMMO_SHELLS
     ((32), (12)), // AMMO_SHELLS_BOX
     ((12), (27)), // AMMO_ROCKET
     ((54), (21)), // AMMO_ROCKET_BOX
     ((15), (12)), // AMMO_CELL
     ((32), (21)), // AMMO_CELL_BIG
     ((22), (29)), // AMMO_BACKPACK
     ((16), (16)), // KEY_RED
     ((16), (16)), // KEY_GREEN
     ((16), (16))); // KEY_BLUE

  AreaSize: Array [AREA_PLAYERPOINT1..AREA_BLUETEAMPOINT] of TRectWH =
    ((X:15; Y:12; Width:34; Height:52), // PLAYERPOINT1
     (X:15; Y:12; Width:34; Height:52), // PLAYERPOINT2
     (X:15; Y:12; Width:34; Height:52), // DMPOINT
     (X: 0; Y: 0; Width:64; Height:64), // REDFLAG
     (X: 0; Y: 0; Width:64; Height:64), // BLUEFLAG
     (X: 0; Y: 0; Width:64; Height:64), // DOMFLAG
     (X:15; Y:12; Width:34; Height:52), // REDTEAMPOINT
     (X:15; Y:12; Width:34; Height:52)); // BLUETEAMPOINT

  MonsterSize: Array [MONSTER_DEMON..MONSTER_MAN] of TRectWH =
    ((X:  7; Y:  8; Width:  50; Height:  52),  // DEMON
     (X: 15; Y: 10; Width:  34; Height:  50),  // IMP
     (X: 15; Y:  8; Width:  34; Height:  52),  // ZOMBY
     (X: 15; Y:  8; Width:  34; Height:  52),  // SERG
     (X: 24; Y:  9; Width:  80; Height: 110),  // CYBER
     (X: 15; Y:  4; Width:  34; Height:  56),  // CGUN
     (X: 39; Y: 32; Width:  50; Height:  64),  // BARON
     (X: 39; Y: 32; Width:  50; Height:  64),  // KNIGHT
     (X: 34; Y: 36; Width:  60; Height:  56),  // CACO
     (X: 16; Y: 14; Width:  32; Height:  36),  // SOUL
     (X: 34; Y: 36; Width:  60; Height:  56),  // PAIN
     (X: 23; Y: 14; Width: 210; Height: 100),  // SPIDER
     (X: 14; Y: 17; Width: 100; Height:  42),  // BSP
     (X: 28; Y: 34; Width:  72; Height:  60),  // MANCUB
     (X: 30; Y: 28; Width:  68; Height:  72),  // SKEL
     (X: 30; Y: 28; Width:  68; Height:  72),  // VILE
     (X:  6; Y: 11; Width:  20; Height:  10),  // FISH
     (X: 20; Y: 13; Width:  24; Height:  36),  // BARREL
     (X: 30; Y: 26; Width:  68; Height:  76),  // ROBO
     (X: 15; Y:  6; Width:  34; Height:  52)); // MAN

var
  Dir: String;

  _i: Boolean = False;
  _n: Boolean = False;
  _b: Boolean = False;
  _o: Boolean = False;
  _c: Boolean = False;

  LMPFile: File;

  header: map_header_t;
  blk: map_block_t;
  musname: Array [0..8] of Char = '';
  sky_type: SmallInt;
  wal: Array [0..254] of wall_t;
  th: Array [0..MAXTH-1] of thing_t;
  sw: Array [0..MAXSW-1] of sw_t;
  fldb: packed Array [0..FLDH-1, 0..FLDW-1] of Byte;
  fldf: packed Array [0..FLDH-1, 0..FLDW-1] of Byte;
  fld: packed Array [0..FLDH-1, 0..FLDW-1] of Byte;

  sa: SArray;
  Buffer, p: Pointer;
  i, j, ii, jj, a, b, c, id, d, ind: Integer;
  t: Byte;
  strz: PChar;
  str: String;
  e: Boolean;
  tdata: TTriggerData;

  MAPFile: File;
  MWriter: TMapWriter_1;
  MapHeader: TMapHeaderRec_1;
  textures: TTexturesRec1Array;
  panels: TPanelsRec1Array;
  panels2: TPanelsRec1Array;
  items: TItemsRec1Array;
  monsters: TMonsterRec1Array;
  areas: TAreasRec1Array;
  triggers: TTriggersRec1Array;
  MapData: Pointer;
  MapSize: LongWord;

  TableFile: TConfig;
  TextureTable: Array of TTexture;
  texture_flags: Array of Integer;

label
  _end;

procedure ShowHelp();
begin
 WriteLn('LMP2MAP v.', VERSION);
 WriteLn('');
 WriteLn(Format(INTER_HELP1, [ExtractFileName(ParamStr(0))]));
 WriteLn(Format(INTER_HELP2, [ExtractFileName(ParamStr(0))]));
 Writeln('');
 WriteLn(INTER_HELP3);
 WriteLn(INTER_HELP4);
 WriteLn(INTER_HELP5);
 WriteLn(INTER_HELP6);
 WriteLn(INTER_HELP7);
 Writeln('');
 Writeln('(c) rs.falcon rmw.falcon@mail.ru http://www.doom2d.org/');
end;

function GetNewTexture(n: string; var id: Integer): Boolean;
var
  a, b: Integer;
  s: PChar;
begin
 Result := False;

 if TextureTable = nil then Exit;

 s := StrAlloc(9);
 StrLCopy(s, PChar(n), 8);
 OemToChar(s, s);

 for a := 0 to High(TextureTable) do
  for b := 0 to High(TextureTable[a].d2dname) do
   if AnsiLowerCase(TextureTable[a].d2dname[b]) = AnsiLowerCase(s) then
   begin
    id := a;
    Result := True;
    Break;
   end;

 StrDispose(s);
end;

procedure CreateTextureTable();
var
  i: Integer;
  str: string;
begin
 Setlength(TextureTable, 4);
 for i := 0 to 2 do
  with TextureTable[i] do
  begin
   SetLength(d2dname, 1);
   d2dname[0] := '_water_'+IntToStr(i);
   ZeroMemory(@dfname, 64);
   CopyMemory(@dfname, @d2dname[0][1], Min(64, Length(d2dname[0])));
   width := 16;
   height := 16;
   anim := False;
  end;

 with TextureTable[3] do
 begin
  SetLength(d2dname, 1);
  d2dname[0] := 'NTEXTURE';
  ZeroMemory(@dfname, 64);
  dfname := NO_TEXTURE;
  width := 16;
  height := 16;
  anim := False;
 end;

 i := 0;
 TableFile := TConfig.CreateFile(Dir+TABLE_FILENAME);
 while TableFile.SectionExists(IntToStr(i)) do
 begin
  SetLength(TextureTable, Length(TextureTable)+1);
  with TextureTable[High(TextureTable)], TableFile do
  begin
   str := ReadStr(IntToStr(i), 'dfname', '');
   ZeroMemory(@dfname, 64);
   CopyMemory(@dfname, @str[1], Min(64, Length(str)));

   ProcessResourceStr(string(dfname), nil, nil, @str);
   d2dname := parse(ReadStr(IntToStr(i), 'd2dname', str));

   sa := parse(ReadStr(IntToStr(i), 'size', ''));
   width := StrToInt(sa[0]);
   height := StrToInt(sa[1]);

   str := ReadStr(IntToStr(i), 'offset', '');
   if str <> '' then
   begin
    sa := parse(str);
    offset.X := StrToInt(sa[0]);
    offset.Y := StrToInt(sa[1]);
   end
    else
   begin
    offset.X := 0;
    offset.Y := 0;
   end;

   anim := ReadBool(IntToStr(i), 'anim', False);
  end;
  i := i+1;
 end;
 TableFile.Destroy;
end;

function finddoor(i, j: Integer): Word;
var
  a, b: Integer;
  door: array [0..FLDH-1, 0..FLDW-1] of Byte;

 procedure fill(a, b: Integer);
 begin
  if (a < 0) or (a > FLDH-1) then Exit;
  if (b < 0) or (b > FLDW-1) then Exit;

  if door[a, b] = 255 then Exit;
  if door[a, b] <> W_OPENEDDOOR then Exit;

  door[a, b] := 255;
  fill(a+1, b);
  fill(a, b+1);
  fill(a-1, b);
  fill(a, b-1);
 end;

begin
 Result := $FFFF;

 for a := 0 to FLDH-1 do
  for b := 0 to FLDW-1 do
   door[a, b] := fld[a, b];

 fill(i, j);

 for a := 0 to FLDH-1 do
  for b := 0 to FLDW-1 do
   if (door[a, b] = 255) and (fldf[a, b] <> 0) then
   begin
    Result := fldf[a, b];
    Break;
   end;
end;

function OptimizePanels(): Integer;
var
  PanelsType: Word;
  flags: array of Integer;
  a: Boolean;
  i, n: Integer;
begin
 Result := 0;

 PanelsType := PANEL_WALL or PANEL_BACK or PANEL_FORE or PANEL_WATER or
               PANEL_ACID1 or PANEL_ACID2 or PANEL_STEP or PANEL_LIFTUP or
               PANEL_LIFTDOWN or PANEL_BLOCKMON or PANEL_CLOSEDOOR or PANEL_OPENDOOR;

 SetLength(flags, Length(panels2));
 for i := 0 to High(flags) do flags[i] := 0;

 if triggers <> nil then
  for i := 0 to High(triggers) do
  begin
   if (TTriggerData(triggers[i].Data).PanelID <> -1) and
       ((triggers[i].TriggerType = TRIGGER_LIFT) or
        (triggers[i].TriggerType = TRIGGER_LIFTUP) or
        (triggers[i].TriggerType = TRIGGER_LIFTDOWN) or
        (triggers[i].TriggerType = TRIGGER_OPENDOOR) or
        (triggers[i].TriggerType = TRIGGER_CLOSEDOOR) or
        (triggers[i].TriggerType = TRIGGER_DOOR) or
        (triggers[i].TriggerType = TRIGGER_DOOR5) or
        (triggers[i].TriggerType = TRIGGER_CLOSETRAP) or
        (triggers[i].TriggerType = TRIGGER_TRAP)) then
    flags[TTriggerData(triggers[i].Data).PanelID] := 1;

   if triggers[i].TexturePanel <> -1 then flags[triggers[i].TexturePanel] := 1;
  end;

 a := True;
 while a do
 begin
  a := False;

  for i := 0 to High(panels2) do
  begin
   if flags[i] = 1 then Continue;
   if panels2[i].PanelType = 0 then Continue;

   for n := 0 to High(panels2) do
   begin
    if flags[n] = 1 then Continue;
    if panels2[n].PanelType = 0 then Continue;

    if (i <> n) and WordBool(panels2[n].PanelType and PanelsType) and
       (panels2[i].Width <> 0) and
       (panels2[n].Width <> 0) and
       (panels2[n].TextureNum = panels2[i].TextureNum) and
       (panels2[n].PanelType = panels2[i].PanelType) and
       (panels2[n].Alpha = panels2[i].Alpha) and
       (panels2[n].Flags = panels2[i].Flags) then
    begin
     if (panels2[n].X = panels2[i].X + panels2[i].Width) and
        (panels2[n].Y = panels2[i].Y) and
        (panels2[n].Height = panels2[i].Height) then
     begin
      panels2[i].Width := panels2[i].Width+panels2[n].Width;
      panels2[n].PanelType := PANEL_NONE;
      a := True;
      Inc(Result);
      Continue;
     end;

     if (panels2[n].Y = panels2[i].Y + panels2[i].Height) and
        (panels2[n].X = panels2[i].X) and
        (panels2[n].Width = panels2[i].Width) then
     begin
      panels2[i].Height := panels2[i].Height+panels2[n].Height;
      panels2[n].PanelType := PANEL_NONE;
      a := True;
      Inc(Result);
      Continue;
     end;
    end;
   end;
  end;
 end;
end;

procedure pack_panels();
var
 PanelTable: array of Integer;
 a, b, c: Integer;
begin
 c := 0;
 for a := 0 to ind-1 do
  if panels2[a].PanelType <> PANEL_NONE then c := c+1;
 SetLength(panels, c);
 SetLength(PanelTable, c);

 if c = 0 then Exit;

 c := 0;
 for a := 0 to High(panels2) do
  if panels2[a].PanelType <> PANEL_NONE then
  begin
   PanelTable[c] := a;
   panels[c] := panels2[a];
   c := c+1;
  end;

 if triggers <> nil then
  for a := 0 to High(triggers) do
  begin
   if triggers[a].TexturePanel <> -1 then
    for b := 0 to c-1 do
     if PanelTable[b] = triggers[a].TexturePanel then
     begin
      triggers[a].TexturePanel := b;
      Break;
     end;

   if triggers[a].TriggerType in [TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR,
                                  TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP,
                                  TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT] then
    if TTriggerData(triggers[a].DATA).PanelID <> -1 then
     for b := 0 to c-1 do
      if PanelTable[b] = TTriggerData(triggers[a].DATA).PanelID then
      begin
       TTriggerData(triggers[a].DATA).PanelID := b;
       Break;
      end;
  end;
end;

procedure OptimizeTextures();
var
  a, b, d: Integer;
  c: Boolean;
  TextureTable: array of Integer;
begin
 if panels = nil then Exit;
 if textures = nil then Exit;

 SetLength(TextureTable, Length(textures));

 for a := 0 to High(textures) do
  TextureTable[a] := a;

 for a := 0 to High(textures) do
 begin
  c := False;

  for b := 0 to High(panels) do
   if panels[b].TextureNum = a then
   begin
    c := True;
    Break;
   end;

  if c then Continue;

  textures[a].Resource := '';
 end;

 d := 0;
 for a := 0 to High(textures) do
  if textures[a].Resource <> '' then
  begin
   TextureTable[a] := d;
   d := d+1;
  end;

 a := 0;
 while a < Length(textures) do
 begin
  if textures[a].Resource = '' then
  begin
   for b := a to High(textures)-1 do
    textures[b] := textures[b+1];

   SetLength(textures, Length(textures)-1);
  end else a := a+1;
 end;

 for a := 0 to High(panels) do
  panels[a].TextureNum := TextureTable[panels[a].TextureNum];
end;

function CollideLevel(X, Y: Integer; Width, Height: Word): Boolean;
var
  a: Integer;
begin
 Result := False;

 if panels <> nil then
  for a := 0 to High(panels) do
   if WordBool(panels[a].PanelType and (PANEL_WALL or PANEL_CLOSEDOOR)) then
    if Collide(X, Y, Width, Height, panels[a].X, panels[a].Y, panels[a].Width, panels[a].Height) then
    begin
     Result := True;
     Exit;
    end;
end;

procedure Correction();
var
  x, y, a, b: Integer;
  w, h: Word;
  e: Boolean;
begin
 if areas <> nil then
  for a := 0 to High(areas) do
  begin
   x := areas[a].X;
   y := areas[a].Y;
   w := AreaSize[areas[a].AreaType].Width;
   h := AreaSize[areas[a].AreaType].Height;

   if CollideLevel(x, y, w, h) then
   begin
    e := False;

    for b := 1 to 8 do
     if not CollideLevel(x, y+b, w, h) then
     begin
      areas[a].Y := areas[a].Y+b;
      e := True;
      Break;
     end;

    if e then Continue;

    for b := 1 to 8 do
     if not CollideLevel(x, y-b, w, h) then
     begin
      areas[a].Y := areas[a].Y-b;
      e := True;
      Break;
     end;

    if e then Continue;

    for b := 1 to 8 do
     if not CollideLevel(x+b, y, w, h) then
     begin
      areas[a].X := areas[a].X+b;
      e := True;
      Break;
     end;

    if e then Continue;

    for b := 1 to 8 do
     if not CollideLevel(x-b, y, w, h) then
     begin
      areas[a].X := areas[a].X-b;
      Break;
     end;
   end;
  end;

 if monsters <> nil then
  for a := 0 to High(monsters) do
  begin
   x := monsters[a].X;
   y := monsters[a].Y;
   w := MonsterSize[monsters[a].MonsterType].Width;
   h := MonsterSize[monsters[a].MonsterType].Height;

   if CollideLevel(x, y, w, h) then
   begin
    e := False;

    for b := 1 to 8 do
     if not CollideLevel(x, y+b, w, h) then
     begin
      monsters[a].Y := monsters[a].Y+b;
      e := True;
      Break;
     end;

    if e then Continue;

    for b := 1 to 8 do
     if not CollideLevel(x, y-b, w, h) then
     begin
      monsters[a].Y := monsters[a].Y-b;
      e := True;
      Break;
     end;

    if e then Continue;

    for b := 1 to 8 do
     if not CollideLevel(x+b, y, w, h) then
     begin
      monsters[a].X := monsters[a].X+b;
      e := True;
      Break;
     end;

    if e then Continue;

    for b := 1 to 8 do
     if not CollideLevel(x-b, y, w, h) then
     begin
      monsters[a].X := monsters[a].X-b;
      Break;
     end;
   end;
  end;
end;

begin
 if (ParamCount() < 2) or (ParamStr(1) = '') or (ParamStr(2) = '') or (ParamStr(3) = '') then
 begin
  ShowHelp();
  Exit;
 end;

 if not FileExists(ParamStr(1)) then
 begin
  print(Format(INTER_ERROR1, [ParamStr(1)]));
  Exit;
 end;

 Dir := ExtractFilePath(ParamStr(0));

 if not FileExists(Dir+TABLE_FILENAME) then
 begin
  print(Format(INTER_ERROR2, [TABLE_FILENAME]));
  Exit;
 end;

 _i := Pos('i', ParamStr(3)) <> 0;
 _n := Pos('n', ParamStr(3)) <> 0;
 _b := Pos('b', ParamStr(3)) <> 0;
 _o := Pos('o', ParamStr(3)) <> 0;
 _c := Pos('c', ParamStr(3)) <> 0;

 if _n then _i := True;

 AssignFile(LMPFile, ParamStr(1));
 Reset(LMPFile, 1);
  BlockRead(LMPFile, Header, SizeOf(map_header_t));

  if _i then
  begin
   print(Format(INTER_MAPID, [Header.id]));
   print(Format(INTER_MAPVERSION, [Header.ver]));
   print('');
  end;
  
  if Header.id <> SIGNATURE then
  begin
   print(Format(INTER_ERROR3, [SIGNATURE]));
   CloseFile(LMPFile);
   Exit;
  end;

  if Header.ver <> LAST_MAP_VER then
  begin
   print(INTER_ERROR4);
   CloseFile(LMPFile);
   Exit;
  end;

  while True do
  begin
   BlockRead(LMPFile, blk, SizeOf(map_block_t));

   if _i then print(Format(INTER_BLOCK, [blk.t, GetBlockName(blk.t), blk.st, blk.sz]));

   case blk.t of
    MB_END: Break;
    MB_COMMENT:
    begin
     Buffer := GetMemory(blk.sz);
     BlockRead(LMPFile, Buffer^, blk.sz);
     print(Format(INTER_COMMENT, [PChar(Buffer)]));
     FreeMem(Buffer);
    end;
    MB_MUSIC: BlockRead(LMPFile, musname[0], blk.sz);
    MB_WALLNAMES:
    begin
     i := 0;

     while (i<256) and (blk.sz>0) do
     begin
      blk.sz := blk.sz-SizeOf(wall_t);
      BlockRead(LMPFile, wal[i], SizeOf(wall_t));
      i := i+1;
     end;
    end;
    MB_BACK, MB_WTYPE, MB_FRONT:
    begin
     case blk.t of
      MB_BACK: p := @fldb;
      MB_WTYPE: p := @fld;
      else p := @fldf;
     end;

     if blk.st = 0 then BlockRead(LMPFile, p^, blk.sz)
      else
     begin
      Buffer := GetMemory(blk.sz);
      Blockread(LMPFile, Buffer^, blk.sz);
      unpack(Buffer, blk.sz, p);
      FreeMem(Buffer);
     end;
    end;
    MB_SKY:
    begin
     sky_type := 0;
     BlockRead(LMPFile, sky_type, 2);
    end;
    MB_THING:
    begin
     i := 0;
     while (i < MAXTH) and (blk.sz > 0) do
     begin
      BlockRead(LMPFile, th[i], SizeOf(thing_t));
      blk.sz := blk.sz-8;
      i := i+1;
     end;
    end;
    MB_SWITCH2:
    begin
     i := 0;
     while (i < MAXSW) and (blk.sz > 0) do
     begin
      BlockRead(LMPFile, sw[i], SizeOf(sw_t));
      blk.sz := blk.sz-SizeOf(sw_t);
      i := i+1;
     end;
    end;
    else
    begin
     print(Format(INTER_UNKNOWNBLOCK, [blk.t]));
     Seek(LMPFile, FilePos(LMPFile)+blk.sz);
    end;
   end;
  end;

 CloseFile(LMPFile);

 OemToChar(musname, musname);

 if _i then
 begin
  WriteLn('');
  WriteLn('');
  print(INTER_INFO);
  WriteLn('');

  print(Format(INTER_MUSIC, [musname]));

  WriteLn('');
  print(Format(INTER_SKY, [sky_type]));

  WriteLn('');
  strz := StrAlloc(9);
  for i := 0 to 254 do
   if wal[i].n = '' then Break else
   begin
    StrLCopy(strz, wal[i].n, 8);
    OemToChar(strz, strz);
    print(Format(INTER_WALL, [strz, wal[i].t]));
   end;
  StrDispose(strz);

  WriteLn('');
  for i := 0 to MAXTH-1 do
   if th[i].t = 0 then Break else
    print(Format(INTER_THING, [th[i].x, th[i].y, th[i].t, th[i].f]));

  WriteLn('');
  for i := 0 to MAXSW-1 do
   if sw[i].t = 0 then Break else
    print(Format(INTER_SWITCH, [sw[i].x, sw[i].y, sw[i].t, sw[i].tm, sw[i].a,
                   sw[i].b, sw[i].c, sw[i].d, sw[i].f]));
  WriteLn('');
 end;

 if _n then goto _end;

 print(INTER_CONVERT);

 CreateTextureTable();

 MWriter := TMapWriter_1.Create;

 with MapHeader do
 begin
  str := ExtractFileName(ParamStr(1));
  ZeroMemory(@MapName[0], 32);
  CopyMemory(@MapName[0], @str[1], Min(32, Length(str)));

  MapAuthor := '';
  MapDescription := 'Converted with LMP2MAP v.'+VERSION;
  MusicName := 'Standart.wad:D2DMUS\        ';
  CopyMemory(@MusicName[20], @musname[0], 8);

  case sky_type of
   1: SkyName := 'Standart.wad:D2DSKY\RSKY1';
   2: SkyName := 'Standart.wad:D2DSKY\RSKY2';
   3: SkyName := 'Standart.wad:D2DSKY\RSKY3';
   else SkyName := 'Standart.wad:D2DSKY\RSKY1';
  end;

  Width := FLDW*16;
  Height := FLDH*16;
 end;

 MWriter.AddHeader(MapHeader);

 e := False;
 for i := 0 to 254 do
  if wal[i].n <> '' then
  begin
   if not GetNewTexture(wal[i].n, a) then
   begin
    strz := StrAlloc(9);
    StrLCopy(strz, wal[i].n, 8);
    OemToChar(strz, strz);
    print(Format(INTER_ERROR5, [strz, TABLE_FILENAME]));
    StrDispose(strz);

    if e then Continue;

    wal[i].n := 'NTEXTURE';
    GetNewTexture(wal[i].n, a);
    e := True;
   end;

   SetLength(textures, Length(textures)+1);
   with textures[High(textures)] do
   begin
    Resource := TextureTable[a].dfname;
    Anim := Byte(TextureTable[a].anim);
   end;
  end;

 ind := 0;
 SetLength(panels, 1024);

 for i := 0 to FLDW-1 do
  for j := 0 to FLDH-1 do
   if fldf[i, j] <> 0 then
   begin
    if not GetNewTexture(wal[fldf[i, j]-1].n, id) then Continue;

    a := i;
    b := j;

    case fld[i, j] of
     W_WATER, W_ACID1, W_ACID2, W_CLOSEDDOOR:
     begin
      a := Min(i+(TextureTable[id].height div 16)-1, 99);
      b := Min(j+(TextureTable[id].width div 16)-1, 99);

      e := True;
      for ii := i to a do
       for jj := j to b do
        if fld[ii, jj] <> fld[i, j] then
        begin
         e := False;
         Break;
        end;
     end;
     else e := False;
    end;

    if e and (fld[i, j] = W_CLOSEDDOOR) then
     for d := 0 to MAXSW-1 do
      if ((sw[d].t = SW_OPENDOOR) or (sw[d].t = SW_DOOR) or (sw[d].t = SW_DOOR5) or
          (sw[d].t = SW_TRAP)) and
         (sw[d].a = j) and (sw[d].b = i) then
      begin
       e := False;
       Break;
      end;

    if ind >= Length(panels) then SetLength(panels, Length(panels)+1024);
    with panels[ind] do
    begin
     X := j*16;
     Y := i*16;
     Width := TextureTable[id].width;
     Height := TextureTable[id].height;

     TextureNum := 2;
     Flags := 0;
     Alpha := 0;

     if e then
     begin
      PanelType := GetPanel(fld[i, j]);

      for ii := i to a do
       for jj := j to b do
        fld[ii, jj] := 0;
     end else PanelType := PANEL_FORE;

     if WordBool(PanelType and (PANEL_WATER or PANEL_ACID1 or PANEL_ACID2)) then
      Flags := Flags or PANEL_FLAG_WATERTEXTURES;

     for d := 0 to High(textures) do
      if textures[d].Resource = TextureTable[id].dfname then
      begin
       TextureNum := d;
       Break;
      end;
    end;
    ind := ind+1;
   end;

 for i := 0 to FLDW-1 do
  for j := 0 to FLDH-1 do
   if fldb[i, j] <> 0 then
   begin
    if not GetNewTexture(wal[fldb[i, j]-1].n, id) then Continue;

    a := i;
    b := j;

    case fld[i, j] of
     W_WALL, W_STEP:
     begin
      a := Min(i+(TextureTable[id].height div 16)-1, 99);
      b := Min(j+(TextureTable[id].width div 16)-1, 99);

      e := True;
      for ii := i to a do
       for jj := j to b do
        if fld[ii, jj] <> fld[i, j] then
        begin
         e := False;
         Break;
        end;
     end;
     else e := False;
    end;

    if ind >= Length(panels) then SetLength(panels, Length(panels)+1024);
    with panels[ind] do
    begin
     X := j*16;
     Y := i*16;

     Width := TextureTable[id].width;
     Height := TextureTable[id].height;

     TextureNum := 2;
     Flags := 0;
     Alpha := 0;

     if e then
     begin
      if fld[i, j] = W_WALL then PanelType := PANEL_WALL else PanelType := PANEL_STEP;
      for ii := i to a do
       for jj := j to b do
        fld[ii, jj] := 0;
     end else PanelType := PANEL_BACK;

     for d := 0 to High(textures) do
      if textures[d].Resource = TextureTable[id].dfname then
      begin
       TextureNum := d;
       Break;
      end;
    end;
    ind := ind+1;
   end;

 for i := 0 to FLDW-1 do
  for j := 0 to FLDH-1 do
   if fld[i, j] <> 0 then
   begin
    if GetPanel(fld[i, j]) = PANEL_NONE then Continue;

    if ind >= Length(panels) then SetLength(panels, Length(panels)+1024);
    with panels[ind] do
    begin
     X := j*16;
     Y := i*16;
     Width := 16;
     Height := 16;

     TextureNum := $FFFF;
     Flags := 0;
     Alpha := 0;

     PanelType := GetPanel(fld[i, j]);

     a := 0;
     case fld[i, j] of
      W_WATER, W_ACID1, W_ACID2, W_CLOSEDDOOR:
       Flags := Flags or PANEL_FLAG_HIDE;

      W_OPENEDDOOR:
      begin
       for d := 0 to MAXSW-1 do
        if ((sw[d].t = SW_SHUTDOOR) or (sw[d].t = SW_DOOR) or (sw[d].t = SW_DOOR5) or
            (sw[d].t = SW_SHUTTRAP) or (sw[d].t = SW_TRAP)) and
           (sw[d].a = j) and (sw[d].b = i) then
        begin
         a := $FFFF;
         Break;
        end;

       if a = 0 then a := finddoor(i, j);
       if a <> $FFFF then
        if a = fldf[i, j] then a := $FFFF;

       if (a <> $FFFF) and GetNewTexture(wal[a-1].n, id) then
       for d := 0 to High(textures) do
        if textures[d].Resource = TextureTable[id].dfname then
        begin
         TextureNum := d;
         Break;
        end;
      end;
     end;

     if TextureNum = $FFFF then
     begin
      TextureNum := 0;
      Flags := Flags or PANEL_FLAG_HIDE;
     end;
    end;
    ind := ind+1;
   end;

 for i := 0 to MAXTH-1 do
  if th[i].t = TH_NONE then Break else
  begin
   if th[i].t < 100 then
   begin
    t := GetArea(th[i].t);
    if t = AREA_NONE then Continue;

    SetLength(areas, Length(areas)+1);
    with areas[High(areas)] do
    begin
     X := th[i].x*2-(AreaSize[t].Width div 2);
     Y := th[i].y*2-AreaSize[t].Height;
     AreaType := t;
     if WordBool(th[i].f and THF_DIR) then Direction := 1 else Direction := 0;
    end;
   end;

   if (th[i].t >= 100) and (th[i].t < TH__LASTI) then
   begin
    t := GetItem(th[i].t);

    if t <> ITEM_NONE then
    begin
     SetLength(items, Length(items)+1);
     with items[High(items)] do
     begin
      X := th[i].x*2-(ItemSize[t][0] div 2);
      Y := th[i].y*2-ItemSize[t][1];
      ItemType := t;
      if WordBool(th[i].f and THF_DM) then Options := ITEM_OPTION_ONLYDM;
      if t in [ITEM_KEY_RED, ITEM_KEY_GREEN, ITEM_KEY_BLUE] then
       Options := Options or ITEM_OPTION_FALL;
     end;
    end;
   end;

   if (th[i].t >= TH_RTORCH) and (th[i].t <= TH_FCAN) then
   begin
    if ind >= Length(panels) then SetLength(panels, Length(panels)+1024);
    with panels[ind] do
    begin
     PanelType := PANEL_BACK;
     Alpha := 0;
     Flags := 0;

     case th[i].t of
      TH_RTORCH: GetNewTexture('SMRTA0', id);
      TH_GTORCH: GetNewTexture('SMGTA0', id);
      TH_BTORCH: GetNewTexture('SMBTA0', id);
      TH_GOR1: GetNewTexture('GOR1A0', id);
      TH_FCAN: GetNewTexture('FCANA0', id);
      else Continue;
     end;

     Width := TextureTable[id].width;
     Height := TextureTable[id].height;

     X := th[i].x*2;
     Y := th[i].y*2;

     TextureNum := $FFFF;
     for d := 0 to High(textures) do
      if textures[d].Resource = TextureTable[id].dfname then
      begin
       TextureNum := d;
       Break;
      end;

     if TextureNum = $FFFF then
     begin
      SetLength(textures, Length(textures)+1);
      with textures[High(textures)] do
      begin
       Resource := TextureTable[id].dfname;
       Anim := Byte(TextureTable[id].anim);
      end;

      TextureNum := High(textures);
     end;
    end;
    ind := ind+1;
   end;

   if (th[i].t >= 200) and (th[i].t < TH__LASTM) then
   begin
    t := GetMonster(th[i].t);
    if t = MONSTER_NONE then Continue;

    SetLength(monsters, Length(monsters)+1);
    with monsters[High(monsters)] do
    begin
     X := th[i].x*2-(MonsterSize[t].Width div 2);
     Y := th[i].y*2-MonsterSize[t].Height;
     MonsterType := t;
     if WordBool(th[i].f and THF_DIR) then Direction := 1 else Direction := 0;
    end;
   end;
  end;

 if textures <> nil then
 begin
  SetLength(texture_flags, Length(textures));
  for i := 0 to High(texture_flags) do texture_flags[i] := -1;

  for i := 0 to High(textures) do
   for j := 0 to High(TextureTable) do
    if TextureTable[j].dfname = textures[i].Resource then
    begin
     if (TextureTable[j].offset.X <> 0) or (TextureTable[j].offset.Y <> 0) then
      texture_flags[i] := j;
     Break;
    end;
 end;

 for i := 0 to MAXSW-1 do
  if sw[i].t = SW_NONE then Break else
  begin
   t := GetTrigger(sw[i].t);
   if t = TRIGGER_NONE then Continue;

   SetLength(triggers, Length(triggers)+1);
   with triggers[High(triggers)] do
   begin
    X := sw[i].x*16;
    Y := sw[i].y*16;
    Width := 16;
    Height := 16;
    Enabled := 1;

    TriggerType := t;

    TexturePanel := -1;
    if panels <> nil then
     for j := 0 to High(panels) do
      if WordBool(panels[j].PanelType and PANEL_BACK) and
         (((X = panels[j].X) and (Y = panels[j].Y)) or
          ((texture_flags[panels[j].TextureNum] <> -1) and
           CollideN(X, Y, Width, Height,
                    panels[j].X+TextureTable[texture_flags[panels[j].TextureNum]].offset.X,
                    panels[j].y+TextureTable[texture_flags[panels[j].TextureNum]].offset.Y,
                    panels[j].Width, panels[j].Height))) then
      begin
       str := textures[panels[j].TextureNum].Resource;
       if (Copy(str, Length(str)-5, 2) = 'SW') and
          ((Copy(str, Length(str)-1, 2) = '_0') or (Copy(str, Length(str)-1, 2) = '_1')) then
       begin
        TexturePanel := j;
        Break;
       end;
      end;

    if ByteBool(sw[i].f and SW_PL_PRESS) then ActivateType := ActivateType or ACTIVATE_PLAYERPRESS;
    if ByteBool(sw[i].f and SW_MN_PRESS) then ActivateType := ActivateType or ACTIVATE_MONSTERPRESS;
    if ByteBool(sw[i].f and SW_PL_NEAR) then ActivateType := ActivateType or ACTIVATE_PLAYERCOLLIDE;
    if ByteBool(sw[i].f and SW_MN_NEAR) then ActivateType := ActivateType or ACTIVATE_MONSTERCOLLIDE;

    if ByteBool(sw[i].f and SW_KEY_R) then Keys := Keys or KEY_RED;
    if ByteBool(sw[i].f and SW_KEY_G) then Keys := Keys or KEY_GREEN;
    if ByteBool(sw[i].f and SW_KEY_B) then Keys := Keys or KEY_BLUE;

    ZeroMemory(@tdata, SizeOf(TTriggerData));

    case TriggerType of
     TRIGGER_EXIT:
     begin
      str := ParamStr(1);
      a := Length(str);
      b := -1;
      str := Copy(str, a-4, 3);
      if str = 'MAP' then
       val(Copy(ParamStr(1), a-1, 2), a, b);

      if b = 0 then
      begin
       if sw[i].t = SW_EXITS then
        if a = 31 then b := 32
        else if a = 32 then b := 16
        else b := 31;

       if sw[i].t = SW_EXIT then
        if (a = 32) or (a = 31 ) then b := 16
        else b := a+1;

       str := Format('MAP%.2d', [b]);
       ZeroMemory(@tdata.MapName[0], 16);
       CopyMemory(@tdata.MapName[0], @str[1], 5);
      end else tdata.MapName := '';
     end;
     TRIGGER_TELEPORT:
     begin
      tdata.TargetPoint.X := sw[i].a*16+4;
      tdata.TargetPoint.Y := sw[i].b*16+16;
      tdata.d2d_teleport := True;
     end;
     TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR, TRIGGER_DOOR5,
     TRIGGER_CLOSETRAP, TRIGGER_TRAP:
     begin
      tdata.PanelID := -1;

      for j := 0 to High(panels) do
       if WordBool(panels[j].PanelType and (PANEL_OPENDOOR or PANEL_CLOSEDOOR)) and
          (sw[i].a*16+1 >=  panels[j].X) and (sw[i].a*16+1 <= panels[j].X+panels[j].Width) and
          (sw[i].b*16+1 >=  panels[j].Y) and (sw[i].b*16+1 <= panels[j].Y+panels[j].Height) then
       begin
        tdata.PanelID := j;
        Break;
       end;

      tdata.d2d_doors := True;
     end;
     TRIGGER_PRESS:
     begin
      tdata.tX := sw[i].a*16+8-24;
      tdata.tY := sw[i].b*16+8-24;
      tdata.tWidth := 48;
      tdata.tHeight := 48;
     end;
     TRIGGER_SECRET: ;
     TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT:
     begin
      tdata.PanelID := -1;

      if panels <> nil then
       for j := 0 to High(panels) do
        if WordBool(panels[j].PanelType and (PANEL_LIFTUP or PANEL_LIFTDOWN)) and
           (sw[i].a*16+1 >=  panels[j].X) and (sw[i].a*16+1 <= panels[j].X+panels[j].Width) and
           (sw[i].b*16+1 >=  panels[j].Y) and (sw[i].b*16+1 <= panels[j].Y+panels[j].Height) then
        begin
         tdata.PanelID := j;
         Break;
        end;

      tdata.d2d_doors := True;
     end;
    end;

    DATA := tdata.default;
   end;
  end;

 if (panels <> nil) and (texture_flags <> nil) then
  for i := 0 to High(panels) do
   if WordBool(panels[i].PanelType and (PANEL_BACK or PANEL_FORE)) and
      (texture_flags[panels[i].TextureNum] <> -1) then
   begin
    panels[i].X := panels[i].X+TextureTable[texture_flags[panels[i].TextureNum]].offset.X;
    panels[i].Y := panels[i].Y+TextureTable[texture_flags[panels[i].TextureNum]].offset.Y;
   end;

 SetLength(panels, ind);

 if Copy(ParamStr(1),  Length(ParamStr(1))-4, 5) = 'MAP19' then
 begin
  SetLength(triggers, Length(triggers)+2);
  a := High(triggers)-1;

  triggers[a].X := 1600-64;
  triggers[a].Y := 1600-64;
  triggers[a].Width := 16;
  triggers[a].Height := 16;
  triggers[a].Enabled := 1;
  triggers[a].TexturePanel := -1;
  triggers[a].TriggerType := TRIGGER_EXIT;
  triggers[a].ActivateType := 0;
  triggers[a].Keys := 0;
  ZeroMemory(@triggers[a].DATA[0], SizeOf(triggers[a].DATA));

  a := a+1;
  triggers[a].X := 1600-96;
  triggers[a].Y := 1600-64;
  triggers[a].Width := 16;
  triggers[a].Height := 16;
  triggers[a].Enabled := 1;
  triggers[a].TexturePanel := -1;
  triggers[a].TriggerType := TRIGGER_PRESS;
  triggers[a].ActivateType := 0;
  triggers[a].Keys := 0;
  ZeroMemory(@triggers[a].DATA[0], SizeOf(triggers[a].DATA));
  TTriggerData(triggers[a].DATA).tX := 1600-64;
  TTriggerData(triggers[a].DATA).tY := 1600-64;
  TTriggerData(triggers[a].DATA).tWidth := 16;
  TTriggerData(triggers[a].DATA).tHeight := 16;

  if monsters <> nil then
   for b := 0 to High(monsters) do
    if monsters[b].MonsterType = MONSTER_ROBO then
     TTriggerData(triggers[a].DATA).MonsterID := b+1;
 end;

 if _o then
 begin
  print(INTER_OPTIMIZATION);

  SetLength(panels2, ind);
  CopyMemory(@panels2[0], @panels[0], SizeOf(TPanelRec_1)*ind);
  panels := nil;
  
  OptimizePanels();
  pack_panels();
  OptimizeTextures();
 end;

 if _c then
 begin
  print(INTER_CORRECTION);
  Correction();
 end;

 MWriter.AddTextures(textures);
 MWriter.AddPanels(panels);
 MWriter.AddAreas(areas);
 MWriter.AddItems(items);
 MWriter.AddMonsters(monsters);
 MWriter.AddTriggers(triggers); 

 MapSize := MWriter.SaveMap(MapData);

 AssignFile(MAPFile, ParamStr(2));
 Rewrite(MAPFile, 1);
  BlockWrite(MAPFile, MapData^, MapSize);
 CloseFile(MAPFile);

 print(Format(INTER_SAVEDTO, [ParamStr(2)]));

 _end:

 if not _b then readln;
end.
