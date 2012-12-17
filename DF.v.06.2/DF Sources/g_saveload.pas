unit g_saveload;

interface

uses
  e_graphics, g_phys, g_textures;

type
  PObjRec = ^TObjRec;
  TObjRec = packed record
   X, Y: Integer;
   Rect: TRectWH;
   Vel: TPoint2i;
   Accel: TPoint2i;
  end;

function g_GetSaveName(n: Integer): string;
procedure g_SaveGame(n: Integer; Name: string);
function g_LoadGame(n: Integer): Boolean;
procedure saveobj(o: PObj; orec: PObjRec);
procedure loadobj(orec: PObjRec; o: PObj);

implementation

uses
  g_game, g_items, g_map, g_monsters, g_triggers, g_basic, windows,
  g_main, SysUtils, Math, WADEDITOR, MAPSTRUCT, MAPDEF, g_weapons,
  g_player, inter;

const
  SIGNATURE = 'DFSAVE';
  VERSION = $02;
  BLOCK_NONE    = 0;
  BLOCK_GAME    = 1;
  BLOCK_MAP     = 2;
  BLOCK_ITEM    = 3;
  BLOCK_PLAYER1 = 4;
  BLOCK_PLAYER2 = 5;
  BLOCK_TRIGGER = 6;
  BLOCK_WEAPON  = 7;
  BLOCK_MONSTER = 8;
  BLOCK_PLAYERS = 9;//bots

type
  TBlock = packed record
   BlockType: Byte;
   Reserved:  LongWord;
   BlockSize: LongWord;
  end;

  PGameRec = ^TGameRec;
  TGameRec = packed record
   Name: Char16;
   WAD: Char64;
   Map: Char16;
   MapCRC32: LongWord;
   TwoPlayers: Boolean;
   Time: LongWord;
   GameType: Byte;
   GameMode: Byte;//DM only
   TimeLimit: Byte;
   GoalLimit: Byte;
   Options: LongWord;
   reserved: array[0..247] of Byte;
  end;

procedure saveobj(o: PObj; orec: PObjRec);
begin
 orec^.X := o^.X;
 orec^.Y := o^.Y;
 orec^.Rect := o^.Rect;
 orec^.Vel := o^.Vel;
 orec^.Accel := o^.Accel;
end;

procedure loadobj(orec: PObjRec; o: PObj);
begin
 o^.X := orec^.X;
 o^.Y := orec^.Y;
 o^.Rect := orec^.Rect;
 o^.Vel := orec^.Vel;
 o^.Accel := orec^.Accel;
end;

function g_GetSaveName(n: Integer): string;
var
  f: File;
  sign: array[0..5] of Char;
  b: Byte;
  FileName: string;
  game: TGameRec;
  block: TBlock;
begin
 Result := '';
 
 FileName := DataDir+'SAVGAME'+IntToStr(n)+'.DAT';
 if not FileExists(FileName) then Exit;


 AssignFile(f, FileName);
 Reset(f, 1);
  BlockRead(f, sign[0], 6);
  BlockRead(f, b, 1);

  if (sign <> SIGNATURE) or (b <> VERSION) then
  begin
   CloseFile(f);
   Exit;
  end;

  BlockRead(f, block, SizeOf(TBlock));
  if block.BlockType <> BLOCK_GAME then
  begin
   CloseFile(f);
   Exit;
  end;
  BlockRead(f, game, SizeOf(TGameRec));

  Result := game.Name;
 CloseFile(f);
end;

procedure g_SaveGame(n: Integer; Name: string);
var
  f: File;
  ver: Byte;
  str: string;
  p: Pointer;
  game: TGameRec;
  player: TPlayerSaveRec;
  block: TBlock;
begin
 ver := VERSION;

 ZeroMemory(@game, SizeOf(TGameRec));
 if Name <> '' then CopyMemory(@game.Name[0], @Name[1], Min(16, Length(Name)));

 str := ExtractRelativePath(MapsDir, gGameSettings.WAD);
 CopyMemory(@game.WAD[0], @str[1], Min(64, Length(str)));

 g_ProcessResourceStr(gMapInfo.Map, nil, nil, @str);
 CopyMemory(@game.Map[0], @str[1], Min(16, Length(str)));

 game.TwoPlayers := LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER);
 game.Time := gTime;
 game.GameType := gGameSettings.GameType;
 game.GameMode := gGameSettings.GameMode;
 game.TimeLimit := gGameSettings.TimeLimit;
 game.GoalLimit := gGameSettings.GoalLimit;
 game.Options := gGameSettings.Options;

 AssignFile(f, DataDir+'SAVGAME'+IntToStr(n)+'.DAT');
 Rewrite(f, 1);
  BlockWrite(f, SIGNATURE, Length(SIGNATURE));
  BlockWrite(f, ver, 1);

  block.BlockType := BLOCK_GAME;
  block.Reserved := 0;
  block.BlockSize := Sizeof(TGameRec);
  BlockWrite(f, block, SizeOf(TBlock));
  BlockWrite(f, game, block.BlockSize);

  block.BlockType := BLOCK_MAP;
  block.Reserved := 0;
  block.BlockSize := g_Map_SaveState(p);
  if block.BlockSize > 0 then
  begin
   BlockWrite(f, block, SizeOf(TBlock));
   BlockWrite(f, p^, block.BlockSize);
   FreeMem(p);
  end;

  block.BlockType := BLOCK_ITEM;
  block.Reserved := 0;
  block.BlockSize := g_Items_Save(p);
  if block.BlockSize > 0 then
  begin
   BlockWrite(f, block, SizeOf(TBlock));
   BlockWrite(f, p^, block.BlockSize);
   FreeMem(p);
  end;

  block.BlockType := BLOCK_TRIGGER;
  block.Reserved := 0;
  block.BlockSize := g_Triggers_Save(p);
  if block.BlockSize > 0 then
  begin
   BlockWrite(f, block, SizeOf(TBlock));
   BlockWrite(f, p^, block.BlockSize);
   FreeMem(p);
  end;

  block.BlockType := BLOCK_WEAPON;
  block.Reserved := 0;
  block.BlockSize := g_Weapon_Save(p);
  if block.BlockSize > 0 then
  begin
   BlockWrite(f, block, SizeOf(TBlock));
   BlockWrite(f, p^, block.BlockSize);
   FreeMem(p);
  end;

  block.BlockType := BLOCK_MONSTER;
  block.Reserved := 0;
  block.BlockSize := g_Monsters_Save(p);
  if block.BlockSize > 0 then
  begin
   BlockWrite(f, block, SizeOf(TBlock));
   BlockWrite(f, p^, block.BlockSize);
   FreeMem(p);
  end;

  block.BlockType := BLOCK_PLAYER1;
  block.Reserved := 0;
  block.BlockSize := SizeOf(TPlayerSaveRec);
  gPlayer1.Save(@player);
  BlockWrite(f, block, SizeOf(TBlock));
  BlockWrite(f, player, block.BlockSize);

  if game.TwoPlayers then
  begin
   block.BlockType := BLOCK_PLAYER2;
   block.Reserved := 0;
   block.BlockSize := SizeOf(TPlayerSaveRec);
   gPlayer2.Save(@player);
   BlockWrite(f, block, SizeOf(TBlock));
   BlockWrite(f, player, block.BlockSize);
  end;

  { TODO 5 : сохранение ботов }

  block.BlockType := BLOCK_NONE;
  block.Reserved := 0;
  block.BlockSize := 0;
  BlockWrite(f, block, SizeOf(TBlock));

 CloseFile(f);
end;

function g_LoadGame(n: Integer): Boolean;
var
  f: File;
  sign: array[0..5] of Char;
  b: Byte;
  FileName: string;
  p: Pointer;
  game: TGameRec;
  player: TPlayerSaveRec;
  block: TBlock;
begin
 Result := False;
 
 FileName := DataDir+'SAVGAME'+IntToStr(n)+'.DAT';
 if not FileExists(FileName) then Exit;

 g_Game_ClearLoading();
 g_Game_SetLoadingText(I_LOAD_SAVE_FILE, 0);

 AssignFile(f, FileName);
 Reset(f, 1);
  BlockRead(f, sign[0], 6);
  BlockRead(f, b, 1);

  if (sign <> SIGNATURE) or (b <> VERSION) then
  begin
   CloseFile(f);
   Exit;
  end;

  BlockRead(f, block, SizeOf(TBlock));
  if block.BlockType <> BLOCK_GAME then
  begin
   CloseFile(f);
   Exit;
  end;
  BlockRead(f, game, SizeOf(TGameRec));

  gLoadGameMode := True;
  if (game.GameType = GT_NONE) or (game.GameType = GT_SINGLE) then
   g_Game_StartSingle(MapsDir+game.WAD, game.Map, game.TwoPlayers)
  else
   g_Game_StartCustom(MapsDir+game.WAD+':\'+game.Map, game.GameMode,
                      game.TimeLimit, game.GoalLimit, game.Options);
  gTime := game.Time;

  repeat
   BlockRead(f, block, SizeOf(TBlock));
   case block.BlockType of
    BLOCK_MAP:
    begin
     g_Game_SetLoadingText(I_LOAD_MAP_STATE, 0);
     p := GetMemory(block.BlockSize);
     BlockRead(f, p^, block.BlockSize);
     g_Map_LoadState(p, block.BlockSize);
     FreeMem(p);
    end;

    BLOCK_ITEM:
    begin
     g_Game_SetLoadingText(I_LOAD_ITEMS_STATE, 0);
     p := GetMemory(block.BlockSize);
     BlockRead(f, p^, block.BlockSize);
     g_Items_Load(p, block.BlockSize);
     FreeMem(p);
    end;

    BLOCK_TRIGGER:
    begin
     g_Game_SetLoadingText(I_LOAD_TRIGGERS_STATE, 0);
     p := GetMemory(block.BlockSize);
     BlockRead(f, p^, block.BlockSize);
     g_Triggers_Load(p, block.BlockSize);
     FreeMem(p);
    end;

    BLOCK_WEAPON:
    begin
     g_Game_SetLoadingText(I_LOAD_WEAPONS_STATE, 0);
     p := GetMemory(block.BlockSize);
     BlockRead(f, p^, block.BlockSize);
     g_Weapon_Load(p, block.BlockSize);
     FreeMem(p);
    end;

    BLOCK_MONSTER:
    begin
     g_Game_SetLoadingText(I_LOAD_MONSTERS_STATE, 0);
     p := GetMemory(block.BlockSize);
     BlockRead(f, p^, block.BlockSize);
     g_Monsters_Load(p, block.BlockSize);
     FreeMem(p);
    end;

    BLOCK_PLAYER1:
    begin
     BlockRead(f, player, SizeOf(TPlayerSaveRec));
     gPlayer1.Load(@player);
    end;

    BLOCK_PLAYER2:
    begin
     BlockRead(f, player, SizeOf(TPlayerSaveRec));
     gPlayer2.Load(@player);
    end;

    else Seek(f, block.BlockSize);
   end;
  until block.BlockType = 0;

  if (gMonsters <> nil) and (gTriggers <> nil) then
    g_Map_ReAdd_DieTriggers;

  gLoadGameMode := False;
 CloseFile(f);

 Result := True;
end;

end.
