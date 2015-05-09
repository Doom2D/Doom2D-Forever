unit g_saveload;

interface

uses
  e_graphics, g_phys, g_textures, BinEditor;

function g_GetSaveName(n: Integer): String;
function g_SaveGame(n: Integer; Name: String): Boolean;
function g_LoadGame(n: Integer): Boolean;
procedure Obj_SaveState(o: PObj; Var Mem: TBinMemoryWriter);
procedure Obj_LoadState(o: PObj; Var Mem: TBinMemoryReader);

implementation

uses
  g_game, g_items, g_map, g_monsters, g_triggers,
  g_basic, windows, g_main, SysUtils, Math, WADEDITOR,
  MAPSTRUCT, MAPDEF, g_weapons, g_player, g_console,
  e_log, g_language;

const
  SAVE_SIGNATURE = $56534644; // 'DFSV'
  SAVE_VERSION = $02;
  END_MARKER_STRING = 'END';
  OBJ_SIGNATURE = $4A424F5F; // '_OBJ'
  
  
procedure Obj_SaveState(o: PObj; Var Mem: TBinMemoryWriter);
var
  sig: DWORD;

begin
  if Mem = nil then
    Exit;

// Сигнатура объекта:
  sig := OBJ_SIGNATURE; // '_OBJ'
  Mem.WriteDWORD(sig);
// Положение по-горизонтали:
  Mem.WriteInt(o^.X);
// Положение по-вертикали:
  Mem.WriteInt(o^.Y);
// Ограничивающий прямоугольник:
  Mem.WriteInt(o^.Rect.X);
  Mem.WriteInt(o^.Rect.Y);
  Mem.WriteWord(o^.Rect.Width);
  Mem.WriteWord(o^.Rect.Height);
// Скорость:
  Mem.WriteInt(o^.Vel.X);
  Mem.WriteInt(o^.Vel.Y);
// Прибавка к скорости:
  Mem.WriteInt(o^.Accel.X);
  Mem.WriteInt(o^.Accel.Y);
end;

procedure Obj_LoadState(o: PObj; Var Mem: TBinMemoryReader);
var
  sig: DWORD;

begin
  if Mem = nil then
    Exit;

// Сигнатура объекта:
  Mem.ReadDWORD(sig);
  if sig <> OBJ_SIGNATURE then // '_OBJ'
  begin
    raise EBinSizeError.Create('Obj_LoadState: Wrong Object Signature');
  end;
// Положение по-горизонтали:
  Mem.ReadInt(o^.X);
// Положение по-вертикали:
  Mem.ReadInt(o^.Y);
// Ограничивающий прямоугольник:
  Mem.ReadInt(o^.Rect.X);
  Mem.ReadInt(o^.Rect.Y);
  Mem.ReadWord(o^.Rect.Width);
  Mem.ReadWord(o^.Rect.Height);
// Скорость:
  Mem.ReadInt(o^.Vel.X);
  Mem.ReadInt(o^.Vel.Y);
// Прибавка к скорости:
  Mem.ReadInt(o^.Accel.X);
  Mem.ReadInt(o^.Accel.Y);
end;

function g_GetSaveName(n: Integer): String;
var
  bFile: TBinFileReader;
  bMem: TBinMemoryReader;
  str: String;

begin
  Result := '';
  str := '';
  bMem := nil;
  bFile := nil;

  try
  // Открываем файл сохранений:
    bFile := TBinFileReader.Create();
    if bFile.OpenFile(DataDir + 'SAVGAME' + IntToStr(n) + '.DAT',
                      SAVE_SIGNATURE, SAVE_VERSION) then
    begin
    // Читаем первый блок - состояние игры:
      bMem := TBinMemoryReader.Create();
      bFile.ReadMemory(bMem);
    // Имя игры:
      bMem.ReadString(str);

    // Закрываем файл:
      bFile.CloseFile();
    end;

  except
    on E1: EInOutError do
      e_WriteLog('GetSaveName I/O Error: '+E1.Message, MSG_WARNING);
    on E2: EBinSizeError do
      e_WriteLog('GetSaveName Size Error: '+E2.Message, MSG_WARNING);
  end;

  bMem.Free();
  bFile.Free();

  Result := str;
end;

function g_SaveGame(n: Integer; Name: String): Boolean;
var
  bFile: TBinFileWriter;
  bMem: TBinMemoryWriter;
  str: String;
  nPlayers: Byte;
  i, k: Integer;

begin
  Result := False;
  bMem := nil;
  bFile := nil;

  try
  // Создаем файл сохранения:
    bFile := TBinFileWriter.Create();
    bFile.OpenFile(DataDir + 'SAVGAME' + IntToStr(n) + '.DAT',
                   SAVE_SIGNATURE, SAVE_VERSION);

  ///// Получаем состояние игры: /////
    bMem := TBinMemoryWriter.Create(256);
  // Имя игры:
    bMem.WriteString(Name, 32);
  // Путь к карте:
    str := ExtractRelativePath(MapsDir, gGameSettings.WAD);
    bMem.WriteString(str, 128);
  // Имя карты:
    g_ProcessResourceStr(gMapInfo.Map, nil, nil, @str);
    bMem.WriteString(str, 32);
  // Количество игроков:
    nPlayers := g_Player_GetCount();
    bMem.WriteByte(nPlayers);
  // Игровое время:
    bMem.WriteDWORD(gTime);
  // Тип игры:
    bMem.WriteByte(gGameSettings.GameType);
  // Режим игры:
    bMem.WriteByte(gGameSettings.GameMode);
  // Лимит времени:
    bMem.WriteWord(gGameSettings.TimeLimit);
  // Лимит очков:
    bMem.WriteWord(gGameSettings.GoalLimit);
  // Игровые опции:
    bMem.WriteDWORD(gGameSettings.Options);
  // Сохраняем состояние игры:
    bFile.WriteMemory(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////

  ///// Получаем состояние карты: /////
    g_Map_SaveState(bMem);
  // Сохраняем состояние карты:
    bFile.WriteMemory(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////

  ///// Получаем состояние предметов: /////
    g_Items_SaveState(bMem);
  // Сохраняем состояние предметов:
    bFile.WriteMemory(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////

  ///// Получаем состояние триггеров: /////
    g_Triggers_SaveState(bMem);
  // Сохраняем состояние триггеров:
    bFile.WriteMemory(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////
  
  ///// Получаем состояние оружия: /////
    g_Weapon_SaveState(bMem);
  // Сохраняем состояние оружия:
    bFile.WriteMemory(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////

  ///// Получаем состояние монстров: /////
    g_Monsters_SaveState(bMem);
  // Сохраняем состояние монстров:
    bFile.WriteMemory(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////

  ///// Получаем состояние трупов: /////
    g_Player_Corpses_SaveState(bMem);
  // Сохраняем состояние трупов:
    bFile.WriteMemory(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////

  ///// Сохраняем игроков (в том числе ботов): /////
    if nPlayers > 0 then
    begin
      k := 0;
      for i := 0 to High(gPlayers) do
        if gPlayers[i] <> nil then
        begin
        // Получаем состояние игрока:
          gPlayers[i].SaveState(bMem);
        // Сохраняем состояние игрока:
          bFile.WriteMemory(bMem);
          bMem.Free();
          bMem := nil;
          Inc(k);
        end;
        
    // Все ли игроки на месте:
      if k <> nPlayers then
      begin
        raise EInOutError.Create('g_SaveGame: Wrong Players Count');
      end;
    end;
  ///// /////

  ///// Маркер окончания: /////
    bMem := TBinMemoryWriter.Create(4);
  // Строка - обозначение конца:
    str := END_MARKER_STRING; // 'END'
    bMem.WriteString(str, 3);
  // Сохраняем маркер окончания:
    bFile.WriteMemory(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////

  // Закрываем файл сохранения:
    bFile.CloseFile();
    Result := True;

  except
    on E1: EInOutError do
      begin
        g_Console_Add(_lc[I_GAME_ERROR_SAVE]);
        e_WriteLog('SaveState I/O Error: '+E1.Message, MSG_WARNING);
      end;
    on E2: EBinSizeError do
      begin
        g_Console_Add(_lc[I_GAME_ERROR_SAVE]);
        e_WriteLog('SaveState Size Error: '+E2.Message, MSG_WARNING);
      end;
  end;

  bMem.Free();
  bFile.Free();
end;

function g_LoadGame(n: Integer): Boolean;
var
  bFile: TBinFileReader;
  bMem: TBinMemoryReader;
  str, WAD_Path, Map_Name: String;
  nPlayers, Game_Type, Game_Mode: Byte;
  Game_TimeLimit, Game_GoalLimit: Word;
  Game_Time, Game_Options: Cardinal;
  i: Integer;

begin
  Result := False;
  bMem := nil;
  bFile := nil;

  try
  // Открываем файл с сохранением:
    bFile := TBinFileReader.Create();
    if not bFile.OpenFile(DataDir + 'SAVGAME' + IntToStr(n) + '.DAT',
                          SAVE_SIGNATURE, SAVE_VERSION) then
    begin
      bFile.Free();
      Exit;
    end;

    g_Game_ClearLoading();
    g_Game_SetLoadingText(_lc[I_LOAD_SAVE_FILE], 0, False);
    gLoadGameMode := True;

  ///// Загружаем состояние игры: /////
    bMem := TBinMemoryReader.Create();
    bFile.ReadMemory(bMem);
  // Имя игры:
    bMem.ReadString(str);
  // Путь к карте:
    bMem.ReadString(WAD_Path);
  // Имя карты:
    bMem.ReadString(Map_Name);
  // Количество игроков:
    bMem.ReadByte(nPlayers);
  // Игровое время:
    bMem.ReadDWORD(Game_Time);
  // Тип игры:
    bMem.ReadByte(Game_Type);
  // Режим игры:
    bMem.ReadByte(Game_Mode);
  // Лимит времени:
    bMem.ReadWord(Game_TimeLimit);
  // Лимит очков:
    bMem.ReadWord(Game_GoalLimit);
  // Игровые опции:
    bMem.ReadDWORD(Game_Options);
  // Cостояние игры загружено:
    bMem.Free();
    bMem := nil;
  ///// /////
  
  // Загружаем карту:
    if (Game_Type = GT_NONE) or (Game_Type = GT_SINGLE) then
      g_Game_StartSingle(MapsDir + WAD_Path, Map_Name,
                         LongBool(Game_Options and GAME_OPTION_TWOPLAYER),
                         nPlayers)
    else
      g_Game_StartCustom(MapsDir + WAD_Path + ':\' + Map_Name,
                         Game_Mode, Game_TimeLimit,
                         Game_GoalLimit, Game_Options,
                         nPlayers);
  // Устанавливаем время:
    gTime := Game_Time;

  ///// Загружаем состояние карты: /////
    bMem := TBinMemoryReader.Create();
    bFile.ReadMemory(bMem);
  // Состояние карты:
    g_Map_LoadState(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////

  ///// Загружаем состояние предметов: /////
    bMem := TBinMemoryReader.Create();
    bFile.ReadMemory(bMem);
  // Состояние предметов:
    g_Items_LoadState(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////

  ///// Загружаем состояние триггеров: /////
    bMem := TBinMemoryReader.Create();
    bFile.ReadMemory(bMem);
  // Состояние триггеров:
    g_Triggers_LoadState(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////

  ///// Загружаем состояние оружия: /////
    bMem := TBinMemoryReader.Create();
    bFile.ReadMemory(bMem);
  // Состояние оружия:
    g_Weapon_LoadState(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////

  ///// Загружаем состояние монстров: /////
    bMem := TBinMemoryReader.Create();
    bFile.ReadMemory(bMem);
  // Состояние монстров:
    g_Monsters_LoadState(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////

  ///// Загружаем состояние трупов: /////
    bMem := TBinMemoryReader.Create();
    bFile.ReadMemory(bMem);
  // Состояние трупов:
    g_Player_Corpses_LoadState(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////

  ///// Загружаем игроков (в том числе ботов): /////
    if nPlayers > 0 then
    begin
    // Есть ли вообще игроки:
      if gPlayers = nil then
      begin
        raise EInOutError.Create('g_LoadGame: No Players');
      end;
      
    // Загружаем:
      for i := 0 to nPlayers-1 do
      begin
      // Загружаем состояние игрока:
        bMem := TBinMemoryReader.Create();
        bFile.ReadMemory(bMem);
      // Состояние игрока/бота:
        if gPlayers[i] = nil then
        begin
          raise EInOutError.Create('g_LoadGame: Nil Player');
        end;
        gPlayers[i].LoadState(bMem);
        bMem.Free();
        bMem := nil;
      end;
    end;
  ///// /////

  ///// Маркер окончания: /////
    bMem := TBinMemoryReader.Create();
    bFile.ReadMemory(bMem);
  // Строка - обозначение конца:
    bMem.ReadString(str);
    if str <> END_MARKER_STRING then // 'END'
    begin
      raise EInOutError.Create('g_LoadGame: No END Marker');
    end;
  // Маркер окончания загружен:
    bMem.Free();
    bMem := nil;
  ///// /////

  // Ищем триггеры с условием смерти монстров:
    if (gMonsters <> nil) and (gTriggers <> nil) then
      g_Map_ReAdd_DieTriggers();

  // Закрываем файл загрузки:
    bFile.CloseFile();
    gLoadGameMode := False;
    Result := True;

  except
    on E1: EInOutError do
      begin
        g_Console_Add(_lc[I_GAME_ERROR_LOAD]);
        e_WriteLog('LoadState I/O Error: '+E1.Message, MSG_WARNING);
      end;
    on E2: EBinSizeError do
      begin
        g_Console_Add(_lc[I_GAME_ERROR_LOAD]);
        e_WriteLog('LoadState Size Error: '+E2.Message, MSG_WARNING);
      end;
  end;

  bMem.Free();
  bFile.Free();
end;

end.
