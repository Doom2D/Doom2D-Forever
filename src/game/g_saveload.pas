(* Copyright (C)  DooM 2D:Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$MODE DELPHI}
unit g_saveload;

interface

uses
  e_graphics, g_phys, g_textures, BinEditor;

function g_GetSaveName(n: Integer): String;
function g_SaveGame(n: Integer; Name: String): Boolean;
function g_LoadGame(n: Integer): Boolean;
procedure Obj_SaveState(o: PObj; var Mem: TBinMemoryWriter);
procedure Obj_LoadState(o: PObj; var Mem: TBinMemoryReader);

implementation

uses
  g_game, g_items, g_map, g_monsters, g_triggers,
  g_basic, g_main, SysUtils, Math, wadreader,
  MAPSTRUCT, MAPDEF, g_weapons, g_player, g_console,
  e_log, g_language;

const
  SAVE_SIGNATURE = $56534644; // 'DFSV'
  SAVE_VERSION = $02;
  END_MARKER_STRING = 'END';
  PLAYER_VIEW_SIGNATURE = $57564C50; // 'PLVW'
  OBJ_SIGNATURE = $4A424F5F; // '_OBJ'

procedure Obj_SaveState(o: PObj; var Mem: TBinMemoryWriter);
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

procedure Obj_LoadState(o: PObj; var Mem: TBinMemoryReader);
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
      bFile.Close();
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
  sig: DWORD;
  str: String;
  nPlayers: Byte;
  i, k: Integer;
  PID1, PID2: Word;
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
    str := gGameSettings.WAD;
    bMem.WriteString(str, 128);
  // Имя карты:
    str := g_ExtractFileName(gMapInfo.Map);
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
  // Лимит жизней:
    bMem.WriteByte(gGameSettings.MaxLives);
  // Игровые опции:
    bMem.WriteDWORD(gGameSettings.Options);
  // Для коопа:
    bMem.WriteWord(gCoopMonstersKilled);
    bMem.WriteWord(gCoopSecretsFound);
    bMem.WriteWord(gCoopTotalMonstersKilled);
    bMem.WriteWord(gCoopTotalSecretsFound);
    bMem.WriteWord(gCoopTotalMonsters);
    bMem.WriteWord(gCoopTotalSecrets);
  // Сохраняем состояние игры:
    bFile.WriteMemory(bMem);
    bMem.Free();
    bMem := nil;
  ///// /////

  ///// Сохраняем состояние областей просмотра: /////
    bMem := TBinMemoryWriter.Create(8);
    sig := PLAYER_VIEW_SIGNATURE;
    bMem.WriteDWORD(sig); // 'PLVW'
    PID1 := 0;
    PID2 := 0;
    if gPlayer1 <> nil then
      PID1 := gPlayer1.UID;
    if gPlayer2 <> nil then
      PID2 := gPlayer2.UID;
    bMem.WriteWord(PID1);
    bMem.WriteWord(PID2);
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
    bFile.Close();
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
  sig: DWORD;
  str, WAD_Path, Map_Name: String;
  nPlayers, Game_Type, Game_Mode, Game_MaxLives: Byte;
  Game_TimeLimit, Game_GoalLimit: Word;
  Game_Time, Game_Options: Cardinal;
  Game_CoopMonstersKilled,
  Game_CoopSecretsFound,
  Game_CoopTotalMonstersKilled,
  Game_CoopTotalSecretsFound,
  Game_CoopTotalMonsters,
  Game_CoopTotalSecrets,
  PID1, PID2: Word;
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

    e_WriteLog('Loading saved game...', MSG_NOTIFY);
    g_Game_Free();

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
  // Лимит жизней:
    bMem.ReadByte(Game_MaxLives);
  // Игровые опции:
    bMem.ReadDWORD(Game_Options);
  // Для коопа:
    bMem.ReadWord(Game_CoopMonstersKilled);
    bMem.ReadWord(Game_CoopSecretsFound);
    bMem.ReadWord(Game_CoopTotalMonstersKilled);
    bMem.ReadWord(Game_CoopTotalSecretsFound);
    bMem.ReadWord(Game_CoopTotalMonsters);
    bMem.ReadWord(Game_CoopTotalSecrets);
  // Cостояние игры загружено:
    bMem.Free();
    bMem := nil;
  ///// /////

  ///// Загружаем состояние областей просмотра: /////
    bMem := TBinMemoryReader.Create();
    bFile.ReadMemory(bMem);
    bMem.ReadDWORD(sig);
    if sig <> PLAYER_VIEW_SIGNATURE then // 'PLVW'
    begin
      raise EInOutError.Create('g_LoadGame: Wrong Player View Signature');
    end;
    bMem.ReadWord(PID1);
    bMem.ReadWord(PID2);
    bMem.Free();
    bMem := nil;
  ///// /////

  // Загружаем карту:
    ZeroMemory(@gGameSettings, SizeOf(TGameSettings));
    gAimLine := False;
    gShowMap := False;
    if (Game_Type = GT_NONE) or (Game_Type = GT_SINGLE) then
    begin
    // Настройки игры:
      gGameSettings.GameType := GT_SINGLE;
      gGameSettings.MaxLives := 0;
      gGameSettings.Options := gGameSettings.Options + GAME_OPTION_ALLOWEXIT;
      gGameSettings.Options := gGameSettings.Options + GAME_OPTION_MONSTERS;
      gGameSettings.Options := gGameSettings.Options + GAME_OPTION_BOTVSMONSTER;
      gSwitchGameMode := GM_SINGLE;
    end
    else
    begin
    // Настройки игры:
      gGameSettings.GameType := GT_CUSTOM;
      gGameSettings.GameMode := Game_Mode;
      gSwitchGameMode := Game_Mode;
      gGameSettings.TimeLimit := Game_TimeLimit;
      gGameSettings.GoalLimit := Game_GoalLimit;
      gGameSettings.MaxLives := IfThen(Game_Mode = GM_CTF, 0, Game_MaxLives);
      gGameSettings.Options := Game_Options;
    end;
    g_Game_ExecuteEvent('ongamestart');

  // Установка размеров окон игроков:
    g_Game_SetupScreenSize();

  // Загрузка и запуск карты:
    if not g_Game_StartMap(WAD_Path + ':\' + Map_Name, True) then
    begin
      g_FatalError(Format(_lc[I_GAME_ERROR_MAP_LOAD], [WAD_Path + ':\' + Map_Name]));
      Exit;
    end;

  // Настройки игроков и ботов:
    g_Player_Init();

  // Устанавливаем время:
    gTime := Game_Time;
  // Возвращаем статы:
    gCoopMonstersKilled := Game_CoopMonstersKilled;
    gCoopSecretsFound := Game_CoopSecretsFound;
    gCoopTotalMonstersKilled := Game_CoopTotalMonstersKilled;
    gCoopTotalSecretsFound := Game_CoopTotalSecretsFound;
    gCoopTotalMonsters := Game_CoopTotalMonsters;
    gCoopTotalSecrets := Game_CoopTotalSecrets;

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
    // Загружаем:
      for i := 0 to nPlayers-1 do
      begin
      // Загружаем состояние игрока:
        bMem := TBinMemoryReader.Create();
        bFile.ReadMemory(bMem);
      // Состояние игрока/бота:
        g_Player_CreateFromState(bMem);
        bMem.Free();
        bMem := nil;
      end;
    end;
  // Привязываем основных игроков к областям просмотра:
    gPlayer1 := g_Player_Get(PID1);
    gPlayer2 := g_Player_Get(PID2);
    if gPlayer1 <> nil then
    begin
      gPlayer1.Name := gPlayer1Settings.Name;
      gPlayer1.FPreferredTeam := gPlayer1Settings.Team;
      gPlayer1.FActualModelName := gPlayer1Settings.Model;
      gPlayer1.SetModel(gPlayer1.FActualModelName);
      gPlayer1.SetColor(gPlayer1Settings.Color);
    end;
    if gPlayer2 <> nil then
    begin
      gPlayer2.Name := gPlayer2Settings.Name;
      gPlayer2.FPreferredTeam := gPlayer2Settings.Team;
      gPlayer2.FActualModelName := gPlayer2Settings.Model;
      gPlayer2.SetModel(gPlayer2.FActualModelName);
      gPlayer2.SetColor(gPlayer2Settings.Color);
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
    bFile.Close();
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
