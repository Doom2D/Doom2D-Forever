(* Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
 * This file is part of Doom2D Forever.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation, version 3 of
 * the License ONLY.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 *)

{$INCLUDE ../shared/a_modes.inc}
unit g_saveload;

interface

uses
  SysUtils, Classes,
  e_graphics, g_phys, g_textures;


function g_GetSaveName (n: Integer; out valid: Boolean): AnsiString;

function g_SaveGameTo (const filename: AnsiString; const aname: AnsiString; deleteOnError: Boolean=true): Boolean;
function g_LoadGameFrom (const filename: AnsiString): Boolean;

function g_SaveGame (n: Integer; const aname: AnsiString): Boolean;
function g_LoadGame (n: Integer): Boolean;

procedure Obj_SaveState (st: TStream; o: PObj);
procedure Obj_LoadState (o: PObj; st: TStream);


implementation

uses
  MAPDEF, utils, xstreams,
  g_game, g_items, g_map, g_monsters, g_triggers,
  g_basic, g_main, Math, wadreader,
  g_weapons, g_player, g_console,
  e_log, e_res, g_language;

const
  SAVE_SIGNATURE = $56534644; // 'DFSV'
  SAVE_VERSION = $07;
  END_MARKER_STRING = 'END';
  PLAYER_VIEW_SIGNATURE = $57564C50; // 'PLVW'
  OBJ_SIGNATURE = $4A424F5F; // '_OBJ'


procedure Obj_SaveState (st: TStream; o: PObj);
begin
  if st = nil then Exit;

  // Сигнатура объекта
  utils.writeSign(st, '_OBJ');
  st.WriteByte(0);  // version

  // Положение
  st.WriteInt32LE(o^.X);
  st.WriteInt32LE(o^.Y);

  // Ограничивающий прямоугольник
  st.WriteInt32LE(o^.Rect.X);
  st.WriteInt32LE(o^.Rect.Y);
  st.WriteWordLE(o^.Rect.Width);
  st.WriteWordLE(o^.Rect.Height);

  // Скорость
  st.WriteInt32LE(o^.Vel.X);
  st.WriteInt32LE(o^.Vel.Y);

  // Ускорение
  st.WriteInt32LE(o^.Accel.X);
  st.WriteInt32LE(o^.Accel.Y);
end;


procedure Obj_LoadState (o: PObj; st: TStream);
begin
  if st = nil then Exit;

  // Сигнатура объекта:
  if not utils.checkSign(st, '_OBJ') then
    Raise XStreamError.Create('invalid object signature');
  if st.ReadByte() <> 0 then
    Raise XStreamError.Create('invalid object version');

  // Положение
  o^.X := st.ReadInt32LE();
  o^.Y := st.ReadInt32LE();

  // Ограничивающий прямоугольник
  o^.Rect.X := st.ReadInt32LE();
  o^.Rect.Y := st.ReadInt32LE();
  o^.Rect.Width := st.ReadWordLE();
  o^.Rect.Height := st.ReadWordLE();

  // Скорость
  o^.Vel.X := st.ReadInt32LE();
  o^.Vel.Y := st.ReadInt32LE();

  // Ускорение
  o^.Accel.X := st.ReadInt32LE();
  o^.Accel.Y := st.ReadInt32LE();
end;


function buildSaveName (n: Integer): AnsiString;
begin
  result := 'SAVGAME' + IntToStr(n) + '.DAT'
end;


function g_GetSaveName (n: Integer; out valid: Boolean): AnsiString;
var
  st: TStream = nil;
  ver: Byte;
  stlen: Word;
  filename: AnsiString;
begin
  valid := false;
  result := '';
  if (n < 0) or (n > 65535) then exit;
  try
    // Открываем файл сохранений
    filename := buildSaveName(n);
    st := e_OpenResourceRO(SaveDirs, filename);
    try
      if not utils.checkSign(st, 'DFSV') then
      begin
        e_LogWritefln('GetSaveName: not a save file: ''%s''', [st], TMsgType.Warning);
        //raise XStreamError.Create('invalid save game signature');
        exit;
      end;
      ver := st.ReadByte();
      if ver < 7 then
      begin
        st.ReadDWordLE();  // section size
        stlen := st.ReadWordLE();
        if (stlen < 1) or (stlen > 64) then
        begin
          e_LogWritefln('GetSaveName: not a save file: ''%s''', [st], TMsgType.Warning);
          //raise XStreamError.Create('invalid save game version');
          exit;
        end;
        // Имя сэйва
        SetLength(result, stlen);
        st.ReadBuffer(result[1], stlen);
      end
      else
      begin
        // 7+
        // Имя сэйва
        result := utils.readStr(st, 64);
      end;
      valid := (ver = SAVE_VERSION);
      //if (utils.readByte(st) <> SAVE_VERSION) then raise XStreamError.Create('invalid save game version');
    finally
      st.Free();
    end;
  except
    begin
      //e_WriteLog('GetSaveName Error: '+e.message, MSG_WARNING);
      //{$IF DEFINED(D2F_DEBUG)}e_WriteStackTrace(e.message);{$ENDIF}
      result := '';
    end;
  end;
end;


function g_SaveGameTo (const filename: AnsiString; const aname: AnsiString; deleteOnError: Boolean=true): Boolean;
var
  st: TStream = nil;
  i, k: Integer;
  PID1, PID2: Word;
begin
  Result := False;
  try
    st := e_CreateResource(SaveDirs, filename);
    try
      utils.writeSign(st, 'DFSV');
      st.WriteByte(SAVE_VERSION);
      utils.writeStr(st, aname, 64);  // Имя сэйва

      // Полный путь к ваду и карта
      //if (Length(gCurrentMapFileName) <> 0) then e_LogWritefln('SAVE: current map is ''%s''...', [gCurrentMapFileName]);
      utils.writeStr(st, gCurrentMapFileName);

      utils.writeStr(st, ExtractFileName(gGameSettings.WAD));  // Путь к карте
      utils.writeStr(st, g_ExtractFileName(gMapInfo.Map));  // Имя карты
      st.WriteWordLE(g_Player_GetCount);  // Количество игроков
      st.WriteDWordLE(gTime);  // Игровое время
      st.WriteByte(gGameSettings.GameType);  // Тип игры
      st.WriteByte(gGameSettings.GameMode);  // Режим игры
      st.WriteWordLE(gGameSettings.TimeLimit);  // Лимит времени
      st.WriteWordLE(gGameSettings.ScoreLimit);  // Лимит очков
      st.WriteByte(gGameSettings.MaxLives);  // Лимит жизней
      st.WriteDWordLE(LongWord(gGameSettings.Options));  // Игровые опции

      // Для коопа
      st.WriteWordLE(gCoopMonstersKilled);
      st.WriteWordLE(gCoopSecretsFound);
      st.WriteWordLE(gCoopTotalMonstersKilled);
      st.WriteWordLE(gCoopTotalSecretsFound);
      st.WriteWordLE(gCoopTotalMonsters);
      st.WriteWordLE(gCoopTotalSecrets);

      ///// Сохраняем состояние областей просмотра /////
      utils.writeSign(st, 'PLVW');
      st.WriteByte(0);  // version
      PID1 := 0;
      PID2 := 0;
      if (gPlayer1 <> nil) then PID1 := gPlayer1.UID;
      if (gPlayer2 <> nil) then PID2 := gPlayer2.UID;
      st.WriteWordLE(PID1);
      st.WriteWordLE(PID2);
      ///// /////

      g_Map_SaveState(st);  // Состояние карты
      g_Items_SaveState(st);  // Состояние предметов
      g_Triggers_SaveState(st);  // Состояние триггеров
      g_Weapon_SaveState(st);  // Состояние оружия
      g_Monsters_SaveState(st);  // Состояние монстров
      g_Player_Corpses_SaveState(st);  // Состояние трупов

      ///// Сохраняем игроков (в том числе ботов) /////
      if g_Player_GetCount > 0 then
      begin
        k := 0;
        for i := 0 to High(gPlayers) do
        begin
          if gPlayers[i] <> nil then
          begin
            // Состояние игрока
            gPlayers[i].SaveState(st);
            k += 1;
          end;
        end;

        // Все ли игроки на месте
        if k <> g_Player_GetCount then
          Raise XStreamError.Create('g_SaveGame: wrong players count');
      end;
      ///// /////

      ///// Маркер окончания /////
      utils.writeSign(st, 'END');
      st.WriteByte(0);
      ///// /////
      Result := True;
    finally
      st.Free();
    end;

  except
    on e: Exception do
    begin
      st.Free();
      g_Console_Add(_lc[I_GAME_ERROR_SAVE]);
      e_WriteLog('SaveState Error: '+e.message, TMsgType.Warning);
      if deleteOnError then DeleteFile(filename);
      {$IF DEFINED(D2F_DEBUG)}e_WriteStackTrace(e.message);{$ENDIF}
      Result := False;
    end;
  end;
end;


function g_LoadGameFrom (const filename: AnsiString): Boolean;
var
  st: TStream = nil;
  WAD_Path, Map_Name: AnsiString;
  nPlayers: Integer;
  Game_Type, Game_Mode, Game_MaxLives: Byte;
  Game_TimeLimit, Game_ScoreLimit: Word;
  Game_Time, Game_Options: Cardinal;
  Game_CoopMonstersKilled,
  Game_CoopSecretsFound,
  Game_CoopTotalMonstersKilled,
  Game_CoopTotalSecretsFound,
  Game_CoopTotalMonsters,
  Game_CoopTotalSecrets,
  PID1, PID2: Word;
  i: Integer;
  gameCleared: Boolean = false;
  curmapfile: AnsiString = '';
  {$IF DEFINED(D2F_DEBUG)}
  errpos: LongWord = 0;
  {$ENDIF}
begin
  result := false;

  try
    st := e_OpenResourceRO(SaveDirs, filename);
    try
      if not utils.checkSign(st, 'DFSV') then
        Raise XStreamError.Create('invalid save game signature');
      if st.ReadByte() <> SAVE_VERSION then
        Raise XStreamError.Create('invalid save game version');

      e_WriteLog('Loading saved game...', TMsgType.Notify);

{$IF DEFINED(D2F_DEBUG)}
      try
{$ENDIF}
        //g_Game_Free(false); // don't free textures for the same map
        g_Game_ClearLoading();
        g_Game_SetLoadingText(_lc[I_LOAD_SAVE_FILE], 0, False);
        gLoadGameMode := True;

        ///// Загружаем состояние игры /////
        // Имя сэйва
        {str :=} utils.readStr(st, 64);

        // Полный путь к ваду и карта
        curmapfile := utils.readStr(st);

        if Length(gCurrentMapFileName) <> 0 then
          e_LogWritefln('LOAD: previous map was ''%s''...', [gCurrentMapFileName]);
        if Length(curmapfile) <> 0 then
          e_LogWritefln('LOAD: new map is ''%s''...', [curmapfile]);

        // А вот тут, наконец, чистим ресурсы
        g_Game_Free(curmapfile <> gCurrentMapFileName);  // don't free textures for the same map
        gameCleared := True;

        WAD_Path := utils.readStr(st);  // Путь к карте
        Map_Name := utils.readStr(st);  // Имя карты
        nPlayers := st.ReadWordLE();  // Количество игроков
        Game_Time := st.ReadDWordLE();  // Игровое время
        Game_Type := st.ReadByte();  // Тип игры
        Game_Mode := st.ReadByte();  // Режим игры
        Game_TimeLimit := st.ReadWordLE();  // Лимит времени
        Game_ScoreLimit := st.ReadWordLE();  // Лимит очков
        Game_MaxLives := st.ReadByte();  // Лимит жизней
        Game_Options := st.ReadDWordLE();  // Игровые опции

        // Для коопа
        Game_CoopMonstersKilled := st.ReadWordLE();
        Game_CoopSecretsFound := st.ReadWordLE();
        Game_CoopTotalMonstersKilled := st.ReadWordLE();
        Game_CoopTotalSecretsFound := st.ReadWordLE();
        Game_CoopTotalMonsters := st.ReadWordLE();
        Game_CoopTotalSecrets := st.ReadWordLE();
        ///// /////

        ///// Загружаем состояние областей просмотра /////
        if not utils.checkSign(st, 'PLVW') then
          Raise XStreamError.Create('invalid viewport signature');
        if st.ReadByte() <> 0 then
          Raise XStreamError.Create('invalid viewport version');
        PID1 := st.ReadWordLE();
        PID2 := st.ReadWordLE();
        ///// /////

        // Загружаем карту:
        gGameSettings := Default(TGameSettings);
        gAimLine := False;
        gShowMap := False;

        // Настройки игры
        gGameSettings.GameType := Game_Type;
        gGameSettings.GameMode := Game_Mode;
        gGameSettings.TimeLimit := Game_TimeLimit;
        gGameSettings.ScoreLimit := Game_ScoreLimit;
        gGameSettings.MaxLives := Game_MaxLives;
        gGameSettings.Options := TGameOptions(Game_Options);
        gSwitchGameMode := Game_Mode;
        g_Game_ExecuteEvent('ongamestart');

        // Установка размеров окон игроков
        g_Game_SetupScreenSize();

        // Загрузка и запуск карты
        // FIXME: save/load `asMegawad`
        if not g_Game_StartMap(False{asMegawad}, WAD_Path+':\'+Map_Name, True, curmapfile) then
          Raise Exception.Create(Format(_lc[I_GAME_ERROR_MAP_LOAD], [WAD_Path + ':\' + Map_Name]));

        g_Player_Init();  // Настройки игроков и ботов
        gTime := Game_Time;  // Устанавливаем время

        // Возвращаем статы
        gCoopMonstersKilled := Game_CoopMonstersKilled;
        gCoopSecretsFound := Game_CoopSecretsFound;
        gCoopTotalMonstersKilled := Game_CoopTotalMonstersKilled;
        gCoopTotalSecretsFound := Game_CoopTotalSecretsFound;
        gCoopTotalMonsters := Game_CoopTotalMonsters;
        gCoopTotalSecrets := Game_CoopTotalSecrets;

        g_Map_LoadState(st);  // Загружаем состояние карты
        g_Items_LoadState(st);  // Загружаем состояние предметов
        g_Triggers_LoadState(st);  // Загружаем состояние триггеров
        g_Weapon_LoadState(st);  // Загружаем состояние оружия
        g_Monsters_LoadState(st);  // Загружаем состояние монстров
        g_Player_Corpses_LoadState(st);  // Загружаем состояние трупов

        ///// Загружаем игроков (в том числе ботов) /////
        for i := 0 to nPlayers-1 do
          g_Player_CreateFromState(st);

        // Привязываем основных игроков к областям просмотра
        gPlayer1 := g_Player_Get(PID1);
        gPlayer2 := g_Player_Get(PID2);

        if gPlayer1 <> nil then
        begin
          gPlayer1.Name := gPlayer1Settings.Name;
          gPlayer1.FPreferredTeam := gPlayer1Settings.Team;
          gPlayer1.FActualModelName := gPlayer1Settings.Model;
          gPlayer1.SetColor(gPlayer1Settings.Color, False);
          gPlayer1.SetModel(gPlayer1.FActualModelName);
        end;

        if gPlayer2 <> nil then
        begin
          gPlayer2.Name := gPlayer2Settings.Name;
          gPlayer2.FPreferredTeam := gPlayer2Settings.Team;
          gPlayer2.FActualModelName := gPlayer2Settings.Model;
          gPlayer2.SetColor(gPlayer2Settings.Color, False);
          gPlayer2.SetModel(gPlayer2.FActualModelName);
        end;
        ///// /////

        ///// Маркер окончания /////
        if not utils.checkSign(st, 'END') then
          Raise XStreamError.Create('no end marker');
        if st.ReadByte() <> 0 then
          Raise XStreamError.Create('invalid end marker');
        ///// /////

        // Ищем триггеры с условием смерти монстров
        if gTriggers <> nil then g_Map_ReAdd_DieTriggers();

        // done
        gLoadGameMode := False;
        Result := True;
{$IF DEFINED(D2F_DEBUG)}
      except
        begin
          errpos := LongWord(st.position);
          raise;
        end;
      end;
{$ENDIF}
    finally
      st.Free();
    end;
  except
    on e: EFileNotFoundException do
      begin
        g_Console_Add(_lc[I_GAME_ERROR_LOAD]);
        g_Console_Add('LoadState Error: '+e.message);
        e_WriteLog('LoadState Error: '+e.message, TMsgType.Warning);
        gLoadGameMode := false;
        result := false;
      end;
    on e: Exception do
      begin
        g_Console_Add(_lc[I_GAME_ERROR_LOAD]);
        g_Console_Add('LoadState Error: '+e.message);
        e_WriteLog('LoadState Error: '+e.message, TMsgType.Warning);
        {$IF DEFINED(D2F_DEBUG)}e_LogWritefln('stream error position: 0x%08x', [errpos], TMsgType.Warning);{$ENDIF}
        gLoadGameMode := false;
        result := false;
        if gState <> STATE_MENU then
          g_FatalError(_lc[I_GAME_ERROR_LOAD])
        else if not gameCleared then
          g_Game_Free();
        {$IF DEFINED(D2F_DEBUG)}e_WriteStackTrace(e.message);{$ENDIF}
      end;
  end;
end;


function g_SaveGame (n: Integer; const aname: AnsiString): Boolean;
begin
  result := g_SaveGameTo(buildSaveName(n), aname, true);
end;


function g_LoadGame (n: Integer): Boolean;
begin
  result := g_LoadGameFrom(buildSaveName(n));
end;


end.
