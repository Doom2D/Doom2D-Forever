(* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../shared/a_modes.inc}
unit g_saveload;

interface

uses
  SysUtils, Classes, g_phys, g_textures;


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
  g_basic, Math, wadreader,
  g_weapons, g_player, g_console,
  e_log, e_res, g_language, g_options;

const
  SAVE_SIGNATURE = $56534644; // 'DFSV'
  SAVE_VERSION = $07;
  END_MARKER_STRING = 'END';
  PLAYER_VIEW_SIGNATURE = $57564C50; // 'PLVW'
  OBJ_SIGNATURE = $4A424F5F; // '_OBJ'


procedure Obj_SaveState (st: TStream; o: PObj);
begin
  if (st = nil) then exit;
  // Сигнатура объекта
  utils.writeSign(st, '_OBJ');
  utils.writeInt(st, Byte(0)); // version
  // Положение по-горизонтали
  utils.writeInt(st, LongInt(o^.X));
  // Положение по-вертикали
  utils.writeInt(st, LongInt(o^.Y));
  // Ограничивающий прямоугольник
  utils.writeInt(st, LongInt(o^.Rect.X));
  utils.writeInt(st, LongInt(o^.Rect.Y));
  utils.writeInt(st, Word(o^.Rect.Width));
  utils.writeInt(st, Word(o^.Rect.Height));
  // Скорость
  utils.writeInt(st, LongInt(o^.Vel.X));
  utils.writeInt(st, LongInt(o^.Vel.Y));
  // Ускорение
  utils.writeInt(st, LongInt(o^.Accel.X));
  utils.writeInt(st, LongInt(o^.Accel.Y));
end;


procedure Obj_LoadState (o: PObj; st: TStream);
begin
  if (st = nil) then exit;
  // Сигнатура объекта:
  if not utils.checkSign(st, '_OBJ') then raise XStreamError.Create('invalid object signature');
  if (utils.readByte(st) <> 0) then raise XStreamError.Create('invalid object version');
  // Положение по-горизонтали
  o^.X := utils.readLongInt(st);
  // Положение по-вертикали
  o^.Y := utils.readLongInt(st);
  // Ограничивающий прямоугольник
  o^.Rect.X := utils.readLongInt(st);
  o^.Rect.Y := utils.readLongInt(st);
  o^.Rect.Width := utils.readWord(st);
  o^.Rect.Height := utils.readWord(st);
  // Скорость
  o^.Vel.X := utils.readLongInt(st);
  o^.Vel.Y := utils.readLongInt(st);
  // Ускорение
  o^.Accel.X := utils.readLongInt(st);
  o^.Accel.Y := utils.readLongInt(st);
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
      ver := utils.readByte(st);
      if (ver < 7) then
      begin
        utils.readLongWord(st); // section size
        stlen := utils.readWord(st);
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
  result := false;
  try
    st := e_CreateResource(SaveDirs, filename);
    try
      utils.writeSign(st, 'DFSV');
      utils.writeInt(st, Byte(SAVE_VERSION));
      // Имя сэйва
      utils.writeStr(st, aname, 64);
      // Полный путь к ваду и карта
      //if (Length(gCurrentMapFileName) <> 0) then e_LogWritefln('SAVE: current map is ''%s''...', [gCurrentMapFileName]);
      utils.writeStr(st, gCurrentMapFileName);
      // Путь к карте
      utils.writeStr(st, ExtractFileName(gGameSettings.WAD));
      // Имя карты
      utils.writeStr(st, g_ExtractFileName(gMapInfo.Map));
      // Количество игроков
      utils.writeInt(st, Word(g_Player_GetCount));
      // Игровое время
      utils.writeInt(st, LongWord(gTime));
      // Тип игры
      utils.writeInt(st, Byte(gGameSettings.GameType));
      // Режим игры
      utils.writeInt(st, Byte(gGameSettings.GameMode));
      // Лимит времени
      utils.writeInt(st, Word(gGameSettings.TimeLimit));
      // Лимит очков
      utils.writeInt(st, Word(gGameSettings.ScoreLimit));
      // Лимит жизней
      utils.writeInt(st, Byte(gGameSettings.MaxLives));
      // Игровые опции
      utils.writeInt(st, LongWord(gGameSettings.Options));
      // Для коопа
      utils.writeInt(st, Word(gCoopMonstersKilled));
      utils.writeInt(st, Word(gCoopSecretsFound));
      utils.writeInt(st, Word(gCoopTotalMonstersKilled));
      utils.writeInt(st, Word(gCoopTotalSecretsFound));
      utils.writeInt(st, Word(gCoopTotalMonsters));
      utils.writeInt(st, Word(gCoopTotalSecrets));

      ///// Сохраняем состояние областей просмотра /////
      utils.writeSign(st, 'PLVW');
      utils.writeInt(st, Byte(0)); // version
      PID1 := 0;
      PID2 := 0;
      if (gPlayer1 <> nil) then PID1 := gPlayer1.UID;
      if (gPlayer2 <> nil) then PID2 := gPlayer2.UID;
      utils.writeInt(st, Word(PID1));
      utils.writeInt(st, Word(PID2));
      ///// /////

      ///// Состояние карты /////
      g_Map_SaveState(st);
      ///// /////

      ///// Состояние предметов /////
      g_Items_SaveState(st);
      ///// /////

      ///// Состояние триггеров /////
      g_Triggers_SaveState(st);
      ///// /////

      ///// Состояние оружия /////
      g_Weapon_SaveState(st);
      ///// /////

      ///// Состояние монстров /////
      g_Monsters_SaveState(st);
      ///// /////

      ///// Состояние трупов /////
      g_Player_Corpses_SaveState(st);
      ///// /////

      ///// Сохраняем игроков (в том числе ботов) /////
      if (g_Player_GetCount > 0) then
      begin
        k := 0;
        for i := 0 to High(gPlayers) do
        begin
          if (gPlayers[i] <> nil) then
          begin
            // Состояние игрока
            gPlayers[i].SaveState(st);
            Inc(k);
          end;
        end;

        // Все ли игроки на месте
        if (k <> g_Player_GetCount) then raise XStreamError.Create('g_SaveGame: wrong players count');
      end;
      ///// /////

      ///// Маркер окончания /////
      utils.writeSign(st, 'END');
      utils.writeInt(st, Byte(0));
      ///// /////
      result := true;
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
e_WriteStackTrace(e.message);
        result := false;
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
      if not utils.checkSign(st, 'DFSV') then raise XStreamError.Create('invalid save game signature');
      if (utils.readByte(st) <> SAVE_VERSION) then raise XStreamError.Create('invalid save game version');

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

        if (Length(gCurrentMapFileName) <> 0) then e_LogWritefln('LOAD: previous map was ''%s''...', [gCurrentMapFileName]);
        if (Length(curmapfile) <> 0) then e_LogWritefln('LOAD: new map is ''%s''...', [curmapfile]);
        // А вот тут, наконец, чистим ресурсы
        g_Game_Free(curmapfile <> gCurrentMapFileName); // don't free textures for the same map
        gameCleared := true;

        // Путь к карте
        WAD_Path := utils.readStr(st);
        // Имя карты
        Map_Name := utils.readStr(st);
        // Количество игроков
        nPlayers := utils.readWord(st);
        // Игровое время
        Game_Time := utils.readLongWord(st);
        // Тип игры
        Game_Type := utils.readByte(st);
        // Режим игры
        Game_Mode := utils.readByte(st);
        // Лимит времени
        Game_TimeLimit := utils.readWord(st);
        // Лимит очков
        Game_ScoreLimit := utils.readWord(st);
        // Лимит жизней
        Game_MaxLives := utils.readByte(st);
        // Игровые опции
        Game_Options := utils.readLongWord(st);
        // Для коопа
        Game_CoopMonstersKilled := utils.readWord(st);
        Game_CoopSecretsFound := utils.readWord(st);
        Game_CoopTotalMonstersKilled := utils.readWord(st);
        Game_CoopTotalSecretsFound := utils.readWord(st);
        Game_CoopTotalMonsters := utils.readWord(st);
        Game_CoopTotalSecrets := utils.readWord(st);
        ///// /////

        ///// Загружаем состояние областей просмотра /////
        if not utils.checkSign(st, 'PLVW') then raise XStreamError.Create('invalid viewport signature');
        if (utils.readByte(st) <> 0) then raise XStreamError.Create('invalid viewport version');
        PID1 := utils.readWord(st);
        PID2 := utils.readWord(st);
        ///// /////

        // Загружаем карту:
        ZeroMemory(@gGameSettings, sizeof(TGameSettings));
        gAimLine := false;
        gShowMap := false;
        if (Game_Type = GT_NONE) or (Game_Type = GT_SINGLE) then
        begin
          // Настройки игры
          gGameSettings.GameType := GT_SINGLE;
          gGameSettings.MaxLives := 0;
          gGameSettings.Options := gGameSettings.Options+GAME_OPTION_ALLOWEXIT;
          gGameSettings.Options := gGameSettings.Options+GAME_OPTION_MONSTERS;
          gGameSettings.Options := gGameSettings.Options+GAME_OPTION_BOTVSMONSTER;
          gSwitchGameMode := GM_SINGLE;
        end
        else
        begin
          // Настройки игры
          gGameSettings.GameType := GT_CUSTOM;
          gGameSettings.GameMode := Game_Mode;
          gSwitchGameMode := Game_Mode;
          gGameSettings.TimeLimit := Game_TimeLimit;
          gGameSettings.ScoreLimit := Game_ScoreLimit;
          gGameSettings.MaxLives := IfThen(Game_Mode = GM_CTF, 0, Game_MaxLives);
          gGameSettings.Options := Game_Options;
        end;
        g_Game_ExecuteEvent('ongamestart');

        // Установка размеров окон игроков
        g_Game_SetupScreenSize();

        // Загрузка и запуск карты
        //FIXME: save/load `asMegawad`
        if not g_Game_StartMap(false{asMegawad}, WAD_Path+':\'+Map_Name, True, curmapfile) then
          raise Exception.Create(Format(_lc[I_GAME_ERROR_MAP_LOAD], [WAD_Path + ':\' + Map_Name]));

        // Настройки игроков и ботов
        g_Player_Init();

        // Устанавливаем время
        gTime := Game_Time;
        // Возвращаем статы
        gCoopMonstersKilled := Game_CoopMonstersKilled;
        gCoopSecretsFound := Game_CoopSecretsFound;
        gCoopTotalMonstersKilled := Game_CoopTotalMonstersKilled;
        gCoopTotalSecretsFound := Game_CoopTotalSecretsFound;
        gCoopTotalMonsters := Game_CoopTotalMonsters;
        gCoopTotalSecrets := Game_CoopTotalSecrets;

        ///// Загружаем состояние карты /////
        g_Map_LoadState(st);
        ///// /////

        ///// Загружаем состояние предметов /////
        g_Items_LoadState(st);
        ///// /////

        ///// Загружаем состояние триггеров /////
        g_Triggers_LoadState(st);
        ///// /////

        ///// Загружаем состояние оружия /////
        g_Weapon_LoadState(st);
        ///// /////

        ///// Загружаем состояние монстров /////
        g_Monsters_LoadState(st);
        ///// /////

        ///// Загружаем состояние трупов /////
        g_Player_Corpses_LoadState(st);
        ///// /////

        ///// Загружаем игроков (в том числе ботов) /////
        if nPlayers > 0 then
        begin
          // Загружаем
          for i := 0 to nPlayers-1 do g_Player_CreateFromState(st);
        end;

        // Привязываем основных игроков к областям просмотра
        gPlayer1 := g_Player_Get(PID1);
        gPlayer2 := g_Player_Get(PID2);

        if (gPlayer1 <> nil) then
        begin
          gPlayer1.Name := gPlayer1Settings.Name;
          gPlayer1.FPreferredTeam := gPlayer1Settings.Team;
          gPlayer1.FActualModelName := gPlayer1Settings.Model;
          gPlayer1.SetModel(gPlayer1.FActualModelName);
          gPlayer1.SetColor(gPlayer1Settings.Color);
        end;

        if (gPlayer2 <> nil) then
        begin
          gPlayer2.Name := gPlayer2Settings.Name;
          gPlayer2.FPreferredTeam := gPlayer2Settings.Team;
          gPlayer2.FActualModelName := gPlayer2Settings.Model;
          gPlayer2.SetModel(gPlayer2.FActualModelName);
          gPlayer2.SetColor(gPlayer2Settings.Color);
        end;
        ///// /////

        ///// Маркер окончания /////
        if not utils.checkSign(st, 'END') then raise XStreamError.Create('no end marker');
        if (utils.readByte(st) <> 0) then raise XStreamError.Create('invalid end marker');
        ///// /////

        // Ищем триггеры с условием смерти монстров
        if (gTriggers <> nil) then g_Map_ReAdd_DieTriggers();

        // done
        gLoadGameMode := false;
        result := true;
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
