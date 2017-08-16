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
{$INCLUDE g_amodes.inc}
unit g_sound;

interface

uses
  e_sound;

const
  SOUND_MINDIST = 400;
  SOUND_MAXDIST = 1000;

type
  TPlayableSound = class(TBasicSound)
  private
    FName: String;

  public
    constructor Create();
    destructor Destroy(); override;
    function Play(Force: Boolean = False): Boolean;
    function PlayAt(X, Y: Integer): Boolean;
    function PlayPanVolume(Pan, Volume: Single; Force: Boolean = False): Boolean;
    function PlayVolumeAt(X, Y: Integer; Volume: Single): Boolean;
    function SetByName(SN: String): Boolean;
    function SetCoords(X, Y: Integer; Volume: Single): Boolean;

    property Loop: Boolean read FMusic write FMusic;
    property Name: String read FName;
  end;

  TMusic = class(TBasicSound)
  private
    FName: String;
    FSpecPause: Boolean; // Спец-пауза. "Сильнее" обычной
    FNoMusic: Boolean;

    procedure SetSpecPause(Enable: Boolean);

  public
    constructor Create();
    destructor Destroy(); override;
    function Play(Force: Boolean = False): Boolean;
    function SetByName(SN: String): Boolean;
    function IsPaused(): Boolean;
    procedure Pause(Enable: Boolean);

    property Name: String read FName;
    property SpecPause: Boolean read FSpecPause write SetSpecPause;
    property NoMusic: Boolean read FNoMusic;
  end;

function g_Sound_PlayEx(SoundName: ShortString): Boolean;
function g_Sound_PlayExPanVolume(SoundName: ShortString; Pan: Single; Volume: Single): Boolean;
function g_Sound_PlayAt(ID: DWORD; X, Y: Integer): Boolean;
function g_Sound_PlayExAt(SoundName: ShortString; X, Y: Integer): Boolean;

function g_Sound_CreateWAD(var ID: DWORD; Resource: string; isMusic: Boolean = False): Boolean;
function g_Sound_CreateWADEx(SoundName: ShortString; Resource: string; isMusic: Boolean = False): Boolean;
function g_Sound_CreateFile(var ID: DWORD; FileName: string; isMusic: Boolean = False): Boolean;
function g_Sound_CreateFileEx(SoundName: ShortString; FileName: string; isMusic: Boolean = False): Boolean;

procedure g_Sound_Delete(SoundName: ShortString);
function g_Sound_Exists(SoundName: string): Boolean;
function g_Sound_Get(var ID: DWORD; SoundName: ShortString): Boolean;

procedure g_Sound_SetupAllVolumes(SoundVol, MusicVol: Byte);

implementation

uses
  e_log, SysUtils, g_console, g_options, wadreader,
  g_game, g_basic, g_items, g_map, Math,
  g_language;

type
  TGameSound = record
    Name:     ShortString;
    ID:       DWORD;
    IsMusic:  Boolean;
  end;

var
  SoundArray: Array of TGameSound;
  //SoundsMuted: Boolean = False;


function FindSound(): DWORD;
var
  i: integer;
begin
  if SoundArray <> nil then
    for i := 0 to High(SoundArray) do
      if SoundArray[i].Name = '' then
      begin
        Result := i;
        Exit;
      end;

  if SoundArray = nil then
    begin
      SetLength(SoundArray, 8);
      Result := 0;
    end
  else
    begin
      Result := High(SoundArray) + 1;
      SetLength(SoundArray, Length(SoundArray) + 8);
    end;
end;

function g_Sound_PlayEx(SoundName: ShortString): Boolean;
var
  a: DWORD;
begin
  Result := False;
  if SoundArray = nil then
    Exit;

  for a := 0 to High(SoundArray) do
    if SoundArray[a].Name = SoundName then
    begin
      Result := (e_PlaySoundVolume(SoundArray[a].ID, gSoundLevel/255.0) >= 0);
      Exit;
    end;

  e_WriteLog(Format(_lc[I_GAME_ERROR_SOUND], [SoundName]), MSG_WARNING);
end;

function g_Sound_PlayExPanVolume(SoundName: ShortString; Pan: Single; Volume: Single): Boolean;
var
  a: DWORD;
begin
  Result := False;
  if SoundArray = nil then
    Exit;

  for a := 0 to High(SoundArray) do
    if SoundArray[a].Name = SoundName then
    begin
      Result := (e_PlaySoundPanVolume(SoundArray[a].ID, Pan, Volume * (gSoundLevel/255.0)) >= 0);
      Exit;
    end;

  e_WriteLog(Format(_lc[I_GAME_ERROR_SOUND], [SoundName]), MSG_WARNING);
end;

function PlaySoundAt(X, Y: Integer; var Pan: Single; var Volume: Single; InVolume: Single = 1.0): Boolean;
var
  l1, l2, lx, rx: Integer;
  d1, d2, sMaxDist: Single;
  c: Boolean;
begin
  l1 := gMaxDist;
  l2 := gMaxDist;
  sMaxDist := SOUND_MAXDIST * InVolume;

  d1 := 0.0;

  c := SOUND_MINDIST >= sMaxDist;

  if X > gMapInfo.Width then
    X := gMapInfo.Width
  else
    if X < 0 then
      X := 0;

  if Y > gMapInfo.Height then
    Y := gMapInfo.Height
  else
    if Y < 0 then
      Y := 0;

  if gHearPoint1.Active then
  begin
    l1 := Round(Hypot(X - gHearPoint1.Coords.X, Y - gHearPoint1.Coords.Y));

    lx := gHearPoint1.Coords.X - SOUND_MINDIST;
    rx := gHearPoint1.Coords.X + SOUND_MINDIST;
    if c then
      d1 := 0.0
    else if (X >= lx) and (X <= rx) then
      d1 := 0.0
    else if X < lx then
      d1 := (X-lx)/sMaxDist
    else
      d1 := (X-rx)/sMaxDist;
  end;

  d2 := d1;

  if gHearPoint2.Active then
  begin
    l2 := Round(Hypot(X - gHearPoint2.Coords.X, Y - gHearPoint2.Coords.Y));

    lx := gHearPoint2.Coords.X - SOUND_MINDIST;
    rx := gHearPoint2.Coords.X + SOUND_MINDIST;
    if c then
      d2 := 0.0
    else if (X >= lx) and (X <= rx) then
      d2 := 0.0
    else if X < lx then
      d2 := (X-lx)/sMaxDist
    else
      d2 := (X-rx)/sMaxDist;
  end;

  if l2 < l1 then
  begin
    l1 := l2;
    d1 := d2;
  end;

  if l1 >= sMaxDist then
    begin
      Pan := 0.0;
      Volume := 0.0;
      Result := False;
    end
  else
    begin
      Pan := d1;
      Volume := 1.0 - l1/sMaxDist;
      Result := True;
    end;
end;

function g_Sound_PlayAt(ID: DWORD; X, Y: Integer): Boolean;
var
  Pan, Vol: Single;
begin
  if PlaySoundAt(X, Y, Pan, Vol) then
    Result := (e_PlaySoundPanVolume(ID, Pan, Vol * (gSoundLevel/255.0)) >= 0)
  else
    Result := False;
end;

function g_Sound_PlayExAt(SoundName: ShortString; X, Y: Integer): Boolean;
var
  a: DWORD;
  Pan, Vol: Single;
begin
  Result := False;

  if SoundArray = nil then
    Exit;

  for a := 0 to High(SoundArray) do
    if SoundArray[a].Name = SoundName then
    begin
      if PlaySoundAt(X, Y, Pan, Vol) then
        Result := (e_PlaySoundPanVolume(SoundArray[a].ID, Pan, Vol * (gSoundLevel/255.0)) >= 0);
      Exit;
    end;

  e_WriteLog(Format(_lc[I_GAME_ERROR_SOUND], [SoundName]), MSG_WARNING);
end;

function g_Sound_CreateFile(var ID: DWORD; FileName: string; isMusic: Boolean = False): Boolean;
begin
  Result := e_LoadSound(FileName, ID, isMusic);
end;

function g_Sound_CreateFileEx(SoundName: ShortString; FileName: string; isMusic: Boolean = False): Boolean;
var
  find_id: DWORD;
begin
  Result := False;

  find_id := FindSound();

  if not e_LoadSound(FileName, SoundArray[find_id].ID, isMusic) then
    Exit;

  SoundArray[find_id].Name := SoundName;
  SoundArray[find_id].IsMusic := isMusic;

  Result := True;
end;

function g_Sound_CreateWAD(var ID: DWORD; Resource: string; isMusic: Boolean = False): Boolean;
var
  WAD: TWADFile;
  FileName: string;
  SoundData: Pointer;
  ResLength: Integer;
  ok: Boolean;
begin
  Result := False;
  ok := False;

  // e_WriteLog('Loading sound: ' + Resource, MSG_NOTIFY);
  FileName := g_ExtractWadName(Resource);

  WAD := TWADFile.Create();
  WAD.ReadFile(FileName);

  if WAD.GetResource(g_ExtractFilePathName(Resource), SoundData, ResLength) then
    begin
      if e_LoadSoundMem(SoundData, ResLength, ID, isMusic) then
        ok := True
      else
        FreeMem(SoundData);
    end
  else
  begin
    //e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
  end;

  WAD.Free();
  if (not ok) then
  begin
{$IFNDEF HEADLESS}
    if isMusic then
      e_WriteLog(Format('Error loading music %s', [Resource]), MSG_WARNING)
    else
      e_WriteLog(Format('Error loading sound %s', [Resource]), MSG_WARNING);
    Exit;
{$ENDIF}
  end;
  Result := True;
end;

function g_Sound_CreateWADEx(SoundName: ShortString; Resource: string; isMusic: Boolean = False): Boolean;
var
  WAD: TWADFile;
  FileName: string;
  SoundData: Pointer;
  ResLength: Integer;
  find_id: DWORD;
  ok: Boolean;
begin
  Result := False;
  ok := False;

  // e_WriteLog('Loading sound: ' + Resource, MSG_NOTIFY);
  FileName := g_ExtractWadName(Resource);

  find_id := FindSound();

  WAD := TWADFile.Create();
  WAD.ReadFile(FileName);

  if WAD.GetResource(g_ExtractFilePathName(Resource), SoundData, ResLength) then
    begin
      if e_LoadSoundMem(SoundData, ResLength, SoundArray[find_id].ID, isMusic) then
        begin
          SoundArray[find_id].Name := SoundName;
          SoundArray[find_id].IsMusic := isMusic;
          ok := True;
        end
      else
        FreeMem(SoundData);
    end
  else
  begin
    //e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
  end;

  WAD.Free();
  if (not ok) then
  begin
{$IFNDEF HEADLESS}
    if isMusic then
      e_WriteLog(Format('Error loading music %s', [Resource]), MSG_WARNING)
    else
      e_WriteLog(Format('Error loading sound %s', [Resource]), MSG_WARNING);
    Exit;
{$ENDIF}
  end;
  Result := True;
end;

procedure g_Sound_Delete(SoundName: ShortString);
var
  a: DWORD;
begin
  if (SoundArray = nil) or (SoundName = '') then
    Exit;

  for a := 0 to High(SoundArray) do
    if SoundArray[a].Name = SoundName then
    begin
      e_DeleteSound(SoundArray[a].ID);
      SoundArray[a].Name := '';
      SoundArray[a].ID := 0;
      SoundArray[a].IsMusic := False;
    end;
end;

function g_Sound_Exists(SoundName: string): Boolean;
var
  a: DWORD;
begin
  Result := False;

  if SoundName = '' then
    Exit;

  if SoundArray <> nil then
    for a := 0 to High(SoundArray) do
      if SoundArray[a].Name = SoundName then
      begin
        Result := True;
        Break;
      end;
end;

function g_Sound_Get(var ID: DWORD; SoundName: ShortString): Boolean;
var
  a: DWORD;
begin
  Result := False;

  if SoundName = '' then
    Exit;

  if SoundArray <> nil then
    for a := 0 to High(SoundArray) do
      if SoundArray[a].Name = SoundName then
      begin
        ID := SoundArray[a].ID;
        Result := True;
        Break;
      end;
end;

procedure g_Sound_SetupAllVolumes(SoundVol, MusicVol: Byte);
var
  Svol, Mvol: Single;
  sm: Boolean;
begin
  Mvol := 0; // shut up, compiler
  if (gSoundLevel = SoundVol) and (gMusicLevel = MusicVol) then
    Exit;

  if gSoundLevel > 0 then
    begin
      Svol := SoundVol / gSoundLevel;
      sm := False;
    end
  else
    begin
      Svol := SoundVol / 255.0;
      sm := True;
    end;

  if gMusic <> nil then
    if gMusicLevel > 0 then
      Mvol := gMusic.GetVolume() * MusicVol / gMusicLevel
    else
      Mvol := MusicVol / 255.0;

  e_ModifyChannelsVolumes(Svol, sm);

  if gMusic <> nil then
    gMusic.SetVolume(Mvol);

  gSoundLevel := SoundVol;
  gMusicLevel := MusicVol;
end;

{ TPlayableSound: }

constructor TPlayableSound.Create();
begin
  inherited;
  FName := '';
end;

destructor TPlayableSound.Destroy();
begin
  inherited;
end;

function TPlayableSound.Play(Force: Boolean = False): Boolean;
begin
  if Force or not IsPlaying() then
    begin
      Stop();
      Result := RawPlay(0.0, gSoundLevel/255.0, FPosition);
    end
  else
    Result := False;
end;

function TPlayableSound.PlayAt(X, Y: Integer): Boolean;
var
  Pan, Vol: Single;
begin
  if PlaySoundAt(X, Y, Pan, Vol) then
    begin
      Stop();
      Result := RawPlay(Pan, Vol * (gSoundLevel/255.0), FPosition);
    end
  else
    Result := False;
end;

function TPlayableSound.PlayPanVolume(Pan, Volume: Single; Force: Boolean = False): Boolean;
begin
  if Force or not IsPlaying() then
    begin
      Stop();
      Result := RawPlay(Pan, Volume * (gSoundLevel/255.0), FPosition);
    end
  else
    Result := False;
end;

function TPlayableSound.PlayVolumeAt(X, Y: Integer; Volume: Single): Boolean;
var
  Pan, Vol: Single;
begin
  if PlaySoundAt(X, Y, Pan, Vol, Volume) then
    begin
      Stop();
      Result := RawPlay(Pan, Volume * Vol * (gSoundLevel/255.0), FPosition);
    end
  else
    Result := False;
end;

function TPlayableSound.SetCoords(X, Y: Integer; Volume: Single): Boolean;
var
  Pan, Vol: Single;
begin
  if PlaySoundAt(X, Y, Pan, Vol, Volume) then
  begin
    SetVolume(Volume * Vol * (gSoundLevel/255.0));
    SetPan(Pan);
    Result := True;
  end
  else
  begin
    SetVolume(0.0);
    SetPan(0.0);
    Result := False;
  end;
end;

function TPlayableSound.SetByName(SN: String): Boolean;
var
  id: DWORD;
begin
  if g_Sound_Get(id, SN) then
    begin
      SetID(id);
      FName := SN;
      Result := True;
    end
  else
    Result := False;
end;

{ TMusic: }

constructor TMusic.Create();
begin
  inherited;
  FName := '';
  FSpecPause := False;
  FNoMusic := True;
end;

destructor TMusic.Destroy();
begin
  inherited;
end;

function TMusic.Play(Force: Boolean = False): Boolean;
begin
  if FNoMusic then
  begin
    Result := True;
    Exit;
  end;

  if Force or not IsPlaying() then
    begin
      Stop();
      Result := RawPlay(0.0, gMusicLevel/255.0, FPosition);
      if Result then
        SetPriority(0);
      if Result and FSpecPause then
        Pause(True);
    end
  else
    Result := False;
end;

function TMusic.SetByName(SN: String): Boolean;
var
  id: DWORD;
begin
  if SN = '' then
  begin
    FNoMusic := True;
    Result := True;
    Exit;
  end;

  if g_Sound_Get(id, SN) then
    begin
      SetID(id);
      FName := SN;
      FNoMusic := False;
      FSpecPause := False;
      Result := True;
    end
  else
    Result := False;
end;

function TMusic.IsPaused(): Boolean;
begin
  Result := inherited IsPaused();
  Result := Result or FSpecPause;
end;

procedure TMusic.Pause(Enable: Boolean);
begin
// Отключаем паузу, только если не было спец-паузы:
  if Enable or (not FSpecPause) then
    inherited Pause(Enable);
end;

procedure TMusic.SetSpecPause(Enable: Boolean);
begin
  FSpecPause := Enable;
  Pause(Enable);
end;

end.
