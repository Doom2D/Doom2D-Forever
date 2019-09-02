(* Copyright (C)  Doom 2D: Forever Developers
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
{$INCLUDE ../shared/a_modes.inc}
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
    function PlayVolumeAtRect (X, Y, W, H: Integer; Volume: Single): Boolean;
    function SetByName(SN: String): Boolean;
    function SetCoords(X, Y: Integer; Volume: Single): Boolean;
    function SetCoordsRect (X, Y, W, H: Integer; Volume: Single): Boolean;

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
function g_Sound_CreateWADEx(SoundName: ShortString; Resource: string; isMusic: Boolean = False; ForceNoLoop: Boolean = False): Boolean;
function g_Sound_CreateFile(var ID: DWORD; FileName: string; isMusic: Boolean = False): Boolean;
function g_Sound_CreateFileEx(SoundName: ShortString; FileName: string; isMusic: Boolean = False; ForceNoLoop: Boolean = False): Boolean;

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

  e_WriteLog(Format(_lc[I_GAME_ERROR_SOUND], [SoundName]), TMsgType.Warning);
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

  e_WriteLog(Format(_lc[I_GAME_ERROR_SOUND], [SoundName]), TMsgType.Warning);
end;

function PlaySoundAtRect (X, Y, W, H: Integer; out Pan, Volume: Single; InVolume: Single = 1.0): Boolean;
  var
    len1, len2: Integer;
    pan1, pan2: Single;
    sMaxDist: Single;

  procedure CalcDest (const p: THearPoint; out pan: Single; out len: Integer);
    var XX, YY, lx, rx: Integer;
  begin
    pan := 0.0; len := gMaxDist;
    if p.Active then
    begin
      XX := Max(X, Min(X + W, p.Coords.X));
      YY := Max(Y, Min(Y + H, p.Coords.Y));
      len := Round(Hypot(XX - p.Coords.X, YY - p.Coords.Y));
      if sMaxDist < SOUND_MINDIST then
      begin
        lx := X - SOUND_MINDIST;
        rx := X + W + SOUND_MINDIST;
        if p.Coords.X < lx then
          pan := (lx - p.Coords.X) / sMaxDist
        else if p.Coords.X > rx then
          pan := (rx - p.Coords.X) / sMaxDist
      end
    end
  end;

begin
  ASSERT((W >= 0) and (H >= 0));
  ASSERT((InVolume >= 0.0) and (InVolume <= 1.0));
  sMaxDist := SOUND_MAXDIST * InVolume;
  X := Max(0, Min(X, gMapInfo.Width));
  Y := Max(0, Min(Y, gMapInfo.Height));
  CalcDest(gHearPoint1, pan1, len1);
  CalcDest(gHearPoint2, pan2, len2);
  if len2 < len1 then
  begin
    len1 := len2;
    pan1 := pan2;
  end;
  if len1 >= sMaxDist then
  begin
    Pan := 0.0;
    Volume := 0.0;
    Result := False
  end
  else
  begin
    Pan := pan1;
    Volume := 1.0 - len1 / sMaxDist;
    Result := True
  end
end;

function PlaySoundAt(X, Y: Integer; out Pan: Single; out Volume: Single; InVolume: Single = 1.0): Boolean;
begin
  Result := PlaySoundAtRect(X, Y, 0, 0, Pan, Volume, InVolume)
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

  e_WriteLog(Format(_lc[I_GAME_ERROR_SOUND], [SoundName]), TMsgType.Warning);
end;

function g_Sound_CreateFile(var ID: DWORD; FileName: string; isMusic: Boolean = False): Boolean;
begin
  Result := e_LoadSound(FileName, ID, isMusic);
end;

function g_Sound_CreateFileEx(SoundName: ShortString; FileName: string; isMusic: Boolean = False; ForceNoLoop: Boolean = False): Boolean;
var
  find_id: DWORD;
begin
  Result := False;

  find_id := FindSound();

  if not e_LoadSound(FileName, SoundArray[find_id].ID, isMusic, ForceNoLoop) then
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
      e_WriteLog(Format('Error loading music %s', [Resource]), TMsgType.Warning)
    else
      e_WriteLog(Format('Error loading sound %s', [Resource]), TMsgType.Warning);
    Exit;
{$ENDIF}
  end;
  Result := True;
end;

function g_Sound_CreateWADEx(SoundName: ShortString; Resource: string; isMusic: Boolean = False; ForceNoLoop: Boolean = False): Boolean;
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
      if e_LoadSoundMem(SoundData, ResLength, SoundArray[find_id].ID, isMusic, ForceNoLoop) then
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
      e_WriteLog(Format('Error loading music %s', [Resource]), TMsgType.Warning)
    else
      e_WriteLog(Format('Error loading sound %s', [Resource]), TMsgType.Warning);
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

function TPlayableSound.PlayVolumeAtRect (X, Y, W, H: Integer; Volume: Single): Boolean;
  var Pan, Vol: Single;
begin
  Result := False;
  if PlaySoundAtRect(X, Y, W, H, Pan, Vol, Volume) then
  begin
    Stop;
    Result := RawPlay(Pan, Volume * Vol * (gSoundLevel / 255.0), FPosition)
  end
end;

function TPlayableSound.PlayVolumeAt (X, Y: Integer; Volume: Single): Boolean;
begin
  Result := Self.PlayVolumeAtRect(X, Y, 0, 0, Volume)
end;

function TPlayableSound.SetCoordsRect (X, Y, W, H: Integer; Volume: Single): Boolean;
  var Pan, Vol: Single;
begin
  if PlaySoundAtRect(X, Y, W, H, Pan, Vol, Volume) then
  begin
    SetVolume(Volume * Vol * (gSoundLevel / 255.0));
    SetPan(Pan);
    Result := True
  end
  else
  begin
    SetVolume(0.0);
    SetPan(0.0);
    Result := False
  end;
end;

function TPlayableSound.SetCoords(X, Y: Integer; Volume: Single): Boolean;
begin
  Result := Self.SetCoordsRect(X, Y, 0, 0, Volume)
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

{$IFDEF USE_OPENAL}
initialization
  {$IF DEFINED(USE_FLUIDSYNTH)}
  conRegVar('s_midi_soundfont', @e_SoundFont, 'soundfont to use for midi playback', 'midi soundfont');
  {$ENDIF}
{$ENDIF}

end.
