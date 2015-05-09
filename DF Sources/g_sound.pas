unit g_sound;

interface

uses
  Windows, e_sound;

type
  TPlayableSound = class (TBasicSound)
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
    
    property Loop: Boolean read FLoop write FLoop;
    property Name: String read FName;
  end;

  TMusic = class (TBasicSound)
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
  e_log, SysUtils, g_console, g_options, WADEDITOR,
  g_game, g_basic, g_items, g_map, fmod, fmodtypes, Math,
  g_language;

type
  TGameSound = record
    Name:     ShortString;
    ID:       DWORD;
    IsMusic:  Boolean;
  end;

var
  SoundArray: Array of TGameSound;
  SoundsMuted: Boolean = False;


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
      Result := e_PlaySoundVolume(SoundArray[a].ID, gSoundLevel/255.0);
      Exit;
    end;

  g_FatalError(Format(_lc[I_GAME_ERROR_SOUND], [SoundName]));
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
      Result := e_PlaySoundPanVolume(SoundArray[a].ID, Pan, Volume * (gSoundLevel/255.0));
      Exit;
    end;

  g_FatalError(Format(_lc[I_GAME_ERROR_SOUND], [SoundName]));
end;

function PlaySoundAt(X, Y: Integer; var Pan: Single; var Volume: Single): Boolean;
var
  l1, l2, a: Integer;
  d1, d2: Single;
  c: Boolean;

begin
  l1 := gMaxDist;
  l2 := gMaxDist;

  d1 := 0.0;

  c := gPlayerScreenSize.X >= gMapInfo.Width;

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

  if gPlayer1 <> nil then
  begin
    l1 := PointToRect(X, Y, gPlayer1ScreenCoord.X, gPlayer1ScreenCoord.Y,
                      gPlayerScreenSize.X, gPlayerScreenSize.Y);

    if c then
      d1 := 0.0
    else
      if (X >= gPlayer1ScreenCoord.X) and
         (X <= gPlayer1ScreenCoord.X+gPlayerScreenSize.X) then
        d1 := 0.0
      else
        if X < gPlayer1ScreenCoord.X then
          d1 := -1.0 + X/gPlayer1ScreenCoord.X
        else
          begin
            a := gPlayer1ScreenCoord.X+gPlayerScreenSize.X;
            d1 := (X-a)/(gMapInfo.Width-a);
          end;
  end;

  d2 := d1;

  if gPlayer2 <> nil then
  begin
    l2 := PointToRect(X, Y, gPlayer2ScreenCoord.X, gPlayer2ScreenCoord.Y,
                      gPlayerScreenSize.X, gPlayerScreenSize.Y);

    if c then
      d2 := 0.0
    else
      if (X >= gPlayer2ScreenCoord.X) and
         (X <= gPlayer2ScreenCoord.X+gPlayerScreenSize.X) then
        d2 := 0.0
      else
        if X < gPlayer2ScreenCoord.X then
          d2 := -1.0 + X/gPlayer2ScreenCoord.X
        else
          begin
            a := gPlayer2ScreenCoord.X+gPlayerScreenSize.X;
            d2 := (X-a)/(gMapInfo.Width-a);
          end;
  end;

  d1 := (d1 + d2) / 2.0;

  if l2 < l1 then
    l1 := l2;
  l1 := l1 * 2;

  if l1 >= gMaxDist then 
    begin
      Pan := 0.0;
      Volume := 0.0;
      Result := False;
    end
  else
    begin
      Pan := d1;
      Volume := 1.0 - l1/gMaxDist;
      Result := True;
    end;
end;

function g_Sound_PlayAt(ID: DWORD; X, Y: Integer): Boolean;
var
  Pan, Vol: Single;

begin
  if PlaySoundAt(X, Y, Pan, Vol) then
    Result := e_PlaySoundPanVolume(ID, Pan, Vol * (gSoundLevel/255.0))
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
        Result := e_PlaySoundPanVolume(SoundArray[a].ID,
                    Pan, Vol * (gSoundLevel/255.0));
      Exit;
    end;

  g_FatalError(Format(_lc[I_GAME_ERROR_SOUND], [SoundName]));
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
  WAD: TWADEditor_1;
  FileName,
  SectionName,
  ResourceName: string;
  SoundData: Pointer;
  ResLength: Integer;
  ok: Boolean;

begin
  Result := False;
  ok := False;

  // e_WriteLog('Loading sound: ' + Resource, MSG_NOTIFY);
  g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

  WAD := TWADEditor_1.Create();
  WAD.ReadFile(FileName);

  if WAD.GetResource(SectionName, ResourceName, SoundData, ResLength) then
    begin
      if e_LoadSoundMem(SoundData, ResLength, ID, isMusic) then
        ok := True
      else
        FreeMem(SoundData);
    end
  else
    e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);

  WAD.Free();

  if not ok then
  begin
    if isMusic then
      e_WriteLog(Format('Error loading music %s', [Resource]), MSG_WARNING)
    else
      e_WriteLog(Format('Error loading sound %s', [Resource]), MSG_WARNING);
    Exit;
  end;

  Result := True;
end;

function g_Sound_CreateWADEx(SoundName: ShortString; Resource: string; isMusic: Boolean = False): Boolean;
var
  WAD: TWADEditor_1;
  FileName, SectionName, ResourceName: string;
  SoundData: Pointer;
  ResLength: Integer;
  find_id: DWORD;
  ok: Boolean;

begin
  Result := False;
  ok := False;

  // e_WriteLog('Loading sound: ' + Resource, MSG_NOTIFY);
  g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

  find_id := FindSound();

  WAD := TWADEditor_1.Create();
  WAD.ReadFile(FileName);

  if WAD.GetResource(SectionName, ResourceName, SoundData, ResLength) then
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
    e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);

  WAD.Free();

  if not ok then
  begin
    if isMusic then
      e_WriteLog(Format('Error loading music %s', [Resource]), MSG_WARNING)
    else
      e_WriteLog(Format('Error loading sound %s', [Resource]), MSG_WARNING);
    Exit;
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
  if PlaySoundAt(X, Y, Pan, Vol) then
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
  Result := False;

  if IsPlaying() then
  begin
    if PlaySoundAt(X, Y, Pan, Vol) then
    begin
      SetVolume(Volume * Vol * (gSoundLevel/255.0));
      SetPan(Pan);

      Result := True;
    end;
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
