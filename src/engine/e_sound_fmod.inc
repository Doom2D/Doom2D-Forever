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
interface

uses
  fmod,
  fmodtypes,
  fmoderrors,
  {$IFDEF USE_MEMPOOL}mempool,{$ENDIF}
  e_log,
  SysUtils;

type
  TSoundRec = record
    Data: Pointer;
    Sound: FMOD_SOUND;
    isMusic: Boolean;
    nRefs: Integer;
  end;

  TBasicSound = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  private
    FChannel: FMOD_CHANNEL;

  protected
    FID: DWORD;
    FMusic: Boolean;
    FPosition: DWORD;
    FPriority: Integer;

    function RawPlay(Pan: Single; Volume: Single; aPos: DWORD): Boolean;

  public
    constructor Create();
    destructor Destroy(); override;
    procedure SetID(ID: DWORD);
    procedure FreeSound();
    function IsPlaying(): Boolean;
    procedure Stop();
    function IsPaused(): Boolean;
    procedure Pause(Enable: Boolean);
    function GetVolume(): Single;
    procedure SetVolume(Volume: Single);
    function GetPan(): Single;
    procedure SetPan(Pan: Single);
    function IsMuted(): Boolean;
    procedure Mute(Enable: Boolean);
    function GetPosition(): DWORD;
    procedure SetPosition(aPos: DWORD);
    procedure SetPriority(priority: Integer);
  end;

const
  NO_SOUND_ID = DWORD(-1);

function e_InitSoundSystem(NoOutput: Boolean = False): Boolean;

function e_LoadSound(FileName: string; var ID: DWORD; bLoop: Boolean): Boolean;
function e_LoadSoundMem(pData: Pointer; Length: Integer; var ID: DWORD; bLoop: Boolean): Boolean;

function e_PlaySound(ID: DWORD): Integer;
function e_PlaySoundPan(ID: DWORD; Pan: Single): Integer;
function e_PlaySoundVolume(ID: DWORD; Volume: Single): Integer;
function e_PlaySoundPanVolume(ID: DWORD; Pan, Volume: Single): Integer;

procedure e_ModifyChannelsVolumes(SoundMod: Single; setMode: Boolean);
procedure e_MuteChannels(Enable: Boolean);
procedure e_StopChannels();

procedure e_DeleteSound(ID: DWORD);
procedure e_RemoveAllSounds();
procedure e_ReleaseSoundSystem();
procedure e_SoundUpdate();

var
  e_SoundsArray: array of TSoundRec = nil;

implementation

uses
  g_window, g_options, utils;

const
  N_CHANNELS = 512;

var
  F_System: FMOD_SYSTEM = nil;
  SoundMuted: Boolean = False;


function Channel_Callback(channel: FMOD_CHANNEL; callbacktype: FMOD_CHANNEL_CALLBACKTYPE;
                          commanddata1: Pointer; commanddata2: Pointer): FMOD_RESULT; {$IFDEF WIN32} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  res: FMOD_RESULT;
  sound: FMOD_SOUND;
  ud: Pointer;
  id: DWORD;

begin
  res := FMOD_OK;

  if callbacktype = FMOD_CHANNEL_CALLBACKTYPE_END then
  begin
    res := FMOD_Channel_GetCurrentSound(channel, sound);
    if res = FMOD_OK then
    begin
      res := FMOD_Sound_GetUserData(sound, ud);
      if res = FMOD_OK then
      begin
        id := DWORD(ud^);
        if id < DWORD(Length(e_SoundsArray)) then
          if e_SoundsArray[id].nRefs > 0 then
            Dec(e_SoundsArray[id].nRefs);
      end;
    end;
  end;

  Result := res;
end;

function TryInitWithOutput(Output: FMOD_OUTPUTTYPE; OutputName: String): FMOD_RESULT;
begin
  e_WriteLog('Trying with ' + OutputName + '...', TMsgType.Warning);
  Result := FMOD_System_SetOutput(F_System, Output);
  if Result <> FMOD_OK then
  begin
    e_WriteLog('Error setting FMOD output to ' + OutputName + '!', TMsgType.Warning);
    e_WriteLog(FMOD_ErrorString(Result), TMsgType.Warning);
    Exit;
  end;
  Result := FMOD_System_Init(F_System, N_CHANNELS, FMOD_INIT_NORMAL, nil);
  if Result <> FMOD_OK then
  begin
    e_WriteLog('Error initializing FMOD system!', TMsgType.Warning);
    e_WriteLog(FMOD_ErrorString(Result), TMsgType.Warning);
    Exit;
  end;
end;

function e_TrySS (Freq: Integer; forceNoSound: Integer): Boolean;
var
  res: FMOD_RESULT;
  ver: Cardinal;
  output: FMOD_OUTPUTTYPE;
  drv: Integer;

begin
  Result := False;
  e_WriteLog(Format('Trying to initialize FMOD with %d', [Freq]), TMsgType.Notify);

  res := FMOD_System_Create(F_System);
  if res <> FMOD_OK then
  begin
    e_WriteLog('Error creating FMOD system:', TMsgType.Fatal);
    e_WriteLog(FMOD_ErrorString(res), TMsgType.Fatal);
    Exit;
  end;

  res := FMOD_System_GetVersion(F_System, ver);
  if res <> FMOD_OK then
  begin
    e_WriteLog('Error getting FMOD version:', TMsgType.Fatal);
    e_WriteLog(FMOD_ErrorString(res), TMsgType.Fatal);
    Exit;
  end;

  if ver < FMOD_VERSION then
  begin
    e_WriteLog('FMOD library version is too old! Need '+IntToStr(FMOD_VERSION), TMsgType.Fatal);
    Exit;
  end;

  res := FMOD_System_SetSoftwareFormat(F_System, Freq, FMOD_SOUND_FORMAT_PCM16, 0, 0, FMOD_DSP_RESAMPLER_LINEAR);
  if res <> FMOD_OK then
  begin
    e_WriteLog('Error setting FMOD software format!', TMsgType.Fatal);
    e_WriteLog(FMOD_ErrorString(res), TMsgType.Fatal);
    Exit;
  end;

  if forceNoSound = 2 then
  begin
    res := TryInitWithOutput(FMOD_OUTPUTTYPE_NOSOUND, 'OUTPUTTYPE_NOSOUND');
    if res <> FMOD_OK then
    begin
      e_WriteLog('FMOD: Giving up, can''t init with NOSOUND.', TMsgType.Fatal);
      Exit;
    end;
  end
  else
  begin
    res := FMOD_System_Init(F_System, N_CHANNELS, FMOD_INIT_NORMAL, nil);
    if res <> FMOD_OK then
    begin
      e_WriteLog('Error initializing FMOD system!', TMsgType.Warning);
      e_WriteLog(FMOD_ErrorString(res), TMsgType.Warning);

      {$IFDEF LINUX}
      res := TryInitWithOutput(FMOD_OUTPUTTYPE_ALSA, 'OUTPUTTYPE_ALSA');
      if res <> FMOD_OK then
        res := TryInitWithOutput(FMOD_OUTPUTTYPE_OSS, 'OUTPUTTYPE_OSS');
      {$ENDIF}
      if (res <> FMOD_OK) and (forceNoSound <> 1) then Exit;
      if res <> FMOD_OK then
        res := TryInitWithOutput(FMOD_OUTPUTTYPE_NOSOUND, 'OUTPUTTYPE_NOSOUND');
      if res <> FMOD_OK then
      begin
        e_WriteLog('FMOD: Giving up, can''t init any output.', TMsgType.Fatal);
        Exit;
      end;
    end;
  end;

  res := FMOD_System_GetOutput(F_System, output);
  if res <> FMOD_OK then
    e_WriteLog('Error getting FMOD output!', TMsgType.Warning)
  else
    case output of
      FMOD_OUTPUTTYPE_NOSOUND: e_WriteLog('FMOD Output Method: NOSOUND', TMsgType.Notify);
      FMOD_OUTPUTTYPE_NOSOUND_NRT: e_WriteLog('FMOD Output Method: NOSOUND_NRT', TMsgType.Notify);
      FMOD_OUTPUTTYPE_DSOUND: e_WriteLog('FMOD Output Method: DSOUND', TMsgType.Notify);
      FMOD_OUTPUTTYPE_WINMM: e_WriteLog('FMOD Output Method: WINMM', TMsgType.Notify);
      FMOD_OUTPUTTYPE_OPENAL: e_WriteLog('FMOD Output Method: OPENAL', TMsgType.Notify);
      FMOD_OUTPUTTYPE_WASAPI: e_WriteLog('FMOD Output Method: WASAPI', TMsgType.Notify);
      FMOD_OUTPUTTYPE_ASIO: e_WriteLog('FMOD Output Method: ASIO', TMsgType.Notify);
      FMOD_OUTPUTTYPE_OSS:  e_WriteLog('FMOD Output Method: OSS', TMsgType.Notify);
      FMOD_OUTPUTTYPE_ALSA: e_Writelog('FMOD Output Method: ALSA', TMsgType.Notify);
      else e_WriteLog('FMOD Output Method: Unknown', TMsgType.Notify);
    end;

  res := FMOD_System_GetDriver(F_System, drv);
  if res <> FMOD_OK then
    e_WriteLog('Error getting FMOD driver!', TMsgType.Warning)
  else
    e_WriteLog('FMOD driver id: '+IntToStr(drv), TMsgType.Notify);

  Result := True;
end;

function e_InitSoundSystem(NoOutput: Boolean = False): Boolean;
begin
  if NoOutput then
  begin
    Result := e_TrySS(48000, 2);
    Exit;
  end;
  Result := e_TrySS(48000, 0);
  if not Result then Result := e_TrySS(44100, 1);
end;

function FindESound(): DWORD;
var
  i: Integer;

begin
  if e_SoundsArray <> nil then
    for i := 0 to High(e_SoundsArray) do
      if e_SoundsArray[i].Sound = nil then
      begin
        Result := i;
        Exit;
      end;

  if e_SoundsArray = nil then
    begin
      SetLength(e_SoundsArray, 16);
      Result := 0;
    end
  else
    begin
      Result := High(e_SoundsArray) + 1;
      SetLength(e_SoundsArray, Length(e_SoundsArray) + 16);
    end;
end;

function e_LoadSound(FileName: String; var ID: DWORD; bLoop: Boolean): Boolean;
var
  find_id: DWORD;
  res: FMOD_RESULT;
  bt: Cardinal;
  ud: Pointer;

begin
  Result := False;

  e_WriteLog('Loading sound '+FileName+'...', TMsgType.Notify);

  find_id := FindESound();

  if bLoop then
    bt := FMOD_LOOP_NORMAL
  else
    bt := FMOD_LOOP_OFF;

  if not bLoop then
    res := FMOD_System_CreateSound(F_System, PAnsiChar(FileName),
             bt + FMOD_2D + FMOD_HARDWARE,
             nil, e_SoundsArray[find_id].Sound)
  else
    res := FMOD_System_CreateStream(F_System, PAnsiChar(FileName),
             bt + FMOD_2D + FMOD_HARDWARE,
             nil, e_SoundsArray[find_id].Sound);
  if res <> FMOD_OK then
  begin
    e_SoundsArray[find_id].Sound := nil;
    Exit;
  end;

  GetMem(ud, SizeOf(DWORD));
  DWORD(ud^) := find_id;
  res := FMOD_Sound_SetUserData(e_SoundsArray[find_id].Sound, ud);
  if res <> FMOD_OK then
  begin
    e_SoundsArray[find_id].Sound := nil;
    Exit;
  end;

  e_SoundsArray[find_id].Data := nil;
  e_SoundsArray[find_id].isMusic := bLoop;
  e_SoundsArray[find_id].nRefs := 0;

  ID := find_id;

  Result := True;
end;

function e_LoadSoundMem(pData: Pointer; Length: Integer; var ID: DWORD; bLoop: Boolean): Boolean;
var
  find_id: DWORD;
  res: FMOD_RESULT;
  sz: Integer;
  bt: Cardinal;
  soundExInfo: FMOD_CREATESOUNDEXINFO;
  ud: Pointer;

begin
  Result := False;

  find_id := FindESound();

  sz := SizeOf(FMOD_CREATESOUNDEXINFO);
  FillMemory(@soundExInfo, sz, 0);
  soundExInfo.cbsize := sz;
  soundExInfo.length := Length;

  if bLoop then
    bt := FMOD_LOOP_NORMAL
  else
    bt := FMOD_LOOP_OFF;

  if not bLoop then
    res := FMOD_System_CreateSound(F_System, pData,
             bt + FMOD_2D + FMOD_HARDWARE + FMOD_OPENMEMORY,
             @soundExInfo, e_SoundsArray[find_id].Sound)
  else
    res := FMOD_System_CreateStream(F_System, pData,
             bt + FMOD_2D + FMOD_HARDWARE + FMOD_OPENMEMORY,
             @soundExInfo, e_SoundsArray[find_id].Sound);
  if res <> FMOD_OK then
  begin
    e_SoundsArray[find_id].Sound := nil;
    Exit;
  end;

  GetMem(ud, SizeOf(DWORD));
  DWORD(ud^) := find_id;
  res := FMOD_Sound_SetUserData(e_SoundsArray[find_id].Sound, ud);
  if res <> FMOD_OK then
  begin
    e_SoundsArray[find_id].Sound := nil;
    Exit;
  end;

  e_SoundsArray[find_id].Data := pData;
  e_SoundsArray[find_id].isMusic := bLoop;
  e_SoundsArray[find_id].nRefs := 0;

  ID := find_id;

  Result := True;
end;

function e_PlaySound(ID: DWORD): Integer;
var
  res: FMOD_RESULT;
  Chan: FMOD_CHANNEL;

begin
  if e_SoundsArray[ID].nRefs >= gMaxSimSounds then
  begin
    Result := 0;
    Exit;
  end;

  Result := -1;

  res := FMOD_System_PlaySound(F_System, FMOD_CHANNEL_FREE,
           e_SoundsArray[ID].Sound, False, Chan);
  if res <> FMOD_OK then
  begin
    Exit;
  end;

  res := FMOD_Channel_SetCallback(Chan, Channel_Callback);
  if res <> FMOD_OK then
  begin
  end;

  if SoundMuted then
  begin
    res := FMOD_Channel_SetMute(Chan, True);
    if res <> FMOD_OK then
    begin
    end;
  end;

  Inc(e_SoundsArray[ID].nRefs);
  Result := 0;
end;

function e_PlaySoundPan(ID: DWORD; Pan: Single): Integer;
var
  res: FMOD_RESULT;
  Chan: FMOD_CHANNEL;

begin
  if e_SoundsArray[ID].nRefs >= gMaxSimSounds then
  begin
    Result := 0;
    Exit;
  end;

  Result := -1;

  res := FMOD_System_PlaySound(F_System, FMOD_CHANNEL_FREE,
           e_SoundsArray[ID].Sound, False, Chan);
  if res <> FMOD_OK then
  begin
    Exit;
  end;

  res := FMOD_Channel_SetPan(Chan, Pan);
  if res <> FMOD_OK then
  begin
  end;

  res := FMOD_Channel_SetCallback(Chan, Channel_Callback);
  if res <> FMOD_OK then
  begin
  end;

  if SoundMuted then
  begin
    res := FMOD_Channel_SetMute(Chan, True);
    if res <> FMOD_OK then
    begin
    end;
  end;

  Inc(e_SoundsArray[ID].nRefs);
  Result := 0;
end;

function e_PlaySoundVolume(ID: DWORD; Volume: Single): Integer;
var
  res: FMOD_RESULT;
  Chan: FMOD_CHANNEL;

begin
  if e_SoundsArray[ID].nRefs >= gMaxSimSounds then
  begin
    Result := 0;
    Exit;
  end;

  Result := -1;

  res := FMOD_System_PlaySound(F_System, FMOD_CHANNEL_FREE,
           e_SoundsArray[ID].Sound, False, Chan);
  if res <> FMOD_OK then
  begin
    Exit;
  end;

  res := FMOD_Channel_SetVolume(Chan, Volume);
  if res <> FMOD_OK then
  begin
  end;

  res := FMOD_Channel_SetCallback(Chan, Channel_Callback);
  if res <> FMOD_OK then
  begin
  end;

  if SoundMuted then
  begin
    res := FMOD_Channel_SetMute(Chan, True);
    if res <> FMOD_OK then
    begin
    end;
  end;

  Inc(e_SoundsArray[ID].nRefs);
  Result := 0;
end;

function e_PlaySoundPanVolume(ID: DWORD; Pan, Volume: Single): Integer;
var
  res: FMOD_RESULT;
  Chan: FMOD_CHANNEL;

begin
  if e_SoundsArray[ID].nRefs >= gMaxSimSounds then
  begin
    Result := 0;
    Exit;
  end;

  Result := -1;

  res := FMOD_System_PlaySound(F_System, FMOD_CHANNEL_FREE,
           e_SoundsArray[ID].Sound, False, Chan);
  if res <> FMOD_OK then
  begin
    Exit;
  end;

  res := FMOD_Channel_SetPan(Chan, Pan);
  if res <> FMOD_OK then
  begin
  end;

  res := FMOD_Channel_SetVolume(Chan, Volume);
  if res <> FMOD_OK then
  begin
  end;

  res := FMOD_Channel_SetCallback(Chan, Channel_Callback);
  if res <> FMOD_OK then
  begin
  end;

  if SoundMuted then
  begin
    res := FMOD_Channel_SetMute(Chan, True);
    if res <> FMOD_OK then
    begin
    end;
  end;

  Inc(e_SoundsArray[ID].nRefs);
  Result := 0;
end;

procedure e_DeleteSound(ID: DWORD);
var
  res: FMOD_RESULT;
  ud: Pointer;

begin
  if e_SoundsArray[ID].Sound = nil then
    Exit;

  if e_SoundsArray[ID].Data <> nil then
    FreeMem(e_SoundsArray[ID].Data);

  res := FMOD_Sound_GetUserData(e_SoundsArray[ID].Sound, ud);
  if res = FMOD_OK then
  begin
    FreeMem(ud);
  end;

  res := FMOD_Sound_Release(e_SoundsArray[ID].Sound);
  if res <> FMOD_OK then
  begin
    e_WriteLog('Error releasing sound:', TMsgType.Warning);
    e_WriteLog(FMOD_ErrorString(res), TMsgType.Warning);
  end;

  e_SoundsArray[ID].Sound := nil;
  e_SoundsArray[ID].Data := nil;
end;

procedure e_ModifyChannelsVolumes(SoundMod: Single; setMode: Boolean);
var
  res: FMOD_RESULT;
  i: Integer;
  Chan: FMOD_CHANNEL;
  vol: Single;

begin
  for i := 0 to N_CHANNELS-1 do
  begin
    Chan := nil;
    res := FMOD_System_GetChannel(F_System, i, Chan);

    if (res = FMOD_OK) and (Chan <> nil) then
    begin
      res := FMOD_Channel_GetVolume(Chan, vol);

      if res = FMOD_OK then
      begin
        if setMode then
          vol := SoundMod
        else
          vol := vol * SoundMod;

        res := FMOD_Channel_SetVolume(Chan, vol);

        if res <> FMOD_OK then
        begin
        end;
      end;
    end;
  end;
end;

procedure e_MuteChannels(Enable: Boolean);
var
  res: FMOD_RESULT;
  i: Integer;
  Chan: FMOD_CHANNEL;

begin
  if Enable = SoundMuted then
    Exit;

  SoundMuted := Enable;

  for i := 0 to N_CHANNELS-1 do
  begin
    Chan := nil;
    res := FMOD_System_GetChannel(F_System, i, Chan);

    if (res = FMOD_OK) and (Chan <> nil) then
    begin
      res := FMOD_Channel_SetMute(Chan, Enable);

      if res <> FMOD_OK then
      begin
      end;
    end;
  end;
end;

procedure e_StopChannels();
var
  res: FMOD_RESULT;
  i: Integer;
  Chan: FMOD_CHANNEL;

begin
  for i := 0 to N_CHANNELS-1 do
  begin
    Chan := nil;
    res := FMOD_System_GetChannel(F_System, i, Chan);

    if (res = FMOD_OK) and (Chan <> nil) then
    begin
      res := FMOD_Channel_Stop(Chan);

      if res <> FMOD_OK then
      begin
      end;
    end;
  end;
end;

procedure e_RemoveAllSounds();
var
  i: Integer;

begin
  for i := 0 to High(e_SoundsArray) do
    if e_SoundsArray[i].Sound <> nil then
      e_DeleteSound(i);

  SetLength(e_SoundsArray, 0);
  e_SoundsArray := nil;
end;

procedure e_ReleaseSoundSystem();
var
  res: FMOD_RESULT;

begin
  e_RemoveAllSounds();

  res := FMOD_System_Close(F_System);
  if res <> FMOD_OK then
  begin
    e_WriteLog('Error closing FMOD system!', TMsgType.Fatal);
    e_WriteLog(FMOD_ErrorString(res), TMsgType.Fatal);
    Exit;
  end;

  res := FMOD_System_Release(F_System);
  if res <> FMOD_OK then
  begin
    e_WriteLog('Error releasing FMOD system!', TMsgType.Fatal);
    e_WriteLog(FMOD_ErrorString(res), TMsgType.Fatal);
  end;
end;

procedure e_SoundUpdate();
begin
  FMOD_System_Update(F_System);
end;

{ TBasicSound: }

constructor TBasicSound.Create();
begin
  FID := NO_SOUND_ID;
  FMusic := False;
  FChannel := nil;
  FPosition := 0;
  FPriority := 128;
end;

destructor TBasicSound.Destroy();
begin
  FreeSound();
  inherited;
end;

procedure TBasicSound.FreeSound();
begin
  if FID = NO_SOUND_ID then
    Exit;

  Stop();
  FID := NO_SOUND_ID;
  FMusic := False;
  FPosition := 0;
end;

function TBasicSound.RawPlay(Pan: Single; Volume: Single; aPos: DWORD): Boolean;
var
  res: FMOD_RESULT;

begin
  Result := False;
  if FID = NO_SOUND_ID then Exit;

  if e_SoundsArray[FID].nRefs >= gMaxSimSounds then
  begin
    Result := True;
    Exit;
  end;

  res := FMOD_System_PlaySound(F_System, FMOD_CHANNEL_FREE,
           e_SoundsArray[FID].Sound, False, FChannel);
  if res <> FMOD_OK then
  begin
    FChannel := nil;
    Exit;
  end;

  res := FMOD_Channel_SetPosition(FChannel, aPos, FMOD_TIMEUNIT_MS);
  if res <> FMOD_OK then
    begin
      FPosition := 0;
    end
  else
    FPosition := aPos;

  res := FMOD_Channel_SetPan(FChannel, Pan);
  if res <> FMOD_OK then
  begin
  end;

  res := FMOD_Channel_SetVolume(FChannel, Volume);
  if res <> FMOD_OK then
  begin
  end;

  res := FMOD_Channel_SetCallback(FChannel, Channel_Callback);
  if res <> FMOD_OK then
  begin
  end;

  if SoundMuted then
  begin
    res := FMOD_Channel_SetMute(FChannel, True);
    if res <> FMOD_OK then
    begin
    end;
  end;

  Inc(e_SoundsArray[FID].nRefs);
  Result := True;
end;

procedure TBasicSound.SetID(ID: DWORD);
begin
  FreeSound();
  FID := ID;
  FMusic := e_SoundsArray[ID].isMusic;
end;

function TBasicSound.IsPlaying(): Boolean;
var
  res: FMOD_RESULT;
  b: LongBool;

begin
  Result := False;

  if FChannel = nil then
    Exit;

  res := FMOD_Channel_IsPlaying(FChannel, b);
  if res <> FMOD_OK then
  begin
    Exit;
  end;

  Result := b;
end;

procedure TBasicSound.Stop();
var
  res: FMOD_RESULT;

begin
  if FChannel = nil then
    Exit;

  GetPosition();

  res := FMOD_Channel_Stop(FChannel);
  if res <> FMOD_OK then
  begin
  end;

  FChannel := nil;
end;

function TBasicSound.IsPaused(): Boolean;
var
  res: FMOD_RESULT;
  b: LongBool;

begin
  Result := False;

  if FChannel = nil then
    Exit;

  res := FMOD_Channel_GetPaused(FChannel, b);
  if res <> FMOD_OK then
  begin
    Exit;
  end;

  Result := b;
end;

procedure TBasicSound.Pause(Enable: Boolean);
var
  res: FMOD_RESULT;

begin
  if FChannel = nil then
    Exit;

  res := FMOD_Channel_SetPaused(FChannel, Enable);
  if res <> FMOD_OK then
  begin
  end;

  if Enable then
  begin
    res := FMOD_Channel_GetPosition(FChannel, FPosition, FMOD_TIMEUNIT_MS);
    if res <> FMOD_OK then
    begin
    end;
  end;
end;

function TBasicSound.GetVolume(): Single;
var
  res: FMOD_RESULT;
  vol: Single;

begin
  Result := 0.0;

  if FChannel = nil then
    Exit;

  res := FMOD_Channel_GetVolume(FChannel, vol);
  if res <> FMOD_OK then
  begin
    Exit;
  end;

  Result := vol;
end;

procedure TBasicSound.SetVolume(Volume: Single);
var
  res: FMOD_RESULT;

begin
  if FChannel = nil then
    Exit;

  res := FMOD_Channel_SetVolume(FChannel, Volume);
  if res <> FMOD_OK then
  begin
  end;
end;

function TBasicSound.GetPan(): Single;
var
  res: FMOD_RESULT;
  pan: Single;

begin
  Result := 0.0;

  if FChannel = nil then
    Exit;

  res := FMOD_Channel_GetPan(FChannel, pan);
  if res <> FMOD_OK then
  begin
    Exit;
  end;

  Result := pan;
end;

procedure TBasicSound.SetPan(Pan: Single);
var
  res: FMOD_RESULT;

begin
  if FChannel = nil then
    Exit;

  res := FMOD_Channel_SetPan(FChannel, Pan);
  if res <> FMOD_OK then
  begin
  end;
end;

function TBasicSound.IsMuted(): Boolean;
var
  res: FMOD_RESULT;
  b: LongBool;

begin
  Result := False;

  if FChannel = nil then
    Exit;

  res := FMOD_Channel_GetMute(FChannel, b);
  if res <> FMOD_OK then
  begin
    Exit;
  end;

  Result := b;
end;

procedure TBasicSound.Mute(Enable: Boolean);
var
  res: FMOD_RESULT;

begin
  if FChannel = nil then
    Exit;

  res := FMOD_Channel_SetMute(FChannel, Enable);
  if res <> FMOD_OK then
  begin
  end;
end;

function TBasicSound.GetPosition(): DWORD;
var
  res: FMOD_RESULT;

begin
  Result := 0;

  if FChannel = nil then
    Exit;

  res := FMOD_Channel_GetPosition(FChannel, FPosition, FMOD_TIMEUNIT_MS);
  if res <> FMOD_OK then
  begin
    Exit;
  end;

  Result := FPosition;
end;

procedure TBasicSound.SetPosition(aPos: DWORD);
var
  res: FMOD_RESULT;

begin
  FPosition := aPos;

  if FChannel = nil then
    Exit;

  res := FMOD_Channel_SetPosition(FChannel, FPosition, FMOD_TIMEUNIT_MS);
  if res <> FMOD_OK then
  begin
  end;
end;

procedure TBasicSound.SetPriority(priority: Integer);
var
  res: FMOD_RESULT;

begin
  if (FChannel <> nil) and (FPriority <> priority) and
     (priority >= 0) and (priority <= 256) then
  begin
    FPriority := priority;
    res := FMOD_Channel_SetPriority(FChannel, priority);
    if res <> FMOD_OK then
    begin
    end;
  end;
end;

end.
