unit e_sound;

interface

uses
  fmod,
  fmodtypes,
  fmoderrors,
  e_log,
  SysUtils,
  Windows;

type
  TSoundRec = record
    Data: Pointer;
    Sound: FMOD_SOUND;
    Loop: Boolean;
    nRefs: Integer;
  end;

  TBasicSound = class (TObject)
  private
    FChannel: FMOD_CHANNEL;
  
  protected
    FID: DWORD;
    FLoop: Boolean;
    FPosition: DWORD;

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
  end;

const
  NO_SOUND_ID = DWORD(-1);

function e_InitSoundSystem(Freq: Integer): Boolean;

function e_LoadSound(FileName: string; var ID: DWORD; bLoop: Boolean): Boolean;
function e_LoadSoundMem(pData: Pointer; Length: Integer; var ID: DWORD; bLoop: Boolean): Boolean;

function e_PlaySound(ID: DWORD): Boolean;
function e_PlaySoundPan(ID: DWORD; Pan: Single): Boolean;
function e_PlaySoundVolume(ID: DWORD; Volume: Single): Boolean;
function e_PlaySoundPanVolume(ID: DWORD; Pan, Volume: Single): Boolean;

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
  g_window, g_options;

const
  N_CHANNELS = 64;

var
  F_System: FMOD_SYSTEM = nil;
  SoundMuted: Boolean = False;


function Channel_Callback(channel: FMOD_CHANNEL; callbacktype: FMOD_CHANNEL_CALLBACKTYPE;
                          commanddata1: Pointer; commanddata2: Pointer): FMOD_RESULT; stdcall;
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
        if id < Length(e_SoundsArray) then
          if e_SoundsArray[id].nRefs > 0 then
            Dec(e_SoundsArray[id].nRefs);
      end;
    end;
  end;

  Result := res;
end;

function e_InitSoundSystem(Freq: Integer): Boolean;
var
  res: FMOD_RESULT;
  ver: Cardinal;
  output: FMOD_OUTPUTTYPE;
  drv: Integer;
  str: PAnsiChar;

begin
  Result := False;

  res := FMOD_System_Create(F_System);
  if res <> FMOD_OK then
  begin
    e_WriteLog('Error creating FMOD system!', MSG_FATALERROR);
    e_WriteLog(FMOD_ErrorString(res), MSG_FATALERROR);
    Exit;
  end;

  res := FMOD_System_GetVersion(F_System, ver);
  if res <> FMOD_OK then
  begin
    e_WriteLog('Error getting FMOD version!', MSG_FATALERROR);
    e_WriteLog(FMOD_ErrorString(res), MSG_FATALERROR);
    Exit;
  end;

  if ver < FMOD_VERSION then
  begin
    e_WriteLog('FMOD version is too old! Need '+IntToStr(FMOD_VERSION), MSG_FATALERROR);
    Exit;
  end;

  res := FMOD_System_SetOutput(F_System, FMOD_OUTPUTTYPE_DSOUND);
  if res <> FMOD_OK then
  begin
    e_WriteLog('Error setting FMOD output type!', MSG_FATALERROR);
    e_WriteLog(FMOD_ErrorString(res), MSG_FATALERROR);
    Exit;
  end;

  res := FMOD_System_SetSoftwareFormat(F_System, Freq,
           FMOD_SOUND_FORMAT_PCM16, 0, 0, FMOD_DSP_RESAMPLER_LINEAR);
  if res <> FMOD_OK then
  begin
    e_WriteLog('Error setting FMOD software format!', MSG_FATALERROR);
    e_WriteLog(FMOD_ErrorString(res), MSG_FATALERROR);
    Exit;
  end;

  res := FMOD_System_Init(F_System, N_CHANNELS, FMOD_INIT_NORMAL, nil);
  if res <> FMOD_OK then
  begin
    e_WriteLog('Error initializing FMOD system!', MSG_FATALERROR);
    e_WriteLog(FMOD_ErrorString(res), MSG_FATALERROR);
    Exit;
  end;

  res := FMOD_System_GetOutput(F_System, output);
  if res <> FMOD_OK then
    e_WriteLog('Error getting FMOD output!', MSG_WARNING)
  else
    case output of
      FMOD_OUTPUTTYPE_NOSOUND: e_WriteLog('FMOD Output Method: NOSOUND', MSG_NOTIFY);
      FMOD_OUTPUTTYPE_NOSOUND_NRT: e_WriteLog('FMOD Output Method: NOSOUND_NRT', MSG_NOTIFY);
      FMOD_OUTPUTTYPE_DSOUND: e_WriteLog('FMOD Output Method: DSOUND', MSG_NOTIFY);
      FMOD_OUTPUTTYPE_WINMM: e_WriteLog('FMOD Output Method: WINMM', MSG_NOTIFY);
      FMOD_OUTPUTTYPE_OPENAL: e_WriteLog('FMOD Output Method: OPENAL', MSG_NOTIFY);
      FMOD_OUTPUTTYPE_WASAPI: e_WriteLog('FMOD Output Method: WASAPI', MSG_NOTIFY);
      FMOD_OUTPUTTYPE_ASIO: e_WriteLog('FMOD Output Method: ASIO', MSG_NOTIFY);
      else e_WriteLog('FMOD Output Method: Unknown', MSG_NOTIFY);
    end;

  res := FMOD_System_GetDriver(F_System, drv);
  if res <> FMOD_OK then
    e_WriteLog('Error getting FMOD driver!', MSG_WARNING)
  else
    begin
      {res := FMOD_System_GetDriverName(F_System, drv, str, 64);
      if res <> FMOD_OK then
        e_WriteLog('Error getting FMOD driver name!', MSG_WARNING)
      else }
        e_WriteLog('FMOD driver id: '+IntToStr(drv), MSG_NOTIFY);
    end;

  Result := True;
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

  e_WriteLog('Loading sound '+FileName+'...', MSG_NOTIFY);

  find_id := FindESound();

  if bLoop then
    bt := FMOD_LOOP_NORMAL
  else
    bt := FMOD_LOOP_OFF;

  if not bLoop then
    res := FMOD_System_CreateSound(F_System, PAnsiChar(FileName),
             bt + FMOD_2D + FMOD_HARDWARE,
             0, e_SoundsArray[find_id].Sound)
  else
    res := FMOD_System_CreateStream(F_System, PAnsiChar(FileName),
             bt + FMOD_2D + FMOD_HARDWARE,
             0, e_SoundsArray[find_id].Sound);
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
  e_SoundsArray[find_id].Loop := bLoop;
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

  e_WriteLog('Loading sound from $'+IntToHex(Integer(pData), 8), MSG_NOTIFY);

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
  e_SoundsArray[find_id].Loop := bLoop;
  e_SoundsArray[find_id].nRefs := 0;

  ID := find_id;

  Result := True;
end;

function e_PlaySound(ID: DWORD): Boolean;
var
  res: FMOD_RESULT;
  Chan: FMOD_CHANNEL;

begin
  if e_SoundsArray[ID].nRefs >= gMaxSimSounds then
  begin
    Result := True;
    Exit;
  end;

  Result := False;

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
  Result := True;
end;

function e_PlaySoundPan(ID: DWORD; Pan: Single): Boolean;
var
  res: FMOD_RESULT;
  Chan: FMOD_CHANNEL;

begin
  if e_SoundsArray[ID].nRefs >= gMaxSimSounds then
  begin
    Result := True;
    Exit;
  end;

  Result := False;

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
  Result := True;
end;

function e_PlaySoundVolume(ID: DWORD; Volume: Single): Boolean;
var
  res: FMOD_RESULT;
  Chan: FMOD_CHANNEL;

begin
  if e_SoundsArray[ID].nRefs >= gMaxSimSounds then
  begin
    Result := True;
    Exit;
  end;

  Result := False;

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
  Result := True;
end;

function e_PlaySoundPanVolume(ID: DWORD; Pan, Volume: Single): Boolean;
var
  res: FMOD_RESULT;
  Chan: FMOD_CHANNEL;

begin
  if e_SoundsArray[ID].nRefs >= gMaxSimSounds then
  begin
    Result := True;
    Exit;
  end;

  Result := False;

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
  Result := True;
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
    e_WriteLog('Error releasing sound:', MSG_WARNING);
    e_WriteLog(FMOD_ErrorString(res), MSG_WARNING);
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
    e_WriteLog('Error closing FMOD system!', MSG_FATALERROR);
    e_WriteLog(FMOD_ErrorString(res), MSG_FATALERROR);
    Exit;
  end;
  
  res := FMOD_System_Release(F_System);
  if res <> FMOD_OK then
  begin
    e_WriteLog('Error releasing FMOD system!', MSG_FATALERROR);
    e_WriteLog(FMOD_ErrorString(res), MSG_FATALERROR);
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
  FLoop := False;
  FChannel := nil;
  FPosition := 0;
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
  FLoop := False;
  FPosition := 0;
end;

function TBasicSound.RawPlay(Pan: Single; Volume: Single; aPos: DWORD): Boolean;
var
  res: FMOD_RESULT;

begin
  if e_SoundsArray[FID].nRefs >= gMaxSimSounds then
  begin
    Result := True;
    Exit;
  end;

  Result := False;

  if FID = NO_SOUND_ID then
    Exit;

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
  FLoop := e_SoundsArray[ID].Loop;
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

end.
