unit e_sound;

interface

uses
  sdl2 in '../lib/sdl2/sdl2.pas',
  SDL2_mixer in '../lib/sdl2/SDL2_mixer.pas',
  e_log,
  SysUtils;

type
  TSoundRec = record
    Data: Pointer;
    Sound: PMix_Chunk;
    Loop: Boolean;
  end;

  TBasicSound = class (TObject)
  private
    FChannel: Integer; // <0: no channel allocated

  protected
    FID: DWORD;
    FLoop: Boolean;
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

function e_InitSoundSystem(): Boolean;

function e_LoadSound(FileName: string; var ID: DWORD; bLoop: Boolean): Boolean;
function e_LoadSoundMem(pData: Pointer; Length: Integer; var ID: DWORD; bLoop: Boolean): Boolean;

// returns channel number or -1
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
  g_window, g_options, BinEditor;

const
  N_CHANNELS = 512;

var
  SoundMuted: Boolean = False;
  SoundInitialized: Boolean = False;


function e_InitSoundSystem(): Boolean;
var
  res: Integer;
begin
  if SoundInitialized then begin Result := true; Exit end;

  Result := False;
  SoundInitialized := False;

  // we need module player
  if (Mix_Init(MIX_INIT_MOD) and MIX_INIT_MOD) <> MIX_INIT_MOD then
  begin
    e_WriteLog('Error initializing SDL module player:', MSG_FATALERROR);
    e_WriteLog(Mix_GetError(), MSG_FATALERROR);
    //Exit;
  end;

  res := Mix_OpenAudio(44100, AUDIO_S16LSB, 2, 512);
  if res = -1 then
  begin
    e_WriteLog('Error initializing SDL mixer:', MSG_FATALERROR);
    e_WriteLog(Mix_GetError(), MSG_FATALERROR);
    Exit;
  end;

  Mix_AllocateChannels(N_CHANNELS);

  SoundInitialized := True;
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
begin
  Result := False;
  if not SoundInitialized then Exit;

  e_WriteLog('Loading sound '+FileName+'...', MSG_NOTIFY);

  find_id := FindESound();

  e_SoundsArray[find_id].Sound := Mix_LoadWAV(PAnsiChar(FileName));
  if e_SoundsArray[find_id].Sound = nil then Exit;

  e_SoundsArray[find_id].Data := nil;
  e_SoundsArray[find_id].Loop := bLoop;

  ID := find_id;

  Result := True;
end;

function e_LoadSoundMem(pData: Pointer; Length: Integer; var ID: DWORD; bLoop: Boolean): Boolean;
var
  find_id: DWORD;
  rw: PSDL_RWops;
begin
  Result := False;
  if not SoundInitialized then Exit;

  rw := SDL_RWFromConstMem(pData, Length);
  if rw = nil then Exit;

  find_id := FindESound();

  e_SoundsArray[find_id].Sound := Mix_LoadWAV_RW(rw, 0);
  SDL_FreeRW(rw);
  if e_SoundsArray[find_id].Sound = nil then Exit;

  e_SoundsArray[find_id].Data := pData;
  e_SoundsArray[find_id].Loop := bLoop;

  ID := find_id;

  Result := True;
end;

function e_PlaySound (ID: DWORD): Integer;
var
  res: Integer;
begin
  Result := -1;
  if not SoundInitialized then Exit;

  if {(e_SoundsArray[ID].nRefs >= gMaxSimSounds) or} (e_SoundsArray[ID].Sound = nil) then Exit;

  if e_SoundsArray[ID].Loop then
    res := Mix_PlayChannel(-1, e_SoundsArray[ID].Sound, -1)
  else
    res := Mix_PlayChannel(-1, e_SoundsArray[ID].Sound, 0);

  if SoundMuted and (res >= 0) then Mix_Volume(res, 0);

  Result := res;
end;

function e_PlaySoundPan(ID: DWORD; Pan: Single): Integer;
var
  chan: Integer;
  l, r: UInt8;
begin
  Result := -1;
  chan := e_PlaySound(ID);
  if chan < 0 then Exit;

  if not SoundMuted then
  if Pan < -1.0 then Pan := -1.0 else if Pan > 1.0 then Pan := 1.0;
  Pan := Pan+1.0; // 0..2
  l := trunc(127.0*(2.0-Pan));
  r := trunc(127.0*Pan);
  Mix_SetPanning(chan, l, r);

  Result := chan;
end;

function e_PlaySoundVolume(ID: DWORD; Volume: Single): Integer;
var
  chan: Integer;
begin
  Result := -1;
  chan := e_PlaySound(ID);
  if chan < 0 then Exit;

  if Volume < 0 then Volume := 0 else if Volume > 1 then Volume := 1;
  if not SoundMuted then Mix_Volume(chan, trunc(Volume*MIX_MAX_VOLUME));

  Result := chan;
end;

function e_PlaySoundPanVolume(ID: DWORD; Pan, Volume: Single): Integer;
var
  chan: Integer;
  l, r: UInt8;
begin
  Result := -1;
  chan := e_PlaySound(ID);
  if chan < 0 then Exit;

  if Pan < -1.0 then Pan := -1.0 else if Pan > 1.0 then Pan := 1.0;
  Pan := Pan+1.0; // 0..2
  l := trunc(127.0*(2.0-Pan));
  r := trunc(127.0*Pan);
  Mix_SetPanning(chan, l, r);

  if Volume < 0 then Volume := 0 else if Volume > 1 then Volume := 1;
  if not SoundMuted then Mix_Volume(chan, trunc(Volume*MIX_MAX_VOLUME));

  Result := chan;
end;

procedure e_DeleteSound(ID: DWORD);
begin
  if e_SoundsArray[ID].Sound = nil then Exit;
  if e_SoundsArray[ID].Data <> nil then FreeMem(e_SoundsArray[ID].Data);

  Mix_FreeChunk(e_SoundsArray[ID].Sound);

  e_SoundsArray[ID].Sound := nil;
  e_SoundsArray[ID].Data := nil;
end;

//TODO
procedure e_ModifyChannelsVolumes(SoundMod: Single; setMode: Boolean);
{
var
  i: Integer;
  Chan: FMOD_CHANNEL;
  vol: Single;
}
begin
  // Mix_Volume(-1, volm);
{
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
}
end;

//TODO
procedure e_MuteChannels(Enable: Boolean);
{
var
  res: FMOD_RESULT;
  i: Integer;
  Chan: FMOD_CHANNEL;
}
begin
{
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
}
end;

procedure e_StopChannels();
begin
  Mix_HaltChannel(-1);
end;

procedure e_RemoveAllSounds();
var
  i: Integer;
begin
  if SoundInitialized then e_StopChannels();

  for i := 0 to High(e_SoundsArray) do
    if e_SoundsArray[i].Sound <> nil then
      e_DeleteSound(i);

  SetLength(e_SoundsArray, 0);
  e_SoundsArray := nil;
end;

procedure e_ReleaseSoundSystem();
begin
  e_RemoveAllSounds();

  if SoundInitialized then
  begin
    Mix_CloseAudio();
    SoundInitialized := False;
  end;
end;

procedure e_SoundUpdate();
begin
  //FMOD_System_Update(F_System);
end;

{ TBasicSound: }

constructor TBasicSound.Create();
begin
  FID := NO_SOUND_ID;
  FLoop := False;
  FChannel := -1;
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
  if FID = NO_SOUND_ID then Exit;
  Stop();
  FID := NO_SOUND_ID;
  FLoop := False;
  FPosition := 0;
end;

// aPos: msecs
function TBasicSound.RawPlay(Pan: Single; Volume: Single; aPos: DWORD): Boolean;
var
  oldloop: Boolean;
begin
  Result := False;
  if (FID = NO_SOUND_ID) or not SoundInitialized then Exit;
  oldloop := e_SoundsArray[FID].Loop;
  e_SoundsArray[FID].Loop := FLoop;
  Result := (e_PlaySoundPanVolume(FID, Pan, Volume) >= 0);
  e_SoundsArray[FID].Loop := oldloop;
  //TODO: aPos
end;

procedure TBasicSound.SetID(ID: DWORD);
begin
  FreeSound();
  FID := ID;
  FLoop := e_SoundsArray[ID].Loop;
end;

function TBasicSound.IsPlaying(): Boolean;
begin
  Result := False;
  if FChannel < 0 then Exit;
  Result := (Mix_Playing(FChannel) > 0);
end;

procedure TBasicSound.Stop();

begin
  if FChannel < 0 then Exit;
  //GetPosition();
  Mix_HaltChannel(FChannel);
  FChannel := -1;
end;

function TBasicSound.IsPaused(): Boolean;
begin
  Result := False;
  if FChannel < 0 then Exit;
  Result := (Mix_Paused(FChannel) > 0);
end;

procedure TBasicSound.Pause(Enable: Boolean);
begin
  if FChannel < 0 then Exit;
  if Mix_Paused(FChannel) > 0 then
  begin
    if Enable then Mix_Resume(FChannel);
  end
  else
  begin
    if not Enable then Mix_Pause(FChannel);
  end;
  {
  if Enable then
  begin
    res := FMOD_Channel_GetPosition(FChannel, FPosition, FMOD_TIMEUNIT_MS);
    if res <> FMOD_OK then
    begin
    end;
  end;
  }
end;

//TODO
function TBasicSound.GetVolume(): Single;
begin
  Result := 0.0;
  if FChannel < 0 then Exit;
{
  res := FMOD_Channel_GetVolume(FChannel, vol);
  if res <> FMOD_OK then
  begin
    Exit;
  end;
  Result := vol;
}
end;

procedure TBasicSound.SetVolume(Volume: Single);
begin
  if FChannel < 0 then Exit;

  if Volume < 0 then Volume := 0 else if Volume > 1 then Volume := 1;
  Mix_Volume(FChannel, trunc(Volume*MIX_MAX_VOLUME));
end;

//TODO
function TBasicSound.GetPan(): Single;
begin
  Result := 0.0;
  if FChannel < 0 then Exit;
{
  res := FMOD_Channel_GetPan(FChannel, pan);
  if res <> FMOD_OK then
  begin
    Exit;
  end;
  Result := pan;
}
end;

procedure TBasicSound.SetPan(Pan: Single);
var
  l, r: UInt8;
begin
  if FChannel < 0 then Exit;

  if Pan < -1.0 then Pan := -1.0 else if Pan > 1.0 then Pan := 1.0;
  Pan := Pan+1.0; // 0..2
  l := trunc(127.0*(2.0-Pan));
  r := trunc(127.0*Pan);
  Mix_SetPanning(FChannel, l, r);
end;

//TODO
function TBasicSound.IsMuted(): Boolean;
begin
  Result := False;
  if FChannel < 0 then Exit;
{
  res := FMOD_Channel_GetMute(FChannel, b);
  if res <> FMOD_OK then
  begin
    Exit;
  end;
  Result := b;
}
end;

//TODO
procedure TBasicSound.Mute(Enable: Boolean);
begin
  if FChannel < 0 then Exit;
{
  res := FMOD_Channel_SetMute(FChannel, Enable);
  if res <> FMOD_OK then
  begin
  end;
}
end;

//TODO
function TBasicSound.GetPosition(): DWORD;
begin
  Result := 0;
  if FChannel < 0 then Exit;
{
  res := FMOD_Channel_GetPosition(FChannel, FPosition, FMOD_TIMEUNIT_MS);
  if res <> FMOD_OK then
  begin
    Exit;
  end;
  Result := FPosition;
}
end;

//TODO
procedure TBasicSound.SetPosition(aPos: DWORD);
begin
  FPosition := aPos;
  if FChannel < 0 then Exit;
{
  res := FMOD_Channel_SetPosition(FChannel, FPosition, FMOD_TIMEUNIT_MS);
  if res <> FMOD_OK then
  begin
  end;
}
end;

//TODO
procedure TBasicSound.SetPriority(priority: Integer);
begin
{
  if (FChannel <> nil) and (FPriority <> priority) and
     (priority >= 0) and (priority <= 256) then
  begin
    FPriority := priority;
    res := FMOD_Channel_SetPriority(FChannel, priority);
    if res <> FMOD_OK then
    begin
    end;
  end;
}
end;

end.
