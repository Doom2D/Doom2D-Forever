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

interface

uses
  AL,
  {$IFDEF USE_MEMPOOL}mempool,{$ENDIF}
  e_soundfile,
  e_log,
  SysUtils;

type
  TSoundID = PtrInt;

  TSoundRec = record
    Loader: TSoundLoader;
    alBuffer: ALuint;
    isMusic: Boolean;
    Loops: Boolean;
    nRefs: Integer;
  end;

  TBasicSound = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  private
    FSource: Integer;
    FOldGain: ALfloat;
    FMuted: Boolean;

    function InvalidSource(): Boolean; inline;

  protected
    FID: TSoundID;
    FMusic: Boolean;
    FPosition: DWORD;

    function RawPlay(Pan: Single; Volume: Single; aPos: DWORD): Boolean;

  public
    constructor Create();
    destructor Destroy(); override;
    procedure SetID(ID: TSoundID);
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
    procedure SetPriority(priority: Cardinal);
  end;

const
  NO_SOUND_ID = TSoundID(-1);

function e_InitSoundSystem(NoOutput: Boolean = False): Boolean;

function e_LoadSound(FileName: String; var ID: TSoundID; isMusic: Boolean;
  ForceNoLoop: Boolean = False): Boolean;
function e_LoadSoundMem(pData: Pointer; Length: Integer; var ID: TSoundID; isMusic: Boolean;
  ForceNoLoop: Boolean = False): Boolean;

function e_PlaySound(ID: TSoundID): Integer;
function e_PlaySoundPan(ID: TSoundID; Pan: Single): Integer;
function e_PlaySoundVolume(ID: TSoundID; Volume: Single): Integer;
function e_PlaySoundPanVolume(ID: TSoundID; Pan, Volume: Single): Integer;

procedure e_ModifyChannelsVolumes(SoundMod: Single; setMode: Boolean);
procedure e_MuteChannels(Enable: Boolean);
procedure e_StopChannels();

procedure e_DeleteSound(ID: TSoundID);
procedure e_RemoveAllSounds();
procedure e_ReleaseSoundSystem();
procedure e_SoundUpdate();

var
  e_SoundFormat: TSoundFormat;  // desired sound format
  e_SoundsArray: array of TSoundRec;
  e_ZeroPosition: array [0..2] of ALfloat;
  e_ALError: ALenum;
  e_SoundFont: String = 'data/banks/default.sf2';
  e_MusicLerp: Boolean = True;

implementation

uses
  g_options, utils;

const
  NUM_SOURCES = 255;  // + 1 stereo
  NUM_STREAM_BUFFERS = 8;
  STREAM_BUFSIZE = 8192;
  MUSIC_SOURCE = 0;

var
  SoundMuted: Boolean;
  CurStream: TSoundID = NO_SOUND_ID;
  alDevice: PALCdevice;
  alContext: PALCcontext;
  // sources for everything
  alSources: array [0..NUM_SOURCES] of ALuint;
  // last TBasicSound that has played on each source
  alOwners: array [0..NUM_SOURCES] of TBasicSound;
  // buffers for the music stream
  alStreamBufs: array [0..NUM_STREAM_BUFFERS-1] of ALuint;
  alStreamData: array [0..STREAM_BUFSIZE-1] of Byte;
  alStreamAvail: Integer = NUM_STREAM_BUFFERS;

{$IFNDEF OPENAL_SINGLETHREADED}
var
  StreamThread: TThreadID = NilThreadId;
  StreamThreadRunning: Boolean;
  StreamLock: TRTLCriticalSection;
  StreamBufTime: Integer = 10; // time to sleep between buffer checks

procedure UpdateStreamSource(Src: Integer); forward;

function StreamThreadProc(Param: Pointer): PtrInt;
begin
  while StreamThreadRunning do
  begin
    EnterCriticalSection(StreamLock);
    UpdateStreamSource(MUSIC_SOURCE);
    LeaveCriticalSection(StreamLock);
    Sleep(StreamBufTime);
  end;
  Result := 0;
end;
{$ENDIF}

function CheckALError(): Boolean;
begin
  e_ALError := alGetError();
  Result := e_ALError <> AL_NO_ERROR;
end;

function GetALError(): String;
begin
  Result := '';
  case e_ALError of
    AL_NO_ERROR:          Result := '';
    AL_INVALID_NAME:      Result := 'AL_INVALID_NAME';
    AL_INVALID_ENUM:      Result := 'AL_INVALID_ENUM';
    AL_INVALID_VALUE:     Result := 'AL_INVALID_VALUE';
    AL_INVALID_OPERATION: Result := 'AL_INVALID_OPERATION';
    AL_OUT_OF_MEMORY:     Result := 'AL_OUT_OF_MEMORY';
    else                  Result := Format('unknown error %x', [e_ALError]);
  end;
end;

function e_InitSoundSystem(NoOutput: Boolean): Boolean;
var
  alExt, alRend, alVendor, alVer: String;
  WantDev: String = '';
  WantAttrs: array [0..4] of ALCint = (
    ALC_STEREO_SOURCES, 1,
    ALC_MONO_SOURCES, NUM_SOURCES,
    0
  );
begin
  Result := False;

  WantDev := alcGetString(nil, ALC_DEVICE_SPECIFIER);
  e_LogWritefln('AL: available devices: %s', [WantDev]);

  // TODO: open a dummy device when NoOutput is true or something
  WantDev := alcGetString(nil, ALC_DEFAULT_DEVICE_SPECIFIER);
  e_LogWritefln('AL: trying to open device %s', [WantDev]);

  alDevice := alcOpenDevice(PChar(WantDev));
  if alDevice = nil then
  begin
    e_LogWritefln('AL: ERROR: could not open device %s: %s', [WantDev, GetALError()]);
    exit;
  end;

  alContext := alcCreateContext(alDevice, WantAttrs);
  if alContext = nil then
  begin
    e_LogWritefln('AL: ERROR: could not create context: %s', [GetALError()]);
    alcCloseDevice(alDevice);
    alDevice := nil;
    exit;
  end;

  alcMakeContextCurrent(alContext);

  // TODO: actually parse these from alc attributes or something
  e_SoundFormat.SampleRate := 48000;
  e_SoundFormat.SampleBits := 16;
  e_SoundFormat.Channels := 2;

  alVendor := alGetString(AL_VENDOR);
  alRend := alGetString(AL_RENDERER);
  alVer := alGetString(AL_VERSION);
  alExt := alGetString(AL_EXTENSIONS);

  e_LogWriteln('AL INFO:');
  e_LogWriteln('  Version:  '    + alVer);
  e_LogWriteln('  Vendor:   '    + alVendor);
  e_LogWriteln('  Renderer: '    + alRend);
  e_LogWriteln('  Device:   '    + WantDev);
  e_LogWriteln('  Sample rate: ' + IntToStr(e_SoundFormat.SampleRate));
  e_LogWriteln('  Extensions:');
  e_LogWriteln('    ' + alExt);

  ZeroMemory(@alSources[0], sizeof(alSources));
  ZeroMemory(@alOwners[0], sizeof(alOwners));
  ZeroMemory(@alStreamBufs[0], sizeof(alStreamBufs));
  ZeroMemory(@alStreamData[0], sizeof(alStreamData));
  CurStream := NO_SOUND_ID;

  alGetError(); // reset the goddamn error state
  alGenSources(1, @alSources[0]); // generate the music source
  if CheckALError() then
    e_LogWriteln('AL: ERROR: alGenSources() for music failed: ' + GetALError());

  alStreamAvail := 0;
  alGenBuffers(NUM_STREAM_BUFFERS, @alStreamBufs[0]); // generate buffers for the music stream
  if CheckALError() then
    e_LogWriteln('AL: ERROR: alGenSources() for music failed: ' + GetALError())
  else
    alStreamAvail := NUM_STREAM_BUFFERS;

  {$IFNDEF OPENAL_SINGLETHREADED}
  InitCriticalSection(StreamLock);
  StreamThreadRunning := True;
  StreamThread := BeginThread(Addr(StreamThreadProc));
  {$ENDIF}

  Result := True;
end;

function FindESound(): TSoundID;
var
  i: TSoundID;
begin
  for i := 0 to High(e_SoundsArray) do
    if (e_SoundsArray[i].alBuffer = 0) and (e_SoundsArray[i].Loader = nil) then
    begin
      Result := i;
      Exit;
    end;

  Result := High(e_SoundsArray) + 1;
  SetLength(e_SoundsArray, Length(e_SoundsArray) + 16);
end;

function GetALSoundFormat(Fmt: TSoundFormat): ALenum; inline;
begin
  if Fmt.Channels = 2 then
  begin
    if Fmt.SampleBits = 16 then
      Result := AL_FORMAT_STEREO16
    else
      Result := AL_FORMAT_STEREO8;
  end
  else
  begin
    if Fmt.SampleBits = 16 then
      Result := AL_FORMAT_MONO16
    else
      Result := AL_FORMAT_MONO8;
  end;
end;

function GetALSourceState(S: ALuint): ALint; inline;
begin
  alGetSourcei(S, AL_SOURCE_STATE, Result);
end;

function LoadEntireSound(var Snd: TSoundRec; Loader: TSoundLoader): Boolean;
var
  Frame, Data, NewData: Pointer;
  Rx, DataLen, OldLen: LongWord;
const
  CHUNK_SIZE = 65536 * 2 * 2;
begin
  Result := False;

  Frame := GetMem(CHUNK_SIZE);
  if Frame = nil then exit;

  Data := nil;
  DataLen := 0;

  repeat
    Rx := Loader.FillBuffer(Frame, CHUNK_SIZE);
    if Rx = 0 then break;

    OldLen := DataLen;
    DataLen := DataLen + Rx;
    NewData := ReAllocMem(Data, DataLen);
    if NewData = nil then
    begin
      FreeMem(Data);
      FreeMem(Frame);
      Exit;
    end;

    Data := NewData;
    Move(Frame^, (Data + OldLen)^, Rx);
  until Loader.Finished();

  FreeMem(Frame);

  alGenBuffers(1, Addr(Snd.alBuffer));
  if CheckALError() then
  begin
    e_LogWritefln('AL: Could not create AL buffer: %s', [GetALError()]);
    FreeMem(Data);
    exit;
  end;

  alBufferData(
    Snd.alBuffer,
    GetALSoundFormat(Loader.Format),
    Data,
    DataLen,
    Loader.Format.SampleRate
  );

  FreeMem(Data);

  if CheckALError() then
  begin
    e_LogWriteln('AL: Could not fill AL buffer: ' + GetALError());
    alDeleteBuffers(1, Addr(Snd.alBuffer));
    Snd.alBuffer := 0;
    exit;
  end;

  Result := True;
end;

function e_LoadSound(FileName: String; var ID: TSoundID; isMusic: Boolean;
  ForceNoLoop: Boolean): Boolean;
var
  find_id: TSoundID;
  Loader: TSoundLoader;
begin
  ID := NO_SOUND_ID;
  Result := False;

  find_id := FindESound();

  e_SoundsArray[find_id].Loader := nil;
  e_SoundsArray[find_id].isMusic := isMusic;
  e_SoundsArray[find_id].Loops := isMusic and not ForceNoLoop;
  e_SoundsArray[find_id].nRefs := 0;

  Loader := e_GetSoundLoader(FileName);
  if Loader = nil then
  begin
    e_LogWritefln('Could not find loader for sound `%s`', [FileName]);
    exit;
  end;

  if not Loader.Load(FileName, e_SoundsArray[find_id].Loops) then
  begin
    e_LogWritefln('Could not load sound `%s`', [FileName]);
    Loader.Destroy();
    exit;
  end;

  alGetError();  // reset error state, god damn it

  if not isMusic then
  begin
    if not LoadEntireSound(e_SoundsArray[find_id], Loader) then
      e_LogWritefln('AL: Could not buffer sound effect `%s`', [FileName]);
    Loader.Destroy();  // don't need this anymore
  end
  else
  begin
    e_SoundsArray[find_id].alBuffer := 0;
    e_SoundsArray[find_id].Loader := Loader;
  end;

  ID := find_id;
  Result := True;
end;

function e_LoadSoundMem(pData: Pointer; Length: Integer; var ID: TSoundID; isMusic: Boolean;
  ForceNoLoop: Boolean): Boolean;
var
  find_id: TSoundID;
  Loader: TSoundLoader;
begin
  ID := NO_SOUND_ID;
  Result := False;

  find_id := FindESound();

  e_SoundsArray[find_id].Loader := nil;
  e_SoundsArray[find_id].isMusic := isMusic;
  e_SoundsArray[find_id].Loops := isMusic and not ForceNoLoop;
  e_SoundsArray[find_id].nRefs := 0;

  Loader := e_GetSoundLoader(pData, LongWord(Length));
  if Loader = nil then
  begin
    e_LogWritefln('Could not find loader for sound `%p`', [pData]);
    exit;
  end;

  if not Loader.Load(pData, LongWord(Length), e_SoundsArray[find_id].Loops) then
  begin
    e_LogWritefln('Could not load sound `%p`', [pData]);
    Loader.Destroy();
    exit;
  end;

  alGetError();  // reset error state, god damn it

  if not isMusic then
  begin
    if not LoadEntireSound(e_SoundsArray[find_id], Loader) then
      e_LogWritefln('AL: Could not buffer sound effect `%p`', [pData]);
    Loader.Destroy();  // don't need this anymore
  end
  else
  begin
    e_SoundsArray[find_id].alBuffer := 0;
    e_SoundsArray[find_id].Loader := Loader;
  end;

  // the calling side won't free this, the loader will get a copy, so fuck it
  FreeMem(pData);
  ID := find_id;
  Result := True;
end;

function FindSourceForSound(ID: TSoundID): Integer;
var
  S: Integer;
begin
  Result := -1;
  if (ID = NO_SOUND_ID) or (High(e_SoundsArray) < ID) then
    Exit;

  if e_SoundsArray[ID].Loader <> nil then
  begin
    // first source is for streaming sounds
    // it always exists
    alOwners[MUSIC_SOURCE] := nil;
    Result := MUSIC_SOURCE;
    exit;
  end;

  for S := 1 to High(alSources) do
    if alSources[S] = 0 then
    begin
      alOwners[S] := nil; // TBasicSounds will set this if needed
      Result := S;
      break;
    end;

  if Result = -1 then Exit; // no voices left

  alGetError(); // reset error state
  alGenSources(1, @alSources[Result]);
  if CheckALError() then
  begin
    e_LogWriteln('AL: FindSourceForSound(): alGenSources() failed: ' + GetALError());
    Result := -1;
  end;
end;

procedure AssignSound(ID: TSoundID; Src: ALuint); inline;
var
  S: ALint;
begin
  alGetError(); // reset error state

  if e_SoundsArray[ID].Loader <> nil then
  begin
    // this is a stream
    {$IFNDEF OPENAL_SINGLETHREADED}
    // lock the stream so the stream thread doesn't shit itself
    EnterCriticalSection(StreamLock);
    // number of stereo samples / samplerate =
    // time until buffer runs out
    StreamBufTime :=
      (STREAM_BUFSIZE div (2 * e_SoundsArray[ID].Loader.Format.SampleBits div 8)) div
      (e_SoundsArray[ID].Loader.Format.SampleRate div 1000) - 1;
    if StreamBufTime < 1 then StreamBufTime := 1;
    {$ENDIF}
    // reset position
    e_SoundsArray[ID].Loader.Restart();
    if CurStream <> ID then // changing streams
    begin
      alSourceStop(Src); // this should mark all buffers as processed
      alGetSourcei(Src, AL_BUFFERS_PROCESSED, S);
      // unqueue all buffers
      if S > 0 then
      begin
        alSourceUnqueueBuffers(Src, S, @alStreamBufs[alStreamAvail]);
        alStreamAvail := NUM_STREAM_BUFFERS;
      end;
    end;
    // this shit is playing now
    CurStream := ID;
    {$IFNDEF OPENAL_SINGLETHREADED}
    // unlock the stream
    LeaveCriticalSection(StreamLock);
    {$ENDIF}
  end
  else
  begin
    // this is a full chunk, assign local buffer
    alSourcei(Src, AL_BUFFER, e_SoundsArray[ID].alBuffer);
    // these can loop
    if e_SoundsArray[ID].Loops
      then alSourcei(Src, AL_LOOPING, AL_TRUE)
      else alSourcei(Src, AL_LOOPING, AL_FALSE);
  end;

  alSourcei(Src, AL_SOURCE_RELATIVE, AL_TRUE);
end;

function e_PlaySound(ID: TSoundID): Integer;
begin
  Result := FindSourceForSound(ID);
  if Result >= 0 then
  begin
    AssignSound(ID, alSources[Result]);
    alSourcef(alSources[Result], AL_GAIN, 1);
    alSourcefv(alSources[Result], AL_POSITION, e_ZeroPosition);
    alSourcePlay(alSources[Result]);
  end;
end;

function e_PlaySoundPan(ID: TSoundID; Pan: Single): Integer;
var
  Pos: array [0..2] of ALfloat;
begin
  Result := FindSourceForSound(ID);
  if Result >= 0 then
  begin
    Pos[0] := Pan;
    Pos[1] := 0;
    Pos[2] := 0;
    AssignSound(ID, alSources[Result]);
    alSourcef(alSources[Result], AL_GAIN, 1);
    alSourcefv(alSources[Result], AL_POSITION, Pos);
    alSourcePlay(alSources[Result]);
  end;
end;

function e_PlaySoundVolume(ID: TSoundID; Volume: Single): Integer;
begin
  Result := FindSourceForSound(ID);
  if Result >= 0 then
  begin
    AssignSound(ID, alSources[Result]);
    alSourcef(alSources[Result], AL_GAIN, Volume);
    alSourcefv(alSources[Result], AL_POSITION, e_ZeroPosition);
    alSourcePlay(alSources[Result]);
  end;
end;

function e_PlaySoundPanVolume(ID: TSoundID; Pan, Volume: Single): Integer;
var
  Pos: array [0..2] of ALfloat;
begin
  Result := FindSourceForSound(ID);
  if Result >= 0 then
  begin
    Pos[0] := Pan;
    Pos[1] := 0;
    Pos[2] := 0;
    AssignSound(ID, alSources[Result]);
    alSourcefv(alSources[Result], AL_POSITION, Pos);
    alSourcef(alSources[Result], AL_GAIN, Volume);
    alSourcePlay(alSources[Result]);
  end;
end;

procedure e_DeleteSound(ID: TSoundID);
begin
  if (ID = NO_SOUND_ID) or (High(e_SoundsArray) < ID) then
    Exit;

  if e_SoundsArray[ID].alBuffer <> 0 then
  begin
    alDeleteBuffers(1, Addr(e_SoundsArray[ID].alBuffer));
    e_SoundsArray[ID].alBuffer := 0;
  end;

  if e_SoundsArray[ID].Loader <> nil then
  begin
    e_SoundsArray[ID].Loader.Destroy();
    e_SoundsArray[ID].Loader := nil;
    if ID = CurStream then
      CurStream := NO_SOUND_ID;
  end;
end;

procedure e_ModifyChannelsVolumes(SoundMod: Single; setMode: Boolean);
var
  S: Integer;
  V: ALfloat;
begin
  // TODO: replace manual volume calculations everywhere with
  //       alListenerf(AL_GAIN) or something
  if setMode then
  begin
    for S := 1 to High(alSources) do
      if alSources[S] <> 0 then
        alSourcef(alSources[S], AL_GAIN, SoundMod)
  end
  else
  begin
    for S := 1 to High(alSources) do
      if alSources[S] <> 0 then
      begin
        alGetSourcef(alSources[S], AL_GAIN, V);
        alSourcef(alSources[S], AL_GAIN, V * SoundMod);
      end;
  end;
end;

procedure e_MuteChannels(Enable: Boolean);
begin
  if Enable = SoundMuted then
    Exit;

  SoundMuted := Enable;
end;

procedure e_StopChannels();
var
  S: Integer;
begin
  alGetError(); // reset error state
  for S := Low(alSources) to High(alSources) do
    if (alSources[S] <> 0) and (GetALSourceState(alSources[S]) = AL_PLAYING) then
    begin
      alSourceStop(alSources[S]);
      alDeleteSources(1, @alSources[S]);
      alSources[S] := 0;
    end;
end;

procedure e_RemoveAllSounds();
var
  i: Integer;
begin
  for i := 0 to High(e_SoundsArray) do
    e_DeleteSound(i);
  SetLength(e_SoundsArray, 0);
  CurStream := NO_SOUND_ID;
end;

procedure e_ReleaseSoundSystem();
begin
  {$IFNDEF OPENAL_SINGLETHREADED}
  if StreamThread <> NilThreadId then
  begin
    StreamThreadRunning := False;
    WaitForThreadTerminate(StreamThread, 66666);
    StreamThread := NilThreadId;
    DoneCriticalSection(StreamLock);
  end;
  {$ENDIF}

  e_RemoveAllSounds();

  alcMakeContextCurrent(nil);
  alcDestroyContext(alContext);
  alcCloseDevice(alDevice);

  alDevice := nil;
  alContext := nil;
end;

procedure UpdateStreamSource(Src: Integer);
var
  OutLen: LongWord;
  Buf: ALuint;
  S: Integer;
begin
  if alSources[Src] = 0 then Exit;

  alGetError(); // reset error state

  alGetSourcei(alSources[Src], AL_BUFFERS_PROCESSED, S);
  // unqueue processed buffers
  if S > 0 then
  begin
    alSourceUnqueueBuffers(alSources[Src], S, @alStreamBufs[alStreamAvail]);
    alStreamAvail := alStreamAvail + S;
  end;

  alGetError(); // reset error state

  if (alStreamAvail > 0) and (CurStream <> NO_SOUND_ID) then
  begin
    // some buffers have freed up, advance stream playback
    OutLen := e_SoundsArray[CurStream].Loader.FillBuffer(@alStreamData[0], STREAM_BUFSIZE);
    if OutLen = 0 then Exit; // ran out of stream
    Buf := alStreamBufs[alStreamAvail-1];
    Dec(alStreamAvail);
    // upload
    alBufferData(
      Buf,
      GetALSoundFormat(e_SoundsArray[CurStream].Loader.Format),
      @alStreamData[0],
      OutLen,
      e_SoundsArray[CurStream].Loader.Format.SampleRate
    );
    // attach
    alSourceQueueBuffers(alSources[Src], 1, @Buf);
    // restart if needed
    S := GetALSourceState(alSources[Src]);
    if (S = AL_STOPPED) or (S = AL_INITIAL) then
      alSourcePlay(alSources[Src]);
  end;
end;

procedure e_SoundUpdate();
var
  S: Integer;
begin
  alGetError(); // reset error state

  // clear out all stopped sources
  for S := 1 to High(alSources) do
    if (alSources[S] <> 0) and (GetALSourceState(alSources[S]) = AL_STOPPED) then
    begin
      alDeleteSources(1, @alSources[S]);
      alSources[S] := 0;
      alOwners[S] := nil;
    end;

  {$IFDEF OPENAL_SINGLETHREADED}
  // update the stream sources
  UpdateStreamSource(MUSIC_SOURCE);
  {$ENDIF}
end;

{ TBasicSound: }

constructor TBasicSound.Create();
begin
  FID := NO_SOUND_ID;
  FMusic := False;
  FSource := -1;
  FPosition := 0;
  FMuted := False;
  FOldGain := 1;
end;

destructor TBasicSound.Destroy();
begin
  FreeSound();
  inherited;
end;

function TBasicSound.InvalidSource(): Boolean; inline;
begin
  Result := (FSource < 0) or (alSources[FSource] = 0) or (alOwners[FSource] <> self);
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
begin
  Result := False;
  if FID = NO_SOUND_ID then Exit;

  if e_SoundsArray[FID].nRefs >= gMaxSimSounds then
  begin
    Result := True;
    Exit;
  end;

  FSource := e_PlaySoundPanVolume(FID, Pan, Volume);
  if FSource >= 0 then
  begin
    alOwners[FSource] := self;
    Result := True;
  end;
end;

procedure TBasicSound.SetID(ID: TSoundID);
begin
  FreeSound();

  if (ID = NO_SOUND_ID) or (High(e_SoundsArray) < ID) then
    Exit;

  FID := ID;
  FMusic := e_SoundsArray[ID].isMusic;
end;

function TBasicSound.IsPlaying(): Boolean;
begin
  Result := False;
  if InvalidSource() then
    Exit;
  Result := GetALSourceState(alSources[FSource]) = AL_PLAYING;
end;

procedure TBasicSound.Stop();
begin
  if FID = CurStream then
    CurStream := NO_SOUND_ID;
  if InvalidSource() then
    Exit;
  GetPosition();
  alSourceStop(alSources[FSource]);
end;

function TBasicSound.IsPaused(): Boolean;
begin
  Result := False;
  if InvalidSource() then
    Exit;
  Result := GetALSourceState(alSources[FSource]) = AL_PAUSED;
end;

procedure TBasicSound.Pause(Enable: Boolean);
begin
  if InvalidSource() then
    Exit;
  if Enable then
    alSourcePause(alSources[FSource])
  else
    alSourcePlay(alSources[FSource]);
end;

function TBasicSound.GetVolume(): Single;
begin
  Result := 0.0;
  if InvalidSource() then
    Exit;
  alGetSourcef(alSources[FSource], AL_GAIN, Result);
end;

procedure TBasicSound.SetVolume(Volume: Single);
begin
  if InvalidSource() then
    Exit;
  alSourcef(alSources[FSource], AL_GAIN, Volume);
end;

function TBasicSound.GetPan(): Single;
var
  Pos: array [0..2] of ALfloat = (0, 0, 0);
begin
  Result := 0.0;
  if InvalidSource() then
    Exit;
  alGetSourcefv(alSources[FSource], AL_POSITION, Pos);
  Result := Pos[0];
end;

procedure TBasicSound.SetPan(Pan: Single);
var
  Pos: array [0..2] of ALfloat;
begin
  if InvalidSource() then
    Exit;
  Pos[0] := Pan;
  Pos[1] := 0;
  Pos[2] := 0;
  alSourcefv(alSources[FSource], AL_POSITION, Pos);
end;

function TBasicSound.IsMuted(): Boolean;
begin
  if InvalidSource() then
    Result := False
  else
    Result := FMuted;
end;

procedure TBasicSound.Mute(Enable: Boolean);
begin
  if InvalidSource() then
    Exit;
   if Enable then
   begin
     FOldGain := GetVolume();
     FMuted := True;
     SetVolume(0);
   end
   else if FMuted then
   begin
     FMuted := False;
     SetVolume(FOldGain);
   end;
end;

function TBasicSound.GetPosition(): DWORD;
var
  Bytes: ALint;
begin
  Result := 0;
  if InvalidSource() then
    Exit;
  alGetSourcei(alSources[FSource], AL_BYTE_OFFSET, Bytes);
  FPosition := Bytes;
  Result := FPosition;
end;

procedure TBasicSound.SetPosition(aPos: DWORD);
begin
  FPosition := aPos;
  if InvalidSource() then
    Exit;
  alSourcei(alSources[FSource], AL_BYTE_OFFSET, aPos);
end;

procedure TBasicSound.SetPriority(priority: Cardinal);
begin
end;

end.
