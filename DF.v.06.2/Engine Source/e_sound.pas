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
  TSound = record
   Data: Pointer;
   SoundSample: PFSoundSample;
  end;

  TMusic = record
   Data: Pointer;
   MusicModule: PFMusicModule;
   SoundStream: PFSoundStream;
  end;

function e_InitSound(Freq: Word): Boolean;
function e_LoadSample(FileName: string; var ID: DWORD): Boolean;
function e_LoadSampleMem(pData: Pointer; Length: Integer; var ID: DWORD): Boolean;
function e_PlaySample(ID: DWORD; Pan, Volume: Byte): Integer;
function e_IsPlayingSample(ID: DWORD; Channel: Integer): Boolean;
procedure e_DeleteSample(ID: DWORD);
procedure e_RemoveAllSamples();
function e_LoadSong(FileName: string; var ID: DWORD): Boolean;
function e_LoadSongMem(pData: Pointer; Length: Integer; var ID: DWORD): Boolean;
procedure e_PlaySong(ID: DWORD; Volume: Byte);
procedure e_DeleteSong(ID: DWORD);
procedure e_StopSong(ID: DWORD);
procedure e_SetSongVolume(ID: DWORD; Volume: Integer);
procedure e_RemoveAllSongs();
procedure e_ReleaseSoundEngine();

var
  e_SamplesArray: array of TSound = nil;
  e_SongsArray: array of TMusic = nil;

implementation

function e_InitSound(Freq: Word): Boolean;
begin
Result := False;

FSOUND_SetOutput(FSOUND_OUTPUT_DSOUND);
FSOUND_SetDriver(0);
FSOUND_SetMixer(FSOUND_MIXER_QUALITY_AUTODETECT);

if not FSOUND_Init(Freq, 64, 0) then
begin
 e_WriteLog('Error initializing FMOD !', MSG_FATALERROR);
 e_WriteLog(FMOD_ErrorString(FSOUND_GetError()), MSG_FATALERROR);
 FSOUND_Close();
 Exit;
end;

case FSOUND_GetOutput() of
 FSOUND_OUTPUT_NOSOUND: e_WriteLog('FSOUND Output Method: FSOUND_OUTPUT_NOSOUND', MSG_NOTIFY);
 FSOUND_OUTPUT_WINMM: e_WriteLog('FSOUND Output Method: FSOUND_OUTPUT_WINMM', MSG_NOTIFY);
 FSOUND_OUTPUT_DSOUND: e_WriteLog('FSOUND Output Method: FSOUND_OUTPUT_DSOUND', MSG_NOTIFY);
 FSOUND_OUTPUT_A3D: e_WriteLog('FSOUND Output Method: FSOUND_OUTPUT_A3D', MSG_NOTIFY);
end;

case FSOUND_GetMixer() of
 FSOUND_MIXER_BLENDMODE: e_WriteLog('FSOUND Mixer: FSOUND_MIXER_BLENDMODE', MSG_NOTIFY);
 FSOUND_MIXER_MMXP5: e_WriteLog('FSOUND Mixer: FSOUND_MIXER_MMXP5', MSG_NOTIFY);
 FSOUND_MIXER_MMXP6: e_WriteLog('FSOUND Mixer: FSOUND_MIXER_MMXP6', MSG_NOTIFY);
 FSOUND_MIXER_QUALITY_FPU: e_WriteLog('FSOUND Mixer: FSOUND_MIXER_QUALITY_FPU', MSG_NOTIFY);
 FSOUND_MIXER_QUALITY_MMXP5: e_WriteLog('FSOUND Mixer: FSOUND_MIXER_QUALITY_MMXP5', MSG_NOTIFY);
 FSOUND_MIXER_QUALITY_MMXP6: e_WriteLog('FSOUND Mixer: FSOUND_MIXER_QUALITY_MMXP6', MSG_NOTIFY);
end;

e_WriteLog('FSOUND Driver: '+FSOUND_GetDriverName(FSOUND_GetDriver()), MSG_NOTIFY);

Result := True;
end;

function FindSample(): DWORD;
var
  i: integer;
begin
 if e_SamplesArray <> nil then
 for i := 0 to High(e_SamplesArray) do if e_SamplesArray[i].SoundSample = nil then
 begin
  Result := i;
  Exit;
 end;

 if e_SamplesArray = nil then
 begin
  SetLength(e_SamplesArray, 16);
  Result := 0;
 end
  else
 begin
  Result := High(e_SamplesArray) + 1;
  SetLength(e_SamplesArray, Length(e_SamplesArray) + 16);
 end;
end;

function e_LoadSample(FileName: String; var ID: DWORD): Boolean;
var
  find_id: DWORD;
begin
 Result := False;

 e_WriteLog('Loading sample '+FileName+'...', MSG_NOTIFY);

 find_id := FindSample;

 e_SamplesArray[find_id].Data := nil;

 e_SamplesArray[find_id].SoundSample := FSOUND_Sample_Load(FSOUND_FREE, PChar(FileName), FSOUND_HW2D, 0, 0);
 if e_SamplesArray[find_id].SoundSample = nil then
 begin
  e_WriteLog('Error loading sample '''+FileName+''' !', MSG_WARNING);
  e_WriteLog(FMOD_ErrorString(FSOUND_GetError()), MSG_WARNING);
  Exit;
 end;

 ID := find_id;

 Result := True;
end;

function e_PlaySample(ID: DWORD; Pan, Volume: Byte): Integer;
var
  ChannelID: Integer;
begin
 ChannelID := FSOUND_PlaySound(FSOUND_FREE, e_SamplesArray[ID].SoundSample);
 FSOUND_SetPan(ChannelID, Pan);
 FSOUND_SetVolume(ChannelID, Volume);
 Result := ChannelID;
end;

function e_IsPlayingSample(ID: DWORD; Channel: Integer): Boolean;
begin
 Result := e_SamplesArray[ID].SoundSample = FSOUND_GetCurrentSample(Channel); 
end;

procedure e_DeleteSample(ID: DWORD);
begin
 if e_SamplesArray[ID].SoundSample = nil then Exit;

 if e_SamplesArray[ID].Data <> nil then FreeMem(e_SamplesArray[ID].Data);
 FSOUND_Sample_Free(e_SamplesArray[ID].SoundSample);

 e_SamplesArray[ID].SoundSample := nil;
 e_SamplesArray[ID].Data := nil;
end;

procedure e_RemoveAllSamples();
var
  i: Integer;
begin
 for i := 0 to High(e_SamplesArray) do
  if e_SamplesArray[i].SoundSample <> nil then e_DeleteSample(i);

 e_SamplesArray := nil;
end;

function FindSong(): DWORD;
var
  i: integer;
begin
 if e_SongsArray <> nil then
 for i := 0 to High(e_SongsArray) do
  if (e_SongsArray[i].MusicModule = nil) and (e_SongsArray[i].SoundStream = nil) then
 begin
  Result := i;
  Exit;
 end;

 if e_SongsArray = nil then
 begin
  SetLength(e_SongsArray, 8);
  Result := 0;
 end
  else
 begin
  Result := High(e_SongsArray) + 1;
  SetLength(e_SongsArray, Length(e_SongsArray) + 8);
 end;
end;

function e_LoadSong(FileName: String; var ID: DWORD): Boolean;
var
  find_id: DWORD;
begin
Result := False;

e_WriteLog(Format('Loading song "%s"...', [FileName]), MSG_NOTIFY);

find_id := FindSong;

e_SongsArray[find_id].MusicModule := FMUSIC_LoadSong(PChar(FileName));
if e_SongsArray[find_id].MusicModule = nil then
begin
 e_SongsArray[find_id].SoundStream := FSOUND_Stream_Open(PChar(FileName),
  FSOUND_NORMAL or FSOUND_2D or FSOUND_MPEGACCURATE or FSOUND_NONBLOCKING, 0, 0);

 if e_SongsArray[find_id].SoundStream = nil then
 begin
  e_WriteLog(Format('Error loading song "%s" !', [FileName]), MSG_WARNING);
  e_WriteLog(FMOD_ErrorString(FSOUND_GetError()), MSG_WARNING);
  Exit;
 end;
end;

ID := find_id;

Result := True;
end;

function e_LoadSongMem(pData: Pointer; Length: Integer; var ID: DWORD): Boolean;
var
  find_id: DWORD;
  a: Integer;
begin
 Result := False;

 e_WriteLog('Loading song from $'+IntToHex(Integer(pData), 8), MSG_NOTIFY);

 find_id := FindSong;

 e_SongsArray[find_id].MusicModule := FMUSIC_LoadSongEx(pData, 0, Length, FSOUND_LOADMEMORY, a, 0);
 if e_SongsArray[find_id].MusicModule = nil then
 begin
  e_SongsArray[find_id].Data := pData;

  e_SongsArray[find_id].SoundStream := FSOUND_Stream_Open(pData, FSOUND_LOADMEMORY, 0, Length);

  if e_SongsArray[find_id].SoundStream = nil then
  begin
   e_WriteLog('Error loading song from memory', MSG_WARNING);
   e_WriteLog(FMOD_ErrorString(FSOUND_GetError()), MSG_WARNING);
   Exit;
  end;

  Result := True;
 end else Result := True;

 ID := find_id;
end;

procedure e_PlaySong(ID: DWORD; Volume: Byte);
begin
 if e_SongsArray[ID].MusicModule <> nil then
 begin
  FMUSIC_PlaySong(e_SongsArray[ID].MusicModule);
  FMUSIC_SetMasterVolume(e_SongsArray[ID].MusicModule, Volume);
 end
  else if e_SongsArray[ID].SoundStream <> nil then
 begin
  FSOUND_Stream_SetMode(e_SongsArray[ID].SoundStream, FSOUND_LOOP_NORMAL);
	FSOUND_SetVolume(FSOUND_Stream_Play(FSOUND_FREE, e_SongsArray[ID].SoundStream), Volume);
 end;
end;

procedure e_DeleteSong(ID: DWORD);
begin
 if e_SongsArray[ID].MusicModule <> nil then FMUSIC_FreeSong(e_SongsArray[ID].MusicModule);
 if e_SongsArray[ID].SoundStream <> nil then
 begin
  FSOUND_Stream_Close(e_SongsArray[ID].SoundStream);
  if e_SongsArray[ID].Data <> nil then FreeMem(e_SongsArray[ID].Data);
 end;

 e_SongsArray[ID].MusicModule := nil;
 e_SongsArray[ID].SoundStream := nil;
end;                                                     

procedure e_StopSong(ID: DWORD);
begin
 if e_SongsArray[ID].MusicModule <> nil then FMUSIC_StopSong(e_SongsArray[ID].MusicModule);
 if e_SongsArray[ID].SoundStream <> nil then FSOUND_Stream_Stop(e_SongsArray[ID].SoundStream);
end;

procedure e_SetSongVolume(ID: DWORD; Volume: Integer);
begin
 if e_SongsArray[ID].MusicModule <> nil then FMUSIC_SetMasterVolume(e_SongsArray[ID].MusicModule, Volume); 
end;

function e_LoadSampleMem(pData: Pointer; Length: Integer; var ID: DWORD): Boolean;
var
  find_id: DWORD;
begin
 Result := False;

 e_WriteLog('Loading sample from $'+IntToHex(Integer(pData), 8), MSG_NOTIFY);

 find_id := FindSample;

 e_SamplesArray[find_id].SoundSample := FSOUND_Sample_Load(FSOUND_FREE, pData, FSOUND_LOADMEMORY, 0, Length);
 if e_SamplesArray[find_id].SoundSample = nil then
 begin
  e_WriteLog('Error loading sample from memory', MSG_WARNING);
  e_WriteLog(FMOD_ErrorString(FSOUND_GetError()), MSG_WARNING);
  Exit;
 end;

 e_SamplesArray[find_id].Data := pData;

 ID := find_id;

 Result := True;
end;

procedure e_RemoveAllSongs();
var
  i: Integer;
begin
 for i := 0 to High(e_SongsArray) do
  if (e_SongsArray[i].MusicModule <> nil) or (e_SongsArray[i].SoundStream <> nil) then
   e_DeleteSong(i);

 e_SongsArray := nil;
end;

procedure e_ReleaseSoundEngine();
begin
 e_RemoveAllSamples;
 e_RemoveAllSongs;
 {FSOUND_Close();}
end;

end.
