unit g_sound;

interface

uses Windows;

type
  TSound = class(TObject)
   private
    FCurrentChannel: Integer;
    FID: DWORD;
    FLoop: Boolean;
    procedure FreeSound();
   public
    constructor Create();
    destructor Destroy(); override;
    procedure SetID(ID: DWORD);
    function IsPlaying(): Boolean;
    procedure Play(Pan: Byte; Volume: Byte; force: Boolean = False); overload;
    procedure Play(X, Y: Integer; Volume: Byte; force: Boolean = False); overload;
    procedure Stop();
    property Loop: Boolean read FLoop write FLoop;
  end;

function g_Sound_PlayEx(SoundName: ShortString; Pan: Byte; Volume: Byte): Boolean;
function g_Sound_PlayExC(SoundName: ShortString; Pan: Byte; Volume: Byte): Integer;
function g_Sound_PlayExAt(SoundName: ShortString; Volume: Byte; X, Y: Integer): Boolean;
function g_Sound_Play(ID: DWORD; Pan: Byte; Volume: Byte): Integer;
function g_Sound_PlayAt(ID: DWORD; Volume: Byte; X, Y: Integer): Integer;
function g_Sound_CreateWAD(var ID: DWORD; Resource: string): Boolean;
function g_Sound_CreateFile(var ID: DWORD; FileName: string): Boolean;
function g_Sound_CreateWADEx(SoundName: ShortString; Resource: string): Boolean;
function g_Sound_CreateFileEx(SoundName: ShortString; FileName: string): Boolean;
procedure g_Sound_Delete(SoundName: ShortString);
function g_Sound_Exists(SoundName: string): Boolean;
function g_Sound_Get(var ID: DWORD; SoundName: ShortString): Boolean;

function g_Music_PlayEx(MusicName: ShortString; Volume: Byte): Boolean;
procedure g_Music_Play(ID: DWORD; Volume: Byte);
procedure g_Music_Stop(ID: DWORD);
procedure g_Music_StopEx(MusicName: ShortString);
function g_Music_CreateWAD(var ID: DWORD; Resource: string): Boolean;
function g_Music_CreateFile(var ID: DWORD; FileName: string): Boolean;
function g_Music_CreateWADEx(MusicName: ShortString; Resource: string): Boolean;
function g_Music_CreateFileEx(MusicName: ShortString; FileName: string): Boolean;
procedure g_Music_Delete(MusicName: ShortString);
function g_Music_Get(var ID: DWORD; MusicName: ShortString): Boolean;

implementation

uses e_sound, e_log, SysUtils, g_console, g_options, WADEDITOR, g_game, g_basic,
  g_items, g_map, fmod, Math;

type
  TGameSound = record
    SoundName: ShortString;
    ID:        DWORD;
  end;

  TGameMusic = record
    MusicName: ShortString;
    ID:        DWORD;
  end;

var
  SoundArray: array of TGameSound;
  MusicArray: array of TGameMusic;

function FindSound: DWORD;
var
  i: integer;
begin
 if SoundArray <> nil then
 for i := 0 to High(SoundArray) do
  if SoundArray[i].SoundName = '' then
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

function g_Sound_Play(ID: DWORD; Pan: Byte; Volume: Byte): Integer;
begin
 Result := e_PlaySample(ID, Pan, Round(Volume*gSoundLevel/255));
end;

function g_Sound_PlayAt(ID: DWORD; Volume: Byte; X, Y: Integer): Integer;
var
  l1, l2, d1, d2, a: Integer;
  c: Boolean;
begin
 Result := 0;

 l1 := gMaxDist;
 l2 := gMaxDist;

 d1 := 127;

 c := gPlayerScreenSize.X >= gMapInfo.Width;

 if X > gMapInfo.Width then X := gMapInfo.Width
  else if X < 0 then X := 0;

 if Y > gMapInfo.Height then Y := gMapInfo.Height
  else if Y < 0 then Y := 0;

 if gPlayer1 <> nil then
 begin
  l1 := PointToRect(X, Y, gPlayer1ScreenCoord.X, gPlayer1ScreenCoord.Y,
                    gPlayerScreenSize.X, gPlayerScreenSize.Y);

  if c then d1 := 127
  else if (X >= gPlayer1ScreenCoord.X) and (X <= gPlayer1ScreenCoord.X+gPlayerScreenSize.X) then d1 := 127
  else if X < gPlayer1ScreenCoord.X then d1 := Round(127*X/gPlayer1ScreenCoord.X)
  else
  begin
   a := gPlayer1ScreenCoord.X+gPlayerScreenSize.X;
   d1 := 127+Round(127*(X-a)/(gMapInfo.Width-a));
  end;
 end;

 d2 := d1;

 if gPlayer2 <> nil then
 begin
  l2 := PointToRect(X, Y, gPlayer2ScreenCoord.X, gPlayer2ScreenCoord.Y,
                    gPlayerScreenSize.X, gPlayerScreenSize.Y);

  if c then d2 := 127
  else if (X >= gPlayer2ScreenCoord.X) and (X <= gPlayer2ScreenCoord.X+gPlayerScreenSize.X) then d2 := 127
  else if X < gPlayer2ScreenCoord.X then d2 := Round(127*X/gPlayer2ScreenCoord.X)
  else
  begin
   a := gPlayer2ScreenCoord.X+gPlayerScreenSize.X;
   d2 := 127+Round(127*(X-a)/(gMapInfo.Width-a));
  end;
 end;

 d1 := d1+((d2-d1) div 2);

 if l2 < l1 then l1 := l2;
 l1 := l1*2;

 if l1 >= gMaxDist then Exit
  else Volume := Round(Volume*(1-l1/gMaxDist));

 Result := g_Sound_Play(ID, d1, Volume);
end;

function g_Sound_PlayEx(SoundName: ShortString; Pan: Byte; Volume: Byte): Boolean;
var
  a: DWORD;
begin
 Result := False;
 if SoundArray = nil then Exit;

 for a := 0 to High(SoundArray) do
  if SoundArray[a].SoundName = SoundName then
  begin
   e_PlaySample(SoundArray[a].ID, Pan, Round(Volume*gSoundLevel/255));
   Result := True;
   Exit;
  end;

 g_Console_Add('Sound '+SoundName+' not found');
end;

function g_Sound_PlayExC(SoundName: ShortString; Pan: Byte; Volume: Byte): Integer;
var
  a: DWORD;
begin
 Result := -1;
 if SoundArray = nil then Exit;

 for a := 0 to High(SoundArray) do
  if SoundArray[a].SoundName = SoundName then
  begin
   Result := e_PlaySample(SoundArray[a].ID, Pan, Round(Volume*gSoundLevel/255));
   Exit;
  end;

 g_Console_Add('Sound '+SoundName+' not found');
end;

function g_Sound_PlayExAt(SoundName: ShortString; Volume: Byte; X, Y: Integer): Boolean;
var
  l1, l2, d1, d2, a: Integer;
  c: Boolean;
begin
 Result := False;

 l1 := gMaxDist;
 l2 := gMaxDist;

 d1 := 127;

 c := gPlayerScreenSize.X >= gMapInfo.Width;

 if X > gMapInfo.Width then X := gMapInfo.Width
  else if X < 0 then X := 0;

 if Y > gMapInfo.Height then Y := gMapInfo.Height
  else if Y < 0 then Y := 0;

 if gPlayer1 <> nil then
 begin
  l1 := PointToRect(X, Y, gPlayer1ScreenCoord.X, gPlayer1ScreenCoord.Y,
                    gPlayerScreenSize.X, gPlayerScreenSize.Y);

  if c then d1 := 127
  else if (X >= gPlayer1ScreenCoord.X) and (X <= gPlayer1ScreenCoord.X+gPlayerScreenSize.X) then d1 := 127
  else if X < gPlayer1ScreenCoord.X then d1 := Round(127*X/gPlayer1ScreenCoord.X)
  else
  begin
   a := gPlayer1ScreenCoord.X+gPlayerScreenSize.X;
   d1 := 127+Round(127*(X-a)/(gMapInfo.Width-a));
  end;
 end;

 d2 := d1;

 if gPlayer2 <> nil then
 begin
  l2 := PointToRect(X, Y, gPlayer2ScreenCoord.X, gPlayer2ScreenCoord.Y,
                    gPlayerScreenSize.X, gPlayerScreenSize.Y);

  if c then d2 := 127
  else if (X >= gPlayer2ScreenCoord.X) and (X <= gPlayer2ScreenCoord.X+gPlayerScreenSize.X) then d2 := 127
  else if X < gPlayer2ScreenCoord.X then d2 := Round(127*X/gPlayer2ScreenCoord.X)
  else
  begin
   a := gPlayer2ScreenCoord.X+gPlayerScreenSize.X;
   d2 := 127+Round(127*(X-a)/(gMapInfo.Width-a));
  end;
 end;

 d1 := d1+((d2-d1) div 2);

 if l2 < l1 then l1 := l2;
 l1 := l1*2;

 if l1 >= gMaxDist then Exit
  else Volume := Round(Volume*(1-l1/gMaxDist));

 Result := g_Sound_PlayEx(SoundName, d1, Volume);
end;

function g_Sound_CreateWAD(var ID: DWORD; Resource: string): Boolean;
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

 g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

 WAD := TWADEditor_1.Create;
 WAD.ReadFile(FileName);

 if WAD.GetResource(SectionName, ResourceName, SoundData, ResLength) then
  if e_LoadSampleMem(SoundData, ResLength, ID) then ok := True else FreeMem(SoundData);

 WAD.Destroy;

 if not ok then
 begin
  e_WriteLog(Format('Error loading sample %s', [Resource]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
  Exit;
 end;

 Result := True;
end;

function g_Sound_CreateFile(var ID: DWORD; FileName: string): Boolean;
begin
 Result := e_LoadSample(FileName, ID);
end;

function g_Sound_CreateWADEx(SoundName: ShortString; Resource: string): Boolean;
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

 g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

 find_id := FindSound;

 WAD := TWADEditor_1.Create;
 WAD.ReadFile(FileName);

 if WAD.GetResource(SectionName, ResourceName, SoundData, ResLength) then
 begin
  if e_LoadSampleMem(SoundData, ResLength, SoundArray[find_id].ID) then
  begin
   SoundArray[find_id].SoundName := SoundName;
   ok := True;
  end else FreeMem(SoundData)
 end else e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);

 WAD.Destroy;

 if not ok then
 begin
  e_WriteLog(Format('Error loading sample %s', [Resource]), MSG_WARNING);
  Exit;
 end;

 Result := True;
end;

function g_Sound_CreateFileEx(SoundName: ShortString; FileName: string): Boolean;
var
  find_id: DWORD;
begin
 Result := False;

 find_id := FindSound;

 if not e_LoadSample(FileName, SoundArray[find_id].ID) then Exit;

 SoundArray[find_id].SoundName := SoundName;

 Result := True;
end;

procedure g_Sound_Delete(SoundName: ShortString);
var
  a: DWORD;
begin
 if SoundArray = nil then Exit;

 for a := 0 to High(SoundArray) do
  if SoundArray[a].SoundName = SoundName then
  begin
   e_DeleteSample(SoundArray[a].ID);
   SoundArray[a].SoundName := '';
   SoundArray[a].ID := 0;
  end;
end;

function g_Sound_Exists(SoundName: string): Boolean;
var
  a: Integer;
begin
 Result := False;

 if SoundArray <> nil then
  for a := 0 to High(SoundArray) do
   if SoundArray[a].SoundName = SoundName then
   begin
    Result := True;
    Exit;
   end;
end;

function g_Sound_Get(var ID: DWORD; SoundName: ShortString): Boolean;
var
  a: Integer;
begin
 Result := False;

 if SoundArray <> nil then
  for a := 0 to High(SoundArray) do
   if SoundArray[a].SoundName = SoundName then
   begin
    ID := SoundArray[a].ID;
    Result := True;
    Exit;
   end;
end;

function FindMusic(): DWORD;
var
  i: integer;
begin
 if MusicArray <> nil then
 for i := 0 to High(MusicArray) do
  if MusicArray[i].MusicName = '' then
  begin
   Result := i;
   Exit;
  end;

 if MusicArray = nil then
 begin
  SetLength(MusicArray, 8);
  Result := 0;
 end
  else
 begin
  Result := High(MusicArray) + 1;
  SetLength(MusicArray, Length(MusicArray) + 8);
 end;
end;

procedure g_Music_Play(ID: DWORD; Volume: Byte);
begin
 e_PlaySong(ID, Round(Volume*gMusicLevel/255));
end;

procedure g_Music_Stop(ID: DWORD);
begin
 e_StopSong(ID);
end;

procedure g_Music_StopEx(MusicName: ShortString);
var
  a: DWORD;
begin
 if MusicArray = nil then Exit;

 for a := 0 to High(MusicArray) do
  if MusicArray[a].MusicName = MusicName then
  begin
   e_StopSong(MusicArray[a].ID);
   Exit;
  end;
end;

function g_Music_PlayEx(MusicName: ShortString; Volume: Byte): Boolean;
var
  a: DWORD;
begin
 Result := False;
 if MusicArray = nil then Exit;

 for a := 0 to High(MusicArray) do
  if MusicArray[a].MusicName = MusicName then
  begin
   Result := True;
   e_PlaySong(MusicArray[a].ID, Round(Volume*gMusicLevel/255));
   Exit;
  end;

 g_Console_Add('Music '+MusicName+' not found');
end;

function g_Music_CreateWAD(var ID: DWORD; Resource: string): Boolean;
var
  WAD: TWADEditor_1;
  FileName,
  SectionName,
  ResourceName: string;
  MusicData: Pointer;
  ResLength: Integer;
  ok: Boolean;
begin
 Result := False;

 ok := False;

 g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

 WAD := TWADEditor_1.Create;
 WAD.ReadFile(FileName);

 if WAD.GetResource(SectionName, ResourceName, MusicData, ResLength) then
  if e_LoadSongMem(MusicData, ResLength, ID) then ok := True else FreeMem(MusicData);

 WAD.Destroy;

 if not ok then
 begin
  e_WriteLog(Format('Error loading music %s', [Resource]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
  Exit;
 end;

 Result := True;
end;

function g_Music_CreateFile(var ID: DWORD; FileName: string): Boolean;
begin
 Result := e_LoadSong(FileName, ID); 
end;

function g_Music_CreateWADEx(MusicName: ShortString; Resource: string): Boolean;
var
  WAD: TWADEditor_1;
  FileName,
  SectionName,
  ResourceName: String;
  MusicData: Pointer;
  ResLength: Integer;
  find_id: DWORD;
  ok: Boolean;
begin
 Result := False;

 ok := False;

 g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

 find_id := FindMusic;

 WAD := TWADEditor_1.Create;
 WAD.ReadFile(FileName);

 if WAD.GetResource(SectionName, ResourceName, MusicData, ResLength) then
  if e_LoadSongMem(MusicData, ResLength, MusicArray[find_id].ID) then
  begin
   ok := True;
   MusicArray[find_id].MusicName := MusicName;
  end else FreeMem(MusicData);

 WAD.Destroy;

 if not ok then
 begin
  e_WriteLog(Format('Error loading music %s', [Resource]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
  Exit;
 end;

 Result := True;
end;

function g_Music_CreateFileEx(MusicName: ShortString; FileName: string): Boolean;
var
  find_id: DWORD;
begin
 Result := False;

 find_id := FindMusic;

 if not e_LoadSong(FileName, MusicArray[find_id].ID) then Exit;

 MusicArray[find_id].MusicName := MusicName;

 Result := True;
end;

procedure g_Music_Delete(MusicName: ShortString);
var
  a: DWORD;
begin
 if MusicArray = nil then Exit;
 if MusicName = '' then Exit;

 for a := 0 to High(MusicArray) do
  if MusicArray[a].MusicName = MusicName then
  begin
   e_DeleteSong(MusicArray[a].ID);
   MusicArray[a].MusicName := '';
   MusicArray[a].ID := 0;
  end;
end;

function g_Music_Get(var ID: DWORD; MusicName: ShortString): Boolean;
var
  a: Integer;
begin
 Result := False;

 if MusicArray <> nil then
  for a := 0 to High(MusicArray) do
   if MusicArray[a].MusicName = MusicName then
   begin
    ID := MusicArray[a].ID;
    Result := True;
    Exit;
   end;
end;

{ TSound }

constructor TSound.Create();
begin
 FID := DWORD(-1);
 FCurrentChannel := -1;
end;

destructor TSound.Destroy();
begin
 FreeSound();
end;


procedure TSound.SetID(ID: DWORD);
begin
 FreeSound();
 FID := ID;
end;

procedure TSound.FreeSound();
begin
 if FID = DWORD(-1) then Exit;

 FID := DWORD(-1);
 FCurrentChannel := -1;
end;

function TSound.IsPlaying(): Boolean;
begin
 if (FID = DWORD(-1)) or (FCurrentChannel = -1) then Result := False
  else Result := e_IsPlayingSample(FID, FCurrentChannel);
end;

procedure TSound.Play(Pan, Volume: Byte; force: Boolean);
begin
 if FID = DWORD(-1) then Exit;

 if force or not IsPlaying() then
  FCurrentChannel := e_PlaySample(FID, Pan, Round(Volume*gSoundLevel/255));
end;

procedure TSound.Play(X, Y: Integer; Volume: Byte; force: Boolean);
var
  l1, l2, d1, d2, a: Integer;
  c: Boolean;
begin
 if FID = DWORD(-1) then Exit;

 l1 := gMaxDist;
 l2 := gMaxDist;

 d1 := 127;

 c := gPlayerScreenSize.X >= gMapInfo.Width;

 if X > gMapInfo.Width then X := gMapInfo.Width
  else if X < 0 then X := 0;

 if Y > gMapInfo.Height then Y := gMapInfo.Height
  else if Y < 0 then Y := 0;

 if gPlayer1 <> nil then
 begin
  l1 := PointToRect(X, Y, gPlayer1ScreenCoord.X, gPlayer1ScreenCoord.Y,
                    gPlayerScreenSize.X, gPlayerScreenSize.Y);

  if c then d1 := 127
  else if (X >= gPlayer1ScreenCoord.X) and (X <= gPlayer1ScreenCoord.X+gPlayerScreenSize.X) then d1 := 127
  else if X < gPlayer1ScreenCoord.X then d1 := Round(127*X/gPlayer1ScreenCoord.X)
  else
  begin
   a := gPlayer1ScreenCoord.X+gPlayerScreenSize.X;
   d1 := 127+Round(127*(X-a)/(gMapInfo.Width-a));
  end;
 end;

 d2 := d1;

 if gPlayer2 <> nil then
 begin
  l2 := PointToRect(X, Y, gPlayer2ScreenCoord.X, gPlayer2ScreenCoord.Y,
                    gPlayerScreenSize.X, gPlayerScreenSize.Y);

  if c then d2 := 127
  else if (X >= gPlayer2ScreenCoord.X) and (X <= gPlayer2ScreenCoord.X+gPlayerScreenSize.X) then d2 := 127
  else if X < gPlayer2ScreenCoord.X then d2 := Round(127*X/gPlayer2ScreenCoord.X)
  else
  begin
   a := gPlayer2ScreenCoord.X+gPlayerScreenSize.X;
   d2 := 127+Round(127*(X-a)/(gMapInfo.Width-a));
  end;
 end;

 d1 := d1+((d2-d1) div 2);

 if l2 < l1 then l1 := l2;
 l1 := l1*2;

 if l1 >= gMaxDist then Exit
  else Volume := Round(Volume*(1-l1/gMaxDist)); { TODO 5 : EnsureRange() }

 FCurrentChannel := e_PlaySample(FID, d1, Round(Volume*gSoundLevel/255));
end;

procedure TSound.Stop();
begin
 if FID = DWORD(-1) then Exit;
 if FCurrentChannel = -1 then Exit;

 if IsPlaying() then
 begin
  FSOUND_StopSound(FCurrentChannel);
  FCurrentChannel := -1;
 end;
end;

end.
