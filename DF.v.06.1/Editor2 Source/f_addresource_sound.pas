unit f_addresource_sound;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, f_addresource, ExtCtrls, StdCtrls, spectrum, Buttons, ComCtrls;

type
  TAddSoundForm = class(TAddResourceForm)
    pSpectrum: TPanel;
    bbPlay: TBitBtn;
    bbStop: TBitBtn;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure bbPlayClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbStopClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
   FSpectrum: TMiniSpectrum;
   FSetResource: string;
   procedure ShowSpectrum;
  public
   property SetResource: string read FSetResource write FSetResource;
  end;

var
  AddSoundForm: TAddSoundForm;

implementation

uses
  fmod, fmodtypes, fmoderrors, WADEDITOR, e_log, f_main;

{$R *.dfm}

var
  ID: DWORD = High(DWORD);
  Channel: Integer = -1;
  Module: PFMusicModule = nil;
  Stream: PFSoundStream = nil;
  Playing: Boolean = False;
  Data: Pointer = nil;

procedure TAddSoundForm.FormCreate(Sender: TObject);
begin
 inherited;

 try
  if not FSOUND_SetOutput(FSOUND_OUTPUT_WINMM) then
   raise Exception.Create('FSOUND_SetOutput failed');
  if not FSOUND_SetDriver(0) then
   raise Exception.Create('FSOUND_SetDriver failed');
  if not FSOUND_SetMixer(FSOUND_MIXER_QUALITY_FPU) then
   raise Exception.Create('FSOUND_SetMixer failed');
  if not FSOUND_SetHWND(Handle) then
   raise Exception.Create('FSOUND_SetHWND failed');
  except
   Application.MessageBox(FMOD_ErrorString(FSOUND_GetError), 'Initialization', MB_OK or MB_ICONHAND);
   raise;
  end;

  if not FSOUND_Init(44100, 128, 0) then
  begin
    Application.MessageBox(FMOD_ErrorString(FSOUND_GetError), 'FSOUND_Init', MB_OK or MB_ICONHAND);
    Halt;
  end;

  FSpectrum := TMiniSpectrum.Create(pSpectrum);
  FSpectrum.Align := alClient;
  FSpectrum.Enabled := True;
  FSpectrum.Style := ssBlock;
end;

function CreateSoundWAD(var Module: PFMusicModule; var Stream: PFSoundStream;
                        Resource: String): Pointer;
var
  WAD: TWADEditor_1;
  FileName,
  SectionName,
  ResourceName: string;
  SoundData: Pointer;
  ResLength, a: Integer;
begin
 Result := nil;
 
 g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

 WAD := TWADEditor_1.Create;
 WAD.ReadFile(FileName);

 if WAD.GetResource(SectionName, ResourceName, SoundData, ResLength) then
 begin
  Stream := nil;
  Module := FMUSIC_LoadSongEx(SoundData, 0, ResLength, FSOUND_LOADMEMORY, a, 0);
  if Module = nil then
   Stream := FSOUND_Stream_Open(SoundData, FSOUND_LOADMEMORY, 0, ResLength);

  Result := SoundData;
 end
  else
 begin
  e_WriteLog(Format('Error loading sample %s', [Resource]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
  WAD.Destroy;
  Exit;
 end;
 WAD.Destroy;
end;

procedure TAddSoundForm.bbPlayClick(Sender: TObject);
begin
 inherited;

 if FResourceSelected then
 begin
  if Playing then bbStop.Click;

  Data := CreateSoundWAD(Module, Stream, FFullResourceName);
  if Data = nil then Exit;

  if Module <> nil then
  begin
   Playing := FMUSIC_PlaySong(Module);
   if not Playing then
    Application.MessageBox(FMOD_ErrorString(FSOUND_GetError), 'Play song', MB_OK or MB_ICONHAND);
  end
   else if Stream <> nil then
  begin
   Channel := FSOUND_Stream_Play(FSOUND_FREE, Stream);
   Playing := Channel >= 0;
   if not Playing then
   begin
    Application.MessageBox(FMOD_ErrorString(FSOUND_GetError), 'Play stream', MB_OK or MB_ICONHAND);
   end
    else
   begin
    FSOUND_SetPan(Channel, FSOUND_STEREOPAN);
    FSOUND_SetVolume(Channel, 255);
   end;
  end;
 end;
end;

procedure TAddSoundForm.ShowSpectrum;
begin
 if FSpectrum.Enabled then FSpectrum.Draw;
end;

procedure TAddSoundForm.TimerTimer(Sender: TObject);
begin
 inherited;
 ShowSpectrum;
 if FMUSIC_IsFinished(Module) then bbStop.Click;
end;

procedure TAddSoundForm.FormDestroy(Sender: TObject);
begin
 inherited;
 FSpectrum.Free;
 FMOD_Unload;
end;

procedure TAddSoundForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 inherited;

 Timer.Enabled := False;

 FMUSIC_StopAllSongs();
 if Module <> nil then FMUSIC_FreeSong(Module);
 if Stream <> nil then
 begin
  FSOUND_Stream_Stop(Stream);
  FSOUND_Stream_Close(Stream);
 end;

 if Data <> nil then FreeMem(Data);
 Data := nil;

 Channel := -1;
 Module := nil;
 Stream := nil;
 Playing := False;
end;

procedure TAddSoundForm.bbStopClick(Sender: TObject);
begin
 inherited;

 if not FResourceSelected then Exit;

 if Module <> nil then FMUSIC_StopSong(Module)
  else if Stream <> nil then FSOUND_Stream_Stop(Stream);

 if Data <> nil then FreeMem(Data);
 Data := nil;

 Channel := -1;
 Module := nil;
 Stream := nil;
 Playing := False;
end;

procedure TAddSoundForm.FormActivate(Sender: TObject);
var
  FileName,
  SectionName,
  ResourceName: string;
  a: Integer;
begin
 inherited;

 Timer.Enabled := True;

 if FSetResource <> '' then
 begin
  g_ProcessResourceStr(FSetResource, FileName, SectionName, ResourceName);

  if FileName = '' then FileName := WAD_SPECIAL_MAP;
  if SectionName = '' then SectionName := '..';

  a := cbWADList.Items.IndexOf(FileName);
  if a <> -1 then
  begin
   cbWADList.ItemIndex := a;
   cbWADList.OnChange(nil);
  end;

  a := cbSectionsList.Items.IndexOf(SectionName);
  if a <> -1 then
  begin
   cbSectionsList.ItemIndex := a;
   cbSectionsList.OnChange(nil);
  end;

  a := lbResourcesList.Items.IndexOf(ResourceName);
  if a <> -1 then
  begin
   lbResourcesList.ItemIndex := a;
   lbResourcesList.OnClick(nil);
  end;
 end;
end;

end.
