unit f_addresource_sound;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, f_addresource,
  ExtCtrls, StdCtrls, spectrum, Buttons, ComCtrls, utils;

type
  TAddSoundForm = class (TAddResourceForm)
    pSpectrum: TPanel;
    bbPlay: TBitBtn;
    bbStop: TBitBtn;
    Timer: TTimer;
    bEmpty: TButton;

    procedure FormCreate(Sender: TObject);
    procedure bbPlayClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbStopClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bEmptyClick(Sender: TObject);

  private
    FSpectrum: TMiniSpectrum;
    FSetResource: String;
    FSoundEnabled: Boolean;

    procedure ShowSpectrum();
    function CreateSoundWAD(Resource: String): Boolean;

  public
    property SetResource: String read FSetResource write FSetResource;
  end;

var
  AddSoundForm: TAddSoundForm;

implementation

uses
  BinEditor, WADEDITOR, e_log, f_main, g_language, g_resources
{$IFNDEF NOSOUND}, fmod, fmodtypes, fmoderrors;{$ELSE};{$ENDIF}

{$R *.lfm}

{$IFDEF NOSOUND}
// fuck my life
const
  FMOD_OK = 0;

type
  FMOD_SYSTEM = Pointer;
  FMOD_CHANNEL = Pointer;
  FMOD_SOUND = Pointer;
  FMOD_CREATESOUNDEXINFO = Pointer;
  FMOD_RESULT = Integer;
{$ENDIF}

var
  F_System: FMOD_SYSTEM;
  SoundData: Pointer = nil;
  Sound: FMOD_SOUND = nil;
  Channel: FMOD_CHANNEL = nil;
  Playing: Boolean = False;

procedure TAddSoundForm.FormCreate(Sender: TObject);
var
  res: FMOD_RESULT;
  ver: Cardinal;

begin
  Inherited;

  res := FMOD_OK;
  FSoundEnabled := False;

{$IFNDEF NOSOUND}
  try
    res := FMOD_System_Create(F_System);
    if res <> FMOD_OK then
      raise Exception.Create('FMOD_System_Create failed!');

    res := FMOD_System_GetVersion(F_System, ver);
    if res <> FMOD_OK then
      raise Exception.Create('FMOD_System_GetVersion failed!');

    if ver < FMOD_VERSION then
      raise Exception.Create('FMOD version is too old! Need '+IntToStr(FMOD_VERSION));

    res := FMOD_System_SetOutput(F_System, FMOD_OUTPUTTYPE_WINMM);
    if res <> FMOD_OK then
      raise Exception.Create('FMOD_System_SetOutput failed!');

    res := FMOD_System_SetSoftwareFormat(F_System, 48000,
             FMOD_SOUND_FORMAT_PCM16, 0, 0, FMOD_DSP_RESAMPLER_LINEAR);
    if res <> FMOD_OK then
      raise Exception.Create('FMOD_System_SetSoftwareFormat failed!');

    res := FMOD_System_Init(F_System, 64, FMOD_INIT_NORMAL, nil);
    if res <> FMOD_OK then
      raise Exception.Create('FMOD_System_Init failed!');

    FSoundEnabled := True;
  except
    Application.MessageBox(PChar('Sound was disabled. Reason: ' + FMOD_ErrorString(res)), 'FMOD Error', MB_OK or MB_ICONWARNING);
  end;
{$ENDIF}

  FSpectrum := TMiniSpectrum.Create(pSpectrum);
  FSpectrum.Align := alClient;
  FSpectrum.Enabled := True;
  FSpectrum.Style := ssBlock;
end;

function TAddSoundForm.CreateSoundWAD(Resource: String): Boolean;
var
  FileName, SectionName, ResourceName: String;
  ResLength: Integer;
  sz: LongWord;
  soundExInfo: FMOD_CREATESOUNDEXINFO;
  res: FMOD_RESULT;

begin
  Result := False;
  SoundData := nil;
  Sound := nil;
  Channel := nil;

  if FSoundEnabled = False then
    Exit;

{$IFNDEF NOSOUND}
  g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);
  g_ReadResource(FileName, SectionName, ResourceName, SoundData, ResLength);

  if SoundData <> nil then
    begin
      sz := SizeOf(FMOD_CREATESOUNDEXINFO);
      FillMemory(@soundExInfo, sz, 0);
      soundExInfo.cbsize := sz;
      soundExInfo.length := LongWord(ResLength);

      res := FMOD_System_CreateStream(F_System, SoundData,
        FMOD_LOOP_OFF or FMOD_2D or FMOD_OPENMEMORY,
        @soundExInfo, Sound);
        
      if res <> FMOD_OK then
      begin
        e_WriteLog(Format('Error creating sound %s', [Resource]), MSG_WARNING);
        e_WriteLog(FMOD_ErrorString(res), MSG_WARNING);
        Exit;
      end;
    end
  else
    begin
      e_WriteLog(Format('Error loading sound %s', [Resource]), MSG_WARNING);
      //e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
      Exit;
    end;
 
  Result := True;
{$ENDIF}
end;

procedure TAddSoundForm.bbPlayClick(Sender: TObject);
var
  res: FMOD_RESULT;
  
begin
  Inherited;

  if FResourceSelected then
  begin
    if Playing then
      bbStop.Click();

    if FSoundEnabled = False then
      Exit;

    if not CreateSoundWAD(FFullResourceName) then
      Exit;

{$IFNDEF NOSOUND}
    res := FMOD_System_PlaySound(F_System, FMOD_CHANNEL_FREE,
             Sound, False, Channel);
    if res <> FMOD_OK then
    begin
      Application.MessageBox(FMOD_ErrorString(res),
                             PChar(MsgMsgSoundError),
                             MB_OK or MB_ICONHAND);
      Exit;
    end;

    Playing := True;

    FMOD_Channel_SetVolume(Channel, 1.0);

    FSpectrum.SetChannel(Channel);
{$ENDIF}
  end;
end;

procedure TAddSoundForm.ShowSpectrum;
begin
  if FSpectrum.Enabled then
    FSpectrum.Draw();
end;

procedure TAddSoundForm.TimerTimer(Sender: TObject);
var
  res: FMOD_RESULT;
  b: LongBool;

begin
  Inherited;

  if FSoundEnabled = False then
    Exit;

{$IFNDEF NOSOUND}
  FMOD_System_Update(F_System);
  
  ShowSpectrum();

  res := FMOD_Channel_IsPlaying(Channel, b);
  if (res <> FMOD_OK) or (not b) then
    bbStop.Click();
{$ENDIF}
end;

procedure TAddSoundForm.FormDestroy(Sender: TObject);
var
  res: FMOD_RESULT;

begin
  Inherited;

  FSpectrum.Free;

  if FSoundEnabled = False then
    Exit;

{$IFNDEF NOSOUND}
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
{$ENDIF}
end;

procedure Sound_StopRelease();
begin
  Playing := False;
{$IFNDEF NOSOUND}
  if Channel <> nil then
    FMOD_Channel_Stop(Channel);

  if Sound <> nil then
    FMOD_Sound_Release(Sound);
{$ENDIF}
  if SoundData <> nil then
    FreeMem(SoundData);

  SoundData := nil;
  Sound := nil;
  Channel := nil;
end;

procedure TAddSoundForm.FormClose(Sender: TObject;
                                  var Action: TCloseAction);
begin
  Inherited;

  Timer.Enabled := False;

  FSpectrum.SetChannel(nil);
  Sound_StopRelease();
end;

procedure TAddSoundForm.bbStopClick(Sender: TObject);
begin
  Inherited;

  FSpectrum.SetChannel(nil);
  Sound_StopRelease();
end;

procedure TAddSoundForm.FormActivate(Sender: TObject);
var
  FileName, SectionName, ResourceName: String;
  a: Integer;

begin
  Inherited;

  Timer.Enabled := True;

// Уже есть выбранный ресурс:
  if FSetResource <> '' then
  begin
    g_ProcessResourceStr(FSetResource, FileName, SectionName, ResourceName);

    if FileName = '' then
      FileName := MsgWadSpecialMap;

    if SectionName = '' then
      SectionName := '..';

  // WAD файл:
    a := cbWADList.Items.IndexOf(FileName);
    if a <> -1 then
    begin
      cbWADList.ItemIndex := a;
      cbWADList.OnChange(nil);
    end;

  // Секция:
    a := cbSectionsList.Items.IndexOf(SectionName);
    if a <> -1 then
    begin
      cbSectionsList.ItemIndex := a;
      cbSectionsList.OnChange(nil);
    end;

  // Ресурс:
    a := lbResourcesList.Items.IndexOf(ResourceName);
    if a <> -1 then
    begin
      lbResourcesList.ItemIndex := a;
      lbResourcesList.OnClick(nil);
    end;
  end;
end;

procedure TAddSoundForm.bOKClick(Sender: TObject);
begin
  inherited;

  ModalResult := mrOk;
end;

procedure TAddSoundForm.bEmptyClick(Sender: TObject);
begin
  FResourceName := '';
  ModalResult := mrOk;
end;

end.
