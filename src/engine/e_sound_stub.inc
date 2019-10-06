(* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
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
    {$IFDEF USE_MEMPOOL}mempool,{$ENDIF}
    SysUtils;

  type
    TSoundRec = record
      nRefs: Integer;
    end;

    TBasicSound = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
    private
    protected
      FMusic: Boolean;
      FPosition: DWORD;
      function RawPlay(Pan: Single; Volume: Single; aPos: DWORD): Boolean;
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetID (ID: DWORD);
      procedure FreeSound;
      function IsPlaying (): Boolean;
      procedure Stop;
      function IsPaused (): Boolean;
      procedure Pause (Enable: Boolean);
      function GetVolume (): Single;
      procedure SetVolume (Volume: Single);
      function GetPan (): Single;
      procedure SetPan (Pan: Single);
      function IsMuted (): Boolean;
      procedure Mute (Enable: Boolean);
      function GetPosition (): DWORD;
      procedure SetPosition (aPos: DWORD);
      procedure SetPriority (priority: Integer);
    end;

  const
    NO_SOUND_ID = DWORD(-1);

  function e_InitSoundSystem (NoOutput: Boolean = False): Boolean;

  function e_LoadSound (FileName: string; var ID: DWORD; isMusic: Boolean; ForceNoLoop: Boolean = False): Boolean;
  function e_LoadSoundMem (pData: Pointer; Length: Integer; var ID: DWORD; isMusic: Boolean; ForceNoLoop: Boolean = False): Boolean;

  function e_PlaySound (ID: DWORD): Integer;
  function e_PlaySoundPan (ID: DWORD; Pan: Single): Integer;
  function e_PlaySoundVolume (ID: DWORD; Volume: Single): Integer;
  function e_PlaySoundPanVolume (ID: DWORD; Pan, Volume: Single): Integer;

  procedure e_ModifyChannelsVolumes (SoundMod: Single; setMode: Boolean);
  procedure e_MuteChannels (Enable: Boolean);
  procedure e_StopChannels;

  procedure e_DeleteSound (ID: DWORD);
  procedure e_RemoveAllSounds;
  procedure e_ReleaseSoundSystem;
  procedure e_SoundUpdate;

  var
    e_SoundsArray: array of TSoundRec = nil;

implementation

  function e_InitSoundSystem(NoOutput: Boolean = False): Boolean;
  begin
    result := false
  end;

  function e_LoadSound(FileName: String; var ID: DWORD; isMusic: Boolean; ForceNoLoop: Boolean = False): Boolean;
  begin
    ID := NO_SOUND_ID;
    result := true
  end;

  function e_LoadSoundMem(pData: Pointer; Length: Integer; var ID: DWORD; isMusic: Boolean; ForceNoLoop: Boolean = False): Boolean;
  begin
    ID := NO_SOUND_ID;
    result := true
  end;

  function e_PlaySound (ID: DWORD): Integer;
  begin
    Result := -1
  end;

  function e_PlaySoundPan(ID: DWORD; Pan: Single): Integer;
  begin
    result := -1
  end;

  function e_PlaySoundVolume(ID: DWORD; Volume: Single): Integer;
  begin
    result := -1
  end;

  function e_PlaySoundPanVolume(ID: DWORD; Pan, Volume: Single): Integer;
  begin
    result := -1
  end;

  procedure e_DeleteSound(ID: DWORD);
  begin
  end;

  procedure e_ModifyChannelsVolumes(SoundMod: Single; setMode: Boolean);
  begin
  end;

  procedure e_MuteChannels(Enable: Boolean);
  begin
  end;

  procedure e_StopChannels();
  begin
  end;

  procedure e_RemoveAllSounds();
  begin
  end;

  procedure e_ReleaseSoundSystem();
  begin
  end;

  procedure e_SoundUpdate();
  begin
  end;

  (* --------- TBasicSound --------- *)

  constructor TBasicSound.Create;
  begin
  end;

  destructor TBasicSound.Destroy;
  begin
    inherited;
  end;

  procedure TBasicSound.FreeSound;
  begin
  end;

  function TBasicSound.RawPlay (Pan: Single; Volume: Single; aPos: DWORD): Boolean;
  begin
    result := false
  end;

  procedure TBasicSound.SetID (ID: DWORD);
  begin
  end;

  function TBasicSound.IsPlaying (): Boolean;
  begin
    result := false
  end;

  procedure TBasicSound.Stop;
  begin
  end;

  function TBasicSound.IsPaused (): Boolean;
  begin
    result := false
  end;

  procedure TBasicSound.Pause (Enable: Boolean);
  begin
  end;

  function TBasicSound.GetVolume (): Single;
  begin
    result := 0.0
  end;

  procedure TBasicSound.SetVolume (Volume: Single);
  begin
  end;

  function TBasicSound.GetPan (): Single;
  begin
    result := 1.0
  end;

  procedure TBasicSound.SetPan (Pan: Single);
  begin
  end;

  function TBasicSound.IsMuted (): Boolean;
  begin
    result := false
  end;

  procedure TBasicSound.Mute (Enable: Boolean);
  begin
  end;

  function TBasicSound.GetPosition (): DWORD;
  begin
    result := 0
  end;

  procedure TBasicSound.SetPosition (aPos: DWORD);
  begin
  end;

  procedure TBasicSound.SetPriority(priority: Integer);
  begin
  end;

end.
