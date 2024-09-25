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

{$IFDEF USE_MEMPOOL}
uses mempool;
{$ENDIF}

type
  TSoundID = PtrInt;

  TSoundRec = record
    nRefs: Integer;
  end;

  TBasicSound = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  protected
    FMusic: Boolean;
    FPosition: DWORD;
    function RawPlay(Pan: Single; Volume: Single; aPos: DWORD): Boolean;
  public
    procedure SetID (ID: TSoundID);
    procedure FreeSound ();
    function IsPlaying (): Boolean;
    procedure Stop ();
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
    procedure SetPriority (priority: Cardinal);
  end;

const
  NO_SOUND_ID = TSoundID(-1);

function e_InitSoundSystem (NoOutput: Boolean = False): Boolean;

function e_LoadSound (FileName: String; var ID: TSoundID; isMusic: Boolean;
  ForceNoLoop: Boolean = False): Boolean;
function e_LoadSoundMem (pData: Pointer; Length: Integer; var ID: TSoundID; isMusic: Boolean;
  ForceNoLoop: Boolean = False): Boolean;

function e_PlaySound (ID: TSoundID): Integer;
function e_PlaySoundPan (ID: TSoundID; Pan: Single): Integer;
function e_PlaySoundVolume (ID: TSoundID; Volume: Single): Integer;
function e_PlaySoundPanVolume (ID: TSoundID; Pan, Volume: Single): Integer;

procedure e_ModifyChannelsVolumes (SoundMod: Single; setMode: Boolean);
procedure e_MuteChannels (Enable: Boolean);
procedure e_StopChannels ();

procedure e_DeleteSound (ID: TSoundID);
procedure e_RemoveAllSounds ();
procedure e_ReleaseSoundSystem ();
procedure e_SoundUpdate ();

var
  e_SoundsArray: array of TSoundRec;

implementation

function e_InitSoundSystem (NoOutput: Boolean): Boolean;
begin
  Result := False;
end;

function e_LoadSound (FileName: String; var ID: TSoundID; isMusic: Boolean;
  ForceNoLoop: Boolean): Boolean;
begin
  ID := NO_SOUND_ID;
  Result := True;
end;

function e_LoadSoundMem (pData: Pointer; Length: Integer; var ID: TSoundID; isMusic: Boolean;
  ForceNoLoop: Boolean): Boolean;
begin
  FreeMem(pData);  // API assumes ownership transition of this buffer, so this is fine.
  ID := NO_SOUND_ID;
  Result := True;
end;

function e_PlaySound (ID: TSoundID): Integer;
begin
  Result := -1;
end;

function e_PlaySoundPan (ID: TSoundID; Pan: Single): Integer;
begin
  Result := -1;
end;

function e_PlaySoundVolume (ID: TSoundID; Volume: Single): Integer;
begin
  Result := -1;
end;

function e_PlaySoundPanVolume (ID: TSoundID; Pan, Volume: Single): Integer;
begin
  Result := -1;
end;

procedure e_DeleteSound (ID: TSoundID);
begin
end;

procedure e_ModifyChannelsVolumes (SoundMod: Single; setMode: Boolean);
begin
end;

procedure e_MuteChannels (Enable: Boolean);
begin
end;

procedure e_StopChannels ();
begin
end;

procedure e_RemoveAllSounds ();
begin
end;

procedure e_ReleaseSoundSystem ();
begin
end;

procedure e_SoundUpdate ();
begin
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TBasicSound
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TBasicSound.FreeSound ();
begin
end;

function TBasicSound.RawPlay (Pan: Single; Volume: Single; aPos: DWORD): Boolean;
begin
  Result := False;
end;

procedure TBasicSound.SetID (ID: TSoundID);
begin
end;

function TBasicSound.IsPlaying (): Boolean;
begin
  Result := False;
end;

procedure TBasicSound.Stop ();
begin
end;

function TBasicSound.IsPaused (): Boolean;
begin
  Result := False;
end;

procedure TBasicSound.Pause (Enable: Boolean);
begin
end;

function TBasicSound.GetVolume (): Single;
begin
  Result := 0.0;
end;

procedure TBasicSound.SetVolume (Volume: Single);
begin
end;

function TBasicSound.GetPan (): Single;
begin
  Result := 1.0;
end;

procedure TBasicSound.SetPan (Pan: Single);
begin
end;

function TBasicSound.IsMuted (): Boolean;
begin
  Result := True;
end;

procedure TBasicSound.Mute (Enable: Boolean);
begin
end;

function TBasicSound.GetPosition (): DWORD;
begin
  Result := 0;
end;

procedure TBasicSound.SetPosition (aPos: DWORD);
begin
end;

procedure TBasicSound.SetPriority (priority: Cardinal);
begin
end;

end.
