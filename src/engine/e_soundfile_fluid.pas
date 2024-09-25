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

{$INCLUDE ../shared/a_modes.inc}
unit e_soundfile_fluid;

interface

uses e_soundfile, fluidsynth;

type
  // a midi loader that uses fluidsynth

  TFluidLoader = class (TSoundLoader)
  public
    destructor Destroy(); override;
    function Load(Data: Pointer; Len: LongWord; Loop: Boolean): Boolean; override; overload;
    function Load(FName: String; Loop: Boolean): Boolean; override; overload;
    function Finished(): Boolean; override;
    function Restart(): Boolean; override;
    function FillBuffer(Buf: Pointer; Len: LongWord): LongWord; override;

  private
    FSynth: pfluid_synth_t;
    FPlayer: pfluid_player_t;
  end;

  TFluidLoaderFactory = class (TSoundLoaderFactory)
  public
    function MatchHeader(Data: Pointer; Len: LongWord): Boolean; override;
    function MatchExtension(FName: String): Boolean; override;
    function GetLoader(): TSoundLoader; override;
  end;

const
  DEFAULT_SOUNDFONT = 'data/soundfont.sf2';

implementation

uses sysutils, utils, e_sound, e_log, ctypes{$IFDEF WINDOWS}, windirs{$ENDIF};

var
  FluidSettings: pfluid_settings_t;

function FindDefaultSoundfont(): String;
{$IFDEF WINDOWS}
var
  SfNames: array [0..1] of String = (
    // creative soundfonts
    'ct4mgm.sf2',
    'ct2mgm.sf2'
    // gm.dls unsupported
  );
  I: Integer;
  SysDir, S: String;
begin
  SysDir := GetWindowsSpecialDir(CSIDL_SYSTEM, False);
  for I := Low(SfNames) to High(SfNames) do
  begin
    S := SysDir + SfNames[I];
    if FileExists(S) then
    begin
      e_LogWriteln('FluidSynth: Found system soundfont ' + S);
      Result := S;
      exit;
    end;
  end;
  Result := DEFAULT_SOUNDFONT;
end;
{$ELSE}
begin
  Result := DEFAULT_SOUNDFONT;
end;
{$ENDIF}

(* TFluidLoaderFactory *)

function TFluidLoaderFactory.MatchHeader(Data: Pointer; Len: LongWord): Boolean;
var
  P: PLongWord;
const
  MIDIHDR = $6468544D; // 'MThd'
begin
  Result := False;
  if Len < 14 then Exit; // the header is at least 4+4+6 bytes
  P := Data;
  Result := ((P+0)^ = MIDIHDR) and ((P+1)^ <> 0); // header length is not 0
end;

function TFluidLoaderFactory.MatchExtension(FName: String): Boolean;
var
  Ext: String;
begin
  Ext := GetFilenameExt(FName);
  Result := (Ext = '.mid') or (Ext = '.midi');
end;

function TFluidLoaderFactory.GetLoader(): TSoundLoader;
begin
  if e_SoundFont = '' then e_SoundFont := FindDefaultSoundfont();
  Result := TFluidLoader.Create();
end;

(* TFluidLoader *)

destructor TFluidLoader.Destroy();
begin
  delete_fluid_player(FPlayer);  // will call fluid_player_stop() if needed
  delete_fluid_synth(FSynth);
  inherited;
end;

function TFluidLoader.Load(Data: Pointer; Len: LongWord; Loop: Boolean): Boolean;
var
  Ret: cint;
begin
  Result := False;

  try
    FSynth := new_fluid_synth(FluidSettings);
    if FSynth = nil then
      raise Exception.Create('new_fluid_synth failed');
    Ret := fluid_synth_sfload(FSynth, PChar(e_SoundFont), 1);
    if Ret = FLUID_FAILED then
      raise Exception.Create('fluid_synth_sfload failed');
    FPlayer := new_fluid_player(FSynth);
    if FPlayer = nil then
      raise Exception.Create('new_fluid_player failed');
    Ret := fluid_player_add_mem(FPlayer, Data, Len);
    if Ret = FLUID_FAILED then
      raise Exception.Create('fluid_player_add failed');
    fluid_player_play(FPlayer);
  except
    if ExceptObject is Exception then
      e_LogWriteln('FluidSynth: Load(Data) failed: ' + Exception(ExceptObject).Message);

    delete_fluid_player(FPlayer);
    delete_fluid_synth(FSynth);
    FPlayer := nil;
    FSynth := nil;
    Exit;
  end;

  if Loop then
    fluid_player_set_loop(FPlayer, -1);

  FFormat.SampleRate := 44100;
  FFormat.SampleBits := 16;
  FFormat.Channels := 2;
  FStreaming := True;

  Result := True;
end;

function TFluidLoader.Load(FName: String; Loop: Boolean): Boolean;
var
  Ret: cint;
begin
  Result := False;

  try
    FSynth := new_fluid_synth(FluidSettings);
    if FSynth = nil then
      raise Exception.Create('new_fluid_synth failed');
    Ret := fluid_synth_sfload(FSynth, PChar(e_SoundFont), 1);
    if Ret = FLUID_FAILED then
      raise Exception.Create('fluid_synth_sfload failed');
    FPlayer := new_fluid_player(FSynth);
    if FPlayer = nil then
      raise Exception.Create('new_fluid_player failed');
    Ret := fluid_player_add(FPlayer, PChar(FName));
    if Ret = FLUID_FAILED then
      raise Exception.Create('fluid_player_add failed');
    fluid_player_play(FPlayer);
  except
    if ExceptObject is Exception then
      e_LogWriteln('FluidSynth: Load(Data) failed: ' + Exception(ExceptObject).Message);

    delete_fluid_player(FPlayer);
    delete_fluid_synth(FSynth);
    FPlayer := nil;
    FSynth := nil;
    Exit;
  end;

  if Loop then
    fluid_player_set_loop(FPlayer, -1);

  FFormat.SampleRate := 44100;
  FFormat.SampleBits := 16;
  FFormat.Channels := 2;
  FStreaming := True;

  Result := True;
end;

function TFluidLoader.Finished(): Boolean;
begin
  Result := fluid_player_get_status(FPlayer) = FLUID_PLAYER_DONE;
end;

function TFluidLoader.Restart(): Boolean;
begin
  Result := False;
  // fluid_player_seek() is only supported in full 2.x.x, and I ain't compiling that shit
  // if (FSynth <> nil) and (FPlayer <> nil) then
  // begin
  //   fluid_synth_system_reset(FSynth);
  //   fluid_player_seek(FPlayer, 0);
  //   fluid_player_play(FPlayer);
  //   Result := True;
  // end;
end;

function TFluidLoader.FillBuffer(Buf: Pointer; Len: LongWord): LongWord;
var
  Ret: cint;
begin
  Result := 0;
  if (FSynth = nil) or (FPlayer = nil) then Exit;
  Ret := fluid_synth_write_s16(FSynth, Len div 4, Buf, 0, 2, Buf, 1, 2);
  if Ret = FLUID_OK then Result := Len;
end;

initialization
  FluidSettings := new_fluid_settings();
  if FluidSettings <> nil then
  begin
    fluid_settings_setint(FluidSettings, PChar('synth.midi-channels'), 16);
    fluid_settings_setint(FluidSettings, PChar('synth.cpu-cores'), 1);
    fluid_settings_setnum(FluidSettings, PChar('synth.sample-rate'), 44100);
    fluid_settings_setnum(FluidSettings, PChar('synth.gain'), 1);
    fluid_settings_setint(FluidSettings, PChar('synth.reverb.active'), 0);
    fluid_settings_setint(FluidSettings, PChar('synth.chorus.active'), 0);
    fluid_settings_setstr(FluidSettings, PChar('player.timing-source'), PChar('sample'));
    e_AddSoundLoader(TFluidLoaderFactory.Create());
  end;

finalization
  if FluidSettings <> nil then
    delete_fluid_settings(FluidSettings);

end.

