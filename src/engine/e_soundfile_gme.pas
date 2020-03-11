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
{$INCLUDE ../shared/a_modes.inc}
unit e_soundfile_gme;

interface

uses e_soundfile, GME;

type
  // a module loader that uses libgme (the version from gzdoom)
  // TODO: play all tracks in the song file and not just 0

  TGMELoader = class (TSoundLoader)
  public
    function Load(Data: Pointer; Len: LongWord; Loop: Boolean): Boolean; override; overload;
    function Load(FName: string; Loop: Boolean): Boolean; override; overload;
    function Finished(): Boolean; override;
    function Restart(): Boolean; override;
    function FillBuffer(Buf: Pointer; Len: LongWord): LongWord; override;
    procedure Free(); override;
  private
    FEmu: pgme_music_emu;
    FLooping: Boolean;
    FTrack: LongInt;
    FInfo: pgme_info_t;

    function StartTrack(Track: LongInt): Boolean;
    function CalcTrackLength(): LongInt;
  end;

  TGMELoaderFactory = class (TSoundLoaderFactory)
  public
    function MatchHeader(Data: Pointer; Len: LongWord): Boolean; override;
    function MatchExtension(FName: string): Boolean; override;
    function GetLoader(): TSoundLoader; override;
  end;

implementation

uses sysutils, utils, math, e_sound, e_log, ctypes;

(* TGMELoaderFactory *)

function TGMELoaderFactory.MatchHeader(Data: Pointer; Len: LongWord): Boolean;
begin
  if (Data = nil) or (Len < 4) then exit(False);
  Result := ((gme_identify_header(Data))^ <> #0);
end;

function TGMELoaderFactory.MatchExtension(FName: string): Boolean;
begin
  Result := gme_identify_extension(PChar(FName)) <> nil;
end;

function TGMELoaderFactory.GetLoader(): TSoundLoader;
begin
  Result := TGMELoader.Create();
end;

(* TGMELoader *)

function TGMELoader.StartTrack(Track: LongInt): Boolean;
var
  Ret: gme_err_t;
begin
  Result := False;

  Ret := gme_track_info(FEmu, @FInfo, Track);
  if Ret <> nil then
  begin
    e_LogWritefln('GME: Error getting info for track %d: %s', [Track, string(Ret)]);
    exit;
  end;

  FTrack := Track;

  if FLooping then
    gme_set_fade(FEmu, -1)
  else
    gme_set_fade(FEmu, CalcTrackLength());

  gme_set_autoload_playback_limit(FEmu, 0);

  Ret := gme_start_track(FEmu, Track);
  // apparently this can happen
  if Ret <> nil then
  begin
    e_LogWritefln('GME: Could not start track %d: %s', [Track, string(Ret)]);
    exit;
  end;

  Result := True;
end;

function TGMELoader.Load(Data: Pointer; Len: LongWord; Loop: Boolean): Boolean;
var
  Ret: gme_err_t;
begin
  Result := False;

  Ret := gme_open_data(Data, clong(Len), @FEmu, 48000);
  if Ret <> nil then
  begin
    e_LogWritefln('GME: Error loading song from `%p`: %s', [Data, string(Ret)]);
    exit;
  end;

  FFormat.SampleRate := 48000;
  FFormat.SampleBits := 16;
  FFormat.Channels := 2;
  FStreaming := True; // modules are always streaming
  FLooping := Loop;

  Result := StartTrack(0);
  if not Result then Free();
end;

function TGMELoader.Load(FName: string; Loop: Boolean): Boolean;
var
  Ret: gme_err_t;
begin
  Result := False;

  Ret := gme_open_file(PChar(FName), @FEmu, 48000);
  if Ret <> nil then
  begin
    e_LogWritefln('GME: Error loading song from `%s`: %s', [FName, string(Ret)]);
    exit;
  end;

  FFormat.SampleRate := 48000;
  FFormat.SampleBits := 16;
  FFormat.Channels := 2;
  FStreaming := True; // modules are always streaming
  FLooping := Loop;

  Result := StartTrack(0);
  if not Result then Free();
end;

function TGMELoader.CalcTrackLength(): LongInt;
begin
  if FInfo = nil then
    Result := 150000
  else if FInfo.length > 0 then
    Result := FInfo.length
  else if FInfo.loop_length > 0 then
    Result := FInfo.intro_length + FInfo.loop_length * 2
  else
    Result := 150000;
end;

function TGMELoader.Finished(): Boolean;
begin
  if FEmu <> nil then
    Result := gme_track_ended(FEmu) <> 0
  else
    Result := False;
end;

function TGMELoader.Restart(): Boolean;
begin
  if FEmu = nil then
    Result := False
  else
    Result := StartTrack(FTrack);
end;

function TGMELoader.FillBuffer(Buf: Pointer; Len: LongWord): LongWord;
begin
  Result := 0;

  if FEmu = nil then Exit;

  if FLooping and (gme_track_ended(FEmu) <> 0) then
    StartTrack(FTrack);

  if gme_play(FEmu, Len div 2, PWord(Buf)) = nil then
    Result := Len
  else
    Result := 0;
end;

procedure TGMELoader.Free();
begin
  if FInfo <> nil then gme_free_info(FInfo);
  if FEmu <> nil then gme_delete(FEmu);
  FInfo := nil;
  FEmu := nil;
end;

initialization
  e_AddSoundLoader(TGMELoaderFactory.Create());
end.
