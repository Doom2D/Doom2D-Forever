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
unit e_soundfile_xmp;

interface

uses e_soundfile, XMP;

type
  // a module loader that uses libxmp-lite

  TXMPLoader = class (TSoundLoader)
  public
    function Load(Data: Pointer; Len: LongWord; Loop: Boolean): Boolean; override; overload;
    function Load(FName: string; Loop: Boolean): Boolean; override; overload;
    function Finished(): Boolean; override;
    function Restart(): Boolean; override;
    function FillBuffer(Buf: Pointer; Len: LongWord): LongWord; override;
    procedure Free(); override;
  private
    FXMP: xmp_context;
    FLoaded: Boolean;
    FLooping: Boolean;
    FFinished: Boolean;
  end;

  TXMPLoaderFactory = class (TSoundLoaderFactory)
  public
    function MatchHeader(Data: Pointer; Len: LongWord): Boolean; override;
    function MatchExtension(FName: string): Boolean; override;
    function GetLoader(): TSoundLoader; override;
  end;

implementation

uses sysutils, utils, math, e_sound, e_log;

(* TXMPLoaderFactory *)

function TXMPLoaderFactory.MatchHeader(Data: Pointer; Len: LongWord): Boolean;
var
  Ctx: xmp_context;
  Err: LongInt;
begin
  // HACK: these fine gentlemen didn't provide us with a xmp_test_module_from_memory()
  //       so just load the module and unload it

  Result := False;

  Ctx := xmp_create_context();
  Err := xmp_load_module_from_memory(Ctx, Data, Len);

  if Err = 0 then
  begin
    xmp_release_module(Ctx);
    Result := True;
  end;

  xmp_free_context(Ctx);
end;

function TXMPLoaderFactory.MatchExtension(FName: string): Boolean;
var
  Ext: string;
begin
  Ext := GetFilenameExt(FName);
  Result := (Ext = '.it') or (Ext = '.xm') or (Ext = '.mod') or (Ext = '.s3m');
end;

function TXMPLoaderFactory.GetLoader(): TSoundLoader;
begin
  Result := TXMPLoader.Create();
end;

(* TXMPLoader *)

function TXMPLoader.Load(Data: Pointer; Len: LongWord; Loop: Boolean): Boolean;
var
  Err: LongInt;
  Interp: LongInt;
begin
  Result := False;

  FLoaded := False;
  FXMP := xmp_create_context();
  if FXMP = nil then Exit;

  try
    Err := xmp_load_module_from_memory(FXMP, Data, Len);
    if Err <> 0 then
      raise Exception.Create('xmp_load_module_from_memory failed');

    if xmp_start_player(FXMP, 48000, 0) <> 0 then
      raise Exception.Create('xmp_start_player failed');

    if e_MusicLerp then Interp := XMP_INTERP_LINEAR
    else Interp := XMP_INTERP_NEAREST;
    xmp_set_player(FXMP, XMP_PLAYER_INTERP, Interp);

    FFormat.SampleRate := 48000;
    FFormat.SampleBits := 16;
    FFormat.Channels := 2;

    FStreaming := True; // modules are always streaming
    FLoaded := True;
    FLooping := Loop;
    FFinished := False;
    Result := True;
  except
    on E: Exception do
    begin
      e_LogWriteln('TXMPLoader.Load() error: ' + E.Message);
      if Err = 0 then xmp_release_module(FXMP);
      xmp_free_context(FXMP);
      FXMP := nil;
    end;
  end;
end;

function TXMPLoader.Load(FName: string; Loop: Boolean): Boolean;
var
  Err: LongInt;
  Interp: LongInt;
begin
  Result := False;

  FLoaded := False;
  FXMP := xmp_create_context();
  if FXMP = nil then Exit;

  try
    Err := xmp_load_module(FXMP, PChar(FName));
    if Err <> 0 then
      raise Exception.Create('xmp_load_module failed');

    if xmp_start_player(FXMP, 48000, 0) <> 0 then
      raise Exception.Create('xmp_start_player failed');

    if e_MusicLerp then Interp := XMP_INTERP_LINEAR
    else Interp := XMP_INTERP_NEAREST;
    xmp_set_player(FXMP, XMP_PLAYER_INTERP, Interp);

    FFormat.SampleRate := 48000;
    FFormat.SampleBits := 16;
    FFormat.Channels := 2;

    FStreaming := True; // modules are always streaming
    FLooping := Loop;
    FLoaded := True;
    FFinished := False;
    Result := True;
  except
    on E: Exception do
    begin
      e_LogWritefln('TXMPLoader.Load(%s) error: %s', [FName, E.Message]);
      if Err = 0 then xmp_release_module(FXMP);
      xmp_free_context(FXMP);
      FXMP := nil;
    end;
  end;
end;

function TXMPLoader.Finished(): Boolean;
begin
  Result := FFinished;
end;

function TXMPLoader.Restart(): Boolean;
begin
  Result := False;
  if FXMP = nil then Exit;
  Result := True;
  FFinished := False;
  xmp_restart_module(FXMP);
end;

function TXMPLoader.FillBuffer(Buf: Pointer; Len: LongWord): LongWord;
var
  Ret: LongInt;
begin
  Result := 0;
  if FXMP = nil then Exit;

  Ret := xmp_play_buffer(FXMP, Buf, Len, IfThen(FLooping, 0, 1));

  if Ret = 0 then
    Result := Len
  else if (Ret = -XMP_END) and not FLooping then
    FFinished := True;
end;

procedure TXMPLoader.Free();
begin
  if FXMP <> nil then
  begin
    if FLoaded then
    begin
      xmp_end_player(FXMP);
      xmp_release_module(FXMP);
    end;
    xmp_free_context(FXMP);
    FXMP := nil;
  end;
  FLoaded := False;
  FLooping := False;
  FFinished := False;
end;

initialization
  e_AddSoundLoader(TXMPLoaderFactory.Create());
end.
