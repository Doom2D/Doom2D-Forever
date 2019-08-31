(* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
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
    function Load(Data: Pointer; Len: LongWord; SStreaming: Boolean): Boolean; override; overload;
    function Load(FName: string; SStreaming: Boolean): Boolean; override; overload;
    function SetPosition(Pos: LongWord): Boolean; override;
    function FillBuffer(Buf: Pointer; Len: LongWord): LongWord; override;
    function GetAll(var OutPtr: Pointer): LongWord; override;
    procedure Free(); override;
  private
    FXMP: xmp_context;
    FLoaded: Boolean;
  end;

  TXMPLoaderFactory = class (TSoundLoaderFactory)
    function MatchHeader(Data: Pointer; Len: LongWord): Boolean; override;
    function MatchExtension(FName: string): Boolean; override;
    function GetLoader(): TSoundLoader; override;
  end;

implementation

uses sysutils, utils, e_sound, e_log;

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

function TXMPLoader.Load(Data: Pointer; Len: LongWord; SStreaming: Boolean): Boolean;
var
  Err: LongInt;
begin
  Result := False;

  FLoaded := False;
  FXMP := xmp_create_context();
  if FXMP = nil then Exit;

  try
    Err := xmp_load_module_from_memory(FXMP, Data, Len);
    if Err <> 0 then
      raise Exception.Create('xmp_load_module_from_memory failed');

    if xmp_start_player(FXMP, e_SoundFormat.SampleRate, 0) <> 0 then
      raise Exception.Create('xmp_start_player failed');

    FFormat.SampleRate := e_SoundFormat.SampleRate;
    FFormat.SampleBits := 16;
    FFormat.Channels := 2;

    FStreaming := True; // modules are always streaming
    FLoaded := True;
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

function TXMPLoader.Load(FName: string; SStreaming: Boolean): Boolean;
var
  Err: LongInt;
begin
  Result := False;

  FLoaded := False;
  FXMP := xmp_create_context();
  if FXMP = nil then Exit;

  try
    Err := xmp_load_module(FXMP, PChar(FName));
    if Err <> 0 then
      raise Exception.Create('xmp_load_module failed');

    if xmp_start_player(FXMP, e_SoundFormat.SampleRate, 0) <> 0 then
      raise Exception.Create('xmp_start_player failed');

    FFormat.SampleRate := e_SoundFormat.SampleRate;
    FFormat.SampleBits := 16;
    FFormat.Channels := 2;

    FStreaming := True; // modules are always streaming
    FLoaded := True;
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

function TXMPLoader.SetPosition(Pos: LongWord): Boolean;
begin
  Result := False;
  if FXMP = nil then Exit;
  Result := xmp_set_position(FXMP, Pos) = 0;
end;

function TXMPLoader.FillBuffer(Buf: Pointer; Len: LongWord): LongWord;
begin
  Result := 0;
  if FXMP = nil then Exit;
  if xmp_play_buffer(FXMP, Buf, Len, 0) = 0 then
    Result := Len;
end;

function TXMPLoader.GetAll(var OutPtr: Pointer): LongWord;
begin
  Result := 0; // modules are always streaming, so this don't make sense
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
end;

initialization
  e_AddSoundLoader(TXMPLoaderFactory.Create());
end.
