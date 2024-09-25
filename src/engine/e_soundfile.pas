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
unit e_soundfile;

interface

type
  TSoundFormat = record
    SampleBits: Integer;
    SampleRate: Integer;
    Channels: Integer;
  end;

  // each sound file format has its own loader factory and loader class,
  // each sound has its own loader instance for streaming purposes

  TSoundLoader = class
  protected
    FFormat: TSoundFormat;
    FStreaming: Boolean;

  public
    function Load(Data: Pointer; Len: LongWord; Loop: Boolean): Boolean; virtual; abstract; overload;
    function Load(FName: String; Loop: Boolean): Boolean; virtual; abstract; overload;

    function Finished(): Boolean; virtual; abstract;
    function Restart(): Boolean; virtual; abstract;
    function FillBuffer(Buf: Pointer; Len: LongWord): LongWord; virtual; abstract;

    property Format: TSoundFormat read FFormat;
    property Streaming: Boolean read FStreaming;
  end;

  TSoundLoaderFactory = class
  public
    function MatchHeader(Data: Pointer; Len: LongWord): Boolean; virtual; abstract;
    function MatchExtension(FName: String): Boolean; virtual; abstract;
    function GetLoader(): TSoundLoader; virtual; abstract;
  end;

function e_GetSoundLoader(Data: Pointer; Len: LongWord): TSoundLoader; overload;
function e_GetSoundLoader(FName: String): TSoundLoader; overload;

procedure e_AddSoundLoader(Loader: TSoundLoaderFactory);

implementation

var
  e_SoundLoaders: array of TSoundLoaderFactory;

function e_GetSoundLoader(FName: String): TSoundLoader; overload;
var
  I: Integer;
begin
  Result := nil;
  for I := Low(e_SoundLoaders) to High(e_SoundLoaders) do
    if e_SoundLoaders[I].MatchExtension(FName) then
    begin
      Result := e_SoundLoaders[I].GetLoader();
      break;
    end;
end;

function e_GetSoundLoader(Data: Pointer; Len: LongWord): TSoundLoader; overload;
var
  I: Integer;
begin
  Result := nil;
  for I := Low(e_SoundLoaders) to High(e_SoundLoaders) do
    if e_SoundLoaders[I].MatchHeader(Data, Len) then
    begin
      Result := e_SoundLoaders[I].GetLoader();
      break;
    end;
end;

procedure e_AddSoundLoader(Loader: TSoundLoaderFactory);
begin
  SetLength(e_SoundLoaders, Length(e_SoundLoaders) + 1);
  e_SoundLoaders[High(e_SoundLoaders)] := Loader;
end;

procedure Cleanup();
var
  ldr: TSoundLoaderFactory;
begin
  for ldr in e_SoundLoaders do
    ldr.Destroy();
end;

finalization
  Cleanup();

end.
