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
unit e_soundfile;

interface

type
  TSoundLoader = class;

  TSoundFormat = record
    Loader: TSoundLoader;
    SampleBits: Integer;
    SampleRate: Integer;
    Channels: Integer;
  end;

  // each sound file format has its own loader 
  // TODO: maybe make TBasicSound contain an instance of its loader
  //       and add a FetchSamples method or something, for streaming shit
  TSoundLoader = class
  public
    // can this loader load the sound file in Data?
    function CanLoad(Data: Pointer; Len: Integer): Boolean; virtual; abstract; overload;
    // can this loader load the sound file at FName?
    function CanLoad(FName: string): Boolean; virtual; abstract; overload;
    // load from memory
    function Load(Data: Pointer; Len: Integer; var OutLen: Integer; var OutFmt: TSoundFormat): Pointer; virtual; abstract; overload;
    // load from file
    function Load(FName: string; var OutLen: Integer; var OutFmt: TSoundFormat): Pointer; virtual; abstract; overload;
    // needed in case memory is allocated in a lib or something
    procedure Free(Data: Pointer); virtual; abstract;
  end;

function e_GetSoundLoader(Data: Pointer; Len: Integer): TSoundLoader; overload;
function e_GetSoundLoader(FName: string): TSoundLoader; overload;

procedure e_AddSoundLoader(Loader: TSoundLoader);

implementation

var
  e_SoundLoaders: array of TSoundLoader;

function e_GetSoundLoader(FName: string): TSoundLoader; overload;
var
  I: Integer;
begin
  Result := nil;
  for I := Low(e_SoundLoaders) to High(e_SoundLoaders) do
    if e_SoundLoaders[I].CanLoad(FName) then
    begin
      Result := e_SoundLoaders[I];
      break;
    end;
end;

function e_GetSoundLoader(Data: Pointer; Len: Integer): TSoundLoader; overload;
var
  I: Integer;
begin
  Result := nil;
  for I := Low(e_SoundLoaders) to High(e_SoundLoaders) do
    if e_SoundLoaders[I].CanLoad(Data, Len) then
    begin
      Result := e_SoundLoaders[I];
      break;
    end;
end;

procedure e_AddSoundLoader(Loader: TSoundLoader);
begin
  SetLength(e_SoundLoaders, Length(e_SoundLoaders) + 1);
  e_SoundLoaders[High(e_SoundLoaders)] := Loader;
end;

end.
