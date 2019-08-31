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
unit e_soundfile_wav;

interface

uses e_soundfile;

type
  // a WAV loader that just uses SDL_LoadWAV
  TWAVLoader = class (TSoundLoader)
  public
    function CanLoad(Data: Pointer; Len: Integer): Boolean; override; overload;
    function CanLoad(FName: string): Boolean; override; overload;
    function Load(Data: Pointer; Len: Integer; var OutLen: Integer; var OutFmt: TSoundFormat): Pointer; override; overload;
    function Load(FName: string; var OutLen: Integer; var OutFmt: TSoundFormat): Pointer; override; overload;
    procedure Free(Data: Pointer); override;
  end;

implementation

uses sdl2, utils, e_log;

function TWAVLoader.CanLoad(Data: Pointer; Len: Integer): Boolean;
var
  P: PByte;
begin
  if Len < 5 then
  begin
    Result := False;
    exit;
  end;
  P := PByte(Data);
  Result := ((P+0)^ = Ord('R')) and ((P+1)^ = Ord('I')) and ((P+2)^ = Ord('F')) and ((P+3)^ = Ord('F'));
end;

function TWAVLoader.CanLoad(FName: string): Boolean;
begin
  // TODO: actually check for RIFF header
  Result := GetFilenameExt(FName) = '.wav';
end;

function TWAVLoader.Load(Data: Pointer; Len: Integer; var OutLen: Integer; var OutFmt: TSoundFormat): Pointer;
var
  Spec: TSDL_AudioSpec;
  RW: PSDL_RWops;
  TmpLen: UInt32;
  TmpBuf: PUInt8;
begin
  Result := nil;

  RW := SDL_RWFromConstMem(Data, Len);

  if SDL_LoadWAV_RW(RW, 0, @Spec, @TmpBuf, @TmpLen) = nil then
  begin
    e_LogWriteln('Could not load WAV: ' + SDL_GetError());
  end
  else
  begin
    OutFmt.Loader := self;
    OutFmt.SampleRate := Spec.freq;
    OutFmt.SampleBits := SDL_AUDIO_BITSIZE(Spec.format);
    OutFmt.Channels := Spec.channels;
    OutLen := TmpLen;
    Result := TmpBuf;
  end;

  SDL_RWclose(RW);
end;

function TWAVLoader.Load(FName: string; var OutLen: Integer; var OutFmt: TSoundFormat): Pointer;
var
  Spec: TSDL_AudioSpec;
  RW: PSDL_RWops;
  TmpLen: UInt32;
  TmpBuf: PUInt8;
begin
  Result := nil;

  RW := SDL_RWFromFile(PChar(FName), 'rb');

  if RW = nil then
  begin
    e_LogWritefln('Could not open WAV file `%s`: %s', [FName, SDL_GetError()]);
    exit;
  end;

  if SDL_LoadWAV_RW(RW, 0, @Spec, @TmpBuf, @TmpLen) = nil then
  begin
    e_LogWritefln('Could not load WAV file `%s`: %s', [FName, SDL_GetError()]);
    Result := nil;
  end
  else
  begin
    OutFmt.Loader := self;
    OutFmt.SampleRate := Spec.freq;
    OutFmt.SampleBits := SDL_AUDIO_BITSIZE(Spec.format);
    OutFmt.Channels := Spec.channels;
    OutLen := TmpLen;
    Result := TmpBuf;
  end;

  SDL_RWclose(RW);
end;

procedure TWAVLoader.Free(Data: Pointer);
begin
  SDL_FreeWAV(Data); // SDL allocates inside the DLL, so we need this
end;

initialization
  e_AddSoundLoader(TWAVLoader.Create());
end.
