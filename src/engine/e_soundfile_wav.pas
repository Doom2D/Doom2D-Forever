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
unit e_soundfile_wav;

interface

uses e_soundfile;

type
  // a WAV loader that just uses SDL_LoadWAV

  TWAVLoader = class (TSoundLoader)
  public
    function Load(Data: Pointer; Len: LongWord; SStreaming: Boolean): Boolean; override; overload;
    function Load(FName: string; SStreaming: Boolean): Boolean; override; overload;
    function SetPosition(Pos: LongWord): Boolean; override;
    function FillBuffer(Buf: Pointer; Len: LongWord): LongWord; override;
    function GetAll(var OutPtr: Pointer): LongWord; override;
    procedure Free(); override;
  private
    FData: Pointer;
    FDataLen: LongWord;
  end;

  TWAVLoaderFactory = class (TSoundLoaderFactory)
  public
    function MatchHeader(Data: Pointer; Len: LongWord): Boolean; override;
    function MatchExtension(FName: string): Boolean; override;
    function GetLoader(): TSoundLoader; override;
  end;

implementation

uses sdl2, utils, e_log;

(* TWAVLoaderFactory *)

function TWAVLoaderFactory.MatchHeader(Data: Pointer; Len: LongWord): Boolean;
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

function TWAVLoaderFactory.MatchExtension(FName: string): Boolean;
begin
  // TODO: ehhh
  Result := GetFilenameExt(FName) = '.wav';
end;

function TWAVLoaderFactory.GetLoader(): TSoundLoader;
begin
  Result := TWAVLoader.Create();
end;

(* TWAVLoader *)

function TWAVLoader.Load(Data: Pointer; Len: LongWord; SStreaming: Boolean): Boolean;
var
  Spec: TSDL_AudioSpec;
  RW: PSDL_RWops;
  TmpLen: UInt32;
  TmpBuf: PUInt8;
begin
  Result := False;

  RW := SDL_RWFromConstMem(Data, Len);

  if SDL_LoadWAV_RW(RW, 0, @Spec, @TmpBuf, @TmpLen) = nil then
  begin
    e_LogWriteln('Could not load WAV: ' + SDL_GetError());
  end
  else
  begin
    FFormat.SampleRate := Spec.freq;
    FFormat.SampleBits := SDL_AUDIO_BITSIZE(Spec.format);
    FFormat.Channels := Spec.channels;
    FStreaming := False; // never stream wavs
    FDataLen := TmpLen;
    FData := TmpBuf;
    Result := True;
  end;

  SDL_RWclose(RW);
end;

function TWAVLoader.Load(FName: string; SStreaming: Boolean): Boolean;
var
  Spec: TSDL_AudioSpec;
  RW: PSDL_RWops;
  TmpLen: UInt32;
  TmpBuf: PUInt8;
begin
  Result := False;

  RW := SDL_RWFromFile(PChar(FName), 'rb');

  if RW = nil then
  begin
    e_LogWritefln('Could not open WAV file `%s`: %s', [FName, SDL_GetError()]);
    exit;
  end;

  if SDL_LoadWAV_RW(RW, 0, @Spec, @TmpBuf, @TmpLen) = nil then
  begin
    e_LogWritefln('Could not load WAV file `%s`: %s', [FName, SDL_GetError()]);
  end
  else
  begin
    FFormat.SampleRate := Spec.freq;
    FFormat.SampleBits := SDL_AUDIO_BITSIZE(Spec.format);
    FFormat.Channels := Spec.channels;
    FStreaming := False; // never stream wavs
    FDataLen := TmpLen;
    FData := TmpBuf;
    Result := True;
  end;

  SDL_RWclose(RW);
end;

function TWAVLoader.SetPosition(Pos: LongWord): Boolean;
begin
  Result := False; // makes no sense when not streaming
end;

function TWAVLoader.FillBuffer(Buf: Pointer; Len: LongWord): LongWord;
begin
  if FDataLen < Len then
    Len := FDataLen;
  if FData <> nil then
  begin
    Move(FData^, Buf^, Len);
    Result := Len;
  end
  else
    Result := 0;
end;

function TWAVLoader.GetAll(var OutPtr: Pointer): LongWord;
begin
  OutPtr := FData;
  Result := FDataLen;
end;

procedure TWAVLoader.Free();
begin
  if FData <> nil then
    SDL_FreeWAV(FData); // SDL allocates inside the DLL, so we need this
end;

initialization
  e_AddSoundLoader(TWAVLoaderFactory.Create());
end.
