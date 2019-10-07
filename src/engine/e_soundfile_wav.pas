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

uses
  {$IFDEF USE_SDL}
    SDL,
  {$ELSE}
    SDL2,
  {$ENDIF}
  utils, ctypes, e_log;

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
function FixSoundEndian (Buf: PUInt8; Len: UInt32; format: UInt16; rate: cint; chan: UInt8): Boolean;
  const
    {$IFDEF FPC_LITTLE_ENDIAN}
      TARGET_AUDIO_S16 = AUDIO_S16LSB;
      TARGET_AUDIO_U16 = AUDIO_U16LSB;
    {$ELSE}
      TARGET_AUDIO_S16 = AUDIO_S16MSB;
      TARGET_AUDIO_U16 = AUDIO_U16MSB;
    {$ENDIF}
  var cvt: TSDL_AudioCVT; tformat: UInt16;
begin
  case format of
    AUDIO_U16LSB, AUDIO_U16MSB: tformat := TARGET_AUDIO_U16;
    AUDIO_S16LSB, AUDIO_S16MSB: tformat := TARGET_AUDIO_S16;
  else tformat := format
  end;
  Result := True;
  if format <> tformat then
  begin
    Result := False;
    if SDL_BuildAudioCVT(@cvt, format, chan, rate, tformat, chan, rate) <> -1 then
    begin
      cvt.buf := Buf;
      cvt.len := Len;
      assert(cvt.len_mult = 1);
      Result := SDL_ConvertAudio(@cvt) = 0;
      assert(cvt.len_ratio = 1);
      assert(cvt.len = Len)
    end
  end
end;

function LoadWavRW (Loader: TWAVLoader; RW: PSDL_RWops): Boolean;
  var
    Spec: TSDL_AudioSpec;
    Len: UInt32;
    Buf: PUInt8;
begin
  Result := False;
{$IFDEF USE_SDL2}
  if SDL_LoadWAV_RW(RW, 0, @Spec, @Buf, @Len) <> nil then
{$ELSE}
  if SDL_LoadWAV_RW(RW, 0, @Spec, PUInt8(@Buf), @Len) <> nil then
{$ENDIF}
  begin
    Result := FixSoundEndian(Buf, Len, Spec.format, Spec.freq, Spec.channels);
    if Result = True then
    begin
      with Loader do
      begin
        FFormat.SampleRate := Spec.freq;
        {$IFDEF USE_SDL2}
          FFormat.SampleBits := SDL_AUDIO_BITSIZE(Spec.format);
        {$ELSE}
          FFormat.SampleBits := Spec.format and $FF;
        {$ENDIF}
        FFormat.Channels := Spec.channels;
        FStreaming := False; // never stream wavs
        FDataLen := Len;
        FData := Buf;
      end
    end
    else
    begin
      SDL_FreeWav(Buf)
    end
  end
end;

function TWAVLoader.Load(Data: Pointer; Len: LongWord; SStreaming: Boolean): Boolean;
  var
    RW: PSDL_RWops;
begin
  RW := SDL_RWFromConstMem(Data, Len);
  Result := LoadWavRW(Self, RW);
  if Result = False then
    e_LogWriteln('Could not load WAV: ' + SDL_GetError());
  SDL_RWclose(RW);
end;

function TWAVLoader.Load(FName: string; SStreaming: Boolean): Boolean;
  var
    RW: PSDL_RWops;
begin
  RW := SDL_RWFromFile(PChar(FName), 'rb');
  if RW <> nil then
  begin
    Result := LoadWavRW(Self, RW);
    if Result = False then
      e_LogWritefln('Could not load WAV file `%s`: %s', [FName, SDL_GetError()]);
  end
  else
  begin
    e_LogWritefln('Could not open WAV file `%s`: %s', [FName, SDL_GetError()]);
    Result := False
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
