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
unit e_soundfile_opus;

interface

uses e_soundfile, opus, classes;

type
  // Opus loader

  TOpusLoader = class (TSoundLoader)
  public
    destructor Destroy(); override;
    function Load(Data: Pointer; Len: LongWord; Loop: Boolean): Boolean; override; overload;
    function Load(FName: String; Loop: Boolean): Boolean; override; overload;
    function Finished(): Boolean; override;
    function Restart(): Boolean; override;
    function FillBuffer(Buf: Pointer; Len: LongWord): LongWord; override;
  private
    FOpus: POggOpusFile;
    FBuf: Pointer;
    FFinished: Boolean;
    FLooping: Boolean;
  end;

  TOpusLoaderFactory = class (TSoundLoaderFactory)
  public
    function MatchHeader(Data: Pointer; Len: LongWord): Boolean; override;
    function MatchExtension(FName: String): Boolean; override;
    function GetLoader(): TSoundLoader; override;
  end;

implementation

uses sysutils, utils, e_log, xstreams, ogg, ctypes;

function TOpusLoaderFactory.MatchHeader(Data: Pointer; Len: LongWord): Boolean;
const
  OGG_HEADER = $5367674F; // 'OggS'
var
  F: POggOpusFile = nil;
begin
  Result := False;

  if Len < 27 then // header is at least 27 bytes
    Exit;
  if PLongWord(Data)^ <> OGG_HEADER then
    Exit;

  // now we gotta check that this is indeed an opus file and not a vorbis file

  F := op_test_memory(Data, Len, nil);
  Result := F <> nil;
  if Result then op_free(F);
end;

function TOpusLoaderFactory.MatchExtension(FName: String): Boolean;
begin
  Result := GetFilenameExt(FName) = '.opus';
end;

function TOpusLoaderFactory.GetLoader(): TSoundLoader;
begin
  Result := TOpusLoader.Create();
end;

(* TOpusLoader *)

destructor TOpusLoader.Destroy();
begin
  if FOpus <> nil then op_free(FOpus);
  FreeMem(FBuf);
  inherited;
end;

function TOpusLoader.Load(Data: Pointer; Len: LongWord; Loop: Boolean): Boolean;
begin
  Result := False;

  FBuf := GetMem(Len);
  if FBuf = nil then
  begin
    e_LogWriteln('Opus: Load(Data) failed: out of memory on copy');
    Exit;
  end;
  Move(Data^, FBuf^, Len);

  FOpus := op_open_memory(FBuf, Len, nil);
  if FOpus = nil then
  begin
    Free();
    e_LogWriteln('Opus: Load(Data) failed: op_open_memory failed');
    Exit;
  end;

  FFormat.Channels := 2; // we use ov_read_stereo
  FFormat.SampleBits := 16;
  FFormat.SampleRate := 48000; // is this even correct?
  FStreaming := True; // opus is always streaming
  FFinished := False;
  FLooping := Loop;

  Result := True;
end;

function TOpusLoader.Load(FName: String; Loop: Boolean): Boolean;
begin
  Result := False;

  FOpus := op_open_file(PChar(FName), nil);
  if FOpus = nil then
  begin
    e_LogWritefln('Opus: Load(%s) failed: op_open_file failed', [FName]);
    Exit;
  end;

  FFormat.Channels := 2; // we use ov_read_stereo
  FFormat.SampleBits := 16;
  FFormat.SampleRate := 48000; // is this even correct?
  FStreaming := True; // opus is always streaming
  FFinished := False;
  FLooping := Loop;

  Result := True;
end;

function TOpusLoader.Finished(): Boolean;
begin
  Result := FFinished;
end;

function TOpusLoader.Restart(): Boolean;
begin
  Result := False;
  if (FOpus = nil) or (op_seekable(FOpus) = 0) then Exit;
  Result := op_pcm_seek(FOpus, 0) = 0;
  FFinished := False;
end;

function TOpusLoader.FillBuffer(Buf: Pointer; Len: LongWord): LongWord;
var
  Ret: cint;
  Rx: Integer;
begin
  Result := 0;
  if FOpus = nil then Exit;

  Rx := 0;

  while Rx < Len do
  begin
    Ret := op_read_stereo(FOpus, Buf + Rx, (Len - Rx) div 2);
    if Ret = OP_HOLE then continue;
    if Ret < 0 then break;
    if Ret = 0 then
    begin
      if FLooping then
        op_pcm_seek(FOpus, 0)
      else
      begin
        FFinished := True;
        break;
      end;
    end;
    Rx := Rx + Ret * 4;
  end;

  Result := Rx;
end;

initialization
  e_AddSoundLoader(TOpusLoaderFactory.Create());
end.
