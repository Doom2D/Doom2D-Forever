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
unit e_soundfile_ogg;

interface

uses e_soundfile, vorbis, classes;

type
  // OGG Vorbis loader

  TOGGLoader = class (TSoundLoader)
  public
    function Load(Data: Pointer; Len: LongWord; SStreaming: Boolean): Boolean; override; overload;
    function Load(FName: string; SStreaming: Boolean): Boolean; override; overload;
    function SetPosition(Pos: LongWord): Boolean; override;
    function FillBuffer(Buf: Pointer; Len: LongWord): LongWord; override;
    function GetAll(var OutPtr: Pointer): LongWord; override;
    procedure Free(); override;

  private
    FOgg: OggVorbis_File;
    FData: TStream;
    FBuf: Pointer;
    FTotal: LongWord;
    FOpen: Boolean;

    function LoadStream(Stream: TStream; SStreaming: Boolean): Boolean;
    function LoadEntireStream(): Pointer;
  end;

  TOGGLoaderFactory = class (TSoundLoaderFactory)
  public
    function MatchHeader(Data: Pointer; Len: LongWord): Boolean; override;
    function MatchExtension(FName: string): Boolean; override;
    function GetLoader(): TSoundLoader; override;
  end;

implementation

uses sysutils, utils, e_log, xstreams, ogg, ctypes;

(* Reader functions for ov_callbacks *)

function streamSeek(h: Pointer; off: ogg_int64_t; whence: cint): cint; cdecl;
var
  S: TStream;
begin
  Result := -1;
  if h = nil then Exit;
  S:= TStream(h);
  try
    case whence of
      0: s.Seek(off, soBeginning); // SEEK_SET
      1: s.Seek(off, soCurrent);   // SEEK_CUR
      2: s.Seek(off, soEnd);       // SEEK_END
    end;
    Result := 0;
  except
    Result := -1;
  end;
end;

function streamRead(buf: Pointer; sz, nmemb: csize_t; h: Pointer): csize_t; cdecl;
var
  S: TStream;
begin
  Result := 0;
  if h = nil then Exit;
  S:= TStream(h);
  try
    Result := S.Read(buf^, sz*nmemb) div sz;
  except
    Result := 0;
  end;
end;

function streamTell(h: Pointer): clong; cdecl;
var
  S: TStream;
begin
  Result := -1;
  if h = nil then Exit;
  S := TStream(h);
  Result := S.Position;
end;

var
  oggIO: ov_callbacks = (
    read:        streamRead;
    seek:        streamSeek;
    close:       nil; // the loader's gonna handle that
    tell:        streamTell;
  );

(* TOGGLoaderFactory *)

function TOGGLoaderFactory.MatchHeader(Data: Pointer; Len: LongWord): Boolean;
const
  OGG_HEADER = $5367674F; // 'OggS'
begin
  if Len < 27 then // header is at least 27 bytes
  begin
    Result := False;
    exit;
  end;
  Result := (PLongWord(Data)^ = OGG_HEADER);
end;

function TOGGLoaderFactory.MatchExtension(FName: string): Boolean;
begin
  Result := GetFilenameExt(FName) = '.ogg';
end;

function TOGGLoaderFactory.GetLoader(): TSoundLoader;
begin
  Result := TOGGLoader.Create();
end;

(* TOGGLoader *)

function TOGGLoader.LoadEntireStream(): Pointer;
var
  Samples: ogg_int64_t;
  Ret: clong;
begin
  Result := nil;

  Samples := ov_pcm_total(FOgg, -1);
  if Samples < 0 then Exit;

  FTotal := Samples * 2 * FFormat.Channels;
  Result := GetMem(FTotal);
  if Result = nil then Exit;

  Ret := ov_read_ext(FOgg, Result, FTotal, False, 2, True);
  if Ret < 0 then
  begin
    FreeMem(Result);
    Result := nil;
  end
  else
    FTotal := Ret;
end;

function TOGGLoader.LoadStream(Stream: TStream; SStreaming: Boolean): Boolean;
var
  Ret, Bit: clong;
  Info: pvorbis_info;
  FullBuf: Pointer;
begin
  Result := False;

  Ret := ov_open_callbacks(Stream, FOgg, nil, 0, oggIO);
  if Ret < 0 then
  begin
    e_LogWriteln('OGG: Load(Data) failed: ov_open_callbacks failed');
    Exit;
  end;

  Info := ov_info(FOgg, -1);
  if Info = nil then
  begin
    e_LogWriteln('OGG: Load(Data) failed: ov_info returned NULL');
    ov_clear(FOgg);
    Exit;
  end;

  FFormat.SampleRate := Info^.rate;
  FFormat.Channels := Info^.channels;
  FFormat.SampleBits := 16;

  if not SStreaming then
  begin
    FullBuf := LoadEntireStream();

    if FullBuf = nil then
    begin
      e_LogWriteln('OGG: Load(Data) failed: couldn''t allocate for non-streaming chunk');
      ov_clear(FOgg);
      FTotal := 0;
      Exit;
    end;

    ov_clear(FOgg);
    Stream.Destroy();

    FreeMem(FBuf);
    FBuf := FullBuf;
  end
  else
  begin
    FTotal := 0;
    FOpen := True;
    FData := Stream;
  end;

  FStreaming := SStreaming;
  Result := True;
end;

function TOGGLoader.Load(Data: Pointer; Len: LongWord; SStreaming: Boolean): Boolean;
var
  S: TStream;
begin
  Result := False;

  // TODO: have to make a dupe here because Data gets deallocated after loading
  //       this is obviously very shit
  FBuf := GetMem(Len);
  if FBuf = nil then Exit;
  Move(Data^, FBuf^, Len);

  S := TSFSMemoryStreamRO.Create(FBuf, Len{, True});
  Result := LoadStream(S, SStreaming);

  if not Result and (S <> nil) then
  begin
    S.Destroy();
    FreeMem(FBuf);
    FBuf := nil;
  end;
end;

function TOGGLoader.Load(FName: string; SStreaming: Boolean): Boolean;
var
  S: TStream = nil;
begin
  Result := False;

  try
    S := openDiskFileRO(FName);
    Result := LoadStream(S, SStreaming);
  except
    on E: Exception do
      e_LogWritefln('OGG: ERROR: could not read file `%s`: %s', [FName, E.Message]);
  end;

  if not Result and (S <> nil) then
    S.Destroy();
end;

function TOGGLoader.SetPosition(Pos: LongWord): Boolean;
begin
  Result := False;
  if not FOpen or (ov_seekable(FOgg) = 0) then Exit;
  Result := ov_pcm_seek(FOgg, Pos) = 0;
end;

function TOGGLoader.FillBuffer(Buf: Pointer; Len: LongWord): LongWord;
var
  Ret: clong;
begin
  Result := 0;
  if not FOpen or not FStreaming then Exit;
  Ret := ov_read_ext(FOgg, Buf, Len, False, 2, True);
  if Ret < 0 then Exit;
  Result := Ret; 
end;

function TOGGLoader.GetAll(var OutPtr: Pointer): LongWord;
begin
  Result := 0;
  if FStreaming or (FTotal = 0) then Exit;
  Result := FTotal;
  OutPtr := FBuf;
end;

procedure TOGGLoader.Free();
begin
  if FOpen then
    ov_clear(FOgg);
  if FData <> nil then
    FData.Destroy();
  if FBuf <> nil then
    FreeMem(FBuf);
  FData := nil;
  FBuf := nil;
  FOpen := False;
  FTotal := 0;
  FStreaming := False;
end;

initialization
  e_AddSoundLoader(TOGGLoaderFactory.Create());
end.
