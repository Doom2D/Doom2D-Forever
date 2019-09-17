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
unit e_soundfile_mp3;

interface

uses e_soundfile, mpg123, classes;

type
  // an MP3 loader that uses libmpg123

  TMP3Loader = class (TSoundLoader)
  public
    function Load(Data: Pointer; Len: LongWord; SStreaming: Boolean): Boolean; override; overload;
    function Load(FName: string; SStreaming: Boolean): Boolean; override; overload;
    function SetPosition(Pos: LongWord): Boolean; override;
    function FillBuffer(Buf: Pointer; Len: LongWord): LongWord; override;
    function GetAll(var OutPtr: Pointer): LongWord; override;
    procedure Free(); override;

  private
    FMPG: pmpg123_handle;
    FData: TStream;
    FBuf: Pointer;
    FAllSamples: Pointer;
    FOpen: Boolean;

    function LoadStream(Stream: TStream; SStreaming: Boolean): Boolean;
  end;

  TMP3LoaderFactory = class (TSoundLoaderFactory)
  public
    function MatchHeader(Data: Pointer; Len: LongWord): Boolean; override;
    function MatchExtension(FName: string): Boolean; override;
    function GetLoader(): TSoundLoader; override;
  end;

implementation

uses sysutils, utils, e_sound, e_log, ctypes, xstreams;

(* Reader functions for mpg123_replace_reader_handle *)

function streamLSeek(h: Pointer; off: coff_t; whence: cint): coff_t; cdecl;
var
  S: TStream;
begin
  S:= TStream(h);
  try
    case whence of
      0: Result := s.Seek(off, soBeginning); // SEEK_SET
      1: Result := s.Seek(off, soCurrent);   // SEEK_CUR
      2: Result := s.Seek(off, soEnd);       // SEEK_END
    end;
  except
    Result := -1;
  end;
end;

function streamRead(h: Pointer; buf: Pointer; len: csize_t): csize_t; cdecl; // ssize_t
var
  S: TStream;
begin
  S:= TStream(h);
  try
    Result := S.Read(buf^, len);
  except
    Result := csize_t(-1);
  end;
end;

(* TMP3LoaderFactory *)


function TMP3LoaderFactory.MatchHeader(Data: Pointer; Len: LongWord): Boolean;
var
  P: PByte;
  N: LongInt;
begin
  Result := False;
  if Len < 10 then Exit; // way too short even without an ID3

  P := PByte(Data);

  // try to check for an ID3v2 header
  if ((P+0)^ = $49) and ((P+1)^ = $44) and ((P+2)^ = $33) then // 'ID3'
  begin
    N := (P+9)^ + ((P+8)^ shl 7) + ((P+7)^ shl 14) + ((P+6)^ shl 21);
    Result := Len > (N + 10);
    if Result then Exit;
  end;

  // try to read the frame sync word, bits 0-10 should be 1
  if ((P+0)^ = $FF) and (((P+1)^ and $E0) = $E0) then
  begin
    // bits 11-12: mpeg version, can't be 01
    if (((P+1)^ and $10) = 0) and (((P+1)^ and $08) = $08) then
      Exit;
    // bits 13-14: layer: can't be 00
    if ((P+1)^ and $06) = 0 then
      Exit;
    // bits 16-19: bitrate index: can't be 1111 or 0000
    if (((P+2)^ and $F0) = 0) or (((P+2)^ and $F0) = $F0) then
      Exit;
    // bits 20-21: samplerate index: can't be 11
    if ((P+2)^ and $0C) = $0C then
      Exit;
    // this is probably an MP3 then
    Result := True;
  end;
end;

function TMP3LoaderFactory.MatchExtension(FName: string): Boolean;
var
  Ext: string;
begin
  Ext := GetFilenameExt(FName);
  Result := (Ext = '.mp3') or (Ext = '.mpeg3');
end;

function TMP3LoaderFactory.GetLoader(): TSoundLoader;
begin
  Result := TMP3Loader.Create();
end;

(* TMP3Loader *)

function TMP3Loader.LoadStream(Stream: TStream; SStreaming: Boolean): Boolean;
var
  SRate: clong;
  SEnc, SChans: LongInt;
begin
  FMPG := mpg123_new(nil, nil);
  if FMPG = nil then
  begin
    e_LogWriteln('MPG123: mpg123_new() failed');
    Exit;
  end;

  try
    if mpg123_replace_reader_handle(FMPG, streamRead, streamLSeek, nil) <> MPG123_OK then
      raise Exception.Create('mpg123_replace_header_handle failed');
    if mpg123_open_handle(FMPG, Stream) <> MPG123_OK then
      raise Exception.Create('mpg123_open_handle failed');

    FOpen := True;

    if mpg123_getformat(FMPG, @SRate, @SChans, @SEnc) <> MPG123_OK then
      raise Exception.Create('mpg123_get_format failed');
    if (SChans < 1) or (SChans > 2) or (SRate <= 0) then
      raise Exception.Create('invalid format');

    mpg123_format_none(FMPG);
    if mpg123_format(FMPG, SRate, SChans, MPG123_ENC_SIGNED_16) <> MPG123_OK then
      raise Exception.Create('mpg123_format failed');
  except
    on E: Exception do
    begin
      e_LogWriteln('MPG123: Load(Data) failed: ' + E.Message);
      if FOpen then mpg123_close(FMPG);
      mpg123_delete(FMPG);
      FMPG := nil;
      FOpen := False;
      Exit;
    end;
  end;

  FData := Stream;
  FFormat.SampleRate := SRate;
  FFormat.SampleBits := 16;
  FFormat.Channels := SChans;
  FStreaming := SStreaming;

  Result := True;
end;

function TMP3Loader.Load(Data: Pointer; Len: LongWord; SStreaming: Boolean): Boolean;
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

function TMP3Loader.Load(FName: string; SStreaming: Boolean): Boolean;
var
  S: TStream = nil;
begin
  Result := False;

  try
    S := openDiskFileRO(FName);
    Result := LoadStream(S, SStreaming);
  except
    on E: Exception do
      e_LogWritefln('MPG123: ERROR: could not read file `%s`: %s', [FName, E.Message]);
  end;

  if not Result and (S <> nil) then
    S.Destroy();
end;

function TMP3Loader.SetPosition(Pos: LongWord): Boolean;
begin
  Result := False;
  if FMPG = nil then Exit;
  Result := mpg123_seek(FMPG, Pos, 0) = MPG123_OK;
end;

function TMP3Loader.FillBuffer(Buf: Pointer; Len: LongWord): LongWord;
var
  Ret: LongInt;
  Got: csize_t;
begin
  Result := 0;
  Got := 0;
  if FMPG = nil then Exit;
  Ret := mpg123_read(FMPG, Buf, Len, @Got);
  if FLooping and ((Ret = MPG123_DONE) or (Got = 0)) then
    Ret := mpg123_seek(FMPG, 0, 0); // loop
  if Ret = MPG123_OK then
    Result := Got;
end;

function TMP3Loader.GetAll(var OutPtr: Pointer): LongWord;
begin
  Result := 0;
  if FMPG = nil then Exit;
  if FStreaming then Exit;
  // TODO
end;

procedure TMP3Loader.Free();
begin
  if FOpen then mpg123_close(FMPG);
  if FMPG <> nil then mpg123_delete(FMPG);
  if FData <> nil then FData.Destroy();
  if FBuf <> nil then FreeMem(FBuf);
  if FAllSamples <> nil then FreeMem(FAllSamples);
  FOpen := False;
  FMPG := nil;
  FData := nil;
  FBuf := nil;
  FAllSamples := nil;
end;

initialization
  if mpg123_init() = MPG123_OK then
    e_AddSoundLoader(TMP3LoaderFactory.Create());
finalization
  mpg123_exit();
end.
