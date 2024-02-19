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
    destructor Destroy(); override;
    function Load(Data: Pointer; Len: LongWord; Loop: Boolean): Boolean; override; overload;
    function Load(FName: String; Loop: Boolean): Boolean; override; overload;
    function Finished(): Boolean; override;
    function Restart(): Boolean; override;
    function FillBuffer(Buf: Pointer; Len: LongWord): LongWord; override;

  private
    FMPG: pmpg123_handle;
    FData: TStream;
    FOpen: Boolean;
    FFinished: Boolean;
    FLooping: Boolean;

    function LoadStream(Stream: TStream): Boolean;
  end;

  TMP3LoaderFactory = class (TSoundLoaderFactory)
  public
    function MatchHeader(Data: Pointer; Len: LongWord): Boolean; override;
    function MatchExtension(FName: String): Boolean; override;
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
begin
  try
    Result := TStream(h).Read(buf^, len);
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

function TMP3LoaderFactory.MatchExtension(FName: String): Boolean;
var
  Ext: String;
begin
  Ext := GetFilenameExt(FName);
  Result := (Ext = '.mp3') or (Ext = '.mpeg3');
end;

function TMP3LoaderFactory.GetLoader(): TSoundLoader;
begin
  Result := TMP3Loader.Create();
end;

(* TMP3Loader *)

destructor TMP3Loader.Destroy();
begin
  mpg123_delete(FMPG);  // will call mpg123_close() if needed
  FData.Free();
  inherited;
end;

function TMP3Loader.LoadStream(Stream: TStream): Boolean;
var
  SRate: clong;
  SEnc, SChans: LongInt;
begin
  Result := False;

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
      e_LogWriteln('MPG123: Load(Data) failed: ' + E.Message);
    else;

    mpg123_delete(FMPG);
    FMPG := nil;
    FOpen := False;
    Exit;
  end;

  FData := Stream;
  FFormat.SampleRate := SRate;
  FFormat.SampleBits := 16;
  FFormat.Channels := SChans;
  FStreaming := True;
  FFinished := False;

  Result := True;
end;

function TMP3Loader.Load(Data: Pointer; Len: LongWord; Loop: Boolean): Boolean;
var
  S: TStream = nil;
  Buf: Pointer;
begin
  Result := False;

  // TODO: have to make a dupe here because Data gets deallocated after loading
  //       this is obviously very shit
  Buf := GetMem(Len);
  if Buf = nil then Exit;
  Move(Data^, Buf^, Len);

  try
    S := TSFSMemoryStreamRO.Create(Buf, Len, True);  // this transfers ownership of Buf
    Result := LoadStream(S);
  finally
    if not Result then
      if S <> nil then S.Destroy() else FreeMem(Buf);
  end;

  FLooping := Loop;
end;

function TMP3Loader.Load(FName: String; Loop: Boolean): Boolean;
var
  S: TStream = nil;
begin
  Result := False;

  try
    S := openDiskFileRO(FName);
    Result := LoadStream(S);
    FLooping := Loop;
  except
    on E: Exception do
      e_LogWritefln('MPG123: ERROR: could not read file `%s`: %s', [FName, E.Message]);
  end;

  if not Result then S.Free();
end;

function TMP3Loader.Finished(): Boolean;
begin
  Result := FFinished;
end;

function TMP3Loader.Restart(): Boolean;
begin
  Result := False;
  if FMPG = nil then Exit;
  FFinished := False;
  Result := mpg123_seek(FMPG, 0, 0) = MPG123_OK;
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

  if (Ret = MPG123_DONE) or (Got = 0) then
  begin
    if FLooping then
      Ret := mpg123_seek(FMPG, 0, 0) // loop
    else
      FFinished := True;
  end;

  if (Ret = MPG123_OK) or FFinished then
    Result := Got;
end;

initialization
  if mpg123_init() = MPG123_OK then
    e_AddSoundLoader(TMP3LoaderFactory.Create());

finalization
  mpg123_exit();

end.
