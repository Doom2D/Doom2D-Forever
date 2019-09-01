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
    constructor Create();
    destructor Destroy(); override;
    function MatchHeader(Data: Pointer; Len: LongWord): Boolean; override;
    function MatchExtension(FName: string): Boolean; override;
    function GetLoader(): TSoundLoader; override;
  private
    FMPG: pmpg123_handle; // tester context
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

constructor TMP3LoaderFactory.Create();
begin
  FMPG := mpg123_new(nil, nil);
  if FMPG <> nil then
    mpg123_replace_reader_handle(FMPG, streamRead, streamLSeek, nil);
end;

destructor TMP3LoaderFactory.Destroy();
begin
  if FMPG <> nil then mpg123_delete(FMPG);
end;

function TMP3LoaderFactory.MatchHeader(Data: Pointer; Len: LongWord): Boolean;
var
  ID3: array [0..9] of Byte;
  HeaderLen: LongInt;
  S: TSFSMemoryStreamRO;
  Info: mpg123_frameinfo;
begin
  Result := False;
  if Len < 10 then Exit;

  // try and check for an ID3 header
  Move(Data^, ID3, 10);
  if (ID3[0] = Ord('I')) and (ID3[1] = Ord('D')) and (ID3[2] = Ord('3')) then
  begin
    HeaderLen := ID3[9] + (ID3[8] shl 7) + (ID3[7] shl 14) + (ID3[6] shl 21);
    Result := Len > (HeaderLen + 10);
    if Result then Exit;
  end;

  // if there isn't one, employ heavier shit
  if FMPG = nil then Exit;

  S := TSFSMemoryStreamRO.Create(Data, Len);

  if mpg123_open_handle(FMPG, S) = MPG123_OK then
  begin
    Result := mpg123_info(FMPG, @Info) = MPG123_OK;
    mpg123_close(FMPG);
  end;

  S.Destroy();
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
  SRate, SEnc, SChans: LongInt;
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
      e_LogWritefln('ModPlug: ERROR: could not read file `%s`: %s', [FName, E.Message]);
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
  if FLooping and (Ret = MPG123_DONE) then
    mpg123_seek(FMPG, 0, 0); // loop
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
