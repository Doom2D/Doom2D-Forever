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
unit e_soundfile_vorbis;

interface

uses e_soundfile, vorbis, classes;

type
  // Ogg Vorbis loader

  TVorbisLoader = class (TSoundLoader)
  public
    destructor Destroy(); override;
    function Load(Data: Pointer; Len: LongWord; Loop: Boolean): Boolean; override; overload;
    function Load(FName: String; Loop: Boolean): Boolean; override; overload;
    function Finished(): Boolean; override;
    function Restart(): Boolean; override;
    function FillBuffer(Buf: Pointer; Len: LongWord): LongWord; override;

  private
    FOgg: OggVorbis_File;
    FData: TStream;
    FOpen: Boolean;
    FFinished: Boolean;
    FLooping: Boolean;

    function LoadStream(Stream: TStream): Boolean;
  end;

  TVorbisLoaderFactory = class (TSoundLoaderFactory)
  public
    function MatchHeader(Data: Pointer; Len: LongWord): Boolean; override;
    function MatchExtension(FName: String): Boolean; override;
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
begin
  Result := 0;
  if h = nil then Exit;

  try
    Result := TStream(h).Read(buf^, sz*nmemb) div sz;
  except
    Result := 0;
  end;
end;

function streamTell(h: Pointer): clong; cdecl;
begin
  Result := -1;
  if h = nil then Exit;
  Result := TStream(h).Position;
end;

var
  oggIO: ov_callbacks = (
    read:        streamRead;
    seek:        streamSeek;
    close:       nil; // the loader's gonna handle that
    tell:        streamTell;
  );

(* TVorbisLoaderFactory *)

function TVorbisLoaderFactory.MatchHeader(Data: Pointer; Len: LongWord): Boolean;
const
  OGG_HEADER = $5367674F; // 'OggS'
var
  S: TStream;
  F: OggVorbis_File;
begin
  Result := False;

  if Len < 27 then // header is at least 27 bytes
    Exit;
  if PLongWord(Data)^ <> OGG_HEADER then
    Exit;

  // now we gotta check that this is indeed a vorbis file and not an opus file

  S := TSFSMemoryStreamRO.Create(Data, Len);
  Result := ov_test_callbacks(S, F, nil, 0, oggIO) = 0;
  if Result then ov_clear(F);
  S.Free();
end;

function TVorbisLoaderFactory.MatchExtension(FName: String): Boolean;
begin
  Result := GetFilenameExt(FName) = '.ogg';
end;

function TVorbisLoaderFactory.GetLoader(): TSoundLoader;
begin
  Result := TVorbisLoader.Create();
end;

(* TVorbisLoader *)

destructor TVorbisLoader.Destroy();
begin
  ov_clear(FOgg);
  FData.Free();
  inherited;
end;

function TVorbisLoader.LoadStream(Stream: TStream): Boolean;
var
  Ret: clong;
  Info: pvorbis_info;
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
  FOpen := True;
  FData := Stream;

  FStreaming := True;
  FFinished := False;
  Result := True;
end;

function TVorbisLoader.Load(Data: Pointer; Len: LongWord; Loop: Boolean): Boolean;
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

function TVorbisLoader.Load(FName: String; Loop: Boolean): Boolean;
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
      e_LogWritefln('OGG: ERROR: could not read file `%s`: %s', [FName, E.Message]);
    else;
  end;

  if not Result then S.Free();
end;

function TVorbisLoader.Finished(): Boolean;
begin
  Result := FFinished;
end;

function TVorbisLoader.Restart(): Boolean;
begin
  Result := False;
  if not FOpen or (ov_seekable(FOgg) = 0) then Exit;
  FFinished := False;
  Result := ov_pcm_seek(FOgg, 0) = 0;
end;

function TVorbisLoader.FillBuffer(Buf: Pointer; Len: LongWord): LongWord;
var
  Ret: clong;
begin
  Result := 0;
  if not FOpen then Exit;
  Ret := ov_read_ext(FOgg, Buf, Len, False, 2, True);
  if Ret < 0 then Exit;
  if Ret = 0 then
  begin
    if FLooping then
      ov_pcm_seek(FOgg, 0)
    else
      FFinished := True;
  end;
  Result := Ret; 
end;

initialization
  e_AddSoundLoader(TVorbisLoaderFactory.Create());
end.
