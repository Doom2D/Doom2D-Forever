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
unit e_soundfile_modplug;

interface

uses e_soundfile, modplug;

type
  // a module loader that uses libxmp-lite

  TModPlugLoader = class (TSoundLoader)
  public
    function Load(Data: Pointer; Len: LongWord; SStreaming: Boolean): Boolean; override; overload;
    function Load(FName: string; SStreaming: Boolean): Boolean; override; overload;
    function SetPosition(Pos: LongWord): Boolean; override;
    function FillBuffer(Buf: Pointer; Len: LongWord): LongWord; override;
    function GetAll(var OutPtr: Pointer): LongWord; override;
    procedure Free(); override;
  private
    FFile: PModPlugFile;
  end;

  TModPlugLoaderFactory = class (TSoundLoaderFactory)
    function MatchHeader(Data: Pointer; Len: LongWord): Boolean; override;
    function MatchExtension(FName: string): Boolean; override;
    function GetLoader(): TSoundLoader; override;
  end;

implementation

uses sysutils, utils, e_sound, e_log, classes;

var
  Settings: ModPlug_Settings = (
    mFlags            : MODPLUG_ENABLE_OVERSAMPLING or MODPLUG_ENABLE_NOISE_REDUCTION;
    mChannels         : 2;
    mBits             : 16;
    mFrequency        : 44100;
    mResamplingMode   : MODPLUG_RESAMPLE_LINEAR;
    mStereoSeparation : 128;
    mMaxMixChannels   : 32;
    mReverbDepth      : 0;
    mReverbDelay      : 0;
    mBassAmount       : 0;
    mBassRange        : 0;
    mSurroundDepth    : 0;
    mSurroundDelay    : 0;
    mLoopCount        : -1;
  );

(* TModPlugLoaderFactory *)

function TModPlugLoaderFactory.MatchHeader(Data: Pointer; Len: LongWord): Boolean;
var
  Mpf: PModPlugFile;
begin
  // HACK: there's no "test" function in modplug, so just try to load that shit
  Result := False;

  Mpf := ModPlug_Load(Data, Len);
  if Mpf = nil then Exit;
  ModPlug_Unload(Mpf);

  Result := True;
end;

function TModPlugLoaderFactory.MatchExtension(FName: string): Boolean;
var
  Ext: string;
begin
  Ext := GetFilenameExt(FName);
  Result := (Ext = '.it') or (Ext = '.xm') or (Ext = '.mod') or (Ext = '.s3m');
end;

function TModPlugLoaderFactory.GetLoader(): TSoundLoader;
begin
  ModPlug_SetSettings(@Settings); // update settings just in case
  Result := TModPlugLoader.Create();
end;

(* TModPlugLoader *)

function TModPlugLoader.Load(Data: Pointer; Len: LongWord; SStreaming: Boolean): Boolean;
begin
  Result := False;

  FFile := ModPlug_Load(Data, Len);
  if FFile = nil then
  begin
    e_LogWriteln('ModPlug: ERROR: ModPlug_Load failed');
    Exit;
  end;

  FFormat.SampleRate := 44100;
  FFormat.SampleBits := 16;
  FFormat.Channels := 2;
  FStreaming := True; // modules are always streaming

  Result := True;
end;

function TModPlugLoader.Load(FName: string; SStreaming: Boolean): Boolean;
var
  S: TStream = nil;
  Data: Pointer;
  Len: LongInt;
begin
  Result := False;

  try
    S := openDiskFileRO(FName);
    // ayy just read the entire file
    Data := GetMem(S.Size);
    if Data = nil then
      raise Exception.Create('out of memory');
    Len := S.Read(Data^, S.Size);
    if Len < 0 then
      raise Exception.Create('what the fuck');
    Result := Load(Data, Len, SStreaming)
  except
    on E: Exception do
      e_LogWritefln('ModPlug: ERROR: could not read file `%s`: %s', [FName, E.Message]);
  end;

  if Data <> nil then FreeMem(Data);
  if S <> nil then S.Free();
end;

function TModPlugLoader.SetPosition(Pos: LongWord): Boolean;
begin
  Result := False;
  if FFile = nil then Exit;
  ModPlug_Seek(FFile, Pos);
  Result := True;
end;

function TModPlugLoader.FillBuffer(Buf: Pointer; Len: LongWord): LongWord;
var
  Cnt: LongInt;
begin
  Result := 0;
  if FFile = nil then Exit;

  Cnt := ModPlug_Read(FFile, Buf, Len);
  if Cnt < 0 then Exit;

  if FLooping and (Cnt < Len) then
  begin
    // assume it just ended and restart, because modplug only loops if the
    // module tells it to
    ModPlug_Seek(FFile, 0);
    // this used to be Result := Cnt + Read(FFile, Buf + Cnt, Len - Cnt)
    // but the difference appears to be negligible
    Result := ModPlug_Read(FFile, Buf, Len);
  end
  else
    Result := Len;
end;

function TModPlugLoader.GetAll(var OutPtr: Pointer): LongWord;
begin
  Result := 0; // modules are always streaming, so this don't make sense
end;

procedure TModPlugLoader.Free();
begin
  if FFile <> nil then
  begin
    ModPlug_Unload(FFile);
    FFile := nil;
  end;
end;

initialization
  e_AddSoundLoader(TModPlugLoaderFactory.Create());
end.
