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

{$I ../shared/a_modes.inc}
unit e_res;

interface

uses
  SysUtils, Classes, Utils;

var
  debug_e_res: Boolean;

// remove last entry from path
function e_UpperDir (path: AnsiString): AnsiString;

// not absolute and have no relative dirs
function e_IsValidResourceName (name: AnsiString): Boolean;

// try to open/create file in one dir from `dirs` in reverse order
// e_OpenResourceRW tries to create if not exists
// create dirs if not exists
// result <> nil, throws exceptions on errors
function e_CreateResource (dirs: SSArray; name: AnsiString): TStream;
function e_OpenResourceRO (dirs: SSArray; name: AnsiString): TStream;
function e_OpenResourceRW (dirs: SSArray; name: AnsiString): TStream;

// same as shared/utils
function e_FindResource (dirs: SSArray; var name: AnsiString; nameIsDir: Boolean = False): Boolean;
function e_FindWad (dirs: SSArray; name: AnsiString): AnsiString;

// returns relative wad name; never empty string
function e_FindWadRel (dirs: SSArray; name: AnsiString): AnsiString;

// prepend dirs to 'disk.wad:\file'. if empty disk string then prepend defWad
// return empty string if error occured or 'path/to/disk.wad:\file' on success
function e_GetResourcePath (dirs: SSArray; path: AnsiString; defWad: AnsiString): AnsiString;

// same as SysUtils.FindFirst
function e_FindFirst (dirs: SSArray; name: AnsiString; attr: LongInt; out Rslt: TSearchRec): LongInt;

// try to get a writeable directory from list, throws if no one directory created
// (unless `required` is `false`: in this case, returns empty string)
// creates all necessary subdirs, if it can
function e_GetWriteableDir (dirs: SSArray; required: Boolean = True): AnsiString;

function e_CanCreateFilesAt (dir: AnsiString): Boolean;

implementation

uses WadReader, e_log, hashtable;

type
  SpawnProc = function (pathname: AnsiString): TStream;

var
  writeableDirs: THashStrCIStr;

function e_UpperDir (path: AnsiString): AnsiString;
var
  i: Integer;
begin
  i := High(path);  // consider possible cases: '\a\', '\a', '\abc\'
  while (i >= 1) and (path[i] <> '/') and (path[i] <> '\') do i -= 1;
  Result := Copy(path, 1, i-1);  // exclude the trailing separator
end;

function IsRelativePath (name: AnsiString): Boolean;
begin
  Result := (Copy(name, 1, 3) = '../') or (Pos('/../', name) <> 0) or (Copy(name, Length(name)-2) = '/..') or
            (Copy(name, 1, 3) = '..\') or (Pos('\..\', name) <> 0) or (Copy(name, Length(name)-2) = '\..') or
            (name = '..');
end;

function IsAbsolutePath (name: AnsiString): Boolean;
begin
  Result := ExpandFileName(name) = name;
end;

function e_IsValidResourceName (name: AnsiString): Boolean;
begin
  Result := (not IsAbsolutePath(name)) and (not IsRelativePath(name));
end;

function SpawnStream (dirs: SSArray; name: AnsiString; p: SpawnProc; createNewDir: Boolean): TStream;
var
  i: Integer;
  s: AnsiString;
begin
  Result := nil;
  Assert(dirs <> nil);
  Assert(e_IsValidResourceName(name));
  i := High(dirs);
  while (i >= 0) and (Result = nil) do
  begin
    try
      if debug_e_res then
        e_LogWritefln('  %s', [dirs[i]]);
      if (not createNewDir) or ForceDirectories(dirs[i]) then
      begin
        // FIXME: hack for improper ConcatPaths(); see commit.
        s := AnsiString(dirs[i]);
        if s = ''
          then s := name
          else s := ConcatPaths([s, name]);

        Result := p(s);
      end;
    finally
      i -= 1;
    end;
  end;
end;

function e_CreateResource (dirs: SSArray; name: AnsiString): TStream;
begin
  if debug_e_res then
    e_LogWritefln('e_CreateResource %s', [name]);

  Result := SpawnStream(dirs, name, @createDiskFile, True);
  if Result = nil then
    Raise Exception.Create('can''t create resource "' + name + '"');
end;

function e_OpenResourceRO (dirs: SSArray; name: AnsiString): TStream;
begin
  if debug_e_res then
    e_LogWritefln('e_OpenResourceRO %s', [name]);

  Result := SpawnStream(dirs, name, @openDiskFileRO, False);
  if Result = nil then
    Raise EFileNotFoundException.Create('can''t open resource "' + name + '"');
end;

function e_OpenResourceRW (dirs: SSArray; name: AnsiString): TStream;
begin
  if debug_e_res then
    e_LogWritefln('e_OpenResourceRW %s', [name]);

  Result := SpawnStream(dirs, name, @openDiskFileRW, True);
  if Result = nil then
    Raise Exception.Create('can''t create resource "' + name + '"');
end;

function e_FindResource (dirs: SSArray; var name: AnsiString; nameIsDir: Boolean): Boolean;
var
  i: Integer;
  dir: AnsiString;
begin
  if debug_e_res then
    e_LogWritefln('e_FindResource %s (%s)', [name, nameIsDir]);

  Result := False;
  Assert(dirs <> nil);
  Assert(e_IsValidResourceName(name));
  i := High(dirs);
  dir := name;

  while (i >= 0) and (not Result) do
  begin
    // FIXME: hack for improper ConcatPaths(); see commit.
    dir := AnsiString(dirs[i]);
    if dir = ''
      then dir := name
      else dir := ConcatPaths([dir, name]);

    Result := findFileCI(dir, nameIsDir);
    if debug_e_res then
      e_LogWritefln('  %s -> %s', [dir, Result]);
    i -= 1;
  end;

  if Result then
    name := dir;

  if debug_e_res then
    e_LogWritefln('  result = %s (%s)', [name, Result]);
end;

function e_FindWad (dirs: SSArray; name: AnsiString): AnsiString;
var
  i: Integer;
begin
  if debug_e_res then
    e_LogWritefln('e_FindWad "%s"', [name]);

  Result := '';
  Assert(dirs <> nil);
  Assert(e_IsValidResourceName(name));
  i := High(dirs);

  while (i >= 0) and (result = '') do
  begin
    Result := findDiskWad(dirs[i] + DirectorySeparator + name);
    if debug_e_res then
      e_LogWritefln('  %s -> %s', [dirs[i] + DirectorySeparator + name, Result]);
    i -= 1;
  end;
end;

function e_FindWadRel (dirs: SSArray; name: AnsiString): AnsiString;
var
  s: AnsiString;
  maxpfx: AnsiString = '';
  pfx: AnsiString;
begin
  Result := name;
  if not findFileCI(name) then Exit;

  for s in dirs do
  begin
    if Length(s) = 0 then Continue;
    if Length(name) <= Length(s) then Continue;
    if Length(s) < Length(maxpfx) then Continue;

    pfx := s;
    if not findFileCI(pfx, True) then Continue;

    if (pfx[Length(pfx)] <> '/') and (pfx[Length(pfx)] <> '\') then pfx += '/';
    if Length(pfx) >= Length(name) then Continue;
    if strEquCI1251(Copy(name, 1, Length(pfx)), pfx) then maxpfx := pfx;
  end;

  if Length(maxpfx) > 0 then
  begin
    Result := name;
    Delete(Result, 1, Length(maxpfx));
  end;
end;

function e_GetResourcePath (dirs: SSArray; path: AnsiString; defWad: AnsiString): AnsiString;
var
  diskName, fileName: AnsiString;
begin
  if debug_e_res then
    e_LogWritefln('e_GetResourcePath %s (%s)', [path, defWad]);

  Assert(Length(dirs) > 0);
  Assert(path <> '');
  Assert(defWad <> '');
  diskName := g_ExtractWadName(path);
  fileName := g_ExtractFilePathName(path);

  if diskName = ''
    then diskName := defWad
    else diskName := e_FindWad(dirs, diskName);

  if diskName = ''
    then Result := ''
    else Result := diskName + ':\' + fileName;

  if debug_e_res then
    e_LogWritefln('  this>>> %s', [Result]);
end;

function e_FindFirst (dirs: SSArray; name: AnsiString; attr: LongInt; out Rslt: TSearchRec): LongInt;
var
  i: Integer;
  dir: AnsiString;
begin
  if debug_e_res then
    e_LogWritefln('e_FindFirst %s', [name]);

  Assert(dirs <> nil);
  Assert(e_IsValidResourceName(name));
  i := High(dirs);
  Result := -1;

  while (i >= 0) and (Result <> 0) do
  begin
    dir := dirs[i] + DirectorySeparator + name;
    Result := FindFirst(dir, attr, Rslt);
    if debug_e_res then
      e_LogWritefln('  %s: %s -- %s', [i, dir, Result]);
    i -= 1;
  end;
end;

// k8: sorry. i know that this sux, but checking directory access rights is unreliable (unportable).
function e_CanCreateFilesAt (dir: AnsiString): Boolean;
var
  f: Integer;
  st: TStream = nil;
  sr: TSearchRec;
  fn: AnsiString;
begin
  Result := False;
  for f := 0 to $7fffffff do
  begin
    fn := Format('%s/$$$temptest$$$_%d.$$$%d$$$', [dir, f, f]);
    if FindFirst(fn, faAnyFile, sr) = 0 then
    begin
      FindClose(sr);
      Continue;
    end;
    FindClose(sr);

    try
      st := TFileStream.Create(fn, fmCreate);
    except // sorry
      st := nil; // just in case
    end;

    if st <> nil then
    begin
      st.Destroy();
      try DeleteFile(fn); except end;
      Result := True;
    end;
    Exit;
  end;
end;

function e_GetWriteableDir (dirs: SSArray; required: Boolean): AnsiString;
var
  f: Integer;
begin
  Assert(Length(dirs) > 0);
  Result := '';

  if writeableDirs <> nil then
  begin
    for f := High(dirs) downto Low(dirs) do
    begin
      if writeableDirs.get(dirs[f], Result) then
      begin
        //writeln('*** KNOWN WRITEABLE DIR: "', Result, '"');
        Exit;
      end;
    end;
  end;

  for f := High(dirs) downto Low(dirs) do
  begin
    try
      if ForceDirectories(dirs[f]) then
      begin
        Result := dirs[f];
        if findFileCI(Result, True) then
        begin
          if e_CanCreateFilesAt(Result) then
          begin
            if not assigned(writeableDirs) then
              writeableDirs := THashStrCIStr.Create();
            writeableDirs.put(dirs[f], Result);
            //writeln('*** NEW WRITEABLE DIR: "', Result, '" ("', dirs[f], '"); rq=', required);
            Exit;
          end;
        end;
      end;
    except // sorry
    end;
  end;

  if required then
    Raise Exception.Create(Format('unable to create directory "%s"', [dirs[High(dirs)]]));
  Result := '';
end;

finalization
  writeableDirs.Free();
end.
