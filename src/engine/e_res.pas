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
{$I ../shared/a_modes.inc}
unit e_res;

interface

  uses SysUtils, Utils, Classes;

  var
    debug_e_res: Boolean;

  {-------------------------------------------}
  {--- insert separator beetwin a and b    ---}
  {--- result are correct if (a or b) = '' ---}
  {--- - - - - - - - - - - - - - - - - - - ---}
  function e_CatPath (a, b: AnsiString): AnsiString;

  {--- remove last entry from path ---}
  function e_UpperDir (path: AnsiString): AnsiString;

  {--- not absolute and have no relative dirs ---}
  function e_IsValidResourceName (name: AnsiString): Boolean;

  {-----------------------------------------------------------------------}
  {--- try to open/create file in one dir from `dirs` in reverse order ---}
  {--- e_OpenResourceRW tries to create if not exists                  ---}
  {--- create dirs if not exists                                       ---}
  {--- result <> nil, throws exceptions on errors                      ---}
  {--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ---}
  function e_CreateResource (dirs: SSArray; name: AnsiString): TStream;
  function e_OpenResourceRO (dirs: SSArray; name: AnsiString): TStream;
  function e_OpenResourceRW (dirs: SSArray; name: AnsiString): TStream;

  {--- same as shared/utils ---}
  function e_FindResource (dirs: SSArray; var name: AnsiString; nameIsDir: Boolean = false): Boolean;
  function e_FindWad (dirs: SSArray; name: AnsiString): AnsiString;

  {--- returns relative wad name; never empty string ---}
  function e_FindWadRel (dirs: SSArray; name: AnsiString): AnsiString;

  {--- prepend dirs to 'disk.wad:\file'. if empty disk string then prepend defWad  ---}
  {--- return empty string if error occured or 'path/to/disk.wad:\file' on success ---}
  function e_GetResourcePath (dirs: SSArray; path: AnsiString; defWad: AnsiString): AnsiString;

  {--- same as SysUtils.FinFirst ---}
  function e_FindFirst (dirs: SSArray; name: AnsiString; attr: LongInt; out Rslt: TSearchRec): LongInt;

  {--- try to get a writeable directory from list, throws if no one directory created ---}
  {--- (unless `required` is `false`: in this case, returns empty string) ---}
  {--- creates all necessary subdirs, if it can ---}
  function e_GetWriteableDir (dirs: SSArray; required: Boolean=true): AnsiString;

  function e_CanCreateFilesAt (dir: AnsiString): Boolean;

implementation

  uses WadReader, e_log, hashtable;

  type
    SpawnProc = function (pathname: AnsiString): Tstream;

  var
    writeableDirs: THashStrCIStr = nil;


  function e_UpperDir (path: AnsiString): AnsiString;
    var i: Integer;
  begin
    i := High(path);  // consider possible cases: '\a\', '\a', '\abc\'
    while (i >= 1) and (path[i] <> '/') and (path[i] <> '\') do Dec(i);
    result := Copy(path, 1, i-1)  // exclude the trailing separator
  end;

  function HasRelativeDirs (name: AnsiString): Boolean;
    var i: Integer; ch: Char;
  begin
    i := 1;
    result := false;
    while (result = false) and (name[i] <> #0) do
    begin
      ch := name[i];
      if (ch = '/') or (ch = '\') then
      begin
        Inc(i);
        if name[i] = '.' then
        begin
          Inc(i);
          if name[i] = '.' then
          begin
            Inc(i);
            ch := name[i];
            result := (ch = #0) or (ch = '/') or (ch = '\')
          end
        end
      end
      else
      begin
        Inc(i)
      end
    end
  end;

  function HasAbsoluteDirs (name: AnsiString): Boolean;
  begin
    result := (name = '') or (name[1] = '/') or (name[1] = '\')
  end;

  function e_IsValidResourceName (name: AnsiString): Boolean;
  begin
    result := (HasAbsoluteDirs(name) = false) and (HasRelativeDirs(name) = false)
  end;

  function SpawnStream (dirs: SSArray; name: AnsiString; p: SpawnProc; createNewDir: Boolean): TStream;
    var i: Integer;
  begin
    result := nil;
    assert(dirs <> nil);
    assert(e_IsValidResourceName(name));
    i := High(dirs);
    while (i >= 0) and (result = nil) do
    begin
      try
        if debug_e_res then
          e_LogWritefln('  %s', [dirs[i]]);
        if (createNewDir = false) or (ForceDirectories(dirs[i]) = true) then
          result := p(e_CatPath(dirs[i], name))
      finally
        Dec(i)
      end
    end
  end;

  function e_CreateResource (dirs: SSArray; name: AnsiString): TStream;
  begin
    if debug_e_res then
      e_LogWritefln('e_CreateResource %s', [name]);
    result := SpawnStream(dirs, name, @createDiskFile, true);
    if result = nil then
      raise Exception.Create('can''t create resource "' + name + '"');
  end;

  function e_OpenResourceRO (dirs: SSArray; name: AnsiString): TStream;
  begin
    if debug_e_res then
      e_LogWritefln('e_OpenResourceRO %s', [name]);
    result := SpawnStream(dirs, name, @openDiskFileRO, false);
    if result = nil then
      raise EFileNotFoundException.Create('can''t open resource "' + name + '"')
  end;

  function e_OpenResourceRW (dirs: SSArray; name: AnsiString): TStream;
  begin
    if debug_e_res then
      e_LogWritefln('e_OpenResourceRW %s', [name]);
    result := SpawnStream(dirs, name, @openDiskFileRW, true);
    if result = nil then
      raise Exception.Create('can''t create resource "' + name + '"')
  end;

  function e_CatPath (a, b: AnsiString): AnsiString;
  begin
    if a = '' then
      result := b
    else if b = '' then
      result := a
    else
      result := a + '/' + b
  end;

  function e_FindResource (dirs: SSArray; var name: AnsiString; nameIsDir: Boolean = false): Boolean;
    var i: Integer; dir: AnsiString;
  begin
    if debug_e_res then
      e_LogWritefln('e_FindResource %s (%s)', [name, nameIsDir]);
    result := false;
    assert(dirs <> nil);
    assert(e_IsValidResourceName(name));
    i := High(dirs); dir := name;
    while (i >= 0) and (result = false) do
    begin
      dir := e_CatPath(dirs[i], name);
      result := findFileCI(dir, nameIsDir);
      if debug_e_res then
        e_LogWritefln('  %s -> %s', [dir, result]);
      Dec(i)
    end;
    if result = true then
      name := dir;
    if debug_e_res then
      e_LogWritefln('  result = %s (%s)', [name, result]);
  end;

  function e_FindWad (dirs: SSArray; name: AnsiString): AnsiString;
    var i: Integer;
  begin
    if debug_e_res then
      e_LogWritefln('e_FindWad "%s"', [name]);
    result := '';
    assert(dirs <> nil);
    assert(e_IsValidResourceName(name));
    i := High(dirs);
    while (i >= 0) and (result = '') do
    begin
      result := findDiskWad(dirs[i] + DirectorySeparator + name);
      if debug_e_res then
        e_LogWritefln('  %s -> %s', [dirs[i] + DirectorySeparator + name, result]);
      Dec(i)
    end
  end;

  function e_FindWadRel (dirs: SSArray; name: AnsiString): AnsiString;
  var
    s: AnsiString;
    maxpfx: AnsiString = '';
    pfx: AnsiString;
  begin
    result := name;
    if not findFileCI(name) then exit;
    for s in dirs do
    begin
      if (length(s) = 0) then continue;
      if (length(name) <= length(s)) then continue;
      if (length(s) < length(maxpfx)) then continue;
      pfx := s;
      if not findFileCI(pfx, true) then continue;
      if (pfx[length(pfx)] <> '/') and (pfx[length(pfx)] <> '\') then pfx := pfx+'/';
      if (length(pfx)+1 > length(name)) then continue;
      if (strEquCI1251(copy(name, 1, length(pfx)), pfx)) then maxpfx := pfx;
    end;
    if (length(maxpfx) > 0) then
    begin
      result := name;
      Delete(result, 1, length(maxpfx));
    end;
  end;

  function e_GetResourcePath (dirs: SSArray; path: AnsiString; defWad: AnsiString): AnsiString;
    var diskName, fileName: AnsiString;
  begin
    if debug_e_res then
      e_LogWritefln('e_GetResourcePath %s (%s)', [path, defWad]);
    assert(length(dirs) > 0);
    assert(path <> '');
    assert(defWad <> '');
    diskName := g_ExtractWadName(path);
    fileName := g_ExtractFilePathName(path);
    if diskName = '' then diskName := defWad else diskName := e_FindWad(dirs, diskName);
    if diskName = '' then result := '' else result := diskName + ':\' + fileName;
    if debug_e_res then
      e_LogWritefln('  this>>> %s', [result]);
  end;

  function e_FindFirst (dirs: SSArray; name: AnsiString; attr: LongInt; out Rslt: TSearchRec): LongInt;
    var i: Integer; dir: AnsiString;
  begin
    if debug_e_res then
      e_LogWritefln('e_FindFirst %s', [name]);
    assert(dirs <> nil);
    assert(e_IsValidResourceName(name));
    i := High(dirs); result := -1;
    while (i >= 0) and (result <> 0) do
    begin
      dir := dirs[i] + DirectorySeparator + name;
      result := FindFirst(dir, attr, Rslt);
      if debug_e_res then
        e_LogWritefln('  %s: %s -- %s', [i, dir, result]);
      Dec(i);
    end
  end;

  // k8: sorry. i know that this sux, but checking directory access rights is unreliable (unportable).
  function e_CanCreateFilesAt (dir: AnsiString): Boolean;
  var
    f: Integer;
    st: TStream = nil;
    sr: TSearchRec;
    fn: AnsiString;
  begin
    result := false;
    for f := 0 to $7fffffff do
    begin
      fn := Format('%s/$$$temptest$$$_%d.$$$%d$$$', [dir, f, f]);
      if (FindFirst(fn, faAnyFile, sr) = 0) then
      begin
        FindClose(sr);
        continue;
      end;
      FindClose(sr);
      try
        st := TFileStream.Create(fn, fmCreate);
      except // sorry
        st := nil; // just in case
      end;
      if assigned(st) then
      begin
        st.Free();
        try DeleteFile(fn); except end;
        result := true;
      end;
      exit;
    end;
  end;

  function e_GetWriteableDir (dirs: SSArray; required: Boolean=true): AnsiString;
  var
    f: Integer;
  begin
    assert(length(dirs) > 0);
    result := '';
    if assigned(writeableDirs) then
    begin
      for f := High(dirs) downto Low(dirs) do
      begin
        if (writeableDirs.get(dirs[f], result)) then
        begin
          //writeln('*** KNOWN WRITEABLE DIR: "', result, '"');
          exit;
        end;
      end;
    end;
    for f := High(dirs) downto Low(dirs) do
    begin
      try
        if ForceDirectories(dirs[f]) then
        begin
          result := dirs[f];
          if (findFileCI(result, true)) then
          begin
            if e_CanCreateFilesAt(result) then
            begin
              if not assigned(writeableDirs) then writeableDirs := THashStrCIStr.Create();
              writeableDirs.put(dirs[f], result);
              //writeln('*** NEW WRITEABLE DIR: "', result, '" ("', dirs[f], '"); rq=', required);
              exit;
            end;
          end;
        end;
      except // sorry
      end;
    end;
    if required then raise Exception.Create(Format('unable to create directory "%s"', [dirs[High(dirs)]]));
    result := '';
  end;

end.
