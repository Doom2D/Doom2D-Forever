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
unit g_res_downloader;

interface

uses sysutils, Classes, md5, g_net, g_netmsg, g_console, g_main, e_log;


// download map wad from server (if necessary)
// download all required map resource wads too
// registers all required replacement wads
// returns name of the map wad (relative to mapdir), or empty string on error
function g_Res_DownloadMapWAD (FileName: AnsiString; const mapHash: TMD5Digest): AnsiString;

// returns original name, or replacement name
function g_Res_FindReplacementWad (oldname: AnsiString): AnsiString;

// call this somewhere in startup sequence
procedure g_Res_CreateDatabases ();


implementation

uses g_language, sfs, utils, wadreader, g_game, hashtable, fhashdb;

var
  // cvars
  g_res_ignore_names: AnsiString = 'standart;shrshade';
  g_res_ignore_enabled: Boolean = true;
  g_res_save_databases: Boolean = true;
  // other vars
  replacements: THashStrStr = nil;
  knownMaps: TFileHashDB = nil;
  knownRes: TFileHashDB = nil;
  saveDBsToDiskEnabled: Boolean = false; // this will be set to `true` if initial database saving succeed


//==========================================================================
//
//  saveDatabases
//
//==========================================================================
procedure saveDatabases (saveMap, saveRes: Boolean);
var
  err: Boolean;
  st: TStream;
begin
  if (not saveDBsToDiskEnabled) or (not g_res_save_databases) then exit;
  // rescan dirs
  // save map database
  if (saveMap) then
  begin
    err := true;
    st := nil;
    try
      st := createDiskFile(GameDir+'/data/maphash.db');
      knownMaps.saveTo(st);
      err := false;
    except
    end;
    st.Free;
    if (err) then begin saveDBsToDiskEnabled := false; e_LogWriteln('cannot write map database, disk refresh disabled'); exit; end;
  end;
  // save resource database
  if (saveRes) then
  begin
    err := true;
    st := nil;
    try
      st := createDiskFile(GameDir+'/data/reshash.db');
      knownRes.saveTo(st);
      err := false;
    except
    end;
    st.Free;
    if (err) then begin saveDBsToDiskEnabled := false; e_LogWriteln('cannot write resource database, disk refresh disabled'); exit; end;
  end;
end;


//==========================================================================
//
//  g_Res_CreateDatabases
//
//==========================================================================
procedure g_Res_CreateDatabases ();
var
  st: TStream;
begin
  // create and load a know map database, if necessary
  knownMaps.Free;
  knownMaps := TFileHashDB.Create(GameDir+'/maps/');
  knownRes := TFileHashDB.Create(GameDir+'/wads/');
  saveDBsToDiskEnabled := true;
  // load map database
  st := nil;
  try
    st := openDiskFileRO(GameDir+'/data/maphash.db');
    knownMaps.loadFrom(st);
    e_LogWriteln('loaded map database');
  except
  end;
  st.Free;
  // load resource database
  st := nil;
  try
    st := openDiskFileRO(GameDir+'/data/reshash.db');
    knownRes.loadFrom(st);
    e_LogWriteln('loaded resource database');
  except
  end;
  st.Free;
  // rescan dirs
  e_LogWriteln('refreshing map database');
  knownMaps.scanFiles();
  e_LogWriteln('refreshing resource database');
  knownRes.scanFiles();
  // save databases
  saveDatabases(true, true);
end;


//==========================================================================
//
//  getWord
//
//  get next word from a string
//  words are delimited with ';'
//  ignores leading and trailing spaces
//  returns empty string if there are no more words
//
//==========================================================================
function getWord (var list: AnsiString): AnsiString;
var
  pos: Integer;
begin
  result := '';
  while (length(list) > 0) do
  begin
    if (ord(list[1]) <= 32) or (list[1] = ';') or (list[1] = ':') then begin Delete(list, 1, 1); continue; end;
    pos := 1;
    while (pos <= length(list)) and (list[pos] <> ';') and (list[pos] <> ':') do Inc(pos);
    result := Copy(list, 1, pos-1);
    Delete(list, 1, pos);
    while (length(result) > 0) and (ord(result[length(result)]) <= 32) do Delete(result, length(result), 1);
    if (length(result) > 0) then exit;
  end;
end;


//==========================================================================
//
//  isIgnoredResWad
//
//  checks if the given resource wad can be ignored
//
//  FIXME: preparse name list?
//
//==========================================================================
function isIgnoredResWad (fname: AnsiString): Boolean;
var
  list: AnsiString;
  name: AnsiString;
begin
  result := false;
  if (not g_res_ignore_enabled) then exit;
  fname := forceFilenameExt(ExtractFileName(fname), '');
  list := g_res_ignore_names;
  name := getWord(list);
  while (length(name) > 0) do
  begin
    name := forceFilenameExt(name, '');
    //writeln('*** name=[', name, ']; fname=[', fname, ']');
    if (StrEquCI1251(name, fname)) then begin result := true; exit; end;
    name := getWord(list);
  end;
end;


//==========================================================================
//
//  clearReplacementWads
//
//  call this before downloading a new map from a server
//
//==========================================================================
procedure clearReplacementWads ();
begin
  if assigned(replacements) then replacements.clear();
  e_LogWriteln('cleared replacement wads');
end;


//==========================================================================
//
//  addReplacementWad
//
//  register new replacement wad
//
//==========================================================================
procedure addReplacementWad (oldname: AnsiString; newDiskName: AnsiString);
begin
  e_LogWritefln('adding replacement wad: oldname=%s; newname=%s', [oldname, newDiskName]);
  if not assigned(replacements) then replacements := THashStrStr.Create();
  replacements.put(toLowerCase1251(oldname), newDiskName);
end;


//==========================================================================
//
//  g_Res_FindReplacementWad
//
//  returns original name, or replacement name
//
//==========================================================================
function g_Res_FindReplacementWad (oldname: AnsiString): AnsiString;
var
  fn: AnsiString;
begin
  //e_LogWritefln('LOOKING for replacement wad for [%s]...', [oldname], TMsgType.Notify);
  result := oldname;
  if not assigned(replacements) then exit;
  if (replacements.get(toLowerCase1251(ExtractFileName(oldname)), fn)) then
  begin
    //e_LogWritefln('found replacement wad for [%s] -> [%s]', [oldname, fn], TMsgType.Notify);
    result := fn;
  end;
end;


//==========================================================================
//
//  scanDir
//
//  look for a wad to match the hash
//  scans subdirs, ignores known wad extensions
//
//  returns found wad disk name, or empty string
//
//==========================================================================
(*
function scanDir (dirName: AnsiString; baseName: AnsiString; const resMd5: TMD5Digest): AnsiString;
var
  searchResult: TSearchRec;
  dfn: AnsiString;
  md5: TMD5Digest;
  dirs: array of AnsiString;
  f: Integer;
begin
  result := '';
  SetLength(dirs, 0);
  if (length(baseName) = 0) then exit;
  dirName := IncludeTrailingPathDelimiter(dirName);
  e_LogWritefln('scanning dir `%s` for file `%s`...', [dirName, baseName]);

  // scan files
  if (FindFirst(dirName+'*', faAnyFile, searchResult) <> 0) then exit;
  try
    repeat
      if ((searchResult.Attr and faDirectory) = 0) then
      begin
        if (isWadNamesEqu(searchResult.Name, baseName)) then
        begin
          dfn := dirName+searchResult.Name;
          if FileExists(dfn) then
          begin
            e_LogWritefln('  found `%s`...', [dfn]);
            md5 := MD5File(dfn);
            if MD5Match(md5, resMd5) then
            begin
              e_LogWritefln('    MATCH `%s`...', [dfn]);
              SetLength(dirs, 0);
              result := dfn;
              exit;
            end;
          end;
        end;
      end
      else
      begin
        if (searchResult.Name <> '.') and (searchResult.Name <> '..') then
        begin
          dfn := dirName+searchResult.Name;
          SetLength(dirs, Length(dirs)+1);
          dirs[length(dirs)-1] := dfn;
        end;
      end;
    until (FindNext(searchResult) <> 0);
  finally
    FindClose(searchResult);
  end;

  // scan subdirs
  for f := 0 to High(dirs) do
  begin
    dfn := dirs[f];
    result := scanDir(dfn, baseName, resMd5);
    if (length(result) <> 0) then begin SetLength(dirs, 0); exit; end;
  end;
  SetLength(dirs, 0);
end;
*)


//==========================================================================
//
//  findExistingMapWadWithHash
//
//  find map or resource wad using its base name and hash
//
//  returns found wad disk name, or empty string
//
//==========================================================================
function findExistingMapWadWithHash (fname: AnsiString; const resMd5: TMD5Digest): AnsiString;
begin
  //result := scanDir(GameDir+'/maps', ExtractFileName(fname), resMd5);
  result := knownMaps.findByHash(resMd5);
  if (length(result) > 0) then
  begin
    result := GameDir+'/maps/'+result;
    if not FileExists(result) then
    begin
      if (knownMaps.scanFiles()) then saveDatabases(true, false);
      result := '';
    end;
  end;
end;


//==========================================================================
//
//  findExistingResWadWithHash
//
//  find map or resource wad using its base name and hash
//
//  returns found wad disk name, or empty string
//
//==========================================================================
function findExistingResWadWithHash (fname: AnsiString; const resMd5: TMD5Digest): AnsiString;
begin
  //result := scanDir(GameDir+'/wads', ExtractFileName(fname), resMd5);
  result := knownRes.findByHash(resMd5);
  if (length(result) > 0) then
  begin
    result := GameDir+'/wads/'+result;
    if not FileExists(result) then
    begin
      if (knownRes.scanFiles()) then saveDatabases(false, true);
      result := '';
    end;
  end;
end;


//==========================================================================
//
//  generateFileName
//
//  generate new file name based on the given one and the hash
//  you can pass files with pathes here too
//
//==========================================================================
function generateFileName (fname: AnsiString; const hash: TMD5Digest): AnsiString;
var
  mds: AnsiString;
  path: AnsiString;
  base: AnsiString;
  ext: AnsiString;
begin
  mds := MD5Print(hash);
  if (length(mds) > 16) then mds := Copy(mds, 1, 16);
  mds := '_'+mds;
  if (length(fname) = 0) then begin result := mds; exit; end;
  path := ExtractFilePath(fname);
  base := ExtractFileName(fname);
  ext := getFilenameExt(base);
  base := forceFilenameExt(base, '');
  if (length(path) > 0) then result := IncludeTrailingPathDelimiter(path) else result := '';
  result := result+base+mds+ext;
end;


//==========================================================================
//
//  g_Res_DownloadMapWAD
//
//  download map wad from server (if necessary)
//  download all required map resource wads too
//  registers all required replacement wads
//
//  returns name of the map wad (relative to mapdir), or empty string on error
//
//==========================================================================
function g_Res_DownloadMapWAD (FileName: AnsiString; const mapHash: TMD5Digest): AnsiString;
var
  tf: TNetFileTransfer;
  resList: TStringList;
  f, res: Integer;
  strm: TStream;
  fname: AnsiString;
  wadname: AnsiString;
  md5: TMD5Digest;
  mapdbUpdated: Boolean = false;
  resdbUpdated: Boolean = false;
begin
  result := '';
  clearReplacementWads();

  resList := TStringList.Create();

  try
    g_Res_received_map_start := 1;
    g_Console_Add(Format(_lc[I_NET_MAP_DL], [FileName]));
    e_WriteLog('Downloading map `' + FileName + '` from server', TMsgType.Notify);
    g_Game_SetLoadingText(FileName + '...', 0, False);
    if (not g_Net_SendMapRequest()) then exit;

    FileName := ExtractFileName(FileName);
    if (length(FileName) = 0) then FileName := 'fucked_map_wad.wad';
    res := g_Net_Wait_MapInfo(tf, resList);
    if (res <> 0) then exit;

    // find or download a map
    result := findExistingMapWadWithHash(tf.diskName, mapHash);
    if (length(result) = 0) then
    begin
      // download map
      res := g_Net_RequestResFileInfo(-1{map}, tf);
      if (res <> 0) then
      begin
        e_LogWriteln('error requesting map wad');
        result := '';
        exit;
      end;
      try
        CreateDir(GameDir+'/maps/downloads');
      except
      end;
      fname := GameDir+'/maps/downloads/'+generateFileName(FileName, mapHash);
      tf.diskName := fname;
      try
        strm := openDiskFileRW(fname);
      except
        e_WriteLog('cannot create map file `'+FileName+'`', TMsgType.Fatal);
        result := '';
        exit;
      end;
      try
        res := g_Net_ReceiveResourceFile(-1{map}, tf, strm);
      except
        e_WriteLog('error downloading map file `'+FileName+'`', TMsgType.Fatal);
        strm.Free;
        result := '';
        exit;
      end;
      strm.Free;
      if (res <> 0) then
      begin
        e_WriteLog('error downloading map file `'+FileName+'`', TMsgType.Fatal);
        result := '';
        exit;
      end;
      // if it was resumed, check md5 and initiate full download if necessary
      if tf.resumed then
      begin
        md5 := MD5File(fname);
        // sorry for pasta, i am asshole
        if not MD5Match(md5, tf.hash) then
        begin
          e_LogWritefln('resuming failed; downloading map `%s` from scratch...', [fname]);
          try
            DeleteFile(fname);
            strm := createDiskFile(fname);
          except
            e_WriteLog('cannot create map file `'+fname+'`', TMsgType.Fatal);
            result := '';
            exit;
          end;
          try
            res := g_Net_ReceiveResourceFile(-1{map}, tf, strm);
          except
            e_WriteLog('error downloading map file `'+FileName+'`', TMsgType.Fatal);
            strm.Free;
            result := '';
            exit;
          end;
          strm.Free;
          if (res <> 0) then
          begin
            e_WriteLog('error downloading map file `'+FileName+'`', TMsgType.Fatal);
            result := '';
            exit;
          end;
        end;
      end;
      if (knownMaps.addWithHash(fname, mapHash)) then mapdbUpdated := true;
      result := fname;
    end;

    // download resources
    for f := 0 to resList.Count-1 do
    begin
      res := g_Net_RequestResFileInfo(f, tf);
      if (res <> 0) then begin result := ''; exit; end;
      if (isIgnoredResWad(tf.diskName)) then
      begin
        // ignored file, abort download
        g_Net_AbortResTransfer(tf);
        e_LogWritefln('ignoring wad resource `%s` by user request', [tf.diskName]);
        continue;
      end;
      wadname := findExistingResWadWithHash(tf.diskName, tf.hash);
      if (length(wadname) <> 0) then
      begin
        // already here
        g_Net_AbortResTransfer(tf);
        addReplacementWad(tf.diskName, wadname);
      end
      else
      begin
        try
          CreateDir(GameDir+'/wads/downloads');
        except
        end;
        fname := GameDir+'/wads/downloads/'+generateFileName(tf.diskName, tf.hash);
        e_LogWritefln('downloading resource `%s` to `%s`...', [tf.diskName, fname]);
        try
          strm := openDiskFileRW(fname);
        except
          e_WriteLog('cannot create resource file `'+fname+'`', TMsgType.Fatal);
          result := '';
          exit;
        end;
        try
          res := g_Net_ReceiveResourceFile(f, tf, strm);
        except
          e_WriteLog('error downloading map file `'+FileName+'`', TMsgType.Fatal);
          strm.Free;
          result := '';
          exit;
        end;
        strm.Free;
        if (res <> 0) then
        begin
          e_WriteLog('error downloading map file `'+FileName+'`', TMsgType.Fatal);
          result := '';
          exit;
        end;
        // if it was resumed, check md5 and initiate full download if necessary
        if tf.resumed then
        begin
          md5 := MD5File(fname);
          // sorry for pasta, i am asshole
          if not MD5Match(md5, tf.hash) then
          begin
            e_LogWritefln('resuming failed; downloading resource `%s` to `%s` from scratch...', [tf.diskName, fname]);
            try
              DeleteFile(fname);
              strm := createDiskFile(fname);
            except
              e_WriteLog('cannot create resource file `'+fname+'`', TMsgType.Fatal);
              result := '';
              exit;
            end;
            try
              res := g_Net_ReceiveResourceFile(f, tf, strm);
            except
              e_WriteLog('error downloading map file `'+FileName+'`', TMsgType.Fatal);
              strm.Free;
              result := '';
              exit;
            end;
            strm.Free;
            if (res <> 0) then
            begin
              e_WriteLog('error downloading map file `'+FileName+'`', TMsgType.Fatal);
              result := '';
              exit;
            end;
          end;
        end;
        addReplacementWad(tf.diskName, fname);
        if (knownRes.addWithHash(fname, tf.hash)) then resdbUpdated := true;
      end;
    end;
  finally
    resList.Free;
    g_Res_received_map_start := 0;
  end;

  if saveDBsToDiskEnabled and (mapdbUpdated or resdbUpdated) then saveDatabases(mapdbUpdated, resdbUpdated);
end;


initialization
  conRegVar('rdl_ignore_names', @g_res_ignore_names, 'list of resource wad names (without extensions) to ignore in dl hash checks', 'dl ignore wads');
  conRegVar('rdl_ignore_enabled', @g_res_ignore_enabled, 'enable dl hash check ignore list', 'dl hash check ignore list active');
  conRegVar('rdl_hashdb_save_enabled', @g_res_save_databases, 'enable saving map/resource hash databases to disk', 'controls storing hash databases to disk');
end.
