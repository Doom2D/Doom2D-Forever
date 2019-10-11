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

function g_Res_SearchSameWAD(const path, filename: AnsiString; const resMd5: TMD5Digest): AnsiString;

// download map wad from server (if necessary)
// download all required map resource wads too
// returns name of the map wad (relative to mapdir), or empty string on error
function g_Res_DownloadMapWAD (FileName: AnsiString; const mapHash: TMD5Digest): AnsiString;

// call this before downloading a new map from a server
procedure g_Res_ClearReplacementWads ();
// returns original name, or replacement name
function g_Res_FindReplacementWad (oldname: AnsiString): AnsiString;
procedure g_Res_PutReplacementWad (oldname: AnsiString; newDiskName: AnsiString);


implementation

uses g_language, sfs, utils, wadreader, g_game, hashtable;

const DOWNLOAD_DIR = 'downloads';

type
  TFileInfo = record
    diskName: AnsiString; // lowercased
    baseName: AnsiString; // lowercased
    md5: TMD5Digest;
    md5valid: Boolean;
    nextBaseNameIndex: Integer;
  end;

var
  knownFiles: array of TFileInfo;
  knownHash: THashStrInt = nil; // key: base name; value: index
  scannedDirs: THashStrInt = nil; // key: lowercased dir name
  replacements: THashStrStr = nil;


function findKnownFile (diskName: AnsiString): Integer;
var
  idx: Integer;
  baseName: AnsiString;
begin
  result := -1;
  if not assigned(knownHash) then exit;
  if (length(diskName) = 0) then exit;
  baseName := toLowerCase1251(ExtractFileName(diskName));
  if (not knownHash.get(baseName, idx)) then exit;
  if (idx < 0) or (idx >= length(knownFiles)) then raise Exception.Create('wutafuck?');
  while (idx >= 0) do
  begin
    if (strEquCI1251(knownFiles[idx].diskName, diskName)) then begin result := idx; exit; end; // i found her!
    idx := knownFiles[idx].nextBaseNameIndex;
  end;
end;


function addKnownFile (diskName: AnsiString): Integer;
var
  idx: Integer;
  lastIdx: Integer = -1;
  baseName: AnsiString;
  fi: ^TFileInfo;
begin
  result := -1;
  if not assigned(knownHash) then knownHash := THashStrInt.Create();
  if (length(diskName) = 0) then exit;
  baseName := toLowerCase1251(ExtractFileName(diskName));
  if (length(baseName) = 0) then exit;
  // check if we already have this file
  if (knownHash.get(baseName, idx)) then
  begin
    if (idx < 0) or (idx >= length(knownFiles)) then raise Exception.Create('wutafuck?');
    while (idx >= 0) do
    begin
      if (strEquCI1251(knownFiles[idx].diskName, diskName)) then
      begin
        // already here
        result := idx;
        exit;
      end;
      lastIdx := idx;
      idx := knownFiles[idx].nextBaseNameIndex;
    end;
  end;
  // this file is not there, append it
  idx := length(knownFiles);
  result := idx;
  SetLength(knownFiles, idx+1); // sorry
  fi := @knownFiles[idx];
  fi.diskName := diskName;
  fi.baseName := baseName;
  fi.md5valid := false;
  fi.nextBaseNameIndex := -1;
  if (lastIdx < 0) then
  begin
    // totally new one
    knownHash.put(baseName, idx);
  end
  else
  begin
    knownFiles[lastIdx].nextBaseNameIndex := idx;
  end;
end;


function getKnownFileWithMD5 (diskDir: AnsiString; baseName: AnsiString; const md5: TMD5Digest): AnsiString;
var
  idx: Integer;
begin
  result := '';
  if not assigned(knownHash) then exit;
  if (not knownHash.get(toLowerCase1251(baseName), idx)) then exit;
  if (idx < 0) or (idx >= length(knownFiles)) then raise Exception.Create('wutafuck?');
  while (idx >= 0) do
  begin
    if (strEquCI1251(knownFiles[idx].diskName, IncludeTrailingPathDelimiter(diskDir)+baseName)) then
    begin
      if (not knownFiles[idx].md5valid) then
      begin
        knownFiles[idx].md5 := MD5File(knownFiles[idx].diskName);
        knownFiles[idx].md5valid := true;
      end;
      if (MD5Match(knownFiles[idx].md5, md5)) then
      begin
        result := knownFiles[idx].diskName;
        exit;
      end;
    end;
    idx := knownFiles[idx].nextBaseNameIndex;
  end;
end;


// call this before downloading a new map from a server
procedure g_Res_ClearReplacementWads ();
begin
  if assigned(replacements) then replacements.clear();
  e_LogWriteln('cleared replacement wads');
end;


// returns original name, or replacement name
function g_Res_FindReplacementWad (oldname: AnsiString): AnsiString;
var
  fn: AnsiString;
begin
  result := oldname;
  if not assigned(replacements) then exit;
  if (replacements.get(toLowerCase1251(ExtractFileName(oldname)), fn)) then result := fn;
end;


procedure g_Res_PutReplacementWad (oldname: AnsiString; newDiskName: AnsiString);
begin
  e_LogWritefln('adding replacement wad: oldname=%s; newname=%s', [oldname, newDiskName]);
  if not assigned(replacements) then replacements := THashStrStr.Create();
  replacements.put(toLowerCase1251(oldname), newDiskName);
end;


procedure scanDir (const dirName: AnsiString; calcMD5: Boolean);
var
  searchResult: TSearchRec;
  dfn: AnsiString;
  idx: Integer;
begin
  if not assigned(scannedDirs) then scannedDirs := THashStrInt.Create();
  dfn := toLowerCase1251(IncludeTrailingPathDelimiter(dirName));
  if scannedDirs.has(dfn) then exit;
  scannedDirs.put(dfn, 42);

  if (FindFirst(dirName+'/*', faAnyFile, searchResult) <> 0) then exit;
  try
    repeat
      if (searchResult.Attr and faDirectory) = 0 then
      begin
        dfn := dirName+'/'+searchResult.Name;
        idx := addKnownFile(dfn);
        if (calcMD5) and (idx >= 0) then
        begin
          if (not knownFiles[idx].md5valid) then
          begin
            knownFiles[idx].md5 := MD5File(knownFiles[idx].diskName);
            knownFiles[idx].md5valid := true;
          end;
        end;
      end
      else if (searchResult.Name <> '.') and (searchResult.Name <> '..') then
      begin
        scanDir(IncludeTrailingPathDelimiter(dirName)+searchResult.Name, calcMD5);
      end;
    until (FindNext(searchResult) <> 0);
  finally
    FindClose(searchResult);
  end;
end;


function CompareFileHash(const filename: AnsiString; const resMd5: TMD5Digest): Boolean;
var
  gResHash: TMD5Digest;
  fname: AnsiString;
begin
  fname := findDiskWad(filename);
  if length(fname) = 0 then begin result := false; exit; end;
  gResHash := MD5File(fname);
  Result := MD5Match(gResHash, resMd5);
end;

function CheckFileHash(const path, filename: AnsiString; const resMd5: TMD5Digest): Boolean;
var
  fname: AnsiString;
begin
  fname := findDiskWad(path+filename);
  if length(fname) = 0 then begin result := false; exit; end;
  Result := FileExists(fname) and CompareFileHash(fname, resMd5);
end;


function g_Res_SearchResWad (asMap: Boolean; fname: AnsiString; const resMd5: TMD5Digest): AnsiString;
var
  f: Integer;
begin
  result := '';
  //if not assigned(scannedDirs) then scannedDirs := THashStrInt.Create();
  if (asMap) then
  begin
    if CheckFileHash(GameDir+'/maps', fname, resMd5) then
    begin
      result := findDiskWad(GameDir+'/maps/'+fname);
      if (length(result) <> 0) then exit;
    end;
    scanDir(GameDir+'/maps/downloads', true);
  end
  else
  begin
    if CheckFileHash(GameDir+'/wads', fname, resMd5) then
    begin
      result := findDiskWad(GameDir+'/wads/'+fname);
      if (length(result) <> 0) then exit;
    end;
    scanDir(GameDir+'/wads/downloads', true);
  end;
  for f := Low(knownFiles) to High(knownFiles) do
  begin
    if (not knownFiles[f].md5valid) then continue;
    if (MD5Match(knownFiles[f].md5, resMd5)) then
    begin
      result := knownFiles[f].diskName;
      exit;
    end;
  end;
  //resStream := createDiskFile(GameDir+'/wads/'+mapData.ExternalResources[i].Name);
end;


function g_Res_SearchSameWAD (const path, filename: AnsiString; const resMd5: TMD5Digest): AnsiString;
begin
  scanDir(path, false);
  result := getKnownFileWithMD5(path, filename, resMd5);
end;


function g_Res_DownloadMapWAD (FileName: AnsiString; const mapHash: TMD5Digest): AnsiString;
var
  tf: TNetFileTransfer;
  resList: TStringList;
  f, res: Integer;
  strm: TStream;
  mmd5: TMD5Digest;
  fname: AnsiString;
  idx: Integer;
  wadname: AnsiString;
begin
  //SetLength(mapData.ExternalResources, 0);
  result := '';
  g_Res_ClearReplacementWads();
  g_Res_received_map_start := false;

  try
    CreateDir(GameDir+'/maps/downloads');
  except
  end;

  try
    CreateDir(GameDir+'/wads/downloads');
  except
  end;

  resList := TStringList.Create();

  try
    g_Console_Add(Format(_lc[I_NET_MAP_DL], [FileName]));
    e_WriteLog('Downloading map `' + FileName + '` from server', TMsgType.Notify);
    g_Game_SetLoadingText(FileName + '...', 0, False);
    //MC_SEND_MapRequest();
    if (not g_Net_SendMapRequest()) then exit;

    FileName := ExtractFileName(FileName);
    if (length(FileName) = 0) then FileName := 'fucked_map_wad.wad';
    res := g_Net_Wait_MapInfo(tf, resList);
    if (res <> 0) then exit;

    // find or download a map
    result := g_Res_SearchResWad(true{asMap}, tf.diskName, mapHash);
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
      fname := GameDir+'/maps/downloads/'+FileName;
      try
        strm := createDiskFile(fname);
      except
        e_WriteLog('cannot create map file `'+FileName+'`', TMsgType.Fatal);
        result := '';
        exit;
      end;
      tf.diskName := fname;
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
      mmd5 := MD5File(fname);
      if (not MD5Match(mmd5, mapHash)) then
      begin
        e_WriteLog('error downloading map file `'+FileName+'` (bad hash)', TMsgType.Fatal);
        result := '';
        exit;
      end;
      idx := addKnownFile(fname);
      if (idx < 0) then
      begin
        e_WriteLog('error downloading map file `'+FileName+'`', TMsgType.Fatal);
        result := '';
        exit;
      end;
      knownFiles[idx].md5 := mmd5;
      knownFiles[idx].md5valid := true;
      result := fname;
    end;

    // download resources
    for f := 0 to resList.Count-1 do
    begin
      res := g_Net_RequestResFileInfo(f, tf);
      if (res <> 0) then begin result := ''; exit; end;
      wadname := g_Res_SearchResWad(false{asMap}, tf.diskName, tf.hash);
      if (length(wadname) <> 0) then
      begin
        // already here
        g_Net_AbortResTransfer(tf);
        g_Res_PutReplacementWad(tf.diskName, wadname);
      end
      else
      begin
        fname := GameDir+'/wads/downloads/'+tf.diskName;
        try
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
        idx := addKnownFile(fname);
        if (idx < 0) then
        begin
          e_WriteLog('error downloading map file `'+FileName+'`', TMsgType.Fatal);
          result := '';
          exit;
        end;
        knownFiles[idx].md5 := tf.hash;
        knownFiles[idx].md5valid := true;
        g_Res_PutReplacementWad(tf.diskName, fname);
      end;
    end;
  finally
    resList.Free;
  end;
end;


end.
