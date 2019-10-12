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

//const DOWNLOAD_DIR = 'downloads';

var
  replacements: THashStrStr = nil;


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


function g_Res_SearchResWad (asMap: Boolean; fname: AnsiString; const resMd5: TMD5Digest): AnsiString;
begin
  result := '';
  //if not assigned(scannedDirs) then scannedDirs := THashStrInt.Create();
  if (asMap) then
  begin
    result := scanDir(GameDir+'/maps', ExtractFileName(fname), resMd5);
  end
  else
  begin
    result := scanDir(GameDir+'/wads', ExtractFileName(fname), resMd5);
  end;
end;


function g_Res_SearchSameWAD (const path, filename: AnsiString; const resMd5: TMD5Digest): AnsiString;
begin
  result := scanDir(path, filename, resMd5);
end;


function g_Res_DownloadMapWAD (FileName: AnsiString; const mapHash: TMD5Digest): AnsiString;
var
  tf: TNetFileTransfer;
  resList: TStringList;
  f, res: Integer;
  strm: TStream;
  fname: AnsiString;
  wadname: AnsiString;
  md5: TMD5Digest;
begin
  //SetLength(mapData.ExternalResources, 0);
  result := '';
  g_Res_ClearReplacementWads();
  g_Res_received_map_start := false;

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
      try
        CreateDir(GameDir+'/maps/downloads');
      except
      end;
      fname := GameDir+'/maps/downloads/'+FileName;
      try
        strm := openDiskFileRW(fname);
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
        try
          CreateDir(GameDir+'/wads/downloads');
        except
        end;
        fname := GameDir+'/wads/downloads/'+tf.diskName;
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
        g_Res_PutReplacementWad(tf.diskName, fname);
      end;
    end;
  finally
    resList.Free;
  end;
end;


end.
