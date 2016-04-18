unit g_res_downloader;

interface

uses sysutils, Classes, md5, g_net, g_netmsg, g_console, g_main, e_log;

function g_Res_SearchSameWAD(const path, filename: string; const resMd5: TMD5Digest): string;
function g_Res_DownloadWAD(const FileName: string): string;

implementation

uses g_language, sfs, utils, wadreader;

const DOWNLOAD_DIR = 'downloads';

procedure FindFiles(const dirName, filename: string; var files: TStringList);
var
  searchResult: TSearchRec;
begin
  if FindFirst(dirName+'/*', faAnyFile, searchResult) = 0 then
  begin
    try
      repeat
        if (searchResult.Attr and faDirectory) = 0 then
        begin
          if StrEquCI1251(searchResult.Name, filename) then
          begin
            files.Add(dirName+'/'+filename);
            Exit;
          end;
        end
        else if (searchResult.Name <> '.') and (searchResult.Name <> '..') then
          FindFiles(IncludeTrailingPathDelimiter(dirName)+searchResult.Name, filename, files);
      until FindNext(searchResult) <> 0;
    finally
      FindClose(searchResult);
    end;
  end;
end;

function CompareFileHash(const filename: string; const resMd5: TMD5Digest): Boolean;
var
  gResHash: TMD5Digest;
  fname: string;
begin
  fname := findDiskWad(filename);
  if length(fname) = 0 then begin result := false; exit; end;
  gResHash := MD5File(fname);
  Result := MD5Match(gResHash, resMd5);
end;

function CheckFileHash(const path, filename: string; const resMd5: TMD5Digest): Boolean;
var
  fname: string;
begin
  fname := findDiskWad(path+filename);
  if length(fname) = 0 then begin result := false; exit; end;
  Result := FileExists(fname) and CompareFileHash(fname, resMd5);
end;

function g_Res_SearchSameWAD(const path, filename: string; const resMd5: TMD5Digest): string;
var
  res: string;
  files: TStringList;
  i: Integer;
begin
  Result := '';

  if CheckFileHash(path, filename, resMd5) then
  begin
    Result := path + filename;
    Exit;
  end;

  files := TStringList.Create;

  FindFiles(path, filename, files);
  for i := 0 to files.Count - 1 do
  begin
    res := files.Strings[i];
    if CompareFileHash(res, resMd5) then
    begin
      Result := res;
      Break;
    end;
  end;

  files.Free;
end;

function SaveWAD(const path, filename: string; const data: array of Byte): string;
var
  resFile: TStream;
  dpt: string;
begin
  try
    result := path+DOWNLOAD_DIR+'/'+filename;
    dpt := path+DOWNLOAD_DIR;
    if not findFileCI(dpt, true) then CreateDir(dpt);
    resFile := createDiskFile(result);
    resFile.WriteBuffer(data[0], Length(data));
    resFile.Free
  except
    Result := '';
  end;
end;

function g_Res_DownloadWAD(const FileName: string): string;
var
  msgStream: TMemoryStream;
  resStream: TStream;
  mapData: TMapDataMsg;
  i: Integer;
  resData: TResDataMsg;
begin
  SetLength(mapData.ExternalResources, 0);
  g_Console_Add(Format(_lc[I_NET_MAP_DL], [FileName]));
  e_WriteLog('Downloading map `' + FileName + '` from server', MSG_NOTIFY);
  MC_SEND_MapRequest();

  msgStream := g_Net_Wait_Event(NET_MSG_MAP_RESPONSE);
  if msgStream <> nil then
  begin
    mapData := MapDataFromMsgStream(msgStream);
    msgStream.Free;
  end;

  for i := 0 to High(mapData.ExternalResources) do
  begin
    if not CheckFileHash(GameDir + '/wads/',
                         mapData.ExternalResources[i].Name,
                         mapData.ExternalResources[i].md5) then
    begin
      g_Console_Add(Format(_lc[I_NET_WAD_DL],
                           [mapData.ExternalResources[i].Name]));
      e_WriteLog('Downloading Wad `' + mapData.ExternalResources[i].Name +
                 '` from server', MSG_NOTIFY);
      MC_SEND_ResRequest(mapData.ExternalResources[i].Name);

      msgStream := g_Net_Wait_Event(NET_MSG_RES_RESPONSE);
      resData := ResDataFromMsgStream(msgStream);

      resStream := createDiskFile(GameDir+'/wads/'+mapData.ExternalResources[i].Name);
      resStream.WriteBuffer(resData.FileData[0], resData.FileSize);

      resData.FileData := nil;
      resStream.Free;
      msgStream.Free;
    end;
  end;

  Result := SaveWAD(MapsDir, ExtractFileName(FileName), mapData.FileData);
end;

end.
