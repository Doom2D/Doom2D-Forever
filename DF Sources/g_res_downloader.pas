unit g_res_downloader;

interface

uses sysutils, Classes, md5asm, g_net, g_netmsg, g_console, g_main, e_log;

function g_Res_SearchSameWAD(const path, filename: string; const resMd5: TMD5Digest): string;
function g_Res_DownloadWAD(const FileName: string): string;

implementation

uses g_language;

const DOWNLOAD_DIR = 'downloads';

procedure FindFiles(const dirName, filename: string; var files: TStringList);
var
  searchResult: TSearchRec;
begin
  if FindFirst(dirName+'\*', faAnyFile, searchResult) = 0 then
  begin
    try
      repeat
        if (searchResult.Attr and faDirectory) = 0 then
        begin
          if searchResult.Name = filename then
          begin
            files.Add(dirName+'\'+filename);
            Exit;
          end;
        end
        else if (searchResult.Name <> '.') and (searchResult.Name <> '..') then
          FindFiles(IncludeTrailingPathDelimiter(dirName)+searchResult.Name,
                    filename, files);
      until FindNext(searchResult) <> 0;
    finally
      FindClose(searchResult);
    end;
  end;
end;

function CompareFileHash(const filename: string; const resMd5: TMD5Digest): Boolean;
var
  gResHash: TMD5Digest;
begin
  gResHash := MD5File(filename);
  Result := MD5Compare(gResHash, resMd5);
end;

function CheckFileHash(const path, filename: string; const resMd5: TMD5Digest): Boolean;
begin
  Result := FileExists(path + filename) and CompareFileHash(path + filename, resMd5);
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
  resFile: TFileStream;
begin
  try
    Result := path + DOWNLOAD_DIR + '\' + filename;
    if not DirectoryExists(path + DOWNLOAD_DIR) then
    begin
      CreateDir(path + DOWNLOAD_DIR);
    end;
    resFile := TFileStream.Create(Result, fmCreate);
    resFile.WriteBuffer(data[0], Length(data));
    resFile.Free
  except
    Result := '';
  end;
end;

function g_Res_DownloadWAD(const FileName: string): string;
var
  msgStream: TMemoryStream;
  resStream: TFileStream;
  mapData: TMapDataMsg;
  i: Integer;
  resData: TResDataMsg;
begin
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
    if not CheckFileHash(GameDir + '\wads\',
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

      resStream := TFileStream.Create(GameDir+'\wads\'+
                                      mapData.ExternalResources[i].Name,
                                      fmCreate);
      resStream.WriteBuffer(resData.FileData[0], resData.FileSize);

      resData.FileData := nil;
      resStream.Free;
      msgStream.Free;
    end;
  end;

  Result := SaveWAD(MapsDir, FileName, mapData.FileData);
end;

end.
