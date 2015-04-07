unit g_res_downloader;

interface

uses sysutils, Classes, md5asm, g_net, g_netmsg, g_console, g_main, e_log;

function MapExist(const path, filename: string; const resMd5:TMD5Digest):string;
function g_res_DownloadMapFromServer(const FileName: string):string;

implementation

const DOWNLOAD_DIR = 'downloads';

procedure findFiles(const dirName, filename:string; var files:TStringList);
var
  searchResult: TSearchRec;
begin
  if FindFirst(dirName+'\*', faAnyFile, searchResult)=0 then begin
    try
      repeat
        if (searchResult.Attr and faDirectory)=0 then begin
          if searchResult.Name = filename then begin
            files.Add(dirName+'\'+filename);
            Exit;
          end;
        end else if (searchResult.Name<>'.') and (searchResult.Name<>'..') then begin
          findFiles(IncludeTrailingBackSlash(dirName)+searchResult.Name, filename, files);
        end;
      until (FindNext(searchResult)<>0);
    finally
      FindClose(searchResult);
    end;
  end;
end;

function compareFile(const filename: string; const resMd5:TMD5Digest):Boolean;
var
  gResHash: TMD5Digest;
begin
  gResHash := MD5File(filename);
  Result := MD5Compare(gResHash, resMd5);
end;

function ResourceExists(const path, filename: string; const resMd5:TMD5Digest):Boolean;
begin
  Result := FileExists(path + filename) and compareFile(path + filename, resMd5)
end;

function MapExist(const path, filename: string; const resMd5:TMD5Digest):string;
var
  res: string;
  files: TStringList;
  i: Integer;
begin
  Result := '';

  if ResourceExists(path, filename, resMd5) then
  begin
    Result := path + filename;
    Exit;
  end;

  files := TStringList.Create;

  findFiles(path, filename, files);
  for i:=0 to files.Count-1 do
  begin
    res := files.Strings[i];
    if compareFile(res, resMd5) then
    begin
      Result := res;
      Break;
    end;
  end;

  files.Free;
end;

function saveMap(const path, filename: string; const data: array of Byte):string;
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

function g_res_DownloadMapFromServer(const FileName: string):string;
var
  msgStream: TMemoryStream;
  resStream: TFileStream;
  mapData: TMapDataMsg;
  i: Integer;
  resData: TResDataMsg;
begin
  g_Console_Add('Map `' + FileName +'` not found. Downloading from server...');
  e_WriteLog('Download map `' + FileName + '` from server', MSG_NOTIFY);
  MC_SEND_MapRequest();

  msgStream := g_net_Wait_Event(NET_MSG_MAP_RESPONSE);
  mapData := MapDataFromMsgStream(msgStream);
  msgStream.Free;

  for i:=0 to High(mapData.ExternalResources) do
  begin
    if not ResourceExists(GameDir + '\wads\', mapData.ExternalResources[i].Name, mapData.ExternalResources[i].md5) then
    begin
      g_Console_Add('Wad `' + mapData.ExternalResources[i].Name +'` not found. Downloading from server...');
      e_WriteLog('Download Wad `' + mapData.ExternalResources[i].Name + '` from server', MSG_NOTIFY);
      MC_SEND_ResRequest(mapData.ExternalResources[i].Name);

      msgStream := g_net_Wait_Event(NET_MSG_RES_RESPONSE);
      resData := ResDataFromMsgStream(msgStream);

      resStream := TFileStream.Create(GameDir+'\wads\'+mapData.ExternalResources[i].Name, fmCreate);
      resStream.WriteBuffer(resData.FileData[0], resData.FileSize);

      resData.FileData := nil;
      resStream.Free;
      msgStream.Free;
    end;
  end;

  Result := saveMap(MapsDir, FileName, mapData.FileData);
end;

end.
