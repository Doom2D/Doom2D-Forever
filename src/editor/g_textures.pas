unit g_textures;

{$INCLUDE ../shared/a_modes.inc}

interface

uses LCLIntf, LCLType, LMessages, e_graphics, utils;

function g_SimpleCreateTextureWAD(var ID: DWORD; Resource: string): Boolean;
function g_SimpleCreateTextureWADSize(var ID: DWORD; Resource: string;
                                      X, Y, Width, Height: Word): Boolean;

function g_CreateTextureWAD(TextureName: ShortString; Resource: string; flag: Byte = 0): Boolean;
function g_CreateTextureWADSize(TextureName: ShortString; Resource: string;
                                X, Y, Width, Height: Word; flag: Byte = 0): Boolean;
function g_CreateTextureMemorySize(pData: Pointer; dataLen: Integer; Name: ShortString; X, Y,
                                   Width, Height: Word; flag: Byte = 0): Boolean;

function g_GetTexture(TextureName: ShortString; var ID: DWORD): Boolean;
function g_GetTextureFlagByName(TextureName: ShortString): Byte;
function g_GetTextureFlagByID(ID: DWORD): Byte;
procedure g_GetTextureSizeByName(TextureName: ShortString; var Width, Height: Word);
procedure g_GetTextureSizeByID(ID: DWORD; var Width, Height: Word);

procedure g_DeleteTexture(TextureName: ShortString);
procedure g_DeleteAllTextures();

implementation

uses
  e_log, WADEDITOR, g_basic, SysUtils;

type
  _TTexture = record
   Name: ShortString;
   ID: DWORD;
   Width, Height: Word;
   flag: Byte;
  end;

var
  TexturesArray: array of _TTexture = nil;

function FindTexture: DWORD;
var
  i: integer;
begin
 if TexturesArray <> nil then
 for i := 0 to High(TexturesArray) do
  if TexturesArray[i].Name = '' then
  begin
   Result := i;
   Exit;
  end;

 if TexturesArray = nil then
 begin
  SetLength(TexturesArray, 8);
  Result := 0;
 end
  else
 begin
  Result := High(TexturesArray) + 1;
  SetLength(TexturesArray, Length(TexturesArray) + 8);
 end;
end;

function g_SimpleCreateTextureWAD(var ID: DWORD; Resource: string): Boolean;
var
  WAD: TWADEditor_1;
  FileName,
  SectionName,
  ResourceName: string;
  TextureData: Pointer;
  ResourceLength: Integer;
begin
 Result := False;
 g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

 WAD := TWADEditor_1.Create;
 WAD.ReadFile(FileName);

 if WAD.GetResource(utf2win(SectionName), utf2win(ResourceName), TextureData, ResourceLength) then
 begin
  if e_CreateTextureMem(TextureData, ResourceLength, ID) then Result := True;
  FreeMem(TextureData);
 end
  else
 begin
  e_WriteLog(Format('Error loading texture %s', [Resource]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
 end;
 WAD.Destroy;
end;

function g_CreateTextureMemorySize(pData: Pointer; dataLen: Integer; Name: ShortString; X, Y,
                                   Width, Height: Word; flag: Byte = 0): Boolean;
var
  find_id: DWORD;
begin
  Result := False;
  if pData = nil then
    Exit;

  find_id := FindTexture;

  if not e_CreateTextureMemEx(pData, dataLen, TexturesArray[find_id].ID, X, Y, Width, Height) then
  begin
    FreeMem(pData);
    Exit;
  end;

  TexturesArray[find_id].Width := Width;
  TexturesArray[find_id].Height := Height;
  TexturesArray[find_id].Name := Name;
  TexturesArray[find_id].flag := flag;

  FreeMem(pData);

  Result := True;
end;

function g_CreateTextureWAD(TextureName: ShortString; Resource: string; flag: Byte = 0): Boolean;
var
  WAD: TWADEditor_1;
  FileName,
  SectionName,
  ResourceName: string;
  TextureData: Pointer;
  find_id: DWORD;
  ResourceLength: Integer;
begin
 g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

 find_id := FindTexture;

 WAD := TWADEditor_1.Create;
 WAD.ReadFile(FileName);

 if WAD.GetResource(utf2win(SectionName), utf2win(ResourceName), TextureData, ResourceLength) then
 begin
  Result := e_CreateTextureMem(TextureData, ResourceLength, TexturesArray[find_id].ID);
  FreeMem(TextureData);
  if Result then
  begin
   e_GetTextureSize(TexturesArray[find_id].ID, @TexturesArray[find_id].Width,
                    @TexturesArray[find_id].Height);
   TexturesArray[find_id].Name := TextureName;
   TexturesArray[find_id].flag := flag;
  end;
 end
  else
 begin
  e_WriteLog(Format('Error loading texture %s', [Resource]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
  Result := False;
 end;
 WAD.Destroy;
end;

function g_SimpleCreateTextureWADSize(var ID: DWORD; Resource: string;
                                      X, Y, Width, Height: Word): Boolean;
var
  WAD: TWADEditor_1;
  FileName,
  SectionName,
  ResourceName: String;
  TextureData: Pointer;
  ResourceLength: Integer;
begin
 Result := False;
 g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

 WAD := TWADEditor_1.Create;
 WAD.ReadFile(FileName);

 if WAD.GetResource(utf2win(SectionName), utf2win(ResourceName), TextureData, ResourceLength) then
 begin
  if e_CreateTextureMemEx(TextureData, ResourceLength, ID, X, Y, Width, Height) then Result := True;
  FreeMem(TextureData);
 end
  else
 begin
  e_WriteLog(Format('Error loading texture %s', [Resource]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
 end;
 WAD.Destroy;
end;

function g_CreateTextureWADSize(TextureName: ShortString; Resource: string;
                                X, Y, Width, Height: Word; flag: Byte = 0): Boolean;
var
  WAD: TWADEditor_1;
  FileName,
  SectionName,
  ResourceName: String;
  TextureData: Pointer;
  find_id: DWORD;
  ResourceLength: Integer;
begin
 g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

 find_id := FindTexture;

 WAD := TWADEditor_1.Create;
 WAD.ReadFile(FileName);

 if WAD.GetResource(utf2win(SectionName), utf2win(ResourceName), TextureData, ResourceLength) then
 begin
  Result := e_CreateTextureMemEx(TextureData, ResourceLength, TexturesArray[find_id].ID, X, Y, Width, Height);
  FreeMem(TextureData);
  if Result then
  begin
   TexturesArray[find_id].Width := Width;
   TexturesArray[find_id].Height := Height;
   TexturesArray[find_id].Name := TextureName;
   TexturesArray[find_id].flag := flag;
  end;
 end
  else
 begin
  e_WriteLog(Format('Error loading texture %s', [Resource]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
  Result := False;
 end;
 WAD.Destroy;
end;

function g_GetTexture(TextureName: ShortString; var ID: DWORD): Boolean;
var
  a: DWORD;
begin
 Result := False;
 
 if TexturesArray = nil then Exit;

 if TextureName = '' then Exit;

 for a := 0 to High(TexturesArray) do
  if TexturesArray[a].Name = TextureName then
  begin
   ID := TexturesArray[a].ID;
   Result := True;
   Break;
  end;
end;

function g_GetTextureFlagByName(TextureName: ShortString): Byte;
var
  ID: DWORD;
begin
 Result := 0;

 if not g_GetTexture(TextureName, ID) then Exit;

 Result := TexturesArray[ID].flag;
end;

function g_GetTextureFlagByID(ID: DWORD): Byte;
begin
 Result := TexturesArray[ID].flag;
end;

procedure g_GetTextureSizeByName(TextureName: ShortString; var Width, Height: Word);
var
  ID: DWORD;
begin
 Width := 0;
 Height := 0;

 if not g_GetTexture(TextureName, ID) then Exit;

 e_GetTextureSize(ID, @Width, @Height);
end;

procedure g_GetTextureSizeByID(ID: DWORD; var Width, Height: Word);
begin
 e_GetTextureSize(ID, @Width, @Height);
end;

procedure g_DeleteTexture(TextureName: ShortString);
var
  a: DWORD;
begin
 if TexturesArray = nil then Exit;

 for a := 0 to High(TexturesArray) do
  if TexturesArray[a].Name = TextureName then
  begin
   e_DeleteTexture(TexturesArray[a].ID);
   TexturesArray[a].Name := '';
   TexturesArray[a].ID := 0;
   TexturesArray[a].Width := 0;
   TexturesArray[a].Height := 0;
  end;
end;

procedure g_DeleteAllTextures;
var
  a: DWORD;
begin
 if TexturesArray = nil then Exit;

 for a := 0 to High(TexturesArray) do
  if TexturesArray[a].Name <> '' then
   e_DeleteTexture(TexturesArray[a].ID);

 TexturesArray := nil;
end;

end.
