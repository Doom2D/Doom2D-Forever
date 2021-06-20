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
{$INCLUDE ../../shared/a_modes.inc}
unit r_textures;

interface

  function g_Texture_CreateWAD (var ID: LongWord; const Resource: AnsiString; filterHint: Boolean = False): Boolean;
  function g_Texture_CreateFile (var ID: LongWord; const FileName: AnsiString): Boolean;
  function g_Texture_CreateWADEx (const textureName, Resource: AnsiString; filterHint: Boolean = False): Boolean;
  function g_Texture_CreateFileEx (const textureName, FileName: AnsiString): Boolean;
  function g_Texture_Get (const textureName: AnsiString; var ID: LongWord): Boolean;
  function g_Texture_GetSize (const textureName: AnsiString; var w, h: Integer): Boolean; overload;
  function g_Texture_GetSize (ID: LongWord; var w, h: Integer): Boolean; overload;
  procedure g_Texture_Delete (const textureName: AnsiString);
  procedure g_Texture_DeleteAll;

implementation

  uses
    SysUtils, Classes, Math,
    WadReader, utils,
    e_log,
    r_graphics,
    g_language, g_game
  ;

  type
    _TTexture = record
      name: AnsiString;
      id: LongWord;
      width, height: Word;
      used: Boolean;
    end;

  var
    texturesArray: array of _TTexture = nil;

function allocTextureSlot (): LongWord;
var
  f: integer;
begin
  for f := 0 to High(texturesArray) do
  begin
    if (not texturesArray[f].used) then
    begin
      result := f;
      exit;
    end;
  end;

  result := Length(texturesArray);
  SetLength(texturesArray, result+64);
  for f := result to High(texturesArray) do
  begin
    with texturesArray[f] do
    begin
      name := '';
      id := 0;
      width := 0;
      height := 0;
      used := false;
    end;
  end;
end;

function g_Texture_CreateWAD (var ID: LongWord; const Resource: AnsiString; filterHint: Boolean = False): Boolean;
var
  WAD: TWADFile;
  FileName: AnsiString;
  TextureData: Pointer;
  ResourceLength: Integer;
begin
  result := false;
  FileName := g_ExtractWadName(Resource);

  WAD := TWADFile.Create;
  WAD.ReadFile(FileName);

  if WAD.GetResource(g_ExtractFilePathName(Resource), TextureData, ResourceLength) then
  begin
    if e_CreateTextureMem(TextureData, ResourceLength, ID, filterHint) then
      result := true;
    FreeMem(TextureData)
  end
  else
  begin
    e_WriteLog(Format('Error loading texture %s', [Resource]), TMsgType.Warning);
    //e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
  end;
  WAD.Free();
end;


function g_Texture_CreateFile (var ID: LongWord; const FileName: AnsiString): Boolean;
begin
  result := true;
  if not e_CreateTexture(FileName, ID) then
  begin
    e_WriteLog(Format('Error loading texture %s', [FileName]), TMsgType.Warning);
    result := false;
  end;
end;

function g_Texture_CreateWADEx (const textureName, Resource: AnsiString; filterHint: Boolean = False): Boolean;
var
  WAD: TWADFile;
  FileName: AnsiString;
  TextureData: Pointer;
  find_id: LongWord;
  ResourceLength: Integer;
begin
  FileName := g_ExtractWadName(Resource);

  find_id := allocTextureSlot();

  WAD := TWADFile.Create;
  WAD.ReadFile(FileName);

  if WAD.GetResource(g_ExtractFilePathName(Resource), TextureData, ResourceLength) then
  begin
    result := e_CreateTextureMem(TextureData, ResourceLength, texturesArray[find_id].ID, filterHint);
    if result then
    begin
      e_GetTextureSize(texturesArray[find_id].ID, @texturesArray[find_id].width, @texturesArray[find_id].height);
      texturesArray[find_id].used := true;
      texturesArray[find_id].Name := textureName;
    end;
    FreeMem(TextureData)
  end
  else
  begin
    e_WriteLog(Format('Error loading texture %s', [Resource]), TMsgType.Warning);
    //e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
    result := false;
  end;
  WAD.Free();
end;


function g_Texture_CreateFileEx (const textureName, FileName: AnsiString): Boolean;
var
  find_id: LongWord;
begin
  find_id := allocTextureSlot();
  result := e_CreateTexture(FileName, texturesArray[find_id].ID);
  if result then
  begin
    texturesArray[find_id].used := true;
    texturesArray[find_id].Name := textureName;
    e_GetTextureSize(texturesArray[find_id].ID, @texturesArray[find_id].width, @texturesArray[find_id].height);
  end
  else e_WriteLog(Format('Error loading texture %s', [FileName]), TMsgType.Warning);
end;


function g_Texture_Get (const textureName: AnsiString; var id: LongWord): Boolean;
var
  a: Integer;
begin
  result := false;
  if (Length(texturesArray) = 0) or (Length(textureName) = 0) then exit;
  for a := 0 to High(texturesArray) do
  begin
    if (StrEquCI1251(texturesArray[a].name, textureName)) then
    begin
      id := texturesArray[a].id;
      result := true;
      break;
    end;
  end;
  //if not Result then g_ConsoleAdd('Texture '+TextureName+' not found');
end;

function g_Texture_GetSize (const textureName: AnsiString; var w, h: Integer): Boolean; overload;
var
  a: Integer;
begin
  result := false;
  w := 0;
  h := 0;
  if (Length(texturesArray) = 0) or (Length(textureName) = 0) then exit;
  for a := 0 to High(texturesArray) do
  begin
    if (StrEquCI1251(texturesArray[a].name, textureName)) then
    begin
      w := texturesArray[a].width;
      h := texturesArray[a].height;
      result := true;
      break;
    end;
  end;
end;


function g_Texture_GetSize (ID: LongWord; var w, h: Integer): Boolean; overload;
var
  a: Integer;
begin
  result := false;
  w := 0;
  h := 0;
  if (Length(texturesArray) = 0) then exit;
  for a := 0 to High(texturesArray) do
  begin
    if (texturesArray[a].id = ID) then
    begin
      w := texturesArray[a].width;
      h := texturesArray[a].height;
      result := true;
      break;
    end;
  end;
end;


procedure g_Texture_Delete (const textureName: AnsiString);
var
  a: Integer;
begin
  if (Length(texturesArray) = 0) or (Length(textureName) = 0) then exit;
  for a := 0 to High(texturesArray) do
  begin
    if (StrEquCI1251(texturesArray[a].name, textureName)) then
    begin
      e_DeleteTexture(texturesArray[a].ID);
      texturesArray[a].used := false;
      texturesArray[a].name := '';
      texturesArray[a].id := 0;
      texturesArray[a].width := 0;
      texturesArray[a].height := 0;
    end;
  end;
end;

procedure g_Texture_DeleteAll ();
var
  a: Integer;
begin
  for a := 0 to High(texturesArray) do
  begin
    if (texturesArray[a].used) then e_DeleteTexture(texturesArray[a].ID);
  end;
  texturesArray := nil;
end;

end.
