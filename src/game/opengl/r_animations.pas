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
unit r_animations;

interface

  uses g_base, g_textures, MAPDEF, Imaging; // TMirrorType, TAnimationState, TDFPoint, TDynImageDataArray

  procedure r_AnimationState_Draw (FID: DWORD; t: TAnimationState; x, y: Integer; alpha: Byte; mirror: TMirrorType; blending: Boolean);
  procedure r_AnimationState_DrawEx (FID: DWORD; t: TAnimationState; x, y: Integer; alpha: Byte; mirror: TMirrorType; blending: Boolean; rpoint: TDFPoint; angle: SmallInt);

  function g_CreateFramesImg (ia: TDynImageDataArray; ID: PDWORD; const Name: AnsiString; BackAnimation: Boolean = false): Boolean;

  function g_Frames_CreateWAD (ID: PDWORD; const Name, Resource: AnsiString; mWidth, mHeight, mCount: Word; BackAnimation: Boolean=false): Boolean;
  function g_Frames_CreateFile (ID: PDWORD; const Name, FileName: AnsiString; mWidth, mHeight, mCount: Word; BackAnimation: Boolean = false): Boolean;
  function g_Frames_CreateMemory (ID: PDWORD; const Name: AnsiString; pData: Pointer; dataSize: LongInt; mWidth, mHeight, mCount: Word; BackAnimation: Boolean=false): Boolean;
  function g_Frames_Dup (const NewName, OldName: AnsiString): Boolean;
  function g_Frames_Get (out ID: LongWord; const FramesName: AnsiString): Boolean;
  function g_Frames_GetTexture (out ID: LongWord; const FramesName: AnsiString; Frame: Word): Boolean;
  function g_Frames_Exists (const FramesName: AnsiString): Boolean;
  procedure g_Frames_DeleteByName (const FramesName: AnsiString);
  procedure g_Frames_DeleteByID (ID: LongWord);
  procedure g_Frames_DeleteAll;

  type
    TFrames = record
      texturesID: array of LongWord;
      name: AnsiString;
      frameWidth, frameHeight: Word;
      used: Boolean;
    end;

  var
    framesArray: array of TFrames = nil;

implementation

  uses
    SysUtils, Classes, Math,
    WadReader, utils,
    e_log,
    r_graphics,
    g_language, g_game
  ;

  procedure r_AnimationState_Draw (FID: DWORD; t: TAnimationState; x, y: Integer; alpha: Byte; mirror: TMirrorType; blending: Boolean);
  begin
    if t.enabled then
      e_DrawAdv(framesArray[FID].TexturesID[t.currentFrame], x, y, alpha, true, blending, 0, nil, mirror)
  end;

  procedure r_AnimationState_DrawEx (FID: DWORD; t: TAnimationState; x, y: Integer; alpha: Byte; mirror: TMirrorType; blending: Boolean; rpoint: TDFPoint; angle: SmallInt);
  begin
    if t.enabled then
      e_DrawAdv(framesArray[FID].TexturesID[t.currentFrame], x, y, alpha, true, blending, angle, @rpoint, mirror)
  end;

function allocFrameSlot (): LongWord;
var
  f: integer;
begin
  for f := 0 to High(framesArray) do
  begin
    if (not framesArray[f].used) then
    begin
      result := f;
      exit;
    end;
  end;

  result := Length(framesArray);
  SetLength(framesArray, result+64);
  for f := result to High(framesArray) do
  begin
    with framesArray[f] do
    begin
      texturesID := nil;
      name := '';
      frameWidth := 0;
      frameHeight := 0;
      used := false;
    end;
  end;
end;

function g_Frames_CreateFile (ID: PDWORD; const Name, FileName: AnsiString;
                              mWidth, mHeight, mCount: Word; BackAnimation: Boolean = false): Boolean;
var
  a: Integer;
  find_id: LongWord;
begin
  result := false;

  find_id := allocFrameSlot();

  if (mCount <= 2) then BackAnimation := false;

  if BackAnimation then SetLength(framesArray[find_id].TexturesID, mCount+mCount-2)
  else SetLength(framesArray[find_id].TexturesID, mCount);

  for a := 0 to mCount-1 do
  begin
    if not e_CreateTextureEx(FileName, framesArray[find_id].TexturesID[a], a*mWidth, 0, mWidth, mHeight) then exit;
  end;

  if BackAnimation then
  begin
    for a := 1 to mCount-2 do framesArray[find_id].TexturesID[mCount+mCount-2-a] := framesArray[find_id].TexturesID[a];
  end;

  framesArray[find_id].used := true;
  framesArray[find_id].FrameWidth := mWidth;
  framesArray[find_id].FrameHeight := mHeight;
  if (Name <> '') then framesArray[find_id].Name := Name else framesArray[find_id].Name := '<noname>';

  if (ID <> nil) then ID^ := find_id;

  result := true;
end;

function CreateFramesMem (pData: Pointer; dataSize: LongInt; ID: PDWORD; Name: AnsiString;
                          mWidth, mHeight, mCount: Word; BackAnimation: Boolean = false): Boolean;
var
  find_id: LongWord;
  a: Integer;
begin
  result := false;

  find_id := allocFrameSlot();

  if (mCount <= 2) then BackAnimation := false;

  if BackAnimation then SetLength(framesArray[find_id].TexturesID, mCount+mCount-2)
  else SetLength(framesArray[find_id].TexturesID, mCount);

  for a := 0 to mCount-1 do
    if not e_CreateTextureMemEx(pData, dataSize, framesArray[find_id].TexturesID[a], a*mWidth, 0, mWidth, mHeight) then
    begin
      //!!!FreeMem(pData);
      exit;
    end;

  if BackAnimation then
  begin
    for a := 1 to mCount-2 do framesArray[find_id].TexturesID[mCount+mCount-2-a] := framesArray[find_id].TexturesID[a];
  end;

  framesArray[find_id].used := true;
  framesArray[find_id].FrameWidth := mWidth;
  framesArray[find_id].FrameHeight := mHeight;
  if (Name <> '') then framesArray[find_id].Name := Name else framesArray[find_id].Name := '<noname>';

  if (ID <> nil) then ID^ := find_id;

  result := true;
end;

function g_CreateFramesImg (ia: TDynImageDataArray; ID: PDWORD; const Name: AnsiString; BackAnimation: Boolean = false): Boolean;
var
  find_id: LongWord;
  a, mCount: Integer;
begin
  result := false;
  find_id := allocFrameSlot();

  mCount := Length(ia);

  //e_WriteLog(Format('+++ creating %d frames [%s]', [FCount, Name]), MSG_NOTIFY);

  if (mCount < 1) then exit;
  if (mCount <= 2) then BackAnimation := false;

  if BackAnimation then SetLength(framesArray[find_id].TexturesID, mCount+mCount-2)
  else SetLength(framesArray[find_id].TexturesID, mCount);

  //e_WriteLog(Format('+++ creating %d frames, %dx%d', [FCount, ia[0].width, ia[0].height]), MSG_NOTIFY);

  for a := 0 to mCount-1 do
  begin
    if not e_CreateTextureImg(ia[a], framesArray[find_id].TexturesID[a]) then exit;
    //e_WriteLog(Format('+++   frame %d, %dx%d', [a, ia[a].width, ia[a].height]), MSG_NOTIFY);
  end;

  if BackAnimation then
  begin
    for a := 1 to mCount-2 do framesArray[find_id].TexturesID[mCount+mCount-2-a] := framesArray[find_id].TexturesID[a];
  end;

  framesArray[find_id].used := true;
  framesArray[find_id].FrameWidth := ia[0].width;
  framesArray[find_id].FrameHeight := ia[0].height;
  if (Name <> '') then framesArray[find_id].Name := Name else framesArray[find_id].Name := '<noname>';

  if (ID <> nil) then ID^ := find_id;

  result := true;
end;

function g_Frames_CreateWAD (ID: PDWORD; const Name, Resource: AnsiString;
                             mWidth, mHeight, mCount: Word; BackAnimation: Boolean=false): Boolean;
var
  WAD: TWADFile;
  FileName: AnsiString;
  TextureData: Pointer;
  ResourceLength: Integer;
begin
  result := false;

  // models without "advanced" animations asks for "nothing" like this; don't spam log
  if (Length(Resource) > 0) and ((Resource[Length(Resource)] = '/') or (Resource[Length(Resource)] = '\')) then exit;

  FileName := g_ExtractWadName(Resource);

  WAD := TWADFile.Create();
  WAD.ReadFile(FileName);

  if not WAD.GetResource(g_ExtractFilePathName(Resource), TextureData, ResourceLength) then
  begin
    WAD.Free();
    e_WriteLog(Format('Error loading texture %s', [Resource]), TMsgType.Warning);
    //e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
    exit;
  end;

  if not CreateFramesMem(TextureData, ResourceLength, ID, Name, mWidth, mHeight, mCount, BackAnimation) then
  begin
    FreeMem(TextureData);
    WAD.Free();
    exit;
  end;

  FreeMem(TextureData);
  WAD.Free();

  result := true;
end;

function g_Frames_CreateMemory (ID: PDWORD; const Name: AnsiString; pData: Pointer; dataSize: LongInt;
                                mWidth, mHeight, mCount: Word; BackAnimation: Boolean = false): Boolean;
begin
  result := CreateFramesMem(pData, dataSize, ID, Name, mWidth, mHeight, mCount, BackAnimation);
end;

function g_Frames_Dup (const NewName, OldName: AnsiString): Boolean;
var
  find_id, b: LongWord;
  a, c: Integer;
begin
  result := false;

  if not g_Frames_Get(b, OldName) then exit;

  find_id := allocFrameSlot();

  framesArray[find_id].used := true;
  framesArray[find_id].Name := NewName;
  framesArray[find_id].FrameWidth := framesArray[b].FrameWidth;
  framesArray[find_id].FrameHeight := framesArray[b].FrameHeight;

  c := High(framesArray[b].TexturesID);
  SetLength(framesArray[find_id].TexturesID, c+1);

  for a := 0 to c do framesArray[find_id].TexturesID[a] := framesArray[b].TexturesID[a];

  result := true;
end;


procedure g_Frames_DeleteByName (const FramesName: AnsiString);
var
  a, b: Integer;
begin
  if (Length(framesArray) = 0) then exit;
  for a := 0 to High(framesArray) do
  begin
    if (StrEquCI1251(framesArray[a].Name, FramesName)) then
    begin
      if framesArray[a].TexturesID <> nil then
      begin
        for b := 0 to High(framesArray[a].TexturesID) do e_DeleteTexture(framesArray[a].TexturesID[b]);
      end;
      framesArray[a].used := false;
      framesArray[a].TexturesID := nil;
      framesArray[a].Name := '';
      framesArray[a].FrameWidth := 0;
      framesArray[a].FrameHeight := 0;
    end;
  end;
end;

procedure g_Frames_DeleteByID (ID: LongWord);
var
  b: Integer;
begin
  if (Length(framesArray) = 0) then exit;
  if (framesArray[ID].TexturesID <> nil) then
  begin
    for b := 0 to High(framesArray[ID].TexturesID) do e_DeleteTexture(framesArray[ID].TexturesID[b]);
  end;
  framesArray[ID].used := false;
  framesArray[ID].TexturesID := nil;
  framesArray[ID].Name := '';
  framesArray[ID].FrameWidth := 0;
  framesArray[ID].FrameHeight := 0;
end;

procedure g_Frames_DeleteAll ();
var
  a, b: Integer;
begin
  for a := 0 to High(framesArray) do
  begin
    if (framesArray[a].used) then
    begin
      for b := 0 to High(framesArray[a].TexturesID) do e_DeleteTexture(framesArray[a].TexturesID[b]);
    end;
    framesArray[a].used := false;
    framesArray[a].TexturesID := nil;
    framesArray[a].Name := '';
    framesArray[a].FrameWidth := 0;
    framesArray[a].FrameHeight := 0;
  end;
  framesArray := nil;
end;


function g_Frames_Get (out ID: LongWord; const FramesName: AnsiString): Boolean;
var
  a: Integer;
begin
  result := false;
  if (Length(framesArray) = 0) then exit;
  for a := 0 to High(framesArray) do
  begin
    if (StrEquCI1251(framesArray[a].Name, FramesName)) then
    begin
      ID := a;
      result := true;
      break;
    end;
  end;
  if not result then g_FatalError(Format(_lc[I_GAME_ERROR_FRAMES], [FramesName]));
end;

function g_Frames_GetTexture (out ID: LongWord; const FramesName: AnsiString; Frame: Word): Boolean;
var
  a: Integer;
begin
  result := false;
  if (Length(framesArray) = 0) then exit;
  for a := 0 to High(framesArray) do
  begin
    if (StrEquCI1251(framesArray[a].Name, FramesName)) then
    begin
      if (Frame < Length(framesArray[a].TexturesID)) then
      begin
        ID := framesArray[a].TexturesID[Frame];
        result := true;
        break;
      end;
    end;
  end;
  if not result then g_FatalError(Format(_lc[I_GAME_ERROR_FRAMES], [FramesName]));
end;

function g_Frames_Exists (const FramesName: AnsiString): Boolean;
var
  a: Integer;
begin
  result := false;
  if (Length(framesArray) = 0) then exit;
  for a := 0 to High(framesArray) do
  begin
    if (StrEquCI1251(framesArray[a].Name, FramesName)) then
    begin
      result := true;
      exit;
    end;
  end;
end;

end.
