(* Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
 * This file is part of Doom2D Forever.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation, version 3 of
 * the License ONLY.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 *)

{$INCLUDE ../shared/a_modes.inc}
unit g_textures;

interface

uses
  SysUtils, Classes,
  {$IFDEF USE_MEMPOOL}mempool,{$ENDIF}
  e_graphics, MAPDEF, ImagingTypes, Imaging, ImagingUtility;

type
  TLevelTexture = record
    textureName: AnsiString;
    width, height: Word;
    case anim: Boolean of
      false: (textureID: LongWord);
      true: (framesID: LongWord; framesCount: Byte; speed: Byte);
  end;

  TLevelTextureArray = array of TLevelTexture;

  TAnimation = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  private
    mId: LongWord;
    mAlpha: Byte;
    mBlending: Boolean;
    mCounter: Byte; // Счетчик ожидания между кадрами
    mSpeed: Byte; // Время ожидания между кадрами
    mCurrentFrame: Integer; // Текущий кадр (начиная с 0)
    mLoop: Boolean; // Переходить на первый кадр после последнего?
    mEnabled: Boolean; // Работа разрешена?
    mPlayed: Boolean; // Проиграна вся хотя бы раз?
    mHeight: Word;
    mWidth: Word;
    mMinLength: Byte; // Ожидание после проигрывания
    mRevert: Boolean; // Смена кадров обратная?

  public
    constructor Create (aframesID: LongWord; aloop: Boolean; aspeed: Byte);
    destructor  Destroy (); override;

    procedure draw (x, y: Integer; mirror: TMirrorType);
    procedure drawEx (x, y: Integer; mirror: TMirrorType; rpoint: TDFPoint; angle: SmallInt);

    procedure reset ();
    procedure update ();
    procedure enable ();
    procedure disable ();
    procedure revert (r: Boolean);

    procedure saveState (st: TStream);
    procedure loadState (st: TStream);

    function totalFrames (): Integer; inline;

  public
    property played: Boolean read mPlayed;
    property enabled: Boolean read mEnabled;
    property isReverse: Boolean read mRevert;
    property loop: Boolean read mLoop write mLoop;
    property speed: Byte read mSpeed write mSpeed;
    property minLength: Byte read mMinLength write mMinLength;
    property currentFrame: Integer read mCurrentFrame write mCurrentFrame;
    property currentCounter: Byte read mCounter write mCounter;
    property counter: Byte read mCounter;
    property blending: Boolean read mBlending write mBlending;
    property alpha: Byte read mAlpha write mAlpha;
    property framesId: LongWord read mId;
    property width: Word read mWidth;
    property height: Word read mHeight;
  end;


function g_Texture_CreateWAD (var ID: LongWord; const Resource: AnsiString): Boolean;
function g_Texture_CreateFile (var ID: LongWord; const FileName: AnsiString): Boolean;
function g_Texture_CreateWADEx (const textureName, Resource: AnsiString): Boolean;
function g_Texture_CreateFileEx (const textureName, FileName: AnsiString): Boolean;
function g_Texture_Get (const textureName: AnsiString; var ID: LongWord): Boolean;
function g_Texture_GetSize (const textureName: AnsiString; var w, h: Integer): Boolean; overload;
function g_Texture_GetSize (ID: LongWord; var w, h: Integer): Boolean; overload;
procedure g_Texture_Delete (const textureName: AnsiString);
procedure g_Texture_DeleteAll ();

function g_CreateFramesImg (ia: TDynImageDataArray; ID: PDWORD; const Name: AnsiString; BackAnimation: Boolean=false): Boolean;

function g_Frames_CreateWAD (ID: PDWORD; const Name, Resource: AnsiString; mWidth, mHeight, mCount: Word; BackAnimation: Boolean=false): Boolean;
function g_Frames_CreateFile (ID: PDWORD; const Name, FileName: AnsiString; mWidth, mHeight, mCount: Word; BackAnimation: Boolean=false): Boolean;
function g_Frames_CreateMemory (ID: PDWORD; const Name: AnsiString; pData: Pointer; dataSize: LongInt;
                                mWidth, mHeight, mCount: Word; BackAnimation: Boolean=false): Boolean;
function g_Frames_Dup (const NewName, OldName: AnsiString): Boolean;
//function g_Frames_CreateRevert(ID: PDWORD; Name: ShortString; Frames: string): Boolean;
function g_Frames_Get (out ID: LongWord; const FramesName: AnsiString): Boolean;
function g_Frames_GetTexture (out ID: LongWord; const FramesName: AnsiString; Frame: Word): Boolean;
function g_Frames_Exists (const FramesName: AnsiString): Boolean;
procedure g_Frames_DeleteByName (const FramesName: AnsiString);
procedure g_Frames_DeleteByID (ID: LongWord);
procedure g_Frames_DeleteAll ();

procedure DumpTextureNames ();

function g_Texture_Light (): Integer;


implementation

uses
  {$INCLUDE ../nogl/noGLuses.inc}
  g_game, e_log, g_basic, g_console, wadreader,
  g_language, utils, xstreams;

type
  _TTexture = record
    name: AnsiString;
    id: LongWord;
    width, height: Word;
    used: Boolean;
  end;

  TFrames = record
    texturesID: array of LongWord;
    name: AnsiString;
    frameWidth, frameHeight: Word;
    used: Boolean;
  end;

var
  texturesArray: array of _TTexture;
  framesArray: array of TFrames;


const
  ANIM_SIGNATURE = $4D494E41; // 'ANIM'


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


// ////////////////////////////////////////////////////////////////////////// //
function g_Texture_CreateWAD (var ID: LongWord; const Resource: AnsiString): Boolean;
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
    if e_CreateTextureMem(TextureData, ResourceLength, ID) then
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


function g_Texture_CreateWADEx (const textureName, Resource: AnsiString): Boolean;
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
    result := e_CreateTextureMem(TextureData, ResourceLength, texturesArray[find_id].ID);
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


{function g_Frames_CreateRevert(ID: PDWORD; Name: ShortString; Frames: string): Boolean;
var
  find_id, b: DWORD;
  a, c: Integer;
begin
 Result := False;

 if not g_Frames_Get(b, Frames) then Exit;

 find_id := FindFrame();

 FramesArray[find_id].Name := Name;
 FramesArray[find_id].FrameWidth := FramesArray[b].FrameWidth;
 FramesArray[find_id].FrameHeight := FramesArray[b].FrameHeight;

 c := High(FramesArray[find_id].TexturesID);

 for a := 0 to c do
  FramesArray[find_id].TexturesID[a] := FramesArray[b].TexturesID[c-a];

 Result := True;
end;}


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


procedure DumpTextureNames ();
var
  i: Integer;
begin
  e_WriteLog('BEGIN Textures:', TMsgType.Notify);
  for i := 0 to High(texturesArray) do e_WriteLog('   '+IntToStr(i)+'. '+texturesArray[i].Name, TMsgType.Notify);
  e_WriteLog('END Textures.', TMsgType.Notify);

  e_WriteLog('BEGIN Frames:', TMsgType.Notify);
  for i := 0 to High(framesArray) do e_WriteLog('   '+IntToStr(i)+'. '+framesArray[i].Name, TMsgType.Notify);
  e_WriteLog('END Frames.', TMsgType.Notify);
end;


{ TAnimation }

constructor TAnimation.Create (aframesID: LongWord; aloop: Boolean; aspeed: Byte);
begin
  if (aframesID >= Length(framesArray)) then
  begin
    //raise Exception.Create('trying to create inexisting frame: something is very wrong here');
    e_LogWritefln('trying to create inexisting frame %u of %u: something is very wrong here', [aframesID, LongWord(Length(framesArray))], TMsgType.Warning);
    aframesID := 0;
    if (Length(framesArray) = 0) then raise Exception.Create('trying to create inexisting frame: something is very wrong here');
  end;
  mId := aframesID;
  mMinLength := 0;
  mLoop := aloop;
  mSpeed := aspeed;
  mEnabled := true;
  mCurrentFrame := 0;
  mPlayed := false;
  mAlpha := 0;
  mWidth := framesArray[mId].FrameWidth;
  mHeight := framesArray[mId].FrameHeight;
end;


destructor TAnimation.Destroy ();
begin
  inherited;
end;


procedure TAnimation.draw (x, y: Integer; mirror: TMirrorType);
begin
  if (not mEnabled) then exit;
  e_DrawAdv(framesArray[mId].TexturesID[mCurrentFrame], x, y, mAlpha, true, mBlending, 0, nil, mirror);
  //e_DrawQuad(X, Y, X+FramesArray[ID].FrameWidth-1, Y+FramesArray[ID].FrameHeight-1, 0, 255, 0);
end;


procedure TAnimation.update ();
begin
  if (not mEnabled) then exit;

  mCounter += 1;

  if (mCounter >= mSpeed) then
  begin
    // Ожидание между кадрами закончилось
    // Обратный порядок кадров?
    if mRevert then
    begin
      // Дошли до конца анимации. Возможно, ждем еще
      if (mCurrentFrame = 0) then
      begin
        if (Length(framesArray[mId].TexturesID)*mSpeed+mCounter < mMinLength) then exit;
      end;

      mCurrentFrame -= 1;
      mPlayed := (mCurrentFrame < 0);

      // Повторять ли анимацию по кругу?
      if mPlayed then
      begin
        if mLoop then mCurrentFrame := High(framesArray[mId].TexturesID) else mCurrentFrame += 1;
      end;

      mCounter := 0;
    end
    else
    begin
      // Прямой порядок кадров
      // Дошли до конца анимации. Возможно, ждем еще
      if (mCurrentFrame = High(framesArray[mId].TexturesID)) then
      begin
        if (Length(framesArray[mId].TexturesID)*mSpeed+mCounter < mMinLength) then exit;
      end;

      mCurrentFrame += 1;
      mPlayed := (mCurrentFrame > High(framesArray[mId].TexturesID));

      // Повторять ли анимацию по кругу?
      if mPlayed then
      begin
        if mLoop then mCurrentFrame := 0 else mCurrentFrame -= 1;
      end;

      mCounter := 0;
    end;
  end;
end;


procedure TAnimation.reset ();
begin
  if mRevert then mCurrentFrame := High(framesArray[mId].TexturesID) else mCurrentFrame := 0;
  mCounter := 0;
  mPlayed := false;
end;


procedure TAnimation.disable (); begin mEnabled := false; end;
procedure TAnimation.enable (); begin mEnabled := true; end;


procedure TAnimation.drawEx (x, y: Integer; mirror: TMirrorType; rpoint: TDFPoint; angle: SmallInt);
begin
  if (not mEnabled) then exit;
  e_DrawAdv(framesArray[mId].TexturesID[mCurrentFrame], x, y, mAlpha, true, mBlending, angle, @rpoint, mirror);
end;


function TAnimation.totalFrames (): Integer; inline; begin result := Length(framesArray[mId].TexturesID); end;


procedure TAnimation.revert (r: Boolean);
begin
  mRevert := r;
  reset();
end;


procedure TAnimation.saveState (st: TStream);
begin
  if st = nil then Exit;

  utils.writeSign(st, 'ANIM');
  st.WriteByte(0);  // version

  st.WriteByte(mCounter);  // Счетчик ожидания между кадрами
  st.WriteInt32LE(mCurrentFrame);  // Текущий кадр
  st.WriteBool(mPlayed);  // Проиграна ли анимация целиком
  st.WriteByte(mAlpha);  // Alpha-канал всей текстуры
  st.WriteBool(mBlending);  // Размытие текстуры
  st.WriteByte(mSpeed);  // Время ожидания между кадрами
  st.WriteBool(mLoop);  // Зациклена ли анимация
  st.WriteBool(mEnabled);  // Включена ли
  st.WriteByte(mMinLength);  // Ожидание после проигрывания
  st.WriteBool(mRevert);  // Обратный ли порядок кадров
end;


procedure TAnimation.loadState (st: TStream);
begin
  if st = nil then Exit;

  if not utils.checkSign(st, 'ANIM') then
    Raise XStreamError.Create('animation chunk expected');
  if st.ReadByte() <> 0 then
    Raise XStreamError.Create('invalid animation chunk version');

  mCounter := st.ReadByte();  // Счетчик ожидания между кадрами
  mCurrentFrame := st.ReadInt32LE();  // Текущий кадр
  mPlayed := st.ReadBool();  // Проиграна ли анимация целиком
  mAlpha := st.ReadByte();  // Alpha-канал всей текстуры
  mBlending := st.ReadBool();  // Размытие текстуры
  mSpeed := st.ReadByte();  // Время ожидания между кадрами
  mLoop := st.ReadBool();  // Зациклена ли анимация
  mEnabled := st.ReadBool();  // Включена ли
  mMinLength := st.ReadByte();  // Ожидание после проигрывания
  mRevert := st.ReadBool();  // Обратный ли порядок кадров
end;


// ////////////////////////////////////////////////////////////////////////// //
var
  ltexid: GLuint;

function g_Texture_Light (): Integer;
const
  Radius: Integer = 128;
var
  tex, tpp: PByte;
  x, y, a: Integer;
  dist: Double;
begin
  if glIsTexture(ltexid) = GL_FALSE then
  begin
    GetMem(tex, (Radius*2)*(Radius*2)*4);
    tpp := tex;
    for y := 0 to Radius*2-1 do
    begin
      for x := 0 to Radius*2-1 do
      begin
        dist := 1.0 - sqrt( (x-Radius)*(x-Radius) + (y-Radius)*(y-Radius) ) / Radius;
        if (dist < 0) then
        begin
          tpp^ := 0; Inc(tpp);
          tpp^ := 0; Inc(tpp);
          tpp^ := 0; Inc(tpp);
          tpp^ := 0; Inc(tpp);
        end
        else
        begin
          //tc.setPixel(x, y, Color(cast(int)(dist*255), cast(int)(dist*255), cast(int)(dist*255)));
          if (dist > 0.5) then dist := 0.5;
          a := Round(dist * 255);
          if a < 0 then a := 0 else if a > 255 then a := 255;
          tpp^ := 255; Inc(tpp);
          tpp^ := 255; Inc(tpp);
          tpp^ := 255; Inc(tpp);
          tpp^ := Byte(a); Inc(tpp);
        end;
      end;
    end;

    glGenTextures(1, @ltexid);
    //if (tid == 0) assert(0, "VGL: can't create screen texture");

    glBindTexture(GL_TEXTURE_2D, ltexid);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    //GLfloat[4] bclr = 0.0;
    //glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, bclr.ptr);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Radius*2, Radius*2, 0, GL_RGBA{gltt}, GL_UNSIGNED_BYTE, tex);
    FreeMem(tex);
  end;

  Result := ltexid;
end;


end.
