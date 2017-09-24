(* Copyright (C)  DooM 2D:Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
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
unit g_textures;

interface

uses
  SysUtils, Classes,
  {$IFDEF USE_MEMPOOL}mempool,{$ENDIF}
  e_graphics, MAPDEF, ImagingTypes, Imaging, ImagingUtility;

Type
  TLevelTexture = record
    TextureName: String;
    Width,
    Height:      Word;
    case Anim: Boolean of
      False: (TextureID:   DWORD;);
      True:  (FramesID:    DWORD;
              FramesCount: Byte;
              Speed:       Byte);
  end;

  TLevelTextureArray = Array of TLevelTexture;

  TAnimation = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  private
    ID:            DWORD;
    FAlpha:        Byte;
    FBlending:     Boolean;
    FCounter:      Byte;    // Счетчик ожидания между кадрами
    FSpeed:        Byte;    // Время ожидания между кадрами
    FCurrentFrame: Integer; // Текущий кадр (начиная с 0)
    FLoop:         Boolean; // Переходить на первый кадр после последнего?
    FEnabled:      Boolean; // Работа разрешена?
    FPlayed:       Boolean; // Проиграна вся хотя бы раз?
    FHeight:       Word;
    FWidth:        Word;
    FMinLength:    Byte;    // Ожидание после проигрывания
    FRevert:       Boolean; // Смена кадров обратная?

  public
    constructor Create(FramesID: DWORD; Loop: Boolean; Speed: Byte);
    destructor  Destroy(); override;
    procedure   Draw(X, Y: Integer; Mirror: TMirrorType);
    procedure   DrawEx(X, Y: Integer; Mirror: TMirrorType; RPoint: TDFPoint;
                       Angle: SmallInt);
    procedure   Reset();
    procedure   Update();
    procedure   Enable();
    procedure   Disable();
    procedure   Revert(r: Boolean);
    procedure   SaveState(st: TStream);
    procedure   LoadState(st: TStream);
    function    TotalFrames(): Integer;

    property    Played: Boolean read FPlayed;
    property    Enabled: Boolean read FEnabled;
    property    IsReverse: Boolean read FRevert;
    property    Loop: Boolean read FLoop write FLoop;
    property    Speed: Byte read FSpeed write FSpeed;
    property    MinLength: Byte read FMinLength write FMinLength;
    property    CurrentFrame: Integer read FCurrentFrame write FCurrentFrame;
    property    CurrentCounter: Byte read FCounter write FCounter;
    property    Counter: Byte read FCounter;
    property    Blending: Boolean read FBlending write FBlending;
    property    Alpha: Byte read FAlpha write FAlpha;
    property    FramesID: DWORD read ID;
    property    Width: Word read FWidth;
    property    Height: Word read FHeight;
  end;

function g_Texture_CreateWAD(var ID: DWORD; Resource: String): Boolean;
function g_Texture_CreateFile(var ID: DWORD; FileName: String): Boolean;
function g_Texture_CreateWADEx(TextureName: ShortString; Resource: String; altrsrc: AnsiString=''): Boolean;
function g_Texture_CreateFileEx(TextureName: ShortString; FileName: String): Boolean;
function g_Texture_Get(TextureName: ShortString; var ID: DWORD): Boolean;
procedure g_Texture_Delete(TextureName: ShortString);
procedure g_Texture_DeleteAll();

function g_CreateFramesImg (ia: TDynImageDataArray; ID: PDWORD; Name: ShortString; BackAnimation: Boolean = False): Boolean;

function g_Frames_CreateWAD(ID: PDWORD; Name: ShortString; Resource: String;
                            FWidth, FHeight, FCount: Word; BackAnimation: Boolean = False): Boolean;
function g_Frames_CreateFile(ID: PDWORD; Name: ShortString; FileName: String;
                             FWidth, FHeight, FCount: Word; BackAnimation: Boolean = False): Boolean;
function g_Frames_CreateMemory(ID: PDWORD; Name: ShortString; pData: Pointer; dataSize: LongInt;
                               FWidth, FHeight, FCount: Word; BackAnimation: Boolean = False): Boolean;
function g_Frames_Dup(NewName, OldName: ShortString): Boolean;
//function g_Frames_CreateRevert(ID: PDWORD; Name: ShortString; Frames: string): Boolean;
function g_Frames_Get(out ID: DWORD; FramesName: ShortString): Boolean;
function g_Frames_GetTexture(out ID: DWORD; FramesName: ShortString; Frame: Word): Boolean;
function g_Frames_Exists(FramesName: String): Boolean;
procedure g_Frames_DeleteByName(FramesName: ShortString);
procedure g_Frames_DeleteByID(ID: DWORD);
procedure g_Frames_DeleteAll();

procedure DumpTextureNames();

function g_Texture_Light(): Integer;

implementation

uses
  g_game, e_log, g_basic, g_console, wadreader,
  g_language, GL, utils, xstreams;

type
  _TTexture = record
    Name: ShortString;
    ID: DWORD;
    Width, Height: Word;
  end;

  TFrames = record
    TexturesID: Array of DWORD;
    Name: ShortString;
    FrameWidth,
    FrameHeight: Word;
  end;

var
  TexturesArray: Array of _TTexture = nil;
  FramesArray: Array of TFrames = nil;

const
  ANIM_SIGNATURE = $4D494E41; // 'ANIM'

function FindTexture(): DWORD;
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

function g_Texture_CreateWAD(var ID: DWORD; Resource: String): Boolean;
var
  WAD: TWADFile;
  FileName: String;
  TextureData: Pointer;
  ResourceLength: Integer;
begin
  Result := False;
  FileName := g_ExtractWadName(Resource);

  WAD := TWADFile.Create;
  WAD.ReadFile(FileName);

  if WAD.GetResource(g_ExtractFilePathName(Resource), TextureData, ResourceLength) then
  begin
    if e_CreateTextureMem(TextureData, ResourceLength, ID) then
      Result := True
    else
      FreeMem(TextureData);
  end
  else
  begin
    e_WriteLog(Format('Error loading texture %s', [Resource]), TMsgType.Warning);
    //e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
  end;
  WAD.Free();
end;

function g_Texture_CreateFile(var ID: DWORD; FileName: String): Boolean;
begin
  Result := True;
  if not e_CreateTexture(FileName, ID) then
  begin
    e_WriteLog(Format('Error loading texture %s', [FileName]), TMsgType.Warning);
    Result := False;
  end;
end;

function texture_CreateWADExInternal (TextureName: ShortString; Resource: String; showmsg: Boolean): Boolean;
var
  WAD: TWADFile;
  FileName: String;
  TextureData: Pointer;
  find_id: DWORD;
  ResourceLength: Integer;
begin
  FileName := g_ExtractWadName(Resource);

  find_id := FindTexture();

  WAD := TWADFile.Create;
  WAD.ReadFile(FileName);

  if WAD.GetResource(g_ExtractFilePathName(Resource), TextureData, ResourceLength) then
  begin
    result := e_CreateTextureMem(TextureData, ResourceLength, TexturesArray[find_id].ID);
    if result then
    begin
      e_GetTextureSize(TexturesArray[find_id].ID, @TexturesArray[find_id].Width, @TexturesArray[find_id].Height);
      TexturesArray[find_id].Name := LowerCase(TextureName);
    end
    else
    begin
      FreeMem(TextureData);
    end;
  end
  else
  begin
    if showmsg then
    begin
      e_WriteLog(Format('Error loading texture %s', [Resource]), TMsgType.Warning);
    end;
    //e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
    result := false;
  end;
  WAD.Free();
end;

function g_Texture_CreateWADEx(TextureName: ShortString; Resource: String; altrsrc: AnsiString=''): Boolean;
begin
  if (Length(altrsrc) > 0) then
  begin
    result := texture_CreateWADExInternal(TextureName, altrsrc, false);
    if result then exit;
  end;
  result := texture_CreateWADExInternal(TextureName, Resource, true);
end;

function g_Texture_CreateFileEx(TextureName: ShortString; FileName: String): Boolean;
var
  find_id: DWORD;
begin
  find_id := FindTexture;

  Result := e_CreateTexture(FileName, TexturesArray[find_id].ID);
  if Result then
  begin
    TexturesArray[find_id].Name := LowerCase(TextureName);
    e_GetTextureSize(TexturesArray[find_id].ID, @TexturesArray[find_id].Width,
                     @TexturesArray[find_id].Height);
  end
  else e_WriteLog(Format('Error loading texture %s', [FileName]), TMsgType.Warning);
end;

function g_Texture_Get(TextureName: ShortString; var ID: DWORD): Boolean;
var
  a: DWORD;
begin
  Result := False;

  if TexturesArray = nil then Exit;

  if TextureName = '' then Exit;

  TextureName := LowerCase(TextureName);

  for a := 0 to High(TexturesArray) do
    if TexturesArray[a].Name = TextureName then
    begin
      ID := TexturesArray[a].ID;
      Result := True;
      Break;
    end;

  //if not Result then g_ConsoleAdd('Texture '+TextureName+' not found');
end;

procedure g_Texture_Delete(TextureName: ShortString);
var
  a: DWORD;
begin
  if TexturesArray = nil then Exit;

  TextureName := LowerCase(TextureName);

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

procedure g_Texture_DeleteAll();
var
  a: DWORD;
begin
  if TexturesArray = nil then Exit;

  for a := 0 to High(TexturesArray) do
    if TexturesArray[a].Name <> '' then
      e_DeleteTexture(TexturesArray[a].ID);

  TexturesArray := nil;
end;

function FindFrame(): DWORD;
var
  i: integer;
begin
  if FramesArray <> nil then
    for i := 0 to High(FramesArray) do
      if FramesArray[i].TexturesID = nil then
      begin
        Result := i;
        Exit;
      end;

  if FramesArray = nil then
  begin
    SetLength(FramesArray, 64);
    Result := 0;
  end
  else
  begin
    Result := High(FramesArray) + 1;
    SetLength(FramesArray, Length(FramesArray) + 64);
  end;
end;

function g_Frames_CreateFile(ID: PDWORD; Name: ShortString; FileName: String;
                             FWidth, FHeight, FCount: Word; BackAnimation: Boolean = False): Boolean;
var
  a: Integer;
  find_id: DWORD;
begin
  Result := False;

  find_id := FindFrame;

  if FCount <= 2 then BackAnimation := False;

  if BackAnimation then SetLength(FramesArray[find_id].TexturesID, FCount+FCount-2)
    else SetLength(FramesArray[find_id].TexturesID, FCount);

  for a := 0 to FCount-1 do
    if not e_CreateTextureEx(FileName, FramesArray[find_id].TexturesID[a],
                             a*FWidth, 0, FWidth, FHeight) then Exit;

  if BackAnimation then
    for a := 1 to FCount-2 do
      FramesArray[find_id].TexturesID[FCount+FCount-2-a] := FramesArray[find_id].TexturesID[a];

  FramesArray[find_id].FrameWidth := FWidth;
  FramesArray[find_id].FrameHeight := FHeight;
  if Name <> '' then
    FramesArray[find_id].Name := LowerCase(Name)
  else
    FramesArray[find_id].Name := '<noname>';

  if ID <> nil then ID^ := find_id;

  Result := True;
end;

function CreateFramesMem(pData: Pointer; dataSize: LongInt; ID: PDWORD; Name: ShortString;
                         FWidth, FHeight, FCount: Word; BackAnimation: Boolean = False): Boolean;
var
  find_id: DWORD;
  a: Integer;
begin
  Result := False;

  find_id := FindFrame();

  if FCount <= 2 then BackAnimation := False;

  if BackAnimation then SetLength(FramesArray[find_id].TexturesID, FCount+FCount-2)
    else SetLength(FramesArray[find_id].TexturesID, FCount);

  for a := 0 to FCount-1 do
    if not e_CreateTextureMemEx(pData, dataSize, FramesArray[find_id].TexturesID[a],
                                a*FWidth, 0, FWidth, FHeight) then
    begin
      //!!!FreeMem(pData);
      Exit;
    end;

  if BackAnimation then
    for a := 1 to FCount-2 do
      FramesArray[find_id].TexturesID[FCount+FCount-2-a] := FramesArray[find_id].TexturesID[a];

  FramesArray[find_id].FrameWidth := FWidth;
  FramesArray[find_id].FrameHeight := FHeight;
  if Name <> '' then
    FramesArray[find_id].Name := LowerCase(Name)
  else
    FramesArray[find_id].Name := '<noname>';

  if ID <> nil then ID^ := find_id;

  Result := True;
end;

function g_CreateFramesImg (ia: TDynImageDataArray; ID: PDWORD; Name: ShortString; BackAnimation: Boolean = False): Boolean;
var
  find_id: DWORD;
  a, FCount: Integer;
begin
  result := false;
  find_id := FindFrame();

  FCount := length(ia);

  //e_WriteLog(Format('+++ creating %d frames [%s]', [FCount, Name]), MSG_NOTIFY);

  if FCount < 1 then exit;
  if FCount <= 2 then BackAnimation := False;
  if BackAnimation then
    SetLength(FramesArray[find_id].TexturesID, FCount+FCount-2)
  else
    SetLength(FramesArray[find_id].TexturesID, FCount);

  //e_WriteLog(Format('+++ creating %d frames, %dx%d', [FCount, ia[0].width, ia[0].height]), MSG_NOTIFY);

  for a := 0 to FCount-1 do
  begin
    if not e_CreateTextureImg(ia[a], FramesArray[find_id].TexturesID[a]) then exit;
    //e_WriteLog(Format('+++   frame %d, %dx%d', [a, ia[a].width, ia[a].height]), MSG_NOTIFY);
  end;

  if BackAnimation then
  begin
    for a := 1 to FCount-2 do
    begin
      FramesArray[find_id].TexturesID[FCount+FCount-2-a] := FramesArray[find_id].TexturesID[a];
    end;
  end;

  FramesArray[find_id].FrameWidth := ia[0].width;
  FramesArray[find_id].FrameHeight := ia[0].height;
  if Name <> '' then
    FramesArray[find_id].Name := LowerCase(Name)
  else
    FramesArray[find_id].Name := '<noname>';

  if ID <> nil then ID^ := find_id;

  result := true;
end;

function g_Frames_CreateWAD(ID: PDWORD; Name: ShortString; Resource: string;
                            FWidth, FHeight, FCount: Word; BackAnimation: Boolean = False): Boolean;
var
  WAD: TWADFile;
  FileName: string;
  TextureData: Pointer;
  ResourceLength: Integer;
begin
  Result := False;

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
    Exit;
  end;

  if not CreateFramesMem(TextureData, ResourceLength, ID, Name, FWidth, FHeight, FCount, BackAnimation) then
  begin
    WAD.Free();
    Exit;
  end;

  WAD.Free();

  Result := True;
end;

function g_Frames_CreateMemory(ID: PDWORD; Name: ShortString; pData: Pointer; dataSize: LongInt;
                               FWidth, FHeight, FCount: Word; BackAnimation: Boolean = False): Boolean;
begin
  Result := CreateFramesMem(pData, dataSize, ID, Name, FWidth, FHeight, FCount, BackAnimation);
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

function g_Frames_Dup(NewName, OldName: ShortString): Boolean;
var
  find_id, b: DWORD;
  a, c: Integer;
begin
  Result := False;

  if not g_Frames_Get(b, OldName) then Exit;

  find_id := FindFrame();

  FramesArray[find_id].Name := LowerCase(NewName);
  FramesArray[find_id].FrameWidth := FramesArray[b].FrameWidth;
  FramesArray[find_id].FrameHeight := FramesArray[b].FrameHeight;

  c := High(FramesArray[b].TexturesID);
  SetLength(FramesArray[find_id].TexturesID, c+1);

  for a := 0 to c do
    FramesArray[find_id].TexturesID[a] := FramesArray[b].TexturesID[a];

  Result := True;
end;

procedure g_Frames_DeleteByName(FramesName: ShortString);
var
  a: DWORD;
  b: Integer;
begin
  if FramesArray = nil then Exit;

  FramesName := LowerCase(FramesName);

  for a := 0 to High(FramesArray) do
    if FramesArray[a].Name = FramesName then
    begin
      if FramesArray[a].TexturesID <> nil then
        for b := 0 to High(FramesArray[a].TexturesID) do
          e_DeleteTexture(FramesArray[a].TexturesID[b]);
      FramesArray[a].TexturesID := nil;
      FramesArray[a].Name := '';
      FramesArray[a].FrameWidth := 0;
      FramesArray[a].FrameHeight := 0;
    end;
end;

procedure g_Frames_DeleteByID(ID: DWORD);
var
  b: Integer;
begin
  if FramesArray = nil then Exit;

  if FramesArray[ID].TexturesID <> nil then
    for b := 0 to High(FramesArray[ID].TexturesID) do
      e_DeleteTexture(FramesArray[ID].TexturesID[b]);
  FramesArray[ID].TexturesID := nil;
  FramesArray[ID].Name := '';
  FramesArray[ID].FrameWidth := 0;
  FramesArray[ID].FrameHeight := 0;
end;

procedure g_Frames_DeleteAll;
var
  a: DWORD;
  b: DWORD;
begin
  if FramesArray = nil then Exit;

  for a := 0 to High(FramesArray) do
    if FramesArray[a].TexturesID <> nil then
    begin
      for b := 0 to High(FramesArray[a].TexturesID) do
        e_DeleteTexture(FramesArray[a].TexturesID[b]);
      FramesArray[a].TexturesID := nil;
      FramesArray[a].Name := '';
      FramesArray[a].FrameWidth := 0;
      FramesArray[a].FrameHeight := 0;
    end;

  FramesArray := nil;
end;

function g_Frames_Get(out ID: DWORD; FramesName: ShortString): Boolean;
var
  a: DWORD;
begin
  Result := False;

  if FramesArray = nil then
    Exit;

  FramesName := LowerCase(FramesName);

  for a := 0 to High(FramesArray) do
    if FramesArray[a].Name = FramesName then
    begin
      ID := a;
      Result := True;
      Break;
    end;

  if not Result then
    g_FatalError(Format(_lc[I_GAME_ERROR_FRAMES], [FramesName]));
end;

function g_Frames_GetTexture(out ID: DWORD; FramesName: ShortString; Frame: Word): Boolean;
var
  a: DWORD;
begin
  Result := False;

  if FramesArray = nil then
    Exit;

  FramesName := LowerCase(FramesName);

  for a := 0 to High(FramesArray) do
    if FramesArray[a].Name = FramesName then
      if Frame <= High(FramesArray[a].TexturesID) then
      begin
        ID := FramesArray[a].TexturesID[Frame];
        Result := True;
        Break;
      end;

  if not Result then
    g_FatalError(Format(_lc[I_GAME_ERROR_FRAMES], [FramesName]));
end;

function g_Frames_Exists(FramesName: string): Boolean;
var
  a: DWORD;
begin
  Result := False;

  if FramesArray = nil then Exit;

  FramesName := LowerCase(FramesName);

  for a := 0 to High(FramesArray) do
    if FramesArray[a].Name = FramesName then
    begin
      Result := True;
      Exit;
    end;
end;

procedure DumpTextureNames();
var
  i: Integer;
begin
  e_WriteLog('BEGIN Textures:', TMsgType.Notify);
  for i := 0 to High(TexturesArray) do
    e_WriteLog('   '+IntToStr(i)+'. '+TexturesArray[i].Name, TMsgType.Notify);
  e_WriteLog('END Textures.', TMsgType.Notify);

  e_WriteLog('BEGIN Frames:', TMsgType.Notify);
  for i := 0 to High(FramesArray) do
    e_WriteLog('   '+IntToStr(i)+'. '+FramesArray[i].Name, TMsgType.Notify);
  e_WriteLog('END Frames.', TMsgType.Notify);
end;

{ TAnimation }

constructor TAnimation.Create(FramesID: DWORD; Loop: Boolean; Speed: Byte);
begin
  ID := FramesID;

  FMinLength := 0;
  FLoop := Loop;
  FSpeed := Speed;
  FEnabled := True;
  FCurrentFrame := 0;
  FPlayed := False;
  FAlpha := 0;
  FWidth := FramesArray[ID].FrameWidth;
  FHeight := FramesArray[ID].FrameHeight;
end;

destructor TAnimation.Destroy;
begin
  inherited;
end;

procedure TAnimation.Draw(X, Y: Integer; Mirror: TMirrorType);
begin
  if not FEnabled then
    Exit;

  e_DrawAdv(FramesArray[ID].TexturesID[FCurrentFrame], X, Y, FAlpha,
            True, FBlending, 0, nil, Mirror);
  //e_DrawQuad(X, Y, X+FramesArray[ID].FrameWidth-1, Y+FramesArray[ID].FrameHeight-1, 0, 255, 0);
end;

procedure TAnimation.Update();
begin
  if not FEnabled then
    Exit;

  FCounter := FCounter + 1;

  if FCounter >= FSpeed then
  begin // Ожидание между кадрами закончилось
    if FRevert then
      begin // Обратный порядок кадров
      // Дошли до конца анимации. Возможно, ждем еще:
        if FCurrentFrame = 0 then
          if Length(FramesArray[ID].TexturesID) * FSpeed +
               FCounter < FMinLength then
            Exit;

        FCurrentFrame := FCurrentFrame - 1;
        FPlayed := FCurrentFrame < 0;

      // Повторять ли анимацию по кругу:
        if FPlayed then
          if FLoop then
            FCurrentFrame := High(FramesArray[ID].TexturesID)
          else
            FCurrentFrame := FCurrentFrame + 1;

        FCounter := 0;
      end
    else
      begin // Прямой порядок кадров
      // Дошли до конца анимации. Возможно, ждем еще:
        if FCurrentFrame = High(FramesArray[ID].TexturesID) then
          if Length(FramesArray[ID].TexturesID) * FSpeed +
               FCounter < FMinLength then
            Exit;

        FCurrentFrame := FCurrentFrame + 1;
        FPlayed := (FCurrentFrame > High(FramesArray[ID].TexturesID));

      // Повторять ли анимацию по кругу:
        if FPlayed then
          if FLoop then
            FCurrentFrame := 0
          else
            FCurrentFrame := FCurrentFrame - 1;

        FCounter := 0;
      end;
  end;
end;

procedure TAnimation.Reset();
begin
  if FRevert then
    FCurrentFrame := High(FramesArray[ID].TexturesID)
  else
    FCurrentFrame := 0;

  FCounter := 0;
  FPlayed := False;
end;

procedure TAnimation.Disable;
begin
  FEnabled := False;
end;

procedure TAnimation.Enable;
begin
  FEnabled := True;
end;

procedure TAnimation.DrawEx(X, Y: Integer; Mirror: TMirrorType; RPoint: TDFPoint;
                            Angle: SmallInt);
begin
  if not FEnabled then
    Exit;

  e_DrawAdv(FramesArray[ID].TexturesID[FCurrentFrame], X, Y, FAlpha,
            True, FBlending, Angle, @RPoint, Mirror);
end;

function TAnimation.TotalFrames(): Integer;
begin
  Result := Length(FramesArray[ID].TexturesID);
end;

procedure TAnimation.Revert(r: Boolean);
begin
  FRevert := r;
  Reset();
end;

procedure TAnimation.SaveState (st: TStream);
begin
  if (st = nil) then exit;

  utils.writeSign(st, 'ANIM');
  utils.writeInt(st, Byte(0)); // version
  // Счетчик ожидания между кадрами
  utils.writeInt(st, Byte(FCounter));
  // Текущий кадр
  utils.writeInt(st, LongInt(FCurrentFrame));
  // Проиграна ли анимация целиком
  utils.writeBool(st, FPlayed);
  // Alpha-канал всей текстуры
  utils.writeInt(st, Byte(FAlpha));
  // Размытие текстуры
  utils.writeInt(st, Byte(FBlending));
  // Время ожидания между кадрами
  utils.writeInt(st, Byte(FSpeed));
  // Зациклена ли анимация
  utils.writeBool(st, FLoop);
  // Включена ли
  utils.writeBool(st, FEnabled);
  // Ожидание после проигрывания
  utils.writeInt(st, Byte(FMinLength));
  // Обратный ли порядок кадров
  utils.writeBool(st, FRevert);
end;

procedure TAnimation.LoadState (st: TStream);
begin
  if (st = nil) then exit;

  if not utils.checkSign(st, 'ANIM') then raise XStreamError.Create('animation chunk expected');
  if (utils.readByte(st) <> 0) then raise XStreamError.Create('invalid animation chunk version');
  // Счетчик ожидания между кадрами
  FCounter := utils.readByte(st);
  // Текущий кадр
  FCurrentFrame := utils.readLongInt(st);
  // Проиграна ли анимация целиком
  FPlayed := utils.readBool(st);
  // Alpha-канал всей текстуры
  FAlpha := utils.readByte(st);
  // Размытие текстуры
  FBlending := utils.readBool(st);
  // Время ожидания между кадрами
  FSpeed := utils.readByte(st);
  // Зациклена ли анимация
  FLoop := utils.readBool(st);
  // Включена ли
  FEnabled := utils.readBool(st);
  // Ожидание после проигрывания
  FMinLength := utils.readByte(st);
  // Обратный ли порядок кадров
  FRevert := utils.readBool(st);
end;


var
  ltexid: GLuint = 0;

function g_Texture_Light(): Integer;
const
  Radius: Integer = 128;
var
  tex, tpp: PByte;
  x, y, a: Integer;
  dist: Double;
begin
  if ltexid = 0 then
  begin
    GetMem(tex, (Radius*2)*(Radius*2)*4);
    tpp := tex;
    for y := 0 to Radius*2-1 do
    begin
      for x := 0 to Radius*2-1 do
      begin
        dist := 1.0-sqrt((x-Radius)*(x-Radius)+(y-Radius)*(y-Radius))/Radius;
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
          a := round(dist*255);
          if (a < 0) then a := 0 else if (a > 255) then a := 255;
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
  end;

  result := ltexid;
end;

end.
