unit g_textures;

interface

uses windows, e_graphics;

type
  PAnimRec = ^TAnimRec;
  TAnimRec = packed record
   Counter: Byte;
   CurrentFrame: Integer;
   Played: Boolean;
   Alpha: Byte;
   Blending: Boolean;
   Speed: Byte;
   Loop: Boolean;
   Enabled: Boolean;
   MinLength: Byte;
   Revert: Boolean;
  end;

  TAnimation = class(TObject)
   private
    ID:            DWORD;
    FAlpha:        Byte;
    FBlending:     Boolean;
    FCounter:      Byte;
    FSpeed:        Byte;
    FCurrentFrame: Integer;
    FLoop:         Boolean;
    FEnabled:      Boolean;
    FPlayed:       Boolean;
    FHeight:       Word;
    FWidth:        Word;
    FMinLength:    Byte;
    FRevert:       Boolean;
   public
    constructor Create(FramesID: DWORD; Loop: Boolean; Speed: Byte);
    destructor  Destroy; override;
    procedure   Draw(X, Y: Integer; Mirror: TMirrorType);
    procedure   DrawEx(X, Y: Integer; Mirror: TMirrorType; RPoint: TPoint;
                       Angle: SmallInt);
    procedure   Reset();
    procedure   Update();
    procedure   Enable();
    procedure   Disable();
    procedure   Revert(r: Boolean);
    procedure   Save(Rec: PAnimRec);
    procedure   Load(Rec: PAnimRec);
    property    Played: Boolean read FPlayed;
    property    Enabled: Boolean read FEnabled;
    property    Loop: Boolean read FLoop write FLoop;
    property    Speed: Byte read FSpeed write FSpeed;
    property    MinLength: Byte read FMinLength write FMinLength;
    property    CurrentFrame: Integer read FCurrentFrame;
    property    Counter: Byte read FCounter;
    function    TotalFrames(): Integer;
    property    Blending: Boolean read FBlending write FBlending;
    property    Alpha: Byte read FAlpha write FAlpha;
    property    FramesID: DWORD read ID;
    property    Width: Word read FWidth;
    property    Height: Word read FHeight;
  end;

function g_Texture_CreateWAD(var ID: DWORD; Resource: string): Boolean;
function g_Texture_CreateFile(var ID: DWORD; FileName: string): Boolean;
function g_Texture_CreateWADEx(TextureName: ShortString; Resource: string): Boolean;
function g_Texture_CreateFileEx(TextureName: ShortString; FileName: string): Boolean;
function g_Texture_Get(TextureName: ShortString; var ID: DWORD): Boolean;
procedure g_Texture_Delete(TextureName: ShortString);
procedure g_Texture_DeleteAll();

function g_Frames_CreateWAD(ID: PDWORD; Name: ShortString; Resource: string;
                            FWidth, FHeight, FCount: Word; BackAnimation: Boolean = False): Boolean;
function g_Frames_CreateFile(ID: PDWORD; Name: ShortString; FileName: string;
                             FWidth, FHeight, FCount: Word; BackAnimation: Boolean = False): Boolean;
function g_Frames_CreateMemory(ID: PDWORD; Name: ShortString; pData: Pointer;
                               FWidth, FHeight, FCount: Word; BackAnimation: Boolean = False): Boolean;
//function g_Frames_CreateRevert(ID: PDWORD; Name: ShortString; Frames: string): Boolean;
function g_Frames_Get(var ID: DWORD; FramesName: ShortString): Boolean;
function g_Frames_GetTexture(var ID: DWORD; FramesName: ShortString; Frame: Word): Boolean;
function g_Frames_Exists(FramesName: string): Boolean;
procedure g_Frames_DeleteByName(FramesName: ShortString);
procedure g_Frames_DeleteByID(ID: DWORD);
procedure g_Frames_DeleteAll();

implementation

uses
  g_game, e_log, g_basic, SysUtils, g_console, WADEDITOR;

type
  _TTexture = record
   Name: ShortString;
   ID: DWORD;
   Width, Height: Word;
  end;

  TFrames = record
   TexturesID: array of DWORD;
   Name: ShortString;
   FrameWidth,
   FrameHeight: Word;
  end;

var
  TexturesArray: array of _TTexture = nil;
  FramesArray: array of TFrames = nil;

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

function g_Texture_CreateWAD(var ID: DWORD; Resource: string): Boolean;
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

 if WAD.GetResource(SectionName, ResourceName, TextureData, ResourceLength) then
 begin
  if e_CreateTextureMem(TextureData, ID) then Result := True;
  FreeMem(TextureData);
 end
  else
 begin
  e_WriteLog(Format('Error loading texture %s', [Resource]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
 end;
 WAD.Destroy;
end;

function g_Texture_CreateFile(var ID: DWORD; FileName: String): Boolean;
begin
 Result := True;
 if not e_CreateTexture(FileName, ID) then
 begin
  e_WriteLog(Format('Error loading texture %s', [FileName]), MSG_WARNING);
  Result := False;
 end;
end;

function g_Texture_CreateWADEx(TextureName: ShortString; Resource: String): Boolean;
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

 if WAD.GetResource(SectionName, ResourceName, TextureData, ResourceLength) then
 begin
  Result := e_CreateTextureMem(TextureData, TexturesArray[find_id].ID);
  FreeMem(TextureData);
  if Result then
  begin
   e_GetTextureSize(TexturesArray[find_id].ID, @TexturesArray[find_id].Width,
                    @TexturesArray[find_id].Height);
   TexturesArray[find_id].Name := LowerCase(TextureName);
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
  else e_WriteLog(Format('Error loading texture %s', [FileName]), MSG_WARNING);
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
 if Name <> '' then FramesArray[find_id].Name := LowerCase(Name)
  else FramesArray[find_id].Name := '<noname>';

 if ID <> nil then ID^ := find_id;

 Result := True;
end;

function CreateFramesMem(pData: Pointer; ID: PDWORD; Name: ShortString;
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
  if not e_CreateTextureMemEx(pData, FramesArray[find_id].TexturesID[a],
                              a*FWidth, 0, FWidth, FHeight) then
  begin
   FreeMem(pData);
   Exit;
  end;

 if BackAnimation then
  for a := 1 to FCount-2 do
   FramesArray[find_id].TexturesID[FCount+FCount-2-a] := FramesArray[find_id].TexturesID[a];

 FramesArray[find_id].FrameWidth := FWidth;
 FramesArray[find_id].FrameHeight := FHeight;
 if Name <> '' then FramesArray[find_id].Name := LowerCase(Name)
  else FramesArray[find_id].Name := '<noname>';

 if ID <> nil then ID^ := find_id;

 FreeMem(pData);

 Result := True;
end;

function g_Frames_CreateWAD(ID: PDWORD; Name: ShortString; Resource: string;
                            FWidth, FHeight, FCount: Word; BackAnimation: Boolean = False): Boolean;
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

 if not WAD.GetResource(SectionName, ResourceName, TextureData, ResourceLength) then
 begin
  WAD.Destroy;
  e_WriteLog(Format('Error loading texture %s', [Resource]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
  Exit;
 end;

 if not CreateFramesMem(TextureData, ID, Name, FWidth, FHeight, FCount, BackAnimation) then
 begin
  WAD.Destroy;
  Exit;
 end;

 WAD.Destroy;

 Result := True;
end;

function g_Frames_CreateMemory(ID: PDWORD; Name: ShortString; pData: Pointer;
                              FWidth, FHeight, FCount: Word; BackAnimation: Boolean = False): Boolean;
begin
 Result := CreateFramesMem(pData, ID, Name, FWidth, FHeight, FCount, BackAnimation);
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

function g_Frames_Get(var ID: DWORD; FramesName: ShortString): Boolean;
var
  a: DWORD;
begin
 Result := False;

 if FramesArray = nil then Exit;

 FramesName := LowerCase(FramesName);

 for a := 0 to High(FramesArray) do
  if FramesArray[a].Name = FramesName then
  begin
   ID := a;
   Result := True;
   Break;
  end;

 if not Result then g_Console_Add('Frames '+FramesName+' not found');
end;

function g_Frames_GetTexture(var ID: DWORD; FramesName: ShortString; Frame: Word): Boolean;
var
  a: DWORD;
begin
 Result := False;

 if FramesArray = nil then Exit;

 FramesName := LowerCase(FramesName);

 for a := 0 to High(FramesArray) do
  if FramesArray[a].Name = FramesName then
   if Frame <= High(FramesArray[a].TexturesID) then
   begin
    ID := FramesArray[a].TexturesID[Frame];
    Result := True;
    Break;
   end;

 if not Result then g_Console_Add('Frames '+FramesName+' not found');
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
 if not FEnabled then Exit;

 e_DrawAdv(FramesArray[ID].TexturesID[FCurrentFrame], X, Y, FAlpha, True,
           FBlending, 0, nil, Mirror);
end;

procedure TAnimation.Update();
begin
 if not FEnabled then Exit;

 FCounter := FCounter+1;

 if FCounter >= FSpeed then
 begin
  if FRevert then
  begin
   if FCurrentFrame = 0 then
   if Length(FramesArray[ID].TexturesID)*FSpeed+FCounter < FMinLength then Exit;

   FCurrentFrame := FCurrentFrame-1;

   FPlayed := FCurrentFrame < 0;

   if FPlayed then
    if FLoop then FCurrentFrame := High(FramesArray[ID].TexturesID) else
     FCurrentFrame := FCurrentFrame+1;

   FCounter := 0;
  end
   else
  begin
   if FCurrentFrame = High(FramesArray[ID].TexturesID) then
   if Length(FramesArray[ID].TexturesID)*FSpeed+FCounter < FMinLength then Exit;

   FCurrentFrame := FCurrentFrame+1;

   FPlayed := (FCurrentFrame > High(FramesArray[ID].TexturesID));// and not FLoop;

   if FPlayed then
    if FLoop then FCurrentFrame := 0 else FCurrentFrame := FCurrentFrame-1;

   FCounter := 0;
  end;
 end;
end;

procedure TAnimation.Reset();
begin
 if FRevert then FCurrentFrame := High(FramesArray[ID].TexturesID)
  else FCurrentFrame := 0;

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

procedure TAnimation.DrawEx(X, Y: Integer; Mirror: TMirrorType; RPoint: TPoint;
                            Angle: SmallInt);
begin
 if not FEnabled then Exit;

 e_DrawAdv(FramesArray[ID].TexturesID[FCurrentFrame], X, Y, FAlpha, True,
           FBlending, Angle, @RPoint, Mirror);
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

procedure TAnimation.Load(Rec: PAnimRec);
begin
 FCounter := Rec^.Counter;
 FCurrentFrame := Rec^.CurrentFrame;
 FPlayed := Rec^.Played;
 FAlpha := Rec^.Alpha;
 FBlending := Rec^.Blending;
 FSpeed := Rec^.Speed;
 FLoop := Rec^.Loop;
 FEnabled := Rec^.Enabled;
 FMinLength := Rec^.MinLength;
 FRevert := Rec^.Revert;
end;

procedure TAnimation.Save(Rec: PAnimRec);
begin
 Rec^.Counter := FCounter;
 Rec^.CurrentFrame := FCurrentFrame;
 Rec^.Played := FPlayed;
 Rec^.Alpha := FAlpha;
 Rec^.Blending := FBlending;
 Rec^.Speed := FSpeed;
 Rec^.Loop := FLoop;
 Rec^.Enabled := FEnabled;
 Rec^.MinLength := FMinLength;
 Rec^.Revert := FRevert;
end;

end.
