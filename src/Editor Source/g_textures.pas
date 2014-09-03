unit g_textures;

interface

uses
  windows, e_graphics;

type
  TAnimation = class(TObject)
   private
    ID:            DWORD;
    FBlending:     Boolean;
    FAlpha:        Byte;
    FAlphaChannel: Boolean;
    FCounter:      Byte;
    FSpeed:        Byte;
    FLoop:         Boolean;
    FEnabled:      Boolean;
   public
    constructor Create(TextureID: DWORD; Loop: Boolean; Speed: Byte);
    destructor  Destroy; override;
    procedure   Draw(X, Y: Integer; Mirror: TMirrorType);
    procedure   Reset;
    procedure   Update;
    function    Played: Boolean;
    procedure   Enable;
    procedure   Disable;
    property    Enabled: Boolean read FEnabled;
    property    Blending: Boolean read FBlending write FBlending;
    property    Alpha: Byte read FAlpha write FAlpha;
    property    AlphaChannel: Boolean read FAlphaChannel write FAlphaChannel;
  end;

function g_SimpleCreateTextureWAD(var ID: DWORD; Resource: String): Boolean;
function g_SimpleCreateTextureFile(var ID: DWORD; FileName: String): Boolean;
function g_CreateTextureWAD(TextureName: ShortString; Resource: String): Boolean;
function g_CreateTextureFile(TextureName: ShortString; FileName: String): Boolean;
procedure g_DrawTexture(TextureName: ShortString; X, Y: Integer;
                        Blending, AlphaChannel: Boolean; Alpha: Byte);
function g_GetTexture(TextureName: ShortString; var ID: DWORD): Boolean;
procedure g_DeleteTexture(TextureName: ShortString);
procedure g_DeleteAllTextures;

function g_SimpleCreateAnimationTextureWAD(var ID: DWORD; Resource: String): Boolean;
function g_SimpleCreateAnimationTextureFile(var ID: DWORD; FileName: String): Boolean;
function g_CreateAnimationTextureWAD(AnimationName: ShortString; Resource: String): Boolean;
function g_CreateAnimationTextureFile(AnimationName: ShortString; FileName: String): Boolean;
function g_GetAnimationTexture(AnimationName: ShortString; var ID: DWORD): Boolean;
procedure g_DeleteAnimationTexture(AnimationName: ShortString);
procedure g_DeleteAllAnimationTextures;

implementation

uses
  e_log, g_wad, IniFiles, SysUtils;

type
  _TTexture = record
   Name: ShortString;
   ID: DWORD;
   Width, Height: Word;
  end;

  _TAnimation = record
   Name: ShortString;
   ID: DWORD;
   FrameWidth,
   FrameHeight: Word;
  end;

var
  TexturesArray:   Array of _TTexture   = nil;
  AnimationsArray: Array of _TAnimation = nil;

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

function g_SimpleCreateTextureWAD(var ID: DWORD; Resource: String): Boolean;
var
  WAD: TWADReader;
  FileName,
  SectionName,
  ResourceName: String;
  TextureData: Pointer;
  ResourceLength: Integer;
begin
 Result := False;
 g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

 WAD := TWADReader.Create(FileName);
 if WAD.Read(SectionName, ResourceName, TextureData, ResourceLength) then
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

function g_SimpleCreateTextureFile(var ID: DWORD; FileName: String): Boolean;
begin
 Result := True;
 if not e_CreateTexture(FileName, ID) then
 begin
  e_WriteLog(Format('Error loading texture %s', [FileName]), MSG_WARNING);
  Result := False;
 end;
end;

function g_CreateTextureWAD(TextureName: ShortString; Resource: String): Boolean;
var
  WAD: TWADReader;
  FileName,
  SectionName,
  ResourceName: String;
  TextureData: Pointer;
  find_id: DWORD;
  ResourceLength: Integer;
begin
 g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

 find_id := FindTexture;

 WAD := TWADReader.Create(FileName);
 if WAD.Read(SectionName, ResourceName, TextureData, ResourceLength) then
 begin
  Result := e_CreateTextureMem(TextureData, TexturesArray[find_id].ID);
  FreeMem(TextureData);
  if Result then
  begin
   e_GetTextureSize(TexturesArray[find_id].ID, TexturesArray[find_id].Width,
                    TexturesArray[find_id].Height);
   TexturesArray[find_id].Name := TextureName;
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

function g_CreateTextureFile(TextureName: ShortString; FileName: String): Boolean;
var
  find_id: DWORD;
begin
 find_id := FindTexture;

 Result := e_CreateTexture(FileName, TexturesArray[find_id].ID);
 if Result then
 begin
  TexturesArray[find_id].Name := TextureName;
  e_GetTextureSize(TexturesArray[find_id].ID, TexturesArray[find_id].Width,
                   TexturesArray[find_id].Height); 
 end
  else e_WriteLog(Format('Error loading texture %s', [FileName]), MSG_WARNING);
end;

procedure g_DrawTexture(TextureName: ShortString; X, Y: Integer;
                        Blending, AlphaChannel: Boolean; Alpha: Byte);
var
  a: DWORD;
begin
 if TexturesArray = nil then Exit;

 for a := 0 to High(TexturesArray) do
  if TexturesArray[a].Name = TextureName then
  begin
   e_Draw(TexturesArray[a].ID, X, Y, Alpha, AlphaChannel, Blending);
   Break;
  end;
end;

function g_GetTexture(TextureName: ShortString; var ID: DWORD): Boolean;
var
  a: DWORD;
begin
 Result := False;
 
 if TexturesArray = nil then Exit;

 for a := 0 to High(TexturesArray) do
  if TexturesArray[a].Name = TextureName then
  begin
   ID := TexturesArray[a].ID;
   Result := True;
   Break;
  end;
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

function FindAnimation: DWORD;
var
  i: integer;
begin
 if AnimationsArray <> nil then
 for i := 0 to High(AnimationsArray) do
  if AnimationsArray[i].Name = '' then
  begin
   Result := i;
   Exit;
  end;

 if AnimationsArray = nil then
 begin
  SetLength(AnimationsArray, 8);
  Result := 0;
 end
  else
 begin
  Result := High(AnimationsArray) + 1;
  SetLength(AnimationsArray, Length(AnimationsArray) + 8);
 end;
end;

function g_SimpleCreateAnimationTextureWAD(var ID: DWORD; Resource: String): Boolean;
var
  WAD: TWADReader;
  FileName,
  SectionName,
  ResourceName: String;
  TextureData: Pointer;
  ResourceLength: Integer;
begin
 Result := True;

 g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

 WAD := TWADReader.Create(FileName);
 if WAD.Read(SectionName, ResourceName, TextureData, ResourceLength) then
 begin
  if e_CreateAnimationTextureMem(TextureData, ID) then FreeMem(TextureData)
   else Result := False;
 end
  else
 begin
  e_WriteLog(Format('Error loading texture %s', [Resource]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
  Result := False;
 end;

 WAD.Destroy;
end;

function g_CreateAnimationTextureWAD(AnimationName: ShortString; Resource: String): Boolean;
var
  WAD: TWADReader;
  FileName,
  SectionName,
  ResourceName: String;
  TextureData: Pointer;
  find_id: DWORD;
  ResourceLength: Integer;
begin
 Result := True;

 g_ProcessResourceStr(Resource, FileName, SectionName, ResourceName);

 find_id := FindAnimation;

 WAD := TWADReader.Create(FileName);
 if WAD.Read(SectionName, ResourceName, TextureData, ResourceLength) then
 begin
  if e_CreateAnimationTextureMem(TextureData, AnimationsArray[find_id].ID) then
  begin
   e_GetAnimationTextureSize(TexturesArray[find_id].ID, TexturesArray[find_id].Width,
                             TexturesArray[find_id].Height);
   FreeMem(TextureData);
   TexturesArray[find_id].Name := ResourceName;
  end else Result := False;
 end
  else
 begin
  e_WriteLog(Format('Error loading texture %s', [Resource]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
  Result := False;
 end;

 WAD.Destroy;
end;

function g_SimpleCreateAnimationTextureFile(var ID: DWORD; FileName: String): Boolean;
begin
 Result := e_CreateAnimationTexture(FileName, ID);
end;

function g_CreateAnimationTextureFile(AnimationName: ShortString; FileName: String): Boolean;
var
  find_id: DWORD;
begin
 Result := False;

 find_id := FindAnimation;

 if not e_CreateAnimationTexture(FileName, AnimationsArray[find_id].ID) then Exit;

 AnimationsArray[find_id].Name := AnimationName;
 e_GetAnimationTextureSize(AnimationsArray[find_id].ID, AnimationsArray[find_id].FrameWidth,
                           AnimationsArray[find_id].FrameHeight);

 Result := True;
end;

procedure g_DeleteAnimationTexture(AnimationName: ShortString);
var
  a: DWORD;
begin
 if AnimationsArray = nil then Exit;

 for a := 0 to High(AnimationsArray) do
  if AnimationsArray[a].Name = AnimationName then
  begin
   e_DeleteFramesTexture(AnimationsArray[a].ID);
   AnimationsArray[a].Name := '';
   AnimationsArray[a].ID := 0;
   AnimationsArray[a].FrameWidth := 0;
   AnimationsArray[a].FrameHeight := 0;
  end;
end;

procedure g_DeleteAllAnimationTextures;
var
  a: DWORD;
begin
 if AnimationsArray = nil then Exit;

 for a := 0 to High(AnimationsArray) do
  if AnimationsArray[a].Name <> '' then
   e_DeleteFramesTexture(AnimationsArray[a].ID);

 AnimationsArray := nil;
end;

function g_GetAnimationTexture(AnimationName: ShortString; var ID: DWORD): Boolean;
var
  a: DWORD;
begin
 Result := False;

 if AnimationsArray = nil then Exit;

 for a := 0 to High(AnimationsArray) do
  if AnimationsArray[a].Name = AnimationName then
  begin
   ID := AnimationsArray[a].ID;
   Result := True;
   Break;
  end;
end;

{ TAnimation }

constructor TAnimation.Create(TextureID: DWORD; Loop: Boolean; Speed: Byte);
begin
 e_CreateAnimation(TextureID, ID);

 Self.FLoop := Loop;
 Self.FSpeed := Speed;
 Self.FEnabled := True; 
end;

destructor TAnimation.Destroy;
begin
  e_DeleteAnimation(ID);
   
  inherited;
end;

procedure TAnimation.Draw(X, Y: Integer; Mirror: TMirrorType);
begin
 { TODO 5 : Сделать отражение }
 if not(FEnabled) then Exit;
 e_DrawAnimation(ID, FBlending, FAlpha, FAlphaChannel, X, Y);
end;

procedure TAnimation.Update;
begin
 if not(FEnabled) then Exit;

 Inc(FCounter);
 
 if FCounter = FSpeed then
 begin
  if not e_AnimationLastFrame(ID) then
   e_PlayAnimation(ID)
   else if FLoop then e_ResetAnimation(ID);
  FCounter := 0;
 end;
end;

procedure TAnimation.Reset;
begin
 e_ResetAnimation(ID);
end;

function TAnimation.Played: Boolean;
begin
 Result := e_AnimationLastFrame(ID); 
end;

procedure TAnimation.Disable;
begin
 FEnabled := False;
end;

procedure TAnimation.Enable;
begin
 FEnabled := True;
end;

end.

