unit g_map;

interface

uses e_graphics, g_basic, MAPSTRUCT, windows, g_textures, g_phys, WADEDITOR;

type
  TMapInfo = record
   Map:           string;
   Name:          string;
   Description:   string;
   Author:        string;
   MusicName:     string;
   SkyName:       string;
   Height:        Word;
   Width:         Word;
  end;

  PRespawnPoint = ^TRespawnPoint;
  TRespawnPoint = record
   X, Y:      Integer;
   Direction: TDirection;
   PointType: Byte;
  end;

  PFlagPoint = ^TFlagPoint;
  TFlagPoint = TRespawnPoint;

  PRenderPanel = ^TRenderPanel;
  TRenderPanel = record
   X, Y:          Integer;
   Width,
   Height:        Word;
   TextureHeight: Word;
   TextureWidth:  Word;
   Hide:          Boolean;
   Enabled:       Boolean;
   WallID:        DWORD;
   SwitchTexture: Boolean;
   case Anim: Boolean of
    False: (TextureID:  DWORD;
            TextureID2: DWORD;
            Alpha:      Byte;
            Blending:   Boolean);
    True:  (Animation:  TAnimation;
            Animation2: TAnimation);
  end;

  TRPanelArray = array of TRenderPanel;

  TFlag = record
   Obj:         TObj;
   RespawnType: Byte;
   State:       Byte;
   Count:       Integer;
   Animation:   TAnimation;
   Direction:   TDirection;
  end;

  TMapSaveRec = packed record
   state_count: Integer;
  end;

function g_Map_Load(Res: string): Boolean;
function g_Map_GetMapInfo(Res: string): TMapInfo;
function g_Map_GetMapsList(WADName: string): SArray;
function g_Map_Exist(Res: string): Boolean;
procedure g_Map_Free();
procedure g_Map_Update();
procedure g_Map_DrawPanels(PanelType: Word);
procedure g_Map_DrawBack(dx, dy: Integer);
function g_Map_CollidePanel(X, Y: Integer; Width, Height: Word; PanelType: Word): Boolean;
procedure g_Map_EnableWall(RenderID: DWORD);
procedure g_Map_DisableWall(RenderID: DWORD);
procedure g_Map_SwitchTexture(PanelType: Word; ID: DWORD; AnimLoop: Byte = 0);
procedure g_Map_SetLift(ID: DWORD; t: Integer);
procedure g_Map_ReAdd_DieTriggers;

function g_Map_GetPoint(PointType: Byte; var RespawnPoint: TRespawnPoint): Boolean;
function g_Map_GetPointCount(PointType: Byte): Word;

function g_Map_HaveFlagPoints(): Boolean;

procedure g_Map_ResetFlag(Flag: Byte);
procedure g_Map_DrawFlags();

function g_Map_SaveState(var p: Pointer): Integer;
procedure g_Map_LoadState(p: Pointer; len: Integer);

const
  RESPAWNPOINT_PLAYER1 = 1;
  RESPAWNPOINT_PLAYER2 = 2;
  RESPAWNPOINT_DM      = 3;
  RESPAWNPOINT_RED     = 4;
  RESPAWNPOINT_BLUE    = 5;

  FLAG_NONE = 0;
  FLAG_RED  = 1;
  FLAG_BLUE = 2;
  FLAG_DOM  = 3;

  FLAG_STATE_NORMAL   = 0;
  FLAG_STATE_DROPPED  = 1;
  FLAG_STATE_CAPTURED = 2;

  FLAG_TIME = 720;

var
  gWalls: array of record
                    Rect: TRectWH;
                    Door: Boolean; 
                    Enabled: Boolean;
                   end;
  gWater, gAcid1,
  gAcid2: array of TRectWH;
  gSteps: array of TRectWH;
  gLifts: array of record
                    Rect: TRectWH;
                    LiftType: Byte;
                   end;
  gBlockMon: array of TRectWH;
  gRenderWalls:       TRPanelArray;
  gRenderBackgrounds: TRPanelArray;
  gRenderForegrounds: TRPanelArray;
  gFlags: array [FLAG_RED..FLAG_BLUE] of TFlag;
  //gDOMFlags: array of TFlag;
  gMapInfo: TMapInfo;
  gBackSize: TPoint;
  gDoorMap: array of array of DWORD;
  gLiftMap: array of array of DWORD;

implementation

uses g_main, e_log, SysUtils, g_items, g_gfx, g_console, dglOpenGL,
     g_weapons, g_game, inter, g_sound, e_sound, CONFIG, g_options, MAPREADER,
     g_triggers, g_player, MAPDEF, Math, g_monsters;

type
  TLevelTexture = record
   TextureName: string;
   Width,
   Height:      Word;
   case Anim: Boolean of
    False: (TextureID:   DWORD;);
    True:  (FramesID:    DWORD;
            FramesCount: Byte;
            Speed:       Byte);
  end;

const
  FLAGRECT: TRectWH = (X:15; Y:12; Width:33; Height:52);
  SKY_STRETCH: Single = 1.5;
  TEXTURE_NAME_WATER = '_water_0';
  TEXTURE_NAME_ACID1 = '_water_1';
  TEXTURE_NAME_ACID2 = '_water_2';
  TEXTURE_SPECIAL_WATER = DWORD(-1);
  TEXTURE_SPECIAL_ACID1 = DWORD(-2);
  TEXTURE_SPECIAL_ACID2 = DWORD(-3);
  TEXTURE_SPECIAL_NONE = DWORD(-4);

var
  Textures:      array of TLevelTexture;
  RenderWater:   TRPanelArray;
  RenderAcid1:   TRPanelArray;
  RenderAcid2:   TRPanelArray;
  RenderSteps:   TRPanelArray;
  RespawnPoints: array of TRespawnPoint;
  FlagPoints:    array [FLAG_RED..FLAG_BLUE] of PFlagPoint;
  //DOMFlagPoints: array of TFlagPoint;
  BackID:  DWORD = DWORD(-1);

procedure CreateDoorMap();
var
  PanelArray: array of record
                        Rect: TRectWH;
                        Active: Boolean;
                        RenderPanelID: DWORD;
                       end;
  a, b, c, m, i, len: Integer;
  ok: Boolean;
begin
 if gWalls = nil then Exit;

 i := 0;
 len := 128;
 SetLength(PanelArray, len);

 for a := 0 to High(gRenderWalls) do
  if gWalls[gRenderWalls[a].WallID].Door then
  begin
   PanelArray[i].Rect := gWalls[gRenderWalls[a].WallID].Rect;
   PanelArray[i].Active := True;
   PanelArray[i].RenderPanelID := a;
   i := i+1;
   if i = len then
   begin
    len := len+128;
    SetLength(PanelArray, len);
   end;
  end;

 if i = 0 then Exit;

 g_Game_SetLoadingText('Door Map', i-1);
 for a := 0 to i-1 do
  if PanelArray[a].Active then
  begin
   PanelArray[a].Active := False;
   SetLength(gDoorMap, Length(gDoorMap)+1);
   m := High(gDoorMap);
   SetLength(gDoorMap[m], 1);
   gDoorMap[m, 0] := PanelArray[a].RenderPanelID;
   ok := True;
   while ok do
   begin
    ok := False;
    for b := 0 to i-1 do
    if PanelArray[b].Active then
     for c := 0 to High(gDoorMap[m]) do
      if {((gRenderWalls[PanelArray[b].RenderPanelID].TextureID = gRenderWalls[gDoorMap[m, c]].TextureID) or
          gRenderWalls[PanelArray[b].RenderPanelID].Hide or gRenderWalls[gDoorMap[m, c]].Hide) and}
         g_CollideWater(PanelArray[b].Rect.X, PanelArray[b].Rect.Y,
                        PanelArray[b].Rect.Width, PanelArray[b].Rect.Height,
                        gRenderWalls[gDoorMap[m, c]].X,
                        gRenderWalls[gDoorMap[m, c]].Y,
                        gRenderWalls[gDoorMap[m, c]].Width,
                        gRenderWalls[gDoorMap[m, c]].Height) then
      begin
       PanelArray[b].Active := False;
       SetLength(gDoorMap[m], Length(gDoorMap[m])+1);
       gDoorMap[m, High(gDoorMap[m])] := PanelArray[b].RenderPanelID;
       ok := True;
       Break;
      end;
   end;

   g_Game_StepLoading();
  end;

 PanelArray := nil;
end;

procedure CreateLiftMap();
var
  PanelArray: array of record
                        Rect: TRectWH;
                        Active: Boolean;
                       end;
  a, b, c, len, i, j: Integer;
  ok: Boolean;
begin
 if gLifts = nil then Exit;

 len := Length(gLifts);
 SetLength(PanelArray, len);

 for a := 0 to len-1 do
 begin
  PanelArray[a].Rect := gLifts[a].Rect;
  PanelArray[a].Active := True;
 end;

 SetLength(gLiftMap, len);
 i := 0;

 g_Game_SetLoadingText('Lift Map', len-1);
 for a := 0 to len-1 do
  if PanelArray[a].Active then
  begin
   PanelArray[a].Active := False;
   SetLength(gLiftMap[i], 32);
   j := 0;
   gLiftMap[i, j] := a;

   ok := True;
   while ok do
   begin
    ok := False;
    for b := 0 to len-1 do
    if PanelArray[b].Active then
     for c := 0 to j do
      if g_CollideWater(PanelArray[b].Rect.X, PanelArray[b].Rect.Y,
                        PanelArray[b].Rect.Width, PanelArray[b].Rect.Height,
                        PanelArray[gLiftMap[i, c]].Rect.X,
                        PanelArray[gLiftMap[i, c]].Rect.Y,
                        PanelArray[gLiftMap[i, c]].Rect.Width,
                        PanelArray[gLiftMap[i, c]].Rect.Height) then
      begin
       PanelArray[b].Active := False;

       j := j+1;
       if j > High(gLiftMap[i]) then
        SetLength(gLiftMap[i], Length(gLiftMap[i])+32);

       gLiftMap[i, j] := b;
       ok := True;

       Break;
      end;
   end;

   SetLength(gLiftMap[i], j+1);
   i := i+1;

   g_Game_StepLoading;
  end;

 SetLength(gLiftMap, i);

 PanelArray := nil;
end;

function IsSpecialTexture(Texture: string): Boolean;
begin
 Result := (Texture = TEXTURE_NAME_WATER) or
           (Texture = TEXTURE_NAME_ACID1) or
           (Texture = TEXTURE_NAME_ACID2);
end;

procedure CreateRenderPanel(var RenderPanel: TRenderPanel; PanelRec: TPanelRec_1;
                            SecondTextureNum: DWORD);
begin
 with RenderPanel do
 begin
  X := PanelRec.X;
  Y := PanelRec.Y;
  Width := PanelRec.Width;
  Height := PanelRec.Height;
  Hide := ByteBool(PanelRec.Flags and PANEL_FLAG_HIDE);
  Enabled := not (PanelRec.PanelType = PANEL_OPENDOOR);

  Animation := nil;
  Animation2 := nil;

  if Hide then
  begin
   TextureID := 0;
   TextureID2 := 0;
   Exit;
  end;

  if WordBool(PanelRec.PanelType and (PANEL_WATER or PANEL_ACID1 or PANEL_ACID2)) and
     not ByteBool(PanelRec.Flags and PANEL_FLAG_WATERTEXTURES) then
  begin
   case PanelRec.PanelType of
    PANEL_WATER: TextureID := TEXTURE_SPECIAL_WATER;
    PANEL_ACID1: TextureID := TEXTURE_SPECIAL_ACID1;
    PANEL_ACID2: TextureID := TEXTURE_SPECIAL_ACID2;
   end;

   TextureID2 := TextureID;
   Exit;
  end;

  Anim := Textures[PanelRec.TextureNum].Anim;

  if not Anim then
  begin
   TextureID := Textures[PanelRec.TextureNum].TextureID;

   if SecondTextureNum <> DWORD(-1) then TextureID2 := Textures[SecondTextureNum].TextureID
    else TextureID2 := TEXTURE_SPECIAL_NONE;//TextureID;

   if not IsSpecialTexture(Textures[PanelRec.TextureNum].TextureName) then
   begin
    TextureHeight := Textures[PanelRec.TextureNum].Height;
    TextureWidth := Textures[PanelRec.TextureNum].Width;
    Alpha := PanelRec.Alpha;
    Blending := ByteBool(PanelRec.Flags and PANEL_FLAG_BLENDING);
   end;
  end
   else
  begin
   TextureID := 0;
   TextureID2 := 0;

   Animation := TAnimation.Create(Textures[PanelRec.TextureNum].FramesID,
                                  True, Textures[PanelRec.TextureNum].Speed);
   Animation.Blending := ByteBool(PanelRec.Flags and PANEL_FLAG_BLENDING);
   Animation.Alpha := PanelRec.Alpha;
   TextureHeight := Textures[PanelRec.TextureNum].Height;
   TextureWidth := Textures[PanelRec.TextureNum].Width;

   if SecondTextureNum <> DWORD(-1) then
   begin
    Animation2 := TAnimation.Create(Textures[SecondTextureNum].FramesID, True,
                                    Textures[SecondTextureNum].Speed);
    Animation2.Blending := ByteBool(PanelRec.Flags and PANEL_FLAG_BLENDING);
    Animation2.Alpha := PanelRec.Alpha;
   end else Animation2 := nil;
  end;
 end;
end;

procedure CreatePanelRect(var Rect: TRectWH; PanelRec: TPanelRec_1);
begin
 with Rect do
 begin
  X := PanelRec.X;
  Y := PanelRec.Y;
  Width := PanelRec.Width;
  Height := PanelRec.Height;
 end;
end;

function CreatePanel(PanelRec: TPanelRec_1; SecondTextureNum: DWORD): Integer;
begin
 Result := -1;

 case PanelRec.PanelType of
  PANEL_WALL:
  begin
   SetLength(gWalls, Length(gWalls)+1);
   CreatePanelRect(gWalls[High(gWalls)].Rect, PanelRec);

   SetLength(gRenderWalls, Length(gRenderWalls)+1);
   CreateRenderPanel(gRenderWalls[High(gRenderWalls)], PanelRec, SecondTextureNum);
   with gWalls[High(gWalls)] do
   begin
    Door := False;
    Enabled := True;
   end;
   gRenderWalls[High(gRenderWalls)].WallID := High(gWalls);

   Result := High(gRenderWalls);
  end;

  PANEL_BACK:
  begin
   SetLength(gRenderBackgrounds, Length(gRenderBackgrounds)+1);
   CreateRenderPanel(gRenderBackgrounds[High(gRenderBackgrounds)], PanelRec, SecondTextureNum);
   with gRenderBackgrounds[High(gRenderBackgrounds)] do
   begin
    Enabled := True;
    WallID := DWORD(-1);
   end;

   Result := High(gRenderBackgrounds);
  end;

  PANEL_FORE:
  begin
   SetLength(gRenderForegrounds, Length(gRenderForegrounds)+1);
   CreateRenderPanel(gRenderForegrounds[High(gRenderForegrounds)], PanelRec, SecondTextureNum);
   with gRenderForegrounds[High(gRenderForegrounds)] do
   begin
    Enabled := True;
    WallID := DWORD(-1);
   end;

   Result := High(gRenderForegrounds);
  end;

  PANEL_OPENDOOR, PANEL_CLOSEDOOR:
  begin
   SetLength(gWalls, Length(gWalls)+1);
   CreatePanelRect(gWalls[High(gWalls)].Rect, PanelRec);

   SetLength(gRenderWalls, Length(gRenderWalls)+1);
   CreateRenderPanel(gRenderWalls[High(gRenderWalls)], PanelRec, DWORD(-1));
   with gWalls[High(gWalls)] do
   begin
    Door := True;
    Enabled := PanelRec.PanelType = PANEL_CLOSEDOOR;
   end;
   gRenderWalls[High(gRenderWalls)].WallID := High(gWalls);

   Result := High(gRenderWalls);
  end;

  PANEL_WATER:
  begin
   SetLength(gWater, Length(gWater)+1);
   CreatePanelRect(gWater[High(gWater)], PanelRec);

   SetLength(RenderWater, Length(RenderWater)+1);
   CreateRenderPanel(RenderWater[High(RenderWater)], PanelRec, DWORD(-1));
   RenderWater[High(RenderWater)].WallID := High(gWater);

   Result := High(RenderWater);
  end;

  PANEL_ACID1:
  begin
   SetLength(gAcid1, Length(gAcid1)+1);
   CreatePanelRect(gAcid1[High(gAcid1)], PanelRec);

   SetLength(RenderAcid1, Length(RenderAcid1)+1);
   CreateRenderPanel(RenderAcid1[High(RenderAcid1)], PanelRec, DWORD(-1));
   RenderAcid1[High(RenderAcid1)].WallID := High(gAcid1);

   Result := High(RenderAcid1);
  end;

  PANEL_ACID2:
  begin
   SetLength(gAcid2, Length(gAcid2)+1);
   CreatePanelRect(gAcid2[High(gAcid2)], PanelRec);

   SetLength(RenderAcid2, Length(RenderAcid2)+1);
   CreateRenderPanel(RenderAcid2[High(RenderAcid2)], PanelRec, DWORD(-1));
   RenderAcid2[High(RenderAcid2)].WallID := High(gAcid2);

   Result := High(RenderAcid2);
  end;

  PANEL_STEP:
  begin
   SetLength(gSteps, Length(gSteps)+1);
   CreatePanelRect(gSteps[High(gSteps)], PanelRec);

   SetLength(RenderSteps, Length(RenderSteps)+1);
   CreateRenderPanel(RenderSteps[High(RenderSteps)], PanelRec, DWORD(-1));
   with RenderSteps[High(RenderSteps)] do
   begin
    Enabled := True;
    WallID := DWORD(-1);
   end;
   Result := High(gSteps);
  end;

  PANEL_LIFTUP, PANEL_LIFTDOWN:
  begin
   SetLength(gLifts, Length(gLifts)+1);
   CreatePanelRect(gLifts[High(gLifts)].Rect, PanelRec);

   if PanelRec.PanelType = PANEL_LIFTUP then gLifts[High(gLifts)].LiftType := 0
    else gLifts[High(gLifts)].LiftType := 1;
   Result := High(gLifts);
  end;

  PANEL_BLOCKMON:
  begin
   SetLength(gBlockMon, Length(gBlockMon)+1);
   CreatePanelRect(gBlockMon[High(gBlockMon)], PanelRec);
   Result := High(gBlockMon);
  end;
 end;
end;

function CreateTexture(Rec: TTextureRec_1; Map: string): Boolean;
var
  WAD: TWADEditor_1;
  TextureData: Pointer;
  WADName:     string;
  SectionName: string;
  TextureName: string;
  a, ResLength: Integer;
begin
 Result := False;

 if Textures <> nil then
  for a := 0 to High(Textures) do
   if Textures[a].TextureName = Rec.Resource then
   begin
    SetLength(Textures, Length(Textures)+1);
    Textures[High(Textures)] := Textures[a];

    Result := True;
    Exit;
   end;

 if (Rec.Resource = TEXTURE_NAME_WATER) or
    (Rec.Resource = TEXTURE_NAME_ACID1) or
    (Rec.Resource = TEXTURE_NAME_ACID2) then
 begin
  SetLength(Textures, Length(Textures)+1);

  with Textures[High(Textures)] do
  begin
   TextureName := Rec.Resource;
   
   if TextureName = TEXTURE_NAME_WATER then TextureID := TEXTURE_SPECIAL_WATER
   else if TextureName = TEXTURE_NAME_ACID1 then TextureID := TEXTURE_SPECIAL_ACID1
   else if TextureName = TEXTURE_NAME_ACID2 then TextureID := TEXTURE_SPECIAL_ACID2;

   Anim := False;
  end;

  Result := True;
  Exit;
 end;

 g_ProcessResourceStr(Rec.Resource, WADName, SectionName, TextureName);

 WAD := TWADEditor_1.Create;

 if WADName <> '' then WADName := GameDir+'\wads\'+WADName else WADName := Map;

 WAD.ReadFile(WADName);

 if WAD.GetResource(SectionName, TextureName, TextureData, ResLength) then
 begin
  SetLength(Textures, Length(Textures)+1);
  if not e_CreateTextureMem(TextureData, Textures[High(Textures)].TextureID) then Exit;
  e_GetTextureSize(Textures[High(Textures)].TextureID,
                   @Textures[High(Textures)].Width,
                   @Textures[High(Textures)].Height);
  FreeMem(TextureData);
  Textures[High(Textures)].TextureName := Rec.Resource;
  Textures[High(Textures)].Anim := False;

  Result := True;
 end
  else
 begin
  e_WriteLog(Format('Error loading texture %s', [Rec.Resource]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
 end;
 WAD.Destroy;
end;

function CreateAnimTexture(Rec: TTextureRec_1; Map: string): Boolean;
var
  WAD: TWADEditor_1;
  TextureWAD: Pointer;
  TextData: Pointer;
  TextureData: Pointer;
  cfg: TConfig;
  WADName: string;
  SectionName: string;
  TextureName: string;
  ResLength: Integer;
  TextureResource: string;
  _width, _height,
  _framecount, _speed: Integer;
  _backanimation: Boolean;
begin
 Result := False;

 g_ProcessResourceStr(Rec.Resource, WADName, SectionName, TextureName);

 WAD := TWADEditor_1.Create;

 if WADName <> '' then WADName := GameDir+'\wads\'+WADName else WADName := Map;

 WAD.ReadFile(WADName);
 if not WAD.GetResource(SectionName, TextureName, TextureWAD, ResLength) then
 begin
  e_WriteLog(Format('Error loading animation texture %s', [Rec.Resource]), MSG_WARNING);
  e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
  WAD.Destroy;
  Exit;
 end;

 WAD.FreeWAD;
 if not WAD.ReadMemory(TextureWAD, ResLength) then
 begin
  FreeMem(TextureWAD);
  WAD.Destroy;
  Exit;
 end;

 if not WAD.GetResource('TEXT', 'ANIM', TextData, ResLength) then
 begin
  FreeMem(TextureWAD);
  WAD.Destroy;
  Exit;
 end;

 cfg := TConfig.CreateMem(TextData, ResLength);

 TextureResource := cfg.ReadStr('', 'resource', '');

 if TextureResource = '' then
 begin
  FreeMem(TextureWAD);
  FreeMem(TextData);
  WAD.Destroy;
  cfg.Destroy;
 end;

 _width := cfg.ReadInt('', 'framewidth', 0);
 _height := cfg.ReadInt('', 'frameheight', 0);
 _framecount := cfg.ReadInt('', 'framecount', 0);
 _speed := cfg.ReadInt('', 'waitcount', 0);
 _backanimation := cfg.ReadBool('', 'backanimation', False);

 cfg.Destroy;

 if not WAD.GetResource('TEXTURES', TextureResource, TextureData, ResLength) then
 begin
  FreeMem(TextureWAD);
  FreeMem(TextData);
  WAD.Destroy;
  Exit;
 end;

 WAD.Destroy;

 SetLength(Textures, Length(Textures)+1);
 with Textures[High(Textures)] do
 begin
  if g_Frames_CreateMemory(@FramesID, '', TextureData, _width, _height, _framecount,
                           _backanimation) then
  begin
   TextureName := Rec.Resource;
   Width := _width;
   Height := _height;
   Anim := True;
   Speed := _speed;
   TextureName := Rec.Resource;

   Result := True;
  end else e_WriteLog(Format('Error loading animation texture %s', [Rec.Resource]), MSG_WARNING);
 end;

 FreeMem(TextureWAD);
 FreeMem(TextData);
end;

procedure CreateItem(Item: TItemRec_1);
begin
 if (gGameSettings.GameType <> GT_CUSTOM) and
    ByteBool(Item.Options and ITEM_OPTION_ONLYDM) then Exit;
    
 g_Items_Create(Item.X, Item.Y, Item.ItemType, ByteBool(Item.Options and ITEM_OPTION_FALL),
                gGameSettings.GameType = GT_CUSTOM)
end;

procedure CreateArea(Area: TAreaRec_1);
var
  a: Integer;
  id: DWORD;
begin
 case Area.AreaType of
  AREA_DMPOINT, AREA_PLAYERPOINT1, AREA_PLAYERPOINT2,
  AREA_REDTEAMPOINT, AREA_BLUETEAMPOINT:
  begin
   SetLength(RespawnPoints, Length(RespawnPoints)+1);
   with RespawnPoints[High(RespawnPoints)] do
   begin
    X := Area.X;
    Y := Area.Y;
    Direction := TDirection(Area.Direction);

    case Area.AreaType of
     AREA_DMPOINT: PointType := RESPAWNPOINT_DM;
     AREA_PLAYERPOINT1: PointType := RESPAWNPOINT_PLAYER1;
     AREA_PLAYERPOINT2: PointType := RESPAWNPOINT_PLAYER2;
     AREA_REDTEAMPOINT: PointType := RESPAWNPOINT_RED;
     AREA_BLUETEAMPOINT: PointType := RESPAWNPOINT_BLUE;
    end;
   end;
  end;

  AREA_REDFLAG, AREA_BLUEFLAG:
  begin
   if Area.AreaType = AREA_REDFLAG then a := FLAG_RED else a := FLAG_BLUE;

   if FlagPoints[a] <> nil then Exit;

   New(FlagPoints[a]);

   with FlagPoints[a]^ do
   begin
    X := Area.X-FLAGRECT.X;
    Y := Area.Y-FLAGRECT.Y;
    Direction := TDirection(Area.Direction);
   end;

   with gFlags[a] do
   begin
    case a of
     FLAG_RED: g_Frames_Get(id, 'FRAMES_FLAG_RED');
     FLAG_BLUE: g_Frames_Get(id, 'FRAMES_FLAG_BLUE');
    end;

    Animation := TAnimation.Create(id, True, 8);
    Obj.Rect := FLAGRECT;

    g_Map_ResetFlag(a);
   end;
  end;

  AREA_DOMFLAG:
  begin
   {SetLength(DOMFlagPoints, Length(DOMFlagPoints)+1);
   with DOMFlagPoints[High(DOMFlagPoints)] do
   begin
    X := Area.X;
    Y := Area.Y;
    Direction := TDirection(Area.Direction);
   end;

   g_Map_CreateFlag(DOMFlagPoints[High(DOMFlagPoints)], FLAG_DOM, FLAG_STATE_NORMAL);}
  end;
 end;
end;

procedure CreateTrigger(Trigger: TTriggerRec_2; fTexturePanelType: Word);
var
  _trigger: TTrigger;
begin
 with _trigger do
 begin
  X := Trigger.X;
  Y := Trigger.Y;
  Width := Trigger.Width;
  Height := Trigger.Height;
  Enabled := ByteBool(Trigger.Enabled);
  TexturePanel := Trigger.TexturePanel;
  TexturePanelType := fTexturePanelType;
  TriggerType := Trigger.TriggerType;
  ActivateType := Trigger.ActivateType;
  Keys := Trigger.Keys;
  Data.Default := Trigger.DATA;
 end;

 g_Triggers_Create(_trigger);
end;

procedure CreateMonster(monster: TMonsterRec_1);
var
  a, i: Integer;
begin
 if (gGameSettings.GameType = GT_SINGLE) or
    LongBool(gGameSettings.Options and GAME_OPTION_MONSTERDM) then
 begin
  i := g_Monsters_Create(monster.MonsterType, monster.X, monster.Y,
                         TDirection(monster.Direction));

  if gTriggers <> nil then
   for a := 0 to High(gTriggers) do
    if gTriggers[a].TriggerType in [TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF] then
     if gTriggers[a].Data.MonsterID-1 = i then gMonsters[i].AddTrigger(a);
 end;
end;

procedure g_Map_ReAdd_DieTriggers;
var
  i, a: Integer;
  
begin
  for i := 0 to High(gMonsters) do
    if gMonsters[i] <> nil then
      begin
        gMonsters[i].ClearTriggers;
        for a := 0 to High(gTriggers) do
          if gTriggers[a].TriggerType in [TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF] then
            if (gTriggers[a].Data.MonsterID-1) = i then
              gMonsters[i].AddTrigger(a);
      end;
end;

function g_Map_Load(Res: string): Boolean;
const
  DefaultMusRes = 'Standart.wad:STDMUS\MUS1';
  DefaultSkyRes = 'Standart.wad:STDSKY\SKY0';

var
  WAD: TWADEditor_1;
  MapReader: TMapReader_2;
  Header: TMapHeaderRec_1;
  _textures: TTexturesRec1Array;
  panels: TPanelsRec1Array;
  items: TItemsRec1Array;
  monsters: TMonsterRec1Array;
  areas: TAreasRec1Array;
  triggers: TTriggersRec2Array;
  a, b, c: Integer;
  d: Single;
  PanelID, SecondTextureID: DWORD;
  texture: TTextureRec_1;
  TriggersTable: array of record
                           TexturePanel: Integer;
                           LiftPanel: Integer;
                           DoorPanel: Integer;
                          end;
  FileName, SectionName, ResName,
  FileName2, s: string;
  Data: Pointer;
  Len: Integer;
  ok: Boolean;
begin
 Result := False;
 gMapInfo.Map := Res;

 g_Game_SetLoadingText('WAD File', 0);

 g_ProcessResourceStr(Res, FileName, SectionName, ResName);

 WAD := TWADEditor_1.Create;
 if not WAD.ReadFile(FileName) then
 begin
  g_Console_Add('! ошибка чтения WAD карты');
  WAD.Destroy;
  Exit;
 end;

 if not WAD.GetResource('', ResName, Data, Len) then
 begin
  g_Console_Add('! ошибка чтения карты из WAD');
  WAD.Destroy;
  Exit;
 end;

 WAD.Destroy;

 g_Game_SetLoadingText('MAP Resource', 0);
 MapReader := TMapReader_2.Create;
 MapReader.LoadMap(Data);

 g_Game_SetLoadingText('Textures', 0);
 _textures := MapReader.GetTextures();
 if _textures <> nil then
 begin
  g_Game_SetLoadingText('Textures', High(_textures));
  for a := 0 to High(_textures) do
  begin
   if ByteBool(_textures[a].Anim) then
    if not CreateAnimTexture(_textures[a], FileName) then
    begin
     SetLength(s, 64);
     CopyMemory(@s[1], @_textures[a].Resource[0], 64);
     g_Console_Add(Format('! ошибка создания анимированной текстуры %s', [s]));
     MapReader.Destroy;
     Exit;
    end;

   if not ByteBool(_textures[a].Anim) then
    if not CreateTexture(_textures[a], FileName) then
    begin
     SetLength(s, 64);
     CopyMemory(@s[1], @_textures[a].Resource[0], 64);
     g_Console_Add(Format('! ошибка создания текстуры %s', [s]));
     MapReader.Destroy;
     Exit;
    end;
    
   g_Game_StepLoading();
  end;
 end;

 g_Game_SetLoadingText('Triggers', 0);
 triggers := MapReader.GetTriggers;
 
 g_Game_SetLoadingText('Panels', 0);
 panels := MapReader.GetPanels;

 if triggers <> nil then
 begin
  SetLength(TriggersTable, Length(triggers));

  g_Game_SetLoadingText('TriggersTable', High(TriggersTable));
  for a := 0 to High(TriggersTable) do
  begin
   TriggersTable[a].TexturePanel := triggers[a].TexturePanel;

   if triggers[a].TriggerType in [TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT] then
    TriggersTable[a].LiftPanel := TTriggerData(triggers[a].DATA).PanelID
     else TriggersTable[a].LiftPanel := -1;

   if triggers[a].TriggerType in [TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR,
                                  TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP,
                                  TRIGGER_PRESS] then
    TriggersTable[a].DoorPanel := TTriggerData(triggers[a].DATA).PanelID
     else TriggersTable[a].DoorPanel := -1;

   g_Game_StepLoading();
  end;
 end;

 if panels <> nil then
 begin
  g_Game_SetLoadingText('Triggers for Panels', High(panels));
  for a := 0 to High(panels) do
  begin
   SecondTextureID := DWORD(-1);

   // Смотрим, ссылаются ли на эту панель триггеры. Если да - то надо создать еще одну текстуру.
   if (triggers <> nil) and (_textures <> nil) then
    for b := 0 to High(triggers) do
     if triggers[b].TexturePanel = a then
     begin
      texture := _textures[panels[a].TextureNum];

      for c := 0 to High(texture.Resource)-1 do
       if texture.Resource[c+1] = #0 then
       begin
        if texture.Resource[c] = '0' then texture.Resource[c] := '1'
         else texture.Resource[c] := '0';
        Break;
       end;

      if ByteBool(texture.Anim) then ok := CreateAnimTexture(texture, FileName)
       else ok := CreateTexture(texture, FileName);

      if ok then
      begin
       for c := 0 to High(Textures) do
        if Textures[c].TextureName = texture.Resource then
        begin
         SecondTextureID := c;
         Break;
        end;
      end
       else
      begin
       //SetLength(s, 64);
       //CopyMemory(@s[1], @texture.Resource[0], 64);
       //g_Console_Add(Format('! ошибка создания текстуры кнопки %s', [s]));
       //triggers[b].TexturePanel := -1;
      end;

      Break;
     end;

   // Создаем панель и запоминаем ее номер
   PanelID := CreatePanel(panels[a], SecondTextureID);

   if triggers <> nil then
   for b := 0 to High(triggers) do
    if (triggers[b].TriggerType in [TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR,
                                    TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP,
                                    TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT]) and
       (TTriggerData(triggers[b].DATA).PanelID = a) then
     TTriggerData(triggers[b].DATA).PanelID := PanelID;

   //Если на панель ссылаются триггеры - надо поставить у них TexturePanel
   for b := 0 to High(triggers) do
    if triggers[b].TexturePanel = a then
     triggers[b].TexturePanel := PanelID;

   g_Game_StepLoading();
  end;
 end;

 if (triggers <> nil) and not gLoadGameMode then
 begin
  g_Game_SetLoadingText('Create Triggers', 0);
  for a := 0 to High(triggers) do
  begin
   if triggers[a].TexturePanel <> -1 then b := panels[TriggersTable[a].TexturePanel].PanelType
    else b := 0;

   CreateTrigger(triggers[a], b);
  end;
 end;

 g_Game_SetLoadingText('Items', 0);
 items := MapReader.GetItems();
 
 if (items <> nil) and not gLoadGameMode then
 begin
  g_Game_SetLoadingText('Create Items', 0);
  for a := 0 to High(items) do
   CreateItem(Items[a]);
 end;

 g_Game_SetLoadingText('Areas', 0);
 areas := MapReader.GetAreas();
 
 if areas <> nil then
 begin
  g_Game_SetLoadingText('Create Areas', 0);
  for a := 0 to High(areas) do
   CreateArea(areas[a]);
 end;

 g_Game_SetLoadingText('Monsters', 0);
 monsters := MapReader.GetMonsters();
 if (monsters <> nil) and not gLoadGameMode then
 begin
  g_Game_SetLoadingText('Create Monsters', 0);
  for a := 0 to High(monsters) do
   CreateMonster(monsters[a]);
 end;

 g_Game_SetLoadingText('Map Header', 0);
 Header := MapReader.GetMapHeader();

 MapReader.Destroy;

 with gMapInfo do
 begin
  Name := Header.MapName;
  Description := Header.MapDescription;
  Author := Header.MapAuthor;
  MusicName := Header.MusicName;
  SkyName := Header.SkyName;
  Height := Header.Height;
  Width := Header.Width;
 end;

 if gMapInfo.SkyName <> '' then
 begin
  g_Game_SetLoadingText('Sky', 0);
  g_ProcessResourceStr(gMapInfo.SkyName, FileName, SectionName, ResName);
  
  if FileName <> '' then FileName := GameDir+'\wads\'+FileName
   else
  begin
   g_ProcessResourceStr(Res, @FileName2, nil, nil);
   FileName := FileName2;
  end;

  s := FileName+':'+SectionName+'\'+ResName;
  if g_Texture_CreateWAD(BackID, s) then
  begin
   d := SKY_STRETCH;

   if (gScreenWidth*d > gMapInfo.Width) or (gScreenHeight*d > gMapInfo.Height) then d := 1;

   gBackSize.X := Round(gScreenWidth*d);
   gBackSize.Y := Round(gScreenHeight*d);
  end else g_Console_Add(Format('! ошибка загрузки неба %s', [s]));
 end;

 if gMapInfo.MusicName <> '' then
 begin
  g_Game_SetLoadingText('Music', 0);
  g_ProcessResourceStr(gMapInfo.MusicName, FileName, SectionName, ResName);
  
  if FileName <> '' then FileName := GameDir+'\wads\'+FileName
   else
  begin
   g_ProcessResourceStr(Res, @FileName2, nil, nil);
   FileName := FileName2;
  end;

  s := FileName+':'+SectionName+'\'+ResName;
  if g_Music_CreateWADEx(gMapInfo.MusicName, s) then g_Game_PlayMusic(gMapInfo.MusicName)
   else g_Console_Add(Format('! ошибка загрузки музыки %s', [s]));
 end;

 CreateDoorMap();
 CreateLiftMap();                                    

 g_Items_Init();
 g_GFX_Init();
 g_Weapon_Init();
 g_Monsters_Init();

 _textures := nil;
 panels := nil;
 items := nil;
 areas := nil;
 triggers := nil;
 TriggersTable := nil;

 Result := True;
end;

function g_Map_GetMapInfo(Res: string): TMapInfo;
var
  WAD: TWADEditor_1;
  MapReader: TMapReader_2;
  Header: TMapHeaderRec_1;
  FileName, SectionName, ResName: string;
  Data: Pointer;
  Len: Integer;
begin
 g_ProcessResourceStr(Res, FileName, SectionName, ResName);

 WAD := TWADEditor_1.Create;
 if not WAD.ReadFile(FileName) then
 begin
  WAD.Destroy;
  Exit;
 end;

 if not WAD.GetResource('', ResName, Data, Len) then
 begin
  WAD.Destroy;
  Exit;
 end;

 WAD.Destroy;

 MapReader := TMapReader_2.Create;

 MapReader.LoadMap(Data);
 Header := MapReader.GetMapHeader;

 MapReader.Destroy;

 Result.Map := Res;
 Result.Name := Header.MapName;
 Result.Author := Header.MapAuthor;
 Result.Description := Header.MapDescription;
 Result.Height := Header.Height;
 Result.Width := Header.Width;
end;

function g_Map_GetMapsList(WADName: string): SArray;
var
  WAD: TWADEditor_1;
  a: Integer;
  ResList: SArray;
  Data: Pointer;
  Len: Integer;
  Sign: array[0..2] of Char;
begin
 Result := nil;

 WAD := TWADEditor_1.Create;
 if not WAD.ReadFile(WADName) then
 begin
  WAD.Destroy;
  Exit;
 end;

 ResList := WAD.GetResourcesList('');

 if ResList <> nil then
  for a := 0 to High(ResList) do
  begin
   if not WAD.GetResource('', ResList[a], Data, Len) then Continue;
   CopyMemory(@Sign[0], Data, 3);
   FreeMem(Data);
   
   if Sign = MAP_SIGNATURE then
   begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := ResList[a];
   end;
   
   Sign := '';
  end;

 WAD.Destroy;
end;

function g_Map_Exist(Res: string): Boolean;
var
  WAD: TWADEditor_1;
  FileName, SectionName, ResName: string;
  ResList: SArray;
  a: Integer;
begin
 Result := False;

 g_ProcessResourceStr(Res, FileName, SectionName, ResName);

 if Pos('.wad', LowerCase(FileName)) = 0 then FileName := FileName+'.wad';

 WAD := TWADEditor_1.Create;
 if not WAD.ReadFile(FileName) then
 begin
  WAD.Destroy;
  Exit;
 end;

 ResList := WAD.GetResourcesList('');
 WAD.Destroy;
 
 if ResList <> nil then
  for a := 0 to High(ResList) do if ResList[a] = ResName then
  begin
   Result := True;
   Exit;
  end;
end;

procedure g_Map_Free();
var
  a: Integer;
begin
 g_GFX_Free();
 g_Weapon_Free();
 g_Items_Free();
 g_Triggers_Free();
 g_Monsters_Free();

 RespawnPoints := nil;
 if FlagPoints[FLAG_RED] <> nil then
 begin
  Dispose(FlagPoints[FLAG_RED]);
  FlagPoints[FLAG_RED] := nil;
 end;
 if FlagPoints[FLAG_BLUE] <> nil then
 begin
  Dispose(FlagPoints[FLAG_BLUE]);
  FlagPoints[FLAG_BLUE] := nil;
 end;
 //DOMFlagPoints := nil;

 gWalls := nil;
 gWater := nil;
 gAcid1 := nil;
 gAcid2 := nil;
 gSteps := nil;
 gLifts := nil;
 gBlockMon := nil;
 //gDOMFlags := nil;

 if Textures <> nil then
 begin
  for a := 0 to High(Textures) do
   if not IsSpecialTexture(Textures[a].TextureName) then
    if Textures[a].Anim then g_Frames_DeleteByID(Textures[a].FramesID)
     else e_DeleteTexture(Textures[a].TextureID);

  Textures := nil;
 end;

 if gRenderWalls <> nil then
 begin
  for a := 0 to High(gRenderWalls) do
   if gRenderWalls[a].Anim and (gRenderWalls[a].Animation <> nil) then
    gRenderWalls[a].Animation.Destroy;
  gRenderWalls := nil;
 end;

 if gRenderBackgrounds <> nil then
 begin
  for a := 0 to High(gRenderBackgrounds) do
   if gRenderBackgrounds[a].Anim and (gRenderBackgrounds[a].Animation <> nil) then
    gRenderBackgrounds[a].Animation.Destroy;

  gRenderBackgrounds := nil;
 end;

 if gRenderForegrounds <> nil then
 begin
  for a := 0 to High(gRenderForegrounds) do
   if gRenderForegrounds[a].Anim and (gRenderForegrounds[a].Animation <> nil) then
    gRenderForegrounds[a].Animation.Destroy;
  gRenderForegrounds := nil;
 end;

 if RenderSteps <> nil then
 begin
  for a := 0 to High(RenderSteps) do
   if RenderSteps[a].Anim and (RenderSteps[a].Animation <> nil) then
    RenderSteps[a].Animation.Destroy;
  RenderSteps := nil;
 end;

 if RenderWater <> nil then
 begin
  for a := 0 to High(RenderWater) do
   if RenderWater[a].Anim and (RenderWater[a].Animation <> nil) then
    RenderWater[a].Animation.Destroy;
  RenderWater := nil;
 end;

 if RenderAcid1 <> nil then
 begin
  for a := 0 to High(RenderAcid1) do
   if RenderAcid1[a].Anim and (RenderAcid1[a].Animation <> nil) then
    RenderAcid1[a].Animation.Destroy;
  RenderAcid1 := nil;
 end;

 if RenderAcid2 <> nil then
 begin
  for a := 0 to High(RenderAcid2) do
   if RenderAcid2[a].Anim and (RenderAcid2[a].Animation <> nil) then
    RenderAcid2[a].Animation.Destroy;
  RenderAcid2 := nil;
 end;


 if BackID <> DWORD(-1) then
 begin
  gBackSize.X := 0;
  gBackSize.Y := 0;
  e_DeleteTexture(BackID);
  BackID := DWORD(-1); 
 end;

 g_Game_StopMusic();
 g_Music_Delete(gMapInfo.MusicName);

 gMapInfo.Name := '';
 gMapInfo.Description := '';
 gMapInfo.MusicName := '';
 gMapInfo.Height := 0;
 gMapInfo.Width := 0;

 gDoorMap := nil;
 gLiftMap := nil;
end;

procedure g_Map_Update();
var
  a, d: Integer;
  m: Word;
  s: string;
begin
 if gRenderWalls <> nil then
  for a := 0 to High(gRenderWalls) do
   with gRenderWalls[a] do
    if (not Hide) and (Width > 0) and (Alpha < 255) and Anim then Animation.Update;

 if gRenderBackgrounds <> nil then
  for a := 0 to High(gRenderBackgrounds) do
   with gRenderBackgrounds[a] do
    if (not Hide) and (Width > 0) and (Alpha < 255) and Anim then Animation.Update;

 if gRenderForegrounds <> nil then
  for a := 0 to High(gRenderForegrounds) do
   with gRenderForegrounds[a] do
    if (not Hide) and (Width > 0) and (Alpha < 255) and Anim then Animation.Update;

 if RenderSteps <> nil then
  for a := 0 to High(RenderSteps) do
   with RenderSteps[a] do
    if (not Hide) and (Width > 0) and (Alpha < 255) and Anim then Animation.Update;

 if gGameSettings.GameMode = GM_CTF then
 begin
  for a := FLAG_RED to FLAG_BLUE do
   if gFlags[a].State <> FLAG_STATE_CAPTURED then
   with gFlags[a] do
   begin
    if gFlags[a].Animation <> nil then gFlags[a].Animation.Update;

    m := g_Obj_Move(@Obj);
    if WordBool(m and MOVE_HITWATER) then g_Obj_Splash(@Obj);
    if gTime mod (GAME_TICK*2) <> 0 then Continue;

    Obj.Vel.X := z_dec(Obj.Vel.X, 1);

    if (Count = 0) or ByteBool(m and MOVE_FALLOUT) then
    begin
     g_Map_ResetFlag(a);
     if a = FLAG_RED then s := I_PLAYER_REDFLAG else s := I_PLAYER_BLUEFLAG;
     g_Game_Message(Format(I_MESSAGE_RETURNFLAG, [AnsiUpperCase(s)]), 144);
     Continue;
    end;

    if Count > 0 then Count := Count-1;

    if gPlayers <> nil then
    for d := 0 to High(gPlayers) do
     if gPlayers[d] <> nil then
      if gPlayers[d].Live and g_Obj_Collide(@Obj, @gPlayers[d].Obj) then
      begin
       gPlayers[d].GetFlag(a);
       Break;
      end;
    end;
 end;
end;

procedure g_Map_DrawPanels(PanelType: Word);

procedure DrawTexture(rpanel: PRenderPanel);
begin
 with rpanel^ do
 begin
  case TextureID of
   TEXTURE_SPECIAL_WATER: e_DrawFillQuad(X, Y, X+Width, Y+Height, 0, 0, 255, 0, B_FILTER);
   TEXTURE_SPECIAL_ACID1: e_DrawFillQuad(X, Y, X+Width, Y+Height, 0, 128, 0, 0, B_FILTER);
   TEXTURE_SPECIAL_ACID2: e_DrawFillQuad(X, Y, X+Width, Y+Height, 128, 0, 0, 0, B_FILTER);
   TEXTURE_SPECIAL_NONE: ;
   else e_DrawFill(TextureID, X, Y, Width div TextureWidth, Height div TextureHeight,
                   Alpha, True, Blending);
  end;
 end;
end;

procedure DrawAnim(rpanel: PRenderPanel);
var
  xx, yy: Integer;
begin
 with rpanel^ do
 begin
  if Animation = nil then Exit;
  
  for xx := 0 to (Width div TextureWidth)-1 do
   for yy := 0 to (Height div TextureHeight)-1 do
    Animation.Draw(X+xx*TextureWidth, Y+yy*TextureHeight, M_NONE);
 end;
end;

procedure DrawPanels(panels: TRPanelArray; door: Boolean = False);
var
  h, a: Integer;
begin
 if panels = nil then Exit;

 h := High(panels);

 for a := 0 to h do
  with panels[a] do
   if Enabled and ((not door) or (gWalls[a].Door)) and
      g_Collide(X, Y, Width, Height, sX, sY, sWidth, sHeight) and
      (not Hide) and (Width > 0) and (Alpha < 255) then
    if Anim then DrawAnim(@panels[a]) else DrawTexture(@panels[a]);
end;

begin
 case PanelType of
  PANEL_WALL: DrawPanels(gRenderWalls);
  PANEL_CLOSEDOOR: DrawPanels(gRenderWalls, True);
  PANEL_BACK: DrawPanels(gRenderBackgrounds);
  PANEL_FORE: DrawPanels(gRenderForegrounds);
  PANEL_WATER: DrawPanels(RenderWater);
  PANEL_ACID1: DrawPanels(RenderAcid1);
  PANEL_ACID2: DrawPanels(RenderAcid2);
  PANEL_STEP: DrawPanels(RenderSteps);
 end;
end;

procedure g_Map_DrawBack(dx, dy: Integer);
begin
 if gDrawBackGround and (BackID <> DWORD(-1)) then
  e_DrawSize(BackID, dx, dy, 0, False, False, gBackSize.X, gBackSize.Y)
   else e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);
end;

function g_Map_CollidePanel(X, Y: Integer; Width, Height: Word; PanelType: Word): Boolean;
var
  a, h: Integer;
begin
 Result := False;

 if WordBool(PanelType and PANEL_WALL) then
  if gWalls <> nil then
  begin
   h := High(gWalls);

   for a := 0 to h do
    if gWalls[a].Enabled and
       g_Collide(X, Y, Width, Height, gWalls[a].Rect.X, gWalls[a].Rect.Y,
                 gWalls[a].Rect.Width, gWalls[a].Rect.Height) then
    begin
     Result := True;
     Exit;
    end;
  end;

 if WordBool(PanelType and PANEL_WATER) then
  if gWater <> nil then
  begin
   h := High(gWater);

   for a := 0 to h do
    if g_Collide(X, Y, Width, Height, gWater[a].X, gWater[a].Y,
                 gWater[a].Width, gWater[a].Height) then
    begin
     Result := True;
     Exit;
    end;
  end;

 if WordBool(PanelType and PANEL_ACID1) then
  if gAcid1 <> nil then
  begin
   h := High(gAcid1);

   for a := 0 to h do
    if g_Collide(X, Y, Width, Height, gAcid1[a].X, gAcid1[a].Y,
                 gAcid1[a].Width, gAcid1[a].Height) then
    begin
     Result := True;
     Exit;
    end;
  end;

 if WordBool(PanelType and PANEL_ACID2) then
  if gAcid2 <> nil then
  begin
   h := High(gAcid2);

   for a := 0 to h do
    if g_Collide(X, Y, Width, Height, gAcid2[a].X, gAcid2[a].Y,
                 gAcid2[a].Width, gAcid2[a].Height) then
    begin
     Result := True;
     Exit;
    end;
  end;

 if WordBool(PanelType and PANEL_STEP) then
  if gSteps <> nil then
  begin
   h := High(gSteps);

   for a := 0 to h do
    if g_Collide(X, Y, Width, Height, gSteps[a].X, gSteps[a].Y,
                 gSteps[a].Width, gSteps[a].Height) then
    begin
     Result := True;
     Exit;
    end;
  end;

 if WordBool(PanelType and (PANEL_LIFTUP or PANEL_LIFTDOWN)) then
  if gLifts <> nil then
  begin
   h := High(gLifts);

   for a := 0 to h do
    if ((WordBool(PanelType and (PANEL_LIFTUP)) and (gLifts[a].LiftType = 0)) or
        (WordBool(PanelType and (PANEL_LIFTDOWN)) and (gLifts[a].LiftType = 1))) and
       g_Collide(X, Y, Width, Height, gLifts[a].Rect.X, gLifts[a].Rect.Y,
                 gLifts[a].Rect.Width, gLifts[a].Rect.Height) then
    begin
     Result := True;
     Exit;
    end;
  end;

 if WordBool(PanelType and PANEL_BLOCKMON) then
  if gBlockMon <> nil then
  begin
   h := High(gBlockMon);

   for a := 0 to h do
    if g_Collide(X, Y, Width, Height, gBlockMon[a].X, gBlockMon[a].Y,
                 gBlockMon[a].Width, gBlockMon[a].Height) then
    begin
     Result := True;
     Exit;
    end;
  end;
end;

procedure g_Map_EnableWall(RenderID: DWORD);
begin
 with gRenderWalls[RenderID] do
 begin
  Enabled := True;
  gWalls[WallID].Enabled := True;
  g_Mark(X, Y, Width, Height, MARK_DOOR);
 end;
end;

procedure g_Map_DisableWall(RenderID: DWORD);
begin
 with gRenderWalls[RenderID] do
 begin
  Enabled := False;
  gWalls[WallID].Enabled := False;
  g_Mark(X, Y, Width, Height, MARK_FREE);
 end;
end;

procedure g_Map_SwitchTexture(PanelType: Word; ID: DWORD; AnimLoop: Byte = 0);
var
  a: DWORD;
  tp: ^TRenderPanel;
  b: TAnimation;
begin
 case PanelType of
  PANEL_WALL: tp := @gRenderWalls[ID];
  PANEL_FORE: tp := @gRenderForegrounds[ID];
  PANEL_BACK: tp := @gRenderBackgrounds[ID];
  else Exit;
 end;

 if tp^.Anim then
 begin
  if (tp^.Animation = nil) then
    Exit;
  if (tp^.Animation2 <> nil) then
    begin
      b := tp^.Animation;
      tp^.Animation := tp^.Animation2;
      tp^.Animation2 := b;
    end
  else
    begin
      if tp^.Animation.Enabled then
        tp^.Animation.Disable
      else
        tp^.Animation.Enable;
    end;
  if AnimLoop = 1 then
    tp^.Animation.Loop := True
  else
    if AnimLoop = 2 then
      tp^.Animation.Loop := False;
  tp^.Animation.Reset();
 end
  else
 begin
  a := tp^.TextureID;
  tp^.TextureID := tp^.TextureID2;
  tp^.TextureID2 := a;
 end;

 tp^.SwitchTexture := not tp^.SwitchTexture;
end;

procedure g_Map_SetLift(ID: DWORD; t: Integer);
begin
 if gLifts[ID].LiftType = t then Exit;

 with gLifts[ID] do
 begin
  LiftType := t;
  if LiftType = 1 then g_Mark(Rect.X, Rect.Y, Rect.Width, Rect.Height, MARK_LIFTDOWN)
   else g_Mark(Rect.X, Rect.Y, Rect.Width, Rect.Height, MARK_LIFTUP);
 end;
end;

function g_Map_GetPoint(PointType: Byte; var RespawnPoint: TRespawnPoint): Boolean;
var
  a: Integer;
  PointsArray: array of TRespawnPoint;
begin
 Result := False;

 if RespawnPoints = nil then Exit;

 for a := 0 to High(RespawnPoints) do
  if RespawnPoints[a].PointType = PointType then
  begin
   SetLength(PointsArray, Length(PointsArray)+1);
   PointsArray[High(PointsArray)] := RespawnPoints[a];
  end;

 if PointsArray = nil then Exit;

 RespawnPoint := PointsArray[Random(Length(PointsArray))];
 Result := True;
end;

function g_Map_GetPointCount(PointType: Byte): Word;
var
  a: Integer;
begin
 Result := 0;

 if RespawnPoints = nil then Exit;

 for a := 0 to High(RespawnPoints) do
  if RespawnPoints[a].PointType = PointType then Result := Result+1;
end;

function g_Map_HaveFlagPoints(): Boolean;
begin
 Result := (FlagPoints[FLAG_RED] <> nil) and (FlagPoints[FLAG_BLUE] <> nil);
end;

procedure g_Map_ResetFlag(Flag: Byte);
begin
 with gFlags[Flag] do
 begin
  Obj.X := FlagPoints[Flag]^.X;
  Obj.Y := FlagPoints[Flag]^.Y;
  Obj.Vel.X := 0;
  Obj.Vel.Y := 0;
  Direction := FlagPoints[Flag]^.Direction;
  State := FLAG_STATE_NORMAL;
  Count := -1;
 end;
end;

procedure g_Map_DrawFlags();
var
  i: Integer;
  Mirror: TMirrorType;
begin
 if gGameSettings.GameMode <> GM_CTF then Exit;

 for i := FLAG_RED to FLAG_BLUE do
  with gFlags[i] do
   if State <> FLAG_STATE_CAPTURED then
   begin
    if Direction = D_LEFT then Mirror := M_HORIZONTAL
     else Mirror := M_NONE;
    Animation.Draw(Obj.X, Obj.Y, Mirror);

    //e_DrawQuad(Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y, Obj.X+Obj.Rect.X+Obj.Rect.Width,
    //           Obj.Y+Obj.Rect.Y+Obj.Rect.Height, 1, 255, 255, 255);
   end;
end;

type
  TStateRec = packed record
   panel_type: Word;
   id: Integer;
   enabled: Boolean;
   switch: Boolean;
   lift: Byte;
   anim1: TAnimRec;
   anim2: TAnimRec;
  end;

function g_Map_SaveState(var p: Pointer): Integer;
var
  a, s: Integer;
  map: TMapSaveRec;
  state: array of TStateRec;

procedure SavePanel(id: DWORD; panel: PRenderPanel; panel_type: Word);
var
  b: Integer;
begin
 if panel <> nil then
  if ((panel_type = PANEL_WALL) and gWalls[panel^.WallID].Door) or
     (panel^.Animation <> nil) or (panel^.Animation2 <> nil) or panel^.SwitchTexture then
 begin
  SetLength(state, Length(state)+1);
  b := High(state);
  state[b].panel_type := panel_type;
  state[b].id := id;
  state[b].enabled := panel^.Enabled;
  state[b].switch := panel^.SwitchTexture;
  if panel^.Anim then
  begin
   panel^.Animation.Save(@state[b].anim1);
   if panel^.Animation2 <> nil then panel^.Animation2.Save(@state[b].anim2);
  end;
 end;

 if panel_type = PANEL_LIFTUP then
 begin
  SetLength(state, Length(state)+1);
  b := High(state);
  state[b].panel_type := PANEL_LIFTUP;
  state[b].id := id;
  state[b].lift := gLifts[id].LiftType;
 end;
end;

begin
 for a := 0 to High(gRenderWalls) do
  SavePanel(a, @gRenderWalls[a], PANEL_WALL);
 for a := 0 to High(gRenderBackgrounds) do
  SavePanel(a, @gRenderBackgrounds[a], PANEL_BACK);
 for a := 0 to High(gRenderForegrounds) do
  SavePanel(a, @gRenderForegrounds[a], PANEL_FORE);
 for a := 0 to High(gLifts) do
  SavePanel(a, nil, PANEL_LIFTUP);

 map.state_count := Length(state);

 s := SizeOf(TMapSaveRec)+map.state_count*SizeOf(TStateRec);
 p := GetMemory(s);
 CopyMemory(p, @map, SizeOf(TMapSaveRec));

 if map.state_count > 0 then
  CopyMemory(Pointer(Integer(p)+SizeOf(TMapSaveRec)), @state[0], map.state_count*SizeOf(TStateRec));

 Result := s;
end;

procedure g_Map_LoadState(p: Pointer; len: Integer);
var
  a: Integer;
  map: TMapSaveRec;
  state: array of TStateRec;
  panel: PRenderPanel;

procedure LoadPanel(state_id: DWORD; panel: PRenderPanel);
begin
 if panel <> nil then
  if ((state[state_id].panel_type = PANEL_WALL) and gWalls[panel^.WallID].Door) or
     (panel^.Animation <> nil) or (panel^.Animation2 <> nil) or panel^.SwitchTexture then
 begin
  if state[state_id].panel_type = PANEL_WALL then
   if gWalls[panel^.WallID].Door then
    if state[state_id].enabled then g_Map_EnableWall(state[state_id].id)
     else g_Map_DisableWall(state[state_id].id);

  if state[state_id].switch then
   g_Map_SwitchTexture(state[state_id].panel_type, state[state_id].id);

  if panel^.Anim then
  begin
   panel^.Animation.Load(@state[state_id].anim1);
   if panel^.Animation2 <> nil then panel^.Animation2.Load(@state[state_id].anim2);
  end;
 end;

 if (state[state_id].panel_type = PANEL_LIFTUP) or
    (state[state_id].panel_type = PANEL_LIFTDOWN) then
  g_Map_SetLift(state[state_id].id, state[state_id].lift);
end;

begin
 if len < SizeOf(TMapSaveRec) then Exit;

 CopyMemory(@map, p, SizeOf(TMapSaveRec));
 if map.state_count = 0 then Exit;

 SetLength(state, map.state_count);
 CopyMemory(@state[0], Pointer(Integer(p)+SizeOf(TMapSaveRec)), map.state_count*SizeOf(TStateRec));

 for a := 0 to High(state) do
 begin
  case state[a].panel_type of
   PANEL_WALL: panel := @gRenderWalls[state[a].id];
   PANEL_FORE: panel := @gRenderForegrounds[state[a].id];
   PANEL_BACK: panel := @gRenderBackgrounds[state[a].id];
   else panel := nil;
  end;

  LoadPanel(a, panel);
 end;
end;

end.
