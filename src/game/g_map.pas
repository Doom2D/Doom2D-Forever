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
{$DEFINE MAP_DEBUG_ENABLED_FLAG}
unit g_map;

interface

uses
  e_graphics, g_basic, MAPDEF, g_textures, Classes,
  g_phys, wadreader, BinEditor, g_panel, g_grid, md5, binheap, xprofiler, xparser, xdynrec;

type
  TMapInfo = record
    Map:           String;
    Name:          String;
    Description:   String;
    Author:        String;
    MusicName:     String;
    SkyName:       String;
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

  PFlag = ^TFlag;
  TFlag = record
    Obj:         TObj;
    RespawnType: Byte;
    State:       Byte;
    Count:       Integer;
    CaptureTime: LongWord;
    Animation:   TAnimation;
    Direction:   TDirection;
  end;

function  g_Map_Load(Res: String): Boolean;
function  g_Map_GetMapInfo(Res: String): TMapInfo;
function  g_Map_GetMapsList(WADName: String): SArray;
function  g_Map_Exist(Res: String): Boolean;
procedure g_Map_Free();
procedure g_Map_Update();

procedure g_Map_DrawPanels (PanelType: Word); // unaccelerated
procedure g_Map_CollectDrawPanels (x0, y0, wdt, hgt: Integer);

procedure g_Map_DrawBack(dx, dy: Integer);
function  g_Map_CollidePanel(X, Y: Integer; Width, Height: Word;
                             PanelType: Word; b1x3: Boolean=false): Boolean;
function  g_Map_CollideLiquid_Texture(X, Y: Integer; Width, Height: Word): DWORD;
procedure g_Map_EnableWall(ID: DWORD);
procedure g_Map_DisableWall(ID: DWORD);
procedure g_Map_SwitchTexture(PanelType: Word; ID: DWORD; AnimLoop: Byte = 0);
procedure g_Map_SetLift(ID: DWORD; t: Integer);
procedure g_Map_ReAdd_DieTriggers();
function  g_Map_IsSpecialTexture(Texture: String): Boolean;

function  g_Map_GetPoint(PointType: Byte; var RespawnPoint: TRespawnPoint): Boolean;
function  g_Map_GetPointCount(PointType: Byte): Word;

function  g_Map_HaveFlagPoints(): Boolean;

procedure g_Map_ResetFlag(Flag: Byte);
procedure g_Map_DrawFlags();

function g_Map_PanelForPID(PanelID: Integer; var PanelArrayID: Integer): PPanel;

procedure g_Map_SaveState(Var Mem: TBinMemoryWriter);
procedure g_Map_LoadState(Var Mem: TBinMemoryReader);

procedure g_Map_DrawPanelShadowVolumes(lightX: Integer; lightY: Integer; radius: Integer);

// returns panel or nil
// sets `ex` and `ey` to `x1` and `y1` when no hit was detected
function g_Map_traceToNearestWall (x0, y0, x1, y1: Integer; hitx: PInteger=nil; hity: PInteger=nil): TPanel;

// returns panel or nil
// sets `ex` and `ey` to `x1` and `y1` when no hit was detected
function g_Map_traceToNearest (x0, y0, x1, y1: Integer; tag: Integer; hitx: PInteger=nil; hity: PInteger=nil): TPanel;

type
  TForEachPanelCB = function (pan: TPanel): Boolean; // return `true` to stop

function g_Map_HasAnyPanelAtPoint (x, y: Integer; panelType: Word): Boolean;
function g_Map_PanelAtPoint (x, y: Integer; tagmask: Integer=-1): TPanel;

// trace liquid, stepping by `dx` and `dy`
// return last seen liquid coords, and `false` if we're started outside of the liquid
function g_Map_TraceLiquidNonPrecise (x, y, dx, dy: Integer; out topx, topy: Integer): Boolean;


procedure g_Map_ProfilersBegin ();
procedure g_Map_ProfilersEnd ();


function g_Map_ParseMap (data: Pointer; dataLen: Integer): TDynRecord;

const
  NNF_NO_NAME         = 0;
  NNF_NAME_BEFORE     = 1;
  NNF_NAME_EQUALS     = 2;
  NNF_NAME_AFTER      = 3;

function g_Texture_NumNameFindStart(name: String): Boolean;
function g_Texture_NumNameFindNext(var newName: String): Byte;

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

  FLAG_STATE_NONE     = 0;
  FLAG_STATE_NORMAL   = 1;
  FLAG_STATE_DROPPED  = 2;
  FLAG_STATE_CAPTURED = 3;
  FLAG_STATE_SCORED   = 4; // Для эвентов через сетку.
  FLAG_STATE_RETURNED = 5; // Для эвентов через сетку.

  FLAG_TIME = 720; // 20 seconds

  SKY_STRETCH: Single = 1.5;

const
  GridTagInvalid = 0;

  (* draw order:
      PANEL_BACK
      PANEL_STEP
      PANEL_WALL
      PANEL_CLOSEDOOR
      PANEL_ACID1
      PANEL_ACID2
      PANEL_WATER
      PANEL_FORE
   *)
  // sorted by draw priority
  GridTagBack = 1 shl 0;
  GridTagStep = 1 shl 1;
  GridTagWall = 1 shl 2;
  GridTagDoor = 1 shl 3;
  GridTagAcid1 = 1 shl 4;
  GridTagAcid2 = 1 shl 5;
  GridTagWater = 1 shl 6;
  GridTagFore = 1 shl 7;
  // the following are invisible
  GridTagLift = 1 shl 8;
  GridTagBlockMon = 1 shl 9;

  GridDrawableMask = (GridTagBack or GridTagStep or GridTagWall or GridTagDoor or GridTagAcid1 or GridTagAcid2 or GridTagWater or GridTagFore);


var
  gWalls: TPanelArray;
  gRenderBackgrounds: TPanelArray;
  gRenderForegrounds: TPanelArray;
  gWater, gAcid1, gAcid2: TPanelArray;
  gSteps: TPanelArray;
  gLifts: TPanelArray;
  gBlockMon: TPanelArray;
  gFlags: array [FLAG_RED..FLAG_BLUE] of TFlag;
  //gDOMFlags: array of TFlag;
  gMapInfo: TMapInfo;
  gBackSize: TDFPoint;
  gDoorMap: array of array of DWORD;
  gLiftMap: array of array of DWORD;
  gWADHash: TMD5Digest;
  BackID:  DWORD = DWORD(-1);
  gExternalResources: TStringList;

  gdbg_map_use_accel_render: Boolean = true;
  gdbg_map_use_accel_coldet: Boolean = true;
  profMapCollision: TProfiler = nil; //WARNING: FOR DEBUGGING ONLY!
  gDrawPanelList: TBinaryHeapObj = nil; // binary heap of all walls we have to render, populated by `g_Map_CollectDrawPanels()`

  gCurrentMap: TDynRecord = nil;


function panelTypeToTag (panelType: Word): Integer; // returns GridTagXXX


type
  TPanelGrid = specialize TBodyGridBase<TPanel>;

var
  mapGrid: TPanelGrid = nil; // DO NOT USE! public for debugging only!


implementation

uses
  g_main, e_log, SysUtils, g_items, g_gfx, g_console,
  GL, GLExt, g_weapons, g_game, g_sound, e_sound, CONFIG,
  g_options, g_triggers, g_player,
  Math, g_monsters, g_saveload, g_language, g_netmsg,
  utils, sfs, xstreams, hashtable,
  ImagingTypes, Imaging, ImagingUtility,
  ImagingGif, ImagingNetworkGraphics;

const
  FLAGRECT: TRectWH = (X:15; Y:12; Width:33; Height:52);
  MUSIC_SIGNATURE = $4953554D; // 'MUSI'
  FLAG_SIGNATURE = $47414C46; // 'FLAG'


var
  dfmapdef: TDynMapDef = nil;

procedure loadMapDefinition ();
var
  pr: TTextParser = nil;
  st: TStream = nil;
  WAD: TWADFile = nil;
begin
  if (dfmapdef <> nil) then exit;
  try
    e_LogWritefln('parsing "mapdef.txt"...', []);
    st := openDiskFileRO(DataDir+'mapdef.txt');
  except
    st := nil;
    e_LogWritefln('local "%smapdef.txt" not found', [DataDir]);
  end;
  if (st = nil) then
  begin
    WAD := TWADFile.Create();
    if not WAD.ReadFile(GameWAD) then
    begin
      //raise Exception.Create('cannot load "game.wad"');
      st := nil;
    end
    else
    begin
      st := WAD.openFileStream('mapdef.txt');
    end;
  end;

  if (st = nil) then
  begin
    //raise Exception.Create('cannot open "mapdef.txt"');
    e_LogWritefln('using default "mapdef.txt"...', [], MSG_WARNING);
    pr := TStrTextParser.Create(defaultMapDef);
  end
  else
  begin
    pr := TFileTextParser.Create(st);
  end;

  try
    dfmapdef := TDynMapDef.Create(pr);
  except on e: Exception do
    raise Exception.Create(Format('ERROR in "mapdef.txt" at (%s,%s): %s', [pr.line, pr.col, e.message]));
  end;

  st.Free();
  WAD.Free();
end;


function g_Map_ParseMap (data: Pointer; dataLen: Integer): TDynRecord;
var
  wst: TSFSMemoryChunkStream = nil;
  pr: TTextParser = nil;
begin
  result := nil;
  if (dataLen < 4) then exit;
  loadMapDefinition();
  if (dfmapdef = nil) then raise Exception.Create('internal map loader error');

  wst := TSFSMemoryChunkStream.Create(data, dataLen);

  if (PAnsiChar(data)[0] = 'M') and (PAnsiChar(data)[1] = 'A') and (PAnsiChar(data)[2] = 'P') and (PByte(data)[3] = 1) then
  begin
    // binary map
    try
      result := dfmapdef.parseBinMap(wst);
    except on e: Exception do
      begin
        e_LogWritefln('ERROR: %s', [e.message]);
        wst.Free();
        result := nil;
        exit;
      end;
    end;
    wst.Free();
  end
  else
  begin
    // text map
    pr := TFileTextParser.Create(wst);
    try
      result := dfmapdef.parseMap(pr);
    except on e: Exception do
      begin
        if (pr <> nil) then e_LogWritefln('ERROR at (%s,%s): %s', [pr.line, pr.col, e.message])
        else e_LogWritefln('ERROR: %s', [e.message]);
        pr.Free(); // will free `wst`
        result := nil;
        exit;
      end;
    end;
    pr.Free(); // will free `wst`
  end;
end;


var
  NNF_PureName: String; // Имя текстуры без цифр в конце
  NNF_FirstNum: Integer; // Число у начальной текстуры
  NNF_CurrentNum: Integer; // Следующее число у текстуры


function g_Texture_NumNameFindStart(name: String): Boolean;
var
  i: Integer;

begin
  Result := False;
  NNF_PureName := '';
  NNF_FirstNum := -1;
  NNF_CurrentNum := -1;

  for i := Length(name) downto 1 do
    if (name[i] = '_') then // "_" - символ начала номерного постфикса
    begin
      if i = Length(name) then
        begin // Нет цифр в конце строки
          Exit;
        end
      else
        begin
          NNF_PureName := Copy(name, 1, i);
          Delete(name, 1, i);
          Break;
        end;
    end;

// Не перевести в число:
  if not TryStrToInt(name, NNF_FirstNum) then
    Exit;

  NNF_CurrentNum := 0;

  Result := True;
end;


function g_Texture_NumNameFindNext(var newName: String): Byte;
begin
  if (NNF_PureName = '') or (NNF_CurrentNum < 0) then
  begin
    newName := '';
    Result := NNF_NO_NAME;
    Exit;
  end;

  newName := NNF_PureName + IntToStr(NNF_CurrentNum);

  if NNF_CurrentNum < NNF_FirstNum then
    Result := NNF_NAME_BEFORE
  else
    if NNF_CurrentNum > NNF_FirstNum then
      Result := NNF_NAME_AFTER
    else
      Result := NNF_NAME_EQUALS;

  Inc(NNF_CurrentNum);
end;


function panelTypeToTag (panelType: Word): Integer;
begin
  case panelType of
    PANEL_WALL: result := GridTagWall; // gWalls
    PANEL_OPENDOOR, PANEL_CLOSEDOOR: result := GridTagDoor; // gWalls
    PANEL_BACK: result := GridTagBack; // gRenderBackgrounds
    PANEL_FORE: result := GridTagFore; // gRenderForegrounds
    PANEL_WATER: result := GridTagWater; // gWater
    PANEL_ACID1: result := GridTagAcid1; // gAcid1
    PANEL_ACID2: result := GridTagAcid2; // gAcid2
    PANEL_STEP: result := GridTagStep; // gSteps
    PANEL_LIFTUP, PANEL_LIFTDOWN, PANEL_LIFTLEFT, PANEL_LIFTRIGHT: result := GridTagLift; // gLifts -- this is for all lifts
    PANEL_BLOCKMON: result := GridTagBlockMon; // gBlockMon -- this is for all blockmons
    else result := GridTagInvalid;
  end;
end;


function dplLess (a, b: TObject): Boolean;
var
  pa, pb: TPanel;
begin
  pa := TPanel(a);
  pb := TPanel(b);
  if (pa.tag < pb.tag) then begin result := true; exit; end;
  if (pa.tag > pb.tag) then begin result := false; exit; end;
  result := (pa.arrIdx < pb.arrIdx);
end;

procedure dplClear ();
begin
  if (gDrawPanelList = nil) then gDrawPanelList := TBinaryHeapObj.Create(@dplLess) else gDrawPanelList.clear();
end;


type
  TPanelID = record
    PWhere: ^TPanelArray;
    PArrID: Integer;
  end;

var
  PanelById:     array of TPanelID;
  Textures:      TLevelTextureArray;
  RespawnPoints: Array of TRespawnPoint;
  FlagPoints:    Array [FLAG_RED..FLAG_BLUE] of PFlagPoint;
  //DOMFlagPoints: Array of TFlagPoint;


procedure g_Map_ProfilersBegin ();
begin
  if (profMapCollision = nil) then profMapCollision := TProfiler.Create('COLSOLID', g_profile_history_size);
  profMapCollision.mainBegin(g_profile_collision);
  // create sections
  if g_profile_collision then
  begin
    profMapCollision.sectionBegin('*solids');
    profMapCollision.sectionEnd();
    profMapCollision.sectionBegin('liquids');
    profMapCollision.sectionEnd();
  end;
end;

procedure g_Map_ProfilersEnd ();
begin
  if (profMapCollision <> nil) then profMapCollision.mainEnd();
end;


// wall index in `gWalls` or -1
function g_Map_traceToNearestWall (x0, y0, x1, y1: Integer; hitx: PInteger=nil; hity: PInteger=nil): TPanel;
var
  ex, ey: Integer;
begin
  result := mapGrid.traceRay(ex, ey, x0, y0, x1, y1, nil, (GridTagWall or GridTagDoor));
  if (result <> nil) then
  begin
    if (hitx <> nil) then hitx^ := ex;
    if (hity <> nil) then hity^ := ey;
  end
  else
  begin
    if (hitx <> nil) then hitx^ := x1;
    if (hity <> nil) then hity^ := y1;
  end;
end;

// returns panel or nil
function g_Map_traceToNearest (x0, y0, x1, y1: Integer; tag: Integer; hitx: PInteger=nil; hity: PInteger=nil): TPanel;
var
  ex, ey: Integer;
begin
  result := mapGrid.traceRay(ex, ey, x0, y0, x1, y1, nil, tag);
  if (result <> nil) then
  begin
    if (hitx <> nil) then hitx^ := ex;
    if (hity <> nil) then hity^ := ey;
  end
  else
  begin
    if (hitx <> nil) then hitx^ := x1;
    if (hity <> nil) then hity^ := y1;
  end;
end;


function g_Map_HasAnyPanelAtPoint (x, y: Integer; panelType: Word): Boolean;

  function checker (pan: TPanel; tag: Integer): Boolean;
  begin
    {
    if ((tag and (GridTagWall or GridTagDoor)) <> 0) then
    begin
      result := pan.Enabled; // stop if wall is enabled
      exit;
    end;
    }

    if ((tag and GridTagLift) <> 0) then
    begin
      // stop if the lift of the right type
      result :=
        ((WordBool(PanelType and PANEL_LIFTUP) and (pan.LiftType = 0)) or
         (WordBool(PanelType and PANEL_LIFTDOWN) and (pan.LiftType = 1)) or
         (WordBool(PanelType and PANEL_LIFTLEFT) and (pan.LiftType = 2)) or
         (WordBool(PanelType and PANEL_LIFTRIGHT) and (pan.LiftType = 3)));
      exit;
    end;

    result := true; // otherwise, stop anyway, 'cause `forEachAtPoint()` is guaranteed to call this only for correct panels
  end;

var
  tagmask: Integer = 0;
begin
  result := false;

  if WordBool(PanelType and (PANEL_WALL or PANEL_CLOSEDOOR or PANEL_OPENDOOR)) then tagmask := tagmask or (GridTagWall or GridTagDoor);
  if WordBool(PanelType and PANEL_WATER) then tagmask := tagmask or GridTagWater;
  if WordBool(PanelType and PANEL_ACID1) then tagmask := tagmask or GridTagAcid1;
  if WordBool(PanelType and PANEL_ACID2) then tagmask := tagmask or GridTagAcid2;
  if WordBool(PanelType and PANEL_STEP) then tagmask := tagmask or GridTagStep;
  if WordBool(PanelType and (PANEL_LIFTUP or PANEL_LIFTDOWN or PANEL_LIFTLEFT or PANEL_LIFTRIGHT)) then tagmask := tagmask or GridTagLift;
  if WordBool(PanelType and PANEL_BLOCKMON) then tagmask := tagmask or GridTagBlockMon;

  if (tagmask = 0) then exit;// just in case
  if ((tagmask and GridTagLift) <> 0) then
  begin
    // slow
    result := (mapGrid.forEachAtPoint(x, y, checker, tagmask) <> nil);
  end
  else
  begin
    // fast
    result := (mapGrid.forEachAtPoint(x, y, nil, tagmask) <> nil);
  end;
end;


function g_Map_PanelAtPoint (x, y: Integer; tagmask: Integer=-1): TPanel;
begin
  result := nil;
  if (tagmask = 0) then exit;
  result := mapGrid.forEachAtPoint(x, y, nil, tagmask);
end;


function g_Map_IsSpecialTexture(Texture: String): Boolean;
begin
  Result := (Texture = TEXTURE_NAME_WATER) or
            (Texture = TEXTURE_NAME_ACID1) or
            (Texture = TEXTURE_NAME_ACID2);
end;

procedure CreateDoorMap();
var
  PanelArray: Array of record
                         X, Y: Integer;
                         Width, Height: Word;
                         Active: Boolean;
                         PanelID: DWORD;
                       end;
  a, b, c, m, i, len: Integer;
  ok: Boolean;
begin
  if gWalls = nil then
    Exit;

  i := 0;
  len := 128;
  SetLength(PanelArray, len);

  for a := 0 to High(gWalls) do
    if gWalls[a].Door then
    begin
      PanelArray[i].X := gWalls[a].X;
      PanelArray[i].Y := gWalls[a].Y;
      PanelArray[i].Width := gWalls[a].Width;
      PanelArray[i].Height := gWalls[a].Height;
      PanelArray[i].Active := True;
      PanelArray[i].PanelID := a;

      i := i + 1;
      if i = len then
      begin
        len := len + 128;
        SetLength(PanelArray, len);
      end;
    end;

// Нет дверей:
  if i = 0 then
  begin
    PanelArray := nil;
    Exit;
  end;

  SetLength(gDoorMap, 0);

  g_Game_SetLoadingText(_lc[I_LOAD_DOOR_MAP], i-1, False);

  for a := 0 to i-1 do
    if PanelArray[a].Active then
    begin
      PanelArray[a].Active := False;
      m := Length(gDoorMap);
      SetLength(gDoorMap, m+1);
      SetLength(gDoorMap[m], 1);
      gDoorMap[m, 0] := PanelArray[a].PanelID;
      ok := True;

      while ok do
      begin
        ok := False;

        for b := 0 to i-1 do
          if PanelArray[b].Active then
            for c := 0 to High(gDoorMap[m]) do
              if {((gRenderWalls[PanelArray[b].RenderPanelID].TextureID = gRenderWalls[gDoorMap[m, c]].TextureID) or
                    gRenderWalls[PanelArray[b].RenderPanelID].Hide or gRenderWalls[gDoorMap[m, c]].Hide) and}
                g_CollideAround(PanelArray[b].X, PanelArray[b].Y,
                                PanelArray[b].Width, PanelArray[b].Height,
                                gWalls[gDoorMap[m, c]].X,
                                gWalls[gDoorMap[m, c]].Y,
                                gWalls[gDoorMap[m, c]].Width,
                                gWalls[gDoorMap[m, c]].Height) then
              begin
                PanelArray[b].Active := False;
                SetLength(gDoorMap[m],
                          Length(gDoorMap[m])+1);
                gDoorMap[m, High(gDoorMap[m])] := PanelArray[b].PanelID;
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
  PanelArray: Array of record
                         X, Y: Integer;
                         Width, Height: Word;
                         Active: Boolean;
                       end;
  a, b, c, len, i, j: Integer;
  ok: Boolean;
begin
  if gLifts = nil then
    Exit;

  len := Length(gLifts);
  SetLength(PanelArray, len);

  for a := 0 to len-1 do
  begin
    PanelArray[a].X := gLifts[a].X;
    PanelArray[a].Y := gLifts[a].Y;
    PanelArray[a].Width := gLifts[a].Width;
    PanelArray[a].Height := gLifts[a].Height;
    PanelArray[a].Active := True;
  end;

  SetLength(gLiftMap, len);
  i := 0;

  g_Game_SetLoadingText(_lc[I_LOAD_LIFT_MAP], len-1, False);

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
              if g_CollideAround(PanelArray[b].X,
                                 PanelArray[b].Y,
                                 PanelArray[b].Width,
                                 PanelArray[b].Height,
                                 PanelArray[gLiftMap[i, c]].X,
                                 PanelArray[gLiftMap[i, c]].Y,
                                 PanelArray[gLiftMap[i, c]].Width,
                                 PanelArray[gLiftMap[i, c]].Height) then
              begin
                PanelArray[b].Active := False;
                j := j+1;
                if j > High(gLiftMap[i]) then
                  SetLength(gLiftMap[i],
                            Length(gLiftMap[i])+32);

                gLiftMap[i, j] := b;
                ok := True;

                Break;
              end;
      end;

      SetLength(gLiftMap[i], j+1);
      i := i+1;

      g_Game_StepLoading();
    end;

  SetLength(gLiftMap, i);

  PanelArray := nil;
end;

function CreatePanel(PanelRec: TDynRecord; AddTextures: TAddTextureArray;
                     CurTex: Integer; sav: Boolean): Integer;
var
  len: Integer;
  panels: ^TPanelArray;
begin
  Result := -1;

  case PanelRec.PanelType of
    PANEL_WALL, PANEL_OPENDOOR, PANEL_CLOSEDOOR:
      panels := @gWalls;
    PANEL_BACK:
      panels := @gRenderBackgrounds;
    PANEL_FORE:
      panels := @gRenderForegrounds;
    PANEL_WATER:
      panels := @gWater;
    PANEL_ACID1:
      panels := @gAcid1;
    PANEL_ACID2:
      panels := @gAcid2;
    PANEL_STEP:
      panels := @gSteps;
    PANEL_LIFTUP, PANEL_LIFTDOWN, PANEL_LIFTLEFT, PANEL_LIFTRIGHT:
      panels := @gLifts;
    PANEL_BLOCKMON:
      panels := @gBlockMon;
    else
      Exit;
  end;

  len := Length(panels^);
  SetLength(panels^, len + 1);

  panels^[len] := TPanel.Create(PanelRec, AddTextures, CurTex, Textures);
  panels^[len].arrIdx := len;
  panels^[len].proxyId := -1;
  panels^[len].tag := panelTypeToTag(PanelRec.PanelType);
  if sav then
    panels^[len].SaveIt := True;

  Result := len;

  len := Length(PanelByID);
  SetLength(PanelByID, len + 1);
  PanelByID[len].PWhere := panels;
  PanelByID[len].PArrID := Result;
end;

function CreateNullTexture(RecName: String): Integer;
begin
  SetLength(Textures, Length(Textures)+1);
  result := High(Textures);

  with Textures[High(Textures)] do
  begin
    TextureName := RecName;
    Width := 1;
    Height := 1;
    Anim := False;
    TextureID := LongWord(TEXTURE_NONE);
  end;
end;

function CreateTexture(RecName: String; Map: string; log: Boolean): Integer;
var
  WAD: TWADFile;
  TextureData: Pointer;
  WADName, txname: String;
  a, ResLength: Integer;
begin
  Result := -1;

  if Textures <> nil then
    for a := 0 to High(Textures) do
      if Textures[a].TextureName = RecName then
      begin // Текстура с таким именем уже есть
        Result := a;
        Exit;
      end;

// Текстуры со специальными именами (вода, лава, кислота):
  if (RecName = TEXTURE_NAME_WATER) or
     (RecName = TEXTURE_NAME_ACID1) or
     (RecName = TEXTURE_NAME_ACID2) then
  begin
    SetLength(Textures, Length(Textures)+1);

    with Textures[High(Textures)] do
    begin
      TextureName := RecName;

      if TextureName = TEXTURE_NAME_WATER then
        TextureID := LongWord(TEXTURE_SPECIAL_WATER)
      else
        if TextureName = TEXTURE_NAME_ACID1 then
          TextureID := LongWord(TEXTURE_SPECIAL_ACID1)
        else
          if TextureName = TEXTURE_NAME_ACID2 then
            TextureID := LongWord(TEXTURE_SPECIAL_ACID2);

      Anim := False;
    end;

    result := High(Textures);
    Exit;
  end;

// Загружаем ресурс текстуры в память из WAD'а:
  WADName := g_ExtractWadName(RecName);

  WAD := TWADFile.Create();

  if WADName <> '' then
    WADName := GameDir+'/wads/'+WADName
  else
    WADName := Map;

  WAD.ReadFile(WADName);

  txname := RecName;
  {
  if (WADName = Map) and WAD.GetResource(g_ExtractFilePathName(RecName), TextureData, ResLength) then
  begin
    FreeMem(TextureData);
    RecName := 'COMMON\ALIEN';
  end;
  }

  if WAD.GetResource(g_ExtractFilePathName(RecName), TextureData, ResLength) then
    begin
      SetLength(Textures, Length(Textures)+1);
      if not e_CreateTextureMem(TextureData, ResLength, Textures[High(Textures)].TextureID) then
        Exit;
      e_GetTextureSize(Textures[High(Textures)].TextureID,
                       @Textures[High(Textures)].Width,
                       @Textures[High(Textures)].Height);
      FreeMem(TextureData);
      Textures[High(Textures)].TextureName := {RecName}txname;
      Textures[High(Textures)].Anim := False;

      result := High(Textures);
    end
  else // Нет такого реусрса в WAD'е
  begin
    //e_WriteLog(Format('SHIT! Error loading texture %s : %s : %s', [RecName, txname, g_ExtractFilePathName(RecName)]), MSG_WARNING);
    if log then
      begin
        e_WriteLog(Format('Error loading texture %s', [RecName]), MSG_WARNING);
        //e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
      end;
  end;

  WAD.Free();
end;

function CreateAnimTexture(RecName: String; Map: string; log: Boolean): Integer;
var
  WAD: TWADFile;
  TextureWAD: PChar = nil;
  TextData: Pointer = nil;
  TextureData: Pointer = nil;
  cfg: TConfig = nil;
  WADName: String;
  ResLength: Integer;
  TextureResource: String;
  _width, _height, _framecount, _speed: Integer;
  _backanimation: Boolean;
  //imgfmt: string;
  ia: TDynImageDataArray = nil;
  f, c, frdelay, frloop: Integer;
begin
  result := -1;

  //e_WriteLog(Format('*** Loading animated texture "%s"', [RecName]), MSG_NOTIFY);

  // Читаем WAD-ресурс аним.текстуры из WAD'а в память:
  WADName := g_ExtractWadName(RecName);

  WAD := TWADFile.Create();
  try
    if WADName <> '' then
      WADName := GameDir+'/wads/'+WADName
    else
      WADName := Map;

    WAD.ReadFile(WADName);

    if not WAD.GetResource(g_ExtractFilePathName(RecName), TextureWAD, ResLength) then
    begin
      if log then
      begin
        e_WriteLog(Format('Error loading animation texture %s', [RecName]), MSG_WARNING);
        //e_WriteLog(Format('WAD Reader error: %s', [WAD.GetLastErrorStr]), MSG_WARNING);
      end;
      exit;
    end;

    {TEST
    if WADName = Map then
    begin
      //FreeMem(TextureWAD);
      if not WAD.GetResource('COMMON/animation', TextureWAD, ResLength) then Halt(1);
    end;
    }

    WAD.FreeWAD();

    if ResLength < 6 then
    begin
      e_WriteLog(Format('Animated texture file "%s" too short', [RecName]), MSG_WARNING);
      exit;
    end;

    // это птица? это самолёт?
    if (TextureWAD[0] = 'D') and (TextureWAD[1] = 'F') and
       (TextureWAD[2] = 'W') and (TextureWAD[3] = 'A') and (TextureWAD[4] = 'D') then
    begin
      // нет, это супермен!
      if not WAD.ReadMemory(TextureWAD, ResLength) then
      begin
        e_WriteLog(Format('Animated texture WAD file "%s" is invalid', [RecName]), MSG_WARNING);
        exit;
      end;

      // Читаем INI-ресурс аним. текстуры и запоминаем его установки:
      if not WAD.GetResource('TEXT/ANIM', TextData, ResLength) then
      begin
        e_WriteLog(Format('Animated texture file "%s" has invalid INI', [RecName]), MSG_WARNING);
        exit;
      end;

      cfg := TConfig.CreateMem(TextData, ResLength);

      TextureResource := cfg.ReadStr('', 'resource', '');
      if TextureResource = '' then
      begin
        e_WriteLog(Format('Animated texture WAD file "%s" has no "resource"', [RecName]), MSG_WARNING);
        exit;
      end;

      _width := cfg.ReadInt('', 'framewidth', 0);
      _height := cfg.ReadInt('', 'frameheight', 0);
      _framecount := cfg.ReadInt('', 'framecount', 0);
      _speed := cfg.ReadInt('', 'waitcount', 0);
      _backanimation := cfg.ReadBool('', 'backanimation', False);

      cfg.Free();
      cfg := nil;

      // Читаем ресурс текстур (кадров) аним. текстуры в память:
      if not WAD.GetResource('TEXTURES/'+TextureResource, TextureData, ResLength) then
      begin
        e_WriteLog(Format('Animated texture WAD file "%s" has no texture "%s"', [RecName, 'TEXTURES/'+TextureResource]), MSG_WARNING);
        exit;
      end;

      WAD.Free();
      WAD := nil;

      SetLength(Textures, Length(Textures)+1);
      with Textures[High(Textures)] do
      begin
        // Создаем кадры аним. текстуры из памяти:
        if g_Frames_CreateMemory(@FramesID, '', TextureData, ResLength, _width, _height, _framecount, _backanimation) then
        begin
          TextureName := RecName;
          Width := _width;
          Height := _height;
          Anim := True;
          FramesCount := _framecount;
          Speed := _speed;
          result := High(Textures);
        end
        else
        begin
          if log then e_WriteLog(Format('Error loading animation texture %s', [RecName]), MSG_WARNING);
        end;
      end;
    end
    else
    begin
      // try animated image
      {
      imgfmt := DetermineMemoryFormat(TextureWAD, ResLength);
      if length(imgfmt) = 0 then
      begin
        e_WriteLog(Format('Animated texture file "%s" has unknown format', [RecName]), MSG_WARNING);
        exit;
      end;
      }
      GlobalMetadata.ClearMetaItems();
      GlobalMetadata.ClearMetaItemsForSaving();
      if not LoadMultiImageFromMemory(TextureWAD, ResLength, ia) then
      begin
        e_WriteLog(Format('Animated texture file "%s" cannot be loaded', [RecName]), MSG_WARNING);
        exit;
      end;
      if length(ia) = 0 then
      begin
        e_WriteLog(Format('Animated texture file "%s" has no frames', [RecName]), MSG_WARNING);
        exit;
      end;

      WAD.Free();
      WAD := nil;

      _width := ia[0].width;
      _height := ia[0].height;
      _framecount := length(ia);
      _speed := 1;
      _backanimation := false;
      frdelay := -1;
      frloop := -666;
      if GlobalMetadata.HasMetaItem(SMetaFrameDelay) then
      begin
        //writeln(' frame delay: ', GlobalMetadata.MetaItems[SMetaFrameDelay]);
        try
          f := GlobalMetadata.MetaItems[SMetaFrameDelay];
          frdelay := f;
          if f < 0 then f := 0;
          // rounding ;-)
          c := f mod 28;
          if c < 13 then c := 0 else c := 1;
          f := (f div 28)+c;
          if f < 1 then f := 1 else if f > 255 then f := 255;
          _speed := f;
        except
        end;
      end;
      if GlobalMetadata.HasMetaItem(SMetaAnimationLoops) then
      begin
        //writeln(' frame loop : ', GlobalMetadata.MetaItems[SMetaAnimationLoops]);
        try
          f := GlobalMetadata.MetaItems[SMetaAnimationLoops];
          frloop := f;
          if f <> 0 then _backanimation := true; // non-infinite looping == forth-and-back
        except
        end;
      end;
      //writeln(' creating animated texture with ', length(ia), ' frames (delay:', _speed, '; backloop:', _backanimation, ') from "', RecName, '"...');
      //for f := 0 to high(ia) do writeln('  frame #', f, ': ', ia[f].width, 'x', ia[f].height);
      f := ord(_backanimation);
      e_WriteLog(Format('Animated texture file "%s": %d frames (delay:%d; back:%d; frdelay:%d; frloop:%d), %dx%d', [RecName, length(ia), _speed, f, frdelay, frloop, _width, _height]), MSG_NOTIFY);

      SetLength(Textures, Length(Textures)+1);
      // cоздаем кадры аним. текстуры из картинок
      if g_CreateFramesImg(ia, @Textures[High(Textures)].FramesID, '', _backanimation) then
      begin
        Textures[High(Textures)].TextureName := RecName;
        Textures[High(Textures)].Width := _width;
        Textures[High(Textures)].Height := _height;
        Textures[High(Textures)].Anim := True;
        Textures[High(Textures)].FramesCount := length(ia);
        Textures[High(Textures)].Speed := _speed;
        result := High(Textures);
        //writeln(' CREATED!');
      end
      else
      begin
        if log then e_WriteLog(Format('Error loading animation texture "%s" images', [RecName]), MSG_WARNING);
      end;
    end;
  finally
    for f := 0 to High(ia) do FreeImage(ia[f]);
    WAD.Free();
    cfg.Free();
    if TextureWAD <> nil then FreeMem(TextureWAD);
    if TextData <> nil then FreeMem(TextData);
    if TextureData <> nil then FreeMem(TextureData);
  end;
end;

procedure CreateItem(Item: TDynRecord);
begin
  if g_Game_IsClient then Exit;

  if (not (gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF])) and
     ByteBool(Item.Options and ITEM_OPTION_ONLYDM) then
    Exit;

  g_Items_Create(Item.X, Item.Y, Item.ItemType, ByteBool(Item.Options and ITEM_OPTION_FALL),
                 gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF, GM_COOP]);
end;

procedure CreateArea(Area: TDynRecord);
var
  a: Integer;
  id: DWORD = 0;
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

procedure CreateTrigger(Trigger: TDynRecord; atpanid, atrigpanid: Integer; fTexturePanel1Type, fTexturePanel2Type: Word);
var
  _trigger: TTrigger;
begin
  if g_Game_IsClient and not (Trigger.TriggerType in [TRIGGER_SOUND, TRIGGER_MUSIC]) then Exit;

  with _trigger do
  begin
    mapId := Trigger.id;
    X := Trigger.X;
    Y := Trigger.Y;
    Width := Trigger.Width;
    Height := Trigger.Height;
    Enabled := Trigger.Enabled;
    //TexturePanel := Trigger.TexturePanel;
    TexturePanel := atpanid;
    TexturePanelType := fTexturePanel1Type;
    ShotPanelType := fTexturePanel2Type;
    TriggerType := Trigger.TriggerType;
    ActivateType := Trigger.ActivateType;
    Keys := Trigger.Keys;
    trigPanelId := atrigpanid;
    //trigShotPanelId := ashotpanid;
    //Data.Default := Trigger.DATA;
    if (Trigger.trigRec = nil) then
    begin
      trigData := nil;
      if (TriggerType <> TRIGGER_SECRET) then
      begin
        e_LogWritefln('trigger of type %s has no triggerdata; wtf?!', [TriggerType], MSG_WARNING);
      end;
    end
    else
    begin
      trigData := Trigger.trigRec.clone();
    end;
  end;

  g_Triggers_Create(_trigger);
end;

procedure CreateMonster(monster: TDynRecord);
var
  a: Integer;
  mon: TMonster;
begin
  if g_Game_IsClient then Exit;

  if (gGameSettings.GameType = GT_SINGLE)
  or LongBool(gGameSettings.Options and GAME_OPTION_MONSTERS) then
  begin
    mon := g_Monsters_Create(monster.MonsterType, monster.X, monster.Y, TDirection(monster.Direction));

    if gTriggers <> nil then
    begin
      for a := 0 to High(gTriggers) do
      begin
        if gTriggers[a].TriggerType in [TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF] then
        begin
          //if (gTriggers[a].Data.MonsterID-1) = Integer(mon.StartID) then mon.AddTrigger(a);
          if (gTriggers[a].trigData.trigMonsterId) = Integer(mon.StartID) then mon.AddTrigger(a);
        end;
      end;
    end;

    if monster.MonsterType <> MONSTER_BARREL then Inc(gTotalMonsters);
  end;
end;

procedure g_Map_ReAdd_DieTriggers();

  function monsDieTrig (mon: TMonster): Boolean;
  var
    a: Integer;
  begin
    result := false; // don't stop
    mon.ClearTriggers();
    for a := 0 to High(gTriggers) do
    begin
      if gTriggers[a].TriggerType in [TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF] then
      begin
        //if (gTriggers[a].Data.MonsterID-1) = Integer(mon.StartID) then mon.AddTrigger(a);
        if (gTriggers[a].trigData.trigMonsterId) = Integer(mon.StartID) then mon.AddTrigger(a);
      end;
    end;
  end;

begin
  if g_Game_IsClient then Exit;

  g_Mons_ForEach(monsDieTrig);
end;

function extractWadName(resourceName: string): string;
var
  posN: Integer;
begin
  posN := Pos(':', resourceName);
  if posN > 0 then
    Result:= Copy(resourceName, 0, posN-1)
  else
    Result := '';
end;

procedure addResToExternalResList(res: string);
begin
  res := extractWadName(res);
  if (res <> '') and (gExternalResources.IndexOf(res) = -1) then
    gExternalResources.Add(res);
end;

procedure generateExternalResourcesList({mapReader: TMapReader_1}map: TDynRecord);
//var
  //textures: TTexturesRec1Array;
  //textures: TDynField;
  //trec: TDynRecord;
  //mapHeader: TMapHeaderRec_1;
  //i: integer;
  //resFile: String = '';
begin
  if gExternalResources = nil then
    gExternalResources := TStringList.Create;

  gExternalResources.Clear;

  (*
  {
  textures := GetTextures(map);
  for i := 0 to High(textures) do
  begin
    addResToExternalResList(resFile);
  end;
  }

  textures := map['texture'];
  if (textures <> nil) then
  begin
    for trec in textures do
    begin
      addResToExternalResList(resFile);
    end;
  end;

  textures := nil;
  *)

  //mapHeader := GetMapHeader(map);

  addResToExternalResList(map.MusicName);
  addResToExternalResList(map.SkyName);
end;


procedure mapCreateGrid ();
var
  mapX0: Integer = $3fffffff;
  mapY0: Integer = $3fffffff;
  mapX1: Integer = -$3fffffff;
  mapY1: Integer = -$3fffffff;

  procedure calcBoundingBox (constref panels: TPanelArray);
  var
    idx: Integer;
    pan: TPanel;
  begin
    for idx := 0 to High(panels) do
    begin
      pan := panels[idx];
      if not pan.visvalid then continue;
      if (pan.Width < 1) or (pan.Height < 1) then continue;
      if (mapX0 > pan.x0) then mapX0 := pan.x0;
      if (mapY0 > pan.y0) then mapY0 := pan.y0;
      if (mapX1 < pan.x1) then mapX1 := pan.x1;
      if (mapY1 < pan.y1) then mapY1 := pan.y1;
    end;
  end;

  procedure addPanelsToGrid (constref panels: TPanelArray);
  var
    idx: Integer;
    pan: TPanel;
    newtag: Integer;
  begin
    //tag := panelTypeToTag(tag);
    for idx := 0 to High(panels) do
    begin
      pan := panels[idx];
      if not pan.visvalid then continue;
      if (pan.proxyId <> -1) then
      begin
        {$IF DEFINED(D2F_DEBUG)}
        e_WriteLog(Format('DUPLICATE wall #%d(%d) enabled (%d); type:%08x', [Integer(idx), Integer(pan.proxyId), Integer(mapGrid.proxyEnabled[pan.proxyId]), pan.PanelType]), MSG_NOTIFY);
        {$ENDIF}
        continue;
      end;
      case pan.PanelType of
        PANEL_WALL: newtag := GridTagWall;
        PANEL_OPENDOOR, PANEL_CLOSEDOOR: newtag := GridTagDoor;
        PANEL_BACK: newtag := GridTagBack;
        PANEL_FORE: newtag := GridTagFore;
        PANEL_WATER: newtag := GridTagWater;
        PANEL_ACID1: newtag := GridTagAcid1;
        PANEL_ACID2: newtag := GridTagAcid2;
        PANEL_STEP: newtag := GridTagStep;
        PANEL_LIFTUP, PANEL_LIFTDOWN, PANEL_LIFTLEFT, PANEL_LIFTRIGHT: newtag := GridTagLift;
        PANEL_BLOCKMON: newtag := GridTagBlockMon;
        else continue; // oops
      end;
      pan.tag := newtag;

      pan.proxyId := mapGrid.insertBody(pan, pan.X, pan.Y, pan.Width, pan.Height, newtag);
      // "enabled" flag has meaning only for doors and walls (engine assumes it); but meh...
      mapGrid.proxyEnabled[pan.proxyId] := pan.Enabled;
      {$IFDEF MAP_DEBUG_ENABLED_FLAG}
      {
      if ((tag and (GridTagWall or GridTagDoor)) <> 0) then
      begin
        e_WriteLog(Format('INSERTED wall #%d(%d) enabled (%d)', [Integer(idx), Integer(pan.proxyId), Integer(mapGrid.proxyEnabled[pan.proxyId])]), MSG_NOTIFY);
      end;
      }
      {$ENDIF}
    end;
  end;

begin
  mapGrid.Free();
  mapGrid := nil;

  calcBoundingBox(gWalls);
  calcBoundingBox(gRenderBackgrounds);
  calcBoundingBox(gRenderForegrounds);
  calcBoundingBox(gWater);
  calcBoundingBox(gAcid1);
  calcBoundingBox(gAcid2);
  calcBoundingBox(gSteps);
  calcBoundingBox(gLifts);
  calcBoundingBox(gBlockMon);

  e_LogWritefln('map dimensions: (%d,%d)-(%d,%d); editor size:(0,0)-(%d,%d)', [mapX0, mapY0, mapX1, mapY1, gMapInfo.Width, gMapInfo.Height]);

  if (mapX0 > 0) then mapX0 := 0;
  if (mapY0 > 0) then mapY0 := 0;

  if (mapX1 < gMapInfo.Width-1) then mapX1 := gMapInfo.Width-1;
  if (mapY1 < gMapInfo.Height-1) then mapY1 := gMapInfo.Height-1;

  mapGrid := TPanelGrid.Create(mapX0-128, mapY0-128, mapX1-mapX0+1+128*2, mapY1-mapY0+1+128*2);
  //mapGrid := TPanelGrid.Create(0, 0, gMapInfo.Width, gMapInfo.Height);

  addPanelsToGrid(gWalls);
  addPanelsToGrid(gRenderBackgrounds);
  addPanelsToGrid(gRenderForegrounds);
  addPanelsToGrid(gWater);
  addPanelsToGrid(gAcid1);
  addPanelsToGrid(gAcid2);
  addPanelsToGrid(gSteps);
  addPanelsToGrid(gLifts); // it doesn't matter which LIFT type is used here
  addPanelsToGrid(gBlockMon);

  mapGrid.dumpStats();

  g_Mons_InitTree(mapGrid.gridX0, mapGrid.gridY0, mapGrid.gridWidth, mapGrid.gridHeight);
end;


function g_Map_Load(Res: String): Boolean;
const
  DefaultMusRes = 'Standart.wad:STDMUS\MUS1';
  DefaultSkyRes = 'Standart.wad:STDSKY\SKY0';
type
  PTRec = ^TTRec;
  TTRec = record
    //TexturePanel: Integer;
    texPanIdx: Integer;
    LiftPanelIdx: Integer;
    DoorPanelIdx: Integer;
    ShotPanelIdx: Integer;
    trigrec: TDynRecord;
    texPan: TDynRecord;
    liftPan: TDynRecord;
    doorPan: TDynRecord;
    shotPan: TDynRecord;
  end;
var
  WAD: TWADFile;
  mapReader: TDynRecord = nil;
  mapTextureList: TDynField = nil; //TTexturesRec1Array; tagInt: texture index
  panels: TDynField = nil; //TPanelsRec1Array;
  items: TDynField = nil; //TItemsRec1Array;
  monsters: TDynField = nil; //TMonsterRec1Array;
  areas: TDynField = nil; //TAreasRec1Array;
  triggers: TDynField = nil; //TTriggersRec1Array;
  b, c, k: Integer;
  PanelID: DWORD;
  AddTextures: TAddTextureArray;
  TriggersTable: array of TTRec;
  FileName, mapResName, s, TexName: String;
  Data: Pointer;
  Len: Integer;
  ok, isAnim, trigRef: Boolean;
  CurTex, ntn: Integer;
  rec, texrec: TDynRecord;
  pttit: PTRec;
  pannum, trignum, cnt, tgpid: Integer;
begin
  mapGrid.Free();
  mapGrid := nil;

  gCurrentMap.Free();
  gCurrentMap := nil;

  Result := False;
  gMapInfo.Map := Res;
  TriggersTable := nil;
  mapReader := nil;

  sfsGCDisable(); // temporary disable removing of temporary volumes
  try
    // Загрузка WAD:
    FileName := g_ExtractWadName(Res);
    e_WriteLog('Loading map WAD: '+FileName, MSG_NOTIFY);
    g_Game_SetLoadingText(_lc[I_LOAD_WAD_FILE], 0, False);

    WAD := TWADFile.Create();
    if not WAD.ReadFile(FileName) then
    begin
      g_FatalError(Format(_lc[I_GAME_ERROR_MAP_WAD], [FileName]));
      WAD.Free();
      Exit;
    end;

    //k8: why loader ignores path here?
    mapResName := g_ExtractFileName(Res);
    if not WAD.GetMapResource(mapResName, Data, Len) then
    begin
      g_FatalError(Format(_lc[I_GAME_ERROR_MAP_RES], [mapResName]));
      WAD.Free();
      Exit;
    end;

    WAD.Free();

    if (Len < 4) then
    begin
      e_LogWritefln('invalid map file: ''%s''', [mapResName]);
      FreeMem(Data);
      exit;
    end;

    // Загрузка карты:
    e_LogWritefln('Loading map: %s', [mapResName], MSG_NOTIFY);
    g_Game_SetLoadingText(_lc[I_LOAD_MAP], 0, False);

    try
      mapReader := g_Map_ParseMap(Data, Len);
    except
      mapReader.Free();
      g_FatalError(Format(_lc[I_GAME_ERROR_MAP_LOAD], [Res]));
      FreeMem(Data);
      Exit;
    end;

    FreeMem(Data);

    generateExternalResourcesList(mapReader);
    mapTextureList := mapReader['texture'];
    // get all other lists here too
    panels := mapReader['panel'];
    triggers := mapReader['trigger'];
    items := mapReader['item'];
    areas := mapReader['area'];
    monsters := mapReader['monster'];

    // Загрузка описания карты:
    e_WriteLog('  Reading map info...', MSG_NOTIFY);
    g_Game_SetLoadingText(_lc[I_LOAD_MAP_HEADER], 0, False);

    with gMapInfo do
    begin
      Name := mapReader.MapName;
      Description := mapReader.MapDesc;
      Author := mapReader.MapAuthor;
      MusicName := mapReader.MusicName;
      SkyName := mapReader.SkyName;
      Height := mapReader.Height;
      Width := mapReader.Width;
    end;

    // Загрузка текстур:
    g_Game_SetLoadingText(_lc[I_LOAD_TEXTURES], 0, False);
    // Добавление текстур в Textures[]:
    if (mapTextureList <> nil) and (mapTextureList.count > 0) then
    begin
      e_WriteLog('  Loading textures:', MSG_NOTIFY);
      g_Game_SetLoadingText(_lc[I_LOAD_TEXTURES], mapTextureList.count-1, False);

      cnt := -1;
      for rec in mapTextureList do
      begin
        Inc(cnt);
        s := rec.Resource;
        {$IF DEFINED(D2F_DEBUG_TXLOAD)}
        e_WriteLog(Format('    Loading texture #%d: %s', [cnt, s]), MSG_NOTIFY);
        {$ENDIF}
        //if g_Map_IsSpecialTexture(s) then e_WriteLog('      SPECIAL!', MSG_NOTIFY);
        if rec.Anim then
        begin
          // Анимированная текстура
          ntn := CreateAnimTexture(rec.Resource, FileName, True);
          if (ntn < 0) then g_SimpleError(Format(_lc[I_GAME_ERROR_TEXTURE_ANIM], [s]));
        end
        else
        begin
          // Обычная текстура
          ntn := CreateTexture(rec.Resource, FileName, True);
          if (ntn < 0) then g_SimpleError(Format(_lc[I_GAME_ERROR_TEXTURE_SIMPLE], [s]));
        end;
        if (ntn < 0) then ntn := CreateNullTexture(rec.Resource);

        rec.tagInt := ntn; // remember texture number
        g_Game_StepLoading();
      end;

      // set panel tagInt to texture index
      if (panels <> nil) then
      begin
        for rec in panels do
        begin
          texrec := rec.TextureRec;
          if (texrec = nil) then rec.tagInt := -1 else rec.tagInt := texrec.tagInt;
        end;
      end;
    end;

    // Загрузка триггеров
    gTriggerClientID := 0;
    e_WriteLog('  Loading triggers...', MSG_NOTIFY);
    g_Game_SetLoadingText(_lc[I_LOAD_TRIGGERS], 0, False);

    // Загрузка панелей
    e_WriteLog('  Loading panels...', MSG_NOTIFY);
    g_Game_SetLoadingText(_lc[I_LOAD_PANELS], 0, False);

    // check texture numbers for panels
    if (panels <> nil) and (panels.count > 0) then
    begin
      for rec in panels do
      begin
        if (rec.tagInt < 0) then
        begin
          e_WriteLog('error loading map: invalid texture index for panel', MSG_FATALERROR);
          result := false;
          exit;
        end;
      end;
    end;

    // Создание таблицы триггеров (соответствие панелей триггерам)
    if (triggers <> nil) and (triggers.count > 0) then
    begin
      e_WriteLog('  Setting up trigger table...', MSG_NOTIFY);
      //SetLength(TriggersTable, triggers.count);
      g_Game_SetLoadingText(_lc[I_LOAD_TRIGGERS_TABLE], triggers.count-1, False);

      for rec in triggers do
      begin
        SetLength(TriggersTable, Length(TriggersTable)+1);
        pttit := @TriggersTable[High(TriggersTable)];
        pttit.trigrec := rec;
        // Смена текстуры (возможно, кнопки)
        pttit.texPan := mapReader.panel[rec.TexturePanel];
        pttit.liftPan := nil;
        pttit.doorPan := nil;
        pttit.shotPan := nil;
        pttit.texPanIdx := -1;
        pttit.LiftPanelIdx := -1;
        pttit.DoorPanelIdx := -1;
        pttit.ShotPanelIdx := -1;
        // Лифты
        if rec.TriggerType in [TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT] then
        begin
          pttit.liftPan := mapReader.panel[rec.trigRec.tgPanelID];
        end;
        // Двери
        if rec.TriggerType in [TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR, TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP] then
        begin
          pttit.doorPan := mapReader.panel[rec.trigRec.tgPanelID];
        end;
        // Турель
        if (rec.TriggerType = TRIGGER_SHOT) then
        begin
          pttit.shotPan := mapReader.panel[rec.trigRec.tgShotPanelID];
        end;

        g_Game_StepLoading();
      end;
    end;

    // Создаем панели
    if (panels <> nil) and (panels.count > 0) then
    begin
      e_WriteLog('  Setting up trigger links...', MSG_NOTIFY);
      g_Game_SetLoadingText(_lc[I_LOAD_LINK_TRIGGERS], panels.count-1, False);

      pannum := -1;
      for rec in panels do
      begin
        Inc(pannum);
        texrec := nil;
        SetLength(AddTextures, 0);
        trigRef := False;
        CurTex := -1;
        ok := false;

        if (mapTextureList <> nil) then
        begin
          texrec := rec.TextureRec;
          ok := (texrec <> nil);
        end;

        if ok then
        begin
          // Смотрим, ссылаются ли на эту панель триггеры.
          // Если да - то надо создать еще текстур
          ok := false;
          if (TriggersTable <> nil) and (mapTextureList <> nil) then
          begin
            for b := 0 to High(TriggersTable) do
            begin
              if (TriggersTable[b].texPan = rec) or (TriggersTable[b].shotPan = rec) then
              begin
                trigRef := True;
                ok := True;
                break;
              end;
            end;
          end;
        end;

        if ok then
        begin
          // Есть ссылки триггеров на эту панель
          s := texrec.Resource;

          // Спец-текстуры запрещены
          if g_Map_IsSpecialTexture(s) then
          begin
            ok := false
          end
          else
          begin
            // Определяем наличие и положение цифр в конце строки
            ok := g_Texture_NumNameFindStart(s);
          end;

          // Если ok, значит есть цифры в конце.
          // Загружаем текстуры с остальными #
          if ok then
          begin
            k := NNF_NAME_BEFORE;
            // Цикл по изменению имени текстуры
            while ok or (k = NNF_NAME_BEFORE) or (k = NNF_NAME_EQUALS) do
            begin
              k := g_Texture_NumNameFindNext(TexName);

              if (k = NNF_NAME_BEFORE) or (k = NNF_NAME_AFTER) then
              begin
                // Пробуем добавить новую текстуру
                if texrec.Anim then
                begin
                  // Начальная - анимированная, ищем анимированную
                  isAnim := True;
                  ok := CreateAnimTexture(TexName, FileName, False) >= 0;
                  if not ok then
                  begin
                    // Нет анимированной, ищем обычную
                    isAnim := False;
                    ok := CreateTexture(TexName, FileName, False) >= 0;
                  end;
                end
                else
                begin
                  // Начальная - обычная, ищем обычную
                  isAnim := False;
                  ok := CreateTexture(TexName, FileName, False) >= 0;
                  if not ok then
                  begin
                    // Нет обычной, ищем анимированную
                    isAnim := True;
                    ok := CreateAnimTexture(TexName, FileName, False) >= 0;
                  end;
                end;

                // Она существует. Заносим ее ID в список панели
                if ok then
                begin
                  for c := 0 to High(Textures) do
                  begin
                    if (Textures[c].TextureName = TexName) then
                    begin
                      SetLength(AddTextures, Length(AddTextures)+1);
                      AddTextures[High(AddTextures)].Texture := c;
                      AddTextures[High(AddTextures)].Anim := isAnim;
                      break;
                    end;
                  end;
                end;
              end
              else
              begin
                if k = NNF_NAME_EQUALS then
                begin
                  // Заносим текущую текстуру на свое место
                  SetLength(AddTextures, Length(AddTextures)+1);
                  AddTextures[High(AddTextures)].Texture := rec.tagInt; // internal texture number, not map index
                  AddTextures[High(AddTextures)].Anim := texrec.Anim;
                  CurTex := High(AddTextures);
                  ok := true;
                end
                else // NNF_NO_NAME
                begin
                  ok := false;
                end;
              end;
            end; // while ok...

            ok := true;
          end; // if ok - есть смежные текстуры
        end; // if ok - ссылаются триггеры

        if not ok then
        begin
          // Заносим только текущую текстуру
          SetLength(AddTextures, 1);
          AddTextures[0].Texture := rec.tagInt; // internal texture number, not map index
          AddTextures[0].Anim := false;
          if (texrec <> nil) then AddTextures[0].Anim := texrec.Anim;
          CurTex := 0;
        end;

        //e_WriteLog(Format('panel #%d: TextureNum=%d; ht=%d; ht1=%d; atl=%d', [a, panels[a].TextureNum, High(mapTextureList), High(Textures), High(AddTextures)]), MSG_NOTIFY);

        // Создаем панель и запоминаем ее номер
        PanelID := CreatePanel(rec, AddTextures, CurTex, trigRef);
        //e_LogWritefln('panel #%s of type %s got id #%s', [pannum, rec.PanelType, PanelID]);
        // set 'gamePanelId' field to panel id
        rec.gamePanelId := PanelID; // remember game panel id, we'll fix triggers later

        g_Game_StepLoading();
      end;
    end;

    // Чиним ID'ы панелей, которые используются в триггерах
    for b := 0 to High(TriggersTable) do
    begin
      if (TriggersTable[b].texPan <> nil) then TriggersTable[b].texPanIdx := TriggersTable[b].texPan.gamePanelId;
      if (TriggersTable[b].liftPan <> nil) then TriggersTable[b].LiftPanelIdx := TriggersTable[b].liftPan.gamePanelId;
      if (TriggersTable[b].doorPan <> nil) then TriggersTable[b].DoorPanelIdx := TriggersTable[b].doorPan.gamePanelId;
      if (TriggersTable[b].shotPan <> nil) then TriggersTable[b].ShotPanelIdx := TriggersTable[b].shotPan.gamePanelId;
    end;

    // create map grid, init other grids (for monsters, for example)
    e_WriteLog('Creating map grid', MSG_NOTIFY);
    mapCreateGrid();

    // Если не LoadState, то создаем триггеры
    if (triggers <> nil) and (panels <> nil) and (not gLoadGameMode) then
    begin
      e_LogWritefln('  Creating triggers (%d)...', [triggers.count]);
      g_Game_SetLoadingText(_lc[I_LOAD_CREATE_TRIGGERS], 0, False);
      // Указываем тип панели, если есть
      trignum := -1;
      for rec in triggers do
      begin
        Inc(trignum);
        if (TriggersTable[trignum].texPan <> nil) then b := TriggersTable[trignum].texPan.PanelType else b := 0;
        if (TriggersTable[trignum].shotPan <> nil) then c := TriggersTable[trignum].shotPan.PanelType else c := 0;
        // we can have only one of those
             if (TriggersTable[trignum].LiftPanelIdx <> -1) then tgpid := TriggersTable[trignum].LiftPanelIdx
        else if (TriggersTable[trignum].DoorPanelIdx <> -1) then tgpid := TriggersTable[trignum].DoorPanelIdx
        else if (TriggersTable[trignum].ShotPanelIdx <> -1) then tgpid := TriggersTable[trignum].ShotPanelIdx
        else tgpid := -1;
        //e_LogWritefln('creating trigger #%s; texpantype=%s; shotpantype=%s (%d,%d)', [trignum, b, c, TriggersTable[trignum].texPanIdx, TriggersTable[trignum].ShotPanelIdx]);
        CreateTrigger(rec, TriggersTable[trignum].texPanIdx, tgpid, Word(b), Word(c));
      end;
    end;

    // Загрузка предметов
    e_WriteLog('  Loading items...', MSG_NOTIFY);
    g_Game_SetLoadingText(_lc[I_LOAD_ITEMS], 0, False);

    // Если не LoadState, то создаем предметы
    if (items <> nil) and not gLoadGameMode then
    begin
      e_WriteLog('  Spawning items...', MSG_NOTIFY);
      g_Game_SetLoadingText(_lc[I_LOAD_CREATE_ITEMS], 0, False);
      for rec in items do CreateItem(rec);
    end;

    // Загрузка областей
    e_WriteLog('  Loading areas...', MSG_NOTIFY);
    g_Game_SetLoadingText(_lc[I_LOAD_AREAS], 0, False);

    // Если не LoadState, то создаем области
    if areas <> nil then
    begin
      e_WriteLog('  Creating areas...', MSG_NOTIFY);
      g_Game_SetLoadingText(_lc[I_LOAD_CREATE_AREAS], 0, False);
      for rec in areas do CreateArea(rec);
    end;

    // Загрузка монстров
    e_WriteLog('  Loading monsters...', MSG_NOTIFY);
    g_Game_SetLoadingText(_lc[I_LOAD_MONSTERS], 0, False);

    gTotalMonsters := 0;

    // Если не LoadState, то создаем монстров
    if (monsters <> nil) and not gLoadGameMode then
    begin
      e_WriteLog('  Spawning monsters...', MSG_NOTIFY);
      g_Game_SetLoadingText(_lc[I_LOAD_CREATE_MONSTERS], 0, False);
      for rec in monsters do CreateMonster(rec);
    end;

    gCurrentMap := mapReader; // this will be our current map now
    mapReader := nil;

    // Загрузка неба
    if gMapInfo.SkyName <> '' then
    begin
      e_WriteLog('  Loading sky: ' + gMapInfo.SkyName, MSG_NOTIFY);
      g_Game_SetLoadingText(_lc[I_LOAD_SKY], 0, False);
      FileName := g_ExtractWadName(gMapInfo.SkyName);

      if FileName <> '' then
        FileName := GameDir+'/wads/'+FileName
      else
        begin
          FileName := g_ExtractWadName(Res);
        end;

      s := FileName+':'+g_ExtractFilePathName(gMapInfo.SkyName);
      if g_Texture_CreateWAD(BackID, s) then
        begin
          g_Game_SetupScreenSize();
        end
      else
        g_FatalError(Format(_lc[I_GAME_ERROR_SKY], [s]));
    end;

    // Загрузка музыки
    ok := False;
    if gMapInfo.MusicName <> '' then
    begin
      e_WriteLog('  Loading music: ' + gMapInfo.MusicName, MSG_NOTIFY);
      g_Game_SetLoadingText(_lc[I_LOAD_MUSIC], 0, False);
      FileName := g_ExtractWadName(gMapInfo.MusicName);

      if FileName <> '' then
        FileName := GameDir+'/wads/'+FileName
      else
        begin
          FileName := g_ExtractWadName(Res);
        end;

      s := FileName+':'+g_ExtractFilePathName(gMapInfo.MusicName);
      if g_Sound_CreateWADEx(gMapInfo.MusicName, s, True) then
        ok := True
      else
        g_FatalError(Format(_lc[I_GAME_ERROR_MUSIC], [s]));
    end;

    // Остальные устанвки
    CreateDoorMap();
    CreateLiftMap();

    g_Items_Init();
    g_Weapon_Init();
    g_Monsters_Init();

    // Если не LoadState, то создаем карту столкновений:
    if not gLoadGameMode then g_GFX_Init();

    // Сброс локальных массивов:
    mapTextureList := nil;
    panels := nil;
    items := nil;
    areas := nil;
    triggers := nil;
    TriggersTable := nil;
    AddTextures := nil;

    // Включаем музыку, если это не загрузка:
    if ok and (not gLoadGameMode) then
    begin
      gMusic.SetByName(gMapInfo.MusicName);
      gMusic.Play();
    end
    else
    begin
      gMusic.SetByName('');
    end;
  finally
    sfsGCEnable(); // enable releasing unused volumes
    mapReader.Free();
  end;

  e_WriteLog('Done loading map.', MSG_NOTIFY);
  Result := True;
end;


function g_Map_GetMapInfo(Res: String): TMapInfo;
var
  WAD: TWADFile;
  mapReader: TDynRecord;
  //Header: TMapHeaderRec_1;
  FileName: String;
  Data: Pointer;
  Len: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  FileName := g_ExtractWadName(Res);

  WAD := TWADFile.Create();
  if not WAD.ReadFile(FileName) then
  begin
    WAD.Free();
    Exit;
  end;

  //k8: it ignores path again
  if not WAD.GetMapResource(g_ExtractFileName(Res), Data, Len) then
  begin
    WAD.Free();
    Exit;
  end;

  WAD.Free();

  {
  MapReader := TMapReader_1.Create();
  if not MapReader.LoadMap(Data) then
    begin
      g_Console_Add(Format(_lc[I_GAME_ERROR_MAP_LOAD], [Res]), True);
      ZeroMemory(@Header, SizeOf(Header));
      Result.Name := _lc[I_GAME_ERROR_MAP_SELECT];
      Result.Description := _lc[I_GAME_ERROR_MAP_SELECT];
    end
  else
    begin
      Header := MapReader.GetMapHeader();
      Result.Name := Header.MapName;
      Result.Description := Header.MapDescription;
    end;
  }
  try
    mapReader := g_Map_ParseMap(Data, Len);
  except
    mapReader := nil;
  end;

  FreeMem(Data);
  //MapReader.Free();

  //if (mapReader <> nil) then Header := GetMapHeader(mapReader) else FillChar(Header, sizeof(Header), 0);
  //MapReader.Free();

  if (mapReader.Width > 0) and (mapReader.Height > 0) then
  begin
    Result.Name := mapReader.MapName;
    Result.Description := mapReader.MapDesc;
    Result.Map := Res;
    Result.Author := mapReader.MapAuthor;
    Result.Height := mapReader.Height;
    Result.Width := mapReader.Width;
  end
  else
  begin
    g_Console_Add(Format(_lc[I_GAME_ERROR_MAP_LOAD], [Res]), True);
    //ZeroMemory(@Header, SizeOf(Header));
    Result.Name := _lc[I_GAME_ERROR_MAP_SELECT];
    Result.Description := _lc[I_GAME_ERROR_MAP_SELECT];
    Result.Map := Res;
    Result.Author := '';
    Result.Height := 0;
    Result.Width := 0;
  end;

  mapReader.Free();
end;

function g_Map_GetMapsList(WADName: string): SArray;
var
  WAD: TWADFile;
  a: Integer;
  ResList: SArray;
begin
  Result := nil;
  WAD := TWADFile.Create();
  if not WAD.ReadFile(WADName) then
  begin
    WAD.Free();
    Exit;
  end;
  ResList := WAD.GetMapResources();
  if ResList <> nil then
  begin
    for a := 0 to High(ResList) do
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := ResList[a];
    end;
  end;
  WAD.Free();
end;

function g_Map_Exist(Res: string): Boolean;
var
  WAD: TWADFile;
  FileName, mnn: string;
  ResList: SArray;
  a: Integer;
begin
  Result := False;

  FileName := addWadExtension(g_ExtractWadName(Res));

  WAD := TWADFile.Create;
  if not WAD.ReadFile(FileName) then
  begin
    WAD.Free();
    Exit;
  end;

  ResList := WAD.GetMapResources();
  WAD.Free();

  mnn := g_ExtractFileName(Res);
  if ResList <> nil then
    for a := 0 to High(ResList) do if StrEquCI1251(ResList[a], mnn) then
    begin
      Result := True;
      Exit;
    end;
end;

procedure g_Map_Free();
var
  a: Integer;

  procedure FreePanelArray(var panels: TPanelArray);
  var
    i: Integer;

  begin
    if panels <> nil then
    begin
      for i := 0 to High(panels) do
        panels[i].Free();
      panels := nil;
    end;
  end;

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

  //gDOMFlags := nil;

  if Textures <> nil then
  begin
    for a := 0 to High(Textures) do
      if not g_Map_IsSpecialTexture(Textures[a].TextureName) then
        if Textures[a].Anim then
          g_Frames_DeleteByID(Textures[a].FramesID)
        else
          if Textures[a].TextureID <> LongWord(TEXTURE_NONE) then
            e_DeleteTexture(Textures[a].TextureID);

    Textures := nil;
  end;

  FreePanelArray(gWalls);
  FreePanelArray(gRenderBackgrounds);
  FreePanelArray(gRenderForegrounds);
  FreePanelArray(gWater);
  FreePanelArray(gAcid1);
  FreePanelArray(gAcid2);
  FreePanelArray(gSteps);
  FreePanelArray(gLifts);
  FreePanelArray(gBlockMon);

  if BackID <> DWORD(-1) then
  begin
    gBackSize.X := 0;
    gBackSize.Y := 0;
    e_DeleteTexture(BackID);
    BackID := DWORD(-1);
  end;

  g_Game_StopAllSounds(False);
  gMusic.FreeSound();
  g_Sound_Delete(gMapInfo.MusicName);

  gMapInfo.Name := '';
  gMapInfo.Description := '';
  gMapInfo.MusicName := '';
  gMapInfo.Height := 0;
  gMapInfo.Width := 0;

  gDoorMap := nil;
  gLiftMap := nil;

  PanelByID := nil;
end;

procedure g_Map_Update();
var
  a, d, j: Integer;
  m: Word;
  s: String;

  procedure UpdatePanelArray(var panels: TPanelArray);
  var
    i: Integer;

  begin
    if panels <> nil then
      for i := 0 to High(panels) do
        panels[i].Update();
  end;

begin
  UpdatePanelArray(gWalls);
  UpdatePanelArray(gRenderBackgrounds);
  UpdatePanelArray(gRenderForegrounds);
  UpdatePanelArray(gWater);
  UpdatePanelArray(gAcid1);
  UpdatePanelArray(gAcid2);
  UpdatePanelArray(gSteps);

  if gGameSettings.GameMode = GM_CTF then
  begin
    for a := FLAG_RED to FLAG_BLUE do
      if not (gFlags[a].State in [FLAG_STATE_NONE, FLAG_STATE_CAPTURED]) then
        with gFlags[a] do
        begin
          if gFlags[a].Animation <> nil then
            gFlags[a].Animation.Update();

          m := g_Obj_Move(@Obj, True, True);

          if gTime mod (GAME_TICK*2) <> 0 then
            Continue;

        // Сопротивление воздуха:
          Obj.Vel.X := z_dec(Obj.Vel.X, 1);

        // Таймаут потерянного флага, либо он выпал за карту:
          if ((Count = 0) or ByteBool(m and MOVE_FALLOUT)) and g_Game_IsServer then
          begin
            g_Map_ResetFlag(a);
            gFlags[a].CaptureTime := 0;
            if a = FLAG_RED then
              s := _lc[I_PLAYER_FLAG_RED]
            else
              s := _lc[I_PLAYER_FLAG_BLUE];
            g_Game_Message(Format(_lc[I_MESSAGE_FLAG_RETURN], [AnsiUpperCase(s)]), 144);

            if g_Game_IsNet then
              MH_SEND_FlagEvent(FLAG_STATE_RETURNED, a, 0);
            Continue;
          end;

          if Count > 0 then
            Count := Count - 1;

        // Игрок берет флаг:
          if gPlayers <> nil then
          begin
            j := Random(Length(gPlayers)) - 1;

            for d := 0 to High(gPlayers) do
            begin
              Inc(j);
              if j > High(gPlayers) then
                j := 0;

              if gPlayers[j] <> nil then
                if gPlayers[j].Live and
                   g_Obj_Collide(@Obj, @gPlayers[j].Obj) then
                begin
                  if gPlayers[j].GetFlag(a) then
                    Break;
                end;
            end;
          end;
        end;
  end;
end;


// old algo
procedure g_Map_DrawPanels (PanelType: Word);

  procedure DrawPanels (constref panels: TPanelArray; drawDoors: Boolean=False);
  var
    idx: Integer;
  begin
    if (panels <> nil) then
    begin
      // alas, no visible set
      for idx := 0 to High(panels) do
      begin
        if not (drawDoors xor panels[idx].Door) then panels[idx].Draw();
      end;
    end;
  end;

begin
  case PanelType of
    PANEL_WALL:       DrawPanels(gWalls);
    PANEL_CLOSEDOOR:  DrawPanels(gWalls, True);
    PANEL_BACK:       DrawPanels(gRenderBackgrounds);
    PANEL_FORE:       DrawPanels(gRenderForegrounds);
    PANEL_WATER:      DrawPanels(gWater);
    PANEL_ACID1:      DrawPanels(gAcid1);
    PANEL_ACID2:      DrawPanels(gAcid2);
    PANEL_STEP:       DrawPanels(gSteps);
  end;
end;


// new algo
procedure g_Map_CollectDrawPanels (x0, y0, wdt, hgt: Integer);

  function checker (pan: TPanel; tag: Integer): Boolean;
  begin
    result := false; // don't stop, ever
    if ((tag and GridTagDoor) <> 0) <> pan.Door then exit;
    gDrawPanelList.insert(pan);
  end;

begin
  dplClear();
  //tagmask := panelTypeToTag(PanelType);
  mapGrid.forEachInAABB(x0, y0, wdt, hgt, checker, GridDrawableMask);
  // list will be rendered in `g_game.DrawPlayer()`
end;


procedure g_Map_DrawPanelShadowVolumes(lightX: Integer; lightY: Integer; radius: Integer);

  function checker (pan: TPanel; tag: Integer): Boolean;
  begin
    result := false; // don't stop, ever
    pan.DrawShadowVolume(lightX, lightY, radius);
  end;

begin
  mapGrid.forEachInAABB(lightX-radius, lightY-radius, radius*2, radius*2, checker, (GridTagWall or GridTagDoor));
end;


procedure g_Map_DrawBack(dx, dy: Integer);
begin
  if gDrawBackGround and (BackID <> DWORD(-1)) then
    e_DrawSize(BackID, dx, dy, 0, False, False, gBackSize.X, gBackSize.Y)
  else
    e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);
end;

function g_Map_CollidePanelOld(X, Y: Integer; Width, Height: Word;
                            PanelType: Word; b1x3: Boolean=false): Boolean;
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
        g_Collide(X, Y, Width, Height,
                  gWalls[a].X, gWalls[a].Y,
                  gWalls[a].Width, gWalls[a].Height) then
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
      if g_Collide(X, Y, Width, Height,
                   gWater[a].X, gWater[a].Y,
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
        if g_Collide(X, Y, Width, Height,
                     gAcid1[a].X, gAcid1[a].Y,
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
        if g_Collide(X, Y, Width, Height,
                     gAcid2[a].X, gAcid2[a].Y,
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
        if g_Collide(X, Y, Width, Height,
                     gSteps[a].X, gSteps[a].Y,
                     gSteps[a].Width, gSteps[a].Height) then
        begin
          Result := True;
          Exit;
        end;
    end;

  if WordBool(PanelType and (PANEL_LIFTUP or PANEL_LIFTDOWN or PANEL_LIFTLEFT or PANEL_LIFTRIGHT)) then
    if gLifts <> nil then
    begin
      h := High(gLifts);

      for a := 0 to h do
        if ((WordBool(PanelType and (PANEL_LIFTUP)) and (gLifts[a].LiftType = 0)) or
           (WordBool(PanelType and (PANEL_LIFTDOWN)) and (gLifts[a].LiftType = 1)) or
           (WordBool(PanelType and (PANEL_LIFTLEFT)) and (gLifts[a].LiftType = 2)) or
           (WordBool(PanelType and (PANEL_LIFTRIGHT)) and (gLifts[a].LiftType = 3))) and
           g_Collide(X, Y, Width, Height,
           gLifts[a].X, gLifts[a].Y,
           gLifts[a].Width, gLifts[a].Height) then
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
        if ( (not b1x3) or
             ((gBlockMon[a].Width + gBlockMon[a].Height) >= 64) ) and
           g_Collide(X, Y, Width, Height,
           gBlockMon[a].X, gBlockMon[a].Y,
           gBlockMon[a].Width, gBlockMon[a].Height) then
        begin
          Result := True;
          Exit;
        end;
    end;
end;

function g_Map_CollideLiquid_TextureOld(X, Y: Integer; Width, Height: Word): DWORD;
var
  texid: DWORD;

  function checkPanels (constref panels: TPanelArray): Boolean;
  var
    a: Integer;
  begin
    result := false;
    if panels = nil then exit;
    for a := 0 to High(panels) do
    begin
      if g_Collide(X, Y, Width, Height, panels[a].X, panels[a].Y, panels[a].Width, panels[a].Height) then
      begin
        result := true;
        texid := panels[a].GetTextureID();
        exit;
      end;
    end;
  end;

begin
  texid := LongWord(TEXTURE_NONE);
  result := texid;
  if not checkPanels(gWater) then
    if not checkPanels(gAcid1) then
      if not checkPanels(gAcid2) then exit;
  result := texid;
end;


function g_Map_CollidePanel(X, Y: Integer; Width, Height: Word; PanelType: Word; b1x3: Boolean): Boolean;
const
  SlowMask = GridTagLift or GridTagBlockMon;
  function checker (pan: TPanel; tag: Integer): Boolean;
  begin
    {
    if ((tag and (GridTagWall or GridTagDoor)) <> 0) then
    begin
      result := pan.Enabled;
      exit;
    end;
    }

    if ((tag and GridTagLift) <> 0) then
    begin
      result :=
        ((WordBool(PanelType and PANEL_LIFTUP) and (pan.LiftType = 0)) or
         (WordBool(PanelType and PANEL_LIFTDOWN) and (pan.LiftType = 1)) or
         (WordBool(PanelType and PANEL_LIFTLEFT) and (pan.LiftType = 2)) or
         (WordBool(PanelType and PANEL_LIFTRIGHT) and (pan.LiftType = 3))) {and
         g_Collide(X, Y, Width, Height, pan.X, pan.Y, pan.Width, pan.Height)};
      exit;
    end;

    if ((tag and GridTagBlockMon) <> 0) then
    begin
      result := ((not b1x3) or (pan.Width+pan.Height >= 64)); //and g_Collide(X, Y, Width, Height, pan.X, pan.Y, pan.Width, pan.Height);
      exit;
    end;

    // other shit
    //result := g_Collide(X, Y, Width, Height, pan.X, pan.Y, pan.Width, pan.Height);
    result := true; // i found her!
  end;

var
  tagmask: Integer = 0;
begin
  if WordBool(PanelType and (PANEL_WALL or PANEL_CLOSEDOOR or PANEL_OPENDOOR)) then tagmask := tagmask or (GridTagWall or GridTagDoor);
  if WordBool(PanelType and PANEL_WATER) then tagmask := tagmask or GridTagWater;
  if WordBool(PanelType and PANEL_ACID1) then tagmask := tagmask or GridTagAcid1;
  if WordBool(PanelType and PANEL_ACID2) then tagmask := tagmask or GridTagAcid2;
  if WordBool(PanelType and PANEL_STEP) then tagmask := tagmask or GridTagStep;
  if WordBool(PanelType and (PANEL_LIFTUP or PANEL_LIFTDOWN or PANEL_LIFTLEFT or PANEL_LIFTRIGHT)) then tagmask := tagmask or GridTagLift;
  if WordBool(PanelType and PANEL_BLOCKMON) then tagmask := tagmask or GridTagBlockMon;

  if (tagmask = 0) then begin result := false; exit; end; // just in case

  if (profMapCollision <> nil) then profMapCollision.sectionBeginAccum('*solids');
  if gdbg_map_use_accel_coldet then
  begin
    if (Width = 1) and (Height = 1) then
    begin
      if ((tagmask and SlowMask) <> 0) then
      begin
        // slow
        result := (mapGrid.forEachAtPoint(X, Y, checker, tagmask) <> nil);
      end
      else
      begin
        // fast
        result := (mapGrid.forEachAtPoint(X, Y, nil, tagmask) <> nil);
      end;
    end
    else
    begin
      if ((tagmask and SlowMask) <> 0) then
      begin
        // slow
        result := (mapGrid.forEachInAABB(X, Y, Width, Height, checker, tagmask) <> nil);
      end
      else
      begin
        // fast
        result := (mapGrid.forEachInAABB(X, Y, Width, Height, nil, tagmask) <> nil);
      end;
    end;
  end
  else
  begin
    result := g_Map_CollidePanelOld(X, Y, Width, Height, PanelType, b1x3);
  end;
  if (profMapCollision <> nil) then profMapCollision.sectionEnd();
end;


function g_Map_CollideLiquid_Texture(X, Y: Integer; Width, Height: Word): DWORD;
var
  cctype: Integer = 3; // priority: 0: water was hit, 1: acid1 was hit, 2: acid2 was hit; 3: nothing was hit
  texid: DWORD;

  // slightly different from the old code, but meh...
  function checker (pan: TPanel; tag: Integer): Boolean;
  begin
    result := false; // don't stop, ever
    //if ((tag and (GridTagWater or GridTagAcid1 or GridTagAcid2)) = 0) then exit;
    // check priorities
    case cctype of
      0: if ((tag and GridTagWater) = 0) then exit; // allowed: water
      1: if ((tag and (GridTagWater or GridTagAcid1)) = 0) then exit; // allowed: water, acid1
      //2: if ((tag and (GridTagWater or GridTagAcid1 or GridTagAcid2) = 0) then exit; // allowed: water, acid1, acid2
    end;
    // collision?
    //if not g_Collide(X, Y, Width, Height, pan.X, pan.Y, pan.Width, pan.Height) then exit;
    // yeah
    texid := pan.GetTextureID();
    // water? water has the highest priority, so stop right here
    if ((tag and GridTagWater) <> 0) then begin cctype := 0; result := true; exit; end;
    // acid2?
    if ((tag and GridTagAcid2) <> 0) then cctype := 2;
    // acid1?
    if ((tag and GridTagAcid1) <> 0) then cctype := 1;
  end;

begin
  if (profMapCollision <> nil) then profMapCollision.sectionBeginAccum('liquids');
  if gdbg_map_use_accel_coldet then
  begin
    texid := LongWord(TEXTURE_NONE);
    if (Width = 1) and (Height = 1) then
    begin
      mapGrid.forEachAtPoint(X, Y, checker, (GridTagWater or GridTagAcid1 or GridTagAcid2));
    end
    else
    begin
      mapGrid.forEachInAABB(X, Y, Width, Height, checker, (GridTagWater or GridTagAcid1 or GridTagAcid2));
    end;
    result := texid;
  end
  else
  begin
    result := g_Map_CollideLiquid_TextureOld(X, Y, Width, Height);
  end;
  if (profMapCollision <> nil) then profMapCollision.sectionEnd();
end;


procedure g_Map_EnableWall(ID: DWORD);
var
  pan: TPanel;
begin
  pan := gWalls[ID];
  pan.Enabled := True;
  g_Mark(pan.X, pan.Y, pan.Width, pan.Height, MARK_DOOR, True);

  mapGrid.proxyEnabled[pan.proxyId] := true;
  //if (pan.proxyId >= 0) then mapGrid.proxyEnabled[pan.proxyId] := true
  //else pan.proxyId := mapGrid.insertBody(pan, pan.X, pan.Y, pan.Width, pan.Height, GridTagDoor);

  if g_Game_IsServer and g_Game_IsNet then MH_SEND_PanelState(gWalls[ID].PanelType, ID);

  {$IFDEF MAP_DEBUG_ENABLED_FLAG}
  //e_WriteLog(Format('ENABLE: wall #%d(%d) enabled (%d)  (%d,%d)-(%d,%d)', [Integer(ID), Integer(pan.proxyId), Integer(mapGrid.proxyEnabled[pan.proxyId]), pan.x, pan.y, pan.width, pan.height]), MSG_NOTIFY);
  {$ENDIF}
end;

procedure g_Map_DisableWall(ID: DWORD);
var
  pan: TPanel;
begin
  pan := gWalls[ID];
  pan.Enabled := False;
  g_Mark(pan.X, pan.Y, pan.Width, pan.Height, MARK_DOOR, False);

  mapGrid.proxyEnabled[pan.proxyId] := false;
  //if (pan.proxyId >= 0) then begin mapGrid.removeBody(pan.proxyId); pan.proxyId := -1; end;

  if g_Game_IsServer and g_Game_IsNet then MH_SEND_PanelState(pan.PanelType, ID);

  {$IFDEF MAP_DEBUG_ENABLED_FLAG}
  //e_WriteLog(Format('DISABLE: wall #%d(%d) disabled (%d)  (%d,%d)-(%d,%d)', [Integer(ID), Integer(pan.proxyId), Integer(mapGrid.proxyEnabled[pan.proxyId]), pan.x, pan.y, pan.width, pan.height]), MSG_NOTIFY);
  {$ENDIF}
end;

procedure g_Map_SwitchTexture(PanelType: Word; ID: DWORD; AnimLoop: Byte = 0);
var
  tp: TPanel;
begin
  case PanelType of
    PANEL_WALL, PANEL_OPENDOOR, PANEL_CLOSEDOOR:
      tp := gWalls[ID];
    PANEL_FORE:
      tp := gRenderForegrounds[ID];
    PANEL_BACK:
      tp := gRenderBackgrounds[ID];
    PANEL_WATER:
      tp := gWater[ID];
    PANEL_ACID1:
      tp := gAcid1[ID];
    PANEL_ACID2:
      tp := gAcid2[ID];
    PANEL_STEP:
      tp := gSteps[ID];
    else
      Exit;
  end;

  tp.NextTexture(AnimLoop);
  if g_Game_IsServer and g_Game_IsNet then
    MH_SEND_PanelTexture(PanelType, ID, AnimLoop);
end;

procedure g_Map_SetLift(ID: DWORD; t: Integer);
begin
  if gLifts[ID].LiftType = t then
    Exit;

  with gLifts[ID] do
  begin
    LiftType := t;

    g_Mark(X, Y, Width, Height, MARK_LIFT, False);
    //TODO: make separate lift tags, and change tag here

    if LiftType = 0 then
      g_Mark(X, Y, Width, Height, MARK_LIFTUP, True)
    else if LiftType = 1 then
      g_Mark(X, Y, Width, Height, MARK_LIFTDOWN, True)
    else if LiftType = 2 then
      g_Mark(X, Y, Width, Height, MARK_LIFTLEFT, True)
    else if LiftType = 3 then
      g_Mark(X, Y, Width, Height, MARK_LIFTRIGHT, True);

    if g_Game_IsServer and g_Game_IsNet then MH_SEND_PanelState(PanelType, ID);
  end;
end;

function g_Map_GetPoint(PointType: Byte; var RespawnPoint: TRespawnPoint): Boolean;
var
  a: Integer;
  PointsArray: Array of TRespawnPoint;
begin
  Result := False;
  SetLength(PointsArray, 0);

  if RespawnPoints = nil then
    Exit;

  for a := 0 to High(RespawnPoints) do
    if RespawnPoints[a].PointType = PointType then
    begin
      SetLength(PointsArray, Length(PointsArray)+1);
      PointsArray[High(PointsArray)] := RespawnPoints[a];
    end;

  if PointsArray = nil then
    Exit;

  RespawnPoint := PointsArray[Random(Length(PointsArray))];
  Result := True;
end;

function g_Map_GetPointCount(PointType: Byte): Word;
var
  a: Integer;
begin
  Result := 0;

  if RespawnPoints = nil then
    Exit;

  for a := 0 to High(RespawnPoints) do
    if RespawnPoints[a].PointType = PointType then
      Result := Result + 1;
end;

function g_Map_HaveFlagPoints(): Boolean;
begin
  Result := (FlagPoints[FLAG_RED] <> nil) and (FlagPoints[FLAG_BLUE] <> nil);
end;

procedure g_Map_ResetFlag(Flag: Byte);
begin
  with gFlags[Flag] do
  begin
    Obj.X := -1000;
    Obj.Y := -1000;
    Obj.Vel.X := 0;
    Obj.Vel.Y := 0;
    Direction := D_LEFT;
    State := FLAG_STATE_NONE;
    if FlagPoints[Flag] <> nil then
    begin
      Obj.X := FlagPoints[Flag]^.X;
      Obj.Y := FlagPoints[Flag]^.Y;
      Direction := FlagPoints[Flag]^.Direction;
      State := FLAG_STATE_NORMAL;
    end;
    Count := -1;
  end;
end;

procedure g_Map_DrawFlags();
var
  i, dx: Integer;
  Mirror: TMirrorType;
begin
  if gGameSettings.GameMode <> GM_CTF then
    Exit;

  for i := FLAG_RED to FLAG_BLUE do
    with gFlags[i] do
      if State <> FLAG_STATE_CAPTURED then
      begin
        if State = FLAG_STATE_NONE then
          continue;

        if Direction = D_LEFT then
          begin
            Mirror := M_HORIZONTAL;
            dx := -1;
          end
        else
          begin
            Mirror := M_NONE;
            dx := 1;
          end;

        Animation.Draw(Obj.X+dx, Obj.Y+1, Mirror);

        if g_debug_Frames then
        begin
          e_DrawQuad(Obj.X+Obj.Rect.X,
                     Obj.Y+Obj.Rect.Y,
                     Obj.X+Obj.Rect.X+Obj.Rect.Width-1,
                     Obj.Y+Obj.Rect.Y+Obj.Rect.Height-1,
                     0, 255, 0);
        end;
      end;
end;

procedure g_Map_SaveState(Var Mem: TBinMemoryWriter);
var
  dw: DWORD;
  b: Byte;
  str: String;
  boo: Boolean;

  procedure SavePanelArray(var panels: TPanelArray);
  var
    PAMem: TBinMemoryWriter;
    i: Integer;
  begin
  // Создаем новый список сохраняемых панелей:
    PAMem := TBinMemoryWriter.Create((Length(panels)+1) * 40);

    i := 0;
    while i < Length(panels) do
    begin
      if panels[i].SaveIt then
      begin
      // ID панели:
        PAMem.WriteInt(i);
      // Сохраняем панель:
        panels[i].SaveState(PAMem);
      end;
      Inc(i);
    end;

  // Сохраняем этот список панелей:
    PAMem.SaveToMemory(Mem);
    PAMem.Free();
  end;

  procedure SaveFlag(flag: PFlag);
  begin
  // Сигнатура флага:
    dw := FLAG_SIGNATURE; // 'FLAG'
    Mem.WriteDWORD(dw);
  // Время перепоявления флага:
    Mem.WriteByte(flag^.RespawnType);
  // Состояние флага:
    Mem.WriteByte(flag^.State);
  // Направление флага:
    if flag^.Direction = D_LEFT then
      b := 1
    else // D_RIGHT
      b := 2;
    Mem.WriteByte(b);
  // Объект флага:
    Obj_SaveState(@flag^.Obj, Mem);
  end;

begin
  Mem := TBinMemoryWriter.Create(1024 * 1024); // 1 MB

///// Сохраняем списки панелей: /////
// Сохраняем панели стен и дверей:
  SavePanelArray(gWalls);
// Сохраняем панели фона:
  SavePanelArray(gRenderBackgrounds);
// Сохраняем панели переднего плана:
  SavePanelArray(gRenderForegrounds);
// Сохраняем панели воды:
  SavePanelArray(gWater);
// Сохраняем панели кислоты-1:
  SavePanelArray(gAcid1);
// Сохраняем панели кислоты-2:
  SavePanelArray(gAcid2);
// Сохраняем панели ступеней:
  SavePanelArray(gSteps);
// Сохраняем панели лифтов:
  SavePanelArray(gLifts);
///// /////

///// Сохраняем музыку: /////
// Сигнатура музыки:
  dw := MUSIC_SIGNATURE; // 'MUSI'
  Mem.WriteDWORD(dw);
// Название музыки:
  Assert(gMusic <> nil, 'g_Map_SaveState: gMusic = nil');
  if gMusic.NoMusic then
    str := ''
  else
    str := gMusic.Name;
  Mem.WriteString(str, 64);
// Позиция проигрывания музыки:
  dw := gMusic.GetPosition();
  Mem.WriteDWORD(dw);
// Стоит ли музыка на спец-паузе:
  boo := gMusic.SpecPause;
  Mem.WriteBoolean(boo);
///// /////

///// Сохраняем количество монстров: /////
  Mem.WriteInt(gTotalMonsters);
///// /////

//// Сохраняем флаги, если это CTF: /////
  if gGameSettings.GameMode = GM_CTF then
  begin
  // Флаг Красной команды:
    SaveFlag(@gFlags[FLAG_RED]);
  // Флаг Синей команды:
    SaveFlag(@gFlags[FLAG_BLUE]);
  end;
///// /////

///// Сохраняем количество побед, если это TDM/CTF: /////
  if gGameSettings.GameMode in [GM_TDM, GM_CTF] then
  begin
  // Очки Красной команды:
    Mem.WriteSmallInt(gTeamStat[TEAM_RED].Goals);
  // Очки Синей команды:
    Mem.WriteSmallInt(gTeamStat[TEAM_BLUE].Goals);
  end;
///// /////
end;

procedure g_Map_LoadState(Var Mem: TBinMemoryReader);
var
  dw: DWORD;
  b: Byte;
  str: String;
  boo: Boolean;

  procedure LoadPanelArray(var panels: TPanelArray);
  var
    PAMem: TBinMemoryReader;
    i, id: Integer;
  begin
  // Загружаем текущий список панелей:
    PAMem := TBinMemoryReader.Create();
    PAMem.LoadFromMemory(Mem);

    for i := 0 to Length(panels)-1 do
    begin
      if panels[i].SaveIt then
      begin
      // ID панели:
        PAMem.ReadInt(id);
        if id <> i then
        begin
          raise EBinSizeError.Create('g_Map_LoadState: LoadPanelArray: Wrong Panel ID');
        end;
      // Загружаем панель:
        panels[i].LoadState(PAMem);
        if (panels[i].arrIdx <> i) then raise Exception.Create('g_Map_LoadState: LoadPanelArray: Wrong Panel arrIdx');
        if (panels[i].proxyId >= 0) then mapGrid.proxyEnabled[panels[i].proxyId] := panels[i].Enabled;
      end;
    end;

  // Этот список панелей загружен:
    PAMem.Free();
  end;

  procedure LoadFlag(flag: PFlag);
  begin
  // Сигнатура флага:
    Mem.ReadDWORD(dw);
    if dw <> FLAG_SIGNATURE then // 'FLAG'
    begin
      raise EBinSizeError.Create('g_Map_LoadState: LoadFlag: Wrong Flag Signature');
    end;
  // Время перепоявления флага:
    Mem.ReadByte(flag^.RespawnType);
  // Состояние флага:
    Mem.ReadByte(flag^.State);
  // Направление флага:
    Mem.ReadByte(b);
    if b = 1 then
      flag^.Direction := D_LEFT
    else // b = 2
      flag^.Direction := D_RIGHT;
  // Объект флага:
    Obj_LoadState(@flag^.Obj, Mem);
  end;

begin
  if Mem = nil then
    Exit;

///// Загружаем списки панелей: /////
// Загружаем панели стен и дверей:
  LoadPanelArray(gWalls);
// Загружаем панели фона:
  LoadPanelArray(gRenderBackgrounds);
// Загружаем панели переднего плана:
  LoadPanelArray(gRenderForegrounds);
// Загружаем панели воды:
  LoadPanelArray(gWater);
// Загружаем панели кислоты-1:
  LoadPanelArray(gAcid1);
// Загружаем панели кислоты-2:
  LoadPanelArray(gAcid2);
// Загружаем панели ступеней:
  LoadPanelArray(gSteps);
// Загружаем панели лифтов:
  LoadPanelArray(gLifts);
///// /////

// Обновляем карту столкновений и сетку:
  g_GFX_Init();
  //mapCreateGrid();

///// Загружаем музыку: /////
// Сигнатура музыки:
  Mem.ReadDWORD(dw);
  if dw <> MUSIC_SIGNATURE then // 'MUSI'
  begin
    raise EBinSizeError.Create('g_Map_LoadState: Wrong Music Signature');
  end;
// Название музыки:
  Assert(gMusic <> nil, 'g_Map_LoadState: gMusic = nil');
  Mem.ReadString(str);
// Позиция проигрывания музыки:
  Mem.ReadDWORD(dw);
// Стоит ли музыка на спец-паузе:
  Mem.ReadBoolean(boo);
// Запускаем эту музыку:
  gMusic.SetByName(str);
  gMusic.SpecPause := boo;
  gMusic.Play();
  gMusic.Pause(True);
  gMusic.SetPosition(dw);
///// /////

///// Загружаем количество монстров: /////
  Mem.ReadInt(gTotalMonsters);
///// /////

//// Загружаем флаги, если это CTF: /////
  if gGameSettings.GameMode = GM_CTF then
  begin
  // Флаг Красной команды:
    LoadFlag(@gFlags[FLAG_RED]);
  // Флаг Синей команды:
    LoadFlag(@gFlags[FLAG_BLUE]);
  end;
///// /////

///// Загружаем количество побед, если это TDM/CTF: /////
  if gGameSettings.GameMode in [GM_TDM, GM_CTF] then
  begin
  // Очки Красной команды:
    Mem.ReadSmallInt(gTeamStat[TEAM_RED].Goals);
  // Очки Синей команды:
    Mem.ReadSmallInt(gTeamStat[TEAM_BLUE].Goals);
  end;
///// /////
end;

function g_Map_PanelForPID(PanelID: Integer; var PanelArrayID: Integer): PPanel;
var
  Arr: TPanelArray;
begin
  Result := nil;
  if (PanelID < 0) or (PanelID > High(PanelByID)) then Exit;
  Arr := PanelByID[PanelID].PWhere^;
  PanelArrayID := PanelByID[PanelID].PArrID;
  Result := Addr(Arr[PanelByID[PanelID].PArrID]);
end;


// trace liquid, stepping by `dx` and `dy`
// return last seen liquid coords, and `false` if we're started outside of the liquid
function g_Map_TraceLiquidNonPrecise (x, y, dx, dy: Integer; out topx, topy: Integer): Boolean;
const
  MaskLiquid = GridTagWater or GridTagAcid1 or GridTagAcid2;
begin
  topx := x;
  topy := y;
  // started outside of the liquid?
  if (mapGrid.forEachAtPoint(x, y, nil, MaskLiquid) = nil) then begin result := false; exit; end;
  if (dx = 0) and (dy = 0) then begin result := false; exit; end; // sanity check
  result := true;
  while true do
  begin
    Inc(x, dx);
    Inc(y, dy);
    if (mapGrid.forEachAtPoint(x, y, nil, MaskLiquid) = nil) then exit; // out of the water, just exit
    topx := x;
    topy := y;
  end;
end;


end.
