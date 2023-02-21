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
{$INCLUDE ../shared/a_modes.inc}
{$M+}
unit g_panel;

interface

uses
  SysUtils, Classes,
  MAPDEF, g_animations, xdynrec;

type
  TLevelTexture = record
    TextureName: AnsiString; // as stored in wad
    FullName: AnsiString; // full path to texture // !!! merge it with TextureName
  end;

  TLevelTextureArray = array of TLevelTexture;

  TAddTextureArray = array of record
    Texture: Cardinal; // Textures[Texture]
  end;

  ATextureID = array of record
    Texture: Cardinal; // Textures[Texture]
  end;

  PPanel = ^TPanel;
  TPanel = Class (TObject)
  private
    const
  private
    mGUID: Integer; // will be assigned in "g_map.pas"
    FAlpha:           Byte;
    FBlending:        Boolean;
    FTextureIDs:      ATextureID;
    FAnimTime:        LongWord;
    FAnimLoop:        Boolean;
    mMovingSpeed: TDFPoint;
    mMovingStart: TDFPoint;
    mMovingEnd: TDFPoint;
    mMovingActive: Boolean;
    mMoveOnce: Boolean;

    mOldMovingActive: Boolean;

    mSizeSpeed: TDFSize;
    mSizeEnd: TDFSize;

    mEndPosTrig: Integer;
    mEndSizeTrig: Integer;

    mNeedSend: Boolean; // for network

  private
    function getx1 (): Integer; inline;
    function gety1 (): Integer; inline;
    function getvisvalid (): Boolean; inline;

    function getMovingSpeedX (): Integer; inline;
    procedure setMovingSpeedX (v: Integer); inline;
    function getMovingSpeedY (): Integer; inline;
    procedure setMovingSpeedY (v: Integer); inline;

    function getMovingStartX (): Integer; inline;
    procedure setMovingStartX (v: Integer); inline;
    function getMovingStartY (): Integer; inline;
    procedure setMovingStartY (v: Integer); inline;

    function getMovingEndX (): Integer; inline;
    procedure setMovingEndX (v: Integer); inline;
    function getMovingEndY (): Integer; inline;
    procedure setMovingEndY (v: Integer); inline;

    function getSizeSpeedX (): Integer; inline;
    procedure setSizeSpeedX (v: Integer); inline;
    function getSizeSpeedY (): Integer; inline;
    procedure setSizeSpeedY (v: Integer); inline;

    function getSizeEndX (): Integer; inline;
    procedure setSizeEndX (v: Integer); inline;
    function getSizeEndY (): Integer; inline;
    procedure setSizeEndY (v: Integer); inline;

  public
    FCurTexture:      Integer; // Номер текущей текстуры
    FX, FY:           Integer;
    FOldX, FOldY:     Integer;
    FWidth, FHeight:  Word;
    FOldW, FOldH:     Word;
    FPanelType:       Word;
    FEnabled:         Boolean;
    FDoor:            Boolean;
    FLiftType:        Byte;
    FLastAnimLoop:    Byte;
    // sorry, there fields are public to allow setting 'em in g_map; this should be fixed later
    // for now, PLEASE, don't modify 'em, or all hell will break loose
    arrIdx:           Integer; // index in one of internal arrays; sorry
    tag:              Integer; // used in coldets and such; sorry; see g_map.GridTagXXX
    proxyId:          Integer; // proxy id in map grid (DO NOT USE!)
    mapId:            AnsiString; // taken directly from map file; dunno why it is here
    hasTexTrigger:    Boolean; // HACK: true when there's a trigger than can change my texture

    constructor Create(PanelRec: TDynRecord;
                       AddTextures: TAddTextureArray;
                       CurTex: Integer;
                       var Textures: TLevelTextureArray; aguid: Integer);
    destructor  Destroy(); override;

    procedure   Update();
    procedure   SetFrame(StartTime: LongWord);
    procedure   NextTexture(AnimLoop: Byte = 0);
    procedure   SetTexture(ID: Integer; AnimLoop: Byte = 0);
    function    GetTextureID(): Cardinal;
    function    GetTextureCount(): Integer;
    function    CanChangeTexture(): Boolean;

    procedure   SaveState (st: TStream);
    procedure   LoadState (st: TStream);

    procedure positionChanged (); inline;

    function getIsGBack (): Boolean; inline; // gRenderBackgrounds
    function getIsGStep (): Boolean; inline; // gSteps
    function getIsGWall (): Boolean; inline; // gWalls
    function getIsGAcid1 (): Boolean; inline; // gAcid1
    function getIsGAcid2 (): Boolean; inline; // gAcid2
    function getIsGWater (): Boolean; inline; // gWater
    function getIsGFore (): Boolean; inline; // gRenderForegrounds
    function getIsGLift (): Boolean; inline; // gLifts
    function getIsGBlockMon (): Boolean; inline; // gBlockMon

    // get-and-clear
    function gncNeedSend (): Boolean; inline;
    procedure setDirty (); inline; // why `dirty`? 'cause i may introduce property `needSend` later

  public
    property visvalid: Boolean read getvisvalid; // panel is "visvalid" when it's width and height are positive

  published
    property guid: Integer read mGUID; // will be assigned in "g_map.pas"
    property x0: Integer read FX;
    property y0: Integer read FY;
    property x1: Integer read getx1; // inclusive!
    property y1: Integer read gety1; // inclusive!
    property x: Integer read FX write FX;
    property y: Integer read FY write FY;
    property width: Word read FWidth write FWidth;
    property height: Word read FHeight write FHeight;
    property oldX: Integer read FOldX;
    property oldY: Integer read FOldY;
    property oldWidth: Word read FOldW;
    property oldHeight: Word read FOldH;
    property oldMovingActive: Boolean read mOldMovingActive write mOldMovingActive;
    property panelType: Word read FPanelType write FPanelType;
    property enabled: Boolean read FEnabled write FEnabled;
    property door: Boolean read FDoor write FDoor;
    property liftType: Byte read FLiftType write FLiftType;
    property lastAnimLoop: Byte read FLastAnimLoop write FLastAnimLoop;

    property movingSpeedX: Integer read getMovingSpeedX write setMovingSpeedX;
    property movingSpeedY: Integer read getMovingSpeedY write setMovingSpeedY;
    property movingStartX: Integer read getMovingStartX write setMovingStartX;
    property movingStartY: Integer read getMovingStartY write setMovingStartY;
    property movingEndX: Integer read getMovingEndX write setMovingEndX;
    property movingEndY: Integer read getMovingEndY write setMovingEndY;
    property movingActive: Boolean read mMovingActive write mMovingActive;
    property moveOnce: Boolean read mMoveOnce write mMoveOnce;

    property sizeSpeedX: Integer read getSizeSpeedX write setSizeSpeedX;
    property sizeSpeedY: Integer read getSizeSpeedY write setSizeSpeedY;
    property sizeEndX: Integer read getSizeEndX write setSizeEndX;
    property sizeEndY: Integer read getSizeEndY write setSizeEndY;

    property isGBack: Boolean read getIsGBack;
    property isGStep: Boolean read getIsGStep;
    property isGWall: Boolean read getIsGWall;
    property isGAcid1: Boolean read getIsGAcid1;
    property isGAcid2: Boolean read getIsGAcid2;
    property isGWater: Boolean read getIsGWater;
    property isGFore: Boolean read getIsGFore;
    property isGLift: Boolean read getIsGLift;
    property isGBlockMon: Boolean read getIsGBlockMon;

    property Alpha: Byte read FAlpha;
    property Blending: Boolean read FBlending;
    property TextureIDs: ATextureID read FTextureIDs;
    property AnimTime: LongWord read FAnimTime;
    property AnimLoop: Boolean read FAnimLoop;

  public
    property movingSpeed: TDFPoint read mMovingSpeed write mMovingSpeed;
    property movingStart: TDFPoint read mMovingStart write mMovingStart;
    property movingEnd: TDFPoint read mMovingEnd write mMovingEnd;

    property sizeSpeed: TDFSize read mSizeSpeed write mSizeSpeed;
    property sizeEnd: TDFSize read mSizeEnd write mSizeEnd;

    property endPosTrigId: Integer read mEndPosTrig write mEndPosTrig;
    property endSizeTrigId: Integer read mEndSizeTrig write mEndSizeTrig;
  end;

  TPanelArray = Array of TPanel;

const
  LIFTTYPE_UP = 0;
  LIFTTYPE_DOWN = 1;
  LIFTTYPE_LEFT = 2;
  LIFTTYPE_RIGHT = 3;

var
  g_dbgpan_mplat_active: Boolean = {$IF DEFINED(D2F_DEBUG)}true{$ELSE}true{$ENDIF};
  g_dbgpan_mplat_step: Boolean = false; // one step, and stop


implementation

  uses
    {$IFDEF ENABLE_GFX}
      g_gfx,
    {$ENDIF}
    {$IFDEF ENABLE_GIBS}
      g_gibs,
    {$ENDIF}
    {$IFDEF ENABLE_CORPSES}
      g_corpses,
    {$ENDIF}
    g_basic, g_map, g_game, g_weapons, g_triggers, g_items,
    g_console, g_language, g_monsters, g_player, g_grid, e_log, geom, utils, xstreams
  ;

const
  PANEL_SIGNATURE = $4C4E4150; // 'PANL'

{ T P a n e l : }

  function GetSpecialTexture (const name: String): Integer;
    (* HACK: get texture id, if not present -> insert it into list *)
    (* required for older maps *)
    var i, len: Integer;
  begin
    i := 0; len := 0;
    if Textures <> nil then
    begin
      len := Length(Textures);
      while (i < len) and (Textures[i].TextureName <> name) do
        Inc(i);
    end;
    if i >= len then
    begin
      i := len;
      SetLength(Textures, len + 1);
      Textures[i].TextureName := name;
    end;
    result := i;
  end;

constructor TPanel.Create(PanelRec: TDynRecord;
                          AddTextures: TAddTextureArray;
                          CurTex: Integer;
                          var Textures: TLevelTextureArray; aguid: Integer);
var
  i: Integer;
  tnum: Integer;
begin
  X := PanelRec.X;
  Y := PanelRec.Y;
  FOldX := X;
  FOldY := Y;
  Width := PanelRec.Width;
  Height := PanelRec.Height;
  FOldW := Width;
  FOldH := Height;
  FAlpha := 0;
  FBlending := False;
  LastAnimLoop := 0;

  mapId := PanelRec.id;
  mGUID := aguid;

  mMovingSpeed := PanelRec.moveSpeed;
  mMovingStart := PanelRec.moveStart;
  mMovingEnd := PanelRec.moveEnd;
  mMovingActive := PanelRec['move_active'].value;
  mOldMovingActive := mMovingActive;
  mMoveOnce := PanelRec.moveOnce;

  mSizeSpeed := PanelRec.sizeSpeed;
  mSizeEnd := PanelRec.sizeEnd;

  mEndPosTrig := PanelRec.endPosTrig;
  mEndSizeTrig := PanelRec.endSizeTrig;

  mNeedSend := false;

// Тип панели:
  PanelType := PanelRec.PanelType;
  Enabled := True;
  Door := False;
  LiftType := LIFTTYPE_UP;
  hasTexTrigger := False;

  case PanelType of
    PANEL_OPENDOOR: begin Enabled := False; Door := True; end;
    PANEL_CLOSEDOOR: Door := True;
    PANEL_LIFTUP: LiftType := LIFTTYPE_UP; //???
    PANEL_LIFTDOWN: LiftType := LIFTTYPE_DOWN;
    PANEL_LIFTLEFT: LiftType := LIFTTYPE_LEFT;
    PANEL_LIFTRIGHT: LiftType := LIFTTYPE_RIGHT;
  end;

// Невидимая:
  if ByteBool(PanelRec.Flags and PANEL_FLAG_HIDE) then
  begin
    SetLength(FTextureIDs, 0);
    FCurTexture := -1;
    Exit;
  end;
// Панели, не использующие текстуры:
  if ByteBool(PanelType and
    (PANEL_LIFTUP or
     PANEL_LIFTDOWN or
     PANEL_LIFTLEFT or
     PANEL_LIFTRIGHT or
     PANEL_BLOCKMON)) then
  begin
    SetLength(FTextureIDs, 0);
    FCurTexture := -1;
    Exit;
  end;

// Если это жидкость без текстуры - спецтекстуру:
  if WordBool(PanelType and (PANEL_WATER or PANEL_ACID1 or PANEL_ACID2)) and
     (not ByteBool(PanelRec.Flags and PANEL_FLAG_WATERTEXTURES)) then
  begin
    SetLength(FTextureIDs, 1);
    case PanelRec.PanelType of
      PANEL_WATER: FTextureIDs[0].Texture := GetSpecialTexture(TEXTURE_NAME_WATER);
      PANEL_ACID1: FTextureIDs[0].Texture := GetSpecialTexture(TEXTURE_NAME_ACID1);
      PANEL_ACID2: FTextureIDs[0].Texture := GetSpecialTexture(TEXTURE_NAME_ACID2);
    end;
    FCurTexture := 0;
    Exit;
  end;

  SetLength(FTextureIDs, Length(AddTextures));

  if CurTex < 0 then
    FCurTexture := -1
  else
    if CurTex >= Length(FTextureIDs) then
      FCurTexture := Length(FTextureIDs) - 1
    else
      FCurTexture := CurTex;

  for i := 0 to Length(FTextureIDs) - 1 do
    FTextureIDs[i].Texture := AddTextures[i].Texture;

  FAnimTime := gTime;
  FAnimLoop := true;

// Текстур несколько - нужно сохранять текущую:
  //if Length(FTextureIDs) > 1 then SaveIt := True;

  if (PanelRec.TextureRec = nil) then tnum := -1 else tnum := PanelRec.tagInt;
  if (tnum < 0) then tnum := Length(Textures);

// Если не спецтекстура, то задаем размеры:
  if ({PanelRec.TextureNum}tnum > High(Textures)) then
  begin
    e_WriteLog(Format('WTF?! tnum is out of limits! (%d : %d)', [tnum, High(Textures)]), TMsgType.Warning);
    FAlpha := 0;
    FBlending := ByteBool(0);
  end
  else if not g_Map_IsSpecialTexture(Textures[{PanelRec.TextureNum}tnum].TextureName) then
  begin
    FAlpha := PanelRec.Alpha;
    FBlending := ByteBool(PanelRec.Flags and PANEL_FLAG_BLENDING);
  end;
end;

destructor TPanel.Destroy();
begin
  SetLength(FTextureIDs, 0);
  Inherited;
end;

function TPanel.getx1 (): Integer; inline; begin result := X+Width-1; end;
function TPanel.gety1 (): Integer; inline; begin result := Y+Height-1; end;
function TPanel.getvisvalid (): Boolean; inline; begin result := (Width > 0) and (Height > 0); end;

function TPanel.getMovingSpeedX (): Integer; inline; begin result := mMovingSpeed.X; end;
procedure TPanel.setMovingSpeedX (v: Integer); inline; begin mMovingSpeed.X := v; end;
function TPanel.getMovingSpeedY (): Integer; inline; begin result := mMovingSpeed.Y; end;
procedure TPanel.setMovingSpeedY (v: Integer); inline; begin mMovingSpeed.Y := v; end;

function TPanel.getMovingStartX (): Integer; inline; begin result := mMovingStart.X; end;
procedure TPanel.setMovingStartX (v: Integer); inline; begin mMovingStart.X := v; end;
function TPanel.getMovingStartY (): Integer; inline; begin result := mMovingStart.Y; end;
procedure TPanel.setMovingStartY (v: Integer); inline; begin mMovingStart.Y := v; end;

function TPanel.getMovingEndX (): Integer; inline; begin result := mMovingEnd.X; end;
procedure TPanel.setMovingEndX (v: Integer); inline; begin mMovingEnd.X := v; end;
function TPanel.getMovingEndY (): Integer; inline; begin result := mMovingEnd.Y; end;
procedure TPanel.setMovingEndY (v: Integer); inline; begin mMovingEnd.Y := v; end;

function TPanel.getSizeSpeedX (): Integer; inline; begin result := mSizeSpeed.w; end;
procedure TPanel.setSizeSpeedX (v: Integer); inline; begin mSizeSpeed.w := v; end;
function TPanel.getSizeSpeedY (): Integer; inline; begin result := mSizeSpeed.h; end;
procedure TPanel.setSizeSpeedY (v: Integer); inline; begin mSizeSpeed.h := v; end;

function TPanel.getSizeEndX (): Integer; inline; begin result := mSizeEnd.w; end;
procedure TPanel.setSizeEndX (v: Integer); inline; begin mSizeEnd.w := v; end;
function TPanel.getSizeEndY (): Integer; inline; begin result := mSizeEnd.h; end;
procedure TPanel.setSizeEndY (v: Integer); inline; begin mSizeEnd.h := v; end;

function TPanel.getIsGBack (): Boolean; inline; begin result := ((tag and GridTagBack) <> 0); end;
function TPanel.getIsGStep (): Boolean; inline; begin result := ((tag and GridTagStep) <> 0); end;
function TPanel.getIsGWall (): Boolean; inline; begin result := ((tag and (GridTagWall or GridTagDoor)) <> 0); end;
function TPanel.getIsGAcid1 (): Boolean; inline; begin result := ((tag and GridTagAcid1) <> 0); end;
function TPanel.getIsGAcid2 (): Boolean; inline; begin result := ((tag and GridTagAcid2) <> 0); end;
function TPanel.getIsGWater (): Boolean; inline; begin result := ((tag and GridTagWater) <> 0); end;
function TPanel.getIsGFore (): Boolean; inline; begin result := ((tag and GridTagFore) <> 0); end;
function TPanel.getIsGLift (): Boolean; inline; begin result := ((tag and GridTagLift) <> 0); end;
function TPanel.getIsGBlockMon (): Boolean; inline; begin result := ((tag and GridTagBlockMon) <> 0); end;

function TPanel.gncNeedSend (): Boolean; inline; begin result := mNeedSend; mNeedSend := false; end;
procedure TPanel.setDirty (); inline; begin mNeedSend := true; end;

procedure TPanel.positionChanged (); inline;
var
  px, py, pw, ph: Integer;
begin
  if (proxyId >= 0) then
  begin
    mapGrid.getBodyDims(proxyId, px, py, pw, ph);
    if (px <> x) or (py <> y) or (pw <> Width) or (ph <> Height) then
    begin
      {
      e_LogWritefln('panel moved: arridx=%s; guid=%s; proxyid=%s; old:(%s,%s)-(%sx%s); new:(%s,%s)-(%sx%s)',
        [arrIdx, mGUID, proxyId, px, py, pw, ph, x, y, width, height]);
      }
      {$IFDEF ENABLE_GFX}
        g_Mark(px, py, pw, ph, MARK_WALL, false);
      {$ENDIF}
      if (Width < 1) or (Height < 1) then
      begin
        mapGrid.proxyEnabled[proxyId] := false;
      end
      else
      begin
        mapGrid.proxyEnabled[proxyId] := Enabled;
        if (pw <> Width) or (ph <> Height) then
        begin
          //writeln('panel resize!');
          mapGrid.moveResizeBody(proxyId, X, Y, Width, Height)
        end
        else
        begin
          mapGrid.moveBody(proxyId, X, Y);
        end;
        {$IFDEF ENABLE_GFX}
          g_Mark(X, Y, Width, Height, MARK_WALL);
        {$ENDIF}
      end;
    end;
  end;
end;


var
  monCheckList: array of TMonster = nil;
  monCheckListUsed: Integer = 0;

procedure TPanel.Update();
var
  ox, oy: Integer;
  nx, ny, nw, nh: Integer;
  ex, ey, nex, ney: Integer;
  mpw, mph: Integer;
  conveyor: Boolean;

  // return `true` if we should move by dx,dy
  function tryMPlatMove (px, py, pw, ph: Integer; out dx, dy: Integer; out squash: Boolean; ontop: PBoolean=nil): Boolean;
  var
    u0: Single;
    tex, tey: Integer;
    pdx, pdy: Integer;
    trtag: Integer;
    szdx, szdy: Integer;
  begin
    squash := false;
    tex := px;
    tey := py;
    pdx := mMovingSpeed.X;
    pdy := mMovingSpeed.Y;
    // standing on the platform?
    if (py+ph = oy) then
    begin
      if (ontop <> nil) then ontop^ := true;
      // yes, move with it; but skip steps (no need to process size change here, 'cause platform top cannot be changed with it)
      mapGrid.traceBox(tex, tey, px, py, pw, ph, pdx, pdy, (GridTagWall or GridTagDoor));
    end
    else
    begin
      if (ontop <> nil) then ontop^ := false;
      // not standing on the platform: trace platform to see if it hits the entity
      // first, process size change (as we cannot sweeptest both move and size change)
      // but we don't have to check for pushing if the panel is shrinking
      szdx := nw-mpw;
      szdy := nh-mph;
      if (szdx > 0) or (szdy > 0) then
      begin
        // ignore shrinking dimension
        if (szdx < 0) then szdx := 0;
        if (szdy < 0) then szdy := 0;
        // move platform by szd* back, and check for szd* movement
        if sweepAABB(ox-szdx, oy-szdy, nw, nh, szdx, szdy, px, py, pw, ph, @u0) then
        begin
          // yes, platform hits the entity, push the entity in the resizing direction
          u0 := 1.0-u0; // how much path left?
          szdx := trunc(szdx*u0);
          szdy := trunc(szdy*u0);
          if (szdx <> 0) or (szdy <> 0) then
          begin
            // has some path to go, trace the entity
            trtag := (GridTagWall or GridTagDoor);
            // if we're moving down, consider steps too
            if (szdy > 0) then trtag := trtag or GridTagStep;
            mapGrid.traceBox(tex, tey, px, py, pw, ph, szdx, szdy, trtag);
          end;
        end;
      end;
      // second, process platform movement, using te* as entity starting point
      if sweepAABB(ox, oy, nw, nh, pdx, pdy, tex, tey, pw, ph, @u0) then
      begin
        //e_LogWritefln('T: platsweep; u0=%s; u1=%s; hedge=%s; sweepAABB(%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)', [u0, u1, hedge, ox, oy, mpw, mph, pdx, pdy, px-1, py-1, pw+2, ph+2]);
        // yes, platform hits the entity, push the entity in the direction of the platform
        u0 := 1.0-u0; // how much path left?
        pdx := trunc(pdx*u0);
        pdy := trunc(pdy*u0);
        //e_LogWritefln(' platsweep; uleft=%s; pd=(%s,%s)', [u0, pdx, pdy]);
        if (pdx <> 0) or (pdy <> 0) then
        begin
          // has some path to go, trace the entity
          trtag := (GridTagWall or GridTagDoor);
          // if we're moving down, consider steps too
          if (pdy > 0) then trtag := trtag or GridTagStep;
          mapGrid.traceBox(tex, tey, px, py, pw, ph, pdx, pdy, trtag);
        end;
      end;
    end;
    // done with entity movement, new coords are in te*
    dx := tex-px;
    dy := tey-py;
    result := (dx <> 0) or (dy <> 0);
    if not conveyor and ((tag and (GridTagWall or GridTagDoor)) <> 0) then
    begin
      // check for squashing; as entity cannot be pushed into a wall, check only collision with the platform itself
      squash := g_Collide(tex, tey, pw, ph, nx, ny, nw, nh); // squash, if still in platform
    end;
  end;

  function monCollect (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    if (monCheckListUsed >= Length(monCheckList)) then SetLength(monCheckList, monCheckListUsed+128);
    monCheckList[monCheckListUsed] := mon;
    Inc(monCheckListUsed);
  end;

var
  cx0, cy0, cx1, cy1, cw, ch: Integer;
  f: Integer;
  px, py, pw, ph, pdx, pdy: Integer;
  squash: Boolean;
  plr: TPlayer;
  {$IFDEF ENABLE_GIBS}
    gib: PGib;
  {$ENDIF}
  {$IFDEF ENABLE_CORPSES}
    cor: TCorpse;
  {$ENDIF}
  ontop: Boolean;
  mon: TMonster;
  flg: PFlag;
  itm: PItem;
  mpfrid: LongWord;
  actMoveTrig: Boolean;
  actSizeTrig: Boolean;
begin
  if (not Enabled) or (Width < 1) or (Height < 1) then exit;

  if not g_dbgpan_mplat_active then exit;

  if (mOldMovingActive <> mMovingActive) then mNeedSend := true;
  mOldMovingActive := mMovingActive;

  if not mMovingActive then exit;
  if mMovingSpeed.isZero and mSizeSpeed.isZero then exit;

  //TODO: write wall size change processing

  // moving platform?
  begin
    (*
     * collect all monsters and players (aka entities) along the possible platform path
     *   if entity is standing on a platform:
     *     try to move it along the platform path, checking wall collisions
     *   if entity is NOT standing on a platform, but hit with sweeped platform aabb:
     *     try to push entity
     *     if we can't push entity all the way, squash it
     *)
    ox := X;
    oy := Y;
    mpw := Width;
    mph := Height;

    // the mplat acts as a stationary conveyor belt when it's locked within a movement rect of zero area
    conveyor := (mMovingEnd.X = mMovingStart.X) and (mMovingEnd.Y = mMovingStart.Y)
      and (mMovingEnd.X = X) and (mMovingEnd.Y = Y);

    nw := mpw+mSizeSpeed.w;
    nh := mph+mSizeSpeed.h;
    nx := ox;
    ny := oy;
    if not conveyor then
    begin
      nx += mMovingSpeed.X;
      ny += mMovingSpeed.Y;
    end;

    // force network updates only if some sudden change happened
    // set the flag here, so we can sync affected monsters
    if not mSizeSpeed.isZero and (nw = mSizeEnd.w) and (nh = mSizeEnd.h) then
    begin
      mNeedSend := true;
    end
    else if ((mMovingSpeed.X < 0) and (nx <= mMovingStart.X)) or ((mMovingSpeed.X > 0) and (nx >= mMovingEnd.X)) then
    begin
      mNeedSend := true;
    end
    else if ((mMovingSpeed.Y < 0) and (ny <= mMovingStart.Y)) or ((mMovingSpeed.Y > 0) and (ny >= mMovingEnd.Y)) then
    begin
      mNeedSend := true;
    end;

    // if pannel disappeared, we don't have to do anything
    if (nw > 0) and (nh > 0) then
    begin
      // old rect
      ex := ox+mpw-1;
      ey := ox+mph-1;
      // new rect
      nex := nx+nw-1;
      ney := ny+nh-1;
      // full rect
      cx0 := nmin(ox, nx);
      cy0 := nmin(oy, ny);
      cx1 := nmax(ex, nex);
      cy1 := nmax(ey, ney);
      // extrude
      cx0 -= 1;
      cy0 -= 1;
      cx1 += 1;
      cy1 += 1;
      cw := cx1-cx0+1;
      ch := cy1-cy0+1;

      // process "obstacle" panels
      if ((tag and GridTagObstacle) <> 0) then
      begin
        // temporarily turn off this panel, so it won't interfere with collision checks
        mapGrid.proxyEnabled[proxyId] := false;

        // process players
        for f := 0 to High(gPlayers) do
        begin
          plr := gPlayers[f];
          if (plr = nil) or (not plr.alive) then continue;
          plr.getMapBox(px, py, pw, ph);
          if not g_Collide(px, py, pw, ph, cx0, cy0, cw, ch) then continue;
          if tryMPlatMove(px, py, pw, ph, pdx, pdy, squash) then
          begin
            // set new position
            plr.moveBy(pdx, pdy); // this will call `positionChanged()` for us
          end;
          // squash player, if necessary
          if not g_Game_IsClient and squash then plr.Damage(15000, 0, 0, 0, HIT_TRAP);
        end;

        {$IFDEF ENABLE_GIBS}
          // process gibs
          for f := 0 to High(gGibs) do
          begin
            gib := @gGibs[f];
            if not gib.alive then continue;
            gib.getMapBox(px, py, pw, ph);
            if not g_Collide(px, py, pw, ph, cx0, cy0, cw, ch) then continue;
            if tryMPlatMove(px, py, pw, ph, pdx, pdy, squash, @ontop) then
            begin
              // set new position
              gib.moveBy(pdx, pdy); // this will call `positionChanged()` for us
            end;
          end;
        {$ENDIF}

        {$IFDEF ENABLE_CORPSES}
          // move and push corpses
          for f := 0 to High(gCorpses) do
          begin
            cor := gCorpses[f];
            if (cor = nil) then continue;
            cor.getMapBox(px, py, pw, ph);
            if not g_Collide(px, py, pw, ph, cx0, cy0, cw, ch) then continue;
            if tryMPlatMove(px, py, pw, ph, pdx, pdy, squash, @ontop) then
            begin
              // set new position
              cor.moveBy(pdx, pdy); // this will call `positionChanged()` for us
            end;
          end;
        {$ENDIF}

        // move and push flags
        if gGameSettings.GameMode = GM_CTF then
          for f := FLAG_RED to FLAG_BLUE do
          begin
            flg := @gFlags[f];
            if (flg.State in [FLAG_STATE_NONE, FLAG_STATE_CAPTURED]) then continue;
            px := flg.Obj.X+flg.Obj.Rect.X;
            py := flg.Obj.Y+flg.Obj.Rect.Y;
            pw := flg.Obj.Rect.Width;
            ph := flg.Obj.Rect.Height;
            if not g_Collide(px, py, pw, ph, cx0, cy0, cw, ch) then continue;
            if tryMPlatMove(px, py, pw, ph, pdx, pdy, squash, @ontop) then
              if (pdx <> 0) or (pdy <> 0) then
              begin
                flg.Obj.X := flg.Obj.X + pdx;
                flg.Obj.Y := flg.Obj.Y + pdy;
                flg.NeedSend := true;
              end;
          end;

        // move and push items
        itm := g_Items_NextAlive(-1);
        while itm <> nil do
        begin
          if itm.Fall then
          begin
            itm.getMapBox(px, py, pw, ph);
            if g_Collide(px, py, pw, ph, cx0, cy0, cw, ch) then
              if tryMPlatMove(px, py, pw, ph, pdx, pdy, squash, @ontop) then
                itm.moveBy(pdx, pdy); // this will call `positionChanged()` for us
          end;
          itm := g_Items_NextAlive(itm.myId);
        end;

        // collect monsters
        monCheckListUsed := 0;
        g_Mons_ForEachAt(cx0, cy0, cw, ch, monCollect);

        // process collected monsters
        if (monCheckListUsed > 0) then
        begin
          mpfrid := g_Mons_getNewMPlatFrameId();
          for f := 0 to monCheckListUsed do
          begin
            mon := monCheckList[f];
            if (mon = nil) or (not mon.alive) or (mon.mplatCheckFrameId = mpfrid) then continue;
            mon.mplatCheckFrameId := mpfrid;
            mon.getMapBox(px, py, pw, ph);
            //if not g_Collide(px, py, pw, ph, cx0, cy0, cw, ch) then continue;
            if tryMPlatMove(px, py, pw, ph, pdx, pdy, squash) then
            begin
              // set new position
              mon.moveBy(pdx, pdy); // this will call `positionChanged()` for us
              //???FIXME: do we really need to send monsters over the net?
              //          i don't think so, as dead reckoning should take care of 'em
              // ok, send new monster position only if platform is going to change it's direction
              if mNeedSend then mon.setDirty();
            end;
            // squash monster, if necessary
            if not g_Game_IsClient and squash then mon.Damage(15000, 0, 0, 0, HIT_TRAP);
          end;
        end;

        // restore panel state
        mapGrid.proxyEnabled[proxyId] := true;
      end;
    end;

    // move panel
    FOldX := X;
    FOldY := Y;
    X := nx;
    Y := ny;
    FOldW := FWidth;
    FOldH := FHeight;
    FWidth := nw;
    FHeight := nh;
    positionChanged();

    actMoveTrig := false;
    actSizeTrig := false;

    // `mNeedSend` was set above

    // check "size stop"
    if not mSizeSpeed.isZero and (nw = mSizeEnd.w) and (nh = mSizeEnd.h) then
    begin
      mSizeSpeed.w := 0;
      mSizeSpeed.h := 0;
      actSizeTrig := true;
      if (nw < 1) or (nh < 1) then mMovingActive := false; //HACK!
    end;

    if not conveyor then
    begin
      // reverse moving direction, if necessary
      if ((mMovingSpeed.X < 0) and (nx <= mMovingStart.X)) or ((mMovingSpeed.X > 0) and (nx >= mMovingEnd.X)) then
      begin
        if mMoveOnce then mMovingActive := false else mMovingSpeed.X := -mMovingSpeed.X;
        actMoveTrig := true;
      end;

      if ((mMovingSpeed.Y < 0) and (ny <= mMovingStart.Y)) or ((mMovingSpeed.Y > 0) and (ny >= mMovingEnd.Y)) then
      begin
        if mMoveOnce then mMovingActive := false else mMovingSpeed.Y := -mMovingSpeed.Y;
        actMoveTrig := true;
      end;

      if (mOldMovingActive <> mMovingActive) then mNeedSend := true;
      mOldMovingActive := mMovingActive;
    end;

    if not g_Game_IsClient then
    begin
      if actMoveTrig then g_Triggers_Press(mEndPosTrig, ACTIVATE_CUSTOM);
      if actSizeTrig then g_Triggers_Press(mEndSizeTrig, ACTIVATE_CUSTOM);
    end;

    // some triggers may activate this, don't delay sending
    //TODO: when triggers will be able to control speed and size, check that here too
    if (mOldMovingActive <> mMovingActive) then mNeedSend := true;
    mOldMovingActive := mMovingActive;
  end;
end;

  procedure TPanel.SetFrame (StartTime: LongWord);
  begin
    if Enabled and (FCurTexture >= 0) and (Width > 0) and (Height > 0) and (FAlpha < 255) then
      FAnimTime := StartTime;
  end;

procedure TPanel.NextTexture(AnimLoop: Byte = 0);
begin
  Assert(FCurTexture >= -1, 'FCurTexture < -1');

// Нет текстур:
  if Length(FTextureIDs) = 0 then
    FCurTexture := -1
  else
  // Только одна текстура:
    if Length(FTextureIDs) = 1 then
      begin
        if FCurTexture = 0 then
          FCurTexture := -1
        else
          FCurTexture := 0;
      end
    else
    // Больше одной текстуры:
      begin
      // Следующая:
        Inc(FCurTexture);
      // Следующей нет - возврат к началу:
        if FCurTexture >= Length(FTextureIDs) then
          FCurTexture := 0;
      end;

  if FCurTexture >= 0 then
  begin
    case AnimLoop of
      1: FAnimLoop := true;
      2: FAnimLoop := false;
    end;
    FAnimTime := gTime;
  end;

  LastAnimLoop := AnimLoop;
end;

procedure TPanel.SetTexture(ID: Integer; AnimLoop: Byte = 0);
begin
  if (ID >= -1) and (ID < Length(FTextureIDs)) then
    FCurTexture := ID;

  if FCurTexture >= 0 then
  begin
    case AnimLoop of
      1: FAnimLoop := true;
      2: FAnimLoop := false;
    end;
    FAnimTime := gTime;
  end;

  LastAnimLoop := AnimLoop;
end;

  function TPanel.GetTextureID(): DWORD;
    var Texture: Integer;
  begin
    Result := LongWord(TEXTURE_NONE);
    if (FCurTexture >= 0) then
    begin
      Texture := FTextureIDs[FCurTexture].Texture;
      if Texture >= 0 then
      begin
        case Textures[Texture].TextureName of (* TODO: optimize it *)
          TEXTURE_NAME_WATER: Result := DWORD(TEXTURE_SPECIAL_WATER);
          TEXTURE_NAME_ACID1: Result := DWORD(TEXTURE_SPECIAL_ACID1);
          TEXTURE_NAME_ACID2: Result := DWORD(TEXTURE_SPECIAL_ACID2);
        end
      end
    end
  end;

function TPanel.GetTextureCount(): Integer;
begin
  Result := Length(FTextureIDs);
  if Enabled and (FCurTexture >= 0) and (Width > 0) and (Height > 0) and (FAlpha < 255) then
    Result := Result + 100; // ???
end;

function TPanel.CanChangeTexture(): Boolean;
begin
  Result := (GetTextureCount() > 1) or hasTexTrigger;
end;

const
  PAN_SAVE_VERSION = 1;

procedure TPanel.SaveState (st: TStream);
  var anim: Boolean; stub: TAnimState;
begin
  if (st = nil) then exit;

  // Сигнатура панели
  utils.writeSign(st, 'PANL');
  utils.writeInt(st, Byte(PAN_SAVE_VERSION));
  // Открыта/закрыта, если дверь
  utils.writeBool(st, FEnabled);
  // Направление лифта, если лифт
  utils.writeInt(st, Byte(FLiftType));
  // Номер текущей текстуры
  utils.writeInt(st, Integer(FCurTexture));
  // Координаты и размер
  utils.writeInt(st, Integer(FX));
  utils.writeInt(st, Integer(FY));
  utils.writeInt(st, Word(FWidth));
  utils.writeInt(st, Word(FHeight));
  // Анимирована ли текущая текстура
  anim := FCurTexture >= 0;
  utils.writeBool(st, anim);
  // Если да - сохраняем анимацию
  if anim then
  begin
    stub := TAnimState.Create(FAnimLoop, 1, 1);
    stub.SaveState(st, FAlpha, FBlending);
    stub.Invalidate;
  end;

  // moving platform state
  utils.writeInt(st, Integer(mMovingSpeed.X));
  utils.writeInt(st, Integer(mMovingSpeed.Y));
  utils.writeInt(st, Integer(mMovingStart.X));
  utils.writeInt(st, Integer(mMovingStart.Y));
  utils.writeInt(st, Integer(mMovingEnd.X));
  utils.writeInt(st, Integer(mMovingEnd.Y));

  utils.writeInt(st, Integer(mSizeSpeed.w));
  utils.writeInt(st, Integer(mSizeSpeed.h));
  utils.writeInt(st, Integer(mSizeEnd.w));
  utils.writeInt(st, Integer(mSizeEnd.h));

  utils.writeBool(st, mMovingActive);
  utils.writeBool(st, mMoveOnce);

  utils.writeInt(st, Integer(mEndPosTrig));
  utils.writeInt(st, Integer(mEndSizeTrig));
end;


procedure TPanel.LoadState (st: TStream);
  var stub: TAnimState;
begin
  if (st = nil) then exit;

  // Сигнатура панели
  if not utils.checkSign(st, 'PANL') then raise XStreamError.create('wrong panel signature');
  if (utils.readByte(st) <> PAN_SAVE_VERSION) then raise XStreamError.create('wrong panel version');
  // Открыта/закрыта, если дверь
  FEnabled := utils.readBool(st);
  // Направление лифта, если лифт
  FLiftType := utils.readByte(st);
  // Номер текущей текстуры
  FCurTexture := utils.readLongInt(st);
  // Координаты и размер
  FX := utils.readLongInt(st);
  FY := utils.readLongInt(st);
  FOldX := FX;
  FOldY := FY;
  FWidth := utils.readWord(st);
  FHeight := utils.readWord(st);
  FOldW := FWidth;
  FOldH := FHeight;
  // Анимированная ли текущая текстура
  if utils.readBool(st) then
  begin
    // Если да - загружаем анимацию
    Assert(FCurTexture >= 0, 'TPanel.LoadState: No animation object');
    stub := TAnimState.Create(FAnimLoop, 1, 1);
    stub.LoadState(st, FAlpha, FBlending);
    stub.Invalidate;
  end;

  // moving platform state
  mMovingSpeed.X := utils.readLongInt(st);
  mMovingSpeed.Y := utils.readLongInt(st);
  mMovingStart.X := utils.readLongInt(st);
  mMovingStart.Y := utils.readLongInt(st);
  mMovingEnd.X := utils.readLongInt(st);
  mMovingEnd.Y := utils.readLongInt(st);

  mSizeSpeed.w := utils.readLongInt(st);
  mSizeSpeed.h := utils.readLongInt(st);
  mSizeEnd.w := utils.readLongInt(st);
  mSizeEnd.h := utils.readLongInt(st);

  mMovingActive := utils.readBool(st);
  mMoveOnce := utils.readBool(st);

  mEndPosTrig := utils.readLongInt(st);
  mEndSizeTrig := utils.readLongInt(st);

  positionChanged();
  //mapGrid.proxyEnabled[proxyId] := FEnabled; // done in g_map.pas
end;


end.
