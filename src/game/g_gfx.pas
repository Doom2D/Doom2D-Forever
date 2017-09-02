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
{$DEFINE D2F_NEW_SPARK_THINKER}
unit g_gfx;

interface

uses
  e_log, g_textures;

const
  BLOOD_NORMAL = 0;
  BLOOD_SPARKS = 1;

  ONCEANIM_NONE  = 0;
  ONCEANIM_SMOKE = 1;

  MARK_FREE     = 0;
  MARK_WALL     = 1;
  MARK_WATER    = 2;
  MARK_ACID     = 4;
  MARK_LIFTDOWN = 8;
  MARK_LIFTUP   = 16;
  MARK_DOOR     = 32;
  MARK_LIFTLEFT  = 64;
  MARK_LIFTRIGHT = 128;
  MARK_BLOCKED  = MARK_WALL or MARK_DOOR;
  MARK_LIQUID   = MARK_WATER or MARK_ACID;
  MARK_LIFT     = MARK_LIFTDOWN or MARK_LIFTUP or MARK_LIFTLEFT or MARK_LIFTRIGHT;


procedure g_GFX_Init ();
procedure g_GFX_Free ();

procedure g_GFX_Blood (fX, fY: Integer; Count: Word; vx, vy: Integer;
                      DevX, DevY: Word; CR, CG, CB: Byte; Kind: Byte = BLOOD_NORMAL);
procedure g_GFX_Spark (fX, fY: Integer; Count: Word; Angle: SmallInt; DevX, DevY: Byte);
procedure g_GFX_Water (fX, fY: Integer; Count: Word; fVelX, fVelY: Single; DevX, DevY, Color: Byte);
procedure g_GFX_SimpleWater (fX, fY: Integer; Count: Word; fVelX, fVelY: Single; DefColor, CR, CG, CB: Byte);
procedure g_GFX_Bubbles (fX, fY: Integer; Count: Word; DevX, DevY: Byte);

procedure g_GFX_SetMax (Count: Integer);
function  g_GFX_GetMax (): Integer;

procedure g_GFX_OnceAnim (X, Y: Integer; Anim: TAnimation; AnimType: Byte = 0);

procedure g_Mark (x, y, Width, Height: Integer; t: Byte; st: Boolean=true);

procedure g_GFX_Update ();
procedure g_GFX_Draw ();


var
  gpart_dbg_enabled: Boolean = true;
  gpart_dbg_phys_enabled: Boolean = true;


implementation

uses
  g_map, g_panel, g_basic, Math, e_graphics, GL, GLExt,
  g_options, g_console, SysUtils, g_triggers, MAPDEF,
  g_game, g_language, g_net, xprofiler;


const
  Unknown = Integer($7fffffff);


type
  TPartType = (Blood, Spark, Bubbles, Water);
  TPartState = (Free, Normal, Stuck, Sleeping);
  TFloorType = (Wall, LiquidIn, LiquidOut);
    // Wall: floorY is just before floor
    // LiquidIn: floorY is liquid *start* (i.e. just in a liquid)
    // LiquidOut: floorY is liquid *end* (i.e. just out of a liquid)

  PParticle = ^TParticle;
  TParticle = record
    x, y: Integer;
    velX, velY: Single;
    accelX, accelY: Single;
    red, green, blue: Byte;
    alpha: Byte;
    time, liveTime: Word;
    state: TPartState;
    particleType: TPartType;
    offsetX, offsetY: ShortInt;
    // for bubbles
    liquidTopY: Integer; // don't float higher than this
    // for water
    stickDX: Integer; // STATE_STICK: -1,1: stuck to a wall; 0: stuck to ceiling
    justSticked: Boolean; // not used
    floorY: Integer; // actually, floor-1; `Unknown`: unknown
    floorType: TFloorType;
    ceilingY: Integer; // actually, ceiling+1; `Unknown`: unknown
    wallEndY: Integer; // if we stuck to a wall, this is where wall ends; will never be > floorY
    // for all
    onGround: Boolean;
    awaken: Boolean;
    stickEY: Integer;

    //k8: sorry, i have to emulate virtual methods this way, 'cause i haet `Object`
    procedure thinkerBlood ();
    procedure thinkerSpark ();
    procedure thinkerBubble ();
    procedure thinkerWater ();

    procedure findFloor (force: Boolean=false); // this updates `floorY` if forced or Unknown
    procedure findCeiling (force: Boolean=false); // this updates `ceilingY` if forced or Unknown

    function isSleeping (): Boolean; inline;
    procedure awake (); inline;

    function alive (): Boolean; inline;
    procedure die (); inline;
    procedure think (); inline;
  end;

  TOnceAnim = record
    AnimType:   Byte;
    x, y:       Integer;
    Animation:  TAnimation;
  end;


var
  Particles: array of TParticle = nil;
  OnceAnims: array of TOnceAnim = nil;
  MaxParticles: Integer = 0;
  CurrentParticle: Integer = 0;
  // awakeMap has one bit for each map grid cell; on g_Mark,
  // corresponding bits will be set, and in `think()` all particles
  // in marked cells will be awaken
  awakeMap: packed array of LongWord = nil;
  awakeMapH: Integer = -1;
  awakeMapW: Integer = -1;
  awakeMinX, awakeMinY: Integer;


// ////////////////////////////////////////////////////////////////////////// //
// HACK! using mapgrid
procedure awmClear (); inline;
begin
  if (awakeMapW > 0) then FillDWord(awakeMap[0], Length(awakeMap), 0);
end;


procedure awmSetup ();
begin
  assert(mapGrid <> nil);
  awakeMapW := (mapGrid.gridWidth+mapGrid.tileSize-1) div mapGrid.tileSize;
  awakeMapW := (awakeMapW+31) div 32; // LongWord has 32 bits ;-)
  awakeMapH := (mapGrid.gridHeight+mapGrid.tileSize-1) div mapGrid.tileSize;
  awakeMinX := mapGrid.gridX0;
  awakeMinY := mapGrid.gridY0;
  SetLength(awakeMap, awakeMapW*awakeMapH);
  {$IF DEFINED(D2F_DEBUG)}
  e_LogWritefln('particle awake map: %sx%s (for grid of size %sx%s)', [awakeMapW, awakeMapH, mapGrid.gridWidth, mapGrid.gridHeight]);
  {$ENDIF}
  awmClear();
end;


function awmIsSet (x, y: Integer): Boolean; inline;
begin
  x := (x-awakeMinX) div mapGrid.tileSize;
  y := (y-awakeMinY) div mapGrid.tileSize;
  if (x >= 0) and (y >= 0) and (x div 32 < awakeMapW) and (y < awakeMapH) then
  begin
    {$IF DEFINED(D2F_DEBUG)}
    assert(y*awakeMapW+x div 32 < Length(awakeMap));
    {$ENDIF}
    result := ((awakeMap[y*awakeMapW+x div 32] and (LongWord(1) shl (x mod 32))) <> 0);
  end
  else
  begin
    result := false;
  end;
end;


procedure awmSet (x, y: Integer); inline;
var
  v: PLongWord;
begin
  x := (x-awakeMinX) div mapGrid.tileSize;
  y := (y-awakeMinY) div mapGrid.tileSize;
  if (x >= 0) and (y >= 0) and (x div 32 < awakeMapW) and (y < awakeMapH) then
  begin
    {$IF DEFINED(D2F_DEBUG)}
    assert(y*awakeMapW+x div 32 < Length(awakeMap));
    {$ENDIF}
    v := @awakeMap[y*awakeMapW+x div 32];
    v^ := v^ or (LongWord(1) shl (x mod 32));
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TParticle.alive (): Boolean; inline; begin result := (state <> TPartState.Free); end;
procedure TParticle.die (); inline; begin state := TPartState.Free; end;

function TParticle.isSleeping (): Boolean; inline;
begin
  result := alive and (onGround or (not justSticked and (state = TPartState.Stuck)));
end;

procedure TParticle.awake (); inline;
begin
  if {alive and} (onGround or (not justSticked and (state = TPartState.Stuck))) then
  begin
    // wakeup this particle
    {
    if (part.ParticleType = PARTICLE_SPARK) then
    begin
      e_LogWritefln('waking up particle of type %s; justSticked=%s; onGround=%s; VelY=%s; AccelY=%s', [part.ParticleType, part.justSticked, part.onGround, part.VelY, part.AccelY]);
    end;
    }
    justSticked := true; // so sticked state will be re-evaluated
    if onGround then
    begin
      if (velY = 0) then velY := 0.1;
      if (accelY = 0) then accelY := 0.5;
    end;
    onGround := false; // so onground state will be re-evaluated
    awaken := true;
  end;
end;


procedure TParticle.findFloor (force: Boolean=false);
var
  ex: Integer;
  pan: TPanel;
begin
  if (not force) and (floorY <> Unknown) then exit;
  // are we in a liquid?
  pan := g_Map_PanelAtPoint(x, y, (GridTagObstacle or GridTagLiquid));
  if (pan <> nil) then
  begin
    // either in a wall, or in a liquid
    if ((pan.tag and GridTagObstacle) <> 0) then
    begin
      // we are in the wall, wtf?!
      floorY := y;
      floorType := TFloorType.Wall;
      state := TPartState.Sleeping; // anyway
      exit;
    end;
    // we are in liquid, trace to liquid end
    floorType := TFloorType.LiquidOut; // exiting liquid
    //e_LogWritefln('tracing out of a liquid; floorY=%s; y=%s', [floorY, y]);
    mapGrid.traceOrthoRayWhileIn(ex, floorY, x, y, x, g_Map_MaxY, GridTagLiquid);
    floorY += 1; // so `floorY` is just out of a liquid
    //e_LogWritefln('  traced out of a liquid; floorY=%s; y=%s', [floorY, y]);
  end
  else
  begin
    // not in a wall
    pan := g_Map_traceToNearest(x, y, x, g_Map_MaxY, (GridTagObstacle or GridTagLiquid), @ex, @floorY);
    if (pan <> nil) then
    begin
      // wall or liquid
      if ((pan.tag and GridTagObstacle) <> 0) then
      begin
        // wall
        floorType := TFloorType.Wall;
      end
      else
      begin
        // liquid
        floorType := TFloorType.LiquidIn; // entering liquid
        floorY += 1; // so `floorY` is just in a liquid
      end;
    end
    else
    begin
      // out of the level; assume wall, but it doesn't really matter
      floorType := TFloorType.Wall;
      floorY := g_Map_MaxY+2;
    end;
  end;
end;


procedure TParticle.findCeiling (force: Boolean=false);
var
  ex: Integer;
begin
  if (not force) and (ceilingY <> Unknown) then exit;
  if (nil = g_Map_traceToNearest(x, y, x, g_Map_MinY, GridTagObstacle, @ex, @ceilingY)) then
  begin
    ceilingY := g_Map_MinY-2;
  end;
end;


procedure TParticle.think (); inline;
begin
  // awake sleeping particle, if necessary
  if (state = TPartState.Sleeping) and awmIsSet(x, y) then state := TPartState.Normal;
  case particleType of
    TPartType.Blood: thinkerBlood();
    //TPartType.Spark: thinkerSpark();
    //TPartType.Bubbles: thinkerBubble();
    //TPartType.Water: thinkerWater();
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function isBlockedAt (x, y: Integer): Boolean; inline;
begin
  if not gpart_dbg_phys_enabled then begin result := false; exit; end;
  result := g_Map_HasAnyPanelAtPoint(x, y, (PANEL_WALL or PANEL_OPENDOOR or PANEL_CLOSEDOOR or PANEL_STEP));
end;

// ???
function isWallAt (x, y: Integer): Boolean; inline;
begin
  if not gpart_dbg_phys_enabled then begin result := false; exit; end;
  result := g_Map_HasAnyPanelAtPoint(x, y, (PANEL_WALL or PANEL_STEP));
end;

function isLiftUpAt (x, y: Integer): Boolean; inline;
begin
  if not gpart_dbg_phys_enabled then begin result := false; exit; end;
  result := g_Map_HasAnyPanelAtPoint(x, y, PANEL_LIFTUP);
end;

function isLiftDownAt (x, y: Integer): Boolean; inline;
begin
  if not gpart_dbg_phys_enabled then begin result := false; exit; end;
  result := g_Map_HasAnyPanelAtPoint(x, y, PANEL_LIFTDOWN);
end;

function isLiftLeftAt (x, y: Integer): Boolean; inline;
begin
  if not gpart_dbg_phys_enabled then begin result := false; exit; end;
  result := g_Map_HasAnyPanelAtPoint(x, y, PANEL_LIFTLEFT);
end;

function isLiftRightAt (x, y: Integer): Boolean; inline;
begin
  if not gpart_dbg_phys_enabled then begin result := false; exit; end;
  result := g_Map_HasAnyPanelAtPoint(x, y, PANEL_LIFTRIGHT);
end;

function isLiquidAt (x, y: Integer): Boolean; inline;
begin
  if not gpart_dbg_phys_enabled then begin result := false; exit; end;
  result := g_Map_HasAnyPanelAtPoint(x, y, (PANEL_WATER or PANEL_ACID1 or PANEL_ACID2));
end;

function isAnythingAt (x, y: Integer): Boolean; inline;
begin
  if not gpart_dbg_phys_enabled then begin result := false; exit; end;
  result := g_Map_HasAnyPanelAtPoint(x, y, (PANEL_WALL or PANEL_CLOSEDOOR or PANEL_OPENDOOR or PANEL_WATER or PANEL_ACID1 or PANEL_ACID2 or PANEL_STEP or PANEL_LIFTUP or PANEL_LIFTDOWN or PANEL_LIFTLEFT or PANEL_LIFTRIGHT));
end;


// ////////////////////////////////////////////////////////////////////////// //
// st: set mark
// t: mark type
// currently unused
procedure g_Mark(x, y, Width, Height: Integer; t: Byte; st: Boolean=true);
var
  cx, ex, ey: Integer;
  ts: Integer;
begin
  if (Width < 1) or (Height < 1) then exit;
  // make some border, so we'll hit particles lying around the panel
  x -= 1; Width += 2;
  y -= 1; Height += 2;
  ex := x+Width;
  ey := y+Height;
  ts := mapGrid.tileSize;
  while (y < ey) do
  begin
    cx := x;
    while (cx < ex) do
    begin
      awmSet(cx, y);
      Inc(cx, ts);
    end;
    Inc(y, ts);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
{$IF DEFINED(HAS_COLLIDE_BITMAP)}
procedure CreateCollideMap();
var
  a: Integer;
begin
  //g_Game_SetLoadingText(_lc[I_LOAD_COLLIDE_MAP]+' 1/6', 0, False);
  //SetLength(gCollideMap, gMapInfo.Height+1);
  //for a := 0 to High(gCollideMap) do SetLength(gCollideMap[a], gMapInfo.Width+1);
end;
{$ENDIF}


procedure g_GFX_Init();
begin
  //CreateCollideMap();
  awmSetup();
{$IFDEF HEADLESS}
  gpart_dbg_enabled := False;
{$ENDIF}
end;


procedure g_GFX_Free();
var
  a: Integer;
begin
  Particles := nil;
  SetLength(Particles, MaxParticles);
  for a := 0 to High(Particles) do Particles[a].die();
  CurrentParticle := 0;

  if (OnceAnims <> nil) then
  begin
    for a := 0 to High(OnceAnims) do OnceAnims[a].Animation.Free();
    OnceAnims := nil;
  end;

  awakeMap := nil;
  // why not?
  awakeMapH := -1;
  awakeMapW := -1;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TParticle.thinkerBlood ();
  procedure stickToCeiling ();
  begin
    state := TPartState.Stuck;
    stickDX := 0;
    // stop right there, you criminal scum!
    velX := 0;
    velY := 0;
    accelX := 0;
    accelY := 0;
    ceilingY := y; // yep
  end;

  procedure stickToWall (dx: Integer);
  var
    ex: Integer;
  begin
    state := TPartState.Stuck;
    if (dX > 0) then stickDX := 1 else stickDX := -1;
    // stop right there, you criminal scum!
    velX := 0;
    velY := 0;
    accelX := 0;
    accelY := 0;
    // find next floor transition
    findFloor();
    // find `wallEndY`
    mapGrid.traceOrthoRayWhileIn(ex, wallEndY, x+stickDX, y, x+stickDX, floorY+1, (GridTagWall or GridTagDoor or GridTagStep));
    //if (wallEndY > floorY) then wallEndY := floorY; // just in case
  end;

  procedure hitAFloor ();
  begin
    state := TPartState.Sleeping; // we aren't moving anymore
    // stop right there, you criminal scum!
    velX := 0;
    velY := 0;
    accelX := 0;
    accelY := 0;
    floorY := y; // yep
    floorType := TFloorType.Wall; // yep
  end;

  // `true`: didn't, get outa thinker
  function drip (): Boolean;
  begin
    result := (Random(200) = 100);
    if result then begin velY := 0.5; accelY := 0.15; end;
  end;

  // `true`: affected by air stream
  function checkAirStreams (): Boolean;
  var
    pan: TPanel;
  begin
    pan := g_Map_PanelAtPoint(x, y, GridTagLift);
    result := (pan <> nil);
    if result then
    begin
      if ((pan.PanelType and PANEL_LIFTUP) <> 0) then
      begin
        if (velY > -4-Random(3)) then velY -= 0.8;
        if (abs(velX) > 0.1) then velX -= velX/10.0;
        velX += (Random-Random)*0.2;
        accelY := 0.15;
      end
      else if ((pan.PanelType and PANEL_LIFTLEFT) <> 0) then
      begin
        if (velX > -8-Random(3)) then velX -= 0.8;
        accelY := 0.15;
      end
      else if ((pan.PanelType and PANEL_LIFTRIGHT) <> 0) then
      begin
        if (velX < 8+Random(3)) then velX += 0.8;
        accelY := 0.15;
      end
      else
      begin
        result := false;
      end;
      // awake
      if result and (state = TPartState.Sleeping) then state := TPartState.Normal;
    end;
  end;

  // switch to sleep mode
  procedure sleep ();
  begin
    state := TPartState.Sleeping;
    // stop right there, you criminal scum!
    velX := 0;
    velY := 0;
    accelX := 0;
    accelY := 0;
  end;

  // switch to freefall mode
  procedure freefall ();
  begin
    state := TPartState.Normal;
    velY := 0.5;
    accelY := 0.15;
  end;

  procedure applyGravity (inLiquid: Boolean);
  begin
    state := TPartState.Normal;
    if (inLiquid) then
    begin
      velY := 0.5;
      accelY := 0.15;
    end
    else
    begin
      velY := 0.8;
      accelY := 0.5;
    end;
  end;

label
  _done;
var
  pan: TPanel;
  dX, dY: SmallInt;
  ex, ey: Integer;
begin
  if gAdvBlood then
  begin
    // still check for air streams when sleeping
    if (state = TPartState.Sleeping) and not checkAirStreams() then exit;

    // process stuck particles
    if (state = TPartState.Stuck) then
    begin
      // stuck to a ceiling?
      if (stickDX = 0) then
      begin
        assert(ceilingY <> Unknown);
        // dropped from a ceiling?
        if (y > ceilingY) then
        begin
          // yep
          velY := 0.5;
          accelY := 0.15;
          state := TPartState.Normal;
        end
        else
        begin
          // otherwise, try to drip
          if drip() then goto _done;
        end;
      end
      else
      begin
        // stuck to a wall
        assert(wallEndY <> Unknown);
        // floor transition?
        if (y = floorY) then
        begin
          case floorType of
            TFloorType.Wall: // hit the ground
              begin
                sleep();
                goto _done; // nothing to do anymore
              end;
            TFloorType.LiquidIn: // entering the liquid
              begin
                // rescan, so we'll know when we'll exit the liquid
                findFloor(true); // force rescan
              end;
            TFloorType.LiquidOut: // exiting the liquid
              begin
                // rescan, so we'll know when we'll enter something interesting
                findFloor(true); // force rescan
                if (floorType = TFloorType.Wall) and (floorY = y) then begin sleep(); goto _done; end;
              end;
          end;
        end;
        // wall transition?
        if (y = wallEndY) then
        begin
          // just unstuck from the wall, switch to freefall mode
          freefall();
        end
        else
        begin
          // otherwise, try to drip
          if drip() then goto _done;
        end;
      end;
      // nope, process as usual
    end;

    // it is important to have it here
    dX := round(velX);
    dY := round(velY);

    // gravity, if not stuck
    if (state <> TPartState.Stuck) and (abs(velX) < 0.1) and (abs(velY) < 0.1) then
    begin
      if (floorY = Unknown) then findFloor();
      // floor transition?
      if (y = floorY) then
      begin
        case floorType of
          TFloorType.Wall: // hit the ground
            begin
              // nothing to do
            end;
          TFloorType.LiquidIn: // entering the liquid
            begin
              // rescan, so we'll know when we'll exit the liquid
              findFloor(true); // force rescan
              applyGravity(true);
            end;
          TFloorType.LiquidOut: // exiting the liquid
            begin
              // rescan, so we'll know when we'll enter something interesting
              findFloor(true); // force rescan
              if (floorType <> TFloorType.Wall) or (floorY <> y) then applyGravity(floorType = TFloorType.LiquidIn);
            end;
        end;
      end
      else
      begin
        // looks like we're in the air
        applyGravity(false);
      end;
    end;

    // horizontal movement
    if (dX <> 0) then
    begin
      pan := g_Map_traceToNearest(x, y, x+dX, y, (GridTagWall or GridTagDoor or GridTagStep), @ex, @ey);
      if (x <> ex) then begin floorY := Unknown; ceilingY := Unknown; end; // dunno yet
      x := ex;
      if (x < g_Map_MinX) or (x > g_Map_MaxX) then begin die(); exit; end;
      if (pan <> nil) then stickToWall(dX); // nope, we stuck
    end;

    // vertical movement
    if (dY < 0) then
    begin
      // flying up
      if (ceilingY = Unknown) then findCeiling(); // need to do this anyway
      y += dY;
      if (y <= ceilingY) then begin y := ceilingY; stickToCeiling(); end; // oops, hit a ceiling
    end
    else
    begin
      while (dY > 0) do
      begin
        // falling down
        if (floorY = Unknown) then findFloor(); // need to do this anyway
        y += dY;
        //e_LogWritefln('floorY=%s; newy=%s; dY=%s; floorType=%s', [floorY, y, dY, floorType]);
        if (y >= floorY) then
        begin
          // floor transition
          dY := y-floorY;
          y := floorY;
          //e_LogWritefln('  HIT FLOORY: floorY=%s; newy=%s; dY=%s; floorType=%s', [floorY, y, dY, floorType]);
          case floorType of
            TFloorType.Wall: // hit the ground
              begin
                hitAFloor();
                break; // done with vertical movement
              end;
            TFloorType.LiquidIn: // entering the liquid
              begin
                // rescan, so we'll know when we'll exit the liquid
                findFloor(true); // force rescan
              end;
            TFloorType.LiquidOut: // exiting the liquid
              begin
                // rescan, so we'll know when we'll enter something interesting
                findFloor(true); // force rescan
                if (floorType = TFloorType.Wall) and (floorY = y) then
                begin
                  hitAFloor();
                  break; // done with vertical movement
                end;
              end;
          end;
        end
        else
        begin
          break; // done with vertical movement
        end;
      end;
    end;
    if (y < g_Map_MinY) or (y > g_Map_MaxY) then begin die(); exit; end;
  end // if gAdvBlood
  else
  begin
    // simple blood
    dX := Round(velX);
    dY := Round(velY);
    if (x+dX > g_Map_MaxX) or (y+dY > g_Map_MaxY) or (x+dX < g_Map_MinX) or (y+dY < g_Map_MinY) or isBlockedAt(x+dX, y+dY) then
    begin
      // Стена/дверь/граница
      die();
      exit;
    end
    else
    begin
      y += dY;
      x += dX;
    end;
  end;

_done:
  velX += accelX;
  velY += accelY;

  // blood will dissolve in other liquids
  if (floorType = TFloorType.LiquidOut) then
  begin
    time += 1;
    alpha := 255-trunc((255.0*time)/liveTime);
  end;
  (*
  // Кровь растворяется в жидкости
  if isLiquidAt(x, y) {ByteBool(gCollideMap[Y, X] and MARK_LIQUID)} then
  begin
    time += 1;
    alpha := 255-trunc((255.0*time)/liveTime);
  end;
  *)
end;


procedure g_GFX_SparkVel (fX, fY: Integer; Count: Word; VX, VY: Integer; DevX, DevY: Byte); forward;

procedure g_GFX_Blood (fX, fY: Integer; Count: Word; vx, vy: Integer;
                       DevX, DevY: Word; CR, CG, CB: Byte; Kind: Byte = BLOOD_NORMAL);
var
  a: Integer;
  DevX1, DevX2, DevY1, DevY2: Word;
  l: Integer;
  CRnd: Byte;
  CC: SmallInt;
begin
  if not gpart_dbg_enabled then Exit;

  if (Kind = BLOOD_SPARKS) then
  begin
    g_GFX_SparkVel(fX, fY, 2+Random(2), -VX div 2, -VY div 2, DevX, DevY);
    exit;
  end;

  l := Length(Particles);
  if (l = 0) then exit;
  if (Count > l) then Count := l;

  DevX1 := DevX div 2;
  DevX2 := DevX+1;
  DevY1 := DevY div 2;
  DevY2 := DevY+1;

  for a := 1 to Count do
  begin
    with Particles[CurrentParticle] do
    begin
      x := fX-DevX1+Random(DevX2);
      y := fY-DevY1+Random(DevY2);

      {
      if (X < 0) or (X > gMapInfo.Width-1) or
         (Y < 0) or (Y > gMapInfo.Height-1) or
         ByteBool(gCollideMap[Y, X] and MARK_WALL) then
        Continue;
      }
      if isWallAt(x, y) then continue;

      velX := vx+(Random-Random)*3;
      velY := vy+(Random-Random)*3;

      if (velY > -4) then
      begin
        if (velY-4 < -4) then velY := -4 else velY := velY-4;
      end;

      accelX := -sign(velX)*Random/100;
      accelY := 0.8;

      CRnd := 20*Random(6);
      if (CR > 0) then
      begin
        CC := CR+CRnd-50;
             if (CC < 0) then CC := 0
        else if (CC > 255) then CC := 255;
        red := CC;
      end
      else
      begin
        red := 0;
      end;
      if CG > 0 then
      begin
        CC := CG+CRnd-50;
             if (CC < 0) then CC := 0
        else if (CC > 255) then CC := 255;
        green := CC;
      end
      else
      begin
        green := 0;
      end;
      if (CB > 0) then
      begin
        CC := CB+CRnd-50;
             if (CC < 0) then CC := 0
        else if (CC > 255) then CC := 255;
        blue := CC;
      end
      else
      begin
        blue := 0;
      end;

      alpha := 255;

      state := TPartState.Normal;
      time := 0;
      liveTime := 120+Random(40);
      particleType := TPartType.Blood;
      //justSticked := false;
      //onGround := (velY >= 0) and g_Map_HasAnyPanelAtPoint(x, y+1, (PANEL_WALL or PANEL_CLOSEDOOR or PANEL_STEP));
      //awaken := false;
      //stickEY := 0;
      floorY := Unknown;
      ceilingY := Unknown;
    end;

    if CurrentParticle >= MaxParticles-1 then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TParticle.thinkerWater ();
var
  dX, dY: SmallInt;
  {$IF not DEFINED(D2F_NEW_SPARK_THINKER)}
  w, h: Integer;
  b: Integer;
  s: ShortInt;
  {$ELSE}
  pan: TPanel;
  ex, ey: Integer;
  {$ENDIF}
begin
  {$IF not DEFINED(D2F_NEW_SPARK_THINKER)}
  w := gMapInfo.Width;
  h := gMapInfo.Height;
  {$ENDIF}

  //TODO: trace wall end when water becomes stick
  if (state = TPartState.Stuck) and (Random(30) = 15) then
  begin // Стекает/отлипает
    velY := 0.5;
    accelY := 0.15;
    {$IF not DEFINED(D2F_NEW_SPARK_THINKER)}
    if (not isBlockedAt(x-1, y) {ByteBool(gCollideMap[Y, X-1] and MARK_BLOCKED)}) and
       (not isBlockedAt(x+1, y) {ByteBool(gCollideMap[Y, X+1] and MARK_BLOCKED)}) then
      state := TPartState.Normal;
    {$ELSE}
    if (stickDX = 0) then
    begin
      // no walls around, drop
      state := TPartState.Normal;
    end
    else
    begin
      if justSticked then
      begin
        if not mapGrid.traceOrthoRayWhileIn(ex, ey, x+stickDX, y, x+stickDX, mapGrid.gridY0+mapGrid.gridHeight, GridTagWall or GridTagDoor or GridTagStep) then
        begin
           // отлипла
          state := TPartState.Normal;
          //e_LogWritefln('juststicked unsticked: X=%s; X+stickDX=%s; stickDX=%s; Y=%s', [X, X+stickDX, stickDX, Y]);
        end
        else
        begin
          stickEY := ey+1;
          justSticked := false;
          if (nil <> g_Map_traceToNearest(x, y, x, stickEY, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey)) then
          begin
            if (ey > stickEY) then stickEY := ey-1;
          end;
          //e_LogWritefln('juststicked: X=%s; X+stickDX=%s; stickDX=%s; Y=%s; stickEY=%s', [X, X+stickDX, stickDX, Y, stickEY]);
        end;
      end
      else
      begin
        if (y >= stickEY) then state := TPartState.Normal;
      end;
      //if not g_Map_CollidePanel(X-1, Y-1, 3, 3, (PANEL_STEP or PANEL_WALL or PANEL_OPENDOOR or PANEL_CLOSEDOOR))
    end;
    {$ENDIF}
    exit;
  end;

  {$IF not DEFINED(D2F_NEW_SPARK_THINKER)}
  if not isBlockedAt(x, y) {ByteBool(gCollideMap[Y, X] and MARK_BLOCKED)} then
  begin
    if isLiftUpAt(x, y) {ByteBool(gCollideMap[Y, X] and MARK_LIFTUP)} then
    begin // Лифт вверх
      if velY > -4-Random(3) then
        velY := velY - 0.8;
      if Abs(velX) > 0.1 then
        velX := velX - velX/10.0;
      velX := velX + (Random-Random)*0.2;
      accelY := 0.15;
    end;
    if isLiftLeftAt(x, y) {ByteBool(gCollideMap[Y, X] and MARK_LIFTLEFT)} then
    begin // Поток влево
      if velX > -8-Random(3) then
        velX := velX - 0.8;
      accelY := 0.15;
    end;
    if isLiftRightAt(x, y) {ByteBool(gCollideMap[Y, X] and MARK_LIFTRIGHT)} then
    begin // Поток вправо
      if velX < 8+Random(3) then
        velX := velX + 0.8;
      accelY := 0.15;
    end;
  end;
  {$ELSE}
  pan := g_Map_PanelAtPoint(x, y, (GridTagAcid1 or GridTagAcid2 or GridTagWater or GridTagLift));
  if (pan <> nil) then
  begin
    if ((pan.tag and (GridTagAcid1 or GridTagAcid2 or GridTagWater)) <> 0) then begin die(); exit; end;
    if ((pan.PanelType and PANEL_LIFTUP) <> 0) then
    begin
      if (velY > -4-Random(3)) then velY -= 0.8;
      if (Abs(velX) > 0.1) then velX -= velX/10.0;
      velX += (Random-Random)*0.2;
      accelY := 0.15;
    end;
    if ((pan.PanelType and PANEL_LIFTLEFT) <> 0) then
    begin
      if (velX > -8-Random(3)) then velX -= 0.8;
      accelY := 0.15;
    end;
    if ((pan.PanelType and PANEL_LIFTRIGHT) <> 0) then
    begin
      if (velX < 8+Random(3)) then velX += 0.8;
      accelY := 0.15;
    end;
  end;
  {$ENDIF}

  dX := Round(velX);
  dY := Round(velY);

  {$IF not DEFINED(D2F_NEW_SPARK_THINKER)}
  if (Abs(velX) < 0.1) and (Abs(velY) < 0.1) then
  begin
    if (state <> TPartState.Stuck) and
       (not isBlockedAt(x, y-1) {ByteBool(gCollideMap[Y-1, X] and MARK_BLOCKED)}) and
       (not isBlockedAt(x, y) {ByteBool(gCollideMap[Y, X] and MARK_BLOCKED)}) and
       (not isBlockedAt(x, y+1) {ByteBool(gCollideMap[Y+1, X] and MARK_BLOCKED)}) then
    begin // Висит в воздухе - капает
      velY := 0.8;
      accelY := 0.5;
      state := TPartState.Normal;
    end;
  end;
  {$ELSE}
  if (state <> TPartState.Stuck) and (Abs(velX) < 0.1) and (Abs(velY) < 0.1) then
  begin
    // Висит в воздухе - капает
    if (nil = g_Map_traceToNearest(x, y-1, x, y+1, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey)) then
    begin
      velY := 0.8;
      accelY := 0.5;
      state := TPartState.Normal;
    end;
  end;
  {$ENDIF}

  {$IF DEFINED(D2F_NEW_SPARK_THINKER)}
  // horizontal
  if (dX <> 0) then
  begin
    pan := g_Map_traceToNearest(x, y, x+dX, y, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey);
    if (x <> ex) then onGround := false;
    x := ex;
    // free to ride?
    if (pan <> nil) then
    begin
      // nope
      if (dY > 0) and ((pan.tag and (GridTagAcid1 or GridTagAcid2 or GridTagWater)) <> 0) then begin die(); exit; end;
      // Стена/дверь?
      if ((pan.tag and (GridTagWall or GridTagDoor or GridTagStep)) <> 0) then
      begin
        velX := 0;
        velY := 0;
        accelX := 0;
        accelY := 0;
        state := TPartState.Stuck;
        justSticked := true;
        if (dX > 0) then stickDX := 1 else stickDX := -1;
      end;
    end;
    if (x < 0) or (x >= gMapInfo.Width) then begin die(); exit; end;
  end;
  // vertical
  if (dY <> 0) then
  begin
    if (dY < 0) or not onGround then
    begin
      pan := g_Map_traceToNearest(x, y, x, y+dY, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey);
      y := ey;
      // free to ride?
      if (pan <> nil) then
      begin
        // nope
        if (dY > 0) and ((pan.tag and (GridTagAcid1 or GridTagAcid2 or GridTagWater)) <> 0) then begin die(); exit; end;
        // Стена/дверь?
        if ((pan.tag and (GridTagWall or GridTagDoor or GridTagStep)) <> 0) then
        begin
          velX := 0;
          velY := 0;
          accelX := 0;
          accelY := 0;
          if (dY > 0) and (state <> TPartState.Stuck) then
          begin
            state := TPartState.Normal;
          end
          else
          begin
            state := TPartState.Stuck;
                 if (g_Map_PanelAtPoint(x-1, y, (GridTagWall or GridTagDoor or GridTagStep)) <> nil) then stickDX := -1
            else if (g_Map_PanelAtPoint(x+1, y, (GridTagWall or GridTagDoor or GridTagStep)) <> nil) then stickDX := 1
            else stickDX := 0;
            justSticked := true;
          end;
        end;
      end;
      onGround := (velY >= 0) and g_Map_HasAnyPanelAtPoint(x, y+1, (PANEL_WALL or PANEL_CLOSEDOOR or PANEL_STEP));
    end;
    if (y < 0) or (y >= gMapInfo.Height) then begin die(); exit; end;
  end;
  {$ELSE}
  // horizontal
  if (dX <> 0) then
  begin
    if (dX > 0) then s := 1 else s := -1;
    for b := 1 to Abs(dX) do
    begin
      // Сбоку граница?
      if (x+s >= w) or (x+s <= 0) then begin die(); break;end;
      //c := gCollideMap[Y, X+s];
      // Сбоку жидкость, а частица уже падает?
      if isLiquidAt(x+s, y) {ByteBool(c and MARK_LIQUID)} and (dY > 0) then begin die(); break; end;
      if isBlockedAt(x+s, y) {ByteBool(c and MARK_BLOCKED)} then
      begin // Стена/дверь
        velX := 0;
        velY := 0;
        accelX := 0;
        accelY := 0;
        state := TPartState.Stuck;
        justSticked := true;
        Break;
      end;
      x := x+s;
    end;
  end;
  // vertical
  if (dY <> 0) then
  begin
    if (dY > 0) then s := 1 else s := -1;
    for b := 1 to Abs(dY) do
    begin
      // Снизу/сверху граница
      if (y+s >= h) or (y+s <= 0) then begin die(); break; end;
      //c := gCollideMap[Y+s, X];
      // Снизу жидкость, а частица уже падает
      if isLiquidAt(x, y+s) {ByteBool(c and MARK_LIQUID)} and (dY > 0) then begin die(); break; end;
      if isBlockedAt(x, y+s) {ByteBool(c and MARK_BLOCKED)} then
      begin // Стена/дверь
        velX := 0;
        velY := 0;
        accelX := 0;
        accelY := 0;
        if (s > 0) and (state <> TPartState.Stuck) then state := TPartState.Normal else state := TPartState.Stuck;
        justSticked := (state = TPartState.Stuck);
        break;
      end;
      y := y+s;
    end;
  end;
  {$ENDIF}

  velX += accelX;
  velY += accelY;

  time += 1;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TParticle.thinkerSpark ();
var
  dX, dY: SmallInt;
  {$IF not DEFINED(D2F_NEW_SPARK_THINKER)}
  b: Integer;
  s: ShortInt;
  {$ELSE}
  pan: TPanel;
  ex, ey: Integer;
  {$ENDIF}
begin
  dX := Round(velX);
  dY := Round(velY);

  {$IF DEFINED(D2F_NEW_SPARK_THINKER)}
  if (Abs(velX) < 0.1) and (Abs(velY) < 0.1) then
  begin
    pan := g_Map_traceToNearest(x, y-1, x, y+1, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey);
  end;
  {$ELSE}
  if (Abs(velX) < 0.1) and (Abs(velY) < 0.1) and
     (not isBlockedAt(x, y-1) {ByteBool(gCollideMap[Y-1, X] and MARK_BLOCKED)}) and
     (not isBlockedAt(x, y) {ByteBool(gCollideMap[Y, X] and MARK_BLOCKED)}) and
     (not isBlockedAt(x, y+1) {ByteBool(gCollideMap[Y+1, X] and MARK_BLOCKED)}) then
  begin // Висит в воздухе
    velY := 0.8;
    accelY := 0.5;
  end;
  {$ENDIF}

  if (dX <> 0) then
  begin
    {$IF DEFINED(D2F_NEW_SPARK_THINKER)}
    pan := g_Map_traceToNearest(x, y, x+dX, y, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey);
    //e_WriteLog(Format('spark h-trace: (%d,%d)-(%d,%d); dx=%d; end=(%d,%d); hit=%d', [X, Y, X+dX, Y, dX, ex, ey, Integer(pan <> nil)]), MSG_NOTIFY);
    if (x <> ex) then onGround := false;
    x := ex;
    // free to ride?
    if (pan <> nil) then
    begin
      // nope
      if ((pan.tag and (GridTagAcid1 or GridTagAcid2 or GridTagWater)) <> 0) then begin die(); exit; end;
      velX := 0;
      accelX := 0;
    end;
    if (x < 0) or (x >= gMapInfo.Width) then begin die(); exit; end;
    {$ELSE}
    if (dX > 0) then s := 1 else s := -1;
    dX := Abs(dX);
    for b := 1 to dX do
    begin
      if (x+s >= gMapInfo.Width) or (x+s <= 0) then begin die(); break; end;
      //c := gCollideMap[Y, X+s];
      if isBlockedAt(x+s, y) {ByteBool(c and MARK_BLOCKED)} then
        begin // Стена/дверь - падает вертикально
          velX := 0;
          accelX := 0;
          Break;
        end
      else // Пусто:
        if not isAnythingAt(x+s, y) {c = MARK_FREE} then
          x := x + s
        else // Остальное:
          begin
            die();
            break;
          end;
    end;
    {$ENDIF}
  end;

  if (dY <> 0) then
  begin
    {$IF DEFINED(D2F_NEW_SPARK_THINKER)}
    if (dY < 0) or not onGround then
    begin
      pan := g_Map_traceToNearest(x, y, x, y+dY, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey);
      y := ey;
      {
      if awaken then
      begin
        awaken := false;
        e_LogWritefln('AWAKEN particle of type %s; justSticked=%s; onGround=%s; VelY=%s; AccelY=%s; Y=%s; ey=%s', [ParticleType, justSticked, onGround, VelY, AccelY, Y, ey]);
      end;
      }
      // free to ride?
      if (pan <> nil) then
      begin
        // nope
        if ((pan.tag and (GridTagAcid1 or GridTagAcid2 or GridTagWater)) <> 0) then begin die(); exit; end;
        if (dY < 0) then
        begin
          velY := -velY;
          accelY := abs(accelY);
        end
        else
        begin
          velX := 0;
          accelX := 0;
          velY := 0;
          accelY := 0.8;
        end;
      end;
      onGround := (velY >= 0) and g_Map_HasAnyPanelAtPoint(x, y+1, (PANEL_WALL or PANEL_CLOSEDOOR or PANEL_STEP));
    end;
    if (y < 0) or (y >= gMapInfo.Height) then begin die(); exit; end;
    {$ELSE}
    if (dY > 0) then s := 1 else s := -1;
    dY := Abs(dY);
    for b := 1 to dY do
    begin
      if (y+s >= gMapInfo.Height) or (y+s <= 0) then begin die(); break; end;
      //c := gCollideMap[Y+s, X];
      if isBlockedAt(x, y+s) {ByteBool(c and MARK_BLOCKED)} then
        begin // Стена/дверь - падает вертикально
          if s < 0 then
            begin
              velY := -velY;
              accelY := Abs(accelY);
            end
          else // Или не падает
            begin
              velX := 0;
              accelX := 0;
              velY := 0;
              accelY := 0.8;
            end;

          Break;
        end
      else // Пусто:
        if not isAnythingAt(x, y+s) {c = MARK_FREE} then
          y := y + s
        else // Осальное:
          begin
            die();
            break;
          end;
    end;
    {$ENDIF}
  end;

  if (velX <> 0.0) then velX += accelX;

  if (velY <> 0.0) then
  begin
    if (accelY < 10) then accelY += 0.08;
    velY += accelY;
  end;

  time += 1;
end;

// ////////////////////////////////////////////////////////////////////////// //
procedure TParticle.thinkerBubble ();
var
  h: Integer;
  dY: SmallInt;
  b: Integer;
  s: ShortInt;
begin
  h := gMapInfo.Height;

  dY := Round(velY);

  if dY <> 0 then
  begin
    if dY > 0 then
      s := 1
    else
      s := -1;

    for b := 1 to Abs(dY) do
    begin
      if (y+s >= h) or (y+s <= 0) then begin die(); break; end;

      (*
      if not isLiquidAt(X, Y+s) {ByteBool(gCollideMap[Y+s, X] and MARK_LIQUID)} then
      begin // Уже не жидкость
        State := STATE_FREE;
        Break;
      end;
      *)
      // we traced liquid before, so don't bother checking
      if (y+s <= liquidTopY) then begin die(); break; end;

      y := y+s;
    end;
  end;

  if velY > -4 then
    velY := velY + accelY;

  time := time + 1;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_GFX_SparkVel (fX, fY: Integer; Count: Word; VX, VY: Integer; DevX, DevY: Byte);
var
  a: Integer;
  DevX1, DevX2,
  DevY1, DevY2: Byte;
  l: Integer;
begin
  exit;
  if not gpart_dbg_enabled then Exit;
  l := Length(Particles);
  if l = 0 then exit;
  if Count > l then Count := l;

  DevX1 := DevX div 2;
  DevX2 := DevX + 1;
  DevY1 := DevY div 2;
  DevY2 := DevY + 1;

  for a := 1 to Count do
  begin
    with Particles[CurrentParticle] do
    begin
      x := fX-DevX1+Random(DevX2);
      y := fY-DevY1+Random(DevY2);

      velX := VX + (Random-Random)*3;
      velY := VY + (Random-Random)*3;

      if velY > -4 then
        if velY-4 < -4 then
          velY := -4
        else
          velY := velY-4;

      accelX := -Sign(velX)*Random/100;
      accelY := 0.8;

      red := 255;
      green := 100+Random(155);
      blue := 64;
      alpha := 255;

      state := TPartState.Normal;
      time := 0;
      liveTime := 30+Random(60);
      particleType := TPartType.Spark;
      justSticked := false;
      onGround := (velY >= 0) and g_Map_HasAnyPanelAtPoint(x, y+1, (PANEL_WALL or PANEL_CLOSEDOOR or PANEL_STEP));
      awaken := false;
    end;

    if CurrentParticle+2 > MaxParticles then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
  end;
end;


procedure g_GFX_Spark(fX, fY: Integer; Count: Word; Angle: SmallInt; DevX, DevY: Byte);
var
  a: Integer;
  b: Single;
  DevX1, DevX2,
  DevY1, DevY2: Byte;
  BaseVelX, BaseVelY: Single;
  l: Integer;
begin
  exit;
  if not gpart_dbg_enabled then Exit;
  l := Length(Particles);
  if l = 0 then
    Exit;
  if Count > l then
    Count := l;

  Angle := 360 - Angle;

  DevX1 := DevX div 2;
  DevX2 := DevX + 1;
  DevY1 := DevY div 2;
  DevY2 := DevY + 1;

  b := DegToRad(Angle);
  BaseVelX := cos(b);
  BaseVelY := 1.6*sin(b);
  if Abs(BaseVelX) < 0.01 then
    BaseVelX := 0.0;
  if Abs(BaseVelY) < 0.01 then
    BaseVelY := 0.0;
  for a := 1 to Count do
  begin
    with Particles[CurrentParticle] do
    begin
      x := fX-DevX1+Random(DevX2);
      y := fY-DevY1+Random(DevY2);

      velX := BaseVelX*Random;
      velY := BaseVelY-Random;
      accelX := velX/3.0;
      accelY := velY/5.0;

      red := 255;
      green := 100+Random(155);
      blue := 64;
      alpha := 255;

      state := TPartState.Normal;
      time := 0;
      liveTime := 30+Random(60);
      particleType := TPartType.Spark;
      justSticked := false;
      onGround := (velY >= 0) and g_Map_HasAnyPanelAtPoint(x, y+1, (PANEL_WALL or PANEL_CLOSEDOOR or PANEL_STEP));
      awaken := false;
    end;

    if CurrentParticle+2 > MaxParticles then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
  end;
end;


procedure g_GFX_Water(fX, fY: Integer; Count: Word; fVelX, fVelY: Single; DevX, DevY, Color: Byte);
var
  a: Integer;
  DevX1, DevX2,
  DevY1, DevY2: Byte;
  l: Integer;
begin
  exit;
  if not gpart_dbg_enabled then Exit;
  l := Length(Particles);
  if l = 0 then
    Exit;
  if Count > l then
    Count := l;

  if Abs(fVelX) < 3.0 then
    fVelX := 3.0 - 6.0*Random;

  DevX1 := DevX div 2;
  DevX2 := DevX + 1;
  DevY1 := DevY div 2;
  DevY2 := DevY + 1;

  for a := 1 to Count do
  begin
    with Particles[CurrentParticle] do
    begin
      x := fX-DevX1+Random(DevX2);
      y := fY-DevY1+Random(DevY2);

      if Abs(fVelX) < 0.5 then
        velX := 1.0 - 2.0*Random
      else
        velX := fVelX*Random;
      if Random(10) < 7 then
        velX := -velX;
      velY := fVelY*Random;
      accelX := 0.0;
      accelY := 0.8;

      case Color of
        1: // Красный
          begin
            red := 155 + Random(9)*10;
            green := Trunc(150*Random);
            blue := green;
          end;
        2: // Зеленый
          begin
            red := Trunc(150*Random);
            green := 175 + Random(9)*10;
            blue := red;
          end;
        3: // Синий
          begin
            red := Trunc(200*Random);
            green := red;
            blue := 175 + Random(9)*10;
          end;
        else // Серый
          begin
            red := 90 + Random(12)*10;
            green := red;
            blue := red;
          end;
      end;

      alpha := 255;

      state := TPartState.Normal;
      time := 0;
      liveTime := 60+Random(60);
      particleType := TPartType.Water;
      justSticked := false;
      onGround := (velY >= 0) and g_Map_HasAnyPanelAtPoint(x, y+1, (PANEL_WALL or PANEL_CLOSEDOOR or PANEL_STEP));
      awaken := false;
    end;

    if CurrentParticle+2 > MaxParticles then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
  end;
end;


procedure g_GFX_SimpleWater(fX, fY: Integer; Count: Word; fVelX, fVelY: Single; DefColor, CR, CG, CB: Byte);
var
  a: Integer;
  l: Integer;
begin
  exit;
  if not gpart_dbg_enabled then Exit;
  l := Length(Particles);
  if l = 0 then
    Exit;
  if Count > l then
    Count := l;

  for a := 1 to Count do
  begin
    with Particles[CurrentParticle] do
    begin
      x := fX;
      y := fY;

      velX := fVelX;
      velY := fVelY;
      accelX := 0.0;
      accelY := 0.8;

      case DefColor of
        1: // Красный
          begin
            red := 155 + Random(9)*10;
            green := Trunc(150*Random);
            blue := green;
          end;
        2: // Зеленый
          begin
            red := Trunc(150*Random);
            green := 175 + Random(9)*10;
            blue := red;
          end;
        3: // Синий
          begin
            red := Trunc(200*Random);
            green := red;
            blue := 175 + Random(9)*10;
          end;
        4: // Свой цвет, светлее
          begin
            red := 20 + Random(19)*10;
            green := red;
            blue := red;
            red := Min(red + CR, 255);
            green := Min(green + CG, 255);
            blue := Min(blue + CB, 255);
          end;
        5: // Свой цвет, темнее
          begin
            red := 20 + Random(19)*10;
            green := red;
            blue := red;
            red := Max(CR - red, 0);
            green := Max(CG - green, 0);
            blue := Max(CB - blue, 0);
          end;
        else // Серый
          begin
            red := 90 + Random(12)*10;
            green := red;
            blue := red;
          end;
      end;

      alpha := 255;

      state := TPartState.Normal;
      time := 0;
      liveTime := 60+Random(60);
      particleType := TPartType.Water;
      justSticked := false;
      onGround := (velY >= 0) and g_Map_HasAnyPanelAtPoint(x, y+1, (PANEL_WALL or PANEL_CLOSEDOOR or PANEL_STEP));
      awaken := false;
    end;

    if CurrentParticle+2 > MaxParticles then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
  end;
end;


{.$DEFINE D2F_DEBUG_BUBBLES}
procedure g_GFX_Bubbles(fX, fY: Integer; Count: Word; DevX, DevY: Byte);
var
  a: Integer;
  DevX1, DevX2,
  DevY1, DevY2: Byte;
  l, liquidx: Integer;
  {$IF DEFINED(D2F_DEBUG_BUBBLES)}
  stt: UInt64;
  nptr, ptr: Boolean;
  {$ENDIF}
begin
  exit;
  if not gpart_dbg_enabled then Exit;
  l := Length(Particles);
  if l = 0 then
    Exit;
  if Count > l then
    Count := l;

  DevX1 := DevX div 2;
  DevX2 := DevX + 1;
  DevY1 := DevY div 2;
  DevY2 := DevY + 1;

  for a := 1 to Count do
  begin
    with Particles[CurrentParticle] do
    begin
      x := fX-DevX1+Random(DevX2);
      y := fY-DevY1+Random(DevY2);

      if (x >= gMapInfo.Width) or (x <= 0) or
         (y >= gMapInfo.Height) or (y <= 0) then
        Continue;

      (*
      // don't spawn bubbles outside of the liquid
      if not isLiquidAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_LIQUID)} then
        Continue;
      *)

      // trace liquid, so we'll know where it ends; do it in 8px steps for speed
      // tracer will return `false` if we started outside of the liquid

      {$IF DEFINED(D2F_DEBUG_BUBBLES)}
      stt := curTimeMicro();
      ptr := mapGrid.traceOrthoRayWhileIn(liquidx, liquidTopY, x, y, x, 0, GridTagWater or GridTagAcid1 or GridTagAcid2);
      stt := curTimeMicro()-stt;
      e_LogWritefln('traceOrthoRayWhileIn: time=%s (%s); liquidTopY=%s', [Integer(stt), ptr, liquidTopY]);
      //
      stt := curTimeMicro();
      nptr := g_Map_TraceLiquidNonPrecise(x, y, 0, -8, liquidx, liquidTopY);
      stt := curTimeMicro()-stt;
      e_LogWritefln('g_Map_TraceLiquidNonPrecise: time=%s (%s); liquidTopY=%s', [Integer(stt), nptr, liquidTopY]);
      if not nptr then continue;
      {$ELSE}
      if not g_Map_TraceLiquidNonPrecise(x, y, 0, -8, liquidx, liquidTopY) then continue;
      {$ENDIF}

      velX := 0;
      velY := -1-Random;
      accelX := 0;
      accelY := velY/10;

      red := 255;
      green := 255;
      blue := 255;
      alpha := 255;

      state := TPartState.Normal;
      time := 0;
      liveTime := 65535;
      particleType := TPartType.Bubbles;
      justSticked := false;
      onGround := (velY >= 0) and g_Map_HasAnyPanelAtPoint(x, y+1, (PANEL_WALL or PANEL_CLOSEDOOR or PANEL_STEP));
      awaken := false;
    end;

    if CurrentParticle+2 > MaxParticles then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_GFX_SetMax(Count: Integer);
var
  a: Integer;
begin
  if Count > 50000 then Count := 50000;
  if (Count < 1) then Count := 1;

  SetLength(Particles, Count);
  for a := 0 to High(Particles) do Particles[a].die();
  MaxParticles := Count;
  //if CurrentParticle >= Count then
    CurrentParticle := 0;
end;


function g_GFX_GetMax(): Integer;
begin
  Result := MaxParticles;
end;


function FindOnceAnim (): DWORD;
var
  i: Integer;
begin
  if OnceAnims <> nil then
    for i := 0 to High(OnceAnims) do
      if OnceAnims[i].Animation = nil then
      begin
        Result := i;
        Exit;
      end;

  if OnceAnims = nil then
    begin
      SetLength(OnceAnims, 16);
      Result := 0;
    end
  else
    begin
      Result := High(OnceAnims) + 1;
      SetLength(OnceAnims, Length(OnceAnims) + 16);
    end;
end;


procedure g_GFX_OnceAnim (x, y: Integer; Anim: TAnimation; AnimType: Byte = 0);
var
  find_id: DWORD;
begin
  if not gpart_dbg_enabled then Exit;
  if Anim = nil then
    Exit;

  find_id := FindOnceAnim();

  OnceAnims[find_id].AnimType := AnimType;
  OnceAnims[find_id].Animation := TAnimation.Create(Anim.FramesID, Anim.Loop, Anim.Speed);
  OnceAnims[find_id].Animation.Blending := Anim.Blending;
  OnceAnims[find_id].Animation.alpha := Anim.alpha;
  OnceAnims[find_id].x := x;
  OnceAnims[find_id].y := y;
end;


procedure g_GFX_Update ();
var
  a: Integer;
  w, h: Integer;
  len: Integer;
begin
  if not gpart_dbg_enabled then exit;

  if (Particles <> nil) then
  begin
    w := gMapInfo.Width;
    h := gMapInfo.Height;

    len := High(Particles);

    for a := 0 to len do
    begin
      if Particles[a].alive then
      begin
        with Particles[a] do
        begin
          if (time = liveTime) then begin die(); continue; end;
          if (x+1 >= w) or (y+1 >= h) or (x <= 0) or (y <= 0) then begin die(); end;
          //if not alive then Continue;
          //e_WriteLog(Format('particle #%d: %d', [State, ParticleType]), MSG_NOTIFY);
          think();
        end; // with
      end; // if
    end; // for
  end; // Particles <> nil

  // clear awake map
  awmClear();

  if OnceAnims <> nil then
  begin
    for a := 0 to High(OnceAnims) do
      if OnceAnims[a].Animation <> nil then
      begin
        case OnceAnims[a].AnimType of
          ONCEANIM_SMOKE:
            begin
              if Random(3) = 0 then
                OnceAnims[a].x := OnceAnims[a].x-1+Random(3);
              if Random(2) = 0 then
                OnceAnims[a].y := OnceAnims[a].y-Random(2);
            end;
        end;

        if OnceAnims[a].Animation.Played then
          begin
            OnceAnims[a].Animation.Free();
            OnceAnims[a].Animation := nil;
          end
        else
          OnceAnims[a].Animation.Update();
      end;
  end;
end;


procedure g_GFX_Draw ();
var
  a, len: Integer;
begin
  if Particles <> nil then
  begin
    glDisable(GL_TEXTURE_2D);
    glPointSize(2);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glBegin(GL_POINTS);

    len := High(Particles);

    for a := 0 to len do
      with Particles[a] do
        if alive and (x >= sX) and (y >= sY) and (x <= sX+sWidth) and (sY <= sY+sHeight) then
        begin
          glColor4ub(red, green, blue, alpha);
          glVertex2i(x + offsetX, y + offsetY);
        end;

    glEnd();

    glDisable(GL_BLEND);
  end;

  if OnceAnims <> nil then
    for a := 0 to High(OnceAnims) do
      if OnceAnims[a].Animation <> nil then
        with OnceAnims[a] do
          Animation.Draw(x, y, M_NONE);
end;


end.
