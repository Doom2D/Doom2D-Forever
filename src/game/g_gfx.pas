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
{.$DEFINE D2F_DEBUG_FALL_MPLAT}
{/$DEFINE D2F_DEBUG_PART_AWAKE}
unit g_gfx;

interface

uses
  e_log, g_textures;

const
  BLOOD_NORMAL = 0;
  BLOOD_SPARKS = 1;
  BLOOD_CSPARKS = 2;
  BLOOD_COMBINE = 3;

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

procedure g_GFX_Blood (fX, fY: Integer; count: Word; vx, vy: Integer;
                       devX, devY: Word; cr, cg, cb: Byte; kind: Byte=BLOOD_NORMAL);
procedure g_GFX_Spark (fX, fY: Integer; count: Word; angle: SmallInt; devX, devY: Byte);
procedure g_GFX_Water (fX, fY: Integer; count: Word; fVelX, fVelY: Single; devX, devY, color: Byte;
                       simple: Boolean=false; cr: Byte=0; cg: Byte=0; cb: Byte=0);
procedure g_GFX_SimpleWater (fX, fY: Integer; count: Word; fVelX, fVelY: Single; defColor, cr, cg, cb: Byte);
procedure g_GFX_Bubbles (fX, fY: Integer; count: Word; devX, devY: Byte);

procedure g_GFX_SetMax (count: Integer);
function  g_GFX_GetMax (): Integer;

procedure g_GFX_OnceAnim (X, Y: Integer; Anim: TAnimation; AnimType: Byte = 0);

procedure g_Mark (x, y, Width, Height: Integer; t: Byte; st: Boolean=true);

procedure g_GFX_Update ();

var
  gpart_dbg_enabled: Boolean = true;
  gpart_dbg_phys_enabled: Boolean = true;


//WARNING: only for Holmes!
function awmIsSetHolmes (x, y: Integer): Boolean; inline;

  type (* private state *)
    TPartType = (Blood, Spark, Bubbles, Water);
    TPartState = (Free, Normal, Stuck, Sleeping);
    TFloorType = (Wall, LiquidIn, LiquidOut);
      // Wall: floorY is just before floor
      // LiquidIn: floorY is liquid *start* (i.e. just in a liquid)
      // LiquidOut: floorY is liquid *end* (i.e. just out of a liquid)
    TEnvType = (EAir, ELiquid, EWall); // where particle is now

    // note: this MUST be record, so we can keep it in
    // dynamic array and has sequential memory access pattern
    PParticle = ^TParticle;
    TParticle = record
      x, y: Integer;
      oldX, oldY: Integer;
      velX, velY: Single;
      accelX, accelY: Single;
      state: TPartState;
      particleType: TPartType;
      red, green, blue: Byte;
      alpha: Byte;
      time, liveTime, waitTime: Word;
      stickDX: Integer; // STATE_STICK: -1,1: stuck to a wall; 0: stuck to ceiling
      justSticked: Boolean; // not used
      floorY: Integer; // actually, floor-1; `Unknown`: unknown
      floorType: TFloorType;
      env: TEnvType; // where particle is now
      ceilingY: Integer; // actually, ceiling+1; `Unknown`: unknown
      wallEndY: Integer; // if we stuck to a wall, this is where wall ends

      //k8: sorry, i have to emulate virtual methods this way, 'cause i haet `Object`
      procedure thinkerBloodAndWater ();
      procedure thinkerSpark ();
      procedure thinkerBubble ();

      procedure findFloor (force: Boolean=false); // this updates `floorY` if forced or Unknown
      procedure findCeiling (force: Boolean=false); // this updates `ceilingY` if forced or Unknown

      procedure freeze (); inline; // remove velocities and acceleration
      procedure sleep (); inline; // switch to sleep mode

      function checkAirStreams (): Boolean; // `true`: affected by air stream

      function alive (): Boolean; inline;
      procedure die (); inline;
      procedure think (); inline;
    end;

    TOnceAnim = record
      AnimType:   Byte;
      x, y:       Integer;
      oldX, oldY: Integer;
      Animation:  TAnimation;
    end;

  var (* private state *)
    Particles: array of TParticle = nil;
    OnceAnims: array of TOnceAnim = nil;

implementation

uses
  g_map, g_panel, g_basic, Math,
  g_options, g_console, SysUtils, g_triggers, MAPDEF,
  g_game, g_language, g_net, utils, xprofiler;


const
  Unknown = Integer($7fffffff);

var
  MaxParticles: Integer = 0;
  CurrentParticle: Integer = 0;
  // awakeMap has one bit for each map grid cell; on g_Mark,
  // corresponding bits will be set, and in `think()` all particles
  // in marked cells will be awaken
  awakeMap: packed array of LongWord = nil;
  awakeMapH: Integer = -1;
  awakeMapW: Integer = -1;
  awakeMinX, awakeMinY: Integer;
  awakeDirty: Boolean = false;
  {$IF DEFINED(D2F_DEBUG_PART_AWAKE)}
  awakeMapHlm: packed array of LongWord = nil;
  {$ENDIF}


// ////////////////////////////////////////////////////////////////////////// //
function awmIsSetHolmes (x, y: Integer): Boolean; inline;
begin
{$IF DEFINED(D2F_DEBUG_PART_AWAKE)}
  if (Length(awakeMapHlm) = 0) then begin result := false; exit; end;
  x := (x-awakeMinX) div mapGrid.tileSize;
  y := (y-awakeMinY) div mapGrid.tileSize;
  if (x >= 0) and (y >= 0) and (x div 32 < awakeMapW) and (y < awakeMapH) then
  begin
    if (y*awakeMapW+x div 32 < Length(awakeMapHlm)) then
    begin
      result := ((awakeMapHlm[y*awakeMapW+x div 32] and (LongWord(1) shl (x mod 32))) <> 0);
    end
    else
    begin
      result := false;
    end;
  end
  else
  begin
    result := false;
  end;
{$ELSE}
  result := false;
{$ENDIF}
end;


// ////////////////////////////////////////////////////////////////////////// //
// HACK! using mapgrid
procedure awmClear (); inline;
begin
  {$IF DEFINED(D2F_DEBUG_PART_AWAKE)}
  if (Length(awakeMap) > 0) then
  begin
    if (Length(awakeMapHlm) <> Length(awakeMap)) then SetLength(awakeMapHlm, Length(awakeMap));
    Move(awakeMap[0], awakeMapHlm[0], Length(awakeMap)*sizeof(awakeMap[0]));
  end;
  {$ENDIF}
  if awakeDirty and (awakeMapW > 0) then
  begin
    FillDWord(awakeMap[0], Length(awakeMap), 0);
    awakeDirty := false;
  end;
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
  {$IF DEFINED(D2F_DEBUG_PART_AWAKE)}
  SetLength(awakeMapHlm, awakeMapW*awakeMapH);
  FillDWord(awakeMapHlm[0], Length(awakeMapHlm), 0);
  {$ENDIF}
  //{$IF DEFINED(D2F_DEBUG)}
  e_LogWritefln('particle awake map: %sx%s (for grid of size %sx%s)', [awakeMapW, awakeMapH, mapGrid.gridWidth, mapGrid.gridHeight]);
  //{$ENDIF}
  awakeDirty := true;
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
    awakeDirty := true;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
// st: set mark
// t: mark type
// currently unused
procedure g_Mark (x, y, Width, Height: Integer; t: Byte; st: Boolean=true);
const Extrude = 1;
var
  dx, dy, ex, ey: Integer;
  v: PLongWord;
begin
  if (not gpart_dbg_enabled) or (not gpart_dbg_phys_enabled) then exit;
  if (awakeMapW < 1) or (awakeMapH < 1) then exit;

  if (Width < 1) or (Height < 1) then exit;

  // make some border, so we'll hit particles around the panel
  ex := x+Width+Extrude-1-awakeMinX;
  ey := y+Height+Extrude-1-awakeMinY;
  x := (x-Extrude)-awakeMinX;
  y := (y-Extrude)-awakeMinY;

  x := x div mapGrid.tileSize;
  y := y div mapGrid.tileSize;
  ex := ex div mapGrid.tileSize;
  ey := ey div mapGrid.tileSize;

  // has something to do?
  if (ex < 0) or (ey < 0) or (x >= awakeMapW*32) or (y >= awakeMapH) then exit;
  if (x < 0) then x := 0;
  if (y < 0) then y := 0;
  if (ex >= awakeMapW*32) then ex := awakeMapW*32-1;
  if (ey >= awakeMapH) then ey := awakeMapH;

  awakeDirty := true;
  for dy := y to ey do
  begin
    for dx := x to ex do
    begin
      {$IF DEFINED(D2F_DEBUG)}
      assert((dx >= 0) and (dy >= 0) and (dx div 32 < awakeMapW) and (dy < awakeMapH));
      assert(dy*awakeMapW+dx div 32 < Length(awakeMap));
      {$ENDIF}
      v := @awakeMap[dy*awakeMapW+dx div 32];
      v^ := v^ or (LongWord(1) shl (dx mod 32));
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TParticle.alive (): Boolean; inline; begin result := (state <> TPartState.Free); end;
procedure TParticle.die (); inline; begin state := TPartState.Free; end;

// remove velocities and acceleration
procedure TParticle.freeze (); inline;
begin
  // stop right there, you criminal scum!
  velX := 0;
  velY := 0;
  accelX := 0;
  accelY := 0;
end;


// `true`: affected by air stream
function TParticle.checkAirStreams (): Boolean;
var
  pan: TPanel;
  r: Integer;
begin
  pan := g_Map_PanelAtPoint(x, y, GridTagLift);
  result := (pan <> nil) and WordBool(pan.PanelType and (PANEL_LIFTUP or PANEL_LIFTDOWN or PANEL_LIFTLEFT or PANEL_LIFTRIGHT));
  r := Random(3);
  if result then
  begin
    case pan.LiftType of
      LIFTTYPE_UP:
      begin
        if (velY > -1-r) then velY -= 0.8;
        if (abs(velX) > 0.1) then velX -= velX/10.0;
        velX += (Random-Random)*0.2;
        accelY := 0.15;
      end;
      LIFTTYPE_DOWN:
      begin
        if (velY < 1+r) then velY += 0.8;
        accelY := 0.15;
      end;
      LIFTTYPE_LEFT:
      begin
        if (velX > -8-r) then velX -= (8+r) div 2;
        accelY := 0.15;
      end;
      LIFTTYPE_RIGHT:
      begin
        if (velX < 8+r) then velX += (8+r) div 2;
        accelY := 0.15;
      end;
      else
        result := false;
    end;
    // awake
    if result and (state = TPartState.Sleeping) then state := TPartState.Normal;
  end;
end;


// switch to sleep mode
procedure TParticle.sleep (); inline;
begin
  if not checkAirStreams() then
  begin
    state := TPartState.Sleeping;
    freeze();
  end;
end;


procedure TParticle.findFloor (force: Boolean=false);
var
  ex: Integer;
  pan: TPanel;
begin
  if (not force) and (floorY <> Unknown) then exit;
  // stuck in the wall? rescan, 'cause it can be mplat
  if (env = TEnvType.EWall) then
  begin
    pan := g_Map_PanelAtPoint(x, y, (GridTagObstacle or GridTagLiquid));
    if (pan <> nil) then
    begin
      // either in a wall, or in a liquid
      if ((pan.tag and GridTagObstacle) <> 0) then
      begin
        // we are in the wall, wtf?!
        floorY := y;
        env := TEnvType.EWall;
        floorType := TFloorType.Wall;
        state := TPartState.Sleeping; // anyway
        exit;
      end;
      // we are in liquid, trace to liquid end
      env := TEnvType.ELiquid;
    end;
  end;
  // are we in a liquid?
  if (env = TEnvType.ELiquid) then
  begin
    // trace out of the liquid
    //env := TEnvType.ELiquid;
    floorType := TFloorType.LiquidOut;
    //e_LogWritefln('tracing out of a liquid; floorY=%s; y=%s', [floorY, y]);
    mapGrid.traceOrthoRayWhileIn(ex, floorY, x, y, x, g_Map_MaxY, GridTagLiquid);
    floorY += 1; // so `floorY` is just out of a liquid
    //e_LogWritefln('  traced out of a liquid; floorY=%s; y=%s', [floorY, y]);
  end
  else
  begin
    // in the air
    assert(env = TEnvType.EAir);
    //env := TEnvType.EAir;
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
  if (nil = g_Map_traceToNearest(x, y, x, g_Map_MinY, GridTagSolid, @ex, @ceilingY)) then
  begin
    ceilingY := g_Map_MinY-2;
  end;
end;


procedure TParticle.think (); inline;
  procedure awake ();
  begin
    if (state = TPartState.Stuck) then
    begin
      //writeln('awaking particle at (', x, ',', y, ')');
      if (stickDX = 0) then
      begin
        state := TPartState.Normal; // stuck to a ceiling
      end
      else
      begin
        // stuck to a wall, check if wall is still there
        if (wallEndY <> Unknown) then
        begin
          wallEndY := Unknown;
          if (g_Map_PanelAtPoint(x+stickDX, y, GridTagObstacle) = nil) then
          begin
            // a wall was moved out, start falling
            state := TPartState.Normal;
            if (velY = 0) then velY := 0.1;
            if (accelY = 0) then accelY := 0.5;
          end;
        end;
      end;
    end
    else
    begin
      state := TPartState.Normal;
      if (velY = 0) then velY := 0.1;
      if (accelY = 0) then accelY := 0.5;
    end;
    floorY := Unknown;
    ceilingY := Unknown;
  end;

begin
  oldx := x;
  oldy := y;
  // awake sleeping particle, if necessary
  if awakeDirty then
  begin
    if awmIsSet(x, y) then awake();
    {
    case state of
      TPartState.Sleeping, TPartState.Stuck:
        if awmIsSet(x, y) then awake();
      else
        if (env = TEnvType.EWall) and awmIsSet(x, y) then awake();
    end;
    }
  end;
  case particleType of
    TPartType.Blood, TPartType.Water: thinkerBloodAndWater();
    TPartType.Spark: thinkerSpark();
    TPartType.Bubbles: thinkerBubble();
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TParticle.thinkerBloodAndWater ();
  procedure stickToCeiling ();
  begin
    state := TPartState.Stuck;
    stickDX := 0;
    freeze();
    ceilingY := y; // yep
  end;

  procedure stickToWall (dx: Integer);
  var
    ex: Integer;
  begin
    state := TPartState.Stuck;
    if (dx > 0) then stickDX := 1 else stickDX := -1;
    freeze();
    // find next floor transition
    findFloor();
    // find `wallEndY`
    mapGrid.traceOrthoRayWhileIn(ex, wallEndY, x+stickDX, y, x+stickDX, floorY+1, (GridTagWall or GridTagDoor or GridTagStep));
  end;

  procedure hitAFloor ();
  begin
    state := TPartState.Sleeping; // we aren't moving anymore
    freeze();
    floorY := y; // yep
    floorType := TFloorType.Wall; // yep
  end;

  // `true`: didn't, get outa thinker
  function drip (): Boolean;
  begin
    case particleType of
      TPartType.Blood: result := (Random(200) = 100);
      TPartType.Water: result := (Random(30) = 15);
      else raise Exception.Create('internal error in particle engine: drip');
    end;
    if result then
    begin
      velY := 0.5;
      accelY := 0.15;
      // if we're falling from ceiling, switch to normal mode
      if (state = TPartState.Stuck) and (stickDX = 0) then state := TPartState.Normal;
    end;
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
    if inLiquid then
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
  _done, _gravityagain, _stuckagain;
var
  pan: TPanel;
  dx, dy: SmallInt;
  ex, ey: Integer;
  checkEnv, inAir, inStep: Boolean;
  floorJustTraced: Boolean;
  {$IF DEFINED(D2F_DEBUG_FALL_MPLAT)}
  oldFloorY: Integer;
  {$ENDIF}
begin
  if not gpart_dbg_phys_enabled then begin x += round(velX); y += round(velY); goto _done; end;

  if gAdvBlood then
  begin
    // still check for air streams when sleeping (no)
    if (state = TPartState.Sleeping) then begin {checkAirStreams();} goto _done; end; // so blood will dissolve

    // process stuck particles
    if (state = TPartState.Stuck) then
    begin
      // stuck to a ceiling?
      if (stickDX = 0) then
      begin
        // yeah, stuck to a ceiling
        if (ceilingY = Unknown) then findCeiling();
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
        if (wallEndY = Unknown) then
        begin
          // this can happen if mplat was moved out; find new `wallEndY`
          findFloor(true); // force trace, just in case
          if (floorType = TFloorType.LiquidOut) then env := TEnvType.ELiquid else env := TEnvType.EAir;
          mapGrid.traceOrthoRayWhileIn(ex, wallEndY, x+stickDX, y, x+stickDX, floorY+1, (GridTagWall or GridTagDoor or GridTagStep));
        end;
       _stuckagain:
        // floor transition?
        if (wallEndY <= floorY) and (y >= floorY) then
        begin
          y := floorY;
          case floorType of
            TFloorType.Wall: // hit the ground
              begin
                // check if our ground wasn't moved since the last scan
                findFloor(true); // force trace
                if (y = floorY) then
                begin
                  sleep();
                  goto _done; // nothing to do anymore
                end;
                // otherwise, do it again
                goto _stuckagain;
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
        if (floorY <= wallEndY) and (y >= wallEndY) then
        begin
          // just unstuck from the wall, switch to freefall mode
          y := wallEndY;
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
    dx := round(velX);
    dy := round(velY);

    inAir := checkAirStreams();

    // gravity, if not stuck
    if (state <> TPartState.Stuck) and (abs(velX) < 0.1) and (abs(velY) < 0.1) then
    begin
      floorJustTraced := (floorY = Unknown);
      if floorJustTraced then findFloor();
     _gravityagain:
      // floor transition?
      if (y = floorY) then
      begin
        case floorType of
          TFloorType.Wall: // hit the ground
            begin
              // check if our ground wasn't moved since the last scan
              if not floorJustTraced then
              begin
                findFloor(true); // force trace
                if (floorType = TFloorType.LiquidOut) then env := TEnvType.ELiquid else env := TEnvType.EAir;
                if (y <> floorY) then goto _gravityagain;
              end;
              // otherwise, nothing to do
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

    // trace movement
    if (dx <> 0) then
    begin
      // has some horizontal velocity
      inStep := False;
      pan := g_Map_traceToNearest(x, y, x+dx, y+dy, GridTagSolid, @ex, @ey);
      if (pan = nil) and (dy >= 0) then
      begin
        // do not stuck inside step
        if g_Map_traceToNearest(x, y, x, y, GridTagStep, nil, nil) = nil then
          // check for step panel below
          pan := g_Map_traceToNearest(x, y, x, y+dy, GridTagStep, nil, @ey);
        inStep := pan <> nil;
        if inStep then
        begin
          // stick to panel edges
          if ex < pan.X then
            ex := pan.X
          else if ex > pan.X + pan.Width - 1 then
            ex := pan.X + pan.Width - 1;
        end;
      end;
      checkEnv := (x <> ex);
      x := ex;
      y := ey;
      if checkEnv then
      begin
        // dunno yet
        floorY := Unknown;
        ceilingY := Unknown;
        // check environment (air/liquid)
        if (g_Map_PanelAtPoint(x, y, GridTagLiquid) <> nil) then env := TEnvType.ELiquid else env := TEnvType.EAir;
      end;
      if (pan <> nil) then
      begin
        if inStep then
          stickToWall(dx)
        else
        begin
          // we stuck
          // the only case when we can have both ceiling and wall is corner; stick to wall in this case
          // check if we stuck to a wall
          if (dx < 0) then dx := -1 else dx := 1;
          if (g_Map_PanelAtPoint(x+dx, y, GridTagSolid) <> nil) then
          begin
            // stuck to a wall
            stickToWall(dx);
          end
          else
          begin
            // stuck to a ceiling
            stickToCeiling();
          end;
        end;
      end;
    end
    else if (dy <> 0) then
    begin
      // has only vertical velocity
      if (dy < 0) then
      begin
        // flying up
        if (ceilingY = Unknown) then findCeiling(); // need to do this anyway
        y += dy;
        if (y <= ceilingY) then begin y := ceilingY; stickToCeiling(); end; // oops, hit a ceiling
        // environment didn't changed
      end
      else
      begin
        while (dy > 0) do
        begin
          // falling down
          floorJustTraced := (floorY = Unknown);
          if floorJustTraced then findFloor();
          if (floorType = TFloorType.LiquidOut) then env := TEnvType.ELiquid else env := TEnvType.EAir;
          y += dy;
          //e_LogWritefln('floorY=%s; newy=%s; dY=%s; floorType=%s', [floorY, y, dY, floorType]);
          if (y >= floorY) then
          begin
            // floor transition
            dy := y-floorY;
            y := floorY;
            //e_LogWritefln('  HIT FLOORY: floorY=%s; newy=%s; dY=%s; floorType=%s', [floorY, y, dY, floorType]);
            case floorType of
              TFloorType.Wall: // hit the ground
                begin
                  // check if our ground wasn't moved since the last scan
                  if not floorJustTraced then
                  begin
                    {$IF DEFINED(D2F_DEBUG_FALL_MPLAT)}
                    oldFloorY := floorY;
                    {$ENDIF}
                    findFloor(true); // force trace
                    {$IF DEFINED(D2F_DEBUG_FALL_MPLAT)}
                    if (floorY <> oldFloorY) then
                    begin
                      e_LogWritefln('force rescanning vpart at (%s,%s); oldFloorY=%s; floorY=%s', [x, y, oldFloorY, floorY]);
                    end;
                    {$ENDIF}
                    if (floorType = TFloorType.LiquidOut) then env := TEnvType.ELiquid else env := TEnvType.EAir;
                    if (y <> floorY) then continue;
                  end;
                  // environment didn't changed
                  if not inAir then hitAFloor();
                  break; // done with vertical movement
                end;
              TFloorType.LiquidIn: // entering the liquid
                begin
                  // we're entered the liquid
                  env := TEnvType.ELiquid;
                  // rescan, so we'll know when we'll exit the liquid
                  findFloor(true); // force rescan
                end;
              TFloorType.LiquidOut: // exiting the liquid
                begin
                  // we're exited the liquid
                  env := TEnvType.EAir;
                  // rescan, so we'll know when we'll enter something interesting
                  findFloor(true); // force rescan
                  if (floorType = TFloorType.Wall) and (floorY = y) then
                  begin
                    if not inAir then hitAFloor();
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
    end;
  end // if gAdvBlood
  else
  begin
    // simple blood
    dx := round(velX);
    dy := round(velY);
    y += dy;
    x += dx;
    if (g_Map_PanelAtPoint(x, y, GridTagObstacle) <> nil) then begin die(); exit; end;
  end;

_done:
  if (x < g_Map_MinX) or (y < g_Map_MinY) or (x > g_Map_MaxX) or (y > g_Map_MaxY) then begin die(); end;

  velX += accelX;
  velY += accelY;

  // blood will dissolve in other liquids
  if (particleType = TPartType.Blood) then
  begin
    if (env = TEnvType.ELiquid) then
    begin
      if waitTime > 0 then
        waitTime -= 1
      else
        time += 1;
      if (liveTime <= 0) then begin die(); exit; end;
      ex := 255-trunc(255.0*time/liveTime);
      if (ex <= 10) then begin die(); exit; end;
      if (ex > 250) then ex := 255;
      alpha := Byte(ex);
    end;
  end
  else
  begin
    // water will disappear in any liquid
    if (env = TEnvType.ELiquid) then begin die(); exit; end;
    if waitTime > 0 then
      waitTime -= 1
    else
      time += 1;
    // dry water
    if (liveTime <= 0) then begin die(); exit; end;
    ex := 255-trunc(255.0*time/liveTime);
    if (ex <= 10) then begin die(); exit; end;
    if (ex > 250) then ex := 255;
    alpha := Byte(ex);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_GFX_SparkVel (fX, fY: Integer; count: Word; vx, vy: Integer; devX, devY: Byte); forward;

procedure g_GFX_Blood (fX, fY: Integer; count: Word; vx, vy: Integer;
                       devX, devY: Word; cr, cg, cb: Byte; kind: Byte = BLOOD_NORMAL);

  function genColor (cbase, crnd: Integer; def: Byte=0): Byte;
  begin
    if (cbase > 0) then
    begin
      cbase += crnd;
           if (cbase < 0) then result := 0
      else if (cbase > 255) then result := 255
      else result := Byte(cbase);
    end
    else
    begin
      result := def;
    end;
  end;

var
  a: Integer;
  devX1, devX2, devY1, devY2: Integer;
  l: Integer;
  crnd: Integer;
  pan: TPanel;
begin
  if not gpart_dbg_enabled then exit;

  if (kind = BLOOD_SPARKS) then
  begin
    g_GFX_SparkVel(fX, fY, 2+Random(2), -vx div 2, -vy div 2, devX, devY);
    exit;
  end
  else if (kind = BLOOD_CSPARKS) OR (kind = BLOOD_COMBINE) then
  begin
    g_GFX_SparkVel(fX, fY, count, -vx div 2, -vy div 2, devX, devY);
    if kind <> BLOOD_COMBINE then exit
  end;

  l := Length(Particles);
  if (l = 0) then exit;
  if (count > l) then count := l;

  devX1 := devX div 2;
  devX2 := devX+1;
  devY1 := devY div 2;
  devY2 := devY+1;

  for a := 1 to count do
  begin
    with Particles[CurrentParticle] do
    begin
      x := fX-devX1+Random(devX2);
      y := fY-devY1+Random(devY2);
      oldx := x;
      oldy := y;

      // check for level bounds
      if (x < g_Map_MinX) or (y < g_Map_MinY) or (x > g_Map_MaxX) or (y > g_Map_MaxY) then continue;

      // in what environment we are starting in?
      pan := g_Map_PanelAtPoint(x, y, (GridTagSolid or GridTagLiquid));
      if (pan <> nil) then
      begin
        // either in a wall, or in a liquid
        if ((pan.tag and GridTagSolid) <> 0) then continue; // don't spawn in walls
        env := TEnvType.ELiquid;
      end
      else
      begin
        env := TEnvType.EAir;
      end;

      velX := vx+(Random-Random)*3;
      velY := vy+(Random-Random)*3;

      if (velY > -4) then
      begin
        if (velY-4 < -4) then velY := -4 else velY := velY-4;
      end;

      accelX := -sign(velX)*Random/100;
      accelY := 0.8;

      crnd := 20*Random(6)-50;

      red := genColor(cr, CRnd, 0);
      green := genColor(cg, CRnd, 0);
      blue := genColor(cb, CRnd, 0);
      alpha := 255;

      particleType := TPartType.Blood;
      state := TPartState.Normal;
      time := 0;
      liveTime := 120+Random(40);
      waitTime := 20;
      floorY := Unknown;
      ceilingY := Unknown;
    end;

    if (CurrentParticle >= MaxParticles-1) then CurrentParticle := 0 else CurrentParticle += 1;
  end;
end;


procedure g_GFX_Water (fX, fY: Integer; count: Word; fVelX, fVelY: Single; devX, devY, color: Byte;
                       simple: Boolean=false; cr: Byte=0; cg: Byte=0; cb: Byte=0);
var
  a: Integer;
  devX1, devX2, devY1, devY2: Integer;
  l: Integer;
  pan: TPanel;
begin
  if not gpart_dbg_enabled then exit;

  l := Length(Particles);
  if (l = 0) then exit;
  if (count > l) then count := l;

  if (abs(fVelX) < 3.0) then fVelX := 3.0-6.0*Random;

  devX1 := devX div 2;
  devX2 := devX+1;
  devY1 := devY div 2;
  devY2 := devY+1;

  if (not simple) and (color > 3) then color := 0;

  for a := 1 to count do
  begin
    with Particles[CurrentParticle] do
    begin
      if not simple then
      begin
        x := fX-devX1+Random(devX2);
        y := fY-devY1+Random(devY2);

        if (abs(fVelX) < 0.5) then velX := 1.0-2.0*Random else velX := fVelX*Random;
        if (Random(10) < 7) then velX := -velX;
        velY := fVelY*Random;
        accelX := 0.0;
        accelY := 0.8;
      end
      else
      begin
        x := fX;
        y := fY;

        velX := fVelX;
        velY := fVelY;
        accelX := 0.0;
        accelY := 0.8;
      end;

      oldx := x;
      oldy := y;

      // check for level bounds
      if (x < g_Map_MinX) or (y < g_Map_MinY) or (x > g_Map_MaxX) or (y > g_Map_MaxY) then continue;

      // this hack will allow water spawned in water to fly out
      // it can happen when player fell from a huge height (see "DOOM2D.WAD:\MAP03", for example)
      if (fVelY >= 0) then
      begin
        // in what environment we are starting in?
        pan := g_Map_PanelAtPoint(x, y, (GridTagObstacle or GridTagLiquid));
      end
      else
      begin
        pan := g_Map_PanelAtPoint(x, y, GridTagObstacle);
      end;
      if (pan <> nil) then continue;
      env := TEnvType.EAir;

      // color
      case color of
        1: // reddish
          begin
            red := 155+Random(9)*10;
            green := trunc(150*Random);
            blue := green;
          end;
        2: // greenish
          begin
            red := trunc(150*Random);
            green := 175+Random(9)*10;
            blue := red;
          end;
        3: // bluish
          begin
            red := trunc(200*Random);
            green := red;
            blue := 175+Random(9)*10;
          end;
        4: // Свой цвет, светлее
          begin
            red := 20+Random(19)*10;
            green := red;
            blue := red;
            red := nmin(red+cr, 255);
            green := nmin(green+cg, 255);
            blue := nmin(blue+cb, 255);
          end;
        5: // Свой цвет, темнее
          begin
            red := 20+Random(19)*10;
            green := red;
            blue := red;
            red := nmax(cr-red, 0);
            green := nmax(cg-green, 0);
            blue := nmax(cb-blue, 0);
          end;
        else // grayish
          begin
            red := 90+random(12)*10;
            green := red;
            blue := red;
          end;
      end;
      alpha := 255;

      particleType := TPartType.Water;
      state := TPartState.Normal;
      time := 0;
      liveTime := 60+Random(60);
      waitTime := 120;
      floorY := Unknown;
      ceilingY := Unknown;
    end;

    if (CurrentParticle >= MaxParticles-1) then CurrentParticle := 0 else CurrentParticle += 1;
  end;
end;


procedure g_GFX_SimpleWater (fX, fY: Integer; count: Word; fVelX, fVelY: Single; defColor, cr, cg, cb: Byte);
begin
  g_GFX_Water(fX, fY, count, 0, 0, 0, 0, defColor, true, cr, cg, cb);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TParticle.thinkerBubble ();
var
  dy: Integer;
begin
  dy := round(velY);

  if (dy <> 0) then
  begin
    y += dy;
    if (dy < 0) then
    begin
      if (y <= ceilingY) then begin die(); exit; end;
    end
    else
    begin
      if (y >= floorY) then begin die(); exit; end;
    end;
    if (y < g_Map_MinY) or (y > g_Map_MaxY) then begin die(); exit; end;
  end;

  if (velY > -4) then velY += accelY;

  if waitTime > 0 then
    waitTime -= 1
  else
    time += 1;
end;


{.$DEFINE D2F_DEBUG_BUBBLES}
procedure g_GFX_Bubbles (fX, fY: Integer; count: Word; devX, devY: Byte);
var
  a, liquidx: Integer;
  devX1, devX2, devY1, devY2: Integer;
  l: Integer;
  {$IF DEFINED(D2F_DEBUG_BUBBLES)}
  stt: UInt64;
  nptr, ptr: Boolean;
  {$ENDIF}
begin
  if not gpart_dbg_enabled then exit;

  l := Length(Particles);
  if (l = 0) then exit;
  if (count > l) then count := l;

  devX1 := devX div 2;
  devX2 := devX+1;
  devY1 := devY div 2;
  devY2 := devY+1;

  for a := 1 to count do
  begin
    with Particles[CurrentParticle] do
    begin
      x := fX-devX1+Random(devX2);
      y := fY-devY1+Random(devY2);
      oldx := x;
      oldy := y;

      // check for level bounds
      if (x < g_Map_MinX) or (y < g_Map_MinY) or (x > g_Map_MaxX) or (y > g_Map_MaxY) then continue;

      (*
      // don't spawn bubbles outside of the liquid
      if not isLiquidAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_LIQUID)} then
        Continue;
      *)

      // trace liquid, so we'll know where it ends; do it in 8px steps for speed
      // tracer will return `false` if we started outside of the liquid

      {$IF DEFINED(D2F_DEBUG_BUBBLES)}
      stt := getTimeMicro();
      ptr := mapGrid.traceOrthoRayWhileIn(liquidx, liquidTopY, x, y, x, 0, GridTagWater or GridTagAcid1 or GridTagAcid2);
      stt := getTimeMicro()-stt;
      e_LogWritefln('traceOrthoRayWhileIn: time=%s (%s); liquidTopY=%s', [Integer(stt), ptr, liquidTopY]);
      //
      stt := getTimeMicro();
      nptr := g_Map_TraceLiquidNonPrecise(x, y, 0, -8, liquidx, liquidTopY);
      stt := getTimeMicro()-stt;
      e_LogWritefln('g_Map_TraceLiquidNonPrecise: time=%s (%s); liquidTopY=%s', [Integer(stt), nptr, liquidTopY]);
      if not nptr then continue;
      {$ELSE}
      if not g_Map_TraceLiquidNonPrecise(x, y, 0, -8, liquidx, ceilingY) then continue;
      if not g_Map_TraceLiquidNonPrecise(x, y, 0, +8, liquidx, floorY) then continue;
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
      particleType := TPartType.Bubbles;
      time := 0;
      liveTime := 65535;
      waitTime := 0;
    end;

    if (CurrentParticle >= MaxParticles-1) then CurrentParticle := 0 else CurrentParticle += 1;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TParticle.thinkerSpark ();
label
  _done;
var
  dx, dy: SmallInt;
  pan: TPanel;
  ex, ey: Integer;
begin
  if not gpart_dbg_phys_enabled then begin x += round(velX); y += round(velY); goto _done; end;

  dx := round(velX);
  dy := round(velY);

  //writeln('spark0: pos=(', x, ',', y, '); delta=(', dx, ',', dy, '); state=', state, '; ceilingY=', ceilingY, '; floorY=', floorY);

  // apply gravity
  if (abs(velX) < 0.1) and (abs(velY) < 0.1) then
  begin
    velY := 0.8;
    accelY := 0.5;
  end;

  // flying
  if (dx <> 0) then
  begin
    // has some horizontal velocity
    pan := g_Map_traceToNearest(x, y, x+dx, y+dy, (GridTagSolid or GridTagLiquid), @ex, @ey);
    if (x <> ex) then begin floorY := Unknown; ceilingY := Unknown; end; // dunno yet
    x := ex;
    y := ey;
    if (pan <> nil) then
    begin
      if ((pan.tag and GridTagLiquid) <> 0) then begin die(); exit; end; // die in liquid
      // hit the wall; falling down vertically
      velX := 0;
      accelX := 0;
    end;
  end
  else if (dy <> 0) then
  begin
    // has some vertical velocity
    if (dy < 0) then
    begin
      // flying up
      if (ceilingY = Unknown) then findCeiling(); // need to do this anyway
      y += dy;
      if (y <= ceilingY) then
      begin
        // oops, hit a ceiling
        y := ceilingY;
        velY := -velY;
        accelY := abs(accelY);
      end;
      // environment didn't changed
    end
    else
    begin
      // falling down
      if (floorY = Unknown) then findFloor(); // need to do this anyway
      y += dy;
      if (y >= floorY) then
      begin
        // hit something except a floor?
        if (floorType <> TFloorType.Wall) then begin die(); exit; end; // yep: just die
        // otherwise, go to sleep
        y := floorY;
        sleep();
        // environment didn't changed
      end;
    end;
  end;

_done:
  if (x < g_Map_MinX) or (y < g_Map_MinY) or (x > g_Map_MaxX) or (y > g_Map_MaxY) then begin die(); end;

  if (velX <> 0.0) then velX += accelX;

  if (velY <> 0.0) then
  begin
    if (accelY < 10) then accelY += 0.08;
    velY += accelY;
  end;

  //writeln('spark1: pos=(', x, ',', y, '); delta=(', velX:6:3, ',', velY:6:3, '); state=', state, '; ceilingY=', ceilingY, '; floorY=', floorY);

  if waitTime > 0 then
    waitTime -= 1
  else
    time += 1;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_GFX_SparkVel (fX, fY: Integer; count: Word; vx, vy: Integer; devX, devY: Byte);
var
  a: Integer;
  devX1, devX2, devY1, devY2: Integer;
  l: Integer;
  pan: TPanel;
begin
  if not gpart_dbg_enabled then exit;

  l := Length(Particles);
  if (l = 0) then exit;
  if (count > l) then count := l;

  devX1 := devX div 2;
  devX2 := devX+1;
  devY1 := devY div 2;
  devY2 := devY+1;

  for a := 1 to count do
  begin
    with Particles[CurrentParticle] do
    begin
      x := fX-devX1+Random(devX2);
      y := fY-devY1+Random(devY2);
      oldx := x;
      oldy := y;

      // check for level bounds
      if (x < g_Map_MinX) or (y < g_Map_MinY) or (x > g_Map_MaxX) or (y > g_Map_MaxY) then continue;

      // in what environment we are starting in?
      pan := g_Map_PanelAtPoint(x, y, (GridTagSolid or GridTagLiquid));
      if (pan <> nil) then
      begin
        // either in a wall, or in a liquid
        //if ((pan.tag and GridTagSolid) <> 0) then continue; // don't spawn in walls
        //env := TEnvType.ELiquid;
        continue;
      end
      else
      begin
        env := TEnvType.EAir;
      end;

      velX := vx+(Random-Random)*3;
      velY := vy+(Random-Random)*3;

      if (velY > -4) then
      begin
        if (velY-4 < -4) then velY := -4 else velY := velY-4;
      end;

      accelX := -sign(velX)*Random/100;
      accelY := 0.8;

      red := 255;
      green := 100+Random(155);
      blue := 64;
      alpha := 255;

      particleType := TPartType.Spark;
      state := TPartState.Normal;
      time := 0;
      liveTime := 30+Random(60);
      waitTime := 0;
      floorY := Unknown;
      ceilingY := Unknown;
    end;

    if (CurrentParticle >= MaxParticles-1) then CurrentParticle := 0 else CurrentParticle += 1;
  end;
end;


procedure g_GFX_Spark (fX, fY: Integer; count: Word; angle: SmallInt; devX, devY: Byte);
var
  a: Integer;
  b: Single;
  devX1, devX2, devY1, devY2: Integer;
  baseVelX, baseVelY: Single;
  l: Integer;
  pan: TPanel;
begin
  if not gpart_dbg_enabled then exit;

  l := Length(Particles);
  if (l = 0) then exit;
  if (count > l) then count := l;

  angle := 360-angle;

  devX1 := devX div 2;
  devX2 := devX+1;
  devY1 := devY div 2;
  devY2 := devY+1;

  b := DegToRad(angle);
  baseVelX := cos(b);
  baseVelY := 1.6*sin(b);
  if (abs(baseVelX) < 0.01) then baseVelX := 0.0;
  if (abs(baseVelY) < 0.01) then baseVelY := 0.0;

  for a := 1 to count do
  begin
    with Particles[CurrentParticle] do
    begin
      x := fX-devX1+Random(devX2);
      y := fY-devY1+Random(devY2);
      oldx := x;
      oldy := y;

      // check for level bounds
      if (x < g_Map_MinX) or (y < g_Map_MinY) or (x > g_Map_MaxX) or (y > g_Map_MaxY) then continue;

      // in what environment we are starting in?
      pan := g_Map_PanelAtPoint(x, y, (GridTagSolid or GridTagLiquid));
      if (pan <> nil) then
      begin
        // either in a wall, or in a liquid
        //if ((pan.tag and GridTagSolid) <> 0) then continue; // don't spawn in walls
        //env := TEnvType.ELiquid;
        continue;
      end
      else
      begin
        env := TEnvType.EAir;
      end;

      velX := baseVelX*Random;
      velY := baseVelY-Random;
      accelX := velX/3.0;
      accelY := velY/5.0;

      red := 255;
      green := 100+Random(155);
      blue := 64;
      alpha := 255;

      particleType := TPartType.Spark;
      state := TPartState.Normal;
      time := 0;
      liveTime := 30+Random(60);
      waitTime := 0;
      floorY := Unknown;
      ceilingY := Unknown;
    end;

    if (CurrentParticle >= MaxParticles-1) then CurrentParticle := 0 else CurrentParticle += 1;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_GFX_SetMax (count: Integer);
var
  a: Integer;
begin
  if count > 50000 then count := 50000;
  if (count < 1) then count := 1;
  SetLength(Particles, count);
  for a := 0 to High(Particles) do Particles[a].die();
  MaxParticles := count;
  CurrentParticle := 0;
end;


function g_GFX_GetMax (): Integer;
begin
  result := MaxParticles;
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
  if not gpart_dbg_enabled then exit;

  if (Anim = nil) then exit;

  find_id := FindOnceAnim();

  OnceAnims[find_id].AnimType := AnimType;
  OnceAnims[find_id].Animation := TAnimation.Create(Anim.FramesID, Anim.Loop, Anim.Speed);
  OnceAnims[find_id].Animation.Blending := Anim.Blending;
  OnceAnims[find_id].Animation.alpha := Anim.alpha;
  OnceAnims[find_id].x := x;
  OnceAnims[find_id].y := y;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_GFX_Init ();
begin
  //g_Game_SetLoadingText(_lc[I_LOAD_COLLIDE_MAP]+' 1/6', 0, False);
  //SetLength(gCollideMap, gMapInfo.Height+1);
  //for a := 0 to High(gCollideMap) do SetLength(gCollideMap[a], gMapInfo.Width+1);
  awmSetup();
{$IFDEF HEADLESS}
  gpart_dbg_enabled := false;
{$ENDIF}
end;


procedure g_GFX_Free ();
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
        OnceAnims[a].oldx := OnceAnims[a].x;
        OnceAnims[a].oldy := OnceAnims[a].y;

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

end.
