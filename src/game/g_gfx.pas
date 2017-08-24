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
  MARK_BLOCKED  = MARK_WALL + MARK_DOOR;
  MARK_LIQUID   = MARK_WATER + MARK_ACID;
  MARK_LIFT     = MARK_LIFTDOWN + MARK_LIFTUP + MARK_LIFTLEFT + MARK_LIFTRIGHT;

procedure g_GFX_Init();
procedure g_GFX_Free();

procedure g_GFX_Blood(fX, fY: Integer; Count: Word; vx, vy: Integer;
                      DevX, DevY: Word; CR, CG, CB: Byte; Kind: Byte = BLOOD_NORMAL);
procedure g_GFX_Spark(fX, fY: Integer; Count: Word; Angle: SmallInt; DevX, DevY: Byte);
procedure g_GFX_Water(fX, fY: Integer; Count: Word; fVelX, fVelY: Single; DevX, DevY, Color: Byte);
procedure g_GFX_SimpleWater(fX, fY: Integer; Count: Word; fVelX, fVelY: Single; DefColor, CR, CG, CB: Byte);
procedure g_GFX_Bubbles(fX, fY: Integer; Count: Word; DevX, DevY: Byte);
procedure g_GFX_SetMax(Count: Integer);
function  g_GFX_GetMax(): Integer;

procedure g_GFX_OnceAnim(X, Y: Integer; Anim: TAnimation; AnimType: Byte = 0);

procedure g_Mark(x, y, Width, Height: Integer; t: Byte; st: Boolean);

procedure g_GFX_Update();
procedure g_GFX_Draw();


var
  gpart_dbg_enabled: Boolean = true;
  gpart_dbg_phys_enabled: Boolean = true;


implementation

uses
  g_map, g_panel, g_basic, Math, e_graphics, GL, GLExt,
  g_options, g_console, SysUtils, g_triggers, MAPDEF,
  g_game, g_language, g_net;

type
  PParticle = ^TParticle;

  TParticle = record
    X, Y:               Integer;
    VelX, VelY:         Single;
    AccelX, AccelY:     Single;
    Red, Green, Blue:   Byte;
    Alpha:              Byte;
    Time, LiveTime:     Word;
    State:              Byte;
    ParticleType:       Byte;
    offsetX, offsetY:   ShortInt;
    // for bubbles
    liquidTopY: Integer; // don't float higher than this
    // for water
    stickDX: Integer;

    //k8: sorry, i have to emulate virtual methods this way, 'cause i haet `Object`

    procedure thinkerBlood ();
    procedure thinkerSpark ();
    procedure thinkerBubble ();
    procedure thinkerWater ();

    function alive (): Boolean; inline;
    procedure die (); inline;
    procedure think (); inline;
  end;

  TOnceAnim = record
    AnimType:   Byte;
    X, Y:       Integer;
    Animation:  TAnimation;
  end;

const
  PARTICLE_BLOOD   = 0;
  PARTICLE_SPARK   = 1;
  PARTICLE_BUBBLES = 2;
  PARTICLE_WATER   = 3;
  STATE_FREE   = 0;
  STATE_NORMAL = 1;
  STATE_STICK  = 2;

var
  Particles: array of TParticle;
  OnceAnims: array of TOnceAnim;
  MaxParticles: Integer;
  CurrentParticle: Integer;


// ////////////////////////////////////////////////////////////////////////// //
function TParticle.alive (): Boolean; inline; begin result := (State <> STATE_FREE); end;
procedure TParticle.die (); inline; begin State := STATE_FREE; end;

procedure TParticle.think (); inline;
begin
  case ParticleType of
    PARTICLE_BLOOD: thinkerBlood();
    PARTICLE_SPARK: thinkerSpark();
    PARTICLE_BUBBLES: thinkerBubble();
    PARTICLE_WATER: thinkerWater();
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function isBlockedAt (x, y: Integer): Boolean; inline;
begin
  if not gpart_dbg_phys_enabled then begin result := false; exit; end;
  result := g_Map_HasAnyPanelAtPoint(x, y, (PANEL_WALL or PANEL_CLOSEDOOR or PANEL_STEP));
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


procedure g_Mark(x, y, Width, Height: Integer; t: Byte; st: Boolean);
{$IF not DEFINED(HAS_COLLIDE_BITMAP)}
begin
end;
{$ELSE}
var
  yy, y2, xx, x2: Integer;
begin
  if x < 0 then
  begin
    Width := Width + x;
    x := 0;
  end;

  if Width < 0 then
    Exit;

  if y < 0 then
  begin
    Height := Height + y;
    y := 0;
  end;

  if Height < 0 then
    Exit;

  if x > gMapInfo.Width then
    Exit;
  if y > gMapInfo.Height then
    Exit;

  y2 := y + Height - 1;
  if y2 > gMapInfo.Height then
    y2 := gMapInfo.Height;

  x2 := x + Width - 1;
  if x2 > gMapInfo.Width then
    x2 := gMapInfo.Width;

  if st then
    begin // Установить признак
      for yy := y to y2 do
        for xx := x to x2 do
          gCollideMap[yy][xx] := gCollideMap[yy][xx] or t;
    end
  else
    begin // Убрать признак
      t := not t;
      for yy := y to y2 do
        for xx := x to x2 do
          gCollideMap[yy][xx] := gCollideMap[yy][xx] and t;
    end;
end;
{$ENDIF}


{$IF DEFINED(HAS_COLLIDE_BITMAP)}
procedure CreateCollideMap();
var
  a: Integer;
begin
  g_Game_SetLoadingText(_lc[I_LOAD_COLLIDE_MAP]+' 1/6', 0, False);
  SetLength(gCollideMap, gMapInfo.Height+1);
  for a := 0 to High(gCollideMap) do
    SetLength(gCollideMap[a], gMapInfo.Width+1);

  if gWater <> nil then
  begin
    g_Game_SetLoadingText(_lc[I_LOAD_COLLIDE_MAP]+' 2/6', 0, True);
    for a := 0 to High(gWater) do
      with gWater[a] do
        g_Mark(X, Y, Width, Height, MARK_WATER, True);
  end;

  if gAcid1 <> nil then
  begin
    g_Game_SetLoadingText(_lc[I_LOAD_COLLIDE_MAP]+' 3/6', 0, True);
    for a := 0 to High(gAcid1) do
      with gAcid1[a] do
        g_Mark(X, Y, Width, Height, MARK_ACID, True);
  end;

  if gAcid2 <> nil then
  begin
    g_Game_SetLoadingText(_lc[I_LOAD_COLLIDE_MAP]+' 4/6', 0, True);
    for a := 0 to High(gAcid2) do
      with gAcid2[a] do
        g_Mark(X, Y, Width, Height, MARK_ACID, True);
  end;

  if gLifts <> nil then
  begin
    g_Game_SetLoadingText(_lc[I_LOAD_COLLIDE_MAP]+' 5/6', 0, True);
    for a := 0 to High(gLifts) do
      with gLifts[a] do
      begin
        g_Mark(X, Y, Width, Height, MARK_LIFT, False);

        if LiftType = 0 then
          g_Mark(X, Y, Width, Height, MARK_LIFTUP, True)
        else if LiftType = 1 then
          g_Mark(X, Y, Width, Height, MARK_LIFTDOWN, True)
        else if LiftType = 2 then
          g_Mark(X, Y, Width, Height, MARK_LIFTLEFT, True)
        else if LiftType = 3 then
          g_Mark(X, Y, Width, Height, MARK_LIFTRIGHT, True)
      end;
  end;

  if gWalls <> nil then
  begin
    g_Game_SetLoadingText(_lc[I_LOAD_COLLIDE_MAP]+' 6/6', 0, True);
    for a := 0 to High(gWalls) do
    begin
      if gWalls[a].Door then
        begin
        // Закрытая дверь:
          if gWalls[a].Enabled then
            with gWalls[a] do
              g_Mark(X, Y, Width, Height, MARK_DOOR, True)
          else // Открытая дверь:
            if gWalls[a].Enabled then
              with gWalls[a] do
                g_Mark(X, Y, Width, Height, MARK_DOOR, False);
        end
      else // Стена
        with gWalls[a] do
          g_Mark(X, Y, Width, Height, MARK_WALL, True);
    end;
  end;
end;
{$ENDIF}


procedure g_GFX_Init();
begin
  //CreateCollideMap();
end;


procedure g_GFX_Free();
var
  a: Integer;
begin
  Particles := nil;
  SetLength(Particles, MaxParticles);
  for a := 0 to High(Particles) do Particles[a].die();
  CurrentParticle := 0;

  if OnceAnims <> nil then
  begin
    for a := 0 to High(OnceAnims) do
      OnceAnims[a].Animation.Free();

    OnceAnims := nil;
  end;
end;


{
procedure CorrectOffsets(id: Integer); inline;
var
  part: PParticle;
begin
  part := @Particles[id];
  part.offsetX := 0;
  part.offsetY := 0;
  // check for upper wall
  if isBlockedAt(part.X, part.Y-1) then part.offsetY := 1;
  // check for left wall
  if isBlockedAt(part.X-1, part.Y) then part.offsetX := 1;
end;
}


// ////////////////////////////////////////////////////////////////////////// //
procedure TParticle.thinkerBlood ();
var
  w, h: Integer;
  dX, dY: SmallInt;
  {$IF not DEFINED(D2F_NEW_SPARK_THINKER)}
  b: Integer;
  s: ShortInt;
  {$ELSE}
  pan: TPanel;
  ex, ey: Integer;
  {$ENDIF}
begin
  w := gMapInfo.Width;
  h := gMapInfo.Height;

  if gAdvBlood then
  begin
    if (State = STATE_STICK) then
    begin
      {$IF not DEFINED(D2F_NEW_SPARK_THINKER)}
      {
      if (not ByteBool(gCollideMap[Y-1, X] and MARK_BLOCKED)) and
         (not ByteBool(gCollideMap[Y+1, X] and MARK_BLOCKED)) and
         (not ByteBool(gCollideMap[Y, X-1] and MARK_BLOCKED)) and
         (not ByteBool(gCollideMap[Y, X+1] and MARK_BLOCKED))
      then
      }
      if (not isBlockedAt(X, Y-1)) and
         (not isBlockedAt(X, Y+1)) and
         (not isBlockedAt(X-1, Y)) and
         (not isBlockedAt(X+1, Y))
      {$ELSE}
      if not g_Map_CollidePanel(X-1, Y-1, 3, 3, (PANEL_STEP or PANEL_WALL or PANEL_OPENDOOR or PANEL_CLOSEDOOR))
      {$ENDIF}
      then
      begin // Отлипла - капает
        VelY := 0.5;
        AccelY := 0.15;
        State := STATE_NORMAL;
      end
      else if (Random(200) = 100) then
      begin // Прилеплена - но возможно стекает
        VelY := 0.5;
        AccelY := 0.15;
        exit;
      end;
    end;

    {$IF not DEFINED(D2F_NEW_SPARK_THINKER)}
    if not isBlockedAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_BLOCKED)} then
    begin
      if isLiftUpAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_LIFTUP)} then
      begin // Лифт вверх
        if (VelY > -4-Random(3)) then VelY -= 0.8;
        if (abs(VelX) > 0.1) then VelX -= VelX/10.0;
        VelX += (Random-Random)*0.2;
        AccelY := 0.15;
      end;
      if isLiftLeftAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_LIFTLEFT)} then
      begin // Поток влево
        if (VelX > -8-Random(3)) then VelX -= 0.8;
        AccelY := 0.15;
      end;
      if isLiftRightAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_LIFTRIGHT)} then
      begin // Поток вправо
        if (VelX < 8+Random(3)) then VelX += 0.8;
        AccelY := 0.15;
      end;
    end;
    {$ELSE}
    pan := g_Map_PanelAtPoint(X, Y, GridTagLift);
    if (pan <> nil) then
    begin
      if ((pan.PanelType and PANEL_LIFTUP) <> 0) then
      begin
        if (VelY > -4-Random(3)) then VelY -= 0.8;
        if (abs(VelX) > 0.1) then VelX -= VelX/10.0;
        VelX += (Random-Random)*0.2;
        AccelY := 0.15;
      end;
      if ((pan.PanelType and PANEL_LIFTLEFT) <> 0) then
      begin
        if (VelX > -8-Random(3)) then VelX -= 0.8;
        AccelY := 0.15;
      end;
      if ((pan.PanelType and PANEL_LIFTRIGHT) <> 0) then
      begin
        if (VelX < 8+Random(3)) then VelX += 0.8;
        AccelY := 0.15;
      end;
    end;
    {$ENDIF}

    dX := Round(VelX);
    dY := Round(VelY);

    {$IF not DEFINED(D2F_NEW_SPARK_THINKER)}
    if (Abs(VelX) < 0.1) and (Abs(VelY) < 0.1) then
    begin
      if (State <> STATE_STICK) and
         (not isBlockedAt(X, Y-1) {ByteBool(gCollideMap[Y-1, X] and MARK_BLOCKED)}) and
         (not isBlockedAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_BLOCKED)}) and
         (not isBlockedAt(X, Y+1) {ByteBool(gCollideMap[Y+1, X] and MARK_BLOCKED)}) then
      begin // Висит в воздухе - капает
        VelY := 0.8;
        AccelY := 0.5;
        State := STATE_NORMAL;
      end;
    end;
    {$ELSE}
    if (State <> STATE_STICK) and (Abs(VelX) < 0.1) and (Abs(VelY) < 0.1) then
    begin
      // Висит в воздухе - капает
      if (nil = g_Map_traceToNearest(X, Y-1, X, Y+1, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey)) then
      begin
        VelY := 0.8;
        AccelY := 0.5;
        State := STATE_NORMAL;
      end;
    end;
    {$ENDIF}

    {$IF DEFINED(D2F_NEW_SPARK_THINKER)}
    // horizontal
    if (dX <> 0) then
    begin
      pan := g_Map_traceToNearest(X, Y, X+dX, Y, (GridTagWall or GridTagDoor or GridTagStep), @ex, @ey);
      X := ex;
      // free to ride?
      if (pan <> nil) then
      begin
        // Стена/дверь
        VelX := 0;
        VelY := 0;
        AccelX := 0;
        AccelY := 0;
        State := STATE_STICK;
        if (dX > 0) then stickDX := 1 else stickDX := -1;
      end;
      if (X < 0) or (X >= w) then begin die(); exit; end;
    end;
    // vertical
    if (dY <> 0) then
    begin
      pan := g_Map_traceToNearest(X, Y, X, Y+dY, (GridTagWall or GridTagDoor or GridTagStep), @ex, @ey);
      Y := ey;
      // free to ride?
      if (pan <> nil) then
      begin
        // Стена/дверь
        VelX := 0;
        VelY := 0;
        AccelX := 0;
        AccelY := 0;
        if (dY > 0) and (State <> STATE_STICK) then
        begin
          State := STATE_NORMAL;
        end
        else
        begin
          State := STATE_STICK;
               if (g_Map_PanelAtPoint(X-1, Y, (GridTagWall or GridTagDoor or GridTagStep)) <> nil) then stickDX := -1
          else if (g_Map_PanelAtPoint(X+1, Y, (GridTagWall or GridTagDoor or GridTagStep)) <> nil) then stickDX := 1
          else stickDX := 0;
        end;
      end;
      if (Y < 0) or (Y >= h) then begin die(); exit; end;
    end;
    {$ELSE}
    // horizontal
    if (dX <> 0) then
    begin
      if (dX > 0) then s := 1 else s := -1;
      dX := Abs(dX);
      for b := 1 to dX do
      begin
        if (X+s >= w) or (X+s <= 0) then begin die(); break; end;
        //c := gCollideMap[Y, X+s];
        if isBlockedAt(X+s, Y) {ByteBool(c and MARK_BLOCKED)} then
        begin // Стена/дверь
          VelX := 0;
          VelY := 0;
          AccelX := 0;
          AccelY := 0;
          State := STATE_STICK;
          break;
        end;
        X := X+s;
      end;
    end;
    // vertical
    if (dY <> 0) then
    begin
      if (dY > 0) then s := 1 else s := -1;
      dY := Abs(dY);
      for b := 1 to dY do
      begin
        if (Y+s >= h) or (Y+s <= 0) then begin die(); break; end;
        //c := gCollideMap[Y+s, X];
        if isBlockedAt(X, Y+s) {ByteBool(c and MARK_BLOCKED)} then
        begin // Стена/дверь
          VelX := 0;
          VelY := 0;
          AccelX := 0;
          AccelY := 0;
          if (s > 0) and (State <> STATE_STICK) then State := STATE_NORMAL else State := STATE_STICK;
          break;
        end;
        Y := Y+s;
      end;
    end;
    {$ENDIF}
  end // if gAdvBlood
  else
  begin
    dX := Round(VelX);
    dY := Round(VelY);
    if (X+dX >= w) or (Y+dY >= h) or (X+dX <= 0) or (Y+dY <= 0) or isBlockedAt(X+dX, Y+dY) {ByteBool(gCollideMap[Y+dY, X+dX] and MARK_BLOCKED)} then
    begin // Стена/дверь/граница
      die();
      exit;
      //VelX := 0;
      //VelY := 0;
    end
    else
    begin
      Y += dY;
      X += dX;
    end;
  end;

  VelX += AccelX;
  VelY += AccelY;

  // Кровь растворяется в жидкости:
  if isLiquidAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_LIQUID)} then
  begin
    Time += 1;
    Alpha := 255-trunc((255.0*Time)/LiveTime);
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
  if (State = STATE_STICK) and (Random(30) = 15) then
  begin // Стекает/отлипает
    VelY := 0.5;
    AccelY := 0.15;
    {$IF not DEFINED(D2F_NEW_SPARK_THINKER)}
    if (not isBlockedAt(X-1, Y) {ByteBool(gCollideMap[Y, X-1] and MARK_BLOCKED)}) and
       (not isBlockedAt(X+1, Y) {ByteBool(gCollideMap[Y, X+1] and MARK_BLOCKED)}) then
      State := STATE_NORMAL;
    {$ELSE}
    if (stickDX = 0) then
    begin
      // no walls around, drop
      State := STATE_NORMAL;
    end
    else
    begin
      if (g_Map_PanelAtPoint(X+stickDX, Y, (GridTagWall or GridTagDoor or GridTagStep)) = nil) then State := STATE_NORMAL;
    end;
    {$ENDIF}
    exit;
  end;

  {$IF not DEFINED(D2F_NEW_SPARK_THINKER)}
  if not isBlockedAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_BLOCKED)} then
  begin
    if isLiftUpAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_LIFTUP)} then
    begin // Лифт вверх
      if VelY > -4-Random(3) then
        VelY := VelY - 0.8;
      if Abs(VelX) > 0.1 then
        VelX := VelX - VelX/10.0;
      VelX := VelX + (Random-Random)*0.2;
      AccelY := 0.15;
    end;
    if isLiftLeftAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_LIFTLEFT)} then
    begin // Поток влево
      if VelX > -8-Random(3) then
        VelX := VelX - 0.8;
      AccelY := 0.15;
    end;
    if isLiftRightAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_LIFTRIGHT)} then
    begin // Поток вправо
      if VelX < 8+Random(3) then
        VelX := VelX + 0.8;
      AccelY := 0.15;
    end;
  end;
  {$ELSE}
  pan := g_Map_PanelAtPoint(X, Y, (GridTagAcid1 or GridTagAcid2 or GridTagWater or GridTagLift));
  if (pan <> nil) then
  begin
    if ((pan.tag and (GridTagAcid1 or GridTagAcid2 or GridTagWater)) <> 0) then begin die(); exit; end;
    if ((pan.PanelType and PANEL_LIFTUP) <> 0) then
    begin
      if (VelY > -4-Random(3)) then VelY -= 0.8;
      if (Abs(VelX) > 0.1) then VelX -= VelX/10.0;
      VelX += (Random-Random)*0.2;
      AccelY := 0.15;
    end;
    if ((pan.PanelType and PANEL_LIFTLEFT) <> 0) then
    begin
      if (VelX > -8-Random(3)) then VelX -= 0.8;
      AccelY := 0.15;
    end;
    if ((pan.PanelType and PANEL_LIFTRIGHT) <> 0) then
    begin
      if (VelX < 8+Random(3)) then VelX += 0.8;
      AccelY := 0.15;
    end;
  end;
  {$ENDIF}

  dX := Round(VelX);
  dY := Round(VelY);

  {$IF not DEFINED(D2F_NEW_SPARK_THINKER)}
  if (Abs(VelX) < 0.1) and (Abs(VelY) < 0.1) then
  begin
    if (State <> STATE_STICK) and
       (not isBlockedAt(X, Y-1) {ByteBool(gCollideMap[Y-1, X] and MARK_BLOCKED)}) and
       (not isBlockedAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_BLOCKED)}) and
       (not isBlockedAt(X, Y+1) {ByteBool(gCollideMap[Y+1, X] and MARK_BLOCKED)}) then
    begin // Висит в воздухе - капает
      VelY := 0.8;
      AccelY := 0.5;
      State := STATE_NORMAL;
    end;
  end;
  {$ELSE}
  if (State <> STATE_STICK) and (Abs(VelX) < 0.1) and (Abs(VelY) < 0.1) then
  begin
    // Висит в воздухе - капает
    if (nil = g_Map_traceToNearest(X, Y-1, X, Y+1, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey)) then
    begin
      VelY := 0.8;
      AccelY := 0.5;
      State := STATE_NORMAL;
    end;
  end;
  {$ENDIF}

  {$IF DEFINED(D2F_NEW_SPARK_THINKER)}
  // horizontal
  if (dX <> 0) then
  begin
    pan := g_Map_traceToNearest(X, Y, X+dX, Y, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey);
    X := ex;
    // free to ride?
    if (pan <> nil) then
    begin
      // nope
      if (dY > 0) and ((pan.tag and (GridTagAcid1 or GridTagAcid2 or GridTagWater)) <> 0) then begin die(); exit; end;
      // Стена/дверь?
      if ((pan.tag and (GridTagWall or GridTagDoor or GridTagStep)) <> 0) then
      begin
        VelX := 0;
        VelY := 0;
        AccelX := 0;
        AccelY := 0;
        State := STATE_STICK;
        if (dX > 0) then stickDX := 1 else stickDX := -1;
      end;
    end;
    if (X < 0) or (X >= gMapInfo.Width) then begin die(); exit; end;
  end;
  // vertical
  if (dY <> 0) then
  begin
    pan := g_Map_traceToNearest(X, Y, X, Y+dY, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey);
    Y := ey;
    // free to ride?
    if (pan <> nil) then
    begin
      // nope
      if (dY > 0) and ((pan.tag and (GridTagAcid1 or GridTagAcid2 or GridTagWater)) <> 0) then begin die(); exit; end;
      // Стена/дверь?
      if ((pan.tag and (GridTagWall or GridTagDoor or GridTagStep)) <> 0) then
      begin
        VelX := 0;
        VelY := 0;
        AccelX := 0;
        AccelY := 0;
        if (dY > 0) and (State <> STATE_STICK) then
        begin
          State := STATE_NORMAL;
        end
        else
        begin
          State := STATE_STICK;
               if (g_Map_PanelAtPoint(X-1, Y, (GridTagWall or GridTagDoor or GridTagStep)) <> nil) then stickDX := -1
          else if (g_Map_PanelAtPoint(X+1, Y, (GridTagWall or GridTagDoor or GridTagStep)) <> nil) then stickDX := 1
          else stickDX := 0;
        end;
      end;
    end;
    if (Y < 0) or (Y >= gMapInfo.Height) then begin die(); exit; end;
  end;
  {$ELSE}
  // horizontal
  if (dX <> 0) then
  begin
    if (dX > 0) then s := 1 else s := -1;
    for b := 1 to Abs(dX) do
    begin
      // Сбоку граница?
      if (X+s >= w) or (X+s <= 0) then begin die(); break;end;
      //c := gCollideMap[Y, X+s];
      // Сбоку жидкость, а частица уже падает?
      if isLiquidAt(X+s, Y) {ByteBool(c and MARK_LIQUID)} and (dY > 0) then begin die(); break; end;
      if isBlockedAt(X+s, Y) {ByteBool(c and MARK_BLOCKED)} then
      begin // Стена/дверь
        VelX := 0;
        VelY := 0;
        AccelX := 0;
        AccelY := 0;
        State := STATE_STICK;
        Break;
      end;
      X := X+s;
    end;
  end;
  // vertical
  if (dY <> 0) then
  begin
    if (dY > 0) then s := 1 else s := -1;
    for b := 1 to Abs(dY) do
    begin
      // Снизу/сверху граница
      if (Y+s >= h) or (Y+s <= 0) then begin die(); break; end;
      //c := gCollideMap[Y+s, X];
      // Снизу жидкость, а частица уже падает
      if isLiquidAt(X, Y+s) {ByteBool(c and MARK_LIQUID)} and (dY > 0) then begin die(); break; end;
      if isBlockedAt(X, Y+s) {ByteBool(c and MARK_BLOCKED)} then
      begin // Стена/дверь
        VelX := 0;
        VelY := 0;
        AccelX := 0;
        AccelY := 0;
        if (s > 0) and (State <> STATE_STICK) then State := STATE_NORMAL else State := STATE_STICK;
        break;
      end;
      Y := Y+s;
    end;
  end;
  {$ENDIF}

  VelX += AccelX;
  VelY += AccelY;

  Time += 1;
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
  dX := Round(VelX);
  dY := Round(VelY);

  {$IF DEFINED(D2F_NEW_SPARK_THINKER)}
  if (Abs(VelX) < 0.1) and (Abs(VelY) < 0.1) then
  begin
    pan := g_Map_traceToNearest(X, Y-1, X, Y+1, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey);
  end;
  {$ELSE}
  if (Abs(VelX) < 0.1) and (Abs(VelY) < 0.1) and
     (not isBlockedAt(X, Y-1) {ByteBool(gCollideMap[Y-1, X] and MARK_BLOCKED)}) and
     (not isBlockedAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_BLOCKED)}) and
     (not isBlockedAt(X, Y+1) {ByteBool(gCollideMap[Y+1, X] and MARK_BLOCKED)}) then
  begin // Висит в воздухе
    VelY := 0.8;
    AccelY := 0.5;
  end;
  {$ENDIF}

  if (dX <> 0) then
  begin
    {$IF DEFINED(D2F_NEW_SPARK_THINKER)}
    pan := g_Map_traceToNearest(X, Y, X+dX, Y, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey);
    //e_WriteLog(Format('spark h-trace: (%d,%d)-(%d,%d); dx=%d; end=(%d,%d); hit=%d', [X, Y, X+dX, Y, dX, ex, ey, Integer(pan <> nil)]), MSG_NOTIFY);
    X := ex;
    // free to ride?
    if (pan <> nil) then
    begin
      // nope
      if ((pan.tag and (GridTagAcid1 or GridTagAcid2 or GridTagWater)) <> 0) then begin die(); exit; end;
      VelX := 0;
      AccelX := 0;
    end;
    if (X < 0) or (X >= gMapInfo.Width) then begin die(); exit; end;
    {$ELSE}
    if (dX > 0) then s := 1 else s := -1;
    dX := Abs(dX);
    for b := 1 to dX do
    begin
      if (X+s >= gMapInfo.Width) or (X+s <= 0) then begin die(); break; end;
      //c := gCollideMap[Y, X+s];
      if isBlockedAt(X+s, Y) {ByteBool(c and MARK_BLOCKED)} then
        begin // Стена/дверь - падает вертикально
          VelX := 0;
          AccelX := 0;
          Break;
        end
      else // Пусто:
        if not isAnythingAt(X+s, Y) {c = MARK_FREE} then
          X := X + s
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
    pan := g_Map_traceToNearest(X, Y, X, Y+dY, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey);
    //e_WriteLog(Format('spark y-trace: (%d,%d)-(%d,%d); dy=%d; end=(%d,%d); hit=%d', [X, Y, X, Y+dY, dY, ex, ey, Integer(pan <> nil)]), MSG_NOTIFY);
    (*
    if (pan <> nil) then
    begin
      e_WriteLog(Format('spark y-trace: %08x (%d,%d)-(%d,%d); dy=%d; end=(%d,%d); hittag=%04x', [LongWord(@self), X, Y, X, Y+dY, dY, ex, ey, pan.tag]), MSG_NOTIFY);
    end
    else
    begin
      e_WriteLog(Format('spark y-trace: %08x (%d,%d)-(%d,%d); dy=%d; end=(%d,%d); hit=%d', [LongWord(@self), X, Y, X, Y+dY, dY, ex, ey, Integer(pan <> nil)]), MSG_NOTIFY);
    end;
    *)
    Y := ey;
    // free to ride?
    if (pan <> nil) then
    begin
      //die(); exit;
      // nope
      if ((pan.tag and (GridTagAcid1 or GridTagAcid2 or GridTagWater)) <> 0) then begin die(); exit; end;
      if (dY < 0) then
      begin
        VelY := -VelY;
        AccelY := abs(AccelY);
      end
      else
      begin
        VelX := 0;
        AccelX := 0;
        VelY := 0;
        AccelY := 0.8;
      end;
    end;
    if (Y < 0) or (Y >= gMapInfo.Height) then begin die(); exit; end;
    {$ELSE}
    if (dY > 0) then s := 1 else s := -1;
    dY := Abs(dY);
    for b := 1 to dY do
    begin
      if (Y+s >= gMapInfo.Height) or (Y+s <= 0) then begin die(); break; end;
      //c := gCollideMap[Y+s, X];
      if isBlockedAt(X, Y+s) {ByteBool(c and MARK_BLOCKED)} then
        begin // Стена/дверь - падает вертикально
          if s < 0 then
            begin
              VelY := -VelY;
              AccelY := Abs(AccelY);
            end
          else // Или не падает
            begin
              VelX := 0;
              AccelX := 0;
              VelY := 0;
              AccelY := 0.8;
            end;

          Break;
        end
      else // Пусто:
        if not isAnythingAt(X, Y+s) {c = MARK_FREE} then
          Y := Y + s
        else // Осальное:
          begin
            die();
            break;
          end;
    end;
    {$ENDIF}
  end;

  if (VelX <> 0.0) then VelX += AccelX;

  if (VelY <> 0.0) then
  begin
    if (AccelY < 10) then AccelY += 0.08;
    VelY += AccelY;
  end;

  Time += 1;
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

  dY := Round(VelY);

  if dY <> 0 then
  begin
    if dY > 0 then
      s := 1
    else
      s := -1;

    for b := 1 to Abs(dY) do
    begin
      if (Y+s >= h) or (Y+s <= 0) then begin die(); break; end;

      (*
      if not isLiquidAt(X, Y+s) {ByteBool(gCollideMap[Y+s, X] and MARK_LIQUID)} then
      begin // Уже не жидкость
        State := STATE_FREE;
        Break;
      end;
      *)
      // we traced liquid before, so don't bother checking
      if (Y+s <= liquidTopY) then begin die(); break; end;

      Y := Y+s;
    end;
  end;

  if VelY > -4 then
    VelY := VelY + AccelY;

  Time := Time + 1;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_GFX_SparkVel (fX, fY: Integer; Count: Word; VX, VY: Integer; DevX, DevY: Byte);
var
  a: Integer;
  DevX1, DevX2,
  DevY1, DevY2: Byte;
  l: Integer;
begin
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
      X := fX-DevX1+Random(DevX2);
      Y := fY-DevY1+Random(DevY2);

      VelX := VX + (Random-Random)*3;
      VelY := VY + (Random-Random)*3;

      if VelY > -4 then
        if VelY-4 < -4 then
          VelY := -4
        else
          VelY := VelY-4;

      AccelX := -Sign(VelX)*Random/100;
      AccelY := 0.8;

      Red := 255;
      Green := 100+Random(155);
      Blue := 64;
      Alpha := 255;

      State := STATE_NORMAL;
      Time := 0;
      LiveTime := 30+Random(60);
      ParticleType := PARTICLE_SPARK;

      {CorrectOffsets(CurrentParticle);}
    end;

    if CurrentParticle+2 > MaxParticles then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
  end;
end;


procedure g_GFX_Blood(fX, fY: Integer; Count: Word; vx, vy: Integer;
  DevX, DevY: Word; CR, CG, CB: Byte; Kind: Byte = BLOOD_NORMAL);
var
  a: Integer;
  DevX1, DevX2,
  DevY1, DevY2: Word;
  l: Integer;
  CRnd: Byte;
  CC: SmallInt;
begin
  if Kind = BLOOD_SPARKS then
  begin
    g_GFX_SparkVel(fX, fY, 2 + Random(2), -VX div 2, -VY div 2, DevX, DevY);
    Exit;
  end;
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
      X := fX - DevX1 + Random(DevX2);
      Y := fY - DevY1 + Random(DevY2);

      {
      if (X < 0) or (X > gMapInfo.Width-1) or
         (Y < 0) or (Y > gMapInfo.Height-1) or
         ByteBool(gCollideMap[Y, X] and MARK_WALL) then
        Continue;
      }
      if isWallAt(X, Y) then continue;

      VelX := vx + (Random-Random)*3;
      VelY := vy + (Random-Random)*3;

      if VelY > -4 then
        if VelY-4 < -4 then
          VelY := -4
        else
          VelY := VelY-4;

      AccelX := -Sign(VelX)*Random/100;
      AccelY := 0.8;

      CRnd := 20*Random(6);
      if CR > 0 then
      begin
        CC := CR + CRnd - 50;
        if CC < 0   then CC := 0;
        if CC > 255 then CC := 255;
        Red := CC;
      end else
        Red := 0;
      if CG > 0 then
      begin
        CC := CG + CRnd - 50;
        if CC < 0   then CC := 0;
        if CC > 255 then CC := 255;
        Green := CC;
      end else
        Green := 0;
      if CB > 0 then
      begin
        CC := CB + CRnd - 50;
        if CC < 0   then CC := 0;
        if CC > 255 then CC := 255;
        Blue := CC;
      end else
        Blue := 0;

      Alpha := 255;

      State := STATE_NORMAL;
      Time := 0;
      LiveTime := 120+Random(40);
      ParticleType := PARTICLE_BLOOD;

      {CorrectOffsets(CurrentParticle);}
    end;

    if CurrentParticle >= MaxParticles-1 then
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
      X := fX-DevX1+Random(DevX2);
      Y := fY-DevY1+Random(DevY2);

      VelX := BaseVelX*Random;
      VelY := BaseVelY-Random;
      AccelX := VelX/3.0;
      AccelY := VelY/5.0;

      Red := 255;
      Green := 100+Random(155);
      Blue := 64;
      Alpha := 255;

      State := STATE_NORMAL;
      Time := 0;
      LiveTime := 30+Random(60);
      ParticleType := PARTICLE_SPARK;

      {CorrectOffsets(CurrentParticle);}
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
      X := fX-DevX1+Random(DevX2);
      Y := fY-DevY1+Random(DevY2);

      if Abs(fVelX) < 0.5 then
        VelX := 1.0 - 2.0*Random
      else
        VelX := fVelX*Random;
      if Random(10) < 7 then
        VelX := -VelX;
      VelY := fVelY*Random;
      AccelX := 0.0;
      AccelY := 0.8;

      case Color of
        1: // Красный
          begin
            Red := 155 + Random(9)*10;
            Green := Trunc(150*Random);
            Blue := Green;
          end;
        2: // Зеленый
          begin
            Red := Trunc(150*Random);
            Green := 175 + Random(9)*10;
            Blue := Red;
          end;
        3: // Синий
          begin
            Red := Trunc(200*Random);
            Green := Red;
            Blue := 175 + Random(9)*10;
          end;
        else // Серый
          begin
            Red := 90 + Random(12)*10;
            Green := Red;
            Blue := Red;
          end;
      end;

      Alpha := 255;

      State := STATE_NORMAL;
      Time := 0;
      LiveTime := 60+Random(60);
      ParticleType := PARTICLE_WATER;

      {CorrectOffsets(CurrentParticle);}
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
  l := Length(Particles);
  if l = 0 then
    Exit;
  if Count > l then
    Count := l;

  for a := 1 to Count do
  begin
    with Particles[CurrentParticle] do
    begin
      X := fX;
      Y := fY;

      VelX := fVelX;
      VelY := fVelY;
      AccelX := 0.0;
      AccelY := 0.8;

      case DefColor of
        1: // Красный
          begin
            Red := 155 + Random(9)*10;
            Green := Trunc(150*Random);
            Blue := Green;
          end;
        2: // Зеленый
          begin
            Red := Trunc(150*Random);
            Green := 175 + Random(9)*10;
            Blue := Red;
          end;
        3: // Синий
          begin
            Red := Trunc(200*Random);
            Green := Red;
            Blue := 175 + Random(9)*10;
          end;
        4: // Свой цвет, светлее
          begin
            Red := 20 + Random(19)*10;
            Green := Red;
            Blue := Red;
            Red := Min(Red + CR, 255);
            Green := Min(Green + CG, 255);
            Blue := Min(Blue + CB, 255);
          end;
        5: // Свой цвет, темнее
          begin
            Red := 20 + Random(19)*10;
            Green := Red;
            Blue := Red;
            Red := Max(CR - Red, 0);
            Green := Max(CG - Green, 0);
            Blue := Max(CB - Blue, 0);
          end;
        else // Серый
          begin
            Red := 90 + Random(12)*10;
            Green := Red;
            Blue := Red;
          end;
      end;

      Alpha := 255;

      State := STATE_NORMAL;
      Time := 0;
      LiveTime := 60+Random(60);
      ParticleType := PARTICLE_WATER;

      {CorrectOffsets(CurrentParticle);}
    end;

    if CurrentParticle+2 > MaxParticles then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
  end;
end;


procedure g_GFX_Bubbles(fX, fY: Integer; Count: Word; DevX, DevY: Byte);
var
  a: Integer;
  DevX1, DevX2,
  DevY1, DevY2: Byte;
  l, liquidx: Integer;
begin
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
      X := fX-DevX1+Random(DevX2);
      Y := fY-DevY1+Random(DevY2);

      if (X >= gMapInfo.Width) or (X <= 0) or
         (Y >= gMapInfo.Height) or (Y <= 0) then
        Continue;

      (*
      // don't spawn bubbles outside of the liquid
      if not isLiquidAt(X, Y) {ByteBool(gCollideMap[Y, X] and MARK_LIQUID)} then
        Continue;
      *)

      // trace liquid, so we'll know where it ends; do it in 8px steps for speed
      // tracer will return `false` if we started outside of the liquid
      if not g_Map_TraceLiquidNonPrecise(X, Y, 0, -8, liquidx, liquidTopY) then continue;

      VelX := 0;
      VelY := -1-Random;
      AccelX := 0;
      AccelY := VelY/10;

      Red := 255;
      Green := 255;
      Blue := 255;
      Alpha := 255;

      State := STATE_NORMAL;
      Time := 0;
      LiveTime := 65535;
      ParticleType := PARTICLE_BUBBLES;

      {CorrectOffsets(CurrentParticle);}
    end;

    if CurrentParticle+2 > MaxParticles then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
  end;
end;

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

function FindOnceAnim: DWORD;
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

procedure g_GFX_OnceAnim(X, Y: Integer; Anim: TAnimation; AnimType: Byte = 0);
var
  find_id: DWORD;
begin
  if Anim = nil then
    Exit;

  find_id := FindOnceAnim();

  OnceAnims[find_id].AnimType := AnimType;
  OnceAnims[find_id].Animation := TAnimation.Create(Anim.FramesID, Anim.Loop, Anim.Speed);
  OnceAnims[find_id].Animation.Blending := Anim.Blending;
  OnceAnims[find_id].Animation.Alpha := Anim.Alpha;
  OnceAnims[find_id].X := X;
  OnceAnims[find_id].Y := Y;
end;

procedure g_GFX_Update();
var
  a: Integer;
  w, h: Integer;
  len: Integer;
begin
  if not gpart_dbg_enabled then exit;
  if Particles <> nil then
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
          if (Time = LiveTime) then begin die(); continue; end;
          if (X+1 >= w) or (Y+1 >= h) or (X <= 0) or (Y <= 0) then begin die(); end;
          //if not alive then Continue;
          //e_WriteLog(Format('particle #%d: %d', [State, ParticleType]), MSG_NOTIFY);
          think();
          {CorrectOffsets(a);}
        end; // with
      end; // if
    end; // for
  end; // Particles <> nil

  if OnceAnims <> nil then
  begin
    for a := 0 to High(OnceAnims) do
      if OnceAnims[a].Animation <> nil then
      begin
        case OnceAnims[a].AnimType of
          ONCEANIM_SMOKE:
            begin
              if Random(3) = 0 then
                OnceAnims[a].X := OnceAnims[a].X-1+Random(3);
              if Random(2) = 0 then
                OnceAnims[a].Y := OnceAnims[a].Y-Random(2);
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

procedure g_GFX_Draw();
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
        if alive and (X >= sX) and (Y >= sY) and (X <= sX+sWidth) and (sY <= sY+sHeight) then
        begin
          glColor4ub(Red, Green, Blue, Alpha);
          glVertex2i(X + offsetX, Y + offsetY);
        end;

    glEnd();

    glDisable(GL_BLEND);
  end;

  if OnceAnims <> nil then
    for a := 0 to High(OnceAnims) do
      if OnceAnims[a].Animation <> nil then
        with OnceAnims[a] do
          Animation.Draw(X, Y, M_NONE);
end;

end.
