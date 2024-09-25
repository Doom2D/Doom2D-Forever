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

{$INCLUDE a_modes.inc}
// some geometry utilities
unit geom;

interface


// do line clipping; returns `false` if line is outside of the box
function clipLine (var x0, y0, x1, y1: Single; xmin, ymin, xmax, ymax: Single): Boolean;

// returns `true` if there is an intersection (if starting point is inside the box, it counts as intersection)
function lineAABBIntersects (x0, y0, x1, y1: Integer; bx, by, bw, bh: Integer): Boolean;

// you are not supposed to understand this
// returns `true` if there is an intersection, and enter coords
// enter coords will be equal to (x0, y0) if starting point is inside the box
// if result is `false`, `inx` and `iny` are undefined
function lineAABBIntersects (x0, y0, x1, y1: Integer; bx, by, bw, bh: Integer; out inx, iny: Integer): Boolean;

type
  TSweepEdge = (None, Top, Right, Bottom, Left);
  PSweepEdge = ^TSweepEdge;

// sweep two AABB's to see if and when they are overlapping
// returns `true` if collision was detected (but boxes aren't overlap)
// `u1` and `u1` has no sense if no collision was detected (`hitx` and `hity` either)
// u0 = normalized time of first collision (i.e. collision starts at myMove*u0)
// u1 = normalized time of second collision (i.e. collision stops after myMove*u1)
// hitedge for `it`: it will probably be `None` if no collision was detected, but it is not guaranteed
// enter/exit coords will form non-intersecting configuration (i.e. will be before/after the actual collision)
// but beware of floating point inexactness; `sweepAABB()` will try to (crudely) compensate for it
// while calculating `hitx` and `hity`.
{
function sweepAABB (mex0, mey0, mew, meh: Integer; medx, medy: Integer; itx0, ity0, itw, ith: Integer;
                    u0: PSingle=nil; hitedge: PSweepEdge=nil; u1: PSingle=nil;
                    hitx: PInteger=nil; hity: PInteger=nil): Boolean;
}
function sweepAABB (mex0, mey0, mew, meh: Integer; medx, medy: Integer; itx0, ity0, itw, ith: Integer;
                    out u0: Single): Boolean;

function distanceSq (x0, y0, x1, y1: Integer): Integer; inline;


implementation


// ////////////////////////////////////////////////////////////////////////// //
function distanceSq (x0, y0, x1, y1: Integer): Integer; inline; begin result := (x1-x0)*(x1-x0)+(y1-y0)*(y1-y0); end;


// ////////////////////////////////////////////////////////////////////////// //
function clipLine (var x0, y0, x1, y1: Single; xmin, ymin, xmax, ymax: Single): Boolean;
const
  Inside = 0;
  Left = 1;
  Right = 2;
  Bottom = 4;
  Top = 8;

  function xcode (x, y: Single): Byte; inline;
  begin
    result := Inside;
    if (x < xmin) then result := result or Left else if (x > xmax) then result := result or Right;
    if (y < ymin) then result := result or Bottom else if (y > ymax) then result := result or Top;
  end;

var
  outcode0, outcode1, outcodeOut: Byte;
  x: Single = 0;
  y: Single = 0;
begin
  result := false; // accept
  outcode0 := xcode(x0, y0);
  outcode1 := xcode(x1, y1);
  while true do
  begin
    if ((outcode0 or outcode1) = 0) then begin result := true; exit; end; // accept
    if ((outcode0 and outcode1) <> 0) then exit; // reject
    outcodeOut := outcode0;
    if (outcodeOut = 0) then outcodeOut := outcode1;
    if ((outcodeOut and Top) <> 0) then
    begin
      x := x0+(x1-x0)*(ymax-y0)/(y1-y0);
      y := ymax;
    end
    else if ((outcodeOut and Bottom) <> 0) then
    begin
      x := x0+(x1-x0)*(ymin-y0)/(y1-y0);
      y := ymin;
    end
    else if ((outcodeOut and Right) <> 0) then
    begin
      y := y0+(y1-y0)*(xmax-x0)/(x1-x0);
      x := xmax;
    end
    else if ((outcodeOut and Left) <> 0) then
    begin
      y := y0+(y1-y0)*(xmin-x0)/(x1-x0);
      x := xmin;
    end;
    if (outcodeOut = outcode0) then
    begin
      x0 := x;
      y0 := y;
      outcode0 := xcode(x0, y0);
    end
    else
    begin
      x1 := x;
      y1 := y;
      outcode1 := xcode(x1, y1);
    end;
  end;
end;


// returns `true` if there is an intersection (if starting point is inside the box, it counts as intersection)
function lineAABBIntersects (x0, y0, x1, y1: Integer; bx, by, bw, bh: Integer): Boolean;
var
  sx0, sy0, sx1, sy1: Single;
begin
  result := false;
  if (bw < 1) or (bh < 1) then exit;
  if (x0 >= bx) and (y0 >= by) and (x0 < bx+bw) and (y0 < by+bh) then begin result := true; exit; end;
  sx0 := x0; sy0 := y0;
  sx1 := x1; sy1 := y1;
  result := clipLine(sx0, sy0, sx1, sy1, bx, by, bx+bw-1, by+bh-1);
end;


// returns `true` if there is an intersection, and enter coords
// enter coords will be equal to (x0, y0) if starting point is inside the box
// if result is `false`, `inx` and `iny` are undefined
function lineAABBIntersects (x0, y0, x1, y1: Integer; bx, by, bw, bh: Integer; out inx, iny: Integer): Boolean;
var
  sx0, sy0, sx1, sy1: Single;
begin
  inx := x0;
  iny := y0;
  result := false;
  if (bw < 1) or (bh < 1) then exit;
  if (x0 >= bx) and (y0 >= by) and (x0 < bx+bw) and (y0 < by+bh) then begin result := true; exit; end;
  sx0 := x0; sy0 := y0;
  sx1 := x1; sy1 := y1;
  result := clipLine(sx0, sy0, sx1, sy1, bx, by, bx+bw-1, by+bh-1);
  if result then
  begin
    inx := trunc(sx0);
    iny := trunc(sy0);
    // hack!
    if (inx = bx) then Dec(inx) else if (inx = bx+bw-1) then Inc(inx);
    if (iny = by) then Dec(iny) else if (iny = by+bh-1) then Inc(iny);
  end
  else
  begin
    inx := x1;
    iny := y1;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
{
function sweepAABB (mex0, mey0, mew, meh: Integer; medx, medy: Integer; itx0, ity0, itw, ith: Integer;
                    u0: PSingle=nil; hitedge: PSweepEdge=nil; u1: PSingle=nil;
                    hitx: PInteger=nil; hity: PInteger=nil): Boolean;
var
  tin, tout: Single;

  function axisOverlap (me0, me1, it0, it1, d: Integer; he0, he1: TSweepEdge): Boolean; inline;
  var
    t: Single;
  begin
    result := false;

    if (me1 < it0) then
    begin
      if (d >= 0) then exit; // oops, no hit
      t := (me1-it0+1)/d;
      if (t > tin) then begin tin := t; hitedge^ := he1; end;
    end
    else if (it1 < me0) then
    begin
      if (d <= 0) then exit; // oops, no hit
      t := (me0-it1-1)/d;
      if (t > tin) then begin tin := t; hitedge^ := he0; end;
    end;

    if (d < 0) and (it1 > me0) then
    begin
      t := (me0-it1-1)/d;
      if (t < tout) then tout := t;
    end
    else if (d > 0) and (me1 > it0) then
    begin
      t := (me1-it0+1)/d;
      if (t < tout) then tout := t;
    end;

    result := true;
  end;

var
  mex1, mey1, itx1, ity1, vx, vy, ex, ey: Integer;
  htt: TSweepEdge = TSweepEdge.None; // has no sense, who cares
begin
  result := false;
  if (u0 <> nil) then u0^ := -1.0;
  if (u1 <> nil) then u1^ := -1.0;
  if (hitx <> nil) then hitx^ := mex0;
  if (hity <> nil) then hity^ := mey0;
  if (hitedge = nil) then hitedge := @htt else hitedge^ := TSweepEdge.None;

  if (mew < 1) or (meh < 1) or (itw < 1) or (ith < 1) then exit;

  mex1 := mex0+mew-1;
  mey1 := mey0+meh-1;
  itx1 := itx0+itw-1;
  ity1 := ity0+ith-1;

  // check if they are overlapping right now (SAT)
  //if (mex1 >= itx0) and (mex0 <= itx1) and (mey1 >= ity0) and (mey0 <= ity1) then begin result := true; exit; end;

  if (medx = 0) and (medy = 0) then exit; // both boxes are sationary

  // treat b as stationary, so invert v to get relative velocity
  vx := -medx;
  vy := -medy;

  tin := -100000000.0;
  tout := 100000000.0;

  if not axisOverlap(mex0, mex1, itx0, itx1, vx, TSweepEdge.Right, TSweepEdge.Left) then exit;
  if not axisOverlap(mey0, mey1, ity0, ity1, vy, TSweepEdge.Bottom, TSweepEdge.Top) then exit;

  if (u0 <> nil) then u0^ := tin;
  if (u1 <> nil) then u1^ := tout;

  if (tin <= tout) and (tin >= 0.0) and (tin <= 1.0) then
  begin
    result := true;
    if (hitx <> nil) or (hity <> nil) then
    begin
      ex := mex0+round(medx*tin);
      ey := mey0+round(medy*tin);
      // just in case, compensate for floating point inexactness
      if (ex >= itx0) and (ey >= ity0) and (ex < itx0+itw) and (ey < ity0+ith) then
      begin
        ex := mex0+trunc(medx*tin);
        ey := mey0+trunc(medy*tin);
      end;
      if (hitx <> nil) then hitx^ := ex;
      if (hity <> nil) then hity^ := ey;
    end;
  end;
end;
}

function sweepAABB (mex0, mey0, mew, meh: Integer; medx, medy: Integer; itx0, ity0, itw, ith: Integer;
                    out u0: Single): Boolean;
var
  tin, tout: Single;

  function axisOverlap (me0, me1, it0, it1, d: Integer): Boolean; inline;
    var t: Single;
  begin
    Result := false;

    if me1 < it0 then
    begin
      if (d >= 0) then
        exit; // oops, no hit
      t := (me1 - it0 + 1) / d;
      if t > tin then
        tin := t;
    end
    else if it1 < me0 then
    begin
      if d <= 0 then
        exit; // oops, no hit
      t := (me0 - it1 - 1) / d;
      if t > tin then
        tin := t;
    end;

    if (d < 0) and (it1 > me0) then
    begin
      t := (me0 - it1 - 1) / d;
      if t < tout then
        tout := t;
    end
    else if (d > 0) and (me1 > it0) then
    begin
      t := (me1 - it0 + 1) / d;
      if t < tout then
        tout := t;
    end;

    result := true;
  end;

begin
  Result := False;
  u0 := -1;

  if (mew >= 1) and (meh >= 1) and (itw >= 1) and (ith >= 1) and ((medx <> 0) or (medy <> 0)) then
  begin
    tin := -100000000.0;
    tout := 100000000.0;
    if axisOverlap(mex0, mex0 + mew - 1, itx0, itx0 + itw - 1, -medx) then
    begin
      if axisOverlap(mey0, mey0 + meh - 1, ity0, ity0 + ith - 1, -medy) then
      begin
        u0 := tin;
        Result := (tin <= tout) and (tin >= 0.0) and (tin <= 1.0);
      end;
    end;
  end;
end;


end.
