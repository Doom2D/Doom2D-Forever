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
{.$DEFINE aabbtree_many_asserts}
{$DEFINE aabbtree_query_count}
{.$DEFINE aabbtree_use_floats}
unit z_aabbtree;

interface

uses
  e_log, g_grid;


// ////////////////////////////////////////////////////////////////////////// //
type
  {$IFDEF aabbtree_use_floats}TreeNumber = Single;{$ELSE}TreeNumber = Integer;{$ENDIF}


// ////////////////////////////////////////////////////////////////////////// //
type
  Ray2D = record
  public
    origX, origY: Single;
    dirX, dirY: Single;

    function getOrigN (idx: Integer): Single; inline;
    function getDirN (idx: Integer): Single; inline;

  public
    constructor Create (ax, ay: Single; aangle: Single); overload;
    constructor Create (ax0, ay0, ax1, ay1: Single); overload;
    constructor Create (constref aray: Ray2D); overload;

    procedure copyFrom (constref aray: Ray2D); inline;

    procedure normalizeDir (); inline;

    procedure setXYAngle (ax, ay: Single; aangle: Single); inline;
    procedure setX0Y0X1Y1 (ax0, ay0, ax1, ay1: Single); inline;

    procedure atTime (time: Single; out rx, ry: Integer); inline;

    property orig[idx: Integer]: Single read getOrigN;
    property dir[idx: Integer]: Single read getDirN;
  end;

// ////////////////////////////////////////////////////////////////////////// //
type
  AABB2D = record
  public
    minX, minY, maxX, maxY: TreeNumber;

  private
    function getvalid (): Boolean; inline;
    function getcenterX (): TreeNumber; inline;
    function getcenterY (): TreeNumber; inline;
    function getextentX (): TreeNumber; inline;
    function getextentY (): TreeNumber; inline;
    function getMinN (idx: Integer): TreeNumber; inline;
    function getMaxN (idx: Integer): TreeNumber; inline;

  public
    constructor Create (x0, y0, x1, y1: TreeNumber); overload;
    constructor Create (constref aabb: AABB2D); overload;
    constructor Create (constref aabb0, aabb1: AABB2D); overload;

    constructor CreateWH (ax, ay, w, h: TreeNumber);

    procedure copyFrom (constref aabb: AABB2D); inline;
    procedure setDims (x0, y0, x1, y1: TreeNumber); inline;

    procedure setMergeTwo (constref aabb0, aabb1: AABB2D); inline;

    function volume (): TreeNumber; inline;

    procedure merge (constref aabb: AABB2D); inline;

    // return true if the current AABB contains the AABB given in parameter
    function contains (constref aabb: AABB2D): Boolean; inline; overload;
    function contains (ax, ay: TreeNumber): Boolean; inline; overload;

    // return true if the current AABB is overlapping with the AABB in parameter
    // two AABBs overlap if they overlap in the two axes at the same time
    function overlaps (constref aabb: AABB2D): Boolean; inline; overload;

    // ray direction must be normalized
    function intersects (constref ray: Ray2D; tmino: PSingle=nil; tmaxo: PSingle=nil): Boolean; overload;
    function intersects (ax, ay, bx, by: Single; tmino: PSingle=nil): Boolean; inline; overload;
    function intersects (constref ray: Ray2D; maxtime: Single; tmino: PSingle=nil): Boolean; inline; overload;

    property valid: Boolean read getvalid;
    property centerX: TreeNumber read getcenterX;
    property centerY: TreeNumber read getcenterY;
    property extentX: TreeNumber read getextentX;
    property extentY: TreeNumber read getextentY;

    property min[idx: Integer]: TreeNumber read getMinN;
    property max[idx: Integer]: TreeNumber read getMaxN;
  end;


// ////////////////////////////////////////////////////////////////////////// //
(* Dynamic AABB tree (bounding volume hierarchy)
 * based on the code from ReactPhysics3D physics library, http://www.reactphysics3d.com
 * Copyright (c) 2010-2016 Daniel Chappuis
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from the
 * use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not claim
 *    that you wrote the original software. If you use this software in a
 *    product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 *
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 *
 * 3. This notice may not be removed or altered from any source distribution.
 *)
// ////////////////////////////////////////////////////////////////////////// //
(*
 * This class implements a dynamic AABB tree that is used for broad-phase
 * collision detection. This data structure is inspired by Nathanael Presson's
 * dynamic tree implementation in BulletPhysics. The following implementation is
 * based on the one from Erin Catto in Box2D as described in the book
 * "Introduction to Game Physics with Box2D" by Ian Parberry.
 *)
// ////////////////////////////////////////////////////////////////////////// //
// Dynamic AABB Tree: can be used to speed up broad phase in various engines
type
  generic TDynAABBTreeBase<ITP> = class(TObject)
  public
    type TTreeFlesh = ITP;

  private
    type
      PTreeNode = ^TTreeNode;
      TTreeNode = record
      public
        const NullTreeNode = -1;
        const Left = 0;
        const Right = 1;
      public
        // a node is either in the tree (has a parent) or in the free nodes list (has a next node)
        parentId: Integer;
        //nextNodeId: Integer;
        // a node is either a leaf (has data) or is an internal node (has children)
        children: array [0..1] of Integer; // left and right child of the node (children[0] = left child)
        // height of the node in the tree (-1 for free nodes)
        height: SmallInt;
        // fat axis aligned bounding box (AABB) corresponding to the node
        aabb: AABB2D;
        //TODO: `flesh` can be united with `children`
        flesh: TTreeFlesh;
        fleshX, fleshY: TreeNumber;
        tag: Integer; // just a user-defined tag
      public
        // return true if the node is a leaf of the tree
        procedure clear (); inline;
        function leaf (): Boolean; inline;
        function isfree (): Boolean; inline;
        property nextNodeId: Integer read parentId write parentId;
        //property flesh: Integer read children[0] write children[0];

        procedure dumpToLog ();
      end;

      TVisitCheckerCB = function (node: PTreeNode): Boolean of object;
      //TVisitVisitorCB = function (abody: TTreeFlesh; atag: Integer): Boolean is nested;

    const ModeNoChecks = 0;
    const ModeAABB = 1;
    const ModePoint = 2;

  public
    // return `true` to stop
    type TForEachLeafCB = function (abody: TTreeFlesh; constref aabb: AABB2D): Boolean is nested; // WARNING! don't modify AABB here!

  public
    // in the broad-phase collision detection (dynamic AABB tree), the AABBs are
    // also inflated in direction of the linear motion of the body by mutliplying the
    // followin constant with the linear velocity and the elapsed time between two frames
    {$IFDEF aabbtree_use_floats}
    const LinearMotionGapMultiplier = 1.7;
    {$ELSE}
    const LinearMotionGapMultiplier = 17; // *10
    {$ENDIF}

  public
    // called when a overlapping node has been found during the call to forEachAABBOverlap()
    // return `true` to stop
    type TQueryOverlapCB = function (abody: TTreeFlesh; atag: Integer): Boolean is nested;
    type TSegQueryCallback = function (abody: TTreeFlesh; var ray: Ray2D): Single is nested; // return hit time

    PSegmentQueryResult = ^TSegmentQueryResult;
    TSegmentQueryResult = record
      time: Single; // <0: nothing was hit
      flesh: TTreeFlesh;

      constructor Create (fuckyoufpc: Boolean);
      procedure reset (); inline;
      function valid (): Boolean; inline;
    end;

  private
    mNodes: array of TTreeNode; // nodes of the tree
    mRootNodeId: Integer; // id of the root node of the tree
    mFreeNodeId: Integer; // id of the first node of the list of free (allocated) nodes in the tree that we can use
    mAllocCount: Integer; // number of allocated nodes in the tree
    mNodeCount: Integer; // number of nodes in the tree

    // extra AABB Gap used to allow the collision shape to move a little bit
    // without triggering a large modification of the tree which can be costly
    mExtraGap: TreeNumber;

    chkAABB: AABB2D; // for checkers
    qSRes: PSegmentQueryResult; // for queries
    // for segment query
    maxFraction: Single;
    curax, curay: Single;
    curbx, curby: Single;
    dirx, diry: Single;
    traceRay: Ray2D;
    sqcb: TSegQueryCallback;
    vstack: array of Integer; // for `visit()`
    vstused: Integer; // to support recursive queries

    function checkerAABB (node: PTreeNode): Boolean;
    function checkerPoint (node: PTreeNode): Boolean;
    function checkerRay (node: PTreeNode): Boolean;
    function visitorRay (flesh: TTreeFlesh; tag: Integer): Boolean;

    type TQueryOverlapDg = function (abody: TTreeFlesh; atag: Integer): Boolean of object;

  private
    function allocateNode (): Integer;
    procedure releaseNode (nodeId: Integer);
    procedure insertLeafNode (nodeId: Integer);
    procedure removeLeafNode (nodeId: Integer);
    function balanceSubTreeAtNode (nodeId: Integer): Integer;
    function computeHeight (nodeId: Integer): Integer;
    function insertObjectInternal (constref aabb: AABB2D; staticObject: Boolean): Integer;
    procedure setup ();
    function visit (constref caabb: AABB2D; mode: Integer; checker: TVisitCheckerCB; visitor: TQueryOverlapCB; visdg: TQueryOverlapDg; tagmask: Integer): Integer;

    function forEachNode (nodeId: Integer; dg: TForEachLeafCB): Boolean;

  public
    {$IFDEF aabbtree_query_count}
    mNodesVisited, mNodesDeepVisited: Integer;
    {$ENDIF}

  public
    constructor Create (extraAABBGap: TreeNumber=0);
    destructor Destroy (); override;

    // clear all the nodes and reset the tree
    procedure reset ();

    function forEachLeaf (dg: TForEachLeafCB): Boolean; // WARNING! don't modify AABB/tree here!
    procedure getRootAABB (out aabb: AABB2D);

    function isValidId (id: Integer): Boolean; inline;
    function getNodeObjectId (nodeid: Integer): TTreeFlesh; inline;
    procedure getNodeFatAABB (out aabb: AABB2D; nodeid: Integer); inline;

    // returns `false` if nodeid is not leaf
    function getNodeXY (nodeid: Integer; out x, y: Integer): Boolean; inline;

    // return `false` for invalid flesh
    function getFleshAABB (out aabb: AABB2D; flesh: TTreeFlesh; tag: Integer): Boolean; virtual; abstract;

    // insert an object into the tree
    // this method creates a new leaf node in the tree and returns the id of the corresponding node or -1 on error
    // AABB for static object will not be "fat" (simple optimization)
    // WARNING! inserting the same object several times *WILL* break everything!
    function insertObject (flesh: TTreeFlesh; tag: Integer=-1; staticObject: Boolean=false): Integer;

    // remove an object from the tree
    // WARNING: ids of removed objects can be reused on later insertions!
    procedure removeObject (nodeId: Integer);

    (** update the dynamic tree after an object has moved.
     *
     * if the new AABB of the object that has moved is still inside its fat AABB, then nothing is done.
     * otherwise, the corresponding node is removed and reinserted into the tree.
     * the method returns true if the object has been reinserted into the tree.
     * the `dispX` and `dispY` parameters are the linear velocity of the AABB multiplied by the elapsed time between two frames.
     * if the `forceReinsert` parameter is `true`, we force a removal and reinsertion of the node
     * (this can be useful if the shape AABB has become much smaller than the previous one for instance).
     *
     * note that you should call this method if body's AABB was modified, even if the body wasn't moved.
     *
     * if `forceReinsert` = `true` and both `dispX` and `dispY` are zeroes, convert object to "static" (don't extrude AABB).
     *
     * return `true` if the tree was modified.
     *)
    function updateObject (nodeId: Integer; dispX, dispY: TreeNumber; forceReinsert: Boolean=false): Boolean; overload;
    function updateObject (nodeId: Integer; forceReinsert: Boolean=false): Boolean; overload;

    function aabbQuery (ax, ay, aw, ah: TreeNumber; cb: TQueryOverlapCB; tagmask: Integer=-1): TTreeFlesh;
    function pointQuery (ax, ay: TreeNumber; cb: TQueryOverlapCB; tagmask: Integer=-1): TTreeFlesh;
    function segmentQuery (out qr: TSegmentQueryResult; ax, ay, bx, by: TreeNumber; cb: TSegQueryCallback; tagmask: Integer=-1): Boolean;

    function computeTreeHeight (): Integer; // compute the height of the tree

    property extraGap: TreeNumber read mExtraGap write mExtraGap;
    property nodeCount: Integer read mNodeCount;
    property nodeAlloced: Integer read mAllocCount;
    {$IFDEF aabbtree_query_count}
    property nodesVisited: Integer read mNodesVisited;
    property nodesDeepVisited: Integer read mNodesDeepVisited;
    {$ELSE}
    const nodesVisited = 0;
    const nodesDeepVisited = 0;
    {$ENDIF}
  end;


function dtMinI (a, b: Integer): Integer; inline;
function dtMaxI (a, b: Integer): Integer; inline;

function dtMinF (a, b: TreeNumber): TreeNumber; inline;
function dtMaxF (a, b: TreeNumber): TreeNumber; inline;

function minSingle (a, b: Single): Single; inline;
function maxSingle (a, b: Single): Single; inline;


implementation

uses
  SysUtils;


// ////////////////////////////////////////////////////////////////////////// //
function dtMinI (a, b: Integer): Integer; inline; begin if (a < b) then result := a else result := b; end;
function dtMaxI (a, b: Integer): Integer; inline; begin if (a > b) then result := a else result := b; end;

function dtMinF (a, b: TreeNumber): TreeNumber; inline; begin if (a < b) then result := a else result := b; end;
function dtMaxF (a, b: TreeNumber): TreeNumber; inline; begin if (a > b) then result := a else result := b; end;

function minSingle (a, b: Single): Single; inline; begin if (a < b) then result := a else result := b; end;
function maxSingle (a, b: Single): Single; inline; begin if (a > b) then result := a else result := b; end;


// ////////////////////////////////////////////////////////////////////////// //
constructor Ray2D.Create (ax, ay: Single; aangle: Single); begin setXYAngle(ax, ay, aangle); end;
constructor Ray2D.Create (ax0, ay0, ax1, ay1: Single); begin setX0Y0X1Y1(ax0, ay0, ax1, ay1); end;
constructor Ray2D.Create (constref aray: Ray2D); overload; begin copyFrom(aray); end;


function Ray2D.getOrigN (idx: Integer): Single; inline; begin if (idx = 0) then result := origX else if (idx = 1) then result := origY else result := 0; end;
function Ray2D.getDirN (idx: Integer): Single; inline; begin if (idx = 0) then result := dirX else if (idx = 1) then result := dirY else result := 0; end;


procedure Ray2D.copyFrom (constref aray: Ray2D); inline;
begin
  origX := aray.origX;
  origY := aray.origY;
  dirX := aray.dirX;
  dirY := aray.dirY;
end;

procedure Ray2D.normalizeDir (); inline;
var
  invlen: Single;
begin
  invlen := 1.0/sqrt(dirX*dirX+dirY*dirY);
  dirX *= invlen;
  dirY *= invlen;
end;

procedure Ray2D.setXYAngle (ax, ay: Single; aangle: Single); inline;
begin
  origX := ax;
  origY := ay;
  dirX := cos(aangle);
  dirY := sin(aangle);
end;

procedure Ray2D.setX0Y0X1Y1 (ax0, ay0, ax1, ay1: Single); inline;
begin
  origX := ax0;
  origY := ay0;
  dirX := ax1-ax0;
  dirY := ay1-ay0;
  normalizeDir();
end;


procedure Ray2D.atTime (time: Single; out rx, ry: Integer); inline;
begin
  rx := round(origX+dirX*time);
  ry := round(origY+dirY*time);
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor AABB2D.Create (x0, y0, x1, y1: TreeNumber); overload;
begin
  setDims(x0, y0, x1, y1);
end;

constructor AABB2D.Create (constref aabb: AABB2D); overload;
begin
  copyFrom(aabb);
end;

constructor AABB2D.Create (constref aabb0, aabb1: AABB2D); overload;
begin
  setMergeTwo(aabb0, aabb1);
end;

constructor AABB2D.CreateWH (ax, ay, w, h: TreeNumber);
begin
  minX := ax;
  minY := ay;
  maxX := ax+w-1;
  maxY := ay+h-1;
end;

function AABB2D.getvalid (): Boolean; inline; begin result := (minX <= maxX) and (minY <= maxY); end;

{$IFDEF aabbtree_use_floats}
function AABB2D.getcenterX (): TreeNumber; inline; begin result := (minX+maxX)/2.0; end;
function AABB2D.getcenterY (): TreeNumber; inline; begin result := (minY+maxY)/2.0; end;
{$ELSE}
function AABB2D.getcenterX (): TreeNumber; inline; begin result := (minX+maxX) div 2; end;
function AABB2D.getcenterY (): TreeNumber; inline; begin result := (minY+maxY) div 2; end;
{$ENDIF}
function AABB2D.getextentX (): TreeNumber; inline; begin result := maxX-minX+1; end;
function AABB2D.getextentY (): TreeNumber; inline; begin result := maxY-minY+1; end;

function AABB2D.getMinN (idx: Integer): TreeNumber; inline; begin if (idx = 0) then result := minX else if (idx = 1) then result := minY else result := 0; end;
function AABB2D.getMaxN (idx: Integer): TreeNumber; inline; begin if (idx = 0) then result := maxX else if (idx = 1) then result := maxY else result := 0; end;

procedure AABB2D.copyFrom (constref aabb: AABB2D); inline;
begin
  minX := aabb.minX;
  minY := aabb.minY;
  maxX := aabb.maxX;
  maxY := aabb.maxY;
  {$IF DEFINED(D2F_DEBUG)}
  if not valid then raise Exception.Create('copyFrom: result is fucked');
  {$ENDIF}
end;


procedure AABB2D.setDims (x0, y0, x1, y1: TreeNumber); inline;
begin
  minX := dtMinF(x0, x1);
  minY := dtMinF(y0, y1);
  maxX := dtMaxF(x0, x1);
  maxY := dtMaxF(y0, y1);
  {$IF DEFINED(D2F_DEBUG)}
  if not valid then raise Exception.Create('setDims: result is fucked');
  {$ENDIF}
end;


procedure AABB2D.setMergeTwo (constref aabb0, aabb1: AABB2D); inline;
begin
  {$IF DEFINED(D2F_DEBUG)}
  if not aabb0.valid then raise Exception.Create('setMergeTwo: aabb0 is fucked');
  if not aabb1.valid then raise Exception.Create('setMergeTwo: aabb0 is fucked');
  {$ENDIF}
  minX := dtMinF(aabb0.minX, aabb1.minX);
  minY := dtMinF(aabb0.minY, aabb1.minY);
  maxX := dtMaxF(aabb0.maxX, aabb1.maxX);
  maxY := dtMaxF(aabb0.maxY, aabb1.maxY);
  {$IF DEFINED(D2F_DEBUG)}
  if not valid then raise Exception.Create('setMergeTwo: result is fucked');
  {$ENDIF}
end;


function AABB2D.volume (): TreeNumber; inline;
begin
  result := (maxX-minX+1)*(maxY-minY+1);
end;


procedure AABB2D.merge (constref aabb: AABB2D); inline;
begin
  {$IF DEFINED(D2F_DEBUG)}
  if not aabb.valid then raise Exception.Create('merge: aabb is fucked');
  {$ENDIF}
  minX := dtMinF(minX, aabb.minX);
  minY := dtMinF(minY, aabb.minY);
  maxX := dtMaxF(maxX, aabb.maxX);
  maxY := dtMaxF(maxY, aabb.maxY);
  {$IF DEFINED(D2F_DEBUG)}
  if not valid then raise Exception.Create('setMergeTwo: result is fucked');
  {$ENDIF}
end;


function AABB2D.contains (constref aabb: AABB2D): Boolean; inline; overload;
begin
  result :=
    (aabb.minX >= minX) and (aabb.minY >= minY) and
    (aabb.maxX <= maxX) and (aabb.maxY <= maxY);
end;


function AABB2D.contains (ax, ay: TreeNumber): Boolean; inline; overload;
begin
  result := (ax >= minX) and (ay >= minY) and (ax <= maxX) and (ay <= maxY);
end;


function AABB2D.overlaps (constref aabb: AABB2D): Boolean; inline; overload;
begin
  result := false;
  // exit with no intersection if found separated along any axis
  if (maxX < aabb.minX) or (minX > aabb.maxX) then exit;
  if (maxY < aabb.minY) or (minY > aabb.maxY) then exit;
  result := true;
end;


// something to consider here is that 0 * inf =nan which occurs when the ray starts exactly on the edge of a box
// https://tavianator.com/fast-branchless-raybounding-box-intersections-part-2-nans/
{
function AABB2D.intersects (constref ray: Ray2D; tmino: PSingle=nil; tmaxo: PSingle=nil): Boolean; overload;
var
  dinv, t1, t2, tmp: Single;
  tmin, tmax: Single;
begin
  // ok with coplanars
  tmin := -1.0e100;
  tmax := 1.0e100;
  // do X
  if (ray.dirX <> 0.0) then
  begin
    dinv := 1.0/ray.dirX;
    t1 := (minX-ray.origX)*dinv;
    t2 := (maxX-ray.origX)*dinv;
    if (t1 < t2) then tmin := t1 else tmin := t2;
    if (t1 > t2) then tmax := t1 else tmax := t2;
  end;
  // do Y
  if (ray.dirY <> 0.0) then
  begin
    dinv := 1.0/ray.dirY;
    t1 := (minY-ray.origY)*dinv;
    t2 := (maxY-ray.origY)*dinv;
    // tmin
    if (t1 < t2) then tmp := t1 else tmp := t2; // min(t1, t2)
    if (tmax < tmp) then tmp := tmax; // min(tmax, tmp)
    if (tmin > tmp) then tmin := tmp; // max(tmin, tmp)
    // tmax
    if (t1 > t2) then tmp := t1 else tmp := t2; // max(t1, t2)
    if (tmin > tmp) then tmp := tmin; // max(tmin, tmp)
    if (tmax < tmp) then tmax := tmp; // min(tmax, tmp)
  end;
  if (tmin > 0) then tmp := tmin else tmp := 0;
  if (tmax > tmp) then
  begin
    if (tmino <> nil) then tmino^ := tmin;
    if (tmaxo <> nil) then tmaxo^ := tmax;
    result := true;
  end
  else
  begin
    result := false;
  end;
end;
}


function AABB2D.intersects (constref ray: Ray2D; tmino: PSingle=nil; tmaxo: PSingle=nil): Boolean; overload;
var
  tmin, tmax, t1, t2, invd: Single;
  i: Integer;
begin
  tmin := -1.0e100;
  tmax := 1.0e100;
  for i := 0 to 1 do
  begin
    if (ray.dir[i] <> 0.0) then
    begin
      //t1 := (self.min[i]-ray.orig[i])/ray.dir[i];
      //t2 := (self.max[i]-ray.orig[i])/ray.dir[i];
      invd := 1.0/ray.dir[i];
      t1 := (self.min[i]-ray.orig[i])*invd;
      t2 := (self.max[i]-ray.orig[i])*invd;
      tmin := maxSingle(tmin, minSingle(t1, t2));
      tmax := minSingle(tmax, maxSingle(t1, t2));
    end
    else if (ray.orig[i] <= self.min[i]) or (ray.orig[i] >= self.max[i]) then
    begin
      result := false;
      exit;
    end;
  end;

  result := (tmax > tmin) and (tmax > 0.0);
  if result then
  begin
    if (tmino <> nil) then tmino^ := tmin;
    if (tmaxo <> nil) then tmaxo^ := tmin;
  end;
end;


function AABB2D.intersects (ax, ay, bx, by: Single; tmino: PSingle=nil): Boolean; inline; overload;
var
  tmin: Single;
  ray: Ray2D;
begin
  result := true;
  if (tmino <> nil) then tmino^ := 0.0;
  // it may be faster to first check if start or end point is inside AABB (this is sometimes enough for dyntree)
  if (ax >= minX) and (ay >= minY) and (ax <= maxX) and (ay <= maxY) then exit; // a
  if (bx >= minX) and (by >= minY) and (bx <= maxX) and (by <= maxY) then exit; // b
  // nope, do it hard way
  ray := Ray2D.Create(ax, ay, bx, by);
  if not intersects(ray, @tmin) then begin if (tmino <> nil) then tmino^ := tmin; result := false; exit; end;
  if (tmino <> nil) then tmino^ := tmin;
  if (tmin < 0) then exit; // inside, just in case
  bx -= ax;
  by -= ay;
  result := (tmin*tmin <= bx*bx+by*by);
end;


function AABB2D.intersects (constref ray: Ray2D; maxtime: Single; tmino: PSingle=nil): Boolean; inline; overload;
var
  tmin: Single;
begin
  result := true;
  if (ray.origX >= minX) and (ray.origY >= minY) and (ray.origX <= maxX) and (ray.origY <= maxY) then
  begin
    if (tmino <> nil) then tmino^ := 0.0;
    exit;
  end;
  if not intersects(ray, @tmin) then begin if (tmino <> nil) then tmino^ := -1.0; result := false; exit; end;
  if (tmin < 0) then tmin := 0; // inside
  if (tmino <> nil) then tmino^ := tmin;
  result := (tmin <= maxtime);
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TDynAABBTreeBase.TSegmentQueryResult.Create (fuckyoufpc: Boolean); begin time := -1; flesh := Default(ITP); end;
procedure TDynAABBTreeBase.TSegmentQueryResult.reset (); inline; begin time := -1; flesh := Default(ITP); end;
function TDynAABBTreeBase.TSegmentQueryResult.valid (): Boolean; inline; begin result := (time >= 0) and (flesh <> Default(ITP)); end;


// ////////////////////////////////////////////////////////////////////////// //
function TDynAABBTreeBase.TTreeNode.leaf (): Boolean; inline; begin result := (height = 0); end;
function TDynAABBTreeBase.TTreeNode.isfree (): Boolean; inline; begin result := (height = -1); end;

procedure TDynAABBTreeBase.TTreeNode.clear (); inline;
begin
  parentId := 0;
  children[0] := 0;
  children[1] := 0;
  flesh := Default(ITP);
  tag := 0;
  height := 0;
  aabb.minX := 0;
  aabb.minY := 0;
  aabb.maxX := 0;
  aabb.maxY := 0;
end;

procedure TDynAABBTreeBase.TTreeNode.dumpToLog ();
begin
  e_WriteLog(Format('NODE: parentId=%d; children=[%d,%d]; height=%d; tag=%d; fleshX=%d; fleshY=%d; aabb=(%d,%d)-(%d,%d)',
    [parentId, children[0], children[1], Integer(height), tag, fleshX, fleshY, aabb.minX, aabb.minY, aabb.maxX, aabb.maxY]),
    MSG_NOTIFY);
end;


// ////////////////////////////////////////////////////////////////////////// //
// allocate and return a node to use in the tree
function TDynAABBTreeBase.allocateNode (): Integer;
var
  i, newsz, freeNodeId: Integer;
  node: PTreeNode;
begin
  // if there is no more allocated node to use
  if (mFreeNodeId = TTreeNode.NullTreeNode) then
  begin
    {$IFDEF aabbtree_many_asserts}assert(mNodeCount = mAllocCount);{$ENDIF}
    // allocate more nodes in the tree
    if (mAllocCount <= 16384) then newsz := mAllocCount*2 else newsz := mAllocCount+16384;
    SetLength(mNodes, newsz);
    mAllocCount := newsz;
    // initialize the allocated nodes
    for i := mNodeCount to mAllocCount-1 do
    begin
      mNodes[i].nextNodeId := i+1;
      mNodes[i].height := -1;
    end;
    mNodes[mAllocCount-1].nextNodeId := TTreeNode.NullTreeNode;
    mFreeNodeId := mNodeCount;
  end;
  // get the next free node
  freeNodeId := mFreeNodeId;
  {$IFDEF aabbtree_many_asserts}assert(freeNodeId < mAllocCount);{$ENDIF}
  node := @mNodes[freeNodeId];
  mFreeNodeId := node.nextNodeId;
  node.clear();
  node.parentId := TTreeNode.NullTreeNode;
  node.height := 0;
  Inc(mNodeCount);
  result := freeNodeId;

  //e_WriteLog(Format('tree: allocated node #%d', [result]), MSG_NOTIFY);
end;


// release a node
procedure TDynAABBTreeBase.releaseNode (nodeId: Integer);
begin
  {$IFDEF aabbtree_many_asserts}assert(mNodeCount > 0);{$ENDIF}
  {$IFDEF aabbtree_many_asserts}assert((nodeId >= 0) and (nodeId < mAllocCount));{$ENDIF}
  {$IFDEF aabbtree_many_asserts}assert(mNodes[nodeId].height >= 0);{$ENDIF}
  mNodes[nodeId].nextNodeId := mFreeNodeId;
  mNodes[nodeId].height := -1;
  mNodes[nodeId].flesh := Default(ITP);
  mFreeNodeId := nodeId;
  Dec(mNodeCount);

  //e_WriteLog(Format('tree: released node #%d', [nodeId]), MSG_NOTIFY);
end;


// insert a leaf node in the tree
// the process of inserting a new leaf node in the dynamic tree is described in the book "Introduction to Game Physics with Box2D" by Ian Parberry
procedure TDynAABBTreeBase.insertLeafNode (nodeId: Integer);
var
  newNodeAABB, mergedAABBs, currentAndLeftAABB, currentAndRightAABB: AABB2D;
  currentNodeId: Integer;
  leftChild, rightChild, siblingNode: Integer;
  oldParentNode, newParentNode: Integer;
  volumeAABB, mergedVolume: TreeNumber;
  costS, costI, costLeft, costRight: TreeNumber;
begin
  // if the tree is empty
  if (mRootNodeId = TTreeNode.NullTreeNode) then
  begin
    mRootNodeId := nodeId;
    mNodes[mRootNodeId].parentId := TTreeNode.NullTreeNode;
    exit;
  end;

  {$IFDEF aabbtree_many_asserts}assert(mRootNodeId <> TTreeNode.NullTreeNode);{$ENDIF}

  // find the best sibling node for the new node
  newNodeAABB := AABB2D.Create(mNodes[nodeId].aabb);
  currentNodeId := mRootNodeId;
  while not mNodes[currentNodeId].leaf do
  begin
    leftChild := mNodes[currentNodeId].children[TTreeNode.Left];
    rightChild := mNodes[currentNodeId].children[TTreeNode.Right];

    // compute the merged AABB
    volumeAABB := mNodes[currentNodeId].aabb.volume;
    mergedAABBs := AABB2D.Create(mNodes[currentNodeId].aabb, newNodeAABB);
    mergedVolume := mergedAABBs.volume;

    // compute the cost of making the current node the sibling of the new node
    costS := 2*mergedVolume;

    // compute the minimum cost of pushing the new node further down the tree (inheritance cost)
    costI := 2*(mergedVolume-volumeAABB);

    // compute the cost of descending into the left child
    currentAndLeftAABB := AABB2D.Create(newNodeAABB, mNodes[leftChild].aabb);
    costLeft := currentAndLeftAABB.volume+costI;
    if not mNodes[leftChild].leaf then costLeft -= mNodes[leftChild].aabb.volume;

    // compute the cost of descending into the right child
    currentAndRightAABB := AABB2D.Create(newNodeAABB, mNodes[rightChild].aabb);
    costRight := currentAndRightAABB.volume+costI;
    if not mNodes[rightChild].leaf then costRight -= mNodes[rightChild].aabb.volume;

    // if the cost of making the current node a sibling of the new node is smaller than the cost of going down into the left or right child
    if (costS < costLeft) and (costS < costRight) then break;

    // it is cheaper to go down into a child of the current node, choose the best child
    //currentNodeId = (costLeft < costRight ? leftChild : rightChild);
    if (costLeft < costRight) then currentNodeId := leftChild else currentNodeId := rightChild;
  end;

  siblingNode := currentNodeId;

  // create a new parent for the new node and the sibling node
  oldParentNode := mNodes[siblingNode].parentId;
  newParentNode := allocateNode();
  mNodes[newParentNode].parentId := oldParentNode;
  mNodes[newParentNode].aabb.setMergeTwo(mNodes[siblingNode].aabb, newNodeAABB);
  mNodes[newParentNode].height := mNodes[siblingNode].height+1;
  {$IFDEF aabbtree_many_asserts}assert(mNodes[newParentNode].height > 0);{$ENDIF}

  // if the sibling node was not the root node
  if (oldParentNode <> TTreeNode.NullTreeNode) then
  begin
    {$IFDEF aabbtree_many_asserts}assert(not mNodes[oldParentNode].leaf);{$ENDIF}
    if (mNodes[oldParentNode].children[TTreeNode.Left] = siblingNode) then
    begin
      mNodes[oldParentNode].children[TTreeNode.Left] := newParentNode;
    end
    else
    begin
      mNodes[oldParentNode].children[TTreeNode.Right] := newParentNode;
    end;
    mNodes[newParentNode].children[TTreeNode.Left] := siblingNode;
    mNodes[newParentNode].children[TTreeNode.Right] := nodeId;
    mNodes[siblingNode].parentId := newParentNode;
    mNodes[nodeId].parentId := newParentNode;
  end
  else
  begin
    // if the sibling node was the root node
    mNodes[newParentNode].children[TTreeNode.Left] := siblingNode;
    mNodes[newParentNode].children[TTreeNode.Right] := nodeId;
    mNodes[siblingNode].parentId := newParentNode;
    mNodes[nodeId].parentId := newParentNode;
    mRootNodeId := newParentNode;
  end;

  // move up in the tree to change the AABBs that have changed
  currentNodeId := mNodes[nodeId].parentId;
  {$IFDEF aabbtree_many_asserts}assert(not mNodes[currentNodeId].leaf);{$ENDIF}
  while (currentNodeId <> TTreeNode.NullTreeNode) do
  begin
    // balance the sub-tree of the current node if it is not balanced
    currentNodeId := balanceSubTreeAtNode(currentNodeId);
    {$IFDEF aabbtree_many_asserts}assert(mNodes[nodeId].leaf);{$ENDIF}

    {$IFDEF aabbtree_many_asserts}assert(not mNodes[currentNodeId].leaf);{$ENDIF}
    leftChild := mNodes[currentNodeId].children[TTreeNode.Left];
    rightChild := mNodes[currentNodeId].children[TTreeNode.Right];
    {$IFDEF aabbtree_many_asserts}assert(leftChild <> TTreeNode.NullTreeNode);{$ENDIF}
    {$IFDEF aabbtree_many_asserts}assert(rightChild <> TTreeNode.NullTreeNode);{$ENDIF}

    // recompute the height of the node in the tree
    mNodes[currentNodeId].height := dtMaxI(mNodes[leftChild].height, mNodes[rightChild].height)+1;
    {$IFDEF aabbtree_many_asserts}assert(mNodes[currentNodeId].height > 0);{$ENDIF}

    // recompute the AABB of the node
    mNodes[currentNodeId].aabb.setMergeTwo(mNodes[leftChild].aabb, mNodes[rightChild].aabb);

    currentNodeId := mNodes[currentNodeId].parentId;
  end;

  {$IFDEF aabbtree_many_asserts}assert(mNodes[nodeId].leaf);{$ENDIF}
end;


// remove a leaf node from the tree
procedure TDynAABBTreeBase.removeLeafNode (nodeId: Integer);
var
  currentNodeId, parentNodeId, grandParentNodeId, siblingNodeId: Integer;
  leftChildId, rightChildId: Integer;
begin
  {$IFDEF aabbtree_many_asserts}assert((nodeId >= 0) and (nodeId < mAllocCount));{$ENDIF}
  {$IFDEF aabbtree_many_asserts}assert(mNodes[nodeId].leaf);{$ENDIF}

  // if we are removing the root node (root node is a leaf in this case)
  if (mRootNodeId = nodeId) then begin mRootNodeId := TTreeNode.NullTreeNode; exit; end;

  parentNodeId := mNodes[nodeId].parentId;
  grandParentNodeId := mNodes[parentNodeId].parentId;

  if (mNodes[parentNodeId].children[TTreeNode.Left] = nodeId) then
  begin
    siblingNodeId := mNodes[parentNodeId].children[TTreeNode.Right];
  end
  else
  begin
    siblingNodeId := mNodes[parentNodeId].children[TTreeNode.Left];
  end;

  // if the parent of the node to remove is not the root node
  if (grandParentNodeId <> TTreeNode.NullTreeNode) then
  begin
    // destroy the parent node
    if (mNodes[grandParentNodeId].children[TTreeNode.Left] = parentNodeId) then
    begin
      mNodes[grandParentNodeId].children[TTreeNode.Left] := siblingNodeId;
    end
    else
    begin
      {$IFDEF aabbtree_many_asserts}assert(mNodes[grandParentNodeId].children[TTreeNode.Right] = parentNodeId);{$ENDIF}
      mNodes[grandParentNodeId].children[TTreeNode.Right] := siblingNodeId;
    end;
    mNodes[siblingNodeId].parentId := grandParentNodeId;
    releaseNode(parentNodeId);

    // now, we need to recompute the AABBs of the node on the path back to the root and make sure that the tree is still balanced
    currentNodeId := grandParentNodeId;
    while (currentNodeId <> TTreeNode.NullTreeNode) do
    begin
      // balance the current sub-tree if necessary
      currentNodeId := balanceSubTreeAtNode(currentNodeId);

      {$IFDEF aabbtree_many_asserts}assert(not mNodes[currentNodeId].leaf);{$ENDIF}

      // get the two children of the current node
      leftChildId := mNodes[currentNodeId].children[TTreeNode.Left];
      rightChildId := mNodes[currentNodeId].children[TTreeNode.Right];

      // recompute the AABB and the height of the current node
      mNodes[currentNodeId].aabb.setMergeTwo(mNodes[leftChildId].aabb, mNodes[rightChildId].aabb);
      mNodes[currentNodeId].height := dtMaxI(mNodes[leftChildId].height, mNodes[rightChildId].height)+1;
      {$IFDEF aabbtree_many_asserts}assert(mNodes[currentNodeId].height > 0);{$ENDIF}

      currentNodeId := mNodes[currentNodeId].parentId;
    end;
  end
  else
  begin
    // if the parent of the node to remove is the root node, the sibling node becomes the new root node
    mRootNodeId := siblingNodeId;
    mNodes[siblingNodeId].parentId := TTreeNode.NullTreeNode;
    releaseNode(parentNodeId);
  end;
end;


// balance the sub-tree of a given node using left or right rotations
// the rotation schemes are described in the book "Introduction to Game Physics with Box2D" by Ian Parberry
// this method returns the new root node id
function TDynAABBTreeBase.balanceSubTreeAtNode (nodeId: Integer): Integer;
var
  nodeA, nodeB, nodeC, nodeF, nodeG: PTreeNode;
  nodeBId, nodeCId, nodeFId, nodeGId: Integer;
  balanceFactor: Integer;
begin
  {$IFDEF aabbtree_many_asserts}assert(nodeId <> TTreeNode.NullTreeNode);{$ENDIF}

  nodeA := @mNodes[nodeId];

  // if the node is a leaf or the height of A's sub-tree is less than 2
  if (nodeA.leaf) or (nodeA.height < 2) then begin result := nodeId; exit; end; // do not perform any rotation

  // get the two children nodes
  nodeBId := nodeA.children[TTreeNode.Left];
  nodeCId := nodeA.children[TTreeNode.Right];
  {$IFDEF aabbtree_many_asserts}assert((nodeBId >= 0) and (nodeBId < mAllocCount));{$ENDIF}
  {$IFDEF aabbtree_many_asserts}assert((nodeCId >= 0) and (nodeCId < mAllocCount));{$ENDIF}
  nodeB := @mNodes[nodeBId];
  nodeC := @mNodes[nodeCId];

  // compute the factor of the left and right sub-trees
  balanceFactor := nodeC.height-nodeB.height;

  // if the right node C is 2 higher than left node B
  if (balanceFactor > 1) then
  begin
    {$IFDEF aabbtree_many_asserts}assert(not nodeC.leaf);{$ENDIF}

    nodeFId := nodeC.children[TTreeNode.Left];
    nodeGId := nodeC.children[TTreeNode.Right];
    {$IFDEF aabbtree_many_asserts}assert((nodeFId >= 0) and (nodeFId < mAllocCount));{$ENDIF}
    {$IFDEF aabbtree_many_asserts}assert((nodeGId >= 0) and (nodeGId < mAllocCount));{$ENDIF}
    nodeF := @mNodes[nodeFId];
    nodeG := @mNodes[nodeGId];

    nodeC.children[TTreeNode.Left] := nodeId;
    nodeC.parentId := nodeA.parentId;
    nodeA.parentId := nodeCId;

    if (nodeC.parentId <> TTreeNode.NullTreeNode) then
    begin
      if (mNodes[nodeC.parentId].children[TTreeNode.Left] = nodeId) then
      begin
        mNodes[nodeC.parentId].children[TTreeNode.Left] := nodeCId;
      end
      else
      begin
        {$IFDEF aabbtree_many_asserts}assert(mNodes[nodeC.parentId].children[TTreeNode.Right] = nodeId);{$ENDIF}
        mNodes[nodeC.parentId].children[TTreeNode.Right] := nodeCId;
      end;
    end
    else
    begin
      mRootNodeId := nodeCId;
    end;

    {$IFDEF aabbtree_many_asserts}assert(not nodeC.leaf);{$ENDIF}
    {$IFDEF aabbtree_many_asserts}assert(not nodeA.leaf);{$ENDIF}

    // if the right node C was higher than left node B because of the F node
    if (nodeF.height > nodeG.height) then
    begin
      nodeC.children[TTreeNode.Right] := nodeFId;
      nodeA.children[TTreeNode.Right] := nodeGId;
      nodeG.parentId := nodeId;

      // recompute the AABB of node A and C
      nodeA.aabb.setMergeTwo(nodeB.aabb, nodeG.aabb);
      nodeC.aabb.setMergeTwo(nodeA.aabb, nodeF.aabb);

      // recompute the height of node A and C
      nodeA.height := dtMaxI(nodeB.height, nodeG.height)+1;
      nodeC.height := dtMaxI(nodeA.height, nodeF.height)+1;
      {$IFDEF aabbtree_many_asserts}assert(nodeA.height > 0);{$ENDIF}
      {$IFDEF aabbtree_many_asserts}assert(nodeC.height > 0);{$ENDIF}
    end
    else
    begin
      // if the right node C was higher than left node B because of node G
      nodeC.children[TTreeNode.Right] := nodeGId;
      nodeA.children[TTreeNode.Right] := nodeFId;
      nodeF.parentId := nodeId;

      // recompute the AABB of node A and C
      nodeA.aabb.setMergeTwo(nodeB.aabb, nodeF.aabb);
      nodeC.aabb.setMergeTwo(nodeA.aabb, nodeG.aabb);

      // recompute the height of node A and C
      nodeA.height := dtMaxI(nodeB.height, nodeF.height)+1;
      nodeC.height := dtMaxI(nodeA.height, nodeG.height)+1;
      {$IFDEF aabbtree_many_asserts}assert(nodeA.height > 0);{$ENDIF}
      {$IFDEF aabbtree_many_asserts}assert(nodeC.height > 0);{$ENDIF}
    end;

    // return the new root of the sub-tree
    result := nodeCId;
    exit;
  end;

  // if the left node B is 2 higher than right node C
  if (balanceFactor < -1) then
  begin
    {$IFDEF aabbtree_many_asserts}assert(not nodeB.leaf);{$ENDIF}

    nodeFId := nodeB.children[TTreeNode.Left];
    nodeGId := nodeB.children[TTreeNode.Right];
    {$IFDEF aabbtree_many_asserts}assert((nodeFId >= 0) and (nodeFId < mAllocCount));{$ENDIF}
    {$IFDEF aabbtree_many_asserts}assert((nodeGId >= 0) and (nodeGId < mAllocCount));{$ENDIF}
    nodeF := @mNodes[nodeFId];
    nodeG := @mNodes[nodeGId];

    nodeB.children[TTreeNode.Left] := nodeId;
    nodeB.parentId := nodeA.parentId;
    nodeA.parentId := nodeBId;

    if (nodeB.parentId <> TTreeNode.NullTreeNode) then
    begin
      if (mNodes[nodeB.parentId].children[TTreeNode.Left] = nodeId) then
      begin
        mNodes[nodeB.parentId].children[TTreeNode.Left] := nodeBId;
      end
      else
      begin
        {$IFDEF aabbtree_many_asserts}assert(mNodes[nodeB.parentId].children[TTreeNode.Right] = nodeId);{$ENDIF}
        mNodes[nodeB.parentId].children[TTreeNode.Right] := nodeBId;
      end;
    end
    else
    begin
      mRootNodeId := nodeBId;
    end;

    {$IFDEF aabbtree_many_asserts}assert(not nodeB.leaf);{$ENDIF}
    {$IFDEF aabbtree_many_asserts}assert(not nodeA.leaf);{$ENDIF}

    // if the left node B was higher than right node C because of the F node
    if (nodeF.height > nodeG.height) then
    begin
      nodeB.children[TTreeNode.Right] := nodeFId;
      nodeA.children[TTreeNode.Left] := nodeGId;
      nodeG.parentId := nodeId;

      // recompute the AABB of node A and B
      nodeA.aabb.setMergeTwo(nodeC.aabb, nodeG.aabb);
      nodeB.aabb.setMergeTwo(nodeA.aabb, nodeF.aabb);

      // recompute the height of node A and B
      nodeA.height := dtMaxI(nodeC.height, nodeG.height)+1;
      nodeB.height := dtMaxI(nodeA.height, nodeF.height)+1;
      {$IFDEF aabbtree_many_asserts}assert(nodeA.height > 0);{$ENDIF}
      {$IFDEF aabbtree_many_asserts}assert(nodeB.height > 0);{$ENDIF}
    end
    else
    begin
      // if the left node B was higher than right node C because of node G
      nodeB.children[TTreeNode.Right] := nodeGId;
      nodeA.children[TTreeNode.Left] := nodeFId;
      nodeF.parentId := nodeId;

      // recompute the AABB of node A and B
      nodeA.aabb.setMergeTwo(nodeC.aabb, nodeF.aabb);
      nodeB.aabb.setMergeTwo(nodeA.aabb, nodeG.aabb);

      // recompute the height of node A and B
      nodeA.height := dtMaxI(nodeC.height, nodeF.height)+1;
      nodeB.height := dtMaxI(nodeA.height, nodeG.height)+1;
      {$IFDEF aabbtree_many_asserts}assert(nodeA.height > 0);{$ENDIF}
      {$IFDEF aabbtree_many_asserts}assert(nodeB.height > 0);{$ENDIF}
    end;

    // return the new root of the sub-tree
    result := nodeBId;
    exit;
  end;

  // if the sub-tree is balanced, return the current root node
  result := nodeId;
end;


// compute the height of a given node in the tree
function TDynAABBTreeBase.computeHeight (nodeId: Integer): Integer;
var
  node: PTreeNode;
  leftHeight, rightHeight: Integer;
begin
  {$IFDEF aabbtree_many_asserts}assert((nodeId >= 0) and (nodeId < mAllocCount));{$ENDIF}
  node := @mNodes[nodeId];

  // if the node is a leaf, its height is zero
  if (node.leaf) then begin result := 0; exit; end;

  // compute the height of the left and right sub-tree
  leftHeight := computeHeight(node.children[TTreeNode.Left]);
  rightHeight := computeHeight(node.children[TTreeNode.Right]);

  // return the height of the node
  result := 1+dtMaxI(leftHeight, rightHeight);
end;


// internally add an object into the tree
function TDynAABBTreeBase.insertObjectInternal (constref aabb: AABB2D; staticObject: Boolean): Integer;
var
  nodeId: Integer;
  node: PTreeNode;
begin
  // get the next available node (or allocate new ones if necessary)
  nodeId := allocateNode();

  node := @mNodes[nodeId];

  // create the fat aabb to use in the tree
  node.aabb := AABB2D.Create(aabb);
  if (not staticObject) then
  begin
    node.aabb.minX -= mExtraGap;
    node.aabb.minY -= mExtraGap;
    node.aabb.maxX += mExtraGap;
    node.aabb.maxY += mExtraGap;
  end;

  // set the height of the node in the tree
  node.height := 0;

  // insert the new leaf node in the tree
  insertLeafNode(nodeId);

  {$IFDEF aabbtree_many_asserts}node := @mNodes[nodeId];{$ENDIF}
  {$IFDEF aabbtree_many_asserts}assert(node.leaf);{$ENDIF}

  // return the id of the node
  result := nodeId;
end;


// initialize the tree
procedure TDynAABBTreeBase.setup ();
var
  i: Integer;
begin
  mRootNodeId := TTreeNode.NullTreeNode;
  mNodeCount := 0;
  mAllocCount := 8192;
  vstused := 0;

  SetLength(mNodes, mAllocCount);
  //memset(mNodes, 0, mAllocCount*TTreeNode.sizeof);
  for i := 0 to mAllocCount-1 do mNodes[i].clear();

  // initialize the allocated nodes
  for i := 0 to mAllocCount-1 do
  begin
    mNodes[i].nextNodeId := i+1;
    mNodes[i].height := -1;
  end;
  mNodes[mAllocCount-1].nextNodeId := TTreeNode.NullTreeNode;
  mFreeNodeId := 0;
end;


// also, checks if the tree structure is valid (for debugging purpose)
function TDynAABBTreeBase.forEachNode (nodeId: Integer; dg: TForEachLeafCB): Boolean;
var
  pNode: PTreeNode;
  leftChild, rightChild, height: Integer;
  aabb: AABB2D;
begin
  result := false;
  if (nodeId = TTreeNode.NullTreeNode) then exit;
  // if it is the root
  if (nodeId = mRootNodeId) then assert(mNodes[nodeId].parentId = TTreeNode.NullTreeNode);
  // get the children nodes
  pNode := @mNodes[nodeId];
  assert(pNode.height >= 0);
  if (not pNode.aabb.valid) then
  begin
    {$IFDEF aabbtree_use_floats}
    e_WriteLog(Format('AABB:(%f,%f)-(%f,%f); volume=%f; valid=%d; height=%d; leaf=%d', [pNode.aabb.minX, pNode.aabb.minY, pNode.aabb.maxX, pNode.aabb.maxY, pNode.aabb.volume, Integer(pNode.aabb.valid), pNode.height, Integer(pNode.leaf)]), MSG_NOTIFY);
    {$ELSE}
    e_WriteLog(Format('AABB:(%d,%d)-(%d,%d); volume=%d; valid=%d; height=%d; leaf=%d', [pNode.aabb.minX, pNode.aabb.minY, pNode.aabb.maxX, pNode.aabb.maxY, pNode.aabb.volume, Integer(pNode.aabb.valid), pNode.height, Integer(pNode.leaf)]), MSG_NOTIFY);
    {$ENDIF}
    if pNode.leaf then
    begin
      getFleshAABB(aabb, pNode.flesh, pNode.tag);
      {$IFDEF aabbtree_use_floats}
      e_WriteLog(Format('  LEAF AABB:(%f,%f)-(%f,%f); valid=%d; volume=%f', [aabb.minX, aabb.minY, aabb.maxX, aabb.maxY, Integer(aabb.valid), aabb.volume]), MSG_NOTIFY);
      {$ELSE}
      e_WriteLog(Format('  LEAF AABB:(%d,%d)-(%d,%d); valid=%d; volume=%d', [aabb.minX, aabb.minY, aabb.maxX, aabb.maxY, Integer(aabb.valid), aabb.volume]), MSG_NOTIFY);
      {$ENDIF}
    end;
  end;
  assert(pNode.aabb.valid);
  assert(pNode.aabb.volume > 0);
  // if the current node is a leaf
  if (pNode.leaf) then
  begin
    assert(pNode.height = 0);
    if assigned(dg) then result := dg(pNode.flesh, pNode.aabb);
  end
  else
  begin
    leftChild := pNode.children[TTreeNode.Left];
    rightChild := pNode.children[TTreeNode.Right];
    // check that the children node Ids are valid
    assert((0 <= leftChild) and (leftChild < mAllocCount));
    assert((0 <= rightChild) and (rightChild < mAllocCount));
    // check that the children nodes have the correct parent node
    assert(mNodes[leftChild].parentId = nodeId);
    assert(mNodes[rightChild].parentId = nodeId);
    // check the height of node
    height := 1+dtMaxI(mNodes[leftChild].height, mNodes[rightChild].height);
    assert(mNodes[nodeId].height = height);
    // check the AABB of the node
    aabb := AABB2D.Create(mNodes[leftChild].aabb, mNodes[rightChild].aabb);
    assert(aabb.minX = mNodes[nodeId].aabb.minX);
    assert(aabb.minY = mNodes[nodeId].aabb.minY);
    assert(aabb.maxX = mNodes[nodeId].aabb.maxX);
    assert(aabb.maxY = mNodes[nodeId].aabb.maxY);
    // recursively check the children nodes
    result := forEachNode(leftChild, dg);
    if not result then result := forEachNode(rightChild, dg);
  end;
end;


// also, checks if the tree structure is valid (for debugging purpose)
function TDynAABBTreeBase.forEachLeaf (dg: TForEachLeafCB): Boolean;
begin
  // recursively check each node
  result := forEachNode(mRootNodeId, dg);
end;


// return `true` from visitor to stop immediately
// checker should check if this node should be considered to further checking
// returns tree node if visitor says stop or -1
function TDynAABBTreeBase.visit (constref caabb: AABB2D; mode: Integer; checker: TVisitCheckerCB; visitor: TQueryOverlapCB; visdg: TQueryOverlapDg; tagmask: Integer): Integer;
const
  StackGran = 1024;
var
  oldvstused: Integer;
  vsp: Integer;
  vstk: array of Integer;
  nodeId: Integer;
  node: PTreeNode;
  doNode: Boolean = false;
begin
  if not assigned(checker) then begin result := -1; exit; end;
  //if not assigned(visitor) and not assigned(visdg) then raise Exception.Create('dyntree: empty visitors aren''t supported');
  oldvstused := vstused;
  if (vstused+StackGran > Length(vstack)) then SetLength(vstack, vstused+StackGran);
  vsp := vstused;
  vstk := vstack;

  {$IFDEF aabbtree_query_count}
  mNodesVisited := 0;
  mNodesDeepVisited := 0;
  {$ENDIF}

  // start from root node
  // we can't have nested functions in generics, sorry
  {$IF FALSE}
    spush(mRootNodeId);
  {$ELSE}
    if (vsp >= Length(vstk)) then SetLength(vstk, vsp+StackGran);
    vstk[vsp] := mRootNodeId;
    Inc(vsp);
  {$ENDIF}

  // while there are still nodes to visit
  while (vsp > oldvstused) do
  begin
    // get the next node id to visit
    // we can't have nested functions in generics, sorry
    {$IF FALSE}
      nodeId := spop();
    {$ELSE}
      Dec(vsp);
      nodeId := vstk[vsp];
    {$ENDIF}
    // skip it if it is a nil node
    if (nodeId = TTreeNode.NullTreeNode) then continue;
    {$IFDEF aabbtree_query_count}Inc(mNodesVisited);{$ENDIF}
    // get the corresponding node
    node := @mNodes[nodeId];
    // should we investigate this node?
    case mode of
      ModeNoChecks: doNode := checker(node);
      ModeAABB:
        begin
          //doNode := caabb.overlaps(node.aabb);
          // this gives small speedup (or not...)
          // exit with no intersection if found separated along any axis
               if (caabb.maxX < node.aabb.minX) or (caabb.minX > node.aabb.maxX) then doNode := false
          else if (caabb.maxY < node.aabb.minY) or (caabb.minY > node.aabb.maxY) then doNode := false
          else doNode := true;
        end;
      ModePoint:
        begin
          //doNode := node.aabb.contains(caabb.minX, caabb.minY);
          // this gives small speedup
          doNode := (caabb.minX >= node.aabb.minX) and (caabb.minY >= node.aabb.minY) and (caabb.minX <= node.aabb.maxX) and (caabb.minY <= node.aabb.maxY);
        end;
    end;
    if doNode then
    begin
      // if the node is a leaf
      if (node.leaf) then
      begin
        // call visitor on it
        {$IFDEF aabbtree_query_count}Inc(mNodesDeepVisited);{$ENDIF}
        if (tagmask = -1) or ((node.tag and tagmask) <> 0) then
        begin
          doNode := false;
          // update object vars from cache, so recursive calls to `visit()` will work
          vstack := vstk;
          vstused := vsp;
          // call callbacks
          if assigned(visitor) then doNode := visitor(node.flesh, node.tag);
          if assigned(visdg) and visdg(node.flesh, node.tag) then doNode := true;
          // do some sanity checks
          if (vstused <> vsp) then raise Exception.Create('internal error in dyntree visitor');
          // should we exit?
          if doNode then
          begin
            result := nodeId;
            vstack := vstk;
            vstused := oldvstused;
            exit;
          end;
        end;
      end
      else
      begin
        // if the node is not a leaf, we need to visit its children
        // we can't have nested functions in generics, sorry
        {$IF FALSE}
          spush(node.children[TTreeNode.Left]);
          spush(node.children[TTreeNode.Right]);
        {$ELSE}
          if (vsp+2 > Length(vstk)) then SetLength(vstk, vsp+StackGran);
          vstk[vsp] := node.children[TTreeNode.Left];
          Inc(vsp);
          vstk[vsp] := node.children[TTreeNode.Right];
          Inc(vsp);
        {$ENDIF}
      end;
    end;
  end;

  result := -1; // oops
  vstack := vstk;
  vstused := oldvstused;
end;


// add `extraAABBGap` to bounding boxes so slight object movement won't cause tree rebuilds
// extra AABB Gap used to allow the collision shape to move a little bit without triggering a large modification of the tree which can be costly
constructor TDynAABBTreeBase.Create (extraAABBGap: TreeNumber=0);
begin
  mExtraGap := extraAABBGap;
  mNodes := nil;
  SetLength(vstack, 2048);
  vstused := 0;
  setup();
end;


destructor TDynAABBTreeBase.Destroy ();
begin
  mNodes := nil;
  vstack := nil;
  inherited;
end;


// clear all the nodes and reset the tree
procedure TDynAABBTreeBase.reset ();
begin
  mNodes := nil;
  setup();
end;


function TDynAABBTreeBase.computeTreeHeight (): Integer; begin result := computeHeight(mRootNodeId); end;


// return the root AABB of the tree
procedure TDynAABBTreeBase.getRootAABB (out aabb: AABB2D);
begin
  {$IFDEF aabbtree_many_asserts}assert((mRootNodeId >= 0) and (mRootNodeId < mAllocCount));{$ENDIF}
  aabb := mNodes[mRootNodeId].aabb;
end;


// does the given id represents a valid object?
// WARNING: ids of removed objects can be reused on later insertions!
function TDynAABBTreeBase.isValidId (id: Integer): Boolean;
begin
  result := (id >= 0) and (id < mAllocCount) and (mNodes[id].leaf);
end;


// get object by nodeid; can return nil for invalid ids
function TDynAABBTreeBase.getNodeObjectId (nodeid: Integer): TTreeFlesh;
begin
  if (nodeid >= 0) and (nodeid < mAllocCount) and (mNodes[nodeid].leaf) then result := mNodes[nodeid].flesh else result := Default(ITP);
end;

// get fat object AABB by nodeid; returns random shit for invalid ids
procedure TDynAABBTreeBase.getNodeFatAABB (out aabb: AABB2D; nodeid: Integer);
begin
  if (nodeid >= 0) and (nodeid < mAllocCount) and (not mNodes[nodeid].isfree) then aabb := AABB2D.Create(mNodes[nodeid].aabb) else aabb := AABB2D.Create(0, 0, 0, 0);
end;

function TDynAABBTreeBase.getNodeXY (nodeid: Integer; out x, y: Integer): Boolean; inline;
begin
  if (nodeid >= 0) and (nodeid < mAllocCount) and (mNodes[nodeid].leaf) then
  begin
    result := true;
    {$IFDEF aabbtree_use_floats}
    x := round(mNodes[nodeid].fleshX);
    y := round(mNodes[nodeid].fleshY);
    {$ELSE}
    x := mNodes[nodeid].fleshX;
    y := mNodes[nodeid].fleshY;
    {$ENDIF}
  end
  else
  begin
    result := false;
    x := 0;
    y := 0;
    //if (nodeid >= 0) and (nodeid < mAllocCount) then mNodes[nodeid].dumpToLog();
  end;
end;


// insert an object into the tree
// this method creates a new leaf node in the tree and returns the id of the corresponding node or -1 on error
// AABB for static object will not be "fat" (simple optimization)
// WARNING! inserting the same object several times *WILL* break everything!
function TDynAABBTreeBase.insertObject (flesh: TTreeFlesh; tag: Integer; staticObject: Boolean=false): Integer;
var
  aabb: AABB2D;
  nodeId, fx, fy: Integer;
begin
  if not getFleshAABB(aabb, flesh, tag) then
  begin
    {$IFDEF aabbtree_use_floats}
    e_WriteLog(Format('trying to insert FUCKED FLESH:(%f,%f)-(%f,%f); volume=%f; valid=%d', [aabb.minX, aabb.minY, aabb.maxX, aabb.maxY, aabb.volume, Integer(aabb.valid)]), MSG_WARNING);
    {$ELSE}
    e_WriteLog(Format('trying to insert FUCKED FLESH:(%d,%d)-(%d,%d); volume=%d; valid=%d', [aabb.minX, aabb.minY, aabb.maxX, aabb.maxY, aabb.volume, Integer(aabb.valid)]), MSG_WARNING);
    {$ENDIF}
    //raise Exception.Create('trying to insert invalid flesh in dyntree');
    result := -1;
    exit;
  end;
  if not aabb.valid then
  begin
    {$IFDEF aabbtree_use_floats}
    e_WriteLog(Format('trying to insert FUCKED AABB:(%f,%f)-(%f,%f); volume=%f; valid=%d', [aabb.minX, aabb.minY, aabb.maxX, aabb.maxY, aabb.volume, Integer(aabb.valid)]), MSG_WARNING);
    {$ELSE}
    e_WriteLog(Format('trying to insert FUCKED AABB:(%d,%d)-(%d,%d); volume=%d; valid=%d', [aabb.minX, aabb.minY, aabb.maxX, aabb.maxY, aabb.volume, Integer(aabb.valid)]), MSG_WARNING);
    {$ENDIF}
    raise Exception.Create('trying to insert invalid aabb in dyntree');
    result := -1;
    exit;
  end;
  //e_WriteLog(Format('inserting AABB:(%f,%f)-(%f,%f); volume=%f; valid=%d', [aabb.minX, aabb.minY, aabb.maxX, aabb.maxY, aabb.volume, Integer(aabb.valid)]), MSG_NOTIFY);
  fx := aabb.minX;
  fy := aabb.minY;
  nodeId := insertObjectInternal(aabb, staticObject);
  {$IFDEF aabbtree_many_asserts}assert(mNodes[nodeId].leaf);{$ENDIF}
  mNodes[nodeId].flesh := flesh;
  mNodes[nodeId].tag := tag;
  mNodes[nodeId].fleshX := fx;
  mNodes[nodeId].fleshY := fy;
  result := nodeId;
end;


// remove an object from the tree
// WARNING: ids of removed objects can be reused on later insertions!
procedure TDynAABBTreeBase.removeObject (nodeId: Integer);
begin
  if (nodeId < 0) or (nodeId >= mAllocCount) or (not mNodes[nodeId].leaf) then raise Exception.Create('invalid node id in TDynAABBTreeBase');
  // remove the node from the tree
  removeLeafNode(nodeId);
  releaseNode(nodeId);
end;


function TDynAABBTreeBase.updateObject (nodeId: Integer; forceReinsert: Boolean=false): Boolean; overload;
var
  newAABB: AABB2D;
  dispX, dispY: TreeNumber;
begin
  if (nodeId < 0) or (nodeId >= mAllocCount) or (not mNodes[nodeId].leaf) then raise Exception.Create('invalid node id in TDynAABBTreeBase.updateObject');

  if not getFleshAABB(newAABB, mNodes[nodeId].flesh, mNodes[nodeId].tag) then raise Exception.Create('invalid flesh dimensions in TDynAABBTreeBase.updateObject');
  if not newAABB.valid then raise Exception.Create('invalid flesh aabb in TDynAABBTreeBase.updateObject');

  dispX := newAABB.minX-mNodes[nodeId].fleshX;
  dispY := newAABB.minY-mNodes[nodeId].fleshY;

  if (dispX < -16) then dispX := -16 else if (dispX > 16) then dispX := 16;
  if (dispY < -16) then dispY := -16 else if (dispY > 16) then dispY := 16;

  result := updateObject(nodeId, dispX, dispY, forceReinsert);
end;

function TDynAABBTreeBase.updateObject (nodeId: Integer; dispX, dispY: TreeNumber; forceReinsert: Boolean=false): Boolean; overload;
var
  newAABB: AABB2D;
  fx, fy: Integer;
  node: PTreeNode;
begin
  if (nodeId < 0) or (nodeId >= mAllocCount) or (not mNodes[nodeId].leaf) then raise Exception.Create('invalid node id in TDynAABBTreeBase.updateObject');

  if not getFleshAABB(newAABB, mNodes[nodeId].flesh, mNodes[nodeId].tag) then raise Exception.Create('invalid flesh dimensions in TDynAABBTreeBase.updateObject');
  if not newAABB.valid then raise Exception.Create('invalid flesh aabb in TDynAABBTreeBase.updateObject');

  fx := newAABB.minX;
  fy := newAABB.minY;

  // if the new AABB is still inside the fat AABB of the node
  if (not forceReinsert) and (mNodes[nodeId].aabb.contains(newAABB)) then
  begin
    node := @mNodes[nodeId];
    node.fleshX := fx;
    node.fleshY := fy;
    result := false;
    exit;
  end;

  // if the new AABB is outside the fat AABB, we remove the corresponding node
  removeLeafNode(nodeId);

  node := @mNodes[nodeId];

  // compute the fat AABB by inflating the AABB with a constant gap
  node.aabb.copyFrom(newAABB);
  node.fleshX := fx;
  node.fleshY := fy;

  if (not forceReinsert) and ((dispX <> 0) or (dispY <> 0)) then
  begin
    node.aabb.minX -= mExtraGap;
    node.aabb.minY += mExtraGap;
    node.aabb.maxX += mExtraGap;
    node.aabb.maxY += mExtraGap;
  end;

  // inflate the fat AABB in direction of the linear motion of the AABB
  if (dispX < 0) then
  begin
    node.aabb.minX += LinearMotionGapMultiplier*dispX {$IFDEF aabbtree_use_floats}{$ELSE}div 10{$ENDIF};
  end
  else
  begin
    node.aabb.maxX += LinearMotionGapMultiplier*dispX {$IFDEF aabbtree_use_floats}{$ELSE}div 10{$ENDIF};
  end;

  if (dispY < 0) then
  begin
    node.aabb.minY += LinearMotionGapMultiplier*dispY {$IFDEF aabbtree_use_floats}{$ELSE}div 10{$ENDIF};
  end
  else
  begin
    node.aabb.maxY += LinearMotionGapMultiplier*dispY {$IFDEF aabbtree_use_floats}{$ELSE}div 10{$ENDIF};
  end;

  {$IFDEF aabbtree_many_asserts}assert(node.aabb.contains(newAABB));{$ENDIF}

  // reinsert the node into the tree
  insertLeafNode(nodeId);

  result := true;
end;


function TDynAABBTreeBase.checkerAABB (node: PTreeNode): Boolean;
begin
  result := chkAABB.overlaps(node.aabb);
end;


// report all shapes overlapping with the AABB given in parameter
function TDynAABBTreeBase.aabbQuery (ax, ay, aw, ah: TreeNumber; cb: TQueryOverlapCB; tagmask: Integer=-1): TTreeFlesh;
var
  nid: Integer;
  oldaabb: AABB2D;
begin
  result := Default(ITP);
  if not assigned(cb) then exit;
  if (aw < 1) or (ah < 1) then exit;
  //chkAABB := AABB2D.Create(ax, ay, ax+aw, ay+ah);
  oldaabb := chkAABB;
  chkAABB.minX := ax;
  chkAABB.minY := ay;
  chkAABB.maxX := ax+aw;
  chkAABB.maxY := ay+ah;
  nid := visit(chkAABB, ModeAABB, checkerAABB, cb, nil, tagmask);
  chkAABB := oldaabb;
  if (nid >= 0) then result := mNodes[nid].flesh else result := Default(ITP);
end;


function TDynAABBTreeBase.checkerPoint (node: PTreeNode): Boolean;
begin
  result := node.aabb.contains(chkAABB.minX, chkAABB.minY);
end;


// report body that contains the given point, or nil
function TDynAABBTreeBase.pointQuery (ax, ay: TreeNumber; cb: TQueryOverlapCB; tagmask: Integer=-1): TTreeFlesh;
var
  nid: Integer;
  oldaabb: AABB2D;
begin
  oldaabb := chkAABB;
  chkAABB := AABB2D.Create(ax, ay, ax+1, ay+1);
  nid := visit(chkAABB, ModePoint, checkerPoint, cb, nil, tagmask);
  {$IFDEF aabbtree_many_asserts}assert((nid < 0) or ((nid >= 0) and (nid < mAllocCount) and (mNodes[nid].leaf)));{$ENDIF}
  chkAABB := oldaabb;
  if (nid >= 0) then result := mNodes[nid].flesh else result := Default(ITP);
end;


function TDynAABBTreeBase.checkerRay (node: PTreeNode): Boolean;
var
  tmin: Single = 0;
begin
  {$IF FALSE}
  result := node.aabb.intersects(curax, curay, curbx, curby, @tmin);
  e_WriteLog(Format('intersect: (%f,%f)-(%f,%f)  (%d,%d)-(%d,%d) tmin=%f  res=%d', [
    minSingle(curax, curbx),
    minSingle(curay, curby),
    maxSingle(curax, curbx),
    maxSingle(curay, curby),
    node.aabb.minX, node.aabb.minY,
    node.aabb.maxX, node.aabb.maxY,
    tmin,
    Integer(result),
  ]), MSG_NOTIFY);
  {$ELSE}
  result := node.aabb.intersects(traceRay, maxFraction, @tmin);
  {
  e_WriteLog(Format('intersect: (%f,%f)-(%f,%f)  (%d,%d)-(%d,%d) tmin=%f  res=%d  frac=%f', [
    curax, curay, curbx, curby,
    node.aabb.minX, node.aabb.minY,
    node.aabb.maxX, node.aabb.maxY,
    tmin,
    Integer(result),
    maxFraction
  ]), MSG_NOTIFY);
  }
  {$ENDIF}
end;


function TDynAABBTreeBase.visitorRay (flesh: TTreeFlesh; tag: Integer): Boolean;
var
  hitFraction: Single;
  ray: Ray2D;
begin
  ray.origX := curax;
  ray.origY := curay;
  ray.dirX := dirx;
  ray.dirY := diry;
  hitFraction := sqcb(flesh, ray);
  // if the user returned a hitFraction of zero, it means that the raycasting should stop here
  if (hitFraction = 0.0) then
  begin
    qSRes.time := 0;
    qSRes.flesh := flesh;
    result := true;
    exit;
  end;
  // if the user returned a positive fraction
  if (hitFraction > 0.0) then
  begin
    // we update the maxFraction value and the ray AABB using the new maximum fraction
    if (hitFraction < maxFraction) then
    begin
      maxFraction := hitFraction;
      qSRes.time := hitFraction;
      qSRes.flesh := flesh;
      // fix curb here
      //curb := cura+dir*hitFraction;
      curbx := curax+dirx*hitFraction;
      curby := curay+diry*hitFraction;
    end;
  end;
  result := false; // continue
end;


// segment querying method
function TDynAABBTreeBase.segmentQuery (out qr: TSegmentQueryResult; ax, ay, bx, by: TreeNumber; cb: TSegQueryCallback; tagmask: Integer=-1): Boolean;
var
  oldmaxFraction: Single;
  oldcurax, oldcuray: Single;
  oldcurbx, oldcurby: Single;
  olddirx, olddiry: Single;
  invlen: Single;
  osres: PSegmentQueryResult;
  osqcb: TSegQueryCallback;
  oldray: Ray2D;
begin
  qr := TSegmentQueryResult.Create(false);

  if (ax = bx) and (ay = by) then begin result := false; exit; end;

  oldmaxFraction := maxFraction;
  oldcurax := curax;
  oldcuray := curay;
  oldcurbx := curbx;
  oldcurby := curby;
  olddirx := dirx;
  olddiry := diry;
  oldray := traceRay;

  maxFraction := 1.0e100; // infinity
  curax := ax;
  curay := ay;
  curbx := bx;
  curby := by;

  dirx := curbx-curax;
  diry := curby-curay;
  // normalize
  invlen := 1.0/sqrt(dirx*dirx+diry*diry);
  dirx *= invlen;
  diry *= invlen;

  traceRay.origX := curax;
  traceRay.origY := curay;
  traceRay.dirX := dirx;
  traceRay.dirY := diry;

  //chkAABB := AABB2D.Create(0, 0, 1, 1);
  osres := qSRes;
  qSRes := @qr;
  osqcb := sqcb;
  sqcb := cb;
  visit(chkAABB, ModeNoChecks, checkerRay, nil, visitorRay, tagmask);
  qSRes := osres;
  sqcb := osqcb;

  curax := oldcurax;
  curay := oldcuray;
  curbx := oldcurbx;
  curby := oldcurby;
  dirx := olddirx;
  diry := olddiry;
  maxFraction := oldmaxFraction;
  traceRay := oldray;

  result := qr.valid;
end;


end.
