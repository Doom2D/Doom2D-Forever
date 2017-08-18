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
{$DEFINE aabbtree_many_asserts}
{$DEFINE aabbtree_query_count}
unit z_aabbtree;

interface

uses e_log;


// ////////////////////////////////////////////////////////////////////////// //
type
  Float = Single;
  PFloat = ^Float;

  TTreeFlesh = TObject;


// ////////////////////////////////////////////////////////////////////////// //
type
  Ray2D = record
  public
    origX, origY: Float;
    dirX, dirY: Float;

  public
    constructor Create (ax, ay: Float; aangle: Float); overload;
    constructor Create (ax0, ay0, ax1, ay1: Float); overload;
    constructor Create (const aray: Ray2D); overload;

    procedure copyFrom (const aray: Ray2D); inline;

    procedure normalizeDir (); inline;

    procedure setXYAngle (ax, ay: Float; aangle: Float); inline;
    procedure setX0Y0X1Y1 (ax0, ay0, ax1, ay1: Float); inline;
  end;

// ////////////////////////////////////////////////////////////////////////// //
type
  AABB2D = record
  public
    minX, minY, maxX, maxY: Float;

  private
    function getvalid (): Boolean; inline;
    function getcenterX (): Float; inline;
    function getcenterY (): Float; inline;
    function getextentX (): Float; inline;
    function getextentY (): Float; inline;

  public
    constructor Create (x0, y0, x1, y1: Float); overload;
    constructor Create (const aabb: AABB2D); overload;
    constructor Create (const aabb0, aabb1: AABB2D); overload;

    procedure copyFrom (const aabb: AABB2D); inline;
    procedure setDims (x0, y0, x1, y1: Float); inline;

    procedure setMergeTwo (const aabb0, aabb1: AABB2D); inline;

    function volume (): Float; inline;

    procedure merge (const aabb: AABB2D); inline;

    // return true if the current AABB contains the AABB given in parameter
    function contains (const aabb: AABB2D): Boolean; inline; overload;
    function contains (ax, ay: Float): Boolean; inline; overload;

    // return true if the current AABB is overlapping with the AABB in parameter
    // two AABBs overlap if they overlap in the two axes at the same time
    function overlaps (const aabb: AABB2D): Boolean; inline; overload;

    // ray direction must be normalized
    function intersects (const ray: Ray2D; tmino: PFloat=nil; tmaxo: PFloat=nil): Boolean; overload;
    function intersects (ax, ay, bx, by: Float): Boolean; inline; overload;

    property valid: Boolean read getvalid;
    property centerX: Float read getcenterX;
    property centerY: Float read getcenterY;
    property extentX: Float read getextentX;
    property extentY: Float read getextentY;
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
  TDynAABBTree = class(TObject)
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
        //TODO: `flesh` can be united with `children`
        flesh: TTreeFlesh;
        // height of the node in the tree (-1 for free nodes)
        height: SmallInt;
        // fat axis aligned bounding box (AABB) corresponding to the node
        aabb: AABB2D;
      public
        // return true if the node is a leaf of the tree
        procedure clear (); inline;
        function leaf (): Boolean; inline;
        function free (): Boolean; inline;
        property nextNodeId: Integer read parentId write parentId;
        //property flesh: Integer read children[0] write children[0];
      end;

      TVisitCheckerCB = function (node: PTreeNode): Boolean is nested;
      TVisitVisitorCB = function (abody: TTreeFlesh): Boolean is nested;

  public
    // return `true` to stop
    type TForEachLeafCB = function (abody: TTreeFlesh; const aabb: AABB2D): Boolean is nested; // WARNING! don't modify AABB here!

  public
    // in the broad-phase collision detection (dynamic AABB tree), the AABBs are
    // also inflated in direction of the linear motion of the body by mutliplying the
    // followin constant with the linear velocity and the elapsed time between two frames
    const LinearMotionGapMultiplier = 1.7;

  private
    mNodes: array of TTreeNode; // nodes of the tree
    mRootNodeId: Integer; // id of the root node of the tree
    mFreeNodeId: Integer; // id of the first node of the list of free (allocated) nodes in the tree that we can use
    mAllocCount: Integer; // number of allocated nodes in the tree
    mNodeCount: Integer; // number of nodes in the tree

    // extra AABB Gap used to allow the collision shape to move a little bit
    // without triggering a large modification of the tree which can be costly
    mExtraGap: Float;

  private
    function allocateNode (): Integer;
    procedure releaseNode (nodeId: Integer);
    procedure insertLeafNode (nodeId: Integer);
    procedure removeLeafNode (nodeId: Integer);
    function balanceSubTreeAtNode (nodeId: Integer): Integer;
    function computeHeight (nodeId: Integer): Integer;
    function insertObjectInternal (var aabb: AABB2D; staticObject: Boolean): Integer;
    procedure setup ();
    function visit (checker: TVisitCheckerCB; visitor: TVisitVisitorCB): Integer;

  public
    {$IFDEF aabbtree_query_count}
    nodesVisited, nodesDeepVisited: Integer;
    {$ENDIF}

  public
    // called when a overlapping node has been found during the call to forEachAABBOverlap()
    // return `true` to stop
    type TQueryOverlapCB = function (abody: TTreeFlesh): Boolean is nested;
    type TSegQueryCallback = function (abody: TTreeFlesh; ax, ay, bx, by: Float): Float is nested; // return dist from (ax,ay) to abody

    TSegmentQueryResult = record
      dist: Float; // <0: nothing was hit
      flesh: TTreeFlesh;

      procedure reset (); inline;
      function valid (): Boolean; inline;
    end;

  public
    constructor Create (extraAABBGap: Float=0.0);
    destructor Destroy (); override;

    // clear all the nodes and reset the tree
    procedure reset ();

    function forEachLeaf (dg: TForEachLeafCB): Boolean; // WARNING! don't modify AABB/tree here!
    procedure getRootAABB (var aabb: AABB2D);

    function isValidId (id: Integer): Boolean; inline;
    function getNodeObjectId (nodeid: Integer): TTreeFlesh; inline;
    procedure getNodeFatAABB (var aabb: AABB2D; nodeid: Integer); inline;

    // return `false` for invalid flesh
    function getFleshAABB (var aabb: AABB2D; flesh: TTreeFlesh): Boolean; virtual; abstract;

    // insert an object into the tree
    // this method creates a new leaf node in the tree and returns the id of the corresponding node or -1 on error
    // AABB for static object will not be "fat" (simple optimization)
    // WARNING! inserting the same object several times *WILL* break everything!
    function insertObject (flesh: TTreeFlesh; staticObject: Boolean=false): Integer;

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
    function updateObject (nodeId: Integer; dispX, dispY: Float; forceReinsert: Boolean=false): Boolean;

    procedure aabbQuery (ax, ay, aw, ah: Float; cb: TQueryOverlapCB);
    function pointQuery (ax, ay: Float; cb: TQueryOverlapCB): TTreeFlesh;
    function segmentQuery (var qr: TSegmentQueryResult; ax, ay, bx, by: Float; cb: TSegQueryCallback): Boolean;

    function computeTreeHeight (): Integer; // compute the height of the tree

    property extraGap: Float read mExtraGap write mExtraGap;
    property nodeCount: Integer read mNodeCount;
    property nodeAlloced: Integer read mAllocCount;
  end;


implementation

uses
  SysUtils;


// ////////////////////////////////////////////////////////////////////////// //
function minI (a, b: Integer): Integer; inline; begin if (a < b) then result := a else result := b; end;
function maxI (a, b: Integer): Integer; inline; begin if (a > b) then result := a else result := b; end;

function minF (a, b: Float): Float; inline; begin if (a < b) then result := a else result := b; end;
function maxF (a, b: Float): Float; inline; begin if (a > b) then result := a else result := b; end;


// ////////////////////////////////////////////////////////////////////////// //
constructor Ray2D.Create (ax, ay: Float; aangle: Float); begin setXYAngle(ax, ay, aangle); end;
constructor Ray2D.Create (ax0, ay0, ax1, ay1: Float); begin setX0Y0X1Y1(ax0, ay0, ax1, ay1); end;
constructor Ray2D.Create (const aray: Ray2D); overload; begin copyFrom(aray); end;


procedure Ray2D.copyFrom (const aray: Ray2D); inline;
begin
  origX := aray.origX;
  origY := aray.origY;
  dirX := aray.dirX;
  dirY := aray.dirY;
end;

procedure Ray2D.normalizeDir (); inline;
var
  invlen: Float;
begin
  invlen := 1.0/sqrt(dirX*dirX+dirY*dirY);
  dirX *= invlen;
  dirY *= invlen;
end;

procedure Ray2D.setXYAngle (ax, ay: Float; aangle: Float); inline;
begin
  origX := ax;
  origY := ay;
  dirX := cos(aangle);
  dirY := sin(aangle);
end;

procedure Ray2D.setX0Y0X1Y1 (ax0, ay0, ax1, ay1: Float); inline;
begin
  origX := ax0;
  origY := ay0;
  dirX := ax1-ax0;
  dirY := ay1-ay0;
  normalizeDir();
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor AABB2D.Create (x0, y0, x1, y1: Float); overload;
begin
  setDims(x0, y0, x1, y1);
end;

constructor AABB2D.Create (const aabb: AABB2D); overload;
begin
  copyFrom(aabb);
end;

constructor AABB2D.Create (const aabb0, aabb1: AABB2D); overload;
begin
  setMergeTwo(aabb0, aabb1);
end;

function AABB2D.getvalid (): Boolean; inline; begin result := (minX < maxX) and (minY < maxY); end;

function AABB2D.getcenterX (): Float; inline; begin result := (minX+maxX)/2; end;
function AABB2D.getcenterY (): Float; inline; begin result := (minY+maxY)/2; end;
function AABB2D.getextentX (): Float; inline; begin result := (maxX-minX)+1; end;
function AABB2D.getextentY (): Float; inline; begin result := (maxY-minY)+1; end;


procedure AABB2D.copyFrom (const aabb: AABB2D); inline;
begin
  minX := aabb.minX;
  minY := aabb.minY;
  maxX := aabb.maxX;
  maxY := aabb.maxY;
end;


procedure AABB2D.setDims (x0, y0, x1, y1: Float); inline;
begin
  minX := minF(x0, x1);
  minY := minF(y0, y1);
  maxX := maxF(x0, x1);
  maxY := maxF(y0, y1);
end;


procedure AABB2D.setMergeTwo (const aabb0, aabb1: AABB2D); inline;
begin
  minX := minF(aabb0.minX, aabb1.minX);
  minY := minF(aabb0.minY, aabb1.minY);
  maxX := maxF(aabb0.maxX, aabb1.maxX);
  maxY := maxF(aabb0.maxY, aabb1.maxY);
end;


function AABB2D.volume (): Float; inline;
begin
  result := (maxX-minX)*(maxY-minY);
end;


procedure AABB2D.merge (const aabb: AABB2D); inline;
begin
  minX := minF(minX, aabb.minX);
  minY := minF(minY, aabb.minY);
  maxX := maxF(maxX, aabb.maxX);
  maxY := maxF(maxY, aabb.maxY);
end;


function AABB2D.contains (const aabb: AABB2D): Boolean; inline; overload;
begin
  result :=
    (aabb.minX >= minX) and (aabb.minY >= minY) and
    (aabb.maxX <= maxX) and (aabb.maxY <= maxY);
end;


function AABB2D.contains (ax, ay: Float): Boolean; inline; overload;
begin
  result := (ax >= minX) and (ay >= minY) and (ax <= maxX) and (ay <= maxY);
end;


function AABB2D.overlaps (const aabb: AABB2D): Boolean; inline; overload;
begin
  result := false;
  // exit with no intersection if found separated along any axis
  if (maxX < aabb.minX) or (minX > aabb.maxX) then exit;
  if (maxY < aabb.minY) or (minY > aabb.maxY) then exit;
  result := true;
end;


// something to consider here is that 0 * inf =nan which occurs when the ray starts exactly on the edge of a box
// https://tavianator.com/fast-branchless-raybounding-box-intersections-part-2-nans/
function AABB2D.intersects (const ray: Ray2D; tmino: PFloat=nil; tmaxo: PFloat=nil): Boolean; overload;
var
  dinv, t1, t2, tmp: Float;
  tmin, tmax: Float;
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

function AABB2D.intersects (ax, ay, bx, by: Float): Boolean; inline; overload;
var
  tmin: Float;
  ray: Ray2D;
begin
  result := true;
  // it may be faster to first check if start or end point is inside AABB (this is sometimes enough for dyntree)
  if (ax >= minX) and (ay >= minY) and (ax <= maxX) and (ay <= maxY) then exit; // a
  if (bx >= minX) and (by >= minY) and (bx <= maxX) and (by <= maxY) then exit; // b
  // nope, do it hard way
  ray := Ray2D.Create(ax, ay, bx, by);
  if not intersects(ray, @tmin) then begin result := false; exit; end;
  if (tmin < 0) then exit; // inside, just in case
  bx := bx-ax;
  by := by-ay;
  result := (tmin*tmin <= bx*bx+by*by);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TDynAABBTree.TSegmentQueryResult.reset (); inline; begin dist := -1; flesh := nil; end;
function TDynAABBTree.TSegmentQueryResult.valid (): Boolean; inline; begin result := (dist >= 0) and (flesh <> nil); end;


// ////////////////////////////////////////////////////////////////////////// //
function TDynAABBTree.TTreeNode.leaf (): Boolean; inline; begin result := (height = 0); end;
function TDynAABBTree.TTreeNode.free (): Boolean; inline; begin result := (height = -1); end;

procedure TDynAABBTree.TTreeNode.clear (); inline;
begin
  parentId := 0;
  children[0] := 0;
  children[1] := 0;
  flesh := nil;
  height := 0;
  //aabb.setX0Y0X1Y1(0, 0, 0, 0);
  aabb.minX := 0;
  aabb.minY := 0;
  aabb.maxX := -1;
  aabb.maxY := -1;
end;


// ////////////////////////////////////////////////////////////////////////// //
// allocate and return a node to use in the tree
function TDynAABBTree.allocateNode (): Integer;
var
  i, newsz, freeNodeId: Integer;
  node: PTreeNode;
begin
  // if there is no more allocated node to use
  if (mFreeNodeId = TTreeNode.NullTreeNode) then
  begin
    {$IFDEF aabbtree_many_asserts}assert(mNodeCount = mAllocCount);{$ENDIF}
    // allocate more nodes in the tree
    if (mAllocCount < 8192) then newsz := mAllocCount*2 else newsz := mAllocCount+8192;
    SetLength(mNodes, newsz);
    mAllocCount := newsz;
    // initialize the allocated nodes
    for i := mNodeCount to mAllocCount-2 do
    begin
      mNodes[i].nextNodeId := i+1;
      mNodes[i].height := -1;
    end;
    mNodes[mAllocCount-1].nextNodeId := TTreeNode.NullTreeNode;
    mNodes[mAllocCount-1].height := -1;
    mFreeNodeId := mNodeCount;
  end;
  // get the next free node
  freeNodeId := mFreeNodeId;
  {$IFDEF aabbtree_many_asserts}assert((freeNodeId >= mNodeCount) and (freeNodeId < mAllocCount));{$ENDIF}
  node := @mNodes[freeNodeId];
  mFreeNodeId := node.nextNodeId;
  node.clear();
  node.parentId := TTreeNode.NullTreeNode;
  node.height := 0;
  Inc(mNodeCount);
  result := freeNodeId;
end;


// release a node
procedure TDynAABBTree.releaseNode (nodeId: Integer);
begin
  {$IFDEF aabbtree_many_asserts}assert(mNodeCount > 0);{$ENDIF}
  {$IFDEF aabbtree_many_asserts}assert((nodeId >= 0) and (nodeId < mAllocCount));{$ENDIF}
  {$IFDEF aabbtree_many_asserts}assert(mNodes[nodeId].height >= 0);{$ENDIF}
  mNodes[nodeId].nextNodeId := mFreeNodeId;
  mNodes[nodeId].height := -1;
  mFreeNodeId := nodeId;
  Dec(mNodeCount);
end;


// insert a leaf node in the tree
// the process of inserting a new leaf node in the dynamic tree is described in the book "Introduction to Game Physics with Box2D" by Ian Parberry
procedure TDynAABBTree.insertLeafNode (nodeId: Integer);
var
  newNodeAABB, mergedAABBs, currentAndLeftAABB, currentAndRightAABB: AABB2D;
  currentNodeId: Integer;
  leftChild, rightChild, siblingNode: Integer;
  oldParentNode, newParentNode: Integer;
  volumeAABB, mergedVolume: Float;
  costS, costI, costLeft, costRight: Float;
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
  newNodeAABB := mNodes[nodeId].aabb;
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
    costS := 2.0*mergedVolume;

    // compute the minimum cost of pushing the new node further down the tree (inheritance cost)
    costI := 2.0*(mergedVolume-volumeAABB);

    // compute the cost of descending into the left child
    currentAndLeftAABB := AABB2D.Create(newNodeAABB, mNodes[leftChild].aabb);
    costLeft := currentAndLeftAABB.volume+costI;
    if not mNodes[leftChild].leaf then costLeft := costLeft-mNodes[leftChild].aabb.volume;

    // compute the cost of descending into the right child
    currentAndRightAABB := AABB2D.Create(newNodeAABB, mNodes[rightChild].aabb);
    costRight := currentAndRightAABB.volume+costI;
    if not mNodes[rightChild].leaf then costRight := costRight-mNodes[rightChild].aabb.volume;

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
    mNodes[currentNodeId].height := maxI(mNodes[leftChild].height, mNodes[rightChild].height)+1;
    {$IFDEF aabbtree_many_asserts}assert(mNodes[currentNodeId].height > 0);{$ENDIF}

    // recompute the AABB of the node
    mNodes[currentNodeId].aabb.setMergeTwo(mNodes[leftChild].aabb, mNodes[rightChild].aabb);

    currentNodeId := mNodes[currentNodeId].parentId;
  end;

  {$IFDEF aabbtree_many_asserts}assert(mNodes[nodeId].leaf);{$ENDIF}
end;


// remove a leaf node from the tree
procedure TDynAABBTree.removeLeafNode (nodeId: Integer);
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
      mNodes[currentNodeId].height := maxI(mNodes[leftChildId].height, mNodes[rightChildId].height)+1;
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
function TDynAABBTree.balanceSubTreeAtNode (nodeId: Integer): Integer;
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
  if (balanceFactor > 1.0) then
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
      nodeA.height := maxI(nodeB.height, nodeG.height)+1;
      nodeC.height := maxI(nodeA.height, nodeF.height)+1;
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
      nodeA.height := maxI(nodeB.height, nodeF.height)+1;
      nodeC.height := maxI(nodeA.height, nodeG.height)+1;
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
      nodeA.height := maxI(nodeC.height, nodeG.height)+1;
      nodeB.height := maxI(nodeA.height, nodeF.height)+1;
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
      nodeA.height := maxI(nodeC.height, nodeF.height)+1;
      nodeB.height := maxI(nodeA.height, nodeG.height)+1;
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
function TDynAABBTree.computeHeight (nodeId: Integer): Integer;
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
  result := 1+maxI(leftHeight, rightHeight);
end;


// internally add an object into the tree
function TDynAABBTree.insertObjectInternal (var aabb: AABB2D; staticObject: Boolean): Integer;
var
  nodeId: Integer;
begin
  // get the next available node (or allocate new ones if necessary)
  nodeId := allocateNode();

  // create the fat aabb to use in the tree
  mNodes[nodeId].aabb := aabb;
  if (not staticObject) then
  begin
    mNodes[nodeId].aabb.minX -= mExtraGap;
    mNodes[nodeId].aabb.minY -= mExtraGap;
    mNodes[nodeId].aabb.maxX += mExtraGap;
    mNodes[nodeId].aabb.maxY += mExtraGap;
  end;

  // set the height of the node in the tree
  mNodes[nodeId].height := 0;

  // insert the new leaf node in the tree
  insertLeafNode(nodeId);
  {$IFDEF aabbtree_many_asserts}assert(mNodes[nodeId].leaf);{$ENDIF}

  {$IFDEF aabbtree_many_asserts}assert(nodeId >= 0);{$ENDIF}

  // return the id of the node
  result := nodeId;
end;


// initialize the tree
procedure TDynAABBTree.setup ();
var
  i: Integer;
begin
  mRootNodeId := TTreeNode.NullTreeNode;
  mNodeCount := 0;
  mAllocCount := 8192;

  SetLength(mNodes, mAllocCount);
  //memset(mNodes, 0, mAllocCount*TTreeNode.sizeof);
  for i := 0 to mAllocCount-1 do mNodes[i].clear();

  // initialize the allocated nodes
  for i := 0 to mAllocCount-2 do
  begin
    mNodes[i].nextNodeId := i+1;
    mNodes[i].height := -1;
  end;
  mNodes[mAllocCount-1].nextNodeId := TTreeNode.NullTreeNode;
  mNodes[mAllocCount-1].height := -1;
  mFreeNodeId := 0;
end;


// also, checks if the tree structure is valid (for debugging purpose)
function TDynAABBTree.forEachLeaf (dg: TForEachLeafCB): Boolean;
  function forEachNode (nodeId: Integer): Boolean;
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
    e_WriteLog(Format('AABB:(%f,%f)-(%f,%f); volume=%f; valid=%d', [pNode.aabb.minX, pNode.aabb.minY, pNode.aabb.maxX, pNode.aabb.maxY, pNode.aabb.volume, Integer(pNode.aabb.valid)]), MSG_NOTIFY);
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
      height := 1+maxI(mNodes[leftChild].height, mNodes[rightChild].height);
      assert(mNodes[nodeId].height = height);
      // check the AABB of the node
      aabb := AABB2D.Create(mNodes[leftChild].aabb, mNodes[rightChild].aabb);
      assert(aabb.minX = mNodes[nodeId].aabb.minX);
      assert(aabb.minY = mNodes[nodeId].aabb.minY);
      assert(aabb.maxX = mNodes[nodeId].aabb.maxX);
      assert(aabb.maxY = mNodes[nodeId].aabb.maxY);
      // recursively check the children nodes
      result := forEachNode(leftChild);
      if not result then result := forEachNode(rightChild);
    end;
  end;

begin
  // recursively check each node
  result := forEachNode(mRootNodeId);
end;


// return `true` from visitor to stop immediately
// checker should check if this node should be considered to further checking
// returns tree node if visitor says stop or -1
function TDynAABBTree.visit (checker: TVisitCheckerCB; visitor: TVisitVisitorCB): Integer;
var
  stack: array [0..255] of Integer; // stack with the nodes to visit
  bigstack: array of Integer = nil;
  sp: Integer = 0;

  procedure spush (id: Integer);
  var
    xsp: Integer;
  begin
    if (sp < length(stack)) then
    begin
      // use "small stack"
      stack[sp] := id;
      Inc(sp);
    end
    else
    begin
      // use "big stack"
      xsp := sp-length(stack);
      if (xsp < length(bigstack)) then
      begin
        // reuse
        bigstack[xsp] := id;
      end
      else
      begin
        // grow
        SetLength(bigstack, length(bigstack)+1);
        bigstack[high(bigstack)] := id;
      end;
      Inc(sp);
    end;
  end;

  function spop (): Integer;
  begin
    assert(sp > 0);
    if (sp <= length(stack)) then
    begin
      // use "small stack"
      Dec(sp);
      result := stack[sp];
    end
    else
    begin
      // use "big stack"
      Dec(sp);
      result := bigstack[sp-length(stack)];
    end;
  end;

var
  nodeId: Integer;
  node: PTreeNode;
begin
  if not assigned(checker) then begin result := -1; exit; end;
  //if not assigned(visitor) then begin result := -1; exit; end;
  try
    {$IFDEF aabbtree_query_count}
    nodesVisited := 0;
    nodesDeepVisited := 0;
    {$ENDIF}

    // start from root node
    spush(mRootNodeId);

    // while there are still nodes to visit
    while (sp > 0) do
    begin
      // get the next node id to visit
      nodeId := spop();
      // skip it if it is a nil node
      if (nodeId = TTreeNode.NullTreeNode) then continue;
      {$IFDEF aabbtree_query_count}Inc(nodesVisited);{$ENDIF}
      // get the corresponding node
      node := @mNodes[nodeId];
      // should we investigate this node?
      if (checker(node)) then
      begin
        // if the node is a leaf
        if (node.leaf) then
        begin
          // call visitor on it
          {$IFDEF aabbtree_query_count}Inc(nodesDeepVisited);{$ENDIF}
          if assigned(visitor) then
          begin
            if (visitor(node.flesh)) then begin result := nodeId; exit; end;
          end;
        end
        else
        begin
          // if the node is not a leaf, we need to visit its children
          spush(node.children[TTreeNode.Left]);
          spush(node.children[TTreeNode.Right]);
        end;
      end;
    end;

    result := -1; // oops
  finally
    bigstack := nil;
  end;
end;


// add `extraAABBGap` to bounding boxes so slight object movement won't cause tree rebuilds
// extra AABB Gap used to allow the collision shape to move a little bit without triggering a large modification of the tree which can be costly
constructor TDynAABBTree.Create (extraAABBGap: Float=0.0);
begin
  mExtraGap := extraAABBGap;
  setup();
end;


destructor TDynAABBTree.Destroy ();
begin
  mNodes := nil;
  inherited;
end;


// clear all the nodes and reset the tree
procedure TDynAABBTree.reset ();
begin
  mNodes := nil;
  setup();
end;


function TDynAABBTree.computeTreeHeight (): Integer; begin result := computeHeight(mRootNodeId); end;


// return the root AABB of the tree
procedure TDynAABBTree.getRootAABB (var aabb: AABB2D);
begin
  {$IFDEF aabbtree_many_asserts}assert((mRootNodeId >= 0) and (mRootNodeId < mNodeCount));{$ENDIF}
  aabb := mNodes[mRootNodeId].aabb;
end;


// does the given id represents a valid object?
// WARNING: ids of removed objects can be reused on later insertions!
function TDynAABBTree.isValidId (id: Integer): Boolean;
begin
  result := (id >= 0) and (id < mNodeCount) and (mNodes[id].leaf);
end;


// get object by nodeid; can return nil for invalid ids
function TDynAABBTree.getNodeObjectId (nodeid: Integer): TTreeFlesh;
begin
  if (nodeid >= 0) and (nodeid < mNodeCount) and (mNodes[nodeid].leaf) then result := mNodes[nodeid].flesh else result := nil;
end;

// get fat object AABB by nodeid; returns random shit for invalid ids
procedure TDynAABBTree.getNodeFatAABB (var aabb: AABB2D; nodeid: Integer);
begin
  if (nodeid >= 0) and (nodeid < mNodeCount) and (not mNodes[nodeid].free) then aabb.copyFrom(mNodes[nodeid].aabb) else aabb.setDims(0, 0, 0, 0);
end;


// insert an object into the tree
// this method creates a new leaf node in the tree and returns the id of the corresponding node or -1 on error
// AABB for static object will not be "fat" (simple optimization)
// WARNING! inserting the same object several times *WILL* break everything!
function TDynAABBTree.insertObject (flesh: TTreeFlesh; staticObject: Boolean=false): Integer;
var
  aabb: AABB2D;
  nodeId: Integer;
begin
  if not getFleshAABB(aabb, flesh) then
  begin
    e_WriteLog(Format('trying to insert FUCKED AABB:(%f,%f)-(%f,%f); volume=%f; valid=%d', [aabb.minX, aabb.minY, aabb.maxX, aabb.maxY, aabb.volume, Integer(aabb.valid)]), MSG_WARNING);
    result := -1;
    exit;
  end;
  e_WriteLog(Format('inserting AABB:(%f,%f)-(%f,%f); volume=%f; valid=%d', [aabb.minX, aabb.minY, aabb.maxX, aabb.maxY, aabb.volume, Integer(aabb.valid)]), MSG_NOTIFY);
  nodeId := insertObjectInternal(aabb, staticObject);
  {$IFDEF aabbtree_many_asserts}assert(mNodes[nodeId].leaf);{$ENDIF}
  mNodes[nodeId].flesh := flesh;
  result := nodeId;
end;


// remove an object from the tree
// WARNING: ids of removed objects can be reused on later insertions!
procedure TDynAABBTree.removeObject (nodeId: Integer);
begin
  if (nodeId < 0) or (nodeId >= mNodeCount) or (not mNodes[nodeId].leaf) then raise Exception.Create('invalid node id in TDynAABBTree');
  // remove the node from the tree
  removeLeafNode(nodeId);
  releaseNode(nodeId);
end;


function TDynAABBTree.updateObject (nodeId: Integer; dispX, dispY: Float; forceReinsert: Boolean=false): Boolean;
var
  newAABB: AABB2D;
begin
  if (nodeId < 0) or (nodeId >= mNodeCount) or (not mNodes[nodeId].leaf) then raise Exception.Create('invalid node id in TDynAABBTree');

  if not getFleshAABB(newAABB, mNodes[nodeId].flesh) then raise Exception.Create('invalid node id in TDynAABBTree');

  // if the new AABB is still inside the fat AABB of the node
  if (not forceReinsert) and (mNodes[nodeId].aabb.contains(newAABB)) then begin result := false; exit; end;

  // if the new AABB is outside the fat AABB, we remove the corresponding node
  removeLeafNode(nodeId);

  // compute the fat AABB by inflating the AABB with a constant gap
  mNodes[nodeId].aabb := newAABB;
  if not forceReinsert and ((dispX <> 0) or (dispY <> 0)) then
  begin
    mNodes[nodeId].aabb.minX := mNodes[nodeId].aabb.minX-mExtraGap;
    mNodes[nodeId].aabb.minY := mNodes[nodeId].aabb.minY-mExtraGap;
    mNodes[nodeId].aabb.maxX := mNodes[nodeId].aabb.maxX+mExtraGap;
    mNodes[nodeId].aabb.maxY := mNodes[nodeId].aabb.maxY+mExtraGap;
  end;

  // inflate the fat AABB in direction of the linear motion of the AABB
  if (dispX < 0.0) then
  begin
    mNodes[nodeId].aabb.minX := mNodes[nodeId].aabb.minX+LinearMotionGapMultiplier*dispX;
  end
  else
  begin
    mNodes[nodeId].aabb.maxX := mNodes[nodeId].aabb.maxX+LinearMotionGapMultiplier*dispX;
  end;
  if (dispY < 0.0) then
  begin
    mNodes[nodeId].aabb.minY := mNodes[nodeId].aabb.minY+LinearMotionGapMultiplier*dispY;
  end
  else
  begin
    mNodes[nodeId].aabb.maxY := mNodes[nodeId].aabb.maxY+LinearMotionGapMultiplier*dispY;
  end;

  {$IFDEF aabbtree_many_asserts}assert(mNodes[nodeId].aabb.contains(newAABB));{$ENDIF}

  // reinsert the node into the tree
  insertLeafNode(nodeId);

  result := true;
end;


// report all shapes overlapping with the AABB given in parameter
procedure TDynAABBTree.aabbQuery (ax, ay, aw, ah: Float; cb: TQueryOverlapCB);
var
  caabb: AABB2D;
  function checker (node: PTreeNode): Boolean;
  begin
    result := caabb.overlaps(node.aabb);
  end;
begin
  if not assigned(cb) then exit;
  if (aw < 1) or (ah < 1) then exit;
  caabb := AABB2D.Create(ax, ay, ax+aw-1, ay+ah-1);
  visit(checker, cb);
end;


// report body that contains the given point, or nil
function TDynAABBTree.pointQuery (ax, ay: Float; cb: TQueryOverlapCB): TTreeFlesh;
var
  nid: Integer;
  function checker (node: PTreeNode): Boolean;
  begin
    result := node.aabb.contains(ax, ay);
  end;
begin
  nid := visit(checker, cb);
  {$IFDEF aabbtree_many_asserts}assert((nid < 0) or ((nid >= 0) and (nid < mNodeCount) and (mNodes[nid].leaf)));{$ENDIF}
  if (nid >= 0) then result := mNodes[nid].flesh else result := nil;
end;


// segment querying method
function TDynAABBTree.segmentQuery (var qr: TSegmentQueryResult; ax, ay, bx, by: Float; cb: TSegQueryCallback): Boolean;
var
  maxFraction: Float = 1.0e100; // infinity
  curax, curay: Float;
  curbx, curby: Float;
  dirx, diry: Float;
  invlen: Float;

  function checker (node: PTreeNode): Boolean;
  begin
    result := node.aabb.intersects(curax, curay, curbx, curby);
  end;

  function visitor (flesh: TTreeFlesh): Boolean;
  var
    hitFraction: Float;
  begin
    hitFraction := cb(flesh, curax, curay, curbx, curby);
    // if the user returned a hitFraction of zero, it means that the raycasting should stop here
    if (hitFraction = 0.0) then
    begin
      qr.dist := 0;
      qr.flesh := flesh;
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
        qr.dist := hitFraction;
        qr.flesh := flesh;
        // fix curb here
        //curb := cura+dir*hitFraction;
        curbx := curax+dirx*hitFraction;
        curby := curay+diry*hitFraction;
      end;
    end;
    result := false; // continue
  end;

begin
  qr.reset();

  if (ax >= bx) or (ay >= by) then begin result := false; exit; end;

  curax := ax;
  curay := ay;
  curbx := bx;
  curby := by;

  dirx := (curbx-curax);
  diry := (curby-curay);
  // normalize
  invlen := 1.0/sqrt(dirx*dirx+diry*diry);
  dirx *= invlen;
  diry *= invlen;

  visit(checker, visitor);

  result := qr.valid;
end;


end.
