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
// universal sweep-and-prune broad phase
{$INCLUDE ../shared/a_modes.inc}
{$DEFINE SAP_CHECKS}
{$DEFINE SAP_ALWAYS_SORT}
{$DEFINE SAP_WALK_DEBUG}
{$DEFINE SAP_INSERT_DEBUG}
unit g_sap;

interface

type
  TSAPQueryCB = function (obj: TObject; tag: Integer): Boolean is nested; // return `true` to stop

type
  TSAPProxy = Integer;

  PSAPProxyRec = ^TSAPProxyRec;
  TSAPProxyRec = record
  private
    mX, mY, mWidth, mHeight: Integer; // aabb
    mQueryMark: DWord; // was this object visited at this query?
    mObj: TObject;
    mTag: Integer;
    mIdx: array [Boolean, 0..1] of Integer; // indicies in corresponding intervals

  private
    procedure setup (aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer);

    function getx1 (): Integer; inline;
    function gety1 (): Integer; inline;

    function getidx (ismin: Boolean; iidx: Integer): Integer; inline;
    procedure setidx (ismin: Boolean; iidx: Integer; v: Integer); inline;

    function getnextfree (): Integer;
    procedure setnextfree (v: Integer);

  public
    property x: Integer read mX;
    property y: Integer read mY;
    property width: Integer read mWidth;
    property height: Integer read mHeight;
    property x0: Integer read mX;
    property y0: Integer read mY;
    property x1: Integer read getx1;
    property y1: Integer read gety1;
    property obj: TObject read mObj;
    property tag: Integer read mTag;
    property idx[ismin: Boolean; iidx: Integer]: Integer read getidx write setidx;
    property nextfree: Integer read getnextfree write setnextfree;
  end;


  TSweepAndPrune = class(TObject)
  private
    type
      PIntervalRec = ^TIntervalRec;
      TIntervalRec = record
      public
        val: Integer;
        mpidx: DWord; // proxy idx; bit 31 is "ismin"

      public
        function less (var i: TIntervalRec): Boolean; inline;
        function inside (v0, v1: Integer): Boolean; inline;

        function getismin (): Boolean; inline;
        procedure setismin (v: Boolean); inline;

        function getpidx (): Integer; inline;
        procedure setpidx (v: Integer); inline;

        property ismin: Boolean read getismin write setismin;
        property pidx: Integer read getpidx write setpidx;
      end;

      TInterval = record
      public
        type
          TWalkCB = function (pidx: Integer; px: PSAPProxyRec): Boolean is nested; // return `true` to stop

      public
        intrs: array of TIntervalRec;
        used: Integer;
        mProxies: array of TSAPProxyRec; // copy of main mProxies
        myidx: Integer; // index of this interval

      public
        procedure setup (aIdx: Integer);
        procedure cleanup ();

        procedure sort ();
        procedure insert (apidx: Integer);

        function walk (v0, v1: Integer; cb: TWalkCB): Boolean;

        procedure dump ();
      end;

  private
    mLastQuery: DWord;
    mIntrs: array[0..1] of TInterval;
    mProxies: array of TSAPProxyRec;
    mProxyFree: TSAPProxy; // free
    mProxyCount: Integer; // currently used
    mProxyMaxCount: Integer;
    mUpdateBlocked: Integer; // >0: updates are blocked

  private
    function allocProxy (aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer): TSAPProxy;
    procedure freeProxy (body: TSAPProxy);

    procedure sortIntervals ();

    procedure insert (body: TSAPProxy);
    //procedure remove (body: TSAPProxy);

  public
    constructor Create ();
    destructor Destroy (); override;

    function insertBody (aObj: TObject; ax, ay, aWidth, aHeight: Integer; aTag: Integer=0): TSAPProxy;
    //procedure removeBody (aObj: TSAPProxy); // WARNING! this WILL destroy proxy!

    //procedure moveBody (body: TSAPProxy; dx, dy: Integer);
    //procedure resizeBody (body: TSAPProxy; sx, sy: Integer);
    //procedure moveResizeBody (body: TSAPProxy; dx, dy, sx, sy: Integer);

    function forEachInAABB (x, y, w, h: Integer; cb: TSAPQueryCB): Boolean;

    //function getProxyForBody (aObj: TObject; x, y, w, h: Integer): TSAPProxy;

    // call these functions before massive update (it may, or may be not faster)
    procedure batchUpdateBegin ();
    procedure batchUpdateEnd ();

    procedure dumpStats ();
  end;


implementation

uses
  SysUtils, e_log;


// ////////////////////////////////////////////////////////////////////////// //
procedure TSAPProxyRec.setup (aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer);
begin
  mX := aX;
  mY := aY;
  mWidth := aWidth;
  mHeight := aHeight;
  mQueryMark := 0;
  mObj := aObj;
  mTag := aTag;
  mIdx[false, 0] := -1;
  mIdx[false, 1] := -1;
  mIdx[true, 0] := -1;
  mIdx[true, 1] := -1;
end;

function TSAPProxyRec.getx1 (): Integer; begin result := mX+mWidth-1; end;
function TSAPProxyRec.gety1 (): Integer; begin result := mY+mHeight-1; end;

function TSAPProxyRec.getidx (ismin: Boolean; iidx: Integer): Integer; begin result := mIdx[ismin, iidx]; end;
procedure TSAPProxyRec.setidx (ismin: Boolean; iidx: Integer; v: Integer); begin mIdx[ismin, iidx] := v; end;

function TSAPProxyRec.getnextfree (): Integer; begin result := mIdx[false, 0]; end;
procedure TSAPProxyRec.setnextfree (v: Integer); begin mIdx[false, 0] := v; end;


// ////////////////////////////////////////////////////////////////////////// //
function TSweepAndPrune.TIntervalRec.getismin (): Boolean; begin result := ((mpidx and $80000000) <> 0); end;
procedure TSweepAndPrune.TIntervalRec.setismin (v: Boolean); begin if (v) then mpidx := mpidx or $80000000 else mpidx := mpidx and $7fffffff; end;

function TSweepAndPrune.TIntervalRec.getpidx (): Integer; begin result := Integer(mpidx and $7fffffff); end;
procedure TSweepAndPrune.TIntervalRec.setpidx (v: Integer); begin mpidx := (v and $7fffffff) or (mpidx and $80000000); end;


function TSweepAndPrune.TIntervalRec.less (var i: TIntervalRec): Boolean;
var
  n: Integer;
begin
  n := val-i.val;
  if (n <> 0) then result := (n < 0) else result := (pidx < i.pidx);
end;


// v0 MUST be <= v1!
function TSweepAndPrune.TIntervalRec.inside (v0, v1: Integer): Boolean;
begin
  result := (val >= v0) and (val <= v1);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TSweepAndPrune.TInterval.setup (aIdx: Integer);
begin
  SetLength(intrs, 8192*2);
  used := 0;
  mProxies := nil;
  myidx := aIdx;
end;


procedure TSweepAndPrune.TInterval.cleanup ();
begin
  intrs := nil;
  mProxies := nil;
end;


procedure TSweepAndPrune.TInterval.sort ();
var
  len, i, j: Integer;
  x: TIntervalRec;
  arr: array of TIntervalRec;
  pxa: array of TSAPProxyRec;
  iidx: Integer;
begin
  len := used;
  if (len = 0) then exit;
  arr := intrs;
  pxa := mProxies;
  iidx := myidx;
  {$IFDEF SAP_CHECKS}
  for i := 0 to len-1 do
  begin
    if (pxa[arr[i].pidx].idx[arr[i].ismin, iidx] <> i) then
    begin
      e_WriteLog(Format('FUCKUP: interval %d; i=%d; val=%d; ismin=%d; got=%d', [iidx, i, arr[i].val, Integer(arr[i].ismin), pxa[arr[i].pidx].idx[arr[i].ismin, iidx]]), MSG_NOTIFY);
      dump();
      raise Exception.Create('sorting fuckup (5)');
    end;
  end;
  {$ENDIF}
  if (len > 1) then
  begin
    i := 1;
    while (i < len) do
    begin
      if (arr[i].less(arr[i-1])) then
      begin
        x := arr[i];
        j := i-1;
        while (j >= 0) and (x.less(arr[j])) do
        begin
          arr[j+1] := arr[j];
          pxa[arr[j+1].pidx].idx[arr[j+1].ismin, iidx] := j+1;
          Dec(j);
        end;
        pxa[x.pidx].idx[x.ismin, iidx] := j+1;
        arr[j+1] := x;
      end;
      Inc(i);
    end;
  end;
  // check
  {$IFDEF SAP_CHECKS}
  for i := 0 to len-1 do
  begin
    if (i <> 0) then
    begin
      if arr[i].less(arr[i-1]) then begin dump(); raise Exception.Create('sorting fuckup (2)'); end;
      if (arr[i-1].val > arr[i].val) then begin dump(); raise Exception.Create('sorting fuckup (3)'); end;
    end;
    if (pxa[arr[i].pidx].idx[arr[i].ismin, iidx] <> i) then begin dump(); raise Exception.Create('sorting fuckup (4)'); end;
  end;
  {$ENDIF}
end;


procedure TSweepAndPrune.TInterval.insert (apidx: Integer);
var
  v0, v1, i: Integer;
  pi: PIntervalRec;
  px: PSAPProxyRec;
begin
  px := @mProxies[apidx];
  // get min/max
  if (myidx = 0) then
  begin
    v0 := px.x0;
    v1 := px.x1;
  end
  else
  begin
    v0 := px.y0;
    v1 := px.y1;
  end;
  // append min
  i := used;
  if (i+2 >= Length(intrs)) then SetLength(intrs, i+8192*2);
  {$IFDEF SAP_INSERT_DEBUG}
  e_WriteLog(Format('inserting proxy %d into interval %d; v0=%d; i=%d', [apidx, myidx, v0, i]), MSG_NOTIFY);
  {$ENDIF}
  pi := @intrs[i];
  pi.val := v0;
  pi.pidx := apidx;
  pi.ismin := true;
  px.idx[true, myidx] := i;
  // append max
  Inc(i);
  {$IFDEF SAP_INSERT_DEBUG}
  e_WriteLog(Format('inserting proxy %d into interval %d; v1=%d; i=%d', [apidx, myidx, v1, i]), MSG_NOTIFY);
  {$ENDIF}
  pi := @intrs[i];
  pi.val := v1;
  pi.pidx := apidx;
  pi.ismin := false;
  px.idx[false, myidx] := i;
  // done
  Inc(used, 2);
  {$IFDEF SAP_CHECKS}
  if (used <> i+1) then raise Exception.Create('something is VERY wrong in SAP');
  {$ENDIF}
end;


function TSweepAndPrune.TInterval.walk (v0, v1: Integer; cb: TWalkCB): Boolean;
var
  len: Integer;
  i, bot, mid, cmp: Integer;
  px: PSAPProxyRec;
  arr: array of TIntervalRec;
  pxa: array of TSAPProxyRec;
begin
  result := false;
  if not assigned(cb) or (v0 > v1) then exit; // alas
  len := used;
  if (len < 1) then exit; // nothing to do
  arr := intrs;
  pxa := mProxies;

  {$IFDEF SAP_WALK_DEBUG}
  e_WriteLog(Format('walking interval #%d; v0=%d; v1=%d; len=%d', [myidx, v0, v1, len]), MSG_NOTIFY);
  {$ENDIF}

  if (len = 1) then
  begin
    // one element
    i := 0;
  end
  else
  begin
    // do search
    bot := 0;
    i := len-1;
    while (bot <> i) do
    begin
      mid := i-(i-bot) div 2;
      cmp := v0-arr[mid].val;
      if (cmp = 0) then break;
      if (cmp < 0) then i := mid-1 else bot := mid;
    end;
    //return (cmpfn(lines+i) == 0 ? i : -1);
    {$IFDEF SAP_WALK_DEBUG}
    e_WriteLog(Format('  binsearch interval #%d; i=%d; len=%d; isect=%d', [myidx, i, len, Integer(arr[i].inside(v0, v1))]), MSG_NOTIFY);
    {$ENDIF}
    if not arr[i].inside(v0, v1) then
    begin
      {$IFDEF SAP_WALK_DEBUG}e_WriteLog('    bin: not good', MSG_NOTIFY);{$ENDIF}
           if (i > 0) and arr[i-1].inside(v0, v1) then begin Dec(i); {$IFDEF SAP_WALK_DEBUG}e_WriteLog('    bin: up', MSG_NOTIFY);{$ENDIF} end
      else if (i+1 < len) and arr[i+1].inside(v0, v1) then begin Inc(i); {$IFDEF SAP_WALK_DEBUG}e_WriteLog('    bin: down', MSG_NOTIFY);{$ENDIF} end
      else begin {$IFDEF SAP_WALK_DEBUG}e_WriteLog('    bin: wtf?!', MSG_NOTIFY);{$ENDIF} end;
    end;
    // find first interval
    while (i > 0) and arr[i-1].inside(v0, v1) do Dec(i);
  end;

  {$IFDEF SAP_WALK_DEBUG}
  if (i >= 0) and (i < len) and arr[i].inside(v0, v1) then
    e_WriteLog(Format('  start interval #%d; i=%d; v0=%d; v1=%d; len=%d; val=%d; ismin=%d', [myidx, i, v0, v1, len, arr[i].val, Integer(arr[i].ismin)]), MSG_NOTIFY);
  {$ENDIF}

  // walk
  while (i >= 0) and (i < len) and arr[i].inside(v0, v1) do
  begin
    px := @pxa[arr[i].pidx];
    result := cb(arr[i].pidx, px);
    if result then break;
    Inc(i);
  end;

  {$IFDEF SAP_WALK_DEBUG}
  Dec(i);
  if (i >= 0) and (i < len) then
    e_WriteLog(Format('  end interval #%d; i=%d; v0=%d; v1=%d; len=%d; val=%d; ismin=%d', [myidx, i, v0, v1, len, arr[i].val, Integer(arr[i].ismin)]), MSG_NOTIFY);
  {$ENDIF}
end;


procedure TSweepAndPrune.TInterval.dump ();
var
  idx: Integer;
  pi: PIntervalRec;
begin
  e_WriteLog(Format('interval #%d; len=%d', [myidx, used]), MSG_NOTIFY);
  for idx := 0 to used-1 do
  begin
    pi := @intrs[idx];
    e_WriteLog(Format('  pi #%d; val=%d; ismin=%d; pidx=%d; px0=%d; py0=%d; px1=%d; py1=%d', [idx, pi.val, Integer(pi.ismin), pi.pidx, mProxies[pi.pidx].x0, mProxies[pi.pidx].y0, mProxies[pi.pidx].x1, mProxies[pi.pidx].y1]), MSG_NOTIFY);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TSweepAndPrune.Create ();
var
  idx: Integer;
begin
  mLastQuery := 0;

  // init proxies
  SetLength(mProxies, 8192);
  for idx := 0 to High(mProxies) do
  begin
    mProxies[idx].idx[true, 0] := idx+1;
    mProxies[idx].idx[true, 1] := -1;
    mProxies[idx].idx[false, 0] := -1;
    mProxies[idx].idx[false, 1] := -1;
  end;
  mProxies[High(mProxies)].idx[true, 0] := -1;

  // init intervals
  for idx := 0 to High(mIntrs) do
  begin
    mIntrs[idx].setup(idx);
    mIntrs[idx].mProxies := mProxies;
  end;

  mProxyFree := 0;
  mProxyCount := 0;
  mProxyMaxCount := 0;

  mUpdateBlocked := 0;
end;


destructor TSweepAndPrune.Destroy ();
var
  idx: Integer;
begin
  for idx := 0 to High(mIntrs) do mIntrs[idx].cleanup();
  mProxies := nil;
  inherited;
end;


procedure TSweepAndPrune.dumpStats ();
begin
  e_WriteLog(Format('used intervals: [%d;%d]; max proxies allocated: %d; proxies used: %d', [mIntrs[0].used, mIntrs[1].used, mProxyMaxCount, mProxyCount]), MSG_NOTIFY);
  mIntrs[0].dump();
  mIntrs[1].dump();
end;


procedure TSweepAndPrune.batchUpdateBegin ();
begin
  Inc(mUpdateBlocked);
end;


procedure TSweepAndPrune.batchUpdateEnd ();
begin
  if (mUpdateBlocked > 0) then
  begin
    Dec(mUpdateBlocked);
    if (mUpdateBlocked = 0) then sortIntervals();
  end;
end;


function TSweepAndPrune.allocProxy (aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer): TSAPProxy;
var
  olen, idx: Integer;
  px: PSAPProxyRec;
begin
  if (mProxyFree = -1) then
  begin
    // no free proxies, resize list
    olen := Length(mProxies);
    SetLength(mProxies, olen+8192); // arbitrary number
    for idx := olen to High(mProxies) do mProxies[idx].idx[true, 0] := idx+1;
    mProxies[High(mProxies)].idx[true, 0] := -1;
    mProxyFree := olen;
    // fix intervals cache
    for idx := 0 to High(mIntrs) do mIntrs[idx].mProxies := mProxies;
  end;
  // get one from list
  result := mProxyFree;
  px := @mProxies[result];
  mProxyFree := px.idx[true, 0];
  px.setup(aX, aY, aWidth, aHeight, aObj, aTag);
  // add to used list
  px.idx[true, 0] := idx+1;
  px.idx[true, 1] := -1;
  px.idx[false, 0] := -1;
  px.idx[false, 1] := -1;
  // statistics
  Inc(mProxyCount);
  if (mProxyMaxCount < mProxyCount) then mProxyMaxCount := mProxyCount;
end;


procedure TSweepAndPrune.freeProxy (body: TSAPProxy);
var
  px: PSAPProxyRec;
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  if (mProxyCount = 0) then raise Exception.Create('wutafuuuuu in grid (no allocated proxies, what i should free now?)');
  // add to free list
  px := @mProxies[body];
  px.mObj := nil;
  px.idx[true, 0] := mProxyFree;
  px.idx[true, 1] := -1;
  px.idx[false, 0] := -1;
  px.idx[false, 1] := -1;
  mProxyFree := body;
  Dec(mProxyCount);
end;


procedure TSweepAndPrune.sortIntervals ();
begin
  mIntrs[0].sort();
  mIntrs[1].sort();
end;


procedure TSweepAndPrune.insert (body: TSAPProxy);
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  mIntrs[0].insert(body);
  mIntrs[1].insert(body);
  {$IFDEF SAP_ALWAYS_SORT}
  sortIntervals();
  {$ELSE}
  if (mUpdateBlocked = 0) then sortIntervals();
  {$ENDIF}
end;


function TSweepAndPrune.insertBody (aObj: TObject; aX, aY, aWidth, aHeight: Integer; aTag: Integer=0): TSAPProxy;
begin
  if (aObj = nil) or (aWidth < 1) or (aHeight < 1) then begin result := -1; exit; end;
  result := allocProxy(aX, aY, aWidth, aHeight, aObj, aTag);
  insert(result);
end;


function TSweepAndPrune.forEachInAABB (x, y, w, h: Integer; cb: TSAPQueryCB): Boolean;
var
  lq: Integer;

  function walker0 (pidx: Integer; px: PSAPProxyRec): Boolean;
  begin
    result := false; // don't stop
    {$IFDEF SAP_WALK_DEBUG}
    e_WriteLog(Format('    walker0: pidx=%d; x0=%d; y0=%d; x1=%d; y1=%d; lq=%d', [pidx, px.x0, px.y0, px.x1, px.y1, lq]), MSG_NOTIFY);
    {$ENDIF}
    px.mQueryMark := lq;
  end;

  function walker1 (pidx: Integer; px: PSAPProxyRec): Boolean;
  begin
    {$IFDEF SAP_WALK_DEBUG}
    e_WriteLog(Format('    walker1: pidx=%d; x0=%d; y0=%d; x1=%d; y1=%d; lq=%d', [pidx, px.x0, px.y0, px.x1, px.y1, px.mQueryMark]), MSG_NOTIFY);
    {$ENDIF}
    if (px.mQueryMark = lq) then
    begin
      result := cb(px.mObj, px.mTag);
      {$IFDEF SAP_WALK_DEBUG}
      e_WriteLog(Format('      CB walker1: pidx=%d; x0=%d; y0=%d; x1=%d; y1=%d; lq=%d; res=%d', [pidx, px.x0, px.y0, px.x1, px.y1, px.mQueryMark, Integer(result)]), MSG_NOTIFY);
      {$ENDIF}
    end
    else
    begin
      result := false; // don't stop
    end;
  end;

var
  idx: Integer;
begin
  result := false;
  if not assigned(cb) then exit; // no callback, not interesting
  if (w < 1) or (h < 1) then exit; // nothing to do

  // increase query counter
  Inc(mLastQuery);
  if (mLastQuery = 0) then
  begin
    // just in case of overflow
    mLastQuery := 1;
    for idx := 0 to High(mProxies) do mProxies[idx].mQueryMark := 0;
  end;

  (*
   * the algorithm is simple:
   *   find start for first interval (binary search will do)
   *   walk the interval, marking proxies with mLastQuery
   *   find start for second interval (binary search will do)
   *   walk the interval, returning proxies marked with mLastQuery
   *)
  lq := mLastQuery;
  mIntrs[0].walk(x, x+w-1, walker0);
  result := mIntrs[1].walk(y, y+h-1, walker1);
end;


end.
