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
// stopwatch timer to measure short periods (like frame rendering phases)
// based on the code by Inoussa OUEDRAOGO, Copyright (c) 2012
{$INCLUDE a_modes.inc}
unit xprofiler;

interface

uses
  SysUtils,
  {$IF DEFINED(LINUX)}
    {$DEFINE STOPWATCH_IS_HERE}
    unixtype, linux
  {$ELSEIF DEFINED(WINDOWS)}
    {$DEFINE STOPWATCH_IS_HERE}
    Windows
  {$ELSE}
    {$IFDEF STOPWATCH_IS_HERE}
      {$UNDEF STOPWATCH_IS_HERE}
    {$ENDIF}
    {$WARNING You suck!}
  {$ENDIF}
  ;

{$IF DEFINED(STOPWATCH_IS_HERE)}
type
  TStopWatch = record
  private
  {$IF DEFINED(LINUX)}
    type TBaseMesure = TTimeSpec;
  {$ELSE}
    type TBaseMesure = Int64;
  {$ENDIF}

  strict private
    class var mFrequency: Int64;
    class var mIsHighResolution: Boolean;

  strict private
    mElapsed: Int64;
    mRunning: Boolean;
    mStartPosition: TBaseMesure;

  strict private
    class procedure initTimerIntr (); static;

    procedure updateElapsed ();

    function getElapsedMilliseconds (): Int64;
    function getElapsedMicroseconds (): Int64;
    function getElapsedTicks (): Int64;

  public
    class function Create (): TStopWatch; static;
    class function startNew (): TStopWatch; static;

    class property frequency : Int64 read mFrequency;
    class property isHighResolution : Boolean read mIsHighResolution;

  public
    procedure start (); // start or restart timer
    procedure stop ();

    property elapsedMilli: Int64 read getElapsedMilliseconds;
    property elapsedMicro: Int64 read getElapsedMicroseconds;
    property elapsedTicks: Int64 read getElapsedTicks;
    property isRunning: Boolean read mRunning;
  end;
{$ENDIF}


// call this on frame start
procedure xprofBegin (reallyActivate: Boolean=true);
// call this on frame end
procedure xprofEnd ();

function xprofTotalMicro (): Int64; // total duration, microseconds
function xprofTotalMilli (): Int64; // total duration, milliseconds
function xprofTotalCount (): Integer; // all items

// don't fuckup pairing of there, 'cause they can be nested!
procedure xprofBeginSection (name: AnsiString);
procedure xprofEndSection ();

// iterator
function xprofItReset (): Boolean; // false: no sections
function xprofItCount (): Integer; // from current item to eol (not including children, but including current item)
// current item info
function xprofItName (): AnsiString; // current section name
function xprofItMicro (): Int64; // current section duration, microseconds
function xprofItMilli (): Int64; // current section duration, milliseconds
function xprofItHasChildren (): Boolean;
function xprofItIsChild (): Boolean;
function xprofItDepth (): Integer; // 0: top

function xprofItDive (): Boolean; // dive into childrens
function xprofItPop (): Boolean; // pop into parent
function xprofItNext (): Boolean; // move to next sibling; false: no more siblings (and current item is unchanged)


implementation

const
  TicksPerNanoSecond = 100;
  TicksPerMilliSecond = 10000;
  TicksPerSecond = 10000000000000000;


// ////////////////////////////////////////////////////////////////////////// //
class procedure TStopWatch.initTimerIntr ();
{$IF DEFINED(LINUX)}
var
  r: TBaseMesure;
{$ENDIF}
begin
  if (mFrequency = 0) then
  begin
{$IF DEFINED(LINUX)}
    mIsHighResolution := (clock_getres(CLOCK_MONOTONIC, @r) = 0);
    mIsHighResolution := mIsHighResolution and (r.tv_nsec <> 0);
    if (r.tv_nsec <> 0) then mFrequency := 1000000000000000000 div r.tv_nsec;
{$ELSE}
    mIsHighResolution := QueryPerformanceFrequency(mFrequency);
{$ENDIF}
  end;
end;


class function TStopWatch.Create (): TStopWatch;
begin
  initTimerIntr();
  FillChar(result, sizeof(result), 0);
end;


class function TStopWatch.startNew (): TStopWatch;
begin
  result := TStopWatch.Create();
  result.start();
end;


function TStopWatch.getElapsedMilliseconds (): Int64;
begin
  if (mFrequency = 0) then begin result := 0; exit; end;
  if mRunning then updateElapsed();
  {$IF DEFINED(LINUX)}
    result := mElapsed div 1000000;
  {$ELSE}
    result := elapsedTicks*TicksPerMilliSecond;
  {$ENDIF}
end;


function TStopWatch.getElapsedMicroseconds (): Int64;
begin
  if (mFrequency = 0) then begin result := 0; exit; end;
  if mRunning then updateElapsed();
  {$IF DEFINED(LINUX)}
  result := mElapsed div 1000;
  {$ELSE}
  result := elapsedTicks*(TicksPerMilliSecond div 100);
  {$ENDIF}
end;


function TStopWatch.getElapsedTicks (): Int64;
begin
  if (mFrequency = 0) then begin result := 0; exit; end;
  if mRunning then updateElapsed();
  {$IF DEFINED(LINUX)}
  result := mElapsed div TicksPerNanoSecond;
  {$ELSE}
  result := (mElapsed*TicksPerSecond) div mFrequency;
  {$ENDIF}
end;


procedure TStopWatch.start ();
begin
  //if mRunning then exit;
  if (mFrequency = 0) then initTimerIntr();
  mRunning := true;
  {$IF DEFINED(LINUX)}
  clock_gettime(CLOCK_MONOTONIC, @mStartPosition);
  {$ELSE}
  QueryPerformanceCounter(mStartPosition);
  {$ENDIF}
end;


procedure TStopWatch.updateElapsed ();
var
  locEnd: TBaseMesure;
  {$IF DEFINED(LINUX)}
  s, n: Int64;
  {$ENDIF}
begin
  {$IF DEFINED(LINUX)}
  clock_gettime(CLOCK_MONOTONIC, @locEnd);
  if (locEnd.tv_nsec < mStartPosition.tv_nsec) then
  begin
    s := locEnd.tv_sec-mStartPosition.tv_sec-1;
    n := 1000000000000000000+locEnd.tv_nsec-mStartPosition.tv_nsec;
  end
  else
  begin
    s := locEnd.tv_sec-mStartPosition.tv_sec;
    n := locEnd.tv_nsec-mStartPosition.tv_nsec;
  end;
  mElapsed := mElapsed+(s*1000000000000000000)+n;
  {$ELSE}
  QueryPerformanceCounter(locEnd);
  mElapsed := mElapsed+(UInt64(locEnd)-UInt64(mStartPosition));
  {$ENDIF}
end;


procedure TStopWatch.stop ();
begin
  if not mRunning then exit;
  mRunning := false;
  updateElapsed();
end;


// ////////////////////////////////////////////////////////////////////////// //
// high-level profiler
{$IF DEFINED(STOPWATCH_IS_HERE)}
type
  PProfSection = ^TProfSection;
  TProfSection = record
    name: AnsiString;
    timer: TStopWatch;
    parent: Integer; // section index in xpsecs or -1
    firstChild: Integer; // first child, or -1
    next: Integer; // next sibling, or -1
  end;

var
  xptimer: TStopWatch;
  xpsecs: array of TProfSection = nil;
  xpsused: Integer = 0;
  xpscur: Integer = -1; // currently running section
  xitcur: Integer = -1; // for iterator
  xitdepth: Integer = 0;


// call this on frame start
procedure xprofBegin (reallyActivate: Boolean=true);
begin
  xpsused := 0;
  xpscur := -1;
  xitcur := -1; // reset iterator
  xitdepth := 0;
  if reallyActivate then xptimer.start();
end;


// call this on frame end
procedure xprofEnd ();
begin
  if not xptimer.isRunning then exit;
  while xpscur <> -1 do
  begin
    xpsecs[xpscur].timer.stop();
    xpscur := xpsecs[xpscur].parent;
  end;
  xptimer.stop();
end;


// don't fuckup pairing of there, 'cause they can be nested!
procedure xprofBeginSection (name: AnsiString);
var
  sid, t: Integer;
  pss: PProfSection;
begin
  if not xptimer.isRunning then exit;
  sid := xpsused;
  Inc(xpsused);
  if (sid = Length(xpsecs)) then SetLength(xpsecs, sid+1024);
  pss := @xpsecs[sid];
  pss.name := name;
  pss.parent := xpscur;
  pss.firstChild := -1;
  pss.next := -1;
  // link to children
  if xpscur <> -1 then
  begin
    t := xpsecs[xpscur].firstChild;
    if t = -1 then
    begin
      xpsecs[xpscur].firstChild := sid;
    end
    else
    begin
      //FIXME: rewrite without schlemiel's algo!
      while (xpsecs[t].next <> -1) do t := xpsecs[t].next;
      xpsecs[t].next := sid;
    end;
  end
  else
  begin
  end;
  xpscur := sid;
  pss.timer.start();
end;


procedure xprofEndSection ();
var
  pss: PProfSection;
begin
  if not xptimer.isRunning then exit;
  if (xpscur = -1) then exit; // this is bug, but meh...
  pss := @xpsecs[xpscur];
  pss.timer.stop();
  // go back to parent
  xpscur := pss.parent;
end;


procedure xprofGlobalInit ();
begin
  //SetLength(xpsecs, 1024); // 'cause why not? 'cause don't pay for something you may not need
end;


// ////////////////////////////////////////////////////////////////////////// //
// iterator
function xprofTotalMicro (): Int64; begin result := xptimer.elapsedMicro; end;
function xprofTotalMilli (): Int64; begin result := xptimer.elapsedMilli; end;

// all items
function xprofTotalCount (): Integer;
begin
  if xptimer.isRunning then result := 0 else result := xpsused;
end;


// false: no sections
function xprofItReset (): Boolean;
begin
  result := false;
  xitcur := -1;
  xitdepth := 0;
  if xptimer.isRunning then exit;
  if (xpsused = 0) then exit; // no sections
  xitcur := 0;
  assert(xpsecs[0].parent = -1);
  result := true;
end;


// from current item to eol (not including children, but including current item)
function xprofItCount (): Integer;
var
  idx: Integer;
begin
  result := 0;
  idx := xitcur;
  while (idx <> -1) do
  begin
    Inc(result);
    idx := xpsecs[idx].next;
  end;
end;


// current item info
function xprofItName (): AnsiString; begin if (xitcur = -1) then result := '' else result := xpsecs[xitcur].name; end;
function xprofItMicro (): Int64; begin if (xitcur = -1) then result := 0 else result := xpsecs[xitcur].timer.elapsedMicro; end;
function xprofItMilli (): Int64; begin if (xitcur = -1) then result := 0 else result := xpsecs[xitcur].timer.elapsedMilli; end;
function xprofItHasChildren (): Boolean; begin if (xitcur = -1) then result := false else result := (xpsecs[xitcur].firstChild <> -1); end;
function xprofItIsChild (): Boolean; begin if (xitcur = -1) then result := false else result := (xpsecs[xitcur].parent <> -1); end;
function xprofItDepth (): Integer; begin result := xitdepth; end;

// dive into childrens
function xprofItDive (): Boolean;
begin
  if (xitcur = -1) or (xpsecs[xitcur].firstChild = -1) then
  begin
    result := false;
  end
  else
  begin
    result := true;
    xitcur := xpsecs[xitcur].firstChild;
    Inc(xitdepth);
  end;
end;

// pop into parent
function xprofItPop (): Boolean;
begin
  if (xitcur = -1) or (xpsecs[xitcur].parent = -1) then
  begin
    result := false;
  end
  else
  begin
    result := true;
    xitcur := xpsecs[xitcur].parent;
    Dec(xitdepth);
  end;
end;

// move to next sibling; false: no more siblings (and current item is unchanged)
function xprofItNext (): Boolean;
begin
  if (xitcur = -1) or (xpsecs[xitcur].next = -1) then
  begin
    result := false;
  end
  else
  begin
    result := true;
    xitcur := xpsecs[xitcur].next;
  end;
end;

{$ELSE}
procedure xprofBegin (reallyActivate: Boolean=true); begin end;
procedure xprofEnd (); begin end;
procedure xprofBeginSection (name: AnsiString); begin end;
procedure xprofEndSection (); begin end;

function xprofTotalMicro (): Int64; begin result := 0; end;
function xprofTotalMilli (): Int64; begin result := 0; end;
function xprofTotalCount (): Integer; begin result := 0; end;

function xprofItReset (): Boolean; begin result := false; end;
function xprofItCount (): Integer; begin result := 0; end;
// current item info
function xprofItName (): AnsiString; begin result := ''; end;
function xprofItMicro (): Int64; begin result := 0; end;
function xprofItMilli (): Int64; begin result := 0; end;
function xprofItHasChildren (): Boolean; begin result := false; end;
function xprofItIsChild (): Boolean; begin result := false; end;

function xprofItDepth (): Integer; begin result := 0; end;

function xprofItDive (): Boolean; begin result := false; end;
function xprofItPop (): Boolean; begin result := false; end;
function xprofItNext (): Boolean; begin result := false; end;
{$ENDIF}

begin
  {$IF DEFINED(STOPWATCH_IS_HERE)}
  xprofGlobalInit();
  {$ENDIF}
end.
