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
// TStopWatch is based on the code by Inoussa OUEDRAOGO, Copyright (c) 2012
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
    procedure clear (); inline; // full clear
    procedure start (); // start or restart timer
    procedure stop ();

    property elapsedMilli: Int64 read getElapsedMilliseconds;
    property elapsedMicro: Int64 read getElapsedMicroseconds;
    property elapsedTicks: Int64 read getElapsedTicks;
    property isRunning: Boolean read mRunning;
  end;
{$ENDIF}


const
  TProfHistorySize = 100;

type
  TProfilerBar = record
  private
    const FilterFadeoff = 0.05; // 5%

  private
    history: array [0..TProfHistorySize-1] of Integer; // circular buffer
    hisHead: Integer;
    curval: Single;
    mName: AnsiString;
    mLevel: Integer;

  private
    procedure initialize (); inline;
    function getvalue (): Integer; inline;
    function getvalat (idx: Integer): Integer; inline;
    function getcount (): Integer; inline;

  public
    procedure update (val: Integer);

    property value: Integer read getvalue;
    property name: AnsiString read mName;
    property level: Integer read mLevel;
    property count: Integer read getcount;
    property values[idx: Integer]: Integer read getvalat;
  end;

  TProfiler = class(TObject)
  public
    bars: array of TProfilerBar;
    name: AnsiString;

  public
    constructor Create (aName: AnsiString);
    destructor Destroy (); override;

    procedure mainBegin (reallyActivate: Boolean=true);
    procedure mainEnd ();

    procedure sectionBegin (name: AnsiString);
    procedure sectionEnd ();
  end;


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

function xprofNameAt (idx: Integer): AnsiString;
function xprofMicroAt (idx: Integer): Int64;
function xprofMilliAt (idx: Integer): Int64;
function xprofHasChildrenAt (idx: Integer): Boolean;
function xprofLevelAt (idx: Integer): Integer;

// iterator
function xprofItReset (): Boolean; // false: no sections
function xprofItCount (): Integer; // from current item to eol (not including children, but including current item)
// current item info
function xprofItName (): AnsiString; // current section name
function xprofItMicro (): Int64; // current section duration, microseconds
function xprofItMilli (): Int64; // current section duration, milliseconds
function xprofItHasChildren (): Boolean;
function xprofItIsChild (): Boolean;
function xprofItLevel (): Integer; // 0: top

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
  result.clear();
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


procedure TStopWatch.clear ();
begin
  //FillChar(self, sizeof(self), 0);
  mElapsed := 0;
  mRunning := false;
  //mStartPosition: TBaseMesure;
end;


procedure TStopWatch.start ();
begin
  //if mRunning then exit;
  if (mFrequency = 0) then initTimerIntr();
  mRunning := true;
  mElapsed := 0;
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
    level: Integer;
  end;

var
  xptimer: TStopWatch;
  xpsecs: array of TProfSection = nil;
  xpsused: Integer = 0;
  xpscur: Integer = -1; // currently running section
  xpslevel: Integer = 0;
  xitcur: Integer = -1; // for iterator


// call this on frame start
procedure xprofBegin (reallyActivate: Boolean=true);
begin
  xpsused := 0;
  xpscur := -1;
  xitcur := -1; // reset iterator
  xpslevel := 0;
  xptimer.clear();
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
//FIXME: rewrite without schlemiel's algo!
procedure xprofBeginSection (name: AnsiString);
var
  sid, t: Integer;
  pss: PProfSection;
begin
  if not xptimer.isRunning then exit;
  if (Length(xpsecs) = 0) then SetLength(xpsecs, 65536); // why not?
  if (xpsused >= Length(xpsecs)) then raise Exception.Create('too many profile sections');
  sid := xpsused;
  Inc(xpsused);
  pss := @xpsecs[sid];
  pss.name := name;
  pss.timer.clear();
  pss.parent := xpscur;
  pss.firstChild := -1;
  pss.next := -1;
  pss.level := xpslevel;
  Inc(xpslevel);
  // link to list
  if (xpscur <> -1) then
  begin
    // child
    t := xpsecs[xpscur].firstChild;
    if (t = -1) then
    begin
      xpsecs[xpscur].firstChild := sid;
    end
    else
    begin
      while (xpsecs[t].next <> -1) do t := xpsecs[t].next;
      xpsecs[t].next := sid;
    end;
  end
  else
  begin
    // top level
    if (sid <> 0) then
    begin
      t := 0;
      while (xpsecs[t].next <> -1) do t := xpsecs[t].next;
      xpsecs[t].next := sid;
    end;
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
  Dec(xpslevel);
  pss := @xpsecs[xpscur];
  pss.timer.stop();
  // go back to parent
  xpscur := pss.parent;
end;


procedure xprofGlobalInit ();
begin
  //SetLength(xpsecs, 1024); // 'cause why not? 'cause don't pay for something you may not need
  xptimer.clear();
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


function xprofNameAt (idx: Integer): AnsiString; begin if xptimer.isRunning or (idx < 0) or (idx >= xpsused) then result := '' else result := xpsecs[idx].name; end;
function xprofMicroAt (idx: Integer): Int64; begin if xptimer.isRunning or (idx < 0) or (idx >= xpsused) then result := 0 else result := xpsecs[idx].timer.elapsedMicro; end;
function xprofMilliAt (idx: Integer): Int64; begin if xptimer.isRunning or (idx < 0) or (idx >= xpsused) then result := 0 else result := xpsecs[idx].timer.elapsedMilli; end;
function xprofHasChildrenAt (idx: Integer): Boolean; begin if xptimer.isRunning or (idx < 0) or (idx >= xpsused) then result := false else result := (xpsecs[idx].firstChild <> -1); end;
function xprofLevelAt (idx: Integer): Integer; begin if xptimer.isRunning or (idx < 0) or (idx >= xpsused) then result := 0 else result := xpsecs[idx].level; end;


// false: no sections
function xprofItReset (): Boolean;
begin
  result := false;
  xitcur := -1;
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
function xprofItLevel (): Integer; begin if (xitcur = -1) then result := 0 else result := xpsecs[xitcur].level; end;

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

function xprofNameAt (idx: Integer): AnsiString; begin result := ''; end;
function xprofMicroAt (idx: Integer): Int64; begin result := 0; end;
function xprofMilliAt (idx: Integer): Int64; begin result := 0; end;
function xprofHasChildrenAt (idx: Integer): Boolean; begin result := false; end;
function xprofLevelAt (idx: Integer): Integer; begin result := 0; end;

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


// ////////////////////////////////////////////////////////////////////////// //
procedure TProfilerBar.initialize (); begin hisHead := -1; curval := 0; end;

procedure TProfilerBar.update (val: Integer);
var
  idx: Integer;
begin
  if (val < 0) then val := 0; //else if (val > 1000000) val := 1000000;
  if (hisHead = -1) then begin hisHead := 0; curval := 0; for idx := 0 to TProfHistorySize-1 do history[idx] := val; end;
  history[hisHead] := val;
  Inc(hisHead);
  if (hisHead = TProfHistorySize) then hisHead := 0;
  curval := FilterFadeoff*val+(1.0-FilterFadeoff)*curval;
end;

function TProfilerBar.getvalue (): Integer; begin result := round(curval); end;

function TProfilerBar.getcount (): Integer; begin result := TProfHistorySize; end;

function TProfilerBar.getvalat (idx: Integer): Integer;
begin
  if (idx < 0) or (idx >= TProfHistorySize) then result := 0 else result := history[(hisHead-idx-1+TProfHistorySize*2) mod TProfHistorySize];
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TProfiler.Create (aName: AnsiString);
begin
  name := aName;
  bars := nil;
end;


destructor TProfiler.Destroy ();
begin
  bars := nil;
  inherited;
end;


procedure TProfiler.mainBegin (reallyActivate: Boolean=true);
begin
  xprofBegin(reallyActivate);
end;

procedure TProfiler.mainEnd ();
var
  idx: Integer;
begin
  xprofEnd();
  if (xprofTotalCount > 0) then
  begin
    // first time?
    if (length(bars) = 0) or (length(bars) <> xprofTotalCount+1) then
    begin
      //if (length(bars) <> 0) then raise Exception.Create('FUUUUUUUUUUUUUUU');
      SetLength(bars, xprofTotalCount+1);
      for idx := 1 to xprofTotalCount do
      begin
        bars[idx].initialize();
        bars[idx].mName := xprofNameAt(idx-1);
        bars[idx].mLevel := xprofLevelAt(idx-1)+1;
      end;
      bars[0].initialize();
      bars[0].mName := name;
      bars[0].mLevel := 0;
    end;
    // update bars
    for idx := 1 to xprofTotalCount do bars[idx].update(xprofMicroAt(idx-1));
    bars[0].update(xprofTotalMicro);
  end;
end;

procedure TProfiler.sectionBegin (name: AnsiString);
begin
  xprofBeginSection(name);
end;

procedure TProfiler.sectionEnd ();
begin
  xprofEndSection();
end;


begin
  {$IF DEFINED(STOPWATCH_IS_HERE)}
  xprofGlobalInit();
  {$ENDIF}
end.
