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
  strict private
    mElapsed: Int64;
    mRunning: Boolean;
    mStartPosition: UInt64;

  strict private
    procedure updateElapsed ();

    function getElapsedMicro (): Int64;
    function getElapsedMilli (): Int64;

  public
    class function Create (): TStopWatch; static;
    class function startNew (): TStopWatch; static;

  public
    procedure clear (); inline; // full clear
    procedure start (reset: Boolean=true); // start or restart timer
    procedure stop ();
    // the following is like start/stop, but doesn't reset elapsed time
    procedure pause ();
    procedure resume ();

    property elapsedMicro: Int64 read getElapsedMicro;
    property elapsedMilli: Int64 read getElapsedMilli;
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
  private
    {$IF DEFINED(STOPWATCH_IS_HERE)}
    type
      PProfSection = ^TProfSection;
      TProfSection = record
        name: AnsiString;
        timer: TStopWatch;
        level: Integer;
        prevAct: Integer; // this serves as stack
      end;

    var
      xptimer: TStopWatch;
      xpsecs: array of TProfSection;
      xpsused: Integer;
      xpscur: Integer; // currently running section
    {$ENDIF}

  public
    bars: array of TProfilerBar; // 0: total time
    name: AnsiString;

  public
    constructor Create (aName: AnsiString);
    destructor Destroy (); override;

    // call this on frame start
    procedure mainBegin (reallyActivate: Boolean=true);
    // call this on frame end
    procedure mainEnd ();

    procedure sectionBegin (name: AnsiString);
    procedure sectionEnd ();

    // this will reuse the section with the given name (if there is any); use `sectionEnd()` to end it as usual
    procedure sectionBeginAccum (name: AnsiString);
  end;


implementation

{$IF DEFINED(LINUX)}
type THPTimeType = TTimeSpec;
{$ELSE}
type THPTimeType = Int64;
{$ENDIF}

var
  mFrequency: Int64 = 0;
  mHasHPTimer: Boolean = false;


// ////////////////////////////////////////////////////////////////////////// //
procedure initTimerIntr ();
var
  r: THPTimeType;
begin
  if (mFrequency = 0) then
  begin
{$IF DEFINED(LINUX)}
    if (clock_getres(CLOCK_MONOTONIC, @r) <> 0) then raise Exception.Create('profiler error: cannot get timer resolution');
    mHasHPTimer := (r.tv_nsec <> 0);
    if not mHasHPTimer then raise Exception.Create('profiler error: hires timer is not available');
    mFrequency := 1; // just a flag
    if (r.tv_nsec <> 0) then mFrequency := 1000000000000000000 div r.tv_nsec;
{$ELSE}
    mHasHPTimer := QueryPerformanceFrequency(r);
    if not mHasHPTimer then raise Exception.Create('profiler error: hires timer is not available');
    mFrequency := r;
{$ENDIF}
  end;
end;


function curTimeMicro (): UInt64; inline;
var
  r: THPTimeType;
begin
  if (mFrequency = 0) then initTimerIntr();
  {$IF DEFINED(LINUX)}
  clock_gettime(CLOCK_MONOTONIC, @r);
  result := UInt64(r.tv_sec)*1000000+UInt64(r.tv_nsec) div 1000; // microseconds
  {$ELSE}
  QueryPerformanceCounter(r);
  result := UInt64(r)*1000000 div mFrequency;
  {$ENDIF}
end;


// ////////////////////////////////////////////////////////////////////////// //
class function TStopWatch.Create (): TStopWatch;
begin
  result.clear();
end;


class function TStopWatch.startNew (): TStopWatch;
begin
  result := TStopWatch.Create();
  result.start();
end;


procedure TStopWatch.updateElapsed ();
var
  e: UInt64;
begin
  e := curTimeMicro();
  if (mStartPosition > e) then mStartPosition := e;
  Inc(mElapsed, e-mStartPosition);
  mStartPosition := e;
end;


function TStopWatch.getElapsedMicro (): Int64;
begin
  if mRunning then updateElapsed();
  result := mElapsed; // microseconds
end;


function TStopWatch.getElapsedMilli (): Int64;
begin
  if mRunning then updateElapsed();
  result := mElapsed div 1000; // milliseconds
end;


procedure TStopWatch.clear ();
begin
  mElapsed := 0;
  mRunning := false;
  mStartPosition := 0;
end;


procedure TStopWatch.start (reset: Boolean=true);
begin
  if mRunning and not reset then exit; // nothing to do
  mStartPosition := curTimeMicro();
  mRunning := true;
  if (reset) then mElapsed := 0;
end;


procedure TStopWatch.stop ();
begin
  if not mRunning then exit;
  mRunning := false;
  updateElapsed();
end;


procedure TStopWatch.pause ();
begin
  stop();
end;


procedure TStopWatch.resume ();
begin
  if mRunning then exit;
  start(false); // don't reset
end;


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

function TProfilerBar.getvalue (): Integer;
//var idx: Integer;
begin
  result := round(curval);
  {
  result := 0;
  for idx := 0 to TProfHistorySize-1 do Inc(result, history[idx]);
  result := result div TProfHistorySize;
  }
end;

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
  {$IF DEFINED(STOPWATCH_IS_HERE)}
  xptimer.clear();
  xpsecs := nil;
  xpsused := 0;
  xpscur := -1;
  {$ENDIF}
end;


destructor TProfiler.Destroy ();
begin
  bars := nil;
  {$IF DEFINED(STOPWATCH_IS_HERE)}
  xpsecs := nil;
  {$ENDIF}
  inherited;
end;


procedure TProfiler.mainBegin (reallyActivate: Boolean=true);
begin
  {$IF DEFINED(STOPWATCH_IS_HERE)}
  xpsused := 0;
  xpscur := -1;
  xptimer.clear();
  if reallyActivate then xptimer.start();
  {$ENDIF}
end;

procedure TProfiler.mainEnd ();
{$IF DEFINED(STOPWATCH_IS_HERE)}
var
  idx: Integer;
  emm: Integer;

  procedure finishProfiling ();
  var
    idx: Integer;
  begin
    if (xpsused > 0) then
    begin
      for idx := 0 to xpsused-1 do
      begin
        xpsecs[idx].timer.stop();
        xpsecs[idx].prevAct := -1;
      end;
    end;
    xptimer.stop();
    xpscur := -1;
  end;
{$ENDIF}
begin
  {$IF DEFINED(STOPWATCH_IS_HERE)}
  if not xptimer.isRunning then exit;
  finishProfiling();
  if (xpsused > 0) then
  begin
    // first time?
    if (length(bars) = 0) or (length(bars) <> xpsused+1) then
    begin
      //if (length(bars) <> 0) then raise Exception.Create('FUUUUUUUUUUUUUUU');
      SetLength(bars, xpsused+1);
      for idx := 1 to xpsused do
      begin
        bars[idx].initialize();
        bars[idx].mName := xpsecs[idx-1].name;
        bars[idx].mLevel := xpsecs[idx-1].level+1;
      end;
      bars[0].initialize();
      bars[0].mName := name;
      bars[0].mLevel := 0;
    end;
    // update bars
    emm := 0;
    for idx := 1 to xpsused do
    begin
      bars[idx].update(Integer(xpsecs[idx-1].timer.elapsedMicro));
      Inc(emm, Integer(xpsecs[idx-1].timer.elapsedMicro));
    end;
    //bars[0].update(xptimer.elapsedMicro);
    bars[0].update(emm);
  end
  else
  begin
    if (length(bars) <> 1) then
    begin
      SetLength(bars, 1);
      bars[0].initialize();
      bars[0].mName := name;
      bars[0].mLevel := 0;
    end;
    bars[0].update(xptimer.elapsedMicro);
  end;
  {$ENDIF}
end;

procedure TProfiler.sectionBegin (name: AnsiString);
{$IF DEFINED(STOPWATCH_IS_HERE)}
var
  sid: Integer;
  pss: PProfSection;
{$ENDIF}
begin
  {$IF DEFINED(STOPWATCH_IS_HERE)}
  if not xptimer.isRunning then exit;
  if (Length(xpsecs) = 0) then SetLength(xpsecs, 512); // why not?
  if (xpsused >= Length(xpsecs)) then raise Exception.Create('too many profile sections');
  sid := xpsused;
  Inc(xpsused);
  pss := @xpsecs[sid];
  pss.name := name;
  pss.timer.clear();
  pss.prevAct := xpscur;
  // calculate level
  if (xpscur = -1) then pss.level := 0 else pss.level := xpsecs[xpscur].level+1;
  xpscur := sid;
  pss.timer.start();
  {$ENDIF}
end;

procedure TProfiler.sectionBeginAccum (name: AnsiString);
{$IF DEFINED(STOPWATCH_IS_HERE)}
var
  idx: Integer;
{$ENDIF}
begin
  {$IF DEFINED(STOPWATCH_IS_HERE)}
  if not xptimer.isRunning then exit;
  if (xpsused > 0) then
  begin
    for idx := 0 to xpsused-1 do
    begin
      if (xpsecs[idx].name = name) then
      begin
        if (idx = xpscur) then raise Exception.Create('profiler error(0): dobule resume: "'+name+'"');
        if (xpsecs[idx].prevAct <> -1) then raise Exception.Create('profiler error(1): dobule resume: "'+name+'"');
        xpsecs[idx].prevAct := xpscur;
        xpscur := idx;
        xpsecs[idx].timer.resume();
        exit;
      end;
    end;
  end;
  sectionBegin(name);
  {$ENDIF}
end;

procedure TProfiler.sectionEnd ();
{$IF DEFINED(STOPWATCH_IS_HERE)}
var
  pss: PProfSection;
{$ENDIF}
begin
  {$IF DEFINED(STOPWATCH_IS_HERE)}
  if not xptimer.isRunning then exit;
  if (xpscur = -1) then exit; // this is bug, but meh...
  pss := @xpsecs[xpscur];
  pss.timer.stop();
  // go back to parent
  xpscur := pss.prevAct;
  pss.prevAct := -1;
  {$ENDIF}
end;


end.
