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
unit sdlcarcass;

interface

uses
  SDL2, fui_events;


// ////////////////////////////////////////////////////////////////////////// //
// call this with SDL2 event; returns `true` if event was eaten
function fuiOnSDLEvent (var ev: TSDL_Event): Boolean;


// ////////////////////////////////////////////////////////////////////////// //
// event handlers
var
  winFocusCB: procedure () = nil; // this will be called when window got focus; `fuiWinActive` already set
  winBlurCB: procedure () = nil; // this will be called when window lost focus; `fuiWinActive` already set
  // for standalone
  buildFrameCB: procedure () = nil; // don't do any rendering here, do it in `renderFrameCB()`
  renderFrameCB: procedure () = nil; // no need to call `glSwap()` here
  exposeFrameCB: procedure () = nil; // call `glSwap()` here instead; automatically set by standalone
  //
  prerenderFrameCB: procedure () = nil;
  postrenderFrameCB: procedure () = nil;
  fuiResizeCB: procedure () = nil; // `fuiScrWdt` and `fuiScrHgt` are already set
  oglInitCB: procedure () = nil; // `fuiScrWdt` and `fuiScrHgt` are already set
  oglDeinitCB: procedure () = nil;


var
  // default size
  fuiScrWdt: Integer = 1024;
  fuiScrHgt: Integer = 768;
  fuiWinActive: Boolean = false;
  fuiQuitReceived: Boolean = false;


// ////////////////////////////////////////////////////////////////////////// //
function fuiTimeMicro (): UInt64; inline;
function fuiTimeMilli (): UInt64; inline;


// ////////////////////////////////////////////////////////////////////////// //
// only for standalone mode
function getFUIFPS (): Integer; inline;
procedure setFUIFPS (v: Integer); inline;

property fuiFPS: Integer read getFUIFPS write setFUIFPS; // default: 30


implementation

uses
  SysUtils, Classes,
  GL, GLExt,
  {$IF DEFINED(LINUX)}
    unixtype, linux
  {$ELSEIF DEFINED(WINDOWS)}
    Windows
  {$ELSE}
    {$WARNING You suck!}
  {$ENDIF}
  ;


// ////////////////////////////////////////////////////////////////////////// //
var
  gEffFPS: Integer = 30;

function getFUIFPS (): Integer; inline; begin result := gEffFPS; end;
procedure setFUIFPS (v: Integer); inline; begin if (v < 1) then v := 1 else if (v > 60*4) then v := 60*4; gEffFPS := v; end;


// ////////////////////////////////////////////////////////////////////////// //
{$IF DEFINED(LINUX)}
type THPTimeType = TTimeSpec;
{$ELSE}
type THPTimeType = Int64;
{$ENDIF}

var
  mFrequency: Int64 = 0;
  mHasHPTimer: Boolean = false;

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


function fuiTimeMicro (): UInt64; inline;
var
  r: THPTimeType;
begin
  //if (mFrequency = 0) then initTimerIntr();
  {$IF DEFINED(LINUX)}
  clock_gettime(CLOCK_MONOTONIC, @r);
  result := UInt64(r.tv_sec)*1000000+UInt64(r.tv_nsec) div 1000; // microseconds
  {$ELSE}
  QueryPerformanceCounter(r);
  result := UInt64(r)*1000000 div mFrequency;
  {$ENDIF}
end;


function fuiTimeMilli (): UInt64; inline;
begin
  result := fuiTimeMicro() div 1000;
end;


// ////////////////////////////////////////////////////////////////////////// //
function fuiOnSDLEvent (var ev: TSDL_Event): Boolean;
var
  mev: THMouseEvent;
  kev: THKeyEvent;

  function buildBut (b: Byte): Word;
  begin
    result := 0;
    case b of
      SDL_BUTTON_LEFT: result := result or THMouseEvent.Left;
      SDL_BUTTON_MIDDLE: result := result or THMouseEvent.Middle;
      SDL_BUTTON_RIGHT: result := result or THMouseEvent.Right;
    end;
  end;

  procedure windowEventHandler (constref ev: TSDL_WindowEvent);
  begin
    case ev.event of
      SDL_WINDOWEVENT_MINIMIZED: if fuiWinActive then begin fuiResetKMState(true); fuiWinActive := false; if assigned(winBlurCB) then winBlurCB(); end;
      SDL_WINDOWEVENT_RESIZED, SDL_WINDOWEVENT_SIZE_CHANGED:
        begin
          if (ev.data1 <> fuiScrWdt) or (ev.data2 <> fuiScrHgt) then
          begin
            fuiScrWdt := ev.data1;
            fuiScrHgt := ev.data2;
            if assigned(fuiResizeCB) then fuiResizeCB();
          end;
        end;
      SDL_WINDOWEVENT_EXPOSED: if assigned(exposeFrameCB) then exposeFrameCB();
      SDL_WINDOWEVENT_FOCUS_GAINED: if not fuiWinActive then begin fuiWinActive := true; if assigned(winFocusCB) then winFocusCB(); end;
      SDL_WINDOWEVENT_FOCUS_LOST: if fuiWinActive then begin fuiResetKMState(true); fuiWinActive := false; if assigned(winBlurCB) then winBlurCB(); end;
    end;
  end;

begin
  result := false;

  case ev.type_ of
    SDL_WINDOWEVENT: windowEventHandler(ev.window);
    SDL_QUITEV: fuiQuitReceived := true;

    SDL_KEYDOWN, SDL_KEYUP:
      begin
        // fix left/right modifiers
        FillChar(kev, sizeof(kev), 0);
        kev.intrInit();
        if (ev.type_ = SDL_KEYDOWN) then kev.kind := THKeyEvent.TKind.Press else kev.kind := THKeyEvent.TKind.Release;
        kev.scan := ev.key.keysym.scancode;
        //kev.sym := ev.key.keysym.sym;

        if (kev.scan = SDL_SCANCODE_RCTRL) then kev.scan := SDL_SCANCODE_LCTRL;
        if (kev.scan = SDL_SCANCODE_RALT) then kev.scan := SDL_SCANCODE_LALT;
        if (kev.scan = SDL_SCANCODE_RSHIFT) then kev.scan := SDL_SCANCODE_LSHIFT;
        if (kev.scan = SDL_SCANCODE_RGUI) then kev.scan := SDL_SCANCODE_LGUI;

        {
        if (kev.sym = SDLK_RCTRL) then kev.sym := SDLK_LCTRL;
        if (kev.sym = SDLK_RALT) then kev.sym := SDLK_LALT;
        if (kev.sym = SDLK_RSHIFT) then kev.sym := SDLK_LSHIFT;
        if (kev.sym = SDLK_RGUI) then kev.sym := SDLK_LGUI;
        }

        kev.x := fuiMouseX;
        kev.y := fuiMouseY;
        kev.bstate := fuiButState;
        kev.kstate := fuiModState;

        case kev.scan of
          SDL_SCANCODE_LCTRL: if (kev.press) then fuiSetModState(fuiModState or THKeyEvent.ModCtrl) else fuiSetModState(fuiModState and (not THKeyEvent.ModCtrl));
          SDL_SCANCODE_LALT: if (kev.press) then fuiSetModState(fuiModState or THKeyEvent.ModAlt) else fuiSetModState(fuiModState and (not THKeyEvent.ModAlt));
          SDL_SCANCODE_LSHIFT: if (kev.press) then fuiSetModState(fuiModState or THKeyEvent.ModShift) else fuiSetModState(fuiModState and (not THKeyEvent.ModShift));
        end;

        if assigned(evKeyCB) then
        begin
          evKeyCB(kev);
          result := kev.eaten;
        end;
      end;

    SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP:
      begin
        FillChar(mev, sizeof(mev), 0);
        mev.intrInit();
        if (ev.type_ = SDL_MOUSEBUTTONDOWN) then mev.kind := THMouseEvent.TKind.Press else mev.kind := THMouseEvent.TKind.Release;
        mev.dx := ev.button.x-fuiMouseX;
        mev.dy := ev.button.y-fuiMouseY;
        fuiSetMouseX(ev.button.x);
        fuiSetMouseY(ev.button.y);
        mev.but := buildBut(ev.button.button);
        mev.x := fuiMouseX;
        mev.y := fuiMouseY;
        mev.bstate := fuiButState;
        mev.kstate := fuiModState;
        if (mev.but <> 0) then
        begin
          // ev.button.clicks: Byte
          if (ev.type_ = SDL_MOUSEBUTTONDOWN) then fuiSetButState(fuiButState or mev.but) else fuiSetButState(fuiButState and (not mev.but));
          if assigned(evMouseCB) then
          begin
            evMouseCB(mev);
            result := mev.eaten;
          end;
        end;
      end;
    SDL_MOUSEWHEEL:
      begin
        if (ev.wheel.y <> 0) then
        begin
          FillChar(mev, sizeof(mev), 0);
          mev.intrInit();
          mev.kind := THMouseEvent.TKind.Press;
          mev.dx := 0;
          mev.dy := ev.wheel.y;
          if (ev.wheel.y < 0) then mev.but := THMouseEvent.WheelUp else mev.but := THMouseEvent.WheelDown;
          mev.x := fuiMouseX;
          mev.y := fuiMouseY;
          mev.bstate := fuiButState;
          mev.kstate := fuiModState;
          if assigned(evMouseCB) then
          begin
            evMouseCB(mev);
            result := mev.eaten;
          end;
        end;
      end;
    SDL_MOUSEMOTION:
      begin
        FillChar(mev, sizeof(mev), 0);
        mev.intrInit();
        mev.kind := THMouseEvent.TKind.Motion;
        mev.dx := ev.button.x-fuiMouseX;
        mev.dy := ev.button.y-fuiMouseY;
        fuiSetMouseX(ev.button.x);
        fuiSetMouseY(ev.button.y);
        mev.but := 0;
        mev.x := fuiMouseX;
        mev.y := fuiMouseY;
        mev.bstate := fuiButState;
        mev.kstate := fuiModState;
        if assigned(evMouseCB) then
        begin
          evMouseCB(mev);
          result := mev.eaten;
        end;
      end;

    {
    SDL_TEXTINPUT:
      begin
        Utf8ToUnicode(@uc, PChar(ev.text.text), 1);
        keychr := Word(uc);
        if (keychr > 127) then keychr := Word(wchar2win(WideChar(keychr)));
        CharPress(AnsiChar(keychr));
      end;
    }
  end;
end;


begin
  initTimerIntr();
  fuiWinActive := fuiWinActive;
  fuiScrWdt := fuiScrWdt;
  fuiScrHgt := fuiScrHgt;
end.
