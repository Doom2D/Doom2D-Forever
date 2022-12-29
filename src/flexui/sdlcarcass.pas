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
  {$IF DEFINED(LINUX) OR DEFINED(ANDROID)}
    unixtype, linux
  {$ELSEIF DEFINED(WINDOWS)}
    Windows
  {$ELSEIF DEFINED(HAIKU) OR DEFINED(UNIX)}
    unixtype
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
{$ELSEIF DEFINED(WINDOWS)}
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
  {$ELSEIF DEFINED(WINDOWS)}
  QueryPerformanceCounter(r);
  result := UInt64(r)*1000000 div mFrequency;
  {$ENDIF}
end;


function fuiTimeMilli (): UInt64; inline;
begin
  result := fuiTimeMicro() div 1000;
end;


// ////////////////////////////////////////////////////////////////////////// //
var
  wc2shitmap: array[0..65535] of AnsiChar;
  wc2shitmapInited: Boolean = false;


// ////////////////////////////////////////////////////////////////////////// //
const
  cp1251: array[0..127] of Word = (
    $0402,$0403,$201A,$0453,$201E,$2026,$2020,$2021,$20AC,$2030,$0409,$2039,$040A,$040C,$040B,$040F,
    $0452,$2018,$2019,$201C,$201D,$2022,$2013,$2014,$003F,$2122,$0459,$203A,$045A,$045C,$045B,$045F,
    $00A0,$040E,$045E,$0408,$00A4,$0490,$00A6,$00A7,$0401,$00A9,$0404,$00AB,$00AC,$00AD,$00AE,$0407,
    $00B0,$00B1,$0406,$0456,$0491,$00B5,$00B6,$00B7,$0451,$2116,$0454,$00BB,$0458,$0405,$0455,$0457,
    $0410,$0411,$0412,$0413,$0414,$0415,$0416,$0417,$0418,$0419,$041A,$041B,$041C,$041D,$041E,$041F,
    $0420,$0421,$0422,$0423,$0424,$0425,$0426,$0427,$0428,$0429,$042A,$042B,$042C,$042D,$042E,$042F,
    $0430,$0431,$0432,$0433,$0434,$0435,$0436,$0437,$0438,$0439,$043A,$043B,$043C,$043D,$043E,$043F,
    $0440,$0441,$0442,$0443,$0444,$0445,$0446,$0447,$0448,$0449,$044A,$044B,$044C,$044D,$044E,$044F
  );


procedure initShitMap ();
var
  f: Integer;
begin
  for f := 0 to High(wc2shitmap) do wc2shitmap[f] := '?';
  for f := 0 to 127 do wc2shitmap[f] := AnsiChar(f);
  for f := 0 to 127 do wc2shitmap[cp1251[f]] := AnsiChar(f+128);
  wc2shitmapInited := true;
end;


function wchar2win (wc: WideChar): AnsiChar; inline;
begin
  if not wc2shitmapInited then initShitMap();
  if (LongWord(wc) > 65535) then result := '?' else result := wc2shitmap[LongWord(wc)];
end;


// ////////////////////////////////////////////////////////////////////////// //
function fuiOnSDLEvent (var ev: TSDL_Event): Boolean;
var
  fev: TFUIEvent;
  uc: UnicodeChar;
  keychr: Word;

  function buildBut (b: Byte): Word;
  begin
    result := 0;
    case b of
      SDL_BUTTON_LEFT: result := result or TFUIEvent.Left;
      SDL_BUTTON_MIDDLE: result := result or TFUIEvent.Middle;
      SDL_BUTTON_RIGHT: result := result or TFUIEvent.Right;
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
        if (ev.type_ = SDL_KEYDOWN) then
        begin
          fev := TFUIEvent.Create(TFUIEvent.TType.Key, TFUIEvent.TKind.Press);
        end
        else
        begin
          fev := TFUIEvent.Create(TFUIEvent.TType.Key, TFUIEvent.TKind.Release);
        end;
        fev.scan := ev.key.keysym.scancode;

        if (fev.scan = SDL_SCANCODE_RCTRL) then fev.scan := SDL_SCANCODE_LCTRL;
        if (fev.scan = SDL_SCANCODE_RALT) then fev.scan := SDL_SCANCODE_LALT;
        if (fev.scan = SDL_SCANCODE_RSHIFT) then fev.scan := SDL_SCANCODE_LSHIFT;
        if (fev.scan = SDL_SCANCODE_RGUI) then fev.scan := SDL_SCANCODE_LGUI;

        fev.x := fuiMouseX;
        fev.y := fuiMouseY;
        fev.bstate := fuiButState;
        fev.kstate := fuiModState;

        case fev.scan of
          SDL_SCANCODE_LCTRL: if (fev.press) then fuiSetModState(fuiModState or TFUIEvent.ModCtrl) else fuiSetModState(fuiModState and (not TFUIEvent.ModCtrl));
          SDL_SCANCODE_LALT: if (fev.press) then fuiSetModState(fuiModState or TFUIEvent.ModAlt) else fuiSetModState(fuiModState and (not TFUIEvent.ModAlt));
          SDL_SCANCODE_LSHIFT: if (fev.press) then fuiSetModState(fuiModState or TFUIEvent.ModShift) else fuiSetModState(fuiModState and (not TFUIEvent.ModShift));
        end;

        if (assigned(fuiEventCB)) then
        begin
          fuiEventCB(fev);
          result := fev.eaten;
        end;
      end;

    SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP:
      begin
        if (ev.type_ = SDL_MOUSEBUTTONDOWN) then
        begin
          fev := TFUIEvent.Create(TFUIEvent.TType.Mouse, TFUIEvent.TKind.Press);
        end
        else
        begin
          fev := TFUIEvent.Create(TFUIEvent.TType.Mouse, TFUIEvent.TKind.Release);
        end;
        fev.dx := ev.button.x-fuiMouseX;
        fev.dy := ev.button.y-fuiMouseY;
        fuiSetMouseX(ev.button.x);
        fuiSetMouseY(ev.button.y);
        fev.but := buildBut(ev.button.button);
        fev.x := fuiMouseX;
        fev.y := fuiMouseY;
        fev.bstate := fuiButState;
        fev.kstate := fuiModState;
        if (fev.but <> 0) then
        begin
          // ev.button.clicks: Byte
          if (ev.type_ = SDL_MOUSEBUTTONDOWN) then fuiSetButState(fuiButState or fev.but) else fuiSetButState(fuiButState and (not fev.but));
          if (assigned(fuiEventCB)) then
          begin
            fuiEventCB(fev);
            result := fev.eaten;
          end;
        end;
      end;
    SDL_MOUSEWHEEL:
      begin
        if (ev.wheel.y <> 0) then
        begin
          fev := TFUIEvent.Create(TFUIEvent.TType.Mouse, TFUIEvent.TKind.Press);
          fev.dx := 0;
          fev.dy := ev.wheel.y;
          if (ev.wheel.y < 0) then fev.but := TFUIEvent.WheelUp else fev.but := TFUIEvent.WheelDown;
          fev.x := fuiMouseX;
          fev.y := fuiMouseY;
          fev.bstate := fuiButState;
          fev.kstate := fuiModState;
          if (assigned(fuiEventCB)) then
          begin
            fuiEventCB(fev);
            result := fev.eaten;
          end;
        end;
      end;
    SDL_MOUSEMOTION:
      begin
        fev := TFUIEvent.Create(TFUIEvent.TType.Mouse, TFUIEvent.TKind.Motion);
        fev.dx := ev.button.x-fuiMouseX;
        fev.dy := ev.button.y-fuiMouseY;
        fuiSetMouseX(ev.button.x);
        fuiSetMouseY(ev.button.y);
        fev.but := 0;
        fev.x := fuiMouseX;
        fev.y := fuiMouseY;
        fev.bstate := fuiButState;
        fev.kstate := fuiModState;
        if (assigned(fuiEventCB)) then
        begin
          fuiEventCB(fev);
          result := fev.eaten;
        end;
      end;

    SDL_TEXTINPUT:
      if ((fuiModState and (not TFUIEvent.ModShift)) = 0) then
      begin
        Utf8ToUnicode(@uc, PChar(ev.text.text), 1);
        keychr := Word(uc);
        if (keychr > 127) then keychr := Word(wchar2win(WideChar(keychr)));
        if (keychr > 0) and (assigned(fuiEventCB)) then
        begin
          fev := TFUIEvent.Create(TFUIEvent.TType.Key, TFUIEvent.TKind.SimpleChar);
          fev.ch := AnsiChar(keychr);
          fev.x := fuiMouseX;
          fev.y := fuiMouseY;
          fev.bstate := fuiButState;
          fev.kstate := fuiModState;
          fuiEventCB(fev);
          result := fev.eaten;
        end;
      end;
  end;
end;


begin
  initTimerIntr();
  fuiWinActive := fuiWinActive;
  fuiScrWdt := fuiScrWdt;
  fuiScrHgt := fuiScrHgt;
end.
