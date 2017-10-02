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
unit sdlstandalone;

interface

uses
  SDL2,
  sdlcarcass;


// ////////////////////////////////////////////////////////////////////////// //
// initialize OpenGL; set `gScreenWidth` and `gScreenHeight` before calling this
function glInit (const winTitle: AnsiString='SDL TEST'): Boolean;
procedure glDeinit ();
// call this to show built frame
procedure glSwap ();
// call this to push "quit" event into queue
procedure pushQuitEvent ();
// call this to process queued messages; result is `true` if quit event was received
function processMessages (): Boolean;

// run main loop, call `buildFrameCB()` and `renderFrameCB()`, maintain the given FPS
procedure mainLoop ();


implementation

uses
  SysUtils;


var
  gWinH: PSDL_Window = nil;
  gGLContext: TSDL_GLContext = nil;
  lastFrameTime: UInt64 = 0;


// ////////////////////////////////////////////////////////////////////////// //
procedure onExposeFrame ();
begin
  glSwap();
end;


// ////////////////////////////////////////////////////////////////////////// //
function sdlInit (): Boolean;
var
  sdlflags: LongWord;
begin
  result := false;

  sdlflags := SDL_INIT_TIMER or SDL_INIT_VIDEO;
  if SDL_Init(sdlflags) < 0 then exit; //raise Exception.Create('SDL: Init failed: ' + SDL_GetError());

  //SDL_Quit();
  result := true;
  fuiWinActive := fuiWinActive;
end;


procedure glSwap ();
begin
  if (gWinH = nil) then exit;
  SDL_GL_SwapWindow(gWinH);
end;


procedure killGLWindow ();
begin
  if (gWinH <> nil) then SDL_DestroyWindow(gWinH);
  if (gGLContext <> nil) then SDL_GL_DeleteContext(gGLContext);
  gWinH := nil;
  gGLContext := nil;
end;


procedure pushQuitEvent ();
var
  ev: TSDL_Event;
begin
  ev.type_ := SDL_QUITEV;
  SDL_PushEvent(@ev);
end;


// ////////////////////////////////////////////////////////////////////////// //
// true: quit
function processMessages (): Boolean;
var
  ev: TSDL_Event;
begin
  result := false;
  FillChar(ev, sizeof(ev), 0);
  while (SDL_PollEvent(@ev) > 0) do
  begin
    if fuiOnSDLEvent(ev) then result := true;
    //if (ev.type_ = SDL_QUITEV) then exit;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure glDeinit ();
begin
  if (gWinH <> nil) and assigned(oglDeinitCB) then oglDeinitCB();
  killGLWindow();
end;


function glInit (const winTitle: AnsiString='SDL TEST'): Boolean;
var
  wFlags: LongWord = 0;
  v: Byte = 0;
begin
  result := false;

  wFlags := SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE;
  //if gFullscreen then wFlags := wFlags or SDL_WINDOW_FULLSCREEN;
  //if gWinMaximized then wFlags := wFlags or SDL_WINDOW_MAXIMIZED;

  glDeinit();

  //if VSync then v := 1 else v := 0;
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 1);
  SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 1); // lights; it is enough to have 1-bit stencil buffer for lighting
  SDL_GL_SetSwapInterval(v);

  {
  if gFullscreen then
  begin
    mode.w := gScreenWidth;
    mode.h := gScreenHeight;
    mode.format := 0;
    mode.refresh_rate := 0;
    mode.driverdata := nil;
    if SDL_GetClosestDisplayMode(0, @mode, @cmode) = nil then
    begin
      gScreenWidth := 800;
      gScreenHeight := 600;
    end
    else
    begin
      gScreenWidth := cmode.w;
      gScreenHeight := cmode.h;
    end;
  end;
  }

  gWinH := SDL_CreateWindow(PAnsiChar(winTitle), -1, -1, fuiScrWdt, fuiScrHgt, wFlags);
  if (gWinH = nil) then exit;

  gGLContext := SDL_GL_CreateContext(gWinH);
  if (gGLContext = nil) then begin SDL_DestroyWindow(gWinH); gWinH := nil; exit; end;

  SDL_GL_MakeCurrent(gWinH, gGLContext);
  SDL_ShowCursor(SDL_DISABLE);

  if assigned(oglInitCB) then oglInitCB();

  result := true;
end;


// run main loop, call `buildFrameCB()` and `renderFrameCB()`, maintain the given FPS
procedure mainLoop ();
var
  nft, ctt: UInt64;
  wt: Integer;
begin
  if assigned(buildFrameCB) then buildFrameCB();
  if assigned(prerenderFrameCB) then prerenderFrameCB();
  if assigned(renderFrameCB) then renderFrameCB();
  if assigned(postrenderFrameCB) then postrenderFrameCB();
  glSwap();
  lastFrameTime := fuiTimeMilli();
  while true do
  begin
    // calculate time to build and render next frame
    nft := lastFrameTime+(1000 div fuiFPS);
    ctt := fuiTimeMilli();
    if (ctt >= nft) then
    begin
      // time to build next frame
      if assigned(buildFrameCB) then buildFrameCB();
      if assigned(prerenderFrameCB) then prerenderFrameCB();
      if assigned(renderFrameCB) then renderFrameCB();
      if assigned(postrenderFrameCB) then postrenderFrameCB();
      glSwap();
      lastFrameTime := ctt; // ignore frame processing time
    end
    else
    begin
      // has to wait for some time
      if (nft-ctt > 1000) then wt := 1000 else wt := Integer(nft-ctt);
      SDL_WaitEventTimeout(nil, wt);
    end;
    if processMessages() then break; // just in case
  end;
end;


initialization
  exposeFrameCB := onExposeFrame();

  if not sdlInit() then raise Exception.Create('cannot initialize SDL');
finalization
  glDeinit();
  SDL_Quit();
end.
