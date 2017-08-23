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
unit g_holmes;

interface

uses
  e_log,
  g_textures, g_basic, e_graphics, g_phys, g_grid, g_player, g_monsters,
  g_window, g_map, g_triggers, g_items, g_game, g_panel, g_console,
  xprofiler;


type
  THMouseEvent = record
  public
    const
      // both for but and for bstate
      Left = $0001;
      Right = $0002;
      Middle = $0004;
      WheelUp = $0008;
      WheelDown = $0010;

      // event types
      Motion = 0;
      Press = 1;
      Release = 2;

  public
    kind: Byte; // motion, press, release
    x, y: Integer;
    dx, dy: Integer; // for wheel this is wheel motion, otherwise this is relative mouse motion
    but: Word; // current pressed button or 0
    bstate: Word; // button state
    kstate: Word; // keyboard state (see THKeyEvent);
  end;

  THKeyEvent = record
  public
    const
      ModCtrl = $0001;
      ModAlt = $0002;
      ModShift = $0004;
  end;


procedure g_Holmes_VidModeChanged ();
procedure g_Holmes_WindowFocused ();
procedure g_Holmes_WindowBlured ();

procedure g_Holmes_Draw ();

function g_Holmes_mouseEvent (var ev: THMouseEvent): Boolean; // returns `true` if event was eaten
function g_Holmes_KeyEvent (var ev: THKeyEvent): Boolean; // returns `true` if event was eaten

// hooks for player
procedure g_Holmes_plrView (viewPortX, viewPortY, viewPortW, viewPortH: Integer);
procedure g_Holmes_plrLaser (ax0, ay0, ax1, ay1: Integer);


implementation

uses
  SysUtils, GL,
  g_options;


var
  //globalInited: Boolean = false;
  msX: Integer = -666;
  msY: Integer = -666;
  msB: Word = 0; // button state
  kbS: Word = 0; // keyboard modifiers state


// ////////////////////////////////////////////////////////////////////////// //
// cursor (hi, Death Track!)
const curWidth = 17;
const curHeight = 23;

const cursorImg: array[0..curWidth*curHeight-1] of Byte = (
  0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,3,2,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,0,3,2,2,0,0,0,0,0,0,0,0,0,0,0,0,
  1,1,3,3,2,2,0,0,0,0,0,0,0,0,0,0,0,
  1,1,3,3,4,2,2,0,0,0,0,0,0,0,0,0,0,
  1,1,3,3,4,4,2,2,0,0,0,0,0,0,0,0,0,
  1,1,3,3,4,4,4,2,2,0,0,0,0,0,0,0,0,
  1,1,3,3,4,4,4,4,2,2,0,0,0,0,0,0,0,
  1,1,3,3,4,4,4,5,6,2,2,0,0,0,0,0,0,
  1,1,3,3,4,4,5,6,7,5,2,2,0,0,0,0,0,
  1,1,3,3,4,5,6,7,5,4,5,2,2,0,0,0,0,
  1,1,3,3,5,6,7,5,4,5,6,7,2,2,0,0,0,
  1,1,3,3,6,7,5,4,5,6,7,7,7,2,2,0,0,
  1,1,3,3,7,5,4,5,6,7,7,7,7,7,2,2,0,
  1,1,3,3,5,4,5,6,8,8,8,8,8,8,8,8,2,
  1,1,3,3,4,5,6,3,8,8,8,8,8,8,8,8,8,
  1,1,3,3,5,6,3,3,1,1,1,1,1,1,1,0,0,
  1,1,3,3,6,3,3,1,1,1,1,1,1,1,1,0,0,
  1,1,3,3,3,3,0,0,0,0,0,0,0,0,0,0,0,
  1,1,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,
  1,1,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
);
const cursorPal: array[0..9*4-1] of Byte = (
    0,  0,  0,  0,
    0,  0,  0,163,
   85,255,255,255,
   85, 85,255,255,
  255, 85, 85,255,
  170,  0,170,255,
   85, 85, 85,255,
    0,  0,  0,255,
    0,  0,170,255
);


var
  curtexid: GLuint = 0;

procedure createCursorTexture ();
var
  tex, tpp: PByte;
  c: Integer;
  x, y: Integer;
begin
  if (curtexid <> 0) then exit; //begin glDeleteTextures(1, @curtexid); curtexid := 0; end;

  GetMem(tex, curWidth*curHeight*4);

  tpp := tex;
  for y := 0 to curHeight-1 do
  begin
    for x := 0 to curWidth-1 do
    begin
      c := cursorImg[y*curWidth+x]*4;
      tpp^ := cursorPal[c+0]; Inc(tpp);
      tpp^ := cursorPal[c+1]; Inc(tpp);
      tpp^ := cursorPal[c+2]; Inc(tpp);
      tpp^ := cursorPal[c+3]; Inc(tpp);
    end;
  end;

  glGenTextures(1, @curtexid);
  if (curtexid = 0) then raise Exception.Create('can''t create Holmes texture');

  glBindTexture(GL_TEXTURE_2D, curtexid);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

  //GLfloat[4] bclr = 0.0;
  //glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, bclr.ptr);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, curWidth, curHeight, 0, GL_RGBA{gltt}, GL_UNSIGNED_BYTE, tex);

  //FreeMem(tex);
end;


procedure drawCursor ();
begin
  if (curtexid = 0) then createCursorTexture() else glBindTexture(GL_TEXTURE_2D, curtexid);
  // blend it
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_TEXTURE_2D);
  // color and opacity
  glColor4f(1, 1, 1, 0.9);
  Dec(msX, 2);
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0); glVertex2i(msX, msY); // top-left
    glTexCoord2f(1.0, 0.0); glVertex2i(msX+curWidth, msY); // top-right
    glTexCoord2f(1.0, 1.0); glVertex2i(msX+curWidth, msY+curHeight); // bottom-right
    glTexCoord2f(0.0, 1.0); glVertex2i(msX, msY+curHeight); // bottom-left
  glEnd();
  Inc(msX, 2);
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, 0);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_Holmes_VidModeChanged ();
begin
  e_WriteLog(Format('Inspector: videomode changed: %dx%d', [gScreenWidth, gScreenHeight]), MSG_NOTIFY);
  curtexid := 0; // texture is possibly lost here, idc
  //createCursorTexture();
end;

procedure g_Holmes_WindowFocused ();
begin
  msB := 0;
  kbS := 0;
end;

procedure g_Holmes_WindowBlured ();
begin
end;


// ////////////////////////////////////////////////////////////////////////// //
var
  vpSet: Boolean = false;
  vpx, vpy: Integer;
  vpw, vph: Integer;
  laserSet: Boolean = false;
  laserX0, laserY0, laserX1, laserY1: Integer;
  monMarkedUID: Integer = -1;

procedure g_Holmes_plrView (viewPortX, viewPortY, viewPortW, viewPortH: Integer);
begin
  vpSet := true;
  vpx := viewPortX;
  vpy := viewPortY;
  vpw := viewPortW;
  vph := viewPortH;
end;

procedure g_Holmes_plrLaser (ax0, ay0, ax1, ay1: Integer);
begin
  laserSet := true;
  laserX0 := ax0;
  laserY0 := ay0;
  laserX1 := ax1;
  laserY1 := ay1;
end;


function pmsCurMapX (): Integer; inline; begin result := msX+vpx; end;
function pmsCurMapY (): Integer; inline; begin result := msY+vpy; end;


procedure plrDebugMouse (var ev: THMouseEvent);

  function wallToggle (pan: TPanel; tag: Integer): Boolean;
  begin
    result := false; // don't stop
    if pan.Enabled then g_Map_DisableWall(pan.arrIdx) else g_Map_EnableWall(pan.arrIdx);
  end;

  function monsAtDump (mon: TMonster; tag: Integer): Boolean;
  begin
    result := false; // don't stop
    e_WriteLog(Format('monster #%d; UID=%d', [mon.arrIdx, mon.UID]), MSG_NOTIFY);
    monMarkedUID := mon.UID;
    //if pan.Enabled then g_Map_DisableWall(pan.arrIdx) else g_Map_EnableWall(pan.arrIdx);
  end;

begin
  //e_WriteLog(Format('mouse: x=%d; y=%d; but=%d; bstate=%d', [msx, msy, but, bstate]), MSG_NOTIFY);
  if (gPlayer1 = nil) then exit;
  if (ev.kind <> THMouseEvent.Press) then exit;

  if (ev.but = THMouseEvent.Left) then
  begin
    mapGrid.forEachAtPoint(pmsCurMapX, pmsCurMapY, wallToggle, (GridTagWall or GridTagDoor));
    exit;
  end;

  if (ev.but = THMouseEvent.Right) then
  begin
    monMarkedUID := -1;
    e_WriteLog('===========================', MSG_NOTIFY);
    monsGrid.forEachAtPoint(pmsCurMapX, pmsCurMapY, monsAtDump);
    e_WriteLog('---------------------------', MSG_NOTIFY);
    exit;
  end;
end;


procedure plrDebugDraw ();

  function monsCollector (mon: TMonster; tag: Integer): Boolean;
  var
    ex, ey: Integer;
    mx, my, mw, mh: Integer;
  begin
    result := false;
    mon.getMapBox(mx, my, mw, mh);
    e_DrawQuad(mx, my, mx+mw-1, my+mh-1, 255, 255, 0, 96);
    if lineAABBIntersects(laserX0, laserY0, laserX1, laserY1, mx, my, mw, mh, ex, ey) then
    begin
      e_DrawPoint(8, ex, ey, 0, 255, 0);
    end;
  end;

var
  mon: TMonster;
  mx, my, mw, mh: Integer;
begin
  //e_DrawPoint(4, plrMouseX, plrMouseY, 255, 0, 255);
  if (gPlayer1 = nil) then exit;

  //e_WriteLog(Format('(%d,%d)-(%d,%d)', [laserX0, laserY0, laserX1, laserY1]), MSG_NOTIFY);

  glPushMatrix();
  glTranslatef(-vpx, -vpy, 0);

  g_Mons_AlongLine(laserX0, laserY0, laserX1, laserY1, monsCollector, true);

  if (monMarkedUID <> -1) then
  begin
    mon := g_Monsters_ByUID(monMarkedUID);
    if (mon <> nil) then
    begin
      mon.getMapBox(mx, my, mw, mh);
      e_DrawQuad(mx, my, mx+mw-1, my+mh-1, 255, 0, 0, 30);
    end;
  end;

  //e_DrawPoint(16, laserX0, laserY0, 255, 255, 255);

  glPopMatrix();
end;


{
    procedure drawTileGrid ();
    var
      x, y: Integer;
    begin
      y := mapGrid.gridY0;
      while (y < mapGrid.gridY0+mapGrid.gridHeight) do
      begin
        x := mapGrid.gridX0;
        while (x < mapGrid.gridX0+mapGrid.gridWidth) do
        begin
          if (x+mapGrid.tileSize > vpx) and (y+mapGrid.tileSize > vpy) and
             (x < vpx+vpw) and (y < vpy+vph) then
          begin
            e_DrawQuad(x, y, x+mapGrid.tileSize-1, y+mapGrid.tileSize-1, 96, 96, 96, 96);
          end;
          Inc(x, mapGrid.tileSize);
        end;
        Inc(y, mapGrid.tileSize);
      end;
    end;
}


// ////////////////////////////////////////////////////////////////////////// //
function g_Holmes_mouseEvent (var ev: THMouseEvent): Boolean;
begin
  result := true;
  msX := ev.x;
  msY := ev.y;
  msB := ev.bstate;
  kbS := ev.kstate;
  plrDebugMouse(ev);
end;


function g_Holmes_KeyEvent (var ev: THKeyEvent): Boolean;
begin
  result := false;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_Holmes_Draw ();
begin
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE); // modify color buffer
  glDisable(GL_STENCIL_TEST);
  glDisable(GL_BLEND);
  glDisable(GL_SCISSOR_TEST);
  glDisable(GL_TEXTURE_2D);

  plrDebugDraw();

  drawCursor();
end;


end.
