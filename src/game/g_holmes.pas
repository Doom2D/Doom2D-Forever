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
      Release = 0;
      Press = 1;
      Motion = 2;

  public
    kind: Byte; // motion, press, release
    x, y: Integer;
    dx, dy: Integer; // for wheel this is wheel motion, otherwise this is relative mouse motion
    but: Word; // current pressed/released button, or 0 for motion
    bstate: Word; // button state
    kstate: Word; // keyboard state (see THKeyEvent);
  end;

  THKeyEvent = record
  public
    const
      // modifiers
      ModCtrl = $0001;
      ModAlt = $0002;
      ModShift = $0004;

      // event types
      Release = 0;
      Press = 1;

  public
    kind: Byte;
    scan: Word; // SDL_SCANCODE_XXX
    sym: Word; // SDLK_XXX
    bstate: Word; // button state
    kstate: Word; // keyboard state
  end;


procedure g_Holmes_VidModeChanged ();
procedure g_Holmes_WindowFocused ();
procedure g_Holmes_WindowBlured ();

procedure g_Holmes_Draw ();
procedure g_Holmes_DrawUI ();

function g_Holmes_MouseEvent (var ev: THMouseEvent): Boolean; // returns `true` if event was eaten
function g_Holmes_KeyEvent (var ev: THKeyEvent): Boolean; // returns `true` if event was eaten

// hooks for player
procedure g_Holmes_plrView (viewPortX, viewPortY, viewPortW, viewPortH: Integer);
procedure g_Holmes_plrLaser (ax0, ay0, ax1, ay1: Integer);


var
  g_holmes_enabled: Boolean = {$IF DEFINED(D2F_DEBUG)}true{$ELSE}false{$ENDIF};


implementation

uses
  SysUtils, GL, SDL2,
  MAPDEF, g_options;


var
  //globalInited: Boolean = false;
  msX: Integer = -666;
  msY: Integer = -666;
  msB: Word = 0; // button state
  kbS: Word = 0; // keyboard modifiers state
  showGrid: Boolean = true;
  showMonsInfo: Boolean = false;
  showMonsLOS2Plr: Boolean = false;
  showAllMonsCells: Boolean = false;
  showMapCurPos: Boolean = false;
  showLayersWindow: Boolean = false;
  showOutlineWindow: Boolean = false;
  oldShowLayersWindow: Boolean = false;
  oldShowOutlineWindow: Boolean = false;

// ////////////////////////////////////////////////////////////////////////// //
{$INCLUDE g_holmes.inc}
{$INCLUDE g_holmes_ui.inc}


// ////////////////////////////////////////////////////////////////////////// //
var
  g_ol_nice: Boolean = false;
  g_ol_fill_walls: Boolean = false;
  g_ol_rlayer_back: Boolean = false;
  g_ol_rlayer_step: Boolean = false;
  g_ol_rlayer_wall: Boolean = false;
  g_ol_rlayer_door: Boolean = false;
  g_ol_rlayer_acid1: Boolean = false;
  g_ol_rlayer_acid2: Boolean = false;
  g_ol_rlayer_water: Boolean = false;
  g_ol_rlayer_fore: Boolean = false;


// ////////////////////////////////////////////////////////////////////////// //
var
  winOptions: THTopWindow = nil;
  winLayers: THTopWindow = nil;
  winOutlines: THTopWindow = nil;


procedure createOptionsWindow ();
var
  llb: THCtlCBListBox;
begin
  llb := THCtlCBListBox.Create(0, 0);
  llb.appendItem('map grid', @showGrid);
  llb.appendItem('cursor position on map', @showMapCurPos);
  llb.appendItem('monster info', @showMonsInfo);
  llb.appendItem('monster LOS to player', @showMonsLOS2Plr);
  llb.appendItem('monster cells (SLOW!)', @showAllMonsCells);
  llb.appendItem('', nil);
  llb.appendItem('layers window', @showLayersWindow);
  llb.appendItem('outline window', @showOutlineWindow);
  winOptions := THTopWindow.Create('Holmes Options', 100, 100);
  winOptions.escClose := true;
  winOptions.appendChild(llb);
end;


procedure createLayersWindow ();
var
  llb: THCtlCBListBox;
begin
  llb := THCtlCBListBox.Create(0, 0);
  llb.appendItem('background', @g_rlayer_back);
  llb.appendItem('steps', @g_rlayer_step);
  llb.appendItem('walls', @g_rlayer_wall);
  llb.appendItem('doors', @g_rlayer_door);
  llb.appendItem('acid1', @g_rlayer_acid1);
  llb.appendItem('acid2', @g_rlayer_acid2);
  llb.appendItem('water', @g_rlayer_water);
  llb.appendItem('foreground', @g_rlayer_fore);
  winLayers := THTopWindow.Create('visible', 10, 10);
  winLayers.escClose := true;
  winLayers.appendChild(llb);
end;


procedure createOutlinesWindow ();
var
  llb: THCtlCBListBox;
begin
  llb := THCtlCBListBox.Create(0, 0);
  llb.appendItem('background', @g_ol_rlayer_back);
  llb.appendItem('steps', @g_ol_rlayer_step);
  llb.appendItem('walls', @g_ol_rlayer_wall);
  llb.appendItem('doors', @g_ol_rlayer_door);
  llb.appendItem('acid1', @g_ol_rlayer_acid1);
  llb.appendItem('acid2', @g_ol_rlayer_acid2);
  llb.appendItem('water', @g_ol_rlayer_water);
  llb.appendItem('foreground', @g_ol_rlayer_fore);
  llb.appendItem('', nil);
  llb.appendItem('fill walls', @g_ol_fill_walls);
  llb.appendItem('', nil);
  llb.appendItem('slow''n''nice', @g_ol_nice);
  winOutlines := THTopWindow.Create('outlines', 100, 10);
  winOutlines.escClose := true;
  winOutlines.appendChild(llb);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_Holmes_VidModeChanged ();
begin
  e_WriteLog(Format('Holmes: videomode changed: %dx%d', [gScreenWidth, gScreenHeight]), MSG_NOTIFY);
  // texture space is possibly lost here, idc
  curtexid := 0;
  font6texid := 0;
  font8texid := 0;
  prfont6texid := 0;
  prfont8texid := 0;
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
  laserSet := laserSet; // shut up, fpc!
end;


function pmsCurMapX (): Integer; inline; begin result := msX+vpx; end;
function pmsCurMapY (): Integer; inline; begin result := msY+vpy; end;


procedure plrDebugMouse (var ev: THMouseEvent);

  function wallToggle (pan: TPanel; tag: Integer): Boolean;
  begin
    result := false; // don't stop
    e_WriteLog(Format('wall #%d(%d); enabled=%d (%d); (%d,%d)-(%d,%d)', [pan.arrIdx, pan.proxyId, Integer(pan.Enabled), Integer(mapGrid.proxyEnabled[pan.proxyId]), pan.X, pan.Y, pan.Width, pan.Height]), MSG_NOTIFY);
    if ((kbS and THKeyEvent.ModAlt) <> 0) then
    begin
      if pan.Enabled then g_Map_DisableWall(pan.arrIdx) else g_Map_EnableWall(pan.arrIdx);
    end;
  end;

  function monsAtDump (mon: TMonster; tag: Integer): Boolean;
  begin
    result := false; // don't stop
    e_WriteLog(Format('monster #%d; UID=%d', [mon.arrIdx, mon.UID]), MSG_NOTIFY);
    monMarkedUID := mon.UID;
    //if pan.Enabled then g_Map_DisableWall(pan.arrIdx) else g_Map_EnableWall(pan.arrIdx);
  end;

  function monsInCell (mon: TMonster; tag: Integer): Boolean;
  begin
    result := false; // don't stop
    e_WriteLog(Format('monster #%d (UID:%u) (proxyid:%d)', [mon.arrIdx, mon.UID, mon.proxyId]), MSG_NOTIFY);
  end;

begin
  //e_WriteLog(Format('mouse: x=%d; y=%d; but=%d; bstate=%d', [msx, msy, but, bstate]), MSG_NOTIFY);
  if (gPlayer1 = nil) or not vpSet then exit;
  if (ev.kind <> THMouseEvent.Press) then exit;

  e_WriteLog(Format('mev: %d', [Integer(ev.kind)]), MSG_NOTIFY);

  if (ev.but = THMouseEvent.Left) then
  begin
    if ((kbS and THKeyEvent.ModShift) <> 0) then
    begin
      // dump monsters in cell
      e_WriteLog('===========================', MSG_NOTIFY);
      monsGrid.forEachInCell(pmsCurMapX, pmsCurMapY, monsInCell);
      e_WriteLog('---------------------------', MSG_NOTIFY);
    end
    else
    begin
      // toggle wall
      e_WriteLog('=== TOGGLE WALL ===', MSG_NOTIFY);
      mapGrid.forEachAtPoint(pmsCurMapX, pmsCurMapY, wallToggle, (GridTagWall or GridTagDoor));
      e_WriteLog('--- toggle wall ---', MSG_NOTIFY);
    end;
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


var
  edgeBmp: array of Byte = nil;


procedure drawOutlines ();
var
  r, g, b: Integer;

  procedure clearEdgeBmp ();
  begin
    SetLength(edgeBmp, (gWinSizeX+4)*(gWinSizeY+4));
    FillChar(edgeBmp[0], Length(edgeBmp)*sizeof(edgeBmp[0]), 0);
  end;

  procedure drawPanel (pan: TPanel);
  var
    sx, len, y0, y1: Integer;
  begin
    if (pan = nil) or (pan.Width < 1) or (pan.Height < 1) then exit;
    if (pan.X+pan.Width <= vpx-1) or (pan.Y+pan.Height <= vpy-1) then exit;
    if (pan.X >= vpx+vpw+1) or (pan.Y >= vpy+vph+1) then exit;
    if g_ol_nice or g_ol_fill_walls then
    begin
      sx := pan.X-(vpx-1);
      len := pan.Width;
      if (len > gWinSizeX+4) then len := gWinSizeX+4;
      if (sx < 0) then begin len += sx; sx := 0; end;
      if (sx+len > gWinSizeX+4) then len := gWinSizeX+4-sx;
      if (len < 1) then exit;
      assert(sx >= 0);
      assert(sx+len <= gWinSizeX+4);
      y0 := pan.Y-(vpy-1);
      y1 := y0+pan.Height;
      if (y0 < 0) then y0 := 0;
      if (y1 > gWinSizeY+4) then y1 := gWinSizeY+4;
      while (y0 < y1) do
      begin
        FillChar(edgeBmp[y0*(gWinSizeX+4)+sx], len*sizeof(edgeBmp[0]), 1);
        Inc(y0);
      end;
    end
    else
    begin
      drawRect(pan.X, pan.Y, pan.Width, pan.Height, r, g, b);
    end;
  end;

  procedure drawEdges ();
  var
    x, y: Integer;
    a: PByte;
  begin
    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
    glLineWidth(1);
    glPointSize(1);
    glDisable(GL_LINE_SMOOTH);
    glDisable(GL_POLYGON_SMOOTH);
    glColor4f(r/255.0, g/255.0, b/255.0, 1.0);
    glBegin(GL_POINTS);
    for y := 1 to vph do
    begin
      a := @edgeBmp[y*(gWinSizeX+4)+1];
      for x := 1 to vpw do
      begin
        if (a[0] <> 0) then
        begin
          if (a[-1] = 0) or (a[1] = 0) or (a[-(gWinSizeX+4)] = 0) or (a[gWinSizeX+4] = 0) or
             (a[-(gWinSizeX+4)-1] = 0) or (a[-(gWinSizeX+4)+1] = 0) or
             (a[gWinSizeX+4-1] = 0) or (a[gWinSizeX+4+1] = 0) then
          begin
            glVertex2f(x-1+vpx+0.37, y-1+vpy+0.37);
          end;
        end;
        Inc(a);
      end;
    end;
    glEnd();
  end;

  procedure drawFilledWalls ();
  var
    x, y, sx, ex: Integer;
    a: PByte;
    procedure drawLine ();
    begin
      if (sx >= 0) then
      begin
        glVertex2f(sx-1+vpx+0.37, y-1+vpy+0.37);
        glVertex2f(ex+vpx+0.37, y-1+vpy+0.37);
      end;
      sx := -1;
      ex := -1;
    end;
  begin
    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
    glLineWidth(1);
    glPointSize(1);
    glDisable(GL_LINE_SMOOTH);
    glDisable(GL_POLYGON_SMOOTH);
    glColor4f(r/255.0, g/255.0, b/255.0, 1.0);
    glBegin(GL_LINES);
    for y := 1 to vph do
    begin
      a := @edgeBmp[y*(gWinSizeX+4)+1];
      sx := -1;
      ex := -1;
      for x := 1 to vpw do
      begin
        if (a[0] <> 0) then
        begin
          if (ex+1 <> x) then drawLine();
          if (sx < 0) then
          begin
            sx := x;
            ex := x;
          end
          else
          begin
            ex := x;
          end;
        end;
        Inc(a);
      end;
      drawLine();
    end;
    glEnd();
  end;

  procedure doWallsOld (parr: array of TPanel; ptype: Word; ar, ag, ab: Integer);
  var
    f: Integer;
    pan: TPanel;
  begin
    r := ar;
    g := ag;
    b := ab;
    if g_ol_nice or g_ol_fill_walls then clearEdgeBmp();
    for f := 0 to High(parr) do
    begin
      pan := parr[f];
      if (pan = nil) or not pan.Enabled or (pan.Width < 1) or (pan.Height < 1) then continue;
      if ((pan.PanelType and ptype) = 0) then continue;
      drawPanel(pan);
    end;
    if g_ol_nice then drawEdges();
    if g_ol_fill_walls then drawFilledWalls();
  end;

var
  xptag: Word;

  function doWallCB (pan: TPanel; tag: Integer): Boolean;
  begin
    result := false; // don't stop
    //if (pan = nil) or not pan.Enabled or (pan.Width < 1) or (pan.Height < 1) then exit;
    if ((pan.PanelType and xptag) = 0) then exit;
    drawPanel(pan);
  end;

  procedure doWalls (parr: array of TPanel; ptype: Word; ar, ag, ab: Integer);
  begin
    r := ar;
    g := ag;
    b := ab;
    xptag := ptype;
    if ((ptype and (PANEL_WALL or PANEL_OPENDOOR or PANEL_CLOSEDOOR)) <> 0) then ptype := GridTagWall or GridTagDoor
    else panelTypeToTag(ptype);
    if g_ol_nice or g_ol_fill_walls then clearEdgeBmp();
    mapGrid.forEachInAABB(vpx-1, vpy-1, vpw+2, vph+2, doWallCB, ptype);
    if g_ol_nice then drawEdges();
    if g_ol_fill_walls then drawFilledWalls();
  end;

begin
  if g_ol_rlayer_back then doWallsOld(gRenderBackgrounds, PANEL_BACK, 255, 127, 0);
  if g_ol_rlayer_step then doWallsOld(gSteps, PANEL_STEP, 192, 192, 192);
  if g_ol_rlayer_wall then doWallsOld(gWalls, PANEL_WALL, 255, 255, 255);
  if g_ol_rlayer_door then doWallsOld(gWalls, PANEL_OPENDOOR or PANEL_CLOSEDOOR, 0, 255, 0);
  if g_ol_rlayer_acid1 then doWallsOld(gAcid1, PANEL_ACID1, 255, 0, 0);
  if g_ol_rlayer_acid2 then doWallsOld(gAcid2, PANEL_ACID2, 198, 198, 0);
  if g_ol_rlayer_water then doWallsOld(gWater, PANEL_WATER, 0, 255, 255);
  if g_ol_rlayer_fore then doWallsOld(gRenderForegrounds, PANEL_FORE, 210, 210, 210);
end;


procedure plrDebugDraw ();

  procedure drawTileGrid ();
  var
    x, y: Integer;
  begin
    for y := 0 to (mapGrid.gridHeight div mapGrid.tileSize) do
    begin
      drawLine(mapGrid.gridX0, mapGrid.gridY0+y*mapGrid.tileSize, mapGrid.gridX0+mapGrid.gridWidth, mapGrid.gridY0+y*mapGrid.tileSize, 96, 96, 96, 255);
    end;

    for x := 0 to (mapGrid.gridWidth div mapGrid.tileSize) do
    begin
      drawLine(mapGrid.gridX0+x*mapGrid.tileSize, mapGrid.gridY0, mapGrid.gridX0+x*mapGrid.tileSize, mapGrid.gridY0+y*mapGrid.gridHeight, 96, 96, 96, 255);
    end;
  end;

  procedure hilightCell (cx, cy: Integer);
  begin
    fillRect(cx, cy, monsGrid.tileSize, monsGrid.tileSize, 0, 128, 0, 64);
  end;

  procedure hilightCell1 (cx, cy: Integer);
  begin
    //e_WriteLog(Format('h1: (%d,%d)', [cx, cy]), MSG_NOTIFY);
    fillRect(cx, cy, monsGrid.tileSize, monsGrid.tileSize, 255, 255, 0, 92);
  end;

  function hilightWallTrc (pan: TPanel; tag: Integer; x, y, prevx, prevy: Integer): Boolean;
  begin
    result := false; // don't stop
    if (pan = nil) then exit; // cell completion, ignore
    //e_WriteLog(Format('h1: (%d,%d)', [cx, cy]), MSG_NOTIFY);
    fillRect(pan.X, pan.Y, pan.Width, pan.Height, 0, 128, 128, 64);
  end;

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

  procedure drawMonsterInfo (mon: TMonster);
  var
    mx, my, mw, mh: Integer;

    procedure drawMonsterTargetLine ();
    var
      emx, emy, emw, emh: Integer;
      enemy: TMonster;
      eplr: TPlayer;
      ex, ey: Integer;
    begin
      if (g_GetUIDType(mon.MonsterTargetUID) = UID_PLAYER) then
      begin
        eplr := g_Player_Get(mon.MonsterTargetUID);
        if (eplr <> nil) then eplr.getMapBox(emx, emy, emw, emh) else exit;
      end
      else if (g_GetUIDType(mon.MonsterTargetUID) = UID_MONSTER) then
      begin
        enemy := g_Monsters_ByUID(mon.MonsterTargetUID);
        if (enemy <> nil) then enemy.getMapBox(emx, emy, emw, emh) else exit;
      end
      else
      begin
        exit;
      end;
      mon.getMapBox(mx, my, mw, mh);
      drawLine(mx+mw div 2, my+mh div 2, emx+emw div 2, emy+emh div 2, 255, 0, 0, 255);
      if (g_Map_traceToNearestWall(mx+mw div 2, my+mh div 2, emx+emw div 2, emy+emh div 2, @ex, @ey) <> nil) then
      begin
        drawLine(mx+mw div 2, my+mh div 2, ex, ey, 0, 255, 0, 255);
      end;
    end;

    procedure drawLOS2Plr ();
    var
      emx, emy, emw, emh: Integer;
      eplr: TPlayer;
      ex, ey: Integer;
    begin
      eplr := gPlayers[0];
      if (eplr = nil) then exit;
      eplr.getMapBox(emx, emy, emw, emh);
      mon.getMapBox(mx, my, mw, mh);
      drawLine(mx+mw div 2, my+mh div 2, emx+emw div 2, emy+emh div 2, 255, 0, 0, 255);
      {$IF DEFINED(D2F_DEBUG)}
      //mapGrid.dbgRayTraceTileHitCB := hilightCell1;
      {$ENDIF}
      if (g_Map_traceToNearestWall(mx+mw div 2, my+mh div 2, emx+emw div 2, emy+emh div 2, @ex, @ey) <> nil) then
      //if (mapGrid.traceRay(ex, ey, mx+mw div 2, my+mh div 2, emx+emw div 2, emy+emh div 2, hilightWallTrc, (GridTagWall or GridTagDoor)) <> nil) then
      begin
        drawLine(mx+mw div 2, my+mh div 2, ex, ey, 0, 255, 0, 255);
      end;
      {$IF DEFINED(D2F_DEBUG)}
      //mapGrid.dbgRayTraceTileHitCB := nil;
      {$ENDIF}
    end;

  begin
    if (mon = nil) then exit;
    mon.getMapBox(mx, my, mw, mh);
    //mx += mw div 2;

    monsGrid.forEachBodyCell(mon.proxyId, hilightCell);

    if showMonsInfo then
    begin
      //fillRect(mx-4, my-7*8-6, 110, 7*8+6, 0, 0, 94, 250);
      darkenRect(mx-4, my-7*8-6, 110, 7*8+6, 128);
      my -= 8;
      my -= 2;

      // type
      drawText6(mx, my, Format('%s(U:%u)', [monsTypeToString(mon.MonsterType), mon.UID]), 255, 127, 0); my -= 8;
      // beh
      drawText6(mx, my, Format('Beh: %s', [monsBehToString(mon.MonsterBehaviour)]), 255, 127, 0); my -= 8;
      // state
      drawText6(mx, my, Format('State:%s (%d)', [monsStateToString(mon.MonsterState), mon.MonsterSleep]), 255, 127, 0); my -= 8;
      // health
      drawText6(mx, my, Format('Health:%d', [mon.MonsterHealth]), 255, 127, 0); my -= 8;
      // ammo
      drawText6(mx, my, Format('Ammo:%d', [mon.MonsterAmmo]), 255, 127, 0); my -= 8;
      // target
      drawText6(mx, my, Format('TgtUID:%u', [mon.MonsterTargetUID]), 255, 127, 0); my -= 8;
      drawText6(mx, my, Format('TgtTime:%d', [mon.MonsterTargetTime]), 255, 127, 0); my -= 8;
    end;

    drawMonsterTargetLine();
    if showMonsLOS2Plr then drawLOS2Plr();
    {
    property MonsterRemoved: Boolean read FRemoved write FRemoved;
    property MonsterPain: Integer read FPain write FPain;
    property MonsterAnim: Byte read FCurAnim write FCurAnim;
    }
  end;

  function highlightAllMonsterCells (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    monsGrid.forEachBodyCell(mon.proxyId, hilightCell);
  end;

var
  mon: TMonster;
  mx, my, mw, mh: Integer;
begin
  if (gPlayer1 = nil) then exit;

  glEnable(GL_SCISSOR_TEST);
  glScissor(0, gWinSizeY-gPlayerScreenSize.Y-1, vpw, vph);

  glPushMatrix();
  glTranslatef(-vpx, -vpy, 0);

  if (showGrid) then drawTileGrid();
  drawOutlines();

  if (laserSet) then g_Mons_AlongLine(laserX0, laserY0, laserX1, laserY1, monsCollector, true);

  if (monMarkedUID <> -1) then
  begin
    mon := g_Monsters_ByUID(monMarkedUID);
    if (mon <> nil) then
    begin
      mon.getMapBox(mx, my, mw, mh);
      e_DrawQuad(mx, my, mx+mw-1, my+mh-1, 255, 0, 0, 30);
      drawMonsterInfo(mon);
    end;
  end;

  if showAllMonsCells then g_Mons_ForEach(highlightAllMonsterCells);

  glPopMatrix();

  glDisable(GL_SCISSOR_TEST);

  if showMapCurPos then drawText8(4, gWinSizeY-10, Format('mappos:(%d,%d)', [pmsCurMapX, pmsCurMapY]), 255, 255, 0);
end;


// ////////////////////////////////////////////////////////////////////////// //
function g_Holmes_MouseEvent (var ev: THMouseEvent): Boolean;
begin
  result := true;
  msX := ev.x;
  msY := ev.y;
  msB := ev.bstate;
  kbS := ev.kstate;
  msB := msB;
  if not uiMouseEvent(ev) then plrDebugMouse(ev);
end;


function g_Holmes_KeyEvent (var ev: THKeyEvent): Boolean;
var
  mon: TMonster;
  pan: TPanel;
  x, y, w, h: Integer;
  ex, ey: Integer;
  dx, dy: Integer;

  procedure dummyWallTrc (cx, cy: Integer);
  begin
  end;

begin
  result := false;
  msB := ev.bstate;
  kbS := ev.kstate;
  case ev.scan of
    SDL_SCANCODE_LCTRL, SDL_SCANCODE_RCTRL,
    SDL_SCANCODE_LALT, SDL_SCANCODE_RALT,
    SDL_SCANCODE_LSHIFT, SDL_SCANCODE_RSHIFT:
      result := true;
  end;
  if uiKeyEvent(ev) then begin result := true; exit; end;
  // press
  if (ev.kind = THKeyEvent.Press) then
  begin
    // M-M: one monster think step
    if (ev.scan = SDL_SCANCODE_M) and ((ev.kstate and THKeyEvent.ModAlt) <> 0) then
    begin
      result := true;
      gmon_debug_think := false;
      gmon_debug_one_think_step := true; // do one step
      exit;
    end;
    // M-I: toggle monster info
    if (ev.scan = SDL_SCANCODE_I) and ((ev.kstate and THKeyEvent.ModAlt) <> 0) then
    begin
      result := true;
      showMonsInfo := not showMonsInfo;
      exit;
    end;
    // M-L: toggle monster LOS to player
    if (ev.scan = SDL_SCANCODE_L) and ((ev.kstate and THKeyEvent.ModAlt) <> 0) then
    begin
      result := true;
      showMonsLOS2Plr := not showMonsLOS2Plr;
      exit;
    end;
    // M-G: toggle "show all cells occupied by monsters"
    if (ev.scan = SDL_SCANCODE_G) and ((ev.kstate and THKeyEvent.ModAlt) <> 0) then
    begin
      result := true;
      showAllMonsCells := not showAllMonsCells;
      exit;
    end;
    // M-A: wake up monster
    if (ev.scan = SDL_SCANCODE_A) and ((ev.kstate and THKeyEvent.ModAlt) <> 0) then
    begin
      result := true;
      if (monMarkedUID <> -1) then
      begin
        mon := g_Monsters_ByUID(monMarkedUID);
        if (mon <> nil) then mon.WakeUp();
      end;
      exit;
    end;
    // C-T: teleport player
    if (ev.scan = SDL_SCANCODE_T) and ((ev.kstate and THKeyEvent.ModCtrl) <> 0) then
    begin
      result := true;
      //e_WriteLog(Format('TELEPORT: (%d,%d)', [pmsCurMapX, pmsCurMapY]), MSG_NOTIFY);
      if (gPlayers[0] <> nil) then
      begin
        gPlayers[0].getMapBox(x, y, w, h);
        gPlayers[0].TeleportTo(pmsCurMapX-w div 2, pmsCurMapY-h div 2, true, 69); // 69: don't change dir
      end;
      exit;
    end;
    // C-P: show cursor position on the map
    if (ev.scan = SDL_SCANCODE_P) and ((ev.kstate and THKeyEvent.ModCtrl) <> 0) then
    begin
      result := true;
      showMapCurPos := not showMapCurPos;
      exit;
    end;
    // C-G: toggle grid
    if (ev.scan = SDL_SCANCODE_G) and ((ev.kstate and THKeyEvent.ModCtrl) <> 0) then
    begin
      result := true;
      showGrid := not showGrid;
      exit;
    end;
    // C-L: toggle layers window
    if (ev.scan = SDL_SCANCODE_L) and ((ev.kstate and THKeyEvent.ModCtrl) <> 0) then
    begin
      result := true;
      showLayersWindow := not showLayersWindow;
      exit;
    end;
    // C-O: toggle outlines window
    if (ev.scan = SDL_SCANCODE_O) and ((ev.kstate and THKeyEvent.ModCtrl) <> 0) then
    begin
      result := true;
      showOutlineWindow := not showOutlineWindow;
      exit;
    end;
    // F1: toggle options window
    if (ev.scan = SDL_SCANCODE_F1) and (ev.kstate = 0) then
    begin
      result := true;
      if (winOptions = nil) then createOptionsWindow();
      if not uiVisibleWindow(winOptions) then uiAddWindow(winOptions) else uiRemoveWindow(winOptions);
      exit;
    end;
    // C-UP, C-DOWN, C-LEFT, C-RIGHT: trace 10 pixels from cursor in the respective direction
    if ((ev.scan = SDL_SCANCODE_UP) or (ev.scan = SDL_SCANCODE_DOWN) or (ev.scan = SDL_SCANCODE_LEFT) or (ev.scan = SDL_SCANCODE_RIGHT)) and
       ((ev.kstate and THKeyEvent.ModCtrl) <> 0) then
    begin
      result := true;
      dx := pmsCurMapX;
      dy := pmsCurMapY;
      case ev.scan of
        SDL_SCANCODE_UP: dy -= 120;
        SDL_SCANCODE_DOWN: dy += 120;
        SDL_SCANCODE_LEFT: dx -= 120;
        SDL_SCANCODE_RIGHT: dx += 120;
      end;
      {$IF DEFINED(D2F_DEBUG)}
      //mapGrid.dbgRayTraceTileHitCB := dummyWallTrc;
      mapGrid.dbgShowTraceLog := true;
      {$ENDIF}
      pan := g_Map_traceToNearest(pmsCurMapX, pmsCurMapY, dx, dy, (GridTagWall or GridTagDoor or GridTagStep or GridTagAcid1 or GridTagAcid2 or GridTagWater), @ex, @ey);
      {$IF DEFINED(D2F_DEBUG)}
      //mapGrid.dbgRayTraceTileHitCB := nil;
      mapGrid.dbgShowTraceLog := false;
      {$ENDIF}
      e_LogWritefln('v-trace: (%d,%d)-(%d,%d); end=(%d,%d); hit=%d', [pmsCurMapX, pmsCurMapY, dx, dy, ex, ey, (pan <> nil)]);
      exit;
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_Holmes_Draw ();
begin
  if (oldShowLayersWindow <> showLayersWindow) then
  begin
    oldShowLayersWindow := showLayersWindow;
    if showLayersWindow then
    begin
      if (winLayers = nil) then createLayersWindow();
      uiAddWindow(winLayers);
    end
    else
    begin
      uiRemoveWindow(winLayers);
    end;
  end
  else
  begin
    showLayersWindow := uiVisibleWindow(winLayers);
    oldShowLayersWindow := showLayersWindow;
  end;

  if (oldShowOutlineWindow <> showOutlineWindow) then
  begin
    oldShowOutlineWindow := showOutlineWindow;
    if showOutlineWindow then
    begin
      if (winOutlines = nil) then createOutlinesWindow();
      uiAddWindow(winOutlines);
    end
    else
    begin
      uiRemoveWindow(winOutlines);
    end;
  end
  else
  begin
    showOutlineWindow := uiVisibleWindow(winOutlines);
    oldShowOutlineWindow := showOutlineWindow;
  end;

  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE); // modify color buffer
  glDisable(GL_STENCIL_TEST);
  glDisable(GL_BLEND);
  glDisable(GL_SCISSOR_TEST);
  glDisable(GL_TEXTURE_2D);

  if gGameOn then
  begin
    plrDebugDraw();
  end;

  laserSet := false;
end;


procedure g_Holmes_DrawUI ();
begin
  uiDraw();
  drawCursor();
end;


end.
