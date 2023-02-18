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
{$INCLUDE ../../../shared/a_modes.inc}
unit r_holmes;

  {$IFDEF USE_GLES1}
    {$FATAL Desktop OpenGL required for current Holmes implementation}
  {$ENDIF}

interface

procedure r_Holmes_Draw ();
procedure r_Holmes_DrawUI ();

// hooks for player
procedure r_Holmes_plrViewPos (viewPortX, viewPortY: Integer);
procedure r_Holmes_plrViewSize (viewPortW, viewPortH: Integer);
procedure r_Holmes_plrLaser (ax0, ay0, ax1, ay1: Integer);

function pmsCurMapX (): Integer;
function pmsCurMapY (): Integer;

var
  vpSet: Boolean = false;

implementation

uses
  {$INCLUDE ../nogl/noGLuses.inc}
  geom,
  e_log,
  g_basic, g_grid, g_player, g_monsters,
  g_map, g_triggers, g_items, g_game, g_panel, g_console,
  xprofiler,
  sdlcarcass,
  fui_common, fui_ctls,
  fui_gfx,
  r_fui_gfx_gl,
  {$IFDEF ENABLE_GFX}
    g_gfx,
  {$ENDIF}
  {$IFDEF ENABLE_GIBS}
    g_gibs,
  {$ENDIF}
  g_holmes,
  typinfo, SysUtils, Classes,
  MAPDEF, g_options;

var
  hlmContext: r_fui_gfx_gl.TGxContext = nil;
  vpx, vpy: Integer;
  vpw, vph: Integer;
  laserSet: Boolean = false;
  laserX0, laserY0, laserX1, laserY1: Integer;

// ////////////////////////////////////////////////////////////////////////// //

{$INCLUDE r_holmes_ol.inc}

// ////////////////////////////////////////////////////////////////////////// //

procedure r_Holmes_plrViewPos (viewPortX, viewPortY: Integer);
begin
  vpSet := true;
  vpx := viewPortX;
  vpy := viewPortY;
end;

procedure r_Holmes_plrViewSize (viewPortW, viewPortH: Integer);
begin
  vpSet := true;
  vpw := viewPortW;
  vph := viewPortH;
end;

procedure r_Holmes_plrLaser (ax0, ay0, ax1, ay1: Integer);
begin
  laserSet := true;
  laserX0 := ax0;
  laserY0 := ay0;
  laserX1 := ax1;
  laserY1 := ay1;
  laserSet := laserSet; // shut up, fpc!
end;

function pmsCurMapX (): Integer; inline; begin result := round(msX/g_dbg_scale)+vpx; end;
function pmsCurMapY (): Integer; inline; begin result := round(msY/g_dbg_scale)+vpy; end;

{$IFDEF HOLMES_OLD_OUTLINES}
var
  edgeBmp: array of Byte = nil;


procedure drawOutlines ();
var
  r, g, b: Integer;

  procedure clearEdgeBmp ();
  begin
    SetLength(edgeBmp, (gScreenWidth+4)*(gScreenHeight+4));
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
      if (len > gScreenWidth+4) then len := gScreenWidth+4;
      if (sx < 0) then begin len += sx; sx := 0; end;
      if (sx+len > gScreenWidth+4) then len := gScreenWidth+4-sx;
      if (len < 1) then exit;
      assert(sx >= 0);
      assert(sx+len <= gScreenWidth+4);
      y0 := pan.Y-(vpy-1);
      y1 := y0+pan.Height;
      if (y0 < 0) then y0 := 0;
      if (y1 > gScreenHeight+4) then y1 := gScreenHeight+4;
      while (y0 < y1) do
      begin
        FillChar(edgeBmp[y0*(gScreenWidth+4)+sx], len*sizeof(edgeBmp[0]), 1);
        Inc(y0);
      end;
    end
    else
    begin
      drawRect(pan.X, pan.Y, pan.Width, pan.Height, r, g, b);
    end;
  end;

var
  lsx: Integer = -1;
  lex: Integer = -1;
  lsy: Integer = -1;

  procedure flushLine ();
  begin
    if (lsy > 0) and (lsx > 0) then
    begin
      if (lex = lsx) then
      begin
        glBegin(GL_POINTS);
          glVertex2f(lsx-1+vpx+0.37, lsy-1+vpy+0.37);
        glEnd();
      end
      else
      begin
        glBegin(GL_LINES);
          glVertex2f(lsx-1+vpx+0.37, lsy-1+vpy+0.37);
          glVertex2f(lex-0+vpx+0.37, lsy-1+vpy+0.37);
        glEnd();
      end;
    end;
    lsx := -1;
    lex := -1;
  end;

  procedure startLine (y: Integer);
  begin
    flushLine();
    lsy := y;
  end;

  procedure putPixel (x: Integer);
  begin
    if (x < 1) then exit;
    if (lex+1 <> x) then flushLine();
    if (lsx < 0) then lsx := x;
    lex := x;
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
    for y := 1 to vph do
    begin
      a := @edgeBmp[y*(gScreenWidth+4)+1];
      startLine(y);
      for x := 1 to vpw do
      begin
        if (a[0] <> 0) then
        begin
          if (a[-1] = 0) or (a[1] = 0) or (a[-(gScreenWidth+4)] = 0) or (a[gScreenWidth+4] = 0) or
             (a[-(gScreenWidth+4)-1] = 0) or (a[-(gScreenWidth+4)+1] = 0) or
             (a[gScreenWidth+4-1] = 0) or (a[gScreenWidth+4+1] = 0) then
          begin
            putPixel(x);
          end;
        end;
        Inc(a);
      end;
      flushLine();
    end;
  end;

  procedure drawFilledWalls ();
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
    for y := 1 to vph do
    begin
      a := @edgeBmp[y*(gScreenWidth+4)+1];
      startLine(y);
      for x := 1 to vpw do
      begin
        if (a[0] <> 0) then putPixel(x);
        Inc(a);
      end;
      flushLine();
    end;
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

{$ELSE}
var
  oliner: TOutliner = nil;

procedure drawOutlines ();
var
  r, g, b: Integer;

  procedure clearOliner ();
  begin
    //if (oliner <> nil) and ((oliner.height <> vph+2) or (oliner.width <> vpw+2)) then begin oliner.Free(); oliner := nil; end;
    if (oliner = nil) then oliner := TOutliner.Create(vpw+2, vph+2) else oliner.setup(vpw+2, vph+2);
  end;

  procedure drawOutline (ol: TOutliner; sx, sy: Integer);
    procedure xline (x0, x1, y: Integer);
    var
      x: Integer;
    begin
      if (g_dbg_scale < 1.0) then
      begin
        glBegin(GL_POINTS);
          for x := x0 to x1 do glVertex2f(sx+x+0.375, sy+y+0.375);
        glEnd();
      end
      else
      begin
        glBegin(GL_QUADS);
          glVertex2f(sx+x0+0, sy+y+0);
          glVertex2f(sx+x1+1, sy+y+0);
          glVertex2f(sx+x1+1, sy+y+1);
          glVertex2f(sx+x0+0, sy+y+1);
        glEnd();
      end;
    end;
  var
    y: Integer;
    sp: TOutliner.TSpanX;
  begin
    if (ol = nil) then exit;
    glPointSize(1);
    glDisable(GL_POINT_SMOOTH);
    for y := 0 to ol.height-1 do
    begin
      for sp in ol.eachSpanAtY(y) do
      begin
        if (g_dbg_scale <= 1.0) then
        begin
          glBegin(GL_POINTS);
            glVertex2f(sx+sp.x0+0.375, sy+y+0.375);
            glVertex2f(sx+sp.x1+0.375, sy+y+0.375);
          glEnd();
        end
        else
        begin
          glBegin(GL_QUADS);
            glVertex2f(sx+sp.x0+0, sy+y+0);
            glVertex2f(sx+sp.x0+1, sy+y+0);
            glVertex2f(sx+sp.x0+1, sy+y+1);
            glVertex2f(sx+sp.x0+0, sy+y+1);

            glVertex2f(sx+sp.x1+0, sy+y+0);
            glVertex2f(sx+sp.x1+1, sy+y+0);
            glVertex2f(sx+sp.x1+1, sy+y+1);
            glVertex2f(sx+sp.x1+0, sy+y+1);
          glEnd();
        end;
      end;
      for sp in ol.eachSpanEdgeAtY(y, -1) do
      begin
        xline(sp.x0, sp.x1, y);
        {
        glBegin(GL_QUADS);
          glVertex2f(sx+sp.x0+0, sy+y+0);
          glVertex2f(sx+sp.x1+1, sy+y+0);
          glVertex2f(sx+sp.x1+1, sy+y+1);
          glVertex2f(sx+sp.x0+0, sy+y+1);
        glEnd();
        }
      end;
      for sp in ol.eachSpanEdgeAtY(y, +1) do
      begin
        xline(sp.x0, sp.x1, y);
        {
        glBegin(GL_QUADS);
          glVertex2f(sx+sp.x0+0, sy+y+0);
          glVertex2f(sx+sp.x1+1, sy+y+0);
          glVertex2f(sx+sp.x1+1, sy+y+1);
          glVertex2f(sx+sp.x0+0, sy+y+1);
        glEnd();
        }
      end;
    end;
  end;

  procedure doWallsOld (parr: array of TPanel; ptype: Word; ar, ag, ab: Integer);
  var
    f: Integer;
    pan: TPanel;
  begin
    r := ar;
    g := ag;
    b := ab;
    if g_ol_nice then clearOliner();
    hlmContext.color := TGxRGBA.Create(r, g, b);
    for f := 0 to High(parr) do
    begin
      pan := parr[f];
      if (pan = nil) or not pan.Enabled or (pan.Width < 1) or (pan.Height < 1) then continue;
      if ((pan.PanelType and ptype) = 0) then continue;
      if (pan.X > vpx+vpw+41) or (pan.Y > vpy+vph+41) then continue;
      if (pan.X+pan.Width < vpx-41) then continue;
      if (pan.Y+pan.Height < vpy-41) then continue;
      if g_ol_nice then
      begin
        oliner.addRect(pan.X-(vpx+1), pan.Y-(vpy+1), pan.Width, pan.Height);
      end;
      if g_ol_fill_walls then
      begin
        hlmContext.fillRect(pan.X, pan.Y, pan.Width, pan.Height);
      end
      else if not g_ol_nice then
      begin
        hlmContext.rect(pan.X, pan.Y, pan.Width, pan.Height);
      end;
    end;
    if g_ol_nice then
    begin
      glColor4f(r/255.0, g/255.0, b/255.0, 1.0);
      drawOutline(oliner, vpx+1, vpy+1);
    end;
  end;

begin
  if (vpw < 2) or (vph < 2) then exit;
  if g_ol_rlayer_back then doWallsOld(gRenderBackgrounds, PANEL_BACK, 255, 127, 0);
  if g_ol_rlayer_step then doWallsOld(gSteps, PANEL_STEP, 192, 192, 192);
  if g_ol_rlayer_wall then doWallsOld(gWalls, PANEL_WALL, 255, 255, 255);
  if g_ol_rlayer_door then doWallsOld(gWalls, PANEL_OPENDOOR or PANEL_CLOSEDOOR, 0, 255, 0);
  if g_ol_rlayer_acid1 then doWallsOld(gAcid1, PANEL_ACID1, 255, 0, 0);
  if g_ol_rlayer_acid2 then doWallsOld(gAcid2, PANEL_ACID2, 198, 198, 0);
  if g_ol_rlayer_water then doWallsOld(gWater, PANEL_WATER, 0, 255, 255);
  if g_ol_rlayer_fore then doWallsOld(gRenderForegrounds, PANEL_FORE, 210, 210, 210);
end;
{$ENDIF}


procedure plrDebugDraw ();
  procedure drawTileGrid ();
  var
    x, y: Integer;
  begin
    hlmContext.color := TGxRGBA.Create(96, 96, 96);
    for y := 0 to (mapGrid.gridHeight div mapGrid.tileSize) do
    begin
      hlmContext.line(mapGrid.gridX0, mapGrid.gridY0+y*mapGrid.tileSize, mapGrid.gridX0+mapGrid.gridWidth, mapGrid.gridY0+y*mapGrid.tileSize);
    end;

    hlmContext.color := TGxRGBA.Create(96, 96, 96);
    for x := 0 to (mapGrid.gridWidth div mapGrid.tileSize) do
    begin
      hlmContext.line(mapGrid.gridX0+x*mapGrid.tileSize, mapGrid.gridY0, mapGrid.gridX0+x*mapGrid.tileSize, mapGrid.gridY0+y*mapGrid.gridHeight);
    end;
  end;

{$IFDEF ENABLE_GFX}
  procedure drawAwakeCells ();
  var
    x, y: Integer;
  begin
    hlmContext.color := TGxRGBA.Create(128, 0, 128, 64);
    for y := 0 to (mapGrid.gridHeight div mapGrid.tileSize) do
    begin
      for x := 0 to (mapGrid.gridWidth div mapGrid.tileSize) do
      begin
        if awmIsSetHolmes(x*mapGrid.tileSize+mapGrid.gridX0+1, y*mapGrid.tileSize++mapGrid.gridY0+1) then
        begin
          hlmContext.fillRect(x*mapGrid.tileSize++mapGrid.gridX0, y*mapGrid.tileSize++mapGrid.gridY0, monsGrid.tileSize, monsGrid.tileSize);
        end;
      end;
    end;
  end;
{$ENDIF}

  procedure drawTraceBox ();
  var
    plr: TPlayer;
    px, py, pw, ph: Integer;
    pdx, pdy: Integer;
    ex, ey: Integer;
    pan: TPanel;
  begin
    if (Length(gPlayers) < 1) then exit;
    plr := gPlayers[0];
    if (plr = nil) then exit;
    plr.getMapBox(px, py, pw, ph);
    hlmContext.color := TGxRGBA.Create(255, 0, 255, 200);
    hlmContext.rect(px, py, pw, ph);
    pdx := pmsCurMapX-(px+pw div 2);
    pdy := pmsCurMapY-(py+ph div 2);
    hlmContext.color := TGxRGBA.Create(255, 0, 255, 200);
    hlmContext.line(px+pw div 2, py+ph div 2, px+pw div 2+pdx, py+ph div 2+pdy);
    pan := mapGrid.traceBox(ex, ey, px, py, pw, ph, pdx, pdy, GridTagObstacle);
    if (pan = nil) then
    begin
      hlmContext.color := TGxRGBA.Create(255, 255, 255, 180);
      hlmContext.rect(px+pdx, py+pdy, pw, ph);
    end
    else
    begin
      hlmContext.color := TGxRGBA.Create(255, 255, 0, 180);
      hlmContext.rect(px+pdx, py+pdy, pw, ph);
    end;
    hlmContext.color := TGxRGBA.Create(255, 127, 0, 180);
    hlmContext.rect(ex, ey, pw, ph);
  end;

  procedure hilightCell (cx, cy: Integer);
  begin
    hlmContext.color := TGxRGBA.Create(0, 128, 0, 64);
    hlmContext.fillRect(cx, cy, monsGrid.tileSize, monsGrid.tileSize);
  end;

  procedure hilightBodyCells (proxyId: Integer);
  var
    it: CellCoordIter;
    pcellxy: PGridCellCoord;
  begin
    //monsGrid.forEachBodyCell(mon.proxyId, hilightCell);
    it := monsGrid.forEachBodyCell(proxyId);
    for pcellxy in it do hilightCell(pcellxy^.x, pcellxy^.y);
    it.release();
  end;

  procedure hilightCell1 (cx, cy: Integer);
  begin
    //e_WriteLog(Format('h1: (%d,%d)', [cx, cy]), MSG_NOTIFY);
    cx := cx and (not (monsGrid.tileSize-1));
    cy := cy and (not (monsGrid.tileSize-1));
    hlmContext.color := TGxRGBA.Create(255, 255, 0, 92);
    hlmContext.fillRect(cx, cy, monsGrid.tileSize, monsGrid.tileSize);
  end;

  function hilightWallTrc (pan: TPanel; tag: Integer; x, y, prevx, prevy: Integer): Boolean;
  begin
    result := false; // don't stop
    if (pan = nil) then exit; // cell completion, ignore
    //e_WriteLog(Format('h1: (%d,%d)', [cx, cy]), MSG_NOTIFY);
    hlmContext.color := TGxRGBA.Create(0, 128, 128, 64);
    hlmContext.fillRect(pan.X, pan.Y, pan.Width, pan.Height);
  end;

  procedure monsCollector (mon: TMonster);
  var
    ex, ey: Integer;
    mx, my, mw, mh: Integer;
  begin
    mon.getMapBox(mx, my, mw, mh);
    hlmContext.color := TGxRGBA.Create(255, 255, 0, 160);
    hlmContext.rect(mx, my, mw, mh);
    //e_DrawQuad(mx, my, mx+mw-1, my+mh-1, 255, 255, 0, 96);
    if lineAABBIntersects(laserX0, laserY0, laserX1, laserY1, mx, my, mw, mh, ex, ey) then
    begin
      //e_DrawPoint(8, ex, ey, 0, 255, 0);
      hlmContext.color := TGxRGBA.Create(0, 255, 0, 220);
      hlmContext.fillRect(ex-2, ey-2, 7, 7);
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
      hlmContext.color := TGxRGBA.Create(255, 0, 0);
      hlmContext.line(mx+mw div 2, my+mh div 2, emx+emw div 2, emy+emh div 2);
      if (g_Map_traceToNearestWall(mx+mw div 2, my+mh div 2, emx+emw div 2, emy+emh div 2, @ex, @ey) <> nil) then
      begin
        hlmContext.color := TGxRGBA.Create(0, 255, 0);
        hlmContext.line(mx+mw div 2, my+mh div 2, ex, ey);
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
      hlmContext.color := TGxRGBA.Create(255, 0, 0);
      hlmContext.line(mx+mw div 2, my+mh div 2, emx+emw div 2, emy+emh div 2);
      {$IF DEFINED(D2F_DEBUG)}
      mapGrid.dbgRayTraceTileHitCB := hilightCell1;
      {$ENDIF}
      if (g_Map_traceToNearestWall(mx+mw div 2, my+mh div 2, emx+emw div 2, emy+emh div 2, @ex, @ey) <> nil) then
      //if (mapGrid.traceRay(ex, ey, mx+mw div 2, my+mh div 2, emx+emw div 2, emy+emh div 2, hilightWallTrc, (GridTagWall or GridTagDoor)) <> nil) then
      begin
        hlmContext.color := TGxRGBA.Create(0, 255, 0);
        hlmContext.line(mx+mw div 2, my+mh div 2, ex, ey);
      end;
      {$IF DEFINED(D2F_DEBUG)}
      mapGrid.dbgRayTraceTileHitCB := nil;
      {$ENDIF}
    end;

  begin
    if (mon = nil) then exit;
    mon.getMapBox(mx, my, mw, mh);
    //mx += mw div 2;

    //monsGrid.forEachBodyCell(mon.proxyId, hilightCell);
    hilightBodyCells(mon.proxyId);

    if showMonsInfo then
    begin
      //fillRect(mx-4, my-7*8-6, 110, 7*8+6, 0, 0, 94, 250);
      hlmContext.font := 'msx6';
      hlmContext.color := TGxRGBA.Create(255, 127, 0);

      hlmContext.darkenRect(mx-4, my-7*hlmContext.charWidth(' ')-6, 110, 7*hlmContext.charWidth(' ')+6, 128);
      my -= 8;
      my -= 2;

      // type
      hlmContext.drawText(mx, my, Format('%s(U:%u)', [monsTypeToString(mon.MonsterType), mon.UID])); my -= hlmContext.charWidth(' ');
      // beh
      hlmContext.drawText(mx, my, Format('Beh: %s', [monsBehToString(mon.MonsterBehaviour)])); my -= hlmContext.charWidth(' ');
      // state
      hlmContext.drawText(mx, my, Format('State:%s (%d)', [monsStateToString(mon.MonsterState), mon.MonsterSleep])); my -= hlmContext.charWidth(' ');
      // health
      hlmContext.drawText(mx, my, Format('Health:%d', [mon.MonsterHealth])); my -= hlmContext.charWidth(' ');
      // ammo
      hlmContext.drawText(mx, my, Format('Ammo:%d', [mon.MonsterAmmo])); my -= hlmContext.charWidth(' ');
      // target
      hlmContext.drawText(mx, my, Format('TgtUID:%u', [mon.MonsterTargetUID])); my -= hlmContext.charWidth(' ');
      hlmContext.drawText(mx, my, Format('TgtTime:%d', [mon.MonsterTargetTime])); my -= hlmContext.charWidth(' ');
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
    //monsGrid.forEachBodyCell(mon.proxyId, hilightCell);
    hilightBodyCells(mon.proxyId);
  end;

  procedure drawSelectedPlatformCells ();
  var
    pan: TPanel;
  begin
    if not showGrid then exit;
    pan := g_Map_PanelByGUID(platMarkedGUID);
    if (pan = nil) then exit;
    //mapGrid.forEachBodyCell(pan.proxyId, hilightCell);
    hilightBodyCells(pan.proxyId);
    hlmContext.color := TGxRGBA.Create(0, 200, 0, 200);
    hlmContext.rect(pan.x, pan.y, pan.width, pan.height);
  end;

  procedure drawTrigger (var trig: TTrigger);

    procedure drawPanelDest (pguid: Integer);
    var
      pan: TPanel;
    begin
      pan := g_Map_PanelByGUID(pguid);
      if (pan = nil) then exit;
      hlmContext.color := TGxRGBA.Create(255, 0, 255, 220);
      hlmContext.line(trig.trigCenter.x, trig.trigCenter.y, pan.x+pan.width div 2, pan.y+pan.height div 2);
    end;

  var
    tts: AnsiString;
    tx: Integer;
  begin
    hlmContext.font := 'msx6';
    hlmContext.color := TGxRGBA.Create(255, 0, 255, 96);
    hlmContext.fillRect(trig.x, trig.y, trig.width, trig.height);
    tts := trigType2Str(trig.TriggerType);
    tx := trig.x+(trig.width-Length(tts)*6) div 2;
    hlmContext.darkenRect(tx-2, trig.y-10, Length(tts)*6+4, 10, 64);
    hlmContext.color := TGxRGBA.Create(255, 127, 0);
    hlmContext.drawText(tx, trig.y-9, tts);
    tx := trig.x+(trig.width-Length(trig.mapId)*6) div 2;
    hlmContext.darkenRect(tx-2, trig.y-20, Length(trig.mapId)*6+4, 10, 64);
    hlmContext.color := TGxRGBA.Create(255, 255, 0);
    hlmContext.drawText(tx, trig.y-19, trig.mapId);
    drawPanelDest(trig.trigPanelGUID);
    case trig.TriggerType of
      TRIGGER_NONE: begin end;
      TRIGGER_EXIT: begin end;
      TRIGGER_TELEPORT: begin end;
      TRIGGER_OPENDOOR: begin end;
      TRIGGER_CLOSEDOOR: begin end;
      TRIGGER_DOOR: begin end;
      TRIGGER_DOOR5: begin end;
      TRIGGER_CLOSETRAP: begin end;
      TRIGGER_TRAP: begin end;
      TRIGGER_SECRET: begin end;
      TRIGGER_LIFTUP: begin end;
      TRIGGER_LIFTDOWN: begin end;
      TRIGGER_LIFT: begin end;
      TRIGGER_TEXTURE: begin end;
      TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF, TRIGGER_PRESS:
        begin
          if (trig.trigDataRec.trigTWidth > 0) and (trig.trigDataRec.trigTHeight > 0) then
          begin
            hlmContext.color := TGxRGBA.Create(0, 255, 255, 42);
            hlmContext.fillRect(
              trig.trigDataRec.trigTX, trig.trigDataRec.trigTY,
              trig.trigDataRec.trigTWidth, trig.trigDataRec.trigTHeight);
            hlmContext.color := TGxRGBA.Create(255, 0, 255, 220);
            hlmContext.line(
              trig.trigCenter.x, trig.trigCenter.y,
              trig.trigDataRec.trigTX+trig.trigDataRec.trigTWidth div 2,
              trig.trigDataRec.trigTY+trig.trigDataRec.trigTHeight div 2);
          end;
        end;
      TRIGGER_SOUND: begin end;
      TRIGGER_SPAWNMONSTER: begin end;
      TRIGGER_SPAWNITEM: begin end;
      TRIGGER_MUSIC: begin end;
      TRIGGER_PUSH: begin end;
      TRIGGER_SCORE: begin end;
      TRIGGER_MESSAGE: begin end;
      TRIGGER_DAMAGE: begin end;
      TRIGGER_HEALTH: begin end;
      TRIGGER_SHOT: begin end;
      TRIGGER_EFFECT: begin end;
      TRIGGER_SCRIPT: begin end;
    end;
    //trigType2Str
    //trigPanelId: Integer;
  end;

  procedure drawTriggers ();
  var
    f: Integer;
  begin
    for f := 0 to High(gTriggers) do drawTrigger(gTriggers[f]);
  end;

{$IFDEF ENABLE_GIBS}
  procedure drawGibsBoxes ();
  var
    f: Integer;
    px, py, pw, ph: Integer;
    gib: PGib;
  begin
    for f := 0 to High(gGibs) do
    begin
      gib := @gGibs[f];
      if gib.alive then
      begin
        gib.getMapBox(px, py, pw, ph);
        hlmContext.color := TGxRGBA.Create(255, 0, 255);
        hlmContext.rect(px, py, pw, ph);
      end;
    end;
  end;
{$ENDIF}

var
  mon: TMonster;
  mit: PMonster;
  it: TMonsterGrid.Iter;
  mx, my, mw, mh: Integer;
  //pan: TPanel;
  //ex, ey: Integer;
  s: AnsiString;
  dx, dy: Integer;
begin
  if (gPlayer1 = nil) then exit;

  glPushMatrix;
  (* hack: scale and translate must be handled by hlmContext.glSetScaleTrans, but it dont work for some reason *)
  glScalef(g_dbg_scale, g_dbg_scale, 1.0);
  glTranslatef(-vpx, -vpy, 0);

  if (hlmContext = nil) then hlmContext := r_fui_gfx_gl.TGxContext.Create();

  gxSetContext(hlmContext);
  try
    //glScissor(0, gScreenHeight-gPlayerScreenSize.Y-1, vpw, vph);
    //hlmContext.clip := TGxRect.Create(0, gScreenHeight-gPlayerScreenSize.Y-1, gPlayerScreenSize.X, gPlayerScreenSize.Y);

    {
    glScalef(g_dbg_scale, g_dbg_scale, 1.0);
    glTranslatef(-vpx, -vpy, 0);
    }
//    hlmContext.glSetScaleTrans(g_dbg_scale, -vpx, -vpy); // uncomment when fix it
    glEnable(GL_SCISSOR_TEST);
    glScissor(0, gScreenHeight-gPlayerScreenSize.Y-1, gPlayerScreenSize.X, gPlayerScreenSize.Y);

    if (showGrid) then drawTileGrid();
    drawOutlines();

    if (laserSet) then
    begin
      //g_Mons_AlongLine(laserX0, laserY0, laserX1, laserY1, monsCollector, true);
      it := monsGrid.forEachAlongLine(laserX0, laserY0, laserX1, laserY1, -1, true);
      for mit in it do monsCollector(mit^);
      it.release();
    end;

    if (monMarkedUID <> -1) then
    begin
      mon := g_Monsters_ByUID(monMarkedUID);
      if (mon <> nil) then
      begin
        mon.getMapBox(mx, my, mw, mh);
        //e_DrawQuad(mx, my, mx+mw-1, my+mh-1, 255, 0, 0, 30);
        hlmContext.color := TGxRGBA.Create(255, 0, 0, 220);
        hlmContext.rect(mx, my, mw, mh);
        drawMonsterInfo(mon);
      end;
    end;

    if showAllMonsCells and showGrid then g_Mons_ForEach(highlightAllMonsterCells);
    if showTriggers then drawTriggers();
    if showGrid then drawSelectedPlatformCells();

    {$IFDEF ENABLE_GFX}
      // drawAwakeCells();
    {$ENDIF}

    if showTraceBox then drawTraceBox();

    {$IFDEF ENABLE_GIBS}
      // drawGibsBoxes();
    {$ENDIF}

    //pan := g_Map_traceToNearest(16, 608, 16, 8, (GridTagObstacle or GridTagLiquid), @ex, @ey);
    (*
    {$IF DEFINED(D2F_DEBUG)}
    mapGrid.dbgRayTraceTileHitCB := hilightCell1;
    {$ENDIF}
    pan := mapGrid.traceRay(ex, ey, 16, 608, 16, 8, nil, (GridTagObstacle or GridTagLiquid));
    if (pan <> nil) then writeln('end=(', ex, ',', ey, ')');
    {$IF DEFINED(D2F_DEBUG)}
    mapGrid.dbgRayTraceTileHitCB := nil;
    {$ENDIF}

    pan := g_Map_PanelAtPoint(16, 608, (GridTagObstacle or GridTagLiquid));
    if (pan <> nil) then writeln('hit!');
    *)

  finally
    gxSetContext(nil);
  end;

  glPopMatrix;

  if showMapCurPos then
  begin
    s := Format('mappos:(%d,%d)', [pmsCurMapX, pmsCurMapY]);
    gxSetContext(hlmContext);
    hlmContext.font := 'win8';
    hlmContext.color := TGxRGBA.Create(0, 0, 0);
    for dy := -1 to 1 do
    begin
      for dx := -1 to 1 do
      begin
        if (dx <> 0) or (dy <> 0) then hlmContext.drawText(4+dx, gScreenHeight-10+dy, s);
      end;
    end;
    hlmContext.color := TGxRGBA.Create(255, 255, 0);
    hlmContext.drawText(4, gScreenHeight-10, s);
    gxSetContext(nil);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure r_Holmes_Draw ();
begin
  if g_Game_IsNet then exit;
  if not g_holmes_enabled then exit;
  if g_holmes_imfunctional then exit;

  holmesInitCommands();
  holmesInitBinds();

{$IFDEF ENABLE_RENDER}
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE); // modify color buffer
  glDisable(GL_STENCIL_TEST);
  glDisable(GL_BLEND);
  glDisable(GL_SCISSOR_TEST);
  glDisable(GL_TEXTURE_2D);

  if gGameOn then plrDebugDraw();
{$ENDIF}

  laserSet := false;
end;


procedure r_Holmes_DrawUI ();
begin
  if g_Game_IsNet then exit;
  if not g_holmes_enabled then exit;
  if g_holmes_imfunctional then exit;

  gGfxDoClear := false;

{$IFDEF ENABLE_RENDER}
  //if assigned(prerenderFrameCB) then prerenderFrameCB();
  uiDraw();
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  try
    //glLoadIdentity();
    if assigned(postrenderFrameCB) then postrenderFrameCB();
  finally
    glPopMatrix();
  end;
{$ENDIF}
end;


begin
  conRegVar('hlm_ui_scale', @fuiRenderScale, 0.01, 5.0, 'Holmes UI scale', '', false);
end.
