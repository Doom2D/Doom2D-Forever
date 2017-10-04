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
  {$IFDEF USE_MEMPOOL}mempool,{$ENDIF} geom,
  e_log, e_input,
  g_textures, g_basic, e_graphics, g_phys, g_grid, g_player, g_monsters,
  g_window, g_map, g_triggers, g_items, g_game, g_panel, g_console, g_gfx,
  xprofiler,
  sdlcarcass,
  fui_common, fui_events, fui_ctls,
  fui_gfx_gl;


procedure g_Holmes_Draw ();
procedure g_Holmes_DrawUI ();

procedure g_Holmes_OnEvent (var ev: TFUIEvent);

// hooks for player
procedure g_Holmes_plrViewPos (viewPortX, viewPortY: Integer);
procedure g_Holmes_plrViewSize (viewPortW, viewPortH: Integer);
procedure g_Holmes_plrLaser (ax0, ay0, ax1, ay1: Integer);


var
  g_holmes_imfunctional: Boolean = false;
  g_holmes_enabled: Boolean = {$IF DEFINED(D2F_DEBUG)}true{$ELSE}false{$ENDIF};


implementation

uses
  {rttiobj,} typinfo, e_texture,
  SysUtils, Classes, GL, SDL2,
  MAPDEF, g_main, g_options,
  utils, hashtable, xparser;


var
  hlmContext: TGxContext = nil;
  //globalInited: Boolean = false;
  msX: Integer = -666;
  msY: Integer = -666;
  msB: Word = 0; // button state
  kbS: Word = 0; // keyboard modifiers state
  showGrid: Boolean = {$IF DEFINED(D2F_DEBUG)}false{$ELSE}false{$ENDIF};
  showMonsInfo: Boolean = false;
  showMonsLOS2Plr: Boolean = false;
  showAllMonsCells: Boolean = false;
  showMapCurPos: Boolean = false;
  showLayersWindow: Boolean = false;
  showOutlineWindow: Boolean = false;
  showTriggers: Boolean = {$IF DEFINED(D2F_DEBUG)}false{$ELSE}false{$ENDIF};
  showTraceBox: Boolean = {$IF DEFINED(D2F_DEBUG)}false{$ELSE}false{$ENDIF};


// ////////////////////////////////////////////////////////////////////////// //
{$INCLUDE g_holmes.inc}
{$INCLUDE g_holmes_ol.inc} // outliner


// ////////////////////////////////////////////////////////////////////////// //
{$INCLUDE g_holmes_cmd.inc}
procedure holmesInitCommands (); forward;
procedure holmesInitBinds (); forward;


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
  winHelp: TUITopWindow = nil;
  winOptions: TUITopWindow = nil;
  winLayers: TUITopWindow = nil;
  winOutlines: TUITopWindow = nil;


procedure createHelpWindow (); forward;
procedure createOptionsWindow (); forward;
procedure createLayersWindow (); forward;
procedure createOutlinesWindow (); forward;


procedure toggleLayersWindowCB (me: TUIControl);
begin
  showLayersWindow := not showLayersWindow;
  if showLayersWindow then
  begin
    if (winLayers = nil) then createLayersWindow();
    uiAddWindow(winLayers);
  end
  else
  begin
    uiRemoveWindow(winLayers);
  end;
end;

procedure toggleOutlineWindowCB (me: TUIControl);
begin
  showOutlineWindow := not showOutlineWindow;
  if showOutlineWindow then
  begin
    if (winOutlines = nil) then createOutlinesWindow();
    uiAddWindow(winOutlines);
  end
  else
  begin
    uiRemoveWindow(winOutlines);
  end;
end;


procedure createHelpWindow ();
  procedure addHelpEmptyLine ();
  var
    stx: TUIStaticText;
  begin
    stx := TUIStaticText.Create();
    stx.flExpand := true;
    stx.halign := 0; // center
    stx.text := '';
    stx.header := false;
    stx.line := false;
    winHelp.appendChild(stx);
  end;

  procedure addHelpCaptionLine (const txt: AnsiString);
  var
    stx: TUIStaticText;
  begin
    stx := TUIStaticText.Create();
    stx.flExpand := true;
    stx.halign := 0; // center
    stx.text := txt;
    stx.header := true;
    stx.line := true;
    winHelp.appendChild(stx);
  end;

  procedure addHelpCaption (const txt: AnsiString);
  var
    stx: TUIStaticText;
  begin
    stx := TUIStaticText.Create();
    stx.flExpand := true;
    stx.halign := 0; // center
    stx.text := txt;
    stx.header := true;
    stx.line := false;
    winHelp.appendChild(stx);
  end;

  procedure addHelpKeyMouse (const key, txt, grp: AnsiString);
  var
    box: TUIHBox;
    span: TUISpan;
    stx: TUIStaticText;
  begin
    box := TUIHBox.Create();
    box.flExpand := true;
      // key
      stx := TUIStaticText.Create();
      stx.flExpand := true;
      stx.halign := 1; // right
      stx.valign := 0; // center
      stx.text := key;
      stx.header := true;
      stx.line := false;
      stx.flHGroup := grp;
      box.appendChild(stx);
      // span
      span := TUISpan.Create();
      span.flDefaultSize := TLaySize.Create(12, 1);
      span.flExpand := true;
      box.appendChild(span);
      // text
      stx := TUIStaticText.Create();
      stx.flExpand := true;
      stx.halign := -1; // left
      stx.valign := 0; // center
      stx.text := txt;
      stx.header := false;
      stx.line := false;
      box.appendChild(stx);
    winHelp.appendChild(box);
  end;

  procedure addHelpKey (const key, txt: AnsiString); begin addHelpKeyMouse(key, txt, 'help-keys'); end;
  procedure addHelpMouse (const key, txt: AnsiString); begin addHelpKeyMouse(key, txt, 'help-mouse'); end;

var
  slist: array of AnsiString = nil;
  cmd: PHolmesCommand;
  bind: THolmesBinding;
  f: Integer;
  {
  llb: TUISimpleText;
  maxkeylen: Integer;
  s: AnsiString;
  }
begin
  winHelp := TUITopWindow.Create('Holmes Help');
  winHelp.escClose := true;
  winHelp.flHoriz := false;

  // keyboard
  for cmd in cmdlist do cmd.helpmark := false;

  //maxkeylen := 0;
  for bind in keybinds do
  begin
    if (Length(bind.key) = 0) then continue;
    if cmdlist.get(bind.cmdName, cmd) then
    begin
      if (Length(cmd.help) > 0) then
      begin
        cmd.helpmark := true;
        //if (maxkeylen < Length(bind.key)) then maxkeylen := Length(bind.key);
      end;
    end;
  end;

  for cmd in cmdlist do
  begin
    if not cmd.helpmark then continue;
    if (Length(cmd.help) = 0) then begin cmd.helpmark := false; continue; end;
    f := 0;
    while (f < Length(slist)) and (CompareText(slist[f], cmd.section) <> 0) do Inc(f);
    if (f = Length(slist)) then
    begin
      SetLength(slist, Length(slist)+1);
      slist[High(slist)] := cmd.section;
    end;
  end;

  addHelpCaptionLine('KEYBOARD');
  //llb := TUISimpleText.Create(0, 0);
  for f := 0 to High(slist) do
  begin
    //if (f > 0) then llb.appendItem('');
    if (f > 0) then addHelpEmptyLine();
    //llb.appendItem(slist[f], true, true);
    addHelpCaption(slist[f]);
    for cmd in cmdlist do
    begin
      if not cmd.helpmark then continue;
      if (CompareText(cmd.section, slist[f]) <> 0) then continue;
      for bind in keybinds do
      begin
        if (Length(bind.key) = 0) then continue;
        if (cmd.name = bind.cmdName) then
        begin
          //s := bind.key;
          //while (Length(s) < maxkeylen) do s += ' ';
          //s := '  '+s+' -- '+cmd.help;
          //llb.appendItem(s);
          addHelpMouse(bind.key, cmd.help);
        end;
      end;
    end;
  end;

  // mouse
  for cmd in cmdlist do cmd.helpmark := false;

  //maxkeylen := 0;
  for bind in msbinds do
  begin
    if (Length(bind.key) = 0) then continue;
    if cmdlist.get(bind.cmdName, cmd) then
    begin
      if (Length(cmd.help) > 0) then
      begin
        cmd.helpmark := true;
        //if (maxkeylen < Length(bind.key)) then maxkeylen := Length(bind.key);
      end;
    end;
  end;

  //llb.appendItem('');
  //llb.appendItem('mouse', true, true);
  if (f > 0) then addHelpEmptyLine();
  addHelpCaptionLine('MOUSE');
  for bind in msbinds do
  begin
    if (Length(bind.key) = 0) then continue;
    if cmdlist.get(bind.cmdName, cmd) then
    begin
      if (Length(cmd.help) > 0) then
      begin
        //s := bind.key;
        //while (Length(s) < maxkeylen) do s += ' ';
        //s := '  '+s+' -- '+cmd.help;
        //llb.appendItem(s);
        addHelpKey(bind.key, cmd.help);
      end;
    end;
  end;

  //winHelp.appendChild(llb);

  uiLayoutCtl(winHelp);
  winHelp.escClose := true;
  winHelp.centerInScreen();
end;


procedure winLayersClosed (me: TUIControl); begin showLayersWindow := false; end;
procedure winOutlinesClosed (me: TUIControl); begin showOutlineWindow := false; end;

procedure addCheckBox (parent: TUIControl; const text: AnsiString; pvar: PBoolean; const aid: AnsiString='');
var
  cb: TUICheckBox;
begin
  cb := TUICheckBox.Create();
  cb.flExpand := true;
  cb.setVar(pvar);
  cb.text := text;
  cb.id := aid;
  parent.appendChild(cb);
end;

procedure addButton (parent: TUIControl; const text: AnsiString; cb: TUIControl.TActionCB);
var
  but: TUIButton;
begin
  but := TUIButton.Create();
  //but.flExpand := true;
  but.actionCB := cb;
  but.text := text;
  parent.appendChild(but);
end;


procedure actionFillWalls (cb: TUIControl);
begin
  TUICheckBox(cb).checked := not TUICheckBox(cb).checked;
  TUICheckBox(cb.topLevel['cbcontour']).enabled := not TUICheckBox(cb).checked;
end;

procedure createLayersWindow ();
var
  box: TUIVBox;
begin
  winLayers := TUITopWindow.Create('layers');
  winLayers.flHoriz := false;
  winLayers.x0 := 10;
  winLayers.y0 := 10;
  winLayers.flHoriz := false;
  winLayers.escClose := true;
  winLayers.closeCB := winLayersClosed;

  box := TUIVBox.Create();
    addCheckBox(box, '~background', @g_rlayer_back);
    addCheckBox(box, '~steps', @g_rlayer_step);
    addCheckBox(box, '~walls', @g_rlayer_wall);
    addCheckBox(box, '~doors', @g_rlayer_door);
    addCheckBox(box, 'acid~1', @g_rlayer_acid1);
    addCheckBox(box, 'acid~2', @g_rlayer_acid2);
    addCheckBox(box, 'wate~r', @g_rlayer_water);
    addCheckBox(box, '~foreground', @g_rlayer_fore);
  winLayers.appendChild(box);

  uiLayoutCtl(winLayers);
end;


procedure createOutlinesWindow ();
var
  box: TUIVBox;
begin
  winOutlines := TUITopWindow.Create('outlines');
  winOutlines.flHoriz := false;
  winOutlines.x0 := 100;
  winOutlines.y0 := 30;
  winOutlines.flHoriz := false;
  winOutlines.escClose := true;
  winOutlines.closeCB := winOutlinesClosed;

  box := TUIVBox.Create();
  box.hasFrame := true;
  box.caption := 'layers';
    addCheckBox(box, '~background', @g_ol_rlayer_back);
    addCheckBox(box, '~steps', @g_ol_rlayer_step);
    addCheckBox(box, '~walls', @g_ol_rlayer_wall);
    addCheckBox(box, '~doors', @g_ol_rlayer_door);
    addCheckBox(box, 'acid~1', @g_ol_rlayer_acid1);
    addCheckBox(box, 'acid~2', @g_ol_rlayer_acid2);
    addCheckBox(box, 'wate~r', @g_ol_rlayer_water);
    addCheckBox(box, '~foreground', @g_ol_rlayer_fore);
  winOutlines.appendChild(box);

  box := TUIVBox.Create();
  box.hasFrame := true;
  box.caption := 'options';
    addCheckBox(box, 'fi~ll walls', @g_ol_fill_walls, 'cbfill');
    addCheckBox(box, 'con~tours', @g_ol_nice, 'cbcontour');
  winOutlines.appendChild(box);

  winOutlines.setActionCBFor('cbfill', actionFillWalls);

  uiLayoutCtl(winOutlines);
end;


procedure createOptionsWindow ();
var
  box: TUIBox;
  span: TUISpan;
begin
  winOptions := TUITopWindow.Create('Holmes Options');
  winOptions.flHoriz := false;
  winOptions.flHoriz := false;
  winOptions.escClose := true;

  box := TUIVBox.Create();
  box.hasFrame := true;
  box.caption := 'visual';
    addCheckBox(box, 'map ~grid', @showGrid);
    addCheckBox(box, 'cursor ~position on map', @showMapCurPos);
    addCheckBox(box, '~monster info', @showMonsInfo);
    addCheckBox(box, 'monster LO~S to player', @showMonsLOS2Plr);
    addCheckBox(box, 'monster ~cells (SLOW!)', @showAllMonsCells);
    addCheckBox(box, 'draw ~triggers (SLOW!)', @showTriggers);
  winOptions.appendChild(box);

  box := TUIHBox.Create();
  box.hasFrame := true;
  box.caption := 'windows';
  box.captionAlign := 0;
  box.flAlign := 0;
    addButton(box, '~layers', toggleLayersWindowCB);
    span := TUISpan.Create();
      span.flExpand := true;
      span.flDefaultSize := TLaySize.Create(4, 1);
      box.appendChild(span);
    addButton(box, '~outline', toggleOutlineWindowCB);
  winOptions.appendChild(box);

  uiLayoutCtl(winOptions);
  winOptions.centerInScreen();
end;


procedure toggleLayersWindow (arg: Integer=-1);
begin
  if (arg < 0) then showLayersWindow := not showLayersWindow else showLayersWindow := (arg > 0);
  showLayersWindow := not showLayersWindow; // hack for callback
  toggleLayersWindowCB(nil);
end;

procedure toggleOutlineWindow (arg: Integer=-1);
begin
  if (arg < 0) then showOutlineWindow := not showOutlineWindow else showOutlineWindow := (arg > 0);
  showOutlineWindow := not showOutlineWindow; // hack for callback
  toggleOutlineWindowCB(nil);
end;

procedure toggleHelpWindow (arg: Integer=-1);
begin
  if (winHelp = nil) then
  begin
    if (arg = 0) then exit;
    createHelpWindow();
  end;
       if (arg < 0) then begin if not uiVisibleWindow(winHelp) then uiAddWindow(winHelp) else uiRemoveWindow(winHelp); end
  else if (arg = 0) then begin if uiVisibleWindow(winHelp) then uiRemoveWindow(winHelp); end
  else begin if (not uiVisibleWindow(winHelp)) then uiAddWindow(winHelp); end;
  if (not uiVisibleWindow(winHelp)) then FreeAndNil(winHelp);
end;

procedure toggleOptionsWindow (arg: Integer=-1);
begin
  if (winOptions = nil) then createOptionsWindow();
       if (arg < 0) then begin if not uiVisibleWindow(winOptions) then uiAddWindow(winOptions) else uiRemoveWindow(winOptions); end
  else if (arg = 0) then begin if uiVisibleWindow(winOptions) then uiRemoveWindow(winOptions); end
  else begin if not uiVisibleWindow(winOptions) then uiAddWindow(winOptions); end
end;


// ////////////////////////////////////////////////////////////////////////// //
var
  vpSet: Boolean = false;
  vpx, vpy: Integer;
  vpw, vph: Integer;
  laserSet: Boolean = false;
  laserX0, laserY0, laserX1, laserY1: Integer;
  monMarkedUID: Integer = -1;
  platMarkedGUID: Integer = -1;


procedure g_Holmes_plrViewPos (viewPortX, viewPortY: Integer);
begin
  vpSet := true;
  vpx := viewPortX;
  vpy := viewPortY;
end;

procedure g_Holmes_plrViewSize (viewPortW, viewPortH: Integer);
begin
  vpSet := true;
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
      a := @edgeBmp[y*(gWinSizeX+4)+1];
      startLine(y);
      for x := 1 to vpw do
      begin
        if (a[0] <> 0) then
        begin
          if (a[-1] = 0) or (a[1] = 0) or (a[-(gWinSizeX+4)] = 0) or (a[gWinSizeX+4] = 0) or
             (a[-(gWinSizeX+4)-1] = 0) or (a[-(gWinSizeX+4)+1] = 0) or
             (a[gWinSizeX+4-1] = 0) or (a[gWinSizeX+4+1] = 0) then
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
      a := @edgeBmp[y*(gWinSizeX+4)+1];
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
    pan := mapGrid.traceBox(ex, ey, px, py, pw, ph, pdx, pdy, nil, GridTagObstacle);
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

  function monsCollector (mon: TMonster; tag: Integer): Boolean;
  var
    ex, ey: Integer;
    mx, my, mw, mh: Integer;
  begin
    result := false;
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

    monsGrid.forEachBodyCell(mon.proxyId, hilightCell);

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
    monsGrid.forEachBodyCell(mon.proxyId, hilightCell);
  end;

  procedure drawSelectedPlatformCells ();
  var
    pan: TPanel;
  begin
    if not showGrid then exit;
    pan := g_Map_PanelByGUID(platMarkedGUID);
    if (pan = nil) then exit;
    mapGrid.forEachBodyCell(pan.proxyId, hilightCell);
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

var
  mon: TMonster;
  mx, my, mw, mh: Integer;
  //pan: TPanel;
  //ex, ey: Integer;
begin
  if (gPlayer1 = nil) then exit;

  if (hlmContext = nil) then hlmContext := TGxContext.Create();

  gxSetContext(hlmContext);
  try
    //glScissor(0, gWinSizeY-gPlayerScreenSize.Y-1, vpw, vph);
    //hlmContext.clip := TGxRect.Create(0, gScreenHeight-gPlayerScreenSize.Y-1, gPlayerScreenSize.X, gPlayerScreenSize.Y);

    {
    glScalef(g_dbg_scale, g_dbg_scale, 1.0);
    glTranslatef(-vpx, -vpy, 0);
    }
    hlmContext.glSetScaleTrans(g_dbg_scale, -vpx, -vpy);
    glEnable(GL_SCISSOR_TEST);
    glScissor(0, gScreenHeight-gPlayerScreenSize.Y-1, gPlayerScreenSize.X, gPlayerScreenSize.Y);

    if (showGrid) then drawTileGrid();
    drawOutlines();

    if (laserSet) then g_Mons_AlongLine(laserX0, laserY0, laserX1, laserY1, monsCollector, true);

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

    //drawAwakeCells();

    if showTraceBox then drawTraceBox();

    //drawGibsBoxes();


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

  if showMapCurPos then
  begin
    gxSetContext(hlmContext);
    hlmContext.font := 'win8';
    hlmContext.color := TGxRGBA.Create(255, 255, 0);
    hlmContext.drawText(4, gWinSizeY-10, Format('mappos:(%d,%d)', [pmsCurMapX, pmsCurMapY]));
    gxSetContext(nil);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure onKeyEvent (var ev: TFUIEvent);
{$IF DEFINED(D2F_DEBUG)}
var
  pan: TPanel;
  ex, ey: Integer;
  dx, dy: Integer;
{$ENDIF}

  procedure dummyWallTrc (cx, cy: Integer);
  begin
  end;

begin
  // press
  if (ev.press) then
  begin
    {$IF DEFINED(D2F_DEBUG)}
    // C-UP, C-DOWN, C-LEFT, C-RIGHT: trace 10 pixels from cursor in the respective direction
    if ((ev.scan = SDL_SCANCODE_UP) or (ev.scan = SDL_SCANCODE_DOWN) or (ev.scan = SDL_SCANCODE_LEFT) or (ev.scan = SDL_SCANCODE_RIGHT)) and
       ((ev.kstate and TFUIEvent.ModCtrl) <> 0) then
    begin
      ev.eat();
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
    {$ENDIF}
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_Holmes_OnEvent (var ev: TFUIEvent);
{$IF not DEFINED(HEADLESS)}
var
  doeat: Boolean = false;
{$ENDIF}
begin
{$IF not DEFINED(HEADLESS)}
  if g_Game_IsNet then exit;
  if not g_holmes_enabled then exit;
  if g_holmes_imfunctional then exit;

  holmesInitCommands();
  holmesInitBinds();

  msB := ev.bstate;
  kbS := ev.kstate;

  if (ev.key) then
  begin
    case ev.scan of
      SDL_SCANCODE_LCTRL, SDL_SCANCODE_RCTRL,
      SDL_SCANCODE_LALT, SDL_SCANCODE_RALT,
      SDL_SCANCODE_LSHIFT, SDL_SCANCODE_RSHIFT:
        doeat := true;
    end;
  end
  else if (ev.mouse) then
  begin
    msX := ev.x;
    msY := ev.y;
    msB := ev.bstate;
    kbS := ev.kstate;
    msB := msB;
  end;

  uiDispatchEvent(ev);
  if (not ev.alive) then exit;

  if (ev.mouse) then
  begin
    if (gPlayer1 <> nil) and (vpSet) then msbindExecute(ev);
    ev.eat();
  end
  else
  begin
    if keybindExecute(ev) then ev.eat();
    if (ev.alive) then onKeyEvent(ev);
  end;

  if (doeat) then ev.eat();
{$ENDIF}
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_Holmes_Draw ();
begin
  if g_Game_IsNet then exit;
  if not g_holmes_enabled then exit;
  if g_holmes_imfunctional then exit;

  {$IF not DEFINED(HEADLESS)}
  holmesInitCommands();
  holmesInitBinds();

  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE); // modify color buffer
  glDisable(GL_STENCIL_TEST);
  glDisable(GL_BLEND);
  glDisable(GL_SCISSOR_TEST);
  glDisable(GL_TEXTURE_2D);

  if gGameOn then plrDebugDraw();
  {$ENDIF}

  laserSet := false;
end;


procedure g_Holmes_DrawUI ();
begin
  if g_Game_IsNet then exit;
  if not g_holmes_enabled then exit;
  if g_holmes_imfunctional then exit;

  {$IF not DEFINED(HEADLESS)}
  gGfxDoClear := false;
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


// ////////////////////////////////////////////////////////////////////////// //
procedure bcOneMonsterThinkStep (); begin gmon_debug_think := false; gmon_debug_one_think_step := true; end;
procedure bcOneMPlatThinkStep (); begin g_dbgpan_mplat_active := false; g_dbgpan_mplat_step := true; end;
procedure bcMPlatToggle (); begin g_dbgpan_mplat_active := not g_dbgpan_mplat_active; end;

procedure bcToggleMonsterInfo (arg: Integer=-1); begin if (arg < 0) then showMonsInfo := not showMonsInfo else showMonsInfo := (arg > 0); end;
procedure bcToggleMonsterLOSPlr (arg: Integer=-1); begin if (arg < 0) then showMonsLOS2Plr := not showMonsLOS2Plr else showMonsLOS2Plr := (arg > 0); end;
procedure bcToggleMonsterCells (arg: Integer=-1); begin if (arg < 0) then showAllMonsCells := not showAllMonsCells else showAllMonsCells := (arg > 0); end;
procedure bcToggleDrawTriggers (arg: Integer=-1); begin if (arg < 0) then showTriggers := not showTriggers else showTriggers := (arg > 0); end;

procedure bcToggleCurPos (arg: Integer=-1); begin if (arg < 0) then showMapCurPos := not showMapCurPos else showMapCurPos := (arg > 0); end;
procedure bcToggleGrid (arg: Integer=-1); begin if (arg < 0) then showGrid := not showGrid else showGrid := (arg > 0); end;

procedure bcMonsterSpawn (s: AnsiString);
var
  mon: TMonster;
begin
  if not gGameOn or g_Game_IsClient then
  begin
    conwriteln('cannot spawn monster in this mode');
    exit;
  end;
  mon := g_Mons_SpawnAt(s, pmsCurMapX, pmsCurMapY);
  if (mon = nil) then begin conwritefln('unknown monster id: ''%s''', [s]); exit; end;
  monMarkedUID := mon.UID;
end;

procedure bcMonsterWakeup ();
var
  mon: TMonster;
begin
  if (monMarkedUID <> -1) then
  begin
    mon := g_Monsters_ByUID(monMarkedUID);
    if (mon <> nil) then mon.WakeUp();
  end;
end;

procedure bcPlayerTeleport ();
var
  x, y, w, h: Integer;
begin
  //e_WriteLog(Format('TELEPORT: (%d,%d)', [pmsCurMapX, pmsCurMapY]), MSG_NOTIFY);
  if (gPlayers[0] <> nil) then
  begin
    gPlayers[0].getMapBox(x, y, w, h);
    gPlayers[0].TeleportTo(pmsCurMapX-w div 2, pmsCurMapY-h div 2, true, 69); // 69: don't change dir
  end;
end;

procedure dbgToggleTraceBox (arg: Integer=-1); begin if (arg < 0) then showTraceBox := not showTraceBox else showTraceBox := (arg > 0); end;

procedure dbgToggleHolmesPause (arg: Integer=-1); begin if (arg < 0) then g_Game_HolmesPause(not gPauseHolmes) else g_Game_HolmesPause(arg > 0); end;

procedure cbAtcurSelectMonster ();
  function monsAtDump (mon: TMonster; tag: Integer): Boolean;
  begin
    result := true; // stop
    e_WriteLog(Format('monster #%d (UID:%u) (proxyid:%d)', [mon.arrIdx, mon.UID, mon.proxyId]), TMsgType.Notify);
    monMarkedUID := mon.UID;
    dumpPublishedProperties(mon);
  end;
var
  plr: TPlayer;
  x, y, w, h: Integer;
begin
  monMarkedUID := -1;
  if (Length(gPlayers) > 0) then
  begin
    plr := gPlayers[0];
    if (plr <> nil) then
    begin
      plr.getMapBox(x, y, w, h);
      if (pmsCurMapX >= x) and (pmsCurMapY >= y) and (pmsCurMapX < x+w) and (pmsCurMapY < y+h) then
      begin
        dumpPublishedProperties(plr);
      end;
    end;
  end;
  //e_WriteLog('===========================', MSG_NOTIFY);
  monsGrid.forEachAtPoint(pmsCurMapX, pmsCurMapY, monsAtDump);
  //e_WriteLog('---------------------------', MSG_NOTIFY);
end;

procedure cbAtcurDumpMonsters ();
  function monsAtDump (mon: TMonster; tag: Integer): Boolean;
  begin
    result := false; // don't stop
    e_WriteLog(Format('monster #%d (UID:%u) (proxyid:%d)', [mon.arrIdx, mon.UID, mon.proxyId]), TMsgType.Notify);
  end;
begin
  e_WriteLog('===========================', TMsgType.Notify);
  monsGrid.forEachAtPoint(pmsCurMapX, pmsCurMapY, monsAtDump);
  e_WriteLog('---------------------------', TMsgType.Notify);
end;

procedure cbAtcurDumpWalls ();
  function wallToggle (pan: TPanel; tag: Integer): Boolean;
  begin
    result := false; // don't stop
    if (platMarkedGUID = -1) then platMarkedGUID := pan.guid;
    e_LogWritefln('wall ''%s'' #%d(%d); enabled=%d (%d); (%d,%d)-(%d,%d)', [pan.mapId, pan.arrIdx, pan.proxyId, Integer(pan.Enabled), Integer(mapGrid.proxyEnabled[pan.proxyId]), pan.X, pan.Y, pan.Width, pan.Height]);
    dumpPublishedProperties(pan);
  end;
var
  hasTrigs: Boolean = false;
  f: Integer;
  trig: PTrigger;
begin
  platMarkedGUID := -1;
  e_WriteLog('=== TOGGLE WALL ===', TMsgType.Notify);
  mapGrid.forEachAtPoint(pmsCurMapX, pmsCurMapY, wallToggle, (GridTagWall or GridTagDoor));
  e_WriteLog('--- toggle wall ---', TMsgType.Notify);
  if showTriggers then
  begin
    for f := 0 to High(gTriggers) do
    begin
      trig := @gTriggers[f];
      if (pmsCurMapX >= trig.x) and (pmsCurMapY >= trig.y) and (pmsCurMapX < trig.x+trig.width) and (pmsCurMapY < trig.y+trig.height) then
      begin
        if not hasTrigs then begin writeln('=== TRIGGERS ==='); hasTrigs := true; end;
        writeln('trigger ''', trig.mapId, ''' of type ''', trigType2Str(trig.TriggerType), '''');
      end;
    end;
    if hasTrigs then writeln('--- triggers ---');
  end;
end;

procedure cbAtcurToggleWalls ();
  function wallToggle (pan: TPanel; tag: Integer): Boolean;
  begin
    result := false; // don't stop
    //e_WriteLog(Format('wall #%d(%d); enabled=%d (%d); (%d,%d)-(%d,%d)', [pan.arrIdx, pan.proxyId, Integer(pan.Enabled), Integer(mapGrid.proxyEnabled[pan.proxyId]), pan.X, pan.Y, pan.Width, pan.Height]), MSG_NOTIFY);
    if pan.Enabled then g_Map_DisableWallGUID(pan.guid) else g_Map_EnableWallGUID(pan.guid);
  end;
begin
  //e_WriteLog('=== TOGGLE WALL ===', MSG_NOTIFY);
  mapGrid.forEachAtPoint(pmsCurMapX, pmsCurMapY, wallToggle, (GridTagWall or GridTagDoor));
  //e_WriteLog('--- toggle wall ---', MSG_NOTIFY);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure holmesInitCommands ();
begin
  if (cmdlist <> nil) then exit;
  cmdAdd('win_layers', toggleLayersWindow, 'toggle layers window', 'window control');
  cmdAdd('win_outline', toggleOutlineWindow, 'toggle outline window', 'window control');
  cmdAdd('win_help', toggleHelpWindow, 'toggle help window', 'window control');
  cmdAdd('win_options', toggleOptionsWindow, 'toggle options window', 'window control');

  cmdAdd('mon_think_step', bcOneMonsterThinkStep, 'one monster think step', 'monster control');
  cmdAdd('mon_info', bcToggleMonsterInfo, 'toggle monster info', 'monster control');
  cmdAdd('mon_los_plr', bcToggleMonsterLOSPlr, 'toggle monster LOS to player', 'monster control');
  cmdAdd('mon_cells', bcToggleMonsterCells, 'toggle "show all cells occupied by monsters" (SLOW!)', 'monster control');
  cmdAdd('mon_wakeup', bcMonsterWakeup, 'wake up selected monster', 'monster control');

  cmdAdd('mon_spawn', bcMonsterSpawn, 'spawn monster', 'monster control');

  cmdAdd('mplat_step', bcOneMPlatThinkStep, 'one mplat think step', 'mplat control');
  cmdAdd('mplat_toggle', bcMPlatToggle, 'activate/deactivate moving platforms', 'mplat control');

  cmdAdd('plr_teleport', bcPlayerTeleport, 'teleport player', 'player control');

  cmdAdd('dbg_curpos', bcToggleCurPos, 'toggle "show cursor position on the map"', 'various');
  cmdAdd('dbg_grid', bcToggleGrid, 'toggle grid', 'various');
  cmdAdd('dbg_triggers', bcToggleDrawTriggers, 'show/hide triggers (SLOW!)', 'various');

  cmdAdd('atcur_select_monster', cbAtcurSelectMonster, 'select monster to operate', 'monster control');
  cmdAdd('atcur_dump_monsters', cbAtcurDumpMonsters, 'dump monsters in cell', 'monster control');
  cmdAdd('atcur_dump_walls', cbAtcurDumpWalls, 'dump walls in cell', 'wall control');
  cmdAdd('atcur_disable_walls', cbAtcurToggleWalls, 'disable walls', 'wall control');

  cmdAdd('dbg_tracebox', dbgToggleTraceBox, 'test traceBox()', 'player control');

  cmdAdd('hlm_pause', dbgToggleHolmesPause, '"Holmes" pause mode', 'game control');
end;


procedure holmesInitBinds ();
var
  st: TStream = nil;
  pr: TTextParser = nil;
  s, kn, v: AnsiString;
  kmods: Byte;
  mbuts: Byte;
begin
  kbS := kbS;
  if not keybindsInited then
  begin
    // keyboard
    keybindAdd('F1', 'win_help');
    keybindAdd('M-F1', 'win_options');
    keybindAdd('C-O', 'win_outline');
    keybindAdd('C-L', 'win_layers');

    keybindAdd('M-M', 'mon_think_step');
    keybindAdd('M-I', 'mon_info');
    keybindAdd('M-L', 'mon_los_plr');
    keybindAdd('M-G', 'mon_cells');
    keybindAdd('M-A', 'mon_wakeup');

    keybindAdd('M-P', 'mplat_step');
    keybindAdd('M-O', 'mplat_toggle');

    keybindAdd('C-T', 'plr_teleport');
    keybindAdd('M-T', 'dbg_tracebox');

    keybindAdd('C-P', 'dbg_curpos');
    keybindAdd('C-G', 'dbg_grid');
    keybindAdd('C-X', 'dbg_triggers');

    keybindAdd('C-1', 'mon_spawn zombie');

    keybindAdd('C-S-P', 'hlm_pause');

    // mouse
    msbindAdd('LMB', 'atcur_select_monster');
    msbindAdd('M-LMB', 'atcur_dump_monsters');
    msbindAdd('RMB', 'atcur_dump_walls');
    msbindAdd('M-RMB', 'atcur_disable_walls');

    // load bindings from file
    try
      st := openDiskFileRO(GameDir+'holmes.rc');
      pr := TFileTextParser.Create(st);
      conwriteln('parsing "holmes.rc"...');
      while (pr.tokType <> pr.TTEOF) do
      begin
        s := pr.expectId();
             if (s = 'stop') then break
        else if (s = 'unbind_keys') then keybinds := nil
        else if (s = 'unbind_mouse') then msbinds := nil
        else if (s = 'bind') then
        begin
               if (pr.tokType = pr.TTStr) then s := pr.expectStr(false)
          else if (pr.tokType = pr.TTInt) then s := Format('%d', [pr.expectInt()])
          else s := pr.expectId();

               if (pr.tokType = pr.TTStr) then v := pr.expectStr(false)
          else if (pr.tokType = pr.TTInt) then v := Format('%d', [pr.expectInt()])
          else v := pr.expectId();

          kn := parseModKeys(s, kmods, mbuts);
          if (CompareText(kn, 'lmb') = 0) or (CompareText(kn, 'rmb') = 0) or (CompareText(kn, 'mmb') = 0) or (CompareText(kn, 'None') = 0) then
          begin
            msbindAdd(s, v);
          end
          else
          begin
            keybindAdd(s, v);
          end;
        end;
      end;
    except on e: Exception do // sorry
      if (pr <> nil) then conwritefln('Holmes config parse error at (%s,%s): %s', [pr.tokLine, pr.tokCol, e.message]);
    end;
    if (pr <> nil) then pr.Free() else st.Free(); // ownership
  end;
end;


begin
  // shut up, fpc!
  msB := msB;
  vpSet := vpSet;

  fuiEventCB := g_Holmes_OnEvent;
  //uiContext.font := 'win14';

  conRegVar('hlm_ui_scale', @fuiRenderScale, 0.01, 5.0, 'Holmes UI scale', '', false);
end.
