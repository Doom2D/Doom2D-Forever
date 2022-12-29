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
unit g_holmes;

interface

  procedure holmesInitCommands ();
  procedure holmesInitBinds ();

  function monsTypeToString (mt: Byte): AnsiString;
  function monsBehToString (bt: Byte): AnsiString;
  function monsStateToString (st: Byte): AnsiString;
  function trigType2Str (ttype: Integer): AnsiString;

  var
    g_holmes_imfunctional: Boolean = false;
    g_holmes_enabled: Boolean = {$IF DEFINED(D2F_DEBUG)}true{$ELSE}false{$ENDIF};

  var
    msX: Integer = -666;
    msY: Integer = -666;
    showGrid: Boolean = {$IF DEFINED(D2F_DEBUG)}false{$ELSE}false{$ENDIF};
    showMonsInfo: Boolean = false;
    showMonsLOS2Plr: Boolean = false;
    showAllMonsCells: Boolean = false;
    showMapCurPos: Boolean = false;
    showLayersWindow: Boolean = false;
    showOutlineWindow: Boolean = false;
    showTriggers: Boolean = {$IF DEFINED(D2F_DEBUG)}false{$ELSE}false{$ENDIF};
    showTraceBox: Boolean = {$IF DEFINED(D2F_DEBUG)}false{$ELSE}false{$ENDIF};

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

  var
    monMarkedUID: Integer = -1;
    platMarkedGUID: Integer = -1;

implementation

uses
  {mempool,}
  e_log, e_input,
  g_player, g_monsters,
  g_map, g_triggers, g_game, g_panel, g_console,
  {xprofiler,}
  fui_common, fui_events, fui_ctls,
  {$IFDEF ENABLE_RENDER}
    r_render,
  {$ENDIF}
  {rttiobj,} typinfo, e_res,
  SysUtils, Classes,
  {$IFDEF USE_SDL2}
    SDL2,
  {$ENDIF}
  MAPDEF, g_options,
  hashtable, xparser;


var
  //globalInited: Boolean = false;
  msB: Word = 0; // button state
  kbS: Word = 0; // keyboard modifiers state


// ////////////////////////////////////////////////////////////////////////// //
{$INCLUDE g_holmes.inc}


// ////////////////////////////////////////////////////////////////////////// //
{$INCLUDE g_holmes_cmd.inc}


// ////////////////////////////////////////////////////////////////////////// //

{$IF NOT DEFINED(ENABLE_RENDER)}
function pmsCurMapX (): Integer; inline;
begin
  result := round(msX/g_dbg_scale)
end;

function pmsCurMapY (): Integer; inline;
begin
  result := round(msY/g_dbg_scale)
end;

function r_Render_HolmesViewIsSet (): Boolean;
begin
  result := false
end;
{$ENDIF}


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
{$IFDEF USE_SDL2}
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
{$ELSE}
procedure onKeyEvent (var ev: TFUIEvent);
begin
end;
{$ENDIF}


// ////////////////////////////////////////////////////////////////////////// //
procedure g_Holmes_OnEvent (var ev: TFUIEvent);
  var doeat: Boolean = false;
begin
  if g_Game_IsNet then exit;
  if not g_holmes_enabled then exit;
  if g_holmes_imfunctional then exit;

  holmesInitCommands();
  holmesInitBinds();

  msB := ev.bstate;
  kbS := ev.kstate;

  if (ev.key) then
  begin
{$IFDEF USE_SDL2}
    case ev.scan of
      SDL_SCANCODE_LCTRL, SDL_SCANCODE_RCTRL,
      SDL_SCANCODE_LALT, SDL_SCANCODE_RALT,
      SDL_SCANCODE_LSHIFT, SDL_SCANCODE_RSHIFT:
        doeat := true;
    end;
{$ENDIF}
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
    if (gPlayer1 <> nil) and r_Render_HolmesViewIsSet() then msbindExecute(ev);
    ev.eat();
  end
  else
  begin
    if keybindExecute(ev) then ev.eat();
    if (ev.alive) then onKeyEvent(ev);
  end;

  if (doeat) then ev.eat();
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
  function monsAtDump (mon: TMonster{; tag: Integer}): Boolean;
  begin
    result := true; // stop
    e_WriteLog(Format('monster #%d (UID:%u) (proxyid:%d)', [mon.arrIdx, mon.UID, mon.proxyId]), TMsgType.Notify);
    monMarkedUID := mon.UID;
    dumpPublishedProperties(mon);
  end;
var
  plr: TPlayer;
  x, y, w, h: Integer;
  mit: PMonster;
  it: TMonsterGrid.Iter;
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
  it := monsGrid.forEachAtPoint(pmsCurMapX, pmsCurMapY);
  for mit in it do monsAtDump(mit^);
  it.release();
  //e_WriteLog('---------------------------', MSG_NOTIFY);
end;

procedure cbAtcurDumpMonsters ();
  function monsAtDump (mon: TMonster{; tag: Integer}): Boolean;
  begin
    result := false; // don't stop
    e_WriteLog(Format('monster #%d (UID:%u) (proxyid:%d)', [mon.arrIdx, mon.UID, mon.proxyId]), TMsgType.Notify);
  end;
var
  mit: PMonster;
  it: TMonsterGrid.Iter;
begin
  it := monsGrid.forEachAtPoint(pmsCurMapX, pmsCurMapY);
  if (it.length > 0) then
  begin
    e_WriteLog('===========================', TMsgType.Notify);
    for mit in it do monsAtDump(mit^);
    e_WriteLog('---------------------------', TMsgType.Notify);
  end;
  it.release();
end;

procedure cbAtcurDumpWalls ();
  function wallToggle (pan: TPanel{; tag: Integer}): Boolean;
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
  mwit: PPanel;
  it: TPanelGrid.Iter;
begin
  platMarkedGUID := -1;
  it := mapGrid.forEachAtPoint(pmsCurMapX, pmsCurMapY, (GridTagWall or GridTagDoor));
  if (it.length > 0) then
  begin
    e_WriteLog('=== TOGGLE WALL ===', TMsgType.Notify);
    for mwit in it do wallToggle(mwit^);
    e_WriteLog('--- toggle wall ---', TMsgType.Notify);
  end;
  it.release();
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
  function wallToggle (pan: TPanel{; tag: Integer}): Boolean;
  begin
    result := false; // don't stop
    //e_WriteLog(Format('wall #%d(%d); enabled=%d (%d); (%d,%d)-(%d,%d)', [pan.arrIdx, pan.proxyId, Integer(pan.Enabled), Integer(mapGrid.proxyEnabled[pan.proxyId]), pan.X, pan.Y, pan.Width, pan.Height]), MSG_NOTIFY);
    if pan.Enabled then g_Map_DisableWallGUID(pan.guid) else g_Map_EnableWallGUID(pan.guid);
  end;
var
  mwit: PPanel;
  it: TPanelGrid.Iter;
begin
  //e_WriteLog('=== TOGGLE WALL ===', MSG_NOTIFY);
  //e_WriteLog('--- toggle wall ---', MSG_NOTIFY);
  it := mapGrid.forEachAtPoint(pmsCurMapX, pmsCurMapY, (GridTagWall or GridTagDoor));
  for mwit in it do wallToggle(mwit^);
  it.release();
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
      st := e_OpenResourceRO(ConfigDirs, 'holmes.rc');
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

  fuiEventCB := g_Holmes_OnEvent;
  //uiContext.font := 'win14';
end.
