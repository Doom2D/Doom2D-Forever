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
// ////////////////////////////////////////////////////////////////////////// //
type
  THControl = class
  public
    type TActionCB = procedure (me: THControl; uinfo: Integer);

  private
    mParent: THControl;
    mX, mY: Integer;
    mWidth, mHeight: Integer;
    mFrameWidth, mFrameHeight: Integer;
    mEnabled: Boolean;
    mCanFocus: Boolean;
    mChildren: array of THControl;
    mFocused: THControl; // valid only for top-level controls
    mGrab: THControl; // valid only for top-level controls
    mEscClose: Boolean; // valid only for top-level controls
    mEatKeys: Boolean;
    mDrawShadow: Boolean;

  private
    scallowed: Boolean;
    scxywh: array[0..3] of GLint;

  protected
    function getEnabled (): Boolean;
    procedure setEnabled (v: Boolean); inline;

    function getFocused (): Boolean; inline;
    procedure setFocused (v: Boolean); inline;

    function isMyChild (ctl: THControl): Boolean;

    function findFirstFocus (): THControl;
    function findLastFocus (): THControl;

    function findNextFocus (cur: THControl): THControl;
    function findPrevFocus (cur: THControl): THControl;

    procedure activated (); virtual;
    procedure blurred (); virtual;

    //WARNING! do not call scissor functions outside `.draw*()` API!
    // reset scissor to whole control
    procedure resetScissor ();
    // set scissor to this internal rect (in local coords)
    procedure setScissor (lx, ly, lw, lh: Integer);

    // DO NOT USE!
    procedure setScissorGLInternal (x, y, w, h: Integer);

  public
    // return `false` if destination rect is empty
    // modifies rect0
    class function intersectRect (var x0, y0, w0, h0: Integer; const x1, y1, w1, h1: Integer): Boolean;

  public
    actionCB: TActionCB;

  public
    constructor Create (ax, ay, aw, ah: Integer; aparent: THControl=nil);
    destructor Destroy (); override;

    // `sx` and `sy` are screen coordinates
    procedure drawControl (sx, sy: Integer); virtual;

    // called after all children drawn
    procedure drawControlPost (sx, sy: Integer); virtual;

    procedure draw (); virtual;

    function topLevel (): THControl; inline;

    // returns `true` if global coords are inside this control
    function toLocal (var x, y: Integer): Boolean;
    procedure toGlobal (var x, y: Integer);

    // x and y are global coords
    function controlAtXY (x, y: Integer): THControl;

    function mouseEvent (var ev: THMouseEvent): Boolean; virtual; // returns `true` if event was eaten
    function keyEvent (var ev: THKeyEvent): Boolean; virtual; // returns `true` if event was eaten

    function prevSibling (): THControl;
    function nextSibling (): THControl;
    function firstChild (): THControl; inline;
    function lastChild (): THControl; inline;

    procedure appendChild (ctl: THControl); virtual;

  public
    property x0: Integer read mX;
    property y0: Integer read mY;
    property height: Integer read mHeight;
    property width: Integer read mWidth;
    property enabled: Boolean read getEnabled write setEnabled;
    property parent: THControl read mParent;
    property focused: Boolean read getFocused write setFocused;
    property escClose: Boolean read mEscClose write mEscClose;
    property eatKeys: Boolean read mEatKeys write mEatKeys;
  end;


  THTopWindow = class(THControl)
  private
    mTitle: AnsiString;
    mDragging: Boolean;
    mDragStartX, mDragStartY: Integer;
    mWaitingClose: Boolean;
    mInClose: Boolean;

  protected
    procedure blurred (); override;

  public
    closeCB: TActionCB; // called after window was removed from ui window list

  public
    constructor Create (const atitle: AnsiString; ax, ay: Integer; aw: Integer=-1; ah: Integer=-1);

    // `sx` and `sy` are screen coordinates
    procedure drawControl (sx, sy: Integer); override;
    procedure drawControlPost (sx, sy: Integer); override;

    function keyEvent (var ev: THKeyEvent): Boolean; override; // returns `true` if event was eaten
    function mouseEvent (var ev: THMouseEvent): Boolean; override; // returns `true` if event was eaten
  end;


  THCtlCBListBox = class(THControl)
  private
    type
      PItem = ^TItem;
      TItem = record
        title: AnsiString;
        varp: PBoolean;
        actionCB: TActionCB;
      end;
  private
    mItems: array of TItem;
    mCurIndex: Integer;

  public
    constructor Create (ax, ay: Integer; aparent: THControl=nil);
    destructor Destroy (); override;

    procedure appendItem (const atext: AnsiString; bv: PBoolean; aaction: TActionCB=nil);

    procedure drawControl (sx, sy: Integer); override;

    function mouseEvent (var ev: THMouseEvent): Boolean; override;
    function keyEvent (var ev: THKeyEvent): Boolean; override;
  end;

// ////////////////////////////////////////////////////////////////////////// //
var
  uiTopList: array of THControl = nil;


function uiMouseEvent (var ev: THMouseEvent): Boolean;
var
  f, c: Integer;
  lx, ly: Integer;
  ctmp: THControl;
begin
  if (Length(uiTopList) = 0) then result := false else result := uiTopList[High(uiTopList)].mouseEvent(ev);
  if not result and (ev.kind = ev.Press) then
  begin
    for f := High(uiTopList) downto 0 do
    begin
      lx := ev.x;
      ly := ev.y;
      if uiTopList[f].toLocal(lx, ly) then
      begin
        result := true;
        if uiTopList[f].mEnabled and (f <> High(uiTopList)) then
        begin
          uiTopList[High(uiTopList)].blurred();
          ctmp := uiTopList[f];
          ctmp.mGrab := nil;
          for c := f+1 to High(uiTopList) do uiTopList[c-1] := uiTopList[c];
          uiTopList[High(uiTopList)] := ctmp;
          ctmp.activated();
          result := ctmp.mouseEvent(ev);
        end;
        exit;
      end;
    end;
  end;
end;


function uiKeyEvent (var ev: THKeyEvent): Boolean;
begin
  if (Length(uiTopList) = 0) then result := false else result := uiTopList[High(uiTopList)].keyEvent(ev);
  if (ev.kind = ev.Release) then begin result := true; exit; end;
end;


procedure uiDraw ();
var
  f: Integer;
  ctl: THControl;
begin
  for f := 0 to High(uiTopList) do
  begin
    ctl := uiTopList[f];
    ctl.draw();
    if (f <> High(uiTopList)) then darkenRect(ctl.x0, ctl.y0, ctl.width, ctl.height, 128);
  end;
end;


procedure uiAddWindow (ctl: THControl);
var
  f, c: Integer;
begin
  if (ctl = nil) then exit;
  ctl := ctl.topLevel;
  for f := 0 to High(uiTopList) do
  begin
    if (uiTopList[f] = ctl) then
    begin
      if (f <> High(uiTopList)) then
      begin
        uiTopList[High(uiTopList)].blurred();
        for c := f+1 to High(uiTopList) do uiTopList[c-1] := uiTopList[c];
        uiTopList[High(uiTopList)] := ctl;
        ctl.activated();
      end;
      exit;
    end;
  end;
  if (Length(uiTopList) > 0) then uiTopList[High(uiTopList)].blurred();
  SetLength(uiTopList, Length(uiTopList)+1);
  uiTopList[High(uiTopList)] := ctl;
  ctl.activated();
end;


// won't free object
procedure uiRemoveWindow (ctl: THControl);
var
  f, c: Integer;
begin
  if (ctl = nil) then exit;
  ctl := ctl.topLevel;
  for f := 0 to High(uiTopList) do
  begin
    if (uiTopList[f] = ctl) then
    begin
      ctl.blurred();
      for c := f+1 to High(uiTopList) do uiTopList[c-1] := uiTopList[c];
      SetLength(uiTopList, Length(uiTopList)-1);
      if (ctl is THTopWindow) then
      begin
        if assigned(THTopWindow(ctl).closeCB) then THTopWindow(ctl).closeCB(ctl, 0);
      end;
      exit;
    end;
  end;
end;


function uiVisibleWindow (ctl: THControl): Boolean;
var
  f: Integer;
begin
  result := false;
  if (ctl = nil) then exit;
  ctl := ctl.topLevel;
  for f := 0 to High(uiTopList) do
  begin
    if (uiTopList[f] = ctl) then begin result := true; exit; end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor THControl.Create (ax, ay, aw, ah: Integer; aparent: THControl=nil);
begin
  mParent := aparent;
  mX := ax;
  mY := ay;
  mWidth := aw;
  mHeight := ah;
  mFrameWidth := 0;
  mFrameHeight := 0;
  mEnabled := true;
  mCanFocus := true;
  mChildren := nil;
  mFocused := nil;
  mGrab := nil;
  mEscClose := false;
  mEatKeys := false;
  scallowed := false;
  mDrawShadow := false;
  actionCB := nil;
end;


destructor THControl.Destroy ();
var
  f, c: Integer;
begin
  if (mParent <> nil) then
  begin
    setFocused(false);
    for f := 0 to High(mParent.mChildren) do
    begin
      if (mParent.mChildren[f] = self) then
      begin
        for c := f+1 to High(mParent.mChildren) do mParent.mChildren[c-1] := mParent.mChildren[c];
        SetLength(mParent.mChildren, Length(mParent.mChildren)-1);
      end;
    end;
  end;
  for f := 0 to High(mChildren) do
  begin
    mChildren[f].mParent := nil;
    mChildren[f].Free();
  end;
  mChildren := nil;
end;


procedure THControl.activated ();
begin
end;


procedure THControl.blurred ();
begin
  mGrab := nil;
end;


function THControl.topLevel (): THControl; inline;
begin
  result := self;
  while (result.mParent <> nil) do result := result.mParent;
end;


function THControl.getEnabled (): Boolean;
var
  ctl: THControl;
begin
  result := false;
  if (not mEnabled) or (mWidth < 1) or (mHeight < 1) then exit;
  ctl := mParent;
  while (ctl <> nil) do
  begin
    if (not ctl.mEnabled) or (ctl.mWidth < 1) or (ctl.mHeight < 1) then exit;
    ctl := ctl.mParent;
  end;
  result := true;
end;


procedure THControl.setEnabled (v: Boolean); inline;
begin
  if (mEnabled = v) then exit;
  mEnabled := v;
  if not v and focused then setFocused(false);
end;


function THControl.getFocused (): Boolean; inline;
begin
  if (mParent = nil) then result := (Length(uiTopList) > 0) and (uiTopList[High(uiTopList)] = self) else result := (topLevel.mFocused = self);
end;


procedure THControl.setFocused (v: Boolean); inline;
var
  tl: THControl;
begin
  tl := topLevel;
  if not v then
  begin
    if (tl.mFocused = self) then
    begin
      tl.blurred();
      tl.mFocused := tl.findNextFocus(self);
      if (tl.mFocused = self) then tl.mFocused := nil;
    end;
    exit;
  end;
  if (not mEnabled) or (not mCanFocus) then exit;
  if (tl.mFocused <> self) then
  begin
    tl.mFocused.blurred();
    tl.mFocused := self;
    if (tl.mGrab <> self) then tl.mGrab := nil;
    activated();
  end;
end;


function THControl.isMyChild (ctl: THControl): Boolean;
begin
  result := true;
  while (ctl <> nil) do
  begin
    if (ctl.mParent = self) then exit;
    ctl := ctl.mParent;
  end;
  result := false;
end;


// returns `true` if global coords are inside this control
function THControl.toLocal (var x, y: Integer): Boolean;
var
  ctl: THControl;
begin
  ctl := self;
  while (ctl <> nil) do
  begin
    Dec(x, ctl.mX);
    Dec(y, ctl.mY);
    ctl := ctl.mParent;
  end;
  result := (x >= 0) and (y >= 0) and (x < mWidth) and (y < mHeight);
end;


procedure THControl.toGlobal (var x, y: Integer);
var
  ctl: THControl;
begin
  ctl := self;
  while (ctl <> nil) do
  begin
    Inc(x, ctl.mX);
    Inc(y, ctl.mY);
    ctl := ctl.mParent;
  end;
end;


// x and y are global coords
function THControl.controlAtXY (x, y: Integer): THControl;
var
  lx, ly: Integer;
  f: Integer;
begin
  result := nil;
  if (not mEnabled) or (mWidth < 1) or (mHeight < 1) then exit;
  lx := x;
  ly := y;
  if not toLocal(lx, ly) then exit;
  for f := High(mChildren) downto 0 do
  begin
    result := mChildren[f].controlAtXY(x, y);
    if (result <> nil) then exit;
  end;
  result := self;
end;


function THControl.prevSibling (): THControl;
var
  f: Integer;
begin
  if (mParent <> nil) then
  begin
    for f := 1 to High(mParent.mChildren) do
    begin
      if (mParent.mChildren[f] = self) then begin result := mParent.mChildren[f-1]; exit; end;
    end;
  end;
  result := nil;
end;

function THControl.nextSibling (): THControl;
var
  f: Integer;
begin
  if (mParent <> nil) then
  begin
    for f := 0 to High(mParent.mChildren)-1 do
    begin
      if (mParent.mChildren[f] = self) then begin result := mParent.mChildren[f+1]; exit; end;
    end;
  end;
  result := nil;
end;

function THControl.firstChild (): THControl; inline;
begin
  if (Length(mChildren) <> 0) then result := mChildren[0] else result := nil;
end;

function THControl.lastChild (): THControl; inline;
begin
  if (Length(mChildren) <> 0) then result := mChildren[High(mChildren)] else result := nil;
end;


function THControl.findFirstFocus (): THControl;
var
  f: Integer;
begin
  result := nil;
  if enabled then
  begin
    for f := 0 to High(mChildren) do
    begin
      result := mChildren[f].findFirstFocus();
      if (result <> nil) then exit;
    end;
    if mCanFocus then result := self;
  end;
end;


function THControl.findLastFocus (): THControl;
var
  f: Integer;
begin
  result := nil;
  if enabled then
  begin
    for f := High(mChildren) downto 0 do
    begin
      result := mChildren[f].findLastFocus();
      if (result <> nil) then exit;
    end;
    if mCanFocus then result := self;
  end;
end;


function THControl.findNextFocus (cur: THControl): THControl;
begin
  result := nil;
  if enabled then
  begin
    if not isMyChild(cur) then cur := nil;
    if (cur = nil) then begin result := findFirstFocus(); exit; end;
    result := cur.findFirstFocus();
    if (result <> nil) and (result <> cur) then exit;
    while true do
    begin
      cur := cur.nextSibling;
      if (cur = nil) then break;
      result := cur.findFirstFocus();
      if (result <> nil) then exit;
    end;
    result := findFirstFocus();
  end;
end;


function THControl.findPrevFocus (cur: THControl): THControl;
begin
  result := nil;
  if enabled then
  begin
    if not isMyChild(cur) then cur := nil;
    if (cur = nil) then begin result := findLastFocus(); exit; end;
    //FIXME!
    result := cur.findLastFocus();
    if (result <> nil) and (result <> cur) then exit;
    while true do
    begin
      cur := cur.prevSibling;
      if (cur = nil) then break;
      result := cur.findLastFocus();
      if (result <> nil) then exit;
    end;
    result := findLastFocus();
  end;
end;


procedure THControl.appendChild (ctl: THControl);
begin
  if (ctl = nil) then exit;
  if (ctl.mParent <> nil) then exit;
  SetLength(mChildren, Length(mChildren)+1);
  mChildren[High(mChildren)] := ctl;
  ctl.mParent := self;
  Inc(ctl.mX, mFrameWidth);
  Inc(ctl.mY, mFrameHeight);
  if (ctl.mWidth > 0) and (ctl.mHeight > 0) and
     (ctl.mX+ctl.mWidth > mFrameWidth) and (ctl.mY+ctl.mHeight > mFrameHeight) then
  begin
    if (mWidth+mFrameWidth < ctl.mX+ctl.mWidth) then mWidth := ctl.mX+ctl.mWidth+mFrameWidth;
    if (mHeight+mFrameHeight < ctl.mY+ctl.mHeight) then mHeight := ctl.mY+ctl.mHeight+mFrameHeight;
  end;
  if (mFocused = nil) and ctl.mEnabled and ctl.mCanFocus and (ctl.mWidth > 0) and (ctl.mHeight > 0) then mFocused := ctl;
end;


//TODO: overflow checks
class function THControl.intersectRect (var x0, y0, w0, h0: Integer; const x1, y1, w1, h1: Integer): Boolean;
var
  ex0, ey0: Integer;
begin
  result := false;
  if (w0 < 1) or (h0 < 1) or (w1 < 1) or (h1 < 1) then exit;
  // check for intersection
  if (x0+w0 <= x1) or (y0+h0 <= y1) or (x1+w1 <= x0) or (y1+h1 <= y0) then exit;
  if (x0 >= x1+w1) or (y0 >= y1+h1) or (x1 >= x0+h0) or (y1 >= y0+h0) then exit;
  // ok, intersects
  ex0 := x0+w0;
  ey0 := y0+h0;
  if (x0 < x1) then x0 := x1;
  if (y0 < y1) then y0 := y1;
  if (ex0 > x1+w1) then ex0 := x1+w1;
  if (ey0 > y1+h1) then ey0 := y1+h1;
  w0 := ex0-x0;
  h0 := ey0-y0;
  result := (w0 > 0) and (h0 > 0);
end;


procedure THControl.setScissorGLInternal (x, y, w, h: Integer);
begin
  if not scallowed then exit;
  y := gWinSizeY-(y+h);
  if not intersectRect(x, y, w, h, scxywh[0], scxywh[1], scxywh[2], scxywh[3]) then glScissor(0, 0, 0, 0) else glScissor(x, y, w, h);
end;


procedure THControl.resetScissor ();
var
  x, y: Integer;
begin
  if not scallowed then exit;
  x := 0;
  y := 0;
  toGlobal(x, y);
  setScissorGLInternal(x, y, mWidth, mHeight);
end;


procedure THControl.setScissor (lx, ly, lw, lh: Integer);
var
  x, y: Integer;
begin
  if not scallowed then exit;
  if not intersectRect(lx, ly, lw, lh, 0, 0, mWidth, mHeight) then begin glScissor(0, 0, 0, 0); exit; end;
  x := lx;
  y := ly;
  toGlobal(x, y);
  setScissorGLInternal(x, y, lw, lh);
end;


procedure THControl.draw ();
var
  f: Integer;
  x, y: Integer;
  wassc: Boolean;
begin
  if (mWidth < 1) or (mHeight < 1) then exit;
  x := 0;
  y := 0;
  toGlobal(x, y);
  //conwritefln('[%s]: (%d,%d)-(%d,%d)  (%d,%d)', [ClassName, mX, mY, mWidth, mHeight, x, y]);

  scxywh[0] := 0;
  scxywh[1] := 0;
  scxywh[2] := 0;
  scxywh[3] := 0;

  wassc := (glIsEnabled(GL_SCISSOR_TEST) <> 0);
  if wassc then glGetIntegerv(GL_SCISSOR_BOX, @scxywh[0]) else glGetIntegerv(GL_VIEWPORT, @scxywh[0]);
  //conwritefln('(%d,%d)-(%d,%d)', [scxywh[0], scxywh[1], scxywh[2], scxywh[3]]);
  glEnable(GL_SCISSOR_TEST);
  scallowed := true;

  resetScissor();
  drawControl(x, y);
  if (mFrameWidth <> 0) or (mFrameHeight <> 0) then setScissor(mFrameWidth, mFrameHeight, mWidth-mFrameWidth*2, mHeight-mFrameHeight*2);
  for f := 0 to High(mChildren) do mChildren[f].draw();
  if (mFrameWidth <> 0) or (mFrameHeight <> 0) then resetScissor();
  drawControlPost(x, y);
  glScissor(scxywh[0], scxywh[1], scxywh[2], scxywh[3]);

  if wassc then glEnable(GL_SCISSOR_TEST) else glDisable(GL_SCISSOR_TEST);
  scallowed := false;
end;


procedure THControl.drawControl (sx, sy: Integer);
begin
  if (mParent = nil) then darkenRect(sx, sy, mWidth, mHeight, 64);
end;


procedure THControl.drawControlPost (sx, sy: Integer);
begin
  if mDrawShadow and (mWidth > 0) and (mHeight > 0) then
  begin
    setScissorGLInternal(sx+8, sy+8, mWidth, mHeight);
    darkenRect(sx+mWidth, sy+8, 8, mHeight, 128);
    darkenRect(sx+8, sy+mHeight, mWidth-8, 8, 128);
  end;
end;


function THControl.mouseEvent (var ev: THMouseEvent): Boolean;
var
  ctl: THControl;
begin
  result := false;
  if not mEnabled then exit;
  if (mParent = nil) then
  begin
    if (mGrab <> nil) then
    begin
      result := mGrab.mouseEvent(ev);
      if (ev.kind = ev.Release) then mGrab := nil;
      exit;
    end;
  end;
  if (mWidth < 1) or (mHeight < 1) then exit;
  ctl := controlAtXY(ev.x, ev.y);
  if (ctl <> nil) and (ctl <> self) then
  begin
    if (ctl <> topLevel.mFocused) then ctl.setFocused(true);
    result := ctl.mouseEvent(ev);
  end
  else if (ctl = self) and assigned(actionCB) then
  begin
    actionCB(self, 0);
  end;
end;


function THControl.keyEvent (var ev: THKeyEvent): Boolean;
var
  ctl: THControl;
begin
  result := false;
  if not mEnabled then exit;
  if (topLevel.mFocused <> self) and isMyChild(topLevel.mFocused) and topLevel.mFocused.mEnabled then result := topLevel.mFocused.keyEvent(ev);
  if (mParent = nil) then
  begin
    if (ev.kstate = THKeyEvent.ModShift) and (ev.scan = SDL_SCANCODE_TAB) then
    begin
      result := true;
      if (ev.kind = ev.Press) then
      begin
        ctl := findPrevFocus(mFocused);
        if (ctl <> mFocused) then
        begin
          mGrab := nil;
          mFocused := ctl;
        end;
      end;
      exit;
    end;
    if (ev.kstate = 0) and (ev.scan = SDL_SCANCODE_TAB) then
    begin
      result := true;
      if (ev.kind = ev.Press) then
      begin
        ctl := findNextFocus(mFocused);
        if (ctl <> mFocused) then
        begin
          mGrab := nil;
          mFocused := ctl;
        end;
      end;
      exit;
    end;
    if mEscClose and (ev.kind = ev.Press) and (ev.kstate = 0) and (ev.scan = SDL_SCANCODE_ESCAPE) then
    begin
      result := true;
      uiRemoveWindow(self);
      exit;
    end;
  end;
  if mEatKeys then result := true;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor THTopWindow.Create (const atitle: AnsiString; ax, ay: Integer; aw: Integer=-1; ah: Integer=-1);
begin
  inherited Create(ax, ay, aw, ah, nil);
  mFrameWidth := 8;
  mFrameHeight := 8;
  mTitle := atitle;
  if (mWidth < mFrameWidth*2+3*8) then mWidth := mFrameWidth*2+3*8;
  if (mHeight < mFrameHeight*2) then mHeight := mFrameHeight*2;
  if (Length(mTitle) > 0) then
  begin
    if (mWidth < Length(mTitle)*8+mFrameWidth*2+3*8) then mWidth := Length(mTitle)*8+mFrameWidth*2+3*8;
  end;
  mDragging := false;
  mDrawShadow := true;
  mWaitingClose := false;
  mInClose := false;
  closeCB := nil;
end;


procedure THTopWindow.drawControl (sx, sy: Integer);
begin
  fillRect(sx, sy, mWidth, mHeight, 0, 0, 128);
end;


procedure THTopWindow.drawControlPost (sx, sy: Integer);
const r = 255;
const g = 255;
const b = 255;
var
  tx: Integer;
begin
  if mDragging then
  begin
    drawRect(mX+4, mY+4, mWidth-8, mHeight-8, r, g, b);
  end
  else
  begin
    drawRect(mX+3, mY+3, mWidth-6, mHeight-6, r, g, b);
    drawRect(mX+5, mY+5, mWidth-10, mHeight-10, r, g, b);
    setScissor(mFrameWidth, 0, 3*8, 8);
    fillRect(mX+mFrameWidth, mY, 3*8, 8, 0, 0, 128);
    drawText8(mX+mFrameWidth, mY, '[ ]', r, g, b);
    if mInClose then drawText8(mX+mFrameWidth+7, mY, '#', 0, 255, 0)
    else drawText8(mX+mFrameWidth+7, mY, '*', 0, 255, 0);
  end;
  if (Length(mTitle) > 0) then
  begin
    setScissor(mFrameWidth+3*8, 0, mWidth-mFrameWidth*2-3*8, 8);
    tx := mX+(mWidth-Length(mTitle)*8) div 2;
    fillRect(tx-3, mY, Length(mTitle)*8+3+2, 8, 0, 0, 128);
    drawText8(tx, mY, mTitle, r, g, b);
  end;
  inherited drawControlPost(sx, sy);
end;


procedure THTopWindow.blurred ();
begin
  mDragging := false;
  mWaitingClose := false;
  mInClose := false;
  inherited;
end;


function THTopWindow.keyEvent (var ev: THKeyEvent): Boolean;
begin
  result := inherited keyEvent(ev);
  if not getFocused then exit;
  if (ev.kstate = ev.ModAlt) and (ev.kind = ev.Press) and (ev.scan = SDL_SCANCODE_F3) then
  begin
    uiRemoveWindow(self);
    result := true;
    exit;
  end;
end;


function THTopWindow.mouseEvent (var ev: THMouseEvent): Boolean;
var
  lx, ly: Integer;
begin
  result := false;
  if not mEnabled then exit;
  if (mWidth < 1) or (mHeight < 1) then exit;

  if mDragging then
  begin
    mX += ev.x-mDragStartX;
    mY += ev.y-mDragStartY;
    mDragStartX := ev.x;
    mDragStartY := ev.y;
    if (ev.kind = ev.Release) then mDragging := false;
    result := true;
    exit;
  end;

  lx := ev.x;
  ly := ev.y;
  if toLocal(lx, ly) then
  begin
    if (ev.kind = ev.Press) then
    begin
      if (ly < 8) then
      begin
        if (lx >= mFrameWidth) and (lx < mFrameWidth+3*8) then
        begin
          //uiRemoveWindow(self);
          mWaitingClose := true;
          mInClose := true;
        end
        else
        begin
          mDragging := true;
          mDragStartX := ev.x;
          mDragStartY := ev.y;
        end;
        result := true;
        exit;
      end;
      if (lx < mFrameWidth) or (lx >= mWidth-mFrameWidth) or (ly >= mHeight-mFrameHeight) then
      begin
        mDragging := true;
        mDragStartX := ev.x;
        mDragStartY := ev.y;
        result := true;
        exit;
      end;
    end;

    if (ev.kind = ev.Release) then
    begin
      if mWaitingClose and (lx >= mFrameWidth) and (lx < mFrameWidth+3*8) then
      begin
        uiRemoveWindow(self);
        result := true;
        exit;
      end;
      mWaitingClose := false;
      mInClose := false;
    end;

    if (ev.kind = ev.Motion) then
    begin
      if mWaitingClose then
      begin
        mInClose := (lx >= mFrameWidth) and (lx < mFrameWidth+3*8);
        result := true;
        exit;
      end;
    end;
  end
  else
  begin
    mInClose := false;
    if (ev.kind <> ev.Motion) then mWaitingClose := false;
  end;

  result := inherited mouseEvent(ev);
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor THCtlCBListBox.Create (ax, ay: Integer; aparent: THControl=nil);
begin
  mItems := nil;
  mCurIndex := -1;
  inherited Create(ax, ay, 4, 4);
end;


destructor THCtlCBListBox.Destroy ();
begin
  mItems := nil;
  inherited;
end;


procedure THCtlCBListBox.appendItem (const atext: AnsiString; bv: PBoolean; aaction: TActionCB=nil);
var
  it: PItem;
begin
  if (Length(atext)*8+3*8+2 > mWidth) then mWidth := Length(atext)*8+3*8+2;
  SetLength(mItems, Length(mItems)+1);
  it := @mItems[High(mItems)];
  it.title := atext;
  it.varp := bv;
  it.actionCB := aaction;
  if (Length(mItems)*8 > mHeight) then mHeight := Length(mItems)*8;
  if (mCurIndex < 0) then mCurIndex := 0;
end;


procedure THCtlCBListBox.drawControl (sx, sy: Integer);
var
  f, tx: Integer;
  it: PItem;
begin
  for f := 0 to High(mItems) do
  begin
    it := @mItems[f];
    if (mCurIndex = f) then fillRect(sx, sy, mWidth, 8, 0, 128, 0);
    if (it.varp <> nil) then
    begin
      if it.varp^ then drawText8(sx, sy, '[x]', 255, 255, 255) else drawText8(sx, sy, '[ ]', 255, 255, 255);
      drawText8(sx+3*8+2, sy, it.title, 255, 255, 0);
    end
    else if (Length(it.title) > 0) then
    begin
      tx := sx+(mWidth-Length(it.title)*8) div 2;
      if (tx-3 > sx+4) then
      begin
        drawLine(sx+4, sy+3, tx-3, sy+3, 255, 255, 255);
        drawLine(tx+Length(it.title)*8, sy+3, sx+mWidth-4, sy+3, 255, 255, 255);
      end;
      drawText8(sx+(mWidth-Length(it.title)*8) div 2, sy, it.title, 255, 255, 255);
    end
    else
    begin
      drawLine(sx+4, sy+3, sx+mWidth-8, sy+3, 255, 255, 255);
    end;
    Inc(sy, 8);
  end;
end;


function THCtlCBListBox.mouseEvent (var ev: THMouseEvent): Boolean;
var
  lx, ly: Integer;
  it: PItem;
begin
  result := inherited mouseEvent(ev);
  lx := ev.x;
  ly := ev.y;
  if not result and toLocal(lx, ly) then
  begin
    result := true;
    if (ev.kind = ev.Press) then
    begin
      ly := ly div 8;
      if (ly >= 0) and (ly < Length(mItems)) then
      begin
        it := @mItems[ly];
        if (it.varp <> nil) then
        begin
          mCurIndex := ly;
          it.varp^ := not it.varp^;
          if assigned(it.actionCB) then it.actionCB(self, Integer(it.varp^));
          if assigned(actionCB) then actionCB(self, ly);
        end;
      end;
    end;
  end;
end;


function THCtlCBListBox.keyEvent (var ev: THKeyEvent): Boolean;
var
  it: PItem;
begin
  result := inherited keyEvent(ev);
  if not getFocused then exit;
  //result := true;
  if (ev.kstate = 0) and (ev.kind = ev.Press) then
  begin
    case ev.scan of
      SDL_SCANCODE_HOME,
      SDL_SCANCODE_PAGEUP:
        begin
          result := true;
          mCurIndex := 0;
        end;
      SDL_SCANCODE_END,
      SDL_SCANCODE_PAGEDOWN:
        begin
          result := true;
          mCurIndex := High(mItems);
        end;
      SDL_SCANCODE_UP:
        begin
          result := true;
          if (Length(mItems) > 0) then
          begin
            if (mCurIndex < 0) then mCurIndex := Length(mItems);
            while (mCurIndex > 0) do
            begin
              Dec(mCurIndex);
              if (mItems[mCurIndex].varp <> nil) then break;
            end;
          end
          else
          begin
            mCurIndex := -1;
          end;
        end;
      SDL_SCANCODE_DOWN:
        begin
          result := true;
          if (Length(mItems) > 0) then
          begin
            if (mCurIndex < 0) then mCurIndex := -1;
            while (mCurIndex < High(mItems)) do
            begin
              Inc(mCurIndex);
              if (mItems[mCurIndex].varp <> nil) then break;
            end;
          end
          else
          begin
            mCurIndex := -1;
          end;
        end;
      SDL_SCANCODE_SPACE,
      SDL_SCANCODE_RETURN:
        begin
          result := true;
          if (mCurIndex >= 0) and (mCurIndex < Length(mItems)) and (mItems[mCurIndex].varp <> nil) then
          begin
            it := @mItems[mCurIndex];
            it.varp^ := not it.varp^;
            if assigned(it.actionCB) then it.actionCB(self, Integer(it.varp^));
            if assigned(actionCB) then actionCB(self, mCurIndex);
          end;
        end;
    end;
  end;
end;
