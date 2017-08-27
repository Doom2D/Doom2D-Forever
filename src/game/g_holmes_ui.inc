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
  private
    mParent: THControl;
    mX, mY: Integer;
    mWidth, mHeight: Integer;
    mEnabled: Boolean;
    mCanFocus: Boolean;
    mChildren: array of THControl;
    mFocused: THControl; // valid only for top-level controls
    mGrab: THControl; // valid only for top-level controls
    mEscClose: Boolean; // valid only for top-level controls
    mEatKeys: Boolean;

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

  protected
    procedure blurred (); override;

  public
    constructor Create (const atitle: AnsiString; ax, ay: Integer; aw: Integer=-1; ah: Integer=-1);

    procedure appendChild (ctl: THControl);

    // `sx` and `sy` are screen coordinates
    procedure drawControl (sx, sy: Integer); override;
    procedure drawControlPost (sx, sy: Integer); override;

    function mouseEvent (var ev: THMouseEvent): Boolean; override; // returns `true` if event was eaten
  end;


  THCtlCBListBox = class(THControl)
  private
    mItems: array of AnsiString;
    mChecks: array of PBoolean;
    mCurIndex: Integer;

  public
    constructor Create (ax, ay: Integer; aparent: THControl=nil);
    destructor Destroy (); override;

    procedure appendItem (const atext: AnsiString; bv: PBoolean);

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
begin
  for f := 0 to High(uiTopList) do uiTopList[f].draw();
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
  mEnabled := true;
  mCanFocus := true;
  mChildren := nil;
  mFocused := nil;
  mGrab := nil;
  mEscClose := false;
  mEatKeys := false;
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


procedure THControl.draw ();
var
  f: Integer;
  x, y: Integer;
  scxywh: array[0..3] of GLint;
  wassc: Boolean;

  procedure setScissor (x, y, w, h: Integer);
  var
    x1, y1: Integer;
    sx0, sy0: Integer;
    sx1, sy1: Integer;
  begin
    if (w < 1) or (h < 1) or (scxywh[2] < 1) or (scxywh[3] < 1) then begin glScissor(0, 0, 0, 0); exit; end;
    x1 := x+w-1;
    y1 := y+h-1;
    sx0 := scxywh[0];
    sy0 := scxywh[1];
    sx1 := sx0+scxywh[2]-1;
    sy1 := sy0+scxywh[3]-1;
    //conwritefln('0: (%d,%d)-(%d,%d)  (%d,%d)-(%d,%d)', [sx0, sy0, sx1, sy1, x, y, x1, y1]);
    if (x1 < sx0) or (y1 < sy0) or (x > sx1) or (y > sy1) then begin glScissor(0, 0, 0, 0); exit; end;
    if (x < sx0) then x := sx0;
    if (y < sy0) then y := sy0;
    if (x1 > sx1) then x1 := sx1;
    if (y1 > sy1) then y1 := sy1;
    //conwritefln('1: (%d,%d)-(%d,%d)  (%d,%d)-(%d,%d)', [sx0, sy0, sx1, sy1, x, y, x1, y1]);
    glScissor(x, y, x1-x+1, y1-y+1);
  end;

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
  if wassc then
  begin
    glGetIntegerv(GL_SCISSOR_BOX, @scxywh[0]);
  end
  else
  begin
    glGetIntegerv(GL_VIEWPORT, @scxywh[0]);
  end;
  //conwritefln('(%d,%d)-(%d,%d)', [scxywh[0], scxywh[1], scxywh[2], scxywh[3]]);

  glEnable(GL_SCISSOR_TEST);
  setScissor(x, gWinSizeY-(y+mHeight-1)-1, mWidth, mHeight);

  drawControl(x, y);
  if (mParent = nil) then setScissor(x+2, gWinSizeY-(y+mHeight-1-2)-1, mWidth-4, mHeight-14);
  for f := 0 to High(mChildren) do mChildren[f].draw();
  if (mParent = nil) then setScissor(x, gWinSizeY-(y+mHeight-1)-1, mWidth, mHeight);
  drawControlPost(x, y);
  glScissor(scxywh[0], scxywh[1], scxywh[2], scxywh[3]);
  if wassc then glEnable(GL_SCISSOR_TEST) else glDisable(GL_SCISSOR_TEST);
end;


procedure THControl.drawControl (sx, sy: Integer);
begin
  if (mParent = nil) then darkenRect(sx, sy, mWidth, mHeight, 64);
  //fillRect(sx, sy, mWidth, mHeight, 0, 0, 255, 120);
end;


procedure THControl.drawControlPost (sx, sy: Integer);
begin
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
  mTitle := atitle;
  if (Length(mTitle) > 0) then
  begin
    if (ah < 14) then ah := 14;
    if (aw < Length(mTitle)*8+4) then aw := Length(mTitle)*8+4;
  end
  else
  begin
    if (ah < 4) then ah := 4;
    if (aw < 4) then aw := 4;
  end;
  mDragging := false;
  inherited Create(ax, ay, aw, ah, nil);
end;


procedure THTopWindow.appendChild (ctl: THControl);
var
  myofs: Integer;
begin
  if (ctl = nil) then exit;
  if (ctl.mParent <> nil) then exit;
  if (Length(mTitle) > 0) then myofs := 12 else myofs := 2;
  SetLength(mChildren, Length(mChildren)+1);
  mChildren[High(mChildren)] := ctl;
  ctl.mParent := self;
  Inc(ctl.mX, 2);
  Inc(ctl.mY, myofs);
  if (ctl.mWidth > 0) and (ctl.mHeight > 0) and
     (ctl.mX+ctl.mWidth > 2) and (ctl.mY+ctl.mHeight > myofs-2) then
  begin
    if (mWidth+2 < ctl.mX+ctl.mWidth) then mWidth := ctl.mX+ctl.mWidth+2;
    if (mHeight+2 < ctl.mY+ctl.mHeight) then mHeight := ctl.mY+ctl.mHeight+2;
  end;
  if (mFocused = nil) and ctl.mEnabled and ctl.mCanFocus and (ctl.mWidth > 0) and (ctl.mHeight > 0) then mFocused := ctl;
end;


procedure THTopWindow.drawControl (sx, sy: Integer);
begin
  //if (mParent = nil) then darkenRect(sx, sy, mWidth, mHeight, 200);
  fillRect(sx, sy, mWidth, mHeight, 0, 0, 128);
end;


procedure THTopWindow.drawControlPost (sx, sy: Integer);
var
  r, g, b: Integer;
begin
  if getFocused then
  begin
    r := 255;
    g := 255;
    b := 255;
  end
  else
  begin
    r := 127;
    g := 127;
    b := 127;
  end;
  drawRect(mX, mY, mWidth, mHeight, r, g, b);
  if (Length(mTitle) > 0) then
  begin
    fillRect(mX+1, mY+1, mWidth-2, 9, r, g, b);
    drawText8(mX+2, mY+1, mTitle, 0, 0, 0);
  end;
end;


procedure THTopWindow.blurred ();
begin
  mDragging := false;
  inherited;
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

  if (ev.kind = ev.Press) and (ev.but = ev.Left) then
  begin
    lx := ev.x;
    ly := ev.y;
    if toLocal(lx, ly) then
    begin
      if ((Length(mTitle) > 0) and (ly < 12)) or ((Length(mTitle) = 0) and (ly < 2)) then
      begin
        mDragging := true;
        mDragStartX := ev.x;
        mDragStartY := ev.y;
        result := true;
        exit;
      end;
    end;
  end;

  result := inherited mouseEvent(ev);
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor THCtlCBListBox.Create (ax, ay: Integer; aparent: THControl=nil);
begin
  mItems := nil;
  mChecks := nil;
  mCurIndex := -1;
  inherited Create(ax, ay, 4, 4);
end;


destructor THCtlCBListBox.Destroy ();
begin
  mItems := nil;
  mChecks := nil;
  inherited;
end;


procedure THCtlCBListBox.appendItem (const atext: AnsiString; bv: PBoolean);
begin
  if (Length(atext)*8+4+10 > mWidth) then mWidth := Length(atext)*8+4+10;
  SetLength(mItems, Length(mItems)+1);
  mItems[High(mItems)] := atext;
  SetLength(mChecks, Length(mChecks)+1);
  mChecks[High(mChecks)] := bv;
  if (Length(mItems)*8+4 > mHeight) then mHeight := Length(mItems)*8+4;
end;


procedure THCtlCBListBox.drawControl (sx, sy: Integer);
var
  f: Integer;
begin
  //fillRect(sx, sy, mWidth, mHeight, 0, 128, 0);
  Inc(sx, 2);
  Inc(sy, 2);
  for f := 0 to High(mItems) do
  begin
    if (mCurIndex = f) then fillRect(sx-2, sy, mWidth, 8, 0, 128, 0);
    if (mChecks[f] <> nil) and (mChecks[f]^) then drawText8(sx, sy, '*', 255, 255, 255);
    drawText8(sx+10, sy, mItems[f], 255, 255, 0);
    Inc(sy, 8);
  end;
end;


function THCtlCBListBox.mouseEvent (var ev: THMouseEvent): Boolean;
var
  lx, ly: Integer;
begin
  result := inherited mouseEvent(ev);
  if not result and (Length(mItems) > 0) and (ev.kind = ev.Press) then
  begin
    lx := ev.x;
    ly := ev.y;
    if toLocal(lx, ly) then
    begin
      if (ly < 2) then ly := 2;
      ly := ly div 8;
      if (ly < 0) then ly := 0 else if (ly > High(mItems)) then ly := High(mItems);
      mCurIndex := ly;
      if (mChecks[ly] <> nil) then mChecks[ly]^ := not mChecks[ly]^;
    end;
  end;
end;


function THCtlCBListBox.keyEvent (var ev: THKeyEvent): Boolean;
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
              if (mChecks[mCurIndex] <> nil) then break;
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
              if (mChecks[mCurIndex] <> nil) then break;
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
          if (mCurIndex >= 0) and (mCurIndex < Length(mChecks)) and (mChecks[mCurIndex] <> nil) then mChecks[mCurIndex]^ := not mChecks[mCurIndex]^;
        end;
    end;
  end;
end;
