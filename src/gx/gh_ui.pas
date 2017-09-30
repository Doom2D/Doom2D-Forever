(* coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
 * Understanding is not required. Only obedience.
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../shared/a_modes.inc}
{$M+}
unit gh_ui;

interface

uses
  SysUtils, Classes,
  SDL2,
  gh_ui_common,
  gh_ui_style,
  sdlcarcass, glgfx,
  xparser;


// ////////////////////////////////////////////////////////////////////////// //
type
  TUIControlClass = class of TUIControl;

  TUIControl = class
  public
    type TActionCB = procedure (me: TUIControl);
    type TCloseRequestCB = function (me: TUIControl): Boolean; // top-level windows will call this before closing with icon/keyboard

    // return `true` to stop
    type TCtlEnumCB = function (ctl: TUIControl): Boolean is nested;

  public
    const ClrIdxActive = 0;
    const ClrIdxDisabled = 1;
    const ClrIdxInactive = 2;
    const ClrIdxMax = 2;

  private
    mParent: TUIControl;
    mId: AnsiString;
    mStyleId: AnsiString;
    mX, mY: Integer;
    mWidth, mHeight: Integer;
    mFrameWidth, mFrameHeight: Integer;
    mScrollX, mScrollY: Integer;
    mEnabled: Boolean;
    mCanFocus: Boolean;
    mChildren: array of TUIControl;
    mFocused: TUIControl; // valid only for top-level controls
    mEscClose: Boolean; // valid only for top-level controls
    mDrawShadow: Boolean;
    mCancel: Boolean;
    mDefault: Boolean;
    // colors
    mCtl4Style: AnsiString;
    mBackColor: array[0..ClrIdxMax] of TGxRGBA;
    mTextColor: array[0..ClrIdxMax] of TGxRGBA;
    mFrameColor: array[0..ClrIdxMax] of TGxRGBA;
    mFrameTextColor: array[0..ClrIdxMax] of TGxRGBA;
    mFrameIconColor: array[0..ClrIdxMax] of TGxRGBA;
    mDarken: array[0..ClrIdxMax] of Integer; // -1: none

  private
    scis: TScissorSave;
    scallowed: Boolean;

  protected
    procedure updateStyle (); virtual;
    procedure cacheStyle (root: TUIStyle); virtual;
    function getColorIndex (): Integer; inline;

  protected
    function getEnabled (): Boolean;
    procedure setEnabled (v: Boolean); inline;

    function getFocused (): Boolean; inline;
    procedure setFocused (v: Boolean); inline;

    function getActive (): Boolean; inline;

    function getCanFocus (): Boolean; inline;

    function isMyChild (ctl: TUIControl): Boolean;

    function findFirstFocus (): TUIControl;
    function findLastFocus (): TUIControl;

    function findNextFocus (cur: TUIControl; wrap: Boolean): TUIControl;
    function findPrevFocus (cur: TUIControl; wrap: Boolean): TUIControl;

    function findCancelControl (): TUIControl;
    function findDefaulControl (): TUIControl;

    function findControlById (const aid: AnsiString): TUIControl;

    procedure activated (); virtual;
    procedure blurred (); virtual;

    procedure calcFullClientSize ();

    //WARNING! do not call scissor functions outside `.draw*()` API!
    // set scissor to this rect (in local coords)
    procedure setScissor (lx, ly, lw, lh: Integer);
    // reset scissor to whole control
    procedure resetScissor (fullArea: Boolean); inline; // "full area" means "with frame"

    // DO NOT USE!
    // set scissor to this rect (in global coords)
    procedure setScissorGLInternal (x, y, w, h: Integer);

  public
    actionCB: TActionCB;
    closeRequestCB: TCloseRequestCB;

  private
    mDefSize: TLaySize; // default size
    mMaxSize: TLaySize; // maximum size
    mFlex: Integer;
    mHoriz: Boolean;
    mCanWrap: Boolean;
    mLineStart: Boolean;
    mHGroup: AnsiString;
    mVGroup: AnsiString;
    mAlign: Integer;
    mExpand: Boolean;
    mLayDefSize: TLaySize;
    mLayMaxSize: TLaySize;
    mFullSize: TLaySize;

  public
    // layouter interface
    function getDefSize (): TLaySize; inline; // default size; <0: use max size
    //procedure setDefSize (const sz: TLaySize); inline; // default size; <0: use max size
    function getMargins (): TLayMargins; inline;
    function getMaxSize (): TLaySize; inline; // max size; <0: set to some huge value
    //procedure setMaxSize (const sz: TLaySize); inline; // max size; <0: set to some huge value
    function getFlex (): Integer; inline; // <=0: not flexible
    function isHorizBox (): Boolean; inline; // horizontal layout for children?
    procedure setHorizBox (v: Boolean); inline; // horizontal layout for children?
    function canWrap (): Boolean; inline; // for horizontal boxes: can wrap children? for child: `false` means 'nonbreakable at *next* ctl'
    procedure setCanWrap (v: Boolean); inline; // for horizontal boxes: can wrap children? for child: `false` means 'nonbreakable at *next* ctl'
    function isLineStart (): Boolean; inline; // `true` if this ctl should start a new line; ignored for vertical boxes
    procedure setLineStart (v: Boolean); inline; // `true` if this ctl should start a new line; ignored for vertical boxes
    function getAlign (): Integer; inline; // aligning in non-main direction: <0: left/up; 0: center; >0: right/down
    procedure setAlign (v: Integer); inline; // aligning in non-main direction: <0: left/up; 0: center; >0: right/down
    function getExpand (): Boolean; inline; // expanding in non-main direction: `true` will ignore align and eat all available space
    procedure setExpand (v: Boolean); inline; // expanding in non-main direction: `true` will ignore align and eat all available space
    function getHGroup (): AnsiString; inline; // empty: not grouped
    procedure setHGroup (const v: AnsiString); inline; // empty: not grouped
    function getVGroup (): AnsiString; inline; // empty: not grouped
    procedure setVGroup (const v: AnsiString); inline; // empty: not grouped

    procedure setActualSizePos (constref apos: TLayPos; constref asize: TLaySize); inline;

    procedure layPrepare (); virtual; // called before registering control in layouter

  public
    property flex: Integer read mFlex write mFlex;
    property flDefaultSize: TLaySize read mDefSize write mDefSize;
    property flMaxSize: TLaySize read mMaxSize write mMaxSize;
    property flHoriz: Boolean read isHorizBox write setHorizBox;
    property flCanWrap: Boolean read canWrap write setCanWrap;
    property flLineStart: Boolean read isLineStart write setLineStart;
    property flAlign: Integer read getAlign write setAlign;
    property flExpand: Boolean read getExpand write setExpand;
    property flHGroup: AnsiString read getHGroup write setHGroup;
    property flVGroup: AnsiString read getVGroup write setVGroup;
    property fullSize: TLaySize read mFullSize;

  protected
    function parsePos (par: TTextParser): TLayPos;
    function parseSize (par: TTextParser): TLaySize;
    function parseBool (par: TTextParser): Boolean;
    function parseAnyAlign (par: TTextParser): Integer;
    function parseHAlign (par: TTextParser): Integer;
    function parseVAlign (par: TTextParser): Integer;
    function parseOrientation (const prname: AnsiString; par: TTextParser): Boolean;
    procedure parseTextAlign (par: TTextParser; var h, v: Integer);
    procedure parseChildren (par: TTextParser); // par should be on '{'; final '}' is eaten

  public
    // par is on property data
    // there may be more data in text stream, don't eat it!
    // return `true` if property name is valid and value was parsed
    // return `false` if property name is invalid; don't advance parser in this case
    // throw on property data errors
    function parseProperty (const prname: AnsiString; par: TTextParser): Boolean; virtual;

    // par should be on '{'; final '}' is eaten
    procedure parseProperties (par: TTextParser);

  public
    constructor Create ();
    destructor Destroy (); override;

    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser

    // `sx` and `sy` are screen coordinates
    procedure drawControl (gx, gy: Integer); virtual;

    // called after all children drawn
    procedure drawControlPost (gx, gy: Integer); virtual;

    procedure draw (); virtual;

    function topLevel (): TUIControl; inline;

    // returns `true` if global coords are inside this control
    function toLocal (var x, y: Integer): Boolean;
    function toLocal (gx, gy: Integer; out x, y: Integer): Boolean; inline;
    procedure toGlobal (var x, y: Integer);
    procedure toGlobal (lx, ly: Integer; out x, y: Integer); inline;

    procedure getDrawRect (out gx, gy, wdt, hgt: Integer);

    // x and y are global coords
    function controlAtXY (x, y: Integer; allowDisabled: Boolean=false): TUIControl;

    function parentScrollX (): Integer; inline;
    function parentScrollY (): Integer; inline;

    procedure makeVisibleInParent ();

    procedure doAction (); virtual; // so user controls can override it

    procedure mouseEvent (var ev: THMouseEvent); virtual; // returns `true` if event was eaten
    procedure keyEvent (var ev: THKeyEvent); virtual; // returns `true` if event was eaten
    procedure keyEventPre (var ev: THKeyEvent); virtual; // will be called before dispatching the event
    procedure keyEventPost (var ev: THKeyEvent); virtual; // will be called after if nobody processed the event

    function prevSibling (): TUIControl;
    function nextSibling (): TUIControl;
    function firstChild (): TUIControl; inline;
    function lastChild (): TUIControl; inline;

    procedure appendChild (ctl: TUIControl); virtual;

    function setActionCBFor (const aid: AnsiString; cb: TActionCB): TActionCB; // returns previous cb

    function forEachChildren (cb: TCtlEnumCB): TUIControl; // doesn't recurse
    function forEachControl (cb: TCtlEnumCB; includeSelf: Boolean=true): TUIControl;

    procedure close (); // this closes *top-level* control

  public
    property id: AnsiString read mId;
    property styleId: AnsiString read mStyleId;
    property scrollX: Integer read mScrollX write mScrollX;
    property scrollY: Integer read mScrollY write mScrollY;
    property x0: Integer read mX write mX;
    property y0: Integer read mY write mY;
    property width: Integer read mWidth write mWidth;
    property height: Integer read mHeight write mHeight;
    property enabled: Boolean read getEnabled write setEnabled;
    property parent: TUIControl read mParent;
    property focused: Boolean read getFocused write setFocused;
    property active: Boolean read getActive;
    property escClose: Boolean read mEscClose write mEscClose;
    property cancel: Boolean read mCancel write mCancel;
    property defctl: Boolean read mDefault write mDefault;
    property canFocus: Boolean read getCanFocus write mCanFocus;
    property ctlById[const aid: AnsiString]: TUIControl read findControlById; default;
  end;


  TUITopWindow = class(TUIControl)
  private
    type TXMode = (None, Drag, Scroll);

  private
    mTitle: AnsiString;
    mDragScroll: TXMode;
    mDragStartX, mDragStartY: Integer;
    mWaitingClose: Boolean;
    mInClose: Boolean;
    mFreeOnClose: Boolean; // default: false
    mDoCenter: Boolean; // after layouting
    mFitToScreen: Boolean;

  protected
    procedure activated (); override;
    procedure blurred (); override;

  public
    closeCB: TActionCB; // called after window was removed from ui window list

  public
    constructor Create (const atitle: AnsiString);

    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser

    function parseProperty (const prname: AnsiString; par: TTextParser): Boolean; override;

    procedure flFitToScreen (); // call this before layouting

    procedure centerInScreen ();

    // `sx` and `sy` are screen coordinates
    procedure drawControl (gx, gy: Integer); override;
    procedure drawControlPost (gx, gy: Integer); override;

    procedure keyEvent (var ev: THKeyEvent); override; // returns `true` if event was eaten
    procedure mouseEvent (var ev: THMouseEvent); override; // returns `true` if event was eaten

  public
    property freeOnClose: Boolean read mFreeOnClose write mFreeOnClose;
    property fitToScreen: Boolean read mFitToScreen write mFitToScreen;
  end;

  // ////////////////////////////////////////////////////////////////////// //
  TUIBox = class(TUIControl)
  private
    mHasFrame: Boolean;
    mCaption: AnsiString;
    mHAlign: Integer; // -1: left; 0: center; 1: right; default: left

  protected
    procedure setCaption (const acap: AnsiString);
    procedure setHasFrame (v: Boolean);

  public
    constructor Create (ahoriz: Boolean);

    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser

    function parseProperty (const prname: AnsiString; par: TTextParser): Boolean; override;

    procedure drawControl (gx, gy: Integer); override;

    procedure mouseEvent (var ev: THMouseEvent); override;
    procedure keyEvent (var ev: THKeyEvent); override;

  public
    property caption: AnsiString read mCaption write setCaption;
    property hasFrame: Boolean read mHasFrame write setHasFrame;
    property captionAlign: Integer read mHAlign write mHAlign;
  end;

  TUIHBox = class(TUIBox)
  public
    constructor Create ();

    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser
  end;

  TUIVBox = class(TUIBox)
  public
    constructor Create ();

    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser
  end;

  // ////////////////////////////////////////////////////////////////////// //
  TUISpan = class(TUIControl)
  public
    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser

    function parseProperty (const prname: AnsiString; par: TTextParser): Boolean; override;

    procedure drawControl (gx, gy: Integer); override;
  end;

  // ////////////////////////////////////////////////////////////////////// //
  TUILine = class(TUIControl)
  public
    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser

    function parseProperty (const prname: AnsiString; par: TTextParser): Boolean; override;

    procedure drawControl (gx, gy: Integer); override;
  end;

  TUIHLine = class(TUILine)
  public
    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser
  end;

  TUIVLine = class(TUILine)
  public
    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser
  end;

  // ////////////////////////////////////////////////////////////////////// //
  TUIStaticText = class(TUIControl)
  private
    mText: AnsiString;
    mHAlign: Integer; // -1: left; 0: center; 1: right; default: left
    mVAlign: Integer; // -1: top; 0: center; 1: bottom; default: center
    mHeader: Boolean; // true: draw with frame text color
    mLine: Boolean; // true: draw horizontal line

  private
    procedure setText (const atext: AnsiString);

  public
    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser

    function parseProperty (const prname: AnsiString; par: TTextParser): Boolean; override;

    procedure drawControl (gx, gy: Integer); override;

  public
    property text: AnsiString read mText write setText;
    property halign: Integer read mHAlign write mHAlign;
    property valign: Integer read mVAlign write mVAlign;
    property header: Boolean read mHeader write mHeader;
    property line: Boolean read mLine write mLine;
  end;

  // ////////////////////////////////////////////////////////////////////// //
  TUITextLabel = class(TUIControl)
  private
    mText: AnsiString;
    mHAlign: Integer; // -1: left; 0: center; 1: right; default: left
    mVAlign: Integer; // -1: top; 0: center; 1: bottom; default: center
    mHotChar: AnsiChar;
    mHotOfs: Integer; // from text start, in pixels
    mHotColor: array[0..ClrIdxMax] of TGxRGBA;
    mLinkId: AnsiString; // linked control

  protected
    procedure cacheStyle (root: TUIStyle); override;

    procedure setText (const s: AnsiString); virtual;

  public
    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser

    function parseProperty (const prname: AnsiString; par: TTextParser): Boolean; override;

    procedure doAction (); override;

    procedure drawControl (gx, gy: Integer); override;

    procedure mouseEvent (var ev: THMouseEvent); override;
    procedure keyEventPost (var ev: THKeyEvent); override;

  public
    property text: AnsiString read mText write setText;
    property halign: Integer read mHAlign write mHAlign;
    property valign: Integer read mVAlign write mVAlign;
  end;

  // ////////////////////////////////////////////////////////////////////// //
  TUIButton = class(TUITextLabel)
  protected
    procedure setText (const s: AnsiString); override;

  public
    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser

    procedure drawControl (gx, gy: Integer); override;

    procedure mouseEvent (var ev: THMouseEvent); override;
    procedure keyEvent (var ev: THKeyEvent); override;
  end;

  // ////////////////////////////////////////////////////////////////////// //
  TUISwitchBox = class(TUITextLabel)
  protected
    mBoolVar: PBoolean;
    mChecked: Boolean;
    mCheckedStr: AnsiString;
    mUncheckedStr: AnsiString;
    mSwitchColor: array[0..ClrIdxMax] of TGxRGBA;

  protected
    procedure cacheStyle (root: TUIStyle); override;

    procedure setText (const s: AnsiString); override;

    function getChecked (): Boolean; virtual;
    procedure setChecked (v: Boolean); virtual; abstract;

  public
    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser

    function parseProperty (const prname: AnsiString; par: TTextParser): Boolean; override;

    procedure drawControl (gx, gy: Integer); override;

    procedure mouseEvent (var ev: THMouseEvent); override;
    procedure keyEvent (var ev: THKeyEvent); override;

    procedure setVar (pvar: PBoolean);

  public
    property checked: Boolean read getChecked write setChecked;
  end;

  TUICheckBox = class(TUISwitchBox)
  protected
    procedure setChecked (v: Boolean); override;

  public
    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser

    procedure doAction (); override;
  end;

  TUIRadioBox = class(TUISwitchBox)
  private
    mRadioGroup: AnsiString;

  protected
    procedure setChecked (v: Boolean); override;

  public
    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser

    function parseProperty (const prname: AnsiString; par: TTextParser): Boolean; override;

    procedure doAction (); override;

  public
    property radioGroup: AnsiString read mRadioGroup write mRadioGroup; //FIXME
  end;


// ////////////////////////////////////////////////////////////////////////// //
procedure uiMouseEvent (var evt: THMouseEvent);
procedure uiKeyEvent (var evt: THKeyEvent);
procedure uiDraw ();


// ////////////////////////////////////////////////////////////////////////// //
procedure uiAddWindow (ctl: TUIControl);
procedure uiRemoveWindow (ctl: TUIControl); // will free window if `mFreeOnClose` is `true`
function uiVisibleWindow (ctl: TUIControl): Boolean;

procedure uiUpdateStyles ();


// ////////////////////////////////////////////////////////////////////////// //
// do layouting
procedure uiLayoutCtl (ctl: TUIControl);


// ////////////////////////////////////////////////////////////////////////// //
var
  gh_ui_scale: Single = 1.0;


implementation

uses
  gh_flexlay,
  utils;


// ////////////////////////////////////////////////////////////////////////// //
var
  ctlsToKill: array of TUIControl = nil;


procedure scheduleKill (ctl: TUIControl);
var
  f: Integer;
begin
  if (ctl = nil) then exit;
  ctl := ctl.topLevel;
  for f := 0 to High(ctlsToKill) do
  begin
    if (ctlsToKill[f] = ctl) then exit;
    if (ctlsToKill[f] = nil) then begin ctlsToKill[f] := ctl; exit; end;
  end;
  SetLength(ctlsToKill, Length(ctlsToKill)+1);
  ctlsToKill[High(ctlsToKill)] := ctl;
end;


procedure processKills ();
var
  f: Integer;
  ctl: TUIControl;
begin
  for f := 0 to High(ctlsToKill) do
  begin
    ctl := ctlsToKill[f];
    if (ctl = nil) then break;
    ctlsToKill[f] := nil;
    FreeAndNil(ctl);
  end;
  if (Length(ctlsToKill) > 0) then ctlsToKill[0] := nil; // just in case
end;


// ////////////////////////////////////////////////////////////////////////// //
var
  knownCtlClasses: array of record
    klass: TUIControlClass;
    name: AnsiString;
  end = nil;


procedure registerCtlClass (aklass: TUIControlClass; const aname: AnsiString);
begin
  assert(aklass <> nil);
  assert(Length(aname) > 0);
  SetLength(knownCtlClasses, Length(knownCtlClasses)+1);
  knownCtlClasses[High(knownCtlClasses)].klass := aklass;
  knownCtlClasses[High(knownCtlClasses)].name := aname;
end;


function findCtlClass (const aname: AnsiString): TUIControlClass;
var
  f: Integer;
begin
  for f := 0 to High(knownCtlClasses) do
  begin
    if (strEquCI1251(aname, knownCtlClasses[f].name)) then
    begin
      result := knownCtlClasses[f].klass;
      exit;
    end;
  end;
  result := nil;
end;


// ////////////////////////////////////////////////////////////////////////// //
type
  TFlexLayouter = specialize TFlexLayouterBase<TUIControl>;

procedure uiLayoutCtl (ctl: TUIControl);
var
  lay: TFlexLayouter;
begin
  if (ctl = nil) then exit;
  lay := TFlexLayouter.Create();
  try
    if (ctl is TUITopWindow) and (TUITopWindow(ctl).fitToScreen) then TUITopWindow(ctl).flFitToScreen();

    lay.setup(ctl);
    //lay.layout();

    //writeln('============================'); lay.dumpFlat();

    //writeln('=== initial ==='); lay.dump();

    //lay.calcMaxSizeInternal(0);
    {
    lay.firstPass();
    writeln('=== after first pass ===');
    lay.dump();

    lay.secondPass();
    writeln('=== after second pass ===');
    lay.dump();
    }

    lay.layout();
    //writeln('=== final ==='); lay.dump();

    if (ctl.mParent = nil) and (ctl is TUITopWindow) and (TUITopWindow(ctl).mDoCenter) then
    begin
      TUITopWindow(ctl).centerInScreen();
    end;

    // calculate full size
    ctl.calcFullClientSize();

    // fix focus
    if (ctl.mParent = nil) then
    begin
      if (ctl.mFocused = nil) or (ctl.mFocused = ctl) or (not ctl.mFocused.enabled) then
      begin
        ctl.mFocused := ctl.findFirstFocus();
      end;
    end;

  finally
    FreeAndNil(lay);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
var
  uiTopList: array of TUIControl = nil;
  uiGrabCtl: TUIControl = nil;


procedure uiUpdateStyles ();
var
  ctl: TUIControl;
begin
  for ctl in uiTopList do ctl.updateStyle();
end;


procedure uiMouseEvent (var evt: THMouseEvent);
var
  ev: THMouseEvent;
  f, c: Integer;
  lx, ly: Integer;
  ctmp: TUIControl;
begin
  processKills();
  if (evt.eaten) or (evt.cancelled) then exit;
  ev := evt;
  ev.x := trunc(ev.x/gh_ui_scale);
  ev.y := trunc(ev.y/gh_ui_scale);
  ev.dx := trunc(ev.dx/gh_ui_scale); //FIXME
  ev.dy := trunc(ev.dy/gh_ui_scale); //FIXME
  try
    if (uiGrabCtl <> nil) then
    begin
      uiGrabCtl.mouseEvent(ev);
      if (ev.release) and ((ev.bstate and (not ev.but)) = 0) then uiGrabCtl := nil;
      ev.eat();
      exit;
    end;
    if (Length(uiTopList) > 0) and (uiTopList[High(uiTopList)].enabled) then uiTopList[High(uiTopList)].mouseEvent(ev);
    if (not ev.eaten) and (not ev.cancelled) and (ev.press) then
    begin
      for f := High(uiTopList) downto 0 do
      begin
        if uiTopList[f].toLocal(ev.x, ev.y, lx, ly) then
        begin
          if (uiTopList[f].enabled) and (f <> High(uiTopList)) then
          begin
            uiTopList[High(uiTopList)].blurred();
            ctmp := uiTopList[f];
            uiGrabCtl := nil;
            for c := f+1 to High(uiTopList) do uiTopList[c-1] := uiTopList[c];
            uiTopList[High(uiTopList)] := ctmp;
            ctmp.activated();
            ctmp.mouseEvent(ev);
          end;
          ev.eat();
          exit;
        end;
      end;
    end;
  finally
    if (ev.eaten) then evt.eat();
    if (ev.cancelled) then evt.cancel();
  end;
end;


procedure uiKeyEvent (var evt: THKeyEvent);
var
  ev: THKeyEvent;
begin
  processKills();
  if (evt.eaten) or (evt.cancelled) then exit;
  ev := evt;
  ev.x := trunc(ev.x/gh_ui_scale);
  ev.y := trunc(ev.y/gh_ui_scale);
  try
    if (Length(uiTopList) > 0) and (uiTopList[High(uiTopList)].enabled) then uiTopList[High(uiTopList)].keyEvent(ev);
    //if (ev.release) then begin ev.eat(); exit; end;
  finally
    if (ev.eaten) then evt.eat();
    if (ev.cancelled) then evt.cancel();
  end;
end;


procedure uiDraw ();
var
  f, cidx: Integer;
  ctl: TUIControl;
begin
  processKills();
  gxBeginUIDraw(gh_ui_scale);
  try
    for f := 0 to High(uiTopList) do
    begin
      ctl := uiTopList[f];
      ctl.draw();
      if (f <> High(uiTopList)) then
      begin
        cidx := ctl.getColorIndex;
        if (ctl.mDarken[cidx] > 0) then darkenRect(ctl.x0, ctl.y0, ctl.width, ctl.height, ctl.mDarken[cidx]);
      end;
    end;
  finally
    gxEndUIDraw();
  end;
end;


procedure uiAddWindow (ctl: TUIControl);
var
  f, c: Integer;
begin
  if (ctl = nil) then exit;
  ctl := ctl.topLevel;
  if not (ctl is TUITopWindow) then exit; // alas
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
  ctl.updateStyle();
  ctl.activated();
end;


procedure uiRemoveWindow (ctl: TUIControl);
var
  f, c: Integer;
begin
  if (ctl = nil) then exit;
  ctl := ctl.topLevel;
  if not (ctl is TUITopWindow) then exit; // alas
  for f := 0 to High(uiTopList) do
  begin
    if (uiTopList[f] = ctl) then
    begin
      ctl.blurred();
      for c := f+1 to High(uiTopList) do uiTopList[c-1] := uiTopList[c];
      SetLength(uiTopList, Length(uiTopList)-1);
      if (ctl is TUITopWindow) then
      begin
        try
          if assigned(TUITopWindow(ctl).closeCB) then TUITopWindow(ctl).closeCB(ctl);
        finally
          if (TUITopWindow(ctl).mFreeOnClose) then scheduleKill(ctl);
        end;
      end;
      exit;
    end;
  end;
end;


function uiVisibleWindow (ctl: TUIControl): Boolean;
var
  f: Integer;
begin
  result := false;
  if (ctl = nil) then exit;
  ctl := ctl.topLevel;
  if not (ctl is TUITopWindow) then exit; // alas
  for f := 0 to High(uiTopList) do
  begin
    if (uiTopList[f] = ctl) then begin result := true; exit; end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TUIControl.Create ();
begin
end;


procedure TUIControl.AfterConstruction ();
begin
  inherited;
  mParent := nil;
  mId := '';
  mX := 0;
  mY := 0;
  mWidth := 64;
  mHeight := 8;
  mFrameWidth := 0;
  mFrameHeight := 0;
  mEnabled := true;
  mCanFocus := true;
  mChildren := nil;
  mFocused := nil;
  mEscClose := false;
  scallowed := false;
  mDrawShadow := false;
  actionCB := nil;
  // layouter interface
  //mDefSize := TLaySize.Create(64, 8); // default size
  mDefSize := TLaySize.Create(0, 0); // default size
  mMaxSize := TLaySize.Create(-1, -1); // maximum size
  mFlex := 0;
  mHoriz := true;
  mCanWrap := false;
  mLineStart := false;
  mHGroup := '';
  mVGroup := '';
  mStyleId := '';
  mCtl4Style := '';
  mAlign := -1; // left/top
  mExpand := false;
end;


destructor TUIControl.Destroy ();
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


function TUIControl.getColorIndex (): Integer; inline;
begin
  if (not enabled) then begin result := ClrIdxDisabled; exit; end;
  // top windows: no focus hack
  if (self is TUITopWindow) then
  begin
    if (getActive) then begin result := ClrIdxActive; exit; end;
  end
  else
  begin
    // if control cannot be focused, take "active" color scheme for it (it is easier this way)
    if (not canFocus) or (getActive) then begin result := ClrIdxActive; exit; end;
  end;
  result := ClrIdxInactive;
end;

procedure TUIControl.updateStyle ();
var
  stl: TUIStyle = nil;
  ctl: TUIControl;
begin
  ctl := self;
  while (ctl <> nil) do
  begin
    if (Length(ctl.mStyleId) <> 0) then begin stl := uiFindStyle(ctl.mStyleId); break; end;
    ctl := ctl.mParent;
  end;
  if (stl = nil) then stl := uiFindStyle(''); // default
  cacheStyle(stl);
  for ctl in mChildren do ctl.updateStyle();
end;

procedure TUIControl.cacheStyle (root: TUIStyle);
var
  cst: AnsiString;
begin
  //writeln('caching style for <', className, '> (', mCtl4Style, ')...');
  cst := mCtl4Style;
  // active
  mBackColor[ClrIdxActive] := root.get('back-color', 'active', cst).asRGBADef(TGxRGBA.Create(0, 0, 128));
  mTextColor[ClrIdxActive] := root.get('text-color', 'active', cst).asRGBADef(TGxRGBA.Create(255, 255, 255));
  mFrameColor[ClrIdxActive] := root.get('frame-color', 'active', cst).asRGBADef(TGxRGBA.Create(255, 255, 255));
  mFrameTextColor[ClrIdxActive] := root.get('frame-text-color', 'active', cst).asRGBADef(TGxRGBA.Create(255, 255, 255));
  mFrameIconColor[ClrIdxActive] := root.get('frame-icon-color', 'active', cst).asRGBADef(TGxRGBA.Create(0, 255, 0));
  mDarken[ClrIdxActive] := root.get('darken', 'active', cst).asInt(-1);
  // disabled
  mBackColor[ClrIdxDisabled] := root.get('back-color', 'disabled', cst).asRGBADef(TGxRGBA.Create(0, 0, 128));
  mTextColor[ClrIdxDisabled] := root.get('text-color', 'disabled', cst).asRGBADef(TGxRGBA.Create(127, 127, 127));
  mFrameColor[ClrIdxDisabled] := root.get('frame-color', 'disabled', cst).asRGBADef(TGxRGBA.Create(127, 127, 127));
  mFrameTextColor[ClrIdxDisabled] := root.get('frame-text-color', 'disabled', cst).asRGBADef(TGxRGBA.Create(127, 127, 127));
  mFrameIconColor[ClrIdxDisabled] := root.get('frame-icon-color', 'disabled', cst).asRGBADef(TGxRGBA.Create(0, 127, 0));
  mDarken[ClrIdxDisabled] := root.get('darken', 'disabled', cst).asInt(-1);
  // inactive
  mBackColor[ClrIdxInactive] := root.get('back-color', 'inactive', cst).asRGBADef(TGxRGBA.Create(0, 0, 128));
  mTextColor[ClrIdxInactive] := root.get('text-color', 'inactive', cst).asRGBADef(TGxRGBA.Create(255, 255, 255));
  mFrameColor[ClrIdxInactive] := root.get('frame-color', 'inactive', cst).asRGBADef(TGxRGBA.Create(255, 255, 255));
  mFrameTextColor[ClrIdxInactive] := root.get('frame-text-color', 'inactive', cst).asRGBADef(TGxRGBA.Create(255, 255, 255));
  mFrameIconColor[ClrIdxInactive] := root.get('frame-icon-color', 'inactive', cst).asRGBADef(TGxRGBA.Create(0, 255, 0));
  mDarken[ClrIdxInactive] := root.get('darken', 'inactive', cst).asInt(-1);
end;


// ////////////////////////////////////////////////////////////////////////// //
function TUIControl.getDefSize (): TLaySize; inline; begin result := mLayDefSize; end;
function TUIControl.getMaxSize (): TLaySize; inline; begin result := mLayMaxSize; end;
function TUIControl.getFlex (): Integer; inline; begin result := mFlex; end;
function TUIControl.isHorizBox (): Boolean; inline; begin result := mHoriz; end;
procedure TUIControl.setHorizBox (v: Boolean); inline; begin mHoriz := v; end;
function TUIControl.canWrap (): Boolean; inline; begin result := mCanWrap; end;
procedure TUIControl.setCanWrap (v: Boolean); inline; begin mCanWrap := v; end;
function TUIControl.isLineStart (): Boolean; inline; begin result := mLineStart; end;
procedure TUIControl.setLineStart (v: Boolean); inline; begin mLineStart := v; end;
function TUIControl.getAlign (): Integer; inline; begin result := mAlign; end;
procedure TUIControl.setAlign (v: Integer); inline; begin mAlign := v; end;
function TUIControl.getExpand (): Boolean; inline; begin result := mExpand; end;
procedure TUIControl.setExpand (v: Boolean); inline; begin mExpand := v; end;
function TUIControl.getHGroup (): AnsiString; inline; begin result := mHGroup; end;
procedure TUIControl.setHGroup (const v: AnsiString); inline; begin mHGroup := v; end;
function TUIControl.getVGroup (): AnsiString; inline; begin result := mVGroup; end;
procedure TUIControl.setVGroup (const v: AnsiString); inline; begin mVGroup := v; end;

function TUIControl.getMargins (): TLayMargins; inline;
begin
  result := TLayMargins.Create(mFrameHeight, mFrameWidth, mFrameHeight, mFrameWidth);
end;

procedure TUIControl.setActualSizePos (constref apos: TLayPos; constref asize: TLaySize); inline;
begin
  //writeln(self.className, '; pos=', apos.toString, '; size=', asize.toString);
  if (mParent <> nil) then
  begin
    mX := apos.x;
    mY := apos.y;
  end;
  mWidth := asize.w;
  mHeight := asize.h;
end;

procedure TUIControl.layPrepare ();
begin
  mLayDefSize := mDefSize;
  mLayMaxSize := mMaxSize;
  if (mLayMaxSize.w >= 0) then mLayMaxSize.w += mFrameWidth*2;
  if (mLayMaxSize.h >= 0) then mLayMaxSize.h += mFrameHeight*2;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TUIControl.parsePos (par: TTextParser): TLayPos;
var
  ech: AnsiChar = ')';
begin
  if (par.eatDelim('[')) then ech := ']' else par.expectDelim('(');
  result.x := par.expectInt();
  par.eatDelim(','); // optional comma
  result.y := par.expectInt();
  par.eatDelim(','); // optional comma
  par.expectDelim(ech);
end;

function TUIControl.parseSize (par: TTextParser): TLaySize;
var
  ech: AnsiChar = ')';
begin
  if (par.eatDelim('[')) then ech := ']' else par.expectDelim('(');
  result.w := par.expectInt();
  par.eatDelim(','); // optional comma
  result.h := par.expectInt();
  par.eatDelim(','); // optional comma
  par.expectDelim(ech);
end;

function TUIControl.parseBool (par: TTextParser): Boolean;
begin
  result :=
    par.eatIdOrStrCI('true') or
    par.eatIdOrStrCI('yes') or
    par.eatIdOrStrCI('tan');
  if not result then
  begin
    if (not par.eatIdOrStrCI('false')) and (not par.eatIdOrStrCI('no')) and (not par.eatIdOrStrCI('ona')) then
    begin
      par.error('boolean value expected');
    end;
  end;
end;

function TUIControl.parseAnyAlign (par: TTextParser): Integer;
begin
       if (par.eatIdOrStrCI('left')) or (par.eatIdOrStrCI('top')) then result := -1
  else if (par.eatIdOrStrCI('right')) or (par.eatIdOrStrCI('bottom')) then result := 1
  else if (par.eatIdOrStrCI('center')) then result := 0
  else par.error('invalid align value');
end;

function TUIControl.parseHAlign (par: TTextParser): Integer;
begin
       if (par.eatIdOrStrCI('left')) then result := -1
  else if (par.eatIdOrStrCI('right')) then result := 1
  else if (par.eatIdOrStrCI('center')) then result := 0
  else par.error('invalid horizontal align value');
end;

function TUIControl.parseVAlign (par: TTextParser): Integer;
begin
       if (par.eatIdOrStrCI('top')) then result := -1
  else if (par.eatIdOrStrCI('bottom')) then result := 1
  else if (par.eatIdOrStrCI('center')) then result := 0
  else par.error('invalid vertical align value');
end;

procedure TUIControl.parseTextAlign (par: TTextParser; var h, v: Integer);
var
  wasH: Boolean = false;
  wasV: Boolean = false;
begin
  while true do
  begin
    if (par.eatIdOrStrCI('left')) then
    begin
      if wasH then par.error('too many align directives');
      wasH := true;
      h := -1;
      continue;
    end;
    if (par.eatIdOrStrCI('right')) then
    begin
      if wasH then par.error('too many align directives');
      wasH := true;
      h := 1;
      continue;
    end;
    if (par.eatIdOrStrCI('hcenter')) then
    begin
      if wasH then par.error('too many align directives');
      wasH := true;
      h := 0;
      continue;
    end;
    if (par.eatIdOrStrCI('top')) then
    begin
      if wasV then par.error('too many align directives');
      wasV := true;
      v := -1;
      continue;
    end;
    if (par.eatIdOrStrCI('bottom')) then
    begin
      if wasV then par.error('too many align directives');
      wasV := true;
      v := 1;
      continue;
    end;
    if (par.eatIdOrStrCI('vcenter')) then
    begin
      if wasV then par.error('too many align directives');
      wasV := true;
      v := 0;
      continue;
    end;
    if (par.eatIdOrStrCI('center')) then
    begin
      if wasV or wasH then par.error('too many align directives');
      wasV := true;
      wasH := true;
      h := 0;
      v := 0;
      continue;
    end;
    break;
  end;
  if not wasV and not wasH then par.error('invalid align value');
end;

function TUIControl.parseOrientation (const prname: AnsiString; par: TTextParser): Boolean;
begin
  if (strEquCI1251(prname, 'orientation')) or (strEquCI1251(prname, 'orient')) then
  begin
         if (par.eatIdOrStrCI('horizontal')) or (par.eatIdOrStrCI('horiz')) then mHoriz := true
    else if (par.eatIdOrStrCI('vertical')) or (par.eatIdOrStrCI('vert')) then mHoriz := false
    else par.error('`horizontal` or `vertical` expected');
    result := true;
  end
  else
  begin
    result := false;
  end;
end;

// par should be on '{'; final '}' is eaten
procedure TUIControl.parseProperties (par: TTextParser);
var
  pn: AnsiString;
begin
  if (not par.eatDelim('{')) then exit;
  while (not par.eatDelim('}')) do
  begin
    if (not par.isIdOrStr) then par.error('property name expected');
    pn := par.tokStr;
    par.skipToken();
    par.eatDelim(':'); // optional
    if not parseProperty(pn, par) then par.errorfmt('invalid property name ''%s''', [pn]);
    par.eatDelim(','); // optional
  end;
end;

// par should be on '{'
procedure TUIControl.parseChildren (par: TTextParser);
var
  cc: TUIControlClass;
  ctl: TUIControl;
begin
  par.expectDelim('{');
  while (not par.eatDelim('}')) do
  begin
    if (not par.isIdOrStr) then par.error('control name expected');
    cc := findCtlClass(par.tokStr);
    if (cc = nil) then par.errorfmt('unknown control name: ''%s''', [par.tokStr]);
    //writeln('children for <', par.tokStr, '>: <', cc.className, '>');
    par.skipToken();
    par.eatDelim(':'); // optional
    ctl := cc.Create();
    //writeln('  mHoriz=', ctl.mHoriz);
    try
      ctl.parseProperties(par);
    except
      FreeAndNil(ctl);
      raise;
    end;
    //writeln(': ', ctl.mDefSize.toString);
    appendChild(ctl);
    par.eatDelim(','); // optional
  end;
end;


function TUIControl.parseProperty (const prname: AnsiString; par: TTextParser): Boolean;
begin
  result := true;
  if (strEquCI1251(prname, 'id')) then begin mId := par.expectIdOrStr(true); exit; end; // allow empty strings
  if (strEquCI1251(prname, 'style')) then begin mStyleId := par.expectIdOrStr(); exit; end; // no empty strings
  if (strEquCI1251(prname, 'flex')) then begin flex := par.expectInt(); exit; end;
  // sizes
  if (strEquCI1251(prname, 'defsize')) or (strEquCI1251(prname, 'size')) then begin mDefSize := parseSize(par); exit; end;
  if (strEquCI1251(prname, 'maxsize')) then begin mMaxSize := parseSize(par); exit; end;
  if (strEquCI1251(prname, 'defwidth')) or (strEquCI1251(prname, 'width')) then begin mDefSize.w := par.expectInt(); exit; end;
  if (strEquCI1251(prname, 'defheight')) or (strEquCI1251(prname, 'height')) then begin mDefSize.h := par.expectInt(); exit; end;
  if (strEquCI1251(prname, 'maxwidth')) then begin mMaxSize.w := par.expectInt(); exit; end;
  if (strEquCI1251(prname, 'maxheight')) then begin mMaxSize.h := par.expectInt(); exit; end;
  // flags
  if (strEquCI1251(prname, 'wrap')) then begin mCanWrap := parseBool(par); exit; end;
  if (strEquCI1251(prname, 'linestart')) then begin mLineStart := parseBool(par); exit; end;
  if (strEquCI1251(prname, 'expand')) then begin mExpand := parseBool(par); exit; end;
  // align
  if (strEquCI1251(prname, 'align')) then begin mAlign := parseAnyAlign(par); exit; end;
  if (strEquCI1251(prname, 'hgroup')) then begin mHGroup := par.expectIdOrStr(true); exit; end; // allow empty strings
  if (strEquCI1251(prname, 'vgroup')) then begin mVGroup := par.expectIdOrStr(true); exit; end; // allow empty strings
  // other
  if (strEquCI1251(prname, 'canfocus')) then begin mCanFocus := true; exit; end;
  if (strEquCI1251(prname, 'nofocus')) then begin mCanFocus := false; exit; end;
  if (strEquCI1251(prname, 'disabled')) then begin mEnabled := false; exit; end;
  if (strEquCI1251(prname, 'enabled')) then begin mEnabled := true; exit; end;
  if (strEquCI1251(prname, 'escclose')) then begin mEscClose := not parseBool(par); exit; end;
  if (strEquCI1251(prname, 'default')) then begin mDefault := true; exit; end;
  if (strEquCI1251(prname, 'cancel')) then begin mCancel := true; exit; end;
  result := false;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUIControl.activated ();
begin
  makeVisibleInParent();
end;


procedure TUIControl.blurred ();
begin
  if (uiGrabCtl = self) then uiGrabCtl := nil;
end;


procedure TUIControl.calcFullClientSize ();
var
  ctl: TUIControl;
begin
  mFullSize := TLaySize.Create(0, 0);
  if (mWidth < 1) or (mHeight < 1) then exit;
  for ctl in mChildren do
  begin
    ctl.calcFullClientSize();
    mFullSize.w := nmax(mFullSize.w, ctl.mX-mFrameWidth+ctl.mFullSize.w);
    mFullSize.h := nmax(mFullSize.h, ctl.mY-mFrameHeight+ctl.mFullSize.h);
  end;
  mFullSize.w := nmax(mFullSize.w, mWidth-mFrameWidth*2);
  mFullSize.h := nmax(mFullSize.h, mHeight-mFrameHeight*2);
end;


function TUIControl.topLevel (): TUIControl; inline;
begin
  result := self;
  while (result.mParent <> nil) do result := result.mParent;
end;


function TUIControl.getEnabled (): Boolean;
var
  ctl: TUIControl;
begin
  result := false;
  if (not mEnabled) then exit;
  ctl := mParent;
  while (ctl <> nil) do
  begin
    if (not ctl.mEnabled) then exit;
    ctl := ctl.mParent;
  end;
  result := true;
end;


procedure TUIControl.setEnabled (v: Boolean); inline;
begin
  if (mEnabled = v) then exit;
  mEnabled := v;
  if (not v) and focused then setFocused(false);
end;


function TUIControl.getFocused (): Boolean; inline;
begin
  if (mParent = nil) then
  begin
    result := (Length(uiTopList) > 0) and (uiTopList[High(uiTopList)] = self);
  end
  else
  begin
    result := (topLevel.mFocused = self);
    if (result) then result := (Length(uiTopList) > 0) and (uiTopList[High(uiTopList)] = topLevel);
  end;
end;


function TUIControl.getActive (): Boolean; inline;
var
  ctl: TUIControl;
begin
  if (mParent = nil) then
  begin
    result := (Length(uiTopList) > 0) and (uiTopList[High(uiTopList)] = self);
  end
  else
  begin
    ctl := topLevel.mFocused;
    while (ctl <> nil) and (ctl <> self) do ctl := ctl.mParent;
    result := (ctl = self);
    if (result) then result := (Length(uiTopList) > 0) and (uiTopList[High(uiTopList)] = topLevel);
  end;
end;


procedure TUIControl.setFocused (v: Boolean); inline;
var
  tl: TUIControl;
begin
  tl := topLevel;
  if (not v) then
  begin
    if (tl.mFocused = self) then
    begin
      blurred(); // this will reset grab, but still...
      if (uiGrabCtl = self) then uiGrabCtl := nil;
      tl.mFocused := tl.findNextFocus(self, true);
      if (tl.mFocused = self) then tl.mFocused := nil;
      if (tl.mFocused <> nil) then tl.mFocused.activated();
    end;
    exit;
  end;
  if (not canFocus) then exit;
  if (tl.mFocused <> self) then
  begin
    if (tl.mFocused <> nil) then tl.mFocused.blurred();
    tl.mFocused := self;
    if (uiGrabCtl <> self) then uiGrabCtl := nil;
    activated();
  end;
end;


function TUIControl.getCanFocus (): Boolean; inline;
begin
  result := (getEnabled) and (mCanFocus) and (mWidth > 0) and (mHeight > 0);
end;


function TUIControl.isMyChild (ctl: TUIControl): Boolean;
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
function TUIControl.toLocal (var x, y: Integer): Boolean;
begin
  if (mParent = nil) then
  begin
    Dec(x, mX);
    Dec(y, mY);
    result := true; // hack
  end
  else
  begin
    result := mParent.toLocal(x, y);
    Inc(x, mParent.mScrollX);
    Inc(y, mParent.mScrollY);
    Dec(x, mX);
    Dec(y, mY);
    if result then result := (x >= 0) and (y >= 0) and (x < mParent.mWidth) and (y < mParent.mHeight);
  end;
  if result then result := (x >= 0) and (y >= 0) and (x < mWidth) and (y < mHeight);
end;

function TUIControl.toLocal (gx, gy: Integer; out x, y: Integer): Boolean; inline;
begin
  x := gx;
  y := gy;
  result := toLocal(x, y);
end;


procedure TUIControl.toGlobal (var x, y: Integer);
begin
  Inc(x, mX);
  Inc(y, mY);
  if (mParent <> nil) then
  begin
    Dec(x, mParent.mScrollX);
    Dec(y, mParent.mScrollY);
    mParent.toGlobal(x, y);
  end;
end;

procedure TUIControl.toGlobal (lx, ly: Integer; out x, y: Integer); inline;
begin
  x := lx;
  y := ly;
  toGlobal(x, y);
end;

procedure TUIControl.getDrawRect (out gx, gy, wdt, hgt: Integer);
var
  cgx, cgy: Integer;
begin
  if (mParent = nil) then
  begin
    gx := mX;
    gy := mY;
    wdt := mWidth;
    hgt := mHeight;
  end
  else
  begin
    toGlobal(0, 0, cgx, cgy);
    mParent.getDrawRect(gx, gy, wdt, hgt);
    if (wdt > 0) and (hgt > 0) then
    begin
      if not intersectRect(gx, gy, wdt, hgt, cgx, cgy, mWidth, mHeight) then
      begin
        wdt := 0;
        hgt := 0;
      end;
    end;
  end;
end;


// x and y are global coords
function TUIControl.controlAtXY (x, y: Integer; allowDisabled: Boolean=false): TUIControl;
var
  lx, ly: Integer;
  f: Integer;
begin
  result := nil;
  if (not allowDisabled) and (not enabled) then exit;
  if (mWidth < 1) or (mHeight < 1) then exit;
  if not toLocal(x, y, lx, ly) then exit;
  for f := High(mChildren) downto 0 do
  begin
    result := mChildren[f].controlAtXY(x, y, allowDisabled);
    if (result <> nil) then exit;
  end;
  result := self;
end;


function TUIControl.parentScrollX (): Integer; inline; begin if (mParent <> nil) then result := mParent.mScrollX else result := 0; end;
function TUIControl.parentScrollY (): Integer; inline; begin if (mParent <> nil) then result := mParent.mScrollY else result := 0; end;


procedure TUIControl.makeVisibleInParent ();
var
  sy, ey, cy: Integer;
  p: TUIControl;
begin
  if (mWidth < 1) or (mHeight < 1) then exit;
  p := mParent;
  if (p = nil) then exit;
  if (p.mFullSize.w < 1) or (p.mFullSize.h < 1) then
  begin
    p.mScrollX := 0;
    p.mScrollY := 0;
    exit;
  end;
  p.makeVisibleInParent();
  cy := mY-p.mFrameHeight;
  sy := p.mScrollY;
  ey := sy+(p.mHeight-p.mFrameHeight*2);
  if (cy < sy) then
  begin
    p.mScrollY := nmax(0, cy);
  end
  else if (cy+mHeight > ey) then
  begin
    p.mScrollY := nmax(0, cy+mHeight-(p.mHeight-p.mFrameHeight*2));
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TUIControl.prevSibling (): TUIControl;
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

function TUIControl.nextSibling (): TUIControl;
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

function TUIControl.firstChild (): TUIControl; inline;
begin
  if (Length(mChildren) <> 0) then result := mChildren[0] else result := nil;
end;

function TUIControl.lastChild (): TUIControl; inline;
begin
  if (Length(mChildren) <> 0) then result := mChildren[High(mChildren)] else result := nil;
end;


function TUIControl.findFirstFocus (): TUIControl;
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
    if (canFocus) then result := self;
  end;
end;


function TUIControl.findLastFocus (): TUIControl;
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
    if (canFocus) then result := self;
  end;
end;


function TUIControl.findNextFocus (cur: TUIControl; wrap: Boolean): TUIControl;
var
  curHit: Boolean = false;

  function checkFocus (ctl: TUIControl): Boolean;
  begin
    if curHit then
    begin
      result := (ctl.canFocus);
    end
    else
    begin
      curHit := (ctl = cur);
      result := false; // don't stop
    end;
  end;

begin
  result := nil;
  if enabled then
  begin
    if not isMyChild(cur) then
    begin
      result := findFirstFocus();
    end
    else
    begin
      result := forEachControl(checkFocus);
      if (result = nil) and (wrap) then result := findFirstFocus();
    end;
  end;
end;


function TUIControl.findPrevFocus (cur: TUIControl; wrap: Boolean): TUIControl;
var
  lastCtl: TUIControl = nil;

  function checkFocus (ctl: TUIControl): Boolean;
  begin
    if (ctl = cur) then
    begin
      result := true;
    end
    else
    begin
      result := false;
      if (ctl.canFocus) then lastCtl := ctl;
    end;
  end;

begin
  result := nil;
  if enabled then
  begin
    if not isMyChild(cur) then
    begin
      result := findLastFocus();
    end
    else
    begin
      forEachControl(checkFocus);
      if (lastCtl = nil) and (wrap) then lastCtl := findLastFocus();
      result := lastCtl;
      //if (lastCtl <> nil) then writeln('ctl<', lastCtl.className, '>: {', lastCtl.id, '}');
    end;
  end;
end;


function TUIControl.findDefaulControl (): TUIControl;
var
  ctl: TUIControl;
begin
  if (enabled) then
  begin
    if (mDefault) then begin result := self; exit; end;
    for ctl in mChildren do
    begin
      result := ctl.findDefaulControl();
      if (result <> nil) then exit;
    end;
  end;
  result := nil;
end;

function TUIControl.findCancelControl (): TUIControl;
var
  ctl: TUIControl;
begin
  if (enabled) then
  begin
    if (mCancel) then begin result := self; exit; end;
    for ctl in mChildren do
    begin
      result := ctl.findCancelControl();
      if (result <> nil) then exit;
    end;
  end;
  result := nil;
end;


function TUIControl.findControlById (const aid: AnsiString): TUIControl;
var
  ctl: TUIControl;
begin
  if (strEquCI1251(aid, mId)) then begin result := self; exit; end;
  for ctl in mChildren do
  begin
    result := ctl.findControlById(aid);
    if (result <> nil) then exit;
  end;
  result := nil;
end;


procedure TUIControl.appendChild (ctl: TUIControl);
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
end;


function TUIControl.setActionCBFor (const aid: AnsiString; cb: TActionCB): TActionCB;
var
  ctl: TUIControl;
begin
  ctl := self[aid];
  if (ctl <> nil) then
  begin
    result := ctl.actionCB;
    ctl.actionCB := cb;
  end
  else
  begin
    result := nil;
  end;
end;


function TUIControl.forEachChildren (cb: TCtlEnumCB): TUIControl;
var
  ctl: TUIControl;
begin
  result := nil;
  if (not assigned(cb)) then exit;
  for ctl in mChildren do
  begin
    if cb(ctl) then begin result := ctl; exit; end;
  end;
end;


function TUIControl.forEachControl (cb: TCtlEnumCB; includeSelf: Boolean=true): TUIControl;

  function forChildren (p: TUIControl; incSelf: Boolean): TUIControl;
  var
    ctl: TUIControl;
  begin
    result := nil;
    if (p = nil) then exit;
    if (incSelf) and (cb(p)) then begin result := p; exit; end;
    for ctl in p.mChildren do
    begin
      result := forChildren(ctl, true);
      if (result <> nil) then break;
    end;
  end;

begin
  result := nil;
  if (not assigned(cb)) then exit;
  result := forChildren(self, includeSelf);
end;


procedure TUIControl.close (); // this closes *top-level* control
var
  ctl: TUIControl;
begin
  ctl := topLevel;
  uiRemoveWindow(ctl);
  if (ctl is TUITopWindow) and (TUITopWindow(ctl).mFreeOnClose) then scheduleKill(ctl); // just in case
end;


procedure TUIControl.doAction ();
begin
  if assigned(actionCB) then actionCB(self);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUIControl.setScissorGLInternal (x, y, w, h: Integer);
begin
  if not scallowed then exit;
  x := trunc(x*gh_ui_scale);
  y := trunc(y*gh_ui_scale);
  w := trunc(w*gh_ui_scale);
  h := trunc(h*gh_ui_scale);
  scis.combineRect(x, y, w, h);
end;

procedure TUIControl.setScissor (lx, ly, lw, lh: Integer);
var
  gx, gy, wdt, hgt, cgx, cgy: Integer;
begin
  if not scallowed then exit;

  if not intersectRect(lx, ly, lw, lh, 0, 0, mWidth, mHeight) then
  begin
    scis.combineRect(0, 0, 0, 0);
    exit;
  end;

  getDrawRect(gx, gy, wdt, hgt);
  toGlobal(lx, ly, cgx, cgy);
  if not intersectRect(gx, gy, wdt, hgt, cgx, cgy, lw, lh) then
  begin
    scis.combineRect(0, 0, 0, 0);
    exit;
  end;

  setScissorGLInternal(gx, gy, wdt, hgt);
end;

procedure TUIControl.resetScissor (fullArea: Boolean); inline;
begin
  if not scallowed then exit;
  if (fullArea) then
  begin
    setScissor(0, 0, mWidth, mHeight);
  end
  else
  begin
    setScissor(mFrameWidth, mFrameHeight, mWidth-mFrameWidth*2, mHeight-mFrameHeight*2);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUIControl.draw ();
var
  f: Integer;
  gx, gy: Integer;
begin
  if (mWidth < 1) or (mHeight < 1) then exit;
  toGlobal(0, 0, gx, gy);

  scis.save(true); // scissoring enabled
  try
    scallowed := true;
    resetScissor(true); // full area
    drawControl(gx, gy);
    resetScissor(false); // client area
    for f := 0 to High(mChildren) do mChildren[f].draw();
    resetScissor(true); // full area
    drawControlPost(gx, gy);
  finally
    scis.restore();
    scallowed := false;
  end;
end;

procedure TUIControl.drawControl (gx, gy: Integer);
begin
  //if (mParent = nil) then darkenRect(gx, gy, mWidth, mHeight, 64);
end;

procedure TUIControl.drawControlPost (gx, gy: Integer);
begin
  // shadow
  if mDrawShadow and (mWidth > 0) and (mHeight > 0) then
  begin
    setScissorGLInternal(gx+8, gy+8, mWidth, mHeight);
    darkenRect(gx+mWidth, gy+8, 8, mHeight, 128);
    darkenRect(gx+8, gy+mHeight, mWidth-8, 8, 128);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUIControl.mouseEvent (var ev: THMouseEvent);
var
  ctl: TUIControl;
begin
  if (not enabled) then exit;
  if (mWidth < 1) or (mHeight < 1) then exit;
  ctl := controlAtXY(ev.x, ev.y);
  if (ctl = nil) then exit;
  if (ctl.canFocus) and (ev.press) then
  begin
    if (ctl <> topLevel.mFocused) then ctl.setFocused(true);
    uiGrabCtl := ctl;
  end;
  if (ctl <> self) then ctl.mouseEvent(ev);
  //ev.eat();
end;


procedure TUIControl.keyEvent (var ev: THKeyEvent);

  function doPreKey (ctl: TUIControl): Boolean;
  begin
    if (not ctl.enabled) then begin result := false; exit; end;
    ctl.keyEventPre(ev);
    result := (ev.eaten) or (ev.cancelled); // stop if event was consumed
  end;

  function doPostKey (ctl: TUIControl): Boolean;
  begin
    if (not ctl.enabled) then begin result := false; exit; end;
    ctl.keyEventPost(ev);
    result := (ev.eaten) or (ev.cancelled); // stop if event was consumed
  end;

var
  ctl: TUIControl;
begin
  if (not enabled) then exit;
  if (ev.eaten) or (ev.cancelled) then exit;
  // call pre-key
  if (mParent = nil) then
  begin
    forEachControl(doPreKey);
    if (ev.eaten) or (ev.cancelled) then exit;
  end;
  // focused control should process keyboard first
  if (topLevel.mFocused <> self) and isMyChild(topLevel.mFocused) and (topLevel.mFocused.enabled) then
  begin
    // bubble keyboard event
    ctl := topLevel.mFocused;
    while (ctl <> nil) and (ctl <> self) do
    begin
      ctl.keyEvent(ev);
      if (ev.eaten) or (ev.cancelled) then exit;
      ctl := ctl.mParent;
    end;
  end;
  // for top-level controls
  if (mParent = nil) then
  begin
    if (ev = 'S-Tab') then
    begin
      ctl := findPrevFocus(mFocused, true);
      if (ctl <> nil) and (ctl <> mFocused) then ctl.setFocused(true);
      ev.eat();
      exit;
    end;
    if (ev = 'Tab') then
    begin
      ctl := findNextFocus(mFocused, true);
      if (ctl <> nil) and (ctl <> mFocused) then ctl.setFocused(true);
      ev.eat();
      exit;
    end;
    if (ev = 'Enter') or (ev = 'C-Enter') then
    begin
      ctl := findDefaulControl();
      if (ctl <> nil) then
      begin
        ev.eat();
        ctl.doAction();
        exit;
      end;
    end;
    if (ev = 'Escape') then
    begin
      ctl := findCancelControl();
      if (ctl <> nil) then
      begin
        ev.eat();
        ctl.doAction();
        exit;
      end;
    end;
    if mEscClose and (ev = 'Escape') then
    begin
      if (not assigned(closeRequestCB)) or (closeRequestCB(self)) then
      begin
        uiRemoveWindow(self);
      end;
      ev.eat();
      exit;
    end;
    // call post-keys
    if (ev.eaten) or (ev.cancelled) then exit;
    forEachControl(doPostKey);
  end;
end;


procedure TUIControl.keyEventPre (var ev: THKeyEvent);
begin
end;


procedure TUIControl.keyEventPost (var ev: THKeyEvent);
begin
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TUITopWindow.Create (const atitle: AnsiString);
begin
  inherited Create();
  mTitle := atitle;
end;


procedure TUITopWindow.AfterConstruction ();
begin
  inherited;
  mFitToScreen := true;
  mFrameWidth := 8;
  mFrameHeight := 8;
  if (mWidth < mFrameWidth*2+3*8) then mWidth := mFrameWidth*2+3*8;
  if (mHeight < mFrameHeight*2) then mHeight := mFrameHeight*2;
  if (Length(mTitle) > 0) then
  begin
    if (mWidth < Length(mTitle)*8+mFrameWidth*2+3*8) then mWidth := Length(mTitle)*8+mFrameWidth*2+3*8;
  end;
  mCanFocus := false;
  mDragScroll := TXMode.None;
  mDrawShadow := true;
  mWaitingClose := false;
  mInClose := false;
  closeCB := nil;
  mCtl4Style := 'window';
end;


function TUITopWindow.parseProperty (const prname: AnsiString; par: TTextParser): Boolean;
begin
  if (strEquCI1251(prname, 'title')) or (strEquCI1251(prname, 'caption')) then
  begin
    mTitle := par.expectIdOrStr(true);
    result := true;
    exit;
  end;
  if (strEquCI1251(prname, 'children')) then
  begin
    parseChildren(par);
    result := true;
    exit;
  end;
  if (strEquCI1251(prname, 'position')) then
  begin
         if (par.eatIdOrStrCI('default')) then mDoCenter := false
    else if (par.eatIdOrStrCI('center')) then mDoCenter := true
    else par.error('`center` or `default` expected');
    result := true;
    exit;
  end;
  if (parseOrientation(prname, par)) then begin result := true; exit; end;
  result := inherited parseProperty(prname, par);
end;


procedure TUITopWindow.flFitToScreen ();
begin
  flMaxSize := TLaySize.Create(trunc(getScrWdt/gh_ui_scale)-mFrameWidth*2-6, trunc(getScrHgt/gh_ui_scale)-mFrameHeight*2-6);
end;


procedure TUITopWindow.centerInScreen ();
begin
  if (mWidth > 0) and (mHeight > 0) then
  begin
    mX := trunc((gScrWidth/gh_ui_scale-mWidth)/2);
    mY := trunc((gScrHeight/gh_ui_scale-mHeight)/2);
  end;
end;


procedure TUITopWindow.drawControl (gx, gy: Integer);
begin
  fillRect(gx, gy, mWidth, mHeight, mBackColor[getColorIndex]);
end;


procedure TUITopWindow.drawControlPost (gx, gy: Integer);
var
  cidx: Integer;
  tx, hgt, sbhgt: Integer;
begin
  cidx := getColorIndex;
  if (mDragScroll = TXMode.Drag) then
  begin
    drawRectUI(gx+4, gy+4, mWidth-8, mHeight-8, mFrameColor[cidx]);
  end
  else
  begin
    drawRectUI(gx+3, gy+3, mWidth-6, mHeight-6, mFrameColor[cidx]);
    drawRectUI(gx+5, gy+5, mWidth-10, mHeight-10, mFrameColor[cidx]);
    // vertical scroll bar
    hgt := mHeight-mFrameHeight*2;
    if (hgt > 0) and (mFullSize.h > hgt) then
    begin
      //writeln(mTitle, ': height=', mHeight-mFrameHeight*2, '; fullsize=', mFullSize.toString);
      sbhgt := mHeight-mFrameHeight*2+2;
      fillRect(gx+mWidth-mFrameWidth+1, gy+7, mFrameWidth-3, sbhgt, mFrameColor[cidx]);
      hgt += mScrollY;
      if (hgt > mFullSize.h) then hgt := mFullSize.h;
      hgt := sbhgt*hgt div mFullSize.h;
      if (hgt > 0) then
      begin
        setScissor(mWidth-mFrameWidth+1, 7, mFrameWidth-3, sbhgt);
        darkenRect(gx+mWidth-mFrameWidth+1, gy+7+hgt, mFrameWidth-3, sbhgt, 128);
      end;
    end;
    // frame icon
    setScissor(mFrameWidth, 0, 3*8, 8);
    fillRect(gx+mFrameWidth, gy, 3*8, 8, mBackColor[cidx]);
    drawText8(gx+mFrameWidth, gy, '[ ]', mFrameColor[cidx]);
    if mInClose then drawText8(gx+mFrameWidth+7, gy, '#', mFrameIconColor[cidx])
    else drawText8(gx+mFrameWidth+7, gy, '*', mFrameIconColor[cidx]);
  end;
  // title
  if (Length(mTitle) > 0) then
  begin
    setScissor(mFrameWidth+3*8, 0, mWidth-mFrameWidth*2-3*8, 8);
    tx := (gx+3*8)+((mWidth-3*8)-Length(mTitle)*8) div 2;
    fillRect(tx-3, gy, Length(mTitle)*8+3+2, 8, mBackColor[cidx]);
    drawText8(tx, gy, mTitle, mFrameTextColor[cidx]);
  end;
  // shadow
  inherited drawControlPost(gx, gy);
end;


procedure TUITopWindow.activated ();
begin
  if (mFocused = nil) or (mFocused = self) then
  begin
    mFocused := findFirstFocus();
  end;
  if (mFocused <> nil) and (mFocused <> self) then mFocused.activated();
  inherited;
end;


procedure TUITopWindow.blurred ();
begin
  mDragScroll := TXMode.None;
  mWaitingClose := false;
  mInClose := false;
  if (mFocused <> nil) and (mFocused <> self) then mFocused.blurred();
  inherited;
end;


procedure TUITopWindow.keyEvent (var ev: THKeyEvent);
begin
  inherited keyEvent(ev);
  if (ev.eaten) or (ev.cancelled) or (not enabled) {or (not getFocused)} then exit;
  if (ev = 'M-F3') then
  begin
    if (not assigned(closeRequestCB)) or (closeRequestCB(self)) then
    begin
      uiRemoveWindow(self);
    end;
    ev.eat();
    exit;
  end;
end;


procedure TUITopWindow.mouseEvent (var ev: THMouseEvent);
var
  lx, ly: Integer;
  hgt, sbhgt: Integer;
begin
  if (not enabled) then exit;
  if (mWidth < 1) or (mHeight < 1) then exit;

  if (mDragScroll = TXMode.Drag) then
  begin
    mX += ev.x-mDragStartX;
    mY += ev.y-mDragStartY;
    mDragStartX := ev.x;
    mDragStartY := ev.y;
    if (ev.release) and (ev.but = ev.Left) then mDragScroll := TXMode.None;
    ev.eat();
    exit;
  end;

  if (mDragScroll = TXMode.Scroll) then
  begin
    // check for vertical scrollbar
    ly := ev.y-mY;
    if (ly < 7) then
    begin
      mScrollY := 0;
    end
    else
    begin
      sbhgt := mHeight-mFrameHeight*2+2;
      hgt := mHeight-mFrameHeight*2;
      if (hgt > 0) and (mFullSize.h > hgt) then
      begin
        hgt := (mFullSize.h*(ly-7) div (sbhgt-1))-(mHeight-mFrameHeight*2);
        mScrollY := nmax(0, hgt);
        hgt := mHeight-mFrameHeight*2;
        if (mScrollY+hgt > mFullSize.h) then mScrollY := nmax(0, mFullSize.h-hgt);
      end;
    end;
    if (ev.release) and (ev.but = ev.Left) then mDragScroll := TXMode.None;
    ev.eat();
    exit;
  end;

  if toLocal(ev.x, ev.y, lx, ly) then
  begin
    if (ev.press) then
    begin
      if (ly < 8) then
      begin
        uiGrabCtl := self;
        if (lx >= mFrameWidth) and (lx < mFrameWidth+3*8) then
        begin
          //uiRemoveWindow(self);
          mWaitingClose := true;
          mInClose := true;
        end
        else
        begin
          mDragScroll := TXMode.Drag;
          mDragStartX := ev.x;
          mDragStartY := ev.y;
        end;
        ev.eat();
        exit;
      end;
      // check for vertical scrollbar
      if (lx >= mWidth-mFrameWidth+1) and (ly >= 7) and (ly < mHeight-mFrameHeight+1) then
      begin
        sbhgt := mHeight-mFrameHeight*2+2;
        hgt := mHeight-mFrameHeight*2;
        if (hgt > 0) and (mFullSize.h > hgt) then
        begin
          hgt := (mFullSize.h*(ly-7) div (sbhgt-1))-(mHeight-mFrameHeight*2);
          mScrollY := nmax(0, hgt);
          uiGrabCtl := self;
          mDragScroll := TXMode.Scroll;
          ev.eat();
          exit;
        end;
      end;
      // drag
      if (lx < mFrameWidth) or (lx >= mWidth-mFrameWidth) or (ly >= mHeight-mFrameHeight) then
      begin
        uiGrabCtl := self;
        mDragScroll := TXMode.Drag;
        mDragStartX := ev.x;
        mDragStartY := ev.y;
        ev.eat();
        exit;
      end;
    end;

    if (ev.release) then
    begin
      if mWaitingClose then
      begin
        if (lx >= mFrameWidth) and (lx < mFrameWidth+3*8) then
        begin
          if (not assigned(closeRequestCB)) or (closeRequestCB(self)) then
          begin
            uiRemoveWindow(self);
          end;
        end;
        mWaitingClose := false;
        mInClose := false;
        ev.eat();
        exit;
      end;
    end;

    if (ev.motion) then
    begin
      if mWaitingClose then
      begin
        mInClose := (lx >= mFrameWidth) and (lx < mFrameWidth+3*8);
        ev.eat();
        exit;
      end;
    end;

    inherited mouseEvent(ev);
  end
  else
  begin
    mInClose := false;
    if (not ev.motion) and (mWaitingClose) then begin ev.eat(); mWaitingClose := false; exit; end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TUIBox.Create (ahoriz: Boolean);
begin
  inherited Create();
  mHoriz := ahoriz;
end;


procedure TUIBox.AfterConstruction ();
begin
  inherited;
  mCanFocus := false;
  mHAlign := -1; // left
  mCtl4Style := 'box';
end;


procedure TUIBox.setCaption (const acap: AnsiString);
begin
  mCaption := acap;
  mDefSize := TLaySize.Create(Length(mCaption)*8+3, 8);
end;


procedure TUIBox.setHasFrame (v: Boolean);
begin
  mHasFrame := v;
  if (mHasFrame) then begin mFrameWidth := 8; mFrameHeight := 8; end else begin mFrameWidth := 0; mFrameHeight := 0; end;
end;


function TUIBox.parseProperty (const prname: AnsiString; par: TTextParser): Boolean;
begin
  if (parseOrientation(prname, par)) then begin result := true; exit; end;
  if (strEquCI1251(prname, 'frame')) then
  begin
    setHasFrame(parseBool(par));
    result := true;
    exit;
  end;
  if (strEquCI1251(prname, 'title')) or (strEquCI1251(prname, 'caption')) then
  begin
    setCaption(par.expectIdOrStr(true));
    result := true;
    exit;
  end;
  if (strEquCI1251(prname, 'textalign')) or (strEquCI1251(prname, 'text-align')) then
  begin
    mHAlign := parseHAlign(par);
    result := true;
    exit;
  end;
  if (strEquCI1251(prname, 'children')) then
  begin
    parseChildren(par);
    result := true;
    exit;
  end;
  result := inherited parseProperty(prname, par);
end;


procedure TUIBox.drawControl (gx, gy: Integer);
var
  cidx: Integer;
  xpos: Integer;
begin
  cidx := getColorIndex;
  fillRect(gx, gy, mWidth, mHeight, mBackColor[cidx]);
  if mHasFrame then
  begin
    // draw frame
    drawRectUI(gx+3, gy+3, mWidth-6, mHeight-6, mFrameColor[cidx]);
  end;
  // draw caption
  if (Length(mCaption) > 0) then
  begin
         if (mHAlign < 0) then xpos := 3
    else if (mHAlign > 0) then xpos := mWidth-mFrameWidth*2-Length(mCaption)*8
    else xpos := (mWidth-mFrameWidth*2-Length(mCaption)*8) div 2;
    xpos += gx+mFrameWidth;

    setScissor(mFrameWidth+1, 0, mWidth-mFrameWidth-2, 8);
    if mHasFrame then fillRect(xpos-3, gy, Length(mCaption)*8+4, 8, mBackColor[cidx]);
    drawText8(xpos, gy, mCaption, mFrameTextColor[cidx]);
  end;
end;


procedure TUIBox.mouseEvent (var ev: THMouseEvent);
var
  lx, ly: Integer;
begin
  inherited mouseEvent(ev);
  if (not ev.eaten) and (not ev.cancelled) and (enabled) and toLocal(ev.x, ev.y, lx, ly) then
  begin
    ev.eat();
  end;
end;


procedure TUIBox.keyEvent (var ev: THKeyEvent);
var
  dir: Integer = 0;
  cur, ctl: TUIControl;
begin
  inherited keyEvent(ev);
  if (ev.eaten) or (ev.cancelled) or (not ev.press) or (not enabled) or (not getActive) then exit;
  if (Length(mChildren) = 0) then exit;
       if (mHoriz) and (ev = 'Left') then dir := -1
  else if (mHoriz) and (ev = 'Right') then dir := 1
  else if (not mHoriz) and (ev = 'Up') then dir := -1
  else if (not mHoriz) and (ev = 'Down') then dir := 1;
  if (dir = 0) then exit;
  ev.eat();
  cur := topLevel.mFocused;
  while (cur <> nil) and (cur.mParent <> self) do cur := cur.mParent;
  //if (cur = nil) then writeln('CUR: nil') else writeln('CUR: ', cur.className, '#', cur.id);
  if (dir < 0) then ctl := findPrevFocus(cur, true) else ctl := findNextFocus(cur, true);
  //if (ctl = nil) then writeln('CTL: nil') else writeln('CTL: ', ctl.className, '#', ctl.id);
  if (ctl <> nil) and (ctl <> self) then
  begin
    ctl.focused := true;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TUIHBox.Create ();
begin
end;


procedure TUIHBox.AfterConstruction ();
begin
  inherited;
  mHoriz := true;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TUIVBox.Create ();
begin
end;


procedure TUIVBox.AfterConstruction ();
begin
  inherited;
  mHoriz := false;
  writeln('VBOX: ', canFocus, ':', enabled);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUISpan.AfterConstruction ();
begin
  inherited;
  mExpand := true;
  mCanFocus := false;
  mCtl4Style := 'span';
end;


function TUISpan.parseProperty (const prname: AnsiString; par: TTextParser): Boolean;
begin
  if (parseOrientation(prname, par)) then begin result := true; exit; end;
  result := inherited parseProperty(prname, par);
end;


procedure TUISpan.drawControl (gx, gy: Integer);
begin
end;


// ////////////////////////////////////////////////////////////////////// //
procedure TUILine.AfterConstruction ();
begin
  inherited;
  mCanFocus := false;
  mExpand := true;
  mCanFocus := false;
  mCtl4Style := 'line';
end;


function TUILine.parseProperty (const prname: AnsiString; par: TTextParser): Boolean;
begin
  if (parseOrientation(prname, par)) then begin result := true; exit; end;
  result := inherited parseProperty(prname, par);
end;


procedure TUILine.drawControl (gx, gy: Integer);
var
  cidx: Integer;
begin
  cidx := getColorIndex;
  if mHoriz then
  begin
    drawHLine(gx, gy+(mHeight div 2), mWidth, mTextColor[cidx]);
  end
  else
  begin
    drawVLine(gx+(mWidth div 2), gy, mHeight, mTextColor[cidx]);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUIHLine.AfterConstruction ();
begin
  inherited;
  mHoriz := true;
  mDefSize.h := 7;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUIVLine.AfterConstruction ();
begin
  inherited;
  mHoriz := false;
  mDefSize.w := 7;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUIStaticText.AfterConstruction ();
begin
  inherited;
  mCanFocus := false;
  mHAlign := -1;
  mVAlign := 0;
  mHoriz := true; // nobody cares
  mHeader := false;
  mLine := false;
  mDefSize.h := 8;
  mCtl4Style := 'static';
end;


procedure TUIStaticText.setText (const atext: AnsiString);
begin
  mText := atext;
  mDefSize := TLaySize.Create(Length(mText)*8, 8);
end;


function TUIStaticText.parseProperty (const prname: AnsiString; par: TTextParser): Boolean;
begin
  if (strEquCI1251(prname, 'title')) or (strEquCI1251(prname, 'caption')) or (strEquCI1251(prname, 'text')) then
  begin
    setText(par.expectIdOrStr(true));
    result := true;
    exit;
  end;
  if (strEquCI1251(prname, 'textalign')) or (strEquCI1251(prname, 'text-align')) then
  begin
    parseTextAlign(par, mHAlign, mVAlign);
    result := true;
    exit;
  end;
  if (strEquCI1251(prname, 'header')) then
  begin
    mHeader := true;
    result := true;
    exit;
  end;
  if (strEquCI1251(prname, 'line')) then
  begin
    mLine := true;
    result := true;
    exit;
  end;
  result := inherited parseProperty(prname, par);
end;


procedure TUIStaticText.drawControl (gx, gy: Integer);
var
  xpos, ypos: Integer;
  cidx: Integer;
  clr: TGxRGBA;
begin
  cidx := getColorIndex;
  fillRect(gx, gy, mWidth, mHeight, mBackColor[cidx]);

       if (mHAlign < 0) then xpos := 0
  else if (mHAlign > 0) then xpos := mWidth-Length(mText)*8
  else xpos := (mWidth-Length(mText)*8) div 2;

  if (Length(mText) > 0) then
  begin
    if (mHeader) then clr := mFrameTextColor[cidx] else clr := mTextColor[cidx];

         if (mVAlign < 0) then ypos := 0
    else if (mVAlign > 0) then ypos := mHeight-8
    else ypos := (mHeight-8) div 2;

    drawText8(gx+xpos, gy+ypos, mText, clr);
  end;

  if (mLine) then
  begin
    if (mHeader) then clr := mFrameColor[cidx] else clr := mTextColor[cidx];

         if (mVAlign < 0) then ypos := 0
    else if (mVAlign > 0) then ypos := mHeight-1
    else ypos := (mHeight div 2);
    ypos += gy;

    if (Length(mText) = 0) then
    begin
      drawHLine(gx, ypos, mWidth, clr);
    end
    else
    begin
      drawHLine(gx, ypos, xpos-1, clr);
      drawHLine(gx+xpos+Length(mText)*8, ypos, mWidth, clr);
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUITextLabel.AfterConstruction ();
begin
  inherited;
  mHAlign := -1;
  mVAlign := 0;
  mCanFocus := false;
  mDefSize := TLaySize.Create(Length(mText)*8, 8);
  mCtl4Style := 'label';
  mLinkId := '';
end;


procedure TUITextLabel.cacheStyle (root: TUIStyle);
begin
  inherited cacheStyle(root);
  // active
  mHotColor[ClrIdxActive] := root.get('hot-color', 'active', mCtl4Style).asRGBADef(TGxRGBA.Create(0, 128, 0));
  // disabled
  mHotColor[ClrIdxDisabled] := root.get('hot-color', 'disabled', mCtl4Style).asRGBADef(TGxRGBA.Create(0, 64, 0));
  // inactive
  mHotColor[ClrIdxInactive] := root.get('hot-color', 'inactive', mCtl4Style).asRGBADef(TGxRGBA.Create(0, 64, 0));
end;


procedure TUITextLabel.setText (const s: AnsiString);
var
  f: Integer;
begin
  mText := '';
  mHotChar := #0;
  mHotOfs := 0;
  f := 1;
  while (f <= Length(s)) do
  begin
    if (s[f] = '\\') then
    begin
      Inc(f);
      if (f <= Length(s)) then mText += s[f];
      Inc(f);
    end
    else if (s[f] = '~') then
    begin
      Inc(f);
      if (f <= Length(s)) then
      begin
        if (mHotChar = #0) then
        begin
          mHotChar := s[f];
          mHotOfs := Length(mText)*8;
        end;
        mText += s[f];
      end;
      Inc(f);
    end
    else
    begin
      mText += s[f];
      Inc(f);
    end;
  end;
  mDefSize := TLaySize.Create(Length(mText)*8, 8);
end;


function TUITextLabel.parseProperty (const prname: AnsiString; par: TTextParser): Boolean;
begin
  if (strEquCI1251(prname, 'title')) or (strEquCI1251(prname, 'caption')) or (strEquCI1251(prname, 'text')) then
  begin
    setText(par.expectIdOrStr(true));
    result := true;
    exit;
  end;
  if (strEquCI1251(prname, 'link')) then
  begin
    mLinkId := par.expectIdOrStr(true);
    result := true;
    exit;
  end;
  if (strEquCI1251(prname, 'textalign')) or (strEquCI1251(prname, 'text-align')) then
  begin
    parseTextAlign(par, mHAlign, mVAlign);
    result := true;
    exit;
  end;
  result := inherited parseProperty(prname, par);
end;


procedure TUITextLabel.drawControl (gx, gy: Integer);
var
  xpos, ypos: Integer;
  cidx: Integer;
begin
  cidx := getColorIndex;
  fillRect(gx, gy, mWidth, mHeight, mBackColor[cidx]);
  if (Length(mText) > 0) then
  begin
         if (mHAlign < 0) then xpos := 0
    else if (mHAlign > 0) then xpos := mWidth-Length(mText)*8
    else xpos := (mWidth-Length(mText)*8) div 2;

         if (mVAlign < 0) then ypos := 0
    else if (mVAlign > 0) then ypos := mHeight-8
    else ypos := (mHeight-8) div 2;

    drawText8(gx+xpos, gy+ypos, mText, mTextColor[cidx]);

    if (Length(mLinkId) > 0) and (mHotChar <> #0) and (mHotChar <> ' ') then
    begin
      drawText8(gx+xpos+8+mHotOfs, gy+ypos, mHotChar, mHotColor[cidx]);
    end;
  end;
end;


procedure TUITextLabel.mouseEvent (var ev: THMouseEvent);
var
  lx, ly: Integer;
begin
  inherited mouseEvent(ev);
  if (not ev.eaten) and (not ev.cancelled) and (enabled) and toLocal(ev.x, ev.y, lx, ly) then
  begin
    ev.eat();
  end;
end;


procedure TUITextLabel.doAction ();
var
  ctl: TUIControl;
begin
  if (assigned(actionCB)) then
  begin
    actionCB(self);
  end
  else
  begin
    ctl := topLevel[mLinkId];
    if (ctl <> nil) then
    begin
      if (ctl.canFocus) then ctl.focused := true;
    end;
  end;
end;


procedure TUITextLabel.keyEventPost (var ev: THKeyEvent);
begin
  if (not enabled) then exit;
  if (mHotChar = #0) then exit;
  if (ev.eaten) or (ev.cancelled) or (not ev.press) then exit;
  if (ev.kstate <> ev.ModAlt) then exit;
  if (not ev.isHot(mHotChar)) then exit;
  ev.eat();
  if (canFocus) then focused := true;
  doAction();
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUIButton.AfterConstruction ();
begin
  inherited;
  mHAlign := -1;
  mVAlign := 0;
  mCanFocus := true;
  mDefSize := TLaySize.Create(Length(mText)*8+8, 10);
  mCtl4Style := 'button';
end;


procedure TUIButton.setText (const s: AnsiString);
begin
  inherited setText(s);
  mDefSize := TLaySize.Create(Length(mText)*8+8*2, 10);
end;


procedure TUIButton.drawControl (gx, gy: Integer);
var
  xpos, ypos: Integer;
  cidx: Integer;
begin
  cidx := getColorIndex;

  fillRect(gx+1, gy, mWidth-2, mHeight, mBackColor[cidx]);
  fillRect(gx, gy+1, 1, mHeight-2, mBackColor[cidx]);
  fillRect(gx+mWidth-1, gy+1, 1, mHeight-2, mBackColor[cidx]);

  if (Length(mText) > 0) then
  begin
         if (mHAlign < 0) then xpos := 0
    else if (mHAlign > 0) then xpos := mWidth-Length(mText)*8
    else xpos := (mWidth-Length(mText)*8) div 2;

         if (mVAlign < 0) then ypos := 0
    else if (mVAlign > 0) then ypos := mHeight-8
    else ypos := (mHeight-8) div 2;

    setScissor(8, 0, mWidth-16, mHeight);
    drawText8(gx+xpos+8, gy+ypos, mText, mTextColor[cidx]);

    if (mHotChar <> #0) and (mHotChar <> ' ') then drawText8(gx+xpos+8+mHotOfs, gy+ypos, mHotChar, mHotColor[cidx]);
  end;
end;


procedure TUIButton.mouseEvent (var ev: THMouseEvent);
var
  lx, ly: Integer;
begin
  inherited mouseEvent(ev);
  if (uiGrabCtl = self) then
  begin
    ev.eat();
    if (ev = '-lmb') and focused and toLocal(ev.x, ev.y, lx, ly) then
    begin
      doAction();
    end;
    exit;
  end;
  if (ev.eaten) or (ev.cancelled) or (not enabled) or not focused then exit;
  ev.eat();
end;


procedure TUIButton.keyEvent (var ev: THKeyEvent);
begin
  inherited keyEvent(ev);
  if (not ev.eaten) and (not ev.cancelled) and (enabled) then
  begin
    if (ev = 'Enter') or (ev = 'Space') then
    begin
      ev.eat();
      doAction();
      exit;
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUISwitchBox.AfterConstruction ();
begin
  inherited;
  mHAlign := -1;
  mVAlign := 0;
  mCanFocus := true;
  mDefSize := TLaySize.Create(Length(mText)*8+8*3, 8);
  mCtl4Style := 'switchbox';
  mChecked := false;
  mBoolVar := @mChecked;
end;


procedure TUISwitchBox.cacheStyle (root: TUIStyle);
begin
  inherited cacheStyle(root);
  // active
  mSwitchColor[ClrIdxActive] := root.get('switch-color', 'active', mCtl4Style).asRGBADef(TGxRGBA.Create(255, 255, 255));
  // disabled
  mSwitchColor[ClrIdxDisabled] := root.get('switch-color', 'disabled', mCtl4Style).asRGBADef(TGxRGBA.Create(255, 255, 255));
  // inactive
  mSwitchColor[ClrIdxInactive] := root.get('switch-color', 'inactive', mCtl4Style).asRGBADef(TGxRGBA.Create(255, 255, 255));
end;


procedure TUISwitchBox.setText (const s: AnsiString);
begin
  inherited setText(s);
  mDefSize := TLaySize.Create(Length(mText)*8+8*3, 8);
end;


function TUISwitchBox.parseProperty (const prname: AnsiString; par: TTextParser): Boolean;
begin
  if (strEquCI1251(prname, 'checked')) then
  begin
    result := true;
    setChecked(true);
    exit;
  end;
  result := inherited parseProperty(prname, par);
end;


function TUISwitchBox.getChecked (): Boolean;
begin
  if (mBoolVar <> nil) then result := mBoolVar^ else result := false;
end;


procedure TUISwitchBox.setVar (pvar: PBoolean);
begin
  if (pvar = nil) then pvar := @mChecked;
  if (pvar <> mBoolVar) then
  begin
    mBoolVar := pvar;
    setChecked(mBoolVar^);
  end;
end;


procedure TUISwitchBox.drawControl (gx, gy: Integer);
var
  xpos, ypos: Integer;
  cidx: Integer;
begin
  cidx := getColorIndex;

       if (mHAlign < 0) then xpos := 0
  else if (mHAlign > 0) then xpos := mWidth-(Length(mText)+4)*8
  else xpos := (mWidth-(Length(mText)+4)*8) div 2;

       if (mVAlign < 0) then ypos := 0
  else if (mVAlign > 0) then ypos := mHeight-8
  else ypos := (mHeight-8) div 2;


  fillRect(gx, gy, mWidth, mHeight, mBackColor[cidx]);

  if (checked) then
  begin
    if (Length(mCheckedStr) <> 3) or (mCheckedStr[2] <> '*') then
    begin
      drawText8(gx+xpos, gy+ypos, mCheckedStr, mSwitchColor[cidx]);
    end
    else
    begin
      drawText8(gx+xpos, gy+ypos, mCheckedStr[1], mSwitchColor[cidx]);
      drawText8(gx+xpos+2*8, gy+ypos, mCheckedStr[3], mSwitchColor[cidx]);
      drawText8(gx+xpos+7, gy+ypos, '*', mSwitchColor[cidx]);
    end;
  end
  else
  begin
    drawText8(gx+xpos, gy+ypos, mUncheckedStr, mSwitchColor[cidx]);
  end;

  drawText8(gx+xpos+8*3, gy+ypos, mText, mTextColor[cidx]);

  if (mHotChar <> #0) and (mHotChar <> ' ') then drawText8(gx+xpos+8*3+mHotOfs, gy+ypos, mHotChar, mHotColor[cidx]);
end;


procedure TUISwitchBox.mouseEvent (var ev: THMouseEvent);
var
  lx, ly: Integer;
begin
  inherited mouseEvent(ev);
  if (uiGrabCtl = self) then
  begin
    ev.eat();
    if (ev = '-lmb') and focused and toLocal(ev.x, ev.y, lx, ly) then
    begin
      doAction();
    end;
    exit;
  end;
  if (ev.eaten) or (ev.cancelled) or (not enabled) or not focused then exit;
  ev.eat();
end;


procedure TUISwitchBox.keyEvent (var ev: THKeyEvent);
begin
  inherited keyEvent(ev);
  if (not ev.eaten) and (not ev.cancelled) and (enabled) then
  begin
    if (ev = 'Space') then
    begin
      ev.eat();
      doAction();
      exit;
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUICheckBox.AfterConstruction ();
begin
  inherited;
  mChecked := false;
  mBoolVar := @mChecked;
  mCheckedStr := '[x]';
  mUncheckedStr := '[ ]';
end;


procedure TUICheckBox.setChecked (v: Boolean);
begin
  mBoolVar^ := v;
end;


procedure TUICheckBox.doAction ();
begin
  if (assigned(actionCB)) then
  begin
    actionCB(self);
  end
  else
  begin
    setChecked(not getChecked);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUIRadioBox.AfterConstruction ();
begin
  inherited;
  mChecked := false;
  mBoolVar := @mChecked;
  mCheckedStr := '(*)';
  mUncheckedStr := '( )';
  mRadioGroup := '';
end;


function TUIRadioBox.parseProperty (const prname: AnsiString; par: TTextParser): Boolean;
begin
  if (strEquCI1251(prname, 'group')) then
  begin
    mRadioGroup := par.expectIdOrStr(true);
    if (getChecked) then setChecked(true);
    result := true;
    exit;
  end;
  if (strEquCI1251(prname, 'checked')) then
  begin
    result := true;
    setChecked(true);
    exit;
  end;
  result := inherited parseProperty(prname, par);
end;


procedure TUIRadioBox.setChecked (v: Boolean);

  function resetGroup (ctl: TUIControl): Boolean;
  begin
    result := false;
    if (ctl <> self) and (ctl is TUIRadioBox) and (TUIRadioBox(ctl).mRadioGroup = mRadioGroup) then
    begin
      TUIRadioBox(ctl).mBoolVar^ := false;
    end;
  end;

begin
  mBoolVar^ := v;
  if v then topLevel.forEachControl(resetGroup);
end;


procedure TUIRadioBox.doAction ();
begin
  if (assigned(actionCB)) then
  begin
    actionCB(self);
  end
  else
  begin
    setChecked(true);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
initialization
  registerCtlClass(TUIHBox, 'hbox');
  registerCtlClass(TUIVBox, 'vbox');
  registerCtlClass(TUISpan, 'span');
  registerCtlClass(TUIHLine, 'hline');
  registerCtlClass(TUIVLine, 'vline');
  registerCtlClass(TUITextLabel, 'label');
  registerCtlClass(TUIStaticText, 'static');
  registerCtlClass(TUIButton, 'button');
  registerCtlClass(TUICheckBox, 'checkbox');
  registerCtlClass(TUIRadioBox, 'radiobox');
end.
