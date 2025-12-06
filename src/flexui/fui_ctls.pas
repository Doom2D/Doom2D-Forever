(* coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
 * Understanding is not required. Only obedience.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation, version 3 of
 * the License ONLY.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 *)

{$INCLUDE ../shared/a_modes.inc}
{$M+}
unit fui_ctls;

interface

uses
  SysUtils, Classes,
  SDL2,
  sdlcarcass,
  fui_common, fui_events, fui_style,
  fui_gfx_gl,
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
    mStyleLoaded: Boolean;
    mCtl4Style: AnsiString;
    mBackColor: array[0..ClrIdxMax] of TGxRGBA;
    mTextColor: array[0..ClrIdxMax] of TGxRGBA;
    mFrameColor: array[0..ClrIdxMax] of TGxRGBA;
    mFrameTextColor: array[0..ClrIdxMax] of TGxRGBA;
    mFrameIconColor: array[0..ClrIdxMax] of TGxRGBA;
    mSBarFullColor: array[0..ClrIdxMax] of TGxRGBA;
    mSBarEmptyColor: array[0..ClrIdxMax] of TGxRGBA;
    mDarken: array[0..ClrIdxMax] of Integer; // >255: none

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

    procedure drawFrame (gx, gy, resx, thalign: Integer; const text: AnsiString; dbl: Boolean);

  protected
    var savedClip: TGxRect; // valid only in `draw*()` calls
    //WARNING! do not call scissor functions outside `.draw*()` API!
    // set scissor to this rect (in local coords)
    procedure setScissor (lx, ly, lw, lh: Integer); // valid only in `draw*()` calls
    procedure resetScissor (); inline; // only client area, w/o frame
    procedure resetScissorNC (); inline; // full drawing area, with frame

  public
    actionCB: TActionCB;
    closeRequestCB: TCloseRequestCB;

  private
    mDefSize: TLaySize; // default size
    mMaxSize: TLaySize; // maximum size
    mFlex: Integer;
    mHoriz: Boolean;
    mHGroup: AnsiString;
    mVGroup: AnsiString;
    mAlign: Integer;
    mExpand: Boolean;
    mLayDefSize: TLaySize;
    mLayMaxSize: TLaySize;
    mFullSize: TLaySize;
    mNoPad: Boolean;
    mPadding: TLaySize;

  public
    // layouter interface
    function getDefSize (): TLaySize; inline; // default size; <0: use max size
    //procedure setDefSize (const sz: TLaySize); inline; // default size; <0: use max size
    function getMargins (): TLayMargins; inline;
    function getPadding (): TLaySize; inline; // children padding (each non-first child will get this on left/top)
    function getMaxSize (): TLaySize; inline; // max size; <0: set to some huge value
    //procedure setMaxSize (const sz: TLaySize); inline; // max size; <0: set to some huge value
    function getFlex (): Integer; inline; // <=0: not flexible
    function isHorizBox (): Boolean; inline; // horizontal layout for children?
    function noPad (): Boolean; inline; // ignore padding in box direction for this control
    function getAlign (): Integer; inline; // aligning in non-main direction: <0: left/up; 0: center; >0: right/down
    function getExpand (): Boolean; inline; // expanding in non-main direction: `true` will ignore align and eat all available space
    function getHGroup (): AnsiString; inline; // empty: not grouped
    function getVGroup (): AnsiString; inline; // empty: not grouped

    procedure setActualSizePos (constref apos: TLayPos; constref asize: TLaySize); inline;

    procedure layPrepare (); virtual; // called before registering control in layouter

  public
    property flex: Integer read mFlex write mFlex;
    property flDefaultSize: TLaySize read mDefSize write mDefSize;
    property flMaxSize: TLaySize read mMaxSize write mMaxSize;
    property flPadding: TLaySize read mPadding write mPadding;
    property flHoriz: Boolean read mHoriz write mHoriz;
    property flAlign: Integer read mAlign write mAlign;
    property flExpand: Boolean read mExpand write mExpand;
    property flHGroup: AnsiString read mHGroup write mHGroup;
    property flVGroup: AnsiString read mVGroup write mVGroup;
    property flNoPad: Boolean read mNoPad write mNoPad;
    property fullSize: TLaySize read mFullSize;

  protected
    function parsePos (par: TTextParser): TLayPos;
    function parseSize (par: TTextParser): TLaySize;
    function parsePadding (par: TTextParser): TLaySize;
    function parseHPadding (par: TTextParser; def: Integer): TLaySize;
    function parseVPadding (par: TTextParser; def: Integer): TLaySize;
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

    procedure onEvent (var ev: TFUIEvent); virtual; // general dispatcher

    procedure mouseEvent (var ev: TFUIEvent); virtual;
    procedure mouseEventSink (var ev: TFUIEvent); virtual;
    procedure mouseEventBubble (var ev: TFUIEvent); virtual;

    procedure keyEvent (var ev: TFUIEvent); virtual;
    procedure keyEventSink (var ev: TFUIEvent); virtual;
    procedure keyEventBubble (var ev: TFUIEvent); virtual;

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
    property id: AnsiString read mId write mId;
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
    type TXMode = (None, Drag, VScroll, HScroll);

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

    procedure keyEventBubble (var ev: TFUIEvent); override; // returns `true` if event was eaten
    procedure mouseEvent (var ev: TFUIEvent); override; // returns `true` if event was eaten

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

    procedure mouseEvent (var ev: TFUIEvent); override;
    procedure keyEvent (var ev: TFUIEvent); override;

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
  end;

  // ////////////////////////////////////////////////////////////////////// //
  TUILine = class(TUIControl)
  public
    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser

    function parseProperty (const prname: AnsiString; par: TTextParser): Boolean; override;

    procedure layPrepare (); override; // called before registering control in layouter

    procedure drawControl (gx, gy: Integer); override;
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

    procedure mouseEvent (var ev: TFUIEvent); override;
    procedure keyEventBubble (var ev: TFUIEvent); override;

  public
    property text: AnsiString read mText write setText;
    property halign: Integer read mHAlign write mHAlign;
    property valign: Integer read mVAlign write mVAlign;
  end;

  // ////////////////////////////////////////////////////////////////////// //
  TUIButton = class(TUITextLabel)
  protected
    mSkipLayPrepare: Boolean;
    mShadowSize: Integer;
    mAddMarkers: Boolean;
    mHideMarkers: Boolean;
    mPushed: Boolean;

  protected
    procedure setText (const s: AnsiString); override;

    procedure cacheStyle (root: TUIStyle); override;

    procedure blurred (); override;

  public
    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser

    procedure layPrepare (); override; // called before registering control in layouter

    procedure drawControl (gx, gy: Integer); override;

    procedure mouseEvent (var ev: TFUIEvent); override;
    procedure keyEvent (var ev: TFUIEvent); override;
  end;

  // ////////////////////////////////////////////////////////////////////// //
  TUIButtonRound = class(TUIButton)
  protected
    procedure setText (const s: AnsiString); override;

  public
    procedure AfterConstruction (); override; // so it will be correctly initialized when created from parser

    procedure layPrepare (); override; // called before registering control in layouter

    procedure drawControl (gx, gy: Integer); override;
  end;

  // ////////////////////////////////////////////////////////////////////// //
  TUISwitchBox = class(TUITextLabel)
  protected
    mBoolVar: PBoolean;
    mChecked: Boolean;
    mIcon: TGxContext.TMarkIcon;
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

    procedure mouseEvent (var ev: TFUIEvent); override;
    procedure keyEvent (var ev: TFUIEvent); override;

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
procedure uiDispatchEvent (var evt: TFUIEvent);
procedure uiDraw ();

procedure uiFocus ();
procedure uiBlur ();


// ////////////////////////////////////////////////////////////////////////// //
procedure uiAddWindow (ctl: TUIControl);
procedure uiRemoveWindow (ctl: TUIControl); // will free window if `mFreeOnClose` is `true`
function uiVisibleWindow (ctl: TUIControl): Boolean;

// this can return `nil` or disabled control
function uiGetFocusedCtl (): TUIControl;

procedure uiUpdateStyles ();


// ////////////////////////////////////////////////////////////////////////// //
// do layouting
procedure uiLayoutCtl (ctl: TUIControl);


// ////////////////////////////////////////////////////////////////////////// //
procedure uiInitialize ();
procedure uiDeinitialize ();


// ////////////////////////////////////////////////////////////////////////// //
var
  fuiRenderScale: Single = 1.0;
  uiContext: TGxContext;


implementation

uses
  fui_flexlay,
  utils;


var
  uiInsideDispatcher: Boolean;
  uiTopList: array of TUIControl;
  uiGrabCtl: TUIControl;


// ////////////////////////////////////////////////////////////////////////// //
procedure uiDeinitialize ();
begin
  FreeAndNil(uiContext);
end;


procedure uiInitialize ();
begin
  if (uiContext <> nil) then raise Exception.Create('FlexUI already initialized');
  uiContext := TGxContext.Create();
end;


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
    if (uiGrabCtl <> nil) and (ctl.isMyChild(uiGrabCtl)) then uiGrabCtl := nil; // just in case
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
    if (not ctl.mStyleLoaded) then ctl.updateStyle();
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
procedure uiUpdateStyles ();
var
  ctl: TUIControl;
begin
  for ctl in uiTopList do ctl.updateStyle();
end;


procedure uiDispatchEvent (var evt: TFUIEvent);
var
  ev: TFUIEvent;
  destCtl: TUIControl;

  procedure doSink (ctl: TUIControl);
  begin
    if (ctl = nil) or (not ev.alive) then exit;
    if (ctl.mParent <> nil) then
    begin
      doSink(ctl.mParent);
      if (not ev.alive) then exit;
    end;
    //if (ctl = destCtl) then writeln(' SINK: MINE! <', ctl.className, '>');
    ev.setSinking();
    ctl.onEvent(ev);
    if (ctl = destCtl) and (ev.alive) then
    begin
      ev.setMine();
      ctl.onEvent(ev);
    end;
  end;

  procedure dispatchTo (ctl: TUIControl);
  begin
    if (ctl = nil) then exit;
    destCtl := ctl;
    // sink
    doSink(ctl);
    // bubble
    //ctl := ctl.mParent; // 'cause "mine" is processed in `doSink()`
    while (ctl <> nil) and (ev.alive) do
    begin
      ev.setBubbling();
      ctl.onEvent(ev);
      ctl := ctl.mParent;
    end;
  end;

  procedure doMouseEvent ();
  var
    doUngrab: Boolean;
    ctl: TUIControl;
    win: TUIControl;
    lx, ly: Integer;
    f, c: Integer;
  begin
    // pass mouse events to control with grab, if there is any
    if (uiGrabCtl <> nil) then
    begin
      //writeln('GRABBED: ', uiGrabCtl.className);
      doUngrab := (ev.release) and ((ev.bstate and (not ev.but)) = 0);
      dispatchTo(uiGrabCtl);
      //FIXME: create API to get grabs, so control can regrab itself event on release
      if (doUngrab) and (uiGrabCtl = destCtl) then uiGrabCtl := nil;
      ev.eat();
      exit;
    end;
    // get top window
    if (Length(uiTopList) > 0) then win := uiTopList[High(uiTopList)] else win := nil;
    // check if we're still in top window
    if (ev.press) and (win <> nil) and (not win.toLocal(0, 0, lx, ly)) then
    begin
      // we have other windows too; check for window switching
      for f := High(uiTopList)-1 downto 0 do
      begin
        if (uiTopList[f].enabled) and (uiTopList[f].toLocal(ev.x, ev.y, lx, ly)) then
        begin
          // switch
          win.blurred();
          win := uiTopList[f];
          for c := f+1 to High(uiTopList) do uiTopList[c-1] := uiTopList[c];
          uiTopList[High(uiTopList)] := win;
          win.activated();
          break;
        end;
      end;
    end;
    // dispatch event
    if (win <> nil) and (win.toLocal(ev.x, ev.y, lx, ly)) then
    begin
      ctl := win.controlAtXY(ev.x, ev.y); // don't allow disabled controls
      if (ctl = nil) or (not ctl.canFocus) or (not ctl.enabled) then ctl := win;
      // pass focus to another event and set grab, if necessary
      if (ev.press) then
      begin
        // pass focus, if necessary
        if (win.mFocused <> ctl) then
        begin
          if (win.mFocused <> nil) then win.mFocused.blurred();
          uiGrabCtl := ctl;
          win.mFocused := ctl;
          if (ctl <> win) then ctl.activated();
        end
        else
        begin
          uiGrabCtl := ctl;
        end;
      end;
      dispatchTo(ctl);
    end;
  end;

var
  svx, svy, svdx, svdy: Integer;
  svscale: Single;
  odp: Boolean;
begin
  processKills();
  if (not evt.alive) then exit;
  odp := uiInsideDispatcher;
  uiInsideDispatcher := true;
  //writeln('ENTER: FUI DISPATCH');
  ev := evt;
  // normalize mouse coordinates
  svscale := fuiRenderScale;
  ev.x := trunc(ev.x/svscale);
  ev.y := trunc(ev.y/svscale);
  ev.dx := trunc(ev.dx/svscale); //FIXME
  ev.dy := trunc(ev.dy/svscale); //FIXME
  svx := ev.x;
  svy := ev.y;
  svdx := ev.dx;
  svdy := ev.dy;
  try
    // "event grab" eats only mouse events
    if (ev.mouse) then
    begin
      // we need to so some special processing here
      doMouseEvent();
    end
    else
    begin
      // simply dispatch to focused control
      dispatchTo(uiGetFocusedCtl);
    end;
  finally
    uiInsideDispatcher := odp;
    if (ev.x = svx) and (ev.y = svy) and (ev.dx = svdx) and (ev.dy = svdy) then
    begin
      // due to possible precision loss
      svx := evt.x;
      svy := evt.y;
      svdx := evt.dx;
      svdy := evt.dy;
      evt := ev;
      evt.x := svx;
      evt.y := svy;
      evt.dx := svdx;
      evt.dy := svdy;
    end
    else
    begin
      // scale back
      evt := ev;
      evt.x := trunc(evt.x*svscale);
      evt.y := trunc(evt.y*svscale);
      evt.dx := trunc(evt.dx*svscale);
      evt.dy := trunc(evt.dy*svscale);
    end;
  end;
  processKills();
  //writeln('EXIT: FUI DISPATCH');
end;

procedure uiFocus ();
begin
  if (Length(uiTopList) > 0) and (uiTopList[High(uiTopList)].enabled) then uiTopList[High(uiTopList)].activated();
end;


procedure uiBlur ();
begin
  if (Length(uiTopList) > 0) and (uiTopList[High(uiTopList)].enabled) then uiTopList[High(uiTopList)].blurred();
end;


procedure uiDraw ();
var
  f, cidx: Integer;
  ctl: TUIControl;
begin
  processKills();
  //if (uiContext = nil) then uiContext := TGxContext.Create();
  gxSetContext(uiContext, fuiRenderScale);
  uiContext.resetClip();
  try
    for f := 0 to High(uiTopList) do
    begin
      ctl := uiTopList[f];
      ctl.draw();
      if (f <> High(uiTopList)) then
      begin
        cidx := ctl.getColorIndex;
        uiContext.darkenRect(ctl.x0, ctl.y0, ctl.width, ctl.height, ctl.mDarken[cidx]);
      end;
    end;
  finally
    gxSetContext(nil);
  end;
end;


function uiGetFocusedCtl (): TUIControl;
begin
  result := nil;
  if (Length(uiTopList) > 0) and (uiTopList[High(uiTopList)].enabled) then
  begin
    result := uiTopList[High(uiTopList)].mFocused;
    if (result = nil) then result := uiTopList[High(uiTopList)];
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
        if (Length(uiTopList) > 0) and (uiTopList[High(uiTopList)].enabled) then uiTopList[High(uiTopList)].blurred();
        for c := f+1 to High(uiTopList) do uiTopList[c-1] := uiTopList[c];
        uiTopList[High(uiTopList)] := ctl;
        ctl.activated();
      end;
      exit;
    end;
  end;
  if (Length(uiTopList) > 0) and (uiTopList[High(uiTopList)].enabled) then uiTopList[High(uiTopList)].blurred();
  SetLength(uiTopList, Length(uiTopList)+1);
  uiTopList[High(uiTopList)] := ctl;
  if (not ctl.mStyleLoaded) then ctl.updateStyle();
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
      if (Length(uiTopList) > 0) and (uiTopList[High(uiTopList)].enabled) then uiTopList[High(uiTopList)].activated();
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
  mHeight := uiContext.charHeight(' ');
  mFrameWidth := 0;
  mFrameHeight := 0;
  mEnabled := true;
  mCanFocus := true;
  mChildren := nil;
  mFocused := nil;
  mEscClose := false;
  mDrawShadow := false;
  actionCB := nil;
  // layouter interface
  //mDefSize := TLaySize.Create(64, uiContext.charHeight(' ')); // default size
  mDefSize := TLaySize.Create(0, 0); // default size: hidden control
  mMaxSize := TLaySize.Create(-1, -1); // maximum size
  mPadding := TLaySize.Create(0, 0);
  mNoPad := false;
  mFlex := 0;
  mHoriz := true;
  mHGroup := '';
  mVGroup := '';
  mStyleId := '';
  mCtl4Style := '';
  mAlign := -1; // left/top
  mExpand := false;
  mStyleLoaded := false;
end;


destructor TUIControl.Destroy ();
var
  f, c: Integer;
  doActivateOtherWin: Boolean = false;
begin
  if (uiInsideDispatcher) then raise Exception.Create('FlexUI: cannot destroy objects in event dispatcher');
  if (uiGrabCtl = self) then uiGrabCtl := nil;
  // just in case, check if this is top-level shit
  for f := 0 to High(uiTopList) do
  begin
    if (uiTopList[f] = self) then
    begin
      if (uiGrabCtl <> nil) and (isMyChild(uiGrabCtl)) then uiGrabCtl := nil;
      for c := f+1 to High(uiTopList) do uiTopList[c-1] := uiTopList[c];
      SetLength(uiTopList, Length(uiTopList)-1);
      doActivateOtherWin := true;
      break;
    end;
  end;
  if (doActivateOtherWin) and (Length(uiTopList) > 0) and (uiTopList[High(uiTopList)].enabled) then
  begin
    uiTopList[High(uiTopList)].activated();
  end;
  // other checks
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
  mStyleLoaded := true;
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
  mSBarFullColor[ClrIdxActive] := root.get('scrollbar-full-color', 'active', cst).asRGBADef(TGxRGBA.Create(255, 255, 255));
  mSBarEmptyColor[ClrIdxActive] := root.get('scrollbar-empty-color', 'active', cst).asRGBADef(TGxRGBA.Create(128, 128, 128));
  mDarken[ClrIdxActive] := root.get('darken', 'active', cst).asInt(666);
  // disabled
  mBackColor[ClrIdxDisabled] := root.get('back-color', 'disabled', cst).asRGBADef(TGxRGBA.Create(0, 0, 128));
  mTextColor[ClrIdxDisabled] := root.get('text-color', 'disabled', cst).asRGBADef(TGxRGBA.Create(127, 127, 127));
  mFrameColor[ClrIdxDisabled] := root.get('frame-color', 'disabled', cst).asRGBADef(TGxRGBA.Create(127, 127, 127));
  mFrameTextColor[ClrIdxDisabled] := root.get('frame-text-color', 'disabled', cst).asRGBADef(TGxRGBA.Create(127, 127, 127));
  mFrameIconColor[ClrIdxDisabled] := root.get('frame-icon-color', 'disabled', cst).asRGBADef(TGxRGBA.Create(0, 127, 0));
  mSBarFullColor[ClrIdxDisabled] := root.get('scrollbar-full-color', 'disabled', cst).asRGBADef(TGxRGBA.Create(127, 127, 127));
  mSBarEmptyColor[ClrIdxDisabled] := root.get('scrollbar-empty-color', 'disabled', cst).asRGBADef(TGxRGBA.Create(98, 98, 98));
  mDarken[ClrIdxDisabled] := root.get('darken', 'disabled', cst).asInt(666);
  // inactive
  mBackColor[ClrIdxInactive] := root.get('back-color', 'inactive', cst).asRGBADef(TGxRGBA.Create(0, 0, 128));
  mTextColor[ClrIdxInactive] := root.get('text-color', 'inactive', cst).asRGBADef(TGxRGBA.Create(255, 255, 255));
  mFrameColor[ClrIdxInactive] := root.get('frame-color', 'inactive', cst).asRGBADef(TGxRGBA.Create(255, 255, 255));
  mFrameTextColor[ClrIdxInactive] := root.get('frame-text-color', 'inactive', cst).asRGBADef(TGxRGBA.Create(255, 255, 255));
  mFrameIconColor[ClrIdxInactive] := root.get('frame-icon-color', 'inactive', cst).asRGBADef(TGxRGBA.Create(0, 255, 0));
  mSBarFullColor[ClrIdxInactive] := root.get('scrollbar-full-color', 'inactive', cst).asRGBADef(TGxRGBA.Create(255, 255, 255));
  mSBarEmptyColor[ClrIdxInactive] := root.get('scrollbar-empty-color', 'inactive', cst).asRGBADef(TGxRGBA.Create(128, 128, 128));
  mDarken[ClrIdxInactive] := root.get('darken', 'inactive', cst).asInt(666);
end;


// ////////////////////////////////////////////////////////////////////////// //
function TUIControl.getDefSize (): TLaySize; inline; begin result := mLayDefSize; end;
function TUIControl.getMaxSize (): TLaySize; inline; begin result := mLayMaxSize; end;
function TUIControl.getPadding (): TLaySize; inline; begin result := mPadding; end;
function TUIControl.getFlex (): Integer; inline; begin result := mFlex; end;
function TUIControl.isHorizBox (): Boolean; inline; begin result := mHoriz; end;
function TUIControl.noPad (): Boolean; inline; begin result := mNoPad; end;
function TUIControl.getAlign (): Integer; inline; begin result := mAlign; end;
function TUIControl.getExpand (): Boolean; inline; begin result := mExpand; end;
function TUIControl.getHGroup (): AnsiString; inline; begin result := mHGroup; end;
function TUIControl.getVGroup (): AnsiString; inline; begin result := mVGroup; end;
function TUIControl.getMargins (): TLayMargins; inline; begin result := TLayMargins.Create(mFrameHeight, mFrameWidth, mFrameHeight, mFrameWidth); end;

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
  if (mLayMaxSize.w >= 0) then mWidth := nmin(mWidth, mLayMaxSize.w);
  if (mLayMaxSize.h >= 0) then mHeight := nmin(mHeight, mLayMaxSize.h);
end;

procedure TUIControl.layPrepare ();
begin
  mLayDefSize := mDefSize;
  if (mLayDefSize.w <> 0) or (mLayDefSize.h <> 0) then
  begin
    mLayMaxSize := mMaxSize;
    if (mLayMaxSize.w >= 0) then begin mLayDefSize.w += mFrameWidth*2; mLayMaxSize.w += mFrameWidth*2; end;
    if (mLayMaxSize.h >= 0) then begin mLayDefSize.h += mFrameHeight*2; mLayMaxSize.h += mFrameHeight*2; end;
  end
  else
  begin
    mLayMaxSize := TLaySize.Create(0, 0);
  end;
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

function TUIControl.parsePadding (par: TTextParser): TLaySize;
begin
  result := parseSize(par);
end;

function TUIControl.parseHPadding (par: TTextParser; def: Integer): TLaySize;
begin
  if (par.isInt) then
  begin
    result.h := def;
    result.w := par.expectInt();
  end
  else
  begin
    result := parsePadding(par);
  end;
end;

function TUIControl.parseVPadding (par: TTextParser; def: Integer): TLaySize;
begin
  if (par.isInt) then
  begin
    result.w := def;
    result.h := par.expectInt();
  end
  else
  begin
    result := parsePadding(par);
  end;
end;

function TUIControl.parseBool (par: TTextParser): Boolean;
begin
  result :=
    par.eatIdOrStrCI('true') or
    par.eatIdOrStrCI('yes');
  if not result then
  begin
    if (not par.eatIdOrStrCI('false')) and (not par.eatIdOrStrCI('no')) then
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
  // padding
  if (strEquCI1251(prname, 'padding')) then begin mPadding := parsePadding(par); exit; end;
  if (strEquCI1251(prname, 'nopad')) then begin mNoPad := true; exit; end;
  // flags
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
      if (not intersectRect(gx, gy, wdt, hgt, cgx, cgy, mWidth, mHeight)) then
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
      result := forEachControl(@checkFocus);
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
      forEachControl(@checkFocus);
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
procedure TUIControl.setScissor (lx, ly, lw, lh: Integer);
var
  gx, gy, wdt, hgt, cgx, cgy: Integer;
begin
  if (not intersectRect(lx, ly, lw, lh, 0, 0, mWidth, mHeight)) then
  begin
    uiContext.clip := TGxRect.Create(0, 0, 0, 0);
    exit;
  end;

  getDrawRect(gx, gy, wdt, hgt);

  toGlobal(lx, ly, cgx, cgy);
  if (not intersectRect(gx, gy, wdt, hgt, cgx, cgy, lw, lh)) then
  begin
    uiContext.clip := TGxRect.Create(0, 0, 0, 0);
    exit;
  end;

  uiContext.clip := savedClip;
  uiContext.combineClip(TGxRect.Create(gx, gy, wdt, hgt));
  //uiContext.clip := TGxRect.Create(gx, gy, wdt, hgt);
end;

procedure TUIControl.resetScissorNC (); inline;
begin
  setScissor(0, 0, mWidth, mHeight);
end;

procedure TUIControl.resetScissor (); inline;
begin
  if ((mFrameWidth <= 0) and (mFrameHeight <= 0)) then
  begin
    resetScissorNC();
  end
  else
  begin
    setScissor(mFrameWidth, mFrameHeight, mWidth-mFrameWidth*2, mHeight-mFrameHeight*2);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUIControl.drawFrame (gx, gy, resx, thalign: Integer; const text: AnsiString; dbl: Boolean);
var
  cidx, tx, tw: Integer;
begin
  if (mFrameWidth < 1) or (mFrameHeight < 1) then exit;
  cidx := getColorIndex;
  uiContext.color := mFrameColor[cidx];
  case mFrameHeight of
    8:
      begin
        if dbl then
        begin
          uiContext.rect(gx+3, gy+3, mWidth-6, mHeight-6);
          uiContext.rect(gx+5, gy+5, mWidth-10, mHeight-10);
        end
        else
        begin
          uiContext.rect(gx+4, gy+4, mWidth-8, mHeight-8);
        end;
      end;
    14:
      begin
        if dbl then
        begin
          uiContext.rect(gx+3, gy+3+3, mWidth-6, mHeight-6-6);
          uiContext.rect(gx+5, gy+5+3, mWidth-10, mHeight-10-6);
        end
        else
        begin
          uiContext.rect(gx+4, gy+4+3, mWidth-8, mHeight-8-6);
        end;
      end;
    16:
      begin
        if dbl then
        begin
          uiContext.rect(gx+3, gy+3+4, mWidth-6, mHeight-6-8);
          uiContext.rect(gx+5, gy+5+4, mWidth-10, mHeight-10-8);
        end
        else
        begin
          uiContext.rect(gx+4, gy+4+4, mWidth-8, mHeight-8-8);
        end;
      end;
    else
      begin
        //TODO!
        if dbl then
        begin
        end
        else
        begin
        end;
      end;
  end;

  // title
  if (Length(text) > 0) then
  begin
    if (resx < 0) then resx := 0;
    tw := uiContext.textWidth(text);
    setScissor(mFrameWidth+resx, 0, mWidth-mFrameWidth*2-resx, mFrameHeight);
         if (thalign < 0) then tx := gx+resx+mFrameWidth+2
    else if (thalign > 0) then tx := gx+mWidth-mFrameWidth-1-tw
    else tx := (gx+resx+mFrameWidth)+(mWidth-mFrameWidth*2-resx-tw) div 2;
    uiContext.color := mBackColor[cidx];
    uiContext.fillRect(tx-2, gy, tw+4, mFrameHeight);
    uiContext.color := mFrameTextColor[cidx];
    uiContext.drawText(tx, gy, text);
  end;
end;


procedure TUIControl.draw ();
var
  f: Integer;
  gx, gy: Integer;

begin
  if (mWidth < 1) or (mHeight < 1) or (uiContext = nil) or (not uiContext.active) then exit;
  toGlobal(0, 0, gx, gy);

  savedClip := uiContext.clip;
  try
    resetScissorNC();
    drawControl(gx, gy);
    resetScissor();
    for f := 0 to High(mChildren) do mChildren[f].draw();
    resetScissorNC();
    drawControlPost(gx, gy);
  finally
    uiContext.clip := savedClip;
  end;
end;

procedure TUIControl.drawControl (gx, gy: Integer);
begin
end;

procedure TUIControl.drawControlPost (gx, gy: Integer);
begin
  // shadow for top-level controls
  if (mParent = nil) and (mDrawShadow) and (mWidth > 0) and (mHeight > 0) then
  begin
    uiContext.resetClip();
    uiContext.darkenRect(gx+mWidth, gy+8, 8, mHeight, 128);
    uiContext.darkenRect(gx+8, gy+mHeight, mWidth-8, 8, 128);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUIControl.onEvent (var ev: TFUIEvent);
begin
  if (not ev.alive) or (not enabled) then exit;
  //if (ev.mine) then writeln(' MINE: <', className, '>');
  if (ev.key) then
  begin
         if (ev.sinking) then keyEventSink(ev)
    else if (ev.bubbling) then keyEventBubble(ev)
    else if (ev.mine) then keyEvent(ev);
  end
  else if (ev.mouse) then
  begin
         if (ev.sinking) then mouseEventSink(ev)
    else if (ev.bubbling) then mouseEventBubble(ev)
    else if (ev.mine) then mouseEvent(ev);
  end;
end;


procedure TUIControl.mouseEventSink (var ev: TFUIEvent);
begin
end;

procedure TUIControl.mouseEventBubble (var ev: TFUIEvent);
begin
end;

procedure TUIControl.mouseEvent (var ev: TFUIEvent);
begin
end;


procedure TUIControl.keyEventSink (var ev: TFUIEvent);
var
  ctl: TUIControl;
begin
  if (not enabled) then exit;
  if (not ev.alive) then exit;
  // for top-level controls
  if (mParent <> nil) then exit;
  if (mEscClose) and (ev = 'Escape') then
  begin
    if (not assigned(closeRequestCB)) or (closeRequestCB(self)) then
    begin
      uiRemoveWindow(self);
    end;
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
end;

procedure TUIControl.keyEventBubble (var ev: TFUIEvent);
var
  ctl: TUIControl;
begin
  if (not enabled) then exit;
  if (not ev.alive) then exit;
  // for top-level controls
  if (mParent <> nil) then exit;
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
end;

procedure TUIControl.keyEvent (var ev: TFUIEvent);
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
  mFrameHeight := uiContext.charHeight(#184);
  if (mWidth < mFrameWidth*2+uiContext.iconWinWidth(TGxContext.TWinIcon.Close)) then mWidth := mFrameWidth*2+uiContext.iconWinWidth(TGxContext.TWinIcon.Close);
  if (mHeight < mFrameHeight*2) then mHeight := mFrameHeight*2;
  if (Length(mTitle) > 0) then
  begin
    if (mWidth < uiContext.textWidth(mTitle)+mFrameWidth*2+uiContext.iconWinWidth(TGxContext.TWinIcon.Close)) then
    begin
      mWidth := uiContext.textWidth(mTitle)+mFrameWidth*2+uiContext.iconWinWidth(TGxContext.TWinIcon.Close);
    end;
  end;
  mCanFocus := false;
  mDragScroll := TXMode.None;
  mDrawShadow := true;
  mWaitingClose := false;
  mInClose := false;
  closeCB := nil;
  mCtl4Style := 'window';
  mDefSize.w := nmax(1, mDefSize.w);
  mDefSize.h := nmax(1, mDefSize.h);
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
var
  nsz: TLaySize;
begin
  nsz := TLaySize.Create(trunc(fuiScrWdt/fuiRenderScale)-mFrameWidth*2-6, trunc(fuiScrHgt/fuiRenderScale)-mFrameHeight*2-6);
  if (mMaxSize.w < 1) then mMaxSize.w := nsz.w;
  if (mMaxSize.h < 1) then mMaxSize.h := nsz.h;
end;


procedure TUITopWindow.centerInScreen ();
begin
  if (mWidth > 0) and (mHeight > 0) then
  begin
    mX := trunc((fuiScrWdt/fuiRenderScale-mWidth)/2);
    mY := trunc((fuiScrHgt/fuiRenderScale-mHeight)/2);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUITopWindow.drawControl (gx, gy: Integer);
begin
  uiContext.color := mBackColor[getColorIndex];
  uiContext.fillRect(gx, gy, mWidth, mHeight);
end;

procedure TUITopWindow.drawControlPost (gx, gy: Integer);
var
  cidx, iwdt, ihgt: Integer;
  ybot, xend, vhgt, vwdt: Integer;
begin
  cidx := getColorIndex;
  iwdt := uiContext.iconWinWidth(TGxContext.TWinIcon.Close);
  if (mDragScroll = TXMode.Drag) then
  begin
    drawFrame(gx, gy, iwdt, 0, mTitle, false);
  end
  else
  begin
    ihgt := uiContext.iconWinHeight(TGxContext.TWinIcon.Close);
    drawFrame(gx, gy, iwdt, 0, mTitle, true);
    // vertical scroll bar
    vhgt := mHeight-mFrameHeight*2;
    if (mFullSize.h > vhgt) then
    begin
      ybot := mScrollY+vhgt;
      resetScissorNC();
      uiContext.drawVSBar(gx+mWidth-mFrameWidth+1, gy+mFrameHeight-1, mFrameWidth-3, vhgt+2, ybot, 0, mFullSize.h, mSBarFullColor[cidx], mSBarEmptyColor[cidx]);
    end;
    // horizontal scroll bar
    vwdt := mWidth-mFrameWidth*2;
    if (mFullSize.w > vwdt) then
    begin
      xend := mScrollX+vwdt;
      resetScissorNC();
      uiContext.drawHSBar(gx+mFrameWidth+1, gy+mHeight-mFrameHeight+1, vwdt-2, mFrameHeight-3, xend, 0, mFullSize.w, mSBarFullColor[cidx], mSBarEmptyColor[cidx]);
    end;
    // frame icon
    setScissor(mFrameWidth, 0, iwdt, ihgt);
    uiContext.color := mBackColor[cidx];
    uiContext.fillRect(gx+mFrameWidth, gy, iwdt, ihgt);
    uiContext.color := mFrameIconColor[cidx];
    uiContext.drawIconWin(TGxContext.TWinIcon.Close, gx+mFrameWidth, gy, mInClose);
  end;
  // shadow (no need to reset scissor, as draw should do it)
  inherited drawControlPost(gx, gy);
end;


// ////////////////////////////////////////////////////////////////////////// //
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


procedure TUITopWindow.keyEventBubble (var ev: TFUIEvent);
begin
  inherited keyEvent(ev);
  if (not ev.alive) or (not enabled) {or (not getFocused)} then exit;
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


procedure TUITopWindow.mouseEvent (var ev: TFUIEvent);
var
  lx, ly: Integer;
  vhgt, ytop: Integer;
  vwdt, xend: Integer;
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

  if (mDragScroll = TXMode.VScroll) then
  begin
    ly := ev.y-mY;
    vhgt := mHeight-mFrameHeight*2;
    ytop := uiContext.sbarPos(ly, mFrameHeight-1, vhgt+2, 0, mFullSize.h)-vhgt;
    mScrollY := nmax(0, ytop);
    if (ev.release) and (ev.but = ev.Left) then mDragScroll := TXMode.None;
    ev.eat();
    exit;
  end;

  if (mDragScroll = TXMode.HScroll) then
  begin
    lx := ev.x-mX;
    vwdt := mWidth-mFrameWidth*2;
    xend := uiContext.sbarPos(lx, mFrameWidth+1, vwdt-2, 0, mFullSize.w)-vwdt;
    mScrollX := nmax(0, xend);
    if (ev.release) and (ev.but = ev.Left) then mDragScroll := TXMode.None;
    ev.eat();
    exit;
  end;

  if toLocal(ev.x, ev.y, lx, ly) then
  begin
    if (ev.press) then
    begin
      if (ly < mFrameHeight) then
      begin
        uiGrabCtl := self;
        if (lx >= mFrameWidth) and (lx < mFrameWidth+uiContext.iconWinWidth(TGxContext.TWinIcon.Close)) then
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
      if (lx >= mWidth-mFrameWidth+1) and (ly >= mFrameHeight-1) and (ly < mHeight-mFrameHeight+2) then
      begin
        vhgt := mHeight-mFrameHeight*2;
        if (mFullSize.h > vhgt) then
        begin
          uiGrabCtl := self;
          mDragScroll := TXMode.VScroll;
          ev.eat();
          ytop := uiContext.sbarPos(ly, mFrameHeight-1, vhgt+2, 0, mFullSize.h)-vhgt;
          mScrollY := nmax(0, ytop);
          exit;
        end;
      end;
      // check for horizontal scrollbar
      if (ly >= mHeight-mFrameHeight+1) and (lx >= mFrameWidth+1) and (lx < mWidth-mFrameWidth-1) then
      begin
        vwdt := mWidth-mFrameWidth*2;
        if (mFullSize.w > vwdt) then
        begin
          uiGrabCtl := self;
          mDragScroll := TXMode.HScroll;
          ev.eat();
          xend := uiContext.sbarPos(lx, mFrameWidth+1, vwdt-2, 0, mFullSize.w)-vwdt;
          mScrollX := nmax(0, xend);
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
        if (lx >= mFrameWidth) and (lx < mFrameWidth+uiContext.iconWinWidth(TGxContext.TWinIcon.Close)) then
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
        mInClose := (lx >= mFrameWidth) and (lx < mFrameWidth+uiContext.iconWinWidth(TGxContext.TWinIcon.Close));
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
  mDefSize := TLaySize.Create(-1, -1);
end;


procedure TUIBox.setCaption (const acap: AnsiString);
begin
  mCaption := acap;
  mDefSize := TLaySize.Create(uiContext.textWidth(mCaption)+3, uiContext.textHeight(mCaption));
end;


procedure TUIBox.setHasFrame (v: Boolean);
begin
  mHasFrame := v;
  if (mHasFrame) then begin mFrameWidth := 8; mFrameHeight := uiContext.charHeight(#184); end else begin mFrameWidth := 0; mFrameHeight := 0; end;
  if (mHasFrame) then mNoPad := true;
end;


function TUIBox.parseProperty (const prname: AnsiString; par: TTextParser): Boolean;
begin
  if (parseOrientation(prname, par)) then begin result := true; exit; end;
  if (strEquCI1251(prname, 'padding')) then
  begin
    if (mHoriz) then mPadding := parseHPadding(par, 0) else mPadding := parseVPadding(par, 0);
    result := true;
    exit;
  end;
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
  //xpos: Integer;
begin
  cidx := getColorIndex;
  uiContext.color := mBackColor[cidx];
  uiContext.fillRect(gx, gy, mWidth, mHeight);
  if (mHasFrame) then
  begin
    // draw frame
    drawFrame(gx, gy, 0, mHAlign, mCaption, false);
  end;
  // no frame -- no caption
  {
  else if (Length(mCaption) > 0) then
  begin
    // draw caption
         if (mHAlign < 0) then xpos := 3
    else if (mHAlign > 0) then xpos := mWidth-mFrameWidth*2-uiContext.textWidth(mCaption)
    else xpos := (mWidth-mFrameWidth*2-uiContext.textWidth(mCaption)) div 2;
    xpos += gx+mFrameWidth;

    setScissor(mFrameWidth+1, 0, mWidth-mFrameWidth-2, uiContext.textHeight(mCaption));
    uiContext.color := mFrameTextColor[cidx];
    uiContext.drawText(xpos, gy, mCaption);
  end;
  }
end;


procedure TUIBox.mouseEvent (var ev: TFUIEvent);
var
  lx, ly: Integer;
begin
  inherited mouseEvent(ev);
  if (ev.alive) and (enabled) and toLocal(ev.x, ev.y, lx, ly) then
  begin
    ev.eat();
  end;
end;


procedure TUIBox.keyEvent (var ev: TFUIEvent);
var
  dir: Integer = 0;
  cur, ctl: TUIControl;
begin
  inherited keyEvent(ev);
  if (not ev.alive) or (not ev.press) or (not enabled) or (not getActive) then exit;
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
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUISpan.AfterConstruction ();
begin
  inherited;
  mExpand := true;
  mCanFocus := false;
  mNoPad := true;
  mCtl4Style := 'span';
  mDefSize := TLaySize.Create(-1, -1);
end;


function TUISpan.parseProperty (const prname: AnsiString; par: TTextParser): Boolean;
begin
  if (parseOrientation(prname, par)) then begin result := true; exit; end;
  result := inherited parseProperty(prname, par);
end;


// ////////////////////////////////////////////////////////////////////// //
procedure TUILine.AfterConstruction ();
begin
  inherited;
  mCanFocus := false;
  mExpand := true;
  mCanFocus := false;
  mCtl4Style := 'line';
  mDefSize := TLaySize.Create(-1, -1);
end;


function TUILine.parseProperty (const prname: AnsiString; par: TTextParser): Boolean;
begin
  if (parseOrientation(prname, par)) then begin result := true; exit; end;
  result := inherited parseProperty(prname, par);
end;


procedure TUILine.layPrepare ();
begin
  inherited layPrepare();
  if (mParent <> nil) then mHoriz := not mParent.mHoriz;
  if (mHoriz) then
  begin
    if (mLayDefSize.w < 0) then mLayDefSize.w := 1;
    if (mLayDefSize.h < 0) then mLayDefSize.h := 7;
  end
  else
  begin
    if (mLayDefSize.w < 0) then mLayDefSize.w := 7;
    if (mLayDefSize.h < 0) then mLayDefSize.h := 1;
  end;
end;


procedure TUILine.drawControl (gx, gy: Integer);
var
  cidx: Integer;
begin
  cidx := getColorIndex;
  uiContext.color := mTextColor[cidx];
  if mHoriz then uiContext.hline(gx, gy+(mHeight div 2), mWidth)
  else uiContext.vline(gx+(mWidth div 2), gy, mHeight);
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
  mCtl4Style := 'static';
end;


procedure TUIStaticText.setText (const atext: AnsiString);
begin
  mText := atext;
  mDefSize := TLaySize.Create(uiContext.textWidth(mText), uiContext.textHeight(mText));
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
begin
  cidx := getColorIndex;
  uiContext.color := mBackColor[cidx];
  uiContext.fillRect(gx, gy, mWidth, mHeight);

       if (mHAlign < 0) then xpos := 0
  else if (mHAlign > 0) then xpos := mWidth-uiContext.textWidth(mText)
  else xpos := (mWidth-uiContext.textWidth(mText)) div 2;

  if (Length(mText) > 0) then
  begin
    if (mHeader) then uiContext.color := mFrameTextColor[cidx] else uiContext.color := mTextColor[cidx];

         if (mVAlign < 0) then ypos := 0
    else if (mVAlign > 0) then ypos := mHeight-uiContext.textHeight(mText)
    else ypos := (mHeight-uiContext.textHeight(mText)) div 2;

    uiContext.drawText(gx+xpos, gy+ypos, mText);
  end;

  if (mLine) then
  begin
    if (mHeader) then uiContext.color := mFrameColor[cidx] else uiContext.color := mTextColor[cidx];

         if (mVAlign < 0) then ypos := 0
    else if (mVAlign > 0) then ypos := mHeight-1
    else ypos := (mHeight div 2);
    ypos += gy;

    if (Length(mText) = 0) then
    begin
      uiContext.hline(gx, ypos, mWidth);
    end
    else
    begin
      uiContext.hline(gx, ypos, xpos-1);
      uiContext.hline(gx+xpos+uiContext.textWidth(mText), ypos, mWidth);
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
          mHotOfs := Length(mText);
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
  // fix hotchar offset
  if (mHotChar <> #0) and (mHotOfs > 0) then
  begin
    mHotOfs := uiContext.textWidth(Copy(mText, 1, mHotOfs+1))-uiContext.charWidth(mText[mHotOfs+1]);
  end;
  // fix size
  mDefSize := TLaySize.Create(uiContext.textWidth(mText), uiContext.textHeight(mText));
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
  uiContext.color := mBackColor[cidx];
  uiContext.fillRect(gx, gy, mWidth, mHeight);
  if (Length(mText) > 0) then
  begin
         if (mHAlign < 0) then xpos := 0
    else if (mHAlign > 0) then xpos := mWidth-uiContext.textWidth(mText)
    else xpos := (mWidth-uiContext.textWidth(mText)) div 2;

         if (mVAlign < 0) then ypos := 0
    else if (mVAlign > 0) then ypos := mHeight-uiContext.textHeight(mText)
    else ypos := (mHeight-uiContext.textHeight(mText)) div 2;

    uiContext.color := mTextColor[cidx];
    uiContext.drawText(gx+xpos, gy+ypos, mText);

    if (Length(mLinkId) > 0) and (mHotChar <> #0) and (mHotChar <> ' ') then
    begin
      uiContext.color := mHotColor[cidx];
      uiContext.drawChar(gx+xpos+mHotOfs, gy+ypos, mHotChar);
    end;
  end;
end;


procedure TUITextLabel.mouseEvent (var ev: TFUIEvent);
var
  lx, ly: Integer;
begin
  inherited mouseEvent(ev);
  if (ev.alive) and (enabled) and toLocal(ev.x, ev.y, lx, ly) then
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


procedure TUITextLabel.keyEventBubble (var ev: TFUIEvent);
begin
  if (not enabled) then exit;
  if (mHotChar = #0) then exit;
  if (not ev.alive) or (not ev.press) then exit;
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
  mHAlign := 0;
  mVAlign := 0;
  mShadowSize := 0;
  mCanFocus := true;
  mDefSize := TLaySize.Create(uiContext.textWidth(mText)+uiContext.textWidth('[  ]'), uiContext.textHeight(mText));
  mCtl4Style := 'button';
  mSkipLayPrepare := false;
  mAddMarkers := false;
  mHideMarkers := false;
end;


procedure TUIButton.cacheStyle (root: TUIStyle);
var
  sz: Integer = 0;
begin
  inherited cacheStyle(root);
  // shadow size
  sz := nmax(0, root.get('shadow-size', 'active', mCtl4Style).asInt(0));
  sz := nmax(sz, root.get('shadow-size', 'disabled', mCtl4Style).asInt(0));
  sz := nmax(sz, root.get('shadow-size', 'inactive', mCtl4Style).asInt(0));
  mShadowSize := sz;
  // markers mode
  mAddMarkers := root.get('add-markers', 'active', mCtl4Style).asBool(false);
  mAddMarkers := mAddMarkers or root.get('add-markers', 'disabled', mCtl4Style).asBool(false);
  mAddMarkers := mAddMarkers or root.get('add-markers', 'inactive', mCtl4Style).asBool(false);
  // hide markers?
  mHideMarkers := root.get('hide-markers', 'active', mCtl4Style).asBool(false);
  mHideMarkers := mHideMarkers or root.get('hide-markers', 'disabled', mCtl4Style).asBool(false);
  mHideMarkers := mHideMarkers or root.get('hide-markers', 'inactive', mCtl4Style).asBool(false);
end;


procedure TUIButton.setText (const s: AnsiString);
begin
  inherited setText(s);
  if (mHideMarkers) then
  begin
    mDefSize := TLaySize.Create(uiContext.textWidth(mText)+10, uiContext.textHeight(mText));
  end
  else if (mAddMarkers) then
  begin
    mDefSize := TLaySize.Create(uiContext.textWidth(mText)+uiContext.textWidth('[<>]'), uiContext.textHeight(mText));
  end
  else
  begin
    mDefSize := TLaySize.Create(uiContext.textWidth(mText)+uiContext.textWidth('<>'), uiContext.textHeight(mText));
  end;
end;


procedure TUIButton.layPrepare ();
var
  ods: TLaySize;
  ww: Integer;
begin
  if (not mSkipLayPrepare) then
  begin
    ods := mDefSize;
    if (ods.w <> 0) or (ods.h <> 0) then
    begin
      mDefSize := TLaySize.Create(uiContext.textWidth(mText), uiContext.textHeight(mText));
      if (mHideMarkers) then
      begin
        ww := 10;
      end
      else if (mAddMarkers) then
      begin
             if (mDefault) then ww := uiContext.textWidth('[<  >]')
        else if (mCancel) then ww := uiContext.textWidth('[{  }]')
        else ww := uiContext.textWidth('[  ]');
      end
      else
      begin
        ww := nmax(0, uiContext.textWidth('<  >'));
        ww := nmax(ww, uiContext.textWidth('{  }'));
        ww := nmax(ww, uiContext.textWidth('[  ]'));
      end;
      mDefSize.w += ww+mShadowSize;
      mDefSize.h += mShadowSize;
    end;
  end
  else
  begin
    ods := TLaySize.Create(0, 0); // fpc is dumb!
  end;
  inherited layPrepare();
  if (not mSkipLayPrepare) then mDefSize := ods;
end;


procedure TUIButton.blurred ();
begin
  mPushed := false;
end;


procedure TUIButton.drawControl (gx, gy: Integer);
var
  wdt, hgt: Integer;
  xpos, ypos, xofsl, xofsr, sofs: Integer;
  cidx: Integer;
  lch, rch: AnsiChar;
  lstr, rstr: AnsiString;
begin
  cidx := getColorIndex;

  wdt := mWidth-mShadowSize;
  hgt := mHeight-mShadowSize;
  if (mPushed) {or (cidx = ClrIdxActive)} then
  begin
    sofs := mShadowSize;
    gx += mShadowSize;
    gy += mShadowSize;
  end
  else
  begin
    sofs := 0;
    if (mShadowSize > 0) then
    begin
      uiContext.darkenRect(gx+mShadowSize, gy+hgt, wdt, mShadowSize, 96);
      uiContext.darkenRect(gx+wdt, gy+mShadowSize, mShadowSize, hgt-mShadowSize, 96);
    end;
  end;

  uiContext.color := mBackColor[cidx];
  uiContext.fillRect(gx, gy, wdt, hgt);

       if (mVAlign < 0) then ypos := 0
  else if (mVAlign > 0) then ypos := hgt-uiContext.textHeight(mText)
  else ypos := (hgt-uiContext.textHeight(mText)) div 2;
  ypos += gy;

  uiContext.color := mTextColor[cidx];

  if (mHideMarkers) then
  begin
    xofsl := 5;
    xofsr := 5;
  end
  else
  begin
    if (mAddMarkers) then
    begin
           if (mDefault) then begin lstr := '[< '; rstr := ' >]'; end
      else if (mCancel) then begin lstr := '[{ '; rstr := ' }]'; end
      else begin lstr := '[ '; rstr := ' ]'; end;
      xofsl := uiContext.textWidth(lstr);
      xofsr := uiContext.textWidth(rstr);
      uiContext.drawText(gx, ypos, lstr);
      uiContext.drawText(gx+wdt-uiContext.textWidth(rstr), ypos, rstr);
    end
    else
    begin
      xofsl := nmax(0, uiContext.textWidth('< '));
      xofsl := nmax(xofsl, uiContext.textWidth('{ '));
      xofsl := nmax(xofsl, uiContext.textWidth('[ '));
      xofsr := nmax(0, uiContext.textWidth(' >'));
      xofsr := nmax(xofsr, uiContext.textWidth(' }'));
      xofsr := nmax(xofsr, uiContext.textWidth(' ]'));
           if (mDefault) then begin lch := '<'; rch := '>'; end
      else if (mCancel) then begin lch := '{'; rch := '}'; end
      else begin lch := '['; rch := ']'; end;
      uiContext.drawChar(gx, ypos, lch);
      uiContext.drawChar(gx+wdt-uiContext.charWidth(rch), ypos, rch);
    end;
  end;

  if (Length(mText) > 0) then
  begin
    if (mHAlign < 0) then xpos := 0
    else begin xpos := wdt-xofsl-xofsr-uiContext.textWidth(mText); if (mHAlign = 0) then xpos := xpos div 2; end;
    xpos += xofsl;

    setScissor(sofs+xofsl, sofs, wdt-xofsl-xofsr, hgt);
    uiContext.drawText(gx+xpos, ypos, mText);

    if (mHotChar <> #0) and (mHotChar <> ' ') then
    begin
      uiContext.color := mHotColor[cidx];
      uiContext.drawChar(gx+xpos+mHotOfs, ypos, mHotChar);
    end;
  end;
end;


procedure TUIButton.mouseEvent (var ev: TFUIEvent);
var
  lx, ly: Integer;
begin
  inherited mouseEvent(ev);
  if (uiGrabCtl = self) then
  begin
    ev.eat();
    mPushed := toLocal(ev.x, ev.y, lx, ly);
    if (ev = '-lmb') and (focused) and (mPushed) then
    begin
      mPushed := false;
      doAction();
    end;
    exit;
  end;
  if (not ev.alive) or (not enabled) or (not focused) then exit;
  mPushed := true;
  ev.eat();
end;


procedure TUIButton.keyEvent (var ev: TFUIEvent);
begin
  inherited keyEvent(ev);
  if (ev.alive) and (enabled) then
  begin
    if (ev = '+Enter') or (ev = '+Space') then
    begin
      focused := true;
      mPushed := true;
      ev.eat();
      exit;
    end;
    if (focused) and ((ev = '-Enter') or (ev = '-Space')) then
    begin
      if (mPushed) then
      begin
        mPushed := false;
        ev.eat();
        doAction();
      end
      else
      begin
        ev.eat();
      end;
      exit;
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TUIButtonRound.AfterConstruction ();
begin
  inherited;
  mHAlign := -1;
  mVAlign := 0;
  mCanFocus := true;
  mDefSize := TLaySize.Create(uiContext.textWidth(mText)+8*2, uiContext.textHeight(mText)+2);
  mCtl4Style := 'button-round';
  mSkipLayPrepare := true;
end;


procedure TUIButtonRound.setText (const s: AnsiString);
begin
  inherited setText(s);
  mDefSize := TLaySize.Create(uiContext.textWidth(mText)+8*2, uiContext.textHeight(mText)+2);
end;


procedure TUIButtonRound.layPrepare ();
var
  ods: TLaySize;
begin
  ods := mDefSize;
  if (ods.w <> 0) or (ods.h <> 0) then
  begin
    mDefSize := TLaySize.Create(uiContext.textWidth(mText)+8*2, uiContext.textHeight(mText)+2);
  end;
  inherited layPrepare();
  mDefSize := ods;
end;


procedure TUIButtonRound.drawControl (gx, gy: Integer);
var
  xpos, ypos: Integer;
  cidx: Integer;
begin
  cidx := getColorIndex;

  uiContext.color := mBackColor[cidx];
  uiContext.fillRect(gx+1, gy, mWidth-2, mHeight);
  uiContext.fillRect(gx, gy+1, 1, mHeight-2);
  uiContext.fillRect(gx+mWidth-1, gy+1, 1, mHeight-2);

  if (Length(mText) > 0) then
  begin
         if (mHAlign < 0) then xpos := 0
    else if (mHAlign > 0) then xpos := mWidth-uiContext.textWidth(mText)
    else xpos := (mWidth-uiContext.textWidth(mText)) div 2;

         if (mVAlign < 0) then ypos := 0
    else if (mVAlign > 0) then ypos := mHeight-uiContext.textHeight(mText)
    else ypos := (mHeight-uiContext.textHeight(mText)) div 2;

    setScissor(8, 0, mWidth-16, mHeight);
    uiContext.color := mTextColor[cidx];
    uiContext.drawText(gx+xpos+8, gy+ypos, mText);

    if (mHotChar <> #0) and (mHotChar <> ' ') then
    begin
      uiContext.color := mHotColor[cidx];
      uiContext.drawChar(gx+xpos+8+mHotOfs, gy+ypos, mHotChar);
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
  mIcon := TGxContext.TMarkIcon.Checkbox;
  mDefSize := TLaySize.Create(uiContext.textWidth(mText)+3+uiContext.iconMarkWidth(mIcon), nmax(uiContext.iconMarkHeight(mIcon), uiContext.textHeight(mText)));
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
  mDefSize := TLaySize.Create(uiContext.textWidth(mText)+3+uiContext.iconMarkWidth(mIcon), nmax(uiContext.iconMarkHeight(mIcon), uiContext.textHeight(mText)));
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
  xpos, ypos, iwdt, dy: Integer;
  cidx: Integer;
begin
  cidx := getColorIndex;

  iwdt := uiContext.iconMarkWidth(mIcon);
       if (mHAlign < 0) then xpos := 0
  else if (mHAlign > 0) then xpos := mWidth-(uiContext.textWidth(mText)+3+iwdt)
  else xpos := (mWidth-(uiContext.textWidth(mText)+3+iwdt)) div 2;

       if (mVAlign < 0) then ypos := 0
  else if (mVAlign > 0) then ypos := mHeight-uiContext.textHeight(mText)
  else ypos := (mHeight-uiContext.textHeight(mText)) div 2;

  uiContext.color := mBackColor[cidx];
  uiContext.fillRect(gx, gy, mWidth, mHeight);

  uiContext.color := mSwitchColor[cidx];
  if (uiContext.iconMarkHeight(mIcon) < uiContext.textHeight(mText)) then
  begin
    case uiContext.textHeight(mText) of
      14: dy := 2;
      16: dy := 3;
      else dy := 1;
    end;
    uiContext.drawIconMark(mIcon, gx, gy+ypos+uiContext.textHeight(mText)-uiContext.iconMarkHeight(mIcon)-dy, checked);
  end
  else
  begin
    uiContext.drawIconMark(mIcon, gx, gy, checked);
  end;

  uiContext.color := mTextColor[cidx];
  uiContext.drawText(gx+xpos+3+iwdt, gy+ypos, mText);

  if (mHotChar <> #0) and (mHotChar <> ' ') then
  begin
    uiContext.color := mHotColor[cidx];
    uiContext.drawChar(gx+xpos+3+iwdt+mHotOfs, gy+ypos, mHotChar);
  end;
end;


procedure TUISwitchBox.mouseEvent (var ev: TFUIEvent);
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
  if (not ev.alive) or (not enabled) or not focused then exit;
  ev.eat();
end;


procedure TUISwitchBox.keyEvent (var ev: TFUIEvent);
begin
  inherited keyEvent(ev);
  if (ev.alive) and (enabled) then
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
  mIcon := TGxContext.TMarkIcon.Checkbox;
  setText('');
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
  mRadioGroup := '';
  mIcon := TGxContext.TMarkIcon.Radiobox;
  setText('');
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
  if v then topLevel.forEachControl(@resetGroup);
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
var
  oldFocus: procedure () = nil;
  oldBlur: procedure () = nil;

procedure onWinFocus (); begin uiFocus(); if (assigned(oldFocus)) then oldFocus(); end;
procedure onWinBlur (); begin fuiResetKMState(true); uiBlur(); if (assigned(oldBlur)) then oldBlur(); end;

initialization
  registerCtlClass(TUIHBox, 'hbox');
  registerCtlClass(TUIVBox, 'vbox');
  registerCtlClass(TUISpan, 'span');
  registerCtlClass(TUILine, 'line');
  registerCtlClass(TUITextLabel, 'label');
  registerCtlClass(TUIStaticText, 'static');
  registerCtlClass(TUIButtonRound, 'round-button');
  registerCtlClass(TUIButton, 'button');
  registerCtlClass(TUICheckBox, 'checkbox');
  registerCtlClass(TUIRadioBox, 'radiobox');

  oldFocus := winFocusCB;
  oldBlur := winBlurCB;
  winFocusCB := @onWinFocus;
  winBlurCB := @onWinBlur;
end.
