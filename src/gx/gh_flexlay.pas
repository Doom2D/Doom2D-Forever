{$INCLUDE ../shared/a_modes.inc}
unit gh_flexlay;

(*
first pass:
  set all 'temp-flex' flags for controls to 'flex'
  reset all 'laywrap' flags for controls
  build group arrays; for each group: find max size for group, adjust 'startsize' controls to group max size
  call 'calc max size' for top-level control
  flags set:
    'firsttime'

second pass:
  calcluate desired sizes (process flexes) using 'startsize', set 'desiredsize' and 'desiredpos'
    if control has children, call 'second pass' recursively with this control as parent
  flags set:
    'group-element-changed', if any group element size was changed
    'wrapping-changed', if not 'firsttime', and wrapping was changed (i.e. first pass will not set the flag)

third pass:
  if 'group-element-changed':
    for each group: adjust controls to max desired size (startsize), set 'temp-flex' flags to 0 for 'em, set 'second-again' flag
  for other controls: if 'desiredsize' > 'maxsize', set 'startsize' to 'maxsize', set 'temp-flex' flag to 0, set 'second-again' flag
  if 'second-again' or 'wrapping-changed':
    reset 'second-again'
    reset 'wrapping-changed'
    reset 'firsttime'
    goto second pass

fourth pass:
  set 'actualsize' and 'actualpos' to 'desiredsize' and 'desiredpos'
  return

calc max size:
  set 'startsize' to max(size, maxsize, 0)
  if 'size' is negative:
    set 'temp-flex' flag to 0
  if has children:
    call 'calc max size' for each child
    set 'desiredmax' to 'startsize'
    do lines, don't distribute space (i.e. calc only wrapping),
      for each complete line, set 'desiredmax' to max(desiredmax, linesize)
    if 'maxsize' >= 0:
      set 'desiredmax' to min(desiredmax, maxsize)
    set 'startsize' to 'desiredmax'
  return


wrapping lines:
  try to stuff controls in line until line width is less or equal to maxsize
  distribute flex for filled line
  continue until we still has something to stuff


for wrapping:
  we'll hold 'laywrap' flag for each control; it will be set if this control
  starts a new line (even if this is the first control in line, as it is obviously
  starts a new line)

  on redoing second pass, if 'laywrap' flag changed, set 'wrapping-changed' flag
*)


(*
  control default size will be increased by margins
  negative margins are ignored
ControlT:
  procedure layPrepare (); // called before registering control in layouter
  function getDefSize (): TLaySize; // default size; <0: use max size
  function getMargins (): TLayMargins;
  function getMaxSize (): TLaySize; // max size; <0: set to some huge value
  function getFlex (): Integer; // <=0: not flexible
  function isHorizBox (): Boolean; // horizontal layout for children?
  function canWrap (): Boolean; // for horizontal boxes: can wrap children? for child: `false` means 'nonbreakable at *next* ctl'
  function isLineStart (): Boolean; // `true` if this ctl should start a new line; ignored for vertical boxes
  function getAlign (): Integer; // aligning in non-main direction: <0: left/up; 0: center; >0: right/down
  function getExpand (): Boolean; // expanding in non-main direction: `true` will ignore align and eat all available space
  procedure setActualSizePos (constref apos: TLayPos; constref asize: TLaySize);
  function getHGroup (): AnsiString; // empty: not grouped
  function getVGroup (): AnsiString; // empty: not grouped
  function nextSibling (): ControlT;
  function firstChild (): ControlT;
*)

interface

uses
  gh_ui_common;


// ////////////////////////////////////////////////////////////////////////// //
type
  generic TFlexLayouterBase<ControlT> = class
  public
    type CtlT = ControlT;

  private
    type LayControlIdx = Integer;

  private
    // flags
    const
      FlagHorizBox = LongWord(1) shl 0; // horizontal layout for children
      FlagLineStart = LongWord(1) shl 1;
      FlagLineCanWrap = LongWord(1) shl 2;
      // internal
      FlagLineDidWrap = LongWord(1) shl 3; // will be set when line was wrapped
      FlagInGroup = LongWord(1) shl 4; // set if this control is a member of any group
      FlagExpand = LongWord(1) shl 5;
      FlagLineFirst = LongWord(1) shl 6;

  private
    type
      PLayControl = ^TLayControl;
      TLayControl = record
      public
        myidx: LayControlIdx;
        tempFlex: Integer;
        flags: LongWord; // see below
        aligndir: Integer;
        startsize: TLaySize; // current
        desiredsize: TLaySize;
        maxsize: TLaySize;
        margins: TLayMargins; // can never be negative
        desiredpos: TLayPos;
        ctl: ControlT;
        parent: LayControlIdx; // = -1;
        firstChild: LayControlIdx; // = -1;
        nextSibling: LayControlIdx; // = -1;

      private
        function getDidWrap (): Boolean; inline;
        procedure setDidWrap (v: Boolean); inline;

      public
        procedure initialize (); inline;

        function horizBox (): Boolean; inline;
        function lineStart (): Boolean; inline;
        function canWrap (): Boolean; inline;
        function inGroup (): Boolean; inline;
        function firstInLine (): Boolean; inline;

        function getExpand (): Boolean; inline;
        procedure setExpand (v: Boolean); inline;

      public
        property didWrap: Boolean read getDidWrap write setDidWrap;
        property expand: Boolean read getExpand write setExpand;
      end;

      PLayGroup = ^TLayGroup;
      TLayGroup = record
        name: AnsiString;
        ctls: array of LayControlIdx;
      end;

      TLayCtlArray = array of TLayControl;
      TLayGrpArray = array of TLayGroup;

  private
    ctlist: TLayCtlArray;
    groups: array[0..1] of TLayGrpArray; // horiz, vert

    firstTime: Boolean;
    groupElementChanged: Boolean;
    wrappingChanged: Boolean;

  private
    procedure fixFlags (cidx: LayControlIdx);
    procedure doChildren (parent: LayControlIdx; child: ControlT);
    procedure appendToGroup (const gname: AnsiString;cidx: LayControlIdx;gidx: Integer);
    procedure setupGroups ();

    // this also sets `tempFlex`
    procedure calcMaxSizeInternal (cidx: LayControlIdx);

    procedure fixLine (me: PLayControl; i0, i1: LayControlIdx; cury: Integer; var spaceLeft: Single; var flexTotal: Integer; var flexBoxCount: Integer);
    // do box layouting; call `layBox()` recursively if necessary
    procedure layBox (boxidx: LayControlIdx);

    procedure firstPass ();
    procedure secondPass ();
    procedure thirdPass ();
    procedure fourthPass ();

    procedure dumpList (cidx: LayControlIdx; indent: Integer);

  public
    type
      TChildrenEnumerator = record
      private
        ctls: TLayCtlArray;
        cur: Integer;
        first: Boolean;
      public
        constructor Create (constref actls: TLayCtlArray; acur: Integer);
        function moveNext (): Boolean; inline;
        function getCurrent (): PLayControl; inline;
        function getEnumerator (): TChildrenEnumerator; inline;
        property current: PLayControl read getCurrent;
      end;

  public
    constructor Create ();
    destructor Destroy (); override;

    // clear build lists
    procedure clear ();

    // build control and group lists
    procedure setup (root: ControlT);

    function forChildren (cidx: LayControlIdx): TChildrenEnumerator; inline;

    procedure layout ();

    procedure dumpFlat ();
    procedure dump ();
  end;


implementation

uses
  utils;


// ////////////////////////////////////////////////////////////////////////// //
procedure TFlexLayouterBase.TLayControl.initialize (); inline;
begin
  FillChar(self, 0, sizeof(self));
  parent := -1;
  firstChild := -1;
  nextSibling := -1;
end;

function TFlexLayouterBase.TLayControl.horizBox (): Boolean; inline; begin result := ((flags and FlagHorizBox) <> 0); end;
function TFlexLayouterBase.TLayControl.lineStart (): Boolean; inline; begin result := ((flags and FlagLineStart) <> 0); end;
function TFlexLayouterBase.TLayControl.canWrap (): Boolean; inline; begin result := ((flags and FlagLineCanWrap) <> 0); end;
function TFlexLayouterBase.TLayControl.inGroup (): Boolean; inline; begin result := ((flags and FlagInGroup) <> 0); end;
function TFlexLayouterBase.TLayControl.firstInLine (): Boolean; inline; begin result := ((flags and FlagLineFirst) <> 0); end;

function TFlexLayouterBase.TLayControl.getDidWrap (): Boolean; inline; begin result := ((flags and FlagLineDidWrap) <> 0); end;
procedure TFlexLayouterBase.TLayControl.setDidWrap (v: Boolean); inline; begin if (v) then flags := flags or FlagLineDidWrap else flags := flags and (not FlagLineDidWrap); end;

function TFlexLayouterBase.TLayControl.getExpand (): Boolean; inline; begin result := ((flags and FlagExpand) <> 0); end;
procedure TFlexLayouterBase.TLayControl.setExpand (v: Boolean); inline; begin if (v) then flags := flags or FlagExpand else flags := flags and (not FlagExpand); end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TFlexLayouterBase.TChildrenEnumerator.Create (constref actls: TLayCtlArray; acur: Integer);
begin
  ctls := actls;
  cur := acur;
  first := true;
end;

function TFlexLayouterBase.TChildrenEnumerator.moveNext (): Boolean; inline;
begin
  if first then
  begin
    if (cur >= 0) and (cur < Length(ctls)) then cur := ctls[cur].firstChild else cur := -1;
    first := false;
  end
  else
  begin
    cur := ctls[cur].nextSibling;
  end;
  result := (cur >= 0);
end;

function TFlexLayouterBase.TChildrenEnumerator.getCurrent (): PLayControl; inline;
begin
  result := @ctls[cur];
end;

function TFlexLayouterBase.TChildrenEnumerator.getEnumerator (): TChildrenEnumerator; inline;
begin
  result := self;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TFlexLayouterBase.Create ();
begin
  ctlist := nil;
  groups[0] := nil;
  groups[1] := nil;

  firstTime := false;
  groupElementChanged := false;
  wrappingChanged := false;
end;


destructor TFlexLayouterBase.Destroy ();
begin
  clear();
  inherited;
end;


function TFlexLayouterBase.forChildren (cidx: LayControlIdx): TChildrenEnumerator; inline;
begin
  result := TChildrenEnumerator.Create(ctlist, cidx);
end;


procedure TFlexLayouterBase.clear ();
begin
  ctlist := nil;
  groups[0] := nil;
  groups[1] := nil;
end;


procedure TFlexLayouterBase.fixFlags (cidx: LayControlIdx);
var
  lc: PLayControl;
begin
  assert((cidx >= 0) and (cidx < Length(ctlist)));
  lc := @ctlist[cidx];
  //lc.flags := 0;
  if (lc.ctl.isHorizBox) then lc.flags := lc.flags or FlagHorizBox;
  if (lc.ctl.isLineStart) then lc.flags := lc.flags or FlagLineStart;
  if (lc.ctl.canWrap) then lc.flags := lc.flags or FlagLineCanWrap;
  if (lc.ctl.getExpand) then lc.flags := lc.flags or FlagExpand;
  lc.aligndir := lc.ctl.getAlign;
end;


procedure TFlexLayouterBase.doChildren (parent: LayControlIdx; child: ControlT);
var
  cidx: LayControlIdx = -1;
  lc: PLayControl;
begin
  assert((parent >= 0) and (parent < Length(ctlist)));
  assert(ctlist[parent].firstChild = -1);
  while (child <> nil) do
  begin
    child.layPrepare;
    SetLength(ctlist, Length(ctlist)+1);
    lc := @ctlist[High(ctlist)];
    lc.initialize();
    if (cidx = -1) then
    begin
      cidx := LayControlIdx(High(ctlist));
      ctlist[parent].firstChild := cidx;
      // first child is always linestart
      lc.flags := lc.flags or FlagLineStart or FlagLineFirst;
    end
    else
    begin
      ctlist[cidx].nextSibling := LayControlIdx(High(ctlist));
      cidx := LayControlIdx(High(ctlist));
    end;
    lc.myidx := cidx;
    lc.ctl := child;
    lc.parent := parent;
    fixFlags(cidx);
    doChildren(cidx, child.firstChild);
    child := child.nextSibling;
  end;
end;


procedure TFlexLayouterBase.appendToGroup (const gname: AnsiString; cidx: LayControlIdx; gidx: Integer);
var
  f: Integer;
begin
  if (Length(gname) = 0) then exit;
  assert((cidx >= 0) and (cidx < Length(ctlist)));
  assert((gidx = 0) or (gidx = 1));
  ctlist[cidx].flags := ctlist[cidx].flags or FlagInGroup;
  for f := 0 to High(groups[gidx]) do
  begin
    if (groups[gidx][f].name = gname) then
    begin
      SetLength(groups[gidx][f].ctls, Length(groups[gidx][f].ctls)+1);
      groups[gidx][f].ctls[High(groups[gidx][f].ctls)] := cidx;
      exit;
    end;
  end;
  // new group
  f := Length(groups[gidx]);
  SetLength(groups[gidx], f+1);
  groups[gidx][f].name := gname;
  SetLength(groups[gidx][f].ctls, Length(groups[gidx][f].ctls)+1);
  groups[gidx][f].ctls[High(groups[gidx][f].ctls)] := cidx;
end;


procedure TFlexLayouterBase.setupGroups ();
var
  idx: Integer;
  lc: PLayControl;
begin
  for idx := 0 to High(ctlist) do
  begin
    lc := @ctlist[idx];
    appendToGroup(lc.ctl.getHGroup, LayControlIdx(idx), 0);
    appendToGroup(lc.ctl.getVGroup, LayControlIdx(idx), 1);
  end;
end;


// build control and group lists
procedure TFlexLayouterBase.setup (root: ControlT);
begin
  clear();
  if (root = nil) then exit;
  root.layPrepare;
  try
    SetLength(ctlist, 1);
    ctlist[0].initialize();
    ctlist[0].myidx := 0;
    ctlist[0].ctl := root;
    fixFlags(0);
    ctlist[0].flags := ctlist[0].flags or FlagLineStart or FlagLineFirst;
    doChildren(0, root.firstChild);
    setupGroups();
  except
    clear();
    raise;
  end;
end;


// this also sets `tempFlex`
procedure TFlexLayouterBase.calcMaxSizeInternal (cidx: LayControlIdx);
var
  lc, c: PLayControl;
  msz: TLaySize;
  negw, negh: Boolean;
  curwdt, curhgt, totalhgt: Integer;
  doWrap: Boolean;
begin
  if (cidx < 0) or (cidx >= Length(ctlist)) then exit;

  lc := @ctlist[cidx];
  msz := lc.ctl.getMaxSize;
  negw := (lc.startsize.w <= 0);
  negh := (lc.startsize.h <= 0);

  lc.tempFlex := lc.ctl.getFlex;

  for c in forChildren(cidx) do calcMaxSizeInternal(c.myidx);

  if (lc.horizBox) then
  begin
    // horizontal boxes
    if (negw) then lc.tempFlex := 0; // size is negative: don't expand
    curwdt := lc.margins.horiz;
    curhgt := lc.margins.vert;
    totalhgt := 0;
    for c in forChildren(cidx) do
    begin
      // new line?
      doWrap := (not c.firstInLine) and (c.lineStart);
      // need to wrap?
      if (not doWrap) and (lc.canWrap) and (c.canWrap) and (msz.w > 0) and (curwdt+c.startsize.w > lc.startsize.w) then doWrap := true;
      if (doWrap) then
      begin
        totalhgt += curhgt;
        if (lc.startsize.w < curwdt) then lc.startsize.w := curwdt;
        curwdt := 0;
        curhgt := 0;
      end;
      curwdt += c.startsize.w;
      if (curhgt < c.startsize.h) then curhgt := c.startsize.h;
    end;
    totalhgt += curhgt;
    if (lc.startsize.w < curwdt) then lc.startsize.w := curwdt;
    if (lc.startsize.h < totalhgt) then lc.startsize.h := totalhgt;
  end
  else
  begin
    // vertical boxes
    if (negh) then lc.tempFlex := 0; // size is negative: don't expand
    curhgt := lc.margins.vert;
    for c in forChildren(cidx) do
    begin
      if (lc.startsize.w < c.startsize.w+lc.margins.horiz) then lc.startsize.w := c.startsize.w+lc.margins.horiz;
      curhgt += c.startsize.h;
    end;
    if (lc.startsize.h < curhgt) then lc.startsize.h := curhgt;
  end;
  if (lc.startsize.w < 0) then lc.startsize.w := 0;
  if (lc.startsize.h < 0) then lc.startsize.h := 0;
  lc.maxsize := msz;
  if (lc.maxsize.w < lc.startsize.w) then lc.maxsize.w := lc.startsize.w;
  if (lc.maxsize.h < lc.startsize.h) then lc.maxsize.h := lc.startsize.h;
end;


procedure TFlexLayouterBase.firstPass ();
var
  f, c: Integer;
  needRecalcMaxSize: Boolean;
  gtype: Integer;
  grp: PLayGroup;
  maxsz: Integer;
  cidx: LayControlIdx;
  mr: TLayMargins;
begin
  // reset all 'laywrap' flags for controls, set initial 'startsize'
  for f := 0 to High(ctlist) do
  begin
    ctlist[f].didWrap := false;
    ctlist[f].startsize := ctlist[f].ctl.getDefSize;
    mr := ctlist[f].ctl.getMargins;
    ctlist[f].margins := mr;
    ctlist[f].startsize.w += mr.horiz;
    ctlist[f].startsize.h += mr.vert;
  end;
  // setup sizes
  calcMaxSizeInternal(0); // this also sets `tempFlex`
  // find max size for group, adjust 'startsize' controls to group max size
  needRecalcMaxSize := false;
  for gtype := 0 to 1 do
  begin
    for f := 0 to High(groups[gtype]) do
    begin
      grp := @groups[gtype][f];
      maxsz := 0;
      for c := 0 to High(grp.ctls) do
      begin
        cidx := grp.ctls[c];
        if (maxsz < ctlist[cidx].startsize[gtype]) then maxsz := ctlist[cidx].startsize[gtype];
      end;
      for c := 0 to High(grp.ctls) do
      begin
        cidx := grp.ctls[c];
        if (maxsz <> ctlist[cidx].startsize[gtype]) then
        begin
          needRecalcMaxSize := true;
          ctlist[cidx].startsize[gtype] := maxsz;
        end;
      end;
    end;
  end;
  // recalc maxsize if necessary
  if (needRecalcMaxSize) then calcMaxSizeInternal(0);
  // set "desired size" to "start size"
  for f := 0 to High(ctlist) do ctlist[f].desiredsize := ctlist[f].startsize;
  // set flags
  firstTime := true;
  //writeln('=== calculated max size ===');
  //dump();
end;


procedure TFlexLayouterBase.fixLine (me: PLayControl; i0, i1: LayControlIdx; cury: Integer; var spaceLeft: Single; var flexTotal: Integer; var flexBoxCount: Integer);
var
  curx: Integer;
  lc: PLayControl;
  osz: TLaySize;
  toadd: Integer;
  sti0: Integer;
  lineh: Integer;
begin
  curx := me.margins.left;
  sti0 := i0;
  // calc minimal line height
  lineh := 0;
  while (i0 <> i1) do
  begin
    lc := @ctlist[i0];
    lineh := nmax(lineh, lc.startsize.h);
    i0 := lc.nextSibling;
  end;
  // distribute space, expand/align
  i0 := sti0;
  while (i0 <> i1) do
  begin
    lc := @ctlist[i0];
    osz := lc.desiredsize;
    lc.desiredsize := lc.startsize;
    lc.desiredpos.x := curx;
    lc.desiredpos.y := cury;
    curx += lc.desiredsize.w;
    // fix flexbox size
    if (lc.tempFlex > 0) and (spaceLeft > 0) then
    begin
      toadd := trunc(spaceLeft*lc.tempFlex/flexTotal+0.5);
      if (toadd > 0) then
      begin
        // size changed
        lc.desiredsize.w += toadd;
        curx += toadd;
        // compensate (crudely) rounding errors
        if (curx > me.desiredsize.w-me.margins.horiz) then begin lc.desiredsize.w -= 1; curx -= 1; end;
        // relayout children
        layBox(lc.firstChild);
      end;
    end;
    // expand or align
         if (lc.expand) then lc.desiredsize.h := nmin(lc.maxsize.h, lineh) // expand
    else if (lc.aligndir > 0) then lc.desiredpos.y := cury+(lineh-lc.desiredsize.h) // bottom align
    else if (lc.aligndir = 0) then lc.desiredpos.y := cury+(lineh-lc.desiredsize.h) div 2; // center
    if (not osz.equals(lc.desiredsize)) then
    begin
      if (lc.inGroup) then groupElementChanged := true;
      // relayout children
      layBox(lc.firstChild);
    end;
    i0 := lc.nextSibling;
  end;
  flexTotal := 0;
  flexBoxCount := 0;
  spaceLeft := me.desiredsize.w-me.margins.horiz;
end;


// do box layouting; call `layBox()` recursively if necessary
procedure TFlexLayouterBase.layBox (boxidx: LayControlIdx);
var
  me: PLayControl;
  flexTotal: Integer; // total sum of flex fields
  flexBoxCount: Integer; // number of boxes
  spaceLeft: Single;
  cury: Integer;
  maxwdt, maxhgt: Integer;
  lineStartIdx: LayControlIdx;
  lc: PLayControl;
  doWrap: Boolean;
  toadd: Integer;
  osz: TLaySize;
begin
  if (boxidx < 0) or (boxidx >= Length(ctlist)) then exit;
  me := @ctlist[boxidx];

  // if we have no children, there's nothing to do
  if (me.firstChild = -1) then exit;

  // first, layout all children
  for lc in forChildren(boxidx) do layBox(lc.myidx);

  // second, layout lines, distribute flex data
  if (me.horizBox) then
  begin
    // horizontal boxes
    cury := me.margins.top;
    maxhgt := 0;

    fixLine(me, -1, -1, cury, spaceLeft, flexTotal, flexBoxCount); //HACK!

    lineStartIdx := me.firstChild;

    for lc in forChildren(boxidx) do
    begin
      // new line?
      doWrap := (not lc.firstInLine) and (lc.lineStart);
      // need to wrap?
      if (not doWrap) and (lc.canWrap) and (lc.canWrap) and (lc.desiredsize.w > 0) and (spaceLeft < lc.desiredsize.w) then doWrap := true;
      if (doWrap) then
      begin
        // new line, fix this one
        if (not lc.didWrap) then
        begin
          wrappingChanged := true;
          lc.didWrap := true;
        end;
        fixLine(me, lineStartIdx, lc.myidx, cury, spaceLeft, flexTotal, flexBoxCount);
        cury += maxhgt;
        lineStartIdx := lc.myidx;
      end
      else
      begin
        if (lc.didWrap) then
        begin
          wrappingChanged := true;
          lc.didWrap := false;
        end;
      end;
      spaceLeft -= lc.desiredsize.w;
      if (maxhgt < lc.desiredsize.h) then maxhgt := lc.desiredsize.h;
      if (lc.tempFlex > 0) then
      begin
        flexTotal += lc.tempFlex;
        flexBoxCount += 1;
      end;
    end;
    // fix last line
    fixLine(me, lineStartIdx, -1, cury, spaceLeft, flexTotal, flexBoxCount);
  end
  else
  begin
    // vertical boxes
    maxwdt := 0;
    flexTotal := 0;
    flexBoxCount := 0;
    spaceLeft := me.desiredsize.h-me.margins.vert;

    // calc flex
    for lc in forChildren(boxidx) do
    begin
      spaceLeft -= lc.desiredsize.h;
      if (maxwdt < lc.desiredsize.w) then maxwdt := lc.desiredsize.w;
      if (lc.tempFlex > 0) then
      begin
        flexTotal += lc.tempFlex;
        flexBoxCount += 1;
      end;
    end;

    // distribute space
    cury := me.margins.top;
    //writeln('me: ', boxidx, '; margins: ', me.margins.toString);
    for lc in forChildren(boxidx) do
    begin
      osz := lc.desiredsize;
      lc.desiredsize := lc.startsize;
      lc.desiredpos.x := me.margins.left;
      lc.desiredpos.y := cury;
      cury += lc.desiredsize.h;
      // fix flexbox size
      if (lc.tempFlex > 0) and (spaceLeft > 0) then
      begin
        toadd := trunc(spaceLeft*lc.tempFlex/flexTotal+0.5);
        if (toadd > 0) then
        begin
          // size changed
          lc.desiredsize.h += toadd;
          cury += toadd;
          // compensate (crudely) rounding errors
          if (cury > me.desiredsize.h-me.margins.vert) then begin lc.desiredsize.h -= 1; cury -= 1; end;
        end;
      end;
      // expand or align
           if (lc.expand) then lc.desiredsize.w := nmin(lc.maxsize.w, me.desiredsize.w-me.margins.vert) // expand
      else if (lc.aligndir > 0) then lc.desiredpos.x := me.desiredsize.w-me.margins.right-lc.desiredsize.w // right align
      else if (lc.aligndir = 0) then lc.desiredpos.x := (me.desiredsize.w-me.margins.horiz-lc.desiredsize.w) div 2; // center
      if (not osz.equals(lc.desiredsize)) then
      begin
        if (lc.inGroup) then groupElementChanged := true;
        // relayout children
        layBox(lc.firstChild);
      end;
    end;
  end;
end;


(*
second pass:
  calcluate desired sizes (process flexes) using 'startsize', set 'desiredsize' and 'desiredpos'
    if control has children, call 'second pass' recursively with this control as parent
  flags set:
    'group-element-changed', if any group element size was changed
    'wrapping-changed', if not 'firsttime', and wrapping was changed (i.e. first pass will not set the flag)
*)
procedure TFlexLayouterBase.secondPass ();
begin
  // reset flags
  groupElementChanged := false;
  wrappingChanged := false;

  if (Length(ctlist) > 0) then
  begin
    ctlist[0].desiredpos := TLayPos.Create(0, 0);
    layBox(0);
  end;

  // fix 'wrapping-changed' flag
  if (firstTime) then begin wrappingChanged := false; firstTime := false; end;
end;


(*
third pass:
  if 'group-element-changed':
    for each group: adjust controls to max desired size (startsize), set 'temp-flex' flags to 0 for 'em, set 'second-again' flag
  for other controls: if 'desiredsize' > 'maxsize', set 'startsize' to 'maxsize', set 'temp-flex' flag to 0, set 'second-again' flag
  if 'second-again' or 'wrapping-changed':
    reset 'second-again'
    reset 'wrapping-changed'
    reset 'firsttime'
    goto second pass
*)
procedure TFlexLayouterBase.thirdPass ();
var
  secondAgain: Boolean;
  gtype: Integer;
  maxsz: Integer;
  grp: PLayGroup;
  f, c: Integer;
  cidx: LayControlIdx;
  ct: PLayControl;
begin
  while true do
  begin
    secondPass();
    secondAgain := false;
    if (groupElementChanged) then
    begin
      secondAgain := true;
      // find max size for group, adjust 'startsize' controls to group max size
      for gtype := 0 to 1 do
      begin
        for f := 0 to High(groups[gtype]) do
        begin
          grp := @groups[gtype][f];
          maxsz := 0;
          for c := 0 to High(grp.ctls) do
          begin
            cidx := grp.ctls[c];
            ct := @ctlist[cidx];
            ct.expand := false; // don't expand grouped controls anymore
            if (maxsz < ct.startsize[gtype]) then maxsz := ct.startsize[gtype];
          end;
          for c := 0 to High(grp.ctls) do
          begin
            cidx := grp.ctls[c];
            ct := @ctlist[cidx];
            ct.startsize[gtype] := maxsz;
            ct.desiredsize[gtype] := maxsz;
            ct.tempFlex := 0; // don't change control size anymore
          end;
        end;
      end;
    end
    else
    begin
      for f := 0 to High(ctlist) do
      begin
        ct := @ctlist[f];
        if (ct.inGroup) then
        begin
          ct.expand := false; // don't expand grouped controls anymore
          ct.tempFlex := 0; // don't change control size anymore
        end;
        for c := 0 to 1 do
        begin
          if (ct.maxsize[c] <= 0) then continue;
          if (ct.desiredsize[c] > ct.maxsize[c]) then
          begin
            //writeln('ctl #', f, '; dimension #', c, ': desired=', ctlist[f].desiredsize[c], '; max=', ctlist[f].maxsize[c]);
            ct.startsize[c] := ct.maxsize[c];
            ct.desiredsize[c] := ct.maxsize[c];
            ct.tempFlex := 0; // don't change control size anymore
            secondAgain := true;
          end;
        end;
      end;
    end;
    if (not secondAgain) and (not wrappingChanged) then break;
    firstTime := false;
  end;
end;


(*
fourth pass:
  set 'actualsize' and 'actualpos' to 'desiredsize' and 'desiredpos'
  return
*)
procedure TFlexLayouterBase.fourthPass ();
var
  f: Integer;
begin
  for f := 0 to High(ctlist) do
  begin
    ctlist[f].ctl.setActualSizePos(ctlist[f].desiredpos, ctlist[f].desiredsize);
  end;
end;


procedure TFlexLayouterBase.layout ();
begin
  firstPass();
  secondPass();
  thirdPass();
  fourthPass();
end;


procedure TFlexLayouterBase.dumpFlat ();
var
  f: Integer;
  lc: PLayControl;
  ds, ms: TLaySize;
begin
  for f := 0 to High(ctlist) do
  begin
    lc := @ctlist[f];
    ds := lc.ctl.getDefSize;
    ms := lc.ctl.getMaxSize;
    writeln(lc.myidx, ': startsize:', lc.startsize.toString(), '; desiredsize=', lc.desiredsize.toString(), '; maxsize=', lc.maxsize.toString(), '; tempFlex=', lc.tempFlex, '; flags=', lc.flags,
      '; parent=', lc.parent, '; next=', lc.nextSibling, '; child=', lc.firstChild, '; ctl.size=', ds.toString(), '; ctl.maxsize=', ms.toString());
  end;
end;


procedure TFlexLayouterBase.dumpList (cidx: LayControlIdx; indent: Integer);
var
  lc: PLayControl;
  f: Integer;
begin
  while (cidx >= 0) do
  begin
    lc := @ctlist[cidx];
    for f := 0 to indent do write(' ');
    writeln(lc.myidx, ': startsize:', lc.startsize.toString, '; desiredsize=', lc.desiredsize.toString, '; maxsize=', lc.maxsize.toString, '; tempFlex=', lc.tempFlex, '; despos=', lc.desiredpos.toString);
    dumpList(lc.firstChild, indent+2);
    cidx := lc.nextSibling;
  end;
end;


procedure TFlexLayouterBase.dump ();
begin
  dumpList(0, 0);
end;


// ////////////////////////////////////////////////////////////////////////// //
(*
void main () begin
  auto win := new GuiControl();
  (win ~= new GuiControl()).mSize := TLaySize(10, 5);
  (win ~= new GuiControl()).mSize := TLaySize(16, 8);

  //win.mSize := TLaySize(40, 20);

  auto lay := TFlexLayouterBase!GuiControl();
  lay.setup(win);

  writeln('============================');
  lay.dumpFlat();

  writeln('=== initial ===');
  lay.dump();

  //lay.calcMaxSizeInternal(0);
  /*
  lay.firstPass();
  writeln('=== after first pass ===');
  lay.dump();

  lay.secondPass();
  writeln('=== after second pass ===');
  lay.dump();
  */
  lay.layout();
  writeln('=== final ===');
  lay.dump();
*)
end.
