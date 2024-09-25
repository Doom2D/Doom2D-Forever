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
unit fui_flexlay;
(*
  control default size will be increased by margins
  negative margins are ignored
ControlT:
  procedure layPrepare (); // called before registering control in layouter
  function getDefSize (): TLaySize; // default size; <0: use max size
  function getMargins (): TLayMargins;
  function getPadding (): TLaySize; // children padding (each non-first child will get this on left/top)
  function getMaxSize (): TLaySize; // max size; <0: set to some huge value
  function getFlex (): Integer; // <=0: not flexible
  function isHorizBox (): Boolean; // horizontal layout for children?
  function noPad (): Boolean; // ignore padding in box direction for this control
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
  fui_common;


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
      FlagNoPad = LongWord(1) shl 1;
      FlagExpand = LongWord(1) shl 2;
      // internal
      FlagInGroupH = LongWord(1) shl 8; // set if this control is a member of any group
      FlagInGroupV = LongWord(1) shl 9; // set if this control is a member of any group

  private
    type
      PLayControl = ^TLayControl;
      TLayControl = record
      public
        myidx: LayControlIdx;
        tempFlex: Integer;
        flags: LongWord; // see above
        aligndir: Integer;
        startsize: TLaySize; // original size
        desiredsize: TLaySize; // current size
        maxsize: TLaySize; // current maximum size
        margins: TLayMargins; // can never be negative
        padding: TLaySize;
        desiredpos: TLayPos;
        ctl: ControlT;
        parent: LayControlIdx; // = -1;
        firstChild: LayControlIdx; // = -1;
        nextSibling: LayControlIdx; // = -1;

      public
        procedure initialize (); inline;

        function horizBox (): Boolean; inline;
        function inGroup (idx: Integer): Boolean; inline;
        function noPad (): Boolean; inline;

        function getExpand (): Boolean; inline;
        procedure setExpand (v: Boolean); inline;

        function alignLeft (): Boolean; inline;
        function alignTop (): Boolean; inline;
        function alignRight (): Boolean; inline;
        function alignBottom (): Boolean; inline;
        function alignCenter (): Boolean; inline;

        function visible (): Boolean; inline;

      public
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
    groupElementChanged: Boolean;

  private
    procedure firstTimeSetup (cidx: LayControlIdx);
    procedure doChildren (parent: LayControlIdx; child: ControlT);
    procedure appendToGroup (const gname: AnsiString;cidx: LayControlIdx;gidx: Integer);
    procedure clearGroups ();
    procedure setupGroups ();

    procedure distributeChildren (boxidx: LayControlIdx; maindir: Integer);
    // do box layouting; call `layBox()` recursively if necessary
    procedure layBox (boxidx: LayControlIdx);

    procedure firstPass ();
    procedure secondPass ();
    procedure thirdPass ();

    procedure dumpList (cidx: LayControlIdx; indent: Integer);

  public
    type
      TChildrenEnumerator = record
      private
        ctls: TLayCtlArray;
        cur: Integer;
        first: Boolean;
        onlyVisible: Boolean;
      public
        constructor Create (constref actls: TLayCtlArray; acur: Integer; aonlyvis: Boolean);
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
    function forVisibleChildren (cidx: LayControlIdx): TChildrenEnumerator; inline;

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
function TFlexLayouterBase.TLayControl.inGroup (idx: Integer): Boolean; inline; begin if (idx = 0) then result := ((flags and FlagInGroupH) <> 0) else if (idx = 1) then result := ((flags and FlagInGroupV) <> 0) else result := false; end;
function TFlexLayouterBase.TLayControl.noPad (): Boolean; inline; begin result := ((flags and FlagNoPad) <> 0); end;

function TFlexLayouterBase.TLayControl.getExpand (): Boolean; inline; begin result := ((flags and FlagExpand) <> 0); end;
procedure TFlexLayouterBase.TLayControl.setExpand (v: Boolean); inline; begin if (v) then flags := flags or FlagExpand else flags := flags and (not FlagExpand); end;

function TFlexLayouterBase.TLayControl.alignLeft (): Boolean; inline; begin result := (aligndir < 0); end;
function TFlexLayouterBase.TLayControl.alignTop (): Boolean; inline; begin result := (aligndir < 0); end;
function TFlexLayouterBase.TLayControl.alignRight (): Boolean; inline; begin result := (aligndir > 0); end;
function TFlexLayouterBase.TLayControl.alignBottom (): Boolean; inline; begin result := (aligndir > 0); end;
function TFlexLayouterBase.TLayControl.alignCenter (): Boolean; inline; begin result := (aligndir = 0); end;

function TFlexLayouterBase.TLayControl.visible (): Boolean; inline;
begin
  result := (startsize.w <> 0) or (startsize.h <> 0);
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TFlexLayouterBase.TChildrenEnumerator.Create (constref actls: TLayCtlArray; acur: Integer; aonlyvis: Boolean);
begin
  ctls := actls;
  cur := acur;
  first := true;
  onlyVisible := aonlyvis;
end;

function TFlexLayouterBase.TChildrenEnumerator.moveNext (): Boolean; inline;
begin
  while true do
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
    if (not result) or (not onlyVisible) then break;
    if (ctls[cur].visible) then break;
  end;
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
  groupElementChanged := false;
end;


destructor TFlexLayouterBase.Destroy ();
begin
  clear();
  inherited;
end;


function TFlexLayouterBase.forChildren (cidx: LayControlIdx): TChildrenEnumerator; inline;
begin
  result := TChildrenEnumerator.Create(ctlist, cidx, false);
end;

function TFlexLayouterBase.forVisibleChildren (cidx: LayControlIdx): TChildrenEnumerator; inline;
begin
  result := TChildrenEnumerator.Create(ctlist, cidx, true);
end;


procedure TFlexLayouterBase.clear ();
begin
  clearGroups();
  ctlist := nil;
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
    //if (msz.w = 0) or (msz.h = 0) then continue; // hidden controls will have zero maxsize, so skip 'em
    SetLength(ctlist, Length(ctlist)+1);
    lc := @ctlist[High(ctlist)];
    lc.initialize();
    if (cidx = -1) then
    begin
      cidx := LayControlIdx(High(ctlist));
      ctlist[parent].firstChild := cidx;
    end
    else
    begin
      ctlist[cidx].nextSibling := LayControlIdx(High(ctlist));
      cidx := LayControlIdx(High(ctlist));
    end;
    lc.myidx := cidx;
    lc.ctl := child;
    lc.parent := parent;
    doChildren(cidx, child.firstChild);
    child := child.nextSibling;
  end;
end;


procedure TFlexLayouterBase.appendToGroup (const gname: AnsiString; cidx: LayControlIdx; gidx: Integer);
var
  f: Integer;
  gflg: LongWord;
begin
  if (Length(gname) = 0) then exit;
  assert((cidx >= 0) and (cidx < Length(ctlist)));
  assert((gidx = 0) or (gidx = 1));
  if (gidx = 0) then gflg := FlagInGroupH else gflg := FlagInGroupV;
  ctlist[cidx].flags := ctlist[cidx].flags or gflg;
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


procedure TFlexLayouterBase.clearGroups ();
var
  gidx, f: Integer;
begin
  for gidx := 0 to 1 do
  begin
    for f := 0 to High(groups[gidx]) do groups[gidx][f].ctls := nil;
    groups[gidx] := nil;
  end;
end;


procedure TFlexLayouterBase.setupGroups ();
var
  gflg: LongWord;
  idx, gidx, f, c: Integer;
  lc: PLayControl;
begin
  clearGroups();
  for idx := 0 to High(ctlist) do
  begin
    lc := @ctlist[idx];
    appendToGroup(lc.ctl.getHGroup, LayControlIdx(idx), 0);
    appendToGroup(lc.ctl.getVGroup, LayControlIdx(idx), 1);
  end;
  // if control is only one in a group, mark is as "not grouped"
  for gidx := 0 to 1 do
  begin
    if (gidx = 0) then gflg := LongWord(not FlagInGroupH) else gflg := LongWord(not FlagInGroupV);
    f := 0;
    while (f < Length(groups[gidx])) do
    begin
      if (Length(groups[gidx][f].ctls) < 2) then
      begin
        // unmark controls
        for c := 0 to High(groups[gidx][f].ctls) do
        begin
          lc := @ctlist[groups[gidx][f].ctls[c]];
          lc.flags := lc.flags and gflg;
        end;
        // remove this group
        groups[gidx][f].ctls := nil;
        for c := f+1 to High(groups[gidx]) do groups[gidx][c-1] := groups[gidx][c];
        c := High(groups[gidx]);
        groups[gidx][c].ctls := nil;
        SetLength(groups[gidx], c);
      end
      else
      begin
        Inc(f);
      end;
    end;
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
    doChildren(0, root.firstChild);
  except
    clear();
    raise;
  end;
end;


procedure TFlexLayouterBase.firstTimeSetup (cidx: LayControlIdx);
var
  lc: PLayControl;
begin
  assert((cidx >= 0) and (cidx < Length(ctlist)));
  lc := @ctlist[cidx];
  lc.flags := 0;
  if (lc.ctl.isHorizBox) then lc.flags := lc.flags or FlagHorizBox;
  if (lc.ctl.getExpand) then lc.flags := lc.flags or FlagExpand;
  if (lc.ctl.noPad) then lc.flags := lc.flags or FlagNoPad;
  lc.aligndir := lc.ctl.getAlign;
  lc.startsize := lc.ctl.getDefSize;
  //lc.startsize.w := nmax(0, lc.startsize.w);
  //lc.startsize.h := nmax(0, lc.startsize.h);
  lc.margins := lc.ctl.getMargins;
  lc.margins.left := nmax(0, lc.margins.left);
  lc.margins.top := nmax(0, lc.margins.top);
  lc.margins.right := nmax(0, lc.margins.right);
  lc.margins.bottom := nmax(0, lc.margins.bottom);
  lc.padding := lc.ctl.getPadding;
  lc.padding.w := nmax(0, lc.padding.w);
  lc.padding.h := nmax(0, lc.padding.h);
  lc.maxsize := TLaySize.Create(-1, -1);
  if (lc.maxsize.w >= 0) then lc.startsize.w := nmin(lc.maxsize.w, lc.startsize.w);
  if (lc.maxsize.h >= 0) then lc.startsize.h := nmin(lc.maxsize.h, lc.startsize.h);
  lc.desiredsize := lc.startsize;
  lc.tempFlex := lc.ctl.getFlex;
end;


procedure TFlexLayouterBase.firstPass ();
var
  f: Integer;
  gtype: Integer;
begin
  groupElementChanged := false;
  setupGroups();
  for f := 0 to High(ctlist) do firstTimeSetup(f);
  // if we have any groups, set "group element changed" flag, so third pass will fix 'em
  for gtype := 0 to 1 do
  begin
    if (Length(groups[gtype]) > 0) then
    begin
      groupElementChanged := true;
      break;
    end;
  end;
end;


procedure TFlexLayouterBase.distributeChildren (boxidx: LayControlIdx; maindir: Integer);
var
  me, lc: PLayControl;
  suppdir: Integer;
  marg0, marg1, margtotal: Integer;
  marg0Op, marg1Op, margtotalOp: Integer;
  flexTotal: Integer = 0; // total sum of flex fields
  spaceLeft: Integer = 0;
  dopad: Boolean = false;
  prevpad: Boolean = false;
  maxdim: Integer = 0;
  curpos: Integer;
  toadd: Integer;
  pad: Integer;
  osz: TLaySize;
begin
  assert((boxidx >= 0) and (boxidx < Length(ctlist)));
  assert((maindir = 0) or (maindir = 1));
  // cache some parameters
  me := @ctlist[boxidx];
  suppdir := 1-maindir;
  if (maindir = 0) then
  begin
    marg0 := me.margins.left;
    marg1 := me.margins.right;
    marg0Op := me.margins.top;
    marg1Op := me.margins.bottom;
  end
  else
  begin
    marg0 := me.margins.top;
    marg1 := me.margins.bottom;
    marg0Op := me.margins.left;
    marg1Op := me.margins.right;
  end;
  margtotal := marg0+marg1;
  margtotalOp := marg0Op+marg1Op;
  // horizontal boxes
  pad := nmax(0, me.padding[maindir]);
  // calc required space, count flexes
  for lc in forVisibleChildren(boxidx) do
  begin
    if (lc.tempFlex > 0) then flexTotal += lc.tempFlex;
    spaceLeft += nmax(0, lc.desiredsize[maindir]);
    // insert padding if both current and previous children allow padding
    dopad := (not lc.noPad);
    if (prevpad) and (dopad) then spaceLeft += pad;
    prevpad := dopad;
    maxdim := nmax(maxdim, lc.desiredsize[suppdir]);
  end;
  // add margins
  spaceLeft += margtotal;
  maxdim += margtotalOp;
  // fix box size
  me.desiredsize[maindir] := nmax(spaceLeft, me.desiredsize[maindir]);
  me.desiredsize[suppdir] := nmax(maxdim, me.desiredsize[suppdir]);
  // calculate free space
  spaceLeft := me.desiredsize[maindir]-spaceLeft;
  // distribute children
  dopad := false;
  prevpad := false;
  curpos := marg0;
  for lc in forVisibleChildren(boxidx) do
  begin
    osz := lc.desiredsize;
    // main direction
    // insert padding if both current and previous children allow padding
    dopad := (not lc.noPad);
    if (prevpad) and (dopad) then curpos += pad;
    prevpad := dopad;
    lc.desiredpos[maindir] := curpos;
    if (lc.desiredsize[maindir] < 0) then lc.desiredsize[maindir] := 0;
    curpos += lc.desiredsize[maindir];
    // fix flexbox size
    //writeln(':lcidx=', lc.myidx, '; tempFlex=', lc.tempFlex, '; spaceLeft=', spaceLeft);
    if (spaceLeft > 0) and (lc.tempFlex > 0) then
    begin
      toadd := trunc(spaceLeft*lc.tempFlex/flexTotal+0.5);
      if (toadd > 0) then
      begin
        // size changed
        // compensate (crudely) rounding errors
        if (curpos+toadd > me.desiredsize[maindir]-margtotal) then toadd -= 1;
        //writeln('***curpos=', curpos, '; toadd=', toadd, '; spaceLeft=', spaceLeft);
        // fix size
        lc.desiredsize[maindir] := lc.desiredsize[maindir]+toadd;
        curpos += toadd;
      end;
    end;
    // secondary direction: expand or align
    if (lc.desiredsize[suppdir] < 0) then lc.desiredsize[suppdir] := 0;
    lc.desiredpos[suppdir] := marg0Op; // left/top align
         if (lc.expand) then lc.desiredsize[suppdir] := me.desiredsize[suppdir]-margtotalOp // expand
    else if (lc.aligndir > 0) then lc.desiredpos[suppdir] := me.desiredsize[suppdir]-marg1Op-lc.desiredsize[suppdir] // right/bottom align
    else if (lc.aligndir = 0) then lc.desiredpos[suppdir] := (me.desiredsize[suppdir]-lc.desiredsize[suppdir]) div 2; // center
    lc.desiredsize[suppdir] := nmax(lc.desiredsize[suppdir], osz[suppdir]);
    // relayout children if size was changed
    if (not osz.equals(lc.desiredsize)) then
    begin
      if (lc.inGroup(0)) or (lc.inGroup(1)) then groupElementChanged := true;
      layBox(lc.myidx);
    end;
  end;
end;


// do box layouting; call `layBox()` recursively if necessary
procedure TFlexLayouterBase.layBox (boxidx: LayControlIdx);
var
  me: PLayControl;
  lc: PLayControl;
  osz: TLaySize;
begin
  if (boxidx < 0) or (boxidx >= Length(ctlist)) then exit;
  me := @ctlist[boxidx];
  // if we have no children, there's nothing to do
  if (me.firstChild <> -1) then
  begin
    while true do
    begin
      osz := me.desiredsize;
      // layout all children
      for lc in forVisibleChildren(boxidx) do layBox(lc.myidx);
      // distribute children
      if (me.horizBox) then distributeChildren(me.myidx, 0) else distributeChildren(me.myidx, 1);
      // relayout children if size was changed
      if (osz.equals(me.desiredsize)) then break;
      if (me.inGroup(0)) or (me.inGroup(1)) then groupElementChanged := true;
    end;
  end;
end;


procedure TFlexLayouterBase.secondPass ();
var
  secondAgain: Boolean;
  gtype: Integer;
  maxsz: Integer;
  grp: PLayGroup;
  f, c: Integer;
  maindir: Integer;
  cidx: LayControlIdx;
  ct: PLayControl;
  loopsLeft: Integer = 64;
begin
  while (loopsLeft > 0) do
  begin
    Dec(loopsLeft);
    layBox(0);
    secondAgain := false;
    if (groupElementChanged) then
    begin
      secondAgain := true;
      groupElementChanged := false;
      // fix group sizes
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
            maxsz := nmax(maxsz, ct.desiredsize[gtype]);
          end;
          for c := 0 to High(grp.ctls) do
          begin
            cidx := grp.ctls[c];
            ct := @ctlist[cidx];
            ct.desiredsize[gtype] := maxsz;
          end;
        end;
      end;
    end;
    // don't change group control sizes anymore
    for f := 0 to High(ctlist) do
    begin
      ct := @ctlist[f];
      if (ct.parent <> -1) then
      begin
        if (ctlist[ct.parent].horizBox) then maindir := 0 else maindir := 1;
      end
      else
      begin
        maindir := 0; // arbitrary
      end;
      if (ct.inGroup(maindir)) then ct.tempFlex := 0; // don't change control size anymore
      if (ct.inGroup(1-maindir)) then ct.expand := false; // don't expand grouped controls anymore
    end;
    if (not secondAgain) then break;
  end;
end;


procedure TFlexLayouterBase.thirdPass ();
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
  if (Length(ctlist) = 0) then exit;
  ctlist[0].desiredpos := TLayPos.Create(0, 0);
  firstPass();
  //writeln('============== AFTER FIRST PASS =============='); dump();
  secondPass();
  //writeln('============== AFTER SECOND PASS =============='); dump();
  thirdPass();
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
    if (not lc.visible) then write('!');
    write(lc.myidx, ': ');
    if (lc.ctl.id <> '') then write('<', lc.ctl.className, '> {', lc.ctl.id, '} ') else write('<', lc.ctl.className, '> ');
    writeln('startsize:', lc.startsize.toString, '; desiredsize=', lc.desiredsize.toString, '; maxsize=', lc.maxsize.toString, '; tempFlex=', lc.tempFlex, '; despos=', lc.desiredpos.toString);
    dumpList(lc.firstChild, indent+2);
    cidx := lc.nextSibling;
  end;
end;


procedure TFlexLayouterBase.dump ();
begin
  dumpList(0, 0);
end;


end.
