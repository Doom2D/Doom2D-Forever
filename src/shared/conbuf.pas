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
{$MODE OBJFPC}
unit conbuf;

interface


procedure cbufPut (const s: AnsiString);
procedure cbufPutChars (buf: PChar; count: Integer);

function cbufLastChange (): LongWord;

function cbufWalkStart (): LongWord;
function cbufWalkEnd (pos: LongWord): LongWord;
procedure cbufPrev (var pos: LongWord);
procedure cbufNext (var pos: LongWord);

function cbufAt (const pos: LongWord): Char;

// get last line
procedure cbufLastLine (var sp: LongWord; var ep: LongWord);
// move one line up; `sp` and `ep` MUST be valid values from previous call to `cbufLastLine()`
function cbufLineUp (var sp: LongWord; var ep: LongWord): Boolean;

procedure cbufClear ();

var
  conbufDumpToStdOut: Boolean = false;


implementation


// ////////////////////////////////////////////////////////////////////////// //
//const ConBufSize = 64;
const ConBufSize = 256*1024;

// each line in buffer ends with '\n'; we don't keep offsets or lengthes, as
// it's fairly easy to search in buffer, and drawing console is not a common
// thing, so it doesn't have to be superfast.
var
  cbuf: packed array [0..ConBufSize-1] of Char;
  cbufhead: LongWord = 0;
  cbuftail: LongWord = 0; // `cbuftail` points *at* last char
  changeCount: LongWord = 1;


function cbufLastChange (): LongWord; begin result := changeCount; end;


// ////////////////////////////////////////////////////////////////////////// //
var
  needCon: Boolean = true;

procedure cbufPutChars (buf: PChar; count: Integer);
var
  np: LongWord;
  ch, och: Char;
begin
  if count > 0 then
  begin
    if conbufDumpToStdOut then
    begin
      for np := 0 to count-1 do
      begin
        if needCon then begin write(stdout, 'CON: '); needCon := false; end;
        write(stdout, buf[np]);
        needCon := (buf[np] = #10);
      end;
    end;
    Inc(changeCount);
    if changeCount = 0 then changeCount := 1;
    while count > 0 do
    begin
      Dec(count);
      ch := buf^;
      Inc(buf);
      np := (cbuftail+1) mod ConBufSize;
      if np = cbufhead then
      begin
        // we have to make some room; delete top line for this
        while true do
        begin
          och := cbuf[cbufhead];
          cbufhead := (cbufhead+1) mod ConBufSize;
          if (cbufhead = np) or (och = #10) then break;
        end;
      end;
      cbuf[np] := ch;
      cbuftail := np;
    end;
  end;
end;


procedure cbufPut (const s: AnsiString);
begin
  if length(s) > 0 then cbufPutChars(@s[1], length(s));
end;


// ////////////////////////////////////////////////////////////////////////// //
// warning! don't modify conbuf while the range is active!
function cbufWalkStart (): LongWord; begin result := cbuftail; end;
function cbufWalkEnd (pos: LongWord): LongWord; begin result := cbufhead; end;
procedure cbufPrev (var pos: LongWord); begin pos := (pos+ConBufSize-1) mod ConBufSize; end;
procedure cbufNext (var pos: LongWord); begin pos := (pos+1) mod ConBufSize; end;

function cbufAt (const pos: LongWord): Char; begin result := cbuf[pos mod ConBufSize]; end;


// ////////////////////////////////////////////////////////////////////////// //
procedure cbufLastLine (var sp: LongWord; var ep: LongWord);
var
  pos, pp: LongWord;
begin
  if cbufhead = cbuftail then
  begin
    sp := cbufhead;
    ep := cbufhead+1;
    exit;
  end;
  pos := cbuftail;
  ep := pos;
  while pos <> cbufhead do
  begin
    pp := (pos+ConBufSize-1) mod ConBufSize;
    if cbuf[pp] = #10 then break;
    pos := pp;
  end;
  sp := pos;
end;


function cbufLineUp (var sp: LongWord; var ep: LongWord): Boolean;
var
  pos, pp: LongWord;
begin
  if sp = cbufhead then begin sp := cbufhead; ep := cbufhead+1; result := false; exit; end;
  pos := (sp+ConBufSize-1) mod ConBufSize;
  if (pos = cbufhead) or (cbuf[pos] <> #10) then begin sp := cbufhead; ep := cbufhead+1; result := false; exit; end;
  ep := pos;
  while pos <> cbufhead do
  begin
    pp := (pos+ConBufSize-1) mod ConBufSize;
    if cbuf[pp] = #10 then break;
    pos := pp;
  end;
  sp := pos;
  result := true;
end;


procedure cbufClear ();
begin
  cbuf[0] := #10;
  cbufhead := 0;
  cbuftail := 0;
  Inc(changeCount);
  if changeCount = 0 then changeCount := 1;
end;


begin
  cbuf[0] := #10;
end.
