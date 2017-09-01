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
{$R-}
{ $DEFINE CBLOG}
unit e_log;

interface

uses
  SysUtils;

type
  TWriteMode = (WM_NEWFILE,  WM_OLDFILE);
  TRecordCategory = (MSG_FATALERROR, MSG_WARNING, MSG_NOTIFY);


procedure e_InitLog (fFileName: String; fWriteMode: TWriteMode);
procedure e_DeinitLog ();

procedure e_SetSafeSlowLog (slowAndSafe: Boolean);

procedure e_WriteLog (TextLine: String; RecordCategory: TRecordCategory; WriteTime: Boolean=True);

function DecodeIPV4 (ip: LongWord): string;

// start Write/WriteLn driver. it will write everything to cbuf.
procedure e_InitWritelnDriver ();

procedure e_LogWritefln (const fmt: AnsiString; args: array of const; category: TRecordCategory=MSG_NOTIFY; writeTime: Boolean=true);
procedure e_LogWriteln (const s: AnsiString; category: TRecordCategory=MSG_NOTIFY; writeTime: Boolean=true);


var
  e_WriteToStdOut: Boolean = False;


implementation

uses
  conbuf, utils;

var
  FirstRecord: Boolean;
  FileName: String;
  driverInited: Boolean = false;


function DecodeIPV4 (ip: LongWord): string;
begin
  Result := Format('%d.%d.%d.%d', [ip and $FF, (ip shr 8) and $FF, (ip shr 16) and $FF, (ip shr 24)]);
end;


procedure e_WriteLog (TextLine: String; RecordCategory: TRecordCategory; WriteTime: Boolean=True);
begin
  e_LogWritefln('%s', [TextLine], RecordCategory, WriteTime);
end;


procedure e_LogWriteln (const s: AnsiString; category: TRecordCategory=MSG_NOTIFY; writeTime: Boolean=true);
begin
  e_LogWritefln('%s', [s], category, writeTime);
end;


// returns formatted string if `writerCB` is `nil`, empty string otherwise
//function formatstrf (const fmt: AnsiString; args: array of const; writerCB: TFormatStrFCallback=nil): AnsiString;
//TFormatStrFCallback = procedure (constref buf; len: SizeUInt);

procedure conwriter (constref buf; len: SizeUInt);
var
  ss: ShortString;
  slen: Integer;
  b: PByte;
begin
  if (len < 1) then exit;
  b := PByte(@buf);
  while (len > 0) do
  begin
    if (len > 255) then slen := 255 else slen := Integer(len);
    Move(b^, ss[1], slen);
    ss[0] := AnsiChar(slen);
    write(ss);
    b += slen;
    len -= slen;
  end;
end;


var
  xlogFile: TextFile;
  xlogFileOpened: Boolean = false;
  xlogPrefix: AnsiString;
  xlogLastWasEOL: Boolean = false;
  xlogWantSpace: Boolean = false;
  xlogSlowAndSafe: Boolean = false;


procedure e_SetSafeSlowLog (slowAndSafe: Boolean);
begin
  xlogSlowAndSafe := slowAndSafe;
  if xlogSlowAndSafe and xlogFileOpened then
  begin
    CloseFile(xlogFile);
    xlogFileOpened := false;
  end;
end;


procedure logwriter (constref buf; len: SizeUInt);
var
  ss: ShortString;
  slen: Integer;
  b: PByte;
begin
  if (len < 1) then exit;
  b := PByte(@buf);
  if xlogLastWasEOL then
  begin
    write(xlogFile, xlogPrefix);
    xlogLastWasEOL := false;
    xlogWantSpace := true;
  end;
  while (len > 0) do
  begin
    slen := 0;
    while (slen < len) and (b[slen] <> 13) and (b[slen] <> 10) do Inc(slen);
    if (slen > 255) then slen := 255;
    // print string
    if (slen > 0) then
    begin
      if xlogWantSpace then begin write(xlogFile, ' '); xlogWantSpace := false; end;
      Move(b^, ss[1], slen);
      ss[0] := AnsiChar(slen);
      write(xlogFile, ss);
      b += slen;
      len -= slen;
      continue;
    end;
    // process newline
    if (len > 0) and ((b[0] = 13) or (b[0] = 10)) then
    begin
      if (b[0] = 13) then begin len -= 1; b += 1; end;
      if (len > 0) and (b[0] = 10) then begin len -= 1; b += 1; end;
      xlogLastWasEOL := false;
      writeln(xlogFile, '');
      write(xlogFile, xlogPrefix);
    end;
  end;
end;


procedure e_LogWritefln (const fmt: AnsiString; args: array of const; category: TRecordCategory=MSG_NOTIFY; writeTime: Boolean=true);

  procedure xwrite (const s: AnsiString);
  begin
    if (Length(s) = 0) then exit;
    logwriter(PAnsiChar(s)^, Length(s));
  end;

begin
  if driverInited and (length(fmt) > 0) then
  begin
    case category of
      MSG_FATALERROR: write('FATAL: ');
      MSG_WARNING: write('WARNING: ');
    end;
    formatstrf(fmt, args, conwriter);
    writeln;
  end;

  if (FileName = '') then exit;

  if not xlogFileOpened then
  begin
    AssignFile(xlogFile, FileName);
    try
      if FileExists(FileName) then Append(xlogFile) else Rewrite(xlogFile);
      xlogFileOpened := true;
    except // sorry
      exit;
    end;
  end;

  if FirstRecord then
  begin
    writeln(xlogFile, '--- Log started at ', TimeToStr(Time), ' ---');
    FirstRecord := false;
  end;

  xlogPrefix := '';
  if writeTime then begin xlogPrefix += '['; xlogPrefix += TimeToStr(Time); xlogPrefix += '] '; end;
  case category of
    MSG_FATALERROR: xlogPrefix += '!!!';
    MSG_WARNING: xlogPrefix += '!  ';
    MSG_NOTIFY: xlogPrefix += '***';
  end;
  xlogLastWasEOL := true; // to output prefix
  xlogWantSpace := true; // after prefix
  formatstrf(fmt, args, logwriter);
  if not xlogLastWasEOL then writeln(xlogFile, '') else writeln(xlogFile, xlogPrefix);

  if xlogSlowAndSafe and xlogFileOpened then
  begin
    CloseFile(xlogFile);
    xlogFileOpened := false;
  end;

  //if fopened then CloseFile(xlogFile);
end;


procedure e_InitLog (fFileName: String; fWriteMode: TWriteMode);
begin
  if xlogFileOpened then CloseFile(xlogFile);
  xlogFileOpened := false;
  FileName := fFileName;
  if (fWriteMode = WM_NEWFILE) then
  begin
    try
      if FileExists(FileName) then DeleteFile(FileName);
    except // sorry
    end;
  end;
  FirstRecord := true;
end;


procedure e_DeinitLog ();
begin
  if xlogFileOpened then CloseFile(xlogFile);
  xlogFileOpened := false;
end;


// ////////////////////////////////////////////////////////////////////////// //
(* Write/WriteLn driver *)
//
// control codes:
//   CR, LF, BS
//   TAB: tab space = 4
//
//   userData[1]: current x (for tabs)
//   userData[2]: #13 was eaten, we should skip next #10
//
type
  TDevFunc = function (var f: TTextRec): Integer;

const
  udX = 1;
  udWasCR = 2;


procedure ProcessOutput (var tf: TTextRec; buf: PChar; count: Integer);
var
  wcr: Boolean;
  ep: PChar;
  f, x: Integer;
  ch: Char;
begin
  x := tf.userData[udX];
  wcr := (tf.userData[udWasCR] <> 0);
  while count > 0 do
  begin
    // look for some special char
    ep := buf;
    f := 0;
    while f < count do
    begin
      ch := ep^;
      if (ch = #13) or (ch = #10) or (ch = #9) or (ch = #8) then break;
      Inc(ep);
      Inc(f);
{$IFDEF CBLOG}
      write(stderr, ch);
{$ENDIF}
    end;
    if f > 0 then
    begin
      wcr := false;
      cbufPutChars(buf, f);
      Inc(buf, f);
      Dec(count, f);
      Inc(x, f);
      continue;
    end;
    // process special chars
    ch := buf^;
    Inc(buf);
    Dec(count);
    // tab
    if ch = #9 then
    begin
{$IFDEF CBLOG}
      write(stderr, ch);
{$ENDIF}
      repeat
        cbufPut(' ');
        Inc(x);
      until (x mod 4) = 0;
      continue;
    end;
    // cr, lf
    if (ch = #13) or (ch = #10) then
    begin
{$IFDEF CBLOG}
      writeln(stderr);
{$ENDIF}
      if not wcr or (ch <> #10) then
      begin
        wcr := (ch = #13);
        x := 0;
        cbufPut(#10);
      end;
      continue;
    end;
  end;
  tf.userData[udX] := x;
  tf.userData[udWasCR] := ord(wcr);
end;


function DevOpen (var f: TTextRec): Integer;
begin
  f.userData[udX] := 0;
  f.userData[udWasCR] := 0;
  f.bufPos := 0;
  f.bufEnd := 0;
  result := 0;
end;

function DevInOut (var f: TTextRec): Integer;
var
  buf: PChar;
  sz: Integer;
begin
  result := 0;
  buf := Pointer(f.BufPtr);
  sz := f.BufPos;
  if sz > 0 then ProcessOutput(f, buf, sz);
  f.bufPos := 0;
  f.bufEnd := 0;
end;

function DevFlush (var f: TTextRec): Integer;
begin
  result := DevInOut(f);
end;

function DevClose (var f: TTextRec): Integer;
begin
  result := 0;
end;


procedure e_InitWritelnDriver ();
begin
  if not driverInited then
  begin
    driverInited := true;
    with TTextRec(output) do
    begin
      Mode := fmClosed;
      if BufPtr = nil then
      begin
        BufSize := SizeOf(Buffer);
        BufPtr := @Buffer;
      end;
      OpenFunc := @DevOpen;
      InOutFunc := @DevInOut;
      FlushFunc := @DevFlush;
      CloseFunc := @DevClose;
      Name[0] := #0;
    end;
    Rewrite(output);
  end;
end;


begin
  //e_InitWritelnDriver();
end.
