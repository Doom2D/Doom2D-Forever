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
procedure e_WriteLog (TextLine: String; RecordCategory: TRecordCategory; WriteTime: Boolean=True);
function DecodeIPV4 (ip: LongWord): string;


// start Write/WriteLn driver. it will write everything to cbuf.
procedure e_InitWritelnDriver ();


var
  e_WriteToStdOut: Boolean = False;


implementation

uses
  conbuf;


var
  FirstRecord: Boolean;
  FileName: String;
  driverInited: Boolean = false;


function DecodeIPV4 (ip: LongWord): string;
begin
  Result := Format('%d.%d.%d.%d', [ip and $FF, (ip shr 8) and $FF, (ip shr 16) and $FF, (ip shr 24)]);
end;


procedure e_WriteLog (TextLine: String; RecordCategory: TRecordCategory; WriteTime: Boolean=True);
var
  LogFile: TextFile;
  Prefix: ShortString = '';
  OutStr: String;
begin
  if driverInited and (length(TextLine) > 0) then
  begin
    case RecordCategory of
      MSG_FATALERROR: write('FATAL: ');
      MSG_WARNING: write('WARNING: ');
    end;
    writeln(TextLine);
  end;

  if FileName = '' then Exit;

  Assign(LogFile, FileName);
  try
    if FileExists(FileName) then
      Append(LogFile)
    else
      Rewrite(LogFile);
    try
      if FirstRecord then
      begin
        Writeln(LogFile, '--- Log started at '+TimeToStr(Time)+' ---');
        FirstRecord := False;
      end;
      case RecordCategory of
        MSG_FATALERROR: Prefix := '!!!';
        MSG_WARNING:    Prefix := '!  ';
        MSG_NOTIFY:     Prefix := '***';
      end;
      if WriteTime then
        OutStr := '['+TimeToStr(Time)+'] '+Prefix+' '+TextLine
      else
        OutStr := Prefix+' '+TextLine;
      Writeln(LogFile, OutStr);
      if e_WriteToStdOut then
        Writeln(OutStr);
    finally
      Close(LogFile);
    end;
  except // sorry
  end;
end;


procedure e_InitLog (fFileName: String; fWriteMode: TWriteMode);
begin
 FileName := fFileName;
 if fWriteMode = WM_NEWFILE then
 begin
   try
     if FileExists(FileName) then DeleteFile(FileName);
   except // sorry
   end;
 end;
 FirstRecord := True;
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
