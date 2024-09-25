(* Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
 * This file is part of Doom2D Forever.
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

// ////////////////////////////////////////////////////////////////////////// //
type
  TBindArgLessCB = procedure ();
  TBindToggleCB = procedure (arg: Integer); // -1: no arg
  TBindStringCB = procedure (s: AnsiString);

  PHolmesCommand = ^THolmesCommand;
  THolmesCommand = record
  public
    type TType = (TArgLess, TToggle, TString);

  public
    name: AnsiString;
    help: AnsiString;
    section: AnsiString;
    cb: Pointer;
    ctype: TType;
    helpmark: Boolean;

    // command name already taken
    procedure execute (pr: TTextParser);
  end;

  PHolmesBinding = ^THolmesBinding;
  THolmesBinding = record
    key: AnsiString; // or mouse
    cmd: AnsiString;

    function cmdName (): AnsiString;
  end;

  TCmdHash = specialize THashBase<AnsiString, PHolmesCommand, THashKeyStr>;


// ////////////////////////////////////////////////////////////////////////// //
function THolmesBinding.cmdName (): AnsiString;
var
  pr: TTextParser = nil;
begin
  try
    pr := TStrTextParser.Create(cmd);
    if (pr.tokType = pr.TTStr)
      then Result := pr.expectStr(false)
      else Result := pr.expectId();
  except
    Result := '';
  end;
  pr.Free();
end;


// ////////////////////////////////////////////////////////////////////////// //
// command name already taken
procedure THolmesCommand.execute (pr: TTextParser);
var
  a: Integer = -1;
  s: AnsiString = '';
begin
  if not assigned(cb) then exit;
  case ctype of
    TType.TToggle:
      begin
        if (pr.tokType <> pr.TTEOF) then
        begin
               if pr.eatId('true') or pr.eatId('yes') then a := 1
          else if pr.eatId('false') or pr.eatId('no') then a := 0
          else begin conwritefln('%s: invalid argument', [name]); exit; end;
          if (pr.tokType <> pr.TTEOF) then begin conwritefln('%s: too many arguments', [name]); exit; end;
        end;
        TBindToggleCB(cb)(a);
      end;
    TType.TArgLess:
      begin
        if (pr.tokType <> pr.TTEOF) then begin conwritefln('%s: too many arguments', [name]); exit; end;
        TBindArgLessCB(cb)();
      end;
    TType.TString:
      begin
        if (pr.tokType <> pr.TTEOF) then
        begin
          if (pr.tokType = pr.TTStr) then s := pr.expectStr(false) else s := pr.expectId;
          if (pr.tokType <> pr.TTEOF) then begin conwritefln('%s: too many arguments', [name]); exit; end;
        end
        else
        begin
          conwritefln('%s: string argument expected', [name]);
          exit;
        end;
        TBindStringCB(cb)(s);
      end;
    else assert(false);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function hashNewCommand (): TCmdHash;
begin
  result := TCmdHash.Create();
end;


// ////////////////////////////////////////////////////////////////////////// //
type
  PHBA = ^THBA;
  THBA = array of THolmesBinding;


var
  cmdlist: TCmdHash = nil;
  keybinds: THBA = nil;
  msbinds: THBA = nil;
  keybindsInited: Boolean = false;


// ////////////////////////////////////////////////////////////////////////// //
function cmdNewInternal (const aname: AnsiString; const ahelp: AnsiString; const asection: AnsiString): PHolmesCommand;
begin
  if (cmdlist = nil) then cmdlist := hashNewCommand();
  if not cmdlist.get(aname, result) then
  begin
    GetMem(result, sizeof(THolmesCommand));
    FillChar(result^, sizeof(THolmesCommand), 0);
    result.name := aname;
    result.help := ahelp;
    result.section := asection;
    result.cb := nil;
    result.ctype := result.TType.TArgLess;
    cmdlist.put(aname, result);
  end
  else
  begin
    result.help := ahelp;
    result.section := asection;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure cmdAdd (const aname: AnsiString; cb: TBindArgLessCB; const ahelp: AnsiString; const asection: AnsiString); overload;
var
  cmd: PHolmesCommand;
begin
  if (Length(aname) = 0) or not assigned(cb) then exit;
  cmd := cmdNewInternal(aname, ahelp, asection);
  cmd.cb := Pointer(@cb);
  cmd.ctype := cmd.TType.TArgLess;
end;


procedure cmdAdd (const aname: AnsiString; cb: TBindToggleCB; const ahelp: AnsiString; const asection: AnsiString); overload;
var
  cmd: PHolmesCommand;
begin
  if (Length(aname) = 0) or not assigned(cb) then exit;
  cmd := cmdNewInternal(aname, ahelp, asection);
  cmd.cb := Pointer(@cb);
  cmd.ctype := cmd.TType.TToggle;
end;


procedure cmdAdd (const aname: AnsiString; cb: TBindStringCB; const ahelp: AnsiString; const asection: AnsiString); overload;
var
  cmd: PHolmesCommand;
begin
  if (Length(aname) = 0) or not assigned(cb) then exit;
  cmd := cmdNewInternal(aname, ahelp, asection);
  cmd.cb := Pointer(@cb);
  cmd.ctype := cmd.TType.TString;
end;


// ////////////////////////////////////////////////////////////////////////// //
function getCommandHelp (const aname: AnsiString): AnsiString;
var
  cmd: PHolmesCommand = nil;
begin
  if not cmdlist.get(aname, cmd) then result := '' else result := cmd.help;
end;


function getCommandSection (const aname: AnsiString): AnsiString;
var
  cmd: PHolmesCommand = nil;
begin
  if not cmdlist.get(aname, cmd) then result := '' else result := cmd.section;
end;


// ////////////////////////////////////////////////////////////////////////// //
function internalBindAdd (ba: PHBA; const akey: AnsiString; const acmd: AnsiString): Boolean;
var
  f, c: Integer;
begin
  for f := 0 to High(ba^) do
  begin
    if (CompareText(ba^[f].key, akey) = 0) then
    begin
      if (Length(acmd) = 0) then
      begin
        // remove
        result := false;
        for c := f+1 to High(ba^) do ba^[c-1] := ba^[c];
        SetLength(ba^, Length(ba^)-1);
      end
      else
      begin
        // replace
        result := true;
        ba^[f].cmd := acmd;
      end;
      exit;
    end;
  end;
  if (Length(acmd) > 0) then
  begin
    result := true;
    SetLength(ba^, Length(ba^)+1);
    ba^[High(ba^)].key := akey;
    ba^[High(ba^)].cmd := acmd;
  end
  else
  begin
    result := false;
  end;
end;


procedure keybindAdd (const akey: AnsiString; const acmd: AnsiString);
begin
  internalBindAdd(@keybinds, akey, acmd);
  keybindsInited := true;
end;

procedure msbindAdd (const akey: AnsiString; const acmd: AnsiString);
begin
  internalBindAdd(@msbinds, akey, acmd);
  keybindsInited := true;
end;


procedure execCommand (const s: AnsiString);
var
  pr: TTextParser = nil;
  cmd: AnsiString;
  cc: PHolmesCommand;
begin
  if (cmdlist = nil) then
  begin
    conwriteln('holmes command system is not initialized!');
    exit;
  end;
  try
    pr := TStrTextParser.Create(s);
    if (pr.tokType = pr.TTStr) then cmd := pr.expectStr(false) else cmd := pr.expectId();
    if cmdlist.get(cmd, cc) then cc.execute(pr) else conwritefln('holmes command ''%s'' not found', [cmd]);
  except
    on E: Exception do
    begin
      conwritefln('error executing holmes command: [%s]', [s]);
      //conwritefln('* [%s] [%s]', [Integer(pr.tokType), E.message]);
    end;
  end;
  pr.Free();
end;


function keybindExecute (var ev: TFUIEvent): Boolean;
var
  f: Integer;
begin
  result := false;
  for f := 0 to High(keybinds) do
  begin
    if (ev = keybinds[f].key) then
    begin
      result := true;
      //conwritefln('found command [%s] for keybind <%s>', [keybinds[f].cmd, keybinds[f].key]);
      execCommand(keybinds[f].cmd);
      exit;
    end;
  end;
end;


function msbindExecute (var ev: TFUIEvent): Boolean;
var
  f: Integer;
begin
  result := false;
  for f := 0 to High(msbinds) do
  begin
    if (ev = msbinds[f].key) then
    begin
      result := true;
      execCommand(msbinds[f].cmd);
      exit;
    end;
  end;
end;
