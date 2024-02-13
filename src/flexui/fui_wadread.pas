(* coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
 * Understanding is not required. Only obedience.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
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
{.$DEFINE FUI_WADREAD_DEBUG}
unit fui_wadread;

interface

uses
  SysUtils, Classes;


function fuiAddWad (const wadfile: AnsiString): Boolean;

// returns `nil` if file wasn't found
function fuiOpenFile (const fname: AnsiString): TStream;


var
  fuiDiskFirst: Boolean = true;


implementation

uses
  sfs, utils;


// ////////////////////////////////////////////////////////////////////////// //
type
  TFUIWad = class
  public
    wadname: AnsiString;
    iter: TSFSFileList;

  public
    constructor Create (const awadname: AnsiString);
    destructor Destroy (); override;

    // returns `nil` if file wasn't found
    function openFile (const fname: AnsiString): TStream;
  end;


constructor TFUIWad.Create (const awadname: AnsiString);
{$IFDEF FUI_WADREAD_DEBUG}
var
  f: Integer;
{$ENDIF}
begin
  if not SFSAddDataFile(awadname, true) then raise Exception.Create('cannot open wad');
  wadname := awadname;
  iter := SFSFileList(awadname);
{$IFDEF FUI_WADREAD_DEBUG}
  if (iter <> nil) then
  begin
    writeln('==== ', awadname, ' ====');
    for f := 0 to iter.Count-1 do
    begin
      if (iter.Files[f] = nil) then continue;
      writeln('  ', f, ': ', iter.Files[f].path, iter.Files[f].name);
    end;
    writeln('========');
  end;
{$ENDIF}
end;


destructor TFUIWad.Destroy ();
begin
  iter.Free();
  inherited;
end;


function TFUIWad.openFile (const fname: AnsiString): TStream;
var
  f: Integer;
  fi: TSFSFileInfo;
begin
  result := nil;
  if (iter = nil) then exit;
  // backwards, due to possible similar names and such
  for f := iter.Count-1 downto 0 do
  begin
    fi := iter.Files[f];
    if (fi = nil) then continue;
    if (StrEquCI1251(fi.path+fi.name, fname)) then
    begin
      try
        result := iter.volume.OpenFileByIndex(f);
      except
        result := nil;
      end;
      if (result <> nil) then exit;
    end;
  end;
end;



// ////////////////////////////////////////////////////////////////////////// //
function getExeDataPath (): AnsiString;
begin
  result := getFilenamePath(ParamStr(0))+'data/';
end;


// ////////////////////////////////////////////////////////////////////////// //
var
  wadlist: array of TFUIWad;

procedure Cleanup();
var
  wad: TFUIWad;
begin
  for wad in wadlist do
    wad.Destroy();
end;

function fuiAddWad (const wadfile: AnsiString): Boolean;
var
  exepath: AnsiString;
  awadname: AnsiString;
  f, c: Integer;
  wad: TFUIWad;
begin
  result := false;

  // find disk file
  if (Length(wadfile) = 0) then exit;

  if (Length(wadfile) > 2) and (wadfile[1] = '.') and ((wadfile[2] = '/') or (wadfile[2] = '\')) then
  begin
    awadname := wadfile;
    awadname := findDiskWad(awadname);
    if (Length(awadname) = 0) then
    begin
      writeln('WARNING: FlexUI WAD ''', wadfile, ''' not found');
      exit;
    end;
  end
  else
  begin
    exepath := getExeDataPath();
    awadname := exepath+wadfile;
    awadname := findDiskWad(awadname);
    if (Length(awadname) = 0) then
    begin
      awadname := wadfile;
      awadname := findDiskWad(awadname);
      if (Length(awadname) = 0) then
      begin
        writeln('WARNING: FlexUI WAD ''', exepath+wadfile, ''' not found');
        exit;
      end;
    end;
  end;

  // check if we already have this file opened
  for f := 0 to High(wadlist) do
  begin
    wad := wadlist[f];
    if (strEquCI1251(awadname, wad.wadname)) then
    begin
      // i found her! move it to the bottom of the list, so it will be checked first
      for c := f+1 to High(wadlist) do wadlist[c-1] := wadlist[c];
      wadlist[High(wadlist)] := wad;
      exit;
    end;
  end;

  // create new wad file
  try
    wad := TFUIWad.Create(awadname);
  except // sorry
    writeln('WARNING: error opening FlexUI WAD ''', wadfile, '''');
    exit;
  end;

  SetLength(wadlist, Length(wadlist)+1);
  wadlist[High(wadlist)] := wad;
  {$IFDEF FUI_WADREAD_DEBUG}writeln('FUI: added WAD: ''', wad.wadname, '''');{$ENDIF}

  result := true;
end;


// ////////////////////////////////////////////////////////////////////////// //
// returns `nil` if file wasn't found
function tryDiskFile (const fname: AnsiString): TStream;
var
  fn: AnsiString;
begin
  fn := getExeDataPath()+fname;
  try
    result := openDiskFileRO(fn);
    {$IFDEF FUI_WADREAD_DEBUG}writeln('FUI: opened DISK file: ''', fn, '''');{$ENDIF}
  except
    result := nil;
  end;
end;


// returns `nil` if file wasn't found
function fuiOpenFile (const fname: AnsiString): TStream;
var
  f: Integer;
begin
  // disk
  if (fuiDiskFirst) then
  begin
    result := tryDiskFile(fname);
    if (result <> nil) then exit;
  end;
  // wads
  for f := High(wadlist) downto 0 do
  begin
    result := wadlist[f].openFile(fname);
    if (result <> nil) then
    begin
      {$IFDEF FUI_WADREAD_DEBUG}writeln('FUI: opened WAD file: ''', fname, ''' (from ''', wadlist[f].wadname, ''')');{$ENDIF}
      exit;
    end;
  end;
  // disk
  if (not fuiDiskFirst) then
  begin
    result := tryDiskFile(fname);
    if (result <> nil) then exit;
  end;
  {$IFDEF FUI_WADREAD_DEBUG}writeln('FUI: file: ''', fname, ''' NOT FOUND!');{$ENDIF}
  result := nil;
end;

finalization
  Cleanup();

end.
