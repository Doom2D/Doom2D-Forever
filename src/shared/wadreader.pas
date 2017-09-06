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
{$INCLUDE a_modes.inc}
unit wadreader;

{$DEFINE SFS_DFWAD_DEBUG}
{$DEFINE SFS_MAPDETECT_FX}

interface

uses
  sfs, xstreams, Classes;


type
  SArray = array of ShortString;

  TWADFile = class
  private
    fFileName: AnsiString; // empty: not opened
    fIter: TSFSFileList;

    function getIsOpen (): Boolean;
    function isMapResource (idx: Integer): Boolean;

    function GetResourceEx (name: AnsiString; wantMap: Boolean; var pData: Pointer; var Len: Integer; logError: Boolean=true): Boolean;

   public
    constructor Create ();
    destructor Destroy (); override;

    procedure FreeWAD ();

    function ReadFile (FileName: AnsiString): Boolean;
    function ReadMemory (Data: Pointer; Len: LongWord): Boolean;

    function GetResource (name: AnsiString; var pData: Pointer; var Len: Integer; logError: Boolean=true): Boolean;
    function GetMapResource (name: AnsiString; var pData: Pointer; var Len: Integer; logError: Boolean=true): Boolean;
    function GetMapResources (): SArray;

    // returns `nil` if file wasn't found
    function openFileStream (name: AnsiString): TStream;

    property isOpen: Boolean read getIsOpen;
  end;


function g_ExtractWadName (resourceStr: AnsiString): AnsiString;
function g_ExtractWadNameNoPath (resourceStr: AnsiString): AnsiString;
function g_ExtractFilePath (resourceStr: AnsiString): AnsiString;
function g_ExtractFileName (resourceStr: AnsiString): AnsiString; // without path
function g_ExtractFilePathName (resourceStr: AnsiString): AnsiString;

// return fixed AnsiString or empty AnsiString
function findDiskWad (fname: AnsiString): AnsiString;


var
  wadoptDebug: Boolean = false;
  wadoptFast: Boolean = false;


implementation

uses
  SysUtils, e_log, utils, MAPDEF, xdynrec;


function findDiskWad (fname: AnsiString): AnsiString;
begin
  result := '';
  if not findFileCI(fname) then
  begin
    //e_WriteLog(Format('findDiskWad: error looking for [%s]', [fname]), MSG_NOTIFY);
    if StrEquCI1251(ExtractFileExt(fname), '.wad') then
    begin
      fname := ChangeFileExt(fname, '.pk3');
      //e_WriteLog(Format('  looking for [%s]', [fname]), MSG_NOTIFY);
      if not findFileCI(fname) then
      begin
        fname := ChangeFileExt(fname, '.zip');
        //e_WriteLog(Format('  looking for [%s]', [fname]), MSG_NOTIFY);
        if not findFileCI(fname) then exit;
      end;
    end
    else
    begin
      exit;
    end;
  end;
  //e_WriteLog(Format('findDiskWad: FOUND [%s]', [fname]), MSG_NOTIFY);
  result := fname;
end;


function normSlashes (s: AnsiString): AnsiString;
var
  f: Integer;
begin
  for f := 1 to length(s) do if s[f] = '\' then s[f] := '/';
  result := s;
end;

function g_ExtractWadNameNoPath (resourceStr: AnsiString): AnsiString;
var
  f, c: Integer;
begin
  for f := length(resourceStr) downto 1 do
  begin
    if resourceStr[f] = ':' then
    begin
      result := normSlashes(Copy(resourceStr, 1, f-1));
      c := length(result);
      while (c > 0) and (result[c] <> '/') do Dec(c);
      if c > 0 then result := Copy(result, c+1, length(result));
      exit;
    end;
  end;
  result := '';
end;

function g_ExtractWadName (resourceStr: AnsiString): AnsiString;
var
  f: Integer;
begin
  for f := length(resourceStr) downto 1 do
  begin
    if resourceStr[f] = ':' then
    begin
      result := normSlashes(Copy(resourceStr, 1, f-1));
      exit;
    end;
  end;
  result := '';
end;

function g_ExtractFilePath (resourceStr: AnsiString): AnsiString;
var
  f, lastSlash: Integer;
begin
  result := '';
  lastSlash := -1;
  for f := length(resourceStr) downto 1 do
  begin
    if (lastSlash < 0) and (resourceStr[f] = '\') or (resourceStr[f] = '/') then lastSlash := f;
    if resourceStr[f] = ':' then
    begin
      if lastSlash > 0 then
      begin
        result := normSlashes(Copy(resourceStr, f, lastSlash-f));
        while (length(result) > 0) and (result[1] = '/') do Delete(result, 1, 1);
      end;
      exit;
    end;
  end;
  if lastSlash > 0 then result := normSlashes(Copy(resourceStr, 1, lastSlash-1));
end;

function g_ExtractFileName (resourceStr: AnsiString): AnsiString; // without path
var
  f, lastSlash: Integer;
begin
  result := '';
  lastSlash := -1;
  for f := length(resourceStr) downto 1 do
  begin
    if (lastSlash < 0) and (resourceStr[f] = '\') or (resourceStr[f] = '/') then lastSlash := f;
    if resourceStr[f] = ':' then
    begin
      if lastSlash > 0 then result := Copy(resourceStr, lastSlash+1, length(resourceStr));
      exit;
    end;
  end;
  if lastSlash > 0 then result := Copy(resourceStr, lastSlash+1, length(resourceStr));
end;

function g_ExtractFilePathName (resourceStr: AnsiString): AnsiString;
var
  f: Integer;
begin
  result := '';
  for f := length(resourceStr) downto 1 do
  begin
    if resourceStr[f] = ':' then
    begin
      result := normSlashes(Copy(resourceStr, f+1, length(resourceStr)));
      while (length(result) > 0) and (result[1] = '/') do Delete(result, 1, 1);
      exit;
    end;
  end;
  result := normSlashes(resourceStr);
  while (length(result) > 0) and (result[1] = '/') do Delete(result, 1, 1);
end;



{ TWADFile }
constructor TWADFile.Create();
begin
  fFileName := '';
end;


destructor TWADFile.Destroy();
begin
  FreeWAD();
  inherited;
end;


function TWADFile.getIsOpen (): Boolean;
begin
  result := (fFileName <> '');
end;


procedure TWADFile.FreeWAD();
begin
  if fIter <> nil then FreeAndNil(fIter);
  //if fFileName <> '' then e_WriteLog(Format('TWADFile.ReadFile: [%s] closed', [fFileName]), MSG_NOTIFY);
  fFileName := '';
end;


//FIXME: detect text maps properly here
function TWADFile.isMapResource (idx: Integer): Boolean;
var
  //sign: packed array [0..2] of Char;
  fs: TStream = nil;
begin
  result := false;
  if not isOpen or (fIter = nil) then exit;
  if (idx < 0) or (idx >= fIter.Count) then exit;
  try
    fs := fIter.volume.OpenFileByIndex(idx);
    result := TDynMapDef.canBeMap(fs);
    (*
    fs.readBuffer(sign, 3);
    result := (sign = MAP_SIGNATURE);
    if not result then result := (sign[0] = 'm') and (sign[1] = 'a') and (sign[2] = 'p');
    *)
  except
    fs.Free();
    result := false; // just in case
    exit;
  end;
  fs.Free();
end;


// returns `nil` if file wasn't found
function TWADFile.openFileStream (name: AnsiString): TStream;
var
  f: Integer;
  fi: TSFSFileInfo;
begin
  result := nil;
  // backwards, due to possible similar names and such
  for f := fIter.Count-1 downto 0 do
  begin
    fi := fIter.Files[f];
    if fi = nil then continue;
    if StrEquCI1251(fi.name, name) then
    begin
      try
        result := fIter.volume.OpenFileByIndex(f);
      except
        result := nil;
      end;
      if (result <> nil) then exit;
    end;
  end;
end;


function removeExt (s: AnsiString): AnsiString;
var
  i: Integer;
begin
  i := length(s)+1;
  while (i > 1) and (s[i-1] <> '.') and (s[i-1] <> '/') do Dec(i);
  if (i > 1) and (s[i-1] = '.') then
  begin
    //writeln('[', s, '] -> [', Copy(s, 1, i-2), ']');
    s := Copy(s, 1, i-2);
  end;
  result := s;
end;


function TWADFile.GetResourceEx (name: AnsiString; wantMap: Boolean; var pData: Pointer; var Len: Integer; logError: Boolean=true): Boolean;
var
  f, lastSlash: Integer;
  fi: TSFSFileInfo;
  fs: TStream;
  fpp: Pointer;
  rpath, rname: AnsiString;
  //sign: packed array [0..2] of Char;
  goodMap: Boolean;
  {$IFNDEF SFS_MAPDETECT_FX}
  wst: TSFSMemoryChunkStream;
  {$ENDIF}
begin
  Result := False;
  if not isOpen or (fIter = nil) then Exit;
  rname := removeExt(name);
  if length(rname) = 0 then Exit; // just in case
  lastSlash := -1;
  for f := 1 to length(rname) do
  begin
    if rname[f] = '\' then rname[f] := '/';
    if rname[f] = '/' then lastSlash := f;
  end;
  if lastSlash > 0 then
  begin
    rpath := Copy(rname, 1, lastSlash);
    Delete(rname, 1, lastSlash);
  end
  else
  begin
    rpath := '';
  end;
  // backwards, due to possible similar names and such
  for f := fIter.Count-1 downto 0 do
  begin
    fi := fIter.Files[f];
    if fi = nil then continue;
    if StrEquCI1251(removeExt(fi.name), rname) then
    begin
      // i found her (maybe)
      if not wantMap then
      begin
        if length(fi.path) < length(rpath) then continue; // alas
        if length(fi.path) = length(rpath) then
        begin
          if not StrEquCI1251(fi.path, rpath) then continue; // alas
        end
        else
        begin
          if fi.path[length(fi.path)-length(rpath)] <> '/' then continue; // alas
          if not StrEquCI1251(Copy(fi.path, length(fi.path)+1-length(rpath), length(fi.path)), rpath) then continue; // alas
        end;
      end;
      try
        fs := fIter.volume.OpenFileByIndex(f);
      except
        fs := nil;
      end;
      if fs = nil then
      begin
        if wantMap then continue;
        if logError then e_WriteLog(Format('DFWAD: can''t open file [%s] in [%s]', [name, fFileName]), MSG_WARNING);
        break;
      end;
      // if we want only maps, check if this is map
{$IFDEF SFS_MAPDETECT_FX}
      if wantMap then
      begin
        goodMap := false;
        {$IF DEFINED(D2D_NEW_MAP_READER_DBG)}
        e_LogWritefln('DFWAD: checking for good map in wad [%s], file [%s] (#%d)', [fFileName, fi.fname, f]);
        {$ENDIF}
        try
          //fs.readBuffer(sign, 3);
          //goodMap := (sign = MAP_SIGNATURE);
          //if not goodMap then goodMap := (sign[0] = 'm') and (sign[1] = 'a') and (sign[2] = 'p');
          goodMap := TDynMapDef.canBeMap(fs);
          {$IF DEFINED(D2D_NEW_MAP_READER_DBG)}
          if goodMap then
            e_LogWritefln('  GOOD map in wad [%s], file [%s] (#%d)', [fFileName, fi.fname, f])
          else
            e_LogWritefln('  BAD map in wad [%s], file [%s] (#%d)', [fFileName, fi.fname, f]);
          {$ENDIF}
        except
        end;
        if not goodMap then
        begin
          {$IF DEFINED(D2D_NEW_MAP_READER_DBG)}
          e_LogWritefln('  not a map in wad [%s], file [%s] (#%d)', [fFileName, fi.fname, f]);
          {$ENDIF}
          fs.Free();
          continue;
        end;
        fs.position := 0;
      end;
{$ENDIF}
      Len := Integer(fs.size);
      GetMem(pData, Len);
      fpp := pData;
      try
        fs.ReadBuffer(pData^, Len);
        fpp := nil;
      finally
        if fpp <> nil then
        begin
          FreeMem(fpp);
          pData := nil;
          Len := 0;
        end;
        fs.Free;
      end;
{$IFNDEF SFS_MAPDETECT_FX}
      if wantMap then
      begin
        goodMap := false;
        if Len >= 3 then
        begin
          //Move(pData^, sign, 3);
          //goodMap := (sign = MAP_SIGNATURE);
          wst := TSFSMemoryChunkStream.Create(pData, Len);
          try
            goodMap := TDynMapDef.canBeMap(wst);
          except
            goodMap := false;
          end;
          wst.Free();
        end;
        if not goodMap then
        begin
          //e_WriteLog(Format('  not a map in wad [%s], file [%s] (#%d)', [fFileName, fi.fname, f]), MSG_NOTIFY);
          FreeMem(pData);
          pData := nil;
          Len := 0;
          continue;
        end;
      end;
{$ENDIF}
      result := true;
      {$IFDEF SFS_DFWAD_DEBUG}
      if wadoptDebug then
        e_WriteLog(Format('DFWAD: file [%s] FOUND in [%s]; size is %d bytes', [name, fFileName, Len]), MSG_NOTIFY);
      {$ENDIF}
      exit;
    end;
  end;
  if logError then e_WriteLog(Format('DFWAD: file [%s] not found in [%s]', [name, fFileName]), MSG_WARNING);
end;

function TWADFile.GetResource (name: AnsiString; var pData: Pointer; var Len: Integer; logError: Boolean=true): Boolean;
begin
  result := GetResourceEx(name, false, pData, Len, logError);
end;

function TWADFile.GetMapResource (name: AnsiString; var pData: Pointer; var Len: Integer; logError: Boolean=true): Boolean;
begin
  result := GetResourceEx(name, true, pData, Len, logError);
end;

function TWADFile.GetMapResources (): SArray;
var
  f, c: Integer;
  fi: TSFSFileInfo;
  s: AnsiString;
begin
  Result := nil;
  if not isOpen or (fIter = nil) then Exit;
  for f := fIter.Count-1 downto 0 do
  begin
    fi := fIter.Files[f];
    if fi = nil then continue;
    if length(fi.name) = 0 then continue;
    {$IF DEFINED(D2D_NEW_MAP_READER)}
    //e_LogWritefln('DFWAD: checking for map in wad [%s], file [%s] (#%d)', [fFileName, fi.fname, f]);
    {$ENDIF}
    if isMapResource(f) then
    begin
      s := removeExt(fi.name);
      c := High(result);
      while c >= 0 do
      begin
        if StrEquCI1251(result[c], s) then break;
        Dec(c);
      end;
      if c < 0 then
      begin
        SetLength(result, Length(result)+1);
        result[high(result)] := removeExt(fi.name);
      end;
    end;
  end;
end;


function TWADFile.ReadFile (FileName: AnsiString): Boolean;
var
  rfn: AnsiString;
  //f: Integer;
  //fi: TSFSFileInfo;
begin
  Result := False;
  //e_WriteLog(Format('TWADFile.ReadFile: [%s]', [FileName]), MSG_NOTIFY);
  FreeWAD();
  rfn := findDiskWad(FileName);
  if length(rfn) = 0 then
  begin
    e_WriteLog(Format('TWADFile.ReadFile: error looking for [%s]', [FileName]), MSG_NOTIFY);
    exit;
  end;
  {$IFDEF SFS_DFWAD_DEBUG}
  if wadoptDebug then e_WriteLog(Format('TWADFile.ReadFile: FOUND [%s]', [rfn]), MSG_NOTIFY);
  {$ENDIF}
  // cache this wad
  try
    if wadoptFast then
    begin
      if not SFSAddDataFile(rfn, true) then exit;
    end
    else
    begin
      if not SFSAddDataFileTemp(rfn, true) then exit;
    end;
  except
    exit;
  end;
  fIter := SFSFileList(rfn);
  if fIter = nil then Exit;
  fFileName := rfn;
  {$IFDEF SFS_DFWAD_DEBUG}
  if wadoptDebug then e_WriteLog(Format('TWADFile.ReadFile: [%s] opened', [fFileName]), MSG_NOTIFY);
  {$ENDIF}
  Result := True;
end;


var
  uniqueCounter: Integer = 0;

function TWADFile.ReadMemory (Data: Pointer; Len: LongWord): Boolean;
var
  fn: AnsiString;
  st: TStream = nil;
  //f: Integer;
  //fi: TSFSFileInfo;
begin
  Result := False;
  FreeWAD();
  if (Data = nil) or (Len = 0) then
  begin
    e_WriteLog('TWADFile.ReadMemory: EMPTY SUBWAD!', MSG_WARNING);
    Exit;
  end;

  fn := Format(' -- memwad %d -- ', [uniqueCounter]);
  Inc(uniqueCounter);
  {$IFDEF SFS_DFWAD_DEBUG}
  if wadoptDebug then e_WriteLog(Format('TWADFile.ReadMemory: [%s]', [fn]), MSG_NOTIFY);
  {$ENDIF}

  try
    st := TSFSMemoryStreamRO.Create(Data, Len);
    if not SFSAddSubDataFile(fn, st, true) then
    begin
      st.Free;
      Exit;
    end;
  except
    st.Free;
    Exit;
  end;

  fIter := SFSFileList(fn);
  if fIter = nil then Exit;

  fFileName := fn;
  {$IFDEF SFS_DFWAD_DEBUG}
  if wadoptDebug then e_WriteLog(Format('TWADFile.ReadMemory: [%s] opened', [fFileName]), MSG_NOTIFY);
  {$ENDIF}

  {
  for f := 0 to fIter.Count-1 do
  begin
    fi := fIter.Files[f];
    if fi = nil then continue;
    st := fIter.volume.OpenFileByIndex(f);
    if st = nil then
    begin
      e_WriteLog(Format('[%s]: [%s : %s] CAN''T OPEN', [fFileName, fi.path, fi.name]), MSG_NOTIFY);
    end
    else
    begin
      e_WriteLog(Format('[%s]: [%s : %s] %u', [fFileName, fi.path, fi.name, st.size]), MSG_NOTIFY);
      st.Free;
    end;
  end;
  //fIter.volume.OpenFileByIndex(0);
  }

  Result := True;
end;


begin
  sfsDiskDirs := '<exedir>/data'; //FIXME
end.
