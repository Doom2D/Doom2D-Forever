(* Copyright (C)  Doom 2D: Forever Developers
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../shared/a_modes.inc}
// database of file hashes (md5)
unit fhashdb;

interface

uses
  SysUtils, Classes,
  md5, hashtable, utils;


type
  THashKeyMD5 = class
  public
    class function hash (const k: TMD5Digest): LongWord; inline;
    class function equ (const a, b: TMD5Digest): Boolean; inline;
    class procedure freekey (var k: TMD5Digest); inline;
  end;

  THashStrCIInt = specialize THashBase<AnsiString, Integer, THashKeyStrAnsiCI>;
  THashMD5Int = specialize THashBase<TMD5Digest, Integer, THashKeyMD5>;

  TFileHashDB = class
  private
    type TStrDynArray = array of AnsiString;

    type
      TFileInfo = record
        name: AnsiString; // name includes `mBasePath`, if necessary
        hash: TMD5Digest;
        size: LongWord;
        age: LongInt;
        nextFree: Integer;
        // used in directory scanner
        wasSeen: Boolean;
      end;

  private
    mBasePath: AnsiString; // ends with '/', or empty string
    mPathList: TStrDynArray;
    mHash2List: THashMD5Int; // hash -> list index
    mFile2List: THashStrCIInt; // file name -> list index
    mFileList: array of TFileInfo;
    mFreeHead: Integer;

  private
    procedure removeIndex (idx: Integer);
    function allocIndex (): Integer;

    procedure scanDir (path: AnsiString; var changed: Boolean);

    procedure appendOneDir (dir: AnsiString);

    procedure setup (aBasePath: AnsiString; const aPathList: TStrDynArray);

  public
    constructor Create (aBasePath: AnsiString; const aPathList: TStrDynArray);
    constructor Create (aBasePath: AnsiString; const aPathList: SSArray);
    destructor Destroy (); override;

    // doesn't automatically rescans
    procedure appendMoreDirs (const aPathList: SSArray);

    // doesn't clear base path
    procedure clear ();

    // (re)scans base path and all its subdirs
    // returns `true` if db was changed
    // you'd better call it after loading a database
    function scanFiles (): Boolean;

    // those throws
    procedure saveTo (st: TStream);
    // this clears existing data
    procedure loadFrom (st: TStream);

    // returns file name relative to base path or empty string
    function findByHash (const md5: TMD5Digest): AnsiString;
    // returns `true` if something was changed
    // name is relative to base
    function addWithHash (fdiskname: AnsiString; const md5: TMD5Digest): Boolean;
  end;


implementation


class function THashKeyMD5.hash (const k: TMD5Digest): LongWord; inline; begin
  //result := joaatHashPtr(@k, sizeof(TMD5Digest));
  //k8: use first 4 bytes of k as a hash instead? it should be good enough
  Move(k, result, sizeof(result));
end;
class function THashKeyMD5.equ (const a, b: TMD5Digest): Boolean; inline; begin result := MD5Match(a, b); end;
class procedure THashKeyMD5.freekey (var k: TMD5Digest); inline; begin end;


//==========================================================================
//
//  fixSlashes
//
//  fixes all slashes; adds a final one too
//
//==========================================================================
function fixSlashes (const path: AnsiString; addFinal: Boolean): AnsiString;
var
  f: Integer;
begin
  result := path;
  for f := 1 to length(result) do if (result[f] = '\') then result[f] := '/';
  if (addFinal) and (length(result) > 0) and (result[length(result)] <> '/') then result := result+'/';
end;


//==========================================================================
//
//  TFileHashDB.appendOneDir
//
//==========================================================================
procedure TFileHashDB.appendOneDir (dir: AnsiString);
var
  mps: AnsiString;
  found: Boolean;
begin
  if (length(dir) = 0) then exit;
  if not findFileCI(dir, true) then exit;
  dir := fixSlashes(dir, true);
  if (mBasePath <> '') and (dir[1] <> '/') then
  begin
    dir := mBasePath+dir;
    if not findFileCI(dir, true) then exit;
    dir := fixSlashes(dir, true);
  end;
  if (dir = '/') then exit;
  found := false;
  for mps in mPathList do if (dir = mps) then begin found := true; break; end;
  if not found then
  begin
    SetLength(mPathList, length(mPathList)+1);
    mPathList[High(mPathList)] := dir;
  end;
end;


//==========================================================================
//
//  TFileHashDB.setup
//
//==========================================================================
procedure TFileHashDB.setup (aBasePath: AnsiString; const aPathList: TStrDynArray);
var
  s: AnsiString;
begin
  mBasePath := aBasePath;
  if (length(aBasePath) <> 0) then
  begin
    if not findFileCI(mBasePath, true) then mBasePath := aBasePath;
  end;
  mBasePath := fixSlashes(mBasePath, true);
  SetLength(mPathList, 0);
  for s in aPathList do appendOneDir(s);
  mHash2List := THashMD5Int.Create();
  mFile2List := THashStrCIInt.Create();
  SetLength(mFileList, 0);
  mFreeHead := -1;
end;


//==========================================================================
//
//  TFileHashDB.Create
//
//==========================================================================
constructor TFileHashDB.Create (aBasePath: AnsiString; const aPathList: TStrDynArray);
begin
  setup(aBasePath, aPathList);
end;


//==========================================================================
//
//  TFileHashDB.Create
//
//==========================================================================
constructor TFileHashDB.Create (aBasePath: AnsiString; const aPathList: SSArray);
var
  f: Integer;
  pl: TStrDynArray = nil;
begin
  SetLength(pl, length(aPathList));
  for f := Low(pl) to High(pl) do pl[f] := aPathList[f-Low(pl)+Low(aPathList)];
  setup(aBasePath, pl);
end;


//==========================================================================
//
//  TFileHashDB.appendMoreDirs
//
//==========================================================================
procedure TFileHashDB.appendMoreDirs (const aPathList: SSArray);
var
  f: Integer;
begin
  for f := Low(aPathList) to High(aPathList) do appendOneDir(aPathList[f]);
end;


//==========================================================================
//
//  TFileHashDB.Destroy
//
//==========================================================================
destructor TFileHashDB.Destroy ();
begin
  mBasePath := '';
  mHash2List.Free;
  mFile2List.Free;
  SetLength(mFileList, 0);
  SetLength(mPathList, 0);
  mFreeHead := -1;
end;


//==========================================================================
//
//  TFileHashDB.clear
//
//  doesn't clear base path
//
//==========================================================================
procedure TFileHashDB.clear ();
begin
  mHash2List.clear();
  mFile2List.clear();
  SetLength(mFileList, 0);
  //SetLength(mPathList, 0);
  mFreeHead := -1;
end;


//==========================================================================
//
//  TFileHashDB.saveTo
//
//==========================================================================
procedure TFileHashDB.saveTo (st: TStream);
var
  sign: array[0..3] of AnsiChar;
  f: Integer;
begin
  sign := 'FHDB';
  st.WriteBuffer(sign, 4);
  st.WriteWord(1); // version
  st.WriteDWord(LongWord(mFile2List.count));
  for f := Low(mFileList) to High(mFileList) do
  begin
    if (length(mFileList[f].name) = 0) then continue;
    st.WriteAnsiString(mFileList[f].name);
    st.WriteBuffer(mFileList[f].hash, sizeof(TMD5Digest));
    st.WriteDWord(mFileList[f].size);
    st.WriteDWord(LongWord(mFileList[f].age));
  end;
end;


//==========================================================================
//
//  TFileHashDB.loadFrom
//
//==========================================================================
procedure TFileHashDB.loadFrom (st: TStream);
var
  sign: array[0..3] of AnsiChar;
  count: Integer;
  idx: Integer;
  fi: ^TFileInfo;
begin
  clear();
  try
    st.ReadBuffer(sign, 4);
    if (sign <> 'FHDB') then raise Exception.Create('invalid database signature');
    count := st.ReadWord();
    if (count <> 1) then raise Exception.Create('invalid database version');
    count := Integer(st.ReadDWord());
    if (count < 0) or (count > 1024*1024) then raise Exception.Create('invalid database file count');
    while (count > 0) do
    begin
      idx := allocIndex();
      fi := @mFileList[idx];
      fi.name := st.ReadAnsiString();
      st.ReadBuffer(fi.hash, sizeof(TMD5Digest));
      fi.size := st.ReadDWord();
      fi.age := Integer(st.ReadDWord());
      if (length(fi.name) = 0) then raise Exception.Create('invalid database file name');
      if (fi.age = -1) then raise Exception.Create('invalid database file age');
      mFile2List.put(fi.name, idx);
      mHash2List.put(fi.hash, idx);
      Dec(count);
    end;
  except
    begin
      clear();
      raise;
     end;
  end;
end;


//==========================================================================
//
//  TFileHashDB.removeIndex
//
//==========================================================================
procedure TFileHashDB.removeIndex (idx: Integer);
begin
  if (idx < 0) or (idx > High(mFileList)) or (length(mFileList[idx].name) = 0) then exit; // nothing to do
  mFile2List.del(mFileList[idx].name);
  mHash2List.del(mFileList[idx].hash);
  mFileList[idx].name := '';
  mFileList[idx].nextFree := mFreeHead;
  mFreeHead := idx;
end;


//==========================================================================
//
//  TFileHashDB.allocIndex
//
//==========================================================================
function TFileHashDB.allocIndex (): Integer;
begin
  result := mFreeHead;
  if (result >= 0) then
  begin
    mFreeHead := mFileList[result].nextFree;
  end
  else
  begin
    result := length(mFileList);
    SetLength(mFileList, length(mFileList)+1); // oooh...
  end;
end;


//==========================================================================
//
//  TFileHashDB.scanDir
//
//==========================================================================
procedure TFileHashDB.scanDir (path: AnsiString; var changed: Boolean);
var
  sr: TSearchRec;
  dfn: AnsiString;
  hfn: AnsiString;
  md5: TMD5Digest;
  ok: Boolean;
  idx: Integer;
  age: LongInt;
  needUpdate: Boolean;
begin
  //writeln('TFileHashDB.scanDir(000): [', path, ']');
  if (FindFirst(path+'*', faAnyFile, sr) <> 0) then
  begin
    FindClose(sr);
    exit;
  end;
  //writeln('TFileHashDB.scanDir(001): [', path, ']');
  try
    repeat
      if ((sr.Attr and faDirectory) <> 0) then
      begin
        // directory
        if (sr.Name <> '.') and (sr.Name <> '..') then scanDir(path+sr.Name+'/', changed);
      end
      else if (hasWadExtension(sr.Name)) then
      begin
        // file
        dfn := fixSlashes(path+sr.Name, false);
        // build internal file name
        hfn := dfn;
        //Delete(hfn, 1, length(mBasePath)); // remove prefix
        // find file in hash
        if not mFile2List.get(hfn, idx) then idx := -1;
        // check if we already have this file
        age := FileAge(dfn);
        if (age <> -1) then
        begin
          // do we need to update this file?
          if (idx >= 0) then
          begin
            needUpdate :=
              (age <> mFileList[idx].age) or
              (LongWord(sr.size) <> mFileList[idx].size);
          end
          else
          begin
            needUpdate := true;
          end;
          // recalc md5 and update file entry, if necessary
          if (needUpdate) then
          begin
            ok := false;
            try
              md5 := MD5File(dfn);
              ok := true;
            except
            end;
            if (ok) then
            begin
              changed := true;
              // remove old hash -> index mapping
              if (idx >= 0) then mHash2List.del(mFileList[idx].hash);
              // update
              if (idx < 0) then idx := allocIndex();
              mFileList[idx].name := hfn;
              mFileList[idx].hash := md5;
              mFileList[idx].size := LongWord(sr.size);
              mFileList[idx].age := age;
              mFileList[idx].nextFree := -1;
              mFileList[idx].wasSeen := true;
              mFile2List.put(hfn, idx);
              mHash2List.put(md5, idx);
            end
            else
            begin
              // update failed, remove this entry
              if (idx >= 0) then changed := true;
              removeIndex(idx); // cannot read, remove
            end;
          end
          else
          begin
            if (idx >= 0) then mFileList[idx].wasSeen := true;
          end;
        end
        else
        begin
          // remove this file if we don't have it anymore
          if (idx >= 0) then changed := true;
          removeIndex(idx);
        end;
      end
      else
      begin
        dfn := fixSlashes(path+sr.Name, false);
        // build internal file name
        hfn := dfn;
        Delete(hfn, 1, length(mBasePath)); // remove prefix
        // find file in hash
        if mFile2List.get(hfn, idx) then
        begin
          changed := true;
          removeIndex(idx);
        end;
      end;
    until (FindNext(sr) <> 0);
  finally
    FindClose(sr);
  end;
end;


//==========================================================================
//
//  TFileHashDB.scanFiles
//
//  scans base path and all its subdirs
//  returns `true` if db was changed
//
//==========================================================================
function TFileHashDB.scanFiles (): Boolean;
var
  f: Integer;
begin
  result := false;
  for f := Low(mFileList) to High(mFileList) do mFileList[f].wasSeen := false;
  //scanDir(mBasePath, result);
  //writeln('TFileHashDB.scanFiles: dll=', length(mPathList));
  for f := Low(mPathList) to High(mPathList) do scanDir(mPathList[f], result);
  // remove all unseen files
  f := High(mFileList);
  while (f >= 0) do
  begin
    if (length(mFileList[f].name) > 0) and (not mFileList[f].wasSeen) then removeIndex(f);
    Dec(f);
  end;
end;


//==========================================================================
//
//  TFileHashDB.findByHash
//
//  returns file name relative to base path or empty string
//
//==========================================================================
function TFileHashDB.findByHash (const md5: TMD5Digest): AnsiString;
var
  idx: Integer;
begin
  if not mHash2List.get(md5, idx) then begin result := ''; exit; end;
  result := mFileList[idx].name;
end;


//==========================================================================
//
//  TFileHashDB.addWithHash
//
//  returns `true` if something was changed
//  name is *NOT* relative to base
//
//==========================================================================
function TFileHashDB.addWithHash (fdiskname: AnsiString; const md5: TMD5Digest): Boolean;
var
  age: LongInt;
  size: LongInt;
  handle: THandle;
  fn: AnsiString;
  idx: Integer;
begin
  result := false;
  //if (length(fdiskname) > length(mBasePath)) and strEquCI1251(mBasePath, Copy(fdiskname, 1, length(mBasePath))) then Delete(fdiskname, 1, Length(mBasePath));
  if (length(fdiskname) = 0) then exit;
  //fn := mBasePath+fdiskname;
  fn := fdiskname;
  if not findFileCI(fn) then exit;
  // get age
  age := FileAge(fn);
  if (age = -1) then exit;
  // get size
  handle := FileOpen(fn, fmOpenRead or fmShareDenyNone);
  if (handle = THandle(-1)) then exit;
  size := FileSeek(handle, 0, fsFromEnd);
  FileClose(handle);
  if (size = -1) then exit;
  // find old file, if any
  //Delete(fn, 1, length(mBasePath));
  if not mFile2List.get(fn, idx) then idx := -1;
  // check for changes
  if (idx >= 0) then
  begin
    if (mFileList[idx].size = size) and (mFileList[idx].age = age) and (MD5Match(mFileList[idx].hash, md5)) then exit;
    removeIndex(idx);
  end;
  idx := allocIndex();
  mFileList[idx].name := fn;
  mFileList[idx].hash := md5;
  mFileList[idx].size := size;
  mFileList[idx].age := age;
  mFileList[idx].nextFree := -1;
  mFile2List.put(fn, idx);
  mHash2List.put(md5, idx);
  result := true;
end;


end.
