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
{$INCLUDE a_modes.inc}
unit wadreader;

interface

// g_ExtractWadName C:\svr\shit.wad:\MAPS\MAP01 -> C:/svr/shit.wad
function g_ExtractWadName (resourceStr: AnsiString): AnsiString;

// g_ExtractWadNameNoPath C:\svr\shit.wad:\MAPS\MAP01 -> shit.wad
function g_ExtractWadNameNoPath (resourceStr: AnsiString): AnsiString;

// g_ExtractFilePath C:\svr\shit.wad:\MAPS\MAP01 -> :/MAPS
function g_ExtractFilePath (resourceStr: AnsiString): AnsiString;

// g_ExtractFileName C:\svr\shit.wad:\MAPS\MAP01 -> MAP01
function g_ExtractFileName (resourceStr: AnsiString): AnsiString; // without path

// g_ExtractFilePathName C:\svr\shit.wad:\MAPS\MAP01 -> MAPS/MAP01
function g_ExtractFilePathName (resourceStr: AnsiString): AnsiString;

implementation

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

end.
