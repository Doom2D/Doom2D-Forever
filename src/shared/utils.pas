{$MODE DELPHI}
unit utils;

interface

// does filename have one of ".wad", ".pk3", ".zip" extensions?
function hasWadExtension (fn: string): Boolean;

// does filepath have ".XXX:\" in it?
function isWadPath (fn: string): Boolean;

// adds ".wad" extension if filename doesn't have one of ".wad", ".pk3", ".zip"
function addWadExtension (fn: string): string;


implementation

uses
  SysUtils, sfs;


function hasWadExtension (fn: string): Boolean;
begin
  fn := ExtractFileExt(fn);
  result := SFSStrEqu(fn, '.wad') or SFSStrEqu(fn, '.pk3') or SFSStrEqu(fn, '.zip');
end;


function addWadExtension (fn: string): string;
begin
  result := fn;
  if not hasWadExtension(result) then result := result+'.wad';
end;


function isWadPath (fn: string): Boolean;
var
  p: Integer;
  s: string;
begin
  result := false;
  while true do
  begin
    p := Pos(':', fn);
    if (p = 0) or (length(fn)-p < 1) then break;
    if (p-4 > 1) and (fn[p-4] = '.') and ((fn[p+1] = '\') or (fn[p+1] = '/')) then
    begin
      s := Copy(fn, p-4, 4);
      if SFSStrEqu(s, '.wad') or SFSStrEqu(s, '.pk3') or SFSStrEqu(s, '.zip') then
      begin
        result := true;
        exit;
      end;
    end;
    Delete(fn, 1, p);
  end;
end;


end.
