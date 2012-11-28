unit utils;

interface

function tostr(i: Integer): string;

implementation

uses windows, SysUtils;

function tostr(i: Integer): string;
begin
 Str(i, Result);
end;

end.
