unit CONFIGSIMPLE;

interface

function config_open(FileName: string): Boolean;
function config_read_int(param: string; def: Integer): Integer;
function config_read_str(param: string; def: string): string;
function config_read_bool(param: string; def: Boolean): Boolean;
procedure config_close();

implementation

uses windows;

var
  cfg_data: array of ShortString = nil;

function tostr(i: Integer): string;
begin
 Str(i, Result);
end;

function toint(s: string; var i: Integer): Boolean;
var
  code: Integer;
begin
 Val(s, i, code);

 Result := code = 0;
end;

function readparam(param: string; var s: string): Boolean;
var
  a, b, len, d_len: Integer;
begin
 Result := False;

 if cfg_data = nil then Exit;

 d_len := Length(cfg_data);

 for a := 0 to d_len do
 begin
  len := Length(cfg_data[a]);
  if len = 0 then Exit;

  for b := 1 to len do
   if cfg_data[a][b] = '=' then
    if Copy(cfg_data[a], 1, b-1) = param then
    begin
     s := Copy(cfg_data[a], b+1, len);
     Result := True;
     Exit;
    end;
 end;
end;

function config_open(FileName: string): Boolean;
var
  f: TextFile;
  str: ShortString;
  len, d_len, line: Integer;
begin
 Result := False;

 if cfg_data <> nil then config_close();

 AssignFile(f, FileName);

 {$I-}
 Reset(f);
 {$I+}

 if IOResult <> 0 then Exit;

 d_len := 32;
 SetLength(cfg_data, d_len);
 line := 0;

 while not EOF(f) do
 begin
  Readln(f, str);

  len := Length(str);
  if len < 3 then Continue;
  if str[1] = ';' then Continue;

  if line >= d_len then
  begin
   d_len := d_len+32;
   SetLength(cfg_data, d_len);
  end;

  cfg_data[line] := str;
  line := line+1;
 end;

 CloseFile(f);

 Result := True;
end;

function config_read_int(param: string; def: Integer): Integer;
var
  s: string;
begin
 Result := def;

 if not readparam(param, s) then Exit;

 if not toint(s, Result) then Result := def;
end;

function config_read_str(param: string; def: string): string;
var
  s: string;
begin
 Result := def;

 if not readparam(param, s) then Exit;

 Result := s;
end;

function config_read_bool(param: string; def: Boolean): Boolean;
var
  s: string;
begin
 Result := def;

 if not readparam(param, s) then Exit;

 Result := s <> '0';
end;

procedure config_close();
begin
 if cfg_data <> nil then cfg_data := nil;
end;


end.
