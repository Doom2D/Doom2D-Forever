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
unit CONFIG;

interface

{$IFDEF USE_MEMPOOL}
uses
  mempool;
{$ENDIF}

type
  TParam = record
   Param: ShortString;
   Value: ShortString;
   Section: Word;
  end;

  TConfig = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
   private
    FParams: array of TParam;
    FSections: array of ShortString;
    FCurrentSection: Word;
    function ReadParam(Section, Param, Default: string): string;
    procedure WriteParam(Section, Param, Value: string);
    procedure ProcessStr(Str: string);
   public
    constructor Create();
    constructor CreateFile(FileName: string);
    constructor CreateMem(pData: Pointer; _Length: LongWord);
    destructor Destroy(); override;
    procedure FreeConfig();
    procedure SaveFile(FileName: string);
    function ReadInt(Section, Param: string; Default: Integer): Integer;
    function ReadStr(Section, Param: string; Default: String): string;
    function ReadBool(Section, Param: string; Default: Boolean): Boolean;
    function SectionExists(Section: string): Boolean;
    procedure WriteInt(Section, Param: string; Value: Integer);
    procedure WriteStr(Section, Param, Value: string);
    procedure WriteBool(Section, Param: string; Value: Boolean);
   end;

implementation

uses
  SysUtils, utils;

{ TConfig }

constructor TConfig.Create();
begin
 inherited Create;

 FreeConfig();
end;

constructor TConfig.CreateFile(FileName: string);
var
  f: TextFile;
  a: string;
begin
 FreeConfig();

 if not FileExists(FileName) then Exit;

 AssignFile(f, FileName);
 Reset(f);
 while not EOF(f) do
 begin
  Readln(f, a);
  ProcessStr(a);
 end;
 CloseFile(f);
end;

constructor TConfig.CreateMem(pData: Pointer; _Length: LongWord);
var
  a: Integer;
  str: string;
begin
 FreeConfig();

 if _Length = 0 then Exit;
 if pData = nil then Exit;

 SetLength(str, _Length);

 CopyMemory(@str[1], pData, _Length);

 while Str <> '' do
 begin
  for a := 2 to Length(Str) do
   if (Str[a-1]+Str[a] = #13#10) or (a = Length(Str)) then
   begin
    if a <> Length(Str) then ProcessStr(Copy(Str, 1, a-2)) else ProcessStr(Str);
    Delete(Str, 1, a);
    Str := Trim(Str);
    Break;
   end;
 end;
end;

destructor TConfig.Destroy();
begin
 FParams := nil;
 FSections := nil;

 inherited;
end;

procedure TConfig.FreeConfig();
begin
 FParams := nil;
 FSections := nil;

 SetLength(FSections, 1);
 FSections[0] := '';
 FCurrentSection := 0;
end;

procedure TConfig.ProcessStr(Str: string);
var
  a, l: Integer;
begin
 Str := Trim(Str);

 if (Str <> '') and (Length(Str) > 2) and (Str[1] <> ';') then
 begin
  l := Length(Str);

  if Pos('=', Str) > 0  then
  begin
   SetLength(FParams, Length(FParams)+1);

   with FParams[High(FParams)] do
   begin
    a := Pos('=', Str);
    Param := Trim(Copy(Str, 1, a-1));
    Value := Trim(Copy(Str, a+1, l));
    Section := FCurrentSection;
   end;
  end
   else if (Str[1] = '[') and (Str[l] = ']') then
  begin
   SetLength(FSections, Length(FSections)+1);
   FCurrentSection := High(FSections);
   FSections[FCurrentSection] := Trim(Copy(Str, 2, l-2));
  end;
 end;
end;

function TConfig.ReadBool(Section, Param: string; Default: Boolean): Boolean;
var
  a: Integer;
begin
 if Default then a := 1 else a := 0;

 Result := StrToIntDef(ReadParam(Section, Param, IntToStr(a)), a) <> 0;
end;

function TConfig.ReadInt(Section, Param: string; Default: Integer): Integer;
begin
 Result := StrToIntDef(ReadParam(Section, Param, IntToStr(Default)), Default);
end;

function TConfig.ReadParam(Section, Param, Default: string): string;
var
  a: Integer;
  s: Word;
  ok: Boolean;
  p: string;
begin
 Result := default;

 if FParams = nil then Exit;
 if FSections = nil then Exit;

 ok := False;
 s := 0;
 for a := 0 to High(FSections) do
  if LowerCase(FSections[a]) = LowerCase(Section) then
  begin
   s := a;
   ok := True;
  end;

 if not ok then Exit;

 p := LowerCase(Param);

 for a := 0 to High(FParams) do
  if (FParams[a].Section = s) and (LowerCase(FParams[a].Param) = p) then
  begin
   Result := FParams[a].Value;
   Break;
  end;
end;

function TConfig.ReadStr(Section, Param, Default: string): string;
begin
 Result := ReadParam(Section, Param, Default);
end;

procedure TConfig.SaveFile(FileName: string);
var
  f: TextFile;
  a: Integer;
  b: Integer;
begin
 AssignFile(f, FileName);
 Rewrite(f);
 if (FSections <> nil) or (FParams <> nil) then
 begin
  if FSections <> nil then
   for a := 0 to High(FSections) do
   begin
    if FSections[a] <> '' then Writeln(f, '['+FSections[a]+']');

    if FParams <> nil then
     for b := 0 to High(FParams) do
      if FParams[b].Section = a then WriteLn(f, FParams[b].Param+'='+FParams[b].Value);

    if (a <> High(FSections)) and (FSections[a] <> '') then WriteLn(f, '');
   end;
 end;
 CloseFile(f);
end;

function TConfig.SectionExists(Section: string): Boolean;
var
  a: Integer;
begin
 Result := False;

 if FSections = nil then Exit;

 Section := LowerCase(Section);

 for a := 0 to High(FSections) do
  if Section = LowerCase(FSections[a]) then
  begin
   Result := True;
   Exit;
  end;
end;

procedure TConfig.WriteBool(Section, Param: string; Value: Boolean);
begin
 WriteParam(Section, Param, BoolToStr(Value));
end;

procedure TConfig.WriteInt(Section, Param: string; Value: Integer);
begin
 WriteParam(Section, Param, IntToStr(Value));
end;

procedure TConfig.WriteParam(Section, Param, Value: string);
var
  a, b: Integer;
  ok: Boolean;
begin
 a := 0;
 b := 0;

 ok := False;

 if FSections <> nil then
  for a := 0 to High(FSections) do
   if FSections[a] = Section then
   begin
    ok := True;
    Break;
   end;

 if not ok then
 begin
  SetLength(FSections, Length(FSections)+1);
  a := High(FSections);
  FSections[a] := Section;
 end;

 ok := False;
 if FParams <> nil then
  for b := 0 to High(FParams) do
   if (LowerCase(FParams[b].Param) = LowerCase(Param)) and (FParams[b].Section = a) then
   begin
    ok := True;
    Break;
   end;

 if ok then FParams[b].Value := Value
  else
 begin
  SetLength(FParams, Length(FParams)+1);
  FParams[High(FParams)].Param := Param;
  FParams[High(FParams)].Value := Value;
  FParams[High(FParams)].Section := a;
 end;
end;

procedure TConfig.WriteStr(Section, Param, Value: string);
begin
 WriteParam(Section, Param, Value);
end;

end.
