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
unit e_msg;

interface

uses md5;

type
  TMsg = record
    Data: Pointer;
    Overflow: Boolean;
    MaxSize: Integer;
    CurSize: Integer;
    ReadCount: Integer;
    Bit: Integer;
    AllocStep: Integer;
    OwnMemory: Boolean;

    function Init(V: Pointer; N: Integer; Full: Boolean = False): Boolean;
    procedure Alloc(N: Integer);
    procedure Clear();
    procedure Free();
    procedure CopyFrom(var From: TMsg; V: Pointer; N: Integer);
    function Allocated(): Boolean;
    function AssignBuffer(P: Pointer; N: Integer; Full: Boolean = False): Boolean;

    procedure BeginReading();
    procedure Seek(Pos: Integer);
    procedure Skip(Size: Integer);
    function BytesLeft(): Integer;
    function ReadData(V: Pointer; N: Integer): Integer;
    function ReadChar(): Char;
    function ReadByte(): Byte;
    function ReadWord(): Word;
    function ReadLongWord(): LongWord;
    function ReadShortInt(): ShortInt;
    function ReadSmallInt(): SmallInt;
    function ReadLongInt(): LongInt;
    function ReadInt64(): Int64;
    function ReadString(): String;
    function ReadMD5(): TMD5Digest;

    procedure WriteData(V: Pointer; N: Integer);
    procedure Write(V: Byte); overload;
    procedure Write(V: Word); overload;
    procedure Write(V: LongWord); overload;
    procedure Write(V: ShortInt); overload;
    procedure Write(V: SmallInt); overload;
    procedure Write(V: LongInt); overload;
    procedure Write(V: Int64); overload;
    procedure Write(V: String); overload;
    procedure Write(V: TMD5Digest); overload;
    procedure Write(V: TMsg);
  end;

type
  pTMsg = ^TMsg;

implementation

uses SysUtils, e_log;

function TMsg.Init(V: Pointer; N: Integer; Full: Boolean = False): Boolean;
begin
  Overflow := False;
  if Full then CurSize := N else CurSize := 0;
  ReadCount := 0;
  Bit := 0;
  MaxSize := N;
  Data := V;
  OwnMemory := False;
  Result := (N > 0) and (V <> nil);
end;

procedure TMsg.Alloc(N: Integer);
var
  P: Pointer;
begin
  P := GetMem(N);
  if P = nil then
    raise Exception.Create('TMsg.Alloc: no mem');
  Init(P, N);
  AllocStep := N;
  OwnMemory := True;
end;

procedure TMsg.Free();
begin
  if not OwnMemory then
    raise Exception.Create('TMsg.Free: called on borrowed memory');
  Clear();
  OwnMemory := False;
  FreeMem(Data);
  Data := nil;
  MaxSize := 0;
end;

procedure TMsg.Clear();
begin
  CurSize := 0;
  ReadCount := 0;
  Overflow := False;
  Bit := 0;
end;

function TMsg.Allocated(): Boolean;
begin
  Result := OwnMemory;
end;

procedure TMsg.CopyFrom(var From: TMsg; V: Pointer; N: Integer);
begin
  if N < From.CurSize then
    raise Exception.Create('TMsg.Copy: can''t copy into a smaller TMsg');
  Move(From, Self, SizeOf(TMsg));
  Data := V;
  Move(From.Data^, Data^, From.CurSize);
end;

function TMsg.AssignBuffer(P: Pointer; N: Integer; Full: Boolean = False): Boolean;
begin
  if OwnMemory then Self.Free();
  Clear();
  Data := P;
  MaxSize := N;
  if Full then CurSize := N;
  Result := (N > 0) and (P <> nil);
end;

procedure TMsg.WriteData(V: Pointer; N: Integer);
var
  NewSize: Integer;
begin
  if CurSize + N > MaxSize then
  begin
    if OwnMemory then
    begin
      NewSize := MaxSize + ((N + AllocStep - 1) div AllocStep) * AllocStep; // round up
      if ReAllocMem(Data, NewSize) = nil then
        raise Exception.Create('TMsg.WriteData: out of memory on realloc');
      MaxSize := NewSize;
    end
    else
    begin
      Overflow := True;
      raise Exception.Create('TMsg.WriteData: buffer overrun on borrowed memory!');
    end;
  end;

  Move(V^, (Data + CurSize)^, N);
  CurSize := CurSize + N;
end;

procedure TMsg.Write(V: TMsg);
begin
  WriteData(V.Data, V.CurSize);
end;

procedure TMsg.Write(V: Byte); overload;
begin
  WriteData(@V, 1);
end;

procedure TMsg.Write(V: Word); overload;
begin
  V := NtoLE(V);
  WriteData(@V, 2);
end;

procedure TMsg.Write(V: LongWord); overload;
begin
  V := NtoLE(V);
  WriteData(@V, 4);
end;

procedure TMsg.Write(V: ShortInt); overload;
begin
  V := NtoLE(V);
  WriteData(@V, 1);
end;

procedure TMsg.Write(V: SmallInt); overload;
begin
  V := NtoLE(V);
  WriteData(@V, 2);
end;

procedure TMsg.Write(V: LongInt); overload;
begin
  V := NtoLE(V);
  WriteData(@V, 4);
end;

procedure TMsg.Write(V: Int64); overload;
begin
  V := NtoLE(V);
  WriteData(@V, 8);
end;

procedure TMsg.Write(V: AnsiString); overload;
var
  I: Integer;
begin
  // TODO: Write(Word(Length(V)));
  Write(Byte(Length(V)));
  for I := 1 to High(V) do
    Write(Byte(V[I]));
end;

procedure TMsg.Write(V: TMD5Digest); overload;
var
  I: Integer;
begin
  for I := 0 to 15 do
    Write(V[I]);
end;

procedure TMsg.BeginReading();
begin
  ReadCount := 0;
  Bit := 0;
end;

procedure TMsg.Seek(Pos: Integer);
begin
  if Pos > CurSize then
    raise Exception.Create('TMsg.Seek: buffer overrun!');
  ReadCount := Pos;
end;

procedure TMsg.Skip(Size: Integer);
begin
  if ReadCount + Size > CurSize then
    raise Exception.Create('TMsg.Skip: buffer overrun!');
  ReadCount := ReadCount + Size;
end;

function TMsg.BytesLeft(): Integer;
begin
  Result := CurSize - ReadCount;
end;

function TMsg.ReadData(V: Pointer; N: Integer): Integer;
begin
  Result := 0;
  if ReadCount + N > CurSize then
  begin
    // TODO: maybe partial reads?
    ReadCount := CurSize + 1;
    raise Exception.Create('TMsg.ReadData: buffer overrun!');
    Exit;
  end;
  Move((Data + ReadCount)^, V^, N);
  ReadCount := ReadCount + N;
  Result := N;
end;

function TMsg.ReadChar(): Char;
begin
  Result := #0;
  ReadData(@Result, 1);
end;

function TMsg.ReadByte(): Byte;
begin
  Result := 0;
  ReadData(@Result, 1);
end;

function TMsg.ReadWord(): Word;
begin
  Result := 0;
  ReadData(@Result, 2);
  Result := LEtoN(Result);
end;

function TMsg.ReadLongWord(): LongWord;
begin
  Result := 0;
  ReadData(@Result, 4);
  Result := LEtoN(Result);
end;

function TMsg.ReadShortInt(): ShortInt;
begin
  Result := 0;
  ReadData(@Result, 1);
  Result := LEtoN(Result);
end;

function TMsg.ReadSmallInt(): SmallInt;
begin
  Result := 0;
  ReadData(@Result, 2);
  Result := LEtoN(Result);
end;

function TMsg.ReadLongInt(): LongInt;
begin
  Result := 0;
  ReadData(@Result, 4);
  Result := LEtoN(Result);
end;

function TMsg.ReadInt64(): Int64;
begin
  Result := 0;
  ReadData(@Result, 8);
  Result := LEtoN(Result);
end;

function TMsg.ReadString(): string;
var
  I: Integer;
  L: Byte;
begin
  Result := '';
  // TODO: L := ReadWord();
  L := ReadByte();
  if (L > 0) and (L <> Byte(-1)) then
  begin
    SetLength(Result, L);
    for I := 1 to L do
      Result[I] := ReadChar();
  end;
end;

function TMsg.ReadMD5(): TMD5Digest;
var
  I: Integer;
begin
  for I := 0 to 15 do
    Result[I] := ReadByte();
end;

end.
