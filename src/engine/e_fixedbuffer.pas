{$MODE DELPHI}
unit e_fixedbuffer;

interface

uses md5;

const
  BUF_SIZE = 65536;

type
  TBuffer = record
    Data: array [0..BUF_SIZE] of Byte; // один байт сверху на всякий случай
    ReadPos: Cardinal;
    WritePos: Cardinal;
    Len: Cardinal;
  end;
  pTBuffer = ^TBuffer;

var
  RawPos: Cardinal = 0;

procedure e_Buffer_Clear(B: pTBuffer);


procedure e_Buffer_Write_Generic(B: pTBuffer; var V; N: Cardinal);
procedure e_Buffer_Read_Generic(B: pTBuffer; var V; N: Cardinal);


procedure e_Buffer_Write(B: pTBuffer; V: Char); overload;

procedure e_Buffer_Write(B: pTBuffer; V: Byte); overload;
procedure e_Buffer_Write(B: pTBuffer; V: Word); overload;
procedure e_Buffer_Write(B: pTBuffer; V: LongWord); overload;

procedure e_Buffer_Write(B: pTBuffer; V: ShortInt); overload;
procedure e_Buffer_Write(B: pTBuffer; V: SmallInt); overload;
procedure e_Buffer_Write(B: pTBuffer; V: LongInt); overload;

procedure e_Buffer_Write(B: pTBuffer; V: string); overload;

procedure e_Buffer_Write(B: pTBuffer; V: TMD5Digest); overload;


function  e_Buffer_Read_Char(B: pTBuffer): Char;

function  e_Buffer_Read_Byte(B: pTBuffer): Byte;
function  e_Buffer_Read_Word(B: pTBuffer): Word;
function  e_Buffer_Read_LongWord(B: pTBuffer): LongWord;

function  e_Buffer_Read_ShortInt(B: pTBuffer): ShortInt;
function  e_Buffer_Read_SmallInt(B: pTBuffer): SmallInt;
function  e_Buffer_Read_LongInt(B: pTBuffer): LongInt;
function  e_Buffer_Read_Int64(B: pTBuffer): Int64;

function  e_Buffer_Read_String(B: pTBuffer): string;

function  e_Buffer_Read_MD5(B: pTBuffer): TMD5Digest;


procedure e_Raw_Read_Generic(P: Pointer; var V; N: Cardinal);

function  e_Raw_Read_Char(P: Pointer): Char;

function  e_Raw_Read_Byte(P: Pointer): Byte;
function  e_Raw_Read_Word(P: Pointer): Word;
function  e_Raw_Read_LongWord(P: Pointer): LongWord;

function  e_Raw_Read_ShortInt(P: Pointer): ShortInt;
function  e_Raw_Read_SmallInt(P: Pointer): SmallInt;
function  e_Raw_Read_LongInt(P: Pointer): LongInt;

function  e_Raw_Read_String(P: Pointer): string;

function  e_Raw_Read_MD5(P: Pointer): TMD5Digest;

procedure e_Raw_Seek(I: Cardinal);

implementation

uses SysUtils, BinEditor;

procedure e_Buffer_Clear(B: pTBuffer);
begin
  B^.WritePos := 0;
  B^.ReadPos := 0;
  B^.Len := 0;
end;


procedure e_Buffer_Write_Generic(B: pTBuffer; var V; N: Cardinal);
begin
  if (B^.WritePos + N >= BUF_SIZE) then Exit;
  if (B^.WritePos + N > B^.Len) then
    B^.Len := B^.WritePos + N + 1;

  CopyMemory(Pointer(Cardinal(Addr(B^.Data)) + B^.WritePos),
             @V, N);

  B^.WritePos := B^.WritePos + N;
end;
procedure e_Buffer_Read_Generic(B: pTBuffer; var V; N: Cardinal);
begin
  if (B^.ReadPos + N >= BUF_SIZE) then Exit;

  CopyMemory(@V, Pointer(Cardinal(Addr(B^.Data)) + B^.ReadPos), N);

  B^.ReadPos := B^.ReadPos + N;
end;


procedure e_Buffer_Write(B: pTBuffer; V: Char); overload;
begin
  e_Buffer_Write_Generic(B, V, 1);
end;

procedure e_Buffer_Write(B: pTBuffer; V: Byte); overload;
begin
  e_Buffer_Write_Generic(B, V, 1);
end;
procedure e_Buffer_Write(B: pTBuffer; V: Word); overload;
begin
  e_Buffer_Write_Generic(B, V, 2);
end;
procedure e_Buffer_Write(B: pTBuffer; V: LongWord); overload;
begin
  e_Buffer_Write_Generic(B, V, 4);
end;

procedure e_Buffer_Write(B: pTBuffer; V: ShortInt); overload;
begin
  e_Buffer_Write_Generic(B, V, 1);
end;
procedure e_Buffer_Write(B: pTBuffer; V: SmallInt); overload;
begin
  e_Buffer_Write_Generic(B, V, 2);
end;
procedure e_Buffer_Write(B: pTBuffer; V: LongInt); overload;
begin
  e_Buffer_Write_Generic(B, V, 4);
end;

procedure e_Buffer_Write(B: pTBuffer; V: string); overload;
var
  Len: Byte;
  P: Cardinal;
begin
  Len := Length(V);
  e_Buffer_Write_Generic(B, Len, 1);

  if (Len = 0) then Exit;

  P := B^.WritePos + Len;
  if (P >= BUF_SIZE) then
  begin
    Len := BUF_SIZE - B^.WritePos;
    P := BUF_SIZE;
  end;

  if (P > B^.Len) then B^.Len := P;

  CopyMemory(Pointer(Cardinal(Addr(B^.Data)) + B^.WritePos),
             @V[1], Len);

  B^.WritePos := P;
end;

procedure e_Buffer_Write(B: pTBuffer; V: TMD5Digest); overload;
var
  I: Integer;
begin
  for I := 0 to 15 do
    e_Buffer_Write(B, V[I]);
end;


function e_Buffer_Read_Char(B: pTBuffer): Char;
begin
  e_Buffer_Read_Generic(B, Result, 1);
end;

function e_Buffer_Read_Byte(B: pTBuffer): Byte;
begin
  e_Buffer_Read_Generic(B, Result, 1);
end;
function e_Buffer_Read_Word(B: pTBuffer): Word;
begin
  e_Buffer_Read_Generic(B, Result, 2);
end;
function e_Buffer_Read_LongWord(B: pTBuffer): LongWord;
begin
  e_Buffer_Read_Generic(B, Result, 4);
end;

function e_Buffer_Read_ShortInt(B: pTBuffer): ShortInt;
begin
  e_Buffer_Read_Generic(B, Result, 1);
end;
function e_Buffer_Read_SmallInt(B: pTBuffer): SmallInt;
begin
  e_Buffer_Read_Generic(B, Result, 2);
end;
function e_Buffer_Read_LongInt(B: pTBuffer): LongInt;
begin
  e_Buffer_Read_Generic(B, Result, 4);
end;
function e_Buffer_Read_Int64(B: pTBuffer): Int64;
begin
  e_Buffer_Read_Generic(B, Result, 8);
end;

function e_Buffer_Read_String(B: pTBuffer): string;
var
  Len: Byte;
begin
  Len := e_Buffer_Read_Byte(B);
  Result := '';
  if Len = 0 then Exit;

  if B^.ReadPos + Len > B^.Len then
    Len := B^.Len - B^.ReadPos;

  SetLength(Result, Len);
  CopyMemory(@Result[1], Pointer(Cardinal(Addr(B^.Data)) + B^.ReadPos), Len);

  B^.ReadPos := B^.ReadPos + Len;
end;

function e_Buffer_Read_MD5(B: pTBuffer): TMD5Digest;
var
  I: Integer;
begin
  for I := 0 to 15 do
    Result[I] := e_Buffer_Read_Byte(B);
end;

procedure e_Raw_Read_Generic(P: Pointer; var V; N: Cardinal);
begin
  CopyMemory(@V, Pointer(Cardinal(P) + RawPos), N);

  RawPos := RawPos + N;
end;

function e_Raw_Read_Char(P: Pointer): Char;
begin
  e_Raw_Read_Generic(P, Result, 1);
end;

function e_Raw_Read_Byte(P: Pointer): Byte;
begin
  e_Raw_Read_Generic(P, Result, 1);
end;
function e_Raw_Read_Word(P: Pointer): Word;
begin
  e_Raw_Read_Generic(P, Result, 2);
end;
function e_Raw_Read_LongWord(P: Pointer): LongWord;
begin
  e_Raw_Read_Generic(P, Result, 4);
end;

function e_Raw_Read_ShortInt(P: Pointer): ShortInt;
begin
  e_Raw_Read_Generic(P, Result, 1);
end;
function e_Raw_Read_SmallInt(P: Pointer): SmallInt;
begin
  e_Raw_Read_Generic(P, Result, 2);
end;
function e_Raw_Read_LongInt(P: Pointer): LongInt;
begin
  e_Raw_Read_Generic(P, Result, 4);
end;

function e_Raw_Read_String(P: Pointer): string;
var
  Len: Byte;
begin
  Len := e_Raw_Read_Byte(P);
  Result := '';
  if Len = 0 then Exit;

  SetLength(Result, Len);
  CopyMemory(@Result[1], Pointer(Cardinal(P) + RawPos), Len);

  RawPos := RawPos + Len;
end;

function e_Raw_Read_MD5(P: Pointer): TMD5Digest;
var
  I: Integer;
begin
  for I := 0 to 15 do
    Result[I] := e_Raw_Read_Byte(P);
end;

procedure e_Raw_Seek(I: Cardinal);
begin
  RawPos := I;
end;

end.
