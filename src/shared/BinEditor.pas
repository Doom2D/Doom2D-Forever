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
unit BinEditor;

interface

Uses
  SysUtils, Classes;

type
  EBinSizeError = class(Exception);

  TBinMemoryWriter = class
  private
    FSize: LongWord;
    FData: Pointer;
    FPosition: LongWord;

    procedure WriteVar (var x; varSize: LongWord);
    procedure ExtendMemory (addLen: LongWord);

  public
    constructor Create (aSize: LongWord);
    destructor Destroy (); override;

    procedure WriteByte (x: Byte);
    procedure WriteWord (x: Word);
    procedure WriteDWORD (x: LongWord);
    procedure WriteShortInt (x: ShortInt);
    procedure WriteSmallInt (x: SmallInt);
    procedure WriteInt (x: LongInt);
    procedure WriteSingle (x: Single);
    procedure WriteBoolean (x: Boolean);
    procedure WriteString (const x: AnsiString; aMaxLen: Word=65535);
    procedure WriteMemory (x: Pointer; memSize: LongWord);
    procedure Fill (aLen: LongWord; aFillSym: Byte);
    procedure SaveToFile (st: TStream);
    procedure SaveToMemory (aMem: TBinMemoryWriter);
  end;

  TBinMemoryReader = class
  private
    FSize: LongWord;
    FData: Pointer;
    FPosition: LongWord;

    procedure ReadVar (var x; varSize: LongWord);

  public
    constructor Create ();
    destructor  Destroy (); override;
    procedure ReadByte (var x: Byte);
    procedure ReadWord (var x: Word);
    procedure ReadDWORD (var x: LongWord);
    procedure ReadShortInt (var x: ShortInt);
    procedure ReadSmallInt (var x: SmallInt);
    procedure ReadInt (var x: LongInt);
    procedure ReadSingle (var x: Single);
    procedure ReadBoolean (var x: Boolean);
    procedure ReadString (var x: AnsiString);
    procedure ReadMemory (var x: Pointer; var memSize: LongWord);
    procedure Skip (aLen: LongWord);
    procedure LoadFromFile (st: TStream);
    procedure LoadFromMemory (aMem: TBinMemoryReader);
  end;

  TBinFileWriter = class
  private
    FHandle: TStream;

  public
    constructor Create ();
    destructor Destroy (); override;
    procedure OpenFile (const aFileName: AnsiString; aFileSig: LongWord;
                        aFileVer: Byte; aOverWrite: Boolean=true);
    procedure Close ();
    procedure WriteMemory (aMemory: TBinMemoryWriter);
  end;

  TBinFileReader = class
  private
    FHandle: TStream;

  public
    constructor Create ();
    destructor Destroy (); override;
    function OpenFile (const aFileName: AnsiString; aFileSig: LongWord; aFileVer: Byte): Boolean;
    procedure Close ();
    procedure ReadMemory (aMemory: TBinMemoryReader);
  end;

procedure FillMemory (Dest: Pointer; Len: LongWord; Ch: Byte); inline;
procedure CopyMemory (Dest: Pointer; Src: Pointer; Len: LongWord); inline;
procedure ZeroMemory (Dest: Pointer; Len: LongWord); inline;


implementation

uses
  Math, e_log, utils;

const
  MAX_BIN_SIZE = 42*1024*1024; // 42 MB


procedure CopyMemory (Dest: Pointer; Src: Pointer; Len: LongWord); inline;
begin
  Move(Src^, Dest^, Len);
end;

procedure FillMemory (Dest: Pointer; Len: LongWord; Ch: Byte); inline;
begin
  FillChar(Dest^, Len, Ch);
end;

procedure ZeroMemory (Dest: Pointer; Len: LongWord); inline;
begin
  FillChar(Dest^, Len, 0);
end;


{ T B i n M e m o r y W r i t e r : }

constructor TBinMemoryWriter.Create (aSize: LongWord);
begin
  if (aSize <= 0) then FSize := 1 else FSize := aSize;
  if (FSize > MAX_BIN_SIZE) then FSize := MAX_BIN_SIZE;
  GetMem(FData, FSize);
  FPosition := 0;
end;

destructor TBinMemoryWriter.Destroy ();
begin
  if (FData <> nil) then
  begin
    FreeMem(FData);
    FData := nil;
  end;
  inherited;
end;

procedure TBinMemoryWriter.WriteVar (var x; varSize: LongWord);
begin
  if (varSize > 0) then
  begin
    if (FPosition+varSize > FSize) then ExtendMemory(varSize);
    CopyMemory(Pointer(PtrUInt(FData)+FPosition), @x, varSize);
    FPosition := FPosition+varSize;
  end;
end;

procedure TBinMemoryWriter.ExtendMemory (addLen: LongWord);
var
  tmp: Pointer;
begin
  while (FPosition+addLen > FSize) and (FSize <= MAX_BIN_SIZE) do FSize := FSize*2;

  if (FSize > MAX_BIN_SIZE) then raise EBinSizeError.Create('TBinMemoryWriter.ExtendMemory: Tried to allocete more than 42 MB');

  GetMem(tmp, FSize);

  if (FPosition > 0) then CopyMemory(tmp, FData, FPosition);

  FreeMem(FData);
  FData := tmp;

  e_WriteLog('Save Memory Extended: '+IntToStr(FSize), MSG_NOTIFY);
end;

procedure TBinMemoryWriter.WriteByte (x: Byte); begin WriteVar(x, sizeof(Byte)); end;
procedure TBinMemoryWriter.WriteWord (x: Word); begin WriteVar(x, sizeof(Word)); end;
procedure TBinMemoryWriter.WriteDWORD (x: LongWord); begin WriteVar(x, sizeof(LongWord)); end;
procedure TBinMemoryWriter.WriteShortInt (x: ShortInt); begin WriteVar(x, sizeof(ShortInt)); end;
procedure TBinMemoryWriter.WriteSmallInt (x: SmallInt); begin WriteVar(x, sizeof(SmallInt)); end;
procedure TBinMemoryWriter.WriteInt (x: LongInt); begin WriteVar(x, sizeof(LongInt)); end;
procedure TBinMemoryWriter.WriteSingle (x: Single); begin WriteVar(x, sizeof(Single)); end;

procedure TBinMemoryWriter.WriteBoolean (x: Boolean);
var
  y: Byte;
begin
  if x then y := 1 else y := 0;
  WriteVar(y, sizeof(Byte));
end;

procedure TBinMemoryWriter.WriteString (const x: AnsiString; aMaxLen: Word=65535);
var
  len: Word;
begin
  if (Length(x) > aMaxLen) then len := aMaxLen else len := Word(Length(x));

  if (FPosition+sizeof(Byte)+len) > FSize then ExtendMemory(sizeof(Byte)+len);

  // Длина строки
  CopyMemory(Pointer(PtrUInt(FData)+FPosition), @len, sizeof(len));
  FPosition := FPosition+sizeof(len);
  // Строка
  if (len > 0) then
  begin
    CopyMemory(Pointer(PtrUInt(FData) + FPosition), @x[1], len);
    FPosition := FPosition+len;
  end;
end;

procedure TBinMemoryWriter.WriteMemory (x: Pointer; memSize: LongWord);
begin
  if (FPosition+sizeof(LongWord)+memSize) > FSize then ExtendMemory(sizeof(LongWord)+memSize);
  // Длина блока памяти
  CopyMemory(Pointer(PtrUInt(FData)+FPosition), @memSize, sizeof(LongWord));
  FPosition := FPosition+sizeof(LongWord);
  // Блок памяти
  if (memSize > 0) then
  begin
    CopyMemory(Pointer(PtrUInt(FData)+FPosition), x, memSize);
    FPosition := FPosition+memSize;
  end;
end;

procedure TBinMemoryWriter.Fill (aLen: LongWord; aFillSym: Byte);
begin
  if (FPosition+aLen > FSize) then ExtendMemory(aLen);
  if (aLen > 0) then
  begin
    FillMemory(Pointer(PtrUInt(FData) + FPosition), aLen, aFillSym);
    FPosition := FPosition+aLen;
  end;
end;

procedure TBinMemoryWriter.SaveToFile (st: TStream);
begin
  // Размер блока
  utils.writeInt(st, LongWord(FPosition));
  // Данные блока
  if (FPosition > 0) then st.WriteBuffer(FData^, FPosition);
end;

procedure TBinMemoryWriter.SaveToMemory (aMem: TBinMemoryWriter);
begin
  if (aMem <> nil) then aMem.WriteMemory(FData, FPosition);
end;


{ T B i n M e m o r y R e a d e r : }

constructor TBinMemoryReader.Create ();
begin
  FSize := 0;
  FData := nil;
  FPosition := 1;
end;

destructor TBinMemoryReader.Destroy ();
begin
  if (FData <> nil) then
  begin
    FreeMem(FData);
    FData := nil;
  end;
  inherited;
end;

procedure TBinMemoryReader.ReadVar (var x; varSize: LongWord);
begin
  if (varSize = 0) then exit;
  if (FPosition+varSize > FSize) then raise EBinSizeError.Create('TBinMemoryReader.ReadVar: End of Memory');
  CopyMemory(@x, Pointer(PtrUInt(FData) + FPosition), varSize);
  FPosition := FPosition+varSize;
end;

procedure TBinMemoryReader.ReadByte (var x: Byte); begin ReadVar(x, sizeof(Byte)); end;
procedure TBinMemoryReader.ReadWord (var x: Word); begin ReadVar(x, sizeof(Word)); end;
procedure TBinMemoryReader.ReadDWORD (var x: LongWord); begin ReadVar(x, sizeof(LongWord)); end;
procedure TBinMemoryReader.ReadShortInt (var x: ShortInt); begin ReadVar(x, sizeof(ShortInt)); end;
procedure TBinMemoryReader.ReadSmallInt (var x: SmallInt); begin ReadVar(x, sizeof(SmallInt)); end;
procedure TBinMemoryReader.ReadInt (var x: LongInt); begin ReadVar(x, sizeof(LongInt)); end;
procedure TBinMemoryReader.ReadSingle (var x: Single); begin ReadVar(x, sizeof(Single)); end;

procedure TBinMemoryReader.ReadBoolean (var x: Boolean);
var
  y: Byte;
begin
  ReadVar(y, sizeof(Byte));
  x := (y > 0);
end;

procedure TBinMemoryReader.ReadString (var x: AnsiString);
var
  len: Word;
begin
  if (FPosition+sizeof(len)) <= FSize then
  begin
    // Длина строки
    CopyMemory(@len, Pointer(PtrUInt(FData)+FPosition), sizeof(len));
    if (FPosition+sizeof(len)+len <= FSize) then
    begin
      FPosition := FPosition+sizeof(len);
      // Строка
      UniqueString(x);
      SetLength(x, len);
      if (len > 0) then
      begin
        CopyMemory(@x[1], Pointer(PtrUInt(FData) + FPosition), len);
        FPosition := FPosition+len;
      end
      else
      begin
        x := '';
      end;
    end
    else
    begin
      raise EBinSizeError.Create('TBinMemoryReader.ReadString: Too Long AnsiString');
    end;
  end
  else
  begin
    raise EBinSizeError.Create('TBinMemoryReader.ReadString: End of Memory');
  end;
end;

procedure TBinMemoryReader.ReadMemory (var x: Pointer; var memSize: LongWord);
begin
  if (FPosition+sizeof(LongWord) > FSize) then raise EBinSizeError.Create('TBinMemoryReader.ReadMemory: End of Memory');
  // Длина блока памяти
  CopyMemory(@memSize, Pointer(PtrUInt(FData)+FPosition), sizeof(LongWord));
  if (FPosition+sizeof(LongWord)+memSize > FSize) then raise EBinSizeError.Create('TBinMemoryReader.ReadMemory: Too Long Memory');
  FPosition := FPosition+sizeof(LongWord);
  // Блок памяти
  if (memSize > 0) then
  begin
    GetMem(x, memSize);
    CopyMemory(x, Pointer(PtrUInt(FData)+FPosition), memSize);
    FPosition += memSize;
  end
  else
  begin
    x := nil;
  end;
end;

procedure TBinMemoryReader.Skip(aLen: LongWord);
begin
  if (FPosition+aLen > FSize) then raise EBinSizeError.Create('TBinMemoryReader.Skip: End of Memory');
  FPosition += aLen;
end;

procedure TBinMemoryReader.LoadFromFile (st: TStream);
var
  aSize: LongWord;
begin
  if (FData <> nil) then begin FreeMem(FData); FData := nil; end;
  // Размер блока
  aSize := utils.readLongWord(st);
  FSize := aSize;
  GetMem(FData, FSize);
  FPosition := 0;
  // Данные блока
  if (aSize <> 0) then st.ReadBuffer(FData^, FSize);
end;


procedure TBinMemoryReader.LoadFromMemory (aMem: TBinMemoryReader);
begin
  if (FData <> nil) then begin FreeMem(FData); FData := nil; end;
  if (aMem <> nil) then
  begin
    aMem.ReadMemory(FData, FSize);
    FPosition := 0;
  end;
end;


{ T B i n F i l e W r i t e r : }

constructor TBinFileWriter.Create ();
begin
  FHandle := nil;
end;

destructor TBinFileWriter.Destroy ();
begin
  Close();
  inherited;
end;

procedure TBinFileWriter.OpenFile (const aFileName: AnsiString; aFileSig: LongWord;
                                   aFileVer: Byte; aOverWrite: Boolean=true);
begin
  Close();
  if (not FileExists(aFileName)) or aOverWrite then
  begin
    try
      FHandle := createDiskFile(aFileName);
      // Сигнатура
      utils.writeInt(FHandle, LongWord(aFileSig));
      // Версия
      utils.writeInt(FHandle, Byte(aFileVer));
    except
      FHandle.Free();
      FHandle := nil;
      raise;
    end;
  end;
end;

procedure TBinFileWriter.Close();
begin
  if (FHandle <> nil) then
  begin
    FHandle.Free();
    FHandle := nil;
  end;
end;

procedure TBinFileWriter.WriteMemory (aMemory: TBinMemoryWriter);
begin
  if (FHandle <> nil) and (aMemory <> nil) then aMemory.SaveToFile(FHandle);
end;


{ T B i n F i l e R e a d e r : }

constructor TBinFileReader.Create ();
begin
  FHandle := nil;
end;

destructor TBinFileReader.Destroy ();
begin
  Close();
  inherited;
end;

function TBinFileReader.OpenFile (const aFileName: AnsiString; aFileSig: LongWord; aFileVer: Byte): Boolean;
var
  sig: LongWord;
  ver: Byte;
begin
  result := false;

  Close();

  if FileExists(aFileName) then
  begin
    FHandle := openDiskFileRO(aFileName);
    try
      // Сигнатура
      sig := utils.readLongWord(FHandle);
      if (sig <> aFileSig) then raise EInOutError.Create('TBinFileReader.OpenFile: Wrong File Signature');
      // Версия
      ver := utils.readByte(FHandle);
      if (ver <> aFileVer) then raise EInOutError.Create('TBinFileReader.OpenFile: Wrong File Version');
      result := true;
    except
      FHandle.Free();
      FHandle := nil;
      raise;
    end;
  end;
end;

procedure TBinFileReader.Close ();
begin
  if (FHandle <> nil) then
  begin
    FHandle.Free();
    FHandle := nil;
  end;
end;

procedure TBinFileReader.ReadMemory (aMemory: TBinMemoryReader);
begin
  if (FHandle <> nil) and (aMemory <> nil) then aMemory.LoadFromFile(FHandle);
end;


end.
