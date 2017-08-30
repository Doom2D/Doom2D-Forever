Unit BinEditor;

Interface

Uses
  SysUtils;

Type
  EBinSizeError = Class (Exception);

  TBinMemoryWriter = Class (TObject)
  Private
    FSize: Cardinal;
    FData: Pointer;
    FPosition: Cardinal;

    Procedure   WriteVar(Var x; varSize: Cardinal);
    Procedure   ExtendMemory(addLen: Cardinal);

  Public
    Constructor Create(aSize: Cardinal);
    Destructor  Destroy(); Override;
    Procedure   WriteByte(Var x: Byte);
    Procedure   WriteWord(Var x: Word);
    Procedure   WriteDWORD(Var x: DWORD);
    Procedure   WriteShortInt(Var x: ShortInt);
    Procedure   WriteSmallInt(Var x: SmallInt);
    Procedure   WriteInt(Var x: Integer);
    Procedure   WriteSingle(Var x: Single);
    Procedure   WriteBoolean(Var x: Boolean);
    Procedure   WriteString(Var x: String; aMaxLen: Byte = 255);
    Procedure   WriteMemory(Var x: Pointer; memSize: Cardinal);
    Procedure   Fill(aLen: Cardinal; aFillSym: Byte);
    Procedure   SaveToFile(Var aFile: File);
    Procedure   SaveToMemory(Var aMem: TBinMemoryWriter);
  End;

  TBinMemoryReader = Class (TObject)
  Private
    FSize: Cardinal;
    FData: Pointer;
    FPosition: Cardinal;

    Procedure   ReadVar(Var x; varSize: Cardinal);

  Public
    Constructor Create();
    Destructor  Destroy(); Override;
    Procedure   ReadByte(Var x: Byte);
    Procedure   ReadWord(Var x: Word);
    Procedure   ReadDWORD(Var x: DWORD);
    Procedure   ReadShortInt(Var x: ShortInt);
    Procedure   ReadSmallInt(Var x: SmallInt);
    Procedure   ReadInt(Var x: Integer);
    Procedure   ReadSingle(Var x: Single);
    Procedure   ReadBoolean(Var x: Boolean);
    Procedure   ReadString(Var x: String);
    Procedure   ReadMemory(Var x: Pointer; Var memSize: Cardinal);
    Procedure   Skip(aLen: Cardinal);
    Procedure   LoadFromFile(Var aFile: File);
    Procedure   LoadFromMemory(Var aMem: TBinMemoryReader);
  End;

  TBinFileWriter = Class (TObject)
  Private
    FHandle: File;
    FOpened: Boolean;

  Public
    Constructor Create();
    Destructor  Destroy(); Override;
    Procedure   OpenFile(Const aFileName: String;
                         aFileSig: Cardinal;
                         aFileVer: Byte;
                         aOverWrite: Boolean = True);
    Procedure   Close();
    Procedure   WriteMemory(Var aMemory: TBinMemoryWriter);
  End;

  TBinFileReader = Class (TObject)
  Private
    FHandle: File;
    FOpened: Boolean;

  Public
    Constructor Create();
    Destructor  Destroy(); Override;
    Function    OpenFile(Const aFileName: String;
                         aFileSig: Cardinal;
                         aFileVer: Byte): Boolean;
    Procedure   Close();
    Procedure   ReadMemory(Var aMemory: TBinMemoryReader);
  End;

procedure FillMemory(Dest: Pointer; Len: Cardinal; Ch: Byte);
procedure CopyMemory(Dest: Pointer; Src: Pointer; Len: Cardinal);
procedure ZeroMemory(Dest: Pointer; Len: Cardinal);

Implementation

Uses
  Math, e_log, utils;

Const
  MAX_BIN_SIZE = 4 * 1024 * 1024; // 4 MB

procedure CopyMemory(Dest: Pointer; Src: Pointer; Len: Cardinal);
begin
  Move(Src^, Dest^, Len);
end;

procedure FillMemory(Dest: Pointer; Len: Cardinal; Ch: Byte);
begin
  FillChar(Dest^, Len, Ch);
end;

procedure ZeroMemory(Dest: Pointer; Len: Cardinal);
begin
  FillChar(Dest^, Len, 0);
end;
  
{ T B i n M e m o r y W r i t e r : }

Constructor TBinMemoryWriter.Create(aSize: Cardinal);
begin
  if aSize <= 0 then
    FSize := 1
  else
    FSize := aSize;
  if FSize > MAX_BIN_SIZE then
    FSize := MAX_BIN_SIZE;

  GetMem(FData, FSize);
  FPosition := 0;
end;

Destructor TBinMemoryWriter.Destroy();
begin
  if FData <> nil then
  begin
    FreeMem(FData);
    FData := nil;
  end;
  
  Inherited;
end;

Procedure TBinMemoryWriter.WriteVar(Var x; varSize: Cardinal);
begin
  if (FPosition + varSize) > FSize then
    ExtendMemory(varSize);

  CopyMemory(Pointer(PtrUInt(FData) + FPosition),
             @x, varSize);
  FPosition := FPosition + varSize;
end;

Procedure TBinMemoryWriter.ExtendMemory(addLen: Cardinal);
var
  tmp: Pointer;

begin
  while ((FPosition + addLen) > FSize) and
        (FSize <= MAX_BIN_SIZE) do
    FSize := FSize * 2;

  if FSize > MAX_BIN_SIZE then
  begin
    raise EBinSizeError.Create('TBinMemoryWriter.ExtendMemory: Tried to allocete more than 4 MB');
    Exit;
  end;

  GetMem(tmp, FSize);

  if FPosition > 0 then
    CopyMemory(tmp, FData, FPosition);

  FreeMem(FData);
  FData := tmp;

  e_WriteLog('Save Memory Extended: '+IntToStr(FSize), MSG_NOTIFY);
end;

Procedure TBinMemoryWriter.WriteByte(Var x: Byte);
begin
  WriteVar(x, SizeOf(Byte));
end;

Procedure TBinMemoryWriter.WriteWord(Var x: Word);
begin
  WriteVar(x, SizeOf(Word));
end;

Procedure TBinMemoryWriter.WriteDWORD(Var x: DWORD);
begin
  WriteVar(x, SizeOf(DWORD));
end;

Procedure TBinMemoryWriter.WriteShortInt(Var x: ShortInt);
begin
  WriteVar(x, SizeOf(ShortInt));
end;

Procedure TBinMemoryWriter.WriteSmallInt(Var x: SmallInt);
begin
  WriteVar(x, SizeOf(SmallInt));
end;

Procedure TBinMemoryWriter.WriteInt(Var x: Integer);
begin
  WriteVar(x, SizeOf(Integer));
end;

Procedure TBinMemoryWriter.WriteSingle(Var x: Single);
begin
  WriteVar(x, SizeOf(Single));
end;

Procedure TBinMemoryWriter.WriteBoolean(Var x: Boolean);
var
  y: Byte;

begin
  if x then
    y := 1
  else
    y := 0;

  WriteVar(y, SizeOf(Byte));
end;

Procedure TBinMemoryWriter.WriteString(Var x: String; aMaxLen: Byte = 255);
var
  len: Byte;

begin
  len := Min(Length(x), aMaxLen);

  if (FPosition + SizeOf(Byte) + len) > FSize then
    ExtendMemory(SizeOf(Byte) + len);

// Длина строки:
  CopyMemory(Pointer(PtrUInt(FData) + FPosition),
             @len, SizeOf(Byte));
  FPosition := FPosition + SizeOf(Byte);
// Строка:
  if len > 0 then
  begin
    CopyMemory(Pointer(PtrUInt(FData) + FPosition),
               @x[1], len);
    FPosition := FPosition + len;
  end;
end;

Procedure TBinMemoryWriter.WriteMemory(Var x: Pointer; memSize: Cardinal);
begin
  if (FPosition + SizeOf(Cardinal) + memSize) > FSize then
    ExtendMemory(SizeOf(Cardinal) + memSize);

// Длина блока памяти:
  CopyMemory(Pointer(PtrUInt(FData) + FPosition),
             @memSize, SizeOf(Cardinal));
  FPosition := FPosition + SizeOf(Cardinal);
// Блок памяти:
  if memSize > 0 then
  begin
    CopyMemory(Pointer(PtrUInt(FData) + FPosition),
               x, memSize);
    FPosition := FPosition + memSize;
  end;
end;

Procedure TBinMemoryWriter.Fill(aLen: Cardinal; aFillSym: Byte);
begin
  if (FPosition + aLen) > FSize then
    ExtendMemory(aLen);

  if aLen > 0 then
  begin
    FillMemory(Pointer(PtrUInt(FData) + FPosition),
               aLen, aFillSym);
    FPosition := FPosition + aLen;
  end;
end;

Procedure TBinMemoryWriter.SaveToFile(Var aFile: File);
var
  nw: Cardinal;

begin
// Размер блока:
  BlockWrite(aFile, FPosition, SizeOf(Cardinal), nw);
  if nw <> SizeOf(Cardinal) then
    begin
      raise EInOutError.Create('TBinMemoryWriter.SaveToFile: Writing Length');
    end
  else
    begin
    // Данные блока:
      BlockWrite(aFile, FData^, FPosition, nw);
      if nw <> FPosition then
      begin
        raise EInOutError.Create('TBinMemoryWriter.SaveToFile: Writing Data');
      end
    end;
end;

Procedure TBinMemoryWriter.SaveToMemory(Var aMem: TBinMemoryWriter);
begin
  if aMem <> nil then
  begin
    aMem.WriteMemory(FData, FPosition);
  end;
end;

{ T B i n M e m o r y R e a d e r : }

Constructor TBinMemoryReader.Create();
begin
  FSize := 0;
  FData := nil;
  FPosition := 1;
end;

Destructor TBinMemoryReader.Destroy();
begin
  if FData <> nil then
  begin
    FreeMem(FData);
    FData := nil;
  end;

  Inherited;
end;

Procedure TBinMemoryReader.ReadVar(Var x; varSize: Cardinal);
begin
  if (FPosition + varSize) <= FSize then
    begin
      CopyMemory(@x,
                 Pointer(PtrUInt(FData) + FPosition),
                 varSize);
      FPosition := FPosition + varSize;
    end
  else
    raise EBinSizeError.Create('TBinMemoryReader.ReadVar: End of Memory');
end;

Procedure TBinMemoryReader.ReadByte(Var x: Byte);
begin
  ReadVar(x, SizeOf(Byte));
end;

Procedure TBinMemoryReader.ReadWord(Var x: Word);
begin
  ReadVar(x, SizeOf(Word));
end;

Procedure TBinMemoryReader.ReadDWORD(Var x: DWORD);
begin
  ReadVar(x, SizeOf(DWORD));
end;

Procedure TBinMemoryReader.ReadShortInt(Var x: ShortInt);
begin
  ReadVar(x, SizeOf(ShortInt));
end;

Procedure TBinMemoryReader.ReadSmallInt(Var x: SmallInt);
begin
  ReadVar(x, SizeOf(SmallInt));
end;

Procedure TBinMemoryReader.ReadInt(Var x: Integer);
begin
  ReadVar(x, SizeOf(Integer));
end;

Procedure TBinMemoryReader.ReadSingle(Var x: Single);
begin
  ReadVar(x, SizeOf(Single));
end;

Procedure TBinMemoryReader.ReadBoolean(Var x: Boolean);
var
  y: Byte;

begin
  ReadVar(y, SizeOf(Byte));

  if y > 0 then
    x := True
  else
    x := False;
end;

Procedure TBinMemoryReader.ReadString(Var x: String);
var
  len: Byte;

begin
  if (FPosition + SizeOf(Byte)) <= FSize then
    begin
    // Длина строки:
      CopyMemory(@len,
                 Pointer(PtrUInt(FData) + FPosition),
                 SizeOf(Byte));
         
      if (FPosition + SizeOf(Byte) + len) <= FSize then
        begin
          FPosition := FPosition + SizeOf(Byte);
        // Строка:
          SetLength(x, len);
          if len > 0 then
            begin
              CopyMemory(@x[1],
                         Pointer(PtrUInt(FData) + FPosition),
                         len);
              FPosition := FPosition + len;
            end
          else
            x := '';
        end
      else
        raise EBinSizeError.Create('TBinMemoryReader.ReadString: Too Long String');
    end
  else
    raise EBinSizeError.Create('TBinMemoryReader.ReadString: End of Memory');
end;

Procedure TBinMemoryReader.ReadMemory(Var x: Pointer; Var memSize: Cardinal);
begin
  if (FPosition + SizeOf(Cardinal)) <= FSize then
    begin
    // Длина блока памяти:
      CopyMemory(@memSize,
                 Pointer(PtrUInt(FData) + FPosition),
                 SizeOf(Cardinal));

      if (FPosition + SizeOf(Cardinal) + memSize) <= FSize then
        begin
          FPosition := FPosition + SizeOf(Cardinal);
        // Блок памяти:
          if memSize > 0 then
            begin
              GetMem(x, memSize);
              CopyMemory(x,
                         Pointer(PtrUInt(FData) + FPosition),
                         memSize);
              FPosition := FPosition + memSize;
            end
          else
            x := nil;
        end
      else
        raise EBinSizeError.Create('TBinMemoryReader.ReadMemory: Too Long Memory');
    end
  else
    raise EBinSizeError.Create('TBinMemoryReader.ReadMemory: End of Memory');
end;

Procedure TBinMemoryReader.Skip(aLen: Cardinal);
begin
  if (FPosition + aLen) <= FSize then
    begin
      FPosition := FPosition + aLen;
    end
  else
    raise EBinSizeError.Create('TBinMemoryReader.Skip: End of Memory');
end;

Procedure TBinMemoryReader.LoadFromFile(Var aFile: File);
var
  nr: Cardinal;
  aSize: Cardinal;

begin
  if FData <> nil then
    FreeMem(FData);

// Размер блока:
  BlockRead(aFile, aSize, SizeOf(Cardinal), nr);
  if nr <> SizeOf(Cardinal) then
    begin
      raise EInOutError.Create('TBinMemoryReader.LoadFromFile: Reading Length');
    end
  else
    begin
      FSize := aSize;
      GetMem(FData, FSize);
      FPosition := 0;
    // Данные блока:
      BlockRead(aFile, FData^, FSize, nr);
      if nr <> FSize then
      begin
        raise EInOutError.Create('TBinMemoryReader.LoadFromFile: Reading Data');
      end
    end;
end;

Procedure TBinMemoryReader.LoadFromMemory(Var aMem: TBinMemoryReader);
begin
  if FData <> nil then
    FreeMem(FData);

  if aMem <> nil then
  begin
    aMem.ReadMemory(FData, FSize);
    FPosition := 0;
  end;
end;

{ T B i n F i l e W r i t e r : }

Constructor TBinFileWriter.Create();
begin
  FOpened := False;
end;

Destructor TBinFileWriter.Destroy();
begin
  Close();

  Inherited;
end;

Procedure TBinFileWriter.OpenFile(Const aFileName: String;
                                  aFileSig: Cardinal;
                                  aFileVer: Byte;
                                  aOverWrite: Boolean = True);
var
  nw: Integer;

begin
  Close();

  if (not FileExists(aFileName)) or (aOverWrite) then
  begin
    AssignFile(FHandle, findFileCIStr(aFileName));
    ReWrite(FHandle, 1);

  // Сигнатура:
    BlockWrite(FHandle, aFileSig, SizeOf(Cardinal), nw);
    if nw <> SizeOf(Cardinal) then
      begin
        raise EInOutError.Create('TBinFileWriter.OpenFile: Writing File Signature');
      end
    else
      begin
      // Версия:
        BlockWrite(FHandle, aFileVer, SizeOf(Byte), nw);
        if nw <> SizeOf(Byte) then
          begin
            raise EInOutError.Create('TBinFileWriter.OpenFile: Writing File Version');
          end
        else
          begin
            FOpened := True;
          end;
      end;
  end;
end;

Procedure TBinFileWriter.Close();
begin
  if FOpened then
  begin
    System.Close(FHandle);
    FOpened := False;
  end;
end;

Procedure TBinFileWriter.WriteMemory(Var aMemory: TBinMemoryWriter);
begin
  if (FOpened) and (aMemory <> nil) then
  begin
    aMemory.SaveToFile(FHandle);
  end;
end;

{ T B i n F i l e R e a d e r : }

Constructor TBinFileReader.Create();
begin
  FOpened := False;
end;

Destructor TBinFileReader.Destroy();
begin
  Close();

  Inherited;
end;

Function TBinFileReader.OpenFile(Const aFileName: String;
                                 aFileSig: Cardinal;
                                 aFileVer: Byte): Boolean;
var
  nr: Integer;
  sig: Cardinal;
  ver: Byte;

begin
  Result := False;

  Close();

  if FileExists(aFileName) then
  begin
    AssignFile(FHandle, findFileCIStr(aFileName));
    ReSet(FHandle, 1);

  // Сигнатура:
    BlockRead(FHandle, sig, SizeOf(Cardinal), nr);
    if (nr <> SizeOf(Cardinal)) then
      begin
        raise EInOutError.Create('TBinFileReader.OpenFile: Reading File Signature');
      end
    else
      if (sig <> aFileSig) then
        begin
          raise EInOutError.Create('TBinFileReader.OpenFile: Wrong File Signature');
        end
      else
        begin
        // Версия:
          BlockRead(FHandle, ver, SizeOf(Byte), nr);
          if (nr <> SizeOf(Byte)) then
            begin
              raise EInOutError.Create('TBinFileReader.OpenFile: Reading File Version');
            end
          else
            if (ver <> aFileVer) then
              begin
                raise EInOutError.Create('TBinFileReader.OpenFile: Wrong File Version');
              end
            else
              begin
                FOpened := True;
                Result := True;
              end;
        end;
  end;
end;

Procedure TBinFileReader.Close();
begin
  if FOpened then
  begin
    System.Close(FHandle);
    FOpened := False;
  end;
end;

Procedure TBinFileReader.ReadMemory(Var aMemory: TBinMemoryReader);
begin
  if (FOpened) and (aMemory <> nil) then
  begin
    aMemory.LoadFromFile(FHandle);
  end;
end;


End.
