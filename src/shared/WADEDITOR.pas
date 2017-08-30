unit WADEDITOR;

{
-----------------------------------
WADEDITOR.PAS ВЕРСИЯ ОТ 26.08.08

Поддержка вадов версии 1
-----------------------------------
}

interface

uses WADSTRUCT;

type
  SArray = array of ShortString;

  TWADEditor_1 = class(TObject)
   private
    FResData:   Pointer;
    FResTable:  packed array of TResourceTableRec_1;
    FHeader:    TWADHeaderRec_1;
    FDataSize:  LongWord;
    FOffset:    LongWord;
    FFileName:  string;
    FWADOpened: Byte;
    FLastError: Integer;
    FVersion:   Byte;
    function LastErrorString(): string;
    function GetResName(ResName: string): Char16;
   public
    constructor Create();
    destructor Destroy(); override;
    procedure FreeWAD();
    function  ReadFile(FileName: string): Boolean;
    function  ReadMemory(Data: Pointer; Len: LongWord): Boolean;
    procedure CreateImage();
    function AddResource(Data: Pointer; Len: LongWord; Name: string;
                         Section: string): Boolean; overload;
    function AddResource(FileName, Name, Section: string): Boolean; overload;
    function AddAlias(Res, Alias: string): Boolean;
    procedure AddSection(Name: string);
    procedure RemoveResource(Section, Resource: string);
    procedure SaveTo(FileName: string);
    function HaveResource(Section, Resource: string): Boolean;
    function HaveSection(Section: string): Boolean;
    function GetResource(Section, Resource: string; var pData: Pointer;
                         var Len: Integer): Boolean;
    function GetSectionList(): SArray;
    function GetResourcesList(Section: string): SArray;

    property GetLastError: Integer read FLastError;
    property GetLastErrorStr: string read LastErrorString;
    property GetResourcesCount: Word read FHeader.RecordsCount;
    property GetVersion: Byte read FVersion;
  end;

const
  DFWAD_NOERROR                = 0;
  DFWAD_ERROR_WADNOTFOUND      = -1;
  DFWAD_ERROR_CANTOPENWAD      = -2;
  DFWAD_ERROR_RESOURCENOTFOUND = -3;
  DFWAD_ERROR_FILENOTWAD       = -4;
  DFWAD_ERROR_WADNOTLOADED     = -5;
  DFWAD_ERROR_READRESOURCE     = -6;
  DFWAD_ERROR_READWAD          = -7;
  DFWAD_ERROR_WRONGVERSION     = -8;


 procedure g_ProcessResourceStr(ResourceStr: String; var FileName,
                                SectionName, ResourceName: String); overload;
 procedure g_ProcessResourceStr(ResourceStr: String; FileName,
                                SectionName, ResourceName: PString); overload;

implementation

uses
  SysUtils, BinEditor, ZLib, utils, e_log;

const
  DFWAD_OPENED_NONE   = 0;
  DFWAD_OPENED_FILE   = 1;
  DFWAD_OPENED_MEMORY = 2;

procedure DecompressBuf(const InBuf: Pointer; InBytes: Integer;
  OutEstimate: Integer; out OutBuf: Pointer; out OutBytes: Integer);
var
  strm: TZStreamRec;
  P: Pointer;
  BufInc: Integer;
begin
  FillChar(strm, sizeof(strm), 0);
  BufInc := (InBytes + 255) and not 255;
  if OutEstimate = 0 then
    OutBytes := BufInc
  else
    OutBytes := OutEstimate;
  GetMem(OutBuf, OutBytes);
  try
    strm.next_in := InBuf;
    strm.avail_in := InBytes;
    strm.next_out := OutBuf;
    strm.avail_out := OutBytes;
    inflateInit_(strm, zlib_version, sizeof(strm));
    try
      while inflate(strm, Z_FINISH) <> Z_STREAM_END do
      begin
        P := OutBuf;
        Inc(OutBytes, BufInc);
        ReallocMem(OutBuf, OutBytes);
        strm.next_out := PByteF(PChar(OutBuf) + (PChar(strm.next_out) - PChar(P)));
        strm.avail_out := BufInc;
      end;
    finally
      inflateEnd(strm);
    end;
    ReallocMem(OutBuf, strm.total_out);
    OutBytes := strm.total_out;
  except
    FreeMem(OutBuf);
    raise
  end;
end;

procedure CompressBuf(const InBuf: Pointer; InBytes: Integer;
                      out OutBuf: Pointer; out OutBytes: Integer);
var
  strm: TZStreamRec;
  P: Pointer;
begin
  FillChar(strm, sizeof(strm), 0);
  OutBytes := ((InBytes + (InBytes div 10) + 12) + 255) and not 255;
  GetMem(OutBuf, OutBytes);
  try
    strm.next_in := InBuf;
    strm.avail_in := InBytes;
    strm.next_out := OutBuf;
    strm.avail_out := OutBytes;
    deflateInit_(strm, Z_BEST_COMPRESSION, zlib_version, sizeof(strm));
    try
      while deflate(strm, Z_FINISH) <> Z_STREAM_END do
      begin
        P := OutBuf;
        Inc(OutBytes, 256);
        ReallocMem(OutBuf, OutBytes);
        strm.next_out := PByteF(Integer(OutBuf) + (Integer(strm.next_out) - Integer(P)));
        strm.avail_out := 256;
      end;
    finally
      deflateEnd(strm);
    end;
    ReallocMem(OutBuf, strm.total_out);
    OutBytes := strm.total_out;
  except
    FreeMem(OutBuf);
    raise
  end;
end;

procedure g_ProcessResourceStr(ResourceStr: String; var FileName,
                               SectionName, ResourceName: String);
var
  a, i: Integer;

begin
  for i := Length(ResourceStr) downto 1 do
    if ResourceStr[i] = ':' then
      Break;

  FileName := Copy(ResourceStr, 1, i-1);

  for a := i+1 to Length(ResourceStr) do
    if (ResourceStr[a] = '\') or (ResourceStr[a] = '/') then Break;

  ResourceName := Copy(ResourceStr, a+1, Length(ResourceStr)-Abs(a));
  SectionName := Copy(ResourceStr, i+1, Length(ResourceStr)-Length(ResourceName)-Length(FileName)-2);
end;

procedure g_ProcessResourceStr(ResourceStr: AnsiString; FileName,
                               SectionName, ResourceName: PAnsiString);
var
  a, i, l1, l2: Integer;

begin
  for i := Length(ResourceStr) downto 1 do
    if ResourceStr[i] = ':' then
      Break;

  if FileName <> nil then
    begin
      FileName^ := Copy(ResourceStr, 1, i-1);
      l1 := Length(FileName^);
    end
  else
    l1 := 0;

  for a := i+1 to Length(ResourceStr) do
    if (ResourceStr[a] = '\') or (ResourceStr[a] = '/') then Break;

  if ResourceName <> nil then
    begin
      ResourceName^ := Copy(ResourceStr, a+1, Length(ResourceStr)-Abs(a));
      l2 := Length(ResourceName^);
    end
  else
    l2 := 0;

  if SectionName <> nil then
    SectionName^ := Copy(ResourceStr, i+1, Length(ResourceStr)-l2-l1-2);
end;

{ TWADEditor_1 }

function TWADEditor_1.AddResource(Data: Pointer; Len: LongWord; Name: string;
                                  Section: string): Boolean;
var
  ResCompressed: Pointer;
  ResCompressedSize: Integer;
  a, b: Integer;
begin
 Result := False;

 SetLength(FResTable, Length(FResTable)+1);

 if Section = '' then
 begin
  if Length(FResTable) > 1 then
   for a := High(FResTable) downto 1 do
    FResTable[a] := FResTable[a-1];

  a := 0;
 end
  else
 begin
  Section := AnsiUpperCase(Section);
  b := -1;

  for a := 0 to High(FResTable) do
   if (FResTable[a].Length = 0) and (FResTable[a].ResourceName = Section) then
   begin
    for b := High(FResTable) downto a+2 do
     FResTable[b] := FResTable[b-1];

    b := a+1;
    Break;
   end;

  if b = -1 then
  begin
   SetLength(FResTable, Length(FResTable)-1);
   Exit;
  end;
  a := b;
 end;

 ResCompressed := nil;
 ResCompressedSize := 0;
 CompressBuf(Data, Len, ResCompressed, ResCompressedSize);
 if ResCompressed = nil then Exit;
 e_WriteLog('Fuck me (D)', MSG_NOTIFY);

 if FResData = nil then FResData := AllocMem(ResCompressedSize)
  else ReallocMem(FResData, FDataSize+Cardinal(ResCompressedSize));

 FDataSize := FDataSize+LongWord(ResCompressedSize);

 CopyMemory(Pointer(PChar(FResData)+FDataSize-PChar(ResCompressedSize)),
            ResCompressed, ResCompressedSize);
 FreeMemory(ResCompressed);

 Inc(FHeader.RecordsCount);

 with FResTable[a] do
 begin
  ResourceName := GetResName(Name);
  Address := FOffset;
  Length := ResCompressedSize;
 end;

 FOffset := FOffset+Cardinal(ResCompressedSize);

 Result := True;
end;

function TWADEditor_1.AddAlias(Res, Alias: string): Boolean;
var
  a, b: Integer;
  ares: Char16;
begin
 Result := False;

 if FResTable = nil then Exit;

 b := -1;
 ares := GetResName(Alias);
 for a := 0 to High(FResTable) do
  if FResTable[a].ResourceName = Res then
  begin
   b := a;
   Break;
  end;

 if b = -1 then Exit;

 Inc(FHeader.RecordsCount);

 SetLength(FResTable, Length(FResTable)+1);

 with FResTable[High(FResTable)] do
 begin
  ResourceName := ares;
  Address := FResTable[b].Address;
  Length := FResTable[b].Length;
 end;

 Result := True;
end;

function TWADEditor_1.AddResource(FileName, Name, Section: string): Boolean;
var
  ResCompressed: Pointer;
  ResCompressedSize: Integer;
  ResourceFile: File;
  TempResource: Pointer;
  OriginalSize: Integer;
  a, b: Integer;
begin
 Result := False;

 AssignFile(ResourceFile, findFileCIStr(FileName));

 try
  Reset(ResourceFile, 1);
 except
  FLastError := DFWAD_ERROR_CANTOPENWAD;
  Exit;
 end;

 OriginalSize := FileSize(ResourceFile);
 GetMem(TempResource, OriginalSize);

 try
  BlockRead(ResourceFile, TempResource^, OriginalSize);
 except
  FLastError := DFWAD_ERROR_READWAD;
  FreeMemory(TempResource);
  CloseFile(ResourceFile);
  Exit;
 end;

 CloseFile(ResourceFile);

 ResCompressed := nil;
 ResCompressedSize := 0;
 CompressBuf(TempResource, OriginalSize, ResCompressed, ResCompressedSize);
 FreeMemory(TempResource);
 if ResCompressed = nil then Exit;

 SetLength(FResTable, Length(FResTable)+1);

 if Section = '' then
 begin
  if Length(FResTable) > 1 then
   for a := High(FResTable) downto 1 do
    FResTable[a] := FResTable[a-1];

  a := 0;
 end
  else
 begin
  Section := AnsiUpperCase(Section);
  b := -1;

  for a := 0 to High(FResTable) do
   if (FResTable[a].Length = 0) and (FResTable[a].ResourceName = Section) then
   begin
    for b := High(FResTable) downto a+2 do
     FResTable[b] := FResTable[b-1];

    b := a+1;
    Break;
   end;

  if b = -1 then
  begin
   FreeMemory(ResCompressed);
   SetLength(FResTable, Length(FResTable)-1);
   Exit;
  end;

  a := b;
 end;

 if FResData = nil then FResData := AllocMem(ResCompressedSize)
  else ReallocMem(FResData, FDataSize+Cardinal(ResCompressedSize));

 FDataSize := FDataSize+LongWord(ResCompressedSize);
 CopyMemory(Pointer(PChar(FResData)+FDataSize-PChar(ResCompressedSize)),
            ResCompressed, ResCompressedSize);
 FreeMemory(ResCompressed);

 Inc(FHeader.RecordsCount);

 with FResTable[a] do
 begin
  ResourceName := GetResName(Name);
  Address := FOffset;
  Length := ResCompressedSize;
 end;

 FOffset := FOffset+Cardinal(ResCompressedSize);

 Result := True;
end;

procedure TWADEditor_1.AddSection(Name: string);
begin
 if Name = '' then Exit;

 Inc(FHeader.RecordsCount);

 SetLength(FResTable, Length(FResTable)+1);
 with FResTable[High(FResTable)] do
 begin
  ResourceName := GetResName(Name);
  Address := $00000000;
  Length := $00000000;
 end;
end;

constructor TWADEditor_1.Create();
begin
 FResData := nil;
 FResTable := nil;
 FDataSize := 0;
 FOffset := 0;
 FHeader.RecordsCount := 0;
 FFileName := '';
 FWADOpened := DFWAD_OPENED_NONE;
 FLastError := DFWAD_NOERROR;
 FVersion := DFWAD_VERSION;
end;

procedure TWADEditor_1.CreateImage();
var
  WADFile: File;
  b: LongWord;
begin
 if FWADOpened = DFWAD_OPENED_NONE then
 begin
  FLastError := DFWAD_ERROR_WADNOTLOADED;
  Exit;
 end;

 if FWADOpened = DFWAD_OPENED_MEMORY then Exit;

 if FResData <> nil then FreeMem(FResData);

 try
  AssignFile(WADFile, findFileCIStr(FFileName));
  Reset(WADFile, 1);

  b := 6+SizeOf(TWADHeaderRec_1)+SizeOf(TResourceTableRec_1)*Length(FResTable);

  FDataSize := LongWord(FileSize(WADFile))-b;

  GetMem(FResData, FDataSize);

  Seek(WADFile, b);
  BlockRead(WADFile, FResData^, FDataSize);

  CloseFile(WADFile);

  FOffset := FDataSize;
 except
  FLastError := DFWAD_ERROR_CANTOPENWAD;
  CloseFile(WADFile);
  Exit;
 end;

 FLastError := DFWAD_NOERROR;
end;

destructor TWADEditor_1.Destroy();
begin
 FreeWAD();

 inherited;
end;

procedure TWADEditor_1.FreeWAD();
begin
 if FResData <> nil then FreeMem(FResData);
 FResTable := nil;
 FDataSize := 0;
 FOffset := 0;
 FHeader.RecordsCount := 0;
 FFileName := '';
 FWADOpened := DFWAD_OPENED_NONE;
 FLastError := DFWAD_NOERROR;
 FVersion := DFWAD_VERSION;
end;

function TWADEditor_1.GetResName(ResName: string): Char16;
begin
 ZeroMemory(@Result[0], 16);
 if ResName = '' then Exit;

 ResName := Trim(UpperCase(ResName));
 if Length(ResName) > 16 then SetLength(ResName, 16);

 CopyMemory(@Result[0], @ResName[1], Length(ResName));
end;

function TWADEditor_1.HaveResource(Section, Resource: string): Boolean;
var
  a: Integer;
  CurrentSection: string;
begin
 Result := False;

 if FResTable = nil then Exit;

 CurrentSection := '';
 Section := AnsiUpperCase(Section);
 Resource := AnsiUpperCase(Resource);

 for a := 0 to High(FResTable) do
 begin
  if FResTable[a].Length = 0 then
  begin
   CurrentSection := FResTable[a].ResourceName;
   Continue;
  end;

  if (FResTable[a].ResourceName = Resource) and
     (CurrentSection = Section) then
  begin
   Result := True;
   Break;
  end;
 end;
end;

function TWADEditor_1.HaveSection(Section: string): Boolean;
var
  a: Integer;
begin
 Result := False;

 if FResTable = nil then Exit;
 if Section = '' then
 begin
  Result := True;
  Exit;
 end;

 Section := AnsiUpperCase(Section);

 for a := 0 to High(FResTable) do
  if (FResTable[a].Length = 0) and (FResTable[a].ResourceName = Section) then
  begin
   Result := True;
   Exit;
  end;
end;

function TWADEditor_1.GetResource(Section, Resource: string;
  var pData: Pointer; var Len: Integer): Boolean;
var
  a: LongWord;
  i: Integer;
  WADFile: File;
  CurrentSection: string;
  TempData: Pointer;
  OutBytes: Integer;
begin
 Result := False;

 CurrentSection := '';

 if FWADOpened = DFWAD_OPENED_NONE then
 begin
  FLastError := DFWAD_ERROR_WADNOTLOADED;
  Exit;
 end;

 Section := UpperCase(Section);
 Resource := UpperCase(Resource);

 i := -1;
 for a := 0 to High(FResTable) do
 begin
  if FResTable[a].Length = 0 then
  begin
   CurrentSection := FResTable[a].ResourceName;
   Continue;
  end;

  if (FResTable[a].ResourceName = Resource) and
     (CurrentSection = Section) then
  begin
   i := a;
   Break;
  end;
 end;

 if i = -1 then
 begin
  FLastError := DFWAD_ERROR_RESOURCENOTFOUND;
  Exit;
 end;

 if FWADOpened = DFWAD_OPENED_FILE then
 begin
  try
   AssignFile(WADFile, findFileCIStr(FFileName));
   Reset(WADFile, 1);

   Seek(WADFile, FResTable[i].Address+6+
        LongWord(SizeOf(TWADHeaderRec_1)+SizeOf(TResourceTableRec_1)*Length(FResTable)));
   TempData := GetMemory(FResTable[i].Length);
   BlockRead(WADFile, TempData^, FResTable[i].Length);
   DecompressBuf(TempData, FResTable[i].Length, 0, pData, OutBytes);
   FreeMem(TempData);

   Len := OutBytes;

   CloseFile(WADFile);
  except
   FLastError := DFWAD_ERROR_CANTOPENWAD;
   CloseFile(WADFile);
   Exit;
  end;
 end
  else
 begin
  TempData := GetMemory(FResTable[i].Length);
  CopyMemory(TempData, Pointer(PtrUInt(FResData)+FResTable[i].Address+6+
             PtrUInt(SizeOf(TWADHeaderRec_1)+SizeOf(TResourceTableRec_1)*Length(FResTable))),
             FResTable[i].Length);
  DecompressBuf(TempData, FResTable[i].Length, 0, pData, OutBytes);
  FreeMem(TempData);

  Len := OutBytes;
 end;

 FLastError := DFWAD_NOERROR;
 Result := True;
end;

function TWADEditor_1.GetResourcesList(Section: string): SArray;
var
  a: Integer;
  CurrentSection: Char16;
begin
 Result := nil;

 if FResTable = nil then Exit;
 if Length(Section) > 16 then Exit;

 CurrentSection := '';

 for a := 0 to High(FResTable) do
 begin
  if FResTable[a].Length = 0 then
  begin
   CurrentSection := FResTable[a].ResourceName;
   Continue;
  end;

  if CurrentSection = Section then
  begin
   SetLength(Result, Length(Result)+1);
   Result[High(Result)] := FResTable[a].ResourceName;
  end;
 end;
end;

function TWADEditor_1.GetSectionList(): SArray;
var
  i: DWORD;
begin
 Result := nil;

 if FResTable = nil then Exit;

 if FResTable[0].Length <> 0 then
 begin
  SetLength(Result, 1);
  Result[0] := '';
 end;

 for i := 0 to High(FResTable) do
  if FResTable[i].Length = 0 then
  begin
   SetLength(Result, Length(Result)+1);
   Result[High(Result)] := FResTable[i].ResourceName;
  end;
end;

function TWADEditor_1.LastErrorString(): string;
begin
 case FLastError of
  DFWAD_NOERROR: Result := '';
  DFWAD_ERROR_WADNOTFOUND: Result := 'DFWAD file not found';
  DFWAD_ERROR_CANTOPENWAD: Result := 'Can''t open DFWAD file';
  DFWAD_ERROR_RESOURCENOTFOUND: Result := 'Resource not found';
  DFWAD_ERROR_FILENOTWAD: Result := 'File is not DFWAD';
  DFWAD_ERROR_WADNOTLOADED: Result := 'DFWAD file is not loaded';
  DFWAD_ERROR_READRESOURCE: Result := 'Read resource error';
  DFWAD_ERROR_READWAD: Result := 'Read DFWAD error';
 end;
end;

function TWADEditor_1.ReadFile(FileName: string): Boolean;
var
  WADFile: File;
  Signature: array[0..4] of Char;
  a: Integer;
begin
 FreeWAD();

 Result := False;

 if not FileExists(FileName) then
 begin
  FLastError := DFWAD_ERROR_WADNOTFOUND;
  Exit;
 end;

 FFileName := FileName;

 AssignFile(WADFile, findFileCIStr(FFileName));

 try
  Reset(WADFile, 1);
 except
  FLastError := DFWAD_ERROR_CANTOPENWAD;
  Exit;
 end;

 try
  BlockRead(WADFile, Signature, 5);
  if Signature <> DFWAD_SIGNATURE then
  begin
   FLastError := DFWAD_ERROR_FILENOTWAD;
   CloseFile(WADFile);
   Exit;
  end;

  BlockRead(WADFile, FVersion, 1);
  if FVersion <> DFWAD_VERSION then
  begin
    FLastError := DFWAD_ERROR_WRONGVERSION;
    CloseFile(WADFile);
    Exit;
  end;

  BlockRead(WADFile, FHeader, SizeOf(TWADHeaderRec_1));
  SetLength(FResTable, FHeader.RecordsCount);
  if FResTable <> nil then
  begin
   BlockRead(WADFile, FResTable[0], SizeOf(TResourceTableRec_1)*FHeader.RecordsCount);

   for a := 0 to High(FResTable) do
    if FResTable[a].Length <> 0 then
     FResTable[a].Address := FResTable[a].Address-6-(LongWord(SizeOf(TWADHeaderRec_1)+
                             SizeOf(TResourceTableRec_1)*Length(FResTable)));
  end;

  CloseFile(WADFile);
 except
  FLastError := DFWAD_ERROR_READWAD;
  CloseFile(WADFile);
  Exit;
 end;

 FWADOpened := DFWAD_OPENED_FILE;
 FLastError := DFWAD_NOERROR;
 Result := True;
end;

function TWADEditor_1.ReadMemory(Data: Pointer; Len: LongWord): Boolean;
var
  Signature: array[0..4] of Char;
  a: Integer;
begin
 FreeWAD();

 Result := False;

 CopyMemory(@Signature[0], Data, 5);
 if Signature <> DFWAD_SIGNATURE then
 begin
  FLastError := DFWAD_ERROR_FILENOTWAD;
  Exit;
 end;

 CopyMemory(@FVersion, Pointer(PtrUInt(Data)+5), 1);
 if FVersion <> DFWAD_VERSION then
 begin
   FLastError := DFWAD_ERROR_WRONGVERSION;
   Exit;
 end;

 CopyMemory(@FHeader, Pointer(PtrUInt(Data)+6), SizeOf(TWADHeaderRec_1));

 SetLength(FResTable, FHeader.RecordsCount);
 if FResTable <> nil then
 begin
  CopyMemory(@FResTable[0], Pointer(PtrUInt(Data)+6+SizeOf(TWADHeaderRec_1)),
             SizeOf(TResourceTableRec_1)*FHeader.RecordsCount);

  for a := 0 to High(FResTable) do
   if FResTable[a].Length <> 0 then
    FResTable[a].Address := FResTable[a].Address-6-(LongWord(SizeOf(TWADHeaderRec_1)+
                            SizeOf(TResourceTableRec_1)*Length(FResTable)));
 end;

 GetMem(FResData, Len);
 CopyMemory(FResData, Data, Len);

 FWADOpened := DFWAD_OPENED_MEMORY;
 FLastError := DFWAD_NOERROR;

 Result := True;
end;

procedure TWADEditor_1.RemoveResource(Section, Resource: string);
var
  a, i: Integer;
  CurrentSection: Char16;
  b, c, d: LongWord;
begin
 if FResTable = nil then Exit;

 e_WriteLog('Fuck me (B) ' + Section + ' ' + Resource, MSG_NOTIFY);

 i := -1;
 b := 0;
 c := 0;
 CurrentSection := '';

 for a := 0 to High(FResTable) do
 begin
  if FResTable[a].Length = 0 then
  begin
   CurrentSection := FResTable[a].ResourceName;
   Continue;
  end;

  if (FResTable[a].ResourceName = Resource) and
     (CurrentSection = Section) then
  begin
   i := a;
   b := FResTable[a].Length;
   c := FResTable[a].Address;
   Break;
  end;
 end;

 if i = -1 then Exit;

 e_WriteLog('Fuck me (C) ' + Section + ' ' + Resource, MSG_NOTIFY);

 for a := i to High(FResTable)-1 do
  FResTable[a] := FResTable[a+1];

 SetLength(FResTable, Length(FResTable)-1);

 d := 0;
 for a := 0 to High(FResTable) do
  if (FResTable[a].Length <> 0) and (FResTable[a].Address > c) then
  begin
   FResTable[a].Address := FResTable[a].Address-b;
   d := d+FResTable[a].Length;
  end;

 CopyMemory(Pointer(PtrUInt(FResData)+c), Pointer(PtrUInt(FResData)+c+b), d);

 FDataSize := FDataSize-b;
 FOffset := FOffset-b;
 ReallocMem(FResData, FDataSize);

 FHeader.RecordsCount := FHeader.RecordsCount-1;
end;

procedure TWADEditor_1.SaveTo(FileName: string);
var
  WADFile: File;
  sign: string;
  ver: Byte;
  Header: TWADHeaderRec_1;
  i: Integer;
begin
 sign := DFWAD_SIGNATURE;
 ver := DFWAD_VERSION;

 Header.RecordsCount := Length(FResTable);

 if FResTable <> nil then
  for i := 0 to High(FResTable) do
   if FResTable[i].Length <> 0 then
    FResTable[i].Address := FResTable[i].Address+6+SizeOf(TWADHeaderRec_1)+
                            SizeOf(TResourceTableRec_1)*Header.RecordsCount;

 AssignFile(WADFile, FileName);
 Rewrite(WADFile, 1);
  BlockWrite(WADFile, sign[1], 5);
  BlockWrite(WADFile, ver, 1);
  BlockWrite(WADFile, Header, SizeOf(TWADHeaderRec_1));
  if FResTable <> nil then BlockWrite(WADFile, FResTable[0],
                                      SizeOf(TResourceTableRec_1)*Header.RecordsCount);
  if FResData <> nil then BlockWrite(WADFile, FResData^, FDataSize);
 CloseFile(WADFile);
end;

end.
