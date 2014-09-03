unit WADREADER;

{
-----------------------------------
WADREADER.PAS ÂÅÐÑÈß 1 ÎÒ 06.01.06
-----------------------------------
}

interface

uses
  WADSTRUCT;

type
  TWADReader_1 = class(TObject)
   private
    FWADOpened:   Byte;
    FWADData:     Pointer;
    FWADFileName: string;
    Header:       TWADHeaderRec_1;
    Table:        packed array of TResourceTableRec_1;
    FLastError:   Integer;
    function      LastErrorString: string;
   public
    constructor Create;
    destructor  Destroy; override;
    function    ReadFile(pFileName: string): Boolean;
    function    ReadMemory(pData: Pointer): Boolean;
    procedure   FreeWAD;
    function    GetResource(Section, Resource: string; var pData: Pointer;
                            var Len: Integer): Boolean;
    function    GetSectionList: SArray;
    function    GetResourceList(Section: string): SArray;
    property    GetLastError: Integer read FLastError;
    property    GetLastErrorStr: string read LastErrorString;
  end;

 procedure g_ProcessResourceStr(ResourceStr: string; var FileName, SectionName, ResourceName: string);

const
  DFWAD_NOERROR                = 0;
  DFWAD_ERROR_FILENOTFOUND     = -1;
  DFWAD_ERROR_CANTOPENFILE     = -2;
  DFWAD_ERROR_RESOURCENOTFOUND = -3;
  DFWAD_ERROR_FILENOTWAD       = -4;
  DFWAD_ERROR_FILENOTOPENED    = -5;

implementation

uses SysUtils, ZLib, Math, Windows;

const
  DFWAD_OPENED_NONE   = 0;
  DFWAD_OPENED_FILE   = 1;
  DFWAD_OPENED_MEMORY = 2;

procedure g_ProcessResourceStr(ResourceStr: string; var FileName, SectionName, ResourceName: string);
var
  a, i: Integer;
begin
 for i := Length(ResourceStr) downto 1 do
  if ResourceStr[i] = ':' then Break;

 FileName := Copy(ResourceStr, 1, i-1);

 for a := i+1 to Length(ResourceStr) do
  if ResourceStr[a] = '\' then Break;

 ResourceName := Copy(ResourceStr, a+1, Length(ResourceStr)-Abs(a));
 SectionName := Copy(ResourceStr, i+1, Length(ResourceStr)-Length(ResourceName)-Length(FileName)-2);
end;

{ TWADReader_1 }

constructor TWADReader_1.Create;
begin
 FWADOpened := DFWAD_OPENED_NONE;
 FWADData := nil;
 FWADFileName := '';
 Table := nil;
 FLastError := DFWAD_NOERROR;
end;

destructor TWADReader_1.Destroy;
begin
 FreeWAD;
 inherited;
end;

function TWADReader_1.GetResourceList(Section: String): SArray;
var
  i: DWORD;
  a: Integer;
begin
 Result := nil;
 if Table = nil then Exit;
 if Length(Section) > 16 then Exit;

 for i := 0 to High(Table) do
  if ((Table[i].Length = 0) and (Table[i].ResourceName = Section)) or
     ((Section = '') and (Table[i].Length <> 0)) then
  begin
   for a := i+1 to High(Table) do
    if Table[a].Length <> 0 then
    begin
     SetLength(Result, Length(Result)+1);
     Result[High(Result)] := Table[a].ResourceName;
    end else Exit;
  end;
end;

function TWADReader_1.GetSectionList: SArray;
var
  i: DWORD;
begin
 Result := nil;
 if Table = nil then Exit;

 for i := 0 to High(Table) do
  if Table[i].Length = 0 then
  begin
   SetLength(Result, Length(Result)+1);
   Result[High(Result)] := Table[i].ResourceName;
  end;
end;

function TWADReader_1.LastErrorString: string;
begin
 case FLastError of
  DFWAD_NOERROR: Result := '';
  DFWAD_ERROR_FILENOTFOUND: Result := 'WAD file not found';
  DFWAD_ERROR_CANTOPENFILE: Result := 'Can''t open WAD file';
  DFWAD_ERROR_RESOURCENOTFOUND: Result := 'Resource not found';
  DFWAD_ERROR_FILENOTWAD: Result := 'File is not WAD';
  DFWAD_ERROR_FILENOTOPENED: Result := 'WAD file is not opened';
 end;
end;

function TWADReader_1.GetResource(Section, Resource: string; var pData: Pointer;
                                var Len: Integer): Boolean;
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
  FLastError := DFWAD_ERROR_FILENOTOPENED;
  Exit;
 end;

 i := -1;
 for a := 0 to High(Table) do
 begin
  if Table[a].Length = 0 then
  begin
   CurrentSection := Table[a].ResourceName;
   Continue;
  end;

  if (Table[a].ResourceName = Resource) and
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
   AssignFile(WADFile, FWADFileName);
   Reset(WADFile, 1);

   Seek(WADFile, Table[i].Address);
   TempData := GetMemory(Table[i].Length);
   BlockRead(WADFile, TempData^, Table[i].Length);
   DecompressBuf(TempData, Table[i].Length, 0, pData, OutBytes);
   FreeMem(TempData);

   Len := OutBytes;

   CloseFile(WADFile);
  except
   FLastError := DFWAD_ERROR_CANTOPENFILE;
   CloseFile(WADFile);
   Exit;
  end;
 end
  else
 begin
  TempData := GetMemory(Table[i].Length);
  CopyMemory(TempData, Pointer(LongWord(FWADData)+Table[i].Address), Table[i].Length);
  DecompressBuf(TempData, Table[i].Length, 0, pData, OutBytes);
  FreeMem(TempData);

  Len := OutBytes;
 end;

 FLastError := DFWAD_NOERROR;
 Result := True;
end;

function TWADReader_1.ReadFile(pFileName: string): Boolean;
var
  WADFile: File;
  Signature: array[0..4] of Char;
  Version: Word;
begin
 Result := False;

 if not FileExists(pFileName) then
 begin
  FLastError := DFWAD_ERROR_FILENOTFOUND;
  Exit;
 end;
 
 FWADFileName := pFileName;

 try
  AssignFile(WADFile, FWADFileName);
  Reset(WADFile, 1);

  BlockRead(WADFile, Signature, 5);
  if Signature <> DFWADSIGNATURE then
  begin
   FLastError := DFWAD_ERROR_FILENOTWAD;
   CloseFile(WADFile);
   Exit;
  end;

  BlockRead(WADFile, Version, 1);
  BlockRead(WADFile, Header, SizeOf(Header));
  SetLength(Table, Header.RecordsCount);
  BlockRead(WADFile, Table[0], SizeOf(TResourceTableRec_1)*Header.RecordsCount);

  CloseFile(WADFile);
 except
  FLastError := DFWAD_ERROR_CANTOPENFILE;
  CloseFile(WADFile);
  Exit;
 end;

 FWADOpened := DFWAD_OPENED_FILE;
 FLastError := DFWAD_NOERROR;
 Result := True;
end;

function TWADReader_1.ReadMemory(pData: Pointer): Boolean;
var
  Signature: array[0..4] of Char;
  Version: Byte;
begin
 Result := False;

 CopyMemory(@Signature[0], pData, 5);
 if Signature <> DFWADSIGNATURE then
 begin
  FLastError := DFWAD_ERROR_FILENOTWAD;
  Exit;
 end;

 CopyMemory(@Version, Pointer(LongWord(pData)+5), 1);

 CopyMemory(@Header, Pointer(LongWord(pData)+6), SizeOf(TWADHeaderRec_1));

 SetLength(Table, Header.RecordsCount);
 CopyMemory(@Table[0], Pointer(LongWord(pData)+6+SizeOf(TWADHeaderRec_1)),
            SizeOf(TResourceTableRec_1)*Header.RecordsCount);

 FWADData := pData;
 FWADOpened := DFWAD_OPENED_MEMORY;
 FLastError := DFWAD_NOERROR;
 Result := True;
end;

procedure TWADReader_1.FreeWAD;
begin
 FWADOpened := DFWAD_OPENED_NONE;
 FWADData := nil;
 FWADFileName := '';
 Table := nil;
 FLastError := DFWAD_NOERROR;
end;

end.
