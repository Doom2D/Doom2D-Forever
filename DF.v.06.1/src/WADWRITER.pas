unit WADWRITER;

interface

uses WADSTRUCT;

type
  TWADWriter_1 = class(TObject)
   private
    FResData: Pointer;
    FResTable: packed array of TResourceTableRec_1;
    FDataSize: LongWord;
    FAddress: LongWord;
    function GetResName(ResName: string): Char16;
   public
    constructor Create;
    destructor Destroy; override;
    procedure FreeWAD;
    procedure AddResource(Data: Pointer; Len: LongWord; Name: string);
    procedure AddSection(Name: string);
    procedure SaveTo(FileName: string);
    function GetResCount: Integer;
  end;

implementation

uses
  SysUtils, Windows, ZLib;

{ TWADWriter_1 }

procedure TWADWriter_1.AddResource(Data: Pointer; Len: LongWord; Name: string);
var
  ResCompressed: Pointer;
  ResCompressedSize: Integer;
begin
 CompressBuf(Data, Len, ResCompressed, ResCompressedSize);

 if FResData = nil then FResData := AllocMem(ResCompressedSize)
  else ReallocMem(FResData, FDataSize+LongWord(ResCompressedSize));

 FDataSize := FDataSize+LongWord(ResCompressedSize);

 CopyMemory(Pointer(LongWord(FResData)+FDataSize-LongWord(ResCompressedSize)),
            ResCompressed, ResCompressedSize);

 SetLength(FResTable, Length(FResTable)+1);
 with FResTable[High(FResTable)] do
 begin
  ResourceName := GetResName(Name);
  Address := FAddress;
  Length := ResCompressedSize;
 end;

 FAddress := FAddress+LongWord(ResCompressedSize);

 FreeMemory(ResCompressed);
end;

procedure TWADWriter_1.AddSection(Name: string);
begin
 SetLength(FResTable, Length(FResTable)+1);
 with FResTable[High(FResTable)] do
 begin
  ResourceName := GetResName(Name);
  Address := $00000000;
  Length := $00000000;
 end;
end;

constructor TWADWriter_1.Create;
begin
 FResData := nil;
 FResTable := nil;
 FDataSize := 0;
 FAddress := 0;
end;

destructor TWADWriter_1.Destroy;
begin
 FreeWAD;

 inherited;
end;

procedure TWADWriter_1.FreeWAD;
begin
 if FResData <> nil then FreeMem(FResData);
 FResTable := nil;
 FDataSize := 0;
 FAddress := 0;
end;

function TWADWriter_1.GetResCount: Integer;
begin
 Result := Length(FResTable);
end;

function TWADWriter_1.GetResName(ResName: string): Char16;
begin
 ZeroMemory(@Result[0], 16);

 ResName := Trim(UpperCase(ResName));
 if Length(ResName) > 16 then SetLength(ResName, 16);

 CopyMemory(@Result[0], @ResName[1], Length(ResName));
end;

procedure TWADWriter_1.SaveTo(FileName: string);
var
  WADFile: File;
  sign: string;
  ver: Byte;
  Header: TWADHeaderRec_1;
  i: Integer;
begin
 if (FResData = nil) or (FResTable = nil) then Exit;

 sign := DFWADSIGNATURE;
 ver := DFWADVERSION;

 Header.RecordsCount := Length(FResTable);

 for i := 0 to High(FResTable) do
  if FResTable[i].Length <> 0 then
   FResTable[i].Address := FResTable[i].Address+6+SizeOf(TWADHeaderRec_1)+
                           SizeOf(TResourceTableRec_1)*Header.RecordsCount;

 AssignFile(WADFile, FileName);
 Rewrite(WADFile, 1);
  BlockWrite(WADFile, sign[1], 5);
  BlockWrite(WADFile, ver, 1);
  BlockWrite(WADFile, Header, SizeOf(TWADHeaderRec_1));
  BlockWrite(WADFile, FResTable[0], SizeOf(TResourceTableRec_1)*Header.RecordsCount);
  BlockWrite(WADFile, FResData^, FDataSize);
 CloseFile(WADFile);
end;

end.
