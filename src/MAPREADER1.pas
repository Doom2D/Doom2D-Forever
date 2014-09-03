unit MAPREADER1;

{
-----------------------------------
MAPREADER1.PAS ВЕРСИЯ 1 ОТ 06.01.06
-----------------------------------
}

interface

uses
  MAPSTRUCT;

type
  // Не пишется и не читается из карты. Это то же самое что и TBlock, только с данными.
  TDataBlock = packed record
   Block: TBlock;
   Data:  Pointer;
  end;

  TDataBlocksArray = packed array of TDataBlock;
  TMapList = packed array of Char32;

  TMapReader_1 = class(TObject)
   private
    FDFLData: Pointer;
    FDataBlocks: TDataBlocksArray;
    FMapHeader: TMapHeaderRec_1;
    function GetBlocks(BlocksType: Byte): TDataBlocksArray;
   public
    constructor Create;
    destructor Destroy; override;
    function LoadDFL(DFLName: string): Boolean; // Сначала грузим DFL
    function LoadMap(MapName: Char32): Boolean; // Затем из DFL читаем и превращаем в
    procedure FreeMap();                        // массив БЛОКОВ С ДАННЫМИ нужную карту
    procedure FreeDFL();
    function GetMapList: TMapList;
    function GetMapHeader: TMapHeaderRec_1; // Читаем заголовок карты
    function GetTextures: TTexturesRec1Array;
    //function GetAnimTextures: TAnimTexturesRec1Array;
    function GetPanels: TPanelsRec1Array;   // Из массива блоков получаем панели
    function GetItems: TItemsRec1Array;     // Итемы
    function GetAreas: TAreasRec1Array;
    function GetTriggers: TTriggersRec1Array;
  end;

implementation

uses SysUtils, Windows;

{ TMapReader_1 }

constructor TMapReader_1.Create;
begin
 FDFLData := nil;
end;

destructor TMapReader_1.Destroy;
begin
 FreeMap;
 FreeDFL;

 inherited;
end;

procedure TMapReader_1.FreeDFL;
begin
 if FDFLData <> nil then
 begin
  FreeMemory(FDFLData);
  FDFLData := nil;
 end;
end;

procedure TMapReader_1.FreeMap;
begin
 FDataBlocks := nil;
end;

{function TMapReader_1.GetAnimTextures: TAnimTexturesRec1Array;
var
  TempDataBlocks: TDataBlocksArray;
  a: Integer;
  b, Size: LongWord;
begin
 Result := nil;

 TempDataBlocks := GetBlocks(BLOCK_ANIMTEXTURES);

 if TempDataBlocks = nil then Exit;

 size := SizeOf(TAnimTextureRec_1);

 for a := 0 to High(TempDataBlocks) do
  for b := 0 to (TempDataBlocks[a].Block.BlockSize div size)-1 do
  begin
   SetLength(Result, Length(Result)+1);
   CopyMemory(@Result[High(Result)], Pointer(LongWord(TempDataBlocks[a].Data)+b*size), size);
  end;

 TempDataBlocks := nil;
end;}

function TMapReader_1.GetAreas: TAreasRec1Array;
var
  TempDataBlocks: TDataBlocksArray;
  a: Integer;
  b, Size: LongWord;
begin
 Result := nil;

 TempDataBlocks := GetBlocks(BLOCK_AREAS);

 if TempDataBlocks = nil then Exit;

 size := SizeOf(TAreaRec_1);

 for a := 0 to High(TempDataBlocks) do
  for b := 0 to (TempDataBlocks[a].Block.BlockSize div size)-1 do
  begin
   SetLength(Result, Length(Result)+1);
   CopyMemory(@Result[High(Result)], Pointer(LongWord(TempDataBlocks[a].Data)+b*size), size);
  end;

 TempDataBlocks := nil;
end;

function TMapReader_1.GetBlocks(BlocksType: Byte): TDataBlocksArray;
var
  a: Integer;
begin
 Result := nil;

 if FDataBlocks = nil then Exit;

 for a := 0 to High(FDataBlocks) do
  if FDataBlocks[a].Block.BlockType = BlocksType then
  begin
   SetlEngth(Result, Length(Result)+1);
   Result[High(Result)] := FDataBlocks[a];
  end;
end;

function TMapReader_1.GetItems: TItemsRec1Array;
var
  TempDataBlocks: TDataBlocksArray;
  a: Integer;
  b, Size: LongWord;
begin
 Result := nil;

 TempDataBlocks := GetBlocks(BLOCK_ITEMS);

 if TempDataBlocks = nil then Exit;

 size := SizeOf(TItemRec_1);

 for a := 0 to High(TempDataBlocks) do
  for b := 0 to (TempDataBlocks[a].Block.BlockSize div size)-1 do
  begin
   SetLength(Result, Length(Result)+1);
   CopyMemory(@Result[High(Result)], Pointer(LongWord(TempDataBlocks[a].Data)+b*size), size);
  end;

 TempDataBlocks := nil;
end;

function TMapReader_1.GetMapHeader: TMapHeaderRec_1;
begin
 Result := FMapHeader;
end;

function TMapReader_1.GetMapList: TMapList;
var
  MapCount: Word;
  TempMapTable: packed array of TMapTableRec_1;
  a: Integer;
begin
 Result := nil;

 CopyMemory(@MapCount, Pointer(LongWord(FDFLData)+4), 2);
 if MapCount = 0 then Exit;

 SetLength(TempMapTable, MapCount);
 SetLength(Result, MapCount);
 CopyMemory(@TempMapTable[0], Pointer(LongWord(FDFLData)+6), MapCount*SizeOf(TMapTableRec_1));

 for a := 0 to High(TempMapTable) do
  Result[a] := TempMapTable[a].Name;
end;

function TMapReader_1.GetPanels: TPanelsRec1Array;
var
  TempDataBlocks: TDataBlocksArray;
  a: Integer;
  b, Size: LongWord;
begin
 Result := nil;

 TempDataBlocks := GetBlocks(BLOCK_PANELS);

 if TempDataBlocks = nil then Exit;

 size := SizeOf(TPanelRec_1);

 for a := 0 to High(TempDataBlocks) do
  for b := 0 to (TempDataBlocks[a].Block.BlockSize div size)-1 do
  begin
   SetLength(Result, Length(Result)+1);
   CopyMemory(@Result[High(Result)], Pointer(LongWord(TempDataBlocks[a].Data)+b*size), size);
  end;

 TempDataBlocks := nil;
end;

function TMapReader_1.GetTextures: TTexturesRec1Array;
var
  TempDataBlocks: TDataBlocksArray;
  a: Integer;
  b, Size: LongWord;
begin
 Result := nil;

 TempDataBlocks := GetBlocks(BLOCK_TEXTURES);

 if TempDataBlocks = nil then Exit;

 size := SizeOf(TTextureRec_1);

 for a := 0 to High(TempDataBlocks) do
  for b := 0 to (TempDataBlocks[a].Block.BlockSize div size)-1 do
  begin
   SetLength(Result, Length(Result)+1);
   CopyMemory(@Result[High(Result)], Pointer(LongWord(TempDataBlocks[a].Data)+b*size), size);
  end;

 TempDataBlocks := nil;
end;

function TMapReader_1.GetTriggers: TTriggersRec1Array;
var
  TempDataBlocks: TDataBlocksArray;
  a: Integer;
  b, Size: LongWord;
begin
 Result := nil;

 TempDataBlocks := GetBlocks(BLOCK_TRIGGERS);

 if TempDataBlocks = nil then Exit;

 size := SizeOf(TTriggerRec_1);

 for a := 0 to High(TempDataBlocks) do
  for b := 0 to (TempDataBlocks[a].Block.BlockSize div size)-1 do
  begin
   SetLength(Result, Length(Result)+1);
   CopyMemory(@Result[High(Result)], Pointer(LongWord(TempDataBlocks[a].Data)+b*size), size);
  end;

 TempDataBlocks := nil;
end;

function TMapReader_1.LoadDFL(DFLName: string): Boolean;
var
  f: File;
  len, BytesRead: Integer;
  Sign: array[0..2] of Char;
  Version: Byte;
begin
 Result := False;

 if not FileExists(DFLName) then Exit;

 AssignFile(f, DFLName);
 {$I-}
 Reset(f, 1);
 {$I+}

 len := FileSize(f);
 if len = 0 then Exit;
 FDFLData := GetMemory(len);

 if IOResult <> 0 then
 begin
  FreeMemory(FDFLData);
  FDFLData := nil; 
  Exit;
 end;

 BlockRead(f, FDFLData^, len, BytesRead);
 CloseFile(f);

 CopyMemory(@Sign[0], FDFLData, 3);
 CopyMemory(@Version, Pointer(Integer(FDFLData)+3), 1);

 if (BytesRead <> len) or (Sign <> DFLSIGNATURE) or (Version <> $01) then
 begin
  FreeMemory(FDFLData);
  FDFLData := nil;
  Exit;
 end;

 Result := True;
end;

function TMapReader_1.LoadMap(MapName: Char32): Boolean;
var
  adr: LongWord;
  TempMapTable: packed array of TMapTableRec_1;
  MapCount, BlockCount: Word;
  a, _id: Integer;
  ok: Boolean;
begin
 Result := False;

 if FDFLData = nil then Exit;

 adr := 4;
 CopyMemory(@MapCount, Pointer(LongWord(FDFLData)+adr), 2);
 adr := adr+2;
 if MapCount = 0 then Exit;

 SetLength(TempMapTable, MapCount);
 CopyMemory(@TempMapTable[0], Pointer(LongWord(FDFLData)+adr), MapCount*SizeOf(TMapTableRec_1));

 ok := False;
 for a := 0 to High(TempMapTable) do
  if TempMapTable[a].Name = MapName then
  begin
   ok := True;
   Break;
  end;

 if not ok then Exit;

 adr := TempMapTable[a].Address;

 CopyMemory(@FMapHeader, Pointer(LongWord(FDFLData)+adr), SizeOf(TMapHeaderRec_1));
 adr := adr+SizeOf(TMapHeaderRec_1);
 CopyMemory(@BlockCount, Pointer(LongWord(FDFLData)+adr), 2);
 adr := adr+2;

 if BlockCount <> 0 then
  for a := 1 to BlockCount do
  begin
   SetLength(FDataBlocks, Length(FDataBlocks)+1);
   _id := High(FDataBlocks);

   CopyMemory(@FDataBlocks[_id].Block, Pointer(LongWord(FDFLData)+adr), SizeOf(TBlock));
   adr := adr+SizeOf(TBlock);

   FDataBlocks[_id].Data := GetMemory(FDataBlocks[_id].Block.BlockSize);

   CopyMemory(FDataBlocks[_id].Data, Pointer(LongWord(FDFLData)+adr), FDataBlocks[_id].Block.BlockSize);
   adr := adr+FDataBlocks[_id].Block.BlockSize;
  end;

 TempMapTable := nil;

 Result := True;
end;

end.
