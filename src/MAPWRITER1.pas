unit MAPWRITER1;

{
-----------------------------------
MAPWRITER1.PAS ВЕРСИЯ 1 ОТ 06.01.06
-----------------------------------
}

interface

uses
  MAPSTRUCT;

type
 TDataBlock = packed record
   Block: TBlock;
   Data:  Pointer;
  end;

 TDataBlocksArray = packed array of TDataBlock;

 TMapWriter_1 = class(TObject)
 private
  FDFLData: Pointer;
  FDFLSize: LongWord;
  FOpenedMap: Char32;
  FMapHeader: TMapHeaderRec_1; // Заголовок текущей (FOpenedMap) карты
  FMapDataBlocks: TDataBlocksArray; // Блоки текущей карты
 public
  constructor Create;
  destructor Destroy; override;
  function LoadDFL(DFLName: string): Boolean;
  function NewDFL(): Boolean; // Создать пустой DFL, без карт
  function NewMap(MapName: Char32): Boolean; // Создать новую пустую карту
  procedure FreeDFL();
  procedure FreeMap();
  function SaveDFL(DFLName: string): Boolean; // записать измененый DFL на диск
  function SaveMap: Boolean; // Записать текущую (FOpenedMap) карту в DFL. Старая с таким же именем должна быть удалена.
  function DeleteMap(MapName: Char32): Boolean; // Удалить карту. Открывать её (OpenMap) перед этим не надо
  function MapExists(MapName: Char32): Boolean;
  function AddTextures(Textures: TTexturesRec1Array): Boolean;
  function AddPanels(Panels: TPanelsRec1Array): Boolean;
  function AddItems(Items: TItemsRec1Array): Boolean;
  function AddAreas(Areas: TAreasRec1Array): Boolean;
  function AddTriggers(Triggers: TTriggersRec1Array): Boolean;
  function SetMapHeader(MapHeader: TMapHeaderRec_1): Boolean; // Установить заголовок текущей карты
 end;

implementation

uses
  Windows, SysUtils;

{ TMapWriter_1 }

function TMapWriter_1.AddAreas(Areas: TAreasRec1Array): Boolean;
var
  a, size: LongWord;
begin
 Result := False;

 if FOpenedMap = '' then Exit;
 if Areas = nil then Exit;

 SetLength(FMapDataBlocks, Length(FMapDataBlocks)+1);

 size := SizeOf(TAreaRec_1);

 with FMapDataBlocks[High(FMapDataBlocks)] do
 begin
  Block.BlockType := BLOCK_AREAS;
  Block.Version := 1;
  Block.Reserved := 0;
  Block.BlockSize := LongWord(Length(Areas))*size;

  Data := GetMemory(Block.BlockSize);

  for a := 0 to High(Areas) do
   CopyMemory(Pointer(LongWord(Data)+a*Size), @Areas[a], size);
 end;
 
 Result := True;
end;

function TMapWriter_1.AddItems(Items: TItemsRec1Array): Boolean;
var
  a, size: LongWord;
begin
 Result := False;

 if FOpenedMap = '' then Exit;
 if Items = nil then Exit;

 SetLength(FMapDataBlocks, Length(FMapDataBlocks)+1);

 size := SizeOf(TItemRec_1);

 with FMapDataBlocks[High(FMapDataBlocks)] do
 begin
  Block.BlockType := BLOCK_ITEMS;
  Block.Version := 1;
  Block.Reserved := 0;
  Block.BlockSize := LongWord(Length(Items))*size;

  Data := GetMemory(Block.BlockSize);

  for a := 0 to High(Items) do
   CopyMemory(Pointer(LongWord(Data)+a*Size), @Items[a], size);
 end;
 
 Result := True;
end;

function TMapWriter_1.AddPanels(Panels: TPanelsRec1Array): Boolean;
var
  a, size: LongWord;
begin
 Result := False;

 if FOpenedMap = '' then Exit;
 if Panels = nil then Exit;

 SetLength(FMapDataBlocks, Length(FMapDataBlocks)+1);

 size := SizeOf(TPanelRec_1);

 with FMapDataBlocks[High(FMapDataBlocks)] do
 begin
  Block.BlockType := BLOCK_PANELS;
  Block.Version := 1;
  Block.Reserved := 0;
  Block.BlockSize := LongWord(Length(Panels))*size;

  Data := GetMemory(Block.BlockSize);

  for a := 0 to High(Panels) do
   CopyMemory(Pointer(LongWord(Data)+a*Size), @Panels[a], size);
 end;
 
 Result := True;
end;

function TMapWriter_1.AddTextures(Textures: TTexturesRec1Array): Boolean;
var
  a, size: LongWord;
begin
 Result := False;

 if FOpenedMap = '' then Exit;
 if Textures = nil then Exit;

 SetLength(FMapDataBlocks, Length(FMapDataBlocks)+1);

 size := SizeOf(TTextureRec_1);

 with FMapDataBlocks[High(FMapDataBlocks)] do
 begin
  Block.BlockType := BLOCK_TEXTURES;
  Block.Version := 1;
  Block.Reserved := 0;
  Block.BlockSize := LongWord(Length(Textures))*size;

  Data := GetMemory(Block.BlockSize);

  for a := 0 to High(Textures) do
   CopyMemory(Pointer(LongWord(Data)+a*Size), @Textures[a], size);
 end;
 
 Result := True;
end;

function TMapWriter_1.AddTriggers(Triggers: TTriggersRec1Array): Boolean;
var
  a, size: LongWord;
begin
 Result := False;

 if FOpenedMap = '' then Exit;
 if Triggers = nil then Exit;

 SetLength(FMapDataBlocks, Length(FMapDataBlocks)+1);

 size := SizeOf(TTriggerRec_1);

 with FMapDataBlocks[High(FMapDataBlocks)] do
 begin
  Block.BlockType := BLOCK_TRIGGERS;
  Block.Version := 1;
  Block.Reserved := 0;
  Block.BlockSize := LongWord(Length(Triggers))*size;

  Data := GetMemory(Block.BlockSize);

  for a := 0 to High(Triggers) do
   CopyMemory(Pointer(LongWord(Data)+a*Size), @Triggers[a], size);
 end;
 
 Result := True;
end;

constructor TMapWriter_1.Create;
begin
 FDFLData := nil;
 FDFLSize := 0;
 FOpenedMap := '';
 FMapDataBlocks := nil;
end;

function TMapWriter_1.DeleteMap(MapName: Char32): Boolean;
var
  NewDFL: Pointer;
  MapCount: Word;
  OldMapTable, NewMapTable: packed array of TMapTableRec_1;
  a: Integer;
  dsize, size, adr: LongWord;
begin
 Result := False;

 if FDFLData = nil then Exit;
 if MapName = FOpenedMap then FreeMap;
 if not MapExists(MapName) then Exit;

 CopyMemory(@MapCount, Pointer(LongWord(FDFLData)+4), 2);

 if MapCount = 1 then
 begin
  ReallocMem(FDFLData, 6);
  MapCount := 0;
  CopyMemory(Pointer(LongWord(FDFLData)+4), @MapCount, 2);
  FDFLSize := 6;
 end
  else
 begin
  SetLength(OldMapTable, MapCount);
  CopyMemory(@OldMapTable[0], Pointer(LongWord(FDFLData)+6), MapCount*SizeOf(TMapTableRec_1));

  size := 6+(MapCount-1)*SizeOf(TMapTableRec_1);

  dsize := 0;

  for a := 0 to High(OldMapTable) do
   if OldMapTable[a].Name <> MapName then
   begin
    SetLength(NewMapTable, Length(NewMapTable)+1);
    with NewMapTable[High(NewMapTable)] do
    begin
     Name := OldMapTable[a].Name;
     Address := OldMapTable[a].Address-dsize;
     if dsize > 0 then Address := Address-SizeOf(TMapTableRec_1);
     Size := OldMapTable[a].Size;
    end;
    
    size := size+OldMapTable[a].Size;
   end else dsize := OldMapTable[a].Size;
    
  NewDFL := GetMemory(size);

  CopyMemory(NewDFL, FDFLData, 4);

  MapCount := MapCount-1;
  CopyMemory(Pointer(LongWord(NewDFL)+4), @MapCount, 2);

  CopyMemory(Pointer(LongWord(NewDFL)+6), @NewMapTable[0], MapCount*SizeOf(TMapTableRec_1));

  adr := 6+MapCount*SizeOf(TMapTableRec_1);

  for a := 0 to High(OldMapTable) do
   if not (OldMapTable[a].Name = MapName) then
   begin
    CopyMemory(Pointer(LongWord(NewDFL)+adr), Pointer(LongWord(FDFLData)+OldMapTable[a].Address), OldMapTable[a].Size);
    adr := adr+OldMapTable[a].Size;
   end;

  FreeMem(FDFLData);
  FDFLData := NewDFL;
  FDFLSize := size;
 end;

 Result := True;
end;

destructor TMapWriter_1.Destroy;
begin
 FreeDFL;

 inherited;
end;

procedure TMapWriter_1.FreeDFL;
begin
 FreeMap;

 if FDFLData <> nil then
 begin
  FreeMemory(FDFLData);
  FDFLData := nil;
 end;

 FDFLSize := 0;
end;

procedure TMapWriter_1.FreeMap;
begin
 FOpenedMap := '';
 FMapDataBlocks := nil;
end;

function TMapWriter_1.LoadDFL(DFLName: string): Boolean;
var
  f: File;
  len, BytesRead: Integer;
  Sign: packed array[0..2] of Char;
  Version: Byte;
begin
 Result := False;

 if not FileExists(DFLName) then Exit;

 FreeDFL;

 AssignFile(f, DFLName);
 {$I-}
 Reset(f, 1);
 {$I+}

 len := FileSize(f);
 if len = 0 then Exit;
 FDFLData := GetMemory(len);
 FDFLSize := len;

 if IOResult <> 0 then
 begin
  FreeDFL; 
  Exit;
 end;

 BlockRead(f, FDFLData^, len, BytesRead);
 CloseFile(f);

 CopyMemory(@Sign[0], FDFLData, 3);
 CopyMemory(@Version, Pointer(Integer(FDFLData)+3), 1);

 if (BytesRead <> len) or (Sign <> DFLSIGNATURE) or (Version <> $01) then
 begin
  FreeDFL;
  Exit;
 end;

 Result := True;
end;

{function TMapWriter_1.LoadMap(MapName: Char32): Boolean;
var
  MapCount: Word;
  TempMapTable: packed array of TMapTableRec_1;
  a: Integer;
  adr: LongWord;
begin
 Result := False;

 if not MapExists(MapName) then Exit; 

 Result := False;

 if FDFLData = nil then Exit;

 CopyMemory(@MapCount, Pointer(LongWord(FDFLData)+4), 2);

 SetLength(TempMapTable, MapCount);
 CopyMemory(@TempMapTable, Pointer(LongWord(FDFLData)+6), MapCount*SizeOf(TMapTableRec_1));

 for a := 0 to High(TempMapTable) do
  if MapName = TempMapTable[a].Name then
  begin
   adr := TempMapTable[a].Address;
   Break;
  end;

 FOpenedMap := MapName;

 Result := True;
end;}

function TMapWriter_1.MapExists(MapName: Char32): Boolean;
var
  MapCount: Word;
  TempMapTable: packed array of TMapTableRec_1;
  a: Integer;
begin
 Result := False;

 if FDFLData = nil then Exit;

 CopyMemory(@MapCount, Pointer(LongWord(FDFLData)+4), 2);
 if MapCount = 0 then Exit;

 SetLength(TempMapTable, MapCount);
 CopyMemory(@TempMapTable[0], Pointer(LongWord(FDFLData)+6), MapCount*SizeOf(TMapTableRec_1));

 for a := 0 to High(TempMapTable) do
  if MapName = TempMapTable[a].Name then
  begin
   Result := True;
   Break;
  end;
end;

function TMapWriter_1.NewDFL(): Boolean;
var
 Sign: packed array[0..2] of Char;
 Ver: Byte;
 MapCount: Word;
begin
 Result := False;

 FreeDFL;

 FDFLData := GetMemory(3+1+2);
 if FDFLData = nil then Exit;
 FDFLSize := 6;

 Sign := DFLSIGNATURE;
 Ver := DFLVERSION;
 MapCount := 0;

 CopyMemory(FDFLData, @Sign[0], 3);
 CopyMemory(Pointer(LongWord(FDFLData)+3), @Ver, 1);
 CopyMemory(Pointer(LongWord(FDFLData)+4), @MapCount, 2);

 Result := True;
end;

function TMapWriter_1.NewMap(MapName: Char32): Boolean;
begin
 Result := False;

 if FDFLData = nil then Exit;

 FreeMap;

 FOpenedMap := MapName;

 Result := True;
end;

function TMapWriter_1.SaveDFL(DFLName: string): Boolean;
var
  f: File;
begin
 Result := False;

 if FDFLData = nil then Exit;

 AssignFile(f, DFLName);
 {$I-}
 Rewrite(f, 1);
 {$I+}

 if IOResult <> 0 then  Exit;

 BlockWrite(f, FDFLData^, FDFLSize);
 CloseFile(f);

 Result := True;
end;

function TMapWriter_1.SaveMap: Boolean;
var
  NewDFL, MapData: Pointer;
  MapCount, BlockCount: Word;
  OldMapTable, NewMapTable: packed array of TMapTableRec_1;
  a: Integer;
  size, adr: LongWord;
begin
 Result := False;

 if FOpenedMap = '' then Exit;
 if FDFLData = nil then Exit;
 if MapExists(FOpenedMap) then Exit;

 CopyMemory(@MapCount, Pointer(LongWord(FDFLData)+4), 2);
 SetLength(OldMapTable, MapCount);
 SetLength(NewMapTable, MapCount+1);

 adr := 6+(MapCount+1)*SizeOf(TMapTableRec_1);
 if MapCount > 0 then
 begin
  CopyMemory(@OldMapTable[0], Pointer(LongWord(FDFLData)+6), MapCount*SizeOf(TMapTableRec_1));
  CopyMemory(@NewMapTable[0], Pointer(LongWord(FDFLData)+6), MapCount*SizeOf(TMapTableRec_1));

  for a := 0 to High(NewMapTable)-1 do
   with NewMapTable[a] do
   begin
    Address := Address+SizeOf(TMapTableRec_1);
    adr := adr+Size;
   end;
 end;

 with NewMapTable[High(NewMapTable)] do
 begin
  Name := FOpenedMap;
  Address := adr;
  Size := SizeOf(TMapHeaderRec_1)+2;

  if FMapDataBlocks <> nil then
  begin
   Size := Size+LongWord(Length(FMapDataBlocks))*SizeOf(TBlock);

   for a := 0 to High(FMapDataBlocks) do
    Size := Size+FMapDataBlocks[a].Block.BlockSize;
  end;
 end;

 size := 0;
 for a := 0 to High(NewMapTable) do
  size := size+NewMapTable[a].Size;

 NewDFL := GetMemory(6+(MapCount+1)*SizeOf(TMapTableRec_1)+size);
 MapData := GetMemory(NewMapTable[High(NewMapTable)].Size);

 CopyMemory(MapData, @FMapHeader, SizeOf(TMapHeaderRec_1));
 BlockCount := Length(FMapDataBlocks);
 CopyMemory(Pointer(LongWord(MapData)+SizeOf(TMapHeaderRec_1)), @BlockCount, 2);

 if FMapDataBlocks <> nil then
 begin
  adr := SizeOf(TMapHeaderRec_1)+2;

  for a := 0 to High(FMapDataBlocks) do
  begin
   CopyMemory(Pointer(LongWord(MapData)+adr), @FMapDataBlocks[a].Block, SizeOf(TBlock));
   adr := adr+SizeOf(TBlock);
   CopyMemory(Pointer(LongWord(MapData)+adr), FMapDataBlocks[a].Data, FMapDataBlocks[a].Block.BlockSize);
   adr := adr+FMapDataBlocks[a].Block.BlockSize;
  end;
 end;

 CopyMemory(NewDFL, FDFLData, 4);
 MapCount := Length(NewMapTable);
 CopyMemory(Pointer(LongWord(NewDFL)+4), @MapCount, 2);
 CopyMemory(Pointer(LongWord(NewDFL)+6), @NewMapTable[0], MapCount*SizeOf(TMapTableRec_1));

 adr := 6+MapCount*SizeOf(TMapTableRec_1);

 if Length(NewMapTable) > 1 then
  for a := 0 to High(NewMapTable)-1 do
  begin
   CopyMemory(Pointer(LongWord(NewDFL)+adr), Pointer(LongWord(FDFLData)+OldMapTable[a].Address),
              OldMapTable[a].Size);
   adr := adr+OldMapTable[a].Size;
  end;

 CopyMemory(Pointer(LongWord(NewDFL)+adr), MapData, NewMapTable[High(NewMapTable)].Size);

 FDFLSize := 6+MapCount*SizeOf(TMapTableRec_1);
 for a := 0 to High(NewMapTable) do
  FDFLSize := FDFLSize+NewMapTable[a].Size;

 FreeMem(FDFLData);
 FreeMem(MapData);
 OldMapTable := nil;
 NewMapTable := nil;
 FDFLData := NewDFL;
 
 Result := True;
end;

function TMapWriter_1.SetMapHeader(MapHeader: TMapHeaderRec_1): Boolean;
begin
 Result := False;

 if FOpenedMap = '' then Exit;

 FMapHeader := MapHeader;

 Result := True;
end;

end.
