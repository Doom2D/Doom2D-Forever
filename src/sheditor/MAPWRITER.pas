{$MODE DELPHI}
unit MAPWRITER;

{
-----------------------------------
MAPWRITER.PAS ВЕРСИЯ ОТ 24.09.06

Поддержка карт версии 1
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

  TMapWriter = class(TObject)
   private
    FDataBlocks: TDataBlocksArray;
   public
    constructor Create();
    destructor Destroy(); override;
    procedure FreeMap();
    function SaveMap(var Data: Pointer): LongWord;
    function HandledVersion(): Byte; virtual;
  end;

  TMapWriter_1 = class(TMapWriter)
   public
    function AddTextures(Textures: TTexturesRec1Array): Boolean;
    function AddPanels(Panels: TPanelsRec1Array): Boolean;
    function AddItems(Items: TItemsRec1Array): Boolean;
    function AddMonsters(Monsters: TMonsterRec1Array): Boolean;
    function AddAreas(Areas: TAreasRec1Array): Boolean;
    function AddTriggers(Triggers: TTriggersRec1Array): Boolean;
    function AddHeader(MapHeader: TMapHeaderRec_1): Boolean;
    function HandledVersion(): Byte; override;
  end;


implementation

uses
  BinEditor, SysUtils;

{ TMapWriter }

constructor TMapWriter.Create();
begin
 FDataBlocks := nil;
end;

destructor TMapWriter.Destroy();
begin
 FreeMap();

 inherited;
end;

procedure TMapWriter.FreeMap();
var
  a: Integer;
begin
 if FDataBlocks <> nil then
  for a := 0 to High(FDataBlocks) do
   if FDataBlocks[a].Data <> nil then FreeMem(FDataBlocks[a].Data);

 FDataBlocks := nil;
end;

function TMapWriter.SaveMap(var Data: Pointer): LongWord;
var
  a: Integer;
  b, c: LongWord;
  Sign: array[0..2] of Char;
  Ver: Byte;
begin
 b := 3+1+SizeOf(TBlock)*(Length(FDataBlocks)+1);

 if FDataBlocks <> nil then
  for a := 0 to High(FDataBlocks) do
   b := b+FDataBlocks[a].Block.BlockSize;

 Result := b;

 GetMem(Data, b);

 Sign := MAP_SIGNATURE;
 CopyMemory(Data, @Sign[0], 3);
 c := 3;

 Ver := HandledVersion();
 CopyMemory(Pointer(LongWord(Data)+c), @Ver, 1);
 c := c+1;

 if FDataBlocks <> nil then
  for a := 0 to High(FDataBlocks) do
  begin
   CopyMemory(Pointer(LongWord(Data)+c), @FDataBlocks[a].Block, SizeOf(TBlock));
   c := c+SizeOf(TBlock);
   CopyMemory(Pointer(LongWord(Data)+c), FDataBlocks[a].Data, FDataBlocks[a].Block.BlockSize);
   c := c+FDataBlocks[a].Block.BlockSize;
  end;

 ZeroMemory(Pointer(LongWord(Data)+c), SizeOf(TBlock));
end;

function TMapWriter.HandledVersion(): Byte;
begin
 Result := $00;
end;

{ TMapWriter_1 }

function TMapWriter_1.AddAreas(Areas: TAreasRec1Array): Boolean;
var
  a, size: LongWord;
begin
 if Areas = nil then
 begin
  Result := True;
  Exit;
 end;

 SetLength(FDataBlocks, Length(FDataBlocks)+1);

 size := SizeOf(TAreaRec_1);

 with FDataBlocks[High(FDataBlocks)] do
 begin
  Block.BlockType := BLOCK_AREAS;
  Block.Reserved := $00000000;
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
 if Items = nil then
 begin
  Result := True;
  Exit;
 end;

 SetLength(FDataBlocks, Length(FDataBlocks)+1);

 size := SizeOf(TItemRec_1);

 with FDataBlocks[High(FDataBlocks)] do
 begin
  Block.BlockType := BLOCK_ITEMS;
  Block.Reserved := $00000000;
  Block.BlockSize := LongWord(Length(Items))*size;

  Data := GetMemory(Block.BlockSize);

  for a := 0 to High(Items) do
   CopyMemory(Pointer(LongWord(Data)+a*size), @Items[a], size);
 end;

 Result := True;
end;

function TMapWriter_1.AddMonsters(Monsters: TMonsterRec1Array): Boolean;
var
  a, size: LongWord;
begin
 if Monsters = nil then
 begin
  Result := True;
  Exit;
 end;

 SetLength(FDataBlocks, Length(FDataBlocks)+1);

 size := SizeOf(TMonsterRec_1);

 with FDataBlocks[High(FDataBlocks)] do
 begin
  Block.BlockType := BLOCK_MONSTERS;
  Block.Reserved := $00000000;
  Block.BlockSize := LongWord(Length(Monsters))*size;

  Data := GetMemory(Block.BlockSize);

  for a := 0 to High(Monsters) do
   CopyMemory(Pointer(LongWord(Data)+a*Size), @Monsters[a], size);
 end;

 Result := True;
end;

function TMapWriter_1.AddPanels(Panels: TPanelsRec1Array): Boolean;
var
  a, size: LongWord;
begin
 if Panels = nil then
 begin
  Result := True;
  Exit;
 end;

 SetLength(FDataBlocks, Length(FDataBlocks)+1);

 size := SizeOf(TPanelRec_1);

 with FDataBlocks[High(FDataBlocks)] do
 begin
  Block.BlockType := BLOCK_PANELS;
  Block.Reserved := $00000000;
  Block.BlockSize := LongWord(Length(Panels))*size;

  Data := GetMemory(Block.BlockSize);

  for a := 0 to High(Panels) do
   CopyMemory(Pointer(LongWord(Data)+a*size), @Panels[a], size);
 end;

 Result := True;
end;

function TMapWriter_1.AddTextures(Textures: TTexturesRec1Array): Boolean;
var
  a, size: LongWord;
begin
 if Textures = nil then
 begin
  Result := True;
  Exit;
 end;

 SetLength(FDataBlocks, Length(FDataBlocks)+1);

 size := SizeOf(TTextureRec_1);

 with FDataBlocks[High(FDataBlocks)] do
 begin
  Block.BlockType := BLOCK_TEXTURES;
  Block.Reserved := $00000000;
  Block.BlockSize := LongWord(Length(Textures))*size;

  Data := GetMemory(Block.BlockSize);

  for a := 0 to High(Textures) do
   CopyMemory(Pointer(LongWord(Data)+a*size), @Textures[a], size);
 end;

 Result := True;
end;

function TMapWriter_1.AddTriggers(Triggers: TTriggersRec1Array): Boolean;
var
  a, size: LongWord;
begin
 if Triggers = nil then
 begin
  Result := True;
  Exit;
 end;

 SetLength(FDataBlocks, Length(FDataBlocks)+1);

 size := SizeOf(TTriggerRec_1);

 with FDataBlocks[High(FDataBlocks)] do
 begin
  Block.BlockType := BLOCK_TRIGGERS;
  Block.Reserved := $00000000;
  Block.BlockSize := LongWord(Length(Triggers))*size;

  Data := GetMemory(Block.BlockSize);

  for a := 0 to High(Triggers) do
   CopyMemory(Pointer(LongWord(Data)+a*size), @Triggers[a], size);
 end;

 Result := True;
end;

function TMapWriter_1.AddHeader(MapHeader: TMapHeaderRec_1): Boolean;
var
  size: LongWord;
begin
 SetLength(FDataBlocks, Length(FDataBlocks)+1);

 size := SizeOf(TMapHeaderRec_1);

 with FDataBlocks[High(FDataBlocks)] do
 begin
  Block.BlockType := BLOCK_HEADER;
  Block.Reserved := $00000000;
  Block.BlockSize := size;

  Data := GetMemory(Block.BlockSize);

  CopyMemory(Pointer(LongWord(Data)), @MapHeader, size);
 end;

 Result := True;
end;

function TMapWriter_1.HandledVersion(): Byte;
begin
 Result := $01;
end;

end.
