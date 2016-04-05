unit MAPREADER;

{
-----------------------------------
MAPREADER.PAS ВЕРСИЯ ОТ 13.11.07

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

  TMapReader = class(TObject)
   private
    FError: Byte;
    FVersion: Byte;
    FDataBlocks: TDataBlocksArray;
    function GetBlocks(BlocksType: Byte): TDataBlocksArray;
   public
    constructor Create();
    destructor Destroy(); override;
    function LoadMap(Data: Pointer): Boolean;
    procedure FreeMap();
    function HandledVersion(): Byte; virtual;
    
    property GetError: Byte read FError;
    property GetVersion: Byte read FVersion;
  end;

  TMapReader_1 = class(TMapReader)
   private
   public
    function GetMapHeader(): TMapHeaderRec_1;
    function GetTextures(): TTexturesRec1Array;
    function GetPanels(): TPanelsRec1Array;
    function GetItems(): TItemsRec1Array;
    function GetAreas(): TAreasRec1Array;
    function GetMonsters(): TMonsterRec1Array;
    function GetTriggers(): TTriggersRec1Array;
    function HandledVersion(): Byte; override;
  end;

const
  MAP_ERROR_NONE      = $00;
  MAP_ERROR_SIGNATURE = $01;
  MAP_ERROR_VERSION   = $02;

  NNF_NO_NAME         = 0;
  NNF_NAME_BEFORE     = 1;
  NNF_NAME_EQUALS     = 2;
  NNF_NAME_AFTER      = 3;

function g_Texture_NumNameFindStart(name: String): Boolean;
function g_Texture_NumNameFindNext(var newName: String): Byte;

implementation

uses
  SysUtils, BinEditor;

var
  NNF_PureName: String; // Имя текстуры без цифр в конце
  NNF_FirstNum: Integer; // Число у начальной текстуры
  NNF_CurrentNum: Integer; // Следующее число у текстуры

function g_Texture_NumNameFindStart(name: String): Boolean;
var
  i: Integer;

begin
  Result := False;
  NNF_PureName := '';
  NNF_FirstNum := -1;
  NNF_CurrentNum := -1;

  for i := Length(name) downto 1 do
    if (name[i] = '_') then // "_" - символ начала номерного постфикса
    begin
      if i = Length(name) then
        begin // Нет цифр в конце строки
          Exit;
        end
      else
        begin
          NNF_PureName := Copy(name, 1, i);
          Delete(name, 1, i);
          Break;
        end;
    end;

// Не перевести в число:
  if not TryStrToInt(name, NNF_FirstNum) then
    Exit;

  NNF_CurrentNum := 0;

  Result := True;
end;

function g_Texture_NumNameFindNext(var newName: String): Byte;
begin
  if (NNF_PureName = '') or (NNF_CurrentNum < 0) then
  begin
    newName := '';
    Result := NNF_NO_NAME;
    Exit;
  end;

  newName := NNF_PureName + IntToStr(NNF_CurrentNum);

  if NNF_CurrentNum < NNF_FirstNum then
    Result := NNF_NAME_BEFORE
  else
    if NNF_CurrentNum > NNF_FirstNum then
      Result := NNF_NAME_AFTER
    else
      Result := NNF_NAME_EQUALS;

  Inc(NNF_CurrentNum);
end;

{ T M a p R e a d e r _ 1 : }

function TMapReader_1.GetAreas(): TAreasRec1Array;
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

function TMapReader_1.GetItems(): TItemsRec1Array;
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

function TMapReader_1.GetMapHeader(): TMapHeaderRec_1;
var
  TempDataBlocks: TDataBlocksArray;
begin
 ZeroMemory(@Result, SizeOf(TMapHeaderRec_1));

 TempDataBlocks := GetBlocks(BLOCK_HEADER);

 if TempDataBlocks = nil then Exit;

 CopyMemory(@Result, TempDataBlocks[0].Data, SizeOf(TMapHeaderRec_1));

 TempDataBlocks := nil;
end;

function TMapReader_1.GetMonsters(): TMonsterRec1Array;
var
  TempDataBlocks: TDataBlocksArray;
  a: Integer;
  b, Size: LongWord;
begin
 Result := nil;

 TempDataBlocks := GetBlocks(BLOCK_MONSTERS);

 if TempDataBlocks = nil then Exit;

 size := SizeOf(TMonsterRec_1);

 for a := 0 to High(TempDataBlocks) do
  for b := 0 to (TempDataBlocks[a].Block.BlockSize div size)-1 do
  begin
   SetLength(Result, Length(Result)+1);
   CopyMemory(@Result[High(Result)], Pointer(LongWord(TempDataBlocks[a].Data)+b*size), size);
  end;

 TempDataBlocks := nil;
end;

function TMapReader_1.GetPanels(): TPanelsRec1Array;
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

function TMapReader_1.GetTextures(): TTexturesRec1Array;
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

function TMapReader_1.GetTriggers(): TTriggersRec1Array;
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

function TMapReader_1.HandledVersion: Byte;
begin
 Result := $01;
end;

{ T M a p R e a d e r : }

constructor TMapReader.Create();
begin
 FDataBlocks := nil;
 FError := MAP_ERROR_NONE;
 FVersion := $00;
end;

destructor TMapReader.Destroy();
begin
 FreeMap();

 inherited;
end;

procedure TMapReader.FreeMap();
var
  a: Integer;
begin
 if FDataBlocks <> nil then
  for a := 0 to High(FDataBlocks) do
   if FDataBlocks[a].Data <> nil then FreeMem(FDataBlocks[a].Data);

 FDataBlocks := nil;
 FVersion := $00;
 FError := MAP_ERROR_NONE;
end;

function TMapReader.GetBlocks(BlocksType: Byte): TDataBlocksArray;
var
  a: Integer;
begin
 Result := nil;

 if FDataBlocks = nil then Exit;

 for a := 0 to High(FDataBlocks) do
  if FDataBlocks[a].Block.BlockType = BlocksType then
  begin
   SetLength(Result, Length(Result)+1);
   Result[High(Result)] := FDataBlocks[a];
  end;
end;

function TMapReader.HandledVersion(): Byte;
begin
 Result := $00;
end;

function TMapReader.LoadMap(Data: Pointer): Boolean;
var
  adr: LongWord;
  _id: Integer;
  Sign: array[0..2] of Char;
  Ver: Byte;
begin
 Result := False;

 CopyMemory(@Sign[0], Data, 3);
 if Sign <> MAP_SIGNATURE then
 begin
  FError := MAP_ERROR_SIGNATURE;
  Exit;
 end;
 adr := 3;

 CopyMemory(@Ver, Pointer(LongWord(Data)+adr), 1);
 FVersion := Ver;
 if Ver > HandledVersion() then
 begin
  FError := MAP_ERROR_VERSION;
  Exit;
 end;
 adr := adr+1;

 repeat
  SetLength(FDataBlocks, Length(FDataBlocks)+1);
  _id := High(FDataBlocks);

  CopyMemory(@FDataBlocks[_id].Block, Pointer(LongWord(Data)+adr), SizeOf(TBlock));
  adr := adr+SizeOf(TBlock);

  FDataBlocks[_id].Data := GetMemory(FDataBlocks[_id].Block.BlockSize);

  CopyMemory(FDataBlocks[_id].Data, Pointer(LongWord(Data)+adr), FDataBlocks[_id].Block.BlockSize);

  adr := adr+FDataBlocks[_id].Block.BlockSize;
 until FDataBlocks[_id].Block.BlockType = BLOCK_NONE;

 Result := True;
end;

end.
