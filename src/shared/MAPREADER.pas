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
  SysUtils, BinEditor, MAPDEF;

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
  b, Size: NativeUInt;
begin
 Result := nil;

 TempDataBlocks := GetBlocks(BLOCK_AREAS);

 if TempDataBlocks = nil then Exit;

 size := SizeOf_TAreaRec_1;

 for a := 0 to High(TempDataBlocks) do
  for b := 0 to (TempDataBlocks[a].Block.BlockSize div size)-1 do
  begin
   SetLength(Result, Length(Result)+1);
   //CopyMemory(@Result[High(Result)], Pointer(LongWord(TempDataBlocks[a].Data)+b*size), size);
   mb_Read_TAreaRec_1(Result[High(Result)], Pointer(NativeUInt(TempDataBlocks[a].Data)+b*size)^, size);
  end;

 TempDataBlocks := nil;
end;

function TMapReader_1.GetItems(): TItemsRec1Array;
var
  TempDataBlocks: TDataBlocksArray;
  a: Integer;
  b, Size: NativeUInt;
begin
 Result := nil;

 TempDataBlocks := GetBlocks(BLOCK_ITEMS);

 if TempDataBlocks = nil then Exit;

 size := SizeOf_TItemRec_1;

 for a := 0 to High(TempDataBlocks) do
  for b := 0 to (TempDataBlocks[a].Block.BlockSize div size)-1 do
  begin
   SetLength(Result, Length(Result)+1);
   //CopyMemory(@Result[High(Result)], Pointer(LongWord(TempDataBlocks[a].Data)+b*size), size);
   mb_Read_TItemRec_1(Result[High(Result)], Pointer(NativeUInt(TempDataBlocks[a].Data)+b*size)^, size);
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

 //CopyMemory(@Result, TempDataBlocks[0].Data, SizeOf(TMapHeaderRec_1));
 mb_Read_TMapHeaderRec_1(Result, TempDataBlocks[0].Data^, SizeOf_TMapHeaderRec_1);

 TempDataBlocks := nil;
end;

function TMapReader_1.GetMonsters(): TMonsterRec1Array;
var
  TempDataBlocks: TDataBlocksArray;
  a: Integer;
  b, Size: NativeUInt;
begin
 Result := nil;

 TempDataBlocks := GetBlocks(BLOCK_MONSTERS);

 if TempDataBlocks = nil then Exit;

 size := SizeOf_TMonsterRec_1;

 for a := 0 to High(TempDataBlocks) do
  for b := 0 to (TempDataBlocks[a].Block.BlockSize div size)-1 do
  begin
   SetLength(Result, Length(Result)+1);
   //CopyMemory(@Result[High(Result)], Pointer(LongWord(TempDataBlocks[a].Data)+b*size), size);
   mb_Read_TMonsterRec_1(Result[High(Result)], Pointer(NativeUInt(TempDataBlocks[a].Data)+b*size)^, size);
  end;

 TempDataBlocks := nil;
end;

function TMapReader_1.GetPanels(): TPanelsRec1Array;
var
  TempDataBlocks: TDataBlocksArray;
  a: Integer;
  b, Size: NativeUInt;
begin
 Result := nil;

 TempDataBlocks := GetBlocks(BLOCK_PANELS);

 if TempDataBlocks = nil then Exit;

 size := SizeOf_TPanelRec_1;

 for a := 0 to High(TempDataBlocks) do
  for b := 0 to (TempDataBlocks[a].Block.BlockSize div size)-1 do
  begin
   SetLength(Result, Length(Result)+1);
   //CopyMemory(@Result[High(Result)], Pointer(LongWord(TempDataBlocks[a].Data)+b*size), size);
   mb_Read_TPanelRec_1(Result[High(Result)], Pointer(NativeUInt(TempDataBlocks[a].Data)+b*size)^, size);
  end;

 TempDataBlocks := nil;
end;

function TMapReader_1.GetTextures(): TTexturesRec1Array;
var
  TempDataBlocks: TDataBlocksArray;
  a: Integer;
  b, Size: NativeUInt;
begin
 Result := nil;

 TempDataBlocks := GetBlocks(BLOCK_TEXTURES);

 if TempDataBlocks = nil then Exit;

 size := SizeOf_TTextureRec_1;

 for a := 0 to High(TempDataBlocks) do
  for b := 0 to (TempDataBlocks[a].Block.BlockSize div size)-1 do
  begin
   SetLength(Result, Length(Result)+1);
    //CopyMemory(@Result[High(Result)], Pointer(LongWord(TempDataBlocks[a].Data)+b*size), size);
    mb_Read_TTextureRec_1(Result[High(Result)], Pointer(NativeUInt(TempDataBlocks[a].Data)+b*size)^, size);
  end;

 TempDataBlocks := nil;
end;

function TMapReader_1.GetTriggers(): TTriggersRec1Array;
var
  TempDataBlocks: TDataBlocksArray;
  a: Integer;
  b: NativeUInt;
  Size: LongWord;
  trdata: TTriggerData;
begin
 Result := nil;

 TempDataBlocks := GetBlocks(BLOCK_TRIGGERS);

 if TempDataBlocks = nil then Exit;

 size := SizeOf_TTriggerRec_1;

 for a := 0 to High(TempDataBlocks) do
  for b := 0 to (TempDataBlocks[a].Block.BlockSize div size)-1 do
  begin
   SetLength(Result, Length(Result)+1);
   //CopyMemory(@Result[High(Result)], Pointer(LongWord(TempDataBlocks[a].Data)+b*size), size);
   mb_Read_TTriggerRec_1(Result[High(Result)], Pointer(NativeUInt(TempDataBlocks[a].Data)+b*size)^, size);
   if (Result[High(Result)].TriggerType <> 0) then
   begin
     // preprocess trigger data
     ZeroMemory(@trdata, SizeOf(trdata));
     mb_Read_TriggerData(trdata, Result[High(Result)].TriggerType, Result[High(Result)].DATA, sizeof(trdata));
     Result[High(Result)].DATA := trdata.Default;
   end;
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
  adr: NativeUInt;
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

 CopyMemory(@Ver, Pointer(NativeUInt(Data)+adr), 1);
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

  CopyMemory(@FDataBlocks[_id].Block, Pointer(NativeUInt(Data)+adr), SizeOf(TBlock));
  adr := adr+SizeOf(TBlock);

  FDataBlocks[_id].Data := GetMemory(FDataBlocks[_id].Block.BlockSize);

  CopyMemory(FDataBlocks[_id].Data, Pointer(NativeUInt(Data)+adr), FDataBlocks[_id].Block.BlockSize);

  adr := adr+FDataBlocks[_id].Block.BlockSize;
 until FDataBlocks[_id].Block.BlockType = BLOCK_NONE;

 Result := True;
end;

end.
