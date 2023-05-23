unit MAPWRITER;

{$INCLUDE ../shared/a_modes.inc}

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
  MAPDEF, BinEditor, SysUtils, Math;

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
  blk: TBlock;
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
 CopyMemory(Pointer(PtrUInt(Data)+c), @Ver, 1);
 c := c+1;

 if FDataBlocks <> nil then
  for a := 0 to High(FDataBlocks) do
  begin
   blk := FDataBlocks[a].Block;
   {$IFDEF FPC_BIG_ENDIAN}
     blk.Reserved := NtoLE(blk.Reserved);
     blk.BlockSize := NtoLE(blk.BlockSize);
   {$ENDIF}
   CopyMemory(Pointer(PtrUInt(Data)+c), @blk, SizeOf(TBlock));
   c := c+SizeOf(TBlock);
   CopyMemory(Pointer(PtrUInt(Data)+c), FDataBlocks[a].Data, FDataBlocks[a].Block.BlockSize);
   c := c+FDataBlocks[a].Block.BlockSize;
  end;

 ZeroMemory(Pointer(PtrUInt(Data)+c), SizeOf(TBlock));
end;

function TMapWriter.HandledVersion(): Byte;
begin
 Result := $00;
end;

{ TMapWriter_1 }

function TMapWriter_1.AddAreas(Areas: TAreasRec1Array): Boolean;
var
  a, size: LongWord;
  area: TAreaRec_1;
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
  begin
   area := Areas[a];
   {$IFDEF FPC_BIG_ENDIAN}
     area.X := NtoLE(area.X);
     area.Y := NtoLE(area.Y);
   {$ENDIF}
   CopyMemory(Pointer(PtrUInt(Data)+a*Size), @area, size);
  end;
 end;
 
 Result := True;
end;

function TMapWriter_1.AddItems(Items: TItemsRec1Array): Boolean;
var
  a, size: LongWord;
  item: TItemRec_1;
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
  begin
   item := Items[a];
   {$IFDEF FPC_BIG_ENDIAN}
     item.X := NtoLE(item.X);
     item.Y := NtoLE(item.Y);
   {$ENDIF}
   CopyMemory(Pointer(PtrUInt(Data)+a*size), @item, size);
  end;
 end;
 
 Result := True;
end;

function TMapWriter_1.AddMonsters(Monsters: TMonsterRec1Array): Boolean;
var
  a, size: LongWord;
  mon: TMonsterRec_1;
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
  begin
   mon := Monsters[a];
   {$IFDEF FPC_BIG_ENDIAN}
     mon.X := NtoLE(mon.X);
     mon.Y := NtoLE(mon.Y);
   {$ENDIF}
   CopyMemory(Pointer(PtrUInt(Data)+a*Size), @mon, size);
  end;
 end;
 
 Result := True;
end;

function TMapWriter_1.AddPanels(Panels: TPanelsRec1Array): Boolean;
var
  a, size: LongWord;
  panel: TPanelRec_1;
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
  begin
   panel := Panels[a];
   {$IFDEF FPC_BIG_ENDIAN}
     panel.X := NtoLE(panel.X);
     panel.Y := NtoLE(panel.Y);
     panel.Width := NtoLE(panel.Width);
     panel.Height := NtoLE(panel.Height);
     panel.TextureNum := NtoLE(panel.TextureNum);
     panel.PanelType := NtoLE(panel.PanelType);
   {$ENDIF}
   CopyMemory(Pointer(PtrUInt(Data)+a*size), @panel, size);
  end;
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
   CopyMemory(Pointer(PtrUInt(Data)+a*size), @Textures[a], size);
 end;

 Result := True;
end;

function TMapWriter_1.AddTriggers(Triggers: TTriggersRec1Array): Boolean;
var
  a, i, size: LongWord;
  tr: TTriggerRec_1;
  data: ^TTriggerData;
begin
 if Triggers = nil then
 begin
  Result := True;
  Exit;
 end;

 SetLength(FDataBlocks, Length(FDataBlocks)+1);

 size := SizeOf(TTriggerRec_1);

 FDataBlocks[High(FDataBlocks)].Block.BlockType := BLOCK_TRIGGERS;
 FDataBlocks[High(FDataBlocks)].Block.Reserved := $00000000;
 FDataBlocks[High(FDataBlocks)].Block.BlockSize := LongWord(Length(Triggers))*size;

 FDataBlocks[High(FDataBlocks)].Data := GetMemory(FDataBlocks[High(FDataBlocks)].Block.BlockSize);

 for a := 0 to High(Triggers) do
 begin
   tr := Triggers[a];
   data := @tr.data;
   // fix broken maps
   case tr.TriggerType of
     TRIGGER_MUSIC: data.MusicAction := Min(Max(data.MusicAction, 0), 1);
   end;
   // fix endianness
   {$IFDEF FPC_BIG_ENDIAN}
     tr.X := NtoLE(tr.X);
     tr.Y := NtoLE(tr.Y);
     tr.Width := NtoLE(tr.Width);
     tr.Height := NtoLE(tr.Height);
     tr.TexturePanel := NtoLE(tr.TexturePanel);
     case tr.TriggerType of
       //TRIGGER_EXIT: ;
       TRIGGER_TELEPORT:
       begin
         data.TargetPoint.X := NtoLE(data.TargetPoint.X);
         data.TargetPoint.Y := NtoLE(data.TargetPoint.Y);
       end;
       TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR, TRIGGER_DOOR5,
       TRIGGER_CLOSETRAP, TRIGGER_TRAP, TRIGGER_LIFTUP, TRIGGER_LIFTDOWN,
       TRIGGER_LIFT:
         data.PanelID := NtoLE(data.PanelID);
       TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF:
       begin
         data.tX := NtoLE(data.tX);
         data.tY := NtoLE(data.tY);
         data.tWidth := NtoLE(data.tWidth);
         data.tHeight := NtoLE(data.tHeight);
         data.Wait := NtoLE(data.Wait);
         data.Count := NtoLE(data.Count);
         data.MonsterID := NtoLE(data.MonsterID);
       end;
       //TRIGGER_SECRET: ;
       //TRIGGER_TEXTURE: ;
       //TRIGGER_SOUND: ;
       TRIGGER_SPAWNMONSTER:
       begin
         data.MonPos.X := NtoLE(data.MonPos.X);
         data.MonPos.Y := NtoLE(data.MonPos.Y);
         data.MonHealth := NtoLE(data.MonHealth);
         data.MonCount := NtoLE(data.MonCount);
         data.MonMax := NtoLE(data.MonMax);
         data.MonDelay := NtoLE(data.MonDelay);
       end;
       TRIGGER_SPAWNITEM:
       begin
         data.ItemPos.X := NtoLE(data.ItemPos.X);
         data.ItemPos.Y := NtoLE(data.ItemPos.Y);
         data.ItemCount := NtoLE(data.ItemCount);
         data.ItemMax := NtoLE(data.ItemMax);
         data.ItemDelay := NtoLE(data.ItemDelay);
       end;
       //TRIGGER_MUSIC:
       TRIGGER_PUSH:
         data.PushAngle := NtoLE(data.PushAngle);
       //TRIGGER_SCORE:
       TRIGGER_MESSAGE:
         data.MessageTime := NtoLE(data.MessageTime);
       TRIGGER_DAMAGE:
       begin
         data.DamageValue := NtoLE(data.DamageValue);
         data.DamageInterval := NtoLE(data.DamageInterval);
       end;
       TRIGGER_HEALTH:
       begin
         data.HealValue := NtoLE(data.HealValue);
         data.HealInterval := NtoLE(data.HealInterval);
       end;
       TRIGGER_SHOT:
       begin
         data.ShotPos.X := NtoLE(data.ShotPos.X);
         data.ShotPos.Y := NtoLE(data.ShotPos.Y);
         data.ShotPanelID := NtoLE(data.ShotPanelID);
         data.ShotIntSight := NtoLE(data.ShotIntSight);
         data.ShotAngle := NtoLE(data.ShotAngle);
         data.ShotWait := NtoLE(data.ShotWait);
         data.ShotAccuracy := NtoLE(data.ShotAccuracy);
         data.ShotAmmo := NtoLE(data.ShotAmmo);
         data.ShotIntReload := NtoLE(data.ShotIntReload);
       end;
       TRIGGER_EFFECT:
       begin
         data.FXWait := NtoLE(data.FXWait);
         data.FXVelX := NtoLE(data.FXVelX);
         data.FXVelY := NtoLE(data.FXVelY);
       end;
     end;
   {$ENDIF}
   CopyMemory(Pointer(PtrUInt(FDataBlocks[High(FDataBlocks)].Data)+a*size), @tr, size);
 end;

 Result := True;
end;

function TMapWriter_1.AddHeader(MapHeader: TMapHeaderRec_1): Boolean;
var
  size: LongWord;
  hdr: TMapHeaderRec_1;
begin
 SetLength(FDataBlocks, Length(FDataBlocks)+1);

 size := SizeOf(TMapHeaderRec_1);

 with FDataBlocks[High(FDataBlocks)] do
 begin
  Block.BlockType := BLOCK_HEADER;
  Block.Reserved := $00000000;
  Block.BlockSize := size;

  Data := GetMemory(Block.BlockSize);

  hdr := MapHeader;
  {$IFDEF FPC_BIG_ENDIAN}
    hdr.Width := NtoLE(hdr.Width);
    hdr.Height := NtoLE(hdr.Height);
  {$ENDIF}
  CopyMemory(Pointer(PtrUInt(Data)), @hdr, size);
 end;
 
 Result := True;
end;

function TMapWriter_1.HandledVersion(): Byte;
begin
 Result := $01;
end;

end.
