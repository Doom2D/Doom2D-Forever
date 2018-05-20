Unit g_map;

{$INCLUDE ../shared/a_modes.inc}

Interface

Uses
  LCLIntf, LCLType, g_basic, e_graphics, MAPREADER, MAPSTRUCT,
  MAPWRITER, e_log, MAPDEF, utils;

Type
  TMapObject = record
    ObjectType: Byte;
    ID: DWORD;
    Live: Boolean;
  end;

  TPanel = record
    TextureID:     DWORD;
    TextureWidth,
    TextureHeight: Word;
    X, Y:          Integer;
    Width, Height: Word;
    PanelType:     Word;
    Alpha:         Byte;
    Blending:      Boolean;
    TextureName:   String;
  end;

  TItem = record
    X, Y:     Integer;
    ItemType: Byte;
    OnlyDM:   Boolean;
    Fall:     Boolean;
  end;

  TArea = record
    X, Y:      Integer;
    AreaType:  Byte;
    Direction: TDirection;
  end;

  TMonster = record
    X, Y:  Integer;
    MonsterType:   Byte;
    Direction:     TDirection;
  end;

  TTrigger = record
    X, Y:          Integer;
    Width, Height: Word;
    Enabled:       Boolean;
    TexturePanel:  Integer;
    TriggerType:   Byte;
    ActivateType:  Byte;
    Key:           Byte;
    Data:          TTriggerData;
  end;

  TMapInfo = record
    FileName:    String;
    MapName:     String;
    Name:        String;
    Description: String;
    Author:      String;
    MusicName:   String;
    SkyName:     String;
    Height:      Word;
    Width:       Word;
  end;

  TMonsterSizeDelta = record
    X, Y: Integer;
    Width: Word;
  end;

const
  OBJECT_PANEL    = 1;
  OBJECT_ITEM     = 2;
  OBJECT_MONSTER  = 3;
  OBJECT_AREA     = 4;
  OBJECT_TRIGGER  = 5;

  COLOR_EDGE: Integer = $000000;
  ALPHA_EDGE: Byte = 96;
  ALPHA_LINE: Byte = 208;
  ALPHA_AREA: Byte = 96;

  TEXTURE_NAME_WATER = '_water_0';
  TEXTURE_NAME_ACID1 = '_water_1';
  TEXTURE_NAME_ACID2 = '_water_2';
  TEXTURE_SPECIAL_NONE  = DWORD(-1);
  TEXTURE_SPECIAL_WATER = DWORD(-2);
  TEXTURE_SPECIAL_ACID1 = DWORD(-3);
  TEXTURE_SPECIAL_ACID2 = DWORD(-4);
  TEXTURE_SPECIAL_NOTEXTURE = DWORD(-8);

  ItemSize: Array [ITEM_MEDKIT_SMALL..ITEM_MAX] of Array [0..1] of Byte =
    (((14), (15)), // MEDKIT_SMALL
     ((28), (19)), // MEDKIT_LARGE
     ((28), (19)), // MEDKIT_BLACK
     ((31), (16)), // ARMOR_GREEN
     ((31), (16)), // ARMOR_BLUE
     ((25), (25)), // SPHERE_BLUE
     ((25), (25)), // SPHERE_WHITE
     ((24), (47)), // SUIT
     ((14), (27)), // OXYGEN
     ((25), (25)), // INVUL
     ((62), (24)), // WEAPON_SAW
     ((63), (12)), // WEAPON_SHOTGUN1
     ((54), (13)), // WEAPON_SHOTGUN2
     ((54), (16)), // WEAPON_CHAINGUN
     ((62), (16)), // WEAPON_ROCKETLAUNCHER
     ((54), (16)), // WEAPON_PLASMA
     ((61), (36)), // WEAPON_BFG
     ((54), (16)), // WEAPON_SUPERPULEMET
     (( 9), (11)), // AMMO_BULLETS
     ((28), (16)), // AMMO_BULLETS_BOX
     ((15), ( 7)), // AMMO_SHELLS
     ((32), (12)), // AMMO_SHELLS_BOX
     ((12), (27)), // AMMO_ROCKET
     ((54), (21)), // AMMO_ROCKET_BOX
     ((15), (12)), // AMMO_CELL
     ((32), (21)), // AMMO_CELL_BIG
     ((22), (29)), // AMMO_BACKPACK
     ((16), (16)), // KEY_RED
     ((16), (16)), // KEY_GREEN
     ((16), (16)), // KEY_BLUE
     (( 1), ( 1)), // WEAPON_KASTET
     ((43), (16)), // WEAPON_PISTOL
     ((14), (18)), // BOTTLE
     ((16), (15)), // HELMET
     ((32), (24)), // JETPACK
     ((25), (25)), // INVIS
     ((53), (20)), // WEAPON_FLAMETHROWER
     ((13), (20))); // AMMO_FUELCAN

  AreaSize: Array [AREA_PLAYERPOINT1..AREA_BLUETEAMPOINT] of TRectWH =
    ((X:15; Y:12; Width:34; Height:52), // PLAYERPOINT1
     (X:15; Y:12; Width:34; Height:52), // PLAYERPOINT2
     (X:15; Y:12; Width:34; Height:52), // DMPOINT
     (X:15; Y:11; Width:34; Height:52), // REDFLAG
     (X:15; Y:11; Width:34; Height:52), // BLUEFLAG
     (X:15; Y:11; Width:34; Height:52), // DOMFLAG
     (X:15; Y:12; Width:34; Height:52), // REDTEAMPOINT
     (X:15; Y:12; Width:34; Height:52)); // BLUETEAMPOINT

  MonsterSize: Array [MONSTER_DEMON..MONSTER_MAN] of TRectWH =
    ((X:  7; Y:  8; Width:  50; Height:  52),  // DEMON
     (X: 15; Y: 10; Width:  34; Height:  50),  // IMP
     (X: 15; Y:  8; Width:  34; Height:  52),  // ZOMBY
     (X: 15; Y:  8; Width:  34; Height:  52),  // SERG
     (X: 24; Y:  9; Width:  80; Height: 110),  // CYBER
     (X: 15; Y:  4; Width:  34; Height:  56),  // CGUN
     (X: 39; Y: 32; Width:  50; Height:  64),  // BARON
     (X: 39; Y: 32; Width:  50; Height:  64),  // KNIGHT
     (X: 34; Y: 36; Width:  60; Height:  56),  // CACO
     (X: 16; Y: 14; Width:  32; Height:  36),  // SOUL
     (X: 34; Y: 36; Width:  60; Height:  56),  // PAIN
     (X: 23; Y: 14; Width: 210; Height: 100),  // SPIDER
     (X: 14; Y: 17; Width: 100; Height:  42),  // BSP
     (X: 28; Y: 34; Width:  72; Height:  60),  // MANCUB
     (X: 30; Y: 28; Width:  68; Height:  72),  // SKEL
     (X: 30; Y: 28; Width:  68; Height:  72),  // VILE
     (X:  6; Y: 11; Width:  20; Height:  10),  // FISH
     (X: 20; Y: 13; Width:  24; Height:  36),  // BARREL
     (X: 30; Y: 26; Width:  68; Height:  76),  // ROBO
     (X: 15; Y:  6; Width:  34; Height:  52)); // MAN

  MonsterSizeDelta: Array [MONSTER_DEMON..MONSTER_MAN] of TMonsterSizeDelta =
    ((X:  1; Y:   4; Width:  64),  // DEMON
     (X:  8; Y:  -4; Width:  64),  // IMP
     (X:  1; Y:  -4; Width:  64),  // ZOMBY
     (X:  0; Y:  -4; Width:  64),  // SERG
     (X:  2; Y:  -6; Width: 128),  // CYBER
     (X: -1; Y:  -2; Width:  64),  // CGUN
     (X:  4; Y:   0; Width: 128),  // BARON
     (X:  4; Y:   0; Width: 128),  // KNIGHT
     (X:  0; Y:  -4; Width: 128),  // CACO
     (X:  1; Y: -10; Width:  64),  // SOUL
     (X: -1; Y:  -3; Width: 128),  // PAIN
     (X: -4; Y:  -4; Width: 256),  // SPIDER
     (X:  0; Y:  -1; Width: 128),  // BSP
     (X: -2; Y:  -7; Width: 128),  // MANCUB
     (X: -1; Y:   4; Width: 128),  // SKEL
     (X:  5; Y: -21; Width: 128),  // VILE
     (X: -1; Y:   0; Width:  32),  // FISH
     (X:  0; Y: -15; Width:  64),  // BARREL
     (X: -2; Y: -26; Width: 128),  // ROBO
     (X:  0; Y:  -6; Width:  64)); // MAN

var
  gColorEdge: Integer;
  gAlphaEdge: Byte;
  gAlphaTriggerLine: Byte;
  gAlphaTriggerArea: Byte;
  gAlphaMonsterRect: Byte;
  gAlphaAreaRect: Byte;
  drEdge: Array[0..3] of Byte;
  gPanels: Array of TPanel;
  gItems: Array of TItem;
  gAreas: Array of TArea;
  gMonsters: Array of TMonster;
  gTriggers: Array of TTrigger;
  gMapInfo: TMapInfo;
  MapOffset: TPoint = (X: 0; Y: 0);
  SelectedObjects: Array of TMapObject = nil;

procedure LoadSky(Res: String);

function  AddItem(Item: TItem): DWORD;
function  AddPanel(Panel: TPanel): DWORD;
function  AddArea(Area: TArea): DWORD;
function  AddMonster(Monster: TMonster): DWORD;
function  AddTrigger(Trigger: TTrigger): DWORD;

procedure RemoveObject(ID: DWORD; ObjectType: Byte);
function  PanelInShownLayer(PanelType: Word): Boolean;
function  ObjectInRect(fX, fY: Integer; fWidth, fHeight: Word; ObjectType: Byte; All: Boolean): DWArray;
function  ObjectCollideLevel(fID: DWORD; ObjectType: Byte; dX, dY: Integer): Boolean;
function  ObjectCollide(ObjectType: Byte; ID: DWORD; fX, fY: Integer; fWidth, fHeight: Word): Boolean;
function  ObjectGetRect(ObjectType: Byte; ID: DWORD): TRectWH;
procedure MoveObject(ObjectType: Byte; ID: DWORD; dX, dY: Integer);
procedure ResizeObject(ObjectType: Byte; ID: DWORD; dWidth, dHeight: Integer; ResizeDir: Byte);
function  ObjectSelected(ObjectType: Byte; ID: DWORD): Boolean;

function  GetPanelName(PanelType: Word): String;
function  GetPanelType(PanelName: String): Word;
function  GetTriggerName(TriggerType: Byte): String;
function  GetTriggerType(TriggerName: String): Byte;

function  IsSpecialTexture(TextureName: String): Boolean;
function  SpecialTextureID(TextureName: String): DWORD;

procedure ClearMap();
function  SaveMap(Res: String): Pointer;
function  LoadMap(Res: String): Boolean;
function  LoadMapOld(_FileName: String): Boolean;
procedure DrawMap();
procedure LoadData();
procedure FreeData();

procedure ShiftMapObjects(dx, dy: Integer);

implementation

uses
  BinEditor, g_textures, Dialogs, SysUtils, CONFIG, f_main,
  Forms, Math, f_addresource_texture, WADEDITOR, g_language;

const
  OLD_ITEM_MEDKIT_SMALL          = 1;
  OLD_ITEM_MEDKIT_LARGE          = 2;
  OLD_ITEM_ARMOR_GREEN           = 3;
  OLD_ITEM_ARMOR_BLUE            = 4;
  OLD_ITEM_SPHERE_BLUE           = 5;
  OLD_ITEM_SPHERE_WHITE          = 6;
  OLD_ITEM_WEAPON_SAW            = 7;
  OLD_ITEM_WEAPON_SHOTGUN1       = 8;
  OLD_ITEM_WEAPON_SHOTGUN2       = 9;
  OLD_ITEM_WEAPON_CHAINGUN       = 10;
  OLD_ITEM_WEAPON_ROCKETLAUNCHER = 11;
  OLD_ITEM_WEAPON_PLASMA         = 12;
  OLD_ITEM_WEAPON_BFG            = 13;
  OLD_ITEM_WEAPON_SUPERPULEMET   = 14;
  OLD_ITEM_AMMO_BULLETS          = 15;
  OLD_ITEM_AMMO_BULLETS_BOX      = 16;
  OLD_ITEM_AMMO_SHELLS           = 17;
  OLD_ITEM_AMMO_SHELLS_BOX       = 18;
  OLD_ITEM_AMMO_ROCKET           = 19;
  OLD_ITEM_AMMO_ROCKET_BOX       = 20;
  OLD_ITEM_AMMO_CELL             = 21;
  OLD_ITEM_AMMO_CELL_BIG         = 22;
  OLD_ITEM_AMMO_BACKPACK         = 23;
  OLD_ITEM_KEY_RED               = 24;
  OLD_ITEM_KEY_GREEN             = 25;
  OLD_ITEM_KEY_BLUE              = 26;
  OLD_ITEM_SUIT                  = 27;
  OLD_ITEM_OXYGEN                = 28;
  OLD_ITEM_MEDKIT_BLACK          = 29;
  OLD_ITEM_INVUL                 = 30;

  ITEMSCONVERT: Array [OLD_ITEM_MEDKIT_SMALL..OLD_ITEM_INVUL] of Integer =
                ((ITEM_MEDKIT_SMALL),
                 (ITEM_MEDKIT_LARGE),
                 (ITEM_ARMOR_GREEN),
                 (ITEM_ARMOR_BLUE),
                 (ITEM_SPHERE_BLUE),
                 (ITEM_SPHERE_WHITE),
                 (ITEM_WEAPON_SAW),
                 (ITEM_WEAPON_SHOTGUN1),
                 (ITEM_WEAPON_SHOTGUN2),
                 (ITEM_WEAPON_CHAINGUN),
                 (ITEM_WEAPON_ROCKETLAUNCHER),
                 (ITEM_WEAPON_PLASMA),
                 (ITEM_WEAPON_BFG),
                 (ITEM_WEAPON_SUPERPULEMET),
                 (ITEM_AMMO_BULLETS),
                 (ITEM_AMMO_BULLETS_BOX),
                 (ITEM_AMMO_SHELLS),
                 (ITEM_AMMO_SHELLS_BOX),
                 (ITEM_AMMO_ROCKET),
                 (ITEM_AMMO_ROCKET_BOX),
                 (ITEM_AMMO_CELL),
                 (ITEM_AMMO_CELL_BIG),
                 (ITEM_AMMO_BACKPACK),
                 (ITEM_KEY_RED),
                 (ITEM_KEY_GREEN),
                 (ITEM_KEY_BLUE),
                 (ITEM_SUIT),
                 (ITEM_OXYGEN),
                 (ITEM_MEDKIT_BLACK),
                 (ITEM_INVUL));

  OldItemSize: Array [ITEM_MEDKIT_SMALL..ITEM_KEY_BLUE] of Array [0..1] of Byte =
    (((14), (15)), // MEDKIT_SMALL
     ((28), (19)), // MEDKIT_LARGE
     ((28), (19)), // MEDKIT_BLACK - not in map generator
     ((30), (16)), // ARMOR_GREEN
     ((30), (16)), // ARMOR_BLUE
     ((24), (24)), // SPHERE_BLUE
     ((24), (24)), // SPHERE_WHITE
     ((24), (46)), // SUIT
     ((14), (27)), // OXYGEN
     ((25), (25)), // INVUL - not in map generator
     ((61), (23)), // WEAPON_SAW
     ((62), (10)), // WEAPON_SHOTGUN1
     ((52), (12)), // WEAPON_SHOTGUN2
     ((53), (15)), // WEAPON_CHAINGUN
     ((61), (15)), // WEAPON_ROCKETLAUNCHER
     ((53), (14)), // WEAPON_PLASMA
     ((61), (34)), // WEAPON_BFG
     ((53), (16)), // WEAPON_SUPERPULEMET
     (( 9), (10)), // AMMO_BULLETS
     ((28), (16)), // AMMO_BULLETS_BOX
     ((15), ( 7)), // AMMO_SHELLS
     ((32), (12)), // AMMO_SHELLS_BOX
     ((12), (26)), // AMMO_ROCKET
     ((54), (21)), // AMMO_ROCKET_BOX
     ((15), (12)), // AMMO_CELL
     ((32), (21)), // AMMO_CELL_BIG
     ((22), (29)), // AMMO_BACKPACK
     ((16), (16)), // KEY_RED
     ((16), (16)), // KEY_GREEN
     ((16), (16))); // KEY_BLUE

  OldAreaSize: Array [0..1] of Byte = ((34), (56));

  SKY_TEXTURE = 'SKY';

procedure LoadSky(Res: String);
var
  fn: String;

begin
  g_DeleteTexture(SKY_TEXTURE);

  if Res = '' then
    Exit;

  g_ProcessResourceStr(Res, @fn, nil, nil);

  if fn = '' then
    begin
      g_ProcessResourceStr(OpenedMap, @fn, nil, nil);
      fn := fn + Res;
    end
  else
    fn := EditorDir + 'wads/' + Res;

  g_CreateTextureWAD(SKY_TEXTURE, fn);
end;

function AddItem(Item: TItem): DWORD;
var
  a: Integer;
  ok: Boolean;

begin
  ok := False;
  a := 0;

  if gItems <> nil then
    for a := 0 to High(gItems) do
      if gItems[a].ItemType = ITEM_NONE then
      begin
        ok := True;
        Break;
      end;

  if not ok then
  begin
    a := Length(gItems);
    SetLength(gItems, a + 128);
  end;

  gItems[a] := Item;

  Result := a;
end;

function AddPanel(Panel: TPanel): DWORD;
var
  a, b: Integer;
  ok: Boolean;

begin
  ok := False;
  a := 0;
  b := -1;

  if gPanels <> nil then
    for a := High(gPanels) downto 0 do
      if gPanels[a].PanelType = PANEL_NONE then
        b := a
      else
        Break;

  if b <> -1 then
  begin
    a := b;
    ok := True;
  end;

  if not ok then
  begin
    a := Length(gPanels);
    SetLength(gPanels, a + 512);
  end;

  gPanels[a] := Panel;

  Result := a;
end;

function AddArea(Area: TArea): DWORD;
var
  a: Integer;
  ok: Boolean;

begin
  ok := False;
  a := 0;

  if gAreas <> nil then
    for a := 0 to High(gAreas) do
      if gAreas[a].AreaType = AREA_NONE then
      begin
        ok := True;
        Break;
      end;

  if not ok then
  begin
    a := Length(gAreas);
    SetLength(gAreas, a + 64);
  end;

  gAreas[a] := Area;

  Result := a;
end;

function AddMonster(Monster: TMonster): DWORD;
var
  a: Integer;
  ok: Boolean;

begin
  ok := False;
  a := 0;

  if gMonsters <> nil then
    for a := 0 to High(gMonsters) do
      if gMonsters[a].MonsterType = MONSTER_NONE then
      begin
        ok := True;
        Break;
      end;

  if not ok then
  begin
    a := Length(gMonsters);
    SetLength(gMonsters, a + 128);
  end;

  gMonsters[a] := Monster;

  Result := a;
end;

function AddTrigger(Trigger: TTrigger): DWORD;
var
  a: Integer;
  ok: Boolean;

begin
  ok := False;
  a := 0;

  if gTriggers <> nil then
    for a := 0 to High(gTriggers) do
      if gTriggers[a].TriggerType = TRIGGER_NONE then
      begin
        ok := True;
        Break;
      end;

  if not ok then
  begin
    a := Length(gTriggers);
    SetLength(gTriggers, a + 64);
  end;

  gTriggers[a] := Trigger;

  Result := a;
end;

procedure RemoveObject(ID: DWORD; ObjectType: Byte);
var
  a: Integer;

begin
  case ObjectType of
    OBJECT_PANEL:
      begin
      // Убираем ссылки триггеров на эту панель:
        if gTriggers <> nil then
          for a := 0 to High(gTriggers) do
            if gTriggers[a].TriggerType <> 0 then
            begin
              if gTriggers[a].TexturePanel = Integer(ID) then
                gTriggers[a].TexturePanel := -1;

              if gTriggers[a].TriggerType in [
                   TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR,
                   TRIGGER_DOOR, TRIGGER_DOOR5, TRIGGER_CLOSETRAP,
                   TRIGGER_TRAP, TRIGGER_LIFTUP, TRIGGER_LIFTDOWN,
                   TRIGGER_LIFT] then
                if gTriggers[a].Data.PanelID = Integer(ID) then
                  gTriggers[a].Data.PanelID := -1;

              if (gTriggers[a].TriggerType = TRIGGER_SHOT) and
                 (gTriggers[a].Data.ShotPanelID = Integer(ID)) then
                gTriggers[a].Data.ShotPanelID := -1;
            end;

        gPanels[ID].PanelType := 0;
      end;

    OBJECT_ITEM:
      gItems[ID].ItemType := 0;

    OBJECT_MONSTER:
      begin
      // Убираем ссылки триггеров на этого монстра:
        if gTriggers <> nil then
          for a := 0 to High(gTriggers) do
            if gTriggers[a].TriggerType <> 0 then
            begin
              if gTriggers[a].TriggerType in [TRIGGER_PRESS,
                   TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF] then
                if (gTriggers[a].Data.MonsterID-1 = Integer(ID)) then
                  gTriggers[a].Data.MonsterID := 0;
            end;

         gMonsters[ID].MonsterType := 0;
       end;

    OBJECT_AREA:
      gAreas[ID].AreaType := 0;

    OBJECT_TRIGGER:
      gTriggers[ID].TriggerType := 0;
  end;
end;

function PanelInShownLayer(PanelType: Word): Boolean;
begin
  case PanelType of
    PANEL_WALL:
      Result := LayerEnabled[LAYER_WALLS];

    PANEL_BACK:
      Result := LayerEnabled[LAYER_BACK];

    PANEL_FORE:
      Result := LayerEnabled[LAYER_FOREGROUND];

    PANEL_STEP:
      Result := LayerEnabled[LAYER_STEPS];

    PANEL_WATER, PANEL_ACID1, PANEL_ACID2,
    PANEL_LIFTUP, PANEL_LIFTDOWN, PANEL_LIFTLEFT, PANEL_LIFTRIGHT,
    PANEL_OPENDOOR, PANEL_CLOSEDOOR,
    PANEL_BLOCKMON:
      Result := LayerEnabled[LAYER_WATER];

    else
      Result := False;
  end;
end;

function ObjectInRect(fX, fY: Integer; fWidth, fHeight: Word; ObjectType: Byte; All: Boolean): DWArray;
var
  a: Integer;
  
begin
  Result := nil;

  case ObjectType of
    OBJECT_PANEL:
      if gPanels <> nil then
        for a := High(gPanels) downto 0 do
          with gPanels[a] do
          begin
            if not PanelInShownLayer(gPanels[a].PanelType) then
              Continue;

            if g_Collide(X, Y, Width, Height,
                         fX, fY, fWidth, fHeight) then
            begin
              SetLength(Result, Length(Result)+1);
              Result[High(Result)] := a;

              if not All then
                Break;
            end;
          end;

    OBJECT_ITEM:
      if LayerEnabled[LAYER_ITEMS] and (gItems <> nil) then
        for a := High(gItems) downto 0 do
          with gItems[a] do
            if (ItemType <> ITEM_NONE) and
               g_Collide(X, Y, ItemSize[ItemType][0], ItemSize[ItemType][1],
                         fX, fY, fWidth, fHeight) then
            begin
              SetLength(Result, Length(Result)+1);
              Result[High(Result)] := a;

              if not All then
                Break;
            end;

    OBJECT_MONSTER:
      if LayerEnabled[LAYER_MONSTERS] and (gMonsters <> nil) then
        for a := High(gMonsters) downto 0 do
          if gMonsters[a].MonsterType <> MONSTER_NONE then
            with MonsterSize[gMonsters[a].MonsterType] do
              if (gMonsters[a].MonsterType <> 0) and
                 g_Collide(gMonsters[a].X, gMonsters[a].Y, Width, Height,
                           fX, fY, fWidth, fHeight) then
              begin
                SetLength(Result, Length(Result)+1);
                Result[High(Result)] := a;

                if not All then
                  Break;
              end;

    OBJECT_AREA:
      if LayerEnabled[LAYER_AREAS] and (gAreas <> nil) then
        for a := High(gAreas) downto 0 do
          if gAreas[a].AreaType <> AREA_NONE then
            with AreaSize[gAreas[a].AreaType] do
              if (gAreas[a].AreaType <> 0) and
                 g_Collide(gAreas[a].X, gAreas[a].Y, Width, Height,
                           fX, fY, fWidth, fHeight) then
              begin
                SetLength(Result, Length(Result)+1);
                Result[High(Result)] := a;

                if not All then
                  Break;
              end;

    OBJECT_TRIGGER:
      if LayerEnabled[LAYER_TRIGGERS] and (gTriggers <> nil) then
        for a := High(gTriggers) downto 0 do
          if gTriggers[a].TriggerType <> TRIGGER_NONE then
            with gTriggers[a] do
            begin
              if (TriggerType <> 0) and
                 g_Collide(X, Y, Width, Height,
                           fX, fY, fWidth, fHeight) then
              begin
                SetLength(Result, Length(Result)+1);
                Result[High(Result)] := a;

                if not All then
                  Break;
              end;
            end;
  end;
end;

function ObjectCollideLevel(fID: DWORD; ObjectType: Byte; dX, dY: Integer): Boolean;
var
  PanelID: DWORD;

begin
  Result := False;
  PanelID := DWORD(-1);

  case ObjectType of
    OBJECT_PANEL:
      with gPanels[fID] do
        Result := (PanelID <> fID) and
                  g_CollideLevel2(X+dX, Y+dY,
                                  Width, Height,
                                  fID, PanelID);

    OBJECT_ITEM:
      with gItems[fID] do
        Result := g_CollideLevel(X+dX, Y+dY,
                                 ItemSize[ItemType][0], ItemSize[ItemType][1]);

    OBJECT_MONSTER:
      with MonsterSize[gMonsters[fID].MonsterType]  do
        Result := g_CollideLevel(gMonsters[fID].X+dX, gMonsters[fID].Y+dY,
                                 Width, Height);

    OBJECT_AREA:
      with AreaSize[gAreas[fID].AreaType]  do
        Result := g_CollideLevel(gAreas[fID].X+dX, gAreas[fID].Y+dY,
                                 Width, Height);

    OBJECT_TRIGGER:
      with gTriggers[fID] do
        Result := g_CollideLevel(X+dX, Y+dY,
                                 Width, Height);
  end;
end;

function ObjectCollide(ObjectType: Byte; ID: DWORD; fX, fY: Integer; fWidth, fHeight: Word): Boolean;
begin
  Result := False;

  case ObjectType of
    OBJECT_PANEL:
      with gPanels[ID] do
        Result := g_Collide(X, Y, Width, Height,
                            fX, fY, fWidth, fHeight);

    OBJECT_ITEM:
      with gItems[ID] do
        Result := g_Collide(X, Y, ItemSize[ItemType][0], ItemSize[ItemType][1],
                            fX, fY, fWidth, fHeight);

    OBJECT_MONSTER:
      with MonsterSize[gMonsters[ID].MonsterType] do
        Result := g_Collide(gMonsters[ID].X, gMonsters[ID].Y, Width, Height,
                            fX, fY, fWidth, fHeight);

    OBJECT_AREA:
      with AreaSize[gAreas[ID].AreaType] do
        Result := g_Collide(gAreas[ID].X, gAreas[ID].Y, Width, Height,
                            fX, fY, fWidth, fHeight);

    OBJECT_TRIGGER:
      with gTriggers[ID] do
        Result := g_Collide(X, Y, Width, Height,
                            fX, fY, fWidth, fHeight);
  end;
end;

function ObjectGetRect(ObjectType: Byte; ID: DWORD): TRectWH;
begin
  case ObjectType of
    OBJECT_PANEL:
      with gPanels[ID] do
      begin
        Result.X := X;
        Result.Y := Y;
        Result.Width := Width;
        Result.Height := Height;
      end;

    OBJECT_ITEM:
      with gItems[ID] do
      begin
        Result.X := X;
        Result.Y := Y;
        Result.Width := ItemSize[ItemType][0];
        Result.Height := ItemSize[ItemType][1];
       end;

    OBJECT_MONSTER:
      with MonsterSize[gMonsters[ID].MonsterType] do
      begin
        Result.X := gMonsters[ID].X;
        Result.Y := gMonsters[ID].Y;
        Result.Width := Width;
        Result.Height := Height;
      end;

    OBJECT_AREA:
      with AreaSize[gAreas[ID].AreaType] do
      begin
        Result.X := gAreas[ID].X;
        Result.Y := gAreas[ID].Y;
        Result.Width := Width;
        Result.Height := Height;
      end;

    OBJECT_TRIGGER:
      with gTriggers[ID] do
      begin
        Result.X := X;
        Result.Y := Y;
        Result.Width := Width;
        Result.Height := Height;
      end;

    else
      begin
        Result.X := 0;
        Result.Y := 0;
        Result.Width := 0;
        Result.Height := 0;
      end;
  end;
end;

procedure MoveObject(ObjectType: Byte; ID: DWORD; dX, dY: Integer);
begin
  case ObjectType of
    OBJECT_PANEL:
      with gPanels[ID] do
      begin
        X := X + dX;
        Y := Y + dY;
      end;

    OBJECT_ITEM:
      with gItems[ID] do
      begin
        X := X + dX;
        Y := Y + dY;
      end;

    OBJECT_MONSTER:
      with gMonsters[ID] do
      begin
        X := X + dX;
        Y := Y + dY;
      end;

    OBJECT_AREA:
      with gAreas[ID] do
      begin
        X := X + dX;
        Y := Y + dY;
      end;

    OBJECT_TRIGGER:
      with gTriggers[ID] do
      begin
        X := X + dX;
        Y := Y + dY;
      end;
  end;
end;

procedure ResizeObject(ObjectType: Byte; ID: DWORD; dWidth, dHeight: Integer; ResizeDir: Byte);
begin
  case ObjectType of
    OBJECT_PANEL:
      with gPanels[ID] do
      begin
        if Width >= -dWidth then
          Width := Width+dWidth
        else
          begin
            Width := 0;
            Exit;
          end;
   
        if Height >= -dHeight then
          Height := Height+dHeight
        else
          begin
            Height := 0;
            Exit;
          end;

        case ResizeDir of
          2: Y := Y - dHeight;
          4: X := X - dWidth;
        end;
      end;

    OBJECT_ITEM, OBJECT_MONSTER, OBJECT_AREA:
      Exit;

    OBJECT_TRIGGER:
      with gTriggers[ID] do
      begin
        if Width >= -dWidth then
          Width := Width+dWidth
        else
          begin
            Width := 0;
            Exit;
          end;

        if Height >= -dHeight then
          Height := Height+dHeight
        else
          begin
            Height := 0;
            Exit;
          end;

        case ResizeDir of
          2: Y := Y - dHeight;
          4: X := X - dWidth;
        end;
      end;
  end;
end;

function ObjectSelected(ObjectType: Byte; ID: DWORD): Boolean;
var
  a: Integer;

begin
  Result := False;

  if SelectedObjects <> nil then
    for a := 0 to High(SelectedObjects) do
      if SelectedObjects[a].Live and
         (SelectedObjects[a].ObjectType = ObjectType) and
         (SelectedObjects[a].ID = ID) then
       begin
         Result := True;
         Break;
       end;
end;

function GetPanelName(PanelType: Word): String;
begin
  Result := '';

  case PanelType of
    PANEL_WALL: Result := PANELNAMES[0];
    PANEL_BACK: Result := PANELNAMES[1];
    PANEL_FORE: Result := PANELNAMES[2];
    PANEL_OPENDOOR: Result := PANELNAMES[3];
    PANEL_CLOSEDOOR: Result := PANELNAMES[4];
    PANEL_STEP: Result := PANELNAMES[5];
    PANEL_WATER: Result := PANELNAMES[6];
    PANEL_ACID1: Result := PANELNAMES[7];
    PANEL_ACID2: Result := PANELNAMES[8];
    PANEL_LIFTUP: Result := PANELNAMES[9];
    PANEL_LIFTDOWN: Result := PANELNAMES[10];
    PANEL_LIFTLEFT: Result := PANELNAMES[11];
    PANEL_LIFTRIGHT: Result := PANELNAMES[12];
    PANEL_BLOCKMON: Result := PANELNAMES[13];
    else Assert(False);
  end;
end;

function GetPanelType(PanelName: String): Word;
begin
  Result := 0;

  if PanelName = PANELNAMES[0] then
    Result := PANEL_WALL
  else if PanelName = PANELNAMES[1] then
    Result := PANEL_BACK
  else if PanelName = PANELNAMES[2] then
    Result := PANEL_FORE
  else if PanelName = PANELNAMES[3] then
    Result := PANEL_OPENDOOR
  else if PanelName = PANELNAMES[4] then
    Result := PANEL_CLOSEDOOR
  else if PanelName = PANELNAMES[5] then
    Result := PANEL_STEP
  else if PanelName = PANELNAMES[6] then
    Result := PANEL_WATER
  else if PanelName = PANELNAMES[7] then
    Result := PANEL_ACID1
  else if PanelName = PANELNAMES[8] then
    Result := PANEL_ACID2
  else if PanelName = PANELNAMES[9] then
    Result := PANEL_LIFTUP
  else if PanelName = PANELNAMES[10] then
    Result := PANEL_LIFTDOWN
  else if PanelName = PANELNAMES[11] then
    Result := PANEL_LIFTLEFT
  else if PanelName = PANELNAMES[12] then
    Result := PANEL_LIFTRIGHT
  else if PanelName = PANELNAMES[13] then
    Result := PANEL_BLOCKMON;

  Assert(Result <> 0);
end;

function GetTriggerName(TriggerType: Byte): String;
begin
  if TriggerType in [TRIGGER_EXIT..TRIGGER_MAX] then
    Result := TriggerNames[TriggerType]
  else
    Assert(False);
end;

function GetTriggerType(TriggerName: String): Byte;
var
  i: Integer;

begin
  Result := TRIGGER_NONE;
  for i := TRIGGER_EXIT to TRIGGER_MAX do
    if TriggerNames[i] = TriggerName then
      begin
        Result := i;
        Exit;
      end;

  Assert(False);
end;

function IsSpecialTexture(TextureName: string): Boolean;
begin
  Result := (TextureName = TEXTURE_NAME_WATER) or
            (TextureName = TEXTURE_NAME_ACID1) or
            (TextureName = TEXTURE_NAME_ACID2);
end;

function SpecialTextureID(TextureName: string): DWORD;
begin
  Assert(IsSpecialTexture(TextureName));

  Result := 0;

  if TextureName = TEXTURE_NAME_WATER then
    Result := TEXTURE_SPECIAL_WATER
  else
    if TextureName = TEXTURE_NAME_ACID1 then
      Result := TEXTURE_SPECIAL_ACID1
    else
      if TextureName = TEXTURE_NAME_ACID2 then
        Result := TEXTURE_SPECIAL_ACID2;
end;

function SaveMap(Res: String): Pointer;
var
  WAD: TWADEditor_1;
  MapWriter: TMapWriter_1;
  textures: TTexturesRec1Array;
  panels: TPanelsRec1Array;
  items: TItemsRec1Array;
  areas: TAreasRec1Array;
  monsters: TMonsterRec1Array;
  triggers: TTriggersRec1Array;
  header: TMapHeaderRec_1;
  a, b, c: Integer;
  s: String;
  PanelTable: Array of Array [0..1] of Word;
  MonsterTable: Array of Array [0..1] of Word;
  Data: Pointer;
  FileName, SectionName, ResName: String;
  Len: LongWord;

begin
  WAD := nil;
  textures := nil;
  panels := nil;
  items := nil;
  areas := nil;
  monsters := nil;
  triggers := nil;
  PanelTable := nil;
  MonsterTable := nil;
  Data := nil;
  Len := 0;

// Открываем WAD, если надо:
  if Res <> '' then
  begin
    WAD := TWADEditor_1.Create();
    g_ProcessResourceStr(Res, FileName, SectionName, ResName);
    if not WAD.ReadFile(FileName) then
      WAD.FreeWAD();

    WAD.CreateImage();
  end;

  MapWriter := TMapWriter_1.Create();

// Сохраняем заголовок:
  with header do
  begin
    ZeroMemory(@header, SizeOf(TMapHeaderRec_1));

    s := utf2win(gMapInfo.Name);
    if s <> '' then
      CopyMemory(@MapName[0], @s[1], Min(32, Length(s)));

    s := utf2win(gMapInfo.Description);
    if s <> '' then
      CopyMemory(@MapDescription[0], @s[1], Min(256, Length(s)));

    s := utf2win(gMapInfo.Author);
    if s <> '' then
      CopyMemory(@MapAuthor[0], @s[1], Min(32, Length(s)));

    s := utf2win(gMapInfo.MusicName);
    if s <> '' then
      CopyMemory(@MusicName[0], @s[1], Min(64, Length(s)));

    s := utf2win(gMapInfo.SkyName);
    if s <> '' then
      CopyMemory(@SkyName[0], @s[1], Min(64, Length(s)));

    Width := gMapInfo.Width;
    Height := gMapInfo.Height;
  end;

  MapWriter.AddHeader(header);

// Сохраняем названия текстур:
  if MainForm.lbTextureList.Items.Count > 0 then
  begin
    for a := 0 to MainForm.lbTextureList.Items.Count-1 do
    begin
      SetLength(textures, Length(textures)+1);
      s := utf2win(MainForm.lbTextureList.Items[a]);
      CopyMemory(@textures[High(textures)].Resource[0], @s[1], Min(64, Length(s)));
      if g_GetTextureFlagByName(MainForm.lbTextureList.Items[a]) = 1 then
        textures[High(textures)].Anim := 1
      else
        textures[High(textures)].Anim := 0;
    end;

    MapWriter.AddTextures(textures);
  end;

// Сохраняем панели:
  if gPanels <> nil then
  begin
    c := 0;
  
    for a := 0 to High(gPanels) do
      if gPanels[a].PanelType <> 0 then
      begin
        SetLength(PanelTable, Length(PanelTable)+1);
        PanelTable[High(PanelTable)][0] := a;
        PanelTable[High(PanelTable)][1] := c;
        c := c + 1;

        SetLength(panels, Length(panels)+1);

        with panels[High(panels)] do
        begin
          X := gPanels[a].X;
          Y := gPanels[a].Y;
          Width := gPanels[a].Width;
          Height := gPanels[a].Height;
          PanelType := gPanels[a].PanelType;
          Alpha := gPanels[a].Alpha;

          TextureNum := 0;
          Flags := 0;

          if WordBool(gPanels[a].PanelType and
                      (PANEL_WATER or PANEL_ACID1 or PANEL_ACID2)) then
            Flags := PANEL_FLAG_WATERTEXTURES;

        // Может быть текстура:
          if not WordBool(gPanels[a].PanelType and
                          (PANEL_LIFTUP or PANEL_LIFTDOWN or PANEL_LIFTLEFT or PANEL_LIFTRIGHT or PANEL_BLOCKMON)) then
          begin
            if gPanels[a].Blending then
              Flags := Flags or PANEL_FLAG_BLENDING;
            if gPanels[a].TextureID = TEXTURE_SPECIAL_NONE then
              Flags := Flags or PANEL_FLAG_HIDE;

          // Номер текстуры в списке текстур:
            if gPanels[a].TextureID <> TEXTURE_SPECIAL_NONE then
              for b := 0 to High(textures) do
                if utf2win(gPanels[a].TextureName) = textures[b].Resource then
                begin
                  TextureNum := b;
                  Break;
                end;
          end;
        end;
      end;

    MapWriter.AddPanels(panels);
  end;

// Сохраняем предметы:
  if gItems <> nil then
  begin
    for a := 0 to High(gItems) do
      if gItems[a].ItemType <> 0 then
      begin
        SetLength(items, Length(items)+1);

        with items[High(items)] do
        begin
          X := gItems[a].X;
          Y := gItems[a].Y;
          ItemType := gItems[a].ItemType;
          if gItems[a].OnlyDM then
            Options := Options or ITEM_OPTION_ONLYDM;
          if gItems[a].Fall then
            Options := Options or ITEM_OPTION_FALL;
        end;
      end;

    MapWriter.AddItems(items);
  end;

// Сохраняем монстров:
  if gMonsters <> nil then
  begin
    c := 0;

    for a := 0 to High(gMonsters) do
      if gMonsters[a].MonsterType <> 0 then
      begin
        SetLength(MonsterTable, Length(MonsterTable)+1);
        MonsterTable[High(MonsterTable)][0] := a;
        MonsterTable[High(MonsterTable)][1] := c;
        c := c + 1;

        SetLength(monsters, Length(monsters)+1);

        with monsters[High(monsters)] do
        begin
          X := gMonsters[a].X;
          Y := gMonsters[a].Y;
          MonsterType := gMonsters[a].MonsterType;
          Direction := Ord(gMonsters[a].Direction);
        end;
      end;

    MapWriter.AddMonsters(monsters);
  end;

// Сохраняем области:
  if gAreas <> nil then
  begin
    for a := 0 to High(gAreas) do
      if gAreas[a].AreaType <> 0 then
      begin
        SetLength(areas, Length(areas)+1);

        with areas[High(areas)] do
        begin
          X := gAreas[a].X;
          Y := gAreas[a].Y;
          AreaType := gAreas[a].AreaType;
          Direction := Ord(gAreas[a].Direction);
        end;
      end;

    MapWriter.AddAreas(areas);
  end;

// Сохраняем триггеры:
  if gTriggers <> nil then
  begin
    for a := 0 to High(gTriggers) do
      if gTriggers[a].TriggerType <> 0 then
      begin
        SetLength(triggers, Length(triggers)+1);

        with triggers[High(triggers)] do
        begin
          X := gTriggers[a].X;
          Y := gTriggers[a].Y;
          Width := gTriggers[a].Width;
          Height := gTriggers[a].Height;
          Enabled := Byte(gTriggers[a].Enabled);
          TexturePanel := -1;
          TriggerType := gTriggers[a].TriggerType;
          ActivateType := gTriggers[a].ActivateType;
          Keys := gTriggers[a].Key;
          DATA := gTriggers[a].Data.Default;

          if PanelTable <> nil then
          begin
          // Ищем номер панели, которой этот триггер меняет текстуру:
            if gTriggers[a].TexturePanel <> -1 then
            begin
              for c := 0 to High(PanelTable) do
                if PanelTable[c][0] = gTriggers[a].TexturePanel then
                  TexturePanel := PanelTable[c][1];
            end;

          // Ищем номер панели, которую меняет этот триггер:
            if gTriggers[a].TriggerType in [
                 TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR,
                 TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP,
                 TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT] then
              if TTriggerData(DATA).PanelID <> -1 then
                for c := 0 to High(PanelTable) do
                  if PanelTable[c][0] = TTriggerData(DATA).PanelID then
                  begin
                    TTriggerData(DATA).PanelID := PanelTable[c][1];
                    Break;
                  end;

          // Ищем номер панели индикации, которой этот триггер меняет текстуру:
            if (gTriggers[a].TriggerType = TRIGGER_SHOT) and
               (TTriggerData(DATA).ShotPanelID <> -1) then
            begin
              for c := 0 to High(PanelTable) do
                if PanelTable[c][0] = TTriggerData(DATA).ShotPanelID then
                  TTriggerData(DATA).ShotPanelID := PanelTable[c][1];
            end;
          end;

          if MonsterTable <> nil then
          begin
          // Ищем номер монстра - условие активации триггера:
            if gTriggers[a].TriggerType in [
                 TRIGGER_PRESS, TRIGGER_ON,
                 TRIGGER_OFF, TRIGGER_ONOFF] then
              if TTriggerData(DATA).MonsterID <> 0 then
                for c := 0 to High(MonsterTable) do
                  if MonsterTable[c][0] = TTriggerData(DATA).MonsterID-1 then
                  begin
                    TTriggerData(DATA).MonsterID := MonsterTable[c][1]+1;
                    Break;
                  end;
          end;
        end;
      end;

    MapWriter.AddTriggers(triggers);
  end;

// Сохраняем карту в память:
  Len := MapWriter.SaveMap(Data);

  MapWriter.Free();

// Записываем в WAD, если надо:
  if Res <> '' then
    begin
      s := utf2win(ResName);
      WAD.RemoveResource('', s);
      WAD.AddResource(Data, Len, s, '');
      WAD.SaveTo(FileName);

      FreeMem(Data);
      WAD.Free();

      Result := nil;
    end
  else
    Result := Data;
end;

procedure AddTexture(res: String; Error: Boolean);
var
  a: Integer;
begin
  with MainForm.lbTextureList do
  begin
    for a := 0 to Count-1 do
      if Items[a] = res then
        Exit;

    if Error and (slInvalidTextures.IndexOf(res) = -1) then
      slInvalidTextures.Add(res);
    Items.Add(res);
  end;
end;

function LoadMap(Res: String): Boolean;
var
  WAD: TWADEditor_1;
  MapReader: TMapReader_1;
  Header: TMapHeaderRec_1;
  textures: TTexturesRec1Array;
  panels: TPanelsRec1Array;
  items: TItemsRec1Array;
  monsters: TMonsterRec1Array;
  areas: TAreasRec1Array;
  triggers: TTriggersRec1Array;
  panel: TPanel;
  item: TItem;
  monster: TMonster;
  area: TArea;
  trigger: TTrigger;
  a: Integer;
  Data: Pointer;
  Width, Height, m: Word;
  FileName, SectionName, ResName, _fn: String;
  TextureRes, ustr: String;
  pData: Pointer;
  Len, FrameLen: Integer;
  Error: Boolean;
  NoTextureID: DWORD;
  NW, NH: Word;
begin
  Result := False;
  pData := nil;
  Len := 0;
  Data := nil;
  FrameLen := 0;
  Width := 0;
  Height := 0;
  NoTextureID := 0;
  NW := 0;
  NH := 0;

  MainForm.pbLoad.Position := 0;
  MainForm.lLoad.Caption := _lc[I_LOAD_WAD];
  Application.ProcessMessages();

// Открываем WAD:
  WAD := TWADEditor_1.Create();
  g_ProcessResourceStr(Res, FileName, SectionName, ResName);

  if not WAD.ReadFile(FileName) then
  begin
    WAD.Free();
    Exit;
  end;

// Читаем ресурс карты:
  if not WAD.GetResource('', utf2win(ResName), pData, Len) then
  begin
    WAD.Free();
    Exit;
  end;

  WAD.Free();

  MapReader := TMapReader_1.Create();

  MainForm.lLoad.Caption := _lc[I_LOAD_MAP];
  Application.ProcessMessages();

  MapReader.LoadMap(pData);

// Загружаем текстуры:
  textures := MapReader.GetTextures();

  if textures <> nil then
  begin
    MainForm.pbLoad.Position := 0;
    MainForm.pbLoad.Max := Length(textures);
    MainForm.lLoad.Caption := _lc[I_LOAD_TEXTURES];

    for a := 0 to High(textures) do
    begin
      MainForm.pbLoad.StepIt();
      Application.ProcessMessages();
      ustr := win2utf(textures[a].Resource);

      if IsSpecialTexture(ustr) then
      begin
        AddTexture(ustr, False);
        Continue;
      end;

      g_ProcessResourceStr(ustr, @_fn, nil, nil);

      if _fn = '' then
        TextureRes := FileName + ustr
      else
        TextureRes := EditorDir+'wads/'+ustr;

      Error := False;

      if not ByteBool(textures[a].Anim) then
        begin // Обычная текстура
          if not g_CreateTextureWAD(ustr, TextureRes) then
          begin
            e_WriteLog(Format('g_CreateTextureWAD() error, res=%s',
                              [ustr]), MSG_WARNING);
            Error := True;
          end;

          AddTexture(ustr, Error);
        end
      else // Anim
        begin // Анимированная текстура
          if not GetFrame(TextureRes, Data, FrameLen, Width, Height) then
          begin // Кадры
            e_WriteLog(Format('GetFrame() error, res=%s',
                              [ustr]), MSG_WARNING);
            Error := True;
          end;

          if not g_CreateTextureMemorySize(Data, FrameLen, ustr, 0, 0, Width, Height, 1) then
          begin // Сама текстура
            e_WriteLog(Format('g_CreateTextureMemorySize() error, res=%s',
                              [ustr]), MSG_WARNING);
            Error := True;
          end;

          AddTexture(ustr, Error);
        end;
    end;
  end;

// Загружаем панели:
  panels := MapReader.GetPanels();

  if panels <> nil then
  begin
    MainForm.pbLoad.Position := 0;
    MainForm.pbLoad.Max := Length(panels);
    MainForm.lLoad.Caption := _lc[I_LOAD_PANELS];

    for a := 0 to High(panels) do
    begin
      MainForm.pbLoad.StepIt();
      Application.ProcessMessages();

      panel.X := panels[a].X;
      panel.Y := panels[a].Y;
      panel.Height := panels[a].Height;
      panel.Width := panels[a].Width;
      panel.PanelType := panels[a].PanelType;
      panel.Alpha := panels[a].Alpha;

      panel.Blending := ByteBool(panels[a].Flags and PANEL_FLAG_BLENDING);

      panel.TextureWidth := 0;
      panel.TextureHeight := 0;
      panel.TextureName := '';
      panel.TextureID := TEXTURE_SPECIAL_NONE;

      m := PANEL_WALL or PANEL_BACK or PANEL_FORE or PANEL_STEP or
           PANEL_OPENDOOR or PANEL_CLOSEDOOR;

      if ByteBool(panels[a].Flags and PANEL_FLAG_WATERTEXTURES) then
        m := m or PANEL_WATER or PANEL_ACID1 or PANEL_ACID2;

    // Если панель видима:
      if WordBool(panel.PanelType and m) and
         (not (ByteBool(panels[a].Flags and PANEL_FLAG_HIDE))) then
      begin
        ustr := win2utf(textures[panels[a].TextureNum].Resource);
        if not IsSpecialTexture(ustr) then
          begin // Текстура
            if g_GetTexture(ustr, panel.TextureID) then
              g_GetTextureSizeByID(panel.TextureID, panel.TextureWidth, panel.TextureHeight)
            else begin
              panel.TextureWidth := 1;
              panel.TextureHeight := 1;
              if g_GetTexture('NOTEXTURE', NoTextureID) then
              begin
                panel.TextureID := TEXTURE_SPECIAL_NOTEXTURE;
                g_GetTextureSizeByID(NoTextureID, NW, NH);
                panel.TextureWidth := NW;
                panel.TextureHeight := NH;
              end;
            end;
          end
        else // Спец. текстура
          panel.TextureID := SpecialTextureID(ustr);

        panel.TextureName := ustr;
      end;

    // Жидкость без текстуры:
      if WordBool(panel.PanelType and (PANEL_WATER or PANEL_ACID1 or PANEL_ACID2)) and
         (not ByteBool(panels[a].Flags and PANEL_FLAG_WATERTEXTURES)) then
      begin
        case panel.PanelType of
          PANEL_WATER: panel.TextureName := TEXTURE_NAME_WATER;
          PANEL_ACID1: panel.TextureName := TEXTURE_NAME_ACID1;
          PANEL_ACID2: panel.TextureName := TEXTURE_NAME_ACID2;
        end;

        panel.TextureID := SpecialTextureID(panel.TextureName);
        AddTexture(panel.TextureName, False);
      end;

      AddPanel(panel);
    end;
  end;

// Загружаем предметы:
  items := MapReader.GetItems();

  if items <> nil then
  begin
    MainForm.pbLoad.Position := 0;
    MainForm.pbLoad.Max := Length(items);
    MainForm.lLoad.Caption := _lc[I_LOAD_ITEMS];

    for a := 0 to High(items) do
    begin
      MainForm.pbLoad.StepIt();
      Application.ProcessMessages();

      item.X := items[a].X;
      item.Y := items[a].Y;
      item.ItemType := items[a].ItemType;
      item.OnlyDM := ByteBool(items[a].Options and ITEM_OPTION_ONLYDM);
      item.Fall := ByteBool(items[a].Options and ITEM_OPTION_FALL);

      AddItem(item);
    end;
  end;

// Загружаем монстров:
  monsters := MapReader.GetMonsters();

  if monsters <> nil then
  begin
    MainForm.pbLoad.Position := 0;
    MainForm.pbLoad.Max := Length(monsters);
    MainForm.lLoad.Caption := _lc[I_LOAD_MONSTERS];

    for a := 0 to High(monsters) do
    begin
      MainForm.pbLoad.StepIt();
      Application.ProcessMessages();

      monster.X := monsters[a].X;
      monster.Y := monsters[a].Y;
      monster.MonsterType := monsters[a].MonsterType;
      monster.Direction := TDirection(monsters[a].Direction);

      AddMonster(monster);
    end;
  end;

// Загружаем области:
  areas := MapReader.GetAreas();

  if areas <> nil then
  begin
    MainForm.pbLoad.Position := 0;
    MainForm.pbLoad.Max := Length(areas);
    MainForm.lLoad.Caption := _lc[I_LOAD_AREAS];

    for a := 0 to High(areas) do
    begin
      MainForm.pbLoad.StepIt();
      Application.ProcessMessages();

      area.X := areas[a].X;
      area.Y := areas[a].Y;
      area.AreaType := areas[a].AreaType;
      area.Direction := TDirection(areas[a].Direction);

      AddArea(area);
    end;
  end;

// Загружаем триггеры:
  triggers := MapReader.GetTriggers();

  if triggers <> nil then
  begin
    MainForm.pbLoad.Position := 0;
    MainForm.pbLoad.Max := Length(triggers);
    MainForm.lLoad.Caption := _lc[I_LOAD_TRIGGERS];

    for a := 0 to High(triggers) do
    begin
      MainForm.pbLoad.StepIt();
      Application.ProcessMessages();

      trigger.X := triggers[a].X;
      trigger.Y := triggers[a].Y;
      trigger.Width := triggers[a].Width;
      trigger.Height := triggers[a].Height;

      trigger.Enabled := ByteBool(triggers[a].Enabled);
      trigger.TexturePanel := triggers[a].TexturePanel;
      trigger.TriggerType := triggers[a].TriggerType;
      trigger.ActivateType := triggers[a].ActivateType;
      trigger.Key := triggers[a].Keys;
      trigger.Data.Default := triggers[a].DATA;

      AddTrigger(trigger);
    end;
  end;

// Загружаем заголовок карты:
  Header := MapReader.GetMapHeader();

  gMapInfo.FileName := FileName;

  with gMapInfo do
  begin
    MapName := ResName;
    Name := win2utf(Header.MapName);
    Description := win2utf(Header.MapDescription);
    Author := win2utf(Header.MapAuthor);
    MusicName := win2utf(Header.MusicName);
    SkyName := win2utf(Header.SkyName);
    Height := Header.Height;
    Width := Header.Width;
  end;

  LoadSky(gMapInfo.SkyName);

  textures := nil;
  panels := nil;
  items := nil;
  areas := nil;
  triggers := nil;

  MapReader.Free();
  FreeMem(pData);

  Result := True;
end;

function LoadMapOld(_FileName: String): Boolean;
const
  TexturePrefix = 'Standart.wad:STDTEXTURES\';
  DefaultMusRes = 'Standart.wad:STDMUS\MUS1';
  DefaultSkyRes = 'Standart.wad:STDSKY\SKY0';

var
  map: TConfig;
  i, a: Integer;
  s, section: String;
  panel: TPanel;
  item: TItem;
  area: TArea;
  
begin
  Result := False;

  if not FileExists(_FileName) then
    Exit;

// Открытие карты:
  MainForm.pbLoad.Position := 0;
  MainForm.lLoad.Caption := _lc[I_LOAD_MAP];;
  Application.ProcessMessages();

  map := TConfig.CreateFile(_FileName);

// Чтение текстур:
  i := map.ReadInt('MapOptions', 'TextureCount', 0);

  MainForm.pbLoad.Max := i;
  MainForm.pbLoad.Position := 0;
  MainForm.lLoad.Caption := _lc[I_LOAD_TEXTURES];

  for a := 1 to i do
  begin
    MainForm.pbLoad.StepIt();
    Application.ProcessMessages();

    s := TexturePrefix + UpperCase(map.ReadStr('Textures', 'TextureName'+IntToStr(a), ''));
    if s = TexturePrefix then
      Continue;

  // Нет такой текстуры - ищем в WAD карты:
    if not g_CreateTextureWAD(s, EditorDir+'wads/'+s) then
    begin
      s := ExtractFileName(_FileName);
      Delete(s, Length(s)-3, 4);
      s := UpperCase(s) + '.WAD:TEXTURES\'+ UpperCase(win2utf(map.ReadStr('Textures', 'TextureName'+IntToStr(a), '')));

      if not g_CreateTextureWAD(s, EditorDir+'wads/'+s) then
        Continue;
    end;

    MainForm.lbTextureList.Items.Add(s);
  end;

// Чтение панелей:
  i := map.ReadInt('MapOptions', 'PanelCount', 0);

  MainForm.pbLoad.Max := i;
  MainForm.pbLoad.Position := 0;
  MainForm.lLoad.Caption := _lc[I_LOAD_PANELS];

  for a := 1 to i do
  begin
    MainForm.pbLoad.StepIt();
    Application.ProcessMessages();

    section := 'Panel' + IntToStr(a);
    if not map.SectionExists(section) then
      Continue;

    panel.X := map.ReadInt(section, 'X1', 0);
    panel.Y := map.ReadInt(section, 'Y1', 0);
    panel.Height := map.ReadInt(section, 'Height', 16);
    panel.Width := map.ReadInt(section, 'Width', 16);

    case map.ReadInt(section, 'PanelType', 0) of
      0: panel.PanelType := PANEL_WALL;
      1: panel.PanelType := PANEL_BACK;
      2: panel.PanelType := PANEL_FORE;
      3: panel.PanelType := PANEL_STEP;
      4: panel.PanelType := PANEL_WATER;
      5: panel.PanelType := PANEL_ACID1;
      6: panel.PanelType := PANEL_ACID2;
    end;

    panel.Alpha := map.ReadInt(section, 'Alpha', 0);

  // Текстура панели:
    if panel.PanelType in [PANEL_WALL, PANEL_BACK, PANEL_FORE, PANEL_STEP] then
      begin
        s := TexturePrefix + UpperCase(map.ReadStr(section, 'TextureName', ''));

        if g_GetTexture(s, panel.TextureID) then
          begin
            g_GetTextureSizeByID(panel.TextureID, panel.TextureWidth, panel.TextureHeight);
            panel.TextureName := s;
          end
        else // Нет такой текстуры - ищем в WAD карты:
          begin
            s := ExtractFileName(_FileName);
            Delete(s, Length(s)-3, 4);
            s := UpperCase(s) + '.WAD:TEXTURES\' + UpperCase(win2utf(map.ReadStr(section, 'TextureName', '')));

            if g_GetTexture(s, panel.TextureID) then
              begin
                g_GetTextureSizeByID(panel.TextureID, panel.TextureWidth, panel.TextureHeight);
                panel.TextureName := s;
              end
            else
              Continue;
          end;
      end
    else if panel.PanelType in [PANEL_WATER, PANEL_ACID1, PANEL_ACID2] then
      begin
        case panel.PanelType of
          PANEL_WATER:
            begin
              s := TEXTURE_NAME_WATER;
              panel.TextureID := TEXTURE_SPECIAL_WATER;
            end;
          PANEL_ACID1:
            begin
              s := TEXTURE_NAME_ACID2;
              panel.TextureID := TEXTURE_SPECIAL_ACID2;
            end;
          PANEL_ACID2: 
            begin
              s := TEXTURE_NAME_ACID1;
              panel.TextureID := TEXTURE_SPECIAL_ACID1;
            end;
        end;

        with MainForm.lbTextureList.Items do
          if IndexOf(s) = -1 then
            Add(s);
        panel.TextureName := s;
        panel.TextureWidth := 1;
        panel.TextureHeight := 1;
      end;

    AddPanel(panel);
  end;

// Чтение предметов:
  i := map.ReadInt('MapOptions', 'ItemCount', 0);

  MainForm.pbLoad.Max := i;
  MainForm.pbLoad.Position := 0;
  MainForm.lLoad.Caption := _lc[I_LOAD_ITEMS];

  for a := 1 to i do
  begin
    MainForm.pbLoad.StepIt();
    Application.ProcessMessages();

    section := 'Item' + IntToStr(a);
    if not map.SectionExists(section) then
      Continue;

    item.X := map.ReadInt(section, 'X', 0);
    item.Y := map.ReadInt(section, 'Y', 0);
    item.ItemType := ITEMSCONVERT[map.ReadInt(section, 'Type', 0)];
    item.OnlyDM := False;
    item.Fall := item.ItemType in [ITEM_KEY_RED, ITEM_KEY_GREEN, ITEM_KEY_BLUE];

  // Размер предметов теперь другой:
    item.X := item.X + OldItemSize[item.ItemType][0] - ItemSize[item.ItemType][0];
    item.Y := item.Y + OldItemSize[item.ItemType][1] - ItemSize[item.ItemType][1];

    AddItem(item);
  end;

// Чтение областей:
  i := map.ReadInt('MapOptions', 'AreaCount', 0);

  MainForm.pbLoad.Max := i;
  MainForm.pbLoad.Position := 0;
  MainForm.lLoad.Caption := _lc[I_LOAD_AREAS];

  for a := 1 to i do
  begin
    MainForm.pbLoad.StepIt();
    Application.ProcessMessages();

    section := 'Area' + IntToStr(a);
    if not map.SectionExists(section) then
      Continue;

    area.X := map.ReadInt(section, 'X', 0);
    area.Y := map.ReadInt(section, 'Y', 0);
    area.AreaType := map.ReadInt(section, 'Type', 0);
    area.Direction := D_RIGHT;

  // Размер областей теперь другой:
    area.X := area.X + OldAreaSize[0] - AreaSize[area.AreaType].Width;
    area.Y := area.Y + OldAreaSize[1] - AreaSize[area.AreaType].Height;

    AddArea(area);
  end;

// Чтение параметров карты:
  with gMapInfo do
  begin
    Name := map.ReadStr('MapOptions', 'MapName', '');
    Description := map.ReadStr('MapOptions', 'MapDescription', '');
    Author := '';
    MusicName := DefaultMusRes;
    SkyName := DefaultSkyRes;
    FileName := _FileName;
    Height := map.ReadInt('MapOptions', 'Height', 1600);
    Width := map.ReadInt('MapOptions', 'Width', 1600);

    if Length(Name) > 32 then
      SetLength(Name, 32);
    if Length(Description) > 256 then
      SetLength(Description, 256);
  end;

  map.Free();

  Result := True;
end;

procedure ClearMap();
var
  a: Integer;

begin
  if gPanels <> nil then
    for a := 0 to High(gPanels) do
      if gPanels[a].TextureName <> '' then
        g_DeleteTexture(gPanels[a].TextureName);

  SetLength(gPanels, 0);
  gPanels := nil;
  SetLength(gItems, 0);
  gItems := nil;
  SetLength(gAreas, 0);
  gAreas := nil;
  SetLength(gMonsters, 0);
  gMonsters := nil;
  SetLength(gTriggers, 0);
  gTriggers := nil;

  with gMapInfo do
  begin
    Name := '';
    Description := '';
    Author := '';
    MusicName := '';
    SkyName := '';
    FileName := '';
    Height := 1600;
    Width := 1600;
  end;

  with MainForm.lbTextureList do
  begin
    if Items.Count > 0 then
      for a := Items.Count-1 downto 0 do
        if not IsSpecialTexture(Items[a]) then
          g_DeleteTexture(Items[a]);

    Clear();
  end;

  MapOffset.X := 0;
  MapOffset.Y := 0;
end;

procedure DrawPanels(fPanelType: Word);

  procedure DrawTexture(a: Integer);
  var
    NoTextureID: DWORD;
    NW, NH: Word;
  begin
    NoTextureID := 0;
    NW := 0;
    NH := 0;
    with gPanels[a] do
    begin
      case TextureID of
        TEXTURE_SPECIAL_NONE:
          if PreviewMode = 0 then
            e_DrawFillQuad(X+MapOffset.X, Y+MapOffset.Y,
                           X+MapOffset.X+Width-1, Y+MapOffset.Y+Height-1,
                           64, 64, 64, 127);

        TEXTURE_SPECIAL_NOTEXTURE:
          if g_GetTexture('NOTEXTURE', NoTextureID) then
          begin
            g_GetTextureSizeByID(NoTextureID, NW, NH);
            e_DrawFill(NoTextureID, X+MapOffset.X, Y+MapOffset.Y,
                       Width div NW, Height div NH, 0, False, False);
          end;

        TEXTURE_SPECIAL_WATER:
          if PreviewMode > 0 then
            e_DrawFillQuad(X+MapOffset.X, Y+MapOffset.Y,
                           X+MapOffset.X+Width-1, Y+MapOffset.Y+Height-1,
                           0, 0, 255, 0, B_FILTER)
          else
            e_DrawFillQuad(X+MapOffset.X, Y+MapOffset.Y,
                           X+MapOffset.X+Width-1, Y+MapOffset.Y+Height-1,
                           0, 0, 255, 127);

        TEXTURE_SPECIAL_ACID1:
          if PreviewMode > 0 then
            e_DrawFillQuad(X+MapOffset.X, Y+MapOffset.Y,
                           X+MapOffset.X+Width-1, Y+MapOffset.Y+Height-1,
                           0, 127, 0, 0, B_FILTER)
          else
            e_DrawFillQuad(X+MapOffset.X, Y+MapOffset.Y,
                           X+MapOffset.X+Width-1, Y+MapOffset.Y+Height-1,
                           0, 255, 0, 127);

        TEXTURE_SPECIAL_ACID2:
          if PreviewMode > 0 then
            e_DrawFillQuad(X+MapOffset.X, Y+MapOffset.Y,
                           X+MapOffset.X+Width-1, Y+MapOffset.Y+Height-1,
                           127, 0, 0, 0, B_FILTER)
          else
            e_DrawFillQuad(X+MapOffset.X, Y+MapOffset.Y,
                           X+MapOffset.X+Width-1, Y+MapOffset.Y+Height-1,
                           255, 0, 0, 127);

        else
          e_DrawFill(TextureID, X+MapOffset.X, Y+MapOffset.Y,
                     Width div TextureWidth, Height div TextureHeight,
                     Alpha, True, Blending);
      end;
    end;
  end;

var
  a: Integer;
begin
  if gPanels <> nil then
    for a := 0 to High(gPanels) do
      if (gPanels[a].PanelType <> 0) and
         WordBool(gPanels[a].PanelType and fPanelType) then
        with gPanels[a] do
        begin
          if (TextureID < TEXTURE_SPECIAL_NOTEXTURE) and (TextureWidth = 0) and (TextureWidth = 0) then
            TextureID := TEXTURE_SPECIAL_NOTEXTURE;
          case PanelType of
            PANEL_WALL, PANEL_BACK, PANEL_FORE,
            PANEL_STEP, PANEL_OPENDOOR, PANEL_CLOSEDOOR,
            PANEL_WATER, PANEL_ACID1, PANEL_ACID2:
              DrawTexture(a);

            PANEL_LIFTUP:
              if PreviewMode = 0 then
                e_DrawFillQuad(X+MapOffset.X, Y+MapOffset.Y,
                               X+MapOffset.X+Width-1, Y+MapOffset.Y+Height-1,
                               128, 64, 0, 0);
            PANEL_LIFTDOWN:
              if PreviewMode = 0 then
                e_DrawFillQuad(X+MapOffset.X, Y+MapOffset.Y,
                               X+MapOffset.X+Width-1, Y+MapOffset.Y+Height-1,
                               90, 154, 138, 0);
            PANEL_LIFTLEFT:
              if PreviewMode = 0 then
                e_DrawFillQuad(X+MapOffset.X, Y+MapOffset.Y,
                               X+MapOffset.X+Width-1, Y+MapOffset.Y+Height-1,
                               200, 80,  4, 0);
            PANEL_LIFTRIGHT:
              if PreviewMode = 0 then
                e_DrawFillQuad(X+MapOffset.X, Y+MapOffset.Y,
                               X+MapOffset.X+Width-1, Y+MapOffset.Y+Height-1,
                               252, 140, 56, 0);
            PANEL_BLOCKMON:
              if PreviewMode = 0 then
                e_DrawFillQuad(X+MapOffset.X, Y+MapOffset.Y,
                               X+MapOffset.X+Width-1, Y+MapOffset.Y+Height-1,
                               192, 0, 192, 0);
          end;
        end;
end;

procedure DrawMap();
var
  a, w, h: Integer;
  ID: DWORD;
  xx, yy, dx, dy: Integer;
  ww, hh: Word;
  sel: Boolean;
  r: TRectWH;

begin
  ID := 0;
// В режиме Превью рисуем небо:
  if PreviewMode > 0 then
  begin
    w := Max(MainForm.RenderPanel.Width, MainForm.RenderPanel.Height);
    if MainForm.RenderPanel.Height > MainForm.RenderPanel.Width*3/4 then
      w := Round(w*4/3);
    h := Round(w*3/4);

    if g_GetTexture(SKY_TEXTURE, ID) then
      e_DrawSize(ID, 0, 0, 0, False, False, w, h)
    else
      e_DrawFillQuad(0, 0, w-1, h-1, 0, 0, 0, 0, B_NONE);
  end;

// Рисуем панели (если Превью или если включен слой):
  if LayerEnabled[LAYER_BACK] or (PreviewMode = 1) then
    DrawPanels(PANEL_BACK);
  if PreviewMode > 0 then
    DrawPanels(PANEL_LIFTUP or PANEL_LIFTDOWN or PANEL_LIFTLEFT or PANEL_LIFTRIGHT)
  else
    if LayerEnabled[LAYER_WATER] then
      DrawPanels(PANEL_LIFTUP or PANEL_LIFTDOWN or PANEL_LIFTLEFT or PANEL_LIFTRIGHT or
                 PANEL_OPENDOOR or PANEL_CLOSEDOOR or PANEL_BLOCKMON);
  if LayerEnabled[LAYER_WALLS] or (PreviewMode = 1) then
    DrawPanels(PANEL_WALL);
  if LayerEnabled[LAYER_STEPS] or (PreviewMode = 1) then
    DrawPanels(PANEL_STEP);

// Рисуем предметы:
  if (LayerEnabled[LAYER_ITEMS] or (PreviewMode = 1)) and
     (gItems <> nil) then
    for a := 0 to High(gItems) do
      if gItems[a].ItemType <> ITEM_NONE then
        with gItems[a] do
        begin
          ID := DWORD(-1);

          case ItemType of
            0: Continue;
            ITEM_MEDKIT_SMALL: g_GetTexture('ITEM_MEDKIT_SMALL', ID);
            ITEM_MEDKIT_LARGE: g_GetTexture('ITEM_MEDKIT_LARGE', ID);
            ITEM_MEDKIT_BLACK: g_GetTexture('ITEM_MEDKIT_BLACK', ID);
            ITEM_ARMOR_GREEN: g_GetTexture('ITEM_ARMORGREEN', ID);
            ITEM_ARMOR_BLUE: g_GetTexture('ITEM_ARMORBLUE', ID);
            ITEM_SPHERE_BLUE: g_GetTexture('ITEM_BLUESPHERE', ID);
            ITEM_SPHERE_WHITE: g_GetTexture('ITEM_WHITESPHERE', ID);
            ITEM_SUIT: g_GetTexture('ITEM_SUIT', ID);
            ITEM_OXYGEN: g_GetTexture('ITEM_OXYGEN', ID);
            ITEM_INVUL: g_GetTexture('ITEM_INVUL', ID);
            ITEM_WEAPON_SAW: g_GetTexture('ITEM_WEAPON_SAW', ID);
            ITEM_WEAPON_SHOTGUN1: g_GetTexture('ITEM_WEAPON_SHOTGUN1', ID);
            ITEM_WEAPON_SHOTGUN2: g_GetTexture('ITEM_WEAPON_SHOTGUN2', ID);
            ITEM_WEAPON_CHAINGUN: g_GetTexture('ITEM_WEAPON_CHAINGUN', ID);
            ITEM_WEAPON_ROCKETLAUNCHER: g_GetTexture('ITEM_WEAPON_ROCKETLAUNCHER', ID);
            ITEM_WEAPON_PLASMA: g_GetTexture('ITEM_WEAPON_PLASMA', ID);
            ITEM_WEAPON_BFG: g_GetTexture('ITEM_WEAPON_BFG', ID);
            ITEM_WEAPON_SUPERPULEMET: g_GetTexture('ITEM_WEAPON_SUPERPULEMET', ID);
            ITEM_AMMO_BULLETS: g_GetTexture('ITEM_AMMO_BULLETS', ID);
            ITEM_AMMO_BULLETS_BOX: g_GetTexture('ITEM_AMMO_BULLETS_BOX', ID);
            ITEM_AMMO_SHELLS: g_GetTexture('ITEM_AMMO_SHELLS', ID);
            ITEM_AMMO_SHELLS_BOX: g_GetTexture('ITEM_AMMO_SHELLS_BOX', ID);
            ITEM_AMMO_ROCKET: g_GetTexture('ITEM_AMMO_ROCKET', ID);
            ITEM_AMMO_ROCKET_BOX: g_GetTexture('ITEM_AMMO_ROCKET_BOX', ID);
            ITEM_AMMO_CELL: g_GetTexture('ITEM_AMMO_CELL', ID);
            ITEM_AMMO_CELL_BIG: g_GetTexture('ITEM_AMMO_CELL_BIG', ID);
            ITEM_AMMO_BACKPACK: g_GetTexture('ITEM_AMMO_BACKPACK', ID);
            ITEM_KEY_RED: g_GetTexture('ITEM_KEY_RED', ID);
            ITEM_KEY_GREEN: g_GetTexture('ITEM_KEY_GREEN', ID);
            ITEM_KEY_BLUE: g_GetTexture('ITEM_KEY_BLUE', ID);
            ITEM_BOTTLE: g_GetTexture('ITEM_BOTTLE', ID);
            ITEM_HELMET: g_GetTexture('ITEM_HELMET', ID);
            ITEM_JETPACK: g_GetTexture('ITEM_JETPACK', ID);
            ITEM_INVIS: g_GetTexture('ITEM_INVIS', ID);
            ITEM_WEAPON_FLAMETHROWER: g_GetTexture('ITEM_WEAPON_FLAMETHROWER', ID);
            ITEM_AMMO_FUELCAN: g_GetTexture('ITEM_AMMO_FUELCAN', ID);
          end;

          if ID <> DWORD(-1) then
            e_Draw(ID, MapOffset.X+X, MapOffset.Y+Y, 0, True, False);
        end;

// Рисуем монстров:
  if (LayerEnabled[LAYER_MONSTERS] or (PreviewMode = 1)) and
     (gMonsters <> nil) then
    for a := 0 to High(gMonsters) do
      if gMonsters[a].MonsterType <> MONSTER_NONE then
        with MonsterSize[gMonsters[a].MonsterType] do
        begin
          ID := DWORD(-1);
          sel := ObjectSelected(OBJECT_MONSTER, a);

          case gMonsters[a].MonsterType of
            0: Continue;
            MONSTER_DEMON: g_GetTexture('MONSTER_DEMON', ID);
            MONSTER_IMP: g_GetTexture('MONSTER_IMP', ID);
            MONSTER_ZOMBY: g_GetTexture('MONSTER_ZOMBY', ID);
            MONSTER_SERG: g_GetTexture('MONSTER_SERG', ID);
            MONSTER_CYBER: g_GetTexture('MONSTER_CYBER', ID);
            MONSTER_CGUN: g_GetTexture('MONSTER_CGUN', ID);
            MONSTER_BARON: g_GetTexture('MONSTER_BARON', ID);
            MONSTER_KNIGHT: g_GetTexture('MONSTER_KNIGHT', ID);
            MONSTER_CACO: g_GetTexture('MONSTER_CACO', ID);
            MONSTER_SOUL: g_GetTexture('MONSTER_SOUL', ID);
            MONSTER_PAIN: g_GetTexture('MONSTER_PAIN', ID);
            MONSTER_SPIDER: g_GetTexture('MONSTER_SPIDER', ID);
            MONSTER_BSP: g_GetTexture('MONSTER_BSP', ID);
            MONSTER_MANCUB: g_GetTexture('MONSTER_MANCUB', ID);
            MONSTER_SKEL: g_GetTexture('MONSTER_SKEL', ID);
            MONSTER_VILE: g_GetTexture('MONSTER_VILE', ID);
            MONSTER_FISH: g_GetTexture('MONSTER_FISH', ID);
            MONSTER_BARREL: g_GetTexture('MONSTER_BARREL', ID);
            MONSTER_ROBO: g_GetTexture('MONSTER_ROBO', ID);
            MONSTER_MAN: g_GetTexture('MONSTER_MAN', ID);
          end;

          if ID <> DWORD(-1) then
            if gMonsters[a].Direction = D_LEFT then
              begin
              // Расстояние от края текстуры до края визуального положения объекта на текстуре:
                xx := (X - MonsterSizeDelta[gMonsters[a].MonsterType].X) + Width;
              // Расстояние от края хит бокса до края визуального положения объекта на текстуре:
                xx := MonsterSizeDelta[gMonsters[a].MonsterType].Width - xx - X;
              // Т.к. двигать текстуру нужно будет в противоположном направлении:
                xx := (gMonsters[a].X - X) - xx;

                yy := (gMonsters[a].Y - Y) +
                      MonsterSizeDelta[gMonsters[a].MonsterType].Y;

                e_Draw(ID, MapOffset.X + xx, MapOffset.Y + yy,
                       0, True, False, M_HORIZONTAL);
              end
            else
              begin
              // Левый верхний угол текстуры + смещение анимации:
                xx := (gMonsters[a].X - X) +
                      MonsterSizeDelta[gMonsters[a].MonsterType].X;
                yy := (gMonsters[a].Y - Y) +
                      MonsterSizeDelta[gMonsters[a].MonsterType].Y;

                e_Draw(ID, MapOffset.X + xx, MapOffset.Y + yy,
                       0, True, False);
              end;

        // Рамка:
          if PreviewMode = 0 then
          begin
            e_DrawQuad(MapOffset.X+gMonsters[a].X, MapOffset.Y+gMonsters[a].Y,
                       MapOffset.X+gMonsters[a].X+Width-1, MapOffset.Y+gMonsters[a].Y+Height-1,
                       255, 255, 255, IfThen(sel, 0, gAlphaMonsterRect));
          end;
        end;

// Рисуем закрытые двери после монстров:
  if ((PreviewMode = 2) and LayerEnabled[LAYER_WATER])
  or (PreviewMode = 1) then
    DrawPanels(PANEL_CLOSEDOOR);

// Рисуем области:
  if (LayerEnabled[LAYER_AREAS] or (PreviewMode = 1)) and
     (gAreas <> nil) then
    for a := 0 to High(gAreas) do
      if gAreas[a].AreaType <> AREA_NONE then
        with AreaSize[gAreas[a].AreaType] do
        begin
          ID := DWORD(-1);
          sel := ObjectSelected(OBJECT_AREA, a);

          case gAreas[a].AreaType of
            AREA_PLAYERPOINT1: g_GetTexture('AREA_PLAYERPOINT1', ID);
            AREA_PLAYERPOINT2: g_GetTexture('AREA_PLAYERPOINT2', ID);
            AREA_DMPOINT: g_GetTexture('AREA_DMPOINT', ID);
            AREA_REDFLAG: g_GetTexture('AREA_REDFLAG', ID);
            AREA_BLUEFLAG: g_GetTexture('AREA_BLUEFLAG', ID);
            AREA_DOMFLAG: g_GetTexture('AREA_DOMFLAG', ID);
            AREA_REDTEAMPOINT: g_GetTexture('AREA_REDPOINT', ID);
            AREA_BLUETEAMPOINT: g_GetTexture('AREA_BLUEPOINT', ID);
          end;

          if (PreviewMode = 0) or
             (gAreas[a].AreaType = AREA_REDFLAG) or
             (gAreas[a].AreaType = AREA_BLUEFLAG) or
             (gAreas[a].AreaType = AREA_DOMFLAG) then
            if ID <> DWORD(-1) then
              if gAreas[a].Direction = D_LEFT then
                e_Draw(ID, MapOffset.X+gAreas[a].X-X, MapOffset.Y+gAreas[a].Y-Y,
                       0, True, False, M_HORIZONTAL)
              else
                e_Draw(ID, MapOffset.X+gAreas[a].X-X, MapOffset.Y+gAreas[a].Y-Y,
                       0, True, False);

        // Рамка:
          if PreviewMode = 0 then
          begin
            e_DrawQuad(MapOffset.X+gAreas[a].X, MapOffset.Y+gAreas[a].Y,
                       MapOffset.X+gAreas[a].X+Width-1, MapOffset.Y+gAreas[a].Y+Height-1,
                       255, 255, 255, IfThen(sel, 0, gAlphaAreaRect));

            e_DrawPoint(2, MapOffset.X+gAreas[a].X, MapOffset.Y+gAreas[a].Y, 255, 0, 0);
          end;
        end;

// Рисуем жидкости и передний план после областей:
  if LayerEnabled[LAYER_WATER] or (PreviewMode = 1) then
    DrawPanels(PANEL_WATER or PANEL_ACID1 or PANEL_ACID2);
  if LayerEnabled[LAYER_FOREGROUND] or (PreviewMode = 1) then
    DrawPanels(PANEL_FORE);

// Рисуем триггеры:
  if LayerEnabled[LAYER_TRIGGERS] and
     (PreviewMode = 0) and (gTriggers <> nil) then
    for a := 0 to High(gTriggers) do
      with gTriggers[a] do
        if TriggerType <> TRIGGER_NONE then
        begin
        // Если выбран - рисуем красным, иначе - белым:
          sel := ObjectSelected(OBJECT_TRIGGER, a);

          e_DrawFillQuad(MapOffset.X+X, MapOffset.Y+Y,
                         MapOffset.X+X+Width-1, MapOffset.Y+Y+Height-1,
                         0, 0, 0, 150);

          if TexturePanel <> -1 then
          begin
            with gPanels[TexturePanel] do
            begin
              xx := X;
              yy := Y;
              ww := Width;
              hh := Height;
            end;

            e_DrawQuad(MapOffset.X+xx, MapOffset.Y+yy,
                       MapOffset.X+xx+ww-1, MapOffset.Y+yy+hh-1,
                       255, 255, 255, IfThen(sel, 0, gAlphaTriggerArea));
            e_DrawLine(1, MapOffset.X+X+(Width div 2), MapOffset.Y+Y+(Height div 2),
                       MapOffset.X+xx, MapOffset.Y+yy,
                       0, 255, 0, IfThen(sel, 0, gAlphaTriggerArea));
          end;

          case TriggerType of
            TRIGGER_EXIT: ;

            TRIGGER_TELEPORT:
              begin
              // Точка назначения Телепорта:
                if Data.d2d_teleport then
                  e_DrawLine(2, MapOffset.X+Data.TargetPoint.X-16,
                             MapOffset.Y+Data.TargetPoint.Y-1,
                             MapOffset.X+Data.TargetPoint.X+16, MapOffset.Y+Data.TargetPoint.Y-1,
                             0, 0, 255, IfThen(sel, 0, gAlphaTriggerArea))
                else
                  e_DrawQuad(MapOffset.X+Data.TargetPoint.X,
                             MapOffset.Y+Data.TargetPoint.Y,
                             MapOffset.X+Data.TargetPoint.X+AreaSize[AREA_DMPOINT].Width-1,
                             MapOffset.Y+Data.TargetPoint.Y+AreaSize[AREA_DMPOINT].Height-1,
                             255, 255, 255, IfThen(sel, 0, gAlphaTriggerArea));
                e_DrawPoint(2, MapOffset.X+Data.TargetPoint.X,
                            MapOffset.Y+Data.TargetPoint.Y,
                            255, 0, 0);
              // Линия к точке назначения Телепорта:
                e_DrawLine(1, MapOffset.X+X+(Width div 2),
                           MapOffset.Y+Y+(Height div 2),
                           MapOffset.X+Data.TargetPoint.X,
                           MapOffset.Y+Data.TargetPoint.Y,
                           255, 255, 255, IfThen(sel, 0, gAlphaTriggerLine));
              end;

            TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR,
            TRIGGER_DOOR, TRIGGER_DOOR5, TRIGGER_CLOSETRAP,
            TRIGGER_TRAP, TRIGGER_LIFTUP, TRIGGER_LIFTDOWN,
            TRIGGER_LIFT:
              if Data.PanelID <> -1 then
              begin
              // Дверь/Ловушка/Лифт:
                e_DrawQuad(MapOffset.X+gPanels[Data.PanelID].X,
                           MapOffset.Y+gPanels[Data.PanelID].Y,
                           MapOffset.X+gPanels[Data.PanelID].X+gPanels[Data.PanelID].Width-1,
                           MapOffset.Y+gPanels[Data.PanelID].Y+gPanels[Data.PanelID].Height-1,
                           255, 255, 255, IfThen(sel, 0, gAlphaTriggerArea));
                e_DrawPoint(2, MapOffset.X+gPanels[Data.PanelID].X,
                            MapOffset.Y+gPanels[Data.PanelID].Y,
                            255, 0, 0);
              // Линия к Двери/Ловушке/Лифту:
                e_DrawLine(1, MapOffset.X+X+(Width div 2),
                           MapOffset.Y+Y+(Height div 2),
                           MapOffset.X+gPanels[Data.PanelID].X,
                           MapOffset.Y+gPanels[Data.PanelID].Y,
                           255, 255, 255, IfThen(sel, 0, gAlphaTriggerLine));
              end;

            TRIGGER_PRESS, TRIGGER_ON,
            TRIGGER_OFF, TRIGGER_ONOFF:
              begin
                if (Data.tWidth > 0) and
                   (Data.tHeight > 0) then
                begin
                // Область Расширителя:
                  e_DrawQuad(MapOffset.X+Data.tX,
                             MapOffset.Y+Data.tY,
                             MapOffset.X+Data.tX+Data.tWidth-1,
                             MapOffset.Y+Data.tY+Data.tHeight-1,
                             255, 255, 255, IfThen(sel, 0, gAlphaTriggerArea));
                  e_DrawPoint(2, MapOffset.X+Data.tX,
                              MapOffset.Y+Data.tY,
                              255, 0, 0);
                // Линия к области Расширителя:
                  e_DrawLine(1, MapOffset.X+X+(Width div 2),
                             MapOffset.Y+Y+(Height div 2),
                             MapOffset.X+Data.tX,
                             MapOffset.Y+Data.tY,
                             255, 255, 255, IfThen(sel, 0, gAlphaTriggerLine));
                end;

                if Data.MonsterID <> 0 then
                  if (gMonsters <> nil) and
                     (gMonsters[Data.MonsterID-1].MonsterType <> MONSTER_NONE) then
                  begin
                  // Рамка вокруг Монстра - условия активации:
                    r := ObjectGetRect(OBJECT_MONSTER, Data.MonsterID-1);
                    e_DrawQuad(MapOffset.X+r.X,
                               MapOffset.Y+r.Y,
                               MapOffset.X+r.X+r.Width-1,
                               MapOffset.Y+r.Y+r.Height-1,
                               0, 255, 0, IfThen(sel, 0, gAlphaTriggerArea));
                  // Линия к Монстру - условию активации:
                    e_DrawLine(1, MapOffset.X+X+(Width div 2),
                               MapOffset.Y+Y+(Height div 2),
                               MapOffset.X+r.X,
                               MapOffset.Y+r.Y,
                               0, 255, 0, IfThen(sel, 0, gAlphaTriggerArea));

                  end;
              end;

            TRIGGER_SECRET: ;

            TRIGGER_SPAWNMONSTER, TRIGGER_SPAWNITEM:
              begin
                if TriggerType = TRIGGER_SPAWNMONSTER then
                  begin
                    xx := Data.MonPos.X;
                    yy := Data.MonPos.Y;
                  end
                else
                  begin
                    xx := Data.ItemPos.X;
                    yy := Data.ItemPos.Y;
                  end;
              // Точка появления Монстра/Предмета:
                e_DrawLine(2, MapOffset.X+xx-16,
                           MapOffset.Y+yy-1,
                           MapOffset.X+xx+16,
                           MapOffset.Y+yy-1,
                           0, 0, 255, IfThen(sel, 0, gAlphaTriggerArea));
                e_DrawPoint(2, MapOffset.X+xx,
                            MapOffset.Y+yy,
                            255, 0, 0);
              // Линия к точке появления Монстра/Предмета:
                e_DrawLine(1, MapOffset.X+X+(Width div 2),
                           MapOffset.Y+Y+(Height div 2),
                           MapOffset.X+xx,
                           MapOffset.Y+yy,
                           255, 255, 255, IfThen(sel, 0, gAlphaTriggerLine));
              // Предпросмотр создаваемого объекта:
                if sel then
                begin
                // Монстр:
                  if TriggerType = TRIGGER_SPAWNMONSTER then
                  begin
                    if Data.MonType <> MONSTER_NONE then
                      with MonsterSize[Data.MonType] do
                      begin
                        ID := DWORD(-1);
                        case Data.MonType of
                          MONSTER_DEMON: g_GetTexture('MONSTER_DEMON', ID);
                          MONSTER_IMP: g_GetTexture('MONSTER_IMP', ID);
                          MONSTER_ZOMBY: g_GetTexture('MONSTER_ZOMBY', ID);
                          MONSTER_SERG: g_GetTexture('MONSTER_SERG', ID);
                          MONSTER_CYBER: g_GetTexture('MONSTER_CYBER', ID);
                          MONSTER_CGUN: g_GetTexture('MONSTER_CGUN', ID);
                          MONSTER_BARON: g_GetTexture('MONSTER_BARON', ID);
                          MONSTER_KNIGHT: g_GetTexture('MONSTER_KNIGHT', ID);
                          MONSTER_CACO: g_GetTexture('MONSTER_CACO', ID);
                          MONSTER_SOUL: g_GetTexture('MONSTER_SOUL', ID);
                          MONSTER_PAIN: g_GetTexture('MONSTER_PAIN', ID);
                          MONSTER_SPIDER: g_GetTexture('MONSTER_SPIDER', ID);
                          MONSTER_BSP: g_GetTexture('MONSTER_BSP', ID);
                          MONSTER_MANCUB: g_GetTexture('MONSTER_MANCUB', ID);
                          MONSTER_SKEL: g_GetTexture('MONSTER_SKEL', ID);
                          MONSTER_VILE: g_GetTexture('MONSTER_VILE', ID);
                          MONSTER_FISH: g_GetTexture('MONSTER_FISH', ID);
                          MONSTER_BARREL: g_GetTexture('MONSTER_BARREL', ID);
                          MONSTER_ROBO: g_GetTexture('MONSTER_ROBO', ID);
                          MONSTER_MAN: g_GetTexture('MONSTER_MAN', ID);
                        end;
                        if ID <> DWORD(-1) then
                        begin
                          if Data.MonDir = Byte(D_LEFT) then
                          begin
                            xx := X - MonsterSizeDelta[Data.MonType].X + Width;
                            xx := MonsterSizeDelta[Data.MonType].Width - xx - X;
                            xx := Data.TargetPoint.X - X - xx - (Width div 2);

                            yy := Data.TargetPoint.Y - Y +
                                  MonsterSizeDelta[Data.MonType].Y-Height;

                            e_Draw(ID, MapOffset.X + xx, MapOffset.Y + yy,
                                   128, True, False, M_HORIZONTAL);
                          end
                          else
                          begin
                            xx := Data.TargetPoint.X - X +
                                  MonsterSizeDelta[Data.MonType].X-(Width div 2);
                            yy := Data.TargetPoint.Y - Y +
                                  MonsterSizeDelta[Data.MonType].Y-Height;

                            e_Draw(ID, MapOffset.X + xx, MapOffset.Y + yy,
                                   128, True, False);
                          end;
                        // Рамка:
                          e_DrawQuad(MapOffset.X+Data.TargetPoint.X-(Width div 2), MapOffset.Y+Data.TargetPoint.Y-Height,
                                     MapOffset.X+Data.TargetPoint.X+(Width div 2)-1, MapOffset.Y+Data.TargetPoint.Y-1,
                                     255, 255, 0, 128);
                        end;
                      end;
                  end
                  else
                // Предмет:
                  begin
                    if Data.ItemType <> ITEM_NONE then
                    begin
                      ID := DWORD(-1);
                      case Data.ItemType of
                        ITEM_MEDKIT_SMALL: g_GetTexture('ITEM_MEDKIT_SMALL', ID);
                        ITEM_MEDKIT_LARGE: g_GetTexture('ITEM_MEDKIT_LARGE', ID);
                        ITEM_MEDKIT_BLACK: g_GetTexture('ITEM_MEDKIT_BLACK', ID);
                        ITEM_ARMOR_GREEN: g_GetTexture('ITEM_ARMORGREEN', ID);
                        ITEM_ARMOR_BLUE: g_GetTexture('ITEM_ARMORBLUE', ID);
                        ITEM_SPHERE_BLUE: g_GetTexture('ITEM_BLUESPHERE', ID);
                        ITEM_SPHERE_WHITE: g_GetTexture('ITEM_WHITESPHERE', ID);
                        ITEM_SUIT: g_GetTexture('ITEM_SUIT', ID);
                        ITEM_OXYGEN: g_GetTexture('ITEM_OXYGEN', ID);
                        ITEM_INVUL: g_GetTexture('ITEM_INVUL', ID);
                        ITEM_WEAPON_SAW: g_GetTexture('ITEM_WEAPON_SAW', ID);
                        ITEM_WEAPON_SHOTGUN1: g_GetTexture('ITEM_WEAPON_SHOTGUN1', ID);
                        ITEM_WEAPON_SHOTGUN2: g_GetTexture('ITEM_WEAPON_SHOTGUN2', ID);
                        ITEM_WEAPON_CHAINGUN: g_GetTexture('ITEM_WEAPON_CHAINGUN', ID);
                        ITEM_WEAPON_ROCKETLAUNCHER: g_GetTexture('ITEM_WEAPON_ROCKETLAUNCHER', ID);
                        ITEM_WEAPON_PLASMA: g_GetTexture('ITEM_WEAPON_PLASMA', ID);
                        ITEM_WEAPON_BFG: g_GetTexture('ITEM_WEAPON_BFG', ID);
                        ITEM_WEAPON_SUPERPULEMET: g_GetTexture('ITEM_WEAPON_SUPERPULEMET', ID);
                        ITEM_AMMO_BULLETS: g_GetTexture('ITEM_AMMO_BULLETS', ID);
                        ITEM_AMMO_BULLETS_BOX: g_GetTexture('ITEM_AMMO_BULLETS_BOX', ID);
                        ITEM_AMMO_SHELLS: g_GetTexture('ITEM_AMMO_SHELLS', ID);
                        ITEM_AMMO_SHELLS_BOX: g_GetTexture('ITEM_AMMO_SHELLS_BOX', ID);
                        ITEM_AMMO_ROCKET: g_GetTexture('ITEM_AMMO_ROCKET', ID);
                        ITEM_AMMO_ROCKET_BOX: g_GetTexture('ITEM_AMMO_ROCKET_BOX', ID);
                        ITEM_AMMO_CELL: g_GetTexture('ITEM_AMMO_CELL', ID);
                        ITEM_AMMO_CELL_BIG: g_GetTexture('ITEM_AMMO_CELL_BIG', ID);
                        ITEM_AMMO_BACKPACK: g_GetTexture('ITEM_AMMO_BACKPACK', ID);
                        ITEM_KEY_RED: g_GetTexture('ITEM_KEY_RED', ID);
                        ITEM_KEY_GREEN: g_GetTexture('ITEM_KEY_GREEN', ID);
                        ITEM_KEY_BLUE: g_GetTexture('ITEM_KEY_BLUE', ID);
                        ITEM_BOTTLE: g_GetTexture('ITEM_BOTTLE', ID);
                        ITEM_HELMET: g_GetTexture('ITEM_HELMET', ID);
                        ITEM_JETPACK: g_GetTexture('ITEM_JETPACK', ID);
                        ITEM_INVIS: g_GetTexture('ITEM_INVIS', ID);
                        ITEM_WEAPON_FLAMETHROWER: g_GetTexture('ITEM_WEAPON_FLAMETHROWER', ID);
                        ITEM_AMMO_FUELCAN: g_GetTexture('ITEM_AMMO_FUELCAN', ID);
                      end;
                      if ID <> DWORD(-1) then
                        e_Draw(ID, MapOffset.X+Data.ItemPos.X-(ItemSize[Data.ItemType][0] div 2),
                               MapOffset.Y+Data.ItemPos.Y-ItemSize[Data.ItemType][1],
                               128, True, False);
                      // Рамка:
                        e_DrawQuad(MapOffset.X+Data.ItemPos.X-(ItemSize[Data.ItemType][0] div 2), MapOffset.Y+Data.ItemPos.Y-ItemSize[Data.ItemType][1],
                                   MapOffset.X+Data.ItemPos.X+(ItemSize[Data.ItemType][0] div 2)-1, MapOffset.Y+Data.ItemPos.Y-1,
                                   255, 255, 0, 128);
                    end;
                  end;
                end;
              end;

            TRIGGER_PUSH:
              begin
              // Линия направления и силы ускорения:
                xx := Round(Cos(-DegToRad(Data.PushAngle)) * Data.PushForce) + X+(Width div 2);
                yy := Round(Sin(-DegToRad(Data.PushAngle)) * Data.PushForce) + Y+(Height div 2);
                e_DrawLine(1, MapOffset.X+X+(Width div 2),
                           MapOffset.Y+Y+(Height div 2),
                           MapOffset.X+xx,
                           MapOffset.Y+yy,
                           255, 255, 255, IfThen(sel, 0, gAlphaTriggerLine));
              end;

            TRIGGER_SHOT:
              begin;
                xx := Data.ShotPos.X;
                yy := Data.ShotPos.Y;
              // Точка появления снаряда:
                e_DrawPoint(2, MapOffset.X+xx,
                            MapOffset.Y+yy,
                            255, 0, 0);
              // Линия к точке появления снаряда:
                e_DrawLine(1, MapOffset.X+X+(Width div 2),
                           MapOffset.Y+Y+(Height div 2),
                           MapOffset.X+xx,
                           MapOffset.Y+yy,
                           255, 255, 255, IfThen(sel, 0, gAlphaTriggerLine));
              // Линия направления выстрела:
                dx := Round(Cos(-DegToRad(Data.ShotAngle)) * 24);
                dy := Round(Sin(-DegToRad(Data.ShotAngle)) * 24);
                e_DrawLine(1, MapOffset.X+xx,
                           MapOffset.Y+yy,
                           MapOffset.X+xx+dx,
                           MapOffset.Y+yy+dy,
                           255, 255, 255, IfThen(sel, 0, gAlphaTriggerLine));

                if (Data.ShotPanelID <> -1) and
                   (Data.ShotPanelID <= High(gPanels)) then
                begin
                // Линия к панели индикации выстрела
                  with gPanels[Data.ShotPanelID] do
                  begin
                    xx := X;
                    yy := Y;
                    ww := Width;
                    hh := Height;
                  end;

                  e_DrawQuad(MapOffset.X+xx, MapOffset.Y+yy,
                             MapOffset.X+xx+ww-1, MapOffset.Y+yy+hh-1,
                             255, 255, 255, IfThen(sel, 0, gAlphaTriggerArea));
                  e_DrawLine(1, MapOffset.X+X+(Width div 2), MapOffset.Y+Y+(Height div 2),
                             MapOffset.X+xx, MapOffset.Y+yy,
                             255, 255, 0, IfThen(sel, 0, gAlphaTriggerArea));
                end;
              end;
          end;
        end;

// Границы карты:
  if PreviewMode = 0 then
  begin
    e_DrawFillQuad(-32+MapOffset.X,
                   -32+MapOffset.Y,
                   gMapInfo.Width+31+MapOffset.X,
                   -1+MapOffset.Y,
                   drEdge[0], drEdge[1], drEdge[2], drEdge[3], B_NONE); // Top
    e_DrawFillQuad(-32+MapOffset.X,
                   gMapInfo.Height+MapOffset.Y,
                   gMapInfo.Width+31+MapOffset.X,
                   gMapInfo.Height+31+MapOffset.Y,
                   drEdge[0], drEdge[1], drEdge[2], drEdge[3], B_NONE); // Bottom
    e_DrawFillQuad(-32+MapOffset.X,
                   MapOffset.Y,
                   -1+MapOffset.X,
                   gMapInfo.Height+MapOffset.Y-1,
                   drEdge[0], drEdge[1], drEdge[2], drEdge[3], B_NONE); // Left
    e_DrawFillQuad(gMapInfo.Width+MapOffset.X,
                   MapOffset.Y,
                   gMapInfo.Width+31+MapOffset.X,
                   gMapInfo.Height+MapOffset.Y-1,
                   drEdge[0], drEdge[1], drEdge[2], drEdge[3], B_NONE); // Right
  end;
end;

procedure ShiftMapObjects(dx, dy: Integer);
var
  i: Integer;
begin
  if gPanels <> nil then
    for i := 0 to High(gPanels) do
      if gPanels[i].PanelType <> 0 then
      begin
        Inc(gPanels[i].X, dx);
        Inc(gPanels[i].Y, dy);
      end;

  if gItems <> nil then
    for i := 0 to High(gItems) do
      if gItems[i].ItemType <> 0 then
      begin
        Inc(gItems[i].X, dx);
        Inc(gItems[i].Y, dy);
      end;

  if gAreas <> nil then
    for i := 0 to High(gAreas) do
      if gAreas[i].AreaType <> 0 then
      begin
        Inc(gAreas[i].X, dx);
        Inc(gAreas[i].Y, dy);
      end;

  if gMonsters <> nil then
    for i := 0 to High(gMonsters) do
      if gMonsters[i].MonsterType <> 0 then
      begin
        Inc(gMonsters[i].X, dx);
        Inc(gMonsters[i].Y, dy);
      end;

  if gTriggers <> nil then
    for i := 0 to High(gTriggers) do
      if gTriggers[i].TriggerType <> 0 then
      begin
        Inc(gTriggers[i].X, dx);
        Inc(gTriggers[i].Y, dy);

        case gTriggers[i].TriggerType of
          TRIGGER_TELEPORT:
            begin
              Inc(gTriggers[i].Data.TargetPoint.X, dx);
              Inc(gTriggers[i].Data.TargetPoint.Y, dy);
            end;

          TRIGGER_PRESS, TRIGGER_ON,
          TRIGGER_OFF, TRIGGER_ONOFF:
            begin
              Inc(gTriggers[i].Data.tX, dx);
              Inc(gTriggers[i].Data.tY, dy);
            end;

          TRIGGER_SPAWNMONSTER:
            begin
              Inc(gTriggers[i].Data.MonPos.X, dx);
              Inc(gTriggers[i].Data.MonPos.Y, dy);
            end;

          TRIGGER_SPAWNITEM:
            begin
              Inc(gTriggers[i].Data.ItemPos.X, dx);
              Inc(gTriggers[i].Data.ItemPos.Y, dy);
            end;

          TRIGGER_SHOT:
            begin
              Inc(gTriggers[i].Data.ShotPos.X, dx);
              Inc(gTriggers[i].Data.ShotPos.Y, dy);
            end;
        end;
      end;
end;

procedure LoadData();
begin
 g_CreateTextureWAD('PREVIEW', EditorDir+'data/Editor.wad:TEXTURES\CHECKERS');
 g_CreateTextureWAD('NOTEXTURE', EditorDir+'data/Game.wad:TEXTURES\NOTEXTURE');

 g_CreateTextureWADSize('AREA_REDFLAG', EditorDir+'data/Game.wad:TEXTURES\FLAGRED', 0, 0, 64, 64);
 g_CreateTextureWADSize('AREA_BLUEFLAG', EditorDir+'data/Game.wad:TEXTURES\FLAGBLUE', 0, 0, 64, 64);
 g_CreateTextureWADSize('AREA_DOMFLAG', EditorDir+'data/Game.wad:TEXTURES\FLAGDOM', 0, 0, 64, 64);

 g_CreateTextureWADSize('MONSTER_DEMON', EditorDir+'data/Game.wad:MTEXTURES\DEMON_SLEEP', 0, 0, 64, 64);
 g_CreateTextureWADSize('MONSTER_IMP', EditorDir+'data/Game.wad:MTEXTURES\IMP_SLEEP', 0, 0, 64, 64);
 g_CreateTextureWADSize('MONSTER_ZOMBY', EditorDir+'data/Game.wad:MTEXTURES\ZOMBY_SLEEP', 0, 0, 64, 64);
 g_CreateTextureWADSize('MONSTER_SERG', EditorDir+'data/Game.wad:MTEXTURES\SERG_SLEEP', 0, 0, 64, 64);
 g_CreateTextureWADSize('MONSTER_CYBER', EditorDir+'data/Game.wad:MTEXTURES\CYBER_SLEEP', 0, 0, 128, 128);
 g_CreateTextureWADSize('MONSTER_CGUN', EditorDir+'data/Game.wad:MTEXTURES\CGUN_SLEEP', 0, 0, 64, 64);
 g_CreateTextureWADSize('MONSTER_BARON', EditorDir+'data/Game.wad:MTEXTURES\BARON_SLEEP', 0, 0, 128, 128);
 g_CreateTextureWADSize('MONSTER_KNIGHT', EditorDir+'data/Game.wad:MTEXTURES\KNIGHT_SLEEP', 0, 0, 128, 128);
 g_CreateTextureWADSize('MONSTER_CACO', EditorDir+'data/Game.wad:MTEXTURES\CACO_SLEEP', 0, 0, 128, 128);
 g_CreateTextureWADSize('MONSTER_SOUL', EditorDir+'data/Game.wad:MTEXTURES\SOUL_SLEEP', 0, 0, 64, 64);
 g_CreateTextureWADSize('MONSTER_PAIN', EditorDir+'data/Game.wad:MTEXTURES\PAIN_SLEEP', 0, 0, 128, 128);
 g_CreateTextureWADSize('MONSTER_SPIDER', EditorDir+'data/Game.wad:MTEXTURES\SPIDER_SLEEP', 0, 0, 256, 128);
 g_CreateTextureWADSize('MONSTER_BSP', EditorDir+'data/Game.wad:MTEXTURES\BSP_SLEEP', 0, 0, 128, 64);
 g_CreateTextureWADSize('MONSTER_MANCUB', EditorDir+'data/Game.wad:MTEXTURES\MANCUB_SLEEP', 0, 0, 128, 128);
 g_CreateTextureWADSize('MONSTER_SKEL', EditorDir+'data/Game.wad:MTEXTURES\SKEL_SLEEP', 0, 0, 128, 128);
 g_CreateTextureWADSize('MONSTER_VILE', EditorDir+'data/Game.wad:MTEXTURES\VILE_SLEEP', 0, 0, 128, 128);
 g_CreateTextureWADSize('MONSTER_FISH', EditorDir+'data/Game.wad:MTEXTURES\FISH_SLEEP', 0, 0, 32, 32);
 g_CreateTextureWADSize('MONSTER_BARREL', EditorDir+'data/Game.wad:MTEXTURES\BARREL_SLEEP', 0, 0, 64, 64);
 g_CreateTextureWADSize('MONSTER_ROBO', EditorDir+'data/Game.wad:MTEXTURES\ROBO_SLEEP', 0, 0, 128, 128);
 g_CreateTextureWADSize('MONSTER_MAN', EditorDir+'data/Game.wad:MTEXTURES\MAN_SLEEP', 0, 0, 64, 64);

 g_CreateTextureWADSize('ITEM_BLUESPHERE', EditorDir+'data/Game.wad:TEXTURES\SBLUE', 0, 0, 32, 32);
 g_CreateTextureWADSize('ITEM_WHITESPHERE', EditorDir+'data/Game.wad:TEXTURES\SWHITE', 0, 0, 32, 32);
 g_CreateTextureWADSize('ITEM_ARMORGREEN', EditorDir+'data/Game.wad:TEXTURES\ARMORGREEN', 0, 0, 32, 16);
 g_CreateTextureWADSize('ITEM_ARMORBLUE', EditorDir+'data/Game.wad:TEXTURES\ARMORBLUE', 0, 0, 32, 16);
 g_CreateTextureWADSize('ITEM_INVUL', EditorDir+'data/Game.wad:TEXTURES\INVUL', 0, 0, 32, 32);
 g_CreateTextureWADSize('ITEM_BOTTLE', EditorDir+'data/Game.wad:TEXTURES\BOTTLE', 0, 0, 16, 32);
 g_CreateTextureWADSize('ITEM_HELMET', EditorDir+'data/Game.wad:TEXTURES\HELMET', 0, 0, 16, 16);
 g_CreateTextureWADSize('ITEM_INVIS', EditorDir+'data/Game.wad:TEXTURES\INVIS', 0, 0, 32, 32);
 g_CreateTextureWADSize('ITEM_WEAPON_FLAMETHROWER', EditorDir+'data/Game.wad:TEXTURES\FLAMETHROWER', 0, 0, 64, 32);
 g_CreateTextureWADSize('ITEM_AMMO_FUELCAN', EditorDir+'data/Game.wad:TEXTURES\FUELCAN', 0, 0, 16, 32);

 g_CreateTextureWAD('ITEM_MEDKIT_SMALL', EditorDir+'data/Game.wad:TEXTURES\MED1');
 g_CreateTextureWAD('ITEM_MEDKIT_LARGE', EditorDir+'data/Game.wad:TEXTURES\MED2');
 g_CreateTextureWAD('ITEM_WEAPON_SAW', EditorDir+'data/Game.wad:TEXTURES\SAW');
 g_CreateTextureWAD('ITEM_WEAPON_PISTOL', EditorDir+'data/Game.wad:TEXTURES\PISTOL');
 g_CreateTextureWAD('ITEM_WEAPON_KASTET', EditorDir+'data/Game.wad:TEXTURES\KASTET');
 g_CreateTextureWAD('ITEM_WEAPON_SHOTGUN1', EditorDir+'data/Game.wad:TEXTURES\SHOTGUN1');
 g_CreateTextureWAD('ITEM_WEAPON_SHOTGUN2', EditorDir+'data/Game.wad:TEXTURES\SHOTGUN2');
 g_CreateTextureWAD('ITEM_WEAPON_CHAINGUN', EditorDir+'data/Game.wad:TEXTURES\MGUN');
 g_CreateTextureWAD('ITEM_WEAPON_ROCKETLAUNCHER', EditorDir+'data/Game.wad:TEXTURES\RLAUNCHER');
 g_CreateTextureWAD('ITEM_WEAPON_PLASMA', EditorDir+'data/Game.wad:TEXTURES\PGUN');
 g_CreateTextureWAD('ITEM_WEAPON_BFG', EditorDir+'data/Game.wad:TEXTURES\BFG');
 g_CreateTextureWAD('ITEM_WEAPON_SUPERPULEMET', EditorDir+'data/Game.wad:TEXTURES\SPULEMET');
 g_CreateTextureWAD('ITEM_AMMO_BULLETS', EditorDir+'data/Game.wad:TEXTURES\CLIP');
 g_CreateTextureWAD('ITEM_AMMO_BULLETS_BOX', EditorDir+'data/Game.wad:TEXTURES\AMMO');
 g_CreateTextureWAD('ITEM_AMMO_SHELLS', EditorDir+'data/Game.wad:TEXTURES\SHELL1');
 g_CreateTextureWAD('ITEM_AMMO_SHELLS_BOX', EditorDir+'data/Game.wad:TEXTURES\SHELL2');
 g_CreateTextureWAD('ITEM_AMMO_ROCKET', EditorDir+'data/Game.wad:TEXTURES\ROCKET');
 g_CreateTextureWAD('ITEM_AMMO_ROCKET_BOX', EditorDir+'data/Game.wad:TEXTURES\ROCKETS');
 g_CreateTextureWAD('ITEM_AMMO_CELL', EditorDir+'data/Game.wad:TEXTURES\CELL');
 g_CreateTextureWAD('ITEM_AMMO_CELL_BIG', EditorDir+'data/Game.wad:TEXTURES\CELL2');
 g_CreateTextureWAD('ITEM_AMMO_BACKPACK', EditorDir+'data/Game.wad:TEXTURES\BPACK');
 g_CreateTextureWAD('ITEM_KEY_RED', EditorDir+'data/Game.wad:TEXTURES\KEYR');
 g_CreateTextureWAD('ITEM_KEY_GREEN', EditorDir+'data/Game.wad:TEXTURES\KEYG');
 g_CreateTextureWAD('ITEM_KEY_BLUE', EditorDir+'data/Game.wad:TEXTURES\KEYB');
 g_CreateTextureWAD('ITEM_OXYGEN', EditorDir+'data/Game.wad:TEXTURES\OXYGEN');
 g_CreateTextureWAD('ITEM_SUIT', EditorDir+'data/Game.wad:TEXTURES\SUIT');
 g_CreateTextureWAD('ITEM_MEDKIT_BLACK', EditorDir+'data/Game.wad:TEXTURES\BMED');
 g_CreateTextureWAD('ITEM_JETPACK', EditorDir+'data/Game.wad:TEXTURES\JETPACK');

 g_CreateTextureWAD('AREA_PLAYERPOINT1', EditorDir+'data/Editor.wad:TEXTURES\P1POINT');
 g_CreateTextureWAD('AREA_PLAYERPOINT2', EditorDir+'data/Editor.wad:TEXTURES\P2POINT');
 g_CreateTextureWAD('AREA_DMPOINT', EditorDir+'data/Editor.wad:TEXTURES\DMPOINT');
 g_CreateTextureWAD('AREA_REDPOINT', EditorDir+'data/Editor.wad:TEXTURES\REDPOINT');
 g_CreateTextureWAD('AREA_BLUEPOINT', EditorDir+'data/Editor.wad:TEXTURES\BLUEPOINT');
end;

procedure FreeData();
begin
 g_DeleteTexture('NOTEXTURE');

 g_DeleteTexture('ITEM_BLUESPHERE');
 g_DeleteTexture('ITEM_WHITESPHERE');
 g_DeleteTexture('ITEM_ARMORGREEN');
 g_DeleteTexture('ITEM_ARMORBLUE');
 g_DeleteTexture('ITEM_INVUL');
 g_DeleteTexture('ITEM_BOTTLE');
 g_DeleteTexture('ITEM_HELMET');
 g_DeleteTexture('AREA_REDFLAG');
 g_DeleteTexture('AREA_BLUEFLAG');
 g_DeleteTexture('AREA_DOMFLAG');

 g_DeleteTexture('MONSTER_DEMON');
 g_DeleteTexture('MONSTER_IMP');
 g_DeleteTexture('MONSTER_ZOMBY');
 g_DeleteTexture('MONSTER_SERG');
 g_DeleteTexture('MONSTER_CGUN');
 g_DeleteTexture('MONSTER_BARREL');
 g_DeleteTexture('MONSTER_MAN');

 g_DeleteTexture('MONSTER_DEMON');
 g_DeleteTexture('MONSTER_IMP');
 g_DeleteTexture('MONSTER_ZOMBY');
 g_DeleteTexture('MONSTER_SERG');
 g_DeleteTexture('MONSTER_CYBER');
 g_DeleteTexture('MONSTER_CGUN');
 g_DeleteTexture('MONSTER_BARON');
 g_DeleteTexture('MONSTER_KNIGHT');
 g_DeleteTexture('MONSTER_CACO');
 g_DeleteTexture('MONSTER_SOUL');
 g_DeleteTexture('MONSTER_PAIN');
 g_DeleteTexture('MONSTER_SPIDER');
 g_DeleteTexture('MONSTER_BSP');
 g_DeleteTexture('MONSTER_MANCUB');
 g_DeleteTexture('MONSTER_SKEL');
 g_DeleteTexture('MONSTER_VILE');
 g_DeleteTexture('MONSTER_FISH');
 g_DeleteTexture('MONSTER_BARREL');
 g_DeleteTexture('MONSTER_ROBO');
 g_DeleteTexture('MONSTER_MAN');

 g_DeleteTexture('ITEM_MEDKIT_SMALL');
 g_DeleteTexture('ITEM_MEDKIT_LARGE');
 g_DeleteTexture('ITEM_WEAPON_SAW');
 g_DeleteTexture('ITEM_WEAPON_PISTOL');
 g_DeleteTexture('ITEM_WEAPON_KASTET');
 g_DeleteTexture('ITEM_WEAPON_SHOTGUN1');
 g_DeleteTexture('ITEM_WEAPON_SHOTGUN2');
 g_DeleteTexture('ITEM_WEAPON_CHAINGUN');
 g_DeleteTexture('ITEM_WEAPON_ROCKETLAUNCHER');
 g_DeleteTexture('ITEM_WEAPON_PLASMA');
 g_DeleteTexture('ITEM_WEAPON_BFG');
 g_DeleteTexture('ITEM_WEAPON_SUPERPULEMET');
 g_DeleteTexture('ITEM_AMMO_BULLETS');
 g_DeleteTexture('ITEM_AMMO_BULLETS_BOX');
 g_DeleteTexture('ITEM_AMMO_SHELLS');
 g_DeleteTexture('ITEM_AMMO_SHELLS_BOX');
 g_DeleteTexture('ITEM_AMMO_ROCKET');
 g_DeleteTexture('ITEM_AMMO_ROCKET_BOX');
 g_DeleteTexture('ITEM_AMMO_CELL');
 g_DeleteTexture('ITEM_AMMO_CELL_BIG');
 g_DeleteTexture('ITEM_AMMO_BACKPACK');
 g_DeleteTexture('ITEM_KEY_RED');
 g_DeleteTexture('ITEM_KEY_GREEN');
 g_DeleteTexture('ITEM_KEY_BLUE');
 g_DeleteTexture('ITEM_OXYGEN');
 g_DeleteTexture('ITEM_SUIT');
 g_DeleteTexture('ITEM_MEDKIT_BLACK');
 g_DeleteTexture('ITEM_JETPACK');
 g_DeleteTexture('ITEM_INVIS');
 g_DeleteTexture('ITEM_WEAPON_FLAMETHROWER');
 g_DeleteTexture('ITEM_AMMO_FUELCAN');

 g_DeleteTexture('AREA_PLAYERPOINT1');
 g_DeleteTexture('AREA_PLAYERPOINT2');
 g_DeleteTexture('AREA_DMPOINT');
 g_DeleteTexture('AREA_REDPOINT');
 g_DeleteTexture('AREA_BLUEPOINT');
end;

end.
