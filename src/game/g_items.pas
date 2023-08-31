(* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../shared/a_modes.inc}
unit g_items;

interface

uses
  SysUtils, Classes,
  MAPDEF, g_textures, g_phys, g_saveload;

Type
  PItem = ^TItem;
  TItem = record
  private
    //treeNode: Integer;
    slotIsUsed: Boolean;
    arrIdx: Integer; // in ggItems

  public
    ItemType: Byte;
    Respawnable: Boolean;
    InitX, InitY: Integer;
    RespawnTime: Word;
    alive: Boolean;
    Fall: Boolean;
    QuietRespawn: Boolean;
    SpawnTrigger: Integer;
    Obj: TObj;
    Animation: TAnimation;
    dropped: Boolean; // dropped from the monster? drops should be rendered after corpses, so zombie corpse will not obscure ammo container, for example
    NeedSend: Boolean;

    procedure positionChanged (); //WARNING! call this after monster position was changed, or coldet will not work right!
    procedure getMapBox (out x, y, w, h: Integer); inline;
    procedure moveBy (dx, dy: Integer); inline;

    property myid: Integer read arrIdx;
  end;

procedure g_Items_LoadData();
procedure g_Items_FreeData();
procedure g_Items_Init();
procedure g_Items_Free();
function g_Items_Create(X, Y: Integer; ItemType: Byte;
           Fall, Respawnable: Boolean; AdjCoord: Boolean = False; ForcedID: Integer = -1): DWORD;
procedure g_Items_SetDrop (ID: DWORD);
procedure g_Items_PreUpdate();
procedure g_Items_Update();
procedure g_Items_Draw();
procedure g_Items_DrawDrop();
procedure g_Items_Pick(ID: DWORD);
procedure g_Items_Remove(ID: DWORD);
procedure g_Items_SaveState (st: TStream);
procedure g_Items_LoadState (st: TStream);

procedure g_Items_RestartRound ();

function g_Items_ValidId (idx: Integer): Boolean; inline;
function g_Items_ByIdx (idx: Integer): PItem;
function g_Items_ObjByIdx (idx: Integer): PObj;

procedure g_Items_EmitPickupSound (idx: Integer); // at item position
procedure g_Items_EmitPickupSoundAt (idx, x, y: Integer);

procedure g_Items_AddDynLights();


type
  TItemEachAliveCB = function (it: PItem): Boolean is nested; // return `true` to stop

function g_Items_ForEachAlive (cb: TItemEachAliveCB; backwards: Boolean=false): Boolean;
function g_Items_NextAlive (startIdx: Integer): PItem;

var
  gItemsTexturesID: Array [1..ITEM_MAX] of DWORD;
  gMaxDist: Integer = 1; // for sounds

implementation

uses
  Math,
  g_basic, e_graphics, g_sound, g_main, g_gfx, g_map,
  g_game, g_triggers, g_console, g_player, g_net, g_netmsg,
  e_log,
  g_grid, binheap, idpool, utils, xstreams;


var
  ggItems: Array of TItem = nil;


// ////////////////////////////////////////////////////////////////////////// //
var
  freeIds: TIdPool = nil;


// ////////////////////////////////////////////////////////////////////////// //
function g_Items_ValidId (idx: Integer): Boolean; inline;
begin
  result := false;
  if (idx < 0) or (idx > High(ggItems)) then exit;
  if not ggItems[idx].slotIsUsed then exit;
  result := true;
end;


function g_Items_ByIdx (idx: Integer): PItem;
begin
  if (idx < 0) or (idx > High(ggItems)) then raise Exception.Create('g_ItemObjByIdx: invalid index');
  result := @ggItems[idx];
  if not result.slotIsUsed then raise Exception.Create('g_ItemObjByIdx: requested inexistent item');
end;


function g_Items_ObjByIdx (idx: Integer): PObj;
begin
  if (idx < 0) or (idx > High(ggItems)) then raise Exception.Create('g_ItemObjByIdx: invalid index');
  if not ggItems[idx].slotIsUsed then raise Exception.Create('g_ItemObjByIdx: requested inexistent item');
  result := @ggItems[idx].Obj;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TItem.positionChanged ();
begin
  NeedSend := NeedSend or (Obj.X <> Obj.oldX) or (Obj.Y <> Obj.oldY);
end;

procedure TItem.getMapBox (out x, y, w, h: Integer); inline;
begin
  x := Obj.X+Obj.Rect.X;
  y := Obj.Y+Obj.Rect.Y;
  w := Obj.Rect.Width;
  h := Obj.Rect.Height;
end;

procedure TItem.moveBy (dx, dy: Integer); inline;
begin
  if (dx <> 0) or (dy <> 0) then
  begin
    Obj.X += dx;
    Obj.Y += dy;
    positionChanged();
  end;
end;

// ////////////////////////////////////////////////////////////////////////// //
const
  ITEM_SIGNATURE = $4D455449; // 'ITEM'

  ITEMSIZE: Array [ITEM_MEDKIT_SMALL..ITEM_MAX] of Array [0..1] of Byte =
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

procedure InitTextures();
begin
  g_Texture_Get('ITEM_MEDKIT_SMALL',     gItemsTexturesID[ITEM_MEDKIT_SMALL]);
  g_Texture_Get('ITEM_MEDKIT_LARGE',     gItemsTexturesID[ITEM_MEDKIT_LARGE]);
  g_Texture_Get('ITEM_MEDKIT_BLACK',     gItemsTexturesID[ITEM_MEDKIT_BLACK]);
  g_Texture_Get('ITEM_SUIT',             gItemsTexturesID[ITEM_SUIT]);
  g_Texture_Get('ITEM_OXYGEN',           gItemsTexturesID[ITEM_OXYGEN]);
  g_Texture_Get('ITEM_WEAPON_SAW',       gItemsTexturesID[ITEM_WEAPON_SAW]);
  g_Texture_Get('ITEM_WEAPON_SHOTGUN1',  gItemsTexturesID[ITEM_WEAPON_SHOTGUN1]);
  g_Texture_Get('ITEM_WEAPON_SHOTGUN2',  gItemsTexturesID[ITEM_WEAPON_SHOTGUN2]);
  g_Texture_Get('ITEM_WEAPON_CHAINGUN',  gItemsTexturesID[ITEM_WEAPON_CHAINGUN]);
  g_Texture_Get('ITEM_WEAPON_ROCKETLAUNCHER', gItemsTexturesID[ITEM_WEAPON_ROCKETLAUNCHER]);
  g_Texture_Get('ITEM_WEAPON_PLASMA',    gItemsTexturesID[ITEM_WEAPON_PLASMA]);
  g_Texture_Get('ITEM_WEAPON_BFG',       gItemsTexturesID[ITEM_WEAPON_BFG]);
  g_Texture_Get('ITEM_WEAPON_SUPERPULEMET', gItemsTexturesID[ITEM_WEAPON_SUPERPULEMET]);
  g_Texture_Get('ITEM_WEAPON_FLAMETHROWER', gItemsTexturesID[ITEM_WEAPON_FLAMETHROWER]);
  g_Texture_Get('ITEM_AMMO_BULLETS',     gItemsTexturesID[ITEM_AMMO_BULLETS]);
  g_Texture_Get('ITEM_AMMO_BULLETS_BOX', gItemsTexturesID[ITEM_AMMO_BULLETS_BOX]);
  g_Texture_Get('ITEM_AMMO_SHELLS',      gItemsTexturesID[ITEM_AMMO_SHELLS]);
  g_Texture_Get('ITEM_AMMO_SHELLS_BOX',  gItemsTexturesID[ITEM_AMMO_SHELLS_BOX]);
  g_Texture_Get('ITEM_AMMO_ROCKET',      gItemsTexturesID[ITEM_AMMO_ROCKET]);
  g_Texture_Get('ITEM_AMMO_ROCKET_BOX',  gItemsTexturesID[ITEM_AMMO_ROCKET_BOX]);
  g_Texture_Get('ITEM_AMMO_CELL',        gItemsTexturesID[ITEM_AMMO_CELL]);
  g_Texture_Get('ITEM_AMMO_CELL_BIG',    gItemsTexturesID[ITEM_AMMO_CELL_BIG]);
  g_Texture_Get('ITEM_AMMO_FUELCAN',     gItemsTexturesID[ITEM_AMMO_FUELCAN]);
  g_Texture_Get('ITEM_AMMO_BACKPACK',    gItemsTexturesID[ITEM_AMMO_BACKPACK]);
  g_Texture_Get('ITEM_KEY_RED',          gItemsTexturesID[ITEM_KEY_RED]);
  g_Texture_Get('ITEM_KEY_GREEN',        gItemsTexturesID[ITEM_KEY_GREEN]);
  g_Texture_Get('ITEM_KEY_BLUE',         gItemsTexturesID[ITEM_KEY_BLUE]);
  g_Texture_Get('ITEM_WEAPON_KASTET',    gItemsTexturesID[ITEM_WEAPON_KASTET]);
  g_Texture_Get('ITEM_WEAPON_PISTOL',    gItemsTexturesID[ITEM_WEAPON_PISTOL]);
end;

procedure g_Items_LoadData();
begin
  e_WriteLog('Loading items data...', TMsgType.Notify);

  g_Sound_CreateWADEx('SOUND_ITEM_RESPAWNITEM', GameWAD+':SOUNDS\RESPAWNITEM');
  g_Sound_CreateWADEx('SOUND_ITEM_GETRULEZ', GameWAD+':SOUNDS\GETRULEZ');
  g_Sound_CreateWADEx('SOUND_ITEM_GETWEAPON', GameWAD+':SOUNDS\GETWEAPON');
  g_Sound_CreateWADEx('SOUND_ITEM_GETITEM', GameWAD+':SOUNDS\GETITEM');

  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_BLUESPHERE', GameWAD+':TEXTURES\SBLUE', 32, 32, 4, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_WHITESPHERE', GameWAD+':TEXTURES\SWHITE', 32, 32, 4, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_ARMORGREEN', GameWAD+':TEXTURES\ARMORGREEN', 32, 16, 3, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_ARMORBLUE', GameWAD+':TEXTURES\ARMORBLUE', 32, 16, 3, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_JETPACK', GameWAD+':TEXTURES\JETPACK', 32, 32, 3, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_INVUL', GameWAD+':TEXTURES\INVUL', 32, 32, 4, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_INVIS', GameWAD+':TEXTURES\INVIS', 32, 32, 4, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_RESPAWN', GameWAD+':TEXTURES\ITEMRESPAWN', 32, 32, 5, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_BOTTLE', GameWAD+':TEXTURES\BOTTLE', 16, 32, 4, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_HELMET', GameWAD+':TEXTURES\HELMET', 16, 16, 4, True);
  g_Frames_CreateWAD(nil, 'FRAMES_FLAG_RED', GameWAD+':TEXTURES\FLAGRED', 64, 64, 5, False);
  g_Frames_CreateWAD(nil, 'FRAMES_FLAG_BLUE', GameWAD+':TEXTURES\FLAGBLUE', 64, 64, 5, False);
  g_Frames_CreateWAD(nil, 'FRAMES_FLAG_DOM', GameWAD+':TEXTURES\FLAGDOM', 64, 64, 5, False);
  g_Texture_CreateWADEx('ITEM_MEDKIT_SMALL', GameWAD+':TEXTURES\MED1');
  g_Texture_CreateWADEx('ITEM_MEDKIT_LARGE', GameWAD+':TEXTURES\MED2');
  g_Texture_CreateWADEx('ITEM_WEAPON_SAW', GameWAD+':TEXTURES\SAW');
  g_Texture_CreateWADEx('ITEM_WEAPON_PISTOL', GameWAD+':TEXTURES\PISTOL');
  g_Texture_CreateWADEx('ITEM_WEAPON_KASTET', GameWAD+':TEXTURES\KASTET');
  g_Texture_CreateWADEx('ITEM_WEAPON_SHOTGUN1', GameWAD+':TEXTURES\SHOTGUN1');
  g_Texture_CreateWADEx('ITEM_WEAPON_SHOTGUN2', GameWAD+':TEXTURES\SHOTGUN2');
  g_Texture_CreateWADEx('ITEM_WEAPON_CHAINGUN', GameWAD+':TEXTURES\MGUN');
  g_Texture_CreateWADEx('ITEM_WEAPON_ROCKETLAUNCHER', GameWAD+':TEXTURES\RLAUNCHER');
  g_Texture_CreateWADEx('ITEM_WEAPON_PLASMA', GameWAD+':TEXTURES\PGUN');
  g_Texture_CreateWADEx('ITEM_WEAPON_BFG', GameWAD+':TEXTURES\BFG');
  g_Texture_CreateWADEx('ITEM_WEAPON_SUPERPULEMET', GameWAD+':TEXTURES\SPULEMET');
  g_Texture_CreateWADEx('ITEM_WEAPON_FLAMETHROWER', GameWAD+':TEXTURES\FLAMETHROWER');
  g_Texture_CreateWADEx('ITEM_AMMO_BULLETS', GameWAD+':TEXTURES\CLIP');
  g_Texture_CreateWADEx('ITEM_AMMO_BULLETS_BOX', GameWAD+':TEXTURES\AMMO');
  g_Texture_CreateWADEx('ITEM_AMMO_SHELLS', GameWAD+':TEXTURES\SHELL1');
  g_Texture_CreateWADEx('ITEM_AMMO_SHELLS_BOX', GameWAD+':TEXTURES\SHELL2');
  g_Texture_CreateWADEx('ITEM_AMMO_ROCKET', GameWAD+':TEXTURES\ROCKET');
  g_Texture_CreateWADEx('ITEM_AMMO_ROCKET_BOX', GameWAD+':TEXTURES\ROCKETS');
  g_Texture_CreateWADEx('ITEM_AMMO_CELL', GameWAD+':TEXTURES\CELL');
  g_Texture_CreateWADEx('ITEM_AMMO_CELL_BIG', GameWAD+':TEXTURES\CELL2');
  g_Texture_CreateWADEx('ITEM_AMMO_FUELCAN', GameWAD+':TEXTURES\FUELCAN');
  g_Texture_CreateWADEx('ITEM_AMMO_BACKPACK', GameWAD+':TEXTURES\BPACK');
  g_Texture_CreateWADEx('ITEM_KEY_RED', GameWAD+':TEXTURES\KEYR');
  g_Texture_CreateWADEx('ITEM_KEY_GREEN', GameWAD+':TEXTURES\KEYG');
  g_Texture_CreateWADEx('ITEM_KEY_BLUE', GameWAD+':TEXTURES\KEYB');
  g_Texture_CreateWADEx('ITEM_OXYGEN', GameWAD+':TEXTURES\OXYGEN');
  g_Texture_CreateWADEx('ITEM_SUIT', GameWAD+':TEXTURES\SUIT');
  g_Texture_CreateWADEx('ITEM_WEAPON_KASTET', GameWAD+':TEXTURES\KASTET');
  g_Texture_CreateWADEx('ITEM_MEDKIT_BLACK', GameWAD+':TEXTURES\BMED');

  InitTextures();

  freeIds := TIdPool.Create();
end;


procedure g_Items_FreeData();
begin
  e_WriteLog('Releasing items data...', TMsgType.Notify);

  g_Sound_Delete('SOUND_ITEM_RESPAWNITEM');
  g_Sound_Delete('SOUND_ITEM_GETRULEZ');
  g_Sound_Delete('SOUND_ITEM_GETWEAPON');
  g_Sound_Delete('SOUND_ITEM_GETITEM');

  g_Frames_DeleteByName('FRAMES_ITEM_BLUESPHERE');
  g_Frames_DeleteByName('FRAMES_ITEM_WHITESPHERE');
  g_Frames_DeleteByName('FRAMES_ITEM_ARMORGREEN');
  g_Frames_DeleteByName('FRAMES_ITEM_ARMORBLUE');
  g_Frames_DeleteByName('FRAMES_ITEM_JETPACK');
  g_Frames_DeleteByName('FRAMES_ITEM_INVUL');
  g_Frames_DeleteByName('FRAMES_ITEM_INVIS');
  g_Frames_DeleteByName('FRAMES_ITEM_RESPAWN');
  g_Frames_DeleteByName('FRAMES_ITEM_BOTTLE');
  g_Frames_DeleteByName('FRAMES_ITEM_HELMET');
  g_Frames_DeleteByName('FRAMES_FLAG_RED');
  g_Frames_DeleteByName('FRAMES_FLAG_BLUE');
  g_Frames_DeleteByName('FRAMES_FLAG_DOM');
  g_Texture_Delete('ITEM_MEDKIT_SMALL');
  g_Texture_Delete('ITEM_MEDKIT_LARGE');
  g_Texture_Delete('ITEM_WEAPON_SAW');
  g_Texture_Delete('ITEM_WEAPON_PISTOL');
  g_Texture_Delete('ITEM_WEAPON_KASTET');
  g_Texture_Delete('ITEM_WEAPON_SHOTGUN1');
  g_Texture_Delete('ITEM_WEAPON_SHOTGUN2');
  g_Texture_Delete('ITEM_WEAPON_CHAINGUN');
  g_Texture_Delete('ITEM_WEAPON_ROCKETLAUNCHER');
  g_Texture_Delete('ITEM_WEAPON_PLASMA');
  g_Texture_Delete('ITEM_WEAPON_BFG');
  g_Texture_Delete('ITEM_WEAPON_SUPERPULEMET');
  g_Texture_Delete('ITEM_WEAPON_FLAMETHROWER');
  g_Texture_Delete('ITEM_AMMO_BULLETS');
  g_Texture_Delete('ITEM_AMMO_BULLETS_BOX');
  g_Texture_Delete('ITEM_AMMO_SHELLS');
  g_Texture_Delete('ITEM_AMMO_SHELLS_BOX');
  g_Texture_Delete('ITEM_AMMO_ROCKET');
  g_Texture_Delete('ITEM_AMMO_ROCKET_BOX');
  g_Texture_Delete('ITEM_AMMO_CELL');
  g_Texture_Delete('ITEM_AMMO_CELL_BIG');
  g_Texture_Delete('ITEM_AMMO_FUELCAN');
  g_Texture_Delete('ITEM_AMMO_BACKPACK');
  g_Texture_Delete('ITEM_KEY_RED');
  g_Texture_Delete('ITEM_KEY_GREEN');
  g_Texture_Delete('ITEM_KEY_BLUE');
  g_Texture_Delete('ITEM_OXYGEN');
  g_Texture_Delete('ITEM_SUIT');
  g_Texture_Delete('ITEM_WEAPON_KASTET');
  g_Texture_Delete('ITEM_MEDKIT_BLACK');

  freeIds.Free();
  freeIds := nil;
end;


procedure releaseItem (idx: Integer);
var
  it: PItem;
begin
  if (idx < 0) or (idx > High(ggItems)) then raise Exception.Create('releaseItem: invalid item id');
  if not freeIds.hasAlloced[LongWord(idx)] then raise Exception.Create('releaseItem: trying to release unallocated item (0)');
  it := @ggItems[idx];
  if not it.slotIsUsed then raise Exception.Create('releaseItem: trying to release unallocated item (1)');
  if (it.arrIdx <> idx) then raise Exception.Create('releaseItem: arrIdx inconsistency');
  it.slotIsUsed := false;
  if (it.Animation <> nil) then
  begin
    it.Animation.Free();
    it.Animation := nil;
  end;
  it.alive := False;
  it.SpawnTrigger := -1;
  it.ItemType := ITEM_NONE;
  it.NeedSend := false;
  freeIds.release(LongWord(idx));
end;


procedure growItemArrayTo (newsz: Integer);
var
  i, olen: Integer;
  it: PItem;
begin
  if (newsz < Length(ggItems)) then exit;
  // no free slots
  olen := Length(ggItems);
  SetLength(ggItems, newsz);
  for i := olen to High(ggItems) do
  begin
    it := @ggItems[i];
    it.slotIsUsed := false;
    it.arrIdx := i;
    it.ItemType := ITEM_NONE;
    it.Animation := nil;
    it.alive := false;
    it.SpawnTrigger := -1;
    it.Respawnable := false;
    it.NeedSend := false;
    //if not freeIds.hasFree[LongWord(i)] then raise Exception.Create('internal error in item idx manager');
  end;
end;


function allocItem (): DWORD;
begin
  result := freeIds.alloc();
  if (result >= Length(ggItems)) then growItemArrayTo(Integer(result)+64);
  if (Integer(result) > High(ggItems)) then raise Exception.Create('allocItem: freeid list corrupted');
  if (ggItems[result].arrIdx <> Integer(result)) then raise Exception.Create('allocItem: arrIdx inconsistency');
end;


// it will be slow if the slot is free (we have to rebuild the heap)
function wantItemSlot (slot: Integer): Integer;
var
  olen: Integer;
  it: PItem;
begin
  if (slot < 0) or (slot > $0fffffff) then raise Exception.Create('wantItemSlot: bad item slot request');
  // do we need to grow item storate?
  olen := Length(ggItems);
  if (slot >= olen) then growItemArrayTo(slot+64);

  it := @ggItems[slot];
  if not it.slotIsUsed then
  begin
    freeIds.alloc(LongWord(slot));
  end
  else
  begin
    if not freeIds.hasAlloced[slot] then raise Exception.Create('wantItemSlot: internal error in item idx manager');
  end;
  it.slotIsUsed := false;

  result := slot;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_Items_Init ();
var
  a, b: Integer;
begin
  if gMapInfo.Height > gPlayerScreenSize.Y then a := gMapInfo.Height-gPlayerScreenSize.Y else a := gMapInfo.Height;
  if gMapInfo.Width > gPlayerScreenSize.X then b := gMapInfo.Width-gPlayerScreenSize.X else b := gMapInfo.Width;
  gMaxDist := Trunc(Hypot(a, b));
end;


procedure g_Items_Free ();
var
  i: Integer;
begin
  if (ggItems <> nil) then
  begin
    for i := 0 to High(ggItems) do ggItems[i].Animation.Free();
    ggItems := nil;
  end;
  freeIds.clear();
end;


function g_Items_Create (X, Y: Integer; ItemType: Byte;
           Fall, Respawnable: Boolean; AdjCoord: Boolean = False; ForcedID: Integer = -1): DWORD;
var
  find_id: DWORD;
  ID: DWORD;
  it: PItem;
begin
  if ForcedID < 0 then find_id := allocItem() else find_id := wantItemSlot(ForcedID);

  //{$IF DEFINED(D2F_DEBUG)}e_WriteLog(Format('allocated item #%d', [Integer(find_id)]), MSG_NOTIFY);{$ENDIF}

  it := @ggItems[find_id];

  if (it.arrIdx <> Integer(find_id)) then raise Exception.Create('g_Items_Create: arrIdx inconsistency');
  //it.arrIdx := find_id;
  it.slotIsUsed := true;

  it.ItemType := ItemType;
  it.Respawnable := Respawnable;
  it.InitX := X;
  it.InitY := Y;
  it.RespawnTime := 0;
  it.Fall := Fall;
  it.alive := True;
  it.QuietRespawn := False;
  it.dropped := false;
  it.NeedSend := false;

  g_Obj_Init(@it.Obj);
  it.Obj.X := X;
  it.Obj.Y := Y;
  it.Obj.Rect.Width := ITEMSIZE[ItemType][0];
  it.Obj.Rect.Height := ITEMSIZE[ItemType][1];

  it.Animation := nil;
  it.SpawnTrigger := -1;

  // Координаты относительно центра нижнего ребра
  if AdjCoord then
  begin
    with it^ do
    begin
      Obj.X := X - (Obj.Rect.Width div 2);
      Obj.Y := Y - Obj.Rect.Height;
      InitX := Obj.X;
      InitY := Obj.Y;
    end;
  end;

  it.Obj.oldX := it.Obj.X;
  it.Obj.oldY := it.Obj.Y;

  // Установка анимации
  case it.ItemType of
    ITEM_ARMOR_GREEN: if g_Frames_Get(ID, 'FRAMES_ITEM_ARMORGREEN') then it.Animation := TAnimation.Create(ID, True, 20);
    ITEM_ARMOR_BLUE: if g_Frames_Get(ID, 'FRAMES_ITEM_ARMORBLUE') then it.Animation := TAnimation.Create(ID, True, 20);
    ITEM_JETPACK: if g_Frames_Get(ID, 'FRAMES_ITEM_JETPACK') then it.Animation := TAnimation.Create(ID, True, 15);
    ITEM_SPHERE_BLUE: if g_Frames_Get(ID, 'FRAMES_ITEM_BLUESPHERE') then it.Animation := TAnimation.Create(ID, True, 15);
    ITEM_SPHERE_WHITE: if g_Frames_Get(ID, 'FRAMES_ITEM_WHITESPHERE') then it.Animation := TAnimation.Create(ID, True, 20);
    ITEM_INVUL: if g_Frames_Get(ID, 'FRAMES_ITEM_INVUL') then it.Animation := TAnimation.Create(ID, True, 20);
    ITEM_INVIS: if g_Frames_Get(ID, 'FRAMES_ITEM_INVIS') then it.Animation := TAnimation.Create(ID, True, 20);
    ITEM_BOTTLE: if g_Frames_Get(ID, 'FRAMES_ITEM_BOTTLE') then it.Animation := TAnimation.Create(ID, True, 20);
    ITEM_HELMET: if g_Frames_Get(ID, 'FRAMES_ITEM_HELMET') then it.Animation := TAnimation.Create(ID, True, 20);
  end;

  it.positionChanged();

  result := find_id;
end;

procedure g_Items_PreUpdate ();
var
  i: Integer;
begin
  if (ggItems = nil) then Exit;
  for i := 0 to High(ggItems) do
    if (ggItems[i].ItemType <> ITEM_NONE) and ggItems[i].slotIsUsed then
    begin
      ggItems[i].Obj.oldX := ggItems[i].Obj.X;
      ggItems[i].Obj.oldY := ggItems[i].Obj.Y;
    end;
end;

procedure g_Items_Update ();
var
  i, j, k: Integer;
  ID: DWord;
  Anim: TAnimation;
  m, ItemRespawnTime: Word;
  r, nxt: Boolean;
begin
  if (ggItems = nil) then exit;

  // respawn items in 15 seconds regardless of settings during warmup
  ItemRespawnTime := IfThen(gLMSRespawn = LMS_RESPAWN_NONE, gGameSettings.ItemRespawnTime, 15);

  for i := 0 to High(ggItems) do
  begin
    if (ggItems[i].ItemType = ITEM_NONE) then continue;
    if not ggItems[i].slotIsUsed then continue; // just in case

    with ggItems[i] do
    begin
      nxt := False;

      if alive then
      begin
        if Fall then
        begin
          m := g_Obj_Move(@Obj, True, True);
          positionChanged(); // this updates spatial accelerators

          // Сопротивление воздуха
          if gTime mod (GAME_TICK*2) = 0 then Obj.Vel.X := z_dec(Obj.Vel.X, 1);

          // Если выпал за карту
          if WordBool(m and MOVE_FALLOUT) then
          begin
            if SpawnTrigger = -1 then
            begin
              g_Items_Pick(i);
            end
            else
            begin
              g_Items_Remove(i);
              if g_Game_IsServer and g_Game_IsNet then MH_SEND_ItemDestroy(True, i);
            end;
            continue;
          end;
        end;

        // Если игроки поблизости
        if (gPlayers <> nil) then
        begin
          j := Random(Length(gPlayers))-1;

          for k := 0 to High(gPlayers) do
          begin
            Inc(j);
            if j > High(gPlayers) then j := 0;

            if (gPlayers[j] <> nil) and gPlayers[j].alive and g_Obj_Collide(@gPlayers[j].Obj, @Obj) then
            begin
              if g_Game_IsClient then continue;

              if not gPlayers[j].PickItem(ItemType, Respawnable, r) then continue;

              if g_Game_IsNet then MH_SEND_PlayerStats(gPlayers[j].UID);

              {
                Doom 2D: Original:
                1. I_NONE,I_CLIP,I_SHEL,I_ROCKET,I_CELL,I_AMMO,I_SBOX,I_RBOX,I_CELP,I_BPACK,I_CSAW,I_SGUN,I_SGUN2,I_MGUN,I_LAUN,I_PLAS,I_BFG,I_GUN2
                +2. I_MEGA,I_INVL,I_SUPER
                3. I_STIM,I_MEDI,I_ARM1,I_ARM2,I_AQUA,I_KEYR,I_KEYG,I_KEYB,I_SUIT,I_RTORCH,I_GTORCH,I_BTORCH,I_GOR1,I_FCAN
              }
              g_Items_EmitPickupSoundAt(i, gPlayers[j].Obj.X, gPlayers[j].Obj.Y);

              // Надо убрать с карты, если это не ключ, которым нужно поделиться с другим игроком
              if r then
              begin
                if not (Respawnable and (ItemRespawnTime > 0)) then
                  g_Items_Remove(i)
                else 
                  g_Items_Pick(i);
                if g_Game_IsNet then MH_SEND_ItemDestroy(False, i);
                nxt := True;
                break;
              end;
            end;
          end;
        end;

        if nxt then continue;
      end;

      if Respawnable and g_Game_IsServer then
      begin
        DecMin(RespawnTime, 0);
        if (RespawnTime = 0) and (not alive) then
        begin
          if not QuietRespawn then g_Sound_PlayExAt('SOUND_ITEM_RESPAWNITEM', InitX, InitY);

          if g_Frames_Get(ID, 'FRAMES_ITEM_RESPAWN') then
          begin
            Anim := TAnimation.Create(ID, False, 4);
            g_GFX_OnceAnim(InitX+(Obj.Rect.Width div 2)-16, InitY+(Obj.Rect.Height div 2)-16, Anim);
            Anim.Free();
          end;

          Obj.oldX := InitX;
          Obj.oldY := InitY;
          Obj.X := InitX;
          Obj.Y := InitY;
          Obj.Vel.X := 0;
          Obj.Vel.Y := 0;
          Obj.Accel.X := 0;
          Obj.Accel.Y := 0;
          positionChanged(); // this updates spatial accelerators

          alive := true;

          if g_Game_IsNet then MH_SEND_ItemSpawn(QuietRespawn, i);
          QuietRespawn := false;
        end;
      end;

      if (Animation <> nil) then Animation.Update();
    end;
  end;
end;


procedure itemsDrawInternal (dropflag: Boolean);
var
  i, fX, fY: Integer;
  it: PItem;
begin
  if (ggItems = nil) then exit;

  for i := 0 to High(ggItems) do
  begin
    it := @ggItems[i];
    if (not it.slotIsUsed) or (it.ItemType = ITEM_NONE) then continue; // just in case
    if not it.alive then continue;
    if (it.dropped <> dropflag) then continue;

    with it^ do
    begin
      if g_Collide(Obj.X, Obj.Y, Obj.Rect.Width, Obj.Rect.Height, sX, sY, sWidth, sHeight) then
      begin
        Obj.lerp(gLerpFactor, fX, fY);
        if (Animation = nil) then
        begin
          e_Draw(gItemsTexturesID[ItemType], fX, fY, 0, true, false);
        end
        else
        begin
          Animation.Draw(fX, fY, TMirrorType.None);
        end;

        if g_debug_Frames then
        begin
          e_DrawQuad(Obj.X+Obj.Rect.X,
                     Obj.Y+Obj.Rect.Y,
                     Obj.X+Obj.Rect.X+Obj.Rect.Width-1,
                     Obj.Y+Obj.Rect.Y+Obj.Rect.Height-1,
                     0, 255, 0);
        end;
      end;
    end;
  end;
end;


procedure g_Items_Draw ();
begin
  itemsDrawInternal(false);
end;

procedure g_Items_DrawDrop ();
begin
  itemsDrawInternal(true);
end;


procedure g_Items_SetDrop (ID: DWORD);
begin
  if (ID < Length(ggItems)) then
  begin
    ggItems[ID].dropped := true;
  end;
end;


procedure g_Items_Pick (ID: DWORD);
begin
  if (ID < Length(ggItems)) then
  begin
    ggItems[ID].Obj.oldX := ggItems[ID].Obj.X;
    ggItems[ID].Obj.oldY := ggItems[ID].Obj.Y;
    ggItems[ID].alive := false;
    ggItems[ID].RespawnTime := IfThen(gLMSRespawn = LMS_RESPAWN_NONE, gGameSettings.ItemRespawnTime, 15) * 36;

    if ((gGameSettings.Options and GAME_OPTION_RULEZRANDOM) = 0) then // Random powerup respawn
    begin
      if ggItems[ID].ItemType in [ITEM_SPHERE_BLUE, ITEM_SPHERE_WHITE, ITEM_INVUL,
                                ITEM_INVIS, ITEM_MEDKIT_BLACK, ITEM_JETPACK] then
      begin
        ggItems[ID].RespawnTime := Random(gGameSettings.RulezTimeMultiplier) * 36;
        //e_logwritefln ('Randomized number', []);
      end;
    end;
  end;
end;

procedure g_Items_Remove (ID: DWORD);
var
  it: PItem;
  trig: Integer;
begin
  if not g_Items_ValidId(ID) then
  begin
    //writeln('g_Items_Remove: invalid item id: ', ID);
    raise Exception.Create('g_Items_Remove: invalid item id');
    //exit;
  end;

  it := @ggItems[ID];
  if (it.arrIdx <> Integer(ID)) then raise Exception.Create('g_Items_Remove: arrIdx desync');

  it.Obj.oldX := it.Obj.X;
  it.Obj.oldY := it.Obj.Y;
  trig := it.SpawnTrigger;

  releaseItem(ID);

  if (trig > -1) then g_Triggers_DecreaseSpawner(trig);
end;


procedure g_Items_SaveState (st: TStream);
var
  count, i: Integer;
  tt: Byte;
begin
  // Считаем количество существующих предметов
  count := 0;
  for i := 0 to High(ggItems) do if (ggItems[i].ItemType <> ITEM_NONE) and (ggItems[i].slotIsUsed) then Inc(count);

  // Количество предметов
  utils.writeInt(st, LongInt(count));
  if (count = 0) then exit;

  for i := 0 to High(ggItems) do
  begin
    if (ggItems[i].ItemType <> ITEM_NONE) and (ggItems[i].slotIsUsed) then
    begin
      // Сигнатура предмета
      utils.writeSign(st, 'ITEM');
      utils.writeInt(st, Byte(0));
      // Тип предмета
      tt := ggItems[i].ItemType;
      if ggItems[i].dropped then tt := tt or $80;
      utils.writeInt(st, Byte(tt));
      // Есть ли респаун
      utils.writeBool(st, ggItems[i].Respawnable);
      // Координаты респуна
      utils.writeInt(st, LongInt(ggItems[i].InitX));
      utils.writeInt(st, LongInt(ggItems[i].InitY));
      // Время до респауна
      utils.writeInt(st, Word(ggItems[i].RespawnTime));
      // Существует ли этот предмет
      utils.writeBool(st, ggItems[i].alive);
      // Может ли он падать
      utils.writeBool(st, ggItems[i].Fall);
      // Индекс триггера, создавшего предмет
      utils.writeInt(st, LongInt(ggItems[i].SpawnTrigger));
      // Объект предмета
      Obj_SaveState(st, @ggItems[i].Obj);
    end;
  end;
end;


procedure g_Items_LoadState (st: TStream);
var
  count, i, a: Integer;
  b: Byte;
begin
  assert(st <> nil);

  g_Items_Free();

  // Количество предметов
  count := utils.readLongInt(st);
  if (count = 0) then exit;
  if (count < 0) or (count > 1024*1024) then raise XStreamError.Create('invalid number of items');

  for a := 0 to count-1 do
  begin
    // Сигнатура предмета
    if not utils.checkSign(st, 'ITEM') then raise XStreamError.Create('invalid item signature');
    if (utils.readByte(st) <> 0) then raise XStreamError.Create('invalid item version');
    // Тип предмета
    b := utils.readByte(st); // bit7=1: monster drop
    // Создаем предмет
    i := g_Items_Create(0, 0, b and $7F, False, False);
    if ((b and $80) <> 0) then g_Items_SetDrop(i);
    // Есть ли респаун
    ggItems[i].Respawnable := utils.readBool(st);
    // Координаты респуна
    ggItems[i].InitX := utils.readLongInt(st);
    ggItems[i].InitY := utils.readLongInt(st);
    // Время до респауна
    ggItems[i].RespawnTime := utils.readWord(st);
    // Существует ли этот предмет
    ggItems[i].alive := utils.readBool(st);
    // Может ли он падать
    ggItems[i].Fall := utils.readBool(st);
    // Индекс триггера, создавшего предмет
    ggItems[i].SpawnTrigger := utils.readLongInt(st);
    // Объект предмета
    Obj_LoadState(@ggItems[i].Obj, st);
  end;
end;


procedure g_Items_RestartRound ();
var
  i: Integer;
  it: PItem;
begin
  for i := 0 to High(ggItems) do
  begin
    it := @ggItems[i];
    it.Obj.oldX := it.Obj.X;
    it.Obj.oldY := it.Obj.Y;
    if not it.slotIsUsed then continue;
    if it.Respawnable and (it.ItemType <> ITEM_NONE) then
    begin
      it.QuietRespawn := True;
      it.RespawnTime := 0;
    end
    else
    begin
      g_Items_Remove(i);
      if g_Game_IsNet then MH_SEND_ItemDestroy(True, i);
    end;
  end;
end;


function g_Items_ForEachAlive (cb: TItemEachAliveCB; backwards: Boolean=false): Boolean;
var
  idx: Integer;
begin
  result := false;
  if (ggItems = nil) or not assigned(cb) then exit;

  if backwards then
  begin
    for idx := High(ggItems) downto 0 do
    begin
      if ggItems[idx].alive and ggItems[idx].slotIsUsed then
      begin
        result := cb(@ggItems[idx]);
        if result then exit;
      end;
    end;
  end
  else
  begin
    for idx := 0 to High(ggItems) do
    begin
      if ggItems[idx].alive and ggItems[idx].slotIsUsed then
      begin
        result := cb(@ggItems[idx]);
        if result then exit;
      end;
    end;
  end;
end;

function g_Items_NextAlive (startIdx: Integer): PItem;
var
  idx: Integer;
begin
  result := nil;
  if (ggItems = nil) or (startIdx >= High(ggItems)) then exit;
  for idx := startIdx + 1 to High(ggItems) do
    if ggItems[idx].alive and ggItems[idx].slotIsUsed then
    begin
      result := @ggItems[idx];
      exit;
    end;
end;

// ////////////////////////////////////////////////////////////////////////// //
procedure g_Items_EmitPickupSound (idx: Integer);
var
  it: PItem;
begin
  if not g_Items_ValidId(idx) then exit;
  it := @ggItems[idx];
  g_Items_EmitPickupSoundAt(idx, it.Obj.X, it.Obj.Y);
end;

procedure g_Items_EmitPickupSoundAt (idx, x, y: Integer);
var
  it: PItem;
begin
  if not g_Items_ValidId(idx) then exit;

  it := @ggItems[idx];
  if gSoundEffectsDF then
  begin
    if it.ItemType in [ITEM_SPHERE_BLUE, ITEM_SPHERE_WHITE, ITEM_INVUL,
                       ITEM_INVIS, ITEM_MEDKIT_BLACK, ITEM_JETPACK] then
    begin
      g_Sound_PlayExAt('SOUND_ITEM_GETRULEZ', x, y);
    end
    else if it.ItemType in [ITEM_WEAPON_SAW, ITEM_WEAPON_PISTOL, ITEM_WEAPON_SHOTGUN1, ITEM_WEAPON_SHOTGUN2,
                            ITEM_WEAPON_CHAINGUN, ITEM_WEAPON_ROCKETLAUNCHER, ITEM_WEAPON_PLASMA,
                            ITEM_WEAPON_BFG, ITEM_WEAPON_SUPERPULEMET, ITEM_WEAPON_FLAMETHROWER,
                            ITEM_AMMO_BACKPACK] then
    begin
      g_Sound_PlayExAt('SOUND_ITEM_GETWEAPON', x, y);
    end
    else
    begin
      g_Sound_PlayExAt('SOUND_ITEM_GETITEM', x, y);
    end;
  end
  else
  begin
    if it.ItemType in [ITEM_SPHERE_BLUE, ITEM_SPHERE_WHITE, ITEM_SUIT,
                       ITEM_MEDKIT_BLACK, ITEM_INVUL, ITEM_INVIS, ITEM_JETPACK] then
    begin
      g_Sound_PlayExAt('SOUND_ITEM_GETRULEZ', x, y);
    end
    else if it.ItemType in [ITEM_WEAPON_SAW, ITEM_WEAPON_PISTOL, ITEM_WEAPON_SHOTGUN1, ITEM_WEAPON_SHOTGUN2,
                            ITEM_WEAPON_CHAINGUN, ITEM_WEAPON_ROCKETLAUNCHER, ITEM_WEAPON_PLASMA,
                            ITEM_WEAPON_BFG, ITEM_WEAPON_SUPERPULEMET, ITEM_WEAPON_FLAMETHROWER] then
    begin
      g_Sound_PlayExAt('SOUND_ITEM_GETWEAPON', x, y);
    end
    else
    begin
      g_Sound_PlayExAt('SOUND_ITEM_GETITEM', x, y);
    end;
  end;
end;


procedure g_Items_AddDynLights();
var
  f: Integer;
  it: PItem;
begin
  for f := 0 to High(ggItems) do
  begin
    it := @ggItems[f];
    if not it.alive then continue;
    case it.ItemType of
      ITEM_KEY_RED: g_AddDynLight(it.Obj.X+(it.Obj.Rect.Width div 2), it.Obj.Y+(it.Obj.Rect.Height div 2), 24,  1.0, 0.0, 0.0, 0.6);
      ITEM_KEY_GREEN: g_AddDynLight(it.Obj.X+(it.Obj.Rect.Width div 2), it.Obj.Y+(it.Obj.Rect.Height div 2), 24,  0.0, 1.0, 0.0, 0.6);
      ITEM_KEY_BLUE: g_AddDynLight(it.Obj.X+(it.Obj.Rect.Width div 2), it.Obj.Y+(it.Obj.Rect.Height div 2), 24,  0.0, 0.0, 1.0, 0.6);
      ITEM_ARMOR_GREEN: g_AddDynLight(it.Obj.X+(it.Obj.Rect.Width div 2), it.Obj.Y+(it.Obj.Rect.Height div 2), 42,  0.0, 1.0, 0.0, 0.6);
      ITEM_ARMOR_BLUE: g_AddDynLight(it.Obj.X+(it.Obj.Rect.Width div 2), it.Obj.Y+(it.Obj.Rect.Height div 2), 42,  0.0, 0.0, 1.0, 0.6);
      ITEM_JETPACK: g_AddDynLight(it.Obj.X+(it.Obj.Rect.Width div 2), it.Obj.Y+(it.Obj.Rect.Height div 2), 32,  1.0, 1.0, 1.0, 0.6);
      ITEM_SPHERE_BLUE: g_AddDynLight(it.Obj.X+(it.Obj.Rect.Width div 2), it.Obj.Y+(it.Obj.Rect.Height div 2), 32,  0.0, 1.0, 0.0, 0.6);
      ITEM_SPHERE_WHITE: g_AddDynLight(it.Obj.X+(it.Obj.Rect.Width div 2), it.Obj.Y+(it.Obj.Rect.Height div 2), 32,  1.0, 1.0, 1.0, 0.6);
      ITEM_INVUL: g_AddDynLight(it.Obj.X+(it.Obj.Rect.Width div 2), it.Obj.Y+(it.Obj.Rect.Height div 2), 32,  1.0, 0.0, 0.0, 0.6);
      ITEM_INVIS: g_AddDynLight(it.Obj.X+(it.Obj.Rect.Width div 2), it.Obj.Y+(it.Obj.Rect.Height div 2), 32,  1.0, 1.0, 0.0, 0.6);
      ITEM_BOTTLE: g_AddDynLight(it.Obj.X+(it.Obj.Rect.Width div 2), it.Obj.Y+(it.Obj.Rect.Height div 2), 16,  0.0, 0.0, 0.8, 0.6);
      ITEM_HELMET: g_AddDynLight(it.Obj.X+(it.Obj.Rect.Width div 2), it.Obj.Y+(it.Obj.Rect.Height div 2), 16,  0.0, 0.8, 0.0, 0.6);
    end;
  end;
end;


end.
