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
{$INCLUDE ../shared/a_modes.inc}
unit g_items;

interface

uses
  g_textures, g_phys, g_saveload, BinEditor, MAPDEF;

Type
  PItem = ^TItem;
  TItem = record
  private
    treeNode: Integer;
    arrIdx: Integer; // in ggItems

  public
    ItemType:      Byte;
    Respawnable:   Boolean;
    InitX, InitY:  Integer;
    RespawnTime:   Word;
    Live:          Boolean;
    Fall:          Boolean;
    QuietRespawn:  Boolean;
    SpawnTrigger:  Integer;
    Obj:           TObj;
    Animation:     TAnimation;

    procedure positionChanged (); //WARNING! call this after monster position was changed, or coldet will not work right!

    property myid: Integer read arrIdx;
  end;

procedure g_Items_LoadData();
procedure g_Items_FreeData();
procedure g_Items_Init();
procedure g_Items_Free();
function g_Items_Create(X, Y: Integer; ItemType: Byte;
           Fall, Respawnable: Boolean; AdjCoord: Boolean = False; ForcedID: Integer = -1): DWORD;
procedure g_Items_Update();
procedure g_Items_Draw();
procedure g_Items_Pick(ID: DWORD);
procedure g_Items_Remove(ID: DWORD);
procedure g_Items_SaveState(var Mem: TBinMemoryWriter);
procedure g_Items_LoadState(var Mem: TBinMemoryReader);

procedure g_Items_RestartRound ();

function g_ItemValidId (idx: Integer): Boolean; inline;
function g_ItemByIdx (idx: Integer): PItem;
function g_ItemObjByIdx (idx: Integer): PObj;

procedure g_Item_EmitPickupSound (idx: Integer); // at item position
procedure g_Item_EmitPickupSoundAt (idx, x, y: Integer);


type
  TItemEachAliveCB = function (it: PItem): Boolean is nested; // return `true` to stop

function g_Items_ForEachAlive (cb: TItemEachAliveCB; backwards: Boolean=false): Boolean;


var
  gItemsTexturesID: Array [1..ITEM_MAX] of DWORD;
  gMaxDist: Integer = 1;
  ITEM_RESPAWNTIME: Integer = 60 * 36;

implementation

uses
  g_basic, e_graphics, g_sound, g_main, g_gfx, g_map,
  Math, g_game, g_triggers, g_console, SysUtils, g_player, g_net, g_netmsg,
  e_log,
  g_grid, z_aabbtree, binheap;


// ////////////////////////////////////////////////////////////////////////// //
var
  itemTree: TDynAABBTree = nil;
  ggItems: Array of TItem = nil;
  freeIds: TBinaryHeapInt = nil; // free item ids


// ////////////////////////////////////////////////////////////////////////// //
function g_ItemValidId (idx: Integer): Boolean; inline;
begin
  result := false;
  if (idx < 0) or (idx > High(ggItems)) then exit;
  if (ggItems[idx].treeNode = -1) then exit;
  result := true;
end;


function g_ItemByIdx (idx: Integer): PItem;
begin
  if (idx < 0) or (idx > High(ggItems)) then raise Exception.Create('g_ItemObjByIdx: invalid index');
  result := @ggItems[idx];
  if (result.treeNode = -1) then raise Exception.Create('g_ItemObjByIdx: requested inexistent item');
end;


function g_ItemObjByIdx (idx: Integer): PObj;
begin
  if (idx < 0) or (idx > High(ggItems)) then raise Exception.Create('g_ItemObjByIdx: invalid index');
  if (ggItems[idx].treeNode = -1) then raise Exception.Create('g_ItemObjByIdx: requested inexistent item');
  result := @ggItems[idx].Obj;
end;


// ////////////////////////////////////////////////////////////////////////// //
type
  TDynAABBTreeItem = class(TDynAABBTree)
    function getFleshAABB (var aabb: AABB2D; flesh: TTreeFlesh; tag: Integer): Boolean; override;
  end;

function TDynAABBTreeItem.getFleshAABB (var aabb: AABB2D; flesh: TTreeFlesh; tag: Integer): Boolean;
var
  it: PItem;
begin
  result := false;
  if (flesh = nil) then begin aabb := AABB2D.Create(0, 0, 0, 0); exit; end;
  //if not g_ItemValidId(tag) then raise Exception.Create('DynTree: trying to get dimensions of inexistant item');
  if (tag < 0) or (tag > High(ggItems)) then raise Exception.Create('DynTree: trying to get dimensions of inexistant item');
  it := @ggItems[tag];
  if (it.Obj.Rect.Width < 1) or (it.Obj.Rect.Height < 1) then exit;
  aabb := AABB2D.Create(it.Obj.X, it.Obj.Y, it.Obj.X+it.Obj.Rect.Width, it.Obj.Y+it.Obj.Rect.Height);
  if not aabb.valid then raise Exception.Create('wutafuuuuuuu?!');
  result := true;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TItem.positionChanged ();
var
  x, y: Integer;
begin
  if (treeNode = -1) then
  begin
    treeNode := itemTree.insertObject(itemTree{doesn't matter}, arrIdx, true); // static object
    itemTree.getNodeXY(treeNode, x, y);
    {$IF DEFINED(D2F_DEBUG)}e_WriteLog(Format('item #%d: inserted into the tree; nodeid=%d; x=%d; y=%d', [arrIdx, treeNode, x, y]), MSG_NOTIFY);{$ENDIF}
  end
  else
  begin
    itemTree.getNodeXY(treeNode, x, y);
    if (Obj.X = x) and (Obj.Y = y) then exit; // nothing to do
    {$IF DEFINED(D2F_DEBUG)}e_WriteLog(Format('item #%d: updating tree; nodeid=%d; x=%d; y=%d', [arrIdx, treeNode, x, y]), MSG_NOTIFY);{$ENDIF}

    {$IFDEF TRUE}
    itemTree.updateObject(treeNode);
    {$ELSE}
    itemTree.removeObject(treeNode);
    treeNode := itemTree.insertObject(itemTree{doesn't matter}, arrIdx, true); // static object
    {$ENDIF}

    itemTree.getNodeXY(treeNode, x, y);
    {$IF DEFINED(D2F_DEBUG)}e_WriteLog(Format('item #%d: updated tree; nodeid=%d; x=%d; y=%d', [arrIdx, treeNode, x, y]), MSG_NOTIFY);{$ENDIF}
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
  g_Texture_Get('ITEM_JETPACK',          gItemsTexturesID[ITEM_JETPACK]);
end;

procedure g_Items_LoadData();
begin
  e_WriteLog('Loading items data...', MSG_NOTIFY);

  g_Sound_CreateWADEx('SOUND_ITEM_RESPAWNITEM', GameWAD+':SOUNDS\RESPAWNITEM');
  g_Sound_CreateWADEx('SOUND_ITEM_GETRULEZ', GameWAD+':SOUNDS\GETRULEZ');
  g_Sound_CreateWADEx('SOUND_ITEM_GETWEAPON', GameWAD+':SOUNDS\GETWEAPON');
  g_Sound_CreateWADEx('SOUND_ITEM_GETITEM', GameWAD+':SOUNDS\GETITEM');

  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_BLUESPHERE', GameWAD+':TEXTURES\SBLUE', 32, 32, 4, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_WHITESPHERE', GameWAD+':TEXTURES\SWHITE', 32, 32, 4, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_ARMORGREEN', GameWAD+':TEXTURES\ARMORGREEN', 32, 16, 3, True);
  g_Frames_CreateWAD(nil, 'FRAMES_ITEM_ARMORBLUE', GameWAD+':TEXTURES\ARMORBLUE', 32, 16, 3, True);
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
  g_Texture_CreateWADEx('ITEM_JETPACK', GameWAD+':TEXTURES\JETPACK');

  InitTextures();

  itemTree := TDynAABBTreeItem.Create();
  freeIds := binHeapNewIntLess();
end;


procedure g_Items_FreeData();
begin
  e_WriteLog('Releasing items data...', MSG_NOTIFY);

  g_Sound_Delete('SOUND_ITEM_RESPAWNITEM');
  g_Sound_Delete('SOUND_ITEM_GETRULEZ');
  g_Sound_Delete('SOUND_ITEM_GETWEAPON');
  g_Sound_Delete('SOUND_ITEM_GETITEM');

  g_Frames_DeleteByName('FRAMES_ITEM_BLUESPHERE');
  g_Frames_DeleteByName('FRAMES_ITEM_WHITESPHERE');
  g_Frames_DeleteByName('FRAMES_ITEM_ARMORGREEN');
  g_Frames_DeleteByName('FRAMES_ITEM_ARMORBLUE');
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
  g_Texture_Delete('ITEM_JETPACK');

  itemTree.Free();
  freeIds.Free();
end;


procedure releaseItem (idx: Integer);
var
  it: PItem;
begin
  if (idx < 0) or (idx > High(ggItems)) then raise Exception.Create('releaseItem: invalid item id');
  it := @ggItems[idx];
  if (it.treeNode = -1) then raise Exception.Create('releaseItem: trying to release unallocated item');
  if (it.arrIdx <> idx) then raise Exception.Create('releaseItem: arrIdx inconsistency');
  itemTree.removeObject(it.treeNode);
  it.treeNode := -1;
  if (it.Animation <> nil) then
  begin
    it.Animation.Free();
    it.Animation := nil;
  end;
  it.Live := False;
  it.SpawnTrigger := -1;
  it.ItemType := ITEM_NONE;
  freeIds.insert(it.arrIdx);
end;


function allocItem (): DWORD;
var
  i, olen: Integer;
  it: PItem;
begin
  if (freeIds.count = 0) then
  begin
    // no free slots
    olen := Length(ggItems);
    SetLength(ggItems, olen+64);
    for i := olen to High(ggItems) do
    begin
      it := @ggItems[i];
      it.treeNode := -1;
      it.arrIdx := i;
      it.ItemType := ITEM_NONE;
      it.Animation := nil;
      it.Live := false;
      it.SpawnTrigger := -1;
      it.Respawnable := false;
      freeIds.insert(i);
    end;
  end;

  result := freeIds.front;
  freeIds.popFront();

  if (result > High(ggItems)) then raise Exception.Create('allocItem: freeid list corrupted');
  if (ggItems[result].arrIdx <> result) then raise Exception.Create('allocItem: arrIdx inconsistency');
end;


// it will be slow if the slot is free (we have to rebuild the heap)
function wantItemSlot (slot: Integer): Integer;
var
  i, olen: Integer;
  it: PItem;
  rebuildFreeList: Boolean = true;
begin
  if (slot < 0) or (slot > $0fffffff) then raise Exception.Create('wantItemSlot: bad item slot request');
  // do we need to grow item storate?
  olen := Length(ggItems);
  if (slot >= olen) then
  begin
    // need more spice!
    SetLength(ggItems, slot+64);
    // add free slots to free list
    for i := olen to High(ggItems) do
    begin
      it := @ggItems[i];
      it.treeNode := -1;
      it.arrIdx := i;
      it.ItemType := ITEM_NONE;
      it.Animation := nil;
      it.Live := false;
      it.SpawnTrigger := -1;
      it.Respawnable := false;
      if (i <> slot) then freeIds.insert(i);
    end;
    rebuildFreeList := false;
  end;

  it := @ggItems[slot];
  if (it.treeNode = -1) then
  begin
    // this is unused slot; get it, and rebuild id list
    if rebuildFreeList then
    begin
      freeIds.clear();
      for i := 0 to High(ggItems) do
      begin
        if (i <> slot) and (ggItems[i].treeNode = -1) then freeIds.insert(i);
      end;
    end;
  end
  else
  begin
    // it will be readded
    itemTree.removeObject(it.treeNode);
    it.treeNode := -1;
  end;

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
  if (itemTree <> nil) then itemTree.reset();
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

  {$IF DEFINED(D2F_DEBUG)}e_WriteLog(Format('allocated item #%d', [Integer(find_id)]), MSG_NOTIFY);{$ENDIF}

  it := @ggItems[find_id];

  it.ItemType := ItemType;
  it.Respawnable := Respawnable;
  if g_Game_IsServer and (ITEM_RESPAWNTIME = 0) then it.Respawnable := False;
  it.InitX := X;
  it.InitY := Y;
  it.RespawnTime := 0;
  it.Fall := Fall;
  it.Live := True;
  it.QuietRespawn := False;

  if (it.treeNode <> -1) then raise Exception.Create('g_Items_Create: trying to reuse already allocated item');
  if (it.arrIdx <> find_id) then raise Exception.Create('g_Items_Create: arrIdx inconsistency');
  //it.treeNode := -1;
  //it.arrIdx := find_id;

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

  // Установка анимации
  case it.ItemType of
    ITEM_ARMOR_GREEN: if g_Frames_Get(ID, 'FRAMES_ITEM_ARMORGREEN') then it.Animation := TAnimation.Create(ID, True, 20);
    ITEM_ARMOR_BLUE: if g_Frames_Get(ID, 'FRAMES_ITEM_ARMORBLUE') then it.Animation := TAnimation.Create(ID, True, 20);
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


procedure g_Items_Update ();
var
  i, j, k: Integer;
  ID: DWord;
  Anim: TAnimation;
  m: Word;
  r, nxt: Boolean;
begin
  if (ggItems = nil) then exit;

  for i := 0 to High(ggItems) do
  begin
    if (ggItems[i].ItemType = ITEM_NONE) then continue;

    with ggItems[i] do
    begin
      nxt := False;

      if Live then
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

            if (gPlayers[j] <> nil) and gPlayers[j].Live and g_Obj_Collide(@gPlayers[j].Obj, @Obj) then
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
              g_Item_EmitPickupSoundAt(i, gPlayers[j].Obj.X, gPlayers[j].Obj.Y);

              // Надо убрать с карты, если это не ключ, которым нужно поделиться с другим игроком
              if r then
              begin
                if not Respawnable then g_Items_Remove(i) else g_Items_Pick(i);
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
        if (RespawnTime = 0) and (not Live) then
        begin
          if not QuietRespawn then g_Sound_PlayExAt('SOUND_ITEM_RESPAWNITEM', InitX, InitY);

          if g_Frames_Get(ID, 'FRAMES_ITEM_RESPAWN') then
          begin
            Anim := TAnimation.Create(ID, False, 4);
            g_GFX_OnceAnim(InitX+(Obj.Rect.Width div 2)-16, InitY+(Obj.Rect.Height div 2)-16, Anim);
            Anim.Free();
          end;

          Obj.X := InitX;
          Obj.Y := InitY;
          Obj.Vel.X := 0;
          Obj.Vel.Y := 0;
          Obj.Accel.X := 0;
          Obj.Accel.Y := 0;
          positionChanged(); // this updates spatial accelerators

          Live := true;

          if g_Game_IsNet then MH_SEND_ItemSpawn(QuietRespawn, i);
          QuietRespawn := false;
        end;
      end;

      if (Animation <> nil) then Animation.Update();
    end;
  end;
end;


procedure g_Items_Draw ();
var
  i: Integer;
begin
  if (ggItems = nil) then exit;

  for i := 0 to High(ggItems) do
  begin
    if not ggItems[i].Live then continue;

    with ggItems[i] do
    begin
      if g_Collide(Obj.X, Obj.Y, Obj.Rect.Width, Obj.Rect.Height, sX, sY, sWidth, sHeight) then
      begin
        if (Animation = nil) then
        begin
          e_Draw(gItemsTexturesID[ItemType], Obj.X, Obj.Y, 0, true, false);
        end
        else
        begin
          Animation.Draw(Obj.X, Obj.Y, M_NONE);
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


procedure g_Items_Pick (ID: DWORD);
begin
  ggItems[ID].Live := false;
  ggItems[ID].RespawnTime := ITEM_RESPAWNTIME;
end;


procedure g_Items_Remove (ID: DWORD);
var
  it: PItem;
  trig: Integer;
  x, y: Integer;
begin
  if not g_ItemValidId(ID) then raise Exception.Create('g_Items_Remove: invalid item id');

  it := @ggItems[ID];
  if (it.arrIdx <> ID) then raise Exception.Create('g_Items_Remove: arrIdx desync');

  itemTree.getNodeXY(it.treeNode, x, y);
  {$IF DEFINED(D2F_DEBUG)}e_WriteLog(Format('removing item #%d: updating tree; nodeid=%d; x=%d; y=%d (%d,%d)', [it.arrIdx, it.treeNode, x, y, it.Obj.X, it.Obj.Y]), MSG_NOTIFY);{$ENDIF}

  trig := it.SpawnTrigger;

  releaseItem(ID);

  if (trig > -1) then g_Triggers_DecreaseSpawner(trig);
end;


procedure g_Items_SaveState (var Mem: TBinMemoryWriter);
var
  count, i: Integer;
  sig: DWORD;
begin
  // Считаем количество существующих предметов
  count := 0;
  if (ggItems <> nil) then
  begin
    for i := 0 to High(ggItems) do if (ggItems[i].ItemType <> ITEM_NONE) then Inc(count);
  end;

  Mem := TBinMemoryWriter.Create((count+1) * 60);

  // Количество предметов
  Mem.WriteInt(count);

  if (count = 0) then exit;

  for i := 0 to High(ggItems) do
  begin
    if (ggItems[i].ItemType <> ITEM_NONE) then
    begin
      // Сигнатура предмета
      sig := ITEM_SIGNATURE; // 'ITEM'
      Mem.WriteDWORD(sig);
      // Тип предмета
      Mem.WriteByte(ggItems[i].ItemType);
      // Есть ли респаун
      Mem.WriteBoolean(ggItems[i].Respawnable);
      // Координаты респуна
      Mem.WriteInt(ggItems[i].InitX);
      Mem.WriteInt(ggItems[i].InitY);
      // Время до респауна
      Mem.WriteWord(ggItems[i].RespawnTime);
      // Существует ли этот предмет
      Mem.WriteBoolean(ggItems[i].Live);
      // Может ли он падать
      Mem.WriteBoolean(ggItems[i].Fall);
      // Индекс триггера, создавшего предмет
      Mem.WriteInt(ggItems[i].SpawnTrigger);
      // Объект предмета
      Obj_SaveState(@ggItems[i].Obj, Mem);
    end;
  end;
end;


procedure g_Items_LoadState (var Mem: TBinMemoryReader);
var
  count, i, a: Integer;
  sig: DWORD;
  b: Byte;
begin
  if (Mem = nil) then exit;

  g_Items_Free();

  // Количество предметов
  Mem.ReadInt(count);

  if (count = 0) then Exit;

  for a := 0 to count-1 do
  begin
    // Сигнатура предмета
    Mem.ReadDWORD(sig);
    if (sig <> ITEM_SIGNATURE) then raise EBinSizeError.Create('g_Items_LoadState: Wrong Item Signature'); // 'ITEM'
    // Тип предмета
    Mem.ReadByte(b);
    // Создаем предмет
    i := g_Items_Create(0, 0, b, False, False);
    // Есть ли респаун
    Mem.ReadBoolean(ggItems[i].Respawnable);
    // Координаты респуна
    Mem.ReadInt(ggItems[i].InitX);
    Mem.ReadInt(ggItems[i].InitY);
    // Время до респауна
    Mem.ReadWord(ggItems[i].RespawnTime);
    // Существует ли этот предмет
    Mem.ReadBoolean(ggItems[i].Live);
    // Может ли он падать
    Mem.ReadBoolean(ggItems[i].Fall);
    // Индекс триггера, создавшего предмет
    Mem.ReadInt(ggItems[i].SpawnTrigger);
    // Объект предмета
    Obj_LoadState(@ggItems[i].Obj, Mem);
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
    if it.Respawnable then
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
      if ggItems[idx].Live then
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
      if ggItems[idx].Live then
      begin
        result := cb(@ggItems[idx]);
        if result then exit;
      end;
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure g_Item_EmitPickupSound (idx: Integer);
var
  it: PItem;
begin
  if not g_ItemValidId(idx) then exit;
  it := @ggItems[idx];
  g_Item_EmitPickupSoundAt(idx, it.Obj.X, it.Obj.Y);
end;

procedure g_Item_EmitPickupSoundAt (idx, x, y: Integer);
var
  it: PItem;
begin
  if not g_ItemValidId(idx) then exit;

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

end.
