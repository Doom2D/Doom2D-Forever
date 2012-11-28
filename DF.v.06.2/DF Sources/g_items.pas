unit g_items;

interface

uses windows, g_textures, g_phys, g_saveload;

type
  TItemSaveRec = packed record
   InitX, InitY: Integer;
   Obj: TObjRec;
   ItemType: Byte;
   RespawnTime: Word;
   Live: Boolean;
   Respawnable: Boolean;
   Fall: Boolean;
  end;

  TItem = record
   InitX, InitY:  Integer;
   Obj:           TObj;
   ItemType:      Byte;
   RespawnTime:   Word;
   Live:          Boolean;
   Respawnable:   Boolean;
   Fall:          Boolean;
   Animation:     TAnimation;
  end;

procedure g_Items_LoadData();
procedure g_Items_FreeData();
procedure g_Items_Init();
procedure g_Items_Free();
function g_Items_Create(X, Y: Integer; ItemType: Byte; Fall, Respawnable: Boolean): DWORD;
procedure g_Items_Update();
procedure g_Items_Draw();
procedure g_Items_Pick(ID: DWORD);
procedure g_Items_Remove(ID: DWORD);
function g_Items_Save(var p: Pointer): Integer;
procedure g_Items_Load(p: Pointer; len: Integer);

var
  gItems: array of TItem = nil;
  gItemsTexturesID: array [1..32] of DWORD;
  gMaxDist: Integer = 1;

const
   ITEM_RESPAWNTIME = 2160;

implementation

uses g_basic, e_graphics, g_sound, g_main, g_gfx, g_map, Math, g_game,
  g_console, SysUtils, g_player, MAPDEF, e_log;

const
  ITEMSIZE: array[1..30] of array[0..1] of Byte =
    (((13), (14)), ((28), (18)), ((28), (18)),
     ((32), (16)), ((32), (16)), ((24), (24)),
     ((24), (24)), ((23), (46)), ((8),  (28)),
     ((24), (24)), ((61), (23)), ((62), (11)),
     ((53), (12)), ((53), (16)), ((61), (16)),
     ((53), (16)), ((60), (35)), ((54), (16)),
     ((8),  (10)), ((27), (16)), ((14), (6)),
     ((31), (11)), ((11), (26)), ((53), (20)),
     ((14), (11)), ((32), (20)), ((21), (28)),
     ((16), (16)), ((16), (16)), ((16), (16)));

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
 g_Texture_Get('ITEM_AMMO_BULLETS',     gItemsTexturesID[ITEM_AMMO_BULLETS]);
 g_Texture_Get('ITEM_AMMO_BULLETS_BOX', gItemsTexturesID[ITEM_AMMO_BULLETS_BOX]);
 g_Texture_Get('ITEM_AMMO_SHELLS',      gItemsTexturesID[ITEM_AMMO_SHELLS]);
 g_Texture_Get('ITEM_AMMO_SHELLS_BOX',  gItemsTexturesID[ITEM_AMMO_SHELLS_BOX]);
 g_Texture_Get('ITEM_AMMO_ROCKET',      gItemsTexturesID[ITEM_AMMO_ROCKET]);
 g_Texture_Get('ITEM_AMMO_ROCKET_BOX',  gItemsTexturesID[ITEM_AMMO_ROCKET_BOX]);
 g_Texture_Get('ITEM_AMMO_CELL',        gItemsTexturesID[ITEM_AMMO_CELL]);
 g_Texture_Get('ITEM_AMMO_CELL_BIG',    gItemsTexturesID[ITEM_AMMO_CELL_BIG]);
 g_Texture_Get('ITEM_AMMO_BACKPACK',    gItemsTexturesID[ITEM_AMMO_BACKPACK]);
 g_Texture_Get('ITEM_KEY_RED',          gItemsTexturesID[ITEM_KEY_RED]);
 g_Texture_Get('ITEM_KEY_GREEN',        gItemsTexturesID[ITEM_KEY_GREEN]);
 g_Texture_Get('ITEM_KEY_BLUE',         gItemsTexturesID[ITEM_KEY_BLUE]);
 g_Texture_Get('ITEM_WEAPON_KASTET',    gItemsTexturesID[ITEM_WEAPON_KASTET]);
 g_Texture_Get('ITEM_WEAPON_PISTOL',    gItemsTexturesID[ITEM_WEAPON_PISTOL]);
end;

procedure g_Items_LoadData();
begin
 e_WriteLog('Loading items data...', MSG_NOTIFY);

 g_Sound_CreateWADEx('SOUND_ITEM_RESPAWNITEM', GameWAD+':SOUNDS\RESPAWNITEM');
 g_Sound_CreateWADEx('SOUND_ITEM_GETRULEZ', GameWAD+':SOUNDS\GETRULEZ');
 g_Sound_CreateWADEx('SOUND_ITEM_GETITEM', GameWAD+':SOUNDS\GETITEM');
 g_Sound_CreateWADEx('SOUND_ITEM_GETMED', GameWAD+':SOUNDS\GETMED');

 g_Frames_CreateWAD(nil, 'FRAMES_ITEM_BLUESPHERE', GameWAD+':TEXTURES\SBLUE', 32, 32, 4, True);
 g_Frames_CreateWAD(nil, 'FRAMES_ITEM_WHITESPHERE', GameWAD+':TEXTURES\SWHITE', 32, 32, 4, True);
 g_Frames_CreateWAD(nil, 'FRAMES_ITEM_ARMORGREEN', GameWAD+':TEXTURES\ARMORGREEN', 32, 16, 3, True);
 g_Frames_CreateWAD(nil, 'FRAMES_ITEM_ARMORBLUE', GameWAD+':TEXTURES\ARMORBLUE', 32, 16, 3, True);
 g_Frames_CreateWAD(nil, 'FRAMES_ITEM_INV', GameWAD+':TEXTURES\INV', 32, 32, 4, True);
 g_Frames_CreateWAD(nil, 'FRAMES_ITEM_RESPAWN', GameWAD+':TEXTURES\ITEMRESPAWN', 32, 32, 5, True);
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
 g_Texture_CreateWADEx('ITEM_AMMO_BULLETS', GameWAD+':TEXTURES\CLIP');
 g_Texture_CreateWADEx('ITEM_AMMO_BULLETS_BOX', GameWAD+':TEXTURES\AMMO');
 g_Texture_CreateWADEx('ITEM_AMMO_SHELLS', GameWAD+':TEXTURES\SHELL1');
 g_Texture_CreateWADEx('ITEM_AMMO_SHELLS_BOX', GameWAD+':TEXTURES\SHELL2');
 g_Texture_CreateWADEx('ITEM_AMMO_ROCKET', GameWAD+':TEXTURES\ROCKET');
 g_Texture_CreateWADEx('ITEM_AMMO_ROCKET_BOX', GameWAD+':TEXTURES\ROCKETS');
 g_Texture_CreateWADEx('ITEM_AMMO_CELL', GameWAD+':TEXTURES\CELL');
 g_Texture_CreateWADEx('ITEM_AMMO_CELL_BIG', GameWAD+':TEXTURES\CELL2');
 g_Texture_CreateWADEx('ITEM_AMMO_BACKPACK', GameWAD+':TEXTURES\BPACK');
 g_Texture_CreateWADEx('ITEM_KEY_RED', GameWAD+':TEXTURES\KEYR');
 g_Texture_CreateWADEx('ITEM_KEY_GREEN', GameWAD+':TEXTURES\KEYG');
 g_Texture_CreateWADEx('ITEM_KEY_BLUE', GameWAD+':TEXTURES\KEYB');
 g_Texture_CreateWADEx('ITEM_OXYGEN', GameWAD+':TEXTURES\OXYGEN');
 g_Texture_CreateWADEx('ITEM_SUIT', GameWAD+':TEXTURES\SUIT');
 g_Texture_CreateWADEx('ITEM_WEAPON_KASTET', GameWAD+':TEXTURES\KASTET');
 g_Texture_CreateWADEx('ITEM_MEDKIT_BLACK', GameWAD+':TEXTURES\BMED');

 InitTextures();
end;

procedure g_Items_FreeData();
begin
 e_WriteLog('Releasing items data...', MSG_NOTIFY);

 g_Sound_Delete('SOUND_ITEM_RESPAWNITEM');
 g_Sound_Delete('SOUND_ITEM_GETRULEZ');
 g_Sound_Delete('SOUND_ITEM_GETITEM');
 g_Sound_Delete('SOUND_ITEM_GETMED');

 g_Frames_DeleteByName('FRAMES_ITEM_BLUESPHERE');
 g_Frames_DeleteByName('FRAMES_ITEM_WHITESPHERE');
 g_Frames_DeleteByName('FRAMES_ITEM_ARMORGREEN');
 g_Frames_DeleteByName('FRAMES_ITEM_ARMORBLUE');
 g_Frames_DeleteByName('FRAMES_ITEM_INV');
 g_Frames_DeleteByName('FRAMES_ITEM_RESPAWN');
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
 g_Texture_Delete('ITEM_AMMO_BULLETS');
 g_Texture_Delete('ITEM_AMMO_BULLETS_BOX');
 g_Texture_Delete('ITEM_AMMO_SHELLS');
 g_Texture_Delete('ITEM_AMMO_SHELLS_BOX');
 g_Texture_Delete('ITEM_AMMO_ROCKET');
 g_Texture_Delete('ITEM_AMMO_ROCKET_BOX');
 g_Texture_Delete('ITEM_AMMO_CELL');
 g_Texture_Delete('ITEM_AMMO_CELL_BIG');
 g_Texture_Delete('ITEM_AMMO_BACKPACK');
 g_Texture_Delete('ITEM_KEY_RED');
 g_Texture_Delete('ITEM_KEY_GREEN');
 g_Texture_Delete('ITEM_KEY_BLUE');
 g_Texture_Delete('ITEM_OXYGEN');
 g_Texture_Delete('ITEM_SUIT');
 g_Texture_Delete('ITEM_WEAPON_KASTET');
 g_Texture_Delete('ITEM_MEDKIT_BLACK');
end;

function FindItem(): DWORD;
var
  i: Integer;
begin
 if gItems <> nil then
 for i := 0 to High(gItems) do
  if gItems[i].ItemType = ITEM_NONE then
  begin
   Result := i;
   Exit;
  end;

 if gItems = nil then
 begin
  SetLength(gItems, 32);
  Result := 0;
 end
  else
 begin
  Result := High(gItems) + 1;
  SetLength(gItems, Length(gItems) + 32);
 end;
end;

procedure g_Items_Init();
var
  a, b: Integer;
begin
 if gMapInfo.Height > gPlayerScreenSize.Y then
  a := gMapInfo.Height - gPlayerScreenSize.Y else a := gMapInfo.Height;

 if gMapInfo.Width > gPlayerScreenSize.X then
  b := gMapInfo.Width - gPlayerScreenSize.X else b := gMapInfo.Width;

 gMaxDist := Trunc(Hypot(a, b));
end;

procedure g_Items_Free();
var
  i: Integer;
begin
 if gItems <> nil then
 begin
  for i := 0 to High(gItems) do
   if gItems[i].Animation <> nil then gItems[i].Animation.Destroy;
  gItems := nil;
 end;
end;

function g_Items_Create(X, Y: Integer; ItemType: Byte; Fall, Respawnable: Boolean): DWORD;
var
  find_id: DWORD;
  ID: DWORD;
begin
 find_id := FindItem();
 g_Obj_Init(@gItems[find_id].Obj);
 gItems[find_id].InitX := X;
 gItems[find_id].InitY := Y;
 gItems[find_id].Obj.X := X;
 gItems[find_id].Obj.Y := Y;
 gItems[find_id].Obj.Rect.Width := ITEMSIZE[ItemType][0];
 gItems[find_id].Obj.Rect.Height := ITEMSIZE[ItemType][1];
 gItems[find_id].ItemType := ItemType;
 gItems[find_id].Respawnable := Respawnable;
 gItems[find_id].Fall := Fall;
 gItems[find_id].Live := True;
 gItems[find_id].Animation := nil;

 with gItems[find_id] do
 begin
  case ItemType of
   ITEM_ARMOR_GREEN: if g_Frames_Get(ID, 'FRAMES_ITEM_ARMORGREEN') then
                      Animation := TAnimation.Create(ID, True, 20);
   ITEM_ARMOR_BLUE: if g_Frames_Get(ID, 'FRAMES_ITEM_ARMORBLUE') then
                     Animation := TAnimation.Create(ID, True, 20);
   ITEM_SPHERE_BLUE: if g_Frames_Get(ID, 'FRAMES_ITEM_BLUESPHERE') then
                      Animation := TAnimation.Create(ID, True, 15);
   ITEM_SPHERE_WHITE: if g_Frames_Get(ID, 'FRAMES_ITEM_WHITESPHERE') then
                       Animation := TAnimation.Create(ID, True, 20);
   ITEM_INV: if g_Frames_Get(ID, 'FRAMES_ITEM_INV') then
              Animation := TAnimation.Create(ID, True, 20);
  end;
 end;

 Result := find_id;
end;

procedure g_Items_Update();
var
  i: Integer;
  ID: DWORD;
  Anim: TAnimation;
  m: Word;
begin
 if gItems <> nil then
 for i := 0 to High(gItems) do
  if gItems[i].ItemType <> ITEM_NONE then
   with gItems[i] do
   begin
    m := 0;
    
    if Live and Fall then
    begin
     m := g_Obj_Move(@Obj);
     if WordBool(m and MOVE_HITWATER) then g_Obj_Splash(@Obj);
     if gTime mod (GAME_TICK*2) = 0 then Obj.Vel.X := z_dec(Obj.Vel.X, 1);
    end;

    if WordBool(m and MOVE_FALLOUT) then
    begin
     g_Items_Pick(i);
     Continue;
    end;

    if Respawnable then
    begin
     DecMin(RespawnTime, 0);
     if (RespawnTime = 0) and not Live then
     begin
      g_Sound_PlayExAt('SOUND_ITEM_RESPAWNITEM', 255, InitX, InitY);

      if g_Frames_Get(ID, 'FRAMES_ITEM_RESPAWN') then
      begin
       Anim := TAnimation.Create(ID, False, 4);
       g_GFX_OnceAnim(InitX+(Obj.Rect.Width div 2)-16, InitY+(Obj.Rect.Height div 2)-16, Anim);
       Anim.Destroy;
      end;

      Obj.X := InitX;
      Obj.Y := InitY;
      Obj.Vel := _Point(0, 0);
      Obj.Accel := _Point(0, 0);

      Live := True;
     end;
    end;

    if Animation <> nil then Animation.Update;
   end;
end;

procedure g_Items_Draw();
var
  i: Integer;
begin
 if gItems <> nil then
  for i := 0 to High(gItems) do
   if gItems[i].Live then
    with gItems[i] do
     if g_Collide(Obj.X, Obj.Y, Obj.Rect.Width, Obj.Rect.Height, sX, sY, sWidth, sHeight) then
      if Animation = nil then e_Draw(gItemsTexturesID[ItemType], Obj.X, Obj.Y, 0, True, False)
       else Animation.Draw(Obj.X, Obj.Y, M_NONE);
end;

procedure g_Items_Pick(ID: DWORD);
begin
 gItems[ID].Live := False;
 gItems[ID].RespawnTime := ITEM_RESPAWNTIME;
end;

procedure g_Items_Remove(ID: DWORD);
begin
 gItems[ID].ItemType := ITEM_NONE;
 if gItems[ID].Animation <> nil then
 begin
  gItems[ID].Animation.Destroy;
  gItems[ID].Animation := nil;
 end;
 gItems[ID].Live := False;
end;

function g_Items_Save(var p: Pointer): Integer;
var
  a, b, i: Integer;
  item: TItemSaveRec;
begin
 Result := 0;
 b := 0;
 if gItems <> nil then
  for i := 0 to High(gItems) do
   if gItems[i].ItemType <> ITEM_NONE then b := b+1;

 if b = 0 then Exit;

 p := GetMemory(SizeOf(TItemSaveRec)*b);
 a := 0;

 for i := 0 to High(gItems) do
  if gItems[i].ItemType <> ITEM_NONE then
  begin
   item.InitX := gItems[i].InitX;
   item.InitY := gItems[i].InitY;
   saveobj(@gItems[i].Obj, @item.obj);
   item.ItemType := gItems[i].ItemType;
   item.RespawnTime := gItems[i].RespawnTime;
   item.Live := gItems[i].Live;
   item.Respawnable := gItems[i].Respawnable;
   item.Fall := gItems[i].Fall;

   CopyMemory(Pointer(Integer(p)+a*SizeOf(TItemSaveRec)), @item, SizeOf(TItemSaveRec));
   a := a+1;
  end;

 Result := SizeOf(TItemSaveRec)*b;
end;

procedure g_Items_Load(p: Pointer; len: Integer);
var
  a, b, c: Integer;
  item: TItemSaveRec;
begin
 g_Items_Free();

 c := len div SizeOf(TItemSaveRec);
 if c = 0 then Exit;

 for a := 0 to c-1 do
 begin
  CopyMemory(@item, Pointer(Integer(p)+a*SizeOf(TItemSaveRec)), SizeOf(TItemSaveRec));
  b := g_Items_Create(0, 0, item.ItemType, False, False);
  gItems[b].InitX := item.InitX;
  gItems[b].InitY := item.InitY;
  loadobj(@item.Obj, @gItems[b].Obj);
  gItems[b].ItemType := item.ItemType;
  gItems[b].RespawnTime := item.RespawnTime;
  gItems[b].Live := item.Live;
  gItems[b].Respawnable := item.Respawnable;
  gItems[b].Fall := item.Fall;
 end;
end;

end.
