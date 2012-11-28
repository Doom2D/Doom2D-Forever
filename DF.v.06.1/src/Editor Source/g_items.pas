unit g_items;

interface

uses
  windows,
  e_graphics,
  g_basic,
  g_textures;
type
  TItemType=(ITEM_NONE,
             ITEM_MEDKIT_SMALL, ITEM_MEDKIT_LARGE,
             ITEM_ARMOR_GREEN, ITEM_ARMOR_BLUE,
             ITEM_SPHERE_BLUE, ITEM_SPHERE_WHITE,
             ITEM_WEAPON_SAW, ITEM_WEAPON_SHOTGUN1, ITEM_WEAPON_SHOTGUN2,
             ITEM_WEAPON_CHAINGUN, ITEM_WEAPON_ROCKETLAUNCHER,
             ITEM_WEAPON_PLASMA, ITEM_WEAPON_BFG, ITEM_WEAPON_SUPERPULEMET,
             ITEM_AMMO_BULLETS, ITEM_AMMO_BULLETS_BOX, ITEM_AMMO_SHELLS,
             ITEM_AMMO_SHELLS_BOX, ITEM_AMMO_ROCKET, ITEM_AMMO_ROCKET_BOX,
             ITEM_AMMO_CELL, ITEM_AMMO_CELL_BIG, ITEM_AMMO_BACKPACK,
             ITEM_KEY_RED, ITEM_KEY_GREEN, ITEM_KEY_BLUE,
             ITEM_SUIT, ITEM_OXYGEN, ITEM_MEDKIT_BLACK, ITEM_INV);

  TItem = record
    GameX, GameY:  Integer;
    Width, Height: Integer;
    ItemType:      TItemType;
    RespawnTime:   Word;
    Live:          Boolean;
    Respawnable:   Boolean;
    Animation:     Boolean;
    ItemAnimation: TGameAnimationOpt;
  end;

  TItemSystem = class(TObject)
    ItemsArray: Array of TItem;
    Count:      Word;
    constructor Create;
    procedure   CreateItem(ItemType: TItemType; GameX, GameY: Integer);
    procedure   RemoveItem(ItemIndex: Integer);
    procedure   Render(XInc, YInc: Integer);
    function    GetCount: DWORD;
  private
    function    FindItem: Integer;
  end;

implementation

uses
  f_main;

function TItemSystem.FindItem: Integer;
var
  i: Integer;
begin
if ItemsArray <> nil then
for i := 0 to High(ItemsArray) do
 if ItemsArray[i].ItemType = ITEM_NONE then
 begin
  Result := i;
  Exit;
 end;

if ItemsArray = nil then
begin
 SetLength(ItemsArray, 1);
 result := 0;
end
 else
begin
 Result := High(ItemsArray) + 1;
 SetLength(ItemsArray, Length(ItemsArray) + 1);
end;
end;

procedure TItemSystem.CreateItem(ItemType: TItemType; GameX, GameY: Integer);
var
  find_id: Integer;
  ID: DWORD;
begin
find_id := FindItem;

case ItemType of
 ITEM_MEDKIT_SMALL:
  begin
   ItemsArray[find_id].Width := 13;
   ItemsArray[find_id].Height := 14;
  end;
 ITEM_MEDKIT_LARGE:
  begin
   ItemsArray[find_id].Width := 28;
   ItemsArray[find_id].Height := 18;
  end;
 ITEM_SPHERE_BLUE:
  begin
   g_GetAnimationTexture('ITEM_BLUESPHERE', ID);
   e_CreateAnimation(ID, ItemsArray[find_id].ItemAnimation.AnimationID); 
   ItemsArray[find_id].Animation := True;
   ItemsArray[find_id].ItemAnimation.AnimationSpeed := 15;
   ItemsArray[find_id].Width := 24;
   ItemsArray[find_id].Height := 24;
  end;
 ITEM_SPHERE_WHITE:
  begin
   g_GetAnimationTexture('ITEM_WHITESPHERE', ID);
   e_CreateAnimation(ID, ItemsArray[find_id].ItemAnimation.AnimationID);
   ItemsArray[find_id].Animation := True;
   ItemsArray[find_id].ItemAnimation.AnimationSpeed := 20;
   ItemsArray[find_id].Width := 24;
   ItemsArray[find_id].Height := 24;
  end;
 ITEM_WEAPON_SAW:
  begin
   ItemsArray[find_id].Width := 61;
   ItemsArray[find_id].Height := 23;
  end;
 ITEM_WEAPON_SHOTGUN1:
  begin
   ItemsArray[find_id].Width := 62;
   ItemsArray[find_id].Height := 11;
  end;
 ITEM_WEAPON_SHOTGUN2:
  begin
   ItemsArray[find_id].Width := 53;
   ItemsArray[find_id].Height := 12;
  end;
 ITEM_WEAPON_CHAINGUN:
  begin
   ItemsArray[find_id].Width := 53;
   ItemsArray[find_id].Height := 16;
  end;
 ITEM_WEAPON_ROCKETLAUNCHER:
  begin
   ItemsArray[find_id].Width := 61;
   ItemsArray[find_id].Height := 16;
  end;
 ITEM_WEAPON_PLASMA:
  begin
   ItemsArray[find_id].Width := 53;
   ItemsArray[find_id].Height := 16;
  end;
 ITEM_WEAPON_BFG:
  begin
   ItemsArray[find_id].Width := 60;
   ItemsArray[find_id].Height := 35;
  end;
 ITEM_WEAPON_SUPERPULEMET:
  begin
   ItemsArray[find_id].Width := 54;
   ItemsArray[find_id].Height := 16;
  end;
 ITEM_ARMOR_BLUE:
  begin
   g_GetAnimationTexture('ANIM_ITEM_ARMORBLUE', ID);
   e_CreateAnimation(ID, ItemsArray[find_id].ItemAnimation.AnimationID);
   ItemsArray[find_id].Animation := True;
   ItemsArray[find_id].ItemAnimation.AnimationSpeed := 20;
   ItemsArray[find_id].Width := 31;
   ItemsArray[find_id].Height := 16;
  end;
 ITEM_ARMOR_GREEN:
  begin
   g_GetAnimationTexture('ANIM_ITEM_ARMORGREEN', ID);
   e_CreateAnimation(ID, ItemsArray[find_id].ItemAnimation.AnimationID);
   ItemsArray[find_id].Animation := True;
   ItemsArray[find_id].ItemAnimation.AnimationSpeed := 20;
   ItemsArray[find_id].Width := 32;
   ItemsArray[find_id].Height := 16;
  end;
 ITEM_AMMO_BULLETS:
  begin
   ItemsArray[find_id].Width := 8;
   ItemsArray[find_id].Height := 10;
  end;
 ITEM_AMMO_BULLETS_BOX:
  begin
   ItemsArray[find_id].Width := 27;
   ItemsArray[find_id].Height := 16;
  end;
 ITEM_AMMO_SHELLS:
  begin
   ItemsArray[find_id].Width := 14;
   ItemsArray[find_id].Height := 6;
  end;
 ITEM_AMMO_SHELLS_BOX:
  begin
   ItemsArray[find_id].Width := 31;
   ItemsArray[find_id].Height := 11;
  end;
 ITEM_AMMO_ROCKET:
  begin
   ItemsArray[find_id].Width := 11;
   ItemsArray[find_id].Height := 26;
  end;
 ITEM_AMMO_ROCKET_BOX:
  begin
   ItemsArray[find_id].Width := 53;
   ItemsArray[find_id].Height := 20;
  end;
 ITEM_AMMO_CELL:
  begin
   ItemsArray[find_id].Width := 14;
   ItemsArray[find_id].Height := 11;
  end;
 ITEM_AMMO_CELL_BIG:
  begin
   ItemsArray[find_id].Width := 32;
   ItemsArray[find_id].Height := 20;
  end;
 ITEM_AMMO_BACKPACK:
  begin
   ItemsArray[find_id].Width := 21;
   ItemsArray[find_id].Height := 28;
  end;
 ITEM_KEY_RED:
  begin
   ItemsArray[find_id].Width := 16;
   ItemsArray[find_id].Height := 16;
  end;
 ITEM_KEY_GREEN:
  begin
   ItemsArray[find_id].Width := 16;
   ItemsArray[find_id].Height := 16;
  end;
 ITEM_KEY_BLUE:
  begin
   ItemsArray[find_id].Width := 16;
   ItemsArray[find_id].Height := 16;
  end;
 ITEM_SUIT:
  begin
   ItemsArray[find_id].Width := 23;
   ItemsArray[find_id].Height := 46;
  end;
 ITEM_OXYGEN:
  begin
   ItemsArray[find_id].Width := 8;
   ItemsArray[find_id].Height := 28;
  end;
 ITEM_MEDKIT_BLACK:
  begin
   ItemsArray[find_id].Width := 28;
   ItemsArray[find_id].Height := 18;
  end;
 ITEM_INV:
  begin
   g_GetAnimationTexture('ITEM_INV', ID);
   e_CreateAnimation(ID, ItemsArray[find_id].ItemAnimation.AnimationID);
   ItemsArray[find_id].Animation := True;
   ItemsArray[find_id].ItemAnimation.AnimationSpeed := 20;
   ItemsArray[find_id].Width := 24;
   ItemsArray[find_id].Height := 24;
  end;
end;

ItemsArray[find_id].ItemType := ItemType;
ItemsArray[find_id].GameX := GameX;
ItemsArray[find_id].GameY := GameY;
ItemsArray[find_id].Live := True;

Inc(Count);
end;

procedure TItemSystem.RemoveItem(ItemIndex: Integer);
begin
ZeroMemory(@ItemsArray[ItemIndex], SizeOf(TItem));

Dec(Count);
end;

procedure TItemSystem.Render(XInc, YInc: Integer);
var
  i: Integer;
  ID: DWORD;
begin
for i := 0 to High(ItemsArray) do
 if ItemsArray[i].Live then
 begin
  case ItemsArray[i].ItemType of
   ITEM_NONE: Continue;
   ITEM_MEDKIT_SMALL: g_GetTexture('ITEM_MEDKIT_SMALL', ID);
   ITEM_MEDKIT_LARGE: g_GetTexture('ITEM_MEDKIT_LARGE', ID);
   ITEM_ARMOR_GREEN: g_GetTexture('ITEM_ARMOR_GREEN', ID);
   ITEM_ARMOR_BLUE: g_GetTexture('ITEM_ARMOR_BLUE', ID);
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
   ITEM_KEY_BLUE: g_GetTexture('ITEM_KEY_BLUE', ID);
   ITEM_KEY_GREEN: g_GetTexture('ITEM_KEY_GREEN', ID);
   ITEM_OXYGEN: g_GetTexture('ITEM_OXYGEN', ID);
   ITEM_SUIT: g_GetTexture('ITEM_SUIT', ID);
   ITEM_MEDKIT_BLACK: g_GetTexture('ITEM_MEDKIT_BLACK', ID);
  end;

  if not(ItemsArray[i].Animation) then
   e_Draw(ID, ItemsArray[i].GameX + XInc,ItemsArray[i].GameY + YInc, 0, False, False)
  else
   e_DrawAnimation(ItemsArray[i].ItemAnimation.AnimationID, False, 0, False,
                   ItemsArray[i].GameX + XInc, ItemsArray[i].GameY + YInc);
 end;
end;

constructor TItemSystem.Create;
begin
g_CreateAnimationTextureFile('ITEM_BLUESPHERE', EditorDir+'\Data\graphics\items\SBLUE.dfa');
g_CreateAnimationTextureFile('ITEM_WHITESPHERE', EditorDir+'\Data\graphics\items\SWHITE.dfa');
g_CreateTextureFile('ITEM_MEDKIT_SMALL', EditorDir+'\Data\graphics\items\MED1.tga');
g_CreateTextureFile('ITEM_MEDKIT_LARGE', EditorDir+'\Data\graphics\items\MED2.tga');
g_CreateAnimationTextureFile('ANIM_ITEM_ARMORGREEN', EditorDir+'\Data\graphics\items\armorgreen.dfa');
g_CreateAnimationTextureFile('ANIM_ITEM_ARMORBLUE', EditorDir+'\Data\graphics\items\armorblue.dfa');
g_CreateAnimationTextureFile('ANIM_ITEM_INV', EditorDir+'\Data\graphics\items\inv.dfa');
g_CreateTextureFile('ITEM_WEAPON_SAW', EditorDir+'\Data\graphics\weapons\SAW.tga');
g_CreateTextureFile('ITEM_WEAPON_PISTOL', EditorDir+'\Data\graphics\weapons\PISTOL.tga');
g_CreateTextureFile('ITEM_WEAPON_SHOTGUN1', EditorDir+'\Data\graphics\weapons\SHOTGUN1.tga');
g_CreateTextureFile('ITEM_WEAPON_SHOTGUN2', EditorDir+'\Data\graphics\weapons\SHOTGUN2.tga');
g_CreateTextureFile('ITEM_WEAPON_CHAINGUN', EditorDir+'\Data\graphics\weapons\MGUN.tga');
g_CreateTextureFile('ITEM_WEAPON_ROCKETLAUNCHER', EditorDir+'\Data\graphics\weapons\RLAUNCHER.tga');
g_CreateTextureFile('ITEM_WEAPON_PLASMA', EditorDir+'\Data\graphics\weapons\PGUN.tga');
g_CreateTextureFile('ITEM_WEAPON_BFG', EditorDir+'\Data\graphics\weapons\BFG.tga');
g_CreateTextureFile('ITEM_WEAPON_SUPERPULEMET', EditorDir+'\Data\graphics\weapons\SPULEMET.tga');
g_CreateTextureFile('ITEM_AMMO_BULLETS', EditorDir+'\Data\graphics\weapons\CLIP.tga');
g_CreateTextureFile('ITEM_AMMO_BULLETS_BOX', EditorDir+'\Data\graphics\weapons\AMMO.tga');
g_CreateTextureFile('ITEM_AMMO_SHELLS', EditorDir+'\Data\graphics\weapons\SHELL1.tga');
g_CreateTextureFile('ITEM_AMMO_SHELLS_BOX', EditorDir+'\Data\graphics\weapons\SHELL2.tga');
g_CreateTextureFile('ITEM_AMMO_ROCKET', EditorDir+'\Data\graphics\weapons\ROCKET.tga');
g_CreateTextureFile('ITEM_AMMO_ROCKET_BOX', EditorDir+'\Data\graphics\weapons\ROCKETS.tga');
g_CreateTextureFile('ITEM_AMMO_CELL', EditorDir+'\Data\graphics\weapons\CELL.tga');
g_CreateTextureFile('ITEM_AMMO_CELL_BIG', EditorDir+'\Data\graphics\weapons\CELL2.tga');
g_CreateTextureFile('ITEM_AMMO_BACKPACK', EditorDir+'\Data\graphics\items\BPACK.tga');
g_CreateTextureFile('ITEM_KEY_RED', EditorDir+'\Data\graphics\items\KEYR.tga');
g_CreateTextureFile('ITEM_KEY_GREEN', EditorDir+'\Data\graphics\items\KEYG.tga');
g_CreateTextureFile('ITEM_KEY_BLUE', EditorDir+'\Data\graphics\items\KEYB.tga');
g_CreateTextureFile('ITEM_OXYGEN', EditorDir+'\Data\graphics\items\OXYGEN.tga');
g_CreateTextureFile('ITEM_SUIT', EditorDir+'\Data\graphics\items\SUIT.tga');
g_CreateTextureFile('ITEM_MEDKIT_BLACK', EditorDir+'\Data\graphics\items\BMED.tga');
g_CreateAnimationTextureFile('ITEM_INV', EditorDir+'\Data\graphics\items\inv.dfa');
end;

function TItemSystem.GetCount: DWORD;
var
  i: DWORD;
begin
 Result := 0;

 if ItemsArray <> nil then
 for i := 0 to High(ItemsArray) do
  if ItemsArray[i].ItemType <> ITEM_NONE then
   Inc(Result);
end;

end.
