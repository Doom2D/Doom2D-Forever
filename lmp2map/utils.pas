unit utils;

interface

type
  SArray = array of string;

function CollideN(X1, Y1: Integer; Width1, Height1: Word;
                 X2, Y2: Integer; Width2, Height2: Word): Boolean;
function Collide(X1, Y1: Integer; Width1, Height1: Word;
                 X2, Y2: Integer; Width2, Height2: Word): Boolean;
function parse(s: string): SArray;
procedure ProcessResourceStr(ResourceStr: string; FileName, SectionName,
                             ResourceName: PString);
function tostr(i: Integer): string;
procedure unpack(Buf: Pointer; Len: Integer; OutBuf: Pointer);
function GetPanel(OldType: Byte): Word;
function GetItem(OldType: Byte): Byte;
function GetMonster(OldType: Byte): Byte;
function GetArea(OldType: Byte): Byte;
function GetTrigger(OldType: Byte): Byte;
function GetBlockName(t: SmallInt): string;
function Min(a, b: Integer): Integer;

implementation

uses
  d2ddef, MAPDEF, MAP, windows;

function CollideN(X1, Y1: Integer; Width1, Height1: Word;
                 X2, Y2: Integer; Width2, Height2: Word): Boolean;
begin
 Result := not (((Y1 + Height1 < Y2) or
                (Y1           > Y2 + Height2)) or
                ((X1 + Width1 < X2) or
                (X1           > X2 + Width2)));
end;

function Collide(X1, Y1: Integer; Width1, Height1: Word;
                 X2, Y2: Integer; Width2, Height2: Word): Boolean;
begin
 Result := not (((Y1 + Height1 <= Y2) or
                (Y1           >= Y2 + Height2)) or
                ((X1 + Width1 <= X2) or
                (X1           >= X2 + Width2)));
end;

function parse(s: string): SArray;
var
  a: Integer;
begin
 Result := nil;
 if s = '' then Exit;

 while s <> '' do
 begin
  for a := 1 to Length(s) do
   if (s[a] = ',') or (a = Length(s)) then
   begin
    SetLength(Result, Length(Result)+1);

    if s[a] = ',' then Result[High(Result)] := Copy(s, 1, a-1)
     else Result[High(Result)] := s;

    Delete(s, 1, a);
    Break;
   end;
 end;
end;


procedure ProcessResourceStr(ResourceStr: string; FileName, SectionName,
                             ResourceName: PString);
var
  a, i, l1, l2: Integer;
begin
 for i := Length(ResourceStr) downto 1 do
  if ResourceStr[i] = ':' then Break;

 if FileName <> nil then
 begin
  FileName^ := Copy(ResourceStr, 1, i-1);
  l1 := Length(FileName^);
 end else l1 := 0;

 for a := i+1 to Length(ResourceStr) do
  if ResourceStr[a] = '\' then Break;

 if ResourceName <> nil then
 begin
  ResourceName^ := Copy(ResourceStr, a+1, Length(ResourceStr)-Abs(a));
  l2 := Length(ResourceName^);
 end else l2 := 0;

 if SectionName <> nil then
  SectionName^ := Copy(ResourceStr, i+1, Length(ResourceStr)-l2-l1-2);
end;

function tostr(i: Integer): string;
begin
 Str(i, Result);
end;

procedure unpack(Buf: Pointer; Len: Integer; OutBuf: Pointer);
type
  ByteArray = array[0..1] of Byte;
  PByteArray = ^ByteArray;

var
  p, o: Word;
  l, n: SmallInt;
begin
 o := 0;
 p := 0;
 l := Len;
 {$R-}
 while l > 0 do
 begin
  if PByteArray(Buf)^[p] = 255 then
  begin
   p := p+1;
   CopyMemory(@n, Pointer(Integer(Buf)+p), 2);
   p := p+2;
   FillMemory(Pointer(Integer(OutBuf)+o), n, PByteArray(Buf)^[p]);
   o := o+n;
   l := l-3;
  end
   else
  begin
   PByteArray(OutBuf)^[o] := PByteArray(Buf)^[p];
   o := o+1;
  end;

  p := p+1;
  l := l-1;
 end;
 {$R+}
end;

function Min(a, b: Integer): Integer;
begin
 if a < b then Result := a else Result := b;
end;

function GetPanel(OldType: Byte): Word;
begin
 case OldType of
  W_CLEAR: Result := PANEL_BACK;
  W_WALL: Result := PANEL_WALL;
  W_STEP: Result := PANEL_STEP;
  W_WATER: Result := PANEL_WATER;
  W_CLOSEDDOOR: Result := PANEL_CLOSEDOOR;
  W_OPENEDDOOR: Result := PANEL_OPENDOOR;
  W_ACID1: Result := PANEL_ACID1;
  W_ACID2: Result := PANEL_ACID2;
  W_BLOCKMON: Result := PANEL_BLOCKMON;
  W_LIFTUP: Result := PANEL_LIFTUP;
  W_LIFTDOWN: Result := PANEL_LIFTDOWN;
  else Result := PANEL_NONE;
 end;
end;

function GetItem(OldType: Byte): Byte;
begin
 case OldType of
  TH_CLIP: Result := ITEM_AMMO_BULLETS;
  TH_SHEL: Result := ITEM_AMMO_SHELLS;
  TH_ROCKET: Result := ITEM_AMMO_ROCKET;
  TH_CELL: Result := ITEM_AMMO_CELL;
  TH_AMMO: Result := ITEM_AMMO_BULLETS_BOX;
  TH_SBOX: Result := ITEM_AMMO_SHELLS_BOX;
  TH_RBOX: Result := ITEM_AMMO_ROCKET_BOX;
  TH_CELP: Result := ITEM_AMMO_CELL_BIG;
  TH_STIM: Result := ITEM_MEDKIT_SMALL;
  TH_MEDI: Result := ITEM_MEDKIT_LARGE;
  TH_BPACK: Result := ITEM_AMMO_BACKPACK;
  TH_CSAW: Result := ITEM_WEAPON_SAW;
  TH_SGUN: Result := ITEM_WEAPON_SHOTGUN1;
  TH_SGUN2: Result := ITEM_WEAPON_SHOTGUN2;
  TH_MGUN: Result := ITEM_WEAPON_CHAINGUN;
  TH_LAUN: Result := ITEM_WEAPON_ROCKETLAUNCHER;
  TH_PLAS: Result := ITEM_WEAPON_PLASMA;
  TH_BFG: Result := ITEM_WEAPON_BFG;
  TH_ARM1: Result := ITEM_ARMOR_GREEN;
  TH_ARM2: Result := ITEM_ARMOR_BLUE;
  TH_MEGA: Result := ITEM_SPHERE_WHITE;
  TH_INVL: Result := ITEM_INV;
  TH_AQUA: Result := ITEM_OXYGEN;
  TH_RKEY: Result := ITEM_KEY_RED;
  TH_GKEY: Result := ITEM_KEY_GREEN;
  TH_BKEY: Result := ITEM_KEY_BLUE;
  TH_SUIT: Result := ITEM_SUIT;
  TH_SUPER: Result := ITEM_SPHERE_BLUE;
  TH_GUN2: Result := ITEM_WEAPON_SUPERPULEMET;
  else Result := ITEM_NONE;
 end;
end;

function GetMonster(OldType: Byte): Byte;
begin
 case OldType of
  TH_DEMON:  Result := MONSTER_DEMON;
  TH_IMP:    Result := MONSTER_IMP;
  TH_ZOMBY:  Result := MONSTER_ZOMBY;
  TH_SERG:   Result := MONSTER_SERG;
  TH_CYBER:  Result := MONSTER_CYBER;
  TH_CGUN:   Result := MONSTER_CGUN;
  TH_BARON:  Result := MONSTER_BARON;
  TH_KNIGHT: Result := MONSTER_KNIGHT;
  TH_CACO:   Result := MONSTER_CACO;
  TH_SOUL:   Result := MONSTER_SOUL;
  TH_PAIN:   Result := MONSTER_PAIN;
  TH_SPIDER: Result := MONSTER_SPIDER;
  TH_BSP:    Result := MONSTER_BSP;
  TH_MANCUB: Result := MONSTER_MANCUB;
  TH_SKEL:   Result := MONSTER_SKEL;
  TH_VILE:   Result := MONSTER_VILE;
  TH_FISH:   Result := MONSTER_FISH;
  TH_BARREL: Result := MONSTER_BARREL;
  TH_ROBO:   Result := MONSTER_ROBO;
  TH_MAN:    Result := MONSTER_MAN;
  else Result := MONSTER_NONE;
 end;
end;

function GetArea(OldType: Byte): Byte;
begin
 case OldType of
  TH_PLR1: Result := AREA_PLAYERPOINT1;
  TH_PLR2: Result := AREA_PLAYERPOINT2;
  TH_DMSTART: Result := AREA_DMPOINT;
  else Result := AREA_NONE;
 end;
end;

function GetTrigger(OldType: Byte): Byte;
begin
 case OldType of
  SW_EXIT: Result := TRIGGER_EXIT;
  SW_EXITS: Result := TRIGGER_EXIT;
  SW_OPENDOOR: Result := TRIGGER_OPENDOOR;
  SW_SHUTDOOR: Result := TRIGGER_CLOSEDOOR;
  SW_SHUTTRAP: Result := TRIGGER_CLOSETRAP;
  SW_DOOR: Result := TRIGGER_DOOR;
  SW_DOOR5: Result := TRIGGER_DOOR5;
  SW_PRESS: Result := TRIGGER_PRESS;
  SW_TELE: Result := TRIGGER_TELEPORT;
  SW_SECRET: Result := TRIGGER_SECRET;
  SW_LIFTUP: Result := TRIGGER_LIFTUP;
  SW_LIFTDOWN: Result := TRIGGER_LIFTDOWN;
  SW_TRAP: Result := TRIGGER_TRAP;
  SW_LIFT: Result := TRIGGER_LIFT;
  else Result := TRIGGER_NONE;
 end;
end;

function GetBlockName(t: SmallInt): string;
begin
 case t of
  MB_COMMENT:   Result := 'MB_COMMENT';
  MB_END:       Result := 'MB_END';
  MB_WALLNAMES: Result := 'MB_WALLNAMES';
  MB_BACK:      Result := 'MB_BACK';
  MB_WTYPE:     Result := 'MB_WTYPE';
  MB_FRONT:     Result := 'MB_FRONT';
  MB_THING:     Result := 'MB_THING';
  MB_SWITCH:    Result := 'MB_SWITCH';
  MB_MUSIC:     Result := 'MB_MUSIC';
  MB_SKY:       Result := 'MB_SKY';
  MB_SWITCH2:   Result := 'MB_SWITCH2';
  MB__UNKNOWN:  Result := 'MB__UNKNOWN';
  else Result := '';
 end;
end;

end.
