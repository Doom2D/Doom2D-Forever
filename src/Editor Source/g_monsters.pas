unit g_monsters;

interface

uses
  windows,
  e_graphics,
  g_textures,
  g_basic;

type
  TMonsterType=(MONSTER_NONE, MONSTER_PRIKOLIST, MONSTER_MEGAPRIKOLIST);

  TMonster = record
    GameX,
    GameY:       Integer;
    Size:        TRect;
    MonsterType: TMonsterType;
    Direction:   TDirection;
  end;

  TMonsterSystem = class(TObject)
    MonstersArray: Array of TMonster;
    constructor    Create;
    destructor     Destroy; override;
    procedure      CreateMonster(GameX, GameY: Integer; MonsterType: TMonsterType;
                                 Direction: TDirection);
    procedure      RemoveMonster(ID: Integer);
    procedure      Render(XInc: Integer; YInc: Integer);
    function       GetCount: DWORD;
  private
    function       FindMonster: DWORD;
  end;

implementation

uses
  f_main;

{ TMonsterSystem }

function TMonsterSystem.FindMonster: DWORD;
var
  i: Integer;
begin
 if MonstersArray <> nil then
 for i := 0 to High(MonstersArray) do
  if MonstersArray[i].MonsterType = MONSTER_NONE then
  begin
   Result := i;
   Exit;
  end;

 if MonstersArray = nil then
 begin
  SetLength(MonstersArray, 1);
  Result := 0;
 end
  else
 begin
  Result := High(MonstersArray) + 1;
  SetLength(MonstersArray, Length(MonstersArray) + 1);
 end;
end;


procedure TMonsterSystem.CreateMonster(GameX, GameY: Integer;
  MonsterType: TMonsterType; Direction: TDirection);
var
  find_id: DWORD;
begin
find_id := FindMonster;

begin
 MonstersArray[find_id].MonsterType := MonsterType;
 case MonsterType of
  MONSTER_PRIKOLIST:
   begin
    MonstersArray[find_id].Size.Left := 12;
    MonstersArray[find_id].Size.Top := 0;
    MonstersArray[find_id].Size.Right := 51;
    MonstersArray[find_id].Size.Bottom := 52;
   end;
  MONSTER_MEGAPRIKOLIST:
   begin
    MonstersArray[find_id].Size.Left := 12;
    MonstersArray[find_id].Size.Top := 0;
    MonstersArray[find_id].Size.Right := 51;
    MonstersArray[find_id].Size.Bottom := 52;
   end;
 end;
 MonstersArray[find_id].GameX := GameX;
 MonstersArray[find_id].GameY := GameY;
 MonstersArray[find_id].Direction := Direction;
end;
end;

procedure TMonsterSystem.RemoveMonster(ID: Integer);
begin
 ZeroMemory(@MonstersArray[ID], SizeOf(TMonster));
end;

procedure TMonsterSystem.Render(XInc, YInc: Integer);
var
  i: Integer;
  ID: DWORD;
begin
if MonstersArray = nil then Exit;

for i := 0 to High(MonstersArray) do
 if MonstersArray[i].MonsterType <> MONSTER_NONE then
 begin
  case MonstersArray[i].MonsterType of
   MONSTER_PRIKOLIST: if MonstersArray[i].Direction = D_LEFT then
                       g_GetTexture('MONSTER_PRIKOLIST_L', ID)
                        else g_GetTexture('MONSTER_PRIKOLIST_R', ID);
   MONSTER_MEGAPRIKOLIST: if MonstersArray[i].Direction = D_LEFT then
                       g_GetTexture('MONSTER_MEGAPRIKOLIST_L', ID)
                        else g_GetTexture('MONSTER_MEGAPRIKOLIST_R', ID);
  end;
  e_Draw(ID, MonstersArray[i].GameX + XInc, MonstersArray[i].GameY + YInc, 0, False, False)
 end;
end;

constructor TMonsterSystem.Create;
begin
 g_CreateTextureFile('MONSTER_PRIKOLIST_L', EditorDir+'\Data\graphics\editor\l_prikolist.tga');
 g_CreateTextureFile('MONSTER_PRIKOLIST_R', EditorDir+'\Data\graphics\editor\r_prikolist.tga');
 g_CreateTextureFile('MONSTER_MEGAPRIKOLIST_L', EditorDir+'\Data\graphics\editor\l_megaprikolist.tga');
 g_CreateTextureFile('MONSTER_MEGAPRIKOLIST_R', EditorDir+'\Data\graphics\editor\r_megaprikolist.tga');
end;

destructor TMonsterSystem.Destroy;
begin

  inherited;
end;

function TMonsterSystem.GetCount: DWORD;
var
  i: DWORD;
begin
 Result := 0;

 if MonstersArray <> nil then
 for i := 0 to High(MonstersArray) do
  if MonstersArray[i].MonsterType <> MONSTER_NONE then
   Inc(Result);
end;

end.
