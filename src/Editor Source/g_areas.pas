unit g_areas;

interface

uses
  g_textures,
  g_basic,
  e_graphics,
  windows;

type
  TAreaType=(AREA_NONE, AREA_PLAYER1, AREA_PLAYER2, AREA_DMPOINT);

  TArea = record
    GameX,
    GameY:     Integer;
    Size:      TRect;
    AreaType:  TAreaType;
    Direction: TDirection;
  end;

  TAreaSystem = class(TObject)
    AreasArray: Array of TArea;
    constructor Create;
    procedure   CreateArea(GameX, GameY: Integer; AreaType: TAreaType; Direction: TDirection);
    procedure   RemoveArea(ID: DWORD);
    procedure   Render(XInc, YInc: Integer);
    function    GetCount: DWORD;
  private
    function    FindArea: DWORD;
  end;

implementation

uses
  f_main;

{ TAreaSystem }

constructor TAreaSystem.Create;
begin
 g_CreateTextureFile('AREA_PLAYER1', EditorDir+'\Data\graphics\editor\player1point.tga');
 g_CreateTextureFile('AREA_PLAYER2', EditorDir+'\Data\graphics\editor\player2point.tga');
 g_CreateTextureFile('AREA_DMPOINT', EditorDir+'\Data\graphics\editor\dmpoint.tga');
end;

procedure TAreaSystem.CreateArea(GameX, GameY: Integer;
  AreaType: TAreaType; Direction: TDirection);
var
  find_id: DWORD;
begin
find_id := FindArea;

case AreaType of
 AREA_PLAYER1:
  begin
   AreasArray[find_id].Size.Left := 12;
   AreasArray[find_id].Size.Top := 0;
   AreasArray[find_id].Size.Right := 51;
   AreasArray[find_id].Size.Bottom := 52;
  end;
 AREA_PLAYER2:
  begin
   AreasArray[find_id].Size.Left := 12;
   AreasArray[find_id].Size.Top := 0;
   AreasArray[find_id].Size.Right := 51;
   AreasArray[find_id].Size.Bottom := 52;
  end;
 AREA_DMPOINT:
  begin
   AreasArray[find_id].Size.Left := 12;
   AreasArray[find_id].Size.Top := 0;
   AreasArray[find_id].Size.Right := 51;
   AreasArray[find_id].Size.Bottom := 52;
  end;
end;

AreasArray[find_id].GameX := GameX;
AreasArray[find_id].GameY := GameY;
AreasArray[find_id].AreaType := AreaType;
AreasArray[find_id].Direction := Direction;
end;

function TAreaSystem.FindArea: DWORD;
var
  i: Integer;
begin
if AreasArray <> nil then
for i := 0 to High(AreasArray) do
 if AreasArray[i].AreaType = AREA_NONE then
 begin
  Result := i;
  Exit;
 end;

if AreasArray = nil then
begin
 SetLength(AreasArray, 1);
 result := 0;
end
 else
begin
 Result := High(AreasArray) + 1;
 SetLength(AreasArray, Length(AreasArray) + 1);
end;
end;

function TAreaSystem.GetCount: DWORD;
var
  i: DWORD;
begin
 Result := 0;

 if AreasArray <> nil then
 for i := 0 to High(AreasArray) do
  if AreasArray[i].AreaType <> AREA_NONE then
   Inc(Result);
end;

procedure TAreaSystem.RemoveArea(ID: DWORD);
begin
 ZeroMemory(@AreasArray[ID], SizeOf(TArea));
end;

procedure TAreaSystem.Render(XInc, YInc: Integer);
var
  i: Integer;
  ID: DWORD;
begin
for i := 0 to High(AreasArray) do
begin
 case AreasArray[i].AreaType of
  AREA_NONE: Continue;
  AREA_PLAYER1: g_GetTexture('AREA_PLAYER1', ID);
  AREA_PLAYER2: g_GetTexture('AREA_PLAYER2', ID);
  AREA_DMPOINT: g_GetTexture('AREA_DMPOINT', ID);
 end;
 e_Draw(ID, AreasArray[i].GameX + XInc,AreasArray[i].GameY + YInc, 0, False, False);
end;
end;

end.
