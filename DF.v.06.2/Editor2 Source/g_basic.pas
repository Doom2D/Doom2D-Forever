unit g_basic;

interface

uses
  windows;

Type
  String16   = String[16];
  Char16     = packed array[0..15] of Char;
  Char32     = packed array[0..31] of Char;
  Char64     = packed array[0..63] of Char;
  Char256    = packed array[0..255] of Char;
  ArrayStr16 = Array of String16;
  SArray     = Array of String;
  DWArray    = Array of DWORD;

  TDirection = (D_LEFT, D_RIGHT);

function  g_Collide(X1, Y1: Integer; Width1, Height1: Word;
                    X2, Y2: Integer; Width2, Height2: Word): Boolean;
function  g_CollidePoint(X, Y, X2, Y2: Integer; Width, Height: Word): Boolean;
function  g_CollideLevel(X, Y: Integer; Width, Height: Word): Boolean;
function  g_CollideLevel2(X, Y: Integer; Width, Height: Word; _ID: DWORD; var PanelID: DWORD): Boolean;
function  g_PatchLength(X1, Y1, X2, Y2: Integer): Word;
procedure IncMax(var A: Integer; B, Max: Integer); overload;
procedure IncMax(var A: Single; B, Max: Single); overload;
procedure IncMax(var A: Integer; Max: Integer); overload;
procedure IncMax(var A: Single; Max: Single); overload;
procedure IncMax(var A: Word; B, Max: Word); overload;
procedure IncMax(var A: Word; Max: Word); overload;
procedure IncMax(var A: SmallInt; B, Max: SmallInt); overload;
procedure IncMax(var A: SmallInt; Max: SmallInt); overload;
procedure DecMin(var A: Integer; B, Min: Integer); overload;
procedure DecMin(var A: Single; B, Min: Single); overload;
procedure DecMin(var A: Integer; Min: Integer); overload;
procedure DecMin(var A: Single; Min: Single); overload;
procedure DecMin(var A: Word; B, Min: Word); overload;
procedure DecMin(var A: Word; Min: Word); overload;
procedure DecMin(var A: Byte; B, Min: Byte); overload;
procedure DecMin(var A: Byte; Min: Byte); overload;
function  Sign(A: Integer): ShortInt; overload;
function  Sign(A: Single): ShortInt; overload;
function  PointToRect(X, Y: Integer; X1, Y1, Width, Height: Integer): Integer;
procedure g_ChangeDir(var dir: TDirection);
function  g_GetFileTime(fileName: String): Integer;
function  g_SetFileTime(fileName: String; time: Integer): Boolean;

implementation

uses
  Math, g_map, MAPDEF, SysUtils;

procedure g_ChangeDir(var dir: TDirection);
begin
  if dir = D_LEFT then
    dir := D_RIGHT
  else
    dir := D_LEFT;
end;

function g_GetFileTime(fileName: String): Integer;
var
  F: File;

begin
  if not FileExists(fileName) then
  begin
    Result := -1;
    Exit;
  end;

  AssignFile(F, fileName);
  Reset(F);
  Result := FileGetDate(TFileRec(F).Handle);
  CloseFile(F);
end;

function g_SetFileTime(fileName: String; time: Integer): Boolean;
var
  F: File;

begin
  if (not FileExists(fileName)) or (time < 0) then
  begin
    Result := False;
    Exit;
  end;

  AssignFile(F, fileName);
  Reset(F);
  Result := (FileSetDate(TFileRec(F).Handle, time) = 0);
  CloseFile(F);
end;

function g_PatchLength(X1, Y1, X2, Y2: Integer): Word;
begin
  Result := Trunc(Hypot(Abs(X2-X1), Abs(Y2-Y1)));
end;

function g_CollideLevel(X, Y: Integer; Width, Height: Word): Boolean;
var
  a: Integer;
  
begin
 Result := False;

 if gPanels = nil then Exit;

 for a := 0 to High(gPanels) do
  if gPanels[a].PanelType = PANEL_WALL then
   if not (((Y + Height <= gPanels[a].Y) or
            (Y          >= gPanels[a].Y + gPanels[a].Height)) or
           ((X + Width  <= gPanels[a].X) or
            (X          >= gPanels[a].X + gPanels[a].Width))) then
   begin
    Result := True;
    Exit;
   end;
end;

{function g_CollideLevel2(X, Y, X2, Y2: Integer): Boolean;
var
  a: Integer;
begin
 Result := False;

 if gWalls = nil then Exit;

 for a := 0 to High(gWalls) do
  if not (((Y2 <= gWalls[a].Y) or
           (Y  >= gWalls[a].Y + gWalls[a].Height)) or
          ((X2 <= gWalls[a].X) or
           (X  >= gWalls[a].X + gWalls[a].Width))) then
  begin
   Result := True;
   Exit;
  end;
end;}

function g_CollideLevel2(X, Y: Integer; Width, Height: Word; _ID: DWORD; var PanelID: DWORD): Boolean;
var
  a: DWORD;
begin
 Result := False;

 if gPanels = nil then Exit;

 for a := 0 to High(gPanels) do
  if (gPanels[a].PanelType = PANEL_WALL) and (_ID <> a) then
   if not (((Y + Height <= gPanels[a].Y) or
            (Y          >= gPanels[a].Y + gPanels[a].Height)) or
           ((X + Width  <= gPanels[a].X) or
            (X          >= gPanels[a].X + gPanels[a].Width))) then
   begin
    Result := True;
    PanelID := a;
    Exit;
   end;
end;

function g_Collide(X1, Y1: Integer; Width1, Height1: Word;
                   X2, Y2: Integer; Width2, Height2: Word): Boolean;
begin
 Result := not (((Y1 + Height1 <= Y2) or
                (Y1           >= Y2 + Height2)) or
                ((X1 + Width1 <= X2) or
                (X1           >= X2 + Width2)));
end;

function g_CollidePoint(X, Y, X2, Y2: Integer; Width, Height: Word): Boolean;
begin
 {X := X-X2;
 Y := Y-Y2;
 Result := (x >= 0) and (x <= Width) and
           (y >= 0) and (y <= Height);}
 Result := (X >= X2) and (X <= (X2+Width)) and
           (Y >= Y2) and (Y <= (Y2+Height));
end;

procedure IncMax(var A: Integer; B, Max: Integer);
begin
 if A+B > Max then A := Max else A := A+B;
end;

procedure IncMax(var A: Single; B, Max: Single);
begin
 if A+B > Max then A := Max else A := A+B;
end;

procedure DecMin(var A: Integer; B, Min: Integer);
begin
 if A-B < Min then A := Min else A := A-B;
end;

procedure DecMin(var A: Word; B, Min: Word);
begin
 if A-B < Min then A := Min else A := A-B;
end;

procedure DecMin(var A: Single; B, Min: Single);
begin
 if A-B < Min then A := Min else A := A-B;
end;

procedure IncMax(var A: Integer; Max: Integer);
begin
 if A+1 > Max then A := Max else A := A+1;
end;

procedure IncMax(var A: Single; Max: Single);
begin
 if A+1 > Max then A := Max else A := A+1;
end;

procedure IncMax(var A: Word; B, Max: Word);
begin
 if A+B > Max then A := Max else A := A+B;
end;

procedure IncMax(var A: Word; Max: Word);
begin
 if A+1 > Max then A := Max else A := A+1;
end;

procedure IncMax(var A: SmallInt; B, Max: SmallInt);
begin
 if A+B > Max then A := Max else A := A+B;
end;

procedure IncMax(var A: SmallInt; Max: SmallInt);
begin
 if A+1 > Max then A := Max else A := A+1;
end;

procedure DecMin(var A: Integer; Min: Integer);
begin
 if A-1 < Min then A := Min else A := A-1;
end;

procedure DecMin(var A: Single; Min: Single);
begin
 if A-1 < Min then A := Min else A := A-1;
end;

procedure DecMin(var A: Word; Min: Word);
begin
 if A-1 < Min then A := Min else A := A-1;
end;

procedure DecMin(var A: Byte; B, Min: Byte);
begin
 if A-B < Min then A := Min else A := A-B;
end;

procedure DecMin(var A: Byte; Min: Byte); overload;
begin
 if A-1 < Min then A := Min else A := A-1;
end;

function Sign(A: Integer): ShortInt;
begin
 if A < 0 then Result := -1
  else if A > 0 then Result := 1
   else Result := 0;
end;

function Sign(A: Single): ShortInt;
const
   Eps = 1.0E-5;
begin
 if Abs(A) < Eps then Result := 0
  else if A < 0 then Result := -1
   else Result := 1;
end;

function PointToRect(X, Y: Integer; X1, Y1, Width, Height: Integer): Integer;
begin
 X := X-X1;
 Y := Y-Y1;

 if X < 0 then
 begin
  if Y < 0 then Result := Round(Sqrt(Sqr(X)+Sqr(Y)))
   else if Y > Height then Result := Round(Sqrt(Sqr(X)+Sqr(Y-Height)))
    else Result := -X;
 end
  else
 if X > Width then
 begin
  X := X-width;
  if y < 0 then Result := Round(Sqrt(Sqr(X)+Sqr(Y)))
   else if Y > Height then Result := Round(Sqrt(Sqr(X)+Sqr(Y-Height)))
    else Result := X;
 end
  else
 begin
  if Y < 0 then Result := -Y
   else if Y > Height then Result := Y-Height
    else Result:=0;
 end;
end;

end.
