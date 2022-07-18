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
unit g_basic;

interface

uses
  utils, g_phys;

const
  GAME_VERSION  = '0.667';
  GAME_BUILDDATE = {$I %DATE%};
  GAME_BUILDTIME = {$I %TIME%};
  UID_GAME    = 1;
  UID_PLAYER  = 2;
  UID_MONSTER = 3;
  UID_ITEM    = 10;
  UID_MAX_GAME    = $10;
  UID_MAX_PLAYER  = $7FFF;
  UID_MAX_MONSTER = $FFFF;

type
  TDirection = (D_LEFT, D_RIGHT);
  WArray = array of Word;
  DWArray = array of DWORD;
  String20 = String[20];

function g_GetBuilderName (): AnsiString;
function g_GetBuildHash (full: Boolean = True): AnsiString;
function g_GetBuildArch (): AnsiString;

function g_CreateUID(UIDType: Byte): Word;
function g_GetUIDType(UID: Word): Byte;
function g_Collide(X1, Y1: Integer; Width1, Height1: Word;
                   X2, Y2: Integer; Width2, Height2: Word): Boolean; inline;
function g_CollideLine(x1, y1, x2, y2, rX, rY: Integer; rWidth, rHeight: Word): Boolean;
function g_CollidePoint(X, Y, X2, Y2: Integer; Width, Height: Word): Boolean; inline;
function g_CollideLevel(X, Y: Integer; Width, Height: Word): Boolean; inline;
function g_CollideAround(X1, Y1: Integer; Width1, Height1: Word;
                         X2, Y2: Integer; Width2, Height2: Word): Boolean; inline;
function g_CollidePlayer(X, Y: Integer; Width, Height: Word): Boolean; inline;
function g_PatchLength(X1, Y1, X2, Y2: Integer): Word;
function g_TraceVector(X1, Y1, X2, Y2: Integer): Boolean; // `true`: no wall hit
function g_GetAcidHit(X, Y: Integer; Width, Height: Word): Byte;
function g_Look(a, b: PObj; d: TDirection): Boolean;
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
function Sign(A: Integer): ShortInt; overload;
function Sign(A: Single): ShortInt; overload;
function PointToRect(X, Y, X1, Y1: Integer; Width, Height: Word): Integer;
function GetAngle(baseX, baseY, pointX, PointY: Integer): SmallInt;
function GetAngle2(vx, vy: Integer): SmallInt;
function GetLines(Text: string; FontID: DWORD; MaxWidth: Word): SSArray;
procedure Sort(var a: SSArray);
function Sscanf(const s: string; const fmt: string;
                const Pointers: array of Pointer): Integer;
function InDWArray(a: DWORD; arr: DWArray): Boolean;
function InWArray(a: Word; arr: WArray): Boolean;
function InSArray(a: string; arr: SSArray): Boolean;
function GetPos(UID: Word; o: PObj): Boolean;
function parse(s: string): SSArray;
function parse2(s: string; delim: Char): SSArray;
function g_GetFileTime(fileName: String): Integer;
function g_SetFileTime(fileName: String; time: Integer): Boolean;
procedure SortSArray(var S: SSArray);
function b_Text_Format(S: string): string;
function b_Text_Unformat(S: string): string;
function b_Text_Wrap(S: string; LineLen: Integer): string;
function b_Text_LineCount(S: string): Integer;

var
  gmon_dbg_los_enabled: Boolean = true;

implementation

uses
  Math, geom, e_log, g_map, g_gfx, g_player, SysUtils, MAPDEF,
  StrUtils, e_graphics, g_monsters, g_items, g_game;

{$PUSH}
{$WARN 2054 OFF} // unknwon env var
{$WARN 6018 OFF} // unreachable code
function g_GetBuilderName (): AnsiString;
begin
  if {$I %D2DF_BUILD_USER%} <> '' then
    result := {$I %D2DF_BUILD_USER%} // custom
  else if {$I %USER%} <> '' then
    result := {$I %USER%} // unix username
  else if {$I %USERNAME%} <> '' then
    result := {$I %USERNAME%} // windows username
  else
    result := 'unknown'
end;

function g_GetBuildHash (full: Boolean = True): AnsiString;
begin
  if {$I %D2DF_BUILD_HASH%} <> '' then
    if full then
      result := {$I %D2DF_BUILD_HASH%}
    else
      result := Copy({$I %D2DF_BUILD_HASH%}, 1, 7)
  else
    result := 'custom build'
end;
{$POP}

function g_GetBuildArch (): AnsiString;
  var cpu, mode, fpu: AnsiString;
begin
  {$IF DEFINED(CPUX86_64) OR DEFINED(CPUAMD64) OR DEFINED(CPUX64)}
    cpu := 'x86_64';
  {$ELSEIF DEFINED(CPUI386) OR DEFINED(CPU386)}
    cpu := 'x86';
  {$ELSEIF DEFINED(CPUI8086)}
    cpu := 'i8086';
  {$ELSEIF DEFINED(CPUI64)}
    cpu := 'Itanium64';
  {$ELSEIF DEFINED(CPUARM)}
    cpu := 'ARM';
  {$ELSEIF DEFINED(CPUAVR)}
    cpu := 'AVR';
  {$ELSEIF DEFINED(CPUPOWERPC32)}
    cpu := 'PowerPC_32';
  {$ELSEIF DEFINED(CPUPOWERPC64)}
    cpu := 'PowerPC_64';
  {$ELSEIF DEFINED(CPUALPHA)}}
    cpu := 'Alpha';
  {$ELSEIF DEFINED(CPUSPARC32)}
    cpu := 'Sparc32';
  {$ELSEIF DEFINED(CPUM68020)}
    cpu := 'M68020"
  {$ELSEIF DEFINED(CPU68K) OR DEFINED(CPUM68K)}
    cpu := 'm68k"
  {$ELSEIF DEFINED(CPUSPARC)}
    cpu := 'unknown-sparc';
  {$ELSEIF DEFINED(CPUPOWERPC)}
    cpu := 'unknown-ppc';
  {$ELSEIF DEFINED(CPU86) OR DEFINED(CPU87)}
    cpu := 'unknown-intel';
  {$ELSE}
    cpu := 'unknown-arch"
  {$ENDIF}

  {$IF DEFINED(CPU64)}
    mode := '64-bit';
  {$ELSEIF DEFINED(CPU32)}
    mode := '32-bit';
  {$ELSEIF DEFINED(CPU16)}
    mode := '16-bit';
  {$ELSE}
    mode := 'unknown-mode';
  {$ENDIF}

  {$IF DEFINED(FPUSOFT)}
    fpu := 'soft';
  {$ELSEIF DEFINED(FPUSSE3)}
    fpu := 'sse3';
  {$ELSEIF DEFINED(FPUSSE2)}
    fpu := 'sse2';
  {$ELSEIF DEFINED(FPUSSE)}
    fpu := 'sse';
  {$ELSEIF DEFINED(FPUSSE64)}
    fpu := 'sse64';
  {$ELSEIF DEFINED(FPULIBGCC)}
    fpu := 'libgcc';
  {$ELSEIF DEFINED(FPU68881)}
    fpu := '68881';
  {$ELSEIF DEFINED(FPUVFP)}
    fpu := 'vfp';
  {$ELSEIF DEFINED(FPUFPA11)}
    fpu := 'fpa11';
  {$ELSEIF DEFINED(FPUFPA10)}
    fpu := 'fpa10';
  {$ELSEIF DEFINED(FPUFPA)}
    fpu := 'fpa';
  {$ELSEIF DEFINED(FPUX87)}
    fpu := 'x87';
  {$ELSEIF DEFINED(FPUITANIUM)}
    fpu := 'itanium';
  {$ELSEIF DEFINED(FPUSTANDARD)}
    fpu := 'standard';
  {$ELSEIF DEFINED(FPUHARD)}
    fpu := 'hard';
  {$ELSE}
    fpu := 'unknown-fpu';
  {$ENDIF}

  result := cpu + ' ' + mode + ' ' + fpu;
end;

function g_PatchLength(X1, Y1, X2, Y2: Integer): Word;
begin
  Result := Min(Round(Hypot(Abs(X2-X1), Abs(Y2-Y1))), 65535);
end;

function g_CollideLevel(X, Y: Integer; Width, Height: Word): Boolean; inline;
begin
  result := g_Map_CollidePanel(X, Y, Width, Height, (PANEL_WALL or PANEL_CLOSEDOOR or PANEL_OPENDOOR), false);
end;
(*
var
  a: Integer;
begin
  Result := False;

  if gWalls = nil then
    Exit;

  for a := 0 to High(gWalls) do
    if gWalls[a].Enabled and
       not ( ((Y + Height <= gWalls[a].Y) or
              (Y          >= gWalls[a].Y + gWalls[a].Height)) or
             ((X + Width  <= gWalls[a].X) or
              (X          >= gWalls[a].X + gWalls[a].Width)) ) then
    begin
      Result := True;
      Exit;
    end;
end;
*)

function g_CollidePlayer(X, Y: Integer; Width, Height: Word): Boolean; inline;
var
  a: Integer;
begin
  Result := False;

  if gPlayers = nil then Exit;

  for a := 0 to High(gPlayers) do
    if (gPlayers[a] <> nil) and gPlayers[a].alive then
      if gPlayers[a].Collide(X, Y, Width, Height) then
      begin
        Result := True;
        Exit;
      end;
end;


function g_TraceVector(X1, Y1, X2, Y2: Integer): Boolean;
var
  wallHitX: Integer = 0;
  wallHitY: Integer = 0;
(*
  i: Integer;
  dx, dy: Integer;
  Xerr, Yerr, d: LongWord;
  incX, incY: Integer;
  x, y: Integer;
*)
begin
  (*
  result := False;

  Assert(gCollideMap <> nil, 'g_TraceVector: gCollideMap = nil');

  Xerr := 0;
  Yerr := 0;
  dx := X2-X1;
  dy := Y2-Y1;

  if dx > 0 then incX := 1 else if dx < 0 then incX := -1 else incX := 0;
  if dy > 0 then incY := 1 else if dy < 0 then incY := -1 else incY := 0;

  dx := abs(dx);
  dy := abs(dy);

  if dx > dy then d := dx else d := dy;

  x := X1;
  y := Y1;

  for i := 1 to d do
  begin
    Inc(Xerr, dx);
    Inc(Yerr, dy);
    if Xerr>d then
    begin
      Dec(Xerr, d);
      Inc(x, incX);
    end;
    if Yerr > d then
    begin
      Dec(Yerr, d);
      Inc(y, incY);
    end;

    if (y > gMapInfo.Height-1) or
    (y < 0) or (x > gMapInfo.Width-1) or (x < 0) then
      Exit;
    if ByteBool(gCollideMap[y, x] and MARK_BLOCKED) then
      Exit;
  end;

  Result := True;
  *)

  // `true` if no obstacles
  if (g_profile_los) then g_Mons_LOS_Start();
  result := (g_Map_traceToNearestWall(x1, y1, x2, y2, @wallHitX, @wallHitY) = nil);
  if (g_profile_los) then g_Mons_LOS_End();
end;


function g_CreateUID(UIDType: Byte): Word;
var
  ok: Boolean;
  i: Integer;
begin
  Result := $0;

  case UIDType of
    UID_PLAYER:
    begin
      repeat
        Result := UID_MAX_GAME+$1+Random(UID_MAX_PLAYER-UID_MAX_GAME+$1);

        ok := True;
        if gPlayers <> nil then
          for i := 0 to High(gPlayers) do
            if gPlayers[i] <> nil then
              if Result = gPlayers[i].UID then
              begin
                ok := False;
                Break;
              end;
      until ok;
    end;

    UID_MONSTER:
    begin
      //FIXME!!!
      while true do
      begin
        result := UID_MAX_PLAYER+$1+Random(UID_MAX_MONSTER-UID_MAX_GAME-UID_MAX_PLAYER+$1);
        if (g_Monsters_ByUID(result) = nil) then break;
      end;
    end;
  end;
end;

function g_GetUIDType(UID: Word): Byte;
begin
  if UID <= UID_MAX_GAME then
    Result := UID_GAME
  else
    if UID <= UID_MAX_PLAYER then
      Result := UID_PLAYER
    else
      Result := UID_MONSTER;
end;

function g_Collide(X1, Y1: Integer; Width1, Height1: Word;
                   X2, Y2: Integer; Width2, Height2: Word): Boolean; inline;
begin
  Result := not ( ((Y1 + Height1 <= Y2) or
                   (Y2 + Height2 <= Y1)) or
                  ((X1 + Width1  <= X2) or
                   (X2 + Width2  <= X1)) );
end;

function g_CollideAround(X1, Y1: Integer; Width1, Height1: Word;
                         X2, Y2: Integer; Width2, Height2: Word): Boolean; inline;
begin
  Result := g_Collide(X1, Y1, Width1, Height1, X2, Y2, Width2, Height2) or
            g_Collide(X1+1, Y1, Width1, Height1, X2, Y2, Width2, Height2) or
            g_Collide(X1-1, Y1, Width1, Height1, X2, Y2, Width2, Height2) or
            g_Collide(X1, Y1+1, Width1, Height1, X2, Y2, Width2, Height2) or
            g_Collide(X1, Y1-1, Width1, Height1, X2, Y2, Width2, Height2);
end;

function c(X1, Y1, Width1, Height1, X2, Y2, Width2, Height2: Integer): Boolean; inline;
begin
  Result := not (((Y1 + Height1 <= Y2) or
                  (Y1           >= Y2 + Height2)) or
                  ((X1 + Width1 <= X2) or
                   (X1          >= X2 + Width2)));
end;

function g_Collide2(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): Boolean; inline;
begin
  //Result :=  not (((Y2 <= Y3) or (Y1  >= Y4)) or ((X2 <= X3) or (X1  >= X4)));
  Result := c(X1, Y1, X2-X1, Y2-Y1, X3, Y3, X4-X3, Y4-Y3);
end;

function g_CollidePoint(X, Y, X2, Y2: Integer; Width, Height: Word): Boolean; inline;
begin
  X := X-X2;
  Y := Y-Y2;
  Result := (x >= 0) and (x <= Width) and
            (y >= 0) and (y <= Height);
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

function PointToRect(X, Y, X1, Y1: Integer; Width, Height: Word): Integer;
begin
  X := X-X1;            // A(0;0) --- B(W;0)
  Y := Y-Y1;            // |          |
                        // D(0;H) --- C(W;H)
  if X < 0 then
    begin // Слева
      if Y < 0 then // Слева сверху: расстояние до A
        Result := Round(Hypot(X, Y))
      else
        if Y > Height then // Слева снизу: расстояние до D
          Result := Round(Hypot(X, Y-Height))
        else // Слева посередине: расстояние до AD
          Result := -X;
    end
  else
    if X > Width then
      begin // Справа
        X := X-Width;
        if y < 0 then // Справа сверху: расстояние до B
          Result := Round(Hypot(X, Y))
        else
          if Y > Height then // Справа снизу: расстояние до C
            Result := Round(Hypot(X, Y-Height))
          else // Справа посередине: расстояние до BC
            Result := X;
      end
    else // Посередине
      begin
        if Y < 0 then // Посередине сверху: расстояние до AB
          Result := -Y
        else
          if Y > Height then // Посередине снизу: расстояние до DC
            Result := Y-Height
          else // Внутри прямоугольника: расстояние 0
            Result := 0;
      end;
end;

function g_GetAcidHit(X, Y: Integer; Width, Height: Word): Byte;
const
  tab: array[0..3] of Byte = (0, 5, 10, 20);
var
  a: Byte;
begin
  a := 0;

  if g_Map_CollidePanel(X, Y, Width, Height, PANEL_ACID1, False) then a := a or 1;
  if g_Map_CollidePanel(X, Y, Width, Height, PANEL_ACID2, False) then a := a or 2;

  Result := tab[a];
end;

function g_Look(a, b: PObj; d: TDirection): Boolean;
begin
  if not gmon_dbg_los_enabled then begin result := false; exit; end; // always "wall hit"

  if ((b^.X > a^.X) and (d = TDirection.D_LEFT)) or
     ((b^.X < a^.X) and (d = TDirection.D_RIGHT)) then
  begin
    Result := False;
    Exit;
  end;

  Result := g_TraceVector(a^.X+a^.Rect.X+(a^.Rect.Width div 2),
                          a^.Y+a^.Rect.Y+(a^.Rect.Height div 2),
                          b^.X+b^.Rect.X+(b^.Rect.Width div 2),
                          b^.Y+b^.Rect.Y+(b^.Rect.Height div 2));
end;

function GetAngle(baseX, baseY, pointX, PointY: Integer): SmallInt;
var
  c: Single;
  a, b: Integer;
begin
  a := abs(pointX-baseX);
  b := abs(pointY-baseY);

  if a = 0 then c := 90
    else c := RadToDeg(ArcTan(b/a));

  if pointY < baseY then c := -c;
  if pointX > baseX then c := 180-c;

  Result := Round(c);
end;

function GetAngle2(vx, vy: Integer): SmallInt;
var
  c: Single;
  a, b: Integer;
begin
  a := abs(vx);
  b := abs(vy);

  if a = 0 then
    c := 90
  else
    c := RadToDeg(ArcTan(b/a));

  if vy < 0 then
    c := -c;
  if vx > 0 then
    c := 180 - c;

  c := c + 180;

  Result := Round(c);
end;

{function g_CollideLine(x1, y1, x2, y2, rX, rY: Integer; rWidth, rHeight: Word): Boolean;
const
  table: array[0..8, 0..8] of Byte =
         ((0, 0, 3, 3, 1, 2, 2, 0, 1),
          (0, 0, 0, 0, 4, 7, 2, 0, 1),
          (3, 0, 0, 0, 4, 4, 1, 3, 1),
          (3, 0, 0, 0, 0, 0, 5, 6, 1),
          (1, 4, 4, 0, 0, 0, 5, 5, 1),
          (2, 7, 4, 0, 0, 0, 0, 0, 1),
          (2, 2, 1, 5, 5, 0, 0, 0, 1),
          (0, 0, 3, 6, 5, 0, 0, 0, 1),
          (1, 1, 1, 1, 1, 1, 1, 1, 1));

function GetClass(x, y: Integer): Byte;
begin
 if y < rY then
 begin
  if x < rX then Result := 7
   else if x < rX+rWidth then Result := 0
    else Result := 1;
 end
  else if y < rY+rHeight then
 begin
  if x < rX then Result := 6
   else if x < rX+rWidth then Result := 8
    else Result := 2;
 end
  else
 begin
  if x < rX then Result := 5
   else if x < rX+rWidth then Result := 4
    else Result := 3;
 end;
end;

begin
 case table[GetClass(x1, y1), GetClass(x2, y2)] of
  0: Result := False;
  1: Result := True;
  2: Result := Abs((rY-y1))/Abs((rX-x1)) <= Abs((y2-y1))/Abs((x2-x1));
  3: Result := Abs((rY-y1))/Abs((rX+rWidth-x1)) <= Abs((y2-y1))/Abs((x2-x1));
  4: Result := Abs((rY+rHeight-y1))/Abs((rX+rWidth-x1)) >= Abs((y2-y1))/Abs((x2-x1));
  5: Result := Abs((rY+rHeight-y1))/Abs((rX-x1)) >= Abs((y2-y1))/Abs((x2-x1));
  6: Result := (Abs((rY-y1))/Abs((rX+rWidth-x1)) <= Abs((y2-y1))/Abs((x2-x1))) and
               (Abs((rY+rHeight-y1))/Abs((rX-x1)) >= Abs((y2-y1))/Abs((x2-x1)));
  7: Result := (Abs((rY+rHeight-y1))/Abs((rX+rWidth-x1)) >= Abs((y2-y1))/Abs((x2-x1))) and
               (Abs((rY-y1))/Abs((rX-x1)) <= Abs((y2-y1))/Abs((x2-x1)));
  else Result := False;
 end;
end;}

function g_CollideLine(x1, y1, x2, y2, rX, rY: Integer; rWidth, rHeight: Word): Boolean;
{
var
  i: Integer;
  dx, dy: Integer;
  Xerr, Yerr: Integer;
  incX, incY: Integer;
  x, y, d: Integer;
}
begin
  result := lineAABBIntersects(x1, y1, x2, y2, rX, rY, rWidth, rHeight);
{
  Result := True;

  Xerr := 0;
  Yerr := 0;
  dx := X2-X1;
  dy := Y2-Y1;

  if dx > 0 then incX := 1 else if dx < 0 then incX := -1 else incX := 0;
  if dy > 0 then incY := 1 else if dy < 0 then incY := -1 else incY := 0;

  dx := abs(dx);
  dy := abs(dy);

  if dx > dy then d := dx else d := dy;

  x := X1;
  y := Y1;

  for i := 1 to d+1 do
  begin
    Inc(Xerr, dx);
    Inc(Yerr, dy);
    if Xerr > d then
    begin
      Dec(Xerr, d);
      Inc(x, incX);
    end;
    if Yerr > d then
    begin
      Dec(Yerr, d);
      Inc(y, incY);
    end;

    if (x >= rX) and (x <= (rX + rWidth - 1)) and
       (y >= rY) and (y <= (rY + rHeight - 1)) then Exit;
  end;

  Result := False;
}
end;

function GetStr(var Str: string): string;
var
  a: Integer;
begin
  Result := '';
  for a := 1 to Length(Str) do
    if (a = Length(Str)) or (Str[a+1] = ' ') then
    begin
      Result := Copy(Str, 1, a);
      Delete(Str, 1, a+1);
      Str := Trim(Str);
      Exit;
    end;
end;

function GetLines (Text: string; FontID: DWORD; MaxWidth: Word): SSArray;
  var i, j, len, lines: Integer;

  function GetLine (j, i: Integer): String;
  begin
    result := Copy(text, j, i - j + 1);
  end;

  function GetWidth (j, i: Integer): Integer;
    var w, h: Word;
  begin
    e_CharFont_GetSize(FontID, GetLine(j, i), w, h);
    result := w
  end;

begin
  result := nil; lines := 0;
  j := 1; i := 1; len := Length(Text);
  // e_LogWritefln('GetLines @%s len=%s [%s]', [MaxWidth, len, Text]);
  while j <= len do
  begin
    (* --- Get longest possible sequence --- *)
    while (i + 1 <= len) and (GetWidth(j, i + 1) <= MaxWidth) do Inc(i);
    (* --- Do not include part of word --- *)
    if (i < len) and (text[i] <> ' ') then
      while (i >= j) and (text[i] <> ' ') do Dec(i);
    (* --- Do not include spaces --- *)
    while (i >= j) and (text[i] = ' ') do Dec(i);
    (* --- Add line --- *)
    SetLength(result, lines + 1);
    result[lines] := GetLine(j, i);
    // e_LogWritefln('  -> (%s:%s::%s) [%s]', [j, i, GetWidth(j, i), result[lines]]);
    Inc(lines);
    (* --- Skip spaces --- *)
    while (i <= len) and (text[i] = ' ') do Inc(i);
    j := i + 2;
  end;
end;

procedure Sort(var a: SSArray);
var
  i, j: Integer;
  s: string;
begin
  if a = nil then Exit;

  for i := High(a) downto Low(a) do
    for j := Low(a) to High(a)-1 do
      if LowerCase(a[j]) > LowerCase(a[j+1]) then
      begin
        s := a[j];
        a[j] := a[j+1];
        a[j+1] := s;
      end;
end;

function Sscanf(const s: String; const fmt: String;
                const Pointers: array of Pointer): Integer;
var
  i, j, n, m: Integer;
  s1: ShortString;
  L: LongInt;
  X: Extended;

  function GetInt(): Integer;
  begin
    s1 := '';
    while (n <= Length(s)) and (s[n] = ' ') do
      Inc(n);

    while (n <= Length(s)) and (s[n] in ['0'..'9', '+', '-']) do
    begin
      s1 := s1 + s[n];
      Inc(n);
    end;

    Result := Length(s1);
  end;

  function GetFloat(): Integer;
  begin
    s1 := '';
    while (n <= Length(s)) and (s[n] = ' ') do
      Inc(n);

    while (n <= Length(s)) and //jd >= rather than >
          (s[n] in ['0'..'9', '+', '-', '.', 'e', 'E']) do
    begin
      s1 := s1 + s[n];
      Inc(n);
    end;

    Result := Length(s1);
  end;

  function GetString(): Integer;
  begin
    s1 := '';
    while (n <= Length(s)) and (s[n] = ' ') do
      Inc(n);

    while (n <= Length(s)) and (s[n] <> ' ') do
    begin
      s1 := s1 + s[n];
      Inc(n);
    end;

    Result := Length(s1);
  end;

  function ScanStr(c: Char): Boolean;
  begin
    while (n <= Length(s)) and (s[n] <> c) do
      Inc(n);
    Inc(n);

    Result := (n <= Length(s));
  end;

  function GetFmt(): Integer;
  begin
    Result := -1;

    while (True) do
    begin
      while (fmt[m] = ' ') and (m < Length(fmt)) do
        Inc(m);
      if (m >= Length(fmt)) then
        Break;

      if (fmt[m] = '%') then
      begin
        Inc(m);
        case fmt[m] of
          'd': Result := vtInteger;
          'f': Result := vtExtended;
          's': Result := vtString;
        end;
        Inc(m);
        Break;
      end;

      if (not ScanStr(fmt[m])) then
        Break;
      Inc(m);
    end;
  end;

begin
  n := 1;
  m := 1;
  Result := 0;
  s1 := '';

  for i := 0 to High(Pointers) do
  begin
    j := GetFmt();

    case j of
      vtInteger :
      begin
        if GetInt() > 0 then
          begin
            L := StrToIntDef(s1, 0);
            Move(L, Pointers[i]^, SizeOf(LongInt));
            Inc(Result);
          end
        else
          Break;
      end;

      vtExtended :
      begin
        if GetFloat() > 0 then
          begin
            X := StrToFloatDef(s1, 0.0);
            Move(X, Pointers[i]^, SizeOf(Extended));
            Inc(Result);
          end
        else
          Break;
      end;

      vtString :
      begin
        if GetString() > 0 then
          begin
            Move(s1, Pointers[i]^, Length(s1)+1);
            Inc(Result);
          end
        else
          Break;
      end;

      else {case}
        Break;
    end; {case}
  end;
end;

function InDWArray(a: DWORD; arr: DWArray): Boolean;
var
  b: Integer;
begin
  Result := False;

  if arr = nil then Exit;

  for b := 0 to High(arr) do
    if arr[b] = a then
    begin
      Result := True;
      Exit;
    end;
end;

function InWArray(a: Word; arr: WArray): Boolean;
var
  b: Integer;
begin
  Result := False;

  if arr = nil then Exit;

  for b := 0 to High(arr) do
    if arr[b] = a then
    begin
      Result := True;
      Exit;
    end;
end;

function InSArray(a: string; arr: SSArray): Boolean;
var
  b: Integer;
begin
  Result := False;

  if arr = nil then Exit;

  a := AnsiLowerCase(a);

  for b := 0 to High(arr) do
    if AnsiLowerCase(arr[b]) = a then
    begin
      Result := True;
      Exit;
    end;
end;

function GetPos(UID: Word; o: PObj): Boolean;
var
  p: TPlayer;
  m: TMonster;
begin
  Result := False;

  case g_GetUIDType(UID) of
    UID_PLAYER:
    begin
      p := g_Player_Get(UID);
      if p = nil then Exit;
      if not p.alive then Exit;

      o^ := p.Obj;
    end;

    UID_MONSTER:
    begin
      m := g_Monsters_ByUID(UID);
      if m = nil then Exit;
      if not m.alive then Exit;

      o^ := m.Obj;
    end;
    else Exit;
  end;

  Result := True;
end;

function parse(s: String): SSArray;
var
  a: Integer;
begin
  Result := nil;
  if s = '' then
    Exit;

  while s <> '' do
  begin
    for a := 1 to Length(s) do
      if (s[a] = ',') or (a = Length(s)) then
      begin
        SetLength(Result, Length(Result)+1);

        if s[a] = ',' then
          Result[High(Result)] := Copy(s, 1, a-1)
        else // Конец строки
          Result[High(Result)] := s;

        Delete(s, 1, a);
        Break;
      end;
  end;
end;

function parse2(s: string; delim: Char): SSArray;
var
  a: Integer;
begin
  Result := nil;
  if s = '' then Exit;

  while s <> '' do
  begin
    for a := 1 to Length(s) do
      if (s[a] = delim) or (a = Length(s)) then
      begin
        SetLength(Result, Length(Result)+1);

        if s[a] = delim then Result[High(Result)] := Copy(s, 1, a-1)
        else Result[High(Result)] := s;

        Delete(s, 1, a);
        Break;
      end;
  end;
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

procedure SortSArray(var S: SSArray);
var
  b: Boolean;
  i: Integer;
  sw: ShortString;
begin
  repeat
    b := False;
    for i := Low(S) to High(S) - 1 do
      if S[i] > S[i + 1] then begin
        sw := S[i];
        S[i] := S[i + 1];
        S[i + 1] := sw;
        b := True;
      end;
  until not b;
end;

function b_Text_Format(S: string): string;
var
  Spec, Rst: Boolean;
  I: Integer;
begin
  Result := '';
  Spec := False;
  Rst := False;
  for I := 1 to Length(S) do
  begin
    if (not Spec) and (S[I] = '\') and (I + 1 <= Length(S)) then
    begin
      Spec := True;
      Rst := True;
      continue;
    end;
    if Spec then
    begin
      case S[I] of
        'n': // line feed
          Result := Result + #10;
        '0': // black
          Result := Result + #1;
        '1': // white
          Result := Result + #2;
        'd': // darker
          Result := Result + #3;
        'l': // lighter
          Result := Result + #4;
        'r': // red
          Result := Result + #18;
        'g': // green
          Result := Result + #19;
        'b': // blue
          Result := Result + #20;
        'y': // yellow
          Result := Result + #21;
        '\': // escape
          Result := Result + '\';
        else
          Result := Result + '\' + S[I];
      end;
      Spec := False;
    end else
      Result := Result + S[I];
  end;
  // reset to white at end
  if Rst then Result := Result + #2;
end;

function b_Text_Unformat(S: string): string;
var
  Spec: Boolean;
  I: Integer;
begin
  Result := '';
  Spec := False;
  for I := 1 to Length(S) do
  begin
    if S[I] in [#1, #2, #3, #4, #10, #18, #19, #20, #21] then
    begin
      Spec := False;
      continue;
    end;
    if (not Spec) and (S[I] = '\') and (I + 1 <= Length(S)) then
    begin
      Spec := True;
      continue;
    end;
    if Spec then
    begin
      case S[I] of
        'n': ;
        '0': ;
        '1': ;
        'd': ;
        'l': ;
        'r': ;
        'g': ;
        'b': ;
        'y': ;
        '\': Result := Result + '\';
        else
          Result := Result + '\' + S[I];
      end;
      Spec := False;
    end else
      Result := Result + S[I];
  end;
end;

function b_Text_Wrap(S: string; LineLen: Integer): string;
begin
  Result := WrapText(S, ''#10, [#10, ' ', '-'], LineLen);
end;

function b_Text_LineCount(S: string): Integer;
var
  I: Integer;
begin
  Result := IfThen(S = '', 0, 1);
  for I := 1 to High(S) do
    if S[I] = #10 then
      Inc(Result);
end;

end.
