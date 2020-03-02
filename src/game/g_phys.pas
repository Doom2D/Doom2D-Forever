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
unit g_phys;

interface

uses
  e_graphics;

type
  PObj = ^TObj;
  TObj = record
    X, Y:    Integer;
    Rect:    TRectWH;
    Vel:     TPoint2i;
    Accel:   TPoint2i;
    // going up the slope will set this, and renderer will adjust the position
    // this is purely visual change, it won't affect anything else
    slopeUpLeft: Integer; // left to go
    slopeFramesLeft: Integer; // frames left to go
    // for frame interpolation
    oldX, oldY: Integer;
    procedure lerp(t: Single; out fX, fY: Integer);
  end;

const
  MAX_YV = 30;
  LIMIT_VEL   = 16384;
  LIMIT_ACCEL = 1024;

  MOVE_NONE       = 0;
  MOVE_HITWALL    = 1;
  MOVE_HITCEIL    = 2;
  MOVE_HITLAND    = 4;
  MOVE_FALLOUT    = 8;
  MOVE_INWATER    = 16;
  MOVE_HITWATER   = 32;
  MOVE_HITAIR     = 64;
  MOVE_BLOCK      = 128;

procedure g_Obj_Init(Obj: PObj); inline;
function  g_Obj_Move(Obj: PObj; Fallable: Boolean; Splash: Boolean; ClimbSlopes: Boolean=False; asProjectile: Boolean=false): Word;
function  g_Obj_Move_Projectile (Obj: PObj; Fallable: Boolean; Splash: Boolean; ClimbSlopes: Boolean=False): Word;
function  g_Obj_Collide(Obj1, Obj2: PObj): Boolean; inline; overload;
function  g_Obj_Collide(X, Y: Integer; Width, Height: Word; Obj: PObj): Boolean; inline; overload;
function  g_Obj_CollidePoint(X, Y: Integer; Obj: PObj): Boolean; inline;
function  g_Obj_CollideLevel(Obj: PObj; XInc, YInc: Integer): Boolean; inline;
function  g_Obj_CollideStep(Obj: PObj; XInc, YInc: Integer): Boolean; inline;
function  g_Obj_CollideWater(Obj: PObj; XInc, YInc: Integer): Boolean; inline;
function  g_Obj_CollideLiquid(Obj: PObj; XInc, YInc: Integer): Boolean; inline;
function  g_Obj_CollidePanel(Obj: PObj; XInc, YInc: Integer; PanelType: Word): Boolean; inline;
function  g_Obj_StayOnStep(Obj: PObj): Boolean; inline;
function  g_Obj_CanMoveY(Obj: PObj; YInc: Integer): Boolean; inline;
procedure g_Obj_Push(Obj: PObj; VelX, VelY: Integer); inline;
procedure g_Obj_PushA(Obj: PObj; Vel: Integer; Angle: SmallInt); inline;
procedure g_Obj_SetSpeed(Obj: PObj; s: Integer); inline;
function  g_Obj_GetSpeedDirF(Obj: PObj; var dirx, diry, speed: Double): Boolean; inline; // `false`: zero speed
function  g_Obj_GetAccelDirF(Obj: PObj; var dirx, diry, speed: Double): Boolean; inline; // `false`: zero speed
function  z_dec(a, b: Integer): Integer; inline;
function  z_fdec(a, b: Double): Double; inline;

var
  gMon: Boolean = False;

implementation

uses
  g_map, g_basic, Math, g_player, g_console, SysUtils,
  g_sound, g_gfx, MAPDEF, g_monsters, g_game, utils;


const
  SmoothSlopeFrames = 4;

procedure TObj.lerp(t: Single; out fX, fY: Integer);
begin
  fX := nlerp(oldX, X, t);
  fY := nlerp(oldY, Y, t);
end;

function g_Obj_StayOnStep(Obj: PObj): Boolean; inline;
begin
  Result := not g_Map_CollidePanel(Obj^.X+Obj^.Rect.X, Obj^.Y+Obj^.Rect.Y+Obj^.Rect.Height-1,
                                   Obj^.Rect.Width, 1,
                                   PANEL_STEP, False)
            and g_Map_CollidePanel(Obj^.X+Obj^.Rect.X, Obj^.Y+Obj^.Rect.Y+Obj^.Rect.Height,
                                   Obj^.Rect.Width, 1,
                                   PANEL_STEP, False);
end;

function g_Obj_CanMoveY(Obj: PObj; YInc: Integer): Boolean; inline;
begin
  // Если шагнуть в по вертикали, а там стена => шагать нельзя
  // Или если шагнуть вниз, а там ступень => шагать нельзя
  Result := not(g_Obj_CollideLevel(Obj, 0, YInc) or ((YInc > 0) and g_Obj_StayOnStep(Obj)));
end;

function CollideLiquid(Obj: PObj; XInc, YInc: Integer): Boolean; inline;
begin
  Result := g_Map_CollidePanel(Obj^.X+Obj^.Rect.X+XInc, Obj^.Y+Obj^.Rect.Y+YInc,
                               Obj^.Rect.Width, Obj^.Rect.Height*2 div 3,
                               PANEL_WATER or PANEL_ACID1 or PANEL_ACID2, False);
end;

function CollideLift(Obj: PObj; XInc, YInc: Integer): Integer; inline;
begin
  if g_Map_CollidePanel(Obj^.X+Obj^.Rect.X+XInc, Obj^.Y+Obj^.Rect.Y+YInc,
                        Obj^.Rect.Width, Obj^.Rect.Height,
                        PANEL_LIFTUP, False) then
    Result := -1
  else if g_Map_CollidePanel(Obj^.X+Obj^.Rect.X+XInc, Obj^.Y+Obj^.Rect.Y+YInc,
                          Obj^.Rect.Width, Obj^.Rect.Height,
                          PANEL_LIFTDOWN, False) then
    Result := 1
  else
    Result := 0;
end;

function CollideHorLift(Obj: PObj; XInc, YInc: Integer): Integer; inline;
var
  left, right: Boolean;
begin
  left := g_Map_CollidePanel(Obj^.X+Obj^.Rect.X+XInc, Obj^.Y+Obj^.Rect.Y+YInc,
                             Obj^.Rect.Width, Obj^.Rect.Height,
                             PANEL_LIFTLEFT, False);
  right := g_Map_CollidePanel(Obj^.X+Obj^.Rect.X+XInc, Obj^.Y+Obj^.Rect.Y+YInc,
                             Obj^.Rect.Width, Obj^.Rect.Height,
                             PANEL_LIFTRIGHT, False);
  if left and not right then
    Result := -1
  else if right and not left then
    Result := 1
  else
    Result := 0;
end;

function CollidePlayers(_Obj: PObj; XInc, YInc: Integer): Boolean;
var
  plr: TPlayer;
begin
  result := false;
  if (gPlayers = nil) then exit;
  for plr in gPlayers do
  begin
    if (plr = nil) then continue;
    if not plr.alive then continue;
    with plr do
    begin
      if g_Collide(GameX+PLAYER_RECT.X, GameY+PLAYER_RECT.Y,
                   PLAYER_RECT.Width, PLAYER_RECT.Height,
                   _Obj^.X+_Obj^.Rect.X+XInc, _Obj^.Y+_Obj^.Rect.Y+YInc,
                   _Obj^.Rect.Width, _Obj^.Rect.Height) then
      begin
        result := true;
        exit;
      end;
    end;
  end;
end;

function Blocked(Obj: PObj; XInc, YInc: Integer): Boolean; inline;
begin
  Result := g_Map_CollidePanel(Obj^.X+Obj^.Rect.X+XInc, Obj^.Y+Obj.Rect.Y+YInc,
                               Obj^.Rect.Width, Obj^.Rect.Height,
                               PANEL_BLOCKMON, False);
end;

function g_Obj_CollideLevel(Obj: PObj; XInc, YInc: Integer): Boolean; inline;
begin
  Result := g_Map_CollidePanel(Obj^.X+Obj^.Rect.X+XInc, Obj^.Y+Obj.Rect.Y+YInc,
                               Obj^.Rect.Width, Obj^.Rect.Height,
                               PANEL_WALL, False);
end;

function g_Obj_CollideStep(Obj: PObj; XInc, YInc: Integer): Boolean; inline;
begin
  Result := g_Map_CollidePanel(Obj^.X+Obj^.Rect.X+XInc, Obj^.Y+Obj.Rect.Y+YInc,
                               Obj^.Rect.Width, Obj^.Rect.Height,
                               PANEL_STEP, False);
end;

function g_Obj_CollideWater(Obj: PObj; XInc, YInc: Integer): Boolean; inline;
begin
  Result := g_Map_CollidePanel(Obj^.X+Obj^.Rect.X+XInc, Obj^.Y+Obj.Rect.Y+YInc,
                               Obj^.Rect.Width, Obj^.Rect.Height,
                               PANEL_WATER, False);
end;

function g_Obj_CollideLiquid(Obj: PObj; XInc, YInc: Integer): Boolean; inline;
begin
  Result := g_Map_CollidePanel(Obj^.X+Obj^.Rect.X+XInc, Obj^.Y+Obj.Rect.Y+YInc,
                               Obj^.Rect.Width, Obj^.Rect.Height,
                               PANEL_WATER or PANEL_ACID1 or PANEL_ACID2, False);
end;

function g_Obj_CollidePanel(Obj: PObj; XInc, YInc: Integer; PanelType: Word): Boolean; inline;
begin
  Result := g_Map_CollidePanel(Obj^.X+Obj^.Rect.X+XInc, Obj^.Y+Obj.Rect.Y+YInc,
                               Obj^.Rect.Width, Obj^.Rect.Height,
                               PanelType, False);
end;

procedure g_Obj_Splash(Obj: PObj; Color: Byte);
var
  MaxVel: Integer;
begin
  MaxVel := nmax(abs(Obj^.Vel.X), abs(Obj^.Vel.Y));
  if MaxVel > 4 then
  begin
    if MaxVel < 10 then
      g_Sound_PlayExAt('SOUND_GAME_BULK1', Obj^.X, Obj^.Y)
    else
      g_Sound_PlayExAt('SOUND_GAME_BULK2', Obj^.X, Obj^.Y);
  end;

  g_GFX_Water(Obj^.X+Obj^.Rect.X+(Obj^.Rect.Width div 2),
              Obj^.Y+Obj^.Rect.Y+(Obj^.Rect.Height div 2),
              Min(5*(abs(Obj^.Vel.X)+abs(Obj^.Vel.Y)), 50),
              -Obj^.Vel.X, -Obj^.Vel.Y,
              Obj^.Rect.Width, 16, Color);
end;


function move (Obj: PObj; dx, dy: Integer; ClimbSlopes: Boolean): Word;
var
  i: Integer;
  sx, sy: ShortInt;
  st: Word;

  procedure slope (s: Integer);
  var
    i: Integer;
  begin
    i := 0;
    while g_Obj_CollideLevel(Obj, sx, 0) and (i < 4) do
    begin
      Obj^.Y += s;
      Inc(i);
    end;
    Obj^.X += sx;
    if (s < 0) then
    begin
      Obj.slopeUpLeft += i*(-s);
      Obj.slopeFramesLeft := SmoothSlopeFrames;
    end;
  end;

  function movex (): Boolean;
  begin
    result := false;

    // Если монстру шагнуть в сторону, а там блокмон
    if gMon and ((st and MOVE_BLOCK) = 0) then
    begin
      if Blocked(Obj, sx, 0) then st := st or MOVE_BLOCK;
    end;

    // Если шагнуть в сторону, а там стена => шагать нельзя
    if g_Obj_CollideLevel(Obj, sx, 0) then
    begin
      if ClimbSlopes and (abs(dy) < 2) then
      begin
        result := true;
        if (not g_Obj_CollideLevel(Obj, sx, -12)) and // забираемся на 12 пикселей влево/вправо
           (sy >= 0) and (not g_Obj_CanMoveY(Obj, sy)) then // только если есть земля под ногами
        begin
          slope(-1);
        end
        else
        begin
          result := false;
          st := st or MOVE_HITWALL;
        end;
      end
      else
      begin
        st := st or MOVE_HITWALL;
      end;
    end
    else // Там стены нет
    begin
      if CollideLiquid(Obj, sx, 0) then
      begin // Если шагнуть в сторону, а там теперь жидкость
        if ((st and MOVE_INWATER) = 0) then st := st or MOVE_HITWATER;
      end
      else // Если шагнуть в сторону, а там уже нет жидкости
      begin
        if ((st and MOVE_INWATER) <> 0) then st := st or MOVE_HITAIR;
      end;

      // Шаг
      Obj^.X += sx;
      result := true;
    end;
  end;

  function movey (): Boolean;
  begin
    result := false;

    // Если монстру шагнуть по вертикали, а там блокмон
    if gMon and ((st and MOVE_BLOCK) = 0) then
    begin
      if Blocked(Obj, 0, sy) then st := st or MOVE_BLOCK;
    end;

    // Если шагать нельзя
    if not g_Obj_CanMoveY(Obj, sy) then
    begin
      if sy > 0 then
        st := st or MOVE_HITLAND
      else
        st := st or MOVE_HITCEIL;
    end
    else // Там стены нет. И ступени снизу тоже нет
    begin
      if CollideLiquid(Obj, 0, sy) then
      begin // Если шагнуть в по вертикали, а там теперь жидкость
        if ((st and MOVE_INWATER) = 0) then st := st or MOVE_HITWATER;
      end
      else // Если шагнуть в по вертикали, а там уже нет жидкости
      begin
        if ((st and MOVE_INWATER) <> 0) then st := st or MOVE_HITAIR;
      end;

      // Шаг
      Obj^.Y += sy;
      result := true;
    end;
  end;

begin
  st := MOVE_NONE;

  // Объект в жидкости?
  if CollideLiquid(Obj, 0, 0) then st := st or MOVE_INWATER;

  // Монстр в блокмоне?
  if gMon then
  begin
    if Blocked(Obj, 0, 0) then st := st or MOVE_BLOCK;
  end;

  // Двигаться не надо?
  if (dx = 0) and (dy = 0) then begin result := st; exit; end;

  sx := g_basic.Sign(dx);
  sy := g_basic.Sign(dy);
  dx := abs(dx);
  dy := abs(dy);

  for i := 1 to dx do if not movex() then break;
  for i := 1 to dy do if not movey() then break;

  result := st;
end;


procedure g_Obj_Init (Obj: PObj); inline;
begin
  ZeroMemory(Obj, SizeOf(TObj));
end;


function g_Obj_Move_Projectile (Obj: PObj; Fallable: Boolean; Splash: Boolean; ClimbSlopes: Boolean=False): Word;
begin
  result := g_Obj_Move(Obj, Fallable, Splash, ClimbSlopes, true);
end;

function g_Obj_Move (Obj: PObj; Fallable: Boolean; Splash: Boolean; ClimbSlopes: Boolean=False; asProjectile: Boolean=false): Word;
var
  xv, yv, dx, dy: Integer;
  inwater: Boolean;
  c: Boolean;
  wtx: DWORD;
  slopeStep: Integer;
  dirx, diry, speed: Double;
label
  _move;
begin
  // Лимиты на скорость и ускорение
  Obj^.Vel.X := nclamp(Obj^.Vel.X, -LIMIT_VEL, LIMIT_VEL);
  Obj^.Vel.Y := nclamp(Obj^.Vel.Y, -LIMIT_VEL, LIMIT_VEL);
  Obj^.Accel.X := nclamp(Obj^.Accel.X, -LIMIT_ACCEL, LIMIT_ACCEL);
  Obj^.Accel.Y := nclamp(Obj^.Accel.Y, -LIMIT_ACCEL, LIMIT_ACCEL);
  {
  if      Obj^.Vel.X < -LIMIT_VEL then Obj^.Vel.X := -LIMIT_VEL
  else if Obj^.Vel.X >  LIMIT_VEL then Obj^.Vel.X :=  LIMIT_VEL;
  if      Obj^.Vel.Y < -LIMIT_VEL then Obj^.Vel.Y := -LIMIT_VEL
  else if Obj^.Vel.Y >  LIMIT_VEL then Obj^.Vel.Y :=  LIMIT_VEL;
  if      Obj^.Accel.X < -LIMIT_ACCEL then Obj^.Accel.X := -LIMIT_ACCEL
  else if Obj^.Accel.X >  LIMIT_ACCEL then Obj^.Accel.X :=  LIMIT_ACCEL;
  if      Obj^.Accel.Y < -LIMIT_ACCEL then Obj^.Accel.Y := -LIMIT_ACCEL
  else if Obj^.Accel.Y >  LIMIT_ACCEL then Obj^.Accel.Y :=  LIMIT_ACCEL;
  }

  // Вылетел за нижнюю границу карты?
  if (Obj^.Y > Integer(gMapInfo.Height)+128) then begin result := MOVE_FALLOUT; Obj.slopeUpLeft := 0; Obj.slopeFramesLeft := 0; exit; end;

  // Меняем скорость и ускорение только по четным кадрам
  c := (gTime mod (GAME_TICK*2) <> 0);

  // smoothed slopes
  if {not c and} (Obj.slopeUpLeft > 0) then
  begin
    if (Obj.slopeFramesLeft < 1) then
    begin
      //conwritefln('SLOPE DONE: slopeUpLeft=%s', [Obj.slopeUpLeft]);
      Obj.slopeUpLeft := 0; // oops
    end
    else
    begin
      slopeStep := Obj.slopeUpLeft div Obj.slopeFramesLeft;
      if (slopeStep < 1) then slopeStep := 1;
      //conwritefln('SLOPE STEP: slopeUpLeft=%s; slopeFramesLeft=%s; slopeStep=%d', [Obj.slopeUpLeft, Obj.slopeFramesLeft, slopeStep]);
      Dec(Obj.slopeFramesLeft);
      Obj.slopeUpLeft -= slopeStep;
      if (Obj.slopeUpLeft < 1) then
      begin
        Obj.slopeUpLeft := 0;
        Obj.slopeFramesLeft := 0;
      end;
    end;
  end;

  if c then goto _move;

  case CollideLift(Obj, 0, 0) of
    -1: //up
      begin
        Obj^.Vel.Y -= 1; // Лифт вверх
        if (Obj^.Vel.Y < -5) then Obj^.Vel.Y += 1;
      end;
    1: //down
      begin
        if (Obj^.Vel.Y > 5) then Obj^.Vel.Y -= 1;
        Obj^.Vel.Y += 1; // Гравитация или лифт вниз
      end;
    0: //???
      begin
        if Fallable then Obj^.Vel.Y += 1; // Гравитация
        if (Obj^.Vel.Y > MAX_YV) then Obj^.Vel.Y -= 1;
      end;
  end;

  case CollideHorLift(Obj, 0, 0) of
    -1: //left
      begin
        Obj^.Vel.X -= 3;
        if (Obj^.Vel.X < -9) then Obj^.Vel.X += 3;
      end;
    1: //right
      begin
        Obj^.Vel.X += 3;
        if (Obj^.Vel.X > 9) then Obj^.Vel.X -= 3;
      end;
    // 0 is not needed here
  end;

  // В воде?
  inwater := CollideLiquid(Obj, 0, 0);
  if inwater then
  begin
    if asProjectile then
    begin
      //writeln('velocity=(', Obj^.Vel.X, ',', Obj^.Vel.Y, '); acceleration=(', Obj^.Accel.X, ',', Obj^.Accel.Y, ')');
      if (g_Obj_GetSpeedDirF(Obj, dirx, diry, speed)) then
      begin
        //writeln('SPEED: ', speed);
        if (speed > 5) then
        begin
          speed := speed/1.4;
          Obj^.Vel.X := round(dirx*speed);
          Obj^.Vel.Y := round(diry*speed);
        end;
      end;

      // acceleration
      if (g_Obj_GetAccelDirF(Obj, dirx, diry, speed)) then
      begin
        if (speed > 5) then
        begin
          speed := speed/1.4;
          Obj^.Accel.X := round(dirx*speed);
          Obj^.Accel.Y := round(diry*speed);
        end;
      end;
    end
    else
    begin
      // velocity
      xv := abs(Obj^.Vel.X)+1;
      if (xv > 5) then Obj^.Vel.X := z_dec(Obj^.Vel.X, (xv div 2)-2);
      yv := abs(Obj^.Vel.Y)+1;
      if (yv > 5) then Obj^.Vel.Y := z_dec(Obj^.Vel.Y, (yv div 2)-2);

      // acceleration
      xv := abs(Obj^.Accel.X)+1;
      if (xv > 5) then Obj^.Accel.X := z_dec(Obj^.Accel.X, (xv div 2)-2);
      yv := abs(Obj^.Accel.Y)+1;
      if (yv > 5) then Obj^.Accel.Y := z_dec(Obj^.Accel.Y, (yv div 2)-2);
    end;
  end;

  // Уменьшаем прибавку к скорости
  Obj^.Accel.X := z_dec(Obj^.Accel.X, 1);
  Obj^.Accel.Y := z_dec(Obj^.Accel.Y, 1);

_move:

  xv := Obj^.Vel.X+Obj^.Accel.X;
  yv := Obj^.Vel.Y+Obj^.Accel.Y;

  dx := xv;
  dy := yv;

  result := move(Obj, dx, dy, ClimbSlopes);

  // Брызги (если нужны)
  if Splash then
  begin
    if WordBool(Result and MOVE_HITWATER) then
    begin
      wtx := g_Map_CollideLiquid_Texture(Obj^.X+Obj^.Rect.X, Obj^.Y+Obj^.Rect.Y,
                                         Obj^.Rect.Width, Obj^.Rect.Height*2 div 3);
      case wtx of
        LongWord(TEXTURE_SPECIAL_WATER): g_Obj_Splash(Obj, 3);
        LongWord(TEXTURE_SPECIAL_ACID1): g_Obj_Splash(Obj, 2);
        LongWord(TEXTURE_SPECIAL_ACID2): g_Obj_Splash(Obj, 1);
        LongWord(TEXTURE_NONE): begin end;
        else g_Obj_Splash(Obj, 0);
      end;
    end;
  end;

  // Меняем скорость и ускорение только по четным кадрам
  if c then exit;

  // Врезались в стену - стоп
  if ((Result and MOVE_HITWALL) <> 0) then
  begin
    Obj^.Vel.X := 0;
    Obj^.Accel.X := 0;
  end;

  // Врезались в пол или потолок - стоп
  if ((Result and (MOVE_HITCEIL or MOVE_HITLAND)) <> 0) then
  begin
    Obj^.Vel.Y := 0;
    Obj^.Accel.Y := 0;
  end;
end;


function g_Obj_Collide(Obj1, Obj2: PObj): Boolean; inline;
begin
  Result := g_Collide(Obj1^.X+Obj1^.Rect.X, Obj1^.Y+Obj1^.Rect.Y,
                      Obj1^.Rect.Width, Obj1^.Rect.Height,
                      Obj2^.X+Obj2^.Rect.X, Obj2^.Y+Obj2^.Rect.Y,
                      Obj2^.Rect.Width, Obj2^.Rect.Height);
end;

function g_Obj_Collide(X, Y: Integer; Width, Height: Word; Obj: PObj): Boolean; inline;
begin
  Result := g_Collide(X, Y,
                      Width, Height,
                      Obj^.X+Obj^.Rect.X, Obj^.Y+Obj^.Rect.Y,
                      Obj^.Rect.Width, Obj^.Rect.Height);
end;

function g_Obj_CollidePoint(X, Y: Integer; Obj: PObj): Boolean; inline;
begin
  Result := g_CollidePoint(X, Y, Obj^.X+Obj^.Rect.X, Obj^.Y+Obj^.Rect.Y,
                           Obj^.Rect.Width, Obj^.Rect.Height);
end;

procedure g_Obj_Push(Obj: PObj; VelX, VelY: Integer); inline;
begin
  Obj^.Vel.X := Obj^.Vel.X + VelX;
  Obj^.Vel.Y := Obj^.Vel.Y + VelY;
end;

procedure g_Obj_PushA(Obj: PObj; Vel: Integer; Angle: SmallInt); inline;
var
  s, c: Extended;

begin
  SinCos(DegToRad(-Angle), s, c);

  Obj^.Vel.X := Obj^.Vel.X + Round(Vel*c);
  Obj^.Vel.Y := Obj^.Vel.Y + Round(Vel*s);
end;

procedure g_Obj_SetSpeed(Obj: PObj; s: Integer); inline;
var
  m, vx, vy: Integer;
begin
  vx := Obj^.Vel.X;
  vy := Obj^.Vel.Y;

  m := Max(abs(vx), abs(vy));
  if m = 0 then
    m := 1;

  Obj^.Vel.X := (vx*s) div m;
  Obj^.Vel.Y := (vy*s) div m;
end;

// `false`: zero speed
function g_Obj_GetSpeedDirF(Obj: PObj; var dirx, diry, speed: Double): Boolean; inline;
var
  len, vx, vy: Double;
begin
  if (Obj^.Vel.X = 0) and (Obj^.Vel.Y = 0) then
  begin
    dirx := 0;
    diry := 0;
    speed := 0;
    result := false;
    exit;
  end;

  vx := Obj^.Vel.X;
  vy := Obj^.Vel.Y;
  len := sqrt(vx*vx+vy*vy);
  dirx := vx/len;
  diry := vy/len;
  speed := len;
  result := true;
end;

// `false`: zero acceleratin
function g_Obj_GetAccelDirF(Obj: PObj; var dirx, diry, speed: Double): Boolean; inline;
var
  len, vx, vy: Double;
begin
  if (Obj^.Accel.X = 0) and (Obj^.Accel.Y = 0) then
  begin
    dirx := 0;
    diry := 0;
    speed := 0;
    result := false;
    exit;
  end;

  vx := Obj^.Accel.X;
  vy := Obj^.Accel.Y;
  len := sqrt(vx*vx+vy*vy);
  dirx := vx/len;
  diry := vy/len;
  speed := len;
  result := true;
end;


// Приближаем a к 0 на b единиц:
function z_dec (a, b: Integer): Integer; inline;
begin
       if (abs(a) < b) then result := 0
  else if (a > 0) then result := a-b
  else if (a < 0) then result := a+b
  else result := 0; // a = 0
end;


// Приближаем a к 0.0 на b единиц:
function z_fdec (a, b: Double): Double; inline;
begin
       if (abs(a) < b) then result := 0.0
  else if (a > 0.0) then result := a-b
  else if (a < 0.0) then result := a+b
  else result := 0.0; // a = 0.0
end;


end.
