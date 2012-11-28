unit g_obj;

interface

type
 PZObj = ^TZObj;
 TZObj = record
  x, y: Integer;
  xv, yv: Integer;
  vx, vy: Integer;
  r, h: Integer;
 end;

const
  _Z_HITWALL  = 1;
  _Z_HITCEIL  = 2;
  _Z_HITLAND  = 4;
  _Z_FALLOUT  = 8;
  _Z_INWATER  = 16;
  _Z_HITWATER = 32;
  _Z_HITAIR   = 64;
  _Z_BLOCK    = 128;

var
  z_dot: Boolean = False;

function Z_sign(a: Integer): Integer;
function Z_dec(a, b: Integer): Integer;
function Z_canfit(x, y, r, h: Integer): Boolean;
procedure Z_kickobj(obj: PZObj; x, y, pwr: Integer);
function Z_moveobj(p: PZObj): Byte;

implementation

uses
  Math, g_gfx, g_map, g_game;

const
  MAX_YV = 5;

function Z_sign(a: Integer): Integer;
begin
 if a > 0 then Result := 1
  else if a < 0 then Result := -1
   else Result := 0;
end;

function Z_dec(a, b: Integer): Integer;
begin
 if abs(a) <= b then Result := 0
  else if a > 0 then Result := a-b
   else if a < 0 then Result := a+b
    else Result := 0;
end;

function Z_canfit(x, y, r, h: Integer): Boolean;
begin
 {if z_dot then Result := gCollideMap[y, x] <> 1
  else }Result := not g_MapCollidePanel(x, y, r, h, PANEL_WALL);
end;

function Z_hitceil(x, y, r, h: Integer): Boolean;
begin
 {if z_dot then Result := gCollideMap[y-1, x] <> 1
  else }Result := g_MapCollidePanel(x, y-1, r, h, PANEL_WALL);
end;

function Z_canstand(x, y, r: Integer): Boolean;
begin
 Result := g_MapCollidePanel(x, y+1, r, 1, PANEL_WALL);
end;

// 0 - Не в лифте
// 1 - Лифт вверх ??
// 2 - Лифт вниз  ??
function Z_inlift(x, y, r, h: Integer): Integer;
begin
 Result := 0;

 if g_MapCollidePanel(x, y, r, h, PANEL_LIFTUP) then
 begin
  Result := 1;
  Exit;
 end;

 if g_MapCollidePanel(x, y, r, h, PANEL_LIFTDOWN) then
 begin
  Result := 2;
  Exit;
 end;
end;

function Z_inwater(x, y, r, h: Integer): Boolean;
begin
 Result := g_MapCollidePanel(x, y, r, h, PANEL_WATER);
end;

function Z_getacid(x, y, r, h: Integer): Integer;
const
  tab: array[0..3] of Byte = (0, 5, 10, 20);

var
  a: Byte;
begin
 a := 0;

 if g_MapCollidePanel(x, y, r, h, PANEL_ACID1) then a := a or 1;
 if g_MapCollidePanel(x, y, r, h, PANEL_ACID2) then a := a or 2;

 Result := tab[a];
end;

procedure Z_set_speed(obj: PZObj; s: Integer);
var
  m: Integer;
begin
 m := Max(Abs(obj^.xv), Abs(obj^.yv));
 if m = 0 then m := 1;

 obj^.xv := (obj^.xv*s) div m;
 obj^.yv := (obj^.yv*s) div m;
end;

procedure Z_kickobj(obj: PZObj; x, y, pwr: Integer);
var
 dx, dy, m: Integer;
begin
 dx := obj^.x-x;
 dy := obj^.y-(obj.h div 2)-y;

 m := Max(Abs(dx), Abs(dy));
 if m = 0 then m := 1;

 obj^.vx := obj^.vx+(dx*pwr) div m;
 obj^.vy := obj^.vy+(dy*pwr) div m;
end;

function Z_cansee(X1, Y1, X2, Y2: Integer): Boolean;
var
 i: Integer;
 dx, dy: Integer;
 Xerr, Yerr: Integer;
 incX, incY: Integer;
 x, y, d: Integer;
begin
 Result := False;

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
   Xerr := Xerr+dx;
   Yerr := Yerr+dy;

   if Xerr > d then
   begin
    Xerr := Xerr-d;
    x := x+incX;
   end;

   if Yerr > d then
   begin
    Yerr := Yerr-d;
    y := y+incY;
   end;

   if gCollideMap[y, x] = 1 then Exit;
  end;

 Result := True;
end;

function Z_overlap(a, b: PZObj): Boolean;
begin
 Result := True;

 if (a^.x-a^.r > b^.x+b^.r) or
    (a^.x+a^.r < b^.x-b^.r) or
    (a^.y <= b^.y-b^.h) or
    (a^.y-a^.h >= b^.y) then Result := False;
end;

function Z_look(a, b: PZObj; d: Integer): Boolean;
begin
 if Z_sign(b^.x-a^.x) <> d*2-1 then
 begin
  Result := False;
  Exit;
 end;

 Result := Z_cansee(a^.x, a^.y-(a^.h div 2), b^.x, b^.y-(b^.h div 2));
end;



function move(var x, y: Integer; width, height: Word; dx, dy: Integer): Byte;
var
 i: Integer;
 Xerr, Yerr: Integer;
 incX, incY: Integer;
 _x, _y, d: Integer;
begin
 Result := 0;

 Xerr := 0;
 Yerr := 0;

 if dx > 0 then incX := 1 else if dx < 0 then incX := -1 else incX := 0;
 if dy > 0 then incY := 1 else if dy < 0 then incY := -1 else incY := 0;

 dx := abs(dx);
 dy := abs(dy);

 if dx > dy then d := dx else d := dy;

 for i := 1 to d do
 begin
  _x := x;
  _y := y;

  Xerr := Xerr+dx;
  Yerr := Yerr+dy;

  if Xerr > d then
  begin
   Xerr := Xerr-d;
   x := x+incX;
  end;

  if Yerr > d then
  begin
   Yerr := Yerr-d;
   y := y+incY;
  end;

  if (x < -100) or (x >= gMapInfo.Width+100) or (y < -100) or (y >= gMapInfo.Height) then
   Result := Result or _Z_FALLOUT;

  if g_MapCollidePanel(x, y, width, height, PANEL_WALL) or
     (not g_MapCollidePanel(x, y+height-1, width, 1, PANEL_STEP)
      and g_MapCollidePanel(x, y+height, width, 1, PANEL_STEP)) then
  begin
   if x <> _x then Result := Result or _Z_HITWALL
    else if y > _y then Result := Result or _Z_HITLAND
     else if y < _y then Result := Result or _Z_HITCEIL;
     
   Exit;
  end;
 end;
end;

function Z_moveobj(p: PZObj): Byte;
var
  x, y, xv, yv,
  r, h, lx, ly, st: Integer;
  inw: Boolean;

//wvel(v) if((xv=abs(v)+1)>5) v=Z_dec(v,xv/2-2)
procedure wvel(var v: Integer);
begin
 xv := abs(v)+1;
 if xv > 5 then v := Z_dec(v, (xv div 2)-2);
end;

begin
 st := 0;

 x := p^.x;
 y := p^.y;
 r := p^.r;
 h := p^.h;

 case Z_inlift(x, y, r, h) of
  0: begin
      p^.yv := p^.yv+1;
      if p^.yv > MAX_YV then p^.yv := p^.yv-1;
     end;
  1: begin
      p^.yv := p^.yv-1;
      if p^.yv < -5 then p^.yv := p^.yv+1;
     end;
  2: begin
      if p^.yv > 5 then p^.yv := p^.yv-1
       else p^.yv := p^.yv+1;             // ??
     end;
 end;

 inw := Z_inwater(x, y, r, h);

 if inw then
 begin
	st := st or _Z_INWATER;
	wvel(p^.xv);
	wvel(p^.yv);
	wvel(p^.vx);
	wvel(p^.vy);
 end;
 
 p^.vx := Z_dec(p^.vx, 1);
 p^.vy := Z_dec(p^.vy, 1);
 xv := p^.xv+p^.vx;
 yv := p^.yv+p^.vy;

 if Abs(xv) > 7 then xv := Z_sign(xv)*7;
 //if Abs(yv) > 7 then yv := Z_sign(yv)*7;

 st := st or move(x, y, r, h, xv, yv);

 if ByteBool(st and _Z_HITWALL) then
 begin
  xv := 0;
  p^.xv := 0;
  p^.vx := 0;
 end;

 if ByteBool(st and (_Z_HITCEIL or _Z_HITLAND)) then
 begin
  yv := 0;
  p^.yv := 0;
  p^.vy := 0;
 end;

 {while (xv <> 0) or (yv <> 0) do
 begin
  if (x < -100) or (x >= gMapInfo.Width+100) or (y < -100) or (y >= gMapInfo.Height) then
   st := st or _Z_FALLOUT;

  lx := x;

  if abs(xv) <= 7 then x := x+xv else
   if xv > 0 then x := x+7 else x := x-7;

  //if(z_mon) if(Z_isblocked(x,y,r,h,xv)) st|=Z_BLOCK;

  if not Z_canfit(x, y, r, h) then
  begin
   if xv = 0 then x := lx
    else if xv < 0 then x := lx//x=((lx-r)&0xFFF8)+r   ???   мб добавить трассировку
     else x := lx;//x=((lx+r)&0xFFF8)-r+7;

   xv := 0;
   p^.xv := 0;
   p^.vx := 0;

   st := st or _Z_HITWALL;
  end;

  //xv-=(abs(xv)<=7)?xv:((xv>0)?7:-7);
  if abs(xv) <= 7 then xv := 0 else
   if xv > 0 then xv := xv-7 else xv := xv+7;

  ly := y;

  //y+=(abs(yv)<=7)?yv:((yv>0)?7:-7);
  if abs(yv) <= 7 then y := y+yv else
   if yv > 0 then y := y+7 else y := y-7;

  if yv >= 8 then y := y-1;
  if (yv < 0) and Z_hitceil(x, y, r, h) then
  begin
   //y := ((ly-h+1)&0xFFF8)+h-1;  !!!!!!!!!!!
   y := ly;

	 yv := 1;
   p^.vy := 1;
   p^.yv := 0;
   st := st or _Z_HITCEIL;
	end;

  if (yv > 0) and Z_canstand(x, y, r) then
  begin
   //y := ((y+1)&0xFFF8)-1; !!!!!!!!!!!
   yv := 0;
   p^.yv := 0;
   p^.vy := 0;
   st := st or _Z_HITLAND;
  end;

  //yv-=(abs(yv)<=7)?yv:((yv>0)?7:-7);
  if abs(yv) <= 7 then yv := 0 else
   if yv > 0 then yv := y-7 else yv := yv+7;
 end;}

 p^.x := x;
 p^.y := y;

 if Z_inwater(x, y, r, h) then
 begin
  st := st or _Z_INWATER;
  if not inw then st := st or _Z_HITWATER;
 end else if inw then st := st or _Z_HITAIR;

 Result := st;
end;

end.
