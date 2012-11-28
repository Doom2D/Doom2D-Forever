unit g_dots;

interface

uses
  g_obj;

const
  MAXDOT = 400;
  MAXINI = 50;
  MAXSR = 20;

  BL_XV = 4;
  BL_YV = 4;
  BL_MINT = 10;
  BL_MAXT = 14;

  SP_V = 2;
  SP_MINT = 5;
  SP_MAXT = 7;

procedure DOT_init();
procedure DOT_alloc();
procedure DOT_act();
procedure DOT_draw();
procedure DOT_add(x, y: Integer;  xv, yv, c, t: Byte);
procedure DOT_blood(x, y, xv, yv, n: Integer);
procedure DOT_spark(x, y, xv, yv, n: Integer);
procedure DOT_water(x, y, xv, yv, n, c: Integer);

implementation

uses
  dglOpenGL;

type
 dot_t = record
  o: TZObj;
  c: Byte;
  t: Byte;
 end;

 init_t = record
  xv: integer;
  yv: integer;
  c: byte;
  t: byte;
 end;

var
 dot: array [0..MAXDOT-1] of dot_t;
 bl_ini: array [0..MAXINI-1] of init_t;
 sp_ini: array [0..MAXINI-1] of init_t;
 bl_r: integer;
 sp_r: integer;
 sr_r: integer;
 sxr: array [0..MAXSR-1] of integer;
 syr: array [0..MAXSR-1] of integer;
 ldot: integer;

procedure DOT_init();
var
  i: integer;
begin
 for i := 0 to MAXDOT-1 do
 begin
  dot[i].t := 0;
  dot[i].o.r := 0;
  dot[i].o.h := 1;
 end;

 ldot:= 0; 
end;

procedure incldot();
begin
 ldot := ldot+1;
 if ldot >= MAXDOT then ldot := 0;
end;

procedure DOT_alloc();
var
  i: integer;
begin
 for i := 0 to MAXINI-1 do
 begin
  bl_ini[i].xv := random(BL_XV*2+1)-BL_XV;
  bl_ini[i].yv := -random(BL_YV);
  bl_ini[i].c := $B0+random(16);
  bl_ini[i].t := random(BL_MAXT-BL_MINT+1)+BL_MINT;
  sp_ini[i].xv := random(SP_V*2+1)-SP_V;
  sp_ini[i].yv := random(SP_V*2+1)-SP_V;
  sp_ini[i].c := $A0+random(6);
  sp_ini[i].t := random(SP_MAXT-SP_MINT+1)+SP_MINT;
 end;

 for i := 0 to MAXSR-1 do
 begin
  sxr[i] := random(2*2+1)-2;
  syr[i] := random(2*2+1)-2;
 end;

 bl_r := 0;
 sp_r := 0;
 sr_r := 0; 
end;

procedure DOT_act();
var
  i: integer;
  s: integer;
  xv: integer;
  yv: integer;
begin
 z_dot := True;

 for i := 0 to MAXDOT-1 do
 if dot[i].t <> 0 then
 begin
  xv := dot[i].o.xv+dot[i].o.vx;
  yv := dot[i].o.yv+dot[i].o.vy;
  s := Z_moveobj(@dot[i].o);
  if dot[i].t < 254 then dot[i].t := dot[i].t-1;
  if ByteBool(s and (_Z_HITWATER or _Z_FALLOUT)) then
  begin
   dot[i].t := 0;
   Continue;
  end;

  if ByteBool(s and _Z_HITLAND) then
  begin
   if dot[i].o.xv <> 0 then
   begin
    if yv > 2 then
    begin
     if xv <> 0 then dot[i].o.vx:= -1+random(2)
      else dot[i].o.vx := Z_sign(dot[i].o.vx);

     if random(yv) = 0 then dot[i].o.vx:= dot[i].o.vx*(2);
     dot[i].o.yv := yv-2;
    end;
   end;

   dot[i].o.xv := 0; 
   if (dot[i].t > 4) and (dot[i].t <> 255) then dot[i].t := 4;
  end;

  if ByteBool(s and _Z_HITWALL) then
  begin
   dot[i].o.vx := Z_sign(xv)*2;
   dot[i].o.yv := Z_sign(dot[i].o.yv);
   if dot[i].o.yv >= 0 then
    if Random(4) = 3 then dot[i].o.yv := dot[i].o.yv-1; // ??????
   if dot[i].o.yv >= 0 then
    if Random(3) = 1 then dot[i].o.yv := dot[i].o.yv-1; // ??????
  end;

  if ByteBool(s and _Z_HITCEIL) then
  begin
   dot[i].o.xv := 0;
   if Random(100) = 1 then dot[i].o.yv := -2 else dot[i].o.yv := 0;
  end;
 end;

 z_dot := False;
end;

procedure DOT_draw();
var
  i: integer; 
begin
 glDisable(GL_TEXTURE_2D);
 glPointSize(2);
 glBegin(GL_POINTS);


 for i := 0 to MAXDOT-1 do
  if dot[i].t <> 0 then
   with dot[i] do
   begin
    glColor3ub(c, c, c); // !!!!!!!!
    glVertex2i(dot[i].o.x, dot[i].o.y);
   end;

 glEnd;
end;

procedure DOT_add(x, y: Integer;  xv, yv, c, t: Byte);
var
  i: integer; 
begin
 if not Z_canfit(x, y, 1, 1) then Exit;

 i := ldot;
 dot[i].o.x := x;
 dot[i].o.y := y;
 dot[i].o.xv := xv;
 dot[i].o.yv := yv;
 dot[i].c := c;
 dot[i].t := t;
 dot[i].o.vx := 0;
 dot[i].o.vy := 0;

 incldot();
end;
       
procedure DOT_blood(x, y, xv, yv, n: Integer);
var
  i: integer;
  k: integer;
  dx: integer;
  dy: integer; 
begin
 for k := 1 to n do
 begin 
  dx := x+sxr[sr_r];
  dy := y+syr[sr_r];
  if not Z_canfit(x, y, 1, 1) then Continue;

  i := ldot;
  dot[i].o.x := dx;
  dot[i].o.y := dy;
  dot[i].o.xv := bl_ini[bl_r].xv+Z_dec(xv, 3);
  dot[i].o.yv := bl_ini[bl_r].yv+Z_dec(yv, 3)-3;
  dot[i].c := bl_ini[bl_r].c;
  dot[i].t := 255;
  dot[i].o.vx := 0;
  dot[i].o.vy := 0;

  bl_r := bl_r+1;
  if bl_r >= MAXINI then bl_r := 0;

  sr_r := sr_r+1;
  if sr_r >= MAXSR then sr_r := 0; 

  incldot();
 end;
end;

procedure DOT_spark(x, y, xv, yv, n: Integer);
var
  i: integer;
  k: integer;
  dx: integer;
  dy: integer; 
begin
 for k := 1 to n do
 begin 
  dx := x+sxr[sr_r];
  dy := y+syr[sr_r];
  if not Z_canfit(x, y, 1, 1) then Continue;

  i := ldot;
  dot[i].o.x := dx;
  dot[i].o.y := dy;
  dot[i].o.xv := sp_ini[sp_r].xv-(xv div 4);
  dot[i].o.yv:= sp_ini[sp_r].yv-yv div 4;
  dot[i].c := sp_ini[sp_r].c;
  dot[i].t := sp_ini[sp_r].t;
  dot[i].o.vx := 0;
  dot[i].o.vy := 0;

  sp_r := sp_r+1;
  if sp_r >= MAXINI then sp_r := 0;

  sr_r := sr_r+1;
  if sr_r >= MAXSR then sr_r := 0;

  incldot();
 end;
end;

procedure DOT_water(x, y, xv, yv, n, c: Integer);
const
  ct: array[0..2] of Byte = ($C0,$70,$B0);

var
  i: integer;
  k: integer;
  dx: integer;
  dy: integer;
begin
 if (c < 0) or (c >= 3) then Exit;

 c := ct[c]; 

 for k := 1 to n do
 begin 
  dx := x+sxr[sr_r];
  dy := y+syr[sr_r];
  if not Z_canfit(x, y, 1, 1) then Continue;

  i := ldot;
  dot[i].o.x := dx;
  dot[i].o.y := dy;
  dot[i].o.xv := bl_ini[bl_r].xv-Z_dec(xv, 3);
  dot[i].o.yv := bl_ini[bl_r].yv-abs(yv);
  dot[i].c := bl_ini[bl_r].c-$B0+c;
  dot[i].t := 254;
  dot[i].o.vx := 0;
  dot[i].o.vy := 0;

  sp_r := sp_r+1;
  if sp_r >= MAXINI then sp_r := 0;

  sr_r := sr_r+1;
  if sr_r >= MAXSR then sr_r := 0;

  incldot();
 end;
end;

end.
