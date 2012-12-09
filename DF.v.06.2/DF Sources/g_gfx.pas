unit g_gfx;

interface

uses g_textures;

procedure g_GFX_Init();
procedure g_GFX_Free();

procedure g_GFX_Blood(fX, fY: Integer; Count: Word; vx, vy: Integer;{Angle: SmallInt; Vel: Single;}
                      DevX, DevY: Word);
procedure g_GFX_Spark(fX, fY: Integer; Count: Word; Angle: SmallInt; DevX, DevY: Byte);
procedure g_GFX_Water(fX, fY: Integer; Count: Word; fVelX, fVelY: Single; DevX, DevY: Byte);
procedure g_GFX_Bubbles(fX, fY: Integer; Count: Word; DevX, DevY: Byte);
procedure g_GFX_SetMax(Count: Integer);
function g_GFX_GetMax(): Integer;

procedure g_GFX_OnceAnim(X, Y: Integer; Anim: TAnimation; AnimType: Byte = 0);

procedure g_Mark(x, y, Width, Height: Integer; t: Byte);

procedure g_GFX_Update();
procedure g_GFX_Draw();

const
  ONCEANIM_NONE  = 0;
  ONCEANIM_SMOKE = 1;
  MARK_FREE     = 0;
  MARK_WALL     = 1;
  MARK_WATER    = 2;
  MARK_ACID     = 3;
  MARK_LIFTDOWN = 4;
  MARK_LIFTUP   = 5;
  MARK_DOOR     = 6;

var
  gCollideMap: array of array of Byte;

implementation

uses g_map, g_basic, Math, e_graphics, dglOpenGL, windows, g_options, g_console,
     SysUtils, g_triggers, MAPDEF, g_game;

type
  TParticle = record
   X, Y: Integer;
   VelX, VelY: Single;
   AccelX, AccelY: Single;
   Red, Green, Blue: Byte;
   Time, LiveTime: Word;
   State: Byte;
   ParticleType: Byte;
  end;

  TOnceAnim = record
   AnimType: Byte;
   X, Y: Integer;
   Animation: TAnimation;
  end;

const
  PARTICLE_BLOOD   = 0;
  PARTICLE_SPARK   = 1;
  PARTICLE_BUBBLES = 2;
  PARTICLE_WATER   = 3;
  STATE_FREE   = 0;
  STATE_NORMAL = 1;
  STATE_STICK  = 2;

var
  Particles: array of TParticle;
  OnceAnims: array of TOnceAnim;
  MaxParticles: Integer;
  CurrentParticle: Integer;

procedure g_Mark(x, y, Width, Height: Integer; t: Byte);
var
  yy, y2: Integer;
begin
 if x < 0 then
 begin
  Width := Width+x;
  x := 0;
 end;

 if Width < 0 then Exit;
 
 if y < 0 then
 begin
  Height := Height+y;
  y := 0;
 end;

 if Height < 0 then Exit;

 if x > gMapInfo.Width then Exit;
 if y > gMapInfo.Height then Exit;

 y2 := y+Height;
 if y2 > gMapInfo.Height then y2 := gMapInfo.Height;

 if x+Width > gMapInfo.Width then Width := gMapInfo.Width-x;
 if Width <= 0 then Exit;

 for yy := y to y2 do
  FillMemory(@gCollideMap[yy][x], Width, t);
end;

procedure CreateCollideMap();
var
  a: Integer;
begin
 SetLength(gCollideMap, gMapInfo.Height+1);
 for a := 0 to High(gCollideMap) do
  SetLength(gCollideMap[a], gMapInfo.Width+1);

 if gWater <> nil then
 for a := 0 to High(gWater) do
  with gWater[a] do g_Mark(X, Y, Width, Height, MARK_WATER);

 if gAcid1 <> nil then
 for a := 0 to High(gAcid1) do
  with gAcid1[a] do g_Mark(X, Y, Width, Height, MARK_ACID);

 if gAcid2 <> nil then
 for a := 0 to High(gAcid2) do
  with gAcid2[a] do g_Mark(X, Y, Width, Height, MARK_ACID);

 if gLifts <> nil then
 for a := 0 to High(gLifts) do
  with gLifts[a].Rect do
   if gLifts[a].LiftType = 1 then g_Mark(X, Y, Width, Height, MARK_LIFTDOWN)
    else g_Mark(X, Y, Width, Height, MARK_LIFTUP);

 if gWalls <> nil then
 for a := 0 to High(gWalls) do
  with gWalls[a], gWalls[a].Rect do
   g_Mark(X, Y, Width, Height, IfThen(Door, IfThen(Enabled, MARK_DOOR, MARK_FREE), MARK_WALL));
end;

procedure g_GFX_Init();
begin
 CreateCollideMap();
end;

procedure g_GFX_Free();
var
  a: Integer;
begin
 Particles := nil;
 SetLength(Particles, MaxParticles);
 CurrentParticle := 0;

 if OnceAnims <> nil then
 begin
  for a := 0 to High(OnceAnims) do
   if OnceAnims[a].Animation <> nil then OnceAnims[a].Animation.Destroy;

  OnceAnims := nil;
 end;

 gCollideMap := nil;
end;

procedure g_GFX_Blood(fX, fY: Integer; Count: Word; vx, vy: Integer; DevX, DevY: Word);
var
  a: Integer;
  DevX1, DevX2,
  DevY1, DevY2: Word;
  l: Integer;
begin
 l := Length(Particles);
 if l = 0 then Exit;
 if Count > l then Count := l;

 DevX1 := DevX div 2;
 DevX2 := DevX+1;
 DevY1 := DevY div 2;
 DevY2 := DevY+1;

 //g_Console_Add(Format('vx=%d vy=%d', [vx, vy]), True);

 for a := 1 to Count do
 begin
  with Particles[CurrentParticle] do
  begin
   X := fX-DevX1+Random(DevX2);
   Y := fY-DevY1+Random(DevY2);

   if (X < 0) or (X > gMapInfo.Width-1) or (Y < 0) or (Y > gMapInfo.Height-1) then Continue;

   VelX := vx+(Random-Random)*3;
   VelY := vy+(Random-Random)*3;

   if VelY > -4 then
    if VelY-4 < -4 then VelY := -4 else VelY := VelY-4;

   AccelX := -Sign(VelX)*Random/100;
   AccelY := 0.8;

   Red := 100+20*Random(6);
   Green := 0;
   Blue := 0;
   State := STATE_NORMAL;
   Time := 0;
   LiveTime := 500+Random(200);
   ParticleType := PARTICLE_BLOOD;
  end;
  if CurrentParticle >= MaxParticles-1 then CurrentParticle := 0
   else CurrentParticle := CurrentParticle+1;
 end;
end;

procedure g_GFX_Spark(fX, fY: Integer; Count: Word; Angle: SmallInt; DevX, DevY: Byte);
var
  a: Integer;
  b: Single;
  DevX1, DevX2,
  DevY1, DevY2: Byte;
  l: Integer;
begin
 l := Length(Particles);
 if l = 0 then Exit;
 if Count > l then Count := l;

 Angle := 360-Angle;

 DevX1 := DevX div 2;
 DevX2 := DevX+1;
 DevY1 := DevY div 2;
 DevY2 := DevY+1;

 for a := 1 to Count do
 begin
  with Particles[CurrentParticle] do
  begin
   X := fX-DevX1+Random(DevX2);
   Y := fY-DevY1+Random(DevY2);
   b := DegToRad(Angle);
   VelX := cos(b);
   VelY := 1.6*sin(b);
   if Abs(VelX) < 0.01 then VelX := 0;
   if Abs(VelY) < 0.01 then VelY := 0;
   VelX := VelX*Random;
   VelY := VelY-Random;
   AccelX := VelX/3;
   AccelY := VelY/5;
   Red := 255;
   Green := 100+Random(155);
   Blue := 64;
   State := STATE_NORMAL;
   Time := 0;
   LiveTime := 30+Random(60);
   ParticleType := PARTICLE_SPARK;
  end;
  if CurrentParticle+2 > MaxParticles then CurrentParticle := 0
   else CurrentParticle := CurrentParticle+1;
 end;
end;

procedure g_GFX_Water(fX, fY: Integer; Count: Word; fVelX, fVelY: Single; DevX, DevY: Byte);
var
  a: Integer;
  DevX1, DevX2,
  DevY1, DevY2: Byte;
  l: Integer;
begin
 l := Length(Particles);
 if l = 0 then Exit;
 if Count > l then Count := l;

 DevX1 := DevX div 2;
 DevX2 := DevX+1;
 DevY1 := DevY div 2;
 DevY2 := DevY+1;

 for a := 1 to Count do
 begin
  with Particles[CurrentParticle] do
  begin
   X := fX-DevX1+Random(DevX2);
   Y := fY-DevY1+Random(DevY2);
   VelX := fVelX*Random;
   if Random(10) < 7 then VelX := -VelX;
   VelY := fVelY*Random;
   AccelX := 0.0;
   AccelY := 0.8;
   Red := Trunc(255*Random);
   Green := Red;
   Blue := 155+Random(10)*10;
   State := STATE_NORMAL;
   Time := 0;
   LiveTime := 40+Random(60);
   ParticleType := PARTICLE_WATER;
  end;
  if CurrentParticle+2 > MaxParticles then CurrentParticle := 0
   else CurrentParticle := CurrentParticle+1;
 end;
end;

procedure g_GFX_Bubbles(fX, fY: Integer; Count: Word; DevX, DevY: Byte);
var
  a: Integer;
  DevX1, DevX2,
  DevY1, DevY2: Byte;
  l: Integer;
begin
 l := Length(Particles);
 if l = 0 then Exit;
 if Count > l then Count := l;

 DevX1 := DevX div 2;
 DevX2 := DevX+1;
 DevY1 := DevY div 2;
 DevY2 := DevY+1;

 for a := 1 to Count do
 begin
  with Particles[CurrentParticle] do
  begin
   X := fX-DevX1+Random(DevX2);
   Y := fY-DevY1+Random(DevY2);

   if X >= gMapInfo.Width then Continue;
   if X <= 0 then Continue;
   if Y >= gMapInfo.Height then Continue;
   if Y <= 0 then Continue;

   if gCollideMap[Y, X] <> MARK_WATER then Continue;

   VelX := 0;
   VelY := -1-Random;
   AccelX := 0;
   AccelY := VelY/10;
   Red := 255;
   Green := 255;
   Blue := 255;
   State := STATE_NORMAL;
   Time := 0;
   LiveTime := 65535;
   ParticleType := PARTICLE_BUBBLES;
  end;
  if CurrentParticle+2 > MaxParticles then CurrentParticle := 0
   else CurrentParticle := CurrentParticle+1;
 end;
end;

procedure g_GFX_SetMax(Count: Integer);
begin
 if Count > 50000 then Count := 50000;

 SetLength(Particles, Count);
 MaxParticles := Count;
 if CurrentParticle+1 > Count then CurrentParticle := 0;
end;

function g_GFX_GetMax(): Integer;
begin
 Result := MaxParticles;
end;

function FindOnceAnim: DWORD;
var
  i: Integer;
begin
 if OnceAnims <> nil then
 for i := 0 to High(OnceAnims) do
  if OnceAnims[i].Animation = nil then
  begin
   Result := i;
   Exit;
  end;

 if OnceAnims = nil then
 begin
  SetLength(OnceAnims, 16);
  Result := 0;
 end
  else
 begin
  Result := High(OnceAnims) + 1;
  SetLength(OnceAnims, Length(OnceAnims) + 16);
 end;
end;

procedure g_GFX_OnceAnim(X, Y: Integer; Anim: TAnimation; AnimType: Byte = 0);
var
  find_id: DWORD;
begin
 if Anim = nil then Exit;

 find_id := FindOnceAnim;

 OnceAnims[find_id].AnimType := AnimType;
 OnceAnims[find_id].Animation := TAnimation.Create(Anim.FramesID, Anim.Loop, Anim.Speed);
 OnceAnims[find_id].Animation.Blending := Anim.Blending;
 OnceAnims[find_id].Animation.Alpha := Anim.Alpha;
 OnceAnims[find_id].X := X;
 OnceAnims[find_id].Y := Y;
end;

procedure g_GFX_Update();
var
  a: Integer;
  w, h: Integer;
  dX, dY: SmallInt;
  b, len: Integer;
  s: ShortInt;
  c: Byte;
begin
 if Particles <> nil then
 begin
 w := gMapInfo.Width;
 h := gMapInfo.Height;

 len := High(Particles);

 for a := 0 to len do
  if Particles[a].State <> 0 then
   with Particles[a] do
   begin
    if Time = LiveTime then State := STATE_FREE;
    if (X >= w) or (Y >= h) or (X <= 0) or (Y <= 0) then State := STATE_FREE;

    if State = STATE_FREE then Continue;

    case ParticleType of
    PARTICLE_BLOOD:
    begin
     if gAdvBlood then
     begin
     if (State = STATE_STICK) then
      if (gCollideMap[Y-1, X] = MARK_FREE) and (gCollideMap[Y+1, X] = MARK_FREE) and
         (gCollideMap[Y, X-1] = MARK_FREE) and (gCollideMap[Y, X+1] = MARK_FREE) then
      begin
       VelY := 0.5;
       AccelY := 0.15;
       State := STATE_NORMAL;
      end
       else if Random(200) = 100 then
      begin
       VelY := 0.5;
       AccelY := 0.15;
       Continue;
      end;

     if gCollideMap[Y, X] = MARK_LIFTUP then
     begin
      if VelY > -4-Random(3) then VelY := VelY-0.8;
      if Abs(VelX) > 0.1 then VelX := VelX-VelX/10;
      VelX := VelX+(Random-Random)*0.2;
      if AccelY = 0 then AccelY := 0.15;
     end;

     dX := Round(VelX);
     dY := Round(VelY);

     if (dX = 0) and (dY = 0) then
      if (State <> STATE_STICK) and (gCollideMap[Y-1, X] = MARK_FREE) and
         (gCollideMap[Y, X] = MARK_FREE) and (gCollideMap[Y+1, X] = MARK_FREE) then
      begin
       VelY := 0.8;
       AccelY := 0.25;
       State := STATE_NORMAL;
      end;

     if dX <> 0 then
     begin
      if dX > 0 then s := 1 else s := -1;

      dX := Abs(dX);
      for b := 1 to dX do
      begin
       if (X+s >= w) or (X+s <= 0) then
       begin
        State := STATE_FREE;
        Break;
       end;

       c := gCollideMap[Y, X+s];

       if c = MARK_WALL then
       begin
        VelX := 0;
        VelY := 0;
        AccelX := 0;
        AccelY := 0;
        State := STATE_STICK;
        Break;
       end else X := X+s;
      end;
     end;

     if dY <> 0 then
     begin
      if dY > 0 then s := 1 else s := -1;

      dY := Abs(dY);
      for b := 1 to dY do
      begin
       if (Y+s >= h) or (Y+s <= 0) then
       begin
        State := STATE_FREE;
        Break;
       end;

       c := gCollideMap[Y+s, X];

       if c = MARK_WALL then
       begin
        VelX := 0;
        VelY := 0;
        AccelX := 0;
        AccelY := 0;
        if s > 0 then State := STATE_NORMAL else State := STATE_STICK;
        Break;
       end;

       Y := Y+s;
      end;
     end;
     end // if gAdvBlood
      else
     begin
      dX := Round(VelX);
      dY := Round(VelY);

      if (X+dX >= w) or (Y+dY >= h) or (X+dX <= 0) or (Y+dY <= 0) or
         (gCollideMap[Y+dY, X+dX] <> 0) then
      begin
       State := STATE_FREE;
       VelX := 0;
       VelY := 0;
      end
       else
      begin
       Y := Y+dY;
       X := X+dX;
      end;
     end;

     VelX := VelX+AccelX;
     VelY := VelY+AccelY;
    end;

    PARTICLE_SPARK:
    begin
     dX := Round(VelX);
     dY := Round(VelY);

     if dX <> 0 then
     begin
      if dX > 0 then s := 1 else s := -1;

      dX := Abs(dX);
      for b := 1 to dX do
      begin
       if (X+s >= w) or (X+s <= 0) then
       begin
        State := STATE_FREE;
        Break;
       end;

       c := gCollideMap[Y, X+s];
       if c > MARK_WALL then
       begin
        State := STATE_FREE;
        Break;
       end;

       if c = MARK_FREE then X := X+s
        else
       if c = MARK_WALL then
       begin
        VelX := 0;
        AccelX := 0;
        Break;
       end;
      end;
     end;

     if dY <> 0 then
     begin
      if dY > 0 then s := 1 else s := -1;

      dY := Abs(dY);
      for b := 1 to dY do
      begin
       if (Y+s >= h) or (Y+s <= 0) then
       begin
        State := STATE_FREE;
        Break;
       end;

       c := gCollideMap[Y+s, X];
       if c > MARK_WALL then
       begin
        State := STATE_FREE;
        Break;
       end;

       if c = MARK_FREE then Y := Y+s
        else
       if c = MARK_WALL then
       begin
        if s < 0 then
        begin
         VelY := -VelY;
         AccelY := Abs(AccelY);
        end
         else
        begin
         VelX := 0;
         AccelX := 0;
         VelY := 0;
         AccelY := 0;
        end;

        Continue;
       end;
      end;
     end;

     if VelX <> 0 then VelX := VelX+AccelX;
     if VelY <> 0 then if 0.08*Time < 10 then
      VelY := VelY+AccelY+0.08*Time else VelY := VelY+AccelY;

     Time := Time+1;
    end;

    PARTICLE_WATER:
    begin
     if (State = STATE_STICK) and (Random(30) = 15) then
     begin
      VelY := 0.5;
      AccelY := 0.15;
      if (gCollideMap[Y, X-1] <> MARK_WALL) and
         (gCollideMap[Y, X+1] <> MARK_WALL) then State := STATE_NORMAL;
      Continue;
     end;

     if gCollideMap[Y, X] = MARK_LIFTUP then
     begin
      if VelY > -4-Random(3) then VelY := VelY-0.8;
      if Abs(VelX) > 0.1 then VelX := VelX-VelX/10;
      VelX := VelX+(Random-Random)*0.2;
     end;

     dX := Round(VelX);
     dY := Round(VelY);

     VelX := VelX+AccelX;
     VelY := VelY+AccelY;

     if dX <> 0 then
     begin
      if dX > 0 then s := 1 else s := -1;

      for b := 1 to Abs(dX) do
      begin
       c := gCollideMap[Y, X+s];

       if (c = MARK_WATER) or (c = MARK_ACID) or
          (X+s >= w) or (X+s <= 0) then
       begin
        State := STATE_FREE;
        Break;
       end;

       if c = MARK_WALL then
       begin
        VelX := 0;
        VelY := 0;
        AccelX := 0;
        AccelY := 0;
        State := STATE_STICK;
        LiveTime := LiveTime + 50;
        Break;
       end else X := X+s;
      end;
     end;

     if dY <> 0 then
     begin
      if dY > 0 then s := 1 else s := -1;

      for b := 1 to Abs(dY) do
      begin
       c := gCollideMap[Y+s, X];

       if (c = MARK_WATER) or (c = MARK_ACID) or
          (Y+s >= h) or (Y+s <= 0) then
       begin
        State := STATE_FREE;
        Break;
       end;

       if c = MARK_WALL then
       begin
        VelX := 0;
        VelY := 0;
        AccelX := 0;
        AccelY := 0;
        if s > 0 then State := STATE_NORMAL 
         else
          begin
            State := STATE_STICK;
            LiveTime := LiveTime + 50;
          end;
        Break;
       end;

       Y := Y+s;
      end;
     end;
    end;

    PARTICLE_BUBBLES:
    begin
     dY := Round(VelY);

     if dY <> 0 then
     begin
      if dY > 0 then s := 1 else s := -1;

      for b := 1 to Abs(dY) do
      begin
       if (Y+s >= h) or (Y+s <= 0) then
       begin
        State := STATE_FREE;
        Break;
       end;

       if gCollideMap[Y+s, X] <> MARK_WATER then
       begin
        State := STATE_FREE;
        Break;
       end else Y := Y+s;
      end;
     end;

     if VelY > -4 then VelY := VelY+AccelY;

     Time := Time+1;
    end;
    end; // case
   end;
 end; // Particles <> nil

 if OnceAnims <> nil then
 begin
  for a := 0 to High(OnceAnims) do
   if OnceAnims[a].Animation <> nil then
    begin
     case OnceAnims[a].AnimType of
      ONCEANIM_SMOKE:
      begin
       if Random(3) = 0 then OnceAnims[a].X := OnceAnims[a].X-1+Random(3);
       if Random(2) = 0 then OnceAnims[a].Y := OnceAnims[a].Y-Random(2);
      end;
     end;

     if OnceAnims[a].Animation.Played then
     begin
      OnceAnims[a].Animation.Destroy;
      OnceAnims[a].Animation := nil;
     end else OnceAnims[a].Animation.Update;
    end;
 end;
end;

procedure g_GFX_Draw();
var
  a, len: Integer;
begin
 if Particles <> nil then
 begin
  glDisable(GL_TEXTURE_2D);
  glPointSize(2);
  glBegin(GL_POINTS);

  len := High(Particles);

  for a := 0 to len do
   with Particles[a] do
    if (State <> 0) and (X >= sX) and (Y >= sY) and (X <= sX+sWidth)
       and (sY <= sY+sHeight) then
    begin
     glColor3ub(Red, Green, Blue);
     glVertex2i(X, Y);
    end;

  glEnd;
 end;

 if OnceAnims <> nil then
  for a := 0 to High(OnceAnims) do
   if OnceAnims[a].Animation <> nil then
    with OnceAnims[a] do Animation.Draw(X, Y, M_NONE);
 end;
end.
