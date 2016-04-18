{$MODE DELPHI}
unit g_gfx;

interface

uses
  g_textures;

const
  BLOOD_NORMAL = 0;
  BLOOD_SPARKS = 1;
  ONCEANIM_NONE  = 0;
  ONCEANIM_SMOKE = 1;
  MARK_FREE     = 0;
  MARK_WALL     = 1;
  MARK_WATER    = 2;
  MARK_ACID     = 4;
  MARK_LIFTDOWN = 8;
  MARK_LIFTUP   = 16;
  MARK_DOOR     = 32;
  MARK_LIFTLEFT  = 64;
  MARK_LIFTRIGHT = 128;
  MARK_BLOCKED  = MARK_WALL + MARK_DOOR;
  MARK_LIQUID   = MARK_WATER + MARK_ACID;
  MARK_LIFT     = MARK_LIFTDOWN + MARK_LIFTUP + MARK_LIFTLEFT + MARK_LIFTRIGHT;

procedure g_GFX_Init();
procedure g_GFX_Free();

procedure g_GFX_Blood(fX, fY: Integer; Count: Word; vx, vy: Integer;
                      DevX, DevY: Word; CR, CG, CB: Byte; Kind: Byte = BLOOD_NORMAL);
procedure g_GFX_Spark(fX, fY: Integer; Count: Word; Angle: SmallInt; DevX, DevY: Byte);
procedure g_GFX_Water(fX, fY: Integer; Count: Word; fVelX, fVelY: Single; DevX, DevY, Color: Byte);
procedure g_GFX_SimpleWater(fX, fY: Integer; Count: Word; fVelX, fVelY: Single; DefColor, CR, CG, CB: Byte);
procedure g_GFX_Bubbles(fX, fY: Integer; Count: Word; DevX, DevY: Byte);
procedure g_GFX_SetMax(Count: Integer);
function  g_GFX_GetMax(): Integer;

procedure g_GFX_OnceAnim(X, Y: Integer; Anim: TAnimation; AnimType: Byte = 0);

procedure g_Mark(x, y, Width, Height: Integer; t: Byte; st: Boolean);

procedure g_GFX_Update();
procedure g_GFX_Draw();

var
  gCollideMap: Array of Array of Byte;

implementation

uses
  g_map, g_basic, Math, e_graphics, GL, GLExt,
  g_options, g_console, SysUtils, g_triggers, MAPDEF,
  g_game, g_language, g_net;

type
  TParticle = record
    X, Y:               Integer;
    VelX, VelY:         Single;
    AccelX, AccelY:     Single;
    Red, Green, Blue:   Byte;
    Alpha:              Byte;
    Time, LiveTime:     Word;
    State:              Byte;
    ParticleType:       Byte;
    offsetX, offsetY:   ShortInt;
  end;

  TOnceAnim = record
    AnimType:   Byte;
    X, Y:       Integer;
    Animation:  TAnimation;
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
  Particles: Array of TParticle;
  OnceAnims: Array of TOnceAnim;
  MaxParticles: Integer;
  CurrentParticle: Integer;

procedure g_Mark(x, y, Width, Height: Integer; t: Byte; st: Boolean);
var
  yy, y2, xx, x2: Integer;
begin
  if x < 0 then
  begin
    Width := Width + x;
    x := 0;
  end;

  if Width < 0 then
    Exit;

  if y < 0 then
  begin
    Height := Height + y;
    y := 0;
  end;

  if Height < 0 then
    Exit;

  if x > gMapInfo.Width then
    Exit;
  if y > gMapInfo.Height then
    Exit;

  y2 := y + Height - 1;
  if y2 > gMapInfo.Height then
    y2 := gMapInfo.Height;

  x2 := x + Width - 1;
  if x2 > gMapInfo.Width then
    x2 := gMapInfo.Width;

  if st then
    begin // Установить признак
      for yy := y to y2 do
        for xx := x to x2 do
          gCollideMap[yy][xx] := gCollideMap[yy][xx] or t;
    end
  else
    begin // Убрать признак
      t := not t;
      for yy := y to y2 do
        for xx := x to x2 do
          gCollideMap[yy][xx] := gCollideMap[yy][xx] and t;
    end;
end;

procedure CreateCollideMap();
var
  a: Integer;
begin
  g_Game_SetLoadingText(_lc[I_LOAD_COLLIDE_MAP]+' 1/6', 0, False);
  SetLength(gCollideMap, gMapInfo.Height+1);
  for a := 0 to High(gCollideMap) do
    SetLength(gCollideMap[a], gMapInfo.Width+1);

  if gWater <> nil then
  begin
    g_Game_SetLoadingText(_lc[I_LOAD_COLLIDE_MAP]+' 2/6', 0, True);
    for a := 0 to High(gWater) do
      with gWater[a] do
        g_Mark(X, Y, Width, Height, MARK_WATER, True);
  end;

  if gAcid1 <> nil then
  begin
    g_Game_SetLoadingText(_lc[I_LOAD_COLLIDE_MAP]+' 3/6', 0, True);
    for a := 0 to High(gAcid1) do
      with gAcid1[a] do
        g_Mark(X, Y, Width, Height, MARK_ACID, True);
  end;

  if gAcid2 <> nil then
  begin
    g_Game_SetLoadingText(_lc[I_LOAD_COLLIDE_MAP]+' 4/6', 0, True);
    for a := 0 to High(gAcid2) do
      with gAcid2[a] do
        g_Mark(X, Y, Width, Height, MARK_ACID, True);
  end;

  if gLifts <> nil then
  begin
    g_Game_SetLoadingText(_lc[I_LOAD_COLLIDE_MAP]+' 5/6', 0, True);
    for a := 0 to High(gLifts) do
      with gLifts[a] do
      begin
        g_Mark(X, Y, Width, Height, MARK_LIFT, False);

        if LiftType = 0 then
          g_Mark(X, Y, Width, Height, MARK_LIFTUP, True)
        else if LiftType = 1 then
          g_Mark(X, Y, Width, Height, MARK_LIFTDOWN, True)
        else if LiftType = 2 then
          g_Mark(X, Y, Width, Height, MARK_LIFTLEFT, True)
        else if LiftType = 3 then
          g_Mark(X, Y, Width, Height, MARK_LIFTRIGHT, True)
      end;
  end;

  if gWalls <> nil then
  begin
    g_Game_SetLoadingText(_lc[I_LOAD_COLLIDE_MAP]+' 6/6', 0, True);
    for a := 0 to High(gWalls) do
    begin
      if gWalls[a].Door then
        begin
        // Закрытая дверь:
          if gWalls[a].Enabled then
            with gWalls[a] do
              g_Mark(X, Y, Width, Height, MARK_DOOR, True)
          else // Открытая дверь:
            if gWalls[a].Enabled then
              with gWalls[a] do
                g_Mark(X, Y, Width, Height, MARK_DOOR, False);
        end
      else // Стена
        with gWalls[a] do
          g_Mark(X, Y, Width, Height, MARK_WALL, True);
    end;
  end;
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
      OnceAnims[a].Animation.Free();

    OnceAnims := nil;
  end;

  gCollideMap := nil;
end;

procedure CorrectOffsets(id: Integer);
begin
  with Particles[id] do
  begin
    if (X >= 0) and (Y > 0) and
    (Y < Length(gCollideMap)) and (X < Length(gCollideMap[0])) and
    (ByteBool(gCollideMap[Y-1, X] and MARK_BLOCKED)) then
      offsetY := 1 // Стена сверху
    else
      offsetY := 0;

    if (X > 0) and (Y >= 0) and
    (Y < Length(gCollideMap)) and (X < Length(gCollideMap[0])) and
    (ByteBool(gCollideMap[Y, X-1] and MARK_BLOCKED)) then
      offsetX := 1 // Стена слева
    else
      offsetX := 0;
  end;
end;

procedure g_GFX_SparkVel(fX, fY: Integer; Count: Word; VX, VY: Integer; DevX, DevY: Byte);
var
  a: Integer;
  DevX1, DevX2,
  DevY1, DevY2: Byte;
  l: Integer;
begin
  l := Length(Particles);
  if l = 0 then
    Exit;
  if Count > l then
    Count := l;

  DevX1 := DevX div 2;
  DevX2 := DevX + 1;
  DevY1 := DevY div 2;
  DevY2 := DevY + 1;

  for a := 1 to Count do
  begin
    with Particles[CurrentParticle] do
    begin
      X := fX-DevX1+Random(DevX2);
      Y := fY-DevY1+Random(DevY2);

      VelX := VX + (Random-Random)*3;
      VelY := VY + (Random-Random)*3;

      if VelY > -4 then
        if VelY-4 < -4 then
          VelY := -4
        else
          VelY := VelY-4;

      AccelX := -Sign(VelX)*Random/100;
      AccelY := 0.8;

      Red := 255;
      Green := 100+Random(155);
      Blue := 64;
      Alpha := 255;

      State := STATE_NORMAL;
      Time := 0;
      LiveTime := 30+Random(60);
      ParticleType := PARTICLE_SPARK;

      CorrectOffsets(CurrentParticle);
    end;

    if CurrentParticle+2 > MaxParticles then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
  end;
end;

procedure g_GFX_Blood(fX, fY: Integer; Count: Word; vx, vy: Integer;
  DevX, DevY: Word; CR, CG, CB: Byte; Kind: Byte = BLOOD_NORMAL);
var
  a: Integer;
  DevX1, DevX2,
  DevY1, DevY2: Word;
  l: Integer;
  CRnd: Byte;
  CC: SmallInt;
begin
  if Kind = BLOOD_SPARKS then
  begin
    g_GFX_SparkVel(fX, fY, 2 + Random(2), -VX div 2, -VY div 2, DevX, DevY);
    Exit;
  end;
  l := Length(Particles);
  if l = 0 then
    Exit;
  if Count > l then
    Count := l;

  DevX1 := DevX div 2;
  DevX2 := DevX + 1;
  DevY1 := DevY div 2;
  DevY2 := DevY + 1;

  for a := 1 to Count do
  begin
    with Particles[CurrentParticle] do
    begin
      X := fX - DevX1 + Random(DevX2);
      Y := fY - DevY1 + Random(DevY2);

      if (X < 0) or (X > gMapInfo.Width-1) or
         (Y < 0) or (Y > gMapInfo.Height-1) or
         ByteBool(gCollideMap[Y, X] and MARK_WALL) then
        Continue;

      VelX := vx + (Random-Random)*3;
      VelY := vy + (Random-Random)*3;

      if VelY > -4 then
        if VelY-4 < -4 then
          VelY := -4
        else
          VelY := VelY-4;

      AccelX := -Sign(VelX)*Random/100;
      AccelY := 0.8;

      CRnd := 20*Random(6);
      if CR > 0 then
      begin
        CC := CR + CRnd - 50;
        if CC < 0   then CC := 0;
        if CC > 255 then CC := 255;
        Red := CC;
      end else
        Red := 0;
      if CG > 0 then
      begin
        CC := CG + CRnd - 50;
        if CC < 0   then CC := 0;
        if CC > 255 then CC := 255;
        Green := CC;
      end else
        Green := 0;
      if CB > 0 then
      begin
        CC := CB + CRnd - 50;
        if CC < 0   then CC := 0;
        if CC > 255 then CC := 255;
        Blue := CC;
      end else
        Blue := 0;

      Alpha := 255;

      State := STATE_NORMAL;
      Time := 0;
      LiveTime := 120+Random(40);
      ParticleType := PARTICLE_BLOOD;

      CorrectOffsets(CurrentParticle);
    end;

    if CurrentParticle >= MaxParticles-1 then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
  end;
end;

procedure g_GFX_Spark(fX, fY: Integer; Count: Word; Angle: SmallInt; DevX, DevY: Byte);
var
  a: Integer;
  b: Single;
  DevX1, DevX2,
  DevY1, DevY2: Byte;
  BaseVelX, BaseVelY: Single;
  l: Integer;
begin
  l := Length(Particles);
  if l = 0 then
    Exit;
  if Count > l then
    Count := l;

  Angle := 360 - Angle;

  DevX1 := DevX div 2;
  DevX2 := DevX + 1;
  DevY1 := DevY div 2;
  DevY2 := DevY + 1;

  b := DegToRad(Angle);
  BaseVelX := cos(b);
  BaseVelY := 1.6*sin(b);
  if Abs(BaseVelX) < 0.01 then
    BaseVelX := 0.0;
  if Abs(BaseVelY) < 0.01 then
    BaseVelY := 0.0;
  for a := 1 to Count do
  begin
    with Particles[CurrentParticle] do
    begin
      X := fX-DevX1+Random(DevX2);
      Y := fY-DevY1+Random(DevY2);

      VelX := BaseVelX*Random;
      VelY := BaseVelY-Random;
      AccelX := VelX/3.0;
      AccelY := VelY/5.0;

      Red := 255;
      Green := 100+Random(155);
      Blue := 64;
      Alpha := 255;

      State := STATE_NORMAL;
      Time := 0;
      LiveTime := 30+Random(60);
      ParticleType := PARTICLE_SPARK;

      CorrectOffsets(CurrentParticle);
    end;

    if CurrentParticle+2 > MaxParticles then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
  end;
end;

procedure g_GFX_Water(fX, fY: Integer; Count: Word; fVelX, fVelY: Single; DevX, DevY, Color: Byte);
var
  a: Integer;
  DevX1, DevX2,
  DevY1, DevY2: Byte;
  l: Integer;
begin
  l := Length(Particles);
  if l = 0 then
    Exit;
  if Count > l then
    Count := l;

  if Abs(fVelX) < 3.0 then
    fVelX := 3.0 - 6.0*Random;

  DevX1 := DevX div 2;
  DevX2 := DevX + 1;
  DevY1 := DevY div 2;
  DevY2 := DevY + 1;

  for a := 1 to Count do
  begin
    with Particles[CurrentParticle] do
    begin
      X := fX-DevX1+Random(DevX2);
      Y := fY-DevY1+Random(DevY2);

      if Abs(fVelX) < 0.5 then
        VelX := 1.0 - 2.0*Random
      else
        VelX := fVelX*Random;
      if Random(10) < 7 then
        VelX := -VelX;
      VelY := fVelY*Random;
      AccelX := 0.0;
      AccelY := 0.8;

      case Color of
        1: // Красный
          begin
            Red := 155 + Random(9)*10;
            Green := Trunc(150*Random);
            Blue := Green;
          end;
        2: // Зеленый
          begin
            Red := Trunc(150*Random);
            Green := 175 + Random(9)*10;
            Blue := Red;
          end;
        3: // Синий
          begin
            Red := Trunc(200*Random);
            Green := Red;
            Blue := 175 + Random(9)*10;
          end;
        else // Серый
          begin
            Red := 90 + Random(12)*10;
            Green := Red;
            Blue := Red;
          end;
      end;

      Alpha := 255;

      State := STATE_NORMAL;
      Time := 0;
      LiveTime := 60+Random(60);
      ParticleType := PARTICLE_WATER;

      CorrectOffsets(CurrentParticle);
    end;

    if CurrentParticle+2 > MaxParticles then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
  end;
end;

procedure g_GFX_SimpleWater(fX, fY: Integer; Count: Word; fVelX, fVelY: Single; DefColor, CR, CG, CB: Byte);
var
  a: Integer;
  l: Integer;
begin
  l := Length(Particles);
  if l = 0 then
    Exit;
  if Count > l then
    Count := l;

  for a := 1 to Count do
  begin
    with Particles[CurrentParticle] do
    begin
      X := fX;
      Y := fY;

      VelX := fVelX;
      VelY := fVelY;
      AccelX := 0.0;
      AccelY := 0.8;

      case DefColor of
        1: // Красный
          begin
            Red := 155 + Random(9)*10;
            Green := Trunc(150*Random);
            Blue := Green;
          end;
        2: // Зеленый
          begin
            Red := Trunc(150*Random);
            Green := 175 + Random(9)*10;
            Blue := Red;
          end;
        3: // Синий
          begin
            Red := Trunc(200*Random);
            Green := Red;
            Blue := 175 + Random(9)*10;
          end;
        4: // Свой цвет, светлее
          begin
            Red := 20 + Random(19)*10;
            Green := Red;
            Blue := Red;
            Red := Min(Red + CR, 255);
            Green := Min(Green + CG, 255);
            Blue := Min(Blue + CB, 255);
          end;
        5: // Свой цвет, темнее
          begin
            Red := 20 + Random(19)*10;
            Green := Red;
            Blue := Red;
            Red := Max(CR - Red, 0);
            Green := Max(CG - Green, 0);
            Blue := Max(CB - Blue, 0);
          end;
        else // Серый
          begin
            Red := 90 + Random(12)*10;
            Green := Red;
            Blue := Red;
          end;
      end;

      Alpha := 255;

      State := STATE_NORMAL;
      Time := 0;
      LiveTime := 60+Random(60);
      ParticleType := PARTICLE_WATER;

      CorrectOffsets(CurrentParticle);
    end;

    if CurrentParticle+2 > MaxParticles then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
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
  if l = 0 then
    Exit;
  if Count > l then
    Count := l;

  DevX1 := DevX div 2;
  DevX2 := DevX + 1;
  DevY1 := DevY div 2;
  DevY2 := DevY + 1;

  for a := 1 to Count do
  begin
    with Particles[CurrentParticle] do
    begin
      X := fX-DevX1+Random(DevX2);
      Y := fY-DevY1+Random(DevY2);

      if (X >= gMapInfo.Width) or (X <= 0) or
         (Y >= gMapInfo.Height) or (Y <= 0) then
        Continue;

      if not ByteBool(gCollideMap[Y, X] and MARK_LIQUID) then
        Continue;

      VelX := 0;
      VelY := -1-Random;
      AccelX := 0;
      AccelY := VelY/10;

      Red := 255;
      Green := 255;
      Blue := 255;
      Alpha := 255;

      State := STATE_NORMAL;
      Time := 0;
      LiveTime := 65535;
      ParticleType := PARTICLE_BUBBLES;

      CorrectOffsets(CurrentParticle);
    end;

    if CurrentParticle+2 > MaxParticles then
      CurrentParticle := 0
    else
      CurrentParticle := CurrentParticle+1;
  end;
end;

procedure g_GFX_SetMax(Count: Integer);
begin
  if Count > 50000 then
    Count := 50000;

  SetLength(Particles, Count);
  MaxParticles := Count;
  if CurrentParticle >= Count then
    CurrentParticle := 0;
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
  if Anim = nil then
    Exit;

  find_id := FindOnceAnim();

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
          if Time = LiveTime then
            State := STATE_FREE;
          if (X+1 >= w) or (Y+1 >= h) or (X <= 0) or (Y <= 0) then
            State := STATE_FREE;
          if State = STATE_FREE then
            Continue;

          case ParticleType of
            PARTICLE_BLOOD:
            begin
              if gAdvBlood then
                begin
                  if (State = STATE_STICK) then
                    if (not ByteBool(gCollideMap[Y-1, X] and MARK_BLOCKED)) and
                       (not ByteBool(gCollideMap[Y+1, X] and MARK_BLOCKED)) and
                       (not ByteBool(gCollideMap[Y, X-1] and MARK_BLOCKED)) and
                       (not ByteBool(gCollideMap[Y, X+1] and MARK_BLOCKED)) then
                      begin // Отлипла - капает
                        VelY := 0.5;
                        AccelY := 0.15;
                        State := STATE_NORMAL;
                      end
                    else
                      if Random(200) = 100 then
                      begin // Прилеплена - но возможно стекает
                        VelY := 0.5;
                        AccelY := 0.15;
                        Continue;
                      end;

                  if not ByteBool(gCollideMap[Y, X] and MARK_BLOCKED) then
                  begin
                    if ByteBool(gCollideMap[Y, X] and MARK_LIFTUP) then
                    begin // Лифт вверх
                      if VelY > -4-Random(3) then
                        VelY := VelY - 0.8;
                      if Abs(VelX) > 0.1 then
                        VelX := VelX - VelX/10.0;
                      VelX := VelX + (Random-Random)*0.2;
                      AccelY := 0.15;
                    end;
                    if ByteBool(gCollideMap[Y, X] and MARK_LIFTLEFT) then
                    begin // Поток влево
                      if VelX > -8-Random(3) then
                        VelX := VelX - 0.8;
                      AccelY := 0.15;
                    end;
                    if ByteBool(gCollideMap[Y, X] and MARK_LIFTRIGHT) then
                    begin // Поток вправо
                      if VelX < 8+Random(3) then
                        VelX := VelX + 0.8;
                      AccelY := 0.15;
                    end;
                  end;

                  dX := Round(VelX);
                  dY := Round(VelY);

                  if (Abs(VelX) < 0.1) and (Abs(VelY) < 0.1) then
                    if (State <> STATE_STICK) and
                       (not ByteBool(gCollideMap[Y-1, X] and MARK_BLOCKED)) and
                       (not ByteBool(gCollideMap[Y, X] and MARK_BLOCKED)) and
                       (not ByteBool(gCollideMap[Y+1, X] and MARK_BLOCKED)) then
                    begin // Висит в воздухе - капает
                      VelY := 0.8;
                      AccelY := 0.5;
                      State := STATE_NORMAL;
                    end;

                  if dX <> 0 then
                  begin
                    if dX > 0 then
                      s := 1
                    else
                      s := -1;

                    dX := Abs(dX);

                    for b := 1 to dX do
                    begin
                      if (X+s >= w) or (X+s <= 0) then
                      begin
                        State := STATE_FREE;
                        Break;
                      end;

                      c := gCollideMap[Y, X+s];

                      if ByteBool(c and MARK_BLOCKED) then
                      begin // Стена/дверь
                        VelX := 0;
                        VelY := 0;
                        AccelX := 0;
                        AccelY := 0;
                        State := STATE_STICK;
                        Break;
                      end;

                      X := X+s;
                    end;
                  end;

                  if dY <> 0 then
                  begin
                    if dY > 0 then
                      s := 1
                    else
                      s := -1;

                    dY := Abs(dY);

                    for b := 1 to dY do
                    begin
                      if (Y+s >= h) or (Y+s <= 0) then
                      begin
                        State := STATE_FREE;
                        Break;
                      end;

                      c := gCollideMap[Y+s, X];

                      if ByteBool(c and MARK_BLOCKED) then
                      begin // Стена/дверь
                        VelX := 0;
                        VelY := 0;
                        AccelX := 0;
                        AccelY := 0;
                        if (s > 0) and (State <> STATE_STICK) then
                          State := STATE_NORMAL
                        else
                          State := STATE_STICK;
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

                  if (X+dX >= w) or (Y+dY >= h) or
                     (X+dX <= 0) or (Y+dY <= 0) or
                     ByteBool(gCollideMap[Y+dY, X+dX] and MARK_BLOCKED) then
                    begin // Стена/дверь/граница
                      State := STATE_FREE;
                      VelX := 0;
                      VelY := 0;
                    end
                  else
                    begin
                      Y := Y + dY;
                      X := X + dX;
                    end;
                end;

              VelX := VelX + AccelX;
              VelY := VelY + AccelY;

            // Кровь растворяется в жидкости:
              if ByteBool(gCollideMap[Y, X] and MARK_LIQUID) then
              begin
                Inc(Time);

                Alpha := 255 - Trunc((255.0 * Time) / LiveTime);
              end;
            end;

            PARTICLE_SPARK:
            begin
              dX := Round(VelX);
              dY := Round(VelY);

              if (Abs(VelX) < 0.1) and (Abs(VelY) < 0.1) and
                 (not ByteBool(gCollideMap[Y-1, X] and MARK_BLOCKED)) and
                 (not ByteBool(gCollideMap[Y, X] and MARK_BLOCKED)) and
                 (not ByteBool(gCollideMap[Y+1, X] and MARK_BLOCKED)) then
              begin // Висит в воздухе
                VelY := 0.8;
                AccelY := 0.5;
              end;

              if dX <> 0 then
              begin
                if dX > 0 then
                  s := 1
                else
                  s := -1;

                dX := Abs(dX);

                for b := 1 to dX do
                begin
                  if (X+s >= w) or (X+s <= 0) then
                  begin
                    State := STATE_FREE;
                    Break;
                  end;

                  c := gCollideMap[Y, X+s];

                  if ByteBool(c and MARK_BLOCKED) then
                    begin // Стена/дверь - падает вертикально
                      VelX := 0;
                      AccelX := 0;
                      Break;
                    end
                  else // Пусто:
                    if c = MARK_FREE then
                      X := X + s
                    else // Остальное:
                      begin
                        State := STATE_FREE;
                        Break;
                      end;
                end;
              end;

              if dY <> 0 then
              begin
                if dY > 0 then
                  s := 1
                else
                  s := -1;

                dY := Abs(dY);

                for b := 1 to dY do
                begin
                  if (Y+s >= h) or (Y+s <= 0) then
                  begin
                    State := STATE_FREE;
                    Break;
                  end;

                  c := gCollideMap[Y+s, X];

                  if ByteBool(c and MARK_BLOCKED) then
                    begin // Стена/дверь - падает вертикально
                      if s < 0 then
                        begin
                          VelY := -VelY;
                          AccelY := Abs(AccelY);
                        end
                      else // Или не падает
                        begin
                          VelX := 0;
                          AccelX := 0;
                          VelY := 0;
                          AccelY := 0.8;
                        end;

                      Break;
                    end
                  else // Пусто:
                    if c = MARK_FREE then
                      Y := Y + s
                    else // Осальное:
                      begin
                        State := STATE_FREE;
                        Break;
                      end;
                end;
              end;

              if VelX <> 0.0 then
                VelX := VelX + AccelX;
              if VelY <> 0.0 then
              begin
                if AccelY < 10 then
                  AccelY := AccelY + 0.08;
                VelY := VelY + AccelY;
              end;

              Time := Time + 1;
            end;

            PARTICLE_WATER:
            begin
              if (State = STATE_STICK) and (Random(30) = 15) then
              begin // Стекает/отлипает
                VelY := 0.5;
                AccelY := 0.15;
                if (not ByteBool(gCollideMap[Y, X-1] and MARK_BLOCKED)) and
                   (not ByteBool(gCollideMap[Y, X+1] and MARK_BLOCKED)) then
                  State := STATE_NORMAL;
                Continue;
              end;

              if not ByteBool(gCollideMap[Y, X] and MARK_BLOCKED) then
              begin
                if ByteBool(gCollideMap[Y, X] and MARK_LIFTUP) then
                begin // Лифт вверх
                  if VelY > -4-Random(3) then
                    VelY := VelY - 0.8;
                  if Abs(VelX) > 0.1 then
                    VelX := VelX - VelX/10.0;
                  VelX := VelX + (Random-Random)*0.2;
                  AccelY := 0.15;
                end;
                if ByteBool(gCollideMap[Y, X] and MARK_LIFTLEFT) then
                begin // Поток влево
                  if VelX > -8-Random(3) then
                    VelX := VelX - 0.8;
                  AccelY := 0.15;
                end;
                if ByteBool(gCollideMap[Y, X] and MARK_LIFTRIGHT) then
                begin // Поток вправо
                  if VelX < 8+Random(3) then
                    VelX := VelX + 0.8;
                  AccelY := 0.15;
                end;
              end;

              dX := Round(VelX);
              dY := Round(VelY);

              if (Abs(VelX) < 0.1) and (Abs(VelY) < 0.1) then
                if (State <> STATE_STICK) and
                   (not ByteBool(gCollideMap[Y-1, X] and MARK_BLOCKED)) and
                   (not ByteBool(gCollideMap[Y, X] and MARK_BLOCKED)) and
                   (not ByteBool(gCollideMap[Y+1, X] and MARK_BLOCKED)) then
                begin // Висит в воздухе - капает
                  VelY := 0.8;
                  AccelY := 0.5;
                  State := STATE_NORMAL;
                end;

              if dX <> 0 then
              begin
                if dX > 0 then
                  s := 1
                else
                  s := -1;

                for b := 1 to Abs(dX) do
                begin
                  if (X+s >= w) or (X+s <= 0) then
                  begin // Сбоку граница
                    State := STATE_FREE;
                    Break;
                  end;

                  c := gCollideMap[Y, X+s];

                  if ByteBool(c and MARK_LIQUID) and (dY > 0) then
                  begin // Сбоку жидкость, а частица уже падает
                    State := STATE_FREE;
                    Break;
                  end;

                  if ByteBool(c and MARK_BLOCKED) then
                  begin // Стена/дверь
                    VelX := 0;
                    VelY := 0;
                    AccelX := 0;
                    AccelY := 0;
                    State := STATE_STICK;
                    Break;
                  end;

                  X := X+s;
                end;
              end;

              if dY <> 0 then
              begin
                if dY > 0 then
                  s := 1
                else
                  s := -1;

                for b := 1 to Abs(dY) do
                begin
                  if (Y+s >= h) or (Y+s <= 0) then
                  begin // Снизу/сверху граница
                    State := STATE_FREE;
                    Break;
                  end;

                  c := gCollideMap[Y+s, X];

                  if ByteBool(c and MARK_LIQUID) and (dY > 0) then
                  begin // Снизу жидкость, а частица уже падает
                    State := STATE_FREE;
                    Break;
                  end;

                  if ByteBool(c and MARK_BLOCKED) then
                  begin // Стена/дверь
                    VelX := 0;
                    VelY := 0;
                    AccelX := 0;
                    AccelY := 0;
                    if (s > 0) and (State <> STATE_STICK) then
                      State := STATE_NORMAL
                    else
                      State := STATE_STICK;
                    Break;
                  end;

                  Y := Y+s;
                end;
              end;

              VelX := VelX + AccelX;
              VelY := VelY + AccelY;

              Time := Time + 1;
            end;

            PARTICLE_BUBBLES:
            begin
              dY := Round(VelY);

              if dY <> 0 then
              begin
                if dY > 0 then
                  s := 1
                else
                  s := -1;

                for b := 1 to Abs(dY) do
                begin
                  if (Y+s >= h) or (Y+s <= 0) then
                  begin
                    State := STATE_FREE;
                    Break;
                  end;

                  if not ByteBool(gCollideMap[Y+s, X] and MARK_LIQUID) then
                  begin // Уже не жидкость
                    State := STATE_FREE;
                    Break;
                  end;

                  Y := Y+s;
                end;
              end;

              if VelY > -4 then
                VelY := VelY + AccelY;

              Time := Time + 1;
            end;
          end; // case

          CorrectOffsets(a);
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
              if Random(3) = 0 then
                OnceAnims[a].X := OnceAnims[a].X-1+Random(3);
              if Random(2) = 0 then
                OnceAnims[a].Y := OnceAnims[a].Y-Random(2);
            end;
        end;

        if OnceAnims[a].Animation.Played then
          begin
            OnceAnims[a].Animation.Free();
            OnceAnims[a].Animation := nil;
          end
        else
          OnceAnims[a].Animation.Update();
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

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glBegin(GL_POINTS);

    len := High(Particles);

    for a := 0 to len do
      with Particles[a] do
        if (State <> STATE_FREE) and (X >= sX) and (Y >= sY) and
           (X <= sX+sWidth) and (sY <= sY+sHeight) then
        begin
          glColor4ub(Red, Green, Blue, Alpha);
          glVertex2i(X + offsetX, Y + offsetY);
        end;

    glEnd();

    glDisable(GL_BLEND);
  end;

  if OnceAnims <> nil then
    for a := 0 to High(OnceAnims) do
      if OnceAnims[a].Animation <> nil then
        with OnceAnims[a] do
          Animation.Draw(X, Y, M_NONE);
end;

end.
