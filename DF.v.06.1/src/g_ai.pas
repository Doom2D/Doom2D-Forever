unit g_ai;

{.$DEFINE DEBUG}

interface

uses
  g_basic, e_graphics;

const
  AI_MOVE_NONE   = 0;
  AI_MOVE_PANIC  = 1;
  AI_MOVE_GOTO   = 2;
  AI_MOVE_RUNOUT = 3;
  AI_MOVE_COVER  = 4;
  AI_MOVE_AIM    = 5;
//  AI_MOVE_STOP   = 6;

  AI_SUBTASK_WP = 0;

type
  TWaypoint = record
   X, Y: Integer;
   WaypointType: Byte;
  end;

  TLink = record
   a, b: Word;
   LinkType: Byte;
  end;

  TAIFlag = record
   Name: string;
   Value: string;
  end;

  TAIMove = record
   MoveType: Byte;
   case Byte of
    AI_MOVE_PANIC:  ();
    AI_MOVE_GOTO:   (TargetPoint: TPoint;
                     MoveMode: Byte;
                     Stop: Boolean);
    AI_MOVE_RUNOUT: ();
    AI_MOVE_COVER:  (ShotY: Integer);
    AI_MOVE_AIM:    (TargetUID: Word);
//    AI_MOVE_STOP:   (StopPoint: TPoint);
  end;

  TAISubTask = record
   SubTaskType: Byte;
   case Byte of
    AI_SUBTASK_WP: (WaypointID: Word;
                    LinkType: Byte;
                    TimeOut: Word;);
  end;

  TAITask = record
   CurrentSubTask: Word;
   SubTask: array of TAISubTask;
  end;

procedure g_AI_Init();
procedure g_AI_Free();
procedure g_AI_CreateWaypoint(fX, fY: Integer; fWaypointType: Byte);
procedure g_AI_CreateLink(fa, fb: Integer; fLinkType: Byte);
function g_AI_CollideWaypoint(X, Y: Integer; Width, Height: Word): Boolean;
function g_AI_CollideWaypointsList(X, Y: Integer; Width, Height: Word): WArray;
function g_AI_GetTask(a, b: Word; var Task: TAITask): Boolean;
function g_AI_GetWaypointPos(ID: Word): TPoint;
procedure g_AI_Update();
procedure g_AI_Draw();

implementation

uses
  MAPDEF, Math, g_console, SysUtils;

const
  Inf = High(Word);
  MaxW = High(Word)-1;
  MaxT = 480;
  MinT = 120;

var
  Waypoints: array of TWaypoint = nil;
  Links: array of TLink = nil;

function GetWeigth(wp1, wp2: TWaypoint; LinkType: Byte): Word;
begin
 Result := Min(Abs(wp1.X-wp2.X)+Abs(wp1.Y-wp2.Y), MaxW);

 if LinkType = LINK_FALL then Result := Max(1, Round(Result/2));
 if LinkType = LINK_JUMP then Result := Max(1, Round(Result/1.5));
end;

function GetTimeOut(wp1, wp2: TWaypoint; LinkType: Byte): Word;
begin
 Result := Min(Round((Abs(wp1.X-wp2.X)*0.7+Abs(wp1.Y-wp2.Y)*0.25)), MaxT);
 {if Result > 32 then} if Result < 100 then Result := MinT;
end;

function FindPath(_a, _b: Word): WArray;
var
  C: array of array of Word;
  D: array of Word;
  flag: array of Boolean;
  pred: array of SmallInt;

  N: Word;
  i, j, k, m: Integer;
begin
 Result := nil;

 if Waypoints = nil then Exit;
 if Links = nil then Exit;

 N := Length(Waypoints);

 SetLength(C, N);
 for i := 0 to N-1 do
  begin
   SetLength(C[i], N);

   for j := 0 to N-1 do
    if i <> j then C[i, j] := Inf else C[i, j] := 0;
  end;

 for i := 0 to High(Links) do
  with Links[i] do
  begin
   C[a, b] := GetWeigth(Waypoints[_a], Waypoints[_b], LinkType);
   if (LinkType = LINK_WALK) and (Waypoints[b].Y-Waypoints[a].Y < 25) then C[b, a] := C[a, b];
  end;

 SetLength(D, N);
 SetLength(flag, N);
 SetLength(pred, N);

 for i := 0 to N-1 do
 begin
  pred[i] := _a;
  flag[i] := False;
  D[i] := C[_a, i];
 end;

 flag[_a] := True;
 pred[_a] := 0;

 for i := 0 to N-2 do
 begin
  k := -1;
  for j := 0 to N-1 do
   if flag[j] = False then
   begin
    if k = -1 then m := Inf else m := D[k];
    if m > D[j] then k := j;
   end;

  if k = -1 then Continue;

  flag[k] := True;

  for j := 0 to N-1 do
   if (flag[j] = False) and (D[j] > D[k]+C[k, j]) then
   begin
    D[j] := D[k]+C[k, j];
    pred[j] := k;
   end;
 end;

 i := _b;
 repeat
  SetLength(Result, Length(Result)+1);
  Result[High(Result)] := i;
  i := pred[i];
 until i = _a;

 for i := 0 to (Length(Result) div 2)-1 do
 begin
  j := Result[i];
  Result[i] := Result[High(Result)-i];
  Result[High(Result)-i] := j;
 end;
end;

procedure g_AI_Init();
begin

end;

procedure g_AI_Free();
begin
 Waypoints := nil;
 Links := nil;
end;

procedure g_AI_CreateWaypoint(fX, fY: Integer; fWaypointType: Byte);
begin
 SetLength(Waypoints, Length(Waypoints)+1);

 with Waypoints[High(Waypoints)] do
 begin
  X := fX;
  Y := fY;
  WaypointType := fWaypointType;
 end;
end;

procedure g_AI_CreateLink(fa, fb: Integer; fLinkType: Byte);
begin
 if (fa = fb) or (fa < 0) or (fa > High(Waypoints)) or
    (fb < 0) or (fb > High(Waypoints)) then Exit;

 SetLength(Links, Length(Links)+1);

 with Links[High(Links)] do
 begin
  a := fa;
  b := fb;
  LinkType := fLinkType;
 end;
end;

function g_AI_CollideWaypoint(X, Y: Integer; Width, Height: Word): Boolean;
var
  a: Integer;
begin
 Result := False;

 if Waypoints = nil then Exit;

 for a := 0 to High(Waypoints) do
  if g_CollidePoint(Waypoints[a].X, Waypoints[a].Y, X, Y, Width, Height) then
  begin
   Result := True;
   Break;
  end;
end;

function g_AI_CollideWaypointsList(X, Y: Integer; Width, Height: Word): WArray;
var
  a: Integer;
begin
 Result := nil;

 if Waypoints = nil then Exit;

 for a := 0 to High(Waypoints) do
  if g_CollidePoint(Waypoints[a].X, Waypoints[a].Y, X, Y, Width, Height) then
  begin
   SetLength(Result, Length(Result)+1);
   Result[High(Result)] := a;
  end;
end;

function g_AI_GetTask(a, b: Word; var Task: TAITask): Boolean;
var
  wplist: WArray;
  c, d, e: Integer;
begin
 Result := False;

 if b = 65535 then b := Random(Length(Waypoints));

 if a = b then
 begin
  SetLength(Task.SubTask, 1);
  with Task.SubTask[0] do
  begin
   SubTaskType := AI_SUBTASK_WP;
   WaypointID := a;
   LinkType := LINK_WALK;
   TimeOut := 120;
  end;
  Task.CurrentSubTask := 0;

  Result := True;
  Exit;
 end;

 wplist := FindPath(a, b);
 if wplist = nil then Exit;

 SetLength(Task.SubTask, Length(wplist));

 for c := 0 to High(wplist) do
  with Task.SubTask[c] do
  begin
   SubTaskType := AI_SUBTASK_WP;
   WaypointID := wplist[c];
   if c = 0 then e := a else e := wplist[c-1];
   for d := 0 to High(Links) do
   begin
    if ((Links[d].a = e) and (Links[d].b = wplist[c])) or
       ((Links[d].LinkType = LINK_WALK) and
        (Links[d].b = e) and (Links[d].a = wplist[c])) then
    begin
     LinkType := Links[d].LinkType;
     Break;
    end;
   end;
   TimeOut := GetTimeOut(Waypoints[e], Waypoints[wplist[c]], LinkType);
  end;

 Task.CurrentSubTask := 0;

 Result := True;
end;

function g_AI_GetWaypointPos(ID: Word): TPoint;
begin
 {$IFDEF DEBUG} Assert(ID <= High(Waypoints)); {$ENDIF}
 Result.X := Waypoints[ID].X;
 Result.Y := Waypoints[ID].Y;
end;

procedure g_AI_Update();
begin
end;

procedure g_AI_Draw();
var
  a: Integer;
begin
 {$IFNDEF DEBUG} Exit; {$ENDIF}

 if Waypoints = nil then Exit;

 for a := 0 to High(Waypoints) do
  with Waypoints[a] do
   if WaypointType <> WAYPOINT_NONE then
   begin
    e_DrawFillQuad(X-8, Y-8, X+8, Y+8, 0, 0, 255, 0, False);
    e_DrawPoint(2, X, Y, 255, 0, 0);
   end;
end;

end.
