unit g_triggers;

interface

uses
  windows,
  SysUtils,
  g_basic,
  e_graphics;

type
  TTriggerType=(TRIGGER_NONE, TRIGGERST_EXIT, TRIGGERST_TELEPORT, TRIGGERST_OPENDOOR,
                TRIGGERST_DOOR5);
  TTriggerActivateType=(TAT_PLAYERCOLLIDE, TAT_MONSTERCOLLIDE, TAT_PLAYERPRESS);
  TTriggerKey=(KEY_RED, KEY_GREEN, KEY_BLUE);

  TTriggerActivateSet=Set of TTriggerActivateType;
  TTriggerActivateKeySet=Set of TTriggerKey;

  TTrigger = packed record
   X, Y:   Integer;
   Width,
   Height: Word;
   TriggerType: TTriggerType;
   TriggerActivateType: TTriggerActivateSet;
   TriggerActivateKey: TTriggerActivateKeySet;
   case Byte of
    0: (Default: Byte16);
    1: (MapName: Char16);
    2: (TeleportPoint: TPoint);
    3: (PanelID: DWORD; Direction: Byte);
  end;

  TTriggersSystem = class(TObject)
   private
    function  FindTrigger: DWORD;
    function  GetCount: Word;
   public
    TriggersArray: Array of TTrigger;
    constructor Create;
    destructor  Destroy; override;
    procedure   CreateTrigger(fTrigger: TTrigger);
    procedure   RemoveTrigger(ID: DWORD);
    procedure   RemoveAll;
    property    Count: Word read GetCount;
    procedure   Render(XInc, YInc: Integer);
  end;

implementation

uses f_main;

{ TTriggersSystem }

constructor TTriggersSystem.Create;
begin

end;

procedure TTriggersSystem.CreateTrigger(fTrigger: TTrigger);
var
  find_id: DWORD;
begin
 find_id := FindTrigger;
 //ZeroMemory(@TriggersArray[find_id], SizeOf(TTrigger));

 TriggersArray[find_id] := fTrigger;
end;

destructor TTriggersSystem.Destroy;
begin

  inherited;
end;

function TTriggersSystem.FindTrigger: DWORD;
var
  i: Integer;
begin
if TriggersArray <> nil then
for i := 0 to High(TriggersArray) do
 if TriggersArray[i].TriggerType = TRIGGER_NONE then
 begin
  Result := i;
  Exit;
 end;

if TriggersArray = nil then
begin
 SetLength(TriggersArray, 8);
 result := 0;
end
 else
begin
 Result := High(TriggersArray) + 8;
 SetLength(TriggersArray, Length(TriggersArray) + 1);
end;
end;

function TTriggersSystem.GetCount: Word;
var
  i: DWORD;
begin
 Result := 0;

 if TriggersArray = nil then Exit;

 for i := 0 to High(TriggersArray) do
  if TriggersArray[i].TriggerType <> TRIGGER_NONE then Inc(Result);
end;

procedure TTriggersSystem.RemoveAll;
begin
 TriggersArray := nil;
end;

procedure TTriggersSystem.RemoveTrigger(ID: DWORD);
begin
 TriggersArray[ID].TriggerType := TRIGGER_NONE;
end;

procedure TTriggersSystem.Render(XInc, YInc: Integer);
var
  i: DWORD;
begin
 if TriggersArray = nil then Exit;

 for i := 0 to High(TriggersArray) do
  if TriggersArray[i].TriggerType <> TRIGGER_NONE then
  with TriggersArray[i] do
  begin
   e_DrawFillQuad(X+XInc, Y+YInc, X+XInc+Width, Y+YInc+Height, 127, 127, 127, 0, False);
  end;
end;

end.
