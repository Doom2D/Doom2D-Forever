unit LclHelpers;

{$MODE OBJFPC}

interface

uses
  StdCtrls;

type

  TListBoxHelper = class helper for TListBox
  public
    function IsItemClick(): Boolean;
  end;

implementation

uses
  Controls;

function TListBoxHelper.IsItemClick(): Boolean;
begin
  Result := ItemIndex = ItemAtPos(ScreenToControl(Mouse.CursorPos), True);
end;

end.

