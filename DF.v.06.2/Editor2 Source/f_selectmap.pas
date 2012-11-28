unit f_selectmap;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSelectMapForm = class(TForm)
    lbMapList: TListBox;
    Panel1: TPanel;
    bOK: TButton;
    bCancel: TButton;
    procedure GetMaps(FileName: string);
    procedure FormActivate(Sender: TObject);
    procedure lbMapListClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SelectMapForm: TSelectMapForm;

implementation

uses
  MAPREADER, WADEDITOR, WADSTRUCT, MAPSTRUCT;

{$R *.dfm}

procedure TSelectMapForm.FormActivate(Sender: TObject);
begin
 bOK.Enabled := False;
end;

procedure TSelectMapForm.lbMapListClick(Sender: TObject);
begin
 if lbMapList.ItemIndex <> -1 then bOK.Enabled := True;
end;

procedure TSelectMapForm.bCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TSelectMapForm.GetMaps(FileName: string);
var
  WAD: TWADEditor_1;
  a: Integer;
  ResList: SArray;
  Data: Pointer;
  Len: Integer;
  Sign: array[0..2] of Char;
begin
 lbMapList.Items.Clear;

 WAD := TWADEditor_1.Create;
 if not WAD.ReadFile(FileName) then
 begin
  WAD.Destroy;
  Exit;
 end;

 ResList := WAD.GetResourcesList('');

 if ResList <> nil then
  for a := 0 to High(ResList) do
  begin
   if not WAD.GetResource('', ResList[a], Data, Len) then Continue;
   CopyMemory(@Sign[0], Data, 3);
   FreeMem(Data);
   
   if Sign = MAP_SIGNATURE then lbMapList.Items.Add(ResList[a]);
   Sign := '';
  end;

 WAD.Destroy;
end;

end.
