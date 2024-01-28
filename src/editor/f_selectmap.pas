unit f_selectmap;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, utils;

type
  TSelectMapForm = class (TForm)
    lbMapList: TListBox;
    Panel1: TPanel;
    bOK: TButton;
    bCancel: TButton;

    procedure GetMaps(FileName: String);
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
  BinEditor, MAPREADER, WADEDITOR, WADSTRUCT, MAPSTRUCT;

{$R *.lfm}

procedure TSelectMapForm.FormActivate(Sender: TObject);
begin
  bOK.Enabled := (lbMapList.ItemIndex <> -1);
  lbMapList.SetFocus();
end;

procedure TSelectMapForm.lbMapListClick(Sender: TObject);
begin
  if lbMapList.ItemIndex <> -1 then
    bOK.Enabled := True;
end;

procedure TSelectMapForm.bCancelClick(Sender: TObject);
begin
  Close();
end;

procedure TSelectMapForm.GetMaps(FileName: String);
var
  WAD: TWADEditor_1;
  a: Integer;
  ResList: SArray;
  Data: Pointer;
  Len: Integer;
  Sign: Array [0..2] of Char;

begin
  lbMapList.Items.Clear();

  WAD := TWADEditor_1.Create();
  if not WAD.ReadFile(FileName) then
  begin
    WAD.Free();
    Exit;
  end;

  ResList := WAD.GetResourcesList('');

  if ResList <> nil then
    for a := 0 to High(ResList) do
    begin
      if not WAD.GetResource('', ResList[a], Data, Len) then
        Continue;

      CopyMemory(@Sign[0], Data, 3);
      FreeMem(Data);

      if Sign = MAP_SIGNATURE then
        lbMapList.Items.Add(win2utf(ResList[a]));
      Sign := '';
    end;

  WAD.Free();
end;

initialization
  SelectMapForm := TSelectMapForm.Create(Application);
end.
