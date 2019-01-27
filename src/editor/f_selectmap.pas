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
  MAPREADER, MAPSTRUCT, g_resources, sfs;

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
    data: PByte;
    list: TSFSFileList;
    sign: Array [0..2] of Char;
    i, len: Integer;
begin
  lbMapList.Items.Clear();

  list := SFSFileList(FileName);
  if list = nil then Exit;

  for i := 0 to list.Count - 1 do
  begin
    g_ReadResource(FileName, win2utf(list.Files[i].path), win2utf(list.Files[i].name), data, len);

    if len >= 3 then
    begin
      sign[0] := chr(data[0]);
      sign[1] := chr(data[1]);
      sign[2] := chr(data[2]);
      if sign = MAP_SIGNATURE then
        lbMapList.Items.Add(win2utf(list.Files[i].name))
    end;

    if len > 0 then FreeMem(data)
  end;

  list.Destroy
end;

end.
