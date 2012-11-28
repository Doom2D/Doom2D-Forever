unit f_savemap;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSaveMapForm = class(TForm)
    Panel2: TPanel;
    eMapName: TEdit;
    lbMapList: TListBox;
    Panel1: TPanel;
    bOK: TButton;
    bCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure eMapNameChange(Sender: TObject);
    procedure lbMapListClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SaveMapForm: TSaveMapForm;

implementation

{$R *.dfm}

procedure TSaveMapForm.FormActivate(Sender: TObject);
begin
 bOK.Enabled := False;
 eMapName.Text := '';
end;

procedure TSaveMapForm.eMapNameChange(Sender: TObject);
begin
 if eMapName.Text <> '' then bOK.Enabled := True
  else bOK.Enabled := False;
end;

procedure TSaveMapForm.lbMapListClick(Sender: TObject);
begin
 eMapName.Text := lbMapList.Items[lbMapList.ItemIndex];
end;

procedure TSaveMapForm.bOKClick(Sender: TObject);
var
  a: Integer;
  ok: Boolean;
begin
 ok := True;
 for a := 0 to lbMapList.Count-1 do
  if eMapName.Text = lbMapList.Items[a] then
  begin
   ok := False;
   Break;
  end;
 if not(ok) then ok := MessageDlg(Format('Карта %s уже существует, заменить ?',
                                  [eMapName.Text]), mtConfirmation, [mbYes, mbNo], 0) = mrYes;
 if ok then
  SaveMapForm.ModalResult := mrOk
 else SaveMapForm.ModalResult := mrCancel;
end;

end.
