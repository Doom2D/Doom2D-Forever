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

end.
