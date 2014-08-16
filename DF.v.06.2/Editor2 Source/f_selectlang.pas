unit f_selectlang;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TSelectLanguageForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure FormActivate(Sender: TObject);
    
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SelectLanguageForm: TSelectLanguageForm;

implementation

{$R *.dfm}

procedure TSelectLanguageForm.FormActivate(Sender: TObject);
begin
  Button1.ModalResult := 1;
  Button2.ModalResult := 2;
end;

end.
