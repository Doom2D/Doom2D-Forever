unit f_choosetype;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TChooseTypeForm = class (TForm)
    lbTypeSelect: TListBox;
    bOK: TButton;

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ChooseTypeForm: TChooseTypeForm;

implementation

{$R *.dfm}

end.
