unit f_choosetype;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TChooseTypeForm = class (TForm)
  var
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

{$R *.lfm}

initialization
  ChooseTypeForm := TChooseTypeForm.Create(Application);
end.
