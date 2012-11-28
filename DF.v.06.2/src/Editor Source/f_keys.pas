unit f_keys;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TKeysForm = class(TForm)
    cbRedKey: TCheckBox;
    cbGreenKey: TCheckBox;
    cbBlueKey: TCheckBox;
    bOK: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  KeysForm: TKeysForm;

implementation

{$R *.dfm}

end.
