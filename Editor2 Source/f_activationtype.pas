unit f_activationtype;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TActivationTypeForm = class(TForm)
    cbPlayerCollide: TCheckBox;
    cbMonsterCollide: TCheckBox;
    cbPlayerPress: TCheckBox;
    bOK: TButton;
    cbMonsterPress: TCheckBox;
    cbShot: TCheckBox;
    cbNoMonster: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ActivationTypeForm: TActivationTypeForm;

implementation

{$R *.dfm}

end.
