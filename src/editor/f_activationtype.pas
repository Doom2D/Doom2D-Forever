unit f_activationtype;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TActivationTypeForm = class (TForm)
    cbPlayerCollide: TCheckBox;
    cbMonsterCollide: TCheckBox;
    cbPlayerPress: TCheckBox;
    cbMonsterPress: TCheckBox;
    cbShot: TCheckBox;
    cbNoMonster: TCheckBox;
    bOK: TButton;

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ActivationTypeForm: TActivationTypeForm;

implementation

{$R *.lfm}

end.
