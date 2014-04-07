unit f_about;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, ShellAPI;

type
  TAboutForm = class (TForm)
    PanelAbout: TPanel;
    LabelTitle: TLabel;
    LabelVer: TLabel;
    LabelAuthor: TLabel;
    LabelMail: TLabel;
    LabelSite: TLabel;
    LabelHttp: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    ButtonOK: TButton;
    LabelAuthor2: TLabel;
    LabelMail2: TLabel;
    
    procedure LabelMailClick(Sender: TObject);
    procedure LabelMailMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure LabelMailMouseLeave(Sender: TObject);
    procedure LabelHttpClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure LabelMail2Click(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

procedure TAboutForm.LabelMailClick(Sender: TObject);
begin
  ShellExecute(AboutForm.Handle, PChar('open'), PChar('mailto:rmw.falcon@mail.ru'), '', '', 0);
end;

procedure TAboutForm.LabelMailMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  (Sender as TLabel).Font.Color := clBlue;
end;

procedure TAboutForm.LabelMailMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Color := clWindowText;
end;

procedure TAboutForm.LabelHttpClick(Sender: TObject);
begin
  ShellExecute(AboutForm.Handle, PChar('open'), PChar('www.doom2d.org'), '', '', 0);
end;

procedure TAboutForm.ButtonOKClick(Sender: TObject);
begin
  Close();
end;

procedure TAboutForm.LabelMail2Click(Sender: TObject);
begin
  ShellExecute(AboutForm.Handle, PChar('open'), PChar('mailto:pssxx@mail.ru'), '', '', 0);
end;

end.
