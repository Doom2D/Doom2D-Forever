unit f_about;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ShellAPI;

type
  TAboutForm = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    procedure Label4Click(Sender: TObject);
    procedure Label4MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Label4MouseLeave(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

procedure TAboutForm.Label4Click(Sender: TObject);
begin
 ShellExecute(AboutForm.Handle, PChar('open'), PChar('mailto:rmw.falcon@mail.ru'), '', '', 0);
end;

procedure TAboutForm.Label4MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
 (Sender as TLabel).Font.Color := clBlue;
end;

procedure TAboutForm.Label4MouseLeave(Sender: TObject);
begin
 (Sender as TLabel).Font.Color := clWindowText;
end;

procedure TAboutForm.Label6Click(Sender: TObject);
begin
 ShellExecute(AboutForm.Handle, PChar('open'), PChar('www.doom2d.org'), '', '', 0);
end;

procedure TAboutForm.Button1Click(Sender: TObject);
begin
 Close;
end;

end.
