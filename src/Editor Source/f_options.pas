unit f_options;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, IniFiles;

type
  TOptionsForm = class(TForm)
    ColorDialog: TColorDialog;
    GroupBox1: TGroupBox;
    Shape1: TShape;
    bOK: TButton;
    bCancel: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    UpDown1: TUpDown;
    Edit1: TEdit;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Shape2: TShape;
    Button2: TButton;
    Label4: TLabel;
    Shape3: TShape;
    Button3: TButton;
    Label5: TLabel;
    ComboBox1: TComboBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses f_main;

{$R *.dfm}

procedure TOptionsForm.Button1Click(Sender: TObject);
begin
 if ColorDialog.Execute then
  Shape1.Brush.Color := ColorDialog.Color;
end;

procedure TOptionsForm.FormActivate(Sender: TObject);
begin
 Shape1.Brush.Color := PointColor;
 CheckBox1.Checked := PointEnable;
 CheckBox2.Checked := DrawTexturePanel;
 CheckBox3.Checked := DrawPanelSize;
 Edit1.Text := IntToStr(PointStep);
 Shape2.Brush.Color := BackColor;
 Shape3.Brush.Color := PreviewBGColor;
 if Scale = 1 then ComboBox1.ItemIndex := 0
  else ComboBox1.ItemIndex := 1;
end;

procedure TOptionsForm.bOKClick(Sender: TObject);
var
  Ini: TIniFile;
begin
 Ini := TIniFile.Create(EditorDir+'\Editor.ini');
 Ini.WriteInteger('Editor', 'PointColor', Shape1.Brush.Color);
 Ini.WriteBool('Editor', 'PointEnable', CheckBox1.Checked);
 Ini.WriteInteger('Editor', 'PointStep', StrToInt(Edit1.Text));
 Ini.WriteBool('Editor', 'DrawTexturePanel', CheckBox2.Checked);
 Ini.WriteBool('Editor', 'DrawPanelSize', CheckBox3.Checked);
 Ini.WriteBool('Editor', 'PointEnable', CheckBox1.Checked);
 Ini.WriteInteger('Editor', 'BackColor', Shape2.Brush.Color);
 Ini.WriteInteger('Editor', 'PreviewColor', Shape3.Brush.Color);
 Ini.WriteInteger('Editor', 'Scale', ComboBox1.ItemIndex);
 PointColor := Shape1.Brush.Color;
 PointEnable := CheckBox1.Checked;
 DrawTexturePanel := CheckBox2.Checked;
 DrawPanelSize := CheckBox3.Checked;
 PointStep := StrToInt(Edit1.Text);
 BackColor := Shape2.Brush.Color;
 PreviewBGColor := Shape3.Brush.Color;
 if ComboBox1.ItemIndex = 0 then Scale := 1
  else Scale := 2;
 Ini.Destroy;
 Close;
end;

procedure TOptionsForm.bCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TOptionsForm.Button2Click(Sender: TObject);
begin
 if ColorDialog.Execute then
  Shape2.Brush.Color := ColorDialog.Color;
end;

procedure TOptionsForm.Button3Click(Sender: TObject);
begin
 if ColorDialog.Execute then
  Shape3.Brush.Color := ColorDialog.Color;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
begin
 Ini := TIniFile.Create(EditorDir+'\Editor.ini');
 Shape1.Brush.Color := Ini.ReadInteger('Editor', 'PointColor', $FFFFFF);
 CheckBox1.Checked := Ini.ReadBool('Editor', 'PointEnable', True);
 CheckBox2.Checked := Ini.ReadBool('Editor', 'DrawTexturePanel', True);
 CheckBox3.Checked := Ini.ReadBool('Editor', 'DrawPanelSize', True);
 Edit1.Text := IntToStr(Ini.ReadInteger('Editor', 'PointStep', 16));
 Shape2.Brush.Color := Ini.ReadInteger('Editor', 'BackColor', $7F6040);
 Shape3.Brush.Color := Ini.ReadInteger('Editor', 'PreviewColor', $00FF00);
 if Ini.ReadInteger('Editor', 'Scale', 0) = 0 then ComboBox1.ItemIndex := 0
  else ComboBox1.ItemIndex := 1;

 PointColor := Shape1.Brush.Color;
 PointEnable := CheckBox1.Checked;
 DrawTexturePanel := CheckBox2.Checked;
 DrawPanelSize := CheckBox3.Checked;
 PointStep := StrToInt(Edit1.Text);
 BackColor := Shape2.Brush.Color;
 PreviewBGColor := Shape3.Brush.Color;
 if ComboBox1.ItemIndex = 0 then Scale := 1
  else Scale := 2;

 Ini.Destroy;
end;

end.
