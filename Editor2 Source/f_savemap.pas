unit f_savemap;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSaveMapForm = class(TForm)
    Panel2: TPanel;
    eMapName: TEdit;
    lbMapList: TListBox;
    Panel1: TPanel;
    bOK: TButton;
    bCancel: TButton;
    procedure GetMaps(FileName: string);
    procedure FormActivate(Sender: TObject);
    procedure eMapNameChange(Sender: TObject);
    procedure lbMapListClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SaveMapForm: TSaveMapForm;

implementation

uses
  MAPREADER, WADEDITOR, WADSTRUCT, MAPSTRUCT;

{$R *.dfm}

procedure TSaveMapForm.FormActivate(Sender: TObject);
begin
 bOK.Enabled := False;
 eMapName.Text := '';
end;

procedure TSaveMapForm.eMapNameChange(Sender: TObject);
begin
 if eMapName.Text <> '' then bOK.Enabled := True
  else bOK.Enabled := False;
end;

procedure TSaveMapForm.lbMapListClick(Sender: TObject);
begin
 if lbMapList.ItemIndex > -1 then 
  eMapName.Text := lbMapList.Items[lbMapList.ItemIndex];
end;

procedure TSaveMapForm.bOKClick(Sender: TObject);
var
  a: Integer;
  ok: Boolean;
begin
 ok := True;
 for a := 0 to lbMapList.Count-1 do
  if eMapName.Text = lbMapList.Items[a] then
  begin
   ok := MessageBox(0, PChar(Format('Карта %s уже существует, заменить ?', [eMapName.Text])),
                    'Сохранить карту', MB_ICONQUESTION or MB_YESNO
                    or MB_TASKMODAL or MB_DEFBUTTON1) = mrYes;
   if not ok then Exit;
   Break;
  end;

 if ok then SaveMapForm.ModalResult := mrOk
 else SaveMapForm.ModalResult := mrCancel;
end;

procedure TSaveMapForm.GetMaps(FileName: string);
var
  WAD: TWADEditor_1;
  a: Integer;
  ResList: SArray;
  Data: Pointer;
  Len: Integer;
  Sign: array[0..2] of Char;
begin
 lbMapList.Items.Clear;

 WAD := TWADEditor_1.Create;
 if not WAD.ReadFile(FileName) then
 begin
  WAD.Destroy;
  Exit;
 end;

 ResList := WAD.GetResourcesList('');

 if ResList <> nil then
  for a := 0 to High(ResList) do
  begin
   if not WAD.GetResource('', ResList[a], Data, Len) then Continue;
   CopyMemory(@Sign[0], Data, 3);
   FreeMem(Data);
   
   if Sign = MAP_SIGNATURE then lbMapList.Items.Add(ResList[a]);
   Sign := '';
  end;

 WAD.Destroy;
end;

end.
