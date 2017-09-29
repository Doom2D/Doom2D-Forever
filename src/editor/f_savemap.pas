unit f_savemap;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, utils;

type
  TSaveMapForm = class (TForm)
    lbMapList: TListBox;
    Panel1: TPanel;
    bOK: TButton;
    bCancel: TButton;
    Panel2: TPanel;
    eMapName: TEdit;

    procedure GetMaps(FileName: String; placeName: Boolean);
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
  BinEditor, MAPREADER, WADEDITOR, WADSTRUCT, MAPSTRUCT, g_language;

{$R *.lfm}

procedure TSaveMapForm.FormActivate(Sender: TObject);
begin
  bOK.Enabled := (eMapName.Text <> '');
  eMapName.SetFocus();
end;

procedure TSaveMapForm.eMapNameChange(Sender: TObject);
begin
  if eMapName.Text <> '' then
    bOK.Enabled := True
  else
    bOK.Enabled := False;
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
      ok := MessageBox(0, PChar(Format(_lc[I_MSG_MAP_EXISTS],
                                       [eMapName.Text])),
                       PChar(_lc[I_MSG_SAVE_MAP]),
                       MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1) = mrYes;
      if not ok then
        Exit;
      Break;
    end;

  if ok then
    SaveMapForm.ModalResult := mrOk
  else
    SaveMapForm.ModalResult := mrCancel;
end;

procedure TSaveMapForm.GetMaps(FileName: String; placeName: Boolean);
var
  WAD: TWADEditor_1;
  a, max_num, j: Integer;
  ResList: SArray;
  Data: Pointer;
  Len: Integer;
  Sign: Array [0..2] of Char;
  nm: String;

begin
  lbMapList.Items.Clear();
  max_num := 1;

  WAD := TWADEditor_1.Create();
  WAD.ReadFile(FileName);
  ResList := WAD.GetResourcesList('');  

  if ResList <> nil then
    for a := 0 to High(ResList) do
    begin
      if not WAD.GetResource('', ResList[a], Data, Len) then
        Continue;

      CopyMemory(@Sign[0], Data, 3);
      FreeMem(Data);
   
      if Sign = MAP_SIGNATURE then
      begin
        nm := win2utf(ResList[a]);
        lbMapList.Items.Add(nm);

        if placeName then
        begin
          nm := UpperCase(nm);
          if (nm[1] = 'M') and
             (nm[2] = 'A') and
             (nm[3] = 'P') then
          begin
            nm := Trim(Copy(nm, 4, Length(nm)-3));
            j := StrToIntDef(nm, 0);
            if j >= max_num then
              max_num := j + 1;
          end;
        end;
      end;

      Sign := '';
    end;

  WAD.Free();

  if placeName then
    begin
      nm := IntToStr(max_num);
      if Length(nm) < 2 then
        nm := '0' + nm;
      nm := 'MAP' + nm;
      eMapName.Text := nm;
    end
  else
    eMapName.Text := '';
end;

end.
