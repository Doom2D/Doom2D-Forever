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
  MAPREADER, MAPSTRUCT, g_language, g_resources, sfs;

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
    nm: String;
    data: Pbyte;
    list: TSFSFileList;
    i, j, len, max_num: Integer;
    sign: Array [0..2] of Char;
begin
  lbMapList.Items.Clear();
  max_num := 1;

  list := SFSFileList(FileName);
  if list <> nil then
  begin
    for i := 0 to list.Count - 1 do
    begin
      g_ReadResource(FileName, win2utf(list.Files[i].path), win2utf(list.Files[i].name), data, len);

      if len >= 3 then
      begin
        sign[0] := chr(data[0]);
        sign[1] := chr(data[1]);
        sign[2] := chr(data[2]);
        if sign = MAP_SIGNATURE then
        begin
          nm := win2utf(list.Files[i].name);
          lbMapList.Items.Add(nm);
          if placeName then
          begin
            nm := UpperCase(nm);
            if (nm[1] = 'M') and (nm[2] = 'A') and (nm[3] = 'P') then
            begin
              nm := Trim(Copy(nm, 4, Length(nm)-3));
              j := StrToIntDef(nm, 0);
              if j >= max_num then
                max_num := j + 1;
            end
          end
        end
      end;

      if len > 0 then FreeMem(data)
    end;

    list.Destroy;
  end;


  if placeName then
  begin
    nm := IntToStr(max_num);
    if Length(nm) < 2 then
      nm := '0' + nm;
    eMapName.Text := 'MAP' + nm
  end
  else
  begin
    eMapName.Text := ''
  end
end;

end.
