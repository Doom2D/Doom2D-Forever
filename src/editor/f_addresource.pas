unit f_addresource;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ExtDlgs, e_Log, e_textures, WADEDITOR;

type
  TOKFunction = function: Boolean;

  TAddResourceForm = class (TForm)
    LabelWADs: TLabel;
    cbWADList: TComboBox;
    LabelSections: TLabel;
    cbSectionsList: TComboBox;
    lbResourcesList: TListBox;
    bOK: TButton;
    bCancel: TButton;

    procedure FormActivate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure cbWADListChange(Sender: TObject);
    procedure cbSectionsListChange(Sender: TObject);
    procedure lbResourcesListClick(Sender: TObject);

  protected
    FResourceName: String;
    FFullResourceName: String;
    FResourceSelected: Boolean;
    FOKFunction: TOKFunction;

  public
    property ResourceName: String read FResourceName;
    property FullResourceName: String read FFullResourceName;
    property OKFunction: TOKFunction read FOKFunction write FOKFunction;
  end;

var
  AddResourceForm: TAddResourceForm;

implementation

uses
  f_main, WADSTRUCT, g_language, utils, sfs, g_options;

{$R *.lfm}

const
  STANDART_WAD = 'standart.wad';

procedure TAddResourceForm.FormActivate(Sender: TObject);
  var
    SR: TSearchRec;
begin
  cbWADList.Clear();
  cbSectionsList.Clear();
  lbResourcesList.Clear();

  FResourceName := '';
  FFullResourceName := '';
  FResourceSelected := False;

  if FindFirst(WadsDir + DirectorySeparator + '*.*', faAnyFile, SR) = 0 then
  repeat
    if (SR.name <> '.') and (SR.name <> '..') then
      cbWADList.Items.Add(SR.Name);
  until FindNext(SR) <> 0;
  FindClose(SR);

// "standart.wad" в начало списка:
  if cbWADList.Items.IndexOf(STANDART_WAD) > 0 then
  begin
    cbWADList.Items.Delete(cbWADList.Items.IndexOf(STANDART_WAD));
    cbWADList.Items.Insert(0, STANDART_WAD);
  end;

// WAD карты:
  if OpenedMap <> '' then
    cbWADList.Items.Add(_lc[I_WAD_SPECIAL_MAP]);
end;

procedure TAddResourceForm.bOKClick(Sender: TObject);
begin
  if FResourceName = '' then
  begin
    MessageBox(0, PChar(_lc[I_MSG_CHOOSE_RES]),
               PChar(_lc[I_MSG_ERROR]), MB_OK + MB_ICONERROR);
    Exit;
  end;

  if @FOKFunction <> nil then
    begin
      if FOKFunction() then
        Close();
    end
  else
    Close();
end;

procedure TAddResourceForm.cbWADListChange(Sender: TObject);
  var
    wad: TSFSFileList;
    i: Integer;
    FileName, Section, sn, rn: String;
begin
  if cbWADList.Text <> _lc[I_WAD_SPECIAL_MAP] then
    FileName := WadsDir + DirectorySeparator + cbWADList.Text (* Resource wad *)
  else
    g_ProcessResourceStr(OpenedMap, FileName, sn, rn); (* Map wad *)

  cbSectionsList.Clear();
  lbResourcesList.Clear();

  wad := SFSFileList(FileName);
  if wad <> nil then
  begin
    for i := 0 to wad.Count - 1 do
    begin
      Section := win2utf(Copy(wad.Files[i].path, 1, Length(wad.Files[i].path) - 1));
      if cbSectionsList.Items.IndexOf(Section) = -1 then
        cbSectionsList.Items.Add(Section)
    end;
    wad.Destroy
  end;

  (* Update resource list (see below) *)
  cbSectionsListChange(Sender)
end;

procedure TAddResourceForm.cbSectionsListChange(Sender: TObject);
  var
    wad: TSFSFileList;
    i: Integer;
    FileName, Section, SectionName, sn, rn: String;
begin
  if cbWADList.Text <> _lc[I_WAD_SPECIAL_MAP] then
    FileName := WadsDir + DirectorySeparator + cbWADList.Text (* Resource wad *)
  else
    g_ProcessResourceStr(OpenedMap, FileName, sn, rn); (* Map wad *)

  SectionName := cbSectionsList.Text;
  lbResourcesList.Clear();

  wad := SFSFileList(FileName);
  if wad <> nil then
  begin
    for i := 0 to wad.Count - 1 do
    begin
      Section := win2utf(Copy(wad.Files[i].path, 1, Length(wad.Files[i].path) - 1));
      if Section = SectionName then
        lbResourcesList.Items.Add(win2utf(wad.Files[i].name))
    end;
    wad.Destroy
  end;
end;

procedure TAddResourceForm.lbResourcesListClick(Sender: TObject);
  var
    FileName, fn: String;
begin
  FResourceSelected := (lbResourcesList.SelCount > 0) or (lbResourcesList.ItemIndex > -1);
  if not FResourceSelected then
  begin
    FResourceName := '';
    FFullResourceName := '';
    Exit;
  end;

  if cbWADList.Text[1] <> '<' then
    FileName := cbWADList.Text
  else
    FileName := '';

  FResourceName := FileName + ':' + cbSectionsList.Text + '\' + lbResourcesList.Items[lbResourcesList.ItemIndex];

  g_ProcessResourceStr(OpenedMap, @fn, nil, nil);
  if FileName <> '' then
    FFullResourceName := WadsDir + DirectorySeparator + FResourceName
  else
    FFullResourceName := fn + FResourceName
end;

end.
