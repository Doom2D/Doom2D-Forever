object AddResourceForm: TAddResourceForm
  Left = 271
  Top = 208
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 268
  ClientWidth = 204
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelWADs: TLabel
    Left = 4
    Top = 4
    Width = 110
    Height = 13
    Caption = #1057#1087#1080#1089#1086#1082' WAD-'#1092#1072#1081#1083#1086#1074':'
  end
  object LabelSections: TLabel
    Left = 4
    Top = 44
    Width = 143
    Height = 13
    Caption = #1057#1087#1080#1089#1086#1082' '#1089#1077#1082#1094#1080#1081' WAD-'#1092#1072#1081#1083#1072':'
  end
  object bOK: TButton
    Left = 36
    Top = 236
    Width = 75
    Height = 25
    Caption = #1054#1050
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 124
    Top = 236
    Width = 75
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 1
  end
  object cbWADList: TComboBox
    Left = 4
    Top = 20
    Width = 193
    Height = 21
    AutoComplete = False
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = cbWADListChange
  end
  object lbResourcesList: TListBox
    Left = 4
    Top = 84
    Width = 193
    Height = 145
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 3
    OnClick = lbResourcesListClick
  end
  object cbSectionsList: TComboBox
    Left = 4
    Top = 60
    Width = 193
    Height = 21
    AutoComplete = False
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 4
    OnChange = cbSectionsListChange
  end
end
