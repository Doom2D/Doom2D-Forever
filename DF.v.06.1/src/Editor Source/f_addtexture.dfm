object AddTextureForm: TAddTextureForm
  Left = 229
  Top = 112
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1090#1077#1082#1089#1090#1091#1088#1091
  ClientHeight = 268
  ClientWidth = 470
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 83
    Height = 13
    Caption = #1057#1087#1080#1089#1086#1082' WAD'#39#1086#1074':'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 108
    Height = 13
    Caption = #1057#1087#1080#1089#1086#1082' '#1089#1077#1082#1094#1080#1081' WAD:'
  end
  object bOK: TButton
    Left = 40
    Top = 240
    Width = 75
    Height = 25
    Caption = #1054#1050
    Default = True
    TabOrder = 0
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 128
    Top = 240
    Width = 75
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 1
    OnClick = bCancelClick
  end
  object Panel1: TPanel
    Left = 208
    Top = 8
    Width = 261
    Height = 260
    BevelInner = bvLowered
    TabOrder = 2
    object iPreview: TImage
      Left = 2
      Top = 2
      Width = 256
      Height = 256
      Hint = #1055#1088#1077#1076#1074#1072#1088#1080#1090#1077#1083#1100#1085#1099#1081' '#1087#1088#1086#1089#1084#1086#1090#1088' '#1090#1077#1082#1089#1090#1091#1088#1099
    end
  end
  object cbWADList: TComboBox
    Left = 8
    Top = 24
    Width = 193
    Height = 21
    AutoComplete = False
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = cbWADListChange
  end
  object lbResourcesList: TListBox
    Left = 8
    Top = 88
    Width = 193
    Height = 145
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 4
    OnClick = lbResourcesListClick
  end
  object cbSectionsList: TComboBox
    Left = 8
    Top = 64
    Width = 193
    Height = 21
    AutoComplete = False
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    OnChange = cbSectionsListChange
  end
end
