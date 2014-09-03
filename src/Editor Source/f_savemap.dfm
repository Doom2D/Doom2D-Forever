object SaveMapForm: TSaveMapForm
  Left = 235
  Top = 190
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1082#1072#1088#1090#1091
  ClientHeight = 239
  ClientWidth = 152
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object lbMapList: TListBox
    Left = 0
    Top = 25
    Width = 152
    Height = 186
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbMapListClick
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 152
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object eMapName: TEdit
      Left = 0
      Top = 0
      Width = 153
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 16
      TabOrder = 0
      OnChange = eMapNameChange
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 211
    Width = 152
    Height = 28
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object bOK: TButton
      Left = 2
      Top = 2
      Width = 72
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = bOKClick
    end
    object bCancel: TButton
      Left = 79
      Top = 2
      Width = 72
      Height = 25
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      TabOrder = 1
    end
  end
end
