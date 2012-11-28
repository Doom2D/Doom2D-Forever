object SelectMapForm: TSelectMapForm
  Left = 186
  Top = 166
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1082#1072#1088#1090#1091
  ClientHeight = 239
  ClientWidth = 152
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object lbMapList: TListBox
    Left = 0
    Top = 0
    Width = 152
    Height = 211
    Align = alClient
    Ctl3D = True
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 0
    OnClick = lbMapListClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 211
    Width = 152
    Height = 28
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object bOK: TButton
      Left = 2
      Top = 2
      Width = 72
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
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
      OnClick = bCancelClick
    end
  end
end
