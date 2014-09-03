object KeysForm: TKeysForm
  Left = 392
  Top = 191
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = #1050#1083#1102#1095#1080
  ClientHeight = 82
  ClientWidth = 124
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object cbRedKey: TCheckBox
    Left = 2
    Top = 0
    Width = 97
    Height = 17
    Caption = #1050#1088#1072#1089#1085#1099#1081' '#1082#1083#1102#1095
    TabOrder = 0
  end
  object cbGreenKey: TCheckBox
    Left = 2
    Top = 16
    Width = 97
    Height = 17
    Caption = #1047#1077#1083#1077#1085#1099#1081' '#1082#1083#1102#1095
    TabOrder = 1
  end
  object cbBlueKey: TCheckBox
    Left = 2
    Top = 32
    Width = 97
    Height = 17
    Caption = #1057#1080#1085#1080#1081' '#1082#1083#1102#1095
    TabOrder = 2
  end
  object bOK: TButton
    Left = 2
    Top = 56
    Width = 121
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
end
