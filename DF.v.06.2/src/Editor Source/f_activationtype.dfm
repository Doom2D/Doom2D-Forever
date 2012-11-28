object ActivationTypeForm: TActivationTypeForm
  Left = 231
  Top = 196
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = #1058#1080#1087' '#1072#1082#1090#1080#1074#1072#1094#1080#1080
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
  object cbPlayerCollide: TCheckBox
    Left = 2
    Top = 0
    Width = 97
    Height = 17
    Caption = #1048#1075#1088#1086#1082' '#1073#1083#1080#1079#1082#1086
    TabOrder = 0
  end
  object cbMonsterCollide: TCheckBox
    Left = 2
    Top = 16
    Width = 97
    Height = 17
    Caption = #1052#1086#1085#1089#1090#1088' '#1073#1083#1080#1079#1082#1086
    TabOrder = 1
  end
  object cbPlayerPress: TCheckBox
    Left = 2
    Top = 32
    Width = 97
    Height = 17
    Caption = #1048#1075#1088#1086#1082' '#1085#1072#1078'.'
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
