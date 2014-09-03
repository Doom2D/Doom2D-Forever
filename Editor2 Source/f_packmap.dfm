object PackMapForm: TPackMapForm
  Left = 312
  Top = 231
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1059#1087#1072#1082#1086#1074#1072#1090#1100' '#1082#1072#1088#1090#1091
  ClientHeight = 229
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 4
    Top = 4
    Width = 353
    Height = 189
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object LabelSaveTo: TLabel
      Left = 8
      Top = 8
      Width = 65
      Height = 13
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074':'
    end
    object LabelMapName: TLabel
      Left = 216
      Top = 8
      Width = 109
      Height = 13
      Caption = #1048#1084#1103' '#1088#1077#1089#1091#1088#1089#1072' ('#1082#1072#1088#1090#1099'):'
    end
    object LabelTextures: TLabel
      Left = 96
      Top = 64
      Width = 103
      Height = 13
      Caption = #1057#1077#1082#1094#1080#1103' '#1076#1083#1103' '#1090#1077#1082#1089#1090#1091#1088':'
    end
    object LabelSky: TLabel
      Left = 96
      Top = 88
      Width = 88
      Height = 13
      Caption = #1057#1077#1082#1094#1080#1103' '#1076#1083#1103' '#1085#1077#1073#1072':'
    end
    object LabelMusic: TLabel
      Left = 96
      Top = 112
      Width = 103
      Height = 13
      Caption = #1057#1077#1082#1094#1080#1103' '#1076#1083#1103' '#1084#1091#1079#1099#1082#1080':'
    end
    object eWAD: TEdit
      Left = 8
      Top = 24
      Width = 169
      Height = 21
      TabOrder = 0
    end
    object bSelectWAD: TButton
      Left = 178
      Top = 22
      Width = 23
      Height = 23
      Hint = #1042#1099#1073#1088#1072#1090#1100' '#1092#1086#1085
      Caption = '..'
      TabOrder = 1
      OnClick = bSelectWADClick
    end
    object eResource: TEdit
      Left = 216
      Top = 24
      Width = 129
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 16
      TabOrder = 2
    end
    object eTSection: TEdit
      Left = 216
      Top = 64
      Width = 129
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 16
      TabOrder = 4
    end
    object eSSection: TEdit
      Left = 216
      Top = 88
      Width = 129
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 16
      TabOrder = 6
    end
    object eMSection: TEdit
      Left = 216
      Top = 112
      Width = 129
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 16
      TabOrder = 8
    end
    object cbTextrures: TCheckBox
      Left = 8
      Top = 64
      Width = 89
      Height = 17
      Caption = #1058#1077#1082#1089#1090#1091#1088#1099
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object cbSky: TCheckBox
      Left = 8
      Top = 88
      Width = 89
      Height = 17
      Caption = #1053#1077#1073#1086
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object cbMusic: TCheckBox
      Left = 8
      Top = 112
      Width = 89
      Height = 17
      Caption = #1052#1091#1079#1099#1082#1091
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
    object cbAdd: TCheckBox
      Left = 8
      Top = 144
      Width = 193
      Height = 17
      Caption = #1053#1077' '#1087#1077#1088#1077#1079#1072#1087#1080#1089#1099#1074#1072#1090#1100' WAD'
      TabOrder = 9
    end
    object cbNonStandart: TCheckBox
      Left = 8
      Top = 160
      Width = 209
      Height = 17
      Hint = #1053#1077' '#1089#1086#1093#1088#1072#1085#1103#1090#1100' '#1088#1077#1089#1091#1088#1089#1099' '#1080#1079' '#1089#1090#1072#1085#1076#1072#1088#1090#1085#1099#1093' WAD'#39#1086#1074' - Standart.wad '#1080' '#1087#1088'.'
      Caption = #1058#1086#1083#1100#1082#1086' '#1085#1077#1089#1090#1072#1085#1076#1072#1088#1090#1085#1099#1077' '#1088#1077#1089#1091#1088#1089#1099
      TabOrder = 10
    end
  end
  object bPack: TButton
    Left = 282
    Top = 200
    Width = 75
    Height = 25
    Caption = #1059#1087#1072#1082#1086#1074#1072#1090#1100
    Default = True
    TabOrder = 1
    OnClick = bPackClick
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'wad'
    Filter = #1050#1072#1088#1090#1099' Doom2D: Forever (*.wad)|*.wad|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 8
    Top = 200
  end
end
