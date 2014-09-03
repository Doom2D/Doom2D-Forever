object MapTestForm: TMapTestForm
  Left = 291
  Top = 194
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1090#1077#1089#1090#1072' '#1082#1072#1088#1090#1099' '#1074' D2D:F'
  ClientHeight = 270
  ClientWidth = 367
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object bOK: TButton
    Left = 192
    Top = 240
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 286
    Top = 240
    Width = 75
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 1
    OnClick = bCancelClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 0
    Width = 353
    Height = 233
    TabOrder = 2
    object LabelTime: TLabel
      Left = 16
      Top = 112
      Width = 83
      Height = 13
      Caption = #1051#1080#1084#1080#1090' '#1074#1088#1077#1084#1077#1085#1080':'
    end
    object LabelSecs: TLabel
      Left = 176
      Top = 112
      Width = 35
      Height = 13
      Caption = #1089#1077#1082#1091#1085#1076
    end
    object LabelScore: TLabel
      Left = 16
      Top = 136
      Width = 68
      Height = 13
      Caption = #1051#1080#1084#1080#1090' '#1086#1095#1082#1086#1074':'
    end
    object LabelPath: TLabel
      Left = 16
      Top = 184
      Width = 107
      Height = 13
      Caption = #1055#1091#1090#1100' '#1082' Doom2DF.exe:'
    end
    object rbDM: TRadioButton
      Left = 16
      Top = 16
      Width = 113
      Height = 17
      Caption = 'Deathmatch'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbTDM: TRadioButton
      Left = 16
      Top = 32
      Width = 113
      Height = 17
      Caption = 'Team Deathmatch'
      TabOrder = 1
    end
    object rbCTF: TRadioButton
      Left = 16
      Top = 48
      Width = 113
      Height = 17
      Caption = 'Capture the Flag'
      TabOrder = 2
    end
    object rbCOOP: TRadioButton
      Left = 16
      Top = 64
      Width = 113
      Height = 17
      Caption = 'Cooperative'
      TabOrder = 3
    end
    object cbTwoPlayers: TCheckBox
      Left = 176
      Top = 16
      Width = 169
      Height = 17
      Caption = #1044#1074#1072' '#1080#1075#1088#1086#1082#1072
      TabOrder = 4
    end
    object cbTeamDamage: TCheckBox
      Left = 176
      Top = 32
      Width = 169
      Height = 17
      Caption = #1059#1088#1086#1085' '#1089#1074#1086#1077#1081' '#1082#1086#1084#1072#1085#1076#1077
      TabOrder = 5
    end
    object cbAllowExit: TCheckBox
      Left = 176
      Top = 48
      Width = 169
      Height = 17
      Caption = #1042#1099#1093#1086#1076' '#1080#1079' '#1091#1088#1086#1074#1085#1103
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object cbWeaponStay: TCheckBox
      Left = 176
      Top = 64
      Width = 169
      Height = 17
      Caption = #1054#1088#1091#1078#1080#1077' '#1086#1089#1090#1072#1077#1090#1089#1103
      TabOrder = 7
    end
    object cbMonstersDM: TCheckBox
      Left = 176
      Top = 80
      Width = 169
      Height = 17
      Caption = #1052#1086#1085#1089#1090#1088#1099' '#1074' DM'
      TabOrder = 8
    end
    object edTime: TEdit
      Left = 104
      Top = 112
      Width = 49
      Height = 21
      TabOrder = 9
      Text = '0'
    end
    object edScore: TEdit
      Left = 104
      Top = 136
      Width = 49
      Height = 21
      TabOrder = 10
      Text = '0'
    end
    object UpDown2: TUpDown
      Left = 153
      Top = 136
      Width = 12
      Height = 21
      Associate = edScore
      Max = 1000
      TabOrder = 11
    end
    object UpDown1: TUpDown
      Left = 153
      Top = 112
      Width = 12
      Height = 21
      Associate = edTime
      Max = 28800
      TabOrder = 12
    end
    object edD2dexe: TEdit
      Left = 16
      Top = 200
      Width = 297
      Height = 21
      TabOrder = 13
      Text = 'Doom2DF.exe'
    end
    object bChooseD2d: TButton
      Left = 320
      Top = 200
      Width = 25
      Height = 20
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 14
      OnClick = bChooseD2dClick
    end
    object cbMapOnce: TCheckBox
      Left = 16
      Top = 160
      Width = 225
      Height = 17
      Caption = #1047#1072#1082#1088#1099#1090#1100' '#1080#1075#1088#1091' '#1087#1086#1089#1083#1077' '#1074#1099#1093#1086#1076#1072' '#1080#1079' '#1082#1072#1088#1090#1099
      TabOrder = 15
    end
  end
  object FindD2dDialog: TOpenDialog
    DefaultExt = 'Doom2DF.exe'
    Filter = 'Doom2DF.exe|Doom2DF.exe'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Title = #1042#1099#1073#1077#1088#1080#1090#1077' '#1092#1072#1081#1083' '#1080#1075#1088#1099' Doom 2D:Forever'
    Left = 320
    Top = 144
  end
end
