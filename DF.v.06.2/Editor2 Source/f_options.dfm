object OptionsForm: TOptionsForm
  Left = 202
  Top = 174
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1088#1077#1076#1072#1082#1090#1086#1088#1072
  ClientHeight = 262
  ClientWidth = 415
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
  object GroupBox1: TGroupBox
    Left = 8
    Top = 0
    Width = 401
    Height = 225
    TabOrder = 0
    object sDotColor: TShape
      Left = 320
      Top = 16
      Width = 57
      Height = 25
    end
    object LabelGrid: TLabel
      Left = 8
      Top = 88
      Width = 61
      Height = 13
      Caption = #1064#1072#1075#1080' '#1089#1077#1090#1082#1080':'
    end
    object LabelGridCol: TLabel
      Left = 192
      Top = 16
      Width = 60
      Height = 13
      Caption = #1062#1074#1077#1090' '#1089#1077#1090#1082#1080':'
    end
    object LabelBack: TLabel
      Left = 192
      Top = 57
      Width = 57
      Height = 13
      Caption = #1062#1074#1077#1090' '#1092#1086#1085#1072':'
    end
    object sBackColor: TShape
      Left = 320
      Top = 57
      Width = 57
      Height = 25
      Brush.Color = clBlack
    end
    object LabelPreview: TLabel
      Left = 192
      Top = 87
      Width = 84
      Height = 39
      Caption = #1062#1074#1077#1090' '#1092#1086#1085#1072' '#1087#1086#1083#1103' '#1087#1088#1077#1076#1087#1088#1086#1089#1084#1086#1090#1088#1072' '#1090#1077#1082#1089#1090#1091#1088#1099':'
      WordWrap = True
    end
    object sPreviewColor: TShape
      Left = 320
      Top = 97
      Width = 57
      Height = 25
      Brush.Color = clBlack
    end
    object LabelMinimap: TLabel
      Left = 192
      Top = 145
      Width = 112
      Height = 13
      Caption = #1052#1072#1089#1096#1090#1072#1073' '#1084#1080#1085#1080'-'#1082#1072#1088#1090#1099':'
    end
    object LabelRecent: TLabel
      Left = 192
      Top = 176
      Width = 121
      Height = 26
      Caption = #1047#1072#1087#1086#1084#1080#1085#1072#1090#1100' '#1087#1086#1089#1083#1077#1076#1085#1080#1093' '#1086#1090#1082#1088#1099#1090#1099#1093' '#1082#1072#1088#1090':'
      WordWrap = True
    end
    object LabelLanguage: TLabel
      Left = 8
      Top = 176
      Width = 31
      Height = 13
      Caption = #1071#1079#1099#1082':'
    end
    object LabelGridSize: TLabel
      Left = 8
      Top = 136
      Width = 73
      Height = 26
      Caption = #1056#1072#1079#1084#1077#1088' '#1090#1086#1095#1077#1082' '#1089#1077#1090#1082#1080':'
      WordWrap = True
    end
    object cbShowDots: TCheckBox
      Left = 8
      Top = 16
      Width = 121
      Height = 17
      Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1089#1077#1090#1082#1091
      TabOrder = 0
    end
    object UpDown1: TUpDown
      Left = 145
      Top = 88
      Width = 12
      Height = 21
      Associate = eDotStepOne
      Min = 2
      Max = 255
      Position = 16
      TabOrder = 6
    end
    object eDotStepOne: TEdit
      Left = 88
      Top = 88
      Width = 57
      Height = 21
      TabOrder = 1
      Text = '16'
    end
    object bGrid: TButton
      Left = 378
      Top = 16
      Width = 11
      Height = 25
      Caption = '..'
      TabOrder = 2
      OnClick = bGridClick
    end
    object bBack: TButton
      Left = 378
      Top = 57
      Width = 11
      Height = 25
      Caption = '..'
      TabOrder = 3
      OnClick = bBackClick
    end
    object bPreview: TButton
      Left = 378
      Top = 97
      Width = 11
      Height = 25
      Caption = '..'
      TabOrder = 4
      OnClick = bPreviewClick
    end
    object cbScale: TComboBox
      Left = 320
      Top = 145
      Width = 70
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 5
      Text = '1x'
      Items.Strings = (
        '1x'
        '2x')
    end
    object cbShowTexture: TCheckBox
      Left = 8
      Top = 32
      Width = 169
      Height = 17
      Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1090#1077#1082#1089#1090#1091#1088#1091' '#1087#1072#1085#1077#1083#1080
      TabOrder = 7
    end
    object cbShowSize: TCheckBox
      Left = 8
      Top = 48
      Width = 169
      Height = 17
      Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1088#1072#1079#1084#1077#1088#1099' '#1087#1072#1085#1077#1083#1080
      TabOrder = 8
    end
    object eRecent: TEdit
      Left = 320
      Top = 180
      Width = 57
      Height = 21
      TabOrder = 9
      Text = '5'
    end
    object UpDown3: TUpDown
      Left = 377
      Top = 180
      Width = 12
      Height = 21
      Associate = eRecent
      Min = 2
      Max = 10
      Position = 5
      TabOrder = 10
    end
    object eDotStepTwo: TEdit
      Left = 88
      Top = 112
      Width = 57
      Height = 21
      TabOrder = 11
      Text = '8'
    end
    object UpDown2: TUpDown
      Left = 145
      Top = 112
      Width = 12
      Height = 21
      Associate = eDotStepTwo
      Min = 2
      Max = 255
      Position = 8
      TabOrder = 12
    end
    object rbRussian: TRadioButton
      Left = 88
      Top = 176
      Width = 81
      Height = 17
      Caption = #1056#1091#1089#1089#1082#1080#1081
      Checked = True
      TabOrder = 13
      TabStop = True
    end
    object rbEnglish: TRadioButton
      Left = 88
      Top = 200
      Width = 81
      Height = 17
      Caption = 'English'
      TabOrder = 14
    end
    object cbDotSize: TComboBox
      Left = 88
      Top = 140
      Width = 73
      Height = 21
      ItemHeight = 13
      TabOrder = 15
      Text = '1'
      Items.Strings = (
        '1'
        '2')
    end
  end
  object bOK: TButton
    Left = 240
    Top = 232
    Width = 75
    Height = 25
    Caption = #1054#1050
    Default = True
    TabOrder = 1
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 336
    Top = 232
    Width = 75
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 2
    OnClick = bCancelClick
  end
  object ColorDialog: TColorDialog
    Options = [cdFullOpen]
    Left = 200
    Top = 232
  end
end
