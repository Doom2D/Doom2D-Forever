object MapOptionsForm: TMapOptionsForm
  Left = 213
  Top = 110
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1082#1072#1088#1090#1099
  ClientHeight = 219
  ClientWidth = 303
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
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 87
    Height = 13
    Caption = #1054#1087#1080#1089#1072#1085#1080#1077' '#1082#1072#1088#1090#1099':'
  end
  object leMapName: TLabeledEdit
    Left = 8
    Top = 24
    Width = 145
    Height = 21
    EditLabel.Width = 56
    EditLabel.Height = 13
    EditLabel.Caption = #1048#1084#1103' '#1082#1072#1088#1090#1099
    MaxLength = 32
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 168
    Top = 16
    Width = 129
    Height = 113
    Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072':'
    TabOrder = 5
    object lPanelCount: TLabel
      Left = 72
      Top = 16
      Width = 42
      Height = 13
      AutoSize = False
    end
    object lTextureCount: TLabel
      Left = 72
      Top = 32
      Width = 42
      Height = 13
      AutoSize = False
    end
    object lItemCount: TLabel
      Left = 72
      Top = 48
      Width = 42
      Height = 13
      AutoSize = False
    end
    object Label4: TLabel
      Left = 8
      Top = 16
      Width = 47
      Height = 13
      Caption = #1055#1072#1085#1077#1083#1077#1081':'
    end
    object Label5: TLabel
      Left = 8
      Top = 32
      Width = 44
      Height = 13
      Caption = #1058#1077#1082#1089#1090#1091#1088':'
    end
    object Label6: TLabel
      Left = 8
      Top = 48
      Width = 42
      Height = 13
      Caption = #1048#1090#1077#1084#1086#1074':'
    end
    object Label2: TLabel
      Left = 8
      Top = 64
      Width = 53
      Height = 13
      Caption = #1052#1086#1085#1089#1090#1088#1086#1074':'
    end
    object lMonsterCount: TLabel
      Left = 72
      Top = 64
      Width = 42
      Height = 13
      AutoSize = False
    end
    object Label3: TLabel
      Left = 8
      Top = 80
      Width = 52
      Height = 13
      Caption = #1054#1073#1083#1072#1089#1090#1077#1081':'
    end
    object lAreaCount: TLabel
      Left = 72
      Top = 80
      Width = 42
      Height = 13
      AutoSize = False
    end
  end
  object bOK: TButton
    Left = 136
    Top = 192
    Width = 75
    Height = 25
    Caption = #1054#1050
    Default = True
    TabOrder = 3
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 224
    Top = 192
    Width = 75
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 4
    OnClick = bCancelClick
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 136
    Width = 289
    Height = 49
    Caption = #1056#1072#1079#1084#1077#1088#1099':'
    TabOrder = 2
    object Label7: TLabel
      Left = 160
      Top = 18
      Width = 41
      Height = 13
      Caption = #1042#1099#1089#1086#1090#1072':'
    end
    object Label8: TLabel
      Left = 8
      Top = 18
      Width = 36
      Height = 13
      Caption = #1044#1083#1080#1085#1072':'
    end
    object eMapHeight: TEdit
      Left = 216
      Top = 14
      Width = 49
      Height = 21
      TabOrder = 1
      Text = '480'
    end
    object eMapWidth: TEdit
      Left = 56
      Top = 14
      Width = 49
      Height = 21
      TabOrder = 0
      Text = '640'
    end
    object UpDown2: TUpDown
      Left = 105
      Top = 14
      Width = 13
      Height = 21
      Associate = eMapWidth
      Min = 640
      Max = 8000
      Increment = 16
      Position = 640
      TabOrder = 2
      Thousands = False
    end
    object UpDown1: TUpDown
      Left = 265
      Top = 14
      Width = 13
      Height = 21
      Associate = eMapHeight
      Min = 480
      Max = 8000
      Increment = 16
      Position = 480
      TabOrder = 3
      Thousands = False
    end
  end
  object mMapDescription: TMemo
    Left = 8
    Top = 72
    Width = 145
    Height = 57
    MaxLength = 256
    TabOrder = 1
    WantReturns = False
  end
end
