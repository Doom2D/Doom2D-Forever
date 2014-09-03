object MapOptimizationForm: TMapOptimizationForm
  Left = 196
  Top = 188
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = #1054#1087#1090#1080#1084#1080#1079#1072#1094#1080#1103' '#1082#1072#1088#1090#1099
  ClientHeight = 249
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 257
    Height = 169
    Caption = #1054#1087#1090#1080#1084#1080#1079#1072#1094#1080#1103
    TabOrder = 0
    object Bevel1: TBevel
      Left = 8
      Top = 88
      Width = 241
      Height = 73
    end
    object lOptimizationDescription: TLabel
      Left = 12
      Top = 92
      Width = 232
      Height = 65
      AutoSize = False
      WordWrap = True
    end
    object rbTexturesOptimization: TRadioButton
      Left = 8
      Top = 16
      Width = 137
      Height = 17
      Caption = #1054#1087#1090#1080#1084#1080#1079#1072#1094#1080#1103' '#1090#1077#1082#1089#1090#1091#1088
      TabOrder = 0
      OnClick = rbTexturesOptimizationClick
    end
    object rbPanelsOptimization: TRadioButton
      Left = 8
      Top = 40
      Width = 137
      Height = 17
      Caption = #1054#1087#1090#1080#1084#1080#1079#1072#1094#1080#1103' '#1087#1072#1085#1077#1083#1077#1081
      TabOrder = 1
      OnClick = rbPanelsOptimizationClick
    end
  end
  object pcOptimizationOptions: TPageControl
    Left = 264
    Top = 3
    Width = 169
    Height = 166
    ActivePage = tsPanelOptimization
    TabOrder = 1
    object tsTextureOptimization: TTabSheet
      TabVisible = False
      object bBeginTextureOptimization: TButton
        Left = 8
        Top = 8
        Width = 145
        Height = 25
        Caption = #1053#1072#1095#1072#1090#1100
        TabOrder = 0
        OnClick = bBeginTextureOptimizationClick
      end
    end
    object tsPanelOptimization: TTabSheet
      ImageIndex = 1
      TabVisible = False
      object cbOptimizeWalls: TCheckBox
        Left = 8
        Top = 0
        Width = 97
        Height = 17
        Caption = #1057#1090#1077#1085#1099
        TabOrder = 0
      end
      object bBeginPanelsOptimization: TButton
        Left = 8
        Top = 128
        Width = 145
        Height = 25
        Caption = #1053#1072#1095#1072#1090#1100
        TabOrder = 1
        OnClick = bBeginPanelsOptimizationClick
      end
      object cbOptimizeForeGround: TCheckBox
        Left = 8
        Top = 16
        Width = 97
        Height = 17
        Caption = #1055#1077#1088#1077#1076#1085#1080#1081' '#1087#1083#1072#1085
        TabOrder = 2
      end
      object cbOptimizeBackGround: TCheckBox
        Left = 8
        Top = 32
        Width = 97
        Height = 17
        Caption = #1047#1072#1076#1085#1080#1081' '#1087#1083#1072#1085
        TabOrder = 3
      end
      object cbOptimizeSteps: TCheckBox
        Left = 8
        Top = 48
        Width = 97
        Height = 17
        Caption = #1057#1090#1091#1087#1077#1085#1080
        TabOrder = 4
      end
      object cbOptimizeWater: TCheckBox
        Left = 8
        Top = 64
        Width = 97
        Height = 17
        Caption = #1042#1086#1076#1072
        TabOrder = 5
      end
      object cbOptimizeAcid1: TCheckBox
        Left = 8
        Top = 80
        Width = 97
        Height = 17
        Caption = #1050#1080#1089#1083#1086#1090#1072' 1'
        TabOrder = 6
      end
      object cbOptimizeAcid2: TCheckBox
        Left = 8
        Top = 96
        Width = 97
        Height = 17
        Caption = #1050#1080#1089#1083#1086#1090#1072' 2'
        TabOrder = 7
      end
    end
  end
  object mOptimizationResult: TMemo
    Left = 0
    Top = 176
    Width = 433
    Height = 73
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
    WantReturns = False
  end
end
