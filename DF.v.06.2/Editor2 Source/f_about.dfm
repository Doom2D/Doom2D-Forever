object AboutForm: TAboutForm
  Left = 414
  Top = 220
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
  ClientHeight = 196
  ClientWidth = 294
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
  object ButtonOK: TButton
    Left = 8
    Top = 168
    Width = 281
    Height = 25
    Cancel = True
    Caption = #1054#1050
    TabOrder = 0
    OnClick = ButtonOKClick
  end
  object PanelAbout: TPanel
    Left = 8
    Top = 8
    Width = 281
    Height = 153
    TabOrder = 1
    object Bevel1: TBevel
      Left = 8
      Top = 8
      Width = 265
      Height = 41
      Shape = bsFrame
    end
    object Bevel2: TBevel
      Left = 8
      Top = 56
      Width = 265
      Height = 89
      Shape = bsFrame
    end
    object LabelTitle: TLabel
      Left = 12
      Top = 12
      Width = 87
      Height = 26
      Caption = #1056#1077#1076#1072#1082#1090#1086#1088' '#1082#1072#1088#1090' Doom 2D: Forever'
      WordWrap = True
    end
    object LabelVer: TLabel
      Left = 210
      Top = 24
      Width = 55
      Height = 13
      Caption = #1042#1077#1088#1089#1080#1103' 2.1'
    end
    object LabelAuthor: TLabel
      Left = 12
      Top = 64
      Width = 76
      Height = 13
      Caption = #1040#1074#1090#1086#1088': rs.falcon'
    end
    object LabelMail: TLabel
      Left = 174
      Top = 64
      Width = 92
      Height = 13
      Cursor = crHandPoint
      Caption = 'rmw.falcon@mail.ru'
      OnClick = LabelMailClick
      OnMouseMove = LabelMailMouseMove
      OnMouseLeave = LabelMailMouseLeave
    end
    object LabelSite: TLabel
      Left = 12
      Top = 120
      Width = 114
      Height = 13
      Caption = #1057#1072#1081#1090' Doom 2D: Forever'
    end
    object LabelHttp: TLabel
      Left = 182
      Top = 120
      Width = 83
      Height = 13
      Cursor = crHandPoint
      Caption = 'www.doom2d.org'
      OnClick = LabelHttpClick
      OnMouseMove = LabelMailMouseMove
      OnMouseLeave = LabelMailMouseLeave
    end
    object LabelAuthor2: TLabel
      Left = 12
      Top = 92
      Width = 93
      Height = 13
      Caption = #1044#1086#1088#1072#1073#1072#1090#1099#1074#1072#1083': Pss'
    end
    object LabelMail2: TLabel
      Left = 199
      Top = 92
      Width = 67
      Height = 13
      Cursor = crHandPoint
      Caption = 'pssxx@mail.ru'
      OnClick = LabelMail2Click
      OnMouseMove = LabelMailMouseMove
      OnMouseLeave = LabelMailMouseLeave
    end
  end
end
