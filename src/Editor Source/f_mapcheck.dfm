object MapCheckForm: TMapCheckForm
  Left = 196
  Top = 225
  Width = 525
  Height = 199
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = #1055#1088#1086#1074#1077#1088#1082#1072' '#1082#1072#1088#1090#1099
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 96
    Width = 517
    Height = 76
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      517
      76)
    object bClose: TButton
      Left = 441
      Top = 50
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #1047#1072#1082#1088#1099#1090#1100
      TabOrder = 0
      OnClick = bCloseClick
    end
    object bCheckMap: TButton
      Left = 329
      Top = 50
      Width = 105
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #1055#1088#1086#1074#1077#1088#1080#1090#1100' '#1079#1072#1085#1086#1074#1086
      TabOrder = 1
      OnClick = bCheckMapClick
    end
    object mErrorDescription: TMemo
      Left = 0
      Top = 0
      Width = 517
      Height = 41
      Align = alTop
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
      WantReturns = False
    end
  end
  object lbErrorList: TListBox
    Left = 0
    Top = 0
    Width = 517
    Height = 96
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbErrorListClick
  end
end
