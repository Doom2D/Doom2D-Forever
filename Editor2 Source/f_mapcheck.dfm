object MapCheckForm: TMapCheckForm
  Left = 238
  Top = 224
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
  object PanelResults: TPanel
    Left = 0
    Top = 85
    Width = 509
    Height = 76
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      509
      76)
    object bClose: TButton
      Left = 441
      Top = 50
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = #1047#1072#1082#1088#1099#1090#1100
      TabOrder = 1
      OnClick = bCloseClick
    end
    object bCheckMap: TButton
      Left = 329
      Top = 50
      Width = 105
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #1055#1088#1086#1074#1077#1088#1080#1090#1100' '#1079#1072#1085#1086#1074#1086
      Default = True
      TabOrder = 0
      OnClick = bCheckMapClick
    end
    object mErrorDescription: TMemo
      Left = 0
      Top = 0
      Width = 509
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
    Width = 509
    Height = 85
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbErrorListClick
  end
end
