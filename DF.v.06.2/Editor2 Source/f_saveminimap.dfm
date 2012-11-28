object SaveMiniMapForm: TSaveMiniMapForm
  Left = 448
  Top = 312
  Width = 431
  Height = 224
  Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1084#1080#1085#1080'-'#1082#1072#1088#1090#1091
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 110
  TextHeight = 16
  object pbMiniMap: TPaintBox
    Left = 228
    Top = 0
    Width = 195
    Height = 196
    Align = alClient
    OnPaint = pbMiniMapPaint
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 228
    Height = 196
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      228
      196)
    object Label1: TLabel
      Left = 7
      Top = 15
      Width = 61
      Height = 16
      Caption = #1052#1072#1089#1096#1090#1072#1073':'
    end
    object bSave: TButton
      Left = 5
      Top = 170
      Width = 213
      Height = 31
      Anchors = [akLeft, akBottom]
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      Default = True
      TabOrder = 1
      OnClick = bSaveClick
    end
    object cbScale: TComboBox
      Left = 123
      Top = 10
      Width = 90
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      ItemIndex = 0
      TabOrder = 0
      Text = '1x'
      OnChange = cbScaleChange
      Items.Strings = (
        '1x'
        '2x')
    end
    object bClose: TButton
      Left = 5
      Top = 207
      Width = 213
      Height = 31
      Anchors = [akLeft, akBottom]
      Cancel = True
      Caption = #1047#1072#1082#1088#1099#1090#1100
      TabOrder = 2
      OnClick = bCloseClick
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.bmp'
    Filter = 'BMP files (*.bmp)|*.bmp|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 8
    Top = 48
  end
end
