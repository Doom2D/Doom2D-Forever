object SaveMiniMapForm: TSaveMiniMapForm
  Left = 362
  Top = 299
  Width = 431
  Height = 225
  Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1084#1080#1085#1080'-'#1082#1072#1088#1090#1091
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
  object pbMiniMap: TPaintBox
    Left = 185
    Top = 0
    Width = 230
    Height = 187
    Align = alClient
    OnPaint = pbMiniMapPaint
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 187
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      185
      187)
    object LabelScale: TLabel
      Left = 6
      Top = 12
      Width = 49
      Height = 13
      Caption = #1052#1072#1089#1096#1090#1072#1073':'
    end
    object bSave: TButton
      Left = 4
      Top = 130
      Width = 173
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      Default = True
      TabOrder = 1
      OnClick = bSaveClick
    end
    object cbScale: TComboBox
      Left = 100
      Top = 8
      Width = 73
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = '1/16'
      OnChange = cbScaleChange
      Items.Strings = (
        '1/16'
        '1/8')
    end
    object bClose: TButton
      Left = 4
      Top = 160
      Width = 173
      Height = 25
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
