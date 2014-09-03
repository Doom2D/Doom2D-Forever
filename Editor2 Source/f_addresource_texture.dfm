inherited AddTextureForm: TAddTextureForm
  Left = 371
  Top = 194
  Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1090#1077#1082#1089#1090#1091#1088#1091
  ClientHeight = 302
  ClientWidth = 466
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited bOK: TButton
    Left = 11
    Top = 272
    Width = 22
    Height = 17
    Visible = False
    OnClick = nil
  end
  inherited bCancel: TButton
    Left = 35
    Top = 271
    Width = 22
    Height = 18
    Visible = False
  end
  inherited lbResourcesList: TListBox
    Top = 112
    Height = 150
  end
  object PanelTexPreview: TPanel
    Left = 204
    Top = 4
    Width = 258
    Height = 258
    BevelOuter = bvLowered
    TabOrder = 5
    object iPreview: TImage
      Left = 1
      Top = 1
      Width = 256
      Height = 256
    end
  end
  object eTextureName: TEdit
    Left = 4
    Top = 88
    Width = 193
    Height = 21
    TabOrder = 6
    OnChange = eTextureNameChange
  end
  object bAddTexture: TButton
    Left = 160
    Top = 272
    Width = 81
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    Default = True
    TabOrder = 7
    OnClick = bAddTextureClick
  end
  object bClose: TButton
    Left = 248
    Top = 272
    Width = 81
    Height = 25
    Caption = #1047#1072#1082#1088#1099#1090#1100
    TabOrder = 8
    OnClick = bCloseClick
  end
  object bAddClose: TButton
    Left = 336
    Top = 272
    Width = 121
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1080' '#1079#1072#1082#1088#1099#1090#1100
    TabOrder = 9
    OnClick = bAddCloseClick
  end
end
