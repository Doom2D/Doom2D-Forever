inherited AddTextureForm: TAddTextureForm
  Left = 371
  Top = 194
  Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1090#1077#1082#1089#1090#1091#1088#1091
  ClientHeight = 298
  ClientWidth = 466
  PixelsPerInch = 96
  TextHeight = 13
  inherited bOK: TButton
    Left = 35
    Top = 271
  end
  inherited bCancel: TButton
    Left = 123
    Top = 271
  end
  inherited lbResourcesList: TListBox
    Top = 112
    Height = 150
  end
  object Panel1: TPanel
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
end
