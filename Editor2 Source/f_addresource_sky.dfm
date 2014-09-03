inherited AddSkyForm: TAddSkyForm
  Left = 225
  Top = 145
  Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1090#1077#1082#1089#1090#1091#1088#1091' '#1076#1083#1103' '#1085#1077#1073#1072
  ClientHeight = 250
  ClientWidth = 529
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited bOK: TButton
    Top = 220
  end
  inherited bCancel: TButton
    Top = 220
  end
  inherited lbResourcesList: TListBox
    Height = 127
  end
  object PanelTexPreview: TPanel
    Left = 204
    Top = 4
    Width = 322
    Height = 242
    BevelOuter = bvLowered
    TabOrder = 5
    object iPreview: TImage
      Left = 1
      Top = 1
      Width = 320
      Height = 240
    end
  end
end
