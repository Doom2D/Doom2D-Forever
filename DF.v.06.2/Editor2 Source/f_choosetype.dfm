object ChooseTypeForm: TChooseTypeForm
  Left = 414
  Top = 235
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = #1042#1099#1073#1086#1088' '#1090#1080#1087#1072
  ClientHeight = 220
  ClientWidth = 205
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
  object lbTypeSelect: TListBox
    Left = 2
    Top = 2
    Width = 201
    Height = 183
    ItemHeight = 13
    TabOrder = 0
  end
  object bOK: TButton
    Left = 2
    Top = 192
    Width = 201
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
