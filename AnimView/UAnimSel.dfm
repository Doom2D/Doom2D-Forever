object AnimSelForm: TAnimSelForm
  Left = 354
  Top = 212
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = #1042#1099#1073#1086#1088' '#1072#1085#1080#1084#1072#1094#1080#1080
  ClientHeight = 128
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 75
    Height = 13
    Caption = #1064#1080#1088#1080#1085#1072' '#1082#1072#1076#1088#1072':'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 74
    Height = 13
    Caption = #1042#1099#1089#1086#1090#1072' '#1082#1072#1076#1088#1072':'
  end
  object Label3: TLabel
    Left = 8
    Top = 64
    Width = 27
    Height = 13
    Caption = #1055#1091#1090#1100':'
  end
  object Label4: TLabel
    Left = 224
    Top = 8
    Width = 58
    Height = 13
    Caption = #1064#1080#1088#1080#1085#1072' Hit:'
  end
  object Label5: TLabel
    Left = 224
    Top = 32
    Width = 57
    Height = 13
    Caption = #1042#1099#1089#1086#1090#1072' Hit:'
  end
  object eFrameWidth: TEdit
    Left = 88
    Top = 8
    Width = 73
    Height = 21
    TabOrder = 0
    Text = '16'
  end
  object eFrameHeight: TEdit
    Left = 88
    Top = 32
    Width = 73
    Height = 21
    TabOrder = 1
    Text = '16'
  end
  object UpDown1: TUpDown
    Left = 161
    Top = 8
    Width = 16
    Height = 21
    Associate = eFrameWidth
    Min = 16
    Max = 512
    Increment = 16
    Position = 16
    TabOrder = 2
  end
  object UpDown2: TUpDown
    Left = 161
    Top = 32
    Width = 16
    Height = 21
    Associate = eFrameHeight
    Min = 16
    Max = 512
    Increment = 16
    Position = 16
    TabOrder = 3
  end
  object bOK: TButton
    Left = 8
    Top = 96
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 104
    Top = 96
    Width = 75
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 5
  end
  object ePath: TEdit
    Left = 40
    Top = 64
    Width = 305
    Height = 21
    TabOrder = 6
  end
  object Button1: TButton
    Left = 352
    Top = 64
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 7
    OnClick = Button1Click
  end
  object eHitWidth: TEdit
    Left = 288
    Top = 8
    Width = 73
    Height = 21
    TabOrder = 8
    Text = '16'
  end
  object eHitHeight: TEdit
    Left = 288
    Top = 32
    Width = 73
    Height = 21
    TabOrder = 9
    Text = '16'
  end
  object UpDown3: TUpDown
    Left = 361
    Top = 8
    Width = 16
    Height = 21
    Associate = eHitWidth
    Min = 1
    Max = 512
    Position = 16
    TabOrder = 10
  end
  object UpDown4: TUpDown
    Left = 361
    Top = 32
    Width = 16
    Height = 21
    Associate = eHitHeight
    Min = 1
    Max = 512
    Position = 16
    TabOrder = 11
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.*'
    Filter = #1042#1089#1077' '#1092#1072#1081#1083#1099'|*.*'
    Left = 352
    Top = 96
  end
end
