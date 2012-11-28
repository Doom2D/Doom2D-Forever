object AboutForm: TAboutForm
  Left = 491
  Top = 309
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
  ClientHeight = 189
  ClientWidth = 177
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
  object Button1: TButton
    Left = 8
    Top = 160
    Width = 161
    Height = 25
    Cancel = True
    Caption = #1054#1050
    TabOrder = 0
    OnClick = Button1Click
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 161
    Height = 145
    TabOrder = 1
    object Bevel1: TBevel
      Left = 8
      Top = 8
      Width = 145
      Height = 41
      Shape = bsFrame
    end
    object Bevel2: TBevel
      Left = 8
      Top = 56
      Width = 145
      Height = 81
      Shape = bsFrame
    end
    object Label1: TLabel
      Left = 12
      Top = 12
      Width = 84
      Height = 26
      Caption = #1056#1077#1076#1072#1082#1090#1086#1088' '#1082#1072#1088#1090' Doom2D: Forever'
      WordWrap = True
    end
    object Label2: TLabel
      Left = 122
      Top = 24
      Width = 21
      Height = 13
      Caption = 'v1.9'
    end
    object Label3: TLabel
      Left = 12
      Top = 64
      Width = 73
      Height = 13
      Caption = #1040#1074#1090#1086#1088' rs.falcon'
    end
    object Label4: TLabel
      Left = 56
      Top = 80
      Width = 92
      Height = 13
      Cursor = crHandPoint
      Caption = 'rmw.falcon@mail.ru'
      OnClick = Label4Click
      OnMouseMove = Label4MouseMove
      OnMouseLeave = Label4MouseLeave
    end
    object Label5: TLabel
      Left = 12
      Top = 104
      Width = 111
      Height = 13
      Caption = #1057#1072#1081#1090' Doom2D: Forever'
    end
    object Label6: TLabel
      Left = 64
      Top = 120
      Width = 83
      Height = 13
      Cursor = crHandPoint
      Caption = 'www.doom2d.org'
      OnClick = Label6Click
      OnMouseMove = Label4MouseMove
      OnMouseLeave = Label4MouseLeave
    end
  end
end
