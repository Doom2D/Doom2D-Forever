object AboutForm: TAboutForm
  Left = 192
  Top = 106
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
      Top = 16
      Width = 136
      Height = 13
      Caption = 'Doom2D: Forever map editor'
    end
    object Label2: TLabel
      Left = 112
      Top = 32
      Width = 33
      Height = 13
      Caption = 'v02.5a'
    end
    object Label3: TLabel
      Left = 12
      Top = 64
      Width = 54
      Height = 13
      Caption = 'by rs.falcon'
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
      Width = 126
      Height = 13
      Caption = 'Doom2D: Forever web site'
    end
    object Label6: TLabel
      Left = 88
      Top = 120
      Width = 60
      Height = 13
      Cursor = crHandPoint
      Caption = 'df.50free.org'
      OnClick = Label6Click
      OnMouseMove = Label4MouseMove
      OnMouseLeave = Label4MouseLeave
    end
  end
end
