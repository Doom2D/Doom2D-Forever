object MainForm: TMainForm
  Left = 202
  Top = 123
  Width = 696
  Height = 480
  Caption = #1053#1072#1078#1084#1080#1090#1077' "L"'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Render: TPanel
    Left = 0
    Top = 0
    Width = 680
    Height = 442
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = RenderResize
  end
end
