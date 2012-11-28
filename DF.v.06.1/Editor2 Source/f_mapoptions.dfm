object MapOptionsForm: TMapOptionsForm
  Left = 213
  Top = 98
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1082#1072#1088#1090#1099
  ClientHeight = 270
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 3
    Top = 51
    Width = 87
    Height = 13
    Caption = #1054#1087#1080#1089#1072#1085#1080#1077' '#1082#1072#1088#1090#1099':'
  end
  object lCharCountName: TLabel
    Left = 139
    Top = 3
    Width = 41
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '00\32'
  end
  object lCharCountDescription: TLabel
    Left = 139
    Top = 51
    Width = 41
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '000\256'
  end
  object Label9: TLabel
    Left = 3
    Top = 3
    Width = 59
    Height = 13
    Caption = #1048#1084#1103' '#1082#1072#1088#1090#1099':'
  end
  object Label10: TLabel
    Left = 3
    Top = 147
    Width = 26
    Height = 13
    Caption = #1060#1086#1085':'
  end
  object Label12: TLabel
    Left = 3
    Top = 195
    Width = 43
    Height = 13
    Caption = #1052#1091#1079#1099#1082#1072':'
  end
  object Label11: TLabel
    Left = 3
    Top = 99
    Width = 33
    Height = 13
    Caption = #1040#1074#1090#1086#1088':'
  end
  object lCharCountAuthor: TLabel
    Left = 139
    Top = 99
    Width = 41
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '00\32'
  end
  object GroupBox1: TGroupBox
    Left = 187
    Top = 3
    Width = 129
    Height = 150
    Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072':'
    TabOrder = 3
    object lPanelCount: TLabel
      Left = 72
      Top = 32
      Width = 42
      Height = 13
      AutoSize = False
    end
    object lTextureCount: TLabel
      Left = 72
      Top = 16
      Width = 42
      Height = 13
      AutoSize = False
    end
    object lItemCount: TLabel
      Left = 72
      Top = 48
      Width = 42
      Height = 13
      AutoSize = False
    end
    object Label4: TLabel
      Left = 8
      Top = 32
      Width = 47
      Height = 13
      Caption = #1055#1072#1085#1077#1083#1077#1081':'
    end
    object Label5: TLabel
      Left = 8
      Top = 16
      Width = 44
      Height = 13
      Caption = #1058#1077#1082#1089#1090#1091#1088':'
    end
    object Label6: TLabel
      Left = 8
      Top = 48
      Width = 60
      Height = 13
      Caption = #1055#1088#1077#1076#1084#1077#1090#1086#1074':'
    end
    object Label2: TLabel
      Left = 8
      Top = 64
      Width = 53
      Height = 13
      Caption = #1052#1086#1085#1089#1090#1088#1086#1074':'
    end
    object lMonsterCount: TLabel
      Left = 72
      Top = 64
      Width = 42
      Height = 13
      AutoSize = False
    end
    object Label3: TLabel
      Left = 8
      Top = 80
      Width = 52
      Height = 13
      Caption = #1054#1073#1083#1072#1089#1090#1077#1081':'
    end
    object lAreaCount: TLabel
      Left = 72
      Top = 80
      Width = 42
      Height = 13
      AutoSize = False
    end
    object Label13: TLabel
      Left = 8
      Top = 96
      Width = 56
      Height = 13
      Caption = #1058#1088#1080#1075#1075#1077#1088#1086#1074':'
    end
    object lTriggerCount: TLabel
      Left = 72
      Top = 96
      Width = 42
      Height = 13
      AutoSize = False
    end
  end
  object bOK: TButton
    Left = 155
    Top = 243
    Width = 75
    Height = 25
    Caption = #1054#1050
    Default = True
    TabOrder = 9
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 243
    Top = 243
    Width = 75
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 10
    OnClick = bCancelClick
  end
  object GroupBox2: TGroupBox
    Left = 187
    Top = 156
    Width = 129
    Height = 78
    Caption = #1056#1072#1079#1084#1077#1088#1099':'
    TabOrder = 8
    object Label7: TLabel
      Left = 8
      Top = 26
      Width = 41
      Height = 13
      Caption = #1042#1099#1089#1086#1090#1072':'
    end
    object Label8: TLabel
      Left = 13
      Top = 50
      Width = 36
      Height = 13
      Caption = #1044#1083#1080#1085#1072':'
    end
    object UpDown2: TUpDown
      Left = 105
      Top = 46
      Width = 15
      Height = 21
      Associate = eMapWidth
      Min = 640
      Max = 32752
      Increment = 16
      Position = 640
      TabOrder = 3
      Thousands = False
    end
    object UpDown1: TUpDown
      Left = 105
      Top = 22
      Width = 15
      Height = 21
      Associate = eMapHeight
      Min = 480
      Max = 32752
      Increment = 16
      Position = 480
      TabOrder = 1
      Thousands = False
    end
    object eMapHeight: TEdit
      Left = 56
      Top = 22
      Width = 49
      Height = 21
      Ctl3D = True
      MaxLength = 5
      ParentCtl3D = False
      TabOrder = 0
      Text = '480'
    end
    object eMapWidth: TEdit
      Left = 56
      Top = 46
      Width = 49
      Height = 21
      Ctl3D = True
      MaxLength = 5
      ParentCtl3D = False
      TabOrder = 2
      Text = '640'
    end
  end
  object eMapDescription: TEdit
    Left = 3
    Top = 67
    Width = 178
    Height = 21
    Ctl3D = True
    MaxLength = 256
    ParentCtl3D = False
    TabOrder = 1
    OnChange = eMapDescriptionChange
  end
  object eMapName: TEdit
    Left = 3
    Top = 19
    Width = 178
    Height = 21
    Ctl3D = True
    MaxLength = 32
    ParentCtl3D = False
    TabOrder = 0
    OnChange = eMapNameChange
  end
  object eBack: TEdit
    Left = 3
    Top = 163
    Width = 126
    Height = 21
    Color = clBtnFace
    Ctl3D = True
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 4
    OnChange = eMapDescriptionChange
  end
  object eMusic: TEdit
    Left = 3
    Top = 211
    Width = 126
    Height = 21
    Color = clBtnFace
    Ctl3D = True
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 6
    OnChange = eMapDescriptionChange
  end
  object bSelectBack: TButton
    Left = 159
    Top = 162
    Width = 23
    Height = 23
    Hint = #1042#1099#1073#1088#1072#1090#1100' '#1092#1086#1085
    Caption = '..'
    TabOrder = 5
    OnClick = bSelectBackClick
  end
  object bSelectMusic: TButton
    Left = 159
    Top = 210
    Width = 23
    Height = 23
    Hint = #1042#1099#1073#1088#1072#1090#1100' '#1084#1091#1079#1099#1082#1091
    Caption = '..'
    TabOrder = 7
    OnClick = bSelectMusicClick
  end
  object eAuthor: TEdit
    Left = 3
    Top = 115
    Width = 178
    Height = 21
    Ctl3D = True
    MaxLength = 32
    ParentCtl3D = False
    TabOrder = 2
    OnChange = eAuthorChange
  end
  object bRemoveBack: TButton
    Left = 133
    Top = 162
    Width = 23
    Height = 23
    Hint = #1059#1073#1088#1072#1090#1100' '#1092#1086#1085
    TabOrder = 11
    OnClick = bRemoveBackClick
  end
  object bRemoveMusic: TButton
    Left = 133
    Top = 210
    Width = 23
    Height = 23
    Hint = #1059#1073#1088#1072#1090#1100' '#1084#1091#1079#1099#1082#1091
    TabOrder = 12
    OnClick = bRemoveMusicClick
  end
end
