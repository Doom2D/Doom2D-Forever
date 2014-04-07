object MapOptionsForm: TMapOptionsForm
  Left = 323
  Top = 229
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1082#1072#1088#1090#1099
  ClientHeight = 408
  ClientWidth = 381
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
  object LabelDesc: TLabel
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
  object LabelName: TLabel
    Left = 3
    Top = 3
    Width = 59
    Height = 13
    Caption = #1048#1084#1103' '#1082#1072#1088#1090#1099':'
  end
  object LabelBack: TLabel
    Left = 3
    Top = 147
    Width = 26
    Height = 13
    Caption = #1060#1086#1085':'
  end
  object LabelMusic: TLabel
    Left = 195
    Top = 147
    Width = 43
    Height = 13
    Caption = #1052#1091#1079#1099#1082#1072':'
  end
  object LabelAuthor: TLabel
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
  object GBStats: TGroupBox
    Left = 195
    Top = 3
    Width = 182
    Height = 134
    Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072':'
    TabOrder = 3
    object lPanelCount: TLabel
      Left = 90
      Top = 32
      Width = 64
      Height = 13
      AutoSize = False
    end
    object lTextureCount: TLabel
      Left = 90
      Top = 16
      Width = 64
      Height = 13
      AutoSize = False
    end
    object lItemCount: TLabel
      Left = 90
      Top = 48
      Width = 64
      Height = 13
      AutoSize = False
    end
    object LabelPanels: TLabel
      Left = 8
      Top = 32
      Width = 47
      Height = 13
      Caption = #1055#1072#1085#1077#1083#1077#1081':'
    end
    object LabelTexs: TLabel
      Left = 8
      Top = 16
      Width = 44
      Height = 13
      Caption = #1058#1077#1082#1089#1090#1091#1088':'
    end
    object LabelItems: TLabel
      Left = 8
      Top = 48
      Width = 60
      Height = 13
      Caption = #1055#1088#1077#1076#1084#1077#1090#1086#1074':'
    end
    object LabelMonsters: TLabel
      Left = 8
      Top = 64
      Width = 53
      Height = 13
      Caption = #1052#1086#1085#1089#1090#1088#1086#1074':'
    end
    object lMonsterCount: TLabel
      Left = 90
      Top = 64
      Width = 64
      Height = 13
      AutoSize = False
    end
    object LabelAreas: TLabel
      Left = 8
      Top = 80
      Width = 52
      Height = 13
      Caption = #1054#1073#1083#1072#1089#1090#1077#1081':'
    end
    object lAreaCount: TLabel
      Left = 90
      Top = 80
      Width = 64
      Height = 13
      AutoSize = False
    end
    object LabelTriggers: TLabel
      Left = 8
      Top = 96
      Width = 56
      Height = 13
      Caption = #1058#1088#1080#1075#1075#1077#1088#1086#1074':'
    end
    object lTriggerCount: TLabel
      Left = 90
      Top = 96
      Width = 64
      Height = 13
      AutoSize = False
    end
  end
  object bOK: TButton
    Left = 211
    Top = 379
    Width = 75
    Height = 25
    Caption = #1054#1050
    Default = True
    TabOrder = 9
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 299
    Top = 379
    Width = 75
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 10
    OnClick = bCancelClick
  end
  object GBSizes: TGroupBox
    Left = 3
    Top = 192
    Width = 374
    Height = 181
    Caption = #1056#1072#1079#1084#1077#1088#1099':'
    TabOrder = 8
    object LabelLeftBorder: TLabel
      Left = 8
      Top = 24
      Width = 158
      Height = 13
      Caption = #1057#1076#1074#1080#1085#1091#1090#1100' '#1083#1077#1074#1091#1102' '#1075#1088#1072#1085#1080#1094#1091' '#1082#1072#1088#1090#1099
    end
    object LabelFor1: TLabel
      Left = 242
      Top = 24
      Width = 12
      Height = 13
      Caption = #1085#1072
    end
    object LabelRightBorder: TLabel
      Left = 8
      Top = 50
      Width = 164
      Height = 13
      Caption = #1057#1076#1074#1080#1085#1091#1090#1100' '#1087#1088#1072#1074#1091#1102' '#1075#1088#1072#1085#1080#1094#1091' '#1082#1072#1088#1090#1099
    end
    object LabelFor2: TLabel
      Left = 242
      Top = 50
      Width = 12
      Height = 13
      Caption = #1085#1072
    end
    object LabelTopBorder: TLabel
      Left = 8
      Top = 76
      Width = 172
      Height = 13
      Caption = #1057#1076#1074#1080#1085#1091#1090#1100' '#1074#1077#1088#1093#1085#1102#1102' '#1075#1088#1072#1085#1080#1094#1091' '#1082#1072#1088#1090#1099
    end
    object LabelFor3: TLabel
      Left = 242
      Top = 76
      Width = 12
      Height = 13
      Caption = #1085#1072
    end
    object LabelBottomBorder: TLabel
      Left = 8
      Top = 102
      Width = 169
      Height = 13
      Caption = #1057#1076#1074#1080#1085#1091#1090#1100' '#1085#1080#1078#1085#1102#1102' '#1075#1088#1072#1085#1080#1094#1091' '#1082#1072#1088#1090#1099
    end
    object LabelFor4: TLabel
      Left = 242
      Top = 102
      Width = 12
      Height = 13
      Caption = #1085#1072
    end
    object LabelMapSize: TLabel
      Left = 8
      Top = 128
      Width = 121
      Height = 13
      Caption = #1054#1073#1097#1080#1077' '#1088#1072#1079#1084#1077#1088#1099' '#1082#1072#1088#1090#1099':'
    end
    object LabelWidth: TLabel
      Left = 192
      Top = 128
      Width = 42
      Height = 13
      Caption = #1064#1080#1088#1080#1085#1072':'
    end
    object LabelHeight: TLabel
      Left = 192
      Top = 154
      Width = 41
      Height = 13
      Caption = #1042#1099#1089#1086#1090#1072':'
    end
    object lMapWidth: TLabel
      Left = 262
      Top = 128
      Width = 64
      Height = 13
      AutoSize = False
    end
    object lMapHeight: TLabel
      Left = 262
      Top = 154
      Width = 64
      Height = 13
      AutoSize = False
    end
    object bLeftB: TButton
      Left = 188
      Top = 21
      Width = 49
      Height = 20
      Caption = #1042#1083#1077#1074#1086
      TabOrder = 0
      OnClick = bLeftBClick
    end
    object eLeftB: TEdit
      Left = 260
      Top = 21
      Width = 60
      Height = 21
      TabOrder = 1
      Text = '0'
      OnChange = eLeftBChange
    end
    object UpDown1: TUpDown
      Left = 320
      Top = 21
      Width = 12
      Height = 21
      Associate = eLeftB
      Max = 16384
      Increment = 16
      TabOrder = 2
    end
    object bRightB: TButton
      Left = 188
      Top = 47
      Width = 49
      Height = 20
      Caption = #1042#1087#1088#1072#1074#1086
      TabOrder = 3
      OnClick = bRightBClick
    end
    object eRightB: TEdit
      Left = 260
      Top = 47
      Width = 60
      Height = 21
      TabOrder = 4
      Text = '0'
      OnChange = eLeftBChange
    end
    object UpDown2: TUpDown
      Left = 320
      Top = 47
      Width = 12
      Height = 21
      Associate = eRightB
      Max = 16384
      Increment = 16
      TabOrder = 5
    end
    object bUpB: TButton
      Left = 188
      Top = 73
      Width = 49
      Height = 20
      Caption = #1042#1074#1077#1088#1093
      TabOrder = 6
      OnClick = bUpBClick
    end
    object eUpB: TEdit
      Left = 260
      Top = 73
      Width = 60
      Height = 21
      TabOrder = 7
      Text = '0'
      OnChange = eLeftBChange
    end
    object UpDown3: TUpDown
      Left = 320
      Top = 73
      Width = 12
      Height = 21
      Associate = eUpB
      Max = 16384
      Increment = 16
      TabOrder = 8
    end
    object bDownB: TButton
      Left = 188
      Top = 99
      Width = 49
      Height = 20
      Caption = #1042#1085#1080#1079
      TabOrder = 9
      OnClick = bDownBClick
    end
    object eDownB: TEdit
      Left = 260
      Top = 99
      Width = 60
      Height = 21
      TabOrder = 10
      Text = '0'
      OnChange = eLeftBChange
    end
    object UpDown4: TUpDown
      Left = 320
      Top = 99
      Width = 12
      Height = 21
      Associate = eDownB
      Max = 16384
      Increment = 16
      TabOrder = 11
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
    Left = 195
    Top = 163
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
    Left = 351
    Top = 162
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
    Left = 325
    Top = 162
    Width = 23
    Height = 23
    Hint = #1059#1073#1088#1072#1090#1100' '#1084#1091#1079#1099#1082#1091
    TabOrder = 12
    OnClick = bRemoveMusicClick
  end
end
