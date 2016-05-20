object PDataBrowserDummy: TPDataBrowserDummy
  Left = 0
  Top = 0
  Caption = 'PDataBrowserDummy'
  ClientHeight = 31
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object FOffset: TEdit
    Left = 0
    Top = 0
    Width = 40
    Height = 21
    TabOrder = 0
    Text = '0'
  end
  object FUDOffset: TUpDown
    Left = 40
    Top = 0
    Width = 14
    Height = 21
    Associate = FOffset
    Max = 2147483647
    TabOrder = 1
    Thousands = False
  end
  object FLimit: TEdit
    Left = 55
    Top = 0
    Width = 33
    Height = 21
    TabOrder = 2
    Text = '100'
  end
  object FUDLimit: TUpDown
    Left = 88
    Top = 0
    Width = 14
    Height = 21
    Associate = FLimit
    Min = 1
    Max = 2147483647
    Increment = 10
    Position = 100
    TabOrder = 3
    Thousands = False
  end
  object TBLimitEnabled: TToolBar
    Left = 111
    Top = 0
    Width = 31
    Height = 23
    Align = alNone
    TabOrder = 4
    Transparent = False
    object FLimitEnabled: TToolButton
      Left = 0
      Top = 0
      Caption = 'FLimitEnabled'
      ImageIndex = 87
      Style = tbsCheck
    end
  end
  object FFilter: TComboBox_Ext
    Left = 132
    Top = 0
    Width = 184
    Height = 21
    Constraints.MinWidth = 93
    TabOrder = 5
  end
  object TBFilterEnabled: TToolBar
    Left = 316
    Top = 0
    Width = 31
    Height = 23
    Align = alNone
    Caption = 'TBFilterEnabled'
    TabOrder = 6
    Transparent = False
    object FFilterEnabled: TToolButton
      Left = 0
      Top = 0
      Enabled = False
      ImageIndex = 88
      Style = tbsCheck
    end
  end
  object FQuickSearch: TEdit
    Left = 357
    Top = 0
    Width = 126
    Height = 20
    AutoSize = False
    TabOrder = 7
  end
  object TBQuickSearchEnabled: TToolBar
    Left = 483
    Top = 0
    Width = 23
    Height = 22
    Align = alNone
    AutoSize = True
    TabOrder = 8
    Transparent = False
    object FQuickSearchEnabled: TToolButton
      Left = 0
      Top = 0
      Caption = ' '
      Enabled = False
      ImageIndex = 89
      Style = tbsCheck
    end
  end
end
