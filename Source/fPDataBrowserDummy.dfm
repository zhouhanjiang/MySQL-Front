object PDataBrowserDummy: TPDataBrowserDummy
  Left = 0
  Top = 0
  Caption = 'PDataBrowserDummy'
  ClientHeight = 26
  ClientWidth = 510
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 106
  TextHeight = 14
  object FOffset: TEdit
    Left = 0
    Top = 0
    Width = 43
    Height = 22
    TabOrder = 0
    Text = '0'
  end
  object FUDOffset: TUpDown
    Left = 43
    Top = 0
    Width = 15
    Height = 22
    Associate = FOffset
    Max = 2147483647
    TabOrder = 1
    Thousands = False
  end
  object FLimit: TEdit
    Left = 59
    Top = 0
    Width = 36
    Height = 22
    TabOrder = 2
    Text = '100'
  end
  object FUDLimit: TUpDown
    Left = 95
    Top = 0
    Width = 15
    Height = 22
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
    Left = 142
    Top = 0
    Width = 174
    Height = 22
    Constraints.MinWidth = 100
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
    Left = 347
    Top = 0
    Width = 140
    Height = 22
    TabOrder = 7
  end
  object TBQuickSearchEnabled: TToolBar
    Left = 487
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
