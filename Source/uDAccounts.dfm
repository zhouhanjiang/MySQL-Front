object DAccounts: TDAccounts
  Left = 0
  Top = 0
  HelpContext = 1065
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DAccounts'
  ClientHeight = 420
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    518
    420)
  PixelsPerInch = 144
  TextHeight = 20
  object GAccounts: TGroupBox_Ext
    Left = 12
    Top = 12
    Width = 496
    Height = 333
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'GAccounts'
    TabOrder = 0
    DesignSize = (
      496
      333)
    object FBNew: TButton
      Left = 12
      Top = 274
      Width = 143
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = aNew
      Anchors = [akLeft, akBottom]
      TabOrder = 1
    end
    object FBDelete: TButton
      Left = 169
      Top = 274
      Width = 143
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = aDelete
      Anchors = [akLeft, akBottom]
      TabOrder = 2
    end
    object FBEdit: TButton
      Left = 328
      Top = 274
      Width = 143
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = aEdit
      Anchors = [akLeft, akBottom]
      TabOrder = 3
    end
    object PAccounts: TPanel_Ext
      Left = 12
      Top = 25
      Width = 471
      Height = 229
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvRaised
      BevelOuter = bvLowered
      ParentBackground = False
      TabOrder = 0
      OnResize = FAccountsResize
      object FAccounts: TListView_Ext
        Left = 2
        Top = 2
        Width = 467
        Height = 225
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Account'
            Width = 231
          end
          item
            Caption = 'LastLogin'
            Width = 231
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = FAccountsColumnClick
        OnCompare = FAccountsCompare
        OnContextPopup = FAccountsContextPopup
        OnDblClick = FAccountsDblClick
        OnSelectItem = FAccountsSelectItem
        OnColumnResize = FAccountsColumnResize
      end
    end
  end
  object FBOk: TButton
    Left = 255
    Top = 369
    Width = 116
    Height = 39
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akRight, akBottom]
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object FBCancel: TButton
    Left = 391
    Top = 369
    Width = 115
    Height = 39
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ItemMenu: TPopupMenu
    OnPopup = ItemMenuPopup
    Left = 96
    Top = 232
    object miIOpen: TMenuItem
      Action = aOpen
      Default = True
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miINew: TMenuItem
      Action = aNew
    end
    object miIDelete: TMenuItem
      Action = aDelete
    end
    object miIEdit: TMenuItem
      Action = aEdit
    end
  end
  object ActionList: TActionList
    Left = 16
    Top = 232
    object aNew: TAction
      Caption = 'aNew'
      ShortCut = 45
      OnExecute = aNewExecute
    end
    object aEdit: TAction
      Caption = 'aEdit'
      ShortCut = 32781
      OnExecute = aEditExecute
    end
    object aDelete: TAction
      Caption = 'aDelete'
      ShortCut = 46
      OnExecute = aDeleteExecute
    end
    object aOpen: TAction
      Caption = 'aOpen'
      OnExecute = aOpenExecute
    end
  end
  object HeaderMenu: TPopupMenu
    Left = 56
    Top = 232
    object miHName: TMenuItem
      AutoCheck = True
      Caption = 'miHName'
      Checked = True
      Enabled = False
      OnClick = HeaderMenuClick
    end
    object miHHost: TMenuItem
      AutoCheck = True
      Caption = 'miHHost'
      OnClick = HeaderMenuClick
    end
    object miHUser: TMenuItem
      AutoCheck = True
      Caption = 'miHUser'
      OnClick = HeaderMenuClick
    end
    object miHDatabase: TMenuItem
      AutoCheck = True
      Caption = 'miHDatabase'
      OnClick = HeaderMenuClick
    end
    object miHLastLogin: TMenuItem
      AutoCheck = True
      Caption = 'miHLastLogin'
      OnClick = HeaderMenuClick
    end
  end
end
