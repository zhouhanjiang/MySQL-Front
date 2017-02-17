object DTable: TDTable
  Left = 0
  Top = 0
  HelpContext = 1054
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DTable'
  ClientHeight = 377
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    337
    377)
  PixelsPerInch = 106
  TextHeight = 13
  object PSQLWait: TPanel
    Left = 8
    Top = 8
    Width = 321
    Height = 325
    Cursor = crHourGlass
    ParentCustomHint = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'PSQLWait'
    TabOrder = 0
    Visible = False
  end
  object FBOk: TButton
    Left = 167
    Top = 344
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object FBCancel: TButton
    Left = 255
    Top = 344
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 3
  end
  object FBHelp: TButton
    Left = 8
    Top = 344
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'FBHelp'
    TabOrder = 1
    OnClick = FBHelpClick
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 321
    Height = 325
    ActivePage = TSDependencies
    Anchors = [akLeft, akTop, akRight, akBottom]
    HotTrack = True
    MultiLine = True
    TabOrder = 4
    object TSBasics: TTabSheet
      Caption = 'TSBasics'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        261)
      object GRecords: TGroupBox_Ext
        Left = 8
        Top = 183
        Width = 297
        Height = 79
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GRecords'
        TabOrder = 1
        object FLRowType: TLabel
          Left = 8
          Top = 20
          Width = 58
          Height = 13
          Caption = 'FLRowType'
        end
        object FLAutoIncrement: TLabel
          Left = 8
          Top = 49
          Width = 81
          Height = 13
          Caption = 'FLAutoIncrement'
        end
        object FRowType: TComboBox_Ext
          Left = 120
          Top = 16
          Width = 97
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = FBOkCheckEnabled
          Items.Strings = (
            ''
            'Fixed'
            'Dynamic'
            'Compressed'
            'Redundant'
            'Compact'
            'Page')
        end
        object FAutoIncrement: TEdit
          Left = 120
          Top = 46
          Width = 89
          Height = 21
          TabOrder = 1
          OnChange = FBOkCheckEnabled
          OnExit = FAutoIncrementExit
        end
      end
      object GBasics: TGroupBox_Ext
        Left = 8
        Top = 8
        Width = 297
        Height = 167
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GBasics'
        TabOrder = 0
        DesignSize = (
          297
          167)
        object FLName: TLabel
          Left = 8
          Top = 20
          Width = 40
          Height = 13
          Caption = 'FLName'
          FocusControl = FName
        end
        object FLEngine: TLabel
          Left = 8
          Top = 49
          Width = 45
          Height = 13
          Caption = 'FLEngine'
          FocusControl = FEngine
        end
        object FLComment: TLabel
          Left = 8
          Top = 139
          Width = 56
          Height = 13
          Caption = 'FLComment'
        end
        object FLCharset: TLabel
          Left = 8
          Top = 79
          Width = 48
          Height = 13
          Caption = 'FLCharset'
        end
        object FLCollation: TLabel
          Left = 8
          Top = 109
          Width = 52
          Height = 13
          Caption = 'FLCollation'
          FocusControl = FCollation
        end
        object FName: TEdit
          Left = 120
          Top = 16
          Width = 145
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 64
          TabOrder = 0
          Text = 'FName'
          OnChange = FBOkCheckEnabled
        end
        object FEngine: TComboBox_Ext
          Left = 120
          Top = 46
          Width = 113
          Height = 21
          Style = csDropDownList
          TabOrder = 1
          OnChange = FEngineChange
        end
        object FComment: TEdit
          Left = 120
          Top = 136
          Width = 169
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 2048
          TabOrder = 4
          Text = 'FComment'
          OnChange = FBOkCheckEnabled
        end
        object FCharset: TComboBox_Ext
          Left = 120
          Top = 76
          Width = 113
          Height = 21
          Style = csDropDownList
          TabOrder = 2
          OnChange = FCharsetChange
          OnExit = FCharsetExit
        end
        object FCollation: TComboBox_Ext
          Left = 120
          Top = 106
          Width = 145
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
          OnChange = FCollationChange
        end
      end
    end
    object TSInformation: TTabSheet
      Caption = 'TSInformation'
      OnShow = TSInformationShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        261)
      object GDates: TGroupBox_Ext
        Left = 8
        Top = 8
        Width = 297
        Height = 71
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GDates'
        TabOrder = 0
        DesignSize = (
          297
          71)
        object FLCreated: TLabel
          Left = 8
          Top = 20
          Width = 49
          Height = 13
          Caption = 'FLCreated'
        end
        object FLUpdated: TLabel
          Left = 8
          Top = 44
          Width = 53
          Height = 13
          Caption = 'FLUpdated'
        end
        object FCreated: TLabel
          Left = 246
          Top = 20
          Width = 43
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FCreated'
        end
        object FUpdated: TLabel
          Left = 242
          Top = 44
          Width = 47
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FUpdated'
        end
      end
      object GSize: TGroupBox_Ext
        Left = 8
        Top = 88
        Width = 297
        Height = 71
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GSize'
        TabOrder = 1
        DesignSize = (
          297
          71)
        object FLIndexSize: TLabel
          Left = 8
          Top = 20
          Width = 58
          Height = 13
          Caption = 'FLIndexSize'
        end
        object FLDataSize: TLabel
          Left = 8
          Top = 44
          Width = 55
          Height = 13
          Caption = 'FLDataSize'
        end
        object FIndexSize: TLabel
          Left = 237
          Top = 20
          Width = 52
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FIndexSize'
        end
        object FDataSize: TLabel
          Left = 240
          Top = 44
          Width = 49
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FDataSize'
        end
      end
      object GRecordCount: TGroupBox_Ext
        Left = 8
        Top = 169
        Width = 297
        Height = 47
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GRecordCount'
        TabOrder = 2
        DesignSize = (
          297
          47)
        object FLRecordCount: TLabel
          Left = 8
          Top = 20
          Width = 75
          Height = 13
          Caption = 'FLRecordCount'
        end
        object FRecordCount: TLabel
          Left = 220
          Top = 20
          Width = 69
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FRecordCount'
        end
      end
    end
    object TSKeys: TTabSheet
      Caption = 'TSKeys'
      OnShow = TSKeysShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        261)
      object FKeys: TListView
        Left = 8
        Top = 32
        Width = 297
        Height = 209
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            AutoSize = True
            Caption = 'Name'
          end
          item
            AutoSize = True
            Caption = 'Type'
          end
          item
            AutoSize = True
            Caption = 'Extras'
          end
          item
            Caption = 'Comment'
          end>
        ColumnClick = False
        HideSelection = False
        MultiSelect = True
        PopupMenu = MList
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = FKeysChange
        OnDblClick = FListDblClick
        OnEnter = FKeysEnter
        OnSelectItem = FListSelectItem
      end
      object TBIndices: TToolBar
        Left = 0
        Top = 0
        Width = 313
        Height = 24
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Transparent = False
        object tbCreateKey: TToolButton
          Left = 0
          Top = 0
          Action = aPCreateKey
        end
        object tbDeleteKey: TToolButton
          Left = 23
          Top = 0
          Action = aPDeleteKey
        end
        object tbPropertiesKey: TToolButton
          Left = 46
          Top = 0
          Action = aPEditKey
        end
      end
    end
    object TSFields: TTabSheet
      Caption = 'TSFields'
      OnShow = TSFieldsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        261)
      object FFields: TListView
        Left = 8
        Top = 32
        Width = 297
        Height = 211
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            AutoSize = True
            Caption = 'Name'
          end
          item
            AutoSize = True
            Caption = 'Type'
          end
          item
            AutoSize = True
            Caption = 'NULL'
          end
          item
            AutoSize = True
            Caption = 'Default'
          end
          item
            AutoSize = True
            Caption = 'Extras'
          end
          item
            AutoSize = True
            Caption = 'Comment'
          end>
        ColumnClick = False
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        PopupMenu = MList
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = FFieldsChange
        OnDblClick = FListDblClick
        OnEnter = FFieldsEnter
        OnSelectItem = FListSelectItem
      end
      object TBFields: TToolBar
        Left = 0
        Top = 0
        Width = 313
        Height = 24
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Transparent = False
        object tbCreateField: TToolButton
          Left = 0
          Top = 0
          Action = aPCreateField
        end
        object tbDeleteField: TToolButton
          Left = 23
          Top = 0
          Action = aPDeleteField
        end
        object tbPropertiesField: TToolButton
          Left = 46
          Top = 0
          Action = aPEditField
        end
        object tbSeparator: TToolButton
          Left = 69
          Top = 0
          Width = 8
          Caption = 'tbSeparator'
          Style = tbsSeparator
        end
        object tbFieldUp: TToolButton
          Left = 77
          Top = 0
          Action = aPUp
        end
        object tbFieldDown: TToolButton
          Left = 100
          Top = 0
          Action = aPDown
        end
      end
    end
    object TSForeignKeys: TTabSheet
      Caption = 'TSForeignKeys'
      OnShow = TSForeignKeysShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        261)
      object FForeignKeys: TListView
        Left = 8
        Top = 32
        Width = 297
        Height = 211
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            AutoSize = True
            Caption = 'Name'
          end
          item
            AutoSize = True
            Caption = 'Type'
          end
          item
            AutoSize = True
            Caption = 'Extras'
          end>
        ColumnClick = False
        HideSelection = False
        MultiSelect = True
        PopupMenu = MList
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = FForeignKeysChange
        OnDblClick = FListDblClick
        OnEnter = FForeignKeysEnter
        OnSelectItem = FListSelectItem
      end
      object TBForeignKeys: TToolBar
        Left = 0
        Top = 0
        Width = 313
        Height = 24
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Transparent = False
        object tbCreateForeignKey: TToolButton
          Left = 0
          Top = 0
          Action = aPCreateForeignKey
        end
        object tbDeleteForeignKey: TToolButton
          Left = 23
          Top = 0
          Action = aPDeleteForeignKey
        end
        object tbPropertiesForeignKey: TToolButton
          Left = 46
          Top = 0
          Action = aPEditForeignKey
        end
      end
    end
    object TSTriggers: TTabSheet
      Caption = 'TSTriggers'
      OnShow = TSTriggersShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        261)
      object FTriggers: TListView
        Left = 8
        Top = 8
        Width = 297
        Height = 235
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            AutoSize = True
            Caption = 'Name'
          end
          item
            AutoSize = True
            Caption = 'Type'
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = FTriggersChange
        OnDblClick = FListDblClick
        OnEnter = FTriggersEnter
        OnSelectItem = FListSelectItem
      end
    end
    object TSPartitions: TTabSheet
      Caption = 'TSPartitions'
      OnShow = TSPartitionsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        261)
      object GPartitions: TGroupBox_Ext
        Left = 8
        Top = 8
        Width = 297
        Height = 235
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'GPartitions'
        TabOrder = 0
        DesignSize = (
          297
          235)
        object FLPartitionType: TLabel
          Left = 8
          Top = 19
          Width = 74
          Height = 13
          Caption = 'FLPartitionType'
          FocusControl = FPartitionType
        end
        object FLPartitionsNumber: TLabel
          Left = 8
          Top = 107
          Width = 92
          Height = 13
          Caption = 'FLPartitionsNumber'
          FocusControl = FPartitionsNumber
        end
        object FLPartitions: TLabel
          Left = 8
          Top = 136
          Width = 55
          Height = 13
          Caption = 'FLPartitions'
          FocusControl = FPartitions
        end
        object FLPartitionExpr: TLabel
          Left = 8
          Top = 75
          Width = 71
          Height = 13
          Caption = 'FLPartitionExpr'
          FocusControl = FPartitionExpr
        end
        object FPartitionType: TComboBox_Ext
          Left = 120
          Top = 16
          Width = 100
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = FPartitionTypeChange
        end
        object FPartitionsNumber: TEdit
          Left = 120
          Top = 104
          Width = 33
          Height = 21
          TabOrder = 3
          Text = '0'
          OnChange = FPartitionsNumberChange
        end
        object FUDPartitionsNumber: TUpDown
          Left = 153
          Top = 104
          Width = 15
          Height = 21
          Associate = FPartitionsNumber
          TabOrder = 4
        end
        object FPartitionExpr: TEdit
          Left = 120
          Top = 72
          Width = 169
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          Text = 'FPartitionExpr'
          OnChange = FPartitionExprChange
        end
        object FLinear: TCheckBox
          Left = 128
          Top = 40
          Width = 161
          Height = 17
          Caption = 'FLinear'
          TabOrder = 1
          OnClick = FLinearClick
          OnKeyPress = FLinearKeyPress
        end
        object PPartitions: TPanel_Ext
          Left = 8
          Top = 152
          Width = 281
          Height = 73
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 5
          object FPartitions: TListView_Ext
            Left = 0
            Top = 0
            Width = 281
            Height = 73
            Align = alClient
            Columns = <
              item
                AutoSize = True
                Caption = 'Name'
              end
              item
                AutoSize = True
                Caption = 'Type'
              end
              item
                AutoSize = True
                Caption = 'MinRows'
              end
              item
                AutoSize = True
                Caption = 'MaxRows'
              end
              item
                AutoSize = True
                Caption = 'Comment'
              end>
            ColumnClick = False
            HideSelection = False
            ReadOnly = True
            PopupMenu = MList
            TabOrder = 0
            ViewStyle = vsReport
            OnChange = FPartitionsChange
            OnDblClick = FListDblClick
            OnSelectItem = FListSelectItem
          end
        end
      end
    end
    object TSDependencies: TTabSheet
      Caption = 'TSDependencies'
      OnShow = TSDependenciesShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        261)
      object FDependencies: TListView
        Left = 8
        Top = 8
        Width = 297
        Height = 235
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            AutoSize = True
            Caption = 'Name'
          end
          item
            AutoSize = True
            Caption = 'Type'
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object TSExtras: TTabSheet
      Caption = 'TSExtras'
      OnShow = TSExtrasShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        261)
      object GOptimize: TGroupBox_Ext
        Left = 8
        Top = 8
        Width = 297
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GOptimize'
        TabOrder = 0
        DesignSize = (
          297
          81)
        object FLUnusedSize: TLabel
          Left = 8
          Top = 20
          Width = 69
          Height = 13
          Caption = 'FLUnusedSize'
        end
        object FUnusedSize: TLabel
          Left = 226
          Top = 20
          Width = 63
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FUnusedSize'
        end
        object FBOptimize: TButton
          Left = 168
          Top = 44
          Width = 122
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'FBOptimize'
          TabOrder = 0
          OnClick = FBOptimizeClick
        end
      end
      object GCheck: TGroupBox_Ext
        Left = 8
        Top = 96
        Width = 297
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GCheck'
        TabOrder = 1
        DesignSize = (
          297
          81)
        object FLChecked: TLabel
          Left = 8
          Top = 20
          Width = 55
          Height = 13
          Caption = 'FLChecked'
        end
        object FChecked: TLabel
          Left = 240
          Top = 20
          Width = 49
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FChecked'
        end
        object FBCheck: TButton
          Left = 168
          Top = 44
          Width = 122
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'FBCheck'
          TabOrder = 0
          OnClick = FBCheckClick
        end
      end
      object GFlush: TGroupBox_Ext
        Left = 8
        Top = 188
        Width = 297
        Height = 53
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GFlush'
        TabOrder = 2
        DesignSize = (
          297
          53)
        object FBFlush: TButton
          Left = 168
          Top = 16
          Width = 121
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'FBFlush'
          TabOrder = 0
          OnClick = FBFlushClick
        end
      end
    end
    object TSSource: TTabSheet
      Caption = 'TSSource'
      OnShow = TSSourceShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        261)
      object FSource: TSynMemo
        Left = 8
        Top = 8
        Width = 297
        Height = 235
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = MSource
        TabOrder = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 0
        MaxScrollWidth = 65535
        Options = [eoAutoIndent, eoDragDropEditing, eoGroupUndo, eoHideShowScrollbars, eoNoCaret, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
        ReadOnly = True
        RightEdge = 0
        RightEdgeColor = clWindow
        ScrollHintFormat = shfTopToBottom
        WantReturns = False
        FontSmoothing = fsmNone
      end
    end
  end
  object MSource: TPopupMenu
    Left = 104
    Top = 336
    object msCopy: TMenuItem
      Caption = 'aECopy'
      OnClick = msCopyClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object msSelectAll: TMenuItem
      Caption = 'aESelectAll'
    end
  end
  object MList: TPopupMenu
    Left = 72
    Top = 336
    object aPUp1: TMenuItem
      Action = aPUp
    end
    object aPDown1: TMenuItem
      Action = aPDown
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object mlDCreate: TMenuItem
      Caption = 'aDCreate'
    end
    object mlDDelete: TMenuItem
      Caption = 'aDDelete'
    end
    object mlDProperties: TMenuItem
      Caption = 'aDProperties'
    end
  end
  object ActionList: TActionList
    Left = 136
    Top = 336
    object aPUp: TAction
      Caption = 'aPUp'
      ImageIndex = 49
      OnExecute = aPUpExecute
    end
    object aPDown: TAction
      Caption = 'aPDown'
      ImageIndex = 50
      OnExecute = aPDownExecute
    end
    object aPCreateField: TAction
      Caption = 'aPCreateField'
      ImageIndex = 65
      OnExecute = aPCreateFieldExecute
    end
    object aPDeleteField: TAction
      Caption = 'aPDeleteField'
      ImageIndex = 66
      OnExecute = aPDeleteFieldExecute
    end
    object aPEditField: TAction
      Caption = 'aPEditField'
      ImageIndex = 11
      OnExecute = aPEditFieldExecute
    end
    object aPCreateKey: TAction
      Caption = 'aPCreateKey'
      ImageIndex = 63
      OnExecute = aPCreateKeyExecute
    end
    object aPDeleteKey: TAction
      Caption = 'aPDeleteKey'
      ImageIndex = 64
      OnExecute = aPDeleteKeyExecute
    end
    object aPEditKey: TAction
      Caption = 'aPEditKey'
      ImageIndex = 11
      OnExecute = aPEditKeyExecute
    end
    object aPCreateForeignKey: TAction
      Caption = 'aPCreateContraint'
      ImageIndex = 67
      OnExecute = aPCreateForeignKeyExecute
    end
    object aPDeleteForeignKey: TAction
      Caption = 'aPDeleteConstraint'
      ImageIndex = 72
      OnExecute = aPDeleteForeignKeyExecute
    end
    object aPEditForeignKey: TAction
      Caption = 'aPEditConstraint'
      ImageIndex = 11
      OnExecute = aPEditForeignKeyExecute
    end
    object aPCreateTrigger: TAction
      Caption = 'aPCreateTrigger'
      ImageIndex = 94
    end
    object aPDeleteTrigger: TAction
      Caption = 'aPDeleteTrigger'
      ImageIndex = 95
    end
    object aPEditTrigger: TAction
      Caption = 'aPEditTrigger'
      ImageIndex = 11
    end
    object aPCreatePartition: TAction
      Caption = 'aPCreatePartition'
      OnExecute = aPCreatePartitionExecute
    end
    object aPDeletePartition: TAction
      Caption = 'aPDeletePartition'
      OnExecute = aPDeletePartitionExecute
    end
    object aPEditPartition: TAction
      Caption = 'aPEditPartition'
      OnExecute = aPEditPartitionExecute
    end
  end
end
