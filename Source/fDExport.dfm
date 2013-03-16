object DExport: TDExport
  Left = 486
  Top = 233
  BorderStyle = bsDialog
  Caption = 'DExport'
  ClientHeight = 331
  ClientWidth = 341
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    341
    331)
  PixelsPerInch = 106
  TextHeight = 13
  object PSQLWait: TPanel_Ext
    Left = 8
    Top = 0
    Width = 341
    Height = 281
    Cursor = crHourGlass
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'PSQLWait'
    ParentBackground = False
    TabOrder = 0
    Visible = False
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 341
    Height = 281
    ActivePage = TSJob
    Anchors = [akLeft, akTop, akRight, akBottom]
    HotTrack = True
    Style = tsFlatButtons
    TabOrder = 1
    TabStop = False
    object TSSelect: TTabSheet
      Caption = 'TSSelect'
      TabVisible = False
      OnShow = TSSelectShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GSelect: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 265
        Caption = 'GSelect'
        TabOrder = 0
        DesignSize = (
          325
          265)
        object PSelect: TPanel_Ext
          Left = 8
          Top = 16
          Width = 310
          Height = 241
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelInner = bvRaised
          BevelOuter = bvLowered
          ParentBackground = False
          TabOrder = 0
          object FSelect: TTreeView_Ext
            Left = 2
            Top = 2
            Width = 306
            Height = 237
            Align = alClient
            BorderStyle = bsNone
            HideSelection = False
            Indent = 19
            MultiSelect = True
            MultiSelectStyle = [msControlSelect, msShiftSelect, msSiblingOnly]
            ReadOnly = True
            ShowLines = False
            ShowRoot = False
            TabOrder = 0
            OnChange = FSelectChange
            OnExpanding = FSelectExpanding
            OnGetImageIndex = FSelectGetImageIndex
          end
        end
      end
    end
    object TSJob: TTabSheet
      Caption = 'TSJob'
      TabVisible = False
      OnShow = TSJobShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        333
        271)
      object GBasics: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 265
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GBasics'
        TabOrder = 0
        TabStop = True
        object FLName: TLabel
          Left = 8
          Top = 17
          Width = 40
          Height = 13
          Caption = 'FLName'
          FocusControl = FName
        end
        object FLExportType: TLabel
          Left = 8
          Top = 48
          Width = 66
          Height = 13
          Caption = 'FLExportType'
        end
        object FLFilename: TLabel
          Left = 8
          Top = 234
          Width = 54
          Height = 13
          Caption = 'FLFilename'
          FocusControl = FFilename
        end
        object FLDataSource: TLabel
          Left = 8
          Top = 234
          Width = 69
          Height = 13
          Caption = 'FLDataSource'
          FocusControl = FDataSource
        end
        object FName: TEdit
          Left = 128
          Top = 16
          Width = 187
          Height = 21
          TabOrder = 0
          Text = 'FName'
          OnChange = FJobOptionChange
        end
        object FSQLFile: TRadioButton
          Left = 128
          Top = 47
          Width = 185
          Height = 17
          Caption = 'FSQLFile'
          TabOrder = 1
          TabStop = True
          OnClick = FExportTypeChange
        end
        object FTextFile: TRadioButton
          Left = 128
          Top = 67
          Width = 185
          Height = 17
          Caption = 'FTextFile'
          TabOrder = 2
          TabStop = True
          OnClick = FExportTypeChange
        end
        object FExcelFile: TRadioButton
          Left = 128
          Top = 87
          Width = 185
          Height = 17
          Caption = 'FExcelFile'
          TabOrder = 3
          TabStop = True
          OnClick = FExportTypeChange
        end
        object FAccessFile: TRadioButton
          Left = 128
          Top = 107
          Width = 185
          Height = 17
          Caption = 'FAccessFile'
          TabOrder = 4
          TabStop = True
          OnClick = FExportTypeChange
        end
        object FODBC: TRadioButton
          Left = 128
          Top = 127
          Width = 185
          Height = 17
          Caption = 'FODBC'
          TabOrder = 5
          TabStop = True
          OnClick = FExportTypeChange
        end
        object FHTMLFile: TRadioButton
          Left = 128
          Top = 147
          Width = 185
          Height = 17
          Caption = 'FHTMLFile'
          TabOrder = 6
          TabStop = True
          OnClick = FExportTypeChange
        end
        object FXMLFile: TRadioButton
          Left = 128
          Top = 167
          Width = 185
          Height = 17
          Caption = 'FXMLFile'
          TabOrder = 7
          TabStop = True
          OnClick = FExportTypeChange
        end
        object FPDFFile: TRadioButton
          Left = 128
          Top = 187
          Width = 185
          Height = 17
          Caption = 'FPDFFile'
          TabOrder = 8
          TabStop = True
          OnClick = FExportTypeChange
        end
        object FFilename: TEdit
          Left = 128
          Top = 233
          Width = 166
          Height = 21
          TabOrder = 9
          Text = 'FFilename'
          OnChange = FFilenameChange
        end
        object FBFilename: TButton
          Left = 294
          Top = 233
          Width = 21
          Height = 21
          Caption = #183#183#183
          TabOrder = 10
          OnClick = FBFilenameClick
        end
        object FDataSource: TEdit
          Left = 128
          Top = 233
          Width = 166
          Height = 21
          ReadOnly = True
          TabOrder = 12
          Text = 'FDataSource'
          OnClick = FBDataSourceClick
        end
        object FBDataSource: TButton
          Left = 294
          Top = 233
          Width = 21
          Height = 21
          Caption = #183#183#183
          TabOrder = 11
          OnClick = FBDataSourceClick
        end
      end
    end
    object TSSQLOptions: TTabSheet
      Caption = 'TSSQLOptions'
      TabVisible = False
      OnHide = TSOptionsHide
      OnShow = TSSQLOptionsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        333
        271)
      object GSQLWhat: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 63
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GSQLWhat'
        TabOrder = 0
        object FLSQLWhat: TLabel
          Left = 8
          Top = 17
          Width = 59
          Height = 13
          Caption = 'FLSQLWhat'
        end
        object FSQLStructure: TCheckBox
          Left = 128
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FSQLStructure'
          TabOrder = 0
          OnClick = FSQLOptionClick
          OnKeyPress = FSQLOptionKeyPress
        end
        object FSQLData: TCheckBox
          Left = 128
          Top = 36
          Width = 193
          Height = 17
          Caption = 'FSQLData'
          TabOrder = 1
          OnClick = FSQLOptionClick
          OnKeyPress = FSQLOptionKeyPress
        end
      end
      object GSQLOptions: TGroupBox_Ext
        Left = 4
        Top = 70
        Width = 325
        Height = 143
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GSQLOptions'
        TabOrder = 1
        object FLGeneral: TLabel
          Left = 9
          Top = 117
          Width = 49
          Height = 13
          Caption = 'FLGeneral'
        end
        object FLDrop: TLabel
          Left = 8
          Top = 67
          Width = 35
          Height = 13
          Caption = 'FLDrop'
        end
        object FLDatabaseHandling: TLabel
          Left = 8
          Top = 17
          Width = 100
          Height = 13
          Caption = 'FLDatabaseHandling'
        end
        object FDropStmts: TCheckBox
          Left = 128
          Top = 66
          Width = 193
          Height = 17
          Caption = 'FDropStmts'
          TabOrder = 2
          OnClick = FSQLOptionClick
          OnKeyPress = FSQLOptionKeyPress
        end
        object FUseDatabase: TCheckBox
          Left = 128
          Top = 36
          Width = 193
          Height = 17
          Caption = 'FUseDatabase'
          TabOrder = 1
          OnClick = FSQLOptionClick
          OnKeyPress = FSQLOptionKeyPress
        end
        object FCreateDatabase: TCheckBox
          Left = 128
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FCreateDatabase'
          TabOrder = 0
          OnClick = FSQLOptionClick
          OnKeyPress = FSQLOptionKeyPress
        end
        object FDisableKeys: TCheckBox
          Left = 129
          Top = 116
          Width = 193
          Height = 17
          Caption = 'FDisableKeys'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = FSQLOptionClick
          OnKeyPress = FSQLOptionKeyPress
        end
        object FReplaceData: TCheckBox
          Left = 128
          Top = 86
          Width = 193
          Height = 17
          Caption = 'FReplaceData'
          TabOrder = 3
          OnClick = FSQLOptionClick
          OnKeyPress = FSQLOptionKeyPress
        end
      end
    end
    object TSCSVOptions: TTabSheet
      Caption = 'TSCSVOptions'
      TabVisible = False
      OnHide = TSOptionsHide
      OnShow = TSCSVOptionsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        333
        271)
      object GCSVOptions: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 197
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GCSVOptions'
        TabOrder = 0
        object FLCSVHeadline: TLabel
          Left = 8
          Top = 17
          Width = 75
          Height = 13
          Caption = 'FLCSVHeadline'
        end
        object PQuote: TPanel_Ext
          Left = 4
          Top = 98
          Width = 317
          Height = 89
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 2
          object FLQuoteValues: TLabel
            Left = 4
            Top = 4
            Width = 73
            Height = 13
            Caption = 'FLQuoteValues'
          end
          object FLQuoteChar: TLabel
            Left = 4
            Top = 67
            Width = 63
            Height = 13
            Caption = 'FLQuoteChar'
            FocusControl = FQuoteChar
          end
          object FQuoteChar: TEdit
            Left = 124
            Top = 64
            Width = 17
            Height = 21
            MaxLength = 1
            TabOrder = 3
            Text = 'FQuoteChar'
            OnExit = FQuoteCharExit
          end
          object FQuoteNothing: TRadioButton
            Left = 124
            Top = 3
            Width = 193
            Height = 17
            Caption = 'FQuoteNothing'
            TabOrder = 2
            OnClick = FQuoteClick
          end
          object FQuoteStrings: TRadioButton
            Left = 124
            Top = 23
            Width = 193
            Height = 17
            Caption = 'FQuoteStrings'
            TabOrder = 1
            OnClick = FQuoteClick
          end
          object FQuoteAll: TRadioButton
            Left = 124
            Top = 43
            Width = 193
            Height = 17
            Caption = 'FQuoteAll'
            TabOrder = 0
            OnClick = FQuoteClick
            OnKeyPress = FQuoteKeyPress
          end
        end
        object FCSVHeadline: TCheckBox
          Left = 128
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FCSVHeadline'
          TabOrder = 0
        end
        object PSeparator: TPanel_Ext
          Left = 4
          Top = 44
          Width = 317
          Height = 53
          BevelOuter = bvNone
          Ctl3D = True
          ParentBackground = False
          ParentCtl3D = False
          TabOrder = 1
          object FLSeparator: TLabel
            Left = 4
            Top = 6
            Width = 58
            Height = 13
            Caption = 'FLSeparator'
          end
          object FSeparatorChar: TRadioButton
            Left = 124
            Top = 26
            Width = 77
            Height = 17
            Caption = 'FSeparatorChar'
            TabOrder = 1
            OnClick = FSeparatorClick
            OnKeyPress = FSeparatorKeyPress
          end
          object FSeparator: TEdit
            Left = 207
            Top = 24
            Width = 21
            Height = 21
            TabOrder = 2
            Text = 'FSeparatorChar'
          end
          object FSeparatorTab: TRadioButton
            Left = 124
            Top = 5
            Width = 193
            Height = 17
            Caption = 'FSeparatorTab'
            TabOrder = 0
            OnClick = FSeparatorClick
            OnKeyPress = FSeparatorKeyPress
          end
        end
      end
    end
    object TSXMLOptions: TTabSheet
      Caption = 'TSXMLOptions'
      TabVisible = False
      OnHide = TSXMLOptionsHide
      OnShow = TSXMLOptionsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        333
        271)
      object GXMLHow: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 265
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GXMLHow'
        TabOrder = 0
        object FLRootNode: TLabel
          Left = 3
          Top = 21
          Width = 61
          Height = 13
          Caption = 'FLRootNode'
          FocusControl = FRootNodeText
        end
        object FL2RootNodeText: TLabel
          Left = 128
          Top = 19
          Width = 6
          Height = 13
          Caption = '<'
        end
        object FL3RootNodeText: TLabel
          Left = 191
          Top = 19
          Width = 6
          Height = 13
          Caption = '>'
        end
        object FLRecordNode: TLabel
          Left = 8
          Top = 185
          Width = 73
          Height = 13
          Caption = 'FLRecordNode'
          FocusControl = FRecordNodeText
        end
        object FL2RecordTag: TLabel
          Left = 128
          Top = 185
          Width = 6
          Height = 13
          Caption = '<'
        end
        object FL3RecordTag: TLabel
          Left = 191
          Top = 185
          Width = 6
          Height = 13
          Caption = '>'
        end
        object FRootNodeText: TEdit
          Left = 134
          Top = 16
          Width = 57
          Height = 21
          CharCase = ecLowerCase
          TabOrder = 0
          Text = 'mysql'
          OnChange = TSXMLOptionChange
        end
        object PDatabaseNode: TPanel_Ext
          Left = 4
          Top = 48
          Width = 317
          Height = 57
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 4
          object FLDatabaseNode: TLabel
            Left = 4
            Top = 1
            Width = 84
            Height = 13
            Caption = 'FLDatabaseNode'
          end
          object FL1DatabaseTagFree: TLabel
            Left = 124
            Top = 37
            Width = 6
            Height = 13
            Caption = '<'
          end
          object FL2DatabaseNodeCustom: TLabel
            Left = 231
            Top = 37
            Width = 66
            Height = 13
            Caption = '="db_name">'
          end
          object FDatabaseNodeCustom: TRadioButton
            Left = 106
            Top = 36
            Width = 17
            Height = 17
            TabOrder = 2
            OnClick = FDatabaseNodeClick
            OnKeyPress = FDatabaseNodeKeyPress
          end
          object FDatabaseNodeText: TEdit
            Left = 130
            Top = 34
            Width = 57
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 3
            Text = 'database'
            OnChange = TSXMLOptionChange
          end
          object FDatabaseNodeDisabled: TRadioButton
            Left = 106
            Top = 0
            Width = 185
            Height = 17
            Caption = 'FDatabaseNodeDisabled'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = FDatabaseNodeClick
            OnKeyPress = FDatabaseNodeKeyPress
          end
          object FDatabaseNodeName: TRadioButton
            Left = 106
            Top = 18
            Width = 185
            Height = 17
            Caption = '<db_name>'
            TabOrder = 1
            OnClick = FDatabaseNodeClick
            OnKeyPress = FDatabaseNodeKeyPress
          end
          object FDatabaseNodeAttribute: TEdit
            Left = 194
            Top = 34
            Width = 37
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 4
            Text = 'name'
            OnChange = TSXMLOptionChange
          end
        end
        object PTableNode: TPanel_Ext
          Left = 4
          Top = 112
          Width = 317
          Height = 57
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 2
          object FLTableNode: TLabel
            Left = 4
            Top = 1
            Width = 65
            Height = 13
            Caption = 'FLTableNode'
          end
          object FL1TableNodeCustom: TLabel
            Left = 124
            Top = 37
            Width = 6
            Height = 13
            Caption = '<'
          end
          object FL2TableNodeCustom: TLabel
            Left = 231
            Top = 37
            Width = 65
            Height = 13
            Caption = '="tbl_name">'
          end
          object FTableNodeCustom: TRadioButton
            Left = 106
            Top = 36
            Width = 17
            Height = 17
            TabOrder = 2
            OnClick = FTableTagClick
            OnKeyPress = FTableTagKeyPress
          end
          object FTableNodeText: TEdit
            Left = 130
            Top = 34
            Width = 57
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 3
            Text = 'table'
            OnChange = TSXMLOptionChange
          end
          object FTableNodeDisabled: TRadioButton
            Left = 106
            Top = 0
            Width = 185
            Height = 17
            Caption = 'FTableNodeDisabled'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = FTableTagClick
            OnKeyPress = FTableTagKeyPress
          end
          object FTableNodeName: TRadioButton
            Left = 106
            Top = 18
            Width = 185
            Height = 17
            Caption = '<tbl_name>'
            TabOrder = 1
            OnClick = FTableTagClick
            OnKeyPress = FTableTagKeyPress
          end
          object FTableNodeAttribute: TEdit
            Left = 194
            Top = 34
            Width = 37
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 4
            Text = 'name'
            OnChange = TSXMLOptionChange
          end
        end
        object FRecordNodeText: TEdit
          Left = 134
          Top = 182
          Width = 57
          Height = 21
          CharCase = ecLowerCase
          TabOrder = 3
          Text = 'row'
          OnChange = TSXMLOptionChange
        end
        object PFieldNode: TPanel_Ext
          Left = 4
          Top = 216
          Width = 317
          Height = 41
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 1
          object FLFieldNode: TLabel
            Left = 4
            Top = 1
            Width = 60
            Height = 13
            Caption = 'FLFieldNode'
          end
          object FL1FieldNodeCustom: TLabel
            Left = 124
            Top = 19
            Width = 6
            Height = 13
            Caption = '<'
          end
          object FL2FieldNodeCustom: TLabel
            Left = 231
            Top = 19
            Width = 65
            Height = 13
            Caption = '="fld_name">'
          end
          object FFieldNodeCustom: TRadioButton
            Left = 106
            Top = 18
            Width = 17
            Height = 17
            Checked = True
            TabOrder = 1
            TabStop = True
            OnClick = FFieldTagClick
            OnKeyPress = FFieldTagKeyPress
          end
          object FFieldNodeText: TEdit
            Left = 130
            Top = 16
            Width = 57
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 2
            Text = 'field'
            OnChange = TSXMLOptionChange
          end
          object FFieldNodeName: TRadioButton
            Left = 106
            Top = 0
            Width = 185
            Height = 17
            Caption = '<fld_name>'
            TabOrder = 0
            OnClick = FFieldTagClick
            OnKeyPress = FFieldTagKeyPress
          end
          object FFieldNodeAttribute: TEdit
            Left = 194
            Top = 16
            Width = 37
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 3
            Text = 'name'
            OnChange = TSXMLOptionChange
          end
        end
      end
    end
    object TSHTMLOptions: TTabSheet
      Caption = 'TSHTMLOptions'
      TabVisible = False
      OnHide = TSOptionsHide
      OnShow = TSHTMLOptionsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        333
        271)
      object GHTMLWhat: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 69
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GHTMLWhat'
        TabOrder = 0
        object FLHTMLWhat: TLabel
          Left = 8
          Top = 17
          Width = 68
          Height = 13
          Caption = 'FLHTMLWhat'
          FocusControl = FHTMLStructure
        end
        object FHTMLStructure: TCheckBox
          Left = 128
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FHTMLStructure'
          TabOrder = 0
          OnClick = FHTMLStructureClick
          OnKeyPress = FHTMLStructureKeyPress
        end
        object FHTMLData: TCheckBox
          Left = 128
          Top = 40
          Width = 193
          Height = 17
          Caption = 'FHTMLData'
          TabOrder = 1
          OnClick = FHTMLDataClick
          OnKeyPress = FHTMLDataKeyPress
        end
      end
      object GHTMLOptions: TGroupBox_Ext
        Left = 4
        Top = 76
        Width = 325
        Height = 109
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GHTMLOptions'
        TabOrder = 1
        object FLHTMLNullValues: TLabel
          Left = 8
          Top = 17
          Width = 92
          Height = 13
          Caption = 'FLHTMLNullValues'
          FocusControl = FHTMLNullText
        end
        object FLHTMLViewDatas: TLabel
          Left = 8
          Top = 49
          Width = 93
          Height = 13
          Caption = 'FLHTMLViewDatas'
        end
        object FLHTMLBGColorEnabled: TLabel
          Left = 9
          Top = 81
          Width = 120
          Height = 13
          Caption = 'FLHTMLBGColorEnabled'
        end
        object FHTMLNullText: TCheckBox
          Left = 128
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FHTMLNullText'
          TabOrder = 0
        end
        object FHTMLMemoContent: TCheckBox
          Left = 128
          Top = 48
          Width = 193
          Height = 17
          Caption = 'FHTMLMemoContent'
          TabOrder = 1
        end
        object FHTMLRowBGColor: TCheckBox
          Left = 129
          Top = 80
          Width = 193
          Height = 17
          Caption = 'FHTMLRowBGColor'
          TabOrder = 2
        end
      end
    end
    object TSFields: TTabSheet
      Caption = 'TSFields'
      TabVisible = False
      OnShow = TSFieldsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        333
        271)
      object GFields: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 265
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'GFields'
        TabOrder = 0
        DesignSize = (
          325
          265)
        object FLFields: TLabel
          Left = 8
          Top = 24
          Width = 39
          Height = 13
          Caption = 'FLFields'
        end
        object FLDestFields: TLabel
          Left = 172
          Top = 24
          Width = 61
          Height = 13
          Caption = 'FLDestFields'
        end
        object ScrollBox: TScrollBox
          Left = 4
          Top = 40
          Width = 309
          Height = 217
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelInner = bvNone
          BorderStyle = bsNone
          TabOrder = 0
          object FLReferrer1: TLabel
            Left = 149
            Top = 11
            Width = 9
            Height = 13
            Caption = '->'
            Visible = False
          end
          object FField1: TComboBox_Ext
            Left = 4
            Top = 8
            Width = 135
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            TabStop = False
            Visible = False
            OnChange = FField1Change
            OnExit = FField1Exit
          end
          object FField2: TComboBox_Ext
            Left = 4
            Top = 40
            Width = 135
            Height = 21
            Style = csDropDownList
            TabOrder = 1
            TabStop = False
            Visible = False
          end
          object FDestField1: TEdit
            Left = 168
            Top = 8
            Width = 117
            Height = 21
            TabStop = False
            TabOrder = 2
            Text = 'FDestField1'
            Visible = False
            OnChange = FDestField1Change
          end
        end
      end
    end
    object TSTask: TTabSheet
      Caption = 'TSTask'
      ImageIndex = 9
      TabVisible = False
      OnShow = TSTaskShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        333
        271)
      object GTask: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 173
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GTask'
        TabOrder = 0
        object FLEnabled: TLabel
          Left = 8
          Top = 143
          Width = 51
          Height = 13
          Caption = 'FLEnabled'
          FocusControl = FEnabled
        end
        object FLExecution: TLabel
          Left = 7
          Top = 55
          Width = 59
          Height = 13
          Caption = 'FLExecution'
        end
        object FLStart: TLabel
          Left = 7
          Top = 23
          Width = 34
          Height = 13
          Caption = 'FLStart'
          FocusControl = FStartDate
        end
        object FEnabled: TCheckBox
          Left = 128
          Top = 142
          Width = 193
          Height = 17
          Caption = 'FEnabled'
          TabOrder = 6
        end
        object FSingle: TRadioButton
          Left = 128
          Top = 54
          Width = 193
          Height = 17
          Caption = 'FSingle'
          TabOrder = 2
        end
        object FStartDate: TDateTimePicker
          Left = 128
          Top = 19
          Width = 81
          Height = 21
          Date = 2.500000000000000000
          Time = 2.500000000000000000
          TabOrder = 0
        end
        object FStartTime: TDateTimePicker
          Left = 214
          Top = 19
          Width = 69
          Height = 21
          Date = 1.000000000000000000
          Time = 1.000000000000000000
          Kind = dtkTime
          TabOrder = 1
        end
        object FDaily: TRadioButton
          Left = 128
          Top = 74
          Width = 193
          Height = 17
          Caption = 'FDaily'
          TabOrder = 3
        end
        object FWeekly: TRadioButton
          Left = 128
          Top = 94
          Width = 193
          Height = 17
          Caption = 'FWeekly'
          TabOrder = 4
        end
        object FMonthly: TRadioButton
          Left = 128
          Top = 114
          Width = 193
          Height = 17
          Caption = 'FMonthly'
          TabOrder = 5
        end
      end
    end
    object TSExecute: TTabSheet
      Caption = 'TSExecute'
      TabVisible = False
      OnShow = TSExecuteShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        333
        271)
      object GProgress: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 169
        Caption = 'GProgress'
        TabOrder = 0
        DesignSize = (
          325
          169)
        object FLProgressTables: TLabel
          Left = 8
          Top = 40
          Width = 85
          Height = 13
          Caption = 'FLProgressTables'
        end
        object FLProgressRecords: TLabel
          Left = 8
          Top = 64
          Width = 93
          Height = 13
          Caption = 'FLProgressRecords'
        end
        object FDoneTables: TLabel
          Left = 171
          Top = 40
          Width = 64
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FDoneTables'
        end
        object FDoneRecords: TLabel
          Left = 163
          Top = 64
          Width = 72
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FDoneRecords'
        end
        object FLProgressTime: TLabel
          Left = 8
          Top = 88
          Width = 76
          Height = 13
          Caption = 'FLProgressTime'
        end
        object FDoneTime: TLabel
          Left = 180
          Top = 88
          Width = 55
          Height = 13
          Alignment = taRightJustify
          Caption = 'FDoneTime'
        end
        object FLDone: TLabel
          Left = 197
          Top = 16
          Width = 38
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FLDone'
        end
        object FLEntiered: TLabel
          Left = 265
          Top = 16
          Width = 51
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FLEntiered'
        end
        object FEntieredTables: TLabel
          Left = 240
          Top = 40
          Width = 77
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FEntieredTables'
        end
        object FEntieredRecords: TLabel
          Left = 232
          Top = 64
          Width = 85
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FEntieredRecords'
        end
        object FEntieredTime: TLabel
          Left = 249
          Top = 88
          Width = 68
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FEntieredTime'
        end
        object FLErrors: TLabel
          Left = 8
          Top = 148
          Width = 39
          Height = 13
          Caption = 'FLErrors'
        end
        object FErrors: TLabel
          Left = 283
          Top = 148
          Width = 33
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FErrors'
        end
        object FProgressBar: TProgressBar
          Left = 8
          Top = 120
          Width = 308
          Height = 16
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
      end
      object GErrors: TGroupBox_Ext
        Left = 4
        Top = 176
        Width = 325
        Height = 89
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'GErrors'
        TabOrder = 1
        DesignSize = (
          325
          89)
        object PErrorMessages: TPanel_Ext
          Left = 8
          Top = 16
          Width = 308
          Height = 65
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelInner = bvRaised
          BevelOuter = bvLowered
          ParentBackground = False
          TabOrder = 0
          object FErrorMessages: TRichEdit
            Left = 2
            Top = 2
            Width = 304
            Height = 61
            TabStop = False
            Align = alClient
            BorderStyle = bsNone
            Ctl3D = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Lines.Strings = (
              'FErrorMessages')
            ParentCtl3D = False
            ParentFont = False
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
        end
      end
    end
  end
  object FBForward: TButton
    Left = 171
    Top = 296
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBForward'
    Default = True
    TabOrder = 4
    OnClick = FBForwardClick
  end
  object FBCancel: TButton
    Left = 257
    Top = 296
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 5
    OnClick = FBCancelClick
  end
  object FBBack: TButton
    Left = 96
    Top = 296
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBBack'
    TabOrder = 3
    OnClick = FBBackClick
  end
  object FBHelp: TButton
    Left = 8
    Top = 296
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'FBHelp'
    TabOrder = 2
    OnClick = FBHelpClick
  end
  object SaveDialog: TSaveDialog_Ext
    Options = [ofOverwritePrompt, ofHideReadOnly, ofExtensionDifferent, ofCreatePrompt, ofNoReadOnlyReturn, ofNoNetworkButton, ofEnableSizing]
    EncodingIndex = -1
    EncodingLabel = '&Encoding:'
    Left = 88
    Top = 288
  end
  object PrintDialog: TPrintDialog_Ext
    FromPage = 1
    MinPage = 1
    MaxPage = 3
    Options = [poWarning]
    ToPage = 2
    Left = 120
    Top = 288
  end
end
