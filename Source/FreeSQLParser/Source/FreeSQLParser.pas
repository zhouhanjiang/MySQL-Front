unit FreeSQLParser;

interface {********************************************************************}

uses
  Classes,
  SQLUtils,
  fspTypes, fspConst;

type
  TCustomSQLParser = class
  public
    type
      TFileType = (ftSQL, ftDebugHTML);
      TSQLDialect = (sdMySQL);

  protected
    type
      TOffset = Integer;
      TOrigin = record X, Y: Integer; end;

  private
    type
      TCreateTableColumnAdd = (caAdd, caChange, caModify, caNone);
      TCreateTableIndexAdd = (iaAdd, iaCreate, iaNone);
      TIntegerArray = array of Integer;
      TRoutineType = (rtFunction, rtProcedure);
      TTableOptionNodes = record
        AutoIncrementValue: TOffset;
        AvgRowLengthValue: TOffset;
        CharacterSetValue: TOffset;
        ChecksumValue: TOffset;
        CollateValue: TOffset;
        CommentValue: TOffset;
        ConnectionValue: TOffset;
        DataDirectoryValue: TOffset;
        DelayKeyWriteValue: TOffset;
        EngineValue: TOffset;
        IndexDirectoryValue: TOffset;
        InsertMethodValue: TOffset;
        KeyBlockSizeValue: TOffset;
        MaxRowsValue: TOffset;
        MinRowsValue: TOffset;
        PackKeysValue: TOffset;
        PasswordValue: TOffset;
        RowFormatValue: TOffset;
        StatsAutoRecalcValue: TOffset;
        StatsPersistentValue: TOffset;
        UnionList: TOffset;
      end;
      TValueAssign = (vaYes, vaNo, vaAuto);

  protected
    type
      TWordList = class(TObject)
      private type
        TIndex = SmallInt;
        TIndices = array of TIndex;
      private
        FCount: TIndex;
        FIndex: array of PChar;
        FFirst: array of Integer;
        FParser: TCustomSQLParser;
        FText: string;
        function GetWord(Index: TIndex): string;
        procedure SetText(AText: string);
      protected
        procedure Clear();
        property Parser: TCustomSQLParser read FParser;
      public
        constructor Create(const ASQLParser: TCustomSQLParser; const AText: string = '');
        destructor Destroy(); override;
        function IndexOf(const Word: PChar; const Length: Integer): Integer;
        property Count: TIndex read FCount;
        property Text: string read FText write SetText;
        property Word[Index: TIndex]: string read GetWord; default;
      end;

  public
    type
      PNode = ^TNode;
      PToken = ^TToken;
      PStmt = ^TStmt;

      TParseFunction = function(): TOffset of object;

      { Base nodes ------------------------------------------------------------}

      TNode = packed record
      private
        FNodeType: TNodeType;
        FParser: TCustomSQLParser;
      private
        class function Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): TOffset; static;
        function GetOffset(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        property Offset: Integer read GetOffset;
      public
        property NodeType: TNodeType read FNodeType;
        property Parser: TCustomSQLParser read FParser;
      end;

      PRoot = ^TRoot;
      TRoot = packed record
      private
        Heritage: TNode;
      private
        FFirstStmt: TOffset;
        FFirstToken: TOffset;
        FLastStmt: TOffset;
        FLastToken: TOffset;
        class function Create(const AParser: TCustomSQLParser): TOffset; static;
        function GetFirstStmt(): PStmt; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastStmt(): PStmt; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property FirstStmt: PStmt read GetFirstStmt;
        property FirstToken: PToken read GetFirstToken;
        property LastStmt: PStmt read GetLastStmt;
        property LastToken: PToken read GetLastToken;
        property NodeType: TNodeType read Heritage.FNodeType;
        property Parser: TCustomSQLParser read Heritage.FParser;
      end;

      PChild = ^TChild;
      TChild = packed record  // Every node, except TRoot
      private
        Heritage: TNode;
      private
        FParentNode: TOffset;
        function GetFFirstToken(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFLastToken(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetNextSibling(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        property FFirstToken: TOffset read GetFFirstToken;
        property FLastToken: TOffset read GetFLastToken;
      public
        property FirstToken: PToken read GetFirstToken;
        property LastToken: PToken read GetLastToken;
        property NextSibling: PChild read GetNextSibling;
        property NodeType: TNodeType read Heritage.FNodeType;
        property ParentNode: PNode read GetParentNode;
        property Parser: TCustomSQLParser read Heritage.FParser;
      end;

      TToken = packed record
      private
        Heritage: TChild;
        property FParentNode: TOffset read Heritage.FParentNode write Heritage.FParentNode;
      private
        FErrorCode: Integer;
        FKeywordIndex: TWordList.TIndex;
        FMySQLVersion: Integer;
        FOperatorType: TOperatorType;
        FOrigin: TOrigin;
        FPriorToken: TOffset;
        FText: packed record
          SQL: PChar;
          Length: Integer;
        end;
        FTokenType: TTokenType;
        FUsageType: TUsageType;
        class function Create(const AParser: TCustomSQLParser;
          const ASQL: PChar; const ALength: Integer; const AOrigin: TOrigin;
          const AErrorCode: Integer; const AMySQLVersion: Integer; const ATokenType: TTokenType;
          const AOperatorType: TOperatorType; const AKeywordIndex: TWordList.TIndex; const AUsageType: TUsageType): TOffset; static;
        function GetAsString(): string;
        function GetDbIdentType(): TDbIdentType;
        function GetErrorMessage(): string;
        function GetGeneration(): Integer;
        function GetIndex(): Integer;
        function GetIsUsed(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
        function GetNextToken(): PToken;
        function GetOffset(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetText(): string;
        procedure SetText(AText: string);
        property Generation: Integer read GetGeneration;
        property Index: Integer read GetIndex;
        property Offset: Integer read GetOffset;
      public
        property AsString: string read GetAsString;
        property DbIdentType: TDbIdentType read GetDbIdentType;
        property ErrorCode: Integer read FErrorCode;
        property ErrorMessage: string read GetErrorMessage;
        property IsUsed: Boolean read GetIsUsed;
        property KeywordIndex: TWordList.TIndex read FKeywordIndex;
        property MySQLVersion: Integer read FMySQLVersion;
        property NextToken: PToken read GetNextToken;
        property NodeType: TNodeType read Heritage.Heritage.FNodeType;
        property OperatorType: TOperatorType read FOperatorType;
        property Origin: TOrigin read FOrigin;
        property ParentNode: PNode read GetParentNode;
        property Parser: TCustomSQLParser read Heritage.Heritage.FParser;
        property Text: string read GetText write SetText;
        property TokenType: TTokenType read FTokenType;
        property UsageType: TUsageType read FUsageType;
      end;

      PRange = ^TRange;
      TRange = packed record
      private
        Heritage: TChild;
        property FParentNode: TOffset read Heritage.FParentNode write Heritage.FParentNode;
      private
        FFirstToken: TOffset;
        FLastToken: TOffset;
        class function Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): TOffset; static;
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOffset(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        procedure AddChild(const ChildNode: TOffset);
        property Offset: TOffset read GetOffset;
      public
        property FirstToken: PToken read GetFirstToken;
        property LastToken: PToken read GetLastToken;
        property NodeType: TNodeType read Heritage.Heritage.FNodeType;
        property Parser: TCustomSQLParser read Heritage.Heritage.FParser;
        property ParentNode: PNode read GetParentNode;
      end;

      PDeleted = ^TDeleted;
      TDeleted = packed record
      private
        Heritage: TRange;
        property FNodeType: TNodeType read Heritage.Heritage.Heritage.FNodeType write Heritage.Heritage.Heritage.FNodeType;
      private
        FNodeSize: Integer;
      end;

      TStmt = packed record
      private
        Heritage: TRange;
        property FFirstToken: TOffset read Heritage.FFirstToken write Heritage.FFirstToken;
        property FLastToken: TOffset read Heritage.FLastToken write Heritage.FLastToken;
        property FParentNode: TOffset read Heritage.Heritage.FParentNode write Heritage.Heritage.FParentNode;
      private
        FStmtType: TStmtType;
        FErrorCode: Integer;
        FErrorToken: TOffset;
        class function Create(const AParser: TCustomSQLParser; const AStmtType: TStmtType): TOffset; static;
        function GetError(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
        function GetErrorMessage(): string;
        function GetErrorToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetNextStmt(): PStmt;
      public
        property Error: Boolean read GetError;
        property ErrorCode: Integer read FErrorCode;
        property ErrorMessage: string read GetErrorMessage;
        property ErrorToken: PToken read GetErrorToken;
        property FirstToken: PToken read GetFirstToken;
        property LastToken: PToken read GetLastToken;
        property NextStmt: PStmt read GetNextStmt;
        property NodeType: TNodeType read Heritage.Heritage.Heritage.FNodeType;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        property StmtType: TStmtType read FStmtType;
      end;

      { Normal nodes ----------------------------------------------------------}

      PAlterDatabaseStmt = ^TAlterDatabaseStmt;
      TAlterDatabaseStmt = packed record
      private type
        TNodes = packed record
          AlterTag: TOffset;
          DatabaseTag: TOffset;
          IdentTag: TOffset;
          CharacterSetValue: TOffset;
          CollateValue: TOffset;
          UpgradeDataDirectoryNameTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterEventStmt = ^TAlterEventStmt;
      TAlterEventStmt = packed record
      private type
        TNodes = packed record
          AlterTag: TOffset;
          DefinerNode: TOffset;
          EventTag: TOffset;
          EventIdent: TOffset;
          OnScheduleValue: TOffset;
          OnCompletitionTag: TOffset;
          RenameValue: TOffset;
          EnableTag: TOffset;
          CommentValue: TOffset;
          DoTag: TOffset;
          Body: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterRoutineStmt = ^TAlterRoutineStmt;
      TAlterRoutineStmt = packed record
      private type
        TNodes = packed record
          AlterTag: TOffset;
          RoutineTag: TOffset;
          IdentNode: TOffset;
          CommentValue: TOffset;
          CommentStringNode: TOffset;
          LanguageTag: TOffset;
          CharacteristicTag: TOffset;
          SQLSecurityTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterServerStmt = ^TAlterServerStmt;
      TAlterServerStmt = packed record
      private type
        TNodes = packed record
          AlterTag: TOffset;
          ServerTag: TOffset;
          IdentNode: TOffset;
          OptionsTag: TOffset;
          OptionsList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterTableStmt = ^TAlterTableStmt;
      TAlterTableStmt = packed record
      private type

        PAlterColumn = ^TAlterColumn;
        TAlterColumn = packed record
        private type
          TNodes = packed record
            AlterTag: TOffset;
            ColumnIdent: TOffset;
            SetDefaultValue: TOffset;
            DropDefaultTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PConvertTo = ^TConvertTo;
        TConvertTo = packed record
        private type
          TNodes = packed record
            ConvertToTag: TOffset;
            CharacterSetTag: TOffset;
            CharacterSetNameToken: TOffset;
            CollateTag: TOffset;
            CollateNameToken: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PDropObject = ^TDropObject;
        TDropObject = packed record
        private type
          TNodes = packed record
            DropTag: TOffset;
            ObjectTypeTag: TOffset;
            NameNode: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PExchangePartition = ^TExchangePartition;
        TExchangePartition = packed record
        private type
          TNodes = packed record
            ExchangePartitionTag: TOffset;
            PartitionIdent: TOffset;
            WithTableTag: TOffset;
            TableIdent: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PReorganizePartition = ^TReorganizePartition;
        TReorganizePartition = packed record
        private type
          TNodes = packed record
            ReorganizePartitionTag: TOffset;
            PartitionIdentList: TOffset;
            IntoTag: TOffset;
            PartitionList: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          AlterTag: TOffset;
          TableTag: TOffset;
          IdentNode: TOffset;
          SpecificationList: TOffset;
          AlgorithmValue: TOffset;
          ConvertToCharacterSetNode: TOffset;
          DiscardTablespaceTag: TOffset;
          EnableKeys: TOffset;
          ForceTag: TOffset;
          IgnoreTag: TOffset;
          ImportTablespaceTag: TOffset;
          LockValue: TOffset;
          OrderByValue: TOffset;
          RenameNode: TOffset;
          TableOptionsNodes: TTableOptionNodes;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterViewStmt = ^TAlterViewStmt;
      TAlterViewStmt = packed record
      private type
        TNodes = packed record
          AlterTag: TOffset;
          AlgorithmValue: TOffset;
          DefinerNode: TOffset;
          SQLSecurityTag: TOffset;
          ViewTag: TOffset;
          IdentNode: TOffset;
          Columns: TOffset;
          AsTag: TOffset;
          SelectStmt: TOffset;
          OptionTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PBinaryOp = ^TBinaryOp;
      TBinaryOp = packed record
      private
        Heritage: TRange;
      private type
        TNodes = packed record
          Operand1: TOffset;
          Operand2: TOffset;
          Operator: TOffset;
        end;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const AOperator, AOperand1, AOperand2: TOffset): TOffset; static;
        function GetOperand1(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperand2(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Operand1: PChild read GetOperand1;
        property Operand2: PChild read GetOperand2;
        property Operator: PChild read GetOperator;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PBetweenOp = ^TBetweenOp;
      TBetweenOp = packed record
      private type
        TNodes = packed record
          Expr: TOffset;
          Max: TOffset;
          Min: TOffset;
          Operator1: TOffset;
          Operator2: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const AOperator1, AOperator2, AExpr, AMin, AMax: TOffset): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PCallStmt = ^TCallStmt;
      TCallStmt = packed record
      private type
        TNodes = packed record
          CallTag: TOffset;
          ProcedureIdent: TOffset;
          ParamList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCaseOp = ^TCaseOp;
      TCaseOp = packed record
      private type

        PBranch = ^TBranch;
        TBranch = packed record
        private type
          TNodes = packed record
            Tag: TOffset;
            ConditionExpr: TOffset;
            ThenTag: TOffset;
            ResultExpr: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          CaseTag: TOffset;
          CompareExpr: TOffset;
          BranchList: TOffset;
          EndTag: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PCaseStmt = ^TCaseStmt;
      TCaseStmt = packed record
      private type

        PBranch = ^TBranch;
        TBranch = packed record
        private type
          TNodes = packed record
            Tag: TOffset;
            ConditionExpr: TOffset;
            ThenTag: TOffset;
            StmtList: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          CaseTag: TOffset;
          CompareExpr: TOffset;
          BranchList: TOffset;
          EndTag: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PColumn = ^TColumn;
      TColumn = packed record
      private type
        TNodes = packed record
          AddTag: TOffset;
          ColumnTag: TOffset;
          OldNameIdent: TOffset;
          NameIdent: TOffset;
          DataTypeNode: TOffset;
          Null: TOffset;
          DefaultValue: TOffset;
          AutoIncrementTag: TOffset;
          KeyTag: TOffset;
          CommentValue: TOffset;
          ColumnFormat: TOffset;
          ReferencesNode: TOffset;
          Position: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PCompoundStmt = ^TCompoundStmt;
      TCompoundStmt = packed record
      private type
        TNodes = packed record
          BeginLabelToken: TOffset;
          BeginTag: TOffset;
          StmtList: TOffset;
          EndTag: TOffset;
          EndLabelToken: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateDatabaseStmt = ^TCreateDatabaseStmt;
      TCreateDatabaseStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          DatabaseTag: TOffset;
          IfNotExistsTag: TOffset;
          DatabaseIdent: TOffset;
          CharacterSetValue: TOffset;
          CollateValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateEventStmt = ^TCreateEventStmt;
      TCreateEventStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          DefinerNode: TOffset;
          EventTag: TOffset;
          IfNotExistsTag: TOffset;
          EventIdent: TOffset;
          OnScheduleValue: TOffset;
          OnCompletitionTag: TOffset;
          EnableTag: TOffset;
          CommentValue: TOffset;
          DoTag: TOffset;
          Body: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateIndexStmt = ^TCreateIndexStmt;
      TCreateIndexStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          IndexTag: TOffset;
          IndexIdent: TOffset;
          OnTag: TOffset;
          TableIdent: TOffset;
          IndexTypeValue: TOffset;
          ColumnNameList: TOffset;
          AlgorithmValue: TOffset;
          CommentValue: TOffset;
          KeyBlockSizeValue: TOffset;
          LockValue: TOffset;
          ParserValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateRoutineStmt = ^TCreateRoutineStmt;
      TCreateRoutineStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          DefinerNode: TOffset;
          RoutineTag: TOffset;
          IdentNode: TOffset;
          ParameterNode: TOffset;
          Return: record
            ReturnsTag: TOffset;
            DataTypeNode: TOffset;
          end;
          CommentValue: TOffset;
          CommentStringNode: TOffset;
          LanguageTag: TOffset;
          DeterministicTag: TOffset;
          CharacteristicTag: TOffset;
          SQLSecurityTag: TOffset;
          Body: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateServerStmt = ^TCreateServerStmt;
      TCreateServerStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          ServerTag: TOffset;
          ServerIdent: TOffset;
          ForeignDataWrapperValue: TOffset;
          OptionsTag: TOffset;
          OptionsList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateTableStmt = ^TCreateTableStmt;
      TCreateTableStmt = packed record
      private type

        PReference = ^TReference;
        TReference = packed record
        private type
          TNodes = packed record
            ReferencesTag: TOffset;
            TableIdent: TOffset;
            IndicesList: TOffset;
            MatchValue: TOffset;
            OnDeleteValue: TOffset;
            OnUpdateValue: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          CreateTag: TOffset;
          TemporaryTag: TOffset;
          TableTag: TOffset;
          IfNotExistsTag: TOffset;
          TableIdent: TOffset;
          OpenBracketToken: TOffset;
          DefinitionList: TOffset;
          TableOptionsNodes: TTableOptionNodes;
          TableOptionList: TOffset;
          PartitionOption: packed record
            PartitionByTag: TOffset;
            PartitionKindTag: TOffset;
            PartitionAlgorithmValue: TOffset;
            PartitionExprList: TOffset;
            PartitionsValue: TOffset;
            SubPartitionByTag: TOffset;
            SubPartitionKindTag: TOffset;
            SubPartitionAlgorithmValue: TOffset;
            SubPartitionExprList: TOffset;
            SubPartitionsValue: TOffset;
          end;
          PartitionDefinitionList: TOffset;
          LikeTag: TOffset;
          LikeTableIdent: TOffset;
          CloseBracketToken: TOffset;
          IgnoreReplaceTag: TOffset;
          AsTag: TOffset;
          SelectStmt: TOffset;
        end;

      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateTriggerStmt = ^TCreateTriggerStmt;
      TCreateTriggerStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          DefinerNode: TOffset;
          TriggerTag: TOffset;
          TriggerIdent: TOffset;
          ActionValue: TOffset;
          OnTag: TOffset;
          TableIdentNode: TOffset;
          ForEachRowTag: TOffset;
          Body: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateViewStmt = ^TCreateViewStmt;
      TCreateViewStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          OrReplaceTag: TOffset;
          AlgorithmValue: TOffset;
          DefinerNode: TOffset;
          SQLSecurityTag: TOffset;
          ViewTag: TOffset;
          IdentNode: TOffset;
          Columns: TOffset;
          AsTag: TOffset;
          SelectStmt: TOffset;
          OptionTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDataType = ^TDataType;
      TDataType = packed record
      private type
        TNodes = packed record
          IdentToken: TOffset;
          OpenBracketToken: TOffset;
          CloseBracketToken: TOffset;
          LengthToken: TOffset;
          CommaToken: TOffset;
          DecimalsToken: TOffset;
          StringValuesNode: TOffset;
          UnsignedTag: TOffset;
          ZerofillTag: TOffset;
          CharacterSetTag: TOffset;
          CharacterSetValueToken: TOffset;
          CollateTag: TOffset;
          CollateValueToken: TOffset;
          BinaryTag: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PDbIdent = ^TDbIdent;
      TDbIdent = packed record
      private type
        TNodes = packed record
          Prefix1: TOffset;
          Prefix2: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FDbIdentType: TDbIdentType;
        FNodes: TNodes;
        procedure AddPrefix(const APrefix, ADot: TOffset);
        class function Create(const AParser: TCustomSQLParser; const AIdent: TOffset; const ADbIdentType: TDbIdentType = ditUnknown): TOffset; static;
        function GetIdent(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetPrefix1(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetPrefix2(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property DbIdentType: TDbIdentType read FDbIdentType;
        property Ident: PToken read GetIdent;
        property LastToken: PToken read GetLastToken;
        property ParentNode: PNode read GetParentNode;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        property Prefix1: PToken read GetPrefix1;
        property Prefix2: PToken read GetPrefix2;
      end;

      PDeleteStmt = ^TDeleteStmt;
      TDeleteStmt = packed record
      private type
        TNodes = packed record
          DeleteTag: TOffset;
          LowPriorityTag: TOffset;
          QuickTag: TOffset;
          IgnoreTag: TOffset;
          FromTag: TOffset;
          TableIdent: TOffset;
          PartitionTag: TOffset;
          PartitionList: TOffset;
          WhereValue: TOffset;
          OrderByValue: TOffset;
          LimitValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDoStmt = ^TDoStmt;
      TDoStmt = packed record
      private type
        TNodes = packed record
          DoTag: TOffset;
          ExprList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropDatabaseStmt = ^TDropDatabaseStmt;
      TDropDatabaseStmt = packed record
      private type
        TNodes = packed record
          DropTag: TOffset;
          DatabaseTag: TOffset;
          IfExistsTag: TOffset;
          DatabaseIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropEventStmt = ^TDropEventStmt;
      TDropEventStmt = packed record
      private type
        TNodes = packed record
          DropTag: TOffset;
          EventTag: TOffset;
          IfExistsTag: TOffset;
          EventIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropIndexStmt = ^TDropIndexStmt;
      TDropIndexStmt = packed record
      private type
        TNodes = packed record
          DropTag: TOffset;
          IndexTag: TOffset;
          IndexIdent: TOffset;
          OnTag: TOffset;
          TableIdent: TOffset;
          AlgorithmValue: TOffset;
          LockValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropRoutineStmt = ^TDropRoutineStmt;
      TDropRoutineStmt = packed record
      private type
        TNodes = packed record
          DropTag: TOffset;
          RoutineTag: TOffset;
          IfExistsTag: TOffset;
          RoutineIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropServerStmt = ^TDropServerStmt;
      TDropServerStmt = packed record
      private type
        TNodes = packed record
          DropTag: TOffset;
          ServerTag: TOffset;
          IfExistsTag: TOffset;
          ServerIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropTableStmt = ^TDropTableStmt;
      TDropTableStmt = packed record
      private type
        TNodes = packed record
          DropTag: TOffset;
          TemporaryTag: TOffset;
          TableTag: TOffset;
          IfExistsTag: TOffset;
          TableIdentList: TOffset;
          RestrictCascadeTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropTriggerStmt = ^TDropTriggerStmt;
      TDropTriggerStmt = packed record
      private type
        TNodes = packed record
          DropTag: TOffset;
          TriggerTag: TOffset;
          IfExistsTag: TOffset;
          TriggerIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropViewStmt = ^TDropViewStmt;
      TDropViewStmt = packed record
      private type
        TNodes = packed record
          DropTag: TOffset;
          TemporaryTag: TOffset;
          ViewTag: TOffset;
          IfExistsTag: TOffset;
          ViewIdentList: TOffset;
          RestrictCascadeTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PForeignKey = ^TForeignKey;
      TForeignKey = packed record
      private type
        TNodes = packed record
          AddTag: TOffset;
          ConstraintTag: TOffset;
          SymbolIdent: TOffset;
          KeyTag: TOffset;
          NameToken: TOffset;
          ColumnNameList: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PFunction = ^TFunction;
      TFunction = packed record
      private type
        TNodes = packed record
          Arguments: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const AIdent, AArguments: TOffset): TOffset; static;
        function GetArguments(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetIdent(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Arguments: PChild read GetArguments;
        property Ident: PChild read GetIdent;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PIfStmt = ^TIfStmt;
      TIfStmt = packed record
      private type

        PBranch = ^TBranch;
        TBranch = packed record
        private type
          TNodes = packed record
            Tag: TOffset;
            SearchConditionExpr: TOffset;
            ThenTag: TOffset;
            StmtList: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          BranchList: TOffset;
          EndTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PIndex = ^TIndex;
      TIndex = packed record
      private type
        TNodes = packed record
          AddTag: TOffset;
          ConstraintTag: TOffset;
          SymbolIdent: TOffset;
          IndexTag: TOffset;
          IndexIdent: TOffset;
          IndexTypeValue: TOffset;
          ColumnNameList: TOffset;
          CommentValue: TOffset;
          KeyBlockSizeValue: TOffset;
          ParserValue: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PIndexColumn = ^TIndexColumn;
      TIndexColumn = packed record
      private type
        TNodes = packed record
          IdentTag: TOffset;
          OpenBracketToken: TOffset;
          LengthToken: TOffset;
          CloseBracketToken: TOffset;
          SortTag: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PInsertStmt = ^TInsertStmt;
      TInsertStmt = packed record
      private type
        TNodes = packed record
          InsertTag: TOffset;
          PriorityTag: TOffset;
          IgnoreTag: TOffset;
          IntoTag: TOffset;
          TableIdent: TOffset;
          PartitionTag: TOffset;
          PartitionList: TOffset;
          ColumnList: TOffset;
          ValuesTag: TOffset;
          SetTag: TOffset;
          SelectTag: TOffset;
          ValuesList: TOffset;
          SelectStmt: TOffset;
          OnDuplicateKeyUpdateTag: TOffset;
          UpdateList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PIterateStmt = ^TIterateStmt;
      TIterateStmt = packed record
      private type
        TNodes = packed record
          IterateToken: TOffset;
          LabelToken: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PLeaveStmt = ^TLeaveStmt;
      TLeaveStmt = packed record
      private type
        TNodes = packed record
          LeaveToken: TOffset;
          LabelToken: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      // PList = ^TList; defined after TList definition, otherwise its assigns Classes.TList
      TList = packed record
      private type
        TNodes = packed record
          OpenBracket: TOffset;
          FirstChild: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes; const AChildrenCount: Integer; const AChildrens: array of TOffset): TOffset; static;
        function GetFirstChild(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property FirstChild: PChild read GetFirstChild;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;
      PList = ^TList;

      PLoopStmt = ^TLoopStmt;
      TLoopStmt = packed record
      private type
        TNodes = packed record
          BeginLabelToken: TOffset;
          BeginTag: TOffset;
          StmtList: TOffset;
          EndTag: TOffset;
          EndLabelToken: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PPartition = ^TPartition;
      TPartition = packed record
      private type
        TNodes = packed record
          AddTag: TOffset;
          PartitionTag: TOffset;
          NameIdent: TOffset;
          ValuesNode: TOffset;
          EngineValue: TOffset;
          CommentValue: TOffset;
          DataDirectoryValue: TOffset;
          IndexDirectoryValue: TOffset;
          MaxRowsValue: TOffset;
          MinRowsValue: TOffset;
          SubPartitionList: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PPartitionValues = ^TPartitionValues;
      TPartitionValues = packed record
      private type
        TNodes = packed record
          ValuesTag: TOffset;
          DescriptionValue: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PRenameTableStmt = ^TRenameTableStmt;
      TRenameTableStmt = packed record
      private type
        TNodes = packed record
          RenameTag: TOffset;
          RenameList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PRenameTableStmtPair = ^TRenameTableStmtPair;
      TRenameTableStmtPair = packed record
      private type
        TNodes = packed record
          OrgTableIdent: TOffset;
          ToTag: TOffset;
          NewTableIdent: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PRepeatStmt = ^TRepeatStmt;
      TRepeatStmt = packed record
      private type
        TNodes = packed record
          BeginLabelToken: TOffset;
          RepeatTag: TOffset;
          StmtList: TOffset;
          UntilTag: TOffset;
          SearchConditionExpr: TOffset;
          EndTag: TOffset;
          EndLabelToken: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PRoutineParam = ^TRoutineParam;
      TRoutineParam = packed record
      private type
        TNodes = packed record
          DirektionTag: TOffset;
          IdentToken: TOffset;
          DataTypeNode: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSelectStmt = ^TSelectStmt;
      TSelectStmt = packed record
      private type
        PField = ^TField;
        TField = packed record
        private
          Heritage: TRange;
        private
          FAsToken: TOffset;
          FAlias: TOffset;
          FExpr: TOffset;
          class function Create(const AParser: TCustomSQLParser; const AValue, AAsToken, AAlias: TOffset): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PTableFactor = ^TTableFactor;
        TTableFactor = packed record
        private type

          PIndexHint = ^TIndexHint;
          TIndexHint = packed record
          public type
            TIndexHintKind = (ihkUnknown, ihkJoin, ihkOrderBy, ihkGroupBy);
            TIndexHintType = (ihtUnknown, ihtUse, ihtIgnore, ihtForce);
          private
            Heritage: TRange;
          private
            FIndexHintType: TIndexHintType;
            FIndexHintKind: TIndexHintKind;
            class function Create(const AParser: TCustomSQLParser; const AIndexHintType: TIndexHintType; const AIndexHintKind: TIndexHintKind): TOffset; static;
          public
            property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
          end;

          TNodes = packed record
            TableIdent: TOffset;
            PartitionTag: TOffset;
            Partitions: TOffset;
            AsToken: TOffset;
            AliasToken: TOffset;
            IndexHints: TOffset;
            SelectStmt: TOffset;
          end;

        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PTableFactorOj = ^TTableFactorOj;
        TTableFactorOj = packed record
        private type
          TNodes = packed record
            OpenBracketToken: TOffset;
            OjTag: TOffset;
            LeftTableReference: TOffset;
            LeftOuterJoinTag: TOffset;
            RightTableReference: TOffset;
            OnTag: TOffset;
            CondExpr: TOffset;
            CloseBracketToken: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PTableFactorReferences = ^TTableFactorReferences;
        TTableFactorReferences = packed record
        private type
          TNodes = packed record
            ReferenceList: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PTableFactorSelect = ^TTableFactorSelect;
        TTableFactorSelect = packed record
        private type
          TNodes = packed record
            SelectStmt: TOffset;
            AsToken: TOffset;
            AliasToken: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PJoin = ^TJoin;
        TJoin = packed record
        private type
          TKeywordTokens = array [0..3] of Integer;
        private
          Heritage: TRange;
        private
          FCondition: TOffset;
          FJoinType: TJoinType;
          FLeftTable: TOffset;
          FRightTable: TOffset;
          class function Create(const AParser: TCustomSQLParser; const ALeftTable: TOffset; const AJoinType: TJoinType; const ARightTable: TOffset; const ACondition: TOffset; const AKeywordTokens: TKeywordTokens): TOffset; static;
        end;

        PGroup = ^TGroup;
        TGroup = packed record
        private
          Heritage: TRange;
        private
          FExpr: TOffset;
          FDirection: TOffset;
          class function Create(const AParser: TCustomSQLParser; const AExpr, ADirection: TOffset): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        POrder = ^TOrderBy;
        TOrderBy = packed record
        private
          Heritage: TRange;
        private
          class function Create(const AParser: TCustomSQLParser; const AExpr, ADirection: TOffset): TOffset; static;
        public
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          SelectTag: TOffset;
          DistinctTag: TOffset;
          HighPriorityTag: TOffset;
          StraightJoinTag: TOffset;
          SQLSmallResultTag: TOffset;
          SQLBigResultTag: TOffset;
          SQLBufferResultTag: TOffset;
          SQLNoCacheTag: TOffset;
          SQLCalcFoundRowsTag: TOffset;
          ColumnsNode: TOffset;
          FromTag: TOffset;
          TablesNodes: TOffset;
          WhereTag: TOffset;
          WhereNode: TOffset;
          GroupByTag: TOffset;
          GroupsNode: TOffset;
          WithRollupTag: TOffset;
          HavingToken: TOffset;
          HavingNode: TOffset;
          OrderByTag: TOffset;
          OrdersNode: TOffset;
          Limit: record
            LimitToken: TOffset;
            OffsetToken: TOffset;
            OffsetValueToken: TOffset;
            CommaToken: TOffset;
            RowCountValueToken: TOffset;
          end;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSchedule = ^TSchedule;
      TSchedule = packed record
      private type

        PInterval = ^TInterval;
        TInterval = packed record
        private type
          TNodes = packed record
            QuantityExp: TOffset;
            UnitTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        end;

        PIntervalListItem = ^TIntervalListItem;
        TIntervalListItem = packed record
        private type
          TNodes = packed record
            PlusToken: TOffset;
            IntervalTag: TOffset;
            Interval: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
        end;

        TIntervalList = array [0..15 - 1] of TOffset;

        TNodes = packed record
          AtValue: TOffset;
          AtIntervalList: TIntervalList;
          EveryValue: TOffset;
          StartsValue: TOffset;
          StartsIntervalList: TIntervalList;
          EndsValue: TOffset;
          EndsIntervalList: TIntervalList;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSoundsLikeOp = ^TSoundsLikeOp;
      TSoundsLikeOp = packed record
      private type
        TNodes = packed record
          Operand1: TOffset;
          Operand2: TOffset;
          Operator1: TOffset;
          Operator2: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const AOperator1, AOperator2: TOffset; const AOperand1, AOperand2: TOffset): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSubArea = ^TSubArea;
      TSubArea = packed record
      private type
        TNodes = packed record
          OpenBracket: TOffset;
          AreaNode: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSubPartition = ^TSubPartition;
      TSubPartition = packed record
      private type
        TNodes = packed record
          SubPartitionTag: TOffset;
          NameIdent: TOffset;
          EngineValue: TOffset;
          CommentValue: TOffset;
          DataDirectoryValue: TOffset;
          IndexDirectoryValue: TOffset;
          MaxRowsValue: TOffset;
          MinRowsValue: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PTag = ^TTag;
      TTag = packed record
      private type
        TNodes = packed record
          KeywordToken1: TOffset;
          KeywordToken2: TOffset;
          KeywordToken3: TOffset;
          KeywordToken4: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PTruncateTableStmt = ^TTruncateTableStmt;
      TTruncateTableStmt = packed record
      private type
        TNodes = packed record
          TruncateTag: TOffset;
          TableTag: TOffset;
          TableIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PUnaryOp = ^TUnaryOp;
      TUnaryOp = packed record
      private type
        TNodes = packed record
          Operand: TOffset;
          Operator: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const AOperator, AOperand: TOffset): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PUnknownStmt = ^TUnknownStmt;
      TUnknownStmt = packed record
      private
        Heritage: TStmt;
      private
        class function Create(const AParser: TCustomSQLParser; const ATokens: Classes.TList): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PUser = ^TUser;
      TUser = packed record
      private type
        TNodes = packed record
          NameToken: TOffset;
          AtToken: TOffset;
          HostToken: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PValue = ^TValue;
      TValue = packed record
      private type
        TNodes = packed record
          IdentTag: TOffset;
          AssignToken: TOffset;
          ValueNode: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PWhileStmt = ^TWhileStmt;
      TWhileStmt = packed record
      private type
        TNodes = packed record
          BeginLabelToken: TOffset;
          WhileTag: TOffset;
          SearchConditionExpr: TOffset;
          DoTag: TOffset;
          StmtList: TOffset;
          EndTag: TOffset;
          EndLabelToken: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

  protected
    FErrorCode: Integer;
    FErrorToken: TOffset;
    FFunctions: TWordList;
    FHighNotPrecedence: Boolean;
    FKeywords: TWordList;
    FMySQLVersion: Integer;
    FNodes: packed record
      Mem: PAnsiChar;
      Offset: Integer;
      Size: Integer;
    end;
    FParsedText: string;
    FParsedTokens: Classes.TList;
    FParsePos: packed record Text: PChar; Length: Integer; Origin: TOrigin; end;
    FPipesAsConcat: Boolean;
    FRoot: TOffset;
    FSQLDialect: TSQLDialect;
    function GetCurrentToken(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function GetErrorMessage(const AErrorCode: Integer): string;
    function GetFunctions(): string; {$IFNDEF Debug} inline; {$ENDIF}
    function GetKeywords(): string; {$IFNDEF Debug} inline; {$ENDIF}
    function GetNextToken(Index: Integer): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function GetParsedToken(Index: Integer): TOffset;
    function GetRoot(): PRoot; {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetFunctions(AFunctions: string); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetKeywords(AKeywords: string);

  private
    kiACTION,
    kiADD,
    kiAFTER,
    kiALGORITHM,
    kiALL,
    kiALTER,
    kiANALYZE,
    kiAND,
    kiAS,
    kiASC,
    kiAT,
    kiAUTO_INCREMENT,
    kiAVG_ROW_LENGTH,
    kiBEFORE,
    kiBEGIN,
    kiBETWEEN,
    kiBINARY,
    kiBTREE,
    kiBY,
    kiCALL,
    kiCASCADE,
    kiCASCADED,
    kiCASE,
    kiCHANGE,
    kiCHARACTER,
    kiCHECK,
    kiCHECKSUM,
    kiCOALESCE,
    kiCOLLATE,
    kiCOLUMN,
    kiCOLUMN_FORMAT,
    kiCOMMENT,
    kiCOMPLETION,
    kiCONNECTION,
    kiCONSTRAINT,
    kiCONTAINS,
    kiCONVERT,
    kiCOPY,
    kiCREATE,
    kiCROSS,
    kiCURRENT_USER,
    kiDATA,
    kiDATABASE,
    kiDAY,
    kiDAY_HOUR,
    kiDAY_MINUTE,
    kiDAY_SECOND,
    kiDEFAULT,
    kiDEFINER,
    kiDELAY_KEY_WRITE,
    kiDELAYED,
    kiDELETE,
    kiDESC,
    kiDETERMINISTIC,
    kiDIRECTORY,
    kiDISABLE,
    kiDISCARD,
    kiDISTINCT,
    kiDISTINCTROW,
    kiDIV,
    kiDO,
    kiDROP,
    kiDUPLICATE,
    kiEACH,
    kiELSE,
    kiELSEIF,
    kiENABLE,
    kiEND,
    kiENDS,
    kiENGINE,
    kiEVENT,
    kiEVERY,
    kiEXCHANGE,
    kiEXCLUSIVE,
    kiEXISTS,
    kiFIRST,
    kiFOR,
    kiFORCE,
    kiFOREIGN,
    kiFROM,
    kiFULL,
    kiFULLTEXT,
    kiFUNCTION,
    kiGROUP,
    kiHASH,
    kiHAVING,
    kiHIGH_PRIORITY,
    kiHOST,
    kiHOUR,
    kiHOUR_MINUTE,
    kiHOUR_SECOND,
    kiIF,
    kiIGNORE,
    kiIMPORT,
    kiIN,
    kiINDEX,
    kiINNER,
    kiINOUT,
    kiINPLACE,
    kiINSERT,
    kiINSERT_METHOD,
    kiINTERVAL,
    kiINTO,
    kiINVOKER,
    kiIS,
    kiITERATE,
    kiJOIN,
    kiKEY,
    kiKEY_BLOCK_SIZE,
    kiKEYS,
    kiLANGUAGE,
    kiLEAVE,
    kiLEFT,
    kiLESS,
    kiLIKE,
    kiLIMIT,
    kiLINEAR,
    kiLIST,
    kiLOCAL,
    kiLOCK,
    kiLOOP,
    kiLOW_PRIORITY,
    kiMATCH,
    kiMAX_ROWS,
    kiMAXVALUE,
    kiMERGE,
    kiMIN_ROWS,
    kiMINUTE,
    kiMINUTE_SECOND,
    kiMOD,
    kiMODIFIES,
    kiMODIFY,
    kiMONTH,
    kiNAME,
    kiNATURAL,
    kiNO,
    kiNONE,
    kiNOT,
    kiNULL,
    kiOFFSET,
    kiOJ,
    kiON,
    kiOPTIMIZE,
    kiOPTION,
    kiOPTIONS,
    kiOR,
    kiORDER,
    kiOUT,
    kiOUTER,
    kiOWNER,
    kiPACK_KEYS,
    kiPARSER,
    kiPARTIAL,
    kiPARTITION,
    kiPARTITIONING,
    kiPARTITIONS,
    kiPASSWORD,
    kiPORT,
    kiPRESERVE,
    kiPRIMARY,
    kiPROCEDURE,
    kiQUARTER,
    kiQUICK,
    kiRANGE,
    kiREADS,
    kiREBUILD,
    kiREFERENCES,
    kiREGEXP,
    kiREMOVE,
    kiRENAME,
    kiREORGANIZE,
    kiREPEAT,
    kiREPAIR,
    kiREPLACE,
    kiRESTRICT,
    kiRETURNS,
    kiRIGHT,
    kiRLIKE,
    kiROLLUP,
    kiROW,
    kiROW_FORMAT,
    kiSCHEDULE,
    kiSCHEMA,
    kiSECOND,
    kiSECURITY,
    kiSELECT,
    kiSERVER,
    kiSET,
    kiSHARED,
    kiSIMPLE,
    kiSOCKET,
    kiSOUNDS,
    kiSPATIAL,
    kiSQL,
    kiSQL_BIG_RESULT,
    kiSQL_BUFFER_RESULT,
    kiSQL_CACHE,
    kiSQL_CALC_FOUND_ROWS,
    kiSQL_NO_CACHE,
    kiSQL_SMALL_RESULT,
    kiSTARTS,
    kiSTATS_AUTO_RECALC,
    kiSTATS_PERSISTENT,
    kiSTORAGE,
    kiSTRAIGHT_JOIN,
    kiSUBPARTITION,
    kiSUBPARTITIONS,
    kiTABLE,
    kiTABLESPACE,
    kiTEMPORARY,
    kiTEMPTABLE,
    kiTHAN,
    kiTHEN,
    kiTO,
    kiTRIGGER,
    kiTRUNCATE,
    kiUNDEFINED,
    kiUNION,
    kiUNIQUE,
    kiUNSIGNED,
    kiUNTIL,
    kiUPDATE,
    kiUPGRADE,
    kiUSE,
    kiUSER,
    kiUSING,
    kiVALUE,
    kiVALUES,
    kiVIEW,
    kiWEEK,
    kiWHEN,
    kiWHERE,
    kiWHILE,
    kiWRAPPER,
    kiWITH,
    kiXOR,
    kiYEAR,
    kiYEAR_MONTH,
    kiZEROFILL: Integer;

    OperatorTypeByKeywordIndex: array of TOperatorType;

    function ApplyCurrentToken(const AUsageType: TUsageType = utUnknown; const ATokenType: TTokenType = ttUnknown): TOffset;
    procedure DeleteNode(const ANode: PNode);
    function GetError(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsChild(const ANode: PNode): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsRange(const ANode: PNode): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsStmt(const ANode: PNode): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsToken(const ANode: TOffset): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsToken(const ANode: PNode): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function NewNode(const ANodeType: TNodeType): TOffset;
    function NodePtr(const ANode: TOffset): PNode; {$IFNDEF Debug} inline; {$ENDIF}
    function NodeSize(const ANode: TOffset): Integer; overload;
    function NodeSize(const ANodeType: TNodeType): Integer; overload;
    function ParseAlterDatabaseStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseAlterEventStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseAlterRoutineStmt(const ARoutineType: TRoutineType): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseAlterServerStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseAlterTableStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseAlterTableStmtAlterColumn(): TOffset;
    function ParseAlterTableStmtConvertTo(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseAlterTableStmtDropObject(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseAlterTableStmtExchangePartition(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseAlterTableStmtReorganizePartition(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseAlterTableStmtUnion(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseAlterStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseAlterViewStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCallStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCaseOp(): TOffset;
    function ParseCaseOpBranch(): TOffset;
    function ParseCaseStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCaseStmtBranch(): TOffset;
    function ParseColumn(const Add: TCreateTableColumnAdd = caNone): TOffset;
    function ParseColumnNames(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCompoundStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseColumnIdent(): TOffset;
    function ParseCreateDatabaseStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCreateEventStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCreateIndexStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCreateRoutineStmt(const ARoutineType: TRoutineType): TOffset;
    function ParseCreateServerStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCreateStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCreateTableStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCreateTableStmtDefinition(): TOffset; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCreateTableStmtDefinition(const AlterTableStmt: Boolean): TOffset; overload;
    function ParseCreateTableStmtReferences(): TOffset;
    function ParseCreateTriggerStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCreateViewStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseDataType(): TOffset;
    function ParseDbIdent(const ADbIdentType: TDbIdentType): TOffset;
    function ParseDefinerValue(): TOffset;
    function ParseDeleteStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseDoStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseDropDatabaseStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseDropEventStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseDropIndexStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseDropRoutineStmt(const ARoutineType: TRoutineType): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseDropServerStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseDropStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseDropTableStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseDropTriggerStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseDropViewStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseEventIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseExpr(): TOffset;
    function ParseExprList(): TOffset;
    function ParseForeignKey(const Add: Boolean = False): TOffset;
    function ParseFunction(): TOffset;
    function ParseFunctionParam(): TOffset;
    function ParseGroup(): TOffset;
    function ParseIfStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseIfStmtBranch(): TOffset;
    function ParseIdent(): TOffset;
    function ParseIndex(const Add: Boolean = False): TOffset;
    function ParseIndexColumn(): TOffset;
    function ParseIndexHint(): TOffset;
    function ParseIndexIdent(): TOffset;
    function ParseInsertStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseInteger(): TOffset;
    function ParseInterval(): TOffset;
    function ParseIntervalList(): TSchedule.TIntervalList;
    function ParseIntervalListItem(const KeywordIndex: TWordList.TIndex): TOffset;
    function ParseIterateStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseKeyword(): TOffset;
    function ParseLeaveStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseList(const Brackets: Boolean; const ParseNode: TParseFunction = nil): TOffset; overload;
    function ParseList(const Brackets: Boolean; const ParseNode: TParseFunction; const DelimterType: TTokenType): TOffset; overload;
    function ParseLoopStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseTag(const KeywordIndex1: TWordList.TIndex; const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1; const KeywordIndex4: TWordList.TIndex = -1): TOffset;
    function ParseOrder(): TOffset;
    function ParsePartition(): TOffset; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function ParsePartition(const Add: Boolean): TOffset; overload;
    function ParsePartitionIdent(): TOffset;
    function ParsePartitionNames(): TOffset;
    function ParsePartitionValues(): TOffset;
    function ParsePL_SQLStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseProcedureParam(): TOffset;
    function ParseRenameTableStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseRenameTableStmtPair(): TOffset;
    function ParseRepeatStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseSchedule(): TOffset;
    function ParseSelectStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseSelectStmtField(): TOffset;
    function ParseServerOptionList(): TOffset;
    function ParseString(): TOffset;
    function ParseSubArea(const ParseNode: TParseFunction): TOffset;
    function ParseSubPartition(): TOffset;
    function ParseStmt(const PL_SQL: Boolean = False): TOffset;
    function ParseTableIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseTableReference(): TOffset;
    function ParseToken(): TOffset;
    function ParseTruncateTableStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseUnknownStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseUpdatePair(): TOffset;
    function ParseUser(): TOffset;
    function ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const ValueKeywordIndex1: TWordList.TIndex; const ValueKeywordIndex2: TWordList.TIndex = -1): TOffset; overload;
    function ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const OptionIndices: TWordList.TIndices): TOffset; overload;
    function ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ValueKeywordIndex1: TWordList.TIndex; const ValueKeywordIndex2: TWordList.TIndex = -1): TOffset; overload;
    function ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset; overload;
    function ParseViewIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseWhileStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function RangeNodePtr(const ANode: TOffset): PRange; {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetError(const AErrorCode: Integer; const AErrorNode: TOffset = 0);
    function StmtNodePtr(const ANode: TOffset): PChild; {$IFNDEF Debug} inline; {$ENDIF}
    function StmtPtr(const ANode: TOffset): PStmt; {$IFNDEF Debug} inline; {$ENDIF}
    function TokenPtr(const ANode: TOffset): PToken; {$IFNDEF Debug} inline; {$ENDIF}

    property CurrentToken: TOffset read GetCurrentToken;
    property Error: Boolean read GetError;
    property ParsedText: string read FParsedText;
    property NextToken[Index: Integer]: TOffset read GetNextToken;

  public
    constructor Create(const ASQLDialect: TSQLDialect);
    destructor Destroy(); override;
    function Parse(const Text: PChar; const Length: Integer): Boolean; overload;
    function Parse(const Text: string): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    procedure SaveToFile(const Filename: string; const FileType: TFileType = ftSQL);
    property Root: PRoot read GetRoot;
    property Functions: string read GetFunctions write SetFunctions;
    property HighNotPrecedence: Boolean read FHighNotPrecedence write FHighNotPrecedence;
    property Keywords: string read GetKeywords write SetKeywords;
    property PipesAsConcat: Boolean read FPipesAsConcat write FPipesAsConcat;
    property SQLDialect: TSQLDialect read FSQLDialect;
    property Text: string read FParsedText;
  end;

  TMySQLSQLParser = class(TCustomSQLParser)
  private
    FAnsiQuotes: Boolean;
    FLowerCaseTableNames: Integer;
    FMySQLVersion: Integer;
  public
    constructor Create(const MySQLVersion: Integer = 0; const LowerCaseTableNames: Integer = 0);
    property AnsiQuotes: Boolean read FAnsiQuotes write FAnsiQuotes;
    property LowerCaseTableNames: Integer read FLowerCaseTableNames write FLowerCaseTableNames;
    property MySQLVersion: Integer read FMySQLVersion write FMySQLVersion;
  end;

implementation {***************************************************************}

uses
  Windows,
  SysUtils, StrUtils, RTLConsts, Math,
  fspUtils;

resourcestring
  SUnknownError = 'Unknown Error';
  SKeywordNotFound = 'Keyword "%s" not found';
  SUnknownOperatorPrecedence = 'Unknown operator precedence for operator "%s"';
  STooManyTokensInExpr = 'Too many tokens (%d) in Expr';
  SUnknownNodeType = 'Unknown node type';

function WordIndices(const Index1: TCustomSQLParser.TWordList.TIndex;
  const Index2: TCustomSQLParser.TWordList.TIndex = -1;
  const Index3: TCustomSQLParser.TWordList.TIndex = -1;
  const Index4: TCustomSQLParser.TWordList.TIndex = -1;
  const Index5: TCustomSQLParser.TWordList.TIndex = -1): TCustomSQLParser.TWordList.TIndices;
begin
  if (Index5 > 0) then SetLength(Result, 5)
  else if (Index4 > 0) then SetLength(Result, 4)
  else if (Index3 > 0) then SetLength(Result, 3)
  else if (Index2 > 0) then SetLength(Result, 2)
  else SetLength(Result, 1);

  Result[0] := Index1;
  if (Index2 >= 0) then Result[1] := Index2;
  if (Index3 >= 0) then Result[2] := Index3;
  if (Index4 >= 0) then Result[3] := Index4;
  if (Index5 >= 0) then Result[4] := Index5;
end;

{ TCustomSQLParser.TWordList **************************************************}

procedure TCustomSQLParser.TWordList.Clear();
begin
  FText := '';

  FCount := 0;
  SetLength(FIndex, 0);

  SetLength(Parser.OperatorTypeByKeywordIndex, 0);
end;

constructor TCustomSQLParser.TWordList.Create(const ASQLParser: TCustomSQLParser; const AText: string = '');
begin
  inherited Create();

  FParser := ASQLParser;

  FCount := 0;
  SetLength(FIndex, 0);

  Text := AText;
end;

destructor TCustomSQLParser.TWordList.Destroy();
begin
  Clear();

  inherited;
end;

function TCustomSQLParser.TWordList.GetWord(Index: TWordList.TIndex): string;
begin
  Result := StrPas(FIndex[Index]);
end;

function TCustomSQLParser.TWordList.IndexOf(const Word: PChar; const Length: Integer): Integer;
var
  Comp: Integer;
  Left: Integer;
  Mid: Integer;
  Right: Integer;
begin
  Result := -1;

  if (Length <= System.Length(FFirst) - 2) then
  begin
    Left := FFirst[Length];
    Right := FFirst[Length + 1] - 1;
    while (Left <= Right) do
    begin
      Mid := (Right - Left) div 2 + Left;
      Comp := StrLIComp(FIndex[Mid], Word, Length);
      if (Comp < 0) then
        Left := Mid + 1
      else if (Comp = 0) then
        begin Result := Mid; break; end
      else
        Right := Mid - 1;
    end;
  end;
end;

procedure TCustomSQLParser.TWordList.SetText(AText: string);
var
  Counts: array of Integer;

  function InsertIndex(const Word: PChar; const Len: Integer; out Index: Integer): Boolean;
  var
    Comp: Integer;
    Left: Integer;
    Mid: Integer;
    Right: Integer;
  begin
    Result := True;

    if ((Counts[Len] = 0) or (StrLIComp(Word, FIndex[FFirst[Len] + Counts[Len] - 1], Len) > 0)) then
      Index := FFirst[Len] + Counts[Len]
    else
    begin
      Left := FFirst[Len];
      Right := Left + Counts[Len] - 1;
      while (Left <= Right) do
      begin
        Mid := (Right - Left) div 2 + Left;
        Comp := StrLIComp(FIndex[Mid], Word, Len);
        if (Comp < 0) then
          begin Left := Mid + 1;  Index := Mid + 1; end
        else if (Comp = 0) then
          begin Result := False; Index := Mid; break; end
        else
          begin Right := Mid - 1; Index := Mid; end;
      end;
    end;
  end;

  procedure Add(const Word: PChar; const Len: Integer);
  var
    I: Integer;
    Index: Integer;
  begin
    if (InsertIndex(Word, Len, Index)) then
    begin
      for I := FFirst[Len] + Counts[Len] - 1 downto Index do
        Move(FIndex[I], FIndex[I + 1], SizeOf(FIndex[0]));
      FIndex[Index] := Word;
      Inc(Counts[Len]);
    end;
  end;

var
  First: Integer;
  I: Integer;
  Index: Integer;
  Len: Integer;
  MaxLen: Integer;
  OldIndex: Integer;
begin
  Clear();

  FText := UpperCase(ReplaceStr(AText, ',', #0)) + #0;
  if (FText <> '') then
  begin
    SetLength(Counts, Length(FText) + 1);

    OldIndex := 1; Index := 1; MaxLen := 0; FCount := 0;
    while (Index < Length(FText)) do
    begin
      while (FText[Index] <> #0) do Inc(Index);
      Len := Index - OldIndex;
      Inc(Counts[Len]);
      Inc(FCount);
      if (Len > MaxLen) then MaxLen := Len;
      Inc(Index);
      OldIndex := Index;
    end;

    SetLength(FFirst, MaxLen + 2);
    SetLength(FIndex, FCount);
    First := 0;
    for I := 1 to MaxLen do
    begin
      FFirst[I] := First;
      Inc(First, Counts[I]);
      Counts[I] := 0;
    end;
    FFirst[MaxLen + 1] := First;

    OldIndex := 1; Index := 1;
    while (Index < Length(FText)) do
    begin
      while (FText[Index] <> #0) do Inc(Index);
      Len := Index - OldIndex;
      Add(@FText[OldIndex], Len);
      Inc(Index);
      OldIndex := Index;
    end;

    SetLength(Counts, 0);
  end;
end;

{ TCustomSQLParser.TNode ******************************************************}

class function TCustomSQLParser.TNode.Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): TOffset;
begin
  Result := AParser.NewNode(ANodeType);

  AParser.NodePtr(Result)^.FParser := AParser;
  AParser.NodePtr(Result)^.FNodeType := ANodeType;
end;

function TCustomSQLParser.TNode.GetOffset(): TOffset;
begin
  Result := @Self - Parser.FNodes.Mem;
end;

{ TCustomSQLParser.TChild *****************************************************}

function TCustomSQLParser.TChild.GetFFirstToken(): TOffset;
begin
  if (NodeType = ntToken) then
    Result := @Self - Parser.FNodes.Mem
  else
  begin
    Assert(Parser.IsRange(@Self));
    Result := TCustomSQLParser.PRange(@Self).FFirstToken;
  end;
end;

function TCustomSQLParser.TChild.GetFirstToken(): PToken;
begin
  if (NodeType = ntToken) then
    Result := @Self
  else
  begin
    Assert(Parser.IsRange(@Self));
    Result := PRange(@Self).FirstToken;
  end;
end;

function TCustomSQLParser.TChild.GetFLastToken(): TOffset;
begin
  if (NodeType = ntToken) then
    Result := PNode(@Self)^.Offset
  else
  begin
    Assert(Parser.IsRange(@Self));
    Result := PRange(@Self)^.FLastToken;
  end;
end;

function TCustomSQLParser.TChild.GetLastToken(): PToken;
begin
  if (NodeType = ntToken) then
    Result := @Self
  else
  begin
    Assert(Parser.IsRange(@Self));
    Result := PRange(@Self).LastToken;
  end;
end;

function TCustomSQLParser.TChild.GetNextSibling(): PChild;
var
  Node: PNode;
  Token: PToken;
begin
  Assert(Parser.IsChild(@Self));

  if (PChild(@Self)^.ParentNode^.NodeType <> ntList) then
    Result := nil
  else
  begin
    Token := PChild(@Self)^.LastToken^.NextToken;

    if (Assigned(Token) and (Token^.TokenType = ttComma)) then
      Token := PToken(Token)^.NextToken; // ttComma

    Node := PNode(Token);

    Result := nil;
    while (Assigned(Node) and (not Parser.IsToken(Node) or (PToken(Node)^.TokenType <> ttComma)) and Parser.IsChild(Node) and (PChild(Node) <> PChild(ParentNode))) do
    begin
      Result := PChild(Node);
      Node := PChild(Node)^.ParentNode;
    end;

    if (Assigned(Result) and ((PChild(Result)^.FParentNode = 0) or (Result^.NodeType = ntToken) or (Assigned(Node) and (PChild(@Self)^.FParentNode <> PChild(Result)^.FParentNode)))) then
      Result := nil;
  end;
end;

function TCustomSQLParser.TChild.GetParentNode(): PNode;
begin
  Result := Parser.NodePtr(FParentNode);
end;

{ TCustomSQLParser.TToken *****************************************************}

class function TCustomSQLParser.TToken.Create(const AParser: TCustomSQLParser;
  const ASQL: PChar; const ALength: Integer; const AOrigin: TOrigin;
  const AErrorCode: Integer; const AMySQLVersion: Integer; const ATokenType: fspTypes.TTokenType;
  const AOperatorType: TOperatorType; const AKeywordIndex: TWordList.TIndex; const AUsageType: TUsageType): TOffset;
begin
  Result := TNode.Create(AParser, ntToken);

  with PToken(AParser.NodePtr(Result))^ do
  begin
    Heritage.Heritage.FParser := AParser;
    FText.SQL := ASQL;
    FText.Length := ALength;
    FOrigin := AOrigin;
    FTokenType := ATokenType;
    FErrorCode := AErrorCode;
    FMySQLVersion := AMySQLVersion;
    FOperatorType := AOperatorType;
    FKeywordIndex := AKeywordIndex;
    FUsageType := AUsageType;
  end;
end;

function TCustomSQLParser.TToken.GetAsString(): string;
begin
  case (TokenType) of
    ttComment:
      if (Copy(Text, 1, 1) = '#') then
        Result := Trim(Copy(Text, Length(Text) - 1, 1))
      else if (Copy(Text, 1, 2) = '--') then
        Result := Trim(Copy(Text, 3, Length(Text) - 2))
      else if ((Copy(Text, 1, 2) = '/*') and (Copy(Text, Length(Text) - 1, 2) = '*/')) then
        Result := Trim(Copy(Text, 3, Length(Text) - 4))
      else
        Result := Text;
    ttBeginLabel:
      if (Copy(Text, Length(Text), 1) = ':') then
        Result := Trim(Copy(Text, 1, Length(Text) - 1))
      else
        Result := Text;
    ttBindVariable:
      if (Copy(Text, 1, 1) = ':') then
        Result := Trim(Copy(Text, 2, Length(Text) - 1))
      else
        Result := Text;
    ttString:
      Result := SQLUnescape(Text);
    ttDQIdentifier:
      Result := SQLUnescape(Text);
    ttDBIdentifier:
      if ((Copy(Text, 1, 1) = '[') and (Copy(Text, Length(Text), 1) = ']')) then
        Result := Trim(Copy(Text, 1, Length(Text) - 2))
      else
        Result := Text;
    ttBRIdentifier:
      if ((Copy(Text, 1, 1) = '{') and (Copy(Text, Length(Text), 1) = '}')) then
        Result := Trim(Copy(Text, 1, Length(Text) - 2))
      else
        Result := Text;
    ttMySQLIdentifier:
      Result := SQLUnescape(Text);
    ttMySQLCodeStart:
      Result := Copy(Text, 1, Length(Text) - 3);
    ttCSString:
      Result := Copy(Text, 1, Length(Text) - 1);
    else
      Result := Text;
  end;
end;

function TCustomSQLParser.TToken.GetDbIdentType(): TDbIdentType;
begin
  if ((FParentNode = 0) or (Parser.NodePtr(FParentNode)^.NodeType <> ntDbIdent)) then
    Result := ditUnknown
  else if (@Self = PDbIdent(Parser.NodePtr(FParentNode))^.Ident) then
    Result := PDbIdent(Parser.NodePtr(FParentNode))^.DbIdentType
  else if (@Self = PDbIdent(Parser.NodePtr(FParentNode))^.Prefix1) then
    case (PDbIdent(Parser.NodePtr(FParentNode))^.DbIdentType) of
      ditUnknown: Result := ditUnknown;
      ditTable,
      ditFunction,
      ditProcedure,
      ditTrigger,
      ditView,
      ditEvent: Result := ditDatabase;
      ditColumn,
      ditAllFields: Result := ditTable;
      else raise ERangeError.Create(SArgumentOutOfRange);
    end
  else if (@Self = PDbIdent(Parser.NodePtr(FParentNode))^.Prefix2) then
    case (PDbIdent(Parser.NodePtr(FParentNode))^.DbIdentType) of
      ditUnknown: Result := ditUnknown;
      ditColumn,
      ditAllFields: Result := ditDatabase;
      else raise ERangeError.Create(SArgumentOutOfRange);
    end
  else
    Result := ditUnknown;
end;

function TCustomSQLParser.TToken.GetErrorMessage(): string;
begin
  Result := Parser.GetErrorMessage(ErrorCode);
end;

function TCustomSQLParser.TToken.GetGeneration(): Integer;
var
  Node: PNode;
begin
  Result := 0;
  Node := ParentNode;
  while (Parser.IsChild(Node)) do
  begin
    Inc(Result);
    Node := PChild(Node)^.ParentNode;
  end;
end;

function TCustomSQLParser.TToken.GetIndex(): Integer;
var
  Token: PToken;
begin
  Token := Parser.Root^.FirstToken;
  Result := 0;
  while (Assigned(Token) and (Token <> @Self)) do
  begin
    Inc(Result);
    Token := Token^.NextToken;
  end;
end;

function TCustomSQLParser.TToken.GetIsUsed(): Boolean;
begin
  Result := not (TokenType in [ttSpace, ttReturn, ttComment]) and (not (Parser is TMySQLSQLParser) or (TMySQLSQLParser(Parser).MySQLVersion >= FMySQLVersion));
end;

function TCustomSQLParser.TToken.GetNextToken(): PToken;
var
  Offset: TOffset;
begin
  Offset := PNode(@Self)^.Offset;
  repeat
    repeat
      Inc(Offset, Parser.NodeSize(Offset));
    until ((Offset = Parser.FNodes.Offset) or (Parser.NodePtr(Offset)^.NodeType = ntToken));
    if (Offset = Parser.FNodes.Offset) then
      Result := nil
    else
      Result := PToken(Parser.NodePtr(Offset));
  until (not Assigned(Result) or (Result^.IsUsed));
end;

function TCustomSQLParser.TToken.GetOffset(): TOffset;
begin
  Result := Heritage.Heritage.GetOffset();
end;

function TCustomSQLParser.TToken.GetParentNode(): PNode;
begin
  Result := Heritage.GetParentNode();
end;

function TCustomSQLParser.TToken.GetText(): string;
begin
//  if (FText.NewText = '') then
    SetString(Result, FText.SQL, FText.Length)
//  else
//    Result := FText.NewText;
end;

procedure TCustomSQLParser.TToken.SetText(AText: string);
begin
//  FText.NewText := AText;
end;

{ TCustomSQLParser.TRange *****************************************************}

class function TCustomSQLParser.TRange.Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): TOffset;
begin
  Result := TNode.Create(AParser, ANodeType);
end;

function TCustomSQLParser.TRange.GetFirstToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FFirstToken));
end;

function TCustomSQLParser.TRange.GetLastToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FLastToken));
end;

function TCustomSQLParser.TRange.GetOffset(): TOffset;
begin
  Result := Heritage.Heritage.Offset;
end;

function TCustomSQLParser.TRange.GetParentNode(): PNode;
begin
  Result := PNode(Parser.NodePtr(FParentNode));
end;

procedure TCustomSQLParser.TRange.AddChild(const ChildNode: TOffset);
var
  Child: PChild;
begin
  if (ChildNode > 0) then
  begin
    Child := Parser.StmtNodePtr(ChildNode);
    Child^.FParentNode := Offset;
    if ((FFirstToken = 0) or (FFirstToken > Child^.FFirstToken) and (Child^.FFirstToken > 0)) then
      FFirstToken := Child^.FFirstToken;
    if ((FLastToken = 0) or (FLastToken < Child^.FLastToken) and (Child^.FLastToken > 0)) then
      FLastToken := Child^.FLastToken;
  end;
end;

{ TCustomSQLParser.TRoot ******************************************************}

class function TCustomSQLParser.TRoot.Create(const AParser: TCustomSQLParser): TOffset;
begin
  Result := TNode.Create(AParser, ntRoot);
end;

function TCustomSQLParser.TRoot.GetFirstStmt(): PStmt;
begin
  Result := PStmt(Parser.NodePtr(FFirstStmt));
end;

function TCustomSQLParser.TRoot.GetFirstToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FFirstToken));
end;

function TCustomSQLParser.TRoot.GetLastStmt(): PStmt;
begin
  Result := PStmt(Parser.NodePtr(FLastStmt));
end;

function TCustomSQLParser.TRoot.GetLastToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FLastToken));
end;

{ TCustomSQLParser.TStmt ******************************************************}

class function TCustomSQLParser.TStmt.Create(const AParser: TCustomSQLParser; const AStmtType: TStmtType): TOffset;
begin
  Result := TRange.Create(AParser, NodeTypeByStmtType[AStmtType]);

  with AParser.StmtPtr(Result)^ do
  begin
    FStmtType := AStmtType;
  end;
end;

function TCustomSQLParser.TStmt.GetError(): Boolean;
begin
  Result := FErrorCode <> PE_Success;
end;

function TCustomSQLParser.TStmt.GetErrorMessage(): string;
begin
  Result := Parser.GetErrorMessage(ErrorCode);
end;

function TCustomSQLParser.TStmt.GetErrorToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FErrorToken));
end;

function TCustomSQLParser.TStmt.GetFirstToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FFirstToken));
end;

function TCustomSQLParser.TStmt.GetLastToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FLastToken));
end;

function TCustomSQLParser.TStmt.GetNextStmt(): PStmt;
var
  Token: PToken;
  Node: PNode;
begin
  Token := LastToken^.NextToken;
  while (Assigned(Token) and not Token^.IsUsed) do
    Token := Token^.NextToken;
  if (Assigned(Token) and (Token^.TokenType = ttDelimiter)) then
    Token := Token^.NextToken;
  while (Assigned(Token) and not Token^.IsUsed) do
    Token := Token^.NextToken;

  if (not Assigned(Token)) then
    Result := nil
  else
  begin
    Node := Token^.ParentNode;
    while (Assigned(Node) and not Parser.IsStmt(Node)) do
      Node := PChild(Node)^.ParentNode;

    if (not Assigned(Node) or not Parser.IsStmt(Node)) then
      Result := nil
    else
      Result := PStmt(Node);
  end;
end;

{ TCustomSQLParser.TAlterDatabase *********************************************}

class function TCustomSQLParser.TAlterDatabaseStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterDatabase);

  with PAlterDatabaseStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.AlterTag);
    Heritage.Heritage.AddChild(ANodes.DatabaseTag);
    Heritage.Heritage.AddChild(ANodes.IdentTag);
    Heritage.Heritage.AddChild(ANodes.CharacterSetValue);
    Heritage.Heritage.AddChild(ANodes.CollateValue);
    Heritage.Heritage.AddChild(ANodes.UpgradeDataDirectoryNameTag);
  end;
end;

{ TCustomSQLParser.TAlterEvent ************************************************}

class function TCustomSQLParser.TAlterEventStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterEvent);

  with PAlterEventStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.AlterTag);
    Heritage.Heritage.AddChild(ANodes.DefinerNode);
    Heritage.Heritage.AddChild(ANodes.EventTag);
    Heritage.Heritage.AddChild(ANodes.EventIdent);
    Heritage.Heritage.AddChild(ANodes.OnScheduleValue);
    Heritage.Heritage.AddChild(ANodes.OnCompletitionTag);
    Heritage.Heritage.AddChild(ANodes.RenameValue);
    Heritage.Heritage.AddChild(ANodes.EnableTag);
    Heritage.Heritage.AddChild(ANodes.CommentValue);
    Heritage.Heritage.AddChild(ANodes.DoTag);
    Heritage.Heritage.AddChild(ANodes.Body);
  end;
end;

{ TCustomSQLParser.TAlterRoutine **********************************************}

class function TCustomSQLParser.TAlterRoutineStmt.Create(const AParser: TCustomSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset;
begin
  if (ARoutineType = rtFunction) then
    Result := TStmt.Create(AParser, stAlterFunction)
  else
    Result := TStmt.Create(AParser, stAlterProcedure);

  with PAlterRoutineStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.AlterTag);
    Heritage.Heritage.AddChild(ANodes.RoutineTag);
    Heritage.Heritage.AddChild(ANodes.IdentNode);
    Heritage.Heritage.AddChild(ANodes.CommentValue);
    Heritage.Heritage.AddChild(ANodes.CommentStringNode);
    Heritage.Heritage.AddChild(ANodes.LanguageTag);
    Heritage.Heritage.AddChild(ANodes.CharacteristicTag);
    Heritage.Heritage.AddChild(ANodes.SQLSecurityTag);
  end;
end;

{ TCustomSQLParser.TAlterServerStmt *******************************************}

class function TCustomSQLParser.TAlterServerStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterServer);

  with PAlterServerStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.AlterTag);
    Heritage.Heritage.AddChild(ANodes.ServerTag);
    Heritage.Heritage.AddChild(ANodes.IdentNode);
    Heritage.Heritage.AddChild(ANodes.OptionsTag);
    Heritage.Heritage.AddChild(ANodes.OptionsList);
  end;
end;

{ TCustomSQLParser.TAlterTableStmt.TAlterColumn *******************************}

class function TCustomSQLParser.TAlterTableStmt.TAlterColumn.Create(const AParser: TCustomSQLParser; const ANodes: TAlterColumn.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtAlterColumn);

  with PAlterColumn(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.AlterTag);
    Heritage.AddChild(ANodes.ColumnIdent);
    Heritage.AddChild(ANodes.SetDefaultValue);
    Heritage.AddChild(ANodes.DropDefaultTag);
  end;
end;

{ TCustomSQLParser.TAlterTableStmt.TConvertTo *********************************}

class function TCustomSQLParser.TAlterTableStmt.TConvertTo.Create(const AParser: TCustomSQLParser; const ANodes: TConvertTo.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtConvertTo);

  with PConvertTo(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.ConvertToTag);
    Heritage.AddChild(ANodes.CharacterSetTag);
    Heritage.AddChild(ANodes.CharacterSetNameToken);
    Heritage.AddChild(ANodes.CollateTag);
    Heritage.AddChild(ANodes.CollateNameToken);
  end;
end;

{ TCustomSQLParser.TAlterTableStmt.TDropObject ********************************}

class function TCustomSQLParser.TAlterTableStmt.TDropObject.Create(const AParser: TCustomSQLParser; const ANodes: TDropObject.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtDropObject);

  with PDropObject(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.DropTag);
    Heritage.AddChild(ANodes.ObjectTypeTag);
    Heritage.AddChild(ANodes.NameNode);
  end;
end;

{ TCustomSQLParser.TAlterTableStmt.TExchangePartition *************************}

class function TCustomSQLParser.TAlterTableStmt.TExchangePartition.Create(const AParser: TCustomSQLParser; const ANodes: TExchangePartition.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtExchangePartition);

  with PExchangePartition(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.ExchangePartitionTag);
    Heritage.AddChild(ANodes.PartitionIdent);
    Heritage.AddChild(ANodes.WithTableTag);
    Heritage.AddChild(ANodes.TableIdent);
  end;
end;

{ TCustomSQLParser.TAlterTableStmt.TReorganizePartition *************************}

class function TCustomSQLParser.TAlterTableStmt.TReorganizePartition.Create(const AParser: TCustomSQLParser; const ANodes: TReorganizePartition.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtReorganizePartition);

  with PReorganizePartition(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.ReorganizePartitionTag);
    Heritage.AddChild(ANodes.PartitionIdentList);
    Heritage.AddChild(ANodes.IntoTag);
    Heritage.AddChild(ANodes.PartitionList);
  end;
end;

{ TCustomSQLParser.TAlterTableStmt ********************************************}

class function TCustomSQLParser.TAlterTableStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterTable);

  with PAlterTableStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.AlterTag);
    Heritage.Heritage.AddChild(ANodes.TableTag);
    Heritage.Heritage.AddChild(ANodes.IdentNode);
    Heritage.Heritage.AddChild(ANodes.SpecificationList);
  end;
end;

{ TCustomSQLParser.TCreateViewStmt ********************************************}

class function TCustomSQLParser.TAlterViewStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterView);

  with PAlterViewStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.AlterTag);
    Heritage.Heritage.AddChild(ANodes.AlgorithmValue);
    Heritage.Heritage.AddChild(ANodes.DefinerNode);
    Heritage.Heritage.AddChild(ANodes.SQLSecurityTag);
    Heritage.Heritage.AddChild(ANodes.ViewTag);
    Heritage.Heritage.AddChild(ANodes.IdentNode);
    Heritage.Heritage.AddChild(ANodes.Columns);
    Heritage.Heritage.AddChild(ANodes.AsTag);
    Heritage.Heritage.AddChild(ANodes.SelectStmt);
    Heritage.Heritage.AddChild(ANodes.OptionTag);
  end;
end;

{ TCustomSQLParser.TBinaryOp **************************************************}

class function TCustomSQLParser.TBinaryOp.Create(const AParser: TCustomSQLParser; const AOperator, AOperand1, AOperand2: TOffset): TOffset;
begin
  Result := TRange.Create(AParser, ntBinaryOp);

  with PBinaryOp(AParser.NodePtr(Result))^ do
  begin
    FNodes.Operator := AOperator;
    FNodes.Operand1 := AOperand1;
    FNodes.Operand2 := AOperand2;

    Heritage.AddChild(AOperator);
    Heritage.AddChild(AOperand1);
    Heritage.AddChild(AOperand2);
  end;
end;

function TCustomSQLParser.TBinaryOp.GetOperand1(): PChild;
begin
  Result := Parser.StmtNodePtr(FNodes.Operand1);
end;

function TCustomSQLParser.TBinaryOp.GetOperand2(): PChild;
begin
  Result := Parser.StmtNodePtr(FNodes.Operand2);
end;

function TCustomSQLParser.TBinaryOp.GetOperator(): PChild;
begin
  Result := Parser.StmtNodePtr(FNodes.Operator);
end;

{ TCustomSQLParser.TBetweenOp *************************************************}

class function TCustomSQLParser.TBetweenOp.Create(const AParser: TCustomSQLParser; const AOperator1, AOperator2, AExpr, AMin, AMax: TOffset): TOffset;
begin
  Result := TRange.Create(AParser, ntBetweenOp);

  with PBetweenOp(AParser.NodePtr(Result))^ do
  begin
    FNodes.Operator1 := AOperator1;
    FNodes.Operator2 := AOperator2;
    FNodes.Expr := AExpr;
    FNodes.Min := AMin;
    FNodes.Max := AMax;

    Heritage.AddChild(AOperator1);
    Heritage.AddChild(AOperator2);
    Heritage.AddChild(AExpr);
    Heritage.AddChild(AMin);
    Heritage.AddChild(AMax);
  end;
end;

{ TCustomSQLParser.TCallStmt **************************************************}

class function TCustomSQLParser.TCallStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCallStmt);

  with PCallStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.CallTag);
    Heritage.Heritage.AddChild(ANodes.ProcedureIdent);
    Heritage.Heritage.AddChild(ANodes.ParamList);
  end;
end;

{ TCustomSQLParser.TCaseOp.TBranch ********************************************}

class function TCustomSQLParser.TCaseOp.TBranch.Create(const AParser: TCustomSQLParser; const ANodes: TBranch.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCaseOpBranch);

  with PBranch(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.Tag);
    Heritage.AddChild(ANodes.ConditionExpr);
    Heritage.AddChild(ANodes.ThenTag);
    Heritage.AddChild(ANodes.ResultExpr);
  end;
end;

{ TCustomSQLParser.TCaseOp ****************************************************}

class function TCustomSQLParser.TCaseOp.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCaseOp);

  with PCaseOp(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.CaseTag);
    Heritage.AddChild(ANodes.CompareExpr);
    Heritage.AddChild(ANodes.BranchList);
    Heritage.AddChild(ANodes.EndTag);
  end;
end;

{ TCustomSQLParser.TCaseStmt.TBranch ********************************************}

class function TCustomSQLParser.TCaseStmt.TBranch.Create(const AParser: TCustomSQLParser; const ANodes: TBranch.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCaseStmtBranch);

  with PBranch(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.Tag);
    Heritage.AddChild(ANodes.ConditionExpr);
    Heritage.AddChild(ANodes.ThenTag);
    Heritage.AddChild(ANodes.StmtList);
  end;
end;

{ TCustomSQLParser.TCaseStmt ****************************************************}

class function TCustomSQLParser.TCaseStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCaseStmt);

  with PCaseStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.BranchList);
    Heritage.AddChild(ANodes.EndTag);
  end;
end;

{ TCustomSQLParser.TCompoundStmt **********************************************}

class function TCustomSQLParser.TCompoundStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCompound);

  with PCompoundStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.BeginLabelToken);
    Heritage.Heritage.AddChild(ANodes.BeginTag);
    Heritage.Heritage.AddChild(ANodes.StmtList);
    Heritage.Heritage.AddChild(ANodes.EndTag);
    Heritage.Heritage.AddChild(ANodes.EndLabelToken);
  end;
end;

{ TCustomSQLParser.TColumn ****************************************************}

class function TCustomSQLParser.TColumn.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntColumn);

  with PColumn(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.AddTag);
    Heritage.AddChild(ANodes.ColumnTag);
    Heritage.AddChild(ANodes.OldNameIdent);
    Heritage.AddChild(ANodes.NameIdent);
    Heritage.AddChild(ANodes.DataTypeNode);
    Heritage.AddChild(ANodes.Null);
    Heritage.AddChild(ANodes.DefaultValue);
    Heritage.AddChild(ANodes.AutoIncrementTag);
    Heritage.AddChild(ANodes.KeyTag);
    Heritage.AddChild(ANodes.CommentValue);
    Heritage.AddChild(ANodes.ColumnFormat);
    Heritage.AddChild(ANodes.ReferencesNode);
    Heritage.AddChild(ANodes.Position);
  end;
end;

{ TCustomSQLParser.TCreateDatabaseStmt ****************************************}

class function TCustomSQLParser.TCreateDatabaseStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateDatabase);

  with PCreateDatabaseStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.CreateTag);
    Heritage.Heritage.AddChild(ANodes.DatabaseTag);
    Heritage.Heritage.AddChild(ANodes.IfNotExistsTag);
    Heritage.Heritage.AddChild(ANodes.DatabaseIdent);
    Heritage.Heritage.AddChild(ANodes.CharacterSetValue);
    Heritage.Heritage.AddChild(ANodes.CollateValue);
  end;
end;

{ TCustomSQLParser.TCreateEventStmt ****************************************}

class function TCustomSQLParser.TCreateEventStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateEvent);

  with PCreateEventStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.CreateTag);
    Heritage.Heritage.AddChild(ANodes.DefinerNode);
    Heritage.Heritage.AddChild(ANodes.EventTag);
    Heritage.Heritage.AddChild(ANodes.IfNotExistsTag);
    Heritage.Heritage.AddChild(ANodes.EventIdent);
    Heritage.Heritage.AddChild(ANodes.OnScheduleValue);
    Heritage.Heritage.AddChild(ANodes.OnCompletitionTag);
    Heritage.Heritage.AddChild(ANodes.EnableTag);
    Heritage.Heritage.AddChild(ANodes.CommentValue);
    Heritage.Heritage.AddChild(ANodes.DoTag);
    Heritage.Heritage.AddChild(ANodes.Body);
  end;
end;

class function TCustomSQLParser.TCreateIndexStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateIndex);

  with PCreateIndexStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.CreateTag);
    Heritage.Heritage.AddChild(ANodes.IndexTag);
    Heritage.Heritage.AddChild(ANodes.IndexIdent);
    Heritage.Heritage.AddChild(ANodes.OnTag);
    Heritage.Heritage.AddChild(ANodes.TableIdent);
    Heritage.Heritage.AddChild(ANodes.IndexTypeValue);
    Heritage.Heritage.AddChild(ANodes.ColumnNameList);
    Heritage.Heritage.AddChild(ANodes.AlgorithmValue);
    Heritage.Heritage.AddChild(ANodes.CommentValue);
    Heritage.Heritage.AddChild(ANodes.KeyBlockSizeValue);
    Heritage.Heritage.AddChild(ANodes.LockValue);
    Heritage.Heritage.AddChild(ANodes.ParserValue);
  end;
end;

{ TCustomSQLParser.TCreateRoutineStmt *****************************************}

class function TCustomSQLParser.TCreateRoutineStmt.Create(const AParser: TCustomSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset;
begin
  if (ARoutineType = rtFunction) then
    Result := TStmt.Create(AParser, stCreateFunction)
  else
    Result := TStmt.Create(AParser, stCreateProcedure);

  with PCreateRoutineStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.CreateTag);
    Heritage.Heritage.AddChild(ANodes.DefinerNode);
    Heritage.Heritage.AddChild(ANodes.RoutineTag);
    Heritage.Heritage.AddChild(ANodes.IdentNode);
    Heritage.Heritage.AddChild(ANodes.ParameterNode);
    Heritage.Heritage.AddChild(ANodes.Return.ReturnsTag);
    Heritage.Heritage.AddChild(ANodes.Return.DataTypeNode);
    Heritage.Heritage.AddChild(ANodes.Body);
  end;
end;

{ TCustomSQLParser.TCreateTableStmt.TReference ********************************}

class function TCustomSQLParser.TCreateTableStmt.TReference.Create(const AParser: TCustomSQLParser; const ANodes: TReference.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtReference);

  with TCreateTableStmt.PReference(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.ReferencesTag);
    Heritage.AddChild(ANodes.TableIdent);
    Heritage.AddChild(ANodes.IndicesList);
    Heritage.AddChild(ANodes.MatchValue);
    Heritage.AddChild(ANodes.OnDeleteValue);
    Heritage.AddChild(ANodes.OnUpdateValue);
  end;
end;

{ TCustomSQLParser.TCreateServerStmt ******************************************}

class function TCustomSQLParser.TCreateServerStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateServer);

  with PCreateServerStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.CreateTag);
    Heritage.Heritage.AddChild(ANodes.ServerTag);
    Heritage.Heritage.AddChild(ANodes.ServerIdent);
    Heritage.Heritage.AddChild(ANodes.ForeignDataWrapperValue);
    Heritage.Heritage.AddChild(ANodes.OptionsTag);
    Heritage.Heritage.AddChild(ANodes.OptionsList);
  end;
end;

{ TCustomSQLParser.TCreateTableStmt *****************************************}

class function TCustomSQLParser.TCreateTableStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateTable);

  with PCreateTableStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.CreateTag);
    Heritage.Heritage.AddChild(ANodes.TemporaryTag);
    Heritage.Heritage.AddChild(ANodes.TableTag);
    Heritage.Heritage.AddChild(ANodes.IfNotExistsTag);
    Heritage.Heritage.AddChild(ANodes.TableIdent);
    Heritage.Heritage.AddChild(ANodes.OpenBracketToken);
    Heritage.Heritage.AddChild(ANodes.DefinitionList);
    Heritage.Heritage.AddChild(ANodes.TableOptionList);
    Heritage.Heritage.AddChild(ANodes.LikeTag);
    Heritage.Heritage.AddChild(ANodes.LikeTableIdent);
    Heritage.Heritage.AddChild(ANodes.CloseBracketToken);
  end;
end;

{ TCustomSQLParser.TCreateTriggerStmt *****************************************}

class function TCustomSQLParser.TCreateTriggerStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateTrigger);

  with PCreateTriggerStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.CreateTag);
    Heritage.Heritage.AddChild(ANodes.DefinerNode);
    Heritage.Heritage.AddChild(ANodes.TriggerTag);
    Heritage.Heritage.AddChild(ANodes.TriggerIdent);
    Heritage.Heritage.AddChild(ANodes.ActionValue);
    Heritage.Heritage.AddChild(ANodes.OnTag);
    Heritage.Heritage.AddChild(ANodes.TableIdentNode);
    Heritage.Heritage.AddChild(ANodes.ForEachRowTag);
    Heritage.Heritage.AddChild(ANodes.Body);
  end;
end;

{ TCustomSQLParser.TCreateViewStmt ********************************************}

class function TCustomSQLParser.TCreateViewStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateView);

  with PCreateViewStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.CreateTag);
    Heritage.Heritage.AddChild(ANodes.OrReplaceTag);
    Heritage.Heritage.AddChild(ANodes.AlgorithmValue);
    Heritage.Heritage.AddChild(ANodes.DefinerNode);
    Heritage.Heritage.AddChild(ANodes.SQLSecurityTag);
    Heritage.Heritage.AddChild(ANodes.ViewTag);
    Heritage.Heritage.AddChild(ANodes.IdentNode);
    Heritage.Heritage.AddChild(ANodes.Columns);
    Heritage.Heritage.AddChild(ANodes.AsTag);
    Heritage.Heritage.AddChild(ANodes.SelectStmt);
    Heritage.Heritage.AddChild(ANodes.OptionTag);
  end;
end;

{ TCustomSQLParser.TDataType **************************************************}

class function TCustomSQLParser.TDataType.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntDataType);

  with PDataType(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.IdentToken);
    Heritage.AddChild(ANodes.IdentToken);
    Heritage.AddChild(ANodes.OpenBracketToken);
    Heritage.AddChild(ANodes.CloseBracketToken);
    Heritage.AddChild(ANodes.LengthToken);
    Heritage.AddChild(ANodes.CommaToken);
    Heritage.AddChild(ANodes.DecimalsToken);
    Heritage.AddChild(ANodes.StringValuesNode);
    Heritage.AddChild(ANodes.UnsignedTag);
    Heritage.AddChild(ANodes.ZerofillTag);
    Heritage.AddChild(ANodes.CharacterSetTag);
    Heritage.AddChild(ANodes.CharacterSetValueToken);
    Heritage.AddChild(ANodes.CollateTag);
    Heritage.AddChild(ANodes.CollateValueToken);
    Heritage.AddChild(ANodes.BinaryTag);
  end;
end;

{ TCustomSQLParser.TDbIdent **********************************************}

procedure TCustomSQLParser.TDbIdent.AddPrefix(const APrefix, ADot: TOffset);
var
  Node: PNode;
begin
  Assert(Parser.NodePtr(APrefix)^.NodeType = ntDbIdent);
  Assert(Parser.TokenPtr(ADot)^.OperatorType = otDot);

  FNodes.Prefix1 := PDbIdent(Parser.NodePtr(APrefix))^.FNodes.Ident;
  FNodes.Prefix2 := PDbIdent(Parser.NodePtr(APrefix))^.FNodes.Prefix1;

  Heritage.AddChild(FNodes.Prefix1);
  Heritage.AddChild(FNodes.Prefix2);
  Heritage.AddChild(ADot);

  Parser.DeleteNode(Parser.NodePtr(APrefix));

  Node := ParentNode;
  while (Parser.IsRange(Node)) do
  begin
    if (PRange(Node)^.FFirstToken > Heritage.FFirstToken) then
      PRange(Node)^.FFirstToken := Heritage.FFirstToken;
    Node := PRange(ParentNode)^.ParentNode;
  end;
end;

class function TCustomSQLParser.TDbIdent.Create(const AParser: TCustomSQLParser; const AIdent: TOffset; const ADbIdentType: TDbIdentType = ditUnknown): TOffset;
begin
  Result := TRange.Create(AParser, ntDbIdent);

  with PDbIdent(AParser.NodePtr(Result))^ do
  begin
    FNodes.Ident := AIdent;
    FDbIdentType := ADbIdentType;

    FNodes.Prefix1 := 0;
    FNodes.Prefix2 := 0;

    Heritage.AddChild(AIdent);
  end;
end;

function TCustomSQLParser.TDbIdent.GetIdent(): PToken;
begin
  Result := Parser.TokenPtr(FNodes.Ident);
end;

function TCustomSQLParser.TDbIdent.GetLastToken(): PToken;
begin
  Result := Heritage.GetLastToken();
end;

function TCustomSQLParser.TDbIdent.GetParentNode(): PNode;
begin
  Result := Heritage.GetParentNode();
end;

function TCustomSQLParser.TDbIdent.GetPrefix1(): PToken;
begin
  if (FNodes.Prefix1 = 0) then
    Result := nil
  else
    Result := Parser.TokenPtr(FNodes.Prefix1);
end;

function TCustomSQLParser.TDbIdent.GetPrefix2(): PToken;
begin
  if (FNodes.Prefix2 = 0) then
    Result := nil
  else
    Result := Parser.TokenPtr(FNodes.Prefix2);
end;

{ TCustomSQLParser.TDeleteStmt ************************************************}

class function TCustomSQLParser.TDeleteStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDelete);

  with PDeleteStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.DeleteTag);
    Heritage.Heritage.AddChild(ANodes.LowPriorityTag);
    Heritage.Heritage.AddChild(ANodes.QuickTag);
    Heritage.Heritage.AddChild(ANodes.IgnoreTag);
    Heritage.Heritage.AddChild(ANodes.FromTag);
    Heritage.Heritage.AddChild(ANodes.TableIdent);
    Heritage.Heritage.AddChild(ANodes.PartitionTag);
    Heritage.Heritage.AddChild(ANodes.PartitionList);
    Heritage.Heritage.AddChild(ANodes.WhereValue);
    Heritage.Heritage.AddChild(ANodes.OrderByValue);
    Heritage.Heritage.AddChild(ANodes.LimitValue);
  end;
end;

{ TCustomSQLParser.TDoStmt ****************************************************}

class function TCustomSQLParser.TDoStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDo);

  with PDoStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.DoTag);
    Heritage.Heritage.AddChild(ANodes.ExprList);
  end;
end;

{ TCustomSQLParser.TDropDatabaseStmt ******************************************}

class function TCustomSQLParser.TDropDatabaseStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropDatabase);

  with PDropDatabaseStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.DropTag);
    Heritage.Heritage.AddChild(ANodes.DatabaseTag);
    Heritage.Heritage.AddChild(ANodes.IfExistsTag);
    Heritage.Heritage.AddChild(ANodes.DatabaseIdent);
  end;
end;

{ TCustomSQLParser.TDropEventStmt *********************************************}

class function TCustomSQLParser.TDropEventStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropEvent);

  with PDropEventStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.DropTag);
    Heritage.Heritage.AddChild(ANodes.EventTag);
    Heritage.Heritage.AddChild(ANodes.IfExistsTag);
    Heritage.Heritage.AddChild(ANodes.EventIdent);
  end;
end;

{ TCustomSQLParser.TDropIndexStmt *********************************************}

class function TCustomSQLParser.TDropIndexStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropIndex);

  with PDropIndexStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.DropTag);
    Heritage.Heritage.AddChild(ANodes.IndexTag);
    Heritage.Heritage.AddChild(ANodes.IndexIdent);
    Heritage.Heritage.AddChild(ANodes.OnTag);
    Heritage.Heritage.AddChild(ANodes.TableIdent);
    Heritage.Heritage.AddChild(ANodes.AlgorithmValue);
    Heritage.Heritage.AddChild(ANodes.LockValue);
  end;
end;

{ TCustomSQLParser.TDropRoutineStmt *********************************************}

class function TCustomSQLParser.TDropRoutineStmt.Create(const AParser: TCustomSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset;
begin
  if (ARoutineType = rtFunction) then
    Result := TStmt.Create(AParser, stDropFunction)
  else
    Result := TStmt.Create(AParser, stDropProcedure);

  with PDropRoutineStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.DropTag);
    Heritage.Heritage.AddChild(ANodes.RoutineTag);
    Heritage.Heritage.AddChild(ANodes.IfExistsTag);
    Heritage.Heritage.AddChild(ANodes.RoutineIdent);
  end;
end;

{ TCustomSQLParser.TDropServerStmt *********************************************}

class function TCustomSQLParser.TDropServerStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropServer);

  with PDropServerStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.DropTag);
    Heritage.Heritage.AddChild(ANodes.ServerTag);
    Heritage.Heritage.AddChild(ANodes.IfExistsTag);
    Heritage.Heritage.AddChild(ANodes.ServerIdent);
  end;
end;

{ TCustomSQLParser.TDropTableStmt *********************************************}

class function TCustomSQLParser.TDropTableStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropTable);

  with PDropTableStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.DropTag);
    Heritage.Heritage.AddChild(ANodes.TemporaryTag);
    Heritage.Heritage.AddChild(ANodes.TableTag);
    Heritage.Heritage.AddChild(ANodes.IfExistsTag);
    Heritage.Heritage.AddChild(ANodes.TableIdentList);
    Heritage.Heritage.AddChild(ANodes.RestrictCascadeTag);
  end;
end;

{ TCustomSQLParser.TDropTriggerStmt *******************************************}

class function TCustomSQLParser.TDropTriggerStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropTrigger);

  with PDropTriggerStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.DropTag);
    Heritage.Heritage.AddChild(ANodes.TriggerTag);
    Heritage.Heritage.AddChild(ANodes.IfExistsTag);
    Heritage.Heritage.AddChild(ANodes.TriggerIdent);
  end;
end;

{ TCustomSQLParser.TDropViewStmt **********************************************}

class function TCustomSQLParser.TDropViewStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropView);

  with PDropViewStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.DropTag);
    Heritage.Heritage.AddChild(ANodes.TemporaryTag);
    Heritage.Heritage.AddChild(ANodes.ViewTag);
    Heritage.Heritage.AddChild(ANodes.IfExistsTag);
    Heritage.Heritage.AddChild(ANodes.ViewIdentList);
    Heritage.Heritage.AddChild(ANodes.RestrictCascadeTag);
  end;
end;

{ TCustomSQLParser.TForeignKey ************************************************}

class function TCustomSQLParser.TForeignKey.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntForeignKey);

  with PForeignKey(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.AddTag);
    Heritage.AddChild(ANodes.ConstraintTag);
    Heritage.AddChild(ANodes.SymbolIdent);
    Heritage.AddChild(ANodes.KeyTag);
    Heritage.AddChild(ANodes.NameToken);
    Heritage.AddChild(ANodes.ColumnNameList);
  end;
end;

{ TCustomSQLParser.TFunction **************************************************}

class function TCustomSQLParser.TFunction.Create(const AParser: TCustomSQLParser; const AIdent, AArguments: TOffset): TOffset;
var
  Token: PToken;
begin
  Result := TRange.Create(AParser, ntFunction);

  with PFunction(AParser.NodePtr(Result))^ do
  begin
    FNodes.Ident := AIdent;
    FNodes.Arguments := AArguments;

    Heritage.AddChild(AIdent);
    Heritage.AddChild(AArguments);

    Token := Ident^.LastToken^.NextToken;
    while (Assigned(Token) and not Token^.IsUsed) do
      Token := Token^.NextToken;
    if (Assigned(Token) and (Token^.TokenType = ttOpenBracket)) then
      Heritage.AddChild(Token^.Offset);

    Token := Arguments^.LastToken^.NextToken;
    while (Assigned(Token) and not Token^.IsUsed) do
      Token := Token^.NextToken;
    if (Assigned(Token) and (Token^.TokenType = ttCloseBracket)) then
      Heritage.AddChild(Token^.Offset);
  end;
end;

function TCustomSQLParser.TFunction.GetArguments(): PChild;
begin
  Result := Parser.StmtNodePtr(FNodes.Arguments);
end;

function TCustomSQLParser.TFunction.GetIdent(): PChild;
begin
  Result := Parser.StmtNodePtr(FNodes.Ident);
end;

{ TCustomSQLParser.TIfStmt.TBranch ********************************************}

class function TCustomSQLParser.TIfStmt.TBranch.Create(const AParser: TCustomSQLParser; const ANodes: TBranch.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntIfStmtBranch);

  with PBranch(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.Tag);
    Heritage.AddChild(ANodes.SearchConditionExpr);
    Heritage.AddChild(ANodes.ThenTag);
    Heritage.AddChild(ANodes.StmtList);
  end;
end;

{ TCustomSQLParser.TIfStmt ****************************************************}

class function TCustomSQLParser.TIfStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stIf);

  with PIfStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.BranchList);
    Heritage.Heritage.AddChild(ANodes.EndTag);
  end;
end;

{ TCustomSQLParser.TItndex ****************************************************}

class function TCustomSQLParser.TIndex.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntIndex);

  with PIndex(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.AddTag);
    Heritage.AddChild(ANodes.ConstraintTag);
    Heritage.AddChild(ANodes.SymbolIdent);
    Heritage.AddChild(ANodes.IndexTag);
    Heritage.AddChild(ANodes.IndexIdent);
    Heritage.AddChild(ANodes.IndexTypeValue);
    Heritage.AddChild(ANodes.ColumnNameList);
    Heritage.AddChild(ANodes.CommentValue);
    Heritage.AddChild(ANodes.KeyBlockSizeValue);
    Heritage.AddChild(ANodes.ParserValue);
  end;
end;

{ TCustomSQLParser.TKeyColumn *************************************************}

class function TCustomSQLParser.TIndexColumn.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntIndexColumn);

  with PIndexColumn(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.IdentTag);
    Heritage.AddChild(ANodes.OpenBracketToken);
    Heritage.AddChild(ANodes.LengthToken);
    Heritage.AddChild(ANodes.CloseBracketToken);
    Heritage.AddChild(ANodes.SortTag);
  end;
end;

{ TCustomSQLParser.TInsertStmt ***********************************************}

class function TCustomSQLParser.TInsertStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stInsert);

  with PInsertStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.InsertTag);
    Heritage.Heritage.AddChild(ANodes.PriorityTag);
    Heritage.Heritage.AddChild(ANodes.IgnoreTag);
    Heritage.Heritage.AddChild(ANodes.IntoTag);
    Heritage.Heritage.AddChild(ANodes.TableIdent);
    Heritage.Heritage.AddChild(ANodes.PartitionTag);
    Heritage.Heritage.AddChild(ANodes.PartitionList);
    Heritage.Heritage.AddChild(ANodes.ColumnList);
    Heritage.Heritage.AddChild(ANodes.ValuesTag);
    Heritage.Heritage.AddChild(ANodes.SetTag);
    Heritage.Heritage.AddChild(ANodes.SelectTag);
    Heritage.Heritage.AddChild(ANodes.ValuesList);
    Heritage.Heritage.AddChild(ANodes.SelectStmt);
    Heritage.Heritage.AddChild(ANodes.OnDuplicateKeyUpdateTag);
    Heritage.Heritage.AddChild(ANodes.UpdateList);
  end;
end;

{ TCustomSQLParser.TIterateStmt ***********************************************}

class function TCustomSQLParser.TIterateStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntIterateStmt);

  with PIterateStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.IterateToken);
    Heritage.AddChild(ANodes.LabelToken);
  end;
end;

{ TCustomSQLParser.TLeaveStmt *************************************************}

class function TCustomSQLParser.TLeaveStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntLeaveStmt);

  with PLeaveStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.LeaveToken);
    Heritage.AddChild(ANodes.LabelToken);
  end;
end;

{ TCustomSQLParser.TList ******************************************************}

class function TCustomSQLParser.TList.Create(const AParser: TCustomSQLParser; const ANodes: TNodes; const AChildrenCount: Integer; const AChildrens: array of TOffset): TOffset;
var
  I: Integer;
begin
  Result := TRange.Create(AParser, ntList);

  with PList(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    if (AChildrenCount > 0) then
      FNodes.FirstChild := Integer(AChildrens[0]);

    Heritage.AddChild(ANodes.OpenBracket);
    for I := 0 to AChildrenCount - 1 do
      Heritage.AddChild(Integer(AChildrens[I]));
    Heritage.AddChild(ANodes.CloseBracket);
  end;
end;

function TCustomSQLParser.TList.GetFirstChild(): PChild;
begin
  Result := PChild(Parser.NodePtr(FNodes.FirstChild));
end;

{ TCustomSQLParser.TLoopStmt **************************************************}

class function TCustomSQLParser.TLoopStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stLoop);

  with PLoopStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.BeginLabelToken);
    Heritage.Heritage.AddChild(ANodes.BeginTag);
    Heritage.Heritage.AddChild(ANodes.StmtList);
    Heritage.Heritage.AddChild(ANodes.EndTag);
    Heritage.Heritage.AddChild(ANodes.EndLabelToken);
  end;
end;

{ TCustomSQLParser.TRenameTableStmt ************************************************}

class function TCustomSQLParser.TRenameTableStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRenameTable);

  with PRenameTableStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.RenameTag);
    Heritage.Heritage.AddChild(ANodes.RenameList);
  end;
end;

{ TCustomSQLParser.TRenameTableStmt ************************************************}

class function TCustomSQLParser.TRenameTableStmtPair.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntRenameTableStmtPair);

  with PRenameTableStmtPair(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.OrgTableIdent);
    Heritage.AddChild(ANodes.ToTag);
    Heritage.AddChild(ANodes.NewTableIdent);
  end;
end;

{ TCustomSQLParser.TRepeatStmt ************************************************}

class function TCustomSQLParser.TRepeatStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRepeat);

  with PRepeatStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.BeginLabelToken);
    Heritage.Heritage.AddChild(ANodes.RepeatTag);
    Heritage.Heritage.AddChild(ANodes.StmtList);
    Heritage.Heritage.AddChild(ANodes.UntilTag);
    Heritage.Heritage.AddChild(ANodes.SearchConditionExpr);
    Heritage.Heritage.AddChild(ANodes.EndTag);
    Heritage.Heritage.AddChild(ANodes.EndLabelToken);
  end;
end;

{ TCustomSQLParser.TPartition *************************************************}

class function TCustomSQLParser.TPartition.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntPartition);

  with PPartition(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.AddTag);
    Heritage.AddChild(ANodes.PartitionTag);
    Heritage.AddChild(ANodes.NameIdent);
    Heritage.AddChild(ANodes.ValuesNode);
    Heritage.AddChild(ANodes.EngineValue);
    Heritage.AddChild(ANodes.CommentValue);
    Heritage.AddChild(ANodes.DataDirectoryValue);
    Heritage.AddChild(ANodes.IndexDirectoryValue);
    Heritage.AddChild(ANodes.MaxRowsValue);
    Heritage.AddChild(ANodes.MinRowsValue);
    Heritage.AddChild(ANodes.SubPartitionList);
  end;
end;

{ TCustomSQLParser.TPartitionValues *******************************************}

class function TCustomSQLParser.TPartitionValues.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntPartition);

  with PPartitionValues(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.ValuesTag);
    Heritage.AddChild(ANodes.DescriptionValue);
  end;
end;

{ TCustomSQLParser.TRoutineParam **********************************************}

class function TCustomSQLParser.TRoutineParam.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntFunctionParam);

  with PRoutineParam(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.IdentToken);
    Heritage.AddChild(ANodes.DataTypeNode);
  end;
end;

{ TCustomSQLParser.TSelectStmt.TColumn ****************************************}

class function TCustomSQLParser.TSelectStmt.TField.Create(const AParser: TCustomSQLParser; const AValue, AAsToken, AAlias: TOffset): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtColumn);

  with PField(AParser.NodePtr(Result))^ do
  begin
    FExpr := AValue;
    FAsToken := AAsToken;
    FAlias := AAlias;

    Heritage.AddChild(AValue);
    Heritage.AddChild(AAsToken);
    Heritage.AddChild(AAlias);
  end;
end;

{ TCustomSQLParser.TSelectStmt.TTable.TIndexHint ******************************}

class function TCustomSQLParser.TSelectStmt.TTableFactor.TIndexHint.Create(const AParser: TCustomSQLParser; const AIndexHintType: TIndexHintType; const AIndexHintKind: TIndexHintKind): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableIndexHint);

  with TSelectStmt.TTableFactor.PIndexHint(AParser.NodePtr(Result))^ do
  begin
    FIndexHintType := AIndexHintType;
    FIndexHintKind := AIndexHintKind;
  end;
end;

{ TCustomSQLParser.TSelectStmt.TTableFactor ***********************************}

class function TCustomSQLParser.TSelectStmt.TTableFactor.Create(const AParser: TCustomSQLParser; const ANodes: TTableFactor.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactor);

  with TSelectStmt.PTableFactor(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.TableIdent);
    Heritage.AddChild(ANodes.PartitionTag);
    Heritage.AddChild(ANodes.Partitions);
    Heritage.AddChild(ANodes.AsToken);
    Heritage.AddChild(ANodes.AliasToken);
    Heritage.AddChild(ANodes.IndexHints);
    Heritage.AddChild(ANodes.SelectStmt);
  end;
end;

{ TCustomSQLParser.TSelectStmt.TTableFactorOj ***********************************}

class function TCustomSQLParser.TSelectStmt.TTableFactorOj.Create(const AParser: TCustomSQLParser; const ANodes: TTableFactorOj.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactorOj);

  with TSelectStmt.PTableFactorOj(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.OpenBracketToken);
    Heritage.AddChild(ANodes.OjTag);
    Heritage.AddChild(ANodes.LeftTableReference);
    Heritage.AddChild(ANodes.LeftOuterJoinTag);
    Heritage.AddChild(ANodes.RightTableReference);
    Heritage.AddChild(ANodes.OnTag);
    Heritage.AddChild(ANodes.CondExpr);
    Heritage.AddChild(ANodes.CloseBracketToken);
  end;
end;

{ TCustomSQLParser.TSelectStmt.TTableFactorReferences *************************}

class function TCustomSQLParser.TSelectStmt.TTableFactorReferences.Create(const AParser: TCustomSQLParser; const ANodes: TTableFactorReferences.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactor);

  with TSelectStmt.PTableFactorReferences(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.ReferenceList);
  end;
end;

{ TCustomSQLParser.TSelectStmt.TTableFactorSelect *************************}

class function TCustomSQLParser.TSelectStmt.TTableFactorSelect.Create(const AParser: TCustomSQLParser; const ANodes: TTableFactorSelect.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactor);

  with TSelectStmt.PTableFactorSelect(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.SelectStmt);
  end;
end;

{ TCustomSQLParser.TSelectStmt.TJoin ******************************************}

class function TCustomSQLParser.TSelectStmt.TJoin.Create(const AParser: TCustomSQLParser; const ALeftTable: TOffset; const AJoinType: TJoinType; const ARightTable: TOffset; const ACondition: TOffset; const AKeywordTokens: TKeywordTokens): TOffset;
var
  I: Integer;
begin
  Result := TRange.Create(AParser, ntSelectStmtJoin);

  with PJoin(AParser.NodePtr(Result))^ do
  begin
    FLeftTable := ALeftTable;
    FJoinType := AJoinType;
    FRightTable := ARightTable;
    FCondition := ACondition;

    Heritage.AddChild(ALeftTable);
    Heritage.AddChild(ARightTable);
    Heritage.AddChild(ACondition);
    for I := 0 to Length(AKeywordTokens) - 1 do
      Heritage.AddChild(AKeywordTokens[I]);
  end;
end;

{ TCustomSQLParser.TSelectStmt.TGroup *****************************************}

class function TCustomSQLParser.TSelectStmt.TGroup.Create(const AParser: TCustomSQLParser; const AExpr, ADirection: TOffset): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtGroup);

  with PGroup(AParser.NodePtr(Result))^ do
  begin
    FExpr := AExpr;
    FDirection := ADirection;

    Heritage.AddChild(AExpr);
    Heritage.AddChild(ADirection);
  end;
end;

{ TCustomSQLParser.TSelectStmt.TOrder *****************************************}

class function TCustomSQLParser.TSelectStmt.TOrderBy.Create(const AParser: TCustomSQLParser; const AExpr, ADirection: TOffset): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtOrderBy);

  with PGroup(AParser.NodePtr(Result))^ do
  begin
    FExpr := AExpr;
    FDirection := ADirection;

    Heritage.AddChild(AExpr);
    Heritage.AddChild(ADirection);
  end;
end;

{ TCustomSQLParser.TSelectStmt ************************************************}

class function TCustomSQLParser.TSelectStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSelect);

  with PSelectStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.SelectTag);
    Heritage.Heritage.AddChild(ANodes.DistinctTag);
    Heritage.Heritage.AddChild(ANodes.ColumnsNode);
    Heritage.Heritage.AddChild(ANodes.FromTag);
    Heritage.Heritage.AddChild(ANodes.TablesNodes);
    Heritage.Heritage.AddChild(ANodes.WhereTag);
    Heritage.Heritage.AddChild(ANodes.WhereNode);
    Heritage.Heritage.AddChild(ANodes.GroupByTag);
    Heritage.Heritage.AddChild(ANodes.GroupsNode);
    Heritage.Heritage.AddChild(ANodes.WithRollupTag);
    Heritage.Heritage.AddChild(ANodes.HavingToken);
    Heritage.Heritage.AddChild(ANodes.HavingNode);
    Heritage.Heritage.AddChild(ANodes.OrderByTag);
    Heritage.Heritage.AddChild(ANodes.OrdersNode);
    Heritage.Heritage.AddChild(ANodes.Limit.LimitToken);
    Heritage.Heritage.AddChild(ANodes.Limit.OffsetToken);
    Heritage.Heritage.AddChild(ANodes.Limit.OffsetValueToken);
    Heritage.Heritage.AddChild(ANodes.Limit.CommaToken);
    Heritage.Heritage.AddChild(ANodes.Limit.RowCountValueToken);
  end;
end;

{ TCustomSQLParser.TSchedule.TInterval ************************************}

class function TCustomSQLParser.TSchedule.TInterval.Create(const AParser: TCustomSQLParser; const ANodes: TInterval.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntScheduleInterval);

  with PInterval(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.QuantityExp);
    Heritage.AddChild(ANodes.UnitTag);
  end;
end;

{ TCustomSQLParser.TSchedule.TIntervalListItem ****************************}

class function TCustomSQLParser.TSchedule.TIntervalListItem.Create(const AParser: TCustomSQLParser; const ANodes: TIntervalListItem.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntScheduleIntervalListItem);

  with PIntervalListItem(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.PlusToken);
    Heritage.AddChild(ANodes.IntervalTag);
    Heritage.AddChild(ANodes.Interval);
  end;
end;

{ TCustomSQLParser.TSchedule **********************************************}

class function TCustomSQLParser.TSchedule.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
var
  I: Integer;
begin
  Result := TRange.Create(AParser, ntSchedule);

  with PSchedule(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.AtValue);
    for I := 0 to Length(ANodes.AtIntervalList) - 1 do
      Heritage.AddChild(ANodes.AtIntervalList[0]);
    Heritage.AddChild(ANodes.EveryValue);
    Heritage.AddChild(ANodes.StartsValue);
    for I := 0 to Length(ANodes.StartsIntervalList) - 1 do
      Heritage.AddChild(ANodes.StartsIntervalList[0]);
    Heritage.AddChild(ANodes.EndsValue);
    for I := 0 to Length(ANodes.EndsIntervalList) - 1 do
      Heritage.AddChild(ANodes.EndsIntervalList[0]);
  end;
end;

{ TCustomSQLParser.TSoundsLikeOp **********************************************}

class function TCustomSQLParser.TSoundsLikeOp.Create(const AParser: TCustomSQLParser; const AOperator1, AOperator2: TOffset; const AOperand1, AOperand2: TOffset): TOffset;
begin
  Result := TRange.Create(AParser, ntSoundsLikeOp);

  with PSoundsLikeOp(AParser.NodePtr(Result))^ do
  begin
    FNodes.Operator1 := AOperator1;
    FNodes.Operator2 := AOperator2;
    FNodes.Operand1 := AOperand1;
    FNodes.Operand2 := AOperand2;

    Heritage.AddChild(AOperator1);
    Heritage.AddChild(AOperator2);
    Heritage.AddChild(AOperand1);
    Heritage.AddChild(AOperand2);
  end;
end;

{ TCustomSQLParser.TSubArea ***************************************************}

class function TCustomSQLParser.TSubArea.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSubArea);

  with PSubArea(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.OpenBracket);
    Heritage.AddChild(ANodes.AreaNode);
    Heritage.AddChild(ANodes.CloseBracket);
  end;
end;

{ TCustomSQLParser.TSubPartition **********************************************}

class function TCustomSQLParser.TSubPartition.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSubPartition);

  with PSubPartition(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.SubPartitionTag);
    Heritage.AddChild(ANodes.NameIdent);
    Heritage.AddChild(ANodes.EngineValue);
    Heritage.AddChild(ANodes.CommentValue);
    Heritage.AddChild(ANodes.DataDirectoryValue);
    Heritage.AddChild(ANodes.IndexDirectoryValue);
    Heritage.AddChild(ANodes.MaxRowsValue);
    Heritage.AddChild(ANodes.MinRowsValue);
  end;
end;

{ TCustomSQLParser.TTag *******************************************************}

class function TCustomSQLParser.TTag.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntTag);

  with PTag(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.KeywordToken1);
    Heritage.AddChild(ANodes.KeywordToken2);
    Heritage.AddChild(ANodes.KeywordToken3);
    Heritage.AddChild(ANodes.KeywordToken4);
  end;
end;

{ TCustomSQLParser.TTruncateTableStmt *********************************************}

class function TCustomSQLParser.TTruncateTableStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stTruncateTable);

  with PTruncateTableStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.TruncateTag);
    Heritage.Heritage.AddChild(ANodes.TableTag);
    Heritage.Heritage.AddChild(ANodes.TableIdent);
  end;
end;

{ TCustomSQLParser.TUnaryOp ***************************************************}

class function TCustomSQLParser.TUnaryOp.Create(const AParser: TCustomSQLParser; const AOperator, AOperand: TOffset): TOffset;
begin
  Result := TRange.Create(AParser, ntUnaryOp);

  with PUnaryOp(AParser.NodePtr(Result))^ do
  begin
    FNodes.Operator := AOperator;
    FNodes.Operand := AOperand;

    Heritage.AddChild(AOperator);
    Heritage.AddChild(AOperand);
  end;
end;

{ TCustomSQLParser.TUnknownStmt ***************************************************}

class function TCustomSQLParser.TUnknownStmt.Create(const AParser: TCustomSQLParser; const ATokens: Classes.TList): TOffset;
var
  I: Integer;
begin
  Result := TStmt.Create(AParser, stUnknownStmt);

  with PUnknownStmt(AParser.NodePtr(Result))^ do
  begin
    for I := 0 to ATokens.Count - 1 do
      Heritage.Heritage.AddChild(Integer(ATokens[I]));
  end;
end;

{ TCustomSQLParser.TUser ******************************************************}

class function TCustomSQLParser.TUser.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntUser);

  with PUser(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.NameToken);
    Heritage.AddChild(ANodes.AtToken);
    Heritage.AddChild(ANodes.HostToken);
  end;
end;

{ TCustomSQLParser.TValue *******************************************************}

class function TCustomSQLParser.TValue.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntValue);

  with PValue(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.IdentTag);
    Heritage.AddChild(ANodes.AssignToken);
    Heritage.AddChild(ANodes.ValueNode);
  end;
end;

{ TCustomSQLParser.TWhileStmt *************************************************}

class function TCustomSQLParser.TWhileStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stWhile);

  with PWhileStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.BeginLabelToken);
    Heritage.Heritage.AddChild(ANodes.WhileTag);
    Heritage.Heritage.AddChild(ANodes.SearchConditionExpr);
    Heritage.Heritage.AddChild(ANodes.DoTag);
    Heritage.Heritage.AddChild(ANodes.StmtList);
    Heritage.Heritage.AddChild(ANodes.EndTag);
    Heritage.Heritage.AddChild(ANodes.EndLabelToken);
  end;
end;

{ TCustomSQLParser ************************************************************}

function TCustomSQLParser.ApplyCurrentToken(const AUsageType: TUsageType = utUnknown; const ATokenType: fspTypes.TTokenType = fspTypes.ttUnknown): TOffset;
begin
  Result := CurrentToken;

  if (Result > 0) then
  begin
    if (AUsageType <> utUnknown) then
      TokenPtr(Result)^.FUsageType := AUsageType;
    if (ATokenType <> ttUnknown) then
      TokenPtr(CurrentToken)^.FTokenType := ATokenType;

    FParsedTokens.Delete(0);
  end;
end;

constructor TCustomSQLParser.Create(const ASQLDialect: TSQLDialect);
begin
  inherited Create();

  FFunctions := TWordList.Create(Self);
  FHighNotPrecedence := False;
  FKeywords := TWordList.Create(Self);
  FNodes.Mem := nil;
  FNodes.Offset := 0;
  FNodes.Size := 0;
  FParsedTokens := Classes.TList.Create();
  FPipesAsConcat := False;
  FSQLDialect := ASQLDialect;
end;

procedure TCustomSQLParser.DeleteNode(const ANode: PNode);
begin
  PDeleted(ANode)^.FNodeSize := NodeSize(ANode^.NodeType);
  PDeleted(ANode)^.FNodeType := ntDeleted;
end;

destructor TCustomSQLParser.Destroy();
begin
  FFunctions.Free();
  FKeywords.Free();
  if (FNodes.Size > 0) then
    FreeMem(FNodes.Mem);
  FParsedTokens.Free();

  inherited;
end;

function TCustomSQLParser.GetCurrentToken(): TOffset;
begin
  Result := GetParsedToken(0);
end;

function TCustomSQLParser.GetError(): Boolean;
begin
  Result := FErrorCode <> PE_Success;
end;

function TCustomSQLParser.GetErrorMessage(const AErrorCode: Integer): string;
begin
  case (AErrorCode) of
    PE_Success: Result := '';
    PE_Unknown: Result := 'Unknown error';
    PE_EmptyText: Result := 'Text is empty';
    PE_Syntax: Result := 'Invalid or unexpected character';
    PE_IncompleteToken: Result := 'Uncompleted Token';
    PE_UnexpectedToken: Result := 'Token unexpected or not understood';
    PE_UnkownStmt: Result := 'First Token is not a known Statement keyword';
    PE_IncompleteStmt: Result := 'Uncompleted Token';
    PE_InvalidEndLabel: Result := 'Begin and End Token are different';
    else Result := '[Unknown Error Code]';
  end;
end;

function TCustomSQLParser.GetFunctions(): string;
begin
  Result := FFunctions.Text;
end;

function TCustomSQLParser.GetKeywords(): string;
begin
  Result := FKeywords.Text;
end;

function TCustomSQLParser.GetNextToken(Index: Integer): TOffset;
begin
  Assert(Index > 0);

  Result := GetParsedToken(Index);
end;

function TCustomSQLParser.GetParsedToken(Index: Integer): TOffset;
var
  Token: TOffset;
begin
  if (FParsedTokens.Count - 1 < Index) then
    repeat
      Token := ParseToken();
      if ((Token > 0) and TokenPtr(Token)^.IsUsed) then
        FParsedTokens.Add(Pointer(Token));
    until ((Token = 0) or (FParsedTokens.Count - 1 = Index));

  if (FParsedTokens.Count - 1 < Index) then
    Result := 0
  else
    Result := TOffset(FParsedTokens[Index]);
end;

function TCustomSQLParser.GetRoot(): PRoot;
begin
  Result := PRoot(NodePtr(FRoot));
end;

function TCustomSQLParser.IsChild(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and not (ANode^.NodeType in [ntUnknown, ntRoot]);
end;

function TCustomSQLParser.IsRange(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and not (ANode^.NodeType in [ntUnknown, ntRoot, ntToken]);
end;

function TCustomSQLParser.IsStmt(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType in [ntUnknownStmt, ntCompoundStmt, ntSelectStmt]);
end;

function TCustomSQLParser.IsToken(const ANode: TOffset): Boolean;
begin
  Result := IsToken(NodePtr(ANode));
end;

function TCustomSQLParser.IsToken(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType in [ntToken]);
end;

function TCustomSQLParser.NewNode(const ANodeType: TNodeType): TOffset;
var
  AdditionalSize: Integer;
  Size: Integer;
begin
  Size := NodeSize(ANodeType);

  if (FNodes.Offset + Size > FNodes.Size) then
  begin
    AdditionalSize := Max(FNodes.Offset + Size, FNodes.Size);
    ReallocMem(FNodes.Mem, FNodes.Size + AdditionalSize);
    FillChar(FNodes.Mem[FNodes.Size], AdditionalSize, #0);
    Inc(FNodes.Size, AdditionalSize);
  end;

  Result := FNodes.Offset;

  Inc(FNodes.Offset, Size);
end;

function TCustomSQLParser.NodePtr(const ANode: TOffset): PNode;
begin
  if (ANode = 0) then
    Result := nil
  else
    Result := @FNodes.Mem[ANode];
end;

function TCustomSQLParser.NodeSize(const ANode: TOffset): Integer;
begin
  if (NodePtr(ANode)^.NodeType = ntDeleted) then
    Result := PDeleted(NodePtr(ANode))^.FNodeSize
  else
    Result := NodeSize(NodePtr(ANode)^.NodeType);
end;

function TCustomSQLParser.NodeSize(const ANodeType: TNodeType): Integer;
begin
  case (ANodeType) of
    ntRoot: Result := SizeOf(TRoot);
    ntToken: Result := SizeOf(TToken);
    ntRange: Result := SizeOf(TRange);

    ntAlterDatabaseStmt: Result := SizeOf(TAlterDatabaseStmt);
    ntAlterEventStmt: Result := SizeOf(TAlterEventStmt);
    ntAlterRoutineStmt: Result := SizeOf(TAlterRoutineStmt);
    ntAlterServerStmt: Result := SizeOf(TAlterServerStmt);
    ntAlterTableStmt: Result := SizeOf(TAlterTableStmt);
    ntAlterTableStmtConvertTo: Result := SizeOf(TAlterTableStmt.TConvertTo);
    ntAlterTableStmtDropObject: Result := SizeOf(TAlterTableStmt.TDropObject);
    ntAlterTableStmtExchangePartition: Result := SizeOf(TAlterTableStmt.TExchangePartition);
    ntAlterTableStmtReorganizePartition: Result := SizeOf(TAlterTableStmt.TReorganizePartition);
    ntAlterViewStmt: Result := SizeOf(TAlterViewStmt);
    ntBetweenOp: Result := SizeOf(TBetweenOp);
    ntBinaryOp: Result := SizeOf(TBinaryOp);
    ntCallStmt: Result := SizeOf(TCallStmt);
    ntCaseOp: Result := SizeOf(TCaseOp);
    ntCaseOpBranch: Result := SizeOf(TCaseOp.TBranch);
    ntCaseStmt: Result := SizeOf(TCaseStmt);
    ntCaseStmtBranch: Result := SizeOf(TCaseStmt.TBranch);
    ntColumn: Result := SizeOf(TColumn);
    ntCompoundStmt: Result := SizeOf(TCompoundStmt);
    ntCreateDatabaseStmt: Result := SizeOf(TCreateDatabaseStmt);
    ntCreateEventStmt: Result := SizeOf(TCreateEventStmt);
    ntCreateIndexStmt: Result := SizeOf(TCreateIndexStmt);
    ntCreateRoutineStmt: Result := SizeOf(TCreateRoutineStmt);
    ntCreateTableStmtReference: Result := SizeOf(TCreateTableStmt.TReference);
    ntCreateServerStmt: Result := SizeOf(TCreateServerStmt);
    ntCreateTableStmt: Result := SizeOf(TCreateTableStmt);
    ntCreateTriggerStmt: Result := SizeOf(TCreateTriggerStmt);
    ntCreateViewStmt: Result := SizeOf(TCreateViewStmt);
    ntDataType: Result := SizeOf(TDataType);
    ntDbIdent: Result := SizeOf(TDbIdent);
    ntDeleteStmt: Result := SizeOf(TDeleteStmt);
    ntDoStmt: Result := SizeOf(TDoStmt);
    ntDropDatabaseStmt: Result := SizeOf(TDropDatabaseStmt);
    ntDropEventStmt: Result := SizeOf(TDropEventStmt);
    ntDropIndexStmt: Result := SizeOf(TDropIndexStmt);
    ntDropRoutineStmt: Result := SizeOf(TDropRoutineStmt);
    ntDropServerStmt: Result := SizeOf(TDropServerStmt);
    ntDropTableStmt: Result := SizeOf(TDropTableStmt);
    ntDropTriggerStmt: Result := SizeOf(TDropTriggerStmt);
    ntDropViewStmt: Result := SizeOf(TDropViewStmt);
    ntForeignKey: Result := SizeOf(TForeignKey);
    ntFunction: Result := SizeOf(TFunction);
    ntFunctionParam: Result := SizeOf(TRoutineParam);
    ntIfStmt: Result := SizeOf(TIfStmt);
    ntIfStmtBranch: Result := SizeOf(TIfStmt.TBranch);
    ntIndex: Result := SizeOf(TIndex);
    ntIndexColumn: Result := SizeOf(TIndexColumn);
    ntInsertStmt: Result := SizeOf(TInsertStmt);
    ntIterateStmt: Result := SizeOf(TIterateStmt);
    ntLeaveStmt: Result := SizeOf(TLeaveStmt);
    ntList: Result := SizeOf(TList);
    ntPartition: Result := SizeOf(TPartition);
    ntPartitionValues: Result := SizeOf(TPartitionValues);
    ntProcedureParam: Result := SizeOf(TRoutineParam);
    ntSchedule: Result := SizeOf(TSchedule);
    ntScheduleInterval: Result := SizeOf(TSchedule.TInterval);
    ntScheduleIntervalListItem: Result := SizeOf(TSchedule.TIntervalListItem);
    ntSelectStmt: Result := SizeOf(TSelectStmt);
    ntSelectStmtColumn: Result := SizeOf(TSelectStmt.TField);
    ntSelectStmtGroup: Result := SizeOf(TSelectStmt.TGroup);
    ntSelectStmtJoin: Result := SizeOf(TSelectStmt.TJoin);
    ntSelectStmtOrderBy: Result := SizeOf(TSelectStmt.TOrderBy);
    ntSelectStmtTableFactor: Result := SizeOf(TSelectStmt.TTableFactor);
    ntSelectStmtTableFactorOj: Result := SizeOf(TSelectStmt.TTableFactorOj);
    ntSelectStmtTableIndexHint: Result := SizeOf(TSelectStmt.TTableFactor.TIndexHint);
    ntSoundsLikeOp: Result := SizeOf(TSoundsLikeOp);
    ntSubArea: Result := SizeOf(TSubArea);
    ntSubPartition: Result := SizeOf(TSubPartition);
    ntTag: Result := SizeOf(TTag);
    ntTruncateTableStmt: Result := SizeOf(TTruncateTableStmt);
    ntUnaryOp: Result := SizeOf(TUnaryOp);
    ntUnknownStmt: Result := SizeOf(TUnknownStmt);
    ntUser: Result := SizeOf(TUser);
    ntValue: Result := SizeOf(TValue);
    else raise ERangeError.Create(SArgumentOutOfRange);
  end;
end;

function TCustomSQLParser.Parse(const Text: PChar; const Length: Integer): Boolean;
begin
  SetString(FParsedText, Text, Length);
  FParsePos.Text := PChar(ParsedText);
  FParsePos.Length := Length;
  FParsePos.Origin.X := 0;
  FParsePos.Origin.Y := 0;

  FNodes.Offset := 1;
  FNodes.Size := 1024 * 1024;
  ReallocMem(FNodes.Mem, FNodes.Size);
  FillChar(FNodes.Mem[0], FNodes.Size, #0);

  FRoot := TRoot.Create(Self);
  FMySQLVersion := -1;

  Result := True;

  Root^.FFirstToken := CurrentToken;
  Root^.FFirstStmt := 0;

  while (CurrentToken <> 0) do
  begin
    FErrorCode := PE_Success;
    FErrorToken := 0;

    if (Root^.FFirstStmt = 0) then
    begin
      Root^.FFirstStmt := ParseStmt();
      Root^.FLastStmt := Root^.FFirstStmt;
    end
    else
      Root^.FLastStmt := ParseStmt();

    if (CurrentToken > 0) then
      if (TokenPtr(CurrentToken)^.TokenType <> ttDelimiter) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
      begin
        ApplyCurrentToken(); // ttDelimiter
      end;
  end;
end;

function TCustomSQLParser.Parse(const Text: string): Boolean;
begin
  Result := Parse(PChar(Text), Length(Text));
end;

function TCustomSQLParser.ParseAlterDatabaseStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TAlterDatabaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSCHEMA)) then
    Nodes.DatabaseTag := ParseTag(kiSCHEMA)
  else
    Nodes.DatabaseTag := ParseTag(kiDATABASE);

  if (not Error) then
    Nodes.IdentTag := ParseDbIdent(ditDatabase);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUPGRADE)) then
    Nodes.UpgradeDataDirectoryNameTag := ParseTag(kiUPGRADE, kiDATA, kiDIRECTORY, kiNAME)
  else
  begin
    Found := True;
    while (not Error and Found and (CurrentToken > 0)) do
      if ((Nodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
        Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaAuto, ParseString)
      else if ((Nodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
        Nodes.CharacterSetValue := ParseValue(kiCOLLATE, vaAuto, ParseString)
      else if ((Nodes.CollateValue = 0) and (NextToken[1] > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
        Nodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseString)
      else if ((Nodes.CollateValue = 0) and (NextToken[1] > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
        Nodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaAuto, ParseString)
      else
        Found := False;
  end;

  Result := TAlterDatabaseStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseAlterEventStmt(): TOffset;
var
  Nodes: TAlterEventStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    Nodes.EventTag := ParseTag(kiEVENT);

  if (not Error) then
    Nodes.EventIdent := ParseEventIdent();

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSCHEDULE)) then
    Nodes.OnScheduleValue := ParseValue(WordIndices(kiON, kiSCHEDULE), vaNo, ParseSchedule);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOMPLETION)) then
    if ((NextToken[2] > 0) and (TokenPtr(NextToken[2])^.KeywordIndex = kiNOT)) then
      Nodes.OnCompletitionTag := ParseTag(kiON, kiCOMPLETION, kiNOT, kiPRESERVE)
    else
      Nodes.OnCompletitionTag := ParseTag(kiON, kiCOMPLETION, kiPRESERVE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiRENAME)) then
    Nodes.RenameValue := ParseValue(WordIndices(kiRENAME, kiTO), vaNo, ParseEventIdent);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
    Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString);

  if (not Error) then
  begin
    Nodes.DoTag := ParseTag(kiDO);

    if (not Error) then
      Nodes.Body := ParsePL_SQLStmt();
  end;

  Result := TAlterEventStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseAlterRoutineStmt(const ARoutineType: TRoutineType): TOffset;
var
  Found: Boolean;
  Nodes: TAlterRoutineStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.RoutineTag := ParseTag(kiFUNCTION)
    else
      Nodes.RoutineTag := ParseTag(kiPROCEDURE);

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.IdentNode := ParseDbIdent(ditFunction)
    else
      Nodes.IdentNode := ParseDbIdent(ditProcedure);

  Found := True;
  while (not Error and Found and (CurrentToken > 0)) do
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLANGUAGE) then
      Nodes.LanguageTag := ParseTag(kiCOMMENT, kiSQL)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCONTAINS) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiCONTAINS, kiSQL)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiNO) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiNO, kiSQL)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREADS) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiREADS, kiSQL, kiDATA)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiMODIFIES) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiMODIFIES, kiSQL, kiDATA)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL) then
      if (NextToken[2] = 0) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[2])^.KeywordIndex = kiDEFINER) then
        Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiDEFINER)
      else
        Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiINVOKER)
    else
      Found := False;

  Result := TAlterRoutineStmt.Create(Self, ARoutineType, Nodes);
end;

function TCustomSQLParser.ParseAlterServerStmt(): TOffset;
var
  Nodes: TAlterServerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error) then
    Nodes.ServerTag := ParseTag(kiSERVER);

  if (not Error) then
    Nodes.IdentNode := ParseDbIdent(ditServer);

  if (not Error) then
    Nodes.OptionsTag := ParseTag(kiOPTIONS);

  if (not Error) then
    Nodes.OptionsList := ParseServerOptionList();

  Result := TAlterServerStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseAlterTableStmt(): TOffset;
var
  DelimiterExpected: Boolean;
  DelimiterFound: Boolean;
  Nodes: TAlterTableStmt.TNodes;
  Specifications: Classes.TList;
  SpecificationsNodes: TList.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Specifications := Classes.TList.Create();

  Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE)) then
    Nodes.IgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    Nodes.IdentNode := ParseDbIdent(ditTable);

  DelimiterFound := False; DelimiterExpected := False;
  while (not Error and (DelimiterFound or not DelimiterExpected) and (CurrentToken > 0)) do
  begin
    DelimiterExpected := True;

    if (TokenPtr(CurrentToken)^.KeywordIndex = kiADD) then
      Specifications.Add(Pointer(ParseCreateTableStmtDefinition(True)))
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiALTER) then
      Specifications.Add(Pointer(ParseAlterTableStmtAlterColumn()))
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCHANGE) then
      Specifications.Add(Pointer(ParseColumn(caChange)))
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDROP) then
      Specifications.Add(Pointer(ParseAlterTableStmtDropObject()))
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiMODIFY) then
      Specifications.Add(Pointer(ParseColumn(caModify)))


    else if ((Nodes.AlgorithmValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
    begin
      Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaAuto, WordIndices(kiDEFAULT, kiINPLACE, kiCOPY));
      Specifications.Add(Pointer(Nodes.AlgorithmValue));
    end
    else if ((Nodes.ConvertToCharacterSetNode = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCONVERT)) then
    begin
      Nodes.ConvertToCharacterSetNode := ParseAlterTableStmtConvertTo();
      Specifications.Add(Pointer(Nodes.ConvertToCharacterSetNode));
    end
    else if ((Nodes.EnableKeys = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDISABLE)) then
    begin
      Nodes.EnableKeys := ParseTag(kiDISABLE, kiKEYS);
      Specifications.Add(Pointer(Nodes.EnableKeys));
    end
    else if ((Nodes.DiscardTablespaceTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDISCARD)) then
    begin
      Nodes.DiscardTablespaceTag := ParseTag(kiDISCARD, kiTABLESPACE);
      Specifications.Add(Pointer(Nodes.DiscardTablespaceTag));
    end
    else if ((Nodes.EnableKeys = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENABLE)) then
    begin
      Nodes.EnableKeys := ParseTag(kiENABLE, kiKEYS);
      Specifications.Add(Pointer(Nodes.EnableKeys));
    end
    else if ((Nodes.ForceTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFORCE)) then
    begin
      Nodes.ForceTag := ParseTag(kiFORCE);
      Specifications.Add(Pointer(Nodes.ForceTag));
    end
    else if ((Nodes.ImportTablespaceTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIMPORT)) then
    begin
      Nodes.ImportTablespaceTag := ParseTag(kiDISCARD, kiTABLESPACE);
      Specifications.Add(Pointer(Nodes.ImportTablespaceTag));
    end
    else if ((Nodes.LockValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCK)) then
    begin
      Nodes.LockValue := ParseValue(kiLOCK, vaAuto, WordIndices(kiDEFAULT, kiNONE, kiSHARED, kiEXCLUSIVE));
      Specifications.Add(Pointer(Nodes.LockValue));
    end
    else if ((Nodes.OrderByValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiORDER)) then
    begin
      Nodes.OrderByValue := ParseValue(WordIndices(kiORDER, kiBY), vaNo, ParseIndexColumn);
      Specifications.Add(Pointer(Nodes.OrderByValue));
    end
    else if ((Nodes.RenameNode = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiRENAME)) then
    begin
      if ((NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiTO)) then
        Nodes.RenameNode := ParseValue(WordIndices(kiRENAME, kiTO), vaNo, ParseTableIdent)
      else if ((NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiAS)) then
        Nodes.RenameNode := ParseValue(WordIndices(kiRENAME, kiAS), vaNo, ParseTableIdent)
      else
        Nodes.RenameNode := ParseValue(kiRENAME, vaNo, ParseTableIdent);
      Specifications.Add(Pointer(Nodes.RenameNode));
    end


    else if ((Nodes.TableOptionsNodes.AutoIncrementValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAUTO_INCREMENT)) then
    begin
      Nodes.TableOptionsNodes.AutoIncrementValue := ParseValue(kiAUTO_INCREMENT, vaAuto, ParseInteger);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.AutoIncrementValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.AvgRowLengthValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAVG_ROW_LENGTH)) then
    begin
      Nodes.TableOptionsNodes.AvgRowLengthValue := ParseValue(kiAVG_ROW_LENGTH, vaAuto, ParseInteger);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.AvgRowLengthValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
    begin
      Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaAuto, ParseIdent);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.CharacterSetValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.ChecksumValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHECKSUM)) then
    begin
      Nodes.TableOptionsNodes.AutoIncrementValue := ParseValue(kiCHECKSUM, vaAuto, ParseInteger);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.AutoIncrementValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.CollateValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
    begin
      Nodes.TableOptionsNodes.CollateValue := ParseValue(WordIndices(kiCOLLATE), vaAuto, ParseIdent);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.CollateValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARACTER)) then
    begin
      Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseIdent);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.CharacterSetValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.CollateValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOLLATE)) then
    begin
      Nodes.TableOptionsNodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaAuto, ParseIdent);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.CollateValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
    begin
      Nodes.TableOptionsNodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.CommentValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.ConnectionValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCONNECTION)) then
    begin
      Nodes.TableOptionsNodes.ConnectionValue := ParseValue(kiCONNECTION, vaAuto, ParseString);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.ConnectionValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.DataDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDATA)) then
    begin
      Nodes.TableOptionsNodes.DataDirectoryValue := ParseValue(WordIndices(kiDATA, kiDIRECTORY), vaAuto, ParseString);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.DataDirectoryValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.DelayKeyWriteValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDELAY_KEY_WRITE)) then
    begin
      Nodes.TableOptionsNodes.DelayKeyWriteValue := ParseValue(kiDELAY_KEY_WRITE, vaAuto, ParseInteger);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.DelayKeyWriteValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENGINE)) then
    begin
      Nodes.TableOptionsNodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseIdent);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.EngineValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.IndexDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX)) then
    begin
      Nodes.TableOptionsNodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.IndexDirectoryValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.InsertMethodValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINSERT_METHOD)) then
    begin
      Nodes.TableOptionsNodes.InsertMethodValue := ParseValue(kiINSERT_METHOD, vaAuto, ParseIdent);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.InsertMethodValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.KeyBlockSizeValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY_BLOCK_SIZE)) then
    begin
      Nodes.TableOptionsNodes.KeyBlockSizeValue := ParseValue(kiKEY_BLOCK_SIZE, vaAuto, ParseInteger);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.KeyBlockSizeValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.MaxRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_ROWS)) then
    begin
      Nodes.TableOptionsNodes.MaxRowsValue := ParseValue(kiMAX_ROWS, vaAuto, ParseInteger);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.MaxRowsValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.MinRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMIN_ROWS)) then
    begin
      Nodes.TableOptionsNodes.MinRowsValue := ParseValue(kiMIN_ROWS, vaAuto, ParseInteger);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.MinRowsValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.PackKeysValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPACK_KEYS)) then
    begin
      Nodes.TableOptionsNodes.PackKeysValue := ParseValue(kiPACK_KEYS, vaAuto, ParseExpr);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.PackKeysValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.PasswordValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPASSWORD)) then
    begin
      Nodes.TableOptionsNodes.PasswordValue := ParseValue(kiPASSWORD, vaAuto, ParseString);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.PasswordValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.RowFormatValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiROW_FORMAT)) then
    begin
      Nodes.TableOptionsNodes.RowFormatValue := ParseValue(kiROW_FORMAT, vaAuto, ParseIdent);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.RowFormatValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.StatsAutoRecalcValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTATS_AUTO_RECALC)) then
    begin
      if (NextToken[1] = 0) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[1])^.TokenType = ttInteger) then
        Nodes.TableOptionsNodes.StatsAutoRecalcValue := ParseValue(kiSTATS_AUTO_RECALC, vaAuto, ParseInteger)
      else
        Nodes.TableOptionsNodes.StatsAutoRecalcValue := ParseValue(kiSTATS_AUTO_RECALC, vaAuto, WordIndices(kiDEFAULT));
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.StatsAutoRecalcValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.StatsPersistentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTATS_PERSISTENT)) then
    begin
      if (NextToken[1] = 0) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[1])^.TokenType = ttInteger) then
        Nodes.TableOptionsNodes.StatsPersistentValue := ParseValue(kiSTATS_PERSISTENT, vaAuto, ParseInteger)
      else
        Nodes.TableOptionsNodes.StatsPersistentValue := ParseValue(kiSTATS_PERSISTENT, vaAuto, WordIndices(kiDEFAULT));
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.StatsPersistentValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.UnionList = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUNION)) then
    begin
      Nodes.TableOptionsNodes.UnionList := ParseAlterTableStmtUnion();
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.UnionList));
      DelimiterExpected := False;
    end


    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiANALYZE)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiCHECK)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiOPTIMIZE)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiREBUILD)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiREPAIR)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiTRUNCATE)) then
    begin
      Specifications.Add(Pointer(ParseValue(TokenPtr(CurrentToken)^.KeywordIndex, vaNo, ParsePartitionNames)));
      break;
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCOALESCE) then
    begin
      Specifications.Add(Pointer(ParseValue(kiCOALESCE, vaNo, ParseInteger)));
      break;
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEXCHANGE) then
    begin
      Specifications.Add(Pointer(ParseAlterTableStmtExchangePartition()));
      break;
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREMOVE) then
    begin
      Specifications.Add(Pointer(ParseTag(kiREMOVE, kiPARTITIONING)));
      break;
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREORGANIZE) then
    begin
      Specifications.Add(Pointer(ParseAlterTableStmtReorganizePartition()));
      break;
    end;

    if (DelimiterExpected and (CurrentToken > 0) and not (TokenPtr(CurrentToken)^.TokenType in [ttComma, ttDelimiter])) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      DelimiterFound := (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttComma);
      if (DelimiterFound) then
        Specifications.Add(Pointer(ApplyCurrentToken()));
    end;
  end;

  FillChar(SpecificationsNodes, SizeOf(SpecificationsNodes), 0);
  Nodes.SpecificationList := TList.Create(Self, SpecificationsNodes, Specifications.Count, TIntegerArray(Specifications.List));
  Result := TAlterTableStmt.Create(Self, Nodes);

  Specifications.Free();
end;

function TCustomSQLParser.ParseAlterTableStmtAlterColumn(): TOffset;
var
  Nodes: TAlterTableStmt.TAlterColumn.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if ((NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOLUMN)) then
    Nodes.AlterTag := ParseTag(kiALTER, kiCOLUMN)
  else
    Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error) then
    Nodes.ColumnIdent := ParseColumnIdent();

  if (not Error and (CurrentToken > 0)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiSET) then
      Nodes.SetDefaultValue := ParseValue(WordIndices(kiSET, kiDEFAULT), vaNo, ParseString)
    else
      Nodes.DropDefaultTag := ParseTag(kiDROP);

  Result := TAlterTableStmt.TAlterColumn.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseAlterTableStmtConvertTo(): TOffset;
var
  Nodes: TAlterTableStmt.TConvertTo.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ConvertToTag := ParseTag(kiCONVERT, kiTO);

  if (not Error) then
    Nodes.CharacterSetTag := ParseTag(kiCHARACTER, kiSET);

  if (not Error) then
    Nodes.CharacterSetNameToken := ParseIdent();

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
  begin
    Nodes.CollateTag := ParseTag(kiCOLLATE);

    if (not Error) then
      Nodes.CollateNameToken := ParseIdent();
  end;

  Result := TAlterTableStmt.TConvertTo.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseAlterTableStmtDropObject(): TOffset;
var
  Nodes: TAlterTableStmt.TDropObject.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error) then
    Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
  begin
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION) then
    begin
      Nodes.ObjectTypeTag := ParseTag(kiPARTITION);

      if (not Error) then
        Nodes.NameNode := ParseList(False, ParsePartitionIdent);
    end
    else
    begin
      if (TokenPtr(CurrentToken)^.KeywordIndex = kiPRIMARY) then
        Nodes.ObjectTypeTag := ParseTag(kiPRIMARY, kiKEY)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX) then
        Nodes.ObjectTypeTag := ParseTag(kiINDEX)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY) then
        Nodes.ObjectTypeTag := ParseTag(kiKEY)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFOREIGN) then
        Nodes.ObjectTypeTag := ParseTag(kiFOREIGN, kiKEY)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLUMN) then
        Nodes.ObjectTypeTag := ParseTag(kiCOLUMN);

      if (not Error) then
        if (CurrentToken = 0) then
          SetError(PE_IncompleteStmt)
        else
          Nodes.NameNode := ApplyCurrentToken();
    end;
  end;

  Result := TAlterTableStmt.TDropObject.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseAlterTableStmtExchangePartition(): TOffset;
var
  Nodes: TAlterTableStmt.TExchangePartition.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ExchangePartitionTag := ParseTag(kiEXCHANGE, kiPARTITION);

  if (not Error) then
    Nodes.PartitionIdent := ParsePartitionIdent();

  if (not Error) then
    Nodes.WithTableTag := ParseTag(kiWITH, kiTABLE);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  Result := TAlterTableStmt.TExchangePartition.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseAlterTableStmtReorganizePartition(): TOffset;
var
  Nodes: TAlterTableStmt.TReorganizePartition.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ReorganizePartitionTag := ParseTag(kiREORGANIZE, kiPARTITION);

  if (not Error) then
    Nodes.PartitionIdentList := ParseList(False, ParsePartitionIdent);

  if (not Error) then
    Nodes.IntoTag := ParseTag(kiINTO);

  if (not Error) then
    Nodes.PartitionList := ParseList(True, ParsePartition);

  Result := TAlterTableStmt.TReorganizePartition.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseAlterTableStmtUnion(): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(kiUNION);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
  begin
    TokenPtr(CurrentToken)^.FOperatorType := otAssign;
    Nodes.AssignToken := ApplyCurrentToken();
  end;

  if (not Error) then
    Nodes.ValueNode := ParseList(True, ParseTableIdent);

  Result := TValue.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseAlterStmt(): TOffset;
var
  Index: Integer;
begin
  Assert(TokenPtr(CurrentToken)^.KeywordIndex = kiALTER);

  Result := 0;
  Index := 1;

  if (not Error and (NextToken[Index] > 0) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiALGORITHM)) then
  begin
    Inc(Index);
    if (NextToken[Index] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);

      if (NextToken[Index] = 0) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiUNDEFINED)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiMERGE)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiTEMPTABLE)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index);
    end;
  end;

  if (not Error and (NextToken[Index] > 0) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiDEFINER)) then
  begin
    Inc(Index);
    if (NextToken[Index] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);

      if (NextToken[Index] = 0) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiCURRENT_USER) then
        if (((NextToken[Index + 1] = 0) or (TokenPtr(NextToken[Index + 1])^.TokenType <> ttOpenBracket))
          and ((NextToken[Index + 2] = 0) or (TokenPtr(NextToken[Index + 2])^.TokenType <> ttCloseBracket))) then
          Inc(Index)
        else
          Inc(Index, 3)
      else
      begin
        Inc(Index); // Username

        if (not Error and (NextToken[Index] > 0) and (TokenPtr(NextToken[Index])^.TokenType = ttAt)) then
        begin
          Inc(Index); // @
          if (not Error and (NextToken[Index] > 0)) then
            Inc(Index); // Servername
        end;
      end;
    end;
  end;

  if (not Error and (NextToken[Index] > 0) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiSQL)) then
  begin
    Inc(Index);
    if (NextToken[Index] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex <> kiSECURITY) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);
      if (NextToken[Index] = 0) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiDEFINER)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiINVOKER)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index);
    end;
  end;

  if (not Error) then
    if (NextToken[Index] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiDATABASE) then
      Result := ParseAlterDatabaseStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiEVENT) then
      Result := ParseAlterEventStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiFUNCTION) then
      Result := ParseAlterRoutineStmt(rtFunction)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiPROCEDURE) then
      Result := ParseAlterRoutineStmt(rtProcedure)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiSERVER) then
      Result := ParseAlterServerStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiTABLE) then
      Result := ParseAlterTableStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiVIEW) then
      Result := ParseAlterViewStmt()
    else
      SetError(PE_UnexpectedToken, NextToken[Index]);
end;

function TCustomSQLParser.ParseAlterViewStmt(): TOffset;
var
  Nodes: TAlterViewStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.AlterTag := ParseTag(kiCREATE);

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
    Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaYes, ParseKeyword);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL)) then
    if (NextToken[2] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[2])^.KeywordIndex = kiDEFINER) then
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiDEFINER)
    else
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiINVOKER);

  if (not Error) then
    Nodes.ViewTag := ParseTag(kiVIEW);

  if (not Error) then
    Nodes.IdentNode := ParseDbIdent(ditView);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.Columns := ParseList(True, ParseColumnIdent);

  if (not Error) then
    Nodes.AsTag := ParseTag(kiAS);

  if (not Error) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiSELECT) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      Nodes.SelectStmt := ParseSelectStmt();

      if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
        if (NextToken[1] = 0) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(NextToken[1])^.KeywordIndex = kiCASCADED) then
          Nodes.OptionTag := ParseTag(kiWITH, kiCASCADED, kiCHECK, kiOPTION)
        else if (TokenPtr(NextToken[1])^.KeywordIndex = kiLOCAL) then
          Nodes.OptionTag := ParseTag(kiWITH, kiLOCAL, kiCHECK, kiOPTION)
        else
          Nodes.OptionTag := ParseTag(kiWITH, kiCHECK, kiOPTION);
    end;

  Result := TAlterViewStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseCallStmt(): TOffset;
var
  Nodes: TCallStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CallTag := ParseTag(kiCALL);

  if (not Error) then
    Nodes.ProcedureIdent := ParseDbIdent(ditProcedure);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.ParamList := ParseList(True, ParseExpr);

  Result := TCallStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseCaseOp(): TOffset;
var
  Branches: array of TOffset;
  ListNodes: TList.TNodes;
  Nodes: TCaseOp.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  SetLength(Branches, 0);

  Nodes.CaseTag := ParseTag(kiCASE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN)) then
    Nodes.CompareExpr := ParseExpr();

  while (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHEN)) do
  begin
    SetLength(Branches, Length(Branches) + 1);
    Branches[Length(Branches) - 1] := ParseCaseOpBranch();
  end;

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE)) then
  begin
    SetLength(Branches, Length(Branches) + 1);
    Branches[Length(Branches) - 1] := ParseCaseOpBranch();
  end;

  FillChar(ListNodes, SizeOf(ListNodes), 0);
  Nodes.BranchList := TList.Create(Self, ListNodes, Length(Branches), Branches);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND);

  Result := TCaseOp.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseCaseOpBranch(): TOffset;
var
  Nodes: TCaseOp.TBranch.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHEN)) then
  begin
    Nodes.Tag := ParseTag(kiWHEN);

    if (not Error) then
      Nodes.ConditionExpr := ParseExpr();

    if (not Error) then
      Nodes.ThenTag := ParseTag(kiTHEN);
  end
  else if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE)) then
  begin
    Nodes.Tag := ParseTag(kiELSE);

    if (not Error) then
      Nodes.ConditionExpr := ParseExpr();
  end;

  if (not Error) then
    Nodes.ResultExpr := ParseExpr();

  Result := TCaseOp.TBranch.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseCaseStmt(): TOffset;
var
  Branches: array of TOffset;
  ListNodes: TList.TNodes;
  Nodes: TCaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  SetLength(Branches, 0);

  Nodes.CaseTag := ParseTag(kiCASE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN)) then
    Nodes.CompareExpr := ParseExpr();

  while (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHEN)) do
  begin
    SetLength(Branches, Length(Branches) + 1);
    Branches[Length(Branches) - 1] := ParseCaseStmtBranch();
  end;

  FillChar(ListNodes, SizeOf(ListNodes), 0);
  Nodes.BranchList := TList.Create(Self, ListNodes, Length(Branches), Branches);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiCASE);

  Result := TCaseStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseCaseStmtBranch(): TOffset;
var
  Nodes: TCaseStmt.TBranch.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if ((CurrentToken = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHEN)) then
  begin
    Nodes.Tag := ParseTag(kiWHEN);

    if (not Error) then
      Nodes.ConditionExpr := ParseExpr();

    if (not Error) then
      Nodes.ThenTag := ParseTag(kiTHEN);
  end
  else if ((CurrentToken = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE)) then
    Nodes.Tag := ParseTag(kiELSE);

  if (not Error and (Nodes.Tag > 0)) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter);

  Result := TCaseStmt.TBranch.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseColumn(const Add: TCreateTableColumnAdd = caNone): TOffset;
var
  Nodes: TColumn.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error) then
    if (Add = caAdd) then
      Nodes.AddTag := ParseTag(kiADD)
    else if (Add = caChange) then
      Nodes.AddTag := ParseTag(kiCHANGE)
    else if (Add = caModify) then
      Nodes.AddTag := ParseTag(kiMODIFY);

  if (not Error and (Add <> caNone)) then
    Nodes.ColumnTag := ParseTag(kiCOLUMN);

  if (not Error and (Add in [caChange, caModify])) then
    Nodes.OldNameIdent := ParseColumnIdent();

  if (not Error) then
    Nodes.NameIdent := ParseColumnIdent();

  if (not Error) then
    Nodes.DataTypeNode := ParseDataType();

  if (not Error and (CurrentToken > 0)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiNOT) then
      Nodes.Null := ParseTag(kiNOT, kiNULL)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiNULL) then
      Nodes.Null := ParseTag(kiNULL);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT)) then
    Nodes.DefaultValue := ParseValue(kiDEFAULT, vaNo, ParseExpr);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAUTO_INCREMENT)) then
    Nodes.AutoIncrementTag := ParseTag(kiAUTO_INCREMENT);

  if (not Error and (CurrentToken > 0)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiUNIQUE) then
      if ((NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
        Nodes.KeyTag := ParseTag(kiUNIQUE, kiKEY)
      else
        Nodes.KeyTag := ParseTag(kiUNIQUE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiPRIMARY) then
      Nodes.KeyTag := ParseTag(kiPRIMARY, kiKEY)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY) then
      Nodes.KeyTag := ParseTag(kiKEY);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT)) then
    Nodes.CommentValue := ParseValue(kiDEFAULT, vaNo, ParseString);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLUMN_FORMAT)) then
    Nodes.ColumnFormat := ParseValue(kiCOLUMN_FORMAt, vaNo, ParseIdent);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiREFERENCES)) then
    Nodes.ReferencesNode := ParseCreateTableStmtReferences();

  if (not Error and (Add <> caNone) and (CurrentToken > 0)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiFIRST) then
      Nodes.Position := ParseTag(kiFIRST)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiAFTER) then
      Nodes.Position := ParseValue(kiAFTER, vaNo, ParseColumnIdent);

  Result := TColumn.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseColumnNames(): TOffset;
begin
  Result := ParseList(False, ParseColumnIdent);
end;

function TCustomSQLParser.ParseCompoundStmt(): TOffset;
var
  Nodes: TCompoundStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(CurrentToken)^.TokenType = ttBeginLabel) then
    Nodes.BeginLabelToken := ApplyCurrentToken();

  Nodes.BeginTag := ParseTag(kiBEGIN);

  if (not Error) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttIdentifier)) then
    if ((Nodes.BeginLabelToken = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(Nodes.BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.EndLabelToken := ApplyCurrentToken(utLabel, ttEndLabel);

  Result := TCompoundStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseColumnIdent(): TOffset;
begin
  Result := 0;
  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Result := ApplyCurrentToken();
end;

function TCustomSQLParser.ParseCreateDatabaseStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TCreateDatabaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSCHEMA)) then
    Nodes.DatabaseTag := ParseTag(kiSCHEMA)
  else
    Nodes.DatabaseTag := ParseTag(kiDATABASE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfNotExistsTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    Nodes.DatabaseIdent := ParseDbIdent(ditDatabase);

  Found := True;
  while (not Error and Found and (CurrentToken > 0)) do
    if ((Nodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
      Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaAuto, ParseString)
    else if ((Nodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
      Nodes.CharacterSetValue := ParseValue(kiCOLLATE, vaAuto, ParseString)
    else if ((Nodes.CollateValue = 0) and (NextToken[1] > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
      Nodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseString)
    else if ((Nodes.CollateValue = 0) and (NextToken[1] > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
      Nodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaAuto, ParseString)
    else
      Found := False;

  Result := TCreateDatabaseStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseCreateEventStmt(): TOffset;
var
  Nodes: TCreateEventStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiALTER);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    Nodes.EventTag := ParseTag(kiEVENT);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfNotExistsTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    Nodes.EventIdent := ParseEventIdent();

  if (not Error) then
    Nodes.OnScheduleValue := ParseValue(WordIndices(kiON, kiSCHEDULE), vaNo, ParseSchedule);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOMPLETION)) then
    if ((NextToken[2] > 0) and (TokenPtr(NextToken[2])^.KeywordIndex = kiNOT)) then
      Nodes.OnCompletitionTag := ParseTag(kiON, kiCOMPLETION, kiNOT, kiPRESERVE)
    else
      Nodes.OnCompletitionTag := ParseTag(kiON, kiCOMPLETION, kiPRESERVE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
    Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString);

  if (not Error) then
    Nodes.DoTag := ParseTag(kiDO);

  if (not Error) then
    Nodes.Body := ParsePL_SQLStmt();

  Result := TCreateEventStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseCreateIndexStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TCreateIndexStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and (CurrentToken > 0)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiUNIQUE) then
      Nodes.IndexTag := ParseTag(kiUNIQUE, kiINDEX)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFULLTEXT) then
      Nodes.IndexTag := ParseTag(kiFULLTEXT, kiINDEX)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSPATIAL) then
      Nodes.IndexTag := ParseTag(kiSPATIAL, kiINDEX)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX) then
      Nodes.IndexTag := ParseTag(kiINDEX)
    else
      SetError(PE_UnexpectedToken, CurrentToken);

  if (not Error) then
    Nodes.IndexIdent := ParseDbIdent(ditIndex);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING)) then
    Nodes.IndexTypeValue := ParseValue(kiUSING, vaNo, WordIndices(kiBTREE, kiHASH));

  if (not Error) then
  begin
    Nodes.OnTag := ParseTag(kiON);

    if (not Error) then
      Nodes.TableIdent := ParseDbIdent(ditTable);
  end;

  if (not Error) then
    Nodes.ColumnNameList := ParseList(True, ParseIndexColumn);

  Found := True;
  while (not Error and Found and (CurrentToken > 0)) do
    if ((Nodes.AlgorithmValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
      Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaAuto, WordIndices(kiDEFAULT,kiINPLACE,kiCOPY))
    else if ((Nodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString)
    else if ((Nodes.KeyBlockSizeValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY_BLOCK_SIZE)) then
      Nodes.KeyBlockSizeValue := ParseValue(kiKEY_BLOCK_SIZE, vaAuto, ParseInteger)
    else if ((Nodes.LockValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCK)) then
      Nodes.LockValue := ParseValue(kiLOCK, vaAuto, WordIndices(kiDEFAULT, kiNONE, kiSHARED, kiEXCLUSIVE))
    else if ((Nodes.IndexTypeValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING)) then
      Nodes.IndexTypeValue := ParseValue(kiUSING, vaNo, WordIndices(kiBTREE, kiHASH))
    else if ((Nodes.ParserValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
      Nodes.ParserValue := ParseValue(WordIndices(kiWITH, kiPARSER), vaNo, ParseString)
    else
      Found := False;

  Result := TCreateIndexStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseCreateRoutineStmt(const ARoutineType: TRoutineType): TOffset;
var
  Found: Boolean;
  Nodes: TCreateRoutineStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.RoutineTag := ParseTag(kiFUNCTION)
    else
      Nodes.RoutineTag := ParseTag(kiPROCEDURE);

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.IdentNode := ParseDbIdent(ditFunction)
    else
      Nodes.IdentNode := ParseDbIdent(ditProcedure);

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.ParameterNode := ParseList(True, ParseFunctionParam)
    else
      Nodes.ParameterNode := ParseList(True, ParseProcedureParam);

  if (ARoutineType = rtFunction) then
  begin
    if (not Error) then
      Nodes.Return.ReturnsTag := ParseTag(kiRETURNS);

    if (not Error) then
      Nodes.Return.DataTypeNode := ParseDataType();
  end;

  Found := True;
  while (not Error and Found and (CurrentToken > 0)) do
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLANGUAGE) then
      Nodes.LanguageTag := ParseTag(kiCOMMENT, kiSQL)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiNOT) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiDETERMINISTIC)) then
      Nodes.DeterministicTag := ParseTag(kiNOT, kiDETERMINISTIC)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDETERMINISTIC) then
      Nodes.DeterministicTag := ParseTag(kiDETERMINISTIC)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCONTAINS) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiCONTAINS, kiSQL)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiNO) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiNO, kiSQL)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREADS) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiREADS, kiSQL, kiDATA)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiMODIFIES) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiMODIFIES, kiSQL, kiDATA)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL) then
      if (NextToken[2] = 0) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[2])^.KeywordIndex = kiDEFINER) then
        Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiDEFINER)
      else
        Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiINVOKER)
    else
      Found := False;

  if (not Error) then
    Nodes.Body := ParseStmt(True);

  Result := TCreateRoutineStmt.Create(Self, rtFunction, Nodes);
end;

function TCustomSQLParser.ParseCreateServerStmt(): TOffset;
var
  Nodes: TCreateServerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error) then
    Nodes.ServerTag := ParseTag(kiSERVER);

  if (not Error) then
    Nodes.ServerIdent := ParseDbIdent(ditServer);

  if (not Error) then
    Nodes.ForeignDataWrapperValue := ParseValue(WordIndices(kiFOREIGN, kiDATA, kiWRAPPER), vaNo, ParseString);

  if (not Error) then
    Nodes.OptionsTag := ParseTag(kiOPTIONS);

  if (not Error) then
    Nodes.OptionsList := ParseServerOptionList();

  Result := TCreateServerStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseCreateStmt(): TOffset;
var
  Index: Integer;
begin
  Result := 0;
  Index := 1;

  if (not Error and (NextToken[Index] > 0) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiOR)) then
  begin
    Inc(Index);
    if (NextToken[Index] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex <> kiREPLACE) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
      Inc(Index);
  end;

  if (not Error and (NextToken[Index] > 0) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiALGORITHM)) then
  begin
    Inc(Index);
    if (NextToken[Index] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);

      if (NextToken[Index] = 0) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiUNDEFINED)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiMERGE)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiTEMPTABLE)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index);
    end;
  end;

  if (not Error and (NextToken[Index] > 0) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiDEFINER)) then
  begin
    Inc(Index);
    if (NextToken[Index] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);

      if (NextToken[Index] = 0) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiCURRENT_USER) then
        if (((NextToken[Index + 1] = 0) or (TokenPtr(NextToken[Index + 1])^.TokenType <> ttOpenBracket))
          and ((NextToken[Index + 2] = 0) or (TokenPtr(NextToken[Index + 2])^.TokenType <> ttCloseBracket))) then
          Inc(Index)
        else
          Inc(Index, 3)
      else
      begin
        Inc(Index); // Username

        if (not Error and (NextToken[Index] > 0) and (TokenPtr(NextToken[Index])^.TokenType = ttAt)) then
        begin
          Inc(Index); // @
          if (not Error and (NextToken[Index] > 0)) then
            Inc(Index); // Servername
        end;
      end;
    end;
  end;

  if (not Error and (NextToken[Index] > 0) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiSQL)) then
  begin
    Inc(Index);
    if (NextToken[Index] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex <> kiSECURITY) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);
      if (NextToken[Index] = 0) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiDEFINER)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiINVOKER)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index);
    end;
  end;

  if (not Error and (NextToken[Index] > 0) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiTEMPORARY)) then
    Inc(Index);

  if (not Error) then
    if (NextToken[Index] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiDATABASE) then
      Result := ParseCreateDatabaseStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiEVENT) then
      Result := ParseCreateEventStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiFUNCTION) then
      Result := ParseCreateRoutineStmt(rtFunction)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiINDEX) then
      Result := ParseCreateIndexStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiPROCEDURE) then
      Result := ParseCreateRoutineStmt(rtProcedure)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiSERVER) then
      Result := ParseCreateServerStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiTABLE) then
      Result := ParseCreateTableStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiTRIGGER) then
      Result := ParseCreateTriggerStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiVIEW) then
      Result := ParseCreateViewStmt()
    else
      SetError(PE_UnexpectedToken, NextToken[Index]);
end;

function TCustomSQLParser.ParseCreateTableStmt(): TOffset;
var
  Found: Boolean;
  ListNodes: TList.TNodes;
  Nodes: TCreateTableStmt.TNodes;
  TableOptions: Classes.TList;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTEMPORARY)) then
    Nodes.TemporaryTag := ParseTag(kiTEMPORARY);

  if (not Error) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.TemporaryTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  if (not Error) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken)^.TokenType = ttOpenBracket) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiLIKE)) then
    begin
      Nodes.OpenBracketToken := ApplyCurrentToken();

      Nodes.LikeTag := ParseTag(kiLIKE);

      if (not Error) then
        Nodes.LikeTableIdent := ParseTableIdent();

      if (not Error) then
        Nodes.CloseBracketToken := ApplyCurrentToken();
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
    begin
      Nodes.LikeTag := ParseTag(kiLIKE);

      if (not Error) then
        Nodes.LikeTableIdent := ParseTableIdent();
    end
    else
    begin
      if (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket) then
        Nodes.DefinitionList := ParseList(True, ParseCreateTableStmtDefinition);

      if (not Error and (CurrentToken <> 0)) then
      begin
        TableOptions := Classes.TList.Create();

        Found := True;
        while (not Error and Found and (CurrentToken = 0)) do
        begin
          if ((Nodes.TableOptionsNodes.AutoIncrementValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAUTO_INCREMENT)) then
          begin
            Nodes.TableOptionsNodes.AutoIncrementValue := ParseValue(kiAUTO_INCREMENT, vaAuto, ParseInteger);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.AutoIncrementValue));
          end
          else if ((Nodes.TableOptionsNodes.AvgRowLengthValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAVG_ROW_LENGTH)) then
          begin
            Nodes.TableOptionsNodes.AvgRowLengthValue := ParseValue(kiAVG_ROW_LENGTH, vaAuto, ParseInteger);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.AvgRowLengthValue));
          end
          else if ((Nodes.TableOptionsNodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
          begin
            Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaAuto, ParseIdent);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.CharacterSetValue));
          end
          else if ((Nodes.TableOptionsNodes.ChecksumValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHECKSUM)) then
          begin
            Nodes.TableOptionsNodes.AutoIncrementValue := ParseValue(kiCHECKSUM, vaAuto, ParseInteger);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.AutoIncrementValue));
          end
          else if ((Nodes.TableOptionsNodes.CollateValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
          begin
            Nodes.TableOptionsNodes.CollateValue := ParseValue(WordIndices(kiCOLLATE), vaAuto, ParseIdent);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.CollateValue));
          end
          else if ((Nodes.TableOptionsNodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARACTER)) then
          begin
            Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseIdent);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.CharacterSetValue));
          end
          else if ((Nodes.TableOptionsNodes.CollateValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOLLATE)) then
          begin
            Nodes.TableOptionsNodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaAuto, ParseIdent);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.CollateValue));
          end
          else if ((Nodes.TableOptionsNodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
          begin
            Nodes.TableOptionsNodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.CommentValue));
          end
          else if ((Nodes.TableOptionsNodes.ConnectionValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCONNECTION)) then
          begin
            Nodes.TableOptionsNodes.ConnectionValue := ParseValue(kiCONNECTION, vaAuto, ParseString);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.ConnectionValue));
          end
          else if ((Nodes.TableOptionsNodes.DataDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDATA)) then
          begin
            Nodes.TableOptionsNodes.DataDirectoryValue := ParseValue(WordIndices(kiDATA, kiDIRECTORY), vaAuto, ParseString);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.DataDirectoryValue));
          end
          else if ((Nodes.TableOptionsNodes.DelayKeyWriteValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDELAY_KEY_WRITE)) then
          begin
            Nodes.TableOptionsNodes.DelayKeyWriteValue := ParseValue(kiDELAY_KEY_WRITE, vaAuto, ParseInteger);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.DelayKeyWriteValue));
          end
          else if ((Nodes.TableOptionsNodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENGINE)) then
          begin
            Nodes.TableOptionsNodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseIdent);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.EngineValue));
          end
          else if ((Nodes.TableOptionsNodes.IndexDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX)) then
          begin
            Nodes.TableOptionsNodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.IndexDirectoryValue));
          end
          else if ((Nodes.TableOptionsNodes.InsertMethodValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINSERT_METHOD)) then
          begin
            Nodes.TableOptionsNodes.InsertMethodValue := ParseValue(kiINSERT_METHOD, vaAuto, ParseIdent);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.InsertMethodValue));
          end
          else if ((Nodes.TableOptionsNodes.KeyBlockSizeValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY_BLOCK_SIZE)) then
          begin
            Nodes.TableOptionsNodes.KeyBlockSizeValue := ParseValue(kiKEY_BLOCK_SIZE, vaAuto, ParseInteger);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.KeyBlockSizeValue));
          end
          else if ((Nodes.TableOptionsNodes.MaxRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_ROWS)) then
          begin
            Nodes.TableOptionsNodes.MaxRowsValue := ParseValue(kiMAX_ROWS, vaAuto, ParseInteger);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.MaxRowsValue));
          end
          else if ((Nodes.TableOptionsNodes.MinRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMIN_ROWS)) then
          begin
            Nodes.TableOptionsNodes.MinRowsValue := ParseValue(kiMIN_ROWS, vaAuto, ParseInteger);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.MinRowsValue));
          end
          else if ((Nodes.TableOptionsNodes.PackKeysValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPACK_KEYS)) then
          begin
            Nodes.TableOptionsNodes.PackKeysValue := ParseValue(kiPACK_KEYS, vaAuto, ParseExpr);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.PackKeysValue));
          end
          else if ((Nodes.TableOptionsNodes.PasswordValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPASSWORD)) then
          begin
            Nodes.TableOptionsNodes.PasswordValue := ParseValue(kiPASSWORD, vaAuto, ParseString);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.PasswordValue));
          end
          else if ((Nodes.TableOptionsNodes.RowFormatValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiROW_FORMAT)) then
          begin
            Nodes.TableOptionsNodes.RowFormatValue := ParseValue(kiROW_FORMAT, vaAuto, ParseIdent);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.RowFormatValue));
          end
          else if ((Nodes.TableOptionsNodes.StatsAutoRecalcValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTATS_AUTO_RECALC)) then
          begin
            if (NextToken[1] = 0) then
              SetError(PE_IncompleteStmt)
            else if (TokenPtr(NextToken[1])^.TokenType = ttInteger) then
              Nodes.TableOptionsNodes.StatsAutoRecalcValue := ParseValue(kiSTATS_AUTO_RECALC, vaAuto, ParseInteger)
            else
              Nodes.TableOptionsNodes.StatsAutoRecalcValue := ParseValue(kiSTATS_AUTO_RECALC, vaAuto, WordIndices(kiDEFAULT));
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.StatsAutoRecalcValue));
          end
          else if ((Nodes.TableOptionsNodes.StatsPersistentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTATS_PERSISTENT)) then
          begin
            if (NextToken[1] = 0) then
              SetError(PE_IncompleteStmt)
            else if (TokenPtr(NextToken[1])^.TokenType = ttInteger) then
              Nodes.TableOptionsNodes.StatsPersistentValue := ParseValue(kiSTATS_PERSISTENT, vaAuto, ParseInteger)
            else
              Nodes.TableOptionsNodes.StatsPersistentValue := ParseValue(kiSTATS_PERSISTENT, vaAuto, WordIndices(kiDEFAULT));
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.StatsPersistentValue));
          end
          else if ((Nodes.TableOptionsNodes.UnionList = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUNION)) then
          begin
            Nodes.TableOptionsNodes.UnionList := ParseAlterTableStmtUnion();
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.UnionList));
          end
          else
            Found := False;

          if (Found and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttComma)) then
            TableOptions.Add(Pointer(ApplyCurrentToken()));
        end;

        Nodes.TableOptionList := TList.Create(Self, ListNodes, TableOptions.Count, TIntegerArray(TableOptions.List));

        TableOptions.Free();
      end;

      if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
      begin
        Nodes.PartitionOption.PartitionByTag := ParseTag(kiPARTITION, kiBY);

        if (not Error) then
          if (CurrentToken = 0) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.KeywordIndex = kiHASH) then
            Nodes.PartitionOption.PartitionKindTag := ParseTag(kiHASH)
          else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLINEAR) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiHASH)) then
            Nodes.PartitionOption.PartitionKindTag := ParseTag(kiLINEAR, kiHASH)
          else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLINEAR) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
            Nodes.PartitionOption.PartitionKindTag := ParseTag(kiLINEAR, kiKEY)
          else if (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY) then
            Nodes.PartitionOption.PartitionKindTag := ParseTag(kiKEY)
          else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIST) then
            Nodes.PartitionOption.PartitionKindTag := ParseTag(kiLIST)
          else if (TokenPtr(CurrentToken)^.KeywordIndex = kiRANGE) then
            Nodes.PartitionOption.PartitionKindTag := ParseTag(kiRANGE)
          else
            SetError(PE_UnexpectedToken, CurrentToken);

        if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
          Nodes.PartitionOption.PartitionAlgorithmValue := ParseValue(kiALGORITHM, vaAuto, ParseInteger);

        if (not Error) then
          Nodes.PartitionOption.PartitionExprList := ParseList(True, ParseExpr);

        if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITIONS)) then
          Nodes.PartitionOption.PartitionsValue := ParseValue(kiPARTITIONS, vaNo, ParseInteger);

        if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSUBPARTITION)) then
        begin
          Nodes.PartitionOption.SubPartitionByTag := ParseTag(kiSUBPARTITION, kiBY);

          if (not Error) then
            if (CurrentToken = 0) then
              SetError(PE_IncompleteStmt)
            else if (TokenPtr(CurrentToken)^.KeywordIndex = kiHASH) then
              Nodes.PartitionOption.SubPartitionKindTag := ParseTag(kiHASH)
            else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLINEAR) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiHASH)) then
              Nodes.PartitionOption.SubPartitionKindTag := ParseTag(kiLINEAR, kiHASH)
            else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLINEAR) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
              Nodes.PartitionOption.SubPartitionKindTag := ParseTag(kiLINEAR, kiKEY)
            else if (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY) then
              Nodes.PartitionOption.SubPartitionKindTag := ParseTag(kiKEY)
            else
              SetError(PE_UnexpectedToken, CurrentToken);

          if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
            Nodes.PartitionOption.SubPartitionAlgorithmValue := ParseValue(kiALGORITHM, vaAuto, ParseInteger);

          if (not Error) then
            Nodes.PartitionOption.SubPartitionExprList := ParseList(True, ParseExpr);

          if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSUBPARTITIONS)) then
            Nodes.PartitionOption.SubPartitionsValue := ParseValue(kiSUBPARTITIONS, vaNo, ParseInteger);
        end;
      end;

      if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
        Nodes.PartitionDefinitionList := ParseList(True, ParsePartition);

      if (not Error and (CurrentToken > 0)) then
        if (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE) then
          Nodes.IgnoreReplaceTag := ParseTag(kiIGNORE)
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREPLACE) then
          Nodes.IgnoreReplaceTag := ParseTag(kiREPLACE);

      if (not Error) then
        if ((CurrentToken = 0) and (Nodes.IgnoreReplaceTag > 0)) then
          SetError(PE_IncompleteStmt)
        else if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAS)) then
          Nodes.AsTag := ParseTag(kiAS);

      if (not Error) then
        if ((CurrentToken = 0) and ((Nodes.IgnoreReplaceTag > 0) or (Nodes.AsTag > 0))) then
          SetError(PE_IncompleteStmt)
        else if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSELECT)) then
          Nodes.SelectStmt := ParseSelectStmt()
        else
          SetError(PE_UnexpectedToken, CurrentToken);

      if (not Error and (Nodes.OpenBracketToken = 0) and (Nodes.SelectStmt = 0)) then
        SetError(PE_IncompleteStmt);
    end;

  Result := TCreateTableStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseCreateTableStmtDefinition(): TOffset;
begin
  Result := ParseCreateTableStmtDefinition(False);
end;

function TCustomSQLParser.ParseCreateTableStmtDefinition(const AlterTableStmt: Boolean): TOffset;
var
  Index: Integer;
  SpecificationType: (stUnknown, stColumn, stIndex, stForeignKey, stPartition);
begin
  if (not AlterTableStmt) then
    Index := 0
  else
    Index := 1;

  Result := 0;
  SpecificationType := stUnknown;

  if (NextToken[Index + 1] = 0) then
    SetError(PE_IncompleteStmt)
  else if ((TokenPtr(NextToken[Index + 1])^.KeywordIndex = kiKEY)
    or (TokenPtr(NextToken[Index + 1])^.KeywordIndex = kiFULLTEXT)
    or (TokenPtr(NextToken[Index + 1])^.KeywordIndex = kiINDEX)
    or (TokenPtr(NextToken[Index + 1])^.KeywordIndex = kiPRIMARY)
    or (TokenPtr(NextToken[Index + 1])^.KeywordIndex = kiUNIQUE)
    or (TokenPtr(NextToken[Index + 1])^.KeywordIndex = kiSPATIAL)) then
    SpecificationType := stIndex
  else if (TokenPtr(NextToken[Index + 1])^.KeywordIndex = kiFOREIGN) then
    SpecificationType := stForeignKey
  else if (TokenPtr(NextToken[Index + 1])^.KeywordIndex = kiCONSTRAINT) then
  begin
    if (NextToken[Index + 2] = 0) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(NextToken[Index + 2])^.TokenType <> ttString) and ((TokenPtr(NextToken[Index + 2])^.TokenType <> ttIdentifier) or (TokenPtr(NextToken[Index + 2])^.KeywordIndex >= 0))) then
      if ((TokenPtr(NextToken[Index + 2])^.KeywordIndex = kiPRIMARY) or (TokenPtr(NextToken[Index + 2])^.KeywordIndex = kiUNIQUE)) then
        SpecificationType := stIndex
      else if (TokenPtr(NextToken[Index + 2])^.KeywordIndex = kiFOREIGN) then
        SpecificationType := stForeignKey
      else
        SetError(PE_UnexpectedToken, CurrentToken)
    else
      if (NextToken[Index + 3] = 0) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index + 3])^.KeywordIndex = kiPRIMARY) or (TokenPtr(NextToken[Index + 3])^.KeywordIndex = kiUNIQUE)) then
        SpecificationType := stIndex
      else if (TokenPtr(NextToken[Index + 3])^.KeywordIndex = kiFOREIGN) then
        SpecificationType := stForeignKey
      else
        SetError(PE_UnexpectedToken, CurrentToken);
  end
  else if (TokenPtr(NextToken[Index + 1])^.KeywordIndex = kiPARTITION) then
    SpecificationType := stPartition
  else
    SpecificationType := stColumn;

  if (not Error) then
    case (SpecificationType) of
      stColumn:
        if (not AlterTableStmt) then
          Result := ParseColumn(caNone)
        else
          Result := ParseColumn(caAdd);
      stIndex: Result := ParseIndex(AlterTableStmt);
      stForeignKey: Result := ParseForeignKey(AlterTableStmt);
      stPartition: Result := ParsePartition(AlterTableStmt);
    end;
end;

function TCustomSQLParser.ParseCreateTableStmtReferences(): TOffset;
var
  Nodes: TCreateTableStmt.TReference.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ReferencesTag := ParseTag(kiREFERENCES);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  if (not Error) then
    Nodes.IndicesList := ParseList(True, ParseIndexColumn);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMATCH)) then
    if ((NextToken[1] > 0) and ((TokenPtr(NextToken[1])^.KeywordIndex = kiFULL) or (TokenPtr(NextToken[1])^.KeywordIndex = kiPARTIAL) or (TokenPtr(NextToken[1])^.KeywordIndex = kiSIMPLE))) then
      Nodes.MatchValue := ParseValue(kiMATCH, vaNo, ParseIdent);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiDELETE)) then
    if (NextToken[2] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[2])^.KeywordIndex = kiRESTRICT) then
      Nodes.OnDeleteValue := ParseValue(WordIndices(kiON, kiUPDATE), vaNo, kiRESTRICT)
    else if (TokenPtr(NextToken[2])^.KeywordIndex = kiCASCADE) then
      Nodes.OnDeleteValue := ParseValue(WordIndices(kiON, kiUPDATE), vaNo, kiCASCADE)
    else if (TokenPtr(NextToken[2])^.KeywordIndex = kiSET) then
      Nodes.OnDeleteValue := ParseValue(WordIndices(kiON, kiDELETE), vaNo, kiSET, kiNULL)
    else if (TokenPtr(NextToken[2])^.KeywordIndex = kiNO) then
      Nodes.OnDeleteValue := ParseValue(WordIndices(kiON, kiDELETE), vaNo, kiNO, kiACTION)
    else
      SetError(PE_UnexpectedToken, CurrentToken);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiUPDATE)) then
    if (NextToken[2] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[2])^.KeywordIndex = kiRESTRICT) then
      Nodes.OnUpdateValue := ParseValue(WordIndices(kiON, kiUPDATE), vaNo, kiRESTRICT)
    else if (TokenPtr(NextToken[2])^.KeywordIndex = kiCASCADE) then
      Nodes.OnUpdateValue := ParseValue(WordIndices(kiON, kiUPDATE), vaNo, kiCASCADE)
    else if (TokenPtr(NextToken[2])^.KeywordIndex = kiSET) then
      Nodes.OnUpdateValue := ParseValue(WordIndices(kiON, kiUPDATE), vaNo, kiSET, kiNULL)
    else if (TokenPtr(NextToken[2])^.KeywordIndex = kiNO) then
      Nodes.OnUpdateValue := ParseValue(WordIndices(kiON, kiUPDATE), vaNo, kiNO, kiACTION)
    else
      SetError(PE_UnexpectedToken, CurrentToken);

  Result := TCreateTableStmt.TReference.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseCreateTriggerStmt(): TOffset;
var
  Nodes: TCreateTriggerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    Nodes.TriggerTag := ParseTag(kiTRIGGER);

  if (not Error) then
    Nodes.TriggerIdent := ParseDbIdent(ditTrigger);

  if (not Error) then
    if ((CurrentToken = 0) or (NextToken[1] = 0)) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(NextToken[1])^.KeywordIndex <> kiINSERT)
      and (TokenPtr(NextToken[1])^.KeywordIndex <> kiUPDATE)
      and (TokenPtr(NextToken[1])^.KeywordIndex <> kiDELETE)) then
      SetError(PE_UnexpectedToken, NextToken[1])
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiBEFORE) then
      Nodes.ActionValue := ParseValue(kiBEFORE, vaNo, ParseKeyword)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiAFTER) then
      Nodes.ActionValue := ParseValue(kiAFTER, vaNo, ParseKeyword)
    else
      SetError(PE_UnexpectedToken, CurrentToken);

  if (not Error) then
    Nodes.OnTag := ParseTag(kiON);

  if (not Error) then
    Nodes.TableIdentNode := ParseDbIdent(ditTable);

  if (not Error) then
    Nodes.ForEachRowTag := ParseTag(kiFOR, kiEACH, kiROW);

  if (not Error) then
    Nodes.Body := ParseStmt(True);

  Result := TCreateTriggerStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseCreateViewStmt(): TOffset;
var
  Nodes: TCreateViewStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiOR)) then
    Nodes.OrReplaceTag := ParseTag(kiOR, kiREPLACE);

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
    Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaYes, ParseKeyword);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL)) then
    if (NextToken[2] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[2])^.KeywordIndex = kiDEFINER) then
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiDEFINER)
    else
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiINVOKER);

  if (not Error) then
    Nodes.ViewTag := ParseTag(kiVIEW);

  if (not Error) then
    Nodes.IdentNode := ParseDbIdent(ditView);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.Columns := ParseList(True, ParseColumnIdent);

  if (not Error) then
    Nodes.AsTag := ParseTag(kiAS);

  if (not Error) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiSELECT) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      Nodes.SelectStmt := ParseSelectStmt();

      if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
        if (NextToken[1] = 0) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(NextToken[1])^.KeywordIndex = kiCASCADED) then
          Nodes.OptionTag := ParseTag(kiWITH, kiCASCADED, kiCHECK, kiOPTION)
        else if (TokenPtr(NextToken[1])^.KeywordIndex = kiLOCAL) then
          Nodes.OptionTag := ParseTag(kiWITH, kiLOCAL, kiCHECK, kiOPTION)
        else
          Nodes.OptionTag := ParseTag(kiWITH, kiCHECK, kiOPTION);
    end;

  Result := TCreateViewStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseDataType(): TOffset;
var
  IdentString: string;
  Nodes: TDataType.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttIdentifier) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Nodes.IdentToken := ApplyCurrentToken();

  if (not Error) then
  begin
    IdentString := UpperCase(TokenPtr(Nodes.IdentToken)^.AsString);

    if ((IdentString <> 'BIT')
      and (IdentString <> 'TINYINT')
      and (IdentString <> 'SMALLINT')
      and (IdentString <> 'MEDIUMINT')
      and (IdentString <> 'INT')
      and (IdentString <> 'INTEGER')
      and (IdentString <> 'BIGINT')
      and (IdentString <> 'REAL')
      and (IdentString <> 'DOUBLE')
      and (IdentString <> 'FLOAT')
      and (IdentString <> 'DECIMAL')
      and (IdentString <> 'NUMERIC')
      and (IdentString <> 'DATE')
      and (IdentString <> 'TIME')
      and (IdentString <> 'TIMESTAMP')
      and (IdentString <> 'DATETIME')
      and (IdentString <> 'YEAR')
      and (IdentString <> 'CHAR')
      and (IdentString <> 'VARCHAR')
      and (IdentString <> 'BINARY')
      and (IdentString <> 'VARBINARY')
      and (IdentString <> 'TINYBLOB')
      and (IdentString <> 'BLOB')
      and (IdentString <> 'MEDIUMBLOB')
      and (IdentString <> 'LONGBLOB')
      and (IdentString <> 'TINYTEXT')
      and (IdentString <> 'TEXT')
      and (IdentString <> 'MEDIUMTEXT')
      and (IdentString <> 'LONGTEXT')
      and (IdentString <> 'ENUM')
      and (IdentString <> 'SET')) then
      SetError(PE_UnexpectedToken, CurrentToken);

      if (not Error) then
        if ((IdentString = 'BIT')
          or (IdentString = 'TINYINT')
          or (IdentString = 'SMALLINT')
          or (IdentString = 'MEDIUMINT')
          or (IdentString = 'INT')
          or (IdentString = 'INTEGER')
          or (IdentString = 'BIGINT')
          or (IdentString = 'REAL')
          or (IdentString = 'DOUBLE')
          or (IdentString = 'FLOAT')
          or (IdentString = 'DECIMAL')
          or (IdentString = 'NUMERIC')
          or (IdentString = 'CHAR')
          or (IdentString = 'VARCHAR')
          or (IdentString = 'BINARY')
          or (IdentString = 'VARBINARY')) then
        begin
          if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
          begin
            Nodes.OpenBracketToken := ApplyCurrentToken();

            if (CurrentToken = 0) then
              SetError(PE_IncompleteStmt)
            else if (TokenPtr(CurrentToken)^.TokenType <> ttInteger) then
              SetError(PE_UnexpectedToken, CurrentToken)
            else
              Nodes.LengthToken := ApplyCurrentToken();

            if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttComma)
              and ((IdentString = 'REAL')
              or (IdentString = 'DOUBLE')
              or (IdentString = 'FLOAT')
              or (IdentString = 'DECIMAL')
              or (IdentString = 'NUMERIC'))) then
            begin
              Nodes.CommaToken := ApplyCurrentToken();

              if (CurrentToken = 0) then
                SetError(PE_IncompleteStmt)
              else if (TokenPtr(CurrentToken)^.TokenType <> ttInteger) then
                SetError(PE_UnexpectedToken, CurrentToken)
              else
                Nodes.DecimalsToken := ApplyCurrentToken();
            end;

            if (not Error) then
              if (CurrentToken = 0) then
                SetError(PE_IncompleteStmt)
              else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
                SetError(PE_UnexpectedToken, CurrentToken)
              else
                Nodes.CloseBracketToken := ApplyCurrentToken();
          end;
        end
        else if ((IdentString = 'ENUM')
          or (IdentString = 'SET')) then
        begin
          if (not Error) then
            Nodes.StringValuesNode := ParseList(True, ParseString);
        end;

    if (not Error) then
    begin
      if ((Nodes.LengthToken = 0)
        and ((IdentString = 'VARCHAR')
        or (IdentString = 'VARBINARY'))) then
          if (CurrentToken = 0) then
            SetError(PE_IncompleteStmt)
          else
            SetError(PE_UnexpectedToken, CurrentToken);

      if ((Nodes.DecimalsToken = 0)
        and ((IdentString = 'REAL')
        or (IdentString = 'DOUBLE')
        or (IdentString = 'FLOAT'))) then
          if (CurrentToken = 0) then
            SetError(PE_IncompleteStmt)
          else
            SetError(PE_UnexpectedToken, CurrentToken);
    end;
  end;

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUNSIGNED)
    and ((IdentString = 'TINYINT')
    or (IdentString = 'SMALLINT')
    or (IdentString = 'MEDIUMINT')
    or (IdentString = 'INT')
    or (IdentString = 'INTEGER')
    or (IdentString = 'BIGINT')
    or (IdentString = 'REAL')
    or (IdentString = 'DOUBLE')
    or (IdentString = 'FLOAT')
    or (IdentString = 'DECIMAL')
    or (IdentString = 'NUMERIC'))) then
    Nodes.UnsignedTag := ParseTag(kiUNSIGNED);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiZEROFILL)
    and ((IdentString = 'TINYINT')
    or (IdentString = 'SMALLINT')
    or (IdentString = 'MEDIUMINT')
    or (IdentString = 'INT')
    or (IdentString = 'INTEGER')
    or (IdentString = 'BIGINT')
    or (IdentString = 'REAL')
    or (IdentString = 'DOUBLE')
    or (IdentString = 'FLOAT')
    or (IdentString = 'DECIMAL')
    or (IdentString = 'NUMERIC'))) then
    Nodes.ZerofillTag := ParseTag(kiZEROFILL);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiBINARY)
    and ((IdentString = 'TINYTEXT')
    or (IdentString = 'TEXT')
    or (IdentString = 'MEDIUMTEXT')
    or (IdentString = 'LONGTEXT'))) then
    Nodes.BinaryTag := ParseTag(kiBINARY);

  if (not Error
    and ((IdentString = 'CHAR')
    or (IdentString = 'VARCHAR')
    or (IdentString = 'TINYTEXT')
    or (IdentString = 'TEXT')
    or (IdentString = 'MEDIUMTEXT')
    or (IdentString = 'LONGTEXT')
    or (IdentString = 'ENUM')
    or (IdentString = 'SET'))) then
    begin
      if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
      begin
        Nodes.CharacterSetTag := ParseTag(kiCHARACTER, kiSET);

        if (not Error) then
          if (CurrentToken = 0) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.TokenType <> ttIdentifier) then
            SetError(PE_UnexpectedToken, CurrentToken)
          else
            Nodes.CharacterSetValueToken := ApplyCurrentToken();
      end;

      if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
      begin
        Nodes.CollateTag := ParseTag(kiCOLLATE);

        if (not Error) then
          if (CurrentToken = 0) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.TokenType <> ttIdentifier) then
            SetError(PE_UnexpectedToken, CurrentToken)
          else
            Nodes.CollateValueToken := ApplyCurrentToken();
      end;
    end;

  Result := TDataType.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseDbIdent(const ADbIdentType: TDbIdentType): TOffset;
var
  Dot: TOffset;
  Prefix: TOffset;
begin
  Result := 0;
  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents) or (TokenPtr(CurrentToken)^.KeywordIndex >= 0)) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Result := TDbIdent.Create(Self, ApplyCurrentToken(), ADbIdentType);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.OperatorType = otDot) and (ADbIdentType <> ditDatabase)) then
  begin
    Dot := ApplyCurrentToken();

    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      Prefix := Result;

      Result := TDbIdent.Create(Self, ApplyCurrentToken(), ADbIdentType);
      PDbIdent(NodePtr(Result))^.AddPrefix(Prefix, Dot);
    end;
  end;

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.OperatorType = otDot) and (ADbIdentType in [ditIndex, ditColumn, ditAllFields, ditPartition])) then
  begin
    Dot := ApplyCurrentToken();

    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      Prefix := Result;

      Result := TDbIdent.Create(Self, ApplyCurrentToken(), ADbIdentType);
      PDbIdent(NodePtr(Result))^.AddPrefix(Prefix, Dot);
    end;
  end;
end;

function TCustomSQLParser.ParseDefinerValue(): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(kiDEFINER);

  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
  begin
    TokenPtr(CurrentToken)^.FOperatorType := otAssign;
    Nodes.AssignToken := ApplyCurrentToken();

    Nodes.ValueNode := ParseUser();
  end;

  Result := TValue.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseDeleteStmt(): TOffset;
var
  Nodes: TDeleteStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DeleteTag := ParseTag(kiDELETE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLOW_PRIORITY)) then
    Nodes.LowPriorityTag := ParseTag(kiLOW_PRIORITY);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiQUICK)) then
    Nodes.QuickTag := ParseTag(kiQUICK);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE)) then
    Nodes.IgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    Nodes.FromTag := ParseTag(kiFROM);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
  begin
    Nodes.PartitionTag := ParseTag(kiPARTITION);

    if (not Error) then
      Nodes.PartitionList := ParseList(True, ParsePartitionIdent);
  end;

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE)) then
    Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiORDER)) then
    Nodes.OrderByValue := ParseValue(WordIndices(kiORDER, kiBY), vaNo, ParseColumnNames);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLIMIT)) then
    Nodes.LimitValue := ParseValue(kiLIMIT, vaNo, ParseInteger);

  Result := TDeleteStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseDoStmt(): TOffset;
var
  Nodes: TDoStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DoTag := ParseTag(kiDO);

  if (not Error) then
    Nodes.ExprList := ParseList(True, ParseExpr);

  Result := TDoStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseDropDatabaseStmt(): TOffset;
var
  Nodes: TDropDatabaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSCHEMA)) then
      Nodes.DatabaseTag := ParseTag(kiSCHEMA)
    else
      Nodes.DatabaseTag := ParseTag(kiDATABASE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.DatabaseIdent := ParseDbIdent(ditDatabase);

  Result := TDropDatabaseStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseDropEventStmt(): TOffset;
var
  Nodes: TDropEventStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    Nodes.EventTag := ParseTag(kiEVENT);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.EventIdent := ParseDbIdent(ditEvent);

  Result := TDropEventStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseDropIndexStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TDropIndexStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    Nodes.IndexTag := ParseTag(kiINDEX);

  if (not Error) then
    Nodes.IndexIdent := ParseDbIdent(ditIndex);

  Found := True;
  while (not Error and Found and (CurrentToken > 0)) do
    if ((Nodes.AlgorithmValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
      Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaAuto, WordIndices(kiDEFAULT, kiINPLACE, kiCOPY))
    else if ((Nodes.LockValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCK)) then
      Nodes.LockValue := ParseValue(kiLOCK, vaAuto, WordIndices(kiDEFAULT, kiNONE, kiSHARED, kiEXCLUSIVE))
    else
      Found := False;

  Result := TDropIndexStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseDropRoutineStmt(const ARoutineType: TRoutineType): TOffset;
var
  Nodes: TDropRoutineStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.RoutineTag := ParseTag(kiFUNCTION)
    else
      Nodes.RoutineTag := ParseTag(kiPROCEDURE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.RoutineIdent := ParseDbIdent(ditFunction)
    else
      Nodes.RoutineIdent := ParseDbIdent(ditProcedure);

  Result := TDropRoutineStmt.Create(Self, ARoutineType, Nodes);
end;

function TCustomSQLParser.ParseDropServerStmt(): TOffset;
var
  Nodes: TDropServerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    Nodes.ServerTag := ParseTag(kiSERVER);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.ServerIdent := ParseDbIdent(ditServer);

  Result := TDropServerStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseDropStmt(): TOffset;
var
  Index: Integer;
begin
  Result := 0;
  Index := 1;

  if (not Error and (NextToken[Index] > 0) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiTEMPORARY)) then
    Inc(Index);

  if (not Error) then
    if (NextToken[Index] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiDATABASE) then
      Result := ParseDropDatabaseStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiEVENT) then
      Result := ParseDropEventStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiFUNCTION) then
      Result := ParseDropRoutineStmt(rtFunction)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiINDEX) then
      Result := ParseDropIndexStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiPROCEDURE) then
      Result := ParseDropRoutineStmt(rtProcedure)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiSERVER) then
      Result := ParseDropServerStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiTABLE) then
      Result := ParseDropTableStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiTRIGGER) then
      Result := ParseDropTriggerStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiVIEW) then
      Result := ParseDropViewStmt()
    else
      SetError(PE_UnexpectedToken, NextToken[Index]);
end;

function TCustomSQLParser.ParseDropTableStmt(): TOffset;
var
  Nodes: TDropTableStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.TableIdentList := ParseList(False, ParseTableIdent);

  if (not Error and (CurrentToken > 0)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiRESTRICT) then
      Nodes.RestrictCascadeTag := ParseTag(kiRESTRICT)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCASCADE) then
      Nodes.RestrictCascadeTag := ParseTag(kiCASCADE);

  Result := TDropTableStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseDropTriggerStmt(): TOffset;
var
  Nodes: TDropTriggerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    Nodes.TriggerTag := ParseTag(kiTrigger);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.TriggerIdent := ParseDbIdent(ditTrigger);

  Result := TDropTriggerStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseDropViewStmt(): TOffset;
var
  Nodes: TDropViewStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    Nodes.ViewTag := ParseTag(kiVIEW);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.ViewIdentList := ParseList(False, ParseViewIdent);

  if (not Error and (CurrentToken > 0)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiRESTRICT) then
      Nodes.RestrictCascadeTag := ParseTag(kiRESTRICT)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCASCADE) then
      Nodes.RestrictCascadeTag := ParseTag(kiCASCADE);

  Result := TDropViewStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseEventIdent(): TOffset;
begin
  Result := ParseDbIdent(ditEvent);
end;

function TCustomSQLParser.ParseExpr(): TOffset;
const
  MaxNodeCount = 100;
var
  NodeCount: Integer;
  Nodes: array[0 .. MaxNodeCount - 1] of TOffset;

  procedure AddNode(const ANode: TOffset; const Apply: Boolean = True);
  begin
    if (NodeCount = MaxNodeCount) then
      raise Exception.CreateFmt(STooManyTokensInExpr, [NodeCount]);

    Nodes[NodeCount] := ANode;
    Inc(NodeCount);
    if (Apply) then
      ApplyCurrentToken();
  end;

var
  I: Integer;
  KeywordIndex: TWordList.TIndex;
  OperatorPrecedence: Integer;
begin
  NodeCount := 0;

  repeat
    KeywordIndex := TokenPtr(CurrentToken)^.KeywordIndex;
    if (KeywordIndex > 0) then
      if (KeywordIndex = kiBETWEEN) then
        TokenPtr(CurrentToken)^.FOperatorType := otBetween
      else if (KeywordIndex = kiBINARY) then
        TokenPtr(CurrentToken)^.FOperatorType := otBinary
      else if (KeywordIndex = kiCOLLATE) then
        TokenPtr(CurrentToken)^.FOperatorType := otCollate
      else if (KeywordIndex = kiEND) then
        break
      else if (KeywordIndex = kiIN) then
        TokenPtr(CurrentToken)^.FOperatorType := otIf
      else if (KeywordIndex = kiIN) then
        TokenPtr(CurrentToken)^.FOperatorType := otIn
      else if (KeywordIndex = kiINTERVAL) then
        TokenPtr(CurrentToken)^.FOperatorType := otInterval
      else if (KeywordIndex = kiSOUNDS) then
        TokenPtr(CurrentToken)^.FOperatorType := otSounds
      else if (KeywordIndex = kiTHEN) then
        TokenPtr(CurrentToken)^.FOperatorType := otThen
      else if (KeywordIndex = kiWHEN) then
        TokenPtr(CurrentToken)^.FOperatorType := otWhen;

    if (KeywordIndex = kiCASE) then
      AddNode(ParseCaseOp(), False)
    else
      case (TokenPtr(CurrentToken)^.TokenType) of
        ttUnknown,
        ttSpace,
        ttReturn:
          raise ERangeError.Create(SArgumentOutOfRange);
        ttComma,
        ttCloseBracket,
        ttDelimiter:
          SetError(PE_UnexpectedToken, CurrentToken);
        ttOpenBracket:
          if (NodeCount = 0) then
            AddNode(ParseSubArea(ParseExpr))
          else if (IsRange(NodePtr(Nodes[NodeCount - 1]))) then
            SetError(PE_UnexpectedToken, RangeNodePtr(Nodes[NodeCount - 1])^.FFirstToken)
          else if ((NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT)) then
            AddNode(ParseSubArea(ParseSelectStmt))
          else if (TokenPtr(Nodes[NodeCount - 1])^.OperatorType = otIn) then
            AddNode(ParseList(True, ParseExpr))
          else if (TokenPtr(Nodes[NodeCount - 1])^.OperatorType in [otInterval, otBinary, otCollate]) then
            AddNode(ParseList(True, ParsePartitionIdent))
          else
            AddNode(ParseList(True, ParseExpr));
        else
          if ((NodeCount = 0) or (IsToken(Nodes[NodeCount - 1]) and (TokenPtr(Nodes[NodeCount - 1])^.OperatorType <> otUnknown))) then
            // Operand
            if (TokenPtr(CurrentToken)^.KeywordIndex = kiNULL) then
            begin
              TokenPtr(CurrentToken)^.FUsageType := utConst;
              AddNode(CurrentToken);
            end
            else
              case (TokenPtr(CurrentToken)^.TokenType) of
                ttOperator:
                  if (TokenPtr(CurrentToken)^.OperatorType <> otMulti) then
                    SetError(PE_UnexpectedToken, CurrentToken)
                  else
                  begin
                    TokenPtr(CurrentToken)^.FTokenType := ttIdentifier;
                    TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
                    TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
                    AddNode(TDbIdent.Create(Self, CurrentToken, ditAllFields));
                  end;
                ttInteger,
                ttNumeric,
                ttString,
                ttDQIdentifier,
                ttCSString:
                  begin
                    TokenPtr(CurrentToken)^.FUsageType := utConst;
                    AddNode(CurrentToken);
                  end;
                ttVariable:
                  begin
                    TokenPtr(CurrentToken)^.FUsageType := utVariable;
                    AddNode(CurrentToken);
                  end;
                ttIdentifier,
                ttDBIdentifier,
                ttBRIdentifier,
                ttMySQLIdentifier:
                  if ((NextToken[1] = 0) or (TokenPtr(NextToken[1])^.TokenType <> ttOpenBracket)) then
                  begin
                    TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
                    AddNode(TDbIdent.Create(Self, CurrentToken, ditColumn));
                  end
                  else
                    AddNode(ParseFunction(), False);
              end
          else if ((NodeCount > 0) and (not IsToken(Nodes[NodeCount - 1]) or (TokenPtr(Nodes[NodeCount - 1])^.OperatorType = otUnknown))) then
            // Operator
            case (TokenPtr(CurrentToken)^.OperatorType) of
              otFunction_,
              otInterval,
              otBinary,
              otCollate,
              otNot1,
              otInvertBits,
              otDot,
              otBitXOR,
              otMulti,
              otDivision,
              otDiv,
              otMod,
              otMinus,
              otPlus,
              otShiftLeft,
              otShiftRight,
              otBitAND,
              otBitOR,
              otEqual,
              otNullSaveEqual,
              otGreaterEqual,
              otGreater,
              otLessEqual,
              otLess,
              otNotEqual,
              otIS,
              otSounds,
              otLike,
              otRegExp,
              otIn,
              otBetween,
              otNot2,
              otAnd,
              otXOr,
              otPipes,
              otOr:
                begin
                  TokenPtr(CurrentToken)^.FUsageType := utOperator;
                  AddNode(CurrentToken);
                end;
              otElse,
              otThen,
              otWhen: break;
              else
                SetError(PE_UnexpectedToken, CurrentToken);
            end
          else
            // Operand Prefix
            case (TokenPtr(CurrentToken)^.OperatorType) of
              otFunction_,
              otInterval,
              otBinary,
              otCollate,
              otNot1,
              otInvertBits:
                begin
                  TokenPtr(CurrentToken)^.FUsageType := utOperator;
                  AddNode(CurrentToken);
                end;
              otMinus:
                begin
                  TokenPtr(CurrentToken)^.FUsageType := utOperator;
                  TokenPtr(CurrentToken)^.FOperatorType := otUnaryMinus;
                  AddNode(CurrentToken);
                end;
              otPlus:
                begin
                  TokenPtr(CurrentToken)^.FUsageType := utOperator;
                  TokenPtr(CurrentToken)^.FOperatorType := otUnaryPlus;
                  AddNode(CurrentToken);
                end;
              otLike:
                if (not IsToken(Nodes[NodeCount - 1]) or (TokenPtr(Nodes[NodeCount - 1])^.OperatorType <> otSounds)) then
                  SetError(PE_UnexpectedToken, CurrentToken)
                else
                begin
                  TokenPtr(CurrentToken)^.FUsageType := utOperator;
                  AddNode(CurrentToken);
                end;
              else
                SetError(PE_UnexpectedToken, CurrentToken);
            end;
      end;

    if (CurrentToken = 0) then
      KeywordIndex := -1
    else
      KeywordIndex := TokenPtr(CurrentToken)^.KeywordIndex;
  until (Error
    or (CurrentToken = 0)
    or (TokenPtr(CurrentToken)^.TokenType in [ttComma, ttCloseBracket, ttDelimiter])
    or ((not IsToken(Nodes[NodeCount - 1]) or (TokenPtr(Nodes[NodeCount - 1])^.OperatorType = otUnknown))
      and not ((TokenPtr(CurrentToken)^.OperatorType <> otUnknown)
        or (KeywordIndex = kiBETWEEN)
        or (KeywordIndex = kiBINARY)
        or (KeywordIndex = kiCOLLATE)
        or (KeywordIndex = kiCASE)
        or (KeywordIndex = kiIN)
        or (KeywordIndex = kiINTERVAL)
        or (KeywordIndex = kiSOUNDS)
        or (KeywordIndex = kiTHEN)
        or (KeywordIndex = kiWHEN))));

  for OperatorPrecedence := 1 to MaxOperatorPrecedence do
  begin
    I := 0;
    while (not Error and (I < NodeCount - 1)) do
    begin
      if ((NodePtr(Nodes[I])^.FNodeType = ntToken) and (OperatorPrecedenceByOperatorType[TokenPtr(Nodes[I])^.OperatorType] = OperatorPrecedence)) then
        case (TokenPtr(Nodes[I])^.OperatorType) of
          otFunction_,
          otInterval,
          otBinary,
          otCollate:
            if (I >= NodeCount - 1) then
              if (CurrentToken = 0) then
                SetError(PE_IncompleteStmt)
              else
                SetError(PE_UnexpectedToken, CurrentToken)
            else if (not (NodePtr(Nodes[I + 1])^.FNodeType = ntList)) then
              SetError(PE_UnexpectedToken, StmtNodePtr(Nodes[I + 1])^.FFirstToken)
            else
            begin
              Nodes[I] := TFunction.Create(Self, Nodes[I], Nodes[I + 1]);
              Dec(NodeCount);
              Move(Nodes[I + 2], Nodes[I + 1], (NodeCount - I - 1) * SizeOf(Nodes[0]));
            end;
          otNot1,
          otUnaryMinus,
          otUnaryPlus,
          otInvertBits,
          otNot2:
            if (I >= NodeCount - 1) then
              SetError(PE_IncompleteStmt)
            else
            begin
              Nodes[I] := TUnaryOp.Create(Self, Nodes[I], Nodes[I + 1]);
              Dec(NodeCount);
              Move(Nodes[I + 2], Nodes[I + 1], (NodeCount - I - 1) * SizeOf(Nodes[0]));
            end;
          otDot:
            if (I >= NodeCount - 1) then
              SetError(PE_IncompleteStmt)
            else if (I = 0) then
              SetError(PE_UnexpectedToken, Nodes[I])
            else if ((NodePtr(Nodes[I - 1])^.NodeType <> ntDbIdent) or (PDbIdent(NodePtr(Nodes[I - 1]))^.FNodes.Prefix2 > 0)) then
              SetError(PE_UnexpectedToken, Nodes[I])
            else if (NodePtr(Nodes[I + 1])^.NodeType = ntDbIdent) then
            begin
              PDbIdent(NodePtr(Nodes[I + 1]))^.AddPrefix(Nodes[I - 1], Nodes[I]);
              Dec(NodeCount, 2);
              Move(Nodes[I + 1], Nodes[I - 1], (NodeCount - I + 1) * SizeOf(Nodes[0]));
              Dec(I);
            end
            else if (NodePtr(Nodes[I + 1])^.NodeType = ntFunction) then
            begin
              if ((PDbIdent(NodePtr(Nodes[I - 1]))^.FNodes.Prefix1 > 0) or (PFunction(NodePtr(Nodes[I + 1]))^.Ident^.NodeType <> ntDbIdent)) then
                SetError(PE_UnexpectedToken, Nodes[I + 1])
              else
              begin
                PDbIdent(PFunction(NodePtr(Nodes[I + 1]))^.Ident)^.AddPrefix(Nodes[I - 1], Nodes[I]);
                Dec(NodeCount, 2);
                Move(Nodes[I + 1], Nodes[I - 1], (NodeCount - I + 1) * SizeOf(Nodes[0]));
                Dec(I);
              end;
            end
            else
              SetError(PE_UnexpectedToken, Nodes[I + 1]);
          otBitXOR,
          otMulti,
          otDivision,
          otDiv,
          otMod,
          otMinus,
          otPlus,
          otShiftLeft,
          otShiftRight,
          otBitAND,
          otBitOR,
          otEqual,
          otNullSaveEqual,
          otGreaterEqual,
          otGreater,
          otLessEqual,
          otLess,
          otNotEqual,
          otIS,
          otLike,
          otRegExp,
          otIn,
          otAnd,
          otXOr,
          otPipes,
          otOr:
            if (I = 0) then
              SetError(PE_UnexpectedToken, Nodes[I])
            else if (I >= NodeCount - 1) then
              SetError(PE_IncompleteStmt)
            else
            begin
              Nodes[I - 1] := TBinaryOp.Create(Self, Nodes[I], Nodes[I - 1], Nodes[I + 1]);
              Dec(NodeCount, 2);
              Move(Nodes[I + 2], Nodes[I], (NodeCount - I) * SizeOf(Nodes[0]));
              Dec(I);
            end;
          otBetween:
            if (I + 3 >= NodeCount) then
              SetError(PE_IncompleteToken, Nodes[I])
            else if ((NodePtr(Nodes[I + 2])^.NodeType <> ntToken) or (TokenPtr(Nodes[I + 2])^.OperatorType <> otAnd)) then
              SetError(PE_UnexpectedToken, Nodes[I + 2])
            else
            begin
              Nodes[I + 3] := TBetweenOp.Create(Self, Nodes[I], Nodes[I + 2], Nodes[I - 1], Nodes[I + 1], Nodes[I + 3]);
              Dec(NodeCount, 4);
              Move(Nodes[I + 3], Nodes[I - 1], NodeCount - I);
              Dec(I);
            end;
          otSounds:
            if (NodeCount - 1 < I + 2) then
              SetError(PE_IncompleteToken, Nodes[I])
            else if ((NodePtr(Nodes[I + 1])^.NodeType <> ntToken) or (TokenPtr(Nodes[I + 1])^.OperatorType <> otLike)) then
              SetError(PE_UnexpectedToken, Nodes[I + 1])
            else
            begin
              Nodes[I + 2] := TSoundsLikeOp.Create(Self, Nodes[I], Nodes[I + 1], Nodes[I - 1], Nodes[I + 2]);
              Dec(NodeCount, 3);
              Move(Nodes[I + 2], Nodes[I - 1], NodeCount - I);
              Dec(I);
            end;
          else
            begin
              case (NodePtr(Nodes[I])^.FNodeType) of
                ntToken: SetError(PE_UnexpectedToken, Nodes[I]);
                ntRange: SetError(PE_UnexpectedToken, RangeNodePtr(Nodes[I])^.FFirstToken);
                else raise ERangeError.Create(SArgumentOutOfRange);
              end;
            end;
        end
      else
        Inc(I);
    end;
  end;

  if (not Error and (NodeCount > 1)) then
    SetError(PE_Unknown);
  if (Error or (NodeCount <> 1)) then
    Result := 0
  else
    Result := Nodes[0];
end;

function TCustomSQLParser.ParseExprList(): TOffset;
begin
  Result := ParseList(True, ParseExpr);
end;

function TCustomSQLParser.ParseForeignKey(const Add: Boolean = False): TOffset;
var
  Nodes: TForeignKey.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (Add) then
    Nodes.AddTag := ParseTag(kiADD);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCONSTRAINT)) then
  begin
    Nodes.ConstraintTag := ParseTag(kiCONSTRAINT);

    if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiPRIMARY) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiUNIQUE)) then
      Nodes.SymbolIdent := ApplyCurrentToken();
  end;

  if (not Error) then
    Nodes.KeyTag := ParseTag(kiFOREIGN, kiKEY);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket)) then
    Nodes.NameToken := ParseString();

  if (not Error) then
    Nodes.ColumnNameList := ParseList(True, ParseIndexColumn);

  Result := TForeignKey.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseFunction(): TOffset;
var
  Ident: TOffset;
  Arguments: TOffset;
begin
  TokenPtr(CurrentToken)^.FOperatorType := otFunction_;
  if ((FFunctions.Count = 0) or (FFunctions.IndexOf(TokenPtr(CurrentToken)^.FText.SQL, TokenPtr(CurrentToken)^.FText.Length) >= 0)) then
    Ident := ApplyCurrentToken(utFunction)
  else
    Ident := TDbIdent.Create(Self, ApplyCurrentToken(utDbIdent), ditFunction);

  Arguments := ParseList(True, ParseExpr);

  Result := TFunction.Create(Self, Ident, Arguments);
end;

function TCustomSQLParser.ParseFunctionParam(): TOffset;
var
  Nodes: TRoutineParam.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Nodes.IdentToken := ApplyCurrentToken();

  if (not Error) then
    Nodes.DataTypeNode := ParseDataType();

  Result := TRoutineParam.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseGroup(): TOffset;
var
  Expr: TOffset;
  Direction: TOffset;
begin
  Expr := ParseExpr();

  if (Error or (CurrentToken = 0) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiASC) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiDESC)) then
    Direction := 0
  else
    Direction := ApplyCurrentToken();

  Result := TSelectStmt.TGroup.Create(Self, Expr, Direction);
end;

function TCustomSQLParser.ParseIfStmt(): TOffset;
var
  Branches: array of TOffset;
  ListNodes: TList.TNodes;
  Nodes: TIfStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  SetLength(Branches, 1);

  Branches[0] := ParseIfStmtBranch();

  while (not Error) do
    if (CurrentToken = 0) then
      SetError(PE_IncompleteToken)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiELSEIF) then
    begin
      SetLength(Branches, Length(Branches) + 1);
      Branches[Length(Branches) - 1] := ParseIfStmtBranch();
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE) then
    begin
      SetLength(Branches, Length(Branches) + 1);
      Branches[Length(Branches) - 1] := ParseIfStmtBranch();
      break;
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEND) then
      break
    else
      SetError(PE_UnexpectedToken, CurrentToken);

  FillChar(ListNodes, SizeOf(ListNodes), 0);
  Nodes.BranchList := TList.Create(Self, ListNodes, Length(Branches), Branches);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiIF);

  Result := TIfStmt.Create(Self, Nodes);

  SetLength(Branches, 0);
end;

function TCustomSQLParser.ParseIfStmtBranch(): TOffset;
var
  Nodes: TIfStmt.TBranch.TNodes;
  SearchCondition: Boolean;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  SearchCondition := False;
  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIF) then
  begin
    Nodes.Tag := ParseTag(kiIF);
    SearchCondition := True;
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiELSEIF) then
  begin
    Nodes.Tag := ParseTag(kiIF);
    SearchCondition := True;
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE) then
    Nodes.Tag := ParseTag(kiIF)
  else
    SetError(PE_UnexpectedToken, CurrentToken);

  if (not Error and SearchCondition) then
  begin
    Nodes.SearchConditionExpr := ParseExpr();

    if (not Error) then
      Nodes.ThenTag := ParseTag(kiTHEN);
  end;

  if (not Error) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter);

  Result := TIfStmt.TBranch.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseIdent(): TOffset;
begin
  Result := 0;
  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttIdentifier) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Result := ApplyCurrentToken();
end;

function TCustomSQLParser.ParseTag(const KeywordIndex1: TWordList.TIndex; const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1; const KeywordIndex4: TWordList.TIndex = -1): TOffset;
var
  Nodes: TTag.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex1) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
  begin
    Nodes.KeywordToken1 := ApplyCurrentToken();

    if (KeywordIndex2 >= 0) then
    begin
      if (CurrentToken = 0) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex2) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
      begin
        Nodes.KeywordToken2 := ApplyCurrentToken();

        if (KeywordIndex3 >= 0) then
        begin
          if (CurrentToken = 0) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex3) then
            SetError(PE_UnexpectedToken, CurrentToken)
          else
          begin
            Nodes.KeywordToken3 := ApplyCurrentToken();

            if (KeywordIndex4 >= 0) then
            begin
              if (CurrentToken = 0) then
                SetError(PE_IncompleteStmt)
              else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex4) then
                SetError(PE_UnexpectedToken, CurrentToken)
              else
                Nodes.KeywordToken4 := ApplyCurrentToken();
            end;
          end;
        end;
      end;
    end;
  end;

  Result := TTag.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseIndex(const Add: Boolean = False): TOffset;
var
  Found: Boolean;
  Nodes: TIndex.TNodes;
  PrimaryKey: Boolean;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (Add) then
    Nodes.AddTag := ParseTag(kiADD);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCONSTRAINT)) then
  begin
    Nodes.ConstraintTag := ParseTag(kiCONSTRAINT);

    if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiPRIMARY) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiUNIQUE)) then
      Nodes.SymbolIdent := ApplyCurrentToken();
  end;

  PrimaryKey := False;
  if (not Error and (CurrentToken > 0)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiPRIMARY) then
    begin
      Nodes.IndexTag := ParseTag(kiPRIMARY, kiKEY);
      PrimaryKey := True;
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUNIQUE) then
      if ((NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiINDEX)) then
        Nodes.IndexTag := ParseTag(kiUNIQUE, kiINDEX)
      else if ((NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
        Nodes.IndexTag := ParseTag(kiUNIQUE, kiKEY)
      else
        Nodes.IndexTag := ParseTag(kiUNIQUE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFULLTEXT) then
      if (Nodes.ConstraintTag > 0) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else if ((NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiINDEX)) then
        Nodes.IndexTag := ParseTag(kiFULLTEXT, kiINDEX)
      else if ((NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
        Nodes.IndexTag := ParseTag(kiFULLTEXT, kiKEY)
      else
        Nodes.IndexTag := ParseTag(kiFULLTEXT)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSPATIAL) then
      if (Nodes.ConstraintTag > 0) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else if ((NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiINDEX)) then
        Nodes.IndexTag := ParseTag(kiSPATIAL, kiINDEX)
      else if ((NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
        Nodes.IndexTag := ParseTag(kiSPATIAL, kiKEY)
      else
        Nodes.IndexTag := ParseTag(kiSPATIAL);

  if (not PrimaryKey and not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiUSING) and (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket)) then
    Nodes.IndexIdent := ParseString();

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING)) then
    Nodes.IndexTypeValue := ParseValue(kiUSING, vaNo, WordIndices(kiBTREE, kiHASH));

  if (not Error) then
    Nodes.ColumnNameList := ParseList(True, ParseIndexColumn);

  Found := True;
  while (not Error and Found and (CurrentToken > 0)) do
    if ((Nodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString)
    else if ((Nodes.KeyBlockSizeValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY_BLOCK_SIZE)) then
      Nodes.KeyBlockSizeValue := ParseValue(kiKEY_BLOCK_SIZE, vaAuto, ParseInteger)
    else if ((Nodes.IndexTypeValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING)) then
      Nodes.IndexTypeValue := ParseValue(kiUSING, vaNo, WordIndices(kiBTREE, kiHASH))
    else if ((Nodes.ParserValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
      Nodes.ParserValue := ParseValue(WordIndices(kiWITH, kiPARSER), vaNo, ParseString)
    else
      Found := False;

  Result := TIndex.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseIndexColumn(): TOffset;
var
  Nodes: TIndexColumn.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error) then
    Nodes.IdentTag := ParseDbIdent(ditColumn);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
  begin
    Nodes.OpenBracketToken := ApplyCurrentToken();

    Nodes.LengthToken := ParseInteger();
    if (not Error) then
      if (CurrentToken = 0) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
        Nodes.CloseBracketToken := ApplyCurrentToken();
  end;

  if (not Error and (CurrentToken > 0)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiASC) then
      Nodes.SortTag := ParseTag(kiASC)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDESC) then
      Nodes.SortTag := ParseTag(kiDESC);

  Result := TIndexColumn.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseIndexHint(): TOffset;
var
  IndexHintKind: TSelectStmt.TTableFactor.TIndexHint.TIndexHintKind;
  IndexHintType: TSelectStmt.TTableFactor.TIndexHint.TIndexHintType;
begin
  Result := 0;
  IndexHintKind := ihkUnknown;

  IndexHintType := ihtUnknown;
  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUSE) then
  begin
    IndexHintType := ihtUse;
    ApplyCurrentToken();
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE) then
  begin
    IndexHintType := ihtIgnore;
    ApplyCurrentToken();
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFORCE) then
  begin
    IndexHintType := ihtForce;
    ApplyCurrentToken();
  end
  else
    SetError(PE_UnexpectedToken, CurrentToken);

  if (not Error) then
  begin
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex <> kiINDEX) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiKEY)) then
      SetError(PE_UnexpectedToken, CurrentToken);

    if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFOR)) then
    begin
      ApplyCurrentToken();

      if (CurrentToken = 0) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiJOIN) then
        begin
          IndexHintKind := ihkJoin;
          ApplyCurrentToken();
          ParseList(True, ParseIndexIdent);
        end
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiORDER) then
        if (CurrentToken = 0) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiBY) then
          SetError(PE_UnexpectedToken, CurrentToken)
        else
        begin
          IndexHintKind := ihkOrderBy;
          ApplyCurrentToken();
          ParseList(True, ParseIndexIdent);
        end
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiGROUP) then
      begin
        ApplyCurrentToken();
        if (CurrentToken = 0) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiBY) then
          SetError(PE_UnexpectedToken, CurrentToken)
        else
        begin
          IndexHintKind := ihkGroupBy;
          ApplyCurrentToken();
          ParseList(True, ParseIndexIdent);
        end;
      end
      else
        SetError(PE_UnexpectedToken, CurrentToken);

      if (not Error) then
        Result := TSelectStmt.TTableFactor.TIndexHint.Create(Self, IndexHintType, IndexHintKind);
    end;
  end;
end;

function TCustomSQLParser.ParseIndexIdent(): TOffset;
begin
  Result := ParseDbIdent(ditIndex);
end;

function TCustomSQLParser.ParseInsertStmt(): TOffset;
var
  Nodes: TInsertStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.InsertTag := ParseTag(kiINSERT);

  if (not Error and (CurrentToken > 0)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOW_PRIORITY) then
      Nodes.PriorityTag := ParseTag(kiLOW_PRIORITY)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDELAYED) then
      Nodes.PriorityTag := ParseTag(kiDELAYED)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiHIGH_PRIORITY) then
      Nodes.PriorityTag := ParseTag(kiHIGH_PRIORITY);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE)) then
    Nodes.IgnoreTag := ParseTag(kiIGNORE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINTO)) then
    Nodes.IntoTag := ParseTag(kiINTO);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
  begin
    Nodes.IntoTag := ParseTag(kiPARTITION);

    if (not Error) then
      Nodes.PartitionList := ParseList(True, ParsePartitionIdent);
  end;

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.ColumnList := ParseList(True, ParseColumnIdent);

  if (not Error) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiVALUE) then
      Nodes.ValuesTag := ParseTag(kiVALUE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiVALUES) then
      Nodes.ValuesTag := ParseTag(kiVALUES)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiSET) and (Nodes.ColumnList = 0)) then
      Nodes.SetTag := ParseTag(kiSET)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSELECT) then
      Nodes.SelectTag := ParseTag(kiSELECT)
    else
      SetError(PE_UnexpectedToken, CurrentToken);

  if (not Error) then
    if (Nodes.ValuesTag > 0) then
      Nodes.ValuesList := ParseList(False, ParseExprList)
    else if (Nodes.SetTag > 0) then
      Nodes.ValuesList := ParseList(False, ParseUpdatePair)
    else if (Nodes.SelectTag > 0) then
      Nodes.SelectStmt := ParseSelectStmt()
    else
      SetError(PE_Unknown);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON)) then
  begin
    Nodes.OnDuplicateKeyUpdateTag := ParseTag(kiON, kiDUPLICATE, kiKEY, kiUPDATE);

    Nodes.UpdateList := ParseList(False, ParseUpdatePair);
  end;

  Result := TInsertStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseInteger(): TOffset;
begin
  Result := 0;
  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttInteger) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Result := ApplyCurrentToken();
end;

function TCustomSQLParser.ParseInterval(): TOffset;
var
  Nodes: TSchedule.TInterval.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error) then
    Nodes.QuantityExp := ParseExpr();

  if (not Error) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex <> kiYEAR)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiQUARTER)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiMONTH)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiDAY)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiHOUR)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiMINUTE)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiWEEK)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiSECOND)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiYEAR_MONTH)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiDAY_HOUR)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiDAY_MINUTE)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiDAY_SECOND)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiHOUR_MINUTE)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiHOUR_SECOND)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiMINUTE_SECOND)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.UnitTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  Result := TSchedule.TInterval.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseIntervalList(): TSchedule.TIntervalList;
var
  Day: Boolean;
  DayHour: Boolean;
  DayMinute: Boolean;
  DaySecond: Boolean;
  Found: Boolean;
  Hour: Boolean;
  HourMinute: Boolean;
  HourSecond: Boolean;
  I: Integer;
  Index: Integer;
  Minute: Boolean;
  MinuteSecond: Boolean;
  Month: Boolean;
  Quarter: Boolean;
  Second: Boolean;
  Week: Boolean;
  Year: Boolean;
  YearMonth: Boolean;
begin
  for I := 0 to Length(Result) - 1 do
    Result[I] := 0;

  Day := False;
  DayHour := False;
  DayMinute := False;
  DaySecond := False;
  Hour := False;
  HourMinute := False;
  HourSecond := False;
  Minute := False;
  MinuteSecond := False;
  Month := False;
  Quarter := False;
  Second := False;
  Week := False;
  Year := False;
  YearMonth := False;
  Found := True;
  Index := 0;
  while (not Error and Found and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.OperatorType = otPlus)) do
    if (NextToken[1] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[1])^.KeywordIndex <> kiINTERVAL) then
      SetError(PE_UnexpectedToken, NextToken[1])
    else if (NextToken[2] = 0) then
      SetError(PE_IncompleteStmt)
    else if (not Year and (TokenPtr(NextToken[2])^.KeywordIndex = kiYEAR)) then
    begin
      Result[Index] := ParseIntervalListItem(kiYEAR);
      Inc(Index);
      Year := True;
      Found := True;
    end
    else if (not Quarter and (TokenPtr(NextToken[2])^.KeywordIndex = kiQUARTER)) then
    begin
      Result[Index] := ParseIntervalListItem(kiQUARTER);
      Inc(Index);
      Quarter := True;
      Found := True;
    end
    else if (not MONTH and (TokenPtr(NextToken[2])^.KeywordIndex = kiMONTH)) then
    begin
      Result[Index] := ParseIntervalListItem(kiMONTH);
      Inc(Index);
      Month := True;
      Found := True;
    end
    else if (not Day and (TokenPtr(NextToken[2])^.KeywordIndex = kiDAY)) then
    begin
      Result[Index] := ParseIntervalListItem(kiDAY);
      Inc(Index);
      Day := True;
      Found := True;
    end
    else if (not Hour and (TokenPtr(NextToken[2])^.KeywordIndex = kiHOUR)) then
    begin
      Result[Index] := ParseIntervalListItem(kiHOUR);
      Inc(Index);
      Hour := True;
      Found := True;
    end
    else if (not Minute and (TokenPtr(NextToken[2])^.KeywordIndex = kiMINUTE)) then
    begin
      Result[Index] := ParseIntervalListItem(kiMINUTE);
      Inc(Index);
      Minute := True;
      Found := True;
    end
    else if (not Week and (TokenPtr(NextToken[2])^.KeywordIndex = kiWEEK)) then
    begin
      Result[Index] := ParseIntervalListItem(kiWEEK);
      Inc(Index);
      Week := True;
      Found := True;
    end
    else if (not Second and (TokenPtr(NextToken[2])^.KeywordIndex = kiSECOND)) then
    begin
      Result[Index] := ParseIntervalListItem(kiSECOND);
      Inc(Index);
      Second := True;
      Found := True;
    end
    else if (not YearMonth and (TokenPtr(NextToken[2])^.KeywordIndex = kiYEAR_MONTH)) then
    begin
      Result[Index] := ParseIntervalListItem(kiYEAR_MONTH);
      Inc(Index);
      YearMonth := True;
      Found := True;
    end
    else if (not DayHour and (TokenPtr(NextToken[2])^.KeywordIndex = kiDAY_HOUR)) then
    begin
      Result[Index] := ParseIntervalListItem(kiDAY_HOUR);
      Inc(Index);
      DayHour := True;
      Found := True;
    end
    else if (not DayMinute and (TokenPtr(NextToken[2])^.KeywordIndex = kiDAY_MINUTE)) then
    begin
      Result[Index] := ParseIntervalListItem(kiDAY_MINUTE);
      Inc(Index);
      DayMinute := True;
      Found := True;
    end
    else if (not DaySecond and (TokenPtr(NextToken[2])^.KeywordIndex = kiDAY_SECOND)) then
    begin
      Result[Index] := ParseIntervalListItem(kiDAY_SECOND);
      Inc(Index);
      DaySecond := True;
      Found := True;
    end
    else if (not HourMinute and (TokenPtr(NextToken[2])^.KeywordIndex = kiHOUR_MINUTE)) then
    begin
      Result[Index] := ParseIntervalListItem(kiHOUR_MINUTE);
      Inc(Index);
      HourMinute := True;
      Found := True;
    end
    else if (not HourSecond and (TokenPtr(NextToken[2])^.KeywordIndex = kiHOUR_SECOND)) then
    begin
      Result[Index] := ParseIntervalListItem(kiHOUR_SECOND);
      Inc(Index);
      HourSecond := True;
      Found := True;
    end
    else if (not MinuteSecond and (TokenPtr(NextToken[2])^.KeywordIndex = kiMINUTE_SECOND)) then
    begin
      Result[Index] := ParseIntervalListItem(kiMINUTE_SECOND);
      Inc(Index);
      MinuteSecond := True;
      Found := True;
    end
    else
      Found := False;
end;

function TCustomSQLParser.ParseIntervalListItem(const KeywordIndex: TWordList.TIndex): TOffset;
var
  Nodes: TSchedule.TIntervalListItem.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.OperatorType <> otPlus) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Nodes.PlusToken := CurrentToken;

  if (not Error) then
    Nodes.IntervalTag := ParseTag(kiINTERVAL);

  if (not Error) then
    Nodes.Interval := ParseInterval();

  Result := TSchedule.TIntervalListItem.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseIterateStmt(): TOffset;
var
  Nodes: TIterateStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IterateToken := ParseTag(kiITERATE);

  if (not Error) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdentifier) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.LabelToken := ApplyCurrentToken(utLabel);

  Result := TIterateStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseKeyword(): TOffset;
begin
  Result := 0;
  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex < 0) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Result := ApplyCurrentToken();
end;

function TCustomSQLParser.ParseLeaveStmt(): TOffset;
var
  Nodes: TLeaveStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.LeaveToken := ParseTag(kiLEAVE);

  if (not Error) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdentifier) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.LabelToken := ApplyCurrentToken(utLabel);

  Result := TLeaveStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseList(const Brackets: Boolean; const ParseNode: TParseFunction = nil): TOffset;
begin
  Result := ParseList(Brackets, ParseNode, ttComma);
end;

function TCustomSQLParser.ParseList(const Brackets: Boolean; const ParseNode: TParseFunction; const DelimterType: fspTypes.TTokenType): TOffset;
var
  DelimiterFound: Boolean;
  I: Integer;
  Index: Integer;
  Nodes: TList.TNodes;
  Childrens: array[0 .. 100 - 1] of TOffset;
  ChildrenList: Classes.TList;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  ChildrenList := nil;

  if (Brackets) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  Index := 0;
  if (not Error and (not Brackets or (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket))) then
  begin
    repeat
      if (Index < Length(Childrens)) then
        Childrens[Index] := ParseNode()
      else
      begin
        if (Index = Length(Childrens)) then
        begin
          ChildrenList := Classes.TList.Create();
          for I := 0 to Length(Childrens) - 1 do
            ChildrenList.Add(Pointer(Childrens[I]));
        end;
        if (not Assigned(ParseNode)) then
          ChildrenList.Add(Pointer(ApplyCurrentToken()))
        else
          ChildrenList.Add(Pointer(ParseNode()));
      end;

      Inc(Index);

      DelimiterFound := not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = DelimterType);
      if (DelimiterFound) then
        if (Index < Length(Childrens)) then
        begin
          Childrens[Index] := ApplyCurrentToken(); // Delimiter
          Inc(Index);
        end
        else
        begin
          begin
            ChildrenList := Classes.TList.Create();
            for I := 0 to Length(Childrens) - 1 do
              ChildrenList.Add(Pointer(Childrens[I]));
          end;
          ChildrenList.Add(Pointer(ApplyCurrentToken()));  // Delimiter
          Inc(Index);
        end;

    until (Error or not DelimiterFound
      or ((DelimterType = ttDelimiter) and
        ((TokenPtr(CurrentToken)^.KeywordIndex = kiEND)
          or (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE)
          or (TokenPtr(CurrentToken)^.KeywordIndex = kiELSEIF)
          or (TokenPtr(CurrentToken)^.KeywordIndex = kiUNTIL)
          or (TokenPtr(CurrentToken)^.KeywordIndex = kiWHEN))));
  end;

  if (not Error and Brackets) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  if (Index <= Length(Childrens)) then
    Result := TList.Create(Self, Nodes, Index, Childrens)
  else
  begin
    Result := TList.Create(Self, Nodes, Index, TIntegerArray(ChildrenList.List));
    ChildrenList.Free();
  end;
end;

function TCustomSQLParser.ParseLoopStmt(): TOffset;
var
  Nodes: TLoopStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(CurrentToken)^.TokenType = ttBeginLabel) then
    Nodes.BeginLabelToken := ApplyCurrentToken();

  Nodes.BeginTag := ParseTag(kiLOOP);

  if (not Error) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiLOOP);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttIdentifier)) then
    if ((Nodes.BeginLabelToken = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(Nodes.BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.EndLabelToken := ApplyCurrentToken(utLabel, ttEndLabel);

  Result := TLoopStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseOrder(): TOffset;
var
  Expr: TOffset;
  Direction: TOffset;
begin
  Expr := ParseExpr();

  if (Error or (CurrentToken = 0) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiASC) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiDESC)) then
    Direction := 0
  else
    Direction := ApplyCurrentToken();

  Result := TSelectStmt.TOrderBy.Create(Self, Expr, Direction);
end;

function TCustomSQLParser.ParsePartition(): TOffset;
begin
  Result := ParsePartition(False);
end;

function TCustomSQLParser.ParsePartition(const Add: Boolean): TOffset;
var
  Found: Boolean;
  Nodes: TPartition.TNodes;
begin
  if (Add) then
    Nodes.AddTag := ParseTag(kiADD);

  if (not Error) then
    Nodes.PartitionTag := ParseTag(kiPARTITION);

  if (not Error) then
    Nodes.NameIdent := ParsePartitionIdent();

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiVALUES)) then
    Nodes.ValuesNode := ParsePartitionValues();

  Found := True;
  while (not Error and Found and (CurrentToken > 0)) do
    if ((Nodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString)
    else if ((Nodes.DataDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDATA)) then
      Nodes.DataDirectoryValue := ParseValue(WordIndices(kiDATA, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENGINE)) then
      Nodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseIdent)
    else if ((Nodes.IndexDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX)) then
      Nodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.MaxRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_ROWS)) then
      Nodes.MaxRowsValue := ParseValue(kiMAX_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.MinRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMIN_ROWS)) then
      Nodes.MinRowsValue := ParseValue(kiMIN_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTORAGE)) then
      Nodes.EngineValue := ParseValue(WordIndices(kiSTORAGE, kiENGINE), vaAuto, ParseIdent)
    else if ((Nodes.SubPartitionList = 0) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
      Nodes.SubPartitionList := ParseList(True, ParseSubPartition)
    else
      Found := False;

  Result := TPartition.Create(Self, Nodes);
end;

function TCustomSQLParser.ParsePartitionIdent(): TOffset;
begin
  Result := ParseDbIdent(ditPartition);
end;

function TCustomSQLParser.ParsePartitionNames(): TOffset;
begin
  if (CurrentToken = 0) then
  begin
    SetError(PE_IncompleteStmt);
    Result := 0;
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiALL) then
    Result := ParseTag(kiALL)
  else
    Result := ParseList(False, ParsePartitionIdent);
end;

function TCustomSQLParser.ParsePartitionValues(): TOffset;
var
  Nodes: TPartitionValues.TNodes;
  ValueNodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ValuesTag := ParseTag(kiVALUES);

  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLESS) then
    if (NextToken[1] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[1])^.KeywordIndex <> kiTHAN) then
      SetError(PE_UnexpectedToken, NextToken[1])
    else if (NextToken[2] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[2])^.TokenType = ttOpenBracket) then
    begin
      FillChar(ValueNodes, SizeOf(ValueNodes), 0);
      ValueNodes.IdentTag := ParseTag(kiLESS, kiTHAN);
      ValueNodes.ValueNode := ParseList(True, ParseExpr);
      Nodes.DescriptionValue := TValue.Create(Self, ValueNodes);
    end
    else
      ParseValue(WordIndices(kiLESS, kiTHAN), vaNo, kiMAXVALUE)

  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIN) then
  begin
    FillChar(ValueNodes, SizeOf(ValueNodes), 0);
    ValueNodes.IdentTag := ParseTag(kiIn);
    ValueNodes.ValueNode := ParseList(True);
    Nodes.DescriptionValue := TValue.Create(Self, ValueNodes);
  end
  else
    SetError(PE_UnexpectedToken, CurrentToken);

  Result := TPartitionValues.Create(Self, Nodes);
end;

function TCustomSQLParser.ParsePL_SQLStmt(): TOffset;
begin
  Result := ParseStmt(True);
end;

function TCustomSQLParser.ParseProcedureParam(): TOffset;
var
  Nodes: TRoutineParam.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIN) then
    Nodes.DirektionTag := ParseTag(kiIN)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiOUT) then
    Nodes.DirektionTag := ParseTag(kiIN)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiINOUT) then
    Nodes.DirektionTag := ParseTag(kiIN);

  if (not Error) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.IdentToken := ApplyCurrentToken();

  if (not Error) then
    Nodes.DataTypeNode := ParseDataType();

  Result := TRoutineParam.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseRenameTableStmt(): TOffset;
var
  Nodes: TRenameTableStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.RenameTag := ParseTag(kiRENAME, kiTABLE);

  if (not Error) then
    Nodes.RenameList := ParseList(False, ParseRenameTableStmtPair);

  Result := TRenameTableStmt.Create(Self, Nodes)
end;

function TCustomSQLParser.ParseRenameTableStmtPair(): TOffset;
var
  Nodes: TRenameTableStmtPair.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.OrgTableIdent := ParseTableIdent();

  if (not Error) then
    Nodes.ToTag := ParseTag(kiTO);

  if (not Error) then
    Nodes.NewTableIdent := ParseTableIdent();

  Result := TRenameTableStmtPair.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseRepeatStmt(): TOffset;
var
  Nodes: TRepeatStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(CurrentToken)^.TokenType = ttBeginLabel) then
    Nodes.BeginLabelToken := ApplyCurrentToken();

  Nodes.RepeatTag := ParseTag(kiREPEAT);

  if (not Error) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter);

  if (not Error) then
    Nodes.UntilTag := ParseTag(kiUNTIL);

  if (not Error) then
    Nodes.SearchConditionExpr := ParseExpr();

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiREPEAT);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttIdentifier)) then
    if ((Nodes.BeginLabelToken = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(Nodes.BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.EndLabelToken := ApplyCurrentToken(utLabel, ttEndLabel);

  Result := TRepeatStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseSchedule(): TOffset;
var
  Nodes: TSchedule.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiAT) then
  begin
    Nodes.AtValue := ParseValue(kiAT, vaNo, ParseString);

    if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.OperatorType = otPlus)) then
      Nodes.AtIntervalList := ParseIntervalList();
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEVERY) then
  begin
    Nodes.EveryValue := ParseValue(kiEVERY, vaNo, ParseInterval);

    if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTARTS)) then
    begin
      Nodes.StartsValue := ParseValue(kiAT, vaNo, ParseString);

      if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.OperatorType = otPlus)) then
        Nodes.AtIntervalList := ParseIntervalList();
    end;

    if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENDS)) then
    begin
      Nodes.EndsValue := ParseValue(kiAT, vaNo, ParseString);

      if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.OperatorType = otPlus)) then
        Nodes.AtIntervalList := ParseIntervalList();
    end;
  end;

  Result := TSchedule.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseSelectStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TSelectStmt.TNodes;
begin
  Assert(TokenPtr(CurrentToken)^.KeywordIndex = kiSELECT);

  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.SelectTag := ParseTag(kiSELECT);

  Found := True;
  while (not Error and Found and (CurrentToken > 0)) do
    if ((Nodes.DistinctTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALL)) then
      Nodes.DistinctTag := ParseTag(kiALL)
    else if ((Nodes.DistinctTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDISTINCT)) then
      Nodes.DistinctTag := ParseTag(kiDISTINCT)
    else if ((Nodes.DistinctTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDISTINCTROW)) then
      Nodes.DistinctTag := ParseTag(kiDISTINCTROW)
    else if ((Nodes.HighPriorityTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiHIGH_PRIORITY)) then
      Nodes.HighPriorityTag := ParseTag(kiHIGH_PRIORITY)
    else if ((Nodes.StraightJoinTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTRAIGHT_JOIN)) then
      Nodes.StraightJoinTag := ParseTag(kiSTRAIGHT_JOIN)
    else if ((Nodes.SQLSmallResultTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_SMALL_RESULT)) then
      Nodes.SQLSmallResultTag := ParseTag(kiSQL_SMALL_RESULT)
    else if ((Nodes.SQLBigResultTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_BIG_RESULT)) then
      Nodes.SQLBigResultTag := ParseTag(kiSQL_BIG_RESULT)
    else if ((Nodes.SQLBufferResultTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_BUFFER_RESULT)) then
      Nodes.SQLBufferResultTag := ParseTag(kiSQL_BUFFER_RESULT)
    else if ((Nodes.SQLNoCacheTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_CACHE)) then
      Nodes.SQLNoCacheTag := ParseTag(kiSQL_CACHE)
    else if ((Nodes.SQLNoCacheTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_NO_CACHE)) then
      Nodes.SQLNoCacheTag := ParseTag(kiSQL_NO_CACHE)
    else if ((Nodes.SQLCalcFoundRowsTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_CALC_FOUND_ROWS)) then
      Nodes.SQLCalcFoundRowsTag := ParseTag(kiSQL_CALC_FOUND_ROWS)
    else
      Found := False;

  if (not Error) then
    Nodes.ColumnsNode := ParseList(False, ParseSelectStmtField);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM)) then
  begin
    if (TokenPtr(CurrentToken)^.KeywordIndex <> kiFROM) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      Nodes.FromTag := ParseTag(kiFROM);
      if (not Error) then
        Nodes.TablesNodes := ParseList(False, ParseTableReference);
    end;

    if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE)) then
    begin
      Nodes.WhereTag := ParseTag(kiWHERE);
      if (not Error) then
        Nodes.WhereNode := ParseExpr();
    end;

    if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiGROUP)) then
    begin
      Nodes.GroupByTag := ParseTag(kiGROUP, kiBY);

      if (not Error) then
        Nodes.GroupsNode := ParseList(False, ParseGroup);
      if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
        Nodes.WithRollupTag := ParseTag(kiWITH, kiROLLUP);
    end;

    if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiHAVING)) then
    begin
      Nodes.HavingToken := ApplyCurrentToken();

      if (CurrentToken = 0) then
        SetError(PE_IncompleteToken)
      else
        Nodes.HavingNode := ParseExpr();
    end;

    if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiORDER)) then
    begin
      Nodes.OrderByTag := ParseTag(kiORDER, kiBY);
      if (not Error) then
        Nodes.OrdersNode := ParseList(False, ParseOrder);
    end;

    if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLIMIT)) then
    begin
      Nodes.Limit.LimitToken := ApplyCurrentToken();

      if (CurrentToken = 0) then
        SetError(PE_IncompleteToken)
      else
      begin
        Nodes.Limit.RowCountValueToken := CurrentToken;

        if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttComma)) then
        begin
          Nodes.Limit.CommaToken := ApplyCurrentToken();

          Nodes.Limit.OffsetValueToken := Nodes.Limit.RowCountValueToken;
          Nodes.Limit.RowCountValueToken := ApplyCurrentToken();
        end
        else if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiOFFSET)) then
        begin
          Nodes.Limit.OffsetToken := ApplyCurrentToken();
          Nodes.Limit.OffsetValueToken := ApplyCurrentToken();
        end;
      end;
    end;
  end;

  Result := TSelectStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseSelectStmtField(): TOffset;
var
  Alias: TOffset;
  AsToken: TOffset;
  Value: TOffset;
begin
  Value := ParseExpr();

  if (Error or (CurrentToken = 0) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiAs)) then
    AsToken := 0
  else
    AsToken := ApplyCurrentToken();

  Alias := 0;
  if (not Error) then
    if ((AsToken > 0) and ((CurrentToken = 0) or (TokenPtr(CurrentToken)^.TokenType = ttDelimiter))) then
      SetError(PE_IncompleteStmt)
    else if ((AsToken > 0) and (CurrentToken > 0) and not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      Alias := ApplyCurrentToken(utAlias);

  Result := TSelectStmt.TField.Create(Self, Value, AsToken, Alias);
end;

function TCustomSQLParser.ParseServerOptionList(): TOffset;
var
  Childrens: array[0 .. 13 - 1] of TOffset;
  Database: Boolean;
  DelimiterFound: Boolean;
  Host: Boolean;
  Index: Integer;
  Nodes: TList.TNodes;
  Owner: Boolean;
  Password: Boolean;
  Port: Boolean;
  Socket: Boolean;
  User: Boolean;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Nodes.OpenBracket := ApplyCurrentToken();

  Index := 0;
  if (not Error) then
  begin
    Database := False;
    Host := False;
    Owner := False;
    Password := False;
    Port := False;
    Socket := False;
    User := False;
    repeat
      if (CurrentToken = 0) then
        SetError(PE_IncompleteStmt)
      else if (not Host and (TokenPtr(CurrentToken).KeywordIndex = kiHOST)) then
      begin
        Childrens[Index] := ParseValue(kiHOST, vaNo, ParseString);
        Host := True;
      end
      else if (not Database and (TokenPtr(CurrentToken).KeywordIndex = kiDATABASE)) then
      begin
        Childrens[Index] := ParseValue(kiDATABASE, vaNo, ParseString);
        Database := True;
      end
      else if (not User and (TokenPtr(CurrentToken).KeywordIndex = kiUSER)) then
      begin
        Childrens[Index] := ParseValue(kiUSER, vaNo, ParseString);
        User := True;
      end
      else if (not Password and (TokenPtr(CurrentToken).KeywordIndex = kiPASSWORD)) then
      begin
        Childrens[Index] := ParseValue(kiPASSWORD, vaNo, ParseString);
        Password := True;
      end
      else if (not Socket and (TokenPtr(CurrentToken).KeywordIndex = kiSOCKET)) then
      begin
        Childrens[Index] := ParseValue(kiSOCKET, vaNo, ParseString);
        Socket := True;
      end
      else if (not Owner and (TokenPtr(CurrentToken).KeywordIndex = kiOWNER)) then
      begin
        Childrens[Index] := ParseValue(kiOWNER, vaNo, ParseString);
        Owner := True;
      end
      else if (not Port and (TokenPtr(CurrentToken).KeywordIndex = kiPORT)) then
      begin
        Childrens[Index] := ParseValue(kiPort, vaNo, ParseInteger);
        Port := True;
      end
      else
        SetError(PE_UnexpectedToken, CurrentToken);

      Inc(Index);

      DelimiterFound := not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttComma);
      if (DelimiterFound) then
      begin
        Childrens[Index] := ApplyCurrentToken(); // Delimiter
        Inc(Index);
      end;
    until (Error or not DelimiterFound);
  end;

  if (not Error) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TList.Create(Self, Nodes, Index, Childrens);
end;

function TCustomSQLParser.ParseString(): TOffset;
begin
  Result := 0;
  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in [ttString, ttCSString]) and not ((TokenPtr(CurrentToken)^.TokenType = ttIdentifier) and (TokenPtr(CurrentToken)^.KeywordIndex < 0))) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Result := ApplyCurrentToken();
end;

function TCustomSQLParser.ParseSubArea(const ParseNode: TParseFunction): TOffset;
var
  Nodes: TSubArea.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    Nodes.AreaNode := ParseNode();

  if (not Error) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TSubArea.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseSubPartition(): TOffset;
var
  Found: Boolean;
  Nodes: TSubPartition.TNodes;
begin
  if (not Error) then
    Nodes.SubPartitionTag := ParseTag(kiPARTITION);

  if (not Error) then
    Nodes.NameIdent := ParsePartitionIdent();

  Found := True;
  while (not Error and Found and (CurrentToken > 0)) do
    if ((Nodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString)
    else if ((Nodes.DataDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDATA)) then
      Nodes.DataDirectoryValue := ParseValue(WordIndices(kiDATA, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENGINE)) then
      Nodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseIdent)
    else if ((Nodes.IndexDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX)) then
      Nodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.MaxRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_ROWS)) then
      Nodes.MaxRowsValue := ParseValue(kiMAX_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.MinRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMIN_ROWS)) then
      Nodes.MinRowsValue := ParseValue(kiMIN_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTORAGE)) then
      Nodes.EngineValue := ParseValue(WordIndices(kiSTORAGE, kiENGINE), vaAuto, ParseIdent)
    else
      Found := False;

  Result := TSubPartition.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseStmt(const PL_SQL: Boolean = False): TOffset;
var
  FirstToken: TOffset;
  KeywordIndex: TWordList.TIndex;
  KeywordToken: TOffset;
  Stmt: PStmt;
  Token: PToken;
begin
  FirstToken := CurrentToken;
  KeywordToken := CurrentToken;
  if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttBeginLabel)) then
    KeywordToken := NextToken[1];

  if (KeywordToken = 0) then
  begin
    SetError(PE_IncompleteStmt);
    Result := 0;
  end
  else
  begin
    KeywordIndex := TokenPtr(KeywordToken)^.KeywordIndex;

    if (KeywordIndex = kiALTER) then
      Result := ParseAlterStmt()
    else if (PL_SQL and (KeywordIndex = kiBEGIN)) then
      Result := ParseCompoundStmt()
    else if (KeywordIndex = kiCALL) then
      Result := ParseCallStmt()
    else if (PL_SQL and (KeywordIndex = kiCASE)) then
      Result := ParseCaseStmt()
    else if (KeywordIndex = kiCREATE) then
      Result := ParseCreateStmt()
    else if (KeywordIndex = kiDELETE) then
      Result := ParseDeleteStmt()
    else if (KeywordIndex = kiDO) then
      Result := ParseDoStmt()
    else if (KeywordIndex = kiDROP) then
      Result := ParseDropStmt()
    else if (PL_SQL and (KeywordIndex = kiIF)) then
      Result := ParseIfStmt()
    else if (KeywordIndex = kiINSERT) then
      Result := ParseInsertStmt()
    else if (PL_SQL and (KeywordIndex = kiITERATE)) then
      Result := ParseIterateStmt()
    else if (PL_SQL and (KeywordIndex = kiLEAVE)) then
      Result := ParseLeaveStmt()
    else if (PL_SQL and (KeywordIndex = kiLOOP)) then
      Result := ParseLoopStmt()
    else if (KeywordIndex = kiRENAME) then
      Result := ParseRenameTableStmt()
    else if (PL_SQL and (KeywordIndex = kiREPEAT)) then
      Result := ParseRepeatStmt()
    else if (KeywordIndex = kiSELECT) then
      Result := ParseSelectStmt()
    else if (KeywordIndex = kiTRUNCATE) then
      Result := ParseTruncateTableStmt()
    else if (PL_SQL and (KeywordIndex = kiWHILE)) then
      Result := ParseWhileStmt()
    else
      Result := ParseUnknownStmt();

    if (Error) then
      while ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType <> ttDelimiter)) do
        ApplyCurrentToken();

    Stmt := StmtPtr(Result);
    Stmt^.FErrorCode := FErrorCode;
    Stmt^.FErrorToken := FErrorToken;
    Stmt^.FParentNode := FRoot;
    Stmt^.FFirstToken := FirstToken;
    if (Root^.LastToken^.TokenType = ttDelimiter) then
      Stmt^.FLastToken := Root^.LastToken^.FPriorToken
    else
      Stmt^.FLastToken := Root^.FLastToken;
    while ((Stmt^.FLastToken <> Stmt^.FFirstToken) and (Stmt^.LastToken^.TokenType in [ttSpace, ttReturn, ttComment])) do
      Stmt^.FLastToken := Stmt^.LastToken^.FPriorToken;

    Token := Stmt^.FirstToken;
    while (Assigned(Token)) do
    begin
      if (Token^.FParentNode = 0) then
        Token^.FParentNode := Result;
      if (Token = StmtPtr(Result)^.LastToken) then
        Token := nil
      else
        Token := Token^.NextToken;
    end;
  end;
end;

function TCustomSQLParser.ParseTableIdent(): TOffset;
begin
  Result := ParseDbIdent(ditTable);
end;

function TCustomSQLParser.ParseTableReference(): TOffset;

  function ParseTableFactor(): TOffset;
  var
    Nodes: TSelectStmt.TTableFactor.TNodes;
    OjNodes: TSelectStmt.TTableFactorOj.TNodes;
    ReferencesNodes: TSelectStmt.TTableFactorReferences.TNodes;
    SelectNodes: TSelectStmt.TTableFactorSelect.TNodes;
  begin
    FillChar(Nodes, SizeOf(Nodes), 0);

      if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      begin
        Nodes.TableIdent := ParseTableIdent();

        if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
        begin
          Nodes.PartitionTag := ParseTag(kiPARTITION);

          if (not Error) then
            Nodes.Partitions := ParseList(True, ParsePartitionIdent);
        end;

        if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAS)) then
          Nodes.AsToken := ParseTag(kiAS);

        if (not Error and ((Nodes.AsToken > 0) or (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType in ttIdents))) then
          Nodes.AliasToken := ApplyCurrentToken(utAlias);

        if (not Error and (CurrentToken > 0) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiUSE) or (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE) or (TokenPtr(CurrentToken)^.KeywordIndex = kiFORCE))) then
          Nodes.IndexHints := ParseList(False, ParseIndexHint);

        Result := TSelectStmt.TTableFactor.Create(Self, Nodes);
      end
      else if ((TokenPtr(CurrentToken)^.TokenType = ttOpenBracket) and (NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT)) then
      begin
        SelectNodes.SelectStmt := ParseSubArea(ParseSelectStmt);

        if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAS)) then
          SelectNodes.AsToken := ParseTag(kiAS);

        if (not Error) then
          SelectNodes.AliasToken := ApplyCurrentToken(utAlias);

        Result := TSelectStmt.TTableFactorSelect.Create(Self, SelectNodes);
      end
      else if (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket) then
      begin
        ReferencesNodes.ReferenceList := ParseList(True, ParseTableReference);

        Result := TSelectStmt.TTableFactorReferences.Create(Self, ReferencesNodes);
      end
      else if (TokenPtr(CurrentToken)^.TokenType = ttOpenCurlyBracket) then
      begin
        OjNodes.OpenBracketToken := ApplyCurrentToken();

        if (not Error) then
          OjNodes.OjTag := ParseTag(kiOJ);

        if (not Error) then
          OjNodes.LeftTableReference := ParseTableReference();

        if (not Error) then
          OjNodes.LeftOuterJoinTag := ParseTag(kiLEFT, kiOUTER, kiJOIN);

        if (not Error) then
          OjNodes.RightTableReference := ParseTableReference();

        if (not Error) then
          OjNodes.OnTag := ParseTag(kiON);

        if (not Error) then
          OjNodes.CondExpr := ParseExpr();

        Result := TSelectStmt.TTableFactorOj.Create(Self, OjNodes);
      end
      else
      begin
        SetError(PE_UnexpectedToken, CurrentToken);
        Result := 0;
      end;
  end;

  procedure ApplyKeywordToken(var AKeywordTokens: TSelectStmt.TJoin.TKeywordTokens);
  var
    Index: Integer;
  begin
    Index := 0;
    while (AKeywordTokens[Index] > 0) do
    begin
      Inc(Index);
      if (Index = Length(AKeywordTokens)) then
        raise ERangeError.Create(SArgumentOutOfRange)
    end;

    AKeywordTokens[Index] := ApplyCurrentToken();
  end;

var
  Condition: TOffset;
  I: Integer;
  JoinType: TJoinType;
  JoinedTable: TOffset;
  KeywordIndex: TWordList.TIndex;
  KeywordTokens: TSelectStmt.TJoin.TKeywordTokens;
begin
  repeat
    Result := ParseTableFactor();

    JoinType := jtUnknown;
    JoinedTable := 0;
    Condition := 0;
    for I := 0 to Length(KeywordTokens) - 1 do
      KeywordTokens[I] := 0;

    if (not Error and (CurrentToken > 0)) then
    begin
      KeywordIndex := TokenPtr(CurrentToken)^.KeywordIndex;

      if (KeywordIndex = kiINNER) then
        JoinType := jtInner
      else if (KeywordIndex = kiCROSS) then
        JoinType := jtCross
      else if (KeywordIndex = kiJOIN) then
        JoinType := jtEqui
      else if (KeywordIndex = kiLEFT) then
        JoinType := jtCross
      else if (KeywordIndex = kiRIGHT) then
        JoinType := jtCross
      else if (KeywordIndex = kiNATURAL) then
      begin
        ApplyKeywordToken(KeywordTokens);
        if (CurrentToken = 0) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLEFT) then
          JoinType := jtNaturalLeft
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiRIGHT) then
          JoinType := jtNaturalRight
        else
          SetError(PE_UnexpectedToken, CurrentToken);
      end
      else if (KeywordIndex = kiSTRAIGHT_JOIN) then
        JoinType := jtEqui
      else
        JoinType := jtUnknown;

      if (JoinType <> jtUnknown) then
      begin
        if (JoinType in [jtNaturalLeft, jtNaturalRight]) then
          SetError(PE_UnexpectedToken, CurrentToken)
        else
        begin
          ApplyKeywordToken(KeywordTokens);

          if ((JoinType in [jtLeft, jtRight]) and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiOUTER)) then
            ApplyKeywordToken(KeywordTokens);

          if (JoinType in [jtInner, jtCross, jtLeft, jtRight]) then
            if (CurrentToken = 0) then
              SetError(PE_IncompleteStmt)
            else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiJOIN) then
              SetError(PE_UnexpectedToken, CurrentToken)
            else
              ApplyKeywordToken(KeywordTokens);
        end;

        if (not Error) then
          if ((JoinType in [jtInner, jtCross, jtEqui]) or (JoinType in [jtNaturalLeft, jtNaturalRight])) then
            JoinedTable := ParseTableFactor()
          else
            JoinedTable := ParseTableReference();

        if (not Error) then
          if (CurrentToken > 0) then
            if ((TokenPtr(CurrentToken)^.KeywordIndex = kiON) and not (JoinType in [jtNaturalLeft, jtNaturalRight])) then
            begin
              ApplyKeywordToken(KeywordTokens);
              Condition := ParseExpr()
            end
            else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiUSING) and not (JoinType in [jtNaturalLeft, jtNaturalRight])) then
            begin
              ApplyKeywordToken(KeywordTokens);
              Condition := ParseList(True, ParseColumnIdent);
            end;

        Result := TSelectStmt.TJoin.Create(Self, Result, JoinType, JoinedTable, Condition, KeywordTokens);
      end;
    end;

    if (Error or (CurrentToken = 0)) then
      KeywordIndex := 0
    else
      KeywordIndex := TokenPtr(CurrentToken)^.KeywordIndex;
  until ((KeywordIndex <> kiINNER)
    and (KeywordIndex <> kiCROSS)
    and (KeywordIndex <> kiJOIN)
    and (KeywordIndex <> kiSTRAIGHT_JOIN)
    and (KeywordIndex <> kiLEFT)
    and (KeywordIndex <> kiRIGHT)
    and (KeywordIndex <> kiNATURAL));
end;

function TCustomSQLParser.ParseToken(): TOffset;
label
  TwoChars,
  Selection, SelSpace, SelQuotedIdent, SelNotLess, SelNotEqual1, SelNotGreater, SelNot1, SelDoubleQuote, SelComment, SelModulo, SelDolor, SelAmpersand2, SelBitAND, SelSingleQuote, SelOpenBracket, SelCloseBracket, SelMySQLCodeEnd, SelMulti, SelComma, SelDoubleDot, SelDot, SelMySQLCode, SelDiv, SelNumeric, SelSLComment, SelArrow, SelMinus, SelPlus, SelAssign, SelColon, SelDelimiter, SelNULLSaveEqual, SelLessEqual, SelShiftLeft, SelNotEqual2, SelLess, SelEqual, SelGreaterEqual, SelShiftRight, SelGreater, SelParameter, SelAt, SelUnquotedIdentifier, SelDBIdentifier, SelBackslash, SelCloseSquareBracket, SelHat, SelMySQLCharacterSet, SelMySQLIdent, SelUnquotedIdentifierLower, SelOpenCurlyBracket, SelOpenCurlyBracket2, SelOpenCurlyBracket3, SelPipe, SelBitOR, SelCloseCurlyBracket, SelTilde, SelE,
  BindVariable,
  Colon,
  Comment,
  Intger, IntgerL, IntgerE,
  MLComment, MLCommentL, MLComment2,
  MySQLCharacterSet, MySQLCharacterSetL, MySQLCharacterSetLE, MySQLCharacterSetE,
  MySQLCondCode, MySQLCondCodeL, MySQLCondCodeE,
  Numeric, NumericL, NumericExp, NumericE, NumericDot, NumericLE,
  QuotedIdentifier, QuotedIdent2,
  Return, ReturnE,
  Separator,
  UnquotedIdentifier, UnquotedIdentLE, UnquotedIdentLabel,
  Variable,
  WhiteSpace, WhiteSpaceL, WhiteSpaceLE,
  Empty, Incomplete, Syntax, Error,
  TrippelChar,
  DoubleChar,
  SingleChar,
  Finish;
const
  Terminators: PChar = #9#10#13#32'#$%&()*+,-./;<=>@'; // Characters, terminating a token
  TerminatorsL = 21; // Count of Terminators
var
  DotFound: Boolean;
  EFound: Boolean;
  ErrorCode: Integer;
  KeywordIndex: TWordList.TIndex;
  Length: Integer;
  MySQLVersion: Integer;
  OperatorType: TOperatorType;
  SQL: PChar;
  TokenLength: Integer;
  TokenType: fspTypes.TTokenType;
  UsageType: TUsageType;
begin
  SQL := FParsePos.Text;
  Length := FParsePos.Length;
  asm
      PUSH ES
      PUSH ESI
      PUSH EDI
      PUSH EBX

      PUSH DS                          // string operations uses ES
      POP ES
      CLD                              // string operations uses forward direction

      MOV ESI,SQL
      MOV ECX,Length

      MOV TokenType,ttUnknown
      MOV OperatorType,otUnknown
      MOV MySQLVersion,0
      MOV ErrorCode,PE_Success

    // ------------------------------

      CMP ECX,1                        // One character in SQL?
      JB Empty                         // Less!
      JA TwoChars                      // More!
      MOV EAX,0                        // Hi Char in EAX
      MOV AX,[ESI]                     // One character from SQL to AX
    TwoChars:
      MOV EAX,[ESI]                    // Two characters from SQL to AX

    Selection:
      CMP AX,9                         // Tab ?
      JE WhiteSpace                    // Yes!
      CMP AX,10                        // Line feed ?
      JE Return                        // Yes!
      CMP AX,13                        // Carriadge Return ?
      JE Return                        // Yes!
      CMP AX,31                        // Invalid char ?
      JBE Syntax                       // Yes!
    SelSpace:
      CMP AX,' '                       // Space ?
      JE WhiteSpace                    // Yes!
    SelNotLess:
      CMP AX,'!'                       // "!" ?
      JNE SelDoubleQuote               // No!
      CMP EAX,$003C0021                // "!<" ?
      JNE SelNotEqual1                 // No!
      MOV OperatorType,otGreaterEqual
      JMP DoubleChar
    SelNotEqual1:
      CMP EAX,$003D0021                // "!=" ?
      JNE SelNotGreater                // No!
      MOV OperatorType,otNotEqual
      JMP DoubleChar
    SelNotGreater:
      CMP EAX,$003E0021                // "!>" ?
      JNE SelNot1                      // No!
      MOV OperatorType,otLessEqual
      JMP DoubleChar
    SelNot1:
      MOV OperatorType,otNot1
      JMP SingleChar
    SelDoubleQuote:
      CMP AX,'"'                       // Double Quote  ?
      JNE SelComment                   // No!
      MOV TokenType,ttDQIdentifier
      MOV DX,'"'                       // End Quoter
      JMP QuotedIdentifier
    SelComment:
      CMP AX,'#'                       // "#" ?
      JE Comment                       // Yes!
    SelDolor:
      CMP AX,'$'                       // "$" ?
      JE Syntax                        // Yes!
    SelModulo:
      CMP AX,'%'                       // "%" ?
      JNE SelAmpersand2                // No!
      MOV OperatorType,otMOD
      JMP SingleChar
    SelAmpersand2:
      CMP AX,'&'                       // "&" ?
      JNE SelSingleQuote               // No!
      CMP EAX,$00260026                // "&&" ?
      JNE SelBitAND                    // No!
      MOV OperatorType,otAND
      JMP DoubleChar
    SelBitAND:
      MOV OperatorType,otBitAND
      JMP SingleChar
    SelSingleQuote:
      CMP AX,''''                      // Single Quote ?
      JNE SelOpenBracket               // No!
      MOV TokenType,ttString
      MOV DX,''''                      // End Quoter
      JMP QuotedIdentifier
    SelOpenBracket:
      CMP AX,'('                       // "(" ?
      JNE SelCloseBracket              // No!
      MOV TokenType,ttOpenBracket
      JMP SingleChar
    SelCloseBracket:
      CMP AX,')'                       // ")" ?
      JNE SelMySQLCodeEnd              // No!
      MOV TokenType,ttCloseBracket
      JMP SingleChar
    SelMySQLCodeEnd:
      CMP AX,'*'                       // "*" ?
      JNE SelPlus                      // No!
      CMP EAX,$002F002A                // "*/" ?
      JNE SelMulti                     // No!
      MOV TokenType,ttMySQLCodeEnd
      JMP DoubleChar
    SelMulti:
      MOV OperatorType,otMulti
      JMP SingleChar
    SelPlus:
      CMP AX,'+'                       // "+" ?
      JNE SelComma                     // No!
      MOV OperatorType,otPlus
      JMP SingleChar
    SelComma:
      CMP AX,','                       // "," ?
      JNE SelSLComment                 // No!
      MOV TokenType,ttComma
      JMP SingleChar
    SelSLComment:
      CMP AX,'-'                       // "-" ?
      JNE SelDoubleDot                 // No!
      CMP EAX,$002D002D                // "--" ?
      JE Comment                       // Yes!
    SelArrow:
      CMP EAX,$003E002D                // "->" ?
      JNE SelMinus                     // No!
      MOV OperatorType,otArrow
      JMP DoubleChar
    SelMinus:
      MOV OperatorType,otMinus
      JMP SingleChar
    SelDoubleDot:
      CMP AX,'.'                       // "." ?
      JNE SelMySQLCode                 // No!
      CMP EAX,$002E002E                // ".." ?
      JNE SelDot                       // No!
      MOV OperatorType,otDoubleDot
      JMP DoubleChar
    SelDot:
      MOV OperatorType,otDot
      JMP SingleChar
    SelMySQLCode:
      CMP AX,'/'                       // "/" ?
      JNE SelNumeric                   // No!
      CMP EAX,$002A002F                // "/*" ?
      JNE SelDiv                       // No!
      CMP ECX,3                        // Three characters in SQL?
      JB MLComment                     // No!
      CMP WORD PTR [ESI + 4],'!'       // "/*!" ?
      JNE MLComment                    // No!
      JMP MySQLCondCode                // MySQL Code!
    SelDiv:
      MOV OperatorType,otDivision
      JMP SingleChar
    SelNumeric:
      CMP AX,'9'                       // Digit?
      JBE Intger                       // Yes!
    SelAssign:
      CMP AX,':'                       // ":" ?
      JNE SelDelimiter                 // No!
      CMP EAX,$003D003A                // ":=" ?
      JNE Colon                        // No!
      MOV OperatorType,otAssign2
      JMP SingleChar
    SelDelimiter:
      CMP AX,';'                       // ";" ?
      JNE SelNULLSaveEqual             // No!
      MOV TokenType,ttDelimiter
      JMP SingleChar
    SelNULLSaveEqual:
      CMP AX,'<'                       // "<" ?
      JNE SelShiftLeft                 // No!
      CMP EAX,$003D003C                // "<=" ?
      JNE SelShiftLeft                 // No!
      CMP ECX,3                        // Three characters in SQL?
      JB SelShiftLeft                  // No!
      CMP WORD PTR [ESI + 4],'>'       // "<=>" ?
      JNE SelLessEqual                 // No!
      MOV OperatorType,otNULLSaveEqual
      JMP TrippelChar
    SelLessEqual:
      MOV OperatorType,otLessEqual
      JMP DoubleChar
    SelShiftLeft:
      CMP EAX,$003C003C                // "<<" ?
      JNE SelNotEqual2                 // No!
      MOV OperatorType,otShiftLeft
      JMP DoubleChar
    SelNotEqual2:
      CMP EAX,$003E003C                // "<>" ?
      JNE SelEqual                     // No!
      MOV OperatorType,otNotEqual
      JMP DoubleChar
    SelLess:
      MOV OperatorType,otLess
      JMP SingleChar
    SelEqual:
      CMP AX,'='                       // "=" ?
      JNE SelGreaterEqual              // No!
      MOV OperatorType,otEqual
      JMP SingleChar
    SelGreaterEqual:
      CMP AX,'>'                       // ">" ?
      JNE SelParameter                 // No!
      CMP EAX,$003D003E                // ">=" ?
      JNE SelShiftRight                // No!
      MOV OperatorType,otGreaterEqual
      JMP DoubleChar
    SelShiftRight:
      CMP EAX,$003E003E                // ">>" ?
      JNE SelGreater                   // No!
      MOV OperatorType,otShiftRight
      JMP DoubleChar
    SelGreater:
      MOV OperatorType,otGreater
      JMP SingleChar
    SelParameter:
      CMP AX,'?'                       // "?" ?
      JNE SelAt                        // No!
      MOV OperatorType,otParameter
      JMP SingleChar
    SelAt:
      CMP AX,'@'                       // "@" ?
      JNE SelUnquotedIdentifier        // No!
      MOV TokenType,ttAt
      JMP SingleChar
    SelUnquotedIdentifier:
      CMP AX,'Z'                       // Up case character?
      JA SelDBIdentifier               // No!
      MOV TokenType,ttIdentifier
      JMP UnquotedIdentifier           // Yes!
    SelDBIdentifier:
      CMP AX,'['                       // "[" ?
      JNE SelBackslash                 // No!
      MOV TokenType,ttDBIdentifier
      MOV DX,']'                       // End Quoter
      JMP QuotedIdentifier
    SelBackslash:
      CMP AX,'\'                       // "\" ?
      JNE SelCloseSquareBracket        // No!
      MOV TokenType,ttBackslash
      JMP SingleChar
    SelCloseSquareBracket:
      CMP AX,']'                       // "]" ?
      JNE SelHat                       // Yes!
      JMP Incomplete
    SelHat:
      CMP AX,'^'                       // "^" ?
      JNE SelMySQLCharacterSet         // No!
      MOV OperatorType,otHat
      JMP SingleChar
    SelMySQLCharacterSet:
      CMP AX,'_'                       // "_" ?
      JE MySQLCharacterSet             // Yes!
    SelMySQLIdent:
      CMP AX,'`'                       // "`" ?
      JNE SelUnquotedIdentifierLower   // No!
      MOV TokenType,ttMySQLIdentifier
      MOV DX,'`'                       // End Quoter
      JMP QuotedIdentifier
    SelUnquotedIdentifierLower:
      CMP AX,'z'                       // Low case character?
      JA SelOpenCurlyBracket           // No!
      MOV TokenType,ttIdentifier
      JMP UnquotedIdentifier           // Yes!
    SelOpenCurlyBracket:
      CMP AX,'{'                       // "{" ?
      JNE SelPipe                      // No!
      MOV TokenType,ttOpenCurlyBracket
      CMP DWORD PTR [ESI + 2],$004A004F// "{OJ" ?
      JE SelOpenCurlyBracket2          // Yes!
      CMP DWORD PTR [ESI + 2],$006A004F// "{Oj" ?
      JE SelOpenCurlyBracket2          // Yes!
      CMP DWORD PTR [ESI + 2],$004A006F// "{oJ" ?
      JE SelOpenCurlyBracket2          // Yes!
      CMP DWORD PTR [ESI + 2],$006A006F// "{oj" ?
      JE SelOpenCurlyBracket2          // Yes!
      JMP SelOpenCurlyBracket3
    SelOpenCurlyBracket2:
      CMP ECX,4                        // Four characters in SQL?
      JB SelOpenCurlyBracket3          // No!
      PUSH EAX
      MOV AX,WORD PTR [ESI + 6]        // "{OJ " ?
      CALL Separator
      POP EAX
      JZ SingleChar                    // Yes!
    SelOpenCurlyBracket3:
      CMP WORD PTR [ESI + 2],' '       // "{ " ?
      JBE SingleChar                   // Yes!
      MOV TokenType,ttBRIdentifier
      MOV DX,'}'                       // End Quoter
      JMP QuotedIdentifier
    SelPipe:
      CMP AX,'|'                       // "|" ?
      JNE SelCloseCurlyBracket         // No!
      CMP EAX,$007C007C                // "||" ?
      JNE SelBitOR                     // No!
      MOV OperatorType,otPipes
      JMP DoubleChar
    SelBitOR:
      MOV OperatorType,otBitOr
      JMP SingleChar
    SelCloseCurlyBracket:
      CMP AX,'}'                       // "}" ?
      JNE SelTilde                     // No!
      MOV TokenType,ttCloseCurlyBracket
      JMP SingleChar
    SelTilde:
      CMP AX,'~'                       // "~" ?
      JNE SelE                         // No!
      MOV OperatorType,otInvertBits
      JMP SingleChar
    SelE:
      CMP AX,127                       // Chr(127) ?
      JNE UnquotedIdentifier           // No!
      JMP Syntax

    // ------------------------------

    BindVariable:
      MOV TokenType,ttBindVariable
      JMP UnquotedIdentifier

    // ------------------------------

    Colon:
      MOV TokenType,ttBindVariable
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      MOV AX,[ESI]                     // One Character from SQL to AX
      CMP AX,'A'
      JB Finish
      CMP AX,'Z'
      JBE BindVariable
      CMP AX,'a'
      JB Finish
      CMP AX,'z'
      JBE BindVariable
      JMP Syntax

    // ------------------------------

    Comment:
      MOV TokenType,ttComment
      CMP AX,10                        // End of line?
      JE Finish                        // Yes!
      CMP AX,13                        // End of line?
      JE Finish                        // Yes!
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      MOV AX,[ESI]                     // One Character from SQL to AX
      JMP Comment

    // ------------------------------

    Intger:
      MOV TokenType,ttInteger
    IntgerL:
      CMP AX,'.'                       // Dot?
      JE NumericDot                    // Yes!
      CMP AX,'E'                       // "E"?
      JE Numeric                       // Yes!
      CMP AX,'e'                       // "e"?
      JE Numeric                       // Yes!
      CMP AX,'0'                       // Digit?
      JB IntgerE                       // No!
      CMP AX,'9'
      JAE IntgerE                      // Yes!
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      MOV AX,[ESI]                     // One Character from SQL to AX
      JMP IntgerL
    IntgerE:
      CALL Separator                   // SQL separator?
      JNE Syntax                       // No!
      MOV TokenType,ttInteger
      JMP Finish

    // ------------------------------

    MLComment:
      MOV TokenType,ttComment
      ADD ESI,4                        // Step over "/*" in SQL
      SUB ECX,2                        // Two characters handled
    MLCommentL:
      CMP ECX,2                        // Two characters left in SQL?
      JAE MLComment2                   // Yes!
      JMP Incomplete
    MLComment2:
      MOV EAX,[ESI]                    // Load two character from SQL
      CMP EAX,$002F002A
      JE DoubleChar
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      JMP MLCommentL

    // ------------------------------

    MySQLCharacterSet:
      MOV TokenType,ttCSString
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      MOV EDX,ESI
    MySQLCharacterSetL:
      MOV AX,[ESI]                     // One Character from SQL to AX
      CMP AX,'0'                       // Digit?
      JB MySQLCharacterSetE            // No!
      CMP AX,'9'
      JBE MySQLCharacterSetLE          // Yes!
      CMP AX,'A'                       // String character?
      JB MySQLCharacterSetE            // No!
      CMP AX,'Z'
      JBE MySQLCharacterSetLE          // Yes!
      CMP AX,'a'                       // String character?
      JB MySQLCharacterSetE            // No!
      CMP AX,'z'
      JBE MySQLCharacterSetLE          // Yes!
    MySQLCharacterSetLE:
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      JZ Incomplete                    // End of SQL!
      JMP MySQLCharacterSetL
    MySQLCharacterSetE:
      CMP ESI,EDX
      JE Incomplete
      MOV AX,[ESI]                     // One Character from SQL to AX
      CMP AX,''''                      // "'"?
      JNE Syntax
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      JZ Incomplete                    // End of SQL!
      MOV DX,''''                      // End Quoter
      JMP QuotedIdentifier

    // ------------------------------

    MySQLCondCode:
      MOV TokenType,ttMySQLCodeStart
      ADD ESI,4                        // Step over "/*" in SQL
      SUB ECX,2                        // Two characters handled
      MOV EAX,0
      MOV EDX,0
    MySQLCondCodeL:
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      CMP ECX,0                        // End of SQL?
      JE MySQLCondCodeE                // Yes!
      MOV AX,[ESI]                     // One Character from SQL to AX
      CMP AX,'0'                       // Digit?
      JB MySQLCondCodeE                // No!
      CMP AX,'9'                       // Digit?
      JA MySQLCondCodeE                // No!
      SUB AX,'0'                       // Str to Int
      PUSH EAX                         // EDX := EDX * 10
      MOV EAX,EDX
      MOV EDX,10
      MUL EDX
      MOV EDX,EAX
      POP EAX
      ADD EDX,EAX                      // EDX := EDX + Digit
      JMP MySQLCondCodeL
    MySQLCondCodeE:
      MOV MySQLVersion,EDX
      JMP Finish

    // ------------------------------

    Numeric:
      MOV DotFound,False               // One dot in a numeric value allowed only
      MOV EFound,False                 // One "E" in a numeric value allowed only
    NumericL:
      CMP AX,'.'                       // Dot?
      JE NumericDot                    // Yes!
      CMP AX,'E'                       // "E"?
      JE NumericExp                    // Yes!
      CMP AX,'e'                       // "e"?
      JE NumericExp                    // Yes!
      CMP AX,'0'                       // Digit?
      JB NumericE                      // No!
      CMP AX,'9'
      JA NumericE                      // No!
      JMP NumericLE
    NumericDot:
      CMP EFound,False                 // A 'e' before?
      JNE Syntax                       // Yes!
      CMP DotFound,False               // A dot before?
      JNE Syntax                       // Yes!
      MOV DotFound,True
      JMP NumericLE
    NumericExp:
      CMP DotFound,False               // A dot before?
      JE Syntax                        // No!
      CMP EFound,False                 // A 'e' before?
      JNE Syntax                       // Yes!
      MOV EFound,True
    NumericLE:
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      MOV AX,[ESI]                     // One Character from SQL to AX
      JMP NumericL
    NumericE:
      CALL Separator                   // SQL separator?
      JNE Syntax                       // No!
      MOV TokenType,ttNumeric
      JMP Finish

    // ------------------------------

    QuotedIdentifier:
      // DX: End Quoter
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      CMP ECX,0                        // End of SQL?
      JE Incomplete                    // Yes!
    QuotedIdent2:
      MOV AX,[ESI]                     // One Character from SQL to AX
      CMP AX,'\'                       // Escaper?
      JE QuotedIdentifier
      CMP AX,DX                        // End Quoter (unescaped)?
      JNE QuotedIdentifier             // No!
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      JMP Finish

    // ------------------------------

    Return:
      MOV TokenType,ttReturn
      MOV EDX,EAX                      // Remember first character
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      JZ Finish                        // End of SQL!
      MOV AX,[ESI]                     // One Character from SQL to AX
      CMP AX,DX                        // Same character like before?
      JE Finish                        // Yes!
      CMP AX,10                        // Line feed?
      JE ReturnE                       // Yes!
      CMP AX,13                        // Carriadge Return?
      JNE Finish                       // No!
    ReturnE:
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      JMP Finish

    // ------------------------------

    Separator:
      // AX: Char
      PUSH ECX
      MOV EDI,[Terminators]
      MOV ECX,TerminatorsL
      REPNE SCASW                      // Character = SQL separator?
      POP ECX
      RET
      // ZF, if Char is in Terminators

    // ------------------------------

    Variable:
      MOV TokenType,ttVariable
      JMP UnquotedIdentifier

    // ------------------------------

    UnquotedIdentifier:
      CALL Separator                   // SQL separator?
      JE Finish
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      MOV AX,[ESI]                     // One Character from SQL to AX
      CMP AX,'@'
      JE UnquotedIdentifier
      CMP AX,'0'
      JB Finish
      CMP AX,'9'
      JBE UnquotedIdentifier
      CMP AX,':'
      JE UnquotedIdentLabel
      CMP AX,'A'
      JB Finish
      CMP AX,'Z'
      JBE UnquotedIdentifier
      CMP AX,'_'
      JE UnquotedIdentifier
      CMP AX,'a'
      JB Finish
      CMP AX,'z'
      JBE UnquotedIdentifier
      CMP AX,128
      JAE UnquotedIdentifier
      JMP Finish
    UnquotedIdentLabel:
      MOV TokenType,ttBeginLabel
      JMP SingleChar

    // ------------------------------

    WhiteSpace:
      MOV TokenType,ttSpace
    WhiteSpaceL:
      CMP AX,9
      JE WhiteSpaceLE
      CMP AX,' '
      JE WhiteSpaceLE
      JMP Finish
    WhiteSpaceLE:
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      MOV AX,[ESI]                     // One Character from SQL to AX
      JMP WhiteSpaceL

    // ------------------------------

    Empty:
      MOV ErrorCode,PE_EmptyText
      JMP Error
    Syntax:
      MOV ErrorCode,PE_Syntax
      MOV AX,[ESI]                     // One Character from SQL to AX
      CALL Separator
      JE Error
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      CMP ECX,0                        // End of SQL?
      JNE Syntax
    Incomplete:
      MOV ErrorCode,PE_IncompleteToken
    Error:
      JMP Finish

    TrippelChar:
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
    DoubleChar:
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
    SingleChar:
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled

    Finish:
      MOV EAX,Length
      SUB EAX,ECX
      MOV TokenLength,EAX

      POP EBX
      POP EDI
      POP ESI
      POP ES
  end;

  if (ErrorCode = PE_EmptyText) then
    Result := 0
  else
  begin
    if (Self is TMySQLSQLParser) then
    begin
      if (TMySQLSQLParser(Self).AnsiQuotes and (TokenType = ttMySQLIdentifier)
        or not TMySQLSQLParser(Self).AnsiQuotes and (TokenType = ttDQIdentifier)) then
        TokenType := ttUnknown;
      case (TokenType) of
        ttMySQLCodeStart:
          if (FMySQLVersion >= 0) then
          begin
            TokenType := ttUnknown;
            ErrorCode := PE_Syntax;
          end
          else
            FMySQLVersion := MySQLVersion;
        ttMySQLCodeEnd:
          if (FMySQLVersion < 0) then
          begin
            TokenType := ttUnknown;
            ErrorCode := PE_Syntax;
          end
          else
            FMySQLVersion := -1;
      end;
    end
    else if (TokenType in [ttMySQLCodeStart, ttMySQLCodeEnd]) then
    begin
      TokenType := ttUnknown;
      ErrorCode := PE_Syntax;
    end;

    if (OperatorType <> otUnknown) then
      TokenType := ttOperator;

    if (TokenType <> ttIdentifier) then
      KeywordIndex := -1
    else
    begin
      KeywordIndex := FKeywords.IndexOf(SQL, TokenLength);
      if (KeywordIndex >= 0) then
        OperatorType := OperatorTypeByKeywordIndex[KeywordIndex];
    end;

    if (KeywordIndex >= 0) then
      UsageType := utKeyword
    else
      UsageType := UsageTypeByTokenType[TokenType];

    Result := TToken.Create(Self, SQL, TokenLength, FParsePos.Origin, ErrorCode, FMySQLVersion, TokenType, OperatorType, KeywordIndex, UsageType);

    if (Root^.FLastToken > 0) then
    begin
      TokenPtr(Result)^.FPriorToken := Root^.FLastToken;
    end;
    Root^.FLastToken := Result;

    FParsePos.Text := @SQL[TokenLength];
    Dec(FParsePos.Length, TokenLength);
    if (TokenType = ttReturn) then
    begin
      Inc(FParsePos.Origin.Y);
      FParsePos.Origin.X := 0;
    end
    else
      Inc(FParsePos.Origin.X, TokenLength);
  end;
end;

function TCustomSQLParser.ParseTruncateTableStmt(): TOffset;
var
  Nodes: TTruncateTableStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.TruncateTag := ParseTag(kiTRUNCATE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTABLE)) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  Result := TTruncateTableStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseUnknownStmt(): TOffset;
var
  FirstToken: TOffset;
  Tokens: Classes.TList;
begin
  SetError(PE_UnkownStmt, CurrentToken);

  Tokens := Classes.TList.Create();

  FirstToken := CurrentToken;
  while ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType <> ttDelimiter)) do
    Tokens.Add(Pointer(ApplyCurrentToken()));

  Result := TUnknownStmt.Create(Self, Tokens);

  StmtPtr(Result)^.FFirstToken := FirstToken;
  StmtPtr(Result)^.FLastToken := Root^.FLastToken;

  Tokens.Free();
end;

function TCustomSQLParser.ParseUpdatePair(): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseColumnIdent();

  if (not Error) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT)) then
      Nodes.ValueNode := ApplyCurrentToken()
    else
      Nodes.ValueNode := ParseExpr();

  Result := TValue.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseUser(): TOffset;
var
  Nodes: TUser.TNodes;
begin
  Result := 0;

  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCURRENT_USER) then
    if (((NextToken[1] = 0) or (TokenPtr(NextToken[1])^.TokenType <> ttOpenBracket))
      and ((NextToken[2] = 0) or (TokenPtr(NextToken[2])^.TokenType <> ttCloseBracket))) then
      Result := ApplyCurrentToken()
    else
      Result := ParseFunction()
  else if (not (TokenPtr(CurrentToken)^.TokenType in [ttIdentifier, ttString])) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
  begin
    FillChar(Nodes, SizeOf(Nodes), 0);

    Nodes.NameToken := ApplyCurrentToken();

    if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttAt)) then
    begin
      Nodes.AtToken := ApplyCurrentToken();

      if (CurrentToken = 0) then
        SetError(PE_IncompleteStmt)
      else if (not (TokenPtr(CurrentToken)^.TokenType in [ttIdentifier, ttString])) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
        Nodes.HostToken := ApplyCurrentToken();
    end;

    if (not Error) then
      Result := TUser.Create(Self, Nodes);
  end;
end;

function TCustomSQLParser.ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const ValueKeywordIndex1: TWordList.TIndex; const ValueKeywordIndex2: TWordList.TIndex = -1): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndex);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    Nodes.ValueNode := ParseTag(ValueKeywordIndex1, ValueKeywordIndex2);

  Result := TValue.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const OptionIndices: TWordList.TIndices): TOffset;
var
  CurrentKeywordIndex: Integer;
  I: Integer;
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndex);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
  begin
    CurrentKeywordIndex := TokenPtr(CurrentToken)^.KeywordIndex;
    for I := 0 to Length(OptionIndices) - 1 do
      if (OptionIndices[I] = CurrentKeywordIndex) then
      begin
        Nodes.ValueNode := ParseTag(CurrentKeywordIndex);
        break;
      end;
    if (Nodes.ValueNode = 0) then
      SetError(PE_UnexpectedToken, CurrentToken);
  end;

  Result := TValue.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndex);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    Nodes.ValueNode := ParseValueNode();

  Result := TValue.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ValueKeywordIndex1: TWordList.TIndex; const ValueKeywordIndex2: TWordList.TIndex = -1): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  case (Length(KeywordIndices)) of
    1: Nodes.IdentTag := ParseTag(KeywordIndices[0]);
    2: Nodes.IdentTag := ParseTag(KeywordIndices[0], KeywordIndices[1]);
    3: Nodes.IdentTag := ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2]);
    4: Nodes.IdentTag := ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2], KeywordIndices[3]);
    else raise ERangeError.Create(SArgumentOutOfRange);
  end;

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    Nodes.ValueNode := ParseTag(ValueKeywordIndex1, ValueKeywordIndex2);

  Result := TValue.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  case (Length(KeywordIndices)) of
    1: Nodes.IdentTag := ParseTag(KeywordIndices[0]);
    2: Nodes.IdentTag := ParseTag(KeywordIndices[0], KeywordIndices[1]);
    3: Nodes.IdentTag := ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2]);
    4: Nodes.IdentTag := ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2], KeywordIndices[3]);
    else raise ERangeError.Create(SArgumentOutOfRange);
  end;

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    Nodes.ValueNode := ParseValueNode();

  Result := TValue.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseViewIdent(): TOffset;
begin
  Result := ParseDbIdent(ditView);
end;

function TCustomSQLParser.ParseWhileStmt(): TOffset;
var
  Nodes: TWhileStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(CurrentToken)^.TokenType = ttBeginLabel) then
    Nodes.BeginLabelToken := ApplyCurrentToken();

  if (not Error) then
    Nodes.WhileTag := ParseTag(kiWHILE);

  if (not Error) then
    Nodes.SearchConditionExpr := ParseExpr();

  if (not Error) then
    Nodes.DoTag := ParseTag(kiDO);

  if (not Error) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiWHILE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttIdentifier)) then
    if ((Nodes.BeginLabelToken = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(Nodes.BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.EndLabelToken := ApplyCurrentToken(utLabel, ttEndLabel);

  Result := TWhileStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.RangeNodePtr(const ANode: TOffset): PRange;
begin
  Assert(IsRange(NodePtr(ANode)));

  Result := PRange(NodePtr(ANode));
end;

procedure TCustomSQLParser.SaveToFile(const Filename: string; const FileType: TFileType = ftSQL);
var
  G: Integer;
  GenerationCount: Integer;
  Handle: THandle;
  HTML: string;
  LastTokenIndex: Integer;
  Node: PNode;
  ParentNodes: Classes.TList;
  Size: DWord;
  Stmt: PStmt;
  Token: PToken;
  Generation: Integer;
begin
  Handle := CreateFile(PChar(Filename),
                       GENERIC_WRITE,
                       FILE_SHARE_READ,
                       nil,
                       CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

  HTML :=
    '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + #13#10 +
    '<html>' + #13#10 +
    '  <head>' + #13#10 +
    '  <meta http-equiv="content-type" content="text/html">' + #13#10 +
    '  <title>Debug - Free SQL Parser</title>' + #13#10 +
    '  <style type="text/css">' + #13#10 +
    '    body {' + #13#10 +
    '      font: 16px Verdana,Arial,Sans-Serif;' + #13#10 +
    '      color: #000;' + #13#10 +
    '    }' + #13#10 +
    '    td {' + #13#10 +
    '    }' + #13#10 +
    '    a {' + #13#10 +
    '      text-decoration: none;' + #13#10 +
    '    }' + #13#10 +
    '    a:link span { display: none; }' + #13#10 +
    '    a:visited span { display: none; }' + #13#10 +
    '    a:hover span {' + #13#10 +
    '      display: block;' + #13#10 +
    '      position: absolute;' + #13#10 +
    '      margin: 18px 0px 0px 0px;' + #13#10 +
    '      background-color: #FFC;' + #13#10 +
    '      padding: 2px 4px 2px 4px;' + #13#10 +
    '      border: 1px solid #000;' + #13#10 +
    '      color: #000;' + #13#10 +
    '    }' + #13#10 +
    '    .Node {' + #13#10 +
    '      font-size: 15px;' + #13#10 +
    '      text-align: center;' + #13#10 +
    '      background-color: #F4F4F4;' + #13#10 +
    '    }' + #13#10 +
    '    .SQL {' + #13#10 +
    '      font-size: 16px;' + #13#10 +
    '      background-color: #F4F4F4;' + #13#10 +
    '      text-align: center;' + #13#10 +
    '    }' + #13#10 +
    '    .StmtError {' + #13#10 +
    '      font-size: 16px;' + #13#10 +
    '      background-color: #FFC0C0;' + #13#10 +
    '      text-align: center;' + #13#10 +
    '    }' + #13#10 +
    '  </style>' + #13#10 +
    '  </head>' + #13#10 +
    '  <body>' + #13#10;

  Stmt := Root^.FirstStmt;
  while (Assigned(Stmt)) do
  begin
    Token := Stmt^.FirstToken; GenerationCount := 0;
    while (Assigned(Token)) do
    begin
      GenerationCount := Max(GenerationCount, Token^.Generation);
      if (Token = Stmt^.LastToken) then
        Token := nil
      else
        Token := Token^.NextToken;
    end;

    ParentNodes := Classes.TList.Create();
    ParentNodes.Add(Root);

    HTML := HTML
      + '<table cellspacing="2" cellpadding="0" border="0">' + #13#10;

    for Generation := 0 to GenerationCount - 1 do
    begin
      HTML := HTML
        + '<tr>' + #13#10;
      Token := Stmt^.FirstToken;
      LastTokenIndex := Token^.Index - 1;;
      while (Assigned(Token)) do
      begin
        Node := Token^.ParentNode; G := Token^.Generation;
        while (IsChild(Node) and (G > Generation)) do
        begin
          Dec(G);
          if (G > Generation) then
            Node := PChild(Node)^.ParentNode;
        end;

        if (IsChild(Node) and (G = Generation) and (ParentNodes.IndexOf(Node) < 1)) then
        begin
          if (PChild(Node)^.FirstToken^.Index - LastTokenIndex - 1 > 0) then
            HTML := HTML
              + '<td colspan="' + IntToStr(PChild(Node)^.FirstToken^.Index - LastTokenIndex - 1) + '"></td>';
          HTML := HTML
            + '<td colspan="' + IntToStr(PChild(Node)^.LastToken^.Index - PChild(Node)^.FirstToken^.Index + 1) + '" class="Node">';
          HTML := HTML
            + '<a href="">'
            + HTMLEscape(NodeTypeToString[Node^.NodeType])
            + '<span><table cellspacing="2" cellpadding="0">';
          if (Assigned(PChild(Node)^.ParentNode)) then
            HTML := HTML
              + '<tr><td>ParentNode Offset:</td><td>&nbsp;</td><td>' + IntToStr(PChild(Node)^.ParentNode^.Offset) + '</td></tr>';
          HTML := HTML
            + '<tr><td>Offset:</td><td>&nbsp;</td><td>' + IntToStr(Node^.Offset) + '</td></tr>';
          if (IsStmt(Node)) then
          begin
            HTML := HTML + '<tr><td>StmtType:</td><td>&nbsp;</td><td>' + StmtTypeToString[PStmt(Node)^.StmtType] + '</td></tr>';
          end;
//          if (Assigned(PNode(PChild(Node)^.NextSibling))) then
//            HTML := HTML
//              + '<tr><td>NextSibling:</td><td>&nbsp;</td><td>' + IntToStr((PNode(PChild(Node)^.NextSibling)^.Offset)) + '</td></tr>';
          case (Node^.NodeType) of
            ntDbIdent:
              HTML := HTML
                + '<tr><td>DbIdentType:</td><td>&nbsp;</td><td>' + DbIdentTypeToString[PDbIdent(Node)^.DbIdentType] + '</td></tr>';
            ntBinaryOp:
              if (IsToken(PNode(PBinaryOp(Node)^.Operator))) then
                HTML := HTML
                  + '<tr><td>OperatorType:</td><td>&nbsp;</td><td>' + OperatorTypeToString[PToken(PBinaryOp(Node)^.Operator)^.OperatorType] + '</td></tr>';
          end;
          HTML := HTML
            + '</table></span>'
            + '</a></td>' + #13#10;

          LastTokenIndex := PChild(Node)^.LastToken^.Index;

          ParentNodes.Add(Node);
          Token := PChild(Node)^.LastToken;
        end;

        if (Token <> Stmt^.LastToken) then
          Token := Token^.NextToken
        else
        begin
          if (Token^.Index - LastTokenIndex > 0) then
            HTML := HTML
              + '<td colspan="' + IntToStr(Token^.Index - LastTokenIndex) + '"></td>';
          Token := nil;
        end;
      end;
      HTML := HTML
        + '</tr>' + #13#10;
    end;

    ParentNodes.Free();


    HTML := HTML
      + '<tr class="SQL">' + #13#10;

    Token := Stmt^.FirstToken;
    while (Assigned(Token)) do
    begin
      HTML := HTML
        + '<td><a href="">';
      HTML := HTML
        + '<code>' + HTMLEscape(ReplaceStr(Token.Text, ' ', '&nbsp;')) + '</code>';
      HTML := HTML
        + '<span><table cellspacing="2" cellpadding="0">';
      if (Assigned(PChild(Token)^.ParentNode)) then
        HTML := HTML + '<tr><td>ParentNode Offset:</td><td>&nbsp;</td><td>' + IntToStr(PChild(Token)^.ParentNode^.Offset) + '</td></tr>';
      HTML := HTML + '<tr><td>Offset:</td><td>&nbsp;</td><td>' + IntToStr(PNode(Token)^.Offset) + '</td></tr>';
      HTML := HTML + '<tr><td>TokenType:</td><td>&nbsp;</td><td>' + HTMLEscape(TokenTypeToString[Token^.TokenType]) + '</td></tr>';
      if (Token^.KeywordIndex >= 0) then
        HTML := HTML + '<tr><td>KeywordIndex:</td><td>&nbsp;</td><td>ki' + HTMLEscape(FKeywords[Token^.KeywordIndex]) + '</td></tr>';
      if (Token^.OperatorType <> otUnknown) then
        HTML := HTML + '<tr><td>OperatorType:</td><td>&nbsp;</td><td>' + HTMLEscape(OperatorTypeToString[Token^.OperatorType]) + '</td></tr>';
      if (Token^.DbIdentType <> ditUnknown) then
        HTML := HTML + '<tr><td>DbIdentType:</td><td>&nbsp;</td><td>' + HTMLEscape(DbIdentTypeToString[Token^.DbIdentType]) + '</td></tr>';
      if ((Trim(Token^.AsString) <> '') and (Token^.KeywordIndex < 0)) then
        HTML := HTML + '<tr><td>AsString:</td><td>&nbsp;</td><td>' + HTMLEscape(Token^.AsString) + '</td></tr>';
      if (Token^.ErrorCode <> PE_Success) then
        HTML := HTML + '<tr><td>ErrorCode:</td><td>&nbsp;</td><td>' + IntToStr(Token^.ErrorCode) + '</td></tr>';
      if (Token^.UsageType <> utUnknown) then
        HTML := HTML + '<tr><td>UsageType:</td><td>&nbsp;</td><td>' + HTMLEscape(UsageTypeToString[Token^.UsageType]) + '</td></tr>';
      HTML := HTML
        + '</table></span>';
      HTML := HTML
        + '</a></td>' + #13#10;

      if (Token = Stmt^.LastToken) then
        Token := nil
      else
        Token := Token.NextToken;
    end;
    HTML := HTML
      + '</tr>' + #13#10;

    if (Stmt^.Error and Assigned(Stmt^.ErrorToken)) then
    begin
      HTML := HTML
        + '<tr class="">';
      if (Stmt^.ErrorToken^.Index - Stmt^.FirstToken^.Index > 0) then
        HTML := HTML
          + '<td colspan="' + IntToStr(Stmt^.ErrorToken^.Index - Stmt^.FirstToken^.Index) + '"></td>';
      HTML := HTML
        + '<td class="StmtError"><a href="">&uarr;'
        + '<span><table cellspacing="2" cellpadding="0">'
        + '<tr><td>ErrorCode:</td><td>' + IntToStr(Stmt.ErrorCode) + '</td></tr>'
        + '<tr><td>ErrorMessage:</td><td>' + HTMLEscape(Stmt.ErrorMessage) + '</td></tr>'
        + '</table></span>'
        + '</a></td>';
      if (Stmt^.ErrorToken^.Index - Stmt^.LastToken^.Index > 0) then
        HTML := HTML
          + '<td colspan="' + IntToStr(Stmt^.LastToken^.Index - Stmt^.ErrorToken^.Index) + '"></td>';
      HTML := HTML
        + '</tr>' + #13#10;
    end;

    HTML := HTML
      + '</table>' + #13#10;

    Stmt := Stmt^.NextStmt;

    if (Assigned(Stmt)) then
    HTML := HTML
      + '<br><br>' + #13#10;
  end;

  HTML := HTML +
    '     <br>' + #13#10 +
    '     <br>' + #13#10 +
    '  </body>' + #13#10 +
    '</html>';

  WriteFile(Handle, PChar(BOM_UNICODE_LE)^, 2, Size, nil);

  WriteFile(Handle, PChar(HTML)^, Length(HTML) * SizeOf(Char), Size, nil);

  CloseHandle(Handle);
end;

procedure TCustomSQLParser.SetError(const AErrorCode: Integer; const AErrorNode: TOffset = 0);
begin
  Assert(not Error);

  FErrorCode := AErrorCode;
  if (AErrorNode = 0) then
    FErrorToken := CurrentToken
  else
    FErrorToken := StmtNodePtr(AErrorNode)^.FFirstToken;
end;

procedure TCustomSQLParser.SetFunctions(AFunctions: string);
begin
  FFunctions.Text := AFunctions;
end;

procedure TCustomSQLParser.SetKeywords(AKeywords: string);

  function IndexOf(const Word: string): Integer;
  begin
    Result := FKeywords.IndexOf(PChar(Word), Length(Word));

    if (Result < 0) then
      raise ERangeError.CreateFmt(SKeywordNotFound, [Word]);
  end;

var
  Index: Integer;
begin
  FKeywords.Text := AKeywords;

  if (AKeywords <> '') then
  begin
    kiACTION              := IndexOf('ACTION');
    kiADD                 := IndexOf('ADD');
    kiAFTER               := IndexOf('AFTER');
    kiALGORITHM           := IndexOf('ALGORITHM');
    kiALL                 := IndexOf('ALL');
    kiALTER               := IndexOf('ALTER');
    kiANALYZE             := IndexOf('ANALYZE');
    kiAND                 := IndexOf('AND');
    kiAS                  := IndexOf('AS');
    kiASC                 := IndexOf('ASC');
    kiAT                  := IndexOf('AT');
    kiAUTO_INCREMENT      := IndexOf('AUTO_INCREMENT');
    kiAVG_ROW_LENGTH      := IndexOf('AVG_ROW_LENGTH');
    kiBEFORE              := IndexOf('BEFORE');
    kiBEGIN               := IndexOf('BEGIN');
    kiBETWEEN             := IndexOf('BETWEEN');
    kiBINARY              := IndexOf('BINARY');
    kiBTREE               := IndexOf('BTREE');
    kiBY                  := IndexOf('BY');
    kiCALL                := IndexOf('CALL');
    kiCASCADE             := IndexOf('CASCADE');
    kiCASCADED            := IndexOf('CASCADED');
    kiCASE                := IndexOf('CASE');
    kiCHANGE              := IndexOf('CHANGE');
    kiCHARACTER           := IndexOf('CHARACTER');
    kiCHECK               := IndexOf('CHECK');
    kiCHECKSUM            := IndexOf('CHECKSUM');
    kiCOALESCE            := IndexOf('COALESCE');
    kiCOLLATE             := IndexOf('COLLATE');
    kiCOLUMN              := IndexOf('COLUMN');
    kiCOLUMN_FORMAT       := IndexOf('COLUMN_FORMAT');
    kiCOMMENT             := IndexOf('COMMENT');
    kiCOMPLETION          := IndexOf('COMPLETION');
    kiCONNECTION          := IndexOf('CONNECTION');
    kiCONSTRAINT          := IndexOf('CONSTRAINT');
    kiCONTAINS            := IndexOf('CONTAINS');
    kiCONVERT             := IndexOf('CONVERT');
    kiCOPY                := IndexOf('COPY');
    kiCREATE              := IndexOf('CREATE');
    kiCROSS               := IndexOf('CROSS');
    kiCURRENT_USER        := IndexOf('CURRENT_USER');
    kiDATA                := IndexOf('DATA');
    kiDATABASE            := IndexOf('DATABASE');
    kiDAY                 := IndexOf('DAY');
    kiDAY_HOUR            := IndexOf('DAY_HOUR');
    kiDAY_MINUTE          := IndexOf('DAY_MINUTE');
    kiDAY_SECOND          := IndexOf('DAY_SECOND');
    kiDEFAULT             := IndexOf('DEFAULT');
    kiDEFINER             := IndexOf('DEFINER');
    kiDELAY_KEY_WRITE     := IndexOf('DELAY_KEY_WRITE');
    kiDELAYED             := IndexOf('DELAYED');
    kiDELETE              := IndexOf('DELETE');
    kiDESC                := IndexOf('DESC');
    kiDETERMINISTIC       := IndexOf('DETERMINISTIC');
    kiDIRECTORY           := IndexOf('DIRECTORY');
    kiDISABLE             := IndexOf('DISABLE');
    kiDISCARD             := IndexOf('DISCARD');
    kiDISTINCT            := IndexOf('DISTINCT');
    kiDISTINCTROW         := IndexOf('DISTINCTROW');
    kiDIV                 := IndexOf('DIV');
    kiDO                  := IndexOf('DO');
    kiDROP                := IndexOf('DROP');
    kiDUPLICATE           := IndexOf('DUPLICATE');
    kiEACH                := IndexOf('EACH');
    kiELSE                := IndexOf('ELSE');
    kiELSEIF              := IndexOf('ELSEIF');
    kiENABLE              := IndexOf('ENABLE');
    kiEND                 := IndexOf('END');
    kiENDS                := IndexOf('ENDS');
    kiENGINE              := IndexOf('ENGINE');
    kiEVENT               := IndexOf('EVENT');
    kiEVERY               := IndexOf('EVERY');
    kiEXCHANGE            := IndexOf('EXCHANGE');
    kiEXCLUSIVE           := IndexOf('EXCLUSIVE');
    kiEXISTS              := IndexOf('EXISTS');
    kiFIRST               := IndexOf('FIRST');
    kiFOR                 := IndexOf('FOR');
    kiFORCE               := IndexOf('FORCE');
    kiFOREIGN             := IndexOf('FOREIGN');
    kiFROM                := IndexOf('FROM');
    kiFULL                := IndexOf('FULL');
    kiFULLTEXT            := IndexOf('FULLTEXT');
    kiFUNCTION            := IndexOf('FUNCTION');
    kiGROUP               := IndexOf('GROUP');
    kiHASH                := IndexOf('HASH');
    kiHAVING              := IndexOf('HAVING');
    kiHIGH_PRIORITY       := IndexOf('HIGH_PRIORITY');
    kiHOST                := IndexOf('HOST');
    kiHOUR                := IndexOf('HOUR');
    kiHOUR_MINUTE         := IndexOf('HOUR_MINUTE');
    kiHOUR_SECOND         := IndexOf('HOUR_SECOND');
    kiIF                  := IndexOf('IF');
    kiIGNORE              := IndexOf('IGNORE');
    kiIMPORT              := IndexOf('IMPORT');
    kiIN                  := IndexOf('IN');
    kiINDEX               := IndexOf('INDEX');
    kiINNER               := IndexOf('INNER');
    kiINOUT               := IndexOf('INOUT');
    kiINPLACE             := IndexOf('INPLACE');
    kiINSERT              := IndexOf('INSERT');
    kiINSERT_METHOD       := IndexOf('INSERT_METHOD');
    kiINTERVAL            := IndexOf('INTERVAL');
    kiINTO                := IndexOf('INTO');
    kiINVOKER             := IndexOf('INVOKER');
    kiIS                  := IndexOf('IS');
    kiITERATE             := IndexOf('ITERATE');
    kiJOIN                := IndexOf('JOIN');
    kiKEY                 := IndexOf('KEY');
    kiKEY_BLOCK_SIZE      := IndexOf('KEY_BLOCK_SIZE');
    kiKEYS                := IndexOf('KEYS');
    kiLANGUAGE            := IndexOf('LANGUAGE');
    kiLEAVE               := IndexOf('LEAVE');
    kiLEFT                := IndexOf('LEFT');
    kiLESS                := IndexOf('LESS');
    kiLIKE                := IndexOf('LIKE');
    kiLIMIT               := IndexOf('LIMIT');
    kiLINEAR              := IndexOf('LINEAR');
    kiLIST                := IndexOf('LIST');
    kiLOCAL               := IndexOf('LOCAL');
    kiLOCK                := IndexOf('LOCK');
    kiLOOP                := IndexOf('LOOP');
    kiLOW_PRIORITY        := IndexOf('LOW_PRIORITY');
    kiMATCH               := IndexOf('MATCH');
    kiMAX_ROWS            := IndexOf('MAX_ROWS');
    kiMAXVALUE            := IndexOf('MAXVALUE');
    kiMERGE               := IndexOf('MERGE');
    kiMIN_ROWS            := IndexOf('MIN_ROWS');
    kiMINUTE              := IndexOf('MINUTE');
    kiMINUTE_SECOND       := IndexOf('MINUTE_SECOND');
    kiMOD                 := IndexOf('MOD');
    kiMODIFIES            := IndexOf('MODIFIES');
    kiMODIFY              := IndexOf('MODIFY');
    kiMONTH               := IndexOf('MONTH');
    kiNAME                := IndexOf('NAME');
    kiNATURAL             := IndexOf('NATURAL');
    kiNO                  := IndexOf('NO');
    kiNONE                := IndexOf('NONE');
    kiNOT                 := IndexOf('NOT');
    kiNULL                := IndexOf('NULL');
    kiOFFSET              := IndexOf('OFFSET');
    kiOJ                  := IndexOf('OJ');
    kiON                  := IndexOf('ON');
    kiOPTIMIZE            := IndexOf('OPTIMIZE');
    kiOPTION              := IndexOf('OPTION');
    kiOPTIONS             := IndexOf('OPTIONS');
    kiOR                  := IndexOf('OR');
    kiORDER               := IndexOf('ORDER');
    kiOUT                 := IndexOf('OUT');
    kiOUTER               := IndexOf('OUTER');
    kiOWNER               := IndexOf('OWNER');
    kiPACK_KEYS           := IndexOf('PACK_KEYS');
    kiPARSER              := IndexOf('PARSER');
    kiPARTIAL             := IndexOf('PARTIAL');
    kiPARTITION           := IndexOf('PARTITION');
    kiPARTITIONING        := IndexOf('PARTITIONING');
    kiPARTITIONS          := IndexOf('PARTITIONS');
    kiPASSWORD            := IndexOf('PASSWORD');
    kiPORT                := IndexOf('PORT');
    kiPRESERVE            := IndexOf('PRESERVE');
    kiPRIMARY             := IndexOf('PRIMARY');
    kiPROCEDURE           := IndexOf('PROCEDURE');
    kiQUARTER             := IndexOf('QUARTER');
    kiQUICK               := IndexOf('QUICK');
    kiRANGE               := IndexOf('RANGE');
    kiREADS               := IndexOf('READS');
    kiREBUILD             := IndexOf('REBUILD');
    kiREFERENCES          := IndexOf('REFERENCES');
    kiREGEXP              := IndexOf('REGEXP');
    kiREMOVE              := IndexOf('REMOVE');
    kiRENAME              := IndexOf('RENAME');
    kiREORGANIZE          := IndexOf('REORGANIZE');
    kiREPEAT              := IndexOf('REPEAT');
    kiREPAIR              := IndexOf('REPAIR');
    kiREPLACE             := IndexOf('REPLACE');
    kiRESTRICT            := IndexOf('RESTRICT');
    kiRETURNS             := IndexOf('RETURNS');
    kiRIGHT               := IndexOf('RIGHT');
    kiRLIKE               := IndexOf('RLIKE');
    kiROLLUP              := IndexOf('ROLLUP');
    kiROW                 := IndexOf('ROW');
    kiROW_FORMAT          := IndexOf('ROW_FORMAT');
    kiSCHEDULE            := IndexOf('SCHEDULE');
    kiSCHEMA              := IndexOf('SCHEMA');
    kiSECOND              := IndexOf('SECOND');
    kiSECURITY            := IndexOf('SECURITY');
    kiSELECT              := IndexOf('SELECT');
    kiSERVER              := IndexOf('SERVER');
    kiSET                 := IndexOf('SET');
    kiSHARED              := IndexOf('SHARED');
    kiSIMPLE              := IndexOf('SIMPLE');
    kiSOCKET              := IndexOf('SOCKET');
    kiSOUNDS              := IndexOf('SOUNDS');
    kiSPATIAL             := IndexOf('SPATIAL');
    kiSQL                 := IndexOf('SQL');
    kiSQL_BIG_RESULT      := IndexOf('SQL_BIG_RESULT');
    kiSQL_BUFFER_RESULT   := IndexOf('SQL_BUFFER_RESULT');
    kiSQL_CACHE           := IndexOf('SQL_CACHE');
    kiSQL_CALC_FOUND_ROWS := IndexOf('SQL_CALC_FOUND_ROWS');
    kiSQL_NO_CACHE        := IndexOf('SQL_NO_CACHE');
    kiSQL_SMALL_RESULT    := IndexOf('SQL_SMALL_RESULT');
    kiSTARTS              := IndexOf('STARTS');
    kiSTATS_AUTO_RECALC   := IndexOf('STATS_AUTO_RECALC');
    kiSTATS_PERSISTENT    := IndexOf('STATS_PERSISTENT');
    kiSTORAGE             := IndexOf('STORAGE');
    kiSTRAIGHT_JOIN       := IndexOf('STRAIGHT_JOIN');
    kiSUBPARTITION        := IndexOf('SUBPARTITION');
    kiSUBPARTITIONS       := IndexOf('SUBPARTITIONS');
    kiTABLE               := IndexOf('TABLE');
    kiTABLESPACE          := IndexOf('TABLESPACE');
    kiTEMPORARY           := IndexOf('TEMPORARY');
    kiTEMPTABLE           := IndexOf('TEMPTABLE');
    kiTHAN                := IndexOf('THAN');
    kiTHEN                := IndexOf('THEN');
    kiTO                  := IndexOf('TO');
    kiTRIGGER             := IndexOf('TRIGGER');
    kiTRUNCATE            := IndexOf('TRUNCATE');
    kiUNDEFINED           := IndexOf('UNDEFINED');
    kiUNION               := IndexOf('UNION');
    kiUNIQUE              := IndexOf('UNIQUE');
    kiUNSIGNED            := IndexOf('UNSIGNED');
    kiUNTIL               := IndexOf('UNTIL');
    kiUPDATE              := IndexOf('UPDATE');
    kiUPGRADE             := IndexOf('UPGRADE');
    kiUSE                 := IndexOf('USE');
    kiUSER                := IndexOf('USER');
    kiUSING               := IndexOf('USING');
    kiVALUE               := IndexOf('VALUE');
    kiVALUES              := IndexOf('VALUES');
    kiVIEW                := IndexOf('VIEW');
    kiWEEK                := IndexOf('WEEK');
    kiWHEN                := IndexOf('WHEN');
    kiWHERE               := IndexOf('WHERE');
    kiWHILE               := IndexOf('WHILE');
    kiWRAPPER             := IndexOf('WRAPPER');
    kiWITH                := IndexOf('WITH');
    kiXOR                 := IndexOf('XOR');
    kiYEAR                := IndexOf('YEAR');
    kiYEAR_MONTH          := IndexOf('YEAR_MONTH');
    kiZEROFILL            := IndexOf('ZEROFILL');

    SetLength(OperatorTypeByKeywordIndex, FKeywords.Count);
    for Index := 0 to FKeywords.Count - 1 do
      OperatorTypeByKeywordIndex[Index] := otUnknown;
    OperatorTypeByKeywordIndex[kiAND]     := otAND;
    OperatorTypeByKeywordIndex[kiCASE]    := otCase;
    OperatorTypeByKeywordIndex[kiBETWEEN] := otBetween;
    OperatorTypeByKeywordIndex[kiBINARY]  := otBinary;
    OperatorTypeByKeywordIndex[kiCOLLATE] := otCollate;
    OperatorTypeByKeywordIndex[kiDIV]     := otDIV;
    OperatorTypeByKeywordIndex[kiELSE]    := otELSE;
    OperatorTypeByKeywordIndex[kiIS]      := otIS;
    OperatorTypeByKeywordIndex[kiIN]      := otIN;
    OperatorTypeByKeywordIndex[kiLIKE]    := otLike;
    OperatorTypeByKeywordIndex[kiMOD]     := otMOD;
    OperatorTypeByKeywordIndex[kiNOT]     := otNOT2;
    OperatorTypeByKeywordIndex[kiOR]      := otOR;
    OperatorTypeByKeywordIndex[kiREGEXP]  := otRegExp;
    OperatorTypeByKeywordIndex[kiRLIKE]   := otRegExp;
    OperatorTypeByKeywordIndex[kiSOUNDS]  := otSounds;
    OperatorTypeByKeywordIndex[kiWHEN]    := otWHEN;
    OperatorTypeByKeywordIndex[kiTHEN]    := otTHEN;
    OperatorTypeByKeywordIndex[kiXOR]     := otXOR;
  end;
end;

function TCustomSQLParser.StmtNodePtr(const ANode: TOffset): PChild;
begin
  Assert(IsChild(NodePtr(ANode)));

  Result := @FNodes.Mem[ANode];
end;

function TCustomSQLParser.StmtPtr(const ANode: TOffset): PStmt;
begin
  Assert(IsChild(NodePtr(ANode)));

  Result := @FNodes.Mem[ANode];
end;

function TCustomSQLParser.TokenPtr(const ANode: TOffset): PToken;
begin
  Assert(NodePtr(ANode)^.FNodeType = ntToken);

  Result := PToken(NodePtr(ANode));
end;

{ TMySQLSQLParser *************************************************************}

constructor TMySQLSQLParser.Create(const MySQLVersion: Integer = 0; const LowerCaseTableNames: Integer = 0);
begin
  FMySQLVersion := MySQLVersion;
  FLowerCaseTableNames := LowerCaseTableNames;

  inherited Create(sdMySQL);

  FAnsiQuotes := False;

  Functions := MySQLFunctions;
  Keywords := MySQLKeywords;
end;

end.
