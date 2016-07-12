unit FreeSQLParser;

interface {********************************************************************}

uses
  Classes,
  SQLUtils,
  fspTypes, fspConst;

type
  TMySQLParser = class
  public
    type
      TFileType = (ftSQL, ftDebugHTML);

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
        TIndices = array[0..5] of TIndex;
      private
        FCount: TIndex;
        FIndex: array of PChar;
        FFirst: array of Integer;
        FParser: TMySQLParser;
        FText: string;
        function GetWord(Index: TIndex): string;
        procedure SetText(AText: string);
      protected
        procedure Clear();
        property Parser: TMySQLParser read FParser;
      public
        constructor Create(const ASQLParser: TMySQLParser; const AText: string = '');
        destructor Destroy(); override;
        function IndexOf(const Word: PChar; const Length: Integer): Integer; overload;
        function IndexOf(const Word: string): Integer; overload; {$IFNDEF Debug} inline; {$ENDIF}
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
        FParser: TMySQLParser;
      private
        class function Create(const AParser: TMySQLParser; const ANodeType: TNodeType): TOffset; static;
        function GetOffset(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        property Offset: TOffset read GetOffset;
      public
        property NodeType: TNodeType read FNodeType;
        property Parser: TMySQLParser read FParser;
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
        class function Create(const AParser: TMySQLParser): TOffset; static;
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
        property Parser: TMySQLParser read Heritage.FParser;
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
        property Parser: TMySQLParser read Heritage.FParser;
      end;

      TToken = packed record
      private
        Heritage: TChild;
        property FParentNode: TOffset read Heritage.FParentNode write Heritage.FParentNode;
      private
        FDbIdentType: TDbIdentType;
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
        class function Create(const AParser: TMySQLParser;
          const ASQL: PChar; const ALength: Integer; const AOrigin: TOrigin;
          const AErrorCode: Integer; const AMySQLVersion: Integer; const ATokenType: TTokenType;
          const AOperatorType: TOperatorType; const AKeywordIndex: TWordList.TIndex; const AUsageType: TUsageType): TOffset; static;
        function GetAsString(): string;
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
        property Offset: TOffset read GetOffset;
      public
        property AsString: string read GetAsString;
        property DbIdentType: TDbIdentType read FDbIdentType;
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
        property Parser: TMySQLParser read Heritage.Heritage.FParser;
        property Text: string read GetText write SetText;
        property TokenType: fspTypes.TTokenType read FTokenType;
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
        class function Create(const AParser: TMySQLParser; const ANodeType: TNodeType): TOffset; static;
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
        property Parser: TMySQLParser read Heritage.Heritage.FParser;
        property ParentNode: PNode read GetParentNode;
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
        class function Create(const AParser: TMySQLParser; const AStmtType: TStmtType): TOffset; static;
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
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PConvertTo = ^TConvertTo;
        TConvertTo = packed record
        private type
          TNodes = packed record
            ConvertToTag: TOffset;
            CharacterSetValue: TOffset;
            CollateValue: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PBeginStmt = ^TBeginStmt;
      TBeginStmt = packed record
      private type
        TNodes = packed record
          BeginTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const AOperator, AOperand1, AOperand2: TOffset): TOffset; static;
        function GetOperand1(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperand2(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Operand1: PChild read GetOperand1;
        property Operand2: PChild read GetOperand2;
        property Operator: PChild read GetOperator;
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const AOperator1, AOperator2, AExpr, AMin, AMax: TOffset): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PCloseStmt = ^TCloseStmt;
      TCloseStmt = packed record
      private type
        TNodes = packed record
          CloseTag: TOffset;
          CursorIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PCommitStmt = ^TCommitStmt;
      TCommitStmt = packed record
      private type
        TNodes = packed record
          CommitTag: TOffset;
          ChainTag: TOffset;
          ReleaseTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          CharacterSetValue: TOffset;
          CollateValue: TOffset;
          BinaryTag: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PDbIdent = ^TDbIdent;
      TDbIdent = packed record
      private type
        TNodes = packed record
          Ident: TOffset;
          DatabaseDot: TOffset;
          DatabaseIdent: TOffset;
          TableDot: TOffset;
          TableIdent: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FDbIdentType: TDbIdentType;
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ADbIdentType: TDbIdentType; const ANodes: TNodes): TOffset; overload; static;
        class function Create(const AParser: TMySQLParser; const ADbIdentType: TDbIdentType;
          const AIdent, ADatabaseDot, ADatabaseIdent, ATableDot, ATableIdent: TOffset): TOffset; overload; static;
        function GetDatabaseIdent(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetIdent(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetTableIdent(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property DatabaseIdent: PToken read GetDatabaseIdent;
        property DbIdentType: TDbIdentType read FDbIdentType;
        property Ident: PToken read GetIdent;
        property ParentNode: PNode read GetParentNode;
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        property TableIdent: PToken read GetTableIdent;
      end;

      PDeclareStmt = ^TDeclareStmt;
      TDeclareStmt = packed record
      private type
        TNodes = packed record
          DeclareTag: TOffset;
          IdentList: TOffset;
          TypeNode: TOffset;
          DefaultValue: TOffset;
          CursorForTag: TOffset;
          SelectStmt: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PFetchStmt = ^TFetchStmt;
      TFetchStmt = packed record
      private type
        TNodes = packed record
          FetchTag: TOffset;
          NextTag: TOffset;
          FromTag: TOffset;
          CursorIdent: TOffset;
          IntoValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const AIdent, AArguments: TOffset): TOffset; static;
        function GetArguments(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetIdent(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Arguments: PChild read GetArguments;
        property Ident: PChild read GetIdent;
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          BranchList: TOffset;
          EndTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PIgnoreLines = ^TIgnoreLines;
      TIgnoreLines = packed record
      private type
        TNodes = packed record
          IgnoreTag: TOffset;
          NumberToken: TOffset;
          LinesTag: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PLeaveStmt = ^TLeaveStmt;
      TLeaveStmt = packed record
      private type
        TNodes = packed record
          LeaveTag: TOffset;
          LabelToken: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes; const AChildrenCount: Integer; const AChildrens: array of TOffset): TOffset; static;
        function GetCount(): Integer;
        function GetFirstChild(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        property Count: Integer read GetCount;
      public
        property FirstChild: PChild read GetFirstChild;
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;
      PList = ^TList;

      PLoadDataStmt = ^TLoadDataStmt;
      TLoadDataStmt = packed record
      private type
        TNodes = packed record
          LoadDataTag: TOffset;
          PriorityTag: TOffset;
          LocalTag: TOffset;
          InfileTag: TOffset;
          FilenameString: TOffset;
          ReplaceIgnoreTag: TOffset;
          IntoTableValue: TOffset;
          PartitionValue: TOffset;
          CharacterSetValue: TOffset;
          ColumnsTag: TOffset;
          ColumnsTerminatedByValue: TOffset;
          EnclosedByValue: TOffset;
          EscapedByValue: TOffset;
          LinesTag: TOffset;
          StartingByValue: TOffset;
          LinesTerminatedByValue: TOffset;
          IgnoreLines: TOffset;
          ColumnList: TOffset;
          SetList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PLoadXMLStmt = ^TLoadXMLStmt;
      TLoadXMLStmt = packed record
      private type
        TNodes = packed record
          LoadXMLTag: TOffset;
          PriorityTag: TOffset;
          LocalTag: TOffset;
          InfileTag: TOffset;
          FilenameString: TOffset;
          ReplaceIgnoreTag: TOffset;
          IntoTableValue: TOffset;
          PartitionValue: TOffset;
          CharacterSetValue: TOffset;
          RowsIdentifiedByValue: TOffset;
          IgnoreLines: TOffset;
          ColumnList: TOffset;
          SetList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PLockStmt = ^TLockStmt;
      TLockStmt = packed record
      private type

        PItem = ^TItem;
        TItem = packed record
        private type
          TNodes = packed record
            TableIdent: TOffset;
            AsTag: TOffset;
            AliasIdent: TOffset;
            TypeTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          LockTablesTag: TOffset;
          ItemList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      POpenStmt = ^TOpenStmt;
      TOpenStmt = packed record
      private type
        TNodes = packed record
          OpenTag: TOffset;
          CursorIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PRenameStmt = ^TRenameStmt;
      TRenameStmt = packed record
      private type

        PPair = ^TPair;
        TPair = packed record
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          RenameTag: TOffset;
          RenameList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PReleaseStmt = ^TReleaseStmt;
      TReleaseStmt = packed record
      private type
        TNodes = packed record
          ReleaseTag: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PRollbackStmt = ^TRollbackStmt;
      TRollbackStmt = packed record
      private type
        TNodes = packed record
          RollbackTag: TOffset;
          ChainTag: TOffset;
          ReleaseTag: TOffset;
          ToValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSavepointStmt = ^TSavepointStmt;
      TSavepointStmt = packed record
      private type
        TNodes = packed record
          SavepointTag: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSelectStmt = ^TSelectStmt;
      TSelectStmt = packed record
      private type

        PField = ^TField;
        TField = packed record
        private type
          TNodes = packed record
            Value: TOffset;
            AsTag: TOffset;
            AliasIdent: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PTableFactor = ^TTableFactor;
        TTableFactor = packed record
        private type

          PIndexHint = ^TIndexHint;
          TIndexHint = packed record
          public type
            TNodes = record
              KindTag: TOffset;
              IndexTag: TOffset;
              ForValue: TOffset;
            end;
          private
            Heritage: TRange;
          private
            FNodes: TNodes;
            class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
          public
            property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ALeftTable: TOffset; const AJoinType: TJoinType; const ARightTable: TOffset; const ACondition: TOffset; const AKeywordTokens: TKeywordTokens): TOffset; static;
        end;

        PGroup = ^TGroup;
        TGroup = packed record
        private type
          TNodes = packed record
            Expr: TOffset;
            Direction: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PGroups = ^TGroups;
        TGroups = packed record
        private type
          TNodes = packed record
            GroupByTag: TOffset;
            ColumnList: TOffset;
            WithRollupTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        POrder = ^TOrder;
        TOrder = packed record
        private type
          TNodes = packed record
            Expr: TOffset;
            Direction: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          ColumnsList: TOffset;
          FromValue: TOffset;
          WhereValue: TOffset;
          GroupByNode: TOffset;
          HavingValue: TOffset;
          OrderByValue: TOffset;
          Limit: record
            LimitTag: TOffset;
            OffsetTag: TOffset;
            OffsetToken: TOffset;
            CommaToken: TOffset;
            RowCountToken: TOffset;
          end;
          IntoValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSetStmt = ^TSetStmt;
      TSetStmt = packed record
      private type

        PAssignment = ^TAssignment;
        TAssignment = packed record
        private type
          TNodes = packed record
            ScopeTag: TOffset;
            Variable: TOffset;
            AssignToken: TOffset;
            ValueExpr: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          SetTag: TOffset;
          AssignmentList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSetPasswordStmt = ^TSetPasswordStmt;
      TSetPasswordStmt = packed record
      private type
        TNodes = packed record
          SetTag: TOffset;
          PasswordTag: TOffset;
          ForValue: TOffset;
          PasswordExpr: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSetTransactionStmt = ^TSetTransactionStmt;
      TSetTransactionStmt = packed record
      private type

        PCharacteristic = ^TCharacteristic;
        TCharacteristic = packed record
        private type
          TNodes = packed record
            KindTag: TOffset;
            Value: TOffset;
          end;
        private
          Heritage: TRange;
        private
          FNodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          SetTag: TOffset;
          ScopeTag: TOffset;
          TransactionTag: TOffset;
          CharacteristicList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowAuthorsStmt = ^TShowAuthorsStmt;
      TShowAuthorsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowBinaryLogsStmt = ^TShowBinaryLogsStmt;
      TShowBinaryLogsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowBinlogEventsStmt = ^TShowBinlogEventsStmt;
      TShowBinlogEventsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          InValue: TOffset;
          FromValue: TOffset;
          LimitTag: TOffset;
          OffsetToken: TOffset;
          CommaToken: TOffset;
          RowCountToken: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowCharacterSetStmt = ^TShowCharacterSetStmt;
      TShowCharacterSetStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowCollationStmt = ^TShowCollationStmt;
      TShowCollationStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowContributorsStmt = ^TShowContributorsStmt;
      TShowContributorsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const AOperator1, AOperator2: TOffset; const AOperand1, AOperand2: TOffset): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PStartTransactionStmt = ^TStartTransactionStmt;
      TStartTransactionStmt = packed record
      private type
        TNodes = packed record
          StartTransactionTag: TOffset;
          RealOnlyTag: TOffset;
          ReadWriteTag: TOffset;
          WithConsistentSnapshotTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PTruncateStmt = ^TTruncateStmt;
      TTruncateStmt = packed record
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const AOperator, AOperand: TOffset): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PUnknownStmt = ^TUnknownStmt;
      TUnknownStmt = packed record
      private
        Heritage: TStmt;
      private
        class function Create(const AParser: TMySQLParser; const ATokens: Classes.TList): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PUnlockStmt = ^TUnlockStmt;
      TUnlockStmt = packed record
      private type
        TNodes = packed record
          UnlockTablesTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PUpdateStmt = ^TUpdateStmt;
      TUpdateStmt = packed record
      private type
        TNodes = packed record
          UpdateTag: TOffset;
          PriorityTag: TOffset;
          TableReferences: TOffset;
          SetValue: TOffset;
          WhereValue: TOffset;
          OrderByValue: TOffset;
          LimitValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PVariable = ^TVariable;
      TVariable = packed record
      private type
        TNodes = packed record
          At2Token: TOffset;
          At1Token: TOffset;
          ScopeTag: TOffset;
          DotToken: TOffset;
          IdentTag: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PXAStmt = ^TXAStmt;
      TXAStmt = packed record
      private type
        TNodes = packed record
          XATag: TOffset;
          ActionTag: TOffset;
          Ident: TOffset;
          RestTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

  protected
    FAnsiQuotes: Boolean;
    FErrorCode: Integer;
    FErrorToken: TOffset;
    FFunctions: TWordList;
    FHighNotPrecedence: Boolean;
    FKeywords: TWordList;
    FLowerCaseTableNames: Integer;
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
    function GetCurrentToken(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function GetErrorMessage(): string; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function GetErrorMessage(const AErrorCode: Integer): string; overload;
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
    kiAUTHORS,
    kiAVG_ROW_LENGTH,
    kiBEFORE,
    kiBEGIN,
    kiBETWEEN,
    kiBINARY,
    kiBINLOG,
    kiBTREE,
    kiBY,
    kiCALL,
    kiCASCADE,
    kiCASCADED,
    kiCASE,
    kiCHANGE,
    kiCHAIN,
    kiCHARACTER,
    kiCHECK,
    kiCHECKSUM,
    kiCLOSE,
    kiCOALESCE,
    kiCOLLATE,
    kiCOLLATION,
    kiCOLUMN,
    kiCOLUMN_FORMAT,
    kiCOLUMNS,
    kiCOMMENT,
    kiCOMMIT,
    kiCOMMITTED,
    kiCOMPACT,
    kiCOMPLETION,
    kiCOMPRESSED,
    kiCONCURRENT,
    kiCONNECTION,
    kiCONSISTENT,
    kiCONSTRAINT,
    kiCONTAINS,
    kiCONTRIBUTORS,
    kiCONVERT,
    kiCOPY,
    kiCREATE,
    kiCROSS,
    kiCURRENT_USER,
    kiCURSOR,
    kiDATA,
    kiDATABASE,
    kiDAY,
    kiDAY_HOUR,
    kiDAY_MINUTE,
    kiDAY_SECOND,
    kiDECLARE,
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
    kiDYNAMIC,
    kiEACH,
    kiELSE,
    kiELSEIF,
    kiENABLE,
    kiENCLOSED,
    kiEND,
    kiENDS,
    kiENGINE,
    kiESCAPED,
    kiEVENT,
    kiEVENTS,
    kiEVERY,
    kiEXCHANGE,
    kiEXCLUSIVE,
    kiEXISTS,
    kiFETCH,
    kiFIELDS,
    kiFIRST,
    kiFIXED,
    kiFOR,
    kiFORCE,
    kiFOREIGN,
    kiFROM,
    kiFULL,
    kiFULLTEXT,
    kiFUNCTION,
    kiGLOBAL,
    kiGROUP,
    kiHASH,
    kiHAVING,
    kiHIGH_PRIORITY,
    kiHOST,
    kiHOUR,
    kiHOUR_MINUTE,
    kiHOUR_SECOND,
    kiIDENTIFIED,
    kiIF,
    kiIGNORE,
    kiIMPORT,
    kiIN,
    kiINDEX,
    kiINNER,
    kiINFILE,
    kiINOUT,
    kiINPLACE,
    kiINSERT,
    kiINSERT_METHOD,
    kiINTERVAL,
    kiINTO,
    kiINVOKER,
    kiIS,
    kiISOLATION,
    kiITERATE,
    kiJOIN,
    kiKEY,
    kiKEY_BLOCK_SIZE,
    kiKEYS,
    kiLANGUAGE,
    kiLAST,
    kiLEAVE,
    kiLEFT,
    kiLESS,
    kiLEVEL,
    kiLIKE,
    kiLIMIT,
    kiLINEAR,
    kiLINES,
    kiLIST,
    kiLOAD,
    kiLOCAL,
    kiLOCK,
    kiLOGS,
    kiLOOP,
    kiLOW_PRIORITY,
    kiMASTER,
    kiMATCH,
    kiMAX_ROWS,
    kiMAXVALUE,
    kiMERGE,
    kiMIGRATE,
    kiMIN_ROWS,
    kiMINUTE,
    kiMINUTE_SECOND,
    kiMOD,
    kiMODIFIES,
    kiMODIFY,
    kiMONTH,
    kiNAME,
    kiNATURAL,
    kiNEXT,
    kiNO,
    kiNONE,
    kiNOT,
    kiNULL,
    kiOFFSET,
    kiOJ,
    kiON,
    kiONE,
    kiONLY,
    kiOPEN,
    kiOPTIMIZE,
    kiOPTION,
    kiOPTIONAL,
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
    kiPHASE,
    kiPORT,
    kiPREPARE,
    kiPRESERVE,
    kiPRIMARY,
    kiPROCEDURE,
    kiQUARTER,
    kiQUICK,
    kiRANGE,
    kiREAD,
    kiREADS,
    kiREBUILD,
    kiRECOVER,
    kiREDUNDANT,
    kiREFERENCES,
    kiREGEXP,
    kiRELEASE,
    kiREMOVE,
    kiRENAME,
    kiREORGANIZE,
    kiREPEAT,
    kiREPAIR,
    kiREPEATABLE,
    kiREPLACE,
    kiRESTRICT,
    kiRESUME,
    kiRETURNS,
    kiRIGHT,
    kiRLIKE,
    kiROLLBACK,
    kiROLLUP,
    kiROW,
    kiROW_FORMAT,
    kiROWS,
    kiSAVEPOINT,
    kiSCHEDULE,
    kiSCHEMA,
    kiSECOND,
    kiSECURITY,
    kiSELECT,
    kiSERIALIZABLE,
    kiSERVER,
    kiSESSION,
    kiSET,
    kiSHARED,
    kiSHOW,
    kiSIMPLE,
    kiSNAPSHOT,
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
    kiSTARTING,
    kiSTART,
    kiSTARTS,
    kiSTATS_AUTO_RECALC,
    kiSTATS_PERSISTENT,
    kiSTORAGE,
    kiSTRAIGHT_JOIN,
    kiSUBPARTITION,
    kiSUBPARTITIONS,
    kiSUSPEND,
    kiTABLE,
    kiTABLES,
    kiTABLESPACE,
    kiTEMPORARY,
    kiTEMPTABLE,
    kiTERMINATED,
    kiTHAN,
    kiTHEN,
    kiTO,
    kiTRANSACTION,
    kiTRIGGER,
    kiTRUNCATE,
    kiUNCOMMITTED,
    kiUNDEFINED,
    kiUNION,
    kiUNIQUE,
    kiUNLOCK,
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
    kiWRITE,
    kiWITH,
    kiWORK,
    kiXA,
    kiXML,
    kiXOR,
    kiYEAR,
    kiYEAR_MONTH,
    kiZEROFILL: Integer;

    OperatorTypeByKeywordIndex: array of TOperatorType;

    function ApplyCurrentToken(const AUsageType: TUsageType = utUnknown; const ATokenType: TTokenType = ttUnknown): TOffset;
    function EndOfStmt(const Token: TOffset): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
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
    function ParseBeginStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCallStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCaseOp(): TOffset;
    function ParseCaseOpBranch(): TOffset;
    function ParseCaseStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCaseStmtBranch(): TOffset;
    function ParseCharacterSetIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCollateIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseColumn(const Add: TCreateTableColumnAdd = caNone): TOffset;
    function ParseCloseStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCommitStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCompoundStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseColumnIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
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
    function ParseDeclareStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
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
    function ParseEngineIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseEventIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseExpr(): TOffset;
    function ParseExprList(): TOffset;
    function ParseFetchStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseForeignKey(const Add: Boolean = False): TOffset;
    function ParseFunction(): TOffset;
    function ParseFunctionParam(): TOffset;
    function ParseIfStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseIfStmtBranch(): TOffset;
    function ParseIndex(const Add: Boolean = False): TOffset;
    function ParseIndexColumn(): TOffset;
    function ParseIndexHint(): TOffset;
    function ParseInsertStmt(const Replace: Boolean = False): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseInteger(): TOffset;
    function ParseInterval(): TOffset;
    function ParseIntervalList(): TSchedule.TIntervalList;
    function ParseIntervalListItem(const KeywordIndex: TWordList.TIndex): TOffset;
    function ParseIterateStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseKeyword(): TOffset;
    function ParseLeaveStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseList(const Brackets: Boolean; const ParseItem: TParseFunction = nil): TOffset; overload; inline;
    function ParseList(const Brackets: Boolean; const ParseItem: TParseFunction; const DelimterType: TTokenType): TOffset; overload;
    function ParseLoadDataStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseLoadStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseLoadXMLStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseLockStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseLockStmtItem(): TOffset;
    function ParseLoopStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseOpenStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParsePartition(): TOffset; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function ParsePartition(const Add: Boolean): TOffset; overload;
    function ParsePartitionIdent(): TOffset;
    function ParsePartitionNames(): TOffset;
    function ParsePartitionValues(): TOffset;
    function ParsePL_SQLStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseProcedureParam(): TOffset;
    function ParseReleaseStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseRenameStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseRenameTableStmtPair(): TOffset;
    function ParseRepeatStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseRollbackStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseSavepointIdent(): TOffset;
    function ParseSavepointStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseSchedule(): TOffset;
    function ParseShowAuthorsStmt(): TOffset;
    function ParseShowBinaryLogsStmt(): TOffset;
    function ParseShowBinlogEventsStmt(): TOffset;
    function ParseShowCharacterSetStmt(): TOffset;
    function ParseShowCollationStmt(): TOffset;
    function ParseShowContributorsStmt(): TOffset;
    function ParseSelectStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseSelectStmtField(): TOffset;
    function ParseSelectStmtGroup(): TOffset;
    function ParseSelectStmtGroups(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseSelectStmtOrder(): TOffset;
    function ParseServerOptionList(): TOffset;
    function ParseSetStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseSetStmtAssignment(): TOffset;
    function ParseStartTransactionStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseString(): TOffset;
    function ParseSubArea(const ParseNode: TParseFunction): TOffset;
    function ParseSubPartition(): TOffset;
    function ParseStmt(const PL_SQL: Boolean = False): TOffset;
    function ParseTableIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseTableReference(): TOffset;
    function ParseTag(const KeywordIndex1: TWordList.TIndex; const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1; const KeywordIndex4: TWordList.TIndex = -1): TOffset;
    function ParseToken(): TOffset;
    function ParseTransactionCharacteristic(): TOffset;
    function ParseTruncateTableStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseUnknownStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseUnlockStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseUpdateStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseUpdatePair(): TOffset;
    function ParseUser(): TOffset;
    function ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const Brackets: Boolean; const ParseItem: TParseFunction): TOffset; overload;
    function ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const OptionIndices: TWordList.TIndices): TOffset; overload;
    function ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const Brackets: Boolean; const ParseItem: TParseFunction): TOffset; overload;
    function ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const OptionIndices: TWordList.TIndices): TOffset; overload;
    function ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset; overload;
    function ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ValueKeywordIndex1: TWordList.TIndex; const ValueKeywordIndex2: TWordList.TIndex = -1): TOffset; overload;
    function ParseValueIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseVariableIdent(): TOffset;
    function ParseViewIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseWhileStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseXAStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function RangeNodePtr(const ANode: TOffset): PRange; {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetError(const AErrorCode: Integer; const AErrorNode: TOffset = 0);
    function StmtNodePtr(const ANode: TOffset): PChild; {$IFNDEF Debug} inline; {$ENDIF}
    function StmtPtr(const ANode: TOffset): PStmt; {$IFNDEF Debug} inline; {$ENDIF}
    function TokenPtr(const ANode: TOffset): PToken; {$IFNDEF Debug} inline; {$ENDIF}

    property CurrentToken: TOffset read GetCurrentToken;
    property Error: Boolean read GetError;
    property ErrorCode: Integer read FErrorCode;
    property ErrorMessage: string read GetErrorMessage;
    property ParsedText: string read FParsedText;
    property NextToken[Index: Integer]: TOffset read GetNextToken;

  public
    constructor Create(const MySQLVersion: Integer = 0; const LowerCaseTableNames: Integer = 0);
    destructor Destroy(); override;
    function Parse(const Text: PChar; const Length: Integer): Boolean; overload;
    function Parse(const Text: string): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    procedure SaveToFile(const Filename: string; const FileType: TFileType = ftSQL);
    property AnsiQuotes: Boolean read FAnsiQuotes write FAnsiQuotes;
    property Functions: string read GetFunctions write SetFunctions;
    property HighNotPrecedence: Boolean read FHighNotPrecedence write FHighNotPrecedence;
    property LowerCaseTableNames: Integer read FLowerCaseTableNames write FLowerCaseTableNames;
    property Keywords: string read GetKeywords write SetKeywords;
    property MySQLVersion: Integer read FMySQLVersion write FMySQLVersion;
    property PipesAsConcat: Boolean read FPipesAsConcat write FPipesAsConcat;
    property Root: PRoot read GetRoot;
    property Text: string read FParsedText;
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

function WordIndices(const Index1: TMySQLParser.TWordList.TIndex;
  const Index2: TMySQLParser.TWordList.TIndex = -1;
  const Index3: TMySQLParser.TWordList.TIndex = -1;
  const Index4: TMySQLParser.TWordList.TIndex = -1;
  const Index5: TMySQLParser.TWordList.TIndex = -1;
  const Index6: TMySQLParser.TWordList.TIndex = -1): TMySQLParser.TWordList.TIndices;
begin
  Result[0] := Index1;
  Result[1] := Index2;
  Result[2] := Index3;
  Result[3] := Index4;
  Result[4] := Index5;
  Result[5] := Index6;
end;

{ TMySQLSQLParser.TWordList **************************************************}

procedure TMySQLParser.TWordList.Clear();
begin
  FText := '';

  FCount := 0;
  SetLength(FIndex, 0);

  SetLength(Parser.OperatorTypeByKeywordIndex, 0);
end;

constructor TMySQLParser.TWordList.Create(const ASQLParser: TMySQLParser; const AText: string = '');
begin
  inherited Create();

  FParser := ASQLParser;

  FCount := 0;
  SetLength(FIndex, 0);

  Text := AText;
end;

destructor TMySQLParser.TWordList.Destroy();
begin
  Clear();

  inherited;
end;

function TMySQLParser.TWordList.GetWord(Index: TWordList.TIndex): string;
begin
  Result := StrPas(FIndex[Index]);
end;

function TMySQLParser.TWordList.IndexOf(const Word: PChar; const Length: Integer): Integer;
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

function TMySQLParser.TWordList.IndexOf(const Word: string): Integer;
begin
  Result := IndexOf(PChar(Word), Length(Word));
end;

procedure TMySQLParser.TWordList.SetText(AText: string);
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

{ TMySQLSQLParser.TNode ******************************************************}

class function TMySQLParser.TNode.Create(const AParser: TMySQLParser; const ANodeType: TNodeType): TOffset;
begin
  Result := AParser.NewNode(ANodeType);

  AParser.NodePtr(Result)^.FParser := AParser;
  AParser.NodePtr(Result)^.FNodeType := ANodeType;
end;

function TMySQLParser.TNode.GetOffset(): TOffset;
begin
  Result := @Self - Parser.FNodes.Mem;
end;

{ TMySQLSQLParser.TChild *****************************************************}

function TMySQLParser.TChild.GetFFirstToken(): TOffset;
begin
  if (NodeType = ntToken) then
    Result := @Self - Parser.FNodes.Mem
  else
  begin
    Assert(Parser.IsRange(@Self));
    Result := TMySQLParser.PRange(@Self).FFirstToken;
  end;
end;

function TMySQLParser.TChild.GetFirstToken(): PToken;
begin
  if (NodeType = ntToken) then
    Result := @Self
  else
  begin
    Assert(Parser.IsRange(@Self));
    Result := PRange(@Self).FirstToken;
  end;
end;

function TMySQLParser.TChild.GetFLastToken(): TOffset;
begin
  if (NodeType = ntToken) then
    Result := PNode(@Self)^.Offset
  else
  begin
    Assert(Parser.IsRange(@Self));
    Result := PRange(@Self)^.FLastToken;
  end;
end;

function TMySQLParser.TChild.GetLastToken(): PToken;
begin
  if (NodeType = ntToken) then
    Result := @Self
  else
  begin
    Assert(Parser.IsRange(@Self));
    Result := PRange(@Self).LastToken;
  end;
end;

function TMySQLParser.TChild.GetNextSibling(): PChild;
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

function TMySQLParser.TChild.GetParentNode(): PNode;
begin
  Result := Parser.NodePtr(FParentNode);
end;

{ TMySQLSQLParser.TToken *****************************************************}

class function TMySQLParser.TToken.Create(const AParser: TMySQLParser;
  const ASQL: PChar; const ALength: Integer; const AOrigin: TOrigin;
  const AErrorCode: Integer; const AMySQLVersion: Integer; const ATokenType: fspTypes.TTokenType;
  const AOperatorType: TOperatorType; const AKeywordIndex: TWordList.TIndex; const AUsageType: TUsageType): TOffset;
begin
  Result := TNode.Create(AParser, ntToken);

  with PToken(AParser.NodePtr(Result))^ do
  begin
    Heritage.Heritage.FParser := AParser;
    FDbIdentType := ditUnknown;
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

function TMySQLParser.TToken.GetAsString(): string;
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
    ttDQIdent:
      Result := SQLUnescape(Text);
    ttDBIdent:
      if ((Copy(Text, 1, 1) = '[') and (Copy(Text, Length(Text), 1) = ']')) then
        Result := Trim(Copy(Text, 1, Length(Text) - 2))
      else
        Result := Text;
    ttBRIdent:
      if ((Copy(Text, 1, 1) = '{') and (Copy(Text, Length(Text), 1) = '}')) then
        Result := Trim(Copy(Text, 1, Length(Text) - 2))
      else
        Result := Text;
    ttMySQLIdent:
      Result := SQLUnescape(Text);
    ttMySQLCodeStart:
      Result := Copy(Text, 1, Length(Text) - 3);
    ttCSString:
      Result := Copy(Text, 1, Length(Text) - 1);
    else
      Result := Text;
  end;
end;

function TMySQLParser.TToken.GetErrorMessage(): string;
begin
  Result := Parser.GetErrorMessage(ErrorCode);
end;

function TMySQLParser.TToken.GetGeneration(): Integer;
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

function TMySQLParser.TToken.GetIndex(): Integer;
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

function TMySQLParser.TToken.GetIsUsed(): Boolean;
begin
  Result := not (TokenType in [ttSpace, ttReturn, ttComment]) and ((MySQLVersion >= FMySQLVersion));
end;

function TMySQLParser.TToken.GetNextToken(): PToken;
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

function TMySQLParser.TToken.GetOffset(): TOffset;
begin
  Result := Heritage.Heritage.GetOffset();
end;

function TMySQLParser.TToken.GetParentNode(): PNode;
begin
  Result := Heritage.GetParentNode();
end;

function TMySQLParser.TToken.GetText(): string;
begin
//  if (FText.NewText = '') then
    SetString(Result, FText.SQL, FText.Length)
//  else
//    Result := FText.NewText;
end;

procedure TMySQLParser.TToken.SetText(AText: string);
begin
//  FText.NewText := AText;
end;

{ TMySQLSQLParser.TRange *****************************************************}

class function TMySQLParser.TRange.Create(const AParser: TMySQLParser; const ANodeType: TNodeType): TOffset;
begin
  Result := TNode.Create(AParser, ANodeType);
end;

function TMySQLParser.TRange.GetFirstToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FFirstToken));
end;

function TMySQLParser.TRange.GetLastToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FLastToken));
end;

function TMySQLParser.TRange.GetOffset(): TOffset;
begin
  Result := Heritage.Heritage.Offset;
end;

function TMySQLParser.TRange.GetParentNode(): PNode;
begin
  Result := PNode(Parser.NodePtr(FParentNode));
end;

procedure TMySQLParser.TRange.AddChild(const ChildNode: TOffset);
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

{ TMySQLSQLParser.TRoot ******************************************************}

class function TMySQLParser.TRoot.Create(const AParser: TMySQLParser): TOffset;
begin
  Result := TNode.Create(AParser, ntRoot);
end;

function TMySQLParser.TRoot.GetFirstStmt(): PStmt;
begin
  Result := PStmt(Parser.NodePtr(FFirstStmt));
end;

function TMySQLParser.TRoot.GetFirstToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FFirstToken));
end;

function TMySQLParser.TRoot.GetLastStmt(): PStmt;
begin
  Result := PStmt(Parser.NodePtr(FLastStmt));
end;

function TMySQLParser.TRoot.GetLastToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FLastToken));
end;

{ TMySQLSQLParser.TStmt ******************************************************}

class function TMySQLParser.TStmt.Create(const AParser: TMySQLParser; const AStmtType: TStmtType): TOffset;
begin
  Result := TRange.Create(AParser, NodeTypeByStmtType[AStmtType]);

  with AParser.StmtPtr(Result)^ do
  begin
    FStmtType := AStmtType;
  end;
end;

function TMySQLParser.TStmt.GetError(): Boolean;
begin
  Result := FErrorCode <> PE_Success;
end;

function TMySQLParser.TStmt.GetErrorMessage(): string;
begin
  Result := Parser.GetErrorMessage(ErrorCode);
end;

function TMySQLParser.TStmt.GetErrorToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FErrorToken));
end;

function TMySQLParser.TStmt.GetFirstToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FFirstToken));
end;

function TMySQLParser.TStmt.GetLastToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FLastToken));
end;

function TMySQLParser.TStmt.GetNextStmt(): PStmt;
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

{ TMySQLSQLParser.TAlterDatabase *********************************************}

class function TMySQLParser.TAlterDatabaseStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TAlterEvent ************************************************}

class function TMySQLParser.TAlterEventStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TAlterRoutine **********************************************}

class function TMySQLParser.TAlterRoutineStmt.Create(const AParser: TMySQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TAlterServerStmt *******************************************}

class function TMySQLParser.TAlterServerStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TAlterTableStmt.TAlterColumn *******************************}

class function TMySQLParser.TAlterTableStmt.TAlterColumn.Create(const AParser: TMySQLParser; const ANodes: TAlterColumn.TNodes): TOffset;
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

{ TMySQLSQLParser.TAlterTableStmt.TConvertTo *********************************}

class function TMySQLParser.TAlterTableStmt.TConvertTo.Create(const AParser: TMySQLParser; const ANodes: TConvertTo.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtConvertTo);

  with PConvertTo(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.ConvertToTag);
    Heritage.AddChild(ANodes.CharacterSetValue);
    Heritage.AddChild(ANodes.CollateValue);
  end;
end;

{ TMySQLSQLParser.TAlterTableStmt.TDropObject ********************************}

class function TMySQLParser.TAlterTableStmt.TDropObject.Create(const AParser: TMySQLParser; const ANodes: TDropObject.TNodes): TOffset;
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

{ TMySQLSQLParser.TAlterTableStmt.TExchangePartition *************************}

class function TMySQLParser.TAlterTableStmt.TExchangePartition.Create(const AParser: TMySQLParser; const ANodes: TExchangePartition.TNodes): TOffset;
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

{ TMySQLSQLParser.TAlterTableStmt.TReorganizePartition ***********************}

class function TMySQLParser.TAlterTableStmt.TReorganizePartition.Create(const AParser: TMySQLParser; const ANodes: TReorganizePartition.TNodes): TOffset;
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

{ TMySQLSQLParser.TAlterTableStmt ********************************************}

class function TMySQLParser.TAlterTableStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TCreateViewStmt ********************************************}

class function TMySQLParser.TAlterViewStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TBeginStmt *************************************************}

class function TMySQLParser.TBeginStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stBegin);

  with PBeginStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.BeginTag);
  end;
end;

{ TMySQLSQLParser.TBinaryOp **************************************************}

class function TMySQLParser.TBinaryOp.Create(const AParser: TMySQLParser; const AOperator, AOperand1, AOperand2: TOffset): TOffset;
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

function TMySQLParser.TBinaryOp.GetOperand1(): PChild;
begin
  Result := Parser.StmtNodePtr(FNodes.Operand1);
end;

function TMySQLParser.TBinaryOp.GetOperand2(): PChild;
begin
  Result := Parser.StmtNodePtr(FNodes.Operand2);
end;

function TMySQLParser.TBinaryOp.GetOperator(): PChild;
begin
  Result := Parser.StmtNodePtr(FNodes.Operator);
end;

{ TMySQLSQLParser.TBetweenOp *************************************************}

class function TMySQLParser.TBetweenOp.Create(const AParser: TMySQLParser; const AOperator1, AOperator2, AExpr, AMin, AMax: TOffset): TOffset;
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

{ TMySQLSQLParser.TCallStmt **************************************************}

class function TMySQLParser.TCallStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TCaseOp.TBranch ********************************************}

class function TMySQLParser.TCaseOp.TBranch.Create(const AParser: TMySQLParser; const ANodes: TBranch.TNodes): TOffset;
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

{ TMySQLSQLParser.TCaseOp ****************************************************}

class function TMySQLParser.TCaseOp.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TCaseStmt.TBranch ******************************************}

class function TMySQLParser.TCaseStmt.TBranch.Create(const AParser: TMySQLParser; const ANodes: TBranch.TNodes): TOffset;
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

{ TMySQLSQLParser.TCaseStmt **************************************************}

class function TMySQLParser.TCaseStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCaseStmt);

  with PCaseStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.BranchList);
    Heritage.AddChild(ANodes.EndTag);
  end;
end;

{ TMySQLSQLParser.TCloseStmt *************************************************}

class function TMySQLParser.TCloseStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stClose);

  with PCloseStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.CloseTag);
    Heritage.Heritage.AddChild(ANodes.CursorIdent);
  end;
end;

{ TMySQLSQLParser.TCommitStmt ************************************************}

class function TMySQLParser.TCommitStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCommit);

  with PCommitStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.CommitTag);
    Heritage.Heritage.AddChild(ANodes.ChainTag);
    Heritage.Heritage.AddChild(ANodes.ReleaseTag);
  end;
end;

{ TMySQLSQLParser.TColumn ****************************************************}

class function TMySQLParser.TColumn.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TCompoundStmt **********************************************}

class function TMySQLParser.TCompoundStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TCreateDatabaseStmt ****************************************}

class function TMySQLParser.TCreateDatabaseStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TCreateEventStmt *******************************************}

class function TMySQLParser.TCreateEventStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

class function TMySQLParser.TCreateIndexStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TCreateRoutineStmt *****************************************}

class function TMySQLParser.TCreateRoutineStmt.Create(const AParser: TMySQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TCreateTableStmt.TReference ********************************}

class function TMySQLParser.TCreateTableStmt.TReference.Create(const AParser: TMySQLParser; const ANodes: TReference.TNodes): TOffset;
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

{ TMySQLSQLParser.TCreateServerStmt ******************************************}

class function TMySQLParser.TCreateServerStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TCreateTableStmt *******************************************}

class function TMySQLParser.TCreateTableStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TCreateTriggerStmt *****************************************}

class function TMySQLParser.TCreateTriggerStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TCreateViewStmt ********************************************}

class function TMySQLParser.TCreateViewStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TDataType **************************************************}

class function TMySQLParser.TDataType.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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
    Heritage.AddChild(ANodes.CharacterSetValue);
    Heritage.AddChild(ANodes.CollateValue);
    Heritage.AddChild(ANodes.BinaryTag);
  end;
end;

{ TMySQLSQLParser.TDbIdent ***************************************************}

class function TMySQLParser.TDbIdent.Create(const AParser: TMySQLParser;
  const ADbIdentType: TDbIdentType; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntDbIdent);

  with PDbIdent(AParser.NodePtr(Result))^ do
  begin
    FDbIdentType := ADbIdentType;

    FNodes := ANodes;

    Heritage.AddChild(ANodes.Ident);
    Heritage.AddChild(ANodes.DatabaseDot);
    Heritage.AddChild(ANodes.DatabaseIdent);
    Heritage.AddChild(ANodes.TableDot);
    Heritage.AddChild(ANodes.TableIdent);
  end;
end;

class function TMySQLParser.TDbIdent.Create(const AParser: TMySQLParser; const ADbIdentType: TDbIdentType;
  const AIdent, ADatabaseDot, ADatabaseIdent, ATableDot, ATableIdent: TOffset): TOffset;
var
  Nodes: TNodes;
begin
  Nodes.Ident := AIdent;
  Nodes.DatabaseDot := ADatabaseDot;
  Nodes.DatabaseIdent := ADatabaseIdent;
  Nodes.TableDot := ATableDot;
  Nodes.TableIdent := ATableIdent;

  Result := Create(AParser, ADbIdentType, Nodes);
end;

function TMySQLParser.TDbIdent.GetDatabaseIdent(): PToken;
begin
  Result := Parser.TokenPtr(FNodes.DatabaseIdent);
end;

function TMySQLParser.TDbIdent.GetIdent(): PToken;
begin
  Result := Parser.TokenPtr(FNodes.Ident);
end;

function TMySQLParser.TDbIdent.GetParentNode(): PNode;
begin
  Result := Heritage.GetParentNode();
end;

function TMySQLParser.TDbIdent.GetTableIdent(): PToken;
begin
  Result := Parser.TokenPtr(FNodes.TableIdent);
end;

{ TMySQLSQLParser.TDeclareStmt ***********************************************}

class function TMySQLParser.TDeclareStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDeclare);

  with PDeclareStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.DeclareTag);
    Heritage.Heritage.AddChild(ANodes.IdentList);
    Heritage.Heritage.AddChild(ANodes.TypeNode);
    Heritage.Heritage.AddChild(ANodes.DefaultValue);
    Heritage.Heritage.AddChild(ANodes.CursorForTag);
    Heritage.Heritage.AddChild(ANodes.SelectStmt);
  end;
end;

{ TMySQLSQLParser.TDeleteStmt ************************************************}

class function TMySQLParser.TDeleteStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TDoStmt ****************************************************}

class function TMySQLParser.TDoStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDo);

  with PDoStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.DoTag);
    Heritage.Heritage.AddChild(ANodes.ExprList);
  end;
end;

{ TMySQLSQLParser.TDropDatabaseStmt ******************************************}

class function TMySQLParser.TDropDatabaseStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TDropEventStmt *********************************************}

class function TMySQLParser.TDropEventStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TDropIndexStmt *********************************************}

class function TMySQLParser.TDropIndexStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TDropRoutineStmt *******************************************}

class function TMySQLParser.TDropRoutineStmt.Create(const AParser: TMySQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TDropServerStmt ********************************************}

class function TMySQLParser.TDropServerStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TDropTableStmt *********************************************}

class function TMySQLParser.TDropTableStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TDropTriggerStmt *******************************************}

class function TMySQLParser.TDropTriggerStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TDropViewStmt **********************************************}

class function TMySQLParser.TDropViewStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TFetchStmt *************************************************}

class function TMySQLParser.TFetchStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stFetch);

  with PFetchStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.FetchTag);
    Heritage.Heritage.AddChild(ANodes.NextTag);
    Heritage.Heritage.AddChild(ANodes.FromTag);
    Heritage.Heritage.AddChild(ANodes.CursorIdent);
    Heritage.Heritage.AddChild(ANodes.IntoValue);
  end;
end;

{ TMySQLSQLParser.TForeignKey ************************************************}

class function TMySQLParser.TForeignKey.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TFunction **************************************************}

class function TMySQLParser.TFunction.Create(const AParser: TMySQLParser; const AIdent, AArguments: TOffset): TOffset;
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

function TMySQLParser.TFunction.GetArguments(): PChild;
begin
  Result := Parser.StmtNodePtr(FNodes.Arguments);
end;

function TMySQLParser.TFunction.GetIdent(): PChild;
begin
  Result := Parser.StmtNodePtr(FNodes.Ident);
end;

{ TMySQLSQLParser.TIfStmt.TBranch ********************************************}

class function TMySQLParser.TIfStmt.TBranch.Create(const AParser: TMySQLParser; const ANodes: TBranch.TNodes): TOffset;
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

{ TMySQLSQLParser.TIfStmt ****************************************************}

class function TMySQLParser.TIfStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stIf);

  with PIfStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.BranchList);
    Heritage.Heritage.AddChild(ANodes.EndTag);
  end;
end;

{ TMySQLSQLParser.TIgnoreLines ***********************************************}

class function TMySQLParser.TIgnoreLines.Create(const AParser: TMySQLParser; const ANodes: TIgnoreLines.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntIgnoreLines);

  with PIgnoreLines(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.IgnoreTag);
    Heritage.AddChild(ANodes.NumberToken);
    Heritage.AddChild(ANodes.LinesTag);
  end;
end;

{ TMySQLSQLParser.TItndex ****************************************************}

class function TMySQLParser.TIndex.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TKeyColumn *************************************************}

class function TMySQLParser.TIndexColumn.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TInsertStmt ************************************************}

class function TMySQLParser.TInsertStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TIterateStmt ***********************************************}

class function TMySQLParser.TIterateStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntIterateStmt);

  with PIterateStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.IterateToken);
    Heritage.AddChild(ANodes.LabelToken);
  end;
end;

{ TMySQLSQLParser.TLeaveStmt *************************************************}

class function TMySQLParser.TLeaveStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntLeaveStmt);

  with PLeaveStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.LeaveTag);
    Heritage.AddChild(ANodes.LabelToken);
  end;
end;

{ TMySQLSQLParser.TList ******************************************************}

class function TMySQLParser.TList.Create(const AParser: TMySQLParser; const ANodes: TNodes; const AChildrenCount: Integer; const AChildrens: array of TOffset): TOffset;
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

function TMySQLParser.TList.GetCount(): Integer;
var
  Child: PChild;
begin
  Result := 0;

  Child := FirstChild;
  while (Assigned(Child)) do
  begin
    Inc(Result);
    Child := Child^.NextSibling;
  end;
end;

function TMySQLParser.TList.GetFirstChild(): PChild;
begin
  Result := PChild(Parser.NodePtr(FNodes.FirstChild));
end;

{ TMySQLSQLParser.TLoadDataStmt **********************************************}

class function TMySQLParser.TLoadDataStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stLoadData);

  with PLoadDataStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.LoadDataTag);
    Heritage.Heritage.AddChild(ANodes.PriorityTag);
    Heritage.Heritage.AddChild(ANodes.LocalTag);
    Heritage.Heritage.AddChild(ANodes.InfileTag);
    Heritage.Heritage.AddChild(ANodes.FilenameString);
    Heritage.Heritage.AddChild(ANodes.ReplaceIgnoreTag);
    Heritage.Heritage.AddChild(ANodes.IntoTableValue);
    Heritage.Heritage.AddChild(ANodes.PartitionValue);
    Heritage.Heritage.AddChild(ANodes.CharacterSetValue);
    Heritage.Heritage.AddChild(ANodes.ColumnsTag);
    Heritage.Heritage.AddChild(ANodes.ColumnsTerminatedByValue);
    Heritage.Heritage.AddChild(ANodes.EnclosedByValue);
    Heritage.Heritage.AddChild(ANodes.EscapedByValue);
    Heritage.Heritage.AddChild(ANodes.LinesTag);
    Heritage.Heritage.AddChild(ANodes.StartingByValue);
    Heritage.Heritage.AddChild(ANodes.LinesTerminatedByValue);
    Heritage.Heritage.AddChild(ANodes.IgnoreLines);
    Heritage.Heritage.AddChild(ANodes.ColumnList);
    Heritage.Heritage.AddChild(ANodes.SetList);
  end;
end;

{ TMySQLSQLParser.TLoadXMLStmt ***********************************************}

class function TMySQLParser.TLoadXMLStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stLoadXML);

  with PLoadXMLStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

  end;
end;

{ TMySQLSQLParser.TLockStmt.TItem ********************************************}

class function TMySQLParser.TLockStmt.TItem.Create(const AParser: TMySQLParser; const ANodes: TItem.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntLockStmtItem);

  with PItem(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.TableIdent);
    Heritage.AddChild(ANodes.AsTag);
    Heritage.AddChild(ANodes.AliasIdent);
    Heritage.AddChild(ANodes.TypeTag);
  end;
end;

{ TMySQLSQLParser.TLockStmt **************************************************}

class function TMySQLParser.TLockStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stLock);

  with PLockStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.LockTablesTag);
    Heritage.Heritage.AddChild(ANodes.ItemList);
  end;
end;

{ TMySQLSQLParser.TLoopStmt **************************************************}

class function TMySQLParser.TLoopStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TOpenStmt **************************************************}

class function TMySQLParser.TOpenStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stOpen);

  with POpenStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.OpenTag);
    Heritage.Heritage.AddChild(ANodes.CursorIdent);
  end;
end;

{ TMySQLSQLParser.TRenameTableStmt *******************************************}

class function TMySQLParser.TRenameStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRenameTable);

  with PRenameStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.RenameTag);
    Heritage.Heritage.AddChild(ANodes.RenameList);
  end;
end;

{ TMySQLSQLParser.TRenameStmt.TPair ******************************************}

class function TMySQLParser.TRenameStmt.TPair.Create(const AParser: TMySQLParser; const ANodes: TPair.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntRenameStmtPair);

  with PPair(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.OrgTableIdent);
    Heritage.AddChild(ANodes.ToTag);
    Heritage.AddChild(ANodes.NewTableIdent);
  end;
end;

{ TMySQLSQLParser.TReleaseStmt ***********************************************}

class function TMySQLParser.TReleaseStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRelease);

  with PReleaseStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.ReleaseTag);
    Heritage.Heritage.AddChild(ANodes.Ident);
  end;
end;

{ TMySQLSQLParser.TRepeatStmt ************************************************}

class function TMySQLParser.TRepeatStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TPartition *************************************************}

class function TMySQLParser.TPartition.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TPartitionValues *******************************************}

class function TMySQLParser.TPartitionValues.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntPartitionValues);

  with PPartitionValues(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.ValuesTag);
    Heritage.AddChild(ANodes.DescriptionValue);
  end;
end;

{ TMySQLSQLParser.TRollbackStmt **********************************************}

class function TMySQLParser.TRollbackStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRollback);

  with PRollbackStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.RollbackTag);
    Heritage.Heritage.AddChild(ANodes.ChainTag);
    Heritage.Heritage.AddChild(ANodes.ReleaseTag);
    Heritage.Heritage.AddChild(ANodes.ToValue);
  end;
end;

{ TMySQLSQLParser.TRoutineParam **********************************************}

class function TMySQLParser.TRoutineParam.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntRoutineParam);

  with PRoutineParam(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.IdentToken);
    Heritage.AddChild(ANodes.DataTypeNode);
  end;
end;

{ TMySQLSQLParser.TSavepointStmt *********************************************}

class function TMySQLParser.TSavepointStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSavepoint);

  with PSavepointStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.SavepointTag);
    Heritage.Heritage.AddChild(ANodes.Ident);
  end;
end;

{ TMySQLSQLParser.TSelectStmt.TColumn ****************************************}

class function TMySQLParser.TSelectStmt.TField.Create(const AParser: TMySQLParser; const ANodes: TField.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtField);

  with PField(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.Value);
    Heritage.AddChild(ANodes.AsTag);
    Heritage.AddChild(ANodes.AliasIdent);
  end;
end;

{ TMySQLSQLParser.TSelectStmt.TTableFactor.TIndexHint ************************}

class function TMySQLParser.TSelectStmt.TTableFactor.TIndexHint.Create(const AParser: TMySQLParser; const ANodes: TIndexHint.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactorIndexHint);

  with TSelectStmt.TTableFactor.PIndexHint(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.KindTag);
    Heritage.AddChild(ANodes.IndexTag);
    Heritage.AddChild(ANodes.ForValue);
  end;
end;

{ TMySQLSQLParser.TSelectStmt.TTableFactor ***********************************}

class function TMySQLParser.TSelectStmt.TTableFactor.Create(const AParser: TMySQLParser; const ANodes: TTableFactor.TNodes): TOffset;
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

{ TMySQLSQLParser.TSelectStmt.TTableFactorOj *********************************}

class function TMySQLParser.TSelectStmt.TTableFactorOj.Create(const AParser: TMySQLParser; const ANodes: TTableFactorOj.TNodes): TOffset;
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

{ TMySQLSQLParser.TSelectStmt.TTableFactorReferences *************************}

class function TMySQLParser.TSelectStmt.TTableFactorReferences.Create(const AParser: TMySQLParser; const ANodes: TTableFactorReferences.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactorReferences);

  with TSelectStmt.PTableFactorReferences(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.ReferenceList);
  end;
end;

{ TMySQLSQLParser.TSelectStmt.TTableFactorSelect *****************************}

class function TMySQLParser.TSelectStmt.TTableFactorSelect.Create(const AParser: TMySQLParser; const ANodes: TTableFactorSelect.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactorSelect);

  with TSelectStmt.PTableFactorSelect(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.SelectStmt);
  end;
end;

{ TMySQLSQLParser.TSelectStmt.TJoin ******************************************}

class function TMySQLParser.TSelectStmt.TJoin.Create(const AParser: TMySQLParser; const ALeftTable: TOffset; const AJoinType: TJoinType; const ARightTable: TOffset; const ACondition: TOffset; const AKeywordTokens: TKeywordTokens): TOffset;
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

{ TMySQLSQLParser.TSelectStmt.TGroup *****************************************}

class function TMySQLParser.TSelectStmt.TGroup.Create(const AParser: TMySQLParser; const ANodes: TGroup.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtGroup);

  with PGroup(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.Expr);
    Heritage.AddChild(ANodes.Direction);
  end;
end;

{ TMySQLSQLParser.TSelectStmt.TGroups ****************************************}

class function TMySQLParser.TSelectStmt.TGroups.Create(const AParser: TMySQLParser; const ANodes: TGroups.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtGroups);

  with PGroups(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.GroupByTag);
    Heritage.AddChild(ANodes.ColumnList);
    Heritage.AddChild(ANodes.WithRollupTag);
  end;
end;

{ TMySQLSQLParser.TSelectStmt.TOrder *****************************************}

class function TMySQLParser.TSelectStmt.TOrder.Create(const AParser: TMySQLParser; const ANodes: TOrder.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtOrder);

  with POrder(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.Expr);
    Heritage.AddChild(ANodes.Direction);
  end;
end;

{ TMySQLSQLParser.TSelectStmt ************************************************}

class function TMySQLParser.TSelectStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSelect);

  with PSelectStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.SelectTag);
    Heritage.Heritage.AddChild(ANodes.DistinctTag);
    Heritage.Heritage.AddChild(ANodes.ColumnsList);
    Heritage.Heritage.AddChild(ANodes.FromValue);
    Heritage.Heritage.AddChild(ANodes.WhereValue);
    Heritage.Heritage.AddChild(ANodes.GroupByNode);
    Heritage.Heritage.AddChild(ANodes.HavingValue);
    Heritage.Heritage.AddChild(ANodes.OrderByValue);
    Heritage.Heritage.AddChild(ANodes.Limit.LimitTag);
    Heritage.Heritage.AddChild(ANodes.Limit.OffsetTag);
    Heritage.Heritage.AddChild(ANodes.Limit.OffsetToken);
    Heritage.Heritage.AddChild(ANodes.Limit.CommaToken);
    Heritage.Heritage.AddChild(ANodes.Limit.RowCountToken);
  end;
end;

{ TMySQLSQLParser.TSchedule.TInterval ****************************************}

class function TMySQLParser.TSchedule.TInterval.Create(const AParser: TMySQLParser; const ANodes: TInterval.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntScheduleInterval);

  with PInterval(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.QuantityExp);
    Heritage.AddChild(ANodes.UnitTag);
  end;
end;

{ TMySQLSQLParser.TSchedule.TIntervalListItem ********************************}

class function TMySQLParser.TSchedule.TIntervalListItem.Create(const AParser: TMySQLParser; const ANodes: TIntervalListItem.TNodes): TOffset;
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

{ TMySQLSQLParser.TSchedule **************************************************}

class function TMySQLParser.TSchedule.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TSetStmt.TAssignment ***************************************}

class function TMySQLParser.TSetStmt.TAssignment.Create(const AParser: TMySQLParser; const ANodes: TAssignment.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSetStmtAssignment);

  with PAssignment(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.ScopeTag);
    Heritage.AddChild(ANodes.Variable);
    Heritage.AddChild(ANodes.AssignToken);
    Heritage.AddChild(ANodes.ValueExpr);
  end;
end;

{ TMySQLSQLParser.TSetStmt ***************************************************}

class function TMySQLParser.TSetStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSet);

  with PSetStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.SetTag);
    Heritage.Heritage.AddChild(ANodes.AssignmentList);
  end;
end;

{ TMySQLSQLParser.TSetPasswordStmt *******************************************}

class function TMySQLParser.TSetPasswordStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSetPassword);

  with PSetPasswordStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.SetTag);
    Heritage.Heritage.AddChild(ANodes.PasswordTag);
    Heritage.Heritage.AddChild(ANodes.ForValue);
    Heritage.Heritage.AddChild(ANodes.PasswordExpr);
  end;
end;

{ TMySQLSQLParser.TSetTransactionStmt.TCharacteristic ************************}

class function TMySQLParser.TSetTransactionStmt.TCharacteristic.Create(const AParser: TMySQLParser; const ANodes: TCharacteristic.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntTransactionCharacteristic);

  with PCharacteristic(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.KindTag);
    Heritage.AddChild(ANodes.Value);
  end;
end;

{ TMySQLSQLParser.TSetTransactionStmt ****************************************}

class function TMySQLParser.TSetTransactionStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSetTransaction);

  with PSetTransactionStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.SetTag);
    Heritage.Heritage.AddChild(ANodes.ScopeTag);
    Heritage.Heritage.AddChild(ANodes.TransactionTag);
    Heritage.Heritage.AddChild(ANodes.CharacteristicList);
  end;
end;

{ TMySQLSQLParser.TShowAuthorsStmt *******************************************}

class function TMySQLParser.TShowAuthorsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowAuthors);

  with PShowAuthorsStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes.StmtTag := ANodes.StmtTag;

    Heritage.Heritage.AddChild(ANodes.StmtTag);
  end;
end;

{ TMySQLSQLParser.TShowBinaryLogsStmt ****************************************}

class function TMySQLParser.TShowBinaryLogsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowBinaryLogs);

  with PShowBinaryLogsStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes.StmtTag := ANodes.StmtTag;

    Heritage.Heritage.AddChild(ANodes.StmtTag);
  end;
end;

{ TMySQLSQLParser.TShowBinlogEventsStmt **************************************}

class function TMySQLParser.TShowBinlogEventsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowBinlogEvents);

  with PShowBinlogEventsStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes.StmtTag := ANodes.StmtTag;
    FNodes.InValue := ANodes.InValue;
    FNodes.FromValue := ANodes.FromValue;
    FNodes.LimitTag := ANodes.LimitTag;
    FNodes.OffsetToken := ANodes.OffsetToken;
    FNodes.CommaToken := ANodes.CommaToken;
    FNodes.RowCountToken := ANodes.RowCountToken;

    Heritage.Heritage.AddChild(ANodes.StmtTag);
    Heritage.Heritage.AddChild(ANodes.InValue);
    Heritage.Heritage.AddChild(ANodes.FromValue);
    Heritage.Heritage.AddChild(ANodes.LimitTag);
    Heritage.Heritage.AddChild(ANodes.OffsetToken);
    Heritage.Heritage.AddChild(ANodes.CommaToken);
    Heritage.Heritage.AddChild(ANodes.RowCountToken);
  end;
end;

{ TMySQLSQLParser.TShowCharacterSetStmt **************************************}

class function TMySQLParser.TShowCharacterSetStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCharacterSet);

  with PShowCharacterSetStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes.StmtTag := ANodes.StmtTag;
    FNodes.LikeValue := ANodes.LikeValue;
    FNodes.WhereValue := ANodes.WhereValue;

    Heritage.Heritage.AddChild(ANodes.StmtTag);
    Heritage.Heritage.AddChild(ANodes.LikeValue);
    Heritage.Heritage.AddChild(ANodes.WhereValue);
  end;
end;

{ TMySQLSQLParser.TShowCollationStmt *****************************************}

class function TMySQLParser.TShowCollationStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCollation);

  with PShowCollationStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes.StmtTag := ANodes.StmtTag;
    FNodes.LikeValue := ANodes.LikeValue;
    FNodes.WhereValue := ANodes.WhereValue;

    Heritage.Heritage.AddChild(ANodes.StmtTag);
    Heritage.Heritage.AddChild(ANodes.LikeValue);
    Heritage.Heritage.AddChild(ANodes.WhereValue);
  end;
end;

{ TMySQLSQLParser.TShowContributorsStmt **************************************}

class function TMySQLParser.TShowContributorsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowContributors);

  with PShowContributorsStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes.StmtTag := ANodes.StmtTag;

    Heritage.Heritage.AddChild(ANodes.StmtTag);
  end;
end;

{ TMySQLSQLParser.TSoundsLikeOp **********************************************}

class function TMySQLParser.TSoundsLikeOp.Create(const AParser: TMySQLParser; const AOperator1, AOperator2: TOffset; const AOperand1, AOperand2: TOffset): TOffset;
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

{ TMySQLSQLParser.TStartTransactionStmt **************************************}

class function TMySQLParser.TStartTransactionStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stStartTransaction);

  with PStartTransactionStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.StartTransactionTag);
    Heritage.Heritage.AddChild(ANodes.RealOnlyTag);
    Heritage.Heritage.AddChild(ANodes.ReadWriteTag);
    Heritage.Heritage.AddChild(ANodes.WithConsistentSnapshotTag);
  end;
end;

{ TMySQLSQLParser.TSubArea ***************************************************}

class function TMySQLParser.TSubArea.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TSubPartition **********************************************}

class function TMySQLParser.TSubPartition.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TTag *******************************************************}

class function TMySQLParser.TTag.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TTruncateStmt **********************************************}

class function TMySQLParser.TTruncateStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stTruncate);

  with PTruncateStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.TruncateTag);
    Heritage.Heritage.AddChild(ANodes.TableTag);
    Heritage.Heritage.AddChild(ANodes.TableIdent);
  end;
end;

{ TMySQLSQLParser.TUnaryOp ***************************************************}

class function TMySQLParser.TUnaryOp.Create(const AParser: TMySQLParser; const AOperator, AOperand: TOffset): TOffset;
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

{ TMySQLSQLParser.TUnknownStmt ***********************************************}

class function TMySQLParser.TUnknownStmt.Create(const AParser: TMySQLParser; const ATokens: Classes.TList): TOffset;
var
  I: Integer;
begin
  Result := TStmt.Create(AParser, stUnknown);

  with PUnknownStmt(AParser.NodePtr(Result))^ do
  begin
    for I := 0 to ATokens.Count - 1 do
      Heritage.Heritage.AddChild(Integer(ATokens[I]));
  end;
end;

{ TMySQLSQLParser.TUnlockStmt ************************************************}

class function TMySQLParser.TUnlockStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stUnlock);

  with PUnlockStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.UnlockTablesTag);
  end;
end;

{ TMySQLSQLParser.TUpdateStmt ************************************************}

class function TMySQLParser.TUpdateStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stUpdate);

  with PUpdateStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.UpdateTag);
    Heritage.Heritage.AddChild(ANodes.PriorityTag);
    Heritage.Heritage.AddChild(ANodes.TableReferences);
    Heritage.Heritage.AddChild(ANodes.SetValue);
    Heritage.Heritage.AddChild(ANodes.WhereValue);
    Heritage.Heritage.AddChild(ANodes.OrderByValue);
    Heritage.Heritage.AddChild(ANodes.LimitValue);
  end;
end;

{ TMySQLSQLParser.TUser ******************************************************}

class function TMySQLParser.TUser.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TValue *****************************************************}

class function TMySQLParser.TValue.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TVariable **************************************************}

class function TMySQLParser.TVariable.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntVariable);

  with PVariable(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.IdentTag);
    Heritage.AddChild(ANodes.At2Token);
    Heritage.AddChild(ANodes.At1Token);
    Heritage.AddChild(ANodes.ScopeTag);
    Heritage.AddChild(ANodes.DotToken);
    Heritage.AddChild(ANodes.IdentTag);
  end;
end;

{ TMySQLSQLParser.TWhileStmt *************************************************}

class function TMySQLParser.TWhileStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
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

{ TMySQLSQLParser.TXAStmt ****************************************************}

class function TMySQLParser.TXAStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stXA);

  with PXAStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.XATag);
    Heritage.Heritage.AddChild(ANodes.ActionTag);
    Heritage.Heritage.AddChild(ANodes.Ident);
    Heritage.Heritage.AddChild(ANodes.RestTag);
  end;
end;

{ TMySQLSQLParser ************************************************************}

function TMySQLParser.ApplyCurrentToken(const AUsageType: TUsageType = utUnknown; const ATokenType: fspTypes.TTokenType = fspTypes.ttUnknown): TOffset;
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

constructor TMySQLParser.Create(const MySQLVersion: Integer = 0; const LowerCaseTableNames: Integer = 0);
begin
  inherited Create();

  FAnsiQuotes := False;
  FFunctions := TWordList.Create(Self);
  FHighNotPrecedence := False;
  FKeywords := TWordList.Create(Self);
  FLowerCaseTableNames := LowerCaseTableNames;
  FMySQLVersion := MySQLVersion;
  FNodes.Mem := nil;
  FNodes.Offset := 0;
  FNodes.Size := 0;
  FParsedTokens := Classes.TList.Create();
  FPipesAsConcat := False;

  Functions := MySQLFunctions;
  Keywords := MySQLKeywords;
end;

destructor TMySQLParser.Destroy();
begin
  FFunctions.Free();
  FKeywords.Free();
  if (FNodes.Size > 0) then
    FreeMem(FNodes.Mem);
  FParsedTokens.Free();

  inherited;
end;

function TMySQLParser.EndOfStmt(const Token: TOffset): Boolean;
begin
  Result := (Token = 0) or (TokenPtr(Token)^.TokenType = ttDelimiter);
end;

function TMySQLParser.GetCurrentToken(): TOffset;
begin
  Result := GetParsedToken(0);
end;

function TMySQLParser.GetError(): Boolean;
begin
  Result := FErrorCode <> PE_Success;
end;

function TMySQLParser.GetErrorMessage(): string;
begin
  Result := GetErrorMessage(ErrorCode);
end;

function TMySQLParser.GetErrorMessage(const AErrorCode: Integer): string;
begin
  case (AErrorCode) of
    PE_Success: Result := '';
    PE_Unknown: Result := 'Unknown error';
    PE_EmptyText: Result := 'Text is empty';
    PE_Syntax: Result := 'Invalid or unexpected character';
    PE_IncompleteToken: Result := 'Incompleted Token';
    PE_UnexpectedToken: Result := 'Token unexpected';
    PE_UnkownStmt: Result := 'Unknown Statement';
    PE_IncompleteStmt: Result := 'Incompleted Statement';
    PE_InvalidEndLabel: Result := 'Begin and End Token are different';
    else Result := '[Unknown Error Code]';
  end;
end;

function TMySQLParser.GetFunctions(): string;
begin
  Result := FFunctions.Text;
end;

function TMySQLParser.GetKeywords(): string;
begin
  Result := FKeywords.Text;
end;

function TMySQLParser.GetNextToken(Index: Integer): TOffset;
begin
  Assert(Index > 0);

  Result := GetParsedToken(Index);
end;

function TMySQLParser.GetParsedToken(Index: Integer): TOffset;
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

function TMySQLParser.GetRoot(): PRoot;
begin
  Result := PRoot(NodePtr(FRoot));
end;

function TMySQLParser.IsChild(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and not (ANode^.NodeType in [ntUnknown, ntRoot]);
end;

function TMySQLParser.IsRange(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and not (ANode^.NodeType in [ntUnknown, ntRoot, ntToken]);
end;

function TMySQLParser.IsStmt(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType in [ntUnknownStmt, ntCompoundStmt, ntSelectStmt]);
end;

function TMySQLParser.IsToken(const ANode: TOffset): Boolean;
begin
  Result := IsToken(NodePtr(ANode));
end;

function TMySQLParser.IsToken(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType in [ntToken]);
end;

function TMySQLParser.NewNode(const ANodeType: TNodeType): TOffset;
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

function TMySQLParser.NodePtr(const ANode: TOffset): PNode;
begin
  if (ANode = 0) then
    Result := nil
  else
    Result := @FNodes.Mem[ANode];
end;

function TMySQLParser.NodeSize(const ANode: TOffset): Integer;
begin
  Result := NodeSize(NodePtr(ANode)^.NodeType);
end;

function TMySQLParser.NodeSize(const ANodeType: TNodeType): Integer;
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
    ntBeginStmt: Result := SizeOf(TBeginStmt);
    ntBetweenOp: Result := SizeOf(TBetweenOp);
    ntBinaryOp: Result := SizeOf(TBinaryOp);
    ntCallStmt: Result := SizeOf(TCallStmt);
    ntCaseOp: Result := SizeOf(TCaseOp);
    ntCaseOpBranch: Result := SizeOf(TCaseOp.TBranch);
    ntCaseStmt: Result := SizeOf(TCaseStmt);
    ntCaseStmtBranch: Result := SizeOf(TCaseStmt.TBranch);
    ntCloseStmt: Result := SizeOf(TCloseStmt);
    ntColumn: Result := SizeOf(TColumn);
    ntCommitStmt: Result := SizeOf(TCommitStmt);
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
    ntDeclareStmt: Result := SizeOf(TDeclareStmt);
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
    ntFetchStmt: Result := SizeOf(TFetchStmt);
    ntForeignKey: Result := SizeOf(TForeignKey);
    ntFunction: Result := SizeOf(TFunction);
    ntIfStmt: Result := SizeOf(TIfStmt);
    ntIfStmtBranch: Result := SizeOf(TIfStmt.TBranch);
    ntIgnoreLines: Result := SizeOf(TIgnoreLines);
    ntIndex: Result := SizeOf(TIndex);
    ntIndexColumn: Result := SizeOf(TIndexColumn);
    ntInsertStmt: Result := SizeOf(TInsertStmt);
    ntIterateStmt: Result := SizeOf(TIterateStmt);
    ntLeaveStmt: Result := SizeOf(TLeaveStmt);
    ntList: Result := SizeOf(TList);
    ntLoadDataStmt: Result := SizeOf(TLoadDataStmt);
    ntLoadXMLStmt: Result := SizeOf(TLoadXMLStmt);
    ntLockStmt: Result := SizeOf(TLockStmt);
    ntLockStmtItem: Result := SizeOf(TLockStmt.TItem);
    ntLoopStmt: Result := SizeOf(TLoopStmt);
    ntOpenStmt: Result := SizeOf(TOpenStmt);
    ntPartition: Result := SizeOf(TPartition);
    ntPartitionValues: Result := SizeOf(TPartitionValues);
    ntRenameStmt: Result := SizeOf(TRenameStmt);
    ntReleaseStmt: Result := SizeOf(TReleaseStmt);
    ntRepeatStmt: Result := SizeOf(TRepeatStmt);
    ntRollbackStmt: Result := SizeOf(TRollbackStmt);
    ntRoutineParam: Result := SizeOf(TRoutineParam);
    ntSavepointStmt: Result := SizeOf(TSavepointStmt);
    ntSchedule: Result := SizeOf(TSchedule);
    ntScheduleInterval: Result := SizeOf(TSchedule.TInterval);
    ntScheduleIntervalListItem: Result := SizeOf(TSchedule.TIntervalListItem);
    ntSelectStmt: Result := SizeOf(TSelectStmt);
    ntSelectStmtField: Result := SizeOf(TSelectStmt.TField);
    ntSelectStmtGroup: Result := SizeOf(TSelectStmt.TGroup);
    ntSelectStmtGroups: Result := SizeOf(TSelectStmt.TGroups);
    ntSelectStmtJoin: Result := SizeOf(TSelectStmt.TJoin);
    ntSelectStmtOrder: Result := SizeOf(TSelectStmt.TOrder);
    ntSelectStmtTableFactor: Result := SizeOf(TSelectStmt.TTableFactor);
    ntSelectStmtTableFactorIndexHint: Result := SizeOf(TSelectStmt.TTableFactor.TIndexHint);
    ntSelectStmtTableFactorOj: Result := SizeOf(TSelectStmt.TTableFactorOj);
    ntSelectStmtTableFactorSelect: Result := SizeOf(TSelectStmt.TTableFactorSelect);
    ntSetPasswordStmt: Result := SizeOf(TSetPasswordStmt);
    ntSetStmt: Result := SizeOf(TSetStmt);
    ntSetStmtAssignment: Result := SizeOf(TSetStmt.TAssignment);
    ntSetTransactionStmt: Result := SizeOf(TSetTransactionStmt);
    ntShowAuthorsStmt: Result := SizeOf(TShowAuthorsStmt);
    ntShowBinaryLogsStmt: Result := SizeOf(TShowBinaryLogsStmt);
    ntShowBinlogEventsStmt: Result := SizeOf(TShowBinlogEventsStmt);
    ntShowCharacterSetStmt: Result := SizeOf(TShowCharacterSetStmt);
    ntShowCollationStmt: Result := SizeOf(TShowCollationStmt);
    ntShowContributorsStmt: Result := SizeOf(TShowContributorsStmt);
    ntSoundsLikeOp: Result := SizeOf(TSoundsLikeOp);
    ntStartTransactionStmt: Result := SizeOf(TStartTransactionStmt);
    ntSubArea: Result := SizeOf(TSubArea);
    ntSubPartition: Result := SizeOf(TSubPartition);
    ntTag: Result := SizeOf(TTag);
    ntTruncateStmt: Result := SizeOf(TTruncateStmt);
    ntUnaryOp: Result := SizeOf(TUnaryOp);
    ntUnknownStmt: Result := SizeOf(TUnknownStmt);
    ntUnlockStmt: Result := SizeOf(TUnlockStmt);
    ntUpdateStmt: Result := SizeOf(TUpdateStmt);
    ntUser: Result := SizeOf(TUser);
    ntValue: Result := SizeOf(TValue);
    ntVariable: Result := SizeOf(TVariable);
    ntWhileStmt: Result := SizeOf(TWhileStmt);
    ntXAStmt: Result := SizeOf(TXAStmt);
    else raise ERangeError.Create(SArgumentOutOfRange);
  end;
end;

function TMySQLParser.Parse(const Text: PChar; const Length: Integer): Boolean;
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

  Root^.FFirstToken := CurrentToken;
  Root^.FFirstStmt := 0;

  while (not Error and (CurrentToken <> 0)) do
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

    if (not Error and IsToken(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttDelimiter)) then
      ApplyCurrentToken(); // ttDelimiter
  end;

  Result := not Error;
end;

function TMySQLParser.Parse(const Text: string): Boolean;
begin
  Result := Parse(PChar(Text), Length(Text));
end;

function TMySQLParser.ParseAlterDatabaseStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TAlterDatabaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSCHEMA)) then
    Nodes.DatabaseTag := ParseTag(kiSCHEMA)
  else
    Nodes.DatabaseTag := ParseTag(kiDATABASE);

  if (not Error) then
    Nodes.IdentTag := ParseDbIdent(ditDatabase);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUPGRADE)) then
    Nodes.UpgradeDataDirectoryNameTag := ParseTag(kiUPGRADE, kiDATA, kiDIRECTORY, kiNAME)
  else
  begin
    Found := True;
    while (not Error and Found and not EndOfStmt(CurrentToken)) do
      if ((Nodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
        Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaAuto, ParseCharacterSetIdent)
      else if ((Nodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
        Nodes.CharacterSetValue := ParseValue(kiCOLLATE, vaAuto, ParseCollateIdent)
      else if ((Nodes.CollateValue = 0) and not EndOfStmt(NextToken[1]) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
        Nodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseCharacterSetIdent)
      else if ((Nodes.CollateValue = 0) and not EndOfStmt(NextToken[1]) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
        Nodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaAuto, ParseCollateIdent)
      else
        Found := False;
  end;

  Result := TAlterDatabaseStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterEventStmt(): TOffset;
var
  Nodes: TAlterEventStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    Nodes.EventTag := ParseTag(kiEVENT);

  if (not Error) then
    Nodes.EventIdent := ParseEventIdent();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSCHEDULE)) then
    Nodes.OnScheduleValue := ParseValue(WordIndices(kiON, kiSCHEDULE), vaNo, ParseSchedule);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOMPLETION)) then
    if (not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiNOT)) then
      Nodes.OnCompletitionTag := ParseTag(kiON, kiCOMPLETION, kiNOT, kiPRESERVE)
    else
      Nodes.OnCompletitionTag := ParseTag(kiON, kiCOMPLETION, kiPRESERVE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiRENAME)) then
    Nodes.RenameValue := ParseValue(WordIndices(kiRENAME, kiTO), vaNo, ParseEventIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
    Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString);

  if (not Error) then
  begin
    Nodes.DoTag := ParseTag(kiDO);

    if (not Error) then
      Nodes.Body := ParsePL_SQLStmt();
  end;

  Result := TAlterEventStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterRoutineStmt(const ARoutineType: TRoutineType): TOffset;
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
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLANGUAGE) then
      Nodes.LanguageTag := ParseTag(kiCOMMENT, kiSQL)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCONTAINS) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiCONTAINS, kiSQL)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiNO) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiNO, kiSQL)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREADS) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiREADS, kiSQL, kiDATA)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiMODIFIES) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiMODIFIES, kiSQL, kiDATA)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL) then
      if (EndOfStmt(NextToken[2])) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[2])^.KeywordIndex = kiDEFINER) then
        Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiDEFINER)
      else
        Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiINVOKER)
    else
      Found := False;

  Result := TAlterRoutineStmt.Create(Self, ARoutineType, Nodes);
end;

function TMySQLParser.ParseAlterServerStmt(): TOffset;
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

function TMySQLParser.ParseAlterTableStmt(): TOffset;
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

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE)) then
    Nodes.IgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    Nodes.IdentNode := ParseDbIdent(ditTable);

  DelimiterFound := False; DelimiterExpected := False;
  while (not Error and (DelimiterFound or not DelimiterExpected) and not EndOfStmt(CurrentToken)) do
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
      if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiTO)) then
        Nodes.RenameNode := ParseValue(WordIndices(kiRENAME, kiTO), vaNo, ParseTableIdent)
      else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiAS)) then
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
      Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaAuto, ParseCharacterSetIdent);
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
      Nodes.TableOptionsNodes.CollateValue := ParseValue(WordIndices(kiCOLLATE), vaAuto, ParseCharacterSetIdent);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.CollateValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARACTER)) then
    begin
      Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseCharacterSetIdent);
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.CharacterSetValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.CollateValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOLLATE)) then
    begin
      Nodes.TableOptionsNodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaAuto, ParseCollateIdent);
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
      Nodes.TableOptionsNodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseEngineIdent);
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
      Nodes.TableOptionsNodes.InsertMethodValue := ParseValue(kiINSERT_METHOD, vaAuto, WordIndices(kiNO, kiFIRST, kiLAST));
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
      Nodes.TableOptionsNodes.RowFormatValue := ParseValue(kiROW_FORMAT, vaAuto, WordIndices(kiDEFAULT, kiDYNAMIC, kiFIXED, kiCOMPRESSED, kiREDUNDANT, kiCOMPACT));
      Specifications.Add(Pointer(Nodes.TableOptionsNodes.RowFormatValue));
      DelimiterExpected := False;
    end
    else if ((Nodes.TableOptionsNodes.StatsAutoRecalcValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTATS_AUTO_RECALC)) then
    begin
      if (EndOfStmt(NextToken[1])) then
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
      if (EndOfStmt(NextToken[1])) then
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

    if (DelimiterExpected and not EndOfStmt(CurrentToken) and not (TokenPtr(CurrentToken)^.TokenType in [ttComma, ttDelimiter])) then
      SetError(PE_UnexpectedToken)
    else
    begin
      DelimiterFound := not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma);
      if (DelimiterFound) then
        Specifications.Add(Pointer(ApplyCurrentToken()));
    end;
  end;

  FillChar(SpecificationsNodes, SizeOf(SpecificationsNodes), 0);
  Nodes.SpecificationList := TList.Create(Self, SpecificationsNodes, Specifications.Count, TIntegerArray(Specifications.List));
  Result := TAlterTableStmt.Create(Self, Nodes);

  Specifications.Free();
end;

function TMySQLParser.ParseAlterTableStmtAlterColumn(): TOffset;
var
  Nodes: TAlterTableStmt.TAlterColumn.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOLUMN)) then
    Nodes.AlterTag := ParseTag(kiALTER, kiCOLUMN)
  else
    Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error) then
    Nodes.ColumnIdent := ParseColumnIdent();

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiSET) then
      Nodes.SetDefaultValue := ParseValue(WordIndices(kiSET, kiDEFAULT), vaNo, ParseString)
    else
      Nodes.DropDefaultTag := ParseTag(kiDROP);

  Result := TAlterTableStmt.TAlterColumn.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterTableStmtConvertTo(): TOffset;
var
  Nodes: TAlterTableStmt.TConvertTo.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ConvertToTag := ParseTag(kiCONVERT, kiTO);

  if (not Error) then
    Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseCharacterSetIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
    Nodes.CollateValue := ParseValue(kiCOLLATE, vaNo, ParseCollateIdent);

  Result := TAlterTableStmt.TConvertTo.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterTableStmtDropObject(): TOffset;
var
  Nodes: TAlterTableStmt.TDropObject.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error) then
    Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
  begin
    if (EndOfStmt(CurrentToken)) then
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
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          Nodes.NameNode := ApplyCurrentToken();
    end;
  end;

  Result := TAlterTableStmt.TDropObject.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterTableStmtExchangePartition(): TOffset;
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

function TMySQLParser.ParseAlterTableStmtReorganizePartition(): TOffset;
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

function TMySQLParser.ParseAlterTableStmtUnion(): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(kiUNION);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
  begin
    TokenPtr(CurrentToken)^.FOperatorType := otAssign;
    Nodes.AssignToken := ApplyCurrentToken();
  end;

  if (not Error) then
    Nodes.ValueNode := ParseList(True, ParseTableIdent);

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterStmt(): TOffset;
var
  Index: Integer;
begin
  Assert(TokenPtr(CurrentToken)^.KeywordIndex = kiALTER);

  Result := 0;
  Index := 1;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiALGORITHM)) then
  begin
    Inc(Index);
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);

      if (EndOfStmt(NextToken[Index])) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiUNDEFINED)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiMERGE)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiTEMPTABLE)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index);
    end;
  end;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiDEFINER)) then
  begin
    Inc(Index);
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);

      if (EndOfStmt(NextToken[Index])) then
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

        if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.TokenType = ttAt)) then
        begin
          Inc(Index); // @
          if (not Error and not EndOfStmt(NextToken[Index])) then
            Inc(Index); // Servername
        end;
      end;
    end;
  end;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiSQL)) then
  begin
    Inc(Index);
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex <> kiSECURITY) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);
      if (EndOfStmt(NextToken[Index])) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiDEFINER)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiINVOKER)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index);
    end;
  end;

  if (not Error) then
    if (EndOfStmt(NextToken[Index])) then
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

function TMySQLParser.ParseAlterViewStmt(): TOffset;
var
  Nodes: TAlterViewStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.AlterTag := ParseTag(kiCREATE);

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
    Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaYes, ParseKeyword);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL)) then
    if (EndOfStmt(NextToken[2])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[2])^.KeywordIndex = kiDEFINER) then
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiDEFINER)
    else
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiINVOKER);

  if (not Error) then
    Nodes.ViewTag := ParseTag(kiVIEW);

  if (not Error) then
    Nodes.IdentNode := ParseDbIdent(ditView);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.Columns := ParseList(True, ParseColumnIdent);

  if (not Error) then
    Nodes.AsTag := ParseTag(kiAS);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiSELECT) then
      SetError(PE_UnexpectedToken)
    else
    begin
      Nodes.SelectStmt := ParseSelectStmt();

      if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
        if (EndOfStmt(NextToken[1])) then
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

function TMySQLParser.ParseBeginStmt(): TOffset;
var
  Nodes: TBeginStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiWORK)) then
    Nodes.BeginTag := ParseTag(kiBEGIN, kiWORK)
  else
    Nodes.BeginTag := ParseTag(kiBEGIN);

  Result := TBeginStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCallStmt(): TOffset;
var
  Nodes: TCallStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CallTag := ParseTag(kiCALL);

  if (not Error) then
    Nodes.ProcedureIdent := ParseDbIdent(ditProcedure);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.ParamList := ParseList(True, ParseExpr);

  Result := TCallStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCaseOp(): TOffset;
var
  Branches: array of TOffset;
  ListNodes: TList.TNodes;
  Nodes: TCaseOp.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  SetLength(Branches, 0);

  Nodes.CaseTag := ParseTag(kiCASE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN)) then
    Nodes.CompareExpr := ParseExpr();

  while (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHEN)) do
  begin
    SetLength(Branches, Length(Branches) + 1);
    Branches[Length(Branches) - 1] := ParseCaseOpBranch();
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE)) then
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

function TMySQLParser.ParseCaseOpBranch(): TOffset;
var
  Nodes: TCaseOp.TBranch.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHEN)) then
  begin
    Nodes.Tag := ParseTag(kiWHEN);

    if (not Error) then
      Nodes.ConditionExpr := ParseExpr();

    if (not Error) then
      Nodes.ThenTag := ParseTag(kiTHEN);
  end
  else if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE)) then
  begin
    Nodes.Tag := ParseTag(kiELSE);

    if (not Error) then
      Nodes.ConditionExpr := ParseExpr();
  end;

  if (not Error) then
    Nodes.ResultExpr := ParseExpr();

  Result := TCaseOp.TBranch.Create(Self, Nodes);
end;

function TMySQLParser.ParseCaseStmt(): TOffset;
var
  Branches: array of TOffset;
  ListNodes: TList.TNodes;
  Nodes: TCaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  SetLength(Branches, 0);

  Nodes.CaseTag := ParseTag(kiCASE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN)) then
    Nodes.CompareExpr := ParseExpr();

  while (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHEN)) do
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

function TMySQLParser.ParseCaseStmtBranch(): TOffset;
var
  Nodes: TCaseStmt.TBranch.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHEN)) then
  begin
    Nodes.Tag := ParseTag(kiWHEN);

    if (not Error) then
      Nodes.ConditionExpr := ParseExpr();

    if (not Error) then
      Nodes.ThenTag := ParseTag(kiTHEN);
  end
  else if (EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE)) then
    Nodes.Tag := ParseTag(kiELSE);

  if (not Error and not EndOfStmt(Nodes.Tag)) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter);

  Result := TCaseStmt.TBranch.Create(Self, Nodes);
end;

function TMySQLParser.ParseCharacterSetIdent(): TOffset;
begin
  Result := ParseDbIdent(ditCharacterSet);
end;

function TMySQLParser.ParseCollateIdent(): TOffset;
begin
  Result := ParseDbIdent(ditCollate);
end;

function TMySQLParser.ParseColumn(const Add: TCreateTableColumnAdd = caNone): TOffset;
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

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiNOT) then
      Nodes.Null := ParseTag(kiNOT, kiNULL)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiNULL) then
      Nodes.Null := ParseTag(kiNULL);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT)) then
    Nodes.DefaultValue := ParseValue(kiDEFAULT, vaNo, ParseExpr);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAUTO_INCREMENT)) then
    Nodes.AutoIncrementTag := ParseTag(kiAUTO_INCREMENT);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiUNIQUE) then
      if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
        Nodes.KeyTag := ParseTag(kiUNIQUE, kiKEY)
      else
        Nodes.KeyTag := ParseTag(kiUNIQUE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiPRIMARY) then
      Nodes.KeyTag := ParseTag(kiPRIMARY, kiKEY)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY) then
      Nodes.KeyTag := ParseTag(kiKEY);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT)) then
    Nodes.CommentValue := ParseValue(kiDEFAULT, vaNo, ParseString);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLUMN_FORMAT)) then
    Nodes.ColumnFormat := ParseValue(kiCOLUMN_FORMAT, vaNo, WordIndices(kiFIXED, kiDYNAMIC, kiDEFAULT));

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiREFERENCES)) then
    Nodes.ReferencesNode := ParseCreateTableStmtReferences();

  if (not Error and (Add <> caNone) and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiFIRST) then
      Nodes.Position := ParseTag(kiFIRST)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiAFTER) then
      Nodes.Position := ParseValue(kiAFTER, vaNo, ParseColumnIdent);

  Result := TColumn.Create(Self, Nodes);
end;

function TMySQLParser.ParseCloseStmt(): TOffset;
var
  Nodes: TCloseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CloseTag := ParseTag(kiCLOSE);

  if (not Error) then
    Nodes.CursorIdent := ParseDbIdent(ditCursor);

  Result := TCloseStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCommitStmt(): TOffset;
var
  Nodes: TCommitStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiWORK)) then
    Nodes.CommitTag := ParseTag(kiCOMMIT, kiWORK)
  else
    Nodes.CommitTag := ParseTag(kiCOMMIT);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAND)) then
    if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiNO)) then
      Nodes.CommitTag := ParseTag(kiAND, kiNO, kiCHAIN)
    else
      Nodes.CommitTag := ParseTag(kiAND, kiCHAIN);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(NextToken[1])^.KeywordIndex = kiNO) then
      Nodes.CommitTag := ParseTag(kiNO, kiRELEASE)
    else if (TokenPtr(NextToken[1])^.KeywordIndex = kiRELEASE) then
      Nodes.CommitTag := ParseTag(kiRELEASE);

  Result := TCommitStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCompoundStmt(): TOffset;
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

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)) then
    if ((Nodes.BeginLabelToken = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(Nodes.BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EndLabelToken := ApplyCurrentToken(utLabel, ttEndLabel);

  Result := TCompoundStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseColumnIdent(): TOffset;
begin
  Result := ParseDbIdent(ditColumn);
end;

function TMySQLParser.ParseCreateDatabaseStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TCreateDatabaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSCHEMA)) then
    Nodes.DatabaseTag := ParseTag(kiSCHEMA)
  else
    Nodes.DatabaseTag := ParseTag(kiDATABASE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfNotExistsTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    Nodes.DatabaseIdent := ParseDbIdent(ditDatabase);

  Found := True;
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
    if ((Nodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
      Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaAuto, ParseCharacterSetIdent)
    else if ((Nodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
      Nodes.CharacterSetValue := ParseValue(kiCOLLATE, vaAuto, ParseCollateIdent)
    else if ((Nodes.CollateValue = 0) and not EndOfStmt(NextToken[1]) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
      Nodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseCharacterSetIdent)
    else if ((Nodes.CollateValue = 0) and not EndOfStmt(NextToken[1]) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
      Nodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaAuto, ParseCollateIdent)
    else
      Found := False;

  Result := TCreateDatabaseStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateEventStmt(): TOffset;
var
  Nodes: TCreateEventStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiALTER);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    Nodes.EventTag := ParseTag(kiEVENT);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfNotExistsTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    Nodes.EventIdent := ParseEventIdent();

  if (not Error) then
    Nodes.OnScheduleValue := ParseValue(WordIndices(kiON, kiSCHEDULE), vaNo, ParseSchedule);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOMPLETION)) then
    if (not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiNOT)) then
      Nodes.OnCompletitionTag := ParseTag(kiON, kiCOMPLETION, kiNOT, kiPRESERVE)
    else
      Nodes.OnCompletitionTag := ParseTag(kiON, kiCOMPLETION, kiPRESERVE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
    Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString);

  if (not Error) then
    Nodes.DoTag := ParseTag(kiDO);

  if (not Error) then
    Nodes.Body := ParsePL_SQLStmt();

  Result := TCreateEventStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateIndexStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TCreateIndexStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiUNIQUE) then
      Nodes.IndexTag := ParseTag(kiUNIQUE, kiINDEX)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFULLTEXT) then
      Nodes.IndexTag := ParseTag(kiFULLTEXT, kiINDEX)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSPATIAL) then
      Nodes.IndexTag := ParseTag(kiSPATIAL, kiINDEX)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX) then
      Nodes.IndexTag := ParseTag(kiINDEX)
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.IndexIdent := ParseDbIdent(ditIndex);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING)) then
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
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
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

function TMySQLParser.ParseCreateRoutineStmt(const ARoutineType: TRoutineType): TOffset;
var
  Found: Boolean;
  Nodes: TCreateRoutineStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
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
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLANGUAGE) then
      Nodes.LanguageTag := ParseTag(kiCOMMENT, kiSQL)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiNOT) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiDETERMINISTIC)) then
      Nodes.DeterministicTag := ParseTag(kiNOT, kiDETERMINISTIC)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDETERMINISTIC) then
      Nodes.DeterministicTag := ParseTag(kiDETERMINISTIC)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCONTAINS) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiCONTAINS, kiSQL)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiNO) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiNO, kiSQL)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREADS) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiREADS, kiSQL, kiDATA)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiMODIFIES) then
      if (Nodes.CharacteristicTag <> 0) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.CharacteristicTag := ParseTag(kiMODIFIES, kiSQL, kiDATA)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL) then
      if (EndOfStmt(NextToken[2])) then
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

function TMySQLParser.ParseCreateServerStmt(): TOffset;
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

function TMySQLParser.ParseCreateStmt(): TOffset;
var
  Index: Integer;
begin
  Result := 0;
  Index := 1;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiOR)) then
  begin
    Inc(Index);
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex <> kiREPLACE) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
      Inc(Index);
  end;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiALGORITHM)) then
  begin
    Inc(Index);
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);

      if (EndOfStmt(NextToken[Index])) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiUNDEFINED)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiMERGE)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiTEMPTABLE)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index);
    end;
  end;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiDEFINER)) then
  begin
    Inc(Index);
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);

      if (EndOfStmt(NextToken[Index])) then
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

        if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.TokenType = ttAt)) then
        begin
          Inc(Index); // @
          if (not Error and not EndOfStmt(NextToken[Index])) then
            Inc(Index); // Servername
        end;
      end;
    end;
  end;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiSQL)) then
  begin
    Inc(Index);
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex <> kiSECURITY) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);
      if (EndOfStmt(NextToken[Index])) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiDEFINER)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiINVOKER)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index);
    end;
  end;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiTEMPORARY)) then
    Inc(Index);

  if (not Error) then
    if (EndOfStmt(NextToken[Index])) then
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

function TMySQLParser.ParseCreateTableStmt(): TOffset;
var
  Found: Boolean;
  ListNodes: TList.TNodes;
  Nodes: TCreateTableStmt.TNodes;
  TableOptions: Classes.TList;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTEMPORARY)) then
    Nodes.TemporaryTag := ParseTag(kiTEMPORARY);

  if (not Error) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.TemporaryTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken)^.TokenType = ttOpenBracket) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiLIKE)) then
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

      if (not Error and not EndOfStmt(CurrentToken)) then
      begin
        TableOptions := Classes.TList.Create();

        Found := True;
        while (not Error and Found and EndOfStmt(CurrentToken)) do
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
            Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaAuto, ParseCharacterSetIdent);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.CharacterSetValue));
          end
          else if ((Nodes.TableOptionsNodes.ChecksumValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHECKSUM)) then
          begin
            Nodes.TableOptionsNodes.AutoIncrementValue := ParseValue(kiCHECKSUM, vaAuto, ParseInteger);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.AutoIncrementValue));
          end
          else if ((Nodes.TableOptionsNodes.CollateValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
          begin
            Nodes.TableOptionsNodes.CollateValue := ParseValue(WordIndices(kiCOLLATE), vaAuto, ParseCollateIdent);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.CollateValue));
          end
          else if ((Nodes.TableOptionsNodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARACTER)) then
          begin
            Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseCharacterSetIdent);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.CharacterSetValue));
          end
          else if ((Nodes.TableOptionsNodes.CollateValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOLLATE)) then
          begin
            Nodes.TableOptionsNodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaAuto, ParseCollateIdent);
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
            Nodes.TableOptionsNodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseEngineIdent);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.EngineValue));
          end
          else if ((Nodes.TableOptionsNodes.IndexDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX)) then
          begin
            Nodes.TableOptionsNodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString);
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.IndexDirectoryValue));
          end
          else if ((Nodes.TableOptionsNodes.InsertMethodValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINSERT_METHOD)) then
          begin
            Nodes.TableOptionsNodes.InsertMethodValue := ParseValue(kiINSERT_METHOD, vaAuto, WordIndices(kiNO, kiFIRST, kiLAST));
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
            Nodes.TableOptionsNodes.RowFormatValue := ParseValue(kiROW_FORMAT, vaAuto, WordIndices(kiDEFAULT, kiDYNAMIC, kiFIXED, kiCOMPRESSED, kiREDUNDANT, kiCOMPACT));
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.RowFormatValue));
          end
          else if ((Nodes.TableOptionsNodes.StatsAutoRecalcValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTATS_AUTO_RECALC)) then
          begin
            if (EndOfStmt(NextToken[1])) then
              SetError(PE_IncompleteStmt)
            else if (TokenPtr(NextToken[1])^.TokenType = ttInteger) then
              Nodes.TableOptionsNodes.StatsAutoRecalcValue := ParseValue(kiSTATS_AUTO_RECALC, vaAuto, ParseInteger)
            else
              Nodes.TableOptionsNodes.StatsAutoRecalcValue := ParseValue(kiSTATS_AUTO_RECALC, vaAuto, WordIndices(kiDEFAULT));
            TableOptions.Add(Pointer(Nodes.TableOptionsNodes.StatsAutoRecalcValue));
          end
          else if ((Nodes.TableOptionsNodes.StatsPersistentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTATS_PERSISTENT)) then
          begin
            if (EndOfStmt(NextToken[1])) then
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

          if (Found and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma)) then
            TableOptions.Add(Pointer(ApplyCurrentToken()));
        end;

        Nodes.TableOptionList := TList.Create(Self, ListNodes, TableOptions.Count, TIntegerArray(TableOptions.List));

        TableOptions.Free();
      end;

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
      begin
        Nodes.PartitionOption.PartitionByTag := ParseTag(kiPARTITION, kiBY);

        if (not Error) then
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.KeywordIndex = kiHASH) then
            Nodes.PartitionOption.PartitionKindTag := ParseTag(kiHASH)
          else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLINEAR) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiHASH)) then
            Nodes.PartitionOption.PartitionKindTag := ParseTag(kiLINEAR, kiHASH)
          else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLINEAR) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
            Nodes.PartitionOption.PartitionKindTag := ParseTag(kiLINEAR, kiKEY)
          else if (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY) then
            Nodes.PartitionOption.PartitionKindTag := ParseTag(kiKEY)
          else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIST) then
            Nodes.PartitionOption.PartitionKindTag := ParseTag(kiLIST)
          else if (TokenPtr(CurrentToken)^.KeywordIndex = kiRANGE) then
            Nodes.PartitionOption.PartitionKindTag := ParseTag(kiRANGE)
          else
            SetError(PE_UnexpectedToken);

        if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
          Nodes.PartitionOption.PartitionAlgorithmValue := ParseValue(kiALGORITHM, vaAuto, ParseInteger);

        if (not Error) then
          Nodes.PartitionOption.PartitionExprList := ParseList(True, ParseExpr);

        if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITIONS)) then
          Nodes.PartitionOption.PartitionsValue := ParseValue(kiPARTITIONS, vaNo, ParseInteger);

        if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSUBPARTITION)) then
        begin
          Nodes.PartitionOption.SubPartitionByTag := ParseTag(kiSUBPARTITION, kiBY);

          if (not Error) then
            if (EndOfStmt(CurrentToken)) then
              SetError(PE_IncompleteStmt)
            else if (TokenPtr(CurrentToken)^.KeywordIndex = kiHASH) then
              Nodes.PartitionOption.SubPartitionKindTag := ParseTag(kiHASH)
            else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLINEAR) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiHASH)) then
              Nodes.PartitionOption.SubPartitionKindTag := ParseTag(kiLINEAR, kiHASH)
            else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLINEAR) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
              Nodes.PartitionOption.SubPartitionKindTag := ParseTag(kiLINEAR, kiKEY)
            else if (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY) then
              Nodes.PartitionOption.SubPartitionKindTag := ParseTag(kiKEY)
            else
              SetError(PE_UnexpectedToken);

          if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
            Nodes.PartitionOption.SubPartitionAlgorithmValue := ParseValue(kiALGORITHM, vaAuto, ParseInteger);

          if (not Error) then
            Nodes.PartitionOption.SubPartitionExprList := ParseList(True, ParseExpr);

          if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSUBPARTITIONS)) then
            Nodes.PartitionOption.SubPartitionsValue := ParseValue(kiSUBPARTITIONS, vaNo, ParseInteger);
        end;
      end;

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
        Nodes.PartitionDefinitionList := ParseList(True, ParsePartition);

      if (not Error and not EndOfStmt(CurrentToken)) then
        if (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE) then
          Nodes.IgnoreReplaceTag := ParseTag(kiIGNORE)
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREPLACE) then
          Nodes.IgnoreReplaceTag := ParseTag(kiREPLACE);

      if (not Error) then
        if (EndOfStmt(CurrentToken) and (Nodes.IgnoreReplaceTag > 0)) then
          SetError(PE_IncompleteStmt)
        else if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAS)) then
          Nodes.AsTag := ParseTag(kiAS);

      if (not Error) then
        if (EndOfStmt(CurrentToken) and ((Nodes.IgnoreReplaceTag > 0) or (Nodes.AsTag > 0))) then
          SetError(PE_IncompleteStmt)
        else if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSELECT)) then
          Nodes.SelectStmt := ParseSelectStmt()
        else
          SetError(PE_UnexpectedToken);

      if (not Error and (Nodes.OpenBracketToken = 0) and (Nodes.SelectStmt = 0)) then
        SetError(PE_IncompleteStmt);
    end;

  Result := TCreateTableStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateTableStmtDefinition(): TOffset;
begin
  Result := ParseCreateTableStmtDefinition(False);
end;

function TMySQLParser.ParseCreateTableStmtDefinition(const AlterTableStmt: Boolean): TOffset;
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

  if (EndOfStmt(NextToken[Index + 1])) then
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
    if (EndOfStmt(NextToken[Index + 2])) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(NextToken[Index + 2])^.TokenType <> ttString) and ((TokenPtr(NextToken[Index + 2])^.TokenType <> ttIdent) or (TokenPtr(NextToken[Index + 2])^.KeywordIndex >= 0))) then
      if ((TokenPtr(NextToken[Index + 2])^.KeywordIndex = kiPRIMARY) or (TokenPtr(NextToken[Index + 2])^.KeywordIndex = kiUNIQUE)) then
        SpecificationType := stIndex
      else if (TokenPtr(NextToken[Index + 2])^.KeywordIndex = kiFOREIGN) then
        SpecificationType := stForeignKey
      else
        SetError(PE_UnexpectedToken)
    else
      if (EndOfStmt(NextToken[Index + 3])) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index + 3])^.KeywordIndex = kiPRIMARY) or (TokenPtr(NextToken[Index + 3])^.KeywordIndex = kiUNIQUE)) then
        SpecificationType := stIndex
      else if (TokenPtr(NextToken[Index + 3])^.KeywordIndex = kiFOREIGN) then
        SpecificationType := stForeignKey
      else
        SetError(PE_UnexpectedToken);
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

function TMySQLParser.ParseCreateTableStmtReferences(): TOffset;
var
  Nodes: TCreateTableStmt.TReference.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ReferencesTag := ParseTag(kiREFERENCES);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  if (not Error) then
    Nodes.IndicesList := ParseList(True, ParseIndexColumn);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMATCH)) then
    if (not EndOfStmt(NextToken[1]) and ((TokenPtr(NextToken[1])^.KeywordIndex = kiFULL) or (TokenPtr(NextToken[1])^.KeywordIndex = kiPARTIAL) or (TokenPtr(NextToken[1])^.KeywordIndex = kiSIMPLE))) then
      Nodes.MatchValue := ParseValue(kiMATCH, vaNo, WordIndices(kiFULL, kiPARTIAL, kiSIMPLE));

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiDELETE)) then
    if (EndOfStmt(NextToken[2])) then
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
      SetError(PE_UnexpectedToken);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiUPDATE)) then
    if (EndOfStmt(NextToken[2])) then
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
      SetError(PE_UnexpectedToken);

  Result := TCreateTableStmt.TReference.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateTriggerStmt(): TOffset;
var
  Nodes: TCreateTriggerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    Nodes.TriggerTag := ParseTag(kiTRIGGER);

  if (not Error) then
    Nodes.TriggerIdent := ParseDbIdent(ditTrigger);

  if (not Error) then
    if (EndOfStmt(CurrentToken) or EndOfStmt(NextToken[1])) then
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
      SetError(PE_UnexpectedToken);

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

function TMySQLParser.ParseCreateViewStmt(): TOffset;
var
  Nodes: TCreateViewStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiOR)) then
    Nodes.OrReplaceTag := ParseTag(kiOR, kiREPLACE);

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
    Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaYes, ParseKeyword);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL)) then
    Nodes.SQLSecurityTag := ParseValue(WordIndices(kiSQL, kiSECURITY), vaNo, WordIndices(kiDEFINER, kiINVOKER));

  if (not Error) then
    Nodes.ViewTag := ParseTag(kiVIEW);

  if (not Error) then
    Nodes.IdentNode := ParseDbIdent(ditView);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.Columns := ParseList(True, ParseColumnIdent);

  if (not Error) then
    Nodes.AsTag := ParseTag(kiAS);

  if (not Error) then
    Nodes.SelectStmt := ParseSelectStmt();

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
    if (EndOfStmt(NextToken[1])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[1])^.KeywordIndex = kiCASCADED) then
      Nodes.OptionTag := ParseTag(kiWITH, kiCASCADED, kiCHECK, kiOPTION)
    else if (TokenPtr(NextToken[1])^.KeywordIndex = kiLOCAL) then
      Nodes.OptionTag := ParseTag(kiWITH, kiLOCAL, kiCHECK, kiOPTION)
    else
      Nodes.OptionTag := ParseTag(kiWITH, kiCHECK, kiOPTION);

  Result := TCreateViewStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDataType(): TOffset;
var
  IdentString: string;
  Nodes: TDataType.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
    SetError(PE_UnexpectedToken)
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
      SetError(PE_UnexpectedToken);

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
          if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
          begin
            Nodes.OpenBracketToken := ApplyCurrentToken();

            if (EndOfStmt(CurrentToken)) then
              SetError(PE_IncompleteStmt)
            else if (TokenPtr(CurrentToken)^.TokenType <> ttInteger) then
              SetError(PE_UnexpectedToken)
            else
              Nodes.LengthToken := ApplyCurrentToken();

            if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma)
              and ((IdentString = 'REAL')
              or (IdentString = 'DOUBLE')
              or (IdentString = 'FLOAT')
              or (IdentString = 'DECIMAL')
              or (IdentString = 'NUMERIC'))) then
            begin
              Nodes.CommaToken := ApplyCurrentToken();

              if (EndOfStmt(CurrentToken)) then
                SetError(PE_IncompleteStmt)
              else if (TokenPtr(CurrentToken)^.TokenType <> ttInteger) then
                SetError(PE_UnexpectedToken)
              else
                Nodes.DecimalsToken := ApplyCurrentToken();
            end;

            if (not Error) then
              if (EndOfStmt(CurrentToken)) then
                SetError(PE_IncompleteStmt)
              else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
                SetError(PE_UnexpectedToken)
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
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else
            SetError(PE_UnexpectedToken);

      if ((Nodes.DecimalsToken = 0)
        and ((IdentString = 'REAL')
        or (IdentString = 'DOUBLE')
        or (IdentString = 'FLOAT'))) then
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else
            SetError(PE_UnexpectedToken);
    end;
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUNSIGNED)
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

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiZEROFILL)
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

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiBINARY)
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
      if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
        Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseCharacterSetIdent);

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
        Nodes.CollateValue := ParseValue(kiCOLLATE, vaNo, ParseCollateIdent);
    end;

  Result := TDataType.Create(Self, Nodes);
end;

function TMySQLParser.ParseDbIdent(const ADbIdentType: TDbIdentType): TOffset;
var
  DbIdentType: TDbIdentType;
  Nodes: TDbIdent.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  DbIdentType := ADbIdentType;

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (ADbIdentType = ditAllFields) then
    if (TokenPtr(CurrentToken)^.OperatorType <> otMulti) then
      SetError(PE_UnexpectedToken)
    else
    begin
      DbIdentType := ditAllFields;
      TokenPtr(CurrentToken)^.FTokenType := ttIdent;
      TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
      TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
      Nodes.Ident := ApplyCurrentToken();
    end
  else if (((TokenPtr(CurrentToken)^.TokenType = ttIdent) or AnsiQuotes and (TokenPtr(CurrentToken)^.TokenType = ttDQIdent) or not AnsiQuotes and (TokenPtr(CurrentToken)^.TokenType = ttMySQLIdent))) then
    Nodes.Ident := ApplyCurrentToken()
  else
    SetError(PE_UnexpectedToken);

  if (not Error and not EndOfStmt(CurrentToken)
    and (TokenPtr(CurrentToken)^.OperatorType = otDot)) then
    case (ADbIdentType) of
      ditIndex,
      ditColumn,
      ditAllFields:
        begin
          Nodes.TableIdent := Nodes.Ident;

          Nodes.TableDot := ApplyCurrentToken();

          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
          begin
            DbIdentType := ditAllFields;
            TokenPtr(CurrentToken)^.FTokenType := ttIdent;
            TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
            TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
            Nodes.Ident := ApplyCurrentToken();
          end
          else if (((TokenPtr(CurrentToken)^.TokenType = ttIdent) or AnsiQuotes and (TokenPtr(CurrentToken)^.TokenType = ttDQIdent) or not AnsiQuotes and (TokenPtr(CurrentToken)^.TokenType = ttMySQLIdent))) then
            Nodes.Ident := ApplyCurrentToken()
          else
            SetError(PE_UnexpectedToken);

          if (not Error and not EndOfStmt(CurrentToken)
            and (TokenPtr(CurrentToken)^.OperatorType = otDot)) then
          begin
            Nodes.DatabaseIdent := Nodes.TableIdent;
            Nodes.DatabaseDot := Nodes.TableDot;
            Nodes.TableIdent := Nodes.Ident;

            Nodes.TableDot := ApplyCurrentToken();

            if (EndOfStmt(CurrentToken)) then
              SetError(PE_IncompleteStmt)
            else if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
            begin
              DbIdentType := ditAllFields;
              TokenPtr(CurrentToken)^.FTokenType := ttIdent;
              TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
              TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
              Nodes.Ident := ApplyCurrentToken();
            end
            else if (((TokenPtr(CurrentToken)^.TokenType = ttIdent) or AnsiQuotes and (TokenPtr(CurrentToken)^.TokenType = ttDQIdent) or not AnsiQuotes and (TokenPtr(CurrentToken)^.TokenType = ttMySQLIdent))) then
              Nodes.Ident := ApplyCurrentToken()
            else
              SetError(PE_UnexpectedToken);
          end;
        end;
      ditTable,
      ditFunction,
      ditProcedure,
      ditTrigger,
      ditView,
      ditEvent:
        begin
          Nodes.DatabaseIdent := Nodes.Ident;

          Nodes.DatabaseDot := ApplyCurrentToken();
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if (((TokenPtr(CurrentToken)^.TokenType = ttIdent) or AnsiQuotes and (TokenPtr(CurrentToken)^.TokenType = ttDQIdent) or not AnsiQuotes and (TokenPtr(CurrentToken)^.TokenType = ttMySQLIdent))) then
            Nodes.Ident := ApplyCurrentToken()
          else
            SetError(PE_UnexpectedToken);
        end;
    end;

  Result := TDbIdent.Create(Self, DbIdentType, Nodes);
end;

function TMySQLParser.ParseDefinerValue(): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(kiDEFINER);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
    SetError(PE_UnexpectedToken)
  else
  begin
    TokenPtr(CurrentToken)^.FOperatorType := otAssign;
    Nodes.AssignToken := ApplyCurrentToken();

    Nodes.ValueNode := ParseUser();
  end;

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseDeclareStmt(): TOffset;
var
  Nodes: TDeclareStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DeclareTag := ParseTag(kiDECLARE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCURSOR)) then
  begin
    Nodes.CursorForTag := ParseTag(kiCURSOR, kiFOR);

    if (not Error) then
      Nodes.SelectStmt := ParseSelectStmt();
  end
  else
  begin
    if (not Error) then
      Nodes.IdentList := ParseList(False, ParseValueIdent);

    if (not Error) then
      Nodes.TypeNode := ParseDataType();

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT)) then
      Nodes.DefaultValue := ParseValue(kiDEFAULT, vaNo, ParseExpr);
  end;

  Result := TDeclareStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDeleteStmt(): TOffset;
var
  Nodes: TDeleteStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DeleteTag := ParseTag(kiDELETE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLOW_PRIORITY)) then
    Nodes.LowPriorityTag := ParseTag(kiLOW_PRIORITY);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiQUICK)) then
    Nodes.QuickTag := ParseTag(kiQUICK);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE)) then
    Nodes.IgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    Nodes.FromTag := ParseTag(kiFROM);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
  begin
    Nodes.PartitionTag := ParseTag(kiPARTITION);

    if (not Error) then
      Nodes.PartitionList := ParseList(True, ParsePartitionIdent);
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE)) then
    Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiORDER)) then
    Nodes.OrderByValue := ParseValue(WordIndices(kiORDER, kiBY), vaNo, False, ParseColumnIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLIMIT)) then
    Nodes.LimitValue := ParseValue(kiLIMIT, vaNo, ParseInteger);

  Result := TDeleteStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDoStmt(): TOffset;
var
  Nodes: TDoStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DoTag := ParseTag(kiDO);

  if (not Error) then
    Nodes.ExprList := ParseList(True, ParseExpr);

  Result := TDoStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDropDatabaseStmt(): TOffset;
var
  Nodes: TDropDatabaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSCHEMA)) then
      Nodes.DatabaseTag := ParseTag(kiSCHEMA)
    else
      Nodes.DatabaseTag := ParseTag(kiDATABASE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.DatabaseIdent := ParseDbIdent(ditDatabase);

  Result := TDropDatabaseStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDropEventStmt(): TOffset;
var
  Nodes: TDropEventStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    Nodes.EventTag := ParseTag(kiEVENT);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.EventIdent := ParseDbIdent(ditEvent);

  Result := TDropEventStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDropIndexStmt(): TOffset;
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
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
    if ((Nodes.AlgorithmValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
      Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaAuto, WordIndices(kiDEFAULT, kiINPLACE, kiCOPY))
    else if ((Nodes.LockValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCK)) then
      Nodes.LockValue := ParseValue(kiLOCK, vaAuto, WordIndices(kiDEFAULT, kiNONE, kiSHARED, kiEXCLUSIVE))
    else
      Found := False;

  Result := TDropIndexStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDropRoutineStmt(const ARoutineType: TRoutineType): TOffset;
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

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.RoutineIdent := ParseDbIdent(ditFunction)
    else
      Nodes.RoutineIdent := ParseDbIdent(ditProcedure);

  Result := TDropRoutineStmt.Create(Self, ARoutineType, Nodes);
end;

function TMySQLParser.ParseDropServerStmt(): TOffset;
var
  Nodes: TDropServerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    Nodes.ServerTag := ParseTag(kiSERVER);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.ServerIdent := ParseDbIdent(ditServer);

  Result := TDropServerStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDropStmt(): TOffset;
var
  Index: Integer;
begin
  Result := 0;
  Index := 1;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiTEMPORARY)) then
    Inc(Index);

  if (not Error) then
    if (EndOfStmt(NextToken[Index])) then
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

function TMySQLParser.ParseDropTableStmt(): TOffset;
var
  Nodes: TDropTableStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.TableIdentList := ParseList(False, ParseTableIdent);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiRESTRICT) then
      Nodes.RestrictCascadeTag := ParseTag(kiRESTRICT)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCASCADE) then
      Nodes.RestrictCascadeTag := ParseTag(kiCASCADE);

  Result := TDropTableStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDropTriggerStmt(): TOffset;
var
  Nodes: TDropTriggerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    Nodes.TriggerTag := ParseTag(kiTrigger);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.TriggerIdent := ParseDbIdent(ditTrigger);

  Result := TDropTriggerStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDropViewStmt(): TOffset;
var
  Nodes: TDropViewStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    Nodes.ViewTag := ParseTag(kiVIEW);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.ViewIdentList := ParseList(False, ParseViewIdent);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiRESTRICT) then
      Nodes.RestrictCascadeTag := ParseTag(kiRESTRICT)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCASCADE) then
      Nodes.RestrictCascadeTag := ParseTag(kiCASCADE);

  Result := TDropViewStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseEngineIdent(): TOffset;
begin
  Result := ParseDbIdent(ditEngine);
end;

function TMySQLParser.ParseEventIdent(): TOffset;
begin
  Result := ParseDbIdent(ditEvent);
end;

function TMySQLParser.ParseExpr(): TOffset;
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
  Node: TOffset;
  OperatorPrecedence: Integer;
  OperatorType: TOperatorType;
begin
  NodeCount := 0;

  repeat
    Node := CurrentToken;

    if (not IsToken(Node)) then
    begin
      SetError(PE_IncompleteStmt);
      break;
    end
    else if (TokenPtr(CurrentToken)^.TokenType in [ttUnknown, ttSpace, ttReturn]) then
      raise ERangeError.Create(SArgumentOutOfRange)
    else if (TokenPtr(CurrentToken)^.TokenType in [ttComma, ttCloseBracket, ttDelimiter]) then
    begin
      SetError(PE_UnexpectedToken);
      break;
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEND) then
      break
    else if (TokenPtr(CurrentToken)^.OperatorType in [otElse, otThen, otWhen]) then
      break
    else if (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket) then
      if (NodeCount = 0) then
        Node := ParseSubArea(ParseExpr)
      else if (IsRange(NodePtr(Nodes[NodeCount - 1]))) then
        SetError(PE_UnexpectedToken, RangeNodePtr(Nodes[NodeCount - 1])^.FFirstToken)
      else if ((not EndOfStmt(NextToken[1])) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT)) then
        Node := ParseSubArea(ParseSelectStmt)
      else if (TokenPtr(Nodes[NodeCount - 1])^.OperatorType in [otInterval, otBinary, otCollate]) then
        Node := ParseList(True, ParsePartitionIdent)
      else
        Node := ParseList(True, ParseExpr)
    else if (TokenPtr(CurrentToken)^.TokenType = ttAt) then
      Node := ParseVariableIdent()
    else if (((NodeCount = 0) or IsToken(Nodes[NodeCount - 1]) and (TokenPtr(Nodes[NodeCount - 1])^.TokenType = ttOperator) and (TokenPtr(Nodes[NodeCount - 1])^.OperatorType = otDot))
      and (TokenPtr(CurrentToken)^.TokenType = ttOperator)
      and (TokenPtr(CurrentToken)^.OperatorType = otMulti)) then
    begin
      TokenPtr(CurrentToken)^.FTokenType := ttIdent;
      TokenPtr(CurrentToken)^.FDbIdentType := ditAllFields;
      TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
    end
    else if ((TokenPtr(CurrentToken)^.TokenType in ttIdents)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiIN)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttOpenBracket)) then
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otFunction;
      TokenPtr(CurrentToken)^.FUsageType := utOperator;
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCASE) then
      Node := ParseCaseOp()
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiNULL)
      or (TokenPtr(CurrentToken)^.TokenType in [ttInteger, ttNumeric, ttString, ttCSString])) then
      TokenPtr(CurrentToken)^.FUsageType := utConst
    else
    begin
      OperatorType := OperatorTypeByKeywordIndex[TokenPtr(CurrentToken)^.KeywordIndex];
      if (OperatorType <> otUnknown) then
      begin
        TokenPtr(CurrentToken)^.FTokenType := ttOperator;
        TokenPtr(CurrentToken)^.FOperatorType := OperatorType;
        TokenPtr(CurrentToken)^.FUsageType := utOperator;
      end;
    end;

    AddNode(Node, Node = CurrentToken);


    for I := 1 to NodeCount - 1 do
      if (IsToken(Nodes[I - 1]) and (TokenPtr(Nodes[I - 1])^.TokenType = ttOperator)
        and IsToken(Nodes[I]) and (TokenPtr(Nodes[I])^.TokenType = ttOperator)) then
        if (TokenPtr(Nodes[I])^.OperatorType = otMinus) then
          TokenPtr(Nodes[I])^.FOperatorType := otUnaryMinus
        else if (TokenPtr(Nodes[I])^.OperatorType = otPlus) then
          TokenPtr(Nodes[I])^.FOperatorType := otUnaryPlus;

  until (Error
    or (EndOfStmt(CurrentToken))
    or (TokenPtr(CurrentToken)^.TokenType in [ttComma, ttCloseBracket, ttDelimiter])
    or ((not IsToken(Nodes[NodeCount - 1]) or (TokenPtr(Nodes[NodeCount - 1])^.OperatorType = otUnknown))
      and not ((TokenPtr(CurrentToken)^.OperatorType <> otUnknown)
        or (TokenPtr(CurrentToken)^.KeywordIndex = kiBETWEEN)
        or (TokenPtr(CurrentToken)^.KeywordIndex = kiBINARY)
        or (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)
        or (TokenPtr(CurrentToken)^.KeywordIndex = kiCASE)
        or (TokenPtr(CurrentToken)^.KeywordIndex = kiIN)
        or (TokenPtr(CurrentToken)^.KeywordIndex = kiINTERVAL)
        or (TokenPtr(CurrentToken)^.KeywordIndex = kiSOUNDS)
        or (TokenPtr(CurrentToken)^.KeywordIndex = kiTHEN)
        or (TokenPtr(CurrentToken)^.KeywordIndex = kiWHEN))));

  for OperatorPrecedence := 1 to MaxOperatorPrecedence do
  begin
    I := 0;
    while (not Error and (I < NodeCount)) do
    begin
      if (IsToken(Nodes[I]) and (OperatorPrecedenceByOperatorType[TokenPtr(Nodes[I])^.OperatorType] = OperatorPrecedence)) then
        case (TokenPtr(Nodes[I])^.OperatorType) of
          otFunction:
            if (I >= NodeCount - 1) then
              if (EndOfStmt(CurrentToken)) then
                SetError(PE_IncompleteStmt)
              else
                SetError(PE_UnexpectedToken)
            else if (not (NodePtr(Nodes[I + 1])^.FNodeType = ntList)) then
              SetError(PE_UnexpectedToken, StmtNodePtr(Nodes[I + 1])^.FFirstToken)
            else
            begin
              TokenPtr(Nodes[I])^.FDbIdentType := ditFunction;
              Nodes[I] := TFunction.Create(Self, Nodes[I], Nodes[I + 1]);
//              Nodes[I] := TDbIdent.Create(Self, ditFunction, Nodes[I], 0, 0, 0, 0);
              Dec(NodeCount);
              Move(Nodes[I + 2], Nodes[I + 1], (NodeCount - I - 1) * SizeOf(Nodes[0]));
            end;
          otInterval,
          otBinary,
          otCollate,
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
            else if (IsToken(Nodes[I - 1]) and (TokenPtr(Nodes[I - 1])^.TokenType = ttOperator)) then
              SetError(PE_UnexpectedToken, Nodes[I])
            else if (IsToken(Nodes[I + 1]) and (TokenPtr(Nodes[I + 1])^.TokenType = ttOperator)) then
              SetError(PE_UnexpectedToken, Nodes[I + 1])
            else
            begin
              Nodes[I - 1] := TBinaryOp.Create(Self, Nodes[I], Nodes[I - 1], Nodes[I + 1]);
              Dec(NodeCount, 2);
              Move(Nodes[I + 2], Nodes[I], (NodeCount - I) * SizeOf(Nodes[0]));
              Dec(I);
            end;
          otDot:
            if (I = 0) then
              SetError(PE_UnexpectedToken, Nodes[I])
            else if (I >= NodeCount - 1) then
              SetError(PE_IncompleteStmt)
            else if ((I + 1 <= NodeCount) and (NodePtr(Nodes[I + 1])^.NodeType = ntFunction)) then
            begin
              TokenPtr(Nodes[I - 1])^.FDbIdentType := ditDatabase;
              Nodes[I - 1] := TDbIdent.Create(Self, ditFunction, Nodes[I + 1], Nodes[I], Nodes[I - 1], 0, 0);
              Dec(NodeCount, 2);
              Move(Nodes[I + 2], Nodes[I], (NodeCount - I) * SizeOf(Nodes[0]));
              Dec(I);
            end
            else if ((I + 2 = NodeCount) or (TokenPtr(Nodes[I + 2])^.OperatorType <> otDot)) then
            begin
              TokenPtr(Nodes[I - 1])^.FDbIdentType := ditTable;
              TokenPtr(Nodes[I + 1])^.FDbIdentType := ditColumn;
              Nodes[I - 1] := TDbIdent.Create(Self, ditColumn, Nodes[I + 1], 0, 0, Nodes[I], Nodes[I - 1]);
              Dec(NodeCount, 2);
              Move(Nodes[I + 2], Nodes[I], (NodeCount - I) * SizeOf(Nodes[0]));
              Dec(I);
            end
            else
            begin
              TokenPtr(Nodes[I - 1])^.FDbIdentType := ditDatabase;
              TokenPtr(Nodes[I + 1])^.FDbIdentType := ditTable;
              TokenPtr(Nodes[I + 3])^.FDbIdentType := ditColumn;
              Nodes[I - 1] := TDbIdent.Create(Self, ditColumn, Nodes[I + 3], Nodes[I], Nodes[I - 1], Nodes[I + 2], Nodes[I + 1]);
              Dec(NodeCount, 4);
              Move(Nodes[I + 4], Nodes[I], (NodeCount - I) * SizeOf(Nodes[0]));
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
  begin
    if (IsToken(Nodes[0]) and (TokenPtr(Nodes[0])^.TokenType in ttIdents)) then
      TokenPtr(Nodes[0])^.FDbIdentType := ditColumn;
    Result := Nodes[0];
  end;
end;

function TMySQLParser.ParseExprList(): TOffset;
begin
  Result := ParseList(True, ParseExpr);
end;

function TMySQLParser.ParseFetchStmt(): TOffset;
var
  Nodes: TFetchStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.FetchTag := ParseTag(kiFETCH);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiNEXT)) then
    Nodes.NextTag := ParseTag(kiNEXT);

  if (not Error and ((Nodes.NextTag > 0) or not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiNEXT))) then
    Nodes.FromTag := ParseTag(kiFROM);

  if (not Error) then
    Nodes.CursorIdent := ParseDbIdent(ditCursor);

  if (not Error) then
    Nodes.IntoValue := ParseValue(kiINTO, vaNo, False, ParseVariableIdent);

  Result := TFetchStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseForeignKey(const Add: Boolean = False): TOffset;
var
  Nodes: TForeignKey.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (Add) then
    Nodes.AddTag := ParseTag(kiADD);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCONSTRAINT)) then
  begin
    Nodes.ConstraintTag := ParseTag(kiCONSTRAINT);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiPRIMARY) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiUNIQUE)) then
      Nodes.SymbolIdent := ApplyCurrentToken();
  end;

  if (not Error) then
    Nodes.KeyTag := ParseTag(kiFOREIGN, kiKEY);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket)) then
    Nodes.NameToken := ParseString();

  if (not Error) then
    Nodes.ColumnNameList := ParseList(True, ParseIndexColumn);

  Result := TForeignKey.Create(Self, Nodes);
end;

function TMySQLParser.ParseFunction(): TOffset;
var
  Nodes: TFunction.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if ((FFunctions.Count = 0) or (FFunctions.IndexOf(TokenPtr(CurrentToken)^.FText.SQL, TokenPtr(CurrentToken)^.FText.Length) >= 0)) then
    Nodes.Ident := ApplyCurrentToken(utFunction)
  else
    Nodes.Ident := ParseDbIdent(ditFunction);

  if (not Error) then
    Nodes.Arguments := ParseList(True, ParseExpr);

  Result := TFunction.Create(Self, Nodes.Ident, Nodes.Arguments);
end;

function TMySQLParser.ParseFunctionParam(): TOffset;
var
  Nodes: TRoutineParam.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
    SetError(PE_UnexpectedToken)
  else
    Nodes.IdentToken := ApplyCurrentToken();

  if (not Error) then
    Nodes.DataTypeNode := ParseDataType();

  Result := TRoutineParam.Create(Self, Nodes);
end;

function TMySQLParser.ParseIfStmt(): TOffset;
var
  Branches: array of TOffset;
  ListNodes: TList.TNodes;
  Nodes: TIfStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  SetLength(Branches, 1);

  Branches[0] := ParseIfStmtBranch();

  while (not Error) do
    if EndOfStmt(CurrentToken) then
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
      SetError(PE_UnexpectedToken);

  FillChar(ListNodes, SizeOf(ListNodes), 0);
  Nodes.BranchList := TList.Create(Self, ListNodes, Length(Branches), Branches);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiIF);

  Result := TIfStmt.Create(Self, Nodes);

  SetLength(Branches, 0);
end;

function TMySQLParser.ParseIfStmtBranch(): TOffset;
var
  Nodes: TIfStmt.TBranch.TNodes;
  SearchCondition: Boolean;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  SearchCondition := False;
  if (EndOfStmt(CurrentToken)) then
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
    SetError(PE_UnexpectedToken);

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

function TMySQLParser.ParseIndex(const Add: Boolean = False): TOffset;
var
  Found: Boolean;
  Nodes: TIndex.TNodes;
  PrimaryKey: Boolean;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (Add) then
    Nodes.AddTag := ParseTag(kiADD);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCONSTRAINT)) then
  begin
    Nodes.ConstraintTag := ParseTag(kiCONSTRAINT);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiPRIMARY) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiUNIQUE)) then
      Nodes.SymbolIdent := ApplyCurrentToken();
  end;

  PrimaryKey := False;
  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiPRIMARY) then
    begin
      Nodes.IndexTag := ParseTag(kiPRIMARY, kiKEY);
      PrimaryKey := True;
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUNIQUE) then
      if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiINDEX)) then
        Nodes.IndexTag := ParseTag(kiUNIQUE, kiINDEX)
      else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
        Nodes.IndexTag := ParseTag(kiUNIQUE, kiKEY)
      else
        Nodes.IndexTag := ParseTag(kiUNIQUE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFULLTEXT) then
      if (Nodes.ConstraintTag > 0) then
        SetError(PE_UnexpectedToken)
      else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiINDEX)) then
        Nodes.IndexTag := ParseTag(kiFULLTEXT, kiINDEX)
      else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
        Nodes.IndexTag := ParseTag(kiFULLTEXT, kiKEY)
      else
        Nodes.IndexTag := ParseTag(kiFULLTEXT)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSPATIAL) then
      if (Nodes.ConstraintTag > 0) then
        SetError(PE_UnexpectedToken)
      else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiINDEX)) then
        Nodes.IndexTag := ParseTag(kiSPATIAL, kiINDEX)
      else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
        Nodes.IndexTag := ParseTag(kiSPATIAL, kiKEY)
      else
        Nodes.IndexTag := ParseTag(kiSPATIAL);

  if (not PrimaryKey and not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiUSING) and (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket)) then
    Nodes.IndexIdent := ParseString();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING)) then
    Nodes.IndexTypeValue := ParseValue(kiUSING, vaNo, WordIndices(kiBTREE, kiHASH));

  if (not Error) then
    Nodes.ColumnNameList := ParseList(True, ParseIndexColumn);

  Found := True;
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
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

function TMySQLParser.ParseIndexColumn(): TOffset;
var
  Nodes: TIndexColumn.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error) then
    Nodes.IdentTag := ParseDbIdent(ditColumn);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
  begin
    Nodes.OpenBracketToken := ApplyCurrentToken();

    Nodes.LengthToken := ParseInteger();
    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.CloseBracketToken := ApplyCurrentToken();
  end;

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiASC) then
      Nodes.SortTag := ParseTag(kiASC)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDESC) then
      Nodes.SortTag := ParseTag(kiDESC);

  Result := TIndexColumn.Create(Self, Nodes);
end;

function TMySQLParser.ParseIndexHint(): TOffset;
var
  Nodes: TSelectStmt.TTableFactor.TIndexHint.TNodes;
  ValueNodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiUSE) or (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE) or (TokenPtr(CurrentToken)^.KeywordIndex = kiFORCE)) then
    Nodes.KindTag := TokenPtr(CurrentToken)^.KeywordIndex
  else
    SetError(PE_UnexpectedToken);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX) or (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY)) then
      Nodes.IndexTag := TokenPtr(CurrentToken)^.KeywordIndex
    else
      SetError(PE_UnexpectedToken);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFOR)) then
  begin
    FillChar(ValueNodes, SizeOf(ValueNodes), 0);

    ValueNodes.IdentTag := ParseTag(kiFOR);

    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiJOIN) then
      ValueNodes.ValueNode := ParseTag(kiJOIN)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiORDER) then
      ValueNodes.ValueNode := ParseTag(kiORDER, kiBY)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiGROUP) then
      ValueNodes.ValueNode := ParseTag(kiGROUP, kiBY)
    else
      SetError(PE_UnexpectedToken);

    Nodes.ForValue := TValue.Create(Self, ValueNodes);
  end;

  Result := TSelectStmt.TTableFactor.TIndexHint.Create(Self, Nodes);
end;

function TMySQLParser.ParseInsertStmt(const Replace: Boolean = False): TOffset;
var
  Nodes: TInsertStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.InsertTag := ParseTag(kiINSERT);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOW_PRIORITY) then
      Nodes.PriorityTag := ParseTag(kiLOW_PRIORITY)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDELAYED) then
      Nodes.PriorityTag := ParseTag(kiDELAYED)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiHIGH_PRIORITY) then
      Nodes.PriorityTag := ParseTag(kiHIGH_PRIORITY);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE)) then
    Nodes.IgnoreTag := ParseTag(kiIGNORE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINTO)) then
    Nodes.IntoTag := ParseTag(kiINTO);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
  begin
    Nodes.IntoTag := ParseTag(kiPARTITION);

    if (not Error) then
      Nodes.PartitionList := ParseList(True, ParsePartitionIdent);
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.ColumnList := ParseList(True, ParseColumnIdent);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
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
      SetError(PE_UnexpectedToken);

  if (not Error) then
    if (Nodes.ValuesTag > 0) then
      Nodes.ValuesList := ParseList(False, ParseExprList)
    else if (Nodes.SetTag > 0) then
      Nodes.ValuesList := ParseList(False, ParseUpdatePair)
    else if (Nodes.SelectTag > 0) then
      Nodes.SelectStmt := ParseSelectStmt()
    else
      SetError(PE_Unknown);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON)) then
  begin
    Nodes.OnDuplicateKeyUpdateTag := ParseTag(kiON, kiDUPLICATE, kiKEY, kiUPDATE);

    Nodes.UpdateList := ParseList(False, ParseUpdatePair);
  end;

  Result := TInsertStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseInteger(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttInteger) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken();
end;

function TMySQLParser.ParseInterval(): TOffset;
var
  Nodes: TSchedule.TInterval.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error) then
    Nodes.QuantityExp := ParseExpr();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
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
      SetError(PE_UnexpectedToken)
    else
      Nodes.UnitTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  Result := TSchedule.TInterval.Create(Self, Nodes);
end;

function TMySQLParser.ParseIntervalList(): TSchedule.TIntervalList;
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
  while (not Error and Found and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.OperatorType = otPlus)) do
    if (EndOfStmt(NextToken[1])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[1])^.KeywordIndex <> kiINTERVAL) then
      SetError(PE_UnexpectedToken, NextToken[1])
    else if (EndOfStmt(NextToken[2])) then
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

function TMySQLParser.ParseIntervalListItem(const KeywordIndex: TWordList.TIndex): TOffset;
var
  Nodes: TSchedule.TIntervalListItem.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.OperatorType <> otPlus) then
    SetError(PE_UnexpectedToken)
  else
    Nodes.PlusToken := CurrentToken;

  if (not Error) then
    Nodes.IntervalTag := ParseTag(kiINTERVAL);

  if (not Error) then
    Nodes.Interval := ParseInterval();

  Result := TSchedule.TIntervalListItem.Create(Self, Nodes);
end;

function TMySQLParser.ParseIterateStmt(): TOffset;
var
  Nodes: TIterateStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IterateToken := ParseTag(kiITERATE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.LabelToken := ApplyCurrentToken(utLabel);

  Result := TIterateStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseKeyword(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex < 0) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken();
end;

function TMySQLParser.ParseLeaveStmt(): TOffset;
var
  Nodes: TLeaveStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.LeaveTag := ParseTag(kiLEAVE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.LabelToken := ApplyCurrentToken(utLabel);

  Result := TLeaveStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseList(const Brackets: Boolean; const ParseItem: TParseFunction = nil): TOffset;
begin
  Result := ParseList(Brackets, ParseItem, ttComma);
end;

function TMySQLParser.ParseList(const Brackets: Boolean; const ParseItem: TParseFunction; const DelimterType: fspTypes.TTokenType): TOffset;
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
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  Index := 0;
  if (not Error and (not Brackets or not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket))) then
  begin
    repeat
      if (Index < Length(Childrens)) then
        Childrens[Index] := ParseItem()
      else
      begin
        if (Index = Length(Childrens)) then
        begin
          ChildrenList := Classes.TList.Create();
          for I := 0 to Length(Childrens) - 1 do
            ChildrenList.Add(Pointer(Childrens[I]));
        end;
        if (not Assigned(ParseItem)) then
          ChildrenList.Add(Pointer(ApplyCurrentToken()))
        else
          ChildrenList.Add(Pointer(ParseItem()));
      end;

      Inc(Index);

      DelimiterFound := not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = DelimterType);
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
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
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

function TMySQLParser.ParseLoadDataStmt(): TOffset;
var
  Nodes: TLoadDataStmt.TNodes;
  IgnoreLinesNodes: TIgnoreLines.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.LoadDataTag := ParseTag(kiLOAD, kiDATA);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOW_PRIORITY) then
      Nodes.PriorityTag := ParseTag(kiLOW_PRIORITY)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCONCURRENT) then
      Nodes.PriorityTag := ParseTag(kiCONCURRENT);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCAL)) then
    Nodes.LocalTag := ParseTag(kiLOCAL);

  if (not Error) then
    Nodes.InfileTag := ParseTag(kiINFILE);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiREPLACE) then
      Nodes.ReplaceIgnoreTag := ParseTag(kiREPLACE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE) then
      Nodes.ReplaceIgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    Nodes.IntoTableValue := ParseValue(WordIndices(kiINTO, kiTABLE), vaNo, ParseTableIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
    Nodes.PartitionValue := ParseValue(kiPARTITION, vaNo, True, ParsePartitionIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
    Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseCharacterSetIdent);

  if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiFIELDS) or (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLUMNS)))then
  begin
    Nodes.ColumnsTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTERMINATED)) then
      Nodes.ColumnsTerminatedByValue := ParseValue(WordIndices(kiTERMINATED, kiBY), vaNo, ParseString);

    if (not Error and not EndOfStmt(CurrentToken)) then
      if (TokenPtr(CurrentToken)^.KeywordIndex = kiOPTIONAL) then
        Nodes.EnclosedByValue := ParseValue(WordIndices(kiOPTIONAL, kiENCLOSED, kiBY), vaNo, ParseString)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiENCLOSED) then
        Nodes.EnclosedByValue := ParseValue(WordIndices(kiENCLOSED, kiBY), vaNo, ParseString);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiESCAPED)) then
      Nodes.EscapedByValue := ParseValue(WordIndices(kiESCAPED, kiBY), vaNo, ParseString);
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLINES)) then
  begin
    Nodes.LinesTag := ParseTag(kiLINES);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTARTING)) then
      Nodes.StartingByValue := ParseValue(WordIndices(kiSTARTING, kiBY), vaNo, ParseString);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTERMINATED)) then
      Nodes.LinesTerminatedByValue := ParseValue(WordIndices(kiTERMINATED, kiBY), vaNo, ParseString);
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE)) then
  begin
    FillChar(IgnoreLinesNodes, SizeOf(IgnoreLinesNodes), 0);

    IgnoreLinesNodes.IgnoreTag := ParseTag(kiIGNORE);

    if (not Error) then
      IgnoreLinesNodes.NumberToken := ParseInteger;

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLINES) then
        IgnoreLinesNodes.LinesTag := ParseTag(kiLINES)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiROWS) then
        IgnoreLinesNodes.LinesTag := ParseTag(kiROWS)
      else
        SetError(PE_UnexpectedToken);

    Nodes.IgnoreLines := TIgnoreLines.Create(Self, IgnoreLinesNodes);
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.ColumnList := ParseList(True, ParseColumnIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSET)) then
    Nodes.SetList := ParseValue(kiSET, vaNo, False, ParseUpdatePair);

  Result := TLoadDataStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseLoadStmt(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(NextToken[1])) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(NextToken[1])^.KeywordIndex = kiDATA) then
    Result := ParseLoadDataStmt()
  else if (TokenPtr(NextToken[1])^.KeywordIndex = kiXML) then
    Result := ParseLoadXMLStmt()
  else
    SetError(PE_UnexpectedToken);
end;

function TMySQLParser.ParseLoadXMLStmt(): TOffset;
var
  Nodes: TLoadXMLStmt.TNodes;
  IgnoreLinesNodes: TIgnoreLines.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.LoadXMLTag := ParseTag(kiLOAD, kiXML);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOW_PRIORITY) then
      Nodes.PriorityTag := ParseTag(kiLOW_PRIORITY)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCONCURRENT) then
      Nodes.PriorityTag := ParseTag(kiCONCURRENT);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCAL)) then
    Nodes.LocalTag := ParseTag(kiLOCAL);

  if (not Error) then
    Nodes.InfileTag := ParseTag(kiINFILE);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiREPLACE) then
      Nodes.ReplaceIgnoreTag := ParseTag(kiREPLACE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE) then
      Nodes.ReplaceIgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    Nodes.IntoTableValue := ParseValue(WordIndices(kiINTO, kiTABLE), vaNo, ParseTableIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
    Nodes.PartitionValue := ParseValue(kiPARTITION, vaNo, True, ParsePartitionIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
    Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseCharacterSetIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiROWS)) then
    Nodes.RowsIdentifiedByValue := ParseValue(WordIndices(kiROWS, kiIDENTIFIED, kiBY), vaNo, ParseString);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE)) then
  begin
    FillChar(IgnoreLinesNodes, SizeOf(IgnoreLinesNodes), 0);

    IgnoreLinesNodes.IgnoreTag := ParseTag(kiIGNORE);

    if (not Error) then
      IgnoreLinesNodes.NumberToken := ParseInteger;

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLINES) then
        IgnoreLinesNodes.LinesTag := ParseTag(kiLINES)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiROWS) then
        IgnoreLinesNodes.LinesTag := ParseTag(kiROWS)
      else
        SetError(PE_UnexpectedToken);

    Nodes.IgnoreLines := TIgnoreLines.Create(Self, IgnoreLinesNodes);
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.ColumnList := ParseList(True, ParseColumnIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSET)) then
    Nodes.SetList := ParseValue(kiSET, vaNo, False, ParseUpdatePair);

  Result := TLoadXMLStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseLockStmt(): TOffset;
var
  Nodes: TLockStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.LockTablesTag := ParseTag(kiLOCK, kiTABLES);

  if (not Error) then
    Nodes.ItemList := ParseList(False, ParseLockStmtItem);

  Result := TLockStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseLockStmtItem(): TOffset;
var
  Nodes: TLockStmt.TItem.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.TableIdent := ParseTableIdent();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAS)) then
    Nodes.AsTag := ParseTag(kiAS);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType in ttIdents) and (TokenPtr(CurrentToken)^.KeywordIndex < 0)) then
    Nodes.AliasIdent := ParseDbIdent(ditAlias);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREAD) then
      if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiLOCAL)) then
        Nodes.TypeTag := ParseTag(kiREAD, kiLOCAL)
      else
        Nodes.TypeTag := ParseTag(kiREAD)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOW_PRIORITY) then
      Nodes.TypeTag := ParseTag(kiLOW_PRIORITY, kiWRITE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWRITE) then
      Nodes.TypeTag := ParseTag(kiWRITE)
    else
      SetError(PE_UnexpectedToken);

  Result := TLockStmt.TItem.Create(Self, Nodes);
end;

function TMySQLParser.ParseLoopStmt(): TOffset;
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

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)) then
    if ((Nodes.BeginLabelToken = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(Nodes.BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EndLabelToken := ApplyCurrentToken(utLabel, ttEndLabel);

  Result := TLoopStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseOpenStmt(): TOffset;
var
  Nodes: TOpenStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.OpenTag := ParseTag(kiOPEN);

  if (not Error) then
    Nodes.CursorIdent := ParseDbIdent(ditCursor);

  Result := TOpenStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParsePartition(): TOffset;
begin
  Result := ParsePartition(False);
end;

function TMySQLParser.ParsePartition(const Add: Boolean): TOffset;
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

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiVALUES)) then
    Nodes.ValuesNode := ParsePartitionValues();

  Found := True;
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
    if ((Nodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString)
    else if ((Nodes.DataDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDATA)) then
      Nodes.DataDirectoryValue := ParseValue(WordIndices(kiDATA, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENGINE)) then
      Nodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseEngineIdent)
    else if ((Nodes.IndexDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX)) then
      Nodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.MaxRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_ROWS)) then
      Nodes.MaxRowsValue := ParseValue(kiMAX_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.MinRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMIN_ROWS)) then
      Nodes.MinRowsValue := ParseValue(kiMIN_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTORAGE)) then
      Nodes.EngineValue := ParseValue(WordIndices(kiSTORAGE, kiENGINE), vaAuto, ParseEngineIdent)
    else if ((Nodes.SubPartitionList = 0) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
      Nodes.SubPartitionList := ParseList(True, ParseSubPartition)
    else
      Found := False;

  Result := TPartition.Create(Self, Nodes);
end;

function TMySQLParser.ParsePartitionIdent(): TOffset;
begin
  Result := ParseDbIdent(ditPartition);
end;

function TMySQLParser.ParsePartitionNames(): TOffset;
begin
  if (EndOfStmt(CurrentToken)) then
  begin
    SetError(PE_IncompleteStmt);
    Result := 0;
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiALL) then
    Result := ParseTag(kiALL)
  else
    Result := ParseList(False, ParsePartitionIdent);
end;

function TMySQLParser.ParsePartitionValues(): TOffset;
var
  Nodes: TPartitionValues.TNodes;
  ValueNodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ValuesTag := ParseTag(kiVALUES);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLESS) then
    if (EndOfStmt(NextToken[1])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[1])^.KeywordIndex <> kiTHAN) then
      SetError(PE_UnexpectedToken, NextToken[1])
    else if (EndOfStmt(NextToken[2])) then
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
    SetError(PE_UnexpectedToken);

  Result := TPartitionValues.Create(Self, Nodes);
end;

function TMySQLParser.ParsePL_SQLStmt(): TOffset;
begin
  Result := ParseStmt(True);
end;

function TMySQLParser.ParseProcedureParam(): TOffset;
var
  Nodes: TRoutineParam.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIN) then
    Nodes.DirektionTag := ParseTag(kiIN)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiOUT) then
    Nodes.DirektionTag := ParseTag(kiIN)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiINOUT) then
    Nodes.DirektionTag := ParseTag(kiIN);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.IdentToken := ApplyCurrentToken();

  if (not Error) then
    Nodes.DataTypeNode := ParseDataType();

  Result := TRoutineParam.Create(Self, Nodes);
end;

function TMySQLParser.ParseReleaseStmt(): TOffset;
var
  Nodes: TReleaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ReleaseTag := ParseTag(kiRELEASE, kiSAVEPOINT);

  if (not Error) then
    Nodes.Ident := ParseSavepointIdent();

  Result := TReleaseStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseRenameStmt(): TOffset;
var
  Nodes: TRenameStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.RenameTag := ParseTag(kiRENAME, kiTABLE);

  if (not Error) then
    Nodes.RenameList := ParseList(False, ParseRenameTableStmtPair);

  Result := TRenameStmt.Create(Self, Nodes)
end;

function TMySQLParser.ParseRenameTableStmtPair(): TOffset;
var
  Nodes: TRenameStmt.TPair.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.OrgTableIdent := ParseTableIdent();

  if (not Error) then
    Nodes.ToTag := ParseTag(kiTO);

  if (not Error) then
    Nodes.NewTableIdent := ParseTableIdent();

  Result := TRenameStmt.TPair.Create(Self, Nodes);
end;

function TMySQLParser.ParseRepeatStmt(): TOffset;
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

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)) then
    if ((Nodes.BeginLabelToken = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(Nodes.BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EndLabelToken := ApplyCurrentToken(utLabel, ttEndLabel);

  Result := TRepeatStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseRollbackStmt(): TOffset;
var
  Nodes: TRollbackStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiWORK)) then
    Nodes.RollbackTag := ParseTag(kiROLLBACK, kiWORK)
  else
    Nodes.RollbackTag := ParseTag(kiROLLBACK);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTO)) then
    if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSAVEPOINT)) then
      ParseValue(WordIndices(kiTO, kiSAVEPOINT), vaNo, ParseSavepointIdent)
    else
      ParseValue(kiTO, vaNo, ParseSavepointIdent)
  else
  begin
    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAND)) then
      if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiNO)) then
        Nodes.RollbackTag := ParseTag(kiAND, kiNO, kiCHAIN)
      else
        Nodes.RollbackTag := ParseTag(kiAND, kiCHAIN);

    if (not Error and not EndOfStmt(CurrentToken)) then
      if (TokenPtr(NextToken[1])^.KeywordIndex = kiNO) then
        Nodes.RollbackTag := ParseTag(kiNO, kiRELEASE)
      else if (TokenPtr(NextToken[1])^.KeywordIndex = kiRELEASE) then
        Nodes.RollbackTag := ParseTag(kiRELEASE);
  end;

  Result := TRollbackStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseSavepointIdent(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken();
end;

function TMySQLParser.ParseSavepointStmt(): TOffset;
var
  Nodes: TSavepointStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.SavepointTag := ParseTag(kiSAVEPOINT);

  if (not Error) then
    Nodes.Ident := ParseSavepointIdent();

  Result := TSavepointStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseSchedule(): TOffset;
var
  Nodes: TSchedule.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiAT) then
  begin
    Nodes.AtValue := ParseValue(kiAT, vaNo, ParseString);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.OperatorType = otPlus)) then
      Nodes.AtIntervalList := ParseIntervalList();
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEVERY) then
  begin
    Nodes.EveryValue := ParseValue(kiEVERY, vaNo, ParseInterval);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTARTS)) then
    begin
      Nodes.StartsValue := ParseValue(kiAT, vaNo, ParseString);

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.OperatorType = otPlus)) then
        Nodes.AtIntervalList := ParseIntervalList();
    end;

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENDS)) then
    begin
      Nodes.EndsValue := ParseValue(kiAT, vaNo, ParseString);

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.OperatorType = otPlus)) then
        Nodes.AtIntervalList := ParseIntervalList();
    end;
  end;

  Result := TSchedule.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowAuthorsStmt(): TOffset;
var
  Nodes: TShowAuthorsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiAUTHORS);

  Result := TShowAuthorsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowBinaryLogsStmt(): TOffset;
var
  Nodes: TShowBinaryLogsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiBINARY, kiLOGS);

  Result := TShowBinaryLogsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowBinlogEventsStmt(): TOffset;
var
  Nodes: TShowBinlogEventsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiBINLOG, kiEVENTS);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIN)) then
    Nodes.InValue := ParseValue(kiIN, vaNo, ParseString);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM)) then
    Nodes.FromValue := ParseValue(kiFROM, vaNo, ParseInteger);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLIMIT)) then
  begin
    Nodes.LimitTag := ParseTag(kiLIMIT);

    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteToken)
    else
    begin
      Nodes.RowCountToken := ParseInteger();

      if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma)) then
      begin
        Nodes.CommaToken := ApplyCurrentToken();

        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteToken)
        else
        begin
          Nodes.OffsetToken := Nodes.RowCountToken;
          Nodes.RowCountToken := ParseInteger();
        end;
      end;
    end;
  end;

  Result := TShowBinlogEventsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowCharacterSetStmt(): TOffset;
var
  Nodes: TShowCharacterSetStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCHARACTER, kiSET);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE) then
      Nodes.LikeValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowCharacterSetStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowCollationStmt(): TOffset;
var
  Nodes: TShowCollationStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCOLLATION);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE) then
      Nodes.LikeValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowCollationStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowContributorsStmt(): TOffset;
var
  Nodes: TShowContributorsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCONTRIBUTORS);

  Result := TShowContributorsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseSelectStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TSelectStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.SelectTag := ParseTag(kiSELECT);

  Found := True;
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
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
    Nodes.ColumnsList := ParseList(False, ParseSelectStmtField);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINTO) and (Nodes.IntoValue = 0)) then
    Nodes.IntoValue := ParseValue(kiINTO, vaNo, False, ParseExpr);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM)) then
  begin
    Nodes.FromValue := ParseValue(kiFROM, vaNo, False, ParseTableReference);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiGROUP)) then
      Nodes.GroupByNode := ParseSelectStmtGroups();

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiHAVING)) then
      Nodes.HavingValue := ParseValue(kiHAVING, vaNo, ParseExpr);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiORDER)) then
      Nodes.OrderByValue := ParseValue(WordIndices(kiORDER, kiBY), vaNo, False, ParseSelectStmtOrder);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLIMIT)) then
    begin
      Nodes.Limit.LimitTag := ParseTag(kiLIMIT);

      if EndOfStmt(CurrentToken) then
        SetError(PE_IncompleteToken)
      else
      begin
        Nodes.Limit.RowCountToken := ParseInteger();

        if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma)) then
        begin
          Nodes.Limit.CommaToken := ApplyCurrentToken();

          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteToken)
          else
          begin
            Nodes.Limit.OffsetToken := Nodes.Limit.RowCountToken;
            Nodes.Limit.RowCountToken := ParseInteger();
          end;
        end
        else if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiOFFSET)) then
        begin
          Nodes.Limit.OffsetTag := ParseTag(kiOFFSET);

          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteToken)
          else
            Nodes.Limit.OffsetToken := ParseInteger();
        end;
      end;
    end;

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINTO) and (Nodes.IntoValue = 0)) then
      Nodes.IntoValue := ParseValue(kiINTO, vaNo, False, ParseExpr);
  end;

  Result := TSelectStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseSelectStmtField(): TOffset;
var
  Nodes: TSelectStmt.TField.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.Value := ParseExpr();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAs)) then
    Nodes.AsTag := ParseTag(kiAS);

  if (not Error) then
    if ((Nodes.AsTag > 0) and EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if ((Nodes.AsTag > 0) and not EndOfStmt(CurrentToken) and (not (TokenPtr(CurrentToken)^.TokenType in ttIdents) or (TokenPtr(CurrentToken)^.KeywordIndex >= 0))) then
      SetError(PE_UnexpectedToken)
    else if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType in ttIdents) and (TokenPtr(CurrentToken)^.KeywordIndex < 0)) then
      Nodes.AliasIdent := ParseDbIdent(ditAlias);

  Result := TSelectStmt.TField.Create(Self, Nodes);
end;

function TMySQLParser.ParseSelectStmtGroup(): TOffset;
var
  Nodes: TSelectStmt.TGroup.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.Expr := ParseExpr();

  if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiASC) or (TokenPtr(CurrentToken)^.KeywordIndex = kiDESC))) then
    Nodes.Direction := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  Result := TSelectStmt.TGroup.Create(Self, Nodes);
end;

function TMySQLParser.ParseSelectStmtGroups(): TOffset;
var
  Nodes: TSelectStmt.TGroups.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.GroupByTag := ParseTag(kiGROUP, kiBY);

  if (not Error) then
    Nodes.ColumnList := ParseList(False, ParseSelectStmtGroup);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
    Nodes.WithRollupTag := ParseTag(kiWITH, kiROLLUP);

  Result := TSelectStmt.TGroups.Create(Self, Nodes);
end;

function TMySQLParser.ParseSelectStmtOrder(): TOffset;
var
  Nodes: TSelectStmt.TOrder.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.Expr := ParseExpr();

  if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiASC) or (TokenPtr(CurrentToken)^.KeywordIndex = kiDESC))) then
    Nodes.Direction := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  Result := TSelectStmt.TOrder.Create(Self, Nodes);
end;

function TMySQLParser.ParseServerOptionList(): TOffset;
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

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
    SetError(PE_UnexpectedToken)
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
      if (EndOfStmt(CurrentToken)) then
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
        SetError(PE_UnexpectedToken);

      Inc(Index);

      DelimiterFound := not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma);
      if (DelimiterFound) then
      begin
        Childrens[Index] := ApplyCurrentToken(); // Delimiter
        Inc(Index);
      end;
    until (Error or not DelimiterFound);
  end;

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TList.Create(Self, Nodes, Index, Childrens);
end;

function TMySQLParser.ParseSetStmt(): TOffset;
var
  Index: Integer;
  Nodes: TSetStmt.TNodes;
  PasswordNodes: TSetPasswordStmt.TNodes;
  TranactionNodes: TSetTransactionStmt.TNodes;
begin
  FillChar(TranactionNodes, SizeOf(TranactionNodes), 0);

  Index := 1;
  if (not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiPASSWORD)) then
  begin
    PasswordNodes.SetTag := ParseTag(kiSET);

    if (not Error) then
      PasswordNodes.PasswordTag := ParseTag(kiPASSWORD);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFOR)) then
      PasswordNodes.ForValue := ParseValue(kiFOR, vaNo, ParseUser);

    if (not Error) then
      PasswordNodes.PasswordExpr := ParseExpr();

    Result := TSetPasswordStmt.Create(Self, PasswordNodes);
  end
  else
  begin
    if (not EndOfStmt(NextToken[Index]) and ((TokenPtr(NextToken[Index])^.KeywordIndex = kiGLOBAL) or (TokenPtr(NextToken[Index])^.KeywordIndex = kiSESSION))) then
      Inc(Index);

    if (not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiTRANSACTION)) then
    begin
      TranactionNodes.SetTag := ParseTag(kiSET);

      if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiGLOBAL) or (TokenPtr(CurrentToken)^.KeywordIndex = kiSESSION))) then
        TranactionNodes.ScopeTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

      if (not Error) then
        TranactionNodes.TransactionTag := ParseTag(kiTRANSACTION);

      if (not Error) then
        TranactionNodes.CharacteristicList := ParseList(False, ParseTransactionCharacteristic);

      Result := TSetTransactionStmt.Create(Self, TranactionNodes);
    end
    else
    begin
      Nodes.SetTag := ParseTag(kiSET);

      if (not Error) then
        Nodes.AssignmentList := ParseList(False, ParseSetStmtAssignment);

      Result := TSetStmt.Create(Self, Nodes);
    end;
  end;
end;

function TMySQLParser.ParseSetStmtAssignment(): TOffset;
var
  Nodes: TSetStmt.TAssignment.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiGLOBAL) or (TokenPtr(CurrentToken)^.KeywordIndex = kiSESSION))) then
    Nodes.ScopeTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  if (not Error) then
    Nodes.Variable := ParseVariableIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign]) then
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end
    else if (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign2]) then
      Nodes.AssignToken := ApplyCurrentToken()
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.ValueExpr := ParseExpr;

  Result := TSetStmt.TAssignment.Create(Self, Nodes);
end;

function TMySQLParser.ParseStartTransactionStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TStartTransactionStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StartTransactionTag := ParseTag(kiSTART, kiTRANSACTION);

  Found := True;
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiREAD) then
      if (EndOfStmt(NextToken[1])) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiONLY) then
        Nodes.RealOnlyTag := ParseTag(kiREAD, kiONLY)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWRITE) then
        Nodes.RealOnlyTag := ParseTag(kiREAD, kiWRITE)
      else
        SetError(PE_UnexpectedToken)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH) then
      Nodes.WithConsistentSnapshotTag := ParseTag(kiWITH, kiCONSISTENT, kiSNAPSHOT)
    else
      Found := False;

  Result := TStartTransactionStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseString(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in [ttString, ttCSString]) and not ((TokenPtr(CurrentToken)^.TokenType = ttIdent) and (TokenPtr(CurrentToken)^.KeywordIndex < 0))) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken();
end;

function TMySQLParser.ParseSubArea(const ParseNode: TParseFunction): TOffset;
var
  Nodes: TSubArea.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
    SetError(PE_UnexpectedToken)
  else
    Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    Nodes.AreaNode := ParseNode();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TSubArea.Create(Self, Nodes);
end;

function TMySQLParser.ParseSubPartition(): TOffset;
var
  Found: Boolean;
  Nodes: TSubPartition.TNodes;
begin
  if (not Error) then
    Nodes.SubPartitionTag := ParseTag(kiPARTITION);

  if (not Error) then
    Nodes.NameIdent := ParsePartitionIdent();

  Found := True;
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
    if ((Nodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString)
    else if ((Nodes.DataDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDATA)) then
      Nodes.DataDirectoryValue := ParseValue(WordIndices(kiDATA, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENGINE)) then
      Nodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseEngineIdent)
    else if ((Nodes.IndexDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX)) then
      Nodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.MaxRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_ROWS)) then
      Nodes.MaxRowsValue := ParseValue(kiMAX_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.MinRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMIN_ROWS)) then
      Nodes.MinRowsValue := ParseValue(kiMIN_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTORAGE)) then
      Nodes.EngineValue := ParseValue(WordIndices(kiSTORAGE, kiENGINE), vaAuto, ParseEngineIdent)
    else
      Found := False;

  Result := TSubPartition.Create(Self, Nodes);
end;

function TMySQLParser.ParseStmt(const PL_SQL: Boolean = False): TOffset;
var
  FirstToken: TOffset;
  KeywordIndex: TWordList.TIndex;
  KeywordIndex1: TWordList.TIndex;
  KeywordIndex2: TWordList.TIndex;
  KeywordToken: TOffset;
  Stmt: PStmt;
  Token: PToken;
begin
  FirstToken := CurrentToken;
  KeywordToken := CurrentToken;
  if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttBeginLabel)) then
    KeywordToken := NextToken[1];

  Result := 0;

  if (KeywordToken = 0) then
    SetError(PE_IncompleteStmt)
  else
  begin
    KeywordIndex := TokenPtr(KeywordToken)^.KeywordIndex;

    if (KeywordIndex = kiALTER) then
      Result := ParseAlterStmt()
    else if (KeywordIndex = kiBEGIN) then
      if (not PL_SQL) then
        Result := ParseBeginStmt()
      else
        Result := ParseCompoundStmt()
    else if (KeywordIndex = kiCALL) then
      Result := ParseCallStmt()
    else if (PL_SQL and (KeywordIndex = kiCASE)) then
      Result := ParseCaseStmt()
    else if (PL_SQL and (KeywordIndex = kiCLOSE)) then
      Result := ParseCloseStmt()
    else if (KeywordIndex = kiCOMMIT) then
      Result := ParseCommitStmt()
    else if (KeywordIndex = kiCREATE) then
      Result := ParseCreateStmt()
    else if (PL_SQL and (KeywordIndex = kiDECLARE)) then
      Result := ParseDeclareStmt()
    else if (KeywordIndex = kiDELETE) then
      Result := ParseDeleteStmt()
    else if (KeywordIndex = kiDO) then
      Result := ParseDoStmt()
    else if (KeywordIndex = kiDROP) then
      Result := ParseDropStmt()
    else if (PL_SQL and (KeywordIndex = kiFETCH)) then
      Result := ParseFetchStmt()
    else if (PL_SQL and (KeywordIndex = kiIF)) then
      Result := ParseIfStmt()
    else if (KeywordIndex = kiINSERT) then
      Result := ParseInsertStmt()
    else if (PL_SQL and (KeywordIndex = kiITERATE)) then
      Result := ParseIterateStmt()
    else if (PL_SQL and (KeywordIndex = kiLEAVE)) then
      Result := ParseLeaveStmt()
    else if ((KeywordIndex = kiLOAD)) then
      Result := ParseLoadStmt()
    else if ((KeywordIndex = kiLOCK)) then
      Result := ParseLockStmt()
    else if (PL_SQL and (KeywordIndex = kiLOOP)) then
      Result := ParseLoopStmt()
    else if (PL_SQL and (KeywordIndex = kiOPEN)) then
      Result := ParseOpenStmt()
    else if (KeywordIndex = kiRENAME) then
      Result := ParseRenameStmt()
    else if (PL_SQL and (KeywordIndex = kiREPEAT)) then
      Result := ParseRepeatStmt()
    else if (KeywordIndex = kiREPLACE) then
      Result := ParseReleaseStmt()
    else if (KeywordIndex = kiROLLBACK) then
      Result := ParseRollbackStmt()
    else if (KeywordIndex = kiSAVEPOINT) then
      Result := ParseSavepointStmt()
    else if (KeywordIndex = kiSELECT) then
      Result := ParseSelectStmt()
    else if (KeywordIndex = kiSET) then
      Result := ParseSetStmt()
    else if (KeywordIndex = kiSHOW) then
    begin
      KeywordIndex1 := 0; KeywordIndex2 := 0;
      if (EndOfStmt(NextToken[1])) then
      begin
        KeywordIndex1 := TokenPtr(NextToken[1])^.KeywordIndex;
        if (not EndOfStmt(NextToken[2])) then
          KeywordIndex2 := TokenPtr(NextToken[2])^.KeywordIndex;
      end;
      if (KeywordIndex1 = kiAUTHORS) then
        Result := ParseShowAuthorsStmt()
      else if ((KeywordIndex = kiSHOW) and (KeywordIndex1 = kiBINARY) and (KeywordIndex2 = kiLOGS)) then
        Result := ParseShowBinaryLogsStmt()
      else if ((KeywordIndex = kiSHOW) and (KeywordIndex1 = kiMASTER) and (KeywordIndex2 = kiLOGS)) then
        Result := ParseShowBinaryLogsStmt()
      else if ((KeywordIndex = kiSHOW) and (KeywordIndex1 = kiBINLOG) and (KeywordIndex2 = kiEVENTS)) then
        Result := ParseShowBinlogEventsStmt()
      else if ((KeywordIndex = kiSHOW) and (KeywordIndex1 = kiCHARACTER) and (KeywordIndex2 = kiSET)) then
        Result := ParseShowCharacterSetStmt()
      else if ((KeywordIndex = kiSHOW) and (KeywordIndex1 = kiCOLLATION)) then
        Result := ParseShowCollationStmt()
      else if ((KeywordIndex = kiSHOW) and (KeywordIndex1 = kiCONTRIBUTORS)) then
        Result := ParseShowContributorsStmt();
    end
    else if (KeywordIndex = kiSTART) then
      Result := ParseStartTransactionStmt()
    else if (KeywordIndex = kiTRUNCATE) then
      Result := ParseTruncateTableStmt()
    else if (KeywordIndex = kiUNLOCK) then
      Result := ParseUnlockStmt()
    else if (KeywordIndex = kiUPDATE) then
      Result := ParseUpdateStmt()
    else if (PL_SQL and (KeywordIndex = kiWHILE)) then
      Result := ParseWhileStmt()
    else if (KeywordIndex = kiXA) then
      Result := ParseXAStmt()
    else
    begin
      SetError(PE_UnkownStmt, CurrentToken);
      Result := ParseUnknownStmt();
    end;

    Stmt := StmtPtr(Result);

    while (not EndOfStmt(CurrentToken)) do
    begin
      if (not Error) then
        SetError(PE_UnexpectedToken);
      if (Assigned(Stmt)) then
        Stmt^.Heritage.AddChild(CurrentToken);
      ApplyCurrentToken();
    end;

    if (Assigned(Stmt)) then
    begin
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
end;

function TMySQLParser.ParseTableIdent(): TOffset;
begin
  Result := ParseDbIdent(ditTable);
end;

function TMySQLParser.ParseTableReference(): TOffset;

  function ParseTableFactor(): TOffset;
  var
    Nodes: TSelectStmt.TTableFactor.TNodes;
    OjNodes: TSelectStmt.TTableFactorOj.TNodes;
    ReferencesNodes: TSelectStmt.TTableFactorReferences.TNodes;
    SelectNodes: TSelectStmt.TTableFactorSelect.TNodes;
  begin
    FillChar(Nodes, SizeOf(Nodes), 0);

      if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      begin
        Nodes.TableIdent := ParseTableIdent();

        if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
        begin
          Nodes.PartitionTag := ParseTag(kiPARTITION);

          if (not Error) then
            Nodes.Partitions := ParseList(True, ParsePartitionIdent);
        end;

        if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAS)) then
          Nodes.AsToken := ParseTag(kiAS);

        if (not Error and ((Nodes.AsToken > 0) or not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType in ttIdents) and (TokenPtr(CurrentToken)^.KeywordIndex < 0))) then
          Nodes.AliasToken := ParseDbIdent(ditAlias);

        if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiUSE) or (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE) or (TokenPtr(CurrentToken)^.KeywordIndex = kiFORCE))) then
          Nodes.IndexHints := ParseList(False, ParseIndexHint);

        Result := TSelectStmt.TTableFactor.Create(Self, Nodes);
      end
      else if ((TokenPtr(CurrentToken)^.TokenType = ttOpenBracket) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT)) then
      begin
        SelectNodes.SelectStmt := ParseSubArea(ParseSelectStmt);

        if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAS)) then
          SelectNodes.AsToken := ParseTag(kiAS);

        if (not Error) then
          SelectNodes.AliasToken := ParseDbIdent(ditAlias);

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
        SetError(PE_UnexpectedToken);
        Result := 0;
      end;
  end;

  procedure ApplyKeywordToken(var AKeywordTokens: TSelectStmt.TJoin.TKeywordTokens);
  var
    Index: Integer;
  begin
    Index := 0;
    while (not EndOfStmt(AKeywordTokens[Index])) do
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

    if (not Error and not EndOfStmt(CurrentToken)) then
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
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLEFT) then
          JoinType := jtNaturalLeft
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiRIGHT) then
          JoinType := jtNaturalRight
        else
          SetError(PE_UnexpectedToken);
      end
      else if (KeywordIndex = kiSTRAIGHT_JOIN) then
        JoinType := jtEqui
      else
        JoinType := jtUnknown;

      if (JoinType <> jtUnknown) then
      begin
        if (JoinType in [jtNaturalLeft, jtNaturalRight]) then
          SetError(PE_UnexpectedToken)
        else
        begin
          ApplyKeywordToken(KeywordTokens);

          if ((JoinType in [jtLeft, jtRight]) and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiOUTER)) then
            ApplyKeywordToken(KeywordTokens);

          if (JoinType in [jtInner, jtCross, jtLeft, jtRight]) then
            if (EndOfStmt(CurrentToken)) then
              SetError(PE_IncompleteStmt)
            else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiJOIN) then
              SetError(PE_UnexpectedToken)
            else
              ApplyKeywordToken(KeywordTokens);
        end;

        if (not Error) then
          if ((JoinType in [jtInner, jtCross, jtEqui]) or (JoinType in [jtNaturalLeft, jtNaturalRight])) then
            JoinedTable := ParseTableFactor()
          else
            JoinedTable := ParseTableReference();

        if (not Error) then
          if not EndOfStmt(CurrentToken) then
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

    if (Error or EndOfStmt(CurrentToken)) then
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

function TMySQLParser.ParseTag(const KeywordIndex1: TWordList.TIndex; const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1; const KeywordIndex4: TWordList.TIndex = -1): TOffset;
var
  Nodes: TTag.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex1) then
    SetError(PE_UnexpectedToken)
  else
  begin
    TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
    Nodes.KeywordToken1 := ApplyCurrentToken();

    if (KeywordIndex2 >= 0) then
    begin
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex2) then
        SetError(PE_UnexpectedToken)
      else
      begin
        TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
        Nodes.KeywordToken2 := ApplyCurrentToken();

        if (KeywordIndex3 >= 0) then
        begin
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex3) then
            SetError(PE_UnexpectedToken)
          else
          begin
            TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
            Nodes.KeywordToken3 := ApplyCurrentToken();

            if (KeywordIndex4 >= 0) then
            begin
              if (EndOfStmt(CurrentToken)) then
                SetError(PE_IncompleteStmt)
              else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex4) then
                SetError(PE_UnexpectedToken)
              else
              begin
                TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
                Nodes.KeywordToken4 := ApplyCurrentToken();
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  Result := TTag.Create(Self, Nodes);
end;

function TMySQLParser.ParseToken(): TOffset;
label
  TwoChars,
  Selection, SelSpace, SelQuotedIdent, SelNotLess, SelNotEqual1, SelNotGreater, SelNot1, SelDoubleQuote, SelComment, SelModulo, SelDolor, SelAmpersand2, SelBitAND, SelSingleQuote, SelOpenBracket, SelCloseBracket, SelMySQLCodeEnd, SelMulti, SelComma, SelDoubleDot, SelDot, SelMySQLCode, SelDiv, SelNumeric, SelSLComment, SelArrow, SelMinus, SelPlus, SelAssign, SelColon, SelDelimiter, SelNULLSaveEqual, SelLessEqual, SelShiftLeft, SelNotEqual2, SelLess, SelEqual, SelGreaterEqual, SelShiftRight, SelGreater, SelParameter, SelAt, SelUnquotedIdent, SelDBIdent, SelBackslash, SelCloseSquareBracket, SelHat, SelMySQLCharacterSet, SelMySQLIdent, SelUnquotedIdentLower, SelOpenCurlyBracket, SelOpenCurlyBracket2, SelOpenCurlyBracket3, SelPipe, SelBitOR, SelCloseCurlyBracket, SelTilde, SelE,
  BindVariable,
  Colon,
  Comment,
  Intger, IntgerL, IntgerE,
  MLComment, MLCommentL, MLComment2,
  MySQLCharacterSet, MySQLCharacterSetL, MySQLCharacterSetLE, MySQLCharacterSetE,
  MySQLCondCode, MySQLCondCodeL, MySQLCondCodeE,
  Numeric, NumericL, NumericExp, NumericE, NumericDot, NumericLE,
  QuotedIdent, QuotedIdent2,
  Return, ReturnE,
  Separator,
  UnquotedIdent, UnquotedIdentLE, UnquotedIdentLabel,
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
      MOV TokenType,ttDQIdent
      MOV DX,'"'                       // End Quoter
      JMP QuotedIdent
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
      JMP QuotedIdent
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
      JNE SelArrow                     // No!
      CMP ECX,3                        // Three characters in SQL?
      JB DoubleChar                    // No!
      CMP WORD PTR [ESI + 4],9         // "--<Tab>" ?
      JE Comment                       // Yes!
      CMP WORD PTR [ESI + 4],10        // "--<LF>" ?
      JE Comment                       // Yes!
      CMP WORD PTR [ESI + 4],13        // "--<CR>" ?
      JE Comment                       // Yes!
      CMP WORD PTR [ESI + 4],' '       // "-- " ?
      JE Comment                       // Yes!
      JE DoubleChar
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
      JNE SelUnquotedIdent        // No!
      MOV TokenType,ttAt
      JMP SingleChar
    SelUnquotedIdent:
      CMP AX,'Z'                       // Up case character?
      JA SelDBIdent               // No!
      MOV TokenType,ttIdent
      JMP UnquotedIdent           // Yes!
    SelDBIdent:
      CMP AX,'['                       // "[" ?
      JNE SelBackslash                 // No!
      MOV TokenType,ttDBIdent
      MOV DX,']'                       // End Quoter
      JMP QuotedIdent
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
      JNE SelUnquotedIdentLower   // No!
      MOV TokenType,ttMySQLIdent
      MOV DX,'`'                       // End Quoter
      JMP QuotedIdent
    SelUnquotedIdentLower:
      CMP AX,'z'                       // Low case character?
      JA SelOpenCurlyBracket           // No!
      MOV TokenType,ttIdent
      JMP UnquotedIdent           // Yes!
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
      MOV TokenType,ttBRIdent
      MOV DX,'}'                       // End Quoter
      JMP QuotedIdent
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
      JNE UnquotedIdent           // No!
      JMP Syntax

    // ------------------------------

    BindVariable:
      MOV TokenType,ttBindVariable
      JMP UnquotedIdent

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
      JMP QuotedIdent

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

    QuotedIdent:
      // DX: End Quoter
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      CMP ECX,0                        // End of SQL?
      JE Incomplete                    // Yes!
    QuotedIdent2:
      MOV AX,[ESI]                     // One Character from SQL to AX
      CMP AX,'\'                       // Escaper?
      JE QuotedIdent
      CMP AX,DX                        // End Quoter (unescaped)?
      JNE QuotedIdent             // No!
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

    UnquotedIdent:
      CALL Separator                   // SQL separator?
      JE Finish
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      MOV AX,[ESI]                     // One Character from SQL to AX
      CMP AX,'@'
      JE UnquotedIdent
      CMP AX,'0'
      JB Finish
      CMP AX,'9'
      JBE UnquotedIdent
      CMP AX,':'
      JE UnquotedIdentLabel
      CMP AX,'A'
      JB Finish
      CMP AX,'Z'
      JBE UnquotedIdent
      CMP AX,'_'
      JE UnquotedIdent
      CMP AX,'a'
      JB Finish
      CMP AX,'z'
      JBE UnquotedIdent
      CMP AX,128
      JAE UnquotedIdent
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
    if (AnsiQuotes and (TokenType = ttMySQLIdent)
      or not AnsiQuotes and (TokenType = ttDQIdent)) then
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

    if (OperatorType <> otUnknown) then
      TokenType := ttOperator;

    if (TokenType <> ttIdent) then
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

function TMySQLParser.ParseTransactionCharacteristic(): TOffset;
var
  Nodes: TSetTransactionStmt.TCharacteristic.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiISOLATION) then
  begin
    Nodes.KindTag := ParseTag(kiISOLATION, kiLEVEL);

    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREPEATABLE) then
      Nodes.Value := ParseTag(kiREPEATABLE, kiREAD)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREAD) then
      if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOMMITTED)) then
        Nodes.Value := ParseTag(kiREAD, kiCOMMITTED)
      else
        Nodes.Value := ParseTag(kiREAD, kiUNCOMMITTED)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSERIALIZABLE) then
      Nodes.Value := ParseTag(kiSERIALIZABLE)
    else
      SetError(PE_UnexpectedToken)
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREAD) then
    if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiWRITE)) then
      Nodes.KindTag := ParseTag(kiREAD, kiWRITE)
    else
      Nodes.KindTag := ParseTag(kiREAD, kiREAD)
  else
   SetError(PE_UnexpectedToken);

  Result := TSetTransactionStmt.TCharacteristic.Create(Self, Nodes);
end;

function TMySQLParser.ParseTruncateTableStmt(): TOffset;
var
  Nodes: TTruncateStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.TruncateTag := ParseTag(kiTRUNCATE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTABLE)) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  Result := TTruncateStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseUnknownStmt(): TOffset;
var
  FirstToken: TOffset;
  Tokens: Classes.TList;
begin
  Tokens := Classes.TList.Create();

  FirstToken := CurrentToken;
  while (not EndOfStmt(CurrentToken)) do
    Tokens.Add(Pointer(ApplyCurrentToken()));

  Result := TUnknownStmt.Create(Self, Tokens);

  StmtPtr(Result)^.FFirstToken := FirstToken;
  StmtPtr(Result)^.FLastToken := Root^.FLastToken;

  Tokens.Free();
end;

function TMySQLParser.ParseUnlockStmt(): TOffset;
var
  Nodes: TUnlockStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.UnlockTablesTag := ParseTag(kiUNLOCK, kiTABLES);

  Result := TUnlockStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseUpdateStmt(): TOffset;
var
  Nodes: TUpdateStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.UpdateTag := ParseTag(kiUPDATE);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOW_PRIORITY) then
      Nodes.PriorityTag := ParseTag(kiLOW_PRIORITY)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCONCURRENT) then
      Nodes.PriorityTag := ParseTag(kiCONCURRENT);

  if (not Error) then
    Nodes.TableReferences := ParseList(False, ParseTableReference);

  if (not Error) then
    Nodes.SetValue := ParseValue(kiSET, vaNo, False, ParseUpdatePair);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE)) then
    Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  if (not Error and (PList(NodePtr(Nodes.TableReferences))^.Count = 1)) then
  begin
    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiORDER)) then
      Nodes.OrderByValue := ParseValue(WordIndices(kiORDER, kiBY), vaNo, False, ParseIndexColumn);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLIMIT)) then
      Nodes.LimitValue := ParseValue(kiLIMIT, vaNo, ParseInteger);
  end;

  Result := TUpdateStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseUpdatePair(): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseColumnIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
      SetError(PE_UnexpectedToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT)) then
      Nodes.ValueNode := ApplyCurrentToken()
    else
      Nodes.ValueNode := ParseExpr();

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseUser(): TOffset;
var
  Nodes: TUser.TNodes;
begin
  Result := 0;

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCURRENT_USER) then
    if (((NextToken[1] = 0) or (TokenPtr(NextToken[1])^.TokenType <> ttOpenBracket))
      and ((NextToken[2] = 0) or (TokenPtr(NextToken[2])^.TokenType <> ttCloseBracket))) then
      Result := ApplyCurrentToken()
    else
      Result := ParseFunction()
  else if (not (TokenPtr(CurrentToken)^.TokenType in [ttIdent, ttString])) then
    SetError(PE_UnexpectedToken)
  else
  begin
    FillChar(Nodes, SizeOf(Nodes), 0);

    Nodes.NameToken := ApplyCurrentToken();

    if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttAt)) then
    begin
      Nodes.AtToken := ApplyCurrentToken();

      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (not (TokenPtr(CurrentToken)^.TokenType in [ttIdent, ttString])) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.HostToken := ApplyCurrentToken();
    end;

    if (not Error) then
      Result := TUser.Create(Self, Nodes);
  end;
end;

function TMySQLParser.ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const Brackets: Boolean; const ParseItem: TParseFunction): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndex);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
      SetError(PE_UnexpectedToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    Nodes.ValueNode := ParseList(Brackets, ParseItem);

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const OptionIndices: TWordList.TIndices): TOffset;
var
  CurrentKeywordIndex: Integer;
  I: Integer;
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndex);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
      SetError(PE_UnexpectedToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
  begin
    CurrentKeywordIndex := TokenPtr(CurrentToken)^.KeywordIndex;
    for I := 0 to Length(OptionIndices) - 1 do
      if ((OptionIndices[I] < 0)) then
        break
      else if (OptionIndices[I] = CurrentKeywordIndex) then
      begin
        Nodes.ValueNode := ParseTag(CurrentKeywordIndex);
        break;
      end;
    if (Nodes.ValueNode = 0) then
      SetError(PE_UnexpectedToken);
  end;

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndex);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
      SetError(PE_UnexpectedToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    Nodes.ValueNode := ParseValueNode();

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const Brackets: Boolean; const ParseItem: TParseFunction): TOffset;
var
  Nodes: TValue.TNodes;
begin
  Assert(KeywordIndices[4] = -1);

  FillChar(Nodes, SizeOf(Nodes), 0);

  ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2], KeywordIndices[3]);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
      SetError(PE_UnexpectedToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    Nodes.ValueNode := ParseList(Brackets, ParseItem);

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const OptionIndices: TWordList.TIndices): TOffset;
var
  CurrentKeywordIndex: Integer;
  I: Integer;
  Nodes: TValue.TNodes;
begin
  Assert(KeywordIndices[4] = -1);

  FillChar(Nodes, SizeOf(Nodes), 0);

  ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2], KeywordIndices[3]);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
      SetError(PE_UnexpectedToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
  begin
    CurrentKeywordIndex := TokenPtr(CurrentToken)^.KeywordIndex;
    for I := 0 to Length(OptionIndices) - 1 do
      if ((OptionIndices[I] < 0)) then
        break
      else if (OptionIndices[I] = CurrentKeywordIndex) then
      begin
        Nodes.ValueNode := ParseTag(CurrentKeywordIndex);
        break;
      end;
    if (Nodes.ValueNode = 0) then
      SetError(PE_UnexpectedToken);
  end;

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset;
var
  Nodes: TValue.TNodes;
begin
  Assert(KeywordIndices[4] = -1);

  FillChar(Nodes, SizeOf(Nodes), 0);

  ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2], KeywordIndices[3]);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
      SetError(PE_UnexpectedToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    Nodes.ValueNode := ParseValueNode();

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ValueKeywordIndex1: TWordList.TIndex; const ValueKeywordIndex2: TWordList.TIndex = -1): TOffset;
var
  Nodes: TValue.TNodes;
begin
  Assert(KeywordIndices[4] = -1);

  FillChar(Nodes, SizeOf(Nodes), 0);

  ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2], KeywordIndices[3]);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType in [otEqual, otAssign])) then
      SetError(PE_UnexpectedToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    Nodes.ValueNode := ParseTag(ValueKeywordIndex1, ValueKeywordIndex2);

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseValueIdent(): TOffset;
begin
  Result := ParseDbIdent(ditVariable);
end;

function TMySQLParser.ParseVariableIdent(): TOffset;
var
  Nodes: TVariable.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttAt)) then
    Nodes.At1Token := ApplyCurrentToken();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttAt)) then
  begin
    Nodes.At2Token := Nodes.At1Token;
    Nodes.At1Token := ApplyCurrentToken();
  end;

  if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiGLOBAL) or (TokenPtr(CurrentToken)^.KeywordIndex = kiSESSION))) then
  begin
    Nodes.ScopeTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType <> otDot) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.DotToken := ApplyCurrentToken();
  end;

  if (not Error) then
    Nodes.IdentTag := ParseDbIdent(ditVariable);

  Result := TVariable.Create(Self, Nodes);
end;

function TMySQLParser.ParseViewIdent(): TOffset;
begin
  Result := ParseDbIdent(ditView);
end;

function TMySQLParser.ParseWhileStmt(): TOffset;
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

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)) then
    if ((Nodes.BeginLabelToken = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(Nodes.BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EndLabelToken := ApplyCurrentToken(utLabel, ttEndLabel);

  Result := TWhileStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseXAStmt(): TOffset;
var
  Nodes: TXAStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.XATag := ParseTag(kiXA);

  if (not Error) then
    Nodes.XATag := ParseTag(kiXA);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiBEGIN) or (TokenPtr(CurrentToken)^.KeywordIndex = kiSTART)) then
  begin
    Nodes.ActionTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

    if (not Error) then
      Nodes.Ident := ParseDbIdent(ditXA);

    if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiJOIN) or (TokenPtr(CurrentToken)^.KeywordIndex = kiRESUME))) then
      Nodes.RestTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMIT) then
  begin
    Nodes.ActionTag := ParseTag(kiCOMMIT);

    if (not Error) then
      Nodes.Ident := ParseDbIdent(ditXA);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiONE)) then
      Nodes.RestTag := ParseTag(kiONE, kiPHASE);
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEND) then
  begin
    Nodes.ActionTag := ParseTag(kiEND);

    if (not Error) then
      Nodes.Ident := ParseDbIdent(ditXA);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSUSPEND)) then
      if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiFOR)) then
        Nodes.RestTag := ParseTag(kiSUSPEND, kiFOR, kiMIGRATE)
      else
        Nodes.RestTag := ParseTag(kiSUSPEND);
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiPREPARE) then
  begin
    Nodes.ActionTag := ParseTag(kiPREPARE);

    if (not Error) then
      Nodes.Ident := ParseDbIdent(ditXA);
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiRECOVER) then
  begin
    Nodes.ActionTag := ParseTag(kiRECOVER);
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiROLLBACK) then
  begin
    Nodes.ActionTag := ParseTag(kiROLLBACK);

    if (not Error) then
      Nodes.Ident := ParseDbIdent(ditXA);
  end
  else
    SetError(PE_UnexpectedToken);

  Result := TXAStmt.Create(Self, Nodes);
end;

function TMySQLParser.RangeNodePtr(const ANode: TOffset): PRange;
begin
  Assert(IsRange(NodePtr(ANode)));

  Result := PRange(NodePtr(ANode));
end;

procedure TMySQLParser.SaveToFile(const Filename: string; const FileType: TFileType = ftSQL);
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
    '      font: 12px Verdana,Arial,Sans-Serif;' + #13#10 +
    '      color: #000;' + #13#10 +
    '    }' + #13#10 +
    '    td {' + #13#10 +
    '      font: 12px Verdana,Arial,Sans-Serif;' + #13#10 +
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
    '      background-color: #FFD;' + #13#10 +
    '      padding: 0px 2px 2px 1px;' + #13#10 +
    '      border: 1px solid #000;' + #13#10 +
    '      color: #000;' + #13#10 +
    '    }' + #13#10 +
    '    .Node {' + #13#10 +
    '      font-size: 11px;' + #13#10 +
    '      text-align: center;' + #13#10 +
    '      background-color: #F0F0F0;' + #13#10 +
    '    }' + #13#10 +
    '    .SQL {' + #13#10 +
    '      font-size: 12px;' + #13#10 +
    '      background-color: #F0F0F0;' + #13#10 +
    '      text-align: center;' + #13#10 +
    '    }' + #13#10 +
    '    .StmtError {' + #13#10 +
    '      font-size: 12px;' + #13#10 +
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

    if (Stmt^.Error and (Stmt^.ErrorCode <> PE_IncompleteStmt) and Assigned(Stmt^.ErrorToken)) then
    begin
      HTML := HTML
        + '<tr class="">';
      if (Stmt^.ErrorToken^.Index - Stmt^.FirstToken^.Index > 0) then
        HTML := HTML
          + '<td colspan="' + IntToStr(Stmt^.ErrorToken^.Index - Stmt^.FirstToken^.Index) + '"></td>';
      HTML := HTML
        + '<td class="StmtError"><a href="">&uarr;'
        + '<span><table cellspacing="2" cellpadding="0">'
        + '<tr><td>ErrorCode:</td><td>' + IntToStr(Stmt^.ErrorCode) + '</td></tr>'
        + '<tr><td>ErrorMessage:</td><td>' + HTMLEscape(Stmt^.ErrorMessage) + '</td></tr>'
        + '</table></span>'
        + '</a></td>';
      if (Stmt^.ErrorToken^.Index - Stmt^.LastToken^.Index > 0) then
        HTML := HTML
          + '<td colspan="' + IntToStr(Stmt^.LastToken^.Index - Stmt^.ErrorToken^.Index) + '"></td>';
      HTML := HTML
        + '</tr>' + #13#10;
    end
    else if (ErrorCode = PE_IncompleteStmt) then
      HTML := HTML
        + '<tr><td class="StmtError" colspan="' + IntToStr(Stmt^.LastToken^.Index + 1) + '">' + ErrorMessage + '</td></tr>' + #13#10;

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

procedure TMySQLParser.SetError(const AErrorCode: Integer; const AErrorNode: TOffset = 0);
begin
  Assert(not Error and ((AErrorCode <> PE_IncompleteStmt) or (AErrorNode = 0)));

  FErrorCode := AErrorCode;
  if (AErrorNode = 0) then
    FErrorToken := CurrentToken
  else
    FErrorToken := StmtNodePtr(AErrorNode)^.FFirstToken;
end;

procedure TMySQLParser.SetFunctions(AFunctions: string);
begin
  FFunctions.Text := AFunctions;
end;

procedure TMySQLParser.SetKeywords(AKeywords: string);

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
    kiAUTHORS             := IndexOf('AUTHORS');
    kiAVG_ROW_LENGTH      := IndexOf('AVG_ROW_LENGTH');
    kiBEFORE              := IndexOf('BEFORE');
    kiBEGIN               := IndexOf('BEGIN');
    kiBETWEEN             := IndexOf('BETWEEN');
    kiBINARY              := IndexOf('BINARY');
    kiBINLOG              := IndexOf('BINLOG');
    kiBTREE               := IndexOf('BTREE');
    kiBY                  := IndexOf('BY');
    kiCALL                := IndexOf('CALL');
    kiCASCADE             := IndexOf('CASCADE');
    kiCASCADED            := IndexOf('CASCADED');
    kiCASE                := IndexOf('CASE');
    kiCHANGE              := IndexOf('CHANGE');
    kiCHAIN               := IndexOf('CHAIN');
    kiCHARACTER           := IndexOf('CHARACTER');
    kiCHECK               := IndexOf('CHECK');
    kiCHECKSUM            := IndexOf('CHECKSUM');
    kiCLOSE               := IndexOf('CLOSE');
    kiCOALESCE            := IndexOf('COALESCE');
    kiCOLLATE             := IndexOf('COLLATE');
    kiCOLLATION           := IndexOf('COLLATION');
    kiCOLUMN              := IndexOf('COLUMN');
    kiCOLUMN_FORMAT       := IndexOf('COLUMN_FORMAT');
    kiCOLUMNS             := IndexOf('COLUMNS');
    kiCOMMENT             := IndexOf('COMMENT');
    kiCOMMIT              := IndexOf('COMMIT');
    kiCOMMITTED           := IndexOf('COMMITTED');
    kiCOMPACT             := IndexOf('COMPACT');
    kiCOMPLETION          := IndexOf('COMPLETION');
    kiCOMPRESSED          := IndexOf('COMPRESSED');
    kiCONCURRENT          := IndexOf('CONCURRENT');
    kiCONNECTION          := IndexOf('CONNECTION');
    kiCONSISTENT          := IndexOf('CONSISTENT');
    kiCONSTRAINT          := IndexOf('CONSTRAINT');
    kiCONTAINS            := IndexOf('CONTAINS');
    kiCONTRIBUTORS        := IndexOf('CONTRIBUTORS');
    kiCONVERT             := IndexOf('CONVERT');
    kiCOPY                := IndexOf('COPY');
    kiCREATE              := IndexOf('CREATE');
    kiCROSS               := IndexOf('CROSS');
    kiCURRENT_USER        := IndexOf('CURRENT_USER');
    kiCURSOR              := IndexOf('CURSOR');
    kiDATA                := IndexOf('DATA');
    kiDATABASE            := IndexOf('DATABASE');
    kiDAY                 := IndexOf('DAY');
    kiDAY_HOUR            := IndexOf('DAY_HOUR');
    kiDAY_MINUTE          := IndexOf('DAY_MINUTE');
    kiDAY_SECOND          := IndexOf('DAY_SECOND');
    kiDECLARE             := IndexOf('DECLARE');
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
    kiDYNAMIC             := IndexOf('DYNAMIC');
    kiEACH                := IndexOf('EACH');
    kiELSE                := IndexOf('ELSE');
    kiELSEIF              := IndexOf('ELSEIF');
    kiENABLE              := IndexOf('ENABLE');
    kiENCLOSED            := IndexOf('ENCLOSED');
    kiEND                 := IndexOf('END');
    kiENDS                := IndexOf('ENDS');
    kiENGINE              := IndexOf('ENGINE');
    kiEVENT               := IndexOf('EVENT');
    kiEVENTS              := IndexOf('EVENTS');
    kiESCAPED             := IndexOf('ESCAPED');
    kiEVERY               := IndexOf('EVERY');
    kiEXCHANGE            := IndexOf('EXCHANGE');
    kiEXCLUSIVE           := IndexOf('EXCLUSIVE');
    kiEXISTS              := IndexOf('EXISTS');
    kiFETCH               := IndexOf('FETCH');
    kiFIELDS              := IndexOf('FIELDS');
    kiFIRST               := IndexOf('FIRST');
    kiFIXED               := IndexOf('FIXED');
    kiFOR                 := IndexOf('FOR');
    kiFORCE               := IndexOf('FORCE');
    kiFOREIGN             := IndexOf('FOREIGN');
    kiFROM                := IndexOf('FROM');
    kiFULL                := IndexOf('FULL');
    kiFULLTEXT            := IndexOf('FULLTEXT');
    kiFUNCTION            := IndexOf('FUNCTION');
    kiGLOBAL              := IndexOf('GLOBAL');
    kiGROUP               := IndexOf('GROUP');
    kiHASH                := IndexOf('HASH');
    kiHAVING              := IndexOf('HAVING');
    kiHIGH_PRIORITY       := IndexOf('HIGH_PRIORITY');
    kiHOST                := IndexOf('HOST');
    kiHOUR                := IndexOf('HOUR');
    kiHOUR_MINUTE         := IndexOf('HOUR_MINUTE');
    kiHOUR_SECOND         := IndexOf('HOUR_SECOND');
    kiIDENTIFIED          := IndexOf('IDENTIFIED');
    kiIF                  := IndexOf('IF');
    kiIGNORE              := IndexOf('IGNORE');
    kiIMPORT              := IndexOf('IMPORT');
    kiIN                  := IndexOf('IN');
    kiINDEX               := IndexOf('INDEX');
    kiINNER               := IndexOf('INNER');
    kiINFILE              := IndexOf('INFILE');
    kiINOUT               := IndexOf('INOUT');
    kiINPLACE             := IndexOf('INPLACE');
    kiINSERT              := IndexOf('INSERT');
    kiINSERT_METHOD       := IndexOf('INSERT_METHOD');
    kiINTERVAL            := IndexOf('INTERVAL');
    kiINTO                := IndexOf('INTO');
    kiINVOKER             := IndexOf('INVOKER');
    kiIS                  := IndexOf('IS');
    kiISOLATION           := IndexOf('ISOLATION');
    kiITERATE             := IndexOf('ITERATE');
    kiJOIN                := IndexOf('JOIN');
    kiKEY                 := IndexOf('KEY');
    kiKEY_BLOCK_SIZE      := IndexOf('KEY_BLOCK_SIZE');
    kiKEYS                := IndexOf('KEYS');
    kiLANGUAGE            := IndexOf('LANGUAGE');
    kiLAST                := IndexOf('LAST');
    kiLEAVE               := IndexOf('LEAVE');
    kiLEFT                := IndexOf('LEFT');
    kiLESS                := IndexOf('LESS');
    kiLEVEL               := IndexOf('LEVEL');
    kiLIKE                := IndexOf('LIKE');
    kiLIMIT               := IndexOf('LIMIT');
    kiLINEAR              := IndexOf('LINEAR');
    kiLINES               := IndexOf('LINES');
    kiLIST                := IndexOf('LIST');
    kiLOGS                := IndexOf('LOGS');
    kiLOAD                := IndexOf('LOAD');
    kiLOCAL               := IndexOf('LOCAL');
    kiLOCK                := IndexOf('LOCK');
    kiLOOP                := IndexOf('LOOP');
    kiLOW_PRIORITY        := IndexOf('LOW_PRIORITY');
    kiMASTER              := IndexOf('MASTER');
    kiMATCH               := IndexOf('MATCH');
    kiMAX_ROWS            := IndexOf('MAX_ROWS');
    kiMAXVALUE            := IndexOf('MAXVALUE');
    kiMERGE               := IndexOf('MERGE');
    kiMIGRATE             := IndexOf('MIGRATE');
    kiMIN_ROWS            := IndexOf('MIN_ROWS');
    kiMINUTE              := IndexOf('MINUTE');
    kiMINUTE_SECOND       := IndexOf('MINUTE_SECOND');
    kiMOD                 := IndexOf('MOD');
    kiMODIFIES            := IndexOf('MODIFIES');
    kiMODIFY              := IndexOf('MODIFY');
    kiMONTH               := IndexOf('MONTH');
    kiNAME                := IndexOf('NAME');
    kiNATURAL             := IndexOf('NATURAL');
    kiNEXT                := IndexOf('NEXT');
    kiNO                  := IndexOf('NO');
    kiNONE                := IndexOf('NONE');
    kiNOT                 := IndexOf('NOT');
    kiNULL                := IndexOf('NULL');
    kiOFFSET              := IndexOf('OFFSET');
    kiOJ                  := IndexOf('OJ');
    kiON                  := IndexOf('ON');
    kiONE                 := IndexOf('ONE');
    kiONLY                := IndexOf('ONLY');
    kiOPEN                := IndexOf('OPEN');
    kiOPTIMIZE            := IndexOf('OPTIMIZE');
    kiOPTION              := IndexOf('OPTION');
    kiOPTIONAL            := IndexOf('OPTIONAL');
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
    kiPHASE               := IndexOf('PHASE');
    kiRECOVER             := IndexOf('RECOVER');
    kiREDUNDANT           := IndexOf('REDUNDANT');
    kiPORT                := IndexOf('PORT');
    kiPREPARE             := IndexOf('PREPARE');
    kiPRESERVE            := IndexOf('PRESERVE');
    kiPRIMARY             := IndexOf('PRIMARY');
    kiPROCEDURE           := IndexOf('PROCEDURE');
    kiQUARTER             := IndexOf('QUARTER');
    kiQUICK               := IndexOf('QUICK');
    kiRANGE               := IndexOf('RANGE');
    kiREAD                := IndexOf('READ');
    kiREADS               := IndexOf('READS');
    kiREBUILD             := IndexOf('REBUILD');
    kiREFERENCES          := IndexOf('REFERENCES');
    kiREGEXP              := IndexOf('REGEXP');
    kiRELEASE             := IndexOf('RELEASE');
    kiREMOVE              := IndexOf('REMOVE');
    kiRENAME              := IndexOf('RENAME');
    kiREORGANIZE          := IndexOf('REORGANIZE');
    kiREPEAT              := IndexOf('REPEAT');
    kiREPAIR              := IndexOf('REPAIR');
    kiREPEATABLE          := IndexOf('REPEATABLE');
    kiREPLACE             := IndexOf('REPLACE');
    kiRESTRICT            := IndexOf('RESTRICT');
    kiRESUME              := IndexOf('RESUME');
    kiRETURNS             := IndexOf('RETURNS');
    kiRIGHT               := IndexOf('RIGHT');
    kiRLIKE               := IndexOf('RLIKE');
    kiROLLBACK            := IndexOf('ROLLBACK');
    kiROLLUP              := IndexOf('ROLLUP');
    kiROW                 := IndexOf('ROW');
    kiROW_FORMAT          := IndexOf('ROW_FORMAT');
    kiROWS                := IndexOf('ROWS');
    kiSAVEPOINT           := IndexOf('SAVEPOINT');
    kiSCHEDULE            := IndexOf('SCHEDULE');
    kiSCHEMA              := IndexOf('SCHEMA');
    kiSECOND              := IndexOf('SECOND');
    kiSECURITY            := IndexOf('SECURITY');
    kiSELECT              := IndexOf('SELECT');
    kiSERIALIZABLE        := IndexOf('SERIALIZABLE');
    kiSERVER              := IndexOf('SERVER');
    kiSESSION             := IndexOf('SESSION');
    kiSET                 := IndexOf('SET');
    kiSHARED              := IndexOf('SHARED');
    kiSHOW                := IndexOf('SHOW');
    kiSIMPLE              := IndexOf('SIMPLE');
    kiSNAPSHOT            := IndexOf('SNAPSHOT');
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
    kiSTARTING            := IndexOf('STARTING');
    kiSTART               := IndexOf('START');
    kiSTARTS              := IndexOf('STARTS');
    kiSTATS_AUTO_RECALC   := IndexOf('STATS_AUTO_RECALC');
    kiSTATS_PERSISTENT    := IndexOf('STATS_PERSISTENT');
    kiSTORAGE             := IndexOf('STORAGE');
    kiSTRAIGHT_JOIN       := IndexOf('STRAIGHT_JOIN');
    kiSUBPARTITION        := IndexOf('SUBPARTITION');
    kiSUBPARTITIONS       := IndexOf('SUBPARTITIONS');
    kiSUSPEND             := IndexOf('SUSPEND');
    kiTABLE               := IndexOf('TABLE');
    kiTABLES              := IndexOf('TABLES');
    kiTABLESPACE          := IndexOf('TABLESPACE');
    kiTEMPORARY           := IndexOf('TEMPORARY');
    kiTEMPTABLE           := IndexOf('TEMPTABLE');
    kiTERMINATED          := IndexOf('TERMINATED');
    kiTHAN                := IndexOf('THAN');
    kiTHEN                := IndexOf('THEN');
    kiTO                  := IndexOf('TO');
    kiTRANSACTION         := IndexOf('TRANSACTION');
    kiTRIGGER             := IndexOf('TRIGGER');
    kiTRUNCATE            := IndexOf('TRUNCATE');
    kiUNCOMMITTED         := IndexOf('UNCOMMITTED');
    kiUNDEFINED           := IndexOf('UNDEFINED');
    kiUNION               := IndexOf('UNION');
    kiUNIQUE              := IndexOf('UNIQUE');
    kiUNLOCK              := IndexOf('UNLOCK');
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
    kiWORK                := IndexOf('WORK');
    kiWRITE               := IndexOf('WRITE');
    kiXA                  := IndexOf('XA');
    kiXML                 := IndexOf('XML');
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

function TMySQLParser.StmtNodePtr(const ANode: TOffset): PChild;
begin
  if (not IsChild(NodePtr(ANode))) then
    Result := nil
  else
    Result := @FNodes.Mem[ANode];
end;

function TMySQLParser.StmtPtr(const ANode: TOffset): PStmt;
begin
  if (not IsChild(NodePtr(ANode))) then
    Result := nil
  else
    Result := @FNodes.Mem[ANode];
end;

function TMySQLParser.TokenPtr(const ANode: TOffset): PToken;
begin
  Assert((ANode = 0) or (NodePtr(ANode)^.FNodeType = ntToken));

  if (ANode = 0) then
    Result := nil
  else
    Result := PToken(NodePtr(ANode));
end;

end.
// If / Case Operator
// DbIdent column / function
// Multiple Stmts in Debug

