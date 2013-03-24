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
  private
    type
      TIntergerArray = array of Integer;
      TRoutineType = (rtFunction, rtProcedure);
      TValueAssign = (vaYes, vaNo, vaAuto);
  protected
    type
      TOffset = Integer;
      TOrigin = record X, Y: Integer; end;

      TWordList = class(TObject)
      private type
        TIndex = SmallInt;
      private
        FCount: TWordList.TIndex;
        FIndex: array of PChar;
        FFirst: array of Integer;
        FParser: TCustomSQLParser;
        FText: string;
        function GetWord(Index: TWordList.TIndex): string;
        procedure SetText(AText: string);
      protected
        procedure Clear();
        property Parser: TCustomSQLParser read FParser;
      public
        constructor Create(const ASQLParser: TCustomSQLParser; const AText: string = '');
        destructor Destroy(); override;
        function IndexOf(const Word: PChar; const Length: Integer): Integer;
        property Count: TWordList.TIndex read FCount;
        property Text: string read FText write SetText;
        property Word[Index: TWordList.TIndex]: string read GetWord; default;
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
        FFirstToken: TOffset;
        FLastToken: TOffset;
      private
        FFirstStmt: TOffset;
        FLastStmt: TOffset;
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
        FParentNode: TOffset;
      private
        function GetFFirstToken(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFLastToken(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetNextSibling(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        property FFirstToken: TOffset read GetFFirstToken;
        property FLastToken: TOffset read GetFLastToken;
      public
        property FirstToken: PToken read GetFirstToken;
        property LastToken: PToken read GetLastToken;
        property NextSibling: PNode read GetNextSibling;
        property NodeType: TNodeType read Heritage.FNodeType;
        property ParentNode: PNode read GetParentNode;
        property Parser: TCustomSQLParser read Heritage.FParser;
      end;

      TToken = packed record
      private
        Heritage: TChild;
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
        property FParentNode: TOffset read Heritage.FParentNode write Heritage.FParentNode;
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
        FFirstToken: TOffset;
        FLastToken: TOffset;
        property FParentNode: TOffset read Heritage.FParentNode write Heritage.FParentNode;
      private
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

      { Stmt nodes ------------------------------------------------------------}

      TStmt = packed record
      private
        Heritage: TRange;
        FStmtType: TStmtType;
        FErrorCode: Integer;
        FErrorToken: TOffset;
        property FFirstToken: TOffset read Heritage.FFirstToken write Heritage.FFirstToken;
        property FLastToken: TOffset read Heritage.FLastToken write Heritage.FLastToken;
        property FParentNode: TOffset read Heritage.Heritage.FParentNode write Heritage.Heritage.FParentNode;
      private
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

      PBinaryOp = ^TBinaryOp;
      TBinaryOp = packed record
      private
        Heritage: TRange;
      private type
        TNodes = record
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
        TNodes = record
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

      PCaseOp = ^TCaseOp;
      TCaseOp = packed record
      private type

        PBranch = ^TBranch;
        TBranch = packed record
        private type
          TNodes = record
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

        TNodes = record
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
          TNodes = record
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

        TNodes = record
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

      PCompoundStmt = ^TCompoundStmt;
      TCompoundStmt = packed record
      private type
        TNodes = record
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

      PCreateRoutineStmt = ^TCreateRoutineStmt;
      TCreateRoutineStmt = packed record
      private type
        TNodes = record
          CreateTag: TOffset;
          DefinerNode: TOffset;
          RoutineToken: TOffset;
          IdentNode: TOffset;
          OpenBracketToken: TOffset;
          ParameterNode: TOffset;
          CloseBracketToken: TOffset;
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
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateTriggerStmt = ^TCreateTriggerStmt;
      TCreateTriggerStmt = packed record
      private type
        TNodes = record
          CreateTag: TOffset;
          DefinerNode: TOffset;
          TriggerTag: TOffset;
          IdentNode: TOffset;
          ActionValue: TOffset;
          OnTag: TOffset;
          TableIdentNode: TOffset;
          ForEachRowTag: TOffset;
          Body: TOffset;
        end;
      private
        Heritage: TStmt;
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateViewStmt = ^TCreateViewStmt;
      TCreateViewStmt = packed record
      private type
        TNodes = record
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
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDataType = ^TDataType;
      TDataType = packed record
      private type
        TNodes = record
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
        TNodes = record
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

      PFunction = ^TFunction;
      TFunction = packed record
      private type
        TNodes = record
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
          TNodes = record
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

        TNodes = record
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

      PIterateStmt = ^TIterateStmt;
      TIterateStmt = packed record
      private type
        TNodes = record
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
        TNodes = record
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

      // PList = ^TList; defined after TList definition, otherwise its addressed Classes.TList
      TList = packed record
      private type
        TNodes = record
          OpenBracket: TOffset;
          FirstChild: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes; const AChildrenCount: Integer; const AChildrens: array of TOffset): TOffset; static;
        function GetFirstChild(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property FirstChild: PNode read GetFirstChild;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;
      PList = ^TList;

      PLoopStmt = ^TLoopStmt;
      TLoopStmt = packed record
      private type
        TNodes = record
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

      PRepeatStmt = ^TRepeatStmt;
      TRepeatStmt = packed record
      private type
        TNodes = record
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
        TNodes = record
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
      private
        Heritage: TStmt;
      public
        type
          PColumn = ^TColumn;
          TColumn = packed record
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

          PTable = ^TTable;
          TTable = packed record
          public type

            PIndexHint = ^TIndexHint;
            TIndexHint = packed record
            public type
              TIndexHintKind = (ihkUnknown, ihkJoin, ihkOrderBy, ihkGroupBy);
              TIndexHintType = (ihtUnknown, ihtUse, ihtIgnore, ihtForce);
            private
              Heritage: TRange;
              FIndexHintType: TIndexHintType;
              FIndexHintKind: TIndexHintKind;
              class function Create(const AParser: TCustomSQLParser; const AIndexHintType: TIndexHintType; const AIndexHintKind: TIndexHintKind): TOffset; static;
            public
              property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
            end;

          private
            Heritage: TRange;
            FAlias: TOffset;
            FAsToken: TOffset;
            FIdent: TOffset;
            FIndexHints: TOffset;
            FPartitionToken: TOffset;
            FPartitions: TOffset;
            class function Create(const AParser: TCustomSQLParser; const AIdent, AAsToken, AAlias: TOffset; const AIndexHints: TOffset = 0; const APartitionToken: TOffset = 0; const APartitions: TOffset = 0): TOffset; static;
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

          POrder = ^TOrder;
          TOrder = packed record
          private
            Heritage: TRange;
          private
            class function Create(const AParser: TCustomSQLParser; const AExpr, ADirection: TOffset): TOffset; static;
          public
            property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
          end;

      private type
        TNodes = record
          SelectTag: TOffset;
          DistinctToken: TOffset;
          HighPriorityToken: TOffset;
          StraightJoinToken: TOffset;
          SQLSmallResultToken: TOffset;
          SQLBigResultToken: TOffset;
          SQLBufferResultToken: TOffset;
          SQLNoCacheToken: TOffset;
          SQLCalcFoundRowsToken: TOffset;
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
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSubArea = ^TSubArea;
      TSubArea = packed record
      private type
        TNodes = record
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

      PSoundsLikeOp = ^TSoundsLikeOp;
      TSoundsLikeOp = packed record
      private type
        TNodes = record
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

      PTag = ^TTag;
      TTag = packed record
      private type
        TNodes = record
          KeywordToken1: TOffset;
          KeywordToken2: TOffset;
          KeywordToken3: TOffset;
          KeywordToken4: TOffset;
          KeywordToken5: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;


      PUnaryOp = ^TUnaryOp;
      TUnaryOp = packed record
      private type
        TNodes = record
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

      PUser = ^TUser;
      TUser = packed record
      private type
        TNodes = record
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
        TNodes = record
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
        TNodes = record
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

  private
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

  protected
    kiAFTER,
    kiALL,
    kiAND,
    kiALGORITHM,
    kiAS,
    kiASC,
    kiBEFORE,
    kiBINARY,
    kiBEGIN,
    kiBETWEEN,
    kiBY,
    kiCASCADED,
    kiCASE,
    kiCHARACTER,
    kiCHECK,
    kiCOLLATE,
    kiCOMMENT,
    kiCONTAINS,
    kiCREATE,
    kiCROSS,
    kiCURRENT_USER,
    kiDATA,
    kiDEFINER,
    kiDELETE,
    kiDESC,
    kiDETERMINISTIC,
    kiDISTINCT,
    kiDISTINCTROW,
    kiDIV,
    kiDO,
    kiEACH,
    kiELSE,
    kiELSEIF,
    kiEND,
    kiFROM,
    kiFOR,
    kiFORCE,
    kiFUNCTION,
    kiGROUP,
    kiHAVING,
    kiHIGH_PRIORITY,
    kiIGNORE,
    kiIF,
    kiIN,
    kiINDEX,
    kiINNER,
    kiINOUT,
    kiINSERT,
    kiINTERVAL,
    kiINVOKER,
    kiIS,
    kiITERATE,
    kiJOIN,
    kiKEY,
    kiLANGUAGE,
    kiLEAVE,
    kiLEFT,
    kiLIKE,
    kiLIMIT,
    kiLOCAL,
    kiLOOP,
    kiMERGE,
    kiMOD,
    kiMODIFIES,
    kiNATURAL,
    kiNO,
    kiNOT,
    kiNULL,
    kiOFFSET,
    kiOJ,
    kiON,
    kiOPTION,
    kiOR,
    kiORDER,
    kiOUT,
    kiOUTER,
    kiPARTITION,
    kiPROCEDURE,
    kiREADS,
    kiREGEXP,
    kiREPEAT,
    kiREPLACE,
    kiRETURNS,
    kiRIGHT,
    kiRLIKE,
    kiROLLUP,
    kiROW,
    kiSECURITY,
    kiSELECT,
    kiSET,
    kiSOUNDS,
    kiSQL,
    kiSQL_BIG_RESULT,
    kiSQL_BUFFER_RESULT,
    kiSQL_CACHE,
    kiSQL_CALC_FOUND_ROWS,
    kiSQL_NO_CACHE,
    kiSQL_SMALL_RESULT,
    kiSTRAIGHT_JOIN,
    kiTEMPTABLE,
    kiTHEN,
    kiTRIGGER,
    kiUNDEFINED,
    kiUNSIGNED,
    kiUNTIL,
    kiUPDATE,
    kiUSE,
    kiUSING,
    kiVIEW,
    kiWHEN,
    kiWITH,
    kiWHERE,
    kiWHILE,
    kiXOR,
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
    function ParseCaseOp(): TOffset;
    function ParseCaseOpBranch(): TOffset;
    function ParseCaseStmt(): TOffset;
    function ParseCaseStmtBranch(): TOffset;
    function ParseColumn(): TOffset;
    function ParseCompoundStmt(): TOffset;
    function ParseColumnIdent(): TOffset;
    function ParseCreateRoutineStmt(const ARoutineType: TRoutineType): TOffset;
    function ParseCreateStmt(): TOffset;
    function ParseCreateTriggerStmt(): TOffset;
    function ParseCreateViewStmt(): TOffset;
    function ParseDataType(): TOffset;
    function ParseDbIdent(const ADbIdentType: TDbIdentType): TOffset;
    function ParseDefinerValue(): TOffset;
    function ParseExpr(): TOffset;
    function ParseFunction(): TOffset;
    function ParseFunctionParam(): TOffset;
    function ParseGroup(): TOffset;
    function ParseIfStmt(): TOffset;
    function ParseIfStmtBranch(): TOffset;
    function ParseIndexHint(): TOffset;
    function ParseIndexIdent(): TOffset;
    function ParseIterateStmt(): TOffset;
    function ParseKeyword(): TOffset;
    function ParseLeaveStmt(): TOffset;
    function ParseList(const Brackets: Boolean; const ParseNode: TParseFunction): TOffset; overload;
    function ParseList(const Brackets: Boolean; const ParseNode: TParseFunction; const DelimterType: TTokenType): TOffset; overload;
    function ParseLoopStmt(): TOffset;
    function ParseTag(const KeywordIndex1: TWordList.TIndex; const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1; const KeywordIndex4: TWordList.TIndex = -1; const KeywordIndex5: TWordList.TIndex = -1): TOffset;
    function ParseOrder(): TOffset;
    function ParsePartitionIdent(): TOffset;
    function ParsePL_SQLStmt(): TOffset;
    function ParseProcedureParam(): TOffset;
    function ParseRepeatStmt(): TOffset;
    function ParseSelectStmt(): TOffset;
    function ParseString(): TOffset;
    function ParseSubArea(const ParseNode: TParseFunction): TOffset;
    function ParseStmt(const PL_SQL: Boolean = False): TOffset;
    function ParseTableReference(): TOffset;
    function ParseToken(): TOffset;
    function ParseUnknownStmt(): TOffset;
    function ParseUser(): TOffset;
    function ParseValue(const IdentTag: TOffset; const Assign: TValueAssign; const AParseValue: TParseFunction): TOffset; overload;
    function ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const AParseValue: TParseFunction): TOffset; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseWhileStmt(): TOffset;
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

function TCustomSQLParser.TWordList.GetWord(Index: TIndex): string;
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

function TCustomSQLParser.TChild.GetNextSibling(): PNode;
var
  Node: PNode;
  Token: PToken;
begin
  Assert(Parser.IsChild(@Self));

  Token := PChild(@Self)^.LastToken^.NextToken;

  if (Assigned(Token) and (Token^.TokenType = ttComma)) then
    Token := PToken(Token)^.NextToken; // ttComma

  Node := PNode(Token);

  Result := nil;
  while (Assigned(Node) and (not Parser.IsToken(Node) or (PToken(Node)^.TokenType <> ttComma)) and Parser.IsChild(Node) and (PChild(Node) <> PChild(ParentNode))) do
  begin
    Result := Node;
    Node := PChild(Node)^.ParentNode;
  end;

  if (Assigned(Result) and (PChild(Token)^.FParentNode <> PChild(Node)^.FParentNode)) then
    Result := nil;
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
      ditField,
      ditAllFields: Result := ditTable;
      else raise ERangeError.Create(SArgumentOutOfRange);
    end
  else if (@Self = PDbIdent(Parser.NodePtr(FParentNode))^.Prefix2) then
    case (PDbIdent(Parser.NodePtr(FParentNode))^.DbIdentType) of
      ditUnknown: Result := ditUnknown;
      ditField,
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
    if ((FFirstToken = 0) or (FFirstToken > Child^.FFirstToken)) then
      FFirstToken := Child^.FFirstToken;
    if ((FLastToken = 0) or (FLastToken < Child^.FLastToken)) then
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
    Heritage.Heritage.AddChild(ANodes.RoutineToken);
    Heritage.Heritage.AddChild(ANodes.IdentNode);
    Heritage.Heritage.AddChild(ANodes.OpenBracketToken);
    Heritage.Heritage.AddChild(ANodes.ParameterNode);
    Heritage.Heritage.AddChild(ANodes.CloseBracketToken);
    Heritage.Heritage.AddChild(ANodes.Return.ReturnsTag);
    Heritage.Heritage.AddChild(ANodes.Return.DataTypeNode);
    Heritage.Heritage.AddChild(ANodes.Body);
  end;
end;

{ TCustomSQLParser.TCreateTriggerStmt ********************************************}

class function TCustomSQLParser.TCreateTriggerStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateTrigger);

  with PCreateTriggerStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.CreateTag);
    Heritage.Heritage.AddChild(ANodes.DefinerNode);
    Heritage.Heritage.AddChild(ANodes.TriggerTag);
    Heritage.Heritage.AddChild(ANodes.IdentNode);
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
  Result := TRange.Create(AParser, ntIfBranch);

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

{ TCustomSQLParser.TIterateStmt *************************************************}

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

function TCustomSQLParser.TList.GetFirstChild(): PNode;
begin
  Result := Parser.NodePtr(FNodes.FirstChild);
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

class function TCustomSQLParser.TSelectStmt.TColumn.Create(const AParser: TCustomSQLParser; const AValue, AAsToken, AAlias: TOffset): TOffset;
begin
  Result := TRange.Create(AParser, ntColumn);

  with PColumn(AParser.NodePtr(Result))^ do
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

class function TCustomSQLParser.TSelectStmt.TTable.TIndexHint.Create(const AParser: TCustomSQLParser; const AIndexHintType: TIndexHintType; const AIndexHintKind: TIndexHintKind): TOffset;
begin
  Result := TRange.Create(AParser, ntIndexHint);

  with TSelectStmt.TTable.PIndexHint(AParser.NodePtr(Result))^ do
  begin
    FIndexHintType := AIndexHintType;
    FIndexHintKind := AIndexHintKind;
  end;
end;

{ TCustomSQLParser.TSelectStmt.TTable *****************************************}

class function TCustomSQLParser.TSelectStmt.TTable.Create(const AParser: TCustomSQLParser; const AIdent, AAsToken, AAlias: TOffset; const AIndexHints: TOffset = 0; const APartitionToken: TOffset = 0; const APartitions: TOffset = 0): TOffset;
begin
  Result := TRange.Create(AParser, ntTable);

  with TSelectStmt.PTable(AParser.NodePtr(Result))^ do
  begin
    FIdent := AIdent;
    FPartitionToken := APartitionToken;
    FPartitions := APartitions;
    FAsToken := AAsToken;
    FAlias := AAlias;
    FIndexHints := AIndexHints;

    Heritage.AddChild(AIdent);
    Heritage.AddChild(APartitionToken);
    Heritage.AddChild(APartitions);
    Heritage.AddChild(AAsToken);
    Heritage.AddChild(AAlias);
    Heritage.AddChild(AIndexHints);
  end;
end;

{ TCustomSQLParser.TSelectStmt.TJoin ******************************************}

class function TCustomSQLParser.TSelectStmt.TJoin.Create(const AParser: TCustomSQLParser; const ALeftTable: TOffset; const AJoinType: TJoinType; const ARightTable: TOffset; const ACondition: TOffset; const AKeywordTokens: TKeywordTokens): TOffset;
var
  I: Integer;
begin
  Result := TRange.Create(AParser, ntJoin);

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
  Result := TRange.Create(AParser, ntGroup);

  with PGroup(AParser.NodePtr(Result))^ do
  begin
    FExpr := AExpr;
    FDirection := ADirection;

    Heritage.AddChild(AExpr);
    Heritage.AddChild(ADirection);
  end;
end;

{ TCustomSQLParser.TSelectStmt.TOrder *****************************************}

class function TCustomSQLParser.TSelectStmt.TOrder.Create(const AParser: TCustomSQLParser; const AExpr, ADirection: TOffset): TOffset;
begin
  Result := TRange.Create(AParser, ntOrder);

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
    Heritage.Heritage.AddChild(ANodes.DistinctToken);
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

{ TCustomSQLParser.TSubArea ******************************************************}

class function TCustomSQLParser.TSubArea.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSubArea);

  with PSubArea(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.AreaNode);
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
    Heritage.AddChild(ANodes.KeywordToken5);
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
    PE_UnkownStmt: Result := 'First Token is not a known keyword';
    PE_IncompleteStmt: Result := 'Uncompleted Token';
    PE_InvalidEndLabel: Result := 'Begin and End Token are different';
    else Result := '[Unknown Error Message]';
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
    ntDbIdent: Result := SizeOf(TDbIdent);
    ntFunction: Result := SizeOf(TFunction);
    ntUnaryOp: Result := SizeOf(TUnaryOp);
    ntBinaryOp: Result := SizeOf(TBinaryOp);
    ntUser: Result := SizeOf(TUser);
    ntColumn: Result := SizeOf(TSelectStmt.TColumn);
    ntJoin: Result := SizeOf(TSelectStmt.TJoin);
    ntTable: Result := SizeOf(TSelectStmt.TTable);
    ntGroup: Result := SizeOf(TSelectStmt.TGroup);
    ntOrder: Result := SizeOf(TSelectStmt.TOrder);
    ntUnknownStmt: Result := SizeOf(TStmt);
    ntCreateViewStmt: Result := SizeOf(TCreateViewStmt);
    ntCreateRoutineStmt: Result := SizeOf(TCreateRoutineStmt);
    ntCompoundStmt: Result := SizeOf(TCompoundStmt);
    ntIfStmt: Result := SizeOf(TIfStmt);
    ntSelectStmt: Result := SizeOf(TSelectStmt);
    ntTag: Result := SizeOf(TTag);
    ntValue: Result := SizeOf(TValue);
    ntFunctionParam: Result := SizeOf(TRoutineParam);
    ntDataType: Result := SizeOf(TDataType);
    ntList: Result := SizeOf(TList);
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
        ApplyCurrentToken();
      end;
  end;
end;

function TCustomSQLParser.Parse(const Text: string): Boolean;
begin
  Result := Parse(PChar(Text), Length(Text));
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

function TCustomSQLParser.ParseColumn(): TOffset;
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

  Result := TSelectStmt.TColumn.Create(Self, Value, AsToken, Alias);
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

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)) then
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

function TCustomSQLParser.ParseCreateRoutineStmt(const ARoutineType: TRoutineType): TOffset;
var
  Found: Boolean;
  Nodes: TCreateRoutineStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.RoutineToken := ParseTag(kiFUNCTION)
    else
      Nodes.RoutineToken := ParseTag(kiPROCEDURE);

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
  while (not Error and (CurrentToken > 0) and Found) do
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

function TCustomSQLParser.ParseCreateStmt(): TOffset;
var
  Index: Integer;
begin
  Assert(TokenPtr(CurrentToken)^.KeywordIndex = kiCREATE);

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

  if (not Error) then
    if (NextToken[Index] = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiFUNCTION) then
      Result := ParseCreateRoutineStmt(rtFunction)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiPROCEDURE) then
      Result := ParseCreateRoutineStmt(rtProcedure)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiVIEW) then
      Result := ParseCreateViewStmt()
    else
      SetError(PE_UnexpectedToken, NextToken[Index]);
end;

function TCustomSQLParser.ParseCreateTriggerStmt(): TOffset;
var
  Nodes: TCreateTriggerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    Nodes.TriggerTag := ParseTag(kiTRIGGER);

  if (not Error) then
    Nodes.IdentNode := ParseDbIdent(ditTrigger);

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

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
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
  else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
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
          else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
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
          else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
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
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
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

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.OperatorType = otDot) and (ADbIdentType in [ditIndex, ditField, ditAllFields, ditPartition])) then
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
  InCaseOp: Boolean;
  KeywordIndex: TWordList.TIndex;
  OperatorPrecedence: Integer;
begin
  NodeCount := 0; InCaseOp := False;

  repeat
    KeywordIndex := TokenPtr(CurrentToken)^.KeywordIndex;
    if (KeywordIndex > 0) then
      if (KeywordIndex = kiBETWEEN) then
        TokenPtr(CurrentToken)^.FOperatorType := otBetween
      else if (KeywordIndex = kiBINARY) then
        TokenPtr(CurrentToken)^.FOperatorType := otBinary
      else if (KeywordIndex = kiCOLLATE) then
        TokenPtr(CurrentToken)^.FOperatorType := otCollate
      else if (KeywordIndex = kiCASE) then
      begin
        TokenPtr(CurrentToken)^.FOperatorType := otCase;
        InCaseOp := True;
      end
      else if (KeywordIndex = kiEND) then
        InCaseOp := False
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
                  TokenPtr(CurrentToken)^.FTokenType := ttIdent;
                  TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
                  TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
                  AddNode(TDbIdent.Create(Self, CurrentToken, ditAllFields));
                end;
              ttInteger,
              ttNumeric,
              ttString,
              ttDQIdent,
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
              ttIdent,
              ttDBIdent,
              ttBRIdent,
              ttMySQLIdent:
                if ((NextToken[1] = 0) or (TokenPtr(NextToken[1])^.TokenType <> ttOpenBracket)) then
                begin
                  TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
                  AddNode(TDbIdent.Create(Self, CurrentToken, ditField));
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
        or (KeywordIndex = kiWHEN)))
    or (not InCaseOp and (KeywordIndex = kiTHEN)));

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

function TCustomSQLParser.ParseIndexHint(): TOffset;
var
  IndexHintKind: TSelectStmt.TTable.TIndexHint.TIndexHintKind;
  IndexHintType: TSelectStmt.TTable.TIndexHint.TIndexHintType;
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
        Result := TSelectStmt.TTable.TIndexHint.Create(Self, IndexHintType, IndexHintKind);
    end;
  end;
end;

function TCustomSQLParser.ParseIndexIdent(): TOffset;
begin
  Result := ParseDbIdent(ditIndex);
end;

function TCustomSQLParser.ParseTag(const KeywordIndex1: TWordList.TIndex; const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1; const KeywordIndex4: TWordList.TIndex = -1; const KeywordIndex5: TWordList.TIndex = -1): TOffset;
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
              begin
                Nodes.KeywordToken4 := ApplyCurrentToken();

                if (KeywordIndex5 >= 0) then
                begin
                  if (CurrentToken = 0) then
                    SetError(PE_IncompleteStmt)
                  else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex5) then
                    SetError(PE_UnexpectedToken, CurrentToken)
                  else
                    Nodes.KeywordToken5 := ApplyCurrentToken();
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  Result := TTag.Create(Self, Nodes);
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

  Result := TSelectStmt.TOrder.Create(Self, Expr, Direction);
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
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
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
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.LabelToken := ApplyCurrentToken(utLabel);

  Result := TLeaveStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseList(const Brackets: Boolean; const ParseNode: TParseFunction): TOffset;
begin
  Result := ParseList(Brackets, ParseNode, ttComma);
end;

function TCustomSQLParser.ParseList(const Brackets: Boolean; const ParseNode: TParseFunction; const DelimterType: fspTypes.TTokenType): TOffset;
var
  DelimiterFound: Boolean;
  I: Integer;
  Index: Integer;
  Nodes: TList.TNodes;
  Childrens: array[0..100-1] of TOffset;
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

    Nodes.FirstChild := Childrens[0];
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
    Result := TList.Create(Self, Nodes, Index, TIntergerArray(ChildrenList.List));
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

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)) then
    if ((Nodes.BeginLabelToken = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(Nodes.BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.EndLabelToken := ApplyCurrentToken(utLabel, ttEndLabel);

  Result := TLoopStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParsePartitionIdent(): TOffset;
begin
  Result := ParseDbIdent(ditPartition);
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

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)) then
    if ((Nodes.BeginLabelToken = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(Nodes.BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.EndLabelToken := ApplyCurrentToken(utLabel, ttEndLabel);

  Result := TRepeatStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseSelectStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TSelectStmt.TNodes;
begin
  Assert(TokenPtr(CurrentToken)^.KeywordIndex = kiSELECT);

  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.SelectTag := ParseTag(kiSELECT);

  repeat
    Found := True;
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiAll) or (TokenPtr(CurrentToken)^.KeywordIndex = kiDISTINCT) or (TokenPtr(CurrentToken)^.KeywordIndex = kiDISTINCTROW)) then
      Nodes.DistinctToken := ApplyCurrentToken()
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiHIGH_PRIORITY) then
      Nodes.HighPriorityToken := ApplyCurrentToken()
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSTRAIGHT_JOIN) then
      Nodes.StraightJoinToken := ApplyCurrentToken()
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_SMALL_RESULT) then
      Nodes.SQLSmallResultToken := ApplyCurrentToken()
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_BIG_RESULT) then
      Nodes.SQLBigResultToken := ApplyCurrentToken()
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_BUFFER_RESULT) then
      Nodes.SQLBufferResultToken := ApplyCurrentToken()
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_CACHE) or (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_NO_CACHE)) then
      Nodes.SQLNoCacheToken := ApplyCurrentToken()
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_CALC_FOUND_ROWS) then
      Nodes.SQLCalcFoundRowsToken := ApplyCurrentToken()
    else
      Found := False;
  until (not Found);

  Nodes.ColumnsNode := ParseList(False, ParseColumn);

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

function TCustomSQLParser.ParseString(): TOffset;
begin
  Result := 0;
  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttStrings)) then
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

function TCustomSQLParser.ParseStmt(const PL_SQL: Boolean = False): TOffset;
var
  FirstToken: TOffset;
  KeywordIndex: TWordList.TIndex;
  KeywordToken: TOffset;
  LabelToken: TOffset;
  Stmt: PStmt;
  Token: PToken;
begin
  FirstToken := CurrentToken;
  KeywordToken := CurrentToken;
  if ((CurrentToken = 0) or (TokenPtr(CurrentToken)^.TokenType <> ttBeginLabel)) then
    LabelToken := 0
  else
  begin
    LabelToken := CurrentToken;
    KeywordToken := NextToken[1];
  end;

  if (KeywordToken = 0) then
  begin
    SetError(PE_IncompleteStmt);
    Result := 0;
  end
  else
  begin
    KeywordIndex := TokenPtr(KeywordToken)^.KeywordIndex;

    if (PL_SQL and (KeywordIndex = kiBEGIN)) then
      Result := ParseCompoundStmt()
    else if (LabelToken > 0) then
      Result := ParseUnknownStmt()
    else if (KeywordIndex = kiCREATE) then
      Result := ParseCreateStmt()
    else if (PL_SQL and (KeywordIndex = kiIF)) then
      Result := ParseIfStmt()
    else if (PL_SQL and (KeywordIndex = kiLOOP)) then
      Result := ParseLoopStmt()
    else if (PL_SQL and (KeywordIndex = kiREPEAT)) then
      Result := ParseRepeatStmt()
    else if (KeywordIndex = kiSELECT) then
      Result := ParseSelectStmt()
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

function TCustomSQLParser.ParseTableReference(): TOffset;

  function ParseTableFactor(): TOffset;
  var
    Alias: TOffset;
    AsToken: TOffset;
    IndexHints: TOffset;
    OJToken: TOffset;
    OpenBracketToken: TOffset;
    Partition: PNode;
    PartitionToken: TOffset;
    Partitions: TOffset;
    Prefix: TOffset;
  begin
    if (CurrentToken = 0) then
    begin
      SetError(PE_IncompleteStmt);
      Result := 0;
    end
    else if (TokenPtr(CurrentToken)^.TokenType in ttIdents) then
    begin
      Result := TDbIdent.Create(Self, ApplyCurrentToken(), ditTable);

      if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.OperatorType = otDot)) then
      begin
        if (NextToken[1] = 0) then
          SetError(PE_IncompleteStmt)
        else if (PDbIdent(NodePtr(Result))^.FNodes.Prefix2 > 0) then
          SetError(PE_UnexpectedToken, CurrentToken)
        else if (not (TokenPtr(NextToken[1])^.TokenType in ttIdents)) then
          SetError(PE_UnexpectedToken, NextToken[1])
        else
        begin
          Prefix := Result;
          Result := TDbIdent.Create(Self, NextToken[1], ditTable);
          PDbIdent(NodePtr(Result))^.AddPrefix(Prefix, CurrentToken);
        end;
        ApplyCurrentToken();
        ApplyCurrentToken();
      end;

      PartitionToken := 0;
      Partitions := 0;
      if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
      begin
        PartitionToken := ApplyCurrentToken();

        if (CurrentToken = 0) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
          SetError(PE_UnexpectedToken, CurrentToken)
        else
        begin
          Partitions := ParseList(True, ParsePartitionIdent);
          Partition := PList(NodePtr(Partitions))^.FirstChild;
          while (Assigned(Partition)) do
          begin
            if (Partition^.NodeType <> ntDbIdent) then
              SetError(PE_UnexpectedToken, PChild(Partition)^.FFirstToken)
            else if (PDbIdent(Partition)^.FNodes.Prefix1 > 0) then
              SetError(PE_UnexpectedToken, PDbIdent(Partition)^.Ident^.NextToken^.Offset)
            else
              PDbIdent(Partition)^.FDbIdentType := ditPartition;
            Partition := PChild(Partition)^.NextSibling;
          end;
        end;
      end;

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

      if (Error or (CurrentToken = 0) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiUSE) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiIGNORE) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiFORCE)) then
        IndexHints := 0
      else
        IndexHints := ParseList(False, ParseIndexHint);

      Result := TSelectStmt.TTable.Create(Self, Result, AsToken, Alias, IndexHints, PartitionToken, Partitions);
    end
    else if (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket) then
    begin
      if ((NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT)) then
        Result := ParseSubArea(ParseSelectStmt)
      else
        Result := ParseList(True, ParseTableReference);

      if (not Error) then
        if (NodePtr(Result)^.NodeType = ntSelectStmt) then
        begin
          AsToken := 0;
          if (CurrentToken = 0) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiAs) then
            SetError(PE_UnexpectedToken, CurrentToken)
          else
            AsToken := ApplyCurrentToken();

          Alias := 0;
          if (not Error) then
            if (CurrentToken = 0) then
              SetError(PE_IncompleteStmt)
            else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
              SetError(PE_UnexpectedToken, CurrentToken)
            else
              Alias := ApplyCurrentToken(utAlias);

          Result := TSelectStmt.TTable.Create(Self, Result, AsToken, Alias);
        end;
    end
    else if (TokenPtr(CurrentToken)^.TokenType = ttOpenCurlyBracket) then
    begin
      OpenBracketToken := ApplyCurrentToken();

      OJToken := 0;
      if (CurrentToken = 0) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiOJ) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
        OJToken := ApplyCurrentToken();

      Result := ParseTableReference();

      if (not Error) then
        if (CurrentToken = 0) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseCurlyBracket) then
          SetError(PE_UnexpectedToken, CurrentToken)
        else
        begin
          if (not IsRange(NodePtr(Result))) then
            raise ERangeError.Create(SArgumentOutOfRange);
          RangeNodePtr(Result)^.AddChild(OpenBracketToken);
          RangeNodePtr(Result)^.AddChild(OJToken);
          RangeNodePtr(Result)^.AddChild(CurrentToken);
          ApplyCurrentToken();
        end;
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

    Variable:
      MOV TokenType,ttVariable
      JMP UnquotedIdent

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
    if (Self is TMySQLSQLParser) then
    begin
      if (TMySQLSQLParser(Self).AnsiQuotes and (TokenType = ttMySQLIdent)
        or not TMySQLSQLParser(Self).AnsiQuotes and (TokenType = ttDQIdent)) then
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

    if (TokenType <> ttIdent) then
      KeywordIndex := -1
    else
    begin
      KeywordIndex := FKeywords.IndexOf(SQL, TokenLength);
      if (KeywordIndex >= 0) then
        OperatorType := OperatorTypeByKeywordIndex[KeywordIndex];
    end;

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

function TCustomSQLParser.ParseUnknownStmt(): TOffset;
var
  Token: TOffset;
begin
  Result := TStmt.Create(Self, stUnknown);

  Token := CurrentToken;
  if (Token > 0) then
    repeat
      Token := CurrentToken; ApplyCurrentToken();
    until ((Token = 0) or (TokenPtr(Token)^.TokenType = ttDelimiter));

  SetError(PE_UnkownStmt, CurrentToken);

  if (TokenPtr(Root^.FLastToken)^.TokenType = ttDelimiter) then
    StmtPtr(Result)^.FLastToken := TokenPtr(Root^.FLastToken)^.FPriorToken
  else
    StmtPtr(Result)^.FLastToken := Root^.FLastToken;
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
  else if (not (TokenPtr(CurrentToken)^.TokenType in [ttIdent, ttString])) then
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
      else if (not (TokenPtr(CurrentToken)^.TokenType in [ttIdent, ttString])) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
        Nodes.HostToken := ApplyCurrentToken();
    end;

    if (not Error) then
      Result := TUser.Create(Self, Nodes);
  end;
end;

function TCustomSQLParser.ParseValue(const IdentTag: TOffset; const Assign: TValueAssign; const AParseValue: TParseFunction): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := IdentTag;

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
    Nodes.ValueNode := AParseValue();

  Result := TValue.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const AParseValue: TParseFunction): TOffset;
begin
  Result := ParseValue(ParseTag(KeywordIndex), Assign, AParseValue);
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

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)) then
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

    if (not Stmt^.Error) then
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
            if (Assigned(PNode(PChild(Node)^.NextSibling))) then
              HTML := HTML
                + '<tr><td>NextSibling:</td><td>&nbsp;</td><td>' + IntToStr((PNode(PChild(Node)^.NextSibling)^.Offset)) + '</td></tr>';
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
      if (not Stmt^.Error and Assigned(PChild(Token)^.ParentNode)) then
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
        + '<tr class=""><td colspan="' + IntToStr(Stmt^.ErrorToken^.Index) + '"></td>'
        + '<td class="StmtError"><a href="">&uarr;'
        + '<span><table cellspacing="2" cellpadding="0">'
        + '<tr><td>ErrorCode:</td><td>' + IntToStr(Stmt.ErrorCode) + '</td></tr>'
        + '<tr><td>ErrorMessage:</td><td>' + HTMLEscape(Stmt.ErrorMessage) + '</td></tr>'
        + '</table></span>'
        + '</a></td>'
        + '<td colspan="' + IntToStr(Stmt^.LastToken.Index - Stmt^.ErrorToken^.Index) + '"></td>'
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
    kiAFTER               := IndexOf('AFTER');
    kiALL                 := IndexOf('ALL');
    kiAND                 := IndexOf('AND');
    kiAS                  := IndexOf('AS');
    kiALGORITHM           := IndexOf('ALGORITHM');
    kiASC                 := IndexOf('ASC');
    kiBEFORE              := IndexOf('BEFORE');
    kiBEGIN               := IndexOf('BEGIN');
    kiBETWEEN             := IndexOf('BETWEEN');
    kiBINARY              := IndexOf('BINARY');
    kiBY                  := IndexOf('BY');
    kiCASCADED            := IndexOf('CASCADED');
    kiCASE                := IndexOf('CASE');
    kiCHECK               := IndexOf('CHECK');
    kiCHARACTER           := IndexOf('CHARACTER');
    kiCOLLATE             := IndexOf('COLLATE');
    kiCOMMENT             := IndexOf('COMMENT');
    kiCREATE              := IndexOf('CREATE');
    kiCONTAINS            := IndexOf('CONTAINS');
    kiCROSS               := IndexOf('CROSS');
    kiCURRENT_USER        := IndexOf('CURRENT_USER');
    kiDATA                := IndexOf('DATA');
    kiDEFINER             := IndexOf('DEFINER');
    kiDELETE              := IndexOf('DELETE');
    kiDESC                := IndexOf('DESC');
    kiDETERMINISTIC       := IndexOf('DETERMINISTIC');
    kiDISTINCT            := IndexOf('DISTINCT');
    kiDISTINCTROW         := IndexOf('DISTINCTROW');
    kiDIV                 := IndexOf('DIV');
    kiDO                  := IndexOf('DO');
    kiEACH                := IndexOf('EACH');
    kiELSE                := IndexOf('ELSE');
    kiELSEIF              := IndexOf('ELSEIF');
    kiEND                 := IndexOf('END');
    kiFORCE               := IndexOf('FORCE');
    kiFUNCTION            := IndexOf('FUNCTION');
    kiFROM                := IndexOf('FROM');
    kiFOR                 := IndexOf('FOR');
    kiGROUP               := IndexOf('GROUP');
    kiHAVING              := IndexOf('HAVING');
    kiHIGH_PRIORITY       := IndexOf('HIGH_PRIORITY');
    kiIGNORE              := IndexOf('IGNORE');
    kiIF                  := IndexOf('IF');
    kiIN                  := IndexOf('IN');
    kiINDEX               := IndexOf('INDEX');
    kiINNER               := IndexOf('INNER');
    kiINOUT               := IndexOf('INOUT');
    kiINSERT              := IndexOf('INSERT');
    kiINTERVAL            := IndexOf('INTERVAL');
    kiINVOKER             := IndexOf('INVOKER');
    kiIS                  := IndexOf('IS');
    kiITERATE             := IndexOf('ITERATE');
    kiJOIN                := IndexOf('JOIN');
    kiKEY                 := IndexOf('KEY');
    kiLANGUAGE            := IndexOf('LANGUAGE');
    kiLEAVE               := IndexOf('LEAVE');
    kiLEFT                := IndexOf('LEFT');
    kiLIKE                := IndexOf('LIKE');
    kiLIMIT               := IndexOf('LIMIT');
    kiLOCAL               := IndexOf('LOCAL');
    kiLOOP                := IndexOf('LOOP');
    kiMERGE               := IndexOf('MERGE');
    kiMOD                 := IndexOf('MOD');
    kiMODIFIES            := IndexOf('MODIFIES');
    kiNATURAL             := IndexOf('NATURAL');
    kiNO                  := IndexOf('NO');
    kiNOT                 := IndexOf('NOT');
    kiNULL                := IndexOf('NULL');
    kiOFFSET              := IndexOf('OFFSET');
    kiOJ                  := IndexOf('OJ');
    kiON                  := IndexOf('ON');
    kiOPTION              := IndexOf('OPTION');
    kiOR                  := IndexOf('OR');
    kiORDER               := IndexOf('ORDER');
    kiOUT                 := IndexOf('OUT');
    kiOUTER               := IndexOf('OUTER');
    kiPARTITION           := IndexOf('PARTITION');
    kiPROCEDURE           := IndexOf('PROCEDURE');
    kiREGEXP              := IndexOf('REGEXP');
    kiREADS               := IndexOf('READS');
    kiREPEAT              := IndexOf('REPEAT');
    kiREPLACE             := IndexOf('REPLACE');
    kiRETURNS             := IndexOf('RETURNS');
    kiRIGHT               := IndexOf('RIGHT');
    kiRLIKE               := IndexOf('RLIKE');
    kiROLLUP              := IndexOf('ROLLUP');
    kiROW                 := IndexOf('ROW');
    kiSECURITY            := IndexOf('SECURITY');
    kiSELECT              := IndexOf('SELECT');
    kiSET                 := IndexOf('SET');
    kiSOUNDS              := IndexOf('SOUNDS');
    kiSQL                 := IndexOf('SQL');
    kiSQL_BIG_RESULT      := IndexOf('SQL_BIG_RESULT');
    kiSQL_BUFFER_RESULT   := IndexOf('SQL_BUFFER_RESULT');
    kiSQL_CACHE           := IndexOf('SQL_CACHE');
    kiSQL_CALC_FOUND_ROWS := IndexOf('SQL_CALC_FOUND_ROWS');
    kiSQL_NO_CACHE        := IndexOf('SQL_NO_CACHE');
    kiSQL_SMALL_RESULT    := IndexOf('SQL_SMALL_RESULT');
    kiSTRAIGHT_JOIN       := IndexOf('STRAIGHT_JOIN');
    kiTEMPTABLE           := IndexOf('TEMPTABLE');
    kiTHEN                := IndexOf('THEN');
    kiTRIGGER             := IndexOf('TRIGGER');
    kiWHEN                := IndexOf('WHEN');
    kiWITH                := IndexOf('WITH');
    kiWHERE               := IndexOf('WHERE');
    kiUNDEFINED           := IndexOf('UNDEFINED');
    kiUNSIGNED            := IndexOf('UNSIGNED');
    kiUNTIL               := IndexOf('UNTIL');
    kiUSE                 := IndexOf('USE');
    kiUSING               := IndexOf('USING');
    kiUPDATE              := IndexOf('UPDATE');
    kiVIEW                := IndexOf('VIEW');
    kiWHILE               := IndexOf('WHILE');
    kiXOR                 := IndexOf('XOR');
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
