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
      TSQLDialect = (sdStandard, sdMySQL);
  private
    type
      TRoutineType = (rtFunction, rtProcedure);
      TValueAssign = (vaYes, vaNo, vaAuto);
  protected
    type
      TNodeOffset = Integer;
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
      PDeletedNode = ^TDeletedNode;
      PToken = ^TToken;
      PRangeNode = ^TRange;
      PRoot = ^TRoot;
      PStmtNode = ^TChild;
      PSiblings = ^TSiblings;
      PStmt = ^TStmt;
      PDbIdentifier = ^TDbIdentifier;

      TParseFunction = function(): TNodeOffset of object;

      TNode = packed record
      private
        FNodeType: TNodeType;
        FParser: TCustomSQLParser;
      private
        class function Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): TNodeOffset; static;
        function GetOffset(): TNodeOffset; {$IFNDEF Debug} inline; {$ENDIF}
        property Offset: Integer read GetOffset;
      public
        property NodeType: TNodeType read FNodeType;
        property Parser: TCustomSQLParser read FParser;
      end;

      TRoot = packed record
      private
        Heritage: TNode;
        FFirstToken: TNodeOffset;
        FLastToken: TNodeOffset;
      private
        FFirstStmt: TNodeOffset;
        FLastStmt: TNodeOffset;
        class function Create(const AParser: TCustomSQLParser): TNodeOffset; static;
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

      TChild = packed record  // Every node, except TRootNode
      private
        Heritage: TNode;
        FParentNode: TNodeOffset;
      private
        function GetFFirstToken(): TNodeOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFLastToken(): TNodeOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetNextSibling(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        property FFirstToken: TNodeOffset read GetFFirstToken;
        property FLastToken: TNodeOffset read GetFLastToken;
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
        FPriorToken: TNodeOffset;
        FText: packed record
          SQL: PChar;
          Length: Integer;
        end;
        FTokenType: TTokenType;
        FUsageType: TUsageType;
        class function Create(const AParser: TCustomSQLParser;
          const ASQL: PChar; const ALength: Integer; const AOrigin: TOrigin;
          const AErrorCode: Integer; const AMySQLVersion: Integer; const ATokenType: TTokenType;
          const AOperatorType: TOperatorType; const AKeywordIndex: TWordList.TIndex; const AUsageType: TUsageType): TNodeOffset; static;
        function GetAsString(): string;
        function GetDbIdentifierType(): TDbIdentifierType;
        function GetErrorMessage(): string;
        function GetGeneration(): Integer;
        function GetIndex(): Integer;
        function GetIsUsed(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
        function GetNextToken(): PToken;
        function GetOffset(): TNodeOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetText(): string;
        procedure SetText(AText: string);
        property FParentNode: TNodeOffset read Heritage.FParentNode write Heritage.FParentNode;
        property Generation: Integer read GetGeneration;
        property Index: Integer read GetIndex;
        property Offset: Integer read GetOffset;
      public
        property AsString: string read GetAsString;
        property DbIdentifierType: TDbIdentifierType read GetDbIdentifierType;
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

      TRange = packed record
      private
        Heritage: TChild;
        FFirstToken: TNodeOffset;
        FLastToken: TNodeOffset;
        property FParentNode: TNodeOffset read Heritage.FParentNode write Heritage.FParentNode;
      private
        class function Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): TNodeOffset; static;
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOffset(): TNodeOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        procedure AddChild(const ChildNode: TNodeOffset);
        property Offset: TNodeOffset read GetOffset;
      public
        property FirstToken: PToken read GetFirstToken;
        property LastToken: PToken read GetLastToken;
        property NodeType: TNodeType read Heritage.Heritage.FNodeType;
        property Parser: TCustomSQLParser read Heritage.Heritage.FParser;
        property ParentNode: PNode read GetParentNode;
      end;

      TDeletedNode = packed record
      private
        Heritage: TRange;
        property FNodeType: TNodeType read Heritage.Heritage.Heritage.FNodeType write Heritage.Heritage.Heritage.FNodeType;
      private
        FNodeSize: Integer;
      end;

      TSiblings = packed record
      private
        Heritage: TRange;
        FFirstSibling: TNodeOffset;
      private
        procedure AddSibling(const ASibling: TNodeOffset);
        class function Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): TNodeOffset; static;
        function GetFirstChild(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property FirstChild: PNode read GetFirstChild;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      TStmt = packed record
      private
        Heritage: TRange;
        FStmtType: TStmtType;
        FErrorCode: Integer;
        FErrorToken: TNodeOffset;
        property FFirstToken: TNodeOffset read Heritage.FFirstToken write Heritage.FFirstToken;
        property FLastToken: TNodeOffset read Heritage.FLastToken write Heritage.FLastToken;
        property FParentNode: TNodeOffset read Heritage.Heritage.FParentNode write Heritage.Heritage.FParentNode;
      private
        class function Create(const AParser: TCustomSQLParser; const AStmtType: TStmtType): TNodeOffset; static;
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


      TDbIdentifier = packed record
      private
        Heritage: TRange;
        FPrefix1: TNodeOffset;
        FPrefix2: TNodeOffset;
        FDbIdentifierType: TDbIdentifierType;
        FIdentifier: TNodeOffset;
        procedure AddPrefix(const APrefix, ADot: TNodeOffset);
        class function Create(const AParser: TCustomSQLParser; const AIdentifier: TNodeOffset; const ADbIdentifierType: TDbIdentifierType = ditUnknown): TNodeOffset; static;
        function GetIdentifier(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetPrefix1(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetPrefix2(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property DbIdentifierType: TDbIdentifierType read FDbIdentifierType;
        property Identifier: PToken read GetIdentifier;
        property LastToken: PToken read GetLastToken;
        property ParentNode: PNode read GetParentNode;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        property Prefix1: PToken read GetPrefix1;
        property Prefix2: PToken read GetPrefix2;
      end;

      PFunction = ^TFunction;
      TFunction = packed record
      private
        Heritage: TRange;
      private
        FArguments: TNodeOffset;
        FIdentifier: TNodeOffset;
        class function Create(const AParser: TCustomSQLParser; const AIdentifier, AArguments: TNodeOffset): TNodeOffset; static;
        function GetArguments(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetIdentifier(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Arguments: PStmtNode read GetArguments;
        property Identifier: PStmtNode read GetIdentifier;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PUnaryOp = ^TUnaryOp;
      TUnaryOp = packed record
      private
        Heritage: TRange;
      private
        FOperand: TNodeOffset;
        FOperator: TNodeOffset;
        class function Create(const AParser: TCustomSQLParser; const AOperator, AOperand: TNodeOffset): TNodeOffset; static;
        function GetOperand(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Operand: PStmtNode read GetOperand;
        property Operator: PStmtNode read GetOperator;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PBinaryOp = ^TBinaryOp;
      TBinaryOp = packed record
      private
        Heritage: TRange;
      private
        FOperand1: TNodeOffset;
        FOperand2: TNodeOffset;
        FOperator: TNodeOffset;
        class function Create(const AParser: TCustomSQLParser; const AOperator, AOperand1, AOperand2: TNodeOffset): TNodeOffset; static;
        function GetOperand1(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperand2(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Operand1: PStmtNode read GetOperand1;
        property Operand2: PStmtNode read GetOperand2;
        property Operator: PStmtNode read GetOperator;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PUser = ^TUser;
      TUser = packed record
      private type
        TNodes = record
          NameToken: TNodeOffset;
          AtToken: TNodeOffset;
          HostToken: TNodeOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PBetweenOp = ^TBetweenOp;
      TBetweenOp = packed record
      private
        Heritage: TRange;
      private
        FExpr: TNodeOffset;
        FMax: TNodeOffset;
        FMin: TNodeOffset;
        FOperator1: TNodeOffset;
        FOperator2: TNodeOffset;
        class function Create(const AParser: TCustomSQLParser; const AOperator1, AOperator2: TNodeOffset; const AExpr, AMin, AMax: TNodeOffset): TNodeOffset; static;
        function GetExpr(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetMax(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetMin(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator1(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator2(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Expr: PStmtNode read GetExpr;
        property Max: PStmtNode read GetMax;
        property Min: PStmtNode read GetMin;
        property Operator1: PToken read GetOperator1;
        property Operator2: PToken read GetOperator2;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PCaseCond = ^TCaseCond;
      TCaseCond = packed record
      private
        Heritage: TRange;
      private
        FConditionValue: TNodeOffset;
        FResultValue: TNodeOffset;
        class function Create(const AParser: TCustomSQLParser; const AConditionValue, AResultValue: TNodeOffset): TNodeOffset; static;
        function GetConditionValue(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetNextCond(): PCaseCond; {$IFNDEF Debug} inline; {$ENDIF}
        function GetResultValue(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property ConditionValue: PStmtNode read GetConditionValue;
        property NextCond: PCaseCond read GetNextCond;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        property ResultValue: PStmtNode read GetResultValue;
      end;

      PCaseOp = ^TCaseOp;
      TCaseOp = packed record
      private
        Heritage: TSiblings;
      private
        FElseValue: TNodeOffset;
        FReferenceValue: TNodeOffset;
        class function Create(const AParser: TCustomSQLParser; const AReferenceValue: TNodeOffset): TNodeOffset; static;
        procedure AddCondition(const AConditionValue, AResultValue: TNodeOffset);
        function GetFirstCond(): PCaseCond; {$IFNDEF Debug} inline; {$ENDIF}
        function GetReferenceValue(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        procedure SetElse(const AElseValue: TNodeOffset);
      public
        property FirstCond: PCaseCond read GetFirstCond;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
        property ReferenceValue: PStmtNode read GetReferenceValue;
      end;

      PSoundsLikeOp = ^TSoundsLikeOp;
      TSoundsLikeOp = packed record
      private
        Heritage: TRange;
      private
        FOperand1: TNodeOffset;
        FOperand2: TNodeOffset;
        FOperator1: TNodeOffset;
        FOperator2: TNodeOffset;
        class function Create(const AParser: TCustomSQLParser; const AOperator1, AOperator2: TNodeOffset; const AOperand1, AOperand2: TNodeOffset): TNodeOffset; static;
        function GetOperand1(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperand2(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator1(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator2(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Operand1: PStmtNode read GetOperand1;
        property Operand2: PStmtNode read GetOperand2;
        property Operator1: PToken read GetOperator1;
        property Operator2: PToken read GetOperator2;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PPLSQLCondPart = ^TPLSQLCondPart;
      TPLSQLCondPart = packed record
      private
        Heritage: TRange;
      private
        FExpression: TNodeOffset;
        FOperator: TNodeOffset;
        FThen: TNodeOffset;
      private
        procedure AddStmt(const AStmt: TNodeOffset); {$IFNDEF Debug} inline; {$ENDIF}
        class function Create(const AParser: TCustomSQLParser; const AOperatorToken, AExpression, AThenToken: TNodeOffset): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PCreateRoutineStmt = ^TCreateRoutineStmt;
      TCreateRoutineStmt = packed record
      private type
        TNodes = record
          CreateTag: TNodeOffset;
          DefinerNode: TNodeOffset;
          RoutineToken: TNodeOffset;
          IdentifierNode: TNodeOffset;
          OpenBracketToken: TNodeOffset;
          ParameterNode: TNodeOffset;
          CloseBracketToken: TNodeOffset;
          Return: record
            ReturnsTag: TNodeOffset;
            DataTypeNode: TNodeOffset;
          end;
          CommentValue: TNodeOffset;
          CommentStringNode: TNodeOffset;
          LanguageTag: TNodeOffset;
          DeterministicTag: TNodeOffset;
          CharacteristicTag: TNodeOffset;
          SQLSecurityTag: TNodeOffset;
          Body: TNodeOffset;
        end;
      private
        Heritage: TStmt;
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateTriggerStmt = ^TCreateTriggerStmt;
      TCreateTriggerStmt = packed record
      private type
        TNodes = record
          CreateTag: TNodeOffset;
          DefinerNode: TNodeOffset;
          TriggerTag: TNodeOffset;
          IdentifierNode: TNodeOffset;
          ActionValue: TNodeOffset;
          OnTag: TNodeOffset;
          TableIdentifierNode: TNodeOffset;
          ForEachRowTag: TNodeOffset;
          Body: TNodeOffset;
        end;
      private
        Heritage: TStmt;
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateViewStmt = ^TCreateViewStmt;
      TCreateViewStmt = packed record
      private type
        TNodes = record
          CreateTag: TNodeOffset;
          OrReplaceTag: TNodeOffset;
          AlgorithmValue: TNodeOffset;
          DefinerNode: TNodeOffset;
          SQLSecurityTag: TNodeOffset;
          ViewTag: TNodeOffset;
          IdentifierNode: TNodeOffset;
          Columns: TNodeOffset;
          AsTag: TNodeOffset;
          SelectStmt: TNodeOffset;
          OptionTag: TNodeOffset;
        end;
      private
        Heritage: TStmt;
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
            FAsToken: TNodeOffset;
            FAlias: TNodeOffset;
            FExpression: TNodeOffset;
            class function Create(const AParser: TCustomSQLParser; const AValue, AAsToken, AAlias: TNodeOffset): TNodeOffset; static;
            function GetAlias(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
            function GetAsToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
            function GetDisplayName(): string;
            function GetExpression(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
          public
            property Alias: PToken read GetAlias;
            property AsToken: PToken read GetAsToken;
            property DisplayName: string read GetDisplayName;
            property Expression: PStmtNode read GetExpression;
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
              class function Create(const AParser: TCustomSQLParser; const AIndexHintType: TIndexHintType; const AIndexHintKind: TIndexHintKind): TNodeOffset; static;
              function GetNextIndexHint(): PIndexHint; {$IFNDEF Debug} inline; {$ENDIF}
            public
              property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
              property NextIndexHint: PIndexHint read GetNextIndexHint;
            end;

          private
            Heritage: TRange;
            FAlias: TNodeOffset;
            FAsToken: TNodeOffset;
            FIdentifier: TNodeOffset;
            FIndexHints: TNodeOffset;
            FPartitionToken: TNodeOffset;
            FPartitions: TNodeOffset;
            class function Create(const AParser: TCustomSQLParser; const AIdentifier, AAsToken, AAlias: TNodeOffset; const AIndexHints: TNodeOffset = 0; const APartitionToken: TNodeOffset = 0; const APartitions: TNodeOffset = 0): TNodeOffset; static;
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
            FCondition: TNodeOffset;
            FJoinType: TJoinType;
            FLeftTable: TNodeOffset;
            FRightTable: TNodeOffset;
            class function Create(const AParser: TCustomSQLParser; const ALeftTable: TNodeOffset; const AJoinType: TJoinType; const ARightTable: TNodeOffset; const ACondition: TNodeOffset; const AKeywordTokens: TKeywordTokens): TNodeOffset; static;
          end;

          PGroup = ^TGroup;
          TGroup = packed record
          private
            Heritage: TRange;
          private
            FExpression: TNodeOffset;
            FDirection: TNodeOffset;
            class function Create(const AParser: TCustomSQLParser; const AExpression, ADirection: TNodeOffset): TNodeOffset; static;
            function GetAscending(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
            function GetExpression(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
          public
            property Expression: PStmtNode read GetExpression;
            property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
            property Ascending: Boolean read GetAscending;
          end;

          POrder = ^TOrder;
          TOrder = packed record
          private
            Heritage: TRange;
          private
            FExpression: TNodeOffset;
            FDirection: TNodeOffset;
            class function Create(const AParser: TCustomSQLParser; const AExpression, ADirection: TNodeOffset): TNodeOffset; static;
            function GetAscending(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
            function GetExpression(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
          public
            property Expression: PStmtNode read GetExpression;
            property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
            property Ascending: Boolean read GetAscending;
          end;

      private type
        TNodes = record
          SelectTag: TNodeOffset;
          DistinctToken: TNodeOffset;
          HighPriorityToken: TNodeOffset;
          StraightJoinToken: TNodeOffset;
          SQLSmallResultToken: TNodeOffset;
          SQLBigResultToken: TNodeOffset;
          SQLBufferResultToken: TNodeOffset;
          SQLNoCacheToken: TNodeOffset;
          SQLCalcFoundRowsToken: TNodeOffset;
          ColumnsNode: TNodeOffset;
          FromTag: TNodeOffset;
          TablesNodes: TNodeOffset;
          WhereTag: TNodeOffset;
          WhereNode: TNodeOffset;
          GroupByTag: TNodeOffset;
          GroupsNode: TNodeOffset;
          WithRollupTag: TNodeOffset;
          HavingToken: TNodeOffset;
          HavingNode: TNodeOffset;
          OrderByTag: TNodeOffset;
          OrdersNode: TNodeOffset;
          Limit: record
            LimitToken: TNodeOffset;
            OffsetToken: TNodeOffset;
            OffsetValueToken: TNodeOffset;
            CommaToken: TNodeOffset;
            RowCountValueToken: TNodeOffset;
          end;
        end;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset; static;
        function GetDistinct(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
        function GetHaving(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetWhere(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Distinct: Boolean read GetDistinct;
        property Having: PStmtNode read GetHaving;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
        property Where: PStmtNode read GetWhere;
      end;

      PCompoundStmt = ^TCompoundStmt;
      TCompoundStmt = packed record
      private type
        TNodes = record
          BeginLabelToken: TNodeOffset;
          BeginTag: TNodeOffset;
          StmtList: TNodeOffset;
          EndTag: TNodeOffset;
          EndLabelToken: TNodeOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PLoopStmt = ^TLoopStmt;
      TLoopStmt = packed record
      private type
        TNodes = record
          BeginLabelToken: TNodeOffset;
          BeginTag: TNodeOffset;
          StmtList: TNodeOffset;
          EndTag: TNodeOffset;
          EndLabelToken: TNodeOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PRepeatStmt = ^TRepeatStmt;
      TRepeatStmt = packed record
      private type
        TNodes = record
          BeginLabelToken: TNodeOffset;
          RepeatTag: TNodeOffset;
          StmtList: TNodeOffset;
          UntilTag: TNodeOffset;
          SearchConditionExpression: TNodeOffset;
          EndTag: TNodeOffset;
          EndLabelToken: TNodeOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PWhileStmt = ^TWhileStmt;
      TWhileStmt = packed record
      private type
        TNodes = record
          BeginLabelToken: TNodeOffset;
          WhileTag: TNodeOffset;
          SearchConditionExpression: TNodeOffset;
          DoTag: TNodeOffset;
          StmtList: TNodeOffset;
          EndTag: TNodeOffset;
          EndLabelToken: TNodeOffset;
        end;
      private
        Heritage: TStmt;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PIfStmt = ^TIfStmt;
      TIfStmt = packed record
      private
        Heritage: TStmt;
        procedure AddPart(const APart: TNodeOffset);
        class function Create(const AParser: TCustomSQLParser): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PTag = ^TTag;
      TTag = packed record
      private type
        TNodes = record
          KeywordToken1: TNodeOffset;
          KeywordToken2: TNodeOffset;
          KeywordToken3: TNodeOffset;
          KeywordToken4: TNodeOffset;
          KeywordToken5: TNodeOffset;
        end;
      private
        Heritage: TSiblings;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PValue = ^TValue;
      TValue = packed record
      private type
        TNodes = record
          KeywordToken: TNodeOffset;
          AssignToken: TNodeOffset;
          ValueNode: TNodeOffset;
        end;
      private
        Heritage: TSiblings;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PRoutineParam = ^TRoutineParam;
      TRoutineParam = packed record
      private type
        TNodes = record
          DirektionTag: TNodeOffset;
          IdentifierToken: TNodeOffset;
          DataTypeNode: TNodeOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PDataType = ^TDataType;
      TDataType = packed record
      private type
        TNodes = record
          IdentifierToken: TNodeOffset;
          OpenBracketToken: TNodeOffset;
          CloseBracketToken: TNodeOffset;
          LengthToken: TNodeOffset;
          CommaToken: TNodeOffset;
          DecimalsToken: TNodeOffset;
          StringValuesNode: TNodeOffset;
          UnsignedTag: TNodeOffset;
          ZerofillTag: TNodeOffset;
          CharacterSetTag: TNodeOffset;
          CharacterSetValueToken: TNodeOffset;
          CollateTag: TNodeOffset;
          CollateValueToken: TNodeOffset;
          BinaryTag: TNodeOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSubArea = ^TSubArea;
      TSubArea = packed record
      private type
        TNodes = record
          OpenBracket: TNodeOffset;
          AreaNode: TNodeOffset;
          CloseBracket: TNodeOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PList = ^TList;
      TList = packed record
      private type
        TNodes = record
          OpenBracket: TNodeOffset;
          FirstChild: TNodeOffset;
          CloseBracket: TNodeOffset;
        end;
      private
        Heritage: TRange;
      private
        FNodes: TNodes;
        class function Create(const AParser: TCustomSQLParser; const ANodes: TNodes; const ChildrenCount: Integer; const AChildrens: array of TNodeOffset): TNodeOffset; static;
      public
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

  private
    FErrorCode: Integer;
    FErrorToken: TNodeOffset;
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
    FRoot: TNodeOffset;
    FSQLDialect: TSQLDialect;
    function GetCurrentToken(): TNodeOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function GetErrorMessage(const AErrorCode: Integer): string;
    function GetFunctions(): string; {$IFNDEF Debug} inline; {$ENDIF}
    function GetKeywords(): string; {$IFNDEF Debug} inline; {$ENDIF}
    function GetNextToken(Index: Integer): TNodeOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function GetParsedToken(Index: Integer): TNodeOffset;
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
    kiJOIN,
    kiKEY,
    kiLANGUAGE,
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

    function ApplyCurrentToken(const AUsageType: TUsageType = utUnknown; const ATokenType: TTokenType = ttUnknown): TNodeOffset;
    procedure DeleteNode(const ANode: PNode);
    function GetError(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsRangeNode(const ANode: PNode): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsStmt(const ANode: PNode): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsStmtNode(const ANode: PNode): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsSibling(const ANode: TNodeOffset): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsSibling(const ANode: PNode): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsToken(const ANode: TNodeOffset): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsToken(const ANode: PNode): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function NewNode(const ANodeType: TNodeType): TNodeOffset;
    function NodePtr(const ANode: TNodeOffset): PNode; {$IFNDEF Debug} inline; {$ENDIF}
    function NodeSize(const ANode: TNodeOffset): Integer; overload;
    function NodeSize(const ANodeType: TNodeType): Integer; overload;
    function ParseCaseOp(): TNodeOffset;
    function ParseColumn(): TNodeOffset;
    function ParseCompoundStmt(): TNodeOffset;
    function ParseColumnIdentifier(): TNodeOffset;
    function ParseCreateRoutineStmt(const ARoutineType: TRoutineType): TNodeOffset;
    function ParseCreateStmt(): TNodeOffset;
    function ParseCreateTriggerStmt(): TNodeOffset;
    function ParseCreateViewStmt(): TNodeOffset;
    function ParseDataType(): TNodeOffset;
    function ParseDbIdentifier(const ADbIdentifierType: TDbIdentifierType): TNodeOffset;
    function ParseDefinerValue(): TNodeOffset;
    function ParseExpression(): TNodeOffset;
    function ParseFunction(): TNodeOffset;
    function ParseFunctionParameter(): TNodeOffset;
    function ParseGroup(): TNodeOffset;
    function ParseIfStmt(): TNodeOffset;
    function ParseIndexHint(): TNodeOffset;
    function ParseIndexIdentifier(): TNodeOffset;
    function ParseKeyword(): TNodeOffset;
    function ParseList(const Brackets: Boolean; const ParseNode: TParseFunction): TNodeOffset; overload;
    function ParseList(const Brackets: Boolean; const ParseNode: TParseFunction; const DelimterType: TTokenType; const DelimiterKeywordIndex: TWordList.TIndex): TNodeOffset; overload;
    function ParseLoopStmt(): TNodeOffset;
    function ParseTag(const KeywordIndex1: TWordList.TIndex; const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1; const KeywordIndex4: TWordList.TIndex = -1; const KeywordIndex5: TWordList.TIndex = -1): TNodeOffset;
    function ParseOrder(): TNodeOffset;
    function ParsePartitionIdentifier(): TNodeOffset;
    function ParsePL_SQLStmt(): TNodeOffset;
    function ParseProcedureParameter(): TNodeOffset;
    function ParseRepeatStmt(): TNodeOffset;
    function ParseSelectStmt(): TNodeOffset;
    function ParseString(): TNodeOffset;
    function ParseSubArea(const ParseNode: TParseFunction): TNodeOffset;
    function ParseStmt(const PL_SQL: Boolean = False): TNodeOffset;
    function ParseTableReference(): TNodeOffset;
    function ParseToken(): TNodeOffset;
    function ParseUnknownStmt(): TNodeOffset;
    function ParseUser(): TNodeOffset;
    function ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const ParseValue: TParseFunction): TNodeOffset;
    function ParseWhileStmt(): TNodeOffset;
    function RangeNodePtr(const ANode: TNodeOffset): PRangeNode; {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetError(const AErrorCode: Integer; const AErrorNode: TNodeOffset = 0);
    function StmtNodePtr(const ANode: TNodeOffset): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
    function StmtPtr(const ANode: TNodeOffset): PStmt; {$IFNDEF Debug} inline; {$ENDIF}
    function TokenPtr(const ANode: TNodeOffset): PToken; {$IFNDEF Debug} inline; {$ENDIF}

    property CurrentToken: TNodeOffset read GetCurrentToken;
    property Error: Boolean read GetError;
    property ParsedText: string read FParsedText;
    property NextToken[Index: Integer]: TNodeOffset read GetNextToken;

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
  STooManyTokensInExpression = 'Too many tokens (%d) in expression';
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

class function TCustomSQLParser.TNode.Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): TNodeOffset;
begin
  Result := AParser.NewNode(ANodeType);

  AParser.NodePtr(Result)^.FParser := AParser;
  AParser.NodePtr(Result)^.FNodeType := ANodeType;
end;

function TCustomSQLParser.TNode.GetOffset(): TNodeOffset;
begin
  Result := @Self - Parser.FNodes.Mem;
end;

{ TCustomSQLParser.TStmtNode **************************************************}

function TCustomSQLParser.TChild.GetFFirstToken(): TNodeOffset;
begin
  if (NodeType = ntToken) then
    Result := @Self - Parser.FNodes.Mem
  else
  begin
    Assert(Parser.IsRangeNode(@Self));
    Result := TCustomSQLParser.PRangeNode(@Self).FFirstToken;
  end;
end;

function TCustomSQLParser.TChild.GetFirstToken(): PToken;
begin
  if (NodeType = ntToken) then
    Result := @Self
  else
  begin
    Assert(Parser.IsRangeNode(@Self));
    Result := PRangeNode(@Self).FirstToken;
  end;
end;

function TCustomSQLParser.TChild.GetFLastToken(): TNodeOffset;
begin
  if (NodeType = ntToken) then
    Result := PNode(@Self)^.Offset
  else
  begin
    Assert(Parser.IsRangeNode(@Self));
    Result := PRangeNode(@Self)^.FLastToken;
  end;
end;

function TCustomSQLParser.TChild.GetLastToken(): PToken;
begin
  if (NodeType = ntToken) then
    Result := @Self
  else
  begin
    Assert(Parser.IsRangeNode(@Self));
    Result := PRangeNode(@Self).LastToken;
  end;
end;

function TCustomSQLParser.TChild.GetNextSibling(): PNode;
var
  Node: PNode;
  Token: PToken;
begin
  Assert(Parser.IsStmtNode(@Self));

  Token := PStmtNode(@Self)^.LastToken^.NextToken;

  if (Assigned(Token) and (Token^.TokenType = ttComma)) then
    Token := PToken(Token)^.NextToken; // ttComma

  Node := PNode(Token);

  Result := nil;
  while (Assigned(Node) and (not Parser.IsToken(Node) or (PToken(Node)^.TokenType <> ttComma)) and Parser.IsStmtNode(Node) and (PStmtNode(Node) <> PStmtNode(ParentNode))) do
  begin
    Result := Node;
    Node := PStmtNode(Node)^.ParentNode;
  end;

  if (Assigned(Result) and (PStmtNode(Token)^.FParentNode <> PStmtNode(Node)^.FParentNode)) then
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
  const AOperatorType: TOperatorType; const AKeywordIndex: TWordList.TIndex; const AUsageType: TUsageType): TNodeOffset;
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

function TCustomSQLParser.TToken.GetDbIdentifierType(): TDbIdentifierType;
begin
  if ((FParentNode = 0) or (Parser.NodePtr(FParentNode)^.NodeType <> ntDbIdentifier)) then
    Result := ditUnknown
  else if (@Self = PDbIdentifier(Parser.NodePtr(FParentNode))^.Identifier) then
    Result := PDbIdentifier(Parser.NodePtr(FParentNode))^.DbIdentifierType
  else if (@Self = PDbIdentifier(Parser.NodePtr(FParentNode))^.Prefix1) then
    case (PDbIdentifier(Parser.NodePtr(FParentNode))^.DbIdentifierType) of
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
  else if (@Self = PDbIdentifier(Parser.NodePtr(FParentNode))^.Prefix2) then
    case (PDbIdentifier(Parser.NodePtr(FParentNode))^.DbIdentifierType) of
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
  while (Parser.IsStmtNode(Node)) do
  begin
    Inc(Result);
    Node := PStmtNode(Node)^.ParentNode;
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
  Offset: TNodeOffset;
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

function TCustomSQLParser.TToken.GetOffset(): TNodeOffset;
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

{ TCustomSQLParser.TRangeNode *************************************************}

class function TCustomSQLParser.TRange.Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): TNodeOffset;
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

function TCustomSQLParser.TRange.GetOffset(): TNodeOffset;
begin
  Result := Heritage.Heritage.Offset;
end;

function TCustomSQLParser.TRange.GetParentNode(): PNode;
begin
  Result := PNode(Parser.NodePtr(FParentNode));
end;

procedure TCustomSQLParser.TRange.AddChild(const ChildNode: TNodeOffset);
var
  Child: PStmtNode;
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

class function TCustomSQLParser.TRoot.Create(const AParser: TCustomSQLParser): TNodeOffset;
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

{ TCustomSQLParser.TSiblings **************************************************}

procedure TCustomSQLParser.TSiblings.AddSibling(const ASibling: TNodeOffset);
begin
  if (FFirstSibling = 0) then
    FFirstSibling := ASibling;

  Heritage.AddChild(ASibling);
end;

class function TCustomSQLParser.TSiblings.Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): TNodeOffset;
begin
  Result := TRange.Create(AParser, ANodeType);
end;

function TCustomSQLParser.TSiblings.GetFirstChild(): PNode;
begin
  Result := Parser.NodePtr(FFirstSibling);
end;

{ TCustomSQLParser.TDbIdentifier **********************************************}

procedure TCustomSQLParser.TDbIdentifier.AddPrefix(const APrefix, ADot: TNodeOffset);
var
  Node: PNode;
begin
  Assert(Parser.NodePtr(APrefix)^.NodeType = ntDbIdentifier);
  Assert(Parser.TokenPtr(ADot)^.OperatorType = otDot);

  FPrefix1 := PDbIdentifier(Parser.NodePtr(APrefix))^.FIdentifier;
  FPrefix2 := PDbIdentifier(Parser.NodePtr(APrefix))^.FPrefix1;

  Heritage.AddChild(FPrefix1);
  Heritage.AddChild(FPrefix2);
  Heritage.AddChild(ADot);

  Parser.DeleteNode(Parser.NodePtr(APrefix));

  Node := ParentNode;
  while (Parser.IsRangeNode(Node)) do
  begin
    if (PRangeNode(Node)^.FFirstToken > Heritage.FFirstToken) then
      PRangeNode(Node)^.FFirstToken := Heritage.FFirstToken;
    Node := PRangeNode(ParentNode)^.ParentNode;
  end;
end;

class function TCustomSQLParser.TDbIdentifier.Create(const AParser: TCustomSQLParser; const AIdentifier: TNodeOffset; const ADbIdentifierType: TDbIdentifierType = ditUnknown): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntDbIdentifier);

  with PDbIdentifier(AParser.NodePtr(Result))^ do
  begin
    FIdentifier := AIdentifier;
    FDbIdentifierType := ADbIdentifierType;

    FPrefix1 := 0;
    FPrefix2 := 0;

    Heritage.AddChild(AIdentifier);
  end;
end;

function TCustomSQLParser.TDbIdentifier.GetIdentifier(): PToken;
begin
  Result := Parser.TokenPtr(FIdentifier);
end;

function TCustomSQLParser.TDbIdentifier.GetLastToken(): PToken;
begin
  Result := Heritage.GetLastToken();
end;

function TCustomSQLParser.TDbIdentifier.GetParentNode(): PNode;
begin
  Result := Heritage.GetParentNode();
end;

function TCustomSQLParser.TDbIdentifier.GetPrefix1(): PToken;
begin
  if (FPrefix1 = 0) then
    Result := nil
  else
    Result := Parser.TokenPtr(FPrefix1);
end;

function TCustomSQLParser.TDbIdentifier.GetPrefix2(): PToken;
begin
  if (FPrefix2 = 0) then
    Result := nil
  else
    Result := Parser.TokenPtr(FPrefix2);
end;

{ TCustomSQLParser.TFunction **************************************************}

class function TCustomSQLParser.TFunction.Create(const AParser: TCustomSQLParser; const AIdentifier, AArguments: TNodeOffset): TNodeOffset;
var
  Token: PToken;
begin
  Result := TRange.Create(AParser, ntFunction);

  with PFunction(AParser.NodePtr(Result))^ do
  begin
    FIdentifier := AIdentifier;
    FArguments := AArguments;

    Heritage.AddChild(AIdentifier);
    Heritage.AddChild(AArguments);

    Token := Identifier^.LastToken^.NextToken;
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

function TCustomSQLParser.TFunction.GetArguments(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FArguments);
end;

function TCustomSQLParser.TFunction.GetIdentifier(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FIdentifier);
end;

{ TCustomSQLParser.TUnaryOperation ********************************************}

class function TCustomSQLParser.TUnaryOp.Create(const AParser: TCustomSQLParser; const AOperator, AOperand: TNodeOffset): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntUnaryOp);

  with PUnaryOp(AParser.NodePtr(Result))^ do
  begin
    FOperator := AOperator;
    FOperand := AOperand;

    Heritage.AddChild(AOperator);
    Heritage.AddChild(AOperand);
  end;
end;

function TCustomSQLParser.TUnaryOp.GetOperand(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FOperand);
end;

function TCustomSQLParser.TUnaryOp.GetOperator(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FOperator);
end;

{ TCustomSQLParser.TBinaryOperation *******************************************}

class function TCustomSQLParser.TBinaryOp.Create(const AParser: TCustomSQLParser; const AOperator, AOperand1, AOperand2: TNodeOffset): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntBinaryOp);

  with PBinaryOp(AParser.NodePtr(Result))^ do
  begin
    FOperator := AOperator;
    FOperand1 := AOperand1;
    FOperand2 := AOperand2;

    Heritage.AddChild(AOperator);
    Heritage.AddChild(AOperand1);
    Heritage.AddChild(AOperand2);
  end;
end;

function TCustomSQLParser.TBinaryOp.GetOperand1(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FOperand1);
end;

function TCustomSQLParser.TBinaryOp.GetOperand2(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FOperand2);
end;

function TCustomSQLParser.TBinaryOp.GetOperator(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FOperator);
end;

{ TCustomSQLParser.TUser ******************************************************}

class function TCustomSQLParser.TUser.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset;
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

{ TCustomSQLParser.TBetweenOperation ******************************************}

class function TCustomSQLParser.TBetweenOp.Create(const AParser: TCustomSQLParser; const AOperator1, AOperator2: TNodeOffset; const AExpr, AMin, AMax: TNodeOffset): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntBetweenOp);

  with PBetweenOp(AParser.NodePtr(Result))^ do
  begin
    FOperator1 := AOperator1;
    FOperator2 := AOperator2;
    FExpr := AExpr;
    FMin := AMin;
    FMax := AMax;

    Heritage.AddChild(AOperator1);
    Heritage.AddChild(AOperator2);
    Heritage.AddChild(AExpr);
    Heritage.AddChild(AMin);
    Heritage.AddChild(AMax);
  end;
end;

function TCustomSQLParser.TBetweenOp.GetExpr(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FExpr);
end;

function TCustomSQLParser.TBetweenOp.GetMax(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FMax);
end;

function TCustomSQLParser.TBetweenOp.GetMin(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FMin);
end;

function TCustomSQLParser.TBetweenOp.GetOperator1(): PToken;
begin
  Result := Parser.TokenPtr(FOperator1);
end;

function TCustomSQLParser.TBetweenOp.GetOperator2(): PToken;
begin
  Result := Parser.TokenPtr(FOperator2);
end;

{ TCustomSQLParser.TCaseCond **************************************************}

class function TCustomSQLParser.TCaseCond.Create(const AParser: TCustomSQLParser; const AConditionValue, AResultValue: TNodeOffset): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntCaseCond);

  with PCaseCond(AParser.NodePtr(Result))^ do
  begin
    FConditionValue := AConditionValue;
    FResultValue := AResultValue;

    Heritage.AddChild(AConditionValue);
    Heritage.AddChild(AResultValue);
  end;
end;

function TCustomSQLParser.TCaseCond.GetConditionValue(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FConditionValue);
end;

function TCustomSQLParser.TCaseCond.GetNextCond(): PCaseCond;
begin
  Result := PCaseCond(Heritage.Heritage.NextSibling);
end;

function TCustomSQLParser.TCaseCond.GetResultValue(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FResultValue);
end;

{ TCustomSQLParser.TCase ******************************************************}

procedure TCustomSQLParser.TCaseOp.AddCondition(const AConditionValue, AResultValue: TNodeOffset);
begin
  Heritage.AddSibling(TCaseCond.Create(Parser, AConditionValue, AResultValue));
end;

class function TCustomSQLParser.TCaseOp.Create(const AParser: TCustomSQLParser; const AReferenceValue: TNodeOffset): TNodeOffset;
begin
  Result := TSiblings.Create(AParser, ntCaseOp);

  with PCaseOp(AParser.NodePtr(Result))^ do
  begin
    FReferenceValue := AReferenceValue;

    Heritage.Heritage.AddChild(AReferenceValue);
  end;
end;

function TCustomSQLParser.TCaseOp.GetFirstCond(): PCaseCond;
begin
  Result := PCaseCond(Heritage.FirstChild);
end;

function TCustomSQLParser.TCaseOp.GetReferenceValue(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FReferenceValue);
end;

procedure TCustomSQLParser.TCaseOp.SetElse(const AElseValue: TNodeOffset);
begin
  FElseValue := AElseValue;

  Heritage.Heritage.AddChild(AElseValue);
end;

{ TCustomSQLParser.TSoundsLikeOperation ***************************************}

class function TCustomSQLParser.TSoundsLikeOp.Create(const AParser: TCustomSQLParser; const AOperator1, AOperator2: TNodeOffset; const AOperand1, AOperand2: TNodeOffset): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntSoundsLikeOp);

  with PSoundsLikeOp(AParser.NodePtr(Result))^ do
  begin
    FOperator1 := AOperator1;
    FOperator2 := AOperator2;
    FOperand1 := AOperand1;
    FOperand2 := AOperand2;

    Heritage.AddChild(AOperator1);
    Heritage.AddChild(AOperator2);
    Heritage.AddChild(AOperand1);
    Heritage.AddChild(AOperand2);
  end;
end;

function TCustomSQLParser.TSoundsLikeOp.GetOperand1(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FOperator1);
end;

function TCustomSQLParser.TSoundsLikeOp.GetOperand2(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FOperator2);
end;

function TCustomSQLParser.TSoundsLikeOp.GetOperator1(): PToken;
begin
  Result := Parser.TokenPtr(FOperator1);
end;

function TCustomSQLParser.TSoundsLikeOp.GetOperator2(): PToken;
begin
  Result := Parser.TokenPtr(FOperator2);
end;

{ TCustomSQLParser.TStmt ******************************************************}

procedure TCustomSQLParser.TPLSQLCondPart.AddStmt(const AStmt: TNodeOffset);
begin
  Heritage.AddChild(AStmt);
end;

class function TCustomSQLParser.TPLSQLCondPart.Create(const AParser: TCustomSQLParser; const AOperatorToken, AExpression, AThenToken: TNodeOffset): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntPLSQLCondPart);

  with PPLSQLCondPart(AParser.NodePtr(Result))^ do
  begin
    FOperator := AOperatorToken;
    FExpression := AExpression;
    FThen := AThenToken;

    Heritage.AddChild(AOperatorToken);
    Heritage.AddChild(AExpression);
    Heritage.AddChild(AThenToken);
  end;
end;

{ TCustomSQLParser.TStmt ******************************************************}

class function TCustomSQLParser.TStmt.Create(const AParser: TCustomSQLParser; const AStmtType: TStmtType): TNodeOffset;
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
      Node := PStmtNode(Node)^.ParentNode;

    if (not Assigned(Node) or not Parser.IsStmt(Node)) then
      Result := nil
    else
      Result := PStmt(Node);
  end;
end;

{ TCustomSQLParser.TCreateRoutineStmt *****************************************}

class function TCustomSQLParser.TCreateRoutineStmt.Create(const AParser: TCustomSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TNodeOffset;
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
    Heritage.Heritage.AddChild(ANodes.IdentifierNode);
    Heritage.Heritage.AddChild(ANodes.OpenBracketToken);
    Heritage.Heritage.AddChild(ANodes.ParameterNode);
    Heritage.Heritage.AddChild(ANodes.CloseBracketToken);
    Heritage.Heritage.AddChild(ANodes.Return.ReturnsTag);
    Heritage.Heritage.AddChild(ANodes.Return.DataTypeNode);
    Heritage.Heritage.AddChild(ANodes.Body);
  end;
end;

{ TCustomSQLParser.TCreateTriggerStmt ********************************************}

class function TCustomSQLParser.TCreateTriggerStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset;
begin
  Result := TStmt.Create(AParser, stCreateTrigger);

  with PCreateTriggerStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.CreateTag);
    Heritage.Heritage.AddChild(ANodes.DefinerNode);
    Heritage.Heritage.AddChild(ANodes.TriggerTag);
    Heritage.Heritage.AddChild(ANodes.IdentifierNode);
    Heritage.Heritage.AddChild(ANodes.ActionValue);
    Heritage.Heritage.AddChild(ANodes.OnTag);
    Heritage.Heritage.AddChild(ANodes.TableIdentifierNode);
    Heritage.Heritage.AddChild(ANodes.ForEachRowTag);
    Heritage.Heritage.AddChild(ANodes.Body);
  end;
end;

{ TCustomSQLParser.TCreateViewStmt ********************************************}

class function TCustomSQLParser.TCreateViewStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset;
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
    Heritage.Heritage.AddChild(ANodes.IdentifierNode);
    Heritage.Heritage.AddChild(ANodes.Columns);
    Heritage.Heritage.AddChild(ANodes.AsTag);
    Heritage.Heritage.AddChild(ANodes.SelectStmt);
    Heritage.Heritage.AddChild(ANodes.OptionTag);
  end;
end;

{ TCustomSQLParser.TSelectStmt.TColumn ****************************************}

class function TCustomSQLParser.TSelectStmt.TColumn.Create(const AParser: TCustomSQLParser; const AValue, AAsToken, AAlias: TNodeOffset): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntColumn);

  with PColumn(AParser.NodePtr(Result))^ do
  begin
    FExpression := AValue;
    FAsToken := AAsToken;
    FAlias := AAlias;

    Heritage.AddChild(AValue);
    Heritage.AddChild(AAsToken);
    Heritage.AddChild(AAlias);
  end;
end;

function TCustomSQLParser.TSelectStmt.TColumn.GetAlias(): PToken;
begin
  if (FAlias = 0) then
    Result := nil
  else
    Result := Parser.TokenPtr(FAlias);
end;

function TCustomSQLParser.TSelectStmt.TColumn.GetAsToken(): PToken;
begin
  if (FAlias = 0) then
    Result := nil
  else
    Result := Parser.TokenPtr(FAlias);
end;

function TCustomSQLParser.TSelectStmt.TColumn.GetDisplayName(): string;
begin

end;

function TCustomSQLParser.TSelectStmt.TColumn.GetExpression(): PStmtNode;
begin
  Assert(Parser.IsStmtNode(Parser.NodePtr(FExpression)));

  Result := PStmtNode(Parser.NodePtr(FExpression));
end;

{ TCustomSQLParser.TSelectStmt.TTable.TIndexHint ******************************}

class function TCustomSQLParser.TSelectStmt.TTable.TIndexHint.Create(const AParser: TCustomSQLParser; const AIndexHintType: TIndexHintType; const AIndexHintKind: TIndexHintKind): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntIndexHint);

  with TSelectStmt.TTable.PIndexHint(AParser.NodePtr(Result))^ do
  begin
    FIndexHintType := AIndexHintType;
    FIndexHintKind := AIndexHintKind;
  end;
end;

function TCustomSQLParser.TSelectStmt.TTable.TIndexHint.GetNextIndexHint(): PIndexHint;
begin
  Result := PIndexHint(Heritage.Heritage.GetNextSibling());
end;

{ TCustomSQLParser.TSelectStmt.TTable *****************************************}

class function TCustomSQLParser.TSelectStmt.TTable.Create(const AParser: TCustomSQLParser; const AIdentifier, AAsToken, AAlias: TNodeOffset; const AIndexHints: TNodeOffset = 0; const APartitionToken: TNodeOffset = 0; const APartitions: TNodeOffset = 0): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntTable);

  with TSelectStmt.PTable(AParser.NodePtr(Result))^ do
  begin
    FIdentifier := AIdentifier;
    FPartitionToken := APartitionToken;
    FPartitions := APartitions;
    FAsToken := AAsToken;
    FAlias := AAlias;
    FIndexHints := AIndexHints;

    Heritage.AddChild(AIdentifier);
    Heritage.AddChild(APartitionToken);
    Heritage.AddChild(APartitions);
    Heritage.AddChild(AAsToken);
    Heritage.AddChild(AAlias);
    Heritage.AddChild(AIndexHints);
  end;
end;

{ TCustomSQLParser.TSelectStmt.TJoin ******************************************}

class function TCustomSQLParser.TSelectStmt.TJoin.Create(const AParser: TCustomSQLParser; const ALeftTable: TNodeOffset; const AJoinType: TJoinType; const ARightTable: TNodeOffset; const ACondition: TNodeOffset; const AKeywordTokens: TKeywordTokens): TNodeOffset;
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

class function TCustomSQLParser.TSelectStmt.TGroup.Create(const AParser: TCustomSQLParser; const AExpression, ADirection: TNodeOffset): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntGroup);

  with PGroup(AParser.NodePtr(Result))^ do
  begin
    FExpression := AExpression;
    FDirection := ADirection;

    Heritage.AddChild(AExpression);
    Heritage.AddChild(ADirection);
  end;
end;

function TCustomSQLParser.TSelectStmt.TGroup.GetExpression(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FExpression);
end;

function TCustomSQLParser.TSelectStmt.TGroup.GetAscending(): Boolean;
begin
  Result := (FDirection = 0) or (Parser.TokenPtr(FDirection)^.KeywordIndex = Parser.kiASC);
end;

{ TCustomSQLParser.TSelectStmt.TOrder *****************************************}

class function TCustomSQLParser.TSelectStmt.TOrder.Create(const AParser: TCustomSQLParser; const AExpression, ADirection: TNodeOffset): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntOrder);

  with PGroup(AParser.NodePtr(Result))^ do
  begin
    FExpression := AExpression;
    FDirection := ADirection;

    Heritage.AddChild(AExpression);
    Heritage.AddChild(ADirection);
  end;
end;

function TCustomSQLParser.TSelectStmt.TOrder.GetExpression(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FExpression);
end;

function TCustomSQLParser.TSelectStmt.TOrder.GetAscending(): Boolean;
begin
  Result := (FDirection = 0) or (Parser.TokenPtr(FDirection)^.KeywordIndex = Parser.kiASC);
end;

{ TCustomSQLParser.TSelectStmt ************************************************}

class function TCustomSQLParser.TSelectStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset;
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

function TCustomSQLParser.TSelectStmt.GetDistinct(): Boolean;
begin
  Result := (FNodes.DistinctToken <> 0) and ((Parser.TokenPtr(FNodes.DistinctToken)^.KeywordIndex = Parser.kiDISTINCT) or (Parser.TokenPtr(FNodes.DistinctToken)^.KeywordIndex = Parser.kiDISTINCTROW));
end;

function TCustomSQLParser.TSelectStmt.GetHaving(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FNodes.HavingNode);
end;

function TCustomSQLParser.TSelectStmt.GetWhere(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
begin
  Result := Parser.StmtNodePtr(FNodes.WhereNode);
end;

{ TCustomSQLParser.TCompoundStmt **********************************************}

class function TCustomSQLParser.TCompoundStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset;
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

{ TCustomSQLParser.TLoopStmt **************************************************}

class function TCustomSQLParser.TLoopStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset;
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

class function TCustomSQLParser.TRepeatStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset;
begin
  Result := TStmt.Create(AParser, stRepeat);

  with PRepeatStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.BeginLabelToken);
    Heritage.Heritage.AddChild(ANodes.RepeatTag);
    Heritage.Heritage.AddChild(ANodes.StmtList);
    Heritage.Heritage.AddChild(ANodes.UntilTag);
    Heritage.Heritage.AddChild(ANodes.SearchConditionExpression);
    Heritage.Heritage.AddChild(ANodes.EndTag);
    Heritage.Heritage.AddChild(ANodes.EndLabelToken);
  end;
end;

{ TCustomSQLParser.TWhileStmt *************************************************}

class function TCustomSQLParser.TWhileStmt.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset;
begin
  Result := TStmt.Create(AParser, stWhile);

  with PWhileStmt(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.BeginLabelToken);
    Heritage.Heritage.AddChild(ANodes.WhileTag);
    Heritage.Heritage.AddChild(ANodes.SearchConditionExpression);
    Heritage.Heritage.AddChild(ANodes.DoTag);
    Heritage.Heritage.AddChild(ANodes.StmtList);
    Heritage.Heritage.AddChild(ANodes.EndTag);
    Heritage.Heritage.AddChild(ANodes.EndLabelToken);
  end;
end;

{ TCustomSQLParser.TIfStmt ****************************************************}

procedure TCustomSQLParser.TIfStmt.AddPart(const APart: TNodeOffset);
begin
  Heritage.Heritage.AddChild(APart);
end;

class function TCustomSQLParser.TIfStmt.Create(const AParser: TCustomSQLParser): TNodeOffset;
begin
  Result := TStmt.Create(AParser, stIf);
end;

{ TCustomSQLParser.TTag *******************************************************}

class function TCustomSQLParser.TTag.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset;
begin
  Result := TSiblings.Create(AParser, ntTag);

  with PTag(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.KeywordToken1);
    Heritage.Heritage.AddChild(ANodes.KeywordToken2);
    Heritage.Heritage.AddChild(ANodes.KeywordToken3);
    Heritage.Heritage.AddChild(ANodes.KeywordToken4);
    Heritage.Heritage.AddChild(ANodes.KeywordToken5);
  end;
end;

{ TCustomSQLParser.TValue *******************************************************}

class function TCustomSQLParser.TValue.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset;
begin
  Result := TSiblings.Create(AParser, ntValue);

  with PValue(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.Heritage.AddChild(ANodes.KeywordToken);
    Heritage.Heritage.AddChild(ANodes.AssignToken);
    Heritage.Heritage.AddChild(ANodes.ValueNode);
  end;
end;

{ TCustomSQLParser.TFunctionParameter *****************************************}

class function TCustomSQLParser.TRoutineParam.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntFunctionParam);

  with PRoutineParam(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.IdentifierToken);
    Heritage.AddChild(ANodes.DataTypeNode);
  end;
end;

{ TCustomSQLParser.TDataType **************************************************}

class function TCustomSQLParser.TDataType.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntDataType);

  with PDataType(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.IdentifierToken);
    Heritage.AddChild(ANodes.IdentifierToken);
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

{ TCustomSQLParser.TSubArea ******************************************************}

class function TCustomSQLParser.TSubArea.Create(const AParser: TCustomSQLParser; const ANodes: TNodes): TNodeOffset;
begin
  Result := TRange.Create(AParser, ntSubArea);

  with PSubArea(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.AreaNode);
  end;
end;

{ TCustomSQLParser.TList ******************************************************}

class function TCustomSQLParser.TList.Create(const AParser: TCustomSQLParser; const ANodes: TNodes; const ChildrenCount: Integer; const AChildrens: array of TNodeOffset): TNodeOffset;
type
  PList = ^TList; // Why is this needed???
var
  I: Integer;
begin
  Result := TRange.Create(AParser, ntList);

  with PList(AParser.NodePtr(Result))^ do
  begin
    FNodes := ANodes;

    Heritage.AddChild(ANodes.OpenBracket);
    for I := 0 to ChildrenCount - 1 do
      Heritage.AddChild(Integer(AChildrens[I]));
    Heritage.AddChild(ANodes.CloseBracket);
  end;
end;

{ TCustomSQLParser ************************************************************}

function TCustomSQLParser.ApplyCurrentToken(const AUsageType: TUsageType = utUnknown; const ATokenType: fspTypes.TTokenType = fspTypes.ttUnknown): TNodeOffset;
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
  PDeletedNode(ANode)^.FNodeSize := NodeSize(ANode^.NodeType);
  PDeletedNode(ANode)^.FNodeType := ntDeleted;
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

function TCustomSQLParser.GetCurrentToken(): TNodeOffset;
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

function TCustomSQLParser.GetNextToken(Index: Integer): TNodeOffset;
begin
  Assert(Index > 0);

  Result := GetParsedToken(Index);
end;

function TCustomSQLParser.GetParsedToken(Index: Integer): TNodeOffset;
var
  Token: TNodeOffset;
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
    Result := TNodeOffset(FParsedTokens[Index]);
end;

function TCustomSQLParser.GetRoot(): PRoot;
begin
  Result := PRoot(NodePtr(FRoot));
end;

function TCustomSQLParser.IsRangeNode(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and not (ANode^.NodeType in [ntUnknown, ntRoot, ntToken]);
end;

function TCustomSQLParser.IsStmt(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType in [ntUnknownStmt, ntCompoundStmt, ntSelectStmt]);
end;

function TCustomSQLParser.IsStmtNode(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and not (ANode^.NodeType in [ntUnknown, ntRoot]);
end;

function TCustomSQLParser.IsSibling(const ANode: TNodeOffset): Boolean;
begin
  Result := IsSibling(NodePtr(ANode));
end;

function TCustomSQLParser.IsSibling(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType in [ntCaseCond, ntColumn, ntIndexHint, ntTable, ntGroup, ntOrder]);
end;

function TCustomSQLParser.IsToken(const ANode: TNodeOffset): Boolean;
begin
  Result := IsToken(NodePtr(ANode));
end;

function TCustomSQLParser.IsToken(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType in [ntToken]);
end;

function TCustomSQLParser.NewNode(const ANodeType: TNodeType): TNodeOffset;
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

function TCustomSQLParser.NodePtr(const ANode: TNodeOffset): PNode;
begin
  if (ANode = 0) then
    Result := nil
  else
    Result := @FNodes.Mem[ANode];
end;

function TCustomSQLParser.NodeSize(const ANode: TNodeOffset): Integer;
begin
  if (NodePtr(ANode)^.NodeType = ntDeleted) then
    Result := PDeletedNode(NodePtr(ANode))^.FNodeSize
  else
    Result := NodeSize(NodePtr(ANode)^.NodeType);
end;

function TCustomSQLParser.NodeSize(const ANodeType: TNodeType): Integer;
begin
  case (ANodeType) of
    ntRoot: Result := SizeOf(TRoot);
    ntToken: Result := SizeOf(TToken);
    ntRange: Result := SizeOf(TRange);
    ntDbIdentifier: Result := SizeOf(TDbIdentifier);
    ntFunction: Result := SizeOf(TFunction);
    ntUnaryOp: Result := SizeOf(TUnaryOp);
    ntBinaryOp: Result := SizeOf(TBinaryOp);
    ntUser: Result := SizeOf(TUser);
    ntColumn: Result := SizeOf(TSelectStmt.TColumn);
    ntJoin: Result := SizeOf(TSelectStmt.TJoin);
    ntTable: Result := SizeOf(TSelectStmt.TTable);
    ntGroup: Result := SizeOf(TSelectStmt.TGroup);
    ntOrder: Result := SizeOf(TSelectStmt.TOrder);
    ntPLSQLCondPart: Result := SizeOf(TPLSQLCondPart);
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

function TCustomSQLParser.ParseCaseOp(): TNodeOffset;
var
  First: Boolean;
  ResultValue: TNodeOffset;
  Value: TNodeOffset;
begin
  ApplyCurrentToken(utOperator); // CASE

  Value := 0;
  if (CurrentToken = 0) then
  begin
    Result := 0;
    SetError(PE_IncompleteStmt);
  end
  else
  begin
    if ((TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiTHEN)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiELSE)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiEND)) then
      Value := ParseExpression;

    Result := TCaseOp.Create(Self, Value);
    with PCaseOp(NodePtr(Result))^ do
    begin
      First := True;
      repeat
        if (First) then
          First := False
        else
          ApplyCurrentToken(utPLSQL); // WHEN

        if (CurrentToken = 0) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN) then
          SetError(PE_UnexpectedToken, CurrentToken)
        else
        begin
          Value := ParseExpression();
          if (CurrentToken = 0) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiTHEN) then
            SetError(PE_UnexpectedToken, CurrentToken)
          else
          begin
            ApplyCurrentToken(utPLSQL); // THEN

            ResultValue := ParseExpression();

            AddCondition(Value, ResultValue);
          end;
        end;
      until (Error or (CurrentToken = 0) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN));

      if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE)) then
      begin
        ApplyCurrentToken(utPLSQL); // ELSE

        if (CurrentToken = 0) then
          SetError(PE_IncompleteStmt)
        else
          SetElse(ParseExpression());
      end;

      if (CurrentToken = 0) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiEND) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
        ApplyCurrentToken(utPLSQL); // END
    end;
  end;
end;

function TCustomSQLParser.ParseColumn(): TNodeOffset;
var
  Alias: TNodeOffset;
  AsToken: TNodeOffset;
  Value: TNodeOffset;
begin
  Value := ParseExpression();

  if (Error or (CurrentToken = 0) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiAs)) then
    AsToken := 0
  else
    AsToken := ApplyCurrentToken();

  Alias := 0;
  if (not Error) then
    if ((AsToken > 0) and ((CurrentToken = 0) or (TokenPtr(CurrentToken)^.TokenType = ttDelimiter))) then
      SetError(PE_IncompleteStmt)
    else if ((AsToken > 0) and (CurrentToken > 0) and not (TokenPtr(CurrentToken)^.TokenType in ttIdentifiers)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType in ttIdentifiers)) then
      Alias := ApplyCurrentToken(utAlias);

  if (Error) then
    Result := 0
  else
    Result := TSelectStmt.TColumn.Create(Self, Value, AsToken, Alias);
end;

function TCustomSQLParser.ParseCompoundStmt(): TNodeOffset;
var
  Nodes: TCompoundStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(CurrentToken)^.TokenType = ttBeginLabel) then
    Nodes.BeginLabelToken := ApplyCurrentToken();

  Nodes.BeginTag := ParseTag(kiBEGIN);

  if (not Error) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter, kiEND);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttIdentifier)) then
    if ((Nodes.BeginLabelToken = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(Nodes.BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.EndLabelToken := ApplyCurrentToken(utLabel, ttEndLabel);

  Result := TCompoundStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseColumnIdentifier(): TNodeOffset;
begin
  Result := 0;
  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdentifiers)) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Result := ApplyCurrentToken();
end;

function TCustomSQLParser.ParseCreateRoutineStmt(const ARoutineType: TRoutineType): TNodeOffset;
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
      Nodes.IdentifierNode := ParseDbIdentifier(ditFunction)
    else
      Nodes.IdentifierNode := ParseDbIdentifier(ditProcedure);

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.ParameterNode := ParseList(True, ParseFunctionParameter)
    else
      Nodes.ParameterNode := ParseList(True, ParseProcedureParameter);

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

function TCustomSQLParser.ParseCreateStmt(): TNodeOffset;
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

function TCustomSQLParser.ParseCreateTriggerStmt(): TNodeOffset;
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
    Nodes.IdentifierNode := ParseDbIdentifier(ditTrigger);

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
    Nodes.TableIdentifierNode := ParseDbIdentifier(ditTable);

  if (not Error) then
    Nodes.ForEachRowTag := ParseTag(kiFOR, kiEACH, kiROW);

  if (not Error) then
    Nodes.Body := ParseStmt(True);

  Result := TCreateTriggerStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseCreateViewStmt(): TNodeOffset;
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
    Nodes.IdentifierNode := ParseDbIdentifier(ditView);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.Columns := ParseList(True, ParseColumnIdentifier);

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

function TCustomSQLParser.ParseDataType(): TNodeOffset;
var
  IdentifierString: string;
  Nodes: TDataType.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttIdentifier) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Nodes.IdentifierToken := ApplyCurrentToken();

  if (not Error) then
  begin
    IdentifierString := UpperCase(TokenPtr(Nodes.IdentifierToken)^.AsString);

    if ((IdentifierString <> 'BIT')
      and (IdentifierString <> 'TINYINT')
      and (IdentifierString <> 'SMALLINT')
      and (IdentifierString <> 'MEDIUMINT')
      and (IdentifierString <> 'INT')
      and (IdentifierString <> 'INTEGER')
      and (IdentifierString <> 'BIGINT')
      and (IdentifierString <> 'REAL')
      and (IdentifierString <> 'DOUBLE')
      and (IdentifierString <> 'FLOAT')
      and (IdentifierString <> 'DECIMAL')
      and (IdentifierString <> 'NUMERIC')
      and (IdentifierString <> 'DATE')
      and (IdentifierString <> 'TIME')
      and (IdentifierString <> 'TIMESTAMP')
      and (IdentifierString <> 'DATETIME')
      and (IdentifierString <> 'YEAR')
      and (IdentifierString <> 'CHAR')
      and (IdentifierString <> 'VARCHAR')
      and (IdentifierString <> 'BINARY')
      and (IdentifierString <> 'VARBINARY')
      and (IdentifierString <> 'TINYBLOB')
      and (IdentifierString <> 'BLOB')
      and (IdentifierString <> 'MEDIUMBLOB')
      and (IdentifierString <> 'LONGBLOB')
      and (IdentifierString <> 'TINYTEXT')
      and (IdentifierString <> 'TEXT')
      and (IdentifierString <> 'MEDIUMTEXT')
      and (IdentifierString <> 'LONGTEXT')
      and (IdentifierString <> 'ENUM')
      and (IdentifierString <> 'SET')) then
      SetError(PE_UnexpectedToken, CurrentToken);

      if (not Error) then
        if ((IdentifierString = 'BIT')
          or (IdentifierString = 'TINYINT')
          or (IdentifierString = 'SMALLINT')
          or (IdentifierString = 'MEDIUMINT')
          or (IdentifierString = 'INT')
          or (IdentifierString = 'INTEGER')
          or (IdentifierString = 'BIGINT')
          or (IdentifierString = 'REAL')
          or (IdentifierString = 'DOUBLE')
          or (IdentifierString = 'FLOAT')
          or (IdentifierString = 'DECIMAL')
          or (IdentifierString = 'NUMERIC')
          or (IdentifierString = 'CHAR')
          or (IdentifierString = 'VARCHAR')
          or (IdentifierString = 'BINARY')
          or (IdentifierString = 'VARBINARY')) then
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
              and ((IdentifierString = 'REAL')
              or (IdentifierString = 'DOUBLE')
              or (IdentifierString = 'FLOAT')
              or (IdentifierString = 'DECIMAL')
              or (IdentifierString = 'NUMERIC'))) then
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
        else if ((IdentifierString = 'ENUM')
          or (IdentifierString = 'SET')) then
        begin
          if (not Error) then
            Nodes.StringValuesNode := ParseList(True, ParseString);
        end;

    if (not Error) then
    begin
      if ((Nodes.LengthToken = 0)
        and ((IdentifierString = 'VARCHAR')
        or (IdentifierString = 'VARBINARY'))) then
          if (CurrentToken = 0) then
            SetError(PE_IncompleteStmt)
          else
            SetError(PE_UnexpectedToken, CurrentToken);

      if ((Nodes.DecimalsToken = 0)
        and ((IdentifierString = 'REAL')
        or (IdentifierString = 'DOUBLE')
        or (IdentifierString = 'FLOAT'))) then
          if (CurrentToken = 0) then
            SetError(PE_IncompleteStmt)
          else
            SetError(PE_UnexpectedToken, CurrentToken);
    end;
  end;

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUNSIGNED)
    and ((IdentifierString = 'TINYINT')
    or (IdentifierString = 'SMALLINT')
    or (IdentifierString = 'MEDIUMINT')
    or (IdentifierString = 'INT')
    or (IdentifierString = 'INTEGER')
    or (IdentifierString = 'BIGINT')
    or (IdentifierString = 'REAL')
    or (IdentifierString = 'DOUBLE')
    or (IdentifierString = 'FLOAT')
    or (IdentifierString = 'DECIMAL')
    or (IdentifierString = 'NUMERIC'))) then
    Nodes.UnsignedTag := ParseTag(kiUNSIGNED);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiZEROFILL)
    and ((IdentifierString = 'TINYINT')
    or (IdentifierString = 'SMALLINT')
    or (IdentifierString = 'MEDIUMINT')
    or (IdentifierString = 'INT')
    or (IdentifierString = 'INTEGER')
    or (IdentifierString = 'BIGINT')
    or (IdentifierString = 'REAL')
    or (IdentifierString = 'DOUBLE')
    or (IdentifierString = 'FLOAT')
    or (IdentifierString = 'DECIMAL')
    or (IdentifierString = 'NUMERIC'))) then
    Nodes.ZerofillTag := ParseTag(kiZEROFILL);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiBINARY)
    and ((IdentifierString = 'TINYTEXT')
    or (IdentifierString = 'TEXT')
    or (IdentifierString = 'MEDIUMTEXT')
    or (IdentifierString = 'LONGTEXT'))) then
    Nodes.BinaryTag := ParseTag(kiBINARY);

  if (not Error
    and ((IdentifierString = 'CHAR')
    or (IdentifierString = 'VARCHAR')
    or (IdentifierString = 'TINYTEXT')
    or (IdentifierString = 'TEXT')
    or (IdentifierString = 'MEDIUMTEXT')
    or (IdentifierString = 'LONGTEXT')
    or (IdentifierString = 'ENUM')
    or (IdentifierString = 'SET'))) then
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

function TCustomSQLParser.ParseDbIdentifier(const ADbIdentifierType: TDbIdentifierType): TNodeOffset;
var
  Dot: TNodeOffset;
  Prefix: TNodeOffset;
begin
  Result := 0;
  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdentifiers)) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Result := TDbIdentifier.Create(Self, ApplyCurrentToken(), ADbIdentifierType);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.OperatorType = otDot) and (ADbIdentifierType <> ditDatabase)) then
  begin
    Dot := ApplyCurrentToken();

    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdentifiers)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      Prefix := Result;

      Result := TDbIdentifier.Create(Self, ApplyCurrentToken(), ADbIdentifierType);
      PDbIdentifier(NodePtr(Result))^.AddPrefix(Prefix, Dot);
    end;
  end;

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.OperatorType = otDot) and (ADbIdentifierType in [ditIndex, ditField, ditAllFields, ditPartition])) then
  begin
    Dot := ApplyCurrentToken();

    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdentifiers)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      Prefix := Result;

      Result := TDbIdentifier.Create(Self, ApplyCurrentToken(), ADbIdentifierType);
      PDbIdentifier(NodePtr(Result))^.AddPrefix(Prefix, Dot);
    end;
  end;
end;

function TCustomSQLParser.ParseDefinerValue(): TNodeOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.KeywordToken := ApplyCurrentToken();

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

  if (Error) then
    Result := 0
  else
    Result := TValue.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseExpression(): TNodeOffset;
const
  MaxNodeCount = 100;
var
  NodeCount: Integer;
  Nodes: array[0 .. MaxNodeCount - 1] of TNodeOffset;

  procedure AddNode(const ANode: TNodeOffset; const Apply: Boolean = True);
  begin
    if (NodeCount = MaxNodeCount) then
      raise Exception.CreateFmt(STooManyTokensInExpression, [NodeCount]);

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
          AddNode(ParseSubArea(ParseExpression))
        else if (IsRangeNode(NodePtr(Nodes[NodeCount - 1]))) then
          SetError(PE_UnexpectedToken, RangeNodePtr(Nodes[NodeCount - 1])^.FFirstToken)
        else if ((NextToken[1] > 0) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT)) then
          AddNode(ParseSubArea(ParseSelectStmt))
        else if (TokenPtr(Nodes[NodeCount - 1])^.OperatorType = otIn) then
          AddNode(ParseList(True, ParseExpression))
        else if (TokenPtr(Nodes[NodeCount - 1])^.OperatorType in [otInterval, otBinary, otCollate]) then
          AddNode(ParseList(True, ParsePartitionIdentifier))
        else
          AddNode(ParseList(True, ParseExpression));
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
                  TokenPtr(CurrentToken)^.FUsageType := utDbIdentifier;
                  AddNode(TDbIdentifier.Create(Self, CurrentToken, ditAllFields));
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
                  TokenPtr(CurrentToken)^.FUsageType := utDbIdentifier;
                  AddNode(TDbIdentifier.Create(Self, CurrentToken, ditField));
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
            else if ((NodePtr(Nodes[I - 1])^.NodeType <> ntDbIdentifier) or (PDbIdentifier(NodePtr(Nodes[I - 1]))^.FPrefix2 > 0)) then
              SetError(PE_UnexpectedToken, Nodes[I])
            else if (NodePtr(Nodes[I + 1])^.NodeType = ntDbIdentifier) then
            begin
              PDbIdentifier(NodePtr(Nodes[I + 1]))^.AddPrefix(Nodes[I - 1], Nodes[I]);
              Dec(NodeCount, 2);
              Move(Nodes[I + 1], Nodes[I - 1], (NodeCount - I + 1) * SizeOf(Nodes[0]));
              Dec(I);
            end
            else if (NodePtr(Nodes[I + 1])^.NodeType = ntFunction) then
            begin
              if ((PDbIdentifier(NodePtr(Nodes[I - 1]))^.FPrefix1 > 0) or (PFunction(NodePtr(Nodes[I + 1]))^.Identifier^.NodeType <> ntDbIdentifier)) then
                SetError(PE_UnexpectedToken, Nodes[I + 1])
              else
              begin
                PDbIdentifier(PFunction(NodePtr(Nodes[I + 1]))^.Identifier)^.AddPrefix(Nodes[I - 1], Nodes[I]);
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

function TCustomSQLParser.ParseFunction(): TNodeOffset;
var
  Identifier: TNodeOffset;
  Arguments: TNodeOffset;
begin
  TokenPtr(CurrentToken)^.FOperatorType := otFunction_;
  if ((FFunctions.Count = 0) or (FFunctions.IndexOf(TokenPtr(CurrentToken)^.FText.SQL, TokenPtr(CurrentToken)^.FText.Length) >= 0)) then
    Identifier := ApplyCurrentToken(utFunction)
  else
    Identifier := TDbIdentifier.Create(Self, ApplyCurrentToken(utDbIdentifier), ditFunction);

  Arguments := ParseList(True, ParseExpression);

  Result := TFunction.Create(Self, Identifier, Arguments);
end;

function TCustomSQLParser.ParseFunctionParameter(): TNodeOffset;
var
  Nodes: TRoutineParam.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdentifiers)) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Nodes.IdentifierToken := ApplyCurrentToken();

  if (not Error) then
    Nodes.DataTypeNode := ParseDataType();

  Result := TRoutineParam.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseGroup(): TNodeOffset;
var
  Expression: TNodeOffset;
  Direction: TNodeOffset;
begin
  Expression := ParseExpression();

  if (Error or (CurrentToken = 0) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiASC) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiDESC)) then
    Direction := 0
  else
    Direction := ApplyCurrentToken();

  if (Error) then
    Result := 0
  else
    Result := TSelectStmt.TGroup.Create(Self, Expression, Direction);
end;

function TCustomSQLParser.ParseIfStmt(): TNodeOffset;
var
  Expression: TNodeOffset;
  First: Boolean;
  OperatorToken: TNodeOffset;
  Part: TNodeOffset;
  ThenToken: TNodeOffset;
begin
  Assert((CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF));

  Result := TIfStmt.Create(Self);

  First := True;

  repeat
    if (First) then
      First := False
    else if (TokenPtr(CurrentToken)^.TokenType = ttDelimiter) then
      ApplyCurrentToken(); // ttDelimiter;

    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken).KeywordIndex <> kiIF)
      and (TokenPtr(CurrentToken).KeywordIndex <> kiELSE)
      and (TokenPtr(CurrentToken).KeywordIndex <> kiELSEIF)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      OperatorToken := CurrentToken;
      if (TokenPtr(OperatorToken)^.KeywordIndex <> kiELSE) then
        ApplyCurrentToken();

      if (TokenPtr(OperatorToken)^.KeywordIndex = kiELSE) then
        Expression := 0
      else
        Expression := ParseExpression;

      if (not Error) then
        if (CurrentToken = 0) then
          SetError(PE_IncompleteStmt)
        else if ((TokenPtr(OperatorToken)^.KeywordIndex <> kiELSE) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiTHEN)) then
          SetError(PE_UnexpectedToken, CurrentToken)
        else
        begin
          ThenToken := ApplyCurrentToken();

          Part := TPLSQLCondPart.Create(Self, OperatorToken, Expression, ThenToken);

          if (CurrentToken = 0) then
            SetError(PE_IncompleteStmt)
          else
            repeat
              PPLSQLCondPart(NodePtr(Part))^.AddStmt(ParseStmt(True));

              if (CurrentToken = 0) then
                SetError(PE_IncompleteStmt)
              else if (TokenPtr(CurrentToken)^.TokenType <> ttDelimiter) then
                SetError(PE_UnexpectedToken, CurrentToken)
              else
                ApplyCurrentToken();
            until (Error
              or (CurrentToken = 0)
              or (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE)
              or (TokenPtr(CurrentToken)^.KeywordIndex = kiELSEIF)
              or (TokenPtr(CurrentToken)^.KeywordIndex = kiEND));

          PIfStmt(NodePtr(Result))^.AddPart(Part);
        end;
    end;
  until (Error or (CurrentToken = 0) or (TokenPtr(CurrentToken)^.KeywordIndex = kiEND));

  if (not Error) then
    if (CurrentToken = 0) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiEND) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
    begin
      ApplyCurrentToken();

      if (CurrentToken = 0) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiIF) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
        ApplyCurrentToken();
    end;
end;

function TCustomSQLParser.ParseIndexHint(): TNodeOffset;
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
          ParseList(True, ParseIndexIdentifier);
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
          ParseList(True, ParseIndexIdentifier);
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
          ParseList(True, ParseIndexIdentifier);
        end;
      end
      else
        SetError(PE_UnexpectedToken, CurrentToken);

      if (not Error) then
        Result := TSelectStmt.TTable.TIndexHint.Create(Self, IndexHintType, IndexHintKind);
    end;
  end;
end;

function TCustomSQLParser.ParseIndexIdentifier(): TNodeOffset;
begin
  Result := ParseDbIdentifier(ditIndex);
end;

function TCustomSQLParser.ParseTag(const KeywordIndex1: TWordList.TIndex; const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1; const KeywordIndex4: TWordList.TIndex = -1; const KeywordIndex5: TWordList.TIndex = -1): TNodeOffset;
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

function TCustomSQLParser.ParseOrder(): TNodeOffset;
var
  Expression: TNodeOffset;
  Direction: TNodeOffset;
begin
  Expression := ParseExpression();

  if (Error or (CurrentToken = 0) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiASC) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiDESC)) then
    Direction := 0
  else
    Direction := ApplyCurrentToken();

  if (Error) then
    Result := 0
  else
    Result := TSelectStmt.TOrder.Create(Self, Expression, Direction);
end;

function TCustomSQLParser.ParseKeyword(): TNodeOffset;
begin
  Result := 0;
  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex < 0) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Result := ApplyCurrentToken();
end;

function TCustomSQLParser.ParseList(const Brackets: Boolean; const ParseNode: TParseFunction): TNodeOffset;
begin
  Result := ParseList(Brackets, ParseNode, ttComma, -1);
end;

function TCustomSQLParser.ParseList(const Brackets: Boolean; const ParseNode: TParseFunction; const DelimterType: fspTypes.TTokenType; const DelimiterKeywordIndex: TWordList.TIndex): TNodeOffset;
type
  TIntergerArray = array of Integer;
var
  DelimiterFound: Boolean;
  I: Integer;
  Index: Integer;
  Nodes: TList.TNodes;
  Childrens: array[0..100-1] of TNodeOffset;
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
      or ((DelimterType = ttDelimiter) and (TokenPtr(CurrentToken)^.KeywordIndex = DelimiterKeywordIndex)));

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

function TCustomSQLParser.ParseLoopStmt(): TNodeOffset;
var
  Nodes: TLoopStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(CurrentToken)^.TokenType = ttBeginLabel) then
    Nodes.BeginLabelToken := ApplyCurrentToken();

  Nodes.BeginTag := ParseTag(kiLOOP);

  if (not Error) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter, kiEND);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiLOOP);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttIdentifier)) then
    if ((Nodes.BeginLabelToken = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(Nodes.BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.EndLabelToken := ApplyCurrentToken(utLabel, ttEndLabel);

  Result := TLoopStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParsePartitionIdentifier(): TNodeOffset;
begin
  Result := ParseDbIdentifier(ditPartition);
end;

function TCustomSQLParser.ParsePL_SQLStmt(): TNodeOffset;
begin
  Result := ParseStmt(True);
end;

function TCustomSQLParser.ParseProcedureParameter(): TNodeOffset;
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
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdentifiers)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.IdentifierToken := ApplyCurrentToken();

  if (not Error) then
    Nodes.DataTypeNode := ParseDataType();

  Result := TRoutineParam.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseRepeatStmt(): TNodeOffset;
var
  Nodes: TRepeatStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(CurrentToken)^.TokenType = ttBeginLabel) then
    Nodes.BeginLabelToken := ApplyCurrentToken();

  Nodes.RepeatTag := ParseTag(kiREPEAT);

  if (not Error) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter, kiUNTIL);

  if (not Error) then
    Nodes.UntilTag := ParseTag(kiUNTIL);

  if (not Error) then
    Nodes.SearchConditionExpression := ParseExpression();

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiREPEAT);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttIdentifier)) then
    if ((Nodes.BeginLabelToken = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(Nodes.BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.EndLabelToken := ApplyCurrentToken(utLabel, ttEndLabel);

  Result := TRepeatStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseSelectStmt(): TNodeOffset;
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
        Nodes.WhereNode := ParseExpression();
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
        Nodes.HavingNode := ParseExpression();
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

function TCustomSQLParser.ParseString(): TNodeOffset;
begin
  Result := 0;
  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttStrings)) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else
    Result := ApplyCurrentToken();
end;

function TCustomSQLParser.ParseSubArea(const ParseNode: TParseFunction): TNodeOffset;
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

function TCustomSQLParser.ParseStmt(const PL_SQL: Boolean = False): TNodeOffset;
var
  FirstToken: TNodeOffset;
  KeywordIndex: TWordList.TIndex;
  KeywordToken: TNodeOffset;
  LabelToken: TNodeOffset;
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

function TCustomSQLParser.ParseTableReference(): TNodeOffset;

  function ParseTableFactor(): TNodeOffset;
  var
    Alias: TNodeOffset;
    AsToken: TNodeOffset;
    IndexHints: TNodeOffset;
    OJToken: TNodeOffset;
    OpenBracketToken: TNodeOffset;
    Partition: PNode;
    PartitionToken: TNodeOffset;
    Partitions: TNodeOffset;
    Prefix: TNodeOffset;
  begin
    if (CurrentToken = 0) then
    begin
      SetError(PE_IncompleteStmt);
      Result := 0;
    end
    else if (TokenPtr(CurrentToken)^.TokenType in ttIdentifiers) then
    begin
      Result := TDbIdentifier.Create(Self, ApplyCurrentToken(), ditTable);

      if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.OperatorType = otDot)) then
      begin
        if (NextToken[1] = 0) then
          SetError(PE_IncompleteStmt)
        else if (PDbIdentifier(NodePtr(Result))^.FPrefix2 > 0) then
          SetError(PE_UnexpectedToken, CurrentToken)
        else if (not (TokenPtr(NextToken[1])^.TokenType in ttIdentifiers)) then
          SetError(PE_UnexpectedToken, NextToken[1])
        else
        begin
          Prefix := Result;
          Result := TDbIdentifier.Create(Self, NextToken[1], ditTable);
          PDbIdentifier(NodePtr(Result))^.AddPrefix(Prefix, CurrentToken);
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
          Partitions := ParseList(True, ParsePartitionIdentifier);
          Partition := PSiblings(NodePtr(Partitions))^.FirstChild;
          while (Assigned(Partition)) do
          begin
            if (Partition^.NodeType <> ntDbIdentifier) then
              SetError(PE_UnexpectedToken, PStmtNode(Partition)^.FFirstToken)
            else if (PDbIdentifier(Partition)^.FPrefix1 > 0) then
              SetError(PE_UnexpectedToken, PDbIdentifier(Partition)^.Identifier^.NextToken^.Offset)
            else
              PDbIdentifier(Partition)^.FDbIdentifierType := ditPartition;
            Partition := PStmtNode(Partition)^.NextSibling;
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
        else if ((AsToken > 0) and (CurrentToken > 0) and not (TokenPtr(CurrentToken)^.TokenType in ttIdentifiers)) then
          SetError(PE_UnexpectedToken, CurrentToken)
        else if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType in ttIdentifiers)) then
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
            else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdentifiers)) then
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
          if (not IsRangeNode(NodePtr(Result))) then
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
  Condition: TNodeOffset;
  I: Integer;
  JoinType: TJoinType;
  JoinedTable: TNodeOffset;
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
              Condition := ParseExpression()
            end
            else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiUSING) and not (JoinType in [jtNaturalLeft, jtNaturalRight])) then
            begin
              ApplyKeywordToken(KeywordTokens);
              Condition := ParseList(True, ParseColumnIdentifier);
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

function TCustomSQLParser.ParseToken(): TNodeOffset;
label
  TwoChars,
  Selection, SelSpace, SelQuotedIdentifier, SelNotLess, SelNotEqual1, SelNotGreater, SelNot1, SelDoubleQuote, SelComment, SelModulo, SelDolor, SelAmpersand2, SelBitAND, SelSingleQuote, SelOpenBracket, SelCloseBracket, SelMySQLCodeEnd, SelMulti, SelComma, SelDoubleDot, SelDot, SelMySQLCode, SelDiv, SelNumeric, SelSLComment, SelArrow, SelMinus, SelPlus, SelAssign, SelColon, SelDelimiter, SelNULLSaveEqual, SelLessEqual, SelShiftLeft, SelNotEqual2, SelLess, SelEqual, SelGreaterEqual, SelShiftRight, SelGreater, SelParameter, SelAt, SelUnquotedIdentifier, SelDBIdentifier, SelBackslash, SelCloseSquareBracket, SelHat, SelMySQLCharacterSet, SelMySQLIdentifier, SelUnquotedIdentifierLower, SelOpenCurlyBracket, SelOpenCurlyBracket2, SelOpenCurlyBracket3, SelPipe, SelBitOR, SelCloseCurlyBracket, SelTilde, SelE,
  BindVariable,
  Colon,
  Comment,
  Intger, IntgerL, IntgerE,
  MLComment, MLCommentL, MLComment2,
  MySQLCharacterSet, MySQLCharacterSetL, MySQLCharacterSetLE, MySQLCharacterSetE,
  MySQLCondCode, MySQLCondCodeL, MySQLCondCodeE,
  Numeric, NumericL, NumericExp, NumericE, NumericDot, NumericLE,
  QuotedIdentifier, QuotedIdentifier2,
  Return, ReturnE,
  Separator,
  UnquotedIdentifier, UnquotedIdentifierLE, UnquotedIdentifierLabel,
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
    SelMySQLIdentifier:
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
    QuotedIdentifier2:
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
      JE UnquotedIdentifierLabel
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
    UnquotedIdentifierLabel:
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

function TCustomSQLParser.ParseUnknownStmt(): TNodeOffset;
var
  Token: TNodeOffset;
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

function TCustomSQLParser.ParseUser(): TNodeOffset;
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

function TCustomSQLParser.ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const ParseValue: TParseFunction): TNodeOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (CurrentToken = 0) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex) then
    SetError(PE_UnexpectedToken)
  else
    Nodes.KeywordToken := ApplyCurrentToken();

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
    Nodes.ValueNode := ParseValue();

  Result := TValue.Create(Self, Nodes);
end;

function TCustomSQLParser.ParseWhileStmt(): TNodeOffset;
var
  Nodes: TWhileStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(CurrentToken)^.TokenType = ttBeginLabel) then
    Nodes.BeginLabelToken := ApplyCurrentToken();

  if (not Error) then
    Nodes.WhileTag := ParseTag(kiWHILE);

  if (not Error) then
    Nodes.SearchConditionExpression := ParseExpression();

  if (not Error) then
    Nodes.DoTag := ParseTag(kiDO);

  if (not Error) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter, kiEND);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiWHILE);

  if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttIdentifier)) then
    if ((Nodes.BeginLabelToken = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(Nodes.BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken, CurrentToken)
    else
      Nodes.EndLabelToken := ApplyCurrentToken(utLabel, ttEndLabel);

  Result := TWhileStmt.Create(Self, Nodes);
end;

function TCustomSQLParser.RangeNodePtr(const ANode: TNodeOffset): PRangeNode;
begin
  Assert(IsRangeNode(NodePtr(ANode)));

  Result := PRangeNode(NodePtr(ANode));
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
          while (IsStmtNode(Node) and (G > Generation)) do
          begin
            Dec(G);
            if (G > Generation) then
              Node := PStmtNode(Node)^.ParentNode;
          end;

          if (IsStmtNode(Node) and (G = Generation) and (ParentNodes.IndexOf(Node) < 1)) then
          begin
            if (PStmtNode(Node)^.FirstToken^.Index - LastTokenIndex - 1 > 0) then
              HTML := HTML
                + '<td colspan="' + IntToStr(PStmtNode(Node)^.FirstToken^.Index - LastTokenIndex - 1) + '"></td>';
            HTML := HTML
              + '<td colspan="' + IntToStr(PStmtNode(Node)^.LastToken^.Index - PStmtNode(Node)^.FirstToken^.Index + 1) + '" class="Node">';
            HTML := HTML
              + '<a href="">'
              + HTMLEscape(NodeTypeToString[Node^.NodeType])
              + '<span><table cellspacing="2" cellpadding="0">';
            if (Assigned(PStmtNode(Node)^.ParentNode)) then
              HTML := HTML
                + '<tr><td>ParentNode Offset:</td><td>&nbsp;</td><td>' + IntToStr(PStmtNode(Node)^.ParentNode^.Offset) + '</td></tr>';
            HTML := HTML
              + '<tr><td>Offset:</td><td>&nbsp;</td><td>' + IntToStr(Node^.Offset) + '</td></tr>';
            if (IsStmt(Node)) then
            begin
              HTML := HTML + '<tr><td>StmtType:</td><td>&nbsp;</td><td>' + StmtTypeToString[PStmt(Node)^.StmtType] + '</td></tr>';
            end;
            if (Assigned(PNode(PStmtNode(Node)^.NextSibling))) then
              HTML := HTML
                + '<tr><td>NextSibling:</td><td>&nbsp;</td><td>' + IntToStr((PNode(PStmtNode(Node)^.NextSibling)^.Offset)) + '</td></tr>';
            case (Node^.NodeType) of
              ntDbIdentifier:
                HTML := HTML
                  + '<tr><td>DbIdentifierType:</td><td>&nbsp;</td><td>' + DbIdentifierTypeToString[PDbIdentifier(Node)^.DbIdentifierType] + '</td></tr>';
              ntBinaryOp:
                if (IsToken(PNode(PBinaryOp(Node)^.Operator))) then
                  HTML := HTML
                    + '<tr><td>OperatorType:</td><td>&nbsp;</td><td>' + OperatorTypeToString[PToken(PBinaryOp(Node)^.Operator)^.OperatorType] + '</td></tr>';
            end;
            HTML := HTML
              + '</table></span>'
              + '</a></td>' + #13#10;

            LastTokenIndex := PStmtNode(Node)^.LastToken^.Index;

            ParentNodes.Add(Node);
            Token := PStmtNode(Node)^.LastToken;
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
      if (not Stmt^.Error and Assigned(PStmtNode(Token)^.ParentNode)) then
        HTML := HTML + '<tr><td>ParentNode Offset:</td><td>&nbsp;</td><td>' + IntToStr(PStmtNode(Token)^.ParentNode^.Offset) + '</td></tr>';
      HTML := HTML + '<tr><td>Offset:</td><td>&nbsp;</td><td>' + IntToStr(PNode(Token)^.Offset) + '</td></tr>';
      HTML := HTML + '<tr><td>TokenType:</td><td>&nbsp;</td><td>' + HTMLEscape(TokenTypeToString[Token^.TokenType]) + '</td></tr>';
      if (Token^.KeywordIndex >= 0) then
        HTML := HTML + '<tr><td>KeywordIndex:</td><td>&nbsp;</td><td>ki' + HTMLEscape(FKeywords[Token^.KeywordIndex]) + '</td></tr>';
      if (Token^.OperatorType <> otUnknown) then
        HTML := HTML + '<tr><td>OperatorType:</td><td>&nbsp;</td><td>' + HTMLEscape(OperatorTypeToString[Token^.OperatorType]) + '</td></tr>';
      if (Token^.DbIdentifierType <> ditUnknown) then
        HTML := HTML + '<tr><td>DbIdentifierType:</td><td>&nbsp;</td><td>' + HTMLEscape(DbIdentifierTypeToString[Token^.DbIdentifierType]) + '</td></tr>';
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

procedure TCustomSQLParser.SetError(const AErrorCode: Integer; const AErrorNode: TNodeOffset = 0);
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
    kiJOIN                := IndexOf('JOIN');
    kiKEY                 := IndexOf('KEY');
    kiLANGUAGE            := IndexOf('LANGUAGE');
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

function TCustomSQLParser.StmtNodePtr(const ANode: TNodeOffset): PStmtNode;
begin
  Assert(IsStmtNode(NodePtr(ANode)));

  Result := @FNodes.Mem[ANode];
end;

function TCustomSQLParser.StmtPtr(const ANode: TNodeOffset): PStmt;
begin
  Assert(IsStmtNode(NodePtr(ANode)));

  Result := @FNodes.Mem[ANode];
end;

function TCustomSQLParser.TokenPtr(const ANode: TNodeOffset): PToken;
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
