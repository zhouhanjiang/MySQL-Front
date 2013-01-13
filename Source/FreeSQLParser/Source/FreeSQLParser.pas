unit FreeSQLParser;

interface {********************************************************************}

uses
  Classes,
  SQLUtils,
  fspTypes, fspConst;

type
  TCustomSQLParser = class
  protected
    type
      ONode = Integer;

      TOrigin = packed record Pos, Char, Line: Integer; end;

      TWordList = class(TObject)
      private
        FCount: Integer;
        FIndex: array of PChar;
        FFirst: array of Integer;
        FParser: TCustomSQLParser;
        FText: string;
        function GetWord(Index: Integer): string;
        procedure SetText(AText: string);
      protected
        procedure Clear();
        property Parser: TCustomSQLParser read FParser;
      public
        constructor Create(const ASQLParser: TCustomSQLParser; const AText: string = '');
        destructor Destroy(); override;
        function IndexOf(const Word: PChar; const Length: Integer): Integer;
        property Count: Integer read FCount;
        property Text: string read FText write SetText;
        property Word[Index: Integer]: string read GetWord; default;
      end;

  public
    type
      PNode = ^TNode;
      PStmtNode = ^TStmtNode;
      PRangeNode = ^TRangeNode;
      PRoot = ^TRoot;
      PToken = ^TToken;
      PSibling = ^TSibling;
      PSiblings = ^TSiblings;
      PStmt = ^TStmt;
      PExpressions = ^TValues;
      PDbIdentifier = ^TDbIdentifier;
      PFunction = ^TFunction;
      PUnaryOperation = ^TUnaryOperation;
      PBinaryOperation = ^TBinaryOperation;
      PBetweenOperation = ^TBetweenOperation;
      PCaseCond = ^TCaseCond;
      PCaseOp = ^TCaseOp;
      PSoundsLikeOperation = ^TSoundsLikeOperation;

      TParseFunction = function(): ONode of object;

      TNode = packed record
      private
        FNodeType: TNodeType;
        FParser: TCustomSQLParser;
      private
        class function Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): ONode; static;
        function GetOffset(): ONode; {$IFNDEF Debug} inline; {$ENDIF}
        property Offset: Integer read GetOffset;
      public
        property NodeType: TNodeType read FNodeType;
        property Parser: TCustomSQLParser read FParser;
      end;

      TStmtNode = packed record
      private // Virtual for TRangeNode / TToken
        Heritage: TNode;
        FParentNode: ONode;
      private
        function GetFFirstToken(): ONode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFLastToken(): ONode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        property FFirstToken: ONode read GetFFirstToken;
        property FLastToken: ONode read GetFLastToken;
      public
        property FirstToken: PToken read GetFirstToken;
        property LastToken: PToken read GetLastToken;
        property NodeType: TNodeType read Heritage.FNodeType;
        property ParentNode: PNode read GetParentNode;
        property Parser: TCustomSQLParser read Heritage.FParser;
      end;

      TToken = packed record
      private
        Heritage: TStmtNode;
      private
        FErrorCode: Integer;
        FKeywordIndex: Integer;
        FMySQLVersion: Integer;
        FOperatorType: TOperatorType;
        FOrigin: TOrigin;
        FPriorToken: ONode;
        FText: packed record
          SQL: PChar;
          Length: Integer;
          NewText: string;
        end;
        FTokenType: TTokenType;
        FUsageType: TUsageType;
        class function Create(const AParser: TCustomSQLParser;
          const ASQL: PChar; const ALength: Integer; const AOrigin: TOrigin;
          const AErrorCode: Integer; const AMySQLVersion: Integer; const ATokenType: TTokenType;
          const AOperatorType: TOperatorType; const AKeywordIndex: Integer): ONode; static;
        function GetAsString(): string;
        function GetDbIdentifierType(): TDbIdentifierType;
        function GetErrorMessage(): string;
        function GetGeneration(): Integer;
        function GetIndex(): Integer;
        function GetIsUsed(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
        function GetNextToken(): PToken;
        function GetOffset(): ONode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetText(): string;
        procedure SetText(AText: string);
        property FParentNode: ONode read Heritage.FParentNode write Heritage.FParentNode;
        property Generation: Integer read GetGeneration;
        property Index: Integer read GetIndex;
        property Offset: Integer read GetOffset;
      public
        property AsString: string read GetAsString;
        property DbIdentifierType: TDbIdentifierType read GetDbIdentifierType;
        property ErrorCode: Integer read FErrorCode;
        property ErrorMessage: string read GetErrorMessage;
        property IsUsed: Boolean read GetIsUsed;
        property KeywordIndex: Integer read FKeywordIndex;
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

      TRangeNode = packed record
      private
        Heritage: TStmtNode;
        FFirstToken: ONode;
        FLastToken: ONode;
        property FParentNode: ONode read Heritage.FParentNode write Heritage.FParentNode;
      private
        class function Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): ONode; static;
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOffset(): ONode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        procedure AddChild(const ChildNode: ONode);
        property Offset: ONode read GetOffset;
      public
        property FirstToken: PToken read GetFirstToken;
        property LastToken: PToken read GetLastToken;
        property NodeType: TNodeType read Heritage.Heritage.FNodeType;
        property Parser: TCustomSQLParser read Heritage.Heritage.FParser;
        property ParentNode: PNode read GetParentNode;
      end;

      TSibling = record
      private
        Heritage: TRangeNode;
        FNextSibling: ONode;
      private
        class function Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): ONode; static;
        function GetNextSibling(): PSibling; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property NextSibling: PSibling read GetNextSibling;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      TSiblings = record
      private
        Heritage: TRangeNode;
        FFirstSibling: ONode;
      private
        procedure AddSibling(const ASibling: ONode);
        class function Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): ONode; static;
        function GetFirstChild(): PSibling; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property FirstChild: PSibling read GetFirstChild;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      TRoot = packed record
      private
        Heritage: TRangeNode;
        property FFirstToken: ONode read Heritage.FFirstToken write Heritage.FFirstToken;
        property FLastToken: ONode read Heritage.FLastToken write Heritage.FLastToken;
      private
        FFirstStmt: ONode;
        FLastStmt: ONode;
        class function Create(const AParser: TCustomSQLParser): ONode; static;
        function GetFirstStmt(): PStmt; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastStmt(): PStmt; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property FirstStmt: PStmt read GetFirstStmt;
        property FirstToken: PToken read GetFirstToken;
        property LastStmt: PStmt read GetLastStmt;
        property LastToken: PToken read GetLastToken;
        property NodeType: TNodeType read Heritage.Heritage.Heritage.FNodeType;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      TValues = packed record
      private
        Heritage: TSiblings;
      public
        class function Create(const AParser: TCustomSQLParser): ONode; static; {$IFNDEF Debug} inline; {$ENDIF}
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      TDbIdentifier = packed record
      private
        Heritage: TRangeNode;
        FPrefix1: ONode;
        FPrefix2: ONode;
        FDbIdentifierType: TDbIdentifierType;
        FIdentifier: ONode;
        procedure AddPrefix(const APrefix, ADot: ONode);
        function GetIdentifier(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetPrefix1(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetPrefix2(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
      public
        class function Create(const AParser: TCustomSQLParser; const AIdentifier: ONode; const ADbIdentifierType: TDbIdentifierType = ditUnknown): ONode; static;
        property DbIdentifierType: TDbIdentifierType read FDbIdentifierType;
        property Identifier: PToken read GetIdentifier;
        property LastToken: PToken read GetLastToken;
        property ParentNode: PNode read GetParentNode;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        property Prefix1: PToken read GetPrefix1;
        property Prefix2: PToken read GetPrefix2;
      end;

      TFunction = packed record
      private
        Heritage: TRangeNode;
      private
        FArguments: ONode;
        FIdentifier: ONode;
        function GetArguments(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetIdentifier(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
      public
        class function Create(const AParser: TCustomSQLParser; const AIdentifier, AArguments: ONode): ONode; static;
        property Arguments: PStmtNode read GetArguments;
        property Identifier: PStmtNode read GetIdentifier;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      TUnaryOperation = packed record
      private
        Heritage: TRangeNode;
      private
        FOperand: ONode;
        FOperator: ONode;
        function GetOperand(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
      public
        class function Create(const AParser: TCustomSQLParser; const AOperator, AOperand: ONode): ONode; static;
        property Operand: PStmtNode read GetOperand;
        property Operator: PStmtNode read GetOperator;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      TBinaryOperation = packed record
      private
        Heritage: TRangeNode;
      private
        FOperand1: ONode;
        FOperand2: ONode;
        FOperator: ONode;
        function GetOperand1(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperand2(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
      public
        class function Create(const AParser: TCustomSQLParser; const AOperator, AOperand1, AOperand2: ONode): ONode; static;
        property Operand1: PStmtNode read GetOperand1;
        property Operand2: PStmtNode read GetOperand2;
        property Operator: PStmtNode read GetOperator;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      TBetweenOperation = packed record
      private
        Heritage: TRangeNode;
      private
        FExpr: ONode;
        FMax: ONode;
        FMin: ONode;
        FOperator1: ONode;
        FOperator2: ONode;
        function GetExpr(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetMax(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetMin(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator1(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator2(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
      public
        class function Create(const AParser: TCustomSQLParser; const AOperator1, AOperator2: ONode; const AExpr, AMin, AMax: ONode): ONode; static;
        property Expr: PStmtNode read GetExpr;
        property Max: PStmtNode read GetMax;
        property Min: PStmtNode read GetMin;
        property Operator1: PToken read GetOperator1;
        property Operator2: PToken read GetOperator2;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      TCaseCond = packed record
      private
        Heritage: TSibling;
      private
        FConditionValue: ONode;
        FResultValue: ONode;
        function GetConditionValue(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetNextCond(): PCaseCond; {$IFNDEF Debug} inline; {$ENDIF}
        function GetResultValue(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
      public
        class function Create(const AParser: TCustomSQLParser; const AConditionValue, AResultValue: ONode): ONode; static;
        property ConditionValue: PStmtNode read GetConditionValue;
        property NextCond: PCaseCond read GetNextCond;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
        property ResultValue: PStmtNode read GetResultValue;
      end;

      TCaseOp = packed record
      private
        Heritage: TSiblings;
      private
        FElseValue: ONode;
        FReferenceValue: ONode;
        procedure AddCondition(const AConditionValue, AResultValue: ONode);
        function GetFirstCond(): PCaseCond; {$IFNDEF Debug} inline; {$ENDIF}
        function GetReferenceValue(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        procedure SetElse(const AElseValue: ONode);
      public
        class function Create(const AParser: TCustomSQLParser; const AReferenceValue: ONode): ONode; static;
        property FirstCond: PCaseCond read GetFirstCond;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
        property ReferenceValue: PStmtNode read GetReferenceValue;
      end;

      TSoundsLikeOperation = packed record
      private
        Heritage: TRangeNode;
      private
        FOperand1: ONode;
        FOperand2: ONode;
        FOperator1: ONode;
        FOperator2: ONode;
        function GetOperand1(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperand2(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator1(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator2(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
      public
        class function Create(const AParser: TCustomSQLParser; const AOperator1, AOperator2: ONode; const AOperand1, AOperand2: ONode): ONode; static;
        property Operand1: PStmtNode read GetOperand1;
        property Operand2: PStmtNode read GetOperand2;
        property Operator1: PToken read GetOperator1;
        property Operator2: PToken read GetOperator2;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      TStmt = packed record
      private
        Heritage: TRangeNode;
        FStmtType: TStmtType;
        FErrorCode: Integer;
        FErrorToken: ONode;
        property FFirstToken: ONode read Heritage.FFirstToken write Heritage.FFirstToken;
        property FLastToken: ONode read Heritage.FLastToken write Heritage.FLastToken;
        property FParentNode: ONode read Heritage.Heritage.FParentNode write Heritage.Heritage.FParentNode;
      private
        class function Create(const AParser: TCustomSQLParser; const AStmtType: TStmtType): ONode; static;
        function GetError(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
        function GetErrorMessage(): string;
        function GetErrorToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Error: Boolean read GetError;
        property ErrorCode: Integer read FErrorCode;
        property ErrorMessage: string read GetErrorMessage;
        property ErrorToken: PToken read GetErrorToken;
        property FirstToken: PToken read GetFirstToken;
        property LastToken: PToken read GetLastToken;
        property NodeType: TNodeType read Heritage.Heritage.Heritage.FNodeType;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
        property StmtType: TStmtType read FStmtType;
      end;

      PSelectStmt = ^TSelectStmt;
      TSelectStmt = packed record
      private
        Heritage: TStmt;
      private
        type
          PColumns = ^TColumns;

          PColumn = ^TColumn;
          TColumn = packed record
          private
            Heritage: TRangeNode;
            property FParentNode: ONode read Heritage.Heritage.FParentNode write Heritage.Heritage.FParentNode;
          private
            FAlias: ONode;
            FExpression: ONode;
            function GetAlias(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
            function GetColumns(): PColumns; {$IFNDEF Debug} inline; {$ENDIF}
            function GetDisplayName(): string;
            function GetExpression(): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
          public
            class function Create(const AParser: TCustomSQLParser; const AExpression, AAlias: ONode): ONode; static;
            property Alias: PToken read GetAlias;
            property Columns: PColumns read GetColumns;
            property DisplayName: string read GetDisplayName;
            property Expression: PStmtNode read GetExpression;
            property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.FParser;
          end;

        TColumns = packed record
        private
          Heritage: TSiblings;
        public
          class function Create(const AParser: TCustomSQLParser): ONode; static;
          property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
        end;

      private
        FColumns: ONode;
        class function Create(const AParser: TCustomSQLParser; const AColumns: ONode): ONode; static;
        function GetColumns(): PColumns; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Columns: PColumns read GetColumns;
        property Parser: TCustomSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

//      TCompoundStmt = packed record
//      private
//        FNodeType: TNodeType;
//        FParser: TCustomSQLParser;
//        FParentNode: ONode;
//        FFirstToken: TNodeOffset;
//        FLastToken: TNodeOffset;
//        FStmtType: TStmtType;
//        FErrorCode: Integer;
//        FErrorToken: TNodeOffset;
//      public
//        constructor Create(const AStmtList: PStmtList: TToken);
//      end;

  private
    FErrorCode: Integer;
    FErrorToken: ONode;
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
    FParsedTokens: TList;
    FParsePos: packed record Text: PChar; Length: Integer; Origin: TOrigin; end;
    FParseStmts: Boolean;
    FPipesAsConcat: Boolean;
    FRoot: ONode;
    FSQLDialect: TSQLDialect;
    function GetCurrentToken(): ONode; {$IFNDEF Debug} inline; {$ENDIF}
    function GetErrorMessage(const AErrorCode: Integer): string;
    function GetFunctions(): string; {$IFNDEF Debug} inline; {$ENDIF}
    function GetKeywords(): string; {$IFNDEF Debug} inline; {$ENDIF}
    function GetNextToken(Index: Integer): ONode; {$IFNDEF Debug} inline; {$ENDIF}
    function GetParsedToken(Index: Integer): ONode;
    function GetRoot(): PRoot; {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetFunctions(AFunctions: string); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetKeywords(AKeywords: string);

  protected
    kiAND,
    kiAS,
    kiBINARY,
    kiBEGIN,
    kiBETWEEN,
    kiCASE,
    kiCOLLATE,
    kiDIV,
    kiDO,
    kiELSE,
    kiEND,
    kiFROM,
    kiIN,
    kiINTERVAL,
    kiIS,
    kiLIKE,
    kiLOOP,
    kiMOD,
    kiNOT,
    kiNULL,
    kiOR,
    kiREGEXP,
    kiREPEAT,
    kiRLIKE,
    kiSELECT,
    kiSOUNDS,
    kiTHEN,
    kiUNTIL,
    kiWHEN,
    kiWHILE,
    kiXOR: Integer;

    OperatorTypeByKeywordIndex: array of TOperatorType;

    procedure ApplyCurrentToken();
    function GetError(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsRangeNode(const ANode: PNode): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsStmt(const ANode: PNode): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsStmtNode(const ANode: PNode): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsSiblings(const ANode: PNode): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function IsToken(const ANode: ONode): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsToken(const ANode: PNode): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function NewNode(const ANodeType: TNodeType): ONode;
    function NodePtr(const ANode: ONode): PNode; {$IFNDEF Debug} inline; {$ENDIF}
    function NodeSize(const ANodeType: TNodeType): Integer;
    function ParseCaseOp(): ONode;
    function ParseColumn(): ONode;
    function ParseCompoundStmt(): ONode;
    function ParseFunction(): ONode;
    function ParseSelectStmt(): ONode;
    function ParseSiblings(const ANodeType: TNodeType; const ParseSibling: TParseFunction): ONode;
    function ParseSubArea(const ANodeTypes: TNodeTypes): ONode;
    function ParseStmt(const AParentNode: ONode): ONode;
    function ParseToken(): ONode;
    function ParseUnknownStmt(): ONode;
    function ParseValue(): ONode;
    function RangeNodePtr(const ANode: ONode): PRangeNode; {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetError(const AErrorCode: Integer; const AErrorNode: ONode = 0);
    function SiblingPtr(const ANode: ONode): PSibling; {$IFNDEF Debug} inline; {$ENDIF}
    function SiblingsPtr(const ANode: ONode): PSiblings; {$IFNDEF Debug} inline; {$ENDIF}
    function StmtNodePtr(const ANode: ONode): PStmtNode; {$IFNDEF Debug} inline; {$ENDIF}
    function StmtPtr(const ANode: ONode): PStmt; {$IFNDEF Debug} inline; {$ENDIF}
    function TokenPtr(const ANode: ONode): PToken; {$IFNDEF Debug} inline; {$ENDIF}

    property CurrentToken: ONode read GetCurrentToken;
    property Error: Boolean read GetError;
    property ParsedText: string read FParsedText;
    property NextToken[Index: Integer]: ONode read GetNextToken;

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
    property ParseStmts: Boolean read FParseStmts write FParseStmts;
    property PipesAsConcat: Boolean read FPipesAsConcat write FPipesAsConcat;
    property SQLDialect: TSQLDialect read FSQLDialect;
    property Text: string read FParsedText;
  end;

  TMySQLSQLParser = class(TCustomSQLParser)
  private
    FAnsiQuotes: Boolean;
    FLowerCaseTableNames: Boolean;
    FMySQLVersion: Integer;
  public
    constructor Create(const MySQLVersion: Integer = 0; const LowerCaseTableNames: Boolean = False);
    property AnsiQuotes: Boolean read FAnsiQuotes write FAnsiQuotes;
    property LowerCaseTableNames: Boolean read FLowerCaseTableNames write FLowerCaseTableNames;
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

function TCustomSQLParser.TWordList.GetWord(Index: Integer): string;
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

class function TCustomSQLParser.TNode.Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): ONode;
begin
  Result := AParser.NewNode(ANodeType);

  AParser.NodePtr(Result)^.FParser := AParser;
  AParser.NodePtr(Result)^.FNodeType := ANodeType;
end;

function TCustomSQLParser.TNode.GetOffset(): ONode;
begin
  Result := @Self - Parser.FNodes.Mem;
end;

{ TCustomSQLParser.TStmtNode **************************************************}

function TCustomSQLParser.TStmtNode.GetFFirstToken(): ONode;
begin
  if (NodeType = ntToken) then
    Result := @Self - Parser.FNodes.Mem
  else
  begin
    Assert(Parser.IsRangeNode(@Self));
    Result := TCustomSQLParser.PRangeNode(@Self).FFirstToken;
  end;
end;

function TCustomSQLParser.TStmtNode.GetFirstToken(): PToken;
begin
  if (NodeType = ntToken) then
    Result := @Self
  else
  begin
    Assert(Parser.IsRangeNode(@Self));
    Result := PRangeNode(@Self).FirstToken;
  end;
end;

function TCustomSQLParser.TStmtNode.GetFLastToken(): ONode;
begin
  if (NodeType = ntToken) then
    Result := PNode(@Self)^.Offset
  else
  begin
    Assert(Parser.IsRangeNode(@Self));
    Result := PRangeNode(@Self)^.FLastToken;
  end;
end;

function TCustomSQLParser.TStmtNode.GetLastToken(): PToken;
begin
  if (NodeType = ntToken) then
    Result := @Self
  else
  begin
    Assert(Parser.IsRangeNode(@Self));
    Result := PRangeNode(@Self).LastToken;
  end;
end;

function TCustomSQLParser.TStmtNode.GetParentNode(): PNode;
begin
  Result := Parser.NodePtr(FParentNode);
end;

{ TCustomSQLParser.TToken *****************************************************}

class function TCustomSQLParser.TToken.Create(const AParser: TCustomSQLParser;
  const ASQL: PChar; const ALength: Integer; const AOrigin: TOrigin;
  const AErrorCode: Integer; const AMySQLVersion: Integer; const ATokenType: fspTypes.TTokenType;
  const AOperatorType: TOperatorType; const AKeywordIndex: Integer): ONode;
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
    ttMySQLCharacterSet:
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
      ditField: Result := ditTable;
      else raise ERangeError.Create(SArgumentOutOfRange);
    end
  else if (@Self = PDbIdentifier(Parser.NodePtr(FParentNode))^.Prefix2) then
    case (PDbIdentifier(Parser.NodePtr(FParentNode))^.DbIdentifierType) of
      ditUnknown: Result := ditUnknown;
      ditField: Result := ditDatabase;
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
  Offset: ONode;
begin
  Offset := PNode(@Self)^.Offset;
  repeat
    Inc(Offset, Parser.NodeSize(Parser.NodePtr(Offset)^.NodeType));
  until ((Offset = Parser.FNodes.Offset) or (Parser.NodePtr(Offset)^.NodeType = ntToken));
  if (Offset = Parser.FNodes.Offset) then
    Result := nil
  else
    Result := PToken(Parser.NodePtr(Offset));
end;

function TCustomSQLParser.TToken.GetOffset(): ONode;
begin
  Result := Heritage.Heritage.GetOffset();
end;

function TCustomSQLParser.TToken.GetParentNode(): PNode;
begin
  Result := Heritage.GetParentNode();
end;

function TCustomSQLParser.TToken.GetText(): string;
begin
  if (FText.NewText = '') then
    SetString(Result, FText.SQL, FText.Length)
  else
    Result := FText.NewText;
end;

procedure TCustomSQLParser.TToken.SetText(AText: string);
begin
  FText.NewText := AText;
end;

{ TCustomSQLParser.TRangeNode *************************************************}

class function TCustomSQLParser.TRangeNode.Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): ONode;
begin
  Result := TNode.Create(AParser, ANodeType);
end;

function TCustomSQLParser.TRangeNode.GetFirstToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FFirstToken));
end;

function TCustomSQLParser.TRangeNode.GetLastToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FLastToken));
end;

function TCustomSQLParser.TRangeNode.GetOffset(): ONode;
begin
  Result := Heritage.Heritage.Offset;
end;

function TCustomSQLParser.TRangeNode.GetParentNode(): PNode;
begin
  Result := PNode(Parser.NodePtr(FParentNode));
end;

procedure TCustomSQLParser.TRangeNode.AddChild(const ChildNode: ONode);
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

{ TCustomSQLParser.TSibling ***************************************************}

class function TCustomSQLParser.TSibling.Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): ONode;
begin
  Result := TRangeNode.Create(AParser, ANodeType);
end;

function TCustomSQLParser.TSibling.GetNextSibling(): PSibling;
begin
  Result := Parser.SiblingPtr(FNextSibling);
end;

{ TCustomSQLParser.TSiblings **************************************************}

procedure TCustomSQLParser.TSiblings.AddSibling(const ASibling: ONode);
var
  LastSibling: ONode;
begin
  if (FFirstSibling = 0) then
    FFirstSibling := ASibling
  else
  begin
    LastSibling := FFirstSibling;
    while (Parser.SiblingPtr(LastSibling)^.FNextSibling > 0) do
      LastSibling := Parser.SiblingPtr(LastSibling)^.FNextSibling;
    Parser.SiblingPtr(LastSibling)^.FNextSibling := ASibling;
  end;

  Heritage.AddChild(ASibling);
end;

class function TCustomSQLParser.TSiblings.Create(const AParser: TCustomSQLParser; const ANodeType: TNodeType): ONode;
begin
  Result := TRangeNode.Create(AParser, ANodeType);
end;

function TCustomSQLParser.TSiblings.GetFirstChild(): PSibling;
begin
  Result := PSibling(Parser.NodePtr(FFirstSibling));
end;

{ TCustomSQLParser.TRoot ******************************************************}

class function TCustomSQLParser.TRoot.Create(const AParser: TCustomSQLParser): ONode;
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

{ TCustomSQLParser.TExpressions ***********************************************}

class function TCustomSQLParser.TValues.Create(const AParser: TCustomSQLParser): ONode;
begin
  Result := TSiblings.Create(AParser, ntValues);
end;

{ TCustomSQLParser.TDbIdentifier **********************************************}

procedure TCustomSQLParser.TDbIdentifier.AddPrefix(const APrefix, ADot: ONode);
var
  Node: PNode;
begin
  if (FPrefix1 = 0) then
  begin
    FPrefix1 := APrefix;

    Heritage.AddChild(APrefix);
    Heritage.AddChild(ADot);
  end
  else if (FPrefix2 = 0) then
  begin
    FPrefix2 := APrefix;

    Heritage.AddChild(APrefix);
    Heritage.AddChild(ADot);
  end
  else
    raise ERangeError.Create(SArgumentOutOfRange);

  Node := ParentNode;
  while (Parser.IsRangeNode(Node)) do
  begin
    if (PRangeNode(Node)^.FFirstToken > Heritage.FFirstToken) then
      PRangeNode(Node)^.FFirstToken := Heritage.FFirstToken;
    Node := PRangeNode(ParentNode)^.ParentNode;
  end;
end;

class function TCustomSQLParser.TDbIdentifier.Create(const AParser: TCustomSQLParser; const AIdentifier: ONode; const ADbIdentifierType: TDbIdentifierType = ditUnknown): ONode;
begin
  Result := TRangeNode.Create(AParser, ntDbIdentifier);

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

class function TCustomSQLParser.TFunction.Create(const AParser: TCustomSQLParser; const AIdentifier, AArguments: ONode): ONode;
var
  Token: PToken;
begin
  Result := TRangeNode.Create(AParser, ntFunction);

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

class function TCustomSQLParser.TUnaryOperation.Create(const AParser: TCustomSQLParser; const AOperator, AOperand: ONode): ONode;
begin
  Result := TRangeNode.Create(AParser, ntUnaryOp);

  with PUnaryOperation(AParser.NodePtr(Result))^ do
  begin
    FOperator := AOperator;
    FOperand := AOperand;

    Heritage.AddChild(AOperator);
    Heritage.AddChild(AOperand);
  end;
end;

function TCustomSQLParser.TUnaryOperation.GetOperand(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FOperand);
end;

function TCustomSQLParser.TUnaryOperation.GetOperator(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FOperator);
end;

{ TCustomSQLParser.TBinaryOperation *******************************************}

class function TCustomSQLParser.TBinaryOperation.Create(const AParser: TCustomSQLParser; const AOperator, AOperand1, AOperand2: ONode): ONode;
begin
  Result := TRangeNode.Create(AParser, ntBinaryOp);

  with PBinaryOperation(AParser.NodePtr(Result))^ do
  begin
    FOperator := AOperator;
    FOperand1 := AOperand1;
    FOperand2 := AOperand2;

    Heritage.AddChild(AOperator);
    Heritage.AddChild(AOperand1);
    Heritage.AddChild(AOperand2);
  end;
end;

function TCustomSQLParser.TBinaryOperation.GetOperand1(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FOperand1);
end;

function TCustomSQLParser.TBinaryOperation.GetOperand2(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FOperand2);
end;

function TCustomSQLParser.TBinaryOperation.GetOperator(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FOperator);
end;

{ TCustomSQLParser.TBetweenOperation ******************************************}

class function TCustomSQLParser.TBetweenOperation.Create(const AParser: TCustomSQLParser; const AOperator1, AOperator2: ONode; const AExpr, AMin, AMax: ONode): ONode;
begin
  Result := TRangeNode.Create(AParser, ntBetweenOp);

  with PBetweenOperation(AParser.NodePtr(Result))^ do
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

function TCustomSQLParser.TBetweenOperation.GetExpr(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FExpr);
end;

function TCustomSQLParser.TBetweenOperation.GetMax(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FMax);
end;

function TCustomSQLParser.TBetweenOperation.GetMin(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FMin);
end;

function TCustomSQLParser.TBetweenOperation.GetOperator1(): PToken;
begin
  Result := Parser.TokenPtr(FOperator1);
end;

function TCustomSQLParser.TBetweenOperation.GetOperator2(): PToken;
begin
  Result := Parser.TokenPtr(FOperator2);
end;

{ TCustomSQLParser.TCaseCond **************************************************}

class function TCustomSQLParser.TCaseCond.Create(const AParser: TCustomSQLParser; const AConditionValue, AResultValue: ONode): ONode;
begin
  Result := TSibling.Create(AParser, ntCaseCond);

  with PCaseCond(AParser.NodePtr(Result))^ do
  begin
    FConditionValue := AConditionValue;
    FResultValue := AResultValue;

    Heritage.Heritage.AddChild(AConditionValue);
    Heritage.Heritage.AddChild(AResultValue);
  end;
end;

function TCustomSQLParser.TCaseCond.GetConditionValue(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FConditionValue);
end;

function TCustomSQLParser.TCaseCond.GetNextCond(): PCaseCond;
begin
  Result := PCaseCond(Heritage.NextSibling);
end;

function TCustomSQLParser.TCaseCond.GetResultValue(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FResultValue);
end;

{ TCustomSQLParser.TCase ******************************************************}

procedure TCustomSQLParser.TCaseOp.AddCondition(const AConditionValue, AResultValue: ONode);
begin
  Heritage.AddSibling(TCaseCond.Create(Parser, AConditionValue, AResultValue));
end;

class function TCustomSQLParser.TCaseOp.Create(const AParser: TCustomSQLParser; const AReferenceValue: ONode): ONode;
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

procedure TCustomSQLParser.TCaseOp.SetElse(const AElseValue: ONode);
begin
  FElseValue := AElseValue;

  Heritage.Heritage.AddChild(AElseValue);
end;

{ TCustomSQLParser.TSoundsLikeOperation ***************************************}

class function TCustomSQLParser.TSoundsLikeOperation.Create(const AParser: TCustomSQLParser; const AOperator1, AOperator2: ONode; const AOperand1, AOperand2: ONode): ONode;
begin
  Result := TRangeNode.Create(AParser, ntSoundsLikeOp);

  with PSoundsLikeOperation(AParser.NodePtr(Result))^ do
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

function TCustomSQLParser.TSoundsLikeOperation.GetOperand1(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FOperator1);
end;

function TCustomSQLParser.TSoundsLikeOperation.GetOperand2(): PStmtNode;
begin
  Result := Parser.StmtNodePtr(FOperator2);
end;

function TCustomSQLParser.TSoundsLikeOperation.GetOperator1(): PToken;
begin
  Result := Parser.TokenPtr(FOperator1);
end;

function TCustomSQLParser.TSoundsLikeOperation.GetOperator2(): PToken;
begin
  Result := Parser.TokenPtr(FOperator2);
end;

{ TCustomSQLParser.TStmt ******************************************************}

class function TCustomSQLParser.TStmt.Create(const AParser: TCustomSQLParser; const AStmtType: TStmtType): ONode;
var
  NodeType: TNodeType;
begin
  case (AStmtType) of
    stUnknown: NodeType := ntStmt;
    stSelect: NodeType := ntSelectStmt;
    else raise ERangeError.Create(SArgumentOutOfRange);
  end;
  Result := TRangeNode.Create(AParser, NodeType);

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

{ TCustomSQLParser.TSelectStmt.TColumn ****************************************}

class function TCustomSQLParser.TSelectStmt.TColumn.Create(const AParser: TCustomSQLParser; const AExpression, AAlias: ONode): ONode;
begin
  Result := TSibling.Create(AParser, ntColumn);

  with PColumn(AParser.NodePtr(Result))^ do
  begin
    FExpression := AExpression;
    FAlias := AAlias;

    Heritage.AddChild(AExpression);
    Heritage.AddChild(AAlias);
  end;
end;

function TCustomSQLParser.TSelectStmt.TColumn.GetAlias(): PToken;
begin
  Result := Parser.TokenPtr(FAlias);
end;

function TCustomSQLParser.TSelectStmt.TColumn.GetColumns(): PColumns;
begin
  Assert(Parser.StmtNodePtr(FParentNode)^.NodeType = ntColumns);
  Result := PColumns(Parser.NodePtr(FParentNode));
end;

function TCustomSQLParser.TSelectStmt.TColumn.GetDisplayName(): string;
begin

end;

function TCustomSQLParser.TSelectStmt.TColumn.GetExpression(): PStmtNode;
begin
  Assert(Parser.IsStmtNode(Parser.NodePtr(FExpression)));

  Result := PStmtNode(Parser.NodePtr(FExpression));
end;

{ TCustomSQLParser.TSelectStmt ************************************************}

class function TCustomSQLParser.TSelectStmt.TColumns.Create(const AParser: TCustomSQLParser): ONode;
begin
  Result := TSiblings.Create(AParser, ntColumns);
end;

{ TCustomSQLParser.TSelectStmt ************************************************}

class function TCustomSQLParser.TSelectStmt.Create(const AParser: TCustomSQLParser; const AColumns: ONode): ONode;
begin
  Result := TStmt.Create(AParser, stSelect);

  with PSelectStmt(AParser.NodePtr(Result))^ do
  begin
    FColumns := AColumns;

    Heritage.Heritage.AddChild(AColumns);
  end;
end;

function TCustomSQLParser.TSelectStmt.GetColumns(): PColumns;
begin
  Assert(Parser.NodePtr(FColumns)^.NodeType = ntColumns);

  Result := PColumns(Parser.NodePtr(FColumns));
end;

{ TCustomSQLParser ************************************************************}

procedure TCustomSQLParser.ApplyCurrentToken();
begin
  if (CurrentToken > 0) then
    FParsedTokens.Delete(0);
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
  FParsedTokens := TList.Create();
  FParseStmts := True;
  FPipesAsConcat := False;
  FSQLDialect := ASQLDialect;
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

function TCustomSQLParser.GetCurrentToken(): ONode;
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

function TCustomSQLParser.GetNextToken(Index: Integer): ONode;
begin
  Assert(Index > 0);

  Result := GetParsedToken(Index);
end;

function TCustomSQLParser.GetParsedToken(Index: Integer): ONode;
var
  Token: ONode;
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
    Result := ONode(FParsedTokens[Index]);
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
  Result := Assigned(ANode) and (ANode^.NodeType in [ntSelectStmt]);
end;

function TCustomSQLParser.IsStmtNode(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and not (ANode^.NodeType in [ntUnknown, ntRoot]);
end;

function TCustomSQLParser.IsSiblings(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType in [ntColumns]);
end;

function TCustomSQLParser.IsToken(const ANode: ONode): Boolean;
begin
  Result := IsToken(NodePtr(ANode));
end;

function TCustomSQLParser.IsToken(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType in [ntToken]);
end;

function TCustomSQLParser.NewNode(const ANodeType: TNodeType): ONode;
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
    FNodes.Size := FNodes.Size + AdditionalSize;
  end;

  Result := FNodes.Offset;

  Inc(FNodes.Offset, Size);
end;

function TCustomSQLParser.NodePtr(const ANode: ONode): PNode;
begin
  if (ANode = 0) then
    Result := nil
  else
    Result := @FNodes.Mem[ANode];
end;

function TCustomSQLParser.NodeSize(const ANodeType: TNodeType): Integer;
begin
  case (ANodeType) of
    ntRoot: Result := SizeOf(TRoot);
    ntToken: Result := SizeOf(TToken);
    ntRangeNode: Result := SizeOf(TRangeNode);
    ntValues: Result := SizeOf(TValues);
    ntColumns: Result := SizeOf(TSelectStmt.TColumns);
    ntColumn: Result := SizeOf(TSelectStmt.TColumn);
    ntDbIdentifier: Result := SizeOf(TDbIdentifier);
    ntFunction: Result := SizeOf(TFunction);
    ntUnaryOp: Result := SizeOf(TUnaryOperation);
    ntBinaryOp: Result := SizeOf(TBinaryOperation);
    ntStmt: Result := SizeOf(TStmt);
    ntSelectStmt: Result := SizeOf(TSelectStmt);
    else raise ERangeError.Create(SArgumentOutOfRange);
  end;
end;

function TCustomSQLParser.Parse(const Text: PChar; const Length: Integer): Boolean;
begin
  SetString(FParsedText, Text, Length);
  FParsePos.Text := PChar(ParsedText);
  FParsePos.Length := Length;
  FParsePos.Origin.Pos := 0;
  FParsePos.Origin.Char := 0;
  FParsePos.Origin.Line := 0;

  FNodes.Offset := 1;
  FNodes.Size := 1024 * 1024;
  ReallocMem(FNodes.Mem, FNodes.Size);
  FillChar(FNodes.Mem[0], FNodes.Size, #0);

  FRoot := TRoot.Create(Self);
  FMySQLVersion := -1;

  Result := True;

  if (FParsePos.Length > 0) then
  begin
    Root^.FFirstToken := CurrentToken;
    Result := Result and (Root^.FFirstToken > 0);
    if (Root^.FFirstToken > 0) then
    begin
      FErrorCode := PE_Success;
      FErrorToken := 0;
      if (not ParseStmts) then
        Result := Result and (Root^.FirstToken^.ErrorCode = PE_Success)
      else
      begin
        Root^.FFirstStmt := ParseStmt(FRoot);
        Result := Result and (Root^.FFirstStmt > 0) and (Root^.FirstStmt^.ErrorCode = PE_Success);
      end;
    end;
  end;

  while (FParsePos.Length > 0) do
  begin
    FErrorCode := PE_Success;
    FErrorToken := 0;
    Root^.FLastToken := CurrentToken; ApplyCurrentToken();
    Result := Result and (Root^.FLastToken > 0);
    if (not ParseStmts) then
      Result := Result and (Root^.LastToken^.ErrorCode = PE_Success)
    else
    begin
      Root^.FLastStmt := ParseStmt(FRoot);
      Result := Result and (Root^.FLastStmt > 0) and (Root^.LastStmt^.ErrorCode = PE_Success);
    end;
  end;
end;

function TCustomSQLParser.Parse(const Text: string): Boolean;
begin
  Result := Parse(PChar(Text), Length(Text));
end;

function TCustomSQLParser.ParseCaseOp(): ONode;
var
  First: Boolean;
  ResultValue: ONode;
  Value: ONode;
begin
  TokenPtr(CurrentToken)^.FUsageType := utOperator;
  ApplyCurrentToken(); // CASE

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
      Value := ParseValue;

    Result := TCaseOp.Create(Self, Value);
    with PCaseOp(NodePtr(Result))^ do
    begin
      First := True;
      repeat
        if (First) then
          First := False
        else
        begin
          TokenPtr(CurrentToken)^.FUsageType := utOperator;
          ApplyCurrentToken(); // WHEN
        end;

        if (CurrentToken = 0) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN) then
          SetError(PE_UnexpectedToken, CurrentToken)
        else
        begin
          Value := ParseValue();
          if (CurrentToken = 0) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiTHEN) then
            SetError(PE_UnexpectedToken, CurrentToken)
          else
          begin
            TokenPtr(CurrentToken)^.FUsageType := utOperator;
            ApplyCurrentToken(); // THEN

            ResultValue := ParseValue();

            AddCondition(Value, ResultValue);
          end;
        end;
      until (Error or (CurrentToken = 0) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN));

      if (not Error and (CurrentToken > 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE)) then
      begin
        TokenPtr(CurrentToken)^.FUsageType := utOperator;
        ApplyCurrentToken(); // ELSE

        if (CurrentToken = 0) then
          SetError(PE_IncompleteStmt)
        else
          SetElse(ParseValue());
      end;

      if (CurrentToken = 0) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiEND) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
      begin
        TokenPtr(CurrentToken)^.FUsageType := utOperator;
        ApplyCurrentToken(); // END
      end;
    end;
  end;
end;

function TCustomSQLParser.ParseColumn(): ONode;
var
  Alias: ONode;
  AsToken: ONode;
  Value: ONode;
begin
  Value := ParseValue();

  if ((CurrentToken = 0) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiAs)) then
    AsToken := 0
  else
  begin
    AsToken := CurrentToken;
    ApplyCurrentToken();
  end;

  Alias := 0;
  if ((AsToken > 0) and ((CurrentToken = 0) or (TokenPtr(CurrentToken)^.TokenType = ttDelimiter))) then
    SetError(PE_IncompleteStmt)
  else if ((AsToken > 0) and (CurrentToken > 0) and not (TokenPtr(CurrentToken)^.TokenType in [ttIdentifier, ttDQIdentifier, ttDBIdentifier, ttBRIdentifier, ttMySQLIdentifier])) then
    SetError(PE_UnexpectedToken, CurrentToken)
  else if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType in [ttIdentifier, ttDQIdentifier, ttDBIdentifier, ttBRIdentifier, ttMySQLIdentifier])) then
  begin
    TokenPtr(CurrentToken)^.FUsageType := utAlias;
    Alias := CurrentToken;
    ApplyCurrentToken();
  end;

  Result := TSelectStmt.TColumn.Create(Self, Value, Alias);

  if (AsToken > 0) then
    TSelectStmt.PColumn(NodePtr(Result))^.Heritage.AddChild(AsToken);
end;

function TCustomSQLParser.ParseCompoundStmt(): ONode;
var
  Token: ONode;
begin
  Result := TStmt.Create(Self, stCompound);

  repeat
    Token := CurrentToken; ApplyCurrentToken();
    if (Token > 0) then
      if (TokenPtr(Token)^.KeywordIndex <> kiEnd) then
        ParseStmt(Result)
      else
      begin
        Token := CurrentToken; ApplyCurrentToken();
        if (Token > 0) then
          case (TokenPtr(Token)^.TokenType) of
            ttDelimiter: ;
            ttEndLabel:
//              if (ALabelToken = 0) then
//                SetError(PE_UnexpectedToken, Token)
//              else
//              if (StrIComp(PChar(TokenPtr(Token).AsString), PChar(TokenPtr(ALabelToken).AsString)) <> 0) then
//                SetError(PE_UnexpectedToken, Token);
            else
              SetError(PE_UnexpectedToken, Token);
          end;
      end;
  until ((Token = 0) or (TokenPtr(Token)^.KeywordIndex = kiEND));
end;

function TCustomSQLParser.ParseFunction(): ONode;
var
  Identifier: ONode;
  Arguments: ONode;
begin
  TokenPtr(CurrentToken)^.FOperatorType := otFunction_;
  if ((FFunctions.Count = 0) or (FFunctions.IndexOf(TokenPtr(CurrentToken)^.FText.SQL, TokenPtr(CurrentToken)^.FText.Length) >= 0)) then
  begin
    TokenPtr(CurrentToken)^.FUsageType := utFunction;
    Identifier := CurrentToken;
  end
  else
  begin
    TokenPtr(CurrentToken)^.FUsageType := utDbIdentifier;
    Identifier := TDbIdentifier.Create(Self, CurrentToken, ditFunction);
  end;
  ApplyCurrentToken();

  Arguments := ParseSubArea([ntValues]);

  if (Error) then
    Result := 0
  else
    Result := TFunction.Create(Self, Identifier, Arguments);
end;

function TCustomSQLParser.ParseSelectStmt(): ONode;
var
  Columns: ONode;
begin
  Assert(TokenPtr(CurrentToken)^.KeywordIndex = kiSELECT); ApplyCurrentToken();

  Columns := ParseSiblings(ntColumns, ParseColumn);

  Result := TSelectStmt.Create(Self, Columns);
end;

function TCustomSQLParser.ParseSiblings(const ANodeType: TNodeType; const ParseSibling: TParseFunction): ONode;
var
  First: Boolean;
  Sibling: ONode;
begin
  case (ANodeType) of
    ntValues: Result := TValues.Create(Self);
    ntColumns: Result := TSelectStmt.TColumns.Create(Self);
    else raise ERangeError.Create(SArgumentOutOfRange);
  end;

  with PSiblings(NodePtr(Result))^ do
  begin
    First := True;
    repeat
      if (First) then
        First := False
      else
      begin
        TokenPtr(CurrentToken)^.FUsageType := utSymbol;
        ApplyCurrentToken();
      end;
      Sibling := ParseSibling();
      if (not Error) then
        AddSibling(Sibling);
    until (Error or (CurrentToken = 0) or (TokenPtr(CurrentToken)^.TokenType <> ttComma));
  end;
end;

function TCustomSQLParser.ParseSubArea(const ANodeTypes: TNodeTypes): ONode;
begin
  Assert(TokenPtr(CurrentToken)^.TokenType = ttOpenBracket);

  TokenPtr(CurrentToken)^.FUsageType := utSymbol;
  ApplyCurrentToken(); // ttOpenBracket

  if (CurrentToken = 0) then
  begin
    Result := 0;
    SetError(PE_IncompleteStmt);
  end
  else
  begin
    if ((ntSelectStmt in ANodeTypes) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSelect)) then
      Result := ParseSelectStmt()
    else if (ntValues in ANodeTypes) then
      Result := ParseSiblings(ntValues, ParseValue)
    else
      Result := ParseValue();

    if (not Error) then
      if (CurrentToken = 0) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
        SetError(PE_UnexpectedToken, CurrentToken)
      else
      begin
        TokenPtr(CurrentToken)^.FUsageType := utSymbol;
        ApplyCurrentToken(); // ttCloseBracket
      end;
  end;
end;

function TCustomSQLParser.ParseStmt(const AParentNode: ONode): ONode;
var
  FirstToken: ONode;
  KeywordIndex: Integer;
  KeywordToken: ONode;
  LabelToken: ONode;
  Stmt: PStmt;
  Token: PToken;
begin
  FirstToken := CurrentToken;
  KeywordToken := FirstToken;
  if ((KeywordToken = 0) or (TokenPtr(KeywordToken)^.TokenType <> ttBeginLabel)) then
    LabelToken := 0
  else
  begin
    LabelToken := KeywordToken;
    KeywordToken := CurrentToken;
  end;

  if ((KeywordToken = 0) or (TokenPtr(KeywordToken)^.TokenType <> ttKeyword)) then
    KeywordIndex := 0
  else
    KeywordIndex := TokenPtr(KeywordToken)^.KeywordIndex;

  if (KeywordIndex = kiBEGIN) then
    Result := ParseCompoundStmt()
  else if (LabelToken > 0) then
    Result := ParseUnknownStmt()
//  if (KeywordIndex = kiEND) then
//    // Will be handled in caller routine of FindNext
  else if (KeywordIndex = kiSELECT) then
    Result := ParseSelectStmt()
  else
    Result := ParseUnknownStmt();

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

  Token := StmtPtr(Result)^.FirstToken;
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

function TCustomSQLParser.ParseToken(): ONode;
label
  TwoChars,
  Selection, SelSpace, SelQuotedIdentifier, SelNotLess, SelNotEqual1, SelNotGreater, SelNot1, SelDoubleQuote, SelComment, SelModulo, SelDolor, SelAmpersand2, SelBitAND, SelSingleQuote, SelOpenBracket, SelCloseBracket, SelMySQLCodeEnd, SelLeftJoin, SelMulti, SelComma, SelDoubleDot, SelDot, SelMySQLCode, SelDiv, SelNumeric, SelSLComment, SelArrow, SelMinus, SelPlus, SelAssign, SelColon, SelDelimiter, SelNULLSaveEqual, SelLessEqual, SelShiftLeft, SelNotEqual2, SelLess, SelRightJoin, SelEqual, SelGreaterEqual, SelShiftRight, SelGreater, SelParameter, SelAt, SelUnquotedIdentifier, SelDBIdentifier, SelBackslash, SelCloseSquareBracket, SelHat, SelMySQLCharacterSet, SelMySQLIdentifier, SelUnquotedIdentifierLower, SelRBIdentifier, SelPipe, SelBitOR, SelTilde, SelE,
  At,
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
  KeywordIndex: Integer;
  Length: Integer;
  MySQLVersion: Integer;
  OperatorType: TOperatorType;
  SQL: PChar;
  TokenLength: Integer;
  TokenType: fspTypes.TTokenType;
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
      JNE SelLeftJoin                  // No!
      MOV TokenType,ttMySQLCodeEnd
      JMP DoubleChar
    SelLeftJoin:
      CMP EAX,$003D002A                // "*=" ?
      JNE SelMulti                     // No!
      MOV OperatorType,otLeftJoin
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
      JNE SelRightJoin                 // No!
      CMP EAX,$003D003C                // "<=" ?
      JNE SelShiftLeft                 // No!
      CMP ECX,3                        // Three characters in SQL?
      JB SelLessEqual                  // No!
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
      JNE SelRightJoin                 // No!
      MOV OperatorType,otNotEqual
      JMP DoubleChar
    SelLess:
      MOV OperatorType,otLess
      JMP SingleChar
    SelRightJoin:
      CMP AX,'='                       // "=" ?
      JNE SelGreaterEqual              // No!
      CMP EAX,$002A003D                // "=*" ?
      JNE SelEqual                     // No!
      MOV OperatorType,otRightJoin
      JMP DoubleChar
    SelEqual:
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
      JE At                            // Yes!
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
      JA SelRBIdentifier               // No!
      MOV TokenType,ttIdentifier
      JMP UnquotedIdentifier           // Yes!
    SelRBIdentifier:
      CMP AX,'{'                       // "{" ?
      JNE SelPipe                      // No!
      MOV TokenType,ttBRIdentifier
      MOV DX,'}'                       // End Quoter
      JMP QuotedIdentifier
    SelPipe:
      CMP AX,'|'                       // "|" ?
      JNE SelTilde                     // No!
      CMP EAX,$007C007C                // "||" ?
      JNE SelBitOR                     // No!
      MOV OperatorType,otPipes
      JMP DoubleChar
    SelBitOR:
      MOV OperatorType,otBitOr
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

    At:
      MOV TokenType,ttVariable
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      MOV AX,[ESI]                     // One Character from SQL to AX
      CMP AX,'A'
      JB Finish
      CMP AX,'Z'
      JBE Variable
      CMP AX,'a'
      JB Finish
      CMP AX,'z'
      JBE Variable
      MOV TokenType,ttAt
      JMP Finish

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
      MOV TokenType,ttMySQLCharacterSet
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
      JE MySQLCondCodeE                    // Yes!
      MOV AX,[ESI]                     // One Character from SQL to AX
      CMP AX,'0'                       // Digit?
      JB MySQLCondCodeE                    // No!
      CMP AX,'9'                       // Digit?
      JA MySQLCondCodeE                    // No!
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
      CMP ECX,1                        // End of SQL?
      JAE QuotedIdentifier2            // No!
      JMP Incomplete
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
      begin
        TokenType := ttKeyword;
        OperatorType := OperatorTypeByKeywordIndex[KeywordIndex];
      end;
    end;

    Result := TToken.Create(Self, SQL, TokenLength, FParsePos.Origin, ErrorCode, FMySQLVersion, TokenType, OperatorType, KeywordIndex);

    if (Root^.FLastToken > 0) then
    begin
      TokenPtr(Result)^.FPriorToken := Root^.FLastToken;
    end;
    Root^.FLastToken := Result;

    FParsePos.Text := @SQL[TokenLength];
    Dec(FParsePos.Length, TokenLength);
    Inc(FParsePos.Origin.Pos, TokenLength);
    if (TokenType = ttReturn) then
    begin
      Inc(FParsePos.Origin.Char);
      FParsePos.Origin.Line := 0;
    end
    else
      Inc(FParsePos.Origin.Char, TokenLength);
  end;
end;

function TCustomSQLParser.ParseUnknownStmt(): ONode;
var
  Token: ONode;
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

function TCustomSQLParser.ParseValue(): ONode;
const
  MaxNodeCount = 100;
var
  NodeCount: Integer;
  Nodes: array[0 .. MaxNodeCount - 1] of ONode;

  procedure AddNode(const ANode: ONode);
  begin
    if (NodeCount = MaxNodeCount) then
      raise Exception.CreateFmt(STooManyTokensInExpression, [NodeCount]);

    Nodes[NodeCount] := ANode;
    Inc(NodeCount);
    if (IsToken(ANode)) then
      ApplyCurrentToken();
  end;

var
  I: Integer;
  KeywordIndex: Integer;
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
      else if (KeywordIndex = kiCASE) then
        TokenPtr(CurrentToken)^.FOperatorType := otCase
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
          AddNode(ParseSubArea([]))
        else if (IsRangeNode(NodePtr(Nodes[NodeCount - 1]))) then
          SetError(PE_UnexpectedToken, RangeNodePtr(Nodes[NodeCount - 1])^.FFirstToken)
        else if (TokenPtr(Nodes[NodeCount - 1])^.OperatorType = otIn) then
          AddNode(ParseSubArea([ntSelectStmt, ntValues]))
        else if (TokenPtr(Nodes[NodeCount - 1])^.OperatorType in [otInterval, otBinary, otCollate]) then
          AddNode(ParseSubArea([ntValues]))
        else
          AddNode(ParseSubArea([]));
      else
        if ((NodeCount = 0) or (IsToken(Nodes[NodeCount - 1]) and (TokenPtr(Nodes[NodeCount - 1])^.OperatorType <> otUnknown))) then
          // Operand
          case (TokenPtr(CurrentToken)^.TokenType) of
            ttOperator:
              begin
                TokenPtr(CurrentToken)^.FUsageType := utOperator;
                AddNode(CurrentToken);
              end;
            ttInteger,
            ttNumeric,
            ttString:
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
            ttDQIdentifier,
            ttDBIdentifier,
            ttBRIdentifier,
            ttMySQLIdentifier:
              if ((NextToken[1] = 0) or (TokenPtr(NextToken[1])^.TokenType <> ttOpenBracket)) then
              begin
                TokenPtr(CurrentToken)^.FUsageType := utDbIdentifier;
                AddNode(CurrentToken);
              end
              else
                AddNode(ParseFunction());
            ttKeyword:
              if (TokenPtr(CurrentToken)^.KeywordIndex = kiNULL) then
              begin
                TokenPtr(CurrentToken)^.FUsageType := utConst;
                AddNode(CurrentToken);
              end
              else
                SetError(PE_UnexpectedToken, CurrentToken);
            else
              SetError(PE_UnexpectedToken, CurrentToken);
          end
        else if ((NodeCount > 0) and IsToken(Nodes[NodeCount - 1]) and (TokenPtr(Nodes[NodeCount - 1])^.OperatorType = otUnknown)) then
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
            otCASE,
            otWHEN,
            otTHEN,
            otELSE,
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
          // Prefix
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
            else if (not (NodePtr(Nodes[I + 1])^.FNodeType = ntValues)) then
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
              Nodes[I] := TUnaryOperation.Create(Self, Nodes[I], Nodes[I + 1]);
              Dec(NodeCount);
              Move(Nodes[I + 2], Nodes[I + 1], (NodeCount - I - 1) * SizeOf(Nodes[0]));
            end;
          otDot:
            if (I = 0) then
              SetError(PE_UnexpectedToken, Nodes[I])
            else if (I >= NodeCount - 1) then
              SetError(PE_IncompleteStmt)
            else if (NodePtr(Nodes[I + 1])^.NodeType = ntDbIdentifier) then
            begin
              PDbIdentifier(Nodes[I + 1])^.AddPrefix(Nodes[I - 1], Nodes[I]);
              Dec(NodeCount, 2);
              Move(Nodes[I + 1], Nodes[I - 1], (NodeCount - I) * SizeOf(Nodes[0]));
            end
            else if (NodePtr(Nodes[I + 1])^.NodeType = ntFunction) then
            begin
              if (PFunction(NodePtr(Nodes[I + 1]))^.Identifier^.NodeType <> ntDbIdentifier) then
              begin
                SetError(PE_UnexpectedToken, Nodes[I + 1]);
                Inc(I);
              end
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
              Nodes[I - 1] := TBinaryOperation.Create(Self, Nodes[I], Nodes[I - 1], Nodes[I + 1]);
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
              Nodes[I + 3] := TBetweenOperation.Create(Self, Nodes[I], Nodes[I + 2], Nodes[I - 1], Nodes[I + 1], Nodes[I + 3]);
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
              Nodes[I + 2] := TSoundsLikeOperation.Create(Self, Nodes[I], Nodes[I + 1], Nodes[I - 1], Nodes[I + 2]);
              Dec(NodeCount, 3);
              Move(Nodes[I + 2], Nodes[I - 1], NodeCount - I);
              Dec(I);
            end;
          else
            begin
              case (NodePtr(Nodes[I])^.FNodeType) of
                ntToken: SetError(PE_UnexpectedToken, Nodes[I]);
                ntRangeNode: SetError(PE_UnexpectedToken, RangeNodePtr(Nodes[I])^.FFirstToken);
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

function TCustomSQLParser.RangeNodePtr(const ANode: ONode): PRangeNode;
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
  ParentNodes: TList;
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
//    '    a.error:hover {' + #13#10 +
//    '      display: block;' + #13#10 +
//    '      background-color: #F00;' + #13#10 +
//    '    }' + #13#10 +
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
//    '    span a:visited { display: none; }' + #13#10 +
//    '    span.error a:hover {' + #13#10 +
//    '      background-color: #FDD;' + #13#10 +
//    '    }' + #13#10 +
//    '    span.plsql a:hover {' + #13#10 +
//    '      background-color: #CFC;' + #13#10 +
//    '    }' + #13#10 +
//    '    span.stmt a:hover {' + #13#10 +
//    '      background-color: #DDF;' + #13#10 +
//    '    }' + #13#10 +
    '  </style>' + #13#10 +
    '  </head>' + #13#10 +
    '  <body>' + #13#10;

  Stmt := Root^.FirstStmt;
  repeat
    Token := Stmt^.FirstToken; GenerationCount := 0;
    while (Assigned(Token)) do
    begin
      GenerationCount := Max(GenerationCount, Token^.Generation);
      if (Token = Stmt^.LastToken) then
        Token := nil
      else
        Token := Token^.NextToken;
    end;

    ParentNodes := TList.Create();
    ParentNodes.Add(Root);

    HTML := HTML
      + '<table cellspacing="2" cellpadding="0" border="0">' + #13#10;

    for Generation := 0 to GenerationCount - 1 do
    begin
      LastTokenIndex := -1;

      HTML := HTML
        + '<tr>' + #13#10;
      Token := Stmt^.FirstToken;
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
            + '<span><table cellspacing="2" cellpadding="0">'
            + '<tr><td>ParentNode Offset:</td><td>&nbsp;</td><td>' + IntToStr(PStmtNode(Node)^.ParentNode^.Offset) + '</td></tr>'
            + '<tr><td>Offset:</td><td>&nbsp;</td><td>' + IntToStr(Node^.Offset) + '</td></tr>';
          if (IsStmt(Node)) then
          begin
            HTML := HTML + '<tr><td>StmtType:</td><td>&nbsp;</td><td>' + StmtTypeToString[PStmt(Node)^.StmtType] + '</td></tr>';
          end;
          case (Node^.NodeType) of
            ntDbIdentifier:
              HTML := HTML
                + '<tr><td>DbIdentifierType:</td><td>&nbsp;</td><td>' + DbIdentifierTypeToString[PDbIdentifier(Node)^.DbIdentifierType] + '</td></tr>';
            ntBinaryOp:
              if (IsToken(PNode(PBinaryOperation(Node)^.Operator))) then
                HTML := HTML
                  + '<tr><td>OperatorType:</td><td>&nbsp;</td><td>' + OperatorTypeToString[PToken(PBinaryOperation(Node)^.Operator)^.OperatorType] + '</td></tr>';
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
      HTML := HTML + '<tr><td>ParentNode Offset:</td><td>&nbsp;</td><td>' + IntToStr(PStmtNode(Token)^.ParentNode^.Offset) + '</td></tr>';
      HTML := HTML + '<tr><td>Offset:</td><td>&nbsp;</td><td>' + IntToStr(PNode(Token)^.Offset) + '</td></tr>';
      HTML := HTML + '<tr><td>Type:</td><td>&nbsp;</td><td>' + HTMLEscape(TokenTypeToString[Token^.TokenType]) + '</td></tr>';
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

    Stmt := nil;
  until (not Assigned(Stmt));

  HTML := HTML +
    '     <br>' + #13#10 +
    '     <br>' + #13#10 +
    '  </body>' + #13#10 +
    '</html>';

  WriteFile(Handle, PChar(BOM_UNICODE_LE)^, 2, Size, nil);

  WriteFile(Handle, PChar(HTML)^, Length(HTML) * SizeOf(Char), Size, nil);

  CloseHandle(Handle);
end;

procedure TCustomSQLParser.SetError(const AErrorCode: Integer; const AErrorNode: ONode = 0);
begin
  if (FErrorCode = PE_Success) then
  begin
    FErrorCode := AErrorCode;
    FErrorToken := StmtNodePtr(AErrorNode)^.FFirstToken;
  end;
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
    kiAND      := IndexOf('AND');
    kiAS       := IndexOf('AS');
    kiBEGIN    := IndexOf('BEGIN');
    kiBETWEEN  := IndexOf('BETWEEN');
    kiBINARY   := IndexOf('BINARY');
    kiCASE     := IndexOf('CASE');
    kiCOLLATE  := IndexOf('COLLATE');
    kiDIV      := IndexOf('DIV');
    kiDO       := IndexOf('DO');
    kiELSE     := IndexOf('ELSE');
    kiEND      := IndexOf('END');
    kiFROM     := IndexOf('FROM');
    kiIS       := IndexOf('IS');
    kiIN       := IndexOf('IN');
    kiINTERVAL := IndexOf('INTERVAL');
    kiLIKE     := IndexOf('LIKE');
    kiLOOP     := IndexOf('LOOP');
    kiMOD      := IndexOf('MOD');
    kiNOT      := IndexOf('NOT');
    kiNULL     := IndexOf('NULL');
    kiOR       := IndexOf('OR');
    kiREGEXP   := IndexOf('REGEXP');
    kiREPEAT   := IndexOf('REPEAT');
    kiRLIKE    := IndexOf('RLIKE');
    kiSELECT   := IndexOf('SELECT');
    kiSOUNDS   := IndexOf('SOUNDS');
    kiWHEN     := IndexOf('WHEN');
    kiTHEN     := IndexOf('THEN');
    kiUNTIL    := IndexOf('UNTIL');
    kiWHILE    := IndexOf('WHILE');
    kiXOR      := IndexOf('XOR');

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

function TCustomSQLParser.SiblingPtr(const ANode: ONode): PSibling;
begin
  if (ANode = 0) then
    Result := nil
  else
    Result := @FNodes.Mem[ANode];
end;

function TCustomSQLParser.SiblingsPtr(const ANode: ONode): PSiblings;
begin
  Assert(IsSiblings(NodePtr(ANode)));

  if (ANode = 0) then
    Result := nil
  else
    Result := @FNodes.Mem[ANode];
end;

function TCustomSQLParser.StmtNodePtr(const ANode: ONode): PStmtNode;
begin
  Assert(IsStmtNode(NodePtr(ANode)));

  Result := @FNodes.Mem[ANode];
end;

function TCustomSQLParser.StmtPtr(const ANode: ONode): PStmt;
begin
  Assert(IsStmtNode(NodePtr(ANode)));

  Result := @FNodes.Mem[ANode];
end;

function TCustomSQLParser.TokenPtr(const ANode: ONode): PToken;
begin
  Assert(NodePtr(ANode)^.FNodeType = ntToken);

  Result := PToken(NodePtr(ANode));
end;

{ TMySQLSQLParser *************************************************************}

constructor TMySQLSQLParser.Create(const MySQLVersion: Integer = 0; const LowerCaseTableNames: Boolean = False);
begin
  FMySQLVersion := MySQLVersion;
  FLowerCaseTableNames := LowerCaseTableNames;

  inherited Create(sdMySQL);

  FAnsiQuotes := False;

  Functions := MySQLFunctions;
  Keywords := MySQLKeywords;
end;

end.
