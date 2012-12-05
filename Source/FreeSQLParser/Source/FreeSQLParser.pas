unit FreeSQLParser;

interface {********************************************************************}

uses
  Classes,
  fspTypes, fspConst;

type
  TCustomSQLParser = class
  protected
    type
      ONode = Integer;

      TOrigin = record Pos, Char, Line: Integer; end;

      TWordList = class(TObject)
      type
        TWordArray = array of array of array of Char;
      private
        FCount: Integer;
        FParser: TCustomSQLParser;
        FWords: TWordArray;
        function GetText(): string;
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
        property Text: string read GetText write SetText;
        property Words[Index: Integer]: string read GetWord; default;
      end;

  public
    type
      PNode = ^TNode;
      PRangeNode = ^TRangeNode;
      PRoot = ^TRoot;
      PToken = ^TToken;
      PList = ^TListNode;
      PStmt = ^TStmt;

      TNode = record
      private
        FNodeType: TNodeType;
        FParser: TCustomSQLParser;
      public
        property NodeType: TNodeType read FNodeType;
        property Parser: TCustomSQLParser read FParser;
      end;

      TRangeNode = record
      private
        FNodeType: TNodeType;
        FParser: TCustomSQLParser;
        FParentNode: ONode;
        FFirstToken: ONode;
        FLastToken: ONode;
        function GetFirstToken(): PToken; inline;
        function GetLastToken(): PToken; inline;
        function GetParentNode(): PNode; inline;
      public
        property FirstToken: PToken read GetFirstToken;
        property LastToken: PToken read GetLastToken;
        property NodeType: TNodeType read FNodeType;
        property Parser: TCustomSQLParser read FParser;
        property ParentNode: PNode read GetParentNode;
      end;

      TStmtNode = record
      private
        FNodeType: TNodeType;
        FParser: TCustomSQLParser;
        FParentNode: ONode;
        FFirstToken: ONode;
        FLastToken: ONode;
        FStmtToken: ONode;
      end;

      TToken = record
      private // Heritage
        FNodeType: TNodeType;
        FParser: TCustomSQLParser;
      private
        FDbObjectType: TDbObjectType;
        FErrorCode: Integer;
        FIsUsed: Boolean;
        FKeywordIndex: Integer;
        FMySQLCode: Integer;
        FNextToken: ONode;
        FOperatorType: TOperatorType;
        FOrigin: TOrigin;
        FParentNode: ONode;
        FParentToken: PToken;
        FPriorToken: ONode;
        FText: record
          SQL: PChar;
          Length: Integer;
          NewText: string;
        end;
        FTokenType: TTokenType;
        FUsageType: TUsageType;
        procedure Assign(const AParentNode: ONode; const AUsageType: TUsageType = utUnknown);
        function GetAsString(): string;
        function GetGeneration(Index: Integer): PNode;
        function GetGenerationCount(): Integer;
        function GetIndex(): Integer;
        function GetNextToken(): PToken; inline;
        function GetParentNode(): PNode; inline;
        function GetPriorToken(): PToken; inline;
        function GetText(): string;
        procedure SetText(AText: string);
        property Generation[Index: Integer]: PNode read GetGeneration;
        property GenerationCount: Integer read GetGenerationCount;
        property Index: Integer read GetIndex;
      public
        property AsString: string read GetAsString;
        property DbObjectType: TDbObjectType read FDbObjectType;
        property ErrorCode: Integer read FErrorCode;
        property IsUsed: Boolean read FIsUsed;
        property KeywordIndex: Integer read FKeywordIndex;
        property NextToken: PToken read GetNextToken;
        property NodeType: TNodeType read FNodeType;
        property OperatorType: TOperatorType read FOperatorType;
        property Origin: TOrigin read FOrigin;
        property ParentNode: PNode read GetParentNode;
        property ParentToken: PToken read FParentToken;
        property PriorToken: PToken read GetPriorToken;
        property Text: string read GetText write SetText;
        property TokenType: TTokenType read FTokenType;
        property UsageType: TUsageType read FUsageType;
      end;

      TRoot = record
      private // Heritage
        FNodeType: TNodeType;
        FParser: TCustomSQLParser;
        FParentNode: ONode;
        FFirstToken: ONode;
        FLastToken: ONode;
      private
        FFirstStmt: ONode;
        FLastStmt: ONode;
        function GetFirstStmt(): PStmt; inline;
        function GetFirstToken(): PToken; inline;
        function GetLastStmt(): PStmt; inline;
        function GetLastToken(): PToken; inline;
      public
        property FirstStmt: PStmt read GetFirstStmt;
        property FirstToken: PToken read GetFirstToken;
        property LastStmt: PStmt read GetLastStmt;
        property LastToken: PToken read GetLastToken;
        property NodeType: TNodeType read FNodeType;
      end;

//      TSibling = record
//      private // Heritage
//        FNodeType: TNodeType;
//        FParser: TCustomSQLParser;
//        FParentNode: ONode;
//        FFirstToken: TNodeOffset;
//        FLastToken: TNodeOffset;
//        FStmtToken: TNodeOffset;
//      private
//        function GetHeigth(): Integer;
//      private
//        function GetText(): string;
//        property Height: Integer read GetHeigth;
//      public
//        constructor Create(const AParentNode: TNode; const AFirstToken: PToken = nil);
//        function GetNextSibling(): TNode;
//        function GetPriorSibling(): TNode;
//        property Stmt: PStmt read Heritage.FStmt;
//      end;

      TListNode = record
      private // Heritage
        FNodeType: TNodeType;
        FParser: TCustomSQLParser;
        FParentNode: ONode;
        FFirstToken: ONode;
        FLastToken: ONode;
        FStmtToken: ONode;
      private
//        List: TList;
//        function GetCount(): Integer;
//        function GetChild(Index: Integer): TNode;
      private
//        procedure Add(const AToken: PToken; const AUsageType: TUsageType); overload;
//        procedure Add(const ANode: PNode); overload; inline;
      public
//        constructor Create(const AParentNode: PNode; const AFirstToken: PToken = nil);
//        procedure Free();
//        property Child[Index: Integer]: TNode read GetChild; default;
//        property Count: Integer read GetCount;
      end;

//      TUnaryOperation = record
//      private // Heritage
//        FNodeType: TNodeType;
//        FParser: TCustomSQLParser;
//        FParentNode: ONode;
//        FFirstToken: TNodeOffset;
//        FLastToken: TNodeOffset;
//        FStmtToken: TNodeOffset;
//      private
//        FOperand: TNode;
//        FOperator: TToken;
//      public
//        constructor Create(const AParentNode: TNode; const AOperator: TToken; const AOperand: TNode);
//        property Operand: TNode read FOperand;
//        property Operator: TToken read FOperator;
//      end;
//
//      TBinaryOperation = record
//      private // Heritage
//        FNodeType: TNodeType;
//        FParser: TCustomSQLParser;
//        FParentNode: ONode;
//        FFirstToken: TNodeOffset;
//        FLastToken: TNodeOffset;
//        FStmtToken: TNodeOffset;
//        FStmtToken: TNodeOffset;
//      private
//        FOperand1: TNode;
//        FOperand2: TNode;
//        FOperator: TToken;
//      public
//        constructor Create(const AParentNode: TNode;  const AOperator: TToken; const AOperand1, AOperand2: TNode);
//        property Operand1: TNode read FOperand1;
//        property Operand2: TNode read FOperand2;
//        property Operator: TToken read FOperator;
//      end;
//
//      TFunction = record
//      private // Heritage
//        FNodeType: TNodeType;
//        FParser: TCustomSQLParser;
//        FParentNode: ONode;
//        FFirstToken: TNodeOffset;
//        FLastToken: TNodeOffset;
//      private
//        FArgumentList: PList;
//      public
//        constructor Create(const AParentNode: TNode; const AIdentifier: TToken; const AArgumentList: PList);
//        property Identifier: PToken read Heritage.Heritage.FFirstToken;
//        property ArgumentList: PList read FArgumentList;
//      end;
//
//      TBetweenOperation = record
//      private // Heritage
//        FNodeType: TNodeType;
//        FParser: TCustomSQLParser;
//        FParentNode: ONode;
//        FFirstToken: TNodeOffset;
//        FLastToken: TNodeOffset;
//        FStmtToken: TNodeOffset;
//      private
//        FExpr: TNode;
//        FMax: TNode;
//        FMin: TNode;
//        FOperator1: TToken;
//        FOperator2: TToken;
//      public
//        constructor Create(const AParentNode: TNode; const AOperator1, AOperator2: TToken; const AExpr, AMin, AMax: TNode);
//        property Expr: TNode read FExpr;
//        property Max: TNode read FMax;
//        property Min: TNode read FMin;
//        property Operator1: TToken read FOperator1;
//        property Operator2: TToken read FOperator2;
//      end;
//
//      TSoundsLikeOperation = record
//      private // Heritage
//        FNodeType: TNodeType;
//        FParser: TCustomSQLParser;
//        FParentNode: ONode;
//        FFirstToken: TNodeOffset;
//        FLastToken: TNodeOffset;
//        FStmtToken: TNodeOffset;
//      private
//        Heritage: TNode;
//      private
//        FOperand1: TNode;
//        FOperand2: TNode;
//        FOperator1: TToken;
//        FOperator2: TToken;
//      public
//        constructor Create(const AParentNode: TNode; const AOperator1, AOperator2: TToken; const AOperand1, AOperand2: TNode);
//        property Operand1: TNode read FOperand1;
//        property Operand2: TNode read FOperand2;
//        property Operator1: TToken read FOperator1;
//        property Operator2: TToken read FOperator2;
//      end;
//
//      TList = record
//      private // Heritage
//        FNodeType: TNodeType;
//        FParser: TCustomSQLParser;
//        FParentNode: ONode;
//        FFirstToken: TNodeOffset;
//        FLastToken: TNodeOffset;
//        FStmtToken: TNodeOffset;
//      public
//        constructor Create(const AParentNode: TNode; const AFirstToken: TToken);
//      end;

      TStmt = record
      private // Heritage
        FNodeType: TNodeType;
        FParser: TCustomSQLParser;
        FParentNode: ONode;
        FFirstToken: ONode;
        FLastToken: ONode;
        FStmtType: TStmtType;
        FErrorCode: Integer;
        FErrorToken: ONode;
      private
        function Create(const AParser: TCustomSQLParser; const AStmtType: TStmtType; const AFirstToken: ONode): ONode;
        function GetError(): Boolean; inline;
        function GetErrorToken(): PToken; inline;
        function GetFirstToken(): PToken; inline;
        function GetLastToken(): PToken; inline;
        procedure SetError(const AErrorCode: Integer; const AErrorToken: ONode = 0);
        property Error: Boolean read GetError;
      public
        property ErrorCode: Integer read FErrorCode;
        property ErrorToken: PToken read GetErrorToken;
        property FirstToken: PToken read GetFirstToken;
        property LastToken: PToken read GetLastToken;
        property NodeType: TNodeType read FNodeType;
        property StmtType: TStmtType read FStmtType;
      end;

//      TUnknownStmt = record
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
//        constructor Create(const AStmtList: PStmtList; const AFirstToken: TToken);
//      end;
//
      TSelectStmt = record
      private
        FNodeType: TNodeType;
        FParser: TCustomSQLParser;
        FParentNode: ONode;
        FFirstToken: ONode;
        FLastToken: ONode;
        FStmtType: TStmtType;
        FErrorCode: Integer;
        FErrorToken: ONode;
      type
        PColumnList = ^TColumnList;

        TColumn = record
        private
          Heritage: TNode;
        private
          FAliasToken: TToken;
          FColumns: PColumnList;
          FExpression: TNode;
          function GetDisplayName(): string;
        public
          constructor Create(const AFields: PColumnList; const AFirstToken: TToken);
          property Alias: TToken read FAliasToken;
          property Columns: PColumnList read FColumns;
          property DisplayName: string read GetDisplayName;
          property Expression: TNode read FExpression;
        end;

        TColumnList = record
        private
          Heritage: TListNode;
        public
          constructor Create(const ASelectStmt: TSelectStmt; const AFirstToken: TToken);
        end;

      private
        FColumns: TColumnList;
      public
        function Create(const AParser: TCustomSQLParser; const AFirstToken: ONode): ONode;
        procedure Free();
        property Columns: TColumnList read FColumns;
      end;

//      TCompoundStmt = record
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
//        constructor Create(const AStmtList: PStmtList; const AFirstToken: TToken);
//      end;

  private
    FFunctions: TWordList;
    FHighNotPrecedence: Boolean;
    FKeywords: TWordList;
    FMySQLCode: Integer;
    FNodes: record
      Mem: PAnsiChar;
      Offset: Integer;
      Size: Integer;
    end;
    FParsedText: string;
    FParsePos: record Text: PChar; Length: Integer; Origin: TOrigin; end;
    FParseStmts: Boolean;
    FPipesAsConcat: Boolean;
    FRoot: ONode;
    FSQLDialect: TSQLDialect;
    function GetRoot(): PRoot; inline;
    function GetFunctions(): string; inline;
    function GetKeywords(): string; inline;
    procedure SetFunctions(AFunctions: string); inline;
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

    function IsRangeNode(const ANode: PNode): Boolean; inline;
    function IsStmtNode(const ANode: PNode): Boolean; inline;
    function NewNode(const ANodeType: TNodeType): ONode;
    function ListPtr(const ANodeOffset: ONode): PList; inline;
    function NodePtr(const ANodeOffset: ONode): PNode; inline;
    function ParseCompoundStmt(const AFirstToken, ALabelToken: ONode): ONode;
    function ParseExpression(const AFirstToken: ONode; const AStmt: ONode): ONode;
    function ParseSelectStmt(const AFirstToken: ONode): ONode;
    function ParseStmt(const AFirstToken: ONode; const SubStmt: Boolean = False): ONode;
    function ParseToken(const AParentNode: ONode = 0): ONode;
    function ParseUnknownStmt(const AFirstToken: ONode): ONode;
    function RangeNodePtr(const ANodeOffset: ONode): PRangeNode; inline;
    function StmtPtr(const ANodeOffset: ONode): PStmt; inline;
    function TokenPtr(const ANodeOffset: ONode): PToken; inline;

    property ParsedText: string read FParsedText;

  public
    constructor Create(const ASQLDialect: TSQLDialect);
    destructor Destroy(); override;
    function Parse(const Text: PChar; const Length: Integer): Boolean; overload;
    function Parse(const Text: string): Boolean; overload; inline;
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
  SQLUtils,
  fspUtils;

resourcestring
  SUnknownError = 'Unknown Error';
  SKeywordNotFound = 'Keyword "%s" not found';
  SUnknownOperatorPrecedence = 'Unknown operator precedence for operator "%s"';
  STooManyTokensInExpression = 'Too many tokens (%d) in expression';
  SUnknownNodeType = 'Unknown node type';

{ TCustomSQLParser.TWordList **************************************************}

procedure TCustomSQLParser.TWordList.Clear();
var
  L: Integer;
  W: Integer;
begin
  FCount := 0;
  for L := 0 to Length(FWords) - 1 do
  begin
    for W := 0 to Length(FWords[L]) - 1 do
      SetLength(FWords[L][W], 0);
    SetLength(FWords[L], 0);
  end;
  SetLength(FWords, 0);

  SetLength(Parser.OperatorTypeByKeywordIndex, 0);
end;

constructor TCustomSQLParser.TWordList.Create(const ASQLParser: TCustomSQLParser; const AText: string = '');
begin
  inherited Create();

  FParser := ASQLParser;

  FCount := 0;
  SetLength(FWords, 0);

  Text := AText;
end;

destructor TCustomSQLParser.TWordList.Destroy();
begin
  Clear();

  inherited;
end;

function TCustomSQLParser.TWordList.GetText(): string;
var
  L: Integer;
  S: string;
  W: Integer;
begin
  for L := 1 to Length(FWords) - 1 do
    for W := 0 to Length(FWords[L]) - 1 do
    begin
      SetString(S, PChar(@FWords[L][W][0]), L);
      Result := Result + S + ',';
    end;
  if (Result <> '') then
    Delete(Result, Length(Result), 1);
end;

function TCustomSQLParser.TWordList.GetWord(Index: Integer): string;
var
  L: Integer;
begin
  Result := '';

  L := 1;
  while (Length(FWords[L]) <= Index) do
  begin
    Dec(Index, Length(FWords[L]));
    Inc(L);
  end;
  if (1 < Length(FWords)) then
    SetString(Result, PChar(@FWords[L][Index][0]), L);
end;

function TCustomSQLParser.TWordList.IndexOf(const Word: PChar; const Length: Integer): Integer;
var
  Comp: Integer;
  L: Integer;
  Left: Integer;
  Mid: Integer;
  Right: Integer;
begin
  Result := -1;

  if (Length < System.Length(FWords)) then
  begin
    Left := 0;
    Right := System.Length(FWords[Length]) - 1;
    while (Left <= Right) do
    begin
      Mid := (Right - Left) div 2 + Left;
      Comp := StrLIComp(PChar(@FWords[Length][Mid][0]), Word, Length);
      if (Comp < 0) then
        Left := Mid + 1
      else if (Comp = 0) then
        begin Result := Mid; break; end
      else
        Right := Mid - 1;
    end;

    if (Result >= 0) then
      for L := 1 to Length - 1 do
        Inc(Result, System.Length(FWords[L]));
  end;
end;

procedure TCustomSQLParser.TWordList.SetText(AText: string);

  function InsertIndex(const Word: PChar; const Len: Integer; out Index: Integer): Boolean;
  var
    Comp: Integer;
    Left: Integer;
    Mid: Integer;
    Right: Integer;
  begin
    Result := True;

    if ((Length(FWords[Len]) = 0) or (StrLIComp(Word, PChar(@FWords[Len][Length(FWords[Len]) - 1][0]), Len) > 0)) then
      Index := Length(FWords[Len])
    else
    begin
      Left := 0;
      Right := Length(FWords[Len]) - 1;
      while (Left <= Right) do
      begin
        Mid := (Right - Left) div 2 + Left;
        Comp := StrLIComp(PChar(@FWords[Len][Mid][0]), Word, Len);
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
    if (Length(FWords) < Len + 1) then
      SetLength(FWords, Len + 1);

    if (InsertIndex(Word, Len, Index)) then
    begin
      SetLength(FWords[Len], Length(FWords[Len]) + 1);
      SetLength(FWords[Len][Length(FWords[Len]) - 1], Len);
      for I := Length(FWords[Len]) - 2 downto Index do
        Move(FWords[Len][I][0], FWords[Len][I + 1][0], Len * 2);
      Move(Word[0], FWords[Len][Index][0], Len * SizeOf(Char));
    end;
  end;

var
  Index: Integer;
  Len: Integer;
  OldIndex: Integer;
begin
  Clear();

  if (AText <> '') then
  begin
    // Todo: Optimize SetLength calls

    OldIndex := 1; Index := 1;
    while (Index < Length(AText)) do
    begin
      while ((Index <= Length(AText)) and (AText[Index] <> ',')) do Inc(Index);
      Len := Index - OldIndex;

      Add(@AText[OldIndex], Len);

      Inc(Index);
      OldIndex := Index;
      Inc(FCount);
    end;
  end;
end;

{ TCustomSQLParser.TRangeNode *************************************************}

function TCustomSQLParser.TRangeNode.GetFirstToken(): PToken;
begin
  Result := PToken(FParser.NodePtr(FFirstToken));
end;

function TCustomSQLParser.TRangeNode.GetLastToken(): PToken;
begin
  Result := PToken(FParser.NodePtr(FLastToken));
end;

function TCustomSQLParser.TRangeNode.GetParentNode(): PNode;
begin
  Result := PNode(FParser.NodePtr(FParentNode));
end;

{ TCustomSQLParser.TRoot ******************************************************}

function TCustomSQLParser.TRoot.GetFirstStmt(): PStmt;
begin
  Result := PStmt(FParser.NodePtr(FFirstStmt));
end;

function TCustomSQLParser.TRoot.GetFirstToken(): PToken;
begin
  Result := PToken(FParser.NodePtr(FFirstToken));
end;

function TCustomSQLParser.TRoot.GetLastStmt(): PStmt;
begin
  Result := PStmt(FParser.NodePtr(FLastStmt));
end;

function TCustomSQLParser.TRoot.GetLastToken(): PToken;
begin
  Result := PToken(FParser.NodePtr(FLastToken));
end;

{ TCustomSQLParser.TToken *****************************************************}

procedure TCustomSQLParser.TToken.Assign(const AParentNode: ONode; const AUsageType: TUsageType = utUnknown);
begin
  FParentNode := AParentNode;
  FUsageType := AUsageType;
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

function TCustomSQLParser.TToken.GetGeneration(Index: Integer): PNode;
var
  Generation: Integer;
begin
  Generation := 0;
  Result := ParentNode;
  while (Assigned(Result) and FParser.IsRangeNode(Result) and (Generation < Index)) do
  begin
    Inc(Generation);
    Result := PRangeNode(Result)^.ParentNode;
  end;
  if (Generation <> Index) then
    Result := nil;
end;

function TCustomSQLParser.TToken.GetGenerationCount(): Integer;
var
  Node: PNode;
begin
  Result := 0;
  Node := ParentNode;
  while (Assigned(Node) and FParser.IsRangeNode(Node)) do
  begin
    Inc(Result);
    Node := PRangeNode(Node)^.ParentNode;
  end;
end;

function TCustomSQLParser.TToken.GetIndex(): Integer;
var
  Token: PToken;
begin
  Token := FParser.Root^.FirstToken;
  Result := 0;
  while (Assigned(Token) and (Token <> @Self)) do
  begin
    Inc(Result);
    Token := Token^.NextToken;
  end;
end;

function TCustomSQLParser.TToken.GetNextToken(): PToken;
begin
  Result := PToken(FParser.NodePtr(FNextToken));
end;

function TCustomSQLParser.TToken.GetParentNode(): PNode;
begin
  Result := FParser.NodePtr(FParentNode);
end;

function TCustomSQLParser.TToken.GetPriorToken(): PToken;
begin
  Result := PToken(FParser.NodePtr(FPriorToken));
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

{ TCustomSQLParser.TTokenList *************************************************}

//function TCustomSQLParser.TTokenList.GetText(): string;
//var
//  I: Integer;
//  Index: Integer;
//begin
//  SetLength(Result, 1000);
//  Index := 0;
//  for I := 0 to Count - 1 do
//    if (Token[I].FText.NewText = '') then
//    begin
//      while (Index + Token[I].FText.Length > Length(Result)) do
//        SetLength(Result, 2 * Length(Result));
//      System.Move(Token[I].FText.SQL^, Result[1 + Index], Token[I].FText.Length * SizeOf(Char));
//      Inc(Index, Token[I].FText.Length);
//    end
//    else
//    begin
//      while (Index + Length(Token[I].FText.NewText) > Length(Result)) do
//        SetLength(Result, 2 * Length(Result));
//      System.Move(PChar(Token[I].FText.NewText)^, Result[1 + Index], Length(Token[I].FText.NewText) * SizeOf(Char));
//      Inc(Index, Length(Token[I].FText.NewText));
//    end;
//  SetLength(Result, Index);
//end;

//function TCustomSQLParser.TNode.GetText(): string;
//var
//  Token: PToken;
//begin
//  Result := '';
//
//  Token := PBaseNode(@Self)^.FirstToken;
//  while (Assigned(Token)) do
//  begin
//    Result := Result + Token.Text;
//    if (Token = LastToken) then
//      Token := nil
//    else
//      Token := Token.NextToken;
//  end;
//end;
//
//{ TCustomSQLParser.TListNode **************************************************}
//
//procedure TCustomSQLParser.TListNode.Add(const AToken: PToken; const AUsageType: TUsageType);
//begin
//  AToken.Assign(@Self, AUsageType);
//
//  List.Add(AToken);
//end;
//
//procedure TCustomSQLParser.TListNode.Add(const ANode: PNode);
//begin
//  List.Add(ANode);
//end;
//
//constructor TCustomSQLParser.TListNode.Create(const AParentNode: PNode; const AFirstToken: PToken = nil);
//begin
//  inherited;
//
//  List := TList.Create();
//end;
//
//function TCustomSQLParser.TListNode.GetCount(): Integer;
//begin
//  Result := List.Count;
//end;
//
//function TCustomSQLParser.TListNode.GetChild(Index: Integer): TNode;
//var
//  I: Integer;
//begin
//  Result := PBaseNode(@Self)^.FirstToken;
//  for I := 1 to Height do
//    Result := Result.ParentNode;
//
//  for I := 1 to Index do
//  begin
//    Result := TNode(Result).GetNextSibling();
//    if (not Assigned(Result)) then
//      raise ERangeError.CreateFmt(sArgumentOutOfRange_Index, [Index, Count]);
//  end;
//end;
//
//{ TCustomSQLParser.TOperation *************************************************}
//
//constructor TCustomSQLParser.TUnaryOperation.Create(const AParentNode: TNode; const AOperator: TToken; const AOperand: TNode);
//begin
//  inherited Create(AParentNode, AOperand.FirstToken);
//
//  FOperator := AOperator;
//  FOperand := AOperand;
//
//  Operator.FParentNode := Self;
//  Operand.FParentNode := Self;
//
//  FLastToken := Operand.LastToken;
//end;
//
//{ TCustomSQLParser.TTwoOperandOperation ***************************************}
//
//constructor TCustomSQLParser.TBinaryOperation.Create(const AParentNode: TNode; const AOperator: TToken; const AOperand1, AOperand2: TNode);
//begin
//  inherited Create(AParentNode, AOperand1.FirstToken);
//
//  FOperator := AOperator;
//  FOperand1 := AOperand1;
//  FOperand2 := AOperand2;
//
//  Operator.FParentNode := Self;
//  Operand1.FParentNode := Self;
//  Operand2.FParentNode := Self;
//
//  FLastToken := Operand2.LastToken;
//end;
//
//{ TCustomSQLParser.TFunction **************************************************}
//
//constructor TCustomSQLParser.TFunction.Create(const AParentNode: TNode; const AIdentifier: TToken; const AArgumentList: TList);
//begin
//  inherited Create(AParentNode, AIdentifier);
//
//  FArgumentList := AArgumentList;
//
//  Identifier.FParentNode := Self;
//  if (Parser.FFunctions.IndexOf(PChar(Identifier.AsString), Length(Identifier.AsString)) < 0) then
//    Identifier.FDbObjectType := dotFunction;
//  ArgumentList.FParentNode := Self;
//
//  FLastToken := FArgumentList.LastToken;
//end;
//
//{ TCustomSQLParser.TBetweenOperation ********************************************}
//
//constructor TCustomSQLParser.TBetweenOperation.Create(const AParentNode: TNode; const AOperator1, AOperator2: TToken; const AExpr, AMin, AMax: TNode);
//begin
//  inherited Create(AParentNode, AExpr.FirstToken);
//
//  FOperator1 := AOperator1;
//  FOperator2 := AOperator2;
//  FExpr := AExpr;
//  FMin := AMin;
//  FMax := AMax;
//
//  Operator2.FParentNode := Self;
//  Operator2.FParentNode := Self;
//  Expr.FParentNode := Self;
//  Min.FParentNode := Self;
//  Max.FParentNode := Self;
//
//  FLastToken := Max.LastToken;
//end;
//
//{ TCustomSQLParser.TTwoOperandOperation ***************************************}
//
//constructor TCustomSQLParser.TSoundsLikeOperation.Create(const AParentNode: TNode;  const AOperator1, AOperator2: TToken; const AOperand1, AOperand2: TNode);
//begin
//  inherited Create(AParentNode, AOperand1.FirstToken);
//
//  FOperator1 := AOperator1;
//  FOperator2 := AOperator2;
//  FOperand1 := AOperand1;
//  FOperand2 := AOperand2;
//
//  Operator1.FParentNode := Self;
//  Operator2.FParentNode := Self;
//  Operand1.FParentNode := Self;
//  Operand2.FParentNode := Self;
//
//  FLastToken := Operand2.LastToken;
//end;
//
//{ TCustomSQLParser.TList ********************************************}
//
//constructor TCustomSQLParser.TList.Create(const AParentNode: TNode; const AFirstToken: TToken);
//var
//  Expression: TNode;
//begin
//  inherited;
//
//  repeat
//    Expression := Stmt.ParseExpression(Self);
//    if (Expression is TToken) then
//      Add(TToken(Expression), TToken(Expression).UsageType)
//    else if (Expression is TNode) then
//      Add(TNode(Expression));
//  until (Stmt.Error or not Assigned(Tokens.Current) or (Tokens.Current.TokenType <> ttComma) or not Tokens.FindNext());
//
//  if (not Assigned(Tokens.Current)) then
//    Stmt.SetError(PE_IncompleteStmt)
//  else if (Tokens.Current.TokenType <> ttCloseBracket) then
//    Stmt.SetError(PE_UnexpectedToken, Tokens.Current)
//  else
//    FLastToken := Expression.LastToken;
//end;
//
{ TCustomSQLParser.TStmt ******************************************************}

function TCustomSQLParser.TStmt.Create(const AParser: TCustomSQLParser; const AStmtType: TStmtType; const AFirstToken: ONode): ONode;
begin
  AParser.NewNode(ntStmt);

  FFirstToken := AFirstToken;
  FStmtType := AStmtType;
end;

function TCustomSQLParser.TStmt.GetError(): Boolean;
begin
  Result := FErrorCode <> PE_Success;
end;

function TCustomSQLParser.TStmt.GetErrorToken(): PToken;
begin
  Result := PToken(FParser.NodePtr(FErrorToken));
end;

function TCustomSQLParser.TStmt.GetFirstToken(): PToken;
begin
  Result := PToken(FParser.NodePtr(FFirstToken));
end;

function TCustomSQLParser.TStmt.GetLastToken(): PToken;
begin
  Result := PToken(FParser.NodePtr(FLastToken));
end;

procedure TCustomSQLParser.TStmt.SetError(const AErrorCode: Integer; const AErrorToken: ONode = 0);
begin
  Assert((AErrorToken = 0) or (FParser.NodePtr(AErrorToken)^.FNodeType = ntToken));

  FErrorCode := AErrorCode;
  FErrorToken := AErrorToken;
end;

//constructor TCustomSQLParser.TStmt.Create(const AStmtList: TStmtList; const AStmtType: TStmtType; const AFirstToken: TToken);
//begin
//  FParser := AStmtList.Parser;
//
//  inherited Create(nil, AFirstToken);
//
//  FErrorCode := PE_Success;
//  FErrorMessage := '';
//  FErrorToken := nil;
//  FStmtType := AStmtType;
//
//  if (FirstToken.TokenType = ttBeginLabel) then
//    FirstToken.Assign(Self, utLabel);
//  Parser.Tokens.Current.Assign(Self, utKeyword);
//end;
//
//function TCustomSQLParser.TStmt.GetError(): Boolean;
//begin
//  Result := ErrorCode <> PE_Success;
//end;
//
//
//procedure TCustomSQLParser.TStmt.SetError(const AErrorCode: Integer; const AErrorToken: TToken = nil);
//begin
//  FErrorCode := AErrorCode;
//  FErrorToken := AErrorToken;
//end;
//
//{ TCustomSQLParser.TUnknownStmt ***********************************************}
//
//constructor TCustomSQLParser.TUnknownStmt.Create(const AStmtList: TStmtList; const AFirstToken: TToken);
//begin
//  inherited Create(AStmtList, stUnknown, AFirstToken);
//
//  while (Tokens.FindNext() and (Tokens.Current.TokenType <> ttDelimiter)) do ;
//
//  FErrorCode := PE_UnkownStmt;
//  FErrorToken := FirstToken;
//
//  if (not Assigned(Tokens.Current)) then
//    FLastToken := Tokens[Tokens.Count - 1]
//  else if (Tokens.Current.TokenType = ttDelimiter) then
//    FLastToken := Tokens.Current.PriorToken
//  else
//    FLastToken := Tokens.Current;
//end;
//
//{ TCustomSQLParser.TSelectStmt ************************************************}
//
//constructor TCustomSQLParser.TSelectStmt.TColumn.Create(const AFields: TColumnList; const AFirstToken: TToken);
//begin
//  FColumns := AFields;
//
//  inherited Create(AFields, AFirstToken);
//
//  FExpression := Stmt.ParseExpression(Self);
//
//  if (Assigned(Tokens.Current) and (Tokens.Current.TokenType <> ttComma) and (Tokens.Current.KeywordIndex <> Parser.kiFROM)) then
//  begin
//    if (Tokens.Current.KeywordIndex = Parser.kiAS) then
//      Tokens.FindNext();
//    if (Tokens.Current.TokenType in [ttIdentifier, ttDQIdentifier, ttBRIdentifier, ttMySQLIdentifier]) then
//    begin
//      Tokens.Current.Assign(Self, utAlias);
//      Tokens.FindNext();
//    end
//    else
//      Stmt.SetError(PE_UnexpectedToken, Tokens.Current);
//  end;
//end;
//
//function TCustomSQLParser.TSelectStmt.TColumn.GetDisplayName(): string;
//begin
//  if (Assigned(Alias)) then
//    Result := Alias.AsString;
//end;
//
//{ TCustomSQLParser.TSelectStmt ************************************************}
//
//constructor TCustomSQLParser.TSelectStmt.TColumnList.Create(const ASelectStmt: TSelectStmt; const AFirstToken: TToken);
//begin
//  inherited Create(ASelectStmt, AFirstToken);
//
//  repeat
//    Add(TColumn.Create(Self, Tokens.Current));
//  until (Stmt.Error or not Assigned(Tokens.Current) or (Tokens.Current.TokenType <> ttComma) or not Tokens.FindNext());
//end;
//
//{ TCustomSQLParser.TSelectStmt ************************************************}
//
//destructor TCustomSQLParser.TSelectStmt.Destroy();
//begin
//  FColumns.Free();
//
//  inherited;
//end;

{ TCustomSQLParser ************************************************************}

constructor TCustomSQLParser.Create(const ASQLDialect: TSQLDialect);
begin
  inherited Create();

  FFunctions := TWordList.Create(Self);
  FHighNotPrecedence := False;
  FKeywords := TWordList.Create(Self);
  FNodes.Mem := nil;
  FNodes.Offset := 0;
  FNodes.Size := 0;
  FParseStmts := True;
  FPipesAsConcat := False;
  FSQLDialect := ASQLDialect;
//  FStmts := TStmtList.Create(Self);
end;

destructor TCustomSQLParser.Destroy();
begin
  FFunctions.Free();
  FKeywords.Free();
  if (FNodes.Size > 0) then
    FreeMem(FNodes.Mem);

  inherited;
end;

function TCustomSQLParser.GetRoot(): PRoot;
begin
  Result := PRoot(NodePtr(FRoot));
end;

function TCustomSQLParser.GetFunctions(): string;
begin
  Result := FFunctions.Text;
end;

function TCustomSQLParser.GetKeywords(): string;
begin
  Result := FKeywords.Text;
end;

function TCustomSQLParser.IsRangeNode(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and not (ANode^.NodeType in [ntUnknown, ntRoot, ntToken]);
end;

function TCustomSQLParser.IsStmtNode(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType = ntStmt);
end;

function TCustomSQLParser.NewNode(const ANodeType: TNodeType): ONode;
var
  AdditionalSize: Integer;
  Size: Integer;
begin
  case (ANodeType) of
    ntRoot: Size := SizeOf(TRoot);
    ntToken: Size := SizeOf(TToken);
    ntStmt: Size := SizeOf(TStmt);
    else raise ERangeError.Create(SArgumentOutOfRange);
  end;

  if (FNodes.Offset + Size > FNodes.Size) then
  begin
    AdditionalSize := Max(FNodes.Offset + Size, FNodes.Size);
    ReallocMem(FNodes.Mem, FNodes.Size + AdditionalSize);
    FillChar(FNodes.Mem[FNodes.Size], AdditionalSize, #0);
    FNodes.Size := FNodes.Size + AdditionalSize;
  end;

  Result := FNodes.Offset;

  Inc(FNodes.Offset, Size);

  NodePtr(Result)^.FNodeType := ANodeType;
  NodePtr(Result)^.FParser := Self;
end;

function TCustomSQLParser.ListPtr(const ANodeOffset: ONode): PList;
begin
  if (ANodeOffset = 0) then
    Result := nil
  else
    Result := @FNodes.Mem[ANodeOffset];
end;

function TCustomSQLParser.NodePtr(const ANodeOffset: ONode): PNode;
begin
  if (ANodeOffset = 0) then
    Result := nil
  else
    Result := @FNodes.Mem[ANodeOffset];
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

  FRoot := 0;
  FMySQLCode := -1;

  FRoot := NewNode(ntRoot);
  PRoot(NodePtr(FRoot))^.FParser := Self;

  Result := True;

  if (FParsePos.Length > 0) then
  begin
    Root^.FFirstToken := ParseToken();
    Result := Result and (Root^.FFirstToken > 0);
    if (Root^.FFirstToken > 0) then
      if (not ParseStmts) then
        Result := Result and (Root^.FirstToken^.ErrorCode = PE_Success)
      else
      begin
        Root^.FFirstStmt := ParseStmt(Root^.FFirstToken);
        Result := Result and (Root^.FFirstStmt > 0) and (Root^.FirstStmt^.ErrorCode = PE_Success);
      end;
  end;

  while (FParsePos.Length > 0) do
  begin
    Root^.FLastToken := ParseToken();
    Result := Result and (Root^.FLastToken > 0);
    if (not ParseStmts) then
      Result := Result and (Root^.LastToken^.ErrorCode = PE_Success)
    else
    begin
      Root^.FLastStmt := ParseStmt(Root^.FLastToken);
      Result := Result and (Root^.FLastStmt > 0) and (Root^.LastStmt^.ErrorCode = PE_Success);
    end;
  end;
end;

function TCustomSQLParser.Parse(const Text: string): Boolean;
begin
  Result := Parse(PChar(Text), Length(Text));
end;

function TCustomSQLParser.ParseCompoundStmt(const AFirstToken, ALabelToken: ONode): ONode;
var
  Token: ONode;
begin
  Result := NewNode(ntStmt);

  Token := AFirstToken;

  repeat
    Token := ParseToken(Result);
    if (Token > 0) then
      if (TokenPtr(Token)^.KeywordIndex <> kiEnd) then
        ParseStmt(Token, True)
      else
      begin
        Token := ParseToken(Result);
        if (Token > 0) then
          case (TokenPtr(Token)^.TokenType) of
            ttDelimiter: ;
            ttEndLabel:
              if (ALabelToken = 0) then
                StmtPtr(Result)^.SetError(PE_UnexpectedToken, Token)
              else if (StrIComp(PChar(TokenPtr(Token).AsString), PChar(TokenPtr(ALabelToken).AsString)) <> 0) then
                StmtPtr(Result)^.SetError(PE_UnexpectedToken, Token);
            else
              StmtPtr(Result)^.SetError(PE_UnexpectedToken, Token);
          end;
      end;
  until ((Token = 0) or (TokenPtr(Token)^.KeywordIndex = kiEND));
end;

function TCustomSQLParser.ParseExpression(const AFirstToken: ONode; const AStmt: ONode): ONode;
const
  MaxNodeCount = 100;
var
  NodeCount: Integer;
  Nodes: array[0 .. MaxNodeCount - 1] of ONode;

  function AddNode(const ANode: ONode): Boolean;
  begin
    if (NodeCount = MaxNodeCount) then
      raise Exception.CreateFmt(STooManyTokensInExpression, [NodeCount]);

    if ((NodeCount = 0)
      or (NodePtr(Nodes[NodeCount - 1])^.FNodeType = ntToken) and (TokenPtr(Nodes[NodeCount - 1])^.OperatorType <> otUnknown)) then
    begin
      if (NodePtr(ANode)^.FNodeType = ntToken) then
        case (TokenPtr(ANode)^.FOperatorType) of
          otUnknown: ;
          otMinus: TokenPtr(ANode)^.FOperatorType := otUnaryMinus;
          otPlus: TokenPtr(ANode)^.FOperatorType := otUnaryPlus;
          otInvertBits,
          otNot1,
          otNot2: ;
          otLike: if ((NodeCount = 0) or (TokenPtr(Nodes[NodeCount - 1])^.OperatorType <> otSounds)) then StmtPtr(AStmt)^.SetError(PE_UnexpectedToken, ANode);
          else StmtPtr(AStmt)^.SetError(PE_UnexpectedToken, ANode);
        end;
      Result := not StmtPtr(AStmt)^.Error;
    end
    else
      Result := (NodePtr(ANode)^.FNodeType = ntToken) and (TokenPtr(ANode)^.OperatorType <> otUnknown);

    if (Result) then
    begin
      Nodes[NodeCount] := ANode;
      Inc(NodeCount);
    end;
  end;

var
  I: Integer;
  Precedence: Integer;
  TokenOffset: ONode;
  SubAreaOffset: ONode;
  EndOfExpression: Boolean;
begin
  NodeCount := 0;

  EndOfExpression := False;
  repeat
    TokenOffset := AFirstToken;

    case (TokenPtr(TokenOffset)^.TokenType) of
      ttComma,
      ttCloseBracket,
      ttDelimiter:
        break;
      ttOperator: ;
      ttInteger,
      ttNumeric,
      ttString:
        begin
          TokenPtr(TokenOffset)^.Assign(AStmt, utConst);
          EndOfExpression := not AddNode(TokenOffset);
        end;
      ttVariable:
        begin
          TokenPtr(TokenOffset)^.Assign(AStmt, utVariable);
          EndOfExpression := not AddNode(TokenOffset);
        end;
      ttIdentifier,
      ttDQIdentifier,
      ttDBIdentifier,
      ttBRIdentifier,
      ttMySQLIdentifier:
        begin
          TokenPtr(TokenOffset)^.Assign(AStmt, utDbObject);
          EndOfExpression := not AddNode(TokenOffset);
        end;
      ttKeyword:
        if (TokenPtr(TokenOffset)^.KeywordIndex = kiBETWEEN) then
          TokenPtr(TokenOffset)^.FOperatorType := otBetween
        else if (TokenPtr(TokenOffset)^.KeywordIndex = kiBINARY) then
          TokenPtr(TokenOffset)^.FOperatorType := otBinary
        else if (TokenPtr(TokenOffset)^.KeywordIndex = kiCOLLATE) then
          TokenPtr(TokenOffset)^.FOperatorType := otCollate
        else if (TokenPtr(TokenOffset)^.KeywordIndex = kiCASE) then
          TokenPtr(TokenOffset)^.FOperatorType := otCase
        else if (TokenPtr(TokenOffset)^.KeywordIndex = kiIN) then
          TokenPtr(TokenOffset)^.FOperatorType := otIn
        else if (TokenPtr(TokenOffset)^.KeywordIndex = kiINTERVAL) then
          TokenPtr(TokenOffset)^.FOperatorType := otInterval
        else if (TokenPtr(TokenOffset)^.KeywordIndex = kiNULL) then
        begin
          TokenPtr(TokenOffset)^.Assign(AStmt, utConst);
          EndOfExpression := not AddNode(TokenOffset);
        end
        else if (TokenPtr(TokenOffset)^.KeywordIndex = kiSOUNDS) then
          TokenPtr(TokenOffset)^.FOperatorType := otSounds
        else if (TokenPtr(TokenOffset)^.KeywordIndex = kiTHEN) then
          TokenPtr(TokenOffset)^.FOperatorType := otThen
        else if (TokenPtr(TokenOffset)^.KeywordIndex = kiWHEN) then
          TokenPtr(TokenOffset)^.FOperatorType := otWhen;
      ttOpenBracket:
        begin
          if (TokenPtr(TokenOffset)^.PriorToken.UsageType = utDbObject) then
            TokenPtr(TokenOffset)^.PriorToken.FOperatorType := otFunction_;
//          SubAreaOffset := ParseSubArea(TokenOffset);
        end;
      else
        PStmt(NodePtr(AStmt))^.SetError(PE_UnexpectedToken, TokenOffset);
    end;
    if (not PStmt(NodePtr(AStmt))^.Error and (PToken(TokenOffset)^.OperatorType <> otUnknown)) then
    begin
      TokenPtr(TokenOffset)^.Assign(AStmt, utOperator);
      EndOfExpression := not AddNode(TokenOffset);
    end;

    if (PStmt(NodePtr(AStmt))^.Error or EndOfExpression) then
      TokenOffset := 0
    else
      TokenOffset := ParseToken(AStmt);
    EndOfExpression := EndOfExpression or (TokenOffset = 0);
  until (StmtPtr(AStmt)^.Error or EndOfExpression);

  for Precedence := 1 to MaxOperatorPrecedence do
  begin
    I := 0;
    while (not StmtPtr(AStmt)^.Error and (I < NodeCount - 1)) do
    begin
//      if ((NodePtr(Nodes[I])^.FNodeType = ntToken) and (OperatorPrecedenceByOperatorType[TokenPtr(Nodes[I])^.OperatorType] = Precedence)) then
//        case (TokenPtr(Nodes[I])^.OperatorType) of
//          otFunction_,
//          otInterval,
//          otBinary,
//          otCollate:
//            if ((I >= NodeCount - 1)
//              or not (NodePtr(Nodes[I + 1])^.FNodeType = ntList)
//              or (TokenPtr(Nodes[I])^.OperatorType = otBinary) and (ListPtr(Nodes[I + 1])^.Count <> 1)) then
//              StmtPtr(AStmt)^.SetError(PE_UnexpectedToken, ListPtr(Nodes[I + 1])^.FNextUsedToken)
//            else
//            begin
//              Nodes[I + 1] := TFunction.Create(AParentNode, TToken(Nodes[I]), TList(Nodes[I + 1]));
//              Dec(NodeCount);
//              Move(Nodes[I + 1], Nodes[I], NodeCount - I);
//            end;
//          otNot1,
//          otUnaryMinus,
//          otUnaryPlus,
//          otInvertBits,
//          otNot2:
//            if ((I >= NodeCount - 1)
//              or (Nodes[I + 1] is TList) and (ListPtr(Nodes[I + 1])^.Count <> 1)) then
//              SetError(PE_UnexpectedToken, TToken(Nodes[I]).NextUsedToken)
//            else
//            begin
//              Nodes[I + 1] := TUnaryOperation.Create(AParentNode, TToken(Nodes[I]), TList(Nodes[I + 1]));
//              Dec(NodeCount);
//              Move(Nodes[I + 1], Nodes[I], NodeCount - I);
//            end;
//          otBitXOR,
//          otMulti,
//          otDivision,
//          otDiv,
//          otMod,
//          otMinus,
//          otPlus,
//          otShiftLeft,
//          otShiftRight,
//          otBitAND,
//          otBitOR,
//          otEqual,
//          otNullSaveEqual,
//          otGreaterEqual,
//          otGreater,
//          otLessEqual,
//          otLess,
//          otNotEqual,
//          otIS,
//          otLike,
//          otRegExp,
//          otIn,
//          otAnd,
//          otXOr,
//          otPipes,
//          otOr:
//            if (I = 0) then
//              StmtPtr(AStmt)^.SetError(PE_UnexpectedToken, Nodes[I])
//            else if (I >= NodeCount - 1) then
//              StmtPtr(AStmt)^.SetError(PE_IncompleteStmt)
//            else
//            begin
//              Nodes[I + 1] := TBinaryOperation.Create(AParentNode, TToken(Nodes[I]), Nodes[I - 1], Nodes[I + 1]);
//              Dec(NodeCount, 2);
//              Move(Nodes[I + 1], Nodes[I - 1], NodeCount - I);
//              Dec(I);
//            end;
//          otDot:
//            if (I = 0) then
//              SetError(PE_UnexpectedToken, TToken(Nodes[I]))
//            else if (I >= NodeCount - 1) then
//              SetError(PE_IncompleteStmt)
//            else if (not (Nodes[I - 1] is TToken) or (TToken(Nodes[I - 1]).UsageType <> utDbObject)) then
//              SetError(PE_UnexpectedToken, TToken(Nodes[I]))
//            else if (not (Nodes[I + 1] is TToken) or (TToken(Nodes[I + 1]).UsageType <> utDbObject)) then
//              SetError(PE_UnexpectedToken, TToken(Nodes[I + 1]))
//            else
//            begin
//              TToken(Nodes[I + 1]).FParentToken := TToken(Nodes[I - 1]);
//              Dec(NodeCount, 2);
//              Move(Nodes[I + 1], Nodes[I - 1], NodeCount - I);
//              Dec(I);
//            end;
//          otBetween:
//            if (NodeCount - 1 < I + 3) then
//              SetError(PE_IncompleteToken, TToken(Nodes[I]))
//            else if (Nodes[I + 2] is TNode) then
//              SetError(PE_UnexpectedToken, TNode(Nodes[I + 2]).FirstToken)
//            else if (TToken(Nodes[I + 2]).OperatorType <> otAnd) then
//              SetError(PE_UnexpectedToken, TToken(Nodes[I + 2]))
//            else
//            begin
//              Nodes[I + 3] := TBetweenOperation.Create(AParentNode, TToken(Nodes[I]), TToken(Nodes[I + 2]), Nodes[I - 1], Nodes[I + 1], Nodes[I + 3]);
//              Dec(NodeCount, 4);
//              Move(Nodes[I + 3], Nodes[I - 1], NodeCount - I);
//              Dec(I);
//            end;
//          otCASE: ; // Todo
//          otSounds:
//            if (NodeCount - 1 < I + 2) then
//              SetError(PE_IncompleteToken, TToken(Nodes[I]))
//            else if (Nodes[I + 1] is TNode) then
//              SetError(PE_UnexpectedToken, TNode(Nodes[I + 1]).FirstToken)
//            else if (TToken(Nodes[I + 1]).OperatorType <> otLike) then
//              SetError(PE_UnexpectedToken, TToken(Nodes[I + 1]))
//            else
//            begin
//              Nodes[I + 2] := TSoundsLikeOperation.Create(AParentNode, TToken(Nodes[I]), TToken(Nodes[I + 1]), Nodes[I - 1], Nodes[I + 2]);
//              Dec(NodeCount, 3);
//              Move(Nodes[I + 2], Nodes[I - 1], NodeCount - I);
//              Dec(I);
//            end;
//          else
//            StmtPtr(AStmt)^.SetError(PE_UnexpectedToken, Nodes[I].FirstToken)
//        end;
      Inc(I);
    end;
  end;

  if (not StmtPtr(AStmt)^.Error and (NodeCount > 1)) then
    StmtPtr(AStmt)^.SetError(PE_Unknown);
  if (StmtPtr(AStmt)^.Error or (NodeCount <> 1)) then
    Result := 0
  else
    Result := Nodes[0];
end;

function TCustomSQLParser.ParseSelectStmt(const AFirstToken: ONode): ONode;
begin
  Result := TSelectStmt.Create(Self, AFirstToken);

//  if (not Tokens.FindNext()) then
//  begin
//    FColumns := nil;
//    SetError(PE_IncompleteStmt);
//  end
//  else
//    FColumns := TColumnList.Create(Self, Tokens.Current);
end;

function TCustomSQLParser.ParseStmt(const AFirstToken: ONode; const SubStmt: Boolean = False): ONode;
var
  KeywordIndex: Integer;
  LabelToken: ONode;
  Token: PToken;
  TokenO: ONode;
begin
  TokenO := AFirstToken;
  while ((TokenO > 0) and not TokenPtr(TokenO)^.IsUsed) do
    TokenO := ParseToken();

  if ((TokenO = 0) or (TokenPtr(TokenO)^.TokenType <> ttBeginLabel)) then
    LabelToken := 0
  else
  begin
    LabelToken := TokenO;

    while ((TokenO > 0) and not TokenPtr(TokenO)^.IsUsed) do
      TokenO := ParseToken();
  end;

  if ((TokenO = 0) or (TokenPtr(TokenO)^.TokenType <> ttKeyword)) then
    KeywordIndex := 0
  else
    KeywordIndex := TokenPtr(TokenO)^.KeywordIndex;

  if (KeywordIndex = kiBEGIN) then
    Result := ParseCompoundStmt(AFirstToken, LabelToken)
  else if (LabelToken > 0) then
    Result := ParseUnknownStmt(AFirstToken)
//  if (KeywordIndex = kiEND) then
//    // Will be handled in caller routine of FindNext
  else if (KeywordIndex = kiSELECT) then
    Result := ParseSelectStmt(AFirstToken)
  else
    Result := ParseUnknownStmt(AFirstToken);

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

function TCustomSQLParser.ParseToken(const AParentNode: ONode = 0): ONode;
label
  TwoChars,
  Selection, SelSpace, SelQuotedIdentifier, SelNotLess, SelNotEqual1, SelNotGreater, SelNot1, SelDoubleQuote, SelComment, SelModulo, SelDolor, SelAmpersand2, SelBitAND, SelSingleQuote, SelOpenBracket, SelCloseBracket, SelMySQLCodeEnd, SelLeftJoin, SelMulti, SelComma, SelDoubleDot, SelDot, SelMySQLCode, SelMLComment, SelDiv, SelNumeric, SelSLComment, SelArrow, SelMinus, SelPlus, SelAssign, SelColon, SelDelimiter, SelNULLSaveEqual, SelLessEqual, SelShiftLeft, SelNotEqual2, SelLess, SelRightJoin, SelEqual, SelGreaterEqual, SelShiftRight, SelGreater, SelParameter, SelAt, SelUnquotedIdentifier, SelDBIdentifier, SelBackslash, SelCloseSquareBracket, SelHat, SelMySQLCharacterSet, SelMySQLIdentifier, SelUnquotedIdentifierLower, SelRBIdentifier, SelPipe, SelBitOR, SelTilde, SelE,
  At,
  BindVariable,
  Colon,
  Comment,
  Intger, IntgerL, IntgerE,
  MLComment, MLCommentL, MLComment2,
  MySQLCharacterSet, MySQLCharacterSetL, MySQLCharacterSetLE, MySQLCharacterSetE,
  MySQLCode, MySQLCodeL,
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
  Code: Integer;
  DotFound: Boolean;
  EFound: Boolean;
  ErrorCode: Integer;
  KeywordIndex: Integer;
  Length: Integer;
  OperatorType: TOperatorType;
  S: string;
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
      CMP ECX,3                        // Three characters in SQL?
      JB SelMLComment                  // No!
      CMP WORD PTR [ESI + 4],'!'       // "/*!" ?
      JE MySQLCode                     // Yes!
    SelMLComment:
      CMP EAX,$002A002F                // "/*" ?
      JE MLComment                     // Yes!
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

    MySQLCode:
      MOV TokenType,ttMySQLCodeStart
      ADD ESI,4                        // Step over "/*" in SQL
      SUB ECX,2                        // Two characters handled
    MySQLCodeL:
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // One character handled
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      MOV AX,[ESI]                     // One Character from SQL to AX
      CMP AX,'0'                       // Digit?
      JB Finish                        // No!
      CMP AX,'9'                       // Digit?
      JA Finish                        // No!
      JMP MySQLCodeL

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
          if (FMySQLCode >= 0) then
          begin
            TokenType := ttUnknown;
            ErrorCode := PE_Syntax;
          end
          else
          begin
            SetString(S, PChar(@SQL[3]), 5);
            Val(S, FMySQLCode, Code);
            if (Code <> 0) then
            begin
              FMySQLCode := -1;
              TokenType := ttUnknown;
              ErrorCode := PE_Syntax;
            end;
          end;
        ttMySQLCodeEnd:
          if (FMySQLCode < 0) then
          begin
            TokenType := ttUnknown;
            ErrorCode := PE_Syntax;
          end
          else
            FMySQLCode := -1;
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

    Result := NewNode(ntToken);

    if (Root^.FLastToken > 0) then
    begin
      TokenPtr(Root^.FLastToken)^.FNextToken := Result;
      TokenPtr(Result)^.FPriorToken := Root^.FLastToken;
    end;

    TokenPtr(Result)^.FErrorCode := ErrorCode;
    TokenPtr(Result)^.FIsUsed := not (TokenType in [ttSpace, ttReturn, ttComment]) and (not (Self is TMySQLSQLParser) or (TMySQLSQLParser(Self).MySQLVersion >= FMySQLCode));
    TokenPtr(Result)^.FKeywordIndex := KeywordIndex;
    TokenPtr(Result)^.FMySQLCode := FMySQLCode;
    TokenPtr(Result)^.FOperatorType := OperatorType;
    TokenPtr(Result)^.FOrigin := FParsePos.Origin;
    TokenPtr(Result)^.FParser := Self;
    TokenPtr(Result)^.FText.SQL := SQL;
    TokenPtr(Result)^.FText.Length := TokenLength;
    TokenPtr(Result)^.FTokenType := TokenType;

    Root^.FLastToken := Result;

    if (AParentNode > 0) then
      TokenPtr(Result)^.Assign(AParentNode);

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

function TCustomSQLParser.ParseUnknownStmt(const AFirstToken: ONode): ONode;
var
  Token: ONode;
begin
  Result := NewNode(ntStmt);

  StmtPtr(Result)^.FParser := Self;
  StmtPtr(Result)^.FFirstToken := AFirstToken;

  Token := AFirstToken;
  if (Token > 0) then
    repeat
      Token := ParseToken(Result);
    until ((Token = 0) or (TokenPtr(Token)^.TokenType = ttDelimiter));

  StmtPtr(Result)^.SetError(PE_UnkownStmt, AFirstToken);

  if (TokenPtr(Root^.FLastToken)^.TokenType = ttDelimiter) then
    StmtPtr(Result)^.FLastToken := TokenPtr(Root^.FLastToken)^.FPriorToken
  else
    StmtPtr(Result)^.FLastToken := Root^.FLastToken;
end;

function TCustomSQLParser.RangeNodePtr(const ANodeOffset: ONode): PRangeNode;
begin
  Assert(NodePtr(ANodeOffset)^.FNodeType <> ntToken);

  Result := PRangeNode(NodePtr(ANodeOffset));
end;

procedure TCustomSQLParser.SaveToFile(const Filename: string; const FileType: TFileType = ftSQL);
var
  FirstToken: PToken;
  Generations: Integer;
  Handle: THandle;
  HTML: string;
  I: Integer;
  J: Integer;
  LastToken: PToken;
  Node: PNode;
  S: string;
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
    '      text-align: center;' + #13#10 +
    '      background-color: #F4F4F4;' + #13#10 +
    '    }' + #13#10 +
    '    .SQL {' + #13#10 +
    '      font-size: 16px;' + #13#10 +
    '      background-color: #F4F4F4;' + #13#10 +
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

  if (not ParseStmts) then
    Stmt := nil
  else
    Stmt := Root^.FirstStmt;
  if (not Assigned(Stmt)) then
  begin
    FirstToken := Root^.FirstToken;
    LastToken := Root^.LastToken;
  end
  else
  begin
    FirstToken := Stmt^.FirstToken;
    LastToken := Stmt^.LastToken;
  end;

  HTML := HTML
    + '<table cellspacing="2" cellpadding="0" border="0">' + #13#10;

  Token := FirstToken; Generations := 0;
  while (Assigned(Token)) do
  begin
    Generations := Max(Generations, Token^.GenerationCount);
    if (Token = LastToken) then
      Token := nil
    else
      Token := Token^.NextToken;
  end;

  for Generation := 0 to Generations - 1 do
  begin
    HTML := HTML
      + '<tr class="Node">' + #13#10;
    Token := FirstToken;
    while (Assigned(Token)) do
    begin
      Node := Token^.Generation[Generation];
      if (IsRangeNode(Node) and (PRangeNode(Node)^.FirstToken = Token)) then
      begin
        HTML := HTML
          + '<td colspan="' + IntToStr(PRangeNode(Node)^.LastToken^.Index - PRangeNode(Node)^.FirstToken^.Index + 1) + '">';
        HTML := HTML
          + '<a href="">'
          + HTMLEscape(NodeTypeToString[Node^.NodeType]);
        if (IsStmtNode(Node)) then
        begin
          HTML := HTML
            + '<span><table cellspacing="2" cellpadding="0">';
          HTML := HTML + '<tr><td>StmtType:</td><td>&nbsp;</td><td>' + StmtTypeToString[PStmt(Node)^.StmtType] + '</td></tr>';
          HTML := HTML
            + '</table></span>';
        end;
        HTML := HTML
          + '</a></td>' + #13#10;
      end;

      if (Token = LastToken) then
        Token := nil
      else
        Token := Token.NextToken;
    end;
    HTML := HTML
      + '</tr>' + #13#10;
  end;


  HTML := HTML
    + '<tr class="SQL">' + #13#10;

  Token := FirstToken;
  while (Assigned(Token)) do
  begin
    HTML := HTML
      + '<td><a href="">';
    HTML := HTML
      + '<code>' + HTMLEscape(ReplaceStr(Token.Text, ' ', '&nbsp;')) + '</code>';
    HTML := HTML
      + '<span><table cellspacing="2" cellpadding="0">';
    HTML := HTML + '<tr><td>Type:</td><td>&nbsp;</td><td>' + HTMLEscape(TokenTypeToString[Token^.TokenType]) + '</td></tr>';
    if (Token^.KeywordIndex >= 0) then
      HTML := HTML + '<tr><td>KeywordIndex:</td><td>&nbsp;</td><td>ki' + HTMLEscape(FKeywords[Token^.KeywordIndex]) + '</td></tr>';
    if (Token^.OperatorType <> otUnknown) then
      HTML := HTML + '<tr><td>OperatorType:</td><td>&nbsp;</td><td>' + HTMLEscape(OperatorTypeToString[Token^.OperatorType]) + '</td></tr>';
    if (Token^.DbObjectType <> dotUnknown) then
      HTML := HTML + '<tr><td>DbObjectType:</td><td>&nbsp;</td><td>' + HTMLEscape(DbObjectTypeToString[Token^.DbObjectType]) + '</td></tr>';
    if ((Trim(Token^.AsString) <> '') and (Token^.KeywordIndex < 0) and (Token^.OperatorType = otUnknown)) then
      HTML := HTML + '<tr><td>AsString:</td><td>&nbsp;</td><td>' + HTMLEscape(Token^.AsString) + '</td></tr>';
    if (Token^.ErrorCode <> PE_Success) then
      HTML := HTML + '<tr><td>ErrorCode:</td><td>&nbsp;</td><td>' + IntToStr(Token^.ErrorCode) + '</td></tr>';
    if (Token^.UsageType <> utUnknown) then
      HTML := HTML + '<tr><td>UsageType:</td><td>&nbsp;</td><td>' + HTMLEscape(UsageTypeToString[Token^.UsageType]) + '</td></tr>';
    HTML := HTML
      + '</table></span>';
    HTML := HTML
      + '</a></td>' + #13#10;

    if (Token = LastToken) then
      Token := nil
    else
      Token := Token.NextToken;
  end;
  HTML := HTML
    + '</tr>' + #13#10;

  HTML := HTML
    + '</table>';

//  for I := 0 to Tokens.Count - 1 do
//  begin
//    for J := 0 to Stmts.Count - 1 do
//      if (Stmts[J].FirstToken = Tokens[I]) then
//      begin
//        Stmt := Stmts[J];
//        if (Stmts[J].ErrorCode <> PE_Success) then
//          HTML := HTML + '<span class="error">'
//        else if (Stmts[J] is TUnknownStmt) then
//          HTML := HTML + '<span class="stmt">'
//        else
//          HTML := HTML + '<span class="plsql">';
//      end;
//
//    if (Tokens[I].ErrorCode <> PE_Success) then
//      HTML := HTML
//        + '<a href="" class="token error">'
//    else
//      HTML := HTML
//        + '<a href="" class="token">';
//    HTML := HTML
//      + HTMLEscape(ReplaceStr(ReplaceStr(Tokens[I].Text, ' ', '&nbsp;'), #13#10, '&nbsp;' + #13#10));
//    if (Tokens[I].ErrorCode <> PE_Success) then
//      HTML := HTML
//        + '<table cellspacing="2" cellpadding="0" class="token" style="background-color: #FCC;">'
//    else
//      HTML := HTML
//        + '<table cellspacing="2" cellpadding="0">';
//    HTML := HTML + '<tr><td>Index:</td><td>&nbsp;</td><td>' + IntToStr(I) + '</td></tr>';
//    HTML := HTML + '<tr><td>Type:</td><td>&nbsp;</td><td>' + HTMLEscape(TokenTypeToString[Tokens[I].TokenType]) + '</td></tr>';
//    Node := Tokens[I].ParentNode;
//    while (Assigned(Node)) do
//    begin
//      HTML := HTML + '<tr><td>Node:</td><td>&nbsp;</td><td>' + HTMLEscape(Node.ClassName) + '</td></tr>';
//      Node := Node.ParentNode;
//    end;
//    if (Assigned(Stmt) and (Stmt.Error)) then
//    begin
//      if (Stmt.ErrorToken = Tokens[I]) then
//        HTML := HTML + '<tr><td>Stmt.ErrorCode:</td><td>&nbsp;</td><td>' + IntToStr(Stmt.ErrorCode) + '</td></tr>'
//      else if (Stmt.FirstToken = Tokens[I]) then
//        HTML := HTML + '<tr><td>Stmt.ErrorCode:</td><td>&nbsp;</td><td>' + IntToStr(Stmt.ErrorCode) + '</td></tr>';
//    end;
//
//    HTML := HTML
//      + '</table></a>';
//
//    for J := 0 to Stmts.Count - 1 do
//      if (Assigned(Stmts[J]) and (Stmts[J].LastToken = Tokens[I])) then
//      begin
//        HTML := HTML + '</span>';
//        Stmt := nil;
//      end;
//  end;

  HTML := HTML +
    '     <br>' + #13#10 +
    '     <br>' + #13#10 +
    '  </body>' + #13#10 +
    '</html>';

  WriteFile(Handle, PChar(BOM_UNICODE_LE)^, 2, Size, nil);

  WriteFile(Handle, PChar(HTML)^, Length(HTML) * SizeOf(Char), Size, nil);

  CloseHandle(Handle);
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

function TCustomSQLParser.StmtPtr(const ANodeOffset: ONode): PStmt;
begin
  Assert(NodePtr(ANodeOffset)^.FNodeType = ntStmt);

  Result := PStmt(NodePtr(ANodeOffset));
end;

function TCustomSQLParser.TokenPtr(const ANodeOffset: ONode): PToken;
begin
  Assert(NodePtr(ANodeOffset)^.FNodeType = ntToken);

  Result := PToken(NodePtr(ANodeOffset));
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
