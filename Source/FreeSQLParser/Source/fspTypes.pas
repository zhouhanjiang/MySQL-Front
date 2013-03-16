unit fspTypes;

interface {********************************************************************}

type
  ONode = Integer;

  TSQLDialect = (sdStandard, sdMySQL);

  TNodeType = (
    ntUnknown,
    ntDeleted,
    ntRoot,
    ntToken,
    ntRangeNode,
    ntSibling,
    ntSiblings,
    ntExpressions,
    ntColumns,
    ntColumn,
    ntDbIdentifier,
    ntFunction,
    ntUnaryOp,
    ntBinaryOp,
    ntUser,
    ntBetweenOp,
    ntCaseCond,
    ntCaseOp,
    ntSoundsLikeOp,
    ntTable,
    ntJoin,
    ntTables,
    ntIndexHint,
    ntIndexHints,
    ntGroup,
    ntGroups,
    ntOrder,
    ntOrders,
    ntPLSQLCondPart,
    ntUnknownStmt,
    ntCreateViewStmt,
    ntCompoundStmt,
    ntIfStmt,
    ntSelectStmt,
    ntStmts,
    ntTag,
    ntValue
  );
  TNodeTypes = set of TNodeType;

  TStmtType = (
    stUnknown,
    stCreateFunction,
    stCreateProcedure,
    stCreateView,
    stCompound,
    stIF,
    stLOOP,
    stREPEAT,
    stSELECT,
    stWHILE
  );

  TUsageType = (
    utUnknown,
    utWhiteSpace,
    utComment,
    utSymbol,
    utKeyword,
    utLabel,
    utOperator,
    utSign,
    utConst,
    utVariable,
    utFunction,
    utDbIdentifier,
    utAlias
  );

  TTokenType = (
    ttUnknown,
    ttSpace,                  // Tab and Space
    ttReturn,                 // New line
    ttComment,                // Comment, like # comment, -- comment or /* this is multi line comment */
    ttComma,                  // ","
    ttOpenBracket,            // "("
    ttCloseBracket,           // ")"
    ttOpenCurlyBracket,       // "{"
    ttCloseCurlyBracket,      // "}"
    ttDelimiter,              // ";"
    ttInteger,                // Tnteger constant, like 123456
    ttNumeric,                // Numeric (float) constant, like -123.456E15
    ttString,                 // String constant, enclosed in ''
    ttIdentifier,             // Identifier
    ttBeginLabel,             // Lable, like Label_Name:
    ttEndLabel,               // Lable, like Label_Name:
    ttVariable,               // Variable, like @varname
    ttBindVariable,           // Bind Variable, like :bindvarname
    ttDQIdentifier,           // Identifier, enclosed in ""
    ttDBIdentifier,           // Identifier, enclosed in []
    ttBRIdentifier,           // Identifier, enclosed in {}
    ttMySQLIdentifier,        // Identifier, enclosed in ``
    ttMySQLCodeStart,         // MySQL specific code, like /*!50000 SELECT 1; */
    ttMySQLCodeEnd,
    ttCSString,               // MySQL Character Set, like _utf8'Hello'
    ttOperator,               // Symbol operator, like +, -, &&, *=
    ttAt,                     // "@"
    ttBackslash,              // "\", DB2 use
    ttKeyword
  );
const
  ttIdentifiers = [ttIdentifier, ttDQIdentifier, ttDBIdentifier, ttBRIdentifier, ttMySQLIdentifier];

type
  TOperatorType = (
    otUnknown,

    otFunction_,              // Something like Abs(1.2), will be defined in ParseExpression
    otInterval,               // "INTERVAL"
    otBinary,                 // "BINARY"
    otCollate,                // "COLLATE"

    otNot1,                   // "!"

    otUnaryMinus,             // "-"
    otUnaryPlus,              // "+"
    otInvertBits,             // "~"
    otDot,                    // "."

    otBitXOR,                 // "^"

    otMulti,                  // "*"
    otDivision,               // "/"
    otDiv,                    // "DIV"
    otMod,                    // "%", "MOD"

    otMinus,                  // "-"
    otPlus,                   // "+"

    otShiftLeft,              // "<<
    otShiftRight,             // ">>"

    otBitAND,                 // "&"

    otBitOR,                  // "|"

    otEqual,                  // "="
    otNullSaveEqual,          // "<=>"
    otGreaterEqual,           // ">=", "!<"
    otGreater,                // ">"
    otLessEqual,              // "<=", "!>"
    otLess,                   // "<"
    otNotEqual,               // "!=", "<>"
    otIS,                     // "IS"
    otSounds,                 // "SOUNDS"
    otLike,                   // "LIKE"
    otRegExp,                 // "REGEXP", "RLIKE"
    otIn,                     // "IN"

    otBetween,                // "BETWEEN"
    otCASE,                   // "CASE"
    otWHEN,                   // "WHEN"
    otIF,                     // "IF"
    otTHEN,                   // "THEN"
    otELSE,                   // "ELSE"
    otELSEIF,                 // "ELSEIF"

    otNot2,                   // "NOT"

    otAnd,                    // "&&", "AND"

    otXOr,                    // "XOR"

    otPipes,                  // "||"
    otOr,                     // "OR"


    otAssign,                 // "="
    otAssign2,                // ":="
    otHat,                    // "^"
    otDoubleDot,              // ".."
    otArrow,                  // "->"
    otParameter               // "?"
  );

  TDbIdentifierType = (
    ditUnknown,
    ditTable,
    ditIndex,
    ditField,
    ditAllFields,
    ditFunction,
    ditProcedure,
    ditTrigger,
    ditView,
    ditDatabase,
    ditParameter,
    ditLocalVariable,
    ditEvent,
    ditPartition
  );

  TJoinType = (
    jtUnknown,
    jtInner,
    jtCross,
    jtEqui,
    jtLeft,
    jtRight,
    jtNaturalLeft,
    jtNaturalRight
  );

  TSubAreaType = (
    satSelectStmt,
    satExpressions,
    satPartitionIdentifiers,
    satIndexIdentifiers,
    satTableReferences,
    satColumnIdentifiers
  );
  TSubAreaTypes = set of TSubAreaType;

  TFileType = (ftSQL, ftDebugHTML);

implementation {***************************************************************}

end.
