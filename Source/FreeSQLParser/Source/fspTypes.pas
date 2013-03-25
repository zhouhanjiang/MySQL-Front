unit fspTypes;

interface {********************************************************************}

type
  TNodeType = (
    ntUnknown,         // Unused
    ntRoot,            // Root token, one usage by the parser to handle node tree
    ntToken,           // Token node
    ntRange,           // A node with a range of tokens, base for all further nodes
    ntDeleted,         // Token was deleted, but is still in memory

    ntAlterDatabaseStmt,
    ntBetweenOp,
    ntBinaryOp,
    ntCaseCond,
    ntCaseOp,
    ntCaseOpBranch,
    ntCaseStmt,
    ntCaseStmtBranch,
    ntColumn,
    ntCompoundStmt,
    ntCreateRoutineStmt,
    ntCreateTriggerStmt,
    ntCreateViewStmt,
    ntDataType,
    ntDbIdent,
    ntFunction,
    ntFunctionParam,
    ntGroup,
    ntIfBranch,
    ntIfStmt,
    ntIndexHint,
    ntIterateStmt,
    ntJoin,
    ntLeaveStmt,
    ntList,
    ntOrder,
    ntProcedureParam,
    ntSelectStmt,
    ntSoundsLikeOp,
    ntSubArea,
    ntTable,
    ntTag,
    ntUnaryOp,
    ntUnknownStmt,
    ntUser,
    ntValue
  );
  TNodeTypes = set of TNodeType;

  TStmtType = (
    stUnknown,         // Unused

    stAlterDatabase,
    stCreateFunction,
    stCreateProcedure,
    stCreateTrigger,
    stCreateView,
    stCompound,
    stIf,
    stLoop,
    stRepeat,
    stSelect,
    stWhile
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
    utDbIdent,
    utAlias,
    utMySQLCondCode,
    utPLSQL
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
    ttCSString,               // MySQL Character Set, like _utf8'Hello'
    ttIdent,             // Ident
    ttDQIdent,           // Ident, enclosed in ""
    ttDBIdent,           // Ident, enclosed in []
    ttBRIdent,           // Ident, enclosed in {}
    ttMySQLIdent,        // Ident, enclosed in ``
    ttBeginLabel,             // Lable, like Label_Name:
    ttEndLabel,               // Lable, like Label_Name:
    ttVariable,               // Variable, like @varname
    ttBindVariable,           // Bind Variable, like :bindvarname
    ttMySQLCodeStart,         // MySQL specific code, like /*!50000 SELECT 1; */
    ttMySQLCodeEnd,
    ttOperator,               // Symbol operator, like +, -, &&, *=
    ttAt,                     // "@"
    ttBackslash               // "\", DB2 use
  );
const
  ttIdents = [ttIdent, ttDQIdent, ttDBIdent, ttBRIdent, ttMySQLIdent];
  ttStrings = [ttString, ttCSString];

type
  TOperatorType = (
    otUnknown,

    otFunction_,              // Something like Abs(1.2), will be defined in ParseExpr
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

  TDbIdentType = (
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

implementation {***************************************************************}

end.
