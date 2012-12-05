unit fspTypes;

interface {********************************************************************}

type
  TSQLDialect = (sdStandard, sdMySQL);

  TNodeType = (
    ntUnknown,
    ntRoot,
    ntToken,
    ntStmtNode,
    ntSibling,
    ntList,
    ntStmt
  );

  TStmtType = (
    stUnknown,
    stSELECT,
    stCompound
  );

  TUsageType = (
    utUnknown,
    utSymbol,
    utKeyword,
    utLabel,
    utOperator,
    utSign,
    utConst,
    utVariable,
    utFunction,
    utDbObject,
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
    ttMySQLCharacterSet,      // MySQL Character Set, like _utf8
    ttOperator,               // Symbol operator, like +, -, &&, *=
    ttAt,                     // "@"
    ttBackslash,              // "\", DB2 use
    ttKeyword
  );

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
    otTHEN,                   // "THEN"
    otELSE,                   // "ELSE"

    otNot2,                   // "NOT"

    otAnd,                    // "&&", "AND"

    otXOr,                    // "XOR"

    otPipes,                  // "||"
    otOr,                     // "OR"


    otAssign1,                // "="
    otAssign2,                // ":="
    otLeftJoin,               // "*="
    otRightJoin,              // "=*"
    otHat,                    // "^"
    otDoubleDot,              // ".."
    otArrow,                  // "->"
    otParameter               // "?"
  );

  TDbObjectType = (
    dotUnknown,
    dotTable,
    dotTableCTE,
    dotTableTemp,
    dotTablePivot,
    dotTableVar,
    dotField,
    dotTableAlias,
    dotFieldAlias,
    dotAlias,
    dotDataType,
    dotFunction,
    dotProcedure,
    dotTrigger,
    dotView,
    dotIndex,
    dotOracleExceptionName,
    dotOracleHint,
    dotDatabase,
    dotServer,
    dotSequence,
    dotSequenceVal,
    dotParameter,
    dotLocalVariable,
    dotPackage,
    dotObjectProperty,
    dotObjectMethod,
    dotMaterializedView
  );

  TFileType = (ftSQL, ftDebugHTML);

implementation {***************************************************************}

end.
