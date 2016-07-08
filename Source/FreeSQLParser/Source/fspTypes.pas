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
    ntAlterEventStmt,
    ntAlterRoutineStmt,
    ntAlterServerStmt,
    ntAlterTableStmt,
    ntAlterTableStmtAlterColumn,
    ntAlterTableStmtConvertTo,
    ntAlterTableStmtDropObject,
    ntAlterTableStmtExchangePartition,
    ntAlterTableStmtReorganizePartition,
    ntAlterViewStmt,
    ntBeginStmt,
    ntBetweenOp,
    ntBinaryOp,
    ntCallStmt,
    ntCaseOp,
    ntCaseOpBranch,
    ntCaseStmt,
    ntCaseStmtBranch,
    ntCloseStmt,
    ntColumn,
    ntCommitStmt,
    ntCompoundStmt,
    ntCreateDatabaseStmt,
    ntCreateEventStmt,
    ntCreateIndexStmt,
    ntCreateRoutineStmt,
    ntCreateServerStmt,
    ntCreateTableStmt,
    ntCreateTableStmtReference,
    ntCreateTriggerStmt,
    ntCreateViewStmt,
    ntDataType,
    ntDbIdent,
    ntDeclareStmt,
    ntDeleteStmt,
    ntDoStmt,
    ntDropDatabaseStmt,
    ntDropEventStmt,
    ntDropIndexStmt,
    ntDropRoutineStmt,
    ntDropServerStmt,
    ntDropTableStmt,
    ntDropTriggerStmt,
    ntDropViewStmt,
    ntFetchStmt,
    ntForeignKey,
    ntFunction,
    ntIfStmt,
    ntIfStmtBranch,
    ntIgnoreLines,
    ntIndex,
    ntIndexColumn,
    ntInsertStmt,
    ntIterateStmt,
    ntLeaveStmt,
    ntList,
    ntLoadDataStmt,
    ntLoadXMLStmt,
    ntLockStmt,
    ntLockStmtItem,
    ntLoopStmt,
    ntOpenStmt,
    ntPartition,
    ntPartitionValues,
    ntRenameStmt,
    ntRenameStmtPair,
    ntReleaseStmt,
    ntRepeatStmt,
    ntRoutineParam,
    ntRollbackStmt,
    ntSavepointStmt,
    ntSchedule,
    ntScheduleInterval,
    ntScheduleIntervalListItem,
    ntSelectStmt,
    ntSelectStmtField,
    ntSelectStmtGroup,
    ntSelectStmtGroups,
    ntSelectStmtJoin,
    ntSelectStmtOrder,
    ntSelectStmtTableFactor,
    ntSelectStmtTableFactorIndexHint,
    ntSelectStmtTableFactorOj,
    ntSelectStmtTableFactorReferences,
    ntSelectStmtTableFactorSelect,
    ntSetPasswordStmt,
    ntSetStmt,
    ntSetStmtAssignment,
    ntSetTransactionStmt,
    ntShowAuthorsStmt,
    ntShowBinaryLogsStmt,
    ntSoundsLikeOp,
    ntStartTransactionStmt,
    ntSubArea,
    ntSubPartition,
    ntTag,
    ntTransactionCharacteristic,
    ntTruncateStmt,
    ntUnaryOp,
    ntUnknownStmt,
    ntUnlockStmt,
    ntUpdateStmt,
    ntUser,
    ntValue,
    ntVariable,
    ntWhileStmt,
    ntXAStmt
  );
  TNodeTypes = set of TNodeType;

  TStmtType = (
    stUnknown,

    stAlterDatabase,
    stAlterEvent,
    stAlterFunction,
    stAlterProcedure,
    stAlterServer,
    stAlterTable,
    stAlterView,
    stBegin,
    stCall,
    stClose,
    stCommit,
    stCreateDatabase,
    stCreateEvent,
    stCreateFunction,
    stCreateIndex,
    stCreateProcedure,
    stCreateServer,
    stCreateTable,
    stCreateTrigger,
    stCreateView,
    stCompound,
    stDeclare,
    stDelete,
    stDo,
    stDropDatabase,
    stDropEvent,
    stDropFunction,
    stDropIndex,
    stDropProcedure,
    stDropServer,
    stDropTable,
    stDropTrigger,
    stDropView,
    stFetch,
    stIf,
    stInsert,
    stLoadData,
    stLoadXML,
    stLock,
    stLoop,
    stOpen,
    stRenameTable,
    stRepeat,
    stRelease,
    stReplace,
    stRollback,
    stSavepoint,
    stSelect,
    stSet,
    stSetPassword,
    stSetTransaction,
    stShowAuthors,
    stShowBinaryLogs,
    stStartTransaction,
    stTruncate,
    stUnlock,
    stUpdate,
    stWhile,
    stXA
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
    utFunction,
    utDbIdent,
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
    ttIdentifier,             // Identifier
    ttDQIdentifier,           // Identifier, enclosed in ""
    ttDBIdentifier,           // Identifier, enclosed in []
    ttBRIdentifier,           // Identifier, enclosed in {}
    ttMySQLIdentifier,        // Identifier, enclosed in ``
    ttBeginLabel,             // Lable, like Label_Name:
    ttEndLabel,               // Lable, like Label_Name:
    ttBindVariable,           // Bind Variable, like :bindvarname
    ttMySQLCodeStart,         // MySQL specific code, like /*!50000 SELECT 1; */
    ttMySQLCodeEnd,
    ttOperator,               // Symbol operator, like +, -, &&, *=
    ttAt,                     // "@"
    ttBackslash               // "\", DB2 use
  );
const
  ttIdentifiers = [ttIdentifier, ttDQIdentifier, ttDBIdentifier, ttBRIdentifier, ttMySQLIdentifier];

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

  TDbIdentifierType = (
    ditUnknown,
    ditCharacterSet,
    ditCollate,
    ditEngine,
    ditAlias,
    ditTable,
    ditIndex,
    ditColumn,
    ditAllFields,
    ditFunction,
    ditProcedure,
    ditTrigger,
    ditView,
    ditDatabase,
    ditParameter,
    ditEvent,
    ditPartition,
    ditServer,
    ditVariable,
    ditXA,
    ditCursor
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
