unit fspTypes;

interface {********************************************************************}

type
  TNodeType = (
    ntUnknown,         // Unused
    ntRoot,            // Root token, one usage by the parser to handle node tree
    ntToken,           // Token node
    ntRange,           // A node with a range of tokens, base for all further nodes

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
    ntCastFunc,
    ntCloseStmt,
    ntCommitStmt,
    ntCompoundStmt,
    ntConvertFunc,
    ntCreateDatabaseStmt,
    ntCreateEventStmt,
    ntCreateIndexStmt,
    ntCreateRoutineStmt,
    ntCreateServerStmt,
    ntCreateTableStmt,
    ntCreateTableStmtColumn,
    ntCreateTableStmtForeignKey,
    ntCreateTableStmtKey,
    ntCreateTableStmtKeyColumn,
    ntCreateTableStmtPartition,
    ntCreateTableStmtPartitionValues,
    ntCreateTriggerStmt,
    ntCreateViewStmt,
    ntDataType,
    ntDbIdent,
    ntDeallocatePrepareStmt,
    ntDeclareStmt,
    ntDeclareConditionStmt,
    ntDeclareCursorStmt,
    ntDeclareHandlerStmt,
    ntDeclareHandlerStmtCondition,
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
    ntExecuteStmt,
    ntExplainStmt,
    ntFetchStmt,
    ntFunctionCall,
    ntFunctionReturns,
    ntGetDiagnosticsStmt,
    ntGetDiagnosticsStmtStmtInfo,
    ntGetDiagnosticsStmtConditionInfo,
    ntGrantStmt,
    ntGrantStmtPrivileg,
    ntGrantStmtUserSpecification,
    ntHelpStmt,
    ntIfStmt,
    ntIfStmtBranch,
    ntIgnoreLines,
    ntInOp,
    ntInsertStmt,
    ntInsertStmtSetItem,
    ntInterval,
    ntIntervalListItem,
    ntIterateStmt,
    ntJoin,
    ntLeaveStmt,
    ntLikeOp,
    ntList,
    ntLoadDataStmt,
    ntLoadXMLStmt,
    ntLockStmt,
    ntLockStmtItem,
    ntLoopStmt,
    ntPrepareStmt,
    ntOpenStmt,
    ntRenameStmt,
    ntRenameStmtPair,
    ntReleaseStmt,
    ntRepeatStmt,
    ntReturnStmt,
    ntRoutineParam,
    ntRollbackStmt,
    ntSavepointStmt,
    ntSchedule,
    ntSecretIdent,
    ntSelectStmt,
    ntSelectStmtColumn,
    ntSelectStmtFrom,
    ntSelectStmtGroup,
    ntSelectStmtGroups,
    ntSelectStmtOrder,
    ntSelectStmtInto,
    ntSelectStmtTableFactor,
    ntSelectStmtTableFactorIndexHint,
    ntSelectStmtTableFactorOj,
    ntSelectStmtTableFactorReferences,
    ntSelectStmtTableFactorSelect,
    ntSetNamesStmt,
    ntSetPasswordStmt,
    ntSetStmt,
    ntSetStmtAssignment,
    ntSetTransactionStmt,
    ntShowAuthorsStmt,
    ntShowBinaryLogsStmt,
    ntShowBinlogEventsStmt,
    ntShowCharacterSetStmt,
    ntShowCollationStmt,
    ntShowContributorsStmt,
    ntShowCountErrorsStmt,
    ntShowCountWarningsStmt,
    ntShowCreateDatabaseStmt,
    ntShowCreateEventStmt,
    ntShowCreateFunctionStmt,
    ntShowCreateProcedureStmt,
    ntShowCreateTableStmt,
    ntShowCreateTriggerStmt,
    ntShowCreateViewStmt,
    ntShowDatabasesStmt,
    ntShowEngineStmt,
    ntShowEnginesStmt,
    ntShowErrorsStmt,
    ntShowEventsStmt,
    ntShowFunctionCodeStmt,
    ntShowFunctionStatusStmt,
    ntShowGrantsStmt,
    ntShowIndexStmt,
    ntShowMasterStatusStmt,
    ntShowOpenTablesStmt,
    ntShowPluginsStmt,
    ntShowPrivilegesStmt,
    ntShowProcedureCodeStmt,
    ntShowProcedureStatusStmt,
    ntShowProcessListStmt,
    ntShowProfileStmt,
    ntShowProfilesStmt,
    ntShowRelaylogEventsStmt,
    ntShowSlaveHostsStmt,
    ntShowSlaveStatusStmt,
    ntShowStatusStmt,
    ntShowTableStatusStmt,
    ntShowTablesStmt,
    ntShowTriggersStmt,
    ntShowVariablesStmt,
    ntShowWarningsStmt,
    ntSignalStmt,
    ntSignalStmtInformation,
    ntSoundsLikeOp,
    ntStartTransactionStmt,
    ntSubArea,
    ntSubPartition,
    ntTableReference,
    ntTag,
    ntTransactionCharacteristic,
    ntTruncateStmt,
    ntUnaryOp,
    ntUnknownStmt,
    ntUnlockStmt,
    ntUpdateStmt,
    ntUser,
    ntUseStmt,
    ntValue,
    ntVariable,
    ntWhileStmt,
    ntXAStmt
  );

  TStmtType = (
    stAlterDatabase,
    stAlterEvent,
    stAlterRoutine,
    stAlterServer,
    stAlterTable,
    stAlterView,
    stBegin,
    stCall,
    stCase,
    stClose,
    stCommit,
    stCompound,
    stCreateDatabase,
    stCreateEvent,
    stCreateIndex,
    stCreateRoutine,
    stCreateServer,
    stCreateTable,
    stCreateTrigger,
    stCreateView,
    stDeallocatePrepare,
    stDeclare,
    stDeclareCondition,
    stDeclareCursor,
    stDeclareHandler,
    stDelete,
    stDo,
    stDropDatabase,
    stDropEvent,
    stDropIndex,
    stDropRoutine,
    stDropServer,
    stDropTable,
    stDropTrigger,
    stDropView,
    stExecute,
    stExplain,
    stFetch,
    stGetDiagnostics,
    stGrant,
    stHelp,
    stIf,
    stInsert,
    stIterate,
    stLeave,
    stLoadData,
    stLoadXML,
    stLock,
    stLoop,
    stPrepare,
    stOpen,
    stRename,
    stRelease,
    stRepeat,
    stReturn,
    stRollback,
    stSavepoint,
    stSelect,
    stSetNames,
    stSetPassword,
    stSet,
    stSetTransaction,
    stShowAuthors,
    stShowBinaryLogs,
    stShowBinlogEvents,
    stShowCharacterSet,
    stShowCollation,
    stShowContributors,
    stShowCountErrors,
    stShowCountWarnings,
    stShowCreateDatabase,
    stShowCreateEvent,
    stShowCreateFunction,
    stShowCreateProcedure,
    stShowCreateTable,
    stShowCreateTrigger,
    stShowCreateView,
    stShowDatabases,
    stShowEngine,
    stShowEngines,
    stShowErrors,
    stShowEvents,
    stShowFunctionCode,
    stShowFunctionStatus,
    stShowGrants,
    stShowIndex,
    stShowMasterStatus,
    stShowOpenTables,
    stShowPlugins,
    stShowPrivileges,
    stShowProcedureCode,
    stShowProcedureStatus,
    stShowProcessList,
    stShowProfile,
    stShowProfiles,
    stShowRelaylogEvents,
    stShowSlaveHosts,
    stShowSlaveStatus,
    stShowStatus,
    stShowTableStatus,
    stShowTables,
    stShowTriggers,
    stShowVariables,
    stShowWarnings,
    stSignal,
    stStartTransaction,
    stTruncate,
    stUnknown,
    stUnlock,
    stUpdate,
    stUse,
    stWhile,
    stXA
  );

  TUsageType = (
    utUnknown,
    utSyntaxError,
    utWhiteSpace,
    utComment,
    utSymbol,
    utKeyword,
    utLabel,
    utOperator,
    utConst,
    utFunction,
    utDbIdent,
    utPLSQL
  );

  TTokenType = (
    ttUnknown,
    ttSyntaxError,            // Error while parsing token
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
    ttIdent,                  // Ident
    ttDQIdent,                // Ident, enclosed in ""
    ttDBIdent,                // Ident, enclosed in []
    ttBRIdent,                // Ident, enclosed in {}
    ttMySQLIdent,             // Ident, enclosed in ``
    ttBeginLabel,             // Label, like Label_Name:
    ttEndLabel,               // Label, like Label_Name:
    ttBindVariable,           // Bind Variable, like :bindvarname
    ttMySQLCodeStart,         // MySQL specific code, like /*!50000 SELECT 1; */
    ttMySQLCodeEnd,
    ttOperator,               // Symbol operator, like +, -, &&, *=
    ttAt,                     // "@"
    ttBackslash,              // "\", DB2 use
    ttColon,                  // ":"
    ttDot                     // "."
  );
const
  ttIdents = [ttIdent, ttMySQLIdent];
  ttStrings = [ttString, ttCSString];

type
  TOperatorType = (
    otUnknown,

    otDot,                    // "."

    otInterval,               // "INTERVAL"

    otBinary,                 // "BINARY"
    otCollate,                // "COLLATE"

    otUnaryNot,               // "!"

    otUnaryMinus,             // "-"
    otUnaryPlus,              // "+"
    otInvertBits,             // "~"

    otHat,                    // "^"

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

    otNot,                    // "NOT"

    otAnd,                    // "&&", "AND"

    otXOr,                    // "XOR"

    otPipes,                  // "||"
    otOr,                     // "OR"

    otAssign,                 // "="
    otAssign2,                // ":="

    otEscape,                 // "ESCAPE"
    otBitXOR,                 // "^"
    otDoubleDot,              // ".."
    otArrow,                  // "->"
    otParameter               // "?"
  );

  TDbIdentType = (
    ditUnknown,
    ditAlias,
    ditTable,
    ditKey,
    ditColumn,
    ditForeignKey,
    ditFunction,
    ditProcedure,
    ditTrigger,
    ditDatabase,
    ditParameter,
    ditEvent,
    ditPartition,
    ditServer,
    ditXA,
    ditCursor,
    ditVariable
  );

  TJoinType = (
    jtUnknown,
    jtInner,
    jtCross,
    jtStraight,
    jtEqui,
    jtLeft,
    jtRight,
    jtNaturalLeft,
    jtNaturalRight
  );

  TRoutineType = (
    rtUnknown,
    rtFunction,
    rtProcedure
  );

implementation {***************************************************************}

end.
