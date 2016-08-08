unit fspTypes;

interface {********************************************************************}

type

  TErrorCode = (
    PE_Success = 0, // No error

    PE_Unknown = 1, // Unknown error

    // Bugs while parsing Tokens:
    PE_IncompleteToken = 2, // Incompleted token
    PE_UnexpectedChar = 3, // Unexpected character

    // Bugs while parsing Stmts:
    PE_IncompleteStmt = 4, // Incompleted statement
    PE_UnexpectedToken = 5, // Unexpected token
    PE_ExtraToken = 6, // Token after completed statement

    // Bugs while parsing Root
    PE_UnkownStmt = 7 // Unknown statement
  );

  TNodeType = (
    ntUnknown,         // Unused
    ntRoot,            // Root token, one usage by the parser to handle stmt list
    ntRange,           // A node with a range of tokens, base for all further nodes
    ntToken,           // Token node

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
    ntCharFunc,
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
    ntCreateUserStmt,
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
    ntDropUserStmt,
    ntDropViewStmt,
    ntExecuteStmt,
    ntExistsFunc,
    ntExplainStmt,
    ntExtractFunc,
    ntFetchStmt,
    ntFlushStmt,
    ntFlushStmtOption,
    ntFunctionCall,
    ntFunctionReturns,
    ntGetDiagnosticsStmt,
    ntGetDiagnosticsStmtStmtInfo,
    ntGetDiagnosticsStmtConditionInfo,
    ntGrantStmt,
    ntGrantStmtPrivileg,
    ntGrantStmtUserSpecification,
    ntGroupConcatFunc,
    ntGroupConcatFuncExpr,
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
    ntKillStmt,
    ntLeaveStmt,
    ntLikeOp,
    ntList,
    ntLoadDataStmt,
    ntLoadXMLStmt,
    ntLockStmt,
    ntLockStmtItem,
    ntLoopStmt,
    ntPositionFunc,
    ntPrepareStmt,
    ntOj,
    ntOpenStmt,
    ntRegExpOp,
    ntRenameStmt,
    ntRenameStmtPair,
    ntReleaseStmt,
    ntRepeatStmt,
    ntReturnStmt,
    ntResetStmt,
    ntRevokeStmt,
    ntRollbackStmt,
    ntRoutineParam,
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
    ntSelectStmtTableJoin,
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
    ntShutdownStmt,
    ntSignalStmt,
    ntSignalStmtInformation,
    ntSoundsLikeOp,
    ntStartTransactionStmt,
    ntSubArea,
    ntSubPartition,
    ntSubstringFunc,
    ntTableReference,
    ntTag,
    ntTransactionStmtCharacteristic,
    ntTrimFunc,
    ntTruncateStmt,
    ntUnaryOp,
    ntUnknownStmt,
    ntUnlockStmt,
    ntUpdateStmt,
    ntUser,
    ntUseStmt,
    ntValue,
    ntVariable,
    ntWeightStringFunc,
    ntWeightStringFuncLevel,
    ntWhileStmt,
    ntXAStmt,
    ntXID
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
    stCreateUser,
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
    stDropUser,
    stDropView,
    stExecute,
    stExplain,
    stFetch,
    stFlush,
    stGetDiagnostics,
    stGrant,
    stHelp,
    stIf,
    stInsert,
    stIterate,
    stKill,
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
    stReset,
    stRevoke,
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
    stShutdown,
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
    utPL_SQL
  );

  TTokenType = (
    ttUnknown,
    ttSyntaxError,            // Error while parsing token
    ttSpace,                  // Tab and Space
    ttReturn,                 // New line
    ttComment,                // Comment, like # comment, -- comment or /* this is multi line comment */
    ttDot,                    // "."
    ttColon,                  // ":"
    ttDelimiter,              // ";"
    ttComma,                  // ","
    ttOpenBracket,            // "("
    ttCloseBracket,           // ")"
    ttOpenCurlyBracket,       // "{"
    ttCloseCurlyBracket,      // "}"
    ttInteger,                // Tnteger constant, like 123456
    ttNumeric,                // Numeric (float) constant, like -123.456E15
    ttString,                 // String constant, enclosed in ''
    ttCSString,               // MySQL Character Set, like _utf8'Hello'
    ttIdent,                  // Ident
    ttDQIdent,                // Ident, enclosed in ""
    ttDBIdent,                // Ident, enclosed in []
    ttMySQLIdent,             // Ident, enclosed in ``
    ttBeginLabel,             // Label, like Label_Name:
    ttEndLabel,               // Label, like Label_Name:
    ttBindVariable,           // Bind Variable, like :bindvarname
    ttMySQLCodeStart,         // MySQL specific code, like /*!50000 SELECT 1; */
    ttMySQLCodeEnd,
    ttOperator,               // Symbol operator like +, -, &&, *=
    ttAt,                     // "@"
    ttBackslash               // "\", DB2 use
  );
const
  ttStrings = [ttString, ttCSString];

type
  TOperatorType = (
    otUnknown,

    otDot,                    // "."

    otInterval,               // "INTERVAL"

    otBinary,                 // "BINARY"
    otCollate,                // "COLLATE"
    otDistinct,               // "DISTINCT"

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
    ditCursor
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
