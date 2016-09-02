unit fspTypes;

interface {********************************************************************}

type

  TNodeType = (
    ntUnknown,         // Unused
    ntRoot,            // Root token, one usage by the parser to handle stmt list
    ntRange,           // A node with a range of tokens, base for all further nodes
    ntToken,           // Token node

    ntAnalyzeStmt,
    ntAlterDatabaseStmt,
    ntAlterEventStmt,
    ntAlterInstanceStmt,
    ntAlterRoutineStmt,
    ntAlterServerStmt,
    ntAlterTableStmt,
    ntAlterTableStmtAlterColumn,
    ntAlterTableStmtConvertTo,
    ntAlterTableStmtDropObject,
    ntAlterTableStmtExchangePartition,
    ntAlterTableStmtReorganizePartition,
    ntAlterViewStmt,
    ntBeginLabel,
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
    ntCheckStmt,
    ntCheckStmtOption,
    ntChecksumStmt,
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
    ntCreateTableStmtField,
    ntCreateTableStmtForeignKey,
    ntCreateTableStmtKey,
    ntCreateTableStmtKeyColumn,
    ntCreateTableStmtPartition,
    ntCreateTableStmtPartitionValues,
    ntCreateTriggerStmt,
    ntCreateUserStmt,
    ntCreateViewStmt,
    ntCurrentTimestamp,
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
    ntEndLabel,
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
    ntGetDiagnosticsStmtCondInfo,
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
    ntIntervalOp,
    ntIntervalOpListItem,
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
    ntMatchFunc,
    ntPositionFunc,
    ntPrepareStmt,
    ntPurgeStmt,
    ntOpenStmt,
    ntOptimizeStmt,
    ntRegExpOp,
    ntRenameStmt,
    ntRenameStmtPair,
    ntReleaseStmt,
    ntRepairStmt,
    ntRepeatStmt,
    ntResetStmt,
    ntReturnStmt,
    ntRevokeStmt,
    ntRollbackStmt,
    ntRoutineParam,
    ntSavepointStmt,
    ntSchedule,
    ntSecretIdent,
    ntSelectStmt,
    ntSelectStmtColumn,
    ntSelectStmtGroup,
    ntSelectStmtOrder,
    ntSelectStmtTableFactor,
    ntSelectStmtTableFactorIndexHint,
    ntSelectStmtTableFactorOj,
    ntSelectStmtTableFactorSelect,
    ntSelectStmtTableJoin,
    ntSetNamesStmt,
    ntSetPasswordStmt,
    ntSetStmt,
    ntSetStmtAssignment,
    ntSetTransactionStmt,
    ntSetTransactionStmtCharacteristic,
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
    ntShowCreateUserStmt,
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
    ntStartSlaveStmt,
    ntStartTransactionStmt,
    ntStopSlaveStmt,
    ntSubArea,
    ntSubAreaSelectStmt,
    ntSubPartition,
    ntSubstringFunc,
    ntTag,
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
    ntXAStmtID
  );

  TStmtType = (
    stAnalyze,
    stAlterDatabase,
    stAlterEvent,
    stAlterInstance,
    stAlterRoutine,
    stAlterServer,
    stAlterTable,
    stAlterView,
    stBegin,
    stCall,
    stCase,
    stCheck,
    stChecksum,
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
    stPurge,
    stOpen,
    stOptimize,
    stRename,
    stRelease,
    stRepair,
    stRepeat,
    stReset,
    stReturn,
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
    stShowCreateUser,
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
    stStartSlave,
    stStartTransaction,
    stStopSlave,
    stSubAreaSelect,
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
    utError,
    utWhiteSpace,
    utComment,
    utSymbol,
    utKeyword,
    utLabel,
    utOperator,
    utDataType,
    utConst,
    utFunction,
    utDbIdent,
    utPL_SQL
  );

  TTokenType = (
    ttUnknown,
    ttError,                  // Error while parsing token
    ttSpace,                  // <Tab> and <Space>
    ttReturn,                 // <CarriageReturn>
    ttSLComment,              // Comment, like # comment, -- comment
    ttMLComment,              // Comment, like /* this is a multi line comment */
    ttDot,                    // "."
    ttColon,                  // ":"
    ttDelimiter,              // ";"
    ttComma,                  // ","
    ttOpenBracket,            // "("
    ttCloseBracket,           // ")"
    ttOpenCurlyBracket,       // "{"
    ttCloseCurlyBracket,      // "}"
    ttInteger,                // Integer constant, like -123456
    ttNumeric,                // Numeric (float) constant, like -123.456E78
    ttString,                 // String constant, enclosed in ''
    ttCSString,               // MySQL Character Set, like _utf8'Hello'
    ttIdent,                  // Ident
    ttDQIdent,                // Ident, enclosed in ""
    ttDBIdent,                // Ident, enclosed in []
    ttMySQLIdent,             // Ident, enclosed in ``
    ttBindVariable,           // Bind Variable, like :bindvarname
    ttMySQLCodeStart,         // MySQL specific code, like /*!50000 SELECT 1; */
    ttMySQLCodeEnd,
    ttOperator,               // Symbol operator like +, -, &&, *=
    ttAt,                     // "@"
    ttBackslash,              // "\", DB2 use
    ttIPAddress               // "123.123.123.123"
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
    ditTable,
    ditKey,
    ditField,
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
