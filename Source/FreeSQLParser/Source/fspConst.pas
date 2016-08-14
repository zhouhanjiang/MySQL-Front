unit fspConst;

interface {********************************************************************}

uses
  fspTypes;

const
  PE_Success = 0; // No error

  PE_Unknown = 1; // Unknown error

  // Bugs while parsing Tokens:
  PE_IncompleteToken = 2; // Incompleted token
  PE_UnexpectedChar = 3; // Unexpected character

  // Bugs while parsing Stmts:
  PE_IncompleteStmt = 4; // Incompleted statement
  PE_UnexpectedToken = 5; // Unexpected token
  PE_ExtraToken = 6; // Token after completed statement

  // Bugs while parsing Root
  PE_UnkownStmt = 7; // Unknown statement

  // Bugs while formating
  PE_InvalidNodeValue = 8; // Invalid node value

  MySQLFunctions =
    'ABS,ACOS,ADD,ADDDATE,ADDTIME,AES_DECRYPT,AES_ENCRYPT,ANALYSE,ASCII,ASIN,' +
    'ATAN,ATAN2,AVG,BIN,BIT_AND,BIT_COUNT,BIT_LENGTH,BIT_OR,BIT_XOR,' +
    'CAST,CEIL,CEILING,CHAR_LENGTH,CHARACTER_LENGTH,COALESCE,CONVERT,' +
    'COERCIBILITY,COMPRESS,CONCAT,CONCAT_WS,CONNECTION_ID,CONV,CONVERT_TZ,' +
    'COS,COT,COUNT,CRC32,CURDATE,CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,' +
    'CURRENT_USER,CURTIME,DATE_ADD,DATE_FORMAT,DATE_SUB,DATEDIFF,DAY,' +
    'DAY_HOUR,DAY_MINUTE,DAY_SECOND,DAYNAME,DAYOFMONTH,DAYOFWEEK,DAYOFYEAR,' +
    'DECODE,DEGREES,ELT,ENCODE,ENCRYPT,EXP,EXPORT_SET,EXTRACT,EXTRACTVALUE,' +
    'FIELD,FIND_IN_SET,FLOOR,FORMAT,FOUND_ROWS,FROM_DAYS,FROM_UNIXTIME,' +
    'GET_FORMAT,GET_LOCK,GREATEST,GROUP_CONCAT,HEX,HOUR,HOUR_MINUTE,' +
    'HOUR_SECOND,IFNULL,INET_ATON,INSERT_ID,INSTR,INTERVAL,IS_FREE_LOCK,' +
    'IS_USED_LOCK,ISNULL,LAST_DAY,LAST_INSERT_ID,LCASE,LEAST,LEFT,LENGTH,' +
    'LINESTRINGFROMTEXT,LINESTRINGFROMWKB,LN,LOAD_FILE,LOCALTIME,' +
    'LOCALTIMESTAMP,LOCATE,LOG,LOG10,LOG2,LOWER,LPAD,LTRIM,MAKE_SET,MAKEDATE,' +
    'MAKETIME,MASTER_POS_LOG,MASTER_POS_WAIT,MAX,MD5,MICROSECOND,MID,MIN,' +
    'MINUTE,MINUTE_SECOND,MOD,MONTH,MONTHNAME,MULTILINESTRINGFROMTEXT,' +
    'MULTILINESTRINGFROMWKB,MULTIPOINTFROMTEXT,MULTIPOINTFROMWKB,' +
    'MULTIPOLYGONFROMTEXT,MULTIPOLYGONFROMWKB,NOW,NULLIF,OCT,OCTET_LENGTH,' +
    'ORD,PASSWORD,PERIOD_ADD,PERIOD_DIFF,PI,POLYGONFROMTEXT,POLYGONFROMWKB,' +
    'POSITION,POW,POWER,QUARTER,QUOTE,RADIANS,RAND,RELEASE_LOCK,REVERSE,' +
    'RIGHT,ROUND,ROW_COUNT,RPAD,RTRIM,SEC_TO_TIME,SECOND,SESSION_USER,SHA,' +
    'SHA1,SHA2,SIGN,SIN,SLEEP,SOUNDEX,SPACE,SPATIAL,SQRT,STD,STDDEV,' +
    'STDDEV_POP,STDDEV_SAMP,STR_TO_DATE,STRCMP,SUBDATE,SUBSTRING,' +
    'SUBSTRING_INDEX,SUBTIME,SUM,SYSDATE,SYSTEM_USER,TAN,TIME_FORMAT,' +
    'TIME_TO_SEC,TIMEDIFF,TIMESTAMPADD,TIMESTAMPDIFF,TO_DAYS,TRIM,TRUNCATE,' +
    'UCASE,UNCOMPRESS,UNCOMPRESSED_LENGTH,UNHEX,UNIX_TIMESTAMP,UPDATEXML,' +
    'UPPER,USER,UTC_DATE,UTC_TIME,UTC_TIMESTAMP,UUID,VAR_POP,VAR_SAMP,' +
    'VARIANCE,VERSION,WEEK,WEEKDAY,WEEKOFYEAR,YEAR_MONTH,YEARWEEK';

  MySQLKeywords =
    'INNODB,INSTANCE,ROTATE,MICROSECOND,' +

    'ACCOUNT,ACTION,ADD,AFTER,AGAINST,AGGREGATE,ALGORITHM,ALL,ALTER,ANALYZE,' +
    'AND,ANY,AS,ASC,ASCII,AT,AUTHORS,AUTO_INCREMENT,AUTOEXTEND_SIZE,' +
    'AVG_ROW_LENGTH,BACKUP,BEFORE,BEGIN,BENCHMARK,BETWEEN,BINARY,BINLOG,BIT,' +
    'BLOCK,BOTH,BTREE,BY,CACHE,CALL,CASCADE,CASCADED,CASE,CATALOG_NAME,CHAIN,' +
    'CHANGE,CHANGED,CHARACTER,CHARSET,CHECK,CHECKSUM,CLASS_ORIGIN,CLIENT,' +
    'CLOSE,COALESCE,CODE,COLLATE,COLLATION,COLUMN,COLUMN_FORMAT,COLUMN_NAME,' +
    'COLUMNS,COMMENT,COMMIT,COMMITTED,COMPACT,COMPLETION,COMPRESSED,' +
    'CONCURRENT,CONDITION,CONNECTION,CONSISTENT,CONSTRAINT,' +
    'CONSTRAINT_CATALOG,CONSTRAINT_NAME,CONSTRAINT_SCHEMA,' +
    'CONSTRAINT_SCHEMA,CONTAINS,CONTENTS,CONTEXT,CONTINUE,CONTRIBUTORS,' +
    'CONVERT,COPY,CPU,CREATE,CROSS,CURRENT,CURRENT_DATE,CURRENT_TIME,' +
    'CURRENT_TIMESTAMP,CURRENT_USER,CURSOR,CURSOR_NAME,DATA,DATABASE,' +
    'DATABASES,DATAFILE,DAY,DAY_HOUR,DAY_MINUTE,DAY_SECOND,DEALLOCATE,DEC,' +
    'DECLARE,DEFAULT,DEFINER,DELAY_KEY_WRITE,DELAYED,DELETE,DESC,DESCRIBE,' +
    'DETERMINISTIC,DIAGNOSTICS,DIRECTORY,DISABLE,DISCARD,DISTINCT,' +
    'DISTINCTROW,DIV,DO,DROP,DUAL,DUMPFILE,DUPLICATE,DYNAMIC,EACH,ELSE,' +
    'ELSEIF,ENABLE,ENCLOSED,END,ENDIF,ENDS,ENGINE,ENGINES,ERRORS,ESCAPE,' +
    'ESCAPED,EVENT,EVENTS,EVERY,EXCHANGE,EXCLUSIVE,EXECUTE,EXISTS,EXIT,' +
    'EXPANSION,EXPIRE,EXPLAIN,EXTENDED,EXTENT_SIZE,FALSE,FAST,FAULTS,FETCH,' +
    'FIELDS,FILE,FIRST,FIXED,FLUSH,FOR,FORCE,FOREIGN,FORMAT,FOUND,FROM,FULL,' +
    'FULLTEXT,FUNCTION,FUNCTIONS,GET,GLOBAL,GOTO,GRANT,GRANTS,GROUP,HANDLER,' +
    'HASH,HAVING,HELP,HIGH_PRIORITY,HOST,HOSTS,HOUR,HOUR_MINUTE,HOUR_SECOND,' +
    'IDENTIFIED,IF,IGNORE,IGNORE_SERVER_IDS,IMPORT,IN,INDEX,INDEXES,INFILE,' +
    'INITIAL_SIZE,INNER,INOUT,INPLACE,INSERT,INSERT_METHOD,INSTALL,INT1,' +
    'INT2,INT3,INT4,INT8,INTERVAL,INTO,INVOKER,IO,IO_THREAD,IPC,IS,' +
    'ISOLATION,ISSUER,ITERATE,JOIN,JSON,KEY,KEY_BLOCK_SIZE,KEYS,KILL,' +
    'LANGUAGE,LAST,LEADING,LEAVE,LEAVES,LEFT,LESS,LEVEL,LIKE,LIMIT,LINEAR,' +
    'LINES,LIST,LOAD,LOCAL,LOCALTIME,LOCALTIMESTAMP,LOCK,LOGFILE,LOGS,LONG,' +
    'LOOP,LOW_PRIORITY,MASTER,MASTER_BIND,MASTER_CONNECT_RETRY,' +
    'MASTER_HEARTBEAT_PERIOD,MASTER_HOST,MASTER_LOG_FILE,MASTER_LOG_POS,' +
    'MASTER_PASSWORD,MASTER_PORT,MASTER_SSL,MASTER_SSL_CA,MASTER_SSL_CAPATH,' +
    'MASTER_SSL_CERT,MASTER_SSL_CIPHER,MASTER_SSL_KEY,' +
    'MASTER_SSL_VERIFY_SERVER_CERT,MASTER_USER,MATCH,' +
    'MAX_CONNECTIONS_PER_HOUR,MAX_QUERIES_PER_HOUR,MAX_ROWS,MAX_SIZE,' +
    'MAX_UPDATES_PER_HOUR,MAX_USER_CONNECTIONS,MAXVALUE,MEDIUM,MEMORY,MERGE,' +
    'MESSAGE_TEXT,MIDDLEINT,MIGRATE,MIN_ROWS,MINUTE,MINUTE_SECOND,MOD,MODE,' +
    'MODIFIES,MODIFY,MONTH,MUTEX,MYSQL_ERRNO,NAME,NAMES,NATIONAL,NATURAL,' +
    'NEVER,NEW,NEXT,NO,NO_WRITE_TO_BINLOG,NODEGROUP,NONE,NOT,NULL,NUMBER,' +
    'OFFLINE,OFFSET,OJ,OLD,ON,ONE,ONLINE,ONLY,OPEN,OPTIMIZE,OPTION,' +
    'OPTIONALLY,OPTIONS,OR,ORDER,OUT,OUTER,OUTFILE,OWNER,PACK_KEYS,PAGE,' +
    'PAGE_CHECKSUM,PARSER,PARTIAL,PARTITION,PARTITIONING,PARTITIONS,' +
    'PASSWORD,PHASE,PLUGIN,PLUGINS,PORT,PREPARE,PRESERVE,PREV,PRIMARY,' +
    'PRIVILEGES,PROCEDURE,PROCESS,PROCESSLIST,PROFILE,PROFILES,PROXY,PURGE,' +
    'QUARTER,QUERY,QUICK,RAID_CHUNKS,RAID_CHUNKSIZE,RAID_TYPE,RANGE,READ,' +
    'READS,REBUILD,RECOVER,REDO_BUFFER_SIZE,REDUNDANT,REFERENCES,REGEXP,' +
    'RELAY_LOG_FILE,RELAY_LOG_POS,RELAYLOG,RELEASE,RELOAD,REMOVE,RENAME,' +
    'REORGANIZE,REPAIR,REPEAT,REPEATABLE,REPLACE,REPLICATION,REQUIRE,RESET,' +
    'RESIGNAL,RESTORE,RESTRICT,RESUME,RETURN,RETURNED_SQLSTATE,RETURNS,' +
    'REVERSE,REVOKE,RIGHT,RLIKE,ROLLBACK,ROLLUP,ROUTINE,ROW,ROW_COUNT,' +
    'ROW_FORMAT,ROWS,SAVEPOINT,SCHEDULE,SCHEMA,SCHEMA_NAME,SCHEMAS,SECOND,' +
    'SECURITY,SELECT,SEPARATOR,SERIALIZABLE,SERVER,SESSION,SET,SHARE,SHARED,' +
    'SHOW,SHUTDOWN,SIGNAL,SIMPLE,SLAVE,SNAPSHOT,SOCKET,SONAME,SOUNDS,SOURCE,' +
    'SPATIAL,SQL,SQL_BIG_RESULT,SQL_BUFFER_RESULT,SQL_CACHE,' +
    'SQL_CALC_FOUND_ROWS,SQL_NO_CACHE,SQL_SMALL_RESULT,SQL_THREAD,' +
    'SQLEXCEPTION,SQLSTATE,SQLWARNING,SQLWARNINGS,SSL,STACKED,START,' +
    'STARTING,STARTS,STATS_AUTO_CALC,STATS_AUTO_RECALC,STATS_PERSISTENT,' +
    'STATUS,STOP,STORAGE,STRAIGHT_JOIN,SUBCLASS_ORIGIN,SUBJECT,SUBPARTITION,' +
    'SUBPARTITIONS,SUPER,SUSPEND,SWAPS,SWITCHES,TABLE,TABLE_NAME,TABLES,' +
    'TABLESPACE,TEMPORARY,TEMPTABLE,TERMINATED,THAN,THEN,TO,TRADITIONAL,' +
    'TRAILING,TRANSACTION,TRIGGER,TRIGGERS,TRUE,TRUNCATE,TYPE,UNCOMMITTED,' +
    'UNDEFINED,UNDO,UNDO_BUFFER_SIZE,UNDOFILE,UNICODE,UNINSTALL,UNION,' +
    'UNIQUE,UNKNOWN,UNLOCK,UNSIGNED,UNTIL,UPDATE,UPGRADE,USAGE,USE,USE_FRM,' +
    'USER,USING,VALUE,VALUES,VARIABLES,VARYING,VIEW,WAIT,WARNINGS,WEEK,WHEN,' +
    'WHERE,WHILE,WITH,WORK,WRAPPER,WRITE,X509,XA,XID,XML,XOR,YEAR,' +
    'YEAR_MONTH,ZEROFILL';

  NodeTypeToString: array[TNodeType] of PChar = (
    'ntUnknown',
    'ntRoot',
    'ntRange',
    'ntToken',

    'ntAnalyzeStmt',
    'ntAlterDatabaseStmt',
    'ntAlterEventStmt',
    'ntAlterInstanceStmt',
    'ntAlterRoutineStmt',
    'ntAlterServerStmt',
    'ntAlterTableStmt',
    'ntAlterTableStmtAlterColumn',
    'ntAlterTableStmtConvertTo',
    'ntAlterTableStmtDropObject',
    'ntAlterTableStmtExchangePartition',
    'ntAlterTableStmtReorganizePartition',
    'ntAlterViewStmt',
    'ntBeginStmt',
    'ntBetweenOp',
    'ntBinaryOp',
    'ntCallStmt',
    'ntCaseOp',
    'ntCaseOpBranch',
    'ntCaseStmt',
    'ntCaseStmtBranch',
    'ntCastFunc',
    'ntCharFunc',
    'ntCheckStmt',
    'ntCheckStmtOption',
    'ntChecksumStmt',
    'ntCloseStmt',
    'ntCommitStmt',
    'ntCompoundStmt',
    'ntConvertFunc',
    'ntCreateDatabaseStmt',
    'ntCreateEventStmt',
    'ntCreateIndexStmt',
    'ntCreateRoutineStmt',
    'ntCreateServerStmt',
    'ntCreateTableStmt',
    'ntCreateTableStmtColumn',
    'ntCreateTableStmtForeignKey',
    'ntCreateTableStmtKey',
    'ntCreateTableStmtKeyColumn',
    'ntCreateTableStmtPartition',
    'ntCreateTableStmtPartitionValues',
    'ntCreateTriggerStmt',
    'ntCreateUserStmt',
    'ntCreateViewStmt',
    'ntCurrentTimestamp',
    'ntDataType',
    'ntDbIdent',
    'ntDeallocatePrepareStmt',
    'ntDeclareStmt',
    'ntDeclareConditionStmt',
    'ntDeclareCursorStmt',
    'ntDeclareHandlerStmt',
    'ntDeclareHandlerStmtCondition',
    'ntDeleteStmt',
    'ntDoStmt',
    'ntDropDatabaseStmt',
    'ntDropEventStmt',
    'ntDropIndexStmt',
    'ntDropRoutineStmt',
    'ntDropServerStmt',
    'ntDropTableStmt',
    'ntDropTriggerStmt',
    'ntDropUserStmt',
    'ntDropViewStmt',
    'ntExecuteStmt',
    'ntExistsFunc',
    'ntExplainStmt',
    'ntExtractFunc',
    'ntFetchStmt',
    'ntFlushStmt',
    'ntFlushStmtOption',
    'ntFunctionCall',
    'ntFunctionReturns',
    'ntGetDiagnosticsStmt',
    'ntGetDiagnosticsStmtStmtInfo',
    'ntGetDiagnosticsStmtConditionInfo',
    'ntGrantStmt',
    'ntGrantStmtPrivileg',
    'ntGrantStmtUserSpecification',
    'ntGroupConcatFunc',
    'ntGroupConcatFuncExpr',
    'ntHelpStmt',
    'ntIfStmt',
    'ntIfStmtBranch',
    'ntIgnoreLines',
    'ntInOp',
    'ntInsertStmt',
    'ntInsertStmtSetItem',
    'ntIntervalOp',
    'ntIntervalListItem',
    'ntIterateStmt',
    'ntKillStmt',
    'ntLeaveStmt',
    'ntLikeOp',
    'ntList',
    'ntLoadDataStmt',
    'ntLoadXMLStmt',
    'ntLockStmt',
    'ntLockStmtItem',
    'ntLoopStmt',
    'ntPositionFunc',
    'ntPrepareStmt',
    'ntPurgeStmt',
    'ntOj',
    'ntOpenStmt',
    'ntOptimizeStmt',
    'ntRegExpOp',
    'ntRenameStmt',
    'ntRenameStmtPair',
    'ntReleaseStmt',
    'ntRepairStmt',
    'ntRepeatStmt',
    'ntResetStmt',
    'ntReturnStmt',
    'ntRevokeStmt',
    'ntRollbackStmt',
    'ntRoutineParam',
    'ntSavepointStmt',
    'ntSchedule',
    'ntSecretIdent',
    'ntSelectStmt',
    'ntSelectStmtColumn',
    'ntSelectStmtFrom',
    'ntSelectStmtGroup',
    'ntSelectStmtGroups',
    'ntSelectStmtOrder',
    'ntSelectStmtInto',
    'ntSelectStmtTableFactor',
    'ntSelectStmtTableFactorIndexHint',
    'ntSelectStmtTableFactorOj',
    'ntSelectStmtTableFactorReferences',
    'ntSelectStmtTableFactorSelect',
    'ntSelectStmtTableJoin',
    'ntSetNamesStmt',
    'ntSetPasswordStmt',
    'ntSetStmt',
    'ntSetStmtAssignment',
    'ntSetTransactionStmt',
    'ntShowAuthorsStmt',
    'ntShowBinaryLogsStmt',
    'ntShowBinlogEventsStmt',
    'ntShowCharacterSetStmt',
    'ntShowCollationStmt',
    'ntShowContributorsStmt',
    'ntShowCountErrorsStmt',
    'ntShowCountWarningsStmt',
    'ntShowCreateDatabaseStmt',
    'ntShowCreateEventStmt',
    'ntShowCreateFunctionStmt',
    'ntShowCreateProcedureStmt',
    'ntShowCreateTableStmt',
    'ntShowCreateTriggerStmt',
    'ntShowCreateViewStmt',
    'ntShowDatabasesStmt',
    'ntShowEngineStmt',
    'ntShowEnginesStmt',
    'ntShowErrorsStmt',
    'ntShowEventsStmt',
    'ntShowFunctionCodeStmt',
    'ntShowFunctionStatusStmt',
    'ntShowGrantsStmt',
    'ntShowIndexStmt',
    'ntShowMasterStatusStmt',
    'ntShowOpenTablesStmt',
    'ntShowPluginsStmt',
    'ntShowPrivilegesStmt',
    'ntShowProcedureCodeStmt',
    'ntShowProcedureStatusStmt',
    'ntShowProcessListStmt',
    'ntShowProfileStmt',
    'ntShowProfilesStmt',
    'ntShowRelaylogEventsStmt',
    'ntShowSlaveHostsStmt',
    'ntShowSlaveStatusStmt',
    'ntShowStatusStmt',
    'ntShowTableStatusStmt',
    'ntShowTablesStmt',
    'ntShowTriggersStmt',
    'ntShowVariablesStmt',
    'ntShowWarningsStmt',
    'ntShutdownStmt',
    'ntSignalStmt',
    'ntSignalStmtInformation',
    'ntSoundsLikeOp',
    'ntStartSlaveStmt',
    'ntStartTransactionStmt',
    'ntStopSlaveStmt',
    'ntSubArea',
    'ntSubPartition',
    'ntSubstringFunc',
    'ntTableReference',
    'ntTag',
    'ntTransactionStmtCharacteristic',
    'ntTrimFunc',
    'ntTruncateStmt',
    'ntUnaryOp',
    'ntUnknownStmt',
    'ntUnlockStmt',
    'ntUpdateStmt',
    'ntUser',
    'ntUseStmt',
    'ntValue',
    'ntVariable',
    'ntWeightStringFunc',
    'ntWeightStringFuncLevel',
    'ntWhileStmt',
    'ntXAStmt',
    'ntXID'
  );

  StmtTypeToString: array[TStmtType] of PChar = (
    'stAnalyze',
    'stAlterDatabase',
    'stAlterEvent',
    'stAlterInstance',
    'stAlterRoutine',
    'stAlterServer',
    'stAlterTable',
    'stAlterView',
    'stBegin',
    'stCall',
    'stCase',
    'stCheck',
    'stChecksum',
    'stClose',
    'stCommit',
    'stCompound',
    'stCreateDatabase',
    'stCreateEvent',
    'stCreateIndex',
    'stCreateRoutine',
    'stCreateServer',
    'stCreateTable',
    'stCreateTrigger',
    'stCreateUser',
    'stCreateView',
    'stDeallocatePrepare',
    'stDeclare',
    'stDeclareCondition',
    'stDeclareCursor',
    'stDeclareHandler',
    'stDelete',
    'stDo',
    'stDropDatabase',
    'stDropEvent',
    'stDropIndex',
    'stDropRoutine',
    'stDropServer',
    'stDropTable',
    'stDropTrigger',
    'stDropUser',
    'stDropView',
    'stExecute',
    'stExplain',
    'stFetch',
    'stFlush',
    'stGetDiagnostics',
    'stGrant',
    'stHelp',
    'stIf',
    'stInsert',
    'stIterate',
    'stKill',
    'stLeave',
    'stLoadData',
    'stLoadXML',
    'stLock',
    'stLoop',
    'stPrepare',
    'stPurge',
    'stOpen',
    'stOptimize',
    'stRename',
    'stRelease',
    'stRepair',
    'stRepeat',
    'stReset',
    'stReturn',
    'stRevoke',
    'stRollback',
    'stSavepoint',
    'stSelect',
    'stSetNames',
    'stSetPassword',
    'stSet',
    'stSetTransaction',
    'stShowAuthors',
    'stShowBinaryLogs',
    'stShowBinlogEvents',
    'stShowCharacterSet',
    'stShowCollation',
    'stShowContributors',
    'stShowCountErrors',
    'stShowCountWarnings',
    'stShowCreateDatabase',
    'stShowCreateEvent',
    'stShowCreateFunction',
    'stShowCreateProcedure',
    'stShowCreateTable',
    'stShowCreateTrigger',
    'stShowCreateView',
    'stShowDatabases',
    'stShowEngine',
    'stShowEngines',
    'stShowErrors',
    'stShowEvents',
    'stShowFunctionCode',
    'stShowFunctionStatus',
    'stShowGrants',
    'stShowIndex',
    'stShowMasterStatus',
    'stShowOpenTables',
    'stShowPlugins',
    'stShowPrivileges',
    'stShowProcedureCode',
    'stShowProcedureStatus',
    'stShowProcessList',
    'stShowProfile',
    'stShowProfiles',
    'stShowRelaylogEvents',
    'stShowSlaveHosts',
    'stShowSlaveStatus',
    'stShowStatus',
    'stShowTableStatus',
    'stShowTables',
    'stShowTriggers',
    'stShowVariables',
    'stShowWarnings',
    'stShutdown',
    'stSignal',
    'stStartSlave',
    'stStartTransaction',
    'stStopSlave',
    'stTruncate',
    'stUnknown',
    'stUnlock',
    'stUpdate',
    'stUse',
    'stWhile',
    'stXA'
  );

  TokenTypeToString: array[TTokenType] of PChar = (
    'ttUnknown',
    'ttSyntaxError',
    'ttSpace',
    'ttReturn',
    'ttLineComment',
    'ttMultiLineComment',
    'ttDot',
    'ttColon',
    'ttDelimiter',
    'ttComma',
    'ttOpenBracket',
    'ttCloseBracket',
    'ttOpenCurlyBracket',
    'ttCloseCurlyBracket',
    'ttInteger',
    'ttNumeric',
    'ttString',
    'ttCSString',
    'ttIdent',
    'ttDQIdent',
    'ttDBIdent',
    'ttMySQLIdent',
    'ttBeginLabel',
    'ttEndLabel',
    'ttBindVariable',
    'ttMySQLCodeStart',
    'ttMySQLCodeEnd',
    'ttOperator',
    'ttAt',
    'ttBackslash',
    'ttIPAddress'
  );

  UsageTypeToString: array[TUsageType] of PChar = (
    'utUnknown',
    'utSyntaxError',
    'utWhiteSpace',
    'utComment',
    'utSymbol',
    'utKeyword',
    'utLabel',
    'utOperator',
    'utConst',
    'utFunction',
    'utDbIdent',
    'utPL_SQL'
  );

  UnaryOperators = [otBinary, otDistinct, otUnaryNot, otUnaryMinus, otUnaryPlus, otInvertBits];

  OperatorTypeToString: array[TOperatorType] of PChar = (
    'otUnknown',

    'otDot',

    'otInterval',

    'otBinary',
    'otCollate',
    'otDistinct',

    'otUnaryNot',

    'otUnaryMinus',
    'otUnaryPlus',
    'otInvertBits',

    'otBitXOR',

    'otMulti',
    'otDivision',
    'otDiv',
    'otMod',

    'otMinus',
    'otPlus',

    'otShiftLeft',
    'otShiftRight',

    'otBitAND',

    'otBitOR',

    'otEqual',
    'otNullSaveEqual',
    'otGreaterEqual',
    'otGreater',
    'otLessEqual',
    'otLess',
    'otNotEqual',
    'otIS',
    'otSounds',
    'otLike',
    'otRegExp',
    'otIn',

    'otBetween',
    'otCASE',

    'otNot',

    'otAnd',

    'otXOr',

    'otPipes',
    'otOr',

    'otEscape',

    'otAssign',
    'otAssign2',
    'otHat',
    'otDoubleDot',
    'otArrow',
    'otParameter'
  );

  DbIdentTypeToString: array[TDbIdentType] of PChar = (
    'ditUnknown',
    'ditTable',
    'ditKey',
    'ditColumn',
    'ditForeignKey',
    'ditFunction',
    'ditProcedure',
    'ditTrigger',
    'ditDatabase',
    'ditParameter',
    'ditEvent',
    'ditPartition',
    'ditServer',
    'ditCursor'
  );

  OperatorPrecedenceByOperatorType: array[TOperatorType] of Integer = (
    0,   // otUnknown

    1,   // otDot

    2,   // otInterval

    3,   // otBinary
    3,   // otCollate
    3,   // otDistinct

    4,   // otUnaryNot

    5,   // otUnaryMinus
    5,   // otUnaryPlus
    5,   // otInvertBits

    6,   // otHat

    7,   // otMulti
    7,   // otDivision
    7,   // otDiv
    7,   // otMod

    8,   // otMinus
    8,   // otPlus

    9,   // otShiftLeft
    9,   // otShiftRight

    9,   // otBitAND

    10,  // otBitOR

    11,  // otEqual
    11,  // otNullSaveEqual
    11,  // otGreaterEqual
    11,  // otGreater
    11,  // otLessEqual
    11,  // otLess
    11,  // otNotEqual
    11,  // otIS
    11,  // otSounds
    11,  // otLike
    11,  // otRegExp
    11,  // otIn

    12,  // otBetween
    12,  // otCASE

    13,  // otNot

    14,  // otAnd

    15,  // otXOr

    16,  // otPipes
    16,  // otOr

    17,   // otAssign
    17,   // otAssign2

    0,   // otEscape
    0,   // otBitXOR
    0,   // otDoubleDot
    0,   // otArrow
    0    // otParameter
  );
  MaxOperatorPrecedence = 17;

var
  UsageTypeByTokenType: array[TTokenType] of TUsageType = (
    utUnknown,
    utSyntaxError,
    utWhiteSpace,
    utWhiteSpace,
    utComment,
    utComment,
    utSymbol,
    utSymbol,
    utSymbol,
    utSymbol,
    utSymbol,
    utSymbol,
    utConst,
    utConst,
    utConst,
    utConst,
    utConst,
    utConst,
    utDbIdent,
    utDbIdent,
    utDbIdent,
    utDbIdent,
    utLabel,
    utLabel,
    utUnknown,
    utSymbol,
    utSymbol,
    utSymbol,
    utSymbol,
    utSymbol,
    utConst
  );

  NodeTypeByStmtType: array[TStmtType] of TNodeType = (
    ntAnalyzeStmt,
    ntAlterDatabaseStmt,
    ntAlterEventStmt,
    ntAlterInstanceStmt,
    ntAlterRoutineStmt,
    ntAlterServerStmt,
    ntAlterTableStmt,
    ntAlterViewStmt,
    ntBeginStmt,
    ntCallStmt,
    ntCaseStmt,
    ntCheckStmt,
    ntChecksumStmt,
    ntCloseStmt,
    ntCommitStmt,
    ntCompoundStmt,
    ntCreateDatabaseStmt,
    ntCreateEventStmt,
    ntCreateIndexStmt,
    ntCreateRoutineStmt,
    ntCreateServerStmt,
    ntCreateTableStmt,
    ntCreateTriggerStmt,
    ntCreateUserStmt,
    ntCreateViewStmt,
    ntDeallocatePrepareStmt,
    ntDeclareStmt,
    ntDeclareConditionStmt,
    ntDeclareCursorStmt,
    ntDeclareHandlerStmt,
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
    ntExplainStmt,
    ntFetchStmt,
    ntFlushStmt,
    ntGetDiagnosticsStmt,
    ntGrantStmt,
    ntHelpStmt,
    ntIfStmt,
    ntInsertStmt,
    ntIterateStmt,
    ntKillStmt,
    ntLeaveStmt,
    ntLoadDataStmt,
    ntLoadXMLStmt,
    ntLockStmt,
    ntLoopStmt,
    ntPrepareStmt,
    ntPurgeStmt,
    ntOpenStmt,
    ntOptimizeStmt,
    ntRenameStmt,
    ntReleaseStmt,
    ntRepairStmt,
    ntRepeatStmt,
    ntResetStmt,
    ntReturnStmt,
    ntRevokeStmt,
    ntRollbackStmt,
    ntSavepointStmt,
    ntSelectStmt,
    ntSetNamesStmt,
    ntSetPasswordStmt,
    ntSetStmt,
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
    ntStartSlaveStmt,
    ntStartTransactionStmt,
    ntStopSlaveStmt,
    ntTruncateStmt,
    ntUnknownStmt,
    ntUnlockStmt,
    ntUpdateStmt,
    ntUseStmt,
    ntWhileStmt,
    ntXAStmt
  );

const
  StmtNodeTypes = [
    ntAnalyzeStmt,
    ntAlterDatabaseStmt,
    ntAlterEventStmt,
    ntAlterInstanceStmt,
    ntAlterRoutineStmt,
    ntAlterServerStmt,
    ntAlterTableStmt,
    ntAlterViewStmt,
    ntBeginStmt,
    ntCallStmt,
    ntCaseStmt,
    ntCheckStmt,
    ntChecksumStmt,
    ntCloseStmt,
    ntCommitStmt,
    ntCompoundStmt,
    ntCreateDatabaseStmt,
    ntCreateEventStmt,
    ntCreateIndexStmt,
    ntCreateRoutineStmt,
    ntCreateServerStmt,
    ntCreateTableStmt,
    ntCreateTriggerStmt,
    ntCreateUserStmt,
    ntCreateViewStmt,
    ntDeallocatePrepareStmt,
    ntDeclareStmt,
    ntDeclareConditionStmt,
    ntDeclareCursorStmt,
    ntDeclareHandlerStmt,
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
    ntExplainStmt,
    ntFetchStmt,
    ntFlushStmt,
    ntGetDiagnosticsStmt,
    ntGrantStmt,
    ntHelpStmt,
    ntIfStmt,
    ntInsertStmt,
    ntIterateStmt,
    ntKillStmt,
    ntLeaveStmt,
    ntLoadDataStmt,
    ntLoadXMLStmt,
    ntLockStmt,
    ntLoopStmt,
    ntPrepareStmt,
    ntPurgeStmt,
    ntOpenStmt,
    ntOptimizeStmt,
    ntRenameStmt,
    ntReleaseStmt,
    ntRepairStmt,
    ntRepeatStmt,
    ntResetStmt,
    ntReturnStmt,
    ntRevokeStmt,
    ntRollbackStmt,
    ntSavepointStmt,
    ntSelectStmt,
    ntSetNamesStmt,
    ntSetPasswordStmt,
    ntSetStmt,
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
    ntStartSlaveStmt,
    ntStartTransactionStmt,
    ntStopSlaveStmt,
    ntTruncateStmt,
    ntUnknownStmt,
    ntUnlockStmt,
    ntUpdateStmt,
    ntUseStmt,
    ntWhileStmt,
    ntXAStmt
  ];

  JoinTypeToString: array[TJoinType] of PChar = (
    'jtUnknown',
    'jtInner',
    'jtCross',
    'jtStraight',
    'jtEqui',
    'jtLeft',
    'jtRight',
    'jtNaturalLeft',
    'jtNaturalRight'
  );

  RoutineTypeToString: array[TRoutineType] of PChar = (
    'rtUnknown',
    'rtFunction',
    'rtProcedure'
  );

implementation {***************************************************************}

{$IFDEF Debug}
var
  Max: Integer;
  OperatorType: TOperatorType;
{$ENDIF}
initialization
  {$IFDEF Debug}
    Max := 0;
    for OperatorType := Low(TOperatorType) to High(TOperatorType) do
      if (OperatorPrecedenceByOperatorType[OperatorType] > Max) then
        Max := OperatorPrecedenceByOperatorType[OperatorType];
    Assert(Max = MaxOperatorPrecedence);
  {$ENDIF}
end.
