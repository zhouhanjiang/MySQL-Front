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
  PE_UnkownStmt = 6; // Unknown statement
  PE_InvalidEndLabel = 7; // Begin and end tokens are different

  MySQLFunctions =
    'CONVERT,' +

    'ABS,ACOS,ADD,ADDDATE,ADDTIME,AES_DECRYPT,AES_ENCRYPT,ANALYSE,ASCII,ASIN,' +
    'ATAN,ATAN2,AVG,BIN,BIT_AND,BIT_COUNT,BIT_LENGTH,BIT_OR,BIT_XOR,' +
    'CAST,CEIL,CEILING,CHAR_LENGTH,CHARACTER_LENGTH,COALESCE,COERCIBILITY,' +
    'COMPRESS,CONCAT,CONCAT_WS,CONNECTION_ID,CONV,CONVERT_TZ,COS,COT,COUNT,' +
    'CRC32,CURDATE,CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,CURRENT_USER,' +
    'CURTIME,DATE_ADD,DATE_FORMAT,DATE_SUB,DATEDIFF,DAY,DAY_HOUR,DAY_MINUTE,' +
    'DAY_SECOND,DAYNAME,DAYOFMONTH,DAYOFWEEK,DAYOFYEAR,DECODE,DEGREES,ELT,' +
    'ENCODE,ENCRYPT,EXP,EXPORT_SET,EXTRACT,EXTRACTVALUE,FIELD,FIND_IN_SET,' +
    'FLOOR,FORMAT,FOUND_ROWS,FROM_DAYS,FROM_UNIXTIME,GET_FORMAT,GET_LOCK,' +
    'GREATEST,GROUP_CONCAT,HEX,HOUR,HOUR_MINUTE,HOUR_SECOND,IFNULL,' +
    'INET_ATON,INSERT_ID,INSTR,INTERVAL,IS_FREE_LOCK,IS_USED_LOCK,ISNULL,' +
    'LAST_DAY,LAST_INSERT_ID,LCASE,LEAST,LEFT,LENGTH,LINESTRINGFROMTEXT,' +
    'LINESTRINGFROMWKB,LN,LOAD_FILE,LOCALTIME,LOCALTIMESTAMP,LOCATE,LOG,' +
    'LOG10,LOG2,LOWER,LPAD,LTRIM,MAKE_SET,MAKEDATE,MAKETIME,MASTER_POS_LOG,' +
    'MASTER_POS_WAIT,MAX,MD5,MICROSECOND,MID,MIN,MINUTE,MINUTE_SECOND,MONTH,' +
    'MONTHNAME,MULTILINESTRINGFROMTEXT,MULTILINESTRINGFROMWKB,' +
    'MULTIPOINTFROMTEXT,MULTIPOINTFROMWKB,MULTIPOLYGONFROMTEXT,' +
    'MULTIPOLYGONFROMWKB,NOW,NULLIF,OCT,OCTET_LENGTH,ORD,PASSWORD,PERIOD_ADD,' +
    'PERIOD_DIFF,PI,POLYGONFROMTEXT,POLYGONFROMWKB,POSITION,POW,POWER,' +
    'QUARTER,QUOTE,RADIANS,RAND,RELEASE_LOCK,REVERSE,RIGHT,ROUND,' +
    'ROW_COUNT,RPAD,RTRIM,SEC_TO_TIME,SECOND,SESSION_USER,SHA,SHA1,SHA2,SIGN,' +
    'SIN,SLEEP,SOUNDEX,SPACE,SPATIAL,SQRT,STD,STDDEV,STDDEV_POP,STDDEV_SAMP,' +
    'STR_TO_DATE,STRCMP,SUBDATE,SUBSTRING,SUBSTRING_INDEX,SUBTIME,SUM,' +
    'SYSDATE,SYSTEM_USER,TAN,TIME_FORMAT,TIME_TO_SEC,TIMEDIFF,TIMESTAMPADD,' +
    'TIMESTAMPDIFF,TO_DAYS,TRIM,TRUNCATE,UCASE,UNCOMPRESS,' +
    'UNCOMPRESSED_LENGTH,UNHEX,UNIX_TIMESTAMP,UPDATEXML,UPPER,USER,UTC_DATE,' +
    'UTC_TIME,UTC_TIMESTAMP,UUID,VAR_POP,VAR_SAMP,VARIANCE,VERSION,WEEK,' +
    'WEEKDAY,WEEKOFYEAR,YEAR_MONTH,YEARWEEK';

  MySQLKeywords =
    'BINARY,INTERVAL,LEFT,RIGHT,MERGE,CURRENT_USER,UNSIGNED,ZEROFILL,DAY,' +
    'DAY_HOUR,DAY_MINUTE,DAY_SECOND,HOUR,HOUR_MINUTE,HOUR_SECOND,MONTH,' +
    'MINUTE,MINUTE_SECOND,QUARTER,SECOND,WEEK,YEAR,YEAR_MONTH,ADD,PASSWORD,' +
    'SPATIAL,STATS_AUTO_CALC,STATS_AUTO_RECALC,STATS_PERSISTENT,USER,BTREE,' +
    'HASH,COALESCE,EXCHANGE,TRUNCATE,COMPACT,COMPRESSED,DYNAMIC,FIXED,' +
    'OPTIONAL,REDUNDANT,XML,ONLY,MIGRATE,RESUME,SUSPEND,XA,ONE,PHASE,' +
    'RECOVER,BLOCK,CONTEXT,CPU,FAULTS,INDEXES,IO,IPC,MEMORY,PAGE,' +
    'SOURCE,SWAPS,SWITCHES,CURRENT_TIMESTAMP,LOCALTIME,LOCALTIMESTAMP,' +
    'CURRENT_DATE,CURRENT_TIME,' +

    'INPLACE,SHARED,EXCLUSIVE,ACTION,AFTER,AGAINST,AGGREGATE,ALGORITHM,ALL,ALTER,ANALYZE,AND,ANY,AS,' +
    'ASC,AT,AUTHORS,AUTO_INCREMENT,AUTOEXTEND_SIZE,AVG_ROW_LENGTH,BACKUP,' +
    'BEFORE,BEGIN,BENCHMARK,BETWEEN,BINLOG,BIT,BOTH,BY,CACHE,CALL,CASCADE,' +
    'CASCADED,CASE,CATALOG_NAME,CHAIN,CHANGE,CHANGED,CHARACTER,CHARSET,CHECK,' +
    'CHECKSUM,CLASS_ORIGIN,CLIENT,CLOSE,CODE,COLLATE,COLLATION,COLUMN,' +
    'COLUMN_FORMAT,COLUMN_NAME,COLUMNS,COMMENT,COMMIT,COMMITTED,COMPLETION,' +
    'CONCURRENT,CONDITION,CONNECTION,CONSISTENT,CONSTRAINT,REPEAT,' +
    'CONSTRAINT_CATALOG,CONSTRAINT_NAME,CONSTRAINT_SCHEMA,CONTAINS,CONTENTS,' +
    'CONTINUE,CONTRIBUTORS,CONVERT,COPY,CREATE,CROSS,CURSOR,CURSOR_NAME,DATA,' +
    'DATABASE,DATABASES,DATAFILE,DEALLOCATE,DEC,DECLARE,DEFAULT,DEFINER,' +
    'DELAY_KEY_WRITE,DELAYED,DELETE,DESC,DESCRIBE,DETERMINISTIC,DIRECTORY,' +
    'DISABLE,DISCARD,DISTINCT,DISTINCTROW,DIV,DO,DROP,DUAL,DUMPFILE,' +
    'DUPLICATE,EACH,ELSE,ELSEIF,ENABLE,ENCLOSED,END,ENDIF,ENDS,ENGINE,' +
    'ENGINES,ERRORS,ESCAPE,ESCAPED,EVENT,EVENTS,EVERY,EXECUTE,EXISTS,EXIT,' +
    'EXPANSION,EXPLAIN,EXTENDED,EXTENT_SIZE,FALSE,FAST,FETCH,FIELDS,FILE,' +
    'FIRST,FLUSH,FOR,FORCE,FOREIGN,FOUND,FROM,FULL,FULLTEXT,FUNCTION,' +
    'FUNCTIONS,GLOBAL,GOTO,GRANT,GRANTS,GROUP,HANDLER,HAVING,HELP,' +
    'HIGH_PRIORITY,HOST,HOSTS,IDENTIFIED,IF,IGNORE,IGNORE_SERVER_IDS,IMPORT,' +
    'IN,INDEX,INFILE,INITIAL_SIZE,INNER,INOUT,INSERT,INSERT_METHOD,INSTALL,INT1,INT2,' +
    'INT3,INT4,INT8,INTO,INVOKER,IO_THREAD,IS,ISOLATION,ISSUER,ITERATE,JOIN,' +
    'JSON,KEY,KEY_BLOCK_SIZE,KEYS,KILL,LANGUAGE,LAST,LEADING,LEAVE,LEAVES,' +
    'LESS,LEVEL,LIKE,LIMIT,LINEAR,LINES,LIST,LOAD,LOCAL,LOCK,LOGFILE,LOGS,' +
    'LONG,LOOP,LOW_PRIORITY,MASTER,MASTER_BIND,MASTER_CONNECT_RETRY,' +
    'MASTER_HEARTBEAT_PERIOD,MASTER_HOST,MASTER_LOG_FILE,MASTER_LOG_POS,' +
    'MASTER_PASSWORD,MASTER_PORT,MASTER_SSL,MASTER_SSL_CA,MASTER_SSL_CAPATH,' +
    'MASTER_SSL_CERT,MASTER_SSL_CIPHER,MASTER_SSL_KEY,' +
    'MASTER_SSL_VERIFY_SERVER_CERT,MASTER_USER,MATCH,' +
    'MAX_CONNECTIONS_PER_HOUR,MAX_QUERIES_PER_HOUR,MAX_ROWS,MAX_SIZE,' +
    'MAX_UPDATES_PER_HOUR,MAX_USER_CONNECTIONS,MAXVALUE,MEDIUM,MESSAGE_TEXT,' +
    'MIDDLEINT,MIN_ROWS,MOD,MODE,MODIFIES,MODIFY,MUTEX,MYSQL_ERRNO,NAME,' +
    'NAMES,NATURAL,NEW,NEXT,NO,NO_WRITE_TO_BINLOG,NODEGROUP,NONE,NOT,NULL,' +
    'OFFLINE,OFFSET,OJ,OLD,ON,ONLINE,OPEN,OPTIMIZE,OPTION,OPTIONALLY,' +
    'OPTIONS,OR,ORDER,OUT,OUTER,OUTFILE,OWNER,PACK_KEYS,PARSER,PARTIAL,' +
    'PARTITION,PARTITIONING,PARTITIONS,PLUGIN,PLUGINS,PORT,PREPARE,PRESERVE,' +
    'PREV,PRIMARY,PRIVILEGES,PROCEDURE,PROCESS,PROCESSLIST,PROFILE,PROFILES,' +
    'PROXY,PURGE,QUERY,QUICK,RAID_CHUNKS,RAID_CHUNKSIZE,RAID_TYPE,RANGE,' +
    'READ,READS,REBUILD,REDO_BUFFER_SIZE,REFERENCES,REGEXP,RELAY_LOG_FILE,' +
    'RELAY_LOG_POS,RELAYLOG,RELEASE,RELOAD,REMOVE,RENAME,REORGANIZE,REPAIR,' +
    'REPEATABLE,REPLACE,REPLICATION,REQUIRE,RESET,RESTORE,RESTRICT,RETURN,' +
    'RETURNS,REVOKE,RLIKE,ROLLBACK,ROLLUP,ROUTINE,ROW,ROW_FORMAT,ROWS,' +
    'SAVEPOINT,SCHEDULE,SCHEMA,SCHEMA_NAME,SCHEMAS,SECURITY,SELECT,' +
    'SEPARATOR,SERIALIZABLE,SERVER,SESSION,SET,SHARE,SHOW,SHUTDOWN,SIGNAL,' +
    'SIMPLE,SLAVE,SNAPSHOT,SOCKET,SONAME,SOUNDS,SQL,SQL_BIG_RESULT,' +
    'SQL_BUFFER_RESULT,SQL_CACHE,SQL_CALC_FOUND_ROWS,SQL_NO_CACHE,' +
    'SQL_SMALL_RESULT,SQL_THREAD,SQLEXCEPTION,SQLSTATE,SQLWARNING,SSL,START,' +
    'STARTING,STARTS,STATUS,STOP,STORAGE,STRAIGHT_JOIN,SUBCLASS_ORIGIN,' +
    'SUBJECT,SUBPARTITION,SUBPARTITIONS,SUPER,TABLE,TABLE_NAME,TABLES,' +
    'TABLESPACE,TEMPORARY,TEMPTABLE,TERMINATED,THAN,THEN,TO,TRAILING,' +
    'TRANSACTION,TRIGGER,TRIGGERS,TRUE,TYPE,UNCOMMITTED,UNDEFINED,UNDO,' +
    'UNDO_BUFFER_SIZE,UNDOFILE,UNINSTALL,UNION,UNIQUE,UNLOCK,UNTIL,UPDATE,' +
    'UPGRADE,USAGE,USE,USE_FRM,USING,VALUE,VALUES,VARIABLES,VARYING,VIEW,' +
    'WAIT,WARNINGS,WHEN,WHERE,WHILE,WITH,WORK,WRAPPER,WRITE,X509,XOR';

  NodeTypeToString: array[TNodeType] of PChar = (
    'ntUnknown',
    'ntRoot',
    'ntToken',
    'ntRange',

    'ntAlterDatabaseStmt',
    'ntAlterEventStmt',
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
    'ntCloseStmt',
    'ntCommitStmt',
    'ntCompoundStmt',
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
    'ntCreateViewStmt',
    'ntDataType',
    'ntDbIdent',
    'ntDeclareStmt',
    'ntDeleteStmt',
    'ntDoStmt',
    'ntDropDatabaseStmt',
    'ntDropEventStmt',
    'ntDropIndexStmt',
    'ntDropRoutineStmt',
    'ntDropServerStmt',
    'ntDropTableStmt',
    'ntDropTriggerStmt',
    'ntDropViewStmt',
    'ntFetchStmt',
    'ntFunctionCall',
    'ntFunctionReturns',
    'ntIfStmt',
    'ntIfStmtBranch',
    'ntIgnoreLines',
    'ntInsertStmt',
    'ntIterateStmt',
    'ntLeaveStmt',
    'ntLikeOp',
    'ntList',
    'ntLoadDataStmt',
    'ntLoadXMLStmt',
    'ntLockStmt',
    'ntLockStmtItem',
    'ntLoopStmt',
    'ntOpenStmt',
    'ntRenameStmt',
    'ntRenameStmtPair',
    'ntReleaseStmt',
    'ntRepeatStmt',
    'ntRoutineParam',
    'ntRollbackStmt',
    'ntSavepointStmt',
    'ntSchedule',
    'ntScheduleInterval',
    'ntScheduleIntervalListItem',
    'ntSelectStmt',
    'ntSelectStmtField',
    'ntSelectStmtGroup',
    'ntSelectStmtGroups',
    'ntSelectStmtJoin',
    'ntSelectStmtOrder',
    'ntSelectStmtTableFactor',
    'ntSelectStmtTableFactorIndexHint',
    'ntSelectStmtTableFactorOj',
    'ntSelectStmtTableFactorReferences',
    'ntSelectStmtTableFactorSelect',
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
    'ntSoundsLikeOp',
    'ntStartTransactionStmt',
    'ntSubArea',
    'ntSubPartition',
    'ntTag',
    'ntTransactionCharacteristic',
    'ntTruncateStmt',
    'ntUnaryOp',
    'ntUnknownStmt',
    'ntUnlockStmt',
    'ntUpdateStmt',
    'ntUser',
    'ntValue',
    'ntVariable',
    'ntWhileStmt',
    'ntXAStmt'
  );

  StmtTypeToString: array[TStmtType] of PChar = (
    'stUnknown',

    'stAlterEvent',
    'stAlterDatabase',
    'stAlterFunction',
    'stAlterProcedure',
    'stAlterServer',
    'stAlterTable',
    'stAlterView',
    'stBegin',
    'stCall',
    'stCase',
    'stClose',
    'stCommit',
    'stCreateDatabase',
    'stCreateEvent',
    'stCreateFunction',
    'stCreateIndex',
    'stCreateProcedure',
    'stCreateServer',
    'stCreateTable',
    'stCreateTrigger',
    'stCreateView',
    'stCompound',
    'stDeclare',
    'stDelete',
    'stDo',
    'stDropDatabase',
    'stDropEvent',
    'stDropFunction',
    'stDropIndex',
    'stDropProcedure',
    'stDropServer',
    'stDropTable',
    'stDropTrigge',
    'stDropView',
    'stFetch',
    'stIf',
    'stInsert',
    'stIterate',
    'stLeave',
    'stLoadData',
    'stLoadXML',
    'stLock',
    'stLoop',
    'stOpen',
    'stRepeat',
    'stRename',
    'stReleaseStmt',
    'stReplace',
    'stRollback',
    'stSavepoint',
    'stSelect',
    'stSet',
    'stSetNames',
    'stSetPassword',
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
    'stStartTransaction',
    'stTruncate',
    'stUnlock',
    'stUpdate',
    'stWhile',
    'stXA'
  );

  TokenTypeToString: array[TTokenType] of PChar = (
    'ttUnknown',
    'ttSyntaxError',
    'ttSpace',
    'ttReturn',
    'ttComment',
    'ttComma',
    'ttOpenBracket',
    'ttCloseBracket',
    'ttOpenCurlyBracket',
    'ttCloseCurlyBracket',
    'ttDelimiter',
    'ttInteger',
    'ttNumeric',
    'ttString',
    'ttCSString',
    'ttIdent',
    'ttDQIdent',
    'ttDBIdent',
    'ttBRIdent',
    'ttMySQLIdent',
    'ttBeginLabel',
    'ttEndLabel',
    'ttBindVariable',
    'ttMySQLCodeStart',
    'ttMySQLCodeEnd',
    'ttOperator',
    'ttAt',
    'ttBackslash'
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
    'utPLSQL'
  );

  OperatorTypeToString: array[TOperatorType] of PChar = (
    'otUnknown',

    'otDot',

    'otInterval',
    'otBinary',
    'otCollate',

    'otNot1',

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
    'otWHEN',

    'otNot2',

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
    'ditAlias',
    'ditTable',
    'ditKey',
    'ditColumn',
    'ditAllFields',
    'ditForeignKey',
    'ditFunction',
    'ditProcedure',
    'ditTrigger',
    'ditDatabase',
    'ditParameter',
    'ditEvent',
    'ditPartition',
    'ditServer',
    'ditXA',
    'ditCursor'
  );

  OperatorPrecedenceByOperatorType: array[TOperatorType] of Integer = (
    0,   // otUnknown

    1,   // otDot

    2,   // otInterval
    2,   // otBinary
    2,   // otCollate

    3,   // otNot1
         // otNot2

    4,   // otUnaryMinus
    4,   // otUnaryPlus
    4,   // otInvertBits

         // otPipes, if Parser.PipesAsConcat

    5,   // otBitXOR

    6,   // otMulti
    6,   // otDivision
    6,   // otDiv
    6,   // otMod

    7,   // otMinus
    7,   // otPlus

    8,   // otShiftLeft
    8,   // otShiftRight

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
    12,  // otWHEN

    13,  // otNot2

    14,  // otAnd

    15,  // otXOr

    16,  // otPipes
    16,  // otOr

    0,   // otEscape

    0,   // otAssignment
    0,   // otAssign
    0,   // otHat
    0,   // otDoubleDot
    0,   // otArrow
    0    // otParameter
  );
  MaxOperatorPrecedence = 16;

  UsageTypeByTokenType: array[TTokenType] of TUsageType = (
    utUnknown,
    utSyntaxError,
    utWhiteSpace,
    utWhiteSpace,
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
    utDbIdent,
    utDbIdent,
    utDbIdent,
    utDbIdent,
    utDbIdent,
    utLabel,
    utLabel,
    utDbIdent,
    utUnknown,
    utUnknown,
    utSymbol,
    utSymbol,
    utSymbol
  );

  NodeTypeByStmtType: array[TStmtType] of TNodeType = (
    ntUnknownStmt,

    ntAlterDatabaseStmt,
    ntAlterEventStmt,
    ntAlterRoutineStmt,
    ntAlterRoutineStmt,
    ntAlterServerStmt,
    ntAlterTableStmt,
    ntAlterViewStmt,
    ntBeginStmt,
    ntCallStmt,
    ntCaseStmt,
    ntCloseStmt,
    ntCommitStmt,
    ntCreateDatabaseStmt,
    ntCreateEventStmt,
    ntCreateRoutineStmt,
    ntCreateIndexStmt,
    ntCreateRoutineStmt,
    ntCreateServerStmt,
    ntCreateTableStmt,
    ntCreateTriggerStmt,
    ntCreateViewStmt,
    ntCompoundStmt,
    ntDeclareStmt,
    ntDeleteStmt,
    ntDoStmt,
    ntDropDatabaseStmt,
    ntDropEventStmt,
    ntDropRoutineStmt,
    ntDropIndexStmt,
    ntDropRoutineStmt,
    ntDropServerStmt,
    ntDropTableStmt,
    ntDropTriggerStmt,
    ntDropViewStmt,
    ntFetchStmt,
    ntIfStmt,
    ntInsertStmt,
    ntIterateStmt,
    ntLeaveStmt,
    ntLoadDataStmt,
    ntLoadXMLStmt,
    ntLockStmt,
    ntLoopStmt,
    ntOpenStmt,
    ntRenameStmt,
    ntReleaseStmt,
    ntRepeatStmt,
    ntRollbackStmt,
    ntInsertStmt,
    ntSavepointStmt,
    ntSelectStmt,
    ntSetStmt,
    ntSetNamesStmt,
    ntSetPasswordStmt,
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
    ntStartTransactionStmt,
    ntTruncateStmt,
    ntUnlockStmt,
    ntUpdateStmt,
    ntWhileStmt,
    ntXAStmt
  );

  StmtNodeTypes = [
    ntAlterDatabaseStmt,
    ntAlterEventStmt,
    ntAlterRoutineStmt,
    ntAlterServerStmt,
    ntAlterTableStmt,
    ntAlterViewStmt,
    ntBeginStmt,
    ntCallStmt,
    ntCaseStmt,
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
    ntCreateViewStmt,
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
    ntIfStmt,
    ntInsertStmt,
    ntIterateStmt,
    ntLeaveStmt,
    ntLoadDataStmt,
    ntLoadXMLStmt,
    ntLockStmt,
    ntLockStmtItem,
    ntLoopStmt,
    ntOpenStmt,
    ntRenameStmt,
    ntRenameStmtPair,
    ntReleaseStmt,
    ntRepeatStmt,
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
    ntStartTransactionStmt,
    ntTruncateStmt,
    ntUnknownStmt,
    ntUnlockStmt,
    ntUpdateStmt,
    ntWhileStmt,
    ntXAStmt
  ];

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
